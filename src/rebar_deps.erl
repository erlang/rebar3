%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2009 Dave Smith (dizzyd@dizzyd.com)
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%% -------------------------------------------------------------------
-module(rebar_deps).

-behaviour(rebar_provider).

-export([init/1,
         do/1]).

-include("rebar.hrl").

-export([setup_env/1]).

%% for internal use only
-export([info/2]).
-export([get_deps_dir/1]).

-define(PROVIDER, deps).
-define(DEPS, []).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_config:config()) -> {ok, rebar_config:config()}.
init(State) ->
    State1 = rebar_config:add_provider(State, #provider{name = ?PROVIDER,
                                                        provider_impl = ?MODULE,
                                                        provides = deps,
                                                        bare = false,
                                                        deps = ?DEPS,
                                                        example = "rebar deps",
                                                        short_desc = "",
                                                        desc = "",
                                                        opts = []}),
    {ok, State1}.

-spec do(rebar_config:config()) -> {ok, rebar_config:config()} | relx:error().
do(Config) ->
    Deps = rebar_config:get_local(Config, deps, []),
    Goals = lists:map(fun({Name, "", _}) ->
                              Name;
                         ({Name, ".*", _}) ->
                              Name;
                         ({Name, Vsn, _}) ->
                              {Name, Vsn}
                      end, Deps),

    {Config1, Deps1} = update_deps(Config, Deps),
    Config2 = rebar_config:deps(Config1, Deps1),

    {ok, rebar_config:goals(Config2, Goals)}.

update_deps(Config, Deps) ->
    DepsDir = get_deps_dir(Config),

    %% Find available apps to fulfill dependencies
    FoundApps = rebar_app_discover:find_apps([DepsDir]),

    %% Resolve deps and their dependencies
    Deps1 = handle_deps(Deps, FoundApps),

    case download_missing_deps(Config, DepsDir, FoundApps, Deps1) of
        {Config1, []} ->
            {Config1, Deps1};
        {Config1, _} ->
            update_deps(Config1, Deps1)
    end.

handle_deps(Deps, Found) ->
    NewDeps =
        lists:foldl(fun(X, DepsAcc) ->
                            C = rebar_config:new2(rebar_config:new(), rebar_app_info:dir(X)),
                            LocalDeps = rebar_config:get_local(C, deps, []),
                            [LocalDeps | DepsAcc]
                    end, [], Found),
    NewDeps1 = lists:flatten(NewDeps),

    %% Weed out duplicates
    lists:umerge(fun(A, B) ->
                         element(1, A) =:= element(1, B)
                 end, lists:usort(Deps), lists:usort(NewDeps1)).

download_missing_deps(Config, DepsDir, Found, Deps) ->
    Apps = rebar_config:apps_to_build(Config),
    Missing = lists:filter(fun(X) ->
                               not lists:any(fun(F) ->
                                                     element(1, X) =:= element(2, F)
                                             end, Found++Apps)
                       end, Deps),
    ec_plists:foreach(fun(X) ->
                              TargetDir = get_deps_dir(DepsDir, element(1, X)),
                              case filelib:is_dir(TargetDir) of
                                  true ->
                                      ok;
                                  false ->
                                      rebar_fetch:download_source(TargetDir, element(3, X))
                              end
                      end, Missing),

    Config1 = lists:foldl(fun(X, ConfigAcc) ->
                                  TargetDir = get_deps_dir(DepsDir, element(1, X)),
                                  [AppSrc] = rebar_app_discover:find_apps([TargetDir]),
                                  rebar_config:add_dep(ConfigAcc, AppSrc)
                          end, Config, Missing),

    {Config1, Missing}.

%% set REBAR_DEPS_DIR and ERL_LIBS environment variables
setup_env(Config) ->
    {true, DepsDir} = get_deps_dir(Config),
    %% include rebar's DepsDir in ERL_LIBS
    Separator = case os:type() of
                    {win32, nt} ->
                        ";";
                    _ ->
                        ":"
                end,
    ERL_LIBS = case os:getenv("ERL_LIBS") of
                   false ->
                       {"ERL_LIBS", DepsDir};
                   PrevValue ->
                       {"ERL_LIBS", DepsDir ++ Separator ++ PrevValue}
               end,
    [{"REBAR_DEPS_DIR", DepsDir}, ERL_LIBS].


get_deps_dir(Config) ->
    BaseDir = rebar_utils:base_dir(Config),
    get_deps_dir(BaseDir, "deps").

%% ===================================================================
%% Internal functions
%% ===================================================================

get_deps_dir(DepsDir, App) ->
    filename:join(DepsDir, App).

-spec gather_application_info(file:name(), file:filename()) ->
                                     {ok, rebar_app_info:t()} |
                                     {warning, Reason::term()} |
                                     {error, Reason::term()}.
gather_application_info(EbinDir, File) ->
    AppDir = filename:dirname(EbinDir),
    case file:consult(File) of
        {ok, [{application, AppName, AppDetail}]} ->
            validate_application_info(EbinDir, File, AppName, AppDetail);
        {error, Reason} ->
            {warning, {unable_to_load_app, AppDir, Reason}};
        _ ->
            {warning, {invalid_app_file, File}}
    end.

-spec validate_application_info(file:name(),
                                file:name(),
                                atom(),
                                proplists:proplist()) ->
                                       {ok, list()} |
                                       {warning, Reason::term()} |
                                       {error, Reason::term()}.
validate_application_info(EbinDir, AppFile, AppName, AppDetail) ->
    AppDir = filename:dirname(EbinDir),
    case get_modules_list(AppFile, AppDetail) of
        {ok, List} ->
            has_all_beams(EbinDir, List);
        Error ->
            Error
    end.

-spec get_modules_list(file:name(), proplists:proplist()) ->
                              {ok, list()} |
                              {warning, Reason::term()} |
                              {error, Reason::term()}.
get_modules_list(AppFile, AppDetail) ->
    case proplists:get_value(modules, AppDetail) of
        undefined ->
            {warning, {invalid_app_file, AppFile}};
        ModulesList ->
            {ok, ModulesList}
    end.

-spec has_all_beams(file:name(), list()) ->
                           ok | {error, Reason::term()}.
has_all_beams(EbinDir, [Module | ModuleList]) ->
    BeamFile = filename:join([EbinDir,
                              list_to_binary(atom_to_list(Module) ++ ".beam")]),
    case filelib:is_file(BeamFile) of
        true ->
            has_all_beams(EbinDir, ModuleList);
        false ->
            {warning, {missing_beam_file, Module, BeamFile}}
    end;
has_all_beams(_, []) ->
    ok.

info(help, 'deps') ->
    info_help("Display dependencies").

info_help(Description) ->
    ?CONSOLE(
       "~s.~n"
       "~n"
       "Valid rebar.config options:~n"
       "  ~p~n"
       "  ~p~n"
       "Valid command line options:~n"
       "  deps_dir=\"deps\" (override default or rebar.config deps_dir)~n",
       [
        Description,
        {deps_dir, "deps"},
        {deps,
         [app_name,
          {rebar, "1.0.*"},
          {rebar, ".*",
           {git, "git://github.com/rebar/rebar.git"}},
          {rebar, ".*",
           {git, "git://github.com/rebar/rebar.git", "Rev"}},
          {rebar, "1.0.*",
           {git, "git://github.com/rebar/rebar.git", {branch, "master"}}},
          {rebar, "1.0.0",
           {git, "git://github.com/rebar/rebar.git", {tag, "1.0.0"}}},
          {rebar, "",
           {git, "git://github.com/rebar/rebar.git", {branch, "master"}},
           [raw]},
          {app_name, ".*", {hg, "https://www.example.org/url"}},
          {app_name, ".*", {rsync, "Url"}},
          {app_name, ".*", {svn, "https://www.example.org/url"}},
          {app_name, ".*", {svn, "svn://svn.example.org/url"}},
          {app_name, ".*", {bzr, "https://www.example.org/url", "Rev"}},
          {app_name, ".*", {fossil, "https://www.example.org/url"}},
          {app_name, ".*", {fossil, "https://www.example.org/url", "Vsn"}},
          {app_name, ".*", {p4, "//depot/subdir/app_dir"}}]}
       ]).
