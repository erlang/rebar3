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
-export([get_deps_dir/2]).

-define(PROVIDER, deps).
-define(DEPS, [app_discovery]).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    State1 = rebar_state:add_provider(State, #provider{name = ?PROVIDER,
                                                       provider_impl = ?MODULE,
                                                       bare = false,
                                                       deps = ?DEPS,
                                                       example = "rebar deps",
                                                       short_desc = "",
                                                       desc = "",
                                                       opts = []}),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()}.
do(Config) ->
    Deps = rebar_state:get(Config, deps, []),
    Goals = lists:map(fun({Name, "", _}) ->
                              Name;
                         ({Name, ".*", _}) ->
                              Name;
                         ({Name, Vsn, _}) ->
                              {Name, Vsn}
                      end, Deps),

    {Config1, Deps1} = update_deps(Config, Deps),
    Config2 = rebar_state:set(Config1, deps, Deps1),

    {ok, rebar_state:set(Config2, goals, Goals)}.

update_deps(Config, Deps) ->
    DepsDir = get_deps_dir(Config),

    %% Find available apps to fulfill dependencies
    UnbuiltApps = rebar_app_discover:find_unbuilt_apps([DepsDir]),
    FoundApps = rebar_app_discover:find_apps([DepsDir]),

    %% Resolve deps and their dependencies
    Deps1 = handle_deps(Deps, UnbuiltApps++FoundApps),

    case download_missing_deps(Config, DepsDir, FoundApps, UnbuiltApps, Deps1) of
        {Config1, []} ->
            {Config1, Deps1};
        {Config1, _} ->
            update_deps(Config1, Deps1)
    end.

handle_deps(Deps, Found) ->
    NewDeps =
        lists:foldl(fun(X, DepsAcc) ->
                            C = rebar_config:consult(rebar_app_info:dir(X)),
                            S = rebar_state:new(rebar_state:new(), C, rebar_app_info:dir(X)),
                            LocalDeps = rebar_state:get(S, deps, []),
                            [LocalDeps | DepsAcc]
                    end, [], Found),
    NewDeps1 = lists:flatten(NewDeps),

    %% Weed out duplicates
    lists:umerge(fun(A, B) ->
                         element(1, A) =:= element(1, B)
                 end, lists:usort(Deps), lists:usort(NewDeps1)).

download_missing_deps(Config, DepsDir, Found, Unbuilt, Deps) ->
    Missing = lists:filter(fun(X) ->
                               not lists:any(fun(F) ->
                                                     element(1, X) =:= element(2, F)
                                             end, Found++Unbuilt)
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
                                  [AppSrc] = rebar_app_discover:find_unbuilt_apps([TargetDir]),
                                  rebar_state:add_app(ConfigAcc, AppSrc)
                          end, Config, Missing),

    {Config1, Missing}.

%% set REBAR_DEPS_DIR and ERL_LIBS environment variables
setup_env(Config) ->
    DepsDir = get_deps_dir(Config),
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


get_deps_dir(State) ->
    BaseDir = rebar_state:get(State, base_dir, ""),
    get_deps_dir(BaseDir, "deps").

get_deps_dir(DepsDir, App) ->
    filename:join(DepsDir, App).

%% ===================================================================
%% Internal functions
%% ===================================================================

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
