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
-module(rebar_prv_deps).

-behaviour(rebar_provider).

-export([init/1,
         do/1]).

-include("rebar.hrl").

-export([setup_env/1]).

%% for internal use only
-export([get_deps_dir/1]).
-export([get_deps_dir/2]).

-define(PROVIDER, deps).
-define(DEPS, [app_discovery]).

-record(dep, {name :: binary(),
              vsn :: binary(),
              source :: binary()}).

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
                                                       short_desc = "Install dependencies",
                                                       desc = info("Install dependencies"),
                                                       opts = []}),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()}.
do(State) ->
    %% Read in package index and dep graph
    {Packages, Graph} = get_packages(State),

    case rebar_state:get(State, deps, []) of
        [] ->
            {ok, State};
        Deps ->
            %% Split source deps form binary deps, needed to keep backwards compatibility
            case parse_deps(Deps) of
                {SrcDeps, []} ->
                    {State1, SrcDeps1} = update_src_deps(State, SrcDeps),
                    {ok, rebar_state:set(State1, deps, SrcDeps1)};
                {SrcDeps, Goals} ->
                    {State1, _SrcDeps1} = update_src_deps(State, SrcDeps),
                    {ok, Solved} = rlx_depsolver:solve(Graph, Goals),
                    M = lists:map(fun({Name, Vsn}) ->
                                          FmtVsn = to_binary(rlx_depsolver:format_version(Vsn)),
                                          {ok, P} = dict:find({Name, FmtVsn}, Packages),
                                          Link = proplists:get_value(<<"link">>, P),
                                          {Name, Vsn, {Name
                                                      ,FmtVsn
                                                      ,Link}}
                                  end, Solved),

                    {State2, Deps1} = update_deps(State1, M),
                    State3 = rebar_state:set(State2, deps, Deps1),
                    {ok, rebar_state:set(State3, goals, Goals)}
            end
    end.

update_deps(State, Deps) ->
    DepsDir = get_deps_dir(State),

    %% Find available apps to fulfill dependencies
    %% Should only have to do this once, not every iteration
    UnbuiltApps = rebar_app_discover:find_unbuilt_apps([DepsDir]),
    FoundApps = rebar_app_discover:find_apps([DepsDir]),

    download_missing_deps(State, DepsDir, FoundApps, UnbuiltApps, Deps).

update_src_deps(State, Deps) ->
    DepsDir = get_deps_dir(State),

    %% Find available apps to fulfill dependencies
    %% Should only have to do this once, not every iteration
    UnbuiltApps = rebar_app_discover:find_unbuilt_apps([DepsDir]),
    FoundApps = rebar_app_discover:find_apps([DepsDir]),

    %% Resolve deps and their dependencies
    Deps1 = handle_src_deps(Deps, UnbuiltApps++FoundApps),
    {State1, Missing} = download_missing_deps(State, DepsDir, FoundApps, UnbuiltApps, Deps1),
    case dict:is_empty(Missing) of
        true ->
            {State1, Deps1};
        false ->
            update_src_deps(State1, Missing)
    end.

handle_src_deps(Deps, Found) ->
    lists:foldl(fun(X, DepsAcc) ->
                        C = rebar_config:consult(rebar_app_info:dir(X)),
                        S = rebar_state:new(rebar_state:new(), C, rebar_app_info:dir(X)),
                        {ParsedDeps, _Goals} = parse_deps(rebar_state:get(S, deps, [])),
                        dict:merge(fun(_K, V1, _V2) -> V1 end, DepsAcc, ParsedDeps)
                end, Deps, Found).

download_missing_deps(State, DepsDir, Found, Unbuilt, Deps) ->
    Missing =
        dict:filter(fun(Key, _) ->
                            not lists:any(fun(F) ->
                                                  Key =:= to_binary(rebar_app_info:name(F))
                                          end, Found++Unbuilt)
                    end, Deps),
    dict:map(fun(_Key, #dep{name=Name, source=Source}) ->
                     TargetDir = get_deps_dir(DepsDir, Name),
                     case filelib:is_dir(TargetDir) of
                         true ->
                             ok;
                         false ->
                             ?INFO("Fetching ~s ~s~n", [Name
                                                       ,element(2, Source)]),
                             rebar_fetch:download_source(TargetDir, Source),
                             case rebar_app_discover:find_unbuilt_apps([TargetDir]) of
                                 [AppSrc] ->
                                     C = rebar_config:consult(rebar_app_info:dir(AppSrc)),
                                     S = rebar_state:new(rebar_state:new()
                                                        ,C
                                                        ,rebar_app_info:dir(AppSrc)),
                                     rebar_prv_app_builder:build(S, AppSrc);
                                 [] ->
                                     []
                             end
                     end
             end, Missing),

    {State, Missing}.

%% set REBAR_DEPS_DIR and ERL_LIBS environment variables
setup_env(State) ->
    DepsDir = get_deps_dir(State),
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

new({Name, Vsn, Source})->
    #dep{name=to_binary(Name), vsn=to_binary(Vsn), source=Source};
new(Name) ->
    #dep{name=to_binary(Name)}.

-spec name(record(dep)) -> binary().
name(#dep{name=Name}) ->
    Name.

-spec vsn(record(dep)) -> binary().
vsn(#dep{vsn=Vsn}) ->
    Vsn.

-spec source(record(dep)) -> tuple().
source(#dep{source=Source}) ->
    Source.

to_binary(X) when is_binary(X) ->
    X;
to_binary(X) when is_atom(X) ->
    atom_to_binary(X, utf8);
to_binary(X) when is_list(X) ->
    iolist_to_binary(X).

parse_deps(Deps) ->
    lists:foldl(fun({Name, Vsn}, {SrcDepsAcc, GoalsAcc}) ->
                        {SrcDepsAcc, [{to_binary(Name), to_binary(Vsn)} | GoalsAcc]};
                   (Name, {SrcDepsAcc, GoalsAcc}) when is_atom(Name) ->
                        {SrcDepsAcc, [to_binary(Name) | GoalsAcc]};
                   (SrcDep, {SrcDepsAcc, GoalsAcc}) ->
                        Dep = new(SrcDep),
                        {dict:store(name(Dep), Dep, SrcDepsAcc), GoalsAcc}
                end, {dict:new(), []}, Deps).

get_packages(State) ->
    RebarDir = rebar_state:get(State, global_rebar_dir, filename:join(os:getenv("HOME"), ".rebar")),
    PackagesFile = filename:join(RebarDir, "packages"),
    case ec_file:exists(PackagesFile) of
        true ->
            try
                {ok, Binary} = file:read_file(PackagesFile),
                binary_to_term(Binary)
            catch
                _:_ ->
                    ?ERROR("Bad packages index, try to fix with `rebar update`~n", []),
                    {[], rlx_depsolver:new()}
            end;
        false ->
            {[], rlx_depsolver:new()}
    end.

info(Description) ->
    io_lib:format("~s.~n"
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
