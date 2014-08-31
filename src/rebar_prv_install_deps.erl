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
-module(rebar_prv_install_deps).

-behaviour(rebar_provider).

-export([init/1,
         do/1]).

-include("rebar.hrl").

-export([setup_env/1]).

%% for internal use only
-export([get_deps_dir/1]).
-export([get_deps_dir/2]).

-define(PROVIDER, install_deps).
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
                                                       short_desc = "Install dependencies",
                                                       desc = info("Install dependencies"),
                                                       opts = []}),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()}.
do(State) ->
    %% Read in package index and dep graph
    {Packages, Graph} = rebar_packages:get_packages(State),
    case rebar_state:get(State, locks, []) of
        [] ->
            case rebar_state:get(State, deps, []) of
                [] ->
                    {ok, State};
                Deps ->
                    %% Split source deps form binary deps, needed to keep backwards compatibility
                    DepsDir = get_deps_dir(State),
                    {SrcDeps, Goals} = parse_deps(DepsDir, Deps),
                    State1 = rebar_state:src_deps(rebar_state:goals(State, Goals), lists:ukeysort(2, SrcDeps)),
                    State2 = update_src_deps(State1),
                    case rebar_state:goals(State2) of
                        [] ->
                            ProjectApps = lists:map(fun(X) ->
                                                            rebar_app_info:deps(X, [rebar_app_info:name(Y) || Y <- SrcDeps])
                                                    end, rebar_state:apps_to_build(State2)),
                            FinalDeps = ProjectApps ++ rebar_state:src_deps(State2),
                            {ok, Sort} = rebar_topo:sort_apps(FinalDeps),
                            State3 = rebar_state:apps_to_build(State2, Sort),
                            {ok, State3};
                        Goals1 ->
                            {ok, Solved} = rlx_depsolver:solve(Graph, Goals1),
                            Final = lists:map(fun({Name, Vsn}) ->
                                                      FmtVsn = ec_cnv:to_binary(rlx_depsolver:format_version(Vsn)),
                                                      {ok, P} = dict:find({Name, FmtVsn}, Packages),
                                                      PkgDeps = proplists:get_value(<<"deps">>, P),
                                                      Link = proplists:get_value(<<"link">>, P),
                                                      Source = {Name, FmtVsn, Link},
                                                      {ok, AppInfo} = rebar_app_info:new(Name, FmtVsn),
                                                      AppInfo1 = rebar_app_info:deps(AppInfo, PkgDeps),
                                                      AppInfo2 =
                                                          rebar_app_info:dir(AppInfo1, get_deps_dir(DepsDir, <<Name/binary, "-", FmtVsn/binary>>)),
                                                      AppInfo3 = rebar_app_info:source(AppInfo2, Source),
                                                      ok = maybe_fetch(AppInfo3),
                                                      AppInfo3
                                              end, Solved),
                            ProjectApps = lists:map(fun(X) ->
                                                            rebar_app_info:deps(X, [rebar_app_info:name(Y) || Y <- SrcDeps] ++ [element(1, G) || G <- Goals1])
                                                    end, rebar_state:apps_to_build(State2)),
                            FinalDeps = ProjectApps ++ rebar_state:src_deps(State2) ++ Final,
                            {ok, Sort} = rebar_topo:sort_apps(FinalDeps),
                            State3 = rebar_state:apps_to_build(State2, Sort),
                            {ok, State3}
                    end
            end;
        _Locks ->
            {ok, State}
    end.

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

update_src_deps(State) ->
    SrcDeps = rebar_state:src_deps(State),
    DepsDir = get_deps_dir(State),
    case lists:foldl(fun(AppInfo, {SrcDepsAcc, GoalsAcc}) ->
                             ok = maybe_fetch(AppInfo),
                             {NewSrcDeps, NewGoals} = handle_dep(DepsDir, AppInfo),
                             {lists:ukeymerge(2, SrcDepsAcc, lists:ukeysort(2, NewSrcDeps)), NewGoals++GoalsAcc}
                     end, {SrcDeps, rebar_state:goals(State)}, SrcDeps) of
        {SrcDeps, NewGoals} ->
            rebar_state:goals(State, NewGoals);
        {NewSrcDeps, NewGoals} ->
            io:format("NEWSRC ~p~n", [NewSrcDeps]),
            State1 = rebar_state:src_deps(rebar_state:goals(State, NewGoals), NewSrcDeps),
            update_src_deps(State1)
    end.

handle_dep(DepsDir, AppInfo) ->
    C = rebar_config:consult(rebar_app_info:dir(AppInfo)),
    S = rebar_state:new(rebar_state:new(), C, rebar_app_info:dir(AppInfo)),
    Deps = rebar_state:get(S, deps, []),
    parse_deps(DepsDir, Deps).

maybe_fetch(AppInfo) ->
    AppDir = rebar_app_info:dir(AppInfo),
    case filelib:is_dir(AppDir) of
        false ->
            ?INFO("Fetching ~s~n", [rebar_app_info:name(AppInfo)]),
            Source = rebar_app_info:source(AppInfo),
            rebar_fetch:download_source(AppDir, Source);
        true ->
            ok
    end.

parse_deps(DepsDir, Deps) ->
    lists:foldl(fun({Name, Vsn}, {SrcDepsAcc, GoalsAcc}) ->
                        {SrcDepsAcc, [parse_goal(ec_cnv:to_binary(Name)
                                                ,ec_cnv:to_binary(Vsn)) | GoalsAcc]};
                   (Name, {SrcDepsAcc, GoalsAcc}) when is_atom(Name) ->
                        {SrcDepsAcc, [ec_cnv:to_binary(Name) | GoalsAcc]};
                   ({Name, _, Source}, {SrcDepsAcc, GoalsAcc}) ->
                        {ok, Dep} = rebar_app_info:new(Name),
                        Dep1 = rebar_app_info:source(
                                 rebar_app_info:dir(Dep, get_deps_dir(DepsDir, Name)), Source),
                        {[Dep1 | SrcDepsAcc], GoalsAcc}
                end, {[], []}, Deps).

parse_goal(Name, Constraint) ->
    case re:run(Constraint, "([^\\d]*)(\\d.*)", [{capture, [1,2], binary}]) of
        {match, [<<>>, Vsn]} ->
            {Name, Vsn};
        {match, [Op, Vsn]} ->
            {Name, Vsn, binary_to_atom(Op, utf8)};
        nomatch ->
            fail
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
