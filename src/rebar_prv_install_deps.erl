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

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-include("rebar.hrl").
-include_lib("providers/include/providers.hrl").

-export([handle_deps/3,
         handle_deps/4,
         handle_deps/5]).

-export_type([dep/0]).

-define(PROVIDER, install_deps).
-define(DEPS, [app_discovery]).

-type src_dep() :: {atom(), {atom(), string(), string()}}
             | {atom(), string(), {atom(), string(), string()}}.
-type pkg_dep() :: {atom(), binary()} | atom().

-type dep() :: src_dep() | pkg_dep().

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    State1 = rebar_state:add_provider(State, providers:create([{name, ?PROVIDER},
                                                               {module, ?MODULE},
                                                               {bare, true},
                                                               {deps, ?DEPS},
                                                               {example, undefined},
                                                               {short_desc, "Install dependencies"},
                                                               {desc, info("Install dependencies")},
                                                               {opts, []}])),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    try
        ?INFO("Verifying dependencies...", []),
        Profiles = rebar_state:current_profiles(State),
        ProjectApps = rebar_state:project_apps(State),

        {Apps, State1} =
            lists:foldl(fun deps_per_profile/2, {[], State}, lists:reverse(Profiles)),

        Source = ProjectApps ++ Apps,
        case find_cycles(Source) of
            {cycles, Cycles} ->
                ?PRV_ERROR({cycles, Cycles});
            {error, Error} ->
                {error, Error};
            no_cycle ->
                case compile_order(Source, ProjectApps) of
                    {ok, ToCompile} ->
                        {ok, rebar_state:deps_to_build(State1, ToCompile)};
                    {error, Error} ->
                        {error, Error}
                end
        end
    catch
        %% maybe_fetch will maybe_throw an exception to break out of some loops
        _:{error, Reason} ->
            {error, Reason}
    end.

-spec format_error(any()) -> iolist().
format_error({parse_dep, Dep}) ->
    io_lib:format("Failed parsing dep ~p", [Dep]);
format_error({missing_package, Package, Version}) ->
    io_lib:format("Package not found in registry: ~s-~s", [Package, Version]);
format_error({cycles, Cycles}) ->
    Prints = [["applications: ",
               [io_lib:format("~s ", [Dep]) || Dep <- Cycle],
               "depend on each other~n"]
              || Cycle <- Cycles],
    ["Dependency cycle(s) detected:~n", Prints];
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

-spec handle_deps(atom(), rebar_state:t(), list()) ->
                         {ok, [rebar_app_info:t()], rebar_state:t()} | {error, string()}.
handle_deps(Profile, State, Deps) ->
    handle_deps(Profile, State, Deps, false, []).

-spec handle_deps(atom(), rebar_state:t(), list(), list() | boolean()) ->
                         {ok, [rebar_app_info:t()], rebar_state:t()} | {error, string()}.
handle_deps(Profile, State, Deps, Upgrade) when is_boolean(Upgrade) ->
    handle_deps(Profile, State, Deps, Upgrade, []);
handle_deps(Profile, State, Deps, Locks) when is_list(Locks) ->
    Upgrade = rebar_state:get(State, upgrade, false),
    handle_deps(Profile, State, Deps, Upgrade, Locks).

-spec handle_deps(atom(), rebar_state:t(), list(), boolean() | {true, binary(), integer()}, list())
                 -> {ok, [rebar_app_info:t()], rebar_state:t()} | {error, string()}.
handle_deps(_Profile, State, [], _, _) ->
    {ok, [], State};
handle_deps(Profile, State, Deps, Upgrade, Locks) ->
    %% Read in package index and dep graph
    {Packages, Graph} = rebar_packages:get_packages(State),
    %% Split source deps from pkg deps, needed to keep backwards compatibility
    DepsDir = rebar_dir:deps_dir(State),
    {SrcDeps, PkgDeps} = parse_deps(DepsDir, Deps, State, Locks, 0),

    %% Fetch transitive src deps
    {State1, SrcApps, PkgDeps1, Seen} =
        update_src_deps(Profile, 0, SrcDeps, PkgDeps, [], State, Upgrade, sets:new(), Locks),

    {Solved, State2} =
        update_pkg_deps(Profile, Packages, PkgDeps1, Graph, Upgrade, Seen, State1),

    AllDeps = lists:ukeymerge(2
                             ,lists:ukeysort(2, SrcApps)
                             ,lists:ukeysort(2, Solved)),
    %% Sort all apps to build order
    State3 = rebar_state:all_deps(State2, AllDeps),

    {ok, AllDeps, State3}.

%% ===================================================================
%% Internal functions
%% ===================================================================

deps_per_profile(Profile, {Apps, State}) ->
    Locks = rebar_state:get(State, {locks, Profile}, []),
    ProfileDeps = rebar_state:get(State, {deps, Profile}, []),
    {ok, NewApps, NewState} = handle_deps(Profile, State, ProfileDeps, Locks),
    {NewApps++Apps, NewState}.

find_cycles(Apps) ->
    case rebar_digraph:compile_order(Apps) of
        {error, {cycles, Cycles}} -> {cycles, Cycles};
        {error, Error} -> {error, Error};
        {ok, _} -> no_cycle
    end.

compile_order(Source, ProjectApps) ->
    case rebar_digraph:compile_order(Source) of
        {ok, Sort} ->
            %% Valid apps are compiled and good
            {ok, lists:dropwhile(fun rebar_app_info:valid/1, Sort -- ProjectApps)};
        {error, Error} ->
            {error, Error}
    end.

update_pkg_deps(Profile, Packages, PkgDeps, Graph, Upgrade, Seen, State) ->
    case PkgDeps of
        [] -> %% No pkg deps
            {[], State};
        PkgDeps ->
            %% Find pkg deps needed
            S = case rebar_digraph:cull_deps(Graph, PkgDeps) of
                {ok, [], _} ->
                    throw({rebar_digraph, no_solution});
                {ok, Solution, []} ->
                    Solution;
                {ok, Solution, Discarded} ->
                    [warn_skip_pkg(Pkg) || Pkg <- Discarded],
                    Solution
            end,
            update_pkg_deps(Profile, S, Packages, Upgrade, Seen, State)
    end.

update_pkg_deps(Profile, Pkgs, Packages, Upgrade, Seen, State) ->
    %% Create app_info record for each pkg dep
    DepsDir = rebar_dir:deps_dir(State),
    {Solved, _, State1}
        = lists:foldl(fun(Pkg, {Acc, SeenAcc, StateAcc}) ->
                        handle_pkg_dep(Profile, Pkg, Packages, Upgrade, DepsDir, Acc, SeenAcc, StateAcc)
                      end, {[], Seen, State}, Pkgs),
    {Solved, State1}.

handle_pkg_dep(Profile, Pkg, Packages, Upgrade, DepsDir, Fetched, Seen, State) ->
    AppInfo = package_to_app(DepsDir, Packages, Pkg),
    {NewSeen, NewState} = maybe_lock(Profile, AppInfo, Seen, State, 0),
    case maybe_fetch(AppInfo, Upgrade, NewSeen, NewState) of
        true ->
            {[AppInfo | Fetched], NewSeen, NewState};
        false ->
            {Fetched, NewSeen, NewState}
    end.


maybe_lock(Profile, AppInfo, Seen, State, Level) ->
    case Profile of
        default ->
            Name = rebar_app_info:name(AppInfo),
            case sets:is_element(Name, Seen) of
                false ->
                    Locks = rebar_state:lock(State),
                    case lists:any(fun(App) -> rebar_app_info:name(App) =:= Name end, Locks) of
                        true ->
                            {sets:add_element(Name, Seen), State};
                        false ->
                            {sets:add_element(Name, Seen),
                             rebar_state:lock(State, rebar_app_info:dep_level(AppInfo, Level))}
                    end;
                true ->
                    {Seen, State}
            end;
        _ ->
            {Seen, State}
    end.

package_to_app(DepsDir, Packages, {Name, Vsn}) ->
    case dict:find({Name, Vsn}, Packages) of
        error ->
            throw(?PRV_ERROR({missing_package, Name, Vsn}));
        {ok, P} ->
            PkgDeps = [{PkgName, PkgVsn}
                       || {PkgName,PkgVsn} <- proplists:get_value(<<"deps">>, P, [])],
            {ok, AppInfo} = rebar_app_info:new(Name, Vsn),
            AppInfo1 = rebar_app_info:deps(AppInfo, PkgDeps),
            AppInfo2 = rebar_app_info:dir(AppInfo1, rebar_dir:deps_dir(DepsDir, Name)),
            rebar_app_info:source(AppInfo2, {pkg, Name, Vsn})
    end.

-spec update_src_deps(atom(), non_neg_integer(), list(), list(), list(), rebar_state:t(), boolean(), sets:set(binary()), list()) -> {rebar_state:t(), list(), list(), sets:set(binary())}.
update_src_deps(Profile, Level, SrcDeps, PkgDeps, SrcApps, State, Upgrade, Seen, Locks) ->
    case lists:foldl(
            fun(AppInfo, {SrcDepsAcc, PkgDepsAcc, SrcAppsAcc, StateAcc, SeenAcc, LocksAcc}) ->
                    update_src_dep(AppInfo, Profile, Level,
                                   SrcDepsAcc, PkgDepsAcc, SrcAppsAcc, StateAcc,
                                   Upgrade, SeenAcc, Locks, LocksAcc)
            end,
            {[], PkgDeps, SrcApps, State, Seen, Locks},
            rebar_utils:sort_deps(SrcDeps)) of
        {[], NewPkgDeps, NewSrcApps, State1, Seen1, _NewLocks} ->
            {State1, NewSrcApps, NewPkgDeps, Seen1};
        {NewSrcDeps, NewPkgDeps, NewSrcApps, State1, Seen1, NewLocks} ->
            update_src_deps(Profile, Level+1, NewSrcDeps, NewPkgDeps, NewSrcApps, State1, Upgrade, Seen1, NewLocks)
    end.

update_src_dep(AppInfo, Profile, Level, SrcDeps, PkgDeps, SrcApps, State, Upgrade, Seen, BaseLocks, Locks) ->
    %% If not seen, add to list of locks to write out
    Name = rebar_app_info:name(AppInfo),
    case sets:is_element(Name, Seen) of
        true ->
            update_seen_src_dep(AppInfo, Level,
                                SrcDeps, PkgDeps, SrcApps,
                                State, Upgrade, Seen, BaseLocks, Locks);
        false ->
            update_unseen_src_dep(AppInfo, Profile, Level,
                                  SrcDeps, PkgDeps, SrcApps,
                                  State, Upgrade, Seen, Locks)

    end.

update_seen_src_dep(AppInfo, Level, SrcDeps, PkgDeps, SrcApps, State, Upgrade, Seen, BaseLocks, Locks) ->
    Name = rebar_app_info:name(AppInfo),
    %% If seen from lock file don't print warning about skipping
    case lists:keymember(Name, 1, BaseLocks) of
        false ->
            warn_skip_deps(AppInfo);
        true ->
            ok
    end,
    %% scan for app children here if upgrading
    case Upgrade of
        false ->
            {SrcDeps, PkgDeps, SrcApps, State, Seen, Locks};
        true ->
            {NewSrcDeps, NewPkgDeps, NewSrcApps, NewState, NewLocks}
            = handle_dep(AppInfo, SrcDeps, PkgDeps, SrcApps,
                         Level, State, Locks),
            {NewSrcDeps, NewPkgDeps, NewSrcApps, NewState, Seen, NewLocks}
    end.

update_unseen_src_dep(AppInfo, Profile, Level, SrcDeps, PkgDeps, SrcApps, State, Upgrade, Seen, Locks) ->
    {NewSeen, State1} = maybe_lock(Profile, AppInfo, Seen, State, Level),
    {NewSrcDeps, NewPkgDeps, NewSrcApps, State2, NewLocks}
    = case Upgrade of
        true ->
            handle_upgrade(AppInfo, SrcDeps, PkgDeps, SrcApps,
                           Level, State1, Locks);
        _ ->
            maybe_fetch(AppInfo, false, Seen, State1),
            handle_dep(AppInfo, SrcDeps, PkgDeps, SrcApps,
                       Level, State1, Locks)
    end,
    {NewSrcDeps, NewPkgDeps, NewSrcApps, State2, NewSeen, NewLocks}.

handle_upgrade(AppInfo, SrcDeps, PkgDeps, SrcApps, Level, State, Locks) ->
    Name = rebar_app_info:name(AppInfo),
    case lists:keyfind(Name, 1, Locks) of
        false ->
            case maybe_fetch(AppInfo, true, sets:new(), State) of
                true ->
                    handle_dep(AppInfo, SrcDeps, PkgDeps, SrcApps,
                               Level, State, Locks);

                false ->
                    {[AppInfo|SrcDeps], PkgDeps, SrcApps, State, Locks}
            end;
        _StillLocked ->
            {[AppInfo|SrcDeps], PkgDeps, SrcApps, State, Locks}
    end.

handle_dep(AppInfo, SrcDeps, PkgDeps, SrcApps, Level, State, Locks) ->
    DepsDir = rebar_dir:deps_dir(State),
    {AppInfo1, NewSrcDeps, NewPkgDeps, NewLocks} =
        handle_dep(State, DepsDir, AppInfo, Locks, Level),
    AppInfo2 = rebar_app_info:dep_level(AppInfo1, Level),
    {NewSrcDeps ++ SrcDeps
    ,NewPkgDeps++PkgDeps
    ,[AppInfo2 | SrcApps]
    ,State
    ,NewLocks}.

-spec handle_dep(rebar_state:t(), file:filename_all(), rebar_app_info:t(), list(), integer()) ->
                        {rebar_app_info:t(), [rebar_app_info:t()], [pkg_dep()]}.
handle_dep(State, DepsDir, AppInfo, Locks, Level) ->
    Profiles = rebar_state:current_profiles(State),
    Name = rebar_app_info:name(AppInfo),

    C = rebar_config:consult(rebar_app_info:dir(AppInfo)),

    S = rebar_app_info:state(AppInfo),
    S1 = rebar_state:new(S, C, rebar_app_info:dir(AppInfo)),
    S2 = rebar_state:apply_profiles(S1, Profiles),
    S3 = rebar_state:apply_overrides(S2, Name),
    AppInfo1 = rebar_app_info:state(AppInfo, S3),

    Deps = rebar_state:get(S3, deps, []),
    %% Upgrade lock level to be the level the dep will have in this dep tree
    NewLocks = [{DepName, Source, LockLevel+Level} ||
                   {DepName, Source, LockLevel} <- rebar_state:get(S3, {locks, default}, [])],
    AppInfo2 = rebar_app_info:deps(AppInfo1, rebar_state:deps_names(Deps)),
    {SrcDeps, PkgDeps} = parse_deps(DepsDir, Deps, S3, Locks, Level),
    {AppInfo2, SrcDeps, PkgDeps, Locks++NewLocks}.

-spec maybe_fetch(rebar_app_info:t(), boolean() | {true, binary(), integer()},
                  sets:set(binary()), rebar_state:t()) -> boolean().
maybe_fetch(AppInfo, Upgrade, Seen, State) ->
    AppDir = ec_cnv:to_list(rebar_app_info:dir(AppInfo)),
    %% Don't fetch dep if it exists in the _checkouts dir
    case in_checkouts(AppInfo) of
        true ->
            false;
        false ->
            case not app_exists(AppDir) of
                true ->
                    fetch_app(AppInfo, AppDir, State);
                false ->
                    case sets:is_element(rebar_app_info:name(AppInfo), Seen) of
                        true ->
                            false;
                        false ->
                            maybe_upgrade(AppInfo, AppDir, Upgrade, State)
                    end
            end
    end.

in_checkouts(AppInfo) ->
    Apps = rebar_app_discover:find_apps(["_checkouts"], all),
    case rebar_app_utils:find(rebar_app_info:name(AppInfo), Apps) of
        {ok, _} -> true;
        error -> false
    end.


-spec parse_deps(binary(), list(), list(), list(), integer()) -> {[rebar_app_info:t()], [pkg_dep()]}.
parse_deps(DepsDir, Deps, State, Locks, Level) ->
    lists:foldl(fun(Dep, Acc) ->
                        Name = case Dep of
                                   Dep when is_tuple(Dep) ->
                                       element(1, Dep);
                                   Dep ->
                                       Dep
                               end,
                        case lists:keyfind(ec_cnv:to_binary(Name), 1, Locks) of
                            false ->
                                parse_dep(Dep, Acc, DepsDir, State);
                            LockedDep ->
                                LockedLevel = element(3, LockedDep),
                                case LockedLevel > Level of
                                    true ->
                                        parse_dep(Dep, Acc, DepsDir, State);
                                    false ->
                                        parse_dep(LockedDep, Acc, DepsDir, State)
                                end
                        end
                end, {[], []}, Deps).

parse_dep({Name, Vsn}, {SrcDepsAcc, PkgDepsAcc}, _DepsDir, _State) when is_list(Vsn) ->
    {SrcDepsAcc, [parse_goal(ec_cnv:to_binary(Name)
                            ,ec_cnv:to_binary(Vsn)) | PkgDepsAcc]};
parse_dep(Name, {SrcDepsAcc, PkgDepsAcc}, _DepsDir, _State) when is_atom(Name) ->
    {SrcDepsAcc, [ec_cnv:to_binary(Name) | PkgDepsAcc]};
parse_dep({Name, Source}, {SrcDepsAcc, PkgDepsAcc}, DepsDir, State) when is_tuple(Source) ->
    Dep = new_dep(DepsDir, Name, [], Source, State),
    {[Dep | SrcDepsAcc], PkgDepsAcc};
parse_dep({Name, Source}, {SrcDepsAcc, PkgDepsAcc}, DepsDir, State) when is_tuple(Source) ->
    Dep = new_dep(DepsDir, Name, [], Source, State),
    {[Dep | SrcDepsAcc], PkgDepsAcc};
parse_dep({Name, _Vsn, Source}, {SrcDepsAcc, PkgDepsAcc}, DepsDir, State) when is_tuple(Source) ->
    Dep = new_dep(DepsDir, Name, [], Source, State),
    {[Dep | SrcDepsAcc], PkgDepsAcc};
parse_dep({Name, _Vsn, Source, Opts}, {SrcDepsAcc, PkgDepsAcc}, DepsDir, State) when is_tuple(Source) ->
    ?WARN("Dependency option list ~p in ~p is not supported and will be ignored", [Opts, Name]),
    Dep = new_dep(DepsDir, Name, [], Source, State),
    {[Dep | SrcDepsAcc], PkgDepsAcc};
parse_dep({Name, Source, Level}, {SrcDepsAcc, PkgDepsAcc}, DepsDir, State) when is_tuple(Source)
                                                                              , is_integer(Level) ->
    Dep = new_dep(DepsDir, Name, [], Source, State),
    {[Dep | SrcDepsAcc], PkgDepsAcc};
parse_dep(Dep, _, _, _) ->
    throw(?PRV_ERROR({parse_dep, Dep})).


new_dep(DepsDir, Name, Vsn, Source, State) ->
    Dir = ec_cnv:to_list(filename:join(DepsDir, Name)),
    {ok, Dep} = case rebar_app_info:discover(Dir) of
                    {ok, App} ->
                        {ok, App};
                    not_found ->
                        rebar_app_info:new(Name, Vsn, ec_cnv:to_list(filename:join(DepsDir, Name)))
                end,
    C = rebar_config:consult(rebar_app_info:dir(Dep)),
    S = rebar_state:new(rebar_state:new(), C, rebar_app_info:dir(Dep)),
    Overrides = rebar_state:get(State, overrides, []),
    ParentOverrides = rebar_state:overrides(State),
    Dep1 = rebar_app_info:state(Dep,
                               rebar_state:overrides(S, ParentOverrides++Overrides)),
    rebar_app_info:source(Dep1, Source).

app_exists(AppDir) ->
    case rebar_app_utils:is_app_dir(filename:absname(AppDir)++"-*") of
        {true, _} ->
            true;
        _ ->
            case rebar_app_utils:is_app_dir(filename:absname(AppDir)) of
                {true, _} ->
                    true;
                _ ->
                    false
            end
    end.

fetch_app(AppInfo, AppDir, State) ->
    ?INFO("Fetching ~s (~p)", [rebar_app_info:name(AppInfo), rebar_app_info:source(AppInfo)]),
    Source = rebar_app_info:source(AppInfo),
    case rebar_fetch:download_source(AppDir, Source, State) of
        {error, Reason} ->
            throw(Reason);
        Result ->
            Result
    end.

maybe_upgrade(AppInfo, AppDir, false, _State) ->
    Source = rebar_app_info:source(AppInfo),
    rebar_fetch:needs_update(AppDir, Source);
maybe_upgrade(AppInfo, AppDir, true, State) ->
    Source = rebar_app_info:source(AppInfo),
    case rebar_fetch:needs_update(AppDir, Source) of
        true ->
            ?INFO("Updating ~s", [rebar_app_info:name(AppInfo)]),
            case rebar_fetch:download_source(AppDir, Source, State) of
                {error, Reason} ->
                    throw(Reason);
                Result ->
                    Result
            end;
        false ->
            false
    end.

-spec parse_goal(binary(), binary()) -> pkg_dep().
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
                 "  ~p~n",
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

warn_skip_deps(AppInfo) ->
    ?WARN("Skipping ~s (from ~p) as an app of the same name "
          "has already been fetched~n",
          [rebar_app_info:name(AppInfo),
           rebar_app_info:source(AppInfo)]).

warn_skip_pkg({Name, Source}) ->
    ?WARN("Skipping ~s (version ~s from package index) as an app of the same "
          "name has already been fetched~n",
          [Name, Source]).
