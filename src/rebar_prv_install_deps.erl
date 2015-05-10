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
                                                               {short_desc, ""},
                                                               {desc, ""},
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
            {no_cycle, Sorted} ->
                ToCompile = cull_compile(Sorted, ProjectApps),
                {ok, rebar_state:deps_to_build(State1, ToCompile)}
        end
    catch
        %% maybe_fetch will maybe_throw an exception to break out of some loops
        _:{error, Reason} ->
            {error, Reason}
    end.

-spec format_error(any()) -> iolist().
format_error({load_registry_fail, Dep}) ->
    io_lib:format("Error loading registry to resolve version of ~s. Try fixing by running 'rebar3 update'", [Dep]);
format_error({bad_constraint, Name, Constraint}) ->
    io_lib:format("Unable to parse version for package ~s: ~s", [Name, Constraint]);
format_error({parse_dep, Dep}) ->
    io_lib:format("Failed parsing dep ~p", [Dep]);
format_error({not_rebar_package, Package, Version}) ->
    io_lib:format("Package not buildable with rebar3: ~s-~s", [Package, Version]);
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
handle_deps(Profile, State0, Deps, Upgrade, Locks) ->
    %% Split source deps from pkg deps, needed to keep backwards compatibility
    DepsDir = profile_dep_dir(State0, Profile),

    {SrcDeps, PkgDeps} = parse_deps(DepsDir, Deps, State0, Locks, 0),

    %% Fetch transitive src deps
    {State1, SrcApps, PkgDeps1, Seen} = update_src_deps(Profile, 0, SrcDeps, PkgDeps, []
                                                       ,State0, Upgrade, sets:new(), Locks),

    {Solved, State4} =
        case PkgDeps1 of
            [] ->
                {[], State1};
            _ ->
                %% Read in package index and dep graph
                {Packages, Graph} = rebar_state:packages(State1),
                Registry = rebar_packages:registry(State1),
                State2 = rebar_state:packages(rebar_state:registry(State1, Registry)
                                            ,{Packages, Graph}),

                update_pkg_deps(Profile, Packages, PkgDeps1
                               ,Graph, Upgrade, Seen, State2)
        end,

    AllDeps = lists:ukeymerge(2
                             ,lists:ukeysort(2, SrcApps)
                             ,lists:ukeysort(2, Solved)),

    State5 = rebar_state:update_all_deps(State4, AllDeps),
    CodePaths = [rebar_app_info:ebin_dir(A) || A <- AllDeps],
    State6 = rebar_state:update_code_paths(State5, all_deps, CodePaths),

    {ok, AllDeps, State6}.

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
        {ok, Sorted} -> {no_cycle, Sorted}
    end.

cull_compile(TopSortedDeps, ProjectApps) ->
    lists:dropwhile(fun not_needs_compile/1, TopSortedDeps -- ProjectApps).

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
                    [warn_skip_pkg(Pkg, State) || Pkg <- Discarded],
                    Solution
            end,
            update_pkg_deps(Profile, S, Packages, Upgrade, Seen, State)
    end.

update_pkg_deps(Profile, Pkgs, Packages, Upgrade, Seen, State) ->
    %% Create app_info record for each pkg dep
    DepsDir = profile_dep_dir(State, Profile),
    {Solved, _, State1}
        = lists:foldl(fun(Pkg, {Acc, SeenAcc, StateAcc}) ->
                        handle_pkg_dep(Profile, Pkg, Packages, Upgrade, DepsDir, Acc, SeenAcc, StateAcc)
                      end, {[], Seen, State}, Pkgs),
    {Solved, State1}.

handle_pkg_dep(Profile, Pkg, Packages, Upgrade, DepsDir, Fetched, Seen, State) ->
    AppInfo = package_to_app(DepsDir, Packages, Pkg, State),
    Level = rebar_app_info:dep_level(AppInfo),
    {NewSeen, NewState} = maybe_lock(Profile, AppInfo, Seen, State, Level),
    {_, AppInfo1} = maybe_fetch(AppInfo, Profile, Upgrade, Seen, NewState),
    {[AppInfo1 | Fetched], NewSeen, NewState}.

maybe_lock(Profile, AppInfo, Seen, State, Level) ->
    case rebar_app_info:is_checkout(AppInfo) of
        false ->
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
            end;
        true ->
            {Seen, State}
    end.

package_to_app(DepsDir, Packages, {Name, Vsn, Level}, State) ->
    case dict:find({Name, Vsn}, Packages) of
        error ->
            case rebar_packages:check_registry(Name, Vsn, State) of
                true ->
                    throw(?PRV_ERROR({not_rebar_package, Name, Vsn}));
                false ->
                    throw(?PRV_ERROR({missing_package, Name, Vsn}))
            end;
        {ok, P} ->
            PkgDeps = [{PkgName, PkgVsn}
                       || {PkgName,PkgVsn} <- proplists:get_value(<<"deps">>, P, [])],
            {ok, AppInfo} = rebar_app_info:new(Name, Vsn),
            AppInfo1 = rebar_app_info:deps(AppInfo, PkgDeps),
            AppInfo2 = rebar_app_info:dir(AppInfo1, filename:join([DepsDir, Name])),
            AppInfo3 = rebar_app_info:dep_level(AppInfo2, Level),
            rebar_app_info:source(AppInfo3, {pkg, Name, Vsn})
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
            update_seen_src_dep(AppInfo, Profile, Level,
                                SrcDeps, PkgDeps, SrcApps,
                                State, Upgrade, Seen, BaseLocks, Locks);
        false ->
            update_unseen_src_dep(AppInfo, Profile, Level,
                                  SrcDeps, PkgDeps, SrcApps,
                                  State, Upgrade, Seen, Locks)

    end.

profile_dep_dir(State, Profile) ->
    case Profile of
        default -> filename:join([rebar_dir:profile_dir(State, [default]), rebar_state:get(State, deps_dir, ?DEFAULT_DEPS_DIR)]);
        _ -> rebar_dir:deps_dir(State)
    end.

update_seen_src_dep(AppInfo, Profile, Level, SrcDeps, PkgDeps, SrcApps, State, Upgrade, Seen, BaseLocks, Locks) ->
    Name = rebar_app_info:name(AppInfo),
    %% If seen from lock file or user requested an upgrade
    %% don't print warning about skipping
    case lists:keymember(Name, 1, BaseLocks) of
        false when Upgrade -> ok;
        false when not Upgrade -> warn_skip_deps(AppInfo, State);
        true -> ok
    end,
    %% scan for app children here if upgrading
    case Upgrade of
        false ->
            {SrcDeps, PkgDeps, SrcApps, State, Seen, Locks};
        true ->
            {NewSrcDeps, NewPkgDeps, NewSrcApps, NewState, NewLocks}
                = handle_dep(AppInfo, Profile, SrcDeps, PkgDeps, SrcApps,
                             Level, State, Locks),
            {NewSrcDeps, NewPkgDeps, NewSrcApps, NewState, Seen, NewLocks}
    end.

update_unseen_src_dep(AppInfo, Profile, Level, SrcDeps, PkgDeps, SrcApps, State, Upgrade, Seen, Locks) ->
    {NewSeen, State1} = maybe_lock(Profile, AppInfo, Seen, State, Level),
    {NewSrcDeps, NewPkgDeps, NewSrcApps, State2, NewLocks}
        = case Upgrade of
            true ->
                handle_upgrade(AppInfo, Profile, SrcDeps, PkgDeps, SrcApps,
                               Level, State1, Locks);
            _ ->
                {_, AppInfo1} = maybe_fetch(AppInfo, Profile, false, Seen, State1),
                handle_dep(AppInfo1, Profile, SrcDeps, PkgDeps, SrcApps,
                           Level, State1, Locks)
        end,
    {NewSrcDeps, NewPkgDeps, NewSrcApps, State2, NewSeen, NewLocks}.

handle_upgrade(AppInfo, Profile, SrcDeps, PkgDeps, SrcApps, Level, State, Locks) ->
    Name = rebar_app_info:name(AppInfo),
    case lists:keyfind(Name, 1, Locks) of
        false ->
            case maybe_fetch(AppInfo, Profile, true, sets:new(), State) of
                {true, AppInfo1} ->
                    handle_dep(AppInfo1, Profile, SrcDeps, PkgDeps, SrcApps,
                               Level, State, Locks);

                {false, AppInfo1} ->
                    {[AppInfo1|SrcDeps], PkgDeps, SrcApps, State, Locks}
            end;
        _StillLocked ->
            {[AppInfo|SrcDeps], PkgDeps, SrcApps, State, Locks}
    end.

handle_dep(AppInfo, Profile, SrcDeps, PkgDeps, SrcApps, Level, State, Locks) ->
    DepsDir = profile_dep_dir(State, Profile),
    {AppInfo1, NewSrcDeps, NewPkgDeps, NewLocks, State1} =
        handle_dep(State, DepsDir, AppInfo, Locks, Level),
    AppInfo2 = rebar_app_info:dep_level(AppInfo1, Level),
    {NewSrcDeps ++ SrcDeps
    ,NewPkgDeps++PkgDeps
    ,[AppInfo2 | SrcApps]
    ,State1
    ,NewLocks}.

-spec handle_dep(rebar_state:t(), file:filename_all(), rebar_app_info:t(), list(), integer()) ->
                        {rebar_app_info:t(), [rebar_app_info:t()], [pkg_dep()], [integer()]}.
handle_dep(State, DepsDir, AppInfo, Locks, Level) ->
    Profiles = rebar_state:current_profiles(State),
    Name = rebar_app_info:name(AppInfo),

    C = rebar_config:consult(rebar_app_info:dir(AppInfo)),

    S = rebar_app_info:state(AppInfo),
    S1 = rebar_state:new(S, C, rebar_app_info:dir(AppInfo)),
    S2 = rebar_state:apply_profiles(S1, Profiles),
    S3 = rebar_state:apply_overrides(S2, Name),
    AppInfo1 = rebar_app_info:state(AppInfo, S3),

    %% Dep may have plugins to install. Find and install here.
    State1 = rebar_plugins:handle_plugins(rebar_state:get(S3, plugins, []), State),

    Deps = rebar_state:get(S3, deps, []),
    %% Upgrade lock level to be the level the dep will have in this dep tree
    NewLocks = [{DepName, Source, LockLevel+Level} ||
                   {DepName, Source, LockLevel} <- rebar_state:get(S3, {locks, default}, [])],
    AppInfo2 = rebar_app_info:deps(AppInfo1, rebar_state:deps_names(Deps)),
    {SrcDeps, PkgDeps} = parse_deps(DepsDir, Deps, S3, Locks, Level),
    {AppInfo2, SrcDeps, PkgDeps, Locks++NewLocks, State1}.

-spec maybe_fetch(rebar_app_info:t(), atom(), boolean() | {true, binary(), integer()},
                  sets:set(binary()), rebar_state:t()) -> {boolean(), rebar_app_info:t()}.
maybe_fetch(AppInfo, Profile, Upgrade, Seen, State) ->
    AppDir = ec_cnv:to_list(rebar_app_info:dir(AppInfo)),
    %% Don't fetch dep if it exists in the _checkouts dir
    case rebar_app_info:is_checkout(AppInfo) of
        true ->
            {false, AppInfo};
        false ->
            case rebar_app_discover:find_app(AppDir, all) of
                false ->
                    case already_in_default(AppInfo, State) of
                        false ->
                            case fetch_app(AppInfo, AppDir, State) of
                                true ->
                                    maybe_symlink_default(State, Profile, AppDir, AppInfo),
                                    {true, update_app_info(AppInfo)};
                                Other ->
                                    {Other, AppInfo}
                            end;
                        {true, FoundApp} ->
                            %% Preserve the state we created with overrides
                            AppState = rebar_app_info:state(AppInfo),
                            FoundApp1 = rebar_app_info:state(FoundApp, AppState),
                            symlink_dep(rebar_app_info:dir(FoundApp1), AppDir),
                            {true, FoundApp1}
                    end;
                {true, AppInfo1} ->
                    %% Preserve the state we created with overrides
                    AppState = rebar_app_info:state(AppInfo),
                    AppInfo2 = rebar_app_info:state(AppInfo1, AppState),
                    maybe_symlink_default(State, Profile, AppDir, AppInfo2),
                    case sets:is_element(rebar_app_info:name(AppInfo), Seen) of
                        true ->
                            {false, AppInfo2};
                        false ->
                            {maybe_upgrade(AppInfo, AppDir, Upgrade, State), AppInfo2}
                    end
            end
    end.

already_in_default(AppInfo, State) ->
    Name = ec_cnv:to_list(rebar_app_info:name(AppInfo)),
    DefaultAppDir = filename:join([rebar_state:get(State, base_dir), "default", "lib", Name]),
    rebar_app_discover:find_app(DefaultAppDir, all).

needs_symlinking(State, Profile) ->
    case {rebar_state:current_profiles(State), Profile} of
        {[default], default} ->
            %% file will be in default already -- this is the only run we have
            false;
        {_, default} ->
            %% file fetched to default, needs to be linked to the current
            %% run's directory.
            true;
        _ ->
            %% File fetched to the right directory already
            false
    end.

maybe_symlink_default(State, Profile, AppDir, AppInfo) ->
    case needs_symlinking(State, Profile) of
        true ->
            SymDir = filename:join([rebar_dir:deps_dir(State),
                                    rebar_app_info:name(AppInfo)]),
            symlink_dep(AppDir, SymDir),
            true;
        false ->
            false
    end.


symlink_dep(From, To) ->
    ?INFO("Linking ~s to ~s", [From, To]),
    filelib:ensure_dir(To),
    rebar_file_utils:symlink_or_copy(From, To).

-spec parse_deps(binary(), list(), rebar_state:t(), list(), integer()) -> {[rebar_app_info:t()], [pkg_dep()]}.
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

parse_dep({Name, Vsn}, {SrcDepsAcc, PkgDepsAcc}, DepsDir, State) when is_list(Vsn) ->
    CheckoutsDir = ec_cnv:to_list(rebar_dir:checkouts_dir(State, Name)),
    case rebar_app_info:discover(CheckoutsDir) of
        {ok, _App} ->
            Dep = new_dep(DepsDir, Name, [], [], State),
            {[Dep | SrcDepsAcc], PkgDepsAcc};
        not_found ->
            {SrcDepsAcc, [parse_goal(ec_cnv:to_binary(Name)
                                    ,ec_cnv:to_binary(Vsn)) | PkgDepsAcc]}
    end;
parse_dep(Name, {SrcDepsAcc, PkgDepsAcc}, DepsDir, State) when is_atom(Name) ->
    {PkgName, PkgVsn} = get_package(ec_cnv:to_binary(Name), State),
    CheckoutsDir = ec_cnv:to_list(rebar_dir:checkouts_dir(State, Name)),
    case rebar_app_info:discover(CheckoutsDir) of
        {ok, _App} ->
            Dep = new_dep(DepsDir, Name, [], [], State),
            {[Dep | SrcDepsAcc], PkgDepsAcc};
        not_found ->
            {SrcDepsAcc, [{PkgName, PkgVsn} | PkgDepsAcc]}
    end;
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
parse_dep({_Name, {pkg, Name, Vsn}, Level}, {SrcDepsAcc, PkgDepsAcc}, DepsDir, State) when is_integer(Level) ->
    CheckoutsDir = ec_cnv:to_list(rebar_dir:checkouts_dir(State, Name)),
    case rebar_app_info:discover(CheckoutsDir) of
        {ok, _App} ->
            Dep = new_dep(DepsDir, Name, [], [], State),
            {[Dep | SrcDepsAcc], PkgDepsAcc};
        not_found ->
            {SrcDepsAcc, [{Name, Vsn} | PkgDepsAcc]}
    end;
parse_dep({Name, Source, Level}, {SrcDepsAcc, PkgDepsAcc}, DepsDir, State) when is_tuple(Source)
                                                                              , is_integer(Level) ->
    Dep = new_dep(DepsDir, Name, [], Source, State),
    {[Dep | SrcDepsAcc], PkgDepsAcc};
parse_dep(Dep, _, _, _) ->
    throw(?PRV_ERROR({parse_dep, Dep})).


new_dep(DepsDir, Name, Vsn, Source, State) ->
    CheckoutsDir = ec_cnv:to_list(rebar_dir:checkouts_dir(State, Name)),
    {ok, Dep} = case rebar_app_info:discover(CheckoutsDir) of
                    {ok, App} ->
                        {ok, rebar_app_info:is_checkout(App, true)};
                    not_found ->
                        Dir = ec_cnv:to_list(filename:join(DepsDir, Name)),
                        case rebar_app_info:discover(Dir) of
                            {ok, App} ->
                                {ok, App};
                            not_found ->
                                rebar_app_info:new(Name, Vsn,
                                                   ec_cnv:to_list(filename:join(DepsDir, Name)))
                        end
                end,
    C = rebar_config:consult(rebar_app_info:dir(Dep)),
    S = rebar_state:new(rebar_state:new(), C, rebar_app_info:dir(Dep)),
    Overrides = rebar_state:get(State, overrides, []),
    ParentOverrides = rebar_state:overrides(State),
    Dep1 = rebar_app_info:state(Dep,
                               rebar_state:overrides(S, ParentOverrides++Overrides)),
    rebar_app_info:source(Dep1, Source).

fetch_app(AppInfo, AppDir, State) ->
    ?INFO("Fetching ~s (~p)", [rebar_app_info:name(AppInfo), rebar_app_info:source(AppInfo)]),
    Source = rebar_app_info:source(AppInfo),
    case rebar_fetch:download_source(AppDir, Source, State) of
        true ->
            true;
        Error ->
            throw(Error)
    end.

update_app_info(AppInfo) ->
    AppDetails = rebar_app_info:app_details(AppInfo),
    Applications = proplists:get_value(applications, AppDetails, []),
    IncludedApplications = proplists:get_value(included_applications, AppDetails, []),
    AppInfo1 = rebar_app_info:applications(
                 rebar_app_info:app_details(AppInfo, AppDetails),
                 IncludedApplications++Applications),
    rebar_app_info:valid(AppInfo1, false).

maybe_upgrade(AppInfo, AppDir, false, State) ->
    Source = rebar_app_info:source(AppInfo),
    rebar_fetch:needs_update(AppDir, Source, State);
maybe_upgrade(AppInfo, AppDir, true, State) ->
    Source = rebar_app_info:source(AppInfo),
    case rebar_fetch:needs_update(AppDir, Source, State) of
        true ->
            ?INFO("Upgrading ~s", [rebar_app_info:name(AppInfo)]),
            case rebar_fetch:download_source(AppDir, Source, State) of
                true ->
                    true;
                Error ->
                    throw(Error)
            end;
        false ->
            ?INFO("No upgrade needed for ~s", [rebar_app_info:name(AppInfo)]),
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
            throw(?PRV_ERROR({bad_constraint, Name, Constraint}))
    end.

warn_skip_deps(AppInfo, State) ->
    Msg = "Skipping ~s (from ~p) as an app of the same name "
          "has already been fetched~n",
    Args = [rebar_app_info:name(AppInfo),
            rebar_app_info:source(AppInfo)],
    case rebar_state:get(State, deps_error_on_conflict, false) of
        false -> ?WARN(Msg, Args);
        true -> ?ERROR(Msg, Args), ?FAIL
    end.

warn_skip_pkg({Name, Source}, State) ->
    Msg = "Skipping ~s (version ~s from package index) as an app of the same "
          "name has already been fetched~n",
    Args = [Name, Source],
    case rebar_state:get(State, deps_error_on_conflict, false) of
        false -> ?WARN(Msg, Args);
        true -> ?ERROR(Msg, Args), ?FAIL
    end.

not_needs_compile(App) ->
    not(rebar_app_info:is_checkout(App))
        andalso rebar_app_info:valid(App).

get_package(Dep, State) ->
    case rebar_state:registry(State) of
        {ok, T} ->
            {ok, HighestDepVsn} = rebar_packages:find_highest_matching(Dep, "0", T),
            {Dep, HighestDepVsn};
        error ->
            throw(?PRV_ERROR({load_registry_fail, Dep}))
    end.
