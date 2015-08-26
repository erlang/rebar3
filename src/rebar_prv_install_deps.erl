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

-export([handle_deps_as_profile/4,
         profile_dep_dir/2,
         find_cycles/1,
         cull_compile/2]).

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
                                                               {bare, false},
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

        Upgrade = rebar_state:get(State, upgrade, false),
        {Apps, State1} = deps_per_profile(Profiles, Upgrade, State),

        State2 = rebar_state:update_all_deps(State1, Apps),
        CodePaths = [rebar_app_info:ebin_dir(A) || A <- Apps],
        State3 = rebar_state:update_code_paths(State2, all_deps, CodePaths),

        Source = ProjectApps ++ Apps,
        case find_cycles(Source) of
            {cycles, Cycles} ->
                ?PRV_ERROR({cycles, Cycles});
            {error, Error} ->
                {error, Error};
            {no_cycle, Sorted} ->
                ToCompile = cull_compile(Sorted, ProjectApps),
                {ok, rebar_state:deps_to_build(State3, ToCompile)}
        end
    catch
        %% maybe_fetch will maybe_throw an exception to break out of some loops
        _:{error, Reason} ->
            {error, Reason}
    end.

-spec format_error(any()) -> iolist().
format_error({dep_app_not_found, AppDir, AppName}) ->
    io_lib:format("Dependency failure: Application ~s not found at the top level of directory ~s", [AppName, AppDir]);
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
format_error({missing_package, Package}) ->
    io_lib:format("Package not found in registry: ~s", [Package]);
format_error({cycles, Cycles}) ->
    Prints = [["applications: ",
               [io_lib:format("~s ", [Dep]) || Dep <- Cycle],
               "depend on each other~n"]
              || Cycle <- Cycles],
    ["Dependency cycle(s) detected:~n", Prints];
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% Allows other providers to install deps in a given profile
%% manually, outside of what is provided by rebar3's deps tuple.
handle_deps_as_profile(Profile, State, Deps, Upgrade) ->
    Locks = [],
    Level = 0,
    DepsDir = profile_dep_dir(State, Profile),
    Deps1 = rebar_app_utils:parse_deps(DepsDir, Deps, State, Locks, Level),
    ProfileLevelDeps = [{Profile, Deps1, Level}],
    handle_profile_level(ProfileLevelDeps, [], sets:new(), Upgrade, Locks, State).

%% ===================================================================
%% Internal functions
%% ===================================================================

%% finds all the deps in `{deps, ...}` for each profile provided.
deps_per_profile(Profiles, Upgrade, State) ->
    Level = 0,
    Locks = rebar_state:get(State, {locks, default}, []),
    Deps = lists:foldl(fun(Profile, DepAcc) ->
                               [parsed_profile_deps(State, Profile, Level) | DepAcc]
                       end, [], Profiles),
    handle_profile_level(Deps, [], sets:new(), Upgrade, Locks, State).

parsed_profile_deps(State, Profile, Level) ->
    ParsedDeps = rebar_state:get(State, {parsed_deps, Profile}, []),
    {Profile, ParsedDeps, Level}.

%% Level-order traversal of all dependencies, across profiles.
%% If profiles x,y,z are present, then the traversal will go:
%% x0, y0, z0, x1, y1, z1, ..., xN, yN, zN.
handle_profile_level([], Apps, _Seen, _Upgrade, _Locks, State) ->
    {Apps, State};
handle_profile_level([{Profile, Deps, Level} | Rest], Apps, Seen, Upgrade, Locks, State) ->
    {Deps1, Apps1, State1, Seen1} =
        update_deps(Profile, Level, Deps, Apps
                   ,State, Upgrade, Seen, Locks),
    Deps2 = case Deps1 of
        [] -> Rest;
        _ -> Rest ++ [{Profile, Deps1, Level+1}]
    end,
    handle_profile_level(Deps2, Apps1, sets:union(Seen, Seen1), Upgrade, Locks, State1).

find_cycles(Apps) ->
    case rebar_digraph:compile_order(Apps) of
        {error, {cycles, Cycles}} -> {cycles, Cycles};
        {error, Error} -> {error, Error};
        {ok, Sorted} -> {no_cycle, Sorted}
    end.

cull_compile(TopSortedDeps, ProjectApps) ->
    lists:dropwhile(fun not_needs_compile/1, TopSortedDeps -- ProjectApps).

maybe_lock(Profile, AppInfo, Seen, State, Level) ->
    Name = rebar_app_info:name(AppInfo),
    case rebar_app_info:is_checkout(AppInfo) of
        false ->
            case Profile of
                default ->
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
                    {sets:add_element(Name, Seen), State}
            end;
        true ->
            {sets:add_element(Name, Seen), State}
    end.

update_deps(Profile, Level, Deps, Apps, State, Upgrade, Seen, Locks) ->
    lists:foldl(
      fun(AppInfo, {DepsAcc, AppsAcc, StateAcc, SeenAcc}) ->
              update_dep(AppInfo, Profile, Level,
                         DepsAcc, AppsAcc, StateAcc,
                         Upgrade, SeenAcc, Locks)
      end,
      {[], Apps, State, Seen},
      rebar_utils:sort_deps(Deps)).

update_dep(AppInfo, Profile, Level, Deps, Apps, State, Upgrade, Seen, Locks) ->
    %% If not seen, add to list of locks to write out
    Name = rebar_app_info:name(AppInfo),
    case sets:is_element(Name, Seen) of
        true ->
            update_seen_dep(AppInfo, Profile, Level,
                            Deps, Apps,
                            State, Upgrade, Seen, Locks);
        false ->
            update_unseen_dep(AppInfo, Profile, Level,
                              Deps, Apps,
                              State, Upgrade, Seen, Locks)
    end.

profile_dep_dir(State, Profile) ->
    case Profile of
        default -> filename:join([rebar_dir:profile_dir(State, [default]), rebar_state:get(State, deps_dir, ?DEFAULT_DEPS_DIR)]);
        _ -> rebar_dir:deps_dir(State)
    end.

update_seen_dep(AppInfo, _Profile, _Level, Deps, Apps, State, Upgrade, Seen, Locks) ->
    Name = rebar_app_info:name(AppInfo),
    %% If seen from lock file or user requested an upgrade
    %% don't print warning about skipping
    case lists:keymember(Name, 1, Locks) of
        false when Upgrade -> ok;
        false when not Upgrade -> warn_skip_deps(AppInfo, State);
        true -> ok
    end,
    {Deps, Apps, State, Seen}.

update_unseen_dep(AppInfo, Profile, Level, Deps, Apps, State, Upgrade, Seen, Locks) ->
    {NewSeen, State1} = maybe_lock(Profile, AppInfo, Seen, State, Level),
    {_, AppInfo1} = maybe_fetch(AppInfo, Profile, Upgrade, Seen, State1),
    DepsDir = profile_dep_dir(State, Profile),
    {AppInfo2, NewDeps, State2} =
        handle_dep(State1, Profile, DepsDir, AppInfo1, Locks, Level),
    AppInfo3 = rebar_app_info:dep_level(AppInfo2, Level),
    {NewDeps ++ Deps, [AppInfo3 | Apps], State2, NewSeen}.

-spec handle_dep(rebar_state:t(), atom(), file:filename_all(), rebar_app_info:t(), list(), integer()) -> {rebar_app_info:t(), [rebar_app_info:t()], [pkg_dep()], [integer()], rebar_state:t()}.
handle_dep(State, Profile, DepsDir, AppInfo, Locks, Level) ->
    Profiles = rebar_state:current_profiles(State),
    Name = rebar_app_info:name(AppInfo),

    %% Deps may be under a sub project app, find it and use its state if so
    S = rebar_app_info:state(AppInfo),
    C = rebar_config:consult(rebar_app_info:dir(AppInfo)),
    S1 = rebar_state:new(S, C, AppInfo),
    S2 = rebar_state:apply_overrides(S1, Name),

    S3 = rebar_state:apply_profiles(S2, Profiles),
    Plugins = rebar_state:get(S3, plugins, []),
    S4 = rebar_state:set(S3, {plugins, Profile}, Plugins),

    rebar_utils:check_min_otp_version(rebar_state:get(S4, minimum_otp_vsn, undefined)),
    rebar_utils:check_blacklisted_otp_versions(rebar_state:get(S4, blacklisted_otp_vsns, [])),

    %% Dep may have plugins to install. Find and install here.
    S5 = rebar_plugins:install(S4),
    AppInfo1 = rebar_app_info:state(AppInfo, S5),

    %% Upgrade lock level to be the level the dep will have in this dep tree
    Deps = rebar_state:get(S5, {deps, default}, []),
    AppInfo2 = rebar_app_info:deps(AppInfo1, rebar_state:deps_names(Deps)),
    Deps1 = rebar_app_utils:parse_deps(Name, DepsDir, Deps, S5, Locks, Level+1),
    {AppInfo2, Deps1, State}.

-spec maybe_fetch(rebar_app_info:t(), atom(), boolean(),
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
                    true = fetch_app(AppInfo, AppDir, State),
                    maybe_symlink_default(State, Profile, AppDir, AppInfo),
                    {true, rebar_app_info:valid(update_app_info(AppDir, AppInfo), false)};
                {true, AppInfo1} ->
                    %% Preserve the state we created with overrides
                    AppInfo2 = copy_app_info(AppInfo, AppInfo1),
                    AppState = rebar_app_info:state(AppInfo),
                    AppInfo3 = rebar_app_info:state(AppInfo2, AppState),
                    case sets:is_element(rebar_app_info:name(AppInfo3), Seen) of
                        true ->
                            {false, AppInfo3};
                        false ->
                            maybe_symlink_default(State, Profile, AppDir, AppInfo3),
                            MaybeUpgrade = maybe_upgrade(AppInfo, AppDir, Upgrade, State),
                            AppInfo4 = update_app_info(AppDir, AppInfo3),
                            {MaybeUpgrade, AppInfo4}
                    end
            end
    end.

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
            symlink_dep(State, AppDir, SymDir),
            true;
        false ->
            false
    end.

symlink_dep(State, From, To) ->
    filelib:ensure_dir(To),
    case rebar_file_utils:symlink_or_copy(From, To) of
        ok ->
            RelativeFrom = make_relative_to_root(State, From),
            RelativeTo = make_relative_to_root(State, To),
            ?INFO("Linking ~s to ~s", [RelativeFrom, RelativeTo]),
            ok;
        exists ->
            ok
    end.

make_relative_to_root(State, Path) when is_binary(Path) ->
    make_relative_to_root(State, binary_to_list(Path));
make_relative_to_root(State, Path) when is_list(Path) ->
    Root = rebar_dir:root_dir(State),
    rebar_dir:make_relative_path(Path, Root).

fetch_app(AppInfo, AppDir, State) ->
    ?INFO("Fetching ~s (~p)", [rebar_app_info:name(AppInfo), rebar_app_info:source(AppInfo)]),
    Source = rebar_app_info:source(AppInfo),
    true = rebar_fetch:download_source(AppDir, Source, State).

%% This is called after the dep has been downloaded and unpacked, if it hadn't been already.
%% So this is the first time for newly downloaded apps that its .app/.app.src data can
%% be read in an parsed.
update_app_info(AppDir, AppInfo) ->
    case rebar_app_info:discover(AppDir) of
        {ok, Found} ->
            AppDetails = rebar_app_info:app_details(Found),
            Vsn = rebar_app_info:original_vsn(Found),
            Applications = proplists:get_value(applications, AppDetails, []),
            IncludedApplications = proplists:get_value(included_applications, AppDetails, []),
            AppInfo1 = rebar_app_info:original_vsn(rebar_app_info:applications(
                                                     rebar_app_info:app_details(AppInfo, AppDetails),
                                                     IncludedApplications++Applications), Vsn),
            AppInfo2 = copy_app_info(AppInfo, AppInfo1),
            rebar_app_info:valid(AppInfo2, undefined);
        not_found ->
            throw(?PRV_ERROR({dep_app_not_found, AppDir, rebar_app_info:name(AppInfo)}))
    end.

copy_app_info(OldAppInfo, NewAppInfo) ->
    Deps = rebar_app_info:deps(OldAppInfo),
    ResourceType = rebar_app_info:resource_type(OldAppInfo),
    Parent = rebar_app_info:parent(OldAppInfo),
    Source = rebar_app_info:source(OldAppInfo),

    rebar_app_info:deps(
      rebar_app_info:resource_type(
        rebar_app_info:source(
          rebar_app_info:parent(NewAppInfo, Parent), Source), ResourceType), Deps).

maybe_upgrade(AppInfo, AppDir, Upgrade, State) ->
    Source = rebar_app_info:source(AppInfo),
    case Upgrade orelse rebar_app_info:is_lock(AppInfo) of
        true ->
            case rebar_fetch:needs_update(AppDir, Source, State) of
                true ->
                    ?INFO("Upgrading ~s", [rebar_app_info:name(AppInfo)]),
                    true = rebar_fetch:download_source(AppDir, Source, State);
                false ->
                    case Upgrade of
                        true ->
                            ?INFO("No upgrade needed for ~s", [rebar_app_info:name(AppInfo)]),
                            false;
                        false ->
                            false
                    end
            end;
        false ->
            false
    end.

warn_skip_deps(AppInfo, State) ->
    Msg = "Skipping ~s (from ~p) as an app of the same name "
          "has already been fetched",
    Args = [rebar_app_info:name(AppInfo),
            rebar_app_info:source(AppInfo)],
    case rebar_state:get(State, deps_error_on_conflict, false) of
        false -> ?WARN(Msg, Args);
        true -> ?ERROR(Msg, Args), ?FAIL
    end.

not_needs_compile(App) ->
    not(rebar_app_info:is_checkout(App))
        andalso rebar_app_info:valid(App)
          andalso rebar_state:has_all_artifacts(rebar_app_info:state(App)) =:= true.
