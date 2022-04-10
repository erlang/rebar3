%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

-module(rebar_plugins).

-export([project_plugins_install/1
        ,top_level_install/1
        ,project_apps_install/1
        ,install/2
        ,handle_plugins/3
        ,handle_plugins/4]).

-include("rebar.hrl").

%% ===================================================================
%% Public API
%% ===================================================================

-spec project_plugins_install(rebar_state:t()) -> rebar_state:t().
project_plugins_install(State) ->
    Profiles = rebar_state:current_profiles(State),
    State1 = rebar_state:allow_provider_overrides(State, true),
    State2 = lists:foldl(fun(Profile, StateAcc) ->
                             Plugins = rebar_state:get(State, {project_plugins, Profile}, []),
                             handle_plugins(Profile, Plugins, StateAcc)
                         end, State1, Profiles),
    rebar_state:allow_provider_overrides(State2, false).

-spec top_level_install(rebar_state:t()) -> rebar_state:t().
top_level_install(State) ->
    Profiles = rebar_state:current_profiles(State),
    lists:foldl(fun(Profile, StateAcc) ->
                        Plugins = rebar_state:get(State, {plugins, Profile}, []),
                        handle_plugins(Profile, Plugins, StateAcc)
                end, State, Profiles).

-spec project_apps_install(rebar_state:t()) -> rebar_state:t().
project_apps_install(State) ->
    Profiles = rebar_state:current_profiles(State),
    ProjectApps = rebar_state:project_apps(State),
    lists:foldl(fun(Profile, StateAcc) ->
                        StateAcc1 = case Profile of
                                        default ->
                                            %% default profile top level plugins
                                            %% are installed in run_aux
                                            StateAcc;
                                        _ ->
                                            Plugins = rebar_state:get(State, {plugins, Profile}, []),
                                            handle_plugins(Profile, Plugins, StateAcc)
                                    end,

                        lists:foldl(fun(AppInfo, StateAcc2) ->
                                            Plugins2 = rebar_app_info:get(AppInfo, {plugins, Profile}, []),
                                            handle_plugins(Profile, Plugins2, StateAcc2)
                                    end, StateAcc1, ProjectApps)
                end, State, Profiles).

-spec install(rebar_state:t(), rebar_app_info:t()) -> rebar_state:t().
install(State, AppInfo) ->
    Profiles = rebar_state:current_profiles(State),

    %% don't lose the overrides of the dep we are processing plugins for
    Overrides = rebar_app_info:get(AppInfo, overrides, []),
    StateOverrides = rebar_state:get(State, overrides, []),
    AllOverrides = Overrides ++ StateOverrides,
    State1 = rebar_state:set(State, overrides, AllOverrides),

    State2 = lists:foldl(fun(Profile, StateAcc) ->
                             Plugins = rebar_app_info:get(AppInfo, {plugins, Profile}, []),
                             Plugins1 = filter_existing_plugins(Plugins, StateAcc),
                             handle_plugins(Profile, Plugins1, StateAcc)
                         end, State1, Profiles),

    %% Reset the overrides after processing the dep
    rebar_state:set(State2, overrides, StateOverrides).

filter_existing_plugins(Plugins, State) ->
    PluginNames = lists:zip(Plugins, rebar_state:deps_names(Plugins)),
    AllPlugins = rebar_state:all_plugin_deps(State),
    lists:filtermap(fun({Plugin, PluginName}) ->
                            case rebar_app_utils:find(PluginName, AllPlugins) of
                                {ok, _} ->
                                    false;
                                _ ->
                                    {true, Plugin}
                            end
                    end, PluginNames).

handle_plugins(Profile, Plugins, State) ->
    handle_plugins(Profile, Plugins, State, false).

handle_plugins(Profile, Plugins, State, Upgrade) ->
    %% Set deps dir to plugins dir so apps are installed there
    Locks = rebar_state:lock(State),
    DepsDir = rebar_state:get(State, deps_dir, ?DEFAULT_DEPS_DIR),
    State1 = rebar_state:set(State, deps_dir, ?DEFAULT_PLUGINS_DIR),
    SrcPlugins = discover_plugins(Plugins, State),
    %% Install each plugin individually so if one fails to install it doesn't effect the others
    {_PluginProviders, State2} =
        lists:foldl(fun(Plugin, {PluginAcc, StateAcc}) ->
                            {NewPlugins, NewState} = handle_plugin(Profile, Plugin, StateAcc, SrcPlugins, Upgrade),
                            NewState1 = rebar_state:create_logic_providers(NewPlugins, NewState),
                            {PluginAcc++NewPlugins, NewState1}
                      end, {[], State1}, Plugins),

    %% reset deps dir
    State3 = rebar_state:set(State2, deps_dir, DepsDir),
    rebar_state:lock(State3, Locks).

handle_plugin(Profile, Plugin, State, SrcPlugins, Upgrade) ->
    try
        %% Inject top-level src plugins as project apps, so that they get skipped
        %% by the installation as already seen
        ProjectApps = rebar_state:project_apps(State),
        State0 = rebar_state:project_apps(State, SrcPlugins),
        %% We however have to pick the deps of top-level apps and promote them
        %% directly to make sure they are installed if they were not also at the top level
        TopDeps = top_level_deps(SrcPlugins),
        %% Install the plugins
        {Apps, State1} = rebar_prv_install_deps:handle_deps_as_profile(Profile, State0, [Plugin|TopDeps], Upgrade),
        {no_cycle, Sorted} = rebar_prv_install_deps:find_cycles(SrcPlugins++Apps),
        ToBuild = rebar_prv_install_deps:cull_compile(Sorted, []),
        %% Return things to normal
        State2 = rebar_state:project_apps(State1, ProjectApps),

        %% Add already built plugin deps to the code path
        ToBuildPaths = [rebar_app_info:ebin_dir(A) || A <- ToBuild],
        PreBuiltPaths = [Ebin || A <- Sorted,
                                 Ebin <- [rebar_app_info:ebin_dir(A)],
                                 not lists:member(Ebin, ToBuildPaths)],
        code:add_pathsa(PreBuiltPaths),

        %% Build plugin and its deps
        build_plugins(ToBuild, Sorted, State2),

        %% Add newly built deps and plugin to code path
        State3 = rebar_state:update_all_plugin_deps(State2, Sorted),
        NewCodePaths = [rebar_app_info:ebin_dir(A) || A <- ToBuild],

        %% Store plugin code paths so we can remove them when compiling project apps
        State4 = rebar_state:update_code_paths(State3, all_plugin_deps, PreBuiltPaths++NewCodePaths),
        rebar_paths:set_paths([plugins], State4),

        {plugin_providers(Plugin), State4}
    catch
        ?WITH_STACKTRACE(C,T,S)
            ?DEBUG("~p ~p ~p", [C, T, S]),
            ?WARN("Errors loading plugin ~p. Run rebar3 with DEBUG=1 set to see errors.", [Plugin]),
            {[], State}
    end.

build_plugins(MustBuildApps, AllApps, State) ->
    State1 = rebar_state:deps_to_build(State, MustBuildApps),
    State2 = rebar_state:all_deps(State1, AllApps),
    State3 = rebar_state:set(State2, deps_dir, ?DEFAULT_PLUGINS_DIR),
    {Args, Extra} = rebar_state:command_parsed_args(State),
    State4 = rebar_state:command_parsed_args(State3, {[{deps_only, true}|Args], Extra}),
    rebar_prv_compile:do(State4),
    ok.

plugin_providers({Plugin, _, _, _}) when is_atom(Plugin) ->
    validate_plugin(Plugin);
plugin_providers({Plugin, _, _}) when is_atom(Plugin) ->
    validate_plugin(Plugin);
plugin_providers({Plugin, _}) when is_atom(Plugin) ->
    validate_plugin(Plugin);
plugin_providers(Plugin) when is_atom(Plugin) ->
    validate_plugin(Plugin).

validate_plugin(Plugin) ->
    _ = application:load(Plugin),
    case application:get_env(Plugin, providers) of
        {ok, Providers} ->
            Providers;
        undefined ->
            Exports = Plugin:module_info(exports),
            case lists:member({init,1}, Exports) of
                false ->
                    ?WARN("Plugin ~p does not export init/1. It will not be used.", [Plugin]),
                    [];
                true ->
                    [Plugin]
            end
    end.

discover_plugins([], _) ->
    %% don't search if nothing is declared
    [];
discover_plugins(_, State) ->
    %% TODO: only support this mode in an umbrella project to avoid cases where
    %% this is used in a project intended to be an installed dependency and accidentally
    %% relies on vendoring when not intended.
    case lists:member(global, rebar_state:current_profiles(State)) of
        true ->
            [];
        false ->
            %% Inject source paths for plugins to allow vendoring and umbrella
            %% top-level declarations
            BaseDir = rebar_state:dir(State),
            LibDirs = rebar_dir:project_plugin_dirs(State),
            ?DIAGNOSTIC("Discovering local plugins in {project_plugin_dirs, ~p}", [LibDirs]),
            Dirs = [filename:join(BaseDir, LibDir) || LibDir <- LibDirs],
            RebarOpts = rebar_state:opts(State),
            SrcDirs = rebar_dir:src_dirs(RebarOpts, ["src"]),
            Found = rebar_app_discover:find_apps(Dirs, SrcDirs, all, State),
            ?DIAGNOSTIC("    Found: ~p", [[rebar_app_info:name(F) || F <- Found]]),
            PluginsDir = rebar_dir:plugins_dir(State),
            SetUp = lists:map(fun(App) ->
                Name = rebar_app_info:name(App),
                OutDir = filename:join(PluginsDir, Name),
                prepare_plugin(rebar_app_info:out_dir(App, OutDir))
            end, Found),
            rebar_utils:sort_deps(SetUp)
    end.

prepare_plugin(AppInfo) ->
    %% We need to handle plugins as dependencies to avoid re-building them
    %% continuously. So here we copy the app directories to the dep location
    %% and then change the AppInfo record to be redirected to the dep location.
    AppDir = rebar_app_info:dir(AppInfo),
    OutDir = rebar_app_info:out_dir(AppInfo),
    rebar_prv_compile:copy_app_dirs(AppInfo, AppDir, OutDir),
    Relocated = rebar_app_info:dir(AppInfo, OutDir),
    %% Force a revalidation from the new paths
    rebar_app_info:valid(Relocated, undefined).

top_level_deps(Apps) ->
    RawDeps = lists:foldl(fun(App, Acc) ->
        %% Only support the profiles we would with regular plugins?
        lists:append([rebar_app_info:get(App, Key, [])
                      || Key <- [{plugins, default},
                                 {plugins, prod},
                                 {deps, default},
                                 {deps, prod}]]) ++ Acc
    end, [], Apps),
    rebar_utils:tup_dedup(RawDeps).
