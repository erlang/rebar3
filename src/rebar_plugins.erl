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
    %% Install each plugin individually so if one fails to install it doesn't effect the others
    {_PluginProviders, State2} =
        lists:foldl(fun(Plugin, {PluginAcc, StateAcc}) ->
                            {NewPlugins, NewState} = handle_plugin(Profile, Plugin, StateAcc, Upgrade),
                            NewState1 = rebar_state:create_logic_providers(NewPlugins, NewState),
                            {PluginAcc++NewPlugins, NewState1}
                      end, {[], State1}, Plugins),

    %% reset deps dir
    State3 = rebar_state:set(State2, deps_dir, DepsDir),
    rebar_state:lock(State3, Locks).

handle_plugin(Profile, Plugin, State, Upgrade) ->
    try
        {Apps, State2} = rebar_prv_install_deps:handle_deps_as_profile(Profile, State, [Plugin], Upgrade),
        {no_cycle, Sorted} = rebar_prv_install_deps:find_cycles(Apps),
        ToBuild = rebar_prv_install_deps:cull_compile(Sorted, []),

        %% Add already built plugin deps to the code path
        PreBuiltPaths = [rebar_app_info:ebin_dir(A) || A <- Apps] -- ToBuild,
        code:add_pathsa(PreBuiltPaths),

        %% Build plugin and its deps
        [build_plugin(AppInfo, Apps, State2) || AppInfo <- ToBuild],

        %% Add newly built deps and plugin to code path
        State3 = rebar_state:update_all_plugin_deps(State2, Apps),
        NewCodePaths = [rebar_app_info:ebin_dir(A) || A <- ToBuild],

        %% Store plugin code paths so we can remove them when compiling project apps
        State4 = rebar_state:update_code_paths(State3, all_plugin_deps, PreBuiltPaths++NewCodePaths),
        rebar_paths:set_paths([plugins], State4),

        {plugin_providers(Plugin), State4}
    catch
        ?WITH_STACKTRACE(C,T,S)
            ?DEBUG("~p ~p ~p", [C, T, S]),
            ?WARN("Plugin ~p not available. It will not be used.", [Plugin]),
            {[], State}
    end.

build_plugin(AppInfo, Apps, State) ->
    Providers = rebar_state:providers(State),
    S = rebar_state:all_deps(State, Apps),
    S1 = rebar_state:set(S, deps_dir, ?DEFAULT_PLUGINS_DIR),
    rebar_prv_compile:compile(S1, Providers, AppInfo).

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
