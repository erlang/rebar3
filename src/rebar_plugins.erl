%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

-module(rebar_plugins).

-export([project_apps_install/1
        ,install/1
        ,handle_plugins/3
        ,handle_plugins/4]).

-include("rebar.hrl").

%% ===================================================================
%% Public API
%% ===================================================================

-spec project_apps_install(rebar_state:t()) -> rebar_state:t().
project_apps_install(State) ->
    Profiles = rebar_state:current_profiles(State),
    ProjectApps = rebar_state:project_apps(State),

    lists:foldl(fun(Profile, StateAcc) ->
                        Plugins = rebar_state:get(State, {plugins, Profile}, []),
                        StateAcc1 = handle_plugins(Profile, Plugins, StateAcc),

                        lists:foldl(fun(App, StateAcc2) ->
                                            AppDir = rebar_app_info:dir(App),
                                            C = rebar_config:consult(AppDir),
                                            S = rebar_state:new(rebar_state:new(), C, AppDir),
                                            Plugins2 = rebar_state:get(S, {plugins, Profile}, []),
                                            handle_plugins(Profile, Plugins2, StateAcc2)
                                    end, StateAcc1, ProjectApps)
                end, State, Profiles).

-spec install(rebar_state:t()) -> rebar_state:t().
install(State) ->
    Profiles = rebar_state:current_profiles(State),
    lists:foldl(fun(Profile, StateAcc) ->
                        Plugins = rebar_state:get(State, {plugins, Profile}, []),
                        handle_plugins(Profile, Plugins, StateAcc)
                end, State, Profiles).

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
        CodePaths = [rebar_app_info:ebin_dir(A) || A <- Apps -- ToBuild],
        code:add_pathsa(CodePaths),

        %% Build plugin and its deps
        [build_plugin(AppInfo, Apps, State2) || AppInfo <- ToBuild],

        %% Add newly built deps and plugin to code path
        State3 = rebar_state:update_all_plugin_deps(State2, Apps),
        NewCodePaths = [rebar_app_info:ebin_dir(A) || A <- ToBuild],
        code:add_pathsa(CodePaths),

        %% Store plugin code paths so we can remove them when compiling project apps
        State4 = rebar_state:update_code_paths(State3, all_plugin_deps, CodePaths++NewCodePaths),

        {plugin_providers(Plugin), State4}
    catch
        C:T ->
            ?DEBUG("~p ~p ~p", [C, T, erlang:get_stacktrace()]),
            ?WARN("Plugin ~p not available. It will not be used.", [Plugin]),
            {[], State}
    end.

build_plugin(AppInfo, Apps, State) ->
    Providers = rebar_state:providers(State),
    Providers1 = rebar_state:providers(rebar_app_info:state(AppInfo)),
    S = rebar_state:all_deps(rebar_app_info:state_or_new(State, AppInfo), Apps),
    S1 = rebar_state:set(S, deps_dir, ?DEFAULT_PLUGINS_DIR),
    rebar_prv_compile:compile(S1, Providers++Providers1, AppInfo).

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
