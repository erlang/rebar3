%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

-module(rebar_plugins).

-export([install/1, handle_plugins/2, handle_plugins/3]).

-include("rebar.hrl").

%% ===================================================================
%% Public API
%% ===================================================================

-spec install(rebar_state:t()) -> rebar_state:t().
install(State) ->
    Plugins = rebar_state:get(State, plugins, []),

    ProjectApps = rebar_state:project_apps(State),

    OtherPlugins = lists:flatmap(fun(App) ->
                                         AppDir = rebar_app_info:dir(App),
                                         C = rebar_config:consult(AppDir),
                                         S = rebar_state:new(rebar_state:new(), C, AppDir),
                                         rebar_state:get(S, plugins, [])
                                 end, ProjectApps),

    handle_plugins(Plugins++OtherPlugins, State).

-spec handle_plugins([rebar_prv_install_deps:dep()], rebar_state:t()) -> rebar_state:t().
handle_plugins(Plugins, State) ->
    handle_plugins(default, Plugins, State).

handle_plugins(Profile, Plugins, State) ->
    %% Set deps dir to plugins dir so apps are installed there
    Locks = rebar_state:lock(State),
    DepsDir = rebar_state:get(State, deps_dir, ?DEFAULT_DEPS_DIR),
    State1 = rebar_state:set(State, deps_dir, ?DEFAULT_PLUGINS_DIR),

    {PluginProviders, State2} =
        lists:foldl(fun(Plugin, {PluginAcc, StateAcc}) ->
                            {NewPlugins, NewState} = handle_plugin(Profile, Plugin, StateAcc),
                            {PluginAcc++NewPlugins, NewState}
                      end, {[], State1}, Plugins),

    %% reset deps dir
    State3 = rebar_state:set(State2, deps_dir, DepsDir),
    State4 = rebar_state:lock(State3, Locks),

    rebar_state:create_logic_providers(PluginProviders, State4).

handle_plugin(Profile, Plugin, State) ->
    try
        {ok, Apps, State2} = rebar_prv_install_deps:handle_deps(Profile, State, [Plugin]),
        {no_cycle, Sorted} = rebar_prv_install_deps:find_cycles(Apps),
        ToBuild = rebar_prv_install_deps:cull_compile(Sorted, []),

        %% Add already built plugin deps to the code path
        CodePaths = [rebar_app_info:ebin_dir(A) || A <- Apps -- ToBuild],
        code:add_pathsa(CodePaths),

        %% Build plugin and its deps
        [build_plugin(AppInfo) || AppInfo <- ToBuild],

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

build_plugin(AppInfo) ->
    AppDir = rebar_app_info:dir(AppInfo),
    C = rebar_config:consult(AppDir),
    S = rebar_state:new(rebar_state:new(), C, AppDir),
    rebar_prv_compile:compile(S, [], AppInfo).

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
