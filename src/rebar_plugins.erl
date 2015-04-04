%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

-module(rebar_plugins).

-export([install/1]).

-include("rebar.hrl").

%% ===================================================================
%% Public API
%% ===================================================================

install(State) ->
    DepsDir = rebar_dir:deps_dir(State),
    %% Set deps_dir to a different dir for plugin so they don't collide
    OldDepsDir = rebar_state:get(State, deps_dir, ?DEFAULT_DEPS_DIR),
    State1 = rebar_state:set(State, deps_dir, ?DEFAULT_PLUGINS_DIR),
    Plugins = rebar_state:get(State1, plugins, []),

    ProjectApps = rebar_state:project_apps(State),
    DepApps = rebar_app_discover:find_apps([DepsDir], all),

    OtherPlugins = lists:flatmap(fun(App) ->
                                         AppDir = rebar_app_info:dir(App),
                                         C = rebar_config:consult(AppDir),
                                         S = rebar_state:new(rebar_state:new(), C, AppDir),
                                         rebar_state:get(S, plugins, [])
                                 end, ProjectApps++DepApps),

    PluginProviders = lists:flatten(rebar_utils:filtermap(fun(Plugin) ->
                                                                  handle_plugin(Plugin, State1)
                                                          end, Plugins++OtherPlugins)),

    State2 = rebar_state:set(State1, deps_dir, OldDepsDir),

    {ok, PluginProviders, State2}.

-spec handle_plugin(rebar_prv_install_deps:dep(), rebar_state:t()) -> {true, any()} | false.
handle_plugin(Plugin, State) ->
    try
        {ok, _, State1} = rebar_prv_install_deps:handle_deps(default, State, [Plugin]),

        Apps = rebar_state:all_deps(State1),
        ToBuild = lists:dropwhile(fun rebar_app_info:valid/1, Apps),
        [build_plugin(AppInfo) || AppInfo <- ToBuild],
        plugin_providers(Plugin)
    catch
        C:T ->
            ?DEBUG("~p ~p", [C, T]),
            ?WARN("Plugin ~p not available. It will not be used.", [Plugin]),
            false
    end.

build_plugin(AppInfo) ->
    AppDir = rebar_app_info:dir(AppInfo),
    C = rebar_config:consult(AppDir),
    S = rebar_state:new(rebar_state:new(), C, AppDir),
    rebar_prv_compile:compile(S, AppInfo),
    true = code:add_patha(filename:join(AppDir, "ebin")).

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
            {true, Providers};
        undefined ->
            Exports = Plugin:module_info(exports),
            case lists:member({init,1}, Exports) of
                false ->
                    ?WARN("Plugin ~p does not export init/1. It will not be used.", [Plugin]),
                    false;
                true ->
                    {true, Plugin}
            end
    end.
