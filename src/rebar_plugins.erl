%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

-module(rebar_plugins).

-export([install/1]).

-include("rebar.hrl").

%% ===================================================================
%% Public API
%% ===================================================================

install(State) ->
    %% Set deps_dir to a different dir for plugin so they don't collide
    DepsDir = rebar_state:get(State, deps_dir, ?DEFAULT_DEPS_DIR),
    State1 = rebar_state:set(State, deps_dir, ?DEFAULT_PLUGINS_DIR),
    expand_plugins(?DEFAULT_PLUGINS_DIR),
    Plugins = rebar_state:get(State1, plugins, []),
    PluginProviders = rebar_utils:filtermap(fun(Plugin) ->
                                                    handle_plugin(Plugin, State1)
                                            end, Plugins),

    {ok, PluginProviders, rebar_state:set(State1, deps_dir, DepsDir)}.

handle_plugin(Plugin, State) ->
    try
        {ok, State1} = rebar_prv_install_deps:handle_deps(State, [Plugin]),
        Apps = rebar_state:get(State1, all_deps, []),
        ToBuild = lists:dropwhile(fun rebar_app_info:valid/1, Apps),
        lists:foreach(fun(AppInfo) ->
                              C = rebar_config:consult(rebar_app_info:dir(AppInfo)),
                              S = rebar_state:new(rebar_state:new(), C, rebar_app_info:dir(AppInfo)),
                              rebar_prv_compile:build(S, AppInfo)
                      end, ToBuild),
        expand_plugins(?DEFAULT_PLUGINS_DIR),
        plugin_providers(Plugin)
    catch
        C:T ->
            ?DEBUG("~p ~p", [C, T]),
            ?WARN("Plugin ~p not available. It will not be used.~n", [Plugin]),
            false
    end.

plugin_providers({Plugin, _, _}) when is_atom(Plugin) ->
    validate_plugin(Plugin);
plugin_providers({Plugin, _}) when is_atom(Plugin) ->
    validate_plugin(Plugin);
plugin_providers(Plugin) when is_atom(Plugin) ->
    validate_plugin(Plugin).

validate_plugin(Plugin) ->
    Exports = sets:from_list(Plugin:module_info(exports)),
    Required = sets:from_list([{init,1},
                               {do,1},
                               {format_error,2}]),
    case sets:is_subset(Required,  Exports) of
        false ->
            ?WARN("Plugin ~p is not a provider. It will not be used.~n", [Plugin]),
            false;
        true ->
            {true, Plugin}
    end.

expand_plugins(Dir) ->
    Apps = filelib:wildcard(filename:join([Dir, "*", "ebin"])),
    ok = code:add_pathsa(Apps).
