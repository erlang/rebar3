%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

-module(rebar_plugins).

-export([install/1]).

-include("rebar.hrl").

%% ===================================================================
%% Public API
%% ===================================================================

install(State) ->
    State1 = rebar_state:set(State, deps_dir, ?DEFAULT_PLUGINS_DIR),

    Plugins = rebar_state:get(State1, plugins, []),
    {ok, State2} = rebar_prv_install_deps:handle_deps(State1, Plugins),

    Apps = rebar_state:get(State2, all_deps),
    ToBuild = lists:dropwhile(fun rebar_app_info:valid/1, Apps),
    lists:foreach(fun(AppInfo) ->
                          C = rebar_config:consult(rebar_app_info:dir(AppInfo)),
                          S = rebar_state:new(rebar_state:new(), C, rebar_app_info:dir(AppInfo)),
                          rebar_prv_compile:build(S, AppInfo)
                  end, ToBuild),

    PluginProviders = plugin_providers(Plugins),
    {ok, PluginProviders, rebar_state:set(State2, deps_dir, ?DEFAULT_DEPS_DIR)}.

plugin_providers(Plugins) ->
    lists:map(fun({Plugin, _, _}) when is_atom(Plugin) ->
                      Plugin;
                 ({Plugin, _}) when is_atom(Plugin) ->
                      Plugin;
                 (Plugin) when is_atom(Plugin) ->
                      Plugin
              end, Plugins).
