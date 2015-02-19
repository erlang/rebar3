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
    OldDepsDir = rebar_state:get(State, deps_dir, ?DEFAULT_DEPS_DIR),
    State1 = rebar_state:set(State, deps_dir, ?DEFAULT_PLUGINS_DIR),
    DepsDir = rebar_dir:deps_dir(State1),
    expand_plugins(DepsDir),
    Plugins = rebar_state:get(State1, plugins, []),
    PluginProviders = lists:flatten(rebar_utils:filtermap(fun(Plugin) ->
                                                                  handle_plugin(Plugin, State1)
                                                          end, Plugins)),

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
            ?WARN("Plugin ~p not available. It will not be used.~n", [Plugin]),
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
    ok = application:load(Plugin),
    case application:get_env(Plugin, providers) of
        {ok, Providers} ->
            {true, Providers};
        undefined ->
            Exports = sets:from_list(Plugin:module_info(exports)),
            Required = sets:from_list([{init,1},
                                       {do,1},
                                       {format_error,1}]),
            case sets:is_subset(Required,  Exports) of
                false ->
                    ?WARN("Plugin ~p is not a provider. It will not be used.~n", [Plugin]),
                    false;
                true ->
                    {true, Plugin}
            end
    end.

expand_plugins(Dir) ->
    Apps = filelib:wildcard(filename:join([Dir, "*", "ebin"])),
    ok = code:add_pathsa(Apps).
