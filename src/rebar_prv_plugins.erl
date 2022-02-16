-module(rebar_prv_plugins).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1,
         list_local_plugins/1]).

-include("rebar.hrl").
-include_lib("providers/include/providers.hrl").

-define(PROVIDER, list).
-define(NAMESPACE, plugins).
-define(DEPS, []).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    State1 = rebar_state:add_provider(
            State,
            providers:create([
                    {name, ?PROVIDER},
                    {module, ?MODULE},
                    {namespace, ?NAMESPACE},
                    {bare, true},
                    {deps, ?DEPS},
                    {example, "rebar3 plugins list"},
                    {short_desc, "List local and global plugins for this project"},
                    {desc, "List local and global plugins for this project"},
                    {opts, []}])),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    GlobalConfigFile = rebar_dir:global_config(State),
    GlobalConfig = rebar_state:new(rebar_config:consult_file(GlobalConfigFile)),
    GlobalPlugins = rebar_state:get(GlobalConfig, plugins, []),
    GlobalSrcDirs = rebar_state:get(GlobalConfig, src_dirs, ["src"]),
    GlobalPluginsDir = filename:join([rebar_dir:global_cache_dir(rebar_state:opts(State)), "plugins", "*"]),
    GlobalApps = rebar_app_discover:find_apps([GlobalPluginsDir], GlobalSrcDirs, all, State),
    display_plugins("Global plugins", GlobalApps, GlobalPlugins),

    RebarOpts = rebar_state:opts(State),
    SrcDirs = rebar_dir:src_dirs(RebarOpts, ["src"]),
    {LocalPluginsDefs, _} = list_local_plugins(State),
    PluginsDirs = filelib:wildcard(filename:join(rebar_dir:plugins_dir(State), "*")),

    %% use `checkouts_dir' and not `checkouts_out_dir'. Since we use `all' in `find_apps'
    %% so it doesn't need to be built and the apps in `checkouts_dir' could be old
    %% because the user removing from `_checkouts/' doesn't cause removal of the output
    CheckoutsDirs = filelib:wildcard(filename:join(rebar_dir:checkouts_dir(State), "*")),
    Apps = rebar_app_discover:find_apps(CheckoutsDirs++PluginsDirs, SrcDirs, all, State),
    display_plugins("Local plugins", Apps, LocalPluginsDefs),
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

list_local_plugins(State) ->
    LocalPluginsDefs = rebar_state:get(State, plugins, [])
                       ++ rebar_state:get(State, project_plugins, []),
    LocalPluginsNames = lists:map(
                            fun (LocalPluginDef) ->
                                rebar_utils:to_atom(
                                    if is_tuple(LocalPluginDef) -> element(1, LocalPluginDef);
                                       true -> LocalPluginDef
                                    end
                                )
                            end,
                            LocalPluginsDefs),
    {LocalPluginsDefs, LocalPluginsNames}.

display_plugins(_Header, _Apps, []) ->
    ok;
display_plugins(Header, Apps, Plugins) ->
    ?CONSOLE("--- ~ts ---", [Header]),
    display_plugins(Apps, Plugins),
    ?CONSOLE("", []).

display_plugins(Apps, Plugins) ->
    lists:foreach(fun(Plugin) ->
                          Name = if is_atom(Plugin) -> atom_to_binary(Plugin, unicode);
                                    is_tuple(Plugin) -> rebar_utils:to_binary(element(1, Plugin))
                                 end,
                          case rebar_app_utils:find(Name, Apps) of
                              {ok, App} ->
                                  ?CONSOLE("~ts (~s)", [Name, rebar_app_info:original_vsn(App)]);
                              error ->
                                  ?DEBUG("Unable to find plugin ~ts", [Name])
                          end
                  end, Plugins).
