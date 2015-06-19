-module(rebar_prv_plugins).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

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
    GlobalConfigFile = rebar_dir:global_config(),
    GlobalConfig = rebar_state:new(rebar_config:consult_file(GlobalConfigFile)),
    GlobalPlugins = rebar_state:get(GlobalConfig, plugins, []),
    GlobalPluginsDir = filename:join(rebar_dir:global_cache_dir(State), "plugins"),
    display_plugins("Global plugins", GlobalPluginsDir, GlobalPlugins),

    Plugins = rebar_state:get(State, plugins, []),
    PluginsDir =rebar_dir:plugins_dir(State),
    display_plugins("Local plugins", PluginsDir, Plugins),
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

display_plugins(_Header, _Dir, []) ->
    ok;
display_plugins(Header, Dir, Plugins) ->
    ?CONSOLE("--- ~s ---", [Header]),
    display_plugins(Dir, Plugins),
    ?CONSOLE("", []).

display_plugins(Dir, Plugins) ->
    lists:foreach(fun(Plugin) ->
                          Name = if is_atom(Plugin) -> Plugin;
                                    is_tuple(Plugin) -> element(1, Plugin)
                                 end,
                          case rebar_app_info:discover(filename:join(Dir, Name)) of
                              {ok, _App} ->
                                  ?CONSOLE("~s", [Name]);
                              not_found ->
                                  ?DEBUG("Unable to find plugin ~s", [Name])
                          end
                  end, Plugins).
