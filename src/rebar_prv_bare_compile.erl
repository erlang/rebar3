-module(rebar_prv_bare_compile).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-include_lib("providers/include/providers.hrl").
-include("rebar.hrl").

-define(PROVIDER, compile).
-define(NAMESPACE, bare).
-define(DEPS, [{default, app_discovery}]).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    State1 = rebar_state:add_provider(State, providers:create([{name, ?PROVIDER},
                                                               {module, ?MODULE},
                                                               {namespace, ?NAMESPACE},
                                                               {bare, false},
                                                               {deps, ?DEPS},
                                                               {example, ""},
                                                               {short_desc, ""},
                                                               {desc, ""},
                                                               {opts, []}])),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    [AppInfo] = rebar_state:project_apps(State),
    AppInfo1 = rebar_app_info:out_dir(AppInfo, rebar_dir:get_cwd()),
    rebar_prv_compile:compile(State, AppInfo1),
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
