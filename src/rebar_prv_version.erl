%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

-module(rebar_prv_version).

-behaviour(provider).

-export([init/1,
         do/1]).

-include("rebar.hrl").

-define(PROVIDER, version).
-define(DEPS, []).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    State1 = rebar_state:add_provider(State, providers:create([{name, ?PROVIDER},
                                                               {module, ?MODULE},
                                                               {bare, false},
                                                               {deps, ?DEPS},
                                                               {example, "rebar version"},
                                                               {short_desc, "Print version for rebar and current Erlang."},
                                                               {desc, ""},
                                                               {opts, []}])),

    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    io:format("REST ~p~n", [rebar_state:command_args(State)]),
    rebar3:version(),
    {ok, State}.
