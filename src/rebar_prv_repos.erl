%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

-module(rebar_prv_repos).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-include("rebar.hrl").

-define(PROVIDER, repos).
-define(DEPS, []).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create(
        [{name, ?PROVIDER},
         {module, ?MODULE},
         {bare, false},
         {deps, ?DEPS},
         {example, "rebar3 repos"},
         {short_desc, "Print current package repository configuration"},
         {desc, "Display repository configuration for debugging purpose"},
         {opts, []}]),
    State1 = rebar_state:add_provider(State, Provider),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Resources = rebar_state:resources(State),
    #{repos := Repos} = rebar_resource_v2:find_resource_state(pkg, Resources),

    ?CONSOLE("Repos:", []),
    %%TODO: do some formatting
    ?CONSOLE("~p", [Repos]),
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
