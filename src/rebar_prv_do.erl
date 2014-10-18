%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

-module(rebar_prv_do).

-behaviour(provider).

-export([init/1,
         do/1]).

-include("rebar.hrl").

-define(PROVIDER, do).
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
                                                               {example, "rebar3 do <task1>, <task2>, ..."},
                                                               {short_desc, "Higher order provider for running multiple tasks in a sequence."},
                                                               {desc, ""},
                                                               {opts, []}])),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Tasks = args_to_tasks(rebar_state:command_args(State)),
    lists:foldl(fun(TaskArgs, {ok, StateAcc}) ->
                                 [TaskStr | Args] = string:tokens(TaskArgs, " "),
                                 Task = list_to_atom(TaskStr),
                                 StateAcc1 = rebar_state:set(StateAcc, task, Task),
                                 StateAcc2 = rebar_state:command_args(StateAcc1, Args),
                                 rebar_core:process_command(StateAcc2, Task)
                       end, {ok, State}, Tasks).

args_to_tasks(Args) ->
    [string:strip(T) || T <- string:tokens(string:join(Args, " "), ",")].
