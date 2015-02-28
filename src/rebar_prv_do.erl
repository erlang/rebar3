%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

-module(rebar_prv_do).

-behaviour(provider).

-export([init/1,
         do/1,
         do_tasks/2,
         format_error/1]).

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
    Tasks = rebar_utils:args_to_tasks(rebar_state:command_args(State)),
    do_tasks(Tasks, State).

do_tasks([], State) ->
    {ok, State};
do_tasks([{TaskStr, Args}|Tail], State) ->
    Task = list_to_atom(TaskStr),
    State1 = rebar_state:set(State, task, Task),
    State2 = rebar_state:command_args(State1, Args),
    case rebar_core:process_command(State2, Task) of
        {ok, _} ->
            do_tasks(Tail, State);
        Error ->
            Error
    end.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
