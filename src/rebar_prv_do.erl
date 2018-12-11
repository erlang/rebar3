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
                                                               {bare, true},
                                                               {deps, ?DEPS},
                                                               {example, "rebar3 do <task1>, <task2>, ..."},
                                                               {short_desc, "Higher order provider for running multiple tasks in a sequence."},
                                                               {desc, "Higher order provider for running multiple tasks in a sequence."},
                                                               {opts, []}])),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    case rebar_utils:args_to_tasks(rebar_state:command_args(State)) of
        [] ->
            AllProviders = rebar_state:providers(State),
            Namespace = rebar_state:namespace(State),
            Providers = providers:get_providers_by_namespace(Namespace, AllProviders),
            providers:help(Providers),
            {ok, State};
        Tasks ->
            do_tasks(Tasks, State)
    end.

-spec do_tasks(list(Task), State) -> Res when
      Task :: {string(), string()} |
              {string(), atom()} |
              {atom(), atom(), string()},
      State :: rebar_state:t(),
      Res :: {ok, rebar_state:t()} |
             {error, term()}.
do_tasks([], State) ->
    {ok, State};
do_tasks([{TaskStr, Args} | Tail], State) when is_list(Args) ->
    Task = list_to_atom(TaskStr),
    State1 = rebar_state:set(State, task, Task),
    State2 = rebar_state:command_args(State1, Args),
    Namespace = rebar_state:namespace(State2),
    do_task(TaskStr, Args, Tail, State, Namespace);
do_tasks([{Namespace, Task} | Tail], State) ->
    do_task(atom_to_list(Task), [], Tail, State, Namespace);
do_tasks([{Namespace, Task, Args} | Tail], State)
  when is_atom(Namespace), is_atom(Task) ->
    do_task(atom_to_list(Task), Args, Tail, State, Namespace).

do_task(TaskStr, Args, Tail, State,  Namespace) ->
    Task = list_to_atom(TaskStr),
    State1 = rebar_state:set(State, task, Task),
    State2 = rebar_state:command_args(State1, Args),
    case Namespace of
        default ->
            %% The first task we hit might be a namespace!
            case maybe_namespace(State2, Task, Args) of
                {ok, FinalState} when Tail =:= [] ->
                    {ok, FinalState};
                {ok, _} ->
                    do_tasks(Tail, State);
                {error, Reason} ->
                    {error, Reason}
            end;
        _ ->
            %% We're already in a non-default namespace, check the
            %% task directly.
            State3 = rebar_state:namespace(State2, Namespace),
            case rebar_core:process_command(State3, Task) of
                {ok, FinalState} when Tail =:= [] ->
                    {ok, FinalState};
                {ok, _} ->
                    do_tasks(Tail, State);
                {error, Reason} ->
                    {error, Reason}
            end
    end.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

maybe_namespace(State, Task, Args) ->
    case rebar_core:process_namespace(State, Task) of
        {ok, State2, Task} ->
            %% The task exists after all.
            rebar_core:process_command(State2, Task);
        {ok, State2, do} when Args =/= [] ->
            %% We are in 'do' so we can't apply it directly.
            [NewTaskStr | NewArgs] = Args,
            NewTask = list_to_atom(NewTaskStr),
            State3 = rebar_state:command_args(State2, NewArgs),
            rebar_core:process_command(State3, NewTask);
        {ok, _, _} ->
            %% No arguments to consider as a command. Woops.
            {error, io_lib:format("Command ~p not found", [Task])};
        {error, Reason} ->
            {error, Reason}
    end.
