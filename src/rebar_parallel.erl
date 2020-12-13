%%% @doc
%%% This module contains a small parallel dispatch queue that allows
%%% to take a list of jobs and run as many of them in parallel as there
%%% are schedulers ongoing.
%%%
%%% Original design by Max Fedorov in the rebar compiler, then generalised
%%% and extracted here to be reused in other circumstances.
%%% @end
-module(rebar_parallel).
-export([queue/5]).
-include("rebar.hrl").

queue(Tasks, WorkF, WArgs, Handler, HArgs) ->
    Parent = self(),
    Worker = fun() -> worker(Parent, WorkF, WArgs) end,
    Jobs = min(length(Tasks), erlang:system_info(schedulers)),
    ?DIAGNOSTIC("Starting ~B worker(s)", [Jobs]),
    Pids = [spawn_monitor(Worker) || _ <- lists:seq(1, Jobs)],
    parallel_dispatch(Tasks, Pids, Handler, HArgs).

parallel_dispatch([], [], _, _) ->
    [];
parallel_dispatch(Targets, Pids, Handler, Args) ->
    receive
        {ready, Worker} when is_pid(Worker), Targets =:= [] ->
            Worker ! empty,
            parallel_dispatch(Targets, Pids, Handler, Args);
        {ready, Worker} when is_pid(Worker) ->
            [Task|Tasks] = Targets,
            Worker ! {task, Task},
            parallel_dispatch(Tasks, Pids, Handler, Args);
        {'DOWN', Mref, _, Pid, normal} ->
            NewPids = lists:delete({Pid, Mref}, Pids),
            parallel_dispatch(Targets, NewPids, Handler, Args);
        {'DOWN', _Mref, _, _Pid, Info} ->
            ?ERROR("Task failed: ~p", [Info]),
            ?ABORT;
        {result, Result} ->
            case Handler(Result, Args) of
                ok ->
                    parallel_dispatch(Targets, Pids, Handler, Args);
                {ok, Acc} ->
                    [Acc | parallel_dispatch(Targets, Pids, Handler, Args)]
            end
    end.

worker(QueuePid, F, Args) ->
    QueuePid ! {ready, self()},
    receive
        {task, Task} ->
            QueuePid ! {result, F(Task, Args)},
            worker(QueuePid, F, Args);
        empty ->
            ok
    end.

