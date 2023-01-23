%%% @doc
%%% This module contains a small parallel dispatch queue that allows
%%% to take a list of jobs and run as many of them in parallel as there
%%% are schedulers ongoing.
%%%
%%% Original design by Max Fedorov in the rebar compiler, then generalised
%%% and extracted here to be reused in other circumstances.
%%% @end
-module(rebar_parallel).
-export([queue/5,
         pool/4, pool/5, pool_task/2, pool_terminate/1]).
-include("rebar.hrl").

queue(Tasks, WorkF, WArgs, Handler, HArgs) ->
    Parent = self(),
    Worker = fun() -> worker(Parent, WorkF, WArgs) end,
    Jobs = min(length(Tasks), erlang:system_info(schedulers)),
    ?DIAGNOSTIC("Starting ~B worker(s)", [Jobs]),
    Pids = [spawn_monitor(Worker) || _ <- lists:seq(1, Jobs)],
    parallel_dispatch(Tasks, Pids, Handler, HArgs).

pool(WorkF, WArgs, Handler, HArgs) ->
    pool(WorkF, WArgs, Handler, HArgs, erlang:system_info(schedulers)).

pool(WorkF, WArgs, Handler, HArgs, Jobs) ->
    Parent = self(),
    Coordinator = spawn_link(fun() ->
        Coord = self(),
        Worker = fun() -> worker(Coord, WorkF, WArgs) end,
        Pids = [spawn_monitor(Worker) || _ <- lists:seq(1, Jobs)],
        Parent ! pool_ready,
        pool_coordinator([], [], Pids, Handler, HArgs, [], undefined)
    end),
    receive pool_ready -> ok end,
    Coordinator.

pool_task(Pid, Task) ->
    Pid ! {task, Task},
    ok.

pool_terminate(Pid) ->
    Ref = erlang:monitor(process, Pid),
    Pid ! {self(), terminate},
    receive
        {Pid, Res} ->
            erlang:demonitor(Ref, [flush]),
            Res;
        {'DOWN', Ref, process, Pid, Info} ->
            error(Info)
    end.

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

pool_coordinator(Tasks, FreePids, Pids, Handler, HArgs, Acc, Report) ->
    receive
        {task, Task} when Report =/= undefined ->
            ?ERROR("Task added to pool after being terminated: ~p", [Task]),
            ?ABORT;
        {task, Task} when FreePids =:= [] ->
            pool_coordinator([Task|Tasks], FreePids, Pids, Handler, HArgs, Acc, Report);
        {task, Task} ->
            [Pid|NewFree] = FreePids,
            Pid ! {task, Task},
            pool_coordinator(Tasks, NewFree, Pids, Handler, HArgs, Acc, Report);
        {ready, _Pid} when Tasks =:= [], Report =/= undefined ->
            %% And we're done
            %% terminate workers async, return results if done
            [Pid ! empty || {Pid,_Ref} <- Pids],
            pool_coordinator(Tasks, FreePids, Pids, Handler, HArgs, Acc, Report);
        {ready, Pid} when Tasks =:= [] ->
            pool_coordinator(Tasks, [Pid|FreePids], Pids, Handler, HArgs, Acc, Report);
        {ready, Pid} ->
            [Task|NewTasks] = Tasks,
            Pid ! {task, Task},
            pool_coordinator(NewTasks, FreePids, Pids, Handler, HArgs, Acc, Report);
        {'DOWN', Mref, _, Pid, normal} ->
            NewPids = lists:delete({Pid, Mref}, Pids),
            NewFree = lists:delete(Pid, FreePids),
            case NewPids of
                [] when is_pid(Report) ->
                    Report ! {self(), Acc};
                _ ->
                    pool_coordinator(Tasks, NewFree, NewPids, Handler, HArgs, Acc, Report)
            end;
        {'DOWN', _Mref, _, _Pid, Info} ->
            ?ERROR("Task failed: ~p", [Info]),
            ?ABORT;
        {result, Result} ->
            case Handler(Result, HArgs) of
                ok ->
                    pool_coordinator(Tasks, FreePids, Pids, Handler, HArgs, Acc, Report);
                {ok, Res} ->
                    pool_coordinator(Tasks, FreePids, Pids, Handler, HArgs, [Res|Acc], Report)
            end;
        {Caller, terminate} ->
            if Pids =:= [];                                       % no workers somehow
               length(Pids) =:= length(FreePids), Tasks =:= [] -> % All Idle
                    Caller ! {self(), Acc};
               true ->
                   %% Still work to do
                   pool_coordinator(Tasks, FreePids, Pids, Handler, HArgs, Acc, Caller)
            end
    end.

