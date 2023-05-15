%%% @doc
%%% This module contains a small parallel dispatch queue that allows
%%% to take a list of jobs and run as many of them in parallel as there
%%% are schedulers ongoing.
%%%
%%% Original design by Max Fedorov in the rebar compiler, then generalised
%%% and extracted here to be reused in other circumstances.
%%%
%%% It also contains an asynchronous version of the queue built with a
%%% naive pool, which allows similar semantics in worker definitions, but
%%% without demanding to know all the tasks to run ahead of time.
%%% @end
-module(rebar_parallel).
-export([queue/5,
         pool/4, pool/5, pool_task_async/2, pool_terminate/1]).
-include("rebar.hrl").

%% @doc Create a queue using as many workers as there are schedulers,
%% that will spread all `Task' entries among them based on whichever
%% is available first.
%%
%% The values returned by the worker function `WorkF' for each value
%% is then passed to a `Handler' which either discards its result
%% after having done a side-effectful operation (by returning `ok')
%% as in a `lists:foreach/2' call, or returns a value that gets
%% added to an accumulator (by returning `{ok, Val}'). The handler
%% can return both types as required.
%%
%% The accumulated list of value is in no specific order and depends
%% on how tasks were scheduled and completed.
-spec queue([Task], WorkF, WArgs, Handler, HArgs) -> [Ret] when
      Task :: term(),
      WorkF :: fun((Task, WArgs) -> TmpRet),
      WArgs :: term(),
      Handler :: fun((TmpRet, HArgs) -> NoRet | AccVal),
      HArgs :: term(),
      NoRet :: ok,
      AccVal :: {ok, Ret},
      Ret :: term().
queue(Tasks, WorkF, WArgs, Handler, HArgs) ->
    Parent = self(),
    Worker = fun() -> worker(Parent, WorkF, WArgs) end,
    Jobs = min(length(Tasks), erlang:system_info(schedulers)),
    ?DIAGNOSTIC("Starting ~B worker(s)", [Jobs]),
    Pids = [spawn_monitor(Worker) || _ <- lists:seq(1, Jobs)],
    parallel_dispatch(Tasks, Pids, Handler, HArgs).

%% @doc Create a pool using as many workers as there are schedulers,
%% and for which tasks can be added by calling `pool_async_task/2'.
%%
%% The values returned by the worker function `WorkF' for each value
%% is then passed to a `Handler' which either discards its result
%% after having done a side-effectful operation (by returning `ok')
%% as in a `lists:foreach/2' call, or returns a value that gets
%% added to an accumulator (by returning `{ok, Val}'). The handler
%% can return both types as required.
%%
%% The accumulated list of value is in no specific order and depends
%% on how tasks were scheduled and completed, and can only
%% be obtained by calling `pool_terminate/1'.
%%
%% The pool process is linked to its initial caller and will error
%% out via a link if any task crashes or other invalid states are found
-spec pool(WorkF, WArgs, Handler, HArgs) -> pid() when
      WorkF :: fun((Task, WArgs) -> TmpRet),
      Task :: term(),
      WArgs :: term(),
      Handler :: fun((TmpRet, HArgs) -> NoRet | AccVal),
      HArgs :: term(),
      NoRet :: ok,
      AccVal :: {ok, term()}.
pool(WorkF, WArgs, Handler, HArgs) ->
    pool(WorkF, WArgs, Handler, HArgs, erlang:system_info(schedulers)).

%% @doc Create a pool using `PoolSize' workers and for which tasks can be
%% added by calling `pool_async_task/2'.
%%
%% The values returned by the worker function `WorkF' for each value
%% is then passed to a `Handler' which either discards its result
%% after having done a side-effectful operation (by returning `ok')
%% as in a `lists:foreach/2' call, or returns a value that gets
%% added to an accumulator (by returning `{ok, Val}'). The handler
%% can return both types as required.
%%
%% The accumulated list of value is in no specific order and depends
%% on how tasks were scheduled and completed, and can only
%% be obtained by calling `pool_terminate/1'.
%%
%% The pool process is linked to its initial caller and will error
%% out via a link if any task crashes or other invalid states are found
-spec pool(WorkF, WArgs, Handler, HArgs, PoolSize) -> pid() when
      WorkF :: fun((Task, WArgs) -> TmpRet),
      Task :: term(),
      WArgs :: term(),
      Handler :: fun((TmpRet, HArgs) -> NoRet | AccVal),
      HArgs :: term(),
      PoolSize :: pos_integer(),
      NoRet :: ok,
      AccVal :: {ok, term()}.
pool(WorkF, WArgs, Handler, HArgs, PoolSize) when PoolSize > 0 ->
    Parent = self(),
    Coordinator = spawn_link(fun() ->
        Coord = self(),
        Worker = fun() -> worker(Coord, WorkF, WArgs) end,
        Pids = [spawn_monitor(Worker) || _ <- lists:seq(1, PoolSize)],
        Parent ! pool_ready,
        pool_coordinator([], [], Pids, Handler, HArgs, [], undefined)
    end),
    receive pool_ready -> ok end,
    Coordinator.

%% @doc Add a task to a pool.
%% This call is asynchronous and does no validation about whether the pool
%% process exists or not. If the pool has already been terminated or is
%% in the process of being terminated, the task may trigger the pool to
%% abort and error out to point out invalid usage.
-spec pool_task_async(pid(), term()) -> ok.
pool_task_async(Pid, Task) ->
    Pid ! {task, Task},
    ok.

%% @doc Mark the pool as terminated. At this point it will stop
%% accepting new tasks but will keep processing those that have been
%% scheduled.
%%
%% Once all tasks are complete and workers have shut down, the
%% accumulated value will be returned.
%%
%% Any process may terminate the pool, and the pool may only be
%% terminated once.
-spec pool_terminate(pid()) -> [term()].
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

%%%%%%%%%%%%%%%%
%%% INTERNAL %%%
%%%%%%%%%%%%%%%%

%%% Queue implementation %%%
%% @private the queue is rather straightforward. `Targets' represents the tasks
%% yet to be run, which are sent to workers in `Pids' as they mark themselves
%% as free. When workers are empty but no tasks are left, they are shutdown.
%% Once the shutdown is complete, the result is returned.
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

%%% Pool implementation %%%
%% @private The pool supports asynchronous tasks addition, which makes it
%% significantly hairier than the task queue. It uses `Tasks' to track
%% enqueued tasks, `FreePids' to track the workers that currently do not
%% have work to do, `Pids' to track all workers (and know what remains to
%% be shut down at the end), an accumulator (`Acc') for results that must
%% be returned on-demand, and a `Report' value that is either `undefined'
%% or marks the Pid of the process calling for a report.
%%
%% Tasks and free processes can grow individually at different times, and
%% we only demand that workers start shutting down once a `Report' entry
%% has been defined, which returns once tasks that were in flight have all
%% terminated.
pool_coordinator(Tasks, FreePids, Pids, Handler, HArgs, Acc, Report) ->
    receive
        {task, Task} when Report =/= undefined ->
            ?ERROR("Task added to pool after being terminated: ~p", [Task]),
            ?ABORT;
        {task, Task} when FreePids =:= [] ->
            %% no worker is free, enqueue.
            pool_coordinator([Task|Tasks], FreePids, Pids, Handler, HArgs, Acc, Report);
        {task, Task} ->
            %% workers are free, assign right away
            [Pid|NewFree] = FreePids,
            Pid ! {task, Task},
            pool_coordinator(Tasks, NewFree, Pids, Handler, HArgs, Acc, Report);
        {ready, _Pid} when Tasks =:= [], Report =/= undefined ->
            %% And we're done!
            %% terminate workers async, and wait for their shutdown
            [Pid ! empty || {Pid,_Ref} <- Pids],
            pool_coordinator(Tasks, FreePids, Pids, Handler, HArgs, Acc, Report);
        {ready, Pid} when Tasks =:= [] ->
            %% worker free, no tasks to run, put in the free list
            pool_coordinator(Tasks, [Pid|FreePids], Pids, Handler, HArgs, Acc, Report);
        {ready, Pid} ->
            %% worker free, tasks are available, assign right away
            [Task|NewTasks] = Tasks,
            Pid ! {task, Task},
            pool_coordinator(NewTasks, FreePids, Pids, Handler, HArgs, Acc, Report);
        {'DOWN', Mref, _, Pid, normal} ->
            %% worker terminated as expected
            NewPids = lists:delete({Pid, Mref}, Pids),
            NewFree = lists:delete(Pid, FreePids),
            case NewPids of
                [] when is_pid(Report) ->
                    %% shutdown complete!
                    Report ! {self(), Acc};
                _ ->
                    %% still shutting down
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
        {Caller, terminate} when Report =:= undefined ->
            %% We're being asked to return results!
            if Pids =:= [];                                       % no workers somehow
               length(Pids) =:= length(FreePids), Tasks =:= [] -> % All Idle, no work to do
                    Caller ! {self(), Acc};
               true ->
                   %% Still work to do
                   pool_coordinator(Tasks, FreePids, Pids, Handler, HArgs, Acc, Caller)
            end;
        {Caller, terminate} when is_pid(Report) ->
            ?ERROR("Another process (~p) already terminates the pool, demand from ~p is invalid",
                   [Report, Caller]),
            ?ABORT
    end.

%%% Shared components %%%

worker(QueuePid, F, Args) ->
    QueuePid ! {ready, self()},
    receive
        {task, Task} ->
            QueuePid ! {result, F(Task, Args)},
            worker(QueuePid, F, Args);
        empty ->
            ok
    end.
