-module(rebar_parallel_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() -> [empty_set, same_results, pool_fetcher, pool_misuse].

empty_set() ->
    [{doc, "Running with null tasks still works"}].
empty_set(_) ->
    ?assertEqual([],
                 rebar_parallel:queue([],
                                      fun(X,_) -> X end, [arg],
                                      fun(_,_) -> ok end, [arg])),
    ?assertEqual([],
                 rebar_parallel:queue([],
                                      fun(X,_) -> X end, [arg],
                                      fun(X,_) -> {ok, X} end, [arg])),
    P1 = rebar_parallel:pool(fun(X,_) -> X end, [arg],
                             fun(_,_) -> ok end, [arg]),
    ?assertEqual([],
                 rebar_parallel:pool_terminate(P1)),
    P2 = rebar_parallel:pool(fun(X,_) -> X end, [arg],
                             fun(X,_) -> {ok,X} end, [arg]),
    ?assertEqual([],
                 rebar_parallel:pool_terminate(P2)),
    ok.

same_results() ->
    [{doc, "The two parallel methods can be equivalent but the pool can "
           "be used to do asynchronous task creation"}].
same_results(_) ->
    ?assertEqual([2,4,6,8,10,12,14],
                 lists:sort(
                   rebar_parallel:queue([1,2,3,4,5,6,7],
                                        fun(X,_) -> X*2 end, [],
                                        fun(X,_) -> {ok, X} end, []))),
    P = rebar_parallel:pool(fun(X,_) -> X*2 end, [],
                            fun(X,_) -> {ok, X} end, []),
    _ = [rebar_parallel:pool_task_async(P, N) || N <- [1,2,3,4,5,6,7]],
    ?assertEqual([2,4,6,8,10,12,14],
                 lists:sort(rebar_parallel:pool_terminate(P))),
    ok.

pool_fetcher() ->
    [{doc, "The fetcher from a pool can be from a different process "
           "and the other one will get an error."}].
pool_fetcher(_) ->
    Parent = self(),
    P = rebar_parallel:pool(fun(X,_) -> X*2 end, [],
                            fun(X,_) -> {ok, X} end, []),
    _ = [rebar_parallel:pool_task_async(P, N) || N <- [1,2,3,4,5,6,7]],
    spawn_link(fun() -> Parent ! {res, lists:sort(rebar_parallel:pool_terminate(P))} end),
    receive
        {res, X} -> ?assertEqual([2,4,6,8,10,12,14], X)
        after 500 -> error(timeout)
    end,
    ok.

pool_misuse() ->
    [{doc, "Using the pool for tasks after it is terminated but before "
           "it returns, you get a crash even if it's async"}].
pool_misuse(_) ->
    Parent = self(),
    P = rebar_parallel:pool(fun(_,_) -> timer:sleep(1000) end, [],
                            fun(X,_) -> {ok, X} end, []),
    _ = [rebar_parallel:pool_task_async(P, N) || N <- [1,2,3,4,5,6,7]],
    spawn(fun() -> Parent ! ok, rebar_parallel:pool_terminate(P) end),
    receive ok -> timer:sleep(100) end,
    Old = process_flag(trap_exit, true),
    rebar_parallel:pool_task_async(P, 0),
    receive
        {'EXIT', P, {{nocatch, rebar_abort}, _Stack}} ->
            process_flag(trap_exit, Old)
    after 1000 ->
        process_flag(trap_exit, Old),
        error(no_abort)
    end,
    ok.

