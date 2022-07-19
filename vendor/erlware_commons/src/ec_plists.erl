%%% -*- mode: Erlang; fill-column: 80; comment-column: 75; -*-
%%% vi:ts=4 sw=4 et
%%% The MIT License
%%%
%%% Copyright (c) 2007 Stephen Marsh
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a copy
%%% of this software and associated documentation files (the "Software"), to deal
%%% in the Software without restriction, including without limitation the rights
%%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%%% copies of the Software, and to permit persons to whom the Software is
%%% furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%%% THE SOFTWARE.
%%%---------------------------------------------------------------------------
%%% @author Stephen Marsh
%%% @copyright 2007 Stephen Marsh freeyourmind ++ [$@|gmail.com]
%%% @doc
%%% plists is a drop-in replacement for module <a
%%% href="http://www.erlang.org/doc/man/lists.html">lists</a>, making
%%% most list operations parallel. It can operate on each element in
%%% parallel, for IO-bound operations, on sublists in parallel, for
%%% taking advantage of multi-core machines with CPU-bound operations,
%%% and across erlang nodes, for parallizing inside a cluster. It
%%% handles errors and node failures. It can be configured, tuned, and
%%% tweaked to get optimal performance while minimizing overhead.
%%%
%%% Almost all the functions are identical to equivalent functions in
%%% lists, returning exactly the same result, and having both a form
%%% with an identical syntax that operates on each element in parallel
%%% and a form which takes an optional "malt", a specification for how
%%% to parallize the operation.
%%%
%%% fold is the one exception, parallel fold is different from linear
%%% fold.  This module also include a simple mapreduce implementation,
%%% and the function runmany. All the other functions are implemented
%%% with runmany, which is as a generalization of parallel list
%%% operations.
%%%
%%% Malts
%%% =====
%%%
%%% A malt specifies how to break a list into sublists, and can optionally
%%% specify a timeout, which nodes to run on, and how many processes to start
%%% per node.
%%%
%%%     Malt = MaltComponent | [MaltComponent]
%%%     MaltComponent = SubListSize::integer() | {processes, integer()} |
%%%                     {processes, schedulers} |
%%%     {timeout, Milliseconds::integer()} | {nodes, [NodeSpec]}<br/>
%%%
%%%     NodeSpec = Node::atom() | {Node::atom(), NumProcesses::integer()} |
%%%    {Node::atom(), schedulers}
%%%
%%% An integer can be given to specify the exact size for sublists. 1
%%% is a good choice for IO-bound operations and when the operation on
%%% each list element is expensive. Larger numbers minimize overhead
%%% and are faster for cheap operations.
%%%
%%% If the integer is omitted, and you have specified a `{processes,
%%% X}`, the list is split into X sublists. This is only useful when
%%% the time to process each element is close to identical and you
%%% know exactly how many lines of execution are available to you.
%%%
%%% If neither of the above applies, the sublist size defaults to 1.
%%%
%%% You can use `{processes, X}` to have the list processed by `X`
%%% processes on the local machine. A good choice for `X` is the
%%% number of lines of execution (cores) the machine provides. This
%%% can be done automatically with {processes, schedulers}, which sets
%%% the number of processes to the number of schedulers in the erlang
%%% virtual machine (probably equal to the number of cores).
%%%
%%% `{timeout, Milliseconds}` specifies a timeout. This is a timeout
%%% for the entire operation, both operating on the sublists and
%%% combining the results.  exit(timeout) is evaluated if the timeout
%%% is exceeded.
%%%
%%% `{nodes, NodeList}` specifies that the operation should be done
%%% across nodes.  Every element of NodeList is of the form
%%% `{NodeName, NumProcesses}` or NodeName, which means the same as
%%% `{NodeName, 1}`. plists runs NumProcesses processes on NodeName
%%% concurrently. A good choice for NumProcesses is the number of
%%% lines of execution (cores) a node provides plus one. This ensures
%%% the node is completely busy even when fetching a new sublist. This
%%% can be done automatically with `{NodeName, schedulers}`, in which
%%% case plists uses a cached value if it has one, and otherwise finds
%%% the number of schedulers in the remote node and adds one. This
%%% will ensure at least one busy process per core (assuming the node
%%% has a scheduler for each core).
%%%
%%% plists is able to recover if a node goes down.  If all nodes go
%%% down, exit(allnodescrashed) is evaluated.
%%%
%%% Any of the above may be used as a malt, or may be combined into a
%%% list.  `{nodes, NodeList}` and {processes, X} may not be combined.
%%%
%%% Examples
%%% ========
%%%
%%%      %%start a process for each element (1-element sublists)<
%%%      1
%%%
%%%     %% start a process for each ten elements (10-element sublists)
%%%     10
%%%
%%%     %% split the list into two sublists and process in two processes
%%%     {processes, 2}
%%%
%%%     %% split the list into X sublists and process in X processes,
%%%     %% where X is the number of cores in the machine
%%%     {processes, schedulers}
%%%
%%%     %% split the list into 10-element sublists and process in two processes
%%%     [10, {processes, 2}]
%%%
%%%     %% timeout after one second. Assumes that a process should be started
%%%     %% for each element.<br/>
%%%     {timeout, 1000}
%%%
%%%     %% Runs 3 processes at a time on apple@desktop, and 2 on orange@laptop
%%%     %% This is the best way to utilize all the CPU-power of a dual-core<br/>
%%%     %% desktop and a single-core laptop. Assumes that the list should be<br/>
%%%     %% split into 1-element sublists.<br/>
%%%     {nodes, [{apple@desktop, 3}, {orange@laptop, 2}]}
%%%
%%%     %% Like above, but makes plists figure out how many processes to use.
%%%     {nodes, [{apple@desktop, schedulers}, {orange@laptop, schedulers}]}
%%%
%%%     %% Gives apple and orange three seconds to process the list as<br/>
%%%     %% 100-element sublists.<br/>
%%%     [100, {timeout, 3000}, {nodes, [{apple@desktop, 3}, {orange@laptop, 2}]}]
%%%
%%% Aside: Why Malt?
%%% ================
%%%
%%% I needed a word for this concept, so maybe my subconsciousness
%%% gave me one by making me misspell multiply. Maybe it is an acronym
%%% for Malt is A List Tearing Specification. Maybe it is a beer
%%% metaphor, suggesting that code only runs in parallel if bribed
%%% with spirits. It's jargon, learn it or you can't be part of the
%%% in-group.
%%%
%%% Messages and Errors
%%% ===================
%%%
%%% plists assures that no extraneous messages are left in or will
%%% later enter the message queue. This is guaranteed even in the
%%% event of an error.
%%%
%%% Errors in spawned processes are caught and propagated to the
%%% calling process. If you invoke
%%%
%%%     plists:map(fun (X) -> 1/X end, [1, 2, 3, 0]).
%%%
%%% you get a badarith error, exactly like when you use lists:map.
%%%
%%% plists uses monitors to watch the processes it spawns. It is not a
%%% good idea to invoke plists when you are already monitoring
%%% processes. If one of them does a non-normal exit, plists receives
%%% the 'DOWN' message believing it to be from one of its own
%%% processes. The error propagation system goes into effect, which
%%% results in the error occuring in the calling process.
%%%
-module(ec_plists).

-export([all/2, all/3,
         any/2, any/3,
         filter/2, filter/3,
         fold/3, fold/4, fold/5,
         foreach/2, foreach/3,
         map/2, map/3,
         ftmap/2, ftmap/3,
         partition/2, partition/3,
         sort/1, sort/2, sort/3,
         usort/1, usort/2, usort/3,
         mapreduce/2, mapreduce/3, mapreduce/5,
         runmany/3, runmany/4]).

-export_type([malt/0, malt_component/0, node_spec/0, fuse/0, fuse_fun/0]).

%%============================================================================
%% types
%%============================================================================

-type malt() :: malt_component() | [malt_component()].

-type malt_component() :: SubListSize::integer()
                          | {processes, integer()}
                          | {processes, schedulers}
                          | {timeout, Milliseconds::integer()}
                          | {nodes, [node_spec()]}.

-type node_spec() ::  Node::atom()
                          | {Node::atom(), NumProcesses::integer()}
                          | {Node::atom(), schedulers}.

-type fuse_fun() ::  fun((term(), term()) -> term()).
-type fuse() ::  fuse_fun() | {recursive, fuse_fun()} | {reverse, fuse_fun()}.
-type el_fun() :: fun((term()) -> term()).

%%============================================================================
%% API
%%============================================================================

%% Everything here is defined in terms of runmany.
%% The following methods are convient interfaces to runmany.

%% @doc Same semantics as in module
%% <a href="http://www.erlang.org/doc/man/lists.html">lists</a>.
-spec all(el_fun(), list()) -> boolean().
all(Fun, List) ->
    all(Fun, List, 1).

%% @doc Same semantics as in module
%% <a href="http://www.erlang.org/doc/man/lists.html">lists</a>.
-spec all(el_fun(), list(), malt()) -> boolean().
all(Fun, List, Malt) ->
    try
        runmany(fun (L) ->
                        B = lists:all(Fun, L),
                        if
                            B ->
                                nil;
                            true ->
                                erlang:throw(notall)
                        end
                end,
                fun (_A1, _A2) ->
                        nil
                end,
                List, Malt),
        true
    catch
        throw:notall ->
            false
    end.

%% @doc Same semantics as in module
%% <a href="http://www.erlang.org/doc/man/lists.html">lists</a>.
-spec any(fun(), list()) -> boolean().
any(Fun, List) ->
    any(Fun, List, 1).

%% @doc Same semantics as in module
%% <a href="http://www.erlang.org/doc/man/lists.html">lists</a>.
-spec any(fun(), list(), malt()) -> boolean().
any(Fun, List, Malt) ->
    try
        runmany(fun (L) ->
                        B = lists:any(Fun, L),
                        if B ->
                                erlang:throw(any);
                           true ->
                                nil
                        end
                end,
                fun (_A1, _A2) ->
                        nil
                end,
                List, Malt) of
        _ ->
            false
    catch throw:any ->
            true
    end.

%% @doc Same semantics as in module
%% <a href="http://www.erlang.org/doc/man/lists.html">lists</a>.
-spec filter(fun(), list()) -> list().
filter(Fun, List) ->
    filter(Fun, List, 1).

%% @doc Same semantics as in module
%% <a href="http://www.erlang.org/doc/man/lists.html">lists</a>.
-spec filter(fun(), list(), malt()) -> list().
filter(Fun, List, Malt) ->
    runmany(fun (L) ->
                    lists:filter(Fun, L)
            end,
            {reverse, fun (A1, A2) ->
                    A1 ++ A2
            end},
            List, Malt).

%% Note that with parallel fold there is not foldl and foldr,
%% instead just one fold that can fuse Accumlators.

%% @doc Like below, but assumes 1 as the Malt. This function is almost useless,
%% and is intended only to aid converting code from using lists to plists.
-spec fold(fun(), InitAcc::term(), list()) -> term().
fold(Fun, InitAcc, List) ->
    fold(Fun, Fun, InitAcc, List, 1).

%% @doc Like below, but uses the Fun as the Fuse by default.
-spec fold(fun(), InitAcc::term(), list(), malt()) -> term().
fold(Fun, InitAcc, List, Malt) ->
    fold(Fun, Fun, InitAcc, List, Malt).

%% @doc fold is more complex when made parallel. There is no foldl and
%% foldr, accumulators aren't passed in any defined order.  The list
%% is split into sublists which are folded together. Fun is identical
%% to the function passed to lists:fold[lr], it takes (an element, and
%% the accumulator) and returns -> a new accumulator.  It is used for
%% the initial stage of folding sublists. Fuse fuses together the
%% results, it takes (Results1, Result2) and returns -> a new result.
%% By default sublists are fused left to right, each result of a fuse
%% being fed into the first element of the next fuse. The result of
%% the last fuse is the result.
%%
%% Fusing may also run in parallel using a recursive algorithm,
%% by specifying the fuse as {recursive, Fuse}. See
%% the discussion in {@link runmany/4}.
%%
%% Malt is the malt for the initial folding of sublists, and for the
%% possible recursive fuse.
-spec fold(fun(), fuse(), InitAcc::term(), list(), malt()) -> term().
fold(Fun, Fuse, InitAcc, List, Malt) ->
    Fun2 = fun (L) ->
                   lists:foldl(Fun, InitAcc, L)
           end,
    runmany(Fun2, Fuse, List, Malt).

%% @doc Similiar to foreach in module
%% <a href="http://www.erlang.org/doc/man/lists.html">lists</a>
%% except it makes no guarantee about the order it processes list elements.
-spec foreach(fun(), list()) -> ok.
foreach(Fun, List) ->
    foreach(Fun, List, 1).

%% @doc Similiar to foreach in module
%% <a href="http://www.erlang.org/doc/man/lists.html">lists</a>
%% except it makes no guarantee about the order it processes list elements.
-spec foreach(fun(), list(), malt()) -> ok.
foreach(Fun, List, Malt) ->
    runmany(fun (L) ->
                    lists:foreach(Fun, L)
            end,
            fun (_A1, _A2) ->
                    ok
            end,
            List, Malt).

%% @doc Same semantics as in module
%% <a href="http://www.erlang.org/doc/man/lists.html">lists</a>.
-spec map(fun(), list()) -> list().
map(Fun, List) ->
    map(Fun, List, 1).

%% @doc Same semantics as in module
%% <a href="http://www.erlang.org/doc/man/lists.html">lists</a>.
-spec map(fun(), list(), malt()) -> list().
map(Fun, List, Malt) ->
    runmany(fun (L) ->
                    lists:map(Fun, L)
            end,
            {reverse, fun (A1, A2) ->
                              A1 ++ A2
                      end},
            List, Malt).

%% @doc values are returned as {value, term()}.
-spec ftmap(fun(), list()) -> list().
ftmap(Fun, List) ->
   map(fun(L) ->
               try
                   {value, Fun(L)}
               catch
                   Class:Type ->
                       {error, {Class, Type}}
               end
       end, List).

%% @doc values are returned as {value, term()}.
-spec ftmap(fun(), list(), malt()) -> list().
ftmap(Fun, List, Malt) ->
    map(fun(L) ->
                try
                    {value, Fun(L)}
                catch
                    Class:Type ->
                        {error, {Class, Type}}
                end
        end, List, Malt).

%% @doc Same semantics as in module
%% <a href="http://www.erlang.org/doc/man/lists.html">lists</a>.
-spec partition(fun(), list()) -> {list(), list()}.
partition(Fun, List) ->
    partition(Fun, List, 1).

%% @doc Same semantics as in module
%% <a href="http://www.erlang.org/doc/man/lists.html">lists</a>.
-spec partition(fun(), list(), malt()) -> {list(), list()}.
partition(Fun, List, Malt) ->
    runmany(fun (L) ->
                    lists:partition(Fun, L)
            end,
            {reverse, fun ({True1, False1}, {True2, False2}) ->
                    {True1 ++ True2, False1 ++ False2}
            end},
            List, Malt).

%% SORTMALT needs to be tuned
-define(SORTMALT, 100).

%% @doc Same semantics as in module
%% <a href="http://www.erlang.org/doc/man/lists.html">lists</a>.
-spec sort(list()) -> list().
sort(List) ->
    sort(fun (A, B) ->
                 A =< B
         end,
         List).

%% @doc Same semantics as in module
%% <a href="http://www.erlang.org/doc/man/lists.html">lists</a>.
-spec sort(fun(), list()) -> list().
sort(Fun, List) ->
    sort(Fun, List, ?SORTMALT).

%% @doc This version lets you specify your own malt for sort.
%%
%% sort splits the list into sublists and sorts them, and it merges the
%% sorted lists together. These are done in parallel. Each sublist is
%% sorted in a seperate process, and each merging of results is done in a
%% seperate process. Malt defaults to 100, causing the list to be split into
%% 100-element sublists.
-spec sort(fun(), list(), malt()) -> list().
sort(Fun, List, Malt) ->
    Fun2 = fun (L) ->
                   lists:sort(Fun, L)
           end,
    Fuse = fun (A1, A2) ->
                   lists:merge(Fun, A1, A2)
           end,
    runmany(Fun2, {recursive, Fuse}, List, Malt).

%% @doc Same semantics as in module
%% <a href="http://www.erlang.org/doc/man/lists.html">lists</a>.
-spec usort(list()) -> list().
usort(List) ->
    usort(fun (A, B) ->
                  A =< B
          end,
         List).

%% @doc Same semantics as in module
%% <a href="http://www.erlang.org/doc/man/lists.html">lists</a>.
-spec usort(fun(), list()) -> list().
usort(Fun, List) ->
    usort(Fun, List, ?SORTMALT).

%% @doc This version lets you specify your own malt for usort.
%%
%% usort splits the list into sublists and sorts them, and it merges the
%% sorted lists together. These are done in parallel. Each sublist is
%% sorted in a seperate process, and each merging of results is done in a
%% seperate process. Malt defaults to 100, causing the list to be split into
%% 100-element sublists.
%%
%% usort removes duplicate elments while it sorts.
-spec usort(fun(), list(), malt()) -> list().
usort(Fun, List, Malt) ->
    Fun2 = fun (L) ->
                   lists:usort(Fun, L)
           end,
    Fuse = fun (A1, A2) ->
                   lists:umerge(Fun, A1, A2)
           end,
    runmany(Fun2, {recursive, Fuse}, List, Malt).

%% @doc Like below, assumes default MapMalt of 1.
-ifdef(namespaced_types).
-spec mapreduce(MapFunc, list()) -> dict:dict() when
      MapFunc ::  fun((term()) -> DeepListOfKeyValuePairs),
      DeepListOfKeyValuePairs :: [DeepListOfKeyValuePairs] | {Key::term(), Value::term()}.
-else.
-spec mapreduce(MapFunc, list()) -> dict() when
      MapFunc ::  fun((term()) -> DeepListOfKeyValuePairs),
      DeepListOfKeyValuePairs :: [DeepListOfKeyValuePairs] | {Key::term(), Value::term()}.
-endif.


mapreduce(MapFunc, List) ->
    mapreduce(MapFunc, List, 1).

%% Like below, but uses a default reducer that collects all
%% {Key, Value} pairs into a
%% <a href="http://www.erlang.org/doc/man/dict.html">dict</a>,
%% with values {Key, [Value1, Value2...]}.
%% This dict is returned as the result.
mapreduce(MapFunc, List, MapMalt) ->
    mapreduce(MapFunc, List, dict:new(), fun add_key/3, MapMalt).

%% @doc This is a very basic mapreduce. You won't write a
%% Google-rivaling search engine with it. It has no equivalent in
%% lists. Each element in the list is run through the MapFunc, which
%% produces either a {Key, Value} pair, or a lists of key value pairs,
%% or a list of lists of key value pairs...etc. A reducer process runs
%% in parallel with the mapping processes, collecting the key value
%% pairs. It starts with a state given by InitState, and for each
%% {Key, Value} pair that it receives it invokes ReduceFunc(OldState,
%% Key, Value) to compute its new state. mapreduce returns the
%% reducer's final state.
%%
%% MapMalt is the malt for the mapping operation, with a default value of 1,
%% meaning each element of the list is mapped by a seperate process.
%%
%% mapreduce requires OTP R11B, or it may leave monitoring messages in the
%% message queue.
-ifdef(namespaced_types).
-spec mapreduce(MapFunc, list(), InitState::term(), ReduceFunc, malt()) -> dict:dict() when
      MapFunc :: fun((term()) -> DeepListOfKeyValuePairs),
      DeepListOfKeyValuePairs :: [DeepListOfKeyValuePairs] | {Key::term(), Value::term()},
      ReduceFunc :: fun((OldState::term(), Key::term(), Value::term()) -> NewState::term()).
-else.
-spec mapreduce(MapFunc, list(), InitState::term(), ReduceFunc, malt()) -> dict() when
      MapFunc :: fun((term()) -> DeepListOfKeyValuePairs),
      DeepListOfKeyValuePairs :: [DeepListOfKeyValuePairs] | {Key::term(), Value::term()},
      ReduceFunc :: fun((OldState::term(), Key::term(), Value::term()) -> NewState::term()).
-endif.
mapreduce(MapFunc, List, InitState, ReduceFunc, MapMalt) ->
    Parent = self(),
    {Reducer, ReducerRef} =
        erlang:spawn_monitor(fun () ->
                                     reducer(Parent, 0, InitState, ReduceFunc)
                             end),
    MapFunc2 = fun (L) ->
                       Reducer ! lists:map(MapFunc, L),
                       1
               end,
    SentMessages = try
                       runmany(MapFunc2, fun (A, B) -> A+B end, List, MapMalt)
                   catch
                       exit:Reason ->
                           erlang:demonitor(ReducerRef, [flush]),
                           Reducer ! die,
                           exit(Reason)
                   end,
    Reducer ! {mappers, done, SentMessages},
    Results = receive
                  {Reducer, Results2} ->
                      Results2;
                  {'DOWN', _, _, Reducer, Reason2} ->
                      exit(Reason2)
              end,
    receive
        {'DOWN', _, _, Reducer, normal} ->
            nil
    end,
    Results.

reducer(Parent, NumReceived, State, Func) ->
    receive
        die ->
            nil;
        {mappers, done, NumReceived} ->
            Parent ! {self (), State};
        Keys  ->
            reducer(Parent, NumReceived + 1, each_key(State, Func, Keys), Func)
    end.

each_key(State, Func, {Key, Value}) ->
    Func(State, Key, Value);
each_key(State, Func, [List|Keys]) ->
    each_key(each_key(State, Func, List), Func, Keys);
each_key(State, _, []) ->
    State.

add_key(Dict, Key, Value) ->
    case dict:is_key(Key, Dict) of
        true ->
            dict:append(Key, Value, Dict);
        false ->
            dict:store(Key, [Value], Dict)
    end.

%% @doc Like below, but assumes a Malt of 1,
%% meaning each element of the list is processed by a seperate process.
-spec runmany(fun(), fuse(), list()) -> term().
runmany(Fun, Fuse, List) ->
    runmany(Fun, Fuse, List, 1).

%% Begin internal stuff (though runmany/4 is exported).

%% @doc All of the other functions are implemented with runmany. runmany
%% takes a List, splits it into sublists, and starts processes to operate on
%% each sublist, all done according to Malt. Each process passes its sublist
%% into Fun and sends the result back.
%%
%% The results are then fused together to get the final result. There are two
%% ways this can operate, lineraly and recursively. If Fuse is a function,
%% a fuse is done linearly left-to-right on the sublists, the results
%% of processing the first and second sublists being passed to Fuse, then
%% the result of the first fuse and processing the third sublits, and so on. If
%% Fuse is {reverse, FuseFunc}, then a fuse is done right-to-left, the results
%% of processing the second-to-last and last sublists being passed to FuseFunc,
%% then the results of processing the third-to-last sublist and
%% the results of the first fuse, and and so forth.
%% Both methods preserve the original order of the lists elements.
%%
%% To do a recursive fuse, pass Fuse as {recursive, FuseFunc}.
%% The recursive fuse makes no guarantee about the order the results of
%% sublists, or the results of fuses are passed to FuseFunc. It
%% continues fusing pairs of results until it is down to one.
%%
%% Recursive fuse is down in parallel with processing the sublists, and a
%% process is spawned to fuse each pair of results. It is a parallized
%% algorithm. Linear fuse is done after all results of processing sublists
%% have been collected, and can only run in a single process.
%%
%% Even if you pass {recursive, FuseFunc}, a recursive fuse is only done if
%% the malt contains {nodes, NodeList} or {processes, X}. If this is not the
%% case, a linear fuse is done.
-spec runmany(fun(([term()]) -> term()), fuse(), list(), malt()) -> term().
runmany(Fun, Fuse, List, Malt)
  when erlang:is_list(Malt) ->
    runmany(Fun, Fuse, List, local, no_split, Malt);
runmany(Fun, Fuse, List, Malt) ->
    runmany(Fun, Fuse, List, [Malt]).

runmany(Fun, Fuse, List, Nodes, no_split, [MaltTerm|Malt])
  when erlang:is_integer(MaltTerm) ->
    runmany(Fun, Fuse, List, Nodes, MaltTerm, Malt);
runmany(Fun, Fuse, List, local, Split, [{processes, schedulers}|Malt]) ->
    %% run a process for each scheduler
    S = erlang:system_info(schedulers),
    runmany(Fun, Fuse, List, local, Split, [{processes, S}|Malt]);
runmany(Fun, Fuse, List, local, no_split, [{processes, X}|_]=Malt) ->
    %% Split the list into X sublists, where X is the number of processes
    L = erlang:length(List),
    case (L rem X) of
        0 ->
            runmany(Fun, Fuse, List, local, (L / X), Malt);
        _ ->
            runmany(Fun, Fuse, List, local, (L / X) + 1, Malt)
    end;
runmany(Fun, Fuse, List, local, Split, [{processes, X}|Malt]) ->
    %% run X process on local machine
    Nodes = lists:duplicate(X, node()),
    runmany(Fun, Fuse, List, Nodes, Split, Malt);
runmany(Fun, Fuse, List, Nodes, Split, [{timeout, X}|Malt]) ->
    Parent = erlang:self(),
    Timer = proc_lib:spawn(fun () ->
                                   receive
                                       stoptimer ->
                                           Parent ! {timerstopped, erlang:self()}
                                   after X ->
                                           Parent ! {timerrang, erlang:self()},
                                           receive
                                               stoptimer ->
                                                   Parent ! {timerstopped, erlang:self()}
                                           end
                                   end
                           end),
    Ans = try
              runmany(Fun, Fuse, List, Nodes, Split, Malt)
          catch
              %% we really just want the after block, the syntax
              %% makes this catch necessary.
              willneverhappen ->
                  nil
          after
              Timer ! stoptimer,
              cleanup_timer(Timer)
          end,
    Ans;
runmany(Fun, Fuse, List, local, Split, [{nodes, NodeList}|Malt]) ->
    Nodes = lists:foldl(fun ({Node, schedulers}, A) ->
                                X = schedulers_on_node(Node) + 1,
                                lists:reverse(lists:duplicate(X, Node), A);
                            ({Node, X}, A) ->
                                lists:reverse(lists:duplicate(X, Node), A);
                            (Node, A) ->
                                [Node|A]
                        end,
                        [], NodeList),
    runmany(Fun, Fuse, List, Nodes, Split, Malt);
runmany(Fun, {recursive, Fuse}, List, local, Split, []) ->
    %% local recursive fuse, for when we weren't invoked with {processes, X}
    %% or {nodes, NodeList}. Degenerates recursive fuse into linear fuse.
    runmany(Fun, Fuse, List, local, Split, []);
runmany(Fun, Fuse, List, Nodes, no_split, []) ->
    %% by default, operate on each element seperately
    runmany(Fun, Fuse, List, Nodes, 1, []);
runmany(Fun, Fuse, List, local, Split, []) ->
    List2 = splitmany(List, Split),
    local_runmany(Fun, Fuse, List2);
runmany(Fun, Fuse, List, Nodes, Split, []) ->
    List2 = splitmany(List, Split),
    cluster_runmany(Fun, Fuse, List2, Nodes).

cleanup_timer(Timer) ->
    receive
        {timerrang, Timer} ->
            cleanup_timer(Timer);
        {timerstopped, Timer} ->
            nil
    end.

schedulers_on_node(Node) ->
    case erlang:get(ec_plists_schedulers_on_nodes) of
        undefined ->
            X = determine_schedulers(Node),
            erlang:put(ec_plists_schedulers_on_nodes,
                dict:store(Node, X, dict:new())),
            X;
        Dict ->
            case dict:is_key(Node, Dict) of
                true ->
                    dict:fetch(Node, Dict);
                false ->
                    X = determine_schedulers(Node),
                    erlang:put(ec_plists_schedulers_on_nodes,
                        dict:store(Node, X, Dict)),
                    X
            end
    end.

determine_schedulers(Node) ->
    Parent = erlang:self(),
    Child = proc_lib:spawn(Node, fun () ->
                                         Parent ! {self(), erlang:system_info(schedulers)}
                                 end),
    erlang:monitor(process, Child),
    receive
        {Child, X} ->
            receive
                {'DOWN', _, _, Child, _Reason} ->
                    nil
            end,
            X;
        {'DOWN', _, _, Child, Reason} when Reason =/= normal ->
            0
    end.

%% @doc local runmany, for when we weren't invoked with {processes, X}
%% or {nodes, NodeList}. Every sublist is processed in parallel.
local_runmany(Fun, Fuse, List) ->
    Parent = self (),
    Pids = lists:map(fun (L) ->
                             F = fun () ->
                                         Parent ! {self (), Fun(L)}
                                 end,
                             {Pid, _} = erlang:spawn_monitor(F),
                             Pid
                     end,
                     List),
    Answers = try
                  lists:map(fun receivefrom/1, Pids)
              catch
                  throw:Message ->
                      {BadPid, Reason} = Message,
                      handle_error(BadPid, Reason, Pids)
              end,
    lists:foreach(fun (Pid) ->
                          normal_cleanup(Pid)
                  end, Pids),
    fuse(Fuse, Answers).

receivefrom(Pid) ->
    receive
        {Pid, R} ->
            R;
        {'DOWN', _, _, Pid, Reason} when Reason =/= normal ->
            erlang:throw({Pid, Reason});
        {timerrang, _} ->
            erlang:throw({nil, timeout})
    end.

%% Convert List into [{Number, Sublist}]
cluster_runmany(Fun, Fuse, List, Nodes) ->
    {List2, _} = lists:foldl(fun (X, {L, Count}) ->
                                     {[{Count, X}|L], Count+1}
                             end,
                             {[], 0}, List),
    cluster_runmany(Fun, Fuse, List2, Nodes, [], []).

%% @doc Add a pair of results into the TaskList as a fusing task
cluster_runmany(Fun, {recursive, Fuse}, [], Nodes, Running,
                [{_, R1}, {_, R2}|Results]) ->
    cluster_runmany(Fun, {recursive, Fuse}, [{fuse, R1, R2}], Nodes,
                    Running, Results);
cluster_runmany(_, {recursive, _Fuse}, [], _Nodes, [], [{_, Result}]) ->
    %% recursive fuse done, return result
    Result;
cluster_runmany(_, {recursive, _Fuse}, [], _Nodes, [], []) ->
    %% edge case where we are asked to do nothing
    [];
cluster_runmany(_, Fuse, [], _Nodes, [], Results) ->
    %% We're done, now we just have to [linear] fuse the results
    fuse(Fuse, lists:map(fun ({_, R}) ->
                                 R
                         end,
                         lists:sort(fun ({A, _}, {B, _}) ->
                                            A =< B
                                    end,
                                    lists:reverse(Results))));
cluster_runmany(Fun, Fuse, [Task|TaskList], [N|Nodes], Running, Results) ->
%% We have a ready node and a sublist or fuse to be processed, so we start
%% a new process

    Parent = erlang:self(),
    case Task of
        {Num, L2} ->
            Fun2 = fun () ->
                           Parent ! {erlang:self(), Num, Fun(L2)}
                   end;
        {fuse, R1, R2} ->
            {recursive, FuseFunc} = Fuse,
            Fun2 = fun () ->
                           Parent ! {erlang:self(), fuse, FuseFunc(R1, R2)}
                   end
    end,
    Fun3 = fun() -> runmany_wrap(Fun2, Parent) end,
    Pid = proc_lib:spawn(N, Fun3),
    erlang:monitor(process, Pid),
    cluster_runmany(Fun, Fuse, TaskList, Nodes, [{Pid, N, Task}|Running], Results);
cluster_runmany(Fun, Fuse, TaskList, Nodes, Running, Results) when length(Running) > 0 ->
    %% We can't start a new process, but can watch over already running ones
    receive
        {_Pid, error, Reason} ->
            RunningPids = lists:map(fun ({Pid, _, _}) ->
                                            Pid
                                    end,
                                    Running),
            handle_error(junkvalue, Reason, RunningPids);
        {Pid, Num, Result} ->
            %% throw out the exit message, Reason should be
            %% normal, noproc, or noconnection
            receive
                {'DOWN', _, _, Pid, _Reason} ->
                    nil
            end,
            {Running2, FinishedNode, _} = delete_running(Pid, Running, []),
            cluster_runmany(Fun, Fuse, TaskList,
                            [FinishedNode|Nodes], Running2, [{Num, Result}|Results]);
        {timerrang, _} ->
            RunningPids = lists:map(fun ({Pid, _, _}) ->
                                            Pid
                                    end,
                                    Running),
            handle_error(nil, timeout, RunningPids);
        %% node failure
        {'DOWN', _, _, Pid, noconnection} ->
            {Running2, _DeadNode, Task} = delete_running(Pid, Running, []),
            cluster_runmany(Fun, Fuse, [Task|TaskList], Nodes,
                            Running2, Results);
        %% could a noproc exit message come before the message from
        %% the process? we are assuming it can't.
        %% this clause is unlikely to get invoked due to cluster_runmany's
        %% spawned processes. It will still catch errors in mapreduce's
        %% reduce process, however.
        {'DOWN', _, _, BadPid, Reason} when Reason =/= normal ->
            RunningPids = lists:map(fun ({Pid, _, _}) ->
                                            Pid
                                    end,
                                    Running),
            handle_error(BadPid, Reason, RunningPids)
    end;
cluster_runmany(_, _, [_Non|_Empty], []=_Nodes, []=_Running, _) ->
%% We have data, but no nodes either available or occupied
    erlang:exit(allnodescrashed).

-ifdef(fun_stacktrace).
runmany_wrap(Fun, Parent) ->
    try
        Fun
    catch
        exit:siblingdied ->
            ok;
        exit:Reason ->
            Parent ! {erlang:self(), error, Reason};
        error:R ->
            Parent ! {erlang:self(), error, {R, erlang:get_stacktrace()}};
        throw:R ->
            Parent ! {erlang:self(), error, {{nocatch, R}, erlang:get_stacktrace()}}
    end.
-else.
runmany_wrap(Fun, Parent) ->
    try
        Fun
    catch
        exit:siblingdied ->
            ok;
        exit:Reason ->
            Parent ! {erlang:self(), error, Reason};
        error:R:Stacktrace ->
            Parent ! {erlang:self(), error, {R, Stacktrace}};
        throw:R:Stacktrace ->
            Parent ! {erlang:self(), error, {{nocatch, R}, Stacktrace}}
    end.
-endif.

delete_running(Pid, [{Pid, Node, List}|Running], Acc) ->
    {Running ++ Acc, Node, List};
delete_running(Pid, [R|Running], Acc) ->
    delete_running(Pid, Running, [R|Acc]).

handle_error(BadPid, Reason, Pids) ->
    lists:foreach(fun (Pid) ->
                          erlang:exit(Pid, siblingdied)
                  end, Pids),
    lists:foreach(fun (Pid) ->
                          error_cleanup(Pid, BadPid)
                  end, Pids),
    erlang:exit(Reason).

error_cleanup(BadPid, BadPid) ->
    ok;
error_cleanup(Pid, BadPid) ->
    receive
        {Pid, _} ->
            error_cleanup(Pid, BadPid);
        {Pid, _, _} ->
            error_cleanup(Pid, BadPid);
        {'DOWN', _, _, Pid, _Reason} ->
            ok
    end.

normal_cleanup(Pid) ->
    receive
        {'DOWN', _, _, Pid, _Reason} ->
            ok
    end.

%% edge case
fuse(_, []) ->
    [];
fuse({reverse, _}=Fuse, Results) ->
    [RL|ResultsR] = lists:reverse(Results),
    fuse(Fuse, ResultsR, RL);
fuse(Fuse, [R1|Results]) ->
    fuse(Fuse, Results, R1).

fuse({reverse, FuseFunc}=Fuse, [R2|Results], R1) ->
    fuse(Fuse, Results, FuseFunc(R2, R1));
fuse(Fuse, [R2|Results], R1) ->
    fuse(Fuse, Results, Fuse(R1, R2));
fuse(_, [], R) ->
    R.

%% @doc Splits a list into a list of sublists, each of size Size,
%% except for the last element which is less if the original list
%% could not be evenly divided into Size-sized lists.
splitmany(List, Size) ->
    splitmany(List, [], Size).

splitmany([], Acc, _) ->
    lists:reverse(Acc);
splitmany(List, Acc, Size) ->
    {Top, NList} = split(Size, List),
    splitmany(NList, [Top|Acc], Size).

%% @doc Like lists:split, except it splits a list smaller than its first
%% parameter
split(Size, List) ->
    split(Size, List, []).

split(0, List, Acc) ->
    {lists:reverse(Acc), List};
split(Size, [H|List], Acc) ->
    split(Size - 1, List, [H|Acc]);
split(_, [], Acc) ->
    {lists:reverse(Acc), []}.
