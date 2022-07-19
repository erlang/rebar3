%% Copyright 2014 Sean Cribbs
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

%% @doc Binomial heap based on Okasaki 6.2.2
-module(binomial_heap).
-export([new/0, insert/2, insert/3, merge/2, delete/1, to_list/1, take/2, size/1]).
-record(node,{
          rank = 0    :: non_neg_integer(),
          key         :: term(),
          value       :: term(),
          children = new() :: binomial_heap()
         }).

-export_type([binomial_heap/0, heap_node/0]).
-type binomial_heap() :: [ heap_node() ].
-type heap_node() :: #node{}.

-spec new() -> binomial_heap().
new() ->
    [].

% Inserts a new pair into the heap (or creates a new heap)
-spec insert(term(), term()) -> binomial_heap().
insert(Key,Value) ->
    insert(Key,Value,[]).

-spec insert(term(), term(), binomial_heap()) -> binomial_heap().
insert(Key,Value,Forest) ->
    insTree(#node{key=Key,value=Value},Forest).

% Merges two heaps
-spec merge(binomial_heap(), binomial_heap()) -> binomial_heap().
merge(TS1,[]) when is_list(TS1) -> TS1;
merge([],TS2) when is_list(TS2) -> TS2;
merge([#node{rank=R1}=T1|TS1]=F1,[#node{rank=R2}=T2|TS2]=F2) ->
    if
        R1 < R2 ->
            [T1 | merge(TS1,F2)];
        R2 < R1 ->
            [T2 | merge(F1, TS2)];
        true ->
            insTree(link(T1,T2),merge(TS1,TS2))
    end.

% Deletes the top entry from the heap and returns it
-spec delete(binomial_heap()) -> {{term(), term()}, binomial_heap()}.
delete(TS) ->
    {#node{key=Key,value=Value,children=TS1},TS2} = getMin(TS),
    {{Key,Value},merge(lists:reverse(TS1),TS2)}.

% Turns the heap into list in heap order
-spec to_list(binomial_heap()) -> [{term(), term()}].
to_list([]) -> [];
to_list(List) when is_list(List) ->
    to_list([],List).
to_list(Acc, []) ->
    lists:reverse(Acc);
to_list(Acc,Forest) ->
    {Next, Trees} = delete(Forest),
    to_list([Next|Acc], Trees).

% Take N elements from the top of the heap
-spec take(non_neg_integer(), binomial_heap()) -> [{term(), term()}].
take(N,Trees) when is_integer(N), is_list(Trees) ->
    take(N,Trees,[]).
take(0,_Trees,Acc) ->
    lists:reverse(Acc);
take(_N,[],Acc)->
    lists:reverse(Acc);
take(N,Trees,Acc) ->
    {Top,T2} = delete(Trees),
    take(N-1,T2,[Top|Acc]).

% Get an estimate of the size based on the binomial property
-spec size(binomial_heap()) -> non_neg_integer().
size(Forest) ->
    erlang:trunc(lists:sum([math:pow(2,R) || #node{rank=R} <- Forest])).

%% Private API
-spec link(heap_node(), heap_node()) -> heap_node().
link(#node{rank=R,key=X1,children=C1}=T1,#node{key=X2,children=C2}=T2) ->
    case X1 < X2 of
        true ->
            T1#node{rank=R+1,children=[T2|C1]};
        _ ->
            T2#node{rank=R+1,children=[T1|C2]}
    end.

insTree(Tree, []) ->
    [Tree];
insTree(#node{rank=R1}=T1, [#node{rank=R2}=T2|Rest] = TS) ->
    case R1 < R2 of
        true ->
           [T1|TS];
        _ ->
            insTree(link(T1,T2),Rest)
    end.

getMin([T]) ->
    {T,[]};
getMin([#node{key=K} = T|TS]) ->
    {#node{key=K1} = T1,TS1} = getMin(TS),
    case K < K1 of
        true -> {T,TS};
        _ -> {T1,[T|TS1]}
    end.
