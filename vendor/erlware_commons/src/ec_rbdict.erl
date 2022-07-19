%%% vi:ts=4 sw=4 et
%%% Copyright (c) 2008 Robert Virding. All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions
%%% are met:
%%%
%%% 1. Redistributions of source code must retain the above copyright
%%%    notice, this list of conditions and the following disclaimer.
%%% 2. Redistributions in binary form must reproduce the above copyright
%%%    notice, this list of conditions and the following disclaimer in the
%%%    documentation and/or other materials provided with the distribution.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
%%% FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
%%% COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
%%% INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%%% BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
%%% CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
%%% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
%%% ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%%% POSSIBILITY OF SUCH DAMAGE.
%%%-------------------------------------------------------------------
%%% @copyright 2008 Robert Verding
%%%
%%% @doc
%%%
%%% Rbdict implements a Key - Value dictionary. An rbdict is a
%%% representation of a dictionary, where a red-black tree is used to
%%% store the keys and values.
%%%
%%% This module implents exactly the same interface as the module
%%% ec_dictionary but with a defined representation. One difference is
%%% that while dict considers two keys as different if they do not
%%% match (=:=), this module considers two keys as different if and
%%% only if they do not compare equal (==).
%%%
%%% The algorithms here are taken directly from Okasaki and Rbset
%%% in ML/Scheme. The interface is compatible with the standard dict
%%% interface.
%%%
%%% The following structures are used to build the the RB-dict:
%%%
%%% {r,Left,Key,Val,Right}
%%% {b,Left,Key,Val,Right}
%%% empty
%%%
%%% It is interesting to note that expanding out the first argument of
%%% l/rbalance, the colour, in store etc. is actually slower than not
%%% doing it. Measured.
%%%
%%% see ec_dictionary
%%% @end
%%%-------------------------------------------------------------------
-module(ec_rbdict).

-behaviour(ec_dictionary).

%% Standard interface.
-export([add/3, from_list/1, get/2, get/3, has_key/2,
         has_value/2, new/0, remove/2, size/1, to_list/1,
         keys/1]).

-export_type([dictionary/2]).

%%%===================================================================
%%% Types
%%%===================================================================
%% This should be opaque, but that kills dialyzer so for now we export it
%% however you should not rely on the internal representation here
-type dictionary(K, V) :: empty | {color(),
                                   dictionary(K, V),
                                   ec_dictionary:key(K),
                                   ec_dictionary:value(V),
                                   dictionary(K, V)}.

-type color() :: r | b.

%%%===================================================================
%%% API
%%%===================================================================

-spec new() -> dictionary(_K, _V).
new() -> empty.

-spec has_key(ec_dictionary:key(K), dictionary(K, _V)) -> boolean().
has_key(_, empty) ->
    false;
has_key(K, {_, Left, K1, _, _}) when K < K1 ->
    has_key(K, Left);
has_key(K, {_, _, K1, _, Right}) when K > K1 ->
    has_key(K, Right);
has_key(_, {_, _, _, _, _}) ->
    true.

-spec get(ec_dictionary:key(K), dictionary(K, V)) -> ec_dictionary:value(V).
get(_, empty) ->
    throw(not_found);
get(K, {_, Left, K1, _, _}) when K < K1 ->
    get(K, Left);
get(K, {_, _, K1, _, Right}) when K > K1 ->
    get(K, Right);
get(_, {_, _, _, Val, _}) ->
    Val.

-spec get(ec_dictionary:key(K),
          ec_dictionary:value(V),
          dictionary(K, V)) -> ec_dictionary:value(V).
get(_, Default, empty) ->
    Default;
get(K, Default, {_, Left, K1, _, _}) when K < K1 ->
    get(K, Default, Left);
get(K, Default, {_, _, K1, _, Right}) when K > K1 ->
    get(K, Default, Right);
get(_, _, {_, _, _, Val, _}) ->
    Val.

-spec add(ec_dictionary:key(K), ec_dictionary:value(V),
          dictionary(K, V)) -> dictionary(K, V).
add(Key, Value, Dict) ->
    {_, L, K1, V1, R} = add1(Key, Value, Dict),
    {b, L, K1, V1, R}.

-spec remove(ec_dictionary:key(K), dictionary(K, V)) -> dictionary(K, V).
remove(Key, Dictionary) ->
    {Dict1, _} = erase_aux(Key, Dictionary), Dict1.

-spec has_value(ec_dictionary:value(V), dictionary(_K, V)) -> boolean().
has_value(Value, Dict) ->
    fold(fun (_, NValue, _) when NValue == Value -> true;
             (_, _, Acc) -> Acc
         end,
         false, Dict).

-spec size(dictionary(_K, _V)) -> non_neg_integer().
size(T) ->
    size1(T).

-spec to_list(dictionary(K, V)) ->
                     [{ec_dictionary:key(K), ec_dictionary:value(V)}].
to_list(T) ->
    to_list(T, []).

-spec from_list([{ec_dictionary:key(K), ec_dictionary:value(V)}]) ->
                       dictionary(K, V).
from_list(L) ->
    lists:foldl(fun ({K, V}, D) ->
                        add(K, V, D)
                end, new(),
                L).

-spec keys(dictionary(K, _V)) -> [ec_dictionary:key(K)].
keys(Dict) ->
    keys(Dict, []).

%%%===================================================================
%%% Enternal functions
%%%===================================================================
-spec keys(dictionary(K, _V), [ec_dictionary:key(K)]) ->
                  [ec_dictionary:key(K)].
keys(empty, Tail) ->
    Tail;
keys({_, L, K, _, R}, Tail) ->
    keys(L, [K | keys(R, Tail)]).


-spec erase_aux(ec_dictionary:key(K), dictionary(K, V)) ->
                       {dictionary(K, V), boolean()}.
erase_aux(_, empty) ->
    {empty, false};
erase_aux(K, {b, A, Xk, Xv, B}) ->
    if K < Xk ->
            {A1, Dec} = erase_aux(K, A),
            if Dec ->
                    unbalright(b, A1, Xk, Xv, B);
               true ->
                    {{b, A1, Xk, Xv, B}, false}
            end;
       K > Xk ->
            {B1, Dec} = erase_aux(K, B),
            if Dec ->
                    unballeft(b, A, Xk, Xv, B1);
               true ->
                    {{b, A, Xk, Xv, B1}, false}
            end;
       true ->
            case B of
                empty ->
                    blackify(A);
                _ ->
                    {B1, {Mk, Mv}, Dec} = erase_min(B),
                    if Dec ->
                            unballeft(b, A, Mk, Mv, B1);
                       true ->
                            {{b, A, Mk, Mv, B1}, false}
                    end
            end
    end;
erase_aux(K, {r, A, Xk, Xv, B}) ->
    if K < Xk ->
            {A1, Dec} = erase_aux(K, A),
            if Dec ->
                    unbalright(r, A1, Xk, Xv, B);
               true ->
                    {{r, A1, Xk, Xv, B}, false}
            end;
       K > Xk ->
            {B1, Dec} = erase_aux(K, B),
            if Dec ->
                    unballeft(r, A, Xk, Xv, B1);
               true ->
                    {{r, A, Xk, Xv, B1}, false}
            end;
       true ->
            case B of
                empty ->
                    {A, false};
                _ ->
                    {B1, {Mk, Mv}, Dec} = erase_min(B),
                    if Dec ->
                            unballeft(r, A, Mk, Mv, B1);
                       true ->
                            {{r, A, Mk, Mv, B1}, false}
                    end
            end
    end.

-spec erase_min(dictionary(K, V)) ->
                       {dictionary(K, V), {ec_dictionary:key(K), ec_dictionary:value(V)}, boolean()}.
erase_min({b, empty, Xk, Xv, empty}) ->
    {empty, {Xk, Xv}, true};
erase_min({b, empty, Xk, Xv, {r, A, Yk, Yv, B}}) ->
    {{b, A, Yk, Yv, B}, {Xk, Xv}, false};
erase_min({b, empty, _, _, {b, _, _, _, _}}) ->
    exit(boom);
erase_min({r, empty, Xk, Xv, A}) ->
    {A, {Xk, Xv}, false};
erase_min({b, A, Xk, Xv, B}) ->
    {A1, Min, Dec} = erase_min(A),
    if Dec ->
            {T, Dec1} = unbalright(b, A1, Xk, Xv, B),
            {T, Min, Dec1};
       true -> {{b, A1, Xk, Xv, B}, Min, false}
    end;
erase_min({r, A, Xk, Xv, B}) ->
    {A1, Min, Dec} = erase_min(A),
    if Dec ->
            {T, Dec1} = unbalright(r, A1, Xk, Xv, B),
            {T, Min, Dec1};
       true -> {{r, A1, Xk, Xv, B}, Min, false}
    end.

blackify({r, A, K, V, B}) -> {{b, A, K, V, B}, false};
blackify(Node) -> {Node, true}.

unballeft(r, {b, A, Xk, Xv, B}, Yk, Yv, C) ->
    {lbalance(b, {r, A, Xk, Xv, B}, Yk, Yv, C), false};
unballeft(b, {b, A, Xk, Xv, B}, Yk, Yv, C) ->
    {lbalance(b, {r, A, Xk, Xv, B}, Yk, Yv, C), true};
unballeft(b, {r, A, Xk, Xv, {b, B, Yk, Yv, C}}, Zk, Zv,
          D) ->
    {{b, A, Xk, Xv,
      lbalance(b, {r, B, Yk, Yv, C}, Zk, Zv, D)},
     false}.

unbalright(r, A, Xk, Xv, {b, B, Yk, Yv, C}) ->
    {rbalance(b, A, Xk, Xv, {r, B, Yk, Yv, C}), false};
unbalright(b, A, Xk, Xv, {b, B, Yk, Yv, C}) ->
    {rbalance(b, A, Xk, Xv, {r, B, Yk, Yv, C}), true};
unbalright(b, A, Xk, Xv,
           {r, {b, B, Yk, Yv, C}, Zk, Zv, D}) ->
    {{b, rbalance(b, A, Xk, Xv, {r, B, Yk, Yv, C}), Zk, Zv,
      D},
     false}.

-spec fold(fun((ec_dictionary:key(K), ec_dictionary:value(V), any()) -> any()),
           any(), dictionary(K, V)) -> any().
fold(_, Acc, empty) -> Acc;
fold(F, Acc, {_, A, Xk, Xv, B}) ->
    fold(F, F(Xk, Xv, fold(F, Acc, B)), A).

add1(K, V, empty) -> {r, empty, K, V, empty};
add1(K, V, {C, Left, K1, V1, Right}) when K < K1 ->
    lbalance(C, add1(K, V, Left), K1, V1, Right);
add1(K, V, {C, Left, K1, V1, Right}) when K > K1 ->
    rbalance(C, Left, K1, V1, add1(K, V, Right));
add1(K, V, {C, L, _, _, R}) -> {C, L, K, V, R}.

size1(empty) -> 0;
size1({_, L, _, _, R}) -> size1(L) + size1(R) + 1.

to_list(empty, List) -> List;
to_list({_, A, Xk, Xv, B}, List) ->
    to_list(A, [{Xk, Xv} | to_list(B, List)]).

%% Balance a tree afer (possibly) adding a node to the left/right.
-spec lbalance(color(), dictionary(K, V),
               ec_dictionary:key(K), ec_dictionary:value(V),
               dictionary(K, V)) ->
                      dictionary(K, V).
lbalance(b, {r, {r, A, Xk, Xv, B}, Yk, Yv, C}, Zk, Zv,
         D) ->
    {r, {b, A, Xk, Xv, B}, Yk, Yv, {b, C, Zk, Zv, D}};
lbalance(b, {r, A, Xk, Xv, {r, B, Yk, Yv, C}}, Zk, Zv,
         D) ->
    {r, {b, A, Xk, Xv, B}, Yk, Yv, {b, C, Zk, Zv, D}};
lbalance(C, A, Xk, Xv, B) -> {C, A, Xk, Xv, B}.

-spec rbalance(color(), dictionary(K, V),
               ec_dictionary:key(K), ec_dictionary:value(V),
               dictionary(K, V)) ->
                      dictionary(K, V).
rbalance(b, A, Xk, Xv,
         {r, {r, B, Yk, Yv, C}, Zk, Zv, D}) ->
    {r, {b, A, Xk, Xv, B}, Yk, Yv, {b, C, Zk, Zv, D}};
rbalance(b, A, Xk, Xv,
         {r, B, Yk, Yv, {r, C, Zk, Zv, D}}) ->
    {r, {b, A, Xk, Xv, B}, Yk, Yv, {b, C, Zk, Zv, D}};
rbalance(C, A, Xk, Xv, B) -> {C, A, Xk, Xv, B}.
