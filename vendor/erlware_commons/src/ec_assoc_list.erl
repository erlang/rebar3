%%% vi:ts=4 sw=4 et
%%%-------------------------------------------------------------------
%%% @author Eric Merritt <ericbmerritt@gmail.com>
%%% @copyright 2011 Erlware, LLC.
%%% @doc
%%%  provides an implementation of ec_dictionary using an association
%%%  list as a basy
%%% see ec_dictionary
%%% @end
%%%-------------------------------------------------------------------
-module(ec_assoc_list).

-behaviour(ec_dictionary).

%% API
-export([new/0,
         has_key/2,
         get/2,
         get/3,
         add/3,
         remove/2,
         has_value/2,
         size/1,
         to_list/1,
         from_list/1,
         keys/1]).

-export_type([dictionary/2]).

%%%===================================================================
%%% Types
%%%===================================================================
%% This should be opaque, but that kills dialyzer so for now we export it
%% however you should not rely on the internal representation here
-type dictionary(K, V) :: {ec_assoc_list,
                           [{ec_dictionary:key(K), ec_dictionary:value(V)}]}.

%%%===================================================================
%%% API
%%%===================================================================

-spec new() -> dictionary(_K, _V).
new() ->
    {ec_assoc_list, []}.

-spec has_key(ec_dictionary:key(K), Object::dictionary(K, _V)) -> boolean().
has_key(Key, {ec_assoc_list, Data}) ->
    lists:keymember(Key, 1, Data).

-spec get(ec_dictionary:key(K), Object::dictionary(K, V)) ->
                 ec_dictionary:value(V).
get(Key, {ec_assoc_list, Data}) ->
    case lists:keyfind(Key, 1, Data) of
        {Key, Value} ->
            Value;
        false ->
            throw(not_found)
    end.

-spec get(ec_dictionary:key(K),
          ec_dictionary:value(V),
          Object::dictionary(K, V)) ->
                 ec_dictionary:value(V).
get(Key, Default, {ec_assoc_list, Data}) ->
    case lists:keyfind(Key, 1, Data) of
        {Key, Value} ->
            Value;
        false ->
            Default
    end.

-spec add(ec_dictionary:key(K), ec_dictionary:value(V),
          Object::dictionary(K, V)) ->
                 dictionary(K, V).
add(Key, Value, {ec_assoc_list, _Data}=Dict) ->
    {ec_assoc_list, Rest} = remove(Key,Dict),
    {ec_assoc_list, [{Key, Value} | Rest ]}.

-spec remove(ec_dictionary:key(K), Object::dictionary(K, _V)) ->
                    dictionary(K, _V).
remove(Key, {ec_assoc_list, Data}) ->
    {ec_assoc_list, lists:keydelete(Key, 1, Data)}.

-spec has_value(ec_dictionary:value(V), Object::dictionary(_K, V)) -> boolean().
has_value(Value, {ec_assoc_list, Data}) ->
    lists:keymember(Value, 2, Data).

-spec size(Object::dictionary(_K, _V)) -> non_neg_integer().
size({ec_assoc_list, Data}) ->
    length(Data).

-spec to_list(dictionary(K, V)) -> [{ec_dictionary:key(K),
                                     ec_dictionary:value(V)}].
to_list({ec_assoc_list, Data}) ->
    Data.

-spec from_list([{ec_dictionary:key(K), ec_dictionary:value(V)}]) ->
                       dictionary(K, V).
from_list(List) when is_list(List) ->
    {ec_assoc_list, List}.

-spec keys(dictionary(K, _V)) -> [ec_dictionary:key(K)].
keys({ec_assoc_list, Data}) ->
    lists:map(fun({Key, _Value}) ->
                      Key
              end, Data).
