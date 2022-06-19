%%% vi:ts=4 sw=4 et
%%%-------------------------------------------------------------------
%%% @author Eric Merritt <ericbmerritt@gmail.com>
%%% @copyright 2011 Erlware, LLC.
%%% @doc
%%%  This provides an implementation of the ec_dictionary type using
%%%  erlang orddicts as a base. The function documentation for
%%%  ec_dictionary applies here as well.
%%% see ec_dictionary
%%% see orddict
%%% @end
%%%-------------------------------------------------------------------
-module(ec_orddict).

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
-type dictionary(K, V) :: [{K, V}].

%%%===================================================================
%%% API
%%%===================================================================

-spec new() -> dictionary(_K, _V).
new() ->
    orddict:new().

-spec has_key(ec_dictionary:key(K), Object::dictionary(K, _V)) -> boolean().
has_key(Key, Data) ->
    orddict:is_key(Key, Data).

-spec get(ec_dictionary:key(K), Object::dictionary(K, V)) ->
                 ec_dictionary:value(V).
get(Key, Data) ->
    case orddict:find(Key, Data) of
        {ok, Value} ->
            Value;
        error ->
            throw(not_found)
    end.

-spec get(ec_dictionary:key(K),
          Default::ec_dictionary:value(V),
          Object::dictionary(K, V)) ->
                 ec_dictionary:value(V).
get(Key, Default, Data) ->
    case orddict:find(Key, Data) of
        {ok, Value} ->
            Value;
        error ->
            Default
    end.

-spec add(ec_dictionary:key(K), ec_dictionary:value(V),
          Object::dictionary(K, V)) ->
                 dictionary(K, V).
add(Key, Value, Data) ->
    orddict:store(Key, Value, Data).

-spec remove(ec_dictionary:key(K), Object::dictionary(K, V)) ->
                    dictionary(K, V).
remove(Key, Data) ->
    orddict:erase(Key, Data).

-spec has_value(ec_dictionary:value(V), Object::dictionary(_K, V)) -> boolean().
has_value(Value, Data) ->
    orddict:fold(fun(_, NValue, _) when NValue == Value ->
                         true;
                    (_, _, Acc) ->
                         Acc
                 end,
                 false,
                 Data).

-spec size(Object::dictionary(_K, _V)) -> non_neg_integer().
size(Data) ->
    orddict:size(Data).

-spec to_list(dictionary(K, V)) ->
                     [{ec_dictionary:key(K), ec_dictionary:value(V)}].
to_list(Data) ->
    orddict:to_list(Data).

-spec from_list([{ec_dictionary:key(K), ec_dictionary:value(V)}]) ->
                       dictionary(K, V).
from_list(List) when is_list(List) ->
    orddict:from_list(List).

-spec keys(dictionary(K, _V)) -> [ec_dictionary:key(K)].
keys(Dict) ->
    orddict:fetch_keys(Dict).
