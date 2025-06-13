%%% vi:ts=4 sw=4 et
%%%-------------------------------------------------------------------
%%% @author Eric Merritt <ericbmerritt@gmail.com>
%%% @copyright 2011 Erlware, LLC.
%%% @doc
%%% This provides an implementation of the type ec_dictionary using
%%% gb_trees as a backin
%%% see ec_dictionary
%%% see gb_trees
%%% @end
%%%-------------------------------------------------------------------
-module(ec_gb_trees).

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

%%%===================================================================
%%% API
%%%===================================================================

%% @doc create a new dictionary object from the specified module. The
%% module should implement the dictionary behaviour. In the clause
%% where an existing object is passed in new empty dictionary of the
%% same implementation is created and returned.
%%
%% @param ModuleName|Object The module name or existing dictionary object.
-spec new() -> gb_trees:tree(_K, _V).
new() ->
    gb_trees:empty().

%% @doc check to see if the dictionary provided has the specified key.
%%
%% @param Object The dictory object to check
%% @param Key The key to check the dictionary for
-spec has_key(ec_dictionary:key(K), Object::gb_trees:tree(K, _V)) -> boolean().
has_key(Key, Data) ->
    case gb_trees:lookup(Key, Data) of
        {value, _Val} ->
            true;
        none ->
            false
    end.

%% @doc given a key return that key from the dictionary. If the key is
%% not found throw a 'not_found' exception.
%%
%% @param Object The dictionary object to return the value from
%% @param Key The key requested
%% when the key does not exist @throws not_found
-spec get(ec_dictionary:key(K), Object::gb_trees:tree(K, V)) ->
    ec_dictionary:value(V).
get(Key, Data) ->
    case gb_trees:lookup(Key, Data) of
        {value, Value} ->
            Value;
        none ->
            throw(not_found)
    end.

-spec get(ec_dictionary:key(K),
          ec_dictionary:value(V),
          Object::gb_trees:tree(K, V)) ->
    ec_dictionary:value(V).
get(Key, Default, Data) ->
    case gb_trees:lookup(Key, Data) of
        {value, Value} ->
            Value;
        none ->
            Default
    end.

%% @doc add a new value to the existing dictionary. Return a new
%% dictionary containing the value.
%%
%% @param Object the dictionary object to add too
%% @param Key the key to add
%% @param Value the value to add
-spec add(ec_dictionary:key(K), ec_dictionary:value(V),
          Object::gb_trees:tree(K, V)) ->
    gb_trees:tree(K, V).
add(Key, Value, Data) ->
    gb_trees:enter(Key, Value, Data).

%% @doc Remove a value from the dictionary returning a new dictionary
%% with the value removed.
%%
%% @param Object the dictionary object to remove the value from
%% @param Key the key of the key/value pair to remove
-spec remove(ec_dictionary:key(K), Object::gb_trees:tree(K, V)) ->
    gb_trees:tree(K, V).
remove(Key, Data) ->
    gb_trees:delete_any(Key, Data).

%% @doc Check to see if the value exists in the dictionary
%%
%% @param Object the dictionary object to check
%% @param Value The value to check if exists
-spec has_value(ec_dictionary:value(V), Object::gb_trees:tree(_K, V)) -> boolean().
has_value(Value, Data) ->
    lists:member(Value, gb_trees:values(Data)).

%% @doc return the current number of key value pairs in the dictionary
%%
%% @param Object the object return the size for.
-spec size(Object::gb_trees:tree(_K, _V)) -> non_neg_integer().
size(Data) ->
    gb_trees:size(Data).

-spec to_list(gb_trees:tree(K, V)) -> [{ec_dictionary:key(K),
                                        ec_dictionary:value(V)}].
to_list(Data) ->
    gb_trees:to_list(Data).

-spec from_list([{ec_dictionary:key(K), ec_dictionary:value(V)}]) ->
    gb_trees:tree(K, V).
from_list(List) when is_list(List) ->
    lists:foldl(fun({Key, Value}, Dict) ->
                        gb_trees:enter(Key, Value, Dict)
                end,
                gb_trees:empty(),
                List).

-spec keys(gb_trees:tree(K,_V)) -> [ec_dictionary:key(K)].
keys(Data) ->
    gb_trees:keys(Data).
