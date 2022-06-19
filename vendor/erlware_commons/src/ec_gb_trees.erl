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

%%%===================================================================
%%% Tests
%%%===================================================================


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% For me unit testing initially is about covering the obvious case. A
%% check to make sure that what you expect the tested functionality to
%% do, it actually does. As time goes on and people detect bugs you
%% add tests for those specific problems to the unit test suit.
%%
%% However, when getting started you can only test your basic
%% expectations. So here are the expectations I have for the add
%% functionality.
%%
%% 1) I can put arbitrary terms into the dictionary as keys
%% 2) I can put arbitrary terms into the dictionary as values
%% 3) When I put a value in the dictionary by a key, I can retrieve
%% that same value
%% 4) When I put a different value in the dictionary by key it does
%% not change other key value pairs.
%% 5) When I update a value the new value in available by the new key
%% 6) When a value does not exist a not found exception is created

add_test() ->
    Dict0 = ec_dictionary:new(ec_gb_trees),

    Key1 = foo,
    Key2 = [1, 3],
    Key3 = {"super"},
    Key4 = <<"fabulous">>,
    Key5 = {"Sona", 2, <<"Zuper">>},

    Value1 = Key5,
    Value2 = Key4,
    Value3 = Key2,
    Value4 = Key3,
    Value5 = Key1,

    Dict01 = ec_dictionary:add(Key1, Value1, Dict0),
    Dict02 = ec_dictionary:add(Key3, Value3,
                               ec_dictionary:add(Key2, Value2,
                                                 Dict01)),
    Dict1 =
        ec_dictionary:add(Key5, Value5,
                          ec_dictionary:add(Key4, Value4,
                                            Dict02)),

    ?assertMatch(Value1, ec_dictionary:get(Key1, Dict1)),
    ?assertMatch(Value2, ec_dictionary:get(Key2, Dict1)),
    ?assertMatch(Value3, ec_dictionary:get(Key3, Dict1)),
    ?assertMatch(Value4, ec_dictionary:get(Key4, Dict1)),
    ?assertMatch(Value5, ec_dictionary:get(Key5, Dict1)),


    Dict2 = ec_dictionary:add(Key3, Value5,
                              ec_dictionary:add(Key2, Value4, Dict1)),


    ?assertMatch(Value1, ec_dictionary:get(Key1, Dict2)),
    ?assertMatch(Value4, ec_dictionary:get(Key2, Dict2)),
    ?assertMatch(Value5, ec_dictionary:get(Key3, Dict2)),
    ?assertMatch(Value4, ec_dictionary:get(Key4, Dict2)),
    ?assertMatch(Value5, ec_dictionary:get(Key5, Dict2)),


    ?assertThrow(not_found, ec_dictionary:get(should_blow_up, Dict2)),
    ?assertThrow(not_found, ec_dictionary:get("This should blow up too",
                                              Dict2)).



-endif.
