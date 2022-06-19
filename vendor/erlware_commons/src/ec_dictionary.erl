%%% vi:ts=4 sw=4 et
%%%-------------------------------------------------------------------
%%% @author Eric Merritt <ericbmerritt@gmail.com>
%%% @copyright 2011 Erlware, LLC.
%%% @doc
%%%  A module that supports association of keys to values. A map cannot
%%%  contain duplicate keys; each key can map to at most one value.
%%%
%%%  This interface is a member of the Erlware Commons Library.
%%% @end
%%%-------------------------------------------------------------------
-module(ec_dictionary).

%% API
-export([new/1,
         has_key/2,
         get/2,
         get/3,
         add/3,
         remove/2,
         has_value/2,
         size/1,
         to_list/1,
         from_list/2,
         keys/1]).

-export_type([dictionary/2,
              key/1,
              value/1]).

%%%===================================================================
%%% Types
%%%===================================================================

-record(dict_t,
        {callback,
          data}).

%% This should be opaque, but that kills dialyzer so for now we export it
%% however you should not rely on the internal representation here
-type dictionary(_K, _V) :: #dict_t{}.
-type key(T) :: T.
-type value(T) :: T.

-ifdef(have_callback_support).

-callback new() -> any().
-callback has_key(key(any()), any()) -> boolean().
-callback get(key(any()), any()) -> any().
-callback add(key(any()), value(any()), T) -> T.
-callback remove(key(any()), T) -> T.
-callback has_value(value(any()), any()) -> boolean().
-callback size(any()) -> non_neg_integer().
-callback to_list(any()) -> [{key(any()), value(any())}].
-callback from_list([{key(any()), value(any())}]) -> any().
-callback keys(any()) -> [key(any())].

-else.

%% In the case where R14 or lower is being used to compile the system
%% we need to export a behaviour info
-export([behaviour_info/1]).
-spec behaviour_info(atom()) -> [{atom(), arity()}] | undefined.
behaviour_info(callbacks) ->
    [{new, 0},
     {has_key, 2},
     {get, 2},
     {add, 3},
     {remove, 2},
     {has_value, 2},
     {size, 1},
     {to_list, 1},
     {from_list, 1},
     {keys, 1}];
behaviour_info(_Other) ->
    undefined.
-endif.

%%%===================================================================
%%% API
%%%===================================================================

%% @doc create a new dictionary object from the specified module. The
%% module should implement the dictionary behaviour.
%%
%% @param ModuleName The module name.
-spec new(module()) -> dictionary(_K, _V).
new(ModuleName) when is_atom(ModuleName) ->
    #dict_t{callback = ModuleName, data = ModuleName:new()}.

%% @doc check to see if the dictionary provided has the specified key.
%%
%% @param Dict The dictory object to check
%% @param Key The key to check the dictionary for
-spec has_key(key(K), dictionary(K, _V)) -> boolean().
has_key(Key, #dict_t{callback = Mod, data = Data}) ->
    Mod:has_key(Key, Data).

%% @doc given a key return that key from the dictionary. If the key is
%% not found throw a 'not_found' exception.
%%
%% @param Dict The dictionary object to return the value from
%% @param Key The key requested
%%  when the key does not exist @throws not_found
-spec get(key(K), dictionary(K, V)) -> value(V).
get(Key, #dict_t{callback = Mod, data = Data}) ->
    Mod:get(Key, Data).

%% @doc given a key return that key from the dictionary. If the key is
%% not found then the default value is returned.
%%
%% @param Dict The dictionary object to return the value from
%% @param Key The key requested
%% @param Default The value that will be returned if no value is found
%% in the database.
-spec get(key(K), value(V), dictionary(K, V)) -> value(V).
get(Key, Default, #dict_t{callback = Mod, data = Data}) ->
    Mod:get(Key, Default, Data).

%% @doc add a new value to the existing dictionary. Return a new
%% dictionary containing the value.
%%
%% @param Dict the dictionary object to add too
%% @param Key the key to add
%% @param Value the value to add
-spec add(key(K), value(V), dictionary(K, V)) -> dictionary(K, V).
add(Key, Value, #dict_t{callback = Mod, data = Data} = Dict) ->
    Dict#dict_t{data = Mod:add(Key, Value, Data)}.

%% @doc Remove a value from the dictionary returning a new dictionary
%% with the value removed.
%%
%% @param Dict the dictionary object to remove the value from
%% @param Key the key of the key/value pair to remove
-spec remove(key(K), dictionary(K, V)) -> dictionary(K, V).
remove(Key, #dict_t{callback = Mod, data = Data} = Dict) ->
    Dict#dict_t{data = Mod:remove(Key, Data)}.

%% @doc Check to see if the value exists in the dictionary
%%
%% @param Dict the dictionary object to check
%% @param Value The value to check if exists
-spec has_value(value(V), dictionary(_K, V)) -> boolean().
has_value(Value, #dict_t{callback = Mod, data = Data}) ->
    Mod:has_value(Value, Data).

%% @doc return the current number of key value pairs in the dictionary
%%
%% @param Dict the object return the size for.
-spec size(dictionary(_K, _V)) -> integer().
size(#dict_t{callback = Mod, data = Data}) ->
    Mod:size(Data).

%% @doc Return the contents of this dictionary as a list of key value
%% pairs.
%%
%% @param Dict the base dictionary to make use of.
-spec to_list(Dict::dictionary(K, V)) -> [{key(K), value(V)}].
to_list(#dict_t{callback = Mod, data = Data}) ->
    Mod:to_list(Data).

%% @doc Create a new dictionary, of the specified implementation using
%% the list provided as the starting contents.
%%
%% @param ModuleName the type to create the dictionary from
%% @param List The list of key value pairs to start with
-spec from_list(module(), [{key(K), value(V)}]) -> dictionary(K, V).
from_list(ModuleName, List) when is_list(List) ->
    #dict_t{callback = ModuleName, data = ModuleName:from_list(List)}.

%% @doc Return the keys of this dictionary as a list
%%
%% @param Dict the base dictionary to make use of.
-spec keys(Dict::dictionary(K, _V)) -> [key(K)].
keys(#dict_t{callback = Mod, data = Data}) ->
    Mod:keys(Data).
