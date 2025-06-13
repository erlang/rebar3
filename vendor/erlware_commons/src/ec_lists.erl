%%% vi:ts=4 sw=4 et
%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, Erlware LLC
%%% @doc
%%%  Provides useful functionionality on standard lists that are
%%%  not provided in the standard library.
%%% @end
%%%-------------------------------------------------------------------
-module(ec_lists).

%% API
-export([find/2,
         fetch/2,
         search/2]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Search each value in the list with the specified
%% function. When the function returns a value of {ok, term()} the
%% search function stops and returns a tuple of {ok, term(), term()},
%% where the second value is the term returned from the function and
%% the third value is the element passed to the function. The purpose
%% of this is to allow a list to be searched where some internal state
%% is important while the input element is not.
-spec search(fun(), list()) -> {ok, Result::term(), Element::term()} | not_found.
search(Fun, [H|T]) ->
    case Fun(H) of
        {ok, Value} ->
            {ok, Value, H};
        not_found ->
            search(Fun, T)
    end;
search(_, []) ->
    not_found.

%% @doc Find a value in the list with the specified function. If the
%% function returns the atom true, the value is returned as {ok,
%% term()} and processing is aborted, if the function returns false,
%% processing continues until the end of the list. If the end is found
%% and the function never returns true the atom error is returned.
-spec find(fun(), list()) -> {ok, term()} | error.
find(Fun, [Head|Tail]) when is_function(Fun) ->
    case Fun(Head) of
        true ->
            {ok, Head};
        false ->
            find(Fun, Tail)
    end;
find(_Fun, []) ->
    error.

%% @doc Fetch a value from the list. If the function returns true the
%% value is returned. If processing reaches the end of the list and
%% the function has never returned true an exception not_found is
%% thrown.
-spec fetch(fun(), list()) -> term().
fetch(Fun, List) when is_list(List), is_function(Fun) ->
    case find(Fun, List) of
        {ok, Head} ->
            Head;
        error ->
            throw(not_found)
    end.
