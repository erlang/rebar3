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
%% value is returend. If processing reaches the end of the list and
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

%%%===================================================================
%%% Test Functions
%%%===================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

find1_test() ->
    TestData = [1, 2, 3, 4, 5, 6],
    Result = find(fun(5) ->
                          true;
                     (_) ->
                          false
                  end,
                  TestData),
    ?assertMatch({ok, 5}, Result),

    Result2 = find(fun(37) ->
                           true;
                      (_) ->
                           false
                   end,
                   TestData),
    ?assertMatch(error, Result2).

find2_test() ->
    TestData = ["one", "two", "three", "four", "five", "six"],
    Result = find(fun("five") ->
                          true;
                     (_) ->
                          false
                  end,
                  TestData),
    ?assertMatch({ok, "five"}, Result),

    Result2 = find(fun(super_duper) ->
                           true;
                      (_) ->
                           false
                   end,
                   TestData),
    ?assertMatch(error, Result2).



find3_test() ->
    TestData = [{"one", 1}, {"two", 2}, {"three", 3}, {"four", 5}, {"five", 5},
                {"six", 6}],
    Result = find(fun({"one", 1}) ->
                          true;
                     (_) ->
                          false
                  end,
                  TestData),
    ?assertMatch({ok, {"one", 1}}, Result),

    Result2 = find(fun([fo, bar, baz]) ->
                           true;
                      ({"onehundred", 100}) ->
                           true;
                      (_) ->
                           false
                   end,
                   TestData),
    ?assertMatch(error, Result2).



fetch1_test() ->
    TestData = [1, 2, 3, 4, 5, 6],
    Result = fetch(fun(5) ->
                           true;
                      (_) ->
                           false
                   end,
                   TestData),
    ?assertMatch(5, Result),

    ?assertThrow(not_found,
                 fetch(fun(37) ->
                               true;
                          (_) ->
                               false
                       end,
                       TestData)).

fetch2_test() ->
    TestData = ["one", "two", "three", "four", "five", "six"],
    Result = fetch(fun("five") ->
                           true;
                      (_) ->
                           false
                   end,
                   TestData),
    ?assertMatch("five", Result),

    ?assertThrow(not_found,
                 fetch(fun(super_duper) ->
                               true;
                          (_) ->
                               false
                       end,
                       TestData)).

fetch3_test() ->
    TestData = [{"one", 1}, {"two", 2}, {"three", 3}, {"four", 5}, {"five", 5},
                {"six", 6}],
    Result = fetch(fun({"one", 1}) ->
                           true;
                      (_) ->
                           false
                   end,
                   TestData),
    ?assertMatch({"one", 1}, Result),

    ?assertThrow(not_found,
                 fetch(fun([fo, bar, baz]) ->
                               true;
                          ({"onehundred", 100}) ->
                               true;
                          (_) ->
                               false
                       end,
                       TestData)).

search1_test() ->
    TestData = [1, 2, 3, 4, 5, 6],
    Result = search(fun(5) ->
                            {ok, 5};
                       (_) ->
                            not_found
                    end,
                    TestData),
    ?assertMatch({ok, 5, 5}, Result),

    Result2 = search(fun(37) ->
                             {ok, 37};
                        (_) ->
                             not_found
                     end,
                     TestData),
    ?assertMatch(not_found, Result2).

search2_test() ->
    TestData = [1, 2, 3, 4, 5, 6],
    Result = search(fun(1) ->
                            {ok, 10};
                       (_) ->
                            not_found
                    end,
                    TestData),
    ?assertMatch({ok, 10, 1}, Result),

    Result2 = search(fun(6) ->
                             {ok, 37};
                        (_) ->
                             not_found
                     end,
                     TestData),
    ?assertMatch({ok, 37, 6}, Result2).

search3_test() ->
    TestData = [1, 2, 3, 4, 5, 6],
    Result = search(fun(10) ->
                            {ok, 10};
                       (_) ->
                            not_found
                    end,
                    TestData),
    ?assertMatch(not_found, Result),

    Result2 = search(fun(-1) ->
                             {ok, 37};
                        (_) ->
                             not_found
                     end,
                     TestData),
    ?assertMatch(not_found, Result2).

-endif.
