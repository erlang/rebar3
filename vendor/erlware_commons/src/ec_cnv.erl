%% -*- erlang-indent-level: 4; indent-tabs-mode: nil; fill-column: 80 -*-
%%% Copyright 2012 Erlware, LLC. All Rights Reserved.
%%%
%%% This file is provided to you under the Apache License,
%%% Version 2.0 (the "License"); you may not use this file
%%% except in compliance with the License.  You may obtain
%%% a copy of the License at
%%%
%%%   http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing,
%%% software distributed under the License is distributed on an
%%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%%% KIND, either express or implied.  See the License for the
%%% specific language governing permissions and limitations
%%% under the License.
%%%---------------------------------------------------------------------------
%%% @author Eric Merritt <ericbmerritt@gmail.com>
%%% @copyright (C) 2012 Erlware, LLC.
%%%
-module(ec_cnv).

%% API
-export([to_integer/1,
         to_integer/2,
         to_float/1,
         to_float/2,
         to_number/1,
         to_list/1,
         to_binary/1,
         to_atom/1,
         to_boolean/1,
         is_true/1,
         is_false/1]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc
%% Automatic conversion of a term into integer type.  The conversion
%% will round a float value if nonstrict is specified otherwise badarg
-spec to_integer(string() | binary() | integer() | float()) ->
                        integer().
to_integer(X)->
    to_integer(X, nonstrict).

-spec to_integer(string() | binary() | integer() | float(),
                 strict | nonstrict) ->
                        integer().
to_integer(X, strict)
  when erlang:is_float(X) ->
    erlang:error(badarg);
to_integer(X, nonstrict)
  when erlang:is_float(X) ->
    erlang:round(X);
to_integer(X, S)
  when erlang:is_binary(X) ->
    to_integer(erlang:binary_to_list(X), S);
to_integer(X, S)
  when erlang:is_list(X) ->
    try erlang:list_to_integer(X) of
        Result ->
            Result
    catch
        error:badarg when S =:= nonstrict ->
            erlang:round(erlang:list_to_float(X))
    end;
to_integer(X, _)
  when erlang:is_integer(X) ->
    X.

%% @doc
%% Automatic conversion of a term into float type. badarg if strict
%% is defined and an integer value is passed.
-spec to_float(string() | binary() | integer() | float()) ->
                      float().
to_float(X) ->
    to_float(X, nonstrict).

-spec to_float(string() | binary() | integer() | float(),
               strict | nonstrict) ->
                       float().
to_float(X, S) when is_binary(X) ->
    to_float(erlang:binary_to_list(X), S);
to_float(X, S)
  when erlang:is_list(X) ->
    try erlang:list_to_float(X) of
        Result ->
            Result
    catch
        error:badarg when S =:= nonstrict ->
            erlang:list_to_integer(X) * 1.0
    end;
to_float(X, strict) when
      erlang:is_integer(X) ->
    erlang:error(badarg);
to_float(X, nonstrict)
  when erlang:is_integer(X) ->
    X * 1.0;
to_float(X, _) when erlang:is_float(X) ->
    X.

%% @doc
%% Automatic conversion of a term into number type.
-spec to_number(binary() | string() | number()) ->
                       number().
to_number(X)
  when erlang:is_number(X) ->
    X;
to_number(X)
  when erlang:is_binary(X) ->
    to_number(to_list(X));
to_number(X)
  when erlang:is_list(X) ->
    try list_to_integer(X) of
        Int -> Int
    catch
        error:badarg ->
            list_to_float(X)
    end.

%% @doc
%% Automatic conversion of a term into Erlang list
-spec to_list(atom() | list() | binary() | integer() | float()) ->
                     list().
to_list(X)
  when erlang:is_float(X) ->
    erlang:float_to_list(X);
to_list(X)
  when erlang:is_integer(X) ->
    erlang:integer_to_list(X);
to_list(X)
  when erlang:is_binary(X) ->
    erlang:binary_to_list(X);
to_list(X)
  when erlang:is_atom(X) ->
    erlang:atom_to_list(X);
to_list(X)
  when erlang:is_list(X) ->
    X.

%% @doc
%% Known limitations:
%%   Converting [256 | _], lists with integers > 255
-spec to_binary(atom() | string() | binary() | integer() | float()) ->
                       binary().
to_binary(X)
  when erlang:is_float(X) ->
    to_binary(to_list(X));
to_binary(X)
  when erlang:is_integer(X) ->
    erlang:iolist_to_binary(integer_to_list(X));
to_binary(X)
  when erlang:is_atom(X) ->
    erlang:list_to_binary(erlang:atom_to_list(X));
to_binary(X)
  when erlang:is_list(X) ->
    erlang:iolist_to_binary(X);
to_binary(X)
  when erlang:is_binary(X) ->
    X.

-spec to_boolean(binary() | string() | atom()) ->
                        boolean().
to_boolean(<<"true">>) ->
    true;
to_boolean("true") ->
    true;
to_boolean(true) ->
    true;
to_boolean(<<"false">>) ->
    false;
to_boolean("false") ->
    false;
to_boolean(false) ->
    false.

-spec is_true(binary() | string() | atom()) ->
                     boolean().
is_true(<<"true">>) ->
    true;
is_true("true") ->
    true;
is_true(true) ->
    true;
is_true(_) ->
    false.

-spec is_false(binary() | string() | atom()) ->
                      boolean().
is_false(<<"false">>) ->
    true;
is_false("false") ->
    true;
is_false(false) ->
    true;
is_false(_) ->
    false.

%% @doc
%% Automation conversion a term to an existing atom. badarg is
%% returned if the atom doesn't exist.  the safer version, won't let
%% you leak atoms
-spec to_atom(atom() | list() | binary() | integer() | float()) ->
                     atom().
to_atom(X)
  when erlang:is_atom(X) ->
    X;
to_atom(X)
  when erlang:is_list(X) ->
    erlang:list_to_existing_atom(X);
to_atom(X) ->
    to_atom(to_list(X)).

%%%===================================================================
%%% Tests
%%%===================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

to_integer_test() ->
    ?assertError(badarg, to_integer(1.5, strict)).

to_float_test() ->
    ?assertError(badarg, to_float(10, strict)).

to_atom_test() ->
    ?assertMatch(true, to_atom("true")),
    ?assertMatch(true, to_atom(<<"true">>)),
    ?assertMatch(false, to_atom(<<"false">>)),
    ?assertMatch(false, to_atom(false)),
    ?assertError(badarg, to_atom("hello_foo_bar_baz")),

    S = erlang:list_to_atom("1"),
    ?assertMatch(S, to_atom(1)).

to_boolean_test()->
    ?assertMatch(true, to_boolean(<<"true">>)),
    ?assertMatch(true, to_boolean("true")),
    ?assertMatch(true, to_boolean(true)),
    ?assertMatch(false, to_boolean(<<"false">>)),
    ?assertMatch(false, to_boolean("false")),
    ?assertMatch(false, to_boolean(false)).

-endif.
