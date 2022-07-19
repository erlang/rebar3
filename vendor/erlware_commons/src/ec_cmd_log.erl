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
%%% @doc This provides simple output functions for command line apps. You should
%%% use this to talk to the users if you are wrting code for the system
-module(ec_cmd_log).

%% Avoid clashing with `error/3` BIF added in Erlang/OTP 24
-compile({no_auto_import,[error/3]}).

-export([new/1,
         new/2,
         new/3,
         log/4,
         should/2,
         debug/2,
         debug/3,
         info/2,
         info/3,
         error/2,
         error/3,
         warn/2,
         warn/3,
         log_level/1,
         atom_log_level/1,
         format/1]).

-include("ec_cmd_log.hrl").

-define(RED,     $r).
-define(GREEN,   $g).
-define(YELLOW,  $y).
-define(BLUE,    $b).
-define(MAGENTA, $m).
-define(CYAN,    $c).

-define(PREFIX, "===> ").

-record(state_t, {log_level=0 :: int_log_level(),
                  caller=api :: caller(),
                  intensity=low :: none | low | high}).

%%============================================================================
%% types
%%============================================================================
-export_type([t/0,
              int_log_level/0,
              atom_log_level/0,
              log_level/0,
              caller/0,
              log_fun/0]).

-type caller() :: api | command_line.

-type log_level() :: int_log_level() | atom_log_level().

-type int_log_level() :: 0..3.

-type atom_log_level() :: error | warn | info | debug.

-type intensity() :: none | low | high.

-type log_fun() :: fun(() -> iolist()).

-type color() :: char().

-opaque t() :: #state_t{}.

%%============================================================================
%% API
%%============================================================================
%% @doc Create a new 'log level' for the system
-spec new(log_level()) -> t().
new(LogLevel) ->
    new(LogLevel, api).

-spec new(log_level(), caller()) -> t().
new(LogLevel, Caller) ->
    new(LogLevel, Caller, high).


-spec new(log_level(), caller(), intensity()) -> t().
new(LogLevel, Caller, Intensity) when (Intensity =:= none orelse
                                       Intensity =:= low orelse
                                       Intensity =:= high),
                                      LogLevel >= 0, LogLevel =< 3 ->
    #state_t{log_level=LogLevel, caller=Caller,
             intensity=Intensity};
new(AtomLogLevel, Caller, Intensity)
  when AtomLogLevel =:= error;
       AtomLogLevel =:= warn;
       AtomLogLevel =:= info;
       AtomLogLevel =:= debug ->
    LogLevel = case AtomLogLevel of
                   error -> 0;
                   warn -> 1;
                   info -> 2;
                   debug -> 3
               end,
    new(LogLevel, Caller, Intensity).


%% @doc log at the debug level given the current log state with a string or
%% function that returns a string
-spec debug(t(), string() | log_fun()) -> ok.
debug(LogState, Fun)
  when erlang:is_function(Fun) ->
    log(LogState, ?EC_DEBUG, fun() ->
                                     colorize(LogState, ?CYAN, false, Fun())
                             end);
debug(LogState, String) ->
    debug(LogState, "~s~n", [String]).

%% @doc log at the debug level given the current log state with a format string
%% and argements @see io:format/2
-spec debug(t(), string(), [any()]) -> ok.
debug(LogState, FormatString, Args) ->
    log(LogState, ?EC_DEBUG, colorize(LogState, ?CYAN, false, FormatString), Args).

%% @doc log at the info level given the current log state with a string or
%% function that returns a string
-spec info(t(), string() | log_fun()) -> ok.
info(LogState, Fun)
    when erlang:is_function(Fun) ->
    log(LogState, ?EC_INFO, fun() ->
                                    colorize(LogState, ?GREEN, false, Fun())
                            end);
info(LogState, String) ->
    info(LogState, "~s~n", [String]).

%% @doc log at the info level given the current log state with a format string
%% and argements @see io:format/2
-spec info(t(), string(), [any()]) -> ok.
info(LogState, FormatString, Args) ->
    log(LogState, ?EC_INFO, colorize(LogState, ?GREEN, false, FormatString), Args).

%% @doc log at the error level given the current log state with a string or
%% format string that returns a function
-spec error(t(), string() | log_fun()) -> ok.
error(LogState, Fun)
    when erlang:is_function(Fun) ->
    log(LogState, ?EC_ERROR, fun() ->
                                     colorize(LogState, ?RED, false, Fun())
                             end);
error(LogState, String) ->
    error(LogState, "~s~n", [String]).

%% @doc log at the error level given the current log state with a format string
%% and argements @see io:format/2
-spec error(t(), string(), [any()]) -> ok.
error(LogState, FormatString, Args) ->
    log(LogState, ?EC_ERROR, colorize(LogState, ?RED, false, FormatString), Args).

%% @doc log at the warn level given the current log state with a string or
%% format string that returns a function
-spec warn(t(), string() | log_fun()) -> ok.
warn(LogState, Fun)
    when erlang:is_function(Fun) ->
    log(LogState, ?EC_WARN, fun() -> colorize(LogState, ?MAGENTA, false, Fun()) end);
warn(LogState, String) ->
    warn(LogState, "~s~n", [String]).

%% @doc log at the warn level given the current log state with a format string
%% and argements @see io:format/2
-spec warn(t(), string(), [any()]) -> ok.
warn(LogState, FormatString, Args) ->
    log(LogState, ?EC_WARN, colorize(LogState, ?MAGENTA, false, FormatString), Args).

%% @doc Execute the fun passed in if log level is as expected.
-spec log(t(), int_log_level(), log_fun()) -> ok.
log(#state_t{log_level=DetailLogLevel}, LogLevel, Fun)
    when DetailLogLevel >= LogLevel ->
    io:format("~s~n", [Fun()]);
log(_, _, _) ->
    ok.

%% @doc when the module log level is less then or equal to the log level for the
%% call then write the log info out. When its not then ignore the call.
-spec log(t(), int_log_level(), string(), [any()]) -> ok.
log(#state_t{log_level=DetailLogLevel}, LogLevel, FormatString, Args)
  when DetailLogLevel >= LogLevel,
       erlang:is_list(Args) ->
    io:format(FormatString, Args);
log(_, _, _, _) ->
    ok.

%% @doc return a boolean indicating if the system should log for the specified
%% levelg
-spec should(t(), int_log_level() | any()) -> boolean().
should(#state_t{log_level=DetailLogLevel}, LogLevel)
  when DetailLogLevel >= LogLevel ->
    true;
should(_, _) ->
    false.

%% @doc get the current log level as an integer
-spec log_level(t()) -> int_log_level().
log_level(#state_t{log_level=DetailLogLevel}) ->
    DetailLogLevel.

%% @doc get the current log level as an atom
-spec atom_log_level(t()) -> atom_log_level().
atom_log_level(#state_t{log_level=?EC_ERROR}) ->
    error;
atom_log_level(#state_t{log_level=?EC_WARN}) ->
    warn;
atom_log_level(#state_t{log_level=?EC_INFO}) ->
    info;
atom_log_level(#state_t{log_level=?EC_DEBUG}) ->
    debug.

-spec format(t()) -> iolist().
format(Log) ->
    [<<"(">>,
     ec_cnv:to_binary(log_level(Log)), <<":">>,
     ec_cnv:to_binary(atom_log_level(Log)),
     <<")">>].

-spec colorize(t(), color(), boolean(), string()) -> string().

-define(VALID_COLOR(C),
        C =:= $r orelse C =:= $g orelse C =:= $y orelse
        C =:= $b orelse C =:= $m orelse C =:= $c orelse
        C =:= $R orelse C =:= $G orelse C =:= $Y orelse
        C =:= $B orelse C =:= $M orelse C =:= $C).

colorize(#state_t{intensity=none}, _, _, Msg) ->
                Msg;
%% When it is suposed to be bold and we already have a uppercase
%% (bold color) we don't need to modify the color
colorize(State, Color, true, Msg)  when ?VALID_COLOR(Color),
                                        Color >= $A, Color =< $Z ->
    colorize(State, Color, false, Msg);
%% We're sneaky we can substract 32 to get the uppercase character if we want
%% bold but have a non bold color.
colorize(State, Color, true, Msg) when ?VALID_COLOR(Color) ->
    colorize(State, Color - 32, false, Msg);
colorize(#state_t{caller=command_line, intensity = high},
         Color, false, Msg) when ?VALID_COLOR(Color) ->
    lists:flatten(cf:format("~!" ++ [Color] ++"~s~s", [?PREFIX, Msg]));
colorize(#state_t{caller=command_line, intensity = low},
         Color, false, Msg) when ?VALID_COLOR(Color) ->
    lists:flatten(cf:format("~!" ++ [Color] ++"~s~!!~s", [?PREFIX, Msg]));
colorize(_LogState, _Color, _Bold, Msg) ->
    Msg.

%%%===================================================================
%%% Test Functions
%%%===================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

should_test() ->
    ErrorLogState = new(error),
    ?assertMatch(true, should(ErrorLogState, ?EC_ERROR)),
    ?assertMatch(true, not should(ErrorLogState, ?EC_INFO)),
    ?assertMatch(true, not should(ErrorLogState, ?EC_DEBUG)),
    ?assertEqual(?EC_ERROR, log_level(ErrorLogState)),
    ?assertEqual(error, atom_log_level(ErrorLogState)),

    InfoLogState = new(info),
    ?assertMatch(true, should(InfoLogState, ?EC_ERROR)),
    ?assertMatch(true, should(InfoLogState, ?EC_INFO)),
    ?assertMatch(true, not should(InfoLogState, ?EC_DEBUG)),
    ?assertEqual(?EC_INFO, log_level(InfoLogState)),
    ?assertEqual(info, atom_log_level(InfoLogState)),

    DebugLogState = new(debug),
    ?assertMatch(true, should(DebugLogState, ?EC_ERROR)),
    ?assertMatch(true, should(DebugLogState, ?EC_INFO)),
    ?assertMatch(true, should(DebugLogState, ?EC_DEBUG)),
    ?assertEqual(?EC_DEBUG, log_level(DebugLogState)),
    ?assertEqual(debug, atom_log_level(DebugLogState)).


no_color_test() ->
    LogState = new(debug, command_line, none),
    ?assertEqual("test",
                 colorize(LogState, ?RED, true, "test")).

color_test() ->
    LogState = new(debug, command_line, high),
    ?assertEqual("\e[1;31m===> test\e[0m",
                 colorize(LogState, ?RED, true, "test")).
-endif.
