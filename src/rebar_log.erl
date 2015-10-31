%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2009 Dave Smith (dizzyd@dizzyd.com)
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%% -------------------------------------------------------------------
-module(rebar_log).

-export([init/2,
         set_level/1,
         error_level/0,
         default_level/0,
         log/3,
         is_verbose/1]).

-define(ERROR_LEVEL, 0).
-define(WARN_LEVEL,  1).
-define(INFO_LEVEL,  2).
-define(DEBUG_LEVEL, 3).

%% ===================================================================
%% Public API
%% ===================================================================

init(Caller, Verbosity) ->
    Level = case valid_level(Verbosity) of
                ?ERROR_LEVEL -> error;
                ?WARN_LEVEL  -> warn;
                ?INFO_LEVEL  -> info;
                ?DEBUG_LEVEL -> debug
            end,
    Log = ec_cmd_log:new(Level, Caller, low),
    application:set_env(rebar, log, Log).

set_level(Level) ->
    ok = application:set_env(rebar, log_level, Level).

log(Level = error, Str, Args) ->
    {ok, LogState} = application:get_env(rebar, log),
    ec_cmd_log:Level(LogState, cf:format("~!^~s~n", [Str]), Args);
log(Level, Str, Args) ->
    {ok, LogState} = application:get_env(rebar, log),
    ec_cmd_log:Level(LogState, Str++"~n", Args).

error_level() -> ?ERROR_LEVEL.
default_level() -> ?INFO_LEVEL.

is_verbose(State) ->
    rebar_state:get(State, is_verbose, false).

%% ===================================================================
%% Internal functions
%% ===================================================================

valid_level(Level) ->
    erlang:max(?ERROR_LEVEL, erlang:min(Level, ?DEBUG_LEVEL)).
