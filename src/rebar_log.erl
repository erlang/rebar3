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
         crashdump/2,
         set_level/1,
         get_level/0,
         error_level/0,
         default_level/0,
         intensity/0,
         log/3,
         is_verbose/1,
         valid_level/1]).

-define(ERROR_LEVEL, 0).
-define(WARN_LEVEL,  1).
-define(INFO_LEVEL,  2).
-define(DEBUG_LEVEL, 3).
-define(DFLT_INTENSITY, high).

%% ===================================================================
%% Public API
%% ===================================================================

%% @doc Returns the color intensity, we first check the application envorinment
%% if that is not set we check the environment variable REBAR_COLOR.
intensity() ->
    case application:get_env(rebar, color_intensity) of
        undefined ->
            R = case os:getenv("REBAR_COLOR") of
                    "high" ->
                        high;
                    "low" ->
                        low;
                    "none" ->
                        none;
                    _ ->
                        ?DFLT_INTENSITY
                end,
            application:set_env(rebar, color_intensity, R),
            R;
        {ok, Mode} ->
            Mode
    end.

init(Caller, Verbosity) ->
    Level = case valid_level(Verbosity) of
                ?ERROR_LEVEL -> error;
                ?WARN_LEVEL  -> warn;
                ?INFO_LEVEL  -> info;
                ?DEBUG_LEVEL -> debug
            end,
    Intensity = intensity(),
    application:set_env(rebar, log_caller, Caller),
    Log = ec_cmd_log:new(Level, Caller, Intensity),
    set_level(valid_level(Verbosity)),
    application:set_env(rebar, log, Log).

set_level(Level) ->
    ok = application:set_env(rebar, log_level, Level).

get_level() ->
    case application:get_env(rebar, log_level) of
        undefined ->
            default_level();
        {ok, Level} ->
            Level
    end.

log(Level = error, Str, Args) ->
    {ok, LogState} = application:get_env(rebar, log),
    ec_cmd_log:Level(LogState, lists:flatten(cf:format("~!^~ts~n", [Str])), Args);
log(Level, Str, Args) ->
    {ok, LogState} = application:get_env(rebar, log),
    ec_cmd_log:Level(LogState, Str++"~n", Args).

crashdump(Str, Args) ->
    crashdump("rebar3.crashdump", Str, Args).
crashdump(File, Str, Args) ->
    case application:get_env(rebar, log_caller) of
        {ok, api} ->
            ok;
        _ ->
            file:write_file(File, io_lib:fwrite(Str, Args))
    end.

error_level() -> ?ERROR_LEVEL.
default_level() -> ?INFO_LEVEL.

is_verbose(State) ->
    rebar_state:get(State, is_verbose, false).

valid_level(Level) ->
    erlang:max(?ERROR_LEVEL, erlang:min(Level, ?DEBUG_LEVEL)).

%% ===================================================================
%% Internal functions
%% ===================================================================
