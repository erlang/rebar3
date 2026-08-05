%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: MIT
%%
%% SPDX-FileCopyrightText: Copyright 2009 Dave Smith (dizzyd@dizzyd.com)
%% SPDX-FileCopyrightText: Copyright 2015-2026 Rebar3 and its contributors
%% SPDX-FileCopyrightText: Copyright 2026 Dipl. Phys. Peer Stritzinger GmbH
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
%%
%% %CopyrightEnd%

-module(rebar_log).

-export([init/2,
         crashdump/2,
         set_level/1,
         get_level/0,
         error_level/0,
         default_level/0,
         debug_level/0,
         diagnostic_level/0,
         atom_to_level/1,
         intensity/0,
         log/3,
         is_verbose/1,
         valid_level/1,
         truncate/1]).

-define(ERROR_LEVEL, 0).
-define(WARN_LEVEL,  1).
-define(INFO_LEVEL,  2).
-define(DEBUG_LEVEL, 3).
-define(DIAGNOSTIC_LEVEL, 4).
-define(DFLT_INTENSITY, high).

-define(PREFIX, "===> ").
-define(RESET, "~!!").
-define(BOLD, "~!^").

%% ===================================================================
%% Types
%% ===================================================================

-type level() :: ?ERROR_LEVEL
                 | ?WARN_LEVEL
                 | ?INFO_LEVEL
                 | ?DEBUG_LEVEL
                 | ?DIAGNOSTIC_LEVEL.

-type level_atom() :: error | warn | info | debug | diagnostic.
-type intensity() ::  low | high | none.
-type color() :: $r | $R | $b | $B | $g | $G | $m | $M | $c | $C.

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
    application:set_env(rebar, log_caller, Caller),
    set_level(Verbosity).

-spec set_level(level_atom() | level()) -> ok | {error, term()}.
set_level(Level) when is_integer(Level) ->
    set_level(level_to_atom(valid_level(Level)));
set_level(Level) when is_atom(Level)->
    ok = application:set_env(rebar, log_level, Level).

-spec get_level() -> level().
get_level() ->
    case application:get_env(rebar, log_level) of
        undefined ->
            ?INFO_LEVEL;
        {ok, Level} ->
            atom_to_level(Level)
    end.

-spec log(level_atom(), string(), list()) -> ok.
log(diagnostic, Str, Args) ->
    %% The diagnostic level is intended for debug info
    %% that is useful for rebar3 developers and implementers who
    %% understand the internal structure; the debug level
    %% itself should aim to be useful for users themselves.
    %% The underlying library only supports debug at its lowest
    %% level, so we filter on our end of the lib.
    case get_level() of
        ?DIAGNOSTIC_LEVEL -> log(debug, Str, Args);
        _ -> ok
    end;
log(Level, Str, Args) ->
    Formatted = format_log(Level, intensity(), Str, Args),
    maybe_log(Level, Formatted).

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
debug_level() -> ?DEBUG_LEVEL.
diagnostic_level() -> ?DIAGNOSTIC_LEVEL.

is_verbose(State) ->
    rebar_state:get(State, is_verbose, false).

-spec valid_level(level()) -> level().
valid_level(Level) ->
    erlang:max(?ERROR_LEVEL, erlang:min(Level, ?DIAGNOSTIC_LEVEL)).

-spec atom_to_level(level_atom()) -> level().
atom_to_level(Level) ->
    case Level of
        error -> ?ERROR_LEVEL;
        warn  -> ?WARN_LEVEL;
        info  -> ?INFO_LEVEL;
        debug -> ?DEBUG_LEVEL;
        diagnostic -> ?DIAGNOSTIC_LEVEL
    end.

-spec level_to_atom(level()) -> level_atom().
level_to_atom(Level) ->
    case Level of
        ?ERROR_LEVEL -> error;
        ?WARN_LEVEL -> warn;
        ?INFO_LEVEL -> info;
        ?DEBUG_LEVEL -> debug;
        ?DIAGNOSTIC_LEVEL -> diagnostic
    end.

%% ===================================================================
%% Internal functions
%% ===================================================================
truncate(IoData) ->
    Size = iolist_size(IoData),
    if Size > 4096 -> [take_bytes(4096, IoData), "[...]"];
       Size =< 4096 -> IoData
    end.

take_bytes(0, _) -> [];
take_bytes(N, Bin) when is_binary(Bin), byte_size(Bin) > N ->
    <<B:N/binary, _>> = Bin,
    binary:copy(B); % avoid holding on to large refs
take_bytes(_, Bin) when is_binary(Bin) ->
    Bin;
take_bytes(_, []) -> [];
take_bytes(N, [H|T]) when is_integer(H) ->
    [H | take_bytes(N-1, T)];
take_bytes(N, [H|T]) when is_binary(H); is_list(H) ->
    Res = take_bytes(N, H),
    [Res | take_bytes(N-byte_size(Res), T)].

-spec level_to_color(level_atom()) -> color().
level_to_color(error) ->
    $R;
level_to_color(warn) ->
    $m;
level_to_color(info) ->
    $g;
level_to_color(debug) ->
    $c;
level_to_color(diagnostic) ->
    $c.

-spec bold(level_atom()) -> boolean().
bold(error) -> true;
bold(_) -> false.

-spec message_format(intensity(), boolean()) -> string().
message_format(high, false) ->
    "~ts~ts";
message_format(high, true) ->
    "~ts" ++ ?BOLD ++ "~ts";
message_format(low, false) ->
    "~ts" ++ ?RESET ++ "~ts";
message_format(low, true) ->
    "~ts" ++ ?RESET ++ ?BOLD ++ "~ts".

-spec colorize(intensity(), level_atom(), list()) -> list().
colorize(none, Level, Text) ->
    case bold(Level) of
        false -> Text;
        true -> lists:flatten(cf:format(?BOLD ++ "~ts", [Text]))
    end;
colorize(Intensity, Level, Text) ->
    Color = case Intensity of
                none -> "";
                _ -> "~!" ++ [level_to_color(Level)]
            end,
    FmtMsg = message_format(Intensity, bold(Level)),
    lists:flatten(cf:format(Color ++ FmtMsg, [?PREFIX, Text])).

format_log(Level, Intensity, Str, Args) ->
    Msg = [io_lib:format(Str, Args), "\n"],
    colorize(Intensity, Level, Msg).

maybe_log(AtomLevel, LogMsg) ->
    CurrentLevel = get_level(),
    case atom_to_level(AtomLevel) of
        Level when Level =< CurrentLevel ->
            io:put_chars(LogMsg);
        _ ->
            ok
    end.
