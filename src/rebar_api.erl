%%% Packages rebar.hrl features and macros into a more generic API
%%% that can be used by plugin builders.
-module(rebar_api).
-include("rebar.hrl").
-include_lib("providers/include/providers.hrl").
-export([abort/0, abort/2,
         console/2,
         debug/2, info/2, warn/2, error/2]).
-export_type([rebar_dict/0, rebar_digraph/0]).

%%%%%%%%%%%%%%%%%%%%%%%
%%% Error reporting %%%
%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Interrupts program flow
abort() -> ?FAIL.

%% @doc like {@link error/2}, except it also raises an
%% exception to interrupt program flow.
abort(Str, Args) -> ?ABORT(Str, Args).

%% @doc Prints to the console, including a newline
console(Str, Args) -> ?CONSOLE(Str, Args).

%% @doc logs with severity `debug'
debug(Str, Args) -> ?DEBUG(Str, Args).
%% @doc logs with severity `info'
info(Str, Args) -> ?INFO(Str, Args).
%% @doc logs with severity `warn'
warn(Str, Args) -> ?WARN(Str, Args).
%% @doc logs with severity `error'
error(Str, Args) -> ?ERROR(Str, Args).
