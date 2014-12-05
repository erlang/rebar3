%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

-module(rebar_prv_app_discovery).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-include("rebar.hrl").

-define(PROVIDER, app_discovery).
-define(DEPS, []).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    State1 = rebar_state:add_provider(State, providers:create([{name, ?PROVIDER},
                                                               {module, ?MODULE},
                                                               {bare, true},
                                                               {deps, ?DEPS},
                                                               {example, ""},
                                                               {short_desc, ""},
                                                               {desc, ""},
                                                               {opts, []}])),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    LibDirs = rebar_dir:lib_dirs(State),
    try
        State1 = rebar_app_discover:do(State, LibDirs),
        {ok, State1}
    catch
        throw:{error, Error}->
            {error, {?MODULE, Error}}
    end.

-spec format_error(any()) -> iolist().
format_error({multiple_app_files, Files}) ->
    io_lib:format("Multiple app files found in one app dir: ~s", [string:join(Files, " and ")]);
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
