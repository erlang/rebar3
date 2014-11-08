%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

-module(rebar_prv_release).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/2]).

-include("rebar.hrl").

-define(PROVIDER, release).
-define(DEPS, [compile]).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    State1 = rebar_state:add_provider(State, providers:create([{name, ?PROVIDER},
                                                               {module, ?MODULE},
                                                               {bare, false},
                                                               {deps, ?DEPS},
                                                               {example, "rebar release"},
                                                               {short_desc, "Build release of project."},
                                                               {desc, ""},
                                                               {opts, relx:opt_spec_list()}])),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Options = rebar_state:command_args(State),
    AllOptions = string:join(["release" | Options], " "),
    try
        case rebar_state:get(State, relx, []) of
            [] ->
                relx:main(AllOptions);
            Config ->
                relx:main([{config, Config}], AllOptions)
        end,
        {ok, State}
    catch
        throw:T ->
            {error, {rlx_prv_release, T}}
    end.

-spec format_error(any(), rebar_state:t()) ->  {iolist(), rebar_state:t()}.
format_error(Reason, State) ->
    {io_lib:format("~p", [Reason]), State}.
