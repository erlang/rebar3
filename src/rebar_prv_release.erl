%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

-module(rebar_prv_release).

-behaviour(rebar_provider).

-export([init/1,
         do/1]).

-include("rebar.hrl").

-define(PROVIDER, release).
-define(DEPS, [compile]).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_config:config()) -> {ok, rebar_config:config()}.
init(State) ->
    State1 = rebar_config:add_provider(State, #provider{name = ?PROVIDER,
                                                        provider_impl = ?MODULE,
                                                        bare = false,
                                                        deps = ?DEPS,
                                                        example = "rebar release",
                                                        short_desc = "",
                                                        desc = "",
                                                        opts = []}),
    {ok, State1}.

-spec do(rebar_config:config()) -> {ok, rebar_config:config()} | relx:error().
do(Config) ->
    RelxConfig = rebar_config:get_local(Config, relx, []),
    relx:main("release"),
    {ok, Config}.
