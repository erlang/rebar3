%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

-module(rebar_prv_tar).

-behaviour(rebar_provider).

-export([init/1,
         do/1]).

-include("rebar.hrl").

-define(PROVIDER, tar).
-define(DEPS, []).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    State1 = rebar_state:add_provider(State, #provider{name = ?PROVIDER,
                                                        provider_impl = ?MODULE,
                                                        bare = false,
                                                        deps = ?DEPS,
                                                        example = "rebar tar",
                                                        short_desc = "Tar archive of release built of project.",
                                                        desc = "",
                                                        opts = []}),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | relx:error().
do(Config) ->
    RelxConfig = rebar_state:get_local(Config, relx, []),
    relx:main("release tar"),
    {ok, Config}.
