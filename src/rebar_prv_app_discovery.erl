%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

-module(rebar_prv_app_discovery).

-behaviour(rebar_provider).

-export([init/1,
         do/1]).

-include("rebar.hrl").

-define(PROVIDER, app_discovery).
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
                                                       example = "",
                                                       short_desc = "",
                                                       desc = "",
                                                       opts = []}),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()}.
do(State) ->
    LibDirs = rebar_state:get(State, lib_dirs, ?DEFAULT_LIB_DIRS),
    State1 = rebar_app_discover:do(State, ["deps" | LibDirs]),
    {ok, State1}.
