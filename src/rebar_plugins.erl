%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

-module(rebar_plugins).

-export([install/1]).

-include("rebar.hrl").

%% ===================================================================
%% Public API
%% ===================================================================

install(State) ->
    BaseDir = rebar_state:get(State, base_dir, ""),
    State1 = rebar_state:set(State, base_dir, "plugins"),
    Plugins = rebar_state:get(State1, plugins, []),
    {ok, State2} = rebar_prv_install_deps:handle_deps(State1, Plugins),
    {ok, rebar_state:set(State2, base_dir, BaseDir)}.
