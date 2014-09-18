%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

-module(rebar_prv_help).

-behaviour(rebar_provider).

-export([init/1,
         do/1]).

-include("rebar.hrl").

-define(PROVIDER, help).
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
                                                       example = "rebar help <task>",
                                                       short_desc = "Display a list of tasks or help for a given task or subtask.",
                                                       desc = "",
                                                       opts = []}),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | relx:error().
do(State) ->
    help(State),
    {ok, State}.

%%
%% print help/usage string
%%
help(State) ->
    ?CONSOLE("Rebar is a tool for working with Erlang projects.~n~n", []),
    OptSpecList = rebar3:option_spec_list(),
    getopt:usage(OptSpecList, "rebar", "", []),
    ?CONSOLE("~nSeveral tasks are available:~n", []),

    rebar_provider:help(State),

    ?CONSOLE("~nRun 'rebar help <TASK>' for details.~n~n", []).
