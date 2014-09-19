%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

-module(rebar_prv_common_test).

-behaviour(rebar_provider).

-export([init/1,
         do/1]).

-include("rebar.hrl").

-define(PROVIDER, ct).
-define(DEPS, [compile]).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    State1 = rebar_state:add_provider(State,
                                      #provider{name = ?PROVIDER,
                                                provider_impl = ?MODULE,
                                                bare = false,
                                                deps = ?DEPS,
                                                example = "rebar ct",
                                                short_desc = "Run common tests",
                                                desc = "",
                                                opts = []}),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()}.
do(State) ->
    % Run common tests.
    % CommandArguments = rebar_state:command_args(State),
    Logdir = filename:join([rebar_state:dir(State), "logs"]),
    ok = ensure_logdir(Logdir),
    % @todo check for test dir, figure out how Tristan does nice errors
    Testdir = filename:join([rebar_state:dir(State), "test"]),
    case ec_file:is_dir(Testdir) of
        false ->
            ?INFO("Test directory ~s does not exist:\n",
                  [Testdir]),
            ?FAIL;
        _ -> ok
    end,
    Opts = [{dir, Testdir},
            {logdir, Logdir}],
    ct:run_test(Opts),
    {ok, State}.

ensure_logdir(Logdir) ->
    case ec_file:is_dir(Logdir) of
        true ->
            ok;
        false ->
            ec_file:mkdir_path(Logdir)
    end.
