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
                                                short_desc = "Run Common Tests",
                                                desc = "",
                                                opts = []}),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()}.
do(State) ->
    Opts = build_options(State),
    ct:run_test(Opts),
    {ok, State}.

build_options(State) ->
    Arguments = rebar_state:command_args(State),
    Opts = parse_args(Arguments, []),
    lists:keymerge(1, Opts, defaults(State)).

defaults(State) ->
    Logdir = filename:join([rebar_state:dir(State), "logs"]),
    ok = ensure_logdir(Logdir),
    Testdir = filename:join([rebar_state:dir(State), "test"]),
    ok = ensure_testdir(Testdir),
    [{dir, Testdir},
     {logdir, Logdir}].

parse_args([], Opts) ->
    Opts;
parse_args([Pair|Rest], Opts) ->
    [Key, Val] = string:tokens(Pair, "="),
    Key0 = list_to_atom(Key),
    parse_args(Rest, [{Key0, string:tokens(Val, " ")}|Opts]).

ensure_logdir(Logdir) ->
    case ec_file:is_dir(Logdir) of
        true ->
            ok;
        false ->
            ec_file:mkdir_path(Logdir)
    end.

ensure_testdir(Testdir) ->
    case ec_file:is_dir(Testdir) of
        false ->
            ?INFO("Test directory ~s does not exist:\n",
                  [Testdir]),
            ?FAIL;
        _ -> ok
    end.
