-module(rebar_prv_test_deps).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/2]).

-include("rebar.hrl").

-define(PROVIDER, test_deps).
-define(DEPS, []).

-define(DEFAULT_TEST_DEPS_DIR, "_tdeps").

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    State1 = rebar_state:add_provider(State, providers:create([{name, ?PROVIDER},
                                                               {module, ?MODULE},
                                                               {bare, true},
                                                               {deps, ?DEPS},
                                                               {hooks, {[], []}},
                                                               {example, undefined},
                                                               {short_desc, "Install dependencies needed only for testing."},
                                                               {desc, ""},
                                                               {opts, []}])),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    ProjectApps = rebar_state:project_apps(State),
    TestDeps = rebar_state:get(State, test_deps, []),
    Names = [ec_cnv:to_binary(element(1, Dep)) || Dep <- TestDeps],
    ProjectApps1 = [rebar_app_info:deps(A, Names) || A <- ProjectApps],

    TestDepsDir = rebar_state:get(State, test_deps_dir, ?DEFAULT_TEST_DEPS_DIR),
    DepsDir = rebar_state:get(State, deps_dir, ?DEFAULT_DEPS_DIR),
    State1 = rebar_state:set(State, deps_dir, TestDepsDir),
    {ok, State2} = rebar_prv_install_deps:handle_deps(State1, TestDeps),
    AllDeps = rebar_state:get(State2, all_deps, []),
    State3 = rebar_state:set(State2, deps_dir, DepsDir),

    case rebar_topo:sort_apps(ProjectApps1++AllDeps) of
        {ok, Sort} ->
            ToBuild = lists:dropwhile(fun rebar_app_info:valid/1, Sort -- ProjectApps1),
            State4 = rebar_state:set(State3, deps_to_build, ToBuild),
            {ok, State4};
        {error, Error} ->
            {error, Error}
    end.

-spec format_error(any(), rebar_state:t()) ->  {iolist(), rebar_state:t()}.
format_error(Reason, State) ->
    {io_lib:format("~p", [Reason]), State}.

%% ===================================================================
%% Internal functions
%% ===================================================================
