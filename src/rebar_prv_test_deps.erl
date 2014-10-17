-module(rebar_prv_test_deps).

-behaviour(provider).

-export([init/1,
         do/1]).

-include("rebar.hrl").

-define(PROVIDER, test_deps).
-define(DEPS, [install_deps]).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    State1 = rebar_state:add_provider(State, providers:create([{name, ?PROVIDER},
                                                               {module, ?MODULE},
                                                               {bare, true},
                                                               {deps, ?DEPS},
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

    {ok, State1} = rebar_prv_install_deps:handle_deps(State, TestDeps),
    AllDeps = rebar_state:get(State1, all_deps, []),

    case rebar_topo:sort_apps(ProjectApps1++AllDeps) of
        {ok, Sort} ->
            ToBuild = lists:dropwhile(fun rebar_app_info:valid/1, Sort),
            lists:foreach(fun(AppInfo) ->
                                  AppDir = rebar_app_info:dir(AppInfo),
                                  C = rebar_config:consult(AppDir),
                                  S = rebar_state:new(State1, C, AppDir),
                                  rebar_prv_compile:build(S, AppInfo)
                          end, ToBuild),
            {ok, State1};
        {error, Error} ->
            {error, Error}
    end.

%% ===================================================================
%% Internal functions
%% ===================================================================
