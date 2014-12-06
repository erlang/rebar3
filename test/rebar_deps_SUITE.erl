%%% TODO: check that warnings are appearing
-module(rebar_deps_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() -> [flat, pick_highest_left, pick_highest_right, pick_earliest,
          circular1, circular2].

init_per_suite(Config) ->
    application:start(meck),
    Config.

end_per_suite(_Config) ->
    application:stop(meck).

init_per_testcase(Case, Config) ->
    {Deps, Expect} = deps(Case),
    Expected = case Expect of
        {ok, List} -> {ok, format_expected_deps(List)};
        {error, Reason} -> {error, Reason}
    end,
    [{expect, Expected} | setup_project(Case, Config, expand_deps(Deps))].

format_expected_deps(Deps) ->
    [case Dep of
        {N,V} -> {dep, N, V};
        N -> {dep, N}
     end || Dep <- Deps].

deps(flat) ->
    {[{"B", []},
      {"C", []}],
     {ok, ["B", "C"]}};
deps(pick_highest_left) ->
    {[{"B", [{"C", "2", []}]},
      {"C", "1", []}],
     {ok, ["B", {"C", "1"}]}}; % Warn C2
deps(pick_highest_right) ->
    {[{"B", "1", []},
      {"C", [{"B", "2", []}]}],
     {ok, [{"B","1"}, "C"]}}; % Warn B2
deps(pick_earliest) ->
    {[{"B", [{"D", "1", []}]},
      {"C", [{"D", "2", []}]}],
     {ok, ["B","C",{"D","1"}]}}; % Warn D2
deps(circular1) ->
    {[{"B", [{"A", []}]}, % A is the top-level app
      {"C", []}],
     {error, {rebar_prv_install_deps, {cycles, [[<<"A">>,<<"B">>]]}}}};
deps(circular2) ->
    {[{"B", [{"C", [{"B", []}]}]},
      {"C", []}],
     {error, {rebar_prv_install_deps, {cycles, [[<<"B">>,<<"C">>]]}}}}.

end_per_testcase(_, Config) ->
    mock_git_resource:unmock(),
    meck:unload(),
    Config.

expand_deps([]) -> [];
expand_deps([{Name, Deps} | Rest]) ->
    Dep = {Name, ".*", {git, "https://example.org/user/"++Name++".git", "master"}},
    [{Dep, expand_deps(Deps)} | expand_deps(Rest)];
expand_deps([{Name, Vsn, Deps} | Rest]) ->
    Dep = {Name, Vsn, {git, "https://example.org/user/"++Name++".git", {tag, Vsn}}},
    [{Dep, expand_deps(Deps)} | expand_deps(Rest)].

setup_project(Case, Config0, Deps) ->
    Config = rebar_test_utils:init_rebar_state(Config0, atom_to_list(Case)),
    AppDir = ?config(apps, Config),
    rebar_test_utils:create_app(AppDir, "A", "0.0.0", [kernel, stdlib]),
    TopDeps = top_level_deps(Deps),
    RebarConf = rebar_test_utils:create_config(AppDir, [{deps, TopDeps}]),
    mock_git_resource:mock([{deps, flat_deps(Deps)}]),
    [{rebarconfig, RebarConf} | Config].


flat_deps([]) -> [];
flat_deps([{{Name,_Vsn,_Ref}, Deps} | Rest]) ->
    [{Name, top_level_deps(Deps)}]
    ++
    flat_deps(Deps)
    ++
    flat_deps(Rest).

top_level_deps(Deps) -> [{list_to_atom(Name),Vsn,Ref} || {{Name,Vsn,Ref},_} <- Deps].

%%% TESTS %%%
flat(Config) -> run(Config).
pick_highest_left(Config) -> run(Config).
pick_highest_right(Config) -> run(Config).
pick_earliest(Config) -> run(Config).
circular1(Config) -> run(Config).
circular2(Config) -> run(Config).

run(Config) ->
    {ok, RebarConfig} = file:consult(?config(rebarconfig, Config)),
    rebar_test_utils:run_and_check(
        Config, RebarConfig, "install_deps", ?config(expect, Config)
    ).

