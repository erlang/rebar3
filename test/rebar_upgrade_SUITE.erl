-module(rebar_upgrade_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

all() -> [{group, git}].%, {group, pkg}].

groups() ->
    [{all, [], [top, pair, triplet, tree]},
     {git, [], [{group, all}]},
     {pkg, [], [{group, all}]}].

init_per_suite(Config) ->
    application:start(meck),
    Config.

end_per_suite(_Config) ->
    application:stop(meck).

init_per_group(git, Config) ->
    [{deps_type, git} | Config];
init_per_group(pkg, Config) ->
    [{deps_type, pkg} | Config];
init_per_group(_, Config) ->
    Config.

end_per_group(_, Config) ->
    Config.

init_per_testcase(Case, Config) ->
    DepsType = ?config(deps_type, Config),
    {Deps, Unlocks} = upgrades(Case),
    Expanded = expand_deps(DepsType, Deps),
    [{unlocks, normalize_unlocks(Unlocks)},
     {mock, fun() -> mock_deps(DepsType, Expanded, []) end}
     | setup_project(Case, Config, Expanded)].

end_per_testcase(_, Config) ->
    meck:unload(),
    Config.

setup_project(Case, Config0, Deps) ->
    DepsType = ?config(deps_type, Config0),
    Config = rebar_test_utils:init_rebar_state(
            Config0,
            atom_to_list(Case)++"_"++atom_to_list(DepsType)++"_"
    ),
    AppDir = ?config(apps, Config),
    rebar_test_utils:create_app(AppDir, "Root", "0.0.0", [kernel, stdlib]),
    TopDeps = top_level_deps(Deps),
    RebarConf = rebar_test_utils:create_config(AppDir, [{deps, TopDeps}]),
    [{rebarconfig, RebarConf} | Config].


upgrades(top) ->
    {[{"A", [{"B", [{"D", "1", []}]},
             {"C", [{"D", "2", []}]}]}
     ],
     %% upgrade vs. locked after upgrade
     [{"A", []},
      {"B", {error, transitive_dependency}},
      {"C", {error, transitive_dependency}},
      {"D", "1", {error, transitive_dependency}},
      {"D", "2", {error, unknown_dependency}}]};
upgrades(pair) ->
    {[{"A", [{"C", []}]},
      {"B", [{"D", []}]}],
     [{"A", ["B"]},
      {"B", ["A"]},
      {"C", {error, transitive_dependency}},
      {"D", {error, transitive_dependency}}]};
upgrades(triplet) ->
    {[{"A", [{"D",[]},
             {"E",[]}]},
      {"B", [{"F",[]},
             {"G",[]}]},
      {"C", [{"H",[]},
             {"I",[]}]}],
     [{"A", ["B","C"]},
      {"B", ["A","C"]},
      {"C", ["A","B"]},
      {"D", {error, transitive_dependency}},
      %% ...
      {"I", {error, transitive_dependency}}]};
upgrades(tree) ->
    {[{"A", [{"D",[{"J",[]}]},
             {"E",[{"K",[]}]}]},
      {"B", [{"F",[]},
             {"G",[]}]},
      {"C", [{"H",[]},
             {"I",[]}]}],
     [{"A", ["B","C"]},
      {"B", ["A","C"]},
      {"C", ["A","B"]},
      {"D", {error, transitive_dependency}},
      %% ...
      {"K", {error, transitive_dependency}}]}.

%% TODO: add a test that verifies that unlocking files and then
%% running the upgrade code is enough to properly upgrade things.

top_level_deps([]) -> [];
top_level_deps([{{Name, Vsn, Ref}, _} | Deps]) ->
    [{list_to_atom(Name), Vsn, Ref} | top_level_deps(Deps)];
top_level_deps([{{pkg, Name, Vsn, _URL}, _} | Deps]) ->
    [{list_to_atom(Name), Vsn} | top_level_deps(Deps)].

mock_deps(git, Deps, Upgrades) ->
    mock_git_resource:mock([{deps, flat_deps(Deps)}, {upgrade, Upgrades}]);
mock_deps(pkg, Deps, Upgrades) ->
    mock_pkg_resource:mock([{pkgdeps, flat_pkgdeps(Deps)}, {upgrade, Upgrades}]).

flat_deps([]) -> [];
flat_deps([{{Name,_Vsn,Ref}, Deps} | Rest]) ->
    [{{Name,vsn_from_ref(Ref)}, top_level_deps(Deps)}]
    ++
    flat_deps(Deps)
    ++
    flat_deps(Rest).

vsn_from_ref({git, _, {_, Vsn}}) -> Vsn;
vsn_from_ref({git, _, Vsn}) -> Vsn.

flat_pkgdeps([]) -> [];
flat_pkgdeps([{{pkg, Name, Vsn, _Url}, Deps} | Rest]) ->
    [{{iolist_to_binary(Name),iolist_to_binary(Vsn)}, top_level_deps(Deps)}]
    ++
    flat_pkgdeps(Deps)
    ++
    flat_pkgdeps(Rest).

expand_deps(_, []) -> [];
expand_deps(git, [{Name, Deps} | Rest]) ->
    Dep = {Name, ".*", {git, "https://example.org/user/"++Name++".git", "master"}},
    [{Dep, expand_deps(git, Deps)} | expand_deps(git, Rest)];
expand_deps(git, [{Name, Vsn, Deps} | Rest]) ->
    Dep = {Name, Vsn, {git, "https://example.org/user/"++Name++".git", {tag, Vsn}}},
    [{Dep, expand_deps(git, Deps)} | expand_deps(git, Rest)];
expand_deps(pkg, [{Name, Deps} | Rest]) ->
    Dep = {pkg, Name, "0.0.0", "https://example.org/user/"++Name++".tar.gz"},
    [{Dep, expand_deps(pkg, Deps)} | expand_deps(pkg, Rest)];
expand_deps(pkg, [{Name, Vsn, Deps} | Rest]) ->
    Dep = {pkg, Name, Vsn, "https://example.org/user/"++Name++".tar.gz"},
    [{Dep, expand_deps(pkg, Deps)} | expand_deps(pkg, Rest)].

normalize_unlocks([]) -> [];
normalize_unlocks([{App, Locks} | Rest]) ->
    [{iolist_to_binary(App),
      normalize_unlocks_expect(Locks)} | normalize_unlocks(Rest)];
normalize_unlocks([{App, Vsn, Locks} | Rest]) ->
    [{iolist_to_binary(App), iolist_to_binary(Vsn),
      normalize_unlocks_expect(Locks)} | normalize_unlocks(Rest)].

normalize_unlocks_expect({error, Reason}) ->
    {error, Reason};
normalize_unlocks_expect([]) ->
    [];
normalize_unlocks_expect([{App,Vsn} | Rest]) ->
    [{iolist_to_binary(App), iolist_to_binary(Vsn)}
     | normalize_unlocks_expect(Rest)];
normalize_unlocks_expect([App | Rest]) ->
    [iolist_to_binary(App) | normalize_unlocks_expect(Rest)].

top(Config) -> run(Config).
pair(Config) -> run(Config).
triplet(Config) -> run(Config).
tree(Config) -> run(Config).

run(Config) ->
    apply(?config(mock, Config), []),
    {ok, RebarConfig} = file:consult(?config(rebarconfig, Config)),
    %% Install dependencies before re-mocking for an upgrade
    rebar_test_utils:run_and_check(Config, RebarConfig, ["lock"], {ok, []}),
    Unlocks = ?config(unlocks, Config),
    [begin
        ct:pal("Unlocks: ~p -> ~p", [App, Unlocked]),
        Expectation = case Unlocked of
            Tuple when is_tuple(Tuple) -> Tuple;
            _ -> {unlocked, Unlocked}
        end,
        rebar_test_utils:run_and_check(
            Config, RebarConfig, ["upgrade", App], Expectation
        )
     end || {App, Unlocked} <- Unlocks].

