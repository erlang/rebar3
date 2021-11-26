-module(rebar_upgrade_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

all() -> [{group, git}, {group, pkg}, novsn_pkg, upgrade_no_args].

groups() ->
    [{all, [], [top_a, top_b, top_c, top_d1, top_d2, top_e,
                pair_a, pair_b, pair_ab, pair_c, pair_all,
                triplet_a, triplet_b, triplet_c,
                tree_a, tree_b, tree_c, tree_c2, tree_cj, tree_ac, tree_all,
                delete_d, promote, stable_lock, fwd_lock,
                compile_upgrade_parity, umbrella_config,
                profiles, profiles_exclusion, tree_migration]},
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

init_per_testcase(novsn_pkg, Config0) ->
    Config = rebar_test_utils:init_rebar_state(Config0, "novsn_pkg_"),
    AppDir = ?config(apps, Config),
    RebarConf = rebar_test_utils:create_config(AppDir, [{deps, [fakeapp]}]),

    Deps = [{{<<"fakeapp">>, <<"1.0.0">>}, []}],
    UpDeps = [{{<<"fakeapp">>, <<"1.1.0">>}, []}],
    Upgrades = ["fakeapp"],

    [{rebarconfig, RebarConf},
     {mock, fun() ->
        catch mock_pkg_resource:unmock(),
        mock_pkg_resource:mock([{pkgdeps, Deps}, {upgrade, []}])
      end},
     {mock_update, fun() ->
        catch mock_pkg_resource:unmock(),
        mock_pkg_resource:mock([{pkgdeps, UpDeps++Deps}, {upgrade, Upgrades}])
      end},
     {expected, {ok, [{dep, "fakeapp", "1.1.0"}, {lock, "fakeapp", "1.1.0"}]}}
     | Config];
init_per_testcase(upgrade_no_args, Config0) ->
    rebar_test_utils:init_rebar_state(Config0, "upgrade_no_args_");
init_per_testcase(Case, Config) ->
    DepsType = ?config(deps_type, Config),
    {Deps, UpDeps, ToUp, Expectations} = upgrades(Case),
    Expanded = rebar_test_utils:expand_deps(DepsType, Deps),
    UpExpanded = rebar_test_utils:expand_deps(DepsType, UpDeps),
    [{expected, normalize_unlocks(Expectations)},
     {mock, fun() -> mock_deps(DepsType, Expanded, []) end},
     {mock_update, fun() -> mock_deps(DepsType, Expanded, UpExpanded, ToUp) end}
     | setup_project(Case, Config, Expanded, UpExpanded)].

end_per_testcase(_, Config) ->
    meck:unload(),
    Config.

setup_project(Case=umbrella_config, Config0, Deps, UpDeps) ->
    DepsType = ?config(deps_type, Config0),
    NameRoot = atom_to_list(Case)++"_"++atom_to_list(DepsType),
    Config = rebar_test_utils:init_rebar_state(Config0, NameRoot++"_"),
    AppDir = filename:join([?config(apps, Config), "apps", NameRoot]),
    rebar_test_utils:create_app(AppDir, "Root", "0.0.0", [kernel, stdlib]),
    TopDeps = rebar_test_utils:top_level_deps(Deps),
    TopConf = rebar_test_utils:create_config(AppDir, [{deps, []}]),
    RebarConf = rebar_test_utils:create_config(AppDir, [{deps, TopDeps}]),
    [{rebarconfig, TopConf},
     {rebarumbrella, RebarConf},
     {next_top_deps, rebar_test_utils:top_level_deps(UpDeps)} | Config];
setup_project(Case, Config0, Deps, UpDeps) when Case == profiles;
                                                Case == profiles_exclusion ->
    DepsType = ?config(deps_type, Config0),
    NameRoot = atom_to_list(Case)++"_"++atom_to_list(DepsType),
    Config = rebar_test_utils:init_rebar_state(Config0, NameRoot++"_"),
    AppDir = filename:join([?config(apps, Config), "apps", NameRoot]),
    rebar_test_utils:create_app(AppDir, "Root", "0.0.0", [kernel, stdlib]),
    [Top|ProfileDeps] = rebar_test_utils:top_level_deps(Deps),
    RebarConf = rebar_test_utils:create_config(AppDir, [
        {deps, [Top]},
        {profiles, [{fake, [{deps, ProfileDeps}]}]}
    ]),
    [NextTop|NextPDeps] = rebar_test_utils:top_level_deps(UpDeps),
    NextConfig = [{deps, [NextTop]},
                  {profiles, [{fake, [{deps, NextPDeps}]}]}],
    [{rebarconfig, RebarConf},
     {next_config, NextConfig} | Config];
setup_project(Case, Config0, Deps, UpDeps) ->
    DepsType = ?config(deps_type, Config0),
    Config = rebar_test_utils:init_rebar_state(
            Config0,
            atom_to_list(Case)++"_"++atom_to_list(DepsType)++"_"
    ),
    AppDir = ?config(apps, Config),
    rebar_test_utils:create_app(AppDir, "Root", "0.0.0", [kernel, stdlib]),
    TopDeps = rebar_test_utils:top_level_deps(Deps),
    RebarConf = rebar_test_utils:create_config(AppDir, [{deps, TopDeps}]),
    [{rebarconfig, RebarConf},
     {next_top_deps, rebar_test_utils:top_level_deps(UpDeps)} | Config].


upgrades(top_a) ->
     %% Original tree
    {[{"A", "1.0.0", [{"B", [{"D", "1.0.0", []}]},
                  {"C", [{"D", "2.0.0", []}]}]}
     ],
     %% Updated tree
     [{"A", "1.0.0", [{"B", [{"D", "3.0.0", []}]},
                  {"C", [{"D", "2.0.0", []}]}]}
     ],
     %% Modified apps, gobally
     ["A","B","D"],
     %% upgrade vs. new tree
     {"A", [{"A","1.0.0"}, "B", "C", {"D","3.0.0"}]}};
upgrades(top_b) ->
     %% Original tree
    {[{"A", "1.0.0", [{"B", [{"D", "1.0.0", []}]},
                  {"C", [{"D", "2.0.0", []}]}]}
     ],
     %% Updated tree
     [{"A", "1.0.0", [{"B", [{"D", "3.0.0", []}]},
                  {"C", [{"D", "2.0.0", []}]}]}
     ],
     %% Modified apps, gobally
     ["A","B","D"],
     %% upgrade vs. new tree
     {"B", {error, {rebar_prv_upgrade, {transitive_dependency, <<"B">>}}}}};
upgrades(top_c) ->
     %% Original tree
    {[{"A", "1.0.0", [{"B", [{"D", "1.0.0", []}]},
                  {"C", [{"D", "2.0.0", []}]}]}
     ],
     %% Updated tree
     [{"A", "1.0.0", [{"B", [{"D", "3.0.0", []}]},
                  {"C", [{"D", "2.0.0", []}]}]}
     ],
     %% Modified apps, gobally
     ["A","B","D"],
     %% upgrade vs. new tree
     {"C", {error, {rebar_prv_upgrade, {transitive_dependency, <<"C">>}}}}};
upgrades(top_d1) ->
     %% Original tree
    {[{"A", "1.0.0", [{"B", [{"D", "1.0.0", []}]},
                  {"C", [{"D", "2.0.0", []}]}]}
     ],
     %% Updated tree
     [{"A", "1.0.0", [{"B", [{"D", "3.0.0", []}]},
                  {"C", [{"D", "2.0.0", []}]}]}
     ],
     %% Modified apps, gobally
     ["A","B","D"],
     %% upgrade vs. new tree
     {"D", {error, {rebar_prv_upgrade, {transitive_dependency, <<"D">>}}}}};
upgrades(top_d2) ->
     %% Original tree
    {[{"A", "1.0.0", [{"B", [{"D", "1.0.0", []}]},
                  {"C", [{"D", "2.0.0", []}]}]}
     ],
     %% Updated tree
     [{"A", "1.0.0", [{"B", [{"D", "3.0.0", []}]},
                  {"C", [{"D", "2.0.0", []}]}]}
     ],
     %% Modified apps, gobally
     ["A","B","D"],
     %% upgrade vs. new tree
     {"D", {error, {rebar_prv_upgrade, {transitive_dependency, <<"D">>}}}}};
upgrades(top_e) ->
     %% Original tree
    {[{"A", "1.0.0", [{"B", [{"D", "1.0.0", []}]},
                  {"C", [{"D", "2.0.0", []}]}]}
     ],
     %% Updated tree
     [{"A", "1.0.0", [{"B", [{"D", "3.0.0", []}]},
                  {"C", [{"D", "2.0.0", []}]}]}
     ],
     %% Modified apps, gobally
     ["A","B","D"],
     %% upgrade vs. new tree
     {"E", {error, {rebar_prv_upgrade, {unknown_dependency, <<"E">>}}}}};
upgrades(pair_a) ->
    {[{"A", "1.0.0", [{"C", "1.0.0", []}]},
      {"B", "1.0.0", [{"D", "1.0.0", []}]}
     ],
     [{"A", "2.0.0", [{"C", "2.0.0", []}]},
      {"B", "2.0.0", [{"D", "2.0.0", []}]}
     ],
     ["A","B","C","D"],
     {"A", [{"A","2.0.0"},{"C","2.0.0"},{"B","1.0.0"},{"D","1.0.0"}]}};
upgrades(pair_b) ->
    {[{"A", "1.0.0", [{"C", "1.0.0", []}]},
      {"B", "1.0.0", [{"D", "1.0.0", []}]}
     ],
     [{"A", "2.0.0", [{"C", "2.0.0", []}]},
      {"B", "2.0.0", [{"D", "2.0.0", []}]}
     ],
     ["A","B","C","D"],
     {"B", [{"A","1.0.0"},{"C","1.0.0"},{"B","2.0.0"},{"D","2.0.0"}]}};
upgrades(pair_ab) ->
    {[{"A", "1.0.0", [{"C", "1.0.0", []}]},
      {"B", "1.0.0", [{"D", "1.0.0", []}]}
     ],
     [{"A", "2.0.0", [{"C", "2.0.0", []}]},
      {"B", "2.0.0", [{"D", "2.0.0", []}]}
     ],
     ["A","B","C","D"],
     {"A,B", [{"A","2.0.0"},{"C","2.0.0"},{"B","2.0.0"},{"D","2.0.0"}]}};
upgrades(pair_c) ->
    {[{"A", "1.0.0", [{"C", "1.0.0", []}]},
      {"B", "1.0.0", [{"D", "1.0.0", []}]}
     ],
     [{"A", "2.0.0", [{"C", "2.0.0", []}]},
      {"B", "2.0.0", [{"D", "2.0.0", []}]}
     ],
     ["A","B","C","D"],
     {"C", {error, {rebar_prv_upgrade, {transitive_dependency, <<"C">>}}}}};
upgrades(pair_all) ->
    {[{"A", "1.0.0", [{"C", "1.0.0", []}]},
      {"B", "1.0.0", [{"D", "1.0.0", []}]}
     ],
     [{"A", "2.0.0", [{"C", "2.0.0", []}]},
      {"B", "2.0.0", [{"D", "2.0.0", []}]}
     ],
     ["A","B","C","D"],
     {"", [{"A","2.0.0"},{"C","2.0.0"},{"B","2.0.0"},{"D","2.0.0"}]}};
upgrades(triplet_a) ->
    {[{"A", "1.0.0", [{"D",[]},
                  {"E","3.0.0",[]}]},
      {"B", "1.0.0", [{"F","1.0.0",[]},
                  {"G",[]}]},
      {"C", "0.0.0", [{"H","3.0.0",[]},
                  {"I",[]}]}],
     [{"A", "1.0.0", [{"D",[]},
                  {"E","2.0.0",[]}]},
      {"B", "1.0.0", [{"F","1.0.0",[]},
                  {"G",[]}]},
      {"C", "1.0.0", [{"H","4.0.0",[]},
                  {"I",[]}]}],
     ["A","C","E","H"],
     {"A", [{"A","1.0.0"}, "D", {"E","2.0.0"},
            {"B","1.0.0"}, {"F","1.0.0"}, "G",
            {"C","0.0.0"}, {"H","3.0.0"}, "I"]}};
upgrades(triplet_b) ->
    {[{"A", "1.0.0", [{"D",[]},
                  {"E","3.0.0",[]}]},
      {"B", "1.0.0", [{"F","1.0.0",[]},
                  {"G",[]}]},
      {"C", "0.0.0", [{"H","3.0.0",[]},
                  {"I",[]}]}],
     [{"A", "2.0.0", [{"D",[]},
                  {"E","2.0.0",[]}]},
      {"B", "1.0.0", [{"F","1.0.0",[]},
                  {"G",[]}]},
      {"C", "1.0.0", [{"H","4.0.0",[]},
                  {"I",[]}]}],
     ["A","C","E","H"],
     {"B", [{"A","1.0.0"}, "D", {"E","3.0.0"},
            {"B","1.0.0"}, {"F","1.0.0"}, "G",
            {"C","0.0.0"}, {"H","3.0.0"}, "I"]}};
upgrades(triplet_c) ->
    {[{"A", "1.0.0", [{"D",[]},
                  {"E","3.0.0",[]}]},
      {"B", "1.0.0", [{"F","1.0.0",[]},
                  {"G",[]}]},
      {"C", "0.0.0", [{"H","3.0.0",[]},
                  {"I",[]}]}],
     [{"A", "2.0.0", [{"D",[]},
                  {"E","2.0.0",[]}]},
      {"B", "1.0.0", [{"F","1.0.0",[]},
                  {"G",[]}]},
      {"C", "1.0.0", [{"H","4.0.0",[]},
                  {"I",[]}]}],
     ["A","C","E","H"],
     {"C", [{"A","1.0.0"}, "D", {"E","3.0.0"},
            {"B","1.0.0"}, {"F","1.0.0"}, "G",
            {"C","1.0.0"}, {"H","4.0.0"}, "I"]}};
upgrades(tree_a) ->
    {[{"A", "1.0.0", [{"D",[{"J",[]}]},
                  {"E",[{"I","1.0.0",[]}]}]},
      {"B", "1.0.0", [{"F",[]},
                  {"G",[]}]},
      {"C", "1.0.0", [{"H",[]},
                  {"I","2.0.0",[]}]}
     ],
     [{"A", "1.0.0", [{"D",[{"J",[]}]},
                  {"E",[{"I","1.0.0",[]}]}]},
      {"B", "1.0.0", [{"F",[]},
                  {"G",[]}]},
      {"C", "2.0.0", [{"H",[]}]}
     ],
     ["C"],
     {"A", [{"A","1.0.0"}, "D", "J", "E",
            {"B","1.0.0"}, "F", "G",
            {"C","1.0.0"}, "H", {"I","2.0.0"}]}};
upgrades(tree_b) ->
    {[{"A", "1.0.0", [{"D",[{"J",[]}]},
                  {"E",[{"I","1.0.0",[]}]}]},
      {"B", "1.0.0", [{"F",[]},
                  {"G",[]}]},
      {"C", "1.0.0", [{"H",[]},
                  {"I","2.0.0",[]}]}
     ],
     [{"A", "1.0.0", [{"D",[{"J",[]}]},
                  {"E",[{"I","1.0.0",[]}]}]},
      {"B", "1.0.0", [{"F",[]},
                  {"G",[]}]},
      {"C", "2.0.0", [{"H",[]}]}
     ],
     ["C"],
     {"B", [{"A","1.0.0"}, "D", "J", "E",
            {"B","1.0.0"}, "F", "G",
            {"C","1.0.0"}, "H", {"I","2.0.0"}]}};
upgrades(tree_c) ->
    {[{"A", "1.0.0", [{"D",[{"J",[]}]},
                  {"E",[{"I","1.0.0",[]}]}]},
      {"B", "1.0.0", [{"F",[]},
                  {"G",[]}]},
      {"C", "1.0.0", [{"H",[]},
                  {"I","2.0.0",[]}]}
     ],
     [{"A", "1.0.0", [{"D",[{"J",[]}]},
                  {"E",[{"I","1.0.0",[]}]}]},
      {"B", "1.0.0", [{"F",[]},
                  {"G",[]}]},
      {"C", "1.0.0", [{"H",[]}]}
     ],
     ["C","I"],
     {"C", [{"A","1.0.0"}, "D", "J", "E", {"I","1.0.0"},
            {"B","1.0.0"}, "F", "G",
            {"C","1.0.0"}, "H"]}};
upgrades(tree_c2) ->
    {[{"A", "1.0.0", [{"D",[{"J",[]}]},
                  {"E",[{"I","1.0.0",[]}]}]},
      {"B", "1.0.0", [{"F",[]},
                  {"G",[]}]},
      {"C", "1.0.0", [{"H",[]},
                  {"I","2.0.0",[]}]}
     ],
     [{"A", "1.0.0", [{"D",[{"J",[]}]},
                  {"E",[{"I","1.0.0",[]}]}]},
      {"B", "1.0.0", [{"F",[]},
                  {"G",[]}]},
      {"C", "1.0.0", [{"H",[{"K",[]}]},
                  {"I","2.0.0",[]}]}
     ],
     ["C", "H"],
     {"C", [{"A","1.0.0"}, "D", "J", "E",
            {"B","1.0.0"}, "F", "G",
            {"C","1.0.0"}, "H", {"I", "2.0.0"}, "K"]}};
upgrades(tree_cj) ->
    {[{"A", "1.0.0", [{"D",[{"J", "1.0.0",[]}]},
                  {"E",[{"I","1.0.0",[]}]}]},
      {"B", "1.0.0", [{"F",[]},
                  {"G",[]}]},
      {"C", "1.0.0", [{"H",[]},
                  {"I","1.0.0",[]}]}
     ],
     [{"A", "1.0.0", [{"D",[{"J", "2.0.0", []}]},
                  {"E",[{"I","1.0.0",[]}]}]},
      {"B", "1.0.0", [{"F",[]},
                  {"G",[]}]},
      {"C", "1.0.0", [{"H",[]},
                  {"I","1.0.0",[]}]}
     ],
     ["C","J"],
     {"C", [{"A","1.0.0"}, "D", {"J", "1.0.0"}, "E", {"I","1.0.0"},
            {"B","1.0.0"}, "F", "G",
            {"C","1.0.0"}, "H"]}};
upgrades(tree_ac) ->
    {[{"A", "1.0.0", [{"D",[{"J",[]}]},
                  {"E",[{"I","1.0.0",[]}]}]},
      {"B", "1.0.0", [{"F",[]},
                  {"G",[]}]},
      {"C", "1.0.0", [{"H",[]},
                  {"I","2.0.0",[]}]}
     ],
     [{"A", "1.0.0", [{"D",[{"J",[]}]},
                  {"E",[{"I","1.0.0",[]}]}]},
      {"B", "1.0.0", [{"F",[]},
                  {"G",[]}]},
      {"C", "1.0.0", [{"H",[]}]}
     ],
     ["C","I"],
     {"C, A", [{"A","1.0.0"}, "D", "J", "E", {"I","1.0.0"},
               {"B","1.0.0"}, "F", "G",
               {"C","1.0.0"}, "H"]}};
upgrades(tree_all) ->
    {[{"A", "1.0.0", [{"D",[{"J",[]}]},
                  {"E",[{"I","1.0.0",[]}]}]},
      {"B", "1.0.0", [{"F",[]},
                  {"G",[]}]},
      {"C", "1.0.0", [{"H",[]},
                  {"I","2.0.0",[]}]}
     ],
     [{"A", "1.0.0", [{"D",[{"J",[]}]},
                  {"E",[{"I","1.0.0",[]}]}]},
      {"B", "1.0.0", [{"F",[]},
                  {"G",[]}]},
      {"C", "1.0.0", [{"H",[]}]}
     ],
     ["C","I"],
     {"", [{"A","1.0.0"}, "D", "J", "E", {"I","1.0.0"},
           {"B","1.0.0"}, "F", "G",
           {"C","1.0.0"}, "H"]}};
upgrades(delete_d) ->
    {[{"A", "1.0.0", [{"B", [{"D", "1.0.0", []}]},
                  {"C", [{"D", "2.0.0", []}]}]}
     ],
     [{"A", "2.0.0", [{"B", []},
                  {"C", []}]}
     ],
     ["A","B", "C"],
     %% upgrade vs. new tree
     {"", [{"A","2.0.0"}, "B", "C"]}};
upgrades(promote) ->
    {[{"A", "1.0.0", [{"C", "1.0.0", []}]},
      {"B", "1.0.0", [{"D", "1.0.0", []}]}
     ],
     [{"A", "2.0.0", [{"C", "2.0.0", []}]},
      {"B", "2.0.0", [{"D", "2.0.0", []}]},
      {"C", "3.0.0", []}
     ],
     ["A","B","C","D"],
     {"C", [{"A","1.0.0"},{"C","3.0.0"},{"B","1.0.0"},{"D","1.0.0"}]}};
upgrades(stable_lock) ->
    {[{"A", "1.0.0", [{"C", "1.0.0", []}]},
      {"B", "1.0.0", [{"D", "1.0.0", []}]}
     ], % lock after this
     [{"A", "2.0.0", [{"C", "2.0.0", []}]},
      {"B", "2.0.0", [{"D", "2.0.0", []}]}
     ],
     [],
     %% Run a regular lock and no app should be upgraded
     {"any", [{"A","1.0.0"},{"C","1.0.0"},{"B","1.0.0"},{"D","1.0.0"}]}};
upgrades(fwd_lock) ->
    {[{"A", "1.0.0", [{"C", "1.0.0", []}]},
      {"B", "1.0.0", [{"D", "1.0.0", []}]}
     ],
     [{"A", "2.0.0", [{"C", "2.0.0", []}]},
      {"B", "2.0.0", [{"D", "2.0.0", []}]}
     ],
     ["A","B","C","D"],
     %% For this one, we should build, rewrite the lock
     %% file to include the result post-upgrade, and then
     %% run a regular lock to see that the lock file is respected
     %% in deps.
     {"any", [{"A","2.0.0"},{"C","2.0.0"},{"B","2.0.0"},{"D","2.0.0"}]}};
upgrades(compile_upgrade_parity) ->
    {[{"A", "1.0.0", [{"D",[{"J",[]}]},
                  {"E",[{"I","1.0.0",[]}]}]},
      {"B", "1.0.0", [{"F",[]},
                  {"G",[]}]},
      {"C", "1.0.0", [{"H",[]},
                  {"I","2.0.0",[]}]}
     ],
     [],
     [],
     {"", [{"A","1.0.0"}, "D", "J", "E", {"I","1.0.0"},
           {"B","1.0.0"}, "F", "G",
           {"C","1.0.0"}, "H"]}};
upgrades(umbrella_config) ->
    {[{"A", "1.0.0", []}],
     [{"A", "2.0.0", []}],
     ["A"],
     {"A", [{"A","2.0.0"}]}};
upgrades(profiles) ->
    %% Ensure that we can unlock deps under a given profile;
    %% B and C should both be in a custom profile
    %% and must not be locked.
    {[{"A", "1.0.0", [{"D",[]},
                  {"E","3.0.0",[]}]},
      {"B", "1.0.0", [{"F","1.0.0",[]},
                  {"G",[]}]},
      {"C", "0.0.0", [{"H","3.0.0",[]},
                  {"I",[]}]}],
     [{"A", "2.0.0", [{"D",[]},
                  {"E","2.0.0",[]}]},
      {"B", "2.0.0", [{"F","2.0.0",[]},
                  {"G",[]}]},
      {"C", "1.0.0", [{"H","4.0.0",[]},
                  {"I",[]}]}],
     ["A","B","C","E","F","H"],
     {"C", [{"A","1.0.0"}, "D", {"E","3.0.0"},
            {"B","2.0.0"}, {"F","2.0.0"}, "G",
            {"C","1.0.0"}, {"H","4.0.0"}, "I"]}};
upgrades(profiles_exclusion) ->
    %% Ensure that we can unlock deps under a given profile;
    %% B and C should both be in a custom profile
    %% and must not be locked.
    {[{"A", "1.0.0", [{"D",[]},
                  {"E","3.0.0",[]}]},
      {"B", "1.0.0", [{"F","1.0.0",[]},
                  {"G",[]}]},
      {"C", "0.0.0", [{"H","3.0.0",[]},
                  {"I",[]}]}],
     [{"A", "2.0.0", [{"D",[]},
                  {"E","2.0.0",[]}]},
      {"B", "2.0.0", [{"F","2.0.0",[]},
                  {"G",[]}]},
      {"C", "1.0.0", [{"H","4.0.0",[]},
                  {"I",[]}]}],
     ["A","B","C","E","F","H"],
     {"A", [{"A","1.0.0"}, "D", {"E","3.0.0"},
            {"B","2.0.0"}, {"F","2.0.0"}, "G",
            {"C","1.0.0"}, {"H","4.0.0"}, "I"]}};
upgrades(tree_migration) ->
    {[{"B", "1.0.0", []},
      {"C", "1.0.0", [{"D","1.0.0",[{"E", "1.0.0", []}]}]}],
     [{"B", "2.0.0", [{"E","1.0.0",[]}]},
      {"C", "1.0.0", [{"D","1.0.0",[]}]}],
     ["B"],
     {"B", [{"A","1.0.0"}, "D", {"E","1.0.0"},
            {"B","2.0.0"}]}}.

%% TODO: add a test that verifies that unlocking files and then
%% running the upgrade code is enough to properly upgrade things.

mock_deps(git, Deps, Upgrades) ->
    catch mock_git_resource:unmock(),
    {SrcDeps, _} = rebar_test_utils:flat_deps(Deps),
    mock_git_resource:mock([{deps, SrcDeps}, {upgrade, Upgrades}]);
mock_deps(pkg, Deps, Upgrades) ->
    catch mock_pkg_resource:unmock(),
    {_, PkgDeps} = rebar_test_utils:flat_deps(Deps),
    mock_pkg_resource:mock([{pkgdeps, PkgDeps}, {upgrade, Upgrades}]).

mock_deps(git, OldDeps, Deps, Upgrades) ->
    catch mock_git_resource:unmock(),
    {SrcDeps, _} = rebar_test_utils:flat_deps(Deps++OldDeps),
    mock_git_resource:mock([{deps, SrcDeps}, {upgrade, Upgrades}]);
mock_deps(pkg, OldDeps, Deps, Upgrades) ->
    catch mock_pkg_resource:unmock(),
    {_, PkgDeps} = rebar_test_utils:flat_deps(Deps++OldDeps),
    mock_pkg_resource:mock([{pkgdeps, PkgDeps}, {upgrade, Upgrades}]).

normalize_unlocks({[], Locks}) ->
    {"--all",
     normalize_unlocks_expect(Locks)};
normalize_unlocks({App, Locks}) ->
    {iolist_to_binary(App),
     normalize_unlocks_expect(Locks)};
normalize_unlocks({App, Vsn, Locks}) ->
    {iolist_to_binary(App), iolist_to_binary(Vsn),
     normalize_unlocks_expect(Locks)}.

normalize_unlocks_expect({error, Reason}) ->
    {error, Reason};
normalize_unlocks_expect([]) ->
    [];
normalize_unlocks_expect([{App,Vsn} | Rest]) ->
    [{dep, App, Vsn},
     {lock, App, Vsn}
     | normalize_unlocks_expect(Rest)];
normalize_unlocks_expect([App | Rest]) ->
    [{dep, App},
     {lock, App} | normalize_unlocks_expect(Rest)].

top_a(Config) -> run(Config).
top_b(Config) -> run(Config).
top_c(Config) -> run(Config).
top_d1(Config) -> run(Config).
top_d2(Config) -> run(Config).
top_e(Config) -> run(Config).

pair_a(Config) -> run(Config).
pair_b(Config) -> run(Config).
pair_ab(Config) -> run(Config).
pair_c(Config) -> run(Config).
pair_all(Config) -> run(Config).

triplet_a(Config) -> run(Config).
triplet_b(Config) -> run(Config).
triplet_c(Config) -> run(Config).

tree_a(Config) -> run(Config).
tree_b(Config) -> run(Config).
tree_c(Config) -> run(Config).
tree_c2(Config) -> run(Config).
tree_cj(Config) -> run(Config).
tree_ac(Config) -> run(Config).
tree_all(Config) -> run(Config).
promote(Config) -> run(Config).

delete_d(Config) ->
    meck:new(rebar_log, [no_link, passthrough]),
    run(Config),
    Infos = [{Str, Args}
            || {_, {rebar_log, log, [info, Str, Args]}, _} <- meck:history(rebar_log)],
    meck:unload(rebar_log),
    ?assertNotEqual([],
                    [1 || {"App ~ts is no longer needed and can be deleted.",
                           [<<"D">>]} <- Infos]).

stable_lock(Config) ->
    apply(?config(mock, Config), []),
    {ok, RebarConfig} = file:consult(?config(rebarconfig, Config)),
    %% Install dependencies before re-mocking for an upgrade
    rebar_test_utils:run_and_check(Config, RebarConfig, ["lock"], {ok, []}),
    {App, Unlocks} = ?config(expected, Config),
    ct:pal("Upgrades: ~p -> ~p", [App, Unlocks]),
    Expectation = case Unlocks of
        {error, Term} -> {error, Term};
        _ -> {ok, Unlocks}
    end,
    apply(?config(mock_update, Config), []),
    NewRebarConf = rebar_test_utils:create_config(?config(apps, Config),
                                                  [{deps, ?config(next_top_deps, Config)}]),
    {ok, NewRebarConfig} = file:consult(NewRebarConf),
    rebar_test_utils:run_and_check(
        Config, NewRebarConfig, ["lock", App], Expectation
    ).

fwd_lock(Config) ->
    apply(?config(mock, Config), []),
    {ok, RebarConfig} = file:consult(?config(rebarconfig, Config)),
    %% Install dependencies before re-mocking for an upgrade
    rebar_test_utils:run_and_check(Config, RebarConfig, ["lock"], {ok, []}),
    {App, Unlocks} = ?config(expected, Config),
    ct:pal("Upgrades: ~p -> ~p", [App, Unlocks]),
    Expectation = case Unlocks of
        {error, Term} -> {error, Term};
        _ -> {ok, Unlocks}
    end,
    rewrite_locks(Expectation, Config),
    apply(?config(mock_update, Config), []),
    NewRebarConf = rebar_test_utils:create_config(?config(apps, Config),
                                                  [{deps, ?config(next_top_deps, Config)}]),
    {ok, NewRebarConfig} = file:consult(NewRebarConf),
    rebar_test_utils:run_and_check(
        Config, NewRebarConfig, ["lock", App], Expectation
    ).

compile_upgrade_parity(Config) ->
    AppDir = ?config(apps, Config),
    apply(?config(mock, Config), []),
    {ok, RebarConfig} = file:consult(?config(rebarconfig, Config)),
    %% compiling and upgrading should generate the same lockfiles when
    %% deps are identical
    Lockfile = filename:join([AppDir, "rebar.lock"]),
    rebar_test_utils:run_and_check(Config, RebarConfig, ["compile"], {ok, []}),
    {ok, CompileLockData1} = file:read_file(Lockfile),
    rebar_test_utils:run_and_check(Config, RebarConfig, ["upgrade", "--all"], {ok, []}),
    {ok, UpgradeLockData} = file:read_file(Lockfile),
    rebar_test_utils:run_and_check(Config, RebarConfig, ["compile"], {ok, []}),
    {ok, CompileLockData2} = file:read_file(Lockfile),
    ?assertEqual(CompileLockData1, CompileLockData2),
    ?assertEqual(CompileLockData1, UpgradeLockData).

umbrella_config(Config) ->
    apply(?config(mock, Config), []),
    {ok, TopConfig} = file:consult(?config(rebarconfig, Config)),
    %% Install dependencies before re-mocking for an upgrade
    rebar_test_utils:run_and_check(Config, TopConfig, ["lock"], {ok, []}),
    {App, Unlocks} = ?config(expected, Config),
    ct:pal("Upgrades: ~p -> ~p", [App, Unlocks]),
    Expectation = case Unlocks of
        {error, Term} -> {error, Term};
        _ -> {ok, Unlocks}
    end,

    meck:new(rebar_prv_upgrade, [passthrough]),
    meck:expect(rebar_prv_upgrade, do, fun(S) ->
                                               apply(?config(mock_update, Config), []),
                                               meck:passthrough([S])
                                       end),
    _NewRebarConf = rebar_test_utils:create_config(filename:dirname(?config(rebarumbrella, Config)),
                                                   [{deps, ?config(next_top_deps, Config)}]),
    %% re-run from the top-level with the old config still in place;
    %% detection must happen when going for umbrella apps!
    rebar_test_utils:run_and_check(
        Config, TopConfig, ["upgrade", App], Expectation
     ),
    meck:unload(rebar_prv_upgrade).

profiles(Config) ->
    apply(?config(mock, Config), []),
    {ok, TopConfig} = file:consult(?config(rebarconfig, Config)),
    %% Install dependencies before re-mocking for an upgrade
    rebar_test_utils:run_and_check(Config, TopConfig, ["lock"], {ok, []}),
    %% Install test deps along with them
    rebar_test_utils:run_and_check(Config, TopConfig, ["as","fake","lock"], {ok, []}),
    {App, Unlocks} = ?config(expected, Config),
    ct:pal("Upgrades: ~p -> ~p", [App, Unlocks]),
    Expectation = case Unlocks of
        {error, Term} -> {error, Term};
        _ -> {ok, [T || T <- Unlocks,
                        element(1,T) == dep orelse
                        lists:member(element(2,T), ["A","D","E"])]}
    end,

    meck:new(rebar_prv_app_discovery, [passthrough]),
    meck:expect(rebar_prv_app_discovery, do, fun(S) ->
                                               apply(?config(mock_update, Config), []),
                                               meck:passthrough([S])
                                       end),
    NewRebarConf = rebar_test_utils:create_config(?config(apps, Config),
                                                  ?config(next_config, Config)),
    {ok, NewRebarConfig} = file:consult(NewRebarConf),
    rebar_test_utils:run_and_check(
        Config, NewRebarConfig, ["as","fake","upgrade", App], Expectation
     ),
    meck:unload(rebar_prv_app_discovery).

profiles_exclusion(Config) -> profiles(Config).

tree_migration(Config) ->
    apply(?config(mock, Config), []),
    ConfigPath = ?config(rebarconfig, Config),
    {ok, RebarConfig} = file:consult(ConfigPath),
    %% Install dependencies before re-mocking for an upgrade
    rebar_test_utils:run_and_check(Config, RebarConfig, ["lock"], {ok, []}),
    {App, _Unlocks} = ?config(expected, Config),

    meck:new(rebar_prv_upgrade, [passthrough]),
    meck:expect(rebar_prv_upgrade, do, fun(S) ->
                                               apply(?config(mock_update, Config), []),
                                               meck:passthrough([S])
                                       end),
    NewRebarConf = rebar_test_utils:create_config(filename:dirname(ConfigPath),
                                                  [{deps, ?config(next_top_deps, Config)}]),
    {ok, NewRebarConfig} = file:consult(NewRebarConf),
    {ok, NewState} = rebar_test_utils:run_and_check(
        Config, NewRebarConfig, ["upgrade", App], return
    ),
    meck:unload(rebar_prv_upgrade),
    %% Check that the internal state properly has E with a lock-level
    %% of 1.
    Locks = rebar_state:lock(NewState),
    [Locked] = [X || X <- Locks, rebar_app_info:name(X) =:= <<"E">>],
    ?assertEqual(1, rebar_app_info:dep_level(Locked)),
    %% Check that the lockfile on disk agrees
    AppDir = ?config(apps, Config),
    Lockfile = filename:join([AppDir, "rebar.lock"]),
    case file:consult(Lockfile) of
        {ok, [{_Vsn, Prop}|_]} -> % packages
            ?assertMatch({<<"E">>, _, 1}, lists:keyfind(<<"E">>, 1, Prop));
        {ok, [Prop]} -> % git source
            ?assertMatch({<<"E">>, _, 1}, lists:keyfind(<<"E">>, 1, Prop))
    end,
    ok.


run(Config) ->
    apply(?config(mock, Config), []),
    ConfigPath = ?config(rebarconfig, Config),
    {ok, RebarConfig} = file:consult(ConfigPath),
    %% Install dependencies before re-mocking for an upgrade
    rebar_test_utils:run_and_check(Config, RebarConfig, ["lock"], {ok, []}),
    {App, Unlocks} = ?config(expected, Config),
    ct:pal("Upgrades: ~p -> ~p", [App, Unlocks]),
    Expectation = case Unlocks of
        {error, Term} -> {error, Term};
        _ -> {ok, Unlocks}
    end,

    meck:new(rebar_prv_upgrade, [passthrough]),
    meck:expect(rebar_prv_upgrade, do, fun(S) ->
                                               apply(?config(mock_update, Config), []),
                                               meck:passthrough([S])
                                       end),
    NewRebarConf = rebar_test_utils:create_config(filename:dirname(ConfigPath),
                                                  [{deps, ?config(next_top_deps, Config)}]),
    {ok, NewRebarConfig} = file:consult(NewRebarConf),
    rebar_test_utils:run_and_check(
        Config, NewRebarConfig, ["upgrade", App], Expectation
     ),
    meck:unload(rebar_prv_upgrade).

novsn_pkg(Config) ->
    apply(?config(mock, Config), []),
    {ok, RebarConfig} = file:consult(?config(rebarconfig, Config)),
    %% Install dependencies before re-mocking for an upgrade
    rebar_test_utils:run_and_check(Config, RebarConfig, ["lock"], {ok, []}),
    Expectation = ?config(expected, Config),
    apply(?config(mock_update, Config), []),
    rebar_test_utils:run_and_check(
        Config, RebarConfig, ["upgrade", "--all"], Expectation
    ),
    ok.

rewrite_locks({ok, Expectations}, Config) ->
    AppDir = ?config(apps, Config),
    LockFile = filename:join([AppDir, "rebar.lock"]),
    Locks = case ?config(deps_type, Config) of
                git ->
                    {ok, [LockData]} = file:consult(LockFile),
                    LockData;
                pkg ->
                    {ok, [{_Vsn, LockData}|_]} = file:consult(LockFile),
                    LockData
            end,
    ExpLocks = [{list_to_binary(Name), Vsn}
               || {lock, Name, Vsn} <- Expectations],
    NewLocks = lists:foldl(
        fun({App, {pkg, Name, _}, Lvl}, Acc) ->
                Vsn = list_to_binary(proplists:get_value(App,ExpLocks)),
                [{App, {pkg, Name, Vsn}, Lvl} | Acc]
        ;  ({App, {git, URL, {ref, _}}, Lvl}, Acc) ->
                Vsn = proplists:get_value(App,ExpLocks),
                [{App, {git, URL, {ref, Vsn}}, Lvl} | Acc]
        end, [], Locks),
    ct:pal("rewriting locks from ~p to~n~p", [Locks, NewLocks]),
    file:write_file(LockFile, io_lib:format("~p.~n", [NewLocks])).

upgrade_no_args(Config) ->
    try rebar_test_utils:run_and_check(Config, [], ["upgrade"], return)
    catch {error, {rebar_prv_upgrade, no_arg}} ->
        ok
    end,
    ok.
