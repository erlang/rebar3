-module(rebar_deps_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() -> [sub_app_deps, newly_added_dep, newly_added_after_empty_lock, no_deps_empty_lock,
          http_proxy_settings, https_proxy_settings,
          http_os_proxy_settings, https_os_proxy_settings,
          semver_matching_lt, semver_matching_lte, semver_matching_gt,
          valid_version, top_override, {group, git}, {group, pkg},
          deps_cmd_needs_update_called
         ].

groups() ->
    [{all, [], [flat, pick_highest_left, pick_highest_right,
                pick_smallest1, pick_smallest2,
                circular1, circular2, circular_skip]},
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

init_per_testcase(valid_version, Config) ->
    rebar_test_utils:init_rebar_state(Config);
init_per_testcase(semver_matching_lt, Config) ->
    rebar_test_utils:init_rebar_state(Config);
init_per_testcase(semver_matching_lte, Config) ->
    rebar_test_utils:init_rebar_state(Config);
init_per_testcase(semver_matching_gt, Config) ->
    rebar_test_utils:init_rebar_state(Config);
init_per_testcase(newly_added_after_empty_lock, Config) ->
    rebar_test_utils:init_rebar_state(Config);
init_per_testcase(no_deps_empty_lock, Config) ->
    rebar_test_utils:init_rebar_state(Config);
init_per_testcase(newly_added_dep, Config) ->
    rebar_test_utils:init_rebar_state(Config);
init_per_testcase(sub_app_deps, Config) ->
    rebar_test_utils:init_rebar_state(Config);
init_per_testcase(top_override, Config) ->
    rebar_test_utils:init_rebar_state(Config);
init_per_testcase(http_proxy_settings, Config) ->
    %% Create private rebar.config
    Priv = ?config(priv_dir, Config),
    GlobalDir = filename:join(Priv, "global"),
    GlobalConfigDir = filename:join([GlobalDir, ".config", "rebar3"]),
    GlobalConfig = filename:join([GlobalDir, ".config", "rebar3", "rebar.config"]),

    meck:new(rebar_dir, [passthrough]),
    meck:expect(rebar_dir, global_config, fun() -> GlobalConfig end),
    meck:expect(rebar_dir, global_cache_dir, fun(_) -> GlobalDir end),

    %% Insert proxy variables into config
    rebar_test_utils:create_config(GlobalConfigDir,
                                   [{http_proxy, "http://localhost:1234"}
                                   ]),
    rebar_test_utils:init_rebar_state(Config);
init_per_testcase(https_proxy_settings, Config) ->
    SupportsHttpsProxy = case erlang:system_info(otp_release) of
                             "R16"++_ -> true;
                             "R"++_ -> false;
                             _ -> true % 17 and up don't have a "R" in the version
                         end,
    if not SupportsHttpsProxy ->
            {skip, https_proxy_unsupported_before_R16};
       SupportsHttpsProxy ->
            %% Create private rebar.config
            Priv = ?config(priv_dir, Config),
            GlobalDir = filename:join(Priv, "global"),
            GlobalConfigDir = filename:join([GlobalDir, ".config", "rebar3"]),
            GlobalConfig = filename:join([GlobalDir, ".config", "rebar3", "rebar.config"]),

            meck:new(rebar_dir, [passthrough]),
            meck:expect(rebar_dir, global_config, fun() -> GlobalConfig end),
            meck:expect(rebar_dir, global_cache_dir, fun(_) -> GlobalDir end),

            %% Insert proxy variables into config
            rebar_test_utils:create_config(GlobalConfigDir,
                                           [{https_proxy, "http://localhost:1234"}
                                           ]),
            %% Add a bad value by default to show config overtakes default
            os:putenv("https_proxy", "unparseable-garbage"),
            rebar_test_utils:init_rebar_state(Config)
    end;
init_per_testcase(http_os_proxy_settings, Config) ->
    %% Create private rebar.config
    Priv = ?config(priv_dir, Config),
    GlobalDir = filename:join(Priv, "global"),
    GlobalConfigDir = filename:join([GlobalDir, ".config", "rebar3"]),
    GlobalConfig = filename:join([GlobalDir, ".config", "rebar3", "rebar.config"]),

    meck:new(rebar_dir, [passthrough]),
    meck:expect(rebar_dir, global_config, fun() -> GlobalConfig end),
    meck:expect(rebar_dir, global_cache_dir, fun(_) -> GlobalDir end),

    %% Insert proxy variables into os env, but not config
    os:putenv("http_proxy", "http://localhost:1234"),
    rebar_test_utils:create_config(GlobalConfigDir, []),
    rebar_test_utils:init_rebar_state(Config);
init_per_testcase(https_os_proxy_settings, Config) ->
    SupportsHttpsProxy = case erlang:system_info(otp_release) of
                             "R16"++_ -> true;
                             "R"++_ -> false;
                             _ -> true % 17 and up don't have a "R" in the version
                         end,
    if not SupportsHttpsProxy ->
            {skip, https_proxy_unsupported_before_R16};
       SupportsHttpsProxy ->
            %% Create private rebar.config
            Priv = ?config(priv_dir, Config),
            GlobalDir = filename:join(Priv, "global"),
            GlobalConfigDir = filename:join([GlobalDir, ".config", "rebar3"]),
            GlobalConfig = filename:join([GlobalDir, ".config", "rebar3", "rebar.config"]),

            meck:new(rebar_dir, [passthrough]),
            meck:expect(rebar_dir, global_config, fun() -> GlobalConfig end),
            meck:expect(rebar_dir, global_cache_dir, fun(_) -> GlobalDir end),

            %% Insert proxy variables into os env, not in config
            os:putenv("https_proxy", "http://localhost:1234"),
            rebar_test_utils:create_config(GlobalConfigDir, []),
            rebar_test_utils:init_rebar_state(Config)
    end;
init_per_testcase(deps_cmd_needs_update_called, Config) ->
    rebar_test_utils:init_rebar_state(Config);
init_per_testcase(Case, Config) ->
    {Deps, Warnings, Expect} = deps(Case),
    Expected = case Expect of
                   {ok, List} -> {ok, format_expected_deps(List)};
                   {error, Reason} -> {error, Reason}
               end,
    DepsType = ?config(deps_type, Config),
    mock_warnings(),
    [{expect, Expected},
     {warnings, Warnings}
     | setup_project(Case, Config, rebar_test_utils:expand_deps(DepsType, Deps))].

end_per_testcase(https_proxy_settings, Config) ->
    os:putenv("https_proxy", ""),
    meck:unload(rebar_dir),
    Config;
end_per_testcase(http_proxy_settings, Config) ->
    meck:unload(rebar_dir),
    Config;
end_per_testcase(http_os_proxy_settings, Config) ->
    os:putenv("http_proxy", ""),
    meck:unload(rebar_dir),
    Config;
end_per_testcase(https_os_proxy_settings, Config) ->
    os:putenv("https_proxy", ""),
    meck:unload(rebar_dir),
    Config;
end_per_testcase(_, Config) ->
    meck:unload(),
    Config.

format_expected_deps(Deps) ->
    [case Dep of
         {N,V} -> {dep, N, V};
         N -> {dep, N}
     end || Dep <- Deps].

%% format:
%% {Spec,
%%  [Warning],
%%  {ok, Result} | {error, Reason}}
%%
%% Spec is a list of levelled dependencies of two possible forms:
%% - {"Name", Spec}
%% - {"Name", "Vsn", Spec}
%%
%% Warnings are going to match on mocked ?WARN(...)
%% calls to be evaluated. An empty list means we do not care about
%% warnings, not that no warnings will be printed. This means
%% the list of warning isn't interpreted to be exhaustive, and more
%% warnings may be generated than are listed.
deps(flat) ->
    {[{"B", []},
      {"C", []}],
     [],
     {ok, ["B", "C"]}};
deps(pick_highest_left) ->
    {[{"B", [{"C", "2.0.0", []}]},
      {"C", "1.0.0", []}],
     [{"C","2.0.0"}],
     {ok, ["B", {"C", "1.0.0"}]}};
deps(pick_highest_right) ->
    {[{"B", "1.0.0", []},
      {"C", [{"B", "2.0.0", []}]}],
     [{"B","2.0.0"}],
     {ok, [{"B","1.0.0"}, "C"]}};
deps(pick_smallest1) ->
    {[{"B", [{"D", "1.0.0", []}]},
      {"C", [{"D", "2.0.0", []}]}],
     [{"D","2.0.0"}],
     %% we pick D1 because B < C
     {ok, ["B","C",{"D","1.0.0"}]}};
deps(pick_smallest2) ->
    {[{"C", [{"D", "2.0.0", []}]},
      {"B", [{"D", "1.0.0", []}]}],
     [{"D","2.0.0"}],
     %% we pick D1 because B < C
     {ok, ["B","C",{"D","1.0.0"}]}};
deps(circular1) ->
    {[{"B", [{"A", []}]}, % A is the top-level app
      {"C", []}],
     [],
     {error, {rebar_prv_install_deps, {cycles, [[<<"A">>,<<"B">>]]}}}};
deps(circular2) ->
    {[{"B", [{"C", [{"B", []}]}]},
      {"C", []}],
     [],
     {error, {rebar_prv_install_deps, {cycles, [[<<"B">>,<<"C">>]]}}}};
deps(circular_skip) ->
    %% Never spot the circular dep due to being to low in the deps tree
    %% in source deps
    {[{"B", [{"C", "2.0.0", [{"B", []}]}]},
      {"C", "1.0.0", [{"D",[]}]}],
     [{"C","2.0.0"}],
     {ok, ["B", {"C","1.0.0"}, "D"]}}.

setup_project(Case, Config0, Deps) ->
    DepsType = ?config(deps_type, Config0),
    %% spread packages across 3 repos randomly
    Repos = [<<"test-repo-1">>, <<"test-repo-2">>, <<"hexpm">>],
    Config = rebar_test_utils:init_rebar_state(
               [{repos, Repos} | Config0],
               atom_to_list(Case)++"_"++atom_to_list(DepsType)++"_"
              ),
    AppDir = ?config(apps, Config),
    rebar_test_utils:create_app(AppDir, "A", "0.0.0", [kernel, stdlib]),
    TopDeps = rebar_test_utils:top_level_deps(Deps),
    RebarConf = rebar_test_utils:create_config(AppDir, [{deps, TopDeps}]),
    {SrcDeps, PkgDeps} = rebar_test_utils:flat_deps(Deps),
    mock_git_resource:mock([{deps, SrcDeps}]),
    mock_pkg_resource:mock([{pkgdeps, PkgDeps}, {repos, Repos}]),
    [{rebarconfig, RebarConf} | Config].

mock_warnings() ->
    %% just let it do its thing, we check warnings through
    %% the call log.
    meck:new(rebar_log, [no_link, passthrough]).

mock_rebar_fetch() ->
    meck:new(rebar_fetch, [no_link, passthrough]).


%%% TESTS %%%
flat(Config) -> run(Config).
pick_highest_left(Config) -> run(Config).
pick_highest_right(Config) -> run(Config).
pick_smallest1(Config) -> run(Config).
pick_smallest2(Config) -> run(Config).
circular1(Config) -> run(Config).
circular2(Config) -> run(Config).
circular_skip(Config) -> run(Config).

%% Test that a top-level application overtakes dependencies, and
%% works even if said deps do not exist.
top_override(Config) ->
    AppDir = ?config(apps, Config),
    ct:pal("dir: ~p", [AppDir]),
    Name1 = rebar_test_utils:create_random_name("sub_app1_"),
    Name2 = rebar_test_utils:create_random_name("sub_app2_"),
    SubAppsDir1 = filename:join([AppDir, "apps", Name1]),
    SubAppsDir2 = filename:join([AppDir, "apps", Name2]),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(SubAppsDir1, Name1, Vsn, [kernel, stdlib]),
    rebar_test_utils:create_app(SubAppsDir2, Name2, Vsn, [kernel, stdlib]),
    rebar_test_utils:create_config(
      SubAppsDir1,
      [{deps, [list_to_atom(Name2)]}]
     ),
    rebar_test_utils:create_config(
      SubAppsDir2,
      [{deps, [{list_to_atom(Name1),
                {git, "https://example.org", {branch, "main"}}}]}]
    ),
    rebar_test_utils:run_and_check(
      Config, [], ["compile"],
      {ok, [{app, Name1}, {app,Name2}]}
    ).

%% Test that the deps of project apps that have their own rebar.config
%% are included, but that top level rebar.config deps take precedence
sub_app_deps(Config) ->
    AppDir = ?config(apps, Config),
    Deps = rebar_test_utils:expand_deps(git, [{"a", "1.0.0", []}
                                             ,{"b", "1.0.0", []}
                                             ,{"b", "2.0.0", []}]),
    {SrcDeps, _} = rebar_test_utils:flat_deps(Deps),
    mock_git_resource:mock([{deps, SrcDeps}]),

    Name = rebar_test_utils:create_random_name("sub_app1_"),
    Vsn = rebar_test_utils:create_random_vsn(),

    SubAppsDir = filename:join([AppDir, "apps", Name]),
    SubDeps = rebar_test_utils:top_level_deps(rebar_test_utils:expand_deps(git, [{"a", "1.0.0", []}
                                                                                ,{"b", "2.0.0", []}])),
    rebar_test_utils:create_app(SubAppsDir, Name, Vsn, [kernel, stdlib]),
    rebar_test_utils:create_config(SubAppsDir, [{deps, SubDeps}]),

    TopDeps = rebar_test_utils:top_level_deps(rebar_test_utils:expand_deps(git, [{"b", "1.0.0", []}])),
    {ok, RebarConfig} = file:consult(rebar_test_utils:create_config(AppDir, [{deps, TopDeps}])),

    rebar_test_utils:run_and_check(
      Config, RebarConfig, ["compile"],
      {ok, [{app, Name}, {dep, "a"}, {dep, "b", "1.0.0"}]}).

%% Newly added dependency after locking
newly_added_dep(Config) ->
    AppDir = ?config(apps, Config),
    Deps = rebar_test_utils:expand_deps(git, [{"a", "1.0.0", []}
                                             ,{"b", "1.0.0", [{"c", "1.0.0", []}]}
                                             ,{"c", "2.0.0", []}]),
    {SrcDeps, _} = rebar_test_utils:flat_deps(Deps),
    mock_git_resource:mock([{deps, SrcDeps}]),

    Name = rebar_test_utils:create_random_name("app_"),
    Vsn = rebar_test_utils:create_random_vsn(),

    SubAppsDir = filename:join([AppDir, "apps", Name]),
    rebar_test_utils:create_app(SubAppsDir, Name, Vsn, [kernel, stdlib]),

    TopDeps = rebar_test_utils:top_level_deps(rebar_test_utils:expand_deps(git, [{"b", "1.0.0", []}])),
    {ok, RebarConfig} = file:consult(rebar_test_utils:create_config(AppDir, [{deps, TopDeps}])),
    rebar_test_utils:run_and_check(
      Config, RebarConfig, ["compile"],
      {ok, [{app, Name}, {dep, "b", "1.0.0"}, {dep, "c", "1.0.0"}]}),

    %% Add a and c to top level
    TopDeps2 = rebar_test_utils:top_level_deps(rebar_test_utils:expand_deps(git, [{"a", "1.0.0", []}
                                                                                 ,{"c", "2.0.0", []}
                                                                                 ,{"b", "1.0.0", []}])),
    {ok, RebarConfig2} = file:consult(rebar_test_utils:create_config(AppDir, [{deps, TopDeps2}])),
    LockFile = filename:join(AppDir, "rebar.lock"),
    RebarConfig3 = rebar_config:merge_locks(RebarConfig2,
                                            rebar_config:consult_lock_file(LockFile)),

    %% a should now be installed and c should not change
    rebar_test_utils:run_and_check(
      Config, RebarConfig3, ["compile"],
      {ok, [{app, Name}, {dep, "a"}, {dep, "b", "1.0.0"}, {dep, "c", "1.0.0"}]}).

newly_added_after_empty_lock(Config) ->
    AppDir = ?config(apps, Config),
    Deps = rebar_test_utils:expand_deps(git, [{"a", "1.0.0", []}]),
    {SrcDeps, _} = rebar_test_utils:flat_deps(Deps),
    mock_git_resource:mock([{deps, SrcDeps}]),

    Name = rebar_test_utils:create_random_name("app_"),
    Vsn = rebar_test_utils:create_random_vsn(),

    SubAppsDir = filename:join([AppDir, "apps", Name]),
    rebar_test_utils:create_app(SubAppsDir, Name, Vsn, [kernel, stdlib]),

    TopDeps = rebar_test_utils:top_level_deps(rebar_test_utils:expand_deps(git, [])),
    {ok, RebarConfig} = file:consult(rebar_test_utils:create_config(AppDir, [{deps, TopDeps}])),
    rebar_test_utils:run_and_check(
      Config, RebarConfig, ["compile"],
      {ok, []}),

    %% Add a and c to top level
    TopDeps2 = rebar_test_utils:top_level_deps(rebar_test_utils:expand_deps(git, [{"a", "1.0.0", []}])),
    {ok, RebarConfig2} = file:consult(rebar_test_utils:create_config(AppDir, [{deps, TopDeps2}])),
    LockFile = filename:join(AppDir, "rebar.lock"),
    RebarConfig3 = rebar_config:merge_locks(RebarConfig2,
                                            rebar_config:consult_lock_file(LockFile)),

    %% a should now be installed and c should not change
    rebar_test_utils:run_and_check(
      Config, RebarConfig3, ["compile"],
      {ok, [{app, Name}, {dep, "a", "1.0.0"}]}).

no_deps_empty_lock(Config) ->
    AppDir = ?config(apps, Config),
    Deps = rebar_test_utils:expand_deps(git, []),
    mock_git_resource:mock([{deps, Deps}]),

    {ok, RebarConfig} = file:consult(rebar_test_utils:create_config(AppDir, [{deps, Deps}])),
    rebar_test_utils:run_and_check(
      Config, RebarConfig, ["compile"],
      {ok, []}),

    LockFile = filename:join(AppDir, "rebar.lock"),
    %% the lock file should exist
    {ok,_} = file:read_file_info(LockFile),

    %% and just in case, it should be empty
    ?assertEqual([], rebar_config:consult_lock_file(LockFile)),
    %% which means that merging that lock file with the current config
    %% returns the same config
    ?assertEqual(RebarConfig, rebar_config:merge_locks(RebarConfig,
                                            rebar_config:consult_lock_file(LockFile))).

http_proxy_settings(_Config) ->
    %% Load config
    rebar_utils:set_httpc_options(),
    rebar3:init_config(),

    %% Assert variable is right
    ?assertEqual({ok,{{"localhost", 1234}, []}},
                 httpc:get_option(proxy, rebar)).

https_proxy_settings(_Config) ->
    %% Load config
    rebar_utils:set_httpc_options(),
    rebar3:init_config(),

    %% Assert variable is right
    ?assertEqual({ok,{{"localhost", 1234}, []}},
                 httpc:get_option(https_proxy, rebar)).

http_os_proxy_settings(_Config) ->
    %% Load config
    rebar_utils:set_httpc_options(),
    rebar3:init_config(),

    %% Assert variable is right
    ?assertEqual({ok,{{"localhost", 1234}, []}},
                 httpc:get_option(proxy, rebar)).

https_os_proxy_settings(_Config) ->
    %% Load config
    rebar_utils:set_httpc_options(),
    rebar3:init_config(),

    %% Assert variable is right
    ?assertEqual({ok,{{"localhost", 1234}, []}},
                 httpc:get_option(https_proxy, rebar)).

semver_matching_lt(_Config) ->
    MaxVsn = <<"0.2.0">>,
    Vsns = [<<"0.1.7">>, <<"0.1.9">>, <<"0.1.8">>, <<"0.2.0">>, <<"0.2.1">>],
    ?assertEqual({ok, <<"0.1.9">>},
                 rebar_packages:cmpl_(undefined, MaxVsn, Vsns,
                                        fun ec_semver:lt/2)).

semver_matching_lte(_Config) ->
    MaxVsn = <<"0.2.0">>,
    Vsns = [<<"0.1.7">>, <<"0.1.9">>, <<"0.1.8">>, <<"0.2.0">>, <<"0.2.1">>],
    ?assertEqual({ok, <<"0.2.0">>},
                 rebar_packages:cmpl_(undefined, MaxVsn, Vsns,
                                        fun ec_semver:lte/2)).

semver_matching_gt(_Config) ->
    MaxVsn = <<"0.2.0">>,
    Vsns = [<<"0.1.7">>, <<"0.1.9">>, <<"0.1.8">>, <<"0.2.0">>, <<"0.2.1">>],
    ?assertEqual({ok, <<"0.2.1">>},
                 rebar_packages:cmp_(undefined, MaxVsn, Vsns,
                                       fun ec_semver:gt/2)).
semver_matching_gte(_Config) ->
    MaxVsn = <<"0.2.0">>,
    Vsns = [<<"0.1.7">>, <<"0.1.9">>, <<"0.1.8">>, <<"0.2.0">>],
    ?assertEqual({ok, <<"0.2.0">>},
                 rebar_packages:cmp_(undefined, MaxVsn, Vsns,
                                       fun ec_semver:gt/2)).

valid_version(_Config) ->
    ?assert(rebar_packages:valid_vsn(<<"0.1">>)),
    ?assert(rebar_packages:valid_vsn(<<"0.1.0">>)),
    ?assert(rebar_packages:valid_vsn(<<" 0.1.0">>)),
    ?assert(rebar_packages:valid_vsn(<<"  0.1.0">>)),
    ?assert(rebar_packages:valid_vsn(<<"<0.1">>)),
    ?assert(rebar_packages:valid_vsn(<<"<0.1.0">>)),
    ?assert(rebar_packages:valid_vsn(<<"< 0.1.0">>)),
    ?assert(rebar_packages:valid_vsn(<<"<  0.1.0">>)),
    ?assert(rebar_packages:valid_vsn(<<">0.1">>)),
    ?assert(rebar_packages:valid_vsn(<<">0.1.0">>)),
    ?assert(rebar_packages:valid_vsn(<<"> 0.1.0">>)),
    ?assert(rebar_packages:valid_vsn(<<">  0.1.0">>)),
    ?assert(rebar_packages:valid_vsn(<<"<=0.1">>)),
    ?assert(rebar_packages:valid_vsn(<<"<=0.1.0">>)),
    ?assert(rebar_packages:valid_vsn(<<"<= 0.1.0">>)),
    ?assert(rebar_packages:valid_vsn(<<"<=  0.1.0">>)),
    ?assert(rebar_packages:valid_vsn(<<">=0.1">>)),
    ?assert(rebar_packages:valid_vsn(<<">=0.1.0">>)),
    ?assert(rebar_packages:valid_vsn(<<">= 0.1.0">>)),
    ?assert(rebar_packages:valid_vsn(<<">=  0.1.0">>)),
    ?assert(rebar_packages:valid_vsn(<<"==0.1">>)),
    ?assert(rebar_packages:valid_vsn(<<"==0.1.0">>)),
    ?assert(rebar_packages:valid_vsn(<<"== 0.1.0">>)),
    ?assert(rebar_packages:valid_vsn(<<"==  0.1.0">>)),
    ?assert(rebar_packages:valid_vsn(<<"~>0.1">>)),
    ?assert(rebar_packages:valid_vsn(<<"~>0.1.0">>)),
    ?assert(rebar_packages:valid_vsn(<<"~> 0.1.0">>)),
    ?assert(rebar_packages:valid_vsn(<<"~>  0.1.0">>)),
    ?assertNot(rebar_packages:valid_vsn(<<"> 0.1.0 and < 0.2.0">>)),
    ok.


run(Config) ->
    {ok, RebarConfig} = file:consult(?config(rebarconfig, Config)),
    rebar_test_utils:run_and_check(
        Config, RebarConfig, ["install_deps"], ?config(expect, Config)
    ),
    check_warnings(warning_calls(), ?config(warnings, Config), ?config(deps_type, Config)).


deps_cmd_needs_update_called(Config) ->
    mock_rebar_fetch(),
    AppDir = ?config(apps, Config),
    Deps = rebar_test_utils:expand_deps(git, [{"a", "1.0.0", []}
                                             ,{"b", "1.0.0", [{"c", "1.0.0", []}]}
                                             ,{"c", "2.0.0", []}]),
    {SrcDeps, _} = rebar_test_utils:flat_deps(Deps),
    mock_git_resource:mock([{deps, SrcDeps}]),

    Name = rebar_test_utils:create_random_name("app_"),
    Vsn = rebar_test_utils:create_random_vsn(),

    SubAppsDir = filename:join([AppDir, "apps", Name]),
    rebar_test_utils:create_app(SubAppsDir, Name, Vsn, [kernel, stdlib]),

    TopDeps = rebar_test_utils:top_level_deps(
                rebar_test_utils:expand_deps(git, [{"b", "1.0.0", []}])),
    {ok, RebarConfig} = file:consult(rebar_test_utils:create_config(AppDir, [{deps, TopDeps}])),
    rebar_test_utils:run_and_check(Config, RebarConfig, ["deps"], {ok, []}),

    [<<"b">>] = rebar_fetch_needs_update_calls_sorted(),

    %% Add c to top level
    TopDeps2 = rebar_test_utils:top_level_deps(rebar_test_utils:expand_deps(git, [{"c", "2.0.0", []}
                                                                                 ,{"b", "1.0.0", []}])),
    {ok, RebarConfig2} = file:consult(rebar_test_utils:create_config(AppDir, [{deps, TopDeps2}])),
    rebar_test_utils:run_and_check(Config, RebarConfig2, ["deps"], {ok, []}),

    %% Only top level deps are checked for updates
    [<<"b">>, <<"b">>, <<"c">>] = rebar_fetch_needs_update_calls_sorted(),

    %% Lock deps
    rebar_test_utils:run_and_check(Config, RebarConfig2, ["lock"], {ok, []}),
    NeedsUpdate1 = rebar_fetch_needs_update_calls_sorted(),

    %% Switch c for a as top level deps
    TopDeps3 = rebar_test_utils:top_level_deps(rebar_test_utils:expand_deps(git, [{"a", "1.0.0", []}
                                                                                 ,{"b", "1.0.0", []}])),

    {ok, RebarConfig3} = file:consult(rebar_test_utils:create_config(AppDir, [{deps, TopDeps3}])),
    LockFile = filename:join(AppDir, "rebar.lock"),
    RebarConfig4 = rebar_config:merge_locks(RebarConfig3,
                                            rebar_config:consult_lock_file(LockFile)),

    rebar_test_utils:run_and_check(Config, RebarConfig4, ["deps"], {ok, []}),

    NeedsUpdate2 = lists:subtract(rebar_fetch_needs_update_calls_sorted(), NeedsUpdate1),

    %% B and C from lock file  + install_deps and A, B and C from 'deps'
    [<<"a">>, <<"b">>, <<"b">>, <<"c">>, <<"c">>] = NeedsUpdate2.


rebar_fetch_needs_update_calls_sorted() ->
    History = meck:history(rebar_fetch),
    DepsNames = [rebar_app_info:name(Dep)
                 || {_, {rebar_fetch, needs_update, [Dep, _]}, _} <- History],
    lists:sort(DepsNames).

warning_calls() ->
    History = meck:history(rebar_log),
    [{Str, Args} || {_, {rebar_log, log, [warn, Str, Args]}, _} <- History].

check_warnings(_, [], _) ->
    ok;
check_warnings(Warns, [{Name, Vsn} | Rest], Type) ->
    ct:pal("Checking for warning ~p in ~p", [{Name,Vsn},Warns]),
    ?assert(in_warnings(Type, Warns, Name, Vsn)),
    check_warnings(Warns, Rest, Type).

in_warnings(git, Warns, NameRaw, VsnRaw) ->
    Name = iolist_to_binary(NameRaw),
    Vsn  = iolist_to_binary([$",VsnRaw,$"]),
    1 =< length([1 || {_, [[AppName, $\s,$(,$f,$r,$o,$m,$\s,[${,["git",$,, _URL, $,,[${,["tag",$,, AppVsn], $}]],$}],$)]]} <- Warns,
                      iolist_to_binary(AppName) =:= Name,
                      iolist_to_binary(AppVsn) =:= Vsn]);
in_warnings(pkg, Warns, NameRaw, VsnRaw) ->
    Name = iolist_to_binary(NameRaw),
    Vsn = iolist_to_binary(VsnRaw),
    1 =< length([1 || {_, [[AppName, $\s,$v, AppVsn]]} <- Warns,
                      iolist_to_binary(AppName) =:= Name,
                      iolist_to_binary(AppVsn) =:= Vsn]).
