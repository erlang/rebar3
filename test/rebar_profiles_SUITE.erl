-module(rebar_profiles_SUITE).

-export([init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2,
         all/0,
         profile_new_key/1,
         profile_merge_keys/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").

all() ->
    [profile_new_key, profile_merge_keys].

init_per_suite(Config) ->
    application:start(meck),
    Config.

end_per_suite(_Config) ->
    application:stop(meck).

init_per_testcase(_, Config) ->
    rebar_test_utils:init_rebar_state(Config).

end_per_testcase(_, Config) ->
    meck:unload(),
    Config.

profile_new_key(Config) ->
    AppDir = ?config(apps, Config),

    AllDeps = rebar_test_utils:expand_deps(git, [{"a", "1.0.0", []}
                                                ,{"b", "1.0.0", []}]),
    mock_git_resource:mock([{deps, rebar_test_utils:flat_deps(AllDeps)}]),

    Name = rebar_test_utils:create_random_name("profile_new_key_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    Deps = rebar_test_utils:top_level_deps(
             rebar_test_utils:expand_deps(git, [{"a", "1.0.0", []}
                                               ,{"b", "1.0.0", []}])),
    ct:pal("Deps ~p", [Deps]),
    RebarConfig = [{profiles,
                   [{ct,
                    [{deps, Deps}]}]}],

    rebar_test_utils:run_and_check(Config, RebarConfig,
                                   ["as", "ct", "compile"], {ok, [{app, Name}
                                                                 ,{dep, "a", "1.0.0"}
                                                                 ,{dep, "b", "1.0.0"}]}).

profile_merge_keys(Config) ->
    AppDir = ?config(apps, Config),

    AllDeps = rebar_test_utils:expand_deps(git, [{"a", "1.0.0", []}
                                                ,{"b", "1.0.0", []}
                                                ,{"b", "2.0.0", []}]),
    mock_git_resource:mock([{deps, rebar_test_utils:flat_deps(AllDeps)}]),

    Name = rebar_test_utils:create_random_name("profile_new_key_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    Deps = rebar_test_utils:top_level_deps(
             rebar_test_utils:expand_deps(git, [{"a", "1.0.0", []}
                                               ,{"b", "1.0.0", []}])),
    ProfileDeps = rebar_test_utils:top_level_deps(
                    rebar_test_utils:expand_deps(git, [{"b", "2.0.0", []}])),

    RebarConfig = [{deps, Deps},
                   {profiles,
                    [{ct,
                      [{deps, ProfileDeps}]}]}],

    rebar_test_utils:run_and_check(Config, RebarConfig,
                                   ["as", "ct", "compile"], {ok, [{app, Name}
                                                                 ,{dep, "a", "1.0.0"}
                                                                 ,{dep, "b", "2.0.0"}]}).
