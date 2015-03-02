-module(rebar_profiles_SUITE).

-export([init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2,
         all/0,
         profile_new_key/1,
         profile_merge_keys/1,
         profile_merges/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").

all() ->
    [profile_new_key, profile_merge_keys, profile_merges].

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

profile_merges(_Config) ->
    RebarConfig = [{test1, [{key1, 1, 2}, key2]},
                   {test2, "hello"},
                   {test3, [key3]},
                   {test4, "oldvalue"},
                   {profiles,
                    [{profile1,
                      [{test1, [{key3, 5}, key1]}]},
                     {profile2, [{test2, "goodbye"},
                                 {test3, []},
                                 {test4, []}]}]}],
    State = rebar_state:new(RebarConfig),
    State1 = rebar_state:apply_profiles(State, [profile1, profile2]),

    %% Combine lists
    ?assertEqual(lists:sort([key1, key2, {key1, 1, 2}, {key3, 5}]),
                 lists:sort(rebar_state:get(State1, test1))),

    %% Use new value for strings
    "goodbye" = rebar_state:get(State1, test2),

    %% Check that a newvalue of []/"" doesn't override non-string oldvalues
    [key3] = rebar_state:get(State1, test3),
    [] = rebar_state:get(State1, test4).
