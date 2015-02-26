-module(rebar_profiles_SUITE).

-export([init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2,
         all/0,
         profile_new_key/1,
         profile_merge_keys/1,
         profile_merges/1,
         add_to_profile/1,
         add_to_existing_profile/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").

all() ->
    [profile_new_key, profile_merge_keys, profile_merges,
     add_to_profile, add_to_existing_profile].

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
                   {profiles,
                    [{profile1,
                      [{test1, [{key3, 5}, key1]}]},
                     {profile2, [{test2, "goodbye"}]}]}],
    State = rebar_state:new(RebarConfig),
    State1 = rebar_state:apply_profiles(State, [profile1, profile2]),

    %% Combine lists
    ?assertEqual(lists:sort([key1, key2, {key1, 1, 2}, {key3, 5}]),
                 lists:sort(rebar_state:get(State1, test1))),

    %% Use new value for strings
    "goodbye" = rebar_state:get(State1, test2).

add_to_profile(_Config) ->
    RebarConfig = [{foo, true}, {bar, false}],
    State = rebar_state:new(RebarConfig),
    State1 = rebar_state:add_to_profile(State, test, [{foo, false}]),
    State2 = rebar_state:apply_profiles(State1, test),

    Opts = rebar_state:opts(State2),
    lists:map(fun(K) -> false = dict:fetch(K, Opts) end, [foo, bar]).

add_to_existing_profile(_Config) ->
    RebarConfig = [{foo, true}, {bar, false}, {profiles, [
        {test, [{foo, false}]}
    ]}],
    State = rebar_state:new(RebarConfig),
    State1 = rebar_state:add_to_profile(State, test, [{baz, false}]),
    State2 = rebar_state:apply_profiles(State1, test),
    
    Opts = rebar_state:opts(State2),
    lists:map(fun(K) -> false = dict:fetch(K, Opts) end, [foo, bar, baz]).
