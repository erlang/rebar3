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
         add_to_existing_profile/1,
         profiles_remain_applied_with_config_present/1,
         test_profile_applied_at_completion/1,
         test_profile_applied_before_compile/1,
         test_profile_applied_before_eunit/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").

all() ->
    [profile_new_key, profile_merge_keys, profile_merges,
     add_to_profile, add_to_existing_profile,
     profiles_remain_applied_with_config_present,
     test_profile_applied_at_completion,
     test_profile_applied_before_compile,
     test_profile_applied_before_eunit].

init_per_suite(Config) ->
    application:start(meck),
    Config.

end_per_suite(_Config) ->
    application:stop(meck).

init_per_testcase(_, Config) ->
    rebar_test_utils:init_rebar_state(Config, "profiles_").

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

profiles_remain_applied_with_config_present(Config) ->
    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("profiles_remain_applied_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    RebarConfig = [{erl_opts, []}, {profiles, [
        {not_ok, [{erl_opts, [{d, not_ok}]}]}
    ]}],

    rebar_test_utils:create_config(AppDir, RebarConfig),

    rebar_test_utils:run_and_check(Config, RebarConfig,
                                   ["as", "not_ok", "compile"], {ok, [{app, Name}]}),

    Mod = list_to_atom("not_a_real_src_" ++ Name),

    true = lists:member({d, not_ok}, proplists:get_value(options, Mod:module_info(compile), [])).

test_profile_applied_at_completion(Config) ->
    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("test_profile_at_completion_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    {ok, State} = rebar_test_utils:run_and_check(Config,
                                                 [],
                                                 ["eunit"],
                                                 return),

    Opts = rebar_state:opts(State),
    ErlOpts = dict:fetch(erl_opts, Opts),
    true = lists:member({d, 'TEST'}, ErlOpts).

test_profile_applied_before_compile(Config) ->
    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("test_profile_before_compile_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    rebar_test_utils:run_and_check(Config, [], ["eunit"], {ok, [{app, Name}]}),

    S = list_to_atom("not_a_real_src_" ++ Name),
    true = lists:member({d, 'TEST'}, proplists:get_value(options, S:module_info(compile), [])).

test_profile_applied_before_eunit(Config) ->
    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("test_profile_before_eunit_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    rebar_test_utils:run_and_check(Config, [], ["eunit"], {ok, [{app, Name}]}),

    T = list_to_atom("not_a_real_src_" ++ Name ++ "_tests"),
    true = lists:member({d, 'TEST'}, proplists:get_value(options, T:module_info(compile), [])).
