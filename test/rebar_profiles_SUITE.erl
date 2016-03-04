-module(rebar_profiles_SUITE).

-export([init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2,
         all/0,
         profile_new_key/1,
         profile_merge_keys/1,
         explicit_profile_deduplicate_deps/1,
         implicit_profile_deduplicate_deps/1,
         all_deps_code_paths/1,
         profile_merges/1,
         same_profile_deduplication/1,
         stack_deduplication/1,
         add_to_profile/1,
         add_to_existing_profile/1,
         profiles_remain_applied_with_config_present/1,
         deduplicated_paths/1,
         test_profile_applied_at_completion/1,
         test_profile_applied_before_compile/1,
         test_profile_applied_before_eunit/1,
         test_profile_applied_to_apps/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").

all() ->
    [profile_new_key, profile_merge_keys, all_deps_code_paths, profile_merges,
     explicit_profile_deduplicate_deps, implicit_profile_deduplicate_deps,
     same_profile_deduplication, stack_deduplication,
     add_to_profile, add_to_existing_profile,
     profiles_remain_applied_with_config_present,
     deduplicated_paths,
     test_profile_applied_at_completion,
     test_profile_applied_before_compile,
     test_profile_applied_before_eunit,
     test_profile_applied_to_apps].

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
    {SrcDeps, []} = rebar_test_utils:flat_deps(AllDeps),
    mock_git_resource:mock([{deps, SrcDeps}]),

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
    {SrcDeps, []} = rebar_test_utils:flat_deps(AllDeps),
    mock_git_resource:mock([{deps, SrcDeps}]),

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

explicit_profile_deduplicate_deps(Config) ->
    AppDir = ?config(apps, Config),

    AllDeps = rebar_test_utils:expand_deps(git, [{"a", "1.0.0", []}
                                                ,{"a", "2.0.0", []}
                                                ,{"b", "1.0.0", []}
                                                ,{"b", "2.0.0", []}]),
    {SrcDeps, []} = rebar_test_utils:flat_deps(AllDeps),
    mock_git_resource:mock([{deps, SrcDeps}]),

    Name = rebar_test_utils:create_random_name("explicit_profile_deduplicate_deps_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    FooDeps = rebar_test_utils:top_level_deps(
                    rebar_test_utils:expand_deps(git, [{"a", "1.0.0", []},
                                                       {"b", "2.0.0", []}])),
    BarDeps = rebar_test_utils:top_level_deps(
                    rebar_test_utils:expand_deps(git, [{"b", "1.0.0", []}])),

    RebarConfig = [{profiles,
                    [{foo,
                      [{deps, FooDeps}]},
                     {bar,
                      [{deps, BarDeps}]}]}],

    rebar_test_utils:run_and_check(Config, RebarConfig,
                                   ["as", "bar,foo,bar", "compile"], {ok, [{app, Name}
                                                                 ,{dep, "a", "1.0.0"}
                                                                 ,{dep, "b", "1.0.0"}]}).

implicit_profile_deduplicate_deps(Config) ->
    AppDir = ?config(apps, Config),

    AllDeps = rebar_test_utils:expand_deps(git, [{"a", "1.0.0", []}
                                                ,{"a", "2.0.0", []}
                                                ,{"b", "1.0.0", []}
                                                ,{"b", "2.0.0", []}]),
    {SrcDeps, []} = rebar_test_utils:flat_deps(AllDeps),
    mock_git_resource:mock([{deps, SrcDeps}]),

    Name = rebar_test_utils:create_random_name("implicit_profile_deduplicate_deps_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    TestDeps = rebar_test_utils:top_level_deps(
                    rebar_test_utils:expand_deps(git, [{"a", "1.0.0", []},
                                                       {"b", "2.0.0", []}])),
    ProfileDeps = rebar_test_utils:top_level_deps(
                    rebar_test_utils:expand_deps(git, [{"b", "1.0.0", []}])),

    RebarConfig = [{profiles,
                    [{test,
                      [{deps, TestDeps}]},
                     {bar,
                      [{deps, ProfileDeps}]}]}],

    rebar_test_utils:run_and_check(Config, RebarConfig,
                                   ["as", "test,bar", "eunit"], {ok, [{app, Name}
                                                                 ,{dep, "a", "1.0.0"}
                                                                 ,{dep, "b", "2.0.0"}]}).

all_deps_code_paths(Config) ->
    AppDir = ?config(apps, Config),

    AllDeps = rebar_test_utils:expand_deps(git, [{"a", "1.0.0", []}
                                                ,{"b", "2.0.0", []}]),
    {SrcDeps, []} = rebar_test_utils:flat_deps(AllDeps),
    mock_git_resource:mock([{deps, SrcDeps}]),

    Name = rebar_test_utils:create_random_name("all_deps_code_paths"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    Deps = rebar_test_utils:top_level_deps(
             rebar_test_utils:expand_deps(git, [{"a", "1.0.0", []}])),
    ProfileDeps = rebar_test_utils:top_level_deps(
                    rebar_test_utils:expand_deps(git, [{"b", "2.0.0", []}])),

    RebarConfig = [{deps, Deps},
                   {profiles,
                    [{all_deps_test,
                      [{deps, ProfileDeps}]}]}],
    os:putenv("REBAR_PROFILE", "all_deps_test"),
    {ok, State} = rebar_test_utils:run_and_check(Config, RebarConfig,
                                   ["compile"], {ok, [{app, Name}
                                                     ,{dep, "a", "1.0.0"}
                                                     ,{dep, "b", "2.0.0"}]}),
    os:putenv("REBAR_PROFILE", ""),

    Paths = rebar_state:code_paths(State, all_deps),
    Path = lists:reverse(["_build", "all_deps_test", "lib", "b", "ebin"]),
    ?assert(lists:any(fun(X) ->
                              Path =:= lists:sublist(lists:reverse(filename:split(X)), 5)
                      end, Paths)).


profile_merges(_Config) ->
    RebarConfig = [{test1, [{key1, 1, 2}, key2]},
                   {test2, "hello"},
                   {test3, [key3]},
                   {test4, "oldvalue"},
                   {test5, [{key5, true}]},
                   {test6, [{key6, false}]},
                   {profiles,
                    [{profile1,
                      [{test1, [{key3, 5}, key1]}]},
                     {profile2, [{test2, "goodbye"},
                                 {test3, []},
                                 {test4, []},
                                 {test5, [{key5, false}]},
                                 {test6, [{key6, true}]}
                                ]}]}],
    State = rebar_state:new(RebarConfig),
    State1 = rebar_state:apply_profiles(State, [profile1, profile2]),

    %% Combine lists
    ?assertEqual(lists:sort([key1, key2, {key1, 1, 2}, {key3, 5}]),
                 lists:sort(rebar_state:get(State1, test1))),

    %% Use new value for strings
    "goodbye" = rebar_state:get(State1, test2),

    %% Check that a newvalue of []/"" doesn't override non-string oldvalues
    [key3] = rebar_state:get(State1, test3),
    [] = rebar_state:get(State1, test4),
    [{key5, false}, {key5, true}] = rebar_state:get(State1, test5),
    [{key6, true}, {key6, false}] = rebar_state:get(State1, test6).

same_profile_deduplication(_Config) ->
    RebarConfig = [{test1, [{key1, 1, 2}, key2]},
                   {test2, [foo]},
                   {test3, [key3]},
                   {profiles,
                    [{profile1,
                      [{test1, [{key3, 5}, {key2, "hello"}]},
                       {test2, [bar]},
                       {test3, []}
                    ]}]
                   }],
    State = rebar_state:new(RebarConfig),
    State1 = rebar_state:apply_profiles(State, [profile1, profile1, profile1]),

    ?assertEqual([default, profile1], rebar_state:current_profiles(State1)),
    Test1 = rebar_state:get(State1, test1),

    %% Combine lists
    ?assertEqual(lists:sort([key2, {key1, 1, 2}, {key3, 5}, {key2, "hello"}]),
                 lists:sort(Test1)),

    %% Key2 from profile1 overrides key2 from default profile
    ?assertEqual("hello", proplists:get_value(key2, Test1)),

    %% Check that a newvalue of []/"" doesn't override non-string oldvalues
    ?assertEqual([key3], rebar_state:get(State1, test3)),
    ?assertEqual([bar, foo], rebar_state:get(State1, test2)).

stack_deduplication(_Config) ->
    RebarConfig = [
        {test_key, default},
        {test_list, [ {foo, default}  ]},
        {profiles, [
            {a, [
                {test_key, a},
                {test_list, [ {foo, a} ]}
            ]},
            {b, [
                {test_key, b},
                {test_list, [ {foo, b} ]}
            ]},
            {c, [
                {test_key, c},
                {test_list, [ {foo, c} ]}
            ]},
            {d, [
                {test_key, d},
                {test_list, [ {foo, d} ]}
            ]},
            {e, [
                {test_key, e},
                {test_list, [ {foo, e} ]}
            ]}
        ]}
    ],
    State = rebar_state:new(RebarConfig),
    State1 = rebar_state:apply_profiles(State, [a, b, c, d, e, a, e, b]),
    ?assertEqual(b, rebar_state:get(State1, test_key)),

    TestList = rebar_state:get(State1, test_list),
    ?assertEqual(
        [{foo, b}, {foo, e}, {foo, a}, {foo, d}, {foo, c}, {foo, default} ],
        TestList
    ),
    ?assertEqual(b, proplists:get_value(foo, TestList)).

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

    Path = filename:join([AppDir, "_build", "not_ok", "lib", Name, "ebin"]),
    code:add_patha(Path),

    Mod = list_to_atom("not_a_real_src_" ++ Name),

    true = lists:member({d, not_ok}, proplists:get_value(options, Mod:module_info(compile), [])).

deduplicated_paths(Config) ->
    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("deduplicated_paths_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    RebarConfig = [],
    rebar_test_utils:create_config(AppDir, RebarConfig),
    rebar_test_utils:run_and_check(Config, RebarConfig,
                                   ["as", "a,b,c,d,e,a,e,b", "compile"],
                                   {ok, [{app, Name}]}),

    Path = filename:join([AppDir, "_build", "c+d+a+e+b", "lib", Name, "ebin"]),
    ?assert(filelib:is_dir(Path)).

test_profile_applied_at_completion(Config) ->
    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("test_profile_at_completion_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_eunit_app(AppDir, Name, Vsn, [kernel, stdlib]),

    RebarConfig = [{erl_opts, [{d, some_define}]}],
    rebar_test_utils:create_config(AppDir, RebarConfig),

    {ok, State} = rebar_test_utils:run_and_check(Config,
                                                 RebarConfig,
                                                 ["eunit"],
                                                 return),

    [App] = rebar_state:project_apps(State),
    ErlOpts = rebar_app_info:get(App, erl_opts),
    true = lists:member({d, 'TEST'}, ErlOpts).

test_profile_applied_before_compile(Config) ->
    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("test_profile_before_compile_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_eunit_app(AppDir, Name, Vsn, [kernel, stdlib]),

    RebarConfig = [{erl_opts, [{d, some_define}]}],
    rebar_test_utils:create_config(AppDir, RebarConfig),

    {ok, State} = rebar_test_utils:run_and_check(Config, RebarConfig, ["eunit"], {ok, [{app, Name}]}),
    code:add_paths(rebar_state:code_paths(State, all_deps)),

    S = list_to_atom("not_a_real_src_" ++ Name),
    true = lists:member({d, 'TEST'}, proplists:get_value(options, S:module_info(compile), [])).

test_profile_applied_before_eunit(Config) ->
    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("test_profile_before_eunit_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_eunit_app(AppDir, Name, Vsn, [kernel, stdlib]),

    RebarConfig = [{erl_opts, [{d, some_define}]}],
    rebar_test_utils:create_config(AppDir, RebarConfig),

    {ok, State} = rebar_test_utils:run_and_check(Config, RebarConfig, ["eunit"], {ok, [{app, Name}]}),
    code:add_paths(rebar_state:code_paths(State, all_deps)),

    T = list_to_atom("not_a_real_src_" ++ Name ++ "_tests"),
    true = lists:member({d, 'TEST'}, proplists:get_value(options, T:module_info(compile), [])).

test_profile_applied_to_apps(Config) ->
    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("test_profile_applied_to_apps_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_eunit_app(AppDir, Name, Vsn, [kernel, stdlib]),

    RebarConfig = [{erl_opts, [{d, some_define}]}],
    rebar_test_utils:create_config(AppDir, RebarConfig),

    {ok, State} = rebar_test_utils:run_and_check(Config,
                                                 RebarConfig,
                                                 ["eunit"],
                                                 return),

    Apps = rebar_state:project_apps(State),
    lists:foreach(fun(App) ->
        Opts = rebar_app_info:opts(App),
        ErlOpts = dict:fetch(erl_opts, Opts),
        true = lists:member({d, 'TEST'}, ErlOpts)
    end, Apps).
