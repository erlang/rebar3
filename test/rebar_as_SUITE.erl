-module(rebar_as_SUITE).

-export([suite/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         all/0,
         as_basic/1,
         as_multiple_profiles/1,
         as_multiple_tasks/1,
         as_multiple_profiles_multiple_tasks/1,
         as_comma_placement/1,
         as_comma_then_space/1,
         as_dir_name/1,
         as_with_task_args/1,
         warn_on_empty_profile/1,
         error_on_empty_tasks/1,
         clean_as_profile/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").

suite() -> [].

init_per_suite(Config) -> Config.

end_per_suite(_Config) -> ok.

init_per_testcase(_, Config) ->
    rebar_test_utils:init_rebar_state(Config, "as_").

all() -> [as_basic, as_multiple_profiles, as_multiple_tasks,
          as_multiple_profiles_multiple_tasks,
          as_comma_placement, as_comma_then_space,
          as_dir_name, as_with_task_args,
          warn_on_empty_profile, error_on_empty_tasks, clean_as_profile].

as_basic(Config) ->
    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("as_basic_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    rebar_test_utils:run_and_check(Config,
                                   [],
                                   ["as", "default", "compile"],
                                   {ok, [{app, Name}]}).

as_multiple_profiles(Config) ->
    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("as_multiple_profiles_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    rebar_test_utils:run_and_check(Config,
                                   [],
                                   ["as", "foo", ",", "bar", "compile"],
                                   {ok, [{app, Name}]}).

as_multiple_tasks(Config) ->
    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("as_multiple_tasks_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    rebar_test_utils:run_and_check(Config,
                                   [],
                                   ["as", "foo", "clean", ",", "compile"],
                                   {ok, [{app, Name}]}).

as_multiple_profiles_multiple_tasks(Config) ->
    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("as_multiple_profiles_multiple_tasks_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    rebar_test_utils:run_and_check(Config,
                                   [],
                                   ["as", "foo", ",", "bar", "clean", ",", "compile"],
                                   {ok, [{app, Name}]}).

as_comma_placement(Config) ->
    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("as_comma_placement_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    rebar_test_utils:run_and_check(Config,
                                   [],
                                   ["as", "foo,bar", ",", "baz", ",qux", "compile"],
                                   {ok, [{app, Name}]}).

as_comma_then_space(Config) ->
    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("as_comma_then_space_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    rebar_test_utils:run_and_check(Config,
                                   [],
                                   ["as", "foo,", "bar,", "baz", "compile"],
                                   {ok, [{app, Name}]}).


as_dir_name(Config) ->
    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("as_dir_name_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    rebar_test_utils:run_and_check(Config,
                                   [],
                                   ["as", "foo,bar,baz", "compile"],
                                   {ok, [{app, Name}]}),

    true = filelib:is_dir(filename:join([AppDir, "_build", "foo+bar+baz"])).


as_with_task_args(Config) ->
    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("as_with_task_args_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    rebar_test_utils:run_and_check(Config,
                                   [],
                                   ["as", "default", "compile"],
                                   {ok, [{app, Name}]}),

    rebar_test_utils:run_and_check(Config,
                                   [],
                                   ["as", "default", "clean", "-a"],
                                   {ok, [{app, Name, invalid}]}).


warn_on_empty_profile(Config) ->
    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("as_warn_empty_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    meck:new(rebar_log, [passthrough]),
    rebar_test_utils:run_and_check(Config,
                                   [],
                                   ["as", "fake1,fake2", "compile"],
                                   {ok, [{app, Name}]}),
    History = meck:history(rebar_log),
    ?assert(warn_match("fake1", History)),
    ?assert(warn_match("fake2", History)),
    meck:unload(rebar_log),
    ok.

error_on_empty_tasks(Config) ->
    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("as_error_empty_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    meck:new(rebar_log, [passthrough]),
    rebar_test_utils:run_and_check(Config,
                                   [],
                                   ["as", "default"],
                                   {error, "At least one task must be specified when using `as`"}),
    ok.

warn_match(App, History) ->
    lists:any(
        fun({_, {rebar_log,log, [warn, "No entry for profile ~ts in config.",
            [ArgApp]]}, _}) -> ArgApp =:= App
        ;  (_) ->
            false
        end,
     History).

clean_as_profile(Config) ->
    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("clean_as_profile_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    rebar_test_utils:run_and_check(Config,
                                   [],
                                   ["as", "foo", "compile"],
                                   {ok, [{app, Name, valid}]}),

    rebar_test_utils:run_and_check(Config,
                                   [],
                                   ["clean", "-a", "-p", "foo"],
                                   {ok, [{app, Name, invalid}]}).
