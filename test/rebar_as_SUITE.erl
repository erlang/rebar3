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
         as_dir_name/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").

suite() -> [].

init_per_suite(Config) -> Config.

end_per_suite(_Config) -> ok.

init_per_testcase(_, Config) ->
    rebar_test_utils:init_rebar_state(Config, "as_").

all() -> [as_basic, as_multiple_profiles, as_multiple_tasks,
          as_multiple_profiles_multiple_tasks, as_comma_placement,
          as_dir_name].

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

