-module(rebar_eunit_SUITE).

-export([suite/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         all/0]).
-export([test_basic_app/1,
         test_multi_app/1,
         test_profile/1,
         test_basic_exports/1,
         test_multi_exports/1,
         test_basic_defines/1,
         test_multi_defines/1,
         test_single_app_flag/1,
         test_multiple_app_flag/1,
         test_nonexistent_app_flag/1,
         test_single_suite_flag/1,
         test_suite_in_app_flag/1,
         test_suite_in_wrong_app_flag/1,
         test_nonexistent_suite_flag/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").

suite() ->
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_, Config) ->
    rebar_test_utils:init_rebar_state(Config, "eunit_").

all() ->
    [test_basic_app, test_multi_app, test_profile,
     test_basic_exports, test_multi_exports,
     test_basic_defines, test_multi_defines,
     test_single_app_flag, test_multiple_app_flag, test_nonexistent_app_flag,
     test_single_suite_flag, test_suite_in_app_flag,
     test_suite_in_wrong_app_flag, test_nonexistent_suite_flag].

test_basic_app(Config) ->
    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("basic_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_eunit_app(AppDir, Name, Vsn, [kernel, stdlib]),

    RebarConfig = [{erl_opts, [{d, some_define}]}],
    rebar_test_utils:run_and_check(Config, RebarConfig, ["eunit"], {ok, [{app, Name}]}).

test_multi_app(Config) ->
    AppDir = ?config(apps, Config),

    Name1 = rebar_test_utils:create_random_name("multi_app1_"),
    Vsn1 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_eunit_app(filename:join([AppDir,Name1]),
                                      Name1,
                                      Vsn1,
                                      [kernel, stdlib]),
    Name2 = rebar_test_utils:create_random_name("multi_app2_"),
    Vsn2 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_eunit_app(filename:join([AppDir,Name2]),
                                      Name2,
                                      Vsn2,
                                      [kernel, stdlib]),

    RebarConfig = [{erl_opts, [{d, some_define}]}],
    rebar_test_utils:run_and_check(Config,
                                   RebarConfig,
                                   ["eunit"],
                                   {ok, [{app, Name1}, {app, Name2}]}).

test_profile(Config) ->
    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("profile_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    RebarConfig = [{erl_opts, [{d, some_define}]},
                  {profiles, [{test, [{erl_opts, [debug_info]}]}]}],
    rebar_test_utils:run_and_check(Config,
                                   RebarConfig,
                                   ["as", "test", "eunit"],
                                   {ok, [{app, Name}]}).

test_basic_exports(Config) ->
    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("basic_exports_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_eunit_app(AppDir, Name, Vsn, [kernel, stdlib]),

    RebarConfig = [{erl_opts, [{d, some_define}]}],
    rebar_test_utils:run_and_check(Config,
                                   RebarConfig,
                                   ["eunit"],
                                   {ok, [{app, Name}]}),

    App = list_to_atom("not_a_real_src_" ++ Name),
    Suite = list_to_atom("not_a_real_src_" ++ Name ++ "_tests"),
    AppExports = App:module_info(exports),
    SuiteExports = Suite:module_info(exports),
    AppExpect = [{some_test_, 0}],
    SuiteExpect = [{some_test_, 0}, {define_test_, 0}],
    lists:foreach(fun(Expect) -> true = lists:member(Expect, AppExports) end, AppExpect),
    lists:foreach(fun(Expect) -> true = lists:member(Expect, SuiteExports) end, SuiteExpect).

test_multi_exports(Config) ->
    AppDir = ?config(apps, Config),

    Name1 = rebar_test_utils:create_random_name("multi_exports_app1_"),
    Vsn1 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_eunit_app(filename:join([AppDir,Name1]),
                                      Name1,
                                      Vsn1,
                                      [kernel, stdlib]),
    Name2 = rebar_test_utils:create_random_name("multi_exports_app2_"),
    Vsn2 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_eunit_app(filename:join([AppDir,Name2]),
                                      Name2,
                                      Vsn2,
                                      [kernel, stdlib]),

    RebarConfig = [{erl_opts, [{d, some_define}]}],
    rebar_test_utils:run_and_check(Config,
                                   RebarConfig,
                                   ["eunit"],
                                   {ok, [{app, Name1}, {app, Name2}]}),

    App1 = list_to_atom("not_a_real_src_" ++ Name1),
    Suite1 = list_to_atom("not_a_real_src_" ++ Name1 ++ "_tests"),
    AppExports1 = App1:module_info(exports),
    SuiteExports1 = Suite1:module_info(exports),
    App2 = list_to_atom("not_a_real_src_" ++ Name2),
    Suite2 = list_to_atom("not_a_real_src_" ++ Name2 ++ "_tests"),
    AppExports2 = App2:module_info(exports),
    SuiteExports2 = Suite2:module_info(exports),
    AppExpect = [{some_test_, 0}],
    SuiteExpect = [{some_test_, 0}, {define_test_, 0}],
    lists:foreach(fun(Expect) -> true = lists:member(Expect, AppExports1) end, AppExpect),
    lists:foreach(fun(Expect) -> true = lists:member(Expect, SuiteExports1) end, SuiteExpect),
    lists:foreach(fun(Expect) -> true = lists:member(Expect, AppExports2) end, AppExpect),
    lists:foreach(fun(Expect) -> true = lists:member(Expect, SuiteExports2) end, SuiteExpect).

test_basic_defines(Config) ->
    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("basic_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_eunit_app(AppDir, Name, Vsn, [kernel, stdlib]),

    RebarConfig = [{erl_opts, [{d, some_define}]}],
    rebar_test_utils:run_and_check(Config, RebarConfig, ["eunit"], {ok, [{app, Name}]}),

    App = list_to_atom("not_a_real_src_" ++ Name),
    Suite = list_to_atom("not_a_real_src_" ++ Name ++ "_tests"),
    AppOpts = proplists:get_value(options, App:module_info(compile), []),
    SuiteOpts = proplists:get_value(options, Suite:module_info(compile), []),
    Expect = [{d, some_define}],
    lists:foreach(fun(Expect) -> true = lists:member(Expect, AppOpts) end, Expect),
    lists:foreach(fun(Expect) -> true = lists:member(Expect, SuiteOpts) end, Expect).

test_multi_defines(Config) ->
    AppDir = ?config(apps, Config),

    Name1 = rebar_test_utils:create_random_name("multi_app1_"),
    Vsn1 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_eunit_app(filename:join([AppDir,Name1]),
                                      Name1,
                                      Vsn1,
                                      [kernel, stdlib]),
    Name2 = rebar_test_utils:create_random_name("multi_app2_"),
    Vsn2 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_eunit_app(filename:join([AppDir,Name2]),
                                      Name2,
                                      Vsn2,
                                      [kernel, stdlib]),

    RebarConfig = [{erl_opts, [{d, some_define}]}],
    rebar_test_utils:run_and_check(Config,
                                   RebarConfig,
                                   ["eunit"],
                                   {ok, [{app, Name1}, {app, Name2}]}),

    App1 = list_to_atom("not_a_real_src_" ++ Name1),
    Suite1 = list_to_atom("not_a_real_src_" ++ Name1 ++ "_tests"),
    AppOpts1 = proplists:get_value(options, App1:module_info(compile), []),
    SuiteOpts1 = proplists:get_value(options, Suite1:module_info(compile), []),
    App2 = list_to_atom("not_a_real_src_" ++ Name2),
    Suite2 = list_to_atom("not_a_real_src_" ++ Name2 ++ "_tests"),
    AppOpts2 = proplists:get_value(options, App2:module_info(compile), []),
    SuiteOpts2 = proplists:get_value(options, Suite2:module_info(compile), []),
    Expect = [{d, some_define}],
    lists:foreach(fun(Expect) -> true = lists:member(Expect, AppOpts1) end, Expect),
    lists:foreach(fun(Expect) -> true = lists:member(Expect, SuiteOpts1) end, Expect),
    lists:foreach(fun(Expect) -> true = lists:member(Expect, AppOpts2) end, Expect),
    lists:foreach(fun(Expect) -> true = lists:member(Expect, SuiteOpts2) end, Expect).

test_single_app_flag(Config) ->
    AppDir = ?config(apps, Config),

    Name1 = rebar_test_utils:create_random_name("multi_exports_app1_"),
    Vsn1 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_eunit_app(filename:join([AppDir,Name1]),
                                      Name1,
                                      Vsn1,
                                      [kernel, stdlib]),
    Name2 = rebar_test_utils:create_random_name("multi_exports_app2_"),
    Vsn2 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_eunit_app(filename:join([AppDir,Name2]),
                                      Name2,
                                      Vsn2,
                                      [kernel, stdlib]),

    RebarConfig = [{erl_opts, [{d, some_define}]}],
    rebar_test_utils:run_and_check(Config,
                                   RebarConfig,
                                   ["eunit", "--app=" ++ Name1],
                                   {ok, [{app, Name1}, {app, Name2}]}),

    Suite1 = list_to_atom("not_a_real_src_" ++ Name1 ++ "_tests"),
    {module, Suite1} = code:ensure_loaded(Suite1),
    Suite2 = list_to_atom("not_a_real_src_" ++ Name2 ++ "_tests"),
    {error, nofile} = code:ensure_loaded(Suite2).

test_multiple_app_flag(Config) ->
    AppDir = ?config(apps, Config),

    Name1 = rebar_test_utils:create_random_name("multi_exports_app1_"),
    Vsn1 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_eunit_app(filename:join([AppDir,Name1]),
                                      Name1,
                                      Vsn1,
                                      [kernel, stdlib]),
    Name2 = rebar_test_utils:create_random_name("multi_exports_app2_"),
    Vsn2 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_eunit_app(filename:join([AppDir,Name2]),
                                      Name2,
                                      Vsn2,
                                      [kernel, stdlib]),

    RebarConfig = [{erl_opts, [{d, some_define}]}],
    rebar_test_utils:run_and_check(Config,
                                   RebarConfig,
                                   ["eunit", "--app=" ++ Name1 ++ "," ++ Name2],
                                   {ok, [{app, Name1}, {app, Name2}]}),

    Suite1 = list_to_atom("not_a_real_src_" ++ Name1 ++ "_tests"),
    {module, Suite1} = code:ensure_loaded(Suite1),
    Suite2 = list_to_atom("not_a_real_src_" ++ Name2 ++ "_tests"),
    {module, Suite2} = code:ensure_loaded(Suite2).

test_nonexistent_app_flag(Config) ->
    AppDir = ?config(apps, Config),

    Name1 = rebar_test_utils:create_random_name("multi_exports_app1_"),
    Vsn1 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_eunit_app(filename:join([AppDir,Name1]),
                                      Name1,
                                      Vsn1,
                                      [kernel, stdlib]),
    Name2 = rebar_test_utils:create_random_name("multi_exports_app2_"),
    Vsn2 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_eunit_app(filename:join([AppDir,Name2]),
                                      Name2,
                                      Vsn2,
                                      [kernel, stdlib]),

    RebarConfig = [{erl_opts, [{d, some_define}]}],
    {error, {_, Error}} = rebar_test_utils:run_and_check(Config,
                                                         RebarConfig,
                                                         ["eunit", "--app=not_a_real_app"],
                                                         return),

    Error = {error_running_tests, "Application `not_a_real_app' not found in project."}.

test_single_suite_flag(Config) ->
    AppDir = ?config(apps, Config),

    Name1 = rebar_test_utils:create_random_name("multi_exports_app1_"),
    Vsn1 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_eunit_app(filename:join([AppDir,Name1]),
                                      Name1,
                                      Vsn1,
                                      [kernel, stdlib]),
    Name2 = rebar_test_utils:create_random_name("multi_exports_app2_"),
    Vsn2 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_eunit_app(filename:join([AppDir,Name2]),
                                      Name2,
                                      Vsn2,
                                      [kernel, stdlib]),

    RebarConfig = [{erl_opts, [{d, some_define}]}],
    rebar_test_utils:run_and_check(Config,
                                   RebarConfig,
                                   ["eunit", "--suite=not_a_real_src_" ++ Name1],
                                   {ok, [{app, Name1}, {app, Name2}]}),

    Suite1 = list_to_atom("not_a_real_src_" ++ Name1 ++ "_tests"),
    {module, Suite1} = code:ensure_loaded(Suite1).

test_suite_in_app_flag(Config) ->
    AppDir = ?config(apps, Config),

    Name1 = rebar_test_utils:create_random_name("multi_exports_app1_"),
    Vsn1 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_eunit_app(filename:join([AppDir,Name1]),
                                      Name1,
                                      Vsn1,
                                      [kernel, stdlib]),
    Name2 = rebar_test_utils:create_random_name("multi_exports_app2_"),
    Vsn2 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_eunit_app(filename:join([AppDir,Name2]),
                                      Name2,
                                      Vsn2,
                                      [kernel, stdlib]),

    RebarConfig = [{erl_opts, [{d, some_define}]}],
    rebar_test_utils:run_and_check(Config,
                                   RebarConfig,
                                   ["eunit",
                                    "--app=" ++ Name1,
                                    "--suite=not_a_real_src_" ++ Name1],
                                   {ok, [{app, Name1}, {app, Name2}]}),

    Suite1 = list_to_atom("not_a_real_src_" ++ Name1 ++ "_tests"),
    {module, Suite1} = code:ensure_loaded(Suite1),
    Suite2 = list_to_atom("not_a_real_src_" ++ Name2 ++ "_tests"),
    {error, nofile} = code:ensure_loaded(Suite2).

test_suite_in_wrong_app_flag(Config) ->
    AppDir = ?config(apps, Config),

    Name1 = rebar_test_utils:create_random_name("multi_exports_app1_"),
    Vsn1 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_eunit_app(filename:join([AppDir,Name1]),
                                      Name1,
                                      Vsn1,
                                      [kernel, stdlib]),
    Name2 = rebar_test_utils:create_random_name("multi_exports_app2_"),
    Vsn2 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_eunit_app(filename:join([AppDir,Name2]),
                                      Name2,
                                      Vsn2,
                                      [kernel, stdlib]),

    RebarConfig = [{erl_opts, [{d, some_define}]}],
    {error, {_, Error}} = rebar_test_utils:run_and_check(Config,
                                                         RebarConfig,
                                                         ["eunit",
                                                         "--app=" ++ Name1,
                                                         "--suite=not_a_real_src_" ++ Name2],
                                                         return),

    Error = {error_running_tests, "Module `not_a_real_src_" ++
                                  Name2 ++
                                  "' not found in applications."}.

test_nonexistent_suite_flag(Config) ->
    AppDir = ?config(apps, Config),

    Name1 = rebar_test_utils:create_random_name("multi_exports_app1_"),
    Vsn1 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_eunit_app(filename:join([AppDir,Name1]),
                                      Name1,
                                      Vsn1,
                                      [kernel, stdlib]),
    Name2 = rebar_test_utils:create_random_name("multi_exports_app2_"),
    Vsn2 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_eunit_app(filename:join([AppDir,Name2]),
                                      Name2,
                                      Vsn2,
                                      [kernel, stdlib]),

    RebarConfig = [{erl_opts, [{d, some_define}]}],
    {error, {_, Error}} = rebar_test_utils:run_and_check(Config,
                                                         RebarConfig,
                                                         ["eunit", "--suite=not_a_real_module"],
                                                         return),

    Error = {error_running_tests, "Module `not_a_real_module' not found in applications."}.
