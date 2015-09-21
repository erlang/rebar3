-module(rebar_eunit_SUITE).

-export([suite/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         all/0]).
-export([basic_app/1,
         multi_app/1,
         profile/1,
         basic_exports/1,
         multi_exports/1,
         basic_defines/1,
         multi_defines/1,
         single_app_flag/1,
         multiple_app_flag/1,
         single_module_flag/1,
         nonexistent_module_flag/1,
         single_file_flag/1,
         multiple_file_flag/1,
         nonexistent_file_flag/1,
         single_dir_flag/1,
         multiple_dir_flag/1,
         nonexistent_dir_flag/1,
         config_tests/1,
         nonexistent_tests/1,
         eunit_compile_opts/1,
         eunit_first_files/1]).

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
    [basic_app, multi_app, profile,
     basic_exports, multi_exports,
     basic_defines, multi_defines,
     single_app_flag, multiple_app_flag,
     single_module_flag, nonexistent_module_flag,
     single_file_flag, multiple_file_flag, nonexistent_file_flag,
     single_dir_flag, multiple_dir_flag, nonexistent_dir_flag,
     config_tests, nonexistent_tests,
     eunit_compile_opts, eunit_first_files].

basic_app(Config) ->
    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("basic_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_eunit_app(AppDir, Name, Vsn, [kernel, stdlib]),

    RebarConfig = [{erl_opts, [{d, some_define}]}],
    rebar_test_utils:run_and_check(Config, RebarConfig, ["eunit"], {ok, [{app, Name}]}).

multi_app(Config) ->
    AppDir = ?config(apps, Config),

    Name1 = rebar_test_utils:create_random_name("multi_app1_"),
    Vsn1 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_eunit_app(filename:join([AppDir,"apps",Name1]),
                                      Name1,
                                      Vsn1,
                                      [kernel, stdlib]),
    Name2 = rebar_test_utils:create_random_name("multi_app2_"),
    Vsn2 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_eunit_app(filename:join([AppDir,"apps",Name2]),
                                      Name2,
                                      Vsn2,
                                      [kernel, stdlib]),

    RebarConfig = [{erl_opts, [{d, some_define}]}],
    rebar_test_utils:run_and_check(Config,
                                   RebarConfig,
                                   ["eunit"],
                                   {ok, [{app, Name1}, {app, Name2}]}).

profile(Config) ->
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

basic_exports(Config) ->
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

multi_exports(Config) ->
    AppDir = ?config(apps, Config),

    Name1 = rebar_test_utils:create_random_name("multi_exports_app1_"),
    Vsn1 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_eunit_app(filename:join([AppDir,"apps",Name1]),
                                      Name1,
                                      Vsn1,
                                      [kernel, stdlib]),
    Name2 = rebar_test_utils:create_random_name("multi_exports_app2_"),
    Vsn2 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_eunit_app(filename:join([AppDir,"apps",Name2]),
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

basic_defines(Config) ->
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
    lists:foreach(fun(E) -> true = lists:member(E, AppOpts) end, Expect),
    lists:foreach(fun(E) -> true = lists:member(E, SuiteOpts) end, Expect).

multi_defines(Config) ->
    AppDir = ?config(apps, Config),

    Name1 = rebar_test_utils:create_random_name("multi_app1_"),
    Vsn1 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_eunit_app(filename:join([AppDir,"apps",Name1]),
                                      Name1,
                                      Vsn1,
                                      [kernel, stdlib]),
    Name2 = rebar_test_utils:create_random_name("multi_app2_"),
    Vsn2 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_eunit_app(filename:join([AppDir,"apps",Name2]),
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
    lists:foreach(fun(E) -> true = lists:member(E, AppOpts1) end, Expect),
    lists:foreach(fun(E) -> true = lists:member(E, SuiteOpts1) end, Expect),
    lists:foreach(fun(E) -> true = lists:member(E, AppOpts2) end, Expect),
    lists:foreach(fun(E) -> true = lists:member(E, SuiteOpts2) end, Expect).

single_app_flag(Config) ->
    AppDir = ?config(apps, Config),

    Name1 = rebar_test_utils:create_random_name("multi_exports_app1_"),
    Vsn1 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_eunit_app(filename:join([AppDir,"apps",Name1]),
                                      Name1,
                                      Vsn1,
                                      [kernel, stdlib]),
    Name2 = rebar_test_utils:create_random_name("multi_exports_app2_"),
    Vsn2 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_eunit_app(filename:join([AppDir,"apps",Name2]),
                                      Name2,
                                      Vsn2,
                                      [kernel, stdlib]),

    BareSuite = io_lib:format("-module(all_tests).\n"
                              "-compile(export_all).\n"
                              "-include_lib(\"eunit/include/eunit.hrl\").\n"
                              "some_test_() -> ?_assert(true).\n"
                              "define_test_() -> ?_assertEqual(true, ?some_define).\n", []),
    FileName = filename:join([AppDir, "test", "all_tests.erl"]),
    ok = filelib:ensure_dir(FileName),
    ok = ec_file:write(FileName, BareSuite),

    RebarConfig = [{erl_opts, [{d, some_define}]}],
    rebar_test_utils:run_and_check(Config,
                                   RebarConfig,
                                   ["eunit", "--application=" ++ Name1],
                                   {ok, [{app, Name1}, {app, Name2}]}),

    Suite1 = list_to_atom("not_a_real_src_" ++ Name1 ++ "_tests"),
    {module, Suite1} = code:ensure_loaded(Suite1),
    Suite2 = list_to_atom("not_a_real_src_" ++ Name2 ++ "_tests"),
    {error, nofile} = code:ensure_loaded(Suite2),
    {error, nofile} = code:ensure_loaded(all_tests).

multiple_app_flag(Config) ->
    AppDir = ?config(apps, Config),

    Name1 = rebar_test_utils:create_random_name("multi_exports_app1_"),
    Vsn1 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_eunit_app(filename:join([AppDir,"apps",Name1]),
                                      Name1,
                                      Vsn1,
                                      [kernel, stdlib]),
    Name2 = rebar_test_utils:create_random_name("multi_exports_app2_"),
    Vsn2 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_eunit_app(filename:join([AppDir,"apps",Name2]),
                                      Name2,
                                      Vsn2,
                                      [kernel, stdlib]),

    BareSuite = io_lib:format("-module(all_tests).\n"
                              "-compile(export_all).\n"
                              "-include_lib(\"eunit/include/eunit.hrl\").\n"
                              "some_test_() -> ?_assert(true).\n"
                              "define_test_() -> ?_assertEqual(true, ?some_define).\n", []),
    FileName = filename:join([AppDir, "test", "all_tests.erl"]),
    ok = filelib:ensure_dir(FileName),
    ok = ec_file:write(FileName, BareSuite),

    RebarConfig = [{erl_opts, [{d, some_define}]}],
    rebar_test_utils:run_and_check(Config,
                                   RebarConfig,
                                   ["eunit", "--application=" ++ Name1 ++ "," ++ Name2],
                                   {ok, [{app, Name1}, {app, Name2}]}),

    Suite1 = list_to_atom("not_a_real_src_" ++ Name1 ++ "_tests"),
    {module, Suite1} = code:ensure_loaded(Suite1),
    Suite2 = list_to_atom("not_a_real_src_" ++ Name2 ++ "_tests"),
    {module, Suite2} = code:ensure_loaded(Suite2),
    {error, nofile} = code:ensure_loaded(all_tests).

single_module_flag(Config) ->
    AppDir = ?config(apps, Config),

    Name1 = rebar_test_utils:create_random_name("multi_exports_app1_"),
    Vsn1 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_eunit_app(filename:join([AppDir,"apps",Name1]),
                                      Name1,
                                      Vsn1,
                                      [kernel, stdlib]),
    Name2 = rebar_test_utils:create_random_name("multi_exports_app2_"),
    Vsn2 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_eunit_app(filename:join([AppDir,"apps",Name2]),
                                      Name2,
                                      Vsn2,
                                      [kernel, stdlib]),

    RebarConfig = [{erl_opts, [{d, some_define}]}],
    rebar_test_utils:run_and_check(Config,
                                   RebarConfig,
                                   ["eunit", "--module=not_a_real_src_" ++ Name1],
                                   {ok, [{app, Name1}, {app, Name2}]}),

    Suite1 = list_to_atom("not_a_real_src_" ++ Name1 ++ "_tests"),
    {module, Suite1} = code:ensure_loaded(Suite1).

nonexistent_module_flag(Config) ->
    AppDir = ?config(apps, Config),

    Name1 = rebar_test_utils:create_random_name("multi_exports_app1_"),
    Vsn1 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_eunit_app(filename:join([AppDir,"apps",Name1]),
                                      Name1,
                                      Vsn1,
                                      [kernel, stdlib]),
    Name2 = rebar_test_utils:create_random_name("multi_exports_app2_"),
    Vsn2 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_eunit_app(filename:join([AppDir,"apps",Name2]),
                                      Name2,
                                      Vsn2,
                                      [kernel, stdlib]),

    RebarConfig = [{erl_opts, [{d, some_define}]}],
    {error, {rebar_prv_eunit, Error}} = rebar_test_utils:run_and_check(Config,
                                                                       RebarConfig,
                                                                       ["eunit", "-e", "--module=not_a_real_module"],
                                                                       return),

    Error = {eunit_test_errors, ["Module `not_a_real_module' not found in applications."]}.

single_file_flag(Config) ->
    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("single_file_flag_app_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_eunit_app(AppDir, Name, Vsn, [kernel, stdlib]),

    File = filename:join([AppDir, "_build", "test", "lib", Name, "ebin", "not_a_real_src_" ++ Name ++ "_tests.beam"]),

    RebarConfig = [{erl_opts, [{d, some_define}]}],
    rebar_test_utils:run_and_check(Config,
                                   RebarConfig,
                                   ["eunit", "--file=" ++ File],
                                   {ok, [{app, Name}]}),

    Mod = list_to_atom("not_a_real_src_" ++ Name ++ "_tests"),
    {module, Mod} = code:ensure_loaded(Mod).

multiple_file_flag(Config) ->
    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("multiple_file_flag_app_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_eunit_app(AppDir, Name, Vsn, [kernel, stdlib]),

    File1 = filename:join([AppDir, "_build", "test", "lib", Name, "ebin", "not_a_real_src_" ++ Name ++ "_tests.beam"]),
    File2 = filename:join([AppDir, "_build", "test", "lib", Name, "ebin", "not_a_real_src_" ++ Name ++ ".beam"]),


    RebarConfig = [{erl_opts, [{d, some_define}]}],
    rebar_test_utils:run_and_check(Config,
                                   RebarConfig,
                                   ["eunit", "--file=" ++ File1 ++ "," ++ File2],
                                   {ok, [{app, Name}]}),

    Mod1 = list_to_atom("not_a_real_src_" ++ Name ++ "_tests"),
    {module, Mod1} = code:ensure_loaded(Mod1),

    Mod2 = list_to_atom("not_a_real_src_" ++ Name),
    {module, Mod2} = code:ensure_loaded(Mod2).

nonexistent_file_flag(Config) ->
    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("nonexistent_file_flag_app_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_eunit_app(AppDir,
                                      Name,
                                      Vsn,
                                      [kernel, stdlib]),

    RebarConfig = [{erl_opts, [{d, some_define}]}],
    {error, {rebar_prv_eunit, Error}} = rebar_test_utils:run_and_check(Config,
                                                                       RebarConfig,
                                                                       ["eunit", "-e", "--file=not_a_real_file.beam"],
                                                                       return),

    Error = {eunit_test_errors, ["File `not_a_real_file.beam' not found."]}.

single_dir_flag(Config) ->
    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("single_file_flag_app_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_eunit_app(AppDir, Name, Vsn, [kernel, stdlib]),

    Dir = filename:join([AppDir, "_build", "test", "lib", Name, "ebin"]),

    RebarConfig = [{erl_opts, [{d, some_define}]}],
    rebar_test_utils:run_and_check(Config,
                                   RebarConfig,
                                   ["eunit", "--dir=" ++ Dir],
                                   {ok, [{app, Name}]}),

    Mod = list_to_atom("not_a_real_src_" ++ Name ++ "_tests"),
    {module, Mod} = code:ensure_loaded(Mod).

multiple_dir_flag(Config) ->
    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("multiple_file_flag_app_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_eunit_app(AppDir, Name, Vsn, [kernel, stdlib]),

    Dir1 = filename:join([AppDir, "_build", "test", "lib", Name, "ebin"]),
    Dir2 = filename:join([AppDir, "_build", "test", "lib", Name, "ebin"]),


    RebarConfig = [{erl_opts, [{d, some_define}]}],
    rebar_test_utils:run_and_check(Config,
                                   RebarConfig,
                                   ["eunit", "--dir=" ++ Dir1 ++ "," ++ Dir2],
                                   {ok, [{app, Name}]}),

    Mod1 = list_to_atom("not_a_real_src_" ++ Name ++ "_tests"),
    {module, Mod1} = code:ensure_loaded(Mod1),

    Mod2 = list_to_atom("not_a_real_src_" ++ Name),
    {module, Mod2} = code:ensure_loaded(Mod2).

nonexistent_dir_flag(Config) ->
    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("nonexistent_file_flag_app_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_eunit_app(AppDir,
                                      Name,
                                      Vsn,
                                      [kernel, stdlib]),

    RebarConfig = [{erl_opts, [{d, some_define}]}],
    {error, {rebar_prv_eunit, Error}} = rebar_test_utils:run_and_check(Config,
                                                                       RebarConfig,
                                                                       ["eunit", "-e", "--dir=not_a_real_dir"],
                                                                       return),

    Error = {eunit_test_errors, ["Directory `not_a_real_dir' not found."]}.

config_tests(Config) ->
    AppDir = ?config(apps, Config),

    Name1 = rebar_test_utils:create_random_name("config_tests_app1_"),
    Vsn1 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_eunit_app(filename:join([AppDir,"apps",Name1]),
                                      Name1,
                                      Vsn1,
                                      [kernel, stdlib]),
    Name2 = rebar_test_utils:create_random_name("config_tests_app2_"),
    Vsn2 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_eunit_app(filename:join([AppDir,"apps",Name2]),
                                      Name2,
                                      Vsn2,
                                      [kernel, stdlib]),

    BareSuite = io_lib:format("-module(all_tests).\n"
                              "-compile(export_all).\n"
                              "-include_lib(\"eunit/include/eunit.hrl\").\n"
                              "some_test_() -> ?_assert(true).\n"
                              "define_test_() -> ?_assertEqual(true, ?some_define).\n", []),
    FileName = filename:join([AppDir, "test", "all_tests.erl"]),
    ok = filelib:ensure_dir(FileName),
    ok = ec_file:write(FileName, BareSuite),

    RebarConfig = [{erl_opts, [{d, some_define}]}, {eunit_tests, [{application, list_to_atom(Name1)}]}],
    rebar_test_utils:run_and_check(Config,
                                   RebarConfig,
                                   ["eunit"],
                                   {ok, [{app, Name1}, {app, Name2}]}),

    Suite1 = list_to_atom("not_a_real_src_" ++ Name1 ++ "_tests"),
    {module, Suite1} = code:ensure_loaded(Suite1),
    Suite2 = list_to_atom("not_a_real_src_" ++ Name2 ++ "_tests"),
    {error, nofile} = code:ensure_loaded(Suite2),
    {error, nofile} = code:ensure_loaded(all_tests).

nonexistent_tests(Config) ->
    AppDir = ?config(apps, Config),

    Name1 = rebar_test_utils:create_random_name("multi_exports_app1_"),
    Vsn1 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_eunit_app(filename:join([AppDir,Name1]),
                                      Name1,
                                      Vsn1,
                                      [kernel, stdlib]),
    Name2 = rebar_test_utils:create_random_name("multi_exports_app2_"),
    Vsn2 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_eunit_app(filename:join([AppDir,"apps",Name2]),
                                      Name2,
                                      Vsn2,
                                      [kernel, stdlib]),

    RebarConfig = [{erl_opts, [{d, some_define}]}],
    {error, {rebar_prv_eunit, Error}} = rebar_test_utils:run_and_check(Config,
                                                                       RebarConfig,
                                                                       ["eunit",
                                                                        "-e",
                                                                        "--app=not_a_real_app",
                                                                        "--application=not_a_real_application",
                                                                        "--module=not_a_real_module",
                                                                        "--suite=not_a_real_suite",
                                                                        "--file=not_a_real_file.beam",
                                                                        "--dir=not_a_real_dir"],
                                                                       return),

    Error = {eunit_test_errors, ["Application `not_a_real_app' not found in project.",
                                 "Application `not_a_real_application' not found in project.",
                                 "Directory `not_a_real_dir' not found.",
                                 "File `not_a_real_file.beam' not found.",
                                 "Module `not_a_real_module' not found in applications.",
                                 "Module `not_a_real_suite' not found in applications."]}.

eunit_compile_opts(Config) ->
    AppDir = ?config(apps, Config),

    Name1 = rebar_test_utils:create_random_name("multi_app1_"),
    Vsn1 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_eunit_app(filename:join([AppDir,"apps",Name1]),
                                      Name1,
                                      Vsn1,
                                      [kernel, stdlib]),
    Name2 = rebar_test_utils:create_random_name("multi_app2_"),
    Vsn2 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_eunit_app(filename:join([AppDir,"apps",Name2]),
                                      Name2,
                                      Vsn2,
                                      [kernel, stdlib]),

    RebarConfig = [{erl_opts, [{d, some_define}]}, {eunit_compile_opts, [{d, some_other_define}]}],
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

    Expect = [{d, some_other_define}, {d, some_define}],
    lists:foreach(fun(E) -> true = lists:member(E, AppOpts1) end, Expect),
    lists:foreach(fun(E) -> true = lists:member(E, SuiteOpts1) end, Expect),
    lists:foreach(fun(E) -> true = lists:member(E, AppOpts2) end, Expect),
    lists:foreach(fun(E) -> true = lists:member(E, SuiteOpts2) end, Expect).

eunit_first_files(Config) ->
    AppDir = ?config(apps, Config),

    Name1 = rebar_test_utils:create_random_name("multi_app1_"),
    Vsn1 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_eunit_app(filename:join([AppDir,"apps",Name1]),
                                      Name1,
                                      Vsn1,
                                      [kernel, stdlib]),
    Name2 = rebar_test_utils:create_random_name("multi_app2_"),
    Vsn2 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_eunit_app(filename:join([AppDir,"apps",Name2]),
                                      Name2,
                                      Vsn2,
                                      [kernel, stdlib]),

    ErlFirstFiles = ["not_a_real_src_" ++ Name1, "not_a_real_src_" ++ Name2],
    EUnitFirstFiles = ["not_a_real_src_" ++ Name1 ++ "_tests", "not_a_real_src_" ++ Name2 ++ "_tests"],

    RebarConfig = [{erl_opts, [{d, some_define}]},
                   {erl_first_files, ErlFirstFiles},
                   {eunit_first_files, EUnitFirstFiles}],
    {ok, State} = rebar_test_utils:run_and_check(Config,
                                                 RebarConfig,
                                                 ["eunit"],
                                                 {ok, [{app, Name1}, {app, Name2}]}),

    AllFirstFiles = EUnitFirstFiles ++ ErlFirstFiles,
    Apps = rebar_state:project_apps(State),
    lists:foreach(fun(App) -> AllFirstFiles = rebar_app_info:get(App, erl_first_files) end,
                  Apps).