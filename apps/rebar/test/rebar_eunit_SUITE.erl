-module(rebar_eunit_SUITE).

-export([all/0, groups/0]).
-export([init_per_suite/1, end_per_suite/1]).
-export([init_per_group/2, end_per_group/2]).
-export([basic_app_compiles/1, basic_app_files/1]).
-export([basic_app_exports/1, basic_app_testset/1]).
-export([basic_app_eunit_macro/1]).
-export([multi_app_compiles/1, multi_app_files/1]).
-export([multi_app_exports/1, multi_app_testset/1]).
-export([multi_app_eunit_macro/1]).
-export([eunit_tests/1, eunit_opts/1, eunit_first_files/1]).
-export([single_application_arg/1, multi_application_arg/1, missing_application_arg/1]).
-export([single_module_arg/1, multi_module_arg/1, missing_module_arg/1]).
-export([single_test_arg/1, multi_test_arg/1, missing_test_arg/1]).
-export([single_suite_arg/1, multi_suite_arg/1, missing_suite_arg/1]).
-export([single_generator_arg/1, multi_generator_arg/1, missing_generator_arg/1]).
-export([single_file_arg/1, multi_file_arg/1, missing_file_arg/1]).
-export([single_dir_arg/1, multi_dir_arg/1, missing_dir_arg/1]).
-export([multiple_arg_composition/1, multiple_arg_errors/1]).
-export([misspecified_eunit_tests/1]).
-export([misspecified_eunit_compile_opts/1]).
-export([misspecified_eunit_first_files/1]).
-export([alternate_test_regex/1]).
-export([syscfg_app_opts/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").

all() ->
    [{group, basic_app}, {group, multi_app}, {group, cmd_line_args},
     misspecified_eunit_tests,
     misspecified_eunit_compile_opts,
     misspecified_eunit_first_files,
     alternate_test_regex, syscfg_app_opts].

groups() ->
    [{basic_app, [sequence], [basic_app_compiles, {group, basic_app_results}]},
     {basic_app_results, [], [basic_app_files,
                              basic_app_exports,
                              basic_app_testset,
                              basic_app_eunit_macro]},
     {multi_app, [sequence], [multi_app_compiles, {group, multi_app_results}]},
     {multi_app_results, [], [multi_app_files,
                              multi_app_exports,
                              multi_app_testset,
                              multi_app_eunit_macro]},
     {cmd_line_args, [], [eunit_tests, eunit_opts, eunit_first_files,
                          single_application_arg, multi_application_arg, missing_application_arg,
                          single_module_arg, multi_module_arg, missing_module_arg,
                          single_test_arg, multi_test_arg, missing_test_arg,
                          single_suite_arg, multi_suite_arg, missing_suite_arg,
                          single_generator_arg, multi_generator_arg, missing_generator_arg,
                          single_file_arg, multi_file_arg, missing_file_arg,
                          single_dir_arg, multi_dir_arg, missing_dir_arg,
                          multiple_arg_composition, multiple_arg_errors]}].

%% this just unzips the example apps used by tests to the priv dir for later use
init_per_suite(Config) ->
    PrivDir = ?config(priv_dir, Config),
    DataDir = ?config(data_dir, Config),
    ok = ec_file:copy(filename:join([DataDir, "basic_app.zip"]), filename:join([PrivDir, "basic_app.zip"])),
    {ok, _} = zip:extract(filename:join([PrivDir, "basic_app.zip"]), [{cwd, PrivDir}]),
    ok = ec_file:copy(filename:join([DataDir, "multi_app.zip"]), filename:join([PrivDir, "multi_app.zip"])),
    {ok, _} = zip:extract(filename:join([PrivDir, "multi_app.zip"]), [{cwd, PrivDir}]),
    ok = ec_file:copy(filename:join([DataDir, "syscfg_app.zip"]), filename:join([PrivDir, "syscfg_app.zip"])),
    {ok, _} = zip:extract(filename:join([PrivDir, "syscfg_app.zip"]), [{cwd, PrivDir}]),
    Config.

end_per_suite(Config) -> Config.

init_per_group(basic_app, Config) ->
    GroupState = rebar_test_utils:init_rebar_state(Config, "basic_app_"),

    AppDir = ?config(apps, GroupState),
    PrivDir = ?config(priv_dir, GroupState),

    AppDirs = ["src", "include", "test"],

    lists:foreach(fun(F) -> ec_file:copy(filename:join([PrivDir, "basic_app", F]),
                                         filename:join([AppDir, F]),
                                         [recursive]) end, AppDirs),

    RebarConfig = [{erl_opts, [{d, config_define}]}, {eunit_compile_opts, [{d, eunit_compile_define}]}],

    {ok, State} = rebar_test_utils:run_and_check(GroupState, RebarConfig, ["as", "test", "lock"], return),

    [{result, State}|GroupState];
init_per_group(multi_app, Config) ->
    GroupState = rebar_test_utils:init_rebar_state(Config, "multi_app_"),

    AppDir = ?config(apps, GroupState),
    PrivDir = ?config(priv_dir, GroupState),

    AppDirs = ["apps", "test"],

    lists:foreach(fun(F) -> ec_file:copy(filename:join([PrivDir, "multi_app", F]),
                                         filename:join([AppDir, F]),
                                         [recursive]) end, AppDirs),

    RebarConfig = [{erl_opts, [{d, config_define}]}, {eunit_compile_opts, [{d, eunit_compile_define}]}],

    {ok, State} = rebar_test_utils:run_and_check(GroupState, RebarConfig, ["as", "test", "lock"], return),

    [{result, State}|GroupState];
init_per_group(cmd_line_args, Config) ->
    GroupState = rebar_test_utils:init_rebar_state(Config, "cmd_line_args_"),

    AppDir = ?config(apps, GroupState),
    PrivDir = ?config(priv_dir, GroupState),

    AppDirs = ["apps", "test"],

    lists:foreach(fun(F) -> ec_file:copy(filename:join([PrivDir, "multi_app", F]),
                                         filename:join([AppDir, F]),
                                         [recursive]) end, AppDirs),

    RebarConfig = [{erl_opts, [{d, config_define}]},
                   {eunit_compile_opts, [{d, eunit_compile_define}]},
                   %% test set not supported by cmd line args
                   {eunit_tests, [{test, multi_app_bar, sanity_test},
                                  {test, multi_app_baz, sanity_test}]},
                   {eunit_opts, [verbose]},
                   {eunit_first_files, [filename:join(["apps", "multi_app_bar", "test", "multi_app_bar_tests_helper.erl"]),
                                        filename:join(["apps", "multi_app_baz", "test", "multi_app_baz_tests_helper.erl"])]}],

    {ok, State} = rebar_test_utils:run_and_check(GroupState, RebarConfig, ["eunit"], return),

    [{result, State}|GroupState];
init_per_group(_, Config) -> Config.

end_per_group(_, Config) -> Config.



%% === tests for a single application at the root of a project ===

%% check that project compiles properly
basic_app_compiles(Config) ->
    AppDir = ?config(apps, Config),
    State = ?config(result, Config),

    {ok, _} = rebar_prv_eunit:do(State),

    rebar_test_utils:check_results(AppDir, [{app, "basic_app"}], "*").

%% check that all files expected to be present are present
basic_app_files(Config) ->
    AppDir = ?config(apps, Config),

    lists:foreach(fun(F) -> true = ec_file:exists(filename:join([AppDir, "_build", "test", "lib", "basic_app", "ebin", F])) end,
                  ["basic_app.app", "basic_app.beam"]),
    lists:foreach(fun(F) -> true = ec_file:exists(filename:join([AppDir, "_build", "test", "lib", "basic_app", "test", F])) end,
                  ["basic_app_tests.beam", "basic_app_tests_helper.beam"]).

%% check that the correct tests are exported from modules for project
%% note that this implies `TEST` is set correctly
basic_app_exports(_Config) ->
    Tests = fun(Mod) ->
        begin
            Path = code:which(Mod),
            {ok, {Mod, [{exports, Ex}]}} = beam_lib:chunks(Path, [exports]),
            true = lists:member({sanity_test, 0}, Ex)
        end
    end,
    Helpers = fun(Mod) ->
        begin
            Path = code:which(Mod),
            {ok, {Mod, [{exports, Ex}]}} = beam_lib:chunks(Path, [exports]),
            true = lists:member({help, 0}, Ex)
        end
    end,
    lists:foreach(Tests, [basic_app, basic_app_tests]),
    lists:foreach(Helpers, [basic_app_tests_helper]).

%% check that the correct tests are schedule to run for project
basic_app_testset(Config) ->
    Result = ?config(result, Config),

    Set = {ok, [{application, basic_app},
                {module, basic_app_tests_helper}]},
    Set = rebar_prv_eunit:prepare_tests(Result).

basic_app_eunit_macro(_Config) ->
    Macro = fun(Mod) ->
        begin
            Path = code:which(Mod),
            {ok, {Mod, [{compile_info, CompileInfo}]}} = beam_lib:chunks(Path, [compile_info]),
            Opts = proplists:get_value(options, CompileInfo, []),
            true = lists:member({d, 'EUNIT'}, Opts)
        end
    end,
    lists:foreach(Macro, [basic_app, basic_app_tests, basic_app_tests_helper]).

%% === tests for multiple applications in the `apps' directory of a project ===

%% check that project compiles properly
multi_app_compiles(Config) ->
    AppDir = ?config(apps, Config),
    State = ?config(result, Config),

    {ok, _} = rebar_prv_eunit:do(State),

    rebar_test_utils:check_results(AppDir, [{app, "multi_app_bar"}, {app, "multi_app_baz"}], "*").

%% check that all files expected to be present are present
multi_app_files(Config) ->
    AppDir = ?config(apps, Config),

    lists:foreach(fun(F) -> true = ec_file:exists(filename:join([AppDir, "_build", "test", "lib", "multi_app_bar", "ebin", F])) end,
                  ["multi_app_bar.app", "multi_app_bar.beam"]),
    lists:foreach(fun(F) -> true = ec_file:exists(filename:join([AppDir, "_build", "test", "lib", "multi_app_baz", "ebin", F])) end,
                  ["multi_app_baz.app", "multi_app_baz.beam"]),
    lists:foreach(fun(F) -> true = ec_file:exists(filename:join([AppDir, "_build", "test", "lib", "multi_app_bar", "test", F])) end,
                  ["multi_app_bar_tests.beam", "multi_app_bar_tests_helper.beam"]),
    lists:foreach(fun(F) -> true = ec_file:exists(filename:join([AppDir, "_build", "test", "lib", "multi_app_baz", "test", F])) end,
                  ["multi_app_baz_tests.beam", "multi_app_baz_tests_helper.beam"]),
    lists:foreach(fun(F) -> true = ec_file:exists(filename:join([AppDir, "_build", "test", "extras", "test", F])) end,
                  ["multi_app_tests.beam", "multi_app_tests_helper.beam"]).

%% check that the correct tests are exported from modules for project
%% note that this implies `TEST` is set correctly
multi_app_exports(_Config) ->
    Tests = fun(Mod) ->
        begin
            Ex = Mod:module_info(exports),
            true = lists:member({sanity_test, 0}, Ex)
        end
    end,
    Helpers = fun(Mod) ->
        begin
            Ex = Mod:module_info(exports),
            true = lists:member({help, 0}, Ex)
        end
    end,
    lists:foreach(Tests, [multi_app_bar, multi_app_bar_tests,
                          multi_app_baz, multi_app_baz_tests,
                          multi_app_tests]),
    lists:foreach(Helpers, [multi_app_bar_tests_helper, multi_app_baz_tests_helper, multi_app_tests_helper]).

%% check that the correct tests are schedule to run for project
multi_app_testset(Config) ->
    Result = ?config(result, Config),

    Set = {ok, [{application, multi_app_baz},
                {application, multi_app_bar},
                {module, multi_app_bar_tests_helper},
                {module, multi_app_baz_tests_helper},
                {module, multi_app_tests},
                {module, multi_app_tests_helper}]},
    Set = rebar_prv_eunit:prepare_tests(Result).

multi_app_eunit_macro(_Config) ->
    Macro = fun(Mod) ->
        begin
            Path = code:which(Mod),
            {ok, {Mod, [{compile_info, CompileInfo}]}} = beam_lib:chunks(Path, [compile_info]),
            Opts = proplists:get_value(options, CompileInfo, []),
            true = lists:member({d, 'EUNIT'}, Opts)
        end
    end,
    lists:foreach(Macro, [multi_app_bar, multi_app_bar_tests,
                          multi_app_baz, multi_app_baz_tests,
                          multi_app_tests, multi_app_tests_helper,
                          multi_app_bar_tests_helper, multi_app_baz_tests_helper]).

%% === tests for command line arguments ===

%% no explicit test for cmd line args taking precedence over the rebar.config since
%% almost every single test implies it

%% check tests in the rebar.config are run if no cmd line opts are specified
eunit_tests(Config) ->
    State = ?config(result, Config),

    Expect = {ok, [{test, multi_app_bar, sanity_test}, {test, multi_app_baz, sanity_test}]},
    Expect = rebar_prv_eunit:prepare_tests(State).

%% check eunit_opts from the rebar.config are respected
eunit_opts(Config) ->
    State = ?config(result, Config),

    Apps = rebar_state:project_apps(State),
    lists:foreach(fun(App) -> [verbose] = rebar_app_info:get(App, eunit_opts) end,
                  Apps).

%% check eunit_first_files from the rebar.config are respected
eunit_first_files(Config) ->
    State = ?config(result, Config),

    FirstFiles = [filename:join(["apps", "multi_app_bar", "test", "multi_app_bar_tests_helper.erl"]),
                  filename:join(["apps", "multi_app_baz", "test", "multi_app_baz_tests_helper.erl"])],

    Apps = rebar_state:project_apps(State),
    lists:foreach(fun(App) -> FirstFiles = rebar_app_info:get(App, eunit_first_files) end,
                  Apps).

%% check that the --application cmd line opt generates the correct test set
single_application_arg(Config) ->
    S = ?config(result, Config),
    {ok, Args} = getopt:parse(rebar_prv_eunit:eunit_opts(S), ["--application=multi_app_bar"]),
    State = rebar_state:command_parsed_args(S, Args),

    {ok, [{application, multi_app_bar}]} = rebar_prv_eunit:prepare_tests(State).

multi_application_arg(Config) ->
    S = ?config(result, Config),
    {ok, Args} = getopt:parse(rebar_prv_eunit:eunit_opts(S), ["--application=multi_app_bar,multi_app_baz"]),
    State = rebar_state:command_parsed_args(S, Args),

    {ok, [{application, multi_app_bar}, {application, multi_app_baz}]} = rebar_prv_eunit:prepare_tests(State).

%% check that an invalid --application cmd line opt generates an error
missing_application_arg(Config) ->
    S = ?config(result, Config),
    {ok, Args} = getopt:parse(rebar_prv_eunit:eunit_opts(S), ["--application=missing_app"]),
    State = rebar_state:command_parsed_args(S, Args),

    Error = {error, {rebar_prv_eunit, {eunit_test_errors, ["Application `missing_app' not found in project."]}}},
    Error = rebar_prv_eunit:validate_tests(State, rebar_prv_eunit:prepare_tests(State)).

%% check that the --module cmd line opt generates the correct test set
single_module_arg(Config) ->
    AppDir = ?config(apps, Config),
    S = ?config(result, Config),

    %% necessary to fix paths
    Path = code:get_path(),
    code:add_paths([filename:join([AppDir, "_build", "test", "lib", "multi_app_bar", "ebin"])]),

    {ok, Args} = getopt:parse(rebar_prv_eunit:eunit_opts(S), ["--module=multi_app_bar"]),
    State = rebar_state:command_parsed_args(S, Args),

    {ok, [{module, multi_app_bar}]} = rebar_prv_eunit:prepare_tests(State),

    %% restore path
    code:set_path(Path).

multi_module_arg(Config) ->
    AppDir = ?config(apps, Config),
    S = ?config(result, Config),

    %% necessary to fix paths
    Path = code:get_path(),
    code:add_paths([filename:join([AppDir, "_build", "test", "lib", "multi_app_bar", "ebin"])]),
    code:add_paths([filename:join([AppDir, "_build", "test", "lib", "multi_app_baz", "ebin"])]),

    {ok, Args} = getopt:parse(rebar_prv_eunit:eunit_opts(S), ["--module=multi_app_bar,multi_app_baz"]),
    State = rebar_state:command_parsed_args(S, Args),

    {ok, [{module, multi_app_bar}, {module, multi_app_baz}]} = rebar_prv_eunit:prepare_tests(State),

    %% restore path
    code:set_path(Path).

%% check that an invalid --module cmd line opt generates an error
missing_module_arg(Config) ->
    S = ?config(result, Config),

    {ok, Args} = getopt:parse(rebar_prv_eunit:eunit_opts(S), ["--module=missing_app"]),
    State = rebar_state:command_parsed_args(S, Args),

    T = rebar_prv_eunit:prepare_tests(State),
    Tests = rebar_prv_eunit:validate_tests(S, T),

    Error = {error, {rebar_prv_eunit, {eunit_test_errors, ["Module `missing_app' not found in project."]}}},
    Error = Tests.

%% check that the --test cmd line opt generates the correct test set
single_test_arg(Config) ->
    S = ?config(result, Config),

    {ok, Args} = getopt:parse(rebar_prv_eunit:eunit_opts(S), ["--test=module_name:function_name"]),
    State = rebar_state:command_parsed_args(S, Args),

    {ok, [{test, module_name, function_name}]} = rebar_prv_eunit:prepare_tests(State).

multi_test_arg(Config) ->
    S = ?config(result, Config),

    {ok, Args} = getopt:parse(rebar_prv_eunit:eunit_opts(S), ["--test=module1:func1+func2,module2:func1;func2"]),
    State = rebar_state:command_parsed_args(S, Args),

    Generators = [{test, module1, func1},
                  {test, module1, func2},
                  {test, module2, func1},
                  {test, module2, func2}],
    {ok, Generators} = rebar_prv_eunit:prepare_tests(State).

%% check that an invalid --test cmd line opt generates an error
missing_test_arg(Config) ->
    S = ?config(result, Config),

    {ok, Args} = getopt:parse(rebar_prv_eunit:eunit_opts(S), ["--test=missing_module:func1"]),
    State = rebar_state:command_parsed_args(S, Args),

    Error = {error, {rebar_prv_eunit, {eunit_test_errors, ["Module `missing_module' not found in project."]}}},
    Error = rebar_prv_eunit:validate_tests(State, rebar_prv_eunit:prepare_tests(State)).

%% check that the --suite cmd line opt generates the correct test set
single_suite_arg(Config) ->
    AppDir = ?config(apps, Config),
    S = ?config(result, Config),

    %% necessary to fix paths
    Path = code:get_path(),
    code:add_paths([filename:join([AppDir, "_build", "test", "lib", "multi_app_bar", "ebin"])]),

    {ok, Args} = getopt:parse(rebar_prv_eunit:eunit_opts(S), ["--suite=multi_app_bar"]),
    State = rebar_state:command_parsed_args(S, Args),

    {ok, [{module, multi_app_bar}]} = rebar_prv_eunit:prepare_tests(State),

    %% restore path
    code:set_path(Path).

multi_suite_arg(Config) ->
    AppDir = ?config(apps, Config),
    S = ?config(result, Config),

    %% necessary to fix paths
    Path = code:get_path(),
    code:add_paths([filename:join([AppDir, "_build", "test", "lib", "multi_app_bar", "ebin"])]),
    code:add_paths([filename:join([AppDir, "_build", "test", "lib", "multi_app_baz", "ebin"])]),

    {ok, Args} = getopt:parse(rebar_prv_eunit:eunit_opts(S), ["--suite=multi_app_bar,multi_app_baz"]),
    State = rebar_state:command_parsed_args(S, Args),

    {ok, [{module, multi_app_bar}, {module, multi_app_baz}]} = rebar_prv_eunit:prepare_tests(State),

    %% restore path
    code:set_path(Path).

%% check that an invalid --suite cmd line opt generates an error
missing_suite_arg(Config) ->
    S = ?config(result, Config),

    {ok, Args} = getopt:parse(rebar_prv_eunit:eunit_opts(S), ["--suite=missing_app"]),
    State = rebar_state:command_parsed_args(S, Args),

    Error = {error, {rebar_prv_eunit, {eunit_test_errors, ["Module `missing_app' not found in project."]}}},
    Error = rebar_prv_eunit:validate_tests(State, rebar_prv_eunit:prepare_tests(State)).

%% check that the --generator cmd line opt generates the correct test set
single_generator_arg(Config) ->
    S = ?config(result, Config),

    {ok, Args} = getopt:parse(rebar_prv_eunit:eunit_opts(S), ["--generator=module_name:function_name"]),
    State = rebar_state:command_parsed_args(S, Args),

    {ok, [{generator, module_name, function_name}]} = rebar_prv_eunit:prepare_tests(State).

multi_generator_arg(Config) ->
    S = ?config(result, Config),

    {ok, Args} = getopt:parse(rebar_prv_eunit:eunit_opts(S), ["--generator=module1:func1+func2,module2:func1;func2"]),
    State = rebar_state:command_parsed_args(S, Args),

    Generators = [{generator, module1, func1},
                  {generator, module1, func2},
                  {generator, module2, func1},
                  {generator, module2, func2}],
    {ok, Generators} = rebar_prv_eunit:prepare_tests(State).

%% check that an invalid --suite cmd line opt generates an error
missing_generator_arg(Config) ->
    S = ?config(result, Config),

    {ok, Args} = getopt:parse(rebar_prv_eunit:eunit_opts(S), ["--generator=missing_module:func1"]),
    State = rebar_state:command_parsed_args(S, Args),

    Error = {error, {rebar_prv_eunit, {eunit_test_errors, ["Module `missing_module' not found in project."]}}},
    Error = rebar_prv_eunit:validate_tests(State, rebar_prv_eunit:prepare_tests(State)).

%% check that the --file cmd line opt generates the correct test set
single_file_arg(Config) ->
    S = ?config(result, Config),
    AppDir = ?config(apps, Config),

    Path = filename:join([AppDir, "_build", "test", "lib", "multi_app_bar", "ebin", "multi_app_bar.beam"]),
    {ok, Args} = getopt:parse(rebar_prv_eunit:eunit_opts(S), ["--file=" ++ Path]),
    State = rebar_state:command_parsed_args(S, Args),

    {ok, [{file, Path}]} = rebar_prv_eunit:prepare_tests(State).

multi_file_arg(Config) ->
    S = ?config(result, Config),
    AppDir = ?config(apps, Config),

    BarPath = filename:join([AppDir, "_build", "test", "lib", "multi_app_bar", "ebin", "multi_app_bar.beam"]),
    BazPath = filename:join([AppDir, "_build", "test", "lib", "multi_app_baz", "ebin", "multi_app_baz.beam"]),
    {ok, Args} = getopt:parse(rebar_prv_eunit:eunit_opts(S), ["--file=" ++ BarPath ++ "," ++ BazPath]),
    State = rebar_state:command_parsed_args(S, Args),

    {ok, [{file, BarPath}, {file, BazPath}]} = rebar_prv_eunit:prepare_tests(State).

%% check that an invalid --file cmd line opt generates an error
missing_file_arg(Config) ->
    S = ?config(result, Config),
    AppDir = ?config(apps, Config),

    Path = filename:join([AppDir, "_build", "test", "lib", "missing_app", "ebin", "missing_app.beam"]),
    {ok, Args} = getopt:parse(rebar_prv_eunit:eunit_opts(S), ["--file=" ++ Path]),
    State = rebar_state:command_parsed_args(S, Args),

    Error = {error, {rebar_prv_eunit, {eunit_test_errors, ["File `" ++ Path ++"' not found."]}}},
    Error = rebar_prv_eunit:validate_tests(State, rebar_prv_eunit:prepare_tests(State)).

%% check that the --dir cmd line opt generates the correct test set
single_dir_arg(Config) ->
    S = ?config(result, Config),
    AppDir = ?config(apps, Config),

    Path = filename:join([AppDir, "_build", "test", "lib", "multi_app_bar", "ebin"]),
    {ok, Args} = getopt:parse(rebar_prv_eunit:eunit_opts(S), ["--dir=" ++ Path]),
    State = rebar_state:command_parsed_args(S, Args),

    {ok, [{dir, Path}]} = rebar_prv_eunit:prepare_tests(State).

multi_dir_arg(Config) ->
    S = ?config(result, Config),
    AppDir = ?config(apps, Config),

    BarPath = filename:join([AppDir, "_build", "test", "lib", "multi_app_bar", "ebin"]),
    BazPath = filename:join([AppDir, "_build", "test", "lib", "multi_app_baz", "ebin"]),
    {ok, Args} = getopt:parse(rebar_prv_eunit:eunit_opts(S), ["--dir=" ++ BarPath ++ "," ++ BazPath]),
    State = rebar_state:command_parsed_args(S, Args),

    {ok, [{dir, BarPath}, {dir, BazPath}]} = rebar_prv_eunit:prepare_tests(State).

%% check that an invalid --dir cmd line opt generates an error
missing_dir_arg(Config) ->
    S = ?config(result, Config),
    AppDir = ?config(apps, Config),

    Path = filename:join([AppDir, "_build", "test", "lib", "missing_app", "ebin"]),
    {ok, Args} = getopt:parse(rebar_prv_eunit:eunit_opts(S), ["--dir=" ++ Path]),
    State = rebar_state:command_parsed_args(S, Args),

    Error = {error, {rebar_prv_eunit, {eunit_test_errors, ["Directory `" ++ Path ++"' not found."]}}},
    Error = rebar_prv_eunit:validate_tests(State, rebar_prv_eunit:prepare_tests(State)).

%% check that multiple args are composed
multiple_arg_composition(Config) ->
    S = ?config(result, Config),
    AppDir = ?config(apps, Config),

    %% necessary to fix paths
    Path = code:get_path(),
    code:add_paths([filename:join([AppDir, "_build", "test", "lib", "multi_app_bar", "ebin"])]),
    FilePath = filename:join([AppDir, "_build", "test", "lib", "multi_app_bar", "ebin", "multi_app_bar.beam"]),
    DirPath = filename:join([AppDir, "_build", "test", "lib", "multi_app_bar", "ebin"]),

    {ok, Args} = getopt:parse(rebar_prv_eunit:eunit_opts(S), ["--application=multi_app_bar",
                                                              "--module=multi_app_bar",
                                                              "--suite=multi_app_bar",
                                                              "--file=" ++ FilePath,
                                                              "--dir=" ++ DirPath]),
    State = rebar_state:command_parsed_args(S, Args),

    Expect = [{application, multi_app_bar},
              {dir, DirPath},
              {file, FilePath},
              {module, multi_app_bar},
              {module, multi_app_bar}],

    {ok, Expect} = rebar_prv_eunit:prepare_tests(State),

    %% restore path
    code:set_path(Path).

%% check that multiple errors are reported
multiple_arg_errors(Config) ->
    S = ?config(result, Config),
    AppDir = ?config(apps, Config),

    FilePath = filename:join([AppDir, "_build", "test", "lib", "missing_app", "ebin", "missing_app.beam"]),
    DirPath = filename:join([AppDir, "_build", "test", "lib", "missing_app", "ebin"]),

    {ok, Args} = getopt:parse(rebar_prv_eunit:eunit_opts(S), ["--application=missing_app",
                                                              "--module=missing_app",
                                                              "--suite=missing_app",
                                                              "--file=" ++ FilePath,
                                                              "--dir=" ++ DirPath]),
    State = rebar_state:command_parsed_args(S, Args),

    T = rebar_prv_eunit:prepare_tests(State),
    Tests = rebar_prv_eunit:validate_tests(S, T),

    Expect = ["Application `missing_app' not found in project.",
              "Directory `" ++ DirPath ++ "' not found.",
              "File `" ++ FilePath ++ "' not found.",
              "Module `missing_app' not found in project.",
              "Module `missing_app' not found in project."],

    {error, {rebar_prv_eunit, {eunit_test_errors, Expect}}} = Tests.

misspecified_eunit_tests(Config) ->
    State = rebar_test_utils:init_rebar_state(Config, "basic_app_"),

    AppDir = ?config(apps, State),
    PrivDir = ?config(priv_dir, State),

    AppDirs = ["src", "include", "test"],

    lists:foreach(fun(F) -> ec_file:copy(filename:join([PrivDir, "basic_app", F]),
                                         filename:join([AppDir, F]),
                                         [recursive]) end, AppDirs),

    BaseConfig = [{erl_opts, [{d, config_define}]}, {eunit_compile_opts, [{d, eunit_compile_define}]}],

    RebarConfig = [{eunit_tests, {dir, "test"}}|BaseConfig],

    {error, {rebar_prv_eunit, Error}} = rebar_test_utils:run_and_check(State, RebarConfig, ["eunit"], return),

    {badconfig, {"Value `~p' of option `~p' must be a list", {{dir, "test"}, eunit_tests}}} = Error.

misspecified_eunit_compile_opts(Config) ->
    State = rebar_test_utils:init_rebar_state(Config, "basic_app_"),

    AppDir = ?config(apps, State),
    PrivDir = ?config(priv_dir, State),

    AppDirs = ["src", "include", "test"],

    lists:foreach(fun(F) -> ec_file:copy(filename:join([PrivDir, "basic_app", F]),
                                         filename:join([AppDir, F]),
                                         [recursive]) end, AppDirs),

    RebarConfig = [{erl_opts, [{d, config_define}]}, {eunit_compile_opts, {d, eunit_compile_define}}],

    {error, {rebar_prv_eunit, Error}} = rebar_test_utils:run_and_check(State, RebarConfig, ["eunit"], return),

    {badconfig, {"Value `~p' of option `~p' must be a list", {{d, eunit_compile_define}, eunit_compile_opts}}} = Error.

misspecified_eunit_first_files(Config) ->
    State = rebar_test_utils:init_rebar_state(Config, "basic_app_"),

    AppDir = ?config(apps, State),
    PrivDir = ?config(priv_dir, State),

    AppDirs = ["src", "include", "test"],

    lists:foreach(fun(F) -> ec_file:copy(filename:join([PrivDir, "basic_app", F]),
                                         filename:join([AppDir, F]),
                                         [recursive]) end, AppDirs),

    BaseConfig = [{erl_opts, [{d, config_define}]}, {eunit_compile_opts, [{d, eunit_compile_define}]}],

    RebarConfig = [{eunit_first_files, some_file}|BaseConfig],

    {error, {rebar_prv_eunit, Error}} = rebar_test_utils:run_and_check(State, RebarConfig, ["eunit"], return),

    {badconfig, {"Value `~p' of option `~p' must be a list", {some_file, eunit_first_files}}} = Error.

alternate_test_regex(Config) ->
    State = rebar_test_utils:init_rebar_state(Config, "alternate_test_regex_"),

    AppDir = ?config(apps, State),
    PrivDir = ?config(priv_dir, State),

    AppDirs = ["src", "include", "test"],

    lists:foreach(fun(F) -> ec_file:copy(filename:join([PrivDir, "basic_app", F]),
                                         filename:join([AppDir, F]),
                                         [recursive]) end, AppDirs),

    BaseConfig = [{erl_opts, [{d, config_define}]}, {eunit_compile_opts, [{d, eunit_compile_define}]}],

    RebarConfig = [{eunit_test_regex, "basic_app_tests.erl"}|BaseConfig],

    {ok, S} = rebar_test_utils:run_and_check(State, RebarConfig, ["as", "test", "lock"], return),

    Set = {ok, [{application, basic_app},
                {module, basic_app_tests}]},
    Set = rebar_prv_eunit:prepare_tests(S).

%% check that sys_config files go through
syscfg_app_opts(Config) ->
    State = rebar_test_utils:init_rebar_state(Config, "syscfg_"),

    AppDir = ?config(apps, State),
    PrivDir = ?config(priv_dir, State),

    AppDirs = ["src", "test", "config"],

    lists:foreach(fun(F) -> ec_file:copy(filename:join([PrivDir, "syscfg_app", F]),
                                         filename:join([AppDir, F]),
                                         [recursive]) end, AppDirs),

    RebarConfig = [{eunit_opts, [
        {sys_config, ["config/file1.config", "config/file2.config"]}
    ]}],
    Opts = ["--sys_config", "config/cmd1.config,config/cmd2.config"],

    {ok, _} = rebar_test_utils:run_and_check(State, RebarConfig,
                                             ["eunit" | Opts], return),
    ok.

