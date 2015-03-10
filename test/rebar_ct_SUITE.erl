-module(rebar_ct_SUITE).

-export([all/0,
         groups/0,
         init_per_group/2,
         end_per_group/2]).
-export([basic_app_default_dirs/1,
         basic_app_default_beams/1,
         multi_app_default_dirs/1,
         multi_app_default_beams/1,
         single_app_dir/1,
         single_extra_dir/1,
         single_unmanaged_dir/1,
         single_suite/1,
         single_extra_suite/1,
         single_unmanaged_suite/1,
         multi_suite/1,
         all_suite/1,
         single_dir_and_single_suite/1]).

-include_lib("common_test/include/ct.hrl").

all() -> [{group, basic_app},
          {group, multi_app},
          {group, dirs_and_suites}].

groups() -> [{basic_app, [], [basic_app_default_dirs,
                              basic_app_default_beams]},
             {multi_app, [], [multi_app_default_dirs,
                              multi_app_default_beams]},
             {dirs_and_suites, [], [single_app_dir,
                                    single_extra_dir,
                                    single_unmanaged_dir,
                                    single_suite,
                                    single_extra_suite,
                                    single_unmanaged_suite,
                                    multi_suite,
                                    all_suite,
                                    single_dir_and_single_suite]}].

init_per_group(basic_app, Config) ->
    C = rebar_test_utils:init_rebar_state(Config, "ct_"),

    AppDir = ?config(apps, C),

    Name = rebar_test_utils:create_random_name(atom_to_list(basic_app) ++ "_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    Suite = filename:join([AppDir, "test", Name ++ "_SUITE.erl"]),
    ok = filelib:ensure_dir(Suite),
    ok = file:write_file(Suite, test_suite(Name)),

    {ok, State} = rebar_test_utils:run_and_check(C, [], ["as", "test", "compile"], return),

    LibDirs = rebar_dir:lib_dirs(State),
    State1 = rebar_app_discover:do(State, LibDirs),

    Providers = rebar_state:providers(State1),
    Namespace = rebar_state:namespace(State1),
    CommandProvider = providers:get_provider(ct, Providers, Namespace),
    GetOptSpec = providers:opts(CommandProvider),
    {ok, GetOptResult} = getopt:parse(GetOptSpec, []),

    State2 = rebar_state:command_parsed_args(State1, GetOptResult),

    Result = rebar_prv_common_test:setup_ct(State2),

    [{result, Result}, {appnames, [Name]}|C];
init_per_group(multi_app, Config) ->
    C = rebar_test_utils:init_rebar_state(Config, "ct_"),

    AppDir = ?config(apps, C),

    Name1 = rebar_test_utils:create_random_name(atom_to_list(multi_app) ++ "_"),
    Vsn1 = rebar_test_utils:create_random_vsn(),
    AppDir1 = filename:join([AppDir, "apps", Name1]),
    rebar_test_utils:create_app(AppDir1, Name1, Vsn1, [kernel, stdlib]),

    Suite1 = filename:join([AppDir1, "test", Name1 ++ "_SUITE.erl"]),
    ok = filelib:ensure_dir(Suite1),
    ok = file:write_file(Suite1, test_suite(Name1)),

    Name2 = rebar_test_utils:create_random_name(atom_to_list(multi_app) ++ "_"),
    Vsn2 = rebar_test_utils:create_random_vsn(),
    AppDir2 = filename:join([AppDir, "apps", Name2]),
    rebar_test_utils:create_app(AppDir2, Name2, Vsn2, [kernel, stdlib]),

    Suite2 = filename:join([AppDir2, "test", Name2 ++ "_SUITE.erl"]),
    ok = filelib:ensure_dir(Suite2),
    ok = file:write_file(Suite2, test_suite(Name2)),

    Suite3 = filename:join([AppDir, "test", "extras_SUITE.erl"]),
    ok = filelib:ensure_dir(Suite3),
    ok = file:write_file(Suite3, test_suite("extras")),

    {ok, State} = rebar_test_utils:run_and_check(C, [], ["as", "test", "compile"], return),

    LibDirs = rebar_dir:lib_dirs(State),
    State1 = rebar_app_discover:do(State, LibDirs),

    Providers = rebar_state:providers(State1),
    Namespace = rebar_state:namespace(State1),
    CommandProvider = providers:get_provider(ct, Providers, Namespace),
    GetOptSpec = providers:opts(CommandProvider),
    {ok, GetOptResult} = getopt:parse(GetOptSpec, []),

    State2 = rebar_state:command_parsed_args(State1, GetOptResult),

    Result = rebar_prv_common_test:setup_ct(State2),

    [{result, Result}, {appnames, [Name1, Name2]}|C];
init_per_group(dirs_and_suites, Config) ->
    C = rebar_test_utils:init_rebar_state(Config, "ct_"),

    AppDir = ?config(apps, C),

    Name1 = rebar_test_utils:create_random_name(atom_to_list(dirs_and_suites) ++ "_"),
    Vsn1 = rebar_test_utils:create_random_vsn(),
    AppDir1 = filename:join([AppDir, "apps", Name1]),
    rebar_test_utils:create_app(AppDir1, Name1, Vsn1, [kernel, stdlib]),

    Suite1 = filename:join([AppDir1, "test", Name1 ++ "_SUITE.erl"]),
    ok = filelib:ensure_dir(Suite1),
    ok = file:write_file(Suite1, test_suite(Name1)),

    Name2 = rebar_test_utils:create_random_name(atom_to_list(dir_and_suites) ++ "_"),
    Vsn2 = rebar_test_utils:create_random_vsn(),
    AppDir2 = filename:join([AppDir, "apps", Name2]),
    rebar_test_utils:create_app(AppDir2, Name2, Vsn2, [kernel, stdlib]),

    Suite2 = filename:join([AppDir2, "test", Name2 ++ "_SUITE.erl"]),
    ok = filelib:ensure_dir(Suite2),
    ok = file:write_file(Suite2, test_suite(Name2)),

    Suite3 = filename:join([AppDir, "test", "extras_SUITE.erl"]),
    ok = filelib:ensure_dir(Suite3),
    ok = file:write_file(Suite3, test_suite("extras")),

    [{appnames, [Name1, Name2]}|C].

end_per_group(_Group, _Config) -> ok.

basic_app_default_dirs(Config) ->
    AppDir = ?config(apps, Config),
    [Name] = ?config(appnames, Config),
    Result = ?config(result, Config),

    Expect = filename:absname(filename:join([AppDir, "_build", "test", "lib", Name, "test"])),
    Dir = proplists:get_value(dir, Result),

    Expect = Dir.

basic_app_default_beams(Config) ->
    AppDir = ?config(apps, Config),
    [Name] = ?config(appnames, Config),

    File = filename:join([AppDir,
                          "_build",
                          "test",
                          "lib",
                          Name,
                          "test",
                          Name ++ "_SUITE.beam"]),

    true = filelib:is_file(File).

multi_app_default_dirs(Config) ->
    AppDir = ?config(apps, Config),
    [Name1, Name2] = ?config(appnames, Config),
    Result = ?config(result, Config),

    Expect1 = filename:absname(filename:join([AppDir, "_build", "test", "lib", Name1, "test"])),
    Expect2 = filename:absname(filename:join([AppDir, "_build", "test", "lib", Name2, "test"])),
    Expect3 = filename:absname(filename:join([AppDir, "_build", "test", "test"])),
    Dirs = proplists:get_value(dir, Result),

    true = (lists:sort([Expect1, Expect2, Expect3]) == lists:sort(Dirs)).

multi_app_default_beams(Config) ->
    AppDir = ?config(apps, Config),
    [Name1, Name2] = ?config(appnames, Config),

    File1 = filename:join([AppDir,
                           "_build",
                           "test",
                           "lib",
                           Name1,
                           "test",
                           Name1 ++ "_SUITE.beam"]),
    File2 = filename:join([AppDir,
                           "_build",
                           "test",
                           "lib",
                           Name2,
                           "test",
                           Name2 ++ "_SUITE.beam"]),
    File3 = filename:join([AppDir,
                           "_build",
                           "test",
                           "test",
                           "extras_SUITE.beam"]),

    true = filelib:is_file(File1),
    true = filelib:is_file(File2),
    true = filelib:is_file(File3).

single_app_dir(Config) ->
    AppDir = ?config(apps, Config),
    [Name1, _Name2] = ?config(appnames, Config),

    {ok, State} = rebar_test_utils:run_and_check(Config, [], ["as", "test", "compile"], return),

    LibDirs = rebar_dir:lib_dirs(State),
    State1 = rebar_app_discover:do(State, LibDirs),

    Providers = rebar_state:providers(State1),
    Namespace = rebar_state:namespace(State1),
    CommandProvider = providers:get_provider(ct, Providers, Namespace),
    GetOptSpec = providers:opts(CommandProvider),
    {ok, GetOptResult} = getopt:parse(GetOptSpec,
                                      ["--dir=" ++ filename:join([AppDir,
                                                                  "apps",
                                                                  Name1,
                                                                  "test"])]),

    State2 = rebar_state:command_parsed_args(State1, GetOptResult),

    Result = rebar_prv_common_test:setup_ct(State2),

    Expect = filename:absname(filename:join([AppDir, "_build", "test", "lib", Name1, "test"])),
    Dir = proplists:get_value(dir, Result),

    Expect = Dir.

single_extra_dir(Config) ->
    AppDir = ?config(apps, Config),

    {ok, State} = rebar_test_utils:run_and_check(Config, [], ["as", "test", "compile"], return),

    LibDirs = rebar_dir:lib_dirs(State),
    State1 = rebar_app_discover:do(State, LibDirs),

    Providers = rebar_state:providers(State1),
    Namespace = rebar_state:namespace(State1),
    CommandProvider = providers:get_provider(ct, Providers, Namespace),
    GetOptSpec = providers:opts(CommandProvider),
    {ok, GetOptResult} = getopt:parse(GetOptSpec, ["--dir=" ++ filename:join([AppDir,
                                                                              "test"])]),

    State2 = rebar_state:command_parsed_args(State1, GetOptResult),

    Result = rebar_prv_common_test:setup_ct(State2),

    Expect = filename:absname(filename:join([AppDir, "_build", "test", "test"])),
    Dir = proplists:get_value(dir, Result),

    Expect = Dir.

single_unmanaged_dir(Config) ->
    PrivDir = ?config(priv_dir, Config),

    Suite = filename:join([PrivDir, "unmanaged_dir", "unmanaged_dir_SUITE.erl"]),
    ok = filelib:ensure_dir(Suite),
    ok = file:write_file(Suite, test_suite("unmanaged_dir")),

    {ok, State} = rebar_test_utils:run_and_check(Config, [], ["as", "test", "compile"], return),

    LibDirs = rebar_dir:lib_dirs(State),
    State1 = rebar_app_discover:do(State, LibDirs),

    Providers = rebar_state:providers(State1),
    Namespace = rebar_state:namespace(State1),
    CommandProvider = providers:get_provider(ct, Providers, Namespace),
    GetOptSpec = providers:opts(CommandProvider),
    {ok, GetOptResult} = getopt:parse(GetOptSpec, ["--dir=" ++ filename:absname(filename:join([PrivDir,
                                                                                               "unmanaged_dir"]))]),

    State2 = rebar_state:command_parsed_args(State1, GetOptResult),

    Result = rebar_prv_common_test:setup_ct(State2),

    Expect = filename:absname(filename:join([PrivDir, "unmanaged_dir"])),
    Dir = proplists:get_value(dir, Result),

    Expect = Dir.

single_suite(Config) ->
    AppDir = ?config(apps, Config),
    [Name1, _Name2] = ?config(appnames, Config),

    {ok, State} = rebar_test_utils:run_and_check(Config, [], ["as", "test", "compile"], return),

    LibDirs = rebar_dir:lib_dirs(State),
    State1 = rebar_app_discover:do(State, LibDirs),

    Providers = rebar_state:providers(State1),
    Namespace = rebar_state:namespace(State1),
    CommandProvider = providers:get_provider(ct, Providers, Namespace),
    GetOptSpec = providers:opts(CommandProvider),
    {ok, GetOptResult} = getopt:parse(GetOptSpec,
                                      ["--suite=" ++ filename:join([AppDir,
                                                                    "apps",
                                                                    Name1,
                                                                    "test",
                                                                    Name1 ++ "_SUITE"])]),

    State2 = rebar_state:command_parsed_args(State1, GetOptResult),

    Result = rebar_prv_common_test:setup_ct(State2),

    Expect = [filename:absname(filename:join([AppDir,
                                              "_build",
                                              "test",
                                              "lib",
                                              Name1,
                                              "test",
                                              Name1 ++ "_SUITE"]))],
    Suite = proplists:get_value(suite, Result),

    Expect = Suite.

single_extra_suite(Config) ->
    AppDir = ?config(apps, Config),
    [_Name1, _Name2] = ?config(appnames, Config),

    {ok, State} = rebar_test_utils:run_and_check(Config, [], ["as", "test", "compile"], return),

    LibDirs = rebar_dir:lib_dirs(State),
    State1 = rebar_app_discover:do(State, LibDirs),

    Providers = rebar_state:providers(State1),
    Namespace = rebar_state:namespace(State1),
    CommandProvider = providers:get_provider(ct, Providers, Namespace),
    GetOptSpec = providers:opts(CommandProvider),
    {ok, GetOptResult} = getopt:parse(GetOptSpec,
                                      ["--suite=" ++ filename:join([AppDir,
                                                                    "test",
                                                                    "extra_SUITE"])]),

    State2 = rebar_state:command_parsed_args(State1, GetOptResult),

    Result = rebar_prv_common_test:setup_ct(State2),

    Expect = [filename:absname(filename:join([AppDir,
                                              "_build",
                                              "test",
                                              "test",
                                              "extra_SUITE"]))],
    Suite = proplists:get_value(suite, Result),

    Expect = Suite.

single_unmanaged_suite(Config) ->
    PrivDir = ?config(priv_dir, Config),
    [_Name1, _Name2] = ?config(appnames, Config),

    Suite = filename:join([PrivDir, "unmanaged", "unmanaged_SUITE.erl"]),
    ok = filelib:ensure_dir(Suite),
    ok = file:write_file(Suite, test_suite("unmanaged")),

    {ok, State} = rebar_test_utils:run_and_check(Config, [], ["as", "test", "compile"], return),

    LibDirs = rebar_dir:lib_dirs(State),
    State1 = rebar_app_discover:do(State, LibDirs),

    Providers = rebar_state:providers(State1),
    Namespace = rebar_state:namespace(State1),
    CommandProvider = providers:get_provider(ct, Providers, Namespace),
    GetOptSpec = providers:opts(CommandProvider),
    {ok, GetOptResult} = getopt:parse(GetOptSpec,
                                      ["--suite=" ++ filename:absname(filename:join([PrivDir,
                                                                                     "unmanaged",
                                                                                     "unmanaged_SUITE"]))]),

    State2 = rebar_state:command_parsed_args(State1, GetOptResult),

    Result = rebar_prv_common_test:setup_ct(State2),

    Expect = [filename:absname(filename:join([PrivDir,
                                              "unmanaged",
                                              "unmanaged_SUITE"]))],
    SuitePath = proplists:get_value(suite, Result),

    Expect = SuitePath.

multi_suite(Config) ->
    AppDir = ?config(apps, Config),
    [Name1, Name2] = ?config(appnames, Config),

    {ok, State} = rebar_test_utils:run_and_check(Config, [], ["as", "test", "compile"], return),

    LibDirs = rebar_dir:lib_dirs(State),
    State1 = rebar_app_discover:do(State, LibDirs),

    Providers = rebar_state:providers(State1),
    Namespace = rebar_state:namespace(State1),
    CommandProvider = providers:get_provider(ct, Providers, Namespace),
    GetOptSpec = providers:opts(CommandProvider),
    {ok, GetOptResult} = getopt:parse(GetOptSpec,
                                      ["--suite=" ++ filename:join([AppDir,
                                                                    "apps",
                                                                    Name1,
                                                                    "test",
                                                                    Name1 ++ "_SUITE," ++ AppDir,
                                                                    "apps",
                                                                    Name2,
                                                                    "test",
                                                                    Name2 ++ "_SUITE"])]),

    State2 = rebar_state:command_parsed_args(State1, GetOptResult),

    Result = rebar_prv_common_test:setup_ct(State2),

    Expect1 = filename:absname(filename:join([AppDir,
                                              "_build",
                                              "test",
                                              "lib",
                                              Name1,
                                              "test",
                                              Name1 ++ "_SUITE"])),
    Expect2 = filename:absname(filename:join([AppDir,
                                              "_build",
                                              "test",
                                              "lib",
                                              Name2,
                                              "test",
                                              Name2 ++ "_SUITE"])),
    Suites = proplists:get_value(suite, Result),

    true = (lists:sort([Expect1, Expect2]) == lists:sort(Suites)).

all_suite(Config) ->
    AppDir = ?config(apps, Config),
    [Name1, Name2] = ?config(appnames, Config),

    {ok, State} = rebar_test_utils:run_and_check(Config, [], ["as", "test", "compile"], return),

    LibDirs = rebar_dir:lib_dirs(State),
    State1 = rebar_app_discover:do(State, LibDirs),

    Providers = rebar_state:providers(State1),
    Namespace = rebar_state:namespace(State1),
    CommandProvider = providers:get_provider(ct, Providers, Namespace),
    GetOptSpec = providers:opts(CommandProvider),
    {ok, GetOptResult} = getopt:parse(GetOptSpec,
                                      ["--suite=" ++ filename:join([AppDir,
                                                                    "apps",
                                                                    Name1,
                                                                    "test",
                                                                    Name1 ++ "_SUITE," ++ AppDir,
                                                                    "apps",
                                                                    Name2,
                                                                    "test",
                                                                    Name2 ++ "_SUITE," ++ AppDir,
                                                                    "test",
                                                                    "extra_SUITE"])]),

    State2 = rebar_state:command_parsed_args(State1, GetOptResult),

    Result = rebar_prv_common_test:setup_ct(State2),

    Expect1 = filename:absname(filename:join([AppDir,
                                              "_build",
                                              "test",
                                              "lib",
                                              Name1,
                                              "test",
                                              Name1 ++ "_SUITE"])),
    Expect2 = filename:absname(filename:join([AppDir,
                                              "_build",
                                              "test",
                                              "lib",
                                              Name2,
                                              "test",
                                              Name2 ++ "_SUITE"])),
    Expect3 = filename:absname(filename:join([AppDir,
                                              "_build",
                                              "test",
                                              "test",
                                              "extra_SUITE"])),
    Suites = proplists:get_value(suite, Result),

    true = (lists:sort([Expect1, Expect2, Expect3]) == lists:sort(Suites)).

single_dir_and_single_suite(Config) ->
    AppDir = ?config(apps, Config),
    [_Name1, _Name2] = ?config(appnames, Config),

    {ok, State} = rebar_test_utils:run_and_check(Config, [], ["as", "test", "compile"], return),

    LibDirs = rebar_dir:lib_dirs(State),
    State1 = rebar_app_discover:do(State, LibDirs),

    Providers = rebar_state:providers(State1),
    Namespace = rebar_state:namespace(State1),
    CommandProvider = providers:get_provider(ct, Providers, Namespace),
    GetOptSpec = providers:opts(CommandProvider),
    {ok, GetOptResult} = getopt:parse(GetOptSpec,
                                      ["--dir=" ++ filename:join([AppDir, "test"]),
                                       "--suite=extra_SUITE"]),

    State2 = rebar_state:command_parsed_args(State1, GetOptResult),

    Result = rebar_prv_common_test:setup_ct(State2),

    Expect = [filename:absname(filename:join([AppDir,
                                              "_build",
                                              "test",
                                              "test",
                                              "extra_SUITE"]))],
    Suite = proplists:get_value(suite, Result),

    Expect = Suite.

test_suite(Name) ->
    io_lib:format("-module(~ts_SUITE).\n"
                  "-compile(export_all).\n"
                  "all() -> [some_test].\n"
                  "some_test(_) -> ok.\n", [Name]).