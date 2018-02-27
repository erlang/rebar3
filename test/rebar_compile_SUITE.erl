-module(rebar_compile_SUITE).

-export([suite/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2,
         init_per_group/2,
         end_per_group/2,
         all/0,
         groups/0,
         build_basic_app/1, paths_basic_app/1, clean_basic_app/1,
         build_release_apps/1, paths_release_apps/1, clean_release_apps/1,
         build_checkout_apps/1, paths_checkout_apps/1,
         build_checkout_deps/1, paths_checkout_deps/1,
         build_basic_srcdirs/1, paths_basic_srcdirs/1,
         build_release_srcdirs/1, paths_release_srcdirs/1,
         build_unbalanced_srcdirs/1, paths_unbalanced_srcdirs/1,
         build_basic_extra_dirs/1, paths_basic_extra_dirs/1, clean_basic_extra_dirs/1,
         build_release_extra_dirs/1, paths_release_extra_dirs/1, clean_release_extra_dirs/1,
         build_unbalanced_extra_dirs/1, paths_unbalanced_extra_dirs/1,
         build_extra_dirs_in_project_root/1,
         paths_extra_dirs_in_project_root/1,
         clean_extra_dirs_in_project_root/1,
         recompile_when_hrl_changes/1,
         recompile_when_included_hrl_changes/1,
         recompile_when_opts_included_hrl_changes/1,
         recompile_when_opts_change/1,
         dont_recompile_when_opts_dont_change/1,
         dont_recompile_yrl_or_xrl/1,
         deps_in_path/1,
         delete_beam_if_source_deleted/1,
         checkout_priority/1,
         highest_version_of_pkg_dep/1,
         parse_transform_test/1,
         erl_first_files_test/1,
         mib_test/1,
         umbrella_mib_first_test/1,
         only_default_transitive_deps/1,
         clean_all/1,
         override_deps/1,
         profile_override_deps/1,
         profile_deps/1,
         deps_build_in_prod/1,
         include_file_relative_to_working_directory/1,
         include_file_in_src/1,
         include_file_relative_to_working_directory_test/1,
         include_file_in_src_test/1,
         include_file_in_src_test_multiapp/1,
         dont_recompile_when_erl_compiler_options_env_does_not_change/1,
         recompile_when_erl_compiler_options_env_changes/1,
         always_recompile_when_erl_compiler_options_set/1,
         recompile_when_parse_transform_inline_changes/1,
         recompile_when_parse_transform_as_opt_changes/1,
         recursive/1,no_recursive/1,
         regex_filter_skip/1, regex_filter_regression/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").

suite() ->
    [].

all() ->
    [{group, basic_app}, {group, release_apps},
     {group, checkout_apps}, {group, checkout_deps},
     {group, basic_srcdirs}, {group, release_srcdirs}, {group, unbalanced_srcdirs},
     {group, basic_extras}, {group, release_extras}, {group, unbalanced_extras},
     {group, root_extras},
     recompile_when_hrl_changes, recompile_when_included_hrl_changes,
     recompile_when_opts_included_hrl_changes,
     recompile_when_opts_change,
     dont_recompile_when_opts_dont_change, dont_recompile_yrl_or_xrl,
     delete_beam_if_source_deleted,
     deps_in_path, checkout_priority, highest_version_of_pkg_dep,
     parse_transform_test, erl_first_files_test, mib_test,
     umbrella_mib_first_test, only_default_transitive_deps,
     clean_all, override_deps, profile_override_deps, deps_build_in_prod,
     profile_override_deps, profile_deps, deps_build_in_prod,
     include_file_relative_to_working_directory, include_file_in_src,
     include_file_relative_to_working_directory_test, include_file_in_src_test,
     include_file_in_src_test_multiapp,
     recompile_when_parse_transform_as_opt_changes,
     recompile_when_parse_transform_inline_changes,
     regex_filter_skip, regex_filter_regression,
     recursive, no_recursive,
     always_recompile_when_erl_compiler_options_set,
     dont_recompile_when_erl_compiler_options_env_does_not_change,
     recompile_when_erl_compiler_options_env_changes].

groups() ->
    [{basic_app, [], [build_basic_app, paths_basic_app, clean_basic_app]},
     {release_apps, [], [build_release_apps, paths_release_apps, clean_release_apps]},
     {checkout_apps, [], [build_checkout_apps, paths_checkout_apps]},
     {checkout_deps, [], [build_checkout_deps, paths_checkout_deps]},
     {basic_srcdirs, [], [build_basic_srcdirs, paths_basic_srcdirs]},
     {release_srcdirs, [], [build_release_srcdirs,
                            paths_release_srcdirs]},
     {unbalanced_srcdirs, [], [build_unbalanced_srcdirs,
                               paths_unbalanced_srcdirs]},
     {basic_extras, [], [build_basic_extra_dirs,
                         paths_basic_extra_dirs,
                         clean_basic_extra_dirs]},
     {release_extras, [], [build_release_extra_dirs,
                           paths_release_extra_dirs,
                           clean_release_extra_dirs]},
     {unbalanced_extras, [], [build_unbalanced_extra_dirs,
                              paths_unbalanced_extra_dirs]},
     {root_extras, [], [build_extra_dirs_in_project_root,
                        paths_extra_dirs_in_project_root,
                        clean_extra_dirs_in_project_root]}].

init_per_group(basic_app, Config) ->
    NewConfig = rebar_test_utils:init_rebar_state(Config, "basic_app_"),
    AppDir = ?config(apps, NewConfig),

    Name = rebar_test_utils:create_random_name("app1"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    [{app_names, [Name]}, {vsns, [Vsn]}|NewConfig];

init_per_group(release_apps, Config) ->
    NewConfig = rebar_test_utils:init_rebar_state(Config, "release_apps_"),
    AppDir = ?config(apps, NewConfig),

    Name1 = rebar_test_utils:create_random_name("relapp1_"),
    Vsn1 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(filename:join([AppDir,"apps",Name1]), Name1, Vsn1, [kernel, stdlib]),

    Name2 = rebar_test_utils:create_random_name("relapp2_"),
    Vsn2 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(filename:join([AppDir,"apps",Name2]), Name2, Vsn2, [kernel, stdlib]),

    [{app_names, [Name1, Name2]}, {vsns, [Vsn1, Vsn2]}|NewConfig];

init_per_group(checkout_apps, Config) ->
    NewConfig = rebar_test_utils:init_rebar_state(Config, "checkout_apps_"),
    AppDir = ?config(apps, NewConfig),
    CheckoutsDir = ?config(checkouts, NewConfig),

    Name1 = rebar_test_utils:create_random_name("checkapp1_"),
    Vsn1 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name1, Vsn1, [kernel, stdlib]),

    Name2 = rebar_test_utils:create_random_name("checkapp2_"),
    Vsn2 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(filename:join([CheckoutsDir,Name2]), Name2, Vsn2, [kernel, stdlib]),

    [{app_names, [Name1, Name2]}, {vsns, [Vsn1, Vsn2]}|NewConfig];

init_per_group(checkout_deps, Config) ->
    NewConfig = rebar_test_utils:init_rebar_state(Config, "checkout_deps_"),
    AppDir = ?config(apps, NewConfig),
    CheckoutsDir = ?config(checkouts, NewConfig),
    DepsDir = filename:join([AppDir, "_build", "default", "lib"]),

    Name1 = rebar_test_utils:create_random_name("checkapp1_"),
    Vsn1 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name1, Vsn1, [kernel, stdlib]),

    Name2 = rebar_test_utils:create_random_name("checkapp2_"),
    Vsn2 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(filename:join([CheckoutsDir,Name2]), Name2, Vsn2, [kernel, stdlib]),

    rebar_test_utils:create_app(filename:join([DepsDir,Name2]), Name2, Vsn1, [kernel, stdlib]),

    [{app_names, [Name1, Name2]}, {vsns, [Vsn1, Vsn2]}|NewConfig];

init_per_group(Group, Config) when Group == basic_srcdirs; Group == basic_extras ->
    NewConfig = rebar_test_utils:init_rebar_state(Config, "basic_srcdirs_"),
    AppDir = ?config(apps, NewConfig),

    Name = rebar_test_utils:create_random_name("app1_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    ExtraSrc = io_lib:format("-module(~ts_extra).\n-export([ok/0]).\nok() -> ok.\n", [Name]),

    ok = filelib:ensure_dir(filename:join([AppDir, "extra", "dummy"])),
    ok = file:write_file(filename:join([AppDir, "extra", io_lib:format("~ts_extra.erl", [Name])]),
                         ExtraSrc),

    [{app_names, [Name]}, {vsns, [Vsn]}|NewConfig];

init_per_group(Group, Config) when Group == release_srcdirs; Group == release_extras ->
    NewConfig = rebar_test_utils:init_rebar_state(Config, "release_srcdirs_"),
    AppDir = ?config(apps, NewConfig),

    Name1 = rebar_test_utils:create_random_name("relapp1_"),
    Vsn1 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(filename:join([AppDir, "apps", Name1]), Name1, Vsn1, [kernel, stdlib]),

    Name2 = rebar_test_utils:create_random_name("relapp2_"),
    Vsn2 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(filename:join([AppDir, "apps", Name2]), Name2, Vsn2, [kernel, stdlib]),

    ExtraOne = io_lib:format("-module(~ts_extra).\n-export([ok/0]).\nok() -> ok.\n", [Name1]),

    ok = filelib:ensure_dir(filename:join([AppDir, "apps", Name1, "extra", "dummy"])),
    ok = file:write_file(filename:join([AppDir, "apps", Name1, "extra",
                                        io_lib:format("~ts_extra.erl", [Name1])]),
                         ExtraOne),

    ExtraTwo = io_lib:format("-module(~ts_extra).\n-export([ok/0]).\nok() -> ok.\n", [Name2]),

    ok = filelib:ensure_dir(filename:join([AppDir, "apps", Name2, "extra", "dummy"])),
    ok = file:write_file(filename:join([AppDir, "apps", Name2, "extra",
                                        io_lib:format("~ts_extra.erl", [Name2])]),
                         ExtraTwo),

    [{app_names, [Name1, Name2]}, {vsns, [Vsn1, Vsn2]}|NewConfig];

init_per_group(Group, Config) when Group == unbalanced_srcdirs; Group == unbalanced_extras ->
    NewConfig = rebar_test_utils:init_rebar_state(Config, "unbalanced_srcdirs_"),
    AppDir = ?config(apps, NewConfig),

    Name1 = rebar_test_utils:create_random_name("relapp1_"),
    Vsn1 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(filename:join([AppDir, "apps", Name1]), Name1, Vsn1, [kernel, stdlib]),

    Name2 = rebar_test_utils:create_random_name("relapp2_"),
    Vsn2 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(filename:join([AppDir, "apps", Name2]), Name2, Vsn2, [kernel, stdlib]),

    ExtraOne = io_lib:format("-module(~ts_extra).\n-export([ok/0]).\nok() -> ok.\n", [Name1]),

    ok = filelib:ensure_dir(filename:join([AppDir, "apps", Name1, "extra", "dummy"])),
    ok = file:write_file(filename:join([AppDir, "apps", Name1, "extra",
                                        io_lib:format("~ts_extra.erl", [Name1])]),
                         ExtraOne),

    [{app_names, [Name1, Name2]}, {vsns, [Vsn1, Vsn2]}|NewConfig];

init_per_group(root_extras, Config) ->
    NewConfig = rebar_test_utils:init_rebar_state(Config, "root_extras_"),
    AppDir = ?config(apps, NewConfig),

    Name1 = rebar_test_utils:create_random_name("relapp1_"),
    Vsn1 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(filename:join([AppDir, "apps", Name1]), Name1, Vsn1, [kernel, stdlib]),

    Name2 = rebar_test_utils:create_random_name("relapp2_"),
    Vsn2 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(filename:join([AppDir, "apps", Name2]), Name2, Vsn2, [kernel, stdlib]),

    Extra = <<"-module(extra).\n-export([ok/0]).\nok() -> ok.\n">>,

    ok = filelib:ensure_dir(filename:join([AppDir, "extra", "dummy"])),
    ok = file:write_file(filename:join([AppDir, "extra", "extra.erl"]), Extra),

    [{app_names, [Name1, Name2]}, {vsns, [Vsn1, Vsn2]}|NewConfig].

end_per_group(_Group, _Config) ->
    ok.

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(Test, Config) when
        Test == dont_recompile_when_erl_compiler_options_env_does_not_change
    orelse
        Test == recompile_when_erl_compiler_options_env_changes ->
    _ = code:ensure_loaded(os),
    UnSetEnv = erlang:function_exported(os, unsetenv, 1),
    _ = code:ensure_loaded(compile),
    EnvOpts = erlang:function_exported(compile, env_compiler_options, 0),
    case {UnSetEnv, EnvOpts} of
        {true, true} -> maybe_init_config(Config);
        _            -> {skip, "compile:env_compiler_options/0 unavailable"}
    end;
init_per_testcase(always_recompile_when_erl_compiler_options_set, Config) ->
    _ = code:ensure_loaded(os),
    UnSetEnv = erlang:function_exported(os, unsetenv, 1),
    _ = code:ensure_loaded(compile),
    EnvOpts = erlang:function_exported(compile, env_compiler_options, 0),
    case {UnSetEnv, EnvOpts} of
        {true, true}  -> {skip, "compile:env_compiler_options/0 available"};
        {true, false} -> maybe_init_config(Config);
        _             -> {skip, "os:unsetenv/1 unavailable"}
    end;
init_per_testcase(_, Config) -> maybe_init_config(Config).

maybe_init_config(Config) ->
    case ?config(apps, Config) of
        undefined -> rebar_test_utils:init_rebar_state(Config);
        _         -> Config
    end.

end_per_testcase(_, _Config) ->
    catch meck:unload().


%% test cases

build_basic_app(Config) ->
    [Name] = ?config(app_names, Config),
    rebar_test_utils:run_and_check(Config, [], ["compile"], {ok, [{app, Name}]}).

build_release_apps(Config) ->
    [Name1, Name2] = ?config(app_names, Config),
    rebar_test_utils:run_and_check(
        Config, [], ["compile"],
        {ok, [{app, Name1}, {app, Name2}]}
    ).

build_checkout_apps(Config) ->
    [Name1, Name2] = ?config(app_names, Config),
    rebar_test_utils:run_and_check(
        Config, [], ["compile"],
        {ok, [{app, Name1}, {checkout, Name2}]}
    ).

build_checkout_deps(Config) ->
    AppDir = ?config(apps, Config),
    [Name1, Name2] = ?config(app_names, Config),
    [_, Vsn2] = ?config(vsns, Config),

    Deps = [{list_to_atom(Name2), Vsn2, {git, "", ""}}],
    {ok, RebarConfig} = file:consult(rebar_test_utils:create_config(AppDir, [{deps, Deps}])),

    rebar_test_utils:run_and_check(
        Config, RebarConfig, ["compile"],
        {ok, [{app, Name1}, {checkout, Name2}]}
    ).

build_basic_srcdirs(Config) ->
    AppDir = ?config(apps, Config),
    [Name] = ?config(app_names, Config),

    RebarConfig = [{erl_opts, [{src_dirs, ["src", "extra"]}]}],

    %% check a beam corresponding to the src in the extra src_dir exists
    ExtraBeam = filename:join([AppDir, "_build", "default", "lib", Name, "ebin",
                               io_lib:format("~ts_extra.beam", [Name])]),
    %% check the extra src_dir was copied/linked into the _build dir
    ExtraDir = filename:join([AppDir, "_build", "default", "lib", Name, "extra"]),

    rebar_test_utils:run_and_check(
        Config, RebarConfig, ["compile"],
        {ok, [{app, Name}, {file, ExtraBeam}, {dir, ExtraDir}]}
    ).

build_release_srcdirs(Config) ->
    AppDir = ?config(apps, Config),
    [Name1, Name2] = ?config(app_names, Config),

    RebarConfig = [{erl_opts, [{src_dirs, ["src", "extra"]}]}],

    %% check a beam corresponding to the src in the extra src_dir exists
    Extra1Beam = filename:join([AppDir, "_build", "default", "lib", Name1, "ebin",
                                io_lib:format("~ts_extra.beam", [Name1])]),
    Extra2Beam = filename:join([AppDir, "_build", "default", "lib", Name2, "ebin",
                                io_lib:format("~ts_extra.beam", [Name2])]),

    %% check the extra src_dir was copied/linked into the _build dir
    Extra1Dir = filename:join([AppDir, "_build", "default", "lib", Name1, "extra"]),
    Extra2Dir = filename:join([AppDir, "_build", "default", "lib", Name2, "extra"]),

    rebar_test_utils:run_and_check(
        Config, RebarConfig, ["compile"],
        {ok, [{app, Name1}, {app, Name2},
              {file, Extra1Beam}, {file, Extra2Beam},
              {dir, Extra1Dir}, {dir, Extra2Dir}]}
    ).

build_unbalanced_srcdirs(Config) ->
    AppDir = ?config(apps, Config),
    [Name1, Name2] = ?config(app_names, Config),

    RebarConfig = [{erl_opts, [{src_dirs, ["src", "extra"]}]}],

    %% check a beam corresponding to the src in the extra src_dir exists
    Extra1Beam = filename:join([AppDir, "_build", "default", "lib", Name1, "ebin",
                                io_lib:format("~ts_extra.beam", [Name1])]),

    %% check the extra src_dir was copied/linked into the _build dir
    Extra1Dir = filename:join([AppDir, "_build", "default", "lib", Name1, "extra"]),

    rebar_test_utils:run_and_check(
        Config, RebarConfig, ["compile"],
        {ok, [{app, Name1}, {app, Name2}, {file, Extra1Beam}, {dir, Extra1Dir}]}
    ),

    %% check no extra src_dir were copied/linked into the _build dir
    Extra2Dir = filename:join([AppDir, "_build", "default", "lib", Name2, "extra"]),
    false = filelib:is_dir(Extra2Dir),
    %% check only expected beams are in the ebin dir
    {ok, Files} = rebar_utils:list_dir(filename:join([AppDir, "_build", "default", "lib", Name2, "ebin"])),
    lists:all(fun(Beam) -> lists:member(Beam, [Name2 ++ ".app", "not_a_real_src_" ++ Name2 ++ ".beam"]) end,
              Files).

build_basic_extra_dirs(Config) ->
    AppDir = ?config(apps, Config),
    [Name] = ?config(app_names, Config),

    RebarConfig = [{erl_opts, [{extra_src_dirs, ["extra"]}]}],

    %% check a beam corresponding to the src in the extra src_dir exists
    ExtraBeam = filename:join([AppDir, "_build", "default", "lib", Name, "extra",
                               io_lib:format("~ts_extra.beam", [Name])]),

    rebar_test_utils:run_and_check(
        Config, RebarConfig, ["compile"],
        {ok, [{app, Name}, {file, ExtraBeam}]}
    ).

build_release_extra_dirs(Config) ->
    AppDir = ?config(apps, Config),
    [Name1, Name2] = ?config(app_names, Config),

    RebarConfig = [{erl_opts, [{extra_src_dirs, ["extra"]}]}],

    %% check a beam corresponding to the src in the extra src_dir exists
    Extra1Beam = filename:join([AppDir, "_build", "default", "lib", Name1, "extra",
                                io_lib:format("~ts_extra.beam", [Name1])]),
    Extra2Beam = filename:join([AppDir, "_build", "default", "lib", Name2, "extra",
                                io_lib:format("~ts_extra.beam", [Name2])]),

    rebar_test_utils:run_and_check(
        Config, RebarConfig, ["compile"],
        {ok, [{app, Name1}, {app, Name2}, {file, Extra1Beam}, {file, Extra2Beam}]}
    ).

build_unbalanced_extra_dirs(Config) ->
    AppDir = ?config(apps, Config),
    [Name1, Name2] = ?config(app_names, Config),

    RebarConfig = [{erl_opts, [{extra_src_dirs, ["extra"]}]}],

    %% check a beam corresponding to the src in the extra src_dir exists
    Extra1Beam = filename:join([AppDir, "_build", "default", "lib", Name1, "extra",
                                io_lib:format("~ts_extra.beam", [Name1])]),

    rebar_test_utils:run_and_check(
        Config, RebarConfig, ["compile"],
        {ok, [{app, Name1}, {app, Name2}, {file, Extra1Beam}]}
    ),

    %% check no extra src_dir were copied/linked into the _build dir
    false = filelib:is_dir(filename:join([AppDir, "_build", "default", "lib", Name2, "extra"])),
    %% check only expected beams are in the ebin dir
    {ok, Files} = rebar_utils:list_dir(filename:join([AppDir, "_build", "default", "lib", Name2, "ebin"])),
    lists:all(fun(Beam) -> lists:member(Beam, [Name2 ++ ".app", "not_a_real_src_" ++ Name2 ++ ".beam"]) end,
              Files).

build_extra_dirs_in_project_root(Config) ->
    AppDir = ?config(apps, Config),
    [Name1, Name2] = ?config(app_names, Config),

    RebarConfig = [{erl_opts, [{extra_src_dirs, ["extra"]}]}],

    %% check a beam corresponding to the src in the extra src_dir exists
    ExtraBeam = filename:join([AppDir, "_build", "default", "extras", "extra", "extra.beam"]),

    rebar_test_utils:run_and_check(
        Config, RebarConfig, ["compile"],
        {ok, [{app, Name1}, {app, Name2}, {file, ExtraBeam}]}
    ).

paths_basic_app(Config) ->
    [Name] = ?config(app_names, Config),
    [Vsn] = ?config(vsns, Config),

    {ok, State} = rebar_test_utils:run_and_check(Config, [], ["compile"], return),

    code:add_paths(rebar_state:code_paths(State, all_deps)),
    ok = application:load(list_to_atom(Name)),
    Loaded = application:loaded_applications(),
    {_, _, Vsn} = lists:keyfind(list_to_atom(Name), 1, Loaded).

paths_release_apps(Config) ->
    [Name1, Name2] = ?config(app_names, Config),
    [Vsn1, Vsn2] = ?config(vsns, Config),

    {ok, State} = rebar_test_utils:run_and_check(Config, [], ["compile"], return),

    code:add_paths(rebar_state:code_paths(State, all_deps)),
    ok = application:load(list_to_atom(Name1)),
    ok = application:load(list_to_atom(Name2)),
    Loaded = application:loaded_applications(),
    {_, _, Vsn1} = lists:keyfind(list_to_atom(Name1), 1, Loaded),
    {_, _, Vsn2} = lists:keyfind(list_to_atom(Name2), 1, Loaded).

paths_checkout_apps(Config) ->
    [Name1, _Name2] = ?config(app_names, Config),
    [Vsn1, _Vsn2] = ?config(vsns, Config),

    {ok, State} = rebar_test_utils:run_and_check(Config, [], ["compile"], return),

    code:add_paths(rebar_state:code_paths(State, all_deps)),
    ok = application:load(list_to_atom(Name1)),
    Loaded = application:loaded_applications(),
    {_, _, Vsn1} = lists:keyfind(list_to_atom(Name1), 1, Loaded).

paths_checkout_deps(Config) ->
    AppDir = ?config(apps, Config),
    [_Name1, Name2] = ?config(app_names, Config),
    [_Vsn1, Vsn2] = ?config(vsns, Config),

    %% rebar_test_utils:init_rebar_state/1,2 uses rebar_state:new/3 which
    %% maybe incorrectly sets deps to [] (based on `rebar.lock`) instead of
    %% to the checkapps
    %% until that is sorted out the lock file has to be removed before
    %% this test will pass
    file:delete(filename:join([AppDir, "rebar.lock"])),

    {ok, RebarConfig} = file:consult(filename:join([AppDir, "rebar.config"])),

    {ok, State} = rebar_test_utils:run_and_check(Config, RebarConfig, ["compile"], return),

    code:add_paths(rebar_state:code_paths(State, all_deps)),
    ok = application:load(list_to_atom(Name2)),
    Loaded = application:loaded_applications(),
    {_, _, Vsn2} = lists:keyfind(list_to_atom(Name2), 1, Loaded).

paths_basic_srcdirs(Config) ->
    AppDir = ?config(apps, Config),
    [Name] = ?config(app_names, Config),

    RebarConfig = [{erl_opts, [{src_dirs, ["src", "extra"]}]}],

    {ok, State} = rebar_test_utils:run_and_check(Config, RebarConfig, ["compile"], return),

    code:add_paths(rebar_state:code_paths(State, all_deps)),
    Mod = list_to_atom(lists:flatten(io_lib:format("~ts_extra", [Name]))),
    {module, Mod} = code:ensure_loaded(Mod),

    Expect = filename:join([AppDir, "_build", "default", "lib", Name, "ebin",
                            io_lib:format("~ts_extra.beam", [Name])]),
    Expect = code:which(Mod).

paths_release_srcdirs(Config) ->
    AppDir = ?config(apps, Config),
    [Name1, Name2] = ?config(app_names, Config),

    RebarConfig = [{erl_opts, [{src_dirs, ["src", "extra"]}]}],

    {ok, State} = rebar_test_utils:run_and_check(Config, RebarConfig, ["compile"], return),

    code:add_paths(rebar_state:code_paths(State, all_deps)),
    Mod1 = list_to_atom(lists:flatten(io_lib:format("~ts_extra", [Name1]))),
    {module, Mod1} = code:ensure_loaded(Mod1),
    Mod2 = list_to_atom(lists:flatten(io_lib:format("~ts_extra", [Name2]))),
    {module, Mod2} = code:ensure_loaded(Mod2),

    ExpectOne = filename:join([AppDir, "_build", "default", "lib", Name1, "ebin",
                               io_lib:format("~ts_extra.beam", [Name1])]),
    ExpectOne = code:which(Mod1),
    ExpectTwo = filename:join([AppDir, "_build", "default", "lib", Name2, "ebin",
                               io_lib:format("~ts_extra.beam", [Name2])]),
    ExpectTwo = code:which(Mod2).

paths_unbalanced_srcdirs(Config) ->
    AppDir = ?config(apps, Config),
    [Name1, Name2] = ?config(app_names, Config),

    RebarConfig = [{erl_opts, [{src_dirs, ["src", "extra"]}]}],

    {ok, State} = rebar_test_utils:run_and_check(Config, RebarConfig, ["compile"], return),

    code:add_paths(rebar_state:code_paths(State, all_deps)),
    Mod1 = list_to_atom(lists:flatten(io_lib:format("~ts_extra", [Name1]))),
    {module, Mod1} = code:ensure_loaded(Mod1),
    Mod2 = list_to_atom(lists:flatten(io_lib:format("~ts_extra", [Name2]))),
    {error, nofile} = code:ensure_loaded(Mod2),

    ExpectOne = filename:join([AppDir, "_build", "default", "lib", Name1, "ebin",
                               io_lib:format("~ts_extra.beam", [Name1])]),
    ExpectOne = code:which(Mod1).

paths_basic_extra_dirs(Config) ->
    AppDir = ?config(apps, Config),
    [Name] = ?config(app_names, Config),

    RebarConfig = [{erl_opts, [{extra_src_dirs, ["extra"]}]}],

    {ok, State} = rebar_test_utils:run_and_check(Config, RebarConfig, ["compile"], return),

    code:add_paths(rebar_state:code_paths(State, all_deps)),
    Mod = list_to_atom(lists:flatten(io_lib:format("~ts_extra", [Name]))),
    {module, Mod} = code:ensure_loaded(Mod),

    Expect = filename:join([AppDir, "_build", "default", "lib", Name, "extra",
                            io_lib:format("~ts_extra.beam", [Name])]),
    Expect = code:which(Mod).

paths_release_extra_dirs(Config) ->
    AppDir = ?config(apps, Config),
    [Name1, Name2] = ?config(app_names, Config),

    RebarConfig = [{erl_opts, [{extra_src_dirs, ["extra"]}]}],

    {ok, State} = rebar_test_utils:run_and_check(Config, RebarConfig, ["compile"], return),

    code:add_paths(rebar_state:code_paths(State, all_deps)),
    Mod1 = list_to_atom(lists:flatten(io_lib:format("~ts_extra", [Name1]))),
    {module, Mod1} = code:ensure_loaded(Mod1),
    Mod2 = list_to_atom(lists:flatten(io_lib:format("~ts_extra", [Name2]))),
    {module, Mod2} = code:ensure_loaded(Mod2),

    ExpectOne = filename:join([AppDir, "_build", "default", "lib", Name1, "extra",
                               io_lib:format("~ts_extra.beam", [Name1])]),
    ExpectOne = code:which(Mod1),
    ExpectTwo = filename:join([AppDir, "_build", "default", "lib", Name2, "extra",
                               io_lib:format("~ts_extra.beam", [Name2])]),
    ExpectTwo = code:which(Mod2).


paths_unbalanced_extra_dirs(Config) ->
    AppDir = ?config(apps, Config),
    [Name1, Name2] = ?config(app_names, Config),

    RebarConfig = [{erl_opts, [{extra_src_dirs, ["extra"]}]}],

    {ok, State} = rebar_test_utils:run_and_check(Config, RebarConfig, ["compile"], return),

    code:add_paths(rebar_state:code_paths(State, all_deps)),
    Mod1 = list_to_atom(lists:flatten(io_lib:format("~ts_extra", [Name1]))),
    {module, Mod1} = code:ensure_loaded(Mod1),
    Mod2 = list_to_atom(lists:flatten(io_lib:format("~ts_extra", [Name2]))),
    {error, nofile} = code:ensure_loaded(Mod2),

    ExpectOne = filename:join([AppDir, "_build", "default", "lib", Name1, "extra",
                               io_lib:format("~ts_extra.beam", [Name1])]),
    ExpectOne = code:which(Mod1).

paths_extra_dirs_in_project_root(Config) ->
    AppDir = ?config(apps, Config),

    RebarConfig = [{erl_opts, [{extra_src_dirs, ["extra"]}]}],

    {ok, State} = rebar_test_utils:run_and_check(Config, RebarConfig, ["compile"], return),

    code:add_paths(rebar_state:code_paths(State, all_deps)),
    {module, extra} = code:ensure_loaded(extra),

    Expect = filename:join([AppDir, "_build", "default", "extras", "extra", "extra.beam"]),
    Expect = code:which(extra).

clean_basic_app(Config) ->
    [Name] = ?config(app_names, Config),

    rebar_test_utils:run_and_check(Config, [], ["clean"], {ok, [{app, Name, invalid}]}).

clean_release_apps(Config) ->
    [Name1, Name2] = ?config(app_names, Config),

    rebar_test_utils:run_and_check(Config, [], ["clean"],
                                   {ok, [{app, Name1, invalid}, {app, Name2, invalid}]}).

clean_basic_extra_dirs(Config) ->
    AppDir = ?config(apps, Config),
    [Name] = ?config(app_names, Config),

    rebar_test_utils:run_and_check(Config, [], ["clean"], {ok, [{app, Name, invalid}]}),

    Beam = lists:flatten(io_lib:format("~ts_extra", [Name])),
    false = ec_file:exists(filename:join([AppDir, "_build", "default", "lib", Name, "extras", Beam])).

clean_release_extra_dirs(Config) ->
    AppDir = ?config(apps, Config),
    [Name1, Name2] = ?config(app_names, Config),

    rebar_test_utils:run_and_check(Config, [], ["clean"],
                                   {ok, [{app, Name1, invalid}, {app, Name2, invalid}]}),

    Beam1 = lists:flatten(io_lib:format("~ts_extra", [Name1])),
    false = ec_file:exists(filename:join([AppDir, "_build", "default", "lib", Name1, "extras", Beam1])),
    Beam2 = lists:flatten(io_lib:format("~ts_extra", [Name2])),
    false = ec_file:exists(filename:join([AppDir, "_build", "default", "lib", Name2, "extras", Beam2])).

clean_extra_dirs_in_project_root(Config) ->
    AppDir = ?config(apps, Config),
    [Name1, Name2] = ?config(app_names, Config),

    rebar_test_utils:run_and_check(Config, [], ["clean"],
                                   {ok, [{app, Name1, invalid}, {app, Name2, invalid}]}),

    false = ec_file:exists(filename:join([AppDir, "_build", "default", "extras"])).

recompile_when_hrl_changes(Config) ->
    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("app1_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    ExtraSrc = <<"-module(test_header_include).\n"
                  "-export([main/0]).\n"
                  "-include(\"test_header_include.hrl\").\n"
                  "main() -> ?SOME_DEFINE.\n">>,

    ExtraHeader = <<"-define(SOME_DEFINE, true).\n">>,
    HeaderFile = filename:join([AppDir, "src", "test_header_include.hrl"]),
    ok = file:write_file(filename:join([AppDir, "src", "test_header_include.erl"]), ExtraSrc),
    ok = file:write_file(HeaderFile, ExtraHeader),

    rebar_test_utils:run_and_check(Config, [], ["compile"], {ok, [{app, Name}]}),

    EbinDir = filename:join([AppDir, "_build", "default", "lib", Name, "ebin"]),
    {ok, Files} = rebar_utils:list_dir(EbinDir),
    ModTime = [filelib:last_modified(filename:join([EbinDir, F]))
               || F <- Files, filename:extension(F) == ".beam"],

    timer:sleep(1000),

    NewExtraHeader = <<"-define(SOME_DEFINE, false).\n">>,
    ok = file:write_file(HeaderFile, NewExtraHeader),

    rebar_test_utils:run_and_check(Config, [], ["compile"], {ok, [{app, Name}]}),

    {ok, NewFiles} = rebar_utils:list_dir(EbinDir),
    NewModTime = [filelib:last_modified(filename:join([EbinDir, F]))
                  || F <- NewFiles, filename:extension(F) == ".beam"],

    ?assert(ModTime =/= NewModTime).

recompile_when_included_hrl_changes(Config) ->
    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("app1_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    ExtraSrc = <<"-module(test_header_include).\n"
                  "-export([main/0]).\n"
                  "-include(\"test_header_include.hrl\").\n"
                  "main() -> ?SOME_DEFINE.\n">>,

    ExtraHeader = <<"-define(SOME_DEFINE, true).\n">>,
    ok = filelib:ensure_dir(filename:join([AppDir, "include", "dummy"])),
    HeaderFile = filename:join([AppDir, "include", "test_header_include.hrl"]),
    ok = file:write_file(filename:join([AppDir, "src", "test_header_include.erl"]), ExtraSrc),
    ok = file:write_file(HeaderFile, ExtraHeader),

    rebar_test_utils:run_and_check(Config, [], ["compile"], {ok, [{app, Name}]}),

    EbinDir = filename:join([AppDir, "_build", "default", "lib", Name, "ebin"]),
    {ok, Files} = rebar_utils:list_dir(EbinDir),
    ModTime = [filelib:last_modified(filename:join([EbinDir, F]))
               || F <- Files, filename:extension(F) == ".beam"],

    timer:sleep(1000),

    NewExtraHeader = <<"-define(SOME_DEFINE, false).\n">>,
    ok = file:write_file(HeaderFile, NewExtraHeader),

    rebar_test_utils:run_and_check(Config, [], ["compile"], {ok, [{app, Name}]}),

    {ok, NewFiles} = rebar_utils:list_dir(EbinDir),
    NewModTime = [filelib:last_modified(filename:join([EbinDir, F]))
                  || F <- NewFiles, filename:extension(F) == ".beam"],

    ?assert(ModTime =/= NewModTime).

recompile_when_opts_included_hrl_changes(Config) ->
    AppsDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("app1_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    AppDir = filename:join([AppsDir, "apps", Name]),

    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    ExtraSrc = <<"-module(test_header_include).\n"
                  "-export([main/0]).\n"
                  "-include(\"test_header_include.hrl\").\n"
                  "main() -> ?SOME_DEFINE.\n">>,

    ExtraHeader = <<"-define(SOME_DEFINE, true).\n">>,
    ok = filelib:ensure_dir(filename:join([AppsDir, "include", "dummy"])),
    HeaderFile = filename:join([AppsDir, "include", "test_header_include.hrl"]),
    ok = file:write_file(filename:join([AppDir, "src", "test_header_include.erl"]), ExtraSrc),
    ok = file:write_file(HeaderFile, ExtraHeader),

    %% Using relative path from the project root
    RebarConfig = [{erl_opts, [{i, "include/"}]}],
    {ok,Cwd} = file:get_cwd(),
    ok = file:set_cwd(AppsDir),

    rebar_test_utils:run_and_check(Config, RebarConfig, ["compile"], {ok, [{app, Name}]}),

    EbinDir = filename:join([AppsDir, "_build", "default", "lib", Name, "ebin"]),
    {ok, Files} = rebar_utils:list_dir(EbinDir),
    ModTime = [filelib:last_modified(filename:join([EbinDir, F]))
               || F <- Files, filename:extension(F) == ".beam"],

    timer:sleep(1000),

    NewExtraHeader = <<"-define(SOME_DEFINE, false).\n">>,
    ok = file:write_file(HeaderFile, NewExtraHeader),

    rebar_test_utils:run_and_check(Config, RebarConfig, ["compile"], {ok, [{app, Name}]}),

    {ok, NewFiles} = rebar_utils:list_dir(EbinDir),
    NewModTime = [filelib:last_modified(filename:join([EbinDir, F]))
                  || F <- NewFiles, filename:extension(F) == ".beam"],

    ok = file:set_cwd(Cwd),

    ?assert(ModTime =/= NewModTime).

recompile_when_opts_change(Config) ->
    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("app1_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    rebar_test_utils:run_and_check(Config, [], ["compile"], {ok, [{app, Name}]}),

    EbinDir = filename:join([AppDir, "_build", "default", "lib", Name, "ebin"]),
    {ok, Files} = rebar_utils:list_dir(EbinDir),
    ModTime = [filelib:last_modified(filename:join([EbinDir, F]))
               || F <- Files, filename:extension(F) == ".beam"],

    timer:sleep(1000),

    rebar_test_utils:create_config(AppDir, [{erl_opts, [{d, some_define}]}]),

    rebar_test_utils:run_and_check(Config, [], ["compile"], {ok, [{app, Name}]}),

    {ok, NewFiles} = rebar_utils:list_dir(EbinDir),
    NewModTime = [filelib:last_modified(filename:join([EbinDir, F]))
                  || F <- NewFiles, filename:extension(F) == ".beam"],

    ?assert(ModTime =/= NewModTime).

dont_recompile_when_opts_dont_change(Config) ->
    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("app1_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    rebar_test_utils:run_and_check(Config, [], ["compile"], {ok, [{app, Name}]}),

    EbinDir = filename:join([AppDir, "_build", "default", "lib", Name, "ebin"]),
    {ok, Files} = rebar_utils:list_dir(EbinDir),
    ModTime = [filelib:last_modified(filename:join([EbinDir, F]))
               || F <- Files, filename:extension(F) == ".beam"],

    timer:sleep(1000),

    rebar_test_utils:run_and_check(Config, [], ["compile"], {ok, [{app, Name}]}),

    {ok, NewFiles} = rebar_utils:list_dir(EbinDir),
    NewModTime = [filelib:last_modified(filename:join([EbinDir, F]))
                  || F <- NewFiles, filename:extension(F) == ".beam"],

    ?assertEqual(ModTime, NewModTime).

dont_recompile_yrl_or_xrl(Config) ->
    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("app1_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    Xrl = filename:join([AppDir, "src", "not_a_real_xrl_" ++ Name ++ ".xrl"]),
    ok = filelib:ensure_dir(Xrl),
    XrlBody =
        "Definitions."
        "\n\n"
        "D = [0-9]"
        "\n\n"
        "Rules."
        "\n\n"
        "{D}+ :"
        "  {token,{integer,TokenLine,list_to_integer(TokenChars)}}."
        "\n\n"
        "{D}+\\.{D}+((E|e)(\\+|\\-)?{D}+)? :"
        "  {token,{float,TokenLine,list_to_float(TokenChars)}}."
        "\n\n"
        "Erlang code.",
    ok = ec_file:write(Xrl, XrlBody),

    XrlBeam = filename:join([AppDir, "ebin", filename:basename(Xrl, ".xrl") ++ ".beam"]),

    rebar_test_utils:run_and_check(Config, [], ["compile"], {ok, [{app, Name}]}),

    ModTime = filelib:last_modified(XrlBeam),

    timer:sleep(1000),

    rebar_test_utils:run_and_check(Config, [], ["compile"], {ok, [{app, Name}]}),

    NewModTime = filelib:last_modified(XrlBeam),

    ?assert(ModTime == NewModTime).

delete_beam_if_source_deleted(Config) ->
    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("app1_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    rebar_test_utils:run_and_check(Config, [], ["compile"], {ok, [{app, Name}]}),

    EbinDir = filename:join([AppDir, "_build", "default", "lib", Name, "ebin"]),
    _SrcDir = filename:join([AppDir, "_build", "default", "lib", Name, "src"]),
    ?assert(filelib:is_regular(filename:join(EbinDir, "not_a_real_src_" ++ Name ++ ".beam"))),
    file:delete(filename:join([AppDir, "src", "not_a_real_src_" ++ Name ++ ".erl"])),

    rebar_test_utils:run_and_check(Config, [], ["compile"], {ok, [{app, Name}]}),

    ?assertNot(filelib:is_regular(filename:join(EbinDir, "not_a_real_src_" ++ Name ++ ".beam"))).

deps_in_path(Config) ->
    AppDir = ?config(apps, Config),
    StartPaths = code:get_path(),

    Name = rebar_test_utils:create_random_name("app1_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    DepName = rebar_test_utils:create_random_name("dep1_"),
    PkgName = rebar_test_utils:create_random_name("pkg1_"),
    mock_git_resource:mock([]),
    mock_pkg_resource:mock([
        {pkgdeps, [{{iolist_to_binary(PkgName), iolist_to_binary(Vsn)}, []}]}
    ]),

    RConfFile = rebar_test_utils:create_config(AppDir, [{deps, [
        {list_to_atom(DepName), {git, "http://site.com/user/"++DepName++".git", {tag, Vsn}}},
        {list_to_atom(PkgName), Vsn}
    ]}]),
    {ok, RConf} = file:consult(RConfFile),
    %% Make sure apps we look for are not visible
    %% Hope not to find src name
    ?assertEqual([], [Path || Path <- code:get_path(),
                              {match, _} <- [re:run(Path, DepName)]]),
    %% Hope not to find pkg name in there

    ?assertEqual([], [Path || Path <- code:get_path(),
                                 {match, _} <- [re:run(Path, PkgName)]]),
    %% Build things
    {ok, State} = rebar_test_utils:run_and_check(
        Config, RConf, ["compile"],
        {ok, [{app, Name}, {dep, DepName}, {dep, PkgName}]}
    ),
    code:add_paths(rebar_state:code_paths(State, all_deps)),
    %% Find src name in there
    ?assertNotEqual([], [Path || Path <- code:get_path(),
                                 {match, _} <- [re:run(Path, DepName)]]),
    %% find pkg name in there
    ?assertNotEqual([], [Path || Path <- code:get_path(),
                                 {match, _} <- [re:run(Path, PkgName)]]),

    true = code:set_path(lists:filter(fun(P) -> ec_file:exists(P) end, StartPaths)),
    %% Make sure apps we look for are not visible again
    %% Hope not to find src name
    ?assertEqual([], [Path || Path <- code:get_path(),
                              {match, _} <- [re:run(Path, DepName)]]),
    %% Hope not to find pkg name in there
    ?assertEqual([], [Path || Path <- code:get_path(),
                                 {match, _} <- [re:run(Path, PkgName)]]),
    %% Rebuild
    {ok, State1} = rebar_test_utils:run_and_check(
        Config, RConf, ["compile"],
        {ok, [{app, Name}, {dep, DepName}, {dep, PkgName}]}
    ),
    %% Find src name in there
    code:add_paths(rebar_state:code_paths(State1, all_deps)),
    ?assertNotEqual([], [Path || Path <- code:get_path(),
                                 {match, _} <- [re:run(Path, DepName)]]),
    %% find pkg name in there
    ?assertNotEqual([], [Path || Path <- code:get_path(),
                                 {match, _} <- [re:run(Path, PkgName)]]).

checkout_priority(Config) ->
    AppDir = ?config(apps, Config),
    CheckoutsDir = ?config(checkouts, Config),
    StartPaths = code:get_path(),

    Name = rebar_test_utils:create_random_name("app1_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    DepName = rebar_test_utils:create_random_name("dep1_"),
    PkgName = rebar_test_utils:create_random_name("pkg1_"),
    mock_git_resource:mock([]),
    mock_pkg_resource:mock([
        {pkgdeps, [{{iolist_to_binary(PkgName), iolist_to_binary(Vsn)}, []}]}
    ]),

    RConfFile = rebar_test_utils:create_config(AppDir, [{deps, [
        {list_to_atom(DepName), {git, "http://site.com/user/"++DepName++".git", {tag, Vsn}}},
        {list_to_atom(PkgName), Vsn}
    ]}]),
    {ok, RConf} = file:consult(RConfFile),

    %% Build with deps.
    rebar_test_utils:run_and_check(
        Config, RConf, ["compile"],
        {ok, [{app, Name}, {dep, DepName}, {dep, PkgName}]}
    ),

    %% Build two checkout apps similar to dependencies to be fetched,
    %% but on a different version
    Vsn2 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(filename:join([CheckoutsDir,DepName]), DepName, Vsn2, [kernel, stdlib]),
    rebar_test_utils:create_app(filename:join([CheckoutsDir,PkgName]), PkgName, Vsn2, [kernel, stdlib]),

    %% Rebuild and make sure the checkout apps are in path
    code:set_path(StartPaths),
    {ok, State} = rebar_test_utils:run_and_check(
        Config, RConf, ["compile"],
        {ok, [{app, Name}, {checkout, DepName}, {checkout, PkgName}]}
    ),
    code:add_paths(rebar_state:code_paths(State, all_deps)),
    [DepPath] = [Path || Path <- code:get_path(),
                         {match, _} <- [re:run(Path, DepName)]],
    [PkgPath] = [Path || Path <- code:get_path(),
                         {match, _} <- [re:run(Path, PkgName)]],

    {ok, [DepApp]} = file:consult(filename:join([DepPath, DepName ++ ".app"])),
    {ok, [PkgApp]} = file:consult(filename:join([PkgPath, PkgName ++ ".app"])),

    {application, _, DepProps} = DepApp,
    {application, _, PkgProps} = PkgApp,

    ?assertEqual(Vsn2, proplists:get_value(vsn, DepProps)),
    ?assertEqual(Vsn2, proplists:get_value(vsn, PkgProps)).


highest_version_of_pkg_dep(Config) ->
    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("app1_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    PkgName = rebar_test_utils:create_random_name("pkg1_"),
    mock_git_resource:mock([]),
    mock_pkg_resource:mock([
        {pkgdeps, [{{iolist_to_binary(PkgName), <<"0.1.0">>}, []},
                   {{iolist_to_binary(PkgName), <<"0.0.1">>}, []},
                   {{iolist_to_binary(PkgName), <<"0.1.3">>}, []},
                   {{iolist_to_binary(PkgName), <<"0.1.1">>}, []}]}
    ]),

    RConfFile = rebar_test_utils:create_config(AppDir, [{deps, [list_to_atom(PkgName)]}]),
    {ok, RConf} = file:consult(RConfFile),

    %% Build with deps.
    rebar_test_utils:run_and_check(
        Config, RConf, ["compile"],
        {ok, [{app, Name}, {dep, PkgName, <<"0.1.3">>}]}
    ).

parse_transform_test(Config) ->
    AppDir = ?config(apps, Config),

    RebarConfig = [{erl_opts, [{parse_transform, pascal}]}],

    Name = rebar_test_utils:create_random_name("app1_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    ExtraSrc = <<"-module(pascal). "
                 "-export([parse_transform/2]). "
                 "parse_transform(Forms, _Options) -> "
                 "Forms.">>,

    ok = file:write_file(filename:join([AppDir, "src", "pascal.erl"]), ExtraSrc),

    rebar_test_utils:run_and_check(Config, RebarConfig, ["compile"], {ok, [{app, Name}]}),

    EbinDir = filename:join([AppDir, "_build", "default", "lib", Name, "ebin"]),
    true = filelib:is_file(filename:join([EbinDir, "pascal.beam"])).

erl_first_files_test(Config) ->
    AppDir = ?config(apps, Config),
    RebarConfig = [{erl_opts, [{parse_transform, mark_time}]},
                   {erl_first_files, ["src/mark_time.erl",
                                      "src/b.erl",
                                      "src/d.erl",
                                      "src/a.erl"]}],

    Name = rebar_test_utils:create_random_name("app1_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),
    rebar_test_utils:write_src_file(AppDir, "a.erl"),
    rebar_test_utils:write_src_file(AppDir, "b.erl"),
    rebar_test_utils:write_src_file(AppDir, "d.erl"),
    rebar_test_utils:write_src_file(AppDir, "e.erl"),

    ExtraSrc = <<"-module(mark_time). "
                 "-export([parse_transform/2]). "
                 "parse_transform([Form={attribute,_,module,Mod}|Forms], Options) -> "
                 "    [Form, {attribute,1,number, os:timestamp()} | Forms];"
                 "parse_transform([Form|Forms], Options) -> "
                 "    [Form | parse_transform(Forms, Options)].">>,

    ok = file:write_file(filename:join([AppDir, "src", "mark_time.erl"]), ExtraSrc),

    rebar_test_utils:run_and_check(Config, RebarConfig, ["compile"], {ok, [{app, Name}]}),

    EbinDir = filename:join([AppDir, "_build", "default", "lib", Name, "ebin"]),
    true = filelib:is_file(filename:join([EbinDir, "mark_time.beam"])),

    code:load_abs(filename:join([EbinDir, "a"])),
    code:load_abs(filename:join([EbinDir, "b"])),
    code:load_abs(filename:join([EbinDir, "d"])),
    code:load_abs(filename:join([EbinDir, "e"])),
    A = proplists:get_value(number, a:module_info(attributes)),
    B = proplists:get_value(number, b:module_info(attributes)),
    D = proplists:get_value(number, d:module_info(attributes)),
    E = proplists:get_value(number, e:module_info(attributes)),
    ?assertEqual([B,D,A,E], lists:sort([A,B,D,E])).

mib_test(Config) ->
    AppDir = ?config(apps, Config),

    RebarConfig = [{mib_first_files, ["mibs/SIMPLE-MIB.mib"]}],

    Name = rebar_test_utils:create_random_name("app1_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    MibsSrc = <<"-- SIMPLE-MIB.\n"
"-- This is just a simple MIB used for testing!\n"
"--\n"
"SIMPLE-MIB DEFINITIONS ::= BEGIN\n"
"IMPORTS\n"
"    MODULE-IDENTITY, enterprises\n"
"        FROM SNMPv2-SMI;\n"
"\n"
"ericsson MODULE-IDENTITY\n"
"    LAST-UPDATED\n"
"        \"201403060000Z\"\n"
"    ORGANIZATION\n"
"        \"rebar\"\n"
"    CONTACT-INFO\n"
"        \"rebar <rebar@example.com>\n"
"    or\n"
"    whoever is currently responsible for the SIMPLE\n"
"    enterprise MIB tree branch (enterprises.999).\"\n"
"    DESCRIPTION\n"
"        \"This very small module is made available\n"
"	for mib-compilation testing.\"\n"
"    ::= { enterprises 999 }\n"
"END\n">>,

    ok = filelib:ensure_dir(filename:join([AppDir, "mibs", "dummy"])),
    ok = file:write_file(filename:join([AppDir, "mibs", "SIMPLE-MIB.mib"]), MibsSrc),

    rebar_test_utils:run_and_check(Config, RebarConfig, ["compile"], {ok, [{app, Name}]}),

    %% check a bin corresponding to the mib in the mibs dir exists in priv/mibs
    PrivMibsDir = filename:join([AppDir, "_build", "default", "lib", Name, "priv", "mibs"]),
    true = filelib:is_file(filename:join([PrivMibsDir, "SIMPLE-MIB.bin"])),

    %% check a hrl corresponding to the mib in the mibs dir exists in include
    true = filelib:is_file(filename:join([AppDir, "include", "SIMPLE-MIB.hrl"])),

    %% check the mibs dir was linked into the _build dir
    true = filelib:is_dir(filename:join([AppDir, "_build", "default", "lib", Name, "mibs"])).

umbrella_mib_first_test(Config) ->
    AppsDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("app1_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    AppDir = filename:join([AppsDir, "apps", Name]),

    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    MibsSrc = <<"-- SIMPLE-MIB.\n"
"-- This is just a simple MIB used for testing!\n"
"--\n"
"SIMPLE-MIB DEFINITIONS ::= BEGIN\n"
"IMPORTS\n"
"    MODULE-IDENTITY, enterprises\n"
"        FROM SNMPv2-SMI;\n"
"\n"
"ericsson MODULE-IDENTITY\n"
"    LAST-UPDATED\n"
"        \"201403060000Z\"\n"
"    ORGANIZATION\n"
"        \"rebar\"\n"
"    CONTACT-INFO\n"
"        \"rebar <rebar@example.com>\n"
"    or\n"
"    whoever is currently responsible for the SIMPLE\n"
"    enterprise MIB tree branch (enterprises.999).\"\n"
"    DESCRIPTION\n"
"        \"This very small module is made available\n"
"	for mib-compilation testing.\"\n"
"    ::= { enterprises 999 }\n"
"END\n">>,

    ok = filelib:ensure_dir(filename:join([AppDir, "mibs", "dummy"])),
    ok = file:write_file(filename:join([AppDir, "mibs", "SIMPLE-MIB.mib"]), MibsSrc),

    RebarConfig = [{mib_first_files, ["mibs/SIMPLE-MIB.mib"]}],

    rebar_test_utils:run_and_check(Config, RebarConfig, ["compile"], {ok, [{app, Name}]}),

    %% check a bin corresponding to the mib in the mibs dir exists in priv/mibs
    PrivMibsDir = filename:join([AppsDir, "_build", "default", "lib", Name, "priv", "mibs"]),
    true = filelib:is_file(filename:join([PrivMibsDir, "SIMPLE-MIB.bin"])),

    %% check a hrl corresponding to the mib in the mibs dir exists in include
    true = filelib:is_file(filename:join([AppDir, "include", "SIMPLE-MIB.hrl"])),

    %% check the mibs dir was linked into the _build dir
    true = filelib:is_dir(filename:join([AppsDir, "_build", "default", "lib", Name, "mibs"])).

only_default_transitive_deps(Config) ->
    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("app1_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    GitDeps = rebar_test_utils:expand_deps(git, [{"a", "1.0.0", []}]),
    PkgName = rebar_test_utils:create_random_name("pkg1_"),
    {SrcDeps, _} = rebar_test_utils:flat_deps(GitDeps),
    mock_git_resource:mock([{deps, SrcDeps},
                            {config, [{profiles, [{test, [{deps, [list_to_atom(PkgName)]}]}]}]}]),

    mock_pkg_resource:mock([{pkgdeps, [{{iolist_to_binary(PkgName), <<"0.1.0">>}, []}]}]),

    Deps = rebar_test_utils:top_level_deps(GitDeps),
    RConfFile = rebar_test_utils:create_config(AppDir, [{deps, Deps}]),
    {ok, RConf} = file:consult(RConfFile),

    %% Build with deps.
    rebar_test_utils:run_and_check(
        Config, RConf, ["as", "test", "compile"],
        {ok, [{app, Name}, {dep, "a", <<"1.0.0">>}, {dep_not_exist, PkgName}]}
    ).

clean_all(Config) ->
    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("app1_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    DepName = rebar_test_utils:create_random_name("dep1_"),
    PkgName = rebar_test_utils:create_random_name("pkg1_"),
    mock_git_resource:mock([]),
    mock_pkg_resource:mock([
        {pkgdeps, [{{iolist_to_binary(PkgName), iolist_to_binary(Vsn)}, []}]}
    ]),

    RConfFile = rebar_test_utils:create_config(AppDir, [{deps, [
        {list_to_atom(DepName), {git, "http://site.com/user/"++DepName++".git", {tag, Vsn}}},
        {list_to_atom(PkgName), Vsn}
    ]}]),
    {ok, RConf} = file:consult(RConfFile),

    %% Build things
    rebar_test_utils:run_and_check(
        Config, RConf, ["compile"],
        {ok, [{app, Name}, {app, DepName}, {app, PkgName}]}
    ),

    %% Clean all
    rebar_test_utils:run_and_check(Config, [], ["clean", "--all"],
                                   {ok, [{app, Name, invalid},
                                         {app, DepName, invalid},
                                         {app, PkgName, invalid}]}).

override_deps(Config) ->
    mock_git_resource:mock([{deps, [{some_dep, "0.0.1"},{other_dep, "0.0.1"}]}]),
    Deps = rebar_test_utils:expand_deps(git, [{"some_dep", "0.0.1", [{"other_dep", "0.0.1", []}]}]),
    TopDeps = rebar_test_utils:top_level_deps(Deps),

    RebarConfig = [
        {deps, TopDeps},
        {overrides, [
            {override, some_dep, [
                                 {deps, []}
                                 ]}
                    ]}
        ],
    rebar_test_utils:run_and_check(
        Config, RebarConfig, ["compile"],
        {ok, [{dep, "some_dep"},{dep_not_exist, "other_dep"}]}
    ).

profile_override_deps(Config) ->
    Deps = rebar_test_utils:expand_deps(git, [{"some_dep", "0.0.1", [{"other_dep", "0.0.1", []}]}]),
    TopDeps = rebar_test_utils:top_level_deps(Deps),
    {SrcDeps, _} = rebar_test_utils:flat_deps(Deps),
    mock_git_resource:mock([{deps, SrcDeps}]),

    RebarConfig = [
        {deps, TopDeps},
        {profiles, [{a,
                    [{overrides, [
                                {override, some_dep, [
                                                     {deps, []}
                                                     ]}
                                ]}
                    ]}
        ]}],
    rebar_test_utils:run_and_check(
        Config, RebarConfig, ["as", "a", "compile"],
        {ok, [{dep, "some_dep"},{dep_not_exist, "other_dep"}]}
    ).

profile_deps(Config) ->
    Deps = rebar_test_utils:expand_deps(git, [{"some_dep", "0.0.1", [{"other_dep", "0.0.1", []}]}]),
    TopDeps = rebar_test_utils:top_level_deps(Deps),
    {SrcDeps, _} = rebar_test_utils:flat_deps(Deps),
    mock_git_resource:mock([{deps, SrcDeps}]),

    RebarConfig = [
        {deps, TopDeps},
        {profiles, [{a, []}]}],
    rebar_test_utils:run_and_check(
        Config, RebarConfig, ["as", "a", "compile"],
        {ok, [{dep, "some_dep"},{dep, "other_dep"}]}
    ).

%% verify a deps prod profile is used
%% tested by checking prod hooks run and outputs to default profile dir for dep
%% and prod deps are installed for dep
deps_build_in_prod(Config) ->
    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("app1_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    GitDeps = rebar_test_utils:expand_deps(git, [{"asdf", "1.0.0", []}]),
    PkgName = rebar_test_utils:create_random_name("pkg1_"),
    {SrcDeps, _} = rebar_test_utils:flat_deps(GitDeps),
    mock_git_resource:mock([{deps, SrcDeps},
                            {config, [{profiles, [{prod, [{pre_hooks, [{compile, "echo whatsup > randomfile"}]},
                                                          {deps, [list_to_atom(PkgName)]}]}]}]}]),

    mock_pkg_resource:mock([{pkgdeps, [{{iolist_to_binary(PkgName), <<"0.1.0">>}, []}]}]),

    Deps = rebar_test_utils:top_level_deps(GitDeps),
    RConfFile = rebar_test_utils:create_config(AppDir, [{deps, Deps}]),
    {ok, RConf} = file:consult(RConfFile),

    %% Build with deps.
    rebar_test_utils:run_and_check(
        Config, RConf, ["compile"],
        {ok, [{app, Name}, {dep, "asdf", <<"1.0.0">>}, {dep, PkgName},
              {file, filename:join([AppDir, "_build", "default", "lib", "asdf", "randomfile"])}]}
    ).

%% verify that the proper include path is defined
%% according the erlang doc which states:
%%      If the filename File is absolute (possibly after variable substitution),
%%      the include file with that name is included. Otherwise, the specified file
%%      is searched for in the following directories, and in this order:
%%          * The current working directory
%%          * The directory where the module is being compiled
%%          * The directories given by the include option
include_file_relative_to_working_directory(Config) ->
    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("app1_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    Src = <<"-module(test).\n"
"\n"
"-include(\"include/test.hrl\").\n"
"\n"
"test() -> ?TEST_MACRO.\n"
"\n">>,
    Include = <<"-define(TEST_MACRO, test).\n">>,

    ok = filelib:ensure_dir(filename:join([AppDir, "src", "dummy"])),
    ok = file:write_file(filename:join([AppDir, "src", "test.erl"]), Src),

    ok = filelib:ensure_dir(filename:join([AppDir, "include", "dummy"])),
    ok = file:write_file(filename:join([AppDir, "include", "test.hrl"]), Include),

    RebarConfig = [],
    rebar_test_utils:run_and_check(Config, RebarConfig,
                                   ["compile"],
                                   {ok, [{app, Name}]}).

include_file_in_src(Config) ->
    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("app1_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    Src = <<"-module(test).\n"
"\n"
"-include(\"test.hrl\").\n"
"\n"
"test() -> ?TEST_MACRO.\n"
"\n">>,
    Include = <<"-define(TEST_MACRO, test).\n">>,

    ok = filelib:ensure_dir(filename:join([AppDir, "src", "dummy"])),
    ok = file:write_file(filename:join([AppDir, "src", "test.erl"]), Src),

    ok = file:write_file(filename:join([AppDir, "src", "test.hrl"]), Include),

    RebarConfig = [],
    rebar_test_utils:run_and_check(Config, RebarConfig,
                                   ["compile"],
                                   {ok, [{app, Name}]}).

%% verify that the proper include path is defined
%% according the erlang doc which states:
%%      If the filename File is absolute (possibly after variable substitution),
%%      the include file with that name is included. Otherwise, the specified file
%%      is searched for in the following directories, and in this order:
%%          * The current working directory
%%          * The directory where the module is being compiled
%%          * The directories given by the include option
%%
%% This test ensures that things keep working when additional directories
%% are used for apps, such as the test/ directory within the test profile.
include_file_relative_to_working_directory_test(Config) ->
    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("app1_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    Src = <<"-module(test).\n"
"\n"
"-include(\"include/test.hrl\").\n"
"\n"
"test() -> ?TEST_MACRO.\n"
"\n">>,
    Include = <<"-define(TEST_MACRO, test).\n">>,

    ok = filelib:ensure_dir(filename:join([AppDir, "src", "dummy"])),
    ok = filelib:ensure_dir(filename:join([AppDir, "test", "dummy"])),
    ok = file:write_file(filename:join([AppDir, "test", "test.erl"]), Src),

    ok = filelib:ensure_dir(filename:join([AppDir, "include", "dummy"])),
    ok = file:write_file(filename:join([AppDir, "include", "test.hrl"]), Include),

    RebarConfig = [],
    rebar_test_utils:run_and_check(Config, RebarConfig,
                                   ["as", "test", "compile"],
                                   {ok, [{app, Name}]}).

%% Same as `include_file_in_src/1' but using the `test/' directory
%% within the test profile.
include_file_in_src_test(Config) ->
    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("app1_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    Src = <<"-module(test).\n"
"\n"
"-include(\"test.hrl\").\n"
"\n"
"test() -> ?TEST_MACRO.\n"
"\n">>,
    Include = <<"-define(TEST_MACRO, test).\n">>,

    ok = filelib:ensure_dir(filename:join([AppDir, "src", "dummy"])),
    ok = filelib:ensure_dir(filename:join([AppDir, "test", "dummy"])),
    ok = file:write_file(filename:join([AppDir, "test", "test.erl"]), Src),

    ok = file:write_file(filename:join([AppDir, "src", "test.hrl"]), Include),

    RebarConfig = [],
    rebar_test_utils:run_and_check(Config, RebarConfig,
                                   ["as", "test", "compile"],
                                   {ok, [{app, Name}]}).

%% Same as `include_file_in_src_test/1' but using multiple top-level
%% apps as dependencies.
include_file_in_src_test_multiapp(Config) ->

    Name1 = rebar_test_utils:create_random_name("app2_"),
    Name2 = rebar_test_utils:create_random_name("app1_"),
    AppDir1 = filename:join([?config(apps, Config), "lib", Name1]),
    AppDir2 = filename:join([?config(apps, Config), "lib", Name2]),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir1, Name1, Vsn, [kernel, stdlib]),
    rebar_test_utils:create_app(AppDir2, Name2, Vsn, [kernel, stdlib]),

    Src = "-module(test).\n"
"\n"
"-include_lib(\"" ++ Name2 ++ "/include/test.hrl\").\n"
"\n"
"test() -> ?TEST_MACRO.\n"
"\n",
    Include = <<"-define(TEST_MACRO, test).\n">>,

    ok = filelib:ensure_dir(filename:join([AppDir1, "src", "dummy"])),
    ok = filelib:ensure_dir(filename:join([AppDir1, "test", "dummy"])),
    ok = filelib:ensure_dir(filename:join([AppDir2, "src", "dummy"])),
    ok = filelib:ensure_dir(filename:join([AppDir2, "include", "dummy"])),
    ok = file:write_file(filename:join([AppDir1, "test", "test.erl"]), Src),

    ok = file:write_file(filename:join([AppDir2, "include", "test.hrl"]), Include),

    RebarConfig = [],
    rebar_test_utils:run_and_check(Config, RebarConfig,
                                   ["as", "test", "compile"],
                                   {ok, [{app, Name1}]}).

%% this test sets the env var, compiles, records the file last modified timestamp,
%% recompiles and compares the file last modified timestamp to ensure it hasn't
%% changed. this test should run on 19.x+
dont_recompile_when_erl_compiler_options_env_does_not_change(Config) ->
    %% save existing env to restore after test
    ExistingEnv = os:getenv("ERL_COMPILER_OPTIONS"),

    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("erl_compiler_options_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    true = os:unsetenv("ERL_COMPILER_OPTIONS"),

    true = os:putenv("ERL_COMPILER_OPTIONS", "[{d, some_macro}]"),

    rebar_test_utils:run_and_check(Config, [], ["compile"], {ok, [{app, Name}]}),

    EbinDir = filename:join([AppDir, "_build", "default", "lib", Name, "ebin"]),

    {ok, Files} = rebar_utils:list_dir(EbinDir),
    ModTime = [filelib:last_modified(filename:join([EbinDir, F]))
               || F <- Files, filename:extension(F) == ".beam"],

    timer:sleep(1000),

    rebar_test_utils:run_and_check(Config, [], ["compile"], {ok, [{app, Name}]}),

    {ok, NewFiles} = rebar_utils:list_dir(EbinDir),
    NewModTime = [filelib:last_modified(filename:join([EbinDir, F]))
                  || F <- NewFiles, filename:extension(F) == ".beam"],

    ?assert(ModTime == NewModTime),

    %% restore existing env
    case ExistingEnv of
        false -> ok;
        _     -> os:putenv("ERL_COMPILER_OPTIONS", ExistingEnv)
    end.

%% this test compiles, records the file last modified timestamp, sets the env
%% var, recompiles and compares the file last modified timestamp to ensure it
%% has changed. this test should run on 19.x+
recompile_when_erl_compiler_options_env_changes(Config) ->
    %% save existing env to restore after test
    ExistingEnv = os:getenv("ERL_COMPILER_OPTIONS"),

    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("erl_compiler_options_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    true = os:unsetenv("ERL_COMPILER_OPTIONS"),

    rebar_test_utils:run_and_check(Config, [], ["compile"], {ok, [{app, Name}]}),

    EbinDir = filename:join([AppDir, "_build", "default", "lib", Name, "ebin"]),

    {ok, Files} = rebar_utils:list_dir(EbinDir),
    ModTime = [filelib:last_modified(filename:join([EbinDir, F]))
               || F <- Files, filename:extension(F) == ".beam"],

    timer:sleep(1000),

    true = os:putenv("ERL_COMPILER_OPTIONS", "[{d, some_macro}]"),

    rebar_test_utils:run_and_check(Config, [], ["compile"], {ok, [{app, Name}]}),

    {ok, NewFiles} = rebar_utils:list_dir(EbinDir),
    NewModTime = [filelib:last_modified(filename:join([EbinDir, F]))
                  || F <- NewFiles, filename:extension(F) == ".beam"],

    ?assert(ModTime =/= NewModTime),

    %% restore existing env
    case ExistingEnv of
        false -> ok;
        _     -> os:putenv("ERL_COMPILER_OPTIONS", ExistingEnv)
    end.

%% this test sets the env var, compiles, records the file last modified
%% timestamp, recompiles and compares the file last modified timestamp to
%% ensure it has changed. this test should run on 18.x
always_recompile_when_erl_compiler_options_set(Config) ->
    %% save existing env to restore after test
    ExistingEnv = os:getenv("ERL_COMPILER_OPTIONS"),

    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("erl_compiler_options_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    true = os:unsetenv("ERL_COMPILER_OPTIONS"),

    true = os:putenv("ERL_COMPILER_OPTIONS", "[{d, some_macro}]"),

    rebar_test_utils:run_and_check(Config, [], ["compile"], {ok, [{app, Name}]}),

    EbinDir = filename:join([AppDir, "_build", "default", "lib", Name, "ebin"]),

    {ok, Files} = rebar_utils:list_dir(EbinDir),
    ModTime = [filelib:last_modified(filename:join([EbinDir, F]))
               || F <- Files, filename:extension(F) == ".beam"],

    timer:sleep(1000),

    rebar_test_utils:run_and_check(Config, [], ["compile"], {ok, [{app, Name}]}),

    {ok, NewFiles} = rebar_utils:list_dir(EbinDir),
    NewModTime = [filelib:last_modified(filename:join([EbinDir, F]))
                  || F <- NewFiles, filename:extension(F) == ".beam"],

    ?assert(ModTime =/= NewModTime),

    %% restore existing env
    case ExistingEnv of
        false -> ok;
        _     -> os:putenv("ERL_COMPILER_OPTIONS", ExistingEnv)
    end.

recompile_when_parse_transform_inline_changes(Config) ->
    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("parse_transform_inline_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    ok = filelib:ensure_dir(filename:join([AppDir, "src", "dummy"])),

    ModSrc = <<"-module(example).\n"
               "-export([foo/2]).\n"
               "-compile([{parse_transform, example_parse_transform}]).\n"
               "foo(_, _) -> ok.">>,

    ok = file:write_file(filename:join([AppDir, "src", "example.erl"]),
                         ModSrc),

    ParseTransform = <<"-module(example_parse_transform).\n"
                       "-export([parse_transform/2]).\n"
                       "parse_transform(AST, _) -> AST.\n">>,

    ok = file:write_file(filename:join([AppDir, "src", "example_parse_transform.erl"]),
                         ParseTransform),

    rebar_test_utils:run_and_check(Config, [], ["compile"], {ok, [{app, Name}]}),

    EbinDir = filename:join([AppDir, "_build", "default", "lib", Name, "ebin"]),
    {ok, Files} = rebar_utils:list_dir(EbinDir),
    ModTime = [filelib:last_modified(filename:join([EbinDir, F]))
               || F <- Files, filename:basename(F, ".beam") == "example"],

    timer:sleep(1000),

    NewParseTransform = <<"-module(example_parse_transform).\n"
                          "-export([parse_transform/2]).\n"
                          "parse_transform(AST, _) -> identity(AST).\n"
                          "identity(AST) -> AST.\n">>,

    ok = file:write_file(filename:join([AppDir, "src", "example_parse_transform.erl"]),
                         NewParseTransform),

    rebar_test_utils:run_and_check(Config, [], ["compile"], {ok, [{app, Name}]}),

    {ok, NewFiles} = rebar_utils:list_dir(EbinDir),
    NewModTime = [filelib:last_modified(filename:join([EbinDir, F]))
                  || F <- NewFiles, filename:basename(F, ".beam") == "example"],

    ?assert(ModTime =/= NewModTime).

recompile_when_parse_transform_as_opt_changes(Config) ->
    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("parse_transform_opt_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    ok = filelib:ensure_dir(filename:join([AppDir, "src", "dummy"])),

    ModSrc = <<"-module(example).\n"
               "-export([foo/2]).\n"
               "foo(_, _) -> ok.">>,

    ok = file:write_file(filename:join([AppDir, "src", "example.erl"]),
                         ModSrc),

    ParseTransform = <<"-module(example_parse_transform).\n"
                       "-export([parse_transform/2]).\n"
                       "parse_transform(AST, _) -> AST.">>,

    ok = file:write_file(filename:join([AppDir, "src", "example_parse_transform.erl"]),
                         ParseTransform),

    RebarConfig = [{erl_opts, [{parse_transform, example_parse_transform}]}],

    rebar_test_utils:run_and_check(Config, RebarConfig, ["compile"], {ok, [{app, Name}]}),

    EbinDir = filename:join([AppDir, "_build", "default", "lib", Name, "ebin"]),
    {ok, Files} = rebar_utils:list_dir(EbinDir),
    ModTime = [filelib:last_modified(filename:join([EbinDir, F]))
               || F <- Files, filename:basename(F, ".beam") == "example"],

    timer:sleep(1000),

    NewParseTransform = <<"-module(example_parse_transform).\n"
                          "-export([parse_transform/2]).\n"
                          "parse_transform(AST, _) -> identity(AST).\n"
                          "identity(AST) -> AST.">>,

    ok = file:write_file(filename:join([AppDir, "src", "example_parse_transform.erl"]),
                         NewParseTransform),

    rebar_test_utils:run_and_check(Config, RebarConfig, ["compile"], {ok, [{app, Name}]}),

    {ok, NewFiles} = rebar_utils:list_dir(EbinDir),
    NewModTime = [filelib:last_modified(filename:join([EbinDir, F]))
                  || F <- NewFiles, filename:basename(F, ".beam") == "example"],

    ?assert(ModTime =/= NewModTime).

recursive(Config) ->
    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("app1_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),
    rebar_test_utils:write_src_file(filename:join(AppDir,src),"rec.erl"),

    rebar_test_utils:run_and_check(Config, [], ["compile"], {ok, [{app, Name}]}),

    EbinDir = filename:join([AppDir, "_build", "default", "lib", Name, "ebin"]),
    {ok, Files} = rebar_utils:list_dir(EbinDir),
    ?assert(lists:member("rec.beam",Files)).

no_recursive(Config) ->
    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("app1_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),
    rebar_test_utils:write_src_file(filename:join(AppDir,src),"rec.erl"),

    RebarConfig1 = [{erlc_compiler,[{recursive,false}]}],
    rebar_test_utils:run_and_check(Config, RebarConfig1, ["compile"],
                                   {ok, [{app, Name}]}),
    EbinDir = filename:join([AppDir, "_build", "default", "lib", Name, "ebin"]),
    {ok, Files1} = rebar_utils:list_dir(EbinDir),
    ?assert(false==lists:member("rec.beam",Files1)),

    RebarConfig2 = [{src_dirs,[{"src",[{recursive,false}]}]}],
    rebar_test_utils:run_and_check(Config, RebarConfig2, ["compile"],
                                   {ok, [{app, Name}]}),
    {ok, Files2} = rebar_utils:list_dir(EbinDir),
    ?assert(false==lists:member("rec.beam",Files2)),
    ok.

regex_filter_skip(Config) ->
    AppDir = ?config(apps, Config),
    Name = rebar_test_utils:create_random_name("regex_skip"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),
    rebar_test_utils:write_src_file(filename:join(AppDir,src),"._rec.erl"),
    Expected = filename:join([AppDir, "_build", "default", "lib", Name, "ebin","._rec.beam"]),

    RebarConfig = [],
    try
        rebar_test_utils:run_and_check(Config, RebarConfig, ["compile"],
                                             {ok, [{file, Expected}]}),
        throw(should_not_be_found)
    catch
        %% the file was not found, as desired!
        error:{assertion_failed,_} -> %% OTP =< 17
            ok;
        error:{assert,_} -> %% OTP >= 18
            ok
    end.

regex_filter_regression(Config) ->
    AppDir = ?config(apps, Config),
    Name = rebar_test_utils:create_random_name("regex_regression"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),
    rebar_test_utils:write_src_file(filename:join(AppDir,src),"r_f.erl"),
    Expected = filename:join([AppDir, "_build", "default", "lib", Name, "ebin","r_f.beam"]),
    RebarConfig = [],
    rebar_test_utils:run_and_check(Config, RebarConfig, ["compile"],
                                   {ok, [{file, Expected}]}),
    ok.

