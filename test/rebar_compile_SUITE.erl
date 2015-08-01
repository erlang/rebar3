-module(rebar_compile_SUITE).

-export([suite/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2,
         all/0,
         build_basic_app/1,
         build_release_apps/1,
         build_checkout_apps/1,
         build_checkout_deps/1,
         build_all_srcdirs/1,
         recompile_when_hrl_changes/1,
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
         only_default_transitive_deps/1,
         clean_all/1]).

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
    rebar_test_utils:init_rebar_state(Config).

end_per_testcase(_, _Config) ->
    catch meck:unload().

all() ->
    [build_basic_app, build_release_apps,
     build_checkout_apps, build_checkout_deps,
     build_all_srcdirs, recompile_when_hrl_changes,
     recompile_when_opts_change, dont_recompile_when_opts_dont_change,
     dont_recompile_yrl_or_xrl, delete_beam_if_source_deleted,
     deps_in_path, checkout_priority, highest_version_of_pkg_dep,
     parse_transform_test, erl_first_files_test, mib_test, only_default_transitive_deps,
     clean_all].

build_basic_app(Config) ->
    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("app1_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    rebar_test_utils:run_and_check(Config, [], ["compile"], {ok, [{app, Name}]}).

build_release_apps(Config) ->
    AppDir = ?config(apps, Config),

    Name1 = rebar_test_utils:create_random_name("relapp1_"),
    Vsn1 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(filename:join([AppDir,"apps",Name1]), Name1, Vsn1, [kernel, stdlib]),
    Name2 = rebar_test_utils:create_random_name("relapp2_"),
    Vsn2 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(filename:join([AppDir,"apps",Name2]), Name2, Vsn2, [kernel, stdlib]),

    rebar_test_utils:run_and_check(
        Config, [], ["compile"],
        {ok, [{app, Name1}, {app, Name2}]}
    ).

build_checkout_apps(Config) ->
    AppDir = ?config(apps, Config),
    CheckoutsDir = ?config(checkouts, Config),
    Name1 = rebar_test_utils:create_random_name("checkapp1_"),
    Vsn1 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name1, Vsn1, [kernel, stdlib]),
    Name2 = rebar_test_utils:create_random_name("checkapp2_"),
    Vsn2 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(filename:join([CheckoutsDir,Name2]), Name2, Vsn2, [kernel, stdlib]),

    rebar_test_utils:run_and_check(
        Config, [], ["compile"],
        {ok, [{app, Name1}, {checkout, Name2}]}
    ).

build_checkout_deps(Config) ->
    AppDir = ?config(apps, Config),
    CheckoutsDir = ?config(checkouts, Config),
    DepsDir = filename:join([AppDir, "_build", "default", "lib"]),
    Name1 = rebar_test_utils:create_random_name("checkapp1_"),
    Vsn1 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name1, Vsn1, [kernel, stdlib]),
    Name2 = rebar_test_utils:create_random_name("checkapp2_"),
    Vsn2 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(filename:join([CheckoutsDir,Name2]), Name2, Vsn2, [kernel, stdlib]),
    rebar_test_utils:create_app(filename:join([DepsDir,Name2]), Name2, Vsn1, [kernel, stdlib]),

    Deps = [{list_to_atom(Name2), Vsn2, {git, "", ""}}],
    {ok, RebarConfig} = file:consult(rebar_test_utils:create_config(AppDir, [{deps, Deps}])),

    {ok, State} = rebar_test_utils:run_and_check(
        Config, RebarConfig, ["compile"],
        {ok, [{app, Name1}, {checkout, Name2}]}
    ),
    code:add_paths(rebar_state:code_paths(State, all_deps)),
    ok = application:load(list_to_atom(Name2)),
    Loaded = application:loaded_applications(),
    {_, _, Vsn2} = lists:keyfind(list_to_atom(Name2), 1, Loaded).

build_all_srcdirs(Config) ->
    AppDir = ?config(apps, Config),

    RebarConfig = [{erl_opts, [{src_dirs, ["src", "extra"]}]}],

    Name = rebar_test_utils:create_random_name("app1_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    ExtraSrc = <<"-module(extra_src).\n"
        "-export([ok/0]).\n"
        "ok() -> ok.\n">>,

    ok = filelib:ensure_dir(filename:join([AppDir, "extra", "dummy"])),
    ok = file:write_file(filename:join([AppDir, "extra", "extra_src.erl"]), ExtraSrc),

    rebar_test_utils:run_and_check(Config, RebarConfig, ["compile"], {ok, [{app, Name}]}),

    %% check a beam corresponding to the src in the extra src_dir exists in ebin
    EbinDir = filename:join([AppDir, "_build", "default", "lib", Name, "ebin"]),
    true = filelib:is_file(filename:join([EbinDir, "extra_src.beam"])),

    %% check the extra src_dir was linked into the _build dir
    true = filelib:is_dir(filename:join([AppDir, "_build", "default", "lib", Name, "extra"])).

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
    {ok, Files} = file:list_dir(EbinDir),
    ModTime = [filelib:last_modified(filename:join([EbinDir, F]))
               || F <- Files, filename:extension(F) == ".beam"],


    timer:sleep(1000),

    rebar_file_utils:touch(HeaderFile),

    rebar_test_utils:run_and_check(Config, [], ["compile"], {ok, [{app, Name}]}),

    {ok, NewFiles} = file:list_dir(EbinDir),
    NewModTime = [filelib:last_modified(filename:join([EbinDir, F]))
                  || F <- NewFiles, filename:extension(F) == ".beam"],

    ?assert(ModTime =/= NewModTime).

recompile_when_opts_change(Config) ->
    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("app1_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    rebar_test_utils:run_and_check(Config, [], ["compile"], {ok, [{app, Name}]}),

    EbinDir = filename:join([AppDir, "_build", "default", "lib", Name, "ebin"]),
    {ok, Files} = file:list_dir(EbinDir),
    ModTime = [filelib:last_modified(filename:join([EbinDir, F]))
               || F <- Files, filename:extension(F) == ".beam"],

    timer:sleep(1000),

    rebar_test_utils:create_config(AppDir, [{erl_opts, [{d, some_define}]}]),

    rebar_test_utils:run_and_check(Config, [], ["compile"], {ok, [{app, Name}]}),

    {ok, NewFiles} = file:list_dir(EbinDir),
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
    {ok, Files} = file:list_dir(EbinDir),
    ModTime = [filelib:last_modified(filename:join([EbinDir, F]))
               || F <- Files, filename:extension(F) == ".beam"],

    timer:sleep(1000),

    rebar_test_utils:run_and_check(Config, [], ["compile"], {ok, [{app, Name}]}),

    {ok, NewFiles} = file:list_dir(EbinDir),
    NewModTime = [filelib:last_modified(filename:join([EbinDir, F]))
                  || F <- NewFiles, filename:extension(F) == ".beam"],

    ?assert(ModTime == NewModTime).

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
    code:set_path(StartPaths),
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

    RebarConfig = [],

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

    %% check a beam corresponding to the src in the extra src_dir exists in ebin
    PrivMibsDir = filename:join([AppDir, "_build", "default", "lib", Name, "priv", "mibs"]),
    true = filelib:is_file(filename:join([PrivMibsDir, "SIMPLE-MIB.bin"])),

    %% check the extra src_dir was linked into the _build dir
    true = filelib:is_dir(filename:join([AppDir, "_build", "default", "lib", Name, "mibs"])).

only_default_transitive_deps(Config) ->
    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("app1_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    GitDeps = rebar_test_utils:expand_deps(git, [{"a", "1.0.0", []}]),
    PkgName = rebar_test_utils:create_random_name("pkg1_"),
    mock_git_resource:mock([{deps, rebar_test_utils:flat_deps(GitDeps)},
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
    rebar_test_utils:run_and_check(
        Config, RConf, ["clean", "--all"],
        {ok, [{app, Name, invalid}, {app, DepName, invalid}, {app, PkgName, invalid}]}
    ).
