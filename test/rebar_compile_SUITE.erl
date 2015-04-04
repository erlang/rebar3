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
         recompile_when_opts_change/1,
         dont_recompile_when_opts_dont_change/1,
         dont_recompile_yrl_or_xrl/1,
         deps_in_path/1,
         checkout_priority/1,
         compile_plugins/1]).

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
     build_all_srcdirs,
     recompile_when_opts_change, dont_recompile_when_opts_dont_change,
     dont_recompile_yrl_or_xrl, deps_in_path, checkout_priority, compile_plugins].

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
    rebar_test_utils:create_app(filename:join([AppDir,Name1]), Name1, Vsn1, [kernel, stdlib]),
    Name2 = rebar_test_utils:create_random_name("relapp2_"),
    Vsn2 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(filename:join([AppDir,Name2]), Name2, Vsn2, [kernel, stdlib]),

    rebar_test_utils:run_and_check(
        Config, [], ["compile"],
        {ok, [{app, Name1}, {app, Name2}]}
    ).

build_checkout_apps(Config) ->
    AppDir = ?config(apps, Config),
    CheckoutsDir = ?config(checkouts, Config),
    Name1 = rebar_test_utils:create_random_name("checkapp1_"),
    Vsn1 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(filename:join([AppDir,Name1]), Name1, Vsn1, [kernel, stdlib]),
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
    rebar_test_utils:create_app(filename:join([AppDir,Name1]), Name1, Vsn1, [kernel, stdlib]),
    Name2 = rebar_test_utils:create_random_name("checkapp2_"),
    Vsn2 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(filename:join([CheckoutsDir,Name2]), Name2, Vsn2, [kernel, stdlib]),
    rebar_test_utils:create_app(filename:join([DepsDir,Name2]), Name2, Vsn1, [kernel, stdlib]),

    Deps = [{list_to_atom(Name2), Vsn2, {git, "", ""}}],
    {ok, RebarConfig} = file:consult(rebar_test_utils:create_config(AppDir, [{deps, Deps}])),

    rebar_test_utils:run_and_check(
        Config, RebarConfig, ["compile"],
        {ok, [{app, Name1}, {checkout, Name2}]}
    ),
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
    rebar_test_utils:run_and_check(
        Config, RConf, ["compile"],
        {ok, [{app, Name}, {dep, DepName}, {dep, PkgName}]}
    ),
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
    rebar_test_utils:run_and_check(
        Config, RConf, ["compile"],
        {ok, [{app, Name}, {dep, DepName}, {dep, PkgName}]}
    ),
    %% Find src name in there
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
    rebar_test_utils:run_and_check(
        Config, RConf, ["compile"],
        {ok, [{app, Name}, {checkout, DepName}, {checkout, PkgName}]}
    ),

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

%% Tests that compiling a project installs and compiles the plugins of deps
compile_plugins(Config) ->
    AppDir = ?config(apps, Config),
    PluginsDir = filename:join([?config(base_dir, Config), "default", "plugins"]),

    Name = rebar_test_utils:create_random_name("app1_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    DepName = rebar_test_utils:create_random_name("dep1_"),
    PluginName = rebar_test_utils:create_random_name("plugin1_"),
    mock_git_resource:mock([{config, [{plugins, [
                                                {list_to_atom(PluginName), Vsn}
                                                ]}]}]),
    mock_pkg_resource:mock([
                           {pkgdeps, [{{iolist_to_binary(PluginName), iolist_to_binary(Vsn)}, []}]}
                           ]),

    RConfFile =
        rebar_test_utils:create_config(AppDir,
                                      [{deps, [
                                              {list_to_atom(DepName), {git, "http://site.com/user/"++DepName++".git", {tag, Vsn}}}
                                              ]}]),
    {ok, RConf} = file:consult(RConfFile),

    %% Build with deps.
    rebar_test_utils:run_and_check(
        Config, RConf, ["compile"],
        {ok, [{app, Name}, {plugin, PluginName}, {dep, DepName}]}
    ).
