-module(rebar_compile_SUITE).

-export([suite/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         all/0,
         build_basic_app/1,
         build_release_apps/1,
         build_checkout_apps/1,
         build_checkout_deps/1,
         build_all_srcdirs/1,
         recompile_when_opts_change/1,
         dont_recompile_when_opts_dont_change/1,
         dont_recompile_yrl_or_xrl/1]).

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

all() ->
    [build_basic_app, build_release_apps,
     build_checkout_apps, build_checkout_deps,
     build_all_srcdirs,
     recompile_when_opts_change, dont_recompile_when_opts_dont_change,
     dont_recompile_yrl_or_xrl].

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

    rebar_test_utils:run_and_check(
        Config, [], ["compile"],
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

