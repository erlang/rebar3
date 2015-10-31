-module(rebar_src_dirs_SUITE).

-export([suite/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2,
         all/0,
         src_dirs_at_root/1,
         extra_src_dirs_at_root/1,
         src_dirs_in_erl_opts/1,
         extra_src_dirs_in_erl_opts/1,
         src_dirs_at_root_and_in_erl_opts/1,
         extra_src_dirs_at_root_and_in_erl_opts/1,
         build_basic_app/1,
         build_multi_apps/1,
         src_dir_takes_precedence_over_extra/1]).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_, Config) ->
    rebar_test_utils:init_rebar_state(Config).

end_per_testcase(_, _Config) -> ok.

all() ->
    [src_dirs_at_root, extra_src_dirs_at_root,
     src_dirs_in_erl_opts, extra_src_dirs_in_erl_opts,
     src_dirs_at_root_and_in_erl_opts, extra_src_dirs_at_root_and_in_erl_opts,
     build_basic_app, build_multi_apps, src_dir_takes_precedence_over_extra].

src_dirs_at_root(Config) ->
    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("app1_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    RebarConfig = [{src_dirs, ["foo", "bar", "baz"]}],

    {ok, State} = rebar_test_utils:run_and_check(Config, RebarConfig, ["compile"], return),

    ["bar", "baz", "foo"] = rebar_dir:src_dirs(rebar_state:opts(State), []).

extra_src_dirs_at_root(Config) ->
    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("app1_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    RebarConfig = [{extra_src_dirs, ["foo", "bar", "baz"]}],

    {ok, State} = rebar_test_utils:run_and_check(Config, RebarConfig, ["compile"], return),

    ["bar", "baz", "foo"] = rebar_dir:extra_src_dirs(rebar_state:opts(State), []).

src_dirs_in_erl_opts(Config) ->
    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("app1_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    RebarConfig = [{erl_opts, [{src_dirs, ["foo", "bar", "baz"]}]}],

    {ok, State} = rebar_test_utils:run_and_check(Config, RebarConfig, ["compile"], return),

    ["bar", "baz", "foo"] = rebar_dir:src_dirs(rebar_state:opts(State), []).

extra_src_dirs_in_erl_opts(Config) ->
    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("app1_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    RebarConfig = [{erl_opts, [{extra_src_dirs, ["foo", "bar", "baz"]}]}],

    {ok, State} = rebar_test_utils:run_and_check(Config, RebarConfig, ["compile"], return),

    ["bar", "baz", "foo"] = rebar_dir:extra_src_dirs(rebar_state:opts(State), []).

src_dirs_at_root_and_in_erl_opts(Config) ->
    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("app1_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    RebarConfig = [{erl_opts, [{src_dirs, ["foo", "bar"]}]}, {src_dirs, ["baz", "qux"]}],

    {ok, State} = rebar_test_utils:run_and_check(Config, RebarConfig, ["compile"], return),

    ["bar", "baz", "foo", "qux"] = rebar_dir:src_dirs(rebar_state:opts(State), []).

extra_src_dirs_at_root_and_in_erl_opts(Config) ->
    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("app1_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    RebarConfig = [{erl_opts, [{extra_src_dirs, ["foo", "bar"]}]}, {extra_src_dirs, ["baz", "qux"]}],

    {ok, State} = rebar_test_utils:run_and_check(Config, RebarConfig, ["compile"], return),

    ["bar", "baz", "foo", "qux"] = rebar_dir:extra_src_dirs(rebar_state:opts(State), []).

build_basic_app(Config) ->
    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("app1_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    Extra = filename:join([AppDir, "extra", "extra.erl"]),
    ok = filelib:ensure_dir(Extra),
    Src = io_lib:format("-module(extra).~n-export([x/0]).~nx() -> ok.", []),
    ok = ec_file:write(Extra, Src),

    RebarConfig = [{erl_opts, [{extra_src_dirs, ["extra"]}]}],

    rebar_test_utils:run_and_check(Config, RebarConfig, ["compile"], {ok, [{app, Name}]}),

    %% check that `extra.erl` was compiled to the `extra` dir
    ExtraOut = filename:join([AppDir, "_build", "default", "lib", Name, "extra"]),
    true = filelib:is_file(filename:join([ExtraOut, "extra.beam"])),

    %% check that `extra.erl` is not in the `modules` key of the app
    {ok, App} = file:consult(filename:join([AppDir,
                                            "_build",
                                            "default",
                                            "lib",
                                            Name,
                                            "ebin",
                                            Name ++ ".app"])),
    [{application, _, KVs}] = App,
    Mods = proplists:get_value(modules, KVs),
    false = lists:member(extra, Mods).

build_multi_apps(Config) ->
    AppDir = ?config(apps, Config),

    Name1 = rebar_test_utils:create_random_name("app1_"),
    Vsn1 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(filename:join([AppDir,"apps",Name1]), Name1, Vsn1, [kernel, stdlib]),
    Name2 = rebar_test_utils:create_random_name("app2_"),
    Vsn2 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(filename:join([AppDir,"apps",Name2]), Name2, Vsn2, [kernel, stdlib]),

    Extra1 = filename:join([AppDir, "apps", Name1, "extra", "extra1.erl"]),
    ok = filelib:ensure_dir(Extra1),
    Src1 = io_lib:format("-module(extra1).~n-export([x/0]).~nx() -> ok.", []),
    ok = ec_file:write(Extra1, Src1),

    Extra2 = filename:join([AppDir, "apps", Name2, "extra", "extra2.erl"]),
    ok = filelib:ensure_dir(Extra2),
    Src2 = io_lib:format("-module(extra2).~n-export([x/0]).~nx() -> ok.", []),
    ok = ec_file:write(Extra2, Src2),

    RebarConfig = [{erl_opts, [{extra_src_dirs, ["extra"]}]}],

    rebar_test_utils:run_and_check(
        Config, RebarConfig, ["compile"],
        {ok, [{app, Name1}, {app, Name2}]}
    ),

    %% check that `extraX.erl` was compiled to the `ebin` dir
    ExtraOut1 = filename:join([AppDir, "_build", "default", "lib", Name1, "extra"]),
    true = filelib:is_file(filename:join([ExtraOut1, "extra1.beam"])),

    ExtraOut2 = filename:join([AppDir, "_build", "default", "lib", Name2, "extra"]),
    true = filelib:is_file(filename:join([ExtraOut2, "extra2.beam"])),

    %% check that `extraX.erl` is not in the `modules` key of the app
    {ok, App1} = file:consult(filename:join([AppDir,
                                             "_build",
                                             "default",
                                             "lib",
                                             Name1,
                                             "ebin",
                                             Name1 ++ ".app"])),
    [{application, _, KVs1}] = App1,
    Mods1 = proplists:get_value(modules, KVs1),
    false = lists:member(extra1, Mods1),

    {ok, App2} = file:consult(filename:join([AppDir,
                                             "_build",
                                             "default",
                                             "lib",
                                             Name2,
                                             "ebin",
                                             Name2 ++ ".app"])),
    [{application, _, KVs2}] = App2,
    Mods2 = proplists:get_value(modules, KVs2),
    false = lists:member(extra2, Mods2).

src_dir_takes_precedence_over_extra(Config) ->
    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("app1_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    Extra = filename:join([AppDir, "extra", "extra.erl"]),
    ok = filelib:ensure_dir(Extra),
    Src = io_lib:format("-module(extra).~n-export([x/0]).~nx() -> ok.", []),
    ok = ec_file:write(Extra, Src),

    RebarConfig = [{erl_opts, [{src_dirs, ["src", "extra"]}, {extra_src_dirs, ["extra"]}]}],

    rebar_test_utils:run_and_check(Config, RebarConfig, ["compile"], {ok, [{app, Name}]}),

    %% check that `extra.erl` was compiled to the `extra` dir
    ExtraOut = filename:join([AppDir, "_build", "default", "lib", Name, "extra"]),
    true = filelib:is_file(filename:join([ExtraOut, "extra.beam"])),

    %% check that `extra.erl` is in the `modules` key of the app
    {ok, App} = file:consult(filename:join([AppDir,
                                            "_build",
                                            "default",
                                            "lib",
                                            Name,
                                            "ebin",
                                            Name ++ ".app"])),
    [{application, _, KVs}] = App,
    Mods = proplists:get_value(modules, KVs),
    true = lists:member(extra, Mods).
