-module(rebar_cover_SUITE).

-export([suite/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         all/0,
         flag_coverdata_written/1,
         config_coverdata_written/1,
         basic_extra_src_dirs/1,
         release_extra_src_dirs/1,
         root_extra_src_dirs/1,
         index_written/1,
         flag_verbose/1,
         config_verbose/1]).

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
    rebar_test_utils:init_rebar_state(Config, "cover_").

all() ->
    [flag_coverdata_written, config_coverdata_written,
     basic_extra_src_dirs, release_extra_src_dirs,
     root_extra_src_dirs,
     index_written,
     flag_verbose, config_verbose].

flag_coverdata_written(Config) ->
    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("cover_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_eunit_app(AppDir, Name, Vsn, [kernel, stdlib]),

    RebarConfig = [{erl_opts, [{d, some_define}]}],
    rebar_test_utils:run_and_check(Config,
                                   RebarConfig,
                                   ["eunit", "--cover"],
                                   {ok, [{app, Name}]}),

    true = filelib:is_file(filename:join([AppDir, "_build", "test", "cover", "eunit.coverdata"])).

config_coverdata_written(Config) ->
    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("cover_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_eunit_app(AppDir, Name, Vsn, [kernel, stdlib]),

    RebarConfig = [{erl_opts, [{d, some_define}]}, {cover_enabled, true}],
    rebar_test_utils:run_and_check(Config,
                                   RebarConfig,
                                   ["eunit"],
                                   {ok, [{app, Name}]}),

    true = filelib:is_file(filename:join([AppDir, "_build", "test", "cover", "eunit.coverdata"])).

basic_extra_src_dirs(Config) ->
    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("cover_extra_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    ExtraSrc = io_lib:format("-module(~ts_extra).\n-export([ok/0]).\nok() -> ok.\n", [Name]),

    ok = filelib:ensure_dir(filename:join([AppDir, "extra", "dummy"])),
    ok = file:write_file(filename:join([AppDir, "extra", io_lib:format("~ts_extra.erl", [Name])]),
                         ExtraSrc),

    RebarConfig = [{erl_opts, [{d, some_define}]}, {extra_src_dirs, ["extra"]}],
    rebar_test_utils:run_and_check(Config,
                                   RebarConfig,
                                   ["eunit", "--cover"],
                                   {ok, [{app, Name}]}),

    Mod = list_to_atom(Name),
    {file, _} = cover:is_compiled(Mod),

    ExtraMod = list_to_atom(lists:flatten(io_lib:format("~ts_extra", [Name]))),
    false = cover:is_compiled(ExtraMod).

release_extra_src_dirs(Config) ->
    AppDir = ?config(apps, Config),

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

    RebarConfig = [{erl_opts, [{d, some_define}]}, {extra_src_dirs, ["extra"]}],
    rebar_test_utils:run_and_check(Config,
                                   RebarConfig,
                                   ["eunit", "--cover"],
                                   {ok, [{app, Name1}, {app, Name2}]}),

    Mod1 = list_to_atom(Name1),
    {file, _} = cover:is_compiled(Mod1),
    Mod2 = list_to_atom(Name2),
    {file, _} = cover:is_compiled(Mod2),

    ExtraMod1 = list_to_atom(lists:flatten(io_lib:format("~ts_extra", [Name1]))),
    false = cover:is_compiled(ExtraMod1),
    ExtraMod2 = list_to_atom(lists:flatten(io_lib:format("~ts_extra", [Name2]))),
    false = cover:is_compiled(ExtraMod2).

root_extra_src_dirs(Config) ->
    AppDir = ?config(apps, Config),

    Name1 = rebar_test_utils:create_random_name("relapp1_"),
    Vsn1 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(filename:join([AppDir, "apps", Name1]), Name1, Vsn1, [kernel, stdlib]),

    Name2 = rebar_test_utils:create_random_name("relapp2_"),
    Vsn2 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(filename:join([AppDir, "apps", Name2]), Name2, Vsn2, [kernel, stdlib]),

    Extra = <<"-module(extra).\n-export([ok/0]).\nok() -> ok.\n">>,

    ok = filelib:ensure_dir(filename:join([AppDir, "extra", "dummy"])),
    ok = file:write_file(filename:join([AppDir, "extra", "extra.erl"]), Extra),

    RebarConfig = [{erl_opts, [{d, some_define}]}, {extra_src_dirs, ["extra"]}],
    rebar_test_utils:run_and_check(Config,
                                   RebarConfig,
                                   ["eunit", "--cover"],
                                   {ok, [{app, Name1}, {app, Name2}]}),

    Mod1 = list_to_atom(Name1),
    {file, _} = cover:is_compiled(Mod1),
    Mod2 = list_to_atom(Name2),
    {file, _} = cover:is_compiled(Mod2),

    false = cover:is_compiled(extra).

index_written(Config) ->
    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("cover_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_eunit_app(AppDir, Name, Vsn, [kernel, stdlib]),

    RebarConfig = [{erl_opts, [{d, some_define}]}],
    rebar_test_utils:run_and_check(Config,
                                   RebarConfig,
                                   ["do", "eunit", "--cover", ",", "cover"],
                                   {ok, [{app, Name}]}),

    true = filelib:is_file(filename:join([AppDir, "_build", "test", "cover", "index.html"])).

flag_verbose(Config) ->
    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("cover_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_eunit_app(AppDir, Name, Vsn, [kernel, stdlib]),

    RebarConfig = [{erl_opts, [{d, some_define}]}],
    rebar_test_utils:run_and_check(Config,
                                   RebarConfig,
                                   ["do", "eunit", "--cover", ",", "cover", "--verbose"],
                                   {ok, [{app, Name}]}),

    true = filelib:is_file(filename:join([AppDir, "_build", "test", "cover", "index.html"])).

config_verbose(Config) ->
    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("cover_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_eunit_app(AppDir, Name, Vsn, [kernel, stdlib]),

    RebarConfig = [{erl_opts, [{d, some_define}]}, {cover_opts, [verbose]}],
    rebar_test_utils:run_and_check(Config,
                                   RebarConfig,
                                   ["do", "eunit", "--cover", ",", "cover"],
                                   {ok, [{app, Name}]}),

    true = filelib:is_file(filename:join([AppDir, "_build", "test", "cover", "index.html"])).
