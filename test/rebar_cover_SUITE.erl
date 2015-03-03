-module(rebar_cover_SUITE).

-export([suite/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         all/0,
         flag_coverdata_written/1,
         config_coverdata_written/1,
         index_written/1,
         config_alt_coverdir/1,
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
     index_written,
     config_alt_coverdir,
     flag_verbose, config_verbose].

flag_coverdata_written(Config) ->
    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("cover_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    RebarConfig = [{erl_opts, [{d, some_define}]}],
    rebar_test_utils:run_and_check(Config,
                                   RebarConfig,
                                   ["eunit", "--cover"],
                                   {ok, [{app, Name}]}),

    true = filelib:is_file(filename:join(["_build", "_cover", "eunit.coverdata"])).

config_coverdata_written(Config) ->
    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("cover_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    RebarConfig = [{erl_opts, [{d, some_define}]}, {cover_enabled, true}],
    rebar_test_utils:run_and_check(Config,
                                   RebarConfig,
                                   ["eunit"],
                                   {ok, [{app, Name}]}),

    true = filelib:is_file(filename:join(["_build", "_cover", "eunit.coverdata"])).

index_written(Config) ->
    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("cover_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    RebarConfig = [{erl_opts, [{d, some_define}]}],
    rebar_test_utils:run_and_check(Config,
                                   RebarConfig,
                                   ["do", "eunit", "--cover", ",", "cover"],
                                   {ok, [{app, Name}]}),

    true = filelib:is_file(filename:join(["_build", "_cover", "index.html"])).

config_alt_coverdir(Config) ->
    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("cover_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    CoverDir = filename:join(["coverage", "goes", "here"]),

    RebarConfig = [{erl_opts, [{d, some_define}]}, {cover_data_dir, CoverDir}],
    rebar_test_utils:run_and_check(Config,
                                   RebarConfig,
                                   ["do", "eunit", "--cover", ",", "cover"],
                                   {ok, [{app, Name}]}),

    true = filelib:is_file(filename:join([CoverDir, "index.html"])).

flag_verbose(Config) ->
    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("cover_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    RebarConfig = [{erl_opts, [{d, some_define}]}],
    rebar_test_utils:run_and_check(Config,
                                   RebarConfig,
                                   ["do", "eunit", "--cover", ",", "cover", "--verbose"],
                                   {ok, [{app, Name}]}),

    true = filelib:is_file(filename:join(["_build", "_cover", "index.html"])).

config_verbose(Config) ->
    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("cover_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    RebarConfig = [{erl_opts, [{d, some_define}]}, {cover_print_enabled, true}],
    rebar_test_utils:run_and_check(Config,
                                   RebarConfig,
                                   ["do", "eunit", "--cover", ",", "cover"],
                                   {ok, [{app, Name}]}),

    true = filelib:is_file(filename:join(["_build", "_cover", "index.html"])).
