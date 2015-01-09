-module(rebar_eunit_SUITE).

-export([suite/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         all/0,
         test_basic_app/1]).

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
    [test_basic_app].

test_basic_app(Config) ->
    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("eunit_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),
    
    RebarConfig = [{erl_opts, [{d, some_define}]}],
    rebar_test_utils:run_and_check(Config, RebarConfig, ["eunit"], {ok, [{app, Name}]}).
