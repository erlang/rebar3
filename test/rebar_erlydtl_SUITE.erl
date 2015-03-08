%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
-module(rebar_erlydtl_SUITE).

-export([suite/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2,
         all/0,
         compile/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").

%% ===================================================================
%% common_test callbacks
%% ===================================================================

suite() ->
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.


init_per_testcase(_, Config) ->
    UpdConfig = rebar_test_utils:init_rebar_state(Config),
    AppDir = ?config(apps, UpdConfig),

    Name = rebar_test_utils:create_random_name("erlydtlapp_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    write_dtl_file(AppDir, Name),

    RebarConfig = [{erl_opts, [debug_info]},
                   {erlydtl_opts, []}],
    [{app_name, Name},
     {rebar_config, RebarConfig} | UpdConfig].

end_per_testcase(_, _Config) ->
    ok.

all() ->
    [compile].

compile(Config) ->
    AppDir = ?config(apps, Config),
    AppName = ?config(app_name, Config),
    RebarConfig = ?config(rebar_config, Config),
    Beam = beam_file(AppDir, AppName),
    rebar_test_utils:run_and_check(
      Config, RebarConfig, ["erlydtl", "compile"],
      {ok, [{file, Beam}]}
     ).

beam_file(AppDir, AppName) ->
    filename:join([AppDir, "_build", "default", "lib",
                   AppName, "ebin", AppName++"_template_dtl.beam"]).

write_dtl_file(Dir, AppName) ->
    Erl = filename:join([Dir, "priv", "templates", AppName++"_template.dtl"]),
    ok = filelib:ensure_dir(Erl),
    ok = ec_file:write(Erl, get_body()).

get_body() ->
    ["[]"].
