-module(rebar_dialyzer_SUITE).

-export([suite/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         all/0,
         update_base_plt/1,
         update_app_plt/1,
         build_release_plt/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").

suite() ->
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(Testcase, Config) ->
    PrivDir = ?config(priv_dir, Config),
    Prefix = ec_cnv:to_list(Testcase),
    BasePrefix = Prefix ++ "_base",
    Opts = [{plt_prefix, Prefix},
            {plt_location, PrivDir},
            {base_plt_prefix, BasePrefix},
            {base_plt_location, PrivDir},
            {base_plt_apps, [erts]}],
    Suffix = "_" ++ rebar_utils:otp_release() ++ "_plt",
    [{plt, filename:join(PrivDir, Prefix ++ Suffix)},
     {base_plt, filename:join(PrivDir, BasePrefix ++ Suffix)},
     {rebar_config, [{dialyzer, Opts}]} |
     rebar_test_utils:init_rebar_state(Config)].

all() ->
    [update_base_plt, update_app_plt, build_release_plt].

update_base_plt(Config) ->
    AppDir = ?config(apps, Config),
    RebarConfig = ?config(rebar_config, Config),
    BasePlt = ?config(base_plt, Config),
    Plt = ?config(plt, Config),

    Name = rebar_test_utils:create_random_name("app1_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [erts]),

    rebar_test_utils:run_and_check(Config, RebarConfig, ["dialyzer"],
                                   {ok, [{app, Name}]}),

    ErtsFiles = erts_files(),

    {ok, BasePltFiles} = plt_files(BasePlt),
    ?assertEqual(ErtsFiles, BasePltFiles),

    alter_plt(BasePlt),
    ok = file:delete(Plt),

    rebar_test_utils:run_and_check(Config, RebarConfig, ["dialyzer"],
                                   {ok, [{app, Name}]}),

    {ok, BasePltFiles2} = plt_files(BasePlt),
    ?assertEqual(ErtsFiles, BasePltFiles2),

    {ok, PltFiles} = plt_files(Plt),
    ?assertEqual(ErtsFiles, PltFiles).


update_app_plt(Config) ->
    AppDir = ?config(apps, Config),
    RebarConfig = ?config(rebar_config, Config),
    Plt = ?config(plt, Config),

    Name = rebar_test_utils:create_random_name("app1_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [erts]),

    rebar_test_utils:run_and_check(Config, RebarConfig, ["dialyzer"],
                                   {ok, [{app, Name}]}),

    ErtsFiles = erts_files(),

    {ok, PltFiles} = plt_files(Plt),
    ?assertEqual(ErtsFiles, PltFiles),

    alter_plt(Plt),

    rebar_test_utils:run_and_check(Config, RebarConfig, ["dialyzer"],
                                   {ok, [{app, Name}]}),

    {ok, PltFiles2} = plt_files(Plt),
    ?assertEqual(ErtsFiles, PltFiles2),

    ok = file:delete(Plt),

    rebar_test_utils:run_and_check(Config, RebarConfig, ["dialyzer"],
                                   {ok, [{app, Name}]}),

    {ok, PltFiles3} = plt_files(Plt),
    ?assertEqual(ErtsFiles, PltFiles3).

build_release_plt(Config) ->
    AppDir = ?config(apps, Config),
    RebarConfig = ?config(rebar_config, Config),
    BasePlt = ?config(base_plt, Config),
    Plt = ?config(plt, Config),

    Name1 = rebar_test_utils:create_random_name("relapp1_"),
    Vsn1 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(filename:join([AppDir,Name1]), Name1, Vsn1,
                                [erts]),
    Name2 = rebar_test_utils:create_random_name("relapp2_"),
    Vsn2 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(filename:join([AppDir,Name2]), Name2, Vsn2,
                                [erts, ec_cnv:to_atom(Name1)]),

    rebar_test_utils:run_and_check(Config, RebarConfig, ["dialyzer"],
                                   {ok, [{app, Name1}, {app, Name2}]}),

    ErtsFiles = erts_files(),

    {ok, BasePltFiles} = plt_files(BasePlt),
    ?assertEqual(ErtsFiles, BasePltFiles),

    {ok, PltFiles} = plt_files(Plt),
    ?assertEqual(ErtsFiles, PltFiles).

%% Helpers

erts_files() ->
    ErtsDir = code:lib_dir(erts, ebin),
    ErtsBeams = filelib:wildcard("*.beam", ErtsDir),
    ErtsFiles = lists:map(fun(Beam) -> filename:join(ErtsDir, Beam) end,
                          ErtsBeams),
    lists:sort(ErtsFiles).

plt_files(Plt) ->
    case dialyzer:plt_info(Plt) of
        {ok, Info} ->
            Files = proplists:get_value(files, Info),
            {ok, lists:sort(Files)};
        Other ->
            Other
    end.

alter_plt(Plt) ->
    {ok, Files} = plt_files(Plt),
    _ = dialyzer:run([{analysis_type, plt_remove},
                      {init_plt, Plt},
                      {files, [hd(Files)]}]),
    _ = dialyzer:run([{analysis_type, plt_add},
                      {init_plt, Plt},
                      {files, [code:which(dialyzer)]}]),
    ok.
