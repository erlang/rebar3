-module(rebar_dialyzer_SUITE).

-export([suite/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_group/2,
         end_per_group/2,
         init_per_testcase/2,
         all/0,
         groups/0,
         empty_base_plt/1,
         empty_app_plt/1,
         empty_app_succ_typings/1,
         update_base_plt/1,
         update_app_plt/1,
         build_release_plt/1,
         plt_apps_option/1,
         exclude_and_extra/1,
         cli_args/1,
         single_app_succ_typing/1,
         extra_src_dirs/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").

suite() ->
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(empty, Config) ->
    [{base_plt_apps, []} | Config];
init_per_group(_Group, Config) ->
    [{base_plt_apps, [erts]} | Config].

end_per_group(_Group, _Config) ->
    ok.

init_per_testcase(Testcase, Config) ->
    PrivDir = ?config(priv_dir, Config),
    Prefix = ec_cnv:to_list(Testcase),
    BasePrefix = Prefix ++ "_base",
    Opts = [{plt_prefix, Prefix},
            {plt_location, PrivDir},
            {base_plt_prefix, BasePrefix},
            {base_plt_location, PrivDir},
            {base_plt_apps, ?config(base_plt_apps, Config)}],
    Suffix = "_" ++ rebar_utils:otp_release() ++ "_plt",
    [{plt, filename:join(PrivDir, Prefix ++ Suffix)},
     {base_plt, filename:join(PrivDir, BasePrefix ++ Suffix)},
     {rebar_config, [{dialyzer, Opts}]} |
     rebar_test_utils:init_rebar_state(Config)].

all() ->
    [{group, empty}, {group, build_and_check}, {group, update}].

groups() ->
    [{empty, [empty_base_plt, empty_app_plt, empty_app_succ_typings]},
     {build_and_check, [cli_args,
                        single_app_succ_typing,
                        build_release_plt,
                        plt_apps_option,
                        exclude_and_extra,
                        extra_src_dirs]},
     {update, [update_base_plt, update_app_plt]}].

empty_base_plt(Config) ->
    AppDir = ?config(apps, Config),
    RebarConfig = ?config(rebar_config, Config),
    BasePlt = ?config(base_plt, Config),
    Plt = ?config(plt, Config),

    Name = rebar_test_utils:create_random_name("app1_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [erts]),

    rebar_test_utils:run_and_check(Config, RebarConfig, ["dialyzer"],
                                   {ok, [{app, Name}]}),

    {ok, BasePltFiles} = plt_files(BasePlt),
    ?assertEqual([], BasePltFiles),

    ErtsFiles = erts_files(),
    {ok, PltFiles} = plt_files(Plt),
    ?assertEqual(ErtsFiles, PltFiles),

    ok.

empty_app_plt(Config) ->
    AppDir = ?config(apps, Config),
    RebarConfig = ?config(rebar_config, Config),
    BasePlt = ?config(base_plt, Config),
    Plt = ?config(plt, Config),

    Name = rebar_test_utils:create_random_name("app1_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, []),

    rebar_test_utils:run_and_check(Config, RebarConfig, ["dialyzer"],
                                   {ok, [{app, Name}]}),

    {ok, BasePltFiles} = plt_files(BasePlt),
    ?assertEqual([], BasePltFiles),

    {ok, PltFiles} = plt_files(Plt),
    ?assertEqual([], PltFiles),

    ok.

empty_app_succ_typings(Config) ->
    AppDir = ?config(apps, Config),
    RebarConfig = ?config(rebar_config, Config),

    Name = rebar_test_utils:create_random_name("app1_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_empty_app(AppDir, Name, Vsn, []),

    rebar_test_utils:run_and_check(Config, RebarConfig, ["dialyzer"],
                                   {ok, [{app, Name}]}),

    ok.

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
    ?assertEqual(ErtsFiles, PltFiles),

    add_missing_file(BasePlt),
    ok = file:delete(Plt),

    rebar_test_utils:run_and_check(Config, RebarConfig, ["dialyzer"],
                                   {ok, [{app, Name}]}),

    {ok, BasePltFiles3} = plt_files(BasePlt),
    ?assertEqual(ErtsFiles, BasePltFiles3).


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
    ?assertEqual(ErtsFiles, PltFiles3),

    add_missing_file(Plt),

    rebar_test_utils:run_and_check(Config, RebarConfig, ["dialyzer"],
                                   {ok, [{app, Name}]}),

    {ok, PltFiles4} = plt_files(Plt),
    ?assertEqual(ErtsFiles, PltFiles4).

build_release_plt(Config) ->
    AppDir = ?config(apps, Config),
    RebarConfig = ?config(rebar_config, Config),
    BasePlt = ?config(base_plt, Config),
    Plt = ?config(plt, Config),

    Name1 = rebar_test_utils:create_random_name("relapp1_"),
    Vsn1 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(filename:join([AppDir,"apps",Name1]), Name1, Vsn1,
                                [erts]),
    Name2 = rebar_test_utils:create_random_name("relapp2_"),
    Vsn2 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(filename:join([AppDir,"apps",Name2]), Name2, Vsn2,
                                [erts, ec_cnv:to_atom(Name1)]),

    rebar_test_utils:run_and_check(Config, RebarConfig, ["dialyzer"],
                                   {ok, [{app, Name1}, {app, Name2}]}),

    ErtsFiles = erts_files(),

    {ok, BasePltFiles} = plt_files(BasePlt),
    ?assertEqual(ErtsFiles, BasePltFiles),

    {ok, PltFiles} = plt_files(Plt),
    ?assertEqual(ErtsFiles, PltFiles).

plt_apps_option(Config) ->
    AppDir = ?config(apps, Config),
    RebarConfig = ?config(rebar_config, Config),
    Plt = ?config(plt, Config),
    State = ?config(state, Config),

    %% Create applications
    Name1 = rebar_test_utils:create_random_name("app1_"),
    Vsn1 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(filename:join([AppDir,"deps",Name1]), Name1, Vsn1,
                                []),
    App1 = ec_cnv:to_atom(Name1),

    Name2 = rebar_test_utils:create_random_name("app2_"),
    Vsn2 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(filename:join([AppDir,"deps",Name2]), Name2, Vsn2,
                                [App1]), % App2 depends on App1
    App2 = ec_cnv:to_atom(Name2),

    Name3 = rebar_test_utils:create_random_name("app3_"), % the project application
    Vsn3 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name3, Vsn3,
                                [App2]), % App3 depends on App2

    %% Dependencies settings
    State1 = rebar_state:add_resource(State, {localfs, rebar_localfs_resource}),
    Config1 = [{state, State1} | Config],
    RebarConfig1 = merge_config(
                     [{deps,
                       [
                        {App1, {localfs, filename:join([AppDir,"deps",Name1])}},
                        {App2, {localfs, filename:join([AppDir,"deps",Name2])}}
                       ]}],
                     RebarConfig),

    %% Dialyzer: plt_apps = top_level_deps (default)
    rebar_test_utils:run_and_check(Config1, RebarConfig1, ["dialyzer"],
                                   {ok, [{app, Name3}]}),

    %% NOTE: `erts` is included in `base_plt_apps`
    {ok, PltFiles1} = plt_files(Plt),
    ?assertEqual([App2, erts], get_apps_from_beam_files(PltFiles1)),

    %% Dialyzer: plt_apps = all_deps
    RebarConfig2 = merge_config([{dialyzer, [{plt_apps, all_deps}]}],
                                RebarConfig1),
    rebar_test_utils:run_and_check(Config1, RebarConfig2, ["dialyzer"],
                                   {ok, [{app, Name3}]}),

    {ok, PltFiles2} = plt_files(Plt),
    ?assertEqual([App1, App2, erts], get_apps_from_beam_files(PltFiles2)).

exclude_and_extra(Config) ->
    AppDir = ?config(apps, Config),
    RebarConfig = ?config(rebar_config, Config),
    BasePlt = ?config(base_plt, Config),
    Plt = ?config(plt, Config),

    {value, {dialyzer, Opts}, Rest} = lists:keytake(dialyzer, 1, RebarConfig),
    % Remove erts => []
    % Add erlang+zlib => [erlang, zlib],
    % Add erl_prim_loader+init => [erl_prim_loader, init, erlang, zlib]
    % Remove zlib+init => [erl_prim_loader, erlang]
    Opts2 = [{exclude_apps, [erts]},
             {base_plt_mods, [erlang, zlib]},
             {plt_extra_mods, [erl_prim_loader, init]},
             {exclude_mods, [zlib, init]} |
             Opts],
    RebarConfig2 = [{dialyzer, Opts2} | Rest],

    Name = rebar_test_utils:create_random_name("app1_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [erts]),

    rebar_test_utils:run_and_check(Config, RebarConfig2, ["dialyzer"],
                                   {ok, [{app, Name}]}),

    Erlang = code:where_is_file("erlang.beam"),
    {ok, BasePltFiles} = plt_files(BasePlt),
    ?assertEqual([Erlang], BasePltFiles),

    Pair = lists:sort([Erlang, code:where_is_file("erl_prim_loader.beam")]),
    {ok, PltFiles} = plt_files(Plt),
    ?assertEqual(Pair, PltFiles).

cli_args(Config) ->
    AppDir = ?config(apps, Config),
    [{dialyzer, Opts}] = ?config(rebar_config, Config),
    BasePlt = ?config(base_plt, Config),
    Plt = ?config(plt, Config),

    {value, {_, Prefix}, Opts1} = lists:keytake(plt_prefix, 1, Opts),
    {value, {_, BasePrefix}, Opts2} = lists:keytake(base_plt_prefix, 1, Opts1),
    {value, {_, Location}, Opts3} = lists:keytake(plt_location, 1, Opts2),
    {value, {_, BasePltLocation}, Opts4} = lists:keytake(base_plt_location, 1, Opts3),
    RebarConfig = [{dialyzer, Opts4}],

    Name1 = rebar_test_utils:create_random_name("relapp1_"),
    Vsn1 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(filename:join([AppDir,"apps",Name1]), Name1, Vsn1,
                                [erts]),
    Name2 = rebar_test_utils:create_random_name("relapp2_"),
    Vsn2 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(filename:join([AppDir,"apps",Name2]), Name2, Vsn2,
                                [erts, ec_cnv:to_atom(Name1)]),

    rebar_test_utils:run_and_check(Config, RebarConfig, ["dialyzer",
                                                         "--plt-location=" ++ Location,
                                                         "--base-plt-location=" ++ BasePltLocation,
                                                         "--plt-prefix=" ++ Prefix,
                                                         "--base-plt-prefix=" ++ BasePrefix,
                                                         "--statistics"],
                                   {ok, [{app, Name1}, {app, Name2}]}),

    ErtsFiles = erts_files(),

    {ok, BasePltFiles} = plt_files(BasePlt),
    ?assertEqual(ErtsFiles, BasePltFiles),

    {ok, PltFiles} = plt_files(Plt),
    ?assertEqual(ErtsFiles, PltFiles).

single_app_succ_typing(Config) ->
    AppDir = ?config(apps, Config),
    State = ?config(state, Config),
    Plt = ?config(plt, Config),
    RebarConfig = ?config(rebar_config, Config),
    %% test workflow:
    %% (a) build PLT containing all project apps and dependencies (no success typing yet)!
    %% (b) perform success-typing for all apps (warnings expected)
    %% (c) perform success-typing for app with and without warnings
    Name = rebar_test_utils:create_random_name("app1_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    %% contains warnings in tests
    rebar_test_utils:create_eunit_app(AppDir, Name, Vsn, []),
    %% second app, depending on first, should not produce warnings
    App1 = ec_cnv:to_atom(Name),
    %%% second app depends on first
    Name2 = rebar_test_utils:create_random_name("app2_"),
    Vsn2 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(filename:join([AppDir,"apps",Name2]), Name2, Vsn2,
        [App1]), % App2 depends on App1
    App2 = ec_cnv:to_atom(Name2),

    %% start with building all apps into PLT
    RebarConfig2 = merge_config([{dialyzer, [{plt_apps, all_apps}]}],
        RebarConfig),
    {ok, _} =
        rebar3:run(rebar_state:new(State, RebarConfig2, AppDir), ["dialyzer", "--succ-typings=false"]),
    %% verify all project apps are in PLT
    {ok, PltFiles} = plt_files(Plt),
    ?assertEqual([App1, App2, erts], get_apps_from_beam_files(PltFiles)),

    %% warnings when apps are not specified
    Command0 = ["as", "test", "dialyzer"],
    % there are few warnings for generated test (see rebar_test_utils:erl_eunit_suite_file/1)
    {error, {rebar_prv_dialyzer, {dialyzer_warnings, _}}} =
        rebar3:run(rebar_state:new(State, RebarConfig, AppDir), Command0),

    %% warnings from App
    Command1 = ["as", "test", "dialyzer", "--app=" ++ Name],
    % there are few warnings for generated test (see rebar_test_utils:erl_eunit_suite_file/1)
    {error, {rebar_prv_dialyzer, {dialyzer_warnings, _}}} =
        rebar3:run(rebar_state:new(State, RebarConfig, AppDir), Command1),

    %% no warnings from App2
    Command2 = ["as", "test", "dialyzer", "--app=" ++ Name2],
    {ok, _} =
        rebar3:run(rebar_state:new(State, RebarConfig, AppDir), Command2).

extra_src_dirs(Config) ->
    AppDir = ?config(apps, Config),
    State = ?config(state, Config),
    RebarConfig = ?config(rebar_config, Config),
    Name = rebar_test_utils:create_random_name("app1_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_eunit_app(AppDir, Name, Vsn, []),
    % `test` directory is already in `extra_src_dirs` when run test profile
    Command = ["as", "test", "dialyzer"],
    % there are few warnings for generated test (see rebar_test_utils:erl_eunit_suite_file/1)
    {error, {rebar_prv_dialyzer, {dialyzer_warnings, _}}} =
        rebar3:run(rebar_state:new(State, RebarConfig, AppDir), Command).

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

add_missing_file(Plt) ->
    Source = code:which(dialyzer),
    Dest = filename:join(filename:dirname(Plt), "dialyzer.beam"),
    {ok, _} = file:copy(Source, Dest),
    _ = try
            dialyzer:run([{analysis_type, plt_add},
                          {init_plt, Plt},
                          {files, [Dest]}])
        after
            ok = file:delete(Dest)
        end,
    ok.

-spec merge_config(Config, Config) -> Config when
      Config :: [{term(), term()}].
merge_config(NewConfig, OldConfig) ->
    dict:to_list(
      rebar_opts:merge_opts(dict:from_list(NewConfig),
                            dict:from_list(OldConfig))).

-spec get_apps_from_beam_files(string()) -> [atom()].
get_apps_from_beam_files(BeamFiles) ->
    lists:usort(
      [begin
           AppNameVsn = filename:basename(filename:dirname(filename:dirname(File))),
           [AppName | _] = rebar_string:lexemes(AppNameVsn ++ "-", "-"),
           ec_cnv:to_atom(AppName)
       end || File <- BeamFiles]).
