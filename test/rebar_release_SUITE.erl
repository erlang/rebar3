-module(rebar_release_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() -> [release,
          dev_mode_release,
          profile_dev_mode_override_release,
          tar,
          extend_release].

init_per_testcase(Case, Config0) ->
    Config = rebar_test_utils:init_rebar_state(Config0),
    Name = rebar_test_utils:create_random_name(atom_to_list(Case)),
    AppDir = ?config(apps, Config),
    application:load(rebar),

    ok = ec_file:mkdir_p(AppDir),
    State = rebar_state:new([{base_dir, filename:join([AppDir, "_build"])}]),

    rebar_test_utils:create_app(AppDir, Name, "1.0.0", [kernel, stdlib]),
    [{name, Name}, {apps, AppDir}, {state, State} | Config].

end_per_testcase(_, Config) ->
    meck:unload(),
    Config.

release(Config) ->
    AppDir = ?config(apps, Config),
    Name = ?config(name, Config),
    Vsn = "1.0.0",
    {ok, RebarConfig} =
        file:consult(rebar_test_utils:create_config(AppDir,
                                                    [{relx, [{release, {list_to_atom(Name), Vsn},
                                                              [list_to_atom(Name)]},
                                                             {lib_dirs, [AppDir]}]}])),
    rebar_test_utils:run_and_check(
      Config, RebarConfig,
      ["release"],
      {ok, [{release, list_to_atom(Name), Vsn, false}]}
     ).

dev_mode_release(Config) ->
    AppDir = ?config(apps, Config),
    Name = ?config(name, Config),
    Vsn = "1.0.0",
    {ok, RebarConfig} =
        file:consult(rebar_test_utils:create_config(AppDir,
                                                    [{relx, [{release, {list_to_atom(Name), Vsn},
                                                              [list_to_atom(Name)]},
                                                             {lib_dirs, [AppDir]},
                                                             {dev_mode, true}]}])),
    rebar_test_utils:run_and_check(
      Config, RebarConfig,
      ["release"],
      {ok, [{release, list_to_atom(Name), Vsn, true}]}
     ).


profile_dev_mode_override_release(Config) ->
    AppDir = ?config(apps, Config),
    Name = ?config(name, Config),
    Vsn = "1.0.0",
    {ok, RebarConfig} =
        file:consult(rebar_test_utils:create_config(AppDir,
                                                    [{relx, [{release, {list_to_atom(Name), Vsn},
                                                              [list_to_atom(Name)]},
                                                             {lib_dirs, [AppDir]},
                                                             {dev_mode, true}]},
                                                     {profiles,
                                                      [{ct,
                                                        [{relx, [{dev_mode, false}]}]}]}])),
    rebar_test_utils:run_and_check(
      Config, RebarConfig,
      ["as", "ct", "release"],
      {ok, [{release, list_to_atom(Name), Vsn, false}]}
     ).


tar(Config) ->
    AppDir = ?config(apps, Config),
    Name = ?config(name, Config),
    Vsn = "1.0.0",
    {ok, RebarConfig} =
        file:consult(rebar_test_utils:create_config(AppDir,
                                                    [{relx, [{release, {list_to_atom(Name), Vsn},
                                                              [list_to_atom(Name)]},
                                                             {lib_dirs, [AppDir]}]}])),
    rebar_test_utils:run_and_check(
      Config, RebarConfig,
      ["tar"],
      {ok, [{release, list_to_atom(Name), Vsn, false}, {tar, Name, Vsn}]}
     ).

%% Test that the order of release config args is not lost. If it is extend would fail.
extend_release(Config) ->
    AppDir = ?config(apps, Config),
    Name = ?config(name, Config),
    Vsn = "1.0.0",
    {ok, RebarConfig} =
        file:consult(rebar_test_utils:create_config(AppDir,
                                                    [{relx, [{release, {list_to_atom(Name), Vsn},
                                                              [list_to_atom(Name)]},
                                                             {release, {extended, Vsn, {extend, list_to_atom(Name)}},
                                                              []},
                                                             {lib_dirs, [AppDir]}]}])),
    rebar_test_utils:run_and_check(
      Config, RebarConfig,
      ["release", "-n", "extended"],
      {ok, [{release, extended, Vsn, false}]}
     ).
