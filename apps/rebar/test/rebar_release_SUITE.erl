-module(rebar_release_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() -> [release,
          config_file,
          dev_mode_release,
          profile_dev_mode_override_release,
          tar,
          profile_ordering_sys_config_extend,
          profile_ordering_sys_config_extend_3_tuple_merge,
          extend_release,
          user_output_dir, profile_overlays,
          profile_overlay_merge,
          overlay_vars].

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

config_file(Config) ->
    AppDir = ?config(apps, Config),
    Name = list_to_atom(?config(name, Config)),
    %% Release build fails if no relx config exists
    ?assertError({error, {relx, no_releases_in_system}},
                 rebar_test_utils:run_and_check(Config, [], ["release"], result)),
    %% Write relx.config
    RelxConfig = fun(Vsn) -> [{release, {Name, Vsn}, [Name]}, {lib_dirs, [AppDir]}] end,
    rebar_test_utils:create_config(AppDir, "relx.config", RelxConfig("1.0.0")),
    %% Release is built with relx.config (default)
    rebar_test_utils:run_and_check(Config, [], ["release"],
                                   {ok, [{release, Name, "1.0.0", false}]}),
    %% Release is built with custom.config (--config)
    rebar_test_utils:create_config(AppDir, "custom.config", RelxConfig("2.0.0")),
    rebar_test_utils:run_and_check(Config, [], ["release", "--config", "custom.config"],
                                   {ok, [{release, Name, "2.0.0", false}]}),
    %% Fail due to non-existing file
    ?assertError({error, {rebar_relx, {config_file, "no_exist.config", enoent}}},
                 rebar_test_utils:run_and_check(Config, [],
                                                ["release", "--config", "no_exist.config"], result)),
    %% Fail due to non-existing file, even with relx config in rebar.config
    ?assertError({error, {rebar_relx, {config_file, "no_exist.config", enoent}}},
                 rebar_test_utils:run_and_check(Config, [{relx, RelxConfig("3.0.0")}],
                                                ["release", "--config", "no_exist.config"], result)),
    %% rebar.config overrides relx.config if both exist
    rebar_test_utils:run_and_check(Config, [{relx, RelxConfig("4.0.0")}], ["release"],
                                   {ok, [{release, Name, "4.0.0", false}]}).

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

%% Ensure proper ordering of sys_config and extended releases in profiles
profile_ordering_sys_config_extend(Config) ->
    AppDir = ?config(apps, Config),
    Name = ?config(name, Config),
    Vsn = "1.0.0",
    TestSysConfig = filename:join(AppDir, "test.config"),
    OtherSysConfig = filename:join(AppDir, "other.config"),
    ok = file:write_file(TestSysConfig, "[]."),
    ok = file:write_file(OtherSysConfig, "[{some, content}]."),
    {ok, RebarConfig} =
        file:consult(rebar_test_utils:create_config(AppDir,
                                                    [{relx, [{release, {list_to_atom(Name), Vsn},
                                                              [list_to_atom(Name)]},
                                                             {sys_config, OtherSysConfig},
                                                             {lib_dirs, [AppDir]}]},
                                                     {profiles, [{extended,
                                                                 [{relx, [
                                                                         {sys_config, TestSysConfig}]}]}]}])),
    rebar_test_utils:run_and_check(
      Config, RebarConfig,
      ["as", "extended", "release"],
      {ok, [{release, list_to_atom(Name), Vsn, false}]}
     ),

    ReleaseDir = filename:join([AppDir, "./_build/extended/rel/", Name, "releases", Vsn]),
    {ok, [[]]} = file:consult(filename:join(ReleaseDir, "sys.config")).

%% test that tup_umerge works with tuples of different sizes
profile_ordering_sys_config_extend_3_tuple_merge(Config) ->
    AppDir = ?config(apps, Config),
    Name = ?config(name, Config),
    Vsn = "1.0.0",
    TestSysConfig = filename:join(AppDir, "test.config"),
    OtherSysConfig = filename:join(AppDir, "other.config"),
    ok = file:write_file(TestSysConfig, "[]."),
    ok = file:write_file(OtherSysConfig, "[{some, content}]."),
    {ok, RebarConfig} =
        file:consult(rebar_test_utils:create_config(AppDir,
                                                    [{relx, [{release, {list_to_atom(Name), Vsn},
                                                              [list_to_atom(Name)]},
                                                             {sys_config, OtherSysConfig},
                                                             {lib_dirs, [AppDir]}]},
                                                     {profiles, [{extended,
                                                                 [{relx, [
                                                                         {release, {extended, Vsn, {extend, list_to_atom(Name)}},
                                                                          []},
                                                                         {sys_config, TestSysConfig}]}]}]}])),

    rebar_test_utils:run_and_check(
      Config, RebarConfig,
      ["as", "extended", "release", "-n", Name],
      {ok, [{release, list_to_atom(Name), Vsn, false}]}
     ),

    ReleaseDir = filename:join([AppDir, "./_build/extended/rel/", Name, "releases", Vsn]),
    {ok, [[]]} = file:consult(filename:join(ReleaseDir, "sys.config")).

user_output_dir(Config) ->
    AppDir = ?config(apps, Config),
    Name = ?config(name, Config),
    ReleaseDir = filename:join(AppDir, "./_rel"),
    Vsn = "1.0.0",

    {ok, RebarConfig} =
        file:consult(rebar_test_utils:create_config(AppDir,
                                                    [{relx, [{release, {list_to_atom(Name), Vsn},
                                                              [list_to_atom(Name)]},
                                                             {lib_dirs, [AppDir]},
                                                             {dev_mode, true}]}])),
    rebar_test_utils:run_and_check(
      Config, RebarConfig,
      ["release", "-o", ReleaseDir],
      {ok, []}
     ),

    ?assertNotMatch([], filelib:wildcard(filename:join([ReleaseDir, Name, "releases", Vsn, "*"]))).

profile_overlays(Config) ->
    AppDir = ?config(apps, Config),
    Name = ?config(name, Config),
    Vsn = "1.0.0",
    file:write_file(filename:join(AppDir, "dev.file"), "dev.\n"),
    file:write_file(filename:join(AppDir, "prod.file"), "prod.\n"),
    file:write_file(filename:join(AppDir, "dev.vars"), "{env, \"dev\"}.\n"),
    file:write_file(filename:join(AppDir, "prod.vars"), "{env, \"prod\"}.\n"),
    {ok, RebarConfig} =
    file:consult(rebar_test_utils:create_config(AppDir,
        %% Paths are relative, but to cwd in relx, not the project root as
        %% seen by rebar3 (in non-test cases, they're the same).
        %% Work around by being explicit.
        [{relx, [{release, {list_to_atom(Name), Vsn},
                 [list_to_atom(Name)]},
                 {overlay_vars, filename:join(AppDir, "dev.vars")},
                 {overlay, [{mkdir, "randomdir"},
                            {copy, filename:join(AppDir,"./dev.file"), "profile.file"},
                            {copy, filename:join(AppDir,"./dev.file"), "{{env}}.file"},
                            {chmod, 8#00770, "profile.file"}]},
                 {lib_dirs, [AppDir]}]},
         {profiles, [{prod,
            [{relx, [
                {debug_info, keep},
                {overlay_vars, filename:join(AppDir, "prod.vars")},
                {overlay, [{mkdir, "otherrandomdir"},
                           {copy, filename:join(AppDir, "./prod.file"), "{{env}}.file"},
                           {copy, filename:join(AppDir, "./prod.file"), "profile.file"},
                           {chmod, 8#00770, "profile.file"}]}
            ]}]
         }]}
        ])),

    ReleaseDir = filename:join([AppDir, "./_build/prod/rel/", Name]),

    rebar_test_utils:run_and_check(
      Config, RebarConfig,
      ["as", "prod", "release"],
      {ok, [{release, list_to_atom(Name), Vsn, false},
            {dir, filename:join(ReleaseDir, "otherrandomdir")},
            {dir, filename:join(ReleaseDir, "randomdir")}]}
     ),
     ?assertMatch({ok,[prod]},
                  file:consult(filename:join(ReleaseDir, "profile.file"))),
     ?assertMatch({ok,[prod]},
                  file:consult(filename:join(ReleaseDir, "prod.file"))),
     ok.

profile_overlay_merge (_Config) ->
    % when profile and relx overlays both exist, the profile overlays should be
    % first, then the relx overlays, all the rest of the config should come
    % after, rebar_relx:merge_overlays/1 should do this.
    RelxOverlay = [{mkdir, "1_from_relx"}, {mkdir, "2_from_relx"}],
    ProfileOverlay = [{mkdir, "0_from_other_profile"}],
    OtherConfig = [{other1, config}, {other2, config}],

    % test with no overlays
    ?assertEqual([{overlay,[]}] ++ OtherConfig,
                 rebar_relx:merge_overlays(OtherConfig)),

    % test with relx only, just move overlays to the top
    RelxOnly = OtherConfig ++ [{overlay, RelxOverlay}],
    ?assertEqual([{overlay, RelxOverlay}]++OtherConfig,
                 rebar_relx:merge_overlays(RelxOnly)),

    % now test with a profile (profiles end up after relx overlays
    ProfilesToMerge = OtherConfig ++
                      [{overlay, RelxOverlay},
                       {overlay, ProfileOverlay}],
    ?assertEqual([{overlay, ProfileOverlay ++ RelxOverlay}] ++ OtherConfig,
                 rebar_relx:merge_overlays(ProfilesToMerge)).

overlay_vars(Config) ->
    AppDir = ?config(apps, Config),
    Name = ?config(name, Config),
    Vsn = "1.0.0",
    {ok, RebarConfig} =
        file:consult(rebar_test_utils:create_config(AppDir,
                                                    [{relx, [{release, {list_to_atom(Name), Vsn},
                                                              [list_to_atom(Name)]},
                                                             {overlay, [
                                                                {template, filename:join([AppDir, "config/app.config"]),
                                                                  "releases/{{release_version}}/sys.config"}
                                                              ]},
                                                             {overlay_vars, filename:join([AppDir, "config/vars.config"])},
                                                             {lib_dirs, [AppDir]}]}
                                                    ])),

    ok = filelib:ensure_dir(filename:join([AppDir, "config", "dummy"])),

    OverlayVars = [{var_int, 1},
                   {var_string, "\"test\""},
                   {var_bin_string, "<<\"test\">>"},
                   {var_tuple, "{t, ['atom']}"},
                   {var_list, "[a, b, c, 'd']"},
                   {var_bin, "<<23, 24, 25>>"}],
    rebar_test_utils:create_config(AppDir,
                                   filename:join([AppDir, "config", "vars.config"]),
                                   OverlayVars),

    AppConfig = [[{var_int, {{var_int}}},
                  {var_string, {{{var_string}}}},
                  {var_bin_string, {{{var_bin_string}}}},
                  {var_tuple, {{{var_tuple}}}},
                  {var_list, {{{var_list}}}},
                  {var_bin, {{{var_bin}}}},
                  {var_profile_string, {{profile_string}}}]], % this comes from `rebar3'
    rebar_test_utils:create_config(AppDir,
                                   filename:join([AppDir, "config", "app.config"]),
                                   AppConfig),

    rebar_test_utils:run_and_check(
      Config, RebarConfig,
      ["release"],
      {ok, [{release, list_to_atom(Name), Vsn, false}]}),

    %% now consult the sys.config file to make sure that is has the expected
    %% format
    ExpectedSysconfig = [{var_int, 1},
                         {var_string, "test"},
                         {var_bin_string, <<"test">>},
                         {var_tuple, {t, ['atom']}},
                         {var_list, [a, b, c, 'd']},
                         {var_bin, <<23, 24, 25>>},
                         {var_profile_string, 'default'}],
    {ok, [ExpectedSysconfig]} = file:consult(filename:join([AppDir, "_build/default/rel",
                                                          Name, "releases", Vsn, "sys.config"])).
