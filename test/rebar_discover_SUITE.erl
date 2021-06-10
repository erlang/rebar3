-module(rebar_discover_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [empty_app_src, bad_app_src, invalid_app_src, overwrite_extension].
     %% note: invalid .app files without a .app.src also present
     %% has rebar3 just ignoring the directory as not OTP-related.


init_per_testcase(_, Config) ->
    NewConfig = rebar_test_utils:init_rebar_state(Config, "discover_app_"),
    AppDir = ?config(apps, NewConfig),

    Name = rebar_test_utils:create_random_name("app1"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    [{app_names, [Name]}, {vsns, [Vsn]}|NewConfig].

end_per_testcase(_, Config) ->
    Config.

empty_app_src() ->
    [{doc, "when there's an empty .app.src file, exit with a good error "
           "message rather than an uncaught exception"}].
empty_app_src(Config) ->
    AppDir = ?config(apps, Config),
    [Name] = ?config(app_names, Config),
    AppSrc = filename:join([AppDir, "src", Name ++ ".app.src"]),
    ok = file:write_file(AppSrc, ""),
    ?assertEqual(
       {error, {rebar_app_discover, {cannot_read_app_file, AppSrc}}},
       rebar_test_utils:run_and_check(Config, [], ["compile"], return)
    ),
    ok.

bad_app_src() ->
    [{doc, "when there's a syntactically invalid "
           ".app.src file, exit with a good error "
           "message rather than an uncaught exception"}].
bad_app_src(Config) ->
    AppDir = ?config(apps, Config),
    [Name] = ?config(app_names, Config),
    AppSrc = filename:join([AppDir, "src", Name ++ ".app.src"]),
    ok = file:write_file(AppSrc, "bad term file :("),
    ?assertMatch(
       {error, {rebar_app_discover, {bad_term_file, AppSrc, _}}},
       rebar_test_utils:run_and_check(Config, [], ["compile"], return)
    ),
    ok.

invalid_app_src() ->
    [{doc, "when there's a syntactically valid but semantically invalid "
           ".app.src file, exit with a good error "
           "message rather than an uncaught exception"}].
invalid_app_src(Config) ->
    AppDir = ?config(apps, Config),
    [Name] = ?config(app_names, Config),
    AppSrc = filename:join([AppDir, "src", Name ++ ".app.src"]),
    ok = file:write_file(AppSrc, "{applications, name_but_no_args}."),
    ?assertEqual(
       {error, {rebar_app_discover, {cannot_read_app_file, AppSrc}}},
       rebar_test_utils:run_and_check(Config, [], ["compile"], return)
    ),
    ok.

overwrite_extension() ->
    [{doc, "when we overwrite the default extension with "
           "`[\".test.app.src\", \".app.src\"]` "
           "check that the right application resource file is used. When "
           "the order is reversed multiple_app_files should be returned."}].

overwrite_extension(Config) ->
    AppDir = ?config(apps, Config),
    [Name] = ?config(app_names, Config),
    AppSrc = filename:join([AppDir, "src", Name ++ ".app.src"]),
    TestAppSrc = filename:join([AppDir, "src", Name ++ ".test.app.src"]),
    AppSrcData =
        io_lib:format(
            "{application, ~s, [{description, \"some description\"},  "
            "{vsn, \"0.1.0\"},  {applications, [kernel,stdlib]} ]}.~n",
            [Name]
        ),
    TestAppSrcData =
        io_lib:format(
            "{application, ~s, [{description, \"some description\"},  "
            "{vsn, \"0.42.0\"},  {applications, [kernel,stdlib]} ]}.~n",
            [Name]
        ),
    ok = file:write_file(AppSrc, AppSrcData),
    ok = file:write_file(TestAppSrc, TestAppSrcData),
    {ok, AfterCompileState} =
        rebar_test_utils:run_and_check(
            Config,
            [{application_resource_extensions, [".test.app.src", ".app.src"]}],
            ["compile"],
            return
        ),
    [App] = rebar_state:project_apps(AfterCompileState),
    ?assertEqual("0.42.0", rebar_app_info:vsn(App)),
    %% reverse order now, check that both are reported as conflicting
    {error, {rebar_prv_app_discovery, {multiple_app_files, Conflicting}}} =
        rebar_test_utils:run_and_check(
            Config,
            [{application_resource_extensions, [".app.src", ".test.app.src"]}],
            ["compile"],
            return
        ),
    ?assert(lists:member(AppSrc, Conflicting)),
    ?assert(lists:member(TestAppSrc, Conflicting)).
