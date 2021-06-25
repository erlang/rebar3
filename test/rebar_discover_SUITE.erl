-module(rebar_discover_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [empty_app_src, bad_app_src, invalid_app_src].
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
