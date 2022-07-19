-module(rebar_disable_app_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(MOD(Name), 
	io_lib:format("-module(~s).~n-export([x/0]).~nx() -> ok.~n", [Name])).

all() -> [disable_app].

init_per_testcase(_, Config) ->
    rebar_test_utils:init_rebar_state(Config).

end_per_testcase(_, _Config) ->
    ok.

disable_app(Config) ->
    AppDir = ?config(apps, Config),

    Name1 = create_random_app(AppDir, "app1_"),
    Name2 = create_random_app(AppDir, "app2_"),

    RebarConfig = [{excluded_apps, [list_to_atom(Name1)]}],
    %RebarConfig = [],

    rebar_test_utils:run_and_check(
      Config, RebarConfig, ["compile"],
      {ok, [{app, Name2}]}),

    App1 = filename:join([AppDir, "_build", "default", "lib", Name1, "ebin", Name1 ++ ".app"]),
    ?assertEqual(filelib:is_file(App1), false),

    App2 = filename:join([AppDir, "_build", "default", "lib", Name2, "ebin", Name2 ++ ".app"]),
    ?assertEqual(filelib:is_file(App2), true).

%%
%% Utils
%%
create_random_app(AppDir, Prefix) ->
    Name = rebar_test_utils:create_random_name(Prefix),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_empty_app(filename:join([AppDir, "apps", Name]), Name, Vsn, [kernel, stdlib]),

    ModName = rebar_test_utils:create_random_name("mod1_"),
    Mod = filename:join([AppDir, "apps", Name, "src", ModName ++ ".erl"]),
    ok = filelib:ensure_dir(Mod),
    Src = ?MOD(ModName),
    ok = ec_file:write(Mod, Src),
    Name.
