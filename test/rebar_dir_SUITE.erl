-module(rebar_dir_SUITE).

-export([all/0, init_per_testcase/2, end_per_testcase/2]).

-export([default_src_dirs/1, default_extra_src_dirs/1, default_all_src_dirs/1]).
-export([src_dirs/1, alt_src_dir_nested/1, src_dirs_with_opts/1, extra_src_dirs/1, all_src_dirs/1]).
-export([src_dir_opts/1, recursive/1]).
-export([top_src_dirs/1]).
-export([profile_src_dirs/1, profile_extra_src_dirs/1, profile_all_src_dirs/1]).
-export([profile_src_dir_opts/1]).
-export([retarget_path/1, alt_base_dir_abs/1, alt_base_dir_env_variable_abs/1, alt_base_dir_rel/1]).
-export([global_cache_dir/1, default_global_cache_dir/1, overwrite_default_global_cache_dir/1]).
-export([default_global_config/1, overwrite_default_global_config/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").


all() -> [default_src_dirs, default_extra_src_dirs, default_all_src_dirs,
          src_dirs, alt_src_dir_nested, extra_src_dirs, all_src_dirs, src_dir_opts, recursive,
          profile_src_dirs, profile_extra_src_dirs, profile_all_src_dirs,
          profile_src_dir_opts, top_src_dirs,
          retarget_path, alt_base_dir_abs, alt_base_dir_env_variable_abs, alt_base_dir_rel,
          global_cache_dir, default_global_cache_dir, overwrite_default_global_cache_dir,
          default_global_config, overwrite_default_global_config].

init_per_testcase(default_global_cache_dir, Config) ->
    [{apps, AppsDir}, {checkouts, CheckoutsDir}, {state, _State} | Config] = rebar_test_utils:init_rebar_state(Config),
    NewState = rebar_state:new([{base_dir, filename:join([AppsDir, "_build"])}
                            ,{root_dir, AppsDir}]),
    [{apps, AppsDir}, {checkouts, CheckoutsDir}, {state, NewState} | Config];
init_per_testcase(overwrite_default_global_cache_dir, Config) ->
    os:putenv("REBAR_CACHE_DIR", ?config(priv_dir, Config)),
    [{apps, AppsDir}, {checkouts, CheckoutsDir}, {state, _State} | Config] = rebar_test_utils:init_rebar_state(Config),
    NewState = rebar_state:new([{base_dir, filename:join([AppsDir, "_build"])}
                            ,{root_dir, AppsDir}]),
    [{apps, AppsDir}, {checkouts, CheckoutsDir}, {state, NewState} | Config];
init_per_testcase(default_global_config, Config) ->
    [{apps, AppsDir}, {checkouts, CheckoutsDir}, {state, _State} | Config] = rebar_test_utils:init_rebar_state(Config),
    NewState = rebar_state:new([{base_dir, filename:join([AppsDir, "_build"])}
                            ,{root_dir, AppsDir}]),
    [{apps, AppsDir}, {checkouts, CheckoutsDir}, {state, NewState} | Config];
init_per_testcase(overwrite_default_global_config, Config) ->
    ConfDir = filename:join([?config(priv_dir, Config), "custom"]),
    ok = file:make_dir(ConfDir),
    os:putenv("REBAR_GLOBAL_CONFIG_DIR", ConfDir),
    [{apps, AppsDir}, {checkouts, CheckoutsDir}, {state, _State} | Config] = rebar_test_utils:init_rebar_state(Config),
    NewState = rebar_state:new([{base_dir, filename:join([AppsDir, "_build"])}
                            ,{root_dir, AppsDir}]),
    [{apps, AppsDir}, {checkouts, CheckoutsDir}, {state, NewState} | Config];
init_per_testcase(_, Config) ->
    C = rebar_test_utils:init_rebar_state(Config),
    AppDir = ?config(apps, C),

    Name1 = rebar_test_utils:create_random_name("app1_"),
    Vsn1 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(filename:join([AppDir,"apps",Name1]), Name1, Vsn1, [kernel, stdlib]),

    Name2 = rebar_test_utils:create_random_name("app2_"),
    Vsn2 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(filename:join([AppDir,"apps",Name2]), Name2, Vsn2, [kernel, stdlib]),

    [{app_one, Name1}, {app_two, Name2}] ++ C.

end_per_testcase(_, _Config) -> ok.

default_src_dirs(Config) ->
    {ok, State} = rebar_test_utils:run_and_check(Config, [], ["compile"], return),

    [] = rebar_dir:src_dirs(rebar_state:opts(State)),
    ["src"] = rebar_dir:src_dirs(rebar_state:opts(State), ["src"]).

default_extra_src_dirs(Config) ->
    {ok, State} = rebar_test_utils:run_and_check(Config, [], ["compile"], return),

    [] = rebar_dir:extra_src_dirs(rebar_state:opts(State)),
    ["src"] = rebar_dir:extra_src_dirs(rebar_state:opts(State), ["src"]).

default_all_src_dirs(Config) ->
    {ok, State} = rebar_test_utils:run_and_check(Config, [], ["compile"], return),

    [] = rebar_dir:all_src_dirs(rebar_state:opts(State)),
    ["src", "test"] = rebar_dir:all_src_dirs(rebar_state:opts(State), ["src"], ["test"]).

src_dirs(Config) ->
    RebarConfig = [{erl_opts, [{src_dirs, ["foo", "./bar", "bar", "bar/", "./bar/", "baz",
                                           "./", ".", "../", "..", "./../", "../.", ".././../"]}]}],
    {ok, State} = rebar_test_utils:run_and_check(Config, RebarConfig, ["compile"], return),

    [".", "..", "../..", "bar", "baz", "foo"] = rebar_dir:src_dirs(rebar_state:opts(State)).

alt_src_dir_nested(Config) ->
    RebarConfig = [{src_dirs, ["src", "alt/nested"]}],
    AppsDir = ?config(apps, Config),
    Name1 = ?config(app_one, Config),
    ModDir = filename:join([AppsDir, "apps", Name1, "alt", "nested"]),
    Mod = "-module(altmod). -export([main/0]). main() -> ok.",

    ec_file:mkdir_path(ModDir),
    ok = file:write_file(filename:join([ModDir, "altmod.erl"]), Mod),

    Ebin = filename:join([AppsDir, "_build", "default", "lib", Name1, "ebin", "altmod.beam"]),
    {ok, State} = rebar_test_utils:run_and_check(
           Config, RebarConfig, ["compile"],
           {ok, [{file, Ebin}]}
    ),
    ["alt/nested", "src"] = rebar_dir:src_dirs(rebar_state:opts(State)).

src_dirs_with_opts(Config) ->
    RebarConfig = [{erl_opts, [{src_dirs, ["foo", "bar", "baz"]},
                               {src_dirs, [{"foo",[{recursive,false}]}, "qux"]}]}],
    {ok, State} = rebar_test_utils:run_and_check(Config, RebarConfig, ["compile"], return),

    ["bar", "baz", "foo", "qux"] = rebar_dir:src_dirs(rebar_state:opts(State)).

extra_src_dirs(Config) ->
    RebarConfig = [{erl_opts, [{extra_src_dirs, ["foo", "bar", "baz"]}]}],
    {ok, State} = rebar_test_utils:run_and_check(Config, RebarConfig, ["compile"], return),

    ["bar", "baz", "foo"] = rebar_dir:extra_src_dirs(rebar_state:opts(State)).

all_src_dirs(Config) ->
    RebarConfig = [{erl_opts, [{src_dirs, ["foo", "bar"]}, {extra_src_dirs, ["baz", "qux"]}, {src_dirs, [{"foo", [{recursive,false}]}]}]}],
    {ok, State} = rebar_test_utils:run_and_check(Config, RebarConfig, ["compile"], return),

    ["bar", "baz", "foo", "qux"] = rebar_dir:all_src_dirs(rebar_state:opts(State)).

src_dir_opts(Config) ->
    RebarConfig =
        [{erl_opts, [{src_dirs, [{"foo",[{recursive,true}]}, "bar"]},
                     {extra_src_dirs, ["baz", {"foo", [{recursive,false}]}]},
                     {src_dirs, [{"foo", [{recursive,false}]}]}]}],
    {ok, State} = rebar_test_utils:run_and_check(Config, RebarConfig,
                                                 ["compile"], return),
    [{recursive,true}] = rebar_dir:src_dir_opts(rebar_state:opts(State), "foo"),
    [] = rebar_dir:src_dir_opts(rebar_state:opts(State), "bar"),
    [] = rebar_dir:src_dir_opts(rebar_state:opts(State), "nonexisting").

recursive(Config) ->
    RebarConfig1 =
        [{erl_opts, [{src_dirs, ["foo", "bar"]},
                     {extra_src_dirs, ["baz", {"foo", [{recursive,true}]}]},
                     {src_dirs, [{"foo", [{recursive,false}]}]}]}],
    {ok, State1} = rebar_test_utils:run_and_check(Config, RebarConfig1,
                                                 ["compile"], return),
    false = rebar_dir:recursive(rebar_state:opts(State1), "foo"),
    true = rebar_dir:recursive(rebar_state:opts(State1), "bar"),

    RebarConfig2 = [{erlc_compiler,[{recursive,false}]},
                    {erl_opts,[{src_dirs,["foo",{"bar",[{recursive,true}]}]}]}],
    {ok, State2} = rebar_test_utils:run_and_check(Config, RebarConfig2,
                                                 ["compile"], return),
    false = rebar_dir:recursive(rebar_state:opts(State2), "foo"),
    true = rebar_dir:recursive(rebar_state:opts(State2), "bar"),

    ok.

top_src_dirs(Config) ->
    %% We can get the same result out of specifying src_dirs from the config root,
    %% not just the erl_opts
    RebarConfig = [{src_dirs, ["foo", "./bar", "bar", "bar/", "./bar/", "baz",
                               "./", ".", "../", "..", "./../", "../.", ".././../"]}],
    {ok, State} = rebar_test_utils:run_and_check(Config, RebarConfig, ["compile"], return),

    [".", "..", "../..", "bar", "baz", "foo"] = rebar_dir:src_dirs(rebar_state:opts(State)).

profile_src_dirs(Config) ->
    RebarConfig = [
        {erl_opts, [{src_dirs, ["foo", "bar"]}]},
        {profiles, [
            {more, [{erl_opts, [{src_dirs, ["baz", "qux"]}]}]}
        ]}
    ],
    {ok, State} = rebar_test_utils:run_and_check(Config, RebarConfig, ["as", "more", "compile"], return),

    R = lists:sort(["foo", "bar", "baz", "qux"]),
    R = rebar_dir:src_dirs(rebar_state:opts(State)).

profile_extra_src_dirs(Config) ->
    RebarConfig = [
        {erl_opts, [{extra_src_dirs, ["foo", "bar"]}]},
        {profiles, [
            {more, [{erl_opts, [{extra_src_dirs, ["baz", "qux"]}]}]}
        ]}
    ],
    {ok, State} = rebar_test_utils:run_and_check(Config, RebarConfig, ["as", "more", "compile"], return),

    R = lists:sort(["foo", "bar", "baz", "qux"]),
    R = rebar_dir:extra_src_dirs(rebar_state:opts(State)).

profile_all_src_dirs(Config) ->
    RebarConfig = [
        {erl_opts, [{src_dirs, ["foo"]}, {extra_src_dirs, ["bar"]}]},
        {profiles, [
            {more, [{erl_opts, [{src_dirs, ["baz"]}, {extra_src_dirs, ["qux"]}]}]}
        ]}
    ],
    {ok, State} = rebar_test_utils:run_and_check(Config, RebarConfig, ["as", "more", "compile"], return),

    R = lists:sort(["foo", "bar", "baz", "qux"]),
    R = rebar_dir:all_src_dirs(rebar_state:opts(State)).

profile_src_dir_opts(Config) ->
    RebarConfig = [
        {erl_opts, [{src_dirs, ["foo"]},
                    {extra_src_dirs, [{"bar",[recursive]}]}]},
        {profiles, [
            {more, [{erl_opts, [{src_dirs, [{"bar",[{recursive,false}]}]},
                                {extra_src_dirs, ["qux"]}]}]}
        ]}
    ],
    {ok, State} = rebar_test_utils:run_and_check(Config, RebarConfig,
                                                 ["as", "more", "compile"],
                                                 return),

    [{recursive,false}] = rebar_dir:src_dir_opts(rebar_state:opts(State),"bar"),

    {ok, State1} = rebar_test_utils:run_and_check(Config, RebarConfig,
                                                 ["compile"], return),

    [{recursive,true}] = rebar_dir:src_dir_opts(rebar_state:opts(State1),"bar").

retarget_path(Config) ->
    {ok, State} = rebar_test_utils:run_and_check(Config, [], ["compile"], return),

    BaseDir = rebar_dir:base_dir(State),

    Name1 = ?config(app_one, Config),
    Name2 = ?config(app_two, Config),

    ?assertEqual(filename:join([BaseDir, "lib", Name1, "test"]),
                 rebar_dir:retarget_path(State, filename:join([rebar_dir:root_dir(State), "apps", Name1, "test"]))),
    ?assertEqual(filename:join([BaseDir, "lib", Name2, "test"]),
                 rebar_dir:retarget_path(State, filename:join([rebar_dir:root_dir(State), "apps", Name2, "test"]))),
    ?assertEqual(filename:join([BaseDir, "lib", Name1, "more_test"]),
                 rebar_dir:retarget_path(State, filename:join([rebar_dir:root_dir(State), "apps", Name1, "more_test"]))),
    ?assertEqual(filename:join([BaseDir, "test"]),
                 rebar_dir:retarget_path(State, filename:join([rebar_dir:root_dir(State), "test"]))),
    ?assertEqual(filename:join([BaseDir, "some_other_dir"]),
                 rebar_dir:retarget_path(State, filename:join([rebar_dir:root_dir(State), "some_other_dir"]))),
    ?assertEqual("/somewhere/outside/the/project",
                 rebar_dir:retarget_path(State, "/somewhere/outside/the/project")).

alt_base_dir_abs(Config) ->
    AltName = lists:flatten(io_lib:format("~p", [os:timestamp()])),
    AltBaseDir = filename:join(?config(priv_dir, Config), AltName),
    RebarConfig = [{base_dir, AltBaseDir}],
    {ok, State} = rebar_test_utils:run_and_check(Config, RebarConfig, ["compile"], return),

    BaseDir = rebar_dir:base_dir(State),
    ?assertEqual(filename:join(AltBaseDir, "default"), BaseDir),

    Name1 = ?config(app_one, Config),
    Name2 = ?config(app_two, Config),

    ?assert(filelib:is_dir(filename:join([BaseDir, "lib", Name1, "ebin"]))),
    ?assert(filelib:is_file(filename:join([BaseDir, "lib", Name1, "ebin", Name1++".app"]))),
    ?assert(filelib:is_file(filename:join([BaseDir, "lib", Name1, "ebin", Name1++".beam"]))),
    ?assert(filelib:is_dir(filename:join([BaseDir, "lib", Name2, "ebin"]))),
    ?assert(filelib:is_file(filename:join([BaseDir, "lib", Name2, "ebin", Name2++".app"]))),
    ?assert(filelib:is_file(filename:join([BaseDir, "lib", Name2, "ebin", Name2++".beam"]))).

alt_base_dir_env_variable_abs(Config) ->
    AltName = lists:flatten(io_lib:format("~p", [os:timestamp()])),
    AltBaseDir = filename:join(?config(priv_dir, Config), AltName),
    RebarConfig = [],

    true = os:putenv("REBAR_BASE_DIR", AltBaseDir),
    {ok, State} = rebar_test_utils:run_and_check(Config, RebarConfig, ["compile"], return),
    true = os:unsetenv("REBAR_BASE_DIR"),

    BaseDir = rebar_dir:base_dir(State),
    ?assertEqual(filename:join(AltBaseDir, "default"), BaseDir),

    Name1 = ?config(app_one, Config),
    Name2 = ?config(app_two, Config),

    ?assert(filelib:is_dir(filename:join([BaseDir, "lib", Name1, "ebin"]))),
    ?assert(filelib:is_file(filename:join([BaseDir, "lib", Name1, "ebin", Name1++".app"]))),
    ?assert(filelib:is_file(filename:join([BaseDir, "lib", Name1, "ebin", Name1++".beam"]))),
    ?assert(filelib:is_dir(filename:join([BaseDir, "lib", Name2, "ebin"]))),
    ?assert(filelib:is_file(filename:join([BaseDir, "lib", Name2, "ebin", Name2++".app"]))),
    ?assert(filelib:is_file(filename:join([BaseDir, "lib", Name2, "ebin", Name2++".beam"]))).

alt_base_dir_rel(Config) ->
    AltName = lists:flatten(io_lib:format("~p", [os:timestamp()])),
    AltBaseDir = filename:join("..", AltName),
    RebarConfig = [{base_dir, AltBaseDir}],
    {ok, State} = rebar_test_utils:run_and_check(Config, RebarConfig, ["compile"], return),

    BaseDir = rebar_dir:base_dir(State),

    Name1 = ?config(app_one, Config),
    Name2 = ?config(app_two, Config),

    ?assert(filelib:is_dir(filename:join([BaseDir, "lib", Name1, "ebin"]))),
    ?assert(filelib:is_file(filename:join([BaseDir, "lib", Name1, "ebin", Name1++".app"]))),
    ?assert(filelib:is_file(filename:join([BaseDir, "lib", Name1, "ebin", Name1++".beam"]))),
    ?assert(filelib:is_dir(filename:join([BaseDir, "lib", Name2, "ebin"]))),
    ?assert(filelib:is_file(filename:join([BaseDir, "lib", Name2, "ebin", Name2++".app"]))),
    ?assert(filelib:is_file(filename:join([BaseDir, "lib", Name2, "ebin", Name2++".beam"]))).

global_cache_dir(Config) ->
    RebarConfig = [{erl_opts, []}],
    {ok, State} = rebar_test_utils:run_and_check(Config, RebarConfig, ["compile"], return),
    DataDir = ?config(priv_dir, Config),
    Expected = filename:join([DataDir, "cache"]),
    ?assertEqual(Expected, rebar_dir:global_cache_dir(rebar_state:opts(State))).

default_global_cache_dir(Config) ->
    RebarConfig = [{erl_opts, []}],
    {ok, State} = rebar_test_utils:run_and_check(Config, RebarConfig, ["compile"], return),
    Expected = filename:join([rebar_dir:home_dir(), ".cache", "rebar3"]),
    ?assertEqual(Expected, rebar_dir:global_cache_dir(rebar_state:opts(State))).

overwrite_default_global_cache_dir(Config) ->
    RebarConfig = [{erl_opts, []}],
    {ok, State} = rebar_test_utils:run_and_check(Config, RebarConfig, ["compile"], return),
    Expected = ?config(priv_dir, Config),
    ?assertEqual(Expected, rebar_dir:global_cache_dir(rebar_state:opts(State))).

default_global_config(Config) ->
    RebarConfig = [{erl_opts, []}],
    {ok, State} = rebar_test_utils:run_and_check(Config, RebarConfig, ["compile"], return),
    ConfDir = ?config(priv_dir, Config),
    Expected = filename:join([ConfDir, ".config", "rebar3", "rebar.config"]),
    ?assertEqual(Expected, rebar_dir:global_config(State)).

overwrite_default_global_config(Config) ->
    RebarConfig = [{erl_opts, []}],
    {ok, State} = rebar_test_utils:run_and_check(Config, RebarConfig, ["compile"], return),
    Expected = filename:join([os:getenv("REBAR_GLOBAL_CONFIG_DIR"), ".config", "rebar3", "rebar.config"]),
    rebar_dir:global_config(State),
    ?assertEqual(Expected, rebar_dir:global_config(State)).
