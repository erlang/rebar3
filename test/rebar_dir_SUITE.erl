-module(rebar_dir_SUITE).

-export([all/0, init_per_testcase/2, end_per_testcase/2]).

-export([default_src_dirs/1, default_extra_src_dirs/1, default_all_src_dirs/1]).
-export([src_dirs/1, extra_src_dirs/1, all_src_dirs/1]).
-export([profile_src_dirs/1, profile_extra_src_dirs/1, profile_all_src_dirs/1]).
-export([retarget_path/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").


all() -> [default_src_dirs, default_extra_src_dirs, default_all_src_dirs,
          src_dirs, extra_src_dirs, all_src_dirs,
          profile_src_dirs, profile_extra_src_dirs, profile_all_src_dirs,
          retarget_path].

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
    RebarConfig = [{erl_opts, [{src_dirs, ["foo", "bar", "baz"]}]}],
    {ok, State} = rebar_test_utils:run_and_check(Config, RebarConfig, ["compile"], return),

    ["foo", "bar", "baz"] = rebar_dir:src_dirs(rebar_state:opts(State)).

extra_src_dirs(Config) ->
    RebarConfig = [{erl_opts, [{extra_src_dirs, ["foo", "bar", "baz"]}]}],
    {ok, State} = rebar_test_utils:run_and_check(Config, RebarConfig, ["compile"], return),

    ["foo", "bar", "baz"] = rebar_dir:extra_src_dirs(rebar_state:opts(State)).

all_src_dirs(Config) ->
    RebarConfig = [{erl_opts, [{src_dirs, ["foo", "bar"]}, {extra_src_dirs, ["baz", "qux"]}]}],
    {ok, State} = rebar_test_utils:run_and_check(Config, RebarConfig, ["compile"], return),

    ["foo", "bar", "baz", "qux"] = rebar_dir:all_src_dirs(rebar_state:opts(State)).

profile_src_dirs(Config) ->
    RebarConfig = [
        {erl_opts, [{src_dirs, ["foo", "bar"]}]},
        {profiles, [
            {more, [{erl_opts, [{src_dirs, ["baz", "qux"]}]}]}
        ]}
    ],
    {ok, State} = rebar_test_utils:run_and_check(Config, RebarConfig, ["as", "more", "compile"], return),

    R = lists:sort(["foo", "bar", "baz", "qux"]),
    R = lists:sort(rebar_dir:src_dirs(rebar_state:opts(State))).

profile_extra_src_dirs(Config) ->
    RebarConfig = [
        {erl_opts, [{extra_src_dirs, ["foo", "bar"]}]},
        {profiles, [
            {more, [{erl_opts, [{extra_src_dirs, ["baz", "qux"]}]}]}
        ]}
    ],
    {ok, State} = rebar_test_utils:run_and_check(Config, RebarConfig, ["as", "more", "compile"], return),

    R = lists:sort(["foo", "bar", "baz", "qux"]),
    R = lists:sort(rebar_dir:extra_src_dirs(rebar_state:opts(State))).

profile_all_src_dirs(Config) ->
    RebarConfig = [
        {erl_opts, [{src_dirs, ["foo"]}, {extra_src_dirs, ["bar"]}]},
        {profiles, [
            {more, [{erl_opts, [{src_dirs, ["baz"]}, {extra_src_dirs, ["qux"]}]}]}
        ]}
    ],
    {ok, State} = rebar_test_utils:run_and_check(Config, RebarConfig, ["as", "more", "compile"], return),

    R = lists:sort(["foo", "bar", "baz", "qux"]),
    R = lists:sort(rebar_dir:all_src_dirs(rebar_state:opts(State))).

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