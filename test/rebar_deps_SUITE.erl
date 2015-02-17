%% TODO: add tests for:
%% - only part of deps fetched
%% - only part of deps locked
%% - output only shown once
%% - modification asterisk on locked file
-module(rebar_deps_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() -> [default_nodep, default_lock].

init_per_suite(Config) ->
    application:start(meck),
    Config.

end_per_suite(_Config) ->
    application:stop(meck).

init_per_testcase(Case, Config) ->
    meck:new(io, [no_link, passthrough, unstick]),
    setup_project(Case, Config).

end_per_testcase(_, Config) ->
    meck:unload(),
    Config.

config_content() ->
    [{deps, [
        {src_a, ".*", {git, "https://example.org/ferd/src_a.git", {branch, "master"}}},
        {src_b, {git, "https://example.org/ferd/src_b.git", {branch, "master"}}},
        {pkg_a, "1.0.0"}
     ]},
     {profiles,
      [{test,
        [{deps, [
            {tdep, {git, "git://example.org/ferd/tdep.git", {tag, "0.8.2"}}}
        ]}]
      }]}
    ].

setup_project(Case, Config0) ->
    Config = rebar_test_utils:init_rebar_state(
            Config0,
            atom_to_list(Case)++"_"
    ),
    AppDir = ?config(apps, Config),
    rebar_test_utils:create_app(AppDir, "A", "0.0.0", [kernel, stdlib]),
    TopDeps = proplists:get_value(deps, config_content()),
    StringDeps = [erlang:setelement(1, Dep, atom_to_list(element(1,Dep)))
                  || Dep <- TopDeps],
    RebarConf = rebar_test_utils:create_config(AppDir, [{deps, TopDeps}]),
    mock_git_resource:mock([{deps, lists:filter(fun src_dep/1, StringDeps)}]),
    mock_pkg_resource:mock([{pkgdeps,
      [{{ec_cnv:to_binary(N),
         ec_cnv:to_binary(V)},[]}
       || {N,V} <- lists:filter(fun pkg_dep/1, StringDeps)]}]),
    [{rebarconfig, RebarConf} | Config].

src_dep(Dep) ->
    case element(1, Dep) of
        "src_"++_ -> true;
        _ -> false
    end.

pkg_dep(Dep) ->
    case element(1, Dep) of
        "pkg_"++_ -> true;
        _ -> false
    end.

default_nodep(Config) ->
    {ok, RebarConfig} = file:consult(?config(rebarconfig, Config)),
    rebar_test_utils:run_and_check(
        Config, RebarConfig, ["deps"], {ok, []}
    ),
    History = meck:history(io),
    Strings = [io_lib:format(Str, Args) || {_, {io, format, [Str, Args]}, _} <- History],
    {match, _} = re:run(Strings, "src_a\\* \\(git source\\)"),
    {match, _} = re:run(Strings, "src_b\\* \\(git source\\)"),
    {match, _} = re:run(Strings, "pkg_a\\* \\(package 1.0.0\\)").

default_lock(Config) ->
    {ok, RebarConfig} = file:consult(?config(rebarconfig, Config)),
    rebar_test_utils:run_and_check(
        Config, RebarConfig, ["lock"], {ok, []}
    ),
    rebar_test_utils:run_and_check(
        Config, RebarConfig, ["deps"], {ok, []}
    ),
    History = meck:history(io),
    Strings = [io_lib:format(Str, Args) || {_, {io, format, [Str, Args]}, _} <- History],
    {match, _} = re:run(Strings, "src_a\\ \\(locked git source\\)"),
    {match, _} = re:run(Strings, "src_b\\ \\(locked git source\\)"),
    {match, _} = re:run(Strings, "pkg_a\\ \\(locked package 1.0.0\\)").
