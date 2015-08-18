-module(rebar_plugins_SUITE).

-export([suite/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         end_per_testcase/2,
         all/0,
         compile_plugins/1,
         compile_global_plugins/1,
         complex_plugins/1,
         upgrade/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").

suite() ->
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_, Config) ->
    rebar_test_utils:init_rebar_state(Config).

end_per_testcase(_, _Config) ->
    catch meck:unload().

all() ->
    [compile_plugins, compile_global_plugins, complex_plugins, upgrade].

%% Tests that compiling a project installs and compiles the plugins of deps
compile_plugins(Config) ->
    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("app1_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    DepName = rebar_test_utils:create_random_name("dep1_"),
    PluginName = rebar_test_utils:create_random_name("plugin1_"),

    Plugins = rebar_test_utils:expand_deps(git, [{PluginName, Vsn, []}]),
    {SrcDeps, _} = rebar_test_utils:flat_deps(Plugins),
    mock_git_resource:mock([{deps, SrcDeps}]),

    mock_pkg_resource:mock([{pkgdeps, [{{list_to_binary(DepName), list_to_binary(Vsn)}, []}]},
                            {config, [{plugins, [
                                                {list_to_atom(PluginName),
                                                 {git, "http://site.com/user/"++PluginName++".git",
                                                 {tag, Vsn}}}]}]}]),

    RConfFile =
        rebar_test_utils:create_config(AppDir,
                                      [{deps, [
                                              list_to_atom(DepName)
                                              ]}]),
    {ok, RConf} = file:consult(RConfFile),

    %% Build with deps.
    rebar_test_utils:run_and_check(
        Config, RConf, ["compile"],
        {ok, [{app, Name}, {plugin, PluginName}, {dep, DepName}]}
    ).

%% Tests that compiling a project installs and compiles the global plugins
compile_global_plugins(Config) ->
    AppDir = ?config(apps, Config),
    GlobalDir = filename:join(AppDir, "global"),
    GlobalConfigDir = filename:join([GlobalDir, ".config", "rebar3"]),
    GlobalConfig = filename:join([GlobalDir, ".config", "rebar3", "rebar.config"]),

    meck:new(rebar_dir, [passthrough]),
    meck:expect(rebar_dir, global_config, fun() -> GlobalConfig end),
    meck:expect(rebar_dir, global_cache_dir, fun(_) -> GlobalDir end),

    Name = rebar_test_utils:create_random_name("app1_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    Vsn2 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    DepName = rebar_test_utils:create_random_name("dep1_"),
    PluginName = rebar_test_utils:create_random_name("plugin1_"),

    mock_git_resource:mock([{deps, [{list_to_atom(PluginName), Vsn},
                                    {list_to_atom(PluginName), Vsn2},
                                    {{iolist_to_binary(DepName), iolist_to_binary(Vsn)}, []}]}]),


    rebar_test_utils:create_config(GlobalConfigDir,
                                   [{plugins, [
                                              {list_to_atom(PluginName), {git, "http://site.com/user/"++PluginName++".git", {tag, Vsn}}}
                                              ]}]),
    RConfFile =
        rebar_test_utils:create_config(AppDir,
                                       [{deps, [
                                               {list_to_atom(DepName), {git, "http://site.com/user/"++DepName++".git", {tag, Vsn}}}
                                               ]},
                                       {plugins, [
                                                 {list_to_atom(PluginName), {git, "http://site.com/user/"++PluginName++".git", {tag, Vsn2}}}
                                                 ]}]),
    {ok, RConf} = file:consult(RConfFile),

    %% Runs global plugin install
    rebar3:init_config(),

    %% Build with deps.
    rebar_test_utils:run_and_check(
        Config, RConf, ["compile"],
        {ok, [{app, Name},
             {global_plugin, PluginName, Vsn},
             {plugin, PluginName, Vsn2},
             {dep, DepName}]}
     ),

    meck:unload(rebar_dir).

%% Tests installing of plugin with transitive deps
complex_plugins(Config) ->
    AppDir = ?config(apps, Config),

    meck:new(rebar_dir, [passthrough]),

    Name = rebar_test_utils:create_random_name("app1_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    Vsn2 = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    DepName = rebar_test_utils:create_random_name("dep1_"),
    DepName2 = rebar_test_utils:create_random_name("dep2_"),
    DepName3 = rebar_test_utils:create_random_name("dep3_"),
    PluginName = rebar_test_utils:create_random_name("plugin1_"),

    Deps = rebar_test_utils:expand_deps(git, [{PluginName, Vsn2, [{DepName2, Vsn,
                                                                  [{DepName3, Vsn, []}]}]}
                                             ,{DepName, Vsn, []}]),
    {SrcDeps, _} = rebar_test_utils:flat_deps(Deps),
    mock_git_resource:mock([{deps, SrcDeps}]),

    RConfFile =
        rebar_test_utils:create_config(AppDir,
                                       [{deps, [
                                               {list_to_atom(DepName), {git, "http://site.com/user/"++DepName++".git", {tag, Vsn}}}
                                               ]},
                                       {plugins, [
                                                 {list_to_atom(PluginName), {git, "http://site.com/user/"++PluginName++".git", {tag, Vsn2}}}
                                                 ]}]),
    {ok, RConf} = file:consult(RConfFile),

    %% Build with deps.
    rebar_test_utils:run_and_check(
        Config, RConf, ["compile"],
        {ok, [{app, Name},
              {plugin, PluginName, Vsn2},
              {plugin, DepName2},
              {plugin, DepName3},
              {dep, DepName}]}
     ),

    meck:unload(rebar_dir).

upgrade(Config) ->
    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("app1_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    PkgName = rebar_test_utils:create_random_name("pkg1_"),
    mock_git_resource:mock([]),
    mock_pkg_resource:mock([
        {pkgdeps, [{{iolist_to_binary(PkgName), <<"0.1.0">>}, []},
                   {{iolist_to_binary(PkgName), <<"0.0.1">>}, []},
                   {{iolist_to_binary(PkgName), <<"0.1.1">>}, []}]}
    ]),

    RConfFile = rebar_test_utils:create_config(AppDir, [{plugins, [list_to_atom(PkgName)]}]),
    {ok, RConf} = file:consult(RConfFile),

    %% Build with deps.
    rebar_test_utils:run_and_check(
        Config, RConf, ["compile"],
        {ok, [{app, Name}, {plugin, PkgName, <<"0.1.1">>}]}
     ),

    catch mock_pkg_resource:unmock(),
    mock_pkg_resource:mock([
        {pkgdeps, [{{iolist_to_binary(PkgName), <<"0.1.0">>}, []},
                   {{iolist_to_binary(PkgName), <<"0.0.1">>}, []},
                   {{iolist_to_binary(PkgName), <<"0.1.3">>}, []},
                   {{iolist_to_binary(PkgName), <<"0.1.1">>}, []}]},
        {upgrade, [PkgName]}
    ]),

    %% Build with deps.
    rebar_test_utils:run_and_check(
        Config, RConf, ["plugins", "upgrade", PkgName],
        {ok, [{app, Name}, {plugin, PkgName, <<"0.1.3">>}]}
     ).
