%%% @doc Plugin handling test
%%%
%%% This test checks if plugins are loaded correctly.
%%%
%%% It has three applications:
%%% <ol>
%%%   <li>fish. top-level app, has one dependency: `dependsonplugin'.
%%%       It also loads a plugin from CWD which creates
%%%       base_dir_cwd_pre.compile on pre_compile.</li>
%%%   <li>dependsonplugin, has one dependency: `testplugin' and loads
%%%       the testplugin_mod plugin.</li>
%%%   <li>testplugin. This is a plugin application which creates
%%%       plugin_pre.compile on pre_compile. It also loads a plugin from CWD
%%%       which creates dep_cwd_pre.compile on pre_compile.</li>
%%% </ol>

-module(depplugins_rt).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

files() ->
    [
     {copy, "../../rebar", "rebar"},
     {copy, "rebar.config", "rebar.config"},
     {copy, "base_dir_cwd_plugin.erl", "base_dir_cwd_plugin.erl"},
     {create, "ebin/fish.app", app(fish, [])},

     {copy, "rebar_dependsonplugin.config",
      "deps/dependsonplugin/rebar.config"},
     {create, "deps/dependsonplugin/ebin/dependsonplugin.app",
      app(dependsonplugin, [])},

     {copy, "rebar_testplugin.config", "deps/testplugin/rebar.config"},
     {copy, "testplugin_mod.erl",
      "deps/testplugin/plugins/testplugin_mod.erl"},
     {copy, "dep_cwd_plugin.erl", "deps/testplugin/dep_cwd_plugin.erl"},
     {create, "deps/testplugin/ebin/testplugin.app", app(testplugin, [])}
    ].

run(_Dir) ->
    ?assertMatch({ok, _}, retest_sh:run("./rebar compile", [])),

    ?assertEqual(true, filelib:is_regular("base_dir_cwd_pre.compile")),

    ?assertEqual(true, filelib:is_regular(
                         "deps/dependsonplugin/base_dir_cwd_pre.compile")),
    ?assertEqual(true, filelib:is_regular(
                         "deps/dependsonplugin/plugin_pre.compile")),

    ?assertEqual(true, filelib:is_regular(
                         "deps/testplugin/base_dir_cwd_pre.compile")),
    ?assertEqual(true, filelib:is_regular(
                         "deps/testplugin/dep_cwd_pre.compile")),
    ?assertEqual(true, filelib:is_regular(
                         "deps/testplugin/plugin_pre.compile")),
    ok.

%%
%% Generate the contents of a simple .app file
%%
app(Name, Modules) ->
    App = {application, Name,
           [{description, atom_to_list(Name)},
            {vsn, "1"},
            {modules, Modules},
            {registered, []},
            {applications, [kernel, stdlib]}]},
    io_lib:format("~p.\n", [App]).
