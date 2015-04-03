-module(rebar_namespace_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() -> [implicit_compile, default_compile, do_compile,
          as_default_compile, as_do_compile,
          notfound, do_notfound, default_notfound, ns_notfound, ns_found,
          as_ns_invalid,
          do_ns_chain, do_ns_chain2, do_ns_noarg, do_ns_badcmd].

init_per_testcase(Case, Config0) ->
    Config = rebar_test_utils:init_rebar_state(Config0),
    AppDir = ?config(apps, Config),
    Name = rebar_test_utils:create_random_name("app1_"++atom_to_list(Case)),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),
    [{name, Name} | Config].

end_per_testcase(_, Config) ->
    Config.

implicit_compile(Config) ->
    Name = ?config(name, Config),
    rebar_test_utils:run_and_check(Config, [],
                                   ["compile"],
                                   {ok, [{app, Name}]}).

default_compile(Config) ->
    Name = ?config(name, Config),
    rebar_test_utils:run_and_check(Config, [],
                                   ["default","compile"],
                                   {ok, [{app, Name}]}).

do_compile(Config) ->
    Name = ?config(name, Config),
    rebar_test_utils:run_and_check(Config, [],
                                   ["do", "compile"],
                                   {ok, [{app, Name}]}).

as_default_compile(Config) ->
    Name = ?config(name, Config),
    rebar_test_utils:run_and_check(Config, [],
                                   ["as", "prod", "default", "compile"],
                                   {ok, [{app, Name}]}).

as_do_compile(Config) ->
    Name = ?config(name, Config),
    rebar_test_utils:run_and_check(Config, [],
                                   ["as", "prod", "do", "compile"],
                                   {ok, [{app, Name}]}).

notfound(Config) ->
    Command = ["fakecommand"],
    rebar_test_utils:run_and_check(
      Config, [], Command,
      {error, io_lib:format("Command ~p not found", [fakecommand])}
    ).

do_notfound(Config) ->
    Command = ["do", "fakecommand"],
    rebar_test_utils:run_and_check(
      Config, [], Command,
      {error, io_lib:format("Command ~p not found", [fakecommand])}
    ).

default_notfound(Config) ->
    Command = ["default", "fakecommand"],
    rebar_test_utils:run_and_check(
      Config, [], Command,
      {error, io_lib:format("Command ~p not found", [fakecommand])}
    ).

ns_notfound(Config) ->
    Command = ["ns", "fakecommand"],
    rebar_test_utils:run_and_check(
      add_fake_ns_provider(Config), [], Command,
      {error, io_lib:format("Command ~p not found in namespace ~p",
                            [fakecommand, ns])}
    ).

ns_found(Config) ->
    Command = ["ns", "fake_provider"],
    rebar_test_utils:run_and_check(
      add_fake_ns_provider(Config), [], Command,
      {ok, []}
    ).

as_ns_invalid(Config) ->
    %% The as namespace is not valid
    Command = ["as", "profile", "as", "task"],
    rebar_test_utils:run_and_check(
      add_fake_ns_provider(Config), [], Command,
      {error, "Namespace 'as' is forbidden"}
    ).

do_ns_chain(Config) ->
    %% `do` is also able to resolve namespaces on
    %% commands not found
    Command = ["do", "deps,", "ns", "fake_provider,", "deps"],
    rebar_test_utils:run_and_check(
      add_fake_ns_provider(Config), [], Command,
      {ok, []}
    ).

do_ns_chain2(Config) ->
    %% `do` is also able to resolve namespaces on
    %% commands not found
    Command = ["do", "ns", "fake_provider,", "deps,", "ns", "fake_provider"],
    rebar_test_utils:run_and_check(
      add_fake_ns_provider(Config), [], Command,
      {ok, []}
    ).

do_ns_noarg(Config) ->
    %% `do` is also able to resolve namespaces on
    %% commands not found
    Command = ["do", "ns"],
    rebar_test_utils:run_and_check(
      add_fake_ns_provider(Config), [], Command,
      {error, io_lib:format("Command ~p not found", [ns])}
    ).

do_ns_badcmd(Config) ->
    %% `do` is also able to resolve namespaces on
    %% commands not found
    Command = ["do", "ns", "badcmd"],
    rebar_test_utils:run_and_check(
      add_fake_ns_provider(Config), [], Command,
      {error, io_lib:format("Command ~p not found in namespace ~p", [badcmd, ns])}
    ).

%%% Helpers %%%
add_fake_ns_provider(Config) ->
    State = ?config(state, Config),
    State1 = rebar_state:add_provider(
      State,
      providers:create(
        [{name, fake_provider},
         {module, ?MODULE},
         {namespace, ns},
         {deps, []},
         {opts, []}]
       )
     ),
    [{state, State1} | Config].

%% callback for the test suite.
do(State) -> {ok, State}.
