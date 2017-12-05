-module(rebar_alias_SUITE).
-compile([export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

init_per_suite(Config) -> Config.
end_per_suite(_Config) -> ok.

init_per_testcase(_, Config) ->
    rebar_test_utils:init_rebar_state(Config, "alias_").

end_per_testcase(_, _Config) ->
    ok.

all() -> [command, args, many, override_default, no_circular].
         %% namespaces: unsupported, untested.

command() ->
    [{doc, "Runs multiple regular commands as one alias"}].
command(Config) ->
    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("alias_command_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),
    RebarConfig = [{alias, [{test, [compile, unlock]}]}],

    %% compile job ran
    rebar_test_utils:run_and_check(Config, RebarConfig,
                                   ["test"], {ok, [{app, Name}]}),
    %% unlock job also ran
    Lockfile = filename:join(?config(apps, Config), "rebar.lock"),
    ?assertNot(filelib:is_file(Lockfile)),
    ok.

args() ->
    [{doc, "Runs multiple regular commands as one alias, some of "
           "which have default arguments"}].
args(Config) ->
    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("alias_args_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),
    RebarConfig = [{alias, [{test, [{eunit,"-c"}, cover]}]}],

    %% test job ran (compiled and succeeded)
    rebar_test_utils:run_and_check(Config, RebarConfig,
                                   ["test"], {ok, [{app, Name}]}),
    %% cover job also ran, meaning eunit had coverage on, otherwise
    %% the index file is not generated.
    CoverFile = filename:join([?config(apps, Config),
                               "_build", "test", "cover", "index.html"]),
    ?assert(filelib:is_file(CoverFile)),
    ok.

many() ->
    [{doc, "Multiple aliases may be registered"}].
many(Config) ->
    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("alias_args_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),
    RebarConfig = [{alias, [{test, [{eunit,"-c"}, cover]},
                            {nolock, [compile, unlock]}]}],

    %% test job ran (compiled and succeeded)
    rebar_test_utils:run_and_check(Config, RebarConfig,
                                   ["test"], {ok, [{app, Name}]}),
    rebar_test_utils:run_and_check(Config, RebarConfig,
                                   ["nolock"], {ok, [{app, Name}]}),
    %% both jobs ran (see args/1 and command/1)
    CoverFile = filename:join([?config(apps, Config),
                               "_build", "test", "cover", "index.html"]),
    ?assert(filelib:is_file(CoverFile)),
    Lockfile = filename:join(?config(apps, Config), "rebar.lock"),
    ?assertNot(filelib:is_file(Lockfile)),
    ok.

override_default() ->
    [{doc, "An alias cannot take over a default provider"}].
override_default(Config) ->
    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("alias_override_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),
    RebarConfig = [{alias, [{compile, [help]}]}],

    %% App compiles anyway
    rebar_test_utils:run_and_check(Config, RebarConfig, ["compile"],
                                   {ok, [{app, Name}]}),
    ok.

no_circular() ->
    [{doc, "An alias cannot define itself as itself"},
     {timetrap, 2000}].
no_circular(Config) ->
    AppDir = ?config(apps, Config),

    Name = rebar_test_utils:create_random_name("alias_circular_"),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),
    RebarConfig = [{alias, [{test, [help, {test,"-a"}, compile]}]}],

    %% Code does not deadlock forever and errors by not knowing
    %% the command
    rebar_test_utils:run_and_check(Config, RebarConfig, ["test"],
                {error, [$C,$o,$m,$m,$a,$n,$d,$ ,"test",$ ,$n,$o,$t,$ ,
                         $f,$o,$u,$n,$d]}),
    ok.
