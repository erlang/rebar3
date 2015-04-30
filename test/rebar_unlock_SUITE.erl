-module(rebar_unlock_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

all() -> [unlock, unlock_all].

init_per_testcase(Case, Config0) ->
    Config = rebar_test_utils:init_rebar_state(Config0, atom_to_list(Case)),
    Lockfile = filename:join(?config(apps, Config), "rebar.lock"),
    ec_file:copy(filename:join(?config(data_dir, Config), "rebar.lock"),
                 Lockfile),
    [{lockfile, Lockfile} | Config].

end_per_testcase(_, Config) ->
    Config.

unlock(Config) ->
    Locks = read_locks(Config),
    rebar_test_utils:run_and_check(Config, [], ["unlock", "fakeapp"], {ok, []}),
    Locks = read_locks(Config),
    rebar_test_utils:run_and_check(Config, [], ["unlock", "uuid"], {ok, []}),
    ?assertEqual(Locks -- ["uuid"], read_locks(Config)),
    rebar_test_utils:run_and_check(Config, [], ["unlock", "gproc,itc"], {ok, []}),
    ?assertEqual(Locks -- ["uuid","gproc","itc"], read_locks(Config)),
    rebar_test_utils:run_and_check(Config, [], ["unlock", string:join(Locks,",")], {ok, []}),
    ?assertEqual({error, enoent}, read_locks(Config)),
    ok.

unlock_all(Config) ->
    [_|_] = read_locks(Config),
    rebar_test_utils:run_and_check(Config, [], ["unlock"], {ok, []}),
    ?assertEqual({error, enoent}, read_locks(Config)),
    ok.

read_locks(Config) ->
    case file:consult(?config(lockfile, Config)) of
        {ok, [Locks]} -> [binary_to_list(element(1,Lock)) || Lock <- Locks];
        Other -> Other
    end.
