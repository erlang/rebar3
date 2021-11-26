-module(rebar_unlock_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

all() -> [pkgunlock, unlock, unlock_all, unlock_no_args].

init_per_testcase(pkgunlock, Config0) ->
    Config = rebar_test_utils:init_rebar_state(Config0, "pkgunlock"),
    Lockfile = filename:join(?config(apps, Config), "rebar.lock"),
    ec_file:copy(filename:join(?config(data_dir, Config), "pkg.rebar.lock"),
                 Lockfile),
    [{lockfile, Lockfile} | Config];
init_per_testcase(Case, Config0) ->
    Config = rebar_test_utils:init_rebar_state(Config0, atom_to_list(Case)),
    Lockfile = filename:join(?config(apps, Config), "rebar.lock"),
    ec_file:copy(filename:join(?config(data_dir, Config), "rebar.lock"),
                 Lockfile),
    [{lockfile, Lockfile} | Config].

end_per_testcase(_, Config) ->
    Config.

pkgunlock(Config) ->
    Locks = read_locks(Config),
    Hashes = read_hashes(Config),
    rebar_test_utils:run_and_check(Config, [], ["unlock", "fakeapp"], {ok, []}),
    Locks = read_locks(Config),
    Hashes = read_hashes(Config),
    rebar_test_utils:run_and_check(Config, [], ["unlock", "bbmustache"], {ok, []}),
    ?assertEqual(Locks -- ["bbmustache"], read_locks(Config)),
    ?assertEqual(Hashes -- ["bbmustache"], read_hashes(Config)),
    rebar_test_utils:run_and_check(Config, [], ["unlock", "cf,certifi"], {ok, []}),
    ?assertEqual(Locks -- ["bbmustache","cf","certifi"], read_locks(Config)),
    ?assertEqual(Hashes -- ["bbmustache","cf","certifi"], read_hashes(Config)),
    rebar_test_utils:run_and_check(Config, [], ["unlock", rebar_string:join(Locks,",")], {ok, []}),
    ?assertEqual({error, enoent}, read_locks(Config)),
    ?assertEqual({error, enoent}, read_hashes(Config)),
    ok.

unlock(Config) ->
    Locks = read_locks(Config),
    rebar_test_utils:run_and_check(Config, [], ["unlock", "fakeapp"], {ok, []}),
    Locks = read_locks(Config),
    {ok, State} = rebar_test_utils:run_and_check(Config, [], ["unlock", "uuid"], return),
    ?assertEqual(Locks -- ["uuid"], read_locks(Config)),
    ?assert(false =:= lists:keyfind(<<"uuid">>, 1, rebar_state:get(State, {locks, default}))),
    ?assert(false =/= lists:keyfind(<<"itc">>, 1, rebar_state:get(State, {locks, default}))),
    rebar_test_utils:run_and_check(Config, [], ["unlock", "gproc,itc"], {ok, []}),
    ?assertEqual(Locks -- ["uuid","gproc","itc"], read_locks(Config)),
    rebar_test_utils:run_and_check(Config, [], ["unlock", rebar_string:join(Locks,",")], {ok, []}),
    ?assertEqual({error, enoent}, read_locks(Config)),
    ok.

unlock_all(Config) ->
    [_|_] = read_locks(Config),
    {ok, State} = rebar_test_utils:run_and_check(Config, [], ["unlock", "--all"], return),
    ?assertEqual({error, enoent}, read_locks(Config)),
    ?assertEqual([], rebar_state:get(State, {locks, default})),
    ok.

unlock_no_args(Config) ->
    try rebar_test_utils:run_and_check(Config, [], ["unlock"], return)
    catch {error, {rebar_prv_unlock, no_arg}} ->
        ok
    end,
    ok.

read_locks(Config) ->
    case file:consult(?config(lockfile, Config)) of
        {ok, _} ->
            Locks = rebar_config:consult_lock_file(?config(lockfile, Config)),
            [binary_to_list(element(1,Lock)) || Lock <- Locks];
        Other ->
            Other
    end.

read_hashes(Config) ->
    case file:consult(?config(lockfile, Config)) of
        {ok, [{_Vsn, _Locks},Props|_]} ->
            Hashes = proplists:get_value(pkg_hash, Props, []),
            [binary_to_list(element(1,Hash)) || Hash <- Hashes];
        {ok, [{_Vsn, _Locks}]} ->
            [];
        Other ->
            Other
    end.
