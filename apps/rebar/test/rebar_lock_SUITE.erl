%%% Most locking tests are implicit in other test suites handling
%%% dependencies.
%%% This suite is to test the compatibility layers between various
%%% versions of lockfiles.
-module(rebar_lock_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() -> [current_version,
          beta_version, future_versions_no_attrs, future_versions_attrs].

current_version(Config) ->
    %% Current version just dumps the locks as is on disk.
    LockFile = filename:join(?config(priv_dir, Config), "current_version"),
    Locks = [{<<"app1">>, {git,"some_url", {ref,"some_ref"}}, 2},
             {<<"app2">>, {git,"some_url", {ref,"some_ref"}}, 0},
             {<<"app3">>, {hg,"some_url", {ref,"some_ref"}}, 1},
             {<<"pkg1">>,{pkg,<<"name">>,<<"0.1.6">>},3},
             {<<"pkg2">>,{pkg,<<"name1">>,<<"1.1.6">>},2},
             {<<"pkg3">>,{pkg,<<"name2">>,<<"3.0.6">>},1}
            ],
    ExpandedNull = [
        {<<"app1">>, {git,"some_url", {ref,"some_ref"}}, 2},
        {<<"app2">>, {git,"some_url", {ref,"some_ref"}}, 0},
        {<<"app3">>, {hg,"some_url", {ref,"some_ref"}}, 1},
        {<<"pkg1">>,{pkg,<<"name">>,<<"0.1.6">>,undefined, undefined},3},
        {<<"pkg2">>,{pkg,<<"name1">>,<<"1.1.6">>,undefined, undefined},2},
        {<<"pkg3">>,{pkg,<<"name2">>,<<"3.0.6">>,undefined, undefined},1}
    ],
    %% Simulate a beta lockfile
    file:write_file(LockFile, io_lib:format("~p.~n", [Locks])),
    %% No properties fetched from a beta lockfile, expand locks
    %% to undefined
    ?assertEqual(ExpandedNull,
                 rebar_config:consult_lock_file(LockFile)),
    %% Adding hash data
    Hashes = [{<<"pkg1">>, <<"tarballhash">>},
              {<<"pkg3">>, <<"otherhash">>}],
    ExtHashes = [{<<"pkg1">>, <<"outer_tarballhash">>},
                 {<<"pkg3">>, <<"outer_otherhash">>}],
    ExpandedLocks = [
        {<<"app1">>, {git,"some_url", {ref,"some_ref"}}, 2},
        {<<"app2">>, {git,"some_url", {ref,"some_ref"}}, 0},
        {<<"app3">>, {hg,"some_url", {ref,"some_ref"}}, 1},
        {<<"pkg1">>,{pkg,<<"name">>,<<"0.1.6">>,<<"tarballhash">>, <<"outer_tarballhash">>},3},
        {<<"pkg2">>,{pkg,<<"name1">>,<<"1.1.6">>,undefined, undefined},2},
        {<<"pkg3">>,{pkg,<<"name2">>,<<"3.0.6">>,<<"otherhash">>, <<"outer_otherhash">>},1}
    ],
    file:write_file(LockFile,
                    io_lib:format("~p.~n~p.~n",
                                  [{"1.2.0", Locks},
                                   [{pkg_hash, Hashes}, {pkg_hash_ext, ExtHashes}]])),
    ?assertEqual(ExpandedLocks, rebar_config:consult_lock_file(LockFile)),
    %% Then check that we can reverse that
    ok = rebar_config:write_lock_file(LockFile, ExpandedLocks),
    ?assertEqual({ok, [{"1.2.0", Locks}, [{pkg_hash, Hashes}, {pkg_hash_ext, ExtHashes}]]},
                 file:consult(LockFile)).

beta_version(Config) ->
    %% Current version just dumps the locks as is on disk.
    LockFile = filename:join(?config(priv_dir, Config), "current_version"),
    Locks = [{<<"app1">>, {git,"some_url", {ref,"some_ref"}}, 2},
             {<<"app2">>, {git,"some_url", {ref,"some_ref"}}, 0},
             {<<"app3">>, {hg,"some_url", {ref,"some_ref"}}, 1},
             {<<"pkg1">>,{pkg,<<"name">>,<<"0.1.6">>},3}],
    ExpandedLocks = [
        {<<"app1">>, {git,"some_url", {ref,"some_ref"}}, 2},
        {<<"app2">>, {git,"some_url", {ref,"some_ref"}}, 0},
        {<<"app3">>, {hg,"some_url", {ref,"some_ref"}}, 1},
        {<<"pkg1">>,{pkg,<<"name">>,<<"0.1.6">>,undefined, undefined},3}
    ],
    file:write_file(LockFile, io_lib:format("~p.~n", [Locks])),
    ?assertEqual(ExpandedLocks, rebar_config:consult_lock_file(LockFile)).

future_versions_no_attrs(Config) ->
    %% Future versions will keep the same core attribute in there, but
    %% will do so under a new format bundled with a version and potentially
    %% some trailing attributes
    LockFile = filename:join(?config(priv_dir, Config), "future_versions"),
    Locks = [{<<"app1">>, {git,"some_url", {ref,"some_ref"}}, 2},
             {<<"app2">>, {git,"some_url", {ref,"some_ref"}}, 0},
             {<<"app3">>, {hg,"some_url", {ref,"some_ref"}}, 1},
             {<<"pkg1">>, {pkg,<<"name">>,<<"0.1.6">>},3}],
    ExpandedLocks = [{<<"app1">>, {git,"some_url", {ref,"some_ref"}}, 2},
                     {<<"app2">>, {git,"some_url", {ref,"some_ref"}}, 0},
                     {<<"app3">>, {hg,"some_url", {ref,"some_ref"}}, 1},
                     {<<"pkg1">>, {pkg,<<"name">>,<<"0.1.6">>,undefined, undefined},3}],
    LockData = {"3.5.2", Locks},
    file:write_file(LockFile, io_lib:format("~p.~n", [LockData])),
    ?assertEqual(ExpandedLocks, rebar_config:consult_lock_file(LockFile)).

future_versions_attrs(Config) ->
    %% Future versions will keep the same core attribute in there, but
    %% will do so under a new format bundled with a version and potentially
    %% some trailing attributes
    LockFile = filename:join(?config(priv_dir, Config), "future_versions"),
    Locks = [{<<"app1">>, {git,"some_url", {ref,"some_ref"}}, 2},
             {<<"app2">>, {git,"some_url", {ref,"some_ref"}}, 0},
             {<<"app3">>, {hg,"some_url", {ref,"some_ref"}}, 1},
             {<<"pkg1">>,{pkg,<<"name">>,<<"0.1.6">>},3}],
    ExpandedLocks = [{<<"app1">>, {git,"some_url", {ref,"some_ref"}}, 2},
                     {<<"app2">>, {git,"some_url", {ref,"some_ref"}}, 0},
                     {<<"app3">>, {hg,"some_url", {ref,"some_ref"}}, 1},
                     {<<"pkg1">>,{pkg,<<"name">>,<<"0.1.6">>, <<"tarballhash">>, <<"outer_tarballhash">>},3}],
    Hashes = [{<<"pkg1">>, <<"tarballhash">>}],
    ExtHashes = [{<<"pkg1">>, <<"outer_tarballhash">>}],
    LockData = {"3.5.2", Locks},
    file:write_file(LockFile,
                    io_lib:format("~p.~n~p.~ngarbage.~n",
                                  [LockData,
                                   [{a, x},
                                    {pkg_hash, Hashes},{pkg_hash_ext, ExtHashes},
                                    {b, y}]])),
    ?assertEqual(ExpandedLocks, rebar_config:consult_lock_file(LockFile)).
