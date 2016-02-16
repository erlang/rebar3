%%% Most locking tests are implicit in other test suites handling
%%% dependencies.
%%% This suite is to test the compatibility layers between various
%%% versions of lockfiles.
-module(rebar_lock_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() -> [current_version, future_versions_no_attrs, future_versions_attrs].

current_version(Config) ->
    %% Current version just dumps the locks as is on disk.
    LockFile = filename:join(?config(priv_dir, Config), "current_version"),
    Locks = [{<<"app1">>, {git,"some_url", {ref,"some_ref"}}, 2},
             {<<"app2">>, {git,"some_url", {ref,"some_ref"}}, 0},
             {<<"app3">>, {hg,"some_url", {ref,"some_ref"}}, 1},
             {<<"pkg1">>,{pkg,<<"name">>,<<"0.1.6">>},3}],
    file:write_file(LockFile, io_lib:format("~p.~n", [Locks])),
    ?assertEqual(Locks, rebar_config:consult_lock_file(LockFile)).

future_versions_no_attrs(Config) ->
    %% Future versions will keep the same core attribute in there, but
    %% will do so under a new format bundled with a version and potentially
    %% some trailing attributes
    LockFile = filename:join(?config(priv_dir, Config), "future_versions"),
    Locks = [{<<"app1">>, {git,"some_url", {ref,"some_ref"}}, 2},
             {<<"app2">>, {git,"some_url", {ref,"some_ref"}}, 0},
             {<<"app3">>, {hg,"some_url", {ref,"some_ref"}}, 1},
             {<<"pkg1">>,{pkg,<<"name">>,<<"0.1.6">>},3}],
    LockData = {"3.5.2", Locks},
    file:write_file(LockFile, io_lib:format("~p.~n", [LockData])),
    ?assertEqual(Locks, rebar_config:consult_lock_file(LockFile)).

future_versions_attrs(Config) ->
    %% Future versions will keep the same core attribute in there, but
    %% will do so under a new format bundled with a version and potentially
    %% some trailing attributes
    LockFile = filename:join(?config(priv_dir, Config), "future_versions"),
    Locks = [{<<"app1">>, {git,"some_url", {ref,"some_ref"}}, 2},
             {<<"app2">>, {git,"some_url", {ref,"some_ref"}}, 0},
             {<<"app3">>, {hg,"some_url", {ref,"some_ref"}}, 1},
             {<<"pkg1">>,{pkg,<<"name">>,<<"0.1.6">>},3}],
    LockData = {"3.5.2", Locks},
    file:write_file(LockFile, io_lib:format("~p.~na.~n{b,c}.~n[d,e,f].~n", [LockData])),
    ?assertEqual(Locks, rebar_config:consult_lock_file(LockFile)).
