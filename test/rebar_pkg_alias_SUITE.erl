-module(rebar_pkg_alias_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").

all() -> [same_alias, diff_alias, diff_alias_vsn].

%% {uuid, {pkg, uuid}} = uuid
%% {uuid, {pkg, alias}} = uuid on disk
%% another run should yield the same lock file without error
init_per_suite(Config) ->
    mock_config(?MODULE, Config).

end_per_suite(Config) ->
    unmock_config(Config).

init_per_testcase(same_alias, Config0) ->
    Config = rebar_test_utils:init_rebar_state(Config0,"same_alias_"),
    AppDir = ?config(apps, Config),
    rebar_test_utils:create_app(AppDir, "A", "0.0.0", [kernel, stdlib]),
    RebarConf = rebar_test_utils:create_config(AppDir, [{deps, [{fakelib, {pkg, fakelib}}]}]),
    [{rebarconfig, RebarConf} | Config];
init_per_testcase(diff_alias, Config0) ->
    Config = rebar_test_utils:init_rebar_state(Config0,"diff_alias_"),
    AppDir = ?config(apps, Config),
    rebar_test_utils:create_app(AppDir, "A", "0.0.0", [kernel, stdlib]),
    RebarConf = rebar_test_utils:create_config(AppDir, [{deps, [{fakelib, {pkg, goodpkg}}]}]),
    [{rebarconfig, RebarConf} | Config];
init_per_testcase(diff_alias_vsn, Config0) ->
    Config = rebar_test_utils:init_rebar_state(Config0,"diff_alias_vsn_"),
    AppDir = ?config(apps, Config),
    rebar_test_utils:create_app(AppDir, "A", "0.0.0", [kernel, stdlib]),
    RebarConf = rebar_test_utils:create_config(AppDir, [{deps, [{fakelib, "1.0.0", {pkg, goodpkg}}]}]),
    [{rebarconfig, RebarConf} | Config].

end_per_testcase(_, Config) ->
    Config.

same_alias(Config) ->
    {ok, RebarConfig} = file:consult(?config(rebarconfig, Config)),
    rebar_test_utils:run_and_check(
        Config, RebarConfig, ["lock"],
        {ok, [{lock, "fakelib"}, {dep, "fakelib"}]}
    ).

diff_alias(Config) ->
    %% even though the dep is 'fakelib' aliased as 'goodpkg' all
    %% internal records use 'fakelib' as a value. Just make sure
    %% the lock actually maintains the proper source as 'goodpkg'
    AppDir = ?config(apps, Config),
    Lockfile = filename:join([AppDir, "rebar.lock"]),
    {ok, RebarConfig} = file:consult(?config(rebarconfig, Config)),
    rebar_test_utils:run_and_check(
        Config, RebarConfig, ["lock"],
        {ok, [{lock, "fakelib"},{dep, "fakelib"}]}
    ),
    {ok, [LockData]} = file:consult(Lockfile),
    ?assert(lists:any(fun({<<"fakelib">>,{pkg,<<"goodpkg">>,_},_}) -> true
                      ;  (_) -> false end, LockData)),
    %% An second run yields the same
    rebar_test_utils:run_and_check(
        Config, RebarConfig, ["lock"],
        {ok, [{lock, "fakelib"},{dep, "fakelib"}]}
    ),
    {ok, [LockData]} = file:consult(Lockfile),
    %% So does an upgrade
    rebar_test_utils:run_and_check(
        Config, RebarConfig, ["upgrade"],
        {ok, [{lock, "fakelib"},{dep, "fakelib"}]}
    ),
    {ok, [LockData]} = file:consult(Lockfile).

diff_alias_vsn(Config) -> diff_alias(Config).

mock_config(Name, Config) ->
    Priv = ?config(priv_dir, Config),
    AppDir = filename:join([Priv, "fakelib"]),
    CacheRoot = filename:join([Priv, "cache", atom_to_list(Name)]),
    TmpDir = filename:join([Priv, "tmp", atom_to_list(Name)]),
    CacheDir = filename:join([CacheRoot, "hex", "com", "test", "packages"]),
    filelib:ensure_dir(filename:join([CacheDir, "registry"])),
    rebar_test_utils:create_app(AppDir, "fakelib", "1.0.0", [kernel, stdlib]),
    {Chk,Etag} = rebar_test_utils:package_app(AppDir, CacheDir, "fakelib-1.0.0"),
    {Chk,Etag} = rebar_test_utils:package_app(AppDir, CacheDir, "goodpkg-1.0.0"),

    Tid = ets:new(registry_table, [public]),
    ets:insert_new(Tid, [
        {<<"fakelib">>,[[<<"1.0.0">>]]},
        {<<"goodpkg">>,[[<<"1.0.0">>]]},
        {{<<"fakelib">>,<<"1.0.0">>}, [[], Chk, [<<"rebar3">>]]},
        {{<<"goodpkg">>,<<"1.0.0">>}, [[], Chk, [<<"rebar3">>]]}
    ]),
    ok = ets:tab2file(Tid, filename:join([CacheDir, "registry"])),
    ets:delete(Tid),
    %% The state returns us a fake registry
    meck:new(rebar_dir, [passthrough, no_link]),
    meck:expect(rebar_dir, global_cache_dir, fun(_) -> CacheRoot end),

    meck:new(rebar_packages, [passthrough, no_link]),
    meck:expect(rebar_packages, registry_dir, fun(_) -> CacheDir end),
    meck:expect(rebar_packages, package_dir, fun(_) -> CacheDir end),
    rebar_prv_update:hex_to_index(rebar_state:new()),

    %% Cache fetches are mocked -- we assume the server and clients are
    %% correctly used.
    meck:new(httpc, [passthrough, unsticky, no_link]),
    meck:expect(httpc, request,
            fun(get, {_Url, _Opts}, _, _, _) ->
                {ok, {{<<"1.0.0">>, 304, <<"Not Modified">>}, [{"etag", Etag}], <<>>}}
            end),
    %% Move all packages to cache
    NewConf = [{cache_root, CacheRoot},
               {cache_dir, CacheDir},
               {tmp_dir, TmpDir},
               {mock_table, Tid} | Config],
    NewConf.

unmock_config(Config) ->
    meck:unload(),
    Config.
