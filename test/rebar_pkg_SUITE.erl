%% Test suite for the rebar pkg index caching and decompression
%% mechanisms.
-module(rebar_pkg_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("rebar.hrl").

-define(bad_etag, <<"abcdef">>).
-define(good_etag, <<"22e1d7387c9085a462340088a2a8ba67">>).
-define(badpkg_checksum, <<"A14E3718B33F8124E98004433193509EC6660F6CA03302657CAB8785751D77A0">>).
-define(badindex_checksum, <<"7B2CBED315C89F3126B5BF553DD7FF0FB5FE94B064888DD1B095CE8BF4B6A16A">>).
-define(bad_checksum, <<"D576B442A68C7B92BACDE1EFE9C6E54D8D6C74BDB71D8175B9D3C6EC8C7B62A7">>).
-define(good_checksum, <<"ABA3B638A653A2414BF9DFAF76D90C937C53D1BE5B5D51A990C6FCC3A775C6F">>).
-define(BADPKG_ETAG, <<"BADETAG">>).

all() -> [good_uncached, good_cached, badpkg, badhash_nocache,
          badindexchk, badhash_cache, bad_to_good, good_disconnect,
          bad_disconnect, pkgs_provider, find_highest_matching].

init_per_suite(Config) ->
    application:start(meck),
    Config.

end_per_suite(_Config) ->
    application:stop(meck).

init_per_testcase(pkgs_provider=Name, Config) ->
    %% Need to mock out a registry for this test now because it will try to update it automatically
    Priv = ?config(priv_dir, Config),
    Tid = ets:new(registry_table, [public]),
    ets:insert_new(Tid, []),
    CacheRoot = filename:join([Priv, "cache", atom_to_list(Name)]),
    CacheDir = filename:join([CacheRoot, "hex", "com", "test", "packages"]),
    filelib:ensure_dir(filename:join([CacheDir, "registry"])),
    ok = ets:tab2file(Tid, filename:join([CacheDir, "registry"])),
    Config;
init_per_testcase(good_uncached=Name, Config0) ->
    Config = [{good_cache, false},
              {pkg, {<<"goodpkg">>, <<"1.0.0">>}}
             | Config0],
    mock_config(Name, Config);
init_per_testcase(good_cached=Name, Config0) ->
    Pkg = {<<"goodpkg">>, <<"1.0.0">>},
    Config1 = [{good_cache, true},
               {pkg, Pkg}
              | Config0],
    Config = mock_config(Name, Config1),
    copy_to_cache(Pkg, Config),
    Config;
init_per_testcase(badindexchk=Name, Config0) ->
    Config = [{good_cache, false},
              {pkg, {<<"badindexchk">>, <<"1.0.0">>}}
             | Config0],
    mock_config(Name, Config);
init_per_testcase(badpkg=Name, Config0) ->
    Config = [{good_cache, false},
              {pkg, {<<"badpkg">>, <<"1.0.0">>}}
             | Config0],
    mock_config(Name, Config);
init_per_testcase(badhash_nocache=Name, Config0) ->
    Config = [{good_cache, false},
              {pkg, {<<"goodpkg">>, <<"1.0.0">>}}
             | Config0],
    mock_config(Name, Config);
init_per_testcase(badhash_cache=Name, Config0) ->
    Pkg = {<<"goodpkg">>, <<"1.0.0">>},
    Config1 = [{good_cache, true},
               {pkg, Pkg}
              | Config0],
    Config = mock_config(Name, Config1),
    copy_to_cache(Pkg, Config),
    Config;
init_per_testcase(bad_to_good=Name, Config0) ->
    Config1 = [{good_cache, false},
              {pkg, {<<"goodpkg">>, <<"1.0.0">>}}
              | Config0],
    Config = mock_config(Name, Config1),
    Source = filename:join(?config(data_dir, Config), <<"badpkg-1.0.0.tar">>),
    Dest = filename:join(?config(cache_dir, Config), <<"goodpkg-1.0.0.tar">>),
    ec_file:copy(Source, Dest),
    Config;
init_per_testcase(good_disconnect=Name, Config0) ->
    Pkg = {<<"goodpkg">>, <<"1.0.0">>},
    Config1 = [{good_cache, false},
               {pkg, Pkg}
              | Config0],
    Config = mock_config(Name, Config1),
    copy_to_cache(Pkg, Config),
    %% meck:unload(httpc),
    meck:new(httpc, [passthrough, unsticky]),
    meck:expect(httpc, request, fun(_, _, _, _) -> {error, econnrefused} end),
    Config;
init_per_testcase(bad_disconnect=Name, Config0) ->
    Pkg = {<<"goodpkg">>, <<"1.0.0">>},
    Config1 = [{good_cache, false},
               {pkg, Pkg}
              | Config0],
    Config = mock_config(Name, Config1),
    meck:expect(r3_hex_repo, get_tarball, fun(_, _, _) ->
                                               {error, econnrefused}
                                       end),
    Config;
init_per_testcase(Name, Config0) ->
    Config = [{good_cache, false},
              {pkg, {<<"goodpkg">>, <<"1.0.0">>}}
              | Config0],
    mock_config(Name, Config).

end_per_testcase(_, Config) ->
    unmock_config(Config),
    Config.

good_uncached(Config) ->
    Tmp = ?config(tmp_dir, Config),
    {Pkg,Vsn} = ?config(pkg, Config),
    State = ?config(state, Config),
    ?assertEqual(ok,
                 rebar_pkg_resource:download(Tmp, {pkg, Pkg, Vsn, ?good_checksum, ?good_checksum, #{}}, State, #{}, true)),
    Cache = ?config(cache_dir, Config),
    ?assert(filelib:is_regular(filename:join(Cache, <<Pkg/binary, "-", Vsn/binary, ".tar">>))).

good_cached(Config) ->
    Tmp = ?config(tmp_dir, Config),
    {Pkg,Vsn} = ?config(pkg, Config),
    State = ?config(state, Config),
    Cache = ?config(cache_dir, Config),
    CachedFile = filename:join(Cache, <<Pkg/binary, "-", Vsn/binary, ".tar">>),
    ?assert(filelib:is_regular(CachedFile)),
    {ok, Content} = file:read_file(CachedFile),
    ?assertEqual(ok,
                 rebar_pkg_resource:download(Tmp, {pkg, Pkg, Vsn, ?good_checksum, ?good_checksum, #{}}, State, #{}, true)),
    {ok, Content} = file:read_file(CachedFile).


badindexchk(Config) ->
    Tmp = ?config(tmp_dir, Config),
    {Pkg,Vsn} = ?config(pkg, Config),
    State = ?config(state, Config),
    ?assertMatch({error, {rebar_pkg_resource, {bad_registry_checksum, _, _, _, _}}},
                 rebar_pkg_resource:download(Tmp, {pkg, Pkg, Vsn, ?bad_checksum, ?bad_checksum, #{}}, State, #{}, true)),
    %% The cached file is there for forensic purposes
    Cache = ?config(cache_dir, Config),
    ?assert(filelib:is_regular(filename:join(Cache, <<Pkg/binary, "-", Vsn/binary, ".tar">>))).

badpkg(Config) ->
    Tmp = ?config(tmp_dir, Config),
    {Pkg,Vsn} = ?config(pkg, Config),
    State = ?config(state, Config),
    Cache = ?config(cache_dir, Config),
    CachePath = filename:join(Cache, <<Pkg/binary, "-", Vsn/binary, ".tar">>),
    ETagPath = filename:join(Cache, <<Pkg/binary, "-", Vsn/binary, ".etag">>),
    rebar_pkg_resource:store_etag_in_cache(ETagPath, ?BADPKG_ETAG),
    ?assertMatch({error, {hex_tarball, {tarball, {inner_checksum_mismatch, _, _}}}},
                 rebar_pkg_resource:download(Tmp, {pkg, Pkg, Vsn, ?badpkg_checksum, ?badpkg_checksum, #{}}, State, #{}, false)),
    %% The cached/etag files are there for forensic purposes
    ?assert(filelib:is_regular(ETagPath)),
    ?assert(filelib:is_regular(CachePath)).

badhash_nocache(Config) ->
    Tmp = ?config(tmp_dir, Config),
    {Pkg,Vsn} = ?config(pkg, Config),
    State = ?config(state, Config),
    ?assertMatch({error, {rebar_pkg_resource, {bad_registry_checksum, _, _, _, _}}},
                 rebar_pkg_resource:download(Tmp, {pkg, Pkg, Vsn, ?bad_checksum, ?bad_checksum, #{}}, State, #{}, true)),
    %% The cached file is there for forensic purposes
    Cache = ?config(cache_dir, Config),
    ?assert(filelib:is_regular(filename:join(Cache, <<Pkg/binary, "-", Vsn/binary, ".tar">>))).

badhash_cache(Config) ->
    Tmp = ?config(tmp_dir, Config),
    {Pkg,Vsn} = ?config(pkg, Config),
    Cache = ?config(cache_dir, Config),
    State = ?config(state, Config),
    CachedFile = filename:join(Cache, <<Pkg/binary, "-", Vsn/binary, ".tar">>),
    ?assert(filelib:is_regular(CachedFile)),
    {ok, Content} = file:read_file(CachedFile),
    ?assertMatch({error, {rebar_pkg_resource, {bad_registry_checksum, _, _, _, _}}},
                 rebar_pkg_resource:download(Tmp, {pkg, Pkg, Vsn, ?bad_checksum, ?bad_checksum, #{}}, State, #{}, true)),
    %% The cached file is there still, unchanged.
    ?assert(filelib:is_regular(CachedFile)),
    ?assertEqual({ok, Content}, file:read_file(CachedFile)).

bad_to_good(Config) ->
    Tmp = ?config(tmp_dir, Config),
    {Pkg,Vsn} = ?config(pkg, Config),
    State = ?config(state, Config),
    Cache = ?config(cache_dir, Config),
    CachedFile = filename:join(Cache, <<Pkg/binary, "-", Vsn/binary, ".tar">>),
    ?assert(filelib:is_regular(CachedFile)),
    {ok, Contents} = file:read_file(CachedFile),
    ?assertEqual(ok,
                 rebar_pkg_resource:download(Tmp, {pkg, Pkg, Vsn, ?good_checksum, ?good_checksum, #{}}, State, #{}, true)),
    %% Cache has refreshed
    ?assert({ok, Contents} =/= file:read_file(CachedFile)).

good_disconnect(Config) ->
    Tmp = ?config(tmp_dir, Config),
    {Pkg,Vsn} = ?config(pkg, Config),
    State = ?config(state, Config),
    Cache = ?config(cache_dir, Config),
    CachedFile = filename:join(Cache, <<Pkg/binary, "-", Vsn/binary, ".tar">>),
    ETagFile = filename:join(Cache, <<Pkg/binary, "-", Vsn/binary, ".etag">>),
    ?assert(filelib:is_regular(CachedFile)),
    {ok, Content} = file:read_file(CachedFile),
    rebar_pkg_resource:store_etag_in_cache(ETagFile, ?BADPKG_ETAG),
    ?assertEqual(ok,
                 rebar_pkg_resource:download(Tmp, {pkg, Pkg, Vsn, ?good_checksum, ?good_checksum, #{}}, State, #{}, true)),
    {ok, Content} = file:read_file(CachedFile).

bad_disconnect(Config) ->
    Tmp = ?config(tmp_dir, Config),
    {Pkg,Vsn} = ?config(pkg, Config),
    State = ?config(state, Config),
    ?assertEqual({fetch_fail, Pkg, Vsn},
                 rebar_pkg_resource:download(Tmp, {pkg, Pkg, Vsn, ?good_checksum, ?good_checksum, #{}}, State, #{}, true)).

pkgs_provider(Config) ->
    Config1 = rebar_test_utils:init_rebar_state(Config),
    rebar_test_utils:run_and_check(
      Config1, [], ["pkgs", "relx"],
        {ok, []}
    ).

find_highest_matching(_Config) ->
    State = rebar_state:new(),
    {ok, Vsn} = rebar_packages:find_highest_matching_(
                  <<"goodpkg">>, ec_semver:parse(<<"1.0.0">>), #{name => <<"hexpm">>}, ?PACKAGE_TABLE, State),
    ?assertEqual({{1,0,1},{[],[]}}, Vsn),
    {ok, Vsn1} = rebar_packages:find_highest_matching(
                   <<"goodpkg">>, ec_semver:parse(<<"1.0">>), #{name => <<"hexpm">>}, ?PACKAGE_TABLE, State),
    ?assertEqual({{1,1,1},{[],[]}}, Vsn1),
    {ok, Vsn2} = rebar_packages:find_highest_matching(
                   <<"goodpkg">>, ec_semver:parse(<<"2.0">>), #{name => <<"hexpm">>}, ?PACKAGE_TABLE, State),
    ?assertEqual({{2,0,0},{[],[]}}, Vsn2),

    %% regression test. ~> constraints higher than the available packages would result
    %% in returning the first package version instead of 'none'.
    ?assertEqual(none, rebar_packages:find_highest_matching_(<<"goodpkg">>, ec_semver:parse(<<"5.0">>),
                                                             #{name => <<"hexpm">>}, ?PACKAGE_TABLE, State)),


    {ok, Vsn3} = rebar_packages:find_highest_matching_(<<"goodpkg">>, ec_semver:parse(<<"3.0.0-rc.0">>),
                                                       #{name => <<"hexpm">>}, ?PACKAGE_TABLE, State),
    ?assertEqual({{3,0,0},{[<<"rc">>,0],[]}}, Vsn3).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%
mock_config(Name, Config) ->
    Priv = ?config(priv_dir, Config),
    CacheRoot = filename:join([Priv, "cache", atom_to_list(Name)]),
    TmpDir = filename:join([Priv, "tmp", atom_to_list(Name)]),
    Tid = ets:new(registry_table, [public]),
    AllDeps = [
        {{<<"badindexchk">>,<<"1.0.0">>}, [[], ?bad_checksum, ?bad_checksum, [<<"rebar3">>]]},
        {{<<"goodpkg">>,<<"1.0.0">>}, [[], ?good_checksum, ?good_checksum, [<<"rebar3">>]]},
        {{<<"goodpkg">>,<<"1.0.1">>}, [[], ?good_checksum, ?good_checksum, [<<"rebar3">>]]},
        {{<<"goodpkg">>,<<"1.1.1">>}, [[], ?good_checksum, ?good_checksum, [<<"rebar3">>]]},
        {{<<"goodpkg">>,<<"2.0.0">>}, [[], ?good_checksum, ?good_checksum, [<<"rebar3">>]]},
        {{<<"goodpkg">>,<<"3.0.0-rc.0">>}, [[], ?good_checksum, ?good_checksum, [<<"rebar3">>]]},
        {{<<"badpkg">>,<<"1.0.0">>}, [[], ?badpkg_checksum, ?badpkg_checksum, [<<"rebar3">>]]}
    ],
    ets:insert_new(Tid, AllDeps),
    CacheDir = filename:join([CacheRoot, "hex", "com", "test", "packages"]),
    filelib:ensure_dir(filename:join([CacheDir, "registry"])),
    ok = ets:tab2file(Tid, filename:join([CacheDir, "registry"])),

    catch ets:delete(?PACKAGE_TABLE),
    rebar_packages:new_package_table(),
    lists:foreach(fun({{N, Vsn}, [Deps, InnerChecksum, OuterChecksum, _]}) ->
                          case ets:member(?PACKAGE_TABLE, {ec_cnv:to_binary(N), Vsn, <<"hexpm">>}) of
                              false ->
                                  ets:insert(?PACKAGE_TABLE, #package{key={ec_cnv:to_binary(N), ec_semver:parse(Vsn), <<"hexpm">>},
                                                                      dependencies=Deps,
                                                                      retired=false,
                                                                      inner_checksum=InnerChecksum,
                                                                      outer_checksum=OuterChecksum});
                              true ->
                                  ok
                          end
                  end, AllDeps),


    meck:new(r3_hex_repo, [passthrough]),
    meck:expect(r3_hex_repo, get_package,
                fun(_Config, PkgName) ->
                        Matches = ets:match_object(Tid, {{PkgName,'_'}, '_'}),
                        Releases =
                            [#{outer_checksum => OuterChecksum,
                               inner_checksum => InnerChecksum,
                               version => Vsn,
                               dependencies => Deps} ||
                                {{_, Vsn}, [Deps, InnerChecksum, OuterChecksum, _]} <- Matches],
                        {ok, {200, #{}, Releases}}
                end),

    %% The state returns us a fake registry
    meck:new(rebar_state, [passthrough]),
    meck:expect(rebar_state, get,
                fun(_State, rebar_packages_cdn, _Default) ->
                        "http://test.com/";
                   (_, _, Default) ->
                        Default
                end),
    meck:expect(rebar_state, resources,
                fun(_State) ->
                        DefaultConfig = r3_hex_core:default_config(),
                        [rebar_resource_v2:new(pkg, rebar_pkg_resource,
                                               #{repos => [DefaultConfig#{name => <<"hexpm">>}],
                                                 base_config => #{}})]
                end),

    meck:new(rebar_dir, [passthrough]),
    meck:expect(rebar_dir, global_cache_dir, fun(_) -> CacheRoot end),

    meck:expect(rebar_packages, registry_dir, fun(_) -> {ok, CacheDir} end),
    meck:expect(rebar_packages, package_dir, fun(_, _) -> {ok, CacheDir} end),

    meck:new(rebar_prv_update, [passthrough]),
    meck:expect(rebar_prv_update, do, fun(State) -> {ok, State} end),

    %% Cache fetches are mocked -- we assume the server and clients are
    %% correctly used.
    GoodCache = ?config(good_cache, Config),
    {Pkg,Vsn} = ?config(pkg, Config),
    PkgFile = <<Pkg/binary, "-", Vsn/binary, ".tar">>,
    {ok, PkgContents} = file:read_file(filename:join(?config(data_dir, Config), PkgFile)),

    meck:expect(r3_hex_repo, get_tarball, fun(_, _, _) when GoodCache ->
                                               {ok, {304, #{<<"etag">> => ?good_etag}, <<>>}};
                                          (_, _, _) ->
                                               {ok, {200, #{<<"etag">> => ?good_etag}, PkgContents}}
                                       end),

    [{cache_root, CacheRoot},
     {cache_dir, CacheDir},
     {tmp_dir, TmpDir},
     {mock_table, Tid} | Config].

unmock_config(Config) ->
    meck:unload(),
    catch ets:delete(?config(mock_table, Config)).

copy_to_cache({Pkg,Vsn}, Config) ->
    Name = <<Pkg/binary, "-", Vsn/binary, ".tar">>,
    Source = filename:join(?config(data_dir, Config), Name),
    Dest = filename:join(?config(cache_dir, Config), Name),
    ec_file:copy(Source, Dest).
