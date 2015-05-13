%% Test suite for the rebar pkg index caching and decompression
%% mechanisms.
-module(rebar_pkg_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(bad_etag, "abcdef").
-define(good_etag, "22e1d7387c9085a462340088a2a8ba67").
-define(bad_checksum, <<"D576B442A68C7B92BACDE1EFE9C6E54D8D6C74BDB71D8175B9D3C6EC8C7B62A7">>).
-define(good_checksum, <<"1C6CE379D191FBAB41B7905075E0BF87CBBE23C77CECE775C5A0B786B2244C35">>).

all() -> [good_uncached].


init_per_suite(Config) ->
    application:start(meck),
    Config.

end_per_suite(_Config) ->
    application:stop(meck).

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
    Config.


end_per_testcase(_, Config) ->
    unmock_config(Config),
    Config.

mock_config(Name, Config) ->
    Priv = ?config(priv_dir, Config),
    CacheRoot = filename:join([Priv, "cache", atom_to_list(Name)]),
    TmpDir = filename:join([Priv, "tmp", atom_to_list(Name)]),
    T = ets:new(fake_registry, [public]),
    ets:insert_new(T, [
        {{<<"badindexchk">>,<<"1.0.0">>}, [[], ?bad_checksum]},
        {{<<"goodpkg">>,<<"1.0.0">>}, [[], ?good_checksum]},
        {{<<"badpkg">>,<<"1.0.0">>}, [[], ?good_checksum]}
    ]),
    CacheDir = filename:join([CacheRoot, "hex", "com", "test", "packages"]),
    filelib:ensure_dir(filename:join([CacheDir, "registry"])),
    ok = ets:tab2file(T, filename:join([CacheDir, "registry"])),
    %% The state returns us a fake registry
    meck:new(rebar_state, [passthrough]),
    meck:expect(rebar_state, registry,
                fun(_State) -> {ok, fake_registry} end),
    meck:expect(rebar_state, get,
                fun(_State, rebar_packages_cdn, _Default) ->
                    "http://test.com/"
                end),
    meck:new(rebar_dir, [passthrough]),
    meck:expect(rebar_dir, global_cache_dir, fun(_) -> CacheRoot end),
    %% Cache fetches are mocked -- we assume the server and clients are
    %% correctly used.
    GoodCache = ?config(good_cache, Config),
    {Pkg,Vsn} = ?config(pkg, Config),
    PkgFile = <<Pkg/binary, "-", Vsn/binary, ".tar">>,
    {ok, PkgContents} = file:read_file(filename:join(?config(data_dir, Config), PkgFile)),
    meck:new(httpc, [passthrough, unsticky]),
    meck:expect(httpc, request,
            fun(get, {_Url, _Opts}, _, _) when GoodCache ->
                {ok, {{Vsn, 304, <<"Not Modified">>}, [{"etag", ?good_etag}], <<>>}};
               (get, {_Url, _Opts}, _, _) ->
                {ok, {{Vsn, 200, <<"OK">>}, [{"etag", ?good_etag}], PkgContents}}
            end),
    [{cache_root, CacheRoot},
     {cache_dir, CacheDir},
     {tmp_dir, TmpDir},
     {mock_table, T} | Config].

unmock_config(Config) ->
    meck:unload(),
    ets:delete(?config(mock_table, Config)).

copy_to_cache({Pkg,Vsn}, Config) ->
    Name = <<Pkg/binary, "-", Vsn/binary, ".tar">>,
    Source = filename:join(?config(data_dir, Config), Name),
    Dest = filename:join(?config(cache_dir, Config), Name),
    ec_file:copy(Source, Dest).

good_uncached(Config) ->
    Tmp = ?config(tmp_dir, Config),
    {Pkg,Vsn} = ?config(pkg, Config),
    State = ?config(state, Config),
    ?assertEqual({ok, true},
                 rebar_pkg_resource:download(Tmp, {pkg, Pkg, Vsn}, State)),
    Cache = ?config(cache_dir, Config),
    ?assert(filelib:is_regular(filename:join(Cache, <<Pkg/binary, "-", Vsn/binary, ".tar">>))).
