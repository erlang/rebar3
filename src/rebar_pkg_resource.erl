%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
-module(rebar_pkg_resource).

-behaviour(rebar_resource).

-export([init/1
        ,lock/2
        ,download/3
        ,download/4
        ,needs_update/2
        ,make_vsn/1]).

-export([request/4
        ,etag/1]).

%% Exported for ct
-export([store_etag_in_cache/2]).

-include("rebar.hrl").

-type cached_result()   :: {'bad_checksum',string()} |
                           {'bad_registry_checksum',string()} |
                           {'failed_extract',string()} |
                           {'ok','true'} |
                           {'unexpected_hash',string(),_,binary()}.

-type download_result() :: {bad_download, binary() | string()} |
                           {fetch_fail, _, _} | cached_result().

%%==============================================================================
%% Public API
%%==============================================================================

-spec init(rebar_state:t()) -> {ok, term()}.
init(State) ->    
    HexConfig=#{api_url := DefaultApiUrl,
                repo_url := DefaultRepoUrl,
                repo_public_key := DefaultRepoPublicKey,
                repo_verify := DefaultRepoVerify} = hex_core:default_config(),
    ApiUrl = rebar_state:get(State, hex_api_url, DefaultApiUrl),    
    RepoUrl = rebar_state:get(State, hex_repo_url, 
                              %% check legacy configuration variable for setting mirrors
                              rebar_state:get(State, rebar_packages_cdn, DefaultRepoUrl)),
    RepoPublicKey = rebar_state:get(State, hex_repo_public_key, DefaultRepoPublicKey),
    RepoVerify = rebar_state:get(State, hex_repo_verify, DefaultRepoVerify),
    {ok, Vsn} = application:get_key(rebar, vsn),
    {ok, #{hex_config => HexConfig#{api_url => ApiUrl,
                                    repo_url => RepoUrl,
                                    repo_public_key => RepoPublicKey,
                                    repo_verify => RepoVerify,
                                    http_user_agent_fragment => 
                                        <<"(rebar3/", (list_to_binary(Vsn))/binary, ") (httpc)">>,
                                    http_adapter_config => #{profile => rebar}}}}.

-spec lock(AppDir, Source) -> Res when
      AppDir :: file:name(),
      Source :: tuple(),
      Res :: {atom(), string(), any()}.
lock(_AppDir, Source) ->
    Source.

%%------------------------------------------------------------------------------
%% @doc
%% Return true if the stored version of the pkg is older than the current
%% version.
%% @end
%%------------------------------------------------------------------------------
-spec needs_update(Dir, Pkg) -> Res when
      Dir :: file:name(),
      Pkg :: {pkg, Name :: binary(), Vsn :: binary(), Hash :: binary()},
      Res :: boolean().
needs_update(Dir, {pkg, _Name, Vsn, _Hash}) ->
    [AppInfo] = rebar_app_discover:find_apps([Dir], all),
    case rebar_app_info:original_vsn(AppInfo) =:= rebar_utils:to_binary(Vsn) of
        true ->
            false;
        false ->
            true
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Download the given pkg.
%% @end
%%------------------------------------------------------------------------------
-spec download(TmpDir, Pkg, State) -> Res when
      TmpDir :: file:name(),
      Pkg :: {pkg, Name :: binary(), Vsn :: binary(), Hash :: binary()},
      State :: rebar_state:t(),
      Res :: {'error',_} | {'ok',_} | {'tarball',binary() | string()}.
download(TmpDir, Pkg, State) ->
    download(TmpDir, Pkg, State, true).

%%------------------------------------------------------------------------------
%% @doc
%% Download the given pkg. The etag belonging to the pkg file will be updated
%% only if the UpdateEtag is true and the ETag returned from the hexpm server
%% is different.
%% @end
%%------------------------------------------------------------------------------
-spec download(TmpDir, Pkg, State, UpdateETag) -> Res when
      TmpDir :: file:name(),
      Pkg :: {pkg, Name :: binary(), Vsn :: binary(), Hash :: binary()},
      State :: rebar_state:t(),
      UpdateETag :: boolean(),
      Res :: download_result().
download(TmpDir, Pkg={pkg, Name, Vsn, _Hash}, State, UpdateETag) ->
    {ok, PackageDir} = rebar_packages:package_dir(State),
    Package = binary_to_list(<<Name/binary, "-", Vsn/binary, ".tar">>),
    ETagFile = binary_to_list(<<Name/binary, "-", Vsn/binary, ".etag">>),
    CachePath = filename:join(PackageDir, Package),
    ETagPath = filename:join(PackageDir, ETagFile),
    cached_download(TmpDir, CachePath, Pkg, etag(ETagPath), State,
                    ETagPath, UpdateETag).

%%------------------------------------------------------------------------------
%% @doc
%% Implementation of rebar_resource make_vsn callback.
%% Returns {error, string()} as this operation is not supported for pkg sources.
%% @end
%%------------------------------------------------------------------------------
-spec make_vsn(Vsn) -> Res when
      Vsn :: any(),
      Res :: {'error',[1..255,...]}.
make_vsn(_) ->
    {error, "Replacing version of type pkg not supported."}.

%%------------------------------------------------------------------------------
%% @doc
%% Download the pkg belonging to the given address. If the etag of the pkg
%% is the same what we stored in the etag file previously return {ok, cached},
%% if the file has changed (so the etag is not the same anymore) return
%% {ok, Contents, NewEtag}, otherwise if some error occured return error.
%% @end
%%------------------------------------------------------------------------------
-spec request(hex_core:config(), binary(), binary(), false | binary())
             -> {ok, cached} | {ok, binary(), binary()} | error.
request(Config, Name, Version, ETag) ->    
    Config1 = Config#{http_etag => ETag},
    case hex_repo:get_tarball(Config1, Name, Version) of
        {ok, {200, #{<<"etag">> := ETag1}, Tarball}} ->
            {ok, Tarball, rebar_utils:to_binary(rebar_string:trim(rebar_utils:to_list(ETag1), both, [$"]))};
        {ok, {304, _Headers, _}} ->
            {ok, cached};
        {ok, {Code, _Headers, _Body}} ->
            ?DEBUG("Request for package ~s-~s failed: status code ~p", [Name, Version, Code]),
            error;
        {error, Reason} ->
            ?DEBUG("Request for package ~s-~s failed: ~p", [Name, Version, Reason]),
            error
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Read the etag belonging to the pkg file from the cache directory. The etag
%% is stored in a separate file when the etag belonging to the package is
%% returned from the hexpm server. The name is package-vsn.etag.
%% @end
%%------------------------------------------------------------------------------
-spec etag(Path) -> Res when
      Path :: file:name(),
      Res :: binary().
etag(Path) ->
    case file:read_file(Path) of
        {ok, Bin} ->
            Bin;
        {error, _} ->
            <<>>
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Store the given etag in the .cache folder. The name is pakckage-vsn.etag.
%% @end
%%------------------------------------------------------------------------------
-spec store_etag_in_cache(File, ETag) -> Res when
      File :: file:name(),
      ETag :: binary(),
      Res :: ok.
store_etag_in_cache(Path, ETag) ->
    _ = file:write_file(Path, ETag).

%%%=============================================================================
%%% Private functions
%%%=============================================================================
-spec cached_download(TmpDir, CachePath, Pkg, ETag, State, ETagPath,
                      UpdateETag) -> Res when
      TmpDir :: file:name(),
      CachePath :: file:name(),
      Pkg :: {pkg, Name :: binary(), Vsn :: binary(), Hash :: binary()},
      ETag :: binary(),
      State :: rebar_state:t(),
      ETagPath :: file:name(),
      UpdateETag :: boolean(),
      Res :: download_result().
cached_download(TmpDir, CachePath, Pkg={pkg, Name, Vsn, _Hash}, ETag,
                State, ETagPath, UpdateETag) ->
    Resources = rebar_state:resources(State),
    #{hex_config := HexConfig} = rebar_resource:find_resource_state(pkg, Resources),
    case request(HexConfig, Name, Vsn, ETag) of
        {ok, cached} ->
            ?INFO("Version cached at ~ts is up to date, reusing it", [CachePath]),
            serve_from_cache(TmpDir, CachePath, Pkg, State);
        {ok, Body, NewETag} ->
            ?INFO("Downloaded package, caching at ~ts", [CachePath]),
            maybe_store_etag_in_cache(UpdateETag, ETagPath, NewETag),
            serve_from_download(TmpDir, CachePath, Pkg, NewETag, Body, 
                                State, ETagPath);
        error when ETag =/= <<>> ->
            store_etag_in_cache(ETagPath, ETag),
            ?INFO("Download error, using cached file at ~ts", [CachePath]),
            serve_from_cache(TmpDir, CachePath, Pkg, State);
        error ->
            {fetch_fail, Name, Vsn}
    end.

-spec serve_from_cache(TmpDir, CachePath, Pkg, State) -> Res when
      TmpDir :: file:name(),
      CachePath :: file:name(),
      Pkg :: {pkg, Name :: binary(), Vsn :: binary(), Hash :: binary()},
      State :: rebar_state:t(),
      Res :: cached_result().
serve_from_cache(TmpDir, CachePath, Pkg, State) ->
    {Files, Contents, Version, Meta} = extract(TmpDir, CachePath),
    case checksums(Pkg, Files, Contents, Version, Meta, State) of
        {Chk, Chk, Chk, Chk} ->
            ok = erl_tar:extract({binary, Contents}, [{cwd, TmpDir}, compressed]),
            {ok, true};
        {_Hash, Chk, Chk, Chk} ->
            ?DEBUG("Expected hash ~p does not match checksums ~p", [_Hash, Chk]),
            {unexpected_hash, CachePath, _Hash, Chk};
        {Chk, _Bin, Chk, Chk} ->
            ?DEBUG("Checksums: registry: ~p, pkg: ~p", [Chk, _Bin]),
            {failed_extract, CachePath};
        {Chk, Chk, _Reg, Chk} ->
            ?DEBUG("Checksums: registry: ~p, pkg: ~p", [_Reg, Chk]),
            {bad_registry_checksum, CachePath};
        {_Hash, _Bin, _Reg, _Tar} ->
            ?DEBUG("Checksums: expected: ~p, registry: ~p, pkg: ~p, meta: ~p",
                   [_Hash, _Reg, _Bin, _Tar]),
            {bad_checksum, CachePath}
    end.

-spec serve_from_download(TmpDir, CachePath, Package, ETag, Binary, State,
                          ETagPath) -> Res when
      TmpDir :: file:name(),
      CachePath :: file:name(),
      Package :: {pkg, Name :: binary(), Vsn :: binary(), Hash :: binary()},
      ETag :: binary(),
      Binary :: binary(),
      State :: rebar_state:t(),
      ETagPath :: file:name(),
      Res :: download_result().
serve_from_download(TmpDir, CachePath, Package, ETag, Binary, State, ETagPath) ->
    ?DEBUG("Writing ~p to cache at ~ts", [Package, CachePath]),
    file:write_file(CachePath, Binary),
    case etag(ETagPath) of
        ETag ->
            serve_from_cache(TmpDir, CachePath, Package, State);
        FileETag ->            
            ?DEBUG("Downloaded file ~ts ETag ~ts doesn't match returned ETag ~ts",
                   [CachePath, ETag, FileETag]),
            {bad_download, CachePath}
    end.

-spec extract(TmpDir, CachePath) -> Res when
      TmpDir :: file:name(),
      CachePath :: file:name(),
      Res :: {Files, Contents, Version, Meta},
      Files :: list({file:name(), binary()}),
      Contents :: binary(),
      Version :: binary(),
      Meta :: binary().
extract(TmpDir, CachePath) ->
    ec_file:mkdir_p(TmpDir),
    {ok, Files} = erl_tar:extract(CachePath, [memory]),
    {"contents.tar.gz", Contents} = lists:keyfind("contents.tar.gz", 1, Files),
    {"VERSION", Version} = lists:keyfind("VERSION", 1, Files),
    {"metadata.config", Meta} = lists:keyfind("metadata.config", 1, Files),
    {Files, Contents, Version, Meta}.

-spec checksums(Pkg, Files, Contents, Version, Meta, State) -> Res when
      Pkg :: {pkg, Name :: binary(), Vsn :: binary(), Hash :: binary()},
      Files :: list({file:name(), binary()}),
      Contents :: binary(),
      Version :: binary(),
      Meta :: binary(),
      State :: rebar_state:t(),
      Res :: {Hash, BinChecksum, RegistryChecksum, TarChecksum},
      Hash :: binary(),
      BinChecksum :: binary(),
      RegistryChecksum :: any(),
      TarChecksum :: binary().
checksums({pkg, Name, Vsn, Hash}, Files, Contents, Version, Meta, State) ->
    Blob = <<Version/binary, Meta/binary, Contents/binary>>,
    <<X:256/big-unsigned>> = crypto:hash(sha256, Blob),
    BinChecksum = list_to_binary(
                    rebar_string:uppercase(
                      lists:flatten(io_lib:format("~64.16.0b", [X])))),
    RegistryChecksum = rebar_packages:registry_checksum(Name, Vsn, State),
    {"CHECKSUM", TarChecksum} = lists:keyfind("CHECKSUM", 1, Files),
    {Hash, BinChecksum, RegistryChecksum, TarChecksum}.

-spec maybe_store_etag_in_cache(UpdateETag, Path, ETag) -> Res when
      UpdateETag :: boolean(),
      Path :: file:name(),
      ETag :: binary(),
      Res :: ok.
maybe_store_etag_in_cache(false = _UpdateETag, _Path, _ETag) ->
    ok;
maybe_store_etag_in_cache(true = _UpdateETag, Path, ETag) ->
    store_etag_in_cache(Path, ETag).
