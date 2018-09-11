%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
-module(rebar_pkg_resource).

-behaviour(rebar_resource_v2).

-export([init/2
        ,lock/2
        ,download/4
        ,download/5
        ,needs_update/2
        ,make_vsn/2]).

-export([request/4
        ,etag/1]).

-ifdef(TEST).
%% exported for test purposes
-export([store_etag_in_cache/2]).
-endif.

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

-spec init(atom(), rebar_state:t()) -> {ok, rebar_resource_v2:resource()}.
init(Type, State) ->
    {ok, Vsn} = application:get_key(rebar, vsn),
    BaseConfig = #{http_adapter => hex_http_httpc,
                   http_user_agent_fragment =>
                       <<"(rebar3/", (list_to_binary(Vsn))/binary, ") (httpc)">>,
                   http_adapter_config => #{profile => rebar}},
    Repos = rebar_hex_repos:from_state(BaseConfig, State),
    Resource = rebar_resource_v2:new(Type, ?MODULE, #{repos => Repos,
                                                      base_config => BaseConfig}),
    {ok, Resource}.



-spec lock(AppInfo, ResourceState) -> Res when
      AppInfo :: rebar_app_info:t(),
      ResourceState :: rebar_resource_v2:resource_state(),
      Res :: {atom(), string(), any(), binary()}.
lock(AppInfo, _) ->
    {pkg, Name, Vsn, Hash, _RepoConfig} = rebar_app_info:source(AppInfo),
    {pkg, Name, Vsn, Hash}.

%%------------------------------------------------------------------------------
%% @doc
%% Return true if the stored version of the pkg is older than the current
%% version.
%% @end
%%------------------------------------------------------------------------------
-spec needs_update(AppInfo, ResourceState) -> Res when
      AppInfo :: rebar_app_info:t(),
      ResourceState :: rebar_resource_v2:resource_state(),
      Res :: boolean().
needs_update(AppInfo, _) ->
    {pkg, _Name, Vsn, _Hash, _} = rebar_app_info:source(AppInfo),
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
-spec download(TmpDir, AppInfo, State, ResourceState) -> Res when
      TmpDir :: file:name(),
      AppInfo :: rebar_app_info:t(),
      ResourceState :: rebar_resource_v2:resource_state(),
      State :: rebar_state:t(),
      Res :: {'error',_} | {'ok',_} | {'tarball',binary() | string()}.
download(TmpDir, AppInfo, State, ResourceState) ->
    download(TmpDir, rebar_app_info:source(AppInfo), State, ResourceState, true).

%%------------------------------------------------------------------------------
%% @doc
%% Download the given pkg. The etag belonging to the pkg file will be updated
%% only if the UpdateEtag is true and the ETag returned from the hexpm server
%% is different.
%% @end
%%------------------------------------------------------------------------------
-spec download(TmpDir, Pkg, State, ResourceState, UpdateETag) -> Res when
      TmpDir :: file:name(),
      Pkg :: {pkg, Name :: binary(), Vsn :: binary(), Hash :: binary(), RepoConfig :: hex_core:config()},
      State :: rebar_state:t(),
      ResourceState:: rebar_resource_v2:resource_state(),
      UpdateETag :: boolean(),
      Res :: download_result().
download(TmpDir, Pkg={pkg, Name, Vsn, _Hash, Repo}, State, _ResourceState, UpdateETag) ->
    {ok, PackageDir} = rebar_packages:package_dir(Repo, State),
    Package = binary_to_list(<<Name/binary, "-", Vsn/binary, ".tar">>),
    ETagFile = binary_to_list(<<Name/binary, "-", Vsn/binary, ".etag">>),
    CachePath = filename:join(PackageDir, Package),
    ETagPath = filename:join(PackageDir, ETagFile),
    cached_download(TmpDir, CachePath, Pkg, etag(ETagPath),
                    State, ETagPath, UpdateETag).

%%------------------------------------------------------------------------------
%% @doc
%% Implementation of rebar_resource make_vsn callback.
%% Returns {error, string()} as this operation is not supported for pkg sources.
%% @end
%%------------------------------------------------------------------------------
-spec make_vsn(AppInfo, ResourceState) -> Res when
      AppInfo :: rebar_app_info:t(),
      ResourceState :: rebar_resource_v2:resource_state(),
      Res :: {'error', string()}.
make_vsn(_, _) ->
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
    try hex_repo:get_tarball(Config1, Name, Version) of
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
    catch
        _:Exception ->
            ?DEBUG("hex_repo:get_tarball failed: ~p", [Exception]),
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
      Pkg :: {pkg, Name :: binary(), Vsn :: binary(), Hash :: binary(), RepoConfig :: hex_core:config()},
      ETag :: binary(),
      State :: rebar_state:t(),
      ETagPath :: file:name(),
      UpdateETag :: boolean(),
      Res :: download_result().
cached_download(TmpDir, CachePath, Pkg={pkg, Name, Vsn, _Hash, RepoConfig}, ETag,
                State, ETagPath, UpdateETag) ->
    case request(RepoConfig, Name, Vsn, ETag) of
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
      Pkg :: {pkg, Name :: binary(), Vsn :: binary(), Hash :: binary(), RepoConfig :: hex_core:config()},
      State :: rebar_state:t(),
      Res :: cached_result().
serve_from_cache(TmpDir, CachePath, Pkg, State) ->
    {Files, Contents, Version, Meta} = extract(TmpDir, CachePath),
    case checksums(Pkg, Files, Contents, Version, Meta, State) of
        {Chk, Chk, Chk} ->
            ok = erl_tar:extract({binary, Contents}, [{cwd, TmpDir}, compressed]),
            {ok, true};
        {_Hash, Chk, Chk} ->
            ?DEBUG("Expected hash ~p does not match checksums ~p", [_Hash, Chk]),
            {unexpected_hash, CachePath, _Hash, Chk};
        {Chk, _Bin, Chk} ->
            ?DEBUG("Checksums: registry: ~p, pkg: ~p", [Chk, _Bin]),
            {failed_extract, CachePath};
        {_Hash, _Bin, _Tar} ->
            ?DEBUG("Checksums: expected: ~p, pkg: ~p, meta: ~p",
                   [_Hash, _Bin, _Tar]),
            {bad_checksum, CachePath}
    end.

-spec serve_from_download(TmpDir, CachePath, Package, ETag, Binary, State,
                          ETagPath) -> Res when
      TmpDir :: file:name(),
      CachePath :: file:name(),
      Package :: {pkg, Name :: binary(), Vsn :: binary(), Hash :: binary(), RepoConfig :: hex_core:config()},
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
      Pkg :: {pkg, Name :: binary(), Vsn :: binary(), Hash :: binary(), RepoConfig :: hex_core:config()},
      Files :: list({file:name(), binary()}),
      Contents :: binary(),
      Version :: binary(),
      Meta :: binary(),
      State :: rebar_state:t(),
      Res :: {Hash, BinChecksum, TarChecksum},
      Hash :: binary(),
      BinChecksum :: binary(),
      TarChecksum :: binary().
checksums({pkg, _Name, _Vsn, Hash, _}, Files, Contents, Version, Meta, _State) ->
    Blob = <<Version/binary, Meta/binary, Contents/binary>>,
    <<X:256/big-unsigned>> = crypto:hash(sha256, Blob),
    BinChecksum = list_to_binary(
                    rebar_string:uppercase(
                      lists:flatten(io_lib:format("~64.16.0b", [X])))),
    {"CHECKSUM", TarChecksum} = lists:keyfind("CHECKSUM", 1, Files),
    {Hash, BinChecksum, TarChecksum}.

-spec maybe_store_etag_in_cache(UpdateETag, Path, ETag) -> Res when
      UpdateETag :: boolean(),
      Path :: file:name(),
      ETag :: binary(),
      Res :: ok.
maybe_store_etag_in_cache(false = _UpdateETag, _Path, _ETag) ->
    ok;
maybe_store_etag_in_cache(true = _UpdateETag, Path, ETag) ->
    store_etag_in_cache(Path, ETag).
