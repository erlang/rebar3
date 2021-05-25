%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
-module(rebar_pkg_resource).

-behaviour(rebar_resource_v2).

-export([init/2,
         lock/2,
         download/4,
         download/5,
         needs_update/2,
         make_vsn/2,
         format_error/1]).

-ifdef(TEST).
%% exported for test purposes
-export([store_etag_in_cache/2]).
-endif.

-include("rebar.hrl").
-include_lib("providers/include/providers.hrl").

-type package() :: {pkg, binary(), binary(), binary(), rebar_hex_repos:repo()}.

%%==============================================================================
%% Public API
%%==============================================================================

-spec init(atom(), rebar_state:t()) -> {ok, rebar_resource_v2:resource()}.
init(Type, State) ->
    {ok, Vsn} = application:get_key(rebar, vsn),
    BaseConfig = #{http_adapter => rebar_httpc_adapter,
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
    case rebar_utils:to_binary(rebar_app_info:original_vsn(AppInfo)) =:= rebar_utils:to_binary(Vsn) of
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
      Res :: ok | {error,_}.
download(TmpDir, AppInfo, State, ResourceState) ->
    case download(TmpDir, rebar_app_info:source(AppInfo), State, ResourceState, true) of
        ok ->
            ok;
        Error ->
            {error, Error}
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Download the given pkg. The etag belonging to the pkg file will be updated
%% only if the UpdateEtag is true and the ETag returned from the hexpm server
%% is different.
%% @end
%%------------------------------------------------------------------------------
-spec download(TmpDir, Pkg, State, ResourceState, UpdateETag) -> Res when
      TmpDir :: file:name(),
      Pkg :: package(),
      State :: rebar_state:t(),
      ResourceState:: rebar_resource_v2:resource_state(),
      UpdateETag :: boolean(),
      Res :: ok | {error,_} | {unexpected_hash, string(), integer(), integer()} |
             {fetch_fail, binary(), binary()}.
download(TmpDir, Pkg={pkg, Name, Vsn, _Hash, Repo}, State, _ResourceState, UpdateETag) ->
    {ok, PackageDir} = rebar_packages:package_dir(Repo, State),
    Package = binary_to_list(<<Name/binary, "-", Vsn/binary, ".tar">>),
    ETagFile = binary_to_list(<<Name/binary, "-", Vsn/binary, ".etag">>),
    CachePath = filename:join(PackageDir, Package),
    ETagPath = filename:join(PackageDir, ETagFile),
    case cached_download(TmpDir, CachePath, Pkg, etag(CachePath, ETagPath), ETagPath, UpdateETag) of
        {bad_registry_checksum, Expected, Found} ->
            %% checksum comparison failed. in case this is from a modified cached package
            %% overwrite the etag if it exists so it is not relied on again
            store_etag_in_cache(ETagPath, <<>>),
            ?PRV_ERROR({bad_registry_checksum, Name, Vsn, Expected, Found});
        Result ->
            Result
    end.

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

format_error({bad_registry_checksum, Name, Vsn, Expected, Found}) ->
    io_lib:format("The checksum for package at ~ts-~ts (~ts) does not match the "
                  "checksum expected from the registry (~ts). "
                  "Run `rebar3 do unlock ~ts, update` and then try again.",
                  [Name, Vsn, Found, Expected, Name]).

%%------------------------------------------------------------------------------
%% @doc
%% Download the pkg belonging to the given address. If the etag of the pkg
%% is the same what we stored in the etag file previously return {ok, cached},
%% if the file has changed (so the etag is not the same anymore) return
%% {ok, Contents, NewEtag}, otherwise if some error occured return error.
%% @end
%%------------------------------------------------------------------------------
-spec request(rebar_hex_repos:repo(), binary(), binary(), false | binary())
             -> {ok, cached} | {ok, binary(), binary()} | error.
request(Config, Name, Version, ETag) ->
    Config1 = Config#{http_etag => ETag},
    try r3_hex_repo:get_tarball(Config1, Name, Version) of
        {ok, {200, #{<<"etag">> := ETag1}, Tarball}} ->
            {ok, Tarball, ETag1};
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
-spec etag(PackagePath, ETagPath) -> Res when
      PackagePath :: file:name(),
      ETagPath :: file:name(),
      Res :: binary().
etag(PackagePath, ETagPath) ->
    case file:read_file(ETagPath) of
        {ok, Bin} ->
            %% just in case a user deleted a cached package but not its etag
            %% verify the package is also there, and if not, ignore the etag
            case filelib:is_file(PackagePath) of
                true ->
                    Bin;
                false ->
                    <<>>
            end;
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
-spec cached_download(TmpDir, CachePath, Pkg, ETag, ETagPath, UpdateETag) -> Res when
      TmpDir :: file:name(),
      CachePath :: file:name(),
      Pkg :: package(),
      ETag :: binary(),
      ETagPath :: file:name(),
      UpdateETag :: boolean(),
      Res :: ok | {unexpected_hash, integer(), integer()} | {fetch_fail, binary(), binary()}.
cached_download(TmpDir, CachePath, Pkg={pkg, Name, Vsn, _Hash, RepoConfig}, ETag,
                ETagPath, UpdateETag) ->
    case request(RepoConfig, Name, Vsn, ETag) of
        {ok, cached} ->
            ?INFO("Version cached at ~ts is up to date, reusing it", [CachePath]),
            serve_from_cache(TmpDir, CachePath, Pkg);
        {ok, Body, NewETag} ->
            ?INFO("Downloaded package, caching at ~ts", [CachePath]),
            maybe_store_etag_in_cache(UpdateETag, ETagPath, NewETag),
            serve_from_download(TmpDir, CachePath, Pkg, Body);
        error when ETag =/= <<>> ->
            store_etag_in_cache(ETagPath, ETag),
            ?INFO("Download error, using cached file at ~ts", [CachePath]),
            serve_from_cache(TmpDir, CachePath, Pkg);
        error ->
            {fetch_fail, Name, Vsn}
    end.

-spec serve_from_cache(TmpDir, CachePath, Pkg) -> Res when
      TmpDir :: file:name(),
      CachePath :: file:name(),
      Pkg :: package(),
      Res :: ok | {error,_} | {bad_registry_checksum, integer(), integer()}.
serve_from_cache(TmpDir, CachePath, Pkg) ->
    {ok, Binary} = file:read_file(CachePath),
    serve_from_memory(TmpDir, Binary, Pkg).

-spec serve_from_memory(TmpDir, Tarball, Package) -> Res when
      TmpDir :: file:name(),
      Tarball :: binary(),
      Package :: package(),
      Res :: ok | {error,_} | {bad_registry_checksum, integer(), integer()}.
serve_from_memory(TmpDir, Binary, {pkg, _Name, _Vsn, Hash, _RepoConfig}) ->
    RegistryChecksum = list_to_integer(binary_to_list(Hash), 16),
    case r3_hex_tarball:unpack(Binary, TmpDir) of
        {ok, #{checksum := <<Checksum:256/big-unsigned>>}} when RegistryChecksum =/= Checksum ->
            ?DEBUG("Expected hash ~64.16.0B does not match checksum of fetched package ~64.16.0B",
                   [RegistryChecksum, Checksum]),
            {bad_registry_checksum, RegistryChecksum, Checksum};
        {ok, #{checksum := <<RegistryChecksum:256/big-unsigned>>}} ->
            ok;
        {error, Reason} ->
            {error, {hex_tarball, Reason}}
    end.

-spec serve_from_download(TmpDir, CachePath, Package, Binary) -> Res when
      TmpDir :: file:name(),
      CachePath :: file:name(),
      Package :: package(),
      Binary :: binary(),
      Res :: ok | {error,_}.
serve_from_download(TmpDir, CachePath, Package, Binary) ->
    ?DEBUG("Writing ~p to cache at ~ts", [Package, CachePath]),
    file:write_file(CachePath, Binary),
    serve_from_memory(TmpDir, Binary, Package).

-spec maybe_store_etag_in_cache(UpdateETag, Path, ETag) -> Res when
      UpdateETag :: boolean(),
      Path :: file:name(),
      ETag :: binary(),
      Res :: ok.
maybe_store_etag_in_cache(false = _UpdateETag, _Path, _ETag) ->
    ok;
maybe_store_etag_in_cache(true = _UpdateETag, Path, ETag) ->
    store_etag_in_cache(Path, ETag).
