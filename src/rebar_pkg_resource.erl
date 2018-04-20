%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
-module(rebar_pkg_resource).

-behaviour(rebar_resource).

-export([lock/2
        ,download/3
        ,download/4
        ,needs_update/2
        ,make_vsn/1]).

-export([request/2
        ,etag/1
        ,ssl_opts/1]).

%% Exported for ct
-export([store_etag_in_cache/2]).

-include("rebar.hrl").
-include_lib("public_key/include/OTP-PUB-KEY.hrl").

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
    case rebar_app_info:original_vsn(AppInfo) =:= rebar_utils:to_list(Vsn) of
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
    CDN = rebar_state:get(State, rebar_packages_cdn, ?DEFAULT_CDN),
    {ok, PackageDir} = rebar_packages:package_dir(State),
    Package = binary_to_list(<<Name/binary, "-", Vsn/binary, ".tar">>),
    ETagFile = binary_to_list(<<Name/binary, "-", Vsn/binary, ".etag">>),
    CachePath = filename:join(PackageDir, Package),
    ETagPath = filename:join(PackageDir, ETagFile),
    case rebar_utils:url_append_path(CDN, filename:join(?REMOTE_PACKAGE_DIR,
                                                        Package)) of
        {ok, Url} ->
            cached_download(TmpDir, CachePath, Pkg, Url, etag(ETagPath), State,
                            ETagPath, UpdateETag);
        _ ->
            {fetch_fail, Name, Vsn}
    end.

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
-spec request(Url, ETag) -> Res when
      Url :: string(),
      ETag :: false | string(),
      Res :: 'error' | {ok, cached} | {ok, any(), string()}.
request(Url, ETag) ->
    HttpOptions = [{ssl, ssl_opts(Url)},
                   {relaxed, true} | rebar_utils:get_proxy_auth()],
    case httpc:request(get, {Url, [{"if-none-match", "\"" ++ ETag ++ "\""}
                                   || ETag =/= false] ++
                             [{"User-Agent", rebar_utils:user_agent()}]},
                       HttpOptions, [{body_format, binary}], rebar) of
        {ok, {{_Version, 200, _Reason}, Headers, Body}} ->
            ?DEBUG("Successfully downloaded ~ts", [Url]),
            {"etag", ETag1} = lists:keyfind("etag", 1, Headers),
            {ok, Body, rebar_string:trim(ETag1, both, [$"])};
        {ok, {{_Version, 304, _Reason}, _Headers, _Body}} ->
            ?DEBUG("Cached copy of ~ts still valid", [Url]),
            {ok, cached};
        {ok, {{_Version, Code, _Reason}, _Headers, _Body}} ->
            ?DEBUG("Request to ~p failed: status code ~p", [Url, Code]),
            error;
        {error, Reason} ->
            ?DEBUG("Request to ~p failed: ~p", [Url, Reason]),
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
      Res :: false | string().
etag(Path) ->
    case file:read_file(Path) of
        {ok, Bin} ->
            binary_to_list(Bin);
        {error, _} ->
            false
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Return the SSL options adequate for the project based on
%% its configuration, including for validation of certs.
%% @end
%%------------------------------------------------------------------------------
-spec ssl_opts(Url) -> Res when
      Url :: string(),
      Res :: proplists:proplist().
ssl_opts(Url) ->
    case get_ssl_config() of
        ssl_verify_enabled ->
            ssl_opts(ssl_verify_enabled, Url);
        ssl_verify_disabled ->
            [{verify, verify_none}]
    end.

%%------------------------------------------------------------------------------
%% @doc
%% Store the given etag in the .cache folder. The name is pakckage-vsn.etag.
%% @end
%%------------------------------------------------------------------------------
-spec store_etag_in_cache(File, ETag) -> Res when
      File :: file:name(),
      ETag :: string(),
      Res :: ok.
store_etag_in_cache(Path, ETag) ->
    _ = file:write_file(Path, ETag).

%%%=============================================================================
%%% Private functions
%%%=============================================================================
-spec cached_download(TmpDir, CachePath, Pkg, Url, ETag, State, ETagPath,
                      UpdateETag) -> Res when
      TmpDir :: file:name(),
      CachePath :: file:name(),
      Pkg :: {pkg, Name :: binary(), Vsn :: binary(), Hash :: binary()},
      Url :: string(),
      ETag :: false | string(),
      State :: rebar_state:t(),
      ETagPath :: file:name(),
      UpdateETag :: boolean(),
      Res :: download_result().
cached_download(TmpDir, CachePath, Pkg={pkg, Name, Vsn, _Hash}, Url, ETag,
                State, ETagPath, UpdateETag) ->
    case request(Url, ETag) of
        {ok, cached} ->
            ?INFO("Version cached at ~ts is up to date, reusing it", [CachePath]),
            serve_from_cache(TmpDir, CachePath, Pkg, State);
        {ok, Body, NewETag} ->
            ?INFO("Downloaded package, caching at ~ts", [CachePath]),
            maybe_store_etag_in_cache(UpdateETag, ETagPath, NewETag),
            serve_from_download(TmpDir, CachePath, Pkg, NewETag, Body, State,
                                ETagPath);
        error when ETag =/= false ->
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
      ETag :: string(),
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
checksums(Pkg={pkg, _Name, _Vsn, Hash}, Files, Contents, Version, Meta, State) ->
    Blob = <<Version/binary, Meta/binary, Contents/binary>>,
    <<X:256/big-unsigned>> = crypto:hash(sha256, Blob),
    BinChecksum = list_to_binary(
                    rebar_string:uppercase(
                      lists:flatten(io_lib:format("~64.16.0b", [X])))),
    RegistryChecksum = rebar_packages:registry_checksum(Pkg, State),
    {"CHECKSUM", TarChecksum} = lists:keyfind("CHECKSUM", 1, Files),
    {Hash, BinChecksum, RegistryChecksum, TarChecksum}.

%%------------------------------------------------------------------------------
%% @doc
%% Return the SSL options adequate for the project based on
%% its configuration, including for validation of certs.
%% @end
%%------------------------------------------------------------------------------
-spec ssl_opts(Enabled, Url) -> Res when
      Enabled :: atom(),
      Url :: string(),
      Res :: proplists:proplist().
ssl_opts(ssl_verify_enabled, Url) ->
    case check_ssl_version() of
        true ->
            {ok, {_, _, Hostname, _, _, _}} =
                http_uri:parse(rebar_utils:to_list(Url)),
            VerifyFun = {fun ssl_verify_hostname:verify_fun/3,
                         [{check_hostname, Hostname}]},
            CACerts = certifi:cacerts(),
            [{verify, verify_peer}, {depth, 2}, {cacerts, CACerts},
             {partial_chain, fun partial_chain/1}, {verify_fun, VerifyFun}];
        false ->
            ?WARN("Insecure HTTPS request (peer verification disabled), "
                  "please update to OTP 17.4 or later", []),
            [{verify, verify_none}]
    end.

-spec partial_chain(Certs) -> Res when
      Certs :: list(any()),
      Res :: unknown_ca | {trusted_ca, any()}.
partial_chain(Certs) ->
    Certs1 = [{Cert, public_key:pkix_decode_cert(Cert, otp)} || Cert <- Certs],
    CACerts = certifi:cacerts(),
    CACerts1 = [public_key:pkix_decode_cert(Cert, otp) || Cert <- CACerts],
    case ec_lists:find(fun({_, Cert}) ->
                               check_cert(CACerts1, Cert)
                       end, Certs1) of
        {ok, Trusted} ->
            {trusted_ca, element(1, Trusted)};
        _ ->
            unknown_ca
    end.

-spec extract_public_key_info(Cert) -> Res when
      Cert :: #'OTPCertificate'{tbsCertificate::#'OTPTBSCertificate'{}},
      Res :: any().
extract_public_key_info(Cert) ->
    ((Cert#'OTPCertificate'.tbsCertificate)#'OTPTBSCertificate'.subjectPublicKeyInfo).

-spec check_cert(CACerts, Cert) -> Res when
      CACerts :: list(any()),
      Cert :: any(),
      Res :: boolean().
check_cert(CACerts, Cert) ->
    lists:any(fun(CACert) ->
                      extract_public_key_info(CACert) == extract_public_key_info(Cert)
              end, CACerts).

-spec check_ssl_version() ->
    boolean().
check_ssl_version() ->
    case application:get_key(ssl, vsn) of
        {ok, Vsn} ->
            parse_vsn(Vsn) >= {5, 3, 6};
        _ ->
            false
    end.

-spec get_ssl_config() ->
      ssl_verify_disabled | ssl_verify_enabled.
get_ssl_config() ->
    GlobalConfigFile = rebar_dir:global_config(),
    Config = rebar_config:consult_file(GlobalConfigFile),
    case proplists:get_value(ssl_verify, Config, []) of
        false ->
            ssl_verify_disabled;
        _ ->
            ssl_verify_enabled
    end.

-spec parse_vsn(Vsn) -> Res when
      Vsn :: string(),
      Res :: {integer(), integer(), integer()}.
parse_vsn(Vsn) ->
    version_pad(rebar_string:lexemes(Vsn, ".-")).

-spec version_pad(list(nonempty_string())) -> Res when
      Res :: {integer(), integer(), integer()}.
version_pad([Major]) ->
    {list_to_integer(Major), 0, 0};
version_pad([Major, Minor]) ->
    {list_to_integer(Major), list_to_integer(Minor), 0};
version_pad([Major, Minor, Patch]) ->
    {list_to_integer(Major), list_to_integer(Minor), list_to_integer(Patch)};
version_pad([Major, Minor, Patch | _]) ->
    {list_to_integer(Major), list_to_integer(Minor), list_to_integer(Patch)}.

-spec maybe_store_etag_in_cache(UpdateETag, Path, ETag) -> Res when
      UpdateETag :: boolean(),
      Path :: file:name(),
      ETag :: string(),
      Res :: ok.
maybe_store_etag_in_cache(false = _UpdateETag, _Path, _ETag) ->
    ok;
maybe_store_etag_in_cache(true = _UpdateETag, Path, ETag) ->
    store_etag_in_cache(Path, ETag).
