%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
-module(rebar_pkg_resource).

-behaviour(rebar_resource).

-export([lock/2
        ,download/3
        ,needs_update/2
        ,make_vsn/1]).

-export([request/2
        ,etag/1
        ,ssl_opts/1]).

-include("rebar.hrl").
-include_lib("public_key/include/OTP-PUB-KEY.hrl").

lock(_AppDir, Source) ->
    Source.

needs_update(Dir, {pkg, _Name, Vsn, _Hash}) ->
    [AppInfo] = rebar_app_discover:find_apps([Dir], all),
    case rebar_app_info:original_vsn(AppInfo) =:= rebar_utils:to_list(Vsn) of
        true ->
            false;
        false ->
            true
    end.

download(TmpDir, Pkg={pkg, Name, Vsn, _Hash}, State) ->
    CDN = rebar_state:get(State, rebar_packages_cdn, ?DEFAULT_CDN),
    {ok, PackageDir} = rebar_packages:package_dir(State),
    Package = binary_to_list(<<Name/binary, "-", Vsn/binary, ".tar">>),
    CachePath = filename:join(PackageDir, Package),
    case rebar_utils:url_append_path(CDN, filename:join(?REMOTE_PACKAGE_DIR, Package)) of
        {ok, Url} ->
            cached_download(TmpDir, CachePath, Pkg, Url, etag(CachePath), State);
        _ ->
            {fetch_fail, Name, Vsn}
    end.

cached_download(TmpDir, CachePath, Pkg={pkg, Name, Vsn, _Hash}, Url, ETag, State) ->
    case request(Url, ETag) of
        {ok, cached} ->
            ?INFO("Version cached at ~ts is up to date, reusing it", [CachePath]),
            serve_from_cache(TmpDir, CachePath, Pkg, State);
        {ok, Body, NewETag} ->
            ?INFO("Downloaded package, caching at ~ts", [CachePath]),
            serve_from_download(TmpDir, CachePath, Pkg, NewETag, Body, State);
        error when ETag =/= false ->
            ?INFO("Download error, using cached file at ~ts", [CachePath]),
            serve_from_cache(TmpDir, CachePath, Pkg, State);
        error ->
            {fetch_fail, Name, Vsn}
    end.

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
            ?DEBUG("Checksums: expected: ~p, registry: ~p, pkg: ~p, meta: ~p", [_Hash, _Reg, _Bin, _Tar]),
            {bad_checksum, CachePath}
    end.

serve_from_download(TmpDir, CachePath, Package, ETag, Binary, State) ->
    ?DEBUG("Writing ~p to cache at ~ts", [Package, CachePath]),
    file:write_file(CachePath, Binary),
    case etag(CachePath) of
        ETag ->
            serve_from_cache(TmpDir, CachePath, Package, State);
        FileETag ->
            ?DEBUG("Downloaded file ~ts ETag ~ts doesn't match returned ETag ~ts", [CachePath, ETag, FileETag]),
            {bad_download, CachePath}
    end.


extract(TmpDir, CachePath) ->
    ec_file:mkdir_p(TmpDir),
    {ok, Files} = erl_tar:extract(CachePath, [memory]),
    {"contents.tar.gz", Contents} = lists:keyfind("contents.tar.gz", 1, Files),
    {"VERSION", Version} = lists:keyfind("VERSION", 1, Files),
    {"metadata.config", Meta} = lists:keyfind("metadata.config", 1, Files),
    {Files, Contents, Version, Meta}.

checksums(Pkg={pkg, _Name, _Vsn, Hash}, Files, Contents, Version, Meta, State) ->
    Blob = <<Version/binary, Meta/binary, Contents/binary>>,
    <<X:256/big-unsigned>> = crypto:hash(sha256, Blob),
    BinChecksum = list_to_binary(rebar_string:uppercase(lists:flatten(io_lib:format("~64.16.0b", [X])))),
    RegistryChecksum = rebar_packages:registry_checksum(Pkg, State),
    {"CHECKSUM", TarChecksum} = lists:keyfind("CHECKSUM", 1, Files),
    {Hash, BinChecksum, RegistryChecksum, TarChecksum}.

make_vsn(_) ->
    {error, "Replacing version of type pkg not supported."}.

request(Url, ETag) ->
    HttpOptions = [{ssl, ssl_opts(Url)}, {relaxed, true} | rebar_utils:get_proxy_auth()],

    case httpc:request(get, {Url, [{"if-none-match", ETag} || ETag =/= false]++[{"User-Agent", rebar_utils:user_agent()}]},
                       HttpOptions,
                       [{body_format, binary}],
                       rebar) of
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

etag(Path) ->
    case file:read_file(Path) of
        {ok, Binary} ->
            <<X:128/big-unsigned-integer>> = crypto:hash(md5, Binary),
            rebar_string:lowercase(lists:flatten(io_lib:format("~32.16.0b", [X])));
        {error, _} ->
            false
    end.

%% @doc returns the SSL options adequate for the project based on
%% its configuration, including for validation of certs.
-spec ssl_opts(string()) -> [term()].
ssl_opts(Url) ->
    case get_ssl_config() of
        ssl_verify_enabled ->
            ssl_opts(ssl_verify_enabled, Url);
        ssl_verify_disabled ->
            [{verify, verify_none}]
    end.

%% @doc returns the SSL options adequate for the project based on
%% its configuration, including for validation of certs.
-spec ssl_opts(atom(), string()) -> [term()].
ssl_opts(ssl_verify_enabled, Url) ->
    case check_ssl_version() of
        true ->
            {ok, {_, _, Hostname, _, _, _}} = http_uri:parse(rebar_utils:to_list(Url)),
            VerifyFun = {fun ssl_verify_hostname:verify_fun/3, [{check_hostname, Hostname}]},
            CACerts = certifi:cacerts(),
            [{verify, verify_peer}, {depth, 2}, {cacerts, CACerts}
            ,{partial_chain, fun partial_chain/1}, {verify_fun, VerifyFun}];
        false ->
            ?WARN("Insecure HTTPS request (peer verification disabled), please update to OTP 17.4 or later", []),
            [{verify, verify_none}]
    end.

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

extract_public_key_info(Cert) ->
    ((Cert#'OTPCertificate'.tbsCertificate)#'OTPTBSCertificate'.subjectPublicKeyInfo).

check_cert(CACerts, Cert) ->
    lists:any(fun(CACert) ->
                      extract_public_key_info(CACert) == extract_public_key_info(Cert)
              end, CACerts).

check_ssl_version() ->
    case application:get_key(ssl, vsn) of
        {ok, Vsn} ->
            parse_vsn(Vsn) >= {5, 3, 6};
        _ ->
            false
    end.

get_ssl_config() ->
    GlobalConfigFile = rebar_dir:global_config(),
    Config = rebar_config:consult_file(GlobalConfigFile),
    case proplists:get_value(ssl_verify, Config, []) of
        false ->
            ssl_verify_disabled;
        _ ->
            ssl_verify_enabled
    end.

parse_vsn(Vsn) ->
    version_pad(rebar_string:lexemes(Vsn, ".-")).

version_pad([Major]) ->
    {list_to_integer(Major), 0, 0};
version_pad([Major, Minor]) ->
    {list_to_integer(Major), list_to_integer(Minor), 0};
version_pad([Major, Minor, Patch]) ->
    {list_to_integer(Major), list_to_integer(Minor), list_to_integer(Patch)};
version_pad([Major, Minor, Patch | _]) ->
    {list_to_integer(Major), list_to_integer(Minor), list_to_integer(Patch)}.
