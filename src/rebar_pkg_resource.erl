%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
-module(rebar_pkg_resource).

-behaviour(rebar_resource).

-export([lock/2
        ,download/3
        ,needs_update/2
        ,make_vsn/1]).

-export([ssl_opts/1]).

-include("rebar.hrl").
-include_lib("public_key/include/OTP-PUB-KEY.hrl").

lock(_AppDir, Source) ->
    Source.

needs_update(Dir, {pkg, _Name, Vsn}) ->
    [AppInfo] = rebar_app_discover:find_apps([Dir], all),
    case rebar_app_info:original_vsn(AppInfo) =:= ec_cnv:to_list(Vsn) of
        true ->
            false;
        false ->
            true
    end.

download(TmpDir, Pkg={pkg, Name, Vsn}, State) ->
    CDN = rebar_state:get(State, rebar_packages_cdn, ?DEFAULT_CDN),
    PackageDir = rebar_packages:package_dir(State),
    Package = binary_to_list(<<Name/binary, "-", Vsn/binary, ".tar">>),
    CachePath = filename:join(PackageDir, Package),
    Url = string:join([CDN, Package], "/"),
    cached_download(TmpDir, CachePath, Pkg, Url, etag(CachePath), State).

cached_download(TmpDir, CachePath, Pkg={pkg, Name, Vsn}, Url, ETag, State) ->
    case request(Url, ETag) of
        {ok, cached} ->
            serve_from_cache(TmpDir, CachePath, Pkg, State);
        {ok, Body, NewETag} ->
            serve_from_download(TmpDir, CachePath, Pkg, NewETag, Body, State);
        error when ETag =/= false ->
            ?DEBUG("Download ~s error, using ~s from cache", [Url, CachePath]),
            serve_from_cache(TmpDir, CachePath, Pkg, State);
        error ->
            {fetch_fail, Name, Vsn}
    end.

serve_from_cache(TmpDir, CachePath, Pkg, State) ->
    {Files, Contents, Version, Meta} = extract(TmpDir, CachePath),
    case checksums(Pkg, Files, Contents, Version, Meta, State) of
        {Chk, Chk, Chk} ->
            ok = erl_tar:extract({binary, Contents}, [{cwd, TmpDir}, compressed]),
            {ok, true};
        {_Bin, Chk, Chk} ->
            ?DEBUG("Checksums: registry: ~p, pkg: ~p", [Chk, _Bin]),
            {failed_extract, CachePath};
        {Chk, _Reg, Chk} ->
            ?DEBUG("Checksums: registry: ~p, pkg: ~p", [_Reg, Chk]),
            {bad_registry_checksum, CachePath};
        {_Bin, _Reg, _Tar} ->
            ?DEBUG("Checksums: registry: ~p, pkg: ~p, meta: ~p", [_Reg, _Bin, _Tar]),
            {bad_checksum, CachePath}
    end.

serve_from_download(TmpDir, CachePath, Package, ETag, Binary, State) ->
    ?DEBUG("Writing ~p to cache at ~s", [Package, CachePath]),
    file:write_file(CachePath, Binary),
    case etag(CachePath) of
        ETag ->
            serve_from_cache(TmpDir, CachePath, Package, State);
        FileETag ->
            ?DEBUG("Downloaded file ~s ETag ~s doesn't match returned ETag ~s", [CachePath, ETag, FileETag]),
            {bad_download, CachePath}
    end.


extract(TmpDir, CachePath) ->
    ec_file:mkdir_p(TmpDir),
    {ok, Files} = erl_tar:extract(CachePath, [memory]),
    {"contents.tar.gz", Contents} = lists:keyfind("contents.tar.gz", 1, Files),
    {"VERSION", Version} = lists:keyfind("VERSION", 1, Files),
    {"metadata.config", Meta} = lists:keyfind("metadata.config", 1, Files),
    {Files, Contents, Version, Meta}.

checksums(Pkg, Files, Contents, Version, Meta, State) ->
    Blob = <<Version/binary, Meta/binary, Contents/binary>>,
    <<X:256/big-unsigned>> = crypto:hash(sha256, Blob),
    BinChecksum = list_to_binary(string:to_upper(lists:flatten(io_lib:format("~64.16.0b", [X])))),
    RegistryChecksum = rebar_packages:registry_checksum(Pkg, State),
    {"CHECKSUM", TarChecksum} = lists:keyfind("CHECKSUM", 1, Files),
    {BinChecksum, RegistryChecksum, TarChecksum}.

make_vsn(_) ->
    {error, "Replacing version of type pkg not supported."}.

request(Url, ETag) ->
    case httpc:request(get, {Url, [{"if-none-match", ETag} || ETag =/= false]},
                       [{ssl, ssl_opts(Url)}, {relaxed, true}],
                       [{body_format, binary}],
                       rebar) of
        {ok, {{_Version, 200, _Reason}, Headers, Body}} ->
            ?DEBUG("Successfully downloaded ~s", [Url]),
            {"etag", ETag1} = lists:keyfind("etag", 1, Headers),
            {ok, Body, string:strip(ETag1, both, $")};
        {ok, {{_Version, 304, _Reason}, _Headers, _Body}} ->
            ?DEBUG("Cached copy of ~s still valid", [Url]),
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
            string:to_lower(lists:flatten(io_lib:format("~32.16.0b", [X])));
        {error, _} ->
            false
    end.

ssl_opts(Url) ->
    case check_ssl_version() of
        true ->
            {ok, {_, _, Hostname, _, _, _}} = http_uri:parse(ec_cnv:to_list(Url)),
            VerifyFun = {fun ssl_verify_hostname:verify_fun/3, [{check_hostname, Hostname}]},
            CACerts = cacerts(),
            [{verify, verify_peer}, {depth, 2}, {cacerts, CACerts}
            ,{partial_chain, fun partial_chain/1}, {verify_fun, VerifyFun}];
        false ->
            ?WARN("Insecure HTTPS request (peer verification disabled), please update to OTP 17.4 or later", []),
            [{verify, verify_none}]
    end.

partial_chain(Certs) ->
    Certs1 = [{Cert, public_key:pkix_decode_cert(Cert, otp)} || Cert <- Certs],
    CACerts = cacerts(),
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

cacerts() ->
    Pems = public_key:pem_decode(rebar_cacerts:cacerts()),
    [Der || {'Certificate', Der, _} <- Pems].

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

parse_vsn(Vsn) ->
    version_pad(string:tokens(Vsn, ".")).

version_pad([Major]) ->
    {list_to_integer(Major), 0, 0};
version_pad([Major, Minor]) ->
    {list_to_integer(Major), list_to_integer(Minor), 0};
version_pad([Major, Minor, Patch]) ->
    {list_to_integer(Major), list_to_integer(Minor), list_to_integer(Patch)};
version_pad([Major, Minor, Patch | _]) ->
    {list_to_integer(Major), list_to_integer(Minor), list_to_integer(Patch)}.
