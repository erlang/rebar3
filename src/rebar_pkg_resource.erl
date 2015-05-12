%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
-module(rebar_pkg_resource).

-behaviour(rebar_resource).

-export([lock/2
        ,download/3
        ,needs_update/2
        ,make_vsn/1]).

-include_lib("providers/include/providers.hrl").
-include("rebar.hrl").

-define(DEFAULT_CDN, "https://s3.amazonaws.com/s3.hex.pm/tarballs").

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
    PackageDir = hex_package_dir(CDN, State),
    Package = binary_to_list(<<Name/binary, "-", Vsn/binary, ".tar">>),
    CachePath = filename:join(PackageDir, Package),
    Url = string:join([CDN, Package], "/"),
    cached_download(TmpDir, CachePath, Pkg, Url, etag(CachePath), State).

cached_download(TmpDir, CachePath, Pkg, Url, ETag, State) ->
    case request(Url, ETag) of
        {ok, cached} ->
            serve_from_cache(TmpDir, CachePath, Pkg, State);
        {ok, Body, NewETag} ->
            serve_from_download(TmpDir, CachePath, Pkg, NewETag, Body, State);
        error when ETag =/= false ->
            ?DEBUG("Download ~s error, using ~s from cache", [Url, CachePath]),
            serve_from_cache(TmpDir, CachePath, Pkg, State);
        error ->
            throw(request_failed)
    end.

serve_from_cache(TmpDir, CachePath, Pkg, State) ->
    {Files, Contents, Version, Meta} = extract(TmpDir, CachePath),
    case checksums(Pkg, Files, Contents, Version, Meta, State) of
        {Chk, Chk, Chk} ->
            ok = erl_tar:extract({binary, Contents}, [{cwd, TmpDir}, compressed]),
            {ok, true};
        {_Bin, Chk, Chk} ->
            ?PRV_ERROR({failed_extract, CachePath});
        {Chk, _Reg, Chk} ->
            ?PRV_ERROR({bad_registry_checksum, CachePath});
        {_Bin, _Reg, _Tar} ->
            ?PRV_ERROR({bad_checksum, CachePath})
    end.

serve_from_download(TmpDir, CachePath, Package, ETag, Binary, State) ->
    ?DEBUG("Writing ~p to cache at ~s", [Package, CachePath]),
    file:write_file(CachePath, Binary),
    case etag(CachePath) of
        ETag ->
            serve_from_cache(TmpDir, CachePath, Package, State);
        FileETag ->
            ?DEBUG("Download ETag ~s doesn't match cached ETag ~s", [ETag, FileETag]),
            ?PRV_ERROR({bad_download, CachePath})
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
    RegistryChecksum = rebar_packages:registry_sum(Pkg, State),
    {"CHECKSUM", TarChecksum} = lists:keyfind("CHECKSUM", 1, Files),
    {BinChecksum, RegistryChecksum, TarChecksum}.

make_vsn(_) ->
    {error, "Replacing version of type pkg not supported."}.

%% Use the shared hex package directory unless a non-default package repo is used
hex_package_dir(?DEFAULT_CDN, _) ->
    filename:join([rebar_dir:home_dir(), ".hex", "packages"]);
hex_package_dir(CDN, State) ->
    CacheDir = rebar_dir:global_cache_dir(State),
    {ok, {_, _, Host, _, _, _}} = http_uri:parse(CDN),
    CDNPath = filename:join(lists:reverse(string:tokens(Host, "."))),
    PackageDir = filename:join([CacheDir, "hex", CDNPath, "packages"]),
    ok = filelib:ensure_dir(filename:join(PackageDir, "placeholder")),
    PackageDir.

request(Url, ETag) ->
    case httpc:request(get, {Url, [{"if-none-match", ETag} || ETag =/= false]},
                      [{relaxed, true}],
                      [{body_format, binary}]) of
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
