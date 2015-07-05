%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
-module(rebar_pkg_resource).

-behaviour(rebar_resource).

-export([lock/2
        ,download/3
        ,needs_update/2
        ,make_vsn/1]).

-include("rebar.hrl").

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
            request_failed
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
                       [{relaxed, true}],
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
