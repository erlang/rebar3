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

download(_Dir, {pkg, Name, Vsn}, State) ->
    CDN = rebar_state:get(State, rebar_packages_cdn, ?DEFAULT_CDN),
    PackageDir = hex_package_dir(CDN, State),

    Package = binary_to_list(<<Name/binary, "-", Vsn/binary, ".tar">>),
    Path = filename:join(PackageDir, Package),
    Url = string:join([CDN, Package], "/"),

    case request(Url, etag(Path)) of
        {ok, cached} ->
            {tarball, Path};
        {ok, Binary} ->
            file:write_file(Path, Binary),
            {tarball, Path};
        error ->
            case filelib:is_regular(Path) of
                true ->
                    ?DEBUG("Download ~s error, using ~s since it exists", [Url, Path]),
                    {tarball, Path};
                false ->
                    throw(request_failed)
            end
    end.

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
        {ok, {{_Version, 200, _Reason}, _Headers, Body}} ->
            ?DEBUG("Successfully downloaded ~s", [Url]),
            {ok, Body};
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
