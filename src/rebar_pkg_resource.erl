%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
-module(rebar_pkg_resource).

-behaviour(rebar_resource).

-export([lock/2
        ,download/2
        ,needs_update/2
        ,make_vsn/1]).

-include("rebar.hrl").

lock(_AppDir, Source) ->
    Source.

needs_update(Dir, {pkg, _Name, Vsn, _Url}) ->
    [AppInfo] = rebar_app_discover:find_apps([Dir], all),
    case rebar_app_info:original_vsn(AppInfo) =:= ec_cnv:to_list(Vsn) of
        true ->
            false;
        false ->
            true
    end.

download(Dir, {pkg, _Name, _Vsn, Url}) ->
    TmpFile = filename:join(Dir, "package.tar.gz"),
    {ok, saved_to_file} = httpc:request(get, {binary_to_list(Url), []}, [], [{stream, TmpFile}]),
    {tarball, TmpFile}.

make_vsn(_) ->
    {error, "Replacing version of type pkg not supported."}.
