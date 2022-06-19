%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%%
%% @doc A localfs custom resource (for testing purposes only)
%% implementing the deprecated rebar_resource instead of v2
%%
%% ```
%% {deps, [
%%     %% Application files are copied from "/path/to/app_name"
%%     {app_name, {localfs, "/path/to/app_name", undefined}}
%% ]}.
%% '''
-module(rebar_localfs_resource).

-behaviour(rebar_resource).

-export([init/1
        ,lock/2
        ,download/3
        ,needs_update/2
        ,make_vsn/1]).

-include_lib("eunit/include/eunit.hrl").

-spec init(rebar_state:t()) -> {ok, term()}.
init(_State) ->
    {ok, #{}}.

lock(AppDir, {localfs, Path, _Ref}) ->
    lock(AppDir, {localfs, Path});
lock(_AppDir, {localfs, Path}) ->
    {localfs, Path, undefined}.

needs_update(_AppDir, _Resource) ->
    false.

download(AppDir, {localfs, Path, _Ref}, State) ->
    download(AppDir, {localfs, Path}, State);
download(AppDir, {localfs, Path}, _State) ->
    ok = rebar_file_utils:cp_r(filelib:wildcard(Path ++ "/*"), AppDir),
    {ok, undefined}.

make_vsn(_AppDir) ->
    {plain, "undefined"}.
