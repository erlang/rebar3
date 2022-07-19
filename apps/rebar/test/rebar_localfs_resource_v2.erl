%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%%
%% @doc A localfs custom resource (for testing purposes only)
%%
%% ```
%% {deps, [
%%     %% Application files are copied from "/path/to/app_name"
%%     {app_name, {localfs, "/path/to/app_name", undefined}}
%% ]}.
%% '''
-module(rebar_localfs_resource_v2).

-behaviour(rebar_resource_v2).

-export([init/2
        ,lock/2
        ,download/4
        ,needs_update/2
        ,make_vsn/2]).

-include_lib("eunit/include/eunit.hrl").

-spec init(atom(), rebar_state:t()) -> {ok, term()}.
init(Type, _State) ->
    Resource = rebar_resource_v2:new(Type, ?MODULE, #{}),
    {ok, Resource}.

lock(AppInfo, _) ->
    case rebar_app_info:source(AppInfo) of
        {localfs, Path, _Ref} ->
            {localfs, Path, undefined};
        {localfs, Path} ->
            {localfs, Path, undefined}
    end.

needs_update(_AppInfo, _) ->
    false.

download(TmpDir, AppInfo, State, _) ->
    download_(TmpDir, rebar_app_info:source(AppInfo), State).

download_(TmpDir, {localfs, Path, _Ref}, State) ->
    download_(TmpDir, {localfs, Path}, State);
download_(TmpDir, {localfs, Path}, _State) ->
    ok = rebar_file_utils:cp_r(filelib:wildcard(Path ++ "/*"), TmpDir),
    {ok, undefined}.

make_vsn(_AppInfo, _) ->
    {plain, "undefined"}.
