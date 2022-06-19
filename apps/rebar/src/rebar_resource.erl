%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
-module(rebar_resource).

-export([new/3,
         lock/2,
         download/4,
         needs_update/2,
         make_vsn/2]).

-export_type([source/0,
              type/0,
              location/0,
              ref/0]).

-include("rebar.hrl").

-type source() :: {type(), location(), ref()} | {type(), location(), ref(), binary()}.
-type type() :: atom().
-type location() :: string().
-type ref() :: any().

-callback lock(file:filename_all(), tuple()) ->
    source().
-callback download(file:filename_all(), tuple(), rebar_state:t()) ->
    {tarball, file:filename_all()} | {ok, any()} | {error, any()}.
-callback needs_update(file:filename_all(), tuple()) ->
    boolean().
-callback make_vsn(file:filename_all()) ->
    {plain, string()} | {error, string()}.

-spec new(type(), module(), term()) -> rebar_resource_v2:resource().
new(Type, Module, State) ->
    #resource{type=Type,
              module=Module,
              state=State,
              implementation=?MODULE}.

lock(Module, AppInfo) ->
    Module:lock(rebar_app_info:dir(AppInfo), rebar_app_info:source(AppInfo)).

download(Module, TmpDir, AppInfo, State) ->
    case Module:download(TmpDir, rebar_app_info:source(AppInfo), State) of
        {ok, _} ->
            ok;
        Error ->
            Error
    end.

needs_update(Module, AppInfo) ->
    Module:needs_update(rebar_app_info:dir(AppInfo), rebar_app_info:source(AppInfo)).

make_vsn(Module, AppInfo) ->
    Module:make_vsn(rebar_app_info:dir(AppInfo)).
