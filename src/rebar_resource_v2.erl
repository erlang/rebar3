%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
-module(rebar_resource_v2).

-export([new/3,
         find_resource_state/2,
         format_source/1,
         lock/2,
         download/3,
         needs_update/2,
         make_vsn/3,
         format_error/1]).

-export_type([resource/0,
              source/0,
              type/0,
              location/0,
              ref/0,
              resource_state/0]).

-include("rebar.hrl").
-include_lib("providers/include/providers.hrl").

-type resource() :: #resource{}.
-type source() :: {type(), location(), ref()} | {type(), location(), ref(), binary()}
                | {type(), location(), ref(), binary(), binary()}.
-type type() :: atom().
-type location() :: string().
-type ref() :: any().
-type resource_state() :: term().

-callback init(type(), rebar_state:t()) -> {ok, resource()}.
-callback lock(rebar_app_info:t(), resource_state()) -> source().
-callback download(file:filename_all(), rebar_app_info:t(), rebar_state:t(), resource_state()) ->
    ok | {error, any()}.
-callback needs_update(rebar_app_info:t(), resource_state()) -> boolean().
-callback make_vsn(rebar_app_info:t(), resource_state()) ->
    {plain, string()} | {error, string()}.

-spec new(type(), module(), term()) -> resource().
new(Type, Module, State) ->
    #resource{type=Type,
              module=Module,
              state=State,
              implementation=?MODULE}.

-spec find_resource(type(), [resource()]) -> {ok, resource()} | {error, not_found}.
find_resource(Type, Resources) ->
    case ec_lists:find(fun(#resource{type=T}) -> T =:= Type end, Resources) of
        error when is_atom(Type) ->
            case code:which(Type) of
                non_existing ->
                    {error, not_found};
                _ ->
                    {ok, rebar_resource:new(Type, Type, #{})}
            end;
        error ->
            {error, not_found};
        {ok, Resource} ->
            {ok, Resource}
    end.

find_resource_state(Type, Resources) ->
    case lists:keyfind(Type, #resource.type, Resources) of
        false ->
            {error, not_found};
        #resource{state=State} ->
            State
    end.

format_source(AppInfo) ->
    Name = rebar_app_info:name(AppInfo),
    case rebar_app_info:source(AppInfo) of
        {pkg, _Name, Vsn, _OldHash, _Hash, _} ->
            io_lib:format("~ts v~s", [Name, Vsn]);
        Source ->
            io_lib:format("~ts (from ~p)", [Name, Source])
    end.

lock(AppInfo, State) ->
    resource_run(lock, rebar_app_info:source(AppInfo), [AppInfo], State).

resource_run(Function, Source, Args, State) ->
    Resources = rebar_state:resources(State),
    case get_resource_type(Source, Resources) of
        {ok, #resource{type=_,
                       module=Module,
                       state=ResourceState,
                       implementation=?MODULE}} ->
            erlang:apply(Module, Function, Args++[ResourceState]);
        {ok, #resource{type=_,
                       module=Module,
                       state=_,
                       implementation=rebar_resource}} ->
            erlang:apply(rebar_resource, Function, [Module | Args])
    end.

download(TmpDir, AppInfo, State) ->
    resource_run(download, rebar_app_info:source(AppInfo), [TmpDir, AppInfo, State], State).

needs_update(AppInfo, State) ->
    resource_run(needs_update, rebar_app_info:source(AppInfo), [AppInfo], State).

%% this is a special case since it is used for project apps as well, not just deps
make_vsn(AppInfo, Vsn, State) ->
    VcsType = case Vsn of {T, _} -> T; T -> T end,
    Resources = rebar_state:resources(State),
    case is_resource_type(VcsType, Resources) of
        true ->
            case find_resource(VcsType, Resources) of
                {ok, #resource{type=_,
                               module=Module,
                               state=ResourceState,
                               implementation=?MODULE}} ->
                    Module:make_vsn(AppInfo, ResourceState);
                {ok, #resource{type=_,
                               module=Module,
                               state=_,
                               implementation=rebar_resource}} ->
                    rebar_resource:make_vsn(Module, AppInfo)
            end;
        false ->
            unknown
    end.

format_error({no_resource, Location, Type}) ->
    io_lib:format("Cannot handle dependency ~ts.~n"
                  "     No module found for resource type ~p.", [Location, Type]);
format_error({no_resource, Source}) ->
    io_lib:format("Cannot handle dependency ~ts.~n"
                  "     No module found for unknown resource type.", [Source]).

is_resource_type(Type, Resources) ->
    lists:any(fun(#resource{type=T}) -> T =:= Type end, Resources).

-spec get_resource_type(term(), [resource()]) -> {ok, resource()}.
get_resource_type({Type, Location}, Resources) ->
    get_resource(Type, Location, Resources);
get_resource_type({Type, Location, _}, Resources) ->
    get_resource(Type, Location, Resources);
get_resource_type({Type, _, _, Location}, Resources) ->
    get_resource(Type, Location, Resources);
get_resource_type(Location={Type, _, _, _, _, _}, Resources) ->
    get_resource(Type, Location, Resources);
get_resource_type(Source, _) ->
    throw(?PRV_ERROR({no_resource, Source})).

-spec get_resource(type(), term(), [resource()]) -> {ok, resource()}.
get_resource(Type, Location, Resources) ->
    case find_resource(Type, Resources) of
        {error, not_found} ->
            throw(?PRV_ERROR({no_resource, Location, Type}));
        {ok, Resource} ->
            {ok, Resource}
    end.
