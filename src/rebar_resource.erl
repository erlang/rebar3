%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
-module(rebar_resource).

-export([new/3,
         find_resource_module/2,
         find_resource_state/2,
         format_source/1]).

-export_type([resource/0
             ,source/0
             ,type/0
             ,location/0
             ,ref/0]).

-record(resource, {type :: atom(),
                   module :: module(),
                   state :: term()}).

-type resource() :: #resource{}.
-type source() :: {type(), location(), ref()} | {type(), location(), ref(), binary()}.
-type type() :: atom().
-type location() :: string().
-type ref() :: any().

-callback init(rebar_state:t()) -> {ok, term()}.
-callback lock(file:filename_all(), tuple()) ->
    source().
-callback download(file:filename_all(), tuple(), rebar_state:t()) ->
    {tarball, file:filename_all()} | {ok, any()} | {error, any()}.
-callback needs_update(file:filename_all(), tuple()) ->
    boolean().
-callback make_vsn(file:filename_all()) ->
    {plain, string()} | {error, string()}.

-optional_callbacks([init/1]).

-spec new(type(), module(), term()) -> resource().
new(Type, Module, State) ->
    #resource{type=Type,
              module=Module,
              state=State}.

find_resource_module(Type, Resources) ->
    case lists:keyfind(Type, #resource.type, Resources) of
        false when is_atom(Type) ->
            case code:which(Type) of
                non_existing ->
                    {error, not_found};
                _ ->
                    {ok, Type}
            end;
        false ->
            {error, not_found};
        #resource{module=Module} ->
            {ok, Module}
    end.

find_resource_state(Type, Resources) ->
    case lists:keyfind(Type, #resource.type, Resources) of
        false ->            
            {error, not_found};
        #resource{state=State} ->
            State
    end.

format_source({pkg, Name, Vsn, _Hash, _}) -> {pkg, Name, Vsn};
format_source(Source) -> Source.
