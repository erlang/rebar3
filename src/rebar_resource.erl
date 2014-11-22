%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
-module(rebar_resource).

-export([]).

-export_type([resource/0
             ,type/0
             ,location/0
             ,ref/0]).

-type resource() :: {type(), location(), ref()}.
-type type() :: atom().
-type location() :: string().
-type ref() :: any().

-ifdef(no_callback_support).

%% In the case where R14 or lower is being used to compile the system
%% we need to export a behaviour info
-export([behaviour_info/1]).

-spec behaviour_info(atom()) -> [{atom(), arity()}] | undefined.
behaviour_info(callbacks) ->
    [{lock, 2},
     {download, 2},
     {needs_update,2},
     {make_vsn, 1}];
behaviour_info(_) ->
    undefined.

-else.

-callback lock(file:filename_all(), tuple()) ->
    rebar_resource:resource().
-callback download(file:filename_all(), tuple()) ->
    {tarball, file:filename_all()} | {ok, any()} | {error, any()}.
-callback needs_update(file:filename_all(), tuple()) ->
    {tarball, file:filename_all()} | {ok, any()} | {error, any()}.
-callback make_vsn(file:filename_all()) ->
    {plain, string()} | {error, string()}.

-endif.
