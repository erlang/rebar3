%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
-module(rebar_resource).

-export([]).

-ifdef(have_callback_support).

%% In the case where R14 or lower is being used to compile the system
%% we need to export a behaviour info
-export([behaviour_info/1]).

-spec behaviour_info(atom()) -> [{atom(), arity()}] | undefined.
behaviour_info(callbacks) ->
    [{lock, 2},
     {download, 2}];
behaviour_info(_) ->
    undefined.

-else.

-callback lock(string(), tuple()) ->  ok.
-callback download(string(), tuple()) -> ok.

-endif.
