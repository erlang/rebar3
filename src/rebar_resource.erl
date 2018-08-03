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

-callback lock(file:filename_all(), tuple()) ->
    rebar_resource:resource().
-callback download(file:filename_all(), tuple(), rebar_state:t()) ->
    {tarball, file:filename_all()} | {ok, any()} | {error, any()}.
-callback needs_update(file:filename_all(), tuple()) ->
    boolean().
-callback make_vsn(file:filename_all()) ->
    {plain, string()} | {error, string()}.

%% optional callbacks instroduced in OTP 18.0
-ifdef(optional_callback).

-callback check_type_support() -> {ok, any()} | {error, any()}.

-optional_callbacks([check_type_support/0]).

-endif.
