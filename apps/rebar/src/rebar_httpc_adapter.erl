%% Derived from hex_core v0.7.1 for extra flexibility.

-module(rebar_httpc_adapter).
-behaviour(r3_hex_http).
-export([request/5]).

%%====================================================================
%% API functions
%%====================================================================

request(Method, URI, ReqHeaders, Body, AdapterConfig) ->
    case os:getenv("REBAR_OFFLINE") of
        "1" ->
            {error, {offline, URI}};
        _ ->
            request_online(Method, URI, ReqHeaders, Body, AdapterConfig)
    end.

%%====================================================================
%% Internal functions
%%====================================================================

request_online(Method, URI, ReqHeaders, Body, AdapterConfig) ->
    request_online2(Method, URI, ReqHeaders, Body, AdapterConfig, 0).

request_online2(_Method, _URI, _ReqHeaders, _Body, _AdapterConfig, RedirectCount) when RedirectCount > 5 ->
    {error, "Too many redirects (>5)"};

request_online2(Method, URI, ReqHeaders, Body, AdapterConfig, RedirectCount) ->
    Profile = maps:get(profile, AdapterConfig, default),
    Request = build_request(URI, ReqHeaders, Body),
    HTTPOpts = [{ssl, rebar_utils:ssl_opts(URI)}, {autoredirect, false}],
    case httpc:request(Method, Request, HTTPOpts, [{body_format, binary}], Profile) of
        {ok, {{_, StatusCode, _}, RespHeaders, _RespBody}} when (StatusCode < 400) and (StatusCode >= 300) and (StatusCode /= 304) ->
            #{<<"location">> := RedirectURI} = load_headers(RespHeaders),
            request_online2(Method, RedirectURI, ReqHeaders, Body, AdapterConfig, RedirectCount + 1);
        {ok, {{_, StatusCode, _}, RespHeaders, RespBody}} ->
            RespHeaders2 = load_headers(RespHeaders),
            {ok, {StatusCode, RespHeaders2, RespBody}};
        {error, Reason} -> {error, Reason}
    end.

build_request(URI, ReqHeaders, Body) ->
    build_request2(binary_to_list(URI), dump_headers(ReqHeaders), Body).

build_request2(URI, ReqHeaders, undefined) ->
    {URI, ReqHeaders};
build_request2(URI, ReqHeaders, {ContentType, Body}) ->
    {URI, ReqHeaders, ContentType, Body}.

dump_headers(Map) ->
    maps:fold(fun(K, V, Acc) ->
        [{binary_to_list(K), binary_to_list(V)} | Acc] end, [], Map).

load_headers(List) ->
    lists:foldl(fun({K, V}, Acc) ->
        maps:put(list_to_binary(K), list_to_binary(V), Acc) end, #{}, List).

