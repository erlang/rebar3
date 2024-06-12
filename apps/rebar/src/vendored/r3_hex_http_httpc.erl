%% Vendored from hex_core v0.10.1, do not edit manually

%% @doc
%% httpc-based implementation of {@link r3_hex_http} contract.
%%
%% Configuration keys:
%%
%% * `profile' - the name of the profile, defaults to `default'. See
%%    {@link httpc:set_options/2} for more information on setting
%%    options on profiles.
%%
%% * `http_options' - a list of HTTP options, defaults to `[]'. See
%%   {@link httpc:request/5} for a list of available HTTP options.

-module(r3_hex_http_httpc).
-behaviour(r3_hex_http).
-export([request/5]).

%%====================================================================
%% API functions
%%====================================================================

request(Method, URI, ReqHeaders, Body, AdapterConfig) when is_binary(URI) ->
    Profile = maps:get(profile, AdapterConfig, default),
    HTTPOptions = maps:get(http_options, AdapterConfig, []),

    HTTPS =
        case URI of
            <<"https", _/binary>> -> true;
            _ -> false
        end,
    SSLOpts = proplists:get_value(ssl, HTTPOptions),

    if
        HTTPS == true andalso SSLOpts == undefined ->
            io:format(
                "[r3_hex_http_httpc] using default ssl options which are insecure.~n"
                "Configure your adapter with: "
                "{r3_hex_http_httpc, #{http_options => [{ssl, SslOpts}]}}~n"
            );
        true ->
            ok
    end,

    Request = build_request(URI, ReqHeaders, Body),
    case httpc:request(Method, Request, HTTPOptions, [{body_format, binary}], Profile) of
        {ok, {{_, StatusCode, _}, RespHeaders, RespBody}} ->
            RespHeaders2 = load_headers(RespHeaders),
            {ok, {StatusCode, RespHeaders2, RespBody}};
        {error, Reason} ->
            {error, Reason}
    end.

%%====================================================================
%% Internal functions
%%====================================================================

%% @private
build_request(URI, ReqHeaders, Body) ->
    build_request2(binary_to_list(URI), dump_headers(ReqHeaders), Body).

%% @private
build_request2(URI, ReqHeaders, undefined) ->
    {URI, ReqHeaders};
build_request2(URI, ReqHeaders, {ContentType, Body}) ->
    {URI, ReqHeaders, ContentType, Body}.

%% @private
dump_headers(Map) ->
    maps:fold(
        fun(K, V, Acc) ->
            [{binary_to_list(K), binary_to_list(V)} | Acc]
        end,
        [],
        Map
    ).

%% @private
load_headers(List) ->
    lists:foldl(
        fun({K, V}, Acc) ->
            maps:put(list_to_binary(K), list_to_binary(V), Acc)
        end,
        #{},
        List
    ).
