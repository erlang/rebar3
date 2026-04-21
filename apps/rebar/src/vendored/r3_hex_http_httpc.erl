%% Vendored from hex_core v0.15.0, do not edit manually

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
-export([request/5, request_to_file/6]).

%%====================================================================
%% API functions
%%====================================================================

request(Method, URI, ReqHeaders, Body, AdapterConfig) when is_binary(URI) ->
    Profile = maps:get(profile, AdapterConfig, default),
    HTTPOptions = http_options(URI, AdapterConfig),
    Request = build_request(URI, ReqHeaders, Body),
    case httpc:request(Method, Request, HTTPOptions, [{body_format, binary}], Profile) of
        {ok, {{_, StatusCode, _}, RespHeaders, RespBody}} ->
            RespHeaders2 = load_headers(RespHeaders),
            {ok, {StatusCode, RespHeaders2, RespBody}};
        {error, Reason} ->
            {error, Reason}
    end.

request_to_file(Method, URI, ReqHeaders, Body, Filename, AdapterConfig) when is_binary(URI) ->
    Profile = maps:get(profile, AdapterConfig, default),
    HTTPOptions = http_options(URI, AdapterConfig),
    Request = build_request(URI, ReqHeaders, Body),
    case
        httpc:request(
            Method,
            Request,
            HTTPOptions,
            [{sync, false}, {stream, self}],
            Profile
        )
    of
        {ok, RequestId} ->
            stream_to_file(RequestId, Filename);
        {error, Reason} ->
            {error, Reason}
    end.

%%====================================================================
%% Internal functions
%%====================================================================

%% @private
%% httpc streams 200/206 responses as messages and returns non-2xx as
%% a normal response tuple. stream_start includes the response headers.
stream_to_file(RequestId, Filename) ->
    receive
        {http, {RequestId, stream_start, Headers}} ->
            {ok, File} = file:open(Filename, [write, binary]),
            case stream_body(RequestId, File) of
                ok ->
                    ok = file:close(File),
                    {ok, {200, load_headers(Headers)}};
                {error, Reason} ->
                    ok = file:close(File),
                    {error, Reason}
            end;
        {http, {RequestId, {{_, StatusCode, _}, RespHeaders, _RespBody}}} ->
            {ok, {StatusCode, load_headers(RespHeaders)}};
        {http, {RequestId, {error, Reason}}} ->
            {error, Reason}
    end.

%% @private
stream_body(RequestId, File) ->
    receive
        {http, {RequestId, stream, BinBodyPart}} ->
            ok = file:write(File, BinBodyPart),
            stream_body(RequestId, File);
        {http, {RequestId, stream_end, _Headers}} ->
            ok;
        {http, {RequestId, {error, Reason}}} ->
            {error, Reason}
    end.

%% @private
http_options(URI, AdapterConfig) ->
    HTTPOptions0 = maps:get(http_options, AdapterConfig, []),

    HTTPS =
        case URI of
            <<"https", _/binary>> -> true;
            _ -> false
        end,
    SSLOpts0 = proplists:get_value(ssl, HTTPOptions0),

    if
        HTTPS == true andalso SSLOpts0 == undefined ->
            try
                [
                    {ssl, [
                        {verify, verify_peer},
                        {cacerts, public_key:cacerts_get()},
                        {depth, 3},
                        {customize_hostname_check, [
                            {match_fun, public_key:pkix_verify_hostname_match_fun(https)}
                        ]}
                    ]}
                    | HTTPOptions0
                ]
            catch
                _:_ ->
                    io:format(
                        "[r3_hex_http_httpc] using default ssl options which are insecure.~n"
                        "Configure your adapter with: "
                        "{r3_hex_http_httpc, #{http_options => [{ssl, SslOpts}]}}~n"
                        "or upgrade Erlang/OTP to OTP-25 or later.~n"
                    ),
                    HTTPOptions0
            end;
        true ->
            HTTPOptions0
    end.

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
