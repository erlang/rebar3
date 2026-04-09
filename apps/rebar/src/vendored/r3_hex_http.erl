%% Vendored from hex_core v0.15.0, do not edit manually

%% @doc
%% HTTP contract.
-module(r3_hex_http).
-export([request/5, request_to_file/6]).
-ifdef(TEST).
-export([user_agent/1]).
-endif.
-include_lib("r3_hex_core.hrl").

-type method() :: get | post | put | patch | delete.
-type status() :: non_neg_integer().
-export_type([status/0]).
-type headers() :: #{binary() => binary()}.
-export_type([headers/0]).
-type body() :: {ContentType :: binary(), Body :: binary()} | undefined.
-export_type([body/0]).
-type adapter_config() :: map().
-export_type([adapter_config/0]).

-callback request(method(), URI :: binary(), headers(), body(), adapter_config()) ->
    {ok, {status(), headers(), binary()}}
    | {error, term()}.

-callback request_to_file(
    method(), URI :: binary(), headers(), body(), file:name_all(), adapter_config()
) ->
    {ok, {status(), headers()}} | {error, term()}.

-spec request(r3_hex_core:config(), method(), URI :: binary(), headers(), body()) ->
    {ok, {status(), headers(), binary()}} | {error, term()}.
request(Config, Method, URI, Headers, Body) when is_binary(URI) and is_map(Headers) ->
    {Adapter, AdapterConfig} = adapter(Config),
    UserAgentFragment = maps:get(http_user_agent_fragment, Config),
    Headers2 = put_new(<<"user-agent">>, user_agent(UserAgentFragment), Headers),
    Adapter:request(Method, URI, Headers2, Body, AdapterConfig).

-spec request_to_file(
    r3_hex_core:config(), method(), URI :: binary(), headers(), body(), file:name_all()
) ->
    {ok, {status(), headers()}} | {error, term()}.
request_to_file(Config, Method, URI, Headers, Body, Filename) when
    is_binary(URI) and is_map(Headers)
->
    {Adapter, AdapterConfig} = adapter(Config),
    UserAgentFragment = maps:get(http_user_agent_fragment, Config),
    Headers2 = put_new(<<"user-agent">>, user_agent(UserAgentFragment), Headers),
    Adapter:request_to_file(Method, URI, Headers2, Body, Filename, AdapterConfig).

%% @private
user_agent(UserAgentFragment) ->
    OTPRelease = erlang:system_info(otp_release),
    ERTSVersion = erlang:system_info(version),
    OTPString = " (OTP/" ++ OTPRelease ++ ") (erts/" ++ ERTSVersion ++ ")",
    iolist_to_binary(["hex_core/", ?HEX_CORE_VERSION, " ", UserAgentFragment, OTPString]).

%%====================================================================
%% Internal functions
%%====================================================================

%% @private
adapter(Config) ->
    case maps:get(http_adapter, Config, {r3_hex_http_httpc, #{}}) of
        {Adapter, AdapterConfig} ->
            {Adapter, AdapterConfig};
        %% TODO: remove in v0.9
        Adapter when is_atom(Adapter) ->
            AdapterConfig = maps:get(http_adapter_config, Config, #{}),
            io:format(
                "[r3_hex_http] setting #{http_adapter => Module, http_adapter_config => Map} "
                "is deprecated in favour of #{http_adapter => {Module, Map}}~n"
            ),
            {Adapter, AdapterConfig}
    end.

%% @private
put_new(Key, Value, Map) ->
    case maps:find(Key, Map) of
        {ok, _} -> Map;
        error -> maps:put(Key, Value, Map)
    end.
