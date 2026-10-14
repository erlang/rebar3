%% Vendored from hex_core v0.20.1, do not edit manually

%% @doc
%% OIDC token issuance for CI providers, used by Workload Identity.
-module(r3_hex_oidc).
-export([detect_provider/0, fetch_token/3]).
-ifdef(TEST).
-export([put_audience/2]).
-endif.

-export_type([provider/0, fetch_error/0]).

-type provider() :: {github_actions, #{url := binary(), request_token := binary()}}.

-type fetch_error() ::
    {oidc_token_request_failed, Status :: non_neg_integer()}
    | {oidc_token_unavailable, Reason :: term()}
    | oidc_token_missing
    | json_unavailable.

%% @doc
%% Detects the CI provider issuing OIDC tokens for the current job, if any.
%%
%% Only GitHub Actions is supported today, detected through the
%% `ACTIONS_ID_TOKEN_REQUEST_URL' and `ACTIONS_ID_TOKEN_REQUEST_TOKEN'
%% environment variables, which GitHub only sets when the job has the
%% `id-token: write' permission.
%% @end
-spec detect_provider() -> {ok, provider()} | none.
detect_provider() ->
    detect_github_actions().

%% @doc
%% Fetches an OIDC token scoped to Audience from Provider, through the build
%% tool's HTTP adapter, so the request uses the same proxy settings and CA
%% certificates as the rest of the build tool's traffic.
%% @end
-spec fetch_token(r3_hex_core:config(), provider(), Audience :: binary()) ->
    {ok, binary()} | {error, fetch_error()}.
fetch_token(Config, Provider, Audience) ->
    case code:ensure_loaded(json) of
        {module, json} -> request_token(Config, Provider, Audience);
        {error, _Reason} -> {error, json_unavailable}
    end.

%%====================================================================
%% Internal functions
%%====================================================================

%% @private
request_token(Config, {github_actions, #{url := Url, request_token := RequestToken}}, Audience) ->
    Headers = #{
        <<"authorization">> => <<"Bearer ", RequestToken/binary>>,
        <<"accept">> => <<"application/json">>
    },
    case r3_hex_http:request(Config, get, put_audience(Url, Audience), Headers, undefined) of
        {ok, {200, _RespHeaders, Body}} ->
            case oidc_token_value(Body) of
                {ok, Token} -> {ok, Token};
                error -> {error, oidc_token_missing}
            end;
        {ok, {Status, _RespHeaders, _Body}} ->
            {error, {oidc_token_request_failed, Status}};
        {error, Reason} ->
            {error, {oidc_token_unavailable, Reason}}
    end.

%% @private
detect_github_actions() ->
    case {os:getenv("ACTIONS_ID_TOKEN_REQUEST_URL"), os:getenv("ACTIONS_ID_TOKEN_REQUEST_TOKEN")} of
        {Url, RequestToken} when
            is_list(Url), Url =/= "", is_list(RequestToken), RequestToken =/= ""
        ->
            {ok,
                {github_actions, #{
                    url => list_to_binary(Url), request_token => list_to_binary(RequestToken)
                }}};
        _Env ->
            none
    end.

%% @private
put_audience(Url, Audience) ->
    Parsed = uri_string:parse(Url),
    Param = uri_string:compose_query([{<<"audience">>, Audience}]),
    Query =
        case maps:get(query, Parsed, <<>>) of
            <<>> -> Param;
            Existing -> <<Existing/binary, "&", Param/binary>>
        end,
    iolist_to_binary(uri_string:recompose(Parsed#{query => Query})).

%% @private
%% Calls `json:decode/1' through `erlang:apply/3' so OTP releases without the
%% `json' module (pre-27) neither resolve the call at compile time nor warn
%% about it.
oidc_token_value(Body) ->
    try erlang:apply(json, decode, [Body]) of
        #{<<"value">> := Token} when is_binary(Token), Token =/= <<>> ->
            {ok, Token};
        _Other ->
            error
    catch
        error:_Reason ->
            error
    end.
