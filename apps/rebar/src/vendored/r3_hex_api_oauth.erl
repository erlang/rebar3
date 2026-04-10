%% Vendored from hex_core v0.15.0, do not edit manually

%% @doc
%% Hex HTTP API - OAuth.
-module(r3_hex_api_oauth).
-export([
    device_authorization/3,
    device_authorization/4,
    poll_device_token/3,
    refresh_token/3,
    revoke_token/3,
    client_credentials_token/4,
    client_credentials_token/5
]).

%% @doc
%% Initiates the OAuth device authorization flow.
%%
%% @see device_authorization/4
%% @end
-spec device_authorization(r3_hex_core:config(), binary(), binary()) -> r3_hex_api:response().
device_authorization(Config, ClientId, Scope) ->
    device_authorization(Config, ClientId, Scope, []).

%% @doc
%% Initiates the OAuth device authorization flow with optional parameters.
%%
%% Returns device code, user code, and verification URIs for user authentication.
%%
%% Options:
%%   * `name' - A name to identify the token (e.g., hostname of the device)
%%
%% Examples:
%%
%% ```
%% 1> Config = r3_hex_core:default_config().
%% 2> r3_hex_api_oauth:device_authorization(Config, <<"cli">>, <<"api:write">>).
%% {ok,{200, ..., #{
%%     <<"device_code">> => <<"...">>,
%%     <<"user_code">> => <<"ABCD-1234">>,
%%     <<"verification_uri">> => <<"https://hex.pm/oauth/device">>,
%%     <<"verification_uri_complete">> => <<"https://hex.pm/oauth/device?user_code=ABCD-1234">>,
%%     <<"expires_in">> => 600,
%%     <<"interval">> => 5
%% }}}
%%
%% 3> r3_hex_api_oauth:device_authorization(Config, <<"cli">>, <<"api:write">>, [{name, <<"MyMachine">>}]).
%% '''
%% @end
-spec device_authorization(r3_hex_core:config(), binary(), binary(), proplists:proplist()) ->
    r3_hex_api:response().
device_authorization(Config, ClientId, Scope, Opts) ->
    Path = <<"oauth/device_authorization">>,
    Params0 = #{
        <<"client_id">> => ClientId,
        <<"scope">> => Scope
    },
    Params =
        case proplists:get_value(name, Opts) of
            undefined -> Params0;
            Name -> Params0#{<<"name">> => Name}
        end,
    r3_hex_api:post(Config, Path, Params).

%% @doc
%% Polls the OAuth token endpoint for device authorization completion.
%%
%% Returns:
%% - `{ok, {200, _, Token}}` - Authorization complete
%% - `{ok, {400, _, #{<<"error">> => <<"authorization_pending">>}}}` - Still waiting
%% - `{ok, {400, _, #{<<"error">> => <<"slow_down">>}}}` - Polling too fast
%% - `{ok, {400, _, #{<<"error">> => <<"expired_token">>}}}` - Code expired
%% - `{ok, {403, _, #{<<"error">> => <<"access_denied">>}}}` - User denied
%%
%% Examples:
%%
%% ```
%% 1> Config = r3_hex_core:default_config().
%% 2> r3_hex_api_oauth:poll_device_token(Config, <<"cli">>, DeviceCode).
%% {ok, {200, _, #{
%%     <<"access_token">> => <<"...">>,
%%     <<"refresh_token">> => <<"...">>,
%%     <<"token_type">> => <<"Bearer">>,
%%     <<"expires_in">> => 3600
%% }}}
%% '''
%% @end
-spec poll_device_token(r3_hex_core:config(), binary(), binary()) -> r3_hex_api:response().
poll_device_token(Config, ClientId, DeviceCode) ->
    Path = <<"oauth/token">>,
    Params = #{
        <<"grant_type">> => <<"urn:ietf:params:oauth:grant-type:device_code">>,
        <<"device_code">> => DeviceCode,
        <<"client_id">> => ClientId
    },
    r3_hex_api:post(Config, Path, Params).

%% @doc
%% Refreshes an access token using a refresh token.
%%
%% Examples:
%%
%% ```
%% 1> Config = r3_hex_core:default_config().
%% 2> r3_hex_api_oauth:refresh_token(Config, <<"cli">>, RefreshToken).
%% {ok, {200, _, #{
%%     <<"access_token">> => <<"...">>,
%%     <<"refresh_token">> => <<"...">>,
%%     <<"token_type">> => <<"Bearer">>,
%%     <<"expires_in">> => 3600
%% }}}
%% '''
%% @end
-spec refresh_token(r3_hex_core:config(), binary(), binary()) -> r3_hex_api:response().
refresh_token(Config, ClientId, RefreshToken) ->
    Path = <<"oauth/token">>,
    Params = #{
        <<"grant_type">> => <<"refresh_token">>,
        <<"refresh_token">> => RefreshToken,
        <<"client_id">> => ClientId
    },
    r3_hex_api:post(Config, Path, Params).

%% @doc
%% Exchanges an API key for an OAuth access token using the client credentials grant.
%%
%% @see client_credentials_token/5
%% @end
-spec client_credentials_token(r3_hex_core:config(), binary(), binary(), binary()) ->
    r3_hex_api:response().
client_credentials_token(Config, ClientId, ApiKey, Scope) ->
    client_credentials_token(Config, ClientId, ApiKey, Scope, []).

%% @doc
%% Exchanges an API key for an OAuth access token using the client credentials grant with optional parameters.
%%
%% This grant type allows exchanging a long-lived API key for a short-lived OAuth access token.
%% The API key is sent as the client_secret parameter.
%%
%% Options:
%%   * `name' - A name to identify the token (e.g., hostname of the client)
%%
%% Returns:
%% - `{ok, {200, _, Token}}` - Token exchange successful
%% - `{ok, {400, _, #{<<"error">> => ...}}}` - Invalid request or scope
%% - `{ok, {401, _, #{<<"error">> => ...}}}` - Invalid API key
%%
%% Examples:
%%
%% ```
%% 1> Config = r3_hex_core:default_config().
%% 2> r3_hex_api_oauth:client_credentials_token(Config, <<"cli">>, ApiKey, <<"api">>).
%% {ok, {200, _, #{
%%     <<"access_token">> => <<"...">>,
%%     <<"token_type">> => <<"bearer">>,
%%     <<"expires_in">> => 1800,
%%     <<"scope">> => <<"api">>
%% }}}
%%
%% 3> r3_hex_api_oauth:client_credentials_token(Config, <<"cli">>, ApiKey, <<"api">>, [{name, <<"MyMachine">>}]).
%% '''
%% @end
-spec client_credentials_token(
    r3_hex_core:config(), binary(), binary(), binary(), proplists:proplist()
) -> r3_hex_api:response().
client_credentials_token(Config, ClientId, ApiKey, Scope, Opts) ->
    Path = <<"oauth/token">>,
    Params0 = #{
        <<"grant_type">> => <<"client_credentials">>,
        <<"client_id">> => ClientId,
        <<"client_secret">> => ApiKey,
        <<"scope">> => Scope
    },
    Params =
        case proplists:get_value(name, Opts) of
            undefined -> Params0;
            Name -> Params0#{<<"name">> => Name}
        end,
    r3_hex_api:post(Config, Path, Params).

%% @doc
%% Revokes an OAuth token (RFC 7009).
%%
%% Can revoke either access tokens or refresh tokens.
%% Returns 200 OK regardless of whether the token was found,
%% following RFC 7009 security recommendations.
%%
%% Examples:
%%
%% ```
%% 1> Config = r3_hex_core:default_config().
%% 2> r3_hex_api_oauth:revoke_token(Config, <<"cli">>, Token).
%% {ok, {200, ..., nil}}
%% '''
%% @end
-spec revoke_token(r3_hex_core:config(), binary(), binary()) -> r3_hex_api:response().
revoke_token(Config, ClientId, Token) ->
    Path = <<"oauth/revoke">>,
    Params = #{
        <<"token">> => Token,
        <<"client_id">> => ClientId
    },
    r3_hex_api:post(Config, Path, Params).
