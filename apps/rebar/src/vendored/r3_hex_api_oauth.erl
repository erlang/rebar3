%% Vendored from hex_core v0.15.0, do not edit manually

%% @doc
%% Hex HTTP API - OAuth.
-module(r3_hex_api_oauth).
-export([
    device_authorization/3,
    device_authorization/4,
    device_auth_flow/4,
    device_auth_flow/5,
    poll_device_token/3,
    refresh_token/3,
    revoke_token/3,
    client_credentials_token/4,
    client_credentials_token/5
]).

-export_type([oauth_tokens/0, device_auth_error/0]).

-type oauth_tokens() :: #{
    access_token := binary(),
    refresh_token => binary() | undefined,
    expires_at := integer()
}.

-type device_auth_error() ::
    timeout
    | {access_denied, Status :: non_neg_integer(), Body :: term()}
    | {device_auth_failed, Status :: non_neg_integer(), Body :: term()}
    | {poll_failed, Status :: non_neg_integer(), Body :: term()}
    | term().

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
%%   * `name' - A name to identify the token (defaults to the machine's hostname)
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
    Name =
        case proplists:get_value(name, Opts) of
            undefined -> get_hostname();
            N -> N
        end,
    Params = #{
        <<"client_id">> => ClientId,
        <<"scope">> => Scope,
        <<"name">> => Name
    },
    r3_hex_api:post(Config, Path, Params).

%% @doc
%% Runs the complete OAuth device authorization flow.
%%
%% @see device_auth_flow/5
%% @end
-spec device_auth_flow(
    r3_hex_core:config(),
    ClientId :: binary(),
    Scope :: binary(),
    PromptUser :: fun((VerificationUri :: binary(), UserCode :: binary()) -> ok)
) -> {ok, oauth_tokens()} | {error, device_auth_error()}.
device_auth_flow(Config, ClientId, Scope, PromptUser) ->
    device_auth_flow(Config, ClientId, Scope, PromptUser, []).

%% @doc
%% Runs the complete OAuth device authorization flow with options.
%%
%% This function handles the entire device authorization flow:
%% 1. Requests a device code from the server
%% 2. Calls `PromptUser' callback with the verification URI and user code
%% 3. Optionally opens the browser for the user (when `open_browser' is true)
%% 4. Polls the token endpoint until authorization completes or times out
%%
%% The `PromptUser' callback is responsible for displaying the verification URI
%% and user code to the user (e.g., printing to console).
%%
%% Options:
%%   * `name' - A name to identify the token (defaults to the machine's hostname)
%%   * `open_browser' - When `true', automatically opens the browser
%%     to the verification URI. When `false' (default), only the callback is invoked.
%%
%% Returns:
%% - `{ok, Tokens}' - Authorization successful, returns access token and optional refresh token
%% - `{error, timeout}' - Device code expired before user completed authorization
%% - `{error, {access_denied, Status, Body}}' - User denied the authorization request
%% - `{error, {device_auth_failed, Status, Body}}' - Initial device authorization request failed
%% - `{error, {poll_failed, Status, Body}}' - Unexpected error during polling
%%
%% Examples:
%%
%% ```
%% 1> Config = r3_hex_core:default_config().
%% 2> PromptUser = fun(Uri, Code) ->
%%        io:format("Visit ~s and enter code: ~s~n", [Uri, Code])
%%    end.
%% 3> r3_hex_api_oauth:device_auth_flow(Config, <<"cli">>, <<"api:write">>, PromptUser).
%% {ok, #{
%%     access_token => <<"...">>,
%%     refresh_token => <<"...">>,
%%     expires_at => 1234567890
%% }}
%% '''
%% @end
-spec device_auth_flow(
    r3_hex_core:config(),
    ClientId :: binary(),
    Scope :: binary(),
    PromptUser :: fun((VerificationUri :: binary(), UserCode :: binary()) -> ok),
    proplists:proplist()
) -> {ok, oauth_tokens()} | {error, device_auth_error()}.
device_auth_flow(Config, ClientId, Scope, PromptUser, Opts) ->
    case device_authorization(Config, ClientId, Scope, Opts) of
        {ok, {200, _, DeviceResponse}} when is_map(DeviceResponse) ->
            #{
                <<"device_code">> := DeviceCode,
                <<"user_code">> := UserCode,
                <<"verification_uri_complete">> := VerificationUri,
                <<"expires_in">> := ExpiresIn,
                <<"interval">> := IntervalSeconds
            } = DeviceResponse,
            ok = PromptUser(VerificationUri, UserCode),
            OpenBrowser = proplists:get_value(open_browser, Opts, false),
            case OpenBrowser of
                true -> open_browser(VerificationUri);
                false -> ok
            end,
            ExpiresAt = erlang:system_time(second) + ExpiresIn,
            poll_for_token_loop(Config, ClientId, DeviceCode, IntervalSeconds, ExpiresAt);
        {ok, {Status, _, Body}} ->
            {error, {device_auth_failed, Status, Body}};
        {error, Reason} ->
            {error, Reason}
    end.

%% @private
poll_for_token_loop(Config, ClientId, DeviceCode, IntervalSeconds, ExpiresAt) ->
    Now = erlang:system_time(second),
    case Now >= ExpiresAt of
        true ->
            {error, timeout};
        false ->
            timer:sleep(IntervalSeconds * 1000),
            case poll_device_token(Config, ClientId, DeviceCode) of
                {ok, {200, _, TokenResponse}} when is_map(TokenResponse) ->
                    #{
                        <<"access_token">> := AccessToken,
                        <<"expires_in">> := ExpiresIn
                    } = TokenResponse,
                    RefreshToken = maps:get(<<"refresh_token">>, TokenResponse, undefined),
                    TokenExpiresAt = erlang:system_time(second) + ExpiresIn,
                    {ok, #{
                        access_token => AccessToken,
                        refresh_token => RefreshToken,
                        expires_at => TokenExpiresAt
                    }};
                {ok, {400, _, #{<<"error">> := <<"authorization_pending">>}}} ->
                    poll_for_token_loop(Config, ClientId, DeviceCode, IntervalSeconds, ExpiresAt);
                {ok, {400, _, #{<<"error">> := <<"slow_down">>}}} ->
                    %% Increase polling interval as requested by server
                    poll_for_token_loop(
                        Config, ClientId, DeviceCode, IntervalSeconds + 5, ExpiresAt
                    );
                {ok, {400, _, #{<<"error">> := <<"expired_token">>}}} ->
                    {error, timeout};
                {ok, {Status, _, #{<<"error">> := <<"access_denied">>} = Body}} ->
                    {error, {access_denied, Status, Body}};
                {ok, {Status, _, Body}} ->
                    {error, {poll_failed, Status, Body}};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

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

%%====================================================================
%% Internal functions
%%====================================================================

%% @private
%% Open a URL in the default browser.
%% Uses platform-specific commands: open (macOS), xdg-open (Linux), start (Windows).
-spec open_browser(binary()) -> ok.
open_browser(Url) when is_binary(Url) ->
    ok = ensure_valid_http_url(Url),
    UrlStr = binary_to_list(Url),
    {Cmd, Args} =
        case os:type() of
            {unix, darwin} ->
                {"open", [UrlStr]};
            {unix, _} ->
                {"xdg-open", [UrlStr]};
            {win32, _} ->
                {"cmd", ["/c", "start", "", UrlStr]}
        end,
    Port = open_port({spawn_executable, os:find_executable(Cmd)}, [{args, Args}]),
    port_close(Port),
    ok.

%% @private
%% Validates that a URL uses http:// or https:// scheme.
-spec ensure_valid_http_url(binary()) -> ok.
ensure_valid_http_url(Url) when is_binary(Url) ->
    case uri_string:parse(Url) of
        #{scheme := <<"https">>} -> ok;
        #{scheme := <<"http">>} -> ok;
        _ -> throw({invalid_url, Url})
    end.

%% @private
%% Get the hostname of the current machine.
-spec get_hostname() -> binary().
get_hostname() ->
    {ok, Hostname} = inet:gethostname(),
    list_to_binary(Hostname).
