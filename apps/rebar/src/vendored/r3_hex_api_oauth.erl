%% Vendored from hex_core v0.20.1, do not edit manually

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
    organization_authorization/2,
    organization_reauth_required/1,
    revoke_token/3,
    client_credentials_token/4,
    client_credentials_token/5,
    jwt_bearer_token/3,
    oidc_audience/1,
    win_cmd_args/1
]).

-export_type([oauth_tokens/0, device_auth_error/0]).

-type oauth_tokens() :: #{
    access_token := binary(),
    refresh_token => binary(),
    expires_at := integer(),
    %% Organizations whose SSO or 2FA requirements the session must satisfy.
    %% Their scopes are absent until verification is complete; see
    %% organization_authorization/2.
    organization_reauth_required => [map()]
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
        {ok, {200, _, DeviceResponse}} ->
            case device_authorization_fields(DeviceResponse) of
                {ok, DeviceCode, UserCode, VerificationUri, ExpiresIn, IntervalSeconds} ->
                    ok = PromptUser(VerificationUri, UserCode),
                    OpenBrowser = proplists:get_value(open_browser, Opts, false),
                    case OpenBrowser of
                        true -> open_browser(VerificationUri);
                        false -> ok
                    end,
                    ExpiresAt = erlang:system_time(second) + ExpiresIn,
                    poll_for_token_loop(Config, ClientId, DeviceCode, IntervalSeconds, ExpiresAt);
                error ->
                    {error, {device_auth_failed, 200, DeviceResponse}}
            end;
        {ok, {Status, _, Body}} ->
            {error, {device_auth_failed, Status, Body}};
        {error, Reason} ->
            {error, Reason}
    end.

%% @private
%% The fields the flow goes on to use, in the types it uses them as: the
%% interval is slept on and the expiry is added to a timestamp.
device_authorization_fields(#{
    <<"device_code">> := DeviceCode,
    <<"user_code">> := UserCode,
    <<"verification_uri_complete">> := VerificationUri,
    <<"expires_in">> := ExpiresIn,
    <<"interval">> := Interval
}) when
    is_binary(DeviceCode),
    is_binary(UserCode),
    is_binary(VerificationUri),
    is_integer(ExpiresIn),
    is_integer(Interval),
    Interval >= 0
->
    {ok, DeviceCode, UserCode, VerificationUri, ExpiresIn, Interval};
device_authorization_fields(_DeviceResponse) ->
    error.

%% @private
poll_for_token_loop(Config, ClientId, DeviceCode, IntervalSeconds, ExpiresAt) ->
    Now = erlang:system_time(second),
    case Now >= ExpiresAt of
        true ->
            {error, timeout};
        false ->
            timer:sleep(IntervalSeconds * 1000),
            case poll_device_token(Config, ClientId, DeviceCode) of
                {ok, {200, _, TokenResponse}} ->
                    case token_response_fields(TokenResponse) of
                        {ok, AccessToken, ExpiresIn} ->
                            Tokens = #{
                                access_token => AccessToken,
                                expires_at => erlang:system_time(second) + ExpiresIn
                            },
                            {ok,
                                put_organization_reauth_required(
                                    put_refresh_token(Tokens, TokenResponse), TokenResponse
                                )};
                        error ->
                            {error, {poll_failed, 200, TokenResponse}}
                    end;
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
                {error, _Reason} ->
                    %% A request that did not get through says nothing about the
                    %% authorization, which the user may be minutes into. The
                    %% device code lives on the server until it expires, so keep
                    %% polling until then.
                    poll_for_token_loop(Config, ClientId, DeviceCode, IntervalSeconds, ExpiresAt)
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
%% Requests a browser URL to verify the organizations' SSO and 2FA requirements
%% for the current OAuth session.
%%
%% The session the access token belongs to is the one being authorized: its
%% owner completes the required verification in a browser. The next token
%% refresh carries the scopes again. The URL is single-use and short-lived.
%%
%% Examples:
%%
%% ```
%% 1> Config = r3_hex_core:default_config().
%% 2> r3_hex_api_oauth:organization_authorization(Config, [<<"acme">>]).
%% {ok, {201, _, #{
%%     <<"verification_uri">> => <<"https://hex.pm/organizations/authorize?code=...">>,
%%     <<"expires_in">> => 600
%% }}}
%% '''
%% @end
-spec organization_authorization(r3_hex_core:config(), [binary()]) -> r3_hex_api:response().
organization_authorization(Config, Organizations) ->
    Path = <<"oauth/organization_authorization">>,
    r3_hex_api:post(Config, Path, #{<<"organizations">> => Organizations}).

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
%% Exchanges an OIDC token from a CI provider for an access token scoped to
%% one package, using the JWT bearer grant (RFC 7523). This is what Workload
%% Identity uses in place of a stored API key.
%%
%% Examples:
%%
%% ```
%% 1> Config = r3_hex_core:default_config().
%% 2> r3_hex_api_oauth:jwt_bearer_token(Config, OidcToken, <<"package:hexpm/my_package">>).
%% {ok, {200, _, #{<<"access_token">> => <<"...">>, <<"expires_in">> => 900}}}
%% '''
%% @end
-spec jwt_bearer_token(r3_hex_core:config(), Assertion :: binary(), Scope :: binary()) ->
    r3_hex_api:response().
jwt_bearer_token(Config, Assertion, Scope) ->
    Path = <<"oauth/token">>,
    Params = #{
        <<"grant_type">> => <<"urn:ietf:params:oauth:grant-type:jwt-bearer">>,
        <<"assertion">> => Assertion,
        <<"scope">> => Scope
    },
    r3_hex_api:post(Config, Path, Params).

%% @doc
%% Fetches the audience Workload Identity OIDC tokens must be issued for.
%%
%% Examples:
%%
%% ```
%% 1> Config = r3_hex_core:default_config().
%% 2> r3_hex_api_oauth:oidc_audience(Config).
%% {ok, {200, _, #{<<"audience">> => <<"hexpm">>}}}
%% '''
%% @end
-spec oidc_audience(r3_hex_core:config()) -> r3_hex_api:response().
oidc_audience(Config) ->
    r3_hex_api:get(Config, <<"oidc/audience">>).

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

%% @doc
%% Missing organization authentication requirements in a token response.
%% An absent field means no outstanding requirements. Malformed fields are
%% rejected so callers cannot report successful authentication from them.
%% @end
-spec organization_reauth_required(map()) -> {ok, [map()]} | error.
organization_reauth_required(#{<<"organization_reauth_required">> := Entries}) when
    is_list(Entries)
->
    parse_organization_requirements(Entries, []);
organization_reauth_required(#{<<"organization_reauth_required">> := _}) ->
    error;
organization_reauth_required(_) ->
    {ok, []}.

parse_organization_requirements([], Acc) ->
    {ok, lists:reverse(Acc)};
parse_organization_requirements(
    [
        #{<<"organization">> := Name, <<"requirements">> := Requirements} | Rest
    ],
    Acc
) when is_binary(Name), byte_size(Name) > 0, Requirements =/= [] ->
    case valid_requirements(Requirements) of
        true ->
            parse_organization_requirements(
                Rest,
                [#{organization => Name, requirements => lists:usort(Requirements)} | Acc]
            );
        false ->
            error
    end;
parse_organization_requirements(_, _) ->
    error.

%% A proper list of known requirements. Decoded terms can carry an improper
%% list, which is_list/1 accepts and lists:all/2 would crash on.
valid_requirements([]) ->
    true;
valid_requirements([Requirement | Rest]) when
    Requirement =:= <<"tfa">>; Requirement =:= <<"sso">>
->
    valid_requirements(Rest);
valid_requirements(_) ->
    false.

%%====================================================================
%% Internal functions
%%====================================================================

%% @private
%% Opens a URL in the default browser using the platform's opener: `open' on
%% macOS, `xdg-open' on Linux, `start' on Windows. Returns
%% `{error, browser_not_found}' when none of them exists, which is the ordinary
%% case on a headless machine, and `{error, invalid_url}' for anything that is
%% not an http(s) URL.
-spec open_browser(binary()) -> ok | {error, browser_not_found | invalid_url}.
open_browser(Url) when is_binary(Url) ->
    case valid_http_url(Url) of
        true ->
            spawn_browser(binary_to_list(Url));
        false ->
            {error, invalid_url}
    end.

%% @private
spawn_browser(Url) ->
    {Cmd, Args} =
        case os:type() of
            {unix, darwin} ->
                {"open", [Url]};
            {unix, _} ->
                {"xdg-open", [Url]};
            {win32, _} ->
                {"cmd", win_cmd_args(Url)}
        end,
    case os:find_executable(Cmd) of
        false ->
            {error, browser_not_found};
        Executable ->
            open_port({spawn_executable, Executable}, [{args, Args}]),
            ok
    end.

%% @private
%% `start' takes its first quoted argument as the window title, so the empty
%% string keeps the URL in the position `start' opens.
-spec win_cmd_args(string()) -> [string()].
win_cmd_args(Url) ->
    ["/c", "start", "", escape_win_cmd(Url)].

%% @private
%% cmd.exe parses the command line before `start' sees it, and erts only quotes
%% an argument that contains whitespace, so every character cmd acts on is
%% prefixed with a caret. `%' is included because cmd expands `%NAME%' before it
%% scans for separators.
escape_win_cmd(Url) ->
    lists:flatmap(fun escape_win_cmd_character/1, Url).

%% @private
escape_win_cmd_character(Character) when
    Character =:= $^;
    Character =:= $&;
    Character =:= $|;
    Character =:= $<;
    Character =:= $>;
    Character =:= $(;
    Character =:= $);
    Character =:= $";
    Character =:= $%
->
    [$^, Character];
escape_win_cmd_character(Character) ->
    [Character].

%% @private
%% Whether a URL uses the http:// or https:// scheme.
-spec valid_http_url(binary()) -> boolean().
valid_http_url(Url) when is_binary(Url) ->
    case uri_string:parse(Url) of
        #{scheme := <<"https">>} -> true;
        #{scheme := <<"http">>} -> true;
        _ -> false
    end.

%% @private
%% The access token and its lifetime, in the types the caller uses them as.
token_response_fields(#{<<"access_token">> := AccessToken, <<"expires_in">> := ExpiresIn}) when
    is_binary(AccessToken), is_integer(ExpiresIn)
->
    {ok, AccessToken, ExpiresIn};
token_response_fields(_TokenResponse) ->
    error.

%% @private
%% A response without a refresh token carries no key at all rather than a
%% placeholder, so what a build tool stores is only ever a real token.
put_refresh_token(Tokens, #{<<"refresh_token">> := RefreshToken}) when is_binary(RefreshToken) ->
    Tokens#{refresh_token => RefreshToken};
put_refresh_token(Tokens, _TokenResponse) ->
    Tokens.

%% @private
%% A response whose organization_reauth_required cannot be read carries no key, so the
%% caller is not handed the empty set as if the server had sent it.
put_organization_reauth_required(Tokens, TokenResponse) ->
    case organization_reauth_required(TokenResponse) of
        {ok, Organizations} -> Tokens#{organization_reauth_required => Organizations};
        error -> Tokens
    end.

%% @private
%% Get the hostname of the current machine.
-spec get_hostname() -> binary().
get_hostname() ->
    {ok, Hostname} = inet:gethostname(),
    list_to_binary(Hostname).
