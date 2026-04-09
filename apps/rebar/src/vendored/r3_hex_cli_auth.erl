%% Vendored from hex_core v0.18.0, do not edit manually

%% @doc
%% Authentication handling with callback functions for build-tool-specific operations.
%%
%% This module provides generic authentication handling that allows both rebar3
%% and Elixir Hex (and future build tools) to share the common auth logic while
%% customizing prompting, persistence, and configuration retrieval.
%%
%% == Callbacks ==
%%
%% Callbacks are provided via the `cli_auth_callbacks' key in the config map.
%% All callbacks are required:
%%
%% ```
%% #{
%%     %% Auth configuration for a specific repo
%%     get_auth_config => fun((RepoName :: binary()) ->
%%         #{api_key => binary(),
%%           auth_key => binary(),
%%           oauth_exchange => boolean(),
%%           oauth_exchange_url => binary()} | undefined),
%%
%%     %% Global OAuth tokens - storage and retrieval
%%     get_oauth_tokens => fun(() -> {ok, #{access_token := binary(),
%%                                          refresh_token => binary(),
%%                                          expires_at := integer()}} | error),
%%     persist_oauth_tokens => fun((Scope :: global | binary(),
%%                                  AccessToken :: binary(),
%%                                  RefreshToken :: binary() | undefined,
%%                                  ExpiresAt :: integer()) -> ok),
%%
%%     %% User interaction
%%     prompt_otp => fun((Message :: binary()) -> {ok, OtpCode :: binary()} | cancelled),
%%     should_authenticate => fun((Reason :: no_credentials | token_refresh_failed) -> boolean()),
%%
%%     %% OAuth client configuration
%%     get_client_id => fun(() -> binary())
%% }
%% '''
%%
%% == Auth Resolution Order ==
%%
%% For API calls:
%% <ol>
%% <li>Per-repo `api_key' from config (with optional OAuth exchange for hex.pm)</li>
%% <li>Parent repo `api_key' (for "hexpm:org" organizations)</li>
%% <li>Global OAuth token (refreshed if expired)</li>
%% <li>Device auth flow (for write operations only)</li>
%% </ol>
%%
%% For repo calls:
%% <ol>
%% <li>Per-repo `auth_key' with optional OAuth exchange (default true for hex.pm)</li>
%% <li>Parent repo `auth_key'</li>
%% <li>Global OAuth token</li>
%% </ol>
%%
%% == OAuth Exchange ==
%%
%% For hex.pm URLs, `api_key' and `auth_key' are exchanged for short-lived OAuth
%% tokens via the client credentials grant. This behavior can be controlled per-repo
%% via the `oauth_exchange' option in the repo config (defaults to `true' for hex.pm).
%%
%% == Auth Context ==
%%
%% Internally, authentication resolution tracks context via `auth_context()':
%% <ul>
%% <li>`source' - Where the credentials came from (`env', `config', or `oauth')</li>
%% <li>`has_refresh_token' - Whether token refresh is possible on 401</li>
%% </ul>
%%
%% == Token Format ==
%%
%% OAuth access tokens are automatically prefixed with `<<"Bearer ">>' when used
%% as `api_key' or `repo_key' in the config.
-module(r3_hex_cli_auth).

-export([
    with_api/3,
    with_api/4,
    with_repo/2,
    with_repo/3,
    resolve_api_auth/2,
    resolve_repo_auth/1
]).

-export_type([
    callbacks/0,
    permission/0,
    auth_error/0,
    auth_context/0,
    repo_auth_config/0,
    auth_prompt_reason/0,
    opts/0
]).

%% 5 minute buffer before expiry
-define(EXPIRY_BUFFER_SECONDS, 300).

%% Maximum OTP retry attempts
-define(MAX_OTP_RETRIES, 3).

-type permission() :: read | write.

-type callbacks() :: #{
    get_auth_config := fun((RepoName :: binary()) -> repo_auth_config() | undefined),
    get_oauth_tokens := fun(() -> {ok, oauth_tokens()} | error),
    persist_oauth_tokens := fun(
        (
            Scope :: global | binary(),
            AccessToken :: binary(),
            RefreshToken :: binary() | undefined,
            ExpiresAt :: integer()
        ) -> ok
    ),
    prompt_otp := fun((Message :: binary()) -> {ok, OtpCode :: binary()} | cancelled),
    should_authenticate := fun((Reason :: auth_prompt_reason()) -> boolean()),
    get_client_id := fun(() -> binary())
}.

-type auth_prompt_reason() ::
    no_credentials
    | token_refresh_failed.

-type repo_auth_config() :: #{
    api_key => binary(),
    repo_key => binary(),
    auth_key => binary(),
    oauth_token => oauth_tokens()
}.

-type oauth_tokens() :: #{
    access_token := binary(),
    refresh_token => binary(),
    expires_at := integer()
}.

-type auth_error() ::
    {auth_error, no_credentials}
    | {auth_error, auth_declined}
    | {auth_error, otp_cancelled}
    | {auth_error, otp_max_retries}
    | {auth_error, token_refresh_failed}
    | {auth_error, device_auth_timeout}
    | {auth_error, device_auth_denied}
    | {auth_error, oauth_exchange_failed}
    | {auth_error, term()}.

-type auth_context() :: #{
    source => env | config | oauth,
    has_refresh_token => boolean()
}.

-type opts() :: [
    {optional, boolean()}
    | {auth_inline, boolean()}
    | {oauth_open_browser, boolean()}
].

%%====================================================================
%% API functions
%%====================================================================

%% @doc
%% Execute a function with API authentication.
%%
%% Equivalent to `with_api(Permission, Config, Fun, [])'.
%%
%% @see with_api/4
-spec with_api(permission(), r3_hex_core:config(), fun((r3_hex_core:config()) -> Result)) ->
    Result | {error, auth_error()}
when
    Result :: term().
with_api(Permission, BaseConfig, Fun) ->
    with_api(Permission, BaseConfig, Fun, []).

%% @doc
%% Execute a function with API authentication.
%%
%% Resolves credentials in this order:
%% <ol>
%% <li>Per-repo `api_key' from config (with optional OAuth exchange for hex.pm)</li>
%% <li>Parent repo `api_key' (for "hexpm:org" organizations)</li>
%% <li>Global OAuth token (refreshed if expired)</li>
%% <li>Device auth flow (when `should_authenticate' callback returns true)</li>
%% </ol>
%%
%% On 401 responses, handles OTP prompts and token refresh automatically.
%%
%% The repository name is taken from the config (`repo_name' or `repo_organization').
%%
%% Callbacks are taken from the `cli_auth_callbacks' key in the config map.
%%
%% Options:
%% <ul>
%% <li>`optional' - When `true', if no credentials are found, executes the function
%%     without authentication first. If the server returns 401, triggers auth
%%     (respecting `auth_inline'). When `false' (default), missing credentials
%%     immediately triggers the `should_authenticate' callback.</li>
%% <li>`auth_inline' - When `true' (default), prompts the user via `should_authenticate'
%%     callback when authentication is needed. When `false', returns
%%     `{error, {auth_error, no_credentials}}' instead of prompting.</li>
%% <li>`oauth_open_browser' - When `true' (default), automatically opens the browser
%%     during device auth flow. When `false', only prints the URL for the user.</li>
%% </ul>
%%
%% Example:
%% ```
%% r3_hex_cli_auth:with_api(write, Config, fun(C) ->
%%     r3_hex_api_release:publish(C, Tarball)
%% end, [{optional, false}, {auth_inline, true}]).
%% '''
-spec with_api(
    permission(),
    r3_hex_core:config(),
    fun((r3_hex_core:config()) -> Result),
    opts()
) ->
    Result | {error, auth_error()}
when
    Result :: term().
with_api(Permission, BaseConfig, Fun, Opts) ->
    Optional = proplists:get_value(optional, Opts, false),
    AuthInline = proplists:get_value(auth_inline, Opts, true),
    case resolve_api_auth(Permission, BaseConfig) of
        {ok, ApiKey, AuthContext} ->
            Config = BaseConfig#{api_key => ApiKey},
            execute_with_retry(Config, Fun, AuthContext, 0, undefined, Opts);
        {error, no_auth} when Optional =:= true ->
            %% Auth is optional, try without credentials first
            execute_optional_with_retry(BaseConfig, Fun, Opts);
        {error, no_auth} when AuthInline =:= true ->
            %% No auth found, ask user if they want to authenticate
            maybe_authenticate_and_retry(BaseConfig, Fun, no_credentials, Opts);
        {error, no_auth} ->
            %% auth_inline is false, just return error
            {error, {auth_error, no_credentials}};
        {error, {auth_error, token_refresh_failed}} when Optional =:= true ->
            %% Token refresh failed but auth is optional, fall back to no credentials
            execute_optional_with_retry(BaseConfig, Fun, Opts);
        {error, _} = Error ->
            Error
    end.

%% @doc
%% Execute a function with repository authentication.
%%
%% Equivalent to `with_repo(Config, Fun, [])'.
%%
%% @see with_repo/3
-spec with_repo(r3_hex_core:config(), fun((r3_hex_core:config()) -> Result)) ->
    Result | {error, auth_error()}
when
    Result :: term().
with_repo(BaseConfig, Fun) ->
    with_repo(BaseConfig, Fun, []).

%% @doc
%% Execute a function with repository authentication.
%%
%% Resolves credentials in this order:
%% <ol>
%% <li>`repo_key' in config - passthrough</li>
%% <li>`repo_key' from `get_auth_config' callback - passthrough</li>
%% <li>`auth_key' from `get_auth_config' when `trusted' is true and `oauth_exchange' is true - exchange for OAuth token</li>
%% <li>`auth_key' from `get_auth_config' when `trusted' is true - use directly</li>
%% <li>Global OAuth token from `get_oauth_tokens' callback</li>
%% <li>No auth when `optional' is true (with retry on 401)</li>
%% <li>Prompt via `should_authenticate' when `auth_inline' is true</li>
%% </ol>
%%
%% The repository name is taken from the config (`repo_name' or `repo_organization').
%%
%% Callbacks are taken from the `cli_auth_callbacks' key in the config map.
%%
%% Options:
%% <ul>
%% <li>`optional' - When `true' (default), proceeds without auth if none found; retries with auth on 401.</li>
%% <li>`auth_inline' - When `true', prompts user via `should_authenticate' callback. Default is `false'.</li>
%% <li>`oauth_open_browser' - When `true' (default), automatically opens the browser
%%     during device auth flow. When `false', only prints the URL for the user.</li>
%% </ul>
%%
%% Example:
%% ```
%% r3_hex_cli_auth:with_repo(Config, fun(C) ->
%%     r3_hex_repo:get_tarball(C, <<"ecto">>, <<"3.0.0">>)
%% end).
%% '''
-spec with_repo(r3_hex_core:config(), fun((r3_hex_core:config()) -> Result), opts()) ->
    Result | {error, auth_error()}
when
    Result :: term().
with_repo(BaseConfig, Fun, Opts) ->
    Optional = proplists:get_value(optional, Opts, true),
    AuthInline = proplists:get_value(auth_inline, Opts, false),
    case resolve_repo_auth(BaseConfig) of
        {ok, RepoKey, _AuthContext} when is_binary(RepoKey) ->
            Config = BaseConfig#{repo_key => RepoKey},
            Fun(Config);
        no_auth when Optional =:= true ->
            %% Auth is optional, try without credentials first
            execute_optional_with_retry(BaseConfig, Fun, Opts);
        no_auth when AuthInline =:= true ->
            %% No auth found, ask user if they want to authenticate
            maybe_authenticate_and_retry(BaseConfig, Fun, no_credentials, Opts);
        no_auth ->
            %% auth_inline is false, return error
            {error, {auth_error, no_credentials}};
        {error, {auth_error, token_refresh_failed}} when Optional =:= true ->
            %% Token refresh failed but auth is optional, fall back to no credentials
            execute_optional_with_retry(BaseConfig, Fun, Opts);
        {error, _} = Error ->
            Error
    end.

%% @private
%% Extract repository name from config.
-spec repo_name(r3_hex_core:config()) -> binary().
repo_name(#{repo_name := Name, repo_organization := Org}) when is_binary(Name) and is_binary(Org) ->
    <<Name/binary, ":", Org/binary>>;
repo_name(#{repo_name := Name}) when is_binary(Name) -> Name;
repo_name(_) ->
    <<"hexpm">>.

%% @private
%% Ask user if they want to authenticate, and if yes, initiate device auth.
%%
%% Serialized with a global lock so concurrent callers don't each trigger their
%% own device auth flow. The first caller to acquire the lock runs device auth
%% and persists the resulting token; subsequent callers re-check for an existing
%% (now-valid) token inside the lock and reuse it instead of re-authenticating.
maybe_authenticate_and_retry(BaseConfig, Fun, Reason, Opts) ->
    global:trans(
        {{?MODULE, device_auth}, self()},
        fun() ->
            do_maybe_authenticate_and_retry(BaseConfig, Fun, Reason, Opts)
        end,
        [node()],
        infinity
    ).

%% @private
%% Another caller may have authenticated while we waited for the lock. Re-resolve
%% and, if we get a token that differs from the one we arrived with (none when
%% credentials were missing; the rejected one on token_refresh_failed), reuse it
%% instead of prompting again. Otherwise proceed to prompt + device auth.
do_maybe_authenticate_and_retry(BaseConfig, Fun, Reason, Opts) ->
    CurrentApiKey = maps:get(api_key, BaseConfig, undefined),
    case resolve_api_auth(write, BaseConfig) of
        {ok, ApiKey, AuthContext} when ApiKey =/= CurrentApiKey ->
            Config = BaseConfig#{api_key => ApiKey},
            execute_with_retry(Config, Fun, AuthContext, 0, undefined, Opts);
        _ ->
            prompt_and_device_auth(BaseConfig, Fun, Reason, Opts)
    end.

%% @private
prompt_and_device_auth(BaseConfig, Fun, Reason, Opts) ->
    case call_callback(BaseConfig, should_authenticate, [Reason]) of
        true ->
            case device_auth(BaseConfig, <<"api repositories">>, Opts) of
                {ok, #{access_token := Token}} ->
                    BearerToken = <<"Bearer ", Token/binary>>,
                    Config = BaseConfig#{api_key => BearerToken},
                    AuthContext = #{source => oauth, has_refresh_token => true},
                    execute_with_retry(Config, Fun, AuthContext, 0, undefined, Opts);
                {error, _} = Error ->
                    Error
            end;
        false ->
            {error, {auth_error, auth_declined}}
    end.

%% @private
%% Execute function without auth, but retry with auth if we get a 401.
execute_optional_with_retry(BaseConfig, Fun, Opts) ->
    AuthInline = proplists:get_value(auth_inline, Opts, true),
    case Fun(BaseConfig) of
        {ok, {401, _Headers, _Body}} when AuthInline =:= true ->
            %% Got 401, need auth - ask user if they want to authenticate
            maybe_authenticate_and_retry(BaseConfig, Fun, no_credentials, Opts);
        {ok, {401, _Headers, _Body}} ->
            %% Got 401 but auth_inline is false, return error
            {error, {auth_error, no_credentials}};
        Other ->
            Other
    end.

%%====================================================================
%% Internal functions - Device Auth
%%====================================================================

%% @private
%% Initiate OAuth device authorization flow.
%% Prompts user, optionally opens the browser for user authentication,
%% polls for token completion, and persists tokens via callback on success.
-spec device_auth(r3_hex_core:config(), binary(), opts()) ->
    {ok, oauth_tokens()} | {error, auth_error()}.
device_auth(Config, Scope, Opts) ->
    ClientId = call_callback(Config, get_client_id, []),
    OpenBrowser = proplists:get_value(oauth_open_browser, Opts, true),
    PromptUser = fun(VerificationUri, UserCode) ->
        io:format("Open ~ts in your browser and enter code: ~ts~n", [VerificationUri, UserCode])
    end,
    FlowOpts = [{open_browser, OpenBrowser}],
    case r3_hex_api_oauth:device_auth_flow(Config, ClientId, Scope, PromptUser, FlowOpts) of
        {ok, #{access_token := AccessToken, refresh_token := RefreshToken, expires_at := ExpiresAt}} ->
            ok = call_callback(Config, persist_oauth_tokens, [
                global, AccessToken, RefreshToken, ExpiresAt
            ]),
            {ok, #{
                access_token => AccessToken,
                refresh_token => RefreshToken,
                expires_at => ExpiresAt
            }};
        {error, timeout} ->
            {error, {auth_error, device_auth_timeout}};
        {error, {access_denied, _Status, _Body}} ->
            {error, {auth_error, device_auth_denied}};
        {error, {device_auth_failed, _Status, _Body} = Reason} ->
            {error, {auth_error, Reason}};
        {error, {poll_failed, _Status, _Body} = Reason} ->
            {error, {auth_error, Reason}};
        {error, Reason} ->
            {error, {auth_error, Reason}}
    end.

%% @private
%% Check if a token is expired (within 5 minute buffer).
-spec is_token_expired(integer()) -> boolean().
is_token_expired(ExpiresAt) ->
    Now = erlang:system_time(second),
    ExpiresAt - Now < ?EXPIRY_BUFFER_SECONDS.

%%====================================================================
%% Internal functions - Auth Resolution
%%====================================================================

%% @private
-spec resolve_api_auth(permission(), r3_hex_core:config()) ->
    {ok, binary(), auth_context()} | {error, no_auth} | {error, auth_error()}.
resolve_api_auth(_Permission, #{api_key := ApiKey}) when is_binary(ApiKey) ->
    %% api_key already in config, pass through directly
    {ok, ApiKey, #{source => config, has_refresh_token => false}};
resolve_api_auth(_Permission, Config) ->
    RepoName = repo_name(Config),
    %% 1. Check per-repo api_key
    case call_callback(Config, get_auth_config, [RepoName]) of
        #{api_key := ApiKey} when is_binary(ApiKey) ->
            {ok, ApiKey, #{source => config, has_refresh_token => false}};
        _ ->
            %% 2. Check parent repo (for "hexpm:org" organizations)
            case get_parent_repo_key(Config, RepoName, api_key) of
                {ok, ApiKey} ->
                    {ok, ApiKey, #{source => config, has_refresh_token => false}};
                error ->
                    %% 3. Try global OAuth token
                    resolve_oauth_token_with_context(Config)
            end
    end.

%% @private
%% Resolve repo auth credentials in this order:
%% 0. repo_key in config => passthrough
%% 1. repo_key from get_auth_config => passthrough
%% 2. trusted + auth_key + oauth_exchange => exchange for OAuth token
%% 3. trusted + auth_key => use directly
%% 4. trusted + global OAuth tokens => use those
%% 5. Fallthrough to no_auth (handled by with_repo/3 for optional/auth_inline)
-spec resolve_repo_auth(r3_hex_core:config()) ->
    {ok, binary(), auth_context()} | no_auth | {error, auth_error()}.
resolve_repo_auth(#{repo_key := RepoKey}) when is_binary(RepoKey) ->
    %% repo_key already in config, pass through directly
    {ok, RepoKey, #{source => config, has_refresh_token => false}};
resolve_repo_auth(Config) ->
    RepoName = repo_name(Config),
    global:trans(
        {{?MODULE, repo, RepoName}, self()},
        fun() ->
            do_resolve_repo_auth(RepoName, RepoName, Config)
        end,
        [node()],
        infinity
    ).

do_resolve_repo_auth(RepoName, LookupRepo, Config) ->
    Trusted = maps:get(trusted, Config, false),
    OAuthExchange = maps:get(oauth_exchange, Config, false),
    case call_callback(Config, get_auth_config, [LookupRepo]) of
        #{repo_key := RepoKey} when is_binary(RepoKey) ->
            %% 1. repo_key from get_auth_config => passthrough
            {ok, RepoKey, #{source => config, has_refresh_token => false}};
        #{oauth_token := OAuthToken, auth_key := AuthKey} when
            is_binary(AuthKey) and OAuthExchange, Trusted
        ->
            %% 2. trusted + oauth_token + auth_key + oauth_exchange => use/refresh existing token
            resolve_repo_oauth_token(RepoName, Config, AuthKey, OAuthToken);
        #{auth_key := AuthKey} when is_binary(AuthKey) and OAuthExchange, Trusted ->
            %% 3. trusted + auth_key + oauth_exchange => exchange for new OAuth token
            exchange_for_oauth_token(RepoName, Config, AuthKey, <<"repositories">>);
        #{auth_key := AuthKey} when is_binary(AuthKey), Trusted ->
            %% 4. trusted + auth_key => use directly
            {ok, AuthKey, #{source => config, has_refresh_token => false}};
        _ when Trusted ->
            %% 5. Check parent repo (for "hexpm:org" organizations)
            case binary:split(LookupRepo, <<":">>) of
                [ParentName, _OrgName] ->
                    do_resolve_repo_auth(RepoName, ParentName, Config);
                _ ->
                    %% 6. trusted + global OAuth tokens => use those
                    resolve_global_oauth_for_repo(Config)
            end;
        _ ->
            %% 7. Not trusted, no auth
            no_auth
    end.

%% @private
resolve_global_oauth_for_repo(Config) ->
    case resolve_oauth_token_with_context(Config) of
        {ok, Token, AuthContext} ->
            {ok, Token, AuthContext};
        {error, no_auth} ->
            no_auth;
        {error, _} = Error ->
            Error
    end.

%% @private
%% Resolve repo OAuth token: use if valid, re-exchange if expiring.
resolve_repo_oauth_token(RepoName, Config, AuthKey, #{
    access_token := AccessToken, expires_at := ExpiresAt
}) ->
    case is_token_expired(ExpiresAt) of
        false ->
            %% Token is still valid, use it
            BearerToken = <<"Bearer ", AccessToken/binary>>,
            {ok, BearerToken, #{source => oauth, has_refresh_token => false}};
        true ->
            %% Token expired, do a new exchange
            exchange_for_oauth_token(RepoName, Config, AuthKey, <<"repositories">>)
    end.

%% @private
%% Exchange api_key/auth_key for OAuth token via client credentials grant.
%% Persists the token with the repo name for per-repo token storage.
exchange_for_oauth_token(RepoName, Config, AuthKey, Scope) ->
    ClientId = call_callback(Config, get_client_id, []),
    ExchangeConfig =
        case maps:get(oauth_exchange_url, Config, undefined) of
            undefined -> Config;
            OAuthUrl -> Config#{api_url => OAuthUrl}
        end,
    case r3_hex_api_oauth:client_credentials_token(ExchangeConfig, ClientId, AuthKey, Scope) of
        {ok, {200, _, #{<<"access_token">> := AccessToken, <<"expires_in">> := ExpiresIn}}} ->
            ExpiresAt = erlang:system_time(second) + ExpiresIn,
            ok = call_callback(Config, persist_oauth_tokens, [
                RepoName, AccessToken, undefined, ExpiresAt
            ]),
            BearerToken = <<"Bearer ", AccessToken/binary>>,
            {ok, BearerToken, #{source => oauth, has_refresh_token => false}};
        {ok, {_Status, _, _Body}} ->
            {error, {auth_error, oauth_exchange_failed}};
        {error, _} ->
            {error, {auth_error, oauth_exchange_failed}}
    end.

%% @private
get_parent_repo_key(Config, RepoName, KeyType) ->
    case binary:split(RepoName, <<":">>) of
        [ParentName, _OrgName] ->
            case call_callback(Config, get_auth_config, [ParentName]) of
                #{KeyType := Key} when is_binary(Key) ->
                    {ok, Key};
                _ ->
                    error
            end;
        _ ->
            error
    end.

%% @private
%% Resolve OAuth token with global lock to prevent concurrent refresh attempts.
resolve_oauth_token_with_context(Config) ->
    global:trans(
        {{?MODULE, token_refresh}, self()},
        fun() ->
            do_resolve_oauth_token_with_context(Config)
        end,
        [node()],
        infinity
    ).

%% @private
do_resolve_oauth_token_with_context(Config) ->
    case call_callback(Config, get_oauth_tokens, []) of
        {ok, #{access_token := AccessToken, expires_at := ExpiresAt} = Tokens} ->
            HasRefreshToken =
                maps:is_key(refresh_token, Tokens) andalso
                    is_binary(maps:get(refresh_token, Tokens)),
            case is_token_expired(ExpiresAt) of
                true ->
                    maybe_refresh_token_with_context(Config, Tokens);
                false ->
                    BearerToken = <<"Bearer ", AccessToken/binary>>,
                    {ok, BearerToken, #{source => oauth, has_refresh_token => HasRefreshToken}}
            end;
        error ->
            {error, no_auth}
    end.

%% @private
maybe_refresh_token_with_context(Config, #{refresh_token := RefreshToken}) when
    is_binary(RefreshToken)
->
    ClientId = call_callback(Config, get_client_id, []),
    case r3_hex_api_oauth:refresh_token(Config, ClientId, RefreshToken) of
        {ok, {200, _, TokenResponse}} when is_map(TokenResponse) ->
            #{
                <<"access_token">> := NewAccessToken,
                <<"expires_in">> := ExpiresIn
            } = TokenResponse,
            NewRefreshToken = maps:get(<<"refresh_token">>, TokenResponse, RefreshToken),
            ExpiresAt = erlang:system_time(second) + ExpiresIn,
            ok = call_callback(Config, persist_oauth_tokens, [
                global, NewAccessToken, NewRefreshToken, ExpiresAt
            ]),
            BearerToken = <<"Bearer ", NewAccessToken/binary>>,
            HasRefreshToken = is_binary(NewRefreshToken),
            {ok, BearerToken, #{source => oauth, has_refresh_token => HasRefreshToken}};
        {ok, {_Status, _, _Body}} ->
            {error, {auth_error, token_refresh_failed}};
        {error, _Reason} ->
            {error, {auth_error, token_refresh_failed}}
    end;
maybe_refresh_token_with_context(_Config, _Tokens) ->
    {error, {auth_error, token_refresh_failed}}.

%%====================================================================
%% Internal functions - Retry Logic
%%====================================================================

%% @private
execute_with_retry(Config, Fun, AuthContext, OtpRetries, LastOtpError, Opts) ->
    case Fun(Config) of
        {error, otp_required} ->
            handle_otp_retry(
                Config, Fun, AuthContext, OtpRetries, <<"Enter OTP code:">>, Opts
            );
        {error, invalid_totp} ->
            handle_otp_retry(
                Config,
                Fun,
                AuthContext,
                OtpRetries,
                <<"Invalid OTP code. Please try again:">>,
                Opts
            );
        {ok, {401, Headers, _Body}} = Response ->
            case detect_auth_error(Headers) of
                otp_required ->
                    handle_otp_retry(
                        Config, Fun, AuthContext, OtpRetries, <<"Enter OTP code:">>, Opts
                    );
                invalid_totp ->
                    Msg =
                        case LastOtpError of
                            invalid_totp -> <<"Invalid OTP code. Please try again:">>;
                            _ -> <<"Enter OTP code:">>
                        end,
                    handle_otp_retry(Config, Fun, AuthContext, OtpRetries, Msg, Opts);
                token_expired ->
                    handle_token_refresh_retry(Config, Fun, AuthContext, Opts);
                none ->
                    Response
            end;
        Other ->
            Other
    end.

%% @private
handle_otp_retry(_Config, _Fun, _AuthContext, OtpRetries, _Message, _Opts) when
    OtpRetries >= ?MAX_OTP_RETRIES
->
    {error, {auth_error, otp_max_retries}};
handle_otp_retry(Config, Fun, AuthContext, OtpRetries, Message, Opts) ->
    case call_callback(Config, prompt_otp, [Message]) of
        {ok, OtpCode} ->
            NewConfig = Config#{api_otp => OtpCode},
            execute_with_retry(
                NewConfig, Fun, AuthContext, OtpRetries + 1, invalid_totp, Opts
            );
        cancelled ->
            {error, {auth_error, otp_cancelled}}
    end.

%% @private
handle_token_refresh_retry(Config, Fun, AuthContext, Opts) ->
    %% Only attempt refresh if we have a refresh token
    case maps:get(has_refresh_token, AuthContext, false) of
        true ->
            case resolve_oauth_token_with_context(Config) of
                {ok, NewBearerToken, NewAuthContext} ->
                    NewConfig = Config#{api_key => NewBearerToken},
                    execute_with_retry(
                        NewConfig, Fun, NewAuthContext, 0, undefined, Opts
                    );
                {error, _} ->
                    maybe_reauthenticate(Config, Fun, Opts)
            end;
        false ->
            maybe_reauthenticate(Config, Fun, Opts)
    end.

%% @private
%% After token refresh failure, prompt the user to re-authenticate via device auth
%% (only when auth_inline is true). Mirrors Hex.OAuth.reauthenticate/1.
maybe_reauthenticate(Config, Fun, Opts) ->
    AuthInline = proplists:get_value(auth_inline, Opts, true),
    case AuthInline of
        true ->
            maybe_authenticate_and_retry(Config, Fun, token_refresh_failed, Opts);
        false ->
            {error, {auth_error, token_refresh_failed}}
    end.

%% @private
-spec detect_auth_error(r3_hex_http:headers()) -> otp_required | invalid_totp | token_expired | none.
detect_auth_error(Headers) ->
    case maps:get(<<"www-authenticate">>, Headers, undefined) of
        undefined ->
            none;
        Value ->
            parse_www_authenticate(Value)
    end.

%% @private
parse_www_authenticate(Value) when is_binary(Value) ->
    case Value of
        <<"Bearer realm=\"hex\", error=\"totp_required\"", _/binary>> ->
            otp_required;
        <<"Bearer realm=\"hex\", error=\"invalid_totp\"", _/binary>> ->
            invalid_totp;
        <<"Bearer realm=\"hex\", error=\"token_expired\"", _/binary>> ->
            token_expired;
        _ ->
            none
    end.

%%====================================================================
%% Internal functions - Utilities
%%====================================================================

%% @private
call_callback(Config, Name, Args) ->
    #{cli_auth_callbacks := Callbacks} = Config,
    Fun = maps:get(Name, Callbacks),
    erlang:apply(Fun, Args).
