%% Vendored from hex_core v0.20.1, do not edit manually

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
%% All callbacks below are required unless marked optional:
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
%%     %% Invalidate the stored global OAuth token after the server refused to
%%     %% refresh it (optional). Lets the build tool drop the unusable
%%     %% token so concurrent and subsequent callers stop retrying the doomed
%%     %% refresh, and warn the user. Invoked at most once per resolution, while
%%     %% holding the token-refresh lock.
%%     clear_oauth_tokens => fun(() -> ok),
%%
%%     %% Report the organizations the server says this session has to
%%     %% complete SSO or 2FA verification for (optional). Called
%%     %% after every token grant that carried a readable set, with the empty
%%     %% list when there are none, so the build tool always holds the current
%%     %% set. It is not told which of them the running command needs; deciding
%%     %% that is the build tool's job.
%%     organization_reauth => fun(([map()]) -> ok),
%%
%%     %% Outcome of the last Workload Identity exchange for a repository
%%     %% (optional, both or neither). Hex.pm organization repositories are
%%     %% only fetched with a workload identity when the build tool keeps the
%%     %% outcome, so concurrent and later requests reuse a token, or the
%%     %% failure, instead of each exchanging a new OIDC token. Keep it in
%%     %% memory for the rest of the run, the token expires in minutes.
%%     get_workload_identity_token => fun((RepoName :: binary()) ->
%%         {ok, #{access_token := binary(), expires_at := integer()}}
%%         | {error, workload_identity_error()}
%%         | error),
%%     persist_workload_identity_token => fun((RepoName :: binary(),
%%         {ok, #{access_token := binary(), expires_at := integer()}}
%%         | {error, workload_identity_error()}) -> ok),
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
%% <li>Workload Identity token (for "hexpm:org" organizations)</li>
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
%% <li>`has_refresh_token' - Whether token refresh is possible on 401</li>
%% </ul>
%%
%% == Token Format ==
%%
%% OAuth access tokens are automatically prefixed with `<<"Bearer ">>' when used
%% as `api_key' or `repo_key' in the config.
%%
%% == Workload Identity ==
%%
%% Workload Identity, sometimes also known as "Trusted Publishing", lets a
%% supported CI job (currently GitHub Actions) publish and fetch without a
%% stored key. `workload_identity_auth/2' exchanges the job's OIDC token for API
%% auth scoped to one package. See its documentation for details.
%%
%% A repository request to a Hex.pm organization ("hexpm:org") that resolves
%% no other credentials exchanges the job's OIDC token for a token scoped to
%% the organization's repository, when the build tool provides the
%% `get_workload_identity_token' and `persist_workload_identity_token'
%% callbacks. A failed exchange isn't repeated in the same run.
-module(r3_hex_cli_auth).

-export([
    with_api/3,
    with_api/4,
    with_repo/2,
    with_repo/3,
    resolve_api_auth/2,
    resolve_repo_auth/1,
    refresh_tokens/1,
    is_token_expired/1,
    workload_identity_auth/2
]).

-export_type([
    callbacks/0,
    permission/0,
    auth_error/0,
    auth_context/0,
    repo_auth_config/0,
    auth_prompt_reason/0,
    opts/0,
    workload_identity_error/0
]).

%% 5 minute buffer before expiry
-define(EXPIRY_BUFFER_SECONDS, 300).

%% Maximum OTP retry attempts
-define(MAX_OTP_RETRIES, 3).

%% Maximum times a 401 that says the token expired is answered by renewing the
%% credential and running the request again.
-define(MAX_TOKEN_RETRIES, 1).

%% Both ways a refresh can leave us without a usable token: the server refused
%% the refresh token, or the refresh got no usable answer.
-define(IS_REFRESH_FAILURE(Reason),
    (Reason =:= token_refresh_failed orelse Reason =:= token_refresh_unavailable)
).

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
    clear_oauth_tokens => fun(() -> ok),
    organization_reauth => fun((Organizations :: [map()]) -> ok),
    get_workload_identity_token => fun(
        (RepoName :: binary()) -> {ok, oauth_tokens()} | {error, workload_identity_error()} | error
    ),
    persist_workload_identity_token => fun(
        (RepoName :: binary(), {ok, oauth_tokens()} | {error, workload_identity_error()}) -> ok
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
    %% The server refused the refresh token: 400 or 401
    | {auth_error, token_refresh_failed}
    %% The refresh got no usable answer: DNS, connect, timeout, TLS, a 429 or a
    %% 5xx, or a 200 whose body is not a token response
    | {auth_error, token_refresh_unavailable}
    | {auth_error, device_auth_timeout}
    | {auth_error, device_auth_denied}
    | {auth_error, oauth_exchange_failed}
    | {auth_error, {workload_identity_failed, workload_identity_error()}}
    | {auth_error, term()}.

-type auth_context() :: #{
    has_refresh_token => boolean()
}.

-type workload_identity_error() ::
    r3_hex_oidc:fetch_error()
    | {oidc_audience_failed, r3_hex_api:response()}
    | {token_exchange_failed, r3_hex_api:response()}.

%% How much of each retry budget a request has already spent.
-type retries() :: #{
    otp := non_neg_integer(),
    otp_error := invalid_totp | undefined,
    token := non_neg_integer()
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
            execute_with_retry(Config, Fun, AuthContext, initial_retries(), Opts);
        {error, no_auth} when Optional =:= true ->
            %% Auth is optional, try without credentials first
            execute_optional_with_retry(api, BaseConfig, Fun, AuthInline, Opts);
        {error, no_auth} when AuthInline =:= true ->
            %% No auth found, ask user if they want to authenticate
            maybe_authenticate_and_retry(
                api, BaseConfig, Fun, no_credentials, initial_retries(), Opts
            );
        {error, no_auth} ->
            %% auth_inline is false, just return error
            {error, {auth_error, no_credentials}};
        {error, {auth_error, Reason}} when Optional =:= true, ?IS_REFRESH_FAILURE(Reason) ->
            %% Token refresh failed but auth is optional, fall back to no credentials
            execute_optional_with_retry(api, BaseConfig, Fun, AuthInline, Opts);
        {error, {auth_error, token_refresh_failed}} when AuthInline =:= true ->
            %% The server refused the refresh token, which leaves us with no
            %% usable token, same as having none, so it gets the same offer to
            %% authenticate. Without this the caller that asked to be prompted up
            %% front is told to run mix hex.user auth instead of being asked.
            %%
            %% Not token_refresh_unavailable: there the refresh got no answer at
            %% all, so the token may well still be good and the network is what
            %% is wrong. Offering a device flow that needs the same network is no
            %% help.
            maybe_authenticate_and_retry(
                api, BaseConfig, Fun, token_refresh_failed, initial_retries(), Opts
            );
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
%% <li>Global OAuth token from `get_oauth_tokens' callback for Hex.pm repositories</li>
%% <li>Workload Identity token for Hex.pm organization repositories, when the
%%     build tool provides the Workload Identity callbacks and the CI job can
%%     issue OIDC tokens. After a failed exchange the function runs without
%%     credentials when `optional' is true, and a 401 or 403 is returned as
%%     `{error, {auth_error, {workload_identity_failed, Reason}}}'.</li>
%% <li>No auth when `optional' is true (with retry on 401)</li>
%% <li>Prompt via `should_authenticate' when `auth_inline' is true</li>
%% </ol>
%%
%% A resolved token the server answers with a `token_expired' 401 is renewed at
%% its source (a per-repo or Workload Identity token is exchanged again, the
%% global token is refreshed) and the request is run once more.
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
            execute_repo_with_retry(BaseConfig, Fun, RepoKey);
        no_auth when Optional =:= true ->
            %% Auth is optional, try without credentials first
            execute_optional_with_retry(repo, BaseConfig, Fun, AuthInline, Opts);
        no_auth when AuthInline =:= true ->
            %% No auth found, ask user if they want to authenticate
            maybe_authenticate_and_retry(
                repo, BaseConfig, Fun, no_credentials, initial_retries(), Opts
            );
        no_auth ->
            %% auth_inline is false, return error
            {error, {auth_error, no_credentials}};
        {error, {auth_error, Reason}} when Optional =:= true, ?IS_REFRESH_FAILURE(Reason) ->
            %% Token refresh failed but auth is optional, fall back to no credentials
            execute_optional_with_retry(repo, BaseConfig, Fun, AuthInline, Opts);
        {error, {auth_error, {workload_identity_failed, _Reason}}} = Error when Optional =:= true ->
            execute_without_workload_identity(BaseConfig, Fun, Error);
        {error, _} = Error ->
            Error
    end.

%% @private
%% The workload identity was only picked up from the CI job, so a failed
%% exchange leaves the request to run the way it does outside CI: a mirror
%% that authenticates another way still works. A refusal is answered with why
%% the exchange failed.
execute_without_workload_identity(BaseConfig, Fun, Error) ->
    case Fun(BaseConfig) of
        {ok, {Status, _Headers, _Body}} when Status =:= 401; Status =:= 403 -> Error;
        {ok, {Status, _Headers}} when Status =:= 401; Status =:= 403 -> Error;
        Other -> Other
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
%% Ask user if they want to authenticate, and if yes, initiate device auth, then
%% run the request with what that produced.
%%
%% Kind says which credential the retried request needs: `api' takes the token
%% as api_key, `repo' resolves repository auth and takes it as repo_key.
%%
%% Acquiring the credential is serialized with a global lock so concurrent
%% callers don't each trigger their own device auth flow. The first caller to
%% acquire the lock runs device auth and persists the resulting token;
%% subsequent callers re-check for an existing (now-valid) token inside the lock
%% and reuse it instead of re-authenticating. The request runs outside the lock.
%% A 401 brings it back here, and global:trans/4 on a lock this process already
%% holds does not take a second reference: the inner transaction's exit deletes
%% the lock entry while the outer one is still running.
maybe_authenticate_and_retry(Kind, BaseConfig, Fun, Reason, Retries, Opts) ->
    Credential = global:trans(
        {{?MODULE, device_auth}, self()},
        fun() ->
            acquire_credential(Kind, BaseConfig, Reason, Opts)
        end,
        [node()],
        infinity
    ),
    case Credential of
        {ok, Key, AuthContext} ->
            execute_authenticated(Kind, BaseConfig, Fun, Key, AuthContext, Retries, Opts);
        {error, _} = Error ->
            Error
    end.

%% @private
%% Another caller may have authenticated while we waited for the lock. Re-resolve
%% and, if we get a token that differs from the one we arrived with (none when
%% credentials were missing; the rejected one on token_refresh_failed), reuse it
%% instead of prompting again. Otherwise proceed to prompt + device auth.
acquire_credential(api, BaseConfig, Reason, Opts) ->
    CurrentApiKey = maps:get(api_key, BaseConfig, undefined),
    case resolve_api_auth(write, BaseConfig) of
        {ok, ApiKey, AuthContext} when ApiKey =/= CurrentApiKey ->
            {ok, ApiKey, AuthContext};
        _ ->
            prompt_and_device_auth(api, BaseConfig, Reason, Opts)
    end;
acquire_credential(repo, BaseConfig, Reason, Opts) ->
    CurrentRepoKey = maps:get(repo_key, BaseConfig, undefined),
    case resolve_repo_auth(BaseConfig) of
        {ok, RepoKey, AuthContext} when is_binary(RepoKey), RepoKey =/= CurrentRepoKey ->
            {ok, RepoKey, AuthContext};
        _ ->
            prompt_and_device_auth(repo, BaseConfig, Reason, Opts)
    end.

%% @private
prompt_and_device_auth(Kind, BaseConfig, Reason, Opts) ->
    case call_callback(BaseConfig, should_authenticate, [Reason]) of
        true ->
            case device_auth(BaseConfig, <<"api repositories">>, Opts) of
                {ok, Tokens} ->
                    authenticated_credential(Kind, BaseConfig, Tokens);
                {error, _} = Error ->
                    Error
            end;
        false ->
            {error, {auth_error, auth_declined}}
    end.

%% @private
%% The credential device auth just produced. The token is the user's API token;
%% what a repository request needs is repository auth, which the token may only
%% be one input to, so resolve that instead of reusing the API-shaped one.
authenticated_credential(api, _BaseConfig, #{access_token := AccessToken} = Tokens) ->
    {ok, <<"Bearer ", AccessToken/binary>>, #{has_refresh_token => has_refresh_token(Tokens)}};
authenticated_credential(repo, BaseConfig, _Tokens) ->
    case resolve_repo_auth(BaseConfig) of
        {ok, RepoKey, AuthContext} when is_binary(RepoKey) ->
            {ok, RepoKey, AuthContext};
        no_auth ->
            {error, {auth_error, no_credentials}};
        {error, _} = Error ->
            Error
    end.

%% @private
execute_authenticated(api, BaseConfig, Fun, ApiKey, AuthContext, Retries, Opts) ->
    execute_with_retry(BaseConfig#{api_key => ApiKey}, Fun, AuthContext, Retries, Opts);
execute_authenticated(repo, BaseConfig, Fun, RepoKey, _AuthContext, _Retries, _Opts) ->
    execute_repo_with_retry(BaseConfig, Fun, RepoKey).

%% @private
%% Execute function without auth, but retry with auth if we get a 401.
execute_optional_with_retry(Kind, BaseConfig, Fun, AuthInline, Opts) ->
    case Fun(BaseConfig) of
        {ok, {401, _Headers, _Body}} when AuthInline =:= true ->
            %% Got 401, need auth - ask user if they want to authenticate
            maybe_authenticate_and_retry(
                Kind, BaseConfig, Fun, no_credentials, initial_retries(), Opts
            );
        {ok, {401, _Headers, _Body}} ->
            %% Got 401 but auth_inline is false, return error
            {error, {auth_error, no_credentials}};
        Other ->
            Other
    end.

%% @private
%% Run a repository request with a resolved token. A 401 that says the token
%% expired is answered by renewing the credential at its source and running the
%% request once more; a second 401 is the caller's to handle.
execute_repo_with_retry(BaseConfig, Fun, RepoKey) ->
    case Fun(BaseConfig#{repo_key => RepoKey}) of
        {ok, {401, Headers, _Body}} = Response ->
            case detect_auth_error(Headers) of
                token_expired ->
                    renew_repo_auth_and_retry(BaseConfig, Fun, RepoKey, Response);
                _Other ->
                    Response
            end;
        Other ->
            Other
    end.

%% @private
renew_repo_auth_and_retry(BaseConfig, Fun, RepoKey, Response) ->
    case resolve_repo_auth(BaseConfig, {rejected, RepoKey}) of
        {ok, NewRepoKey, _AuthContext} when is_binary(NewRepoKey), NewRepoKey =/= RepoKey ->
            Fun(BaseConfig#{repo_key => NewRepoKey});
        {error, {auth_error, {workload_identity_failed, _Reason}}} = Error ->
            Error;
        _Other ->
            Response
    end.

%% @doc
%% Refreshes the stored global OAuth token now, whether or not it has expired.
%%
%% What a token carries can change without it expiring: authenticating a
%% session for an organization's authentication requirements grants scopes the
%% current access token was minted without. This is how a build tool picks
%% those up rather than waiting out the access token.
-spec refresh_tokens(r3_hex_core:config()) -> ok | {error, auth_error()}.
refresh_tokens(Config) ->
    Refresh = fun(Tokens) ->
        %% A failed refresh leaves the stored token in place; the caller warns
        %% and continues with it.
        case maybe_refresh_token_with_context(Config, Tokens) of
            {ok, _BearerToken, _AuthContext} -> ok;
            {error, _Reason} = Error -> Error
        end
    end,
    case with_token_refresh_lock(Config, Refresh) of
        error -> {error, {auth_error, no_credentials}};
        Result -> Result
    end.

%% @private
%% Check if a token is expired (within 5 minute buffer).
-spec is_token_expired(integer()) -> boolean().
is_token_expired(ExpiresAt) ->
    Now = erlang:system_time(second),
    ExpiresAt - Now < ?EXPIRY_BUFFER_SECONDS.

%% @doc
%% Authenticates with a workload identity, exchanging the CI job's OIDC token
%% for API auth scoped to Scope (for example `<<"package:hexpm/my_package">>').
%%
%% Returns `none' when credentials already resolve through
%% `resolve_api_auth/2' (any credential the user configured takes precedence)
%% or when no supported CI provider is detected, in which case the caller
%% falls back to its ordinary auth resolution. Call this once per publish and
%% reuse the resulting key for every request the publish makes; do not call
%% it from inside `with_api/3,4'.
-spec workload_identity_auth(r3_hex_core:config(), Scope :: binary()) ->
    {ok, binary()} | none | {error, workload_identity_error()}.
workload_identity_auth(Config, Scope) ->
    case resolve_api_auth(write, Config) of
        {ok, _ApiKey, _AuthContext} ->
            none;
        _NoUsableCredentials ->
            case r3_hex_oidc:detect_provider() of
                {ok, Provider} ->
                    case authenticate_workload_identity(Config, Provider, Scope) of
                        {ok, #{access_token := AccessToken}} ->
                            {ok, <<"Bearer ", AccessToken/binary>>};
                        {error, _Reason} = Error ->
                            Error
                    end;
                none ->
                    none
            end
    end.

%% @private
-spec authenticate_workload_identity(r3_hex_core:config(), r3_hex_oidc:provider(), binary()) ->
    {ok, oauth_tokens()} | {error, workload_identity_error()}.
authenticate_workload_identity(Config, Provider, Scope) ->
    case r3_hex_api_oauth:oidc_audience(Config) of
        {ok, {200, _Headers, #{<<"audience">> := Audience}}} when is_binary(Audience) ->
            case r3_hex_oidc:fetch_token(Config, Provider, Audience) of
                {ok, OidcToken} ->
                    exchange_workload_identity_token(Config, OidcToken, Scope);
                {error, _Reason} = Error ->
                    Error
            end;
        Response ->
            {error, {oidc_audience_failed, Response}}
    end.

%% @private
exchange_workload_identity_token(Config, OidcToken, Scope) ->
    case r3_hex_api_oauth:jwt_bearer_token(Config, OidcToken, Scope) of
        {ok,
            {200, _Headers, #{<<"access_token">> := AccessToken, <<"expires_in">> := ExpiresIn}}} when
            is_binary(AccessToken), is_integer(ExpiresIn)
        ->
            {ok, #{
                access_token => AccessToken, expires_at => erlang:system_time(second) + ExpiresIn
            }};
        Response ->
            {error, {token_exchange_failed, Response}}
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
        {ok, Response} ->
            %% organization_reauth_required reaches the build tool through the organization_reauth
            %% callback rather than with the tokens. The response carries no key
            %% when the server sent a set that could not be read.
            Tokens = maps:without([organization_reauth_required], Response),
            ok = persist_tokens(Config, global, Tokens),
            report_organization_reauth(Config, maps:find(organization_reauth_required, Response)),
            {ok, Tokens};
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

%%====================================================================
%% Internal functions - Auth Resolution
%%====================================================================

%% @private
-spec resolve_api_auth(permission(), r3_hex_core:config()) ->
    {ok, binary(), auth_context()} | {error, no_auth} | {error, auth_error()}.
resolve_api_auth(_Permission, #{api_key := ApiKey}) when is_binary(ApiKey) ->
    %% api_key already in config, pass through directly
    {ok, ApiKey, #{has_refresh_token => false}};
resolve_api_auth(_Permission, Config) ->
    RepoName = repo_name(Config),
    %% 1. Check per-repo api_key
    case call_callback(Config, get_auth_config, [RepoName]) of
        #{api_key := ApiKey} when is_binary(ApiKey) ->
            {ok, ApiKey, #{has_refresh_token => false}};
        _ ->
            %% 2. Check parent repo (for "hexpm:org" organizations)
            case get_parent_repo_key(Config, RepoName, api_key) of
                {ok, ApiKey} ->
                    {ok, ApiKey, #{has_refresh_token => false}};
                error ->
                    %% 3. Try global OAuth token
                    resolve_oauth_token_with_context(Config, false)
            end
    end.

%% @private
%% Resolve repo auth credentials in this order:
%% 0. repo_key in config => passthrough
%% 1. repo_key from get_auth_config => passthrough
%% 2. trusted + auth_key + oauth_exchange => exchange for OAuth token
%% 3. trusted + auth_key => use directly
%% 4. trusted Hex.pm or child repository + global OAuth tokens => use those
%% 5. trusted Hex.pm child repository + Workload Identity => use or exchange
%% 6. Fallthrough to no_auth (handled by with_repo/3 for optional/auth_inline)
-spec resolve_repo_auth(r3_hex_core:config()) ->
    {ok, binary(), auth_context()} | no_auth | {error, auth_error()}.
resolve_repo_auth(Config) ->
    resolve_repo_auth(Config, false).

%% @private
%% Renew is `{rejected, RepoKey}' when the server rejected the credential we
%% have, so a stored token that has not run out of time is exchanged or
%% refreshed anyway, and `false' otherwise.
resolve_repo_auth(#{repo_key := RepoKey}, _Renew) when is_binary(RepoKey) ->
    %% repo_key already in config, pass through directly
    {ok, RepoKey, #{has_refresh_token => false}};
resolve_repo_auth(Config, Renew) ->
    RepoName = repo_name(Config),
    %% Most requests resolve to a stored credential, or to none, without
    %% exchanging a token. Those are answered without the repo lock so that
    %% concurrent requests don't queue on it; only an exchange takes the lock and
    %% resolves again under it.
    case do_resolve_repo_auth(RepoName, RepoName, Config, Renew, unlocked) of
        needs_lock ->
            global:trans(
                {{?MODULE, repo, RepoName}, self()},
                fun() ->
                    do_resolve_repo_auth(RepoName, RepoName, Config, Renew, locked)
                end,
                [node()],
                infinity
            );
        Result ->
            Result
    end.

do_resolve_repo_auth(RepoName, LookupRepo, Config, Renew, Lock) ->
    Trusted = maps:get(trusted, Config, false),
    OAuthExchange = maps:get(oauth_exchange, Config, false),
    case call_callback(Config, get_auth_config, [LookupRepo]) of
        #{repo_key := RepoKey} when is_binary(RepoKey) ->
            %% 1. repo_key from get_auth_config => passthrough
            {ok, RepoKey, #{has_refresh_token => false}};
        #{oauth_token := OAuthToken, auth_key := AuthKey} when
            is_binary(AuthKey) and OAuthExchange, Trusted
        ->
            %% 2. trusted + oauth_token + auth_key + oauth_exchange => use/refresh existing token
            resolve_repo_oauth_token(RepoName, Config, AuthKey, OAuthToken, Renew, Lock);
        #{auth_key := AuthKey} when is_binary(AuthKey) and OAuthExchange, Trusted ->
            %% 3. trusted + auth_key + oauth_exchange => exchange for new OAuth token
            case Lock of
                unlocked -> needs_lock;
                locked -> exchange_for_oauth_token(RepoName, Config, AuthKey, <<"repositories">>)
            end;
        #{auth_key := AuthKey} when is_binary(AuthKey), Trusted ->
            %% 4. trusted + auth_key => use directly
            {ok, AuthKey, #{has_refresh_token => false}};
        _ when Trusted ->
            %% 5. Check parent repo (for "hexpm:org" organizations)
            case binary:split(LookupRepo, <<":">>) of
                [ParentName, _OrgName] ->
                    do_resolve_repo_auth(RepoName, ParentName, Config, Renew, Lock);
                _ ->
                    %% 6. trusted Hex.pm or child repository + global OAuth tokens => use those
                    case resolve_global_oauth_for_repo(RepoName, Config, Renew) of
                        no_auth ->
                            %% 7. trusted Hex.pm child repository + Workload Identity
                            resolve_workload_identity_for_repo(RepoName, Config, Renew, Lock);
                        Result ->
                            Result
                    end
            end;
        _ ->
            %% 8. Not trusted, no auth
            no_auth
    end.

%% @private
resolve_global_oauth_for_repo(<<"hexpm">>, Config, Renew) ->
    resolve_global_oauth_for_repo(Config, Renew);
resolve_global_oauth_for_repo(<<"hexpm:", _/binary>>, Config, Renew) ->
    resolve_global_oauth_for_repo(Config, Renew);
resolve_global_oauth_for_repo(_RepoName, _Config, _Renew) ->
    no_auth.

resolve_global_oauth_for_repo(Config, Renew) ->
    case resolve_oauth_token_with_context(Config, Renew =/= false) of
        {ok, Token, AuthContext} ->
            {ok, Token, AuthContext};
        {error, no_auth} ->
            no_auth;
        {error, _} = Error ->
            Error
    end.

%% @private
%% A Hex.pm organization's repository fetched from a CI job. The build tool
%% keeps the outcome, so a token is reused until it is about to expire, and
%% only an exchange takes the repo lock, like resolve_repo_oauth_token/6. A
%% renewal uses a token another caller exchanged while this one waited for the
%% lock instead of exchanging again. A failure is kept too: each failed
%% exchange counts against the CI job's limit at Hex, and the next one would
%% fail the same way.
resolve_workload_identity_for_repo(
    <<"hexpm:", Organization/binary>> = RepoName, Config, Renew, Lock
) ->
    case workload_identity_provider(Config) of
        {ok, Provider} ->
            case call_callback(Config, get_workload_identity_token, [RepoName]) of
                {ok, #{access_token := AccessToken, expires_at := ExpiresAt}} ->
                    BearerToken = <<"Bearer ", AccessToken/binary>>,
                    case Renew =:= {rejected, BearerToken} orelse is_token_expired(ExpiresAt) of
                        false ->
                            {ok, BearerToken, #{has_refresh_token => false}};
                        true ->
                            exchange_workload_identity_for_repo(
                                RepoName, Organization, Config, Provider, Lock
                            )
                    end;
                {error, Reason} ->
                    {error, {auth_error, {workload_identity_failed, Reason}}};
                error ->
                    exchange_workload_identity_for_repo(
                        RepoName, Organization, Config, Provider, Lock
                    )
            end;
        none ->
            no_auth
    end;
resolve_workload_identity_for_repo(_RepoName, _Config, _Renew, _Lock) ->
    no_auth.

%% @private
%% Without somewhere to keep the token every request would exchange a new OIDC
%% token, so a build tool that provides no Workload Identity callbacks doesn't
%% get Workload Identity for repositories.
workload_identity_provider(#{cli_auth_callbacks := Callbacks}) ->
    case
        is_map_key(get_workload_identity_token, Callbacks) andalso
            is_map_key(persist_workload_identity_token, Callbacks)
    of
        true -> r3_hex_oidc:detect_provider();
        false -> none
    end.

%% @private
exchange_workload_identity_for_repo(_RepoName, _Organization, _Config, _Provider, unlocked) ->
    needs_lock;
exchange_workload_identity_for_repo(RepoName, Organization, Config, Provider, locked) ->
    Scope = <<"repository:", Organization/binary>>,
    Result = authenticate_workload_identity(exchange_config(Config), Provider, Scope),
    ok = call_callback(Config, persist_workload_identity_token, [RepoName, Result]),
    case Result of
        {ok, #{access_token := AccessToken}} ->
            {ok, <<"Bearer ", AccessToken/binary>>, #{has_refresh_token => false}};
        {error, Reason} ->
            {error, {auth_error, {workload_identity_failed, Reason}}}
    end.

%% @private
%% Resolve repo OAuth token: use if valid, re-exchange if expiring or rejected.
resolve_repo_oauth_token(
    RepoName,
    Config,
    AuthKey,
    #{access_token := AccessToken, expires_at := ExpiresAt},
    Renew,
    Lock
) ->
    case {Renew =/= false orelse is_token_expired(ExpiresAt), Lock} of
        {false, _} ->
            %% Token is still valid, use it
            BearerToken = <<"Bearer ", AccessToken/binary>>,
            {ok, BearerToken, #{has_refresh_token => false}};
        {true, unlocked} ->
            needs_lock;
        {true, locked} ->
            %% Token expired, do a new exchange
            exchange_for_oauth_token(RepoName, Config, AuthKey, <<"repositories">>)
    end.

%% @private
%% Exchange api_key/auth_key for OAuth token via client credentials grant.
%% Persists the token with the repo name for per-repo token storage.
exchange_for_oauth_token(RepoName, Config, AuthKey, Scope) ->
    ClientId = call_callback(Config, get_client_id, []),
    ExchangeConfig = exchange_config(Config),
    case r3_hex_api_oauth:client_credentials_token(ExchangeConfig, ClientId, AuthKey, Scope) of
        {ok, {200, _, #{<<"access_token">> := AccessToken, <<"expires_in">> := ExpiresIn}}} ->
            Tokens = #{
                access_token => AccessToken,
                expires_at => erlang:system_time(second) + ExpiresIn
            },
            ok = persist_tokens(Config, RepoName, Tokens),
            BearerToken = <<"Bearer ", AccessToken/binary>>,
            {ok, BearerToken, #{has_refresh_token => false}};
        {ok, {_Status, _, _Body}} ->
            {error, {auth_error, oauth_exchange_failed}};
        {error, _} ->
            {error, {auth_error, oauth_exchange_failed}}
    end.

%% @private
%% Repository tokens are exchanged at `oauth_exchange_url' when it is set.
exchange_config(Config) ->
    case maps:get(oauth_exchange_url, Config, undefined) of
        undefined -> Config;
        OAuthUrl -> Config#{api_url => OAuthUrl}
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
%% Renew refreshes a token that has not run out of time, for when the server
%% has rejected it anyway.
%%
%% A stored token that is still valid, or no stored token, is answered without
%% the lock. Only a refresh takes it, and the tokens are read again under the
%% lock in case another caller refreshed them while this one waited.
resolve_oauth_token_with_context(Config, false) ->
    case call_callback(Config, get_oauth_tokens, []) of
        {ok, #{expires_at := ExpiresAt} = Tokens} ->
            case is_token_expired(ExpiresAt) of
                false -> bearer_token(Tokens);
                true -> resolve_oauth_token_with_lock(Config, false)
            end;
        error ->
            {error, no_auth}
    end;
resolve_oauth_token_with_context(Config, true) ->
    resolve_oauth_token_with_lock(Config, true).

resolve_oauth_token_with_lock(Config, Renew) ->
    Resolve = fun(#{expires_at := ExpiresAt} = Tokens) ->
        case Renew orelse is_token_expired(ExpiresAt) of
            true -> refresh_or_clear(Config, Tokens);
            false -> bearer_token(Tokens)
        end
    end,
    case with_token_refresh_lock(Config, Resolve) of
        error -> {error, no_auth};
        Result -> Result
    end.

bearer_token(#{access_token := AccessToken} = Tokens) ->
    BearerToken = <<"Bearer ", AccessToken/binary>>,
    {ok, BearerToken, #{has_refresh_token => has_refresh_token(Tokens)}}.

%% @private
%% Fetch the stored global tokens and hand them to Fun under the token-refresh
%% lock, so concurrent callers do not each refresh the same token. Returns
%% `error' without calling Fun when no tokens are stored.
with_token_refresh_lock(Config, Fun) ->
    global:trans(
        {{?MODULE, token_refresh}, self()},
        fun() ->
            case call_callback(Config, get_oauth_tokens, []) of
                {ok, Tokens} -> Fun(Tokens);
                error -> error
            end
        end,
        [node()],
        infinity
    ).

%% @private
%% Refresh an expired global token; if the server rejected the refresh token,
%% invalidate the stored token via the optional clear_oauth_tokens callback.
%% This runs inside the token_refresh lock, so the unusable token is dropped
%% exactly once and the callers serialized behind the lock re-read it as absent
%% instead of each retrying the doomed refresh against the server. A refresh
%% that got no usable answer says nothing about the token, so it is kept.
refresh_or_clear(Config, Tokens) ->
    case maybe_refresh_token_with_context(Config, Tokens) of
        {ok, _Bearer, _Ctx} = Ok ->
            Ok;
        {error, {auth_error, token_refresh_failed}} = Error ->
            maybe_call_callback(Config, clear_oauth_tokens, []),
            Error;
        {error, _} = Error ->
            Error
    end.

%% @private
%% Only 400 and 401 are the server refusing the refresh token, which is what
%% makes the stored credential dead. A 429, a 5xx and a 200 whose body cannot be
%% read all leave the refresh token as good as it was.
maybe_refresh_token_with_context(Config, #{refresh_token := RefreshToken}) when
    is_binary(RefreshToken)
->
    ClientId = call_callback(Config, get_client_id, []),
    case r3_hex_api_oauth:refresh_token(Config, ClientId, RefreshToken) of
        {ok,
            {200, _,
                #{
                    <<"access_token">> := NewAccessToken,
                    <<"expires_in">> := ExpiresIn
                } = TokenResponse}} when
            is_binary(NewAccessToken), is_integer(ExpiresIn)
        ->
            NewTokens = #{
                access_token => NewAccessToken,
                refresh_token => new_refresh_token(TokenResponse, RefreshToken),
                expires_at => erlang:system_time(second) + ExpiresIn
            },
            ok = persist_tokens(Config, global, NewTokens),
            report_organization_reauth(
                Config, r3_hex_api_oauth:organization_reauth_required(TokenResponse)
            ),
            BearerToken = <<"Bearer ", NewAccessToken/binary>>,
            {ok, BearerToken, #{has_refresh_token => has_refresh_token(NewTokens)}};
        {ok, {Status, _, _Body}} when Status =:= 400; Status =:= 401 ->
            {error, {auth_error, token_refresh_failed}};
        {ok, {_Status, _, _Body}} ->
            {error, {auth_error, token_refresh_unavailable}};
        {error, _Reason} ->
            {error, {auth_error, token_refresh_unavailable}}
    end;
maybe_refresh_token_with_context(_Config, _Tokens) ->
    {error, {auth_error, token_refresh_failed}}.

%% @private
%% A refresh that does not rotate the refresh token keeps the one we sent.
new_refresh_token(#{<<"refresh_token">> := RefreshToken}, _CurrentRefreshToken) when
    is_binary(RefreshToken)
->
    RefreshToken;
new_refresh_token(_TokenResponse, CurrentRefreshToken) ->
    CurrentRefreshToken.

%% @private
%% Whether these tokens can be refreshed.
-spec has_refresh_token(oauth_tokens()) -> boolean().
has_refresh_token(Tokens) ->
    is_binary(maps:get(refresh_token, Tokens, undefined)).

%% @private
%% The one place tokens are handed to the build tool for storage. A token map
%% without a refresh token is persisted as `undefined', which is what the
%% persist_oauth_tokens callback documents for "there is none".
-spec persist_tokens(r3_hex_core:config(), global | binary(), oauth_tokens()) -> ok.
persist_tokens(Config, Scope, #{access_token := AccessToken, expires_at := ExpiresAt} = Tokens) ->
    RefreshToken = maps:get(refresh_token, Tokens, undefined),
    ok = call_callback(Config, persist_oauth_tokens, [
        Scope, AccessToken, RefreshToken, ExpiresAt
    ]).

%%====================================================================
%% Internal functions - Retry Logic
%%====================================================================

%% @private
-spec initial_retries() -> retries().
initial_retries() ->
    #{otp => 0, otp_error => undefined, token => 0}.

%% @private
execute_with_retry(Config, Fun, AuthContext, Retries, Opts) ->
    case Fun(Config) of
        {error, otp_required} ->
            handle_otp_retry(
                Config, Fun, AuthContext, Retries, <<"Enter OTP code:">>, Opts
            );
        {error, invalid_totp} ->
            handle_otp_retry(
                Config,
                Fun,
                AuthContext,
                Retries,
                <<"Invalid OTP code. Please try again:">>,
                Opts
            );
        {ok, {401, Headers, _Body}} = Response ->
            case detect_auth_error(Headers) of
                otp_required ->
                    handle_otp_retry(
                        Config, Fun, AuthContext, Retries, <<"Enter OTP code:">>, Opts
                    );
                invalid_totp ->
                    Msg =
                        case maps:get(otp_error, Retries) of
                            invalid_totp -> <<"Invalid OTP code. Please try again:">>;
                            _ -> <<"Enter OTP code:">>
                        end,
                    handle_otp_retry(Config, Fun, AuthContext, Retries, Msg, Opts);
                token_expired ->
                    handle_token_refresh_retry(Config, Fun, AuthContext, Retries, Response, Opts);
                none ->
                    Response
            end;
        Other ->
            Other
    end.

%% @private
handle_otp_retry(_Config, _Fun, _AuthContext, #{otp := OtpRetries}, _Message, _Opts) when
    OtpRetries >= ?MAX_OTP_RETRIES
->
    {error, {auth_error, otp_max_retries}};
handle_otp_retry(Config, Fun, AuthContext, #{otp := OtpRetries} = Retries, Message, Opts) ->
    case call_callback(Config, prompt_otp, [Message]) of
        {ok, OtpCode} ->
            NewConfig = Config#{api_otp => OtpCode},
            NewRetries = Retries#{otp := OtpRetries + 1, otp_error := invalid_totp},
            execute_with_retry(NewConfig, Fun, AuthContext, NewRetries, Opts);
        cancelled ->
            {error, {auth_error, otp_cancelled}}
    end.

%% @private
%% A 401 that says the token expired is answered by renewing the credential at
%% its source and running the request once more, the way a repository request
%% is. The renewal is counted, so a server that answers token_expired to every
%% bearer we send it gets a bounded number of requests rather than a loop.
handle_token_refresh_retry(
    _Config, _Fun, _AuthContext, #{token := TokenRetries}, Response, _Opts
) when
    TokenRetries >= ?MAX_TOKEN_RETRIES
->
    Response;
handle_token_refresh_retry(
    Config, Fun, AuthContext, #{token := TokenRetries} = Retries, _Response, Opts
) ->
    NewRetries = Retries#{token := TokenRetries + 1},
    %% Only attempt refresh if we have a refresh token
    case maps:get(has_refresh_token, AuthContext, false) of
        true ->
            case resolve_oauth_token_with_context(Config, true) of
                {ok, NewBearerToken, NewAuthContext} ->
                    NewConfig = Config#{api_key => NewBearerToken},
                    execute_with_retry(NewConfig, Fun, NewAuthContext, NewRetries, Opts);
                {error, _} ->
                    maybe_reauthenticate(Config, Fun, NewRetries, Opts)
            end;
        false ->
            maybe_reauthenticate(Config, Fun, NewRetries, Opts)
    end.

%% @private
%% After token refresh failure, prompt the user to re-authenticate via device auth
%% (only when auth_inline is true). Mirrors Hex.OAuth.reauthenticate/1.
maybe_reauthenticate(Config, Fun, Retries, Opts) ->
    AuthInline = proplists:get_value(auth_inline, Opts, true),
    case AuthInline of
        true ->
            maybe_authenticate_and_retry(api, Config, Fun, token_refresh_failed, Retries, Opts);
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

%% @private
%% Hands the build tool the organizations this session has to authenticate for.
%% A grant that flagged none carries the empty list, so a set that has been
%% resolved does not linger. A grant whose set could not be read is not
%% reported: the empty list would be taken for the server saying there is
%% nothing, and the build tool would drop the organizations it holds.
report_organization_reauth(Config, {ok, Organizations}) when is_list(Organizations) ->
    maybe_call_callback(Config, organization_reauth, [Organizations]);
report_organization_reauth(_Config, error) ->
    ok.

%% @private
%% Like call_callback/3 but for optional callbacks: returns ok without doing
%% anything when the callback is not provided.
maybe_call_callback(Config, Name, Args) ->
    #{cli_auth_callbacks := Callbacks} = Config,
    case maps:find(Name, Callbacks) of
        {ok, Fun} -> erlang:apply(Fun, Args);
        error -> ok
    end.
