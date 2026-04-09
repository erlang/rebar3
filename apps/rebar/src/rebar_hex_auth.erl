%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% @doc
%% Authentication wrapper for Hex package manager.
%%
%% This module provides rebar3-specific callbacks for r3_hex_cli_auth.
%% See r3_hex_cli_auth for auth resolution order and details.
%% @end
-module(rebar_hex_auth).

-export([with_api/5, with_repo/4]).

%% OAuth utilities
-export([client_id/0, global_oauth_key/0, persist_tokens/4]).


-include("rebar.hrl").
-include_lib("providers/include/providers.hrl").

-define(GLOBAL_OAUTH_KEY, <<"$oauth">>).
-define(OAUTH_CLIENT_ID, <<"b9721cf5-be2f-4a65-bfa5-141698b4b9cf">>).

%% @doc Execute an API call with authentication handling.
%% @see r3_hex_cli_auth:with_api/5
-spec with_api(Permission, Config, State, Opts, Callback) -> Result when
      Permission :: r3_hex_cli_auth:permission(),
      Config :: rebar_hex_repos:repo(),
      State :: rebar_state:t(),
      Opts :: r3_hex_cli_auth:opts(),
      Callback :: fun((rebar_hex_repos:repo()) -> Result),
      Result :: term().
with_api(Permission, Config, State, Opts, Callback) when Permission =:= read; Permission =:= write ->
    Callbacks = make_callbacks(State),
    HexConfig = to_hex_config(Config),
    r3_hex_cli_auth:with_api(Callbacks, Permission, HexConfig, Callback, Opts).

%% @doc Execute a repository call with authentication handling.
%% @see r3_hex_cli_auth:with_repo/4
-spec with_repo(Config, State, Opts, Callback) -> Result when
      Config :: rebar_hex_repos:repo(),
      State :: rebar_state:t(),
      Opts :: r3_hex_cli_auth:opts(),
      Callback :: fun((rebar_hex_repos:repo()) -> Result),
      Result :: term().
with_repo(Config, State, Opts, Callback) ->
    Callbacks = make_callbacks(State),
    HexConfig = to_hex_config(Config),
    r3_hex_cli_auth:with_repo(Callbacks, HexConfig, Callback, Opts).

%% @private
%% Strip rebar3-specific fields from repo config before passing to hex_core.
-spec to_hex_config(rebar_hex_repos:repo()) -> r3_hex_core:config().
to_hex_config(Config) ->
    maps:without([name, parent, mirror_of], Config).

%%====================================================================
%% OAuth utilities
%%====================================================================

%% @doc Returns the OAuth client ID for Hex.
-spec client_id() -> binary().
client_id() -> ?OAUTH_CLIENT_ID.

%% @doc Returns the key used for global OAuth storage in hex.config.
-spec global_oauth_key() -> binary().
global_oauth_key() -> ?GLOBAL_OAUTH_KEY.

%% @doc Persist the global OAuth tokens to hex.config.
-spec persist_tokens(AccessToken, RefreshToken, ExpiresAt, State) -> ok when
      AccessToken :: binary(),
      RefreshToken :: binary() | undefined,
      ExpiresAt :: integer(),
      State :: rebar_state:t().
persist_tokens(AccessToken, RefreshToken, ExpiresAt, State) ->
    Updates = #{
        ?GLOBAL_OAUTH_KEY => #{
            access_token => AccessToken,
            refresh_token => RefreshToken,
            expires_at => ExpiresAt
        }
    },
    rebar_hex_repos:update_auth_config(Updates, State),
    ?DEBUG("Updated global OAuth tokens", []),
    ok.

%%====================================================================
%% Callbacks builder
%%====================================================================

%% @private
%% Build the callbacks map required by r3_hex_cli_auth.
-spec make_callbacks(rebar_state:t()) -> r3_hex_cli_auth:callbacks().
make_callbacks(State) ->
    #{
        get_auth_config => fun(RepoName) ->
            get_repo_auth_config(RepoName, State)
        end,

        get_oauth_tokens => fun() ->
            get_global_oauth_tokens(State)
        end,

        persist_oauth_tokens => fun(Scope, AccessToken, RefreshToken, ExpiresAt) ->
            persist_oauth_tokens(Scope, AccessToken, RefreshToken, ExpiresAt, State)
        end,

        prompt_otp => fun(Message) ->
            prompt_otp(Message)
        end,

        should_authenticate => fun(Reason) ->
            should_authenticate(Reason)
        end,

        get_client_id => fun() ->
            client_id()
        end
    }.

%%====================================================================
%% Helper functions for callbacks
%%====================================================================

%% @private
%% Get auth config for a specific repo from hex.config.
-spec get_repo_auth_config(RepoName, State) -> r3_hex_cli_auth:repo_auth_config() | undefined when
      RepoName :: unicode:unicode_binary(),
      State :: rebar_state:t().
get_repo_auth_config(RepoName, State) ->
    rebar_hex_repos:get_repo_auth_config(RepoName, State).

%% @private
%% Get global OAuth tokens from auth config.
-spec get_global_oauth_tokens(rebar_state:t()) -> {ok, map()} | error.
get_global_oauth_tokens(State) ->
    case rebar_hex_repos:get_repo_auth_config(?GLOBAL_OAUTH_KEY, State) of
        #{access_token := _, expires_at := _} = Tokens ->
            {ok, Tokens};
        _ ->
            error
    end.

%% @private
%% Persist OAuth tokens. Scope can be 'global' or a repo name binary.
-spec persist_oauth_tokens(Scope, AccessToken, RefreshToken, ExpiresAt, State) -> ok when
      Scope :: global | unicode:unicode_binary(),
      AccessToken :: binary(),
      RefreshToken :: binary() | undefined,
      ExpiresAt :: integer(),
      State :: rebar_state:t().
persist_oauth_tokens(global, AccessToken, RefreshToken, ExpiresAt, State) ->
    OAuthTokens = #{
        access_token => AccessToken,
        refresh_token => RefreshToken,
        expires_at => ExpiresAt
    },
    rebar_hex_repos:update_repo_auth_config(OAuthTokens, ?GLOBAL_OAUTH_KEY, State),
    ?DEBUG("Updated global OAuth tokens", []),
    ok;
persist_oauth_tokens(RepoName, AccessToken, RefreshToken, ExpiresAt, State) ->
    OAuthTokens = #{
        oauth_token => #{
            access_token => AccessToken,
            refresh_token => RefreshToken,
            expires_at => ExpiresAt
        }
    },
    rebar_hex_repos:update_repo_auth_config(OAuthTokens, RepoName, State),
    ?DEBUG("Updated OAuth tokens for ~ts", [RepoName]),
    ok.

%% @private
%% Prompt user for OTP code.
-spec prompt_otp(binary()) -> {ok, binary()} | cancelled.
prompt_otp(Message) ->
    ?CONSOLE("~ts", [Message]),
    case io:get_line("OTP code: ") of
        eof -> cancelled;
        {error, _} -> cancelled;
        Line ->
            case string:trim(Line) of
                "" -> cancelled;
                Code -> {ok, list_to_binary(Code)}
            end
    end.

%% @private
%% Ask user if they want to authenticate.
-spec should_authenticate(r3_hex_cli_auth:auth_prompt_reason()) -> boolean().
should_authenticate(no_credentials) ->
    ?CONSOLE("No Hex credentials found. Would you like to authenticate?", []),
    prompt_yes_no();
should_authenticate(token_refresh_failed) ->
    ?CONSOLE("Hex token refresh failed. Would you like to re-authenticate?", []),
    prompt_yes_no().

%% @private
-spec prompt_yes_no() -> boolean().
prompt_yes_no() ->
    case io:get_line("[Y/n]: ") of
        eof -> false;
        {error, _} -> false;
        Line ->
            case string:lowercase(string:trim(Line)) of
                "" -> true;  % Default to yes
                "y" -> true;
                "yes" -> true;
                _ -> false
            end
    end.

