%% Vendored from hex_core v0.5.0, do not edit manually

-module(r3_hex_api_user).
-export([
    create/4,
    get/2,
    me/1,
    reset_password/2
]).

me(Config) when is_map(Config) ->
    r3_hex_api:get(Config, ["users", "me"]).

create(Config, Username, Password, Email) ->
    Params = #{
      <<"username">> => Username,
      <<"password">> => Password,
      <<"email">> => Email
    },
    r3_hex_api:post(Config, ["users"], Params).

reset_password(Username, Config) when is_binary(Username) and is_map(Config) ->
    r3_hex_api:post(Config, ["users", Username, "reset"], #{}).

%% @doc
%% Gets user.
%%
%% Examples:
%%
%% ```
%% > r3_hex_api_user:get(<<"user">>, r3_hex_core:default_config()).
%% {ok, {200, ..., #{
%%      <<"username">> => <<"user">>,
%%      <<"packages">> => [
%%          #{
%%              <<"name">> => ...,
%%              <<"url">> => ...,
%%              ...
%%          },
%%          ...
%%      ],
%%      ...}}}
%% '''
%% @end
get(Config, Username) when is_binary(Username) and is_map(Config) ->
    r3_hex_api:get(Config, ["users", Username]).
