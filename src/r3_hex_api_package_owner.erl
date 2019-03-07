%% Vendored from hex_core v0.5.0, do not edit manually

-module(r3_hex_api_package_owner).
-export([
    add/3,
    delete/3,
    get/3,
    list/2
]).

%% Examples:
%%
%% ```
%% > r3_hex_api_owner:list(r3_hex_core:default_config(), <<"package">>).
%% {ok, {200, ..., [
%%     #{<<"username">> => <<"alice">>, ...},
%%     ...
%% ]}}
%% '''
list(Config, PackageName) when is_binary(PackageName) and is_map(Config) ->
    Path = r3_hex_api:build_repository_path(Config, ["packages", PackageName, "owners"]),
    r3_hex_api:get(Config, Path).

get(Config, PackageName, UsernameOrEmail) when is_binary(PackageName) and is_map(Config) ->
    Path = r3_hex_api:build_repository_path(Config, ["packages", PackageName, "owners", UsernameOrEmail]),
    r3_hex_api:get(Config, Path).

add(Config, PackageName, UsernameOrEmail) when is_binary(PackageName) and is_map(Config) ->
    Path = r3_hex_api:build_repository_path(Config, ["packages", PackageName, "owners", UsernameOrEmail]),
    r3_hex_api:put(Config, Path, #{}).

delete(Config, PackageName, UsernameOrEmail) when is_binary(PackageName) and is_map(Config) ->
    Path = r3_hex_api:build_repository_path(Config, ["packages", PackageName, "owners", UsernameOrEmail]),
    r3_hex_api:delete(Config, Path).
