%% Vendored from hex_core v0.5.0, do not edit manually

-module(r3_hex_api_key).
-export([
    list/1,
    get/2,
    add/3,
    delete/2,
    delete_all/1
]).

list(Config) when is_map(Config) ->
    Path = r3_hex_api:build_organization_path(Config, ["keys"]),
    r3_hex_api:get(Config, Path).

get(Config, Name) when is_map(Config) ->
    Path = r3_hex_api:build_organization_path(Config, ["keys", Name]),
    r3_hex_api:get(Config, Path).

add(Config, Name, Permissions) when is_map(Config) ->
    Path = r3_hex_api:build_organization_path(Config, ["keys"]),
    Params = #{<<"name">> => Name, <<"permissions">> => Permissions},
    r3_hex_api:post(Config, Path, Params).

delete(Config, Name) when is_map(Config) ->
    Path = r3_hex_api:build_organization_path(Config, ["keys", Name]),
    r3_hex_api:delete(Config, Path).

delete_all(Config) when is_map(Config) ->
    Path = r3_hex_api:build_organization_path(Config, ["keys"]),
    r3_hex_api:delete(Config, Path).
