%% Vendored from hex_core v0.7.1, do not edit manually

-module(r3_hex_api_package).
-export([get/2, search/3]).

%% @doc
%% Gets a package.
%%
%% Examples:
%%
%% ```
%% > r3_hex_api_package:get(r3_hex_core:default_config(), <<"package">>).
%% {ok, {200, ..., #{
%%     <<"name">> => <<"package1">>,
%%     <<"meta">> => #{
%%         <<"description">> => ...,
%%         <<"licenses">> => ...,
%%         <<"links">> => ...,
%%         <<"maintainers">> => ...
%%     },
%%     ...,
%%     <<"releases">> => [
%%         #{<<"url">> => ..., <<"version">> => <<"0.5.0">>}],
%%         #{<<"url">> => ..., <<"version">> => <<"1.0.0">>}],
%%         ...
%%     ]}}}
%% '''
%% @end
-spec get(r3_hex_core:config(), binary()) -> r3_hex_api:response().
get(Config, Name) when is_map(Config) and is_binary(Name)->
    Path = r3_hex_api:build_repository_path(Config, ["packages", Name]),
    r3_hex_api:get(Config, Path).

%% @doc
%% Searches packages.
%%
%% Examples:
%%
%% ```
%% > r3_hex_api_package:search(r3_hex_core:default_config(), <<"package">>, []).
%% {ok, {200, ..., [
%%     #{<<"name">> => <<"package1">>, ...},
%%     ...
%% ]}}
%% '''
-spec search(r3_hex_core:config(), binary(), list(binary())) -> r3_hex_api:response().
search(Config, Query, SearchParams) when is_map(Config) and is_binary(Query) and is_list(SearchParams) ->
    QueryString = r3_hex_api:encode_query_string([{search, Query} | SearchParams]),
    Path = r3_hex_api:join_path_segments(r3_hex_api:build_repository_path(Config, ["packages"])),
    PathQuery = <<Path/binary, "?", QueryString/binary>>,
    r3_hex_api:get(Config, PathQuery).
