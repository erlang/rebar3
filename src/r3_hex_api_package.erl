%% Vendored from hex_core v0.5.0, do not edit manually

-module(r3_hex_api_package).
-export([get/2, search/3]).

%% @doc
%% Gets package.
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
get(Config, Name) when is_binary(Name) and is_map(Config) ->
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
search(Config, Query, SearchParams) when is_binary(Query) and is_list(SearchParams) and is_map(Config) ->
    QueryString = r3_hex_api:encode_query_string([{search, Query} | SearchParams]),
    Path = r3_hex_api:join_path_segments(r3_hex_api:build_repository_path(Config, ["packages"])),
    PathQuery = <<Path/binary, "?", QueryString/binary>>,
    r3_hex_api:get(Config, PathQuery).
