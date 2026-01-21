%% Vendored from hex_core v0.12.0, do not edit manually

%% @doc
%% Hex HTTP API - Releases.
-module(r3_hex_api_release).
-export([
    delete/3,
    get/3,
    publish/2,
    publish/3,
    retire/4,
    unretire/3
]).

-export_type([publish_params/0, retirement_params/0]).

-type publish_params() :: [{replace, boolean()}].

-type retirement_params() :: #{binary() := binary()}.
%% @doc
%% Gets a package release.
%%
%% Examples:
%%
%% ```
%% > r3_hex_api_release:get(r3_hex_core:default_config(), <<"package">>, <<"1.0.0">>).
%% {ok, {200, ..., #{
%%      <<"checksum">> => <<"540d210d81f56f17f64309a4896430e727972499b37bd59342dc08d61dff74d8">>,
%%      <<"docs_html_url">> => <<"https://hexdocs.pm/package/1.0.0/">>,
%%      <<"downloads">> => 740,<<"has_docs">> => true,
%%      <<"html_url">> => <<"https://hex.pm/packages/package/1.0.0">>,
%%      <<"inserted_at">> => <<"2014-12-09T18:32:03Z">>,
%%      <<"meta">> =>
%%          #{<<"app">> => <<"package">>,
%%            <<"build_tools">> => [<<"mix">>]},
%%      <<"package_url">> => <<"https://hex.pm/api/packages/package">>,
%%      <<"publisher">> => nil,<<"requirements">> => #{},
%%      <<"retirement">> => nil,
%%      <<"updated_at">> => <<"2019-07-28T21:12:11Z">>,
%%      <<"url">> => <<"https://hex.pm/api/packages/package/releases/1.0.0">>,
%%      <<"version">> => <<"1.0.0">>
%%      }}}
%% '''
%% @end
-spec get(r3_hex_core:config(), binary(), binary()) -> r3_hex_api:response().
get(Config, Name, Version) when is_map(Config) and is_binary(Name) and is_binary(Version) ->
    Path = r3_hex_api:build_repository_path(Config, ["packages", Name, "releases", Version]),
    r3_hex_api:get(Config, Path).

%% @doc
%% Publishes a new package release.
%%
%% Examples:
%%
%% ```
%% > r3_hex_api_release:publish(r3_hex_core:default_config(), Tarball).
%% {ok, {200, ..., #{
%%      <<"checksum">> => <<"540d210d81f56f17f64309a4896430e727972499b37bd59342dc08d61dff74d8">>,
%%      <<"docs_html_url">> => <<"https://hexdocs.pm/package/1.0.0/">>,
%%      <<"downloads">> => 740,<<"has_docs">> => true,
%%      <<"html_url">> => <<"https://hex.pm/packages/package/1.0.0">>,
%%      <<"inserted_at">> => <<"2014-12-09T18:32:03Z">>,
%%      <<"meta">> =>
%%          #{<<"app">> => <<"package">>,
%%            <<"build_tools">> => [<<"mix">>]},
%%      <<"package_url">> => <<"https://hex.pm/api/packages/package">>,
%%      <<"publisher">> => nil,<<"requirements">> => #{},
%%      <<"retirement">> => nil,
%%      <<"updated_at">> => <<"2019-07-28T21:12:11Z">>,
%%      <<"url">> => <<"https://hex.pm/api/packages/package/releases/1.0.0">>,
%%      <<"version">> => <<"1.0.0">>
%%      }}}
%% '''
%% @end
-spec publish(r3_hex_core:config(), binary()) -> r3_hex_api:response().
publish(Config, Tarball) -> publish(Config, Tarball, []).

%% @doc
%% Publishes a new package release with query parameters.
%%
%% Supported query params :
%%  - replace : boolean
%%
%% Examples:
%%
%% ```
%% > r3_hex_api_release:publish(r3_hex_core:default_config(), Tarball, [{replace, true}]).
%% {ok, {201, ..., #{
%%      <<"checksum">> => <<"540d210d81f56f17f64309a4896430e727972499b37bd59342dc08d61dff74d8">>,
%%      <<"docs_html_url">> => <<"https://hexdocs.pm/package/1.0.0/">>,
%%      <<"downloads">> => 740,<<"has_docs">> => true,
%%      <<"html_url">> => <<"https://hex.pm/packages/package/1.0.0">>,
%%      <<"inserted_at">> => <<"2014-12-09T18:32:03Z">>,
%%      <<"meta">> =>
%%          #{<<"app">> => <<"package">>,
%%            <<"build_tools">> => [<<"mix">>]},
%%      <<"package_url">> => <<"https://hex.pm/api/packages/package">>,
%%      <<"publisher">> => nil,<<"requirements">> => #{},
%%      <<"retirement">> => nil,
%%      <<"updated_at">> => <<"2019-07-28T21:12:11Z">>,
%%      <<"url">> => <<"https://hex.pm/api/packages/package/releases/1.0.0">>,
%%      <<"version">> => <<"1.0.0">>
%%      }}}
%% '''
%% @end
-spec publish(r3_hex_core:config(), binary(), publish_params()) -> r3_hex_api:response().
publish(Config, Tarball, Params) when
    is_map(Config) andalso is_binary(Tarball) andalso is_list(Params)
->
    case r3_hex_tarball:unpack(Tarball, memory) of
        {ok, #{metadata := Metadata}} ->
            PackageName = maps:get(<<"name">>, Metadata),
            QueryString = r3_hex_api:encode_query_string([
                {replace, proplists:get_value(replace, Params, false)}
            ]),
            Path = r3_hex_api:join_path_segments(
                r3_hex_api:build_repository_path(Config, ["packages", PackageName, "releases"])
            ),
            PathWithQuery = <<Path/binary, "?", QueryString/binary>>,
            TarballContentType = "application/octet-stream",
            Config2 = put_header(
                <<"content-length">>, integer_to_binary(byte_size(Tarball)), Config
            ),
            Config3 = maybe_put_expect_header(Config2),
            Body = {TarballContentType, Tarball},
            r3_hex_api:post(Config3, PathWithQuery, Body);
        {error, Reason} ->
            {error, {tarball, Reason}}
    end.

%% @doc
%% Deletes a package release.
%%
%% Examples:
%%
%% ```
%% > r3_hex_api_release:delete(r3_hex_core:default_config(), <<"package">>, <<"1.0.0">>).
%% {ok, {204, ..., nil}}
%% '''
%% @end
-spec delete(r3_hex_core:config(), binary(), binary()) -> r3_hex_api:response().
delete(Config, Name, Version) when is_map(Config) and is_binary(Name) and is_binary(Version) ->
    Path = r3_hex_api:build_repository_path(Config, ["packages", Name, "releases", Version]),
    r3_hex_api:delete(Config, Path).

%% @doc
%% Retires a package release.
%%
%% Examples:
%%
%% ```
%% > r3_hex_api_release:retire(r3_hex_core:default_config(), <<"package">>, <<"1.0.0">>, Params).
%% {ok, {204, ..., nil}}
%% '''
%% @end
-spec retire(r3_hex_core:config(), binary(), binary(), retirement_params()) -> r3_hex_api:response().
retire(Config, Name, Version, Params) when
    is_map(Config) and is_binary(Name) and is_binary(Version)
->
    Path = r3_hex_api:build_repository_path(Config, ["packages", Name, "releases", Version, "retire"]),
    r3_hex_api:post(Config, Path, Params).

%% @doc
%% Unretires a package release.
%%
%% Examples:
%%
%% ```
%% > r3_hex_api_release:unretire(r3_hex_core:default_config(), <<"package">>, <<"1.0.0">>).
%% {ok, {204, ..., nil}}
%% '''
%% @end
-spec unretire(r3_hex_core:config(), binary(), binary()) -> r3_hex_api:response().
unretire(Config, Name, Version) when is_map(Config) and is_binary(Name) and is_binary(Version) ->
    Path = r3_hex_api:build_repository_path(Config, ["packages", Name, "releases", Version, "retire"]),
    r3_hex_api:delete(Config, Path).

%%====================================================================
%% Internal functions
%%====================================================================

%% @private
put_header(Name, Value, Config) ->
    Headers = maps:get(http_headers, Config, #{}),
    Headers2 = maps:put(Name, Value, Headers),
    maps:put(http_headers, Headers2, Config).

%% @private
maybe_put_expect_header(Config) ->
    case maps:get(send_100_continue, Config, true) of
        true -> put_header(<<"expect">>, <<"100-continue">>, Config);
        false -> Config
    end.
