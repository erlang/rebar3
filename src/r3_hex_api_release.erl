%% Vendored from hex_core v0.6.8, do not edit manually

-module(r3_hex_api_release).
-export([
    delete/3,
    get/3,
    publish/2,
    retire/4,
    unretire/3
]).

-export_type([retirement_params/0, retirement_reason/0]).

-type retirement_reason() :: other | invalid | security | deprecated | renamed.

-ifdef(OTP_19).
-type retirement_params() :: #{reason := retirement_reason(), message => binary()}.
-else.
-type retirement_params() :: #{reason => retirement_reason(), message => binary()}.
-endif.
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
publish(Config, Tarball) when is_map(Config) and is_binary(Tarball) ->
    Path = r3_hex_api:build_repository_path(Config, ["publish"]),
    TarballContentType = "application/octet-stream",
    Config2 = put_header(<<"content-length">>, integer_to_binary(byte_size(Tarball)), Config),
    Body = {TarballContentType, Tarball},
    r3_hex_api:post(Config2, Path, Body).

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
retire(Config, Name, Version, Params) when is_map(Config) and is_binary(Name) and is_binary(Version) ->
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

put_header(Name, Value, Config) ->
    Headers = maps:get(http_headers, Config, #{}),
    Headers2 = maps:put(Name, Value, Headers),
    maps:put(http_headers, Headers2, Config).
