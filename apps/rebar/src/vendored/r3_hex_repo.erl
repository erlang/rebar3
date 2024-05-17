%% Vendored from hex_core v0.10.0, do not edit manually

%% @doc
%% Repo API.
-module(r3_hex_repo).
-export([
    get_names/1,
    get_versions/1,
    get_package/2,
    get_tarball/3,
    get_docs/3,
    get_public_key/1
]).

%%====================================================================
%% API functions
%%====================================================================

%% @doc
%% Gets names resource from the repository.
%%
%% Examples:
%%
%% ```
%% > r3_hex_repo:get_names(r3_hex_core:default_config()).
%% {ok, {200, ...,
%%     [
%%         #{name => <<"package1">>},
%%         #{name => <<"package2">>},
%%     ]}}
%% '''
%% @end
get_names(Config) when is_map(Config) ->
    Decoder = fun(Data) ->
        r3_hex_registry:decode_names(Data, verify_repo(Config))
    end,
    get_protobuf(Config, <<"names">>, Decoder).

%% @doc
%% Gets versions resource from the repository.
%%
%% Examples:
%%
%% ```
%% > r3_hex_repo:get_versions(Config).
%% {ok, {200, ...,
%%     [
%%         #{name => <<"package1">>, retired => [],
%%           versions => [<<"1.0.0">>]},
%%         #{name => <<"package2">>, retired => [<<"0.5.0>>"],
%%           versions => [<<"0.5.0">>, <<"1.0.0">>]},
%%     ]}}
%% '''
%% @end
get_versions(Config) when is_map(Config) ->
    Decoder = fun(Data) ->
        r3_hex_registry:decode_versions(Data, verify_repo(Config))
    end,
    get_protobuf(Config, <<"versions">>, Decoder).

%% @doc
%% Gets package resource from the repository.
%%
%% Examples:
%%
%% ```
%% > r3_hex_repo:get_package(r3_hex_core:default_config(), <<"package1">>).
%% {ok, {200, ...,
%%     {
%%         #{checksum => ..., version => <<"0.5.0">>, dependencies => []},
%%         #{checksum => ..., version => <<"1.0.0">>, dependencies => [
%%             #{package => <<"package2">>, optional => true, requirement => <<"~> 0.1">>}
%%         ]},
%%     ]}}
%% '''
%% @end
get_package(Config, Name) when is_binary(Name) and is_map(Config) ->
    Verify = maps:get(repo_verify_origin, Config, true),
    Decoder = fun(Data) ->
        case Verify of
            true -> r3_hex_registry:decode_package(Data, repo_name(Config), Name);
            false -> r3_hex_registry:decode_package(Data, no_verify, no_verify)
        end
    end,
    get_protobuf(Config, <<"packages/", Name/binary>>, Decoder).

%% @doc
%% Gets tarball from the repository.
%%
%% Examples:
%%
%% ```
%% > {ok, {200, _, Tarball}} = r3_hex_repo:get_tarball(r3_hex_core:default_config(), <<"package1">>, <<"1.0.0">>),
%% > {ok, #{metadata := Metadata}} = r3_hex_tarball:unpack(Tarball, memory).
%% '''
%% @end
get_tarball(Config, Name, Version) ->
    ReqHeaders = make_headers(Config),

    case get(Config, tarball_url(Config, Name, Version), ReqHeaders) of
        {ok, {200, RespHeaders, Tarball}} ->
            {ok, {200, RespHeaders, Tarball}};
        Other ->
            Other
    end.

%% @doc
%% Gets docs tarball from the repository.
%%
%% Examples:
%%
%% ```
%% > {ok, {200, _, Docs}} = r3_hex_repo:get_docs(r3_hex_core:default_config(), <<"package1">>, <<"1.0.0">>),
%% > r3_hex_tarball:unpack_docs(Docs, memory)
%% {ok, [{"index.html", <<"<!doctype>">>}, ...]}
%% '''
get_docs(Config, Name, Version) ->
    ReqHeaders = make_headers(Config),

    case get(Config, docs_url(Config, Name, Version), ReqHeaders) of
        {ok, {200, RespHeaders, Docs}} ->
            {ok, {200, RespHeaders, Docs}};
        Other ->
            Other
    end.

%% @doc
%% Gets the public key from the repository.
%%
%% Examples:
%%
%% ```
%% > r3_hex_repo:get_public_key(r3_hex_core:default_config())
%% {ok, {200, _, PublicKey}}
%% '''
get_public_key(Config) ->
    ReqHeaders = make_headers(Config),
    URI = build_url(Config, <<"public_key">>),

    case get(Config, URI, ReqHeaders) of
        {ok, {200, RespHeaders, PublicKey}} ->
            {ok, {200, RespHeaders, PublicKey}};
        Other ->
            Other
    end.

%%====================================================================
%% Internal functions
%%====================================================================

%% @private
get(Config, URI, Headers) ->
    r3_hex_http:request(Config, get, URI, Headers, undefined).

%% @private
get_protobuf(Config, Path, Decoder) ->
    PublicKey = maps:get(repo_public_key, Config),
    ReqHeaders = make_headers(Config),

    case get(Config, build_url(Config, Path), ReqHeaders) of
        {ok, {200, RespHeaders, Compressed}} ->
            Signed = zlib:gunzip(Compressed),
            case decode(Signed, PublicKey, Decoder, Config) of
                {ok, Decoded} ->
                    {ok, {200, RespHeaders, Decoded}};
                {error, _} = Error ->
                    Error
            end;
        Other ->
            Other
    end.

%% @private
decode(Signed, PublicKey, Decoder, Config) ->
    Verify = maps:get(repo_verify, Config, true),

    case Verify of
        true ->
            case r3_hex_registry:decode_and_verify_signed(Signed, PublicKey) of
                {ok, Payload} ->
                    Decoder(Payload);
                Other ->
                    Other
            end;
        false ->
            #{payload := Payload} = r3_hex_registry:decode_signed(Signed),
            Decoder(Payload)
    end.

%% @private
verify_repo(Config) ->
    case maps:get(repo_verify_origin, Config, true) of
        true -> repo_name(Config);
        false -> no_verify
    end.

%% @private
repo_name(#{repo_organization := Name}) when is_binary(Name) -> Name;
repo_name(#{repo_name := Name}) when is_binary(Name) -> Name.

%% @private
tarball_url(Config, Name, Version) ->
    Filename = tarball_filename(Name, Version),
    build_url(Config, <<"tarballs/", Filename/binary>>).

%% @private
docs_url(Config, Name, Version) ->
    Filename = docs_filename(Name, Version),
    build_url(Config, <<"docs/", Filename/binary>>).

%% @private
build_url(#{repo_url := URI, repo_organization := Org}, Path) when is_binary(Org) ->
    <<URI/binary, "/repos/", Org/binary, "/", Path/binary>>;
build_url(#{repo_url := URI, repo_organization := undefined}, Path) ->
    <<URI/binary, "/", Path/binary>>;
build_url(Config, Path) ->
    build_url(Config#{repo_organization => undefined}, Path).

%% @private
tarball_filename(Name, Version) ->
    <<Name/binary, "-", Version/binary, ".tar">>.

%% @private
docs_filename(Name, Version) ->
    <<Name/binary, "-", Version/binary, ".tar.gz">>.

%% @private
make_headers(Config) ->
    maps:fold(fun set_header/3, #{}, Config).

%% @private
set_header(http_etag, ETag, Headers) when is_binary(ETag) ->
    maps:put(<<"if-none-match">>, ETag, Headers);
set_header(repo_key, Token, Headers) when is_binary(Token) ->
    maps:put(<<"authorization">>, Token, Headers);
set_header(_, _, Headers) ->
    Headers.
