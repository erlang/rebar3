%% Vendored from hex_core v0.10.1, do not edit manually

%% @doc
%% Functions for encoding and decoding Hex registries.
-module(r3_hex_registry).
-export([
    encode_names/1,
    decode_names/2,
    build_names/2,
    unpack_names/3,
    encode_versions/1,
    decode_versions/2,
    build_versions/2,
    unpack_versions/3,
    encode_package/1,
    decode_package/3,
    build_package/2,
    unpack_package/4,
    sign_protobuf/2,
    decode_signed/1,
    decode_and_verify_signed/2,
    sign/2,
    verify/3
]).
-include_lib("public_key/include/public_key.hrl").

-type private_key() :: #'RSAPrivateKey'{} | binary().
-type public_key() :: #'RSAPublicKey'{} | binary().

%%====================================================================
%% API functions
%%====================================================================

%% @doc
%% Builds names resource.
build_names(Names, PrivateKey) ->
    Payload = encode_names(Names),
    zlib:gzip(sign_protobuf(Payload, PrivateKey)).

%% @doc
%% Unpacks names resource.
unpack_names(Payload, Repository, PublicKey) ->
    case decode_and_verify_signed(zlib:gunzip(Payload), PublicKey) of
        {ok, Names} -> decode_names(Names, Repository);
        Other -> Other
    end.

%% @private
encode_names(Names) ->
    r3_hex_pb_names:encode_msg(Names, 'Names').

%% @private
decode_names(Payload, no_verify) ->
    {ok, r3_hex_pb_names:decode_msg(Payload, 'Names')};
decode_names(Payload, Repository) ->
    case r3_hex_pb_names:decode_msg(Payload, 'Names') of
        #{repository := Repository, packages := _Packages} = Result ->
            {ok, Result};
        _ ->
            {error, unverified}
    end.

%% @doc
%% Builds versions resource.
build_versions(Versions, PrivateKey) ->
    Payload = encode_versions(Versions),
    zlib:gzip(sign_protobuf(Payload, PrivateKey)).

%% @doc
%% Unpacks versions resource.
unpack_versions(Payload, Repository, PublicKey) ->
    case decode_and_verify_signed(zlib:gunzip(Payload), PublicKey) of
        {ok, Versions} -> decode_versions(Versions, Repository);
        Other -> Other
    end.

%% @private
encode_versions(Versions) ->
    r3_hex_pb_versions:encode_msg(Versions, 'Versions').

%% @private
decode_versions(Payload, no_verify) ->
    {ok, r3_hex_pb_versions:decode_msg(Payload, 'Versions')};
decode_versions(Payload, Repository) ->
    case r3_hex_pb_versions:decode_msg(Payload, 'Versions') of
        #{repository := Repository, packages := _Packages} = Result ->
            {ok, Result};
        _ ->
            {error, unverified}
    end.

%% @doc
%% Builds package resource.
build_package(Package, PrivateKey) ->
    Payload = encode_package(Package),
    zlib:gzip(sign_protobuf(Payload, PrivateKey)).

%% @doc
%% Unpacks package resource.
unpack_package(Payload, Repository, Name, PublicKey) ->
    case decode_and_verify_signed(zlib:gunzip(Payload), PublicKey) of
        {ok, Package} -> decode_package(Package, Repository, Name);
        Other -> Other
    end.

%% @private
encode_package(Package) ->
    r3_hex_pb_package:encode_msg(Package, 'Package').

%% @private
decode_package(Payload, no_verify, no_verify) ->
    {ok, r3_hex_pb_package:decode_msg(Payload, 'Package')};
decode_package(Payload, Repository, Package) ->
    case r3_hex_pb_package:decode_msg(Payload, 'Package') of
        #{repository := Repository, name := Package, releases := _Releases} = Result ->
            {ok, Result};
        _ ->
            {error, unverified}
    end.

%% @private
sign_protobuf(Payload, PrivateKey) ->
    Signature = sign(Payload, PrivateKey),
    r3_hex_pb_signed:encode_msg(#{payload => Payload, signature => Signature}, 'Signed').

%% @private
decode_signed(Signed) ->
    r3_hex_pb_signed:decode_msg(Signed, 'Signed').

%% @private
-spec decode_and_verify_signed(binary(), public_key()) -> {ok, binary()} | {error, term()}.
decode_and_verify_signed(Signed, PublicKey) ->
    #{payload := Payload, signature := Signature} = decode_signed(Signed),
    case verify(Payload, Signature, PublicKey) of
        true -> {ok, Payload};
        false -> {error, unverified};
        {error, Reason} -> {error, Reason}
    end.

%% @private
-spec sign(binary(), private_key()) -> binary().
sign(Binary, PrivateKey) ->
    {ok, RSAPrivateKey} = key(PrivateKey),
    public_key:sign(Binary, sha512, RSAPrivateKey).

%% @private
-spec verify(binary(), binary(), public_key()) -> boolean() | {error, term()}.
verify(Binary, Signature, PublicKey) ->
    case key(PublicKey) of
        {ok, RSAPublicKey} -> public_key:verify(Binary, sha512, Signature, RSAPublicKey);
        {error, Reason} -> {error, Reason}
    end.

%%====================================================================
%% Internal functions
%%====================================================================

%% @private
key(#'RSAPublicKey'{} = Key) ->
    {ok, Key};
key(#'RSAPrivateKey'{} = Key) ->
    {ok, Key};
key(Binary) when is_binary(Binary) ->
    case public_key:pem_decode(Binary) of
        [Entry | _] -> {ok, public_key:pem_entry_decode(Entry)};
        _ -> {error, bad_key}
    end.
