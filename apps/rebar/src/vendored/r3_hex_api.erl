%% Vendored from hex_core v0.20.1, do not edit manually

%% @doc
%% Hex HTTP API
-module(r3_hex_api).

-export([
    delete/2,
    get/2,
    post/3,
    put/3,
    encode_query_string/1,
    build_repository_path/2,
    build_organization_path/2,
    join_path_segments/1
]).
-define(ERL_CONTENT_TYPE, <<"application/vnd.hex+erlang">>).

-export_type([response/0]).

-type response() :: {ok, {r3_hex_http:status(), r3_hex_http:headers(), body() | nil}} | {error, term()}.
-type body() :: #{binary() => value()} | [#{binary() => value()}].
-type value() :: binary() | boolean() | nil | number() | [value()] | #{binary() => value()}.

%% @private
get(Config, Path) ->
    request(Config, get, Path, undefined).

%% @private
post(Config, Path, Body) ->
    request(Config, post, Path, encode_body(Body)).

%% @private
put(Config, Path, Body) ->
    request(Config, put, Path, encode_body(Body)).

%% @private
delete(Config, Path) ->
    request(Config, delete, Path, undefined).

%% @private
encode_query_string(List) ->
    Pairs = lists:map(fun({K, V}) -> {to_list(K), to_list(V)} end, List),
    list_to_binary(uri_string:compose_query(Pairs)).

%% @private
build_repository_path(#{api_repository := Repo}, Path) when is_binary(Repo) ->
    ["repos", Repo | Path];
build_repository_path(#{api_repository := undefined}, Path) ->
    Path.

%% @private
build_organization_path(#{api_organization := Org}, Path) when is_binary(Org) ->
    ["orgs", Org | Path];
build_organization_path(#{api_organization := undefined}, Path) ->
    Path.

%% @private
%% Percent-encodes each segment on its own, so a `/' inside a segment stays
%% inside that segment. The dot segments `.' and `..' are encoded in full
%% because a server resolves them against the parent path otherwise.
join_path_segments(Segments) ->
    iolist_to_binary(lists:join(<<"/">>, lists:map(fun encode_path_segment/1, Segments))).

%% @private
%% Raises `badarg' for a segment that is not valid UTF-8, so every OTP release
%% rejects the same input.
encode_path_segment(Segment) ->
    case unicode:characters_to_binary(Segment) of
        <<".">> -> <<"%2E">>;
        <<"..">> -> <<"%2E%2E">>;
        Binary when is_binary(Binary) -> quote(Binary);
        _ -> error(badarg, [Segment])
    end.

-if(?OTP_RELEASE >= 25).
quote(Segment) ->
    uri_string:quote(Segment).
-else.
quote(Segment) ->
    <<<<(encode_path_byte(Byte))/binary>> || <<Byte>> <= Segment>>.

%% The unreserved set of RFC 3986 section 2.3.
encode_path_byte(Byte) when
    Byte >= $a andalso Byte =< $z;
    Byte >= $A andalso Byte =< $Z;
    Byte >= $0 andalso Byte =< $9;
    Byte =:= $-;
    Byte =:= $.;
    Byte =:= $_;
    Byte =:= $~
->
    <<Byte>>;
encode_path_byte(Byte) ->
    <<$%, (hex_digit(Byte bsr 4)), (hex_digit(Byte band 15))>>.

hex_digit(N) when N < 10 -> $0 + N;
hex_digit(N) -> $A + N - 10.
-endif.

%%====================================================================
%% Internal functions
%%====================================================================

request(Config, Method, PathSegments, Body) when is_list(PathSegments) ->
    Path = join_path_segments(PathSegments),
    request(Config, Method, Path, Body);
request(Config, Method, Path, Body) when is_binary(Path) and is_map(Config) ->
    DefaultHeaders = make_headers(Config),
    ReqHeaders = maps:merge(maps:get(http_headers, Config, #{}), DefaultHeaders),
    ReqHeaders2 = put_new(<<"accept">>, ?ERL_CONTENT_TYPE, ReqHeaders),

    case r3_hex_http:request(Config, Method, build_url(Path, Config), ReqHeaders2, Body) of
        {ok, {Status, RespHeaders, RespBody}} ->
            ContentType = maps:get(<<"content-type">>, RespHeaders, <<"">>),
            Response =
                case binary:match(ContentType, ?ERL_CONTENT_TYPE) of
                    {_, _} ->
                        case r3_hex_safe_binary_to_term:safe_binary_to_term(RespBody) of
                            {ok, Term} ->
                                {ok, {Status, RespHeaders, Term}};
                            {error, Reason} ->
                                {error, Reason}
                        end;
                    nomatch ->
                        {ok, {Status, RespHeaders, nil}}
                end,
            detect_otp_error(Response);
        Other ->
            Other
    end.

%% TODO: not needed after exdoc is fixed
%% @private
build_url(Path, #{api_url := URI}) ->
    <<URI/binary, "/", Path/binary>>.

%% TODO: not needed after exdoc is fixed
%% @private
encode_body({_ContentType, _Body} = Body) ->
    Body;
encode_body(Body) ->
    {binary_to_list(?ERL_CONTENT_TYPE), term_to_binary(Body)}.

%% TODO: not needed after exdoc is fixed
%% @private
%% TODO: copy-pasted from r3_hex_repo
make_headers(Config) ->
    maps:fold(fun set_header/3, #{}, Config).

%% TODO: not needed after exdoc is fixed
%% @private
set_header(api_key, Token, Headers) when is_binary(Token) ->
    maps:put(<<"authorization">>, Token, Headers);
set_header(api_otp, OTP, Headers) when is_binary(OTP) ->
    maps:put(<<"x-hex-otp">>, OTP, Headers);
set_header(_, _, Headers) ->
    Headers.

%% TODO: not needed after exdoc is fixed
%% @private
put_new(Key, Value, Map) ->
    case maps:find(Key, Map) of
        {ok, _} -> Map;
        error -> maps:put(Key, Value, Map)
    end.

%% TODO: not needed after exdoc is fixed
%% @private
to_list(A) when is_atom(A) -> atom_to_list(A);
to_list(B) when is_binary(B) -> unicode:characters_to_list(B);
to_list(I) when is_integer(I) -> integer_to_list(I);
to_list(Str) -> unicode:characters_to_list(Str).

%% TODO: not needed after exdoc is fixed
%% @private
detect_otp_error({ok, {401, Headers, Body}}) ->
    case maps:get(<<"www-authenticate">>, Headers, nil) of
        <<"Bearer realm=\"hex\", error=\"totp_required\"", _/binary>> ->
            {error, otp_required};
        <<"Bearer realm=\"hex\", error=\"invalid_totp\"", _/binary>> ->
            {error, invalid_totp};
        _ ->
            {ok, {401, Headers, Body}}
    end;
detect_otp_error(Response) ->
    Response.
