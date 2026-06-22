%% Vendored from hex_core v0.18.0, do not edit manually

%% @doc
%% Functions for creating and unpacking Hex tarballs.
-module(r3_hex_tarball).
-export([
    create/2, create/3,
    create_docs/1, create_docs/2,
    unpack/2, unpack/3,
    unpack_docs/2, unpack_docs/3,
    format_checksum/1,
    format_error/1
]).
-ifdef(TEST).
-export([do_decode_metadata/1, do_decode_metadata/2, gzip/1, normalize_requirements/1]).
-endif.
-define(VERSION, <<"3">>).
-define(HASH_CHUNK_SIZE, 65536).
-define(MAX_VERSION_SIZE, 32).
-define(MAX_CHECKSUM_SIZE, 128).
-define(MAX_METADATA_SIZE, 1024 * 1024).
-define(METADATA_CHUNK_SIZE, 4096).
-define(BUILD_TOOL_FILES, [
    {<<"mix.exs">>, <<"mix">>},
    {<<"rebar.config">>, <<"rebar3">>},
    {<<"rebar">>, <<"rebar3">>},
    {<<"Makefile">>, <<"make">>},
    {<<"Makefile.win">>, <<"make">>}
]).
-include_lib("kernel/include/file.hrl").

-type checksum() :: binary().
-type contents() :: [{filename(), binary()}].
-type filename() :: string().
-type files() :: [{filename(), filename() | binary()}].
-type metadata() :: map().
-type tarball() :: binary().

%%====================================================================
%% API functions
%%====================================================================

%% @doc
%% Creates a package tarball.
%%
%% Returns the binary of the tarball the "inner checksum" and "outer checksum".
%% The inner checksum is deprecated in favor of the outer checksum.
%%
%% Examples:
%%
%% ```
%% > Metadata = #{<<"name">> => <<"foo">>, <<"version">> => <<"1.0.0">>},
%% > Files = [{"src/foo.erl", <<"-module(foo).">>}],
%% > r3_hex_tarball:create(Metadata, Files).
%% {ok, #{tarball => <<86,69,...>>,
%%        outer_checksum => <<40,32,...>>,
%%        inner_checksum => <<178,12,...>>}}
%% '''
%% @end
-spec create(metadata(), files(), r3_hex_core:config()) ->
    {ok, #{
        tarball => tarball(),
        outer_checksum => checksum(),
        inner_checksum => tarball()
    }}
    | {error, term()}.
create(Metadata, Files, Config) ->
    #{
        tarball_max_size := TarballMaxSize,
        tarball_max_uncompressed_size := TarballMaxUncompressedSize
    } = Config,
    FilesRoot = maps:get(tarball_files_root, Config, "."),

    MetadataBinary = encode_metadata(Metadata),

    case valid_size(MetadataBinary, ?MAX_METADATA_SIZE) of
        false ->
            {error, {tarball, {file_too_big, "metadata.config"}}};
        true ->
            case validate_create_files(Files, FilesRoot) of
                {ok, ValidatedFiles} ->
                    ContentsTarball = create_memory_tarball(ValidatedFiles),
                    ContentsTarballCompressed = gzip(ContentsTarball),
                    InnerChecksum = inner_checksum(
                        ?VERSION, MetadataBinary, ContentsTarballCompressed
                    ),
                    InnerChecksumBase16 = encode_base16(InnerChecksum),

                    OuterFiles = [
                        {"VERSION", ?VERSION},
                        {"CHECKSUM", InnerChecksumBase16},
                        {"metadata.config", MetadataBinary},
                        {"contents.tar.gz", ContentsTarballCompressed}
                    ],

                    case valid_size(ContentsTarball, TarballMaxUncompressedSize) of
                        true ->
                            Tarball = create_memory_tarball(OuterFiles),
                            OuterChecksum = checksum(Tarball),

                            case valid_size(Tarball, TarballMaxSize) of
                                true ->
                                    {ok, #{
                                        tarball => Tarball,
                                        outer_checksum => OuterChecksum,
                                        inner_checksum => InnerChecksum
                                    }};
                                false ->
                                    {error, {tarball, {too_big_compressed, TarballMaxSize}}}
                            end;
                        false ->
                            {error, {tarball, {too_big_uncompressed, TarballMaxUncompressedSize}}}
                    end;
                {error, _} = Error ->
                    Error
            end
    end.

-spec create(metadata(), files()) ->
    {ok, #{
        tarball => tarball(),
        outer_checksum => checksum(),
        inner_checksum => tarball()
    }}
    | {error, term()}.
create(Metadata, Files) ->
    create(Metadata, Files, r3_hex_core:default_config()).

%% @doc
%% Creates a docs tarball.
%%
%% Examples:
%%
%% ```
%% > Files = [{"doc/index.html", <<"Docs">>}],
%% > r3_hex_tarball:create_docs(Files).
%% {ok, <<86,69,...>>}
%% '''
%% @end
-spec create_docs(files(), r3_hex_core:config()) -> {ok, tarball()} | {error, term()}.
create_docs(Files, Config) ->
    #{
        docs_tarball_max_size := TarballMaxSize,
        docs_tarball_max_uncompressed_size := TarballMaxUncompressedSize
    } = Config,
    FilesRoot = maps:get(tarball_files_root, Config, "."),

    case validate_create_files(Files, FilesRoot) of
        {ok, ValidatedFiles} ->
            UncompressedTarball = create_memory_tarball(ValidatedFiles),

            case valid_size(UncompressedTarball, TarballMaxUncompressedSize) of
                true ->
                    Tarball = gzip(UncompressedTarball),

                    case valid_size(Tarball, TarballMaxSize) of
                        true ->
                            {ok, Tarball};
                        false ->
                            {error, {tarball, {too_big_compressed, TarballMaxSize}}}
                    end;
                false ->
                    {error, {tarball, {too_big_uncompressed, TarballMaxUncompressedSize}}}
            end;
        {error, _} = Error ->
            Error
    end.

-spec create_docs(files()) -> {ok, tarball()} | {error, term()}.
create_docs(Files) ->
    create_docs(Files, r3_hex_core:default_config()).

%% @doc
%% Unpacks a package tarball.
%%
%% Remember to verify the outer tarball checksum against the registry checksum
%% returned from `r3_hex_repo:get_package(Config, Package)'.
%%
%% The first argument is the tarball, either as a binary or `{file, Path}'
%% to read from a file on disk. Using `{file, Path}' avoids loading the
%% tarball into memory.
%%
%% The second argument controls the output:
%%
%% - `memory' - unpack contents into memory and return them
%% - `none' - only extract metadata and checksums, skip contents
%% - A path string - extract contents to the given directory
%%
%% Examples:
%%
%% ```
%% > r3_hex_tarball:unpack(Tarball, memory).
%% {ok,#{outer_checksum => <<...>>,
%%       contents => [{"src/foo.erl",<<"-module(foo).">>}],
%%       metadata => #{<<"name">> => <<"foo">>, ...}}}
%%
%% > r3_hex_tarball:unpack(Tarball, none).
%% {ok,#{outer_checksum => <<...>>,
%%       metadata => #{<<"name">> => <<"foo">>, ...}}}
%%
%% > r3_hex_tarball:unpack(Tarball, "path/to/unpack").
%% {ok,#{outer_checksum => <<...>>,
%%       metadata => #{<<"name">> => <<"foo">>, ...}}}
%% '''
-spec unpack
    (tarball() | {file, filename()}, memory, r3_hex_core:config()) ->
        {ok, #{
            outer_checksum => checksum(),
            inner_checksum => checksum(),
            metadata => metadata(),
            contents => contents()
        }}
        | {error, term()};
    (tarball() | {file, filename()}, none, r3_hex_core:config()) ->
        {ok, #{
            outer_checksum => checksum(),
            inner_checksum => checksum(),
            metadata => metadata()
        }}
        | {error, term()};
    (tarball() | {file, filename()}, filename(), r3_hex_core:config()) ->
        {ok, #{
            outer_checksum => checksum(),
            inner_checksum => checksum(),
            metadata => metadata()
        }}
        | {error, term()}.
unpack(Input, memory, Config) ->
    case check_input_size(Input, Config) of
        true ->
            OuterChecksum = outer_checksum(Input),
            Source = tar_source(Input),
            case r3_hex_erl_tar:extract(Source, [memory]) of
                {ok, []} ->
                    {error, {tarball, empty}};
                {ok, FileList} ->
                    case validate_outer_file_sizes(maps:from_list(FileList)) of
                        {ok, Files} ->
                            do_unpack(Files, OuterChecksum, memory, Config);
                        {error, _} = Error ->
                            Error
                    end;
                {error, Reason} ->
                    {error, {tarball, Reason}}
            end;
        false ->
            {error, {tarball, too_big}}
    end;
unpack(Input, Output, Config) ->
    case check_input_size(Input, Config) of
        true ->
            OuterChecksum = outer_checksum(Input),
            Source = tar_source(Input),
            TmpDir = tmp_path(),
            ok = file:make_dir(TmpDir),
            try
                case r3_hex_erl_tar:extract(Source, [{cwd, TmpDir}]) of
                    ok ->
                        case read_outer_files(TmpDir) of
                            {ok, Files} ->
                                do_unpack(Files, OuterChecksum, Output, Config);
                            {error, _} = Error ->
                                Error
                        end;
                    {error, Reason} ->
                        {error, {tarball, Reason}}
                end
            after
                remove_dir(TmpDir)
            end;
        false ->
            {error, {tarball, too_big}}
    end.

%% @doc
%% Unpacks a package tarball.
%%
%% @see unpack/3
-spec unpack
    (tarball() | {file, filename()}, memory) ->
        {ok, #{
            outer_checksum => checksum(),
            inner_checksum => checksum(),
            metadata => metadata(),
            contents => contents()
        }}
        | {error, term()};
    (tarball() | {file, filename()}, none) ->
        {ok, #{
            outer_checksum => checksum(),
            inner_checksum => checksum(),
            metadata => metadata()
        }}
        | {error, term()};
    (tarball() | {file, filename()}, filename()) ->
        {ok, #{
            outer_checksum => checksum(),
            inner_checksum => checksum(),
            metadata => metadata()
        }}
        | {error, term()}.
unpack(Tarball, Output) ->
    unpack(Tarball, Output, r3_hex_core:default_config()).

%% @doc
%% Unpacks a documentation tarball.
%%
%% The first argument is the tarball, either as a binary or `{file, Path}'
%% to read from a file on disk. Using `{file, Path}' avoids loading the
%% tarball into memory.
%%
%% Examples:
%%
%% ```
%% > r3_hex_tarball:unpack_docs(Tarball, memory).
%% {ok, [{"index.html", <<"<!doctype>">>}, ...]}
%%
%% > r3_hex_tarball:unpack_docs(Tarball, "path/to/unpack").
%% ok
%% '''
-spec unpack_docs
    (tarball() | {file, filename()}, memory, r3_hex_core:config()) ->
        {ok, contents()} | {error, term()};
    (tarball() | {file, filename()}, filename(), r3_hex_core:config()) -> ok | {error, term()}.
unpack_docs(Input, Output, Config) ->
    case check_docs_input_size(Input, Config) of
        true ->
            MaxSize = maps:get(docs_tarball_max_uncompressed_size, Config),
            unpack_tarball(tar_source(Input), Output, MaxSize);
        false ->
            {error, {tarball, too_big}}
    end.

-spec unpack_docs
    (tarball() | {file, filename()}, memory) -> {ok, contents()} | {error, term()};
    (tarball() | {file, filename()}, filename()) -> ok | {error, term()}.
unpack_docs(Input, Output) ->
    unpack_docs(Input, Output, r3_hex_core:default_config()).

%% @doc
%% Returns base16-encoded representation of checksum.
-spec format_checksum(checksum()) -> binary().
format_checksum(Checksum) ->
    encode_base16(Checksum).

%% @doc
%% Converts an error reason term to a human-readable error message string.
-spec format_error(term()) -> string().
format_error({tarball, {file_too_big, Name}}) ->
    io_lib:format("file too big: ~s", [Name]);
format_error({tarball, empty}) ->
    "empty tarball";
format_error({tarball, {too_big_uncompressed, Size}}) ->
    io_lib:format("package exceeds max uncompressed size ~w ~s", [format_byte_size(Size), "MB"]);
format_error({tarball, {too_big_compressed, Size}}) ->
    io_lib:format("package exceeds max compressed size ~w ~s", [format_byte_size(Size), "MB"]);
format_error({tarball, {missing_files, Files}}) ->
    io_lib:format("missing files: ~p", [Files]);
format_error({tarball, {bad_version, Vsn}}) ->
    io_lib:format("unsupported version: ~p", [Vsn]);
format_error({tarball, invalid_checksum}) ->
    "invalid tarball checksum";
format_error({tarball, {unsafe_path, Name}}) ->
    io_lib:format("unsafe path in tarball: ~s", [Name]);
format_error({tarball, {unsafe_symlink, Name, LinkTarget}}) ->
    io_lib:format("unsafe symlink in tarball: ~s -> ~s", [Name, LinkTarget]);
format_error({tarball, {unsupported_file_type, Name, Type}}) ->
    io_lib:format("unsupported file type in tarball: ~s (~p)", [Name, Type]);
format_error({tarball, Reason}) ->
    "tarball error, " ++ r3_hex_erl_tar:format_error(Reason);
format_error({inner_tarball, Reason}) ->
    "inner tarball error, " ++ r3_hex_erl_tar:format_error(Reason);
format_error({metadata, invalid_terms}) ->
    "error reading package metadata: invalid terms";
format_error({metadata, not_key_value}) ->
    "error reading package metadata: not in key-value format";
format_error({metadata, Reason}) ->
    "error reading package metadata" ++ r3_safe_erl_term:format_error(Reason);
format_error({checksum_mismatch, ExpectedChecksum, ActualChecksum}) ->
    io_lib:format(
        "tarball checksum mismatch~n~n" ++
            "Expected (base16-encoded): ~s~n" ++
            "Actual   (base16-encoded): ~s",
        [encode_base16(ExpectedChecksum), encode_base16(ActualChecksum)]
    ).

format_byte_size(Size) ->
    Size / 1000000.

%%====================================================================
%% Internal functions
%%====================================================================

%% @private
inner_checksum(Version, MetadataBinary, {path, ContentsPath}) ->
    HashState0 = crypto:hash_init(sha256),
    HashState1 = crypto:hash_update(HashState0, Version),
    HashState2 = crypto:hash_update(HashState1, MetadataBinary),
    HashState3 = stream_file_hash(HashState2, ContentsPath),
    crypto:hash_final(HashState3);
inner_checksum(Version, MetadataBinary, ContentsBinary) ->
    Blob = <<Version/binary, MetadataBinary/binary, ContentsBinary/binary>>,
    crypto:hash(sha256, Blob).

%% @private
checksum(ContentsBinary) when is_binary(ContentsBinary) ->
    crypto:hash(sha256, ContentsBinary).

%% @private
tar_source({file, Path}) -> Path;
tar_source(Tarball) -> {binary, Tarball}.

%% @private
outer_checksum({file, Path}) -> file_checksum(Path);
outer_checksum(Tarball) -> crypto:hash(sha256, Tarball).

%% @private
check_input_size({file, Path}, Config) ->
    valid_file_size(Path, maps:get(tarball_max_size, Config));
check_input_size(Tarball, Config) ->
    valid_size(Tarball, maps:get(tarball_max_size, Config)).

%% @private
check_docs_input_size({file, Path}, Config) ->
    valid_file_size(Path, maps:get(docs_tarball_max_size, Config));
check_docs_input_size(Tarball, Config) ->
    valid_size(Tarball, maps:get(docs_tarball_max_size, Config)).

%% @private
encode_metadata(Meta) ->
    Data = lists:map(
        fun(MetaPair) ->
            String = io_lib_pretty:print(binarify(MetaPair), [{encoding, utf8}]),
            unicode:characters_to_binary([String, ".\n"])
        end,
        maps:to_list(Meta)
    ),
    iolist_to_binary(Data).

%% @private
do_unpack(Files, OuterChecksum, Output, Config) ->
    State = #{
        inner_checksum => undefined,
        outer_checksum => OuterChecksum,
        contents => undefined,
        files => Files,
        metadata => undefined,
        output => Output,
        config => Config
    },
    State1 = check_files(State),
    State2 = check_version(State1),
    State3 = check_inner_checksum(State2),
    State4 = decode_metadata(State3),
    finish_unpack(State4).

%% @private
finish_unpack({error, _} = Error) ->
    Error;
finish_unpack(#{
    metadata := Metadata,
    files := Files,
    inner_checksum := InnerChecksum,
    outer_checksum := OuterChecksum,
    output := Output,
    config := Config
}) ->
    _ = maps:get("VERSION", Files),
    Contents = maps:get("contents.tar.gz", Files),
    MaxUncompressedSize = maps:get(tarball_max_uncompressed_size, Config),

    Result = #{
        inner_checksum => InnerChecksum,
        outer_checksum => OuterChecksum,
        metadata => Metadata
    },

    case Output of
        none ->
            {ok, Result};
        memory ->
            case unpack_contents(Contents, memory, MaxUncompressedSize) of
                {ok, UnpackedContents} ->
                    {ok, Result#{contents => UnpackedContents}};
                {error, Reason} ->
                    {error, {inner_tarball, Reason}}
            end;
        _ ->
            filelib:ensure_dir(filename:join(Output, "*")),
            case unpack_contents(Contents, Output, MaxUncompressedSize) of
                ok ->
                    [
                        try_updating_mtime(filename:join(Output, P))
                     || P <- filelib:wildcard("**", Output)
                    ],
                    copy_metadata_config(Output, maps:get("metadata.config", Files)),
                    {ok, Result};
                {error, Reason} ->
                    {error, {inner_tarball, Reason}}
            end
    end.

%% @private
unpack_contents(Contents, Output, MaxSize) ->
    Opts =
        case Output of
            memory -> [memory, compressed];
            _ -> [{cwd, Output}, compressed]
        end,
    Source =
        case Contents of
            {path, ContentsPath} -> ContentsPath;
            ContentsBinary -> {binary, ContentsBinary}
        end,
    case r3_hex_erl_tar:extract(Source, [{max_size, MaxSize} | Opts]) of
        {error, too_big} ->
            {error, {too_big_uncompressed, MaxSize}};
        Other ->
            Other
    end.

%% @private
copy_metadata_config(Output, MetadataBinary) ->
    ok = file:write_file(filename:join(Output, "hex_metadata.config"), MetadataBinary).

%% @private
check_files(#{files := Files} = State) ->
    RequiredFiles = ["VERSION", "CHECKSUM", "metadata.config", "contents.tar.gz"],
    case diff_keys(Files, RequiredFiles, []) of
        ok ->
            State;
        {error, {missing_keys, Keys}} ->
            {error, {tarball, {missing_files, Keys}}}
    end.

%% @private
check_version({error, _} = Error) ->
    Error;
check_version(#{files := Files} = State) ->
    case maps:get("VERSION", Files) of
        <<"3">> ->
            State;
        Version ->
            {error, {tarball, {bad_version, Version}}}
    end.

%% @private
% Note: This checksum is deprecated
check_inner_checksum({error, _} = Error) ->
    Error;
check_inner_checksum(#{files := Files} = State) ->
    ChecksumBase16 = maps:get("CHECKSUM", Files),
    ExpectedChecksum = decode_base16(ChecksumBase16),

    Version = maps:get("VERSION", Files),
    MetadataBinary = maps:get("metadata.config", Files),
    Contents = maps:get("contents.tar.gz", Files),
    ActualChecksum = inner_checksum(Version, MetadataBinary, Contents),

    if
        byte_size(ExpectedChecksum) /= 32 ->
            {error, {tarball, invalid_inner_checksum}};
        ExpectedChecksum == ActualChecksum ->
            maps:put(inner_checksum, ExpectedChecksum, State);
        true ->
            {error, {tarball, {inner_checksum_mismatch, ExpectedChecksum, ActualChecksum}}}
    end.

%% @private
decode_metadata({error, _} = Error) ->
    Error;
decode_metadata(#{files := #{"metadata.config" := Binary}, config := Config} = State) when
    is_binary(Binary)
->
    Fields = maps:get(metadata_fields, Config, all),
    case do_decode_metadata(Binary, Fields) of
        #{} = Metadata -> maps:put(metadata, normalize_metadata(Metadata), State);
        Other -> Other
    end.

-ifdef(TEST).
do_decode_metadata(Binary) ->
    do_decode_metadata(Binary, all).
-endif.

%% @private
do_decode_metadata(Binary, all) when is_binary(Binary) ->
    case decode_metadata_chunked(utf8, Binary, <<>>, [], "", []) of
        latin1_fallback ->
            decode_metadata_chunked(latin1, Binary, <<>>, [], "", []);
        Other ->
            Other
    end;
do_decode_metadata(Binary, Fields) when is_binary(Binary), is_list(Fields) ->
    case decode_metadata_streaming(utf8, Binary, <<>>, [], "", [], Fields, start) of
        latin1_fallback ->
            decode_metadata_streaming(latin1, Binary, <<>>, [], "", [], Fields, start);
        Other ->
            Other
    end.

%% @private
%% Streams the metadata.config binary through r3_safe_erl_term:tokens/2 in
%% small chunks so we never materialize the whole binary as a char list.
%% Each accepted dot-terminated form is parsed and accumulated immediately,
%% keeping peak memory at roughly one chunk + one term's tokens + AST.
decode_metadata_chunked(Encoding, Binary, IncTail, Cont, Chars, Acc) ->
    case Chars of
        [] when Binary =:= <<>>, IncTail =:= <<>> ->
            flush_metadata_eof(Cont, Acc);
        [] when Binary =:= <<>>, Encoding =:= utf8 ->
            %% Trailing bytes that can never form a complete UTF-8 sequence —
            %% restart the whole decode in latin1 mode rather than spin.
            latin1_fallback;
        [] ->
            case decode_metadata_chunk(Encoding, Binary, IncTail) of
                {ok, NewChars, NewBinary, NewTail} ->
                    feed_metadata(Encoding, Cont, NewChars, NewBinary, NewTail, Acc);
                latin1_fallback ->
                    latin1_fallback
            end;
        _ ->
            feed_metadata(Encoding, Cont, Chars, Binary, IncTail, Acc)
    end.

%% @private
feed_metadata(Encoding, Cont, Chars, Binary, IncTail, Acc) ->
    case r3_safe_erl_term:tokens(Cont, Chars) of
        {more, NewCont} ->
            decode_metadata_chunked(Encoding, Binary, IncTail, NewCont, "", Acc);
        {done, {ok, Tokens, _}, RestChars} ->
            case parse_metadata_term(Tokens) of
                {ok, Term} ->
                    decode_metadata_chunked(
                        Encoding, Binary, IncTail, [], normalize_rest_chars(RestChars), [Term | Acc]
                    );
                {error, _} = Err ->
                    Err
            end;
        {done, {eof, _}, _} ->
            finalize_metadata(Acc);
        {done, {error, {_, r3_safe_erl_term, Reason}, _}, _} ->
            {error, {metadata, Reason}}
    end.

%% @private
flush_metadata_eof([], Acc) ->
    finalize_metadata(Acc);
flush_metadata_eof(Cont, Acc) ->
    case r3_safe_erl_term:tokens(Cont, eof) of
        {done, {eof, _}, _} ->
            finalize_metadata(Acc);
        {done, {ok, _Tokens, _}, _} ->
            {error, {metadata, invalid_terms}};
        {done, {error, {_, r3_safe_erl_term, Reason}, _}, _} ->
            {error, {metadata, Reason}}
    end.

%% @private
finalize_metadata([]) ->
    {error, {metadata, invalid_terms}};
finalize_metadata(Acc) ->
    try maps:from_list(lists:reverse(Acc)) of
        Map -> Map
    catch
        error:badarg -> {error, {metadata, not_key_value}}
    end.

%% @private
parse_metadata_term(Tokens) ->
    case erl_parse:parse_term(Tokens) of
        {ok, Term} -> {ok, Term};
        {error, _} -> {error, {metadata, invalid_terms}}
    end.

%% @private
decode_metadata_chunk(utf8, Binary, IncTail) ->
    {Chunk, Rest} = take_metadata_chunk(Binary),
    Combined =
        case IncTail of
            <<>> -> Chunk;
            _ -> <<IncTail/binary, Chunk/binary>>
        end,
    case unicode:characters_to_list(Combined, utf8) of
        L when is_list(L) ->
            {ok, L, Rest, <<>>};
        {incomplete, L, NewTail} ->
            {ok, L, Rest, NewTail};
        {error, _, _} ->
            latin1_fallback
    end;
decode_metadata_chunk(latin1, Binary, _IncTail) ->
    {Chunk, Rest} = take_metadata_chunk(Binary),
    {ok, binary_to_list(Chunk), Rest, <<>>}.

%% @private
take_metadata_chunk(Binary) when byte_size(Binary) > ?METADATA_CHUNK_SIZE ->
    <<Chunk:(?METADATA_CHUNK_SIZE)/binary, Rest/binary>> = Binary,
    {Chunk, Rest};
take_metadata_chunk(Binary) ->
    {Binary, <<>>}.

%% @private
normalize_rest_chars(eof) -> "";
normalize_rest_chars(L) when is_list(L) -> L.

%% @private
%% Streams the metadata.config binary through r3_safe_erl_term:token/2 one token
%% at a time. Forms whose key is in Fields are accumulated and parsed; forms
%% whose key is not in Fields are discarded with only a depth counter held in
%% state, so peak memory stays bounded regardless of the unwanted form's size.
decode_metadata_streaming(Encoding, Binary, IncTail, Cont, Chars, Acc, Fields, State) ->
    case Chars of
        [] when Binary =:= <<>>, IncTail =:= <<>> ->
            flush_metadata_streaming_eof(Cont, Acc, Fields, State);
        [] when Binary =:= <<>>, Encoding =:= utf8 ->
            latin1_fallback;
        [] ->
            case decode_metadata_chunk(Encoding, Binary, IncTail) of
                {ok, NewChars, NewBinary, NewTail} ->
                    feed_metadata_streaming(
                        Encoding, Cont, NewChars, NewBinary, NewTail, Acc, Fields, State
                    );
                latin1_fallback ->
                    latin1_fallback
            end;
        _ ->
            feed_metadata_streaming(Encoding, Cont, Chars, Binary, IncTail, Acc, Fields, State)
    end.

%% @private
feed_metadata_streaming(Encoding, Cont, Chars, Binary, IncTail, Acc, Fields, State) ->
    case r3_safe_erl_term:token(Cont, Chars) of
        {more, NewCont} ->
            decode_metadata_streaming(Encoding, Binary, IncTail, NewCont, "", Acc, Fields, State);
        {done, {ok, Token, _}, RestChars} ->
            case advance_metadata_state(State, Acc, Fields, Token) of
                {next, NewState, NewAcc} ->
                    decode_metadata_streaming(
                        Encoding,
                        Binary,
                        IncTail,
                        [],
                        normalize_rest_chars(RestChars),
                        NewAcc,
                        Fields,
                        NewState
                    );
                {error, _} = Err ->
                    Err
            end;
        {done, {eof, _}, _} ->
            finalize_metadata_streaming(Acc, State);
        {done, {error, {_, r3_safe_erl_term, Reason}, _}, _} ->
            {error, {metadata, Reason}}
    end.

%% @private
flush_metadata_streaming_eof([], Acc, _Fields, State) ->
    finalize_metadata_streaming(Acc, State);
flush_metadata_streaming_eof(Cont, Acc, Fields, State) ->
    case r3_safe_erl_term:token(Cont, eof) of
        {done, {ok, Token, _}, _} ->
            case advance_metadata_state(State, Acc, Fields, Token) of
                {next, NewState, NewAcc} ->
                    flush_metadata_streaming_eof([], NewAcc, Fields, NewState);
                {error, _} = Err ->
                    Err
            end;
        {done, {eof, _}, _} ->
            finalize_metadata_streaming(Acc, State);
        {done, {error, {_, r3_safe_erl_term, Reason}, _}, _} ->
            {error, {metadata, Reason}}
    end.

%% @private
finalize_metadata_streaming(Acc, start) ->
    finalize_metadata(Acc);
finalize_metadata_streaming([], between) ->
    #{};
finalize_metadata_streaming(Acc, between) ->
    finalize_metadata(Acc);
finalize_metadata_streaming(_Acc, _State) ->
    {error, {metadata, invalid_terms}}.

%% @private
%% State machine for streaming the metadata.config schema. Forms are required
%% to be `{<<"key">>, value}.` — anything else is rejected as invalid.
%%
%% States: start | between | {after_open, Prefix} | {after_left_binary, Prefix}
%%       | {after_key, KeyChars, Prefix} | {after_right_binary, KeyChars, Prefix}
%%       | {accumulate, Prefix, Depth} | {skip, Depth}
%%
%% `start` is the initial position; `between` is the position after a form has
%% been completed. Distinguishing them lets empty input return the same
%% invalid_terms error as the non-streaming path while a stream that
%% successfully skipped every form returns an empty map.
advance_metadata_state(Open, Acc, _Fields, {'{', _} = T) when Open =:= start; Open =:= between ->
    {next, {after_open, [T]}, Acc};
advance_metadata_state({after_open, Prefix}, Acc, _Fields, {'<<', _} = T) ->
    {next, {after_left_binary, [T | Prefix]}, Acc};
advance_metadata_state({after_left_binary, Prefix}, Acc, _Fields, {string, _, KeyChars} = T) ->
    {next, {after_key, KeyChars, [T | Prefix]}, Acc};
advance_metadata_state({after_key, KeyChars, Prefix}, Acc, _Fields, {'>>', _} = T) ->
    {next, {after_right_binary, KeyChars, [T | Prefix]}, Acc};
advance_metadata_state({after_right_binary, KeyChars, Prefix}, Acc, Fields, {',', _} = T) ->
    case extract_metadata_key(KeyChars) of
        {ok, Key} ->
            case lists:member(Key, Fields) of
                true -> {next, {accumulate, [T | Prefix], 1}, Acc};
                false -> {next, {skip, 1}, Acc}
            end;
        error ->
            {error, {metadata, not_key_value}}
    end;
advance_metadata_state({accumulate, Prefix, 0}, Acc, _Fields, {dot, _} = T) ->
    Tokens = lists:reverse([T | Prefix]),
    case parse_metadata_term(Tokens) of
        {ok, Term} -> {next, between, [Term | Acc]};
        {error, _} = Err -> Err
    end;
advance_metadata_state({accumulate, _, _}, _Acc, _Fields, {dot, _}) ->
    {error, {metadata, invalid_terms}};
advance_metadata_state({accumulate, Prefix, Depth}, Acc, _Fields, {Open, _} = T) when
    Open =:= '{'; Open =:= '['
->
    {next, {accumulate, [T | Prefix], Depth + 1}, Acc};
advance_metadata_state({accumulate, Prefix, Depth}, Acc, _Fields, {Close, _} = T) when
    Close =:= '}'; Close =:= ']'
->
    {next, {accumulate, [T | Prefix], Depth - 1}, Acc};
advance_metadata_state({accumulate, Prefix, Depth}, Acc, _Fields, T) ->
    {next, {accumulate, [T | Prefix], Depth}, Acc};
advance_metadata_state({skip, 0}, Acc, _Fields, {dot, _}) ->
    {next, between, Acc};
advance_metadata_state({skip, _}, _Acc, _Fields, {dot, _}) ->
    {error, {metadata, invalid_terms}};
advance_metadata_state({skip, Depth}, Acc, _Fields, {Open, _}) when
    Open =:= '{'; Open =:= '['
->
    {next, {skip, Depth + 1}, Acc};
advance_metadata_state({skip, Depth}, Acc, _Fields, {Close, _}) when
    Close =:= '}'; Close =:= ']'
->
    {next, {skip, Depth - 1}, Acc};
advance_metadata_state({skip, Depth}, Acc, _Fields, _Token) ->
    {next, {skip, Depth}, Acc};
advance_metadata_state(_State, _Acc, _Fields, _Token) ->
    {error, {metadata, not_key_value}}.

%% @private
extract_metadata_key(KeyChars) ->
    try list_to_binary(KeyChars) of
        Key -> {ok, Key}
    catch
        error:badarg -> error
    end.

%% @private
normalize_metadata(Metadata1) ->
    Metadata2 = maybe_update_with(<<"requirements">>, fun normalize_requirements/1, Metadata1),
    Metadata3 = maybe_update_with(<<"links">>, fun try_into_map/1, Metadata2),
    Metadata4 = maybe_update_with(<<"extra">>, fun try_into_nested_map/1, Metadata3),
    guess_build_tools(Metadata4).

%% @private
normalize_requirements(Requirements) ->
    case is_list(Requirements) andalso (Requirements /= []) andalso is_list(hd(Requirements)) of
        true ->
            maps:from_list(lists:map(fun normalize_legacy_requirement/1, Requirements));
        false ->
            try_into_map(fun normalize_normal_requirement/1, Requirements)
    end.

%% @private
normalize_normal_requirement({Name, Requirement}) ->
    {Name, try_into_map(Requirement)}.

%% @private
normalize_legacy_requirement(Requirement) ->
    Map = maps:from_list(Requirement),
    Name = maps:get(<<"name">>, Map),
    {Name, maps:without([<<"name">>], Map)}.

%% @private
guess_build_tools(#{<<"build_tools">> := BuildTools} = Metadata) when is_list(BuildTools) ->
    Metadata;
guess_build_tools(#{<<"files">> := Filenames} = Metadata) ->
    BaseFiles = [
        Filename
     || Filename <- Filenames, filename:dirname(binary_to_list(Filename)) == "."
    ],
    BuildTools = lists:usort([
        Tool
     || {Filename, Tool} <- ?BUILD_TOOL_FILES, lists:member(Filename, BaseFiles)
    ]),
    Metadata#{<<"build_tools">> => BuildTools};
guess_build_tools(Metadata) ->
    Metadata.

%%====================================================================
%% Tar Helpers
%%====================================================================

%% @private
validate_create_files(Files, FilesRoot) when is_list(Files) ->
    validate_create_files(Files, FilesRoot, []).

validate_create_files([], _FilesRoot, Acc) ->
    {ok, lists:reverse(Acc)};
validate_create_files([File | Rest], FilesRoot, Acc) ->
    case validate_create_file(File, FilesRoot) of
        {ok, ValidatedFile} -> validate_create_files(Rest, FilesRoot, [ValidatedFile | Acc]);
        {error, _} = Error -> Error
    end.

validate_create_file({Filename, Contents}, _FilesRoot) when
    is_list(Filename), is_binary(Contents)
->
    case validate_archive_path(Filename) of
        ok -> {ok, {Filename, Contents}};
        {error, _} = Error -> Error
    end;
validate_create_file(Filename, FilesRoot) when is_list(Filename) ->
    validate_create_file({Filename, Filename}, FilesRoot);
validate_create_file({Filename, AbsFilename}, FilesRoot) when
    is_list(Filename), is_list(AbsFilename)
->
    case validate_archive_path(Filename) of
        ok -> validate_source_file(Filename, AbsFilename, FilesRoot);
        {error, _} = Error -> Error
    end.

validate_archive_path(Filename) ->
    case safe_relative_archive_path(Filename) of
        false -> {error, {tarball, {unsafe_path, Filename}}};
        true -> ok
    end.

validate_source_file(ArchiveName, SourcePath, FilesRoot) ->
    case source_file_paths(SourcePath, FilesRoot) of
        {ok, DiskPath, RelativePath, Root} ->
            validate_source_file_root(ArchiveName, DiskPath, RelativePath, Root);
        outside_root ->
            {error, {tarball, {unsafe_path, ArchiveName}}}
    end.

source_file_paths(SourcePath, FilesRoot) ->
    Root = normalize_root(filename:absname(FilesRoot)),
    case source_relative_path(SourcePath, Root) of
        {ok, RelativePath} ->
            {ok, source_disk_path(SourcePath, Root), RelativePath, Root};
        outside_root ->
            outside_root
    end.

normalize_root(Path) ->
    filename:join(normalize_root_parts(filename:split(Path), [])).

normalize_root_parts([], Acc) ->
    lists:reverse(Acc);
normalize_root_parts(["." | Parts], Acc) ->
    normalize_root_parts(Parts, Acc);
normalize_root_parts([".." | Parts], Acc) ->
    normalize_root_parent(Parts, Acc);
normalize_root_parts([Part | Parts], Acc) ->
    normalize_root_parts(Parts, [Part | Acc]).

normalize_root_parent(Parts, [Root] = Acc) ->
    case filename:pathtype(Root) of
        relative -> normalize_root_parts(Parts, []);
        _ -> normalize_root_parts(Parts, Acc)
    end;
normalize_root_parent(Parts, [_Part | Acc]) ->
    normalize_root_parts(Parts, Acc);
normalize_root_parent(Parts, []) ->
    normalize_root_parts(Parts, []).

source_disk_path(SourcePath, Root) ->
    case filename:pathtype(SourcePath) of
        absolute -> SourcePath;
        _ -> filename:join(Root, SourcePath)
    end.

source_relative_path(SourcePath, Root) ->
    case filename:pathtype(SourcePath) of
        absolute -> strip_root_path(filename:split(SourcePath), filename:split(Root));
        _ -> {ok, SourcePath}
    end.

strip_root_path([], []) ->
    {ok, "."};
strip_root_path(PathParts, []) ->
    {ok, filename:join(PathParts)};
strip_root_path([Part | PathParts], [Part | RootParts]) ->
    strip_root_path(PathParts, RootParts);
strip_root_path(_PathParts, _RootParts) ->
    outside_root.

validate_source_file_root(ArchiveName, DiskPath, RelativePath, Root) ->
    case file:read_link_info(DiskPath, []) of
        {ok, #file_info{type = Type}} when Type =:= regular; Type =:= directory ->
            case validate_source_root(ArchiveName, RelativePath, Root) of
                ok -> {ok, {ArchiveName, DiskPath}};
                {error, _} = Error -> Error
            end;
        {ok, #file_info{type = symlink}} ->
            {ok, LinkTarget} = file:read_link(DiskPath),
            ResolvedTarget = archive_join(archive_dirname(ArchiveName), LinkTarget),
            case safe_relative_archive_path(ResolvedTarget) of
                false ->
                    {error, {tarball, {unsafe_symlink, ArchiveName, LinkTarget}}};
                true ->
                    case validate_source_root(ArchiveName, RelativePath, Root) of
                        ok -> {ok, {ArchiveName, DiskPath}};
                        {error, _} = Error -> Error
                    end
            end;
        {ok, #file_info{type = Type}} ->
            {error, {tarball, {unsupported_file_type, ArchiveName, Type}}};
        _ ->
            case validate_source_root(ArchiveName, RelativePath, Root) of
                ok -> {ok, {ArchiveName, DiskPath}};
                {error, _} = Error -> Error
            end
    end.

validate_source_root(ArchiveName, SourcePath, FilesRoot) ->
    case filelib:safe_relative_path(SourcePath, FilesRoot) of
        unsafe -> {error, {tarball, {unsafe_path, ArchiveName}}};
        _ -> ok
    end.

safe_relative_archive_path(Path) ->
    case archive_path_absolute(Path) orelse archive_path_drive(Path) of
        true -> false;
        false -> safe_relative_archive_path(archive_path_split(Path), [])
    end.

safe_relative_archive_path([], _Acc) ->
    true;
safe_relative_archive_path(["." | Rest], Acc) ->
    safe_relative_archive_path(Rest, Acc);
safe_relative_archive_path([".." | _Rest], []) ->
    false;
safe_relative_archive_path([".." | Rest], [_ | Acc]) ->
    safe_relative_archive_path(Rest, Acc);
safe_relative_archive_path([_Part | Rest], Acc) ->
    safe_relative_archive_path(Rest, [ok | Acc]).

archive_path_absolute([$/ | _Rest]) ->
    true;
archive_path_absolute([$\\ | _Rest]) ->
    true;
archive_path_absolute(_Path) ->
    false.

archive_path_drive([Drive, $: | _Rest]) when
    Drive >= $a, Drive =< $z;
    Drive >= $A, Drive =< $Z
->
    true;
archive_path_drive(_Path) ->
    false.

archive_path_split(Path) ->
    string:tokens(Path, "/\\").

archive_dirname(Path) ->
    case archive_path_split(Path) of
        [] -> ".";
        [_Name] -> ".";
        Parts -> string:join(lists:droplast(Parts), "/")
    end.

archive_join(_Dir, Path) when Path =:= [] ->
    Path;
archive_join(_Dir, Path = [$/ | _Rest]) ->
    Path;
archive_join(_Dir, Path = [$\\ | _Rest]) ->
    Path;
archive_join(_Dir, Path = [Drive, $: | _Rest]) when
    Drive >= $a, Drive =< $z;
    Drive >= $A, Drive =< $Z
->
    Path;
archive_join(".", Path) ->
    Path;
archive_join(Dir, Path) ->
    Dir ++ "/" ++ Path.

%% @private
unpack_tarball(Source, memory, MaxSize) ->
    case r3_hex_erl_tar:extract(Source, [memory, compressed, {max_size, MaxSize}]) of
        {error, too_big} ->
            {error, {tarball, {too_big_uncompressed, MaxSize}}};
        Other ->
            Other
    end;
unpack_tarball(Source, Output, MaxSize) ->
    filelib:ensure_dir(filename:join(Output, "*")),
    case r3_hex_erl_tar:extract(Source, [{cwd, Output}, compressed, {max_size, MaxSize}]) of
        ok ->
            [
                try_updating_mtime(filename:join(Output, Path))
             || Path <- filelib:wildcard("**", Output)
            ],
            ok;
        {error, too_big} ->
            {error, {tarball, {too_big_uncompressed, MaxSize}}};
        Other ->
            Other
    end.

%% @private
%% let it silently fail for bad symlinks
try_updating_mtime(Path) ->
    Time = calendar:universal_time(),
    _ = file:write_file_info(Path, #file_info{mtime = Time}, [{time, universal}]),
    ok.

%% @private
create_memory_tarball(Files) ->
    Path = tmp_path(),
    {ok, Tar} = r3_hex_erl_tar:open(Path, [write]),

    try
        add_files(Tar, Files)
    after
        ok = r3_hex_erl_tar:close(Tar)
    end,
    {ok, Tarball} = file:read_file(Path),
    ok = file:delete(Path),
    Tarball.

%% @private
tmp_path() ->
    "tmp_" ++ binary_to_list(encode_base16(crypto:strong_rand_bytes(32))).

%% @private
add_files(Tar, Files) when is_list(Files) ->
    lists:map(fun(File) -> add_file(Tar, File) end, Files).

%% @private
add_file(Tar, {Filename, Contents}) when is_list(Filename) and is_binary(Contents) ->
    ok = r3_hex_erl_tar:add(Tar, Contents, Filename, tar_opts());
add_file(Tar, Filename) when is_list(Filename) ->
    add_file(Tar, {Filename, Filename});
add_file(Tar, {Filename, AbsFilename}) when is_list(Filename), is_list(AbsFilename) ->
    {ok, FileInfo} = file:read_link_info(AbsFilename, []),

    case FileInfo#file_info.type of
        symlink ->
            ok = r3_hex_erl_tar:add(Tar, {Filename, AbsFilename}, tar_opts());
        directory ->
            case file:list_dir(AbsFilename) of
                {ok, []} ->
                    r3_hex_erl_tar:add(Tar, {Filename, AbsFilename}, tar_opts());
                {ok, _} ->
                    ok
            end;
        _ ->
            Mode = FileInfo#file_info.mode,
            {ok, Contents} = file:read_file(AbsFilename),
            ok = r3_hex_erl_tar:add(Tar, Contents, Filename, [{mode, Mode} | tar_opts()])
    end.

%% @private
tar_opts() ->
    NixEpoch = calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {0, 0, 0}}),
    Y2kEpoch = calendar:datetime_to_gregorian_seconds({{2000, 1, 1}, {0, 0, 0}}),
    Epoch = Y2kEpoch - NixEpoch,
    [{atime, Epoch}, {mtime, Epoch}, {ctime, Epoch}, {uid, 0}, {gid, 0}].

%% @private
%% Reproducible gzip by not setting mtime and OS
%%
%% From https://tools.ietf.org/html/rfc1952
%%
%% +---+---+---+---+---+---+---+---+---+---+
%% |ID1|ID2|CM |FLG|     MTIME     |XFL|OS | (more-->)
%% +---+---+---+---+---+---+---+---+---+---+
%%
%% +=======================+
%% |...compressed blocks...| (more-->)
%% +=======================+
%%
%% +---+---+---+---+---+---+---+---+
%% |     CRC32     |     ISIZE     |
%% +---+---+---+---+---+---+---+---+
gzip(Uncompressed) ->
    Compressed = gzip_no_header(Uncompressed),
    Header = <<31, 139, 8, 0, 0, 0, 0, 0, 0, 0>>,
    Crc = erlang:crc32(Uncompressed),
    Size = byte_size(Uncompressed),
    Trailer = <<Crc:32/little, Size:32/little>>,
    iolist_to_binary([Header, Compressed, Trailer]).

%% @private
gzip_no_header(Uncompressed) ->
    Zstream = zlib:open(),

    try
        zlib:deflateInit(Zstream, default, deflated, -15, 8, default),
        Compressed = zlib:deflate(Zstream, Uncompressed, finish),
        zlib:deflateEnd(Zstream),
        iolist_to_binary(Compressed)
    after
        zlib:close(Zstream)
    end.

%%====================================================================
%% Helpers
%%====================================================================

%% @private
valid_size(Binary, infinity) when is_binary(Binary) ->
    true;
valid_size(Binary, Limit) when is_binary(Binary) and is_integer(Limit) ->
    byte_size(Binary) =< Limit.

%% @private
valid_file_size(_Path, infinity) ->
    true;
valid_file_size(Path, Limit) when is_integer(Limit) ->
    case file:read_file_info(Path) of
        {ok, #file_info{size = Size}} -> Size =< Limit;
        {error, _} -> false
    end.

%% @private
file_checksum(Path) ->
    {ok, Fd} = file:open(Path, [read, raw, binary]),
    try
        file_checksum_loop(Fd, crypto:hash_init(sha256))
    after
        file:close(Fd)
    end.

%% @private
file_checksum_loop(Fd, HashState) ->
    case file:read(Fd, ?HASH_CHUNK_SIZE) of
        {ok, Data} -> file_checksum_loop(Fd, crypto:hash_update(HashState, Data));
        eof -> crypto:hash_final(HashState)
    end.

%% @private
stream_file_hash(HashState, Path) ->
    {ok, Fd} = file:open(Path, [read, raw, binary]),
    try
        stream_file_hash_loop(Fd, HashState)
    after
        file:close(Fd)
    end.

%% @private
stream_file_hash_loop(Fd, HashState) ->
    case file:read(Fd, ?HASH_CHUNK_SIZE) of
        {ok, Data} -> stream_file_hash_loop(Fd, crypto:hash_update(HashState, Data));
        eof -> HashState
    end.

%% @private
%% Reads outer tar files from a directory after extraction.
%% Small files (VERSION, CHECKSUM, metadata.config) are read into memory.
%% contents.tar.gz is referenced by path.
read_outer_files(Dir) ->
    RequiredFiles = ["VERSION", "CHECKSUM", "metadata.config", "contents.tar.gz"],
    case read_outer_files(Dir, RequiredFiles, #{}) of
        {ok, Files} ->
            validate_outer_file_sizes(Files);
        {error, _} = Error ->
            Error
    end.

read_outer_files(_Dir, [], Acc) ->
    {ok, Acc};
read_outer_files(Dir, ["contents.tar.gz" | Rest], Acc) ->
    Path = filename:join(Dir, "contents.tar.gz"),
    case filelib:is_regular(Path) of
        true ->
            read_outer_files(Dir, Rest, Acc#{"contents.tar.gz" => {path, Path}});
        false ->
            {error, {tarball, {missing_files, ["contents.tar.gz"]}}}
    end;
read_outer_files(Dir, [Name | Rest], Acc) ->
    Path = filename:join(Dir, Name),
    case file:read_file(Path) of
        {ok, Data} ->
            read_outer_files(Dir, Rest, Acc#{Name => Data});
        {error, _} ->
            {error, {tarball, {missing_files, [Name]}}}
    end.

%% @private
validate_outer_file_sizes(Files) ->
    case byte_size(maps:get("VERSION", Files, <<>>)) > ?MAX_VERSION_SIZE of
        true ->
            {error, {tarball, {file_too_big, "VERSION"}}};
        false ->
            case byte_size(maps:get("CHECKSUM", Files, <<>>)) > ?MAX_CHECKSUM_SIZE of
                true ->
                    {error, {tarball, {file_too_big, "CHECKSUM"}}};
                false ->
                    case byte_size(maps:get("metadata.config", Files, <<>>)) > ?MAX_METADATA_SIZE of
                        true -> {error, {tarball, {file_too_big, "metadata.config"}}};
                        false -> {ok, Files}
                    end
            end
    end.

%% @private
remove_dir(Dir) ->
    case file:list_dir(Dir) of
        {ok, Entries} ->
            lists:foreach(
                fun(Entry) ->
                    Path = filename:join(Dir, Entry),
                    case filelib:is_dir(Path) of
                        true -> remove_dir(Path);
                        false -> file:delete(Path)
                    end
                end,
                Entries
            ),
            file:del_dir(Dir);
        {error, _} ->
            ok
    end.

%% @private
binarify(Binary) when is_binary(Binary) -> Binary;
binarify(Number) when is_number(Number) -> Number;
binarify(Atom) when Atom == undefined orelse is_boolean(Atom) -> Atom;
binarify(Atom) when is_atom(Atom) -> atom_to_binary(Atom, utf8);
binarify(List) when is_list(List) ->
    [binarify(E) || E <- List];
binarify({Key, Value}) ->
    {binarify(Key), binarify(Value)};
binarify(Map) when is_map(Map) ->
    List = maps:to_list(Map),
    lists:map(fun({K, V}) -> binarify({K, V}) end, List).

%% @private
diff_keys(Map, RequiredKeys, OptionalKeys) ->
    Keys = maps:keys(Map),
    MissingKeys = RequiredKeys -- Keys,
    UnknownKeys = Keys -- (RequiredKeys ++ OptionalKeys),

    case {MissingKeys, UnknownKeys} of
        {[], []} ->
            ok;
        % Server should validate this but clients should not
        % {_, [_ | _]} ->
        %     {error, {unknown_keys, UnknownKeys}};

        _ ->
            {error, {missing_keys, MissingKeys}}
    end.

%% @private
maybe_update_with(Key, Fun, Map) ->
    case maps:find(Key, Map) of
        {ok, Value} -> maps:put(Key, Fun(Value), Map);
        error -> Map
    end.

%% @private
try_into_map(List) ->
    try_into_map(fun(X) -> X end, List).

%% @private
try_into_map(Fun, Input) ->
    case has_map_shape(Input) of
        true -> maps:from_list(lists:map(Fun, Input));
        false -> Input
    end.

%% @private
try_into_nested_map(List) ->
    try_into_nested_map(fun(X) -> X end, List).

%% @private
try_into_nested_map(Fun, Input) ->
    case has_map_shape(Input) of
        true ->
            maps:from_list(
                lists:map(
                    fun({Key, Value}) ->
                        Fun({Key, try_into_nested_map(Fun, Value)})
                    end,
                    Input
                )
            );
        false ->
            Input
    end.

%% @private
has_map_shape(Input) ->
    is_list(Input) andalso
        lists:all(fun(E) -> is_tuple(E) andalso (tuple_size(E) == 2) end, Input).

%% @private
encode_base16(Binary) ->
    <<X:256/big-unsigned-integer>> = Binary,
    String = string:to_upper(lists:flatten(io_lib:format("~64.16.0b", [X]))),
    list_to_binary(String).

%% Based on https://github.com/goj/base16/blob/master/src/base16.erl
%% (C) 2012, Erlang Solutions Ltd.

%% @private
decode_base16(Base16) ->
    <<<<(unhex(H) bsl 4 + unhex(L))>> || <<H, L>> <= Base16>>.

%% @private
unhex(D) when $0 =< D andalso D =< $9 ->
    D - $0;
unhex(D) when $a =< D andalso D =< $f ->
    10 + D - $a;
unhex(D) when $A =< D andalso D =< $F ->
    10 + D - $A.
