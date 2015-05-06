%% @copyright 2015 Hinagiku Soranoba All Rights Reserved.
%%
%% @doc Mustach template engine for Erlang/OTP.
-module(rebar_mustache).

%%----------------------------------------------------------------------------------------------------------------------
%% Exported API
%%----------------------------------------------------------------------------------------------------------------------
-export([
         render/2,
         parse_binary/1,
         parse_file/1,
         compile/2
        ]).

-export_type([
              template/0,
              data/0
             ]).

%%----------------------------------------------------------------------------------------------------------------------
%% Defines & Records & Types
%%----------------------------------------------------------------------------------------------------------------------

-define(PARSE_ERROR,        incorrect_format).
-define(FILE_ERROR,         file_not_found).
-define(COND(Cond, TValue, FValue),
        case Cond of true -> TValue; false -> FValue end).

-type key()  :: binary().
-type tag()  :: {n,   key()}                              |
                {'&', key()}                              |
                {'#', key(), [tag()], Source :: binary()} |
                {'^', key(), [tag()]}                     |
                binary().

-record(state,
        {
          dirname = <<>>     :: file:filename_all(),
          start   = <<"{{">> :: binary(),
          stop    = <<"}}">> :: binary()
        }).
-type state() :: #state{}.

-record(?MODULE,
        {
          data :: [tag()]
        }).

-opaque template() :: #?MODULE{}.
%% @see parse_binary/1
%% @see parse_file/1
-type data()       :: #{string() => data() | iodata() | fun((data(), function()) -> iodata())}.
%% @see render/2
%% @see compile/2
-type partial()    :: {partial, {state(), EndTag :: binary(), LastTagSize :: non_neg_integer(), Rest :: binary(), [tag()]}}.

%%----------------------------------------------------------------------------------------------------------------------
%% Exported Functions
%%----------------------------------------------------------------------------------------------------------------------

%% @equiv compile(parse_binary(Bin), Map)
-spec render(binary(), data()) -> binary().
render(Bin, Map) ->
    compile(parse_binary(Bin), Map).

%% @doc Create a {@link template/0} from a binary.
-spec parse_binary(binary()) -> template().
parse_binary(Bin) when is_binary(Bin) ->
    parse_binary_impl(#state{}, Bin).

%% @doc Create a {@link template/0} from a file.
-spec parse_file(file:filename()) -> template().
parse_file(Filename) ->
    case file:read_file(Filename) of
        {ok, Bin} -> parse_binary_impl(#state{dirname = filename:dirname(Filename)}, Bin);
        _         -> error(?FILE_ERROR, [Filename])
    end.

%% @doc Embed the data in the template.
-spec compile(template(), data()) -> binary().
compile(#?MODULE{data = Tags}, Map) ->
    ec_cnv:to_binary(lists:reverse(compile_impl(Tags, Map, []))).

%%----------------------------------------------------------------------------------------------------------------------
%% Internal Function
%%----------------------------------------------------------------------------------------------------------------------

%% @doc {@link compile/2}
%%
%% ATTENTION: The result is a list that is inverted.
-spec compile_impl(Template :: [tag()], data(), Result :: iodata()) -> iodata().
compile_impl([], _, Result) ->
    Result;
compile_impl([{n, Key} | T], Map, Result) ->
    compile_impl(T, Map, [escape(to_binary(dict_get(binary_to_list(Key), Map, <<>>))) | Result]);
compile_impl([{'&', Key} | T], Map, Result) ->
    compile_impl(T, Map, [to_binary(dict_get(binary_to_list(Key), Map, <<>>)) | Result]);
compile_impl([{'#', Key, Tags, Source} | T], Map, Result) ->
    Value = dict_get(binary_to_list(Key), Map, undefined),
    if
        is_list(Value)                       -> compile_impl(T, Map, lists:foldl(fun(X, Acc) -> compile_impl(Tags, X, Acc) end,
                                                                                 Result, Value));
        Value =:= false; Value =:= undefined -> compile_impl(T, Map, Result);
        is_function(Value, 2)                -> compile_impl(T, Map, [Value(Source, fun(Text) -> render(Text, Map) end) | Result]);
        %is_dict(Value)                        -> compile_impl(T, Map, compile_impl(Tags, Value, Result));
        true                                 -> compile_impl(T, Map, compile_impl(Tags, Map, Result))
    end;
compile_impl([{'^', Key, Tags} | T], Map, Result) ->
    Value = dict_get(binary_to_list(Key), Map, undefined),
    case Value =:= undefined orelse Value =:= [] orelse Value =:= false of
        true  -> compile_impl(T, Map, compile_impl(Tags, Map, Result));
        false -> compile_impl(T, Map, Result)
    end;
compile_impl([Bin | T], Map, Result) ->
    compile_impl(T, Map, [Bin | Result]).

%% @see parse_binary/1
-spec parse_binary_impl(state(), Input :: binary()) -> template().
parse_binary_impl(State, Input) ->
    #?MODULE{data = parse(State, Input)}.

%% @doc Analyze the syntax of the mustache.
-spec parse(state(), binary()) -> [tag()].
parse(State, Bin) ->
    case parse1(State, Bin, []) of
        {partial, _} -> error(?PARSE_ERROR);
        {_, Tags}    -> lists:reverse(Tags)
    end.

%% @doc Part of the `parse/1'
%%
%% ATTENTION: The result is a list that is inverted.
-spec parse1(state(), Input :: binary(), Result :: [tag()]) -> {state(), [tag()]} | partial().
parse1(#state{start = Start, stop = Stop} = State, Bin, Result) ->
    case binary:split(Bin, Start) of
        []                       -> {State, Result};
        [B1]                     -> {State, [B1 | Result]};
        [B1, <<"{", B2/binary>>] -> parse2(State, binary:split(B2, <<"}", Stop/binary>>), [B1 | Result]);
        [B1, B2]                 -> parse3(State, binary:split(B2, Stop),                 [B1 | Result])
    end.

%% @doc Part of the `parse/1'
%%
%% ATTENTION: The result is a list that is inverted.
parse2(State, [B1, B2], Result) ->
    parse1(State, B2, [{'&', remove_space_from_edge(B1)} | Result]);
parse2(_, _, _) ->
    error(?PARSE_ERROR).

%% @doc Part of the `parse/1'
%%
%% ATTENTION: The result is a list that is inverted.
parse3(State, [B1, B2], Result) ->
    case remove_space_from_head(B1) of
        <<"&", Tag/binary>> ->
            parse1(State, B2, [{'&', remove_space_from_edge(Tag)} | Result]);
        <<T, Tag/binary>> when T =:= $#; T =:= $^ ->
            parse_loop(State, ?COND(T =:= $#, '#', '^'), remove_space_from_edge(Tag), B2, Result);
        <<"=", Tag0/binary>> ->
            Tag1 = remove_space_from_tail(Tag0),
            Size = byte_size(Tag1) - 1,
            case Size >= 0 andalso Tag1 of
                <<Tag2:Size/binary, "=">> -> parse_delimiter(State, Tag2, B2, Result);
                _                         -> error(?PARSE_ERROR)
            end;
        <<"!", _/binary>> ->
            parse1(State, B2, Result);
        <<"/", Tag/binary>> ->
            {partial, {State, remove_space_from_edge(Tag), byte_size(B1) + 4, B2, Result}};
        <<">", Tag/binary>> ->
            parse_jump(State, remove_space_from_edge(Tag), B2, Result);
        Tag ->
            parse1(State, B2, [{n, remove_space_from_tail(Tag)} | Result])
    end;
parse3(_, _, _) ->
    error(?PARSE_ERROR).

%% @doc Loop processing part of the `parse/1'
%%
%% `{{# Tag}}' or `{{^ Tag}}' corresponds to this.
-spec parse_loop(state(), '#' | '^', Tag :: binary(), Input :: binary(), Result :: [tag()]) -> [tag()] | partial().
parse_loop(State0, Mark, Tag, Input, Result0) ->
    case parse1(State0, Input, []) of
        {partial, {State, Tag, LastTagSize, Rest, Result1}} when is_list(Result1) ->
            case Mark of
                '#' -> Source = binary:part(Input, 0, byte_size(Input) - byte_size(Rest) - LastTagSize),
                       parse1(State, Rest, [{'#', Tag, lists:reverse(Result1), Source} | Result0]);
                '^' -> parse1(State, Rest, [{'^', Tag, lists:reverse(Result1)} | Result0])
            end;
        _ ->
            error(?PARSE_ERROR)
    end.

%% @doc Partial part of the `parse/1'
-spec parse_jump(state(), Tag :: binary(), NextBin :: binary(), Result :: [tag()]) -> [tag()] | partial().
parse_jump(#state{dirname = Dirname} = State0, Tag, NextBin, Result0) ->
    Filename0 = <<Tag/binary, ".mustache">>,
    Filename  = filename:join(?COND(Dirname =:= <<>>, [Filename0], [Dirname, Filename0])),
    case file:read_file(Filename) of
        {ok, Bin} ->
            case parse1(State0, Bin, Result0) of
                {partial, _}    -> error(?PARSE_ERROR);
                {State, Result} -> parse1(State, NextBin, Result)
            end;
        _ ->
            error(?FILE_ERROR, [Filename])
    end.

%% @doc Update delimiter part of the `parse/1'
%%
%% Parse_BinaryDelimiterBin :: e.g. `{{=%% %%=}}' -> `%% %%'
-spec parse_delimiter(state(), Parse_BinaryDelimiterBin :: binary(), NextBin :: binary(), Result :: [tag()]) -> [tag()] | partial().
parse_delimiter(State0, Parse_BinaryDelimiterBin, NextBin, Result) ->
    case binary:match(Parse_BinaryDelimiterBin, <<"=">>) of
        nomatch ->
            case [X || X <- binary:split(Parse_BinaryDelimiterBin, <<" ">>, [global]), X =/= <<>>] of
                [Start, Stop] -> parse1(State0#state{start = Start, stop = Stop}, NextBin, Result);
                _             -> error(?PARSE_ERROR)
            end;
        _ ->
            error(?PARSE_ERROR)
    end.

%% @doc Remove the space from the edge.
-spec remove_space_from_edge(binary()) -> binary().
remove_space_from_edge(Bin) ->
    remove_space_from_tail(remove_space_from_head(Bin)).

%% @doc Remove the space from the head.
-spec remove_space_from_head(binary()) -> binary().
remove_space_from_head(<<" ", Rest/binary>>) -> remove_space_from_head(Rest);
remove_space_from_head(Bin)                  -> Bin.

%% @doc Remove the space from the tail.
-spec remove_space_from_tail(binary()) -> binary().
remove_space_from_tail(<<>>) -> <<>>;
remove_space_from_tail(Bin) ->
    PosList = binary:matches(Bin, <<" ">>),
    LastPos = remove_space_from_tail_impl(lists:reverse(PosList), byte_size(Bin)),
    binary:part(Bin, 0, LastPos).

%% @see remove_space_from_tail/1
-spec remove_space_from_tail_impl([{non_neg_integer(), pos_integer()}], non_neg_integer()) -> non_neg_integer().
remove_space_from_tail_impl([{X, Y} | T], Size) when Size =:= X + Y ->
    remove_space_from_tail_impl(T, X);
remove_space_from_tail_impl(_, Size) ->
    Size.

%% @doc Number to binary
-spec to_binary(number() | binary() | string()) -> binary() | string().
to_binary(Integer) when is_integer(Integer) ->
    integer_to_binary(Integer);
to_binary(Float) when is_float(Float) ->
    io_lib:format("~p", [Float]);
to_binary(X) ->
    X.

%% @doc HTML Escape
-spec escape(iodata()) -> binary().
escape(IoData) ->
    Bin = ec_cnv:to_binary(IoData),
    << <<(escape_char(X))/binary>> || <<X:8>> <= Bin >>.

%% @see escape/1
-spec escape_char(0..16#FFFF) -> binary().
escape_char($<) -> <<"&lt;">>;
escape_char($>) -> <<"&gt;">>;
escape_char($&) -> <<"&amp;">>;
escape_char($") -> <<"&quot;">>;
escape_char($') -> <<"&apos;">>;
escape_char(C)  -> <<C:8>>.

dict_get(Key, Dict, Default) ->
    case dict:find(ec_cnv:to_atom(Key), Dict) of
        {ok, Value} ->
            Value;
        error ->
            Default
    end.
