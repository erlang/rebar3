%%% @doc Module handling rich formatting of errors.
-module(rebar_compiler_format).
-export([format/5]).

-include("rebar.hrl").

-spec format(file:filename_all(), {Line, Column}, Extra, Desc, rebar_dict()) ->
    string() when
        Extra :: iodata(),
        Line :: non_neg_integer(),
        Column :: non_neg_integer(),
        Desc :: iodata().
format(Source, {Line, Column}, Extra, Desc, Config) ->
    CompilerErrFmt = compiler_error_format(Config),
    case CompilerErrFmt == rich andalso find_line(Line, Source) of
        {ok, LnBin} ->
            LnPad = lists:duplicate(length(integer_to_list(Line)), " "),
            Arrow = cf:format("~!R~ts~!!",["╰──"]),
            ?FMT(" ~ts ┌─ ~ts:~n"
                 " ~ts │~n"
                  " ~w │  ~ts~n"
                 " ~ts │  ~s~ts ~ts~ts~n~n",
                 [LnPad, Source,
                  LnPad,
                  Line, colorize(LnBin, Column),
                  LnPad, indent(max(0, Column-1), LnBin), Arrow, Extra, Desc]);
        _ ->
            ?FMT("~ts:~w:~w: ~ts~ts~n", [Source, Line, Column, Extra, Desc])
    end.

find_line(Nth, Source) ->
    try do_find_line(Nth, Source)
    catch
        error:X -> {error, X}
    end.

do_find_line(Nth, Source) ->
    case file:read_file(Source) of
        {ok, <<>>} ->
            {error, empty_file};
        {ok, Bin} ->
            Splits = re:split(Bin, "(?:\n|\r\n|\r)", [{newline, anycrlf}]),
            {ok, lists:nth(Nth, Splits)};
        {error, Reason} ->
            {error, Reason}
    end.

indent(0, _) -> "";
indent(N, <<"\t", Rest/binary>>) -> [$\t | indent(N-1, Rest)];
indent(N, <<_/utf8, Rest/binary>>) -> [$\s | indent(N-1, Rest)].

compiler_error_format(Opts) ->
    %% `Opts' can be passed in both as a list or a dictionary depending
    %% on whether the first call to rebar_erlc_compiler was done with
    %% the type `rebar_dict()' or `rebar_state:t()'.
    LookupFn = if is_list(Opts) -> fun(K,L) -> lists:keyfind(K, 1, L) end
                ; true          -> fun(K,O) -> rebar_opts:get(O, K, false) end
               end,
    case LookupFn(compiler_error_format, Opts) of
        false -> ?DEFAULT_COMPILER_ERROR_FORMAT;
        {ok, minimal} -> minimal;
        {ok, rich} -> rich;
        minimal -> minimal;
        rich -> rich
    end.

%% @private try to colorize data based on common ways to end terminators
%% in Erlang-like languages. Any character that isn't one of the following
%% is considered to end a "word" of some type:
%%
%% - letters
%% - numbers
%% - underscore
%% - quotations
%%
%% This will have false positives in some cases and if that becomes annoying
%% we'll need to allow per-compiler module configurations here, but it should
%% generally lead to proper colorization.
colorize(Str, Col) ->
    Pre = string:slice(Str, 0, max(0,Col-1)),
    At = string:slice(Str, max(0,Col-1)),
    case [B || B <- re:split(At, "([^[A-Za-z0-9_#\"]+)", []),
                         B =/= <<>>] of
      [Bad |Tail] ->
        cf:format("~ts~!R~ts~!!~ts", [Pre,Bad,Tail]);
     [] ->
        cf:format("~ts~n", [Str])
   end.
