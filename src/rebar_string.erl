%%% @doc Compatibility module for string functionality
%%% for pre- and post-unicode support.
%%%
%%% Also contains other useful string functionality.
-module(rebar_string).
%% Compatibility exports
-export([join/2, split/2, lexemes/2, trim/1, trim/3, uppercase/1, lowercase/1, chr/2]).
%% Util exports
-export([consult/1]).

%% string:join/2 copy; string:join/2 is getting obsoleted
%% and replaced by lists:join/2, but lists:join/2 is too new
%% for version support (only appeared in 19.0) so it cannot be
%% used. Instead we just adopt join/2 locally and hope it works
%% for most unicode use cases anyway.
join([], Sep) when is_list(Sep) ->
    [];
join([H|T], Sep) ->
    H ++ lists:append([Sep ++ X || X <- T]).

split(Str, SearchPattern) -> string:split(Str, SearchPattern).
lexemes(Str, SepList) -> string:lexemes(Str, SepList).
trim(Str) -> string:trim(Str).
trim(Str, Direction, Cluster=[_]) -> string:trim(Str, Direction, Cluster).
uppercase(Str) -> string:uppercase(Str).
lowercase(Str) -> string:lowercase(Str).

chr(S, C) when is_integer(C) -> chr(S, C, 1).
chr([C|_Cs], C, I) -> I;
chr([_|Cs], C, I) -> chr(Cs, C, I+1);
chr([], _C, _I) -> 0.

%% @doc
%% Given a string or binary, parse it into a list of terms, ala file:consult/1
-spec consult(unicode:chardata()) -> {error, term()} | [term()].
consult(Str) ->
    consult([], unicode:characters_to_list(Str), []).

consult(Cont, Str, Acc) ->
    case erl_scan:tokens(Cont, Str, 0) of
        {done, Result, Remaining} ->
            case Result of
                {ok, Tokens, _} ->
                    case erl_parse:parse_term(Tokens) of
                        {ok, Term} -> consult([], Remaining, [Term | Acc]);
                        {error, Reason} -> {error, Reason}
                    end;
                {eof, _Other} ->
                    lists:reverse(Acc);
                {error, Info, _} ->
                    {error, Info}
            end;
        {more, Cont1} ->
            consult(Cont1, eof, Acc)
    end.
