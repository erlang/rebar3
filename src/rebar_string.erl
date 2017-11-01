%%% @doc Compatibility module for string functionality
%%% for pre- and post-unicode support.
-module(rebar_string).
-export([join/2, lexemes/2, trim/3, uppercase/1, lowercase/1, chr/2]).

-ifdef(unicode_str).

%% string:join/2 copy; string:join/2 is getting obsoleted
%% and replaced by lists:join/2, but lists:join/2 is too new
%% for version support (only appeared in 19.0) so it cannot be
%% used. Instead we just adopt join/2 locally and hope it works
%% for most unicode use cases anyway.
join([], Sep) when is_list(Sep) ->
        [];
join([H|T], Sep) ->
    H ++ lists:append([Sep ++ X || X <- T]).

lexemes(Str, SepList) -> string:lexemes(Str, SepList).
trim(Str, Direction, Cluster=[_]) -> string:trim(Str, Direction, Cluster).
uppercase(Str) -> string:uppercase(Str).
lowercase(Str) -> string:lowercase(Str).

chr(S, C) when is_integer(C) -> chr(S, C, 1).
chr([C|_Cs], C, I) -> I;
chr([_|Cs], C, I) -> chr(Cs, C, I+1);
chr([], _C, _I) -> 0.
-else.

join(Strings, Separator) -> string:join(Strings, Separator).
lexemes(Str, SepList) -> string:tokens(Str, SepList).
trim(Str, Direction, [Char]) ->
    Dir = case Direction of
              both -> both;
              leading -> left;
              trailing -> right
          end,
    string:strip(Str, Dir, Char).
uppercase(Str) -> string:to_upper(Str).
lowercase(Str) -> string:to_lower(Str).
chr(Str, Char) -> string:chr(Str, Char).
-endif.
