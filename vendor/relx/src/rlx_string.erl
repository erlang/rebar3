%% Compatibility module for the string API changes between
%% OTP-19 and OTP-21, where Unicode support means the deprecation
%% of a lot of string functions.
-module(rlx_string).
-export([concat/2, lexemes/2, join/2, trim/3, to_list/1]).

-ifdef(unicode_str).
concat(Str1, Str2) -> unicode:characters_to_list([Str1,Str2]).
lexemes(Str, Separators) -> string:lexemes(Str, Separators).
trim(Str, Direction, Cluster=[_]) -> string:trim(Str, Direction, Cluster).
-else.
concat(Str1, Str2) -> string:concat(Str1, Str2).
lexemes(Str, Separators) -> string:tokens(Str, Separators).
trim(Str, Direction, [Char]) ->
    Dir = case Direction of
              both -> both;
              leading -> left;
              trailing -> right
          end,
    string:strip(Str, Dir, Char).
-endif.

%% string:join/2 copy; string:join/2 is getting obsoleted
%% and replaced by lists:join/2, but lists:join/2 is too new
%% for version support (only appeared in 19.0) so it cannot be
%% used. Instead we just adopt join/2 locally and hope it works
%% for most unicode use cases anyway.
join([], Sep) when is_list(Sep) ->
    [];
join([H|T], Sep) ->
    H ++ lists:append([Sep ++ X || X <- T]).

-spec to_list(binary() | list()) -> list().
to_list(String) when is_binary(String) ->
    binary_to_list(String);
to_list(String) when is_list(String) ->
    String.
