-module(cth_readable_transform).
-export([parse_transform/2]).

parse_transform(ASTs, _Options) ->
    try
        [erl_syntax_lib:map(fun(T) ->
             transform(erl_syntax:revert(T))
         end, AST) || AST <- ASTs]
    catch
        _:_ ->
            ASTs
    end.

transform({call, Line, {remote, _, {atom, _, ct}, {atom, _, pal}}, Args}) ->
    {call, Line, {remote, Line, {atom, Line, cthr}, {atom, Line, pal}}, Args};
transform(Term) ->
    Term.
