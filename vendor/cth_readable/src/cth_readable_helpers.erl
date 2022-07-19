-module(cth_readable_helpers).
-export([format_path/2, colorize/2, maybe_eunit_format/1]).

format_path(TC, Groups) ->
    join([atom_to_list(P) || P <- lists:reverse([TC|Groups])], ".").

%% string:join/2 copy; string:join/2 is getting obsoleted
%% and replaced by lists:join/2, but lists:join/2 is too new
%% for version support (only appeared in 19.0) so it cannot be
%% used. Instead we just adopt join/2 locally and hope it works
%% for most unicode use cases anyway.
join([], Sep) when is_list(Sep) ->
    [];
join([H|T], Sep) ->
    H ++ lists:append([Sep ++ X || X <- T]).

colorize(red, Txt) -> cf:format("~!r~s~!!", [Txt]);
colorize(green, Txt) -> cf:format("~!g~s~!!", [Txt]);
colorize(magenta, Txt) -> cf:format("~!m~s~!!",[Txt]).

maybe_eunit_format({failed, Reason}) ->
    maybe_eunit_format(Reason);

maybe_eunit_format({{Type, Props}, _}) when Type =:= assert_failed
                                            ; Type =:= assert ->
    Keys = proplists:get_keys(Props),
    HasEUnitProps = ([expression, value, line] -- Keys) =:= [],
    HasHamcrestProps = ([expected, actual, matcher, line] -- Keys) =:= [],
    if
        HasEUnitProps ->
            [io_lib:format("~nFailure/Error: ?assert(~s)~n", [proplists:get_value(expression, Props)]),
             io_lib:format("  expected: ~p~n", [proplists:get_value(expected, Props)]),
             case proplists:get_value(value, Props) of
                 {not_a_boolean, V} ->
                     io_lib:format("       got: ~p~n", [V]);
                 V ->
                     io_lib:format("       got: ~p~n", [V])
             end, io_lib:format("      line: ~p", [proplists:get_value(line, Props)])] ++
             [io_lib:format("~n   comment: ~p", [Comment]) || {comment, Comment} <- [proplists:lookup(comment, Props)]];
        HasHamcrestProps ->
            [io_lib:format("~nFailure/Error: ?assertThat(~p)~n", [proplists:get_value(matcher, Props)]),
             io_lib:format("  expected: ~p~n", [proplists:get_value(expected, Props)]),
             io_lib:format("       got: ~p~n", [proplists:get_value(actual, Props)]),
             io_lib:format("      line: ~p", [proplists:get_value(line, Props)])];
        true ->
            [io_lib:format("~nFailure/Error: unknown assert: ~p", [Props])]
    end;

maybe_eunit_format({{Type, Props}, _}) when Type =:= assertMatch_failed
                                          ; Type =:= assertMatch ->
    Expr = proplists:get_value(expression, Props),
    Pattern = proplists:get_value(pattern, Props),
    Value = proplists:get_value(value, Props),
    [io_lib:format("~nFailure/Error: ?assertMatch(~s, ~s)~n", [Pattern, Expr]),
     io_lib:format("  expected: = ~s~n", [Pattern]),
     io_lib:format("       got: ~p~n", [Value]),
     io_lib:format("      line: ~p", [proplists:get_value(line, Props)])] ++
    [io_lib:format("~n   comment: ~p", [Comment]) || {comment, Comment} <- [proplists:lookup(comment, Props)]];

maybe_eunit_format({{Type, Props}, _}) when Type =:= assertNotMatch_failed
                                            ; Type =:= assertNotMatch  ->
    Expr = proplists:get_value(expression, Props),
    Pattern = proplists:get_value(pattern, Props),
    Value = proplists:get_value(value, Props),
    [io_lib:format("~nFailure/Error: ?assertNotMatch(~s, ~s)~n", [Pattern, Expr]),
     io_lib:format("  expected not: = ~s~n", [Pattern]),
     io_lib:format("           got:   ~p~n", [Value]),
     io_lib:format("          line: ~p", [proplists:get_value(line, Props)])] ++
    [io_lib:format("~n   comment: ~p", [Comment]) || {comment, Comment} <- [proplists:lookup(comment, Props)]];

maybe_eunit_format({{Type, Props}, _}) when Type =:= assertEqual_failed
                                            ; Type =:= assertEqual  ->
    Expr = proplists:get_value(expression, Props),
    Expected = proplists:get_value(expected, Props),
    Value = proplists:get_value(value, Props),
    [io_lib:format("~nFailure/Error: ?assertEqual(~w, ~s)~n", [Expected,
                                                             Expr]),
     io_lib:format("  expected: ~p~n", [Expected]),
     io_lib:format("       got: ~p~n", [Value]),
     io_lib:format("      line: ~p", [proplists:get_value(line, Props)])] ++
    [io_lib:format("~n   comment: ~p", [Comment]) || {comment, Comment} <- [proplists:lookup(comment, Props)]];

maybe_eunit_format({{Type, Props}, _}) when Type =:= assertNotEqual_failed
                                            ; Type =:= assertNotEqual ->
    Expr = proplists:get_value(expression, Props),
    Value = proplists:get_value(value, Props),
    [io_lib:format("~nFailure/Error: ?assertNotEqual(~p, ~s)~n",
                   [Value, Expr]),
     io_lib:format("  expected not: == ~p~n", [Value]),
     io_lib:format("           got:    ~p~n", [Value]),
     io_lib:format("          line: ~p", [proplists:get_value(line, Props)])] ++
    [io_lib:format("~n   comment: ~p", [Comment]) || {comment, Comment} <- [proplists:lookup(comment, Props)]];

maybe_eunit_format({{Type, Props}, _}) when Type =:= assertException_failed
                                            ; Type =:= assertException ->
    Expr = proplists:get_value(expression, Props),
    Pattern = proplists:get_value(pattern, Props),
    {Class, Term} = extract_exception_pattern(Pattern), % I hate that we have to do this, why not just give DATA
    [io_lib:format("~nFailure/Error: ?assertException(~s, ~s, ~s)~n", [Class, Term, Expr]),
     case proplists:is_defined(unexpected_success, Props) of
         true ->
             [io_lib:format("  expected: exception ~s but nothing was raised~n", [Pattern]),
              io_lib:format("       got: value ~p~n", [proplists:get_value(unexpected_success, Props)]),
              io_lib:format("      line: ~p", [proplists:get_value(line, Props)])];
         false ->
             Ex = proplists:get_value(unexpected_exception, Props),
             [io_lib:format("  expected: exception ~s~n", [Pattern]),
              io_lib:format("       got: exception ~p~n", [Ex]),
              io_lib:format("      line: ~p", [proplists:get_value(line, Props)])]
     end] ++
    [io_lib:format("~n   comment: ~p", [Comment]) || {comment, Comment} <- [proplists:lookup(comment, Props)]];

maybe_eunit_format({{Type, Props}, _}) when Type =:= assertNotException_failed
                                            ; Type =:= assertNotException ->
    Expr = proplists:get_value(expression, Props),
    Pattern = proplists:get_value(pattern, Props),
    {Class, Term} = extract_exception_pattern(Pattern), % I hate that we have to do this, why not just give DAT
    Ex = proplists:get_value(unexpected_exception, Props),
    [io_lib:format("~nFailure/Error: ?assertNotException(~s, ~s, ~s)~n", [Class, Term, Expr]),
     io_lib:format("  expected not: exception ~s~n", [Pattern]),
     io_lib:format("           got: exception ~p~n", [Ex]),
     io_lib:format("          line: ~p", [proplists:get_value(line, Props)])] ++
    [io_lib:format("~n   comment: ~p", [Comment]) || {comment, Comment} <- [proplists:lookup(comment, Props)]];

maybe_eunit_format({{Type, Props}, _}) when Type =:= command_failed
                                            ; Type =:= command ->
    Cmd = proplists:get_value(command, Props),
    Expected = proplists:get_value(expected_status, Props),
    Status = proplists:get_value(status, Props),
    [io_lib:format("~nFailure/Error: ?cmdStatus(~p, ~p)~n", [Expected, Cmd]),
     io_lib:format("  expected: status ~p~n", [Expected]),
     io_lib:format("       got: status ~p~n", [Status]),
     io_lib:format("      line: ~p", [proplists:get_value(line, Props)])] ++
    [io_lib:format("~n   comment: ~p", [Comment]) || {comment, Comment} <- [proplists:lookup(comment, Props)]];

maybe_eunit_format({{Type, Props}, _}) when Type =:= assertCmd_failed
                                            ; Type =:= assertCmd ->
    Cmd = proplists:get_value(command, Props),
    Expected = proplists:get_value(expected_status, Props),
    Status = proplists:get_value(status, Props),
    [io_lib:format("~nFailure/Error: ?assertCmdStatus(~p, ~p)~n", [Expected, Cmd]),
     io_lib:format("  expected: status ~p~n", [Expected]),
     io_lib:format("       got: status ~p~n", [Status]),
     io_lib:format("      line: ~p", [proplists:get_value(line, Props)])] ++
    [io_lib:format("~n   comment: ~p", [Comment]) || {comment, Comment} <- [proplists:lookup(comment, Props)]];

maybe_eunit_format({{Type, Props}, _}) when Type =:= assertCmdOutput_failed
                                            ; Type =:= assertCmdOutput ->
    Cmd = proplists:get_value(command, Props),
    Expected = proplists:get_value(expected_output, Props),
    Output = proplists:get_value(output, Props),
    [io_lib:format("~nFailure/Error: ?assertCmdOutput(~p, ~p)~n", [Expected, Cmd]),
     io_lib:format("  expected: ~p~n", [Expected]),
     io_lib:format("       got: ~p~n", [Output]),
     io_lib:format("      line: ~p", [proplists:get_value(line, Props)])] ++
    [io_lib:format("~n   comment: ~p", [Comment]) || {comment, Comment} <- [proplists:lookup(comment, Props)]];

maybe_eunit_format(Reason) ->
    io_lib:format("~p", [Reason]).

extract_exception_pattern(Str) ->
    ["{", Class, Term|_] = re:split(Str, "[, ]{1,2}", [unicode,{return,list}]),
    {Class, Term}.
