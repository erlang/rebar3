%%%
%%% General colour conventions mostly follow the same schema
%%% that git uses:
%%%
%%% * cyan:  files/lines or locations in general
%%% * bold:  important text i.e. the human parts of warnings
%%%          this allows quickly 'scanning' over a warning fading
%%%          out the cody bits
%%% * red:   things that went bad, i.e. the wrong argument in a
%%%          call. It allows to quickly catching where in the code
%%%          ane error is.
%%% * green: the 'good' stuff, i.e. what was expected as an argument
%%%          the 'red vs green' resambles the diff view 'remove vs add'
%%% * blue:  argument positions.
-module(rebar_dialyzer_format).

-include("rebar.hrl").

-export([format_warnings/1]).

%% Formats a list of warnings in a nice per file way. Note that we reverse
%% the list at the end to 'undo' the reversal by foldl
format_warnings(Warnings) ->
    {_, Res} = lists:foldl(fun format_warning_/2, {undefined, []}, Warnings),
    lists:reverse(Res).


%% If the last seen file is and the file of this warning are the same
%% we skip the file header
format_warning_(Warning = {_Tag, {File, Line}, Msg}, {File, Acc}) ->
    try
        String = message_to_string(Msg),
        {File, [lists:flatten(fmt("~!c~4w~!!: ~s", [Line, String])) | Acc]}
    catch
        Error:Reason ->
            ?DEBUG("Failed to pretty format warning: ~p:~p",
                   [Error, Reason]),
            {File, [dialyzer:format_warning(Warning, fullpath) | Acc]}
    end;

%% With a new file detencted we also write a file header.
format_warning_(Warning = {_Tag, {File, Line}, Msg}, {_LastFile, Acc}) ->
    try
        Base = filename:basename(File),
        Dir = filename:dirname(File),
        Root = filename:rootname(Base),
        Ext = filename:extension(Base),
        Path = re:replace(Dir, "^.*/_build/", "_build/", [{return, list}]),
        Base1 = fmt("~!_c~s~!!~!__~s", [Root, Ext]),
        F = fmt("~!__~s", [filename:join(Path, Base1)]),
        String = message_to_string(Msg),
        {File, [lists:flatten(fmt("~n~s~n~!c~4w~!!: ~s", [F, Line, String])) | Acc]}
    catch
        Error:Reason ->
            ?DEBUG("Failed to pretty format warning: ~p:~p~n~p",
                   [Error, Reason, erlang:get_stacktrace()]),
            {File, [dialyzer:format_warning(Warning, fullpath) | Acc]}
    end.

fmt(Fmt) ->
    cf:format(Fmt, []).
fmt(Fmt, Args) ->
    cf:format(Fmt, Args).

%%-----------------------------------------------------------------------------
%% Message classification and pretty-printing below. Messages appear in
%% categories and in more or less alphabetical ordering within each category.
%%-----------------------------------------------------------------------------

%%----- Warnings for general discrepancies ----------------
message_to_string({apply, [Args, ArgNs, FailReason,
                           SigArgs, SigRet, Contract]}) ->
    fmt("~!^Fun application with arguments ~!!~s ",
        [bad_arg(ArgNs, Args)]) ++
        call_or_apply_to_string(ArgNs, FailReason, SigArgs, SigRet, Contract);
message_to_string({app_call, [M, F, Args, Culprit, ExpectedType, FoundType]}) ->
    fmt("~!^The call~!! ~s:~s~s ~!^requires that"
        "~!! ~s ~!^is of type ~!g~s~!^ not ~!r~s",
        [M, F, Args, Culprit, ExpectedType, FoundType]);
message_to_string({bin_construction, [Culprit, Size, Seg, Type]}) ->
    fmt("~!^Binary construction will fail since the ~!b~s~!^ field~!!"
        " ~s~!^ in segment~!! ~s~!^ has type~!! ~s",
        [Culprit, Size, Seg, Type]);
message_to_string({call, [M, F, Args, ArgNs, FailReason,
                          SigArgs, SigRet, Contract]}) ->
    fmt("~!^The call~!! ~w:~w~s ", [M, F, bad_arg(ArgNs, Args)]) ++
        call_or_apply_to_string(ArgNs, FailReason, SigArgs, SigRet, Contract);
message_to_string({call_to_missing, [M, F, A]}) ->
    fmt("~!^Call to missing or unexported function ~!!~w:~w/~w",
        [M, F, A]);
message_to_string({exact_eq, [Type1, Op, Type2]}) ->
    fmt("~!^The test ~!!~s ~s ~s~!^ can never evaluate to 'true'",
        [Type1, Op, Type2]);
message_to_string({fun_app_args, [Args, Type]}) ->
    fmt("~!^Fun application with arguments ~!!~s~!^ will fail"
        " since the function has type ~!!~s", [Args, Type]);
message_to_string({fun_app_no_fun, [Op, Type, Arity]}) ->
    fmt("~!^Fun application will fail since ~!!~s ~!^::~!! ~s"
        " is not a function of arity ~!!~w", [Op, Type, Arity]);
message_to_string({guard_fail, []}) ->
    "~!^Clause guard cannot succeed.~!!";
message_to_string({guard_fail, [Arg1, Infix, Arg2]}) ->
    fmt("~!^Guard test ~!!~s ~s ~s~!^ can never succeed",
        [Arg1, Infix, Arg2]);
message_to_string({neg_guard_fail, [Arg1, Infix, Arg2]}) ->
    fmt("~!^Guard test not(~!!~s ~s ~s~!^) can never succeed",
        [Arg1, Infix, Arg2]);
message_to_string({guard_fail, [Guard, Args]}) ->
    fmt("~!^Guard test ~!!~w~s~!^ can never succeed",
        [Guard, Args]);
message_to_string({neg_guard_fail, [Guard, Args]}) ->
    fmt("~!^Guard test not(~!!~w~s~!^) can never succeed",
        [Guard, Args]);
message_to_string({guard_fail_pat, [Pat, Type]}) ->
    fmt("~!^Clause guard cannot succeed. The ~!!~s~!^ was matched"
        " against the type ~!!~s", [Pat, Type]);
message_to_string({improper_list_constr, [TlType]}) ->
    fmt("~!^Cons will produce an improper list"
        " since its ~!b2~!!nd~!^ argument is~!! ~s", [TlType]);
message_to_string({no_return, [Type|Name]}) ->
    NameString =
        case Name of
            [] -> fmt("~!^The created fun ");
            [F, A] -> fmt("~!^Function ~!r~w/~w ", [F, A])
        end,
    case Type of
        no_match -> fmt("~s~!^has no clauses that will ever match",[NameString]);
        only_explicit -> fmt("~s~!^only terminates with explicit exception", [NameString]);
        only_normal -> fmt("~s~!^has no local return", [NameString]);
        both -> fmt("~s~!^has no local return", [NameString])
    end;
message_to_string({record_constr, [RecConstr, FieldDiffs]}) ->
    fmt("~!^Record construction ~!!~s~!^ violates the"
        " declared type of field ~!!~s", [RecConstr, FieldDiffs]);
message_to_string({record_constr, [Name, Field, Type]}) ->
    fmt("~!^Record construction violates the declared type for ~!!#~w{}~!^"
        " since ~!!~s~!^ cannot be of type ~!!~s",
        [Name, Field, Type]);
message_to_string({record_matching, [String, Name]}) ->
    fmt("~!^The ~!!~s~!^ violates the"
        " declared type for ~!!#~w{}", [String, Name]);
message_to_string({record_match, [Pat, Type]}) ->
    fmt("~!^Matching of ~!!~s~!^ tagged with a record name violates the"
        " declared type of ~!!~s", [Pat, Type]);
message_to_string({pattern_match, [Pat, Type]}) ->
    fmt("~!^The ~s~!^ can never match the type ~!g~s",
        [bad_pat(Pat), Type]);
message_to_string({pattern_match_cov, [Pat, Type]}) ->
    fmt("~!^The ~s~!^ can never match since previous"
        " clauses completely covered the type ~!g~s",
        [bad_pat(Pat), Type]);
message_to_string({unmatched_return, [Type]}) ->
    fmt("~!^Expression produces a value of type ~!!~s~!^,"
        " but this value is unmatched", [Type]);
message_to_string({unused_fun, [F, A]}) ->
    fmt("~!^Function ~!r~w/~w~!!~!^ will never be called", [F, A]);
%%----- Warnings for specs and contracts -------------------
message_to_string({contract_diff, [M, F, _A, Contract, Sig]}) ->
    fmt("~!^Type specification ~!!~w:~w~s~!^"
        " is not equal to the success typing: ~!!~w:~w~s",
        [M, F, Contract, M, F, Sig]);
message_to_string({contract_subtype, [M, F, _A, Contract, Sig]}) ->
    fmt("~!^Type specification ~!!~w:~w~s~!^"
        " is a subtype of the success typing: ~!!~w:~w~s",
        [M, F, Contract, M, F, Sig]);
message_to_string({contract_supertype, [M, F, _A, Contract, Sig]}) ->
    fmt("~!^Type specification ~!!~w:~w~s~!^"
        " is a supertype of the success typing: ~!!~w:~w~s",
        [M, F, Contract, M, F, Sig]);
message_to_string({contract_range, [Contract, M, F, ArgStrings, Line, CRet]}) ->
    fmt("~!^The contract ~!!~w:~w~s~!^ cannot be right because the"
        " inferred return for ~!!~w~s~!^ on line ~!!~w~!^ is ~!!~s",
        [M, F, Contract, F, ArgStrings, Line, CRet]);
message_to_string({invalid_contract, [M, F, A, Sig]}) ->
    fmt("~!^Invalid type specification for function~!! ~w:~w/~w."
        "~!^ The success typing is~!! ~s", [M, F, A, Sig]);
message_to_string({extra_range, [M, F, A, ExtraRanges, SigRange]}) ->
    fmt("~!^The specification for ~!!~w:~w/~w~!^ states that the function"
        " might also return ~!!~s~!^ but the inferred return is ~!!~s",
        [M, F, A, ExtraRanges, SigRange]);
message_to_string({overlapping_contract, [M, F, A]}) ->
    fmt("~!^Overloaded contract for ~!!~w:~w/~w~!^ has overlapping"
        " domains; such contracts are currently unsupported and are simply "
        "ignored", [M, F, A]);
message_to_string({spec_missing_fun, [M, F, A]}) ->
    fmt("~!^Contract for function that does not exist: ~!!~w:~w/~w",
        [M, F, A]);
%%----- Warnings for opaque type violations -------------------
message_to_string({call_with_opaque, [M, F, Args, ArgNs, ExpArgs]}) ->
    fmt("~!^The call ~!!~w:~w~s~!^ contains ~!!~s~!^ when ~!!~s",
        [M, F, bad_arg(ArgNs, Args), form_positions(ArgNs), form_expected(ExpArgs)]);
message_to_string({call_without_opaque, [M, F, Args, [{N,_,_}|_] = ExpectedTriples]}) ->
    fmt("~!^The call ~!!~w:~w~s ~!^does not have~!! ~s",
        [M, F, bad_arg(N, Args), form_expected_without_opaque(ExpectedTriples)]);
message_to_string({opaque_eq, [Type, _Op, OpaqueType]}) ->
    fmt("~!^Attempt to test for equality between a term of type ~!!~s~!^"
        " and a term of opaque type ~!!~s", [Type, OpaqueType]);
message_to_string({opaque_guard, [Arg1, Infix, Arg2, ArgNs]}) ->
    fmt("~!^Guard test ~!!~s ~s ~s~!^ contains ~!!~s",
        [Arg1, Infix, Arg2, form_positions(ArgNs)]);
message_to_string({opaque_guard, [Guard, Args]}) ->
    fmt("~!^Guard test ~!!~w~s~!^ breaks the opaqueness of its"
        " argument", [Guard, Args]);
message_to_string({opaque_match, [Pat, OpaqueType, OpaqueTerm]}) ->
    Term = if OpaqueType =:= OpaqueTerm -> "the term";
              true -> OpaqueTerm
           end,
    fmt("~!^The attempt to match a term of type ~!!~s~!^ against the"
        "~!! ~s~!^ breaks the opaqueness of ~!!~s",
        [OpaqueType, Pat, Term]);
message_to_string({opaque_neq, [Type, _Op, OpaqueType]}) ->
    fmt("~!^Attempt to test for inequality between a term of type ~!!~s"
        "~!^ and a term of opaque type ~!!~s", [Type, OpaqueType]);
message_to_string({opaque_type_test, [Fun, Args, Arg, ArgType]}) ->
    fmt("~!^The type test ~!!~s~s~!^ breaks the opaqueness of the term "
        "~!!~s~s", [Fun, Args, Arg, ArgType]);
message_to_string({opaque_size, [SizeType, Size]}) ->
    fmt("~!^The size ~!!~s~!^ breaks the opaqueness of ~!!~s",
        [SizeType, Size]);
message_to_string({opaque_call, [M, F, Args, Culprit, OpaqueType]}) ->
    fmt("~!^The call ~!!~s:~s~s~!^ breaks the opaqueness of the term~!!"
        " ~s :: ~s", [M, F, Args, Culprit, OpaqueType]);
%%----- Warnings for concurrency errors --------------------
message_to_string({race_condition, [M, F, Args, Reason]}) ->
    fmt("~!^The call ~!!~w:~w~s ~s", [M, F, Args, Reason]);
%%----- Warnings for behaviour errors --------------------
message_to_string({callback_type_mismatch, [B, F, A, ST, CT]}) ->
    fmt("~!^The inferred return type of~!! ~w/~w (~s) ~!^"
        "has nothing in common with~!! ~s, ~!^which is the expected"
        " return type for the callback of~!! ~w ~!^behaviour",
        [F, A, ST, CT, B]);
message_to_string({callback_arg_type_mismatch, [B, F, A, N, ST, CT]}) ->
    fmt("~!^The inferred type for the~!! ~s ~!^argument of~!!"
        " ~w/~w (~s) ~!^is not a supertype of~!! ~s~!^, which is"
        "expected type for this argument in the callback of the~!! ~w "
        "~!^behaviour",
        [ordinal(N), F, A, ST, CT, B]);
message_to_string({callback_spec_type_mismatch, [B, F, A, ST, CT]}) ->
    fmt("~!^The return type ~!!~s~!^ in the specification of ~!!"
        "~w/~w~!^ is not a subtype of ~!!~s~!^, which is the expected"
        " return type for the callback of ~!!~w~!^ behaviour",
        [ST, F, A, CT, B]);
message_to_string({callback_spec_arg_type_mismatch, [B, F, A, N, ST, CT]}) ->
    fmt("~!^The specified type for the ~!!~s~!^ argument of ~!!"
        "~w/~w (~s)~!^ is not a supertype of ~!!~s~!^, which is"
        " expected type for this argument in the callback of the ~!!~w"
        "~!^ behaviour", [ordinal(N), F, A, ST, CT, B]);
message_to_string({callback_missing, [B, F, A]}) ->
    fmt("~!^Undefined callback function ~!!~w/~w~!^ (behaviour ~!!"
        "'~w'~!^)",[F, A, B]);
message_to_string({callback_info_missing, [B]}) ->
    fmt("~!^Callback info about the ~!r~w~!!~!^"
        " behaviour is not available", [B]);
%%----- Warnings for unknown functions, types, and behaviours -------------
message_to_string({unknown_type, {M, F, A}}) ->
    fmt("~!^Unknown type ~!r~w:~w/~w", [M, F, A]);
message_to_string({unknown_function, {M, F, A}}) ->
    fmt("~!^Unknown function ~!r~w:~w/~w", [M, F, A]);
message_to_string({unknown_behaviour, B}) ->
    fmt("~!^Unknown behaviour ~!r~w", [B]).

%%-----------------------------------------------------------------------------
%% Auxiliary functions below
%%-----------------------------------------------------------------------------

call_or_apply_to_string(ArgNs, FailReason, SigArgs, SigRet,
                        {IsOverloaded, Contract}) ->
    PositionString = form_position_string(ArgNs),
    case FailReason of
        only_sig ->
            case ArgNs =:= [] of
                true ->
                    %% We do not know which argument(s) caused the failure
                    fmt("~!^will never return since the success typing arguments"
                        " are ~!!~s", [SigArgs]);
                false ->
                    fmt("~!^will never return since it differs in the~!!"
                        " ~s ~!^argument from the success typing"
                        " arguments:~!! ~s",
                        [PositionString, good_arg(ArgNs, SigArgs)])
            end;
        only_contract ->
            case (ArgNs =:= []) orelse IsOverloaded of
                true ->
                    %% We do not know which arguments caused the failure
                    fmt("~!^breaks the contract~!! ~s", [good_arg(ArgNs, Contract)]);
                false ->
                    fmt("~!^breaks the contract~!! ~s ~!^in the~!!"
                        " ~s ~!^argument",
                        [good_arg(ArgNs, Contract), PositionString])
            end;
        both ->
            fmt("~!^will never return since the success typing is "
                "~!!~s ~!^->~!! ~s ~!^and the contract is ~!!~s",
                [good_arg(ArgNs, SigArgs), SigRet,
                 good_arg(ArgNs, Contract)])
    end.

form_positions(ArgNs) ->
    ArgS = form_position_string(ArgNs),
    case ArgNs of
        [_] ->     fmt("~!^an opaque term as ~!!~s~!^ argument", [ArgS]);
        [_,_|_] -> fmt("~!^opaque terms as ~!!~s~!^ arguments", [ArgS])
    end.

%% We know which positions N are to blame;
%% the list of triples will never be empty.
form_expected_without_opaque([{N, T, TStr}]) ->
    FStr = case erl_types:t_is_opaque(T) of
               true  ->
                   "~!^an opaque term of type~!g ~s ~!^as ";
               false ->
                   "~!^a term of type ~!g~s ~!^(with opaque subterms) as "
           end ++ form_position_string([N]) ++ "~!^ argument",
    fmt(FStr, [TStr]);

form_expected_without_opaque(ExpectedTriples) -> %% TODO: can do much better here
    {ArgNs, _Ts, _TStrs} = lists:unzip3(ExpectedTriples),
    "opaque terms as " ++ form_position_string(ArgNs) ++ " arguments".

form_expected(ExpectedArgs) ->
    case ExpectedArgs of
        [T] ->
            TS = erl_types:t_to_string(T),
            case erl_types:t_is_opaque(T) of
                true  -> fmt("~!^an opaque term of type ~!!~s~!^ is"
                             " expected", [TS]);
                false -> fmt("~!^a structured term of type ~!!~s~!^ is"
                             " expected", [TS])
            end;
        [_,_|_] -> fmt("~!^terms of different types are expected in these"
                       " positions", [])
    end.

form_position_string(ArgNs) ->
    case ArgNs of
        [] -> "";
        [N1] -> ordinal(N1);
        [_,_|_] ->
            [Last|Prevs] = lists:reverse(ArgNs),
            ", " ++ Head = lists:flatten([fmt(", ~s",[ordinal(N)]) ||
                                             N <- lists:reverse(Prevs)]),
            Head ++ " and " ++ ordinal(Last)
    end.

ordinal(1) -> fmt("~!B1~!!st");
ordinal(2) -> fmt("~!B2~!!nd");
ordinal(3) -> fmt("~!B3~!!rd");
ordinal(N) when is_integer(N) -> fmt("~!B~w~!!th", [N]).

%% Format a pattern ad highlight errorous part in red.
bad_pat("pattern " ++ P) ->
    fmt("pattern ~!r~s",[P]);
bad_pat("variable " ++ P) ->
    fmt("variable ~!r~s",[P]);
bad_pat(P) ->
    fmt("~!r~s",[P]).


bad_arg(N, Args) ->
    colour_arg(N, r, Args).

good_arg(N, Args) ->
    colour_arg(N, g, Args).

%% colour one or more arg of an argument list, this unparses the args
%% highlights one or more of them and puts them back together.
colour_arg(N, C, Args) when is_integer(N) ->
    colour_arg([N], C, Args);
colour_arg(Ns, C, Args) ->
    {Args1, Rest} =seperate_args(Args),
    Args2 = highlight(Ns, 1, C, Args1),
    join_args(Args2) ++ Rest.

highlight([], _N, _C, Rest) ->
    Rest;

highlight([N | Nr], N, g, [Arg | Rest]) ->
    [fmt("~!g~s", [Arg]) | highlight(Nr, N+1, g, Rest)];

highlight([N | Nr], N, r, [Arg | Rest]) ->
    [fmt("~!r~s", [Arg]) | highlight(Nr, N+1, r, Rest)];

highlight(Ns, N, C, [Arg | Rest]) ->
    [Arg | highlight(Ns, N + 1, C, Rest)].

%% Arugments to functions and constraints are passed as
%% strings not as data, this function pulls them apart
%% to allow interacting with them seperately and not
%% as one bug chunk of data.
seperate_args([$( | S]) ->
    seperate_args([], S, "", []).

%% We strip this space since dialyzer is inconsistant in adding or not adding
%% it ....
seperate_args([], [$,, $\s | R], Arg, Args) ->
    seperate_args([], R, [], [lists:reverse(Arg) | Args]);

seperate_args([], [$, | R], Arg, Args) ->
    seperate_args([], R, [], [lists:reverse(Arg) | Args]);

seperate_args([], [$) | Rest], Arg, Args) ->
    {lists:reverse([lists:reverse(Arg) | Args]), Rest};
seperate_args([C | D], [C | R], Arg, Args) ->
    seperate_args(D, R, [C | Arg], Args);
%% Brackets
seperate_args(D, [${ | R], Arg, Args) ->
    seperate_args([$}|D], R, [${ | Arg], Args);

seperate_args(D, [$( | R], Arg, Args) ->
    seperate_args([$)|D], R, [$( | Arg], Args);

seperate_args(D, [$[ | R], Arg, Args) ->
    seperate_args([$]|D], R, [$[ | Arg], Args);

seperate_args(D, [$< | R], Arg, Args) ->
    seperate_args([$>|D], R, [$< | Arg], Args);
%% 'strings'
seperate_args(D, [$' | R], Arg, Args) ->
    seperate_args([$'|D], R, [$' | Arg], Args);
seperate_args(D, [$" | R], Arg, Args) ->
    seperate_args([$"|D], R, [$" | Arg], Args);

seperate_args(D, [C | R], Arg, Args) ->
    seperate_args(D, R, [C | Arg], Args).

join_args(Args) ->
    [$(, string:join(Args, ", "), $)].
