-module(rebar_dialyzer_format).

-export([format/1, bad_arg/2]).

-define(NR, "\033[0;31m").
-define(NG, "\033[0;32m").
-define(NB, "\033[0;34m").
-define(NW, "\033[0;37m").
-define(BR, "\033[1;31m").
-define(BG, "\033[1;32m").
-define(BB, "\033[1;34m").
-define(BW, "\033[1;37m").
-define(R,  "\033[0m").

format(Warning) ->
    Str = try
              format_warning(Warning, fullpath)
          catch
              _:_ ->
                  dialyzer:format_warning(Warning, fullpath)
          end,
    case strip(Str) of
        ":0: " ++ Unknown ->
            Unknown;
        Warning1 ->
            Warning1
    end.

strip(Warning) ->
    string:strip(Warning, right, $\n).

%%format(Fmt, Args) ->
%%    Args2 = [format("~s\033[1;37m", [A]) || A <- Args],
%%    format(Fmt, Args2).

format(Fmt, Args) ->
    io_lib:format(lists:flatten(Fmt), Args).


%% Mostrly from: https://github.com/erlware/erlware_commons/blob/49bc69e35a282bde4a0a6a8f211b5f77d8585256/src/ec_cmd_log.erl#L220
%%colorize(Color, Msg) when is_integer(Color) ->
%%    colorize(Color, false, Msg).

%% colorize(Color, false, Msg) when is_integer(Color) ->
%%     lists:flatten(format("\033[~B;~Bm~s\033[0m", [0, Color, Msg]));
%% colorize(Color, true, Msg) when is_integer(Color) ->
%%     lists:flatten(format("\033[~B;~Bm~s\033[0m", [1, Color, Msg])).


%%bw(M) ->
%%    colorize(37, true, M).

%% Based on: https://github.com/erlang/otp/blob/a2670f0822fc6729df956c8ec8c381340ff0a5fb/lib/dialyzer/src/dialyzer.erl#L290

format_warning({Tag, {File, Line, _MFA}, Msg}, FOpt) ->
    format_warning({Tag, {File, Line}, Msg}, FOpt);
format_warning({_Tag, {File, Line}, Msg}, FOpt) when is_list(File),
                                                     is_integer(Line) ->
    F = case FOpt of
            fullpath -> File;
            basename -> filename:basename(File)
        end,
    String = lists:flatten(message_to_string(Msg)),
    lists:flatten(format("~s:~w: ~s", [F, Line, String])).


%%-----------------------------------------------------------------------------
%% Message classification and pretty-printing below. Messages appear in
%% categories and in more or less alphabetical ordering within each category.
%%-----------------------------------------------------------------------------

%%----- Warnings for general discrepancies ----------------
message_to_string({apply, [Args, ArgNs, FailReason,
                           SigArgs, SigRet, Contract]}) ->
    format("Fun application with arguments ~s ", [Args]) ++
        call_or_apply_to_string(ArgNs, FailReason, SigArgs, SigRet, Contract);
message_to_string({app_call, [M, F, Args, Culprit, ExpectedType, FoundType]}) ->
    format("The call ~s:~s~s requires that ~s is of type ~s not ~s\n",
           [M, F, Args, Culprit, ExpectedType, FoundType]);
message_to_string({bin_construction, [Culprit, Size, Seg, Type]}) ->
    format("Binary construction will fail since the ~s field ~s in"
           " segment ~s has type ~s\n", [Culprit, Size, Seg, Type]);
message_to_string({call, [M, F, Args, ArgNs, FailReason,
                          SigArgs, SigRet, Contract]}) ->
    format("The call ~w:~w~s ", [M, F, Args]) ++
        call_or_apply_to_string(ArgNs, FailReason, SigArgs, SigRet, Contract);
message_to_string({call_to_missing, [M, F, A]}) ->
    format("Call to missing or unexported function ~w:~w/~w\n", [M, F, A]);
message_to_string({exact_eq, [Type1, Op, Type2]}) ->
    format("The test ~s ~s ~s can never evaluate to 'true'\n",
           [Type1, Op, Type2]);
message_to_string({fun_app_args, [Args, Type]}) ->
    format("Fun application with arguments ~s will fail"
           " since the function has type ~s\n", [Args, Type]);
message_to_string({fun_app_no_fun, [Op, Type, Arity]}) ->
    format("Fun application will fail since ~s :: ~s"
           " is not a function of arity ~w\n", [Op, Type, Arity]);
message_to_string({guard_fail, []}) ->
    "Clause guard cannot succeed.\n";
message_to_string({guard_fail, [Arg1, Infix, Arg2]}) ->
    format("Guard test ~s ~s ~s can never succeed\n", [Arg1, Infix, Arg2]);
message_to_string({neg_guard_fail, [Arg1, Infix, Arg2]}) ->
    format("Guard test not(~s ~s ~s) can never succeed\n",
           [Arg1, Infix, Arg2]);
message_to_string({guard_fail, [Guard, Args]}) ->
    format("Guard test ~w~s can never succeed\n", [Guard, Args]);
message_to_string({neg_guard_fail, [Guard, Args]}) ->
    format("Guard test not(~w~s) can never succeed\n", [Guard, Args]);
message_to_string({guard_fail_pat, [Pat, Type]}) ->
    format("Clause guard cannot succeed. The ~s was matched"
           " against the type ~s\n", [Pat, Type]);
message_to_string({improper_list_constr, [TlType]}) ->
    format("Cons will produce an improper list"
           " since its 2nd argument is ~s\n", [TlType]);
message_to_string({no_return, [Type|Name]}) ->
    NameString =
        case Name of
            [] -> "The created fun ";
            [F, A] -> format("Function ~w/~w ", [F, A])
        end,
    case Type of
        no_match -> NameString ++ "has no clauses that will ever match\n";
        only_explicit -> NameString ++ "only terminates with explicit exception\n";
        only_normal -> NameString ++ "has no local return\n";
        both -> NameString ++ "has no local return\n"
    end;
message_to_string({record_constr, [RecConstr, FieldDiffs]}) ->
    format("Record construction ~s violates the"
           " declared type of field ~s\n", [RecConstr, FieldDiffs]);
message_to_string({record_constr, [Name, Field, Type]}) ->
    format("Record construction violates the declared type for #~w{}"
           " since ~s cannot be of type ~s\n", [Name, Field, Type]);
message_to_string({record_matching, [String, Name]}) ->
    format("The ~s violates the"
           " declared type for #~w{}\n", [String, Name]);
message_to_string({record_match, [Pat, Type]}) ->
    format("Matching of ~s tagged with a record name violates the declared"
           " type of ~s\n", [Pat, Type]);
message_to_string({pattern_match, [Pat, Type]}) ->
    format("The ~s can never match the type ~s\n", [Pat, Type]);
message_to_string({pattern_match_cov, [Pat, Type]}) ->
    format("The ~s can never match since previous"
           " clauses completely covered the type ~s\n",
           [Pat, Type]);
message_to_string({unmatched_return, [Type]}) ->
    format("Expression produces a value of type ~s,"
           " but this value is unmatched\n", [Type]);
message_to_string({unused_fun, [F, A]}) ->
    format("Function ~w/~w will never be called\n", [F, A]);
%%----- Warnings for specs and contracts -------------------
message_to_string({contract_diff, [M, F, _A, Contract, Sig]}) ->
    format("Type specification ~w:~w~s"
           " is not equal to the success typing: ~w:~w~s\n",
           [M, F, Contract, M, F, Sig]);
message_to_string({contract_subtype, [M, F, _A, Contract, Sig]}) ->
    format("Type specification ~w:~w~s"
           " is a subtype of the success typing: ~w:~w~s\n",
           [M, F, Contract, M, F, Sig]);
message_to_string({contract_supertype, [M, F, _A, Contract, Sig]}) ->
    format("Type specification ~w:~w~s"
           " is a supertype of the success typing: ~w:~w~s\n",
           [M, F, Contract, M, F, Sig]);
message_to_string({contract_range, [Contract, M, F, ArgStrings, Line, CRet]}) ->
    format("The contract ~w:~w~s cannot be right because the inferred"
           " return for ~w~s on line ~w is ~s\n",
           [M, F, Contract, F, ArgStrings, Line, CRet]);
message_to_string({invalid_contract, [M, F, A, Sig]}) ->
    format("Invalid type specification for function ~w:~w/~w."
           " The success typing is ~s\n", [M, F, A, Sig]);
message_to_string({extra_range, [M, F, A, ExtraRanges, SigRange]}) ->
    format("The specification for ~w:~w/~w states that the function"
           " might also return ~s but the inferred return is ~s\n",
           [M, F, A, ExtraRanges, SigRange]);
message_to_string({overlapping_contract, [M, F, A]}) ->
    format("Overloaded contract for ~w:~w/~w has overlapping domains;"
           " such contracts are currently unsupported and are simply ignored\n",
           [M, F, A]);
message_to_string({spec_missing_fun, [M, F, A]}) ->
    format("Contract for function that does not exist: ~w:~w/~w\n",
           [M, F, A]);
%%----- Warnings for opaque type violations -------------------
message_to_string({call_with_opaque, [M, F, Args, ArgNs, ExpArgs]}) ->
    format("The call ~w:~w~s contains ~s when ~s\n",
           [M, F, Args, form_positions(ArgNs), form_expected(ExpArgs)]);
message_to_string({call_without_opaque, [M, F, Args, [{N,_,_}|_] = ExpectedTriples]}) ->
    format([?BW, "The call", ?R, " ~w:~w~s ", ?BW, "does not have" ?R " ~s\n"],
           [M, F, bad_arg(N, Args), form_expected_without_opaque(ExpectedTriples)]);
message_to_string({opaque_eq, [Type, _Op, OpaqueType]}) ->
    format("Attempt to test for equality between a term of type ~s"
           " and a term of opaque type ~s\n", [Type, OpaqueType]);
message_to_string({opaque_guard, [Arg1, Infix, Arg2, ArgNs]}) ->
    format("Guard test ~s ~s ~s contains ~s\n",
           [Arg1, Infix, Arg2, form_positions(ArgNs)]);
message_to_string({opaque_guard, [Guard, Args]}) ->
    format("Guard test ~w~s breaks the opaqueness of its argument\n",
           [Guard, Args]);
message_to_string({opaque_match, [Pat, OpaqueType, OpaqueTerm]}) ->
    Term = if OpaqueType =:= OpaqueTerm -> "the term";
              true -> OpaqueTerm
           end,
    format("The attempt to match a term of type ~s against the ~s"
           " breaks the opaqueness of ~s\n", [OpaqueType, Pat, Term]);
message_to_string({opaque_neq, [Type, _Op, OpaqueType]}) ->
    format("Attempt to test for inequality between a term of type ~s"
           " and a term of opaque type ~s\n", [Type, OpaqueType]);
message_to_string({opaque_type_test, [Fun, Args, Arg, ArgType]}) ->
    format("The type test ~s~s breaks the opaqueness of the term ~s~s\n",
           [Fun, Args, Arg, ArgType]);
message_to_string({opaque_size, [SizeType, Size]}) ->
    format("The size ~s breaks the opaqueness of ~s\n",
           [SizeType, Size]);
message_to_string({opaque_call, [M, F, Args, Culprit, OpaqueType]}) ->
    format("The call ~s:~s~s breaks the opaqueness of the term ~s :: ~s\n",
           [M, F, Args, Culprit, OpaqueType]);
%%----- Warnings for concurrency errors --------------------
message_to_string({race_condition, [M, F, Args, Reason]}) ->
    format("The call ~w:~w~s ~s\n", [M, F, Args, Reason]);
%%----- Warnings for behaviour errors --------------------
message_to_string({callback_type_mismatch, [B, F, A, ST, CT]}) ->
    format("The inferred return type of ~w/~w (~s) has nothing in common"
           " with ~s, which is the expected return type for the callback of"
           " ~w behaviour\n", [F, A, ST, CT, B]);
message_to_string({callback_arg_type_mismatch, [B, F, A, N, ST, CT]}) ->
    format("The inferred type for the ~s argument of ~w/~w (~s) is"
           " not a supertype of ~s, which is expected type for this"
           " argument in the callback of the ~w behaviour\n",
           [ordinal(N), F, A, ST, CT, B]);
message_to_string({callback_spec_type_mismatch, [B, F, A, ST, CT]}) ->
    format("The return type ~s in the specification of ~w/~w is not a"
           " subtype of ~s, which is the expected return type for the"
           " callback of ~w behaviour\n", [ST, F, A, CT, B]);
message_to_string({callback_spec_arg_type_mismatch, [B, F, A, N, ST, CT]}) ->
    format("The specified type for the ~s argument of ~w/~w (~s) is"
           " not a supertype of ~s, which is expected type for this"
           " argument in the callback of the ~w behaviour\n",
           [ordinal(N), F, A, ST, CT, B]);
message_to_string({callback_missing, [B, F, A]}) ->
    format("Undefined callback function ~w/~w (behaviour '~w')\n",
           [F, A, B]);
message_to_string({callback_info_missing, [B]}) ->
    format("Callback info about the ~w behaviour is not available\n", [B]);
%%----- Warnings for unknown functions, types, and behaviours -------------
message_to_string({unknown_type, {M, F, A}}) ->
    format("Unknown type ~w:~w/~w", [M, F, A]);
message_to_string({unknown_function, {M, F, A}}) ->
    format("Unknown function ~w:~w/~w", [M, F, A]);
message_to_string({unknown_behaviour, B}) ->
    format("Unknown behaviour ~w", [B]).

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
                    format("will never return since the success typing arguments"
                           " are ~s\n", [SigArgs]);
                false ->
                    format("will never return since it differs in the ~s argument"
                           " from the success typing arguments: ~s\n",
                           [PositionString, SigArgs])
            end;
        only_contract ->
            case (ArgNs =:= []) orelse IsOverloaded of
                true ->
                    %% We do not know which arguments caused the failure
                    format("breaks the contract ~s\n", [Contract]);
                false ->
                    format("breaks the contract ~s in the ~s argument\n",
                           [Contract, PositionString])
            end;
        both ->
            format("will never return since the success typing is ~s -> ~s"
                   " and the contract is ~s\n", [SigArgs, SigRet, Contract])
    end.

form_positions(ArgNs) ->
    case ArgNs of
        [_] -> "an opaque term as ";
        [_,_|_] -> "opaque terms as "
    end ++ form_position_string(ArgNs) ++
        case ArgNs of
            [_] -> " argument";
            [_,_|_] -> " arguments"
        end.

%% We know which positions N are to blame;
%% the list of triples will never be empty.
form_expected_without_opaque([{N, T, TStr}]) ->
    case erl_types:t_is_opaque(T) of
        true  ->
            format([?BW, "an opaque term of type", ?NG, " ~s ", ?BW, "as "], [TStr]);
        false ->
            format([?BW, "a term of type ", ?NG, "~s ", ?BW, "(with opaque subterms) as "], [TStr])
    end ++ form_position_string([N]) ++ ?BW ++ " argument" ++ ?R;
form_expected_without_opaque(ExpectedTriples) -> %% TODO: can do much better here
    {ArgNs, _Ts, _TStrs} = lists:unzip3(ExpectedTriples),
    "opaque terms as " ++ form_position_string(ArgNs) ++ " arguments".

form_expected(ExpectedArgs) ->
    case ExpectedArgs of
        [T] ->
            TS = erl_types:t_to_string(T),
            case erl_types:t_is_opaque(T) of
                true  -> format("an opaque term of type ~s is expected", [TS]);
                false -> format("a structured term of type ~s is expected", [TS])
            end;
        [_,_|_] -> "terms of different types are expected in these positions"
    end.

form_position_string(ArgNs) ->
    case ArgNs of
        [] -> "";
        [N1] -> ordinal(N1);
        [_,_|_] ->
            [Last|Prevs] = lists:reverse(ArgNs),
            ", " ++ Head = lists:flatten([format(", ~s",[ordinal(N)]) ||
                                             N <- lists:reverse(Prevs)]),
            Head ++ " and " ++ ordinal(Last)
    end.

ordinal(1) -> ?BB ++ "1" ++ ?R ++ "st";
ordinal(2) -> ?BB ++ "2" ++ ?R ++ "nd";
ordinal(3) -> ?BB ++ "3" ++ ?R ++ "rd";
ordinal(N) when is_integer(N) -> format(?BB ++ "~w" ++ ?R ++ "th", [N]).


bad_arg(N, Args) ->
    Args1 = seperate_args(Args),
    Args2 = highlight(N, Args1),
    join_args(Args2).


highlight(1, [Arg | Rest]) ->
    [[?NR, Arg, ?R] | Rest];

highlight(N, [Arg | Rest]) ->
    [Arg | highlight(N - 1, Rest)].

seperate_args([$( | S]) ->
    seperate_args([], S, "", []).

seperate_args([], [$,, Next | R], Arg, Args) ->
    seperate_args([], R, [Next], [lists:reverse(Arg) | Args]);
seperate_args([], [$)], Arg, Args) ->
    lists:reverse([lists:reverse(Arg) | Args]);
seperate_args([C | D], [C | R], Arg, Args) ->
    seperate_args(D, R, [C | Arg], Args);
seperate_args(D, [${ | R], Arg, Args) ->
    seperate_args([$}|D], R, [${ | Arg], Args);
seperate_args(D, [$' | R], Arg, Args) ->
    seperate_args([$'|D], R, [$' | Arg], Args);
seperate_args(D, [$" | R], Arg, Args) ->
    seperate_args([$"|D], R, [$" | Arg], Args);
seperate_args(D, [$( | R], Arg, Args) ->
    seperate_args([$)|D], R, [$( | Arg], Args);
seperate_args(D, [$< | R], Arg, Args) ->
    seperate_args([$>|D], R, [$< | Arg], Args);
seperate_args(D, [C | R], Arg, Args) ->
    seperate_args(D, R, [C | Arg], Args).

join_args(Args) ->
    [$(, string:join(Args, ", "), $)].
