-module(rebar_dialyzer_format).

-include("rebar.hrl").

-export([format/1]).


format(Warning) ->
    Str = try
              format_warning(Warning, fullpath)
          catch
              Error:Reason ->
                  ?DEBUG("Failed to pretty format warning: ~p:~p",
                         [Error, Reason]),
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

fmt(Fmt) ->
    cf:format(Fmt, []).
fmt(Fmt, Args) ->
    cf:format(Fmt, Args).

format_warning({Tag, {File, Line, _MFA}, Msg}, FOpt) ->
    format_warning({Tag, {File, Line}, Msg}, FOpt);
format_warning({_Tag, {File, Line}, Msg}, FOpt) when is_list(File),
                                                     is_integer(Line) ->
    F = case FOpt of
            fullpath -> re:replace(File, "^.*/_build/", "_build/");
            basename -> filename:basename(File)
        end,
    String = lists:flatten(message_to_string(Msg)),
    lists:flatten(fmt("~s:~w~n~s", [F, Line, String])).

%%-----------------------------------------------------------------------------
%% Message classification and pretty-printing below. Messages appear in
%% categories and in more or less alphabetical ordering within each category.
%%-----------------------------------------------------------------------------

%%----- Warnings for general discrepancies ----------------
message_to_string({apply, [Args, ArgNs, FailReason,
                           SigArgs, SigRet, Contract]}) ->
    fmt("~!WFun application with arguments ~!!~s ",
        [bad_arg(ArgNs, Args)]) ++
        call_or_apply_to_string(ArgNs, FailReason, SigArgs, SigRet, Contract);
message_to_string({app_call, [M, F, Args, Culprit, ExpectedType, FoundType]}) ->
    fmt("~!WThe call~!! ~s:~s~s ~!Wrequires that"
        "~!! ~s ~!Wis of type ~!g~s~!W not ~!r~s"
        "~!!\n",
        [M, F, Args, Culprit, ExpectedType, FoundType]);
message_to_string({bin_construction, [Culprit, Size, Seg, Type]}) ->
    fmt("~!WBinary construction will fail since the ~!b~s~!W field~!!"
        " ~s~!W in segment~!! ~s~!W has type~!! ~s\n",
        [Culprit, Size, Seg, Type]);
message_to_string({call, [M, F, Args, ArgNs, FailReason,
                          SigArgs, SigRet, Contract]}) ->
    fmt("~!WThe call~!! ~w:~w~s ", [M, F, bad_arg(ArgNs, Args)]) ++
        call_or_apply_to_string(ArgNs, FailReason, SigArgs, SigRet, Contract);
message_to_string({call_to_missing, [M, F, A]}) ->
    fmt("~!WCall to missing or unexported function ~!!~w:~w/~w\n",
        [M, F, A]);
message_to_string({exact_eq, [Type1, Op, Type2]}) ->
    fmt("~!WThe test ~!!~s ~s ~s~!W can never evaluate to 'true'\n",
        [Type1, Op, Type2]);
message_to_string({fun_app_args, [Args, Type]}) ->
    fmt("~!WFun application with arguments ~!!~s~!W will fail"
        " since the function has type ~!!~s\n", [Args, Type]);
message_to_string({fun_app_no_fun, [Op, Type, Arity]}) ->
    fmt("~!WFun application will fail since ~!!~s ~!W::~!! ~s"
        " is not a function of arity ~!!~w\n", [Op, Type, Arity]);
message_to_string({guard_fail, []}) ->
    "~!WClause guard cannot succeed.\n~!!";
message_to_string({guard_fail, [Arg1, Infix, Arg2]}) ->
    fmt("~!WGuard test ~!!~s ~s ~s~!W can never succeed\n",
        [Arg1, Infix, Arg2]);
message_to_string({neg_guard_fail, [Arg1, Infix, Arg2]}) ->
    fmt("~!WGuard test not(~!!~s ~s ~s~!W) can never succeed\n",
        [Arg1, Infix, Arg2]);
message_to_string({guard_fail, [Guard, Args]}) ->
    fmt("~!WGuard test ~!!~w~s~!W can never succeed\n",
        [Guard, Args]);
message_to_string({neg_guard_fail, [Guard, Args]}) ->
    fmt("~!WGuard test not(~!!~w~s~!W) can never succeed\n",
        [Guard, Args]);
message_to_string({guard_fail_pat, [Pat, Type]}) ->
    fmt("~!WClause guard cannot succeed. The ~!!~s~!W was matched"
        " against the type ~!!~s\n", [Pat, Type]);
message_to_string({improper_list_constr, [TlType]}) ->
    fmt("~!WCons will produce an improper list"
        " since its ~!b2~!!nd~!W argument is~!! ~s\n", [TlType]);
message_to_string({no_return, [Type|Name]}) ->
    NameString =
        case Name of
            [] -> fmt("~!WThe created fun ");
            [F, A] -> fmt("~!WFunction ~!r~w/~w ", [F, A])
        end,
    case Type of
        no_match -> fmt("~s~!Whas no clauses that will ever match\n",[NameString]);
        only_explicit -> fmt("~s~!Wonly terminates with explicit exception\n", [NameString]);
        only_normal -> fmt("~s~!Whas no local return\n", [NameString]);
        both -> fmt("~s~!Whas no local return\n", [NameString])
    end;
message_to_string({record_constr, [RecConstr, FieldDiffs]}) ->
    fmt("~!WRecord construction ~!!~s~!W violates the"
        " declared type of field ~!!~s\n", [RecConstr, FieldDiffs]);
message_to_string({record_constr, [Name, Field, Type]}) ->
    fmt("~!WRecord construction violates the declared type for ~!!#~w{}~!W"
        " since ~!!~s~!W cannot be of type ~!!~s\n",
        [Name, Field, Type]);
message_to_string({record_matching, [String, Name]}) ->
    fmt("~!WThe ~!!~s~!W violates the"
        " declared type for ~!!#~w{}\n", [String, Name]);
message_to_string({record_match, [Pat, Type]}) ->
    fmt("~!WMatching of ~!!~s~!W tagged with a record name violates the"
        " declared type of ~!!~s\n", [Pat, Type]);
message_to_string({pattern_match, [Pat, Type]}) ->
    fmt("~!WThe ~s~!W can never match the type ~!g~s\n",
        [bad_pat(Pat), Type]);
message_to_string({pattern_match_cov, [Pat, Type]}) ->
    fmt("~!WThe ~s~!W can never match since previous"
        " clauses completely covered the type ~!g~s\n",
        [bad_pat(Pat), Type]);
message_to_string({unmatched_return, [Type]}) ->
    fmt("~!WExpression produces a value of type ~!!~s~!W,"
        " but this value is unmatched\n", [Type]);
message_to_string({unused_fun, [F, A]}) ->
    fmt("~!WFunction ~!r~w/~w~!W will never be called\n", [F, A]);
%%----- Warnings for specs and contracts -------------------
message_to_string({contract_diff, [M, F, _A, Contract, Sig]}) ->
    fmt("~!WType specification ~!!~w:~w~s~!W"
        " is not equal to the success typing: ~!!~w:~w~s\n",
        [M, F, Contract, M, F, Sig]);
message_to_string({contract_subtype, [M, F, _A, Contract, Sig]}) ->
    fmt("~!WType specification ~!!~w:~w~s~!W"
        " is a subtype of the success typing: ~!!~w:~w~s\n",
        [M, F, Contract, M, F, Sig]);
message_to_string({contract_supertype, [M, F, _A, Contract, Sig]}) ->
    fmt("~!WType specification ~!!~w:~w~s~!W"
        " is a supertype of the success typing: ~!!~w:~w~s\n",
        [M, F, Contract, M, F, Sig]);
message_to_string({contract_range, [Contract, M, F, ArgStrings, Line, CRet]}) ->
    fmt("~!WThe contract ~!!~w:~w~s~!W cannot be right because the"
        " inferred return for ~!!~w~s~!W on line ~!!~w~!W is ~!!~s\n",
        [M, F, Contract, F, ArgStrings, Line, CRet]);
message_to_string({invalid_contract, [M, F, A, Sig]}) ->
    fmt("~!WInvalid type specification for function~!! ~w:~w/~w."
        "~!W The success typing is~!! ~s\n", [M, F, A, Sig]);
message_to_string({extra_range, [M, F, A, ExtraRanges, SigRange]}) ->
    fmt("~!WThe specification for ~!!~w:~w/~w~!W states that the function"
        " might also return ~!!~s~!W but the inferred return is ~!!~s\n",
        [M, F, A, ExtraRanges, SigRange]);
message_to_string({overlapping_contract, [M, F, A]}) ->
    fmt("~!WOverloaded contract for ~!!~w:~w/~w~!W has overlapping"
        " domains; such contracts are currently unsupported and are simply "
        "ignored\n", [M, F, A]);
message_to_string({spec_missing_fun, [M, F, A]}) ->
    fmt("~!WContract for function that does not exist: ~!!~w:~w/~w\n",
        [M, F, A]);
%%----- Warnings for opaque type violations -------------------
message_to_string({call_with_opaque, [M, F, Args, ArgNs, ExpArgs]}) ->
    fmt("~!WThe call ~!!~w:~w~s~!W contains ~!!~s~!W when ~!!~s\n",
        [M, F, Args, form_positions(ArgNs), form_expected(ExpArgs)]);
message_to_string({call_without_opaque, [M, F, Args, [{N,_,_}|_] = ExpectedTriples]}) ->
    fmt("~!WThe call ~!!~w:~w~s ~!Wdoes not have~!! ~s\n",
        [M, F, bad_arg(N, Args), form_expected_without_opaque(ExpectedTriples)]);
message_to_string({opaque_eq, [Type, _Op, OpaqueType]}) ->
    fmt("~!WAttempt to test for equality between a term of type ~!!~s~!W"
        " and a term of opaque type ~!!~s\n", [Type, OpaqueType]);
message_to_string({opaque_guard, [Arg1, Infix, Arg2, ArgNs]}) ->
    fmt("~!WGuard test ~!!~s ~s ~s~!W contains ~!!~s\n",
        [Arg1, Infix, Arg2, form_positions(ArgNs)]);
message_to_string({opaque_guard, [Guard, Args]}) ->
    fmt("~!WGuard test ~!!~w~s~!W breaks the opaqueness of its"
        " argument\n", [Guard, Args]);
message_to_string({opaque_match, [Pat, OpaqueType, OpaqueTerm]}) ->
    Term = if OpaqueType =:= OpaqueTerm -> "the term";
              true -> OpaqueTerm
           end,
    fmt("~!WThe attempt to match a term of type ~!!~s~!W against the"
        "~!! ~s~!W breaks the opaqueness of ~!!~s\n",
        [OpaqueType, Pat, Term]);
message_to_string({opaque_neq, [Type, _Op, OpaqueType]}) ->
    fmt("~!WAttempt to test for inequality between a term of type ~!!~s"
        "~!W and a term of opaque type ~!!~s\n", [Type, OpaqueType]);
message_to_string({opaque_type_test, [Fun, Args, Arg, ArgType]}) ->
    fmt("~!WThe type test ~!!~s~s~!W breaks the opaqueness of the term "
        "~!!~s~s\n", [Fun, Args, Arg, ArgType]);
message_to_string({opaque_size, [SizeType, Size]}) ->
    fmt("~!WThe size ~!!~s~!W breaks the opaqueness of ~!!~s\n",
        [SizeType, Size]);
message_to_string({opaque_call, [M, F, Args, Culprit, OpaqueType]}) ->
    fmt("~!WThe call ~!!~s:~s~s~!W breaks the opaqueness of the term~!!"
        " ~s :: ~s\n", [M, F, Args, Culprit, OpaqueType]);
%%----- Warnings for concurrency errors --------------------
message_to_string({race_condition, [M, F, Args, Reason]}) ->
    fmt("~!WThe call ~!!~w:~w~s ~s\n", [M, F, Args, Reason]);
%%----- Warnings for behaviour errors --------------------
message_to_string({callback_type_mismatch, [B, F, A, ST, CT]}) ->
    fmt("~!WThe inferred return type of~!! ~w/~w (~s) ~!W"
        "has nothing in common with~!! ~s, ~!Wwhich is the expected"
        " return type for the callback of~!! ~w ~!Wbehaviour\n",
        [F, A, ST, CT, B]);
message_to_string({callback_arg_type_mismatch, [B, F, A, N, ST, CT]}) ->
    fmt("~!WThe inferred type for the~!! ~s ~!Wargument of~!!"
        " ~w/~w (~s) ~!Wis not a supertype of~!! ~s~!W, which is"
        "expected type for this argument in the callback of the~!! ~w "
        "~!Wbehaviour\n",
        [ordinal(N), F, A, ST, CT, B]);
message_to_string({callback_spec_type_mismatch, [B, F, A, ST, CT]}) ->
    fmt("~!WThe return type ~!!~s~!W in the specification of ~!!"
        "~w/~w~!W is not a subtype of ~!!~s~!W, which is the expected"
        " return type for the callback of ~!!~w~!W behaviour\n",
        [ST, F, A, CT, B]);
message_to_string({callback_spec_arg_type_mismatch, [B, F, A, N, ST, CT]}) ->
    fmt("~!WThe specified type for the ~!!~s~!W argument of ~!!"
        "~w/~w (~s)~!W is not a supertype of ~!!~s~!W, which is"
        " expected type for this argument in the callback of the ~!!~w"
        "~!W behaviour\n", [ordinal(N), F, A, ST, CT, B]);
message_to_string({callback_missing, [B, F, A]}) ->
    fmt("~!WUndefined callback function ~!!~w/~w~!W (behaviour ~!!"
        "'~w'~!W)\n",[F, A, B]);
message_to_string({callback_info_missing, [B]}) ->
    fmt("~!WCallback info about the ~!r~w~!W"
        " behaviour is not available\n", [B]);
%%----- Warnings for unknown functions, types, and behaviours -------------
message_to_string({unknown_type, {M, F, A}}) ->
    fmt("~!WUnknown type ~!r~w:~w/~w", [M, F, A]);
message_to_string({unknown_function, {M, F, A}}) ->
    fmt("~!WUnknown function ~!r~w:~w/~w", [M, F, A]);
message_to_string({unknown_behaviour, B}) ->
    fmt("~!WUnknown behaviour ~!r~w", [B]).

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
                    fmt("~!Wwill never return since the success typing arguments"
                        " are ~!!~s\n", [SigArgs]);
                false ->
                    fmt("~!Wwill never return since it differs in the~!!"
                        " ~s ~!Wargument from the success typing"
                        " arguments:~!! ~s\n",
                        [PositionString, good_arg(ArgNs, SigArgs)])
            end;
        only_contract ->
            case (ArgNs =:= []) orelse IsOverloaded of
                true ->
                    %% We do not know which arguments caused the failure
                    fmt("~!Wbreaks the contract~!! ~s\n", [good_arg(ArgNs, Contract)]);
                false ->
                    fmt("~!Wbreaks the contract~!! ~s ~!Win the~!!"
                        " ~s ~!Wargument\n",
                        [good_arg(ArgNs, Contract), PositionString])
            end;
        both ->
            fmt("~!Wwill never return since the success typing is "
                "~!!~s ~!W->~!! ~s ~!Wand the contract is ~!!~s\n",
                [good_arg(ArgNs, SigArgs), SigRet,
                 good_arg(ArgNs, Contract)])
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
    FStr = case erl_types:t_is_opaque(T) of
               true  ->
                   "~!Wan opaque term of type~!g ~s ~!Was ";
               false ->
                   "~!Wa term of type ~!g~s ~!W(with opaque subterms) as "
           end ++ form_position_string([N]) ++ "~!W argument",
    fmt(FStr, [TStr]);

form_expected_without_opaque(ExpectedTriples) -> %% TODO: can do much better here
    {ArgNs, _Ts, _TStrs} = lists:unzip3(ExpectedTriples),
    "opaque terms as " ++ form_position_string(ArgNs) ++ " arguments".

form_expected(ExpectedArgs) ->
    case ExpectedArgs of
        [T] ->
            TS = erl_types:t_to_string(T),
            case erl_types:t_is_opaque(T) of
                true  -> fmt("~!Wan opaque term of type ~!!~s~!W is"
                             " expected", [TS]);
                false -> fmt("~!Wa structured term of type ~!!~s~!W is"
                             " expected", [TS])
            end;
        [_,_|_] -> fmt("~!Wterms of different types are expected in these"
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


bad_pat("pattern " ++ P) ->
    fmt("pattern ~!r~s",[P]);
bad_pat("variable " ++ P) ->
    fmt("variable ~!r~s",[P]);
bad_pat(P) ->
    fmt("~!r~s",[P]).


bad_arg(N, Args) ->
    color_arg(N, r, Args).

good_arg(N, Args) ->
    color_arg(N, g, Args).

color_arg(N, C, Args) when is_integer(N) ->
    color_arg([N], C, Args);
color_arg(Ns, C, Args) ->
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
