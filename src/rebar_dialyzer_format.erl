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
%%%          and error is.
%%% * green: the 'good' stuff, i.e. what was expected as an argument
%%%          the 'red vs green' resambles the diff view 'remove vs add'
%%% * blue:  argument positions.
-module(rebar_dialyzer_format).

-include("rebar.hrl").

-export([format_warnings/2]).

%% Formats a list of warnings in a nice per file way. Note that we reverse
%% the list at the end to 'undo' the reversal by foldl
format_warnings(Opts, Warnings) ->
    Fold = fun(Warning, Acc) -> format_warning_(Opts, Warning, Acc) end,
    {_, Res} = lists:foldl(Fold, {undefined, []}, Warnings),
    lists:reverse(Res).


%% If the last seen file is and the file of this warning are the same

%% `dialyzer_cl` returns _Filename = "", _Line = 0 for `unknown` (prior to OTP 24)
format_warning_(_Opts, Warning = {_Tag, {"" = _SrcFile0, 0 = Line}, Msg}, {_LastFile, Acc}) ->
    SrcFile = "<path unknown>",
    try
        F = fmt("~!_c~ts", [SrcFile]),
        String = message_to_string(Msg),
        {SrcFile, [lists:flatten(fmt("~n~ts~n~!c~4w~!!: ~ts", [F, Line, String])) | Acc]}
    catch
        ?WITH_STACKTRACE(Error, Reason, Stacktrace)
            ?DEBUG("Failed to pretty format warning: ~p:~p~n~p",
                   [Error, Reason, Stacktrace]),
            {SrcFile, [dialyzer:format_warning(Warning, fullpath) | Acc]}
    end;

%% we skip the file header
format_warning_(_Opts, Warning = {_Tag, {File, Line}, Msg}, {File, Acc}) ->
    try
        String = message_to_string(Msg),
        {Fmt, Args} = file_location_warning(no_file, Line, String),
        {File, [lists:flatten(fmt(Fmt, Args)) | Acc]}
    catch
        Error:Reason ->
            ?DEBUG("Failed to pretty format warning: ~p:~p",
                   [Error, Reason]),
            {File, [dialyzer:format_warning(Warning, fullpath) | Acc]}
    end;

%% With a new file detected we also write a file header.
format_warning_(Opts, Warning = {_Tag, {SrcFile, Line}, Msg}, {_LastFile, Acc}) ->
    try
        File = rebar_dir:format_source_file_name(SrcFile, Opts),
        Base = filename:basename(File),
        Dir = filename:dirname(File),
        Root = filename:rootname(Base),
        Ext = filename:extension(Base),
        Path = re:replace(Dir, "^.*/_build/", "_build/", [{return, list}, unicode]),
        Base1 = fmt("~!_c~ts~!!~!__~ts", [Root, Ext]),
        F = fmt("~!__~ts", [filename:join(Path, Base1)]),
        String = message_to_string(Msg),
        {Fmt, Args} = file_location_warning(F, Line, String),
        {SrcFile, [lists:flatten(fmt(Fmt, Args)) | Acc]}
    catch
        ?WITH_STACKTRACE(Error, Reason, Stacktrace)
            ?DEBUG("Failed to pretty format warning: ~p:~p~n~p",
                   [Error, Reason, Stacktrace]),
            {SrcFile, [dialyzer:format_warning(Warning, fullpath) | Acc]}
    end.

fmt(Fmt) ->
    cf:format(Fmt, []).
fmt(Fmt, Args) ->
    cf:format(Fmt, Args).

file_location_warning(no_file, {Line, Col}, String) ->
    {"Line ~!c~w~!! Column ~!c~w~!!: ~ts", [Line, Col, String]};
file_location_warning(F, {Line, Col}, String) ->
    {"~n~ts~nLine ~!c~w~!! Column ~!c~w~!!: ~ts", [F, Line, Col, String]};
file_location_warning(no_file, Line, String) ->
    {"Line ~!c~w~!!: ~ts", [Line, String]};
file_location_warning(F, Line, String) ->
    {"~n~ts~nLine ~!c~w~!!: ~ts", [F, Line, String]}.

%%-----------------------------------------------------------------------------
%% Message classification and pretty-printing below. Messages appear in
%% categories and in more or less alphabetical ordering within each category.
%%-----------------------------------------------------------------------------

%%----- Warnings for general discrepancies ----------------
message_to_string({apply, [Args, ArgNs, FailReason,
                           SigArgs, SigRet, Contract]}) ->
    fmt("~!^Fun application with arguments ~!!~ts ",
        [bad_arg(ArgNs, Args)]) ++
        call_or_apply_to_string(ArgNs, FailReason, SigArgs, SigRet, Contract);
message_to_string({app_call, [M, F, Args, Culprit, ExpectedType, FoundType]}) ->
    fmt("~!^The call~!! ~ts:~ts~ts ~!^requires that"
        "~!! ~ts ~!^is of type ~!g~ts~!^ not ~!r~ts",
        [M, F, Args, Culprit, ExpectedType, FoundType]);
message_to_string({bin_construction, [Culprit, Size, Seg, Type]}) ->
    fmt("~!^Binary construction will fail since the ~!b~ts~!^ field~!!"
        " ~ts~!^ in segment~!! ~ts~!^ has type~!! ~ts",
        [Culprit, Size, Seg, Type]);
message_to_string({call, [M, F, Args, ArgNs, FailReason,
                          SigArgs, SigRet, Contract]}) ->
    fmt("~!^The call~!! ~w:~w~ts ", [M, F, bad_arg(ArgNs, Args)]) ++
        call_or_apply_to_string(ArgNs, FailReason, SigArgs, SigRet, Contract);
message_to_string({call_to_missing, [M, F, A]}) ->
    fmt("~!^Call to missing or unexported function ~!!~w:~w/~w",
        [M, F, A]);
message_to_string({exact_eq, [Type1, Op, Type2]}) ->
    fmt("~!^The test ~!!~ts ~ts ~ts~!^ can never evaluate to 'true'",
        [Type1, Op, Type2]);
message_to_string({fun_app_args, [Args, Type]}) ->
    fmt("~!^Fun application with arguments ~!!~ts~!^ will fail"
        " since the function has type ~!!~ts", [Args, Type]);
message_to_string({fun_app_no_fun, [Op, Type, Arity]}) ->
    fmt("~!^Fun application will fail since ~!!~ts ~!^::~!! ~ts"
        " is not a function of arity ~!!~w", [Op, Type, Arity]);
message_to_string({guard_fail, []}) ->
    "~!^Clause guard cannot succeed.~!!";
message_to_string({guard_fail, [Arg1, Infix, Arg2]}) ->
    fmt("~!^Guard test ~!!~ts ~ts ~ts~!^ can never succeed",
        [Arg1, Infix, Arg2]);
message_to_string({neg_guard_fail, [Arg1, Infix, Arg2]}) ->
    fmt("~!^Guard test not(~!!~ts ~ts ~ts~!^) can never succeed",
        [Arg1, Infix, Arg2]);
message_to_string({guard_fail, [Guard, Args]}) ->
    fmt("~!^Guard test ~!!~w~ts~!^ can never succeed",
        [Guard, Args]);
message_to_string({neg_guard_fail, [Guard, Args]}) ->
    fmt("~!^Guard test not(~!!~w~ts~!^) can never succeed",
        [Guard, Args]);
message_to_string({guard_fail_pat, [Pat, Type]}) ->
    fmt("~!^Clause guard cannot succeed. The ~!!~ts~!^ was matched"
        " against the type ~!!~ts", [Pat, Type]);
message_to_string({improper_list_constr, [TlType]}) ->
    fmt("~!^Cons will produce an improper list"
        " since its ~!b2~!!nd~!^ argument is~!! ~ts", [TlType]);
message_to_string({no_return, [Type|Name]}) ->
    NameString =
        case Name of
            [] -> fmt("~!^The created fun ");
            [F, A] -> fmt("~!^Function ~!r~w/~w ", [F, A])
        end,
    case Type of
        no_match -> fmt("~ts~!^has no clauses that will ever match",[NameString]);
        only_explicit -> fmt("~ts~!^only terminates with explicit exception", [NameString]);
        only_normal -> fmt("~ts~!^has no local return", [NameString]);
        both -> fmt("~ts~!^has no local return", [NameString])
    end;
message_to_string({record_constr, [RecConstr, FieldDiffs]}) ->
    fmt("~!^Record construction ~!!~ts~!^ violates the"
        " declared type of field ~!!~ts", [RecConstr, FieldDiffs]);
message_to_string({record_constr, [Name, Field, Type]}) ->
    fmt("~!^Record construction violates the declared type for ~!!#~w{}~!^"
        " since ~!!~ts~!^ cannot be of type ~!!~ts",
        [Name, Field, Type]);
message_to_string({record_matching, [String, Name]}) ->
    fmt("~!^The ~!!~ts~!^ violates the"
        " declared type for ~!!#~w{}", [String, Name]);
message_to_string({record_match, [Pat, Type]}) ->
    fmt("~!^Matching of ~!!~ts~!^ tagged with a record name violates the"
        " declared type of ~!!~ts", [Pat, Type]);
message_to_string({pattern_match, [Pat, Type]}) ->
    fmt("~!^The ~ts~!^ can never match the type ~!g~ts",
        [bad_pat(Pat), Type]);
message_to_string({pattern_match_cov, [Pat, Type]}) ->
    fmt("~!^The ~ts~!^ can never match since previous"
        " clauses completely covered the type ~!g~ts",
        [bad_pat(Pat), Type]);
message_to_string({unmatched_return, [Type]}) ->
    fmt("~!^Expression produces a value of type ~!!~ts~!^,"
        " but this value is unmatched", [Type]);
message_to_string({unused_fun, [F, A]}) ->
    fmt("~!^Function ~!r~w/~w~!!~!^ will never be called", [F, A]);
%%----- Warnings for specs and contracts -------------------
message_to_string({contract_diff, [M, F, _A, Contract, Sig]}) ->
    fmt("~!^Type specification ~!!~w:~w~ts~!^"
        " is not equal to the success typing: ~!!~w:~w~ts",
        [M, F, Contract, M, F, Sig]);
message_to_string({contract_subtype, [M, F, _A, Contract, Sig]}) ->
    fmt("~!^Type specification ~!!~w:~w~ts~!^"
        " is a subtype of the success typing: ~!!~w:~w~ts",
        [M, F, Contract, M, F, Sig]);
message_to_string({contract_supertype, [M, F, _A, Contract, Sig]}) ->
    fmt("~!^Type specification ~!!~w:~w~ts~!^"
        " is a supertype of the success typing: ~!!~w:~w~ts",
        [M, F, Contract, M, F, Sig]);
message_to_string({contract_range, [Contract, M, F, ArgStrings, Line, CRet]}) ->
    fmt("~!^The contract ~!!~w:~w~ts~!^ cannot be right because the"
        " inferred return for ~!!~w~ts~!^ on line ~!!~w~!^ is ~!!~ts",
        [M, F, Contract, F, ArgStrings, Line, CRet]);
message_to_string({invalid_contract, [M, F, A, Sig]}) ->
    fmt("~!^Invalid type specification for function~!! ~w:~w/~w."
        "~!^ The success typing is~!! ~ts", [M, F, A, Sig]);
message_to_string({extra_range, [M, F, A, ExtraRanges, SigRange]}) ->
    fmt("~!^The specification for ~!!~w:~w/~w~!^ states that the function"
        " might also return ~!!~ts~!^ but the inferred return is ~!!~ts",
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
    fmt("~!^The call ~!!~w:~w~ts~!^ contains ~!!~ts~!^ when ~!!~ts",
        [M, F, bad_arg(ArgNs, Args), form_positions(ArgNs), form_expected(ExpArgs)]);
message_to_string({call_without_opaque, [M, F, Args, [{N,_,_}|_] = ExpectedTriples]}) ->
    fmt("~!^The call ~!!~w:~w~ts ~!^does not have~!! ~ts",
        [M, F, bad_arg(N, Args), form_expected_without_opaque(ExpectedTriples)]);
message_to_string({opaque_eq, [Type, _Op, OpaqueType]}) ->
    fmt("~!^Attempt to test for equality between a term of type ~!!~ts~!^"
        " and a term of opaque type ~!!~ts", [Type, OpaqueType]);
message_to_string({opaque_guard, [Arg1, Infix, Arg2, ArgNs]}) ->
    fmt("~!^Guard test ~!!~ts ~ts ~ts~!^ contains ~!!~ts",
        [Arg1, Infix, Arg2, form_positions(ArgNs)]);
message_to_string({opaque_guard, [Guard, Args]}) ->
    fmt("~!^Guard test ~!!~w~ts~!^ breaks the opaqueness of its"
        " argument", [Guard, Args]);
message_to_string({opaque_match, [Pat, OpaqueType, OpaqueTerm]}) ->
    Term = if OpaqueType =:= OpaqueTerm -> "the term";
              true -> OpaqueTerm
           end,
    fmt("~!^The attempt to match a term of type ~!!~ts~!^ against the"
        "~!! ~ts~!^ breaks the opaqueness of ~!!~ts",
        [OpaqueType, Pat, Term]);
message_to_string({opaque_neq, [Type, _Op, OpaqueType]}) ->
    fmt("~!^Attempt to test for inequality between a term of type ~!!~ts"
        "~!^ and a term of opaque type ~!!~ts", [Type, OpaqueType]);
message_to_string({opaque_type_test, [Fun, Args, Arg, ArgType]}) ->
    fmt("~!^The type test ~!!~ts~ts~!^ breaks the opaqueness of the term "
        "~!!~ts~ts", [Fun, Args, Arg, ArgType]);
message_to_string({opaque_size, [SizeType, Size]}) ->
    fmt("~!^The size ~!!~ts~!^ breaks the opaqueness of ~!!~ts",
        [SizeType, Size]);
message_to_string({opaque_call, [M, F, Args, Culprit, OpaqueType]}) ->
    fmt("~!^The call ~!!~ts:~ts~ts~!^ breaks the opaqueness of the term~!!"
        " ~ts :: ~ts", [M, F, Args, Culprit, OpaqueType]);
%%----- Warnings for concurrency errors --------------------
message_to_string({race_condition, [M, F, Args, Reason]}) ->
    fmt("~!^The call ~!!~w:~w~ts ~ts", [M, F, Args, Reason]);
%%----- Warnings for behaviour errors --------------------
message_to_string({callback_type_mismatch, [B, F, A, ST, CT]}) ->
    fmt("~!^The inferred return type of~!! ~w/~w (~ts) ~!^"
        "has nothing in common with~!! ~ts, ~!^which is the expected"
        " return type for the callback of~!! ~w ~!^behaviour",
        [F, A, ST, CT, B]);
message_to_string({callback_arg_type_mismatch, [B, F, A, N, ST, CT]}) ->
    fmt("~!^The inferred type for the~!! ~ts ~!^argument of~!!"
        " ~w/~w (~ts) ~!^is not a supertype of~!! ~ts~!^, which is"
        "expected type for this argument in the callback of the~!! ~w "
        "~!^behaviour",
        [ordinal(N), F, A, ST, CT, B]);
message_to_string({callback_spec_type_mismatch, [B, F, A, ST, CT]}) ->
    fmt("~!^The return type ~!!~ts~!^ in the specification of ~!!"
        "~w/~w~!^ is not a subtype of ~!!~ts~!^, which is the expected"
        " return type for the callback of ~!!~w~!^ behaviour",
        [ST, F, A, CT, B]);
message_to_string({callback_spec_arg_type_mismatch, [B, F, A, N, ST, CT]}) ->
    fmt("~!^The specified type for the ~!!~ts~!^ argument of ~!!"
        "~w/~w (~ts)~!^ is not a supertype of ~!!~ts~!^, which is"
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
                        " are ~!!~ts", [SigArgs]);
                false ->
                    fmt("~!^will never return since it differs in the~!!"
                        " ~ts ~!^argument from the success typing"
                        " arguments:~!! ~ts",
                        [PositionString, good_arg(ArgNs, SigArgs)])
            end;
        only_contract ->
            case (ArgNs =:= []) orelse IsOverloaded of
                true ->
                    %% We do not know which arguments caused the failure
                    fmt("~!^breaks the contract~!! ~ts", [good_arg(ArgNs, Contract)]);
                false ->
                    fmt("~!^breaks the contract~!! ~ts ~!^in the~!!"
                        " ~ts ~!^argument",
                        [good_arg(ArgNs, Contract), PositionString])
            end;
        both ->
            fmt("~!^will never return since the success typing is "
                "~!!~ts ~!^->~!! ~ts ~!^and the contract is ~!!~ts",
                [good_arg(ArgNs, SigArgs), SigRet,
                 good_arg(ArgNs, Contract)])
    end.

form_positions(ArgNs) ->
    ArgS = form_position_string(ArgNs),
    case ArgNs of
        [_] ->     fmt("~!^an opaque term as ~!!~ts~!^ argument", [ArgS]);
        [_,_|_] -> fmt("~!^opaque terms as ~!!~ts~!^ arguments", [ArgS])
    end.

%% We know which positions N are to blame;
%% the list of triples will never be empty.
form_expected_without_opaque([{N, T, TStr}]) ->
    FStr = case erl_types:t_is_opaque(T) of
               true  ->
                   "~!^an opaque term of type~!g ~ts ~!^as ";
               false ->
                   "~!^a term of type ~!g~ts ~!^(with opaque subterms) as "
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
                true  -> fmt("~!^an opaque term of type ~!!~ts~!^ is"
                             " expected", [TS]);
                false -> fmt("~!^a structured term of type ~!!~ts~!^ is"
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
            ", " ++ Head = lists:flatten([fmt(", ~ts",[ordinal(N)]) ||
                                             N <- lists:reverse(Prevs)]),
            Head ++ " and " ++ ordinal(Last)
    end.

ordinal(1) -> fmt("~!B1~!!st");
ordinal(2) -> fmt("~!B2~!!nd");
ordinal(3) -> fmt("~!B3~!!rd");
ordinal(N) when is_integer(N) -> fmt("~!B~w~!!th", [N]).

%% Format a pattern ad highlight errorous part in red.
bad_pat("pattern " ++ P) ->
    fmt("pattern ~!r~ts",[P]);
bad_pat("variable " ++ P) ->
    fmt("variable ~!r~ts",[P]);
bad_pat(P) ->
    fmt("~!r~ts",[P]).


bad_arg(N, Args) ->
    colour_arg(N, r, Args).

good_arg(N, Args) ->
    colour_arg(N, g, Args).

%% colour one or more arg of an argument list, this unparses the args
%% highlights one or more of them and puts them back together.
colour_arg(N, C, Args) when is_integer(N) ->
    colour_arg([N], C, Args);
colour_arg(Ns, C, Args) ->
    {Args1, Rest} =separate_args(Args),
    Args2 = highlight(Ns, 1, C, Args1),
    join_args(Args2) ++ Rest.

highlight([], _N, _C, Rest) ->
    Rest;

highlight([N | Nr], N, g, [Arg | Rest]) ->
    [fmt("~!g~ts", [Arg]) | highlight(Nr, N+1, g, Rest)];

highlight([N | Nr], N, r, [Arg | Rest]) ->
    [fmt("~!r~ts", [Arg]) | highlight(Nr, N+1, r, Rest)];

highlight(Ns, N, C, [Arg | Rest]) ->
    [Arg | highlight(Ns, N + 1, C, Rest)].

%% Arguments to functions and constraints are passed as
%% strings not as data, this function pulls them apart
%% to allow interacting with them separately and not
%% as one bug chunk of data.
separate_args([$( | S]) ->
    separate_args([], S, "", []).

%% We strip this space since dialyzer is inconsistent in adding or not adding
%% it ....
separate_args([], [$,, $\s | R], Arg, Args) ->
    separate_args([], R, [], [lists:reverse(Arg) | Args]);

separate_args([], [$, | R], Arg, Args) ->
    separate_args([], R, [], [lists:reverse(Arg) | Args]);

separate_args([], [$) | Rest], Arg, Args) ->
    {lists:reverse([lists:reverse(Arg) | Args]), Rest};
separate_args([C | D], [C | R], Arg, Args) ->
    separate_args(D, R, [C | Arg], Args);
%% Brackets
separate_args(D, [${ | R], Arg, Args) ->
    separate_args([$}|D], R, [${ | Arg], Args);

separate_args(D, [$( | R], Arg, Args) ->
    separate_args([$)|D], R, [$( | Arg], Args);

separate_args(D, [$[ | R], Arg, Args) ->
    separate_args([$]|D], R, [$[ | Arg], Args);

separate_args(D, [$< | R], Arg, Args) ->
    separate_args([$>|D], R, [$< | Arg], Args);
%% 'strings'
separate_args(D, [$' | R], Arg, Args) ->
    separate_args([$'|D], R, [$' | Arg], Args);
separate_args(D, [$" | R], Arg, Args) ->
    separate_args([$"|D], R, [$" | Arg], Args);

separate_args(D, [C | R], Arg, Args) ->
    separate_args(D, R, [C | Arg], Args).

join_args(Args) ->
    [$(, rebar_string:join(Args, ", "), $)].
