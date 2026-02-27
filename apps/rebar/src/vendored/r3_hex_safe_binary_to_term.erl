%% Vendored from hex_core v0.12.1, do not edit manually

%% @hidden
%% Safe deserialization of Erlang terms from binary.
%%
%% This module provides a restricted version of `binary_to_term/1' that:
%% - Uses the `safe' option to prevent creation of new atoms (DoS protection)
%% - Validates that the term contains no executable code (RCE protection)
%%
%% Inspired by Plug.Crypto's non_executable_binary_to_term:
%% https://github.com/elixir-plug/plug_crypto/blob/c326c3c743b18cf5f4b12735d06dd90c72dcd779/lib/plug/crypto.ex
-module(r3_hex_safe_binary_to_term).

-export([safe_binary_to_term/1]).

-type unsafe_term() :: function() | port().
-type error_reason() :: invalid_term | {unsafe_term, unsafe_term()}.

-spec safe_binary_to_term(binary()) -> {ok, term()} | {error, error_reason()}.
safe_binary_to_term(Binary) when is_binary(Binary) ->
    try binary_to_term(Binary, [safe]) of
        Term ->
            case validate_term(Term) of
                ok -> {ok, Term};
                {error, _} = Error -> Error
            end
    catch
        error:badarg ->
            {error, invalid_term}
    end.

-spec validate_term(term()) -> ok | {error, {unsafe_term, term()}}.
validate_term(Term) when is_list(Term) ->
    validate_list(Term);
validate_term(Term) when is_tuple(Term) ->
    validate_tuple(Term, tuple_size(Term));
validate_term(Term) when is_map(Term) ->
    validate_map(Term);
validate_term(Term) when
    is_atom(Term);
    is_number(Term);
    is_bitstring(Term);
    is_pid(Term);
    is_reference(Term)
->
    ok;
validate_term(Term) ->
    {error, {unsafe_term, Term}}.

-spec validate_list(list()) -> ok | {error, {unsafe_term, term()}}.
validate_list([]) ->
    ok;
validate_list([H | T]) when is_list(T) ->
    case validate_term(H) of
        ok -> validate_list(T);
        Error -> Error
    end;
validate_list([H | T]) ->
    %% Improper list
    case validate_term(H) of
        ok -> validate_term(T);
        Error -> Error
    end.

-spec validate_tuple(tuple(), non_neg_integer()) -> ok | {error, {unsafe_term, term()}}.
validate_tuple(_Tuple, 0) ->
    ok;
validate_tuple(Tuple, N) ->
    case validate_term(element(N, Tuple)) of
        ok -> validate_tuple(Tuple, N - 1);
        Error -> Error
    end.

-spec validate_map(map()) -> ok | {error, {unsafe_term, term()}}.
validate_map(Map) ->
    try
        maps:fold(
            fun(Key, Value, ok) ->
                case validate_term(Key) of
                    ok ->
                        case validate_term(Value) of
                            ok -> ok;
                            Error -> throw(Error)
                        end;
                    Error ->
                        throw(Error)
                end
            end,
            ok,
            Map
        )
    catch
        throw:{error, _} = Error -> Error
    end.
