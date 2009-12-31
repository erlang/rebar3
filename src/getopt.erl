%%%-------------------------------------------------------------------
%%% @author Juan Jose Comellas <juanjo@comellas.org>
%%% @copyright (C) 2009 Juan Jose Comellas
%%% @doc Parses command line options with a format similar to that of GNU getopt.
%%% @end
%%%
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------
-module(getopt).
-author('juanjo@comellas.org').

-export([parse/2, usage/2]).


-define(TAB_LENGTH, 8).
%% Indentation of the help messages in number of tabs.
-define(INDENTATION, 3).

-define(OPT_NAME, 1).
-define(OPT_SHORT, 2).
-define(OPT_LONG, 3).
-define(OPT_ARG, 4).
-define(OPT_HELP, 5).

-define(IS_OPT_SPEC(Opt), (is_tuple(Opt) andalso (size(Opt) =:= ?OPT_HELP))).


%% @type arg_type() = 'atom' | 'binary' | 'bool' | 'float' | 'integer' | 'string'.
%% Atom indicating the data type that an argument can be converted to.
-type arg_type() :: 'atom' | 'binary' | 'boolean' | 'float' | 'integer' | 'string'.
%% @type arg_value() = atom() | binary() | bool() | float() | integer() | string().
%% Data type that an argument can be converted to.
-type arg_value() :: atom() | binary() | boolean() | float() | integer() | string().
%% @type arg_spec() = arg_type() | {arg_type(), arg_value()} | undefined.
%% Argument specification.
-type arg_spec() :: arg_type() | {arg_type(), arg_value()} | undefined.
%% @type option() = atom() | {atom(), arg_value()}. Option type and optional default argument.
-type option() :: atom() | {atom(), arg_value()}.
%% @type option_spec() = #option{}. Command line option specification.
-type option_spec() :: {
                   Name    :: atom(),
                   Short   :: char() | undefined,
                   Long    :: string() | undefined,
                   ArgSpec :: arg_spec(),
                   Help    :: string() | undefined
                  }.


-spec parse([option_spec()], string() | [string()]) -> {ok, {[option()], [string()]}} | {error, {Reason :: atom(), Data :: any()}}.
%%--------------------------------------------------------------------
%% @spec parse(OptSpecList::[option_spec()], Args::string() | [string()]) -> [option()].
%% @doc  Parse the command line options and arguments returning a list of tuples
%%       and/or atoms using the Erlang convention for sending options to a
%%       function.
%%--------------------------------------------------------------------
parse(OptSpecList, CmdLine) ->
    try
        Args = if
                   is_integer(hd(CmdLine)) ->
                       string:tokens(CmdLine, " \t\n");
                   true ->
                       CmdLine
               end,
        parse(OptSpecList, [], [], 0, Args)
    catch
        throw: {error, {_Reason, _Data}} = Error ->
            Error
    end.

-spec parse([option_spec()], [option()], [string()], integer(), [string()]) ->
    {ok, {[option()], [string()]}} | {error, {Reason :: atom(), Data:: any()}}.
%% Process long options.
parse(OptSpecList, OptAcc, ArgAcc, ArgPos, [[$-, $- | Name] = OptStr | Tail]) ->
    {Option, Tail1} = 
        case split_embedded_arg(Name) of
            {Name1, Arg} ->
                %% Get option that has its argument within the same string
                %% separated by an equal ('=') character.
                {get_option_embedded_arg(OptSpecList, OptStr, ?OPT_LONG, Name1, Arg), Tail};
            _Name1 ->
                get_option(OptSpecList, OptStr, ?OPT_LONG, Name, Tail)
        end,
    parse(OptSpecList, [Option | OptAcc], ArgAcc, ArgPos, Tail1);
%% Process short options.
parse(OptSpecList, OptAcc, ArgAcc, ArgPos, [[$-, ShortName] = OptStr | Tail]) ->
    {Option, Tail1} = get_option(OptSpecList, OptStr, ?OPT_SHORT, ShortName, Tail),
    parse(OptSpecList, [Option | OptAcc], ArgAcc, ArgPos, Tail1);
%% Process multiple short options with no argument.
parse(OptSpecList, OptAcc, ArgAcc, ArgPos, [[$- | ShortNameList] = OptStr | Tail]) ->
    NewOptAcc =
        lists:foldl(
          fun (ShortName, OptAcc1) ->
                  [get_option_no_arg(OptSpecList, OptStr, ShortName, ?OPT_SHORT) | OptAcc1]
          end, OptAcc, ShortNameList),
    parse(OptSpecList, NewOptAcc, ArgAcc, ArgPos, Tail);
%% Process non-option arguments.
parse(OptSpecList, OptAcc, ArgAcc, ArgPos, [OptStr | Tail]) ->
    case split_embedded_arg(OptStr) of
        {Name, Arg} ->
            %% Get option that has its argument within the same string
            %% separated by an equal ('=') character.
            parse(OptSpecList, [get_option_embedded_arg(OptSpecList, OptStr, ?OPT_LONG, Name, Arg) | OptAcc],
                  ArgAcc, ArgPos, Tail);
        Arg ->
            case find_non_option_arg(OptSpecList, ArgPos) of
                {value, OptSpec} when ?IS_OPT_SPEC(OptSpec) ->
                    parse(OptSpecList, [convert_option_arg(OptSpec, Arg) | OptAcc],
                          ArgAcc, ArgPos + 1, Tail);
                false ->
                    parse(OptSpecList, OptAcc, [Arg | ArgAcc], ArgPos, Tail)
            end
    end;
parse(OptSpecList, OptAcc, ArgAcc, _ArgPos, []) ->
    %% Once we have completed gathering the options we add the ones that were
    %% not present but had default arguments in the specification.
    {ok, {lists:reverse(append_default_args(OptSpecList, OptAcc)), lists:reverse(ArgAcc)}}.


-spec get_option([option_spec()], string(), integer(), string() | char(), [string()]) ->
    {option(), [string()]}.
%% @doc Retrieve the specification corresponding to an option matching a string
%%      received on the command line.
get_option(OptSpecList, OptStr, FieldPos, OptName, Tail) ->
    case lists:keysearch(OptName, FieldPos, OptSpecList) of
        {value, {Name, _Short, _Long, ArgSpec, _Help} = OptSpec} ->
            case ArgSpec of
                undefined ->
                    {Name, Tail};
                _ ->
                    ArgSpecType = arg_spec_type(ArgSpec),
                    case Tail of
                        [Arg | Tail1] ->
                            case (ArgSpecType =:= boolean) andalso not is_boolean_arg(Arg) of
                                %% Special case for booleans: when the next string
                                %% is an option we assume the value is 'true'.
                                true ->
                                    {{Name, true}, Tail};
                                _ ->
                                    {convert_option_arg(OptSpec, Arg), Tail1}
                            end;
                        [] ->
                            case ArgSpecType of
                                boolean ->
                                    {{Name, true}, Tail};
                                _ ->
                                    throw({error, {missing_option_arg, Name}})
                            end
                    end
            end;
        false ->
            throw({error, {invalid_option, OptStr}})
    end.


-spec get_option_embedded_arg([option_spec()], string(), integer(), string(),
                              string()) ->  option().
%% @doc Retrieve the specification corresponding to an option matching a string
%%      received on the command line that had its argument assigned within the
%%      same string (e.g. "verbose=true").
get_option_embedded_arg(OptSpecList, OptStr, FieldPos, OptName, Arg) ->
    case lists:keysearch(OptName, FieldPos, OptSpecList) of
        {value, {_Name, _Short, _Long, ArgSpec, _Help} = OptSpec} ->
            case ArgSpec of
                undefined ->
                    throw({error, {invalid_option_arg, OptStr}});
                _ ->
                    convert_option_arg(OptSpec, Arg)
            end;
        false ->
            throw({error, {invalid_option, OptStr}})
    end.


-spec split_embedded_arg(string()) -> {Name :: string(), Arg :: string()} | string().
%% @doc Split an option string that may contain and option with its argument
%%      separated by an equal ('=') character (e.g. "port=1000").
split_embedded_arg(OptStr) ->
    split_embedded_arg(OptStr, OptStr, []).

split_embedded_arg(_OptStr, [$= | Tail], Acc) ->
    {lists:reverse(Acc), Tail};
split_embedded_arg(OptStr, [Char | Tail], Acc) ->
    split_embedded_arg(OptStr, Tail, [Char | Acc]);
split_embedded_arg(OptStr, [], _Acc) ->
    OptStr.


-spec get_option_no_arg([option_spec()], string(), string() | char(), integer()) -> option().
%% @doc Retrieve the specification corresponding to an option that has no
%%      argument and matches a string received on the command line.
get_option_no_arg(OptSpecList, OptStr, OptName, FieldPos) ->
    case lists:keysearch(OptName, FieldPos, OptSpecList) of
        {value, {Name, _Short, _Long, undefined, _Help}} ->
            Name;
        {value, {Name, _Short, _Long, ArgSpec, _Help}} ->
            case arg_spec_type(ArgSpec) of
                %% Special case for booleans: if there is no argument we assume
                %% the value is 'true'.
                boolean ->
                    {Name, true};
                _ ->
                    throw({error, {missing_option_arg, Name}})
            end;
        false ->
            throw({error, {invalid_option, OptStr}})
    end.


-spec find_non_option_arg([option_spec()], integer()) -> {value, option_spec()} | false.
%% @doc Find the option for the discrete argument in position specified in the
%%      Pos argument.
find_non_option_arg([{_Name, undefined, undefined, _ArgSpec, _Help} = OptSpec | _Tail], 0) ->
     {value, OptSpec};
find_non_option_arg([{_Name, undefined, undefined, _ArgSpec, _Help} | Tail], Pos) ->
    find_non_option_arg(Tail, Pos - 1);
find_non_option_arg([_Head | Tail], Pos) ->
    find_non_option_arg(Tail, Pos);
find_non_option_arg([], _Pos) ->
    false.


-spec append_default_args([option_spec()], [option()]) -> [option()].
%% @doc Appends the default values of the options that are not present.
append_default_args([{Name, _Short, _Long, {_Type, DefaultArg}, _Help} | Tail], OptAcc) ->
    append_default_args(Tail,
               case lists:keymember(Name, 1, OptAcc) of
                   false ->
                       [{Name, DefaultArg} | OptAcc];
                   _ ->
                       OptAcc
               end);
%% For options with no default argument.
append_default_args([_Head | Tail], OptAcc) ->
    append_default_args(Tail, OptAcc);
append_default_args([], OptAcc) ->
    OptAcc.


%% -spec is_option(string()) -> boolean().
%% is_option([Char | _Tail] = OptStr) ->
%%     (Char =:= $-) orelse lists:member($=, OptStr).
    

-spec convert_option_arg(option_spec(), string()) -> [option()].
%% @doc Convert the argument passed in the command line to the data type
%%      indicated by the argument specification.
convert_option_arg({Name, _Short, _Long, ArgSpec, _Help}, Arg) ->
    try
        {Name, to_type(arg_spec_type(ArgSpec), Arg)}
    catch
        error:_ ->
            throw({error, {invalid_option_arg, {Name, Arg}}})
    end.


-spec arg_spec_type(arg_spec()) -> arg_type() | undefined.
arg_spec_type({Type, _DefaultArg}) ->
    Type;
arg_spec_type(Type) when is_atom(Type) ->
    Type.


-spec to_type(atom(), string()) -> arg_value().
to_type(binary, Arg) ->
    list_to_binary(Arg);
to_type(atom, Arg) ->
    list_to_atom(Arg);
to_type(integer, Arg) ->
    list_to_integer(Arg);
to_type(float, Arg) ->
    list_to_float(Arg);
to_type(boolean, Arg) ->
    is_boolean_arg(Arg);
to_type(_Type, Arg) ->
    Arg.


-spec is_boolean_arg(string()) -> boolean().
is_boolean_arg(Arg) ->
    LowerArg = string:to_lower(Arg),
    (LowerArg =:= "true") orelse (LowerArg =:= "t") orelse
    (LowerArg =:= "yes") orelse (LowerArg =:= "y") orelse
    (LowerArg =:= "on") orelse (LowerArg =:= "enabled") orelse
    (LowerArg =:= "1").
    

-spec usage([option_spec()], string()) -> ok.
%%--------------------------------------------------------------------
%% @spec usage(OptSpecList :: option_spec_list(), ProgramName :: string()) -> ok.
%% @doc  Show a message on stdout indicating the command line options and
%%       arguments that are supported by the program.
%%--------------------------------------------------------------------
usage(OptSpecList, ProgramName) ->
    io:format("Usage: ~s~s~n~n~s~n",
              [ProgramName, usage_cmd_line(OptSpecList), usage_options(OptSpecList)]).


-spec usage_cmd_line([option_spec()]) -> string().
%% @doc Return a string with the syntax for the command line options and
%%      arguments.
usage_cmd_line(OptSpecList) ->
    usage_cmd_line(OptSpecList, []).

usage_cmd_line([{Name, Short, Long, ArgSpec, _Help} | Tail], Acc) ->
    CmdLine =
        case ArgSpec of
            undefined ->
                if
                    %% For options with short form and no argument.
                    Short =/= undefined ->
                        [$\s, $[, $-, Short, $]];
                    %% For options with only long form and no argument.
                    Long =/= undefined ->
                        [$\s, $[, $-, $-, Long, $]];
                    true ->
                        []
                end;
            _ ->
                if
                    %% For options with short form and argument.
                    Short =/= undefined ->
                        [$\s, $[, $-, Short, $\s, $<, atom_to_list(Name), $>, $]];
                    %% For options with only long form and argument.
                    Long =/= undefined ->
                        [$\s, $[, $-, $-, Long, $\s, $<, atom_to_list(Name), $>, $]];
                    %% For options with neither short nor long form and argument.
                    true ->
                        [$\s, $<, atom_to_list(Name), $>]
                end
        end,
    usage_cmd_line(Tail, [CmdLine | Acc]);
usage_cmd_line([], Acc) ->
    lists:flatten(lists:reverse(Acc)).


-spec usage_options([option_spec()]) -> string().
%% @doc Return a string with the help message for each of the options and
%%      arguments.
usage_options(OptSpecList) ->
    usage_options(OptSpecList, []).

usage_options([{Name, Short, Long, _ArgSpec, _Help} = OptSpec | Tail], Acc) ->
    Prefix = 
        case Long of 
            undefined ->
                case Short of
                    %% Neither short nor long form (non-option argument).
                    undefined ->
                        [$<, atom_to_list(Name), $>];
                    %% Only short form.
                    _ ->
                        [$-, Short]
                end;
            _ ->
                case Short of
                    %% Only long form.
                    undefined ->
                        [$-, $-, Long];
                    %% Both short and long form.
                    _ ->
                        [$-, Short, $,, $\s, $-, $-, Long]
                end
        end,
    usage_options(Tail, add_option_help(OptSpec, Prefix, Acc));
usage_options([], Acc) ->
    lists:flatten(lists:reverse(Acc)).

-spec add_option_help(option_spec(), Prefix :: string(), Acc :: string()) -> string().
%% @doc Add the help message corresponding to an option specification to a list
%%      with the correct indentation.
add_option_help({_Name, _Short, _Long, _ArgSpec, Help}, Prefix, Acc) when is_list(Help), Help =/= [] ->
    FlatPrefix = lists:flatten(Prefix),
    case ((?INDENTATION * ?TAB_LENGTH) - 2 - length(FlatPrefix)) of
        TabSize when TabSize > 0 ->
            Tab = lists:duplicate(ceiling(TabSize / ?TAB_LENGTH), $\t),
            [[$\s, $\s, FlatPrefix, Tab, Help, $\n] | Acc];
        _ ->
            %% The indentation for the option description is 3 tabs (i.e. 24 characters)
            %% IMPORTANT: Change the number of tabs below if you change the
            %%            value of the INDENTATION macro.
            [[$\t, $\t, $\t, Help, $\n], [$\s, $\s, FlatPrefix, $\n] | Acc]
    end;
add_option_help(_Opt, _Prefix, Acc) ->
    Acc.


-spec ceiling(float()) -> integer().
%% @doc Return the smallest integral valur not less than the argument.
ceiling(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        %% Neg when Neg < 0 ->
        %%    T;
        Pos when Pos > 0 ->
            T + 1;
        _ ->
            T
    end.
