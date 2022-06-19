%%%-------------------------------------------------------------------
%%% @author Eric Merritt <>
%%% @copyright (C) 2011, Erlware, LLC.
%%% @doc
%%%  These are various utility functions to help with compiling and
%%%  decompiling erlang source. They are mostly useful to the
%%%  language/parse transform implementor.
%%% @end
%%%-------------------------------------------------------------------
-module(ec_compile).

-export([beam_to_erl_source/2,
         erl_source_to_core_ast/1,
         erl_source_to_erl_ast/1,
         erl_source_to_asm/1,
         erl_source_to_erl_syntax/1,
         erl_string_to_core_ast/1,
         erl_string_to_erl_ast/1,
         erl_string_to_asm/1,
         erl_string_to_erl_syntax/1]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc decompile a beam file that has been compiled with +debug_info
%% into a erlang source file
%%
%% @param BeamFName the name of the beamfile
%% @param ErlFName the name of the erlang file where the generated
%% source file will be output. This should *not* be the same as the
%% source file that created the beamfile unless you want to overwrite
%% it.
-spec beam_to_erl_source(string(), string()) -> ok | term().
beam_to_erl_source(BeamFName, ErlFName) ->
  case beam_lib:chunks(BeamFName, [abstract_code]) of
    {ok, {_, [{abstract_code, {raw_abstract_v1,Forms}}]}} ->
          Src =
              erl_prettypr:format(erl_syntax:form_list(tl(Forms))),
          {ok, Fd} = file:open(ErlFName, [write]),
          io:fwrite(Fd, "~s~n", [Src]),
          file:close(Fd);
      Error ->
          Error
  end.

%% @doc compile an erlang source file into a Core Erlang AST
%%
%% @param Path - The path to the erlang source file
-spec erl_source_to_core_ast(file:filename()) -> CoreAst::term().
erl_source_to_core_ast(Path) ->
    {ok, Contents} = file:read_file(Path),
    erl_string_to_core_ast(binary_to_list(Contents)).

%% @doc compile an erlang source file into an Erlang AST
%%
%% @param Path - The path to the erlang source file
-spec erl_source_to_erl_ast(file:filename()) -> ErlangAst::term().
erl_source_to_erl_ast(Path) ->
    {ok, Contents} = file:read_file(Path),
    erl_string_to_erl_ast(binary_to_list(Contents)).

%% @doc compile an erlang source file into erlang terms that represent
%% the relevant ASM
%%
%% @param Path - The path to the erlang source file
-spec erl_source_to_asm(file:filename()) -> ErlangAsm::term().
erl_source_to_asm(Path) ->
    {ok, Contents} = file:read_file(Path),
    erl_string_to_asm(binary_to_list(Contents)).

%% @doc compile an erlang source file to a string that displays the
%% 'erl_syntax1 calls needed to reproduce those terms.
%%
%% @param Path - The path to the erlang source file
-spec erl_source_to_erl_syntax(file:filename()) -> string().
erl_source_to_erl_syntax(Path) ->
    {ok, Contents} = file:read_file(Path),
    erl_string_to_erl_syntax(Contents).

%% @doc compile a string representing an erlang expression into an
%% Erlang AST
%%
%% @param StringExpr - The path to the erlang source file
-spec erl_string_to_erl_ast(string()) -> ErlangAst::term().
erl_string_to_erl_ast(StringExpr) ->
    Forms0 =
        lists:foldl(fun(<<>>, Acc) ->
                            Acc;
                       (<<"\n\n">>, Acc) ->
                            Acc;
                       (El, Acc) ->
                            {ok, Tokens, _} =
                                erl_scan:string(binary_to_list(El)
                                                ++ "."),
                            [Tokens | Acc]
                    end, [], re:split(StringExpr, "\\.\n")),
    %% No need to reverse. This will rereverse for us
    lists:foldl(fun(Form, Forms) ->
                        {ok, ErlAST} = erl_parse:parse_form(Form),
                        [ErlAST | Forms]
                end, [], Forms0).

%% @doc compile a string representing an erlang expression into a
%% Core Erlang AST
%%
%% @param StringExpr - The path to the erlang source file
-spec erl_string_to_core_ast(string()) -> CoreAst::term().
erl_string_to_core_ast(StringExpr) ->
    compile:forms(erl_string_to_erl_ast(StringExpr), [to_core]).

%% @doc compile a string representing an erlang expression into a term
%% that represents the ASM
%%
%% @param StringExpr - The path to the erlang source file
-spec erl_string_to_asm(string()) -> ErlangAsm::term().
erl_string_to_asm(StringExpr) ->
    compile:forms(erl_string_to_erl_ast(StringExpr), ['S']).

%% @doc compile an erlang source file to a string that displays the
%% 'erl_syntax1 calls needed to reproduce those terms.
%%
%% @param StringExpr - The string representing the erlang code.
-spec erl_string_to_erl_syntax(string() | binary()) -> string().
erl_string_to_erl_syntax(BinaryExpr)
  when erlang:is_binary(BinaryExpr) ->
    erlang:binary_to_list(BinaryExpr);
erl_string_to_erl_syntax(StringExpr) ->
    {ok, Tokens, _} = erl_scan:string(StringExpr),
    {ok, ErlAST} = erl_parse:parse_form(Tokens),
    io:format(erl_prettypr:format(erl_syntax:meta(ErlAST))).
