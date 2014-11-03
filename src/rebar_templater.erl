%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2009 Dave Smith (dizzyd@dizzyd.com)
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%% -------------------------------------------------------------------
-module(rebar_templater).

-export([new/3,
         list_templates/1,
         create/1]).

%% API for other utilities that need templating functionality
-export([resolve_variables/2,
         render/2]).

-include("rebar.hrl").

-define(TEMPLATE_RE, "^[^._].*\\.template\$").
-define(ERLYDTL_COMPILE_OPTS, [report_warnings, return_errors, {auto_escape, false}, {out_dir, false}]).

%% ===================================================================
%% Public API
%% ===================================================================

new(app, DirName, State) ->
    create1(State, DirName, "otp_app");
new(lib, DirName, State) ->
    create1(State, DirName, "otp_lib");
new(plugin, DirName, State) ->
    create1(State, DirName, "plugin");
new(rel, DirName, State) ->
    create1(State, DirName, "otp_rel").

list_templates(State) ->
    {AvailTemplates, Files} = find_templates(State),
    ?DEBUG("Available templates: ~p\n", [AvailTemplates]),

    lists:foreach(
      fun({Type, F}) ->
              BaseName = filename:basename(F, ".template"),
              TemplateTerms = consult(load_file(Files, Type, F)),
              {_, VarList} = lists:keyfind(variables, 1, TemplateTerms),
              Vars = lists:foldl(fun({V,_}, Acc) ->
                                         [atom_to_list(V) | Acc]
                                 end, [], VarList),
              ?INFO("  * ~s: ~s (~p) (variables: ~p)\n",
                       [BaseName, F, Type, string:join(Vars, ", ")])
      end, AvailTemplates),
    ok.

create(State) ->
    TemplateId = template_id(State),
    create1(State, "", TemplateId).

%%
%% Given a list of key value pairs, for each string value attempt to
%% render it using Dict as the context. Storing the result in Dict as Key.
%%
resolve_variables([], Dict) ->
    Dict;
resolve_variables([{Key, Value0} | Rest], Dict) when is_list(Value0) ->
    Value = render(Value0, Dict),
    resolve_variables(Rest, dict:store(Key, Value, Dict));
resolve_variables([{Key, {list, Dicts}} | Rest], Dict) when is_list(Dicts) ->
    %% just un-tag it so erlydtl can use it
    resolve_variables(Rest, dict:store(Key, Dicts, Dict));
resolve_variables([_Pair | Rest], Dict) ->
    resolve_variables(Rest, Dict).

%%
%% Render a binary to a string, using erlydtl and the specified context
%%
render(Template, Context) when is_atom(Template) ->
    Template:render(Context);
render(Template, Context) ->
    Module = list_to_atom(Template++"_dtl"),
    Module:render(Context).

%% ===================================================================
%% Internal functions
%% ===================================================================

create1(State, AppDir, TemplateId) ->
    ec_file:mkdir_p(AppDir),
    file:set_cwd(AppDir),
    {AvailTemplates, Files} = find_templates(State),
    ?DEBUG("Available templates: ~p\n", [AvailTemplates]),

    %% Using the specified template id, find the matching template file/type.
    %% Note that if you define the same template in both ~/.rebar/templates
    %% that is also present in the escript, the one on the file system will
    %% be preferred.
    {Type, Template} = select_template(AvailTemplates, TemplateId),

    %% Load the template definition as is and get the list of variables the
    %% template requires.
    Context0 = dict:from_list([{appid, AppDir}]),
    TemplateTerms = consult(load_file(Files, Type, Template)),
    case lists:keyfind(variables, 1, TemplateTerms) of
        {variables, Vars} ->
            case parse_vars(Vars, Context0) of
                {error, Entry} ->
                    Context1 = undefined,
                    ?ABORT("Failed while processing variables from template ~p."
                           "Variable definitions must follow form of "
                           "[{atom(), term()}]. Failed at: ~p\n",
                           [TemplateId, Entry]);
                Context1 ->
                    ok
            end;
        false ->
            ?WARN("No variables section found in template ~p; "
                  "using empty context.\n", [TemplateId]),
            Context1 = Context0
    end,

    %% Load variables from disk file, if provided
    Context2 = case rebar_state:get(State, template_vars, undefined) of
                   undefined ->
                       Context1;
                   File ->
                       case consult(load_file([], file, File)) of
                           {error, Reason} ->
                               ?ABORT("Unable to load template_vars from ~s: ~p\n",
                                      [File, Reason]);
                           Terms ->
                               %% TODO: Cleanup/merge with similar code in rebar_reltool
                               M = fun(_Key, _Base, Override) -> Override end,
                               dict:merge(M, Context1, dict:from_list(Terms))
                       end
               end,

    %% For each variable, see if it's defined in global vars -- if it is,
    %% prefer that value over the defaults
    Context3 = update_vars(State, dict:fetch_keys(Context2), Context1),
    ?DEBUG("Template ~p context: ~p\n", [TemplateId, dict:to_list(Context2)]),

    %% Handle variables that possibly include other variables in their
    %% definition
    %Context = resolve_variables(dict:to_list(Context3), Context3),

    %?DEBUG("Resolved Template ~p context: ~p\n",
           %[TemplateId, dict:to_list(Context)]),

    %% Now, use our context to process the template definition -- this
    %% permits us to use variables within the definition for filenames.
    %FinalTemplate = consult(render(load_file(Files, Type, Template), Context)),
    %?DEBUG("Final template def ~p: ~p\n", [TemplateId, FinalTemplate]),

    %% Execute the instructions in the finalized template
    Force = rebar_state:get(State, force, "0"),
    execute_template([], TemplateTerms, Type, TemplateId, Context3, Force, []).

find_templates(State) ->
    %% Load a list of all the files in the escript -- cache them since
    %% we'll potentially need to walk it several times over the course of
    %% a run.
    Files = cache_escript_files(State),

    %% Build a list of available templates
    AvailTemplates = find_disk_templates(State)
        ++ find_escript_templates(Files),

    {AvailTemplates, Files}.

%%
%% Scan the current escript for available files
%%
cache_escript_files(State) ->
    {ok, Files} = rebar_utils:escript_foldl(
                    fun(Name, _, GetBin, Acc) ->
                            [{Name, GetBin()} | Acc]
                    end,
                    [], rebar_state:get(State, escript)),
    Files.

template_id(State) ->
    case rebar_state:get(State, template, undefined) of
        undefined ->
            ?ABORT("No template specified.\n", []);
        TemplateId ->
            TemplateId
    end.

find_escript_templates(Files) ->
    [{escript, Name}
     || {Name, _Bin} <- Files,
        re:run(Name, ?TEMPLATE_RE, [{capture, none}]) == match].

find_disk_templates(State) ->
    OtherTemplates = find_other_templates(State),
    HomeFiles = rebar_utils:find_files(filename:join([os:getenv("HOME"),
                                                      ".rebar", "templates"]),
                                       ?TEMPLATE_RE),
    LocalFiles = rebar_utils:find_files(".", ?TEMPLATE_RE, true),
    [{file, F} || F <- OtherTemplates ++ HomeFiles ++ LocalFiles].

find_other_templates(State) ->
    case rebar_state:get(State, template_dir, undefined) of
        undefined ->
            [];
        TemplateDir ->
            rebar_utils:find_files(TemplateDir, ?TEMPLATE_RE)
    end.

select_template([], Template) ->
    ?ABORT("Template ~s not found.\n", [Template]);
select_template([{Type, Avail} | Rest], Template) ->
    case filename:basename(Avail, ".template") == Template of
        true ->
            {Type, Avail};
        false ->
            select_template(Rest, Template)
    end.

%%
%% Read the contents of a file from the appropriate source
%%
load_file(Files, escript, Name) ->
    {Name, Bin} = lists:keyfind(Name, 1, Files),
    Bin;
load_file(_Files, file, Name) ->
    {ok, Bin} = file:read_file(Name),
    Bin.

%%
%% Parse/validate variables out from the template definition
%%
parse_vars([], Dict) ->
    Dict;
parse_vars([{Key, Value} | Rest], Dict) when is_atom(Key) ->
    parse_vars(Rest, dict:store(Key, Value, Dict));
parse_vars([Other | _Rest], _Dict) ->
    {error, Other};
parse_vars(Other, _Dict) ->
    {error, Other}.

%%
%% Given a list of keys in Dict, see if there is a corresponding value defined
%% in the global config; if there is, update the key in Dict with it
%%
update_vars(_State, [], Dict) ->
    Dict;
update_vars(State, [Key | Rest], Dict) ->
    Value = rebar_state:get(State, Key, dict:fetch(Key, Dict)),
    update_vars(State, Rest, dict:store(Key, Value, Dict)).


%%
%% Given a string or binary, parse it into a list of terms, ala file:consult/1
%%
consult(Str) when is_list(Str) ->
    consult([], Str, []);
consult(Bin) when is_binary(Bin)->
    consult([], binary_to_list(Bin), []).

consult(Cont, Str, Acc) ->
    case erl_scan:tokens(Cont, Str, 0) of
        {done, Result, Remaining} ->
            case Result of
                {ok, Tokens, _} ->
                    {ok, Term} = erl_parse:parse_term(Tokens),
                    consult([], Remaining, [maybe_dict(Term) | Acc]);
                {eof, _Other} ->
                    lists:reverse(Acc);
                {error, Info, _} ->
                    {error, Info}
            end;
        {more, Cont1} ->
            consult(Cont1, eof, Acc)
    end.


maybe_dict({Key, {list, Dicts}}) ->
    %% this is a 'list' element; a list of lists representing dicts
    {Key, {list, [dict:from_list(D) || D <- Dicts]}};
maybe_dict(Term) ->
    Term.


write_file(Output, Data, Force) ->
    %% determine if the target file already exists
    FileExists = filelib:is_regular(Output),

    %% perform the function if we're allowed,
    %% otherwise just process the next template
    case Force =:= "1" orelse FileExists =:= false of
        true ->
            ok = filelib:ensure_dir(Output),
            case {Force, FileExists} of
                {"1", true} ->
                    ?INFO("Writing ~s (forcibly overwriting)~n",
                             [Output]);
                _ ->
                    ?INFO("Writing ~s~n", [Output])
            end,
            case file:write_file(Output, Data) of
                ok ->
                    ok;
                {error, Reason} ->
                    ?ABORT("Failed to write output file ~p: ~p\n",
                           [Output, Reason])
            end;
        false ->
            {error, exists}
    end.

prepend_instructions(Instructions, Rest) when is_list(Instructions) ->
    Instructions ++ Rest;
prepend_instructions(Instruction, Rest) ->
    [Instruction|Rest].

%%
%% Execute each instruction in a template definition file.
%%
execute_template(_Files, [], _TemplateType, _TemplateName,
                 _Context, _Force, ExistingFiles) ->
    case ExistingFiles of
        [] ->
            ok;
        _ ->
            Msg = lists:flatten([io_lib:format("\t* ~p~n", [F]) ||
                                    F <- lists:reverse(ExistingFiles)]),
            Help = "To force overwriting, specify -f/--force/force=1"
                " on the command line.\n",
            ?ERROR("One or more files already exist on disk and "
                   "were not generated:~n~s~s", [Msg , Help])
    end;
execute_template(Files, [{'if', Cond, True} | Rest], TemplateType,
                 TemplateName, Context, Force, ExistingFiles) ->
    execute_template(Files, [{'if', Cond, True, []}|Rest], TemplateType,
                     TemplateName, Context, Force, ExistingFiles);
execute_template(Files, [{'if', Cond, True, False} | Rest], TemplateType,
                 TemplateName, Context, Force, ExistingFiles) ->
    Instructions = case dict:find(Cond, Context) of
                       {ok, true} ->
                           True;
                       {ok, "true"} ->
                           True;
                       _ ->
                           False
                   end,
    execute_template(Files, prepend_instructions(Instructions, Rest),
                     TemplateType, TemplateName, Context, Force,
                     ExistingFiles);
execute_template(Files, [{'case', Variable, Values, Instructions} | Rest], TemplateType,
                 TemplateName, Context, Force, ExistingFiles) ->
    {ok, Value} = dict:find(Variable, Context),
    Instructions2 = case lists:member(Value, Values) of
                       true ->
                           Instructions;
                       _ ->
                           []
                   end,
    execute_template(Files, prepend_instructions(Instructions2, Rest),
                     TemplateType, TemplateName, Context, Force,
                     ExistingFiles);
execute_template(Files, [{template, Input, Output} | Rest], TemplateType,
                 TemplateName, Context, Force, ExistingFiles) ->
    _InputName = filename:join(filename:dirname(TemplateName), Input),
    %File = load_file(Files, TemplateType, InputName),
    OutputTemplateName = make_template_name("rebar_output_template", Output),
    {ok, OutputTemplateName1} = erlydtl:compile_template(Output, OutputTemplateName, ?ERLYDTL_COMPILE_OPTS),
    {ok, OutputRendered} = OutputTemplateName1:render(dict:to_list(Context)),
    {ok, Rendered} = render(Input, dict:to_list(Context)),
    case write_file(lists:flatten(io_lib:format("~s", [OutputRendered])), Rendered, Force) of
        ok ->
            execute_template(Files, Rest, TemplateType, TemplateName,
                             Context, Force, ExistingFiles);
        {error, exists} ->
            execute_template(Files, Rest, TemplateType, TemplateName,
                             Context, Force, [Output|ExistingFiles])
    end;
execute_template(Files, [{file, Input, Output} | Rest], TemplateType,
                 TemplateName, Context, Force, ExistingFiles) ->
    InputName = filename:join(filename:dirname(TemplateName), Input),
    File = load_file(Files, TemplateType, InputName),
    case write_file(Output, File, Force) of
        ok ->
            execute_template(Files, Rest, TemplateType, TemplateName,
                             Context, Force, ExistingFiles);
        {error, exists} ->
            execute_template(Files, Rest, TemplateType, TemplateName,
                             Context, Force, [Output|ExistingFiles])
    end;
execute_template(Files, [{dir, Name} | Rest], TemplateType,
                 TemplateName, Context, Force, ExistingFiles) ->
    case filelib:ensure_dir(filename:join(Name, "dummy")) of
        ok ->
            execute_template(Files, Rest, TemplateType, TemplateName,
                             Context, Force, ExistingFiles);
        {error, Reason} ->
            ?ABORT("Failed while processing template instruction "
                   "{dir, ~s}: ~p\n", [Name, Reason])
    end;
execute_template(Files, [{copy, Input, Output} | Rest], TemplateType,
                 TemplateName, Context, Force, ExistingFiles) ->
    InputName = filename:join(filename:dirname(TemplateName), Input),
    try rebar_file_utils:cp_r([InputName ++ "/*"], Output) of
        ok ->
            execute_template(Files, Rest, TemplateType, TemplateName,
                             Context, Force, ExistingFiles)
    catch _:_ ->
            ?ABORT("Failed while processing template instruction "
                   "{copy, ~s, ~s}~n", [Input, Output])
    end;
execute_template(Files, [{chmod, Mod, File} | Rest], TemplateType,
                 TemplateName, Context, Force, ExistingFiles)
  when is_integer(Mod) ->
    case file:change_mode(File, Mod) of
        ok ->
            execute_template(Files, Rest, TemplateType, TemplateName,
                             Context, Force, ExistingFiles);
        {error, Reason} ->
            ?ABORT("Failed while processing template instruction "
                   "{chmod, ~b, ~s}: ~p~n", [Mod, File, Reason])
    end;
execute_template(Files, [{symlink, Existing, New} | Rest], TemplateType,
                 TemplateName, Context, Force, ExistingFiles) ->
    case file:make_symlink(Existing, New) of
        ok ->
            execute_template(Files, Rest, TemplateType, TemplateName,
                             Context, Force, ExistingFiles);
        {error, Reason} ->
            ?ABORT("Failed while processing template instruction "
                   "{symlink, ~s, ~s}: ~p~n", [Existing, New, Reason])
    end;
execute_template(Files, [{variables, _} | Rest], TemplateType,
                 TemplateName, Context, Force, ExistingFiles) ->
    execute_template(Files, Rest, TemplateType, TemplateName,
                     Context, Force, ExistingFiles);
execute_template(Files, [Other | Rest], TemplateType, TemplateName,
                 Context, Force, ExistingFiles) ->
    ?WARN("Skipping unknown template instruction: ~p\n", [Other]),
    execute_template(Files, Rest, TemplateType, TemplateName, Context,
                     Force, ExistingFiles).

-spec make_template_name(string(), term()) -> module().
make_template_name(Base, Value) ->
    %% Seed so we get different values each time
    random:seed(erlang:now()),
    Hash = erlang:phash2(Value),
    Ran = random:uniform(10000000),
    erlang:list_to_atom(Base ++ "_" ++
                            erlang:integer_to_list(Hash) ++
                            "_" ++ erlang:integer_to_list(Ran)).
