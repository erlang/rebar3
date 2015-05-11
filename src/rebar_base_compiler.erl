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
-module(rebar_base_compiler).

-export([run/4,
         run/7,
         run/8,
         ok_tuple/3,
         error_tuple/5]).

-export([format_error/1]).

-include_lib("providers/include/providers.hrl").
-include("rebar.hrl").

%% ===================================================================
%% Public API
%% ===================================================================

run(Config, FirstFiles, RestFiles, CompileFn) ->
    %% Compile the first files in sequence
    compile_each(FirstFiles++RestFiles, Config, CompileFn).

run(Config, FirstFiles, SourceDir, SourceExt, TargetDir, TargetExt,
    Compile3Fn) ->
    run(Config, FirstFiles, SourceDir, SourceExt, TargetDir, TargetExt,
        Compile3Fn, [check_last_mod]).

run(Config, FirstFiles, SourceDir, SourceExt, TargetDir, TargetExt,
    Compile3Fn, Opts) ->
    %% Convert simple extension to proper regex
    SourceExtRe = "^[^._].*\\" ++ SourceExt ++ [$$],

    Recursive = proplists:get_value(recursive, Opts, true),
    %% Find all possible source files
    FoundFiles = rebar_utils:find_files(SourceDir, SourceExtRe, Recursive),
    %% Remove first files from found files
    RestFiles = [Source || Source <- FoundFiles,
                           not lists:member(Source, FirstFiles)],

    %% Check opts for flag indicating that compile should check lastmod
    CheckLastMod = proplists:get_bool(check_last_mod, Opts),

    run(Config, FirstFiles, RestFiles,
        fun(S, C) ->
                Target = target_file(S, SourceDir, SourceExt,
                                     TargetDir, TargetExt),
                simple_compile_wrapper(S, Target, Compile3Fn, C, CheckLastMod)
        end).

ok_tuple(Config, Source, Ws) ->
    {ok, format_warnings(Config, Source, Ws)}.

error_tuple(Config, Source, Es, Ws, Opts) ->
    {error, format_errors(Config, Source, Es),
     format_warnings(Config, Source, Ws, Opts)}.

format_error({compile_file, File, Error}) ->
    maybe_report(Error),
    io_lib:format("Compiling ~s failed", [File]).

%% ===================================================================
%% Internal functions
%% ===================================================================

simple_compile_wrapper(Source, Target, Compile3Fn, Config, false) ->
    Compile3Fn(Source, Target, Config);
simple_compile_wrapper(Source, Target, Compile3Fn, Config, true) ->
    case filelib:last_modified(Target) < filelib:last_modified(Source) of
        true ->
            Compile3Fn(Source, Target, Config);
        false ->
            skipped
    end.

target_file(SourceFile, SourceDir, SourceExt, TargetDir, TargetExt) ->
    BaseFile = remove_common_path(SourceFile, SourceDir),
    filename:join([TargetDir, filename:basename(BaseFile, SourceExt) ++ TargetExt]).

remove_common_path(Fname, Path) ->
    remove_common_path1(filename:split(Fname), filename:split(Path)).

remove_common_path1([Part | RestFilename], [Part | RestPath]) ->
    remove_common_path1(RestFilename, RestPath);
remove_common_path1(FilenameParts, _) ->
    filename:join(FilenameParts).

compile_each([], _Config, _CompileFn) ->
    ok;
compile_each([Source | Rest], Config, CompileFn) ->
    case CompileFn(Source, Config) of
        ok ->
            ?DEBUG("~sCompiled ~s", [rebar_utils:indent(1), filename:basename(Source)]);
        {ok, Warnings} ->
            report(Warnings),
            ?DEBUG("~sCompiled ~s", [rebar_utils:indent(1), filename:basename(Source)]);
        skipped ->
            ?DEBUG("~sSkipped ~s", [rebar_utils:indent(1), filename:basename(Source)]);
        Error ->
            throw(?PRV_ERROR({compile_file, Source, Error}))
    end,
    compile_each(Rest, Config, CompileFn).

format_errors(Config, Source, Errors) ->
    format_errors(Config, Source, "", Errors).

format_warnings(Config, Source, Warnings) ->
    format_warnings(Config, Source, Warnings, []).

format_warnings(Config, Source, Warnings, Opts) ->
    Prefix = case lists:member(warnings_as_errors, Opts) of
                 true -> "";
                 false -> "Warning: "
             end,
    format_errors(Config, Source, Prefix, Warnings).

maybe_report({{error, {error, _Es, _Ws}=ErrorsAndWarnings}, {source, _}}) ->
    maybe_report(ErrorsAndWarnings);
maybe_report([{error, E}, {source, S}]) ->
    report(["unexpected error compiling " ++ S, io_lib:fwrite("~n~p", [E])]);
maybe_report({error, Es, Ws}) ->
    report(Es),
    report(Ws);
maybe_report(_) ->
    ok.

report(Messages) ->
    lists:foreach(fun(Msg) -> io:format("~s~n", [Msg]) end, Messages).

format_errors(_Config, _MainSource, Extra, Errors) ->
    [begin
         [format_error(Source, Extra, Desc) || Desc <- Descs]
     end
     || {Source, Descs} <- Errors].

format_error(AbsSource, Extra, {{Line, Column}, Mod, Desc}) ->
    ErrorDesc = Mod:format_error(Desc),
    ?FMT("~s:~w:~w: ~s~s~n", [AbsSource, Line, Column, Extra, ErrorDesc]);
format_error(AbsSource, Extra, {Line, Mod, Desc}) ->
    ErrorDesc = Mod:format_error(Desc),
    ?FMT("~s:~w: ~s~s~n", [AbsSource, Line, Extra, ErrorDesc]);
format_error(AbsSource, Extra, {Mod, Desc}) ->
    ErrorDesc = Mod:format_error(Desc),
    ?FMT("~s: ~s~s~n", [AbsSource, Extra, ErrorDesc]).
