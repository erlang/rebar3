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

-include("rebar.hrl").

-export([run/4,
         run/7,
         run/8,
         ok_tuple/2,
         error_tuple/4,
         report/1,
         maybe_report/1,
         format_error_source/2]).

-type desc() :: term().
-type loc() :: {line(), col()} | line().
-type line() :: integer().
-type col() :: integer().
-type err_or_warn() :: {module(), desc()} | {loc(), module(), desc()}.

-type compile_fn_ret() ::  ok | {ok, [string()]} | skipped | term().
-type compile_fn() :: fun((file:filename(), [{_,_}] | rebar_dict()) -> compile_fn_ret()).
-type compile_fn3() :: fun((file:filename(), file:filename(), [{_,_}] | rebar_dict())
                           -> compile_fn_ret()).
-type error_tuple() :: {error, [string()], [string()]}.
-export_type([compile_fn/0, compile_fn_ret/0, error_tuple/0]).


%% ===================================================================
%% Public API
%% ===================================================================

%% @doc Runs a compile job, applying `compile_fn()' to all files,
%% starting with `First' files, and then `RestFiles'.
-spec run(rebar_dict() | [{_,_}] , [First], [Next], compile_fn()) ->
    compile_fn_ret() when
      First :: file:filename(),
      Next :: file:filename().
run(Config, FirstFiles, RestFiles, CompileFn) ->
    %% Compile the first files in sequence
    compile_each(FirstFiles++RestFiles, Config, CompileFn).

%% @doc Runs a compile job, applying `compile_fn3()' to all files,
%% starting with `First' files, and then the other content of `SourceDir'.
%% Files looked for are those ending in `SourceExt'. Results of the
%% compilation are put in `TargetDir' with the base file names
%% postfixed with `SourceExt'.
-spec run(rebar_dict() | [{_,_}] , [First], SourceDir, SourceExt,
      TargetDir, TargetExt, compile_fn3()) -> compile_fn_ret() when
      First :: file:filename(),
      SourceDir :: file:filename(),
      TargetDir :: file:filename(),
      SourceExt :: string(),
      TargetExt :: string().
run(Config, FirstFiles, SourceDir, SourceExt, TargetDir, TargetExt,
    Compile3Fn) ->
    run(Config, FirstFiles, SourceDir, SourceExt, TargetDir, TargetExt,
        Compile3Fn, [check_last_mod]).

%% @doc Runs a compile job, applying `compile_fn3()' to all files,
%% starting with `First' files, and then the other content of `SourceDir'.
%% Files looked for are those ending in `SourceExt'. Results of the
%% compilation are put in `TargetDir' with the base file names
%% postfixed with `SourceExt'.
%% Additional compile options can be passed in the last argument as
%% a proplist.
-spec run(rebar_dict() | [{_,_}] , [First], SourceDir, SourceExt,
      TargetDir, TargetExt, compile_fn3(), [term()]) -> compile_fn_ret() when
      First :: file:filename(),
      SourceDir :: file:filename(),
      TargetDir :: file:filename(),
      SourceExt :: string(),
      TargetExt :: string().
run(Config, FirstFiles, SourceDir, SourceExt0, TargetDir, TargetExt,
    Compile3Fn, Opts) ->
    %% Convert simple extension to proper regex.
    %% If the extension has a leading dot (e.g.: `.peg')
    %% we escape it.
    %% Otherwise, if the extension doesn't have a leading dot
    %% we add it ourselves (e.g.: `peg' -> `.peg')
    SourceExt = case SourceExt0 of
        [$.|_Ext] -> SourceExt0;
        _ -> [$.] ++ SourceExt0
    end,
    SourceExtRe = "^(?!\\._).*\\" ++ SourceExt ++ [$$],

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
                Target = target_file(S, SourceExt,
                                     TargetDir, TargetExt),
                simple_compile_wrapper(S, Target, Compile3Fn, C, CheckLastMod)
        end).

%% @doc Format good compiler results with warnings to work with
%% module internals. Assumes that warnings are not treated as errors.
-spec ok_tuple(file:filename(), [string()]) -> {ok, [string()]}.
ok_tuple(Source, Ws) ->
    {ok, format_warnings(Source, Ws)}.

%% @doc format error and warning strings for a given source file
%% according to user preferences.
-spec error_tuple(file:filename(), [Err], [Warn], rebar_dict() | [{_,_}]) ->
    error_tuple() when
      Err :: string(),
      Warn :: string().
error_tuple(Source, Es, Ws, Opts) ->
    {error, format_errors(Source, Es),
     format_warnings(Source, Ws, Opts)}.

%% @doc from a given path, and based on the user-provided options,
%% format the file path according to the preferences.
-spec format_error_source(file:filename(), rebar_dict() | [{_,_}]) ->
    file:filename().
format_error_source(Path, Opts) ->
    rebar_dir:format_source_file_name(Path, Opts).

%% ===================================================================
%% Internal functions
%% ===================================================================

%% @private if a check for last modifications is required, do the verification
%% and possibly skip the compile job.
-spec simple_compile_wrapper(Source, Target, compile_fn3(), [{_,_}] | rebar_dict(), boolean()) -> compile_fn_ret() when
      Source :: file:filename(),
      Target :: file:filename().
simple_compile_wrapper(Source, Target, Compile3Fn, Config, false) ->
    Compile3Fn(Source, Target, Config);
simple_compile_wrapper(Source, Target, Compile3Fn, Config, true) ->
    case filelib:last_modified(Target) < filelib:last_modified(Source) of
        true ->
            Compile3Fn(Source, Target, Config);
        false ->
            skipped
    end.

%% @private take a basic source set of file fragments and a target location,
%% create a file path and name for a compile artifact.
-spec target_file(SourceFile, SourceExt, TargetDir, TargetExt) -> File when
      SourceFile :: file:filename(),
      TargetDir :: file:filename(),
      SourceExt :: string(),
      TargetExt :: string(),
      File :: file:filename().
target_file(SourceFile, SourceExt, TargetDir, TargetExt) ->
    %% BaseFile = remove_common_path(SourceFile, SourceDir),
    filename:join([TargetDir, filename:basename(SourceFile, SourceExt) ++ TargetExt]).

%% @private runs the compile function `CompileFn' on every file
%% passed internally, along with the related project configuration.
%% If any errors are encountered, they're reported to stdout.
-spec compile_each([file:filename()], Config, CompileFn) -> Ret | no_return() when
      Config :: [{_,_}] | rebar_dict(),
      CompileFn :: compile_fn(),
      Ret :: compile_fn_ret().
compile_each([], _Config, _CompileFn) ->
    ok;
compile_each([Source | Rest], Config, CompileFn) ->
    case CompileFn(Source, Config) of
        ok ->
            ?DEBUG("~tsCompiled ~ts", [rebar_utils:indent(1), filename:basename(Source)]);
        {ok, Warnings} ->
            report(Warnings),
            ?DEBUG("~tsCompiled ~ts", [rebar_utils:indent(1), filename:basename(Source)]);
        skipped ->
            ?DEBUG("~tsSkipped ~ts", [rebar_utils:indent(1), filename:basename(Source)]);
        Error ->
            NewSource = format_error_source(Source, Config),
            ?ERROR("Compiling ~ts failed", [NewSource]),
            maybe_report(Error),
            ?DEBUG("Compilation failed: ~p", [Error]),
            ?ABORT
    end,
    compile_each(Rest, Config, CompileFn).

%% @private Formats and returns errors ready to be output.
-spec format_errors(string(), [err_or_warn()]) -> [string()].
format_errors(Source, Errors) ->
    format_errors(Source, "", Errors).

%% @private Formats and returns warning strings ready to be output.
-spec format_warnings(string(), [err_or_warn()]) -> [string()].
format_warnings(Source, Warnings) ->
    format_warnings(Source, Warnings, []).

%% @private Formats and returns warnings; chooses the distinct format they
%% may have based on whether `warnings_as_errors' option is on.
-spec format_warnings(string(), [err_or_warn()], rebar_dict() | [{_,_}]) -> [string()].
format_warnings(Source, Warnings, Opts) ->
    %% `Opts' can be passed in both as a list or a dictionary depending
    %% on whether the first call to rebar_erlc_compiler was done with
    %% the type `rebar_dict()' or `rebar_state:t()'.
    LookupFn = if is_list(Opts) -> fun lists:member/2
                ; true          -> fun dict:is_key/2
               end,
    Prefix = case LookupFn(warnings_as_errors, Opts) of
                 true -> "";
                 false -> "Warning: "
             end,
    format_errors(Source, Prefix, Warnings).

%% @private output compiler errors if they're judged to be reportable.
-spec maybe_report(Reportable | term()) -> ok when
      Reportable :: {{error, error_tuple()}, Source} | error_tuple() | ErrProps,
      ErrProps :: [{error, string()} | Source, ...],
      Source :: {source, string()}.
maybe_report({{error, {error, _Es, _Ws}=ErrorsAndWarnings}, {source, _}}) ->
    maybe_report(ErrorsAndWarnings);
maybe_report([{error, E}, {source, S}]) ->
    report(["unexpected error compiling " ++ S, io_lib:fwrite("~n~p", [E])]);
maybe_report({error, Es, Ws}) ->
    report(Es),
    report(Ws);
maybe_report(_) ->
    ok.

%% @private Outputs a bunch of strings, including a newline
-spec report([string()]) -> ok.
report(Messages) ->
    lists:foreach(fun(Msg) -> io:format("~ts~n", [Msg]) end, Messages).

%% private format compiler errors into proper outputtable strings
-spec format_errors(_, Extra, [err_or_warn()]) -> [string()] when
      Extra :: string().
format_errors(_MainSource, Extra, Errors) ->
    [[format_error(Source, Extra, Desc) || Desc <- Descs]
     || {Source, Descs} <- Errors].

%% @private format compiler errors into proper outputtable strings
-spec format_error(file:filename(), Extra, err_or_warn()) -> string() when
      Extra :: string().
format_error(Source, Extra, {Line, Mod=epp, Desc={include,lib,File}}) ->
    %% Special case for include file errors, overtaking the default one
    BaseDesc = Mod:format_error(Desc),
    Friendly = case filename:split(File) of
        [Lib, "include", _] ->
            io_lib:format("; Make sure ~s is in your app "
                          "file's 'applications' list", [Lib]);
        _ ->
            ""
    end,
    FriendlyDesc = BaseDesc ++ Friendly,
    ?FMT("~ts:~w: ~ts~ts~n", [Source, Line, Extra, FriendlyDesc]);
format_error(Source, Extra, {{Line, Column}, Mod, Desc}) ->
    ErrorDesc = Mod:format_error(Desc),
    ?FMT("~ts:~w:~w: ~ts~ts~n", [Source, Line, Column, Extra, ErrorDesc]);
format_error(Source, Extra, {Line, Mod, Desc}) ->
    ErrorDesc = Mod:format_error(Desc),
    ?FMT("~ts:~w: ~ts~ts~n", [Source, Line, Extra, ErrorDesc]);
format_error(Source, Extra, {Mod, Desc}) ->
    ErrorDesc = Mod:format_error(Desc),
    ?FMT("~ts: ~ts~ts~n", [Source, Extra, ErrorDesc]).
