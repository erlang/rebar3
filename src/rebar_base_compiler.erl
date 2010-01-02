%% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
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

-export([run/8]).


%% ===================================================================
%% Public API
%% ===================================================================

run(Config, SourceDir, SourceExt, TargetDir, TargetExt,
    FirstFiles, CompileFn, Opts) ->
    SourceExtRe = ".*\\" ++ SourceExt ++ [$$],

    %% Options:
    %% recurse_source_dir
    %% needs_compile_checks - [ fun/2 ]
    Recursive = proplists:get_bool(recurse_source_dir, Opts),

    %% Find all the source files we can
    FoundFiles = filelib:fold_files(SourceDir, SourceExtRe, Recursive,
                                 fun(F, Acc) -> [F | Acc] end, []),

    %% Construct two lists of targets. "FirstTargets" is the list of files which
    %% must be compiled first and in strict order; "RestTargets" is all remaining files
    %% that may be compiled in any order.
    FirstTargets = [{Fs, target_file(Fs, SourceDir, SourceExt, TargetDir, TargetExt)} ||
                       Fs <- FirstFiles],
    RestTargets = [{Fs, target_file(Fs, SourceDir, SourceExt, TargetDir, TargetExt)} ||
                      Fs <- drop_each(FirstFiles, FoundFiles)],

    %% Setup list of functions which determine if a file needs compilation or not. By
    %% default we just check the last modified date
    NeedsCompileFns = [ fun check_source_lastmod/3 ] ++
        rebar_config:get(Config, needs_compile_checks, []),

    %% Compile the first targets in sequence
    compile_each(FirstTargets, Config, NeedsCompileFns, CompileFn),

    %% Spin up workers
    case RestTargets of
        [] ->
            ok;
        _ ->
            Self = self(),
            F = fun() -> compile_worker(Self, Config, NeedsCompileFns, CompileFn) end,
            Pids = [spawn_monitor(F) || _I <- lists:seq(1,3)],
            compile_queue(Pids, RestTargets)
    end.


%% ===================================================================
%% Internal functions
%% ===================================================================

target_file(SourceFile, SourceDir, SourceExt, TargetDir, TargetExt) ->
    %% Remove all leading components of the source dir from the file -- we want
    %% to maintain the deeper structure (if any) of the source file path
    BaseFile = remove_common_path(SourceFile, SourceDir),
    filename:join([TargetDir, filename:dirname(BaseFile),
                   filename:basename(BaseFile, SourceExt) ++ TargetExt]).


remove_common_path(Fname, Path) ->
    remove_common_path1(filename:split(Fname), filename:split(Path)).

remove_common_path1([Part | RestFilename], [Part | RestPath]) ->
    remove_common_path1(RestFilename, RestPath);
remove_common_path1(FilenameParts, _) ->
    filename:join(FilenameParts).


drop_each([], List) ->
    List;
drop_each([Member | Rest], List) ->
    drop_each(Rest, lists:delete(Member, List)).


needs_compile(_SourceFile, _TargetFile, _Config, []) ->
    false;
needs_compile(SourceFile, TargetFile, Config, [Fn | Rest]) ->
    case Fn(SourceFile, TargetFile, Config) of
        true ->
            true;
        false ->
            needs_compile(SourceFile, TargetFile, Config, Rest)
    end.

check_source_lastmod(SourceFile, TargetFile, _Config) ->
    filelib:last_modified(TargetFile) < filelib:last_modified(SourceFile).

compile(Source, Target, Config, NeedsCompileFns, CompileFn) ->
    case needs_compile(Source, Target, Config, NeedsCompileFns) of
        true ->
            ok = filelib:ensure_dir(Target),
            CompileFn(Source, Target, Config);
        false ->
            skipped
    end.



compile_each([], _Config, _NeedsCompileFns, _CompileFn) ->
    ok;
compile_each([{Source, Target} | Rest], Config, NeedsCompileFns, CompileFn) ->
    case compile(Source, Target, Config, NeedsCompileFns, CompileFn) of
        ok ->
            ?CONSOLE("Compiled ~s\n", [Source]);
        skipped ->
            ?INFO("Skipped ~s\n", [Source])
    end,
    compile_each(Rest, Config, NeedsCompileFns, CompileFn).



compile_queue([], []) ->
    ok;
compile_queue(Pids, Targets) ->
    receive
        {next, Worker} ->
            case Targets of
                [] ->
                    Worker ! empty,
                    compile_queue(Pids, Targets);
                [{Source, Target} | Rest] ->
                    Worker ! {compile, Source, Target},
                    compile_queue(Pids, Rest)
            end;

        {fail, Error} ->
            ?DEBUG("Worker compilation failed: ~p\n", [Error]),
            ?FAIL;

        {compiled, Source} ->
            ?CONSOLE("Compiled ~s\n", [Source]),
            compile_queue(Pids, Targets);

        {skipped, Source} ->
            ?INFO("Skipped ~s\n", [Source]),
            compile_queue(Pids, Targets);

        {'DOWN', Mref, _, Pid, normal} ->
            ?DEBUG("Worker exited cleanly\n", []),
            Pids2 = lists:delete({Pid, Mref}, Pids),
            compile_queue(Pids2, Targets);

        {'DOWN', _Mref, _, _Pid, Info} ->
            ?DEBUG("Worker failed: ~p\n", [Info]),
            ?FAIL
    end.

compile_worker(QueuePid, Config, NeedsCompileFns, CompileFn) ->
    QueuePid ! {next, self()},
    receive
        {compile, Source, Target} ->
            case catch(compile(Source, Target, Config, NeedsCompileFns, CompileFn)) of
                ok ->
                    QueuePid ! {compiled, Source},
                    compile_worker(QueuePid, Config, NeedsCompileFns, CompileFn);
                skipped ->
                    QueuePid ! {skipped, Source},
                    compile_worker(QueuePid, Config, NeedsCompileFns, CompileFn);
                Error ->
                    QueuePid ! {fail, Error},
                    ok
            end;

        empty ->
            ok
    end.
