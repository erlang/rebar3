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
-module(rebar_erlc_compiler).

-export([compile/2,
         clean/2]).

 %% make available for rebar_eunit until there is a better option
-export([do_compile/8, compile_opts/2]).

-include("rebar.hrl").

%% ===================================================================
%% Public API
%% ===================================================================

compile(Config, _AppFile) ->
    do_compile(Config, "src/*.erl", "ebin", ".erl", ".beam",
               fun list_hrls/2, fun compile_erl/2,
               rebar_config:get_list(Config, erl_first_files, [])),
    do_compile(Config, "mibs/*.mib", "priv/mibs", ".mib", ".bin",
               undefined, fun compile_mib/2,
               rebar_config:get_list(Config, mib_first_files, [])).

clean(_Config, _AppFile) ->
    %% TODO: This would be more portable if it used Erlang to traverse
    %%       the dir structure and delete each file; however it would also
    %%       much slower.
    [] = os:cmd("rm -f ebin/*.beam priv/mibs/*.bin"),
    ok.



%% ===================================================================
%% Internal functions
%% ===================================================================

do_compile(Config, SrcWildcard, OutDir, InExt, OutExt,
           IncludeFn, CompileFn, FirstFiles) ->
    case filelib:wildcard(SrcWildcard) of
        [] ->
            ok;
        FoundFiles when is_list(FoundFiles) ->
            %% Ensure that the FirstFiles are compiled first; drop them from the
            %% FoundFiles and compile them in sequence
            FirstTargets = [{Fs, target_file(Fs, OutDir, InExt, OutExt)} || Fs <- FirstFiles],
            RestTargets = [{Fs, target_file(Fs, OutDir, InExt, OutExt)} ||
                              Fs <- drop_each(FirstFiles, FoundFiles)],

            %% Make sure target directory exists 
            ok = filelib:ensure_dir(target_file(hd(FoundFiles), OutDir, InExt, OutExt)),

            %% Compile first targets in sequence
            compile_each(FirstTargets, Config, IncludeFn, CompileFn),
            
            %% Spin up workers
            Self = self(),
            Pids = [spawn_monitor(fun() -> compile_worker(Self) end) || _I <- lists:seq(1,3)],

            %% Process rest of targets
            compile_queue(Pids, RestTargets, Config, IncludeFn, CompileFn)
    end.

drop_each([], List) ->
    List;
drop_each([Member | Rest], List) ->
    drop_each(Rest, lists:delete(Member, List)).

compile_each([], _Config, _IncludeFn, _CompileFn) ->
    ok;
compile_each([{Src, Target} | Rest], Config, IncludeFn, CompileFn) ->
    case needs_compile(Src, Target, IncludeFn, Config) of
        true ->
            ?CONSOLE("Compiling ~s\n", [Src]),
            CompileFn(Src, Config);
        false ->
            ?INFO("Skipping ~s\n", [Src]),
            ok
    end,
    compile_each(Rest, Config, IncludeFn, CompileFn).
            
needs_compile(Src, Target, IncludeFn, Config) ->
    TargetLM = filelib:last_modified(Target),
    case TargetLM < filelib:last_modified(Src) of
        true ->
            true;
        false ->
            if is_function(IncludeFn) ->
                    lists:any(fun(I) ->
                                      TargetLM < filelib:last_modified(I)
                              end,
                              IncludeFn(Src, Config));
               true ->
                    false
            end
    end.

target_file(F, TargetDir, InExt, OutExt) ->
    filename:join([TargetDir, filename:basename(F, InExt) ++ OutExt]).

compile_opts(Config, Key) ->
    rebar_config:get_list(Config, Key, []).

list_hrls(Src, Config) ->
    case epp:open(Src, include_path(Src, Config)) of
        {ok, Epp} -> 
            %% check include for erlang files
            extract_includes(Epp, Src);
        _ ->
            false
    end.

include_path(Src, Config) ->
    [filename:dirname(Src)|compile_opts(Config, i)].

extract_includes(Epp, Src) ->
    case epp:parse_erl_form(Epp) of
	{ok, {attribute, 1, file, {Src, 1}}} ->
	    extract_includes(Epp, Src);
	{ok, {attribute, 1, file, {IncFile, 1}}} ->
            [IncFile|extract_includes(Epp, Src)];
	{ok, _} ->
	    extract_includes(Epp, Src);
	{eof, _} ->
	    epp:close(Epp),
	    [];
	{error, _Error} ->
	    extract_includes(Epp, Src)
    end.

compile_erl(Source, Config) ->
    Opts = [{i, "include"}, {outdir, "ebin"}, report, return] ++ compile_opts(Config, erl_opts),
    case compile:file(Source, Opts) of
        {ok, _, []} ->
            ok;
        {ok, _, _Warnings} ->
            %% We got at least one warning -- if fail_on_warning is in options, fail
            case lists:member(fail_on_warning, Opts) of
                true ->
                    ?FAIL;
                false ->
                    ok
            end;
        _ ->
            ?FAIL
    end.

compile_mib(Source, Config) ->
    Opts = [{outdir, "priv/mibs"}, {i, ["priv/mibs"]}] ++ compile_opts(Config, mib_opts),
    case snmpc:compile(Source, Opts) of
        {ok, _} ->
            ok;
        {error, compilation_failed} ->
            ?FAIL
    end.

compile_queue([], [], _Config, _IncludeFn, _CompileFn) ->
    ok;
compile_queue(Pids, Targets, Config, IncludeFn, CompileFn) ->
    receive
        {next, Worker} ->
            case Targets of
                [] ->
                    Worker ! empty,
                    compile_queue(Pids, Targets, Config, IncludeFn, CompileFn);
                [{Src, Target} | Rest] ->
                    Worker ! {compile, Src, Target, Config, IncludeFn, CompileFn},
                    compile_queue(Pids, Rest, Config, IncludeFn, CompileFn)
            end;

        {fail, Error} ->
            ?DEBUG("Worker compilation failed: ~p\n", [Error]),
            ?FAIL;
        
        {compiled, Source} ->
            ?CONSOLE("Compiled ~s\n", [Source]),
            compile_queue(Pids, Targets, Config, IncludeFn, CompileFn);
        
        {'DOWN', Mref, _, Pid, normal} ->
            ?DEBUG("Worker exited cleanly\n", []),
            Pids2 = lists:delete({Pid, Mref}, Pids),
            compile_queue(Pids2, Targets, Config, IncludeFn, CompileFn);
        
        {'DOWN', _Mref, _, _Pid, Info} ->
            ?DEBUG("Worker failed: ~p\n", [Info]),
            ?FAIL
    end.

compile_worker(QueuePid) ->
    QueuePid ! {next, self()},
    receive
        {compile, Src, Target, Config, IncludeFn, CompileFn} ->
            case needs_compile(Src, Target, IncludeFn, Config) of
                true ->
                    case catch(CompileFn(Src, Config)) of
                        ok ->
                            QueuePid ! {compiled, Src},
                            compile_worker(QueuePid);
                        Error ->
                            QueuePid ! {fail, Error},
                            ok
                    end;
                false ->
                    ?INFO("Skipping ~s\n", [Src]),
                    compile_worker(QueuePid)
            end;

        empty ->
            ok
    end.
