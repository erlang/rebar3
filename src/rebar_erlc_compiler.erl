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

-include("rebar.hrl").

%% ===================================================================
%% Public API
%% ===================================================================

compile(Config, _AppFile) ->
    do_compile(Config, "src/*.erl", "ebin", ".erl", ".beam",
               fun compile_erl/2,
               rebar_config:get_list(Config, erl_first_files, [])),
    do_compile(Config, "mibs/*.mib", "priv/mibs", ".mib", ".bin",
               fun compile_mib/2,
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

do_compile(Config, SrcWildcard, OutDir, InExt, OutExt, CompileFn, FirstFiles) ->
    case filelib:wildcard(SrcWildcard) of
        [] ->
            ok;
        FoundFiles when is_list(FoundFiles) ->
            %% Ensure that the FirstFiles are compiled first; drop them from the
            %% FoundFiles and then build a final list of sources
            Srcs = FirstFiles ++ drop_each(FirstFiles, FoundFiles),
            
            %% Build list of output files
            Targets = [target_file(S, OutDir, InExt, OutExt) || S <- Srcs],
            Files = lists:zip(Srcs, Targets),

            %% Make sure target directory exists
            ok = filelib:ensure_dir(hd(Targets)),
            
            %% Start compiling
            compile_each(Files, Config, CompileFn)
    end.

drop_each([], List) ->
    List;
drop_each([Member | Rest], List) ->
    drop_each(Rest, lists:delete(Member, List)).

compile_each([], _Config, _CompileFn) ->
    ok;
compile_each([{Src, Target} | Rest], Config, CompileFn) ->
    case needs_compile(Src, Target) of
        true ->
            ?CONSOLE("Compiling ~s\n", [Src]),
            CompileFn(Src, Config);
        false ->
            ?INFO("Skipping ~s\n", [Src]),
            ok
    end,
    compile_each(Rest, Config, CompileFn).
            
needs_compile(Src, Target) ->
    filelib:last_modified(Target) < filelib:last_modified(Src).


target_file(F, TargetDir, InExt, OutExt) ->
    filename:join([TargetDir, filename:basename(F, InExt) ++ OutExt]).

compile_opts(Config, Key) ->
    rebar_config:get_list(Config, Key, []).

compile_erl(Source, Config) ->
    Opts = [{i, "include"}, {outdir, "ebin"}, report] ++ compile_opts(Config, erl_opts),
    case compile:file(Source, Opts) of
        {ok, _} ->
            ok;
        error ->
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
