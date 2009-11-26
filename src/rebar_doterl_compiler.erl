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
-module(rebar_doterl_compiler).

-export([compile/2,
         clean/2]).

-include("rebar.hrl").

%% ===================================================================
%% Public API
%% ===================================================================

compile(Config, Dir) ->
    do_compile(Config, "src/*.erl", "ebin", ".erl", ".beam",
               fun compile_erl/2),
    do_compile(Config, "mibs/*.mib", "priv/mibs", ".mib", ".bin",
               fun compile_mib/2).

clean(Config, Dir) ->
    %% TODO: This would be more portable if it used Erlang to traverse
    %%       the dir structure and delete each file; however it would also
    %%       much slower.
    [] = os:cmd("rm -f ebin/*.beam priv/mibs/*.bin"),
    ok.



%% ===================================================================
%% Internal functions
%% ===================================================================

do_compile(Config, SrcWildcard, OutDir, InExt, OutExt, CompileFn) ->
    case filelib:wildcard(SrcWildcard) of
        [] ->
            ok;
        Srcs when is_list(Srcs) ->
            %% Build list of output files
            Targets = [target_file(S, OutDir, InExt, OutExt) || S <- Srcs],
            Files = lists:zip(Targets, Srcs),
            
            %% Make sure target directory exists
            ok = filelib:ensure_dir(hd(Targets)),
            
            %% Start compiling
            compile_each(Files, Config, CompileFn)
    end.


compile_each([], _Config, _CompileFn) ->
    ok;
compile_each([{Target, Src} | Rest], Config, CompileFn) ->
    case needs_compile(Target, Src) of
        true ->
            ?CONSOLE("Compiling ~s\n", [Src]),
            CompileFn(Src, Config);
        false ->
            ok
    end,
    compile_each(Rest, Config, CompileFn).
            
needs_compile(Target, Src) ->
    filelib:last_modified(Target) < filelib:last_modified(Src).


target_file(F, TargetDir, InExt, OutExt) ->
    filename:join([TargetDir, filename:basename(F, InExt) ++ OutExt]).


compile_erl(Source, Config) ->
    Opts = rebar_config:get_list(Config, erlc_opts, []),
    case compile:file(Source, [{i, "include"}, {outdir, "ebin"}, report] ++ Opts) of
        {ok, _} ->
            ok;
        error ->
            ?FAIL
    end.

compile_mib(Source, Config) ->
    Opts = rebar_config:get_list(Config, mibc_opts, []),
    case snmpc:compile(Source, [{outdir, "priv/mibs"}, {i, ["priv/mibs"]}] ++ Opts) of
        {ok, _} ->
            ok;
        {error, compilation_failed} ->
            ?FAIL
    end.
