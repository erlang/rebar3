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
-module(rebar_erlc_compiler).

-export([compile/2,
         clean/2]).

 %% make available for rebar_eunit until there is a better option
-export([hrls_check/3]).

-include("rebar.hrl").

%% ===================================================================
%% Public API
%% ===================================================================

compile(Config, _AppFile) ->
    rebar_base_compiler:run(Config, "src", ".erl", "ebin", ".beam",
                            rebar_config:get_list(Config, erl_first_files, []),
                            fun compile_erl/3,
                            [recurse_source_dir,
                             {needs_compile_checks, [fun hrls_check/3]}]),

    rebar_base_compiler:run(Config, "mibs", ".mib", "priv/mibs", ".bin",
                            rebar_config:get_list(Config, mib_first_files, []),
                            fun compile_mib/3,
                            []).

clean(_Config, _AppFile) ->
    %% TODO: This would be more portable if it used Erlang to traverse
    %%       the dir structure and delete each file; however it would also
    %%       much slower.
    ok = rebar_file_utils:rm_rf("ebin/*.beam priv/mibs/*.bin"),

    %% Erlang compilation is recursive, so it's possible that we have a nested
    %% directory structure in ebin with .beam files within. As such, we want
    %% to scan whatever is left in the ebin/ directory for sub-dirs which
    %% satisfy our criteria. TODO: Is there a better way to do this?
    BeamFiles = filelib:fold_files("ebin", "^.*\\.beam\$", true,
                                   fun(F, BeamFiles) -> BeamFiles ++ [F] end, []),
    rebar_file_utils:delete_each(BeamFiles),
    ok.



%% ===================================================================
%% Internal functions
%% ===================================================================

hrls_check(Source, Target, Config) ->
    TargetLastMod = filelib:last_modified(Target),
    lists:any(fun(I) -> TargetLastMod < filelib:last_modified(I) end,
              list_hrls(Source, Config)).


list_hrls(Src, Config) ->
    case epp:open(Src, include_path(Src, Config)) of
        {ok, Epp} ->
            %% check include for erlang files
            extract_includes(Epp, Src);
        _ ->
            false
    end.


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

include_path(Source, Config) ->
    [filename:dirname(Source) | compile_opts(Config, i)].

compile_opts(Config, Key) ->
    rebar_config:get_list(Config, Key, []).

compile_erl(Source, Target, Config) ->
    Opts = [{i, "include"}, {outdir, filename:dirname(Target)}, report, return] ++
            compile_opts(Config, erl_opts),
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

compile_mib(Source, _Target, Config) ->
    Opts = [{outdir, "priv/mibs"}, {i, ["priv/mibs"]}] ++ compile_opts(Config, mib_opts),
    case snmpc:compile(Source, Opts) of
        {ok, _} ->
            ok;
        {error, compilation_failed} ->
            ?FAIL
    end.
