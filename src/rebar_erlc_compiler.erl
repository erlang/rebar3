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

-export([doterl_compile/2]).

-include("rebar.hrl").

%% ===================================================================
%% Public API
%% ===================================================================

compile(Config, _AppFile) ->
    doterl_compile(Config, "ebin"),
    rebar_base_compiler:run(Config, rebar_config:get_list(Config, mib_first_files, []),
                            "mibs", ".mib", "priv/mibs", ".bin",
                            fun compile_mib/3).

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
%% .erl Compilation API (externally used by only eunit)
%% ===================================================================

doterl_compile(Config, Outdir) ->
    FirstErls = rebar_config:get_list(Config, erl_first_files, []),
    RestErls  = [Source || Source <- rebar_utils:find_files("src", ".*.erl"),
                           lists:member(Source, FirstErls) == false],
    rebar_base_compiler:run(Config, FirstErls, RestErls,
                            fun(S, C) -> internal_erl_compile(S, C, Outdir) end).


%% ===================================================================
%% Internal functions
%% ===================================================================

include_path(Source, Config) ->
    ErlOpts = rebar_config:get(Config, erl_opts, []),
    [filename:dirname(Source)] ++ proplists:get_all_values(i, ErlOpts).

inspect(Source, IncludePath) ->
    ModuleDefault = filename:basename(Source, ".erl"),
    case epp:open(Source, IncludePath) of
        {ok, Epp} ->
            inspect_epp(Epp, ModuleDefault, []);
        {error, Reason} ->
            ?DEBUG("Failed to inspect ~s: ~p\n", [Source, Reason]),
            {ModuleDefault, []}
    end.

inspect_epp(Epp, Module, Includes) ->
    case epp:parse_erl_form(Epp) of
        {ok, {attribute, _, module, ActualModule}} when is_list(ActualModule) ->
            %% If the module name includes package info, we get a list of atoms...
            case is_list(ActualModule) of
                true ->
                    ActualModuleStr = string:join([atom_to_list(P) || P <- ActualModule], ".");
                false ->
                    ActualModuleStr = atom_to_list(ActualModule)
            end,
            inspect_epp(Epp, ActualModuleStr, Includes);
        {ok, {attribute, 1, file, {Module, 1}}} ->
            inspect_epp(Epp, Module, Includes);
        {ok, {attribute, 1, file, {IncFile, 1}}} ->
            inspect_epp(Epp, Module, [IncFile | Includes]);
        {eof, _} ->
            epp:close(Epp),
            {Module, Includes};
        _ ->
            inspect_epp(Epp, Module, Includes)
    end.

needs_compile(Source, Target, Hrls) ->
    TargetLastMod = filelib:last_modified(Target),
    lists:any(fun(I) -> TargetLastMod < filelib:last_modified(I) end,
              [Source] ++ Hrls).


internal_erl_compile(Source, Config, Outdir) ->
    %% Determine the target name and includes list by inspecting the source file
    {Module, Hrls} = inspect(Source, include_path(Source, Config)),

    %% Construct the target filename
    Target = filename:join([Outdir | string:tokens(Module, ".")]) ++ ".beam",

    %% If the file needs compilation, based on last mod date of includes or
    %% the target,
    case needs_compile(Source, Target, Hrls) of
        true ->
            Opts = [{i, "include"}, {outdir, filename:dirname(Target)}, report, return] ++
                rebar_config:get(Config, erl_opts, []),
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
            end;
        false ->
            skipped
    end.

compile_mib(Source, _Target, Config) ->
    Opts = [{outdir, "priv/mibs"}, {i, ["priv/mibs"]}] ++
        rebar_config:get(Config, mib_opts, []),
    case snmpc:compile(Source, Opts) of
        {ok, _} ->
            ok;
        {error, compilation_failed} ->
            ?FAIL
    end.
