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
-module(rebar_reltool).

-export([generate/2,
         clean/2]).

-include("rebar.hrl").
-include_lib("reltool/src/reltool.hrl").
-include_lib("kernel/include/file.hrl").

%% ===================================================================
%% Public API
%% ===================================================================

generate(Config, ReltoolFile) ->
    %% Make sure we have decent version of reltool available
    check_vsn(),

    %% Load the reltool configuration from the file
    ReltoolConfig = load_config(ReltoolFile),

    %% Spin up reltool server and load our config into it
    {ok, Server} = reltool:start_server([sys_tuple(ReltoolConfig)]),

    %% Do some validation of the reltool configuration; error messages out of
    %% reltool are still pretty cryptic
    validate_rel_apps(Server, sys_tuple(ReltoolConfig)),

    %% Finally, run reltool
    case catch(run_reltool(Server, Config, ReltoolConfig)) of
        ok ->
            ok;
        {error, failed} ->
            ?FAIL;
        Other2 ->
            ?ERROR("Unexpected error: ~p\n", [Other2]),
            ?FAIL
    end.


clean(_Config, ReltoolFile) ->
    ReltoolConfig = load_config(ReltoolFile),
    TargetDir = target_dir(ReltoolConfig),
    rebar_file_utils:rm_rf(TargetDir),
    rebar_file_utils:delete_each(["reltool.spec"]).



%% ===================================================================
%% Internal functions
%% ===================================================================

check_vsn() ->
    case code:lib_dir(reltool) of
        {error, bad_name} ->
            ?ABORT("Reltool support requires the reltool application to be installed!", []);
        Path ->
            ReltoolVsn = filename:basename(Path),
            case ReltoolVsn < "reltool-0.5.2" of
                true ->
                    ?ABORT("Reltool support requires at least reltool-0.5.2; this VM is using ~s\n",
                           [ReltoolVsn]);
                false ->
                    ok
            end
    end.

%%
%% Load terms from reltool.config
%%
load_config(ReltoolFile) ->
    case file:consult(ReltoolFile) of
        {ok, Terms} ->
            Terms;
        Other ->
            ?ABORT("Failed to load expected config from ~s: ~p\n", [ReltoolFile, Other])
    end.

%%
%% Look for the {sys, [...]} tuple in the reltool.config file. Without this present, we
%% can't run reltool.
%%
sys_tuple(ReltoolConfig) ->
    case lists:keysearch(sys, 1, ReltoolConfig) of
        {value, {sys, Data}} ->
            {sys, Data};
        false ->
            ?ABORT("Failed to find {sys, [...]} tuple in reltool.config.", [])
    end.

%%
%% Look for {target_dir, TargetDir} in the reltool config file; if none is
%% found, use the name of the release as the default target directory.
%%
target_dir(ReltoolConfig) ->
    case rebar_config:get_global(target_dir, undefined) of
        undefined ->
            case lists:keysearch(target_dir, 1, ReltoolConfig) of
                {value, {target_dir, TargetDir}} ->
                    filename:absname(TargetDir);
                false ->
                    {sys, SysInfo} = sys_tuple(ReltoolConfig),
                    case lists:keysearch(rel, 1, SysInfo) of
                        {value, {rel, Name, _Vsn, _Apps}} ->
                            filename:absname(Name);
                        false ->
                            filename:absname("target")
                    end
            end;
        TargetDir ->
            filename:absname(TargetDir)
    end.

%%
%% Look for overlay_vars file reference. The user can override this from the
%% command line (i.e. globals), so we check there first and then fall back to
%% what is present in the reltool.config file
%%
overlay_vars(ReltoolConfig) ->
    case rebar_config:get_global(overlay_vars, undefined) of
        undefined ->
            case lists:keysearch(overlay_vars, 1, ReltoolConfig) of
                {value, {overlay_vars, File}} ->
                    File;
                false ->
                    undefined
            end;
        File ->
            File
    end.


validate_rel_apps(ReltoolServer, {sys, ReltoolConfig}) ->
    case lists:keysearch(rel, 1, ReltoolConfig) of
        {value, {rel, _Name, _Vsn, Apps}} ->
            %% Identify all the apps that do NOT exist, based on what's available
            %% from the reltool server
            Missing = lists:sort([App || App <- Apps,
                                         app_exists(App, ReltoolServer) == false]),
            case Missing of
                [] ->
                    ok;
                _ ->
                    ?ABORT("Apps in {rel, ...} section not found by reltool: ~p\n", [Missing])
            end;
        {value, Rel} ->
            %% Invalid release format!
            ?ABORT("Invalid {rel, ...} section in reltools.config: ~p\n", [Rel]);
        false ->
            ok
    end.

app_exists(App, Server) when is_atom(App) ->
    case reltool_server:get_app(Server, App) of
        {ok, _} ->
            true;
        _ ->
            false
    end;
app_exists(AppTuple, Server) when is_tuple(AppTuple) ->
    app_exists(element(1, AppTuple), Server).


run_reltool(Server, _Config, ReltoolConfig) ->
    case reltool:get_target_spec(Server) of
        {ok, Spec} ->
            %% Pull the target dir and make sure it exists
            TargetDir = target_dir(ReltoolConfig),
            mk_target_dir(TargetDir),

            %% Dump the spec, if necessary
            dump_spec(Spec),

            %% Have reltool actually run
            case reltool:eval_target_spec(Spec, code:root_dir(), TargetDir) of
                ok ->
                    ok;
                {error, Reason} ->
                    ?ABORT("Failed to generate target from spec: ~p\n", [Reason])
            end,

            %% Initialize overlay vars with some basics (that can get overwritten)
            OverlayVars0 = [{erts_vsn, "erts-" ++ erlang:system_info(version)}],

            %% Load up any variables specified by overlay_vars
            OverlayVars = case overlay_vars(ReltoolConfig) of
                              undefined ->
                                  dict:from_list(OverlayVars0);
                              File ->
                                  case file:consult(File) of
                                      {ok, Terms} ->
                                          dict:from_list(OverlayVars0 ++ Terms);
                                      {error, Reason2} ->
                                          ?ABORT("Unable to load overlay_vars from ~s: ~p\n",
                                                 [File, Reason2])
                                  end
                          end,

            %% Finally, overlay the files specified by the overlay section
            case lists:keysearch(overlay, 1, ReltoolConfig) of
                {value, {overlay, Overlay}} when is_list(Overlay) ->
                    execute_overlay(Overlay, OverlayVars, rebar_utils:get_cwd(), TargetDir);
                {value, _} ->
                    ?ABORT("{overlay, [...]} entry in reltool.config must be a list.\n", []);
                false ->
                    ?INFO("No {overlay, [...]} found in reltool.config.\n", [])
            end;

        {error, Reason} ->
            ?ABORT("Unable to generate spec: ~s\n", [Reason])
    end.


mk_target_dir(TargetDir) ->
    case file:make_dir(TargetDir) of
        ok ->
            ok;
        {error, eexist} ->
            %% Output directory already exists; if force=1, wipe it out
            case rebar_config:get_global(force, "0") of
                "1" ->
                    rebar_file_utils:rm_rf(TargetDir),
                    ok = file:make_dir(TargetDir);
                _ ->
                    ?ERROR("Release target directory ~p already exists!\n", [TargetDir]),
                    ?FAIL
            end
    end.


dump_spec(Spec) ->
    case rebar_config:get_global(dump_spec, "0") of
        "1" ->
            SpecBin = list_to_binary(io_lib:print(Spec, 1, 120, -1)),
            ok = file:write_file("reltool.spec", SpecBin);
        _ ->
            ok
    end.


execute_overlay([], _Vars, _BaseDir, _TargetDir) ->
    ok;
execute_overlay([{mkdir, Out} | Rest], Vars, BaseDir, TargetDir) ->
    OutFile = render(filename:join([TargetDir, Out, "dummy"]), Vars),
    ok = filelib:ensure_dir(OutFile),
    ?DEBUG("Created dir ~s\n", [filename:dirname(OutFile)]),
    execute_overlay(Rest, Vars, BaseDir, TargetDir);
execute_overlay([{copy, In} | Rest], _Vars, BaseDir, TargetDir) ->
    execute_overlay([{copy, In, ""} | Rest], _Vars, BaseDir, TargetDir);
execute_overlay([{copy, In, Out} | Rest], Vars, BaseDir, TargetDir) ->
    InFile = render(filename:join(BaseDir, In), Vars),
    OutFile = render(filename:join(TargetDir, Out), Vars),
    case filelib:is_dir(InFile) of
        true ->
            ok;
        false ->
            ok = filelib:ensure_dir(OutFile)
    end,
    rebar_utils:sh(?FMT("cp -R ~p ~p", [InFile, OutFile]), []),
    execute_overlay(Rest, Vars, BaseDir, TargetDir);
execute_overlay([{template, In, Out} | Rest], Vars, BaseDir, TargetDir) ->
    InFile = render(filename:join(BaseDir, In), Vars),
    {ok, InFileData} = file:read_file(InFile),
    OutFile = render(filename:join(TargetDir, Out), Vars),
    ok = filelib:ensure_dir(OutFile),
    case file:write_file(OutFile, render(InFileData, Vars)) of
        ok ->
            ok = apply_file_info(InFile, OutFile),
            ?DEBUG("Templated ~p\n", [OutFile]),
            execute_overlay(Rest, Vars, BaseDir, TargetDir);
        {error, Reason} ->
            ?ABORT("Failed to template ~p: ~p\n", [OutFile, Reason])
    end;
execute_overlay([{create, Out, Contents} | Rest], Vars, BaseDir, TargetDir) ->
    OutFile = render(filename:join(TargetDir, Out), Vars),
    ok = filelib:ensure_dir(OutFile),
    case file:write_file(OutFile, Contents) of
        ok ->
            ?DEBUG("Created ~p\n", [OutFile]),
            execute_overlay(Rest, Vars, BaseDir, TargetDir);
        {error, Reason} ->
            ?ABORT("Failed to create ~p: ~p\n", [OutFile, Reason])
    end;
execute_overlay([{replace, Out, Regex, Replacement} | Rest],
                Vars, BaseDir, TargetDir) ->
    execute_overlay([{replace, Out, Regex, Replacement, []} | Rest], Vars, BaseDir, TargetDir);
execute_overlay([{replace, Out, Regex, Replacement, Opts} | Rest],
                Vars, BaseDir, TargetDir) ->
    Filename = render(filename:join(TargetDir, Out), Vars),
    {ok, OrigData} = file:read_file(Filename),
    Data = re:replace(OrigData, Regex, Replacement, [global, {return, binary}] ++ Opts),
    case file:write_file(Filename, Data) of
        ok ->
            ?DEBUG("Edited ~s: s/~s/~s/\n", [Filename, Regex, Replacement]),
            execute_overlay(Rest, Vars, BaseDir, TargetDir);
        {error, Reason} ->
            ?ABORT("Failed to edit ~p: ~p\n", [Filename, Reason])
    end;
execute_overlay([Other | _Rest], _Vars, _BaseDir, _TargetDir) ->
    {error, {unsupported_operation, Other}}.



%%
%% Render a binary to a string, using mustache and the specified context
%%
render(Bin, Context) ->
    %% Be sure to escape any double-quotes before rendering...
    Str0 = re:replace(Bin, "\\\\", "\\\\\\", [global, {return, list}]),
    Str1 = re:replace(Str0, "\"", "\\\\\"", [global, {return,list}]),
    mustache:render(Str1, Context).


apply_file_info(InFile, OutFile) ->
    {ok, FileInfo} = file:read_file_info(InFile),
    ok = file:write_file_info(OutFile, FileInfo).
