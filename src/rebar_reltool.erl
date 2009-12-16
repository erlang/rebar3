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
    %% Load the reltool configuration from the file
    ReltoolConfig = load_config(ReltoolFile),

    %% Spin up reltool server and load our config into it
    {ok, Server} = reltool:start_server([sys_tuple(ReltoolConfig)]),

    %% Do some validation of the reltool configuration; error messages out of
    %% reltool are still pretty cryptic
    validate_rel_apps(Server, sys_tuple(ReltoolConfig)),

    %% Finally, run reltool
    case catch(run_reltool(Config, ReltoolConfig)) of
        ok ->
            ok;
        {error, failed} ->
            ?FAIL;
        Other2 ->
            ?ERROR("Unexpected error: ~p\n", [Other2]),
            ?FAIL
    end.


clean(Config, ReltoolFile) ->
    ReltoolConfig = load_config(ReltoolFile),
    TargetDir = target_dir(Config, sys_tuple(ReltoolConfig)),
    rebar_file_utils:rm_rf(TargetDir),
    rebar_file_utils:delete_each(["reltool.spec"]).



%% ===================================================================
%% Internal functions
%% ===================================================================

sys_tuple(ReltoolConfig) ->
    case lists:keysearch(sys, 1, ReltoolConfig) of
        {value, {sys, Data}} ->
            {sys, Data};
        false ->
            ?ERROR("Failed to find {sys, ...} tuple in reltool.config.", []),
            ?FAIL
    end.

load_config(ReltoolFile) ->
    %% Load the reltool configuration from the file
    case file:consult(ReltoolFile) of
        {ok, Terms} ->
            Terms;
        Other ->
            ?ERROR("Failed to load expected config from ~s: ~p\n", [ReltoolFile, Other]),
            ?FAIL
    end.
    

%%
%% Determine the name of the target directory; try the user provided name
%% first, or fall back to the release name if that's available. If neither
%% is available, just use "target"
%%
target_dir(Config, {sys, ReltoolConfig}) ->
  case rebar_config:get(Config, target_name, undefined) of
      undefined ->
          case lists:keysearch(rel, 1, ReltoolConfig) of
              {value, {rel, Name, _Vsn, _Apps}} ->
                  Name;
              false ->
                  "target"
          end;
      Name ->
          Name
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
                    ?ERROR("Apps in {rel, ...} section not found by reltool: ~p\n", [Missing]),
                    ?FAIL
            end;
        {value, Rel} ->
            %% Invalid release format!
            ?ERROR("Invalid {rel, ...} section in reltools.config: ~p\n", [Rel]),
            ?FAIL;
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
                       

run_reltool(Config, ReltoolConfig) ->
    {ok, Server} = reltool:start_server([sys_tuple(ReltoolConfig)]),
    case reltool:get_target_spec(Server) of
        {ok, Spec} ->
            TargetDir = target_dir(Config, sys_tuple(ReltoolConfig)),
            mk_target_dir(TargetDir),

            %% Post process the specification with rebar directives (if any exist)
            FinalSpec = case lists:keysearch(rebar, 1, ReltoolConfig) of
                            {value, {rebar, RebarConfig}} ->
                                process_rebar_specs(RebarConfig, Spec);
                            false ->
                                Spec
                        end,

            dump_spec(FinalSpec),

            case reltool:eval_target_spec(FinalSpec, code:root_dir(), TargetDir) of
                ok ->
                    ok;
                {error, Reason} ->
                    ?ERROR("Failed to generate target from spec: ~p\n", [Reason]),
                    ?FAIL
            end;
        {error, Reason} ->
            ?ERROR("Unable to generate spec: ~s\n", [Reason]),
            ?FAIL
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
            
process_rebar_specs([], Spec) ->
    Spec;
process_rebar_specs([{overlay, Source} | Rest], Spec) ->
    case file:list_dir(Source) of
        {ok, Files} ->
            OverlaySpec = spec_copy_overlay(Files, Source, []),
            process_rebar_specs(Rest, Spec ++ OverlaySpec);
        {error, Reason} ->
            ?ERROR("Failed to list overlay directory ~p: ~p\n", [Source, Reason]),
            ?FAIL
    end;
process_rebar_specs([{empty_dirs, Dirs} | Rest], Spec) ->
    Spec2 = lists:foldl(fun(Dir, SpecAcc) ->
                                spec_create_dir(filename:split(Dir), SpecAcc)
                        end, Spec, Dirs),
    process_rebar_specs(Rest, Spec2);
process_rebar_specs([ Other | Rest], Spec) ->
    ?WARN("Ignoring unknown rebar spec: ~p\n", [Other]),
    process_rebar_specs(Rest, Spec).


spec_create_dir([], Spec) ->
    Spec;
spec_create_dir([Path | Rest], Spec) ->
    case lists:keysearch(Path, 2, Spec) of
        {value, {create_dir, Path, Subspec}} ->
            %% Directory already exists; process down into
            %% Note: this is not tail recursive, but unless the directory structure
            %% is insanely deep, shouldn't be a problem
            lists:keystore(Path, 2, Spec, {create_dir, Path, spec_create_dir(Rest, Subspec)});
        {value, Other} ->
            %% Collision -- something other than create_dir is associated with this
            %% portion of our path name.
            ?ERROR("Collision of path name with existing tuple in spec: ~p\n", [Other]),
            ?FAIL;
        false ->
            %% Directory doesn't yet exist
            [{create_dir, Path, spec_create_dir(Rest, [])} | Spec]
    end.

spec_copy_overlay([], _Dir, Acc) ->
    lists:reverse(Acc);
spec_copy_overlay([F | Rest], Dir, Acc) ->
    Filename = filename:join(Dir, F),
    {ok, Info} = file:read_file_info(Filename),
    case Info#file_info.type of
        directory ->
            {ok, Files} = file:list_dir(Filename),
            Entry = {create_dir, filename:basename(Filename), spec_copy_overlay(Files, Filename, [])},
            spec_copy_overlay(Rest, Dir, [Entry | Acc]);
        regular ->
            Entry = {copy_file, filename:basename(F), filename:absname(Filename)},
            spec_copy_overlay(Rest, Dir, [Entry | Acc]);
        Other ->
            ?DEBUG("Skipping ~p of type ~p\n", [F, Other]),
            spec_copy_overlay(Rest, Dir, Acc)
    end.
    
