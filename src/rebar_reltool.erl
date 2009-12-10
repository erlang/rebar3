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

%% ===================================================================
%% Public API
%% ===================================================================

generate(Config, ReltoolFile) ->
    %% Load the reltool configuration from the file
    ReltoolConfig = load_config(ReltoolFile),

    %% Spin up reltool server and load our config into it
    {ok, Server} = reltool:start_server([{sys, ReltoolConfig}]),

    %% Do some validation of the reltool configuration; error messages out of
    %% reltool are still pretty cryptic
    validate_rel_apps(Server, ReltoolConfig),

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
    TargetDir = target_dir(Config, ReltoolConfig),
    rebar_file_utils:rm_rf(TargetDir),
    rebar_file_utils:delete_each(["reltool.spec"]).



%% ===================================================================
%% Internal functions
%% ===================================================================

load_config(ReltoolFile) ->
    %% Load the reltool configuration from the file
    case file:consult(ReltoolFile) of
        {ok, [{sys, ReltoolConfig}]} ->
            ReltoolConfig;
        Other ->
            ?ERROR("Failed to load expected config from ~s: ~p\n", [ReltoolFile, Other]),
            ?FAIL
    end.
    

%%
%% Determine the name of the target directory; try the user provided name
%% first, or fall back to the release name if that's available. If neither
%% is available, just use "target"
%%
target_dir(Config, ReltoolConfig) ->
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

validate_rel_apps(ReltoolServer, ReltoolConfig) ->
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
    {ok, Server} = reltool:start_server([{sys, ReltoolConfig}]),
    case reltool:get_target_spec(Server) of
        {ok, Spec} ->
            dump_spec(Spec),
            TargetDir = target_dir(Config, ReltoolConfig),
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
            end,            
            case reltool:eval_target_spec(Spec, code:root_dir(), TargetDir) of
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
    

dump_spec(Spec) ->
    case rebar_config:get_global(dump_spec, "0") of
        "1" ->
            SpecBin = list_to_binary(io_lib:print(Spec, 1, 120, -1)),
            ok = file:write_file("reltool.spec", SpecBin);
        _ ->
            ok
    end.
            
