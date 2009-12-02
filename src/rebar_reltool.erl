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

%% ===================================================================
%% Public API
%% ===================================================================

generate(Config, ReltoolFile) ->
    %% Load the reltool configuration from the file
    case file:consult(ReltoolFile) of
        {ok, [{sys, ReltoolConfig}]} ->
            ok;
        Other ->
            ReltoolConfig = undefined,
            ?ERROR("Failed to load expected config from ~s: ~p\n", [ReltoolFile, Other]),
            ?FAIL
    end,

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
    ok.



%% ===================================================================
%% Internal functions
%% ===================================================================

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

run_reltool(Config, ReltoolConfig) ->
    {ok, Server} = reltool:start_server([{sys, ReltoolConfig}]),
    {ok, Spec} = reltool:get_target_spec(Server),
    TargetDir = target_dir(Config, ReltoolConfig),
    ok = file:make_dir(TargetDir),
    case reltool:eval_target_spec(Spec, code:root_dir(), TargetDir) of
        ok ->
            ok;
        {error, Reason} ->
            ?ERROR("Failed to generate target from spec: ~p\n", [Reason]),
            ?FAIL
    end.
    
