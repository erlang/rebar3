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
-module(rebar_config).

-export([new/1,
         get_modules/2,
         get_list/3]).

-include("rebar.hrl").

-record(config, { dir,
                  opts }).


%% ===================================================================
%% Public API
%% ===================================================================

new(Dir) ->
    {ok, DefaultConfig} = application:get_env(rebar, default_config),
    BaseDict = orddict:from_list(DefaultConfig),

    %% Load terms from rebar.config, if it exists
    ConfigFile = filename:join([Dir, "rebar.config"]),
    case file:consult(ConfigFile) of
        {ok, Terms} ->
            Dict = merge_terms(Terms, BaseDict);
        {error, enoent} ->
            Dict = BaseDict;
        Other ->
            ?WARN("Failed to load ~s: ~p\n", [ConfigFile, Other]),
            ?FAIL,
            Dict = BaseDict
    end,
    #config { dir = Dir, opts = Dict }.


get_modules(Config, app) ->
    case orddict:find(app_modules, Config#config.opts) of
        error ->
            [];
        {ok, Modules} ->
            Modules
    end.

get_list(Config, Key, Default) ->
    case orddict:find(Key, Config#config.opts) of
        error ->
            Default;
        {ok, List} ->
            List
    end.
    


%% ===================================================================
%% Internal functions
%% ===================================================================

merge_terms([], Dict) ->
    Dict;
merge_terms([{Key, Value} | Rest], Dict) ->
    merge_terms(Rest, orddict:append(Key, Value, Dict));
merge_terms([_ | Rest], Dict) ->
    merge_terms(Rest, Dict).
