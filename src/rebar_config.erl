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

-export([new/0, new/1,
         get/3, get_list/3,
         delete/2,
         set_global/2, get_global/2,
         is_verbose/0]).

-include("rebar.hrl").

-record(config, { dir,
                  opts }).


%% ===================================================================
%% Public API
%% ===================================================================

new() ->
    #config { dir = rebar_utils:get_cwd(),
              opts = []}.

new(ParentConfig) ->
    %% Load terms from rebar.config, if it exists
    Dir = rebar_utils:get_cwd(),
    ConfigFile = filename:join([Dir, "rebar.config"]),
    case file:consult(ConfigFile) of
        {ok, Terms} ->
            Opts = Terms ++ ParentConfig#config.opts;
        {error, enoent} ->
            Opts = ParentConfig#config.opts;
        Other ->
            Opts = undefined, % Keep erlc happy
            ?WARN("Failed to load ~s: ~p\n", [ConfigFile, Other]),
            ?FAIL
    end,
    #config { dir = Dir, opts = Opts }.

get_list(Config, Key, Default) ->
    get(Config, Key, Default).

get(Config, Key, Default) ->
    proplists:get_value(Key, Config#config.opts, Default).

delete(Config, Key) ->
    Config#config { opts = proplists:delete(Key, Config#config.opts) }.
    
set_global(Key, Value) ->
    application:set_env(rebar_global, Key, Value).

get_global(Key, Default) ->
    case application:get_env(rebar_global, Key) of
        undefined ->
            Default;
        {ok, Value} ->
            Value
    end.

is_verbose() ->
    case get_global(verbose, "0") of
        "1" ->
            true;
        _ ->
            false
    end.


%% ===================================================================
%% Internal functions
%% ===================================================================
