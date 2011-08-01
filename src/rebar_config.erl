%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
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
-module(rebar_config).

-export([new/0, new/1, base_config/1,
         get/3, get_local/3, get_list/3,
         get_all/2,
         set/3,
         set_global/2, get_global/2,
         is_verbose/0, get_jobs/0]).

-include("rebar.hrl").

-record(config, { dir :: file:filename(),
                  opts :: list() }).

%% Types that can be used from other modules -- alphabetically ordered.
-export_type([config/0]).

%% data types
-opaque config() :: #config{}.

%% ===================================================================
%% Public API
%% ===================================================================

base_config(#config{opts=Opts0}) ->
    ConfName = rebar_config:get_global(config, "rebar.config"),
    new(Opts0, ConfName).

new() ->
    #config { dir = rebar_utils:get_cwd(),
              opts = [] }.

new(ConfigFile) when is_list(ConfigFile) ->
    case consult_file(ConfigFile) of
        {ok, Opts} ->
            #config { dir = rebar_utils:get_cwd(),
                      opts = Opts };
        Other ->
            ?ABORT("Failed to load ~s: ~p~n", [ConfigFile, Other])
    end;
new(_ParentConfig=#config{opts=Opts0})->
    new(Opts0, "rebar.config").

new(Opts0, ConfName) ->
    %% Load terms from rebar.config, if it exists
    Dir = rebar_utils:get_cwd(),
    ConfigFile = filename:join([Dir, ConfName]),
    Opts = case consult_file(ConfigFile) of
               {ok, Terms} ->
                   %% Found a config file with some terms. We need to
                   %% be able to distinguish between local definitions
                   %% (i.e. from the file in the cwd) and inherited
                   %% definitions. To accomplish this, we use a marker
                   %% in the proplist (since order matters) between
                   %% the new and old defs.
                   Terms ++ [local] ++
                       [Opt || Opt <- Opts0, Opt /= local];
               {error, enoent} ->
                   [local] ++
                       [Opt || Opt <- Opts0, Opt /= local];
               Other ->
                   ?ABORT("Failed to load ~s: ~p\n", [ConfigFile, Other])
           end,

    #config { dir = Dir, opts = Opts }.

get(Config, Key, Default) ->
    proplists:get_value(Key, Config#config.opts, Default).

get_list(Config, Key, Default) ->
    get(Config, Key, Default).

get_local(Config, Key, Default) ->
    proplists:get_value(Key, local_opts(Config#config.opts, []), Default).

get_all(Config, Key) ->
    proplists:get_all_values(Key, Config#config.opts).

set(Config, Key, Value) ->
    Opts = proplists:delete(Key, Config#config.opts),
    Config#config { opts = [{Key, Value} | Opts] }.

set_global(jobs=Key, Value) when is_list(Value) ->
    set_global(Key, list_to_integer(Value));
set_global(jobs=Key, Value) when is_integer(Value) ->
    application:set_env(rebar_global, Key, erlang:max(1,Value));
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
    get_global(verbose, "0") =:= "1".

get_jobs() ->
    get_global(jobs, 3).

%% ===================================================================
%% Internal functions
%% ===================================================================

consult_file(File) ->
    ?DEBUG("Consult config file ~p~n", [File]),
    file:consult(File).

local_opts([], Acc) ->
    lists:reverse(Acc);
local_opts([local | _Rest], Acc) ->
    lists:reverse(Acc);
local_opts([Item | Rest], Acc) ->
    local_opts(Rest, [Item | Acc]).
