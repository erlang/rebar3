%%%-------------------------------------------------------------------
%% @doc {{name}} public API
%% @end
%%%-------------------------------------------------------------------

-module({{name}}_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {{name}}_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
