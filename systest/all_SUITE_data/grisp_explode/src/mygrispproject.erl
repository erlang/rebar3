% @doc mygrispproject public API.
% @end
-module(mygrispproject).

-behavior(application).

% Callbacks
-export([start/2]).
-export([stop/1]).

%--- Callbacks -----------------------------------------------------------------

start(_Type, _Args) -> mygrispproject_sup:start_link().

stop(_State) -> ok.
