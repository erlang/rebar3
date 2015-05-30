%%% external alias for rebar_agent
-module(r3).
-export([do/1, do/2]).

do(Command) -> rebar_agent:do(Command).

do(Namespace, Command) -> rebar_agent:do(Namespace, Command).
