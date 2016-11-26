%%% @doc external alias for `rebar_agent' for more convenient
%%% calls from a shell.
-module(r3).
-export([do/1, do/2]).

%% @doc alias for `rebar_agent:do/1'
-spec do(atom()) -> ok | {error, term()}.
do(Command) -> rebar_agent:do(Command).

%% @doc alias for `rebar_agent:do/2'
-spec do(atom(), atom()) -> ok | {error, term()}.
do(Namespace, Command) -> rebar_agent:do(Namespace, Command).
