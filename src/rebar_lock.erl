-module(rebar_lock).

-export([create/1]).

create(State) ->
    LockDeps = rebar_state:get(State, locks, []),
    ok = file:write_file("./rebar.lock", io_lib:format("~p.~n", [LockDeps])).
