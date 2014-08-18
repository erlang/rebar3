-module(rebar_lock).

-export([update/3]).

create(State) ->
    LockDeps = [],
    ok = file:write_file("./rebar.lock", io_lib:format("~p.~n", [LockDeps])).

update(State, App, Source) ->
    New = rebar_fetch:new(rebar_app_info:dir(App), rebar_app_info:name(App), rebar_app_info:original_vsn(App), Source),
    {ok, [Terms]} = file:consult("./rebar.lock"),
    LockDeps = lists:keyreplace(rebar_app_info:name(App), 1, Terms, New),
    ok = file:write_file("./rebar.lock", io_lib:format("~p.~n", [LockDeps])).
