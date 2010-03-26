-module({{module}}).

-export([new/0,
         myfunction/1]).

-on_load(init/0).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

init() ->
    case code:priv_dir({{module}}) of
        {error, bad_name} ->
            SoName = filename:join("../priv", {{module}});
        Dir ->
            SoName = filename:join(Dir, {{module}})
    end,
    erlang:load_nif(SoName, 0).

new() ->
    "NIF library not loaded".

myfunction(Ref) ->
    "NIF library not loaded".

%% ===================================================================
%% EUnit tests
%% ===================================================================
-ifdef(TEST).

basic_test() ->
    {ok, Ref} = new(),
    ok = myfunction(Ref).

-endif.
