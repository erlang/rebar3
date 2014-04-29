-module(foo).

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

foo_test() ->
    ?assert(true).

-endif.
