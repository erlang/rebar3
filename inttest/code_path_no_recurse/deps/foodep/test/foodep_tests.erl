-module(foodep_tests).
-include_lib("eunit/include/eunit.hrl").

foodep_test() ->
    ?assert(foodep:foodep()).
