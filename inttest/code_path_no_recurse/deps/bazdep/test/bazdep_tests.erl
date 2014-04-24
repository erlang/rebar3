-module(bazdep_tests).
-include_lib("eunit/include/eunit.hrl").

bazdep_test() ->
    ?assert(bazdep:bazdep() =:= bazdep).
