-module(codepath_tests).
-include_lib("eunit/include/eunit.hrl").

codepath_test() ->
    ?assertEqual({module, codepath}, code:ensure_loaded(codepath)),
    ?assertEqual({module, foodep}, code:ensure_loaded(foodep)),
    ?assertEqual({module, bazdep}, code:ensure_loaded(bazdep)),
    ?assert(codepath:codepath()).

unuseddep_test() ->
    ?assertEqual(non_existing, code:which(unuseddep)),
    ?assertEqual({error, nofile}, code:ensure_loaded(unuseddep)).
