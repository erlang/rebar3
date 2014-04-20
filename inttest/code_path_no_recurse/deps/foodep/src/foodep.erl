-module(foodep).

-export([foodep/0]).

foodep() ->
    bazdep:bazdep() =:= bazdep.
