-module({{modid}}).

-export([my_func/0]).

-ifdef(TEST).
-compile(export_all).
-endif.

my_func() ->
    ok.
