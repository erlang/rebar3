-module(bar2).
-export([bar2/0]).
-export_type([barer2/0]).

-type barer2() :: string().

% @doc Bar2 bars the bar2.
-spec bar2() -> barer2().
bar2() -> "Barer2".