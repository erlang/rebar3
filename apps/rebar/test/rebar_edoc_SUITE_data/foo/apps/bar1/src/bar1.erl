-module(bar1).
-export([bar1/0]).
-export_type([barer1/0]).

-type barer1() :: string().

% @doc Bar1 bars the bar.
-spec bar1() -> barer1().
bar1() -> "Barer1".