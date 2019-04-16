%% @doc one docline is fine
%% @doc a second docline causes a failure
%% @doc if not, then a & causes a bad ref error.
-module(bad_bar2).
-export([bar2/0]).
-export_type([barer2/0]).

-type barer2() :: string().

% @doc Bar2 bars the bar2.
-spec bar2() -> barer2().
bar2() -> "Barer2".
