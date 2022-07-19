-module(foo).

-export([foo/0, bar1/0, bar2/0]).

-export_type([fooer/0]).

-type fooer() :: string().

% @doc Foo function returns fooer.
-spec foo() -> fooer().
foo() -> "fooer".

% @doc Bar1 function returns barer1.
-spec bar1() -> bar1:barer1().
bar1() -> bar1:bar1().

% @doc Bar2 functions returns barer2.
-spec bar2() -> bar2:barer2().
bar2() -> bar2:bar2().