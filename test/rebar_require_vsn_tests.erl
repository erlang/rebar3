-module(rebar_require_vsn_tests).

-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

version_tuple_test_() ->
    [%% typical cases
     ?_assert(rebar_require_vsn:version_tuple("R15B", "eunit") =:= {15, 0}),
     ?_assert(rebar_require_vsn:version_tuple("R15B01", "eunit") =:= {15, 1}),
     ?_assert(rebar_require_vsn:version_tuple("R15B02", "eunit") =:= {15, 2}),
     ?_assert(rebar_require_vsn:version_tuple("R15B03-1", "eunit") =:= {15, 3}),
     ?_assert(rebar_require_vsn:version_tuple("R15B03", "eunit") =:= {15, 3}),
     ?_assert(rebar_require_vsn:version_tuple("R16B", "eunit") =:= {16, 0}),
     ?_assert(rebar_require_vsn:version_tuple("R16B01", "eunit") =:= {16, 1}),
     ?_assert(rebar_require_vsn:version_tuple("R16B02", "eunit") =:= {16, 2}),
     ?_assert(rebar_require_vsn:version_tuple("R16B03", "eunit") =:= {16, 3}),
     ?_assert(rebar_require_vsn:version_tuple("R16B03-1", "eunit") =:= {16, 3}),
     ?_assert(rebar_require_vsn:version_tuple("17", "eunit") =:= {17, 0}),
     %% error cases
     ?_assertException(throw, rebar_abort, rebar_require_vsn:version_tuple("", "eunit")),
     ?_assertException(throw, rebar_abort, rebar_require_vsn:version_tuple("abc", "eunit"))
    ].
