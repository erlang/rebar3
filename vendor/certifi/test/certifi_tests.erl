-module(certifi_tests).

-include_lib("eunit/include/eunit.hrl").

-ifdef('OTP_20_AND_ABOVE').
reproducible_module_test() ->
    %% When compiled with +deterministic, only version is left out.
    ?assertMatch([{version,[_|_]}], certifi:module_info(compile)).
-endif.

cacerts_test_() ->
    Certs = [Cert1, Cert2, Cert3 | _] = certifi:cacerts(),
    [?_assertEqual(128, length(Certs))
    ,?_assertMatch(<<48,130,5,192,48,130,3,168,160,3,2,1,2,2,16,30,191,89,80,184,_/binary>>, Cert1)
    ,?_assertMatch(<<48,130,2,101,48,130,1,235,160,3,2,1,2,2,16,120,143,39,92,_/binary>>, Cert2)
    ,?_assertMatch(<<48,130,5,239,48,130,3,215,160,3,2,1,2,2,8,13,211,227,188,_/binary>>, Cert3)
    ,?_assertMatch(<<48,130,3,117,48,130,2,93,160,3,2,1,2,2,11,4,0,0,0,0,1,21,75,90,195,148,48,13,6,_/binary>>, lists:last(Certs))
    ].
