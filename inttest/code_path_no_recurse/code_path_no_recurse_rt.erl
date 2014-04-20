-module(code_path_no_recurse_rt).
-export([files/0,
         run/1]).

files() ->
    [
     {copy, "../../rebar", "rebar"},
     {copy, "rebar.config", "rebar.config"},
     {copy, "src", "src"},
     {copy, "test", "test"},
     {copy, "deps", "deps"}
    ].

run(_Dir) ->
    retest:log(info, "Compile project~n"),
    {ok, _} = retest:sh("./rebar -v compile"),
    retest:log(info, "Run eunit with referenced deps on the code path~n"),
    {ok, _} = retest:sh("./rebar -v eunit"),
    ok.
