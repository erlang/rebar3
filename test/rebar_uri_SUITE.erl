-module(rebar_uri_SUITE).

-export([all/0,
         parse/1,
         append_path/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").

all() ->
    [parse].

parse(_Config) ->
    #{scheme := Scheme, host := Host, path := Path} = rebar_uri:parse("https://repo.hex.pm"),
    ?assertEqual("https", Scheme),
    ?assertEqual("repo.hex.pm", Host),
    ?assert(lists:member(Path, ["", "/"])),

    #{scheme := Scheme2, host := Host2, port := Port2, path := Path2, query := Query2} =
        rebar_uri:parse("https://repo.hex.pm:443?foo=bar"),
    ?assertEqual("https", Scheme2),
    ?assertEqual("repo.hex.pm", Host2),
    ?assertEqual(443, Port2),
    ?assert(lists:member(Path2, ["", "/"])),
    ?assertEqual("foo=bar", Query2),

    #{scheme := Scheme3, host := Host3, path := Path3, query := Query3} =
        rebar_uri:parse("https://repo.hex.pm/over/here?foo=bar"),
    ?assertEqual("https", Scheme3),
    ?assertEqual("repo.hex.pm", Host3),
    ?assertEqual("/over/here", Path3),
    ?assertEqual("foo=bar", Query3).

append_path(_Config) ->
    %% OTP version differences
    {ok, Val1} = rebar_utils:append_path("https://repo.hex.pm", "/repos/org"),
    ?assert(lists:member(Val1, [
        "https://repo.hex.pm/repos/org",
        "https://repo.hex.pm:443/repos/org"
    ])),
    {ok, Val2} = rebar_utils:append_path("https://repo.hex.pm?foo=bar", "/repos/org"),
    ?assert(lists:member(Val2, [
        "https://repo.hex.pm/repos/org?foo=bar",
        "https://repo.hex.pm:443/repos/org?foo=bar"
    ])),
    ?assertEqual({ok, "https://repo.hex.pm:443/repos/org?foo=bar"},
                 rebar_utils:append_path("https://repo.hex.pm:443?foo=bar", "/repos/org")).
