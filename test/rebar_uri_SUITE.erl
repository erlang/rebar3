-module(rebar_uri_SUITE).

-export([all/0,
         parse/1,
         append_path/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").

all() ->
    [parse, append_path].

parse(_Config) ->
    #{scheme := Scheme, host := Host, path := Path} = rebar_uri:parse("https://repo.hex.pm"),
    ?assertEqual("https", Scheme),
    ?assertEqual("repo.hex.pm", Host),
    ?assertEqual(Path, "/"), % Normalize on OTP-23 behaviour.

    #{scheme := Scheme2, host := Host2, port := Port2, path := Path2, query := Query2} =
        rebar_uri:parse("https://repo.hex.pm:443?foo=bar"),
    ?assertEqual("https", Scheme2),
    ?assertEqual("repo.hex.pm", Host2),
    ?assertEqual(443, Port2),
    ?assertEqual(Path2, "/"), % Normalize on old http_uri behaviour
    ?assertEqual("foo=bar", Query2),

    #{scheme := Scheme3, host := Host3, path := Path3, query := Query3} =
        rebar_uri:parse("https://repo.hex.pm/over/here?foo=bar"),
    ?assertEqual("https", Scheme3),
    ?assertEqual("repo.hex.pm", Host3),
    ?assertEqual("/over/here", Path3),
    ?assertEqual("foo=bar", Query3),

    %% override default port and get it parsed as such
    ?assertMatch(#{port := 1337},
                 rebar_uri:parse("https://repo.hex.pm/",
                                 [{scheme_defaults, [{https,1337}]}])),
    ok.

append_path(_Config) ->
    %% Default port for the proto is omitted if not mentioned originally
    {ok, Val1} = rebar_uri:append_path("https://repo.hex.pm/", "/repos/org"),
    ?assertEqual("https://repo.hex.pm/repos/org", Val1),
    %% QS elements come after the path
    {ok, Val2} = rebar_uri:append_path("https://repo.hex.pm?foo=bar", "/repos/org"),
    ?assertEqual("https://repo.hex.pm/repos/org?foo=bar", Val2),
    %% If the port is explicitly mentioned, keep it.
    ?assertEqual({ok, "https://repo.hex.pm:443/repos/org?foo=bar"},
                 rebar_uri:append_path("https://repo.hex.pm:443?foo=bar", "/repos/org")).
