-module(rebar_compiler_format_SUITE).
-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(EOL, lists:flatten(io_lib:format("~n",[]))).

all() ->
    [minimal, nocolor].

init_per_testcase(minimal, Config) ->
    Conf = dict:from_list([{compiler_error_format, minimal}]),
    [{conf, Conf} | init_per_testcase(regular, Config)];
init_per_testcase(_, Config) ->
    OriginalTerm = os:getenv("TERM"),
    os:putenv("TERM", "dumb"), % disable color
    application:set_env(cf, colour_term, cf_term:has_color("dumb")),
    FileName = filename:join(?config(priv_dir, Config), "oracle.erl"),
    ok = file:write_file(FileName, oracle()),
    EmptyFileName = filename:join(?config(priv_dir, Config), "empty.erl"),
    ok = file:write_file(EmptyFileName, ""),
    Conf = dict:from_list([{compiler_error_format, rich}]),
    [{conf, Conf}, {file, FileName}, {empty_file, EmptyFileName}, {term, OriginalTerm} | Config].

end_per_testcase(_, Config) ->
    case ?config(term, Config) of
        false ->
            os:unsetenv("TERM"),
            application:unset_env(cf, colour_term);
        Original ->
            os:putenv("TERM", Original),
            application:set_env(cf, colour_term, cf_term:has_color("Original"))
    end,
    Config.

oracle() ->
    "-module(noline_end);\n"
    ++ lists:duplicate(9, $\n) ++
    "first character on line 11.\n"
    ++ lists:duplicate(99, $\n) ++
    "case \tX of ^whatever % on line 111\n".

minimal() ->
    [{doc, "showing minimal (default) output"}].
minimal(Config) ->
    Path = ?config(file, Config),
    Conf = ?config(conf, Config),
    ?assertEqual(Path++":1:20: => unexpected token: ;"++?EOL,
                 rebar_compiler_format:format(Path, {1,20}, "=> ", "unexpected token: ;", Conf)),
    ?assertEqual(Path++":11:1: some message"++?EOL,
                 rebar_compiler_format:format(Path, {11,1}, "", "some message", Conf)),
    ?assertEqual(Path++":111:12: the character '^' is not expected here."++?EOL,
                 rebar_compiler_format:format(Path, {111,12}, "", "the character '^' is not expected here.", Conf)),
    ?assertEqual(Path++":-23:-42: invalid ranges."++?EOL,
                 rebar_compiler_format:format(Path, {-23,-42}, "", "invalid ranges.", Conf)),
    ?assertEqual(Path++":-23:-42: invalid ranges."++?EOL,
                 rebar_compiler_format:format(Path, {-23,-42}, "", "invalid ranges.", Conf)),
    ?assertEqual(Path++":855:1: invalid ranges."++?EOL,
                 rebar_compiler_format:format(Path, {855,1}, "", "invalid ranges.", Conf)),
    ?assertEqual("/very/fake/path.oof:1:1: unknown file."++?EOL,
                 rebar_compiler_format:format("/very/fake/path.oof", {1,1}, "", "unknown file.", Conf)),
    ok.


nocolor() ->
    [{doc, "testing all sorts of planned output"}].
nocolor(Config) ->
    Path = ?config(file, Config),
    EmptyPath = ?config(empty_file, Config),
    Conf = ?config(conf, Config),
    ?assertEqual("   ┌─ "++Path++":"++?EOL++
                 "   │"++?EOL++
                 " 1 │  -module(noline_end);"++?EOL++
                 "   │                     ╰── => unexpected token: ;"++?EOL++?EOL,
                 rebar_compiler_format:format(Path, {1,20}, "=> ", "unexpected token: ;", Conf)),
    ?assertEqual("    ┌─ "++Path++":"++?EOL++
                 "    │"++?EOL++
                 " 11 │  first character on line 11."++?EOL++
                 "    │  ╰── some message"++?EOL++?EOL,
                 rebar_compiler_format:format(Path, {11,1}, "", "some message", Conf)),
    ?assertEqual("     ┌─ "++Path++":"++?EOL++
                 "     │"++?EOL++
                 " 111 │  case \tX of ^whatever % on line 111"++?EOL++
                 "     │       \t     ╰── the character '^' is not expected here."++?EOL++?EOL,
                 rebar_compiler_format:format(Path, {111,12}, "", "the character '^' is not expected here.", Conf)),
    %% invalid cases fall back to minimal mode
    ?assertEqual(Path++":-23:-42: invalid ranges."++?EOL,
                 rebar_compiler_format:format(Path, {-23,-42}, "", "invalid ranges.", Conf)),
    ?assertEqual(Path++":-23:-42: invalid ranges."++?EOL,
                 rebar_compiler_format:format(Path, {-23,-42}, "", "invalid ranges.", Conf)),
    ?assertEqual(Path++":855:1: invalid ranges."++?EOL,
                 rebar_compiler_format:format(Path, {855,1}, "", "invalid ranges.", Conf)),
    ?assertEqual("/very/fake/path.oof:1:1: unknown file."++?EOL,
                 rebar_compiler_format:format("/very/fake/path.oof", {1,1}, "", "unknown file.", Conf)),
    %% empty file
    ?assertEqual(EmptyPath++":1:1: should fallback to the minimal output"++?EOL,
             rebar_compiler_format:format(EmptyPath, {1,1}, "", "should fallback to the minimal output", Conf)),
    ok.

