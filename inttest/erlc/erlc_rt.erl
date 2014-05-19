-module(erlc_rt).
-export([files/0,
         run/1]).

-include_lib("eunit/include/eunit.hrl").

-define(MODULES,
        [first_xrl,
         first_yrl,
         foo,
         foo_app,
         foo_sup,
         foo_test_worker,
         foo_worker,
         'SIMPLE-ASN']).

-define(BEAM_FILES,
        ["first_xrl.beam",
         "first_yrl.beam",
         "foo.beam",
         "foo_app.beam",
         "foo_sup.beam",
         "foo_test_worker.beam",
         "foo_worker.beam",
         "SIMPLE-ASN.beam"]).

files() ->
    [
     {copy, "../../rebar", "rebar"},
     {copy, "rebar.config", "rebar.config"},
     {copy, "rebar-no_debug_info.config", "rebar-no_debug_info.config"},
     {copy, "include", "include"},
     {copy, "extra-include", "extra-include"},
     {copy, "src", "src"},
     {copy, "extra-src", "extra-src"},
     {copy, "mibs", "mibs"},
     {copy, "asn1", "asn1"},
     {create, "ebin/foo.app", app(foo, ?MODULES)}
    ].

run(_Dir) ->
    ?assertMatch({ok, _}, retest_sh:run("./rebar compile", [])),
    ok = check_beams(true),
    ok = check_debug_info(true),
    MibResult = filename:join(["priv", "mibs", "SIMPLE-MIB.bin"]),
    ?assertMatch(true, filelib:is_regular(MibResult)),
    ?assertMatch({ok, _}, retest_sh:run("./rebar clean", [])),
    ok = check_beams(false),
    ?assertMatch(false, filelib:is_regular(MibResult)),
    ?assertMatch(
       {ok, _},
       retest_sh:run("./rebar -C rebar-no_debug_info.config compile", [])),
    ok = check_beams(true),
    ok = check_debug_info(false),
    ?assertMatch(true, filelib:is_regular(MibResult)),
    ok.

check_beams(Exist) ->
    check_files(Exist, fun filelib:is_regular/1).

check_debug_info(HasDebugInfo) ->
    check_files(HasDebugInfo, fun has_debug_info/1).

check_files(Expected, Check) ->
    lists:foreach(
      fun(F) ->
              File = filename:join("ebin", F),
              ?assertEqual(Expected, Check(File))
      end,
      ?BEAM_FILES).

%% NOTE: Copied from dialyzer_utils:get_abstract_code_from_beam/1 and
%% modified for local use. We could have called the function directly,
%% but dialyzer_utils is not an official API to rely on.
has_debug_info(File) ->
    case beam_lib:chunks(File, [abstract_code]) of
        {ok, {_Mod, List}} ->
            case lists:keyfind(abstract_code, 1, List) of
                {abstract_code, {raw_abstract_v1, _Abstr}} ->
                    true;
                _ ->
                    false
            end;
        _ ->
            false
    end.

%%
%% Generate the contents of a simple .app file
%%
app(Name, Modules) ->
    App = {application, Name,
           [{description, atom_to_list(Name)},
            {vsn, "1"},
            {modules, Modules},
            {registered, []},
            {applications, [kernel, stdlib]}]},
    io_lib:format("~p.\n", [App]).
