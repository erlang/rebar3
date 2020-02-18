%% coding:utf-8

-module(rebar_templater_SUITE).
-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() ->
    [
     consult_template_latin1_test,
     consult_template_utf8_test
    ].

init_per_suite(Config) -> Config.
end_per_suite(_Config) -> ok.

init_per_testcase(Case, Config)
    when Case =:= consult_template_latin1_test;
         Case =:= consult_template_utf8_test ->
    %% Generate UCS string containing all printable characters of latin-1 area.
    Description = lists:seq(16#A1, 16#AC) ++ lists:seq(16#AE, 16#FE),
    Expected = [{description, Description}],
    SampleTemplate = "{description, \"" ++ Description ++ "\"}.\n",
    Path = generate_sample_template_file(Case, SampleTemplate, Config),
    [{template_file_path, Path}, {expected, Expected} | Config];
init_per_testcase(_Case, Config) -> Config.

end_per_testcase(_Case, _Config) -> ok.

generate_sample_template_file(Case, Content, Config) ->
    CaseName = atom_to_list(Case),
    {Encoding, EncodingName} =
        case string:str(CaseName, "latin1") of
            0 -> {utf8, "utf-8"};
            _ -> {latin1, "latin-1"}
        end,
    PrivDir = ?config(priv_dir, Config),
    Path = filename:join([PrivDir, CaseName ++ ".template"]),
    {ok, FH} = file:open(Path, [write, {encoding, Encoding}]),
    try
        io:format(FH, "%% coding:~s~n~s", [EncodingName, Content])
    after
        file:close(FH)
    end,
    Path.

consult_template_test_common(Config) ->
    Expected = ?config(expected, Config),
    Path = ?config(template_file_path, Config),
    Result = rebar_templater:consult_template([], file, Path),
    ?assertEqual(Expected, Result),
    ok.

consult_template_latin1_test() ->
    [{doc, "parse test for latin1 template file"}].
consult_template_latin1_test(Config) ->
    consult_template_test_common(Config).

consult_template_utf8_test() ->
    [{doc, "parse test for utf8 template file"}].
consult_template_utf8_test(Config) ->
    consult_template_test_common(Config).
