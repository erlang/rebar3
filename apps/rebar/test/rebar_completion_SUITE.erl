-module(rebar_completion_SUITE).

-compile([export_all, nowarn_export_all]).

-include_lib("common_test/include/ct.hrl").

suite() ->
    [].

all() ->
    [test_competion_gen, check_bash].

groups() ->
    [].

init_per_suite(Config) ->
    Shells = [bash],
    ComplFile = compl_file(Config),
    ok = filelib:ensure_dir(ComplFile),
    [{compl_file, ComplFile}, {shells, Shells} | Config].

end_per_suite(_Config) ->
    ok.

init_per_testcase(_, Config) ->
    rebar_test_utils:init_rebar_state(Config, "completion_").

end_per_testcase(_, _Config) ->
    ok.

%% test cases

test_competion_gen(Config) ->
    Shells = ?config(shells, Config),
    ComplFile = ?config(compl_file, Config),
    lists:foreach(fun(Shell) ->
                    file:delete(ComplFile),
                    completion_gen(Config, #{shell=>Shell, file=>ComplFile}),
                    {Shell, true} = {Shell,filelib:is_file(ComplFile)}
                  end,
                  Shells).

check_bash(Config) ->
    ComplFile = ?config(compl_file, Config),
    Aliases = ["rebar", "r3"],
    Opts = #{shell => bash,
             file => ComplFile,
             aliases => Aliases},
    completion_gen(Config, Opts),
    {ok, Completion} = file:read_file(ComplFile),
    %% function definition
    {match, _} = re:run(Completion, "_rebar3\\(\\)\\{"),
    %% aliases
    CompleteCmd = "complete -o nospace -F _rebar3 ",
    lists:foreach(fun(Alias) ->
                    {Alias, {match, _}} = {Alias, re:run(Completion, CompleteCmd++Alias++"\n")}
                  end,
                  ["rebar3" | Aliases]).

%% helpers

completion_gen(Config, CmplOpts) ->
    CmplConf = maps:to_list(CmplOpts),
    Res = rebar_test_utils:run_and_check(Config, [{completion,CmplConf}], ["completion"], return),
    {ok, _} = Res.

compl_file(Config) ->
    filename:absname(filename:join(?config(priv_dir,Config), "_rebar3")).
