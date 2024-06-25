-module(rebar_completion_SUITE).

-compile([export_all, nowarn_export_all]).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

suite() ->
    [].

all() ->
    [test_completion_gen, check_bash, check_zsh].

groups() ->
    [].

init_per_suite(Config) ->
    Shells = [bash, zsh],
    ComplFile = compl_file(Config),
    ok = filelib:ensure_dir(ComplFile),
    [{compl_file, ComplFile}, {shells, Shells} | Config].

end_per_suite(_Config) ->
    ok.

init_per_testcase(check_bash, Config) ->
    case shell_available(bash) of
        true ->
            rebar_test_utils:init_rebar_state(Config, "completion_");
        false ->
            {skip, "bash not found"}
    end;
init_per_testcase(check_zsh, Config) ->
    case shell_available(zsh) of
        true ->
            rebar_test_utils:init_rebar_state(Config, "completion_");
        false ->
            {skip, "zsh not found"}
    end;
init_per_testcase(_, Config) ->
    rebar_test_utils:init_rebar_state(Config, "completion_").

end_per_testcase(_, _Config) ->
    ok.

shell_available(Shell) ->
    os:find_executable(atom_to_list(Shell)) =/= false.

%% test cases

test_completion_gen(Config) ->
    Shells = ?config(shells, Config),
    ComplFile = ?config(compl_file, Config),
    lists:foreach(fun(Shell) ->
                    file:delete(ComplFile),
                    completion_gen(Config, #{shell=>Shell, file=>ComplFile}),
                    ?assertEqual({Shell, true}, {Shell,filelib:is_file(ComplFile)})
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
                    ?assertMatch({Alias, {match, _}}, {Alias, re:run(Completion, CompleteCmd++Alias++"\n")})
                  end,
                  ["rebar3" | Aliases]).

check_zsh(Config) ->
    ComplFile = ?config(compl_file, Config),
    Aliases = ["rebar", "r3"],
    Opts = #{shell => zsh,
             file => ComplFile,
             aliases => Aliases},
    completion_gen(Config, Opts),
    {ok, Completion} = file:read_file(ComplFile),
    %% function definition
    {match, _} = re:run(Completion, "function _rebar3 {"),
    CompleteCmd = "compdef _rebar3 ",
    lists:foreach(fun(Alias) ->
                    ?assertMatch({Alias, {match, _}}, {Alias, re:run(Completion, CompleteCmd++Alias++"\n")})
                  end,
                  ["rebar3" | Aliases]).

%% helpers

completion_gen(Config, CmplOpts) ->
    CmplConf = maps:to_list(CmplOpts),
    Res = rebar_test_utils:run_and_check(Config, [{completion,CmplConf}], ["completion"], return),
    {ok, _} = Res.

compl_file(Config) ->
    filename:absname(filename:join(?config(priv_dir,Config), "_rebar3")).
