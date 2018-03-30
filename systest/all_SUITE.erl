-module(all_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile([export_all, nowarn_export_all]).

init_per_suite(Config) ->
    %% TODO: figure out how to use a local rebar3 copy?
    %% Ensure global rebar3 has the same version as current one!
    {ok, Vsn} = application:get_key(rebar, vsn),
    {ok, ExecVsn} = rebar3("version", [{path, "."} | Config]),
    case rebar_string:lexemes(ExecVsn, " ") of
        ["rebar", Vsn | _] ->
            %% Copy all base cases to priv_dir
            rebar_file_utils:cp_r([?config(data_dir, Config)],
                                   ?config(priv_dir, Config)),
            Config;
        _ ->
            {skip, "expected current version "++Vsn++" in path "
                   "and found '"++ ExecVsn ++ "'"}
    end.

end_per_suite(Config) ->
    Config.

init_per_testcase(Name, Config) ->
    set_name_config(Name, Config).

end_per_testcase(_Name, Config) ->
    Config.

all() ->
    [noop, resource_plugins, alias_clash, grisp_explode, compile_deps].

%groups() ->
%    [{plugins, [shuffle], []},
%     {deps, [shuffle], []}].

%%%%%%%%%%%%%%%%%%
%%% TEST CASES %%%
%%%%%%%%%%%%%%%%%%

noop() ->
    [{doc, "just a sanity check on the handling of the test suite init/end"}].
noop(_Config) ->
    true.

resource_plugins() ->
    [{doc, "Issue #1673: "
           "Ensure that deps using resource plugins with semver compile."}].
resource_plugins(Config) ->
    %% When the environment handling is wrong, the run fails violently.
    {ok, Output} = rebar3("compile", Config),
    ct:pal("Rebar3 Output:~n~s",[Output]),
    ok.

alias_clash() ->
    [{doc, "checking that the provider won't get plugin interference."},
     {timetrap, 10000}].
alias_clash(Config) ->
    {ok, Help} = rebar3("help", Config), % should be redefined, but by the plugin
    ?assertNotEqual(nomatch,
        re:run(Help, "Alias help is already the name of a command[a-z ]+and will be ignored")
    ),
    {ok, Output} = rebar3("test", Config, [{env, [{"DEBUG", "1"}]}]),
    ?assertNotEqual(nomatch, re:run(Output, "cover summary written to:")),
    ?assertNotEqual(nomatch,
        re:run(Output, "Not adding provider default test from module rebar_prv_alias_test "
                       "because it already exists from module rebar_prv_alias_test")),
    ok.

grisp_explode() ->
    [{doc, "Don't force purge a plugin that runs the compile job itself"}].
grisp_explode(Config) ->
    %% When the purge handling is wrong, the run fails violently.
    {error, {_,Output}} = rebar3("grisp deploy -n robot -v 0.1.0", Config),
    ct:pal("Rebar3 Output:~n~s",[Output]),
    ?assertNotEqual(nomatch,
        re:run(Output, "No releases exist in the system for robot:0.1.0!")
    ),
    ok.

compile_deps() ->
    [{doc, "When compiling a project multiple times, the deps should always be built event if refetched"}].
compile_deps(Config) ->
    rebar3("compile", Config),
    rebar3("compile", Config),

    PrivDir = ?config(path, Config),
    EbinDir = filename:join([PrivDir, "_build", "default", "lib", "fake_dep", "ebin"]),

    {ok, Beams} = file:list_dir(EbinDir),
    ?assert(length(Beams) > 1).


%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%
set_name_config(Atom, Config) ->
    [{path,
      filename:join([?config(priv_dir, Config),
                     atom_to_list(?MODULE)++"_data", atom_to_list(Atom)])}
     | Config].

rebar3(Args, Config) -> rebar3(Args, Config, []).

rebar3(Args, Config, UserOpts) ->
    Exec = case os:type() of
        {win32, _} ->
            "rebar3.cmd";
        _ ->
            "rebar3"
    end,
    Cmd = Exec ++ " " ++ Args,
    Opts = [{cd, ?config(path, Config)}, return_on_error, use_stdout
            | UserOpts],
    ct:pal("Calling rebar3 ~s with options ~p", [Cmd, Opts]),
    rebar_utils:sh(Cmd, Opts).
