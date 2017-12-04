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
    [noop, resource_plugins].

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

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%
set_name_config(Atom, Config) ->
    [{path,
      filename:join([?config(priv_dir, Config),
                     atom_to_list(?MODULE)++"_data", atom_to_list(Atom)])}
     | Config].

rebar3(Args, Config) ->
    Exec = case os:type() of
        {win32, _} ->
            "rebar3.cmd";
        _ ->
            "rebar3"
    end,
    Cmd = Exec ++ " " ++ Args,
    Opts = [{cd, ?config(path, Config)}, return_on_error, use_stdout],
    ct:pal("Calling rebar3 ~s with options ~p", [Cmd, Opts]),
    rebar_utils:sh(Cmd, Opts).
