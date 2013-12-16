-module(logging_rt).
-export([files/0,
         run/1]).

-define(APP_FILE, "ebin/logging.app").

files() ->
    [
     {copy, "../../rebar", "rebar"},
     {create, ?APP_FILE, app(invalid_name, [])}
    ].

run(_Dir) ->
    SharedExpected = "==> logging_rt \\(compile\\)",
    %% provoke ERROR due to an invalid app file
    retest:log(info, "Check 'compile' failure output~n"),
    ok = check_output("./rebar compile -q", should_fail,
                      [SharedExpected, "ERROR: "],
                      ["WARN: ", "INFO: ", "DEBUG: "]),
    %% fix bad app file
    ok = file:write_file(?APP_FILE, app(logging, [])),
    retest:log(info, "Check 'compile' success output~n"),
    ok = check_output("./rebar compile", should_succeed,
                      [SharedExpected],
                      ["ERROR: ", "WARN: ", "INFO: ", "DEBUG: "]),
    retest:log(info, "Check 'compile -v' success output~n"),
    ok = check_output("./rebar compile -v", should_succeed,
                      [SharedExpected],
                      ["ERROR: ", "INFO: ", "DEBUG: "]),
    retest:log(info, "Check 'compile -vv' success output~n"),
    ok = check_output("./rebar compile -vv", should_succeed,
                      [SharedExpected, "DEBUG: "],
                      ["ERROR: ", "INFO: "]),
    ok.

check_output(Cmd, FailureMode, Expected, Unexpected) ->
    case {retest:sh(Cmd), FailureMode} of
        {{error, _}=Error, should_succeed} ->
            retest:log(error, "cmd '~s' failed:~n~p~n", [Cmd, Error]),
            Error;
        {{ok, Captured}, should_succeed} ->
            Joined = string:join(Captured, "\n"),
            check_output1(Cmd, Joined, Expected, Unexpected);
        {{error, {stopped, {_Rc, Captured}}}, should_fail} ->
            Joined = string:join(Captured, "\n"),
            check_output1(Cmd, Joined, Expected, Unexpected)
    end.

check_output1(Cmd, Captured, Expected, Unexpected) ->
    ReOpts = [{capture, all, list}],
    ExMatches =
        lists:zf(
          fun(Pattern) ->
                  case re:run(Captured, Pattern, ReOpts) of
                      nomatch ->
                          retest:log(error,
                                     "Expected pattern '~s' missing "
                                     "in the following output:~n"
                                     "=== BEGIN ===~n~s~n=== END ===~n",
                                     [Pattern, Captured]),
                          {true, Pattern};
                      {match, _} ->
                          false
                  end
          end, Expected),

    UnExMatches =
        lists:zf(
          fun(Pattern) ->
                  case re:run(Captured, Pattern, ReOpts) of
                      nomatch ->
                          false;
                      {match, [Match]} ->
                          retest:log(
                            console,
                            "Unexpected output when running cmd '~s':~n~s~n",
                            [Cmd, Match]),
                          {true, Match}
                  end
          end, Unexpected),

    case {ExMatches, UnExMatches} of
        {[], []} ->
            ok;
        _ ->
            error
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
