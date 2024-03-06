-module(cth_readable_compact_shell).
-import(cth_readable_helpers, [format_path/2, colorize/2, maybe_eunit_format/1]).

-define(OKC, green).
-define(FAILC, red).
-define(SKIPC, magenta).

-define(OK(Suite, CasePat, CaseArgs),
        ?CASE(Suite, CasePat, ?OKC, "OK", CaseArgs)).
-define(SKIP(Suite, CasePat, CaseArgs, Reason, Verbose),
        ?STACK(Suite, CasePat, CaseArgs, Reason, ?SKIPC, "SKIPPED", Verbose)).
-define(FAIL(Suite, CasePat, CaseArgs, Reason),
        ?STACK(Suite, CasePat, CaseArgs, Reason, ?FAILC, "FAILED")).
-define(STACK(Suite, CasePat, CaseArgs, Reason, Color, Label),
        ?STACK(Suite, CasePat, CaseArgs, Reason, Color, Label, true)).
-define(STACK(Suite, CasePat, CaseArgs, Reason, Color, Label, Verbose),
        begin
          case Verbose of
            true ->
              ?CASE(Suite, CasePat, Color, Label, CaseArgs),
               io:format(user, "~n%%% ~p ==> ~ts~n", [Suite,colorize(Color, maybe_eunit_format(Reason))]);
            false ->
              io:format(user, colorize(Color, "*"), [])
          end
        end).
-define(CASE(Suite, CasePat, Color, Res, Args),
        case Res of
            "OK" -> io:put_chars(user, colorize(Color, "."));
            _ -> io:format(user, lists:flatten(["%%% ~p ==> ",CasePat,": ",colorize(Color, Res)]), [Suite | Args])
        end).

%% Callbacks
-export([id/1]).
-export([init/2]).

-export([pre_init_per_suite/3]).
-export([post_init_per_suite/4]).
-export([pre_end_per_suite/3]).
-export([post_end_per_suite/4]).

-export([pre_init_per_group/3]).
-export([post_init_per_group/4]).
-export([pre_end_per_group/3]).
-export([post_end_per_group/4]).

-export([pre_init_per_testcase/3]).
-export([post_end_per_testcase/5]).

-export([on_tc_fail/3]).
-export([on_tc_skip/3, on_tc_skip/4]).

-export([terminate/1]).

-record(state, {id, suite, groups, opts, last_suite, last_suite_skipped, last_tc_failed}).

%% @doc Return a unique id for this CTH.
id(_Opts) ->
    {?MODULE, make_ref()}.

%% @doc Always called before any other callback function. Use this to initiate
%% any common state.
init(Id, Opts) ->
    {ok, #state{id=Id, opts=Opts, last_suite=undefined, last_suite_skipped=false, last_tc_failed=false}}.

%% @doc Called before init_per_suite is called.
pre_init_per_suite(Suite,Config,#state{last_suite=LastSuite, last_suite_skipped=LastSuiteSkipped, last_tc_failed=LastTCFailed, opts=Opts}=State) ->
    IsVerbose = is_verbose(Opts),
    case LastSuite of
        undefined -> % first suite
            ok;
        _Defined ->
            case LastTCFailed of
                true ->
                    ok;
                false ->
                    io:format(user, "~n", [])
            end,
            case {IsVerbose, LastSuiteSkipped} of
                {false, true} ->
                    io:format(user, "~n", []);
                _Other ->
                    ok
            end
    end,
    io:format(user, "%%% ~p", [Suite]),
    {Config, State#state{suite=Suite, groups=[]}}.

%% @doc Called after init_per_suite.
post_init_per_suite(_Suite,_Config,Return,State=#state{opts=Opts}) ->
    IsVerbose = is_verbose(Opts),
    SuiteSkipped
        = case {IsVerbose, Return} of
              {true, {skip, _}} ->
                  io:format(user, " ==> ", []),
                  true;
              {false, {skip, _}} ->
                  io:format(user, ": ", []),
                  true;
              _Other ->
                  false
          end,
    {Return, State#state{last_suite_skipped=SuiteSkipped, last_tc_failed=SuiteSkipped}}.

%% @doc Called before end_per_suite.
pre_end_per_suite(_Suite,Config,State) ->
    {Config, State}.

%% @doc Called after end_per_suite.
post_end_per_suite(_Suite,_Config,Return,#state{opts=Opts,last_tc_failed=LastTCFailed}=State) ->
    IsVerbose = is_verbose(Opts),
    case {IsVerbose, LastTCFailed} of
        {false, true} ->
            io:format(user, "~n", []);
        _Other ->
            ok
    end,
    {Return, State#state{suite=undefined, groups=[]}}.

%% @doc Called before each init_per_group.
pre_init_per_group(_Group,Config,State) ->
    {Config, State}.

%% @doc Called after each init_per_group.
post_init_per_group(Group,_Config,Return, State=#state{groups=Groups}) ->
    {Return, State#state{groups=[Group|Groups]}}.

%% @doc Called after each end_per_group.
pre_end_per_group(_Group,Config,State) ->
    {Config, State}.

%% @doc Called after each end_per_group.
post_end_per_group(_Group,_Config,Return, State=#state{groups=Groups}) ->
    {Return, State#state{groups=tl(Groups)}}.

%% @doc Called before each test case.
pre_init_per_testcase(_TC,Config,State) ->
    {Config, State}.

%% @doc Called after each test case.
post_end_per_testcase(SuiteName,TC,_Config,ok,State=#state{suite=Suite, groups=Groups, last_suite=LastSuite, opts=Opts, last_tc_failed=LastTCFailed}) ->
    IsVerbose = is_verbose(Opts),
    IsFirstInSuite = Suite =/= LastSuite,
    case {IsVerbose, IsFirstInSuite, LastTCFailed} of
        {_, true, _} ->
            io:format(user, ": ", []);
        {true, false, true} ->
            io:format(user, "%%% ~p: ", [SuiteName]);
        _Other ->
            ok
    end,
    ?OK(Suite, "~s", [format_path(TC,Groups)]),
    {ok, State#state{last_suite = SuiteName, last_tc_failed=false}};
post_end_per_testcase(SuiteName,TC,Config,Error,State=#state{suite=Suite, groups=Groups, opts=Opts, last_suite=LastSuite, last_tc_failed=LastTCFailed}) ->
    IsVerbose = is_verbose(Opts),
    IsFirstInSuite = Suite =/= LastSuite,
    case {IsVerbose, IsFirstInSuite, LastTCFailed} of
        {true, true, _} ->
            io:format(user, " ==> ", []);
        {true, false, false} ->
            io:format(user, "~n%%% ~p ==> ", [SuiteName]);
        {true, false, true} ->
            io:format(user, "%%% ~p ==> ", [SuiteName]);
        {false, true, _} ->
            io:format(user, ": ", []);
        _Other ->
            ok
    end,
    case lists:keyfind(tc_status, 1, Config) of
        {tc_status, ok} ->
            %% Test case passed, but we still ended in an error
            ?STACK(Suite, "~s", [format_path(TC,Groups)], Error, ?SKIPC, "end_per_testcase FAILED");
        _ ->
            %% Test case failed, in which case on_tc_fail already reports it
            ok
    end,
    {Error, State#state{last_suite = SuiteName, last_tc_failed=true}}.

%% @doc Called after post_init_per_suite, post_end_per_suite, post_init_per_group,
%% post_end_per_group and post_end_per_testcase if the suite, group or test case failed.
on_tc_fail({TC,_Group}, Reason, State=#state{suite=Suite, groups=Groups}) ->
    ?FAIL(Suite, "~s", [format_path(TC,Groups)], Reason),
    State;
on_tc_fail(TC, Reason, State=#state{suite=Suite, groups=Groups}) ->
    ?FAIL(Suite, "~s", [format_path(TC,Groups)], Reason),
    State.

%% @doc Called when a test case is skipped by either user action
%% or due to an init function failing. (>= 19.3)
on_tc_skip(Suite, {TC,_Group}, Reason, State=#state{groups=Groups}) ->
    skip(Suite, TC, Groups, Reason, State),
    State;
on_tc_skip(Suite, TC, Reason, State=#state{groups=Groups}) ->
    skip(Suite, TC, Groups, Reason, State),
    State.

skip(Suite, TC, Groups, Reason, #state{opts=Opts, last_suite_skipped=_LastSuiteSkipped}) ->
    ?SKIP(Suite, "~s", [format_path(TC,Groups)], Reason, is_verbose(Opts)).

is_verbose(Opts) ->
    proplists:get_value(verbose, Opts, true).

%% @doc Called when a test case is skipped by either user action
%% or due to an init function failing. (Pre-19.3)
on_tc_skip({TC,Group}, Reason, State=#state{suite=Suite}) ->
    ?SKIP(Suite, "~p (group ~p)", [TC, Group], Reason, true),
    State;
on_tc_skip(TC, Reason, State=#state{suite=Suite}) ->
    ?SKIP(Suite, "~p", [TC], Reason, true),
    State.

%% @doc Called when the scope of the CTH is done
terminate(_State) ->
    ok.
