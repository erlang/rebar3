-module(cth_readable_shell).
-import(cth_readable_helpers, [format_path/2, colorize/2, maybe_eunit_format/1]).

-define(OKC, green).
-define(FAILC, red).
-define(SKIPC, magenta).

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
-export([post_end_per_testcase/4]).

-export([on_tc_fail/3]).
-export([on_tc_skip/3, on_tc_skip/4]).

-export([terminate/1]).

-record(state, {id, suite, groups, opts}).

%% @doc Return a unique id for this CTH.
id(_Opts) ->
    {?MODULE, make_ref()}.

%% @doc Always called before any other callback function. Use this to initiate
%% any common state.
init(Id, Opts) ->
    {ok, #state{id=Id, opts=Opts}}.

%% @doc Called before init_per_suite is called.
pre_init_per_suite(Suite,Config,State) ->
    {Config, State#state{suite=Suite, groups=[]}}.

%% @doc Called after init_per_suite.
post_init_per_suite(_Suite,_Config,Return,State) ->
    {Return, State}.

%% @doc Called before end_per_suite.
pre_end_per_suite(_Suite,Config,State) ->
    {Config, State}.

%% @doc Called after end_per_suite.
post_end_per_suite(_Suite,_Config,Return,State) ->
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
post_end_per_testcase(TC,_Config,ok,State=#state{suite=Suite, groups=Groups}) ->
    format_ok(Suite, "~s", [format_path(TC,Groups)]),
    {ok, State};
post_end_per_testcase(TC,Config,Error,State=#state{suite=Suite, groups=Groups}) ->
    case lists:keyfind(tc_status, 1, Config) of
        {tc_status, ok} ->
            %% Test case passed, but we still ended in an error
            format_stack(Suite, "~s", [format_path(TC,Groups)], Error, ?SKIPC, "end_per_testcase FAILED");
        _ ->
            %% Test case failed, in which case on_tc_fail already reports it
            ok
    end,
    {Error, State}.

%% @doc Called after post_init_per_suite, post_end_per_suite, post_init_per_group,
%% post_end_per_group and post_end_per_testcase if the suite, group or test case failed.
on_tc_fail({TC,_Group}, Reason, State=#state{suite=Suite, groups=Groups}) ->
    format_fail(Suite, "~s", [format_path(TC,Groups)], Reason),
    State;
on_tc_fail(TC, Reason, State=#state{suite=Suite, groups=Groups}) ->
    format_fail(Suite, "~s", [format_path(TC,Groups)], Reason),
    State.

%% @doc Called when a test case is skipped by either user action
%% or due to an init function failing. (>= 19.3)
on_tc_skip(Suite, {TC,_Group}, Reason, State=#state{groups=Groups, opts=Opts}) ->
    skip(Suite, TC, Groups, Reason, Opts),
    State#state{suite=Suite};
on_tc_skip(Suite, TC, Reason, State=#state{groups=Groups, opts=Opts}) ->
    skip(Suite, TC, Groups, Reason, Opts),
    State#state{suite=Suite}.

skip(Suite, TC, Groups, Reason, Opts) ->
    Verbose = proplists:get_value(verbose, Opts, true),
    format_skip(Suite, "~s", [format_path(TC,Groups)], Reason, Verbose).

%% @doc Called when a test case is skipped by either user action
%% or due to an init function failing. (Pre-19.3)
on_tc_skip({TC,Group}, Reason, State=#state{suite=Suite}) ->
    format_skip(Suite, "~p (group ~p)", [TC, Group], Reason, true),
    State;
on_tc_skip(TC, Reason, State=#state{suite=Suite}) ->
    format_skip(Suite, "~p", [TC], Reason, true),
    State.

%% @doc Called when the scope of the CTH is done
terminate(_State) ->
    ok.

%%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%%
format_ok(Suite, CasePat, CaseArgs) ->
    format_case(Suite, CasePat, ?OKC, "OK", CaseArgs).

format_skip(Suite, CasePat, CaseArgs, Reason, Verbose) ->
    format_stack(Suite, CasePat, CaseArgs, Reason, ?SKIPC, "SKIPPED", Verbose).

format_fail(Suite, CasePat, CaseArgs, Reason) ->
    format_stack(Suite, CasePat, CaseArgs, Reason, ?FAILC, "FAILED").

format_case(Suite, CasePat, Color, Res, Args) ->
    case Res of
        "OK" -> io:put_chars(user, colorize(Color, "."));
        _ -> io:format(user, lists:flatten(["~n%%% ~p ==> ",CasePat,": ",colorize(Color, Res),"~n"]), [Suite | Args])
    end.

format_stack(Suite, CasePat, CaseArgs, Reason, Color, Label) ->
    format_stack(Suite, CasePat, CaseArgs, Reason, Color, Label, true).

format_stack(Suite, CasePat, CaseArgs, Reason, Color, Label, Verbose) ->
    case Verbose of
        true ->
            format_case(Suite, CasePat, Color, Label, CaseArgs),
            io:format(user, "%%% ~p ==> ~ts~n", [Suite,colorize(Color, maybe_eunit_format(Reason))]);
        false ->
            io:format(user, colorize(Color, "*"), [])
    end.
