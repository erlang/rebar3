-module(cth_fail_fast).
-moduledoc false.

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

%% We work by setting an 'abort' variable on each test case that fails
%% and then triggering the failure before starting the next test. This
%% ensures that all other hooks have run for the same event, and
%% simplifies error reporting.
-record(state, {abort=false}).

%% @doc Return a unique id for this CTH.
id(_Opts) ->
    {?MODULE, make_ref()}.

%% @doc Always called before any other callback function. Use this to initiate
%% any common state.
init(_Id, _Opts) ->
    {ok, #state{}}.

%% @doc Called before init_per_suite is called.
pre_init_per_suite(_Suite,_Config,#state{abort=true}) ->
    abort();
pre_init_per_suite(_Suite,Config,State) ->
    {Config, State}.

%% @doc Called after init_per_suite.
post_init_per_suite(_Suite,_Config,Return,State) ->
    {Return, State}.

%% @doc Called before end_per_suite.
pre_end_per_suite(_Suite,_Config,#state{abort=true}) ->
    abort();
pre_end_per_suite(_Suite,Config,State) ->
    {Config, State}.

%% @doc Called after end_per_suite.
post_end_per_suite(_Suite,_Config,Return,State) ->
    {Return, State}.

%% @doc Called before each init_per_group.
pre_init_per_group(_Group,_Config,#state{abort=true}) ->
    abort();
pre_init_per_group(_Group,Config,State) ->
    {Config, State}.

%% @doc Called after each init_per_group.
post_init_per_group(_Group,_Config,Return, State) ->
    {Return, State}.

%% @doc Called after each end_per_group.
pre_end_per_group(_Group,_Config,#state{abort=true}) ->
    abort();
pre_end_per_group(_Group,Config,State) ->
    {Config, State}.

%% @doc Called after each end_per_group.
post_end_per_group(_Group,_Config,Return, State) ->
    {Return, State}.

%% @doc Called before each test case.
pre_init_per_testcase(_TC,_Config,#state{abort=true}) ->
    abort();
pre_init_per_testcase(_TC,Config,State) ->
    {Config, State}.

%% @doc Called after each test case.
post_end_per_testcase(_TC,_Config,ok,State) ->
    {ok, State};
post_end_per_testcase(_TC,_Config,Error,State) ->
    {Error, State#state{abort=true}}.

%% @doc Called after post_init_per_suite, post_end_per_suite, post_init_per_group,
%% post_end_per_group and post_end_per_testcase if the suite, group or test case failed.
on_tc_fail(_TC, _Reason, State) ->
    State.

%% @doc Called when a test case is skipped by either user action
%% or due to an init function failing. (>= 19.3)
on_tc_skip(_Suite, _TC, {tc_auto_skip, _}, State) ->
    State#state{abort=true};
on_tc_skip(_Suite, _TC, _Reason, State) ->
    State.

%% @doc Called when a test case is skipped by either user action
%% or due to an init function failing. (Pre-19.3)
on_tc_skip(_TC, {tc_auto_skip, _}, State) ->
    State#state{abort=true};
on_tc_skip(_TC, _Reason, State) ->
    State.

%% @doc Called when the scope of the CTH is done
terminate(#state{}) ->
    ok.

%%% Helpers
abort() ->
    io:format(user, "Detected test failure. Aborting~n", []),
    halt(1).
