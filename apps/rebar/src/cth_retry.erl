-module(cth_retry).

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

-record(state, {id, suite, groups, acc=[]}).

%% @doc Return a unique id for this CTH.
id(_Opts) ->
    {?MODULE, make_ref()}.

%% @doc Always called before any other callback function. Use this to initiate
%% any common state.
init(Id, _Opts) ->
    {ok, #state{id=Id}}.

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
post_end_per_testcase(_TC,_Config,ok,State) ->
    {ok, State};
post_end_per_testcase(TC,_Config,Error,State=#state{suite=Suite, groups=Groups, acc=Acc}) ->
    Test = case TC of
        {_Group, Case} -> Case;
        TC -> TC
    end,
    {Error, State#state{acc=[{Suite, Groups, Test}|Acc]}}.

%% @doc Called after post_init_per_suite, post_end_per_suite, post_init_per_group,
%% post_end_per_group and post_end_per_testcase if the suite, group or test case failed.
on_tc_fail(_TC, _Reason, State) ->
    State.

%% @doc Called when a test case is skipped by either user action
%% or due to an init function failing. (>= 19.3)
on_tc_skip(Suite, TC, {tc_auto_skip, _}, State=#state{suite=Suite, groups=Groups, acc=Acc}) ->
    NewAcc = case TC of
        init_per_testcase -> Acc;
        end_per_testcase -> Acc;
        {init_per_group,_} -> Acc;
        {end_per_group, _} -> Acc;
        init_per_suite -> Acc;
        end_per_suite -> Acc;
        {Case, _Group} -> [{Suite, Groups, Case}|Acc];
        TC -> [{Suite, Groups, TC}|Acc]
    end,
    State#state{suite=Suite, acc=NewAcc};
on_tc_skip(Suite, _TC, _Reason, State) ->
    State#state{suite=Suite}.

%% @doc Called when a test case is skipped by either user action
%% or due to an init function failing. (Pre-19.3)
on_tc_skip(TC, {tc_auto_skip, _}, State=#state{suite=Suite, groups=Groups, acc=Acc}) ->
    NewAcc = case TC of
        init_per_testcase -> Acc;
        end_per_testcase -> Acc;
        {init_per_group,_} -> Acc;
        {end_per_group, _} -> Acc;
        init_per_suite -> Acc;
        end_per_suite -> Acc;
        {Case, _Group} -> [{Suite, Groups, Case}|Acc];
        TC -> [{Suite, Groups, TC}|Acc]
    end,
    State#state{acc=NewAcc};
on_tc_skip(_TC, _Reason, State) ->
    State.

%% @doc Called when the scope of the CTH is done
terminate(#state{acc=[]}) ->
    ok;
terminate(#state{acc=Acc}) ->
    Spec = to_spec(Acc),
    {ok, Cwd} = file:get_cwd(),
    Path = filename:join(lists:droplast(filename:split(Cwd))++["retry.spec"]),
    io:format(user,
              "EXPERIMENTAL: Writing retry specification at ~s~n"
              "              call rebar3 ct with '--retry' to re-run failing cases.~n",
             [Path]),
    file:write_file(Path, Spec),
    ok.

%%% Helpers
to_spec(List) ->
    [to_spec_entry(X) || X <- merge(List)].

merge([]) -> [];
merge([{Suite, Groups, Case}|T]) when is_atom(Case) ->
    merge([{Suite, Groups, [Case]}|T]);
merge([{Suite, Groups, Cases}, {Suite, Groups, Case} | T]) ->
    merge([{Suite, Groups, [Case|Cases]}|T]);
merge([{Suite, Groups, Cases} | T]) ->
    [{Suite, Groups, Cases} | merge(T)].

to_spec_entry({Suite, [], Cases}) ->
    Dir = filename:dirname(proplists:get_value(source, Suite:module_info(compile))),
    io_lib:format("~p.~n", [{cases, Dir, Suite, Cases}]);
to_spec_entry({Suite, Groups, Cases}) ->
    Dir = filename:dirname(proplists:get_value(source, Suite:module_info(compile))),
    ExpandedGroups = expand_groups(lists:reverse(Groups)),
    io_lib:format("~p.~n", [{groups, Dir, Suite, ExpandedGroups, {cases,Cases}}]).

expand_groups([Group]) ->
    {Group, []};
expand_groups([H|T]) ->
    {H,[],[expand_groups(T)]}.

