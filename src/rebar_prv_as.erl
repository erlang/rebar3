%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

-module(rebar_prv_as).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-include("rebar.hrl").

-define(PROVIDER, as).
-define(DEPS, []).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    State1 = rebar_state:add_provider(State, providers:create([{name, ?PROVIDER},
                                                               {module, ?MODULE},
                                                               {bare, false},
                                                               {deps, ?DEPS},
                                                               {example, "rebar3 as <profile1>,<profile2>,... <task1>, <task2>, ..."},
                                                               {short_desc, "Higher order provider for running multiple tasks in a sequence as a certain profiles."},
                                                               {desc, ""},
                                                               {opts, [{profile, undefined, undefined, string, "Profiles to run as."}]}])),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    [Profile | Rest] = rebar_state:command_args(State),
    Tasks = args_to_tasks(Rest),
    Profiles = [list_to_atom(X) || X <- string:tokens(Profile, ",")],
    State1 = rebar_state:apply_profiles(State, Profiles),
    lists:foldl(fun(TaskArgs, {ok, StateAcc}) ->
                        [TaskStr | Args] = string:tokens(TaskArgs, " "),
                        Task = list_to_atom(TaskStr),
                        StateAcc1 = rebar_state:set(StateAcc, task, Task),
                        StateAcc2 = rebar_state:command_args(StateAcc1, Args),
                        rebar_core:process_command(StateAcc2, Task)
                end, {ok, State1}, Tasks).

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

args_to_tasks(Args) ->
    [string:strip(T) || T <- string:tokens(string:join(Args, " "), ",")].
