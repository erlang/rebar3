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
    {Profiles, Tasks} = args_to_profiles_and_tasks(rebar_state:command_args(State)),
    case Profiles of
        [] ->
            {error, "At least one profile must be specified when using `as`"};
        _  ->
            State1 = rebar_state:apply_profiles(State, [list_to_atom(X) || X <- Profiles]),
            rebar_prv_do:do_tasks(Tasks, State1)
    end.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

args_to_profiles_and_tasks(Args) ->
    first_profile(Args).

first_profile([]) -> {[], []};
first_profile([ProfileList|Rest]) ->
    case re:split(ProfileList, ",", [{return, list}, {parts, 2}]) of
        %% profile terminated by comma
        [P, More]       -> profiles([More] ++ Rest, [P]);
        %% profile not terminated by comma
        [P]           -> comma_or_end(Rest, [P])
    end.

profiles([], Acc) -> {lists:reverse(Acc), rebar_utils:args_to_tasks([])};
profiles([ProfileList|Rest], Acc) ->
    case re:split(ProfileList, ",", [{return, list}, {parts, 2}]) of
        %% profile terminated by comma
        [P, More]  -> profiles([More] ++ Rest, [P|Acc]);
        %% profile not terminated by comma
        [P]      -> comma_or_end(Rest, [P|Acc])
    end.

%% `, foo...`
comma_or_end([","|Rest], Acc) ->
    profiles(Rest, Acc);
%% `,foo...`
comma_or_end(["," ++ Profile|Rest], Acc) ->
    profiles([Profile|Rest], Acc);
comma_or_end(Tasks, Acc) ->
    {lists:reverse(Acc), rebar_utils:args_to_tasks(Tasks)}.
