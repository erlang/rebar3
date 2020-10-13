%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

-module(rebar_prv_help).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-include("rebar.hrl").

-define(PROVIDER, help).
-define(DEPS, []).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    State1 = rebar_state:add_provider(State, providers:create([{name, ?PROVIDER},
                                                               {module, ?MODULE},
                                                               {bare, true},
                                                               {deps, ?DEPS},
                                                               {example, "rebar3 help <task>"},
                                                               {short_desc, "Display a list of tasks or help for a given task or subtask."},
                                                               {desc, "Display a list of tasks or help for a given task or subtask."},
                                                               {opts, [
                                                                      {help_task, undefined, undefined, string, "Task to print help for."}
                                                                      ]}])),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    case rebar_state:command_args(State) of
        [] ->
            help(State),
            {ok, State};
        [Name] -> % default namespace
            task_help(default, list_to_atom(Name), State);
        [Namespace, Name] ->
            task_help(list_to_atom(Namespace), list_to_atom(Name), State);
        _ ->
            {error, "Too many arguments given. " ++
                 "Usage: rebar3 help [<namespace>] <task>"}
    end.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%%
%% print help/usage string
%%
help(State) ->
    ?CONSOLE("Rebar3 is a tool for working with Erlang projects.~n", []),
    OptSpecList = rebar3:global_option_spec_list(),
    getopt:usage(OptSpecList, "rebar3", "", []),
    ?CONSOLE("  Set the environment variable DEBUG=1 for detailed output.~n", []),
    ?CONSOLE("Several tasks are available:~n", []),

    providers:help(rebar_state:providers(State)),

    ?CONSOLE("~nRun 'rebar3 help <TASK>' for details.", []).

task_help(Namespace, Name, State) ->
    Providers = rebar_state:providers(State),
    case providers:get_provider(Name, Providers, Namespace) of
        not_found ->
            case providers:get_providers_by_namespace(Name, Providers) of
                [] ->
                    {error, io_lib:format("Unknown task ~p", [Name])};
                NSProviders ->
                    providers:help(NSProviders),
                    {ok, State}
            end;
        Provider ->
            providers:help(Provider),
            {ok, State}
    end.
