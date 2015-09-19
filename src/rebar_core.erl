%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2009 Dave Smith (dizzyd@dizzyd.com)
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%% -------------------------------------------------------------------
-module(rebar_core).

-export([init_command/2, process_namespace/2, process_command/2, do/2, format_error/1]).

-include("rebar.hrl").
-include_lib("providers/include/providers.hrl").

init_command(State, do) ->
    process_command(rebar_state:namespace(State, default), do);
init_command(State, as) ->
    process_command(rebar_state:namespace(State, default), as);
init_command(State, Command) ->
    case process_namespace(State, Command) of
        {ok, State1, Command1} ->
            process_command(State1, Command1);
        {error, Reason} ->
            {error, Reason}
    end.

process_namespace(_State, as) ->
    {error, "Namespace 'as' is forbidden"};
process_namespace(State, Command) ->
    Providers = rebar_state:providers(State),
    CommandProvider = providers:get_provider(Command, Providers, default),
    case CommandProvider of
        not_found ->
            case providers:get_providers_by_namespace(Command, Providers) of
                [] ->
                    {error, io_lib:format("Command ~p not found", [Command])};
                _ ->
                    %% Replay 'do' as a command of that namespace
                    {ok, rebar_state:namespace(State, Command), do}
            end;
        _ ->
            {ok, rebar_state:namespace(State, default), Command}
    end.

-spec process_command(rebar_state:t(), atom()) -> {ok, rebar_state:t()} | {error, string()} | {error, {module(), any()}}.
process_command(State, Command) ->
    %% ? rebar_prv_install_deps:setup_env(State),
    Providers = rebar_state:providers(State),
    Namespace = rebar_state:namespace(State),
    TargetProviders = providers:get_target_providers(Command, Providers, Namespace),
    CommandProvider = providers:get_provider(Command, Providers, Namespace),
    ?DEBUG("Expanded command sequence to be run: ~p", [TargetProviders]),
    case CommandProvider of
        not_found when Command =/= do ->
            case Namespace of
                default ->
                    {error, io_lib:format("Command ~p not found", [Command])};
                _ ->
                    {error, io_lib:format("Command ~p not found in namespace ~p",
                                          [Command, Namespace])}
            end;
        not_found when Command =:= do, Namespace =/= default ->
            do([{default, do} | TargetProviders], State);
        CommandProvider ->
            case Command of
                do ->
                    do(TargetProviders, State);
                as ->
                    do(TargetProviders, State);
                _ ->
                    Profiles = providers:profiles(CommandProvider),
                    State1 = rebar_state:apply_profiles(State, Profiles),
                    Opts = providers:opts(CommandProvider)++rebar3:global_option_spec_list(),
                    case getopt:parse(Opts, rebar_state:command_args(State1)) of
                        {ok, Args} ->
                            State2 = rebar_state:command_parsed_args(State1, Args),
                            do(TargetProviders, State2);
                        {error, {invalid_option, Option}} ->
                            {error, io_lib:format("Invalid option ~s on task ~p", [Option, Command])};
                        {error, {invalid_option_arg, {Option, Arg}}} ->
                            {error, io_lib:format("Invalid argument ~s to option ~s", [Arg, Option])};
                        {error, {missing_option_arg, Option}} ->
                            {error, io_lib:format("Missing argument to option ~s", [Option])}
                    end
            end
    end.

-spec do([{atom(), atom()}], rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()} | {error, {module(), any()}}.
do([], State) ->
    {ok, State};
do([ProviderName | Rest], State) ->
    %% Special providers like 'as', 'do' or some hooks may be passed
    %% as a tuple {Namespace, Name}, otherwise not. Handle them
    %% on a per-need basis.
    Provider = case ProviderName of
        {Namespace, Name} ->
            providers:get_provider(Name
                                  ,rebar_state:providers(State)
                                  ,Namespace);
        _ ->
            providers:get_provider(ProviderName
                                  ,rebar_state:providers(State)
                                  ,rebar_state:namespace(State))
    end,

    try providers:do(Provider, State) of
        {ok, State1} ->
            do(Rest, State1);
        {error, Error} ->
            {error, Error}
    catch
        error:undef ->
            Stack = erlang:get_stacktrace(),
            case Stack of
                [{ProviderName, do, [_], _}|_] ->
                    %% This should really only happen if a plugin provider doesn't export do/1
                    ?DEBUG("Undefined call to provider's do/1 function:~n~p", [Stack]),
                    ?PRV_ERROR({bad_provider_namespace, ProviderName});
                _ -> % re-raise
                    erlang:raise(error, undef, Stack)
            end;
        error:{badrecord,provider} ->
            {error, ProviderName}
    end.

format_error({bad_provider_namespace, {Namespace, Name}}) ->
    io_lib:format("Undefined command ~s in namespace ~s", [Name, Namespace]);
format_error({bad_provider_namespace, Name}) ->
    io_lib:format("Undefined command ~s", [Name]).
