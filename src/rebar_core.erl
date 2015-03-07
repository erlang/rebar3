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

-export([process_command/2]).

-include("rebar.hrl").

-spec process_command(rebar_state:t(), atom()) -> {ok, rebar_state:t()} | {error, string()}.
process_command(State, Command) ->
    %% ? rebar_prv_install_deps:setup_env(State),
    Providers = rebar_state:providers(State),
    Namespace = rebar_state:namespace(State),
    {TargetProviders, CommandProvider} =
        case Namespace of
            undefined ->
                %% undefined namespace means 'default', but on the first run;
                %% it is used as an implicit counter for the first vs. subsequent
                %% runs.
                {providers:get_target_providers(Command, Providers, default),
                 providers:get_provider(Command, Providers, default)};
            _ ->
                {providers:get_target_providers(Command, Providers, Namespace),
                 providers:get_provider(Command, Providers, Namespace)}
        end,

    case CommandProvider of
        not_found ->
            case Namespace of
                undefined ->
                    %% On the first run (Namespace = undefined), we use the
                    %% unfound command name to be a namespace.
                    case providers:get_providers_by_namespace(Command, Providers) of
                        [] ->
                            {error, io_lib:format("Command ~p not found", [Command])};
                        _ ->
                            do([{default, do} | TargetProviders],
                               rebar_state:namespace(State, Command))
                    end;
                default ->
                    {error, io_lib:format("Command ~p not found", [Command])};
                _ ->
                    {error, io_lib:format("Command ~p not found in namespace ~p",
                                          [Command, Namespace])}
            end;
        CommandProvider ->
            case Command of
                Command when Command =:= do, Namespace =:= undefined ->
                    %% We're definitely in the default namespace. 'do' doesn't
                    %% properly exist for non-default namespaces outside of
                    %% dynamic dispatch calls for namespaces.
                    do(TargetProviders, rebar_state:namespace(State, default));
                Command when Command =:= as, Namespace =:= undefined ->
                    %% Because of the possible forms such as:
                    %% 'rebar3 as profile task`, `rebar3 as profile do task`
                    %% and `rebar3 as profile namespace task`, we can only know
                    %% whether we're in the first 'as' or a namespace 'as' by
                    %% looking at profiles (as makes them non-default).
                    %% The namespace 'as' is banned. It also makes it impossible
                    %% to have both $REBAR_PROFILE set and use 'as' in a command
                    case rebar_state:current_profiles(State) of
                        [default] ->
                            do([{default, hd(TargetProviders)} | tl(TargetProviders)], State);
                        _ ->
                            {error, "Namespace 'as' is forbidden"}
                    end;
                Command when Command =:= do ->
                    do(TargetProviders, State);
                _ ->
                    Profiles = providers:profiles(CommandProvider),
                    State1 = rebar_state:apply_profiles(State, Profiles),
                    Opts = providers:opts(CommandProvider)++rebar3:global_option_spec_list(),
                    case getopt:parse(Opts, rebar_state:command_args(State1)) of
                        {ok, Args} ->
                            State2 = rebar_state:command_parsed_args(State1, Args),
                            case Namespace of
                                undefined -> % we're executing commands, set the default namespace
                                    do(TargetProviders, rebar_state:namespace(State2, default));
                                _ ->
                                    do(TargetProviders, State2)
                            end;
                        {error, {invalid_option, Option}} ->
                            {error, io_lib:format("Invalid option ~s on task ~p", [Option, Command])}
                    end
            end
    end.

-spec do([{atom(), atom()}], rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
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
    case providers:do(Provider, State) of
        {ok, State1} ->
            do(Rest, State1);
        {error, Error} ->
            {error, Error}
    end.
