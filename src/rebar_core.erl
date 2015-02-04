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

-export([process_command/2
        ,update_code_path/1]).

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
                Command when (Command =:= do orelse Command =:= cover),
                             Namespace =:= undefined ->
                    %% We're definitely in the default namespace. 'do' doesn't
                    %% properly exist for non-default namespaces outside of
                    %% dynamic dispatch calls for namespaces.
                    do(TargetProviders, rebar_state:namespace(State, default));
                Command when Command =:= do; Command =:= as; Command =:= cover ->
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
                            {error, io_lib:format("Invalid option ~s on task ~p", [Option, Command])}
                    end
            end
    end.
-spec do([{atom(), atom()}], rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do([], State) ->
    {ok, State};
do([ProviderName | Rest], State) ->
    Provider = providers:get_provider(ProviderName
                                     ,rebar_state:providers(State)),
    case providers:do(Provider, State) of
        {ok, State1} ->
            do(Rest, State1);
        {error, Error} ->
            {error, Error}
    end.

update_code_path(State) ->
    true = rebar_utils:expand_code_path(),
    LibDirs = rebar_dir:lib_dirs(State),
    DepsDir = rebar_dir:deps_dir(State),
    PluginsDir = rebar_dir:plugins_dir(State),
    _UpdatedCodePaths = update_code_path_(lists:usort([DepsDir, PluginsDir | LibDirs])).

%% ===================================================================
%% Internal functions
%% ===================================================================

update_code_path_(Paths) ->
    LibPaths = expand_lib_dirs(Paths, rebar_dir:get_cwd(), []),
    ok = code:add_pathsa(LibPaths),
    %% track just the paths we added, so we can remove them without
    %% removing other paths added by this dep
    {added, LibPaths}.

expand_lib_dirs([], _Root, Acc) ->
    Acc;
expand_lib_dirs([Dir | Rest], Root, Acc) ->
    %% The current dir should only have an ebin dir.
    %% Other lib dirs contain app directories, so need the wildcard
    Apps = case Dir of
               "." ->
                   [filename:join(Dir, "ebin")];
               _ ->
                   filelib:wildcard(filename:join([Dir, "*", "ebin"]))
           end,
    FqApps = case filename:pathtype(Dir) of
                 absolute -> Apps;
                 _        -> [filename:join([Root, A]) || A <- Apps]
             end,
    expand_lib_dirs(Rest, Root, Acc ++ FqApps).
