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
-module(rebar3).

-export([main/1,
         run/2,
         global_option_spec_list/0,
         init_config/0,
         set_options/2,
         parse_args/1,
         version/0,
         log_level/0]).

-include("rebar.hrl").

%% ====================================================================
%% Public API
%% ====================================================================

%% escript Entry point
main(Args) ->
    case catch(run(Args)) of
        {ok, _State} ->
            ok;
        rebar_abort ->
            rebar_utils:delayed_halt(1);
        {error, rebar_abort} ->
            rebar_utils:delayed_halt(1);
        {error, {Module, Reason}} ->
            case code:which(Module) of
                non_existing ->
                    ?ERROR("Uncaught error in rebar_core. Run with DEBUG=1 to see stacktrace", []),
                    ?DEBUG("Uncaught error: ~p ~p", [Module, Reason]);
                _ ->
                    ?ERROR(Module:format_error(Reason), [])
            end,
            rebar_utils:delayed_halt(1);
        {error, Error} when is_list(Error) ->
            ?ERROR(Error, []),
            rebar_utils:delayed_halt(1);
        Error ->
            %% Nothing should percolate up from rebar_core;
            %% Dump this error to console
            ?ERROR("Uncaught error in rebar_core. Run with DEBUG=1 to see stacktrace", []),
            ?DEBUG("Uncaught error: ~p", [Error]),
            rebar_utils:delayed_halt(1)
    end.

%% Erlang-API entry point
run(BaseState, Command) ->
    _ = application:load(rebar),
    BaseState1 = rebar_state:set(BaseState, task, Command),
    run_aux(BaseState1, [], [Command]).

%% ====================================================================
%% Internal functions
%% ====================================================================

run(RawArgs) ->
    _ = application:load(rebar),
    {GlobalPluginProviders, BaseConfig} = init_config(),

    case erlang:system_info(version) of
        "6.1" ->
            ?WARN("Due to a filelib bug in Erlang 17.1 it is recommended"
                 "you update to a newer release.", []);
        _ ->
            ok
    end,

    {BaseConfig1, _Args1} = set_options(BaseConfig, {[], []}),
    run_aux(BaseConfig1, GlobalPluginProviders, RawArgs).

run_aux(State, GlobalPluginProviders, RawArgs) ->
    %% Make sure crypto is running
    case crypto:start() of
        ok -> ok;
        {error,{already_started,crypto}} -> ok
    end,
    application:start(asn1),
    application:start(public_key),
    application:start(ssl),
    inets:start(),

    %% Process each command, resetting any state between each one
    BaseDir = rebar_utils:base_dir(State),
    State2 = rebar_state:set(State, base_dir,
                            filename:join(filename:absname(rebar_state:dir(State)), BaseDir)),

    {ok, Providers} = application:get_env(rebar, providers),
    {ok, PluginProviders, State3} = rebar_plugins:install(State2),
    rebar_core:update_code_path(State3),

    AllProviders = Providers++PluginProviders++GlobalPluginProviders,
    State4 = rebar_state:create_logic_providers(AllProviders, State3),
    {Task, Args} = parse_args(RawArgs),

    rebar_core:process_command(rebar_state:command_args(State4, Args), list_to_atom(Task)).

init_config() ->
    %% Initialize logging system
    Verbosity = log_level(),
    ok = rebar_log:init(command_line, Verbosity),

    Config = case os:getenv("REBAR_CONFIG") of
                 false ->
                     rebar_config:consult_file(?DEFAULT_CONFIG_FILE);
                 ConfigFile ->
                     rebar_config:consult_file(ConfigFile)
             end,

    Config1 = case rebar_config:consult_file(?LOCK_FILE) of
                  [D] ->
                      [{locks, D} | Config];
                  _ ->
                      Config
              end,

    %% If $HOME/.rebar3/config exists load and use as global config
    Home = rebar_utils:home_dir(),
    GlobalConfigFile = filename:join([Home, ?CONFIG_DIR, "config"]),
    State = case filelib:is_regular(GlobalConfigFile) of
                true ->
                    ?DEBUG("Load global config file ~p",
                           [GlobalConfigFile]),
                    GlobalConfig = rebar_state:new(global, rebar_config:consult_file(GlobalConfigFile)),
                    {ok, PluginProviders, GlobalConfig1} = rebar_plugins:install(GlobalConfig),
                    rebar_state:new(GlobalConfig1, Config1);
                false ->
                    PluginProviders = [],
                    rebar_state:new(Config1)
            end,

    %% Determine the location of the rebar executable; important for pulling
    %% resources out of the escript
    State1 = try
                 ScriptName = filename:absname(escript:script_name()),
                 rebar_state:set(State, escript, ScriptName)
             catch
                 _:_ ->
                     State
             end,

    %% TODO: Do we need this still? I think it may still be used.
    %% Initialize vsn cache
    {PluginProviders, rebar_state:set(State1, vsn_cache, dict:new())}.

%%
%% Parse command line arguments using getopt and also filtering out any
%% key=value pairs. What's left is the list of commands to run
%%
parse_args([]) ->
    parse_args(["help"]);
parse_args([H | Rest]) when H =:= "-h"
                          ; H =:= "--help" ->
    parse_args(["help" | Rest]);
parse_args([H | Rest]) when H =:= "-v"
                          ; H =:= "--version" ->
    parse_args(["version" | Rest]);
parse_args([RawTask | RawRest]) ->
    {RawTask, RawRest}.

set_options(State, {Options, NonOptArgs}) ->
    GlobalDefines = proplists:get_all_values(defines, Options),

    State1 = rebar_state:set(State, defines, GlobalDefines),

    %% Set global variables based on getopt options
    State2 = set_global_flag(State1, Options, force),

    Task = proplists:get_value(task, Options, "help"),

    {rebar_state:set(State2, task, Task), NonOptArgs}.

%%
%% get log level based on getopt option
%%
log_level() ->
    case os:getenv("QUIET") of
        false ->
            DefaultLevel = rebar_log:default_level(),
            case os:getenv("DEBUG") of
                false ->
                    DefaultLevel;
                _ ->
                    DefaultLevel + 3
            end;
         _ ->
            rebar_log:error_level()
    end.

%%
%% show version information and halt
%%
version() ->
    {ok, Vsn} = application:get_key(rebar, vsn),
    ?CONSOLE("rebar ~s on Erlang/OTP ~s Erts ~s",
             [Vsn, erlang:system_info(otp_release), erlang:system_info(version)]).



%% TODO: Actually make it 'global'
%%
%% set global flag based on getopt option boolean value
%%
set_global_flag(State, Options, Flag) ->
    Value = case proplists:get_bool(Flag, Options) of
                true ->
                    "1";
                false ->
                    "0"
            end,
    rebar_state:set(State, Flag, Value).

%%
%% options accepted via getopt
%%
global_option_spec_list() ->
    [
    %% {Name, ShortOpt, LongOpt, ArgSpec, HelpMsg}
    {help,     $h, "help",     undefined, "Print this help."},
    %{verbose,  $v, "verbose",  integer,   "Verbosity level (-v, -vv)."},
    {version,  $V, "version",  undefined, "Show version information."},
    %{config,   $C, "config",   string,    "Rebar config file to use."},
    {task,     undefined, undefined, string, "Task to run."}
    ].
