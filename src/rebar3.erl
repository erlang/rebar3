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
         option_spec_list/0,
         parse_args/1,
         version/0,
         log_level/1]).

-include("rebar.hrl").

-define(DEFAULT_JOBS, 3).

%% ====================================================================
%% Public API
%% ====================================================================

%% escript Entry point
main(Args) ->
    case catch(run(Args)) of
        ok ->
            ok;
        rebar_abort ->
            rebar_utils:delayed_halt(1);
        {error, Error} ->
            ?ERROR(Error, []),
            rebar_utils:delayed_halt(1);
        Error ->
            %% Nothing should percolate up from rebar_core;
            %% Dump this error to console
            ?ERROR("Uncaught error in rebar_core. Run with -vvv to see stacktrace~n", []),
            ?DEBUG("Uncaught error: ~p~n", [Error]),
            rebar_utils:delayed_halt(1)
    end.

%% Erlang-API entry point
run(BaseState, Command) ->
    _ = application:load(rebar),
    BaseState1 = rebar_state:set(BaseState, task, Command),
    run_aux(BaseState1, []).

%% ====================================================================
%% Internal functions
%% ====================================================================

run(RawArgs) ->
    ok = load_rebar_app(),
    %% Parse out command line arguments -- what's left is a list of commands to
    %% run -- and start running commands
    Args = parse_args(RawArgs),
    BaseConfig = init_config(Args),
    {BaseConfig1, Args1} = set_options(BaseConfig, Args),
    run_aux(BaseConfig1, Args1).

load_rebar_app() ->
    %% Pre-load the rebar app so that we get default configuration
    ok = application:load(rebar).

init_config({Options, _NonOptArgs}) ->
    %% Initialize logging system
    Verbosity = log_level(Options),
    ok = rebar_log:init(command_line, Verbosity),

    Config = case proplists:get_value(config, Options) of
                 undefined ->
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

    %% If $HOME/.rebar/config exists load and use as global config
    GlobalConfigFile = filename:join([os:getenv("HOME"), ".rebar", "config"]),
    State = case filelib:is_regular(GlobalConfigFile) of
                true ->
                    ?DEBUG("Load global config file ~p~n",
                           [GlobalConfigFile]),
                    GlobalConfig = rebar_state:new(rebar_config:consult_file(GlobalConfigFile)),
                    rebar_state:new(GlobalConfig, Config1);
                false ->
                    rebar_state:new(Config1)
            end,

    %% Initialize vsn cache
    rebar_state:set(State, vsn_cache, dict:new()).

init_config1(BaseConfig) ->
    %% Determine the location of the rebar executable; important for pulling
    %% resources out of the escript
    try
        ScriptName = filename:absname(escript:script_name()),
        rebar_state:set(BaseConfig, escript, ScriptName)
    catch
        _:_ ->
            BaseConfig
    end.

run_aux(State, Args) ->
    %% Make sure crypto is running
    case crypto:start() of
        ok -> ok;
        {error,{already_started,crypto}} -> ok
    end,
    application:start(asn1),
    application:start(public_key),
    application:start(ssl),
    inets:start(),

    State1 = init_config1(State),

    code:add_pathsa([filename:join(rebar_utils:get_cwd(), "plugins")]),
    %% Process each command, resetting any state between each one
    State2 = rebar_state:set(State1, base_dir, filename:absname(rebar_state:dir(State1))),
    {ok, Providers} = application:get_env(rebar, providers),

    {ok, PluginProviders, State3} = rebar_plugins:install(State2),
    rebar_core:update_code_path(State),

    State4 = rebar_state:create_logic_providers(Providers++PluginProviders, State3),
    Task = rebar_state:get(State4, task, "help"),
    rebar_core:process_command(rebar_state:command_args(State4, Args), list_to_atom(Task)).

%%
%% Parse command line arguments using getopt and also filtering out any
%% key=value pairs. What's left is the list of commands to run
%%
parse_args(RawArgs) ->
    %% Parse getopt options
    OptSpecList = option_spec_list(),
    case getopt:parse(OptSpecList, RawArgs) of
        {ok, Args} ->
            Args;
        {error, {Reason, Data}} ->
            ?ERROR("~s ~p~n~n", [Reason, Data]),
            rebar_utils:delayed_halt(1)
    end.

set_options(State, {Options, NonOptArgs}) ->
    GlobalDefines = proplists:get_all_values(defines, Options),

    State1 = rebar_state:set(State, defines, GlobalDefines),

    %% Set global variables based on getopt options
    State2 = set_global_flag(State1, Options, force),
    State3 = case proplists:get_value(jobs, Options, ?DEFAULT_JOBS) of
                 ?DEFAULT_JOBS ->
                     State2;
                 Jobs ->
                     rebar_state:set(State2, jobs, Jobs)
             end,

    Task = proplists:get_value(task, Options, "help"),

    {rebar_state:set(State3, task, Task), NonOptArgs}.

%%
%% get log level based on getopt option
%%
log_level(Options) ->
    case proplists:get_bool(quiet, Options) of
        true ->
            rebar_log:error_level();
        false ->
            DefaultLevel = rebar_log:default_level(),
            case proplists:get_all_values(verbose, Options) of
                [] ->
                    DefaultLevel;
                Verbosities ->
                    DefaultLevel + lists:last(Verbosities)
            end
    end.

%%
%% show version information and halt
%%
version() ->
    {ok, Vsn} = application:get_key(rebar, vsn),
    ?CONSOLE("rebar ~s on Erlang/OTP ~s Erts ~s~n",
             [Vsn, erlang:system_info(otp_release), erlang:system_info(version)]).


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
option_spec_list() ->
    Jobs = ?DEFAULT_JOBS,
    JobsHelp = io_lib:format(
                 "Number of concurrent workers a command may use. Default: ~B",
                 [Jobs]),
    [
     %% {Name, ShortOpt, LongOpt, ArgSpec, HelpMsg}
     {help,     $h, "help",     undefined, "Print this help."},
     {verbose,  $v, "verbose",  integer,   "Verbosity level (-v, -vv)."},
     {version,  $V, "version",  undefined, "Show version information."},
     {jobs,     $j, "jobs",     integer,   JobsHelp},
     {config,   $C, "config",   string,    "Rebar config file to use."},
     {task,     undefined, undefined, string, "Task to run."}
    ].
