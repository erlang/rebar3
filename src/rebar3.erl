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
         help/0,
         parse_args/1,
         version/0,
         log_level/1]).

-include("rebar.hrl").

-ifndef(BUILD_TIME).
-define(BUILD_TIME, "undefined").
-endif.

-ifndef(VCS_INFO).
-define(VCS_INFO, "undefined").
-endif.

-ifndef(OTP_INFO).
-define(OTP_INFO, "undefined").
-endif.

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
        Error ->
            %% Nothing should percolate up from rebar_core;
            %% Dump this error to console
            io:format("Uncaught error in rebar_core: ~p\n", [Error]),
            rebar_utils:delayed_halt(1)
    end.

%% Erlang-API entry point
run(BaseConfig, Commands) ->
    _ = application:load(rebar),
    run_aux(BaseConfig, Commands).

%% ====================================================================
%% Internal functions
%% ====================================================================

run(["help"|RawCmds]) when RawCmds =/= [] ->
    ok = load_rebar_app(),
    Cmds = RawCmds,
    Args = parse_args(Cmds),
    BaseConfig = init_config(Args),
    {BaseConfig1, _} = save_options(BaseConfig, Args),
    BaseConfig2 = init_config1(BaseConfig1),
    rebar_core:help(BaseConfig2, [list_to_atom(C) || C <- Cmds]);
run(["help"]) ->
    help();
run(["info"|_]) ->
    %% Catch calls to 'rebar info' to avoid treating plugins' info/2 functions
    %% as commands.
    ?CONSOLE("Command 'info' not understood or not applicable~n", []);
run(["version"]) ->
    ok = load_rebar_app(),
    %% Display vsn and build time info
    version();
run(RawArgs) ->
    ok = load_rebar_app(),
    %% Parse out command line arguments -- what's left is a list of commands to
    %% run -- and start running commands
    Args = parse_args(RawArgs),
    BaseConfig = init_config(Args),
    {BaseConfig1, Cmds} = save_options(BaseConfig, Args),

    case rebar_state:get(BaseConfig1, enable_profiling, false) of
        true ->
            io:format("Profiling!\n"),
            try
                fprof:apply(fun run_aux/2, [BaseConfig1, Cmds])
            after
                ok = fprof:profile(),
                ok = fprof:analyse([{dest, "fprof.analysis"}])
            end;
        false ->
            run_aux(BaseConfig1, Cmds)
    end.

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
                    rebar_config:consult_file(GlobalConfigFile),
                    rebar_state:new(GlobalConfigFile, Config1);
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

run_aux(BaseConfig, Commands) ->
    %% Make sure crypto is running
    case crypto:start() of
        ok -> ok;
        {error,{already_started,crypto}} -> ok
    end,
    application:start(asn1),
    application:start(public_key),
    application:start(ssl),
    inets:start(),
    [Command | Args] = Commands,
    CommandAtom = list_to_atom(Command),

    BaseConfig1 = init_config1(BaseConfig),

    %% Process each command, resetting any state between each one
    BaseConfig2 = rebar_state:set(BaseConfig1, base_dir, filename:absname(rebar_state:dir(BaseConfig1))),
    {ok, Providers} = application:get_env(rebar, providers),
    BaseConfig3 = rebar_state:create_logic_providers(Providers, BaseConfig2),
    rebar_core:process_command(rebar_state:command_args(BaseConfig3, Args), CommandAtom),
    ok.

%%
%% print help/usage string
%%
help() ->
    OptSpecList = option_spec_list(),
    getopt:usage(OptSpecList, "rebar",
                 "[var=value,...] <command,...>",
                 [{"var=value", "rebar global variables (e.g. force=1)"},
                  {"command", "Command to run (e.g. compile)"}]),

    ?CONSOLE("To see a list of built-in commands, execute rebar -c.~n~n", []),
    ?CONSOLE(
       "Type 'rebar help <CMD1> <CMD2>' for help on specific commands."
       "~n~n", []).

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
            help(),
            rebar_utils:delayed_halt(1)
    end.

save_options(State, {Options, NonOptArgs}) ->
    %% Check options and maybe halt execution
    ok = show_info_maybe_halt(Options, NonOptArgs),

    GlobalDefines = proplists:get_all_values(defines, Options),

    State1 = rebar_state:set(State, defines, GlobalDefines),

    %% Setup profiling flag
    State2 = rebar_state:set(State1, enable_profiling,
                               proplists:get_bool(profile, Options)),

    %% Set global variables based on getopt options
    State3 = set_global_flag(State2, Options, force),
    State4 = case proplists:get_value(jobs, Options, ?DEFAULT_JOBS) of
                  ?DEFAULT_JOBS ->
                      State3;
                  Jobs ->
                      rebar_state:set(State3, jobs, Jobs)
              end,

    %% Filter all the flags (i.e. strings of form key=value) from the
    %% command line arguments. What's left will be the commands to run.
    {State5, RawCmds} = filter_flags(State4, NonOptArgs, []),
    {State5, RawCmds}.

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
    ?CONSOLE("rebar ~s ~s ~s ~s\n",
             [Vsn, ?OTP_INFO, ?BUILD_TIME, ?VCS_INFO]).


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
%% show info and maybe halt execution
%%
show_info_maybe_halt(Opts, NonOptArgs) ->
    false = show_info_maybe_halt(help, Opts, fun help/0),
    false = show_info_maybe_halt(commands, Opts, fun commands/0),
    false = show_info_maybe_halt(version, Opts, fun version/0),
    case NonOptArgs of
        [] ->
            ?CONSOLE("No command to run specified!~n",[]),
            help(),
            rebar_utils:delayed_halt(1);
        _ ->
            ok
    end.

show_info_maybe_halt(O, Opts, F) ->
    case proplists:get_bool(O, Opts) of
        true ->
            F(),
            rebar_utils:delayed_halt(0);
        false ->
            false
    end.

%%
%% print known commands
%%
commands() ->
    S = <<"
clean                                    Clean
compile                                  Compile sources

do

escriptize                               Generate escript archive

shell                                    Start a shell similar to
                                         'erl -pa ebin -pa deps/*/ebin'

update [dep]                             Update source dep

ct
eunit

new

help                                     Show the program options
version                                  Show version information
">>,
    io:put_chars(S).

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
     {help,     $h, "help",     undefined, "Show the program options"},
     {commands, $c, "commands", undefined, "Show available commands"},
     {verbose,  $v, "verbose",  integer,   "Verbosity level (-v, -vv)"},
     {quiet,    $q, "quiet",    boolean,   "Quiet, only print error messages"},
     {version,  $V, "version",  undefined, "Show version information"},
     {force,    $f, "force",    undefined, "Force"},
     {defines,  $D, undefined,  string,    "Define compiler macro"},
     {jobs,     $j, "jobs",     integer,   JobsHelp},
     {config,   $C, "config",   string,    "Rebar config file to use"},
     {profile,  $p, "profile",  undefined, "Profile this run of rebar"},
     {keep_going, $k, "keep-going", undefined,
      "Keep running after a command fails"},
     {recursive, $r, "recursive", boolean,
      "Apply commands to subdirs and dependencies"}
    ].

%%
%% Seperate all commands (single-words) from flags (key=value) and store
%% values into the rebar_state global storage.
%%
filter_flags(State, [], Commands) ->
    {State, lists:reverse(Commands)};
filter_flags(State, [Item | Rest], Commands) ->
    case string:tokens(Item, "=") of
        [Command] ->
            filter_flags(State, Rest, [Command | Commands]);
        [KeyStr, RawValue] ->
            Key = list_to_atom(KeyStr),
            Value = case Key of
                        verbose ->
                            list_to_integer(RawValue);
                        _ ->
                            RawValue
                    end,
            State1 = rebar_state:set(State, Key, Value),
            filter_flags(State1, Rest, Commands);
        Other ->
            ?CONSOLE("Ignoring command line argument: ~p\n", [Other]),
            filter_flags(State, Rest, Commands)
    end.

command_names() ->
    [
     "clean",
     "compile",
     "release",
     "update",
     "escriptize",
     "help",
     "shell",
     "version"
    ].
