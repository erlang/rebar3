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

-export([main/0,
         main/1,
         run/1,
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

%% For running with:
%% erl +sbtu +A0 -noinput -mode minimal -boot start_clean -s rebar3 main -extra "$@"
-spec main() -> no_return().
main() ->
    List = init:get_plain_arguments(),
    main(List).

%% escript Entry point
-spec main(list()) -> no_return().
main(Args) ->
    try run(Args) of
        {ok, _State} ->
            erlang:halt(0);
        Error ->
            handle_error(Error)
    catch
        _:Error ->
            handle_error(Error)
    end.

%% Erlang-API entry point
run(BaseState, Commands) ->
    start_and_load_apps(api),
    BaseState1 = rebar_state:set(BaseState, task, Commands),
    BaseState2 = rebar_state:set(BaseState1, caller, api),

    Verbosity = log_level(),
    ok = rebar_log:init(api, Verbosity),

    run_aux(BaseState2, Commands).

%% ====================================================================
%% Internal functions
%% ====================================================================

run(RawArgs) ->
    start_and_load_apps(command_line),

    BaseState = init_config(),
    BaseState1 = rebar_state:set(BaseState, caller, command_line),

    case erlang:system_info(version) of
        "6.1" ->
            ?WARN("Due to a filelib bug in Erlang 17.1 it is recommended"
                 "you update to a newer release.", []);
        _ ->
            ok
    end,

    {BaseState2, _Args1} = set_options(BaseState1, {[], []}),
    run_aux(BaseState2, RawArgs).

run_aux(State, RawArgs) ->
    State1 = case os:getenv("REBAR_PROFILE") of
                 false ->
                     State;
                 "" ->
                     State;
                 Profile ->
                     rebar_state:apply_profiles(State, [list_to_atom(Profile)])
             end,

    %% bootstrap test profile
    State2 = rebar_state:add_to_profile(State1, test, test_state(State1)),

    %% Process each command, resetting any state between each one
    BaseDir = rebar_state:get(State, base_dir, ?DEFAULT_BASE_DIR),
    State3 = rebar_state:set(State2, base_dir,
                             filename:join(filename:absname(rebar_state:dir(State2)), BaseDir)),

    {ok, Providers} = application:get_env(rebar, providers),
    %% Providers can modify profiles stored in opts, so set default after initializing providers
    State4 = rebar_state:create_logic_providers(Providers, State3),
    State5 = rebar_plugins:project_apps_install(State4),
    State6 = rebar_state:default(State5, rebar_state:opts(State5)),

    {Task, Args} = parse_args(RawArgs),

    State7 = rebar_state:code_paths(State6, default, code:get_path()),

    rebar_core:init_command(rebar_state:command_args(State7, Args), Task).

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
	
    Config1 = rebar_config:merge_locks(Config, rebar_config:consult_lock_file(?LOCK_FILE)),
    %% If $HOME/.config/rebar3/rebar.config exists load and use as global config
    GlobalConfigFile = rebar_dir:global_config(),
    State = case filelib:is_regular(GlobalConfigFile) of
                true ->
                    ?DEBUG("Load global config file ~s", [GlobalConfigFile]),
                    try state_from_global_config(Config1, GlobalConfigFile)
                    catch
                        _:_ ->
                            ?WARN("Global config ~s exists but can not be read. Ignoring global config values.", [GlobalConfigFile]),
                            rebar_state:new(Config1)
                    end;
                false ->
                    rebar_state:new(Config1)
            end,

    %% Determine the location of the rebar executable; important for pulling
    %% resources out of the escript
    State1 = try
                 ScriptName = filename:absname(escript:script_name()),
                 %% Running with 'erl -s rebar3 main' still sets a name for some reason
                 %% so verify it is a real file
                 case filelib:is_regular(ScriptName) of
                     true ->
                         rebar_state:escript_path(State, ScriptName);
                     false ->
                         State
                 end
             catch
                 _:_ ->
                     State
             end,

    %% TODO: Do we need this still? I think it may still be used.
    %% Initialize vsn cache
    rebar_state:set(State1, vsn_cache, dict:new()).

parse_args([]) ->
    parse_args(["help"]);
parse_args([H | Rest]) when H =:= "-h"
                          ; H =:= "--help" ->
    parse_args(["help" | Rest]);
parse_args([H | Rest]) when H =:= "-v"
                          ; H =:= "--version" ->
    parse_args(["version" | Rest]);
parse_args([Task | RawRest]) ->
    {list_to_atom(Task), RawRest}.

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
    {version,  $v, "version",  undefined, "Show version information."},
    {task,     undefined, undefined, string, "Task to run."}
    ].

handle_error(rebar_abort) ->
    erlang:halt(1);
handle_error({error, rebar_abort}) ->
    erlang:halt(1);
handle_error({error, permission_denied}) ->
    erlang:halt(1);
handle_error({error, {Module, Reason}}) ->
    case code:which(Module) of
        non_existing ->
            ?ERROR("Uncaught error in rebar_core. Run with DEBUG=1 to see stacktrace", []),
            ?DEBUG("Uncaught error: ~p ~p", [Module, Reason]),
            ?INFO("When submitting a bug report, please include the output of `rebar3 report \"your command\"`", []);
        _ ->
            ?ERROR(Module:format_error(Reason), [])
    end,
    erlang:halt(1);
handle_error({error, Error}) when is_list(Error) ->
    ?ERROR(Error, []),
    erlang:halt(1);
handle_error(Error) ->
    %% Nothing should percolate up from rebar_core;
    %% Dump this error to console
    ?ERROR("Uncaught error in rebar_core. Run with DEBUG=1 to see stacktrace", []),
    ?DEBUG("Uncaught error: ~p", [Error]),
    case erlang:get_stacktrace() of
        [] -> ok;
        Trace ->
            ?DEBUG("Stack trace to the error location: ~p", [Trace])
    end,
    ?INFO("When submitting a bug report, please include the output of `rebar3 report \"your command\"`", []),
    erlang:halt(1).

start_and_load_apps(Caller) ->
    _ = application:load(rebar),
    %% Make sure crypto is running
    ensure_running(crypto, Caller),
    ensure_running(asn1, Caller),
    ensure_running(public_key, Caller),
    ensure_running(ssl, Caller),
    inets:start(),
    inets:start(httpc, [{profile, rebar}]).

ensure_running(App, Caller) ->
    case application:start(App) of
        ok -> ok;
        {error, {already_started, App}} -> ok;
        {error, Reason} ->
            %% These errors keep rebar3's own configuration to be loaded,
            %% which disables the log level and causes a failure without
            %% showing the error message. Bypass this entirely by overriding
            %% the default value (which allows logging to take place)
            %% and shut things down manually.
            Log = ec_cmd_log:new(warn, Caller),
            ec_cmd_log:error(Log, "Rebar dependency ~p could not be loaded "
                                  "for reason ~p~n", [App, Reason]),
            throw(rebar_abort)
    end.

state_from_global_config(Config, GlobalConfigFile) ->
    rebar_utils:set_httpc_options(),
    GlobalConfigTerms = rebar_config:consult_file(GlobalConfigFile),
    GlobalConfig = rebar_state:new(GlobalConfigTerms),

    %% We don't want to worry about global plugin install state effecting later
    %% usage. So we throw away the global profile state used for plugin install.
    GlobalConfigThrowAway = rebar_state:current_profiles(GlobalConfig, [global]),
    GlobalState = case rebar_state:get(GlobalConfigThrowAway, plugins, []) of
                      [] ->
                          GlobalConfigThrowAway;
                      GlobalPluginsToInstall ->
                          rebar_plugins:handle_plugins(global,
                                                       GlobalPluginsToInstall,
                                                       GlobalConfigThrowAway)
                  end,
    GlobalPlugins = rebar_state:providers(GlobalState),
    GlobalConfig2 = rebar_state:set(GlobalConfig, plugins, []),
    GlobalConfig3 = rebar_state:set(GlobalConfig2, {plugins, global}, rebar_state:get(GlobalConfigThrowAway, plugins, [])),
    rebar_state:providers(rebar_state:new(GlobalConfig3, Config), GlobalPlugins).

test_state(State) ->
    ErlOpts = rebar_state:get(State, erl_opts, []),
    TestOpts = safe_define_test_macro(ErlOpts),
    [{extra_src_dirs, ["test"]}, {erl_opts, TestOpts}].

safe_define_test_macro(Opts) ->
    %% defining a compile macro twice results in an exception so
    %% make sure 'TEST' is only defined once
    case test_defined(Opts) of
       true  -> [];
       false -> [{d, 'TEST'}]
    end.

test_defined([{d, 'TEST'}|_]) -> true;
test_defined([{d, 'TEST', true}|_]) -> true;
test_defined([_|Rest]) -> test_defined(Rest);
test_defined([]) -> false.