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
%%
%% @doc Main module for rebar3. Supports two interfaces; one for escripts,
%% and one for usage as a library (although rebar3 makes a lot of
%% assumptions about its environment, making it a bit tricky to use as
%% a lib).
%%
%% This module's job is mostly to set up the root environment for rebar3
%% and handle global options (mostly all from the ENV) and make them
%% accessible to the rest of the run.
%% @end
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

%% @doc For running with:
%% erl +sbtu +A1 -noinput -mode minimal -boot start_clean -s rebar3 main -extra "$@"
-spec main() -> no_return().
main() ->
    List = init:get_plain_arguments(),
    main(List).

%% @doc escript Entry point
-spec main(list()) -> no_return().
main(Args) ->
    try run(Args) of
        {ok, _State} ->
            erlang:halt(0);
        Error ->
            handle_error(Error, [])
    catch
        ?WITH_STACKTRACE(_,Error,Stacktrace)
            handle_error(Error, Stacktrace)
    end.

%% @doc Erlang-API entry point
-spec run(rebar_state:t(), [string()]) -> {ok, rebar_state:t()} | {error, term()}.
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

%% @private sets up the rebar3 environment based on the command line
%% arguments passed, if they have any relevance; used to translate
%% from the escript call-site into a common one with the library
%% usage.
-spec run([any(), ...]) -> {ok, rebar_state:t()} | {error, term()}.
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

%% @private Junction point between the CLI and library entry points.
%% From here on the module's role is a shared path here to finish
%% up setting the environment for the run.
-spec run_aux(rebar_state:t(), [string()]) ->
    {ok, rebar_state:t()} | {error, term()}.
run_aux(State, RawArgs) ->
    io:setopts([{encoding, unicode}]),
    %% Profile override; can only support one profile
    State1 = case os:getenv("REBAR_PROFILE") of
                 false ->
                     State;
                 "" ->
                     State;
                 Profile ->
                     rebar_state:apply_profiles(State, [list_to_atom(Profile)])
             end,

    rebar_utils:check_min_otp_version(rebar_state:get(State1, minimum_otp_vsn, undefined)),
    rebar_utils:check_blacklisted_otp_versions(rebar_state:get(State1, blacklisted_otp_vsns, undefined)),

    %% Maybe change the default hex CDN
    HexCDN = case os:getenv("HEX_CDN") of
                 false -> ?DEFAULT_CDN;
                 [] -> ?DEFAULT_CDN;
                 CDN -> CDN
             end,
    State2 = rebar_state:set(State1, rebar_packages_cdn, HexCDN),

    Compilers = application:get_env(rebar, compilers, []),
    State0 = rebar_state:compilers(State2, Compilers),

    %% TODO: this means use of REBAR_PROFILE=profile will replace the repos with
    %% the repos defined in the profile. But it will not work with `as profile`.
    %% Maybe it shouldn't work with either to be consistent?
    Resources = application:get_env(rebar, resources, []),
    State2_ = rebar_state:create_resources(Resources, State0),

    %% bootstrap test profile
    State3 = rebar_state:add_to_profile(State2_, test, test_state(State1)),

    BaseDir = case os:getenv("REBAR_BASE_DIR") of
                  D when D =:= false orelse D =:= "" ->
                      rebar_state:get(State, base_dir, ?DEFAULT_BASE_DIR);
                  Dir ->
                      Dir
              end,
    State4 = rebar_state:set(State3, base_dir,
                             filename:join(filename:absname(rebar_state:dir(State3)), BaseDir)),

    State5 = case os:getenv("REBAR_CACHE_DIR") of
                false ->
                    State4;
                CachePath ->
                    rebar_state:set(State4, global_rebar_dir, CachePath)
            end,

    {ok, Providers} = application:get_env(rebar, providers),
    %% Providers can modify profiles stored in opts, so set default after initializing providers
    State6 = rebar_state:create_logic_providers(Providers, State5),
    %% Initializing project_plugins which can override default providers
    State7 = rebar_plugins:project_plugins_install(State6),
    State8 = rebar_plugins:top_level_install(State7),

    State9 = rebar_state:default(State8, rebar_state:opts(State8)),

    {Task, Args} = parse_args(RawArgs),
    Offline = case lists:member("--offline", Args) of
                  true ->
                      %% We store this redundantly in env, because some APIs
                      %% don't get a state.
                      os:putenv("REBAR_OFFLINE", "1"),
                      true;
                  false ->
                      os:getenv("REBAR_OFFLINE") =:= "1"
              end,
    State10 = rebar_state:set(State9, offline, Offline),

    State11 = rebar_state:code_paths(State10, default, code:get_path()),

    case rebar_core:init_command(rebar_state:command_args(State11, Args), Task) of
        {ok, State12} ->
            case rebar_state:get(State11, caller, command_line) of
                api ->
                    rebar_paths:unset_paths([deps, plugins], State11),
                    {ok, State12};
                _ ->
                    {ok, State12}
            end;
        Other ->
            Other
    end.



%% @doc set up base configuration having to do with verbosity, where
%% to find config files, and so on, and return an internal rebar3 state term.
-spec init_config() -> rebar_state:t().
init_config() ->
    rebar_utils:set_httpc_options(),

    %% Initialize logging system
    Verbosity = log_level(),
    ok = rebar_log:init(command_line, Verbosity),

    Config = rebar_config:consult_root(),
    Config1 = rebar_config:merge_locks(Config, rebar_config:consult_lock_file(?LOCK_FILE)),
    InitState = rebar_state:new(Config1),

    %% If $HOME/.config/rebar3/rebar.config exists load and use as global config
    GlobalConfigFile = rebar_dir:global_config(InitState),
    State = case filelib:is_regular(GlobalConfigFile) of
                true ->
                    ?DEBUG("Load global config file ~ts", [GlobalConfigFile]),
                    try state_from_global_config(Config1, GlobalConfigFile)
                    catch
                        _:_ ->
                            ?WARN("Global config ~ts exists but can not be read. Ignoring global config values.", [GlobalConfigFile]),
                            InitState
                    end;
                false ->
                    InitState
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

%% @doc Parse basic rebar3 arguments to find the top-level task
%% to be run; this parsing is only partial from the point of view that
%% runs done with arguments like `as $PROFILE do $TASK' will just
%% return `as', which is then in charge of doing a more dynamic
%% dispatch.
%% If no arguments are given, the `help' task is returned.
%% If special arguments like `-h' or `-v' are translated to `help'
%% and `version' tasks.
%% The unparsed parts of arguments are returned in:
%% `{Task, Rest}'.
-spec parse_args([string()]) -> {atom(), [string()]}.
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

%% @private actually not too sure what this does anymore.
-spec set_options(rebar_state:t(),{[any()],[any()]}) -> {rebar_state:t(),[any()]}.
set_options(State, {Options, NonOptArgs}) ->
    GlobalDefines = proplists:get_all_values(defines, Options),

    State1 = rebar_state:set(State, defines, GlobalDefines),

    %% Set global variables based on getopt options
    State2 = set_global_flag(State1, Options, force),

    Task = proplists:get_value(task, Options, "help"),

    {rebar_state:set(State2, task, Task), NonOptArgs}.

%% @doc get log level based on getopt options and ENV
-spec log_level() -> integer().
log_level() ->
    case os:getenv("QUIET") of
        Q when Q == false; Q == "" ->
            case os:getenv("DIAGNOSTIC") of
                Di when Di == false; Di == "" ->
                    case os:getenv("DEBUG") of
                        D when D == false; D == "" ->
                            rebar_log:default_level();
                        _ ->
                            rebar_log:debug_level()
                    end;
                _ ->
                    rebar_log:diagnostic_level()
            end;
         _ ->
            rebar_log:error_level()
    end.

%% @doc show version information
-spec version() -> ok.
version() ->
    {ok, Vsn} = application:get_key(rebar, vsn),
    ?CONSOLE("rebar ~ts on Erlang/OTP ~ts Erts ~ts",
             [Vsn, erlang:system_info(otp_release), erlang:system_info(version)]).

%% @private set global flag based on getopt option boolean value
%% TODO: Actually make it 'global'
-spec set_global_flag(rebar_state:t(), list(), term()) -> rebar_state:t().
set_global_flag(State, Options, Flag) ->
    Value = case proplists:get_bool(Flag, Options) of
                true ->
                    "1";
                false ->
                    "0"
            end,
    rebar_state:set(State, Flag, Value).


%% @doc options accepted via getopt
-spec global_option_spec_list() -> [{atom(), char(), string(), atom(), string()}, ...].
global_option_spec_list() ->
    [
    %% {Name,  ShortOpt,  LongOpt,    ArgSpec,   HelpMsg}
    {help,     $h,        "help",     undefined, "Print this help."},
    {version,  $v,        "version",  undefined, "Show version information."},
    {task,     undefined, undefined,  string,    "Task to run."}
    ].

%% @private translate unhandled errors and internal return codes into proper
%% erroneous program exits.
-spec handle_error(term(), term()) -> no_return().
handle_error(rebar_abort, _) ->
    erlang:halt(1);
handle_error({error, rebar_abort}, _) ->
    erlang:halt(1);
handle_error({error, {Module, Reason}}, Stacktrace) ->
    case code:which(Module) of
        non_existing ->
            ?CRASHDUMP("~p: ~p~n~p~n~n", [Module, Reason, Stacktrace]),
            ?ERROR("Uncaught error in rebar_core. Run with DIAGNOSTIC=1 to stacktrace or consult rebar3.crashdump", []),
            ?DEBUG("Uncaught error: ~p ~p", [Module, Reason]),
            ?INFO("When submitting a bug report, please include the output of `rebar3 report \"your command\"`", []);
        _ ->
            ?ERROR("~ts", [Module:format_error(Reason)])
    end,
    erlang:halt(1);
handle_error({error, Error}, _) when is_list(Error) ->
    ?ERROR("~ts", [Error]),
    erlang:halt(1);
handle_error(Error, StackTrace) ->
    %% Nothing should percolate up from rebar_core;
    %% Dump this error to console
    ?CRASHDUMP("Error: ~p~n~p~n~n", [Error, StackTrace]),
    ?ERROR("Uncaught error in rebar_core. Run with DIAGNOSTIC=1 to see stacktrace or consult rebar3.crashdump", []),
    ?DEBUG("Uncaught error: ~p", [Error]),
    case StackTrace of
        [] -> ok;
        Trace ->
            ?DEBUG("Stack trace to the error location:~n~p", [Trace])
    end,
    ?INFO("When submitting a bug report, please include the output of `rebar3 report \"your command\"`", []),
    erlang:halt(1).

%% @private Boot Erlang dependencies; problem is that escripts don't auto-boot
%% stuff the way releases do and we have to do it by hand.
%% This also lets us detect and show nicer errors when a critical lib is
%% not supported
-spec start_and_load_apps(command_line|api) -> term().
start_and_load_apps(Caller) ->
    _ = application:load(rebar),
    %% Make sure crypto is running
    ensure_running(crypto, Caller),
    ensure_running(asn1, Caller),
    ensure_running(public_key, Caller),
    case os:getenv("REBAR_OFFLINE") of
        "1" ->
            ok;
        _ ->
            ensure_running(ssl, Caller),
            ensure_running(inets, Caller),
            inets:start(httpc, [{profile, rebar}])
    end.

%% @doc Make sure a required app is running, or display an error message
%% and abort if there's a problem.
-spec ensure_running(atom(), command_line|api) -> ok | no_return().
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

-spec state_from_global_config([term()], file:filename()) -> rebar_state:t().
state_from_global_config(Config, GlobalConfigFile) ->
    GlobalConfigTerms = rebar_config:consult_file(GlobalConfigFile),
    GlobalConfigTmp = rebar_state:new(GlobalConfigTerms),

    GlobalConfig = case os:getenv("REBAR_CACHE_DIR") of
                false ->
                    GlobalConfigTmp;
                CachePath ->
                    rebar_state:set(GlobalConfigTmp, global_rebar_dir, CachePath)
            end,

    %% We don't want to worry about global plugin install state effecting later
    %% usage. So we throw away the global profile state used for plugin install.
    GlobalConfigThrowAway0 = rebar_state:current_profiles(GlobalConfig, [global]),

    Resources = application:get_env(rebar, resources, []),
    GlobalConfigThrowAway = rebar_state:create_resources(Resources, GlobalConfigThrowAway0),

    Compilers = application:get_env(rebar, compilers, []),
    GlobalConfigThrowAway1 = rebar_state:compilers(GlobalConfigThrowAway, Compilers),

    GlobalState = case rebar_state:get(GlobalConfigThrowAway1, plugins, []) of
                      [] ->
                          GlobalConfigThrowAway1;
                      GlobalPluginsToInstall ->
                          rebar_plugins:handle_plugins(global,
                                                       GlobalPluginsToInstall,
                                                       GlobalConfigThrowAway1)
                  end,
    GlobalPlugins = rebar_state:providers(GlobalState),
    GlobalConfig2 = rebar_state:set(GlobalConfig, plugins, []),
    GlobalConfig3 = rebar_state:set(GlobalConfig2, {plugins, global},
                                    rebar_state:get(GlobalConfigThrowAway1, plugins, [])),
    rebar_state:providers(rebar_state:new(GlobalConfig3, Config), GlobalPlugins).

-spec test_state(rebar_state:t()) -> [{'extra_src_dirs',[string()]} | {'erl_opts',[any()]}].
test_state(State) ->
    %% Fetch the test profile's erl_opts only
    Opts = rebar_state:opts(State),
    Profiles = rebar_opts:get(Opts, profiles, []),
    ProfileOpts = proplists:get_value(test, Profiles, []),
    ErlOpts = proplists:get_value(erl_opts, ProfileOpts, []),
    TestOpts = safe_define_test_macro(ErlOpts),
    %% Only define the test directory if it wasn't set by the user already,
    %% otherwise our definition may clash with theirs
    Extras = rebar_opts:get(Opts, extra_src_dirs, []),
    ExtrasTest = proplists:get_value(extra_src_dirs, ProfileOpts, []),
    IsDefined = lists:any(fun({"test", _}) -> true
                          ;  ("test") -> true
                          ;  (_) -> false
                          end, Extras ++ ExtrasTest),
    case IsDefined of
        true -> [];
        false -> [{extra_src_dirs, [{"test", [{recursive, false}]}]}]
    end ++ [{erl_opts, TestOpts}].

-spec safe_define_test_macro([any()]) -> [any()] | [{'d',atom()} | any()].
safe_define_test_macro(Opts) ->
    %% defining a compile macro twice results in an exception so
    %% make sure 'TEST' is only defined once
    case test_defined(Opts) of
       true  -> Opts;
       false -> [{d, 'TEST'}|Opts]
    end.

-spec test_defined([{d, atom()} | {d, atom(), term()} | term()]) -> boolean().
test_defined([{d, 'TEST'}|_]) -> true;
test_defined([{d, 'TEST', true}|_]) -> true;
test_defined([_|Rest]) -> test_defined(Rest);
test_defined([]) -> false.
