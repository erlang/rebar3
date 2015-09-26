%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2011 Trifork
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

-module(rebar_prv_shell).
-author("Kresten Krab Thorup <krab@trifork.com>").

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-include("rebar.hrl").

-define(PROVIDER, shell).
-define(DEPS, [compile]).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    State1 = rebar_state:add_provider(
            State,
            providers:create([
                {name, ?PROVIDER},
                {module, ?MODULE},
                {bare, true},
                {deps, ?DEPS},
                {example, "rebar3 shell"},
                {short_desc, "Run shell with project apps and deps in path."},
                {desc, info()},
                {opts, [{config, undefined, "config", string,
                         "Path to the config file to use. Defaults to the "
                         "sys_config defined for relx, if present."},
                        {name, undefined, "name", atom,
                         "Gives a long name to the node."},
                        {sname, undefined, "sname", atom,
                         "Gives a short name to the node."}]}
            ])
    ),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(Config) ->
    shell(Config),
    {ok, Config}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% NOTE:
%% this is an attempt to replicate `erl -pa ./ebin -pa deps/*/ebin`. it is
%% mostly successful but does stop and then restart the user io system to get
%% around issues with rebar being an escript and starting in `noshell` mode.
%% it also lacks the ctrl-c interrupt handler that `erl` features. ctrl-c will
%% immediately kill the script. ctrl-g, however, works fine

shell(State) ->
    setup_name(State),
    setup_paths(State),
    setup_shell(),
    %% apps must be started after the change in shell because otherwise
    %% their application masters never gets the new group leader (held in
    %% their internal state)
    maybe_boot_apps(State),
    simulate_proc_lib(),
    true = register(rebar_agent, self()),
    {ok, GenState} = rebar_agent:init(State),
    gen_server:enter_loop(rebar_agent, [], GenState, {local, rebar_agent}, hibernate).

info() ->
    "Start a shell with project and deps preloaded similar to~n'erl -pa ebin -pa deps/*/ebin'.~n".

setup_shell() ->
    %% scan all processes for any with references to the old user and save them to
    %% update later
    NeedsUpdate = [Pid || Pid <- erlang:processes(),
        proplists:get_value(group_leader, erlang:process_info(Pid)) == whereis(user)
    ],
    %% terminate the current user
    ok = supervisor:terminate_child(kernel_sup, user),
    %% start a new shell (this also starts a new user under the correct group)
    _ = user_drv:start(),
    %% wait until user_drv and user have been registered (max 3 seconds)
    ok = wait_until_user_started(3000),
    %% set any process that had a reference to the old user's group leader to the
    %% new user process. Catch the race condition when the Pid exited after the
    %% liveness check.
    _ = [catch erlang:group_leader(whereis(user), Pid) || Pid <- NeedsUpdate,
                                                          is_process_alive(Pid)],
    try
        %% enable error_logger's tty output
        error_logger:swap_handler(tty),
        %% disable the simple error_logger (which may have been added multiple
        %% times). removes at most the error_logger added by init and the
        %% error_logger added by the tty handler
        remove_error_handler(3)
    catch
        E:R -> % may fail with custom loggers
            ?DEBUG("Logger changes failed for ~p:~p (~p)", [E,R,erlang:get_stacktrace()]),
            hope_for_best
    end.

setup_paths(State) ->
    %% Add deps to path
    code:add_pathsa(rebar_state:code_paths(State, all_deps)),
    %% add project app test paths
    ok = add_test_paths(State).

maybe_boot_apps(State) ->
    case find_apps_to_boot(State) of
        undefined ->
            %% try to read in sys.config file
            ok = reread_config(State);
        Apps ->
            %% load apps, then check config, then boot them.
            load_apps(Apps),
            ok = reread_config(State),
            boot_apps(Apps)
    end.

simulate_proc_lib() ->
    FakeParent = spawn_link(fun() -> timer:sleep(infinity) end),
    put('$ancestors', [FakeParent]),
    put('$initial_call', {rebar_agent, init, 1}).

setup_name(State) ->
    {Opts, _} = rebar_state:command_parsed_args(State),
    case {proplists:get_value(name, Opts), proplists:get_value(sname, Opts)} of
        {undefined, undefined} ->
            ok;
        {Name, undefined} ->
            check_epmd(net_kernel:start([Name, longnames]));
        {undefined, SName} ->
            check_epmd(net_kernel:start([SName, shortnames]));
        {_, _} ->
            ?ABORT("Cannot have both short and long node names defined", [])
    end.

check_epmd({error,{{shutdown, {_,net_kernel,{'EXIT',nodistribution}}},_}}) ->
    ?ERROR("Erlang Distribution failed, falling back to nonode@nohost. "
           "Verify that epmd is running and try again.",[]);
check_epmd(_) ->
    ok.

find_apps_to_boot(State) ->
    %% Try the shell_apps option
    case rebar_state:get(State, shell_apps, undefined) of
        undefined ->
            %% Get to the relx tuple instead
            case lists:keyfind(release, 1, rebar_state:get(State, relx, [])) of
                {_, _, Apps} -> Apps;
                false -> undefined
            end;
        Apps ->
            Apps
    end.

load_apps(Apps) ->
    [case application:load(App) of
        ok ->
             {ok, Ks} = application:get_all_key(App),
             load_apps(proplists:get_value(applications, Ks));
        _ ->
            error % will be caught when starting the app
     end || App <- normalize_load_apps(Apps),
            not lists:keymember(App, 1, application:loaded_applications())],
    ok.

reread_config(State) ->
    case find_config(State) of
        no_config ->
            ok;
        ConfigList ->
            _ = [application:set_env(Application, Key, Val)
                  || {Application, Items} <- ConfigList,
                     {Key, Val} <- Items],
            ok
    end.

boot_apps(Apps) ->
    ?WARN("The rebar3 shell is a development tool; to deploy "
            "applications in production, consider using releases "
            "(http://www.rebar3.org/v3.0/docs/releases)", []),
    Normalized = normalize_boot_apps(Apps),
    Res = [application:ensure_all_started(App) || App <- Normalized],
    _ = [?INFO("Booted ~p", [App])
            || {ok, Booted} <- Res,
            App <- Booted],
    _ = [?ERROR("Failed to boot ~p for reason ~p", [App, Reason])
            || {error, {App, Reason}} <- Res],
    ok.

normalize_load_apps([]) -> [];
normalize_load_apps([{App, _} | T]) -> [App | normalize_load_apps(T)];
normalize_load_apps([{App, _Vsn, load} | T]) -> [App | normalize_load_apps(T)];
normalize_load_apps([App | T]) when is_atom(App) -> [App | normalize_load_apps(T)].

normalize_boot_apps([]) -> [];
normalize_boot_apps([{_App, load} | T]) -> normalize_boot_apps(T);
normalize_boot_apps([{_App, _Vsn, load} | T]) -> normalize_boot_apps(T);
normalize_boot_apps([{App, _Vsn} | T]) -> [App | normalize_boot_apps(T)];
normalize_boot_apps([App | T]) when is_atom(App) -> [App | normalize_boot_apps(T)].


remove_error_handler(0) ->
    ?WARN("Unable to remove simple error_logger handler", []);
remove_error_handler(N) ->
    case gen_event:delete_handler(error_logger, error_logger, []) of
        {error, module_not_found} -> ok;
        {error_logger, _} -> remove_error_handler(N-1)
    end.

%% Timeout is a period to wait before giving up
wait_until_user_started(0) ->
    ?ABORT("Timeout exceeded waiting for `user` to register itself", []),
    erlang:error(timeout);
wait_until_user_started(Timeout) ->
    case whereis(user) of
        %% if user is not yet registered wait a tenth of a second and try again
        undefined -> timer:sleep(100), wait_until_user_started(Timeout - 100);
        _ -> ok
    end.

add_test_paths(State) ->
    _ = [begin
            AppDir = rebar_app_info:out_dir(App),
            %% ignore errors resulting from non-existent directories
            _ = code:add_path(filename:join([AppDir, "test"]))
         end || App <- rebar_state:project_apps(State)],
    _ = code:add_path(filename:join([rebar_dir:base_dir(State), "test"])),
    ok.

% First try the --config flag, then try the relx sys_config
-spec find_config(rebar_state:t()) -> [tuple()] | no_config.
find_config(State) ->
    case find_config_option(State) of
        no_config ->
            find_config_relx(State);
        Result ->
            Result
    end.

-spec find_config_option(rebar_state:t()) -> [tuple()] | no_config.
find_config_option(State) ->
    {Opts, _} = rebar_state:command_parsed_args(State),
    case proplists:get_value(config, Opts) of
        undefined ->
            no_config;
        Filename ->
            consult_config(State, Filename)
    end.

-spec find_config_relx(rebar_state:t()) -> [tuple()] | no_config.
find_config_relx(State) ->
    case proplists:get_value(sys_config, rebar_state:get(State, relx, [])) of
        undefined ->
            no_config;
        Filename ->
            consult_config(State, Filename)
    end.

-spec consult_config(rebar_state:t(), string()) -> [tuple()].
consult_config(State, Filename) ->
    Fullpath = filename:join(rebar_dir:root_dir(State), Filename),
    ?DEBUG("Loading configuration from ~p", [Fullpath]),
    case rebar_file_utils:try_consult(Fullpath) of
        [T] -> T;
        [] -> []
    end.
