%%% @doc Runs a process that holds a rebar3 state and can be used
%%% to statefully maintain loaded project state into a running VM.
-module(rebar_agent).
-export([start_link/1, do/1, do/2, do/3, async_do/1, async_do/2, async_do/3]).
-export(['$handle_undefined_function'/2]).
-export([init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-include("rebar.hrl").

-record(state, {state,
                cwd,
                show_warning=true}).

%% @doc boots an agent server; requires a full rebar3 state already.
%% By default (within rebar3), this isn't called; `rebar_prv_shell'
%% enters and transforms into this module
-spec start_link(rebar_state:t()) -> {ok, pid()}.
start_link(State) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, State, []).

%% @doc runs a given command in the agent's context.
-spec do(atom() | string()) -> ok | {error, term()}.
do(Command) when is_atom(Command) ->
    gen_server:call(?MODULE, {cmd, Command}, infinity);
do(Args) when is_list(Args) ->
    gen_server:call(?MODULE, {cmd, default, do, Args}, infinity).

%% @doc runs a given command in the agent's context, under a given
%% namespace.
-spec do(atom(), atom() | string()) -> ok | {error, term()}.
do(Namespace, Command) when is_atom(Namespace), is_atom(Command) ->
    gen_server:call(?MODULE, {cmd, Namespace, Command}, infinity);
do(Namespace, Args) when is_atom(Namespace), is_list(Args) ->
    gen_server:call(?MODULE, {cmd, Namespace, do, Args}, infinity).

-spec do(atom(), atom(), string()) -> ok | {error, term()}.
do(Namespace, Command, Args) when is_atom(Namespace), is_atom(Command), is_list(Args) ->
    gen_server:call(?MODULE, {cmd, Namespace, Command, Args}, infinity).

-spec async_do(atom()) -> ok.
async_do(Command) when is_atom(Command) ->
    gen_server:cast(?MODULE, {cmd, Command});
async_do(Args) when is_list(Args) ->
    gen_server:cast(?MODULE, {cmd, default, do, Args}).

-spec async_do(atom(), atom()) -> ok.
async_do(Namespace, Command) when is_atom(Namespace), is_atom(Command) ->
    gen_server:cast(?MODULE, {cmd, Namespace, Command});
async_do(Namespace, Args) when is_atom(Namespace), is_list(Args) ->
    gen_server:cast(?MODULE, {cmd, Namespace, do, Args}).

-spec async_do(atom(), atom(), string()) -> ok.
async_do(Namespace, Command, Args) when is_atom(Namespace), is_atom(Command), is_list(Args) ->
    gen_server:cast(?MODULE, {cmd, Namespace, Command, Args}).

'$handle_undefined_function'(Cmd, [Namespace, Args]) ->
    gen_server:call(?MODULE, {cmd, Namespace, Cmd, Args}, infinity);
'$handle_undefined_function'(Cmd, [Args]) ->
    gen_server:call(?MODULE, {cmd, default, Cmd, Args}, infinity);
'$handle_undefined_function'(Cmd, []) ->
    gen_server:call(?MODULE, {cmd, default, Cmd}, infinity).

%%%%%%%%%%%%%%%%%
%%% CALLBACKS %%%
%%%%%%%%%%%%%%%%%

%% @private
init(State) ->
    Cwd = rebar_dir:get_cwd(),
    {ok, #state{state=State, cwd=Cwd}}.

%% @private
handle_call({cmd, Command}, _From, State=#state{state=RState, cwd=Cwd}) ->
    MidState = maybe_show_warning(State),
    put(cmd_type, sync),
    {Res, NewRState} = run(default, Command, "", RState, Cwd),
    put(cmd_type, undefined),
    {reply, Res, MidState#state{state=NewRState}, hibernate};
handle_call({cmd, Namespace, Command}, _From, State = #state{state=RState, cwd=Cwd}) ->
    MidState = maybe_show_warning(State),
    put(cmd_type, sync),
    {Res, NewRState} = run(Namespace, Command, "", RState, Cwd),
    put(cmd_type, undefined),
    {reply, Res, MidState#state{state=NewRState}, hibernate};
handle_call({cmd, Namespace, Command, Args}, _From, State = #state{state=RState, cwd=Cwd}) ->
    MidState = maybe_show_warning(State),
    put(cmd_type, sync),
    {Res, NewRState} = run(Namespace, Command, Args, RState, Cwd),
    put(cmd_type, undefined),
    {reply, Res, MidState#state{state=NewRState}, hibernate};
handle_call(_Call, _From, State) ->
    {noreply, State}.

%% @private
handle_cast({cmd, Command}, State=#state{state=RState, cwd=Cwd}) ->
    MidState = maybe_show_warning(State),
    put(cmd_type, async),
    {_, NewRState} = run(default, Command, "", RState, Cwd),
    put(cmd_type, undefined),
    {noreply, MidState#state{state=NewRState}, hibernate};
handle_cast({cmd, Namespace, Command}, State = #state{state=RState, cwd=Cwd}) ->
    MidState = maybe_show_warning(State),
    put(cmd_type, async),
    {_, NewRState} = run(Namespace, Command, "", RState, Cwd),
    put(cmd_type, undefined),
    {noreply, MidState#state{state=NewRState}, hibernate};
handle_cast({cmd, Namespace, Command, Args}, State = #state{state=RState, cwd=Cwd}) ->
    MidState = maybe_show_warning(State),
    put(cmd_type, async),
    {_, NewRState} = run(Namespace, Command, Args, RState, Cwd),
    put(cmd_type, undefined),
    {noreply, MidState#state{state=NewRState}, hibernate};
handle_cast(_Cast, State) ->
    {noreply, State}.

%% @private
handle_info(_Info, State) ->
    {noreply, State}.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%%%%%%%%%%%%%%%
%%% PRIVATE %%%
%%%%%%%%%%%%%%%

%% @private runs the actual command and maintains the state changes
-spec run(atom(), atom(), string(), rebar_state:t(), file:filename()) ->
    {ok, rebar_state:t()} | {{error, term()}, rebar_state:t()}.
run(Namespace, Command, StrArgs, RState, Cwd) ->
    try
        case rebar_dir:get_cwd() of
            Cwd ->
                PArgs = getopt:tokenize(StrArgs),
                Args = [atom_to_list(Namespace), atom_to_list(Command)] ++ PArgs,
                CmdState0 = refresh_state(RState, Cwd),
                CmdState1 = rebar_state:set(CmdState0, task, atom_to_list(Command)),
                CmdState = rebar_state:set(CmdState1, caller, api),
                case rebar3:run(CmdState, Args) of
                    {ok, TmpState} ->
                        refresh_paths(TmpState),
                        {ok, CmdState};
                    {error, Err} when is_list(Err) ->
                        refresh_paths(CmdState),
                        {{error, lists:flatten(Err)}, CmdState};
                    {error, Err} ->
                        refresh_paths(CmdState),
                        {{error, Err}, CmdState}
                end;
            _ ->
                {{error, cwd_changed}, RState}
        end
    catch
        ?WITH_STACKTRACE(Type, Reason, Stacktrace)
            ?DIAGNOSTIC("Agent Stacktrace: ~p", [Stacktrace]),
            {{error, {Type, Reason}}, RState}
    end.

%% @private function to display a warning for the feature only once
-spec maybe_show_warning(#state{}) -> #state{}.
maybe_show_warning(S=#state{show_warning=true}) ->
    ?WARN("This feature is experimental and may be modified or removed at any time.", []),
    S#state{show_warning=false};
maybe_show_warning(State) ->
    State.

%% @private based on a rebar3 state term, reload paths in a way
%% that makes sense.
-spec refresh_paths(rebar_state:t()) -> ok.
refresh_paths(RState) ->
    RefreshPaths = application:get_env(rebar, refresh_paths, [all_deps, test]),
    ToRefresh = parse_refresh_paths(RefreshPaths, RState, []),
    %% Modules from apps we can't reload without breaking functionality
    ShellOpts = rebar_state:get(RState, shell, []),
    ShellBlacklist = proplists:get_value(app_reload_blacklist, ShellOpts, []),
    Blacklist = lists:usort(
        application:get_env(rebar, refresh_paths_blacklist, ShellBlacklist)
        ++ [rebar, erlware_commons, providers, cf, cth_readable]),
    %% Similar to rebar_utils:update_code/1, but also forces a reload
    %% of used modules. Also forces to reload all of ebin/ instead
    %% of just the modules in the .app file, because 'extra_src_dirs'
    %% allows to load and compile files that are not to be kept
    %% in the app file.
    [refresh_path(Path, Blacklist) || Path <- ToRefresh],
    ok.

refresh_path(Path, Blacklist) ->
    Name = filename:basename(Path, "/ebin"),
    App = list_to_atom(Name),
    case App of
        test -> % skip
            code:add_patha(Path),
            ok;
        _ ->
            application:load(App),
            case application:get_key(App, modules) of
                undefined ->
                    code:add_patha(Path);
                {ok, _Mods} ->
                    case lists:member(App, Blacklist) of
                        false ->
                            refresh_path_do(Path, App);
                        true ->
                            refresh_path_blacklisted(Path)
                    end
            end
    end.

refresh_path_do(Path, App) ->
    Modules = mods_in_path(Path),
    ?DIAGNOSTIC("reloading ~p from ~ts", [Modules, Path]),
    code:replace_path(App, Path),
    reload_modules(Modules).

%% @private blacklisted paths are not reloaded, but if they were not loaded
%% already, we try and ensure they are loaded once. This is a soft operation
%% that does not provoke crashes in existing processes, but hides issues
%% as seen in issue #2013 comments where some loaded modules that are currently
%% run by no processes get unloaded by rebar_paths, without being loaded back in.
refresh_path_blacklisted(Path) ->
    Modules = [M || M <- mods_in_path(Path), not is_loaded(M)],
    ?DIAGNOSTIC("ensure ~p loaded", [Modules]),
    code:add_pathz(Path), % in case the module is only in a new non-clashing path
     _ = [code:ensure_loaded(M) || M <- Modules],
    ok.

%% @private fetch module names from a given directory that contains
%% pre-build beam files.
mods_in_path(Path) ->
    Files = filelib:wildcard(filename:join([Path, "*.beam"])),
    [list_to_atom(filename:basename(F, ".beam")) || F <- Files].

%% @private check that a module is already loaded
is_loaded(Mod) ->
    code:is_loaded(Mod) =/= false.

%% @private parse refresh_paths option
%% no_deps means only project_apps's ebin path
%% no_test means no test path
%% OtherPath.
parse_refresh_paths([all_deps | RefreshPaths], RState, Acc) ->
    Paths = rebar_state:code_paths(RState, all_deps),
    parse_refresh_paths(RefreshPaths, RState, Paths ++ Acc);
parse_refresh_paths([project_apps | RefreshPaths], RState, Acc) ->
    Paths = [filename:join([rebar_app_info:out_dir(App), "ebin"])
        || App <- rebar_state:project_apps(RState)],
    parse_refresh_paths(RefreshPaths, RState, Paths ++ Acc);
parse_refresh_paths([test | RefreshPaths], RState, Acc) ->
    Paths = [filename:join([rebar_app_info:out_dir(App), "test"])
        || App <- rebar_state:project_apps(RState)],
    parse_refresh_paths(RefreshPaths, RState, Paths ++ Acc);
parse_refresh_paths([RefreshPath0 | RefreshPaths], RState, Acc) when is_list(RefreshPath0) ->
    case filelib:is_dir(RefreshPath0) of
        true ->
            RefreshPath0 =
            case filename:basename(RefreshPath0) of
                "ebin" -> RefreshPath0;
                _ -> filename:join([RefreshPath0, "ebin"])
            end,
            parse_refresh_paths(RefreshPaths, RState, [RefreshPath0 | Acc]);
        false ->
            parse_refresh_paths(RefreshPaths, RState, Acc)
    end;
parse_refresh_paths([_ | RefreshPaths], RState, Acc) ->
    parse_refresh_paths(RefreshPaths, RState, Acc);
parse_refresh_paths([], _RState, Acc) ->
    lists:usort(Acc).

%% @private from a disk config, reload and reapply with the current
%% profiles; used to find changes in the config from a prior run.
-spec refresh_state(rebar_state:t(), file:filename()) -> rebar_state:t().
refresh_state(RState, _Dir) ->
    lists:foldl(
        fun(F, State) -> F(State) end,
        rebar3:init_config(),
        [fun(S) -> rebar_state:apply_profiles(S, rebar_state:current_profiles(RState)) end]
    ).

%% @private takes a list of modules and reloads them
-spec reload_modules([module()]) -> term().
reload_modules([]) -> noop;
reload_modules(Modules0) ->
    Modules = [M || M <- Modules0, is_changed(M)],
    reload_modules(Modules, erlang:function_exported(code, prepare_loading, 1)).

%% @spec is_changed(atom()) -> boolean()
%% @doc true if the loaded module is a beam with a vsn attribute
%%      and does not match the on-disk beam file, returns false otherwise.
is_changed(M) ->
    try
        module_vsn(M:module_info(attributes)) =/= module_vsn(code:get_object_code(M))
    catch _:_ ->
        false
    end.

module_vsn({M, Beam, _Fn}) ->
    % Because the vsn can set by -vsn(X) in module.
    % So didn't use beam_lib:version/1 to get the vsn.
    % So if set -vsn(X) in module, it will always reload the module.
    {ok, {M, <<Vsn:128>>}} = beam_lib:md5(Beam),
    Vsn;
module_vsn(Attrs) when is_list(Attrs) ->
    {_, Vsn} = lists:keyfind(vsn, 1, Attrs),
    Vsn.

%% @private reloading modules, when there are modules to actually reload
reload_modules(Modules, true) ->
    %% OTP 19 and later -- use atomic loading and ignore unloadable mods
    case code:prepare_loading(Modules) of
        {ok, Prepared} ->
            [code:purge(M) || M <- Modules],
            code:finish_loading(Prepared);
        {error, ModRsns} ->
            Blacklist =
                lists:foldr(fun({ModError, Error}, Acc) ->
                    case Error of
                        % perhaps cover other cases of failure?
                        on_load_not_allowed ->
                            reload_modules([ModError], false),
                            [ModError|Acc];
                        _ ->
                            ?DIAGNOSTIC("Module ~p failed to atomic load because ~p", [ModError, Error]),
                            [ModError|Acc]
                    end
                end,
                [], ModRsns
            ),
            reload_modules(Modules -- Blacklist, true)
    end;
reload_modules(Modules, false) ->
    %% Older versions, use a more ad-hoc mechanism.
    lists:foreach(fun(M) ->
            code:delete(M),
            code:purge(M),
            case code:load_file(M) of
                {module, M} -> ok;
                {error, Error} ->
                    ?DIAGNOSTIC("Module ~p failed to load because ~p", [M, Error])
            end
        end, Modules
    ).
