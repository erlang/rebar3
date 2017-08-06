%%% @doc Runs a process that holds a rebar3 state and can be used
%%% to statefully maintain loaded project state into a running VM.
-module(rebar_agent).
-export([start_link/1, do/1, do/2]).
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
-spec do(atom()) -> ok | {error, term()}.
do(Command) when is_atom(Command) ->
    gen_server:call(?MODULE, {cmd, Command}, infinity);
do(Args) when is_list(Args) ->
    gen_server:call(?MODULE, {cmd, default, do, Args}, infinity).

%% @doc runs a given command in the agent's context, under a given
%% namespace.
-spec do(atom(), atom()) -> ok | {error, term()}.
do(Namespace, Command) when is_atom(Namespace), is_atom(Command) ->
    gen_server:call(?MODULE, {cmd, Namespace, Command}, infinity);
do(Namespace, Args) when is_atom(Namespace), is_list(Args) ->
    gen_server:call(?MODULE, {cmd, Namespace, do, Args}, infinity).

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
    {Res, NewRState} = run(default, Command, "", RState, Cwd),
    {reply, Res, MidState#state{state=NewRState}, hibernate};
handle_call({cmd, Namespace, Command}, _From, State = #state{state=RState, cwd=Cwd}) ->
    MidState = maybe_show_warning(State),
    {Res, NewRState} = run(Namespace, Command, "", RState, Cwd),
    {reply, Res, MidState#state{state=NewRState}, hibernate};
handle_call({cmd, Namespace, Command, Args}, _From, State = #state{state=RState, cwd=Cwd}) ->
    MidState = maybe_show_warning(State),
    {Res, NewRState} = run(Namespace, Command, Args, RState, Cwd),
    {reply, Res, MidState#state{state=NewRState}, hibernate};
handle_call(_Call, _From, State) ->
    {noreply, State}.

%% @private
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
        Type:Reason ->
            ?DEBUG("Agent Stacktrace: ~p", [erlang:get_stacktrace()]),
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
    ToRefresh = (rebar_state:code_paths(RState, all_deps)
                 ++ [filename:join([rebar_app_info:out_dir(App), "test"])
                     || App <- rebar_state:project_apps(RState)]
                %% make sure to never reload self; halt()s the VM
                ) -- [filename:dirname(code:which(?MODULE))],
    %% Modules from apps we can't reload without breaking functionality
    Blacklist = [ec_cmd_log, providers, cf, cth_readable],
    %% Similar to rebar_utils:update_code/1, but also forces a reload
    %% of used modules. Also forces to reload all of ebin/ instead
    %% of just the modules in the .app file, because 'extra_src_dirs'
    %% allows to load and compile files that are not to be kept
    %% in the app file.
    lists:foreach(fun(Path) ->
            Name = filename:basename(Path, "/ebin"),
            Files = filelib:wildcard(filename:join([Path, "*.beam"])),
            Modules = [list_to_atom(filename:basename(F, ".beam"))
                       || F <- Files],
            App = list_to_atom(Name),
            application:load(App),
            case application:get_key(App, modules) of
                undefined ->
                    code:add_patha(Path),
                    ok;
                {ok, Mods} ->
                    case {length(Mods), length(Mods -- Blacklist)} of
                        {X,X} ->
                            ?DEBUG("reloading ~p from ~ts", [Modules, Path]),
                            code:replace_path(App, Path),
                            reload_modules(Modules);
                        {_,_} ->
                            ?DEBUG("skipping app ~p, stable copy required", [App])
                    end
            end
        end, ToRefresh).

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
reload_modules(Modules) ->
        reload_modules(Modules, erlang:function_exported(code, prepare_loading, 1)).

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
                            ?DEBUG("Module ~p failed to atomic load because ~p", [ModError, Error]),
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
                    ?DEBUG("Module ~p failed to load because ~p", [M, Error])
            end
        end, Modules
    ).
