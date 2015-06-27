-module(rebar_agent).
-export([start_link/1, do/1, do/2]).
-export([init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-include("rebar.hrl").

-record(state, {state,
                cwd,
                show_warning=true}).

start_link(State) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, State, []).

do(Command) when is_atom(Command) ->
    gen_server:call(?MODULE, {cmd, Command}, infinity).

do(Namespace, Command) when is_atom(Namespace), is_atom(Command) ->
    gen_server:call(?MODULE, {cmd, Namespace, Command}, infinity).

init(State) ->
    Cwd = rebar_dir:get_cwd(),
    {ok, #state{state=State, cwd=Cwd}}.

handle_call({cmd, Command}, _From, State=#state{state=RState, cwd=Cwd}) ->
    MidState = maybe_show_warning(State),
    {Res, NewRState} = run(default, Command, RState, Cwd),
    {reply, Res, MidState#state{state=NewRState}};
handle_call({cmd, Namespace, Command}, _From, State = #state{state=RState, cwd=Cwd}) ->
    MidState = maybe_show_warning(State),
    {Res, NewRState} = run(Namespace, Command, RState, Cwd),
    {reply, Res, MidState#state{state=NewRState}};
handle_call(_Call, _From, State) ->
    {noreply, State}.

handle_cast(_Cast, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

run(Namespace, Command, RState, Cwd) ->
    try
        case rebar_dir:get_cwd() of
            Cwd ->
                Args = [atom_to_list(Namespace), atom_to_list(Command)],
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

maybe_show_warning(S=#state{show_warning=true}) ->
    ?WARN("This feature is experimental and may be modified or removed at any time.", []),
    S#state{show_warning=false};
maybe_show_warning(State) ->
    State.

refresh_paths(RState) ->
    ToRefresh = (rebar_state:code_paths(RState, all_deps)
                 ++ [filename:join([rebar_app_info:out_dir(App), "test"])
                     || App <- rebar_state:project_apps(RState)]
                %% make sure to never reload self; halt()s the VM
                ) -- [filename:dirname(code:which(?MODULE))],
    %% Similar to rebar_utils:update_code/1, but also forces a reload
    %% of used modules.
    lists:foreach(fun(Path) ->
            Name = filename:basename(Path, "/ebin"),
            App = list_to_atom(Name),
            application:load(App),
            case application:get_key(App, modules) of
                undefined ->
                    code:add_patha(Path),
                    ok;
                {ok, Modules} ->
                    ?DEBUG("reloading ~p from ~s", [Modules, Path]),
                    code:replace_path(Name, Path),
                    [begin code:purge(M), code:delete(M), code:load_file(M) end
                    || M <- Modules]
            end
        end, ToRefresh).

refresh_state(RState, _Dir) ->
    lists:foldl(
        fun(F, State) -> F(State) end,
        rebar3:init_config(),
        [fun(S) -> rebar_state:current_profiles(S, rebar_state:current_profiles(RState)) end]
    ).
