-module(rebar_agent).
-export([start_link/1, do/1, do/2]).
-export([init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-record(state, {state,
                cwd}).

start_link(State) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, State, []).

do(Command) when is_atom(Command) ->
    gen_server:call(?MODULE, {cmd, Command}, infinity).

do(Namespace, Command) when is_atom(Namespace), is_atom(Command) ->
    gen_server:call(?MODULE, {cmd, Namespace, Command}, infinity).

init(State0) ->
    Cwd = file:get_cwd(),
    State = rebar_state:update_code_paths(State0, default, code:get_path()),
    {ok, #state{state=State, cwd=Cwd}}.

handle_call({cmd, Command}, _From, State=#state{state=RState, cwd=Cwd}) ->
    Res = try
        case file:get_cwd() of
            Cwd ->
                case rebar_core:process_command(RState, Command) of
                    {ok, _} ->
                        refresh(RState),
                        ok;
                    {error, Err} when is_list(Err) ->
                        refresh(RState),
                        {error, lists:flatten(Err)};
                    {error, Err} ->
                        refresh(RState),
                        {error, Err}
                end;
            _ ->
                {error, cwd_changed}
        end
    catch
        Type:Reason ->
            {error, {Type, Reason}}
    end,
    {reply, Res, State};
handle_call({cmd, Namespace, Command}, From, State = #state{state=RState}) ->
    {reply, Res, _} = handle_call({cmd, Command}, From, State#state{
        state = rebar_state:namespace(RState, Namespace)
    }),
    {reply, Res, State};
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

refresh(RState) ->
    ToRefresh = rebar_state:code_paths(RState, all_deps)
                %% make sure to never reload self; halt()s the VM
                -- [filename:dirname(code:which(?MODULE))],
    rebar_utils:update_code(ToRefresh).
