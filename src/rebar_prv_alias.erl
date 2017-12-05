%%% @doc Meta-provider that dynamically compiles providers
%%% to run aliased commands.
%%%
%%% This is hackish and out-there, but this module has graduated
%%% from a plugin at https://github.com/tsloughter/rebar_alias after
%%% years of stability. Only some error checks were added
-module(rebar_prv_alias).

-export([init/1]).
-include("rebar.hrl").

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Aliases = rebar_state:get(State, alias, []),
    lists:foldl(fun({Alias, Cmds}, {ok, StateAcc}) ->
                    case validate_provider(Alias, Cmds, State) of
                        true -> init_alias(Alias, Cmds, StateAcc);
                        false -> {ok, State}
                    end
                end, {ok, State}, Aliases).

init_alias(Alias, Cmds, State) ->
    Module = list_to_atom("rebar_prv_alias_" ++ atom_to_list(Alias)),

    MF = module(Module),
    EF = exports(),
    FF = do_func(Cmds),

    {ok, _, Bin} = compile:forms([MF, EF, FF]),
    code:load_binary(Module, "none", Bin),

    Provider = providers:create([
            {name, Alias},
            {module, Module},
            {bare, true},
            {deps, []},
            {example, example(Alias)},
            {opts, []},
            {short_desc, desc(Cmds)},
            {desc, desc(Cmds)}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

validate_provider(Alias, Cmds, State) ->
    %% This would be caught and prevented anyway, but the warning
    %% is friendlier
    case providers:get_provider(Alias, rebar_state:providers(State)) of
        not_found ->
            %% check for circular deps in the alias.
            case not proplists:is_defined(Alias, Cmds) of
                true -> true;
                false ->
                    ?WARN("Alias ~p contains itself and would never "
                          "terminate. It will be ignored.",
                          [Alias]),
                    false
            end;
        _ ->
            ?WARN("Alias ~p is already the name of a command in "
                  "the default namespace and will be ignored.",
                  [Alias]),
            false
    end.


example(Alias) ->
    "rebar3 " ++ atom_to_list(Alias).

desc(Cmds) ->
    "Equivalent to running: rebar3 do " ++
    rebar_string:join(lists:map(fun({Cmd, Args}) ->
                                        atom_to_list(Cmd) ++ " " ++ Args;
                                   (Cmd) ->
                                        atom_to_list(Cmd)
                                end, Cmds), ",").

module(Name) ->
    {attribute,1,module,Name}.

exports() ->
    {attribute,1,export,[{do,1}]}.

do_func(Cmds) ->
    {function,1,do,1,
     [{clause,1,
       [{var,1,'State'}],
       [],
       [{call,1,
         {remote,1,{atom,1,rebar_prv_do},{atom,1,do_tasks}},
         [to_args(Cmds),{var,1,'State'}]}]}]}.


to_args([]) ->
    {nil,1};
to_args([{Cmd, Args} | Rest]) ->
    {cons,1,{tuple,1,[{string,1,atom_to_list(Cmd)},{string,1,Args}]}, to_args(Rest)};
to_args([Cmd | Rest]) ->
    {cons,1,{tuple,1,[{string,1,atom_to_list(Cmd)},{nil,1}]}, to_args(Rest)}.

