-module(rebar_hooks).

-export([run_compile_hooks/4]).

run_compile_hooks(Dir, Type, Command, State) ->
    Hooks = rebar_state:get(State, Type, []),
    Env = [{"REBAR_DEPS_DIR", rebar_utils:deps_dir(State)}],
    lists:foreach(fun({_, C, _}=Hook) when C =:= Command ->
                          apply_hook(Dir, Env, Hook);
                     ({C, _}=Hook) when C =:= Command ->
                          apply_hook(Dir, Env, Hook);
                     (_) ->
                          continue
                  end, Hooks).

apply_hook(Dir, Env, {Arch, Command, Hook}) ->
    case rebar_utils:is_arch(Arch) of
        true ->
            apply_hook(Dir, Env, {Command, Hook});
        false ->
            ok
    end;
apply_hook(Dir, Env, {Command, Hook}) ->
    Msg = lists:flatten(io_lib:format("Hook for ~p failed!~n", [Command])),
    rebar_utils:sh(Hook, [{cd, Dir}, {env, Env}, {abort_on_error, Msg}]).
