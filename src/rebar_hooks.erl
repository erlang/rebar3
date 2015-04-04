-module(rebar_hooks).

-export([run_all_hooks/5]).

-spec run_all_hooks(file:filename_all(), pre | post,
                   atom() | {atom(), atom()} | string(),
                   [providers:t()], rebar_state:t()) -> ok.
run_all_hooks(Dir, Type, Command, Providers, State) ->
    run_provider_hooks(Dir, Type, Command, Providers, State),
    run_hooks(Dir, Type, Command, State).

run_provider_hooks(Dir, Type, Command, Providers, State) ->
    State1 = rebar_state:providers(rebar_state:dir(State, Dir), Providers),
    AllHooks = rebar_state:get(State1, provider_hooks, []),
    TypeHooks = proplists:get_value(Type, AllHooks, []),
    HookProviders = proplists:get_all_values(Command, TypeHooks),
    rebar_core:do(HookProviders, State1).

run_hooks(Dir, Type, Command, State) ->
    Hooks = case Type of
                pre ->
                    rebar_state:get(State, pre_hooks, []);
                post ->
                    rebar_state:get(State, post_hooks, []);
                _ ->
                    []
            end,
    Env = [{"REBAR_DEPS_DIR", filename:absname(rebar_dir:deps_dir(State))}],
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
    rebar_utils:sh(Hook, [use_stdout, {cd, Dir}, {env, Env}, {abort_on_error, Msg}]).
