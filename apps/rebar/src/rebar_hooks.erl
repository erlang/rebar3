-module(rebar_hooks).

-export([run_all_hooks/5
        ,run_all_hooks/6
        ,run_project_and_app_hooks/5
        ,format_error/1]).

-include("rebar.hrl").
-include_lib("providers/include/providers.hrl").

-spec run_all_hooks(file:filename_all(), pre | post,
                   atom() | {atom(), atom()} | string(),
                   [providers:t()], rebar_app_info:t(), rebar_state:t()) -> rebar_app_info:t().
run_all_hooks(Dir, Type, Command, Providers, AppInfo, State) ->
    ?DEBUG("Running hooks for ~p in app ~ts (~ts) with configuration:",
           [Command, rebar_app_info:name(AppInfo), Dir]),
    State1 = rebar_state:current_app(State, AppInfo),
    State2 = run_provider_hooks(Dir, Type, Command, Providers, rebar_app_info:opts(AppInfo), State1),
    run_hooks(Dir, Type, Command, rebar_app_info:opts(AppInfo), State1),
    rebar_state:current_app(State2).

run_all_hooks(Dir, Type, Command, Providers, State) ->
    ?DEBUG("Running hooks for ~p with configuration:", [Command]),
    run_provider_hooks(Dir, Type, Command, Providers, rebar_state:opts(State), State),
    run_hooks(Dir, Type, Command, rebar_state:opts(State), State).

run_project_and_app_hooks(Dir, Type, Command, Providers, State) ->
    ProjectApps = rebar_state:project_apps(State),
    ?DEBUG("Running app-specific hooks", []),
    [rebar_hooks:run_all_hooks(Dir, Type, Command, Providers, AppInfo, State) || AppInfo <- ProjectApps],
    ?DEBUG("Running project-wide hooks", []),
    run_all_hooks(Dir, Type, Command, Providers, State).

format_error({bad_provider, Type, Command, {Name, Namespace}}) ->
    io_lib:format("Unable to run ~ts hooks for '~p', command '~p' in namespace '~p' not found.", [Type, Command, Namespace, Name]);
format_error({bad_provider, Type, Command, Name}) ->
    io_lib:format("Unable to run ~ts hooks for '~p', command '~p' not found.", [Type, Command, Name]).

%%%%%%%%%%%%%%%
%%% PRIVATE %%%
%%%%%%%%%%%%%%%

run_provider_hooks(Dir, Type, Command, Providers, Opts, State) ->
    case rebar_opts:get(Opts, provider_hooks, []) of
        [] ->
            State;
        AllHooks ->
            TypeHooks = proplists:get_value(Type, AllHooks, []),
            run_provider_hooks_(Dir, Type, Command, Providers, TypeHooks, rebar_state:opts(State, Opts))
    end.

run_provider_hooks_(_Dir, _Type, _Command, _Providers, [], State) ->
    State;
run_provider_hooks_(Dir, Type, Command, Providers, TypeHooks, State) ->
    case proplists:get_all_values(Command, TypeHooks) of
        [] ->
            ?DEBUG("\t{provider_hooks, []}.", []),
            State;
        HookProviders ->
            rebar_paths:set_paths([plugins], State),
            Providers1 = rebar_state:providers(State),
            %% Drop CLI arguments from the command for the hook.
            State0 = rebar_state:command_parsed_args(
                       rebar_state:command_args(State, []),
                       {[], []}
            ),
            State1 = rebar_state:providers(rebar_state:dir(State0, Dir), Providers++Providers1),
            ?DEBUG("\t{provider_hooks, [{~p, ~p}]}.",
                   [Type, HookProviders]),
            case rebar_core:do(HookProviders, State1) of
                {error, ProviderName} ->
                    ?DEBUG(format_error({bad_provider, Type, Command, ProviderName}), []),
                    throw(?PRV_ERROR({bad_provider, Type, Command, ProviderName}));
                {ok, State2} ->
                    rebar_paths:set_paths([deps], State2),
                    State2
            end
    end.

run_hooks(Dir, pre, Command, Opts, State) ->
    run_hooks(Dir, pre_hooks, Command, Opts, State);
run_hooks(Dir, post, Command, Opts, State) ->
    run_hooks(Dir, post_hooks, Command, Opts, State);
run_hooks(Dir, Type, Command, Opts, State) ->
    case rebar_opts:get(Opts, Type, []) of
        [] ->
            ?DEBUG("\t{~p, []}.", [Type]),
            ?DIAGNOSTIC("run_hooks(~p, ~p, ~p) -> no hooks defined\n", [Dir, Type, Command]),
            ok;
        Hooks ->
            Env = rebar_env:create_env(State, Opts),
            CommandHooks = lists:filter(
                fun({_, C, _}) when C =:= Command -> true;
                   ({C, _}) when C =:= Command -> true;
                   (_) -> false
                end,
                Hooks
            ),
            ?DEBUG("\t{~p, ~p}.",
                   [Type, CommandHooks]),
            lists:foreach(fun(Hook) -> apply_hook(Dir, Env, Hook) end, CommandHooks)
    end.

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
