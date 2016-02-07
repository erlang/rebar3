-module(rebar_hooks).

-export([run_all_hooks/5
        ,run_all_hooks/6
        ,format_error/1]).

-include("rebar.hrl").
-include_lib("providers/include/providers.hrl").

-spec run_all_hooks(file:filename_all(), pre | post,
                   atom() | {atom(), atom()} | string(),
                   [providers:t()], rebar_app_info:t(), rebar_state:t()) -> rebar_app_info:t().
run_all_hooks(Dir, Type, Command, Providers, AppInfo, State) ->
    State1 = rebar_state:current_app(State, AppInfo),
    State2 = run_provider_hooks(Dir, Type, Command, Providers, rebar_app_info:opts(AppInfo), State1),
    run_hooks(Dir, Type, Command, rebar_app_info:opts(AppInfo), State1),
    rebar_state:current_app(State2).

run_all_hooks(Dir, Type, Command, Providers, State) ->
    run_provider_hooks(Dir, Type, Command, Providers, rebar_state:opts(State), State),
    run_hooks(Dir, Type, Command, rebar_state:opts(State), State).

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
    PluginDepsPaths = rebar_state:code_paths(State, all_plugin_deps),
    code:add_pathsa(PluginDepsPaths),
    Providers1 = rebar_state:providers(State),
    State1 = rebar_state:providers(rebar_state:dir(State, Dir), Providers++Providers1),
    HookProviders = proplists:get_all_values(Command, TypeHooks),
    case rebar_core:do(HookProviders, State1) of
        {error, ProviderName} ->
            ?DEBUG(format_error({bad_provider, Type, Command, ProviderName}), []),
            throw(?PRV_ERROR({bad_provider, Type, Command, ProviderName}));
        {ok, State2} ->
            rebar_utils:remove_from_code_path(PluginDepsPaths),
            State2
    end.

format_error({bad_provider, Type, Command, {Name, Namespace}}) ->
    io_lib:format("Unable to run ~s hooks for '~p', command '~p' in namespace '~p' not found.", [Type, Command, Namespace, Name]);
format_error({bad_provider, Type, Command, Name}) ->
    io_lib:format("Unable to run ~s hooks for '~p', command '~p' not found.", [Type, Command, Name]).

%% @doc The following environment variables are exported when running
%% a hook (absolute paths):
%%
%% REBAR_DEPS_DIR          = rebar_dir:deps_dir/1
%% REBAR_BUILD_DIR         = rebar_dir:base_dir/1
%% REBAR_ROOT_DIR          = rebar_dir:root_dir/1
%% REBAR_CHECKOUTS_DIR     = rebar_dir:checkouts_dir/1
%% REBAR_PLUGINS_DIR       = rebar_dir:plugins_dir/1
%% REBAR_GLOBAL_CONFIG_DIR = rebar_dir:global_config_dir/1
%% REBAR_GLOBAL_CACHE_DIR  = rebar_dir:global_cache_dir/1
%% REBAR_TEMPLATE_DIR      = rebar_dir:template_dir/1
%% REBAR_APP_DIRS          = rebar_dir:lib_dirs/1
%% REBAR_SRC_DIRS          = rebar_dir:src_dirs/1
%%
%% autoconf compatible variables
%% (see: http://www.gnu.org/software/autoconf/manual/autoconf.html#Erlang-Libraries):
%% ERLANG_ERTS_VER              = erlang:system_info(version)
%% ERLANG_ROOT_DIR              = code:root_dir/0
%% ERLANG_LIB_DIR_erl_interface = code:lib_dir(erl_interface)
%% ERLANG_LIB_VER_erl_interface = version part of path returned by code:lib_dir(erl_interface)
%% ERL                          = ERLANG_ROOT_DIR/bin/erl
%% ERLC                         = ERLANG_ROOT_DIR/bin/erl
%%
run_hooks(Dir, pre, Command, Opts, State) ->
    run_hooks(Dir, pre_hooks, Command, Opts, State);
run_hooks(Dir, post, Command, Opts, State) ->
    run_hooks(Dir, post_hooks, Command, Opts, State);
run_hooks(Dir, Type, Command, Opts, State) ->
    case rebar_opts:get(Opts, Type, []) of
        [] ->
            ok;
        Hooks ->
            Env = create_env(State, Opts),
            lists:foreach(fun({_, C, _}=Hook) when C =:= Command ->
                                  apply_hook(State, Dir, Env, Hook);
                             ({C, _}=Hook) when C =:= Command ->
                                  apply_hook(State, Dir, Env, Hook);
                             (_) ->
                                  continue
                          end, Hooks)
    end.

apply_hook(State, Dir, Env, {Arch, Command, Hook}) ->
    case rebar_utils:is_arch(Arch) of
        true ->
            apply_hook(State, Dir, Env, {Command, Hook});
        false ->
            ok
    end;
apply_hook(State, _Dir, _Env, {_Command, {Module, Function}}) ->
    Module:Function(State);
apply_hook(_State, Dir, Env, {Command, Hook}) ->
    Msg = lists:flatten(io_lib:format("Hook for ~p failed!~n", [Command])),
    rebar_utils:sh(Hook, [use_stdout, {cd, Dir}, {env, Env}, {abort_on_error, Msg}]).

create_env(State, Opts) ->
    BaseDir = rebar_dir:base_dir(State),
    [
     {"REBAR_DEPS_DIR",          filename:absname(rebar_dir:deps_dir(State))},
     {"REBAR_BUILD_DIR",         filename:absname(rebar_dir:base_dir(State))},
     {"REBAR_ROOT_DIR",          filename:absname(rebar_dir:root_dir(State))},
     {"REBAR_CHECKOUTS_DIR",     filename:absname(rebar_dir:checkouts_dir(State))},
     {"REBAR_PLUGINS_DIR",       filename:absname(rebar_dir:plugins_dir(State))},
     {"REBAR_GLOBAL_CONFIG_DIR", filename:absname(rebar_dir:global_config_dir(State))},
     {"REBAR_GLOBAL_CACHE_DIR",  filename:absname(rebar_dir:global_cache_dir(Opts))},
     {"REBAR_TEMPLATE_DIR",      filename:absname(rebar_dir:template_dir(State))},
     {"REBAR_APP_DIRS",          join_dirs(BaseDir, rebar_dir:lib_dirs(State))},
     {"REBAR_SRC_DIRS",          join_dirs(BaseDir, rebar_dir:all_src_dirs(Opts))},
     {"ERLANG_ERTS_VER",         erlang:system_info(version)},
     {"ERLANG_ROOT_DIR",         code:root_dir()},
     {"ERLANG_LIB_DIR_erl_interface", code:lib_dir(erl_interface)},
     {"ERLANG_LIB_VER_erl_interface", re_version(code:lib_dir(erl_interface))},
     {"ERL",                     filename:join([code:root_dir(), "bin", "erl"])},
     {"ERLC",                    filename:join([code:root_dir(), "bin", "erlc"])},
     {"ERLANG_ARCH"  ,           rebar_api:wordsize()},
     {"ERLANG_TARGET",           rebar_api:get_arch()}

    ].

join_dirs(BaseDir, Dirs) ->
    string:join([ filename:join(BaseDir, Dir) || Dir <- Dirs ], ":").

re_version(Path) ->
    case re:run(Path, "^.*-(?<VER>[^/-]*)$", [{capture, [1], list}]) of
        nomatch -> "";
        {match, [Ver]} -> Ver
    end.
