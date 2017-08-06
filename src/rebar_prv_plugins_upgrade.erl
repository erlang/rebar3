-module(rebar_prv_plugins_upgrade).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-include("rebar.hrl").
-include_lib("providers/include/providers.hrl").

-define(PROVIDER, upgrade).
-define(NAMESPACE, plugins).
-define(DEPS, []).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    State1 = rebar_state:add_provider(
            State,
            providers:create([
                    {name, ?PROVIDER},
                    {module, ?MODULE},
                    {namespace, ?NAMESPACE},
                    {bare, true},
                    {deps, ?DEPS},
                    {example, "rebar3 plugins upgrade <plugin>"},
                    {short_desc, "Upgrade plugins"},
                    {desc, "List or upgrade plugins"},
                    {opts, [{plugin, undefined, undefined, string,
                             "Plugin to upgrade"}]}])),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    case proplists:get_value(plugin, Args, none) of
        none ->
            ?PRV_ERROR(no_plugin_arg);
        Plugin ->
            upgrade(Plugin, State)
    end.

-spec format_error(any()) -> iolist().
format_error(no_plugin_arg) ->
    io_lib:format("Must give an installed plugin to upgrade as an argument", []);
format_error({not_found, Plugin}) ->
    io_lib:format("Plugin to upgrade not found: ~ts", [Plugin]);
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

upgrade(Plugin, State) ->
    Profiles = rebar_state:current_profiles(State),
    case find_plugin(Plugin, Profiles, State) of
        not_found ->
            Dep = find_plugin(Plugin, [global], State);
        Dep ->
            Dep
    end,

    case Dep of
        not_found ->
            ?PRV_ERROR({not_found, Plugin});
        {ok, P, Profile} ->
            State1 = rebar_state:set(State, deps_dir, ?DEFAULT_PLUGINS_DIR),
            {Apps, _State2} = rebar_prv_install_deps:handle_deps_as_profile(Profile, State1, [P], true),

            {no_cycle, Sorted} = rebar_prv_install_deps:find_cycles(Apps),
            ToBuild = rebar_prv_install_deps:cull_compile(Sorted, []),

            %% Add already built plugin deps to the code path
            CodePaths = [rebar_app_info:ebin_dir(A) || A <- Apps -- ToBuild],
            code:add_pathsa(CodePaths),

            %% Build plugin and its deps
            [build_plugin(AppInfo, Apps, State) || AppInfo <- ToBuild],
            {ok, State}
    end.

find_plugin(Plugin, Profiles, State) ->
    ec_lists:search(fun(Profile) ->
                        Plugins = rebar_state:get(State, {plugins, Profile}, []) ++
                            rebar_state:get(State, {project_plugins, Profile}, []),
                        case rebar_utils:tup_find(list_to_atom(Plugin), Plugins) of
                            false ->
                                not_found;
                            P ->
                                {ok, P}
                        end
                    end, Profiles).

build_plugin(AppInfo, Apps, State) ->
    Providers = rebar_state:providers(State),
    AppDir = rebar_app_info:dir(AppInfo),
    C = rebar_config:consult(AppDir),
    S = rebar_state:new(rebar_state:all_deps(rebar_state:new(), Apps), C, AppDir),
    AppInfo1 = rebar_app_info:update_opts(AppInfo, rebar_app_info:opts(AppInfo), C),
    rebar_prv_compile:compile(S, Providers, AppInfo1).
