-module(rebar_prv_app_builder).

-behaviour(rebar_provider).

-export([init/1,
         do/1]).

-include("rebar.hrl").

-define(PROVIDER, app_builder).
-define(DEPS, [deps]).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_config:config()) -> {ok, rebar_config:config()}.
init(State) ->
    State1 = rebar_config:add_provider(State, #provider{name = ?PROVIDER,
                                                        provider_impl = ?MODULE,
                                                        provides = build,
                                                        bare = false,
                                                        deps = ?DEPS,
                                                        example = "rebar build",
                                                        short_desc = "",
                                                        desc = "",
                                                        opts = []}),
    {ok, State1}.

-spec do(rebar_config:config()) -> {ok, rebar_config:config()} | relx:error().
do(Config) ->
    Deps = rebar_config:deps_to_build(Config),
    Apps = rebar_config:apps_to_build(Config),
    Config1 =
        lists:foldl(fun(AppInfo, ConfigAcc) ->
                            ?INFO("Building ~p version ~p~n", [rebar_app_info:name(AppInfo)
                                                              ,rebar_app_info:original_vsn(AppInfo)]),
                            {_AppInfo1, ConfigAcc1} = build(ConfigAcc, AppInfo),
                            ConfigAcc
                    end, Config, Deps++Apps),
    Graph = construct_graph(Config),
    Goals = rebar_config:goals(Config1),
    {ok, Solve} = rlx_depsolver:solve(Graph, Goals),

    DepsDir = get_deps_dir(Config1),
    LockDeps = lists:map(fun({Name, _, Source}) ->
                                 Dir = get_deps_dir(DepsDir, Name),
                                 {Name, Vsn} = lists:keyfind(Name, 1, Solve),
                                 rebar_fetch:new(Dir, Name, format_vsn(Vsn), Source)
                         end, rebar_config:deps(Config)),
    ok = file:write_file("./rebar.lock", io_lib:format("~p.~n", [LockDeps])),
    {ok, Config1}.

build(Config, AppInfo) ->
    {ok, AppInfo1} = rebar_otp_app:compile(Config, AppInfo),
    Config1 = rebar_config:replace_app(Config, rebar_app_info:name(AppInfo1), AppInfo1),
    rebar_erlc_compiler:compile(Config, rebar_app_info:dir(AppInfo1)),
    {AppInfo1, Config1}.

%% ===================================================================
%% Internal functions
%% ===================================================================

construct_graph(Config) ->
    LibDirs = rebar_config:get_local(Config, lib_dirs, ["apps", "libs", "."]),
    DepsDir = rebar_deps:get_deps_dir(Config),
    Graph = rlx_depsolver:new_graph(),
    Apps = rebar_app_discover:find_apps([DepsDir | LibDirs]),
    lists:foldl(fun(AppInfo, GraphAcc) ->
                        Name = rebar_app_info:name(AppInfo),
                        Vsn = rebar_app_info:original_vsn(AppInfo),
                        C = rebar_config:new2(rebar_config:new(), rebar_app_info:dir(AppInfo)),
                        LocalDeps = rebar_config:get_local(C, deps, []),
                        PkgDeps = lists:map(fun({A, "", _}) ->
                                                    A;
                                               ({A, ".*", _}) ->
                                                    A;
                                               ({A, V, _}) ->
                                                    {A, V}
                                            end, LocalDeps),
                        rlx_depsolver:add_package_version(GraphAcc
                                                         ,Name
                                                         ,Vsn
                                                         ,PkgDeps)
                end, Graph, Apps).

get_deps_dir(Config) ->
    BaseDir = rebar_utils:base_dir(Config),
    get_deps_dir(BaseDir, deps).

get_deps_dir(DepsDir, App) ->
    filename:join(DepsDir, atom_to_list(App)).

format_vsn(Vsn) ->
    binary_to_list(iolist_to_binary(ec_semver:format(Vsn))).
