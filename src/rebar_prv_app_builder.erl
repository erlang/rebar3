-module(rebar_prv_app_builder).

-behaviour(rebar_provider).

-export([init/1,
         do/1]).

-include("rebar.hrl").

-define(PROVIDER, compile).
-define(DEPS, [deps]).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    State1 = rebar_state:add_provider(State, #provider{name = ?PROVIDER,
                                                        provider_impl = ?MODULE,
                                                        bare = false,
                                                        deps = ?DEPS,
                                                        example = "rebar compile",
                                                        short_desc = "",
                                                        desc = "",
                                                        opts = []}),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | relx:error().
do(Config) ->
    Apps = rebar_state:apps_to_build(Config),
    Config1 =
        lists:foldl(fun(AppInfo, ConfigAcc) ->
                            ?INFO("Compiling ~p ~s~n", [rebar_app_info:name(AppInfo)
                                                       ,rebar_app_info:original_vsn(AppInfo)]),
                            {_AppInfo1, ConfigAcc1} = build(ConfigAcc, AppInfo),
                            ConfigAcc1
                    end, Config, Apps),

    %% DepsDir = get_deps_dir(Config1),
    %% LockDeps = lists:map(fun({Name, Vsn, Source}) ->
    %%                              Dir = get_deps_dir(DepsDir, Name),
    %%                              rebar_fetch:new(Dir, Name, Vsn, Source)
    %%                      end, rebar_state:deps(Config)),
    %% ok = file:write_file("./rebar.lock", io_lib:format("~p.~n", [LockDeps])),
    {ok, Config1}.

build(Config, AppInfo) ->
    {ok, AppInfo1} = rebar_otp_app:compile(Config, AppInfo),
    Config1 = rebar_state:add_app(Config, AppInfo1),
    rebar_erlc_compiler:compile(Config, rebar_app_info:dir(AppInfo1)),
    {AppInfo1, Config1}.

%% ===================================================================
%% Internal functions
%% ===================================================================

get_deps_dir(Config) ->
    BaseDir = rebar_utils:base_dir(Config),
    get_deps_dir(BaseDir, deps).

get_deps_dir(DepsDir, App) ->
    filename:join(DepsDir, atom_to_list(App)).
