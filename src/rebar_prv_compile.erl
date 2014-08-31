-module(rebar_prv_compile).

-behaviour(rebar_provider).

-export([init/1,
         do/1,
         build/2]).

-include("rebar.hrl").

-define(PROVIDER, compile).
-define(DEPS, [install_deps]).

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
do(State) ->
    Apps = rebar_state:project_apps(State),

    lists:foreach(fun(AppInfo) ->
                          C = rebar_config:consult(rebar_app_info:dir(AppInfo)),
                          S = rebar_state:new(rebar_state:new(), C, rebar_app_info:dir(AppInfo)),
                          _AppInfo1 = build(S, AppInfo)
                  end, Apps),

    rebar_lock:create(State),
    {ok, State}.

build(State, AppInfo) ->
    ?INFO("Compiling ~s~n", [rebar_app_info:name(AppInfo)]),
    rebar_erlc_compiler:compile(State, ec_cnv:to_list(rebar_app_info:dir(AppInfo))),
    {ok, AppInfo1} = rebar_otp_app:compile(State, AppInfo),
    AppInfo1.

%% ===================================================================
%% Internal functions
%% ===================================================================
