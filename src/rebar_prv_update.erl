%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

-module(rebar_prv_update).

-behaviour(rebar_provider).

-export([init/1,
         do/1]).

-include("rebar.hrl").

-define(PROVIDER, update).
-define(DEPS, []).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_config:config()) -> {ok, rebar_config:config()}.
init(State) ->
    State1 = rebar_config:add_provider(State, #provider{name = ?PROVIDER,
                                                        provider_impl = ?MODULE,
                                                        bare = false,
                                                        deps = ?DEPS,
                                                        example = "rebar update cowboy",
                                                        short_desc = "",
                                                        desc = "",
                                                        opts = []}),
    {ok, State1}.

-spec do(rebar_config:config()) -> {ok, rebar_config:config()} | relx:error().
do(Config) ->
    [Name] = rebar_config:command_args(Config),
    ?INFO("Updating ~s~n", [Name]),

    DepsDir = rebar_deps:get_deps_dir(Config),
    Deps = rebar_config:get_local(Config, deps, []),
    {_, _, Source} = lists:keyfind(list_to_atom(Name), 1, Deps),
    TargetDir = rebar_deps:get_deps_dir(DepsDir, Name),
    rebar_fetch:update_source1(TargetDir, Source),

    [App] = rebar_app_discover:find_apps([TargetDir]),

    {ok, AppInfo1} = rebar_otp_app:compile(Config, App),
    Config1 = rebar_config:replace_app(Config, rebar_app_info:name(AppInfo1), AppInfo1),
    rebar_erlc_compiler:compile(Config, rebar_app_info:dir(AppInfo1)),
    update_lock_file(Config, AppInfo1, Source),

    {ok, Config}.

update_lock_file(Config, App, Source) ->
    New = rebar_fetch:new(rebar_app_info:dir(App), rebar_app_info:name(App), rebar_app_info:original_vsn(App), Source),
    {ok, [Terms]} = file:consult("./rebar.lock"),
    LockDeps = lists:keyreplace(rebar_app_info:name(App), 1, Terms, New),
    ok = file:write_file("./rebar.lock", io_lib:format("~p.~n", [LockDeps])).
