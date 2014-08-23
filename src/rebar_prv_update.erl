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

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    State1 = rebar_state:add_provider(State, #provider{name = ?PROVIDER,
                                                        provider_impl = ?MODULE,
                                                        bare = false,
                                                        deps = ?DEPS,
                                                        example = "rebar update cowboy",
                                                        short_desc = "",
                                                        desc = "",
                                                        opts = []}),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | relx:error().
do(State) ->
    case rebar_state:command_args(State) of
        [Name] ->
            ?INFO("Updating ~s~n", [Name]),

            DepsDir = rebar_deps:get_deps_dir(State),
            Deps = rebar_state:get_local(State, deps, []),
            {_, _, Source} = lists:keyfind(list_to_atom(Name), 1, Deps),
            TargetDir = rebar_deps:get_deps_dir(DepsDir, Name),
            rebar_fetch:update_source1(TargetDir, Source),

            [App] = rebar_app_discover:find_apps([TargetDir]),

            {ok, AppInfo1} = rebar_otp_app:compile(State, App),
            State1 = rebar_state:replace_app(State, rebar_app_info:name(AppInfo1), AppInfo1),
            rebar_erlc_compiler:compile(State, rebar_app_info:dir(AppInfo1)),

            %update_lock_file(State, AppInfo1, Source),

            {ok, State1};
        [] ->
            ?INFO("Updating package index...", []),
            Url = rebar_state:get(State, rebar_packages_url, "http://localhost:8080"),
            ec_file:mkdir_p(filename:join([os:getenv("HOME"), ".rebar"])),
            PackagesFile = filename:join([os:getenv("HOME"), ".rebar", "packages"]),
            {ok, RequestId} = httpc:request(get, {Url, []}, [], [{stream, PackagesFile}, {sync, false}]),
            wait(RequestId, State)
    end.

wait(RequestId, State) ->
    receive
        {http, {RequestId, saved_to_file}} ->
            io:format("~n"),
            {ok, State}
    after
        500 ->
            io:format("."),
            wait(RequestId, State)
    end.
