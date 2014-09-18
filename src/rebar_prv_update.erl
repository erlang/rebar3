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
                                                       short_desc = "Update package index or individual dependency.",
                                                       desc = "",
                                                       opts = []}),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | relx:error().
do(State) ->
    case rebar_state:command_args(State) of
        [Name] ->
            ?INFO("Updating ~s~n", [Name]),

            DepsDir = rebar_prv_install_deps:get_deps_dir(State),
            Deps = rebar_state:get(State, deps, []),
            {_, _, Source} = lists:keyfind(list_to_atom(Name), 1, Deps),
            TargetDir = rebar_prv_install_deps:get_deps_dir(DepsDir, Name),
            rebar_fetch:update_source1(TargetDir, Source),
            State1 = rebar_state:set(State, locks, []),
            {ok, State2} = rebar_prv_install_deps:do(State1),
            {ok, State3} = rebar_prv_lock:do(State2),
            {ok, State4} = rebar_prv_compile:do(State3),
            {ok, State4};
        [] ->
            ?INFO("Updating package index...~n", []),
            Url = url(State),
            %{ok, [Home]} = init:get_argument(home),
            ec_file:mkdir_p(filename:join([os:getenv("HOME"), ".rebar"])),
            PackagesFile = filename:join([os:getenv("HOME"), ".rebar", "packages"]),
            {ok, RequestId} = httpc:request(get, {Url, []}, [], [{stream, PackagesFile}, {sync, false}]),
            wait(RequestId, State)
    end.

wait(RequestId, State) ->
    receive
        {http, {RequestId, saved_to_file}} ->
            {ok, State}
    after
        500 ->
            io:format("."),
            wait(RequestId, State)
    end.

url(State) ->
    SystemArch = erlang:system_info(system_architecture),
    ErtsVsn = erlang:system_info(version),
    {glibc, GlibcVsn, _, _} = erlang:system_info(allocator),
    GlibcVsnStr = io_lib:format("~p.~p", GlibcVsn),

    Qs = [io_lib:format("~s=~s", [X, Y]) || {X, Y} <- [{"arch", SystemArch}
                                                      ,{"erts", ErtsVsn}
                                                      ,{"glibc", GlibcVsnStr}]],
    Url = rebar_state:get(State, rebar_packages_url, "http://polar-caverns-6802.herokuapp.com"),
    Url ++ "?" ++ string:join(Qs, "&").
