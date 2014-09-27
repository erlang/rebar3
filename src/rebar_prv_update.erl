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

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    case rebar_state:command_args(State) of
        [Name] ->
            ?INFO("Updating ~s~n", [Name]),
            Locks = rebar_state:get(State, locks, []),
            case lists:keyfind(ec_cnv:to_binary(Name), 1, Locks) of
                {_, _, _, Level} ->
                    Deps = rebar_state:get(State, deps),
                    Dep = lists:keyfind(list_to_atom(Name), 1, Deps),
                    rebar_prv_install_deps:handle_deps(State, [Dep], {true, ec_cnv:to_binary(Name), Level}),
                    {ok, State};
                false ->
                    {error, io_lib:format("No such dependency ~s~n", [Name])}
            end;
        [] ->
            ?INFO("Updating package index...~n", []),
            try
                Url = url(State),
                                                %{ok, [Home]} = init:get_argument(home),
                ec_file:mkdir_p(filename:join([os:getenv("HOME"), ".rebar"])),
                PackagesFile = filename:join([os:getenv("HOME"), ".rebar", "packages"]),
                {ok, RequestId} = httpc:request(get, {Url, []}, [], [{stream, PackagesFile}
                                                                    ,{sync, true}])
            catch
                _:_ ->
                    {error, io_lib:format("Failed to write package index.~n", [])}
            end
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
