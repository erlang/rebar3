%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

-module(rebar_prv_clean).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/2]).

-include("rebar.hrl").

-define(PROVIDER, clean).
-define(DEPS, [app_discovery]).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    State1 = rebar_state:add_provider(State, providers:create([{name, ?PROVIDER},
                                                               {module, ?MODULE},
                                                               {bare, false},
                                                               {deps, ?DEPS},
                                                               {example, "rebar clean"},
                                                               {short_desc, "Remove compiled beam files from apps."},
                                                               {desc, ""},
                                                               {opts, [{all, $a, "all", undefined, "Clean all apps include deps"}]}])),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    ProjectApps = rebar_state:project_apps(State),
    {all, All} = handle_args(State),

    Apps = case All of
               true ->
                   DepsDir = rebar_state:get(State, deps_dir, ?DEFAULT_DEPS_DIR),
                   DepApps = rebar_app_discover:find_apps([DepsDir], all),
                   ProjectApps ++ DepApps;
               false ->
                   ProjectApps
           end,

    lists:foreach(fun(AppInfo) ->
                          ?INFO("Cleaning out ~s...~n", [rebar_app_info:name(AppInfo)]),
                          rebar_erlc_compiler:clean(State, ec_cnv:to_list(rebar_app_info:dir(AppInfo)))
                  end, Apps),

    {ok, State}.

-spec format_error(any(), rebar_state:t()) ->  {iolist(), rebar_state:t()}.
format_error(Reason, State) ->
    {io_lib:format("~p", [Reason]), State}.

%% ===================================================================
%% Internal functions
%% ===================================================================

handle_args(State) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    All = proplists:get_value(all, Args, false),
    {all, All}.
