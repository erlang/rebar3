-module(rebar_prv_compile).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/2,
         build/2]).

-include("rebar.hrl").

-define(PROVIDER, compile).
-define(DEPS, [lock]).

-define(DEFAULT_JOBS, 3).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    JobsHelp = io_lib:format(
                 "Number of concurrent workers the compiler may use. Default: ~B",
                 [?DEFAULT_JOBS]),
    State1 = rebar_state:add_provider(State, providers:create([{name, ?PROVIDER},
                                                               {module, ?MODULE},
                                                               {bare, false},
                                                               {deps, ?DEPS},
                                                               {example, "rebar compile"},
                                                               {short_desc, "Compile apps .app.src and .erl files."},
                                                               {desc, ""},
                                                               {opts, [
                                                                      {jobs,     $j, "jobs",     integer,   JobsHelp}
                                                                      ]}])),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    {ok, State1} = handle_args(State),
    Jobs = rebar_state:get(State1, jobs),

    ProjectApps = rebar_state:project_apps(State1),
    Deps = rebar_state:get(State1, deps_to_build, []),

    %% Need to allow global config vars used on deps
    %% Right now no way to differeniate and just give deps a new state
    lists:foreach(fun(AppInfo) ->
                          AppDir = rebar_app_info:dir(AppInfo),
                          C = rebar_config:consult(AppDir),
                          S = rebar_state:new(rebar_state:new(), C, AppDir),
                          S1 = rebar_state:set(S, jobs, Jobs),
                          build(S1, AppInfo)
                  end, Deps),

    %% Use the project State for building project apps
    lists:foreach(fun(AppInfo) ->
                          AppDir = rebar_app_info:dir(AppInfo),
                          C = rebar_config:consult(AppDir),
                          S = rebar_state:new(State1, C, AppDir),
                          build(S, AppInfo)
                  end, ProjectApps),

    {ok, State1}.

-spec format_error(any(), rebar_state:t()) ->  {iolist(), rebar_state:t()}.
format_error(Reason, State) ->
    {io_lib:format("~p", [Reason]), State}.

build(State, AppInfo) ->
    ?INFO("Compiling ~s~n", [rebar_app_info:name(AppInfo)]),
    rebar_erlc_compiler:compile(State, ec_cnv:to_list(rebar_app_info:dir(AppInfo))),
    {ok, AppInfo1} = rebar_otp_app:compile(State, AppInfo),
    AppInfo1.

%% ===================================================================
%% Internal functions
%% ===================================================================

handle_args(State) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    Jobs = proplists:get_value(jobs, Args, ?DEFAULT_JOBS),
    {ok, rebar_state:set(State, jobs, Jobs)}.
