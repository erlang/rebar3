-module(rebar_prv_compile).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-export([compile/2]).

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
                                                                      {jobs, $j, "jobs", integer, JobsHelp}
                                                                      ]}])),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    {ok, State1} = handle_args(State),
    Jobs = rebar_state:get(State1, jobs),

    ProjectApps = rebar_state:project_apps(State1),
    Deps = rebar_state:deps_to_build(State1),
    Cwd = rebar_dir:get_cwd(),
    rebar_hooks:run_compile_hooks(Cwd, pre_hooks, compile, State1),

    %% Need to allow global config vars used on deps
    %% Right now no way to differeniate and just give deps a new state
    EmptyState = rebar_state:new(),
    EmptyState1 = rebar_state:set(EmptyState, jobs, Jobs),
    build_apps(EmptyState1, Deps),

    %% Use the project State for building project apps
    %% Set hooks to empty so top-level hooks aren't run for each project app
    State2 = rebar_state:set(rebar_state:set(State1, post_hooks, []), pre_hooks, []),
    ProjectApps1 = build_apps(State2, ProjectApps),
    rebar_hooks:run_compile_hooks(Cwd, post_hooks, compile, State1),

    {ok, rebar_state:project_apps(State1, ProjectApps1)}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

build_apps(State, Apps) ->
    [build_app(State, AppInfo) || AppInfo <- Apps].

build_app(State, AppInfo) ->
    AppDir = rebar_app_info:dir(AppInfo),
    OutDir = rebar_app_info:out_dir(AppInfo),

    copy_app_dirs(rebar_app_info:name(AppInfo), AppDir, OutDir),

    S = case rebar_app_info:state(AppInfo) of
            undefined ->
                C = rebar_config:consult(AppDir),
                rebar_state:new(State, C, AppDir);
            AppState ->
                AppState
        end,

    %% Legacy hook support
    rebar_hooks:run_compile_hooks(AppDir, pre_hooks, compile, S),
    AppInfo1 = compile(S, AppInfo),
    rebar_hooks:run_compile_hooks(AppDir, post_hooks, compile, S),

    true = code:add_patha(rebar_app_info:ebin_dir(AppInfo1)),
    AppInfo1.

compile(State, AppInfo) ->
    ?INFO("Compiling ~s", [rebar_app_info:name(AppInfo)]),
    rebar_erlc_compiler:compile(State, ec_cnv:to_list(rebar_app_info:dir(AppInfo)), ec_cnv:to_list(rebar_app_info:out_dir(AppInfo))),
    case rebar_otp_app:compile(State, AppInfo) of
        {ok, AppInfo1} ->
            AppInfo1;
        Error ->
            throw(Error)
    end.

%% ===================================================================
%% Internal functions
%% ===================================================================

handle_args(State) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    Jobs = proplists:get_value(jobs, Args, ?DEFAULT_JOBS),
    {ok, rebar_state:set(State, jobs, Jobs)}.

copy_app_dirs(AppName, OldAppDir, AppDir) ->
    case ec_cnv:to_binary(filename:absname(OldAppDir)) =/=
        ec_cnv:to_binary(filename:absname(AppDir)) of
        true ->
            Name = ec_cnv:to_list(AppName),
            AppFile = filename:join([OldAppDir, "ebin", Name++".app"]),
            case filelib:is_file(AppFile) of
                true ->
                    file:copy(AppFile,
                              filename:join([AppDir, "ebin", Name++".app"]));
                false ->
                    ok
            end,
            filelib:ensure_dir(filename:join(AppDir, "dummy")),
            [file:make_symlink(filename:join(OldAppDir, Dir), filename:join(AppDir, Dir))
            || Dir <- ["priv", "include"]];
        false ->
            ok
    end.
