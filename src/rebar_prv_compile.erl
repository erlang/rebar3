-module(rebar_prv_compile).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-export([compile/2]).

-include("rebar.hrl").

-define(PROVIDER, compile).
-define(DEPS, [lock]).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    State1 = rebar_state:add_provider(State, providers:create([{name, ?PROVIDER},
                                                               {module, ?MODULE},
                                                               {bare, false},
                                                               {deps, ?DEPS},
                                                               {example, "rebar3 compile"},
                                                               {short_desc, "Compile apps .app.src and .erl files."},
                                                               {desc, ""},
                                                               {opts, []}])),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    ProjectApps = rebar_state:project_apps(State),
    Providers = rebar_state:providers(State),
    Deps = rebar_state:deps_to_build(State),
    Cwd = rebar_dir:get_cwd(),

    rebar_hooks:run_all_hooks(Cwd, pre, compile, Providers, State),

    %% Need to allow global config vars used on deps
    %% Right now no way to differeniate and just give deps a new state
    EmptyState = rebar_state:new(),
    build_apps(EmptyState, Providers, Deps),

    %% Use the project State for building project apps
    %% Set hooks to empty so top-level hooks aren't run for each project app
    State2 = rebar_state:set(rebar_state:set(State, post_hooks, []), pre_hooks, []),
    ProjectApps1 = build_apps(State2, Providers, ProjectApps),

    rebar_hooks:run_all_hooks(Cwd, post, compile, Providers, State),

    {ok, rebar_state:project_apps(State, ProjectApps1)}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

build_apps(State, Providers, Apps) ->
    [build_app(State, Providers, AppInfo) || AppInfo <- Apps].

build_app(State, Providers, AppInfo) ->
    AppDir = rebar_app_info:dir(AppInfo),
    OutDir = rebar_app_info:out_dir(AppInfo),

    copy_app_dirs(State, AppDir, OutDir),

    S = case rebar_app_info:state(AppInfo) of
            undefined ->
                C = rebar_config:consult(AppDir),
                rebar_state:new(State, C, AppDir);
            AppState ->
                AppState
        end,

    %% Legacy hook support

    rebar_hooks:run_all_hooks(AppDir, pre, compile,  Providers, S),
    AppInfo1 = compile(S, AppInfo),
    rebar_hooks:run_all_hooks(AppDir, post, compile, Providers, S),

    true = code:add_patha(rebar_app_info:ebin_dir(AppInfo1)),
    AppInfo1.

compile(State, AppInfo) ->
    ?INFO("Compiling ~s", [rebar_app_info:name(AppInfo)]),
    rebar_erlc_compiler:compile(State, ec_cnv:to_list(rebar_app_info:out_dir(AppInfo))),
    case rebar_otp_app:compile(State, AppInfo) of
        {ok, AppInfo1} ->
            AppInfo1;
        Error ->
            throw(Error)
    end.

%% ===================================================================
%% Internal functions
%% ===================================================================

copy_app_dirs(State, OldAppDir, AppDir) ->
    case ec_cnv:to_binary(filename:absname(OldAppDir)) =/=
        ec_cnv:to_binary(filename:absname(AppDir)) of
        true ->
            EbinDir = filename:join([OldAppDir, "ebin"]),
            %% copy all files from ebin if it exists
            case filelib:is_dir(EbinDir) of
                true ->
                    OutEbin = filename:join(AppDir, "ebin"),
                    filelib:ensure_dir(filename:join(OutEbin, "dummy.beam")),
                    rebar_file_utils:cp_r(filelib:wildcard(filename:join(EbinDir, "*")), OutEbin);
                false ->
                    ok
            end,
            filelib:ensure_dir(filename:join(AppDir, "dummy")),
            %% link to src_dirs to be adjacent to ebin is needed for R15 use of cover/xref
            ErlOpts = rebar_utils:erl_opts(State),
            SrcDirs = proplists:get_value(src_dirs, ErlOpts, ["src"]),
            [symlink_or_copy(OldAppDir, AppDir, Dir) || Dir <- ["priv", "include", "test"] ++ SrcDirs];
        false ->
            ok
    end.

symlink_or_copy(OldAppDir, AppDir, Dir) ->
    Source = filename:join(OldAppDir, Dir),
    Target = filename:join(AppDir, Dir),
    rebar_file_utils:symlink_or_copy(Source, Target).
