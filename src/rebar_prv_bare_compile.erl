-module(rebar_prv_bare_compile).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-include_lib("providers/include/providers.hrl").
-include("rebar.hrl").

-define(PROVIDER, compile).
-define(NAMESPACE, bare).
-define(DEPS, [{default, app_discovery}]).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    State1 =
        rebar_state:add_provider(State,
                                providers:create([{name, ?PROVIDER},
                                                  {module, ?MODULE},
                                                  {namespace, ?NAMESPACE},
                                                  {bare, false},
                                                  {deps, ?DEPS},
                                                  {example, ""},
                                                  {short_desc, ""},
                                                  {desc, ""},
                                                  {opts, [{paths, $p, "paths", string, "Wildcard path of ebin directories to add to code path"}]}])),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    OrigPath = code:get_path(),

    %% Add code paths from --paths to the beginning of the code path
    {RawOpts, _} = rebar_state:command_parsed_args(State),
    Paths = proplists:get_value(paths, RawOpts),
    CodePaths = filelib:wildcard(Paths),
    code:add_pathsa(CodePaths),

    [AppInfo] = rebar_state:project_apps(State),
    AppInfo1 = rebar_app_info:out_dir(AppInfo, rebar_dir:get_cwd()),
    AppInfo2 = rebar_prv_compile:compile(State, AppInfo1),
    rebar_prv_app_builder:build_app_file(State, AppInfo2),

    rebar_utils:cleanup_code_path(OrigPath),

    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
