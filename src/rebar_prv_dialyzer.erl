%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

-module(rebar_prv_dialyzer).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-include("rebar.hrl").

-define(PROVIDER, dialyzer).
-define(DEPS, [compile]).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    State1 = rebar_state:add_provider(State, providers:create([{name, ?PROVIDER},
                                                               {module, ?MODULE},
                                                               {bare, false},
                                                               {deps, ?DEPS},
                                                               {example, "rebar dialyzer"},
                                                               {short_desc, "Run the Dialyzer analyzer on the project."},
                                                               {desc, ""},
                                                               {opts, []}])),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    ?INFO("Dialyzer starting, this may take a while...", []),
    BuildDir = rebar_state:get(State, base_dir, ?DEFAULT_BASE_DIR),
    {ProjectPlt, DepPlt} = get_plt_location(BuildDir),
    Apps = rebar_state:project_apps(State),
    Deps = rebar_state:get(State, all_deps, []),

    try
        ?INFO("Doing plt for dependencies...", []),
        update_dep_plt(State, DepPlt, Deps),
        ?INFO("Doing plt for project apps...", []),
        update_dep_plt(State, ProjectPlt, Apps),
        WarningTypes = rebar_state:get(State, dialyzer_warnings, default_warnings()),
        Paths = [filename:join(rebar_app_info:dir(App), "ebin") || App <- Apps],
        Opts = [{analysis_type, succ_typings},
                {from, byte_code},
                {files_rec, Paths},
                {warnings, WarningTypes},
                {plts, [ProjectPlt, DepPlt]}],

        case dialyzer:run(Opts) of
            [] ->
                {ok, State};
            Warnings ->
                [?CONSOLE(string:strip(dialyzer:format_warning(Warning), right, $\n), []) ||
                    Warning <- Warnings],
                {ok, State}
        end
    catch
        _:{dialyzer_error, Error} ->
            {error, {?MODULE, {error_processing_apps, Error, Apps}}}
    end.

-spec format_error(any()) -> iolist().
format_error({error_processing_apps, Error, _Apps}) ->
    io_lib:format("Error in dialyzing apps: ~s", [Error]);
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% Internal functions

get_plt_location(BuildDir) ->
    {filename:join([BuildDir, ".project.plt"]),
     filename:join([BuildDir, ".deps.plt"])}.

update_dep_plt(_State, DepPlt, AppList) ->
    Opts0 =
        case filelib:is_file(DepPlt) of
            true ->
                ?INFO("Plt is built, checking/updating...", []),
                [{analysis_type, plt_check},
                 {plts, [DepPlt]}];
            false ->
                ?INFO("Building the plt, this will take a while...", []),
                [{analysis_type, plt_build},
                 {output_plt, DepPlt}]
        end,
    Paths = [filename:join(rebar_app_info:dir(App), "ebin") || App <- AppList],
    Opts = [{files_rec, Paths},
            {from, byte_code}] ++ Opts0,

    dialyzer:run(Opts).

default_warnings() ->
    [error_handling,
     unmatched_returns,
     underspecs].
