%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

-module(rebar_prv_app_discovery).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-include("rebar.hrl").
-include_lib("providers/include/providers.hrl").

-define(PROVIDER, app_discovery).
-define(DEPS, []).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    State1 = rebar_state:add_provider(State, providers:create([{name, ?PROVIDER},
                                                               {module, ?MODULE},
                                                               {bare, false},
                                                               {deps, ?DEPS},
                                                               {example, ""},
                                                               {short_desc, ""},
                                                               {desc, ""},
                                                               {opts, []}])),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    LibDirs = rebar_dir:lib_dirs(State),
    try
        State1 = rebar_app_discover:do(State, LibDirs),
        State2 = rebar_plugins:project_apps_install(State1),
        {ok, State2}
    catch
        throw:{error, {rebar_packages, Error}} ->
            {error, {rebar_packages, Error}};
        throw:{error, {rebar_app_utils, Error}} ->
            {error, {rebar_app_utils, Error}};
        throw:{error, {rebar_app_discover, Error}} ->
            {error, {rebar_app_discover, Error}};
        throw:{error, Error} ->
            ?PRV_ERROR(Error)
    end.

-spec format_error(any()) -> iolist().
format_error({multiple_app_files, Files}) ->
    io_lib:format("Multiple app files found in one app dir: ~ts", [rebar_string:join(Files, " and ")]);
format_error({invalid_app_file, File, Reason}) ->
    case Reason of
        {Line, erl_parse, Description} ->
            io_lib:format("Invalid app file ~ts at line ~b: ~p",
                [File, Line, lists:flatten(Description)]);
        _ ->
            io_lib:format("Invalid app file ~ts: ~p", [File, Reason])
    end;
%% Provide a slightly more informative error message for consult of app file failure
format_error({rebar_file_utils, {bad_term_file, AppFile, Reason}}) ->
    io_lib:format("Error in app file ~ts: ~ts", [rebar_dir:make_relative_path(AppFile,
                                                                              rebar_dir:get_cwd()),
                                                 file:format_error(Reason)]);
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
