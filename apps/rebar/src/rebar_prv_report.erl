%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

-module(rebar_prv_report).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-include("rebar.hrl").

-define(PROVIDER, report).
-define(DEPS, []).
-define(ISSUES_URL, "https://github.com/erlang/rebar3/issues").

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    State1 = rebar_state:add_provider(State, providers:create([{name, ?PROVIDER},
                                                               {module, ?MODULE},
                                                               {bare, true},
                                                               {deps, ?DEPS},
                                                               {example, "rebar3 report \"<task>\""},
                                                               {short_desc, "Provide a crash report to be sent to the rebar3 issues page."},
                                                               {desc, "Provide a crash report to be sent to the rebar3 issues page."},
                                                               {opts, [
                                                                      {task, undefined, undefined, string, "Task to print details for."}
                                                                      ]}])),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    %% Show command
    Task = rebar_state:command_args(State),
    Command = parse_task(Task),
    %% Show command version (if a plugin?)
    %% ...
    %% Show app versions (including rebar3)
    {ok, Vsn} = application:get_key(rebar, vsn),
    {ok, Apps} = application:get_key(rebar, applications),
    [application:load(App) || App <- Apps],
    Vsns = [io_lib:format("~p: ~ts~n", [App, AVsn])
            || App <- lists:sort(Apps),
               {ok, AVsn} <- [application:get_key(App, vsn)]],
    %% Show OS and versions
    OS = erlang:system_info(system_architecture),
    %% Erlang version (ERTS)
    ERTS = erlang:system_info(system_version),
    %% ERTS root directory
    Root = code:root_dir(),
    Lib = code:lib_dir(),
    %% datetime
    UTC = calendar:universal_time(),
    %%
    ?CONSOLE(
        "Rebar3 report~n"
        " version ~ts~n"
        " generated at ~ts~n"
        "=================~n"
        "Please submit this along with your issue at ~ts "
        "(and feel free to edit out private information, if any)~n"
        "-----------------~n"
        "Task: ~ts~n"
        "Entered as:~n"
        "  ~ts~n"
        "-----------------~n"
        "Operating System: ~ts~n"
        "ERTS: ~ts"
        "Root Directory: ~ts~n"
        "Library directory: ~ts~n"
        "-----------------~n"
        "Loaded Applications:~n"
        "~ts~n"
        "-----------------~n"
        "Escript path: ~ts~n"
        "Providers:~n"
        "  ~ts",
        [Vsn, time_to_string(UTC),
         ?ISSUES_URL, Command, Task,
         OS, ERTS, Root, Lib,
         Vsns,
         rebar_state:escript_path(State),
         [providers:format(P)++" "
          || P <- lists:sort(rebar_state:providers(State))]
        ]
    ),
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

time_to_string({{Y,M,D},{H,Min,S}}) ->
    lists:flatten(io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w+00:00",
                  [Y,M,D,H,Min,S])).

parse_task(Str) ->
    hd(re:split(Str, " ", [unicode])).
