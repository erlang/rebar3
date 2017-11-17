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
                                                  {opts, [{paths, $p, "paths", string, "Wildcard paths of ebin directories to add to code path, separated by a colon"},
                                                          {separator, $s, "separator", string, "In case of multiple return paths, the separator character to use to join them."}]}])),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    OrigPath = code:get_path(),

    %% Add code paths from --paths to the beginning of the code path
    {RawOpts, _} = rebar_state:command_parsed_args(State),
    Paths = proplists:get_value(paths, RawOpts),
    Sep = proplists:get_value(separator, RawOpts, " "),
    [ code:add_pathsa(filelib:wildcard(PathWildcard))
      || PathWildcard <- rebar_string:lexemes(Paths, Sep) ],

    [AppInfo] = rebar_state:project_apps(State),
    AppInfo1 = rebar_app_info:out_dir(AppInfo, rebar_dir:get_cwd()),
    rebar_prv_compile:compile(State, AppInfo1),

    rebar_utils:cleanup_code_path(OrigPath),

    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
