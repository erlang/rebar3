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
    State1 = rebar_state:add_provider(
        State,
        providers:create([
            {name, ?PROVIDER},
            {module, ?MODULE},
            {namespace, ?NAMESPACE},
            {bare, false},
            {deps, ?DEPS},
            {example, ""},
            {short_desc, ""},
            {desc, ""},
            {opts, [
                {paths, $p, "paths", string,
                 "Wildcard paths of ebin directories to add to code path, "
                 "separated by a colon"},
                {separator, $s, "separator", string,
                 "In case of multiple return paths, the separator character "
                 "to use to join them."},
                {outdir, $o, "outdir", string,
                 "Path where build artifacts are located. Defaults to the "
                 "current directory."}
            ]}
        ])
    ),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    OrigPath = code:get_path(),

    %% Add code paths from --paths to the beginning of the code path
    {RawOpts, _} = rebar_state:command_parsed_args(State),
    Paths = proplists:get_value(paths, RawOpts),
    Sep = proplists:get_value(separator, RawOpts, " "),
    %% Because mix won't check for versions, it instead sets this variable
    %% that it knows older rebar3 version will ignore so we play nice and
    %% honor it.
    DefaultOutDir = os:getenv("REBAR_BARE_COMPILER_OUTPUT_DIR", rebar_dir:get_cwd()),
    OutDir = proplists:get_value(outdir, RawOpts, DefaultOutDir),
    [ code:add_pathsa(filelib:wildcard(PathWildcard))
      || PathWildcard <- rebar_string:lexemes(Paths, Sep) ],

    case rebar_state:project_apps(State) of
        [] ->
            {error, {?MODULE, {not_an_application_structure, OutDir}}};
        [AppInfo] ->
            AppInfo1 = rebar_app_info:out_dir(AppInfo, OutDir),

            %% run compile in the default namespace
            rebar_prv_compile:compile(rebar_state:namespace(State, default), AppInfo1),

            rebar_utils:cleanup_code_path(OrigPath),

            {ok, State}
    end.

-spec format_error(any()) -> iolist().
format_error({not_an_application_structure, Path}) ->
    io_lib:format(
        "Compilation failed: there is no code in this directory (~ts), " ++
        "it is unreadable or for some other reason " ++
        "is not a recognizable application structure.",
        [Path]);
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
