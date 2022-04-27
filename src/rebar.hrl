-define(ABORT, rebar_utils:abort()).
-define(ABORT(Str, Args), rebar_utils:abort(Str, Args)).

-define(CONSOLE(Str, Args), io:format(Str++"~n", Args)).

-define(DIAGNOSTIC(Str, Args), rebar_log:log(diagnostic, Str, Args)).
-define(DEBUG(Str, Args), rebar_log:log(debug, Str, Args)).
-define(INFO(Str, Args), rebar_log:log(info, Str, Args)).
-define(WARN(Str, Args), rebar_log:log(warn, Str, Args)).
-define(ERROR(Str, Args), rebar_log:log(error, Str, Args)).
-define(CRASHDUMP(Str, Args), rebar_log:crashdump(Str, Args)).

-define(FMT(Str, Args), lists:flatten(io_lib:format(Str, Args))).

-define(DEFAULT_BASE_DIR, "_build").
-define(DEFAULT_ROOT_DIR, ".").
-define(DEFAULT_PROJECT_APP_DIRS, ["apps/*", "lib/*", "."]).
-define(DEFAULT_PROJECT_PLUGIN_DIRS, ["plugins/*"]).
-define(DEFAULT_CHECKOUTS_DIR, "_checkouts").
-define(DEFAULT_CHECKOUTS_OUT_DIR, "checkouts").
-define(DEFAULT_DEPS_DIR, "lib").
-define(DEFAULT_PLUGINS_DIR, "plugins").
-define(DEFAULT_TEST_DEPS_DIR, "test/lib").
-define(DEFAULT_RELEASE_DIR, "rel").
-define(CONFIG_VERSION, "1.2.0").
-define(SUPPORTED_CONFIG_VERSIONS, ["1.1.0", "1.2.0"]). % older were untagged
-define(DEFAULT_CDN, "https://repo.hex.pm").
-define(LOCK_FILE, "rebar.lock").
-define(DEFAULT_COMPILER_SOURCE_FORMAT, relative).
-define(PACKAGE_INDEX_VERSION, 6).
-define(PACKAGE_TABLE, package_index).
-define(INDEX_FILE, "packages.idx").
-define(HEX_AUTH_FILE, "hex.config").
-define(PUBLIC_HEX_REPO, <<"hexpm">>).
-define(DEFAULT_APP_RESOURCE_EXT, [".app.src.script", ".app.src"]).

%% ignore this function in all modules
%% not every module that exports it and relies on it being called implements provider
-ignore_xref([{format_error, 1}]).

%% the package record is used in a select match spec which upsets dialyzer
%% this is the suggested workaround from Tobias
%% http://erlang.org/pipermail/erlang-questions/2009-February/041445.html
-type ms_field() :: '$1' | '_' | {'$1', '$2'}.

%% TODO: change package and requirement keys to be required (:=) after dropping support for OTP-18
-record(package, {key :: {unicode:unicode_binary() | ms_field(), unicode:unicode_binary() | ms_field() | ec_semver:semver(),
                          unicode:unicode_binary() | ms_field()},
                  inner_checksum :: binary() | ms_field(),
                  outer_checksum :: binary() | ms_field(),
                  retired :: boolean() | ms_field() | #{reason := atom()},
                  dependencies :: [#{package => unicode:unicode_binary(),
                                     requirement => unicode:unicode_binary()}] | ms_field()}).

-record(resource, {type :: atom(),
                   module :: module(),
                   state :: term(),
                   implementation :: rebar_resource | rebar_resource_v2}).

-type rebar_dict() :: dict:dict().
-type rebar_digraph() :: digraph:graph().
-type rebar_set() :: sets:set().

-ifdef(fun_stacktrace).
-define(WITH_STACKTRACE(T, R, S), T:R -> S = erlang:get_stacktrace(),).
-else.
-define(WITH_STACKTRACE(T, R, S), T:R:S ->).
-endif.

-define(GRAPH_VSN, 2).
-type v() :: {digraph:vertex(), term()} | 'false'.
-type e() :: {digraph:vertex(), digraph:vertex()}.
-type graph() :: {list(v()), list(e())}.
-record(graph,
        {
          vsn = ?GRAPH_VSN :: pos_integer(),
          info = {[], []} :: graph()
        }).
