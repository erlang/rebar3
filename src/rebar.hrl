%% TODO: rename FAIL to ABORT once we require at least R13B04 for
%% building rebar. Macros with different arity were not supported by the
%% compiler before 13B04.
-define(FAIL, rebar_utils:abort()).
-define(ABORT(Str, Args), rebar_utils:abort(Str, Args)).

-define(CONSOLE(Str, Args), io:format(Str++"~n", Args)).

-define(DEBUG(Str, Args), rebar_log:log(debug, Str, Args)).
-define(INFO(Str, Args), rebar_log:log(info, Str, Args)).
-define(WARN(Str, Args), rebar_log:log(warn, Str, Args)).
-define(ERROR(Str, Args), rebar_log:log(error, Str, Args)).
-define(CRASHDUMP(Str, Args), rebar_log:crashdump(Str, Args)).

-define(FMT(Str, Args), lists:flatten(io_lib:format(Str, Args))).

-define(DEFAULT_BASE_DIR, "_build").
-define(DEFAULT_ROOT_DIR, ".").
-define(DEFAULT_PROJECT_APP_DIRS, ["apps/*", "lib/*", "."]).
-define(DEFAULT_CHECKOUTS_DIR, "_checkouts").
-define(DEFAULT_DEPS_DIR, "lib").
-define(DEFAULT_PLUGINS_DIR, "plugins").
-define(DEFAULT_TEST_DEPS_DIR, "test/lib").
-define(DEFAULT_RELEASE_DIR, "rel").
-define(CONFIG_VERSION, "1.1.0").
-define(DEFAULT_CDN, "https://repo.hex.pm/").
-define(REMOTE_PACKAGE_DIR, "tarballs").
-define(REMOTE_REGISTRY_FILE, "registry.ets.gz").
-define(LOCK_FILE, "rebar.lock").
-define(DEFAULT_COMPILER_SOURCE_FORMAT, relative).

-define(PACKAGE_INDEX_VERSION, 3).
-define(PACKAGE_TABLE, package_index).
-define(INDEX_FILE, "packages.idx").
-define(REGISTRY_FILE, "registry").

-ifdef(namespaced_types).
-type rebar_dict() :: dict:dict().
-else.
-type rebar_dict() :: dict().
-endif.

-ifdef(namespaced_types).
-type rebar_digraph() :: digraph:graph().
-else.
-type rebar_digraph() :: digraph().
-endif.

-ifdef(namespaced_types).
-type rebar_set() :: sets:set().
-else.
-type rebar_set() :: set().
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
