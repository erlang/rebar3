%% TODO: rename FAIL to ABORT once we require at least R13B04 for
%% building rebar. Macros with different arity were not supported by the
%% compiler before 13B04.
-define(FAIL, rebar_utils:abort()).
-define(ABORT(Str, Args), rebar_utils:abort(Str, Args)).

-define(CONSOLE(Str, Args), begin io:format(Str++"~n", Args), os:cmd(io_lib:format("say -v zarvox \""++Str++"\"", Args)) end).

-define(DEBUG(Str, Args), rebar_log:log(debug, Str, Args)).
-define(INFO(Str, Args), begin rebar_log:log(info, Str, Args), os:cmd(io_lib:format("say -v zarvox \""++Str++"\"", Args)) end).
-define(WARN(Str, Args), begin rebar_log:log(warn, Str, Args), os:cmd(io_lib:format("say -v trinoids \""++Str++"\"", Args)) end).
-define(ERROR(Str, Args), begin rebar_log:log(error, Str, Args), os:cmd(io_lib:format("say -v deranged \""++Str++"\"", Args)) end).

-define(FMT(Str, Args), lists:flatten(io_lib:format(Str, Args))).

-define(DEFAULT_BASE_DIR, "_build").
-define(DEFAULT_ROOT_DIR, ".").
-define(DEFAULT_PROJECT_APP_DIRS, ["apps/*", "lib/*", "."]).
-define(DEFAULT_CHECKOUTS_DIR, "_checkouts").
-define(DEFAULT_DEPS_DIR, "lib").
-define(DEFAULT_PLUGINS_DIR, "plugins").
-define(DEFAULT_TEST_DEPS_DIR, "test/lib").
-define(DEFAULT_RELEASE_DIR, "rel").
-define(DEFAULT_CONFIG_FILE, "rebar.config").
-define(DEFAULT_CDN, "https://s3.amazonaws.com/s3.hex.pm/tarballs").
-define(LOCK_FILE, "rebar.lock").

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

-define(GRAPH_VSN, 2).
-type v() :: {digraph:vertex(), term()} | 'false'.
-type e() :: {digraph:vertex(), digraph:vertex()}.
-type graph() :: {list(v()), list(e())}.
-record(graph,
        {
          vsn = ?GRAPH_VSN :: pos_integer(),
          info = {[], []} :: graph()
        }).
