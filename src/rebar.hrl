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
-define(LOCK_FILE, "rebar.lock").
-define(DEFAULT_COMPILER_SOURCE_FORMAT, relative).
-define(PACKAGE_INDEX_VERSION, 5).
-define(PACKAGE_TABLE, package_index).
-define(INDEX_FILE, "packages.idx").
-define(HEX_AUTH_FILE, "hex.config").
-define(PUBLIC_HEX_REPO, <<"hexpm">>).

%% ignore this function in all modules
%% not every module that exports it and relies on it being called implements provider
-ignore_xref([{format_error, 1}]).

%% the package record is used in a select match spec which upsets dialyzer
%% this is the suggested workaround from Tobias
%% http://erlang.org/pipermail/erlang-questions/2009-February/041445.html
-type ms_field() :: '$1' | '_'.

%% TODO: change package and requirement keys to be required (:=) after dropping support for OTP-18
-record(package, {key :: {unicode:unicode_binary() | ms_field(), unicode:unicode_binary() | ms_field(),
                          unicode:unicode_binary() | ms_field()},
                  checksum :: binary() | ms_field(),
                  retired :: boolean() | ms_field(),
                  dependencies :: [#{package => unicode:unicode_binary(),
                                     requirement => unicode:unicode_binary()}] | ms_field()}).

-record(resource, {type :: atom(),
                   module :: module(),
                   state :: term(),
                   implementation :: rebar_resource | rebar_resource_v2}).

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
