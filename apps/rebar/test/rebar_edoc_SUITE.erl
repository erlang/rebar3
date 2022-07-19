-module(rebar_edoc_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

all() -> [multiapp, multiapp_macros, error_survival].

init_per_testcase(multiapp, Config) ->
    application:load(rebar),
    DataDir = ?config(data_dir, Config),
    PrivDir = ?config(priv_dir, Config),
    Name = rebar_test_utils:create_random_name("multiapp"),
    AppsDir = filename:join([PrivDir, rebar_test_utils:create_random_name(Name)]),
    ec_file:copy(filename:join([DataDir, "foo"]), AppsDir, [recursive]),
    Verbosity = rebar3:log_level(),
    rebar_log:init(command_line, Verbosity),
    State = rebar_state:new([{base_dir, filename:join([AppsDir, "_build"])}
                            ,{root_dir, AppsDir}]),
    [{apps, AppsDir}, {state, State}, {name, Name} | Config];
init_per_testcase(multiapp_macros, Config) ->
    application:load(rebar),
    DataDir = ?config(data_dir, Config),
    PrivDir = ?config(priv_dir, Config),
    Name = rebar_test_utils:create_random_name("multiapp_macros"),
    AppsDir = filename:join([PrivDir, rebar_test_utils:create_random_name(Name)]),
    ec_file:copy(filename:join([DataDir, "foo"]), AppsDir, [recursive]),
    ok = ec_file:remove(filename:join([AppsDir, "apps", "foo"]), [recursive]),
    Verbosity = rebar3:log_level(),
    rebar_log:init(command_line, Verbosity),
    State = rebar_state:new([{base_dir, filename:join([AppsDir, "_build"])}
                            ,{root_dir, AppsDir}]),
    [{apps, AppsDir}, {state, State}, {name, Name} | Config];
init_per_testcase(error_survival, Config) ->
    application:load(rebar),
    DataDir = ?config(data_dir, Config),
    PrivDir = ?config(priv_dir, Config),
    Name = rebar_test_utils:create_random_name("error_survival"),
    AppsDir = filename:join([PrivDir, rebar_test_utils:create_random_name(Name)]),
    ec_file:copy(filename:join([DataDir, "bad"]), AppsDir, [recursive]),
    Verbosity = rebar3:log_level(),
    rebar_log:init(command_line, Verbosity),
    State = rebar_state:new([{base_dir, filename:join([AppsDir, "_build"])}
                            ,{root_dir, AppsDir}]),
    [{apps, AppsDir}, {state, State}, {name, Name} | Config].

end_per_testcase(_, Config) ->
    Config.

multiapp(Config) ->
    %% With an empty config (no `dir'), links are being processed
    RebarConfig = [],
    rebar_test_utils:run_and_check(Config, RebarConfig, ["edoc"], {ok, []}),
    %% validate that all doc entries are generated and links work
    AppsDir = ?config(apps, Config),
    ct:pal("AppsDir: ~s", [AppsDir]),
    ?assert(file_content_matches(
              filename:join([AppsDir, "apps", "bar1", "doc", "bar1.html"]),
              "barer1")),
    ?assert(file_content_matches(
              filename:join([AppsDir, "apps", "bar2", "doc", "bar2.html"]),
              "barer2")),
    %% Links are in place for types
    ?assert(file_content_matches(
              filename:join([AppsDir, "apps", "foo", "doc", "foo.html"]),
              "barer1")),
    ?assert(file_content_matches(
              filename:join([AppsDir, "apps", "foo", "doc", "foo.html"]),
              "apps/bar1/doc/bar1.html")),
    %% Options such from rebar.config in the app themselves are
    %% respected
    ?assert(file_content_matches(
        filename:join([AppsDir, "apps", "foo", "doc", "overview-summary.html"]),
        "foo_custom_title"
    )),
    ok.

multiapp_macros(Config) ->
    RebarConfig = [{edoc_opts, [
        preprocess,
        {macros, [{m1, x1}, {m2, x2}]},
        {def, [{d1, "1"}, {d2, "1"}]}
    ]}],
    AppConfig = {edoc_opts, [
        {preprocess, true},
        {macros, [{m2, f2}, {m3, f3}]},
        {def, [{d2, "2"}, {d3, "2"}]}
    ]},
    DebugModule = "
    -module(debug).
    -ifndef(m1). -define(m1,z1). -endif.
    -ifndef(m2). -define(m2,z2). -endif.
    -ifndef(m3). -define(m3,z3). -endif.
    -export([?m1 /0, ?m2 /0, ?m3 /0]).

    %% @doc
    %% d1:{@d1}
    %% d2:{@d2}
    %% d3:{@d3}
    %% @end
    ?m1 () -> ok.
    ?m2 () -> ok.
    ?m3 () -> ok.
    ",
    AppsDir = ?config(apps, Config),
    ct:pal("AppsDir: ~s", [AppsDir]),
    ok = file:write_file(filename:join([AppsDir, "apps", "bar1", "rebar.config"]),
                         io_lib:format("~p.~n", [AppConfig])),
    ok = file:write_file(filename:join([AppsDir, "apps", "bar1", "src", "debug.erl"]),
                         DebugModule),
    rebar_test_utils:run_and_check(Config, RebarConfig, ["edoc"], {ok, []}),
    DocFile = filename:join([AppsDir, "apps", "bar1", "doc", "debug.html"]),
    ?assert(file_content_matches(DocFile, "d1:1")), % config layered
    ?assert(file_content_matches(DocFile, "d2:2")),
    ?assert(file_content_matches(DocFile, "d3:2")),
    ?assert(file_content_matches(DocFile, "x1/0")), % elided in config drop
    ?assert(file_content_matches(DocFile, "f2/0")),
    ?assert(file_content_matches(DocFile, "f3/0")),
    ok.

error_survival(Config) ->
    RebarConfig = [],
    rebar_test_utils:run_and_check(
        Config, RebarConfig, ["edoc"],
        {error,{rebar_prv_edoc,{app_failed,"bad_bar2"}}}
    ),
    ok.


file_content_matches(Path, Regex) ->
    case file:read_file(Path) of
        {ok, Bin} ->
            nomatch =/= re:run(Bin, Regex);
        {error, Reason} ->
            Reason
    end.
