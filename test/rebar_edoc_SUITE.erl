-module(rebar_edoc_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

all() -> [multiapp, error_survival].

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
              "apps/bar1/doc/bar1.html")).

error_survival(Config) ->
    RebarConfig = [],
    rebar_test_utils:run_and_check(
        Config, RebarConfig, ["edoc"],
        {error,{rebar_prv_edoc,{app_failed,"bar2"}}}
    ),
    ok.


file_content_matches(Path, Regex) ->
    case file:read_file(Path) of
        {ok, Bin} ->
            nomatch =/= re:run(Bin, Regex);
        {error, Reason} ->
            Reason
    end.
