%%% This suite can't run tests for built-in templates because
%%% they require being escriptize and we currently don't support
%%% this in here!
-module(rebar_new_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() -> [app_git_user, app_hg_user, app_with_fallbacks,
          app_with_flags1, app_with_flags2, plugin_tpl, unknown_template].


init_per_testcase(plugin_tpl, Config) ->
    application:load(rebar),
    DataDir = ?config(data_dir, Config),
    PrivDir = ?config(priv_dir, Config),
    Name = rebar_test_utils:create_random_name("plugin_tpl"),
    AppsDir = filename:join([PrivDir, rebar_test_utils:create_random_name(Name)]),
    ec_file:copy(filename:join([DataDir, "plugin_tpl"]), AppsDir, [recursive]),
    Verbosity = rebar3:log_level(),
    rebar_log:init(command_line, Verbosity),
    GlobalDir = filename:join([DataDir, "cache"]),
    State = rebar_state:new([{base_dir, filename:join([AppsDir, "_build"])}
                            ,{global_rebar_dir, GlobalDir}
                            ,{root_dir, AppsDir}]),
    mock_home_dir(DataDir),
    mock_empty_escript_templates(),
    [{apps, AppsDir}, {state, State}, {name, Name} | Config];
init_per_testcase(Case, Config0) ->
    Config = rebar_test_utils:init_rebar_state(Config0),
    Name = rebar_test_utils:create_random_name(atom_to_list(Case)),
    Data = ?config(data_dir, Config),
    mock_home_dir(Data),
    mock_empty_escript_templates(),
    [{name, Name} | Config].

end_per_testcase(_, Config) ->
    meck:unload(),
    Config.

mock_home_dir(Path) ->
    meck:new(rebar_dir, [passthrough]),
    meck:expect(rebar_dir, template_dir, fun(_) -> Path end).

mock_empty_escript_templates() ->
    %% Can't find escript templates unless we run
    %% from the escript, which obviously crashes these here tests.
    meck:new(rebar_utils, [passthrough]),
    meck:expect(rebar_utils, escript_foldl, fun(_,_,_) -> {ok, []} end).

app_git_user(Config) ->
    meck:expect(rebar_utils, sh, fun("git config --global user.name", _) -> {ok, "gitname"};
                                    ("git config --global user.email", _) -> {ok, "git@email.com"}
                                 end),

    Name = ?config(name, Config),
    rebar_test_utils:run_and_check(
        Config, [],
        ["new", "test_app", Name, "author_name=<some_name>"],
        {ok, []}
    ),
    validate_files(
        Config, Name,
        [{"LICENSE", ["<some_name>", "git@email.com"]},
         {"README.md", [Name]},
         {".gitignore", []},
         {"rebar.config", []},
         {filename:join(["src", Name++".app.src"]), [Name]},
         {filename:join(["src", Name++"_sup.erl"]), [Name]},
         {filename:join(["src", Name++"_app.erl"]), [Name]}
        ]).

app_with_fallbacks(Config) ->
    meck:expect(rebar_utils, sh, fun(_, _) -> {error, fallback} end),

    Name = ?config(name, Config),
    rebar_test_utils:run_and_check(
      Config, [],
      ["new", "test_app", Name],
      {ok, []}
     ),
    validate_files(
      Config, Name,
      [{"LICENSE", ["Anonymous", "anonymous@example.org"]},
       {"README.md", [Name]},
       {".gitignore", []},
       {"rebar.config", []},
       {filename:join(["src", Name++".app.src"]), [Name]},
       {filename:join(["src", Name++"_sup.erl"]), [Name]},
       {filename:join(["src", Name++"_app.erl"]), [Name]}
      ]).

app_hg_user(Config) ->
    meck:expect(rebar_utils, sh, fun("hg showconfig ui.username", _) -> {ok, "hgname <hg@email.com>"};
                                    (_, _) -> {error, fallback}
                                 end),

    Name = ?config(name, Config),
    rebar_test_utils:run_and_check(
      Config, [],
      ["new", "test_app", Name],
      {ok, []}
     ),
    validate_files(
      Config, Name,
      [{"LICENSE", ["hgname", "hg@email.com"]},
       {"README.md", [Name]},
       {".gitignore", []},
       {"rebar.config", []},
       {filename:join(["src", Name++".app.src"]), [Name]},
       {filename:join(["src", Name++"_sup.erl"]), [Name]},
       {filename:join(["src", Name++"_app.erl"]), [Name]}
      ]).

app_with_flags1(Config) ->
    Name = ?config(name, Config),
    rebar_test_utils:run_and_check(
      Config, [],
      ["new", "test_app", "-f", Name],
      {ok, []}
     ),
    validate_files(
      Config, Name,
      [{"LICENSE", []},
       {"README.md", []},
       {".gitignore", []},
       {"rebar.config", []},
       {filename:join(["src", Name++".app.src"]), [Name]},
       {filename:join(["src", Name++"_sup.erl"]), [Name]},
       {filename:join(["src", Name++"_app.erl"]), [Name]}
      ]).

app_with_flags2(Config) ->
    Name = ?config(name, Config),
    rebar_test_utils:run_and_check(
      Config, [],
      ["new", "-f", "test_app", Name],
      {ok, []}
     ),
    validate_files(
      Config, Name,
      [{"LICENSE", []},
       {"README.md", []},
       {".gitignore", []},
       {"rebar.config", []},
       {filename:join(["src", Name++".app.src"]), [Name]},
       {filename:join(["src", Name++"_sup.erl"]), [Name]},
       {filename:join(["src", Name++"_app.erl"]), [Name]}
      ]).

plugin_tpl(Config) ->
    Name = ?config(name, Config),
    rebar_test_utils:run_and_check(
      Config, [],
      ["new", "-f", "tpl", Name],
      {ok, []}
     ),
    Result = filename:join(["src", Name++".erl"]), % In CWD
    {ok, Bin} = file:read_file(Result),
    {match, _} = re:run(Bin, Name, [multiline,global]).

unknown_template(Config) ->
    Name = float_to_list(rand:uniform()),
    rebar_test_utils:run_and_check(Config, [], ["new", Name],
                                   {error, {rebar_prv_new, {template_not_found, Name}}}),
    rebar_test_utils:run_and_check(Config, [], ["new", "help", Name],
                                   {error, {rebar_prv_new, {template_not_found, Name}}}),
    ok.

validate_files(_Config, Name, Checks) ->
    [begin
        Path = filename:join([Name, File]),
        ct:pal("validating ~s for content", [Path]),
        {ok, Bin} = file:read_file(Path),
        [{match, _} = re:run(Bin, Pattern, [multiline,global])
         || Pattern <- Patterns]
     end || {File, Patterns} <- Checks],
    ok.
