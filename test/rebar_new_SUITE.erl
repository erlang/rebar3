%%% This suite can't run tests for built-in templates because
%%% they require being escriptize and we currently don't support
%%% this in here!
-module(rebar_new_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() -> [app].


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

app(Config) ->
    Name = ?config(name, Config),
    rebar_test_utils:run_and_check(
        Config, [],
        ["new", "test_app", Name, "author_name=some_name"],
        {ok, []}
    ),
    validate_files(
        Config, Name,
        [{"LICENSE", ["some_name", "anonymous@example.org"]},
         {"README.md", [Name]},
         {".gitignore", []},
         {"rebar.config", []},
         {filename:join(["src", Name++".app.src"]), [Name]},
         {filename:join(["src", Name++"_sup.erl"]), [Name]},
         {filename:join(["src", Name++"_app.erl"]), [Name]}
        ]).

validate_files(_Config, Name, Checks) ->
    [begin
        Path = filename:join([Name, File]),
        {ok, Bin} = file:read_file(Path),
        [{match, _} = re:run(Bin, Pattern, [multiline,global])
         || Pattern <- Patterns]
     end || {File, Patterns} <- Checks],
    ok.
