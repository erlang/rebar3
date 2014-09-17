-module(rebar_compile_SUITE).

-export([suite/0,
         init_per_suite/1,
         end_per_suite/1,
         init_per_testcase/2,
         all/0,
         build_basic_app/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").
-include_lib("kernel/include/file.hrl").

suite() ->
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_, Config) ->
    DataDir = proplists:get_value(data_dir, Config),
    AppsDir = filename:join([DataDir, create_random_name("apps_dir1_")]),
    ok = ec_file:mkdir_p(AppsDir),
    Verbosity = rebar3:log_level([]),
    rebar_log:init(command_line, Verbosity),
    State = rebar_state:new(),
    [{apps, AppsDir}, {state, State} | Config].

all() ->
    [build_basic_app].

build_basic_app(Config) ->
    AppDir = proplists:get_value(apps, Config),
    State = proplists:get_value(state, Config),

    Name = create_random_name("app1_"),
    Vsn = create_random_vsn(),
    create_app(AppDir, Name, Vsn, [kernel, stdlib]),

    ConfigFile = filename:join([AppDir, "rebar.config"]),
    write_config(ConfigFile, []),
    rebar3:run(rebar_state:new(State, [], AppDir), ["compile"]),

    %% Verify app was built
    [App] = rebar_app_discover:find_apps([AppDir]),

    ?assertEqual(Name, ec_cnv:to_list(rebar_app_info:name(App))).

%%%===================================================================
%%% Helper Functions
%%%===================================================================

create_app(AppDir, Name, Vsn, Deps) ->
    write_src_file(AppDir, Name),
    write_app_src_file(AppDir, Name, Vsn, Deps),
    rebar_app_info:new(Name, Vsn, AppDir, Deps).

create_empty_app(AppDir, Name, Vsn, Deps) ->
    write_app_file(AppDir, Name, Vsn, Deps),
    rebar_app_info:new(Name, Vsn, AppDir, Deps).

write_beam_file(Dir, Name) ->
    Beam = filename:join([Dir, "ebin", "not_a_real_beam" ++ Name ++ ".beam"]),
    ok = filelib:ensure_dir(Beam),
    ok = ec_file:write_term(Beam, testing_purposes_only).

write_src_file(Dir, Name) ->
    Erl = filename:join([Dir, "src", "not_a_real_src" ++ Name ++ ".erl"]),
    ok = filelib:ensure_dir(Erl),
    ok = ec_file:write(Erl, erl_src_file("not_a_real_src" ++ Name ++ ".erl")).

write_app_file(Dir, Name, Version, Deps) ->
    Filename = filename:join([Dir, "ebin", Name ++ ".app"]),
    ok = filelib:ensure_dir(Filename),
    ok = ec_file:write_term(Filename, get_app_metadata(ec_cnv:to_list(Name), Version, Deps)).

write_app_src_file(Dir, Name, Version, Deps) ->
    Filename = filename:join([Dir, "src", Name ++ ".app.src"]),
    ok = filelib:ensure_dir(Filename),
    ok = ec_file:write_term(Filename, get_app_metadata(ec_cnv:to_list(Name), Version, Deps)).

get_app_metadata(Name, Vsn, Deps) ->
    {application, erlang:list_to_atom(Name),
     [{description, ""},
      {vsn, Vsn},
      {modules, []},
      {included_applications, []},
      {registered, []},
      {applications, Deps}]}.

create_random_name(Name) ->
    random:seed(erlang:now()),
    Name ++ erlang:integer_to_list(random:uniform(1000000)).

create_random_vsn() ->
    random:seed(erlang:now()),
    lists:flatten([erlang:integer_to_list(random:uniform(100)),
                   ".", erlang:integer_to_list(random:uniform(100)),
                   ".", erlang:integer_to_list(random:uniform(100))]).

write_config(Filename, Values) ->
    ok = filelib:ensure_dir(Filename),
    ok = ec_file:write(Filename,
                       [io_lib:format("~p.\n", [Val]) || Val <- Values]).

erl_src_file(Name) ->
    io_lib:format("-module(~s).\n"
                 "-export([main/0]).\n"
                 "main() -> ok.\n", [filename:basename(Name, ".erl")]).
