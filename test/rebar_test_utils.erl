-module(rebar_test_utils).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-export([init_rebar_state/1, init_rebar_state/2, run_and_check/4]).
-export([create_app/4, create_empty_app/4, create_config/2]).
-export([create_random_name/1, create_random_vsn/0]).

%%%%%%%%%%%%%%
%%% Public %%%
%%%%%%%%%%%%%%

%% @doc {@see init_rebar_state/2}
init_rebar_state(Config) -> init_rebar_state(Config, "apps_dir1_").

%% @doc Takes a common test config and a name (string) and sets up
%% a basic OTP app directory with a pre-configured rebar state to
%% run tests with.
init_rebar_state(Config, Name) ->
    application:load(rebar),
    DataDir = ?config(priv_dir, Config),
    AppsDir = filename:join([DataDir, create_random_name(Name)]),
    ok = ec_file:mkdir_p(AppsDir),
    Verbosity = rebar3:log_level(),
    rebar_log:init(command_line, Verbosity),
    State = rebar_state:new([{base_dir, filename:join([AppsDir, "_build"])}]),
    [{apps, AppsDir}, {state, State} | Config].

%% @doc Takes common test config, a rebar config ([] if empty), a command to
%% run ("install_deps", "compile", etc.), and a list of expected applications
%% and/or dependencies to be present, and verifies whether they are all in
%% place.
%%
%% The expectation list takes elements of the form:
%% - `{app, Name :: string()}': checks that the app is properly built.
%% - `{dep, Name :: string()}': checks that the dependency has been fetched.
%%   Ignores the build status of the dependency.
%% - `{dep, Name :: string(), Vsn :: string()}': checks that the dependency
%%   has been fetched, and that a given version has been chosen. Useful to
%%   test for conflict resolution. Also ignores the build status of the
%%   dependency.
%%
%% This function assumes `init_rebar_state/1-2' has run before, in order to
%% fetch the `apps' and `state' values from the CT config.
run_and_check(Config, RebarConfig, Command, Expect) ->
    %% Assumes init_rebar_state has run first
    AppDir = ?config(apps, Config),
    State = ?config(state, Config),
    Res = rebar3:run(rebar_state:new(State, RebarConfig, AppDir), Command),
    case Expect of
        {error, Reason} ->
            ?assertEqual({error, Reason}, Res);
        {ok, Expected} ->
            {ok, _} = Res,
            check_results(AppDir, Expected)
    end.

%% @doc Creates a dummy application including:
%% - src/<file>.erl
%% - src/<file>.app.src
%% And returns a `rebar_app_info' object.
create_app(AppDir, Name, Vsn, Deps) ->
    write_src_file(AppDir, Name),
    write_app_src_file(AppDir, Name, Vsn, Deps),
    rebar_app_info:new(Name, Vsn, AppDir, Deps).

%% @doc Creates a dummy application including:
%% - ebin/<file>.app
%% And returns a `rebar_app_info' object.
create_empty_app(AppDir, Name, Vsn, Deps) ->
    write_app_file(AppDir, Name, Vsn, Deps),
    rebar_app_info:new(Name, Vsn, AppDir, Deps).

%% @doc Creates a rebar.config file. The function accepts a list of terms,
%% each of which will be dumped as a consult file. For example, the list
%% `[a, b, c]' will return the consult file `a. b. c.'.
create_config(AppDir, Contents) ->
    Conf = filename:join([AppDir, "rebar.config"]),
    ok = filelib:ensure_dir(Conf),
    Config = lists:flatten([io_lib:fwrite("~p.~n", [Term]) || Term <- Contents]),
    ok = ec_file:write(Conf, Config),
    Conf.

%% @doc Util to create a random variation of a given name.
create_random_name(Name) ->
    random:seed(erlang:now()),
    Name ++ erlang:integer_to_list(random:uniform(1000000)).

%% @doc Util to create a random variation of a given version.
create_random_vsn() ->
    random:seed(erlang:now()),
    lists:flatten([erlang:integer_to_list(random:uniform(100)),
                   ".", erlang:integer_to_list(random:uniform(100)),
                   ".", erlang:integer_to_list(random:uniform(100))]).

%%%%%%%%%%%%%%%
%%% Helpers %%%
%%%%%%%%%%%%%%%
check_results(AppDir, Expected) ->
    BuildDir = filename:join([AppDir, "_build", "default", "lib"]),
    Deps = rebar_app_discover:find_apps([BuildDir], all),
    DepsNames = [{ec_cnv:to_list(rebar_app_info:name(App)), App} || App <- Deps],
    lists:foreach(
        fun({app, Name}) ->
                [App] = rebar_app_discover:find_apps([AppDir]),
                ct:pal("Name: ~p", [Name]),
                ?assertEqual(Name, ec_cnv:to_list(rebar_app_info:name(App)))
        ;  ({dep, Name}) ->
                ct:pal("Name: ~p", [Name]),
                ?assertNotEqual(false, lists:keyfind(Name, 1, DepsNames))
        ;  ({dep, Name, Vsn}) ->
                ct:pal("Name: ~p, Vsn: ~p", [Name, Vsn]),
                case lists:keyfind(Name, 1, DepsNames) of
                    false ->
                        error({app_not_found, Name});
                    {Name, App} ->
                        ?assertEqual(iolist_to_binary(Vsn),
                                     iolist_to_binary(rebar_app_info:original_vsn(App)))
                end
        end, Expected).

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

erl_src_file(Name) ->
    io_lib:format("-module(~s).\n"
                 "-export([main/0]).\n"
                 "main() -> ok.\n", [filename:basename(Name, ".erl")]).

get_app_metadata(Name, Vsn, Deps) ->
    {application, erlang:list_to_atom(Name),
     [{description, ""},
      {vsn, Vsn},
      {modules, []},
      {included_applications, []},
      {registered, []},
      {applications, Deps}]}.

