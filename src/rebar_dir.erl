-module(rebar_dir).

-export([base_dir/1,
         deps_dir/1,
         deps_dir/2,
         plugins_dir/1,
         lib_dirs/1,
         home_dir/0,
         global_config_dir/1,
         get_cwd/0,
         ensure_dir/1,
         src_dirs/1,
         ebin_dir/0,
         processing_base_dir/1,
         processing_base_dir/2]).

-include("rebar.hrl").

-spec base_dir(rebar_state:t()) -> file:filename_all().
base_dir(State) ->
    rebar_state:get(State, base_dir, ?DEFAULT_BASE_DIR).

-spec deps_dir(rebar_state:t()) -> file:filename_all().
deps_dir(State) ->
    filename:join(base_dir(State), rebar_state:get(State, deps_dir, ?DEFAULT_DEPS_DIR)).

-spec deps_dir(file:filename_all(), file:filename_all()) -> file:filename_all().
deps_dir(DepsDir, App) ->
    filename:join(DepsDir, App).

-spec plugins_dir(rebar_state:t()) -> file:filename_all().
plugins_dir(State) ->
    case lists:member(global, rebar_state:current_profiles(State)) of
        true ->
            filename:join([base_dir(State), global_config_dir(State), rebar_state:get(State, plugins_dir, ?DEFAULT_PLUGINS_DIR)]);
        false ->
            filename:join(base_dir(State), rebar_state:get(State, plugins_dir, ?DEFAULT_PLUGINS_DIR))
    end.

-spec lib_dirs(rebar_state:t()) -> file:filename_all().
lib_dirs(State) ->
    rebar_state:get(State, project_app_dirs, ?DEFAULT_PROJECT_APP_DIRS).

home_dir() ->
    {ok, [[Home]]} = init:get_argument(home),
    Home.

global_config_dir(State) ->
    Home = home_dir(),
    rebar_state:get(State, global_rebar_dir, filename:join(Home, ?CONFIG_DIR)).

get_cwd() ->
    {ok, Dir} = file:get_cwd(),
    Dir.

%% TODO: filelib:ensure_dir/1 corrected in R13B04. Remove when we drop
%% support for OTP releases older than R13B04.
ensure_dir(Path) ->
    case filelib:ensure_dir(Path) of
        ok ->
            ok;
        {error,eexist} ->
            ok;
        Error ->
            Error
    end.

-spec src_dirs([string()]) -> [file:filename(), ...].
src_dirs([]) ->
    ["src"];
src_dirs(SrcDirs) ->
    SrcDirs.

ebin_dir() ->
    filename:join(get_cwd(), "ebin").

processing_base_dir(State) ->
    Cwd = get_cwd(),
    processing_base_dir(State, Cwd).

processing_base_dir(State, Dir) ->
    AbsDir = filename:absname(Dir),
    AbsDir =:= rebar_state:get(State, base_dir).
