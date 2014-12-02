-module(rebar_dir).

-export([base_dir/1,
         deps_dir/1,
         deps_dir/2,
         plugins_dir/1,
         lib_dirs/1,
         profile_dir/1,
         default_deps_dir/1,
         default_profile_dir/1,
         default_profile_deps/1,
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
    DepsDir = rebar_state:get(State, deps_dir, ?DEFAULT_DEPS_DIR),
    filename:join(profile_dir(State), DepsDir).

-spec deps_dir(file:filename_all(), file:filename_all()) -> file:filename_all().
deps_dir(DepsDir, App) ->
    filename:join(DepsDir, App).

-spec default_deps_dir(rebar_state:t()) -> file:filename_all().
default_deps_dir(State) ->
    DepsDir = rebar_state:get(State, deps_dir, ?DEFAULT_DEPS_DIR),
    filename:join([base_dir(State), "default", DepsDir]).

-spec plugins_dir(rebar_state:t()) -> file:filename_all().
plugins_dir(State) ->
    filename:join(base_dir(State), rebar_state:get(State, plugins_dir, ?DEFAULT_PLUGINS_DIR)).

-spec lib_dirs(rebar_state:t()) -> file:filename_all().
lib_dirs(State) ->
    rebar_state:get(State, lib_dirs, ?DEFAULT_LIB_DIRS).

-spec default_profile_dir(rebar_state:t()) -> file:filename_all().
default_profile_dir(State) ->
    filename:join(base_dir(State), "default").

profile_dir(State) ->
    case rebar_state:current_profile(State) of
        global ->
            global_config_dir(State);
        Profile ->
            filename:join(base_dir(State), atom_to_list(Profile))
    end.

-spec default_profile_deps(rebar_state:t()) -> file:filename_all().
default_profile_deps(State) ->
    filename:join(default_profile_dir(State), ?DEFAULT_DEPS_DIR).

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
