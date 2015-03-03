-module(rebar_dir).

-export([base_dir/1,
         deps_dir/1,
         deps_dir/2,
         plugins_dir/1,
         lib_dirs/1,
         home_dir/0,
         global_config_dir/1,
         global_config/1,
         global_config/0,
         global_cache_dir/1,
         local_cache_dir/0,
         get_cwd/0,
         template_globals/1,
         template_dir/1,
         processing_base_dir/1,
         processing_base_dir/2,
         make_relative_path/2]).

-include("rebar.hrl").

-spec base_dir(rebar_state:t()) -> file:filename_all().
base_dir(State) ->
    Profiles = rebar_state:current_profiles(State),
    ProfilesStrings = case [ec_cnv:to_list(P) || P <- Profiles] of
        ["default"]      -> ["default"];
        %% drop `default` from the profile dir if it's implicit and reverse order
        %%  of profiles to match order passed to `as`
        ["default"|Rest] -> lists:reverse(Rest)
    end,
    ProfilesDir = string:join(ProfilesStrings, "+"),
    filename:join(rebar_state:get(State, base_dir, ?DEFAULT_BASE_DIR), ProfilesDir).

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
    rebar_state:get(State, global_rebar_dir, filename:join([Home, ".config", "rebar3"])).

global_config(State) ->
    filename:join(global_config_dir(State), "config").

global_config() ->
    Home = home_dir(),
    filename:join([Home, ".config", "rebar3", "config"]).

global_cache_dir(State) ->
    Home = home_dir(),
    rebar_state:get(State, global_rebar_dir, filename:join([Home, ".cache", "rebar3"])).

local_cache_dir() ->
    filename:join(get_cwd(), ".rebar3").

get_cwd() ->
    {ok, Dir} = file:get_cwd(),
    Dir.

template_globals(State) ->
    filename:join([global_config_dir(State), "templates", "globals"]).

template_dir(State) ->
    filename:join([global_config_dir(State), "templates"]).

processing_base_dir(State) ->
    Cwd = get_cwd(),
    processing_base_dir(State, Cwd).

processing_base_dir(State, Dir) ->
    AbsDir = filename:absname(Dir),
    AbsDir =:= rebar_state:get(State, base_dir).

make_relative_path(Source, Target) ->
    do_make_relative_path(filename:split(Source), filename:split(Target)).

do_make_relative_path([H|T1], [H|T2]) ->
    do_make_relative_path(T1, T2);
do_make_relative_path(Source, Target) ->
    Base = lists:duplicate(max(length(Target) - 1, 0), ".."),
    filename:join(Base ++ Source).
