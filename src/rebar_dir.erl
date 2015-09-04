-module(rebar_dir).

-export([base_dir/1,
         profile_dir/2,
         deps_dir/1,
         deps_dir/2,
         root_dir/1,
         checkouts_dir/1,
         checkouts_dir/2,
         plugins_dir/1,
         lib_dirs/1,
         home_dir/0,
         global_config_dir/1,
         global_config/1,
         global_config/0,
         global_cache_dir/1,
         local_cache_dir/1,
         get_cwd/0,
         template_globals/1,
         template_dir/1,
         processing_base_dir/1,
         processing_base_dir/2,
         make_relative_path/2,
         src_dirs/1, src_dirs/2,
         extra_src_dirs/1, extra_src_dirs/2,
         all_src_dirs/1, all_src_dirs/3]).

-include("rebar.hrl").

-spec base_dir(rebar_state:t()) -> file:filename_all().
base_dir(State) ->
    profile_dir(State, rebar_state:current_profiles(State)).

-spec profile_dir(rebar_state:t(), [atom()]) -> file:filename_all().
profile_dir(State, Profiles) ->
    {BaseDir, ProfilesStrings} = case [ec_cnv:to_list(P) || P <- Profiles] of
        ["global" | _] -> {?MODULE:global_cache_dir(State), [""]};
        ["bootstrap", "default"] -> {rebar_state:get(State, base_dir, ?DEFAULT_BASE_DIR), ["default"]};
        ["default"] -> {rebar_state:get(State, base_dir, ?DEFAULT_BASE_DIR), ["default"]};
        %% drop `default` from the profile dir if it's implicit and reverse order
        %%  of profiles to match order passed to `as`
        ["default"|Rest] -> {rebar_state:get(State, base_dir, ?DEFAULT_BASE_DIR), Rest}
    end,
    ProfilesDir = string:join(ProfilesStrings, "+"),
    filename:join(BaseDir, ProfilesDir).

-spec deps_dir(rebar_state:t()) -> file:filename_all().
deps_dir(State) ->
    filename:join(base_dir(State), rebar_state:get(State, deps_dir, ?DEFAULT_DEPS_DIR)).

-spec deps_dir(file:filename_all(), file:filename_all()) -> file:filename_all().
deps_dir(DepsDir, App) ->
    filename:join(DepsDir, App).

root_dir(State) ->
    filename:absname(rebar_state:get(State, root_dir, ?DEFAULT_ROOT_DIR)).

-spec checkouts_dir(rebar_state:t()) -> file:filename_all().
checkouts_dir(State) ->
    filename:join(root_dir(State), rebar_state:get(State, checkouts_dir, ?DEFAULT_CHECKOUTS_DIR)).

-spec checkouts_dir(rebar_state:t(), file:filename_all()) -> file:filename_all().
checkouts_dir(State, App) ->
    filename:join(checkouts_dir(State), App).

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
    filename:join(global_config_dir(State), "rebar.config").

global_config() ->
    Home = home_dir(),
    filename:join([Home, ".config", "rebar3", "rebar.config"]).

global_cache_dir(State) ->
    Home = home_dir(),
    rebar_state:get(State, global_rebar_dir, filename:join([Home, ".cache", "rebar3"])).

local_cache_dir(Dir) ->
    filename:join(Dir, ".rebar3").

get_cwd() ->
    {ok, Dir} = file:get_cwd(),
    %% On windows cwd may return capital letter for drive,
    %% for example C:/foobar. But as said in http://www.erlang.org/doc/man/filename.html#join-1
    %% filename:join/1,2 anyway will convert drive-letter to lowercase, so we have to "internalize"
    %% cwd as soon as it possible.
    filename:join([Dir]).

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

-spec src_dirs(rebar_dict()) -> list(file:filename_all()).
src_dirs(Opts) -> src_dirs(Opts, []).

-spec src_dirs(rebar_dict(), list(file:filename_all())) -> list(file:filename_all()).
src_dirs(Opts, Default) ->
    ErlOpts = rebar_opts:erl_opts(Opts),
    Vs = proplists:get_all_values(src_dirs, ErlOpts),
    case lists:append([rebar_opts:get(Opts, src_dirs, []) | Vs]) of
        []   -> Default;
        Dirs -> Dirs
    end.

-spec extra_src_dirs(rebar_dict()) -> list(file:filename_all()).
extra_src_dirs(Opts) -> extra_src_dirs(Opts, []).

-spec extra_src_dirs(rebar_dict(), list(file:filename_all())) -> list(file:filename_all()).
extra_src_dirs(Opts, Default) ->
    ErlOpts = rebar_opts:erl_opts(Opts),
    Vs = proplists:get_all_values(extra_src_dirs, ErlOpts),
    case lists:append([rebar_opts:get(Opts, extra_src_dirs, []) | Vs]) of
        []   -> Default;
        Dirs -> Dirs
    end.

-spec all_src_dirs(rebar_dict()) -> list(file:filename_all()).
all_src_dirs(Opts) -> all_src_dirs(Opts, [], []).

-spec all_src_dirs(rebar_dict(), list(file:filename_all()), list(file:filename_all())) ->
    list(file:filename_all()).
all_src_dirs(Opts, SrcDefault, ExtraDefault) ->
    src_dirs(Opts, SrcDefault) ++ extra_src_dirs(Opts, ExtraDefault).
