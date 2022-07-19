-module(rebar_env).

-export([create_env/1,
         create_env/2]).

-include("rebar.hrl").

%% @doc The following environment variables are exported when running
%% a hook (absolute paths):
%%
%% REBAR_DEPS_DIR          = rebar_dir:deps_dir/1
%% REBAR_BUILD_DIR         = rebar_dir:base_dir/1
%% REBAR_ROOT_DIR          = rebar_dir:root_dir/1
%% REBAR_CHECKOUTS_DIR     = rebar_dir:checkouts_dir/1
%% REBAR_CHECKOUTS_OUT_DIR = rebar_dir:checkouts_out_dir/1
%% REBAR_PLUGINS_DIR       = rebar_dir:plugins_dir/1
%% REBAR_GLOBAL_CONFIG_DIR = rebar_dir:global_config_dir/1
%% REBAR_GLOBAL_CACHE_DIR  = rebar_dir:global_cache_dir/1
%% REBAR_TEMPLATE_DIR      = rebar_dir:template_dir/1
%% REBAR_APP_DIRS          = rebar_dir:lib_dirs/1
%% REBAR_SRC_DIRS          = rebar_dir:src_dirs/1
%%
%% autoconf compatible variables
%% (see: http://www.gnu.org/software/autoconf/manual/autoconf.html#Erlang-Libraries):
%% ERLANG_ERTS_VER              = erlang:system_info(version)
%% ERLANG_ROOT_DIR              = code:root_dir/0
%% ERLANG_LIB_DIR_erl_interface = code:lib_dir(erl_interface)
%% ERLANG_LIB_VER_erl_interface = version part of path returned by code:lib_dir(erl_interface)
%% ERL                          = ERLANG_ROOT_DIR/bin/erl
%% ERLC                         = ERLANG_ROOT_DIR/bin/erl
%%

-spec create_env(rebar_state:t()) -> proplists:proplist().
create_env(State) ->
    Opts = rebar_state:opts(State),
    create_env(State, Opts).

-spec create_env(rebar_state:t(), rebar_dict()) -> proplists:proplist().
create_env(State, Opts) ->
    BaseDir = rebar_dir:base_dir(State),
    EnvVars = [
        {"REBAR_DEPS_DIR",          filename:absname(rebar_dir:deps_dir(State))},
        {"REBAR_BUILD_DIR",         filename:absname(rebar_dir:base_dir(State))},
        {"REBAR_ROOT_DIR",          filename:absname(rebar_dir:root_dir(State))},
        {"REBAR_CHECKOUTS_DIR",     filename:absname(rebar_dir:checkouts_dir(State))},
        {"REBAR_CHECKOUTS_OUT_DIR", filename:absname(rebar_dir:checkouts_out_dir(State))},
        {"REBAR_PLUGINS_DIR",       filename:absname(rebar_dir:plugins_dir(State))},
        {"REBAR_GLOBAL_CONFIG_DIR", filename:absname(rebar_dir:global_config_dir(State))},
        {"REBAR_GLOBAL_CACHE_DIR",  filename:absname(rebar_dir:global_cache_dir(Opts))},
        {"REBAR_TEMPLATE_DIR",      filename:absname(rebar_dir:template_dir(State))},
        {"REBAR_APP_DIRS",          join_dirs(BaseDir, rebar_dir:lib_dirs(State))},
        {"REBAR_SRC_DIRS",          join_dirs(BaseDir, rebar_dir:all_src_dirs(Opts))},
        {"ERLANG_ERTS_VER",         erlang:system_info(version)},
        {"ERLANG_ROOT_DIR",         code:root_dir()},
        {"ERL",                     filename:join([code:root_dir(), "bin", "erl"])},
        {"ERLC",                    filename:join([code:root_dir(), "bin", "erlc"])},
        {"ERLANG_ARCH"  ,           rebar_api:wordsize()},
        {"ERLANG_TARGET",           rebar_api:get_arch()}
    ],
    EInterfaceVars = create_erl_interface_env(),
    lists:append([EnvVars, EInterfaceVars]).

-spec create_erl_interface_env() -> list().
create_erl_interface_env() ->
    case code:lib_dir(erl_interface) of
        {error, bad_name} ->
            ?WARN("erl_interface is missing. ERLANG_LIB_DIR_erl_interface and "
            "ERLANG_LIB_VER_erl_interface will not be added to the environment.", []),
            [];
        Dir ->
            [
             {"ERLANG_LIB_DIR_erl_interface", Dir},
             {"ERLANG_LIB_VER_erl_interface", re_version(Dir)}
            ]
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

join_dirs(BaseDir, Dirs) ->
    rebar_string:join([filename:join(BaseDir, Dir) || Dir <- Dirs], ":").

re_version(Path) ->
    case re:run(Path, "^.*-(?<VER>[^/-]*)$", [{capture,[1],list}, unicode]) of
        nomatch -> "";
        {match, [Ver]} -> Ver
    end.
