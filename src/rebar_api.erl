%%% @doc Packages rebar.hrl features and macros into a more generic API
%%% that can be used by plugin builders.
-module(rebar_api).
-include("rebar.hrl").
-include_lib("providers/include/providers.hrl").
-export([abort/0, abort/2,
         console/2,
         debug/2, info/2, warn/2, error/2,
         expand_env_variable/3,
         get_arch/0,
         wordsize/0,
         set_paths/2,
         unset_paths/2,
         add_deps_to_path/1,
         restore_code_path/1,
         processing_base_dir/1,
         ssl_opts/1]).

-export_type([rebar_dict/0, rebar_digraph/0]).

%%%%%%%%%%%%%%%%%%%%%%%
%%% Error reporting %%%
%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Interrupts program flow
-spec abort() -> no_return().
abort() -> ?ABORT.

%% @doc like {@link error/2}, except it also raises an
%% exception to interrupt program flow.
-spec abort(string(), list()) -> no_return().
abort(Str, Args) -> ?ABORT(Str, Args).

%% @doc Prints to the console, including a newline
-spec console(string(), list()) -> ok.
console(Str, Args) -> ?CONSOLE(Str, Args).

%% @doc logs with severity `debug'
-spec debug(string(), list()) -> ok.
debug(Str, Args) -> ?DEBUG(Str, Args).

%% @doc logs with severity `info'
-spec info(string(), list()) -> ok.
info(Str, Args) -> ?INFO(Str, Args).

%% @doc logs with severity `warn'
-spec warn(string(), list()) -> ok.
warn(Str, Args) -> ?WARN(Str, Args).

%% @doc logs with severity `error'
-spec error(string(), list()) -> ok.
error(Str, Args) -> ?ERROR(Str, Args).

%% @doc Given env. variable `FOO' we want to expand all references to
%% it in `InStr'. References can have two forms: `$FOO' and `${FOO}'
%% The end of form `$FOO' is delimited with whitespace or EOL
-spec expand_env_variable(string(), string(), term()) -> string().
expand_env_variable(InStr, VarName, RawVarValue) ->
    rebar_utils:expand_env_variable(InStr, VarName, RawVarValue).

%% @doc returns the system architecture, in strings like
%% `"19.0.4-x86_64-unknown-linux-gnu-64"'.
-spec get_arch() -> string().
get_arch() ->
    rebar_utils:get_arch().

%% @doc returns the size of a word on the system, as a string
-spec wordsize() -> string().
wordsize() ->
    rebar_utils:wordsize().

%% @doc Set code paths. Takes arguments of the form
%% `[plugins, deps]' or `[deps, plugins]' and ensures the
%% project's app and dependencies are set in the right order
%% for the next bit of execution
-spec set_paths(rebar_paths:targets(), rebar_state:t()) -> ok.
set_paths(List, State) ->
    rebar_paths:set_paths(List, State).

%% @doc Unsets code paths. Takes arguments of the form
%% `[plugins, deps]' or `[deps, plugins]' and ensures the
%% paths are no longer active.
-spec unset_paths(rebar_paths:targets(), rebar_state:t()) -> ok.
unset_paths(List, State) ->
    rebar_paths:unset_paths(List, State).

%% @doc Add deps to the code path
-spec add_deps_to_path(rebar_state:t()) -> ok.
add_deps_to_path(State) ->
  code:add_pathsa(rebar_state:code_paths(State, all_deps)).

%% @doc Revert to only having the beams necessary for running rebar3 and
%% plugins in the path
-spec restore_code_path(rebar_state:t()) -> true | {error, term()}.
restore_code_path(State) ->
  rebar_utils:cleanup_code_path(rebar_state:code_paths(State, default)).

%% @doc checks if the current working directory is the base directory
%% for the project.
-spec processing_base_dir(rebar_state:t()) -> boolean().
processing_base_dir(State) ->
    rebar_dir:processing_base_dir(State).

%% @doc returns the SSL options adequate for the project based on
%% its configuration, including for validation of certs.
-spec ssl_opts(string() | binary()) -> [term()].
ssl_opts(Url) ->
    rebar_utils:ssl_opts(Url).
