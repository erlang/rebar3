%%% Packages rebar.hrl features and macros into a more generic API
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
abort() -> ?FAIL.

%% @doc like {@link error/2}, except it also raises an
%% exception to interrupt program flow.
-spec abort(string(), list()) -> no_return().
abort(Str, Args) -> ?ABORT(Str, Args).

%% @doc Prints to the console, including a newline
console(Str, Args) -> ?CONSOLE(Str, Args).

%% @doc logs with severity `debug'
debug(Str, Args) -> ?DEBUG(Str, Args).
%% @doc logs with severity `info'
info(Str, Args) -> ?INFO(Str, Args).
%% @doc logs with severity `warn'
warn(Str, Args) -> ?WARN(Str, Args).
%% @doc logs with severity `error'
error(Str, Args) -> ?ERROR(Str, Args).

%%
%% Given env. variable FOO we want to expand all references to
%% it in InStr. References can have two forms: $FOO and ${FOO}
%% The end of form $FOO is delimited with whitespace or eol
%%
expand_env_variable(InStr, VarName, RawVarValue) ->
    rebar_utils:expand_env_variable(InStr, VarName, RawVarValue).

get_arch() ->
    rebar_utils:get_arch().

wordsize() ->
    rebar_utils:wordsize().


%% Add deps to the code path
add_deps_to_path(State) ->
  code:add_pathsa(rebar_state:code_paths(State, all_deps)).

%% Revert to only having the beams necessary for running rebar3 and plugins in the path
restore_code_path(State) ->
  rebar_utils:cleanup_code_path(rebar_state:code_paths(State, default)).

processing_base_dir(State) ->
    rebar_dir:processing_base_dir(State).

ssl_opts(Url) ->
    rebar_pkg_resource:ssl_opts(Url).
