-module(rebar_state).

-export([new/0, new/1, new/2, new/3,
         get/2, get/3, set/3,
         command_args/1, command_args/2,

         set_skip_dir/2, is_skip_dir/2, reset_skip_dirs/1,
         create_logic_providers/2,

         project_apps/1, project_apps/2,

         deps_names/1,
         binary_deps/1, binary_deps/2,
         src_deps/1, src_deps/2,

         providers/1, providers/2, add_provider/2]).

-include("rebar.hrl").

-ifdef(namespaced_types).
%% dict:dict() exists starting from Erlang 17.
-type rebar_dict() :: dict:dict(term(), term()).
-else.
%% dict() has been obsoleted in Erlang 17 and deprecated in 18.
-type rebar_dict() :: dict().
-endif.

-record(state_t, {dir :: file:filename(),
                  opts = [] :: list(),
                  local_opts = [] :: list(),
                  config = new_globals() :: rebar_dict(),

                  envs = new_env() :: rebar_dict(),
                  command_args = [] :: list(),

                  src_deps = [] :: [rebar_app_info:t()],
                  binary_deps = [],
                  project_apps = [],

                  providers = [],
                  skip_dirs = new_skip_dirs() :: rebar_dict() }).

-export_type([t/0]).

-opaque t() :: #state_t{}.

-spec new() -> t().
new() ->
    #state_t{dir = rebar_utils:get_cwd()}.

-spec new(list()) -> t().
new(Config) when is_list(Config) ->
    #state_t { dir = rebar_utils:get_cwd(),
               opts = Config }.

-spec new(t(), list()) -> t().
new(ParentState=#state_t{}, Config) ->
    %% Load terms from rebar.config, if it exists
    Dir = rebar_utils:get_cwd(),
    new(ParentState, Config, Dir).

-spec new(t(), list(), file:name()) -> t().
new(ParentState, Config, Dir) ->
    _Opts = ParentState#state_t.opts,
    LocalOpts = case rebar_config:consult_file(?LOCK_FILE) of
                    {ok, [D]} ->
                        [{locks, D} | Config];
                    _ ->
                        Config
                end,

    ProviderModules = [],
    create_logic_providers(ProviderModules, ParentState#state_t{dir=Dir
                                                               ,opts=LocalOpts}).

get(State, Key) ->
    proplists:get_value(Key, State#state_t.opts).

get(State, Key, Default) ->
    proplists:get_value(Key, State#state_t.opts, Default).

set(State, Key, Value) ->
    Opts = proplists:delete(Key, State#state_t.opts),
    State#state_t { opts = [{Key, Value} | Opts] }.

set_skip_dir(State, Dir) ->
    OldSkipDirs = State#state_t.skip_dirs,
    NewSkipDirs = case is_skip_dir(State, Dir) of
                      false ->
                          ?DEBUG("Adding skip dir: ~s\n", [Dir]),
                          dict:store(Dir, true, OldSkipDirs);
                      true ->
                          OldSkipDirs
                  end,
    State#state_t{skip_dirs = NewSkipDirs}.

is_skip_dir(State, Dir) ->
    dict:is_key(Dir, State#state_t.skip_dirs).

reset_skip_dirs(State) ->
    State#state_t{skip_dirs = new_skip_dirs()}.

command_args(#state_t{command_args=CmdArgs}) ->
    CmdArgs.

command_args(State, CmdArgs) ->
    State#state_t{command_args=CmdArgs}.


deps_names(State) ->
    Deps = rebar_state:get(State, deps, []),
    lists:map(fun(Dep) when is_tuple(Dep) ->
                      ec_cnv:to_binary(element(1, Dep));
                 (Dep) when is_atom(Dep) ->
                      ec_cnv:to_binary(Dep)
              end, Deps).

binary_deps(#state_t{binary_deps=BinaryDeps}) ->
    BinaryDeps.

binary_deps(State=#state_t{binary_deps=BinaryDeps}, NewBinaryDeps) when is_list(BinaryDeps) ->
    State#state_t{binary_deps=NewBinaryDeps};
binary_deps(State=#state_t{binary_deps=BinaryDeps}, BinaryDep) ->
    State#state_t{binary_deps=[BinaryDep | BinaryDeps]}.

src_deps(#state_t{src_deps=SrcDeps}) ->
    SrcDeps.

src_deps(State=#state_t{src_deps=SrcDeps}, NewSrcDeps) when is_list(SrcDeps) ->
    State#state_t{src_deps=NewSrcDeps};
src_deps(State=#state_t{src_deps=SrcDeps}, SrcDep) ->
    State#state_t{src_deps=[SrcDep | SrcDeps]}.

project_apps(#state_t{project_apps=Apps}) ->
    Apps.

project_apps(State=#state_t{}, NewApps) when is_list(NewApps) ->
    State#state_t{project_apps=NewApps};
project_apps(State=#state_t{project_apps=Apps}, App) ->
    State#state_t{project_apps=[App | Apps]}.

providers(#state_t{providers=Providers}) ->
    Providers.

providers(State, NewProviders) ->
    State#state_t{providers=NewProviders}.

add_provider(State=#state_t{providers=Providers}, Provider) ->
    State#state_t{providers=[Provider | Providers]}.

create_logic_providers(ProviderModules, State0) ->
    lists:foldl(fun(ProviderMod, Acc) ->
                        {ok, State1} = rebar_provider:new(ProviderMod, Acc),
                        State1
                end, State0, ProviderModules).

%% ===================================================================
%% Internal functions
%% ===================================================================

new_globals() -> dict:new().

new_env() -> dict:new().

new_skip_dirs() -> dict:new().
