-module(rebar_state).

-export([new/0, new/1, new/2, new/3,
         get/2, get/3, set/3,
         command_args/1, command_args/2,

         dir/1, dir/2,
         create_logic_providers/2,

         project_apps/1, project_apps/2,

         deps_names/1,
         pkg_deps/1, pkg_deps/2,
         src_deps/1, src_deps/2,
         src_apps/1, src_apps/2,

         prepend_hook/3, append_hook/3, hooks/2,
         providers/1, providers/2, add_provider/2]).

-include("rebar.hrl").

-record(state_t, {dir :: file:name(),
                  opts = [],

                  command_args = [],

                  src_deps = [],
                  src_apps = [],
                  pkg_deps = [],
                  project_apps = [],

                  providers = [],
                  hooks = []}).


-export_type([t/0]).

-type t() :: record(state_t).

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
                    [D] ->
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

-spec set(t(), any(), any()) -> t().
set(State, Key, Value) ->
    Opts = proplists:delete(Key, State#state_t.opts),
    State#state_t { opts = [{Key, Value} | Opts] }.

command_args(#state_t{command_args=CmdArgs}) ->
    CmdArgs.

command_args(State, CmdArgs) ->
    State#state_t{command_args=CmdArgs}.

dir(#state_t{dir=Dir}) ->
    Dir.

dir(State=#state_t{}, Dir) ->
    State#state_t{dir=filename:absname(Dir)}.

deps_names(State) ->
    Deps = rebar_state:get(State, deps, []),
    lists:map(fun(Dep) when is_tuple(Dep) ->
                      ec_cnv:to_binary(element(1, Dep));
                 (Dep) when is_atom(Dep) ->
                      ec_cnv:to_binary(Dep)
              end, Deps).

pkg_deps(#state_t{pkg_deps=PkgDeps}) ->
    PkgDeps.

pkg_deps(State=#state_t{pkg_deps=PkgDeps}, NewPkgDeps) when is_list(PkgDeps) ->
    State#state_t{pkg_deps=NewPkgDeps};
pkg_deps(State=#state_t{pkg_deps=PkgDeps}, PkgDep) ->
    State#state_t{pkg_deps=[PkgDep | PkgDeps]}.

src_deps(#state_t{src_deps=SrcDeps}) ->
    SrcDeps.

src_deps(State=#state_t{src_deps=SrcDeps}, NewSrcDeps) when is_list(SrcDeps) ->
    State#state_t{src_deps=NewSrcDeps};
src_deps(State=#state_t{src_deps=SrcDeps}, SrcDep) ->
    Name = rebar_app_info:name(SrcDep),
    NewSrcDeps = lists:keystore(Name, 2, SrcDeps, SrcDep),
    State#state_t{src_deps=NewSrcDeps}.

src_apps(#state_t{src_apps=SrcApps}) ->
    SrcApps.

src_apps(State=#state_t{src_apps=_SrcApps}, NewSrcApps) when is_list(NewSrcApps) ->
    State#state_t{src_apps=NewSrcApps};
src_apps(State=#state_t{src_apps=SrcApps}, NewSrcApp) ->
    Name = rebar_app_info:name(NewSrcApp),
    NewSrcApps = lists:keystore(Name, 2, SrcApps, NewSrcApp),
    State#state_t{src_apps=NewSrcApps}.

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

-spec add_provider(t(), rebar_provider:t()) -> t().
add_provider(State=#state_t{providers=Providers}, Provider) ->
    State#state_t{providers=[Provider | Providers]}.

create_logic_providers(ProviderModules, State0) ->
    lists:foldl(fun(ProviderMod, Acc) ->
                        {ok, State1} = rebar_provider:new(ProviderMod, Acc),
                        State1
                end, State0, ProviderModules).

prepend_hook(State=#state_t{hooks=Hooks}, Target, Hook) ->
    {PreHooks, PostHooks} = proplists:get_value(Target, Hooks, {[], []}),
    State#state_t{hooks=[{Target, {[Hook | PreHooks], PostHooks}} | proplists:delete(Target, Hooks)]}.

append_hook(State=#state_t{hooks=Hooks}, Target, Hook) ->
    {PreHooks, PostHooks} = proplists:get_value(Target, Hooks, {[], []}),
    State#state_t{hooks=[{Target, {PreHooks, [Hook | PostHooks]}} | proplists:delete(Target, Hooks)]}.

-spec hooks(t(), atom()) -> {[rebar_provider:t()], [rebar_provider:t()]}.
hooks(#state_t{hooks=Hooks}, Target) ->
    proplists:get_value(Target, Hooks, {[], []}).

%% ===================================================================
%% Internal functions
%% ===================================================================
