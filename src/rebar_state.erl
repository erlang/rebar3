-module(rebar_state).

-export([new/0, new/1, new/2, new/3,
         get/2, get/3, set/3,

         opts/1,
         default/1, default/2,

         escript_path/1, escript_path/2,

         lock/1, lock/2,

         current_profile/1,
         current_profile/2,

         command_args/1, command_args/2,
         command_parsed_args/1, command_parsed_args/2,

         apply_profile/2,

         dir/1, dir/2,
         create_logic_providers/2,

         project_apps/1, project_apps/2,
         deps_to_build/1, deps_to_build/2,
         all_deps/1, all_deps/2,

         deps_names/1,

         prepend_hook/3, append_hook/3, hooks/2,
         providers/1, providers/2, add_provider/2]).

-include("rebar.hrl").

-record(state_t, {dir                               :: file:name(),
                  opts                = dict:new()  :: rebar_dict(),
                  default             = dict:new()  :: rebar_dict(),

                  escript_path                      :: undefined | file:filename_all(),

                  lock                = [],
                  current_profile     = default     :: atom(),

                  command_args        = [],
                  command_parsed_args = [],

                  project_apps        = []          :: [rebar_app_into:t()],
                  deps_to_build       = []          :: [rebar_app_into:t()],
                  all_deps            = []          :: [rebar_app_into:t()],

                  providers           = []}).

-export_type([t/0]).

-type t() :: record(state_t).

-spec new() -> t().
new() ->
    #state_t{dir = rebar_dir:get_cwd()}.

-spec new(list()) -> t().
new(Config) when is_list(Config) ->
    Opts = dict:from_list(Config),
    #state_t { dir = rebar_dir:get_cwd(),
               default = Opts,
               opts = Opts }.

-spec new(t() | atom(), list()) -> t().
new(Profile, Config) when is_atom(Profile)
                        , is_list(Config) ->
    Opts = dict:from_list(Config),
    #state_t { dir = rebar_dir:get_cwd(),
               current_profile = Profile,
               default = Opts,
               opts = Opts };
new(ParentState=#state_t{}, Config) ->
    %% Load terms from rebar.config, if it exists
    Dir = rebar_dir:get_cwd(),
    new(ParentState, Config, Dir).

-spec new(t(), list(), file:name()) -> t().
new(ParentState, Config, Dir) ->
    Opts = ParentState#state_t.opts,
    LocalOpts = case rebar_config:consult_file(?LOCK_FILE) of
                    [D] ->
                        dict:from_list([{locks, D} | Config]);
                    _ ->
                        dict:from_list(Config)
                end,
    NewOpts = dict:merge(fun(_Key, Value1, _Value2) ->
                                 Value1
                         end, LocalOpts, Opts),
    ProviderModules = [],
    create_logic_providers(ProviderModules
                          ,ParentState#state_t{dir=Dir
                                              ,opts=NewOpts
                                              ,default=NewOpts}).

get(State, Key) ->
    {ok, Value} = dict:find(Key, State#state_t.opts),
    Value.

get(State, Key, Default) ->
    case dict:find(Key, State#state_t.opts) of
        {ok, Value} ->
            Value;
        error ->
            Default
    end.

-spec set(t(), any(), any()) -> t().
set(State=#state_t{opts=Opts}, Key, Value) ->
    State#state_t{ opts = dict:store(Key, Value, Opts) }.

default(#state_t{default=Opts}) ->
    Opts.

default(State, Opts) ->
    State#state_t{default=Opts}.

opts(#state_t{opts=Opts}) ->
    Opts.

current_profile(#state_t{current_profile=Profile}) ->
    Profile.

current_profile(State, Profile) ->
    apply_profile(State#state_t{current_profile=Profile}, Profile).

lock(#state_t{lock=Lock}) ->
    Lock.

lock(State=#state_t{lock=Lock}, App) ->
    State#state_t{lock=[App | Lock]}.

escript_path(#state_t{escript_path=EscriptPath}) ->
    EscriptPath.

escript_path(State, EscriptPath) ->
    State#state_t{escript_path=EscriptPath}.

command_args(#state_t{command_args=CmdArgs}) ->
    CmdArgs.

command_args(State, CmdArgs) ->
    State#state_t{command_args=CmdArgs}.

command_parsed_args(#state_t{command_parsed_args=CmdArgs}) ->
    CmdArgs.

command_parsed_args(State, CmdArgs) ->
    State#state_t{command_parsed_args=CmdArgs}.

%% Only apply profiles to the default profile
apply_profile(State=#state_t{default=Opts}, default) ->
    Deps = rebar_state:get(State, deps, []),
    Opts1 = dict:store({deps, default}, Deps, Opts),
    State#state_t{opts=Opts1};
apply_profile(State=#state_t{default=Opts}, Profile) ->
    ConfigProfiles = rebar_state:get(State, profiles, []),
    Deps = rebar_state:get(State, deps, []),
    Opts1 = dict:store({deps, default}, Deps, Opts),
    ProfileOpts = dict:from_list(proplists:get_value(Profile, ConfigProfiles, [])),
    State#state_t{opts=merge_opts(Profile, ProfileOpts, Opts1)}.

merge_opts(Profile, NewOpts, OldOpts) ->
    Dict = dict:merge(fun(_Key, NewValue, OldValue) when is_list(NewValue) ->
                              case io_lib:printable_list(NewValue) of
                                  true ->
                                      NewValue;
                                  false ->
                                      lists:keymerge(1
                                                    ,lists:keysort(1, OldValue)
                                                    ,lists:keysort(1, NewValue))
                              end;
                         (_Key, NewValue, _OldValue) ->
                              NewValue
                      end, NewOpts, OldOpts),

    case dict:find(deps, NewOpts) of
        error ->
            dict:store({deps, Profile}, [], Dict);
        {ok, Deps} ->
            dict:store({deps, Profile}, Deps, Dict)
    end.


dir(#state_t{dir=Dir}) ->
    Dir.

dir(State=#state_t{}, Dir) ->
    State#state_t{dir=filename:absname(Dir)}.

deps_names(Deps) when is_list(Deps) ->
    lists:map(fun(Dep) when is_tuple(Dep) ->
                      ec_cnv:to_binary(element(1, Dep));
                 (Dep) when is_atom(Dep) ->
                      ec_cnv:to_binary(Dep)
              end, Deps);
deps_names(State) ->
    Deps = rebar_state:get(State, deps, []),
    deps_names(Deps).

project_apps(#state_t{project_apps=Apps}) ->
    Apps.

project_apps(State=#state_t{}, NewApps) when is_list(NewApps) ->
    State#state_t{project_apps=NewApps};
project_apps(State=#state_t{project_apps=Apps}, App) ->
    State#state_t{project_apps=lists:keystore(rebar_app_info:name(App), 2, Apps, App)}.

deps_to_build(#state_t{deps_to_build=Apps}) ->
    Apps.

deps_to_build(State=#state_t{}, NewApps) when is_list(NewApps) ->
    State#state_t{deps_to_build=NewApps};
deps_to_build(State=#state_t{deps_to_build=Apps}, App) ->
    State#state_t{deps_to_build=lists:keystore(rebar_app_info:name(App), 2, Apps, App)}.

all_deps(#state_t{all_deps=Apps}) ->
    Apps.

all_deps(State=#state_t{}, NewApps) ->
    State#state_t{all_deps=NewApps}.

providers(#state_t{providers=Providers}) ->
    Providers.

providers(State, NewProviders) ->
    State#state_t{providers=NewProviders}.

-spec add_provider(t(), providers:t()) -> t().
add_provider(State=#state_t{providers=Providers}, Provider) ->
    State#state_t{providers=[Provider | Providers]}.

create_logic_providers(ProviderModules, State0) ->
    lists:foldl(fun(ProviderMod, Acc) ->
                        case providers:new(ProviderMod, Acc) of
                            {error, Reason} ->
                                ?ERROR(Reason++"~n", []),
                                Acc;
                            {ok, State1} ->
                                State1
                        end
                end, State0, ProviderModules).

prepend_hook(State=#state_t{providers=Providers}, Target, Hook) ->
    State#state_t{providers=add_hook(pre, Providers, Target, Hook)}.

append_hook(State=#state_t{providers=Providers}, Target, Hook) ->
    State#state_t{providers=add_hook(post, Providers, Target, Hook)}.

-spec hooks(t(), atom()) -> {[providers:t()], [providers:t()]}.
hooks(_State=#state_t{providers=Providers}, Target) ->
    Provider = providers:get_provider(Target, Providers),
    providers:hooks(Provider).

%% ===================================================================
%% Internal functions
%% ===================================================================

add_hook(Which, Providers, Target, Hook) ->
    Provider = providers:get_provider(Target, Providers),
    Hooks = providers:hooks(Provider),
    NewHooks = add_hook(Which, Hooks, Hook),
    NewProvider = providers:hooks(Provider, NewHooks),
    [NewProvider | lists:delete(Provider, Providers)].

add_hook(pre, {PreHooks, PostHooks}, Hook) ->
    {[Hook | PreHooks], PostHooks};
add_hook(post, {PreHooks, PostHooks}, Hook) ->
    {PreHooks, [Hook | PostHooks]}.
