-module(rebar_state).

-export([new/0, new/1, new/2, new/3,

         get/2, get/3, set/3,

         format_error/1,

         has_all_artifacts/1,

         code_paths/2, code_paths/3, update_code_paths/3,

         opts/1, opts/2,
         default/1, default/2,

         escript_path/1, escript_path/2,

         lock/1, lock/2,

         current_profiles/1, current_profiles/2,

         command_args/1, command_args/2,
         command_parsed_args/1, command_parsed_args/2,

         add_to_profile/3, apply_profiles/2,

         dir/1, dir/2,
         create_logic_providers/2,

         current_app/1, current_app/2,
         project_apps/1, project_apps/2,
         deps_to_build/1, deps_to_build/2,
         all_plugin_deps/1, all_plugin_deps/2, update_all_plugin_deps/2,
         all_deps/1, all_deps/2, update_all_deps/2, merge_all_deps/2,
         all_checkout_deps/1,
         namespace/1, namespace/2,

         default_hex_repo_url_override/1,

         deps_names/1,

         to_list/1,

         compilers/1, compilers/2,
         prepend_compilers/2, append_compilers/2,

         project_builders/1, add_project_builder/3,

         create_resources/2, set_resources/2,
         resources/1, resources/2, add_resource/2,
         providers/1, providers/2, add_provider/2,
         allow_provider_overrides/1, allow_provider_overrides/2
        ]).

-include("rebar.hrl").
-include_lib("providers/include/providers.hrl").

-record(state_t, {dir                               :: file:name(),
                  opts                = dict:new()  :: rebar_dict(),
                  code_paths          = dict:new()  :: rebar_dict(),
                  default             = dict:new()  :: rebar_dict(),
                  escript_path                      :: undefined | file:filename_all(),

                  lock                = [],
                  current_profiles    = [default]   :: [atom()],
                  namespace           = default     :: atom(),

                  command_args        = [],
                  command_parsed_args = {[], []},

                  current_app                       :: undefined | rebar_app_info:t(),
                  project_apps        = []          :: [rebar_app_info:t()],
                  deps_to_build       = []          :: [rebar_app_info:t()],
                  all_plugin_deps     = []          :: [rebar_app_info:t()],
                  all_deps            = []          :: [rebar_app_info:t()],

                  compilers           = []          :: [module()],
                  project_builders    = []          :: [{rebar_app_info:project_type(), module()}],
                  resources           = [],
                  providers           = [],
                  allow_provider_overrides = false  :: boolean()}).

-export_type([t/0]).

-type t() :: #state_t{}.

-spec new() -> t().
new() ->
    BaseState = base_state(dict:new()),
    BaseState#state_t{dir = rebar_dir:get_cwd()}.

-spec new(list()) -> t().
new(Config) when is_list(Config) ->
    Opts = base_opts(Config),
    BaseState = base_state(Opts),
    BaseState#state_t{dir=rebar_dir:get_cwd(),
                      default=Opts}.

-spec new(t() | atom(), list()) -> t().
new(Profile, Config) when is_atom(Profile)
                        , is_list(Config) ->
    Opts = base_opts(Config),
    BaseState = base_state(Opts),
    BaseState#state_t{dir = rebar_dir:get_cwd(),
                      current_profiles = [Profile],
                      default = Opts};
new(ParentState=#state_t{}, Config) ->
    %% Load terms from rebar.config, if it exists
    Dir = rebar_dir:get_cwd(),
    new(ParentState, Config, Dir).

-spec new(t(), list(), file:filename_all()) -> t().
new(ParentState, Config, Dir) ->
    new(ParentState, Config, deps_from_config(Dir, Config), Dir).

new(ParentState, Config, Deps, Dir) ->
    Opts = ParentState#state_t.opts,
    Plugins = proplists:get_value(plugins, Config, []),
    ProjectPlugins = proplists:get_value(project_plugins, Config, []),
    Terms = Deps++[{{project_plugins, default}, ProjectPlugins}, {{plugins, default}, Plugins} | Config],
    true = rebar_config:verify_config_format(Terms),
    LocalOpts = dict:from_list(Terms),

    NewOpts = rebar_opts:merge_opts(LocalOpts, Opts),

    ParentState#state_t{dir=Dir
                       ,opts=NewOpts
                       ,default=NewOpts}.

deps_from_config(Dir, Config) ->
    case rebar_config:consult_lock_file(filename:join(Dir, ?LOCK_FILE)) of
        [] ->
            [{{deps, default}, proplists:get_value(deps, Config, [])}];
        D ->
            %% We want the top level deps only from the lock file.
            %% This ensures deterministic overrides for configs.
            Deps = [X || X <- D, element(3, X) =:= 0],
            [{{locks, default}, D}, {{deps, default}, Deps}]
    end.

base_state(Opts) ->
    #state_t{opts=Opts}.

base_opts(Config) ->
    Deps = proplists:get_value(deps, Config, []),
    Plugins = proplists:get_value(plugins, Config, []),
    ProjectPlugins = proplists:get_value(project_plugins, Config, []),
    Terms = [{{deps, default}, Deps}, {{plugins, default}, Plugins},
             {{project_plugins, default}, ProjectPlugins} | Config],
    true = rebar_config:verify_config_format(Terms),
    dict:from_list(Terms).

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

format_error({profile_not_list, Profile, Other}) ->
    io_lib:format("Profile config must be a list but for profile '~p' config given as:~n~p", [Profile, Other]).

-spec has_all_artifacts(#state_t{}) -> true | {false, file:filename()}.
has_all_artifacts(State) ->
    Artifacts = rebar_state:get(State, artifacts, []),
    Dir = rebar_dir:base_dir(State),
    all(Dir, Artifacts).

all(_, []) ->
    true;
all(Dir, [File|Artifacts]) ->
    case filelib:is_regular(filename:join(Dir, File)) of
        false ->
            ?DEBUG("Missing artifact ~ts", [filename:join(Dir, File)]),
            {false, File};
        true ->
            all(Dir, Artifacts)
    end.

-spec code_paths(#state_t{}, atom()) -> [file:filename()].
code_paths(#state_t{code_paths=CodePaths}, Key) ->
    case dict:find(Key, CodePaths) of
        {ok, CodePath} ->
            CodePath;
        _ ->
            []
    end.

-spec code_paths(#state_t{}, atom(), [file:filename()]) -> #state_t{}.
code_paths(State=#state_t{code_paths=CodePaths}, Key, CodePath) ->
    State#state_t{code_paths=dict:store(Key, CodePath, CodePaths)}.

-spec update_code_paths(#state_t{}, atom(), [file:filename()]) -> #state_t{}.
update_code_paths(State=#state_t{code_paths=CodePaths}, Key, CodePath) ->
    case dict:is_key(Key, CodePaths) of
        true ->
            State#state_t{code_paths=dict:append_list(Key, CodePath, CodePaths)};
        false ->
            State#state_t{code_paths=dict:store(Key, CodePath, CodePaths)}
    end.

opts(#state_t{opts=Opts}) ->
    Opts.

opts(State, Opts) ->
    State#state_t{opts=Opts}.

current_profiles(#state_t{current_profiles=Profiles}) ->
    Profiles.

current_profiles(State, Profiles) ->
    State#state_t{current_profiles=Profiles}.

lock(#state_t{lock=Lock}) ->
    Lock.

lock(State=#state_t{}, Apps) when is_list(Apps) ->
    State#state_t{lock=Apps};
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

add_to_profile(State, Profile, KVs) when is_atom(Profile), is_list(KVs) ->
    Opts = rebar_opts:add_to_profile(opts(State), Profile, KVs),
    State#state_t{opts=Opts}.

apply_profiles(State, Profile) when not is_list(Profile) ->
    apply_profiles(State, [Profile]);
apply_profiles(State, [default]) ->
    State;
apply_profiles(State=#state_t{default = Defaults, current_profiles=CurrentProfiles}, Profiles) ->
    ProvidedProfiles = lists:prefix([default|Profiles], CurrentProfiles),
    AppliedProfiles = case Profiles of
                          %% Head of list global profile is special, only for use by rebar3
                          %% It does not clash if a user does `rebar3 as global...` but when
                          %% it is the head we must make sure not to prepend `default`
                          [global | _] ->
                              Profiles;
                          _ when ProvidedProfiles ->
                              deduplicate(CurrentProfiles);
                          _ ->
                              deduplicate(CurrentProfiles ++ Profiles)
                      end,

    ConfigProfiles = rebar_state:get(State, profiles, []),

    NewOpts =
        lists:foldl(fun(default, OptsAcc) ->
                            OptsAcc;
                       (Profile, OptsAcc) ->
                            case proplists:get_value(Profile, ConfigProfiles, []) of
                                OptsList when is_list(OptsList) ->
                                    ProfileOpts = dict:from_list(OptsList),
                                    rebar_opts:merge_opts(Profile, ProfileOpts, OptsAcc);
                                Other ->
                                    throw(?PRV_ERROR({profile_not_list, Profile, Other}))
                            end
                    end, Defaults, AppliedProfiles),
    State#state_t{current_profiles = AppliedProfiles, opts=NewOpts}.

deduplicate(Profiles) ->
    do_deduplicate(lists:reverse(Profiles), []).

do_deduplicate([], Acc) ->
    Acc;
do_deduplicate([Head | Rest], Acc) ->
    case lists:member(Head, Acc) of
        true -> do_deduplicate(Rest, Acc);
        false -> do_deduplicate(Rest, [Head | Acc])
    end.

dir(#state_t{dir=Dir}) ->
    Dir.

dir(State=#state_t{}, Dir) ->
    State#state_t{dir=filename:absname(Dir)}.

deps_names(Deps) when is_list(Deps) ->
    lists:map(fun(Dep) when is_tuple(Dep) ->
                      rebar_utils:to_binary(element(1, Dep));
                 (Dep) when is_atom(Dep) ->
                      rebar_utils:to_binary(Dep)
              end, Deps);
deps_names(State) ->
    Deps = rebar_state:get(State, deps, []),
    deps_names(Deps).

current_app(#state_t{current_app=CurrentApp}) ->
    CurrentApp.

current_app(State=#state_t{}, CurrentApp) ->
    State#state_t{current_app=CurrentApp}.

project_apps(#state_t{project_apps=Apps}) ->
    Apps.

project_apps(State=#state_t{}, NewApps) when is_list(NewApps) ->
    State#state_t{project_apps=NewApps};
project_apps(State=#state_t{project_apps=Apps}, App) ->
    State#state_t{project_apps=lists:keystore(rebar_app_info:name(App), 2, Apps, App)}.

deps_to_build(#state_t{deps_to_build=Apps}) ->
    Apps.

deps_to_build(State=#state_t{deps_to_build=Apps}, NewApps) when is_list(NewApps) ->
    State#state_t{deps_to_build=Apps++NewApps};
deps_to_build(State=#state_t{deps_to_build=Apps}, App) ->
    State#state_t{deps_to_build=lists:keystore(rebar_app_info:name(App), 2, Apps, App)}.

all_deps(#state_t{all_deps=Apps}) ->
    Apps.

all_deps(State=#state_t{}, NewApps) ->
    State#state_t{all_deps=NewApps}.

all_checkout_deps(#state_t{all_deps=Apps}) ->
    [App || App <- Apps, rebar_app_info:is_checkout(App)].

all_plugin_deps(#state_t{all_plugin_deps=Apps}) ->
    Apps.

all_plugin_deps(State=#state_t{}, NewApps) ->
    State#state_t{all_plugin_deps=NewApps}.

update_all_plugin_deps(State=#state_t{all_plugin_deps=Apps}, NewApps) ->
    State#state_t{all_plugin_deps=Apps++NewApps}.

update_all_deps(State=#state_t{all_deps=Apps}, NewApps) ->
    State#state_t{all_deps=Apps++NewApps}.

merge_all_deps(State=#state_t{all_deps=Apps}, UpdatedApps) when is_list(UpdatedApps) ->
    State#state_t{all_deps=lists:ukeymerge(2, lists:keysort(2, UpdatedApps), lists:keysort(2, Apps))}.

namespace(#state_t{namespace=Namespace}) ->
    Namespace.

namespace(State=#state_t{}, Namespace) ->
    State#state_t{namespace=Namespace}.

-spec resources(t()) -> [{rebar_resource_v2:type(), module()}].
resources(#state_t{resources=Resources}) ->
    Resources.

-spec set_resources(t(), [{rebar_resource_v2:type(), module()}]) -> t().
set_resources(State, Resources) ->
    State#state_t{resources=Resources}.

-spec resources(t(), [{rebar_resource_v2:type(), module()}]) -> t().
resources(State, NewResources) ->
    lists:foldl(fun(Resource, StateAcc) ->
                        add_resource(StateAcc, Resource)
                end, State, NewResources).

-spec add_resource(t(), {rebar_resource_v2:type(), module()}) -> t().
add_resource(State=#state_t{resources=Resources}, {ResourceType, ResourceModule}) ->
    _ = code:ensure_loaded(ResourceModule),
    Resource = case erlang:function_exported(ResourceModule, init, 2) of
                   true ->
                       case ResourceModule:init(ResourceType, State) of
                           {ok, R=#resource{}} ->
                               R;
                           _ ->
                               %% init didn't return a resource
                               %% must be an old resource
                               warn_old_resource(ResourceModule),
                               rebar_resource:new(ResourceType,
                                                  ResourceModule,
                                                  #{})
                       end;
                   false ->
                       %% no init, must be initial implementation
                       warn_old_resource(ResourceModule),
                       rebar_resource:new(ResourceType,
                                          ResourceModule,
                                          #{})
               end,
    State#state_t{resources=[Resource | Resources]}.

warn_old_resource(ResourceModule) ->
    ?WARN("Using custom resource ~s that implements a deprecated api. "
          "It should be upgraded to rebar_resource_v2.", [ResourceModule]).

%% @doc get a list of all registered compiler modules, which should implement
%% the `rebar_compiler' behaviour
-spec compilers(t()) -> [module()].
compilers(#state_t{compilers=Compilers}) ->
    Compilers.

%% @doc register compiler modules prior to the existing ones. Each compiler
%% module should implement the `rebar_compiler' behaviour. Use this when
%% your custom compiler generates .erl files (or files of another type) and
%% that should run before other compiler modules.
-spec prepend_compilers(t(), [module()]) -> t().
prepend_compilers(State=#state_t{compilers=Compilers}, NewCompilers) ->
    State#state_t{compilers=NewCompilers++Compilers}.

%% @doc register compiler modules. Each compiler
%% module should implement the `rebar_compiler' behaviour. Use this when
%% your custom compiler generates binary artifacts and does not have
%% a particular need to run before any other compiler.
-spec append_compilers(t(), [module()]) -> t().
append_compilers(State=#state_t{compilers=Compilers}, NewCompilers) ->
    State#state_t{compilers=Compilers++NewCompilers}.

%% @private reset all compiler modules by replacing them by a list of
%% modules that implement the `rebar_compiler' behaviour.
-spec compilers(t(), [module()]) -> t().
compilers(State, Compilers) ->
    State#state_t{compilers=Compilers}.

project_builders(#state_t{project_builders=ProjectBuilders}) ->
    ProjectBuilders.

add_project_builder(State=#state_t{project_builders=ProjectBuilders}, Type, Module) ->
    _ = code:ensure_loaded(Module),
    case erlang:function_exported(Module, build, 1) of
        true ->
            State#state_t{project_builders=[{Type, Module} | ProjectBuilders]};
        false ->
            ?WARN("Unable to add project builder for type ~s, required function ~s:build/1 not found.",
                  [Type, Module]),
            State
    end.

create_resources(Resources, State) ->
    lists:foldl(fun(R, StateAcc) ->
                        add_resource(StateAcc, R)
                end, State, Resources).

providers(#state_t{providers=Providers}) ->
    Providers.

providers(State, NewProviders) ->
    State#state_t{providers=NewProviders}.

allow_provider_overrides(#state_t{allow_provider_overrides=Allow}) ->
    Allow.

allow_provider_overrides(State, Allow) ->
    State#state_t{allow_provider_overrides=Allow}.

-spec add_provider(t(), providers:t()) -> t().
add_provider(State=#state_t{providers=Providers, allow_provider_overrides=true}, Provider) ->
    State#state_t{providers=[Provider | Providers]};
add_provider(State=#state_t{providers=Providers, allow_provider_overrides=false}, Provider) ->
    Name = providers:impl(Provider),
    Namespace = providers:namespace(Provider),
    Module = providers:module(Provider),
    case lists:any(fun(P) ->
                           case {providers:impl(P), providers:namespace(P)} of
                               {Name, Namespace} ->
                                   ?DEBUG("Not adding provider ~ts ~ts from module ~ts because it already exists from module ~ts",
                                          [atom_to_list(Namespace), atom_to_list(Name),
                                           atom_to_list(Module), atom_to_list(providers:module(P))]),
                                   true;
                               _ ->
                                   false
                           end
                   end, Providers) of
        true ->
            State;
        false ->
            State#state_t{providers=[Provider | Providers]}
    end.

-spec default_hex_repo_url_override(t()) -> binary().
default_hex_repo_url_override(State) ->
    CDN = rebar_state:get(State, rebar_packages_cdn, ?DEFAULT_CDN),
    rebar_utils:to_binary(CDN).

-dialyzer({no_match, create_logic_providers/2}). % we want to be permissive with providers:new/2
create_logic_providers(ProviderModules, State0) ->
    try
        lists:foldl(fun(ProviderMod, StateAcc) ->
                            case providers:new(ProviderMod, StateAcc) of
                                {error, {Mod, Error}} ->
                                    ?WARN("~ts", [Mod:format_error(Error)]),
                                    StateAcc;
                                {error, Reason} ->
                                    ?WARN(Reason++"~n", []),
                                    StateAcc;
                                {ok, StateAcc1} ->
                                    StateAcc1
                            end
                    end, State0, ProviderModules)
    catch
        ?WITH_STACKTRACE(C,T,S)
            ?DEBUG("~p: ~p ~p", [C, T, S]),
            ?CRASHDUMP("~p: ~p~n~p~n~n~p", [C, T, S, State0]),
            throw({error, "Failed creating providers. Run with DIAGNOSTIC=1 for stacktrace or consult rebar3.crashdump."})
    end.

to_list(#state_t{} = State) ->
    Fields = record_info(fields, state_t),
    Values = tl(tuple_to_list(State)),
    lists:zip(Fields, [reformat(I) || I <- Values]).

reformat({K,V}) when is_list(V) ->
    {K, [reformat(I) || I <- V]};
reformat({K,V}) ->
    try
        {K, [reformat(I) || I <- dict:to_list(V)]}
    catch
        error:{badrecord,dict} ->
            {K,V}
    end;
reformat(V) ->
    try
        [reformat(I) || I <- dict:to_list(V)]
    catch
        error:{badrecord,dict} ->
            V
    end.

%% ===================================================================
%% Internal functions
%% ===================================================================
