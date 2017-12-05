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
         all_deps/1, all_deps/2, update_all_deps/2,
         namespace/1, namespace/2,

         deps_names/1,

         to_list/1,

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

                  resources           = [],
                  providers           = [],
                  allow_provider_overrides = false  :: boolean()}).

-export_type([t/0]).

-type t() :: #state_t{}.

-spec new() -> t().
new() ->
    BaseState = base_state(),
    BaseState#state_t{dir = rebar_dir:get_cwd()}.

-spec new(list()) -> t().
new(Config) when is_list(Config) ->
    BaseState = base_state(),
    Opts = base_opts(Config),
    BaseState#state_t { dir = rebar_dir:get_cwd(),
                        default = Opts,
                        opts = Opts }.

-spec new(t() | atom(), list()) -> t().
new(Profile, Config) when is_atom(Profile)
                        , is_list(Config) ->
    BaseState = base_state(),
    Opts = base_opts(Config),
    BaseState#state_t { dir = rebar_dir:get_cwd(),
                        current_profiles = [Profile],
                        default = Opts,
                        opts = Opts };
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

base_state() ->
    case application:get_env(rebar, resources) of
        undefined ->
            Resources = [];
        {ok, Resources} ->
            Resources
    end,
    #state_t{resources=Resources}.

base_opts(Config) ->
    Deps = proplists:get_value(deps, Config, []),
    Plugins = proplists:get_value(plugins, Config, []),
    ProjectPlugins = proplists:get_value(project_plugins, Config, []),
    Terms = [{{deps, default}, Deps}, {{plugins, default}, Plugins}, {{project_plugins, default}, ProjectPlugins} | Config],
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
    AppliedProfiles = case Profiles of
                          %% Head of list global profile is special, only for use by rebar3
                          %% It does not clash if a user does `rebar3 as global...` but when
                          %% it is the head we must make sure not to prepend `default`
                          [global | _] ->
                              Profiles;
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

all_plugin_deps(#state_t{all_plugin_deps=Apps}) ->
    Apps.

all_plugin_deps(State=#state_t{}, NewApps) ->
    State#state_t{all_plugin_deps=NewApps}.

update_all_plugin_deps(State=#state_t{all_plugin_deps=Apps}, NewApps) ->
    State#state_t{all_plugin_deps=Apps++NewApps}.

update_all_deps(State=#state_t{all_deps=Apps}, NewApps) ->
    State#state_t{all_deps=Apps++NewApps}.

namespace(#state_t{namespace=Namespace}) ->
    Namespace.

namespace(State=#state_t{}, Namespace) ->
    State#state_t{namespace=Namespace}.

-spec resources(t()) -> [{rebar_resource:type(), module()}].
resources(#state_t{resources=Resources}) ->
    Resources.

-spec resources(t(), [{rebar_resource:type(), module()}]) -> t().
resources(State, NewResources) ->
    State#state_t{resources=NewResources}.

-spec add_resource(t(), {rebar_resource:type(), module()}) -> t().
add_resource(State=#state_t{resources=Resources}, Resource) ->
    State#state_t{resources=[Resource | Resources]}.

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
                                   ?DEBUG("Not adding provider ~p ~p from module ~p because it already exists from module ~p",
                                          [Namespace, Name, Module, providers:module(P)]),
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

create_logic_providers(ProviderModules, State0) ->
    try
        lists:foldl(fun(ProviderMod, StateAcc) ->
                            case providers:new(ProviderMod, StateAcc) of
                                {error, Reason} ->
                                    ?ERROR(Reason++"~n", []),
                                    StateAcc;
                                {ok, StateAcc1} ->
                                    StateAcc1
                            end
                    end, State0, ProviderModules)
    catch
        C:T ->
            ?DEBUG("~p: ~p ~p", [C, T, erlang:get_stacktrace()]),
            ?CRASHDUMP("~p: ~p~n~p~n~n~p", [C, T, erlang:get_stacktrace(), State0]),
            throw({error, "Failed creating providers. Run with DEBUG=1 for stacktrace or consult rebar3.crashdump."})
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
