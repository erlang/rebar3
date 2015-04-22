-module(rebar_state).

-export([new/0, new/1, new/2, new/3,
         get/2, get/3, set/3,

         code_paths/2, code_paths/3,

         opts/1, opts/2,
         default/1, default/2,

         escript_path/1, escript_path/2,

         lock/1, lock/2,

         current_profiles/1,

         command_args/1, command_args/2,
         command_parsed_args/1, command_parsed_args/2,

         add_to_profile/3, apply_profiles/2,

         dir/1, dir/2,
         create_logic_providers/2,

         project_apps/1, project_apps/2,
         deps_to_build/1, deps_to_build/2,
         all_deps/1, all_deps/2,
         namespace/1, namespace/2,

         deps_names/1,

         overrides/1, overrides/2,
         apply_overrides/2,

         packages/1, packages/2,
         registry/1, registry/2,

         resources/1, resources/2, add_resource/2,
         providers/1, providers/2, add_provider/2]).

-include("rebar.hrl").

-record(state_t, {dir                               :: file:name(),
                  opts                = dict:new()  :: rebar_dict(),
                  code_paths          = dict:new()  :: rebar_dict(),
                  default             = dict:new()  :: rebar_dict(),
                  escript_path                      :: undefined | file:filename_all(),

                  lock                = [],
                  current_profiles    = [default]     :: [atom()],
                  namespace           = undefined     :: atom(),

                  command_args        = [],
                  command_parsed_args = [],

                  project_apps        = []          :: [rebar_app_info:t()],
                  deps_to_build       = []          :: [rebar_app_info:t()],
                  all_deps            = []          :: [rebar_app_info:t()],

                  packages            = undefined   :: {rebar_dict(), rebar_digraph()} | undefined,
                  registry            = undefined   :: {ok, ets:tid()} | error | undefined,
                  overrides           = [],
                  resources           = [],
                  providers           = []}).

-export_type([t/0]).

-type t() :: record(state_t).

-spec new() -> t().
new() ->
    #state_t{dir = rebar_dir:get_cwd()}.

-spec new(list()) -> t().
new(Config) when is_list(Config) ->
    Deps = proplists:get_value(deps, Config, []),
    Opts = dict:from_list([{{deps, default}, Deps} | Config]),
    #state_t { dir = rebar_dir:get_cwd(),
               default = Opts,
               opts = Opts }.

-spec new(t() | atom(), list()) -> t().
new(Profile, Config) when is_atom(Profile)
                        , is_list(Config) ->
    Deps = proplists:get_value(deps, Config, []),
    Opts = dict:from_list([{{deps, default}, Deps} | Config]),
    #state_t { dir = rebar_dir:get_cwd(),
               current_profiles = [Profile],
               default = Opts,
               opts = Opts };
new(ParentState=#state_t{}, Config) ->
    %% Load terms from rebar.config, if it exists
    Dir = rebar_dir:get_cwd(),
    new(ParentState, Config, Dir).

-spec new(t(), list(), file:name()) -> t().
new(ParentState, Config, Dir) ->
    Opts = ParentState#state_t.opts,
    LocalOpts = case rebar_config:consult_file(filename:join(Dir, ?LOCK_FILE)) of
                    [D] ->
                        %% We want the top level deps only from the lock file.
                        %% This ensures deterministic overrides for configs.
                        Deps = [X || X <- D, element(3, X) =:= 0],
                        dict:from_list([{{locks, default}, D}, {{deps, default}, Deps} | Config]);
                    _ ->
                        D = proplists:get_value(deps, Config, []),
                        dict:from_list([{{deps, default}, D} | Config])
                end,

    NewOpts = merge_opts(LocalOpts, Opts),

    ParentState#state_t{dir=Dir
                       ,opts=NewOpts
                       ,default=NewOpts}.

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

code_paths(#state_t{code_paths=CodePaths}, Key) ->
    case dict:find(Key, CodePaths) of
        {ok, CodePath} ->
            CodePath;
        _ ->
            []
    end.

code_paths(State=#state_t{code_paths=CodePaths}, Key, CodePath) ->
    State#state_t{code_paths=dict:store(Key, CodePath, CodePaths)}.

opts(#state_t{opts=Opts}) ->
    Opts.

opts(State, Opts) ->
    State#state_t{opts=Opts}.

current_profiles(#state_t{current_profiles=Profiles}) ->
    Profiles.

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

apply_overrides(State=#state_t{overrides=Overrides}, AppName) ->
    Name = binary_to_atom(AppName, utf8),

    %% Inefficient. We want the order we get here though.
    State1 = lists:foldl(fun({override, O}, StateAcc) ->
                                 lists:foldl(fun({Key, Value}, StateAcc1) ->
                                                     rebar_state:set(StateAcc1, Key, Value)
                                             end, StateAcc, O);
                            (_, StateAcc) ->
                                 StateAcc
                         end, State, Overrides),

    State2 = lists:foldl(fun({override, N, O}, StateAcc) when N =:= Name ->
                                 lists:foldl(fun({Key, Value}, StateAcc1) ->
                                                     rebar_state:set(StateAcc1, Key, Value)
                                             end, StateAcc, O);
                            (_, StateAcc) ->
                                 StateAcc
                         end, State1, Overrides),

    lists:foldl(fun({add, N, O}, StateAcc) when N =:= Name ->
                        lists:foldl(fun({Key, Value}, StateAcc1) ->
                                            OldValue = rebar_state:get(StateAcc1, Key, []),
                                            rebar_state:set(StateAcc1, Key, Value++OldValue)
                                    end, StateAcc, O);
                   (_, StateAcc) ->
                        StateAcc
                end, State2, Overrides).

add_to_profile(State, Profile, KVs) when is_atom(Profile), is_list(KVs) ->
    Profiles = rebar_state:get(State, profiles, []),
    ProfileOpts = dict:from_list(proplists:get_value(Profile, Profiles, [])),
    NewOpts = merge_opts(Profile, dict:from_list(KVs), ProfileOpts),
    NewProfiles = [{Profile, dict:to_list(NewOpts)}|lists:keydelete(Profile, 1, Profiles)],
    rebar_state:set(State, profiles, NewProfiles).

apply_profiles(State, Profile) when not is_list(Profile) ->
    apply_profiles(State, [Profile]);
apply_profiles(State, [default]) ->
    State;
apply_profiles(State=#state_t{opts=Opts, current_profiles=CurrentProfiles}, Profiles) ->
    ConfigProfiles = rebar_state:get(State, profiles, []),
    {Profiles1, NewOpts} =
        lists:foldl(fun(default, {ProfilesAcc, OptsAcc}) ->
                            {ProfilesAcc, OptsAcc};
                       (Profile, {ProfilesAcc, OptsAcc}) ->
                            ProfileOpts = dict:from_list(proplists:get_value(Profile, ConfigProfiles, [])),
                            {[Profile]++ProfilesAcc, merge_opts(Profile, ProfileOpts, OptsAcc)}
                    end, {[], Opts}, Profiles),
    State#state_t{current_profiles=CurrentProfiles++Profiles1, opts=NewOpts}.

merge_opts(Profile, NewOpts, OldOpts) ->
    Opts = merge_opts(NewOpts, OldOpts),

    case dict:find(deps, NewOpts) of
        {ok, Value} ->
            dict:store({deps, Profile}, Value, Opts);
        error ->
            Opts
    end.

merge_opts(NewOpts, OldOpts) ->
    dict:merge(fun(deps, NewValue, _OldValue) ->
                       NewValue;
                  ({deps, _}, NewValue, _OldValue) ->
                       NewValue;
                  (profiles, NewValue, OldValue) ->
                       dict:to_list(merge_opts(dict:from_list(NewValue), dict:from_list(OldValue)));
                  (_Key, NewValue, OldValue) when is_list(NewValue) ->
                       case io_lib:printable_list(NewValue) of
                           true when NewValue =:= [] ->
                               case io_lib:printable_list(OldValue) of
                                   true ->
                                       NewValue;
                                   false ->
                                       OldValue
                               end;
                           true ->
                               NewValue;
                           false ->
                               rebar_utils:tup_umerge(rebar_utils:tup_sort(NewValue)
                                                     ,rebar_utils:tup_sort(OldValue))
                       end;
                  (_Key, NewValue, _OldValue) ->
                       NewValue
               end, NewOpts, OldOpts).

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

overrides(#state_t{overrides=Overrides}) ->
    Overrides.

overrides(State=#state_t{}, Overrides) ->
    State#state_t{overrides=Overrides}.

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

namespace(#state_t{namespace=Namespace}) ->
    Namespace.

namespace(State=#state_t{}, Namespace) ->
    State#state_t{namespace=Namespace}.

packages(State=#state_t{packages=undefined}) ->
    rebar_packages:get_packages(State);
packages(#state_t{packages=Packages}) ->
    Packages.

packages(State, Packages) ->
    State#state_t{packages=Packages}.

registry(State=#state_t{registry=undefined}) ->
    rebar_packages:registry(State);
registry(#state_t{registry=Registry}) ->
    Registry.

registry(State, Registry) ->
    State#state_t{registry=Registry}.

-spec resources(t()) -> rebar_resource:resource().
resources(#state_t{resources=Resources}) ->
    Resources.

-spec resources(t(), [rebar_resource:resource()]) -> t().
resources(State, NewResources) ->
    State#state_t{resources=NewResources}.

-spec add_resource(t(), rebar_resource:resource()) -> t().
add_resource(State=#state_t{resources=Resources}, Resource) ->
    State#state_t{resources=[Resource | Resources]}.

providers(#state_t{providers=Providers}) ->
    Providers.

providers(State, NewProviders) ->
    State#state_t{providers=NewProviders}.

-spec add_provider(t(), providers:t()) -> t().
add_provider(State=#state_t{providers=Providers}, Provider) ->
    State#state_t{providers=[Provider | Providers]}.

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
            throw({error, "Failed creating providers. Run with DEBUG=1 for stacktrace."})
    end.

%% ===================================================================
%% Internal functions
%% ===================================================================
