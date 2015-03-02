-module(rebar_state).

-export([new/0, new/1, new/2, new/3,
         get/2, get/3, set/3,

         opts/1,
         default/1, default/2,

         escript_path/1, escript_path/2,

         lock/1, lock/2,

         current_profiles/1,

         command_args/1, command_args/2,
         command_parsed_args/1, command_parsed_args/2,

         apply_profiles/2,

         dir/1, dir/2,
         create_logic_providers/2,

         project_apps/1, project_apps/2,
         deps_to_build/1, deps_to_build/2,
         all_deps/1, all_deps/2,
         namespace/1, namespace/2,

         deps_names/1,

         overrides/1, overrides/2,
         apply_overrides/2,

         providers/1, providers/2, add_provider/2]).

-include("rebar.hrl").

-record(state_t, {dir                               :: file:name(),
                  opts                = dict:new()  :: rebar_dict(),
                  default             = dict:new()  :: rebar_dict(),

                  escript_path                      :: undefined | file:filename_all(),

                  lock                = [],
                  current_profiles    = [default]     :: [atom()],
                  namespace           = undefined     :: [atom()],

                  command_args        = [],
                  command_parsed_args = [],

                  project_apps        = []          :: [rebar_app_info:t()],
                  deps_to_build       = []          :: [rebar_app_info:t()],
                  all_deps            = []          :: [rebar_app_info:t()],

                  overrides           = [],
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
    NewOpts = dict:merge(fun(_Key, Value1, _Value2) ->
                                 Value1
                         end, LocalOpts, Opts),
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

opts(#state_t{opts=Opts}) ->
    Opts.

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

apply_profiles(State, Profile) when not is_list(Profile) ->
    apply_profiles(State, [Profile]);
apply_profiles(State, [default]) ->
    State;
apply_profiles(State=#state_t{opts=Opts, current_profiles=CurrentProfiles}, Profiles) ->
    ConfigProfiles = rebar_state:get(State, profiles, []),
    NewOpts = lists:foldl(fun(default, OptsAcc) ->
                                  OptsAcc;
                             (Profile, OptsAcc) ->
                                  ProfileOpts = dict:from_list(proplists:get_value(Profile, ConfigProfiles, [])),
                                  merge_opts(Profile, ProfileOpts, OptsAcc)
                          end, Opts, Profiles),
    State#state_t{current_profiles=CurrentProfiles++Profiles, opts=NewOpts}.

merge_opts(Profile, NewOpts, OldOpts) ->
    Opts = dict:merge(fun(_Key, NewValue, OldValue) when is_list(NewValue) ->
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
                                      OldValue ++ NewValue
                              end;
                         (_Key, NewValue, _OldValue) ->
                              NewValue
                      end, NewOpts, OldOpts),
    case dict:find(deps, NewOpts) of
        {ok, Value} ->
            dict:store({deps, Profile}, Value, Opts);
        error ->
            Opts
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

providers(#state_t{providers=Providers}) ->
    Providers.

providers(State, NewProviders) ->
    State#state_t{providers=NewProviders}.

-spec add_provider(t(), providers:t()) -> t().
add_provider(State=#state_t{providers=Providers}, Provider) ->
    State#state_t{providers=[Provider | Providers]}.

create_logic_providers(ProviderModules, State0) ->
    try
        State1 = lists:foldl(fun(ProviderMod, StateAcc) ->
                                     case providers:new(ProviderMod, StateAcc) of
                                         {error, Reason} ->
                                             ?ERROR(Reason++"~n", []),
                                             StateAcc;
                                         {ok, StateAcc1} ->
                                             StateAcc1
                                     end
                             end, State0, ProviderModules),
        apply_hooks(State1)
    catch
        C:T ->
            ?DEBUG("~p: ~p ~p", [C, T, erlang:get_stacktrace()]),
            throw({error, "Failed creating providers. Run with DEBUG=1 for stacktrace."})
    end.

apply_hooks(State0) ->
    try
        Hooks = rebar_state:get(State0, provider_hooks, []),
        PreHooks = proplists:get_value(pre, Hooks, []),
        PostHooks = proplists:get_value(post, Hooks, []),
        State1 = lists:foldl(fun({Target, Hook}, StateAcc) ->
                                     prepend_hook(StateAcc, Target, Hook)
                             end, State0, PreHooks),
        lists:foldl(fun({Target, Hook}, StateAcc) ->
                            append_hook(StateAcc, Target, Hook)
                    end, State1, PostHooks)
    catch
        C:T ->
            ?DEBUG("~p: ~p ~p", [C, T, erlang:get_stacktrace()]),
            throw({error, "Failed parsing provider hooks. Run with DEBUG=1 for stacktrace."})
    end.

%% ===================================================================
%% Internal functions
%% ===================================================================

prepend_hook(State=#state_t{providers=Providers}, Target, Hook) ->
    State#state_t{providers=add_hook(pre, Providers, Target, Hook)}.

append_hook(State=#state_t{providers=Providers}, Target, Hook) ->
    State#state_t{providers=add_hook(post, Providers, Target, Hook)}.

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
