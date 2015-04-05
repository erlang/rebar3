-module(rebar_state).

-export([new/0, new/1, new/2, new/3,
         get/2, get/3, set/3,

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

         resources/1, resources/2, add_resource/2,
         providers/1, providers/2, add_provider/2]).

-include("rebar.hrl").

-record(state_t, {dir                               :: file:name(),
                  opts                = dict:new()  :: rebar_dict(),
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
                               tup_umerge(tup_sort(NewValue), tup_sort(OldValue))
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

resources(#state_t{resources=Resources}) ->
    Resources.

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

%% Sort the list in proplist-order, meaning that `{a,b}' and `{a,c}'
%% both compare as usual, and `a' and `b' do the same, but `a' and `{a,b}' will
%% compare based on the first element of the key, and in order. So the following
%% list will sort as:
%% - `[native, {native,o3}, check]' -> `[check, native, {native, o3}]'
%% - `[native, {native,o3}, {native, o2}, check]' -> `[check,native,{native,o3},{native,o2}]'
%% Meaning that:
%% a) no deduplication takes place
%% b) the key of a tuple is what counts in being sorted, but atoms are seen as {atom}
%%    as far as comparison is concerned (departing from lists:ukeysort/2)
%% c) order is preserved for similar keys and tuples no matter their size (sort is stable)
%%
%% These properties let us merge proplists fairly easily.
tup_sort(List) ->
    lists:sort(fun(A, B) when is_tuple(A), is_tuple(B) -> element(1, A) =< element(1, B)
               ;  (A, B) when is_tuple(A) -> element(1, A) =< B
               ;  (A, B) when is_tuple(B) -> A =< element(1, B)
               ;  (A, B) -> A =< B
               end, List).

%% Custom merge functions. The objective is to behave like lists:umerge/2,
%% except that we also compare the merge elements based on the key if they're a
%% tuple, such that `{key, val1}' is always prioritized over `{key, val0}' if
%% the former is from the 'new' list.
%%
%% This lets us apply proper overrides to list of elements according to profile
%% priority. This function depends on a stable proplist sort.
tup_umerge([], Olds) ->
    Olds;
tup_umerge([New|News], Olds) ->
    lists:reverse(umerge(News, Olds, [], New)).

%% This is equivalent to umerge2_2 in the stdlib, except we use the expanded
%% value/key only to compare
umerge(News, [Old|Olds], Merged, Cmp) when element(1, Cmp) == element(1, Old);
                                           element(1, Cmp) == Old;
                                           Cmp == element(1, Old);
                                           Cmp =< Old ->
    umerge(News, Olds, [Cmp | Merged], Cmp, Old);
umerge(News, [Old|Olds], Merged, Cmp) ->
    umerge(News, Olds, [Old | Merged], Cmp);
umerge(News, [], Merged, Cmp) ->
    lists:reverse(News, [Cmp | Merged]).

%% Similar to stdlib's umerge2_1 in the stdlib, except that when the expanded
%% value/keys compare equal, we check if the element is a full dupe to clear it
%% (like the stdlib function does) or otherwise keep the duplicate around in
%% an order that prioritizes 'New' elements.
umerge([New|News], Olds, Merged, CmpMerged, Cmp) when CmpMerged == Cmp ->
    umerge(News, Olds, Merged, New);
umerge([New|News], Olds, Merged, _CmpMerged, Cmp) when element(1,New) == element(1, Cmp);
                                                       element(1,New) == Cmp;
                                                       New == element(1, Cmp);
                                                       New =< Cmp ->
    umerge(News, Olds, [New | Merged], New, Cmp);
umerge([New|News], Olds, Merged, _CmpMerged, Cmp) -> % >
    umerge(News, Olds, [Cmp | Merged], New);
umerge([], Olds, Merged, CmpMerged, Cmp) when CmpMerged == Cmp ->
    lists:reverse(Olds, Merged);
umerge([], Olds, Merged, _CmpMerged, Cmp) ->
    lists:reverse(Olds, [Cmp | Merged]).
