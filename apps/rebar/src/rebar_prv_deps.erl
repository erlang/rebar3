-module(rebar_prv_deps).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-include("rebar.hrl").

-define(PROVIDER, deps).
-define(DEPS, [install_deps]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    State1 = rebar_state:add_provider(
            State,
            providers:create([
                    {name, ?PROVIDER},
                    {module, ?MODULE},
                    {bare, true},
                    {deps, ?DEPS},
                    {example, "rebar3 deps"},
                    {short_desc, "List dependencies"},
                    {desc, "List dependencies. Those not matching "
                           "the config file are followed by "
                           "an asterisk (*)."},
                    {opts, []}])),
    {ok, State1}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Profiles = rebar_state:current_profiles(State),
    [display(State, Profile) || Profile <- Profiles],
    {ok, State}.


-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).


display(State, Profile = default) ->
    display_profile_deps(State, Profile),
    ?CONSOLE("", []);
display(State, Profile) ->
    ?CONSOLE("-- ~p --", [Profile]),
    display_profile_deps(State, Profile),
    ?CONSOLE("", []).


display_profile_deps(State, Profile) ->
    DepsDir = rebar_prv_install_deps:profile_dep_dir(State, Profile),

    ProfileDeps = rebar_state:get(State, {deps, Profile}, []),
    % ProfileDeps include those deps from rebar.lock that have been
    % removed from rebar.config
    ConfiguredDeps = [parse_dep_without_locks(DepsDir, Dep, State)
                      || Dep <- ProfileDeps],
    LockedDepsMap = locked_deps_map(State, Profile),
    [display_dep(State, Dep, LockedDepsMap) || Dep <- ConfiguredDeps].


parse_dep_without_locks(DepsDir, Dep, State) ->
    ParsedDep = rebar_app_utils:parse_dep(Dep, root, DepsDir, State, [], 0),
    case Dep of
        {_Name, Src, Level} when is_tuple(Src), is_integer(Level) ->
            % This Dep is not in rebar.config but in rebar.lock
            rebar_app_info:source(ParsedDep, undefined);
        _ ->
            rebar_app_utils:expand_deps_sources(ParsedDep, State)
    end.


locked_deps_map(State, Profile) ->
    ParsedDeps = rebar_state:get(State, {parsed_deps, Profile}, []),
    lists:foldl(fun(Dep, DepsIn) ->
                        case rebar_app_info:is_lock(Dep) of
                            true ->
                                DepName = rebar_app_info:name(Dep),
                                maps:put(rebar_utils:to_binary(DepName), Dep, DepsIn);
                            _ ->
                                DepsIn
                        end
                end, maps:new(), ParsedDeps).


display_dep(State, Dep, LockedDeps) ->
    Name = rebar_utils:to_binary(rebar_app_info:name(Dep)),
    NeedsUpdate = rebar_fetch:needs_update(Dep, State),
    Source = rebar_app_info:source(Dep),
    LockedSource = case maps:get(Name, LockedDeps, undefined) of
                       undefined -> undefined;
                       LockedDep -> rebar_app_info:source(LockedDep)
                   end,

    display_dep_line(Name, NeedsUpdate, source_text(LockedSource), source_text(Source)).


% Dep is a checkout
display_dep_line(Name, _NeedsUpdate, _LockedSource, Source = checkout) ->
    ?CONSOLE("~ts* (~ts)", [Name, Source]);

% Dep exists only in lock file
display_dep_line(Name, _NeedsUpdate, LockedSource, _Source = undefined) ->
    ?CONSOLE("~ts* (locked ~ts <> none)", [Name, LockedSource]);

% Dep not locked, report whether the disk copy matches the Source
display_dep_line(Name, true, undefined, Source) ->
    ?CONSOLE("~ts* (~ts)", [Name, Source]);
display_dep_line(Name, _, undefined, Source) ->
    ?CONSOLE("~ts (~ts)", [Name, Source]);

% Dep locked, install_deps provider should have had updated the disk copy with
% the locked version
display_dep_line(Name, false, _LockedSource, Source) ->
    % dep locked and no need to update (LockedSource and Source might not match
    % because of one using {ref, X} and the other {tag, Y})
    ?CONSOLE("~ts (locked ~ts)", [Name, Source]);
display_dep_line(Name, _NeedsUpdate, LockedSource, Source) ->
    % dep locked with mismatching lock and config files
    ?CONSOLE("~ts* (locked ~ts <> ~ts)", [Name, LockedSource, Source]).


source_text(Source) when is_list(Source); is_atom(Source) ->
    Source;
source_text({pkg, _Name, Vsn, _Hash, _RepoConfig}) ->
    source_text({pkg, _Name, Vsn, _Hash});
source_text({pkg, _Name, Vsn, _Hash}) ->
    [<<"package">>, " ", rebar_utils:to_binary(Vsn)];
source_text(Source) when is_tuple(Source), tuple_size(Source) < 3 ->
    element(1, Source);
source_text(Source) when is_tuple(Source) ->
    Type = element(1, Source),
    case element(3, Source) of
        {ref , Ref} ->
            SmallRef = case rebar_utils:to_binary(Ref) of
                           <<R:7/binary, _/binary>> -> <<R/binary, "...">>;
                           R -> R
                       end,
            [atom_to_binary(Type, unicode), " source ", SmallRef];
        {_ , Vsn} ->
            [atom_to_binary(Type, unicode), " source ", rebar_utils:to_binary(Vsn)];
        _ ->
            [atom_to_binary(Type, unicode), " source"]
    end.

