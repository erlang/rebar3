-module(rebar_packages).

-export([get_packages/1
        ,registry/1
        ,check_registry/3
        ,registry_checksum/2
        ,find_highest_matching/3]).

-export_type([package/0]).

-include("rebar.hrl").

-type pkg_name() :: binary() | atom().
-type vsn() :: binary().
-type package() :: pkg_name() | {pkg_name(), vsn()}.

-spec get_packages(rebar_state:t()) -> {rebar_dict(), rebar_digraph()}.
get_packages(State) ->
    RebarDir = rebar_dir:global_cache_dir(State),
    RegistryDir = filename:join(RebarDir, "packages"),
    DictFile = filename:join(RegistryDir, "dict"),
    Edges = filename:join(RegistryDir, "edges"),
    Vertices = filename:join(RegistryDir, "vertices"),
    Neighbors = filename:join(RegistryDir, "neighbors"),

    case lists:all(fun(X) -> filelib:is_file(X) end, [DictFile, Edges, Vertices, Neighbors]) of
        true ->
            try
                {ok, DictBinary} = file:read_file(DictFile),
                Dict = binary_to_term(DictBinary),
                {ok, EdgesTab} = ets:file2tab(Edges),
                {ok, VerticesTab} = ets:file2tab(Vertices),
                {ok, NeighborsTab} = ets:file2tab(Neighbors),
                {Dict, {digraph, EdgesTab, VerticesTab, NeighborsTab, true}}
            catch
                _:_ ->
                    ?ERROR("Bad packages index, try to fix with `rebar3 update`", []),
                    {dict:new(), digraph:new()}
            end;
        false ->
            ?ERROR("Bad packages index, try to fix with `rebar3 update`", []),
            {dict:new(), digraph:new()}
    end.

registry(State) ->
    Dir = rebar_dir:global_cache_dir(State),
    RegistryDir = filename:join(Dir, "packages"),
    HexFile = filename:join(RegistryDir, "registry"),
    case ets:file2tab(HexFile) of
        {ok, T} ->
            {ok, T};
        {error, Reason} ->
            ?DEBUG("Error loading registry: ~p", [Reason]),
            error
    end.

check_registry(Pkg, Vsn, State) ->
    case rebar_state:registry(State) of
        {ok, T} ->
            case ets:lookup(T, Pkg) of
                [{Pkg, [Vsns]}] ->
                    lists:member(Vsn, Vsns);
                _ ->
                    false
            end;
        error ->
            false
    end.

registry_checksum({pkg, Name, Vsn}, State) ->
    {ok, Registry} = registry(State),
    case ets:lookup(Registry, {Name, Vsn}) of
        [{{_, _}, [_, Checksum | _]}] ->
            Checksum;
        [] ->
            none
    end.

%% Hex supports use of ~> to specify the version required for a dependency.
%% Since rebar3 requires exact versions to choose from we find the highest
%% available version of the dep that passes the constraint.

%% `~>` will never include pre-release versions of its upper bound.
%% It can also be used to set an upper bound on only the major
%% version part. See the table below for `~>` requirements and
%% their corresponding translation.
%% `~>` | Translation
%% :------------- | :---------------------
%% `~> 2.0.0` | `>= 2.0.0 and < 2.1.0`
%% `~> 2.1.2` | `>= 2.1.2 and < 2.2.0`
%% `~> 2.1.3-dev` | `>= 2.1.3-dev and < 2.2.0`
%% `~> 2.0` | `>= 2.0.0 and < 3.0.0`
%% `~> 2.1` | `>= 2.1.0 and < 3.0.0`
find_highest_matching(Dep, Constraint, T) ->
    case ets:lookup(T, Dep) of
        [{Dep, [[Vsn]]}] ->
            case ec_semver:pes(Vsn, Constraint) of
                true ->
                    {ok, Vsn};
                false ->
                    ?WARN("Only existing version of ~s is ~s which does not match constraint ~~> ~s. "
                         "Using anyway, but it is not guarenteed to work.", [Dep, Vsn, Constraint]),
                    {ok, Vsn}
            end;
        [{Dep, [[HeadVsn | VsnTail]]}] ->
            {ok, lists:foldl(fun(Version, Highest) ->
                                case ec_semver:pes(Version, Constraint) andalso
                                    ec_semver:gt(Version, Highest) of
                                    true ->
                                        Version;
                                    false ->
                                        Highest
                                end
                        end, HeadVsn, VsnTail)};
        [] ->
            ?WARN("Missing registry entry for package ~s", [Dep]),
            none
    end.
