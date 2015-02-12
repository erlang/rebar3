-module(rebar_packages).

-export([get_packages/1]).

-export_type([package/0]).

-include("rebar.hrl").

-type pkg_name() :: binary() | atom().
-type vsn() :: binary().
-type package() :: pkg_name() | {pkg_name(), vsn()}.

-spec get_packages(rebar_state:t()) -> {rebar_dict(), rebar_digraph()}.
get_packages(State) ->
    RebarDir = rebar_dir:global_config_dir(State),
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
            {dict:new(), digraph:new()}
    end.
