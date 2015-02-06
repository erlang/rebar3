-module(rebar_digraph).

-export([compile_order/1
        ,restore_graph/1
        ,cull_deps/2
        ,subgraph/2
        ,format_error/1]).

-include("rebar.hrl").

%% Sort apps with topological sort to get proper build order
compile_order(Apps) ->
    Graph = digraph:new(),
    lists:foreach(fun(App) ->
                          Name = rebar_app_info:name(App),
                          Deps = rebar_app_info:deps(App),
                          add(Graph, {Name, Deps})
                  end, Apps),
    case digraph_utils:topsort(Graph) of
        false ->
            case digraph_utils:is_acyclic(Graph) of
                true ->
                    {error, no_sort};
                false ->
                    Cycles = lists:sort(
                        [lists:sort(Comp) || Comp <- digraph_utils:strong_components(Graph),
                                             length(Comp)>1]),
                    {error, {cycles, Cycles}}
            end;
        V ->
            {ok, names_to_apps(lists:reverse(V), Apps)}
    end.

add(Graph, {PkgName, Deps}) ->
    case digraph:vertex(Graph, PkgName) of
        false ->
            V = digraph:add_vertex(Graph, PkgName);
        {V, []} ->
            V
    end,

    lists:foreach(fun(DepName) ->
                          Name1 = case DepName of
                                      {Name, _Vsn} ->
                                          ec_cnv:to_binary(Name);
                                      Name ->
                                          ec_cnv:to_binary(Name)
                                  end,
                          V3 = case digraph:vertex(Graph, Name1) of
                                   false ->
                                       digraph:add_vertex(Graph, Name1);
                                   {V2, []} ->
                                       V2
                               end,
                          digraph:add_edge(Graph, V, V3)
                  end, Deps).

restore_graph({Vs, Es}) ->
    Graph = digraph:new(),
    lists:foreach(fun({V, LastUpdated}) ->
                          digraph:add_vertex(Graph, V, LastUpdated)
                  end, Vs),
    lists:foreach(fun({V1, V2}) ->
                          digraph:add_edge(Graph, V1, V2)
                  end, Es),
    Graph.

%% Pick packages to fullfill dependencies
%% The first dep while traversing the graph is chosen and any conflicting
%% dep encountered later on is ignored.
cull_deps(Graph, Vertices) ->
    cull_deps(Graph,
              Vertices,
              lists:foldl(fun({Key, _}=N, Solution) -> dict:store(Key, N, Solution) end,
                          dict:new(), Vertices),
              []).

cull_deps(_Graph, [], Solution, Discarded) ->
    {_, Vertices} = lists:unzip(dict:to_list(Solution)),
    {ok, Vertices, Discarded};
cull_deps(Graph, Vertices, Solution, Discarded) ->
    {NV, NS, DS} =
        lists:foldl(fun(V, {NewVertices, SolutionAcc, DiscardedAcc}) ->
                        OutNeighbors = digraph:out_neighbours(Graph, V),
                        lists:foldl(fun({Key, _}=N, {NewVertices1, SolutionAcc1, DiscardedAcc1}) ->
                                            case dict:find(Key, SolutionAcc1) of
                                                {ok, N} -> % already seen
                                                    {NewVertices1, SolutionAcc1, DiscardedAcc1};
                                                {ok, _} -> % conflict resolution!
                                                    {NewVertices1, SolutionAcc1, [N|DiscardedAcc1]};
                                                error ->
                                                    {[N | NewVertices1], dict:store(Key, N, SolutionAcc1), DiscardedAcc1}
                                            end
                                    end, {NewVertices, SolutionAcc, DiscardedAcc}, OutNeighbors)
                    end, {[], Solution, Discarded}, lists:sort(Vertices)),
    cull_deps(Graph, NV, NS, DS).

subgraph(Graph, Vertices) ->
    digraph_utils:subgraph(Graph, Vertices).

format_error(no_solution) ->
    io_lib:format("No solution for packages found.", []).

%%====================================================================
%% Internal Functions
%%====================================================================

-spec names_to_apps([atom()], [rebar_app_info:t()]) -> [rebar_app_info:t()].
names_to_apps(Names, Apps) ->
    [element(2, App) || App <- [find_app_by_name(Name, Apps) || Name <- Names], App =/= error].

-spec find_app_by_name(atom(), [rebar_app_info:t()]) -> {ok, rebar_app_info:t()} | error.
find_app_by_name(Name, Apps) ->
    ec_lists:find(fun(App) ->
                          rebar_app_info:name(App) =:= Name
                  end, Apps).
