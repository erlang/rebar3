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
            {error, no_sort};
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
                          V3 = case digraph:vertex(Graph, DepName) of
                                   false ->
                                       digraph:add_vertex(Graph, DepName);
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
    cull_deps(Graph, Vertices, lists:foldl(fun({Key, _}=N, Solution) ->
                                               dict:store(Key, N, Solution)
                                       end, dict:new(), Vertices)).

cull_deps(_Graph, [], Solution) ->
    {_, Vertices} = lists:unzip(dict:to_list(Solution)),
    {ok, Vertices};
cull_deps(Graph, Vertices, Solution) ->
    {NV, NS} =
        lists:foldl(fun(V, {NewVertices, SolutionAcc}) ->
                        OutNeighbors = digraph:out_neighbours(Graph, V),
                        lists:foldl(fun({Key, _}=N, {NewVertices1, SolutionAcc1}) ->
                                            case dict:is_key(Key, SolutionAcc1) of
                                                true ->
                                                    {NewVertices1, SolutionAcc1};
                                                false ->
                                                    {[N | NewVertices1], dict:store(Key, N, SolutionAcc1)}
                                            end
                                    end, {NewVertices, SolutionAcc}, OutNeighbors)
                    end, {[], Solution}, Vertices),
    cull_deps(Graph, NV, NS).

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
                          ec_cnv:to_atom(rebar_app_info:name(App)) =:= ec_cnv:to_atom(Name)
                  end, Apps).
