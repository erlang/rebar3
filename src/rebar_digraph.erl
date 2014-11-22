-module(rebar_digraph).

-export([restore_graph/1
        ,solve/2
        ,subgraph/2
        ,format_error/1]).

-include("rebar.hrl").

restore_graph({Vs, Es}) ->
    Graph = digraph:new(),
    lists:foreach(fun({V, LastUpdated}) ->
                          digraph:add_vertex(Graph, V, LastUpdated)
                  end, Vs),
    lists:foreach(fun({V1, V2}) ->
                          digraph:add_edge(Graph, V1, V2)
                  end, Es),
    Graph.

solve(Graph, Vertices) ->
    solve(Graph, Vertices, dict:new()).

solve(_Graph, [], Solution) ->
    {_, Vertices} = lists:unzip(dict:to_list(Solution)),
    {ok, Vertices};
solve(Graph, Vertices, Solution) ->
    {NV, NS} =
        lists:foldl(fun(V, {NewVertices, SolutionAcc}) ->
                        OutNeighbors = digraph:out_neighbours(Graph, V),
                        lists:foldl(fun({Key, _}=N, {NewVertices1, SolutionAcc1}) ->
                                            case dict:is_key(Key, SolutionAcc1) orelse
                                                lists:keymember(Key, 1, Vertices) of
                                                true ->
                                                    {NewVertices1, SolutionAcc1};
                                                false ->
                                                    {[N | NewVertices1], dict:store(Key, N, SolutionAcc1)}
                                            end
                                    end, {NewVertices, SolutionAcc}, OutNeighbors)
                    end, {[], Solution}, Vertices),
    solve(Graph, NV, NS).

subgraph(Graph, Vertices) ->
    digraph_utils:subgraph(Graph, Vertices).

format_error(no_solution) ->
    io_lib:format("No solution for packages found.", []).
