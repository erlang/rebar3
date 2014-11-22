-module(rebar_digraph).

-export([check_graph/2
        ,restore_graph/1
        ,store_graph/3
        ,solve/2
        ,subgraph/2]).

-include("rebar.hrl").

check_graph(_File, #graph{vsn=?GRAPH_VSN}) ->
    ok;
check_graph(File, #graph{vsn=Vsn}) ->
    ?ABORT("~s file version is incompatible. expected: ~b got: ~b",
           [File, ?GRAPH_VSN, Vsn]);
check_graph(File, _) ->
    ?ABORT("~s file is invalid. Please delete before next run.",
           [File]).

restore_graph({Vs, Es}) ->
    G = digraph:new(),
    lists:foreach(fun({V, LastUpdated}) ->
                          digraph:add_vertex(G, V, LastUpdated)
                  end, Vs),
    lists:foreach(fun({V1, V2}) ->
                          digraph:add_edge(G, V1, V2)
                  end, Es),
    G;
restore_graph(File) ->
    G = digraph:new(),
    case file:read_file(File) of
        {ok, Data} ->
            try binary_to_term(Data) of
                G ->
                    %% ok = check_erlcinfo(Config, Graph),
                    #graph{info=Graph} = G,
                    {Vs, Es} = Graph,
                    lists:foreach(
                      fun({V, LastUpdated}) ->
                              digraph:add_vertex(G, V, LastUpdated)
                      end, Vs),
                    lists:foreach(
                      fun({V1, V2}) ->
                              digraph:add_edge(G, V1, V2)
                      end, Es)
            catch
                error:badarg ->
                    ?ERROR(
                       "Failed (binary_to_term) to restore rebar info file."
                       " Discard file.", []),
                    ok
            end;
        _Err ->
            ok
    end,
    G.

store_graph(_G, _File, _Modified = false) ->
    ok;
store_graph(G, File, _Modified) ->
    Vs = lists:map(
           fun(V) ->
                   digraph:vertex(G, V)
           end, digraph:vertices(G)),
    Es = lists:flatmap(
           fun({V, _}) ->
                   lists:map(
                     fun(E) ->
                             {_, V1, V2, _} = digraph:edge(G, E),
                             {V1, V2}
                     end, digraph:out_edges(G, V))
           end, Vs),

    ok = filelib:ensure_dir(File),
    Data = term_to_binary(#graph{info={Vs, Es}}, [{compressed, 9}]),
    file:write_file(File, Data).

solve(Graph, Vertices) ->
    solve(Graph, Vertices, dict:new()).

solve(_Graph, [], Solution) ->
    {_, Vertices} = lists:unzip(dict:to_list(Solution)),
    Vertices;
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
