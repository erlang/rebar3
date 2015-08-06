-module(rebar_digraph).

-export([compile_order/1
        ,restore_graph/1
        ,cull_deps/3
        ,cull_deps/4
        ,subgraph/2
        ,print_solution/2
        ,format_error/1]).

-include("rebar.hrl").

%% Sort apps with topological sort to get proper build order
compile_order(Apps) ->
    Graph = digraph:new(),
    lists:foreach(fun(App) ->
                          Name = rebar_app_info:name(App),
                          Deps = all_apps_deps(App),
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

cull_deps(Graph, Vertices, Level) ->
    {ok, LvlVertices, Discarded, _} = cull_deps(Graph, Vertices, Level, none),
    {ok, LvlVertices, Discarded}.

cull_deps(Graph, Vertices, Level, SolutionGraph) ->
    cull_deps(Graph,
              Vertices,
              Level+1,
              lists:foldl(fun({Key, _}, Levels) ->
                                  dict:store(Key, Level, Levels)
                          end, dict:new(), Vertices),
              lists:foldl(fun({Key, _}=N, Solution) ->
                                  dict:store(Key, N, Solution)
                          end, dict:new(), Vertices),
              [],
              SolutionGraph).

cull_deps(_Graph, [], _Level, Levels, Solution, Discarded, SolutionGraph) ->
    {_, Vertices} = lists:unzip(dict:to_list(Solution)),
    LvlVertices = [{App,Vsn,dict:fetch(App,Levels)} || {App,Vsn} <- Vertices],
    {ok, LvlVertices, Discarded, SolutionGraph};
cull_deps(Graph, Vertices, Level, Levels, Solution, Discarded, SolutionGraph) ->
    {NV, NS, LS, DS} =
        lists:foldl(fun(V, {NewVertices, SolutionAcc, LevelsAcc, DiscardedAcc}) ->
                        OutNeighbors = lists:keysort(1, digraph:out_neighbours(Graph, V)),
                        lists:foldl(fun({Key, _}=N, {NewVertices1, SolutionAcc1, LevelsAcc1, DiscardedAcc1}) ->
                                            case dict:find(Key, SolutionAcc1) of
                                                {ok, N} -> % already seen
                                                    {NewVertices1, SolutionAcc1, LevelsAcc1, DiscardedAcc1};
                                                {ok, _} -> % conflict resolution!
                                                    {NewVertices1, SolutionAcc1, LevelsAcc1, [N|DiscardedAcc1]};
                                                error ->
                                                    add_to_solution_graph(N, V, SolutionGraph),
                                                    {[N | NewVertices1],
                                                     dict:store(Key, N, SolutionAcc1),
                                                     dict:store(Key, Level, LevelsAcc1),
                                                     DiscardedAcc1}
                                            end
                                    end, {NewVertices, SolutionAcc, LevelsAcc, DiscardedAcc}, OutNeighbors)
                    end, {[], Solution, Levels, Discarded}, lists:keysort(1, Vertices)),
    cull_deps(Graph, NV, Level+1, LS, NS, DS, SolutionGraph).

subgraph(Graph, Vertices) ->
    digraph_utils:subgraph(Graph, Vertices).

add_to_solution_graph(_, _, none) ->
    ok;
add_to_solution_graph(N, V, SolutionGraph) ->
    NewV = digraph:add_vertex(SolutionGraph, N),
    digraph:add_edge(SolutionGraph, V, NewV).

print_solution(Graph, Deps) ->
    SolutionGraph = digraph:new(),
    [digraph:add_vertex(SolutionGraph, V) || V <- Deps],
    cull_deps(Graph, Deps, 0, SolutionGraph),
    print_solution(SolutionGraph, Deps, 0).

print_solution(_, [], _) ->
    ok;
print_solution(SolutionGraph, [{N, V} | Vertices], 0) ->
    ?CONSOLE("~s-~s", [N, V]),
    OutNeighbors = lists:keysort(1, digraph:out_neighbours(SolutionGraph, {N,V})),
    print_solution(SolutionGraph, OutNeighbors, 4),
    print_solution(SolutionGraph, Vertices, 0);
print_solution(SolutionGraph, [{N, V} | Vertices], Indent) ->
    ?CONSOLE("~s~s-~s", [[" " || _ <- lists:seq(0, Indent)], N, V]),
    OutNeighbors = lists:keysort(1, digraph:out_neighbours(SolutionGraph, {N,V})),
    print_solution(SolutionGraph, OutNeighbors, Indent+4),
    print_solution(SolutionGraph, Vertices, Indent).

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

%% The union of all entries in the applications list for an app and
%% the deps listed in its rebar.config is all deps that may be needed
%% for building the app.
all_apps_deps(App) ->
    Applications = lists:usort([atom_to_binary(X, utf8) || X <- rebar_app_info:applications(App)]),
    Deps = lists:usort(lists:map(fun({Name, _}) -> Name; (Name) -> Name end, rebar_app_info:deps(App))),
    lists:umerge(Deps, Applications).
