-module(rebar_digraph).

-export([compile_order/1
        ,restore_graph/1
        ,cull_deps/2
        ,cull_deps/3
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
cull_deps(Graph, Vertices) ->
    {ok, LvlVertices, Discarded, _} = cull_deps(Graph, Vertices, none),
    {ok, LvlVertices, Discarded}.

print_solution(Graph, PkgDeps=[{_, _, Deps} | _]) ->
    SolutionGraph = digraph:new(),
    [digraph:add_vertex(SolutionGraph, V) || V <- Deps],
    cull_deps(Graph, PkgDeps, SolutionGraph),
    print_solution(SolutionGraph, Deps, 0).

format_error(no_solution) ->
    io_lib:format("No solution for packages found.", []).

%%====================================================================
%% Internal Functions
%%====================================================================

cull_deps(Graph, Vertices, SolutionGraph) ->
    {Solution, Levels} = build_initial_dicts(Vertices),
    cull_deps(Graph,
              Vertices,
              Levels,
              Solution,
              [],
              SolutionGraph).

cull_deps(_Graph, [], Levels, Solution, Discarded, SolutionGraph) ->
    {_, Vertices} = lists:unzip(dict:to_list(Solution)),
    LvlVertices = [{Profile, {App,Vsn,dict:fetch(App,Levels)}} || {Profile, {App,Vsn}} <- Vertices],
    {ok, LvlVertices, Discarded, SolutionGraph};
cull_deps(Graph, [{Profile, Level, Vs} | Vertices], Levels, Solution, Discarded, SolutionGraph) ->
    {NV, NS, LS, DS} =
        lists:foldl(fun(V, {Acc, SolutionAcc, LevelsAcc, DiscardedAcc}) ->
                            OutNeighbors = lists:keysort(1, digraph:out_neighbours(Graph, V)),
                            handle_neighbors(Profile, Level, V
                                            ,OutNeighbors, Acc, SolutionAcc
                                            ,LevelsAcc, DiscardedAcc, SolutionGraph)

                    end, {[], Solution, Levels, Discarded}, lists:keysort(1, Vs)),

    cull_deps(Graph, Vertices++NV, LS, NS, DS, SolutionGraph).

%% For each outgoing edge of a dep check if it should be added to the solution
%% and add it to the list of vertices to do the same for
handle_neighbors(Profile, Level, Vertex, OutNeighbors, Vertices
                ,Solution, Levels, Discarded, SolutionGraph) ->
        case lists:foldl(fun({Key, _}=N, {NewVertices, Solution1, Levels1, Discarded1}) ->
                            maybe_add_to_solution(Profile, Level, Vertex, Key, N
                                                 ,NewVertices, Solution1
                                                 ,Levels1, Discarded1, SolutionGraph)
                    end, {[], Solution, Levels, Discarded}, OutNeighbors) of
            {[], SolutionAcc2, LevelsAcc2, DiscardedAcc2} ->
                {Vertices, SolutionAcc2, LevelsAcc2, DiscardedAcc2};
            {NewVertices1, SolutionAcc2, LevelsAcc2, DiscardedAcc2} ->
                {Vertices++[{Profile, Level+1, NewVertices1}]
                ,SolutionAcc2, LevelsAcc2, DiscardedAcc2}
        end.

maybe_add_to_solution(Profile, Level, Vertex, Key, Value, Vertices
                     ,Solution, Levels, Discarded, SolutionGraph) ->
    case dict:find(Key, Solution) of
        {ok, Value} -> % already seen
            {Vertices,
             Solution,
             Levels,
             Discarded};
        {ok, _} -> % conflict resolution!
            {Vertices,
             Solution,
             Levels,
             [Value|Discarded]};
        error ->
            add_to_solution_graph(Value, Vertex, SolutionGraph),
            {[Value | Vertices],
             dict:store(Key, {Profile, Value}, Solution),
             dict:store(Key, Level+1, Levels),
             Discarded}
    end.

subgraph(Graph, Vertices) ->
    digraph_utils:subgraph(Graph, Vertices).

maybe_add_to_dict(Key, Value, Dict) ->
    case dict:is_key(Key, Dict) of
        true ->
            Dict;
        false ->
            dict:store(Key, Value, Dict)
    end.

%% Track the profile (so we know where to install it), name/vsn of each dep
%% and the level it is from (for the lock file)
build_initial_dicts(Vertices) ->
    lists:foldl(fun({Profile, Level, Vs}, {Solution, Levels}) ->
                        lists:foldl(fun({Key, Vsn}, {SAcc, LAcc}) ->
                                            {maybe_add_to_dict(Key, {Profile, {Key,Vsn}}, SAcc),
                                             maybe_add_to_dict(Key, Level, LAcc)}
                                    end, {Solution, Levels}, Vs)
                end, {dict:new(), dict:new()}, Vertices).

add_to_solution_graph(_, _, none) ->
    ok;
add_to_solution_graph(N, V, SolutionGraph) ->
    NewV = digraph:add_vertex(SolutionGraph, N),
    digraph:add_edge(SolutionGraph, V, NewV).

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
