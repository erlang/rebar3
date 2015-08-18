-module(rebar_digraph).

-export([compile_order/1
        ,restore_graph/1
        ,cull_deps/2
        ,cull_deps/3
        ,subgraph/2
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
    Order =
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
        end,
    true = digraph:delete(Graph),
    Order.

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
    cull_deps(Graph, Vertices, sets:new()).

cull_deps(Graph, Vertices, Seen) ->
    Vertices1 = lists:keysort(2, Vertices),
    {Solution, Levels, Discarded} = {dict:new(), dict:new(), sets:new()},
    cull_deps(Graph, Vertices1, Levels, Solution, Seen, Discarded).

format_error(no_solution) ->
    io_lib:format("No solution for packages found.", []).

%%====================================================================
%% Internal Functions
%%====================================================================

cull_deps(_Graph, [], Levels, Solution, _, Discarded) ->
    {_, Vertices} = lists:unzip(dict:to_list(Solution)),
    LvlVertices = [{Profile, {Parent, App, Vsn, dict:fetch(App, Levels)}}
                  || {Profile, {Parent,App,Vsn}} <- Vertices],
    {ok, LvlVertices,  sets:to_list(Discarded)};
cull_deps(Graph, [{Profile, Level, Vs} | Vertices], Levels, Solution, Seen, Discarded) ->
    {NV, NS, LS, DS} =
        lists:foldl(fun({Parent, Name, Vsn}, {Acc, SolutionAcc, LevelsAcc, DiscardedAcc}) ->
                            {SolutionAcc1, LevelsAcc1, DiscardedAcc1} =
                                maybe_add_to_solution(Profile, Level, Name, {Name, Vsn}, Parent
                                                     ,SolutionAcc
                                                     ,LevelsAcc, Seen, DiscardedAcc),
                            OutNeighbors = digraph:out_neighbours(Graph, {Name,Vsn}),
                            {NewVertices, DiscardedAcc2} = handle_neighbors(Profile, Level, Name
                                                          ,OutNeighbors, Acc, SolutionAcc1
                                                                           ,Seen, DiscardedAcc1),
                            {NewVertices, SolutionAcc1, LevelsAcc1, DiscardedAcc2}
                    end, {[], Solution, Levels, Discarded}, Vs),
    NewVertices = combine_profile_levels(Vertices, NV),
    cull_deps(Graph, NewVertices, LS, NS, Seen, DS).

%% Combine lists of deps that have the same profile and level
combine_profile_levels(Vertices, NewVertices) ->
    V = lists:foldl(fun({Profile, Level, Vs}, Acc) ->
                            case ec_lists:find(fun({P, L, _}) ->
                                                       P =:= Profile andalso L =:= Level
                                               end, Acc) of
                            {ok, {_, _, OldVs}=Old} ->
                                lists:delete(Old, Acc)++[{Profile, Level, lists:keysort(1, OldVs++Vs)}];
                            error ->
                                Acc++[{Profile, Level, Vs}]
                        end
                end, Vertices, NewVertices),
    lists:keysort(2, V).

%% For each outgoing edge of a dep check if it should be added to the solution
%% and add it to the list of vertices to do the same for
handle_neighbors(Profile, Level, Parent, OutNeighbors, Vertices
                ,Solution, Seen, Discarded) ->
    case lists:foldl(fun({Name, Vsn}=Value, {NewVertices, Discarded1}) ->
                             case dict:find(Name, Solution) of
                                 {ok, {Profile, {Parent, Name, Vsn}}} -> % already seen
                                     {NewVertices,
                                      Discarded1};
                                 {ok, _} -> % conflict resolution!
                                     %% Warn on different version
                                     {NewVertices,
                                      sets:add_element(Value, Discarded1)};
                                 error ->
                                     %% We check Seen separately because we don't care
                                     %% to warn if the exact same version of a package
                                     %% was already part of the solution but we do
                                     %% if it was simply seen in source deps
                                     case sets:is_element(Name, Seen) of
                                         true ->
                                             {NewVertices,
                                              sets:add_element(Value, Discarded1)};
                                         false ->
                                             {[{Parent, Name, Vsn} | NewVertices],
                                              Discarded1}
                                     end
                             end
                    end, {[], Discarded}, OutNeighbors) of
            {[], DiscardedAcc2} ->
                {Vertices, DiscardedAcc2};
            {NewVertices1, DiscardedAcc2} ->
                {Vertices++[{Profile, Level+1, NewVertices1}] ,DiscardedAcc2}
        end.

maybe_add_to_solution(Profile, Level, Key, {Name, Vsn}=Value, Parent
                     ,Solution, Levels, Seen, Discarded) ->
    case dict:find(Key, Solution) of
        {ok, {Profile, {Parent, Name, Vsn}}} -> % already seen
            {Solution,
             Levels,
             Discarded};
        {ok, _} -> % conflict resolution!
            %% Warn on different version
            {Solution,
             Levels,
             sets:add_element(Value, Discarded)};
        error ->
            %% We check Seen separately because we don't care to warn if the exact
            %% same version of a package was already part of the solution but we do
            %% if it was simply seen in source deps
            case sets:is_element(Name, Seen) of
                true ->
                    {Solution,
                     Levels,
                     sets:add_element(Value, Discarded)};
                false ->
                    {dict:store(Key, {Profile, {Parent, Name, Vsn}}, Solution),
                     dict:store(Key, Level, Levels),
                     Discarded}
            end
    end.

subgraph(Graph, Vertices) ->
    digraph_utils:subgraph(Graph, Vertices).

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
