%%% @doc build a digraph of applications in order to figure out dependency
%%% and compile order.
-module(rebar_digraph).

-export([compile_order/1
        ,restore_graph/1
        ,subgraph/2
        ,format_error/1]).

-include("rebar.hrl").

%% @doc Sort apps with topological sort to get proper build order
-spec compile_order([rebar_app_info:t()]) ->
    {ok, [rebar_app_info:t()]} | {error, no_sort | {cycles, [[binary(),...]]}}.
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

%% @private Add a package and its dependencies to an existing digraph
-spec add(digraph:graph(), {PkgName, [Dep]}) -> ok when
      PkgName :: binary(),
      Dep :: {Name, term()} | Name,
      Name :: atom() | iodata().
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
                                          rebar_utils:to_binary(Name);
                                      Name ->
                                          rebar_utils:to_binary(Name)
                                  end,
                          V3 = case digraph:vertex(Graph, Name1) of
                                   false ->
                                       digraph:add_vertex(Graph, Name1);
                                   {V2, []} ->
                                       V2
                               end,
                          digraph:add_edge(Graph, V, V3)
                  end, Deps).

%% @doc based on a list of vertices and edges, build a digraph.
-spec restore_graph({[digraph:vertex()], [digraph:edge()]}) -> digraph:graph().
restore_graph({Vs, Es}) ->
    Graph = digraph:new(),
    lists:foreach(fun({V, LastUpdated}) ->
                          digraph:add_vertex(Graph, V, LastUpdated)
                  end, Vs),
    lists:foreach(fun({V1, V2}) ->
                          digraph:add_edge(Graph, V1, V2)
                  end, Es),
    Graph.

%% @doc convert a given exception's payload into an io description.
-spec format_error(any()) -> iolist().
format_error(no_solution) ->
    io_lib:format("No solution for packages found.", []).

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc alias for `digraph_utils:subgraph/2'.
subgraph(Graph, Vertices) ->
    digraph_utils:subgraph(Graph, Vertices).

%% @private from a list of app names, fetch the proper app info records
%% for them.
-spec names_to_apps([atom()], [rebar_app_info:t()]) -> [rebar_app_info:t()].
names_to_apps(Names, Apps) ->
    [element(2, App) || App <- [find_app_by_name(Name, Apps) || Name <- Names], App =/= error].

%% @private fetch the proper app info record for a given app name.
-spec find_app_by_name(atom(), [rebar_app_info:t()]) -> {ok, rebar_app_info:t()} | error.
find_app_by_name(Name, Apps) ->
    ec_lists:find(fun(App) ->
                          rebar_app_info:name(App) =:= Name
                  end, Apps).

%% @private The union of all entries in the applications list for an app and
%% the deps listed in its rebar.config is all deps that may be needed
%% for building the app.
-spec all_apps_deps(rebar_app_info:t()) -> [binary()].
all_apps_deps(App) ->
    Applications = lists:usort([atom_to_binary(X, utf8) || X <- rebar_app_info:applications(App)]),
    Deps = lists:usort(lists:map(fun({Name, _}) -> Name; (Name) -> Name end, rebar_app_info:deps(App))),
    lists:umerge(Deps, Applications).
