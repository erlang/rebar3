-module(rebar_packages).

-export([get_packages/1]).

-export_type([package/0]).

-include("rebar.hrl").

-type pkg_name() :: binary() | atom().
-type vsn() :: binary().
-type package() :: pkg_name() | {pkg_name(), vsn()}.

-spec get_packages(rebar_state:t()) -> {rebar_dict(), rebar_digraph()}.
get_packages(State) ->
    Home = rebar_utils:home_dir(),
    RebarDir = rebar_state:get(State, global_rebar_dir, filename:join(Home, ?CONFIG_DIR)),
    PackagesFile = filename:join(RebarDir, "packages"),
    case ec_file:exists(PackagesFile) of
        true ->
            try
                {ok, Binary} = file:read_file(PackagesFile),
                {Dict, Graph} = binary_to_term(Binary),
                {Dict, rebar_digraph:restore_graph(Graph)}
            catch
                _:_ ->
                    ?ERROR("Bad packages index, try to fix with `rebar update`", []),
                    {dict:new(), digraph:new()}
            end;
        false ->
            {dict:new(), digraph:new()}
    end.
