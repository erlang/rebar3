-module(rebar_packages).

-export([get_packages/1]).

-export_type([constraint/0]).

-include("rebar.hrl").

-type pkg_name() :: string() | binary() | atom().

-type vsn() :: 'NO_VSN'
             | ec_semver:semver().

-type constraint_op() ::
        '=' | gte | '>=' | lte | '<='
      | gt | '>' | lt | '<' | pes | '~>' | between.

-type constraint() :: pkg_name()
                    | {pkg_name(), vsn()}
                    | {pkg_name(), vsn(), constraint_op()}
                    | {pkg_name(), vsn(), vsn(), between}.


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
