-module(rebar_prv_packages).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-include("rebar.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("providers/include/providers.hrl").

-define(PROVIDER, pkgs).
-define(DEPS, []).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    State1 = rebar_state:add_provider(State, providers:create([{name, ?PROVIDER},
                                                               {module, ?MODULE},
                                                               {bare, true},
                                                               {deps, ?DEPS},
                                                               {example, "rebar3 pkgs"},
                                                               {short_desc, "List versions of a package."},
                                                               {desc, info("List versions of a package")},
                                                               {opts, []}])),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Resources = rebar_state:resources(State),
    #{hex_config := HexConfig} = rebar_resource:find_resource_state(pkg, Resources),
    case rebar_state:command_args(State) of
        [Name] ->            
            print_packages(rebar_packages:get(HexConfig, rebar_utils:to_binary(Name)));
        _ ->
            ?ERROR("Must provide a package name.", [])
    end,
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(load_registry_fail) ->
    "Failed to load package regsitry. Try running 'rebar3 update' to fix".

print_packages({ok, #{<<"name">> := Name,
                      <<"meta">> := Meta,
                      <<"releases">> := Releases}}) ->
    Description = maps:get(<<"description">>, Meta, ""),
    Licenses = join(maps:get(<<"licenses">>, Meta, []), <<", ">>),
    Links = join_map(maps:get(<<"links">>, Meta, []), <<"\n        ">>),
    Maintainers = join(maps:get(<<"maintainers">>, Meta, []), <<", ">>),
    Versions = [V || #{<<"version">> := V} <- Releases],
    VsnStr = join(Versions, <<", ">>),
    ?CONSOLE("~ts:~n"
             "    Description: ~ts~n"
             "    Licenses: ~ts~n"
             "    Maintainers: ~ts~n"
             "    Links:~n        ~ts~n"
             "    Versions: ~ts~n", [Name, Description, Licenses, Maintainers, Links, VsnStr]);
print_packages(_) ->
    ok.

-spec join([binary()], binary()) -> binary().
join([Bin], _Sep) ->
    <<Bin/binary>>;
join([Bin | T], Sep) ->
    <<Bin/binary, Sep/binary, (join(T, Sep))/binary>>.

-spec join_map(map(), binary()) -> binary().
join_map(Map, Sep) ->
    join_tuple_list(maps:to_list(Map), Sep).

join_tuple_list([{K, V}], _Sep) ->
    <<K/binary, ": ", V/binary>>;
join_tuple_list([{K, V} | T], Sep) ->
    <<K/binary, ": ", V/binary, Sep/binary, (join_tuple_list(T, Sep))/binary>>.

info(Description) ->
    io_lib:format("~ts.~n", [Description]).
