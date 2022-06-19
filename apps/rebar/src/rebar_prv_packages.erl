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
    State1 = rebar_state:add_provider(State,
                                      providers:create([{name, ?PROVIDER},
                                                        {module, ?MODULE},
                                                        {bare, true},
                                                        {deps, ?DEPS},
                                                        {example, "rebar3 pkgs elli"},
                                                        {short_desc, "List information for a package."},
                                                        {desc, info("List information for a package")},
                                                        {opts, [{package, undefined, undefined, string,
                                                                 "Package to fetch information for."}]}])),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    case proplists:get_value(package, Args, undefined) of
        undefined ->
            ?PRV_ERROR(no_package_arg);
        Name ->
            Resources = rebar_state:resources(State),
            #{repos := Repos} = rebar_resource_v2:find_resource_state(pkg, Resources),
            Results = get_package(rebar_utils:to_binary(Name), Repos),
            case lists:all(fun({_, {error, not_found}}) -> true; (_) -> false end, Results) of
                true ->
                    ?PRV_ERROR({not_found, Name});
                false ->
                    [print_packages(Result) || Result <- Results],
                    {ok, State}
            end
    end.

-spec get_package(binary(), [map()]) -> [{binary(), {ok, map()} | {error, term()}}].
get_package(Name, Repos) ->
    lists:foldl(fun(RepoConfig, Acc) ->
                        [{maps:get(name, RepoConfig), rebar_packages:get(RepoConfig, Name)} | Acc]
                end, [], Repos).


-spec format_error(any()) -> iolist().
format_error(no_package_arg) ->
    "Missing package argument to `rebar3 pkgs` command.";
format_error({not_found, Name}) ->
    io_lib:format("Package ~ts not found in any repo.", [Name]);
format_error(unknown) ->
    "Something went wrong with fetching package metadata.".


print_packages({RepoName, {error, not_found}}) ->
    ?CONSOLE("~ts: Package not found in this repo.~n", [RepoName]);
print_packages({RepoName, {error, _}}) ->
    ?CONSOLE("~ts: Error fetching from this repo.~n", [RepoName]);
print_packages({RepoName, {ok, #{<<"name">> := Name,
                                 <<"meta">> := Meta,
                                 <<"releases">> := Releases}}}) ->
    Description = maps:get(<<"description">>, Meta, ""),
    Licenses = join(maps:get(<<"licenses">>, Meta, []), <<", ">>),
    Links = join_map(maps:get(<<"links">>, Meta, []), <<"\n        ">>),
    Versions = [V || #{<<"version">> := V} <- Releases],
    VsnStr = join(Versions, <<", ">>),
    ?CONSOLE("~ts:~n"
             "    Name: ~ts~n"
             "    Description: ~ts~n"
             "    Licenses: ~ts~n"
             "    Links:~n        ~ts~n"
             "    Versions: ~ts~n", [RepoName, Name, Description, Licenses, Links, VsnStr]);
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
