%% Test suite for the handling hexpm repo configurations
-module(rebar_pkg_repos_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("rebar.hrl").

all() ->
    [default_repo, repo_merging, repo_replacing, {group, resolve_version}].

groups() ->
    [{resolve_version, [use_first_repo_match, use_exact_with_hash, fail_repo_update,
                        ignore_match_in_excluded_repo]}].

init_per_group(resolve_version, Config) ->
    Repo1 = <<"test-repo-1">>,
    Repo2 = <<"test-repo-2">>,
    Repo3 = <<"test-repo-3">>,
    Hexpm = <<"hexpm">>,
    Repos = [Repo1, Repo2, Repo3, Hexpm],

    Deps = [{"A", "0.1.1", <<"good checksum">>, Repo1},
            {"A", "0.1.1", <<"good checksum">>, Repo2},
            {"B", "1.0.0", Repo1},
            {"B", "2.0.0", Repo2},
            {"B", "1.4.0", Repo3},
            {"B", "1.4.3", Hexpm},
            {"C", "1.3.1", <<"bad checksum">>, Repo1},
            {"C", "1.3.1", <<"good checksum">>, Repo2}],
    [{deps, Deps}, {repos, Repos} | Config];
init_per_group(_, Config) ->
    Config.

end_per_group(_, _) ->
    ok.

init_per_testcase(use_first_repo_match, Config) ->
    Deps = ?config(deps, Config),
    Repos = ?config(repos, Config),
    State = setup_deps_and_repos(Deps, Repos),

    meck:new(rebar_packages, [passthrough, no_link]),

    %% fail when the first repo is updated since it doesn't have a matching package
    %% should continue anyway
    meck:expect(rebar_packages, update_package,
                fun(_, _, _State) -> ok  end),
    meck:expect(rebar_packages, verify_table,
                fun(_State) -> true end),

    [{state, State} | Config];
init_per_testcase(use_exact_with_hash, Config) ->
    Deps = ?config(deps, Config),
    Repos = ?config(repos, Config),
    State = setup_deps_and_repos(Deps, Repos),

    meck:new(rebar_packages, [passthrough, no_link]),

    %% fail when the first repo is updated since it doesn't have a matching package
    %% should continue anyway
    meck:expect(rebar_packages, update_package,
                fun(_, _, _State) -> ok  end),
    meck:expect(rebar_packages, verify_table,
                fun(_State) -> true end),

    [{state, State} | Config];
init_per_testcase(fail_repo_update, Config) ->
    Deps = ?config(deps, Config),
    Repos = ?config(repos, Config),
    State = setup_deps_and_repos(Deps, Repos),

    meck:new(rebar_packages, [passthrough, no_link]),

    %% fail when the first repo is updated since it doesn't have a matching package
    %% should continue anyway
    [Repo1 | _] = Repos,
    meck:expect(rebar_packages, update_package,
                fun(_, #{name := Repo}, _State) when Repo =:= Repo1 -> fail;
                   (_, _, _State) -> ok  end),
    meck:expect(rebar_packages, verify_table,
                fun(_State) -> true end),

    [{state, State} | Config];
init_per_testcase(ignore_match_in_excluded_repo, Config) ->
    Deps = ?config(deps, Config),
    Repos = [Repo1, _, Repo3 | _] = ?config(repos, Config),

    %% drop repo1 and repo2 from the repos to be used by the pkg resource
    State = setup_deps_and_repos(Deps, [R || R <- Repos, R =/= Repo3, R =/= Repo1]),

    meck:new(rebar_packages, [passthrough, no_link]),

    %% fail when the first repo is updated since it doesn't have a matching package
    %% should continue anyway
    [_, _, Repo3 | _] = Repos,
    meck:expect(rebar_packages, update_package,
                fun(_, _, _State) -> ok  end),
    meck:expect(rebar_packages, verify_table,
                fun(_State) -> true end),

    [{state, State} | Config];
init_per_testcase(_, Config) ->
    Config.

end_per_testcase(Case, _Config) when Case =:= use_first_repo_match ;
                                     Case =:= use_exact_with_hash ;
                                     Case =:= fail_repo_update ;
                                     Case =:= ignore_match_in_excluded_repo ->
    meck:unload(rebar_packages);
end_per_testcase(_, _) ->
    ok.


default_repo(_Config) ->
    Repo1 = #{name => <<"hexpm">>,
              api_key => <<"asdf">>},

    MergedRepos = rebar_pkg_resource:repos([{repos, [Repo1]}]),

    ?assertMatch([#{name := <<"hexpm">>,
                    api_key := <<"asdf">>,
                    api_url := <<"https://hex.pm/api">>}], MergedRepos).


repo_merging(_Config) ->
    Repo1 = #{name => <<"repo-1">>,
              api_url => <<"repo-1/api">>},
    Repo2 = #{name => <<"repo-2">>,
              repo_url => <<"repo-2/repo">>,
              repo_verify => false},
    Result = rebar_pkg_resource:merge_repos([Repo1, Repo2,
                                             #{name => <<"repo-2">>,
                                               api_url => <<"repo-2/api">>,
                                               repo_url => <<"bad url">>,
                                               repo_verify => true},
                                             #{name => <<"repo-1">>,
                                               api_url => <<"bad url">>,
                                               repo_verify => true},
                                             #{name => <<"repo-2">>,
                                               organization => <<"repo-2-org">>,
                                               api_url => <<"repo-2/api-2">>,
                                               repo_url => <<"other/repo">>}]),
    ?assertMatch([#{name := <<"repo-1">>,
                    api_url := <<"repo-1/api">>,
                    repo_verify := true},
                  #{name := <<"repo-2">>,
                    api_url := <<"repo-2/api">>,
                    repo_url := <<"repo-2/repo">>,
                    organization := <<"repo-2-org">>,
                    repo_verify := false}], Result).

repo_replacing(_Config) ->
    Repo1 = #{name => <<"repo-1">>,
              api_url => <<"repo-1/api">>},
    Repo2 = #{name => <<"repo-2">>,
              repo_url => <<"repo-2/repo">>,
              repo_verify => false},

    ?assertMatch([Repo1, Repo2, #{name := <<"hexpm">>}],
                 rebar_pkg_resource:repos([{repos, [Repo1]},
                                           {repos, [Repo2]}])),

    %% use of replace is ignored if found in later entries than the first
    ?assertMatch([Repo1, Repo2, #{name := <<"hexpm">>}],
                 rebar_pkg_resource:repos([{repos, [Repo1]},
                                           {repos, replace, [Repo2]}])),

    ?assertMatch([Repo1],
                 rebar_pkg_resource:repos([{repos, replace, [Repo1]},
                                           {repos, [Repo2]}])).



use_first_repo_match(Config) ->
    State = ?config(state, Config),

    ?assertMatch({ok,{package,{<<"B">>, <<"2.0.0">>, Repo2},
                      <<"some checksum">>, false, []},
                  #{name := Repo2,
                    http_adapter_config := #{profile := rebar}}},
                 rebar_packages:resolve_version(<<"B">>, <<"> 1.4.0">>, undefined,
                                                ?PACKAGE_TABLE, State)),

    ?assertMatch({ok,{package,{<<"B">>, <<"1.4.0">>, Repo3},
                      <<"some checksum">>, false, []},
                  #{name := Repo3,
                    http_adapter_config := #{profile := rebar}}},
                 rebar_packages:resolve_version(<<"B">>, <<"~> 1.4.0">>, undefined,
                                                ?PACKAGE_TABLE, State)).

%% tests that even though an easier repo has C-1.3.1 it doesn't use it since its hash is different
use_exact_with_hash(Config) ->
    State = ?config(state, Config),

    ?assertMatch({ok,{package,{<<"C">>, <<"1.3.1">>, Repo2},
                      <<"good checksum">>, false, []},
                  #{name := Repo2,
                    http_adapter_config := #{profile := rebar}}},
                 rebar_packages:resolve_version(<<"C">>, <<"1.3.1">>, <<"good checksum">>,
                                                ?PACKAGE_TABLE, State)).

fail_repo_update(Config) ->
    State = ?config(state, Config),

    ?assertMatch({ok,{package,{<<"B">>, <<"1.4.0">>, Repo3},
                      <<"some checksum">>, false, []},
                  #{name := Repo3,
                    http_adapter_config := #{profile := rebar}}},
                 rebar_packages:resolve_version(<<"B">>, <<"~> 1.4.0">>, undefined,
                                                ?PACKAGE_TABLE, State)).

ignore_match_in_excluded_repo(Config) ->
    State = ?config(state, Config),
    Repos = ?config(repos, Config),

    ?assertMatch({ok,{package,{<<"B">>, <<"1.4.3">>, Hexpm},
                      <<"some checksum">>, false, []},
                  #{name := Hexpm,
                    http_adapter_config := #{profile := rebar}}},
                 rebar_packages:resolve_version(<<"B">>, <<"~> 1.4.0">>, undefined,
                                                ?PACKAGE_TABLE, State)),

    [_, Repo2 | _] = Repos,
    ?assertMatch({ok,{package,{<<"A">>, <<"0.1.1">>, Repo2},
                      <<"good checksum">>, false, []},
                  #{name := Repo2,
                    http_adapter_config := #{profile := rebar}}},
                 rebar_packages:resolve_version(<<"A">>, <<"0.1.1">>, <<"good checksum">>,
                                                ?PACKAGE_TABLE, State)).

%%

setup_deps_and_repos(Deps, Repos) ->
    true = rebar_packages:new_package_table(),
    insert_deps(Deps),
    State = rebar_state:new([{hex, [{repos, [#{name => R} || R <- Repos]}]}]),
    rebar_state:create_resources([{pkg, rebar_pkg_resource}], State).


insert_deps(Deps) ->
    lists:foreach(fun({Name, Version, Repo}) ->
                          ets:insert(?PACKAGE_TABLE, #package{key={rebar_utils:to_binary(Name),
                                                                   rebar_utils:to_binary(Version),
                                                                   rebar_utils:to_binary(Repo)},
                                                              dependencies=[],
                                                              retired=false,
                                                              checksum = <<"some checksum">>});
                     ({Name, Version, Checksum, Repo}) ->
                          ets:insert(?PACKAGE_TABLE, #package{key={rebar_utils:to_binary(Name),
                                                                   rebar_utils:to_binary(Version),
                                                                   rebar_utils:to_binary(Repo)},
                                                              dependencies=[],
                                                              retired=false,
                                                              checksum = Checksum})
                  end, Deps).
