-module(rebar_hex_repos).

-export([from_state/2,
         get_repo_config/2,
         auth_config/1,
         update_auth_config/2,
         format_error/1]).

-ifdef(TEST).
%% exported for test purposes
-export([repos/1, merge_repos/1]).
-endif.

-include("rebar.hrl").
-include_lib("providers/include/providers.hrl").

-export_type([repo/0]).

-type repo() :: #{name => unicode:unicode_binary(),
                  api_url => binary(),
                  api_key => binary(),
                  repo_url => binary(),
                  repo_public_key => binary(),
                  repo_verify => binary()}.

from_state(BaseConfig, State) ->
    HexConfig = rebar_state:get(State, hex, []),
    Repos = repos(HexConfig),
    %% auth is stored in a separate config file since the plugin generates and modifies it
    Auth = ?MODULE:auth_config(State),
    %% add base config entries that are specific to use by rebar3 and not overridable
    Repos1 = merge_with_base_and_auth(Repos, BaseConfig, Auth),
    %% merge organizations parent repo options into each oraganization repo
    update_organizations(Repos1).

-spec get_repo_config(unicode:unicode_binary(), rebar_state:t() | [repo()])
                     -> {ok, repo()} | error.
get_repo_config(RepoName, Repos) when is_list(Repos) ->
    case ec_lists:find(fun(#{name := N}) -> N =:= RepoName end, Repos) of
        error ->
            throw(?PRV_ERROR({repo_not_found, RepoName}));
        {ok, RepoConfig} ->
            {ok, RepoConfig}
    end;
get_repo_config(RepoName, State) ->
    Resources = rebar_state:resources(State),
    #{repos := Repos} = rebar_resource_v2:find_resource_state(pkg, Resources),
    get_repo_config(RepoName, Repos).

merge_with_base_and_auth(Repos, BaseConfig, Auth) ->
    [maps:merge(maps:get(maps:get(name, Repo), Auth, #{}),
                maps:merge(Repo, BaseConfig)) || Repo <- Repos].

%% A user's list of repos are merged by name while keeping the order
%% intact. The order is based on the first use of a repo by name in the
%% list. The default repo is appended to the user's list.
repos(HexConfig) ->
    HexDefaultConfig = default_repo(),
    case [R || R <- HexConfig, element(1, R) =:= repos] of
        [] ->
            [HexDefaultConfig];
        %% we only care if the first element is a replace entry
        [{repos, replace, Repos} | _]->
            merge_repos(Repos);
        Repos ->
            RepoList = repo_list(Repos),
            merge_repos(RepoList ++ [HexDefaultConfig])
    end.

-spec merge_repos([repo()]) -> [repo()].
merge_repos(Repos) ->
    lists:foldl(fun(R=#{name := Name}, ReposAcc) ->
                        %% private organizations include the parent repo before a :
                        case rebar_string:split(Name, <<":">>) of
                            [Repo, Org] ->
                                update_repo_list(R#{name => Name,
                                                    organization => Org,
                                                    parent => Repo}, ReposAcc);
                            _ ->
                                update_repo_list(R, ReposAcc)
                        end
                end, [], Repos).

update_organizations(Repos) ->
    lists:map(fun(Repo=#{organization := Organization,
                         parent := ParentName}) ->
                      {ok, Parent} = get_repo_config(ParentName, Repos),
                      ParentRepoUrl = rebar_utils:to_list(maps:get(repo_url, Parent)),
                      {ok, RepoUrl} =
                          rebar_utils:url_append_path(ParentRepoUrl,
                                                      filename:join("repos", rebar_utils:to_list(Organization))),
                      %% still let the organization config override this constructed repo url
                      maps:merge(Parent#{repo_url => rebar_utils:to_binary(RepoUrl)}, Repo);
                 (Repo) ->
                      Repo
              end, Repos).

update_repo_list(R=#{name := N}, [H=#{name := HN} | Rest]) when N =:= HN ->
    [maps:merge(R, H) | Rest];
update_repo_list(R, [H | Rest]) ->
    [H | update_repo_list(R, Rest)];
update_repo_list(R, []) ->
    [R].

default_repo() ->
    HexDefaultConfig = hex_core:default_config(),
    HexDefaultConfig#{name => ?PUBLIC_HEX_REPO}.

repo_list([]) ->
    [];
repo_list([{repos, Repos} | T]) ->
    Repos ++ repo_list(T);
repo_list([{repos, replace, Repos} | T]) ->
    Repos ++ repo_list(T).

format_error({repo_not_found, RepoName}) ->
    io_lib:format("The repo ~ts was not found in the configuration.", [RepoName]).

%% auth functions

%% authentication is in a separate config file because the hex plugin updates it

-spec auth_config_file(rebar_state:t()) -> file:filename_all().
auth_config_file(State) ->
    filename:join(rebar_dir:global_config_dir(State), ?HEX_AUTH_FILE).

-spec auth_config(rebar_state:t()) -> map().
auth_config(State) ->
    case file:consult(auth_config_file(State)) of
        {ok, [Config]} ->
            Config;
        _ ->
            #{}
    end.

-spec update_auth_config(map(), rebar_state:t()) -> ok.
update_auth_config(Updates, State) ->
    Config = auth_config(State),
    AuthConfigFile = auth_config_file(State),
    ok = filelib:ensure_dir(AuthConfigFile),
    NewConfig = iolist_to_binary([io_lib:print(maps:merge(Config, Updates)) | ".\n"]),
    ok = file:write_file(AuthConfigFile, NewConfig).
