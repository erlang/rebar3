-module(rebar_hex_repos).

-export([from_state/2,
         get_repo_config/2,
         auth_config/1,
         remove_from_auth_config/2,
         update_auth_config/2,
         format_error/1,
         anon_repo_config/1,
         format_repo/1
        ]).

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
                  repo_key => binary(),
                  repo_public_key => binary(),
                  repo_verify => binary(),
                  repo_verify_origin => binary(),
                  mirror_of => _ % legacy field getting stripped
                 }.

from_state(BaseConfig, State) ->
    HexConfig = rebar_state:get(State, hex, []),
    Repos = repos(HexConfig),
    %% auth is stored in a separate config file since the plugin generates and modifies it
    Auth = ?MODULE:auth_config(State),
    %% add base config entries that are specific to use by rebar3 and not overridable
    Repos1 = merge_with_base_and_auth(Repos, BaseConfig, Auth),
    %% merge organizations parent repo options into each oraganization repo
    update_organizations(maybe_override_default_repo_url(Repos1, State)).

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

-spec anon_repo_config(repo()) ->
    #{api_url := _, name := _, repo_name => _, repo_organization => _,
      repo_url := _, repo_verify => _, repo_verify_origin => _,
      mirror_of => _}.
anon_repo_config(Map) ->
    maps:with([name, repo_name, api_url, repo_url, repo_organization,
               mirror_of, repo_verify, repo_verify_origin], Map).

-spec format_repo(repo()) -> unicode:chardata().
format_repo(RepoConfig) ->
    Name = maps:get(name, RepoConfig, undefined),
    case get({?MODULE, format_repo, Name}) of
        undefined ->
            put({?MODULE, format_repo, Name}, true),
            Anon = anon_repo_config(RepoConfig),
            io_lib:format("~ts (~p)", [Name, Anon]);
        true ->
            io_lib:format("~ts", [Name])
    end.

merge_with_base_and_auth(Repos, BaseConfig, Auth) ->
    [maps:merge(maps:merge(Repo, BaseConfig),
                maps:get(maps:get(name, Repo), Auth, #{})) || Repo <- Repos].

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

%% merge repos must add a field repo_name to work with r3_hex_core 0.5.0
-spec merge_repos([repo()]) -> [repo()].
merge_repos(Repos) ->
    lists:foldl(fun(R = #{name := Name}, ReposAcc) ->
                        %% private orgs are in the format of <<"parent:org">>
                        case rebar_string:split(Name, <<":">>) of
                            [Repo, Org] ->

                                %% We set the repo_organization and api_organization to org
                                %% for fetching and publishing private packages.
                                update_repo_list(R#{name => Name,
                                                    repo_name => Org,
                                                    repo_organization => Org,
                                                    api_organization => Org,
                                                    api_repository => Org,
                                                    parent => Repo}, ReposAcc);
                            _ ->
                                update_repo_list(R#{repo_name => Name}, ReposAcc)
                        end
                end, [], Repos).

update_organizations(Repos) ->
    lists:map(fun(Repo=#{repo_name := RepoName,
                         parent := ParentName}) ->
                      {ok, Parent} = get_repo_config(ParentName, Repos),
                      ParentRepoUrl = rebar_utils:to_list(maps:get(repo_url, Parent)),
                      {ok, _RepoUrl} =
                          rebar_uri:append_path(ParentRepoUrl,
                                                filename:join("repos", rebar_utils:to_list(RepoName))),
                      %% still let the organization config override this constructed repo url
                      maps:merge(Parent#{repo_url => rebar_utils:to_binary(ParentRepoUrl)}, Repo);
                 (Repo) ->
                      Repo
              end, Repos).

maybe_override_default_repo_url(Repos, State) ->
    lists:map(fun(Repo=#{repo_name := ?PUBLIC_HEX_REPO}) ->
                      Repo#{repo_url => rebar_state:default_hex_repo_url_override(State)};
                 (Repo=#{name := ?PUBLIC_HEX_REPO}) ->
                      Repo#{repo_url => rebar_state:default_hex_repo_url_override(State)};
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
    HexDefaultConfig = r3_hex_core:default_config(),
    HexDefaultConfig#{name => ?PUBLIC_HEX_REPO, repo_verify_origin => repo_verify_origin()}.

repo_verify_origin() ->
    case os:getenv("REBAR_NO_VERIFY_REPO_ORIGIN") of
        "1" -> false;
        _ -> true
    end.

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
    AuthFile = auth_config_file(State),
    case file:consult(AuthFile) of
        {ok, [Config]} ->
            Config;
        {error, Reason} when is_atom(Reason) ->
            case Reason of
                enoent ->
                    #{};
                _ ->
                    % TODO: map to an english reason
                    ?ABORT("Error reading repos auth config (~ts) : ~ts", [AuthFile, atom_to_list(Reason)])
            end;
        {error, {_Line, _Mod, _Term} = Err} ->
            Reason = file:format_error(Err),
            ?ABORT("Error found in repos auth config (~ts) at line ~ts", [AuthFile, Reason])
    end.

-spec remove_from_auth_config(term(), rebar_state:t()) -> ok.
remove_from_auth_config(Key, State) ->
    Updated = maps:remove(Key, auth_config(State)),
    write_auth_config(Updated, State).

-spec update_auth_config(map(), rebar_state:t()) -> ok.
update_auth_config(Updates, State) ->
    Updated = maps:merge(auth_config(State), Updates),
    write_auth_config(Updated, State).

write_auth_config(Config, State) ->
    AuthConfigFile = auth_config_file(State),
    ok = filelib:ensure_dir(AuthConfigFile),
    NewConfig = iolist_to_binary(["%% coding: utf-8", io_lib:nl(),
                                  io_lib:print(Config), ".", io_lib:nl()]),
    ok = file:write_file(AuthConfigFile, NewConfig, [{encoding, utf8}]).
