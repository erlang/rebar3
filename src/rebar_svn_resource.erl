%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
-module(rebar_svn_resource).

-behaviour(rebar_resource_v2).

-export([init/2,
         lock/2,
         download/4,
         needs_update/2,
         make_vsn/2]).


%% For backward compatibilty
-export([ download/3
        ]).

-include("rebar.hrl").

-spec init(atom(), rebar_state:t()) -> {ok, rebar_resource_v2:resource()}.
init(Type, _State) ->
    Resource = rebar_resource_v2:new(Type, ?MODULE, #{}),
    {ok, Resource}.

lock(AppInfo, _) ->
    check_type_support(),
    lock_(rebar_app_info:dir(AppInfo), rebar_app_info:source(AppInfo)).

lock_(AppDir, {svn, Url, {tag, Tag}}) ->
    TagUrl = make_tag_url(Url,Tag),
    lock_(AppDir, {svn, TagUrl});
lock_(AppDir, {svn, Url, {branch, Branch}}) ->
    BranchUrl = make_branch_url(Url,Branch),
    lock_(AppDir, {svn, BranchUrl});
lock_(AppDir, {svn, Url, _}) ->
    lock_(AppDir, {svn, Url});
lock_(AppDir, {svn, Url}) ->
    Rev = local_svn_revision(AppDir),
    {svn, Url, {rev, Rev}}.

%% Return `true' if either the svn url or tag/branch/vsn is not the same as
%% the currently checked out repo for the dep
needs_update(AppInfo, _) ->
    %%?INFO("RUNNING NEEDS UPDATE ~ts ~p", [rebar_app_info:dir(AppInfo), rebar_app_info:source(AppInfo)]),
    needs_update_(rebar_app_info:dir(AppInfo), rebar_app_info:source(AppInfo)).

needs_update_(Dir, {svn, Url, {tag, Tag}}) ->
    {ok, Current} = local_svn_tag(Dir),
    TagUrl = make_tag_url(Url,Tag),
    %%?INFO("Comparing svn tag ~ts with ~ts, Dir ~ts with TagUrl ~ts", [Tag, Current, Dir, TagUrl]),
    
    not ((Tag =:= Current) andalso compare_url(Dir, TagUrl));
needs_update_(Dir, {svn, Url, {branch, Branch}}) ->
    %% Need to check remote side to see if anything changed
    BranchUrl = make_branch_url(Url, Branch),
    Remote = remote_svn_revision(BranchUrl),
    Local = local_svn_revision(Dir),
    %%?INFO("Comparing svn Branch ~ts with ~ts", [Remote, Local]),
    not ((Remote =:= Local) andalso compare_url(Dir, BranchUrl));
needs_update_(Dir, {svn, Url, "HEAD"}) ->
    Remote = remote_svn_revision(Url),
    Local = local_svn_revision(Dir),
    %%%?INFO("HEAD svn compare ~ts with ~ts", [Remote, Local]),
    not ((Remote =:= Local) andalso compare_url(Dir, Url));
needs_update_(Dir, {svn, Url, {rev, Rev}}) ->
    LocalRev = local_svn_revision(Dir),
    %%?INFO("Comparing svn rev ~ts with ~ts, Dir ~ts with Url ~ts", [Rev, LocalRev, Dir, Url]),
    not ((LocalRev =:= Rev) andalso compare_url(Dir, Url));
needs_update_(Dir, {svn, Url, Rev}) ->
    Local = local_svn_revision(Dir),
    %%?INFO("Comparing default svn rev ~ts with ~ts", [Rev, Local]),
    not ((Rev =:= Local) andalso compare_url(Dir, Url)).

download(TmpDir, AppInfo, State, _) ->
    check_type_support(),
    case download_(TmpDir, rebar_app_info:source(AppInfo), State) of
        {ok, _} ->
            ok;
        {error, Reason} ->
            {error, Reason};
        Error ->
            {error, Error}
    end.

%% For backward compatibilty
download(Dir, AppInfo, State) ->
    download_(Dir, AppInfo, State).

download_(Dir, {svn, Url}, State) ->
    ?WARN("WARNING: It is recommended to use {branch, Name}, {tag, Tag} or {rev, Rev}, otherwise updating the dep may not work as expected.", []),
    download_(Dir, {svn, Url, {rev, "HEAD"}}, State);
download_(Dir, {svn, Url, {branch, Branch}}, _State) ->
    ok = filelib:ensure_dir(Dir),
    maybe_warn_local_url(Url),
    BranchUrl = make_branch_url(Url, Branch),
    rebar_utils:sh(?FMT("svn co -q ~ts ~ts",
                       [rebar_utils:escape_chars(BranchUrl),
                        rebar_utils:escape_chars(filename:basename(Dir))]),
                   [{cd, filename:dirname(Dir)}]);
download_(Dir, {svn, Url, {tag, Tag}}, _State) ->
    ok = filelib:ensure_dir(Dir),
    maybe_warn_local_url(Url),
    TagUrl = make_tag_url(Url,Tag),
    rebar_utils:sh(?FMT("svn co -q ~ts ~ts",
                        [rebar_utils:escape_chars(TagUrl),
                         rebar_utils:escape_chars(filename:basename(Dir))]),
                   [{cd, filename:dirname(Dir)}]);
download_(Dir, {svn, Url, {rev, Rev}}, _State) ->
    ok = filelib:ensure_dir(Dir),
    maybe_warn_local_url(Url),
    rebar_utils:sh(?FMT("svn co -q -r ~ts ~ts ~ts",
                        [rebar_utils:escape_chars(Rev),
                         rebar_utils:escape_chars(Url),
                         rebar_utils:escape_chars(filename:basename(Dir))]),
                   [{cd, filename:dirname(Dir)}]);
download_(Dir, {svn, Url, Rev}, State) ->
    download_(Dir, {svn, Url, {rev, Rev}}, State).

make_vsn(AppInfo, _) ->
    check_type_support(),
    make_vsn_(rebar_app_info:dir(AppInfo)).

make_vsn_(Dir) ->
    Vsn = local_svn_revision(Dir),
    {plain, Vsn}.

%%% Internal functions

make_branch_url(Url, Branch) ->
    {Scheme, Host, Path} = parse_svn_url(Url),
    case lists:reverse(Path) of
        [_Branch, "branches"| Tail] ->
            ?WARN("Specified branch (~ts) as well as a branch based URL. Using specified branch",
                  [Branch]),
            Path1 = lists:reverse([Branch,  "branches" | Tail]),
            make_svn_url(Scheme, Host, Path1);
        ["branches"| Tail] ->
            Path1 = lists:reverse([Branch,  "branches" | Tail]),
            make_svn_url(Scheme, Host, Path1);
        ["trunk"| Tail] ->
            ?WARN("Specified branch (~ts) on top of trunk URL. Using specified branch", [Branch]),
            Path1 = lists:reverse([Branch,  "branches" | Tail]),
            make_svn_url(Scheme, Host, Path1);
        RevPath ->
            Path1 = lists:reverse([Branch,  "branches" | RevPath]),
            make_svn_url(Scheme, Host, Path1)
    end.

make_tag_url(Url, Tag) ->
    {Scheme, Host, Path} = parse_svn_url(Url),
    case lists:reverse(Path) of
        [_Tag, "tags"| Tail] ->
            ?WARN("Specified tag (~ts) as well as a tag based URL. Using specified tag", [Tag]),
            Path1 = lists:reverse([Tag,  "tags" | Tail]),
            make_svn_url(Scheme, Host, Path1);
        ["tags"| Tail] ->
            Path1 = lists:reverse([Tag,  "tags" | Tail]),
            make_svn_url(Scheme, Host, Path1);
        ["trunk"| Tail] ->
            ?WARN("Specified tag (~ts) on top of trunk URL. Using specified tag", [Tag]),
            Path1 = lists:reverse([Tag,  "tags" | Tail]),
            make_svn_url(Scheme, Host, Path1);
        RevPath ->
            Path1 = lists:reverse([Tag,  "tags" | RevPath]),
            make_svn_url(Scheme, Host, Path1)
    end.
        

compare_url(Dir, Url) ->
    CurrentUrl = trim(os:cmd("svn info \"" ++
                                 rebar_utils:escape_double_quotes(Dir) ++
                                 "\" | grep URL | cut -c6\\- ")),
    {_, Host1, Path1} = parse_svn_url(CurrentUrl),
    {_, HostU, PathU} = parse_svn_url(Url),
    %%?INFO("Comparing URLs ~ts (~tp) with ~ts (~tp)", [CurrentUrl,  {Host1, Path1}, Url, {HostU, PathU}]),
    {Host1, Path1} =:= {HostU, PathU}.


maybe_warn_local_url(Url) ->
    try
        _ = parse_svn_url(Url),
        ok
    catch
        _:_ ->
            ?WARN("URL format (~ts) unsupported.", [])
    end.

local_svn_tag(Dir) ->
    AbortMsg = io_lib:format("Get tag name of svn dependency failed in ~ts", [Dir]),
    Str = rebar_utils:sh("svn info \"" ++  rebar_utils:escape_double_quotes(Dir) ++
                             "\" | grep URL", [{use_stdout, false},
                                               {debug_abort_on_error, AbortMsg}]),
    {ok, "URL: " ++ URL} = Str,
    URL1 = trim(URL),
    {_Scheme, _Host, Path} = parse_svn_url(URL1),
    case lists:reverse(Path) of
        [Tag, "tags"|_] ->
            {ok, Tag};
        _ ->
            {ok, "not tagged"}
    end.

%% svn info -r HEAD gives version at the HEAD of the remote repo
remote_svn_revision(Url) ->
    svn_revision(Url, "HEAD").

%% svn info -r BASE gives version of the local working copy
local_svn_revision(Dir) ->
    svn_revision(Dir, "BASE").

%% Use the "Last Changed Rev: " field to avoid update
%% deps for commits in other parts of the svn repo
svn_revision(Path, Where) ->
    AbortMsg = io_lib:format("Get svn revision of svn dependency failed in ~ts", [Path]),
    Rev = rebar_utils:sh("svn info -r " ++ Where ++ " \"" ++
                             rebar_utils:escape_double_quotes(Path) ++
                             "\" | grep 'Last Changed Rev: '", [{use_stdout, false},
                                                                {debug_abort_on_error, AbortMsg}]),
    {ok, "Last Changed Rev: " ++ RevNum} = Rev,
    trim(RevNum).

    
parse_svn_url("svn://" ++ HostPath) ->
    [Host | Path] = rebar_string:lexemes(HostPath, "/"),
    {svn, Host, Path};
parse_svn_url("http://" ++ HostPath) ->
    [Host | Path] = rebar_string:lexemes(HostPath, "/"),
    {http, Host, Path};
parse_svn_url("https://" ++ HostPath) ->
    [Host | Path] = rebar_string:lexemes(HostPath, "/"),
    {https, Host, Path}.

make_svn_url(svn, Host, Path) ->
    "svn://" ++ rebar_string:join([Host |Path], "/");
make_svn_url(http, Host, Path) ->
    "http://" ++ rebar_string:join([Host |Path], "/");
make_svn_url(https, Host, Path) ->
    "https://" ++ rebar_string:join([Host |Path], "/").

trim(Str) ->    
    rebar_string:trim(rebar_string:trim(Str, both, "\n"), both, "\r").

check_type_support() ->
    case get({is_supported, ?MODULE}) of
        true ->
            ok;
        _ ->
            case rebar_utils:sh("svn --version", [{return_on_error, true},
                                                  {use_stdout, false}]) of
                {error, _} ->
                    ?ABORT("svn not installed", []);
                _ ->
                    put({is_supported, ?MODULE}, true),
                    ok
            end
    end.
