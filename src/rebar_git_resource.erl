%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
-module(rebar_git_resource).

-behaviour(rebar_resource).

-export([lock/2
        ,download/3
        ,needs_update/2
        ,make_vsn/1]).

-include("rebar.hrl").

%% Regex used for parsing scp style remote url
-define(SCP_PATTERN, "\\A(?<username>[^@]+)@(?<host>[^:]+):(?<path>.+)\\z").

lock(AppDir, {git, Url, _}) ->
    lock(AppDir, {git, Url});
lock(AppDir, {git, Url}) ->
    AbortMsg = lists:flatten(io_lib:format("Locking of git dependency failed in ~ts", [AppDir])),
    Dir = rebar_utils:escape_double_quotes(AppDir),
    {ok, VsnString} =
        case os:type() of
            {win32, _} ->
                rebar_utils:sh("git --git-dir=\"" ++ Dir ++ "/.git\" --work-tree=\"" ++ Dir ++ "\" rev-parse --verify HEAD",
                    [{use_stdout, false}, {debug_abort_on_error, AbortMsg}]);
            _ ->
                rebar_utils:sh("git --git-dir=\"" ++ Dir ++ "/.git\" rev-parse --verify HEAD",
                    [{use_stdout, false}, {debug_abort_on_error, AbortMsg}])
        end,
    Ref = rebar_string:trim(VsnString, both, "\n"),
    {git, Url, {ref, Ref}}.

%% Return true if either the git url or tag/branch/ref is not the same as the currently
%% checked out git repo for the dep
needs_update(Dir, {git, Url, {tag, Tag}}) ->
    {ok, Current} = rebar_utils:sh(?FMT("git describe --tags --exact-match", []),
                                   [{cd, Dir}]),
    Current1 = rebar_string:trim(rebar_string:trim(Current, both, "\n"),
                                 both, "\r"),
    ?DEBUG("Comparing git tag ~ts with ~ts", [Tag, Current1]),
    not ((Current1 =:= Tag) andalso compare_url(Dir, Url));
needs_update(Dir, {git, Url, {branch, Branch}}) ->
    %% Fetch remote so we can check if the branch has changed
    SafeBranch = rebar_utils:escape_chars(Branch),
    {ok, _} = rebar_utils:sh(?FMT("git fetch origin ~ts", [SafeBranch]),
                             [{cd, Dir}]),
    %% Check for new commits to origin/Branch
    {ok, Current} = rebar_utils:sh(?FMT("git log HEAD..origin/~ts --oneline", [SafeBranch]),
                                   [{cd, Dir}]),
    ?DEBUG("Checking git branch ~ts for updates", [Branch]),
    not ((Current =:= []) andalso compare_url(Dir, Url));
needs_update(Dir, {git, Url, "master"}) ->
    needs_update(Dir, {git, Url, {branch, "master"}});
needs_update(Dir, {git, _, Ref}) ->
    {ok, Current} = rebar_utils:sh(?FMT("git rev-parse --short=7 -q HEAD", []),
                                   [{cd, Dir}]),
    Current1 = rebar_string:trim(rebar_string:trim(Current, both, "\n"),
                                 both, "\r"),
    Ref2 = case Ref of
               {ref, Ref1} ->
                   Length = length(Current1),
                   case Length >= 7 of
                       true -> lists:sublist(Ref1, Length);
                       false -> Ref1
                   end;
               _ ->
                   Ref
           end,

    ?DEBUG("Comparing git ref ~ts with ~ts", [Ref2, Current1]),
    (Current1 =/= Ref2).

compare_url(Dir, Url) ->
    {ok, CurrentUrl} = rebar_utils:sh(?FMT("git config --get remote.origin.url", []),
                                      [{cd, Dir}]),
    CurrentUrl1 = rebar_string:trim(rebar_string:trim(CurrentUrl, both, "\n"),
                                     both, "\r"),
    {ok, ParsedUrl} = parse_git_url(Url),
    {ok, ParsedCurrentUrl} = parse_git_url(CurrentUrl1),
    ?DEBUG("Comparing git url ~p with ~p", [ParsedUrl, ParsedCurrentUrl]),
    ParsedCurrentUrl =:= ParsedUrl.

parse_git_url(Url) ->
    %% Checks for standard scp style git remote
    case re:run(Url, ?SCP_PATTERN, [{capture, [host, path], list}, unicode]) of
        {match, [Host, Path]} ->
            {ok, {Host, filename:rootname(Path, ".git")}};
        nomatch ->
            parse_git_url(not_scp, Url)
    end.
parse_git_url(not_scp, Url) ->
    UriOpts = [{scheme_defaults, [{git, 9418} | http_uri:scheme_defaults()]}],
    case http_uri:parse(Url, UriOpts) of
        {ok, {_Scheme, _User, Host, _Port, Path, _Query}} ->
            {ok, {Host, filename:rootname(Path, ".git")}};
        {error, Reason} ->
            {error, Reason}
    end.

download(Dir, {git, Url}, State) ->
    ?WARN("WARNING: It is recommended to use {branch, Name}, {tag, Tag} or {ref, Ref}, otherwise updating the dep may not work as expected.", []),
    download(Dir, {git, Url, {branch, "master"}}, State);
download(Dir, {git, Url, ""}, State) ->
    ?WARN("WARNING: It is recommended to use {branch, Name}, {tag, Tag} or {ref, Ref}, otherwise updating the dep may not work as expected.", []),
    download(Dir, {git, Url, {branch, "master"}}, State);
download(Dir, {git, Url, {branch, Branch}}, _State) ->
    ok = filelib:ensure_dir(Dir),
    maybe_warn_local_url(Url),
    git_clone(branch, git_vsn(), Url, Dir, Branch);
download(Dir, {git, Url, {tag, Tag}}, _State) ->
    ok = filelib:ensure_dir(Dir),
    maybe_warn_local_url(Url),
    git_clone(tag, git_vsn(), Url, Dir, Tag);
download(Dir, {git, Url, {ref, Ref}}, _State) ->
    ok = filelib:ensure_dir(Dir),
    maybe_warn_local_url(Url),
    git_clone(ref, git_vsn(), Url, Dir, Ref);
download(Dir, {git, Url, Rev}, _State) ->
    ?WARN("WARNING: It is recommended to use {branch, Name}, {tag, Tag} or {ref, Ref}, otherwise updating the dep may not work as expected.", []),
    ok = filelib:ensure_dir(Dir),
    maybe_warn_local_url(Url),
    git_clone(rev, git_vsn(), Url, Dir, Rev).

maybe_warn_local_url(Url) ->
    WarnStr = "Local git resources (~ts) are unsupported and may have odd behaviour. "
              "Use remote git resources, or a plugin for local dependencies.",
    case parse_git_url(Url) of
        {error, no_scheme} -> ?WARN(WarnStr, [Url]);
        {error, {no_default_port, _, _}} -> ?WARN(WarnStr, [Url]);
        {error, {malformed_url, _, _}} -> ?WARN(WarnStr, [Url]);
        _ -> ok
    end.

%% Use different git clone commands depending on git --version
git_clone(branch,Vsn,Url,Dir,Branch) when Vsn >= {1,7,10}; Vsn =:= undefined ->
    rebar_utils:sh(?FMT("git clone ~ts ~ts -b ~ts --single-branch",
                        [rebar_utils:escape_chars(Url),
                         rebar_utils:escape_chars(filename:basename(Dir)),
                         rebar_utils:escape_chars(Branch)]),
                   [{cd, filename:dirname(Dir)}]);
git_clone(branch,_Vsn,Url,Dir,Branch) ->
    rebar_utils:sh(?FMT("git clone ~ts ~ts -b ~ts",
                        [rebar_utils:escape_chars(Url),
                         rebar_utils:escape_chars(filename:basename(Dir)),
                         rebar_utils:escape_chars(Branch)]),
                   [{cd, filename:dirname(Dir)}]);
git_clone(tag,Vsn,Url,Dir,Tag) when Vsn >= {1,7,10}; Vsn =:= undefined ->
    rebar_utils:sh(?FMT("git clone ~ts ~ts -b ~ts --single-branch",
                        [rebar_utils:escape_chars(Url),
                         rebar_utils:escape_chars(filename:basename(Dir)),
                         rebar_utils:escape_chars(Tag)]),
                   [{cd, filename:dirname(Dir)}]);
git_clone(tag,_Vsn,Url,Dir,Tag) ->
    rebar_utils:sh(?FMT("git clone ~ts ~ts -b ~ts",
                        [rebar_utils:escape_chars(Url),
                         rebar_utils:escape_chars(filename:basename(Dir)),
                         rebar_utils:escape_chars(Tag)]),
                   [{cd, filename:dirname(Dir)}]);
git_clone(ref,_Vsn,Url,Dir,Ref) ->
    rebar_utils:sh(?FMT("git clone -n ~ts ~ts",
                        [rebar_utils:escape_chars(Url),
                         rebar_utils:escape_chars(filename:basename(Dir))]),
                   [{cd, filename:dirname(Dir)}]),
    rebar_utils:sh(?FMT("git checkout -q ~ts", [Ref]), [{cd, Dir}]);
git_clone(rev,_Vsn,Url,Dir,Rev) ->
    rebar_utils:sh(?FMT("git clone -n ~ts ~ts",
                        [rebar_utils:escape_chars(Url),
                         rebar_utils:escape_chars(filename:basename(Dir))]),
                   [{cd, filename:dirname(Dir)}]),
    rebar_utils:sh(?FMT("git checkout -q ~ts", [rebar_utils:escape_chars(Rev)]),
                   [{cd, Dir}]).

git_vsn() ->
    case application:get_env(rebar, git_vsn) of
        {ok, Vsn} -> Vsn;
        undefined ->
            Vsn = git_vsn_fetch(),
            application:set_env(rebar, git_vsn, Vsn),
            Vsn
    end.

git_vsn_fetch() ->
    case rebar_utils:sh("git --version",[]) of
        {ok, VsnStr} ->
            case re:run(VsnStr, "git version\\h+(\\d)\\.(\\d)\\.(\\d).*", [{capture,[1,2,3],list}, unicode]) of
                {match,[Maj,Min,Patch]} ->
                    {list_to_integer(Maj),
                     list_to_integer(Min),
                     list_to_integer(Patch)};
                nomatch ->
                    undefined
            end;
        {error, _} ->
            undefined
    end.

make_vsn(Dir) ->
    case collect_default_refcount(Dir) of
        Vsn={plain, _} ->
            Vsn;
        {Vsn, RawRef, RawCount} ->
            {plain, build_vsn_string(Vsn, RawRef, RawCount)}
    end.

%% Internal functions

collect_default_refcount(Dir) ->
    %% Get the tag timestamp and minimal ref from the system. The
    %% timestamp is really important from an ordering perspective.
    case rebar_utils:sh("git log -n 1 --pretty=format:\"%h\n\" ",
                       [{use_stdout, false},
                        return_on_error,
                        {cd, Dir}]) of
        {error, _} ->
            ?WARN("Getting log of git dependency failed in ~ts. Falling back to version 0.0.0", [rebar_dir:get_cwd()]),
            {plain, "0.0.0"};
        {ok, String} ->
            RawRef = rebar_string:trim(String, both, "\n"),

            {Tag, TagVsn} = parse_tags(Dir),
            {ok, RawCount} =
                case Tag of
                    undefined ->
                        AbortMsg2 = "Getting rev-list of git depedency failed in " ++ Dir,
                        {ok, PatchLines} = rebar_utils:sh("git rev-list HEAD",
                                                          [{use_stdout, false},
                                                           {cd, Dir},
                                                           {debug_abort_on_error, AbortMsg2}]),
                        rebar_utils:line_count(PatchLines);
                    _ ->
                        get_patch_count(Dir, Tag)
                end,
            {TagVsn, RawRef, RawCount}
    end.

build_vsn_string(Vsn, RawRef, Count) ->
    %% Cleanup the tag and the Ref information. Basically leading 'v's and
    %% whitespace needs to go away.
    RefTag = [".ref", re:replace(RawRef, "\\s", "", [global, unicode])],

    %% Create the valid [semver](http://semver.org) version from the tag
    case Count of
        0 ->
            rebar_utils:to_list(Vsn);
        _ ->
            rebar_utils:to_list([Vsn, "+build.", integer_to_list(Count), RefTag])
    end.

get_patch_count(Dir, RawRef) ->
    AbortMsg = "Getting rev-list of git dep failed in " ++ Dir,
    Ref = re:replace(RawRef, "\\s", "", [global, unicode]),
    Cmd = io_lib:format("git rev-list ~ts..HEAD",
                        [rebar_utils:escape_chars(Ref)]),
    {ok, PatchLines} = rebar_utils:sh(Cmd,
                                        [{use_stdout, false},
                                         {cd, Dir},
                                         {debug_abort_on_error, AbortMsg}]),
    rebar_utils:line_count(PatchLines).


parse_tags(Dir) ->
    %% Don't abort on error, we want the bad return to be turned into 0.0.0
    case rebar_utils:sh("git -c color.ui=false log --oneline --no-walk --tags --decorate",
                        [{use_stdout, false}, return_on_error, {cd, Dir}]) of
        {error, _} ->
            {undefined, "0.0.0"};
        {ok, Line} ->
            case re:run(Line, "(\\(|\\s)(HEAD[^,]*,\\s)tag:\\s(v?([^,\\)]+))", [{capture, [3, 4], list}, unicode]) of
                {match,[Tag, Vsn]} ->
                    {Tag, Vsn};
                nomatch ->
                    case rebar_utils:sh("git describe --tags --abbrev=0",
                            [{use_stdout, false}, return_on_error, {cd, Dir}]) of
                        {error, _} ->
                            {undefined, "0.0.0"};
                        %% strip the v prefix if it exists like is done in the above match
                        {ok, [$v | LatestVsn]} ->
                            {undefined, rebar_string:trim(LatestVsn, both, "\n")};
                        {ok, LatestVsn} ->
                            {undefined, rebar_string:trim(LatestVsn,both, "\n")}
                    end
            end
    end.
