%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
-module(rebar_git_resource).

-behaviour(rebar_resource).

-export([lock/2
        ,download/3
        ,needs_update/2
        ,make_vsn/1]).

-include("rebar.hrl").

lock(AppDir, {git, Url, _}) ->
    lock(AppDir, {git, Url});
lock(AppDir, {git, Url}) ->
    AbortMsg = io_lib:format("Locking of git dependency failed in ~s", [AppDir]),
    {ok, VsnString} =
        rebar_utils:sh("git --git-dir=\"" ++ rebar_utils:escape_double_quotes(AppDir) ++ "/.git\" rev-parse --verify HEAD",
                       [{use_stdout, false}, {debug_abort_on_error, AbortMsg}]),
    Ref = string:strip(VsnString, both, $\n),
    {git, Url, {ref, Ref}}.

%% Return true if either the git url or tag/branch/ref is not the same as the currently
%% checked out git repo for the dep
needs_update(Dir, {git, Url, {tag, Tag}}) ->
    {ok, Current} = rebar_utils:sh(?FMT("git describe --tags --exact-match", []),
                                   [{cd, Dir}]),
    Current1 = string:strip(string:strip(Current, both, $\n), both, $\r),

    ?DEBUG("Comparing git tag ~s with ~s", [Tag, Current1]),
    not ((Current1 =:= Tag) andalso compare_url(Dir, Url));
needs_update(Dir, {git, Url, {branch, Branch}}) ->
    %% Fetch remote so we can check if the branch has changed
    SafeBranch = rebar_utils:escape_chars(Branch),
    {ok, _} = rebar_utils:sh(?FMT("git fetch origin ~s", [SafeBranch]),
                             [{cd, Dir}]),
    %% Check for new commits to origin/Branch
    {ok, Current} = rebar_utils:sh(?FMT("git log HEAD..origin/~s --oneline", [SafeBranch]),
                                   [{cd, Dir}]),
    ?DEBUG("Checking git branch ~s for updates", [Branch]),
    not ((Current =:= []) andalso compare_url(Dir, Url));
needs_update(Dir, {git, Url, "master"}) ->
    needs_update(Dir, {git, Url, {branch, "master"}});
needs_update(Dir, {git, Url, Ref}) ->
    {ok, Current} = rebar_utils:sh(?FMT("git rev-parse -q HEAD", []),
                                   [{cd, Dir}]),
    Current1 = string:strip(string:strip(Current, both, $\n), both, $\r),

    Ref2 = case Ref of
               {ref, Ref1} ->
                   Length = length(Current1),
                   if
                       Length >= 7 ->
                           lists:sublist(Ref1, Length);
                       true ->
                           Ref1
                   end;
               Ref1 ->
                   Ref1
           end,

    ?DEBUG("Comparing git ref ~s with ~s", [Ref1, Current1]),
    not ((Current1 =:= Ref2) andalso compare_url(Dir, Url)).

compare_url(Dir, Url) ->
    {ok, CurrentUrl} = rebar_utils:sh(?FMT("git config --get remote.origin.url", []),
                                      [{cd, Dir}]),
    CurrentUrl1 = string:strip(string:strip(CurrentUrl, both, $\n), both, $\r),
    ParsedUrl = parse_git_url(Url),
    ParsedCurrentUrl = parse_git_url(CurrentUrl1),
    ?DEBUG("Comparing git url ~p with ~p", [ParsedUrl, ParsedCurrentUrl]),
    ParsedCurrentUrl =:= ParsedUrl.

parse_git_url("git@" ++ HostPath) ->
    [Host, Path] = string:tokens(HostPath, ":"),
    {Host, filename:rootname(Path, ".git")};
parse_git_url("git://" ++ HostPath) ->
    [Host | Path] = string:tokens(HostPath, "/"),
    {Host, filename:rootname(filename:join(Path), ".git")};
parse_git_url("http://" ++ HostPath) ->
    [Host | Path] = string:tokens(HostPath, "/"),
    {Host, filename:rootname(filename:join(Path), ".git")};
parse_git_url("https://" ++ HostPath) ->
    [Host | Path] = string:tokens(HostPath, "/"),
    {Host, filename:rootname(filename:join(Path), ".git")}.

download(Dir, {git, Url}, State) ->
    ?WARN("WARNING: It is recommended to use {branch, Name}, {tag, Tag} or {ref, Ref}, otherwise updating the dep may not work as expected.", []),
    download(Dir, {git, Url, {branch, "master"}}, State);
download(Dir, {git, Url, ""}, State) ->
    ?WARN("WARNING: It is recommended to use {branch, Name}, {tag, Tag} or {ref, Ref}, otherwise updating the dep may not work as expected.", []),
    download(Dir, {git, Url, {branch, "master"}}, State);
download(Dir, {git, Url, {branch, Branch}}, _State) ->
    ok = filelib:ensure_dir(Dir),
    rebar_utils:sh(?FMT("git clone ~s ~s -b ~s --single-branch",
                       [rebar_utils:escape_chars(Url),
                        rebar_utils:escape_chars(filename:basename(Dir)),
                        rebar_utils:escape_chars(Branch)]),
                   [{cd, filename:dirname(Dir)}]);
download(Dir, {git, Url, {tag, Tag}}, _State) ->
    ok = filelib:ensure_dir(Dir),
    rebar_utils:sh(?FMT("git clone ~s ~s -b ~s --single-branch",
                       [rebar_utils:escape_chars(Url),
                        rebar_utils:escape_chars(filename:basename(Dir)),
                        rebar_utils:escape_chars(Tag)]),
                   [{cd, filename:dirname(Dir)}]);
download(Dir, {git, Url, {ref, Ref}}, _State) ->
    ok = filelib:ensure_dir(Dir),
    rebar_utils:sh(?FMT("git clone -n ~s ~s",
                        [rebar_utils:escape_chars(Url),
                         rebar_utils:escape_chars(filename:basename(Dir))]),
                   [{cd, filename:dirname(Dir)}]),
    rebar_utils:sh(?FMT("git checkout -q ~s", [Ref]), [{cd, Dir}]);
download(Dir, {git, Url, Rev}, _State) ->
    ?WARN("WARNING: It is recommended to use {branch, Name}, {tag, Tag} or {ref, Ref}, otherwise updating the dep may not work as expected.", []),
    ok = filelib:ensure_dir(Dir),
    rebar_utils:sh(?FMT("git clone -n ~s ~s",
                        [rebar_utils:escape_chars(Url),
                         rebar_utils:escape_chars(filename:basename(Dir))]),
                   [{cd, filename:dirname(Dir)}]),
    rebar_utils:sh(?FMT("git checkout -q ~s", [rebar_utils:escape_chars(Rev)]),
                   [{cd, Dir}]).

make_vsn(Dir) ->
    {Vsn, RawRef, RawCount} = collect_default_refcount(Dir),
    {plain, build_vsn_string(Vsn, RawRef, RawCount)}.

%% Internal functions

collect_default_refcount(Dir) ->
    %% Get the tag timestamp and minimal ref from the system. The
    %% timestamp is really important from an ordering perspective.
    AbortMsg1 = "Getting log of git dependency failed in " ++ rebar_dir:get_cwd(),
    {ok, String} =
        rebar_utils:sh("git log -n 1 --pretty=format:\"%h\n\" ",
                       [{use_stdout, false},
                        {cd, Dir},
                        {debug_abort_on_error, AbortMsg1}]),
    RawRef = string:strip(String, both, $\n),

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
    {TagVsn, RawRef, RawCount}.

build_vsn_string(Vsn, RawRef, Count) ->
    %% Cleanup the tag and the Ref information. Basically leading 'v's and
    %% whitespace needs to go away.
    RefTag = [".ref", re:replace(RawRef, "\\s", "", [global])],

    %% Create the valid [semver](http://semver.org) version from the tag
    case Count of
        0 ->
            erlang:binary_to_list(erlang:iolist_to_binary(Vsn));
        _ ->
            erlang:binary_to_list(erlang:iolist_to_binary([Vsn, "+build.",
                                                           integer_to_list(Count), RefTag]))
    end.

get_patch_count(Dir, RawRef) ->
    AbortMsg = "Getting rev-list of git dep failed in " ++ Dir,
    Ref = re:replace(RawRef, "\\s", "", [global]),
    Cmd = io_lib:format("git rev-list ~s..HEAD",
                        [rebar_utils:escape_chars(Ref)]),
    {ok, PatchLines} = rebar_utils:sh(Cmd,
                                        [{use_stdout, false},
                                         {cd, Dir},
                                         {debug_abort_on_error, AbortMsg}]),
    rebar_utils:line_count(PatchLines).


parse_tags(Dir) ->
    %% Don't abort on error, we want the bad return to be turned into 0.0.0
    case rebar_utils:sh("git log --oneline --no-walk --tags --decorate",
                        [{use_stdout, false}, return_on_error, {cd, Dir}]) of
        {error, _} ->
            {undefined, "0.0.0"};
        {ok, Line} ->
            case re:run(Line, "(\\(|\\s)tag:\\s(v([^,\\)]+))", [{capture, [2, 3], list}]) of
                {match,[Tag, Vsn]} ->
                    {Tag, Vsn};
                nomatch ->
                    {undefined, "0.0.0"}
            end
    end.
