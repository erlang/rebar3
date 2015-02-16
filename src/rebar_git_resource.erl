%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
-module(rebar_git_resource).

-behaviour(rebar_resource).

-export([lock/2
        ,download/2
        ,needs_update/2
        ,make_vsn/1]).

-include("rebar.hrl").

lock(AppDir, {git, Url, _}) ->
    lock(AppDir, {git, Url});
lock(AppDir, {git, Url}) ->
    Ref = string:strip(
            os:cmd("git --git-dir='" ++ AppDir ++ "/.git' rev-parse --verify HEAD")
            ,both, $\n),
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
    {ok, Current} = rebar_utils:sh(?FMT("git symbolic-ref -q --short HEAD", []),
                                   [{cd, Dir}]),
    Current1 = string:strip(string:strip(Current, both, $\n), both, $\r),
    ?DEBUG("Comparing git branch ~s with ~s", [Branch, Current1]),
    not ((Current1 =:= Branch) andalso compare_url(Dir, Url));
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
    ParsedCurrentUrl =:= ParsedUrl.

parse_git_url("git@" ++ HostPath) ->
    [Host, Path] = string:tokens(HostPath, ":"),
    {Host, filename:rootname(Path, ".git")};
parse_git_url("git://" ++ HostPath) ->
    [Host | Path] = string:tokens(HostPath, "/"),
    {Host, filename:rootname(filename:join(Path), ".git")};
parse_git_url("https://" ++ HostPath) ->
    [Host | Path] = string:tokens(HostPath, "/"),
    {Host, filename:rootname(filename:join(Path), ".git")}.

download(Dir, {git, Url}) ->
    ?WARN("WARNING: It is recommended to use {branch, Name}, {tag, Tag} or {ref, Ref}, otherwise updating the dep may not work as expected.", []),
    download(Dir, {git, Url, {branch, "master"}});
download(Dir, {git, Url, ""}) ->
    ?WARN("WARNING: It is recommended to use {branch, Name}, {tag, Tag} or {ref, Ref}, otherwise updating the dep may not work as expected.", []),
    download(Dir, {git, Url, {branch, "master"}});
download(Dir, {git, Url, {branch, Branch}}) ->
    ok = filelib:ensure_dir(Dir),
    rebar_utils:sh(?FMT("git clone ~s ~s -b ~s --single-branch",
                       [Url, filename:basename(Dir), Branch]),
                   [{cd, filename:dirname(Dir)}]);
download(Dir, {git, Url, {tag, Tag}}) ->
    ok = filelib:ensure_dir(Dir),
    rebar_utils:sh(?FMT("git clone ~s ~s -b ~s --single-branch",
                        [Url, filename:basename(Dir), Tag]),
                   [{cd, filename:dirname(Dir)}]);
download(Dir, {git, Url, {ref, Ref}}) ->
    ok = filelib:ensure_dir(Dir),
    rebar_utils:sh(?FMT("git clone -n ~s ~s", [Url, filename:basename(Dir)]),
                   [{cd, filename:dirname(Dir)}]),
    rebar_utils:sh(?FMT("git checkout -q ~s", [Ref]), [{cd, Dir}]);
download(Dir, {git, Url, Rev}) ->
    ?WARN("WARNING: It is recommended to use {branch, Name}, {tag, Tag} or {ref, Ref}, otherwise updating the dep may not work as expected.", []),
    ok = filelib:ensure_dir(Dir),
    rebar_utils:sh(?FMT("git clone -n ~s ~s", [Url, filename:basename(Dir)]),
                   [{cd, filename:dirname(Dir)}]),
    rebar_utils:sh(?FMT("git checkout -q ~s", [Rev]), [{cd, Dir}]).

make_vsn(Dir) ->
    {ok, Cwd} = file:get_cwd(),
    try
        ok = file:set_cwd(Dir),
        {Vsn, RawRef, RawCount} = collect_default_refcount(),
        {plain, build_vsn_string(Vsn, RawRef, RawCount)}
    after
        file:set_cwd(Cwd)
    end.

%% Internal functions

collect_default_refcount() ->
    %% Get the tag timestamp and minimal ref from the system. The
    %% timestamp is really important from an ordering perspective.
    RawRef = os:cmd("git log -n 1 --pretty=format:'%h\n' "),

    {Tag, TagVsn} = parse_tags(),
    RawCount =
        case Tag of
            undefined ->
                os:cmd("git rev-list HEAD | wc -l");
            _ ->
                get_patch_count(Tag)
        end,
    {TagVsn, RawRef, RawCount}.

build_vsn_string(Vsn, RawRef, RawCount) ->
    %% Cleanup the tag and the Ref information. Basically leading 'v's and
    %% whitespace needs to go away.
    RefTag = [".ref", re:replace(RawRef, "\\s", "", [global])],
    Count = erlang:iolist_to_binary(re:replace(RawCount, "\\s", "", [global])),

    %% Create the valid [semver](http://semver.org) version from the tag
    case Count of
        <<"0">> ->
            erlang:binary_to_list(erlang:iolist_to_binary(Vsn));
        _ ->
            erlang:binary_to_list(erlang:iolist_to_binary([Vsn, "+build.",
                                                           Count, RefTag]))
    end.

get_patch_count(RawRef) ->
    Ref = re:replace(RawRef, "\\s", "", [global]),
    Cmd = io_lib:format("git rev-list ~s..HEAD | wc -l",
                         [Ref]),
    os:cmd(Cmd).

parse_tags() ->
    first_valid_tag(os:cmd("git log --oneline --decorate  | fgrep \"tag: \" -1000")).

first_valid_tag(Line) ->
    case re:run(Line, "(\\(|\\s)tag:\\s(v([^,\\)]+))", [{capture, [2, 3], list}]) of
        {match,[Tag, Vsn]} ->
            {Tag, Vsn};
        nomatch ->
            {undefined, "0.0.0"}
    end.
