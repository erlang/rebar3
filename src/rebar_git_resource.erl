%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
-module(rebar_git_resource).

-behaviour(rebar_resource).

-export([lock/2
        ,download/2
        ,needs_update/2]).

-include("rebar.hrl").

lock(AppDir, {git, Url, _}) ->
    Ref = string:strip(
            os:cmd("git --git-dir='" ++ AppDir ++ "/.git' rev-parse --verify HEAD")
            ,both, $\n),
    {git, Url, {ref, Ref}}.

%% Return true if either the git url or tag/branch/ref is not the same as the currently
%% checked out git repo for the dep
needs_update(Dir, {git, Url, {tag, Tag}}) ->
    {ok, CurrentUrl} = rebar_utils:sh(?FMT("git config --get remote.origin.url", []),
                                      [{cd, Dir}]),
    {ok, Current} = rebar_utils:sh(?FMT("git describe --tags --exact-match", []),
                                   [{cd, Dir}]),
    Current1 = string:strip(string:strip(Current, both, $\n), both, $\r),
    CurrentUrl1 = string:strip(string:strip(CurrentUrl, both, $\n), both, $\r),
    ?DEBUG("Comparing git tag ~s with ~s and url ~s with ~s~n", [Tag, Current1, Url, CurrentUrl1]),
    not ((Current1 =:= Tag) andalso (CurrentUrl1 =:= Url));
needs_update(Dir, {git, Url, {branch, Branch}}) ->
    {ok, CurrentUrl} = rebar_utils:sh(?FMT("git config --get remote.origin.url", []),
                                      [{cd, Dir}]),
    {ok, Current} = rebar_utils:sh(?FMT("git symbolic-ref -q --short HEAD", []),
                                   [{cd, Dir}]),
    Current1 = string:strip(string:strip(Current, both, $\n), both, $\r),
    CurrentUrl1 = string:strip(string:strip(CurrentUrl, both, $\n), both, $\r),
    ?DEBUG("Comparing git branch ~s with ~s and url ~s with ~s~n", [Branch, Current1, Url, CurrentUrl1]),
    not ((Current1 =:= Branch) andalso (CurrentUrl1 =:= Url));
needs_update(Dir, {git, Url, Ref}) ->
    case Ref of
        {ref, Ref1} ->
            Ref1;
        Ref1 ->
            Ref1
    end,
    {ok, CurrentUrl} = rebar_utils:sh(?FMT("git config --get remote.origin.url", []),
                                      [{cd, Dir}]),
    {ok, Current} = rebar_utils:sh(?FMT("git rev-parse -q HEAD", []),
                                   [{cd, Dir}]),
    Current1 = string:strip(string:strip(Current, both, $\n), both, $\r),
    CurrentUrl1 = string:strip(string:strip(CurrentUrl, both, $\n), both, $\r),
    ?DEBUG("Comparing git ref ~s with ~s and url ~s with ~s~n", [Ref1, Current1, Url, CurrentUrl1]),
    not ((Current1 =:= Ref1) andalso (CurrentUrl1 =:= Url)).

download(Dir, {git, Url}) ->
    ?WARN("WARNING: It is recommended to use {branch, Name}, {tag, Tag} or {ref, Ref}, otherwise updating the dep may not work as expected.~n", []),
    download(Dir, {git, Url, {branch, "HEAD"}});
download(Dir, {git, Url, ""}) ->
    ?WARN("WARNING: It is recommended to use {branch, Name}, {tag, Tag} or {ref, Ref}, otherwise updating the dep may not work as expected.~n", []),
    download(Dir, {git, Url, {branch, "HEAD"}});
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
    ?WARN("WARNING: It is recommended to use {branch, Name}, {tag, Tag} or {ref, Ref}, otherwise updating the dep may not work as expected.~n", []),
    ok = filelib:ensure_dir(Dir),
    rebar_utils:sh(?FMT("git clone -n ~s ~s", [Url, filename:basename(Dir)]),
                   [{cd, filename:dirname(Dir)}]),
    rebar_utils:sh(?FMT("git checkout -q ~s", [Rev]), [{cd, Dir}]).
