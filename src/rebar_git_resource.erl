%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
-module(rebar_git_resource).

-behaviour(rebar_resource).

-export([lock/2
        ,download/2]).

-include("rebar.hrl").

lock(AppDir, {git, Url, _}) ->
    Ref = string:strip(
            os:cmd("git --git-dir='" ++ AppDir ++ "/.git' rev-parse --verify HEAD")
            ,both, $\n),
    {git, Url, {ref, Ref}}.

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
