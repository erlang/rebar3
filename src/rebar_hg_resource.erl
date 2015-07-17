%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
-module(rebar_hg_resource).

-behaviour(rebar_resource).

-export([lock/2
        ,download/3
        ,needs_update/2
        ,make_vsn/1]).

-include("rebar.hrl").

lock(AppDir, {hg, Url, _}) ->
    lock(AppDir, {hg, Url});
lock(AppDir, {hg, Url}) ->
    Ref = get_ref(AppDir),
    {hg, Url, {ref, Ref}}.

%% Return `true' if either the hg url or tag/branch/ref is not the same as
%% the currently checked out repo for the dep
needs_update(Dir, {hg, Url, {tag, Tag}}) ->
    Ref = get_ref(Dir),
    {ClosestTag, Distance} = get_tag_distance(Dir, Ref),
    ?DEBUG("Comparing hg tag ~s with ref ~s (closest tag is ~s at distance ~s)",
           [Tag, Ref, ClosestTag, Distance]),
    not ((Distance =:= "0") andalso (Tag =:= ClosestTag)
         andalso compare_url(Dir, Url));
needs_update(Dir, {hg, Url, {branch, Branch}}) ->
    Ref = get_ref(Dir),
    BRef = get_branch_ref(Dir, Branch),
    not ((Ref =:= BRef) andalso compare_url(Dir, Url));
needs_update(Dir, {hg, Url, "default"}) ->
    Ref = get_ref(Dir),
    BRef = get_branch_ref(Dir, "default"),
    not ((Ref =:= BRef) andalso compare_url(Dir, Url));
needs_update(Dir, {hg, Url, Ref}) ->
    LocalRef = get_ref(Dir),
    TargetRef = case Ref of
        {ref, Ref1} ->
            Length = length(LocalRef),
            if Length >= 7 -> lists:sublist(Ref1, Length);
               Length < 7 -> Ref1
            end;
        Ref1 ->
            Ref1
    end,
    ?DEBUG("Comparing hg ref ~s with ~s", [Ref1, LocalRef]),
    not ((LocalRef =:= TargetRef) andalso compare_url(Dir, Url)).

download(Dir, {hg, Url}, State) ->
    ?WARN("WARNING: It is recommended to use {branch, Name}, {tag, Tag} or {ref, Ref}, otherwise updating the dep may not work as expected.", []),
    download(Dir, {hg, Url, {branch, "default"}}, State);
download(Dir, {hg, Url, ""}, State) ->
    ?WARN("WARNING: It is recommended to use {branch, Name}, {tag, Tag} or {ref, Ref}, otherwise updating the dep may not work as expected.", []),
    download(Dir, {hg, Url, {branch, "default"}}, State);
download(Dir, {hg, Url, {branch, Branch}}, _State) ->
    ok = filelib:ensure_dir(Dir),
    rebar_utils:sh(?FMT("hg clone -q -b ~s ~s ~s",
                       [rebar_utils:escape_chars(Branch),
                        rebar_utils:escape_chars(Url),
                        rebar_utils:escape_chars(filename:basename(Dir))]),
                   [{cd, filename:dirname(Dir)}]);
download(Dir, {hg, Url, {tag, Tag}}, _State) ->
    ok = filelib:ensure_dir(Dir),
    rebar_utils:sh(?FMT("hg clone -q -u ~s ~s ~s",
                        [rebar_utils:escape_chars(Tag),
                         rebar_utils:escape_chars(Url),
                         rebar_utils:escape_chars(filename:basename(Dir))]),
                   [{cd, filename:dirname(Dir)}]);
download(Dir, {hg, Url, {ref, Ref}}, _State) ->
    ok = filelib:ensure_dir(Dir),
    rebar_utils:sh(?FMT("hg clone -q -r ~s ~s ~s",
                        [rebar_utils:escape_chars(Ref),
                         rebar_utils:escape_chars(Url),
                         rebar_utils:escape_chars(filename:basename(Dir))]),
                   [{cd, filename:dirname(Dir)}]);
download(Dir, {hg, Url, Rev}, _State) ->
    ok = filelib:ensure_dir(Dir),
    rebar_utils:sh(?FMT("hg clone -q -r ~s ~s ~s",
                        [rebar_utils:escape_chars(Rev),
                         rebar_utils:escape_chars(Url),
                         rebar_utils:escape_chars(filename:basename(Dir))]),
                   [{cd, filename:dirname(Dir)}]).

make_vsn(Dir) ->
    BaseHg = "hg -R \"" ++ rebar_utils:escape_double_quotes(Dir) ++ "\" ",
    Ref = get_ref(Dir),
    Cmd = BaseHg ++ "log --template \"{latesttag}+build.{latesttagdistance}.rev.{node|short}\""
          " --rev " ++ Ref,
    AbortMsg = io_lib:format("Version resolution of hg dependency failed in ~s", [Dir]),
    {ok, VsnString} =
        rebar_utils:sh(Cmd,
                       [{use_stdout, false}, {debug_abort_on_error, AbortMsg}]),
    RawVsn = string:strip(VsnString, both, $\n),

    Vsn = case RawVsn of
        "null+" ++ Rest -> "0.0.0+" ++ Rest;
        _ -> RawVsn
    end,
    {plain, Vsn}.

%%% Internal functions

compare_url(Dir, Url) ->
    CurrentUrl = string:strip(os:cmd("hg -R \"" ++ rebar_utils:escape_double_quotes(Dir) ++"\" paths default"), both, $\n),
    CurrentUrl1 = string:strip(CurrentUrl, both, $\r),
    parse_hg_url(CurrentUrl1) =:= parse_hg_url(Url).

get_ref(Dir) ->
    AbortMsg = io_lib:format("Get ref of hg dependency failed in ~s", [Dir]),
    {ok, RefString} =
        rebar_utils:sh("hg -R \"" ++ rebar_utils:escape_double_quotes(Dir) ++ "\" --debug id -i",
                       [{use_stdout, false}, {debug_abort_on_error, AbortMsg}]),
    string:strip(RefString, both, $\n).

get_tag_distance(Dir, Ref) ->
    AbortMsg = io_lib:format("Get tag distance of hg dependency failed in ~s", [Dir]),
    {ok, LogString} =
        rebar_utils:sh("hg -R \"" ++ rebar_utils:escape_double_quotes(Dir) ++ "\" "
                      "log --template \"{latesttag}-{latesttagdistance}\n\" "
                      "--rev " ++ rebar_utils:escape_chars(Ref),
                      [{use_stdout, false}, {debug_abort_on_error, AbortMsg}]),
    Log = string:strip(LogString,
                       both, $\n),
    [Tag, Distance] = re:split(Log, "-([0-9]+)$", [{parts,0}, {return, list}]),
    {Tag, Distance}.

get_branch_ref(Dir, Branch) ->
    AbortMsg = io_lib:format("Get branch ref of hg dependency failed in ~s", [Dir]),
    {ok, BranchRefString} =
        rebar_utils:sh("hg -R \"" ++ rebar_utils:escape_double_quotes(Dir) ++
                       "\" log --template \"{node}\n\" --rev " ++ rebar_utils:escape_chars(Branch),
                       [{use_stdout, false}, {debug_abort_on_error, AbortMsg}]),
    string:strip(BranchRefString, both, $\n).

parse_hg_url("ssh://" ++ HostPath) ->
    [Host | Path] = string:tokens(HostPath, "/"),
    {Host, filename:rootname(filename:join(Path), ".hg")};
parse_hg_url("http://" ++ HostPath) ->
    [Host | Path] = string:tokens(HostPath, "/"),
    {Host, filename:rootname(filename:join(Path), ".hg")};
parse_hg_url("https://" ++ HostPath) ->
    [Host | Path] = string:tokens(HostPath, "/"),
    {Host, filename:rootname(filename:join(Path), ".hg")}.
