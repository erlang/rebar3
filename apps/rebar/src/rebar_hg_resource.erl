%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
-module(rebar_hg_resource).

-behaviour(rebar_resource_v2).

-export([init/2,
         lock/2,
         download/4,
         needs_update/2,
         make_vsn/2]).


%% For backward compatibility
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

lock_(AppDir, {hg, Url, _}) ->
    lock_(AppDir, {hg, Url});
lock_(AppDir, {hg, Url}) ->
    Ref = get_ref(AppDir),
    {hg, Url, {ref, Ref}}.

%% Return `true' if either the hg url or tag/branch/ref is not the same as
%% the currently checked out repo for the dep
needs_update(AppInfo, _) ->
    needs_update_(rebar_app_info:dir(AppInfo), rebar_app_info:source(AppInfo)).

needs_update_(Dir, {hg, Url, {tag, Tag}}) ->
    Ref = get_ref(Dir),
    {ClosestTag, Distance} = get_tag_distance(Dir, Ref),
    ?DEBUG("Comparing hg tag ~ts with ref ~ts (closest tag is ~ts at distance ~ts)",
           [Tag, Ref, ClosestTag, Distance]),
    not ((Distance =:= "0") andalso (Tag =:= ClosestTag)
         andalso compare_url(Dir, Url));
needs_update_(Dir, {hg, Url, {branch, Branch}}) ->
    Ref = get_ref(Dir),
    BRef = get_branch_ref(Dir, Branch),
    not ((Ref =:= BRef) andalso compare_url(Dir, Url));
needs_update_(Dir, {hg, Url, "default"}) ->
    Ref = get_ref(Dir),
    BRef = get_branch_ref(Dir, "default"),
    not ((Ref =:= BRef) andalso compare_url(Dir, Url));
needs_update_(Dir, {hg, Url, Ref}) ->
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
    ?DEBUG("Comparing hg ref ~ts with ~ts", [Ref1, LocalRef]),
    not ((LocalRef =:= TargetRef) andalso compare_url(Dir, Url)).

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

%% For backward compatibility
download(Dir, AppInfo, State) ->
    download_(Dir, AppInfo, State).

download_(Dir, {hg, Url}, State) ->
    ?WARN("WARNING: It is recommended to use {branch, Name}, {tag, Tag} or {ref, Ref}, otherwise updating the dep may not work as expected.", []),
    download_(Dir, {hg, Url, {branch, "default"}}, State);
download_(Dir, {hg, Url, ""}, State) ->
    ?WARN("WARNING: It is recommended to use {branch, Name}, {tag, Tag} or {ref, Ref}, otherwise updating the dep may not work as expected.", []),
    download_(Dir, {hg, Url, {branch, "default"}}, State);
download_(Dir, {hg, Url, {branch, Branch}}, _State) ->
    ok = filelib:ensure_dir(Dir),
    maybe_warn_local_url(Url),
    rebar_utils:sh(?FMT("hg clone -q -b ~ts ~ts ~ts",
                       [rebar_utils:escape_chars(Branch),
                        rebar_utils:escape_chars(Url),
                        rebar_utils:escape_chars(filename:basename(Dir))]),
                   [{cd, filename:dirname(Dir)}]);
download_(Dir, {hg, Url, {tag, Tag}}, _State) ->
    ok = filelib:ensure_dir(Dir),
    maybe_warn_local_url(Url),
    rebar_utils:sh(?FMT("hg clone -q -u ~ts ~ts ~ts",
                        [rebar_utils:escape_chars(Tag),
                         rebar_utils:escape_chars(Url),
                         rebar_utils:escape_chars(filename:basename(Dir))]),
                   [{cd, filename:dirname(Dir)}]);
download_(Dir, {hg, Url, {ref, Ref}}, _State) ->
    ok = filelib:ensure_dir(Dir),
    maybe_warn_local_url(Url),
    rebar_utils:sh(?FMT("hg clone -q -r ~ts ~ts ~ts",
                        [rebar_utils:escape_chars(Ref),
                         rebar_utils:escape_chars(Url),
                         rebar_utils:escape_chars(filename:basename(Dir))]),
                   [{cd, filename:dirname(Dir)}]);
download_(Dir, {hg, Url, Rev}, _State) ->
    ok = filelib:ensure_dir(Dir),
    maybe_warn_local_url(Url),
    rebar_utils:sh(?FMT("hg clone -q -r ~ts ~ts ~ts",
                        [rebar_utils:escape_chars(Rev),
                         rebar_utils:escape_chars(Url),
                         rebar_utils:escape_chars(filename:basename(Dir))]),
                   [{cd, filename:dirname(Dir)}]).

make_vsn(AppInfo, _) ->
    check_type_support(),
    make_vsn_(rebar_app_info:dir(AppInfo)).

make_vsn_(Dir) ->
    BaseHg = "hg -R \"" ++ rebar_utils:escape_double_quotes(Dir) ++ "\" ",
    Ref = get_ref(Dir),
    Cmd = BaseHg ++ "log --template \"{latesttag}+build.{latesttagdistance}.rev.{node|short}\""
          " --rev " ++ Ref,
    AbortMsg = io_lib:format("Version resolution of hg dependency failed in ~ts", [Dir]),
    {ok, VsnString} =
        rebar_utils:sh(Cmd,
                       [{use_stdout, false}, {debug_abort_on_error, AbortMsg}]),
    RawVsn = rebar_string:trim(VsnString, both, "\n"),

    Vsn = case RawVsn of
        "null+" ++ Rest -> "0.0.0+" ++ Rest;
        _ -> RawVsn
    end,
    {plain, Vsn}.

%%% Internal functions

compare_url(Dir, Url) ->
    CurrentUrl = rebar_string:trim(os:cmd("hg -R \"" ++ rebar_utils:escape_double_quotes(Dir) ++"\" paths default"), both, "\n"),
    CurrentUrl1 = rebar_string:trim(CurrentUrl, both, "\r"),
    parse_hg_url(CurrentUrl1) =:= parse_hg_url(Url).

get_ref(Dir) ->
    AbortMsg = io_lib:format("Get ref of hg dependency failed in ~ts", [Dir]),
    {ok, RefString} =
        rebar_utils:sh("hg -R \"" ++ rebar_utils:escape_double_quotes(Dir) ++ "\" --debug id -i",
                       [{use_stdout, false}, {debug_abort_on_error, AbortMsg}]),
    rebar_string:trim(RefString, both, "\n").

get_tag_distance(Dir, Ref) ->
    AbortMsg = io_lib:format("Get tag distance of hg dependency failed in ~ts", [Dir]),
    {ok, LogString} =
        rebar_utils:sh("hg -R \"" ++ rebar_utils:escape_double_quotes(Dir) ++ "\" "
                      "log --template \"{latesttag}-{latesttagdistance}\n\" "
                      "--rev " ++ rebar_utils:escape_chars(Ref),
                      [{use_stdout, false}, {debug_abort_on_error, AbortMsg}]),
    Log = rebar_string:trim(LogString,
                            both, "\n"),
    [Tag, Distance] = re:split(Log, "-([0-9]+)$",
                               [{parts,0}, {return,list}, unicode]),
    {Tag, Distance}.

get_branch_ref(Dir, Branch) ->
    AbortMsg = io_lib:format("Get branch ref of hg dependency failed in ~ts", [Dir]),
    {ok, BranchRefString} =
        rebar_utils:sh("hg -R \"" ++ rebar_utils:escape_double_quotes(Dir) ++
                       "\" log --template \"{node}\n\" --rev " ++ rebar_utils:escape_chars(Branch),
                       [{use_stdout, false}, {debug_abort_on_error, AbortMsg}]),
    rebar_string:trim(BranchRefString, both, "\n").


maybe_warn_local_url(Url) ->
    try
        _ = parse_hg_url(Url),
        ok
    catch
        _:_ ->
            ?WARN("URL format (~ts) unsupported.", [])
    end.

parse_hg_url("ssh://" ++ HostPath) ->
    [Host | Path] = rebar_string:lexemes(HostPath, "/"),
    {Host, filename:rootname(filename:join(Path), ".hg")};
parse_hg_url("http://" ++ HostPath) ->
    [Host | Path] = rebar_string:lexemes(HostPath, "/"),
    {Host, filename:rootname(filename:join(Path), ".hg")};
parse_hg_url("https://" ++ HostPath) ->
    [Host | Path] = rebar_string:lexemes(HostPath, "/"),
    {Host, filename:rootname(filename:join(Path), ".hg")}.

check_type_support() ->
    case get({is_supported, ?MODULE}) of
        true ->
            ok;
        _ ->
            case rebar_utils:sh("hg --version", [{return_on_error, true},
                                                 {use_stdout, false}]) of
                {error, _} ->
                    ?ABORT("hg not installed", []);
                _ ->
                    put({is_supported, ?MODULE}, true),
                    ok
            end
    end.
