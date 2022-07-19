%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
-module(rebar_git_subdir_resource).

-behaviour(rebar_resource_v2).

-export([init/2,
         lock/2,
         download/4,
         needs_update/2,
         make_vsn/2,
         format_error/1]).

-include("rebar.hrl").
-include_lib("providers/include/providers.hrl").

%% Regex used for parsing scp style remote url
-define(SCP_PATTERN, "\\A(?<username>[^@]+)@(?<host>[^:]+):(?<path>.+)\\z").

-spec init(atom(), rebar_state:t()) -> {ok, rebar_resource_v2:resource()}.
init(Type, _State) ->
    Resource = rebar_resource_v2:new(Type, ?MODULE, #{}),
    {ok, Resource}.

lock(AppInfo, _) ->
    {git_subdir, Url, Checkout, Dir} = rebar_app_info:source(AppInfo),
    {git, Url1, {ref, Ref}} =
        rebar_git_resource:lock_(rebar_app_info:dir(AppInfo), {git, Url, Checkout}),
    {git_subdir, Url1, {ref, Ref}, Dir}.

download(TmpDir, AppInfo, State, _) ->
    Name = rebar_app_info:name(AppInfo),
    {git_subdir, Url, Checkout, SparseDir} = rebar_app_info:source(AppInfo),
    case rebar_git_resource:download_(TmpDir, {git, Url, Checkout}, State) of
        ok ->
            sparse_checkout(Name, rebar_git_resource:git_vsn(), TmpDir, to_ref(Checkout), SparseDir);
        {ok, _} ->
            sparse_checkout(Name, rebar_git_resource:git_vsn(), TmpDir, to_ref(Checkout), SparseDir);
        {error, Reason} ->
            {error, Reason};
        Error ->
            {error, Error}
    end.

%% Return true if either the git url or tag/branch/ref is not the same as the currently
%% checked out git repo for the dep
needs_update(AppInfo, _) ->
    {git_subdir, Url, Ref, _Dir} = rebar_app_info:source(AppInfo),
    rebar_git_resource:needs_update_(rebar_app_info:dir(AppInfo), {git, Url, Ref}).

make_vsn(AppInfo, _) ->
    Dir = rebar_app_info:dir(AppInfo),
    rebar_git_resource:make_vsn_(Dir).

%%

to_ref({branch, Branch}) ->
    Branch;
to_ref({tag, Tag}) ->
    Tag;
to_ref({ref, Ref}) ->
    Ref;
to_ref(Rev) ->
    Rev.

sparse_checkout(Name, GitVsn, Dir, Ref, SparseDir) when GitVsn >= {1,7,4};
                                                        GitVsn =:= undefined  ->
    ?DEBUG("doing sparse checkout in ~s of dir ~s", [Dir, SparseDir]),
    check_directory(Name, Dir, SparseDir),
    rebar_utils:sh(?FMT("git --git-dir=.git config core.sparsecheckout true", []),
                   [{cd, Dir}]),
    filelib:ensure_dir(filename:join(Dir, ".git/info/sparse-checkout")),
    file:write_file(filename:join(Dir, ".git/info/sparse-checkout"), SparseDir),
    rebar_utils:sh(?FMT("git checkout -q ~ts", [rebar_utils:escape_chars(Ref)]), [{cd, Dir}]),
    ok;
sparse_checkout(Name, _, Dir, _, SparseDir) ->
    %% sparse checkout not supported but we can still use the subdirectory
    %% so no need to fail, just don't do the sparse checkout
    ?DEBUG("too old a git version to do a sparse checkout for a subdir dep", []),
    check_directory(Name, Dir, SparseDir),
    ok.

%% verify that subdirectory exists
check_directory(Name, Dir, SparseDir) ->
    case filelib:is_dir(filename:join(Dir, SparseDir)) of
        true ->
            ok;
        false ->
            erlang:error(?PRV_ERROR({bad_subdir, Name, SparseDir}))
    end.

format_error({bad_subdir, Name, SubDir}) ->
    io_lib:format("Failed to fetch git_subdir dependency ~ts: directory ~ts does not exist.", [Name, SubDir]).
