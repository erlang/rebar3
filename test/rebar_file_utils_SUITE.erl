-module(rebar_file_utils_SUITE).

-export([all/0,
         groups/0,
         init_per_group/2,
         end_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2,
         raw_tmpdir/1,
         empty_tmpdir/1,
         simple_tmpdir/1,
         multi_tmpdir/1,
         reset_nonexistent_dir/1,
         reset_empty_dir/1,
         reset_dir/1,
         path_from_ancestor/1,
         canonical_path/1,
         resolve_link/1,
         split_dirname/1,
         mv_warning_is_ignored/1,
         mv_dir/1,
         mv_file_same/1,
         mv_file_diff/1,
         mv_file_dir_same/1,
         mv_file_dir_diff/1,
         mv_no_clobber/1,
         symlink_existing_dir/1,
         preserve_existing_symlink/1,
         create_missing_dir/1,
         preserve_existing_dir/1,
         delete_symlink_and_create_dir/1,
         delete_dir_and_create_symlink/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").


all() ->
    [{group, tmpdir},
     {group, reset_dir},
     {group, mv},
     path_from_ancestor,
     canonical_path,
     resolve_link,
     split_dirname,
     mv_warning_is_ignored,
     symlink_existing_dir,
     preserve_existing_symlink,
     create_missing_dir,
     preserve_existing_dir,
     delete_symlink_and_create_dir,
     delete_dir_and_create_symlink].

groups() ->
    [{tmpdir, [], [raw_tmpdir, empty_tmpdir, simple_tmpdir, multi_tmpdir]},
     {reset_dir, [], [reset_nonexistent_dir, reset_empty_dir, reset_dir]},
     {mv, [], [mv_dir, mv_file_same, mv_file_diff,
               mv_file_dir_same, mv_file_dir_diff, mv_no_clobber]}].

init_per_group(reset_dir, Config) ->
    TmpDir = rebar_file_utils:system_tmpdir(["rebar_file_utils_SUITE", "resetable"]),
    [{tmpdir, TmpDir}|Config];
init_per_group(_, Config) -> Config.
end_per_group(_, Config) -> Config.

init_per_testcase(Test, Config) ->
    case os:type() of
        {win32, _} ->
            case lists:member(Test, [resolve_link, mv_warning_is_ignored]) of
                true -> {skip, "broken in windows"};
                false -> Config
            end;
        _ ->
            Config
    end.

end_per_testcase(_Test, Config) ->
    Config.

raw_tmpdir(_Config) ->
    case rebar_file_utils:system_tmpdir() of
        "/tmp"  -> ok;
        "./tmp" -> ok
    end.

empty_tmpdir(_Config) ->
    case rebar_file_utils:system_tmpdir([]) of
        "/tmp"  -> ok;
        "./tmp" -> ok
    end.

simple_tmpdir(_Config) ->
    case rebar_file_utils:system_tmpdir(["test"]) of
        "/tmp/test"  -> ok;
        "./tmp/test" -> ok
    end.

multi_tmpdir(_Config) ->
    case rebar_file_utils:system_tmpdir(["a", "b", "c"]) of
        "/tmp/a/b/c"  -> ok;
        "./tmp/a/b/c" -> ok
    end.

reset_nonexistent_dir(Config) ->
    TmpDir = ?config(tmpdir, Config),
    _ = ec_file:remove(TmpDir, [recursive]),
    ?assertNot(filelib:is_dir(TmpDir)),
    ok = rebar_file_utils:reset_dir(TmpDir),
    ?assert(filelib:is_dir(TmpDir)),
    {ok, []} = rebar_utils:list_dir(TmpDir).

reset_empty_dir(Config) ->
    TmpDir = ?config(tmpdir, Config),
    _ = ec_file:remove(TmpDir, [recursive]),
    _ = filelib:ensure_dir(filename:join([TmpDir, "dummy.beam"])),
    ?assert(filelib:is_dir(TmpDir)),
    ok = rebar_file_utils:reset_dir(TmpDir),
    ?assert(filelib:is_dir(TmpDir)),
    {ok, []} = rebar_utils:list_dir(TmpDir).

reset_dir(Config) ->
    TmpDir = ?config(tmpdir, Config),
    _ = ec_file:remove(TmpDir, [recursive]),
    _ = filelib:ensure_dir(filename:join([TmpDir, "dummy.beam"])),
    ?assert(filelib:is_dir(TmpDir)),
    lists:foreach(fun(Name) -> file:write_file(filename:join([TmpDir, Name]), <<>>) end,
                  ["a", "b", "c"]),
    lists:foreach(fun(File) -> ?assert(filelib:is_file(filename:join([TmpDir, File]))) end,
                  ["a", "b", "c"]),
    ok = rebar_file_utils:reset_dir(TmpDir),
    ?assert(filelib:is_dir(TmpDir)),
    {ok, []} = rebar_utils:list_dir(TmpDir).

path_from_ancestor(_Config) ->
    ?assertEqual({ok, "foo/bar/baz"}, rebar_file_utils:path_from_ancestor("/foo/bar/baz", "/")),
    ?assertEqual({ok, "bar/baz"}, rebar_file_utils:path_from_ancestor("/foo/bar/baz", "/foo")),
    ?assertEqual({ok, "bar"}, rebar_file_utils:path_from_ancestor("foo/bar", "foo")),
    ?assertEqual({ok, "bar"}, rebar_file_utils:path_from_ancestor("foo/bar/", "foo/")),
    ?assertEqual({error, badparent}, rebar_file_utils:path_from_ancestor("/foo/bar/baz", "/qux")),
    ?assertEqual({error, badparent}, rebar_file_utils:path_from_ancestor("/foo/bar/baz", "/foo/bar/baz/qux")).

canonical_path(_Config) ->
    %% We find the root so that the name works both on unix-likes and
    %% with Windows.
    Root = case os:type() of
               {win32, _} -> filename:nativename(filename:absname("/")); % C:\, with proper drive
               _ -> "/"
           end,
    ?assertEqual(filename:nativename(Root), rebar_file_utils:canonical_path("/")),
    ?assertEqual(filename:nativename(Root), rebar_file_utils:canonical_path("/../../..")),
    ?assertEqual(Root ++ "foo", rebar_file_utils:canonical_path("/foo/bar/..")),
    ?assertEqual(Root ++ "foo", rebar_file_utils:canonical_path("/foo/../foo")),
    ?assertEqual(Root ++ "foo", rebar_file_utils:canonical_path("/foo/.")),
    ?assertEqual(Root ++ "foo", rebar_file_utils:canonical_path("/foo/./.")),
    ?assertEqual(filename:nativename(Root ++ "foo/bar"),
                 rebar_file_utils:canonical_path("/foo/./bar")).

resolve_link(_Config) ->
    TmpDir = rebar_file_utils:system_tmpdir(
            ["rebar_file_utils_SUITE", "resolve_link"]),
    Link = filename:join(TmpDir, "link"),
    Target = filename:join(TmpDir, "link-target"),
    ec_file:remove(TmpDir, [recursive]),
    ok = filelib:ensure_dir(Target),
    ok = file:write_file(Target, <<>>),
    ok = file:make_symlink(Target, Link),
    ?assertEqual(Target, rebar_file_utils:resolve_link(Link)).

split_dirname(_Config) ->
    ?assertEqual({".", ""}, rebar_file_utils:split_dirname("")),
    ?assertEqual({"/", ""}, rebar_file_utils:split_dirname("/")),
    ?assertEqual({"/", "foo"}, rebar_file_utils:split_dirname("/foo")),
    ?assertEqual({".", "foo"}, rebar_file_utils:split_dirname("foo")),
    ?assertEqual({"/foo", "bar"}, rebar_file_utils:split_dirname("/foo/bar")),
    ?assertEqual({"foo", "bar"}, rebar_file_utils:split_dirname("foo/bar")).

mv_warning_is_ignored(_Config) ->
    meck:new(rebar_utils, [passthrough]),
    meck:expect(rebar_utils, sh, fun("mv ding dong", _) -> {ok, "Warning"}  end),
    ok = rebar_file_utils:mv("ding", "dong"),
    meck:unload(rebar_utils).

%%% Ensure Windows & Unix operations to move files

mv_dir(Config) ->
    %% Move a directory to another one location
    PrivDir = ?config(priv_dir, Config),
    BaseDir = mk_base_dir(PrivDir, mv_dir),
    SrcDir = filename:join(BaseDir, "src/"),
    ec_file:mkdir_p(SrcDir),
    ?assert(filelib:is_dir(SrcDir)),
    %% empty dir movement
    DstDir1 = filename:join(BaseDir, "dst1/"),
    ?assertNot(filelib:is_dir(DstDir1)),
    ?assertEqual(ok, rebar_file_utils:mv(SrcDir, DstDir1)),
    ?assert(filelib:is_dir(DstDir1)),

    %% move files from dir to empty dir
    F1 = filename:join(SrcDir, "file1"),
    F2 = filename:join(SrcDir, "subdir/file2"),
    filelib:ensure_dir(F2),
    file:write_file(F1, "hello"),
    file:write_file(F2, "world"),
    DstDir2 = filename:join(BaseDir, "dst2/"),
    D2F1 = filename:join(DstDir2, "file1"),
    D2F2 = filename:join(DstDir2, "subdir/file2"),
    ?assertNot(filelib:is_dir(DstDir2)),
    ?assertEqual(ok, rebar_file_utils:mv(SrcDir, DstDir2)),
    ?assert(filelib:is_file(D2F1)),
    ?assert(filelib:is_file(D2F2)),

    %% move files from dir to existing dir moves it to
    %% a subdir
    filelib:ensure_dir(F2),
    file:write_file(F1, "hello"),
    file:write_file(F2, "world"),
    DstDir3 = filename:join(BaseDir, "dst3/"),
    D3F1 = filename:join(DstDir3, "src/file1"),
    D3F2 = filename:join(DstDir3, "src/subdir/file2"),
    ec_file:mkdir_p(DstDir3),
    ?assert(filelib:is_dir(DstDir3)),
    ?assertEqual(ok, rebar_file_utils:mv(SrcDir, DstDir3)),
    ?assertNot(filelib:is_file(F1)),
    ?assertNot(filelib:is_file(F2)),
    ?assert(filelib:is_file(D3F1)),
    ?assert(filelib:is_file(D3F2)),
    ?assertNot(filelib:is_dir(SrcDir)),
    ok.

mv_file_same(Config) ->
    %% Move a file from a directory to the other without renaming
    PrivDir = ?config(priv_dir, Config),
    BaseDir = mk_base_dir(PrivDir, mv_file_same),
    SrcDir = filename:join(BaseDir, "src/"),
    ec_file:mkdir_p(SrcDir),
    ?assert(filelib:is_dir(SrcDir)),
    F = filename:join(SrcDir, "file"),
    file:write_file(F, "hello"),
    DstDir = filename:join(BaseDir, "dst/"),
    ec_file:mkdir_p(DstDir),
    Dst = filename:join(DstDir, "file"),
    ?assertEqual(ok, rebar_file_utils:mv(F, Dst)),
    ?assert(filelib:is_file(Dst)),
    ?assertNot(filelib:is_file(F)),
    ok.

mv_file_diff(Config) ->
    %% Move a file from a directory to another one while renaming
    %% into a pre-existing file
    PrivDir = ?config(priv_dir, Config),
    BaseDir = mk_base_dir(PrivDir, mv_file_diff),
    SrcDir = filename:join(BaseDir, "src/"),
    ec_file:mkdir_p(SrcDir),
    ?assert(filelib:is_dir(SrcDir)),
    F = filename:join(SrcDir, "file"),
    file:write_file(F, "hello"),
    DstDir = filename:join(BaseDir, "dst/"),
    ec_file:mkdir_p(DstDir),
    Dst = filename:join(DstDir, "file-rename"),
    file:write_file(Dst, "not-the-right-content"),
    ?assert(filelib:is_file(Dst)),
    ?assertEqual(ok, rebar_file_utils:mv(F, Dst)),
    ?assert(filelib:is_file(Dst)),
    ?assertEqual({ok, <<"hello">>}, file:read_file(Dst)),
    ?assertNot(filelib:is_file(F)),
    ok.

mv_file_dir_same(Config) ->
    %% Move a file to a directory without renaming
    PrivDir = ?config(priv_dir, Config),
    BaseDir = mk_base_dir(PrivDir, mv_file_dir_same),
    SrcDir = filename:join(BaseDir, "src/"),
    ec_file:mkdir_p(SrcDir),
    ?assert(filelib:is_dir(SrcDir)),
    F = filename:join(SrcDir, "file"),
    file:write_file(F, "hello"),
    DstDir = filename:join(BaseDir, "dst/"),
    ec_file:mkdir_p(DstDir),
    Dst = filename:join(DstDir, "file"),
    ?assert(filelib:is_dir(DstDir)),
    ?assertEqual(ok, rebar_file_utils:mv(F, DstDir)),
    ?assert(filelib:is_file(Dst)),
    ?assertNot(filelib:is_file(F)),
    ok.

mv_file_dir_diff(Config) ->
    %% Move a file to a directory while renaming
    PrivDir = ?config(priv_dir, Config),
    BaseDir = mk_base_dir(PrivDir, mv_file_dir_diff),
    SrcDir = filename:join(BaseDir, "src/"),
    ec_file:mkdir_p(SrcDir),
    ?assert(filelib:is_dir(SrcDir)),
    F = filename:join(SrcDir, "file"),
    file:write_file(F, "hello"),
    DstDir = filename:join(BaseDir, "dst/"),
    ec_file:mkdir_p(DstDir),
    Dst = filename:join(DstDir, "file-rename"),
    ?assert(filelib:is_dir(DstDir)),
    ?assertNot(filelib:is_file(Dst)),
    ?assertEqual(ok, rebar_file_utils:mv(F, Dst)),
    ?assert(filelib:is_file(Dst)),
    ?assertNot(filelib:is_file(F)),
    ok.

mv_no_clobber(Config) ->
    %% Moving a file while renaming does not clobber other files
    PrivDir = ?config(priv_dir, Config),
    BaseDir = mk_base_dir(PrivDir, mv_no_clobber),
    SrcDir = filename:join(BaseDir, "src/"),
    ec_file:mkdir_p(SrcDir),
    ?assert(filelib:is_dir(SrcDir)),
    F = filename:join(SrcDir, "file"),
    file:write_file(F, "hello"),
    FBad = filename:join(SrcDir, "file-alt"),
    file:write_file(FBad, "wrong-data"),
    DstDir = filename:join(BaseDir, "dst/"),
    ec_file:mkdir_p(DstDir),
    Dst = filename:join(DstDir, "file-alt"),
    DstBad = filename:join(DstDir, "file"),
    file:write_file(DstBad, "wrong-data"),
    ?assert(filelib:is_file(F)),
    ?assert(filelib:is_file(FBad)),
    ?assert(filelib:is_dir(DstDir)),
    ?assertNot(filelib:is_file(Dst)),
    ?assert(filelib:is_file(DstBad)),
    ?assertEqual(ok, rebar_file_utils:mv(F, Dst)),
    ?assert(filelib:is_file(Dst)),
    ?assertNot(filelib:is_file(F)),
    ?assert(filelib:is_file(DstBad)),
    ?assert(filelib:is_file(FBad)),
    ?assertEqual({ok, <<"hello">>}, file:read_file(Dst)),
    ?assertEqual({ok, <<"wrong-data">>}, file:read_file(FBad)),
    ?assertEqual({ok, <<"wrong-data">>}, file:read_file(DstBad)),
    ok.


mk_base_dir(BasePath, Name) ->
    {_,_,Micro} = os:timestamp(),
    Index = integer_to_list(Micro),
    Path = filename:join(BasePath, atom_to_list(Name) ++ Index),
    ec_file:mkdir_p(Path),
    Path.


symlink_existing_dir(Config) ->
    PrivDir = ?config(priv_dir, Config),

    Source = filename:join([PrivDir, "exists"]),
    Target = filename:join([PrivDir, "_build", "test", "lib", "app", "exists"]),
    ok = filelib:ensure_dir(filename:join([PrivDir, "_build", "test", "lib", "app", "dummy"])),

    ok = filelib:ensure_dir(filename:join([Source, "dummy"])),

    ok = rebar_file_utils:symlink_or_create_dir(Source, Target),

    %% `Target' should be a symlink, and `Source'
    %% should exist
    ?assert(ec_file:is_dir(Target)),
    ?assert(ec_file:is_symlink(Target)),
    ?assert(ec_file:is_dir(Source)),

    %% repeat to check for idempotence
    ok = rebar_file_utils:symlink_or_create_dir(Source, Target),

    %% `Target' should be a symlink, and `Source'
    %% should exist
    ?assert(ec_file:is_dir(Target)),
    ?assert(ec_file:is_symlink(Target)),
    ?assert(ec_file:is_dir(Source)).

preserve_existing_symlink(Config) ->
    PrivDir = ?config(priv_dir, Config),

    Source = filename:join([PrivDir, "symlinked"]),
    Target = filename:join([PrivDir, "_build", "test", "lib", "app", "symlinked"]),
    ok = filelib:ensure_dir(filename:join([PrivDir, "_build", "test", "lib", "app", "dummy"])),

    ok = filelib:ensure_dir(filename:join([Source, "dummy"])),

    ok = rebar_file_utils:symlink(Source, Target),

    ok = rebar_file_utils:symlink_or_create_dir(Source, Target),

    %% `Target' should be a symlink, and `Source'
    %% should exist
    ?assert(ec_file:is_dir(Target)),
    ?assert(ec_file:is_symlink(Target)),
    ?assert(ec_file:is_dir(Source)),

    %% repeat to check for idempotence
    ok = rebar_file_utils:symlink_or_create_dir(Source, Target),

    %% `Target' should be a symlink, and `Source'
    %% should exist
    ?assert(ec_file:is_dir(Target)),
    ?assert(ec_file:is_symlink(Target)),
    ?assert(ec_file:is_dir(Source)).

create_missing_dir(Config) ->
    PrivDir = ?config(priv_dir, Config),

    Source = filename:join([PrivDir, "missing"]),
    Target = filename:join([PrivDir, "_build", "test", "lib", "app", "missing"]),
    ok = filelib:ensure_dir(filename:join([PrivDir, "_build", "test", "lib", "app", "dummy"])),

    ok = rebar_file_utils:symlink_or_create_dir(Source, Target),

    %% `Target' should be a directory not a symlink, and `Source'
    %% should not exist
    ?assert(ec_file:is_dir(Target)),
    ?assert(not ec_file:is_symlink(Target)),
    ?assert(not ec_file:is_dir(Source)),

    %% repeat to check for idempotence
    ok = rebar_file_utils:symlink_or_create_dir(Source, Target),

    %% `Target' should be a directory not a symlink, and `Source'
    %% should not exist
    ?assert(ec_file:is_dir(Target)),
    ?assert(not ec_file:is_symlink(Target)),
    ?assert(not ec_file:is_dir(Source)).

preserve_existing_dir(Config) ->
    PrivDir = ?config(priv_dir, Config),

    Source = filename:join([PrivDir, "created"]),
    Target = filename:join([PrivDir, "_build", "test", "lib", "app", "created"]),
    ok = filelib:ensure_dir(filename:join([PrivDir, "_build", "test", "lib", "app", "dummy"])),

    ok = filelib:ensure_dir(filename:join([Target, "dummy"])),

    ok = rebar_file_utils:symlink_or_create_dir(Source, Target),

    %% `Target' should be a directory, and `Source'
    %% should not exist
    ?assert(ec_file:is_dir(Target)),
    ?assert(not ec_file:is_symlink(Target)),
    ?assert(not ec_file:is_dir(Source)),

    %% repeat to check for idempotence
    ok = rebar_file_utils:symlink_or_create_dir(Source, Target),

    %% `Target' should be a directory, and `Source'
    %% should not exist
    ?assert(ec_file:is_dir(Target)),
    ?assert(not ec_file:is_symlink(Target)),
    ?assert(not ec_file:is_dir(Source)).

delete_symlink_and_create_dir(Config) ->
    PrivDir = ?config(priv_dir, Config),

    Source = filename:join([PrivDir, "gone"]),
    Target = filename:join([PrivDir, "_build", "test", "lib", "app", "gone"]),
    ok = filelib:ensure_dir(filename:join([PrivDir, "_build", "test", "lib", "app", "dummy"])),

    %% create dir temporarily to ensure symlink can be created
    ok = filelib:ensure_dir(filename:join([Source, "dummy"])),

    ok = rebar_file_utils:symlink(Source, Target),

    %% remove source dir
    ok = ec_file:remove(Source, [recursive]),

    ?assert(ec_file:is_symlink(Target)),
    ?assert(not ec_file:is_dir(Source)),

    ok = rebar_file_utils:symlink_or_create_dir(Source, Target),

    %% `Target' should be a directory not a symlink, and `Source'
    %% should not exist
    ?assert(not ec_file:is_symlink(Target)),
    ?assert(ec_file:is_dir(Target)),
    ?assert(not ec_file:is_dir(Source)),

    %% repeat to check for idempotence
    ok = rebar_file_utils:symlink_or_create_dir(Source, Target),

    %% `Target' should be a directory not a symlink, and `Source'
    %% should not exist
    ?assert(not ec_file:is_symlink(Target)),
    ?assert(ec_file:is_dir(Target)),
    ?assert(not ec_file:is_dir(Source)).

delete_dir_and_create_symlink(Config) ->
    PrivDir = ?config(priv_dir, Config),

    Source = filename:join([PrivDir, "create"]),
    Target = filename:join([PrivDir, "_build", "test", "lib", "app", "create"]),
    ok = filelib:ensure_dir(filename:join([PrivDir, "_build", "test", "lib", "app", "dummy"])),

    ok = filelib:ensure_dir(filename:join([Source, "dummy"])),
    ok = filelib:ensure_dir(filename:join([Target, "dummy"])),

    ?assert(not ec_file:is_symlink(Target)),
    ?assert(ec_file:is_dir(Target)),
    ?assert(ec_file:is_dir(Source)),

    ok = rebar_file_utils:symlink_or_create_dir(Source, Target),

    %% `Target' should be a symlink not a directory, and `Source'
    %% should exist
    ?assert(ec_file:is_dir(Target)),
    ?assert(ec_file:is_symlink(Target)),
    ?assert(ec_file:is_dir(Source)),

    %% repeat to check for idempotence
    ok = rebar_file_utils:symlink_or_create_dir(Source, Target),

    %% `Target' should be a symlink not a directory, and `Source'
    %% should exist
    ?assert(ec_file:is_dir(Target)),
    ?assert(ec_file:is_symlink(Target)),
    ?assert(ec_file:is_dir(Source)).