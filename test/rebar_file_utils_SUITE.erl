-module(rebar_file_utils_SUITE).

-export([all/0,
         groups/0,
         init_per_group/2,
         end_per_group/2,
         raw_tmpdir/1,
         empty_tmpdir/1,
         simple_tmpdir/1,
         multi_tmpdir/1,
         reset_nonexistent_dir/1,
         reset_empty_dir/1,
         reset_dir/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").


all() ->
    [{group, tmpdir},
     {group, reset_dir}].

groups() ->
    [{tmpdir, [], [raw_tmpdir, empty_tmpdir, simple_tmpdir, multi_tmpdir]},
     {reset_dir, [], [reset_nonexistent_dir, reset_empty_dir, reset_dir]}].

init_per_group(reset_dir, Config) ->
    TmpDir = rebar_file_utils:system_tmpdir(["rebar_file_utils_SUITE", "resetable"]),
    [{tmpdir, TmpDir}|Config];
init_per_group(_, Config) -> Config.
end_per_group(_, Config) -> Config.

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
    {ok, []} = file:list_dir_all(TmpDir).

reset_empty_dir(Config) ->
    TmpDir = ?config(tmpdir, Config),
    _ = ec_file:remove(TmpDir, [recursive]),
    _ = filelib:ensure_dir(filename:join([TmpDir, "dummy.beam"])),
    ?assert(filelib:is_dir(TmpDir)),
    ok = rebar_file_utils:reset_dir(TmpDir),
    ?assert(filelib:is_dir(TmpDir)),
    {ok, []} = file:list_dir_all(TmpDir).

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
    {ok, []} = file:list_dir_all(TmpDir).
