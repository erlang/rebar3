%%% vi:ts=4 sw=4 et
%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, Erlware LLC
%%% @doc
%%%  Helper functions for working with files.
%%% @end
%%%-------------------------------------------------------------------
-module(ec_file).

-export([
         exists/1,
         copy/2,
         copy/3,
         copy_file_info/3,
         insecure_mkdtemp/0,
         mkdir_path/1,
         mkdir_p/1,
         find/2,
         is_symlink/1,
         is_dir/1,
         type/1,
         real_dir_path/1,
         remove/1,
         remove/2,
         md5sum/1,
         sha1sum/1,
         read/1,
         write/2,
         write_term/2
        ]).

-export_type([
              option/0
             ]).

-include_lib("kernel/include/file.hrl").

-define(CHECK_PERMS_MSG,
        "Try checking that you have the correct permissions and try again~n").

%%============================================================================
%% Types
%%============================================================================
-type file_info() :: mode | time | owner | group.
-type option() :: recursive | {file_info, [file_info()]}.

%%%===================================================================
%%% API
%%%===================================================================
-spec exists(file:filename()) -> boolean().
exists(Filename) ->
    case file:read_file_info(Filename) of
        {ok, _}         ->
            true;
        {error, _Reason} ->
            false
    end.

%% @doc copy an entire directory to another location.
-spec copy(file:name(), file:name(), Options::[option()]) -> ok | {error, Reason::term()}.
copy(From, To, []) ->
    copy_(From, To, []);
copy(From, To, Options) ->
    case proplists:get_value(recursive,  Options, false) of
        true ->
            case is_dir(From) of
                false ->
                    copy_(From, To, Options);
                true ->
                    make_dir_if_dir(To),
                    copy_subfiles(From, To, Options)
            end;
        false ->
            copy_(From, To, Options)
    end.

%% @doc copy a file including timestamps,ownership and mode etc.
-spec copy(From::file:filename(), To::file:filename()) -> ok | {error, Reason::term()}.
copy(From, To) ->
    copy_(From, To, [{file_info, [mode, time, owner, group]}]).

copy_(From, To, Options) ->
    Linked
        = case file:read_link(From) of
            {ok, Linked0} -> Linked0;
            {error, _} -> undefined
          end,
    case Linked =/= undefined orelse file:copy(From, To) of
        true ->
            file:make_symlink(Linked, To);
        {ok, _} ->
            copy_file_info(To, From, proplists:get_value(file_info, Options, []));
        {error, Error} ->
            {error, {copy_failed, Error}}
    end.

copy_file_info(To, From, FileInfoToKeep) ->
    case file:read_file_info(From) of
        {ok, FileInfo} ->
            case write_file_info(To, FileInfo, FileInfoToKeep) of
                [] ->
                    ok;
                Errors ->
                    {error, {write_file_info_failed_for, Errors}}
            end;
        {error, RFError} ->
            {error, {read_file_info_failed, RFError}}
    end.

write_file_info(To, FileInfo, FileInfoToKeep) ->
    WriteInfoFuns = [{mode, fun try_write_mode/2},
                     {time, fun try_write_time/2},
                     {group, fun try_write_group/2},
                     {owner, fun try_write_owner/2}],
    lists:foldl(fun(Info, Acc) ->
                        case proplists:get_value(Info, WriteInfoFuns, undefined) of
                            undefined ->
                                Acc;
                            F ->
                                case F(To, FileInfo) of
                                    ok ->
                                        Acc;
                                    {error, Reason} ->
                                        [{Info, Reason} | Acc]
                                end
                        end
                end, [], FileInfoToKeep).


try_write_mode(To, #file_info{mode=Mode}) ->
    file:write_file_info(To, #file_info{mode=Mode}).

try_write_time(To, #file_info{atime=Atime, mtime=Mtime}) ->
    file:write_file_info(To, #file_info{atime=Atime, mtime=Mtime}).

try_write_owner(To, #file_info{uid=OwnerId}) ->
    file:write_file_info(To, #file_info{uid=OwnerId}).

try_write_group(To, #file_info{gid=OwnerId}) ->
    file:write_file_info(To, #file_info{gid=OwnerId}).

%% @doc return an md5 checksum string or a binary. Same as unix utility of
%%      same name.
-spec md5sum(string() | binary()) -> string().
md5sum(Value) ->
    hex(binary_to_list(erlang:md5(Value))).

%% @doc return an sha1sum checksum string or a binary. Same as unix utility of
%%      same name.
-ifdef(deprecated_crypto).
-spec sha1sum(string() | binary()) -> string().
sha1sum(Value) ->
    hex(binary_to_list(crypto:sha(Value))).
-else.
-spec sha1sum(string() | binary()) -> string().
sha1sum(Value) ->
    hex(binary_to_list(crypto:hash(sha, Value))).
-endif.

%% @doc delete a file. Use the recursive option for directories.
%% <pre>
%% Example: remove("./tmp_dir", [recursive]).
%% </pre>
-spec remove(file:name(), Options::[option()]) -> ok | {error, Reason::term()}.
remove(Path, Options) ->
    case lists:member(recursive, Options) of
        false -> file:delete(Path);
        true  -> remove_recursive(Path, Options)
    end.


%% @doc delete a file.
-spec remove(file:name()) -> ok | {error, Reason::term()}.
remove(Path) ->
    remove(Path, []).

%% @doc indicates witha boolean if the path supplied refers to symlink.
-spec is_symlink(file:name()) -> boolean().
is_symlink(Path) ->
    case file:read_link_info(Path) of
        {ok, #file_info{type = symlink}} ->
            true;
        _ ->
            false
    end.

is_dir(Path) ->
    case file:read_file_info(Path) of
        {ok, #file_info{type = directory}} ->
            true;
        _ ->
            false
    end.

%% @doc returns the type of the file.
-spec type(file:name()) -> file | symlink | directory | undefined.
type(Path) ->
    case filelib:is_regular(Path) of
        true ->
            file;
        false ->
            case is_symlink(Path) of
                true ->
                    symlink;
                false ->
                    case is_dir(Path) of
                        true -> directory;
                        false -> undefined
                    end
            end

    end.
%% @doc gets the real path of a directory. This is mostly useful for
%% resolving symlinks. Be aware that this temporarily changes the
%% current working directory to figure out what the actual path
%% is. That means that it can be quite slow.
-spec real_dir_path(file:name()) -> file:name().
real_dir_path(Path) ->
    {ok, CurCwd} = file:get_cwd(),
    ok = file:set_cwd(Path),
    {ok, RealPath} = file:get_cwd(),
    ok = file:set_cwd(CurCwd),
    filename:absname(RealPath).

%% @doc make a unique temporary directory. Similar function to BSD stdlib
%% function of the same name.
-spec insecure_mkdtemp() -> TmpDirPath::file:name() | {error, term()}.
insecure_mkdtemp() ->
    UniqueNumber = erlang:integer_to_list(erlang:trunc(random_uniform() * 1000000000000)),
    TmpDirPath =
        filename:join([tmp(), lists:flatten([".tmp_dir", UniqueNumber])]),

    case mkdir_path(TmpDirPath) of
        ok -> TmpDirPath;
        Error -> Error
    end.

%% @doc Makes a directory including parent dirs if they are missing.
-spec mkdir_p(file:name()) -> ok | {error, Reason::term()}.
mkdir_p(Path) ->
    %% We are exploiting a feature of ensuredir that that creates all
    %% directories up to the last element in the filename, then ignores
    %% that last element. This way we ensure that the dir is created
    %% and not have any worries about path names
    DirName = filename:join([filename:absname(Path), "tmp"]),
    filelib:ensure_dir(DirName).


%% @doc Makes a directory including parent dirs if they are missing.
-spec mkdir_path(file:name()) -> ok | {error, Reason::term()}.
mkdir_path(Path) ->
    mkdir_p(Path).


%% @doc read a file from the file system. Provide UEX exeption on failure.
-spec read(FilePath::file:filename()) -> {ok, binary()} | {error, Reason::term()}.
read(FilePath) ->
    %% Now that we are moving away from exceptions again this becomes
    %% a bit redundant but we want to be backwards compatible as much
    %% as possible in the api.
    file:read_file(FilePath).


%% @doc write a file to the file system. Provide UEX exeption on failure.
-spec write(FileName::file:filename(), Contents::string()) -> ok | {error, Reason::term()}.
write(FileName, Contents) ->
    %% Now that we are moving away from exceptions again this becomes
    %% a bit redundant but we want to be backwards compatible as much
    %% as possible in the api.
    file:write_file(FileName, Contents).

%% @doc write a term out to a file so that it can be consulted later.
-spec write_term(file:filename(), term()) -> ok | {error, Reason::term()}.
write_term(FileName, Term) ->
    write(FileName, lists:flatten(io_lib:fwrite("~p. ", [Term]))).

%% @doc Finds files and directories that match the regexp supplied in
%%  the TargetPattern regexp.
-spec find(FromDir::file:name(), TargetPattern::string()) -> [file:name()].
find([], _) ->
    [];
find(FromDir, TargetPattern) ->
    case is_dir(FromDir) of
        false ->
            case re:run(FromDir, TargetPattern) of
                {match, _} -> [FromDir];
                _ -> []
            end;
        true ->
            FoundDir = case re:run(FromDir, TargetPattern) of
                           {match, _} -> [FromDir];
                           _ -> []
                       end,
            List = find_in_subdirs(FromDir, TargetPattern),
            FoundDir ++ List
    end.
%%%===================================================================
%%% Internal Functions
%%%===================================================================
-spec find_in_subdirs(file:name(), string()) -> [file:name()].
find_in_subdirs(FromDir, TargetPattern) ->
    lists:foldl(fun(CheckFromDir, Acc)
                      when CheckFromDir == FromDir ->
                        Acc;
                   (ChildFromDir, Acc) ->
                        case find(ChildFromDir, TargetPattern) of
                            []  -> Acc;
                            Res -> Res ++ Acc
                        end
                end,
                [],
                sub_files(FromDir)).



-spec remove_recursive(file:name(), Options::list()) -> ok | {error, Reason::term()}.
remove_recursive(Path, Options) ->
    case is_dir(Path) of
        false ->
            file:delete(Path);
        true ->
            lists:foreach(fun(ChildPath) ->
                                  remove_recursive(ChildPath, Options)
                          end, sub_files(Path)),
            file:del_dir(Path)
    end.

-spec tmp() -> file:name().
tmp() ->
    case erlang:system_info(system_architecture) of
        "win32" ->
            case os:getenv("TEMP") of
                false -> "./tmp";
                Val -> Val
            end;
        _SysArch ->
            case os:getenv("TMPDIR") of
                false -> "/tmp";
                Val -> Val
            end
    end.

%% Copy the subfiles of the From directory to the to directory.
-spec copy_subfiles(file:name(), file:name(), [option()]) -> {error, Reason::term()} | ok.
copy_subfiles(From, To, Options) ->
    Fun =
        fun(ChildFrom) ->
                ChildTo = filename:join([To, filename:basename(ChildFrom)]),
                copy(ChildFrom, ChildTo, Options)
        end,
    lists:foreach(Fun, sub_files(From)).

-spec make_dir_if_dir(file:name()) -> ok | {error, Reason::term()}.
make_dir_if_dir(File) ->
    case is_dir(File) of
        true  -> ok;
        false -> mkdir_path(File)
    end.

%% @doc convert a list of integers into hex.
-spec hex(string() | non_neg_integer()) -> string().
hex(L) when is_list (L) ->
    lists:flatten([hex(I) || I <- L]);
hex(I) when I > 16#f ->
    [hex0((I band 16#f0) bsr 4), hex0((I band 16#0f))];
hex(I)               ->
    [$0, hex0(I)].

hex0(10) -> $a;
hex0(11) -> $b;
hex0(12) -> $c;
hex0(13) -> $d;
hex0(14) -> $e;
hex0(15) -> $f;
hex0(I)  -> $0 + I.


sub_files(From) ->
    {ok, SubFiles} = file:list_dir(From),
    [filename:join(From, SubFile) || SubFile <- SubFiles].

-ifdef(rand_module).
random_uniform() ->
    rand:uniform().
-else.
random_uniform() ->
    random:seed(os:timestamp()),
    random:uniform().
-endif.

%%%===================================================================
%%% Test Functions
%%%===================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

setup_test() ->
    Dir = insecure_mkdtemp(),
    mkdir_path(Dir),
    ?assertMatch(false, is_symlink(Dir)),
    ?assertMatch(true, filelib:is_dir(Dir)).

md5sum_test() ->
    ?assertMatch("cfcd208495d565ef66e7dff9f98764da", md5sum("0")).

sha1sum_test() ->
    ?assertMatch("b6589fc6ab0dc82cf12099d1c2d40ab994e8410c", sha1sum("0")).

file_test() ->
    Dir = insecure_mkdtemp(),
    TermFile = filename:join(Dir, "ec_file/dir/file.term"),
    TermFileCopy = filename:join(Dir, "ec_file/dircopy/file.term"),
    filelib:ensure_dir(TermFile),
    filelib:ensure_dir(TermFileCopy),
    write_term(TermFile, "term"),
    ?assertMatch({ok, <<"\"term\". ">>}, read(TermFile)),
    copy(filename:dirname(TermFile),
         filename:dirname(TermFileCopy),
         [recursive]).

teardown_test() ->
    Dir = insecure_mkdtemp(),
    remove(Dir, [recursive]),
    ?assertMatch(false, filelib:is_dir(Dir)).

setup_base_and_target() ->
    BaseDir = insecure_mkdtemp(),
    DummyContents = <<"This should be deleted">>,
    SourceDir = filename:join([BaseDir, "source"]),
    ok = file:make_dir(SourceDir),
    Name1 = filename:join([SourceDir, "fileone"]),
    Name2 = filename:join([SourceDir, "filetwo"]),
    Name3 = filename:join([SourceDir, "filethree"]),
    NoName = filename:join([SourceDir, "noname"]),

    ok = file:write_file(Name1, DummyContents),
    ok = file:write_file(Name2, DummyContents),
    ok = file:write_file(Name3, DummyContents),
    ok = file:write_file(NoName, DummyContents),
    {BaseDir, SourceDir, {Name1, Name2, Name3, NoName}}.

exists_test() ->
    BaseDir = insecure_mkdtemp(),
    SourceDir = filename:join([BaseDir, "source1"]),
    NoName = filename:join([SourceDir, "noname"]),
    ok = file:make_dir(SourceDir),
    Name1 = filename:join([SourceDir, "fileone"]),
    ok = file:write_file(Name1, <<"Testn">>),
    ?assertMatch(true, exists(Name1)),
    ?assertMatch(false, exists(NoName)).

real_path_test() ->
    BaseDir = "foo",
    Dir = filename:absname(filename:join(BaseDir, "source1")),
    LinkDir = filename:join([BaseDir, "link"]),
    ok = mkdir_p(Dir),
    file:make_symlink(Dir, LinkDir),
    ?assertEqual(Dir, real_dir_path(LinkDir)),
    ?assertEqual(directory, type(Dir)),
    ?assertEqual(symlink, type(LinkDir)),
    TermFile = filename:join(BaseDir, "test_file"),
    ok = write_term(TermFile, foo),
    ?assertEqual(file, type(TermFile)),
    ?assertEqual(true, is_symlink(LinkDir)),
    ?assertEqual(false, is_symlink(Dir)).

find_test() ->
    %% Create a directory in /tmp for the test. Clean everything afterwards
    {BaseDir, _SourceDir, {Name1, Name2, Name3, _NoName}} = setup_base_and_target(),
    Result = find(BaseDir, "file[a-z]+\$"),
    ?assertMatch(3, erlang:length(Result)),
    ?assertEqual(true, lists:member(Name1, Result)),
    ?assertEqual(true, lists:member(Name2, Result)),
    ?assertEqual(true, lists:member(Name3, Result)),
    remove(BaseDir, [recursive]).

-endif.
