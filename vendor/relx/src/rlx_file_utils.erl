-module(rlx_file_utils).

-export([mkdtemp/0,
         mkdir_p/1,
         wildcard_paths/1,
         copy/2,
         copy/3,
         ensure_writable/1,
         copy_file_info/3,
         exists/1,
         write/2,
         write_term/2,
         is_symlink/1,
         is_dir/1,
         type/1,
         symlink_or_copy/2,
         remove/1,
         remove/2,
         print_path/1,
         path_from_ancestor/2,
         format_error/1]).

-include("relx.hrl").
-include_lib("kernel/include/file.hrl").

-spec mkdtemp() -> file:name() | {error, term()}.
mkdtemp() ->
    UniqueNumber = erlang:integer_to_list(rand:uniform(1000000000000)),
    TmpDirPath = filename:join(tmp(), [".tmp_dir-", UniqueNumber]),
    case mkdir_p(TmpDirPath) of
        ok ->
            TmpDirPath;
        Error ->
            Error
    end.

-spec tmp() -> file:name().
tmp() ->
    case erlang:system_info(system_architecture) of
        "win32" ->
            case os:getenv("TEMP") of
                false ->
                    "./tmp";
                Val ->
                    Val
            end;
        _SysArch ->
            case os:getenv("TMPDIR") of
                false ->
                    "/tmp";
                Val ->
                    Val
            end
    end.

%% @doc Makes a directory including parent dirs if they are missing.
-spec mkdir_p(string()) -> ok | {error, Reason::file:posix()}.
mkdir_p(Path) ->
    %% We are exploiting a feature of ensuredir that that creates all
    %% directories up to the last element in the filename, then ignores
    %% that last element. This way we ensure that the dir is created
    %% and not have any worries about path names
    DirName = filename:join([filename:absname(Path), "tmp"]),
    filelib:ensure_dir(DirName).

%% @doc expand wildcards and names in the given paths
-spec wildcard_paths([file:filename_all()]) -> [string()].
wildcard_paths(Paths) ->
    [filename:absname(Expanded) || Path <- Paths, Expanded <- wildcard(Path)].

%% In case the given directory does not expand,
%% we return it back in a list so we trigger the
%% proper error reportings.
-spec wildcard(file:filename_all()) -> [string()].
wildcard(Path) when is_binary(Path) ->
    wildcard(binary_to_list(Path));
wildcard(Path) when is_list(Path) ->
    case filelib:wildcard(Path) of
        []   -> [Path];
        Paths -> Paths
    end.

-spec exists(file:filename()) -> boolean().
exists(Filename) ->
    case file:read_file_info(Filename) of
        {ok, _}         ->
            true;
        {error, _Reason} ->
            false
    end.

-spec write(file:name(), string()) -> ok | {error, term()}.
write(FileName, Contents) ->
    file:write_file(FileName, Contents).

-spec write_term(file:filename(), term()) -> ok | {error, term()}.
write_term(FileName, Term) ->
    write(FileName, lists:flatten(io_lib:fwrite("~p. ", [Term]))).

-spec is_symlink(file:name()) -> boolean().
is_symlink(Path) ->
    case file:read_link_info(Path) of
        {ok, #file_info{type = symlink}} ->
            true;
        _ ->
            false
    end.

symlink_or_copy(Source, Target) ->
    case file:make_symlink(Source, Target) of
        ok ->
            ok;
        {error, eexist} ->
            {error, eexist};
        {error, Err} ->
            case {os:type(), Err} of
                {{win32, _}, eperm} ->
                    % We get eperm on Windows if we do not have
                    %   SeCreateSymbolicLinkPrivilege
                    % Try the next alternative
                    win32_make_junction_or_copy(Source, Target);
                _ ->
                    % On other systems we try to copy next
                    cp_r(Source, Target)
            end
    end.

cp_r(Source, Target) ->
    copy(Source, Target, [{recursive, true}, {file_info, [mode, time, owner, group]}]).

win32_make_junction_or_copy(Source, Target) ->
    case filelib:is_dir(Source) of
        true ->
            win32_make_junction(Source, Target);
        _ ->
            cp_r(Source, Target)
    end.

win32_make_junction(Source, Target) ->
    % The mklink will fail if the target already exists, check for that first
    case file:read_link_info(Target) of
        {error, enoent} ->
            win32_make_junction_cmd(Source, Target);
        {ok, #file_info{type = symlink}} ->
            case file:read_link(Target) of
                {ok, Source} ->
                    ok;
                {ok, _} ->
                    ok = file:del_dir(Target),
                    win32_make_junction_cmd(Source, Target);
                {error, Reason} ->
                    {error, {readlink, Reason}}
            end;
        {ok, #file_info{type = _Type}} ->
            % Directory already exists, so we overwrite the copy
            cp_r(Source, Target);
        Error ->
            Error
    end.

win32_make_junction_cmd(Source, Target) ->
    S = unicode:characters_to_list(Source),
    T = unicode:characters_to_list(Target),
    Cmd = "cmd /c mklink /j \"" ++ filename:nativename(T) ++ "\" \"" ++ filename:nativename(S) ++ "\"",
    case os:cmd(Cmd) of
        "Junction created " ++ _ ->
            ok;
        _ ->
            % When not English output, success out will not match, so just recheck link target
            % if target is source, then also do nothing
            case file:read_link(Target) of
                {ok, Source} ->
                    ok;
                _ ->
                    cp_r(Source, Target)
            end
    end.

-spec copy(file:name(), file:name()) -> ok | {error, term()}.
copy(From, To) ->
    copy_(From, To, [{file_info, [mode, time, owner, group]}]).

%% @doc copy an entire directory to another location.
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

copy_(From, To, Options) ->
    case file:copy(From, To) of
        {ok, _} ->
            copy_file_info(To, From, proplists:get_value(file_info, Options, []));
        {error, Error} ->
            {error, {copy_failed, From, To, Error}}
    end.

ensure_writable(File) ->
    {ok, #file_info{mode=CurrentMode}} = file:read_file_info(File),
    case file:change_mode(File, CurrentMode bor 8#00200) of
        ok ->
            ok;
        {error, Reason} ->
            erlang:error(?RLX_ERROR({ensure_writable_failed, File, Reason}))
    end.

copy_file_info(To, From, FileInfoToKeep) ->
    case file:read_file_info(From) of
        {ok, FileInfo} ->
            case write_file_info(To, FileInfo, FileInfoToKeep) of
                [] ->
                    ok;
                Errors ->
                    {error, {write_file_info_failed_for, From, To, Errors}}
            end;
        {error, RFError} ->
            {error, {read_file_info_failed, From, To, RFError}}
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

is_dir(Path) ->
    case file:read_file_info(Path) of
        {ok, #file_info{type = directory}} ->
            true;
        _ ->
            false
    end.

-spec make_dir_if_dir(file:name()) -> ok | {error, term()}.
make_dir_if_dir(File) ->
    case is_dir(File) of
        true  ->
            ok;
        false ->
            mkdir_path(File)
    end.

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
                        true ->
                            directory;
                        false ->
                            undefined
                    end
            end

    end.

%% @doc Makes a directory including parent dirs if they are missing.
-spec mkdir_path(file:name()) -> ok | {error, Reason::term()}.
mkdir_path(Path) ->
    mkdir_p(Path).

copy_subfiles(From, To, Options) ->
    Fun =
        fun(ChildFrom) ->
                ChildTo = filename:join([To, filename:basename(ChildFrom)]),
                copy(ChildFrom, ChildTo, Options)
        end,
    lists:foreach(Fun, sub_files(From)).

sub_files(From) ->
    {ok, SubFiles} = file:list_dir(From),
    [filename:join(From, SubFile) || SubFile <- SubFiles].

%% @doc delete a file. Use the recursive option for whole directory trees.
-spec remove(file:name(), [] | [recursive]) -> ok | {error, term()}.
remove(Path, Options) ->
    case lists:member(recursive, Options) of
        false -> remove(Path);
        true  -> remove_recursive(Path, Options)
    end.

%% @doc delete a file or directory, including symlinks and junctions
-spec remove(file:name()) -> ok | {error, term()}.
remove(Path) ->
    case is_symlink(Path) of
        true -> remove_symlink(Path);
        false ->
            case is_dir(Path) of
                true -> file:del_dir(Path);
                false -> file:delete(Path)
            end
    end.

-spec remove_recursive(file:name(), list()) -> ok | {error, term()}.
remove_recursive(Path, Options) ->
    case is_dir(Path) of
        false ->
            remove(Path);
        true ->
            lists:foreach(fun(ChildPath) ->
                                  remove_recursive(ChildPath, Options)
                          end, sub_files(Path)),
            file:del_dir(Path)
    end.

-spec remove_symlink(file:name()) -> ok | {error, term()}.
remove_symlink(Path) ->
    case os:type() of
        {win32, _} ->
            % On windows we remove symlinks and junctions differently if they refer to directories.
            case filelib:is_dir(Path) of
                true -> file:del_dir(Path);
                false -> file:delete(Path)
            end;
        _ ->
            % On other systems we use can use file:delete() in all cases.
            file:delete(Path)
    end.

%% returns the path for printing to logs. if get_cwd succeeds it
%% attempts to return the relative path. It falls back on juts
%% returning the original path.
print_path(OriginalPath) ->
   case file:get_cwd() of
       {ok, Cwd} ->
           case rlx_file_utils:path_from_ancestor(OriginalPath, Cwd) of
               {ok, ""} ->
                   OriginalPath;
               {ok, Path} ->
                   Path;
               _ ->
                   OriginalPath
           end;
       _ ->
           OriginalPath
   end.

%% for a given path return the path relative to a base directory
-spec path_from_ancestor(string(), string()) -> {ok, string()} | {error, badparent}.

path_from_ancestor(Target, To) ->
    path_from_ancestor_(filename:split(canonical_path(Target)),
                        filename:split(canonical_path(To))).

path_from_ancestor_([Part|Target], [Part|To]) -> path_from_ancestor_(Target, To);
path_from_ancestor_([], [])                   -> {ok, ""};
path_from_ancestor_(Target, [])               -> {ok, filename:join(Target)};
path_from_ancestor_(_, _)                     -> {error, badparent}.

%% reduce a filepath by removing all incidences of `.' and `..'
-spec canonical_path(string()) -> string().

canonical_path(Dir) ->
    Canon = canonical_path([], filename:split(filename:absname(Dir))),
    filename:nativename(Canon).

canonical_path([], [])                -> filename:absname("/");
canonical_path(Acc, [])               -> filename:join(lists:reverse(Acc));
canonical_path(Acc, ["."|Rest])       -> canonical_path(Acc, Rest);
canonical_path([_|Acc], [".."|Rest])  -> canonical_path(Acc, Rest);
canonical_path([], [".."|Rest])       -> canonical_path([], Rest);
canonical_path(Acc, [Component|Rest]) -> canonical_path([Component|Acc], Rest).

format_error({ensure_writable_failed, File, Reason}) ->
    io_lib:format("Making file ~ts writable failed with reason:~n~ts",
                  [File, file:format_error(Reason)]);
format_error({write_file_info_failed_for, From, To, Errors}) ->
    io_lib:format("Writing file info during copy of ~ts to ~ts failed with reasons:~n~ts",
                  [From, To, [[file:format_error(Error), "\n"] || Error <- Errors]]);
format_error({copy_failed, From, To, Reason}) ->
    io_lib:format("Copying ~ts to ~ts failed with reason: ~ts", [From, To, file:format_error(Reason)]);
format_error({read_file_info_failed, From, To, Reason}) ->
    io_lib:format("Reading file info failed during copy of ~ts ~ts with reason: ~ts",
                  [From, To, file:format_error(Reason)]).
