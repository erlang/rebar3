%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2009 Dave Smith (dizzyd@dizzyd.com)
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%% -------------------------------------------------------------------
-module(rebar_file_utils).

-export([try_consult/1,
         consult_config/2,
         format_error/1,
         symlink_or_copy/2,
         rm_rf/1,
         cp_r/2,
         mv/2,
         delete_each/1,
         write_file_if_contents_differ/2,
         system_tmpdir/0,
         system_tmpdir/1,
         reset_dir/1,
         touch/1,
         path_from_ancestor/2,
         canonical_path/1,
         resolve_link/1,
         split_dirname/1]).

-include("rebar.hrl").

-include_lib("providers/include/providers.hrl").
-include_lib("kernel/include/file.hrl").


%% ===================================================================
%% Public API
%% ===================================================================

try_consult(File) ->
    case file:consult(File) of
        {ok, Terms} ->
            Terms;
        {error, enoent} ->
            [];
        {error, Reason} ->
            throw(?PRV_ERROR({bad_term_file, File, Reason}))
    end.

-spec consult_config(rebar_state:t(), string()) -> [[tuple()]].
consult_config(State, Filename) ->
    Fullpath = filename:join(rebar_dir:root_dir(State), Filename),
    ?DEBUG("Loading configuration from ~p", [Fullpath]),
    Config = case try_consult(Fullpath) of
        [T] -> T;
        [] -> []
    end,
    SubConfigs = [consult_config(State, Entry ++ ".config") ||
                  Entry <- Config, is_list(Entry)
                 ],

    [Config | lists:merge(SubConfigs)].

format_error({bad_term_file, AppFile, Reason}) ->
    io_lib:format("Error reading file ~s: ~s", [AppFile, file:format_error(Reason)]).

symlink_or_copy(Source, Target) ->
    Link = case os:type() of
               {win32, _} ->
                   Source;
               _ ->
                   rebar_dir:make_relative_path(Source, Target)
           end,
    case file:make_symlink(Link, Target) of
        ok ->
            ok;
        {error, eexist} ->
            exists;
        {error, _} ->
            case os:type() of
                {win32, _} ->
                    S = unicode:characters_to_list(Source),
                    T = unicode:characters_to_list(Target),
                    case filelib:is_dir(S) of
                        true ->
                            win32_symlink(S, T);
                        false ->
                            cp_r([S], T)
                    end;
                _ ->
                    case filelib:is_dir(Target) of
                        true ->
                            ok;
                        false ->
                            cp_r([Source], Target)
                    end
            end
    end.

win32_symlink(Source, Target) ->
    Res = rebar_utils:sh(
            ?FMT("cmd /c mklink /j \"~s\" \"~s\"",
                 [rebar_utils:escape_double_quotes(filename:nativename(Target)),
                  rebar_utils:escape_double_quotes(filename:nativename(Source))]),
            [{use_stdout, false}, return_on_error]),
    case win32_ok(Res) of
        true -> ok;
        false ->
            {error, lists:flatten(
                      io_lib:format("Failed to symlink ~s to ~s~n",
                                    [Source, Target]))}
    end.


%% @doc Remove files and directories.
%% Target is a single filename, directoryname or wildcard expression.
-spec rm_rf(string()) -> 'ok'.
rm_rf(Target) ->
    case os:type() of
        {unix, _} ->
            EscTarget = rebar_utils:escape_chars(Target),
            {ok, []} = rebar_utils:sh(?FMT("rm -rf ~s", [EscTarget]),
                                      [{use_stdout, false}, abort_on_error]),
            ok;
        {win32, _} ->
            Filelist = filelib:wildcard(Target),
            Dirs = [F || F <- Filelist, filelib:is_dir(F)],
            Files = Filelist -- Dirs,
            ok = delete_each(Files),
            ok = delete_each_dir_win32(Dirs),
            ok
    end.

-spec cp_r(list(string()), file:filename()) -> 'ok'.
cp_r([], _Dest) ->
    ok;
cp_r(Sources, Dest) ->
    case os:type() of
        {unix, _} ->
            EscSources = [rebar_utils:escape_chars(Src) || Src <- Sources],
            SourceStr = string:join(EscSources, " "),
            {ok, []} = rebar_utils:sh(?FMT("cp -Rp ~s \"~s\"",
                                           [SourceStr, rebar_utils:escape_double_quotes(Dest)]),
                                      [{use_stdout, false}, abort_on_error]),
            ok;
        {win32, _} ->
            lists:foreach(fun(Src) -> ok = cp_r_win32(Src,Dest) end, Sources),
            ok
    end.

-spec mv(string(), file:filename()) -> 'ok'.
mv(Source, Dest) ->
    case os:type() of
        {unix, _} ->
            EscSource = rebar_utils:escape_chars(Source),
            EscDest = rebar_utils:escape_chars(Dest),
            {ok, []} = rebar_utils:sh(?FMT("mv ~s ~s", [EscSource, EscDest]),
                                      [{use_stdout, false}, abort_on_error]),
            ok;
        {win32, _} ->
            Cmd = case filelib:is_dir(Source) of
                      true ->
                          ?FMT("robocopy /move /e \"~s\" \"~s\" 1> nul",
                               [rebar_utils:escape_double_quotes(filename:nativename(Source)),
                                rebar_utils:escape_double_quotes(filename:nativename(Dest))]);
                      false ->
                          ?FMT("robocopy /move /e \"~s\" \"~s\" \"~s\" 1> nul",
                               [rebar_utils:escape_double_quotes(filename:nativename(filename:dirname(Source))),
                                rebar_utils:escape_double_quotes(filename:nativename(Dest)),
                                rebar_utils:escape_double_quotes(filename:basename(Source))])
                  end,
            Res = rebar_utils:sh(Cmd,
                        [{use_stdout, false}, return_on_error]),
            case win32_ok(Res) of
                true -> ok;
                false ->
                    {error, lists:flatten(
                              io_lib:format("Failed to move ~s to ~s~n",
                                            [Source, Dest]))}
            end
    end.

win32_ok({ok, _}) -> true;
win32_ok({error, {Rc, _}}) when Rc<9; Rc=:=16 -> true;
win32_ok(_) -> false.

delete_each([]) ->
    ok;
delete_each([File | Rest]) ->
    case file:delete(File) of
        ok ->
            delete_each(Rest);
        {error, enoent} ->
            delete_each(Rest);
        {error, Reason} ->
            ?ERROR("Failed to delete file ~s: ~p\n", [File, Reason]),
            ?FAIL
    end.

write_file_if_contents_differ(Filename, Bytes) ->
    ToWrite = iolist_to_binary(Bytes),
    case file:read_file(Filename) of
        {ok, ToWrite} ->
            ok;
        {ok,  _} ->
            file:write_file(Filename, ToWrite, [raw]);
        {error,  _} ->
            file:write_file(Filename, ToWrite, [raw])
    end.

%% returns an os appropriate tmpdir given a path
-spec system_tmpdir() -> file:filename().
-spec system_tmpdir(PathComponents) -> file:filename() when
      PathComponents :: [file:name()].

system_tmpdir() -> system_tmpdir([]).
system_tmpdir(PathComponents) ->
    Tmp = case erlang:system_info(system_architecture) of
        "win32" ->
            "./tmp";
        _SysArch ->
            "/tmp"
    end,
    filename:join([Tmp|PathComponents]).

%% recursively removes a directory and then recreates the same
%%  directory but empty
-spec reset_dir(Path) -> ok | {error, Reason} when
      Path   :: file:name(),
      Reason :: file:posix().

reset_dir(Path) ->
    %% delete the directory if it exists
    _ = ec_file:remove(Path, [recursive]),
    %% recreate the directory
    filelib:ensure_dir(filename:join([Path, "dummy.beam"])).


%% Linux touch but using erlang functions to work in bot *nix os and
%% windows
-spec touch(Path) -> ok | {error, Reason} when
      Path :: file:name(),
      Reason :: file:posix().
touch(Path) ->
    {ok, A} = file:read_file_info(Path),
    ok = file:write_file_info(Path, A#file_info{mtime = calendar:local_time(),
                                                atime = calendar:local_time()}).

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

%% returns canonical target of path if path is a link, otherwise returns path
-spec resolve_link(string()) -> string().

resolve_link(Path) ->
    case file:read_link(Path) of
        {ok, Target} ->
            canonical_path(filename:absname(Target, filename:dirname(Path)));
        {error, _} -> Path
    end.

%% splits a path into dirname and basename
-spec split_dirname(string()) -> {string(), string()}.

split_dirname(Path) ->
    {filename:dirname(Path), filename:basename(Path)}.

%% ===================================================================
%% Internal functions
%% ===================================================================

delete_each_dir_win32([]) -> ok;
delete_each_dir_win32([Dir | Rest]) ->
    {ok, []} = rebar_utils:sh(?FMT("rd /q /s \"~s\"",
                                   [rebar_utils:escape_double_quotes(filename:nativename(Dir))]),
                              [{use_stdout, false}, return_on_error]),
    delete_each_dir_win32(Rest).

xcopy_win32(Source,Dest)->
    %% "xcopy \"~s\" \"~s\" /q /y /e 2> nul", Changed to robocopy to
    %% handle long names. May have issues with older windows.
    Cmd = case filelib:is_dir(Source) of
              true ->
                  %% For robocopy, copying /a/b/c/ to /d/e/f/ recursively does not
                  %% create /d/e/f/c/*, but rather copies all files to /d/e/f/*.
                  %% The usage we make here expects the former, not the later, so we
                  %% must manually add the last fragment of a directory to the `Dest`
                  %% in order to properly replicate POSIX platforms
                  NewDest = filename:join([Dest, filename:basename(Source)]),
                  ?FMT("robocopy \"~s\" \"~s\" /e /is 1> nul",
                       [rebar_utils:escape_double_quotes(filename:nativename(Source)),
                        rebar_utils:escape_double_quotes(filename:nativename(NewDest))]);
              false ->
                  ?FMT("robocopy \"~s\" \"~s\" \"~s\" /e /is 1> nul",
                       [rebar_utils:escape_double_quotes(filename:nativename(filename:dirname(Source))),
                        rebar_utils:escape_double_quotes(filename:nativename(Dest)),
                        rebar_utils:escape_double_quotes(filename:basename(Source))])
          end,
    Res = rebar_utils:sh(Cmd,
                [{use_stdout, false}, return_on_error]),
    case win32_ok(Res) of
                true -> ok;
                false ->
                    {error, lists:flatten(
                              io_lib:format("Failed to copy ~s to ~s~n",
                                            [Source, Dest]))}
    end.

cp_r_win32({true, SourceDir}, {true, DestDir}) ->
    %% from directory to directory
     ok = case file:make_dir(DestDir) of
             {error, eexist} -> ok;
             Other -> Other
         end,
    ok = xcopy_win32(SourceDir, DestDir);
cp_r_win32({false, Source} = S,{true, DestDir}) ->
    %% from file to directory
    cp_r_win32(S, {false, filename:join(DestDir, filename:basename(Source))});
cp_r_win32({false, Source},{false, Dest}) ->
    %% from file to file
    {ok,_} = file:copy(Source, Dest),
    ok;
cp_r_win32({true, SourceDir}, {false, DestDir}) ->
    case filelib:is_regular(DestDir) of
        true ->
            %% From directory to file? This shouldn't happen
            {error, lists:flatten(
                      io_lib:format("Cannot copy dir (~p) to file (~p)\n",
                                    [SourceDir, DestDir]))};
        false ->
            %% Specifying a target directory that doesn't currently exist.
            %% So let's attempt to create this directory
            case filelib:ensure_dir(filename:join(DestDir, "dummy")) of
                ok ->
                    ok = xcopy_win32(SourceDir, DestDir);
                {error, Reason} ->
                    {error, lists:flatten(
                              io_lib:format("Unable to create dir ~p: ~p\n",
                                            [DestDir, Reason]))}
            end
    end;
cp_r_win32(Source,Dest) ->
    Dst = {filelib:is_dir(Dest), Dest},
    lists:foreach(fun(Src) ->
                          ok = cp_r_win32({filelib:is_dir(Src), Src}, Dst)
                  end, filelib:wildcard(Source)),
    ok.
