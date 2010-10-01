%% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2009, 2010 Dave Smith (dizzyd@dizzyd.com)
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
-module(rebar_utils).

-export([get_cwd/0,
         is_arch/1,
         get_arch/0,
         get_os/0,
         sh/2, sh/3,
         sh_failfast/2,
         find_files/2,
         now_str/0,
         ensure_dir/1,
         beam_to_mod/2, beams/1,
         erl_to_mod/1,
         abort/2,
         escript_foldl/3,
         find_executable/1]).

-include("rebar.hrl").

%% ====================================================================
%% Public API
%% ====================================================================

get_cwd() ->
    {ok, Dir} = file:get_cwd(),
    Dir.


is_arch(ArchRegex) ->
    case re:run(get_arch(), ArchRegex, [{capture, none}]) of
        match ->
            true;
        nomatch ->
            false
    end.

get_arch() ->
    Words = integer_to_list(8 * erlang:system_info(wordsize)),
    erlang:system_info(system_architecture) ++ "-" ++ Words.

get_os() ->
    Arch = erlang:system_info(system_architecture),
    case match_first([{"linux", linux}, {"darwin", darwin}], Arch) of
        nomatch ->
            {unknown, Arch};
        ArchAtom ->
            ArchAtom
    end.


sh(Command, Env) ->
    sh(Command, Env, get_cwd()).

sh(Command0, Env, Dir) ->
    ?INFO("sh: ~s\n~p\n", [Command0, Env]),
    Command = patch_on_windows(Command0, os:type()),
    Port = open_port({spawn, Command}, [{cd, Dir}, {env, Env}, exit_status, {line, 16384},
                                        use_stdio, stderr_to_stdout]),
    case sh_loop(Port) of
        ok ->
            ok;
        {error, Rc} ->
            ?ABORT("~s failed with error: ~w\n", [Command, Rc])
    end.


%% We need a bash shell to execute on windows
%% also the port doesn't seem to close from time to time (mingw)
patch_on_windows(Cmd, {win32,nt}) ->
    case os:find_executable("bash") of
        false -> Cmd;
        Bash -> 
            Bash ++ " -c \"" ++ Cmd ++ "; echo _port_cmd_status_ $?\" "
    end;
patch_on_windows(Command, _) ->
    Command.

sh_failfast(Command, Env) ->
    sh(Command, Env).

find_files(Dir, Regex) ->
    filelib:fold_files(Dir, Regex, true, fun(F, Acc) -> [F | Acc] end, []).

now_str() ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:local_time(),
    lists:flatten(io_lib:format("~4b/~2..0b/~2..0b ~2..0b:~2..0b:~2..0b",
				[Year, Month, Day, Hour, Minute, Second])).

%% TODO: filelib:ensure_dir/1 corrected in R13B04. Can be removed.
ensure_dir(Path) ->
    case filelib:ensure_dir(Path) of
        ok ->
            ok;
        {error,eexist} ->
            ok;
        Error ->
            Error
    end.

abort(String, Args) ->
    ?ERROR(String, Args),
    halt(1).

%% TODO: Rename emulate_escript_foldl to escript_foldl and remove
%% this function when the time is right. escript:foldl/3 was an
%% undocumented exported fun and has been removed in R14.
escript_foldl(Fun, Acc, File) ->
    {module, zip} = code:ensure_loaded(zip),
    case erlang:function_exported(zip, foldl, 3) of
        true ->
            emulate_escript_foldl(Fun, Acc, File);
        false ->
            escript:foldl(Fun, Acc, File)
    end.

find_executable(Name) ->
    case os:find_executable(Name) of
        false -> false;
        Path ->
            "\"" ++ filename:nativename(Path) ++ "\""
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

match_first([], _Val) ->
    nomatch;
match_first([{Regex, MatchValue} | Rest], Val) ->
    case re:run(Val, Regex, [{capture, none}]) of
        match ->
            MatchValue;
        nomatch ->
           match_first(Rest, Val)
    end.

sh_loop(Port) ->
    receive
        {Port, {data, {_, "_port_cmd_status_ " ++ Status}}} ->
            (catch erlang:port_close(Port)), % sigh () for indentation
            case list_to_integer(Status) of
                0  -> ok;
                Rc -> {error, Rc}
            end;
        {Port, {data, {_, Line}}} ->
            ?CONSOLE("~s\n", [Line]),
            sh_loop(Port);
        {Port, {exit_status, 0}} ->
            ok;
        {Port, {exit_status, Rc}} ->
            {error, Rc}
    end.

beam_to_mod(Dir, Filename) ->
    [Dir | Rest] = filename:split(Filename),
    list_to_atom(filename:basename(string:join(Rest, "."), ".beam")).

erl_to_mod(Filename) ->
    list_to_atom(filename:rootname(filename:basename(Filename))).

beams(Dir) ->
    filelib:fold_files(Dir, ".*\.beam\$", true,
                       fun(F, Acc) -> [F | Acc] end, []).

emulate_escript_foldl(Fun, Acc, File) ->
    case escript:extract(File, [compile_source]) of
        {ok, [_Shebang, _Comment, _EmuArgs, Body]} ->
            case Body of
                {source, BeamCode} ->
                    GetInfo = fun() -> file:read_file_info(File) end,
                    GetBin = fun() -> BeamCode end,
                    {ok, Fun(".", GetInfo, GetBin, Acc)};
                {beam, BeamCode} ->
                    GetInfo = fun() -> file:read_file_info(File) end,
                    GetBin = fun() -> BeamCode end,
                    {ok, Fun(".", GetInfo, GetBin, Acc)};
                {archive, ArchiveBin} ->
                    zip:foldl(Fun, Acc, {File, ArchiveBin})
            end;
        {error, Reason} ->
            {error, Reason}
    end.
