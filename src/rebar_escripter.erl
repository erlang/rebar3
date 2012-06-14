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
-module(rebar_escripter).

-export([escriptize/2,
         clean/2]).

-include("rebar.hrl").
-include_lib("kernel/include/file.hrl").

%% ===================================================================
%% Public API
%% ===================================================================

escriptize(Config, AppFile) ->
    %% Extract the application name from the archive -- this is the default
    %% name of the generated script
    AppName = rebar_app_utils:app_name(AppFile),

    %% Get the output filename for the escript -- this may include dirs
    Filename = rebar_config:get_local(Config, escript_name, AppName),
    ok = filelib:ensure_dir(Filename),

    AppNameString = atom_to_list(AppName),
    TempDir = make_temp_dir(AppNameString),
    ok = copy_files(Config, AppNameString, TempDir),
    {ok, Dirs} = file:list_dir(TempDir),

    case zip:create("mem", Dirs, [memory, {cwd, TempDir}]) of
        {ok, {"mem", ZipBin}} ->
            ok = rebar_file_utils:rm_rf(TempDir),
            %% Archive was successfully created. Prefix that binary with our
            %% header and write to our escript file
            Shebang = rebar_config:get(Config, escript_shebang,
                                       "#!/usr/bin/env escript\n"),
            Comment = rebar_config:get(Config, escript_comment, "%%\n"),
            EmuArgs = rebar_config:get(Config, escript_emu_args, "%%!\n"),
            Script = iolist_to_binary([Shebang, Comment, EmuArgs, ZipBin]),
            case file:write_file(Filename, Script) of
                ok ->
                    ok;
                {error, WriteError} ->
                    ?ERROR("Failed to write ~p script: ~p\n",
                           [AppName, WriteError]),
                    ?ABORT
            end;
        {error, ZipError} ->
            ok = rebar_file_utils:rm_rf(TempDir),
            ?ERROR("Failed to construct ~p escript: ~p\n",
                   [AppName, ZipError]),
            ?ABORT
    end,

    %% Finally, update executable perms for our script
    {ok, #file_info{mode = Mode}} = file:read_file_info(Filename),
    ok = file:change_mode(Filename, Mode bor 8#00100),
    ok.

clean(Config, AppFile) ->
    %% Extract the application name from the archive -- this is the default
    %% name of the generated script
    AppName = rebar_app_utils:app_name(AppFile),

    %% Get the output filename for the escript -- this may include dirs
    Filename = rebar_config:get_local(Config, escript_name, AppName),
    rebar_file_utils:delete_each([Filename]).


%% ===================================================================
%% Internal functions
%% ===================================================================

make_temp_dir(AppName) ->
    TempDir = temp_name(AppName ++ "."),
    case file:make_dir(TempDir) of
        ok ->
            ?CONSOLE("TempDir: ~p~n", [TempDir]),
            TempDir;
        Error ->
            ?ABORT("Failed to create temporary directory: ~p~n",
                   [Error])
    end.

temp_name(Prefix) ->
    Hash = erlang:phash2(make_ref()),
    Prefix ++ integer_to_list(Hash).

copy_files(Config, AppName, Temp) ->
    BaseEbinDir = filename:join(AppName, "ebin"),
    EbinDir = filename:join(Temp, BaseEbinDir),

    %% Look for a list of other applications (dependencies) to include
    %% in the output file. We then use the .app files for each of
    %% these to pull in all the .beam files.
    InclApps = rebar_config:get_local(Config, escript_incl_apps, []),
    InclEbinDirs = get_app_ebin_dirs(InclApps, []),
    %% copy incl_apps files
    lists:foreach(fun(Src) -> ok = copy_files(Src, EbinDir) end, InclEbinDirs),

    %% Look for a list of extra files to copy
    InclExtr = rebar_config:get_local(Config, escript_incl_extra, []),
    lists:foreach(fun({Src, Dst}) ->
                          copy_files(Src, filename:join(Temp, Dst))
                  end, InclExtr),

    %% copy script's beam files and app file
    EbinSrc = filename:join(["ebin", "*"]),
    ok = copy_files(EbinSrc, EbinDir).

copy_files(Src, Dst) ->
    ok = filelib:ensure_dir(filename:join(Dst, "dummy")),
    rebar_file_utils:cp_r([Src], Dst).

get_app_ebin_dirs([], Acc) ->
    Acc;
get_app_ebin_dirs([App | Rest], Acc) ->
    case code:lib_dir(App, ebin) of
        {error, bad_name} ->
            ?ABORT("Failed to get ebin/ directory for "
                   "~p escript_incl_apps.", [App]);
        Path ->
            %% TODO: shouldn't we also include .app files? escript
            %% should support multiple app files in one ebin/
            Acc2 = filename:join(Path, "*.beam"),
            get_app_ebin_dirs(Rest, [Acc2|Acc])
    end.
