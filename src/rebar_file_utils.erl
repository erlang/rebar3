%% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
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

-export([rm_rf/1,
         cp_r/2,
         delete_each/1]).

-include("rebar.hrl").

%% ===================================================================
%% Public API
%% ===================================================================

%% @doc Remove files and directories.
%% Target is a single filename, directoryname or wildcard expression.
%% @spec rm_rf(string()) -> ok
-spec rm_rf(Target::string()) -> ok.
rm_rf(Target) ->
    case os:type() of
        {unix,_} ->
            [] = os:cmd(?FMT("rm -rf ~s", [Target])),
            ok;
        {win32,_} ->
            ok = rm_rf_win32(Target)
    end.

-spec cp_r(Sources::list(string()), Dest::string()) -> ok.
cp_r(Sources, Dest) ->
    case os:type() of
        {unix,_} ->
            SourceStr = string:join(Sources, " "),
            [] = os:cmd(?FMT("cp -R ~s ~s", [SourceStr, Dest])),
            ok;
        {win32,_} ->
            lists:foreach(fun(Src) -> ok = cp_r_win32(Src,Dest) end, Sources),
            ok
    end.

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

%% ===================================================================
%% Internal functions
%% ===================================================================

rm_rf_win32(Target) ->
    Filelist = filelib:wildcard(Target),
    Dirs = lists:filter(fun filelib:is_dir/1,Filelist),
    Files = lists:subtract(Filelist,Dirs),
    ok = delete_each(Files),
    ok = delete_each_dir_win32(Dirs),
    ok.

delete_each_dir_win32([]) -> ok;
delete_each_dir_win32([Dir | Rest]) ->
    [] = os:cmd(?FMT("rd /q /s ~s", [filename:nativename(Dir)])),
    delete_each_dir_win32(Rest).

xcopy_win32(Source,Dest)->
    R = os:cmd(?FMT("xcopy ~s ~s /q /y /e 2> nul",
                    [filename:nativename(Source), filename:nativename(Dest)])),
    case string:str(R,"\r\n") > 0 of
        %% when xcopy fails, stdout is empty and and error message is printed
        %% to stderr (which is redirected to nul)
        true -> ok;
        false ->
            {error, lists:flatten(
                      io_lib:format("Failed to xcopy from ~s to ~s\n",
                                     [Source, Dest]))}
    end.

cp_r_win32({true,SourceDir},{true,DestDir}) ->
    % from directory to directory
    SourceBase = filename:basename(SourceDir),
    ok = case file:make_dir(filename:join(DestDir,SourceBase)) of
             {error,eexist} -> ok;
             Other -> Other
         end,
    ok = xcopy_win32(SourceDir,filename:join(DestDir,SourceBase));
cp_r_win32({false,Source},{true,DestDir}) ->
    % from file to directory
    cp_r_win32({false,Source},
               {false,filename:join(DestDir,filename:basename(Source))});
cp_r_win32({false,Source},{false,Dest}) ->
    % from file to file
    {ok,_} = file:copy(Source,Dest),
    ok;
cp_r_win32(Source,Dest) ->
    Dst = {filelib:is_dir(Dest),Dest},
    lists:foreach(fun(Src) ->
                          ok = cp_r_win32({filelib:is_dir(Src),Src},Dst)
                  end, filelib:wildcard(Source)),
    ok.
