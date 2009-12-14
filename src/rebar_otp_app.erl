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
-module(rebar_otp_app).

-export([compile/2,
         install/2]).

-include("rebar.hrl").

%% ===================================================================
%% Public API
%% ===================================================================

compile(_Config, File) ->
    %% Load the app name and version from the .app file and construct
    %% the app identifier
    {ok, AppName, AppData} = rebar_app_utils:load_app_file(File),
    validate_name(AppName, File),
    validate_modules(AppName, proplists:get_value(modules, AppData)),
    ok.


install(Config, File) ->
    %% Load the app name and version from the .app file and construct
    %% the app identifier
    {ok, AppName, AppData} = rebar_app_utils:load_app_file(File),

    %% Validate the .app file prior to installation
    validate_name(AppName, File),
    validate_modules(AppName, proplists:get_value(modules, AppData)),

    %% Pull out the vsn and construct identifier
    Vsn = proplists:get_value(vsn, AppData),
    AppId = ?FMT("~s-~s", [AppName, Vsn]),
    ?CONSOLE("Installing: ~s\n", [AppId]),

    %% Check the erlang lib directory to see if this app identifier
    %% is already present.
    AppDir = filename:join([code:lib_dir(), AppId]),
    case filelib:is_dir(AppDir) of
        true ->
            %% Already exists -- check for force=1 global flag and only
            %% continue if it's set
            case rebar_config:get_global(force, "0") of
                "0" ->
                    ?ERROR("~s already exists. Installation failed.\n", [AppId]),
                    ?FAIL;
                "1" ->
                    ?WARN("~s already exists, but forcibly overwriting.\n", [AppId])
            end;
        false ->
            ok
    end,

    %% Wipe out any previous versions
    ok = rebar_file_utils:rm_rf(AppDir),

    %% Re-create target
    ok = rebar_file_utils:mkdir_p(AppDir),

    %% By default we copy the ebin, include, src and priv directories (if they exist)
    Files = [F || F <- ["ebin", "src", "priv", "include"],
                  filelib:last_modified(F) /= 0],
    ok = rebar_file_utils:cp_r(Files, AppDir),

    %% Check the config to see if we have any binaries that need to be
    %% linked into the erlang path
    case rebar_config:get_list(Config, app_bin, []) of
        [] ->
            ok;
        List ->
            %% code:root_dir() gives $OTPROOT/lib/erlang on a stock install
            %% so find the bin dir relative to that.
            BinDir = filename:join([code:root_dir(), "..", "..", "bin"]),
            install_binaries(List, AppDir, BinDir)
    end.
    

%% ===================================================================
%% Internal functions
%% ===================================================================

install_binaries([], _AppDir, _BinDir) ->
    ok;
install_binaries([Bin | Rest], AppDir, BinDir) ->
    FqBin = filename:join([AppDir, Bin]),
    rebar_file_utils:ln_sf(FqBin, BinDir),
    install_binaries(Rest, AppDir, BinDir).
    
 
validate_name(AppName, File) ->
    %% Convert the .app file name to an atom -- check it against the identifier within the file
    ExpApp = list_to_atom(filename:basename(File, ".app")),
    case ExpApp == AppName of
        true ->
            ok;
        false ->
            ?ERROR("Invalid ~s: name of application (~p) must match filename.\n", [File, AppName]),
            ?FAIL
    end.

validate_modules(AppName, undefined) ->
            ?ERROR("Missing modules declaration in~p.app:\n~s", [AppName]),
            ?FAIL;

validate_modules(AppName, Mods) ->
    %% Construct two sets -- one for the actual .beam files in ebin/ and one for the modules
    %% listed in the .app file
    EbinSet = ordsets:from_list([beam_to_mod(N) || N <- filelib:wildcard("ebin/*.beam")]),
    ModSet = ordsets:from_list(Mods),

    %% Identify .beam files listed in the .app, but not present in ebin/
    case ordsets:subtract(ModSet, EbinSet) of
        [] ->
            ok;
        MissingBeams ->
            Msg1 = lists:flatten([io_lib:format("\t* ~p\n", [M]) || M <- MissingBeams]),
            ?ERROR("One or more modules listed in ~p.app are not present in ebin/*.beam:\n~s", 
                   [AppName, Msg1]),
            ?FAIL
    end,

    %% Identify .beam files NOT list in the .app, but present in ebin/
    case ordsets:subtract(EbinSet, ModSet) of
        [] ->
            ok;
        MissingMods ->
            Msg2 = lists:flatten([io_lib:format("\t* ~p\n", [M]) || M <- MissingMods]),
            ?ERROR("On or more .beam files exist that are not listed in ~p.app:\n~s", 
                   [AppName, Msg2]),
            ?FAIL
    end.

beam_to_mod(Filename) ->
    list_to_atom(filename:basename(Filename, ".beam")).

