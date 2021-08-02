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
-module(rebar_prv_escriptize).

-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, escriptize).
-define(DEPS, [compile]).

-include("rebar.hrl").

-include_lib("providers/include/providers.hrl").
-include_lib("kernel/include/file.hrl").

%%%=============================================================================
%%% API
%%%=============================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
                                {name, ?PROVIDER},
                                {module, ?MODULE},
                                {bare, true},
                                {deps, ?DEPS},
                                {example, "rebar3 escriptize"},
                                {opts, opt_spec_list()},
                                {short_desc, "Generate escript archive."},
                                {desc, desc()}
                                ]),
    {ok, rebar_state:add_provider(State, Provider)}.

desc() ->
    "Generate an escript executable containing "
        "the project's and its dependencies' BEAM files.".

-spec opt_spec_list() -> [getopt:option_spec()].
opt_spec_list() ->
    [{main_app,  $a, "main-app",  string,
      "Specify the name of the application to build an escript for."}].

do(State) ->
    Providers = rebar_state:providers(State),
    Cwd = rebar_state:dir(State),
    {Opts, _} = rebar_state:command_parsed_args(State),
    ConfiguredMainApp = rebar_state:get(State, escript_main_app, undefined),
    AppInfo0 = case proplists:get_value(main_app, Opts, ConfiguredMainApp) of
        undefined ->
            case rebar_state:project_apps(State) of
                [AppInfo] ->
                    AppInfo;
                _ ->
                    ?PRV_ERROR(no_main_app)
            end;
        Name ->
            AllApps = rebar_state:all_deps(State)++rebar_state:project_apps(State),
            case rebar_app_utils:find(rebar_utils:to_binary(Name), AllApps) of
                {ok, AppInfo} ->
                    AppInfo;
                _ ->
                    ?PRV_ERROR({bad_name, Name})
            end
    end,
    case AppInfo0 of
        {error, _} = Err ->
            Err;
        _ ->
            AppInfo1 = rebar_hooks:run_all_hooks(Cwd, pre, ?PROVIDER, Providers, AppInfo0, State),
            ?INFO("Building escript for ~s...", [rebar_app_info:name(AppInfo0)]),
            Res = try
                escriptize(State, AppInfo1)
            catch
                throw:Err=?PRV_ERROR(_) -> Err
            end,
            rebar_hooks:run_all_hooks(Cwd, post, ?PROVIDER, Providers, AppInfo1, State),
            Res
    end.

escriptize(State0, App) ->
    AppName = rebar_app_info:name(App),
    AppNameStr = rebar_utils:to_list(AppName),

    %% Get the output filename for the escript -- this may include dirs
    Filename = filename:join([rebar_dir:base_dir(State0), "bin",
                              rebar_state:get(State0, escript_name, AppName)]),
    ?DEBUG("Creating escript file ~ts", [Filename]),
    ok = filelib:ensure_dir(Filename),
    State = rebar_state:escript_path(State0, Filename),

    %% Look for a list of other applications (dependencies) to include
    %% in the output file. We then use the .app files for each of these
    %% to pull in all the .beam files.
    TopInclApps = lists:usort([ec_cnv:to_atom(AppName) | rebar_state:get(State, escript_incl_apps, [])]),
    AllApps = rebar_state:all_deps(State)++rebar_state:project_apps(State),
    InclApps = find_deps(TopInclApps, AllApps, State),
    ?DEBUG("bundled applications:~n\t~p", [InclApps]),
    InclBeams = get_apps_beams(InclApps, AllApps),

    %% Look for a list of extra files to include in the output file.
    %% For internal rebar-private use only. Do not use outside rebar.
    InclExtra = get_extra(State),

    %% Construct the archive of everything in ebin/ dir -- put it on the
    %% top-level of the zip file so that code loading works properly.
    EbinPrefix = filename:join(AppNameStr, "ebin"),
    EbinFiles = usort(load_files(EbinPrefix, "*", "ebin")),

    ExtraFiles = usort(InclBeams ++ InclExtra),
    Files = get_nonempty(EbinFiles ++ (ExtraFiles -- EbinFiles)), % drop dupes

    DefaultEmuArgs = ?FMT("%%! -escript main ~ts -pz ~ts/~ts/ebin\n",
                          [AppNameStr, AppNameStr, AppNameStr]),
    EscriptSections =
        [ {shebang,
           def("#!", State, escript_shebang, "#!/usr/bin/env escript\n")}
        , {comment, def("%%", State, escript_comment, "%%\n")}
        , {emu_args, def("%%!", State, escript_emu_args, DefaultEmuArgs)}
        , {archive, Files, []} ],
    ?DEBUG("escript options:", []),
    [?DEBUG("\t{escript_~p, ~p}.", [K, V]) || {K, V} <- lists:droplast(EscriptSections)],
    case escript:create(Filename, EscriptSections) of
        ok -> ok;
        {error, EscriptError} ->
            throw(?PRV_ERROR({escript_creation_failed, AppName, EscriptError}))
    end,

    %% Finally, update executable perms for our script on *nix or write out
    %% script files on win32
    case os:type() of
        {unix, _} ->
            {ok, #file_info{mode = Mode}} = file:read_file_info(Filename),
            ok = file:change_mode(Filename, Mode bor 8#00111);
        {win32, _} ->
            write_windows_scripts(Filename, rebar_state:get(State, escript_wrappers_windows, ["cmd"]))
    end,
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error({write_failed, AppName, WriteError}) ->
    io_lib:format("Failed to write ~p script: ~p", [AppName, WriteError]);
format_error({zip_error, AppName, ZipError}) ->
    io_lib:format("Failed to construct ~p escript: ~p", [AppName, ZipError]);
format_error({bad_name, App}) ->
    io_lib:format("Failed to get ebin/ directory for "
                   "escript_incl_app: ~p", [App]);
format_error({bad_app, App}) ->
    io_lib:format("Failed to find application ~p", [App]);
format_error(no_main_app) ->
    io_lib:format("Multiple project apps and {escript_main_app, atom()}."
                 " not set in rebar.config", []).

%% ===================================================================
%% Internal functions
%% ===================================================================

get_apps_beams(Apps, AllApps) ->
    get_apps_beams(Apps, AllApps, []).

get_apps_beams([], _, Acc) ->
    Acc;
get_apps_beams([App | Rest], AllApps, Acc) ->
    case rebar_app_utils:find(rebar_utils:to_binary(App), AllApps) of
        {ok, App1} ->
            OutDir = filename:absname(rebar_app_info:ebin_dir(App1)),
            Beams = get_app_beams(App, OutDir),
            get_apps_beams(Rest, AllApps, Beams ++ Acc);
        _->
            case code:lib_dir(App, ebin) of
                {error, bad_name} ->
                    throw(?PRV_ERROR({bad_name, App}));
                Path ->
                    Beams = get_app_beams(App, Path),
                    get_apps_beams(Rest, AllApps, Beams ++ Acc)
            end
    end.

get_app_beams(App, Path) ->
    Prefix = filename:join(atom_to_list(App), "ebin"),
    load_files(Prefix, "*.beam", Path) ++
        load_files(Prefix, "*.app", Path).

get_extra(State) ->
    AllApps = rebar_state:all_deps(State) ++ rebar_state:project_apps(State),
    InclPriv = rebar_state:get(State, escript_incl_priv, []),
    InclPrivPaths = lists:map(fun(Entry) ->
                                      resolve_incl_priv(Entry, AllApps)
                              end, InclPriv),
    % `escript_incl_extra` is kept for historical reasons as its internal use in
    % rebar3 has been replaced with `escript_incl_priv`.
    InclExtraPaths = rebar_state:get(State, escript_incl_extra, []),
    lists:foldl(fun({Wildcard, Dir}, Files) ->
                        load_files(Wildcard, Dir) ++ Files
                end, [], InclPrivPaths ++ InclExtraPaths).

% Converts a wildcard path relative to an app (e.g., `priv/*`) into a wildcard
% path with with the app name included (e.g., `relx/priv/*`).
resolve_incl_priv({AppName, PrivWildcard}, AllApps) when is_atom(AppName) ->
    {ok, AppInfo} =
        rebar_app_utils:find(rebar_utils:to_binary(AppName), AllApps),
    AppOutDir = rebar_app_info:out_dir(AppInfo),
    Wildcard = filename:join([atom_to_list(AppName), "priv", PrivWildcard]),
    {Wildcard, filename:dirname(AppOutDir)}.

load_files(Wildcard, Dir) ->
    load_files("", Wildcard, Dir).

load_files(Prefix, Wildcard, Dir) ->
    [read_file(Prefix, Filename, Dir)
     || Filename <- filelib:wildcard(Wildcard, Dir),
        not filelib:is_dir(filename:join(Dir, Filename))].

read_file(Prefix, Filename, Dir) ->
    Filename1 = case Prefix of
                    "" ->
                        Filename;
                    _ ->
                        filename:join([Prefix, Filename])
                end,
    [dir_entries(filename:dirname(Filename1)),
     {Filename1, file_contents(filename:join(Dir, Filename))}].

file_contents(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    Bin.

%% Given a filename, return zip archive dir entries for each sub-dir.
%% Required to work around issues fixed in OTP-10071.
dir_entries(File) ->
    Dirs = dirs(File),
    [{Dir ++ "/", <<>>} || Dir <- Dirs].

%% Given "foo/bar/baz", return ["foo", "foo/bar", "foo/bar/baz"].
dirs(Dir) ->
    dirs1(filename:split(Dir), "", []).

dirs1([], _, Acc) ->
    lists:reverse(Acc);
dirs1([H|T], "", []) ->
    dirs1(T, H, [H]);
dirs1([H|T], Last, Acc) ->
    Dir = filename:join(Last, H),
    dirs1(T, Dir, [Dir|Acc]).

usort(List) ->
    lists:ukeysort(1, lists:flatten(List)).

get_nonempty(Files) ->
    [{FName,FBin} || {FName,FBin} <- Files, FBin =/= <<>>].

find_deps(AppNames, AllApps, State) ->
    BinAppNames = [rebar_utils:to_binary(Name) || Name <- AppNames],
    [ec_cnv:to_atom(Name) ||
     Name <- find_deps_of_deps(BinAppNames, AllApps, State, BinAppNames)].

%% Should look at the app files to find direct dependencies
find_deps_of_deps([], _, _, Acc) -> Acc;
find_deps_of_deps([Name|Names], Apps, State, Acc) ->
    ?DIAGNOSTIC("processing ~p", [Name]),
    App = case rebar_app_utils:find(Name, Apps) of
        {ok, Found} ->
            Found;
        error ->
            case find_external_app(Name, State) of
                error -> throw(?PRV_ERROR({bad_app, binary_to_atom(Name, utf8)}));
                App0 -> App0
            end
    end,
    DepNames = proplists:get_value(applications, rebar_app_info:app_details(App), []),
    BinDepNames = [rebar_utils:to_binary(Dep) || Dep <- DepNames,
                   %% ignore system libs; shouldn't include them.
                   DepDir <- [code:lib_dir(Dep)],
                   DepDir =:= {error, bad_name} orelse % those are all local
                   not lists:prefix(code:root_dir(), DepDir)]
                -- ([Name|Names]++Acc), % avoid already seen deps
    ?DIAGNOSTIC("new deps of ~p found to be ~p", [Name, BinDepNames]),
    find_deps_of_deps(BinDepNames ++ Names, Apps, State, BinDepNames ++ Acc).

%% This is for apps which are on the code path (ERL_LIBS) but not part of OTP
find_external_app(Name, State) ->
    case code:lib_dir(binary_to_atom(Name, utf8)) of
        {error, bad_name} ->
            error;
        Dir ->
            case rebar_app_discover:find_app(Dir, valid, State) of
                {true, App} -> App;
                false -> error
            end
    end.

def(Rm, State, Key, Default) ->
    Value0 = rebar_state:get(State, Key, Default),
    case Rm of
        "#!"  -> "#!"  ++ Value = Value0, rm_newline(Value);
        "%%"  -> "%%"  ++ Value = Value0, rm_newline(Value);
        "%%!" -> "%%!" ++ Value = Value0, rm_newline(Value)
    end.

rm_newline(String) ->
    [C || C <- String, C =/= $\n].

write_windows_scripts(Target, Wrappers) ->
    lists:foreach(fun(Wrapper) -> write_windows_script(Target, Wrapper) end, Wrappers).

write_windows_script(Target, "powershell") ->
    CmdPath = unicode:characters_to_list(Target) ++ ".ps1",
    CmdScript="& escript.exe \"$PSScriptRoot\\$((Get-Item $PSCommandPath).Basename)\" @args\r\n",
    ok = file:write_file(CmdPath, CmdScript);
write_windows_script(Target, _) ->
    CmdPath = unicode:characters_to_list(Target) ++ ".cmd",
    CmdScript=
        "@echo off\r\n"
        "escript.exe \"%~dpn0\" %*\r\n",
    ok = file:write_file(CmdPath, CmdScript).

