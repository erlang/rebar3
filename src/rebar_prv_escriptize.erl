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
                                {opts, []},
                                {short_desc, "Generate escript archive."},
                                {desc, desc()}
                                ]),
    {ok, rebar_state:add_provider(State, Provider)}.

desc() ->
    "Generate an escript executable containing "
        "the project's and its dependencies' BEAM files.".

do(State) ->
    ?INFO("Building escript...", []),
    case rebar_state:get(State, escript_main_app, undefined) of
        undefined ->
            case rebar_state:project_apps(State) of
                [App] ->
                    escriptize(State, App);
                _ ->
                    ?PRV_ERROR(no_main_app)
            end;
        Name ->
            AllApps = rebar_state:all_deps(State)++rebar_state:project_apps(State),
            {ok, AppInfo} = rebar_app_utils:find(ec_cnv:to_binary(Name), AllApps),
            escriptize(State, AppInfo)
    end.

escriptize(State0, App) ->
    AppName = rebar_app_info:name(App),
    AppNameStr = ec_cnv:to_list(AppName),

    %% Get the output filename for the escript -- this may include dirs
    Filename = filename:join([rebar_dir:base_dir(State0), "bin",
                              rebar_state:get(State0, escript_name, AppName)]),
    ?DEBUG("Creating escript file ~s", [Filename]),
    ok = filelib:ensure_dir(Filename),
    State = rebar_state:escript_path(State0, Filename),

    %% Look for a list of other applications (dependencies) to include
    %% in the output file. We then use the .app files for each of these
    %% to pull in all the .beam files.
    InclApps = lists:usort([ec_cnv:to_atom(AppName) | rebar_state:get(State, escript_incl_apps, [])
                           ++ all_deps(State)]),
    AllApps = rebar_state:all_deps(State)++rebar_state:project_apps(State),
    InclBeams = get_apps_beams(InclApps, AllApps),

    %% Look for a list of extra files to include in the output file.
    %% For internal rebar-private use only. Do not use outside rebar.
    InclExtra = get_extra(State),

    %% Construct the archive of everything in ebin/ dir -- put it on the
    %% top-level of the zip file so that code loading works properly.
    EbinPrefix = filename:join(AppNameStr, "ebin"),
    EbinFiles = usort(load_files(EbinPrefix, "*", "ebin")),

    ExtraFiles = usort(InclBeams ++ InclExtra),
    Files = get_nonempty(EbinFiles ++ ExtraFiles),

    DefaultEmuArgs = ?FMT("%%! -escript main ~s -pz ~s/~s/ebin\n",
                          [AppNameStr, AppNameStr, AppNameStr]),
    EscriptSections =
        [ {shebang,
           def("#!", State, escript_shebang, "#!/usr/bin/env escript\n")}
        , {comment, def("%%", State, escript_comment, "%%\n")}
        , {emu_args, def("%%!", State, escript_emu_args, DefaultEmuArgs)}
        , {archive, Files, []} ],
    case escript:create(Filename, EscriptSections) of
        ok -> ok;
        {error, EscriptError} ->
            throw(?PRV_ERROR({escript_creation_failed, AppName, EscriptError}))
    end,

    %% Finally, update executable perms for our script
    {ok, #file_info{mode = Mode}} = file:read_file_info(Filename),
    ok = file:change_mode(Filename, Mode bor 8#00111),
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error({write_failed, AppName, WriteError}) ->
    io_lib:format("Failed to write ~p script: ~p", [AppName, WriteError]);
format_error({zip_error, AppName, ZipError}) ->
    io_lib:format("Failed to construct ~p escript: ~p", [AppName, ZipError]);
format_error({bad_name, App}) ->
    io_lib:format("Failed to get ebin/ directory for "
                   "escript_incl_app: ~p", [App]);
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
    case rebar_app_utils:find(ec_cnv:to_binary(App), AllApps) of
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
    Extra = rebar_state:get(State, escript_incl_extra, []),
    lists:foldl(
        fun
            ({Prefix, Wildcard, Dir}, Files) ->
                load_files(Prefix, Wildcard, Dir) ++ Files
        end, [], Extra).

load_files(Prefix, Wildcard, Dir) ->
    [read_file(Prefix, Filename, Dir)
     || Filename <- filelib:wildcard(Wildcard, Dir) ].

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

all_deps(State) ->
    [list_to_existing_atom(binary_to_list(rebar_app_info:name(App)))
     || App <- rebar_state:all_deps(State)].

def(Rm, State, Key, Default) ->
    Value0 = rebar_state:get(State, Key, Default),
    case Rm of
        "#!"  -> "#!"  ++ Value = Value0, rm_newline(Value);
        "%%"  -> "%%"  ++ Value = Value0, rm_newline(Value);
        "%%!" -> "%%!" ++ Value = Value0, rm_newline(Value)
    end.

rm_newline(String) ->
    [C || C <- String, C =/= $\n].
