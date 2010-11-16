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
-module(rebar_deps).

-include("rebar.hrl").

-export([preprocess/2,
         postprocess/2,
         compile/2,
         'check-deps'/2,
         'get-deps'/2,
         'delete-deps'/2]).


-record(dep, { dir,
               app,
               vsn_regex,
               source }).

%% ===================================================================
%% Public API
%% ===================================================================

preprocess(Config, _) ->
    %% Side effect to set deps_dir globally for all dependencies from top level down.
    %% Means the root deps_dir is honoured or the default used globally
    %% since it will be set on the first time through here
    set_global_deps_dir(Config, rebar_config:get_global(deps_dir, [])),

    %% Get the list of deps for the current working directory and identify those
    %% deps that are available/present.
    Deps = rebar_config:get_local(Config, deps, []),
    {AvailableDeps, MissingDeps} = find_deps(Deps),

    ?DEBUG("Available deps: ~p\n", [AvailableDeps]),
    ?DEBUG("Missing deps  : ~p\n", [MissingDeps]),

    %% Add available deps to code path
    update_deps_code_path(AvailableDeps),

    %% If skip_deps=true, mark each dep dir as a skip_dir w/ the core so that
    %% the current command doesn't run on the dep dir. However, pre/postprocess
    %% WILL run (and we want it to) for transitivity purposes.
    case rebar_config:get_global(skip_deps, false) of
        "true" ->
            lists:foreach(fun (#dep{dir = Dir}) ->
				  rebar_core:skip_dir(Dir)
			  end, AvailableDeps);
        _ ->
            ok
    end,

    %% Return all the available dep directories for process
    {ok, [D#dep.dir || D <- AvailableDeps]}.


postprocess(_Config, _) ->
    case erlang:get(?MODULE) of
        undefined ->
            {ok, []};
        Dirs ->
            erlang:erase(?MODULE),
            {ok, Dirs}
    end.

compile(Config, AppFile) ->
    'check-deps'(Config, AppFile).

'check-deps'(Config, _) ->
    %% Get the list of immediate (i.e. non-transitive) deps that are missing
    Deps = rebar_config:get_local(Config, deps, []),
    case find_deps(Deps) of
        {_, []} ->
            %% No missing deps
            ok;
        {_, MissingDeps} ->
            lists:foreach(fun (#dep{app=App, vsn_regex=Vsn, source=Src}) ->
			    ?CONSOLE("Dependency not available: ~p-~s (~p)\n",
				     [App, Vsn, Src])
			  end, MissingDeps),
            ?FAIL
    end.

'get-deps'(Config, _) ->
    %% Determine what deps are available and missing
    Deps = rebar_config:get_local(Config, deps, []),
    {AvailableDeps, MissingDeps} = find_deps(Deps),

    %% For each missing dep with a specified source, try to pull it.
    PulledDeps0 = [use_source(D) || D <- MissingDeps, D#dep.source /= undefined],

    %% For each available dep try to update the source to the specified
    %% version.
    PulledDeps1 = [update_source(D) || D <- AvailableDeps,
                                       D#dep.source /= undefined],

    %% Add each pulled dep to our list of dirs for post-processing. This yields
    %% the necessary transitivity of the deps
    erlang:put(?MODULE, [D#dep.dir || D <- PulledDeps0 ++ PulledDeps1]),
    ok.

'delete-deps'(Config, _) ->
    %% Delete all the available deps in our deps/ directory, if any
    DepsDir = get_deps_dir(),
    Deps = rebar_config:get_local(Config, deps, []),
    {AvailableDeps, _} = find_deps(Deps),
    _ = [delete_dep(D) || D <- AvailableDeps,
			  lists:prefix(DepsDir, D#dep.dir) == true],
    ok.


%% ===================================================================
%% Internal functions
%% ===================================================================

%% Added because of trans deps,
%% need all deps in same dir and should be the one set by the root rebar.config
%% Sets a default if root config has no deps_dir set
set_global_deps_dir(Config, []) ->
    rebar_config:set_global(deps_dir, rebar_config:get_local(Config, deps_dir, "deps"));
set_global_deps_dir(_Config, _DepsDir) ->
    ok.

get_deps_dir() ->
    BaseDir = rebar_config:get_global(base_dir, []),
    DepsDir = rebar_config:get_global(deps_dir, "deps"),
    filename:join(BaseDir, DepsDir).

update_deps_code_path([]) ->
    ok;
update_deps_code_path([Dep | Rest]) ->
    case is_app_available(Dep#dep.app, Dep#dep.vsn_regex, Dep#dep.dir) of
        {true, _} ->
            Dir = filename:join(Dep#dep.dir, "ebin"),
            ok = filelib:ensure_dir(filename:join(Dir, "dummy")),
            true = code:add_patha(Dir);
        false ->
            true
    end,
    update_deps_code_path(Rest).

find_deps(Deps) ->
    find_deps(Deps, {[], []}).

find_deps([], {Avail, Missing}) ->
    {lists:reverse(Avail), lists:reverse(Missing)};
find_deps([App | Rest], Acc) when is_atom(App) ->
    find_deps([{App, ".*", undefined} | Rest], Acc);
find_deps([{App, VsnRegex} | Rest], Acc) when is_atom(App) ->
    find_deps([{App, VsnRegex, undefined} | Rest], Acc);
find_deps([{App, VsnRegex, Source} | Rest], {Avail, Missing}) ->
    Dep = #dep { app = App,
                 vsn_regex = VsnRegex,
                 source = Source },
    case is_app_available(App, VsnRegex) of
        {true, AppDir} ->
            find_deps(Rest, {[Dep#dep { dir = AppDir } | Avail], Missing});
        false ->
            AppDir = filename:join(get_deps_dir(), Dep#dep.app),
            case is_app_available(App, VsnRegex, AppDir) of
                {true, AppDir} ->
                    find_deps(Rest, {[Dep#dep { dir = AppDir } | Avail], Missing});
                false ->
                    find_deps(Rest, {Avail, [Dep#dep { dir = AppDir } | Missing]})
            end
    end;
find_deps([Other | _Rest], _Acc) ->
    ?ABORT("Invalid dependency specification ~p in ~s\n",
           [Other, rebar_utils:get_cwd()]).


delete_dep(D) ->
    case filelib:is_dir(D#dep.dir) of
        true ->
            ?INFO("Deleting dependency: ~s\n", [D#dep.dir]),
            rebar_file_utils:rm_rf(D#dep.dir);
        false ->
            ok
    end.

require_source_engine(Source) ->
    true = source_engine_avail(Source),
    ok.

is_app_available(App, VsnRegex) ->
    case code:lib_dir(App) of
        {error, bad_name} ->
            false;
        Path ->
            is_app_available(App, VsnRegex, Path)
    end.

is_app_available(App, VsnRegex, Path) ->
    case rebar_app_utils:is_app_dir(Path) of
        {true, AppFile} ->
            case rebar_app_utils:app_name(AppFile) of
                App ->
                    Vsn = rebar_app_utils:app_vsn(AppFile),
                    ?INFO("Looking for ~s-~s ; found ~s-~s at ~s\n",
                          [App, VsnRegex, App, Vsn, Path]),
                    case re:run(Vsn, VsnRegex, [{capture, none}]) of
                        match ->
                            {true, Path};
                        nomatch ->
                            ?WARN("~s has version ~p; requested regex was ~s\n",
                                  [AppFile, Vsn, VsnRegex]),
                            false
                    end;
                OtherApp ->
                    ?WARN("~s has application id ~p; expected ~p\n", [AppFile, OtherApp, App]),
                    false
            end;
        false ->
            ?WARN("Expected ~s to be an app dir (containing ebin/*.app), but no .app found.\n",
                  [Path]),
            false
    end.

use_source(Dep) ->
    use_source(Dep, 3).

use_source(Dep, 0) ->
    ?ABORT("Failed to acquire source from ~p after 3 tries.\n", [Dep#dep.source]);
use_source(Dep, Count) ->
    case filelib:is_dir(Dep#dep.dir) of
        true ->
            %% Already downloaded -- verify the versioning matches up with our regex
            case is_app_available(Dep#dep.app, Dep#dep.vsn_regex, Dep#dep.dir) of
                {true, _} ->
                    Dir = filename:join(Dep#dep.dir, "ebin"),
                    ok = filelib:ensure_dir(filename:join(Dir, "dummy")),
                    %% Available version matches up -- we're good to go;
                    %% add the app dir to our code path
                    true = code:add_patha(Dir),
                    Dep;
                false ->
                    %% The app that was downloaded doesn't match up (or had
                    %% errors or something). For the time being, abort.
                    ?ABORT("Dependency dir ~s does not satisfy version regex ~s.\n",
                           [Dep#dep.dir, Dep#dep.vsn_regex])
            end;
        false ->
            ?CONSOLE("Pulling ~p from ~p\n", [Dep#dep.app, Dep#dep.source]),
            require_source_engine(Dep#dep.source),
            TargetDir = filename:join(get_deps_dir(), Dep#dep.app),
            download_source(TargetDir, Dep#dep.source),
            use_source(Dep#dep { dir = TargetDir }, Count-1)
    end.

download_source(AppDir, {hg, Url, Rev}) ->
    ok = filelib:ensure_dir(AppDir),
    rebar_utils:sh(?FMT("hg clone -U ~s ~s", [Url, filename:basename(AppDir)]), [], filename:dirname(AppDir)),
    rebar_utils:sh(?FMT("hg update ~s", [Rev]), [], AppDir);
download_source(AppDir, {git, Url, {branch, Branch}}) ->
    ok = filelib:ensure_dir(AppDir),
    rebar_utils:sh(?FMT("git clone -n ~s ~s", [Url, filename:basename(AppDir)]), [], filename:dirname(AppDir)),
    rebar_utils:sh(?FMT("git checkout -q origin/~s", [Branch]), [], AppDir);
download_source(AppDir, {git, Url, {tag, Tag}}) ->
    ok = filelib:ensure_dir(AppDir),
    rebar_utils:sh(?FMT("git clone -n ~s ~s", [Url, filename:basename(AppDir)]), [], filename:dirname(AppDir)),
    rebar_utils:sh(?FMT("git checkout -q ~s", [Tag]), [], AppDir);
download_source(AppDir, {git, Url, Rev}) ->
    download_source(AppDir, {git, Url, {branch, Rev}});
download_source(AppDir, {bzr, Url, Rev}) ->
    ok = filelib:ensure_dir(AppDir),
    rebar_utils:sh(?FMT("bzr branch -r ~s ~s ~s",
                        [Rev, Url, filename:basename(AppDir)]), [],
                   filename:dirname(AppDir));
download_source(AppDir, {svn, Url, Rev}) ->
    ok = filelib:ensure_dir(AppDir),
    rebar_utils:sh(?FMT("svn checkout -r ~s ~s ~s",
                        [Rev, Url, filename:basename(AppDir)]), [],
                   filename:dirname(AppDir)).

update_source(Dep) ->
    ?CONSOLE("Updating ~p from ~p\n", [Dep#dep.app, Dep#dep.source]),
    require_source_engine(Dep#dep.source),
    update_source(filename:join(get_deps_dir(), Dep#dep.app),
                  Dep#dep.source),
    Dep.

update_source(AppDir, {git, _Url, {branch, Branch}}) ->
    rebar_utils:sh(?FMT("git fetch origin", []), [], AppDir),
    rebar_utils:sh(?FMT("git checkout -q origin/~s", [Branch]), [], AppDir);
update_source(AppDir, {git, _Url, {tag, Tag}}) ->
    rebar_utils:sh(?FMT("git fetch --tags origin", []), [], AppDir),
    rebar_utils:sh(?FMT("git checkout -q ~s", [Tag]), [], AppDir);
update_source(AppDir, {git, Url, Refspec}) ->
    update_source(AppDir, {git, Url, {branch, Refspec}});
update_source(AppDir, {svn, _Url, Rev}) ->
    rebar_utils:sh(?FMT("svn up -r ~s", [Rev]), [], AppDir);
update_source(AppDir, {hg, _Url, Rev}) ->
    rebar_utils:sh(?FMT("hg pull -u -r ~s", [Rev]), [], AppDir);
update_source(AppDir, {bzr, _Url, Rev}) ->
    rebar_utils:sh(?FMT("bzr update -r ~s", [Rev]), [], AppDir).



%% ===================================================================
%% Source helper functions
%% ===================================================================

source_engine_avail({Name, _, _}=Source)
  when Name == hg; Name == git; Name == svn; Name == bzr ->
    case scm_client_vsn(Name) >= required_scm_client_vsn(Name) of
        true ->
            true;
        false ->
            ?ABORT("Rebar requires version ~p or higher of ~s to process ~p\n",
                   [required_scm_client_vsn(Name), Name, Source])
    end.

scm_client_vsn(false, _VsnArg, _VsnRegex) ->
    false;
scm_client_vsn(Path, VsnArg, VsnRegex) ->
    Info = os:cmd("LANG=C " ++ Path ++ VsnArg),
    case re:run(Info, VsnRegex, [{capture, all_but_first, list}]) of
        {match, Match} ->
            list_to_tuple([list_to_integer(S) || S <- Match]);
        _ ->
            false
    end.

required_scm_client_vsn(hg)  -> {1, 1};
required_scm_client_vsn(git) -> {1, 5};
required_scm_client_vsn(bzr) -> {2, 0};
required_scm_client_vsn(svn) -> {1, 6}.

scm_client_vsn(hg) ->
    scm_client_vsn(rebar_utils:find_executable("hg"), " --version", "version (\\d+).(\\d+)");
scm_client_vsn(git) ->
    scm_client_vsn(rebar_utils:find_executable("git"), " --version", "git version (\\d+).(\\d+)");
scm_client_vsn(bzr) ->
    scm_client_vsn(rebar_utils:find_executable("bzr"), " --version", "Bazaar \\(bzr\\) (\\d+).(\\d+)");
scm_client_vsn(svn) ->
    scm_client_vsn(rebar_utils:find_executable("svn"), " --version", "svn, version (\\d+).(\\d+)").
