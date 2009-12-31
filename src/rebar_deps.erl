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

-export([preprocess/2]).

%% ===================================================================
%% Public API
%% ===================================================================

preprocess(Config, _) ->
    %% Get the directory where we will place downloaded deps
    DepsDir = rebar_config:get(Config, deps_dir, "deps"),

    %% Process the list of deps from the configuration
    case catch(process_deps(rebar_config:get(Config, deps, []), [], DepsDir)) of
        Dirs when is_list(Dirs) ->
            %% Filter out deps from config so sub-dirs don't wind up trying to d/l deps again
            Config2 = rebar_config:delete(Config, deps),
            {ok, Config2, Dirs};
        {'EXIT', Reason} ->
            ?ABORT("Error while processing dependencies: ~p\n", [Reason])
    end.


%% ===================================================================
%% Internal functions
%% ===================================================================

process_deps([], Acc, _Dir) ->
    Acc;
process_deps([App | Rest], Acc, Dir) when is_atom(App) ->
    require_app(App, ".*"),
    process_deps(Rest, Acc, Dir);
process_deps([{App, VsnRegex} | Rest], Acc, Dir) when is_atom(App) ->
    require_app(App, VsnRegex),
    process_deps(Rest, Acc, Dir);
process_deps([{App, VsnRegex, Source} | Rest], Acc, Dir) ->
    ?DEBUG("Process deps: ~p\n", [Rest]),
    case is_app_available(App, VsnRegex) of
        true ->
            process_deps(Rest, Acc, Dir);
        false ->
            %% App may not be on the code path, or the version that is doesn't
            %% match our regex. Either way, we want to pull our revision into
            %% the deps dir and try to use that
            require_source_engine(Source),
            AppDir = filename:join(Dir, App),
            use_source(AppDir, App, VsnRegex, Source),
            process_deps(Rest, [AppDir | Acc], Dir)
    end;
process_deps([Other | _Rest], _Acc, _Dir) ->
    ?ABORT("Invalid dependency specification ~p in ~s\n",
           [Other, rebar_utils:get_cwd()]).



require_app(App, VsnRegex) ->
    case is_app_available(App, VsnRegex) of
        true ->
            ok;
        false ->
            %% The requested app is not available on the code path
            ?ABORT("~s: Dependency ~s-~s not available.\n",
                   [rebar_utils:get_cwd(), App, VsnRegex])
    end.

require_source_engine(Source) ->
    case source_engine_avail(Source) of
        true ->
            ok;
        false ->
            ?ABORT("No command line interface available to process ~p\n", [Source])
    end.


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
            case rebar_app_utils:load_app_file(AppFile) of
                {ok, App, AppData} ->
                    {vsn, Vsn} = lists:keyfind(vsn, 1, AppData),
                    ?INFO("Looking for ~s-~s ; found ~s-~s at ~s\n",
                          [App, VsnRegex, App, Vsn, Path]),
                    case re:run(Vsn, VsnRegex, [{capture, none}]) of
                        match ->
                            true;
                        nomatch ->
                            ?WARN("~s has version ~p; requested regex was ~s\n",
                                  [AppFile, Vsn, VsnRegex]),
                            false
                    end;
                {ok, OtherApp, _} ->
                    ?WARN("~s has application id ~p; expected ~p\n", [AppFile, OtherApp, App]),
                    false;
                {error, Reason} ->
                    ?ABORT("Failed to parse ~s: ~p\n", [AppFile, Reason])
            end;
        false ->
            ?WARN("Expected ~s to be an app dir (containing ebin/*.app), but no .app found.\n",
                  [Path]),
            false
    end.

use_source(AppDir, App, VsnRegex, Source) ->
    use_source(AppDir, App, VsnRegex, Source, 3).

use_source(_AppDir, _App, _VsnRegex, Source, 0) ->
    ?ABORT("Failed to acquire source from ~p after 3 tries.\n", [Source]);
use_source(AppDir, App, VsnRegex, Source, Count) ->
    case filelib:is_dir(AppDir) of
        true ->
            %% Already downloaded -- verify the versioning matches up with our regex
            case is_app_available(App, VsnRegex, AppDir) of
                true ->
                    %% Available version matches up -- we're good to go; add the
                    %% app dir to our code path
                    code:add_patha(filename:join(AppDir, ebin)),
                    ok;
                false ->
                    %% The app that was downloaded doesn't match up (or had
                    %% errors or something). For the time being, abort.
                    ?ABORT("Dependency dir ~s does not satisfy version regex ~s.\n",
                           [AppDir, VsnRegex])
            end;
        false ->
            download_source(AppDir, Source),
            use_source(AppDir, App, VsnRegex, Source, Count-1)
    end.

download_source(AppDir, {hg, Url, Rev}) ->
    ok = filelib:ensure_dir(AppDir),
    Cmd = ?FMT("hg clone -u ~s ~s", [Rev, Url]),
    rebar_utils:sh(Cmd, [], filename:dirname(AppDir)).


%% ===================================================================
%% Source helper functions
%% ===================================================================

source_engine_avail({hg, _, _}) ->
    Res = os:cmd("which hg"),
    ?DEBUG("which hg = ~p\n", [Res]),
    case Res of
        [] ->
            false;
        _ ->
            true
    end;
source_engine_avail(_) ->
    false.
