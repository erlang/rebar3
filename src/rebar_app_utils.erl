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
-module(rebar_app_utils).

-export([find/2,
         find/3,
         is_app_src/1,
         app_src_to_app/2,
         validate_application_info/1,
         validate_application_info/2,
         parse_deps/5,
         parse_deps/6,
         expand_deps_sources/2,
         dep_to_app/7,
         format_error/1]).

-include("rebar.hrl").
-include_lib("providers/include/providers.hrl").

%% ===================================================================
%% Public API
%% ===================================================================

%% @doc finds the proper app info record for a given app name in a list of
%% such records.
-spec find(binary(), [rebar_app_info:t()]) -> {ok, rebar_app_info:t()} | error.
find(Name, Apps) ->
    ec_lists:find(fun(App) -> rebar_app_info:name(App) =:= Name end, Apps).

%% @doc finds the proper app info record for a given app name at a given version
%% in a list of such records.
-spec find(binary(), binary(), [rebar_app_info:t()]) -> {ok, rebar_app_info:t()} | error.
find(Name, Vsn, Apps) ->
    ec_lists:find(fun(App) ->
                          rebar_app_info:name(App) =:= Name
                              andalso rebar_app_info:original_vsn(App) =:= Vsn
                  end, Apps).

%% @doc checks if a given file is .app.src file
is_app_src(Filename) ->
    %% If removing the extension .app.src yields a shorter name,
    %% this is an .app.src file.
    Filename =/= filename:rootname(Filename, ".app.src").

%% @doc translates the name of the .app.src[.script] file to where
%% its .app counterpart should be stored.
-spec app_src_to_app(OutDir, SrcFilename) -> OutFilename when
      OutDir :: file:filename(),
      SrcFilename :: file:filename(),
      OutFilename :: file:filename().
app_src_to_app(OutDir, Filename) ->
    AppFile =
        case lists:suffix(".app.src", Filename) of
            true ->
                filename:join([OutDir, "ebin", filename:basename(Filename, ".app.src") ++ ".app"]);
            false ->
                filename:join([OutDir, "ebin", filename:basename(Filename,
                                                                 ".app.src.script") ++ ".app"])
        end,
    filelib:ensure_dir(AppFile),
    AppFile.

%% @doc checks whether the .app file has all the required data to be valid,
%% and cross-references it with compiled modules on disk
-spec validate_application_info(rebar_app_info:t()) -> boolean().
validate_application_info(AppInfo) ->
    validate_application_info(AppInfo, rebar_app_info:app_details(AppInfo)).

%% @doc checks whether the .app file has all the required data to be valid
%% and cross-references it with compiled modules on disk.
%% The app info is passed explicitly as a second argument.
-spec validate_application_info(rebar_app_info:t(), list()) -> boolean().
validate_application_info(AppInfo, AppDetail) ->
    EbinDir = rebar_app_info:ebin_dir(AppInfo),
    case rebar_app_info:app_file(AppInfo) of
        undefined ->
            false;
        AppFile ->
            case proplists:get_value(modules, AppDetail) of
                undefined ->
                    ?PRV_ERROR({module_list, AppFile});
                List ->
                    has_all_beams(EbinDir, List)
            end
    end.

%% @doc parses all dependencies from the root of the project
-spec parse_deps(Dir, Deps, State, Locks, Level) -> [rebar_app_info:t()] when
      Dir :: file:filename(),
      Deps :: [tuple() | atom() | binary()], % TODO: meta to source() | lock()
      State :: rebar_state:t(),
      Locks :: [tuple()], % TODO: meta to [lock()]
      Level :: non_neg_integer().
parse_deps(DepsDir, Deps, State, Locks, Level) ->
    parse_deps(root, DepsDir, Deps, State, Locks, Level).

%% @doc runs `parse_dep/6' for a set of dependencies.
-spec parse_deps(Parent, Dir, Deps, State, Locks, Level) -> [rebar_app_info:t()] when
      Parent :: root | binary(),
      Dir :: file:filename(),
      Deps :: [tuple() | atom() | binary()], % TODO: meta to source() | lock()
      State :: rebar_state:t(),
      Locks :: [tuple()], % TODO: meta to [lock()]
      Level :: non_neg_integer().
parse_deps(Parent, DepsDir, Deps, State, Locks, Level) ->
    [parse_dep(Dep, Parent, DepsDir, State, Locks, Level) || Dep <- Deps].

%% @doc for a given dep, return its app info record. The function
%% also has to choose whether to define the dep from its immediate spec
%% (if it is a newer thing) or from the locks specified in the lockfile.
-spec parse_dep(Dep, Parent, Dir, State, Locks, Level) -> rebar_app_info:t() when
      Dep :: tuple() | atom() | binary(), % TODO: meta to source() | lock()
      Parent :: root | binary(),
      Dir :: file:filename(),
      State :: rebar_state:t(),
      Locks :: [tuple()], % TODO: meta to [lock()]
      Level :: non_neg_integer().
parse_dep(Dep, Parent, DepsDir, State, Locks, Level) ->
    Name = case Dep of
               Dep when is_tuple(Dep) ->
                   element(1, Dep);
               Dep ->
                   Dep
           end,
    case lists:keyfind(rebar_utils:to_binary(Name), 1, Locks) of
        false ->
            parse_dep(Parent, Dep, DepsDir, false, State);
        LockedDep ->
            LockedLevel = element(3, LockedDep),
            case LockedLevel > Level of
                true ->
                    parse_dep(Parent, Dep, DepsDir, false, State);
                false ->
                    parse_dep(Parent, LockedDep, DepsDir, true, State)
            end
    end.

%% @doc converts a dependency definition and a location for it on disk
%% into an app info tuple representing it.
-spec parse_dep(Parent, Dep, Dir, IsLock, State) -> rebar_app_info:t() when
      Parent :: root | binary(),
      Dep :: tuple() | atom() | binary(), % TODO: meta to source() | lock()
      Dir :: file:filename(),
      IsLock :: boolean(),
      State :: rebar_state:t().
parse_dep(Parent, {Name, Vsn, {pkg, PkgName}}, DepsDir, IsLock, State) ->
    {PkgName1, PkgVsn} = {rebar_utils:to_binary(PkgName),
                          rebar_utils:to_binary(Vsn)},
    dep_to_app(Parent, DepsDir, Name, PkgVsn, {pkg, PkgName1, PkgVsn, undefined}, IsLock, State);
parse_dep(Parent, {Name, {pkg, PkgName}}, DepsDir, IsLock, State) ->
    %% Package dependency with different package name from app name
    dep_to_app(Parent, DepsDir, Name, undefined, {pkg, rebar_utils:to_binary(PkgName), undefined, undefined}, IsLock, State);
parse_dep(Parent, {Name, Vsn}, DepsDir, IsLock, State) when is_list(Vsn); is_binary(Vsn) ->
    %% Versioned Package dependency
    {PkgName, PkgVsn} = {rebar_utils:to_binary(Name),
                         rebar_utils:to_binary(Vsn)},
    dep_to_app(Parent, DepsDir, PkgName, PkgVsn, {pkg, PkgName, PkgVsn, undefined}, IsLock, State);
parse_dep(Parent, Name, DepsDir, IsLock, State) when is_atom(Name); is_binary(Name) ->
    %% Unversioned package dependency
    dep_to_app(Parent, DepsDir, rebar_utils:to_binary(Name), undefined, {pkg, rebar_utils:to_binary(Name), undefined, undefined}, IsLock, State);
parse_dep(Parent, {Name, Source}, DepsDir, IsLock, State) when is_tuple(Source) ->
    dep_to_app(Parent, DepsDir, Name, [], Source, IsLock, State);
parse_dep(Parent, {Name, _Vsn, Source}, DepsDir, IsLock, State) when is_tuple(Source) ->
    dep_to_app(Parent, DepsDir, Name, [], Source, IsLock, State);
parse_dep(Parent, {Name, _Vsn, Source, Opts}, DepsDir, IsLock, State) when is_tuple(Source),
                                                                           is_list(Opts) ->
    ?WARN("Dependency option list ~p in ~p is not supported and will be ignored", [Opts, Name]),
    dep_to_app(Parent, DepsDir, Name, [], Source, IsLock, State);
parse_dep(Parent, {Name, Source, Opts}, DepsDir, IsLock, State) when is_tuple(Source),
                                                                     is_list(Opts) ->
    ?WARN("Dependency option list ~p in ~p is not supported and will be ignored", [Opts, Name]),
    dep_to_app(Parent, DepsDir, Name, [], Source, IsLock, State);
parse_dep(Parent, {Name, {pkg, PkgName, Vsn}, Level}, DepsDir, IsLock, State) when is_integer(Level) ->
    dep_to_app(Parent, DepsDir, Name, Vsn, {pkg, PkgName, Vsn, undefined}, IsLock, State);
parse_dep(Parent, {Name, {pkg, PkgName, Vsn, Hash}, Level}, DepsDir, IsLock, State) when is_integer(Level) ->
    dep_to_app(Parent, DepsDir, Name, Vsn, {pkg, PkgName, Vsn, Hash}, IsLock, State);
parse_dep(Parent, {Name, Source, Level}, DepsDir, IsLock, State) when is_tuple(Source)
                                                                    , is_integer(Level) ->
    dep_to_app(Parent, DepsDir, Name, [], Source, IsLock, State);
parse_dep(_, Dep, _, _, _) ->
    throw(?PRV_ERROR({parse_dep, Dep})).

%% @doc convert a dependency that has just been fetched into
%% an app info record related to it
-spec dep_to_app(Parent, Dir, Name, Vsn, Source, IsLock, State) -> rebar_app_info:t() when
      Parent :: root | binary(),
      Dir :: file:filename(),
      Name :: binary(),
      Vsn :: iodata() | undefined,
      Source :: tuple(),
      IsLock :: boolean(),
      State :: rebar_state:t().
dep_to_app(Parent, DepsDir, Name, Vsn, Source, IsLock, State) ->
    CheckoutsDir = rebar_utils:to_list(rebar_dir:checkouts_dir(State, Name)),
    AppInfo = case rebar_app_info:discover(CheckoutsDir) of
                  {ok, App} ->
                      rebar_app_info:source(rebar_app_info:is_checkout(App, true), checkout);
                  not_found ->
                      Dir = rebar_utils:to_list(filename:join(DepsDir, Name)),
                      {ok, AppInfo0} =
                          case rebar_app_info:discover(Dir) of
                              {ok, App} ->
                                  {ok, rebar_app_info:is_available(rebar_app_info:parent(App, Parent),
                                                                   true)};
                              not_found ->
                                  rebar_app_info:new(Parent, Name, Vsn, Dir, [])
                          end,
                      rebar_app_info:source(AppInfo0, Source)
              end,
    Overrides = rebar_app_info:get(AppInfo, overrides, []) ++ rebar_state:get(State, overrides, []),
    AppInfo2 = rebar_app_info:set(AppInfo, overrides, Overrides),
    AppInfo5 = rebar_app_info:profiles(AppInfo2, [default]),
    rebar_app_info:is_lock(AppInfo5, IsLock).

%% @doc Takes a given application app_info record along with the project.
%% If the app is a package, resolve and expand the package definition.
-spec expand_deps_sources(rebar_app_info:t(), rebar_state:t()) ->
    rebar_app_info:t().
expand_deps_sources(Dep, State) ->
    update_source(Dep, rebar_app_info:source(Dep), State).

%% @doc sets the source for a given dependency or app along with metadata
%% around version if required.
-spec update_source(rebar_app_info:t(), Source, rebar_state:t()) ->
    rebar_app_info:t() when
      Source :: rebar_resource_v2:source().
update_source(AppInfo, {pkg, PkgName, PkgVsn, Hash}, State) ->
    case rebar_packages:resolve_version(PkgName, PkgVsn, Hash,
                                        ?PACKAGE_TABLE, State) of
        {ok, Package, RepoConfig} ->
            #package{key={_, PkgVsn1, _},
                     checksum=Hash1,
                     dependencies=Deps,
                     retired=Retired} = Package,
            maybe_warn_retired(PkgName, PkgVsn1, Hash, Retired),
            PkgVsn2 = list_to_binary(lists:flatten(ec_semver:format(PkgVsn1))),
            AppInfo1 = rebar_app_info:source(AppInfo, {pkg, PkgName, PkgVsn2, Hash1, RepoConfig}),
            AppInfo2 = rebar_app_info:update_opts_deps(AppInfo1, Deps),
            rebar_app_info:original_vsn(AppInfo2, PkgVsn2);
        not_found ->
            throw(?PRV_ERROR({missing_package, PkgName, PkgVsn}));
        {error, {invalid_vsn, InvalidVsn}} ->
            throw(?PRV_ERROR({invalid_vsn, PkgName, InvalidVsn}))
    end;
update_source(AppInfo, Source, _State) ->
    rebar_app_info:source(AppInfo, Source).

%% @doc convert a given exception's payload into an io description.
-spec format_error(any()) -> iolist().
format_error({missing_package, Name, undefined}) ->
    io_lib:format("Package not found in any repo: ~ts", [rebar_utils:to_binary(Name)]);
format_error({missing_package, Name, Constraint}) ->
    io_lib:format("Package not found in any repo: ~ts ~ts", [Name, Constraint]);
format_error({parse_dep, Dep}) ->
    io_lib:format("Failed parsing dep ~p", [Dep]);
format_error({invalid_vsn, Dep, InvalidVsn}) ->
    io_lib:format("Dep ~ts has invalid version ~ts", [Dep, InvalidVsn]);
format_error(Error) ->
    io_lib:format("~p", [Error]).

%% ===================================================================
%% Internal functions
%% ===================================================================

maybe_warn_retired(_, _, _, false) ->
    ok;
maybe_warn_retired(_, _, Hash, _) when is_binary(Hash) ->
    %% don't warn if this is a lock
    ok;
maybe_warn_retired(Name, Vsn, _, R=#{reason := Reason}) ->
    Message = maps:get(message, R, ""),
    ?WARN("Warning: package ~s-~s is retired: (~s) ~s",
          [Name, ec_semver:format(Vsn), retire_reason(Reason), Message]);
maybe_warn_retired(_, _, _, _) ->
    ok.

%% TODO: move to hex_core
retire_reason('RETIRED_OTHER') ->
    "other";
retire_reason('RETIRED_INVALID') ->
    "invalid";
retire_reason('RETIRED_SECURITY') ->
    "security";
retire_reason('RETIRED_DEPRECATED') ->
    "deprecated";
retire_reason('RETIRED_RENAMED') ->
    "renamed";
retire_reason(_Other) ->
    "other".

%% @private checks that all the beam files have been properly
%% created.
-spec has_all_beams(file:filename_all(), [module()]) ->
    true | ?PRV_ERROR({missing_module, module()}).
has_all_beams(EbinDir, [Module | ModuleList]) ->
    BeamFile = filename:join([EbinDir, rebar_utils:to_list(Module) ++ ".beam"]),
    case filelib:is_file(BeamFile) of
        true ->
            has_all_beams(EbinDir, ModuleList);
        false ->
            ?PRV_ERROR({missing_module, Module})
    end;
has_all_beams(_, []) ->
    true.
