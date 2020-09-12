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
-module(rebar_config).

-export([consult_root/0
        ,consult/1
        ,consult_app_file/1
        ,consult_file/1
        ,consult_lock_file/1
        ,maybe_write_lock_file/3
        ,write_lock_file/2
        ,verify_config_format/1
        ,format_error/1
        ,merge_locks/2]).

-include("rebar.hrl").
-include_lib("providers/include/providers.hrl").

-define(DEFAULT_CONFIG_FILE, "rebar.config").

%% ===================================================================
%% Public API
%% ===================================================================

%% @doc reads the default config file at the top of a full project
-spec consult_root() -> [any()].
consult_root() ->
    consult_file(config_file()).

%% @doc reads the default config file in a given directory.
-spec consult(file:name()) -> [any()].
consult(Dir) ->
    consult_file(filename:join(Dir, ?DEFAULT_CONFIG_FILE)).

%% @doc reads a given app file, including the `.script' variations,
%% if any can be found.
-spec consult_app_file(file:filename()) -> [any()].
consult_app_file(File) ->
    consult_file_(File).

%% @doc reads the lock file for the project, and re-formats its
%% content to match the internals for rebar3.
-spec consult_lock_file(file:filename()) -> [any()]. % TODO: refine lock()
consult_lock_file(File) ->
    Terms = consult_file_(File),
    case Terms of
        [] ->
            [];
        [Locks] when is_list(Locks) -> % beta/1.0.0 lock file
            read_attrs(beta, Locks, []);
        [{Vsn, Locks}|Attrs] when is_list(Locks) -> % versioned lock file
            %% Because this is the first version of rebar3 to introduce a lock
            %% file, all versioned lock files with a different version have
            %% to be newer.
            case Vsn of
                ?CONFIG_VERSION ->
                    ok;
                _ ->
                    case lists:member(Vsn, ?SUPPORTED_CONFIG_VERSIONS) of
                        true ->
                            ok;
                        false ->
                            %% Make sure the warning below is to be shown
                            %% whenever a version newer than the current
                            %% one is being used, as we can't parse all the
                            %% contents of the lock file properly.
                            warn_vsn_once()
                    end
            end,
            read_attrs(Vsn, Locks, Attrs)
    end.

%% @private outputs a warning for a newer lockfile format than supported
%% at most once.
%% The warning can also be cancelled by configuring the `warn_config_vsn'
%% OTP env variable.
-spec warn_vsn_once() -> ok.
warn_vsn_once() ->
    Warn = application:get_env(rebar, warn_config_vsn) =/= {ok, false},
    application:set_env(rebar, warn_config_vsn, false),
    case Warn of
        false -> ok;
        true ->
            ?WARN("Rebar3 detected a lock file from a newer version. "
                  "It will be loaded in compatibility mode, but important "
                  "information may be missing or lost. It is recommended to "
                  "upgrade Rebar3.", [])
    end.

%% Only call `write_lock_file/2' if the locks have changed.
maybe_write_lock_file(LockFile, Locks, OldLocks) when Locks =/= OldLocks ->
    write_lock_file(LockFile, Locks);
maybe_write_lock_file(LockFile, Locks, Locks) ->
    %% rewrite if the configured format would have changed
    Terms = consult_file_(LockFile),
    case Terms of
        [] ->
            case filelib:is_regular(LockFile) of
                true -> ok;
                false -> write_lock_file(LockFile, Locks)
            end;
        [{?CONFIG_VERSION, FileLocks}|_] when is_list(FileLocks) ->
            ok;
        _ ->
            write_lock_file(LockFile, Locks)
    end.

%% @doc Converts the internal format for locks into the multi-version
%% compatible one used within rebar3 lock files.
%% @end
%% TODO: refine type for lock()
-spec write_lock_file(file:filename(), [any()]) -> ok | {error, term()}.
write_lock_file(LockFile, Locks) ->
    {NewLocks, Attrs} = write_attrs(Locks),
    %% Write locks in the beta format, at least until it's been long
    %% enough we can start modifying the lock format.
    case Attrs of
        [] -> % write the old beta copy to avoid changes
            file:write_file(LockFile, io_lib:format("~p.~n", [NewLocks]));
        _ ->
            file:write_file(LockFile,
                            io_lib:format("{~p,~n~p}.~n[~n~ts~n].~n",
                                          [?CONFIG_VERSION, NewLocks,
                                           format_attrs(Attrs)]))
    end.

%% @private Because attributes for packages are fairly large, there is the need
%% for a special formatting to ensure there's only one entry per lock file
%% line and that diffs are generally stable.
-spec format_attrs([term()]) -> iodata().
format_attrs([]) -> [];
format_attrs([{pkg_hash, Vals}|T]) ->
    [io_lib:format("{pkg_hash,[~n",[]), format_hashes(Vals), "]}",
     maybe_comma(T) | format_attrs(T)];
format_attrs([{pkg_hash_ext, Vals}|T]) ->
    [io_lib:format("{pkg_hash_ext,[~n",[]), format_hashes(Vals), "]}",
     maybe_comma(T) | format_attrs(T)].

%% @private format hashing in order to disturb source diffing as little
%% as possible
-spec format_hashes([term()]) -> iodata().
format_hashes([]) -> [];
format_hashes([{Pkg,Hash}|T]) ->
    [" {", io_lib:format("~p",[Pkg]), ", ", io_lib:format("~p", [Hash]), "}",
     maybe_comma(T) | format_hashes(T)].

%% @private add a comma if we're not done with the full list of terms
%% to convert.
-spec maybe_comma([term()]) -> iodata().
maybe_comma([]) -> "";
maybe_comma([_|_]) -> io_lib:format(",~n", []).

%% @private extract attributes from the lock file and integrate them
%% into the full-blow internal lock format
%% @end
%% TODO: refine typings for lock()
-spec read_attrs(_, [any()], [any()]) -> [any()].
read_attrs(_Vsn, Locks, Attrs) ->
    %% Beta copy does not know how to expand attributes, but
    %% is ready to support it.
    {OldHashes, NewHashes} =  extract_pkg_hashes(Attrs),
    expand_locks(Locks, OldHashes, NewHashes).

%% @private extract the package hashes from lockfile attributes, if any.
-spec extract_pkg_hashes(list()) -> {[binary()], [binary()]}.
extract_pkg_hashes(Attrs) ->
    Props = case Attrs of
                [First|_] -> First;
                [] -> []
            end,
     { proplists:get_value(pkg_hash, Props, []), proplists:get_value(pkg_hash_ext, Props, [])}.

%% @private extract attributes from the lock file and integrate them
%% into the full-blow internal lock format
%% @end
%% TODO: refine typings for lock()
-spec expand_locks(list(), list(), list()) -> list().
expand_locks([], _OldHashes, _NewHashes) ->
    [];
expand_locks([{Name, {pkg,PkgName,Vsn}, Lvl} | Locks], OldHashes, NewHashes) ->
    OldHash = proplists:get_value(Name, OldHashes),
    NewHash = proplists:get_value(Name, NewHashes),
    [{Name, {pkg,PkgName,Vsn,OldHash, NewHash}, Lvl} | expand_locks(Locks, OldHashes, NewHashes)];
expand_locks([Lock|Locks], OldHashes, NewHashes) ->
    [Lock | expand_locks(Locks, OldHashes, NewHashes)].

%% @private split up extra attributes for locks out of the internal lock
%% structure for backwards compatibility reasons
-spec write_attrs(list()) -> {list(), list()}.
write_attrs(Locks) ->
    %% No attribute known that needs to be taken out of the structure,
    %% just return terms as is.
    {NewLocks, OldHashes, NewHashes} = split_locks(Locks, [], [], []),
    case {OldHashes, NewHashes} of
        {[], []} -> {NewLocks, []};
        _ ->
            {NewLocks, [{pkg_hash, lists:sort(OldHashes)}, {pkg_hash_ext, lists:sort(NewHashes)}]}
    end.

%% @private split up extra attributes for locks out of the internal lock
%% structure for backwards compatibility reasons
-spec split_locks(list(), list(), [{_,binary()}], [{_,binary()}]) -> {list(), list(), list()}.
split_locks([], Locks, OldHashes, NewHashes) ->
    {lists:reverse(Locks), OldHashes, NewHashes};
split_locks([{Name, {pkg,PkgName,Vsn,undefined}, Lvl} | Locks], LAcc, OldHAcc, NewHAcc) ->
    split_locks(Locks, [{Name,{pkg,PkgName,Vsn},Lvl}|LAcc], OldHAcc, NewHAcc);
split_locks([{Name, {pkg,PkgName,Vsn,undefined, undefined}, Lvl} | Locks], LAcc, OldHAcc, NewHAcc) ->
    split_locks(Locks, [{Name,{pkg,PkgName,Vsn},Lvl}|LAcc], OldHAcc, NewHAcc);
split_locks([{Name, {pkg,PkgName,Vsn, OldHash}, Lvl} | Locks], LAcc, OldHAcc, NewHAcc) ->
    split_locks(Locks, [{Name,{pkg,PkgName, Vsn},Lvl}|LAcc], [{Name, OldHash}|OldHAcc], NewHAcc);
split_locks([{Name, {pkg,PkgName,Vsn, OldHash, NewHash}, Lvl} | Locks], LAcc, OldHAcc, NewHAcc) ->
    split_locks(Locks, [{Name,{pkg,PkgName,Vsn},Lvl}|LAcc], [{Name, OldHash}|OldHAcc], [{Name, NewHash}|NewHAcc]);
split_locks([Lock|Locks], LAcc, OldHAcc, NewHAcc) ->
    split_locks(Locks, [Lock|LAcc], OldHAcc, NewHAcc).

%% @doc reads a given config file, including the `.script' variations,
%% if any can be found, and asserts that the config format is in
%% a key-value format.
-spec consult_file(file:filename()) -> [{_,_}].
consult_file(File) ->
    Terms = consult_file_(File),
    true = verify_config_format(Terms),
    Terms.

%% @private reads a given file; if the file has a `.script'-postfixed
%% counterpart, it is evaluated along with the original file.
-spec consult_file_(file:name()) -> [any()].
consult_file_(File) when is_binary(File) ->
    consult_file_(binary_to_list(File));
consult_file_(File) ->
    case filename:extension(File) of
        ".script" ->
            {ok, Terms} = consult_and_eval(remove_script_ext(File), File),
            Terms;
        _ ->
            Script = File ++ ".script",
            case filelib:is_regular(Script) of
                true ->
                    {ok, Terms} = consult_and_eval(File, Script),
                    Terms;
                false ->
                    rebar_file_utils:try_consult(File)
            end
    end.

%% @private checks that a list is in a key-value format.
%% Raises an exception in any other case.
-spec verify_config_format([{_,_}]) -> true.
verify_config_format([]) ->
    true;
verify_config_format([{_Key, _Value} | T]) ->
    verify_config_format(T);
verify_config_format([Term | _]) ->
    throw(?PRV_ERROR({bad_config_format, Term})).

%% @doc takes an existing configuration and the content of a lockfile
%% and merges the locks into the config.
-spec merge_locks([{_,_}], list()) -> [{_,_}].
merge_locks(Config, []) ->
%% no lockfile
    Config;
merge_locks(Config, Locks) ->
    %% lockfile with entries
    ConfigDeps = proplists:get_value(deps, Config, []),
    %% We want the top level deps only from the lock file.
    %% This ensures deterministic overrides for configs.
    %% Then check if any new deps have been added to the config
    %% since it was locked.
    Deps = [X || X <- Locks, element(3, X) =:= 0],
    NewDeps = find_newly_added(ConfigDeps, Locks),
    [{{locks, default}, Locks}, {{deps, default}, NewDeps++Deps} | Config].

%% @doc convert a given exception's payload into an io description.
-spec format_error(any()) -> iolist().
format_error({bad_config_format, Term}) ->
    io_lib:format("Unable to parse config. Term is not in {Key, Value} format:~n~p", [Term]);
format_error({bad_dep_name, Dep}) ->
    io_lib:format("Dependency name must be an atom, instead found: ~p", [Dep]).

%% ===================================================================
%% Internal functions
%% ===================================================================

%% @private consults a consult file, then executes its related script file
%% with the data returned from the consult.
-spec consult_and_eval(File::file:name_all(), Script::file:name_all()) ->
                              {ok, Terms::[term()]} |
                              {error, Reason::term()}.
consult_and_eval(File, Script) ->
    ?DEBUG("Evaluating config script ~p", [Script]),
    StateData = rebar_file_utils:try_consult(File),
    %% file:consult/1 always returns the terms as a list, however file:script
    %% can (and will) return any kind of term(), to make consult_and_eval
    %% work the same way as eval we ensure that when no list is returned we
    %% convert it in a list.
    case file:script(Script, bs([{'CONFIG', StateData}, {'SCRIPT', Script}])) of
        {ok, Terms} when is_list(Terms) ->
            {ok, Terms};
        {ok, Term} ->
            {ok, [Term]};
        Error ->
            ?ERROR("Error evaluating configuration script at ~p:~n~p~n",
                   [Script, Error]),
            Error
    end.

%% @private drops the .script extension from a filename.
-spec remove_script_ext(file:filename()) -> file:filename().
remove_script_ext(F) ->
    filename:rootname(F, ".script").

%% @private sets up bindings for evaluations from a KV list.
-spec bs([{_,_}]) -> erl_eval:binding_struct().
bs(Vars) ->
    lists:foldl(fun({K,V}, Bs) ->
                        erl_eval:add_binding(K, V, Bs)
                end, erl_eval:new_bindings(), Vars).

%% @private Find deps that have been added to the config after the lock was created
-spec find_newly_added(list(), list()) -> list().
find_newly_added(ConfigDeps, LockedDeps) ->
    [D || {true, D} <- [check_newly_added(Dep, LockedDeps) || Dep <- ConfigDeps]].

%% @private checks if a given dependency is not within the lock file.
%% TODO: refine types for dependencies
-spec check_newly_added(term(), list()) -> false | {true, term()}.
check_newly_added({_, _}=Dep, LockedDeps) ->
    check_newly_added_(Dep, LockedDeps);
check_newly_added({_, _, {pkg, _}}=Dep, LockedDeps) ->
    check_newly_added_(Dep, LockedDeps);
check_newly_added({Name, _, Source}, LockedDeps) ->
    check_newly_added_({Name, Source}, LockedDeps);
check_newly_added(Dep, LockedDeps) ->
    check_newly_added_(Dep, LockedDeps).

%% @private checks if a given dependency is not within the lock file.
%% TODO: refine types for dependencies
%% @end
-spec check_newly_added_(term(), list()) -> false | {true, term()}.
%% get [raw] deps out of the way
check_newly_added_({Name, Source, Opts}, LockedDeps) when is_tuple(Source),
                                                          is_list(Opts) ->
    case check_newly_added_(Name, LockedDeps) of
        {true, Name1} ->
            {true, {Name1, Source}};
        false ->
            false
    end;
check_newly_added_({Name,_Vsn,Source,Opts}, LockedDeps) when is_tuple(Source),
                                                             is_list(Opts) ->
    case check_newly_added_(Name, LockedDeps) of
        {true, Name1} ->
            {true, {Name1, Source}};
        false ->
            false
    end;
%% and on to regular deps
check_newly_added_({Name, Vsn, Source}, LockedDeps) ->
    case check_newly_added_(Name, LockedDeps) of
        {true, Name1} ->
            {true, {Name1, Vsn, Source}};
        false ->
            false
    end;
check_newly_added_({Name, Source}, LockedDeps) ->
    case check_newly_added_(Name, LockedDeps) of
        {true, Name1} ->
            {true, {Name1, Source}};
        false ->
            false
    end;
check_newly_added_(Dep, LockedDeps) when is_atom(Dep) ->
    Name = rebar_utils:to_binary(Dep),
    case lists:keyfind(Name, 1, LockedDeps) of
        false ->
            {true, Name};
        Match ->
            case element(3, Match) of
                0 ->
                    {true, Name};
                _ ->
                    ?WARN("Newly added dep ~ts is locked at a lower level. "
                          "If you really want to unlock it, use 'rebar3 upgrade ~ts'",
                          [Name, Name]),
                    false
            end
    end;
check_newly_added_(Dep, _) ->
    throw(?PRV_ERROR({bad_dep_name, Dep})).

%% @private returns the name/path of the default config file, or its
%% override from the OS ENV var `REBAR_CONFIG'.
-spec config_file() -> file:filename().
config_file() ->
    case os:getenv("REBAR_CONFIG") of
        false ->
            ?DEFAULT_CONFIG_FILE;
        ConfigFile ->
            ConfigFile
    end.
