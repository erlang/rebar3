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
-module(rebar_core).

-export([process_commands/2,
         skip_dir/1,
         is_skip_dir/1,
         skip_dirs/0]).

-include("rebar.hrl").


%% ===================================================================
%% Public API
%% ===================================================================

skip_dir(Dir) ->
    SkipDir = {skip_dir, Dir},
    case erlang:get(SkipDir) of
        undefined ->
            ?DEBUG("Adding skip dir: ~s\n", [Dir]),
            erlang:put(SkipDir, true);
        true ->
            ok
    end.

is_skip_dir(Dir) ->
    case erlang:get({skip_dir, Dir}) of
        undefined ->
            false;
        true ->
            true
    end.

skip_dirs() ->
    [Dir || {{skip_dir, Dir}, true} <- erlang:get()].

%% ===================================================================
%% Internal functions
%% ===================================================================

process_commands([], _ParentConfig) ->
    case erlang:get(operations) of
        0 ->
            %% none of the commands had an effect
            ?FAIL;
        _ ->
            ok
    end;
process_commands([Command | Rest], ParentConfig) ->
    %% Reset skip dirs
    lists:foreach(fun (D) -> erlang:erase({skip_dir, D}) end, skip_dirs()),
    Operations = erlang:get(operations),

    %% Convert the code path so that all the entries are absolute paths.
    %% If not, code:set_path() may choke on invalid relative paths when trying
    %% to restore the code path from inside a subdirectory.
    true = rebar_utils:expand_code_path(),
    _ = process_dir(rebar_utils:get_cwd(), ParentConfig,
                    Command, sets:new()),
    case erlang:get(operations) of
        Operations ->
            %% This command didn't do anything
            ?CONSOLE("Command '~p' not understood or not applicable~n",
                     [Command]);
        _ ->
            ok
    end,
    %% Wipe out vsn cache to avoid invalid hits when
    %% dependencies are updated
    ets:delete_all_objects(rebar_vsn_cache),
    process_commands(Rest, ParentConfig).


process_dir(Dir, ParentConfig, Command, DirSet) ->
    case filelib:is_dir(Dir) of
        false ->
            ?WARN("Skipping non-existent sub-dir: ~p\n", [Dir]),
            DirSet;

        true ->
            AbsDir = filename:absname(Dir),
            ShouldPrintDir = not (is_skip_dir(Dir) orelse processing_base_dir(Dir)),

            case ShouldPrintDir of
                true ->
                    ?CONSOLE("==> Entering directory `~s'\n", [AbsDir]);
                _ ->
                    ok
            end,

            ok = file:set_cwd(Dir),
            Config = maybe_load_local_config(Dir, ParentConfig),

            %% Save the current code path and then update it with
            %% lib_dirs. Children inherit parents code path, but we
            %% also want to ensure that we restore everything to pristine
            %% condition after processing this child
            CurrentCodePath = update_code_path(Config),

            %% Get the list of processing modules and check each one against
            %% CWD to see if it's a fit -- if it is, use that set of modules
            %% to process this dir.
            {ok, AvailModuleSets} = application:get_env(rebar, modules),
            ModuleSet = choose_module_set(AvailModuleSets, Dir),
            Res = maybe_process_dir(ModuleSet, Config, CurrentCodePath,
                                    Dir, Command, DirSet),

            case ShouldPrintDir of
                true ->
                    ?CONSOLE("==> Leaving directory `~s'\n", [AbsDir]);
                false ->
                    ok
            end,

            Res
    end.

maybe_process_dir({[], undefined}=ModuleSet, Config, CurrentCodePath,
                  Dir, Command, DirSet) ->
    process_dir0(Dir, Command, DirSet, Config, CurrentCodePath, ModuleSet);
maybe_process_dir({_, ModuleSetFile}=ModuleSet, Config, CurrentCodePath,
                  Dir, Command, DirSet) ->
    case lists:suffix(".app.src", ModuleSetFile)
        orelse lists:suffix(".app", ModuleSetFile) of
        true ->
            %% .app or .app.src file, check if is_skipped_app
            maybe_process_dir0(ModuleSetFile, ModuleSet,
                               Config, CurrentCodePath, Dir,
                               Command, DirSet);
        false ->
            %% not an app dir, no need to consider apps=/skip_apps=
            process_dir0(Dir, Command, DirSet, Config,
                         CurrentCodePath, ModuleSet)
    end.

maybe_process_dir0(AppFile, ModuleSet, Config, CurrentCodePath,
                   Dir, Command, DirSet) ->
    case rebar_app_utils:is_skipped_app(AppFile) of
        {true, SkippedApp} ->
            ?DEBUG("Skipping app: ~p~n", [SkippedApp]),
            increment_operations(),
            DirSet;
        false ->
            process_dir0(Dir, Command, DirSet, Config,
                         CurrentCodePath, ModuleSet)
    end.

process_dir0(Dir, Command, DirSet, Config0, CurrentCodePath,
             {DirModules, ModuleSetFile}) ->
    %% Get the list of modules for "any dir". This is a catch-all list
    %% of modules that are processed in addition to modules associated
    %% with this directory type. These any_dir modules are processed
    %% FIRST.
    {ok, AnyDirModules} = application:get_env(rebar, any_dir_modules),

    Modules = AnyDirModules ++ DirModules,

    %% Invoke 'preprocess' on the modules -- this yields a list of other
    %% directories that should be processed _before_ the current one.
    Predirs = acc_modules(Modules, preprocess, Config0, ModuleSetFile),

    SubdirAssoc = remember_cwd_subdir(Dir, Predirs),

    %% Get the list of plug-in modules from rebar.config. These
    %% modules may participate in preprocess and postprocess.
    {ok, PluginModules} = plugin_modules(Config0, SubdirAssoc),

    PluginPredirs = acc_modules(PluginModules, preprocess,
                                Config0, ModuleSetFile),

    AllPredirs = Predirs ++ PluginPredirs,

    ?DEBUG("Predirs: ~p\n", [AllPredirs]),
    DirSet2 = process_each(AllPredirs, Command, Config0,
                           ModuleSetFile, DirSet),

    %% Make sure the CWD is reset properly; processing the dirs may have
    %% caused it to change
    ok = file:set_cwd(Dir),

    %% Check that this directory is not on the skip list
    Config = case is_skip_dir(Dir) of
                 true ->
                     %% Do not execute the command on the directory, as some
                     %% module has requested a skip on it.
                     ?INFO("Skipping ~s in ~s\n", [Command, Dir]),
                     Config0;

                 false ->
                     %% Check for and get command specific environments
                     {Config1, Env} = setup_envs(Config0, Modules),

                     %% Execute any before_command plugins on this directory
                     execute_pre(Command, PluginModules,
                                 Config1, ModuleSetFile, Env),

                     %% Execute the current command on this directory
                     execute(Command, Modules ++ PluginModules,
                             Config1, ModuleSetFile, Env),

                     %% Execute any after_command plugins on this directory
                     execute_post(Command, PluginModules,
                                  Config1, ModuleSetFile, Env),

                     Config1
             end,

    %% Mark the current directory as processed
    DirSet3 = sets:add_element(Dir, DirSet2),

    %% Invoke 'postprocess' on the modules. This yields a list of other
    %% directories that should be processed _after_ the current one.
    Postdirs = acc_modules(Modules ++ PluginModules, postprocess,
                           Config, ModuleSetFile),
    ?DEBUG("Postdirs: ~p\n", [Postdirs]),
    DirSet4 = process_each(Postdirs, Command, Config,
                           ModuleSetFile, DirSet3),

    %% Make sure the CWD is reset properly; processing the dirs may have
    %% caused it to change
    ok = file:set_cwd(Dir),

    %% Once we're all done processing, reset the code path to whatever
    %% the parent initialized it to
    restore_code_path(CurrentCodePath),

    %% Return the updated dirset as our result
    DirSet4.

remember_cwd_subdir(Cwd, Subdirs) ->
    Store = fun(Dir, Dict) ->
                    case dict:find(Dir, Dict) of
                        error ->
                            ?DEBUG("Associate sub_dir ~s with ~s~n", [Dir, Cwd]),
                            dict:store(Dir, Cwd, Dict);
                        {ok, Existing} ->
                            ?ABORT("Internal consistency assertion failed.~n"
                                   "sub_dir ~s already associated with ~s.~n"
                                   "Duplicate sub_dirs or deps entries?",
                                   [Dir, Existing]),
                            Dict
                    end
            end,
    lists:foldl(Store, dict:new(), Subdirs).

maybe_load_local_config(Dir, ParentConfig) ->
    %% We need to ensure we don't overwrite custom
    %% config when we are dealing with base_dir.
    case processing_base_dir(Dir) of
        true ->
            ParentConfig;
        false ->
            rebar_config:new(ParentConfig)
    end.

processing_base_dir(Dir) ->
    Dir == rebar_config:get_global(base_dir, undefined).

%%
%% Given a list of directories and a set of previously processed directories,
%% process each one we haven't seen yet
%%
process_each([], _Command, _Config, _ModuleSetFile, DirSet) ->
    DirSet;
process_each([Dir | Rest], Command, Config, ModuleSetFile, DirSet) ->
    case sets:is_element(Dir, DirSet) of
        true ->
            ?DEBUG("Skipping ~s; already processed!\n", [Dir]),
            process_each(Rest, Command, Config, ModuleSetFile, DirSet);
        false ->
            DirSet2 = process_dir(Dir, Config, Command, DirSet),
            process_each(Rest, Command, Config, ModuleSetFile, DirSet2)
    end.


%%
%% Given a list of module sets from rebar.app and a directory, find
%% the appropriate subset of modules for this directory
%%
choose_module_set([], _Dir) ->
    {[], undefined};
choose_module_set([{Type, Modules} | Rest], Dir) ->
    case is_dir_type(Type, Dir) of
        {true, File} ->
            {Modules, File};
        false ->
            choose_module_set(Rest, Dir)
    end.

is_dir_type(app_dir, Dir) ->
    rebar_app_utils:is_app_dir(Dir);
is_dir_type(rel_dir, Dir) ->
    rebar_rel_utils:is_rel_dir(Dir);
is_dir_type(_, _) ->
    false.

execute_pre(Command, Modules, Config, ModuleFile, Env) ->
    execute_plugin_hook("pre_", Command, Modules,
                        Config, ModuleFile, Env).

execute_post(Command, Modules, Config, ModuleFile, Env) ->
    execute_plugin_hook("post_", Command, Modules,
                        Config, ModuleFile, Env).

execute_plugin_hook(Hook, Command, Modules, Config, ModuleFile, Env) ->
    HookFunction = list_to_atom(Hook ++ atom_to_list(Command)),
    execute(HookFunction, Modules, Config, ModuleFile, Env).

%%
%% Execute a command across all applicable modules
%%
execute(Command, Modules, Config, ModuleFile, Env) ->
    case select_modules(Modules, Command, []) of
        [] ->
            Cmd = atom_to_list(Command),
            case lists:prefix("pre_", Cmd)
                orelse lists:prefix("post_", Cmd) of
                true ->
                    ok;
                false ->
                    ?WARN("'~p' command does not apply to directory ~s\n",
                          [Command, rebar_utils:get_cwd()])
            end;

        TargetModules ->
            %% Provide some info on where we are
            Dir = rebar_utils:get_cwd(),
            ?CONSOLE("==> ~s (~s)\n", [filename:basename(Dir), Command]),

            increment_operations(),

            %% Run the available modules
            apply_hooks(pre_hooks, Config, Command, Env),
            case catch(run_modules(TargetModules, Command,
                                   Config, ModuleFile)) of
                ok ->
                    apply_hooks(post_hooks, Config, Command, Env),
                    ok;
                {error, failed} ->
                    ?FAIL;
                {Module, {error, _} = Other} ->
                    ?ABORT("~p failed while processing ~s in module ~s: ~s\n",
                           [Command, Dir, Module,
                            io_lib:print(Other, 1, 80, -1)]);
                Other ->
                    ?ABORT("~p failed while processing ~s: ~s\n",
                           [Command, Dir, io_lib:print(Other, 1, 80, -1)])
            end
    end.

%% Increment the count of operations, since some module
%% responds to this command
increment_operations() ->
    erlang:put(operations, erlang:get(operations) + 1).


update_code_path(Config) ->
    case rebar_config:get_local(Config, lib_dirs, []) of
        [] ->
            no_change;
        Paths ->
            OldPath = code:get_path(),
            LibPaths = expand_lib_dirs(Paths, rebar_utils:get_cwd(), []),
            ok = code:add_pathsa(LibPaths),
            {old, OldPath}
    end.

restore_code_path(no_change) ->
    ok;
restore_code_path({old, Path}) ->
    %% Verify that all of the paths still exist -- some dynamically
    %% added paths can get blown away during clean.
    true = code:set_path([F || F <- Path, filelib:is_file(F)]),
    ok.


expand_lib_dirs([], _Root, Acc) ->
    Acc;
expand_lib_dirs([Dir | Rest], Root, Acc) ->
    Apps = filelib:wildcard(filename:join([Dir, "*", "ebin"])),
    FqApps = [filename:join([Root, A]) || A <- Apps],
    expand_lib_dirs(Rest, Root, Acc ++ FqApps).



select_modules([], _Command, Acc) ->
    lists:reverse(Acc);
select_modules([Module | Rest], Command, Acc) ->
    {module, Module} = code:ensure_loaded(Module),
    case erlang:function_exported(Module, Command, 2) of
        true ->
            select_modules(Rest, Command, [Module | Acc]);
        false ->
            select_modules(Rest, Command, Acc)
    end.

run_modules([], _Command, _Config, _File) ->
    ok;
run_modules([Module | Rest], Command, Config, File) ->
    case Module:Command(Config, File) of
        ok ->
            run_modules(Rest, Command, Config, File);
        {error, _} = Error ->
            {Module, Error}
    end.

apply_hooks(Mode, Config, Command, Env) ->
    Hooks = rebar_config:get_local(Config, Mode, []),
    lists:foreach(fun apply_hook/1,
                  [{Env, Hook} || Hook <- Hooks,
                                  element(1, Hook) =:= Command orelse
                                      element(2, Hook) =:= Command]).

apply_hook({Env, {Arch, Command, Hook}}) ->
    case rebar_utils:is_arch(Arch) of
        true ->
            apply_hook({Env, {Command, Hook}});
        false ->
            ok
    end;
apply_hook({Env, {Command, Hook}}) ->
    Msg = lists:flatten(io_lib:format("Command [~p] failed!~n", [Command])),
    rebar_utils:sh(Hook, [{env, Env}, {abort_on_error, Msg}]).

setup_envs(Config, Modules) ->
    lists:foldl(fun(M, {C,E}=T) ->
                        case erlang:function_exported(M, setup_env, 1) of
                            true ->
                                Env = M:setup_env(C),
                                C1 = rebar_config:set_env(C, M, Env),
                                {C1, E++Env};
                            false ->
                                T
                        end
                end, {Config, []}, Modules).

acc_modules(Modules, Command, Config, File) ->
    acc_modules(select_modules(Modules, Command, []),
                Command, Config, File, []).

acc_modules([], _Command, _Config, _File, Acc) ->
    Acc;
acc_modules([Module | Rest], Command, Config, File, Acc) ->
    {ok, Dirs} = Module:Command(Config, File),
    acc_modules(Rest, Command, Config, File, Acc ++ Dirs).

%%
%% Return a flat list of rebar plugin modules.
%%
plugin_modules(Config, SubdirAssoc) ->
    Modules = lists:flatten(rebar_config:get_all(Config, plugins)),
    plugin_modules(Config, SubdirAssoc, ulist(Modules)).

ulist(L) ->
    ulist(L, []).

ulist([], Acc) ->
    lists:reverse(Acc);
ulist([H | T], Acc) ->
    case lists:member(H, Acc) of
        true ->
            ulist(T, Acc);
        false ->
            ulist(T, [H | Acc])
    end.

plugin_modules(_Config, _SubdirAssoc, []) ->
    {ok, []};
plugin_modules(Config, SubdirAssoc, Modules) ->
    FoundModules = [M || M <- Modules, code:which(M) =/= non_existing],
    plugin_modules(Config, SubdirAssoc, FoundModules, Modules -- FoundModules).

plugin_modules(_Config, _SubdirAssoc, FoundModules, []) ->
    {ok, FoundModules};
plugin_modules(Config, SubdirAssoc, FoundModules, MissingModules) ->
    {Loaded, NotLoaded} = load_plugin_modules(Config, SubdirAssoc, MissingModules),
    AllViablePlugins = FoundModules ++ Loaded,
    case NotLoaded =/= [] of
        true ->
            %% NB: we continue to ignore this situation, as did the original code
            ?WARN("Missing plugins: ~p\n", [NotLoaded]);
        false ->
            ?DEBUG("Loaded plugins: ~p~n", [AllViablePlugins]),
            ok
    end,
    {ok, AllViablePlugins}.

load_plugin_modules(Config, SubdirAssoc, Modules) ->
    Cwd = rebar_utils:get_cwd(),
    PluginDir = case rebar_config:get_local(Config, plugin_dir, undefined) of
                    undefined ->
                        filename:join(Cwd, "plugins");
                    Dir ->
                        Dir
                end,

    %% Find relevant sources in base_dir and plugin_dir
    Erls = string:join([atom_to_list(M)++"\\.erl" || M <- Modules], "|"),
    RE = "^" ++ Erls ++ "\$",
    BaseDir = get_plugin_base_dir(Cwd, SubdirAssoc),
    %% If a plugin is found both in base_dir and plugin_dir, the clash
    %% will provoke an error and we'll abort.
    Sources = rebar_utils:find_files(PluginDir, RE, false)
        ++ rebar_utils:find_files(BaseDir, RE, false),

    %% Compile and load plugins
    Loaded = [load_plugin(Src) || Src <- Sources],
    FilterMissing = is_missing_plugin(Loaded),
    NotLoaded = [V || V <- Modules, FilterMissing(V)],
    {Loaded, NotLoaded}.

get_plugin_base_dir(Cwd, SubdirAssoc) ->
    case dict:find(Cwd, SubdirAssoc) of
        {ok, BaseDir} ->
            BaseDir;
        error ->
            Cwd
    end.

is_missing_plugin(Loaded) ->
    fun(Mod) -> not lists:member(Mod, Loaded) end.

load_plugin(Src) ->
    case compile:file(Src, [binary, return_errors]) of
        {ok, Mod, Bin} ->
            load_plugin_module(Mod, Bin, Src);
        {error, Errors, _Warnings} ->
            ?ABORT("Plugin ~s contains compilation errors: ~p~n",
                   [Src, Errors])
    end.

load_plugin_module(Mod, Bin, Src) ->
    case code:is_loaded(Mod) of
        {file, Loaded} ->
            ?ABORT("Plugin ~p clashes with previously loaded module ~p~n",
                   [Mod, Loaded]);
        false ->
            ?INFO("Loading plugin ~p from ~s~n", [Mod, Src]),
            {module, Mod} = code:load_binary(Mod, Src, Bin),
            Mod
    end.
