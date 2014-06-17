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

-export([process_commands/2, help/2]).

-include("rebar.hrl").

%% ===================================================================
%% Internal functions
%% ===================================================================

help(ParentConfig, Commands) ->
    %% get all core modules
    {ok, AnyDirModules} = application:get_env(rebar, any_dir_modules),
    {ok, RawCoreModules} = application:get_env(rebar, modules),
    AppDirModules = proplists:get_value(app_dir, RawCoreModules),
    RelDirModules = proplists:get_value(rel_dir, RawCoreModules),
    CoreModules = AnyDirModules ++ AppDirModules ++ RelDirModules,

    %% get plugin modules
    Predirs = [],
    Dir = rebar_utils:get_cwd(),
    PredirsAssoc = remember_cwd_predirs(Dir, Predirs),
    Config = maybe_load_local_config(Dir, ParentConfig),
    {ok, PluginModules} = plugin_modules(Config, PredirsAssoc),

    AllModules = CoreModules ++ PluginModules,

    lists:foreach(
      fun(Cmd) ->
              ?CONSOLE("==> help ~p~n~n", [Cmd]),
              CmdModules = select_modules(AllModules, Cmd, []),
              Modules = select_modules(CmdModules, info, []),
              lists:foreach(fun(M) ->
                                    ?CONSOLE("=== ~p:~p ===~n", [M, Cmd]),
                                    M:info(help, Cmd),
                                    ?CONSOLE("~n", [])
                            end, Modules)
      end, Commands).

process_commands([], ParentConfig) ->
    AbortTrapped = rebar_config:get_xconf(ParentConfig, abort_trapped, false),
    case {get_operations(ParentConfig), AbortTrapped} of
        {0, _} ->
            %% None of the commands had any effect
            ?FAIL;
        {_, true} ->
            %% An abort was previously trapped
            ?FAIL;
        _ ->
            ok
    end;
process_commands([Command | Rest], ParentConfig) ->
    %% Reset skip dirs
    ParentConfig1 = rebar_config:reset_skip_dirs(ParentConfig),
    Operations = get_operations(ParentConfig1),

    ParentConfig4 =
        try
            %% Convert the code path so that all the entries are
            %% absolute paths. If not, code:set_path() may choke on
            %% invalid relative paths when trying to restore the code
            %% path from inside a subdirectory.
            true = rebar_utils:expand_code_path(),
            {ParentConfig2, _DirSet} = process_dir(rebar_utils:get_cwd(),
                                                   Command, ParentConfig1,
                                                   sets:new()),
            case get_operations(ParentConfig2) of
                Operations ->
                    %% This command didn't do anything
                    ?CONSOLE("Command '~p' not understood or not applicable~n",
                             [Command]);
                _ ->
                    ok
            end,
            %% TODO: reconsider after config inheritance removal/re-design
            ParentConfig3 = rebar_config:clean_config(ParentConfig1,
                                                      ParentConfig2),
            %% Wipe out vsn cache to avoid invalid hits when
            %% dependencies are updated
            rebar_config:set_xconf(ParentConfig3, vsn_cache, dict:new())
        catch
            throw:rebar_abort ->
                case rebar_config:get_xconf(ParentConfig1, keep_going, false) of
                    false ->
                        ?FAIL;
                    true ->
                        ?WARN("Continuing on after abort: ~p\n", [Rest]),
                        rebar_config:set_xconf(ParentConfig1,
                                               abort_trapped, true)
                end
        end,
    process_commands(Rest, ParentConfig4).

process_dir(Dir, Command, ParentConfig, DirSet) ->
    case filelib:is_dir(Dir) of
        false ->
            ?WARN("Skipping non-existent sub-dir: ~p\n", [Dir]),
            {ParentConfig, DirSet};
        true ->
            WouldCd = would_cd_into_dir(Dir, Command, ParentConfig),
            ok = file:set_cwd(Dir),
            Config = maybe_load_local_config(Dir, ParentConfig),

            %% Save the current code path and then update it with
            %% lib_dirs. Children inherit parents code path, but we also
            %% want to ensure that we restore everything to pristine
            %% condition after processing this child
            CurrentCodePath = update_code_path(Config),

            %% Get the list of processing modules and check each one
            %% against CWD to see if it's a fit -- if it is, use that
            %% set of modules to process this dir.
            {ok, AvailModuleSets} = application:get_env(rebar, modules),
            ModuleSet = choose_module_set(AvailModuleSets, Dir),
            skip_or_process_dir(Dir, Command, Config, DirSet, CurrentCodePath,
                                ModuleSet, WouldCd)
    end.

would_cd_into_dir(Dir, Command, Config) ->
    case would_cd_into_dir1(Dir, Command, Config) of
        true ->
            would_cd;
        false ->
            would_not_cd
    end.

would_cd_into_dir1(Dir, Command, Config) ->
    rebar_utils:processing_base_dir(Config, Dir) orelse
        rebar_config:is_recursive(Config) orelse
        is_recursive_command(Command, Config) orelse
        is_generate_in_rel_dir(Command, Dir).

%% Check whether the command is part of the built-in (or extended via
%% rebar.config) list of default-recursive commands.
is_recursive_command(Command, Config) ->
    {ok, AppCmds} = application:get_env(rebar, recursive_cmds),
    ConfCmds = rebar_config:get_local(Config, recursive_cmds, []),
    RecursiveCmds = AppCmds ++ ConfCmds,
    lists:member(Command, RecursiveCmds).

%% If the directory we're about to process contains
%% reltool.config[.script] and the command to be applied is
%% 'generate', then it's safe to process. We do this to retain the
%% behavior of specifying {sub_dirs, ["rel"]} and have "rebar generate"
%% pick up rel/reltool.config[.script]. Without this workaround you'd
%% have to run "rebar -r generate" (which you don't want to do if you
%% have deps or other sub_dirs) or "cd rel && rebar generate".
is_generate_in_rel_dir(generate, Dir) ->
    case rebar_rel_utils:is_rel_dir(Dir) of
        {true, _} ->
            true;
        false ->
            false
    end;
is_generate_in_rel_dir(_, _) ->
    false.

skip_or_process_dir(Dir, Command, Config, DirSet, CurrentCodePath,
                    {[], undefined}=ModuleSet, WouldCd) ->
    process_dir1(Dir, Command, Config, DirSet, CurrentCodePath, ModuleSet,
                 WouldCd);
skip_or_process_dir(Dir, Command, Config, DirSet, CurrentCodePath,
                    {_, File}=ModuleSet, WouldCd) ->
    case lists:suffix(".app.src", File)
        orelse lists:suffix(".app", File) of
        true ->
            %% .app or .app.src file, check if is_skipped_app
            skip_or_process_dir1(Dir, Command, Config, DirSet, CurrentCodePath,
                                 ModuleSet, WouldCd, File);
        false ->
            %% not an app dir, no need to consider apps=/skip_apps=
            process_dir1(Dir, Command, Config, DirSet, CurrentCodePath,
                         ModuleSet, WouldCd)
    end.

skip_or_process_dir1(Dir, Command, Config, DirSet, CurrentCodePath, ModuleSet,
                     WouldCd, AppFile) ->
    case rebar_app_utils:is_skipped_app(Config, AppFile) of
        {Config1, {true, _SkippedApp}} when Command == 'update-deps' ->
            %% update-deps does its own app skipping. Unfortunately there's no
            %% way to signal this to rebar_core, so we have to explicitly do it
            %% here... Otherwise if you use app=, it'll skip the toplevel
            %% directory and nothing will be updated.
            process_dir1(Dir, Command, Config1, DirSet, CurrentCodePath,
                         ModuleSet, WouldCd);
        {Config1, {true, SkippedApp}} ->
            ?DEBUG("Skipping app: ~p~n", [SkippedApp]),
            {increment_operations(Config1), DirSet};
        {Config1, false} ->
            process_dir1(Dir, Command, Config1, DirSet, CurrentCodePath,
                         ModuleSet, WouldCd)
    end.

process_dir1(Dir, Command, Config, DirSet, CurrentCodePath,
             {DirModules, File}, WouldCd) ->
    Config0 = rebar_config:set_xconf(Config, current_command, Command),

    %% Get the list of modules for "any dir". This is a catch-all list
    %% of modules that are processed in addition to modules associated
    %% with this directory type. These any_dir modules are processed
    %% FIRST.
    {ok, AnyDirModules} = application:get_env(rebar, any_dir_modules),

    Modules = AnyDirModules ++ DirModules,

    %% Invoke 'preprocess' on the modules -- this yields a list of other
    %% directories that should be processed _before_ the current one.
    {Config1, Predirs} = acc_modules(Modules, preprocess, Config0, File),

    %% Remember associated pre-dirs (used for plugin lookup)
    PredirsAssoc = remember_cwd_predirs(Dir, Predirs),

    %% Get the list of plug-in modules from rebar.config. These
    %% modules may participate in preprocess and postprocess.
    {ok, PluginModules} = plugin_modules(Config1, PredirsAssoc),
    AllModules = Modules ++ PluginModules,

    {Config2, PluginPredirs} = acc_modules(PluginModules, preprocess, Config1,
                                           File),

    AllPredirs = Predirs ++ PluginPredirs,

    ?DEBUG("Predirs: ~p\n", [AllPredirs]),
    {Config3, DirSet2} = process_each(AllPredirs, Command, Config2, DirSet,
                                      File),

    %% Make sure the CWD is reset properly; processing the dirs may have
    %% caused it to change
    ok = file:set_cwd(Dir),

    %% Maybe apply command to Dir
    Config4 = maybe_execute(Dir, Command, Config3, Modules, PluginModules,
                            AllModules, File, WouldCd),

    %% Mark the current directory as processed
    DirSet3 = sets:add_element(Dir, DirSet2),

    %% Invoke 'postprocess' on the modules. This yields a list of other
    %% directories that should be processed _after_ the current one.
    {Config5, Postdirs} = acc_modules(AllModules, postprocess, Config4, File),
    ?DEBUG("Postdirs: ~p\n", [Postdirs]),
    Res = process_each(Postdirs, Command, Config5, DirSet3, File),

    %% Make sure the CWD is reset properly; processing the dirs may have
    %% caused it to change
    ok = file:set_cwd(Dir),

    %% Once we're all done processing, reset the code path to whatever
    %% the parent initialized it to
    restore_code_path(CurrentCodePath),

    %% Return the updated {config, dirset} as result
    Res.

maybe_execute(Dir, Command, Config, Modules, PluginModules, AllModules, File,
              would_cd) ->
    %% Check that this directory is not on the skip list
    case rebar_config:is_skip_dir(Config, Dir) of
        true ->
            %% Do not execute the command on the directory, as some
            %% module has requested a skip on it.
            ?INFO("Skipping ~s in ~s\n", [Command, Dir]),
            Config;

        false ->
            %% Check for and get command specific environments
            {Config1, Env} = setup_envs(Config, Modules),

            %% Execute any before_command plugins on this directory
            Config2 = execute_pre(Command, PluginModules, Config1, File, Env),

            %% Execute the current command on this directory
            Config3 = execute(Command, AllModules, Config2, File, Env),

            %% Execute any after_command plugins on this directory
            execute_post(Command, PluginModules, Config3, File, Env)
    end;
maybe_execute(_Dir, _Command, Config, _Modules, _PluginModules, _AllModules,
              _File, would_not_cd) ->
    Config.

remember_cwd_predirs(Cwd, Predirs) ->
    Store = fun(Dir, Dict) ->
                    case dict:find(Dir, Dict) of
                        error ->
                            ?DEBUG("Associate sub_dir ~s with ~s~n",
                                   [Dir, Cwd]),
                            dict:store(Dir, Cwd, Dict);
                        {ok, Existing} ->
                            ?ABORT("Internal consistency assertion failed.~n"
                                   "sub_dir ~s already associated with ~s.~n"
                                   "Duplicate sub_dirs or deps entries?",
                                   [Dir, Existing])
                    end
            end,
    lists:foldl(Store, dict:new(), Predirs).

maybe_load_local_config(Dir, ParentConfig) ->
    %% We need to ensure we don't overwrite custom
    %% config when we are dealing with base_dir.
    case rebar_utils:processing_base_dir(ParentConfig, Dir) of
        true ->
            ParentConfig;
        false ->
            rebar_config:new(ParentConfig)
    end.

%%
%% Given a list of directories and a set of previously processed directories,
%% process each one we haven't seen yet
%%
process_each([], _Command, Config, DirSet, _File) ->
    %% reset cached (setup_env) envs
    Config1 = rebar_config:reset_envs(Config),
    {Config1, DirSet};
process_each([Dir | Rest], Command, Config, DirSet, File) ->
    case sets:is_element(Dir, DirSet) of
        true ->
            ?DEBUG("Skipping ~s; already processed!\n", [Dir]),
            process_each(Rest, Command, Config, DirSet, File);
        false ->
            {Config1, DirSet2} = process_dir(Dir, Command, Config, DirSet),
            Config2 = rebar_config:clean_config(Config, Config1),
            %% reset cached (setup_env) envs
            Config3 = rebar_config:reset_envs(Config2),
            process_each(Rest, Command, Config3, DirSet2, File)
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
    execute(HookFunction, hook, Modules, Config, ModuleFile, Env).

%%
%% Execute a command across all applicable modules
%%
execute(Command, Modules, Config, ModuleFile, Env) ->
    execute(Command, not_a_hook, Modules, Config, ModuleFile, Env).

execute(Command, Type, Modules, Config, ModuleFile, Env) ->
    case select_modules(Modules, Command, []) of
        [] ->
            case Type of
                hook ->
                    ok;
                not_a_hook ->
                    ?WARN("'~p' command does not apply to directory ~s\n",
                          [Command, rebar_utils:get_cwd()])
            end,
            Config;

        TargetModules ->
            %% Provide some info on where we are
            Dir = rebar_utils:get_cwd(),
            ?CONSOLE("==> ~s (~s)\n", [filename:basename(Dir), Command]),

            Config1 = increment_operations(Config),

            %% Run the available modules
            apply_hooks(pre_hooks, Config1, Command, Env),
            case catch(run_modules(TargetModules, Command,
                                   Config1, ModuleFile)) of
                {ok, NewConfig} ->
                    apply_hooks(post_hooks, NewConfig, Command, Env),
                    NewConfig;
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
increment_operations(Config) ->
    Operations = get_operations(Config),
    rebar_config:set_xconf(Config, operations, Operations + 1).

get_operations(Config) ->
    rebar_config:get_xconf(Config, operations).

update_code_path(Config) ->
    case rebar_config:get_local(Config, lib_dirs, []) of
        [] ->
            no_change;
        Paths ->
            LibPaths = expand_lib_dirs(Paths, rebar_utils:get_cwd(), []),
            ok = code:add_pathsa(LibPaths),
            %% track just the paths we added, so we can remove them without
            %% removing other paths added by this dep
            {added, LibPaths}
    end.

restore_code_path(no_change) ->
    ok;
restore_code_path({added, Paths}) ->
    %% Verify that all of the paths still exist -- some dynamically
    %% added paths can get blown away during clean.
    _ = [code:del_path(F) || F <- Paths, erl_prim_loader_is_file(F)],
    ok.

erl_prim_loader_is_file(File) ->
    erl_prim_loader:read_file_info(File) =/= error.

expand_lib_dirs([], _Root, Acc) ->
    Acc;
expand_lib_dirs([Dir | Rest], Root, Acc) ->
    Apps = filelib:wildcard(filename:join([Dir, "*", "ebin"])),
    FqApps = case filename:pathtype(Dir) of
                 absolute -> Apps;
                 _        -> [filename:join([Root, A]) || A <- Apps]
             end,
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

run_modules([], _Command, Config, _File) ->
    {ok, Config};
run_modules([Module | Rest], Command, Config, File) ->
    case Module:Command(Config, File) of
        ok ->
            run_modules(Rest, Command, Config, File);
        {ok, NewConfig} ->
            run_modules(Rest, Command, NewConfig, File);
        {error, _} = Error ->
            {Module, Error}
    end.

apply_hooks(Mode, Config, Command, Env0) ->
    Hooks = rebar_config:get_local(Config, Mode, []),
    Env = rebar_utils:patch_env(Config, Env0),
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
                                C1 = rebar_config:save_env(C, M, Env),
                                {C1, E++Env};
                            false ->
                                T
                        end
                end, {Config, []}, Modules).

acc_modules(Modules, Command, Config, File) ->
    acc_modules(select_modules(Modules, Command, []),
                Command, Config, File, []).

acc_modules([], _Command, Config, _File, Acc) ->
    {Config, Acc};
acc_modules([Module | Rest], Command, Config, File, Acc) ->
    {Config1, Dirs1} = case Module:Command(Config, File) of
                           {ok, Dirs} ->
                               {Config, Dirs};
                           {ok, NewConfig, Dirs} ->
                               {NewConfig, Dirs}
                       end,
    acc_modules(Rest, Command, Config1, File, Acc ++ Dirs1).

%%
%% Return a flat list of rebar plugin modules.
%%
plugin_modules(Config, PredirsAssoc) ->
    Modules = lists:flatten(rebar_config:get_all(Config, plugins)),
    ?DEBUG("Plugins requested while processing ~s: ~p~n",
           [rebar_utils:get_cwd(), Modules]),
    plugin_modules(Config, PredirsAssoc, ulist(Modules)).

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

plugin_modules(_Config, _PredirsAssoc, []) ->
    {ok, []};
plugin_modules(Config, PredirsAssoc, Modules) ->
    FoundModules = [M || M <- Modules, code:which(M) =/= non_existing],
    plugin_modules(Config, PredirsAssoc, FoundModules, Modules -- FoundModules).

plugin_modules(_Config, _PredirsAssoc, FoundModules, []) ->
    {ok, FoundModules};
plugin_modules(Config, PredirsAssoc, FoundModules, MissingModules) ->
    {Loaded, NotLoaded} = load_plugin_modules(Config, PredirsAssoc,
                                              MissingModules),
    AllViablePlugins = FoundModules ++ Loaded,
    case NotLoaded =/= [] of
        true ->
            %% NB: we continue to ignore this situation, as did the
            %% original code
            ?WARN("Missing plugins: ~p\n", [NotLoaded]);
        false ->
            ?DEBUG("Loaded plugins: ~p~n", [AllViablePlugins]),
            ok
    end,
    {ok, AllViablePlugins}.

load_plugin_modules(Config, PredirsAssoc, Modules) ->
    Cwd = rebar_utils:get_cwd(),
    PluginDirs = get_all_plugin_dirs(Config, Cwd, PredirsAssoc),
    ?DEBUG("Plugin dirs for ~s:~n~p~n", [Cwd, PluginDirs]),

    %% Find relevant sources in base_dir and plugin_dir
    RE = string:join([atom_to_list(M)++"\\.erl" || M <- Modules], "|"),
    %% If a plugin is found both in base_dir and plugin_dir, the clash
    %% will provoke an error and we'll abort.
    Sources = [rebar_utils:find_files(PD, RE, false) || PD <- PluginDirs],

    %% Compile and load plugins
    Loaded = [load_plugin(Src) || Src <- lists:append(Sources)],
    FilterMissing = is_missing_plugin(Loaded),
    NotLoaded = [V || V <- Modules, FilterMissing(V)],
    {Loaded, NotLoaded}.

get_all_plugin_dirs(Config, Cwd, PredirsAssoc) ->
    [rebar_utils:get_cwd()]
        ++ get_plugin_dir(Config, Cwd)
        ++ get_base_plugin_dirs(Cwd, PredirsAssoc).

get_plugin_dir(Config, Cwd) ->
    case rebar_config:get_local(Config, plugin_dir, undefined) of
        undefined ->
            %% Plugin can be in the project's "plugins" folder
            [filename:join(Cwd, "plugins")];
        Dir ->
            [Dir]
    end.

%% We also want to include this case:
%% Plugin can be in "plugins" directory of the plugin base directory.
%% For example, Cwd depends on Plugin, and deps/Plugin/plugins/Plugin.erl
%% is the plugin.
get_base_plugin_dirs(Cwd, PredirsAssoc) ->
    [filename:join(Dir, "plugins") ||
        Dir <- get_plugin_base_dirs(Cwd, PredirsAssoc)].

%% @doc PredirsAssoc is a dictionary of plugindir -> 'parent' pairs.
%% 'parent' in this case depends on plugin; therefore we have to give
%% all plugins that Cwd ('parent' in this case) depends on.
get_plugin_base_dirs(Cwd, PredirsAssoc) ->
    [PluginDir || {PluginDir, Master} <- dict:to_list(PredirsAssoc),
                  Master =:= Cwd].

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
