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
-module(rebar_core).

-export([run/1,
         skip_dir/1,
         is_skip_dir/1,
         skip_dirs/0]).

-include("rebar.hrl").


-ifndef(BUILD_TIME).
-define(BUILD_TIME, "undefined").
-endif.

-ifndef(VCS_INFO).
-define(VCS_INFO, "undefined").
-endif.

%% ===================================================================
%% Public API
%% ===================================================================

run(["help"]) ->
    help(),
    ok;
run(["version"]) ->
    %% Load application spec and display vsn and build time info
    ok = application:load(rebar),
    version(),
    ok;
run(RawArgs) ->
    %% Pre-load the rebar app so that we get default configuration
    ok = application:load(rebar),

    %% Parse out command line arguments -- what's left is a list of commands to
    %% run
    Commands = parse_args(RawArgs),

    %% Make sure crypto is running
    ok = crypto:start(),

    %% Initialize logging system
    rebar_log:init(),

    %% Convert command strings to atoms
    CommandAtoms = [list_to_atom(C) || C <- Commands],

    %% Determine the location of the rebar executable; important for pulling
    %% resources out of the escript
    rebar_config:set_global(escript, filename:absname(escript:script_name())),
    ?DEBUG("Rebar location: ~p\n", [rebar_config:get_global(escript, undefined)]),

    %% Note the top-level directory for reference
    rebar_config:set_global(base_dir, filename:absname(rebar_utils:get_cwd())),

    %% Keep track of how many operations we do, so we can detect bad commands
    erlang:put(operations, 0),

    %% Process each command, resetting any state between each one
    process_commands(CommandAtoms).

skip_dir(Dir) ->
    case erlang:get({skip_dir, Dir}) of
        undefined ->
            ?DEBUG("Adding skip dir: ~s\n", [Dir]),
            erlang:put({skip_dir, Dir}, true);
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

%%
%% Parse command line arguments using getopt and also filtering out any
%% key=value pairs. What's left is the list of commands to run
%%
parse_args(Args) ->
    %% Parse getopt options
    OptSpecList = option_spec_list(),
    case getopt:parse(OptSpecList, Args) of
        {ok, {Options, NonOptArgs}} ->
            %% Check options and maybe halt execution
            {ok, continue} = show_info_maybe_halt(Options, NonOptArgs),

            %% Set global variables based on getopt options
            set_global_flag(Options, verbose),
            set_global_flag(Options, force),
            DefJobs = rebar_config:get_jobs(),
            case proplists:get_value(jobs, Options, DefJobs) of
                DefJobs ->
                    ok;
                Jobs ->
                    rebar_config:set_global(jobs, Jobs)
            end,

            %% Filter all the flags (i.e. strings of form key=value) from the
            %% command line arguments. What's left will be the commands to run.
            filter_flags(NonOptArgs, []);

        {error, {Reason, Data}} ->
            ?ERROR("Error: ~s ~p~n~n", [Reason, Data]),
            help(),
            halt(1)
    end.

%%
%% set global flag based on getopt option boolean value
%%
set_global_flag(Options, Flag) ->
    Value = case proplists:get_bool(Flag, Options) of
                true ->
                    "1";
                false ->
                    "0"
            end,
    rebar_config:set_global(Flag, Value).

%%
%% show info and maybe halt execution
%%
show_info_maybe_halt(Opts, NonOptArgs) ->
    case proplists:get_bool(help, Opts) of
        true ->
            help(),
            halt(0);
        false ->
            case proplists:get_bool(commands, Opts) of
                true ->
                    commands(),
                    halt(0);
                false ->
                    case proplists:get_bool(version, Opts) of
                        true ->
                            version(),
                            halt(0);
                        false ->
                            case NonOptArgs of
                                [] ->
                                    ?CONSOLE("No command to run specified!~n",[]),
                                    help(),
                                    halt(1);
                                _ ->
                                    {ok, continue}
                            end
                    end
            end
    end.

%%
%% print help/usage string
%%
help() ->
    OptSpecList = option_spec_list(),
    getopt:usage(OptSpecList, "rebar",
                 "[var=value,...] <command,...>",
                 [{"var=value", "rebar global variables (e.g. force=1)"},
                  {"command", "Command to run (e.g. compile)"}]).

%%
%% print known commands
%%
commands() ->
    S = <<"
analyze                              Analyze with Dialyzer
build_plt                            Build Dialyzer PLT
check_plt                            Check Dialyzer PLT

clean                                Clean
compile                              Compile sources

create      template= [var=foo,...]  Create skel based on template and vars
create-app  [appid=myapp]            Create simple app skel
create-node [nodeid=mynode]          Create simple node skel
list-templates                       List available templates

doc                                  Generate Erlang program documentation

check-deps                           Display to be fetched dependencies
get-deps                             Fetch dependencies
delete-deps                          Delete fetched dependencies

generate    [dump_spec=0/1]          Build release with reltool

eunit       [suite=foo]              Run eunit [test/foo_tests.erl] tests
ct          [suite=] [case=]         Run common_test suites in ./test

xref                                 Run cross reference analysis

help                                 Show the program options
version                              Show version information
">>,
    io:put_chars(S),
    %% workaround to delay exit until all output is written
    timer:sleep(300).

%%
%% show version information and halt
%%
version() ->
    {ok, Vsn} = application:get_key(rebar, vsn),
    ?CONSOLE("rebar version: ~s date: ~s vcs: ~s\n", [Vsn, ?BUILD_TIME, ?VCS_INFO]).

%%
%% options accepted via getopt
%%
option_spec_list() ->
    Jobs = rebar_config:get_jobs(),
    JobsHelp = io_lib:format(
        "Number of concurrent workers a command may use. Default: ~B",
        [Jobs]),
    [
     %% {Name, ShortOpt, LongOpt, ArgSpec, HelpMsg}
     {help,     $h, "help",     undefined, "Show the program options"},
     {commands, $c, "commands", undefined, "Show available commands"},
     {verbose,  $v, "verbose",  undefined, "Be verbose about what gets done"},
     {version,  $V, "version",  undefined, "Show version information"},
     {force,    $f, "force",    undefined, "Force"},
     {jobs,     $j, "jobs",     integer,   JobsHelp}
    ].

%%
%% Seperate all commands (single-words) from flags (key=value) and store
%% values into the rebar_config global storage.
%%
filter_flags([], Commands) ->
    lists:reverse(Commands);
filter_flags([Item | Rest], Commands) ->
    case string:tokens(Item, "=") of
        [Command] ->
            filter_flags(Rest, [Command | Commands]);
        [KeyStr, Value] ->
            Key = list_to_atom(KeyStr),
            rebar_config:set_global(Key, Value),
            filter_flags(Rest, Commands);
        Other ->
            ?CONSOLE("Ignoring command line argument: ~p\n", [Other]),
            filter_flags(Rest, Commands)
    end.

process_commands([]) ->
    case erlang:get(operations) of
        0 ->
            %% none of the commands had an effect
            ?FAIL;
        _ ->
            ok
    end;
process_commands([Command | Rest]) ->
    %% Reset skip dirs
    lists:foreach(fun (D) -> erlang:erase({skip_dir, D}) end, skip_dirs()),
    Operations = erlang:get(operations),

    _ = process_dir(rebar_utils:get_cwd(), rebar_config:new(), Command, sets:new()),
    case erlang:get(operations) of
        Operations ->
            %% This command didn't do anything
            ?CONSOLE("Command '~p' not understood\n", [Command]);
        _ ->
            ok
    end,
    process_commands(Rest).


process_dir(Dir, ParentConfig, Command, DirSet) ->
    case filelib:is_dir(Dir) of
        false ->
            ?WARN("Skipping non-existent sub-dir: ~p\n", [Dir]),
            DirSet;

        true ->
            ?DEBUG("Entering ~s\n", [Dir]),
            ok = file:set_cwd(Dir),
            Config = rebar_config:new(ParentConfig),

            %% Save the current code path and then update it with
            %% lib_dirs. Children inherit parents code path, but we
            %% also want to ensure that we restore everything to pristine
            %% condition after processing this child
            CurrentCodePath = update_code_path(Config),

            %% Get the list of processing modules and check each one against
            %% CWD to see if it's a fit -- if it is, use that set of modules
            %% to process this dir.
            {ok, AvailModuleSets} = application:get_env(rebar, modules),
            {DirModules, ModuleSetFile} = choose_module_set(AvailModuleSets, Dir),

            %% Get the list of modules for "any dir". This is a catch-all list
            %% of modules that are processed in addition to modules associated
            %% with this directory type. These any_dir modules are processed
            %% FIRST.
            {ok, AnyDirModules} = application:get_env(rebar, any_dir_modules),

            Modules = AnyDirModules ++ DirModules,

            %% Invoke 'preprocess' on the modules -- this yields a list of other
            %% directories that should be processed _before_ the current one.
            Predirs = acc_modules(Modules, preprocess, Config, ModuleSetFile),
            ?DEBUG("Predirs: ~p\n", [Predirs]),
            DirSet2 = process_each(Predirs, Command, Config, ModuleSetFile, DirSet),

            %% Make sure the CWD is reset properly; processing the dirs may have
            %% caused it to change
            ok = file:set_cwd(Dir),

            %% Check that this directory is not on the skip list
            case is_skip_dir(Dir) of
                true ->
                    %% Do not execute the command on the directory, as some module
                    %% as requested a skip on it.
                    ?INFO("Skipping ~s in ~s\n", [Command, Dir]);

                false ->
                    %% Get the list of plug-in modules from rebar.config. These modules are
                    %% processed LAST and do not participate in preprocess.
                    {ok, PluginModules} = plugin_modules(Config),

                    %% Execute the current command on this directory
                    execute(Command, Modules ++ PluginModules, Config, ModuleSetFile)
            end,

            %% Mark the current directory as processed
            DirSet3 = sets:add_element(Dir, DirSet2),

            %% Invoke 'postprocess' on the modules -- this yields a list of other
            %% directories that should be processed _after_ the current one.
            Postdirs = acc_modules(Modules, postprocess, Config, ModuleSetFile),
            ?DEBUG("Postdirs: ~p\n", [Postdirs]),
            DirSet4 = process_each(Postdirs, Command, Config, ModuleSetFile, DirSet3),

            %% Make sure the CWD is reset properly; processing the dirs may have
            %% caused it to change
            ok = file:set_cwd(Dir),

            %% Once we're all done processing, reset the code path to whatever
            %% the parent initialized it to
            restore_code_path(CurrentCodePath),

            %% Return the updated dirset as our result
            DirSet4
    end.



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


%%
%% Execute a command across all applicable modules
%%
execute(Command, Modules, Config, ModuleFile) ->
    case select_modules(Modules, Command, []) of
        [] ->
            ?WARN("'~p' command does not apply to directory ~s\n",
                     [Command, rebar_utils:get_cwd()]);

        TargetModules ->
            %% Provide some info on where we are
            Dir = rebar_utils:get_cwd(),
            ?CONSOLE("==> ~s (~s)\n", [filename:basename(Dir), Command]),

            %% Increment the count of operations, since some module responds to this command
            erlang:put(operations, erlang:get(operations) + 1),

            %% Run the available modules
            case catch(run_modules(TargetModules, Command, Config, ModuleFile)) of
                ok ->
                    ok;
                {error, failed} ->
                    ?FAIL;
                Other ->
                    ?ABORT("~p failed while processing ~s: ~s",
                           [Command, Dir, io_lib:print(Other, 1,80,-1)])
            end
    end.


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
    %% Verify that all of the paths still exist -- some dynamically add paths
    %% can get blown away during clean.
    true = code:set_path(lists:filter(fun filelib:is_file/1, Path)),
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
    Exports = Module:module_info(exports),
    case lists:member({Command, 2}, Exports) of
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
        {error, Reason} ->
            {error, Reason}
    end.


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
plugin_modules(Config) ->
    Modules = lists:flatten(rebar_config:get_all(Config, rebar_plugins)),
    plugin_modules(Config, ulist(Modules)).

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

plugin_modules(_Config, []) ->
    {ok, []};
plugin_modules(_Config, Modules) ->
    FoundModules = [M || M <- Modules, code:which(M) =/= non_existing],
    case (Modules =:= FoundModules) of
        true ->
            ok;
        false ->
            ?WARN("Missing plugins: ~p\n", [Modules -- FoundModules]),
            ok
    end,
    {ok, FoundModules}.
