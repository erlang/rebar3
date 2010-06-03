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

-export([run/1]).

-export([app_dir/1, rel_dir/1]). % Ugh

-include("rebar.hrl").


-ifndef(BUILD_TIME).
-define(BUILD_TIME, "undefined").
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
    crypto:start(),

    %% Initialize logging system
    rebar_log:init(),

    %% Convert command strings to atoms
    CommandAtoms = [list_to_atom(C) || C <- Commands],

    %% Determine the location of the rebar executable; important for pulling
    %% resources out of the escript
    rebar_config:set_global(escript, filename:absname(escript:script_name())),
    ?DEBUG("Rebar location: ~p\n", [rebar_config:get_global(escript, undefined)]),

    %% Load rebar.config, if it exists
    [process_dir(rebar_utils:get_cwd(), rebar_config:new(), Command)
     || Command <- CommandAtoms],
    ok.


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

check-deps                           Display to be fetched dependencies
get-deps                             Fetch dependencies
delete-deps                          Delete fetched dependencies

generate    [dump_spec=0/1]          Build release with reltool
install     [target=]                Install build into target

eunit       [suite=foo]              Run eunit [test/foo_tests.erl] tests

int_test    [suite=] [case=]         Run ct suites in ./int_test
perf_test   [suite=] [case=]         Run ct suites in ./perf_test
test        [suite=] [case=]         Run ct suites in ./test

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
    ?CONSOLE("Version ~s built ~s\n", [Vsn, ?BUILD_TIME]).

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


process_dir(Dir, ParentConfig, Command) ->
    case filelib:is_dir(Dir) of
        false ->
            ?WARN("Skipping non-existent sub-dir: ~p\n", [Dir]),
            ok;

        true ->
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

            %% Get the list of modules for "any dir". This is a catch-all list of modules
            %% that are processed in addition to modules associated with this directory
            %% type. These any_dir modules are processed FIRST.
            {ok, AnyDirModules} = application:get_env(rebar, any_dir_modules),

            Modules = AnyDirModules ++ DirModules,

            ok = process_subdirs(Dir, Modules, Config, ModuleSetFile, Command),

            %% Once we're all done processing, reset the code path to whatever
            %% the parent initialized it to
            restore_code_path(CurrentCodePath),
            ok
    end.


%%
%% Run the preprocessors and execute the command on all newly
%% found Dirs until no new Dirs are found by the preprocessors.
%%
process_subdirs(Dir, Modules, Config, ModuleSetFile, Command) ->
    process_subdirs(Dir, Modules, Config, ModuleSetFile, Command, sets:new()).

process_subdirs(Dir, Modules, Config, ModuleSetFile, Command, ProcessedDirs) ->
    %% Give the modules a chance to tweak config and indicate if there
    %% are any other dirs that might need processing first.
    {UpdatedConfig, Dirs} = acc_modules(Modules, preprocess, Config, ModuleSetFile),
    ?DEBUG("~s subdirs: ~p\n", [Dir, Dirs]),

    %% Add ebin to path if this app has any plugins configured locally.
    prep_plugin_modules(UpdatedConfig),

    %% Process subdirs that haven't already been processed.
    F = fun (D, S) ->
                case filelib:is_dir(D) andalso (not sets:is_element(D, S)) of
                    true ->
                        process_dir(D, UpdatedConfig, Command),
                        sets:add_element(D, S);
                    false ->
                        S
                end
        end,
    NewProcessedDirs = lists:foldl(F, sets:add_element(parent, ProcessedDirs), Dirs),

    %% http://bitbucket.org/basho/rebar/issue/5
    %% If the compiler ran, run the preprocess again because a new ebin dir
    %% may have been produced.
    {UpdatedConfig1, _} = case (Dirs =/= [] andalso compile == Command) of
                              true ->
                                  acc_modules(Modules, preprocess, UpdatedConfig,
                                              ModuleSetFile);
                              false ->
                                  {UpdatedConfig, Dirs}
                          end,

    %% Make sure the CWD is reset properly; processing subdirs may have caused it
    %% to change
    ok = file:set_cwd(Dir),

    %% Run the parent commands exactly once as well
    case sets:is_element(parent, ProcessedDirs) of
        true ->
            ok;
        false ->
            %% Get the list of plug-in modules from rebar.config. These modules are
            %% processed LAST and do not participate in preprocess.
            {ok, PluginModules} = plugin_modules(UpdatedConfig1),

            %% Finally, process the current working directory
            ?DEBUG("Command: ~p Modules: ~p Plugins: ~p\n", [Command, Modules, PluginModules]),
            apply_command(Command, Modules ++ PluginModules, UpdatedConfig1, ModuleSetFile)
    end,

    %% Repeat the process if there are new SeenDirs
    case NewProcessedDirs =:= ProcessedDirs of
        true ->
            ok;
        false ->
            process_subdirs(Dir, Modules, UpdatedConfig1, ModuleSetFile, Command,
                            NewProcessedDirs)
    end.

%%
%% Given a list of module sets from rebar.app and a directory, find
%% the appropriate subset of modules for this directory
%%
choose_module_set([], _Dir) ->
    {[], undefined};
choose_module_set([{Fn, Modules} | Rest], Dir) ->
    case ?MODULE:Fn(Dir) of
        {true, File} ->
            {Modules, File};
        false ->
            choose_module_set(Rest, Dir)
    end.

%%
%% Add ebin to path if there are any local plugin modules for this app.
%%
prep_plugin_modules(Config) ->
    case rebar_config:get_local(Config, rebar_plugins, []) of
        [_H | _T] ->
            code:add_path(filename:join([rebar_utils:get_cwd(), "ebin"]));
        _ ->
            ok
    end.

%%
%% Return a flat list of rebar plugin modules.
%%
plugin_modules(Config) ->
    Modules = lists:flatten(rebar_config:get_all(Config, rebar_plugins)),
    plugin_modules(Config, ulist(Modules)).

ulist(L) ->
    ulist(L, sets:new(), []).

ulist([], _S, Acc) ->
    lists:reverse(Acc);
ulist([H | T], S, Acc) ->
    case sets:is_element(H, S) of
        true ->
            ulist(T, S, Acc);
        false ->
            ulist(T, sets:add_element(H, S), [H | Acc])
    end.

plugin_modules(_Config, []) ->
    {ok, []};
plugin_modules(_Config, Modules) ->
    FoundModules = [M || M <- Modules, code:which(M) =/= non_existing],
    case (Modules =:= FoundModules) of
        true ->
            ok;
        false ->
            ?DEBUG("Missing plugins: ~p\n", [Modules -- FoundModules]),
            ok
    end,
    {ok, FoundModules}.

%%
%% Return .app file if the current directory is an OTP app
%%
app_dir(Dir) ->
    rebar_app_utils:is_app_dir(Dir).

%%
%% Return the reltool.config file if the current directory is release directory
%%
rel_dir(Dir) ->
    rebar_rel_utils:is_rel_dir(Dir).




apply_command(Command, Modules, Config, ModuleFile) ->
    case select_modules(Modules, Command, []) of
        [] ->
            ?WARN("'~p' command does not apply to directory ~s\n",
                     [Command, rebar_utils:get_cwd()]);

        TargetModules ->
            %% Provide some info on where we are
            Dir = rebar_utils:get_cwd(),
            ?CONSOLE("==> ~s (~s)\n", [filename:basename(Dir), Command]),

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
    case rebar_config:get(Config, lib_dirs, []) of
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
    Apps = filelib:wildcard(filename:join([Dir, '*', ebin])),
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

acc_modules([], _Command, Config, _File, Acc) ->
    {Config, Acc};
acc_modules([Module | Rest], Command, Config, File, Acc) ->
    case Module:Command(Config, File) of
        {ok, NewConfig, Result} when is_list(Result) ->
            List = Result;
        {ok, NewConfig, Result} ->
            List = [Result]
    end,
    acc_modules(Rest, Command, NewConfig, File, List ++ Acc).
