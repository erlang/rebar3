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
-module(rebar).

-export([main/1,
         help/0,
         parse_args/1,
         version/0]).

-include("rebar.hrl").

-ifndef(BUILD_TIME).
-define(BUILD_TIME, "undefined").
-endif.

-ifndef(VCS_INFO).
-define(VCS_INFO, "undefined").
-endif.

%% ====================================================================
%% Public API
%% ====================================================================

main(Args) ->
    case catch(run(Args)) of
        ok ->
            ok;
        {error, failed} ->
            halt(1);
        Error ->
            %% Nothing should percolate up from rebar_core;
            %% Dump this error to console
            io:format("Uncaught error in rebar_core: ~p\n", [Error]),
            halt(1)
    end.

%% ====================================================================
%% Internal functions
%% ====================================================================

run(RawArgs) ->
    %% Pre-load the rebar app so that we get default configuration
    ok = application:load(rebar),
    %% Parse out command line arguments -- what's left is a list of commands to
    %% run -- and start running commands
    run_aux(parse_args(RawArgs)).

run_aux(["help"]) ->
    help(),
    ok;
run_aux(["version"]) ->
    %% Display vsn and build time info
    version(),
    ok;
run_aux(Commands) ->
    %% Make sure crypto is running
    ok = crypto:start(),

    %% Initialize logging system
    rebar_log:init(),

    %% Convert command strings to atoms
    CommandAtoms = [list_to_atom(C) || C <- Commands],

    %% Determine the location of the rebar executable; important for pulling
    %% resources out of the escript
    rebar_config:set_global(escript, filename:absname(escript:script_name())),
    ?DEBUG("Rebar location: ~p\n",
           [rebar_config:get_global(escript, undefined)]),

    %% Note the top-level directory for reference
    rebar_config:set_global(base_dir, filename:absname(rebar_utils:get_cwd())),

    %% Keep track of how many operations we do, so we can detect bad commands
    erlang:put(operations, 0),

    %% Process each command, resetting any state between each one
    rebar_core:process_commands(CommandAtoms).

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
%% Parse command line arguments using getopt and also filtering out any
%% key=value pairs. What's left is the list of commands to run
%%
parse_args(Args) ->
    %% Parse getopt options
    OptSpecList = option_spec_list(),
    case getopt:parse(OptSpecList, Args) of
        {ok, {Options, NonOptArgs}} ->
            %% Check options and maybe halt execution
            ok = show_info_maybe_halt(Options, NonOptArgs),

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

            %% Set the rebar config to use
            case proplists:get_value(config, Options) of
                undefined -> ok;
                Conf -> rebar_config:set_global(config, Conf)
            end,

            %% Filter all the flags (i.e. strings of form key=value) from the
            %% command line arguments. What's left will be the commands to run.
            unabbreviate_command_names(filter_flags(NonOptArgs, []));

        {error, {Reason, Data}} ->
            ?ERROR("Error: ~s ~p~n~n", [Reason, Data]),
            help(),
            halt(1)
    end.

%%
%% show version information and halt
%%
version() ->
    {ok, Vsn} = application:get_key(rebar, vsn),
    ?CONSOLE("rebar version: ~s date: ~s vcs: ~s\n",
             [Vsn, ?BUILD_TIME, ?VCS_INFO]).


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
    false = show_info_maybe_halt(help, Opts, fun help/0),
    false = show_info_maybe_halt(commands, Opts, fun commands/0),
    false = show_info_maybe_halt(version, Opts, fun version/0),
    case NonOptArgs of
        [] ->
            ?CONSOLE("No command to run specified!~n",[]),
            help(),
            halt(1);
        _ ->
            ok
    end.

show_info_maybe_halt(O, Opts, F) ->
    case proplists:get_bool(O, Opts) of
        true ->
            F(),
            halt(0);
        false ->
            false
    end.

%%
%% print known commands
%%
commands() ->
    S = <<"
dialyze                              Analyze with Dialyzer
build-plt                            Build Dialyzer PLT
check-plt                            Check Dialyzer PLT

clean                                Clean
compile                              Compile sources

create      template= [var=foo,...]  Create skel based on template and vars
create-app  [appid=myapp]            Create simple app skel
create-node [nodeid=mynode]          Create simple node skel
list-templates                       List available templates

doc                                  Generate Erlang program documentation

check-deps                           Display to be fetched dependencies
get-deps                             Fetch dependencies
update-deps                          Update fetched dependencies
delete-deps                          Delete fetched dependencies

generate    [dump_spec=0/1]          Build release with reltool

generate-upgrade  previous_release=path  Build an upgrade package

generate-appups   previous_release=path Generate appup files

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
     {jobs,     $j, "jobs",     integer,   JobsHelp},
     {config,   $C, "config",   string,    "Rebar config file to use"}
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

command_names() ->
    ["build-plt", "check-deps", "check-plt", "clean", "compile", "create",
     "create-app", "create-node", "ct", "delete-deps", "dialyze", "doc",
     "eunit", "generate", "generate-appups", "generate-upgrade", "get-deps",
     "help", "list-templates", "update-deps", "version", "xref"].

unabbreviate_command_names([]) ->
    [];
unabbreviate_command_names([Command | Commands]) ->
    case get_command_name_candidates(Command) of
        [] ->
            %% let the rest of the code detect that the command doesn't exist
            %% (this would perhaps be a good place to fail)
            [Command | unabbreviate_command_names(Commands)];
        [FullCommand] ->
            [FullCommand | unabbreviate_command_names(Commands)];
        Candidates ->
            ?ABORT("Found more than one match for abbreviated command name "
                   " '~s',~nplease be more specific. Possible candidates:~n"
                   "  ~s~n",
                   [Command, string:join(Candidates, ", ")])
    end.

get_command_name_candidates(Command) ->
    %% Get the command names which match the given (abbreviated) command name.
    %% * "c"        matches commands like compile, clean and create-app
    %% * "create"   matches command create only, since it's unique
    %% * "create-"  matches commands starting with create-
    %% * "c-a"      matches create-app
    %% * "create-a" matches create-app
    %% * "c-app"    matches create-app
    Candidates = [Candidate || Candidate <- command_names(),
                               is_command_name_candidate(Command, Candidate)],
    %% Is there a complete match?  If so return only that, return a
    %% list of candidates otherwise
    case lists:member(Command, Candidates) of
        true  -> [Command];
        false -> Candidates
    end.

is_command_name_candidate(Command, Candidate) ->
    lists:prefix(Command, Candidate)
        orelse is_command_name_sub_word_candidate(Command, Candidate).

is_command_name_sub_word_candidate(Command, Candidate) ->
    %% Allow for parts of commands to be abbreviated, i.e. create-app
    %% can be shortened to "create-a", "c-a" or "c-app" (but not
    %% "create-" since that would be ambiguous).
    CommandSubWords = re:split(Command, "-", [{return, list}]),
    CandidateSubWords = re:split(Candidate, "-", [{return, list}]),
    is_command_name_sub_word_candidate_aux(CommandSubWords, CandidateSubWords).

is_command_name_sub_word_candidate_aux([CmdSW | CmdSWs], [CandSW | CandSWs]) ->
    case lists:prefix(CmdSW, CandSW) of
        true ->
            is_command_name_sub_word_candidate_aux(CmdSWs, CandSWs);
        false ->
            false
    end;
is_command_name_sub_word_candidate_aux([], []) ->
    true;
is_command_name_sub_word_candidate_aux(_CmdSWs, _CandSWs) ->
    false.
