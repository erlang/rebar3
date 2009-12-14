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

run(["version"]) ->
    %% Load application spec and display vsn and build time info
    ok = application:load(rebar),
    {ok, Vsn} = application:get_key(rebar, vsn),
    ?CONSOLE("Version ~s built ~s\n", [Vsn, ?BUILD_TIME]),
    ok;
run(Args) ->
    %% Filter all the flags (i.e. string of form key=value) from the
    %% command line arguments. What's left will be the commands to run.
    Commands = filter_flags(Args, []),
    
    %% Pre-load the rebar app so that we get default configuration
    ok = application:load(rebar),

    %% Make sure crypto is running
    crypto:start(),

    %% Initialize logging system
    rebar_log:init(),

    %% Convert command strings to atoms
    CommandAtoms = [list_to_atom(C) || C <- Commands],

    %% Load rebar.config, if it exists
    process_dir(rebar_utils:get_cwd(), rebar_config:new(), CommandAtoms).


%% ===================================================================
%% Internal functions
%% ===================================================================

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


process_dir(Dir, ParentConfig, Commands) ->
    ok = file:set_cwd(Dir),
    Config = rebar_config:new(ParentConfig),

    %% Save the current code path and then update it with
    %% lib_dirs. Children inherit parents code path, but we
    %% also want to ensure that we restore everything to pristine
    %% condition after processing this child
    CurrentCodePath = update_code_path(Config),

    %% If there are any subdirs specified, process those first...
    case rebar_config:get(Config, sub_dirs, []) of
        [] ->
            ok;
        Subdirs ->
            %% Edge case: config is inherited, EXCEPT for sub_dir directives -- filter those out
            FilteredConfig = rebar_config:delete(Config, sub_dirs),
            [process_dir(filename:join(Dir, Subdir), FilteredConfig, Commands) || Subdir <- Subdirs],
            ok = file:set_cwd(Dir)
    end,

    %% Get the list of processing modules and check each one against
    %% CWD to see if it's a fit -- if it is, use that set of modules
    %% to process this dir.
    {ok, AvailModuleSets} = application:get_env(rebar, modules),
    case choose_module_set(AvailModuleSets, Dir) of
        {ok, Modules, ModuleSetFile} ->
            apply_commands(Commands, Modules, Config, ModuleSetFile);
        none ->
            ok
    end,

    %% Once we're all done processing, reset the code path to whatever
    %% the parent initialized it to
    restore_code_path(CurrentCodePath),
    ok.

%%
%% Give a list of module sets from rebar.app and a directory, find
%% the appropriate subset of modules for this directory
%%
choose_module_set([], _Dir) ->
    none;
choose_module_set([{Fn, Modules} | Rest], Dir) ->
    case ?MODULE:Fn(Dir) of
        {true, File} ->
            {ok, Modules, File};
        false ->
            choose_module_set(Rest, Dir)
    end.

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
            
    


apply_commands([], _Modules, _Config, _ModuleFile) ->
    ok;
apply_commands([Command | Rest], Modules, Config, ModuleFile) ->
    case select_modules(Modules, Command, []) of
        [] ->
            apply_commands(Rest, Modules, Config, ModuleFile);
        TargetModules ->
            %% Provide some info on where we are
            Dir = rebar_utils:get_cwd(),
            ?CONSOLE("==> ~s (~s)\n", [filename:basename(Dir), Command]),

            %% Run the available modules
            case catch(run_modules(TargetModules, Command, Config, ModuleFile)) of
                ok ->
                    apply_commands(Rest, Modules, Config, ModuleFile);
                {error, failed} ->
                    ?FAIL;
                Other ->
                    ?ERROR("~p failed while processing ~s: ~p", [Command, Dir, Other]),
                    ?FAIL              
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
    true = code:set_path(Path),
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


