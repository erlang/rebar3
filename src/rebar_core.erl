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

%% ===================================================================
%% Public API
%% ===================================================================

run(Args) ->
    %% Filter all the flags (i.e. string of form key=value) from the
    %% command line arguments. What's left will be the commands to run.
    Commands = filter_flags(Args, []),
    
    %% Pre-load the rebar app so that we get default configuration
    application:load(rebar),

    %% Initialize logging system
    rebar_log:init(),
    
    %% From the current working directory, search recursively and find
    %% all the application and release directories. We always terminate the
    %% recursion at an application or release directory.
    Cwd = rebar_utils:get_cwd(),
    case target_type(Cwd) of
        undefined ->
            Targets = find_targets(Cwd);
        {Type, Filename} ->
            Targets = [{Type, Cwd, Filename}]
    end,

    %% Prefix all the app targets to the code path so that inter-app compilation
    %% works properly
    update_code_path(Targets),

    %% Finally, apply the specified command to each target
    apply_commands(Targets, Commands)


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
            


%%
%% Recursively find all the targets starting at a root directory
%%
find_targets(Root) ->
    {ok, Files} = file:list_dir(Root),
    find_targets(Files, Root, [], 1).

find_targets([], _Root, Acc, _Depth) ->
    Acc;
find_targets(_Files, _Root, Acc, 10) ->
    Acc;
find_targets([F | Rest], Root, Acc, Depth) ->
    AbsName = filename:join([Root, F]),
    case target_type(AbsName) of
        undefined ->
            case filelib:is_dir(AbsName) of
                true ->
                    {ok, SubFiles} = file:list_dir(AbsName),
                    Acc2 = find_targets(SubFiles, AbsName, Acc, Depth+1);
                false ->
                    Acc2 = Acc
            end;
        {Type, Filename} ->
            Acc2 = [{Type, AbsName, Filename} | Acc]
    end,
    find_targets(Rest, Root, Acc2, Depth).

%%
%% Determine the target type of a given file: app, rel or undefined
%%
target_type(AbsName) ->
    case rebar_app_utils:is_app_dir(AbsName) of
        {true, AppFile} ->
            {app, AppFile};
        false ->
            case rebar_rel_utils:is_rel_dir(AbsName) of
                true ->
                    {rel, ""};
                false ->
                    undefined
            end
    end.


%%
%% Given a command and target type, determine if the command is applicable
%%
valid_command(compile, app) -> true;
valid_command(install, app) -> true;
valid_command(clean, _Any)  -> true;
valid_command(_, _)         -> false.


%%
%% Add all application targets to the front of the code path
%%
update_code_path([]) ->     
    ok;
update_code_path([{app, Dir, _} | Rest]) ->
    EbinDir = filename:join([Dir, "ebin"]),
    true = code:add_patha(EbinDir),
    update_code_path(Rest);
update_code_path([_ | Rest]) ->
    update_code_path(Rest).


apply_commands(_Targets, []) ->
    ok;
apply_commands(Targets, [CommandStr | Rest]) ->
    %% Convert the command into an atom for convenience
    Command = list_to_atom(CommandStr),

    %% Filter out all the targets, based on the specified command.
    FilteredTargets = [{Type, Dir, Filename} || {Type, Dir, Filename} <- Targets,
                                                valid_command(Command, Type) == true],
    case apply_command(FilteredTargets, Command) of
        ok ->
            apply_commands(Targets, Rest);
        Other ->
            Other
    end.

apply_command([], _Command) ->
    ok;
apply_command([{Type, Dir, File} | Rest], Command) ->
    ok = file:set_cwd(Dir),
    Config = rebar_config:new(Dir),

    %% Provide some info on where we are
    ?CONSOLE("==> ~s (~s)\n", [filename:basename(Dir), Command]),

    %% Pull the list of modules that are associated with Type operations. Each module
    %% will be inspected for a function matching Command and if found, will execute that. 
    Modules = rebar_config:get_modules(Config, Type),
    case catch(run_modules(Modules, Command, Config, File)) of
        ok ->
            apply_command(Rest, Command);
        Other ->
            ?ERROR("~p failed while processing ~s: ~p", [Command, Dir, Other])
    end.


run_modules([], _Command, _Config, _File) ->
    ok;
run_modules([Module | Rest], Command, Config, File) ->
    case invoke_command(Module, Command, Config, File) of
        ok ->
            run_modules(Rest, Command, Config, File);
        {error, Reason} ->
            {error, Reason}
    end.

invoke_command(Module, Command, Config, File) ->
    Exports = Module:module_info(exports),
    case lists:member({Command, 2}, Exports) of
        true ->
            Module:Command(Config, File);
        false ->
            ?DEBUG("Skipping ~s:~s/2 (not exported)\n", [Module, Command]),
            ok
    end.
