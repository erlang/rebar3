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
-module(rebar_port_compiler).

-export([compile/2,
         clean/2]).

-include("rebar.hrl").

%% ===================================================================
%% Public API
%% ===================================================================

%% Port driver name - determined by app name
%% Source files (or c_src/*.c by default)
%% Pre-compile hook (optional)
%% Env variables

compile(Config, _AppFile) ->
    %% Compose list of sources from config file -- default to c_src/*.c
    Sources = expand_sources(rebar_config:get_list(Config, port_sources, ["c_src/*.c"]), []),
    case Sources of
        [] ->
            ok;
        _ ->
            %% Extract environment values from the config (if specified) and merge with the
            %% default for this operating system. This enables max flexibility for users.
            OperatingSystem = rebar_utils:get_os(),
            DefaultEnvs  = driver_envs() ++ default_envs(OperatingSystem),
            OverrideEnvs = rebar_config:get_list(Config, port_env, []),
            Env = merge_envs(OverrideEnvs, DefaultEnvs),
            
            %% One or more files are available for building. Run the pre-compile hook, if necessary.
%            run_precompile_hook(Config),

            %% Compile each of the sources
            compile_each(Sources, [], Config, Env),
            ok

            %% Finally, link everything together
%            do_link(Config, AppFile, Bins)
    end.

clean(Config, _AppFile) ->
    ok.


%% ===================================================================
%% Internal functions
%% ===================================================================

expand_sources([], Acc) ->
    Acc;
expand_sources([Spec | Rest], Acc) ->
    Acc2 = filelib:wildcard(Spec) ++ Acc,
    expand_sources(Rest, Acc2).


%% CC       - C compiler
%% CXX      - C++ compiler
%% CFLAGS   - C compiler
%% CXXFLAGS - C++ compiler
%% LDFLAGS  - Link flags

%% DRIVER_CFLAGS  - default -I paths for erts and ei
%% DRIVER_LDFLAGS - default -L and -lerl_interface -lei 


compile_each([], Acc, Config, Env) ->
    lists:reverse(Acc);
compile_each([Source | Rest], Acc, Config, Env) ->
    Ext = filename:extension(Source),
    Bin = filename:rootname(Source, Ext) ++ ".o",
    ?CONSOLE("Compiling ~s\n", [Source]),
    Compiler = compiler(Ext),
    case compiler(Ext) of
        "$CC" ->
            sh(?FMT("$CC -c $CFLAGS $DRIVER_CFLAGS ~s ~s", [Source, Bin]), Env);
        "$CXX" ->
            sh(?FMT("$CXX -c $CXXFLAGS $DRIVER_CFLAGS ~s ~s", [Source, Bin]), Env)
    end,
    compile_each(Rest, [Bin | Acc], Config, Env).



            


needs_compile(Source, Bin) ->
    %% TODO: Generate depends using gcc -MM so we can also check for include changes
    filelib:last_modified(Bin) < filelib:last_modified(Source).

merge_envs(OverrideEnvs, DefaultEnvs) ->
    orddict:merge(fun(Key, Override, Default) ->
                          expand_env_variable(Override, Key, Default)
                  end,
                  orddict:from_list(OverrideEnvs),
                  orddict:from_list(DefaultEnvs)).

    
    

compiler(".cc")  -> "$CXX";
compiler(".cp")  -> "$CXX";
compiler(".cxx") -> "$CXX";
compiler(".cpp") -> "$CXX";
compiler(".CPP") -> "$CXX";
compiler(".c++") -> "$CXX";
compiler(".C")   -> "$CXX";
compiler(_)      -> "$CC".
     
expand_env_variable(InStr, VarName, VarValue) ->
    %% Given env. variable FOO we want to expand all references to
    %% it in InStr. References can have two forms: $FOO and ${FOO}
    R1 = re:replace(InStr, "\\\$" ++ VarName, VarValue),
    re:replace(R1, "\\\${" ++ VarName ++ "}", VarValue).


erts_dir() ->
    lists:concat([code:root_dir(), "/erts-", erlang:system_info(version)]).

driver_envs() ->
    [{"DRIVER_CFLAGS", lists:concat([" -I", code:lib_dir(erl_interface, include),
                                     " -I", filename:join(erts_dir(), include),
                                     " "])},
     {"DRIVER_LDFLAGS", lists:concat([" -L", code:lib_dir(erl_interface, lib),
                                      " -lerl_interface -lei"])}].

default_envs(darwin) ->
    [{"CC", "gcc"},
     {"CXX", "g++"},
     {"CFLAGS", "-g -Wall -fPIC"},
     {"LDFLAGS", "-bundle -flat_namespace -undefined surpress"}];
default_envs(linux) ->
    [{"CC", "gcc"},
     {"CXX", "g++"},
     {"CFLAGS", "-g -Wall -fPIC"},
     {"LDFLAGS", "-shared"}];
default_envs(Os) ->
    ?ERROR("Unsupported operating system ~s: can not generate default build environment.\n", [Os]),
    ?FAIL.


sh(Command, Env) ->
    ?CONSOLE("Cmd: ~p\n~p\n", [Command, Env]),
    Port = open_port({spawn, Command}, [{env, Env}, exit_status, {line, 16384},
                                        use_stdio, stderr_to_stdout]),
    sh_loop(Port).
    
sh_loop(Port) ->            
    receive
        {Port, {data, {_, Line}}} ->
            ?CONSOLE("> ~s\n", [Line]),
            sh_loop(Port);
        {Port, Other} ->
            ?CONSOLE(">> ~p\n", [Other])
    end.
