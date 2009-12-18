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

%% Supported configuration variables:
%%
%% * port_sources - Erlang list of files and/or wildcard strings to be compiled
%%
%% * port_envs - Erlang list of key/value pairs which will control the environment when
%%               running the compiler and linker. By default, the following variables
%%               are defined:
%%               CC       - C compiler
%%               CXX      - C++ compiler
%%               CFLAGS   - C compiler
%%               CXXFLAGS - C++ compiler
%%               LDFLAGS  - Link flags
%%               DRIVER_CFLAGS  - default -I paths for erts and ei
%%               DRIVER_LDFLAGS - default -L and -lerl_interface -lei
%%
%%               Note that if you wish to extend (vs. replace) these variables, you MUST
%%               include a shell-style reference in your definition. E.g. to extend CFLAGS,
%%               do something like:
%%
%%               {port_envs, [{"CFLAGS", "$CFLAGS -MyOtherOptions"}]}
%%
%%               It is also possible to specify platform specific options by specifying a triplet
%%               where the first string is a regex that is checked against erlang's system architecture
%%               string. E.g. to specify a CFLAG that only applies to x86_64 on linux do:
%%
%%               {port_envs, [{"x86_64.*-linux", "CFLAGS", "$CFLAGS -X86Options"}]}
%%
%% * port_pre_script - Tuple which specifies a pre-compilation script to run, and a filename that
%%                     exists as a result of the script running. 
%%
%% * port_cleanup_script - String that specifies a script to run during cleanup. Use this to remove
%%                         files/directories created by port_pre_script.
%%

compile(Config, AppFile) ->
    %% Compose list of sources from config file -- defaults to c_src/*.c
    Sources = expand_sources(rebar_config:get_list(Config, port_sources, ["c_src/*.c"]), []),
    case Sources of
        [] ->
            ok;
        _ ->
            %% Extract environment values from the config (if specified) and merge with the
            %% default for this operating system. This enables max flexibility for users.
            DefaultEnvs  = filter_envs(default_env(), []),
            OverrideEnvs = filter_envs(rebar_config:get_list(Config, port_envs, []), []),
            Env = merge_envs(OverrideEnvs, DefaultEnvs),
            
            %% One or more files are available for building. Run the pre-compile hook, if
            %% necessary.
            run_precompile_hook(Config, Env),

            %% Compile each of the sources
            {NewBins, ExistingBins} = compile_each(Sources, Config, Env, [], []),

            %% Construct the driver name and make sure priv/ exists
            SoName = so_name(AppFile),
            ok = filelib:ensure_dir(SoName),

            %% Only relink if necessary, given the SoName and list of new binaries
            case needs_link(SoName, NewBins) of
                true ->
                    AllBins = string:join(NewBins ++ ExistingBins, " "),
                    rebar_utils:sh_failfast(?FMT("$CC ~s $LDFLAGS $DRIVER_LDFLAGS -o ~s", [AllBins, SoName]), Env);
                false ->
                    ?INFO("Skipping relink of ~s\n", [SoName]),
                    ok
            end
    end.

clean(Config, AppFile) ->
    %% Build a list of sources so as to derive all the bins we generated
    Sources = expand_sources(rebar_config:get_list(Config, port_sources, ["c_src/*.c"]), []),
    rebar_file_utils:delete_each([source_to_bin(S) || S <- Sources]),

    %% Delete the .so file
    rebar_file_utils:delete_each([so_name(AppFile)]),

    %% Run the cleanup script, if it exists
    run_cleanup_hook(Config).




%% ===================================================================
%% Internal functions
%% ===================================================================

expand_sources([], Acc) ->
    Acc;
expand_sources([Spec | Rest], Acc) ->
    Acc2 = filelib:wildcard(Spec) ++ Acc,
    expand_sources(Rest, Acc2).

run_precompile_hook(Config, Env) ->
    case rebar_config:get(Config, port_pre_script, undefined) of
        undefined ->
            ok;
        {Script, BypassFileName} ->
            case filelib:is_regular(BypassFileName) of
                false ->
                    ?CONSOLE("Running ~s\n", [Script]),
                    rebar_utils:sh_failfast(Script, Env);
                true ->
                    ?INFO("~s exists; not running ~s\n", [BypassFileName, Script])
            end
    end.

run_cleanup_hook(Config) ->
    case rebar_config:get(Config, port_cleanup_script, undefined) of
        undefined ->
            ok;
        Script ->
            ?CONSOLE("Running ~s\n", [Script]),
            rebar_utils:sh_failfast(Script, [])
    end.


compile_each([], _Config, _Env, NewBins, ExistingBins) ->
    {lists:reverse(NewBins), lists:reverse(ExistingBins)};
compile_each([Source | Rest], Config, Env, NewBins, ExistingBins) ->
    Ext = filename:extension(Source),
    Bin = filename:rootname(Source, Ext) ++ ".o",
    case needs_compile(Source, Bin) of
        true ->
            ?CONSOLE("Compiling ~s\n", [Source]),
            case compiler(Ext) of
                "$CC" ->
                    rebar_utils:sh_failfast(?FMT("$CC -c $CFLAGS $DRIVER_CFLAGS ~s -o ~s", [Source, Bin]), Env);
                "$CXX" ->
                    rebar_utils:sh_failfast(?FMT("$CXX -c $CXXFLAGS $DRIVER_CFLAGS ~s -o ~s", [Source, Bin]), Env)
            end,
            compile_each(Rest, Config, Env, [Bin | NewBins], ExistingBins);
        
        false ->
            ?INFO("Skipping ~s\n", [Source]),
            compile_each(Rest, Config, Env, NewBins, [Bin | ExistingBins])
    end.

          

needs_compile(Source, Bin) ->
    %% TODO: Generate depends using gcc -MM so we can also check for include changes
    filelib:last_modified(Bin) < filelib:last_modified(Source).

needs_link(SoName, []) ->
    ?DEBUG("2 Lad mod \n", []),
    filelib:last_modified(SoName) == 0;
needs_link(SoName, NewBins) ->
    MaxLastMod = lists:max([filelib:last_modified(B) || B <- NewBins]),
    case filelib:last_modified(SoName) of
        0 ->
            ?DEBUG("Last mod is 0 on ~s\n", [SoName]),
            true;
        Other ->
            ?DEBUG("Checking ~p >= ~p", [MaxLastMod, Other]),
            MaxLastMod >= Other
    end.
    
merge_envs(OverrideEnvs, DefaultEnvs) ->
    orddict:merge(fun(Key, Override, Default) ->
                          expand_env_variable(Override, Key, Default)
                  end,
                  orddict:from_list(OverrideEnvs),
                  orddict:from_list(DefaultEnvs)).


%%
%% Choose a compiler variable, based on a provided extension
%%
compiler(".cc")  -> "$CXX";
compiler(".cp")  -> "$CXX";
compiler(".cxx") -> "$CXX";
compiler(".cpp") -> "$CXX";
compiler(".CPP") -> "$CXX";
compiler(".c++") -> "$CXX";
compiler(".C")   -> "$CXX";
compiler(_)      -> "$CC".


%%
%% Given env. variable FOO we want to expand all references to
%% it in InStr. References can have two forms: $FOO and ${FOO}
%%
expand_env_variable(InStr, VarName, VarValue) ->

    R1 = re:replace(InStr, "\\\$" ++ VarName, VarValue),
    re:replace(R1, "\\\${" ++ VarName ++ "}", VarValue, [{return, list}]).


%%
%% Filter a list of env vars such that only those which match the provided
%% architecture regex (or do not have a regex) are returned.
%%
filter_envs([], Acc) ->
    lists:reverse(Acc);
filter_envs([{ArchRegex, Key, Value} | Rest], Acc) ->
    case rebar_utils:is_arch(ArchRegex) of
        true ->
            filter_envs(Rest, [{Key, Value} | Acc]);
        false ->
            filter_envs(Rest, Acc)
    end;
filter_envs([{Key, Value} | Rest], Acc) ->
    filter_envs(Rest, [{Key, Value} | Acc]).


erts_dir() ->
    lists:concat([code:root_dir(), "/erts-", erlang:system_info(version)]).

default_env() ->
    [{"CC", "gcc"},
     {"CXX", "g++"},
     {"CFLAGS", "-g -Wall -fPIC"},
     {"CXXFLAGS", "-g -Wall -fPIC"},
     {"darwin", "LDFLAGS", "-bundle -flat_namespace -undefined suppress"},
     {"linux", "LDFLAGS", "-shared"},
     {"DRIVER_CFLAGS", lists:concat([" -I", code:lib_dir(erl_interface, include),
                                     " -I", filename:join(erts_dir(), include),
                                     " "])},
     {"DRIVER_LDFLAGS", lists:concat([" -L", code:lib_dir(erl_interface, lib),
                                      " -lerl_interface -lei"])}].



source_to_bin(Source) ->
    Ext = filename:extension(Source),
    filename:rootname(Source, Ext) ++ ".o".

so_name(AppFile) ->
    %% Get the app name, which we'll use to generate the linked port driver name
    case rebar_app_utils:load_app_file(AppFile) of
        {ok, AppName, _} ->
            ok;
        error ->
            AppName = undefined,
            ?FAIL
    end,

    %% Construct the driver name 
    ?FMT("priv/~s_drv.so", [AppName]).

