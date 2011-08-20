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
-module(rebar_port_compiler).

-export([compile/2,
         clean/2,
         setup_env/1]).

-include("rebar.hrl").

%% ===================================================================
%% Public API
%% ===================================================================

%% Supported configuration variables:
%%
%% * port_sources - Erlang list of filenames or wildcards to be compiled. May
%%                  also contain a tuple consisting of a regular expression to
%%                  be applied against the system architecture and a list of
%%                  filenames or wildcards to include should the expression
%%                  pass.
%%
%% * so_specs  - Erlang list of tuples of the form
%%               {"priv/so_name.so", ["c_src/object_file_name.o"]}
%%               useful for building multiple *.so files.
%%
%% * port_envs - Erlang list of key/value pairs which will control
%%               the environment when running the compiler and linker.
%%
%%               By default, the following variables
%%               are defined:
%%               CC       - C compiler
%%               CXX      - C++ compiler
%%               CFLAGS   - C compiler
%%               CXXFLAGS - C++ compiler
%%               LDFLAGS  - Link flags
%%               ERL_CFLAGS  - default -I paths for erts and ei
%%               ERL_LDFLAGS - default -L and -lerl_interface -lei
%%               DRV_CFLAGS  - flags that will be used for compiling the driver
%%               DRV_LDFLAGS - flags that will be used for linking the driver
%%               ERL_EI_LIBDIR - ei library directory
%%               CXX_TEMPLATE  - C++ command template
%%               CC_TEMPLATE   - C command template
%%               LINK_TEMPLATE - Linker command template
%%               PORT_IN_FILES - contains a space separated list of input
%%                    file(s), (used in command template)
%%               PORT_OUT_FILE - contains the output filename (used in
%%                    command template)
%%
%%               Note that if you wish to extend (vs. replace) these variables,
%%               you MUST include a shell-style reference in your definition.
%%               e.g. to extend CFLAGS, do something like:
%%
%%               {port_envs, [{"CFLAGS", "$CFLAGS -MyOtherOptions"}]}
%%
%%               It is also possible to specify platform specific options
%%               by specifying a tripletwhere the first string is a regex
%%               that is checked against erlang's system architecture string.
%%               e.g. to specify a CFLAG that only applies to x86_64 on linux
%%               do:
%%
%%               {port_envs, [{"x86_64.*-linux", "CFLAGS",
%%                             "$CFLAGS -X86Options"}]}
%%
%% * port_pre_script - Tuple which specifies a pre-compilation script to run,
%%                     and a filename that exists as a result of the script
%%                     running.
%%
%% * port_cleanup_script - String that specifies a script to run during cleanup.
%%                         Use this to remove files/directories created by
%%                         port_pre_script.
%%

compile(Config, AppFile) ->
    %% Compose list of sources from config file -- defaults to c_src/*.c
    Sources = expand_sources(rebar_config:get_list(Config, port_sources,
                                                   ["c_src/*.c"]), []),
    case Sources of
        [] ->
            ok;
        _ ->
            Env = setup_env(Config),

            %% Compile each of the sources
            {NewBins, ExistingBins} = compile_each(Sources, Config, Env,
                                                   [], []),

            %% Construct the driver name and make sure priv/ exists
            SoSpecs = so_specs(Config, AppFile, NewBins ++ ExistingBins),
            ?INFO("Using specs ~p\n", [SoSpecs]),
            lists:foreach(fun({SoName,_}) ->
                                  ok = filelib:ensure_dir(SoName)
                          end, SoSpecs),

            %% Only relink if necessary, given the SoName
            %% and list of new binaries
            lists:foreach(
              fun({SoName,Bins}) ->
                      AllBins = [sets:from_list(Bins),
                                 sets:from_list(NewBins)],
                      Intersection = sets:intersection(AllBins),
                      case needs_link(SoName, sets:to_list(Intersection)) of
                          true ->
                              Cmd = expand_command("LINK_TEMPLATE", Env,
                                                   string:join(Bins, " "),
                                                   SoName),
                              rebar_utils:sh(Cmd, [{env, Env}]);
                          false ->
                              ?INFO("Skipping relink of ~s\n", [SoName]),
                              ok
                      end
              end, SoSpecs)
    end.

clean(Config, AppFile) ->
    %% Build a list of sources so as to derive all the bins we generated
    Sources = expand_sources(rebar_config:get_list(Config, port_sources,
                                                   ["c_src/*.c"]), []),
    rebar_file_utils:delete_each([source_to_bin(S) || S <- Sources]),

    %% Delete the .so file
    ExtractSoName = fun({SoName, _}) -> SoName end,
    rebar_file_utils:delete_each([ExtractSoName(S)
                                  || S <- so_specs(Config, AppFile,
                                                   expand_objects(Sources))]).

setup_env(Config) ->
    %% Extract environment values from the config (if specified) and
    %% merge with the default for this operating system. This enables
    %% max flexibility for users.
    DefaultEnvs  = filter_envs(default_env(), []),
    PortEnvs = rebar_config:get_list(Config, port_envs, []),
    OverrideEnvs = filter_envs(PortEnvs, []),
    RawEnv = apply_defaults(os_env(), DefaultEnvs) ++ OverrideEnvs,
    expand_vars_loop(merge_each_var(RawEnv, [])).

%% ===================================================================
%% Internal functions
%% ===================================================================

expand_sources([], Acc) ->
    Acc;
expand_sources([{ArchRegex, Spec} | Rest], Acc) ->
    case rebar_utils:is_arch(ArchRegex) of
        true ->
            Acc2 = expand_sources(Spec, Acc),
            expand_sources(Rest, Acc2);
        false ->
            expand_sources(Rest, Acc)
    end;
expand_sources([Spec | Rest], Acc) ->
    Acc2 = filelib:wildcard(Spec) ++ Acc,
    expand_sources(Rest, Acc2).

expand_objects(Sources) ->
    [filename:join([filename:dirname(F), filename:basename(F) ++ ".o"])
     || F <- Sources].

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
                    rebar_utils:sh(expand_command("CC_TEMPLATE", Env,
                                                  Source, Bin),
                                   [{env, Env}]);
                "$CXX" ->
                    rebar_utils:sh(expand_command("CXX_TEMPLATE", Env,
                                                  Source, Bin),
                                   [{env, Env}])
            end,
            compile_each(Rest, Config, Env, [Bin | NewBins], ExistingBins);

        false ->
            ?INFO("Skipping ~s\n", [Source]),
            compile_each(Rest, Config, Env, NewBins, [Bin | ExistingBins])
    end.

needs_compile(Source, Bin) ->
    %% TODO: Generate depends using gcc -MM so we can also
    %% check for include changes
    filelib:last_modified(Bin) < filelib:last_modified(Source).

needs_link(SoName, []) ->
    filelib:last_modified(SoName) == 0;
needs_link(SoName, NewBins) ->
    MaxLastMod = lists:max([filelib:last_modified(B) || B <- NewBins]),
    case filelib:last_modified(SoName) of
        0 ->
            ?DEBUG("Last mod is 0 on ~s\n", [SoName]),
            true;
        Other ->
            ?DEBUG("Checking ~p >= ~p\n", [MaxLastMod, Other]),
            MaxLastMod >= Other
    end.


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
%% Given a list of {Key, Value} variables, and another list of default
%% {Key, Value} variables, return a merged list where the rule is if the
%% default is expandable expand it with the value of the variable list,
%% otherwise just return the value of the variable.
%%
apply_defaults(Vars, Defaults) ->
    dict:to_list(
      dict:merge(fun(Key, VarValue, DefaultValue) ->
                         case is_expandable(DefaultValue) of
                             true ->
                                 rebar_utils:expand_env_variable(DefaultValue,
                                                                 Key,
                                                                 VarValue);
                             false -> VarValue
                         end
                 end,
                 dict:from_list(Vars),
                 dict:from_list(Defaults))).

%%
%% Given a list of {Key, Value} environment variables, where Key may be defined
%% multiple times, walk the list and expand each self-reference so that we
%% end with a list of each variable singly-defined.
%%
merge_each_var([], Vars) ->
    Vars;
merge_each_var([{Key, Value} | Rest], Vars) ->
    Evalue = case orddict:find(Key, Vars) of
                 error ->
                     %% Nothing yet defined for this key/value.
                     %% Expand any self-references as blank.
                     rebar_utils:expand_env_variable(Value, Key, "");
                 {ok, Value0} ->
                     %% Use previous definition in expansion
                     rebar_utils:expand_env_variable(Value, Key, Value0)
             end,
    merge_each_var(Rest, orddict:store(Key, Evalue, Vars)).

%%
%% Give a unique list of {Key, Value} environment variables, expand each one
%% for every other key until no further expansions are possible.
%%
expand_vars_loop(Vars) ->
    expand_vars_loop(Vars, 10).

expand_vars_loop(_, 0) ->
    ?ABORT("Max. expansion reached for ENV vars!\n", []);
expand_vars_loop(Vars0, Count) ->
    Vars = lists:foldl(fun({Key, Value}, Acc) ->
                               expand_vars(Key, Value, Acc)
                       end,
                       Vars0, Vars0),
    case orddict:from_list(Vars) of
        Vars0 ->
            Vars0;
        _ ->
            expand_vars_loop(Vars, Count-1)
    end.

%%
%% Expand all OTHER references to a given K/V pair
%%
expand_vars(Key, Value, Vars) ->
    lists:foldl(
      fun({AKey, AValue}, Acc) ->
              NewValue = case AKey of
                             Key ->
                                 AValue;
                             _ ->
                                 rebar_utils:expand_env_variable(AValue,
                                                                 Key, Value)
                         end,
              [{AKey, NewValue} | Acc]
      end,
      [], Vars).

expand_command(TmplName, Env, InFiles, OutFile) ->
    Cmd0 = proplists:get_value(TmplName, Env),
    Cmd1 = rebar_utils:expand_env_variable(Cmd0, "PORT_IN_FILES", InFiles),
    Cmd2 = rebar_utils:expand_env_variable(Cmd1, "PORT_OUT_FILE", OutFile),
    re:replace(Cmd2, "\\\$\\w+|\\\${\\w+}", "", [global, {return, list}]).

%%
%% Given a string, determine if it is expandable
%%
is_expandable(InStr) ->
    case re:run(InStr,"\\\$",[{capture,none}]) of
        match -> true;
        nomatch -> false
    end.

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

os_env() ->
    Os = [list_to_tuple(re:split(S, "=", [{return, list}, {parts, 2}])) ||
             S <- os:getenv()],
    %% Drop variables without a name (win32)
    [T1 || {K, _V} = T1 <- Os, K =/= []].

default_env() ->
    [
     {"CXX_TEMPLATE",
      "$CXX -c $CXXFLAGS $DRV_CFLAGS $PORT_IN_FILES -o $PORT_OUT_FILE"},
     {"CC_TEMPLATE",
      "$CC -c $CFLAGS $DRV_CFLAGS $PORT_IN_FILES -o $PORT_OUT_FILE"},
     {"LINK_TEMPLATE",
      "$CC $PORT_IN_FILES $LDFLAGS $DRV_LDFLAGS -o $PORT_OUT_FILE"},
     {"CC", "cc"},
     {"CXX", "c++"},
     {"ERL_CFLAGS", lists:concat([" -I", code:lib_dir(erl_interface, include),
                                  " -I", filename:join(erts_dir(), "include"),
                                  " "])},
     {"ERL_LDFLAGS", " -L$ERL_EI_LIBDIR -lerl_interface -lei"},
     {"DRV_CFLAGS", "-g -Wall -fPIC $ERL_CFLAGS"},
     {"DRV_LDFLAGS", "-shared $ERL_LDFLAGS"},
     {"ERL_EI_LIBDIR", code:lib_dir(erl_interface, lib)},
     {"darwin", "DRV_LDFLAGS",
      "-bundle -flat_namespace -undefined suppress $ERL_LDFLAGS"},
     {"ERLANG_ARCH", rebar_utils:wordsize()},
     {"ERLANG_TARGET", rebar_utils:get_arch()},

     %% Solaris specific flags
     {"solaris.*-64$", "CFLAGS", "-D_REENTRANT -m64 $CFLAGS"},
     {"solaris.*-64$", "CXXFLAGS", "-D_REENTRANT -m64 $CXXFLAGS"},
     {"solaris.*-64$", "LDFLAGS", "-m64 $LDFLAGS"},

     %% OS X Leopard flags for 64-bit
     {"darwin9.*-64$", "CFLAGS", "-m64 $CFLAGS"},
     {"darwin9.*-64$", "CXXFLAGS", "-m64 $CXXFLAGS"},
     {"darwin9.*-64$", "LDFLAGS", "-arch x86_64 $LDFLAGS"},

     %% OS X Snow Leopard flags for 32-bit
     {"darwin10.*-32", "CFLAGS", "-m32 $CFLAGS"},
     {"darwin10.*-32", "CXXFLAGS", "-m32 $CXXFLAGS"},
     {"darwin10.*-32", "LDFLAGS", "-arch i386 $LDFLAGS"},

     %% OS X Lion flags for 32-bit
     {"darwin11.*-32", "CFLAGS", "-m32 $CFLAGS"},
     {"darwin11.*-32", "CXXFLAGS", "-m32 $CXXFLAGS"},
     {"darwin11.*-32", "LDFLAGS", "-arch i386 $LDFLAGS"}
    ].



source_to_bin(Source) ->
    Ext = filename:extension(Source),
    filename:rootname(Source, Ext) ++ ".o".

so_specs(Config, AppFile, Bins) ->
    Specs = make_so_specs(Config, AppFile, Bins),
    case os:type() of
        {win32, nt} ->
            [switch_so_to_dll(SoSpec) || SoSpec <- Specs];
        _ ->
            Specs
    end.

switch_so_to_dll(Orig = {Name, Spec}) ->
    case filename:extension(Name) of
        ".so" ->
            {filename:rootname(Name, ".so") ++ ".dll", Spec};
        _ ->
            %% Not a .so; leave it
            Orig
    end.

make_so_specs(Config, AppFile, Bins) ->
    case rebar_config:get(Config, so_specs, undefined) of
        undefined ->
            %% New form of so_specs is not provided. See if the old form
            %% of {so_name} is available instead
            Dir = "priv",
            SoName = case rebar_config:get(Config, so_name, undefined) of
                         undefined ->
                             %% Ok, neither old nor new form is available. Use
                             %% the app name and generate a sensible default.
                             AppName = rebar_app_utils:app_name(AppFile),
                             filename:join(Dir,
                                           lists:concat([AppName, "_drv.so"]));

                         AName ->
                             %% Old form is available -- use it
                             filename:join(Dir, AName)
                     end,
            [{SoName, Bins}];

        SoSpecs ->
            SoSpecs
    end.
