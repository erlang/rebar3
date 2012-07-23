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
%% * port_specs - Erlang list of tuples of the forms
%%                {ArchRegex, TargetFile, Sources, Options}
%%                {ArchRegex, TargetFile, Sources}
%%                {TargetFile, Sources}
%%
%% * port_env - Erlang list of key/value pairs which will control
%%              the environment when running the compiler and linker.
%%
%%              By default, the following variables are defined:
%%              CC       - C compiler
%%              CXX      - C++ compiler
%%              CFLAGS   - C compiler
%%              CXXFLAGS - C++ compiler
%%              LDFLAGS  - Link flags
%%              ERL_CFLAGS  - default -I paths for erts and ei
%%              ERL_LDFLAGS - default -L and -lerl_interface -lei
%%              DRV_CFLAGS  - flags that will be used for compiling
%%              DRV_LDFLAGS - flags that will be used for linking
%%              EXE_CFLAGS  - flags that will be used for compiling
%%              EXE_LDFLAGS - flags that will be used for linking
%%              ERL_EI_LIBDIR - ei library directory
%%              DRV_CXX_TEMPLATE  - C++ command template
%%              DRV_CC_TEMPLATE   - C command template
%%              DRV_LINK_TEMPLATE - Linker command template
%%              EXE_CXX_TEMPLATE  - C++ command template
%%              EXE_CC_TEMPLATE   - C command template
%%              EXE_LINK_TEMPLATE - Linker command template
%%              PORT_IN_FILES - contains a space separated list of input
%%                   file(s), (used in command template)
%%              PORT_OUT_FILE - contains the output filename (used in
%%                   command template)
%%
%%              Note that if you wish to extend (vs. replace) these variables,
%%              you MUST include a shell-style reference in your definition.
%%              e.g. to extend CFLAGS, do something like:
%%
%%              {port_env, [{"CFLAGS", "$CFLAGS -MyOtherOptions"}]}
%%
%%              It is also possible to specify platform specific options
%%              by specifying a triplet where the first string is a regex
%%              that is checked against Erlang's system architecture string.
%%              e.g. to specify a CFLAG that only applies to x86_64 on linux
%%              do:
%%
%%              {port_env, [{"x86_64.*-linux", "CFLAGS",
%%                           "$CFLAGS -X86Options"}]}
%%

-record(spec, {type::'drv' | 'exe',
               target::file:filename(),
               sources = [] :: [file:filename(), ...],
               objects = [] :: [file:filename(), ...],
               opts = [] ::list() | []}).

compile(Config, _AppFile) ->
    case get_specs(Config) of
        [] ->
            ok;
        Specs ->
            SharedEnv = rebar_config:get_env(Config, ?MODULE),

            %% Compile each of the sources
            NewBins = compile_sources(Specs, SharedEnv),

            %% Make sure that the target directories exist
            ?INFO("Using specs ~p\n", [Specs]),
            lists:foreach(fun(#spec{target=Target}) ->
                                  ok = filelib:ensure_dir(Target)
                          end, Specs),

            %% Only relink if necessary, given the Target
            %% and list of new binaries
            lists:foreach(
              fun(#spec{target=Target, objects=Bins, opts=Opts}) ->
                      AllBins = [sets:from_list(Bins),
                                 sets:from_list(NewBins)],
                      Intersection = sets:intersection(AllBins),
                      case needs_link(Target, sets:to_list(Intersection)) of
                          true ->
                              LinkTemplate = select_link_template(Target),
                              Env = proplists:get_value(env, Opts, SharedEnv),
                              Cmd = expand_command(LinkTemplate, Env,
                                                   string:join(Bins, " "),
                                                   Target),
                              rebar_utils:sh(Cmd, [{env, Env}]);
                          false ->
                              ?INFO("Skipping relink of ~s\n", [Target]),
                              ok
                      end
              end, Specs)
    end.

clean(Config, _AppFile) ->
    case get_specs(Config) of
        [] ->
            ok;
        Specs ->
            lists:foreach(fun(#spec{target=Target, objects=Objects}) ->
                                  rebar_file_utils:delete_each([Target]),
                                  rebar_file_utils:delete_each(Objects)
                          end, Specs)
    end,
    ok.

setup_env(Config) ->
    setup_env(Config, []).

%% ===================================================================
%% Internal functions
%% ===================================================================

setup_env(Config, ExtraEnv) ->
    %% Extract environment values from the config (if specified) and
    %% merge with the default for this operating system. This enables
    %% max flexibility for users.
    DefaultEnv  = filter_env(default_env(), []),
    RawPortEnv = rebar_config:get_list(Config, port_env, []),
    PortEnv = filter_env(RawPortEnv, []),
    GlobalDefines = global_defines(Config),
    OverrideEnv = GlobalDefines ++ PortEnv ++ filter_env(ExtraEnv, []),
    RawEnv = apply_defaults(os_env(), DefaultEnv) ++ OverrideEnv,
    expand_vars_loop(merge_each_var(RawEnv, [])).

global_defines(Config) ->
    Defines = rebar_config:get_global(Config, defines, []),
    Flags = string:join(["-D" ++ D || D <- Defines], " "),
    [{"ERL_CFLAGS", "$ERL_CFLAGS " ++ Flags}].

replace_extension(File, NewExt) ->
    OldExt = filename:extension(File),
    replace_extension(File, OldExt, NewExt).

replace_extension(File, OldExt, NewExt) ->
    filename:rootname(File, OldExt) ++ NewExt.

%%
%% == compile and link ==
%%

compile_sources(Specs, SharedEnv) ->
    lists:foldl(
      fun(#spec{sources=Sources, type=Type, opts=Opts}, NewBins) ->
              Env = proplists:get_value(env, Opts, SharedEnv),
              compile_each(Sources, Type, Env, NewBins)
      end, [], Specs).

compile_each([], _Type, _Env, NewBins) ->
    lists:reverse(NewBins);
compile_each([Source | Rest], Type, Env, NewBins) ->
    Ext = filename:extension(Source),
    Bin = replace_extension(Source, Ext, ".o"),
    case needs_compile(Source, Bin) of
        true ->
            Template = select_compile_template(Type, compiler(Ext)),
            Cmd = expand_command(Template, Env, Source, Bin),
            ShOpts = [{env, Env}, return_on_error, {use_stdout, false}],
            exec_compiler(Source, Cmd, ShOpts),
            compile_each(Rest, Type, Env, [Bin | NewBins]);
        false ->
            ?INFO("Skipping ~s\n", [Source]),
            compile_each(Rest, Type, Env, NewBins)
    end.

exec_compiler(Source, Cmd, ShOpts) ->
    case rebar_utils:sh(Cmd, ShOpts) of
        {error, {_RC, RawError}} ->
            AbsSource = filename:absname(Source),
            ?CONSOLE("Compiling ~s\n", [AbsSource]),
            Error = re:replace(RawError, Source, AbsSource,
                               [{return, list}, global]),
            ?CONSOLE("~s", [Error]),
            ?ABORT;
        {ok, Output} ->
            ?CONSOLE("Compiling ~s\n", [Source]),
            ?CONSOLE("~s", [Output])
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
%% == port_specs ==
%%

get_specs(Config) ->
    PortSpecs = rebar_config:get_local(Config, port_specs, []),
    Filtered = filter_port_specs(PortSpecs),
    OsType = os:type(),
    [get_port_spec(Config, OsType, Spec) || Spec <- Filtered].

filter_port_specs(Specs) ->
    [S || S <- Specs, filter_port_spec(S)].

filter_port_spec({ArchRegex, _, _, _}) ->
    rebar_utils:is_arch(ArchRegex);
filter_port_spec({ArchRegex, _, _}) ->
    rebar_utils:is_arch(ArchRegex);
filter_port_spec({_, _}) ->
    true.

get_port_spec(Config, OsType, {Target, Sources}) ->
    get_port_spec(Config, OsType, {undefined, Target, Sources, []});
get_port_spec(Config, OsType, {Arch, Target, Sources}) ->
    get_port_spec(Config, OsType, {Arch, Target, Sources, []});
get_port_spec(Config, OsType, {_Arch, Target, Sources, Opts}) ->
    SourceFiles = port_sources(Sources),
    ObjectFiles = port_objects(SourceFiles),
    #spec{type=target_type(Target),
          target=maybe_switch_extension(OsType, Target),
          sources=SourceFiles,
          objects=ObjectFiles,
          opts=port_opts(Config, Opts)}.

port_sources(Sources) ->
    lists:flatmap(fun filelib:wildcard/1, Sources).

port_objects(SourceFiles) ->
    [replace_extension(O, ".o") || O <- SourceFiles].

port_opts(Config, Opts) ->
    [port_opt(Config, O) || O <- Opts].

port_opt(Config, {env, Env}) ->
    {env, setup_env(Config, Env)};
port_opt(_Config, Opt) ->
    Opt.

maybe_switch_extension({win32, nt}, Target) ->
    switch_to_dll_or_exe(Target);
maybe_switch_extension(_OsType, Target) ->
    Target.

switch_to_dll_or_exe(Target) ->
    case filename:extension(Target) of
        ".so"  -> filename:rootname(Target, ".so") ++ ".dll";
        []     -> Target ++ ".exe";
        _Other -> Target
    end.

%%
%% == port_env ==
%%

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
    expand_vars_loop(Vars, [], dict:from_list(Vars), 10).

expand_vars_loop(_Pending, _Recurse, _Vars, 0) ->
    ?ABORT("Max. expansion reached for ENV vars!\n", []);
expand_vars_loop([], [], Vars, _Count) ->
    lists:keysort(1, dict:to_list(Vars));
expand_vars_loop([], Recurse, Vars, Count) ->
    expand_vars_loop(Recurse, [], Vars, Count-1);
expand_vars_loop([{K, V} | Rest], Recurse, Vars, Count) ->
    %% Identify the variables that need expansion in this value
    ReOpts = [global, {capture, all_but_first, list}],
    case re:run(V, "\\\${?(\\w+)}?", ReOpts) of
        {match, Matches} ->
            %% Identify the unique variables that need to be expanded
            UniqueMatches = lists:usort([M || [M] <- Matches]),

            %% For each variable, expand it and return the final
            %% value. Note that if we have a bunch of unresolvable
            %% variables, nothing happens and we don't bother
            %% attempting further expansion
            case expand_keys_in_value(UniqueMatches, V, Vars) of
                V ->
                    %% No change after expansion; move along
                    expand_vars_loop(Rest, Recurse, Vars, Count);
                Expanded ->
                    %% Some expansion occurred; move to next k/v but
                    %% revisit this value in the next loop to check
                    %% for further expansion
                    NewVars = dict:store(K, Expanded, Vars),
                    expand_vars_loop(Rest, [{K, Expanded} | Recurse],
                                     NewVars, Count)
            end;

        nomatch ->
            %% No values in this variable need expansion; move along
            expand_vars_loop(Rest, Recurse, Vars, Count)
    end.

expand_keys_in_value([], Value, _Vars) ->
    Value;
expand_keys_in_value([Key | Rest], Value, Vars) ->
    NewValue = case dict:find(Key, Vars) of
                   {ok, KValue} ->
                       rebar_utils:expand_env_variable(Value, Key, KValue);
                   error ->
                       Value
               end,
    expand_keys_in_value(Rest, NewValue, Vars).

expand_command(TmplName, Env, InFiles, OutFile) ->
    Cmd0 = proplists:get_value(TmplName, Env),
    Cmd1 = rebar_utils:expand_env_variable(Cmd0, "PORT_IN_FILES", InFiles),
    rebar_utils:expand_env_variable(Cmd1, "PORT_OUT_FILE", OutFile).

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
filter_env([], Acc) ->
    lists:reverse(Acc);
filter_env([{ArchRegex, Key, Value} | Rest], Acc) ->
    case rebar_utils:is_arch(ArchRegex) of
        true ->
            filter_env(Rest, [{Key, Value} | Acc]);
        false ->
            filter_env(Rest, Acc)
    end;
filter_env([{Key, Value} | Rest], Acc) ->
    filter_env(Rest, [{Key, Value} | Acc]).

erts_dir() ->
    lists:concat([code:root_dir(), "/erts-", erlang:system_info(version)]).

os_env() ->
    Os = [list_to_tuple(re:split(S, "=", [{return, list}, {parts, 2}])) ||
             S <- os:getenv()],
    %% Drop variables without a name (win32)
    [T1 || {K, _V} = T1 <- Os, K =/= []].

select_compile_template(drv, Compiler) ->
    select_compile_drv_template(Compiler);
select_compile_template(exe, Compiler) ->
    select_compile_exe_template(Compiler).

select_compile_drv_template("$CC")  -> "DRV_CC_TEMPLATE";
select_compile_drv_template("$CXX") -> "DRV_CXX_TEMPLATE".

select_compile_exe_template("$CC")  -> "EXE_CC_TEMPLATE";
select_compile_exe_template("$CXX") -> "EXE_CXX_TEMPLATE".

select_link_template(Target) ->
    case target_type(Target) of
        drv -> "DRV_LINK_TEMPLATE";
        exe -> "EXE_LINK_TEMPLATE"
    end.

target_type(Target) -> target_type1(filename:extension(Target)).

target_type1(".so")  -> drv;
target_type1(".dll") -> drv;
target_type1("")     -> exe;
target_type1(".exe") -> exe.

erl_interface_dir(Subdir) ->
    case code:lib_dir(erl_interface, Subdir) of
        {error, bad_name} ->
            throw({error, {erl_interface,Subdir,"code:lib_dir(erl_interface)"
                           "is unable to find the erl_interface library."}});
        Dir -> Dir
    end.

default_env() ->
    [
     {"CC" , "cc"},
     {"CXX", "c++"},
     {"DRV_CXX_TEMPLATE",
      "$CXX -c $CXXFLAGS $DRV_CFLAGS $PORT_IN_FILES -o $PORT_OUT_FILE"},
     {"DRV_CC_TEMPLATE",
      "$CC -c $CFLAGS $DRV_CFLAGS $PORT_IN_FILES -o $PORT_OUT_FILE"},
     {"DRV_LINK_TEMPLATE",
      "$CC $PORT_IN_FILES $LDFLAGS $DRV_LDFLAGS -o $PORT_OUT_FILE"},
     {"EXE_CXX_TEMPLATE",
      "$CXX -c $CXXFLAGS $EXE_CFLAGS $PORT_IN_FILES -o $PORT_OUT_FILE"},
     {"EXE_CC_TEMPLATE",
      "$CC -c $CFLAGS $EXE_CFLAGS $PORT_IN_FILES -o $PORT_OUT_FILE"},
     {"EXE_LINK_TEMPLATE",
      "$CC $PORT_IN_FILES $LDFLAGS $EXE_LDFLAGS -o $PORT_OUT_FILE"},
     {"DRV_CFLAGS" , "-g -Wall -fPIC $ERL_CFLAGS"},
     {"DRV_LDFLAGS", "-shared $ERL_LDFLAGS"},
     {"EXE_CFLAGS" , "-g -Wall -fPIC $ERL_CFLAGS"},
     {"EXE_LDFLAGS", "$ERL_LDFLAGS"},

     {"ERL_CFLAGS", lists:concat([" -I", erl_interface_dir(include),
                                  " -I", filename:join(erts_dir(), "include"),
                                  " "])},
     {"ERL_EI_LIBDIR", erl_interface_dir(lib)},
     {"ERL_LDFLAGS"  , " -L$ERL_EI_LIBDIR -lerl_interface -lei"},
     {"ERLANG_ARCH"  , rebar_utils:wordsize()},
     {"ERLANG_TARGET", rebar_utils:get_arch()},

     {"darwin", "DRV_LDFLAGS",
      "-bundle -flat_namespace -undefined suppress $ERL_LDFLAGS"},

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
     {"darwin11.*-32", "LDFLAGS", "-arch i386 $LDFLAGS"},

     %% Windows specific flags
     %% add MS Visual C++ support to rebar on Windows
     {"win32", "CC", "cl.exe"},
     {"win32", "CXX", "cl.exe"},
     {"win32", "LINKER", "link.exe"},
     {"win32", "DRV_CXX_TEMPLATE",
      %% DRV_* and EXE_* Templates are identical
      "$CXX /c $CXXFLAGS $DRV_CFLAGS $PORT_IN_FILES /Fo$PORT_OUT_FILE"},
     {"win32", "DRV_CC_TEMPLATE",
      "$CC /c $CFLAGS $DRV_CFLAGS $PORT_IN_FILES /Fo$PORT_OUT_FILE"},
     {"win32", "DRV_LINK_TEMPLATE",
      "$LINKER $PORT_IN_FILES $LDFLAGS $DRV_LDFLAGS /OUT:$PORT_OUT_FILE"},
     %% DRV_* and EXE_* Templates are identical
     {"win32", "EXE_CXX_TEMPLATE",
      "$CXX /c $CXXFLAGS $EXE_CFLAGS $PORT_IN_FILES /Fo$PORT_OUT_FILE"},
     {"win32", "EXE_CC_TEMPLATE",
      "$CC /c $CFLAGS $EXE_CFLAGS $PORT_IN_FILES /Fo$PORT_OUT_FILE"},
     {"win32", "EXE_LINK_TEMPLATE",
      "$LINKER $PORT_IN_FILES $LDFLAGS $EXE_LDFLAGS /OUT:$PORT_OUT_FILE"},
     %% ERL_CFLAGS are ok as -I even though strictly it should be /I
     {"win32", "ERL_LDFLAGS", " /LIBPATH:$ERL_EI_LIBDIR erl_interface.lib ei.lib"},
     {"win32", "DRV_CFLAGS", "/Zi /Wall $ERL_CFLAGS"},
     {"win32", "DRV_LDFLAGS", "/DLL $ERL_LDFLAGS"}
    ].
