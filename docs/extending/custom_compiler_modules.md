# Custom Compiler Modules

This is a new feature in Rebar3 3.7.0, to write custom compilers to be used with it. It is useful whenever you have files of a different language that you want to build alongside Erlang resources. Starting with Rebar3 3.14.0, the interface was enhanced to allow better tracking of files in a directed acyclic graph (DAG), which lets you annotate build artifacts and allows Rebar3 to track dependencies across applications.

This interface is currently used internally for `.xrl`, `.yrl`, and `.mib` files. Few plugins have tried it.

> #### This is an unstable interface {: .warning}
> Since we have not had many plugin authors try this interface yet, it is marked as unstable and is subject to change.
>
> We are looking for help from contributors to further stabilize it before marking it as stable. You should use this if you are willing to contact us, and help iterate on the > features available in [Custom Compiler Plugins](extending/custom_compiler_plugins.md).
>
> It is possible that your custom compiler requires something more complex. For example, the facilities provided by this interface are insufficient to build projects that run with `mix` as a build tool, and the plugin for that uses [a custom compiler plugin](extending/custom_compiler_plugins.md).

## Current Interface

The following callbacks are defined:

```erlang
%% specify what kind of files to find and where to find them. Rebar3 handles
%% doing all the searching from these concepts.
context(AppInfo) ->
    %% Mandatory Fields
      %% directories containing source files
    #{src_dirs     => ["/path/to/src/"],
      include_dirs => ["/path/to/includes/"],
      src_ext      => ".erl",
      out_mappings => [{".beam", "/path/to/ebin"}],
    %% Optional Fieldsstate passed to dependencies callback
      dependencies_opts => term()}.                 % optional, v3.14+

%% Define which files each of the files depends on, including includes and whatnot.
%% This is then used to create a digraph of all existing files to know how to propagate
%% file changes. The Digraph is passed to other callbacks as `G' and annotates all files
%% with their last changed timestamp
%% Prior to 3.14, the `State' argument was not available.
dependencies("/path/to/file.erl", "/path/to/",
             ["/path/to/all/other_sources/", ...], State) ->
    ["path/to/deps.erl", ...].

%% do your own analysis aided by the graph to specify what needs re-compiling.
%% You can use this to add more or fewer files (i.e. compiler options changed),
%% and specify how to schedule their compilation. One thing we do here for
%% Erlang files is look at the digraph to only rebuild files with newer
%% timestamps than their build artifacts (which are also in the DAG after the
%% first build) or those with compiler options that changed (the
%% compile_and_track callback lets you annotate artifacts)
needed_files(G, ["/path/to/all/files.erl", ...], [{".beam", "/path/to/ebin"}], AppInfo) ->
    %% the returned files to build essentially specify a schedule and priority with special
    %% option sets
     %% Files that _must_ be built first like those in parse transforms, with
     %% different build options
    {{["/top/priority/files.erl"], CompilerOpts},
     %% {Sequential, Parallel} build order for regular files, with shared
     %% compiler options
     {{["/path/to/file.erl", ...], ["other/files/mod.erl", ...]}, CompilerOpts}}.

%% Compilation callback with the ability to track build artifacts in the DAG itself.
%% Introduced in 3.14. Prior to this version, refer to `compile/4'.
compile_and_track("/path/to/file.erl", [{".beam", "/path/to/ebin"}],
                  AppOptDict, CompilerOpts) ->
    %% Successfully built a file, tying it to artifacts with optional metadata
    {ok, [{"/path/to/file.erl", "path/to/ebin/file.beam", Metadata}]} |
    %% Successfully built a file, but it has compiler warnings
    {ok, [{"/path/to/file.erl", "path/to/ebin/file.beam", Metadata}],
         ["Some compiler warning"]} |
    %% Failed build
    {error, ["error strings"], ["warning strings"]} | error.

%% A simpler compilation mechanism which does not track build artifacts into the
%% DAG for the compiler. Change for built files must be figured out from files on
%% disk or other storage.
compile("/path/to/file.erl", [{".beam", "/path/to/ebin"}],
        AppConfig, CompilerOpts) ->
            ok
          | {ok, ["Some compiler warning"]}
          | {ok, ["error strings"], ["warning strings"]}.

%% Just delete files however you need to
clean(["/path/to/file"], AppInfo) -> _.
```

## Initializing a Compiler Module as a Plugin

Register the compiler module in the same place where you would register [a custom compiler plugin](extending/custom_compiler_plugins.md):

```erlang
%% Note: the name of the module matches the name of the plugin application
-module(my_compiler_plugin).
-export([init/1]).

%% Called when Rebar3 first boots, before even parsing the arguments
%% or commands to be run. Purely initiates the provider, and nothing
%% else should be done here.
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    %% Optional:
    %% Provider = providers:create([Options]),
    %% State1 = rebar_state:add_provider(State, Provider),

    %% This adds the new compiler module:
    State1 = rebar_state:append_compilers(State, [my_compiler_mod]),
    %% If needing the new compiler module to take precedence over
    %% other ones (i.e. generating .erl files from another format):
    State2 = rebar_state:prepend_compilers(State1, [translator_mod]),
    {ok, State2}.
```
