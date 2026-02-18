# Custom Compiler Plugins

This tutorial shows you how to write plugins that provide wholesale new compilers. This is something you should use if you want to be compatible with Rebar3 prior to version 3.7.0, or when your compiler requires features outside of the scope of what is provided by the compiler plugin behaviour.

Often applications have non-Erlang code that needs compiling, such as DTL templates, C code for generating parsers from PEG files, etc. The plugin providers that implement these compilers should be in their own namespace and must be hooked to the main `compile` provider if they are to automatically run when a user invokes `compile`:

```erlang
{provider_hooks, [
    {post, [{compile, {pc, compile}}]}
]}.
```

In the above example the namespace `pc` (port compiler) has a provider named `compile` that we have set to run after the main `compile` provider.

## Example Provider

We'll implement an example provider that "compiles" files in the directory `exc_files/` with the extension `.exc` to the priv directory of the application. The full source code can be found on [github](https://github.com/tsloughter/rebar3_ex_compiler).

The definitions are similar to what was done in the [Building Plugins](tutorials/building_plugins.md) tutorial, but in this case we also have a `NAMESPACE` macro. This is important because the provider name is `compile` which, without defining a new namespace, would conflict with the existing `default` namespace `compile` provider.

```erlang
-module(rebar3_prv_ex_compiler).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, compile).
-define(NAMESPACE, exc).
-define(DEPS, [{default, app_discovery}]).
```

The same is done for `init/1`, similar to the previous tutorial, but with `namespace` added to the properties:

```erlang
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},
            {namespace, ?NAMESPACE},
            {module, ?MODULE},
            {bare, true},
            {deps, ?DEPS},
            {example, "rebar3 exc compile"},
            {opts, []},
            {short_desc, "An example rebar compile plugin"},
            {desc, ""}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.
```

Now for the meat of the provider. Since the provider is for compiling part of an Erlang application we must find what application it is we are currently building. If the provider is being run as a hook `current_app` will contain the application record to use. Otherwise it will be undefined, like in the case the user ran `rebar3 exc compile`. For that case the list of applications to compile files for are the `project_apps`, found in `State`.

For each application the `rebar_base_compiler:run/4` function is run, it will run `CompileFun` (in this case `exc_compile/3` on each source file:

```erlang
-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Apps = case rebar_state:current_app(State) of
                undefined ->
                    rebar_state:project_apps(State);
                AppInfo ->
                    [AppInfo]
           end,
    [begin
         Opts = rebar_app_info:opts(AppInfo),
         OutDir = rebar_app_info:out_dir(AppInfo),
         SourceDir = filename:join(rebar_app_info:dir(AppInfo), "exc_files"),
         FoundFiles = rebar_utils:find_files(SourceDir, ".*\\.exc\$"),

         CompileFun = fun(Source, Opts1) ->
                              exc_compile(Opts1, Source, OutDir)
                      end,

         rebar_base_compiler:run(Opts, [], FoundFiles, CompileFun)
     end || AppInfo <- Apps],

    {ok, State}.
```

Finally, `exc_compile/3` reads in the source file and writes it to the output `priv` directory for the application. Yes, we aren't actually "compiling" anything, but if you wanted to, this is where you would:

```erlang
exc_compile(_Opts, Source, OutDir) ->
    {ok, Binary} = file:read_file(Source),
    OutFile = filename:join([OutDir, "priv", filename:basename(Source)]),
    filelib:ensure_dir(OutFile),
    rebar_api:info("Writing out ~s", [OutFile]),
    file:write_file(OutFile, Binary).
```

Finally, in the `rebar.config` of a project our provider can be hooked to the default `compile` provider with a `provider_hook` and run every time `rebar3 compile` is executed. In this case `rebar_state:current_app/1` would return a single `AppInfo` record for the application we are currently building:

```erlang
{provider_hooks, [
    {pre, [{compile, {exc, compile}}]}
]}.
```
