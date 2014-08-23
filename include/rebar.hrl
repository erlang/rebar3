%% TODO: rename FAIL to ABORT once we require at least R13B04 for
%% building rebar. Macros with different arity were not supported by the
%% compiler before 13B04.
-define(FAIL, rebar_utils:abort()).
-define(ABORT(Str, Args), rebar_utils:abort(Str, Args)).

-define(CONSOLE(Str, Args), io:format(Str, Args)).

-define(DEBUG(Str, Args), rebar_log:log(debug, Str, Args)).
-define(INFO(Str, Args), rebar_log:log(info, Str, Args)).
-define(WARN(Str, Args), rebar_log:log(warn, Str, Args)).
-define(ERROR(Str, Args), rebar_log:log(error, Str, Args)).

-define(FMT(Str, Args), lists:flatten(io_lib:format(Str, Args))).

-record(provider,  {name :: atom(),            % The 'user friendly' name of the task
                    provider_impl :: atom(),   % The implementation of the task, maybe fun or
                    bare :: boolean(),         % Indicates whether a build config is needed
                    deps :: [atom()],          % The list of dependencies
                    desc :: string(),          % The description for the task
                    short_desc :: string(),    % A one line short description of the task
                    example :: string(),       % An example of the task usage
                    opts :: list()}).          % The list of options that the task requires/understands

-define(DEFAULT_LIB_DIRS, ["apps", "libs", "."]).
-define(DEFAULT_DEPS_DIRS, ["deps"]).
-define(DEFAULT_CONFIG_FILE, "rebar.config").
-define(LOCK_FILE, "rebar.lock").
