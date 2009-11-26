
-record(global_state, { working_dir }).

-define(CONSOLE(Str, Args), io:format(Str, Args)).
-define(WARN(Str, Args), io:format("WARN: " ++ Str, Args)).

-define(FAIL, throw({error, failed})).

