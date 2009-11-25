
-record(global_state, { working_dir }).

-define(CONSOLE(Str, Args), io:format(Str, Args)).
