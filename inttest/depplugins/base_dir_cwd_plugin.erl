-module(base_dir_cwd_plugin).
-export([pre_compile/2]).

pre_compile(_, _) ->
    File = "base_dir_cwd_pre.compile",
    ok = file:write_file(File, <<"base_dir cwd pre_compile plugin">>),
    rebar_log:log(info, "Wrote ~p/~s~n", [rebar_utils:get_cwd(), File]).
