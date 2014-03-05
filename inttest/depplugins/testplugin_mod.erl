-module(testplugin_mod).
-export([pre_compile/2]).

pre_compile(_, _) ->
    File = "plugin_pre.compile",
    ok = file:write_file(File, <<"Yadda!">>),
    rebar_log:log(info, "Wrote ~p/~s~n", [rebar_utils:get_cwd(), File]).
