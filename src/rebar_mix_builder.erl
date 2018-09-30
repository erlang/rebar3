-module(rebar_mix_builder).

-export([build/1,
         format_error/1]).

-include_lib("providers/include/providers.hrl").

build(AppInfo) ->
    AppDir = rebar_app_info:dir(AppInfo),
    case rebar_utils:sh("mix compile --no-deps-check --no-protocol-consolidation",
                        [{cd, AppDir},
                         {return_on_error, true},
                         {use_stdout, true},
                         {env, [{"MIX_BUILD_PATH", filename:join(AppDir, "../../")},
                                {"MIX_ENV", "prod"}]}]) of
        {error, {127, _}} ->
            ?PRV_ERROR({mix_not_found, rebar_app_info:name(AppInfo)});
        {error, {_Code, _Error}} ->
            ?PRV_ERROR({mix_compile_failed, rebar_app_info:name(AppInfo)});
        _ ->
            ok
    end.

format_error({mix_not_found, Name}) ->
    io_lib:format("Elixir and mix must be installed to build application ~ts. "
                  "Install Elixir or check your path and try again.", [Name]);
format_error({mix_compile_failed, Name}) ->
    io_lib:format("Failed to compile application ~ts with mix", [Name]);
format_error(Reason) ->
    io_lib:format("~p", Reason).
