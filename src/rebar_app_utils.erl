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
-module(rebar_app_utils).

-export([find/2,
         find/3,
         is_app_src/1,
         app_src_to_app/2,
         validate_application_info/1,
         validate_application_info/2,
         format_error/1]).

-include("rebar.hrl").
-include_lib("providers/include/providers.hrl").

%% ===================================================================
%% Public API
%% ===================================================================

-spec find(binary(), [rebar_app_info:t()]) -> {ok, rebar_app_info:t()} | error.
find(Name, Apps) ->
    ec_lists:find(fun(App) -> rebar_app_info:name(App) =:= Name end, Apps).

-spec find(binary(), binary(), [rebar_app_info:t()]) -> {ok, rebar_app_info:t()} | error.
find(Name, Vsn, Apps) ->
    ec_lists:find(fun(App) ->
                          rebar_app_info:name(App) =:= Name
                              andalso rebar_app_info:original_vsn(App) =:= Vsn
                  end, Apps).

is_app_src(Filename) ->
    %% If removing the extension .app.src yields a shorter name,
    %% this is an .app.src file.
    Filename =/= filename:rootname(Filename, ".app.src").

app_src_to_app(OutDir, Filename) ->
    AppFile =
        case lists:suffix(".app.src", Filename) of
            true ->
                filename:join([OutDir, "ebin", filename:basename(Filename, ".app.src") ++ ".app"]);
            false ->
                filename:join([OutDir, "ebin", filename:basename(Filename,
                                                                 ".app.src.script") ++ ".app"])
        end,
    filelib:ensure_dir(AppFile),
    AppFile.

-spec validate_application_info(rebar_app_info:t()) -> boolean().
validate_application_info(AppInfo) ->
    validate_application_info(AppInfo, rebar_app_info:app_details(AppInfo)).

validate_application_info(AppInfo, AppDetail) ->
    EbinDir = rebar_app_info:ebin_dir(AppInfo),
    case rebar_app_info:app_file(AppInfo) of
        undefined ->
            false;
        AppFile ->
            case proplists:get_value(modules, AppDetail) of
                undefined ->
                    ?PRV_ERROR({module_list, AppFile});
                List ->
                    has_all_beams(EbinDir, List)
            end
    end.

format_error(Error) ->
    io_lib:format("~p", [Error]).

%% ===================================================================
%% Internal functions
%% ===================================================================

-spec has_all_beams(file:filename_all(), [module()]) ->
    true | ?PRV_ERROR({missing_module, module()}).
has_all_beams(EbinDir, [Module | ModuleList]) ->
    BeamFile = filename:join([EbinDir,
                              ec_cnv:to_list(Module) ++ ".beam"]),
    case filelib:is_file(BeamFile) of
        true ->
            has_all_beams(EbinDir, ModuleList);
        false ->
            ?PRV_ERROR({missing_module, Module})
    end;
has_all_beams(_, []) ->
    true.
