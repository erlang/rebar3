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
-module(rebar_otp_app).

-export([compile/2,
         format_error/1]).

-include("rebar.hrl").
-include_lib("providers/include/providers.hrl").

%% ===================================================================
%% Public API
%% ===================================================================

compile(State, App) ->
    %% If we get an .app.src file, it needs to be pre-processed and
    %% written out as a ebin/*.app file. That resulting file will then
    %% be validated as usual.
    App1 = case rebar_app_info:app_file_src(App) of
               undefined ->
                   App;
               AppFileSrc ->
                   File = preprocess(State, App, AppFileSrc),
                   rebar_app_info:app_file(App, File)
           end,

    %% Load the app file and validate it.
    validate_app(State, App1).

format_error(invalid_app_file) ->
    "Failed to read app file";
format_error({file_read, File, Reason}) ->
    io_lib:format("Failed to read ~s for processing: ~p", [File, Reason]);
format_error({invalid_name, File, AppName}) ->
    io_lib:format("Invalid ~s: name of application (~p) must match filename.", [File, AppName]).

%% ===================================================================
%% Internal functions
%% ===================================================================

validate_app(State, App) ->
    AppFile = rebar_app_info:app_file(App),
    case consult_app_file(AppFile) of
        {ok, [{application, AppName, AppData}]} ->
            case validate_name(AppName, AppFile) of
                ok ->
                    validate_app_modules(State, App, AppData);
                Error ->
                    Error
            end;
        {error, Reason} ->
            ?PRV_ERROR({file_read, AppFile, Reason})
    end.

validate_app_modules(State, App, AppData) ->
    %% In general, the list of modules is an important thing to validate
    %% for compliance with OTP guidelines and upgrade procedures.
    %% However, some people prefer not to validate this list.
    AppVsn = proplists:get_value(vsn, AppData),
    case rebar_state:get(State, validate_app_modules, true) of
        true ->
            case rebar_app_utils:validate_application_info(App, AppData) of
                true ->
                    {ok, rebar_app_info:original_vsn(App, AppVsn)};
                Error ->
                    Error
            end;
        false ->
            {ok, rebar_app_info:original_vsn(App, AppVsn)}
    end.

preprocess(State, AppInfo, AppSrcFile) ->
    case consult_app_file(AppSrcFile) of
        {ok, [{application, AppName, AppData}]} ->
            %% Look for a configuration file with vars we want to
            %% substitute. Note that we include the list of modules available in
            %% ebin/ and update the app data accordingly.
            OutDir = rebar_app_info:out_dir(AppInfo),
            AppVars = load_app_vars(State) ++ [{modules, ebin_modules(OutDir)}],
            A1 = apply_app_vars(AppVars, AppData),

            %% AppSrcFile may contain instructions for generating a vsn number
            Vsn = app_vsn(AppSrcFile),
            A2 = lists:keystore(vsn, 1, A1, {vsn, Vsn}),

            %% systools:make_relup/4 fails with {missing_param, registered}
            %% without a 'registered' value.
            A3 = ensure_registered(A2),

            %% Build the final spec as a string
            Spec = io_lib:format("~p.\n", [{application, AppName, A3}]),

            %% Setup file .app filename and write new contents
            EbinDir = rebar_app_info:ebin_dir(AppInfo),
            filelib:ensure_dir(filename:join(EbinDir, "dummy.beam")),
            AppFile = rebar_app_utils:app_src_to_app(OutDir, AppSrcFile),
            ok = rebar_file_utils:write_file_if_contents_differ(AppFile, Spec),

            %% Make certain that the ebin/ directory is available
            %% on the code path
            true = code:add_path(filename:absname(filename:dirname(AppFile))),

            AppFile;
        {error, Reason} ->
            ?PRV_ERROR({file_read, AppSrcFile, Reason})
    end.

load_app_vars(State) ->
    case rebar_state:get(State, app_vars_file, undefined) of
        undefined ->
            ?DEBUG("No app_vars_file defined.", []),
            [];
        Filename ->
            ?INFO("Loading app vars from ~p", [Filename]),
            {ok, Vars} = file:consult(Filename),
            Vars
    end.

apply_app_vars([], AppData) ->
    AppData;
apply_app_vars([{Key, Value} | Rest], AppData) ->
    AppData2 = lists:keystore(Key, 1, AppData, {Key, Value}),
    apply_app_vars(Rest, AppData2).

validate_name(AppName, File) ->
    %% Convert the .app file name to an atom -- check it against the
    %% identifier within the file
    ExpApp = list_to_atom(filename:basename(File, ".app")),
    case ExpApp == AppName of
        true ->
            ok;
        false ->
            ?PRV_ERROR({invalid_name, File, AppName})
    end.

ebin_modules(Dir) ->
    lists:sort([rebar_utils:beam_to_mod(N) ||
                   N <- rebar_utils:beams(filename:join(Dir, "ebin"))]).

ensure_registered(AppData) ->
    case lists:keyfind(registered, 1, AppData) of
        false ->
            [{registered, []} | AppData];
        {registered, _} ->
            %% We could further check whether the value is a list of atoms.
            AppData
    end.

%% In the case of *.app.src we want to give the user the ability to
%% dynamically script the application resource file (think dynamic version
%% string, etc.), in a way similar to what can be done with the rebar
%% config. However, in the case of *.app, rebar should not manipulate
%% that file. This enforces that dichotomy between app and app.src.
consult_app_file(Filename) ->
    case filelib:is_file(Filename) of
        false ->
            throw(?PRV_ERROR(invalid_app_file));
        true ->
            case lists:suffix(".app.src", Filename) of
                false ->
                    file:consult(Filename);
                true ->
                    {ok, rebar_config:consult_file(Filename)}
            end
    end.

app_vsn(AppFile) ->
    case consult_app_file(AppFile) of
        {ok, [{application, _AppName, AppData}]} ->
            AppDir = filename:dirname(filename:dirname(AppFile)),
            rebar_utils:vcs_vsn(get_value(vsn, AppData, AppFile), AppDir);
        {error, Reason} ->
            ?ABORT("Failed to consult app file ~s: ~p\n", [AppFile, Reason])
    end.

get_value(Key, AppInfo, AppFile) ->
    case proplists:get_value(Key, AppInfo) of
        undefined ->
            ?ABORT("Failed to get app value '~p' from '~s'~n", [Key, AppFile]);
        Value ->
            Value
    end.
