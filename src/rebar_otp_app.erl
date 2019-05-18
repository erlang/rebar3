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
    App1 = case rebar_app_info:app_file_src_script(App) of
               undefined ->
                   case rebar_app_info:app_file_src(App) of
                       undefined ->
                           App;
                       AppFileSrc ->
                           File = preprocess(State, App, AppFileSrc),
                           rebar_app_info:app_file(App, File)
                   end;
               AppFileSrcScript ->
                   File = preprocess(State, App, AppFileSrcScript),
                   rebar_app_info:app_file(App, File)
           end,

    %% Load the app file and validate it.
    validate_app(State, App1).

format_error({missing_app_file, Filename}) ->
    io_lib:format("App file is missing: ~ts", [Filename]);
format_error({file_read, AppName, File, Reason}) ->
    io_lib:format("Failed to read required ~ts file for processing the application '~ts': ~ts",
                  [File, AppName, file:format_error(Reason)]);
format_error({invalid_name, File, AppName}) ->
    io_lib:format("Invalid ~ts: name of application (~p) must match filename.", [File, AppName]).

%% ===================================================================
%% Internal functions
%% ===================================================================

validate_app(State, App) ->
    AppFile = rebar_app_info:app_file(App),
    case consult_app_file(AppFile) of
        {ok, [{application, AppName, AppData}]} ->
            case validate_name(AppName, AppFile) of
                ok ->
                    lint_app_file(App, AppData, AppFile),
                    validate_app_modules(State, App, AppData);
                Error ->
                    Error
            end;
        {error, Reason} ->
            ?PRV_ERROR({file_read, rebar_app_info:name(App), ".app", Reason})
    end.

lint_app_file(App, AppData, AppFile) ->
    AppOpts = rebar_app_info:opts(App),
    case rebar_opts:get(AppOpts, lint_app_file, true) of
        true ->
            rebar_app_utils:lint_app_file(AppData, AppFile);
        false ->
            ok
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
            AppVars = load_app_vars(State) ++ [{modules, ebin_modules(AppInfo, OutDir)}],
            A1 = apply_app_vars(AppVars, AppData),

            %% AppSrcFile may contain instructions for generating a vsn number
            Vsn = app_vsn(AppInfo, AppData, AppSrcFile, State),
            A2 = lists:keystore(vsn, 1, A1, {vsn, Vsn}),

            %% systools:make_relup/4 fails with {missing_param, registered}
            %% without a 'registered' value.
            A3 = ensure_registered(A2),

            %% some tools complain if a description is not present.
            A4 = ensure_description(A3),

            %% Build the final spec as a string
            Spec = io_lib:format("~p.\n", [{application, AppName, A4}]),

            %% Setup file .app filename and write new contents
            EbinDir = rebar_app_info:ebin_dir(AppInfo),
            rebar_file_utils:ensure_dir(EbinDir),
            AppFile = rebar_app_utils:app_src_to_app(OutDir, AppSrcFile),
            ok = rebar_file_utils:write_file_if_contents_differ(AppFile, Spec, utf8),

            AppFile;
        {error, Reason} ->
            throw(?PRV_ERROR({file_read, rebar_app_info:name(AppInfo), ".app.src", Reason}))
    end.

load_app_vars(State) ->
    case rebar_state:get(State, app_vars_file, undefined) of
        undefined ->
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

ebin_modules(AppInfo, Dir) ->
    Beams = lists:sort(rebar_utils:beams(filename:join(Dir, "ebin"))),
    ExtraDirs = extra_dirs(AppInfo),
    F = fun(Beam) -> not in_extra_dir(AppInfo, Beam, ExtraDirs) end,
    Filtered = lists:filter(F, Beams),
    [rebar_utils:beam_to_mod(N) || N <- Filtered].

extra_dirs(State) ->
    Extras = rebar_dir:extra_src_dirs(rebar_app_info:opts(State)),
    SrcDirs = rebar_dir:src_dirs(rebar_app_info:opts(State), ["src"]),
    %% remove any dirs that are defined in `src_dirs` from `extra_src_dirs`
    Extras -- SrcDirs.

in_extra_dir(AppInfo, Beam, Dirs) ->
    lists:any(fun(Dir) -> lists:prefix(filename:join([rebar_app_info:out_dir(AppInfo), Dir]),
                                       beam_src(Beam)) end,
              Dirs).

beam_src(Beam) ->
    case beam_lib:chunks(Beam, [compile_info]) of
        {ok, {_mod, Chunks}} ->
            CompileInfo = proplists:get_value(compile_info, Chunks, []),
            proplists:get_value(source, CompileInfo, []);
        {error, beam_lib, Reason} ->
            ?WARN("Couldn't read debug info from ~p for reason: ~p", [Beam, Reason]),
            []
    end.

ensure_registered(AppData) ->
    case lists:keyfind(registered, 1, AppData) of
        false ->
            [{registered, []} | AppData];
        {registered, _} ->
            %% We could further check whether the value is a list of atoms.
            AppData
    end.

ensure_description(AppData) ->
    case lists:keyfind(description, 1, AppData) of
        false ->
            %% Required for releases to work.
            [{description, ""} | AppData];
        {description, _} ->
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
            {error, enoent};
        true ->
            case lists:suffix(".app.src", Filename)
                orelse lists:suffix(".app.src.script", Filename) of
                false ->
                    file:consult(Filename);
                true ->
                    {ok, rebar_config:consult_app_file(Filename)}
            end
    end.

app_vsn(AppInfo, AppData, AppFile, State) ->
    rebar_utils:vcs_vsn(AppInfo, get_value(vsn, AppData, AppFile), State).

get_value(Key, AppInfo, AppFile) ->
    case proplists:get_value(Key, AppInfo) of
        undefined ->
            ?ABORT("Failed to get app value '~p' from '~ts'~n", [Key, AppFile]);
        Value ->
            Value
    end.
