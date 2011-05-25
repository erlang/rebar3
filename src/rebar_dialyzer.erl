%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2010 Dave Smith (dizzyd@dizzyd.com)
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
%% @author Dave Smith <dizzyd@dizzyd.com>
%% @doc rebar_dialyzer supports the following commands:
%% <ul>
%%   <li>dialyze (essentially "dialyzer ebin" or "dialyzer --src src")</li>
%%   <li>build-plt (essentially "dialyzer --build_plt -r &lt;app_dirs&gt;")</li>
%%   <li>check-plt (essentially "dialyzer --check_plt")</li>
%% </ul>
%% A single option <code>plt</code> can be presented in the
%% <code>dialyzer_opts</code> options in <code>rebar.config</code>. If it
%% is present, it is used as the PLT for the supported commands. Should it
%% not be present, then the default is <code>$HOME/.dialyzer_plt</code> or
%% <code>$REBAR_PLT_DIR/.dialyzer_plt</code> if $REBAR_PLT_DIR is defined.
%%
%% @reference <a href="http://user.it.uu.se/~kostis/Papers/bugs05.pdf">
%% Experience from developing the Dialyzer: A static analysis tool detecting
%% defects in Erlang applications</a>
%% @reference <a href="http://user.it.uu.se/~kostis/Papers/contracts.pdf">
%% A Language for Specifying Type Contracts in Erlang and its Interaction
%% with Success Typings</a>
%% @reference <a href="http://user.it.uu.se/~kostis/Papers/wrangler.pdf">Gradual
%% Typing of Erlang Programs: A Wrangler Experience</a>
%% @copyright 2010 Dave Smith
%% -------------------------------------------------------------------
-module(rebar_dialyzer).

-export([dialyze/2,
         'build-plt'/2,
         'check-plt'/2]).

-include("rebar.hrl").

-type warning() :: {atom(), {string(), integer()}, any()}.

%% ===================================================================
%% Public API
%% ===================================================================

%% @doc Perform static analysis on the contents of the ebin directory.
-spec dialyze(Config::rebar_config:config(), File::file:filename()) -> ok.
dialyze(Config, File) ->
    Plt = existing_plt_path(Config, File),
    case dialyzer:plt_info(Plt) of
        {ok, _} ->
            FromSrc = proplists:get_bool(src, rebar_config:get(Config,
                                                               dialyzer_opts,
                                                               [])),
            DialyzerOpts0 = case FromSrc of
                                true ->
                                    [{files_rec, ["src"]}, {init_plt, Plt},
                                     {from, src_code}];
                                false ->
                                    [{files_rec, ["ebin"]}, {init_plt, Plt}]
                            end,
            WarnOpts = warnings(Config),
            DialyzerOpts = case WarnOpts of
                               [] -> DialyzerOpts0;
                               _ -> [{warnings, WarnOpts}|DialyzerOpts0]
                           end,
            ?DEBUG("DialyzerOpts: ~p~n", [DialyzerOpts]),
            try dialyzer:run(DialyzerOpts) of
                [] ->
                    ok;
                Warnings ->
                    print_warnings(Warnings)
            catch
                throw:{dialyzer_error, Reason} ->
                    ?ABORT("~s~n", [Reason])
            end;
        {error, no_such_file} ->
            ?ABORT("The PLT ~s does not exist. Please perform the build-plt "
                   "command to ~n"
                   "produce the initial PLT. Be aware that this operation may "
                   "take several minutes.~n", [Plt]);
        {error, read_error} ->
            ?ABORT("Unable to read PLT ~n~n", [Plt]);
        {error, not_valid} ->
            ?ABORT("The PLT ~s is not valid.~n", [Plt])
    end,
    ok.

%% @doc Build the PLT.
-spec 'build-plt'(Config::rebar_config:config(), File::file:filename()) -> ok.
'build-plt'(Config, File) ->
    Plt = new_plt_path(Config, File),

    Apps = rebar_app_utils:app_applications(File),

    ?DEBUG("Build PLT ~s including following apps:~n~p~n", [Plt, Apps]),
    Warnings = dialyzer:run([{analysis_type, plt_build},
                             {files_rec, app_dirs(Apps)},
                             {output_plt, Plt}]),
    case Warnings of
        [] ->
            ?INFO("The built PLT can be found in ~s~n", [Plt]);
        _ ->
            print_warnings(Warnings)
    end,
    ok.

%% @doc Check whether the PLT is up-to-date.
-spec 'check-plt'(Config::rebar_config:config(), File::file:filename()) -> ok.
'check-plt'(Config, File) ->
    Plt = existing_plt_path(Config, File),
    try dialyzer:run([{analysis_type, plt_check}, {init_plt, Plt}]) of
        [] ->
            ?CONSOLE("The PLT ~s is up-to-date~n", [Plt]);
        _ ->
            %% @todo Determine whether this is the correct summary.
            ?ABORT("The PLT ~s is not up-to-date~n", [Plt])
    catch
        throw:{dialyzer_error, _Reason} ->
            ?ABORT("The PLT ~s is not valid.~n", [Plt])
    end,
    ok.

%% ===================================================================
%% Internal functions
%% ===================================================================

%% @doc Obtain the library paths for the supplied applications.
-spec app_dirs(Apps::[atom()]) -> [file:filename()].
app_dirs(Apps) ->
    [filename:join(Path, "ebin")
     || Path <- [code:lib_dir(App) || App <- Apps], erlang:is_list(Path)].

%% @doc Render the warnings on the console.
-spec print_warnings(Warnings::[warning(), ...]) -> no_return().
print_warnings(Warnings) ->
    lists:foreach(fun(Warning) ->
                          ?CONSOLE("~s", [dialyzer:format_warning(Warning)])
                  end, Warnings),
    ?FAIL.

%% @doc If the plt option is present in rebar.config return its value,
%% otherwise return $HOME/.dialyzer_plt or $REBAR_PLT_DIR/.dialyzer_plt.
-spec new_plt_path(Config::rebar_config:config(),
                   File::file:filename()) -> file:filename().
new_plt_path(Config, File) ->
    AppName = rebar_app_utils:app_name(File),
    DialyzerOpts = rebar_config:get(Config, dialyzer_opts, []),
    case proplists:get_value(plt, DialyzerOpts) of
        undefined ->
            case os:getenv("REBAR_PLT_DIR") of
                false ->
                    filename:join(os:getenv("HOME"),
                                  "." ++ atom_to_list(AppName)
                                  ++ "_dialyzer_plt");
                PltDir ->
                    filename:join(PltDir, "." ++ atom_to_list(AppName)
                                  ++ "_dialyzer_plt")
            end;
        Plt ->
            Plt
    end.

%% @doc If the plt option is present in rebar.config and the file exists
%% return its value or if $HOME/.AppName_dialyzer_plt exists return that.
%% Otherwise return $HOME/.dialyzer_plt if it exists or abort.
%% If $REBAR_PLT_DIR is defined, it is used instead of $HOME.
-spec existing_plt_path(Config::rebar_config:config(),
                        File::file:filename()) -> file:filename().
existing_plt_path(Config, File) ->
    AppName = rebar_app_utils:app_name(File),
    DialyzerOpts = rebar_config:get(Config, dialyzer_opts, []),
    Home = os:getenv("HOME"),
    Base = case os:getenv("REBAR_PLT_DIR") of
               false ->
                   Home;
               PltDir ->
                   PltDir
           end,
    case proplists:get_value(plt, DialyzerOpts) of
        undefined ->
            AppPlt = filename:join(Base, "." ++ atom_to_list(AppName)
                                   ++ "_dialyzer_plt"),
            case filelib:is_regular(AppPlt) of
                true ->
                    AppPlt;
                false ->
                    BasePlt = filename:join(Base, ".dialyzer_plt"),
                    case filelib:is_regular(BasePlt) of
                        true ->
                            BasePlt;
                        false ->
                            ?ABORT("No PLT found~n", [])
                    end
            end;
        "~/" ++ Plt ->
            filename:join(Home, Plt);
        Plt ->
            Plt
    end.

%% @doc If the warnings option is present in rebar.config return its value,
%% otherwise return [].
-spec warnings(Config::rebar_config:config()) -> list().
warnings(Config) ->
    DialyzerOpts = rebar_config:get(Config, dialyzer_opts, []),
    proplists:get_value(warnings, DialyzerOpts, []).
