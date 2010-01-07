%% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
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
%%   <li>analyze (essentially "dialyzer -r ebin")</li>
%%   <li>build_plt (essentially "dialyzer --build_plt -r &rt;stdlib_lib&lt; &rt;kernel_lib&lt; &rt;mnesia_lib&lt;")</li>
%%   <li>check_plt (essentially "dialyzer --check_plt")</li>
%% </ul>
%% A single option <code>plt</code> can be presented in the <code>dialyzer_opts</code>
%% options in <code>rebar.config</code>. If it is present, it is used as the PLT for the
%% supported commands. Should it not be present, then the default is <code>$HOME/.dialyzer_plt</code>.
%% @reference <a href="http://user.it.uu.se/~kostis/Papers/bugs05.pdf">Experience from developing the Dialyzer:
%% A static analysis tool detecting defects in Erlang applications</a>
%% @reference <a href="http://user.it.uu.se/~kostis/Papers/contracts.pdf">A Language for Specifying Type
%% Contracts in Erlang and its Interaction with Success Typings</a>
%% @reference <a href="http://user.it.uu.se/~kostis/Papers/wrangler.pdf">Gradual Typing of Erlang
%% Programs: A Wrangler Experience</a>
%% @copyright 2010 Dave Smith
%% -------------------------------------------------------------------
-module(rebar_dialyzer).

-export([analyze/2,
         build_plt/2,
         check_plt/2]).

-include("rebar.hrl").

-type(warning() :: {atom(), {string(), integer()}, any()}).

%% ===================================================================
%% Public API
%% ===================================================================

%% @doc Perform static analysis on the contents of the ebin directory.
%% @spec analyze(Config::#config{}, File::string()) -> ok.
-spec(analyze(Config::#config{}, File::string()) -> ok).
analyze(Config, _File) ->
    Plt = plt_path(Config),
    case dialyzer:plt_info(Plt) of
        {ok, _} ->
            try dialyzer:run([{files_rec, ["ebin"]}, {init_plt, Plt}]) of
                Warnings -> output_warnings(Warnings)
            catch
                throw:{dialyzer_error, Reason} ->
                    ?ABORT("~s~n", [Reason])
            end;
        {error, no_such_file} ->
            ?ABORT("The PLT ~s does not exist. Please perform the build_plt command to ~n"
                   "produce the initial PLT. Be aware this operation may take several minutes.", [Plt]);
        {error, not_valid} ->
            ?ABORT("The PLT ~s is not valid.~n", [Plt]);
        {error, _Reason} ->
            ?ABORT("Unable to determine the validity of the PLT ~s~n", [Plt])
    end,
    ok.

%% @doc Build the PLT.
%% @spec build_plt(Config::#config{}, File::string()) -> ok
-spec(build_plt(Config::#config{}, File::string()) -> ok).
build_plt(Config, _File) ->
    Plt = plt_path(Config),

    %% This is the recommended minimal PLT for OTP
    %% (see http://www.erlang.org/doc/apps/dialyzer/dialyzer_chapter.html#id2256857).
    Warnings = dialyzer:run([{analysis_type, plt_build},
                             {files_rec, [
                                 filename:join(code:lib_dir(stdlib), "ebin"),
                                 filename:join(code:lib_dir(kernel), "ebin"),
                                 filename:join(code:lib_dir(mnesia), "ebin")
                             ]},
                             {output_plt, Plt}]),
    case Warnings of
        [] ->
            ?INFO("The built PLT can be found in ~s", [Plt]);
        _ ->
            output_warnings(Warnings)
    end,
    ok.

%% @doc Check whether the PLT is up-to-date (rebuilding it if not).
%% @spec check_plt(Config::#config{}, File::string()) -> ok
-spec(check_plt(Config::#config{}, File::string()) -> ok).
check_plt(Config, _File) ->
    Plt = plt_path(Config),
    try dialyzer:run([{analysis_type, plt_check}, {init_plt, Plt}]) of
        [] ->
            ?CONSOLE("The PLT ~s is up-to-date~n", [Plt]);
        _ ->
            %% @todo Determine whether this is the correct summary.
            ?CONSOLE("The PLT ~s is not up-to-date~n", [Plt])
    catch
        throw:{dialyzer_error, _Reason} ->
            ?CONSOLE("The PLT ~s is not valid.~n", [Plt])
    end,
    ok.

%% ===================================================================
%% Internal functions
%% ===================================================================

%% @doc Render the warnings on the console.
%% @spec output_warnings(Warnings::[warning()]) -> void()
-spec(output_warnings(Warnings::[warning()]) -> none()).
output_warnings(Warnings) ->
    lists:foreach(fun(Warning) ->
                      ?CONSOLE("~s", [dialyzer:format_warning(Warning)])
                  end, Warnings).

%% @doc If the plt option is present in rebar.config return its value, otherwise
%% return $HOME/.dialyzer_plt.
%% @spec plt_path(Config::#config{}) -> string()
-spec(plt_path(Config::#config{}) -> string()).
plt_path(Config) ->
    DialyzerOpts = rebar_config:get(Config, dialyzer_opts, []),
    case proplists:get_value(plt, DialyzerOpts) of
        undefined ->
            filename:join(os:getenv("HOME"), ".dialyzer_plt");
        Plt ->
            Plt
    end.
