%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2009, 2010 Dave Smith (dizzyd@dizzyd.com)
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
-module(rebar_asn1_compiler).
-author('ruslan@babayev.com').

-export([compile/2,
         clean/2]).

-include("rebar.hrl").

%% ===================================================================
%% Public API
%% ===================================================================

-spec compile(Config::rebar_config:config(), AppFile::file:filename()) -> 'ok'.
compile(Config, _AppFile) ->
    rebar_base_compiler:run(Config, filelib:wildcard("asn1/*.asn1"),
                            "asn1", ".asn1", "src", ".erl",
                            fun compile_asn1/3).

-spec clean(Config::rebar_config:config(), AppFile::file:filename()) -> 'ok'.
clean(_Config, _AppFile) ->
    rebar_file_utils:delete_each(asn_generated_files("asn1", "src")),
    ok.

-spec compile_asn1(file:filename(), file:filename(),
                   rebar_config:config()) -> ok.
compile_asn1(Source, Target, Config) ->
    ok = rebar_utils:ensure_dir(Target),
    Opts = [{outdir, "src"}, noobj] ++ rebar_config:get(Config, asn1_opts, []),
    case asn1ct:compile(Source, Opts) of
        ok ->
            ok;
        {error, _Reason} ->
            ?FAIL
    end.

asn_generated_files(AsnDir, SrcDir) ->
    lists:foldl(
        fun(AsnFile, Acc) ->
                Base = filename:rootname(filename:basename(AsnFile)),
                filelib:wildcard(filename:join([SrcDir, Base ++ ".*"])) ++ Acc
        end,
        [],
        filelib:wildcard(filename:join([AsnDir, "*.asn1"]))
       ).
