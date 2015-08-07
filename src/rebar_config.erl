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
-module(rebar_config).

-export([consult/1
        ,consult_app_file/1
        ,consult_file/1
        ,consult_lock_file/1
        ,verify_config_format/1
        ,format_error/1

        ,merge_locks/2]).

-callback consult(file:name()) -> [any()].
-callback consult_app_file(file:name()) -> [any()].
-callback consult_file(file:name()) -> [any()].
-callback consult_lock_file(file:name()) -> [any()].
-callback verify_config_format([any()]) -> true.
-callback format_error(term()) -> ok.
-callback merge_locks([proplists:property()], [term()]) -> [proplists:property()].

%% ===================================================================
%% Public API
%% ===================================================================

-spec consult(file:name()) -> [any()].
consult(Dir) ->
    impl(consult, [Dir]).

consult_app_file(File) ->
    impl(consult_app_file, [File]).

consult_lock_file(File) ->
    impl(consult_lock_file, [File]).

consult_file(File) ->
    impl(consult_file, [File]).

verify_config_format(Config) ->
    impl(verify_config_format, [Config]).

merge_locks(Config, Locks) ->
    impl(merge_locks, [Config, Locks]).

format_error(Err) ->
    impl(format_error, [Err]).

%%%
%%% Priv
%%%
impl(FunName, Args) ->
    Mod = application:get_env(rebar, config_loader, rebar_config_default),
    try erlang:apply(Mod, FunName, Args)
    catch
        error:undef ->
            throw({bad_config_loader, Mod})
    end.
