%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2014 Tuncer Ayaz
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
-module(rebar_otp_release_tests).

-include_lib("eunit/include/eunit.hrl").

check_otp_release_test() ->
    case rebar_utils:otp_release() of
        %% <= R16
        [$R,N|_] when is_integer(N) ->
            ?assert(true);
        %% >= 17.x
        [N|_]=Rel when is_integer(N) ->
            %% Check that it has at least Major.Minor.
            ?assert(length(string:tokens(Rel, ".")) > 1),

            %% If otp_patch_apply was used and the release version has
            %% a "**" suffix, we drop that part in otp_release/0.
            ?assertEqual(0, string:str(Rel, "*")),

            %% Check that "\n" is dropped in otp_release/0.
            ?assertEqual(0, string:str(Rel, "\n"))
    end.
