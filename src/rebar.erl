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
-module(rebar).

-export([main/1]).

main(Args) ->
    %% HACK: Make sure the caller is running w/ r13b03 and reltool >= 0.5.2
    case erlang:system_info(version) < "5.7.4" of
        true ->
            io:format("Rebar requires at least erts 5.7.4; this VM is using ~s\n",
                      [erlang:system_info(version)]),
            halt(1);
        false ->
            ok
    end,

    ReltoolVsn = filename:basename(code:lib_dir(reltool)),
    case ReltoolVsn < "reltool-0.5.2" of
        true ->
            io:format("Rebar requires at least reltool-0.5.2; this VM is using ~s\n",
                      [ReltoolVsn]),
            halt(1);
        false ->
            ok
    end,
    
    case catch(rebar_core:run(Args)) of
        ok ->
            ok;
        _ ->
            halt(1)
    end.
