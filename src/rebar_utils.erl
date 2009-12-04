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
-module(rebar_utils).

-export([get_cwd/0,
         is_arch/1,
         get_os/0,
         sh/2,
         sh_failfast/2,
	 now_str/0]).

-include("rebar.hrl").

%% ====================================================================
%% Public API
%% ====================================================================

get_cwd() ->
    {ok, Dir} = file:get_cwd(),
    Dir.


is_arch(ArchRegex) ->
    Arch = erlang:system_info(system_architecture),
    case re:run(Arch, ArchRegex, [{capture, none}]) of
        match ->
            true;
        nomatch ->
            false
    end.

get_os() ->
    Arch = erlang:system_info(system_architecture),
    case match_first([{"linux", linux}, {"darwin", darwin}], Arch) of
        nomatch ->
            {unknown, Arch};
        ArchAtom ->
            ArchAtom
    end.


sh(Command, Env) ->
    ?INFO("sh: ~s\n~p\n", [Command, Env]),
    Port = open_port({spawn, Command}, [{env, Env}, exit_status, {line, 16384},
                                        use_stdio, stderr_to_stdout]),
    sh_loop(Port).

sh_failfast(Command, Env) ->
    case sh(Command, Env) of
        ok ->
            ok;
        {error, Rc} ->
            ?ERROR("~s failed with error: ~w\n", [Command, Rc]),
            ?FAIL
    end.

now_str() ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:local_time(),
    lists:flatten(io_lib:format("~4b/~2..0b/~2..0b ~2..0b:~2..0b:~2..0b", 
				[Year, Month, Day, Hour, Minute, Second])).

%% ====================================================================
%% Internal functions
%% ====================================================================

match_first([], Val) ->
    nomatch;
match_first([{Regex, MatchValue} | Rest], Val) ->
    case re:run(Val, Regex, [{capture, none}]) of
        match ->
            MatchValue;
        nomatch ->
           match_first(Rest, Val)
    end.

sh_loop(Port) ->            
    receive
        {Port, {data, {_, Line}}} ->
            ?INFO("> ~s\n", [Line]),
            sh_loop(Port);
        {Port, {exit_status, 0}} ->
            ok;
        {Port, {exit_status, Rc}} ->
            {error, Rc}
    end.
