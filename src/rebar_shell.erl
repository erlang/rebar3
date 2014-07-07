%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2011 Trifork
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

-module(rebar_shell).
-author("Kresten Krab Thorup <krab@trifork.com>").

-include("rebar.hrl").

-export([shell/2, info/2]).

%% NOTE:
%% this is an attempt to replicate `erl -pa ./ebin -pa deps/*/ebin`. it is
%% mostly successful but does stop and then restart the user io system to get
%% around issues with rebar being an escript and starting in `noshell` mode.
%% it also lacks the ctrl-c interrupt handler that `erl` features. ctrl-c will
%% immediately kill the script. ctrl-g, however, works fine

shell(_Config, _AppFile) ->
    true = code:add_pathz(rebar_utils:ebin_dir()),
    %% scan all processes for any with references to the old user and save them to
    %% update later
    NeedsUpdate = [Pid || Pid <- erlang:processes(),
        proplists:get_value(group_leader, erlang:process_info(Pid)) == whereis(user)
    ],
    %% terminate the current user
    ok = supervisor:terminate_child(kernel_sup, user),
    %% start a new shell (this also starts a new user under the correct group)
    _ = user_drv:start(),
    %% wait until user_drv and user have been registered (max 3 seconds)
    ok = wait_until_user_started(3000),
    %% set any process that had a reference to the old user's group leader to the
    %% new user process
    _ = [erlang:group_leader(whereis(user), Pid) || Pid <- NeedsUpdate],
    %% enable error_logger's tty output
    ok = error_logger:swap_handler(tty),
    %% disable the simple error_logger (which may have been added multiple
    %% times). removes at most the error_logger added by init and the
    %% error_logger added by the tty handler
    ok = remove_error_handler(3),
    %% this call never returns (until user quits shell)
    timer:sleep(infinity).

info(help, shell) ->
    ?CONSOLE(
        "Start a shell with project and deps preloaded similar to~n"
        "'erl -pa ebin -pa deps/*/ebin'.~n",
        []
    ).

remove_error_handler(0) ->
    ?WARN("Unable to remove simple error_logger handler~n", []);
remove_error_handler(N) ->
    case gen_event:delete_handler(error_logger, error_logger, []) of
        {error, module_not_found} -> ok;
        {error_logger, _} -> remove_error_handler(N-1)
    end.

%% Timeout is a period to wait before giving up
wait_until_user_started(0) ->
    ?ABORT("Timeout exceeded waiting for `user` to register itself~n", []),
    erlang:error(timeout);
wait_until_user_started(Timeout) ->
    case whereis(user) of
        %% if user is not yet registered wait a tenth of a second and try again
        undefined -> timer:sleep(100), wait_until_user_started(Timeout - 100);
        _ -> ok
    end.