-module(foo_test_worker).

-behaviour(gen_server).
-behaviour(foo_worker).

-export([start_link/0,
         start_link/1,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3,
         status/0]).

-include_lib("kernel/include/inet.hrl").

start_link() -> start_link(undefined).

start_link(Args) -> gen_server:start_link(?MODULE, Args, []).

init([]) -> {ok, undefined}.

handle_call(_Event, _From, State) -> {reply, ok, State}.

handle_cast(_Event, State) -> {noreply, State}.

handle_info(_Info, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

status() -> busy.
