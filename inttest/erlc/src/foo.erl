-module(foo).

-export([start_link/0,
         start_link/1,
         init/1,
         terminate/2,
         handle_info/2,
         handle_call/3,
         handle_cast/2,
         code_change/3]).

-behavior(gen_server).

-include("foo_core.hrl").
-include("foo_extra.hrl").
-include_lib("kernel/include/file.hrl").

-record(state, {node :: node()}).

start_link() -> start_link(undefined).

start_link(Args) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

init(_Args) -> {ok, #state{node=node()}}.

terminate(_Reason, _Data) -> ok.

handle_info(_Info, State) -> {noreply, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_call(_Msg, _From, State) -> {reply, ok, State}.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
