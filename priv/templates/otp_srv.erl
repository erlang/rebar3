%%%-------------------------------------------------------------------
%% @copyright {{copyright_holder}} ({{copyright_year}})
%% @author {{author_name}} <{{author_email}}>
%% @doc {{appid}} {{srvid}} OTP gen_server.
%% @end
%%%-------------------------------------------------------------------

-module({{appid}}_{{srvid}}).

-behaviour(gen_server).

-include("{{appid}}_log.hrl").

%% API
-export([start_link/0
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {replaceme}).

%%====================================================================
%% API
%%====================================================================
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @private
init([]) ->
    {ok, #state{}}.

%% @private
handle_call(Call, _From, State) ->
    ?WARN("Unexpected call ~p.", [Call]),
    {noreply, State}.

%% @private
handle_cast(Msg, State) ->
    ?WARN("Unexpected cast ~p", [Msg]),
    {noreply, State}.

%% @private
handle_info(Info, State) ->
    ?WARN("Unexpected info ~p", [Info]),
    {noreply, State}.

%% @private
terminate(_Reason, _State) ->
    ok.

%% @private
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% internal functions
%%====================================================================
