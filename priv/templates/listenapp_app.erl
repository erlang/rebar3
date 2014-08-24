%%%-------------------------------------------------------------------
%% @copyright {{copyright_holder}} ({{copyright_year}})
%% @author {{author_name}} <{{author_email}}>
%% @doc {{appid}} OTP application callback module.
%% @end
%%%-------------------------------------------------------------------

-module({{appid}}_app).

-behaviour(application).

-define(APP, {{appid}}).

%% Application callbacks
-export([start_phase/3, start/2, stop/1]).

-export([config/0, config/1, config/2,
         start/0, a_start/2]).

%%%===================================================================
%%% Convenience Functions
%%%===================================================================

start() ->
    a_start(?APP, permanent).

config(Key, Default) ->
    case application:get_env(?APP, Key) of
        undefined -> Default;
        {ok, Val} -> Val
    end.

config(Key) ->
    case application:get_env(?APP, Key) of
        undefined -> erlang:error({missing_config, Key});
        {ok, Val} -> Val
    end.

config() ->
    application:get_all_env(?APP).


%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {{appid}}_sup:start_link().

stop(_State) ->
    ok.

start_phase(listen, _Type, _Args) ->
    Dispatch = [{'_',
                 [{'_', {{app}}_http_handler, []}]}
               ],
    cowboy:start_listener(http, 100,
                          cowboy_tcp_transport,
                          [{port, config(http_listen_port)}],
                          cowboy_http_protocol,
                          [{dispatch, Dispatch}]),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

a_start(App, Type) ->
    start_ok(App, Type, application:start(App, Type)).

start_ok(_App, _Type, ok) -> ok;
start_ok(_App, _Type, {error, {already_started, _App}}) -> ok;
start_ok(App, Type, {error, {not_started, Dep}}) ->
    ok = a_start(Dep, Type),
    a_start(App, Type);
start_ok(App, _Type, {error, Reason}) ->
    erlang:error({app_start_failed, App, Reason}).
