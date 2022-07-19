-module(cth_readable_failonly).

-record(state, {id,
                sasl_reset,
                lager_reset,
                handlers=[],
                named,
                has_logger}).
-record(eh_state, {buf=queue:new(),
                   sasl=false,
                   max_events = inf,
                   stored_events = 0,
                   dropped_events = 0}).

%% Callbacks
-export([id/1]).
-export([init/2]).

-export([pre_init_per_suite/3]).
-export([post_init_per_suite/4]).
-export([pre_end_per_suite/3]).
-export([post_end_per_suite/4]).

-export([pre_init_per_group/3]).
-export([post_init_per_group/4]).
-export([pre_end_per_group/3]).
-export([post_end_per_group/4]).

-export([pre_init_per_testcase/3]).
-export([post_end_per_testcase/4]).

-export([on_tc_fail/3]).
-export([on_tc_skip/3, on_tc_skip/4]).

-export([terminate/1]).

%% Error Logger Handler API
-export([init/1,
         handle_event/2, handle_call/2, handle_info/2,
         terminate/2, code_change/3]).

%% Logger API
-export([log/2,
         adding_handler/1, removing_handler/1]).

-define(DEFAULT_LAGER_SINK, lager_event).
-define(DEFAULT_LAGER_HANDLER_CONF,
        [{lager_console_backend, [{level, info}]},
         {lager_file_backend,
            [{file, "log/error.log"}, {level, error},
             {size, 10485760}, {date, "$D0"}, {count, 5}]
         },
         {lager_file_backend,
            [{file, "log/console.log"}, {level, info},
             {size, 10485760}, {date, "$D0"}, {count, 5}]
         }
        ]).

-ifndef(LOCATION).
%% imported from kernel/include/logger.hrl but with replaced unsupported macros
-define(LOCATION,#{mfa=>{?MODULE,log_to_binary,2},
                   line=>?LINE,
                   file=>?FILE}).
-endif.
%% imported from logger_internal.hrl
-define(DEFAULT_FORMATTER, logger_formatter).
-define(DEFAULT_FORMAT_CONFIG, #{legacy_header => true,
                                 single_line => false}).
-define(LOG_INTERNAL(Level,Report),
        case logger:allow(Level,?MODULE) of
            true ->
                %% Spawn this to avoid deadlocks
                _ = spawn(logger,macro_log,[?LOCATION,Level,Report,
                                            logger:add_default_metadata(#{})]),
                ok;
            false ->
                ok
        end).

%% @doc Return a unique id for this CTH.
id(_Opts) ->
    {?MODULE, make_ref()}.

%% @doc Always called before any other callback function. Use this to initiate
%% any common state.
init(Id, Opts) ->
    %% ct:pal replacement needs to know if this hook is enabled -- we use a named proc for that.
    %% Use a `receive' -- if people mock `timer' or reload it, it can kill the
    %% hook and then CT as a whole.
    Named = spawn_link(fun() -> receive after infinity -> ok end end),
    register(?MODULE, Named),
    MaxEvents = proplists:get_value(max_events, Opts, inf),
    HasLogger = has_logger(), % Pre OTP-21 or not
    Cfg = maybe_steal_logger_config(),
    case HasLogger of
        false ->
            error_logger:tty(false), % TODO check if on to begin with
            application:load(sasl); % TODO do this optionally?
        true ->
            %% Assume default logger is running // TODO: check if on to begin with
            logger:add_handler_filter(default, ?MODULE, {fun(_,_) -> stop end, nostate}),
            ok
    end,
    LagerReset = setup_lager(),
    case application:get_env(sasl, sasl_error_logger) of
        {ok, tty} when not HasLogger ->
            ok = gen_event:add_handler(error_logger, ?MODULE, [sasl, {max_events, MaxEvents}]),
            application:set_env(sasl, sasl_error_logger, false),
            {ok, #state{id=Id, sasl_reset={reset, tty}, lager_reset=LagerReset,
                        handlers=[?MODULE], named=Named, has_logger=HasLogger}};
        {ok, tty} when HasLogger ->
            logger:add_handler(?MODULE, ?MODULE, Cfg#{sasl => true, max_events => MaxEvents}),
            {ok, #state{id=Id, lager_reset=LagerReset, handlers=[?MODULE],
                        named=Named, has_logger=HasLogger}};
        _ when HasLogger ->
            logger:add_handler(?MODULE, ?MODULE, Cfg#{sasl => false, max_events => MaxEvents}),
            {ok, #state{id=Id, lager_reset=LagerReset, handlers=[?MODULE],
                        named=Named, has_logger=HasLogger}};
        _ ->
            ok = gen_event:add_handler(error_logger, ?MODULE, [{max_events, MaxEvents}]),
            {ok, #state{id=Id, lager_reset=LagerReset, handlers=[?MODULE],
                        named=Named, has_logger=HasLogger}}
    end.

%% @doc Called before init_per_suite is called.
pre_init_per_suite(_Suite,Config,State) ->
    {Config, State}.

%% @doc Called after init_per_suite.
post_init_per_suite(_Suite,_Config,Return,State) ->
    {Return, State}.

%% @doc Called before end_per_suite.
pre_end_per_suite(_Suite,Config,State) ->
    call_handlers(ignore, State),
    {Config, State}.

%% @doc Called after end_per_suite.
post_end_per_suite(_Suite,_Config,Return,State) ->
    {Return, State}.

%% @doc Called before each init_per_group.
pre_init_per_group(_Group,Config,State) ->
    call_handlers(ignore, State),
    {Config, State}.

%% @doc Called after each init_per_group.
post_init_per_group(_Group,_Config,Return,State) ->
    {Return, State}.

%% @doc Called after each end_per_group.
pre_end_per_group(_Group,Config,State) ->
    {Config, State}.

%% @doc Called after each end_per_group.
post_end_per_group(_Group,_Config,Return,State) ->
    {Return, State}.

%% @doc Called before each test case.
pre_init_per_testcase(_TC,Config,State) ->
    call_handlers(ignore, State),
    {Config, State}.

%% @doc Called after each test case.
post_end_per_testcase(_TC,_Config,ok,State=#state{}) ->
    {ok, State};
post_end_per_testcase(_TC,_Config,Error,State) ->
    {Error, State}.

%% @doc Called after post_init_per_suite, post_end_per_suite, post_init_per_group,
%% post_end_per_group and post_end_per_testcase if the suite, group or test case failed.
on_tc_fail({_TC,_Group}, _Reason, State=#state{}) ->
    call_handlers(flush, State),
    State;
on_tc_fail(_TC, _Reason, State=#state{}) ->
    call_handlers(flush, State),
    State.

%% @doc Called when a test case is skipped by either user action
%% or due to an init function failing (>= 19.3)
on_tc_skip(_Suite, {_TC,_Group}, _Reason, State=#state{}) ->
    call_handlers(flush, State),
    State;
on_tc_skip(_Suite, _TC, _Reason, State=#state{}) ->
    call_handlers(flush, State),
    State.

%% @doc Called when a test case is skipped by either user action
%% or due to an init function failing (pre 19.3)
on_tc_skip({_TC,_Group}, _Reason, State=#state{}) ->
    call_handlers(flush, State),
    State;
on_tc_skip(_TC, _Reason, State=#state{}) ->
    call_handlers(flush, State),
    State.

%% @doc Called when the scope of the CTH is done
terminate(_State=#state{handlers=Handlers, sasl_reset=SReset,
                        lager_reset=LReset, named=Pid, has_logger=HasLogger}) ->

    if HasLogger ->
           logger:remove_handler(?MODULE),
           logger:remove_handler_filter(default, ?MODULE);
       not HasLogger ->
           _ = [gen_event:delete_handler(error_logger, Handler, shutdown)
                || Handler <- Handlers]
    end,
    case SReset of
        {reset, Val} -> application:set_env(sasl, sasl_error_logger, Val);
        undefined -> ok
    end,
    not HasLogger andalso error_logger:tty(true),
    application:unload(sasl), % silently fails if running
    lager_reset(LReset),
    %% Kill the named process signifying this is running
    unlink(Pid),
    Ref = erlang:monitor(process, Pid),
    exit(Pid, shutdown),
    receive
        {'DOWN', Ref, process, Pid, shutdown} -> ok
    end.

%%%%%%%%%%%%% ERROR_LOGGER HANDLER %%%%%%%%%%%%

init(Opts) ->
    {ok,
        #eh_state{
            sasl = proplists:get_bool(sasl, Opts),
            max_events = proplists:get_value(max_events, Opts)
        }}.

handle_event(Event, State) ->
    NewState = case parse_event(Event) of
        ignore -> State;
        logger -> buffer_event({logger, Event}, State);
        sasl -> buffer_event({sasl, {calendar:local_time(), Event}}, State);
        error_logger -> buffer_event({error_logger, {erlang:universaltime(), Event}}, State)
    end,
    {ok, NewState}.

handle_info(_, State) ->
    {ok, State}.

handle_call({lager, _} = Event, State) ->
    %% lager events come in from our fake handler, pre-filtered.
    {ok, ok, buffer_event(Event, State)};
handle_call({ct_pal, ignore}, S) ->
    {ok, ok, S};
handle_call({ct_pal, _}=Event, State) ->
    {ok, ok, buffer_event(Event, State)};
handle_call(ignore, State) ->
    {ok, ok, State#eh_state{buf=queue:new(), stored_events=0}};
handle_call(flush, S=#eh_state{buf=Buf, dropped_events=Dropped}) ->
    Cfg = maybe_steal_logger_config(),
    ShowSASL = sasl_running() orelse sasl_ran(Buf) andalso S#eh_state.sasl,
    SASLType = get_sasl_error_logger_type(),
    not queue:is_empty(Buf) andalso io:put_chars(user, "\n"),
    flush(Buf, Cfg, ShowSASL, SASLType, Dropped),
    {ok, ok, S#eh_state{buf=queue:new(), stored_events=0}};
handle_call(_Event, State) ->
    {ok, ok, State}.

code_change(_, _, State) ->
    {ok, State}.

terminate(_, _) ->
    ok.

buffer_event(Event, S=#eh_state{buf=Buf, max_events=inf}) ->
     %% unbound buffer
    S#eh_state{buf=queue:in(Event, Buf)};
buffer_event(Event, S=#eh_state{buf=Buf, max_events=MaxEvents, stored_events=StoredEvents}) when MaxEvents > StoredEvents ->
     %% bound buffer; buffer not filled yet
    S#eh_state{buf=queue:in(Event, Buf), stored_events=StoredEvents + 1};
buffer_event(Event, S=#eh_state{buf=Buf0, dropped_events=DroppedEvents}) ->
     %% bound buffer; buffer filled
    {_, Buf1} = queue:out(Buf0),
    S#eh_state{buf=queue:in(Event, Buf1), dropped_events=DroppedEvents + 1}.

flush(Buf, Cfg, ShowSASL, SASLType, Dropped) when Dropped > 0 ->
    io:format(user, "(logs are truncated, dropped ~b events)~n", [Dropped]),
    flush(Buf, Cfg, ShowSASL, SASLType);
flush(Buf, Cfg, ShowSASL, SASLType, _) ->
    flush(Buf, Cfg, ShowSASL, SASLType).

flush(Buf, Cfg, ShowSASL, SASLType) ->
    case queue:out(Buf) of
        {empty, _} -> ok;
        {{value, {T, Event}}, NextBuf} ->
            case T of
                error_logger ->
                    error_logger_tty_h:write_event(Event, io);
                sasl when ShowSASL ->
                    sasl_report:write_report(standard_io, SASLType, Event);
                ct_pal ->
                    io:format(user, Event, []);
                lager ->
                    io:put_chars(user, Event);
                logger ->
                    Bin = log_to_binary(Event,Cfg),
                    io:put_chars(user, Bin);
                _ ->
                    ignore
            end,
            flush(NextBuf, Cfg, ShowSASL, SASLType)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%% LOGGER %%%%%%%%%%%%%%%%%%%
adding_handler(Config = #{sasl := SASL, max_events := MaxEvents}) ->
    {ok, Pid} = gen_event:start({local, cth_readable_logger}, []),
    gen_event:add_handler(cth_readable_logger, ?MODULE, [{sasl, SASL}, {max_events, MaxEvents}]),
    {ok, Config#{cth_readable_logger => Pid}}.

removing_handler(#{cth_readable_logger := Pid}) ->
    try gen_event:stop(Pid, shutdown, 1000) of
        ok -> ok
    catch
        error:noproc -> ok;
        error:timeout -> at_least_we_tried
    end.

log(Msg, #{cth_readable_logger := Pid}) ->
    gen_event:notify(Pid, Msg).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

has_logger() ->
    %% Module is present
    erlang:function_exported(logger, module_info, 0).

has_usable_logger() ->
    %% The config is set (lager didn't remove it)
    erlang:function_exported(logger, get_handler_config, 1) andalso
    logger:get_handler_config(default) =/= {error, {not_found, default}}.

maybe_steal_logger_config() ->
    case has_logger() andalso has_usable_logger() of
        false ->
            #{};
        true ->
            case logger:get_handler_config(default) of
                {ok, {_,Cfg}} -> %% OTP-21.0-rc2 result
                    maps:with([formatter], Cfg); % only keep the essential
                 {ok, Cfg} -> %% OTP-21.0 result
                    maps:with([formatter], Cfg) % only keep the essential
            end
    end.

sasl_running() ->
    length([1 || {sasl, _, _} <- application:which_applications()]) > 0.

get_sasl_error_logger_type() ->
    case application:get_env(sasl, errlog_type) of
        {ok, error} -> error;
        {ok, progress} -> progress;
        {ok, all} -> all;
        {ok, Bad} -> exit({bad_config, {sasl, {errlog_type, Bad}}});
        _ -> all
    end.

sasl_ran(Buf) ->
    case queue:out(Buf) of
        {empty, _} -> false;
        {{value, {sasl, {_DateTime, {info_report,_,
            {_,progress, [{application,sasl},{started_at,_}|_]}}}}}, _} -> true;
        {_, Rest} -> sasl_ran(Rest)
    end.

call_handlers(Msg, #state{handlers=Handlers, has_logger=HasLogger}) ->
    Name = if HasLogger -> cth_readable_logger;
              not HasLogger -> error_logger
           end,
    _ = [gen_event:call(Name, Handler, Msg, 300000)
         || Handler <- Handlers],
    ok.

parse_event({_, GL, _}) when node(GL) =/= node() -> ignore;
parse_event({info_report, _GL, {_Pid, progress, _Args}}) -> sasl;
parse_event({error_report, _GL, {_Pid, supervisor_report, _Args}}) -> sasl;
parse_event({error_report, _GL, {_Pid, crash_report, _Args}}) -> sasl;
parse_event({error, _GL, {_Pid, _Format, _Args}}) -> error_logger;
parse_event({info_msg, _GL, {_Pid, _Format, _Args}}) -> error_logger;
parse_event({warning_msg, _GL, {_Pid, _Format, _Args}}) -> error_logger;
parse_event({error_report, _GL, {_Pid, _Format, _Args}}) -> error_logger;
parse_event({info_report, _GL, {_Pid, _Format, _Args}}) -> error_logger;
parse_event({warning_report, _GL, {_Pid, _Format, _Args}}) -> error_logger;
parse_event(Map) when is_map(Map) -> logger;
parse_event(_) -> sasl. % sasl does its own filtering

setup_lager() ->
    case application:load(lager) of
        {error, {"no such file or directory", _}} ->
            %% app not available
            undefined;
        _ -> % it's show time
            %% Keep lager from throwing us out
            WhiteList = application:get_env(lager, error_logger_whitelist, []),
            application:set_env(lager, error_logger_whitelist, [?MODULE|WhiteList]),
            InitConf = application:get_env(lager, handlers, ?DEFAULT_LAGER_HANDLER_CONF),
            %% Add ourselves to the config
            NewConf = case proplists:get_value(lager_console_backend, InitConf) of
                undefined -> % no console backend running
                    InitConf;
                Opts ->
                    [{cth_readable_lager_backend, Opts}
                     | InitConf -- [{lager_console_backend, Opts}]]
            end,
            application:set_env(lager, handlers, NewConf),
            %% check if lager is running and override!
            case {whereis(lager_sup),
                  proplists:get_value(cth_readable_lager_backend, NewConf)} of
                {undefined, _} ->
                    InitConf;
                {_, undefined} ->
                    InitConf;
                {_, LOpts} ->
                    swap_lager_handlers(lager_console_backend,
                                        cth_readable_lager_backend, LOpts),
                    InitConf
            end
    end.

lager_reset(undefined) ->
    ok;
lager_reset(InitConf) ->
    %% Reset the whitelist
    WhiteList = application:get_env(lager, error_logger_whitelist, []),
    application:set_env(lager, error_logger_whitelist, WhiteList--[?MODULE]),
    %% Swap them handlers again
    Opts = proplists:get_value(lager_console_backend, InitConf),
    application:set_env(lager, handlers, InitConf),
    case {whereis(lager_sup), Opts} of
        {undefined, _} -> % not running
            ok;
        {_, undefined} -> % not scheduled
            ok;
        {_, _} ->
            swap_lager_handlers(cth_readable_lager_backend,
                                lager_console_backend, Opts)
     end.

swap_lager_handlers(Old, New, Opts) ->
    gen_event:delete_handler(?DEFAULT_LAGER_SINK, Old, shutdown),
    lager_app:start_handler(?DEFAULT_LAGER_SINK,
                            New, Opts).

%% Imported from Erlang/OTP -- this function used to be public in OTP-20,
%% but was then taken public by OTP-21, which broke functionality.
%% Original at https://raw.githubusercontent.com/erlang/otp/OTP-21.2.5/lib/kernel/src/logger_h_common.erl
log_to_binary(#{msg:={report,_},meta:=#{report_cb:=_}}=Log,Config) ->
    do_log_to_binary(Log,Config);
log_to_binary(#{msg:={report,_},meta:=Meta}=Log,Config) ->
    DefaultReportCb = fun logger:format_otp_report/1,
    do_log_to_binary(Log#{meta=>Meta#{report_cb=>DefaultReportCb}},Config);
log_to_binary(Log,Config) ->
    do_log_to_binary(Log,Config).

do_log_to_binary(Log,Config) ->
    {Formatter,FormatterConfig} =
        maps:get(formatter,Config,{?DEFAULT_FORMATTER,?DEFAULT_FORMAT_CONFIG}),
    String = try_format(Log,Formatter,FormatterConfig),
    try string_to_binary(String)
    catch C2:R2 ->
            ?LOG_INTERNAL(debug,[{formatter_error,Formatter},
                                 {config,FormatterConfig},
                                 {log_event,Log},
                                 {bad_return_value,String},
                                 {catched,{C2,R2,[]}}]),
            <<"FORMATTER ERROR: bad return value">>
    end.

try_format(Log,Formatter,FormatterConfig) ->
    try Formatter:format(Log,FormatterConfig)
    catch
        C:R ->
            ?LOG_INTERNAL(debug,[{formatter_crashed,Formatter},
                                 {config,FormatterConfig},
                                 {log_event,Log},
                                 {reason,
                                  {C,R,[]}}]),
            case {?DEFAULT_FORMATTER,#{}} of
                {Formatter,FormatterConfig} ->
                    "DEFAULT FORMATTER CRASHED";
                {DefaultFormatter,DefaultConfig} ->
                    try_format(Log#{msg=>{"FORMATTER CRASH: ~tp",
                                          [maps:get(msg,Log)]}},
                              DefaultFormatter,DefaultConfig)
            end
    end.

string_to_binary(String) ->
    case unicode:characters_to_binary(String) of
        Binary when is_binary(Binary) ->
            Binary;
        Error ->
            throw(Error)
    end.
