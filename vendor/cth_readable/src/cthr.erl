%% @doc Experimental
-module(cthr).
-include_lib("common_test/include/ct.hrl").

-ifndef(MAX_VERBOSITY).
-define(R15_FALLBACK, true).
%% the log level is used as argument to any CT logging function
-define(MIN_IMPORTANCE, 0 ).
-define(LOW_IMPORTANCE, 25).
-define(STD_IMPORTANCE, 50).
-define(HI_IMPORTANCE,  75).
-define(MAX_IMPORTANCE, 99).

%% verbosity thresholds to filter out logging printouts
-define(MIN_VERBOSITY, 0  ).  %% turn logging off
-define(LOW_VERBOSITY, 25 ).
-define(STD_VERBOSITY, 50 ).
-define(HI_VERBOSITY,  75 ).
-define(MAX_VERBOSITY, 100).
-endif.

-export([pal/1, pal/2, pal/3, pal/4, pal/5]).

pal(Format) ->
    pal(default, ?STD_IMPORTANCE, Format, []).

pal(X1,X2) ->
    {Category,Importance,Format,Args} =
    if is_atom(X1)    -> {X1,?STD_IMPORTANCE,X2,[]};
       is_integer(X1) -> {default,X1,X2,[]};
       is_list(X1)    -> {default,?STD_IMPORTANCE,X1,X2}
    end,
    pal(Category,Importance,Format,Args).

pal(X1,X2,X3) ->
    {Category,Importance,Format,Args} =
    if is_atom(X1), is_integer(X2) -> {X1,X2,X3,[]};
       is_atom(X1), is_list(X2)    -> {X1,?STD_IMPORTANCE,X2,X3};
       is_integer(X1)              -> {default,X1,X2,X3}
    end,
    pal(Category,Importance,Format,Args).

-ifdef(R15_FALLBACK).
%% R15 and earlier didn't support log verbosity.

pal(Category,_Importance,Format,Args) ->
    case whereis(cth_readable_failonly) of
        undefined -> % hook not running, passthrough
            ct_logs:tc_pal(Category,Format,Args);
        _ -> % hook running, take over
            Name = case erlang:function_exported(logger, module_info, 0) of
                true -> cth_readable_logger;
                false -> error_logger
            end,
            gen_event:call(Name, cth_readable_failonly,
                           {ct_pal, format(Category,Format,Args)}),
            %% Send to ct group leader
            ct_logs:tc_log(Category, Format, Args),
            ok
    end.

-else.

pal(Category,Importance,Format,Args) ->
    case whereis(cth_readable_failonly) of
        undefined -> % hook not running, passthrough
            ct_logs:tc_pal(Category,Importance,Format,Args);
        _ -> % hook running, take over
            %% Send to error_logger, but only our own handler
            Name = case erlang:function_exported(logger, module_info, 0) of
                true -> cth_readable_logger;
                false -> error_logger
            end,
            gen_event:call(Name, cth_readable_failonly,
                           {ct_pal, format(Category,Importance,Format,Args)}),
            %% Send to ct group leader
            ct_logs:tc_log(Category, Importance, Format, Args),
            ok
    end.

pal(Category,Importance,Format,Args,Opts) ->
    case whereis(cth_readable_failonly) of
        undefined -> % hook not running, passthrough
            ct_logs:tc_pal(Category,Importance,Format,Args,Opts);
        _ -> % hook running, take over
            %% Send to error_logger, but only our own handler
            Name = case erlang:function_exported(logger, module_info, 0) of
                true -> cth_readable_logger;
                false -> error_logger
            end,
            gen_event:call(Name, cth_readable_failonly,
                           {ct_pal, format(Category,Importance,Format,Args)}),
            %% Send to ct group leader
            ct_logs:tc_log(Category, Importance, Format, Args, Opts),
            ok
    end.

-endif.
%%% Replicate CT stuff but don't output it
format(Category, Importance, Format, Args) ->
    VLvl = try ct_util:get_verbosity(Category) of
        undefined ->
            ct_util:get_verbosity('$unspecified');
        {error,bad_invocation} ->
            ?MAX_VERBOSITY;
        {error,_Failure} ->
            ?MAX_VERBOSITY;
        Val ->
            Val
    catch error:undef ->
        ?MAX_VERBOSITY
    end,
    if Importance >= (100-VLvl) ->
            format(Category, Format, Args);
        true ->
            ignore
    end.

format(Category, Format, Args) ->
    Head = get_heading(Category),
    io_lib:format(lists:concat([Head,Format,"\n\n"]), Args).

get_heading(default) ->
    io_lib:format("\n-----------------------------"
                  "-----------------------\n~s\n",
                  [log_timestamp(os:timestamp())]);
get_heading(Category) ->
    io_lib:format("\n-----------------------------"
                  "-----------------------\n~s  ~w\n",
                  [log_timestamp(os:timestamp()),Category]).

log_timestamp({MS,S,US}) ->
    put(log_timestamp, {MS,S,US}),
    {{Year,Month,Day}, {Hour,Min,Sec}} =
                                         calendar:now_to_local_time({MS,S,US}),
    MilliSec = trunc(US/1000),
    lists:flatten(io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B "
                                "~2.10.0B:~2.10.0B:~2.10.0B.~3.10.0B",
                                [Year,Month,Day,Hour,Min,Sec,MilliSec])).

