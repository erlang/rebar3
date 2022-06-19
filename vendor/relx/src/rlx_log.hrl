-ifndef(RLX_LOG).
-define(RLX_LOG, rlx_log).
-endif.

%% logging macros
-define(log_debug(Msg), ?RLX_LOG:log(debug, Msg, [])).
-define(log_warn(Msg), ?RLX_LOG:log(warn, Msg, [])).
-define(log_error(Msg), ?RLX_LOG:log(error, Msg, [])).
-define(log_info(Msg), ?RLX_LOG:log(info, Msg, [])).

-define(log_debug(Msg, Args), ?RLX_LOG:log(debug, Msg, Args)).
-define(log_warn(Msg, Args), ?RLX_LOG:log(warn, Msg, Args)).
-define(log_error(Msg, Args), ?RLX_LOG:log(error, Msg, Args)).
-define(log_info(Msg, Args), ?RLX_LOG:log(info, Msg, Args)).
