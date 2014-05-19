-module(foo_worker).

-ifdef(NO_CALLBACK_ATTRIBUTE).

-export([behaviour_info/1]).

behaviour_info(callbacks) -> [{status, 0}];
behaviour_info(_) -> undefined.

-else.

-callback status() -> 'idle' | 'busy'.

-endif.
