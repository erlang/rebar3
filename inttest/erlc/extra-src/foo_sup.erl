-module(foo_sup).

-behavior(supervisor).

-export([start_link/0,
         init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
    FooChild = {foo,{foo, start_link, []}, permanent, 5000, worker, [foo]},
    {ok,{{one_for_all,1,1}, [FooChild]}}.
