-module(rebar_prv_unlock).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-include("rebar.hrl").
-include_lib("providers/include/providers.hrl").

-define(PROVIDER, unlock).
-define(DEPS, []).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    State1 = rebar_state:add_provider(
        State,
        providers:create([{name, ?PROVIDER},
                          {module, ?MODULE},
                          {bare, true},
                          {deps, ?DEPS},
                          {example, ""},
                          {short_desc, "Unlock dependencies."},
                          {desc, "Unlock project dependencies. Mentioning no application "
                                 "will unlock all dependencies. To unlock specific dependencies, "
                                 "their name can be listed in the command."},
                          {opts, [
                            {package, undefined, undefined, string,
                             "List of packages to unlock. If not specified, all dependencies are unlocked."}
                          ]}
                         ])
    ),
    {ok, State1}.

do(State) ->
    Dir = rebar_state:dir(State),
    LockFile = filename:join(Dir, ?LOCK_FILE),
    case file:consult(LockFile) of
        {error, enoent} ->
            %% Our work is done.
            {ok, State};
        {error, Reason} ->
            ?PRV_ERROR({file,Reason});
        {ok, _} ->
            Locks = rebar_config:consult_lock_file(LockFile),
            case handle_unlocks(State, Locks, LockFile) of
                {ok, NewLocks} ->
                    {ok, rebar_state:set(State, {locks, default}, NewLocks)};
                {error, Reason} ->
                    ?PRV_ERROR({file,Reason})
            end
    end.

-spec format_error(any()) -> iolist().
format_error({file, Reason}) ->
    io_lib:format("Lock file editing failed for reason ~p", [Reason]);
format_error(unknown_lock_format) ->
    "Lock file format unknown";
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

handle_unlocks(State, Locks, LockFile) ->
    {Args, _} = rebar_state:command_parsed_args(State),
    Names = parse_names(rebar_utils:to_binary(proplists:get_value(package, Args, <<"">>))),
    case [Lock || Lock = {Name, _, _} <- Locks, not lists:member(Name, Names)] of
        [] ->
            file:delete(LockFile),
            {ok, []};
        _ when Names =:= [] -> % implicitly all locks
            file:delete(LockFile),
            {ok, []};
        NewLocks ->
            rebar_config:write_lock_file(LockFile, NewLocks),
            {ok, NewLocks}
    end.

parse_names(Bin) ->
    case lists:usort(re:split(Bin, <<" *, *">>, [trim, unicode])) of
        [<<"">>] -> []; % nothing submitted
        Other -> Other
    end.
