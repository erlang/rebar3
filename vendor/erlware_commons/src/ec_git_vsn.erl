%%% vi:ts=4 sw=4 et
%%%-------------------------------------------------------------------
%%% @author Eric Merritt <ericbmerritt@gmail.com>
%%% @copyright 2011 Erlware, LLC.
%%% @doc
%%%  This provides an implementation of the ec_vsn for git. That is
%%%  it is capable of returning a semver for a git repository
%%% see ec_vsn
%%% see ec_semver
%%% @end
%%%-------------------------------------------------------------------
-module(ec_git_vsn).

-behaviour(ec_vsn).

%% API
-export([new/0,
         vsn/1]).

-ifdef(TEST).
-export([parse_tags/1,
	 get_patch_count/1,
	 collect_default_refcount/1
	]).
-endif.

-export_type([t/0]).

%%%===================================================================
%%% Types
%%%===================================================================
%% This should be opaque, but that kills dialyzer so for now we export it
%% however you should not rely on the internal representation here
-type t() :: {}.

%%%===================================================================
%%% API
%%%===================================================================

-spec new() -> t().
new() ->
    {}.

-spec vsn(t()|string()) -> {ok, string()} | {error, Reason::any()}.
vsn(Data) ->
    {Vsn, RawRef, RawCount} = collect_default_refcount(Data),
    {ok, build_vsn_string(Vsn, RawRef, RawCount)}.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

collect_default_refcount(Data) ->
    %% Get the tag timestamp and minimal ref from the system. The
    %% timestamp is really important from an ordering perspective.
    RawRef = os:cmd("git log -n 1 --pretty=format:'%h\n' "),

    {Tag, TagVsn} = parse_tags(Data),
    RawCount =
        case Tag of
            undefined ->
                os:cmd("git rev-list --count HEAD");
            _ ->
                get_patch_count(Tag)
        end,
    {TagVsn, RawRef, RawCount}.

build_vsn_string(Vsn, RawRef, RawCount) ->
    %% Cleanup the tag and the Ref information. Basically leading 'v's and
    %% whitespace needs to go away.
    RefTag = [".ref", re:replace(RawRef, "\\s", "", [global])],
    Count = erlang:iolist_to_binary(re:replace(RawCount, "\\s", "", [global])),

    %% Create the valid [semver](http://semver.org) version from the tag
    case Count of
        <<"0">> ->
            erlang:binary_to_list(erlang:iolist_to_binary(Vsn));
        _ ->
            erlang:binary_to_list(erlang:iolist_to_binary([Vsn, "+build.",
                                                           Count, RefTag]))
    end.

get_patch_count(RawRef) ->
    Ref = re:replace(RawRef, "\\s", "", [global]),
    Cmd = io_lib:format("git rev-list --count ~ts..HEAD",
                         [Ref]),
    case os:cmd(Cmd) of
        "fatal: " ++ _ ->
            0;
        Count ->
            Count
    end.

-spec parse_tags(t()|string()) -> {string()|undefined, ec_semver:version_string()}.
parse_tags({}) ->
    parse_tags("");
parse_tags(Pattern) ->
    Cmd = io_lib:format("git describe --abbrev=0 --tags --match \"~ts*\"", [Pattern]),
    Tag = os:cmd(Cmd),
    case Tag of
        "fatal: " ++ _ ->
            {undefined, ""};
        _ ->
            Vsn  = string:slice(Tag, string:length(Pattern)),
            Vsn1 = string:trim(string:trim(Vsn, leading, "v"), trailing, "\n"),
            {Tag, Vsn1}
    end.
