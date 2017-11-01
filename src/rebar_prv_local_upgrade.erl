%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et

-module(rebar_prv_local_upgrade).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-include("rebar.hrl").
-include_lib("providers/include/providers.hrl").
-include_lib("kernel/include/file.hrl").

-define(PROVIDER, upgrade).
-define(NAMESPACE, local).
-define(DEPS, []).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    State1 =
        rebar_state:add_provider(State,
                                providers:create([{name, ?PROVIDER},
                                                  {module, ?MODULE},
                                                  {bare, true},
                                                  {namespace, ?NAMESPACE},
                                                  {deps, ?DEPS},
                                                  {example, "rebar3 unstable upgrade"},
                                                  {short_desc, "Download latest rebar3 escript and extract."},
                                                  {desc, ""},
                                                  {opts, []}])),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    case os:type() of
        {win32, _} ->
            ?ERROR("Sorry, this feature is not yet available on Windows.", []),
            {ok, State};
        _ ->
            Md5 = case rebar_state:escript_path(State) of
                      undefined ->
                          false;
                      ScriptPath ->
                          get_md5(ScriptPath)
                  end,

            case maybe_fetch_rebar3(Md5) of
                {saved, TmpRebar3} ->
                    rebar_prv_local_install:extract_escript(State, TmpRebar3);
                up_to_date ->
                    {ok, State};
                Error ->
                    Error
            end
    end.

-spec format_error(any()) -> iolist().
format_error(bad_checksum) ->
    "Not updating rebar3, the checksum of download did not match the one provided by s3.";
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% Internal

get_md5(Rebar3Path) ->
    {ok, Rebar3File} = file:read_file(Rebar3Path),
    Digest = crypto:hash(md5, Rebar3File),
    DigestHex = lists:flatten([io_lib:format("~2.16.0B", [X]) || X <- binary_to_list(Digest)]),
    rebar_string:lowercase(DigestHex).

maybe_fetch_rebar3(Rebar3Md5) ->
    TmpDir = ec_file:insecure_mkdtemp(),
    TmpFile = filename:join(TmpDir, "rebar3"),
    case rebar_pkg_resource:request("https://s3.amazonaws.com/rebar3/rebar3", Rebar3Md5) of
        {ok, Binary, ETag} ->
            file:write_file(TmpFile, Binary),
            case rebar_pkg_resource:etag(TmpFile) of
                ETag ->
                    {saved, TmpFile};
                _ ->
                    ?PRV_ERROR(bad_checksum)
            end;
        error ->
            ?ERROR("Unable to fetch latest rebar3 escript. Please try again later.", []);
        _ ->
            ?CONSOLE("No upgrade available", []),
            up_to_date
    end.
