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
                    {Vsn, Archive} =
                        try
                            {ok, Escript} = escript:extract(TmpRebar3, []),
                            {comment, "Rebar3 " ++ Rebar3Vsn} = lists:keyfind(comment, 1, Escript),
                            {archive, FullArchive} = lists:keyfind(archive, 1, Escript),
                            {Rebar3Vsn, FullArchive}
                        catch
                            C:T:S ->
                                ?DIAGNOSTIC("local upgrade version extraction exception: ~p:~p:~p", [C, T, S]),
                                error(?PRV_ERROR(failed_vsn_lookup))
                        end,
                    rebar_prv_local_install:install_escript(State, Vsn, Archive);
                up_to_date ->
                    {ok, State};
                Error ->
                    Error
            end
    end.

-spec format_error(any()) -> iolist().
format_error(failed_vsn_lookup) ->
    "Failed to extract the version from the downloaded rebar3 escript.\n     Try downloading https://s3.amazonaws.com/rebar3/rebar3 manually and running `chmod +x rebar3 && ./rebar3 local install`";
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
    case request("https://s3.amazonaws.com/rebar3/rebar3", Rebar3Md5) of
        {ok, Binary, ETag} ->
            file:write_file(TmpFile, Binary),
            case etag(TmpFile) of
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

etag(Path) ->
     case file:read_file(Path) of
         {ok, Binary} ->
             <<X:128/big-unsigned-integer>> = crypto:hash(md5, Binary),
             rebar_string:lowercase(lists:flatten(io_lib:format("~32.16.0b", [X])));
         {error, _} ->
             false
     end.

-spec request(Url, ETag) -> Res when
      Url :: string(),
      ETag :: false | string(),
      Res :: 'error' | {ok, cached} | {ok, any(), string()}.
request(Url, ETag) ->
    case os:getenv("REBAR_OFFLINE") of
        "1" ->
            ?DEBUG("Rebar is in offline mode", []),
            error;
        _ ->
            request_online(Url, ETag)
    end.

-spec request_online(Url, ETag) -> Res when
      Url :: string(),
      ETag :: false | string(),
      Res :: 'error' | {ok, cached} | {ok, any(), string()}.
request_online(Url, ETag) ->
    HttpOptions = [{ssl, rebar_utils:ssl_opts(Url)},
                   {relaxed, true} | rebar_utils:get_proxy_auth()],
    case httpc:request(get, {Url, [{"if-none-match", "\"" ++ ETag ++ "\""}
                                   || ETag =/= false] ++
                                 [{"User-Agent", rebar_utils:user_agent()}]},
                       HttpOptions, [{body_format, binary}], rebar) of
        {ok, {{_Version, 200, _Reason}, Headers, Body}} ->
            ?DEBUG("Successfully downloaded ~ts", [Url]),
            {"etag", ETag1} = lists:keyfind("etag", 1, Headers),
            {ok, Body, rebar_string:trim(ETag1, both, [$"])};
        {ok, {{_Version, 304, _Reason}, _Headers, _Body}} ->
            ?DEBUG("Cached copy of ~ts still valid", [Url]),
            {ok, cached};
        {ok, {{_Version, Code, _Reason}, _Headers, _Body}} ->
            ?DEBUG("Request to ~p failed: status code ~p", [Url, Code]),
            error;
        {error, Reason} ->
            ?DEBUG("Request to ~p failed: ~p", [Url, Reason]),
            error
    end.
