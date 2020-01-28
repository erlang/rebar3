%%% @doc multi-OTP version compatibility shim for working with URIs
-module(rebar_uri).

-export([
         parse/1,
         append_path/2
]).

-import(rebar_utils, [to_list/1]).

-ifdef (OTP_RELEASE).
-spec parse(URIString) -> URIMap when
    URIString :: uri_string:uri_string(),
    URIMap :: uri_string:uri_map() | uri_string:error().

parse(URIString) ->
    uri_string:parse(URIString).
-else.
-spec parse(URIString) -> URIMap when
    URIString :: iodata(),
    URIMap :: map() | {error, atom(), term()}.

parse(URIString) ->
    case http_uri:parse(URIString) of
        {error, Reason} ->
            %% no additional parser/term info available to us,
            %% e.g. see what uri_string returns in
            %% uri_string:parse(<<"h$ttp:::://////lolz">>).
            {error, Reason, ""};
        {ok, {Scheme, UserInfo, Host, Port, Path, Query}} ->
            #{
                scheme => rebar_utils:to_list(Scheme),
                host => Host,
                port => Port,
                path => Path,
                %% http_uri:parse/1 includes the leading question mark
                %% in query string but uri_string:parse/1 leaves it out.
                %% string:slice/2 isn't available in OTP <= 19.
                query => case Query of
                           [] -> "";
                           _  -> string:substr(Query, 2)
                         end,
                userinfo => UserInfo
            }
    end.
-endif.

%% OTP 21+
-ifdef (OTP_RELEASE).
append_path(Url, ExtraPath) ->
     case parse(Url) of
         #{path := Path} = Map ->
             FullPath = filename:join(Path, ExtraPath),
             {ok, uri_string:recompose(maps:update(path, FullPath, Map))};
         _ ->
             error
     end.
-else.
append_path(Url, ExtraPath) ->
     case parse(Url) of
         #{scheme := Scheme, userinfo := UserInfo, host := Host, port := Port, path := Path, query := Query} ->
             PrefixedQuery = case Query of
                               []    -> [];
                               Other -> lists:append(["?", Other])
                             end,
             {ok, lists:append([to_list(Scheme), "://", UserInfo, Host, ":", to_list(Port),
                                filename:join(Path, ExtraPath), PrefixedQuery])};
         _ ->
             error
     end.
-endif.
