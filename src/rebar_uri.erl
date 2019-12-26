%%% @doc multi-OTP version compatibility shim for working with URIs
-module(rebar_uri).

-ifdef (OTP_RELEASE).
-export([parse/1]).

-spec parse(URIString) -> URIMap when
    URIString :: uri_string:uri_string(),
    URIMap :: uri_string:uri_map() | uri_string:error().

parse(URIString) ->
    uri_string:parse(URIString).
-else.
-export([parse/1]).

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
                scheme => Scheme,
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

