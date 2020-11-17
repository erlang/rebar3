%%% @doc multi-OTP version compatibility shim for working with URIs
-module(rebar_uri).

-export([
         parse/1, parse/2, scheme_defaults/0,
         append_path/2
]).

-ifdef(OTP_RELEASE).
-spec parse(URIString) -> URIMap when
    URIString :: uri_string:uri_string(),
    URIMap :: uri_string:uri_map() | uri_string:error().

parse(URIString) ->
    parse(URIString, []).

parse(URIString, URIOpts) ->
    case uri_string:parse(URIString) of
        #{path := ""} = Map -> apply_opts(Map#{path => "/"}, URIOpts);
        Map when is_map(Map) -> apply_opts(Map, URIOpts);
        {error, _, _} = E -> E
    end.
-else.
-spec parse(URIString) -> URIMap when
    URIString :: iodata(),
    URIMap :: map() | {error, term(), term()}.

parse(URIString) ->
    parse(URIString, []).

parse(URIString, URIOpts) ->
    case http_uri:parse(URIString, URIOpts) of
        {error, Reason} ->
            %% no additional parser/term info available to us,
            %% e.g. see what uri_string returns in
            %% uri_string:parse(<<"h$ttp:::://////lolz">>).
            {error, "", Reason};
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
-ifdef(OTP_RELEASE).
append_path(Url, ExtraPath) ->
     case parse(Url) of
         #{path := Path} = Map ->
             FullPath = join(Path, ExtraPath),
             {ok, uri_string:recompose(maps:update(path, FullPath, Map))};
         _ ->
             error
     end.
-else.
append_path(Url, ExtraPath) ->
     case parse(Url) of
         #{scheme := Scheme, userinfo := UserInfo, host := Host,
           port := Port, path := Path, query := Query} ->
             ListScheme = rebar_utils:to_list(Scheme),
             PrefixedQuery = case Query of
                               []    -> [];
                               Other -> lists:append(["?", Other])
                             end,
             NormPath = case Path of
                            "" -> "/";
                            _ -> Path
                        end,
             {ok, maybe_port(
                Url, lists:append([ListScheme, "://", UserInfo, Host]),
                [$: | rebar_utils:to_list(Port)],
                lists:append([join(NormPath, ExtraPath), PrefixedQuery])
             )};
         _ ->
             error
     end.
-endif.

%% OTP 21+
-ifdef(OTP_RELEASE).
scheme_defaults() ->
    %% no scheme defaults here; just custom ones
    [].
-else.
scheme_defaults() ->
    http_uri:scheme_defaults().
-endif.

join(URI, "") -> URI;
join(URI, "/") -> URI;
join("/", [$/|_] = Path) -> Path;
join("/", Path) -> [$/ | Path];
join("", [$/|_] = Path) -> Path;
join("", Path) -> [$/ | Path];
join([H|T], Path) -> [H | join(T, Path)].


-ifdef(OTP_RELEASE).
apply_opts(Map = #{port := _}, _) ->
    Map;
apply_opts(Map = #{scheme := Scheme}, URIOpts) ->
    SchemeDefaults = proplists:get_value(scheme_defaults, URIOpts, []),
    %% Here is the funky bit: don't add the port number if it's in a default
    %% to maintain proper default behaviour.
    try lists:keyfind(list_to_existing_atom(Scheme), 1, SchemeDefaults) of
        {_, Port} ->
            Map#{port => Port};
        false ->
            Map
    catch
        error:badarg -> % not an existing atom, not in the list
            Map
    end.
-else.
maybe_port(Url, Host, Port, PathQ) ->
    case lists:prefix(Host ++ Port, Url) of
        true -> Host ++ Port ++ PathQ; % port was explicit
        false -> Host ++ PathQ % port was implicit
    end.
-endif.
