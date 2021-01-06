%%% @doc multi-OTP version compatibility shim for working with URIs
-module(rebar_uri).

-export([
         parse/1, parse/2, scheme_defaults/0,
         append_path/2, percent_decode/1
]).

-type error() :: {error, atom(), term()}.

-define(DEC2HEX(X),
        if ((X) >= 0) andalso ((X) =< 9) -> (X) + $0;
           ((X) >= 10) andalso ((X) =< 15) -> (X) + $A - 10
        end).

-define(HEX2DEC(X),
        if ((X) >= $0) andalso ((X) =< $9) -> (X) - $0;
           ((X) >= $A) andalso ((X) =< $F) -> (X) - $A + 10;
           ((X) >= $a) andalso ((X) =< $f) -> (X) - $a + 10
        end).

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

%% Taken from OTP 23.2
-spec percent_decode(URI) -> Result when
      URI :: uri_string:uri_string() | uri_string:uri_map(),
      Result :: uri_string:uri_string() |
                uri_string:uri_map() |
                {error, {invalid, {atom(), {term(), term()}}}}.
percent_decode(URIMap) when is_map(URIMap)->
    Fun = fun (K,V) when K =:= userinfo; K =:= host; K =:= path;
                         K =:= query; K =:= fragment ->
                  case raw_decode(V) of
                      {error, Reason, Input} ->
                          throw({error, {invalid, {K, {Reason, Input}}}});
                      Else ->
                          Else
                  end;
              %% Handle port and scheme
              (_,V) ->
                  V
          end,
    try maps:map(Fun, URIMap)
    catch throw:Return ->
            Return
    end;
percent_decode(URI) when is_list(URI) orelse
                         is_binary(URI) ->
    raw_decode(URI).

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

-spec raw_decode(list()|binary()) -> list() | binary() | error().
raw_decode(Cs) ->
    raw_decode(Cs, <<>>).

raw_decode(L, Acc) when is_list(L) ->
    try
        B0 = unicode:characters_to_binary(L),
        B1 = raw_decode(B0, Acc),
        unicode:characters_to_list(B1)
    catch
        throw:{error, Atom, RestData} ->
            {error, Atom, RestData}
    end;
raw_decode(<<$%,C0,C1,Cs/binary>>, Acc) ->
    case is_hex_digit(C0) andalso is_hex_digit(C1) of
        true ->
            B = ?HEX2DEC(C0)*16+?HEX2DEC(C1),
            raw_decode(Cs, <<Acc/binary, B>>);
        false ->
            throw({error,invalid_percent_encoding,<<$%,C0,C1>>})
    end;
raw_decode(<<C,Cs/binary>>, Acc) ->
    raw_decode(Cs, <<Acc/binary, C>>);
raw_decode(<<>>, Acc) ->
    check_utf8(Acc).

-spec is_hex_digit(char()) -> boolean().
is_hex_digit(C)
  when $0 =< C, C =< $9;$a =< C, C =< $f;$A =< C, C =< $F -> true;
is_hex_digit(_) -> false.

%% Returns Cs if it is utf8 encoded.
check_utf8(Cs) ->
    case unicode:characters_to_list(Cs) of
        {incomplete,_,_} ->
            throw({error,invalid_utf8,Cs});
        {error,_,_} ->
            throw({error,invalid_utf8,Cs});
        _ -> Cs
    end.
