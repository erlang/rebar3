-module(r3_certifi).
-compile({parse_transform, ct_expand}).

-export([cacertfile/0,
         cacerts/0]).


%% @doc CACertFile gives the path to the file with an X.509 certificate list
%% containing the Mozilla CA Certificate that can then be used via the
%% cacertfile setting in ssl options passed to the connect function.
cacertfile() ->
  PrivDir = case code:priv_dir(rebar) of
              {error, _} ->
                %% try to get relative priv dir. useful for tests.
                AppDir = filename:dirname(
                           filename:dirname(code:which(?MODULE))
                          ),
                filename:join(AppDir, "priv");
              Dir -> Dir
            end,
  filename:join(PrivDir, "cacerts.pem").

%% @doc CACerts builds an X.509 certificate list containing the Mozilla CA
%% Certificate that can then be used via the cacerts setting in ssl options
%% passed to the connect function.
-spec cacerts() -> [binary(),...].
cacerts() ->
    ct_expand:term(
      lists:reverse(
        [Der || {ok,Bin} <- [file:read_file(cacertfile())],
                {'Certificate',Der,_} <- public_key:pem_decode(Bin)
        ]
       )
     ).
