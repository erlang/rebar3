-module(certifi_pt).
-export([parse_transform/2]).

parse_transform(Forms, _Opts) ->
  [replace_cacerts(Form) || Form <- Forms].

replace_cacerts({function, Ann, cacerts, 0, [_]}) ->
  {ok, Binary} = file:read_file(cert_file() ),
  Pems = public_key:pem_decode(Binary),
  Cacerts = [Der || {'Certificate', Der, _} <- Pems],
  Body = lists:foldl(fun(Cert, Acc) ->
    {cons, 0, cert_to_bin_ast(Cert), Acc}
  end, {nil, 0}, Cacerts),
  {function, Ann, cacerts, 0, [{clause, Ann, [], [], [Body]}]};
replace_cacerts(Other) ->
  Other.

-spec cert_file() -> Result when
	Result :: file:filename_all().
cert_file() ->
  AppDir = filename:dirname(
             filename:dirname(code:which(?MODULE))
            ),
  filename:join([AppDir, "priv", "cacerts.pem"]).

-spec cert_to_bin_ast(Cert) -> Result when
	Cert :: binary(),
	Result :: {bin, 0, [{bin_element, 0, {string, 0, list()}, default, default}]}.
cert_to_bin_ast(Cert) ->
  {bin, 0, [{bin_element, 0, {string, 0, binary_to_list(Cert)}, default, default}]}.
