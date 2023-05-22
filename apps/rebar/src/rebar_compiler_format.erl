%%% @doc Module handling rich formatting of errors.
-module(rebar_compiler_format).
-export([format/5]).

-include("rebar.hrl").

-spec format(file:filename_all(), {Line, Column}, Extra, Desc, rebar_dict()) ->
    string() when
        Extra :: iodata(),
        Line :: non_neg_integer(),
        Column :: non_neg_integer(),
        Desc :: iodata().
format(Source, {Line, Column}, Extra, Desc, Config) ->
    CompilerErrFmt = compiler_error_format(Config),
    case CompilerErrFmt == rich andalso find_line(Line, Source) of
        {ok, LnBin} ->
            ?FMT("~ts: ~w:~w:~n"
                 "  ~ts~n"
                 "  ~s^-- ~ts~ts~n",
                 [Source, Line, Column,
                  LnBin,
                  lists:duplicate(max(0, Column-1), " "), Extra, Desc]);
        _ ->
            ?FMT("~ts:~w:~w: ~ts~ts~n", [Source, Line, Column, Extra, Desc])
    end.

find_line(Nth, Source) ->
  try
      {ok, Bin} = file:read_file(Source),
      Splits = re:split(Bin, "(?:\n|\r\n|\r)", [{newline, anycrlf}]),
      {ok, lists:nth(Nth, Splits)}
  catch
      error:X -> {error, X}
  end.

compiler_error_format(Opts) ->
    %% `Opts' can be passed in both as a list or a dictionary depending
    %% on whether the first call to rebar_erlc_compiler was done with
    %% the type `rebar_dict()' or `rebar_state:t()'.
    LookupFn = if is_list(Opts) -> fun(K,L) -> lists:keyfind(K, 1, L) end
                ; true          -> fun(K,O) -> rebar_opts:get(O, K, false) end
               end,
    case LookupFn(compiler_error_format, Opts) of
        false -> ?DEFAULT_COMPILER_ERROR_FORMAT;
        {ok, minimal} -> minimal;
        {ok, rich} -> rich;
        minimal -> minimal;
        rich -> rich
    end.
