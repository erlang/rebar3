-module(rebar_compiler_xrl).

-behaviour(rebar_compiler).

-export([context/1,
         needed_files/4,
         dependencies/3,
         compile/4,
         clean/2]).

-export([update_opts/2]).

context(AppInfo) ->
    Dir = rebar_app_info:dir(AppInfo),
    Mappings = [{".erl", filename:join([Dir, "src"])}],
    #{src_dirs => ["src"],
      include_dirs => [],
      src_ext => ".xrl",
      out_mappings => Mappings}.

needed_files(_, FoundFiles, Mappings, AppInfo) ->
    FirstFiles = [],

    %% Remove first files from found files
    RestFiles = [Source || Source <- FoundFiles,
                           not lists:member(Source, FirstFiles),
                           rebar_compiler:needs_compile(Source, ".erl", Mappings)],

    Opts = rebar_opts:get(rebar_app_info:opts(AppInfo), xrl_opts, []),
    Opts1 = update_opts(Opts, AppInfo),

    {{FirstFiles, Opts1}, {RestFiles, Opts1}}.

dependencies(_, _, _) ->
    [].

compile(Source, [{_, _}], _, Opts) ->
    case leex:file(Source, [{return, true} | Opts]) of
        {ok, _} ->
            ok;
        {ok, _Mod, Ws} ->
            rebar_compiler:ok_tuple(Source, Ws);
        {error, Es, Ws} ->
            rebar_compiler:error_tuple(Source, Es, Ws, Opts)
    end.

clean(XrlFiles, _AppInfo) ->
    rebar_file_utils:delete_each(
      [rebar_utils:to_list(re:replace(F, "\\.xrl$", ".erl", [unicode]))
       || F <- XrlFiles]).

%% make includefile options absolute paths
update_opts(Opts, AppInfo) ->
    OutDir = rebar_app_info:out_dir(AppInfo),
    lists:map(fun({includefile, I}) ->
                      case filename:pathtype(I) =:= relative of
                          true ->
                              {includefile, filename:join(OutDir, I)};
                          false ->
                              {includefile, I}
                      end;
                 (O) ->
                      O
              end, Opts).
