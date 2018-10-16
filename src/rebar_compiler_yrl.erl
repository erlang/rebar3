-module(rebar_compiler_yrl).

-behaviour(rebar_compiler).

-export([context/1,
         needed_files/3,
         dependencies/3,
         compile/4,
         clean/2]).

context(AppInfo) ->
    Dir = rebar_app_info:dir(AppInfo),
    Mappings = [{".erl", filename:join([Dir, "src"])}],
    #{src_dirs => ["src"],
      include_dirs => [],
      src_ext => ".yrl",
      out_mappings => Mappings}.

needed_files(_, FoundFiles, AppInfo) ->
    FirstFiles = [],

    %% Remove first files from found files
    RestFiles = [Source || Source <- FoundFiles, not lists:member(Source, FirstFiles)],

    Opts = rebar_opts:get(rebar_app_info:opts(AppInfo), yrl_opts, []),
    {{FirstFiles, Opts}, {RestFiles, Opts}}.

dependencies(_, _, _) ->
    [].

compile(Source, [{_, OutDir}], _, Opts) ->
    BaseName = filename:basename(Source),
    Target = filename:join([OutDir, BaseName]),
    AllOpts = [{parserfile, Target} | Opts],
    AllOpts1 = [{includefile, filename:join(OutDir, I)} || {includefile, I} <- AllOpts,
                                                           filename:pathtype(I) =:= relative],
    case yecc:file(Source, AllOpts1 ++ [{return, true}]) of
        {ok, _} ->
            ok;
        {ok, _Mod, Ws} ->
            rebar_compiler:ok_tuple(Source, Ws);
        {error, Es, Ws} ->
            rebar_compiler:error_tuple(Source, Es, Ws, AllOpts1)
    end.

clean(YrlFiles, _AppInfo) ->
    rebar_file_utils:delete_each(
      [rebar_utils:to_list(re:replace(F, "\\.yrl$", ".erl", [unicode]))
       || F <- YrlFiles]).
