%%% @doc
%%% Analyze erlang-related files and compilation data using EPP, in order to
%%% build complete and accurate DAGs
%%% @end
-module(rebar_compiler_epp).
-export([deps/2, resolve_module/2]).
%% cache (a la code path storage, but for dependencies not in code path)
-export([ensure_started/0, flush/0, resolve_source/2]).
-export([init/1, handle_call/3, handle_cast/2]).
%% remove when OTP 19 support is no longer needed
-export([handle_info/2, terminate/2, code_change/3]).

-behaviour(gen_server).

-include_lib("kernel/include/file.hrl").

-include("rebar.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Basic File Handling %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Find all Erlang code dependencies for a given file
-spec deps(file:filename_all(), Opts) -> Attributes when
      Opts :: [Opt, ...],
      Opt :: {includes, [file:filename_all()]}
           | {macros, [file:filename_all()]},
                      %% following are all required, OTP-18 don't like it though
      Attributes :: #{include => [file:filename_all()],
                      missing_include_file => [file:filename_all()],
                      missing_include_lib => [file:filename_all()],
                      behaviour => [atom()],
                      parse_transform => [atom()],
                      is_behaviour => boolean()}.
deps(File, Opts) ->
    {EppOpts, ExtraOpts} = split_opts(Opts),
    {ok, Forms} = epp:parse_file(File, EppOpts),
    normalize(handle_forms(Forms, default_attrs(), ExtraOpts)).

%% Find the path matching a given erlang module
resolve_module(Mod, Paths) ->
    ModStr = atom_to_list(Mod),
    try
        [throw(P) || P <- Paths, ModStr =:= filename:basename(P, ".erl")],
        {error, not_found}
    catch
        Path -> {ok, Path}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Cache for deps      %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec ensure_started() -> ok.
ensure_started() ->
    case whereis(?MODULE) of
        undefined ->
            case gen_server:start({local, ?MODULE}, ?MODULE, [], []) of
                {ok, _Pid} ->
                    ok;
                {error, {already_started, _Pid}} ->
                    ok
            end;
        Pid when is_pid(Pid) ->
            ok
    end.

flush() ->
    gen_server:cast(?MODULE, flush).

%% @doc Resolves "Name" erl module to a path, given list of paths to search.
%% Caches result for subsequent requests.
-spec resolve_source(atom() | file:filename_all(), [file:filename_all()]) -> {true, file:filename_all()} | false.
resolve_source(Name, Dirs) when is_atom(Name) ->
    gen_server:call(?MODULE, {resolve, atom_to_list(Name) ++ ".erl", Dirs}, infinity);
resolve_source(Name, Dirs) when is_list(Name) ->
    gen_server:call(?MODULE, {resolve, Name, Dirs}, infinity).

-record(state, {
    %% filesystem cache, denormalised
    fs = #{} :: #{file:filename_all() => [file:filename_all()]},
    %% map of module name => abs path
    resolved = #{} :: #{file:filename_all() => file:filename_all()}
}).

init([]) ->
    {ok, #state{}}.

handle_call({resolve, Name, Dirs}, _From, #state{fs = Fs, resolved = Res} = State) ->
    case maps:find(Name, Res) of
        {ok, Found} ->
            {reply, Found, State};
        error ->
            {Resolved, NewFs} = resolve(Name, Fs, Dirs),
            {reply, Resolved, State#state{resolved = Res#{Name => Resolved}, fs = NewFs}}
    end.

handle_cast(flush, _State) ->
    {noreply, #state{}}.

resolve(_Name, Fs, []) ->
    {false, Fs};
resolve(Name, Fs, [Dir | Tail]) ->
    {NewFs, Files} = list_directory(Dir, Fs),
    case lists:keyfind(Name, 2, Files) of
        {FullDir, _} ->
            {{true, filename:join(FullDir, Name)}, NewFs};
        false ->
            resolve(Name, NewFs, Tail)
    end.

%% list_directory/2 caches files in the directory and all subdirectories,
%%  to support the behaviour of looking for source files in
%%  subdirectories of src/* folder.
%% This may introduce weird dependencies for cases when CT
%%  test cases contain test data with files named the same
%%  as requested behaviour/parse_transforms, but let's hope
%%  it won't happen for many projects. If it does, in fact,
%%  it won't cause any damage, just extra unexpected recompiles.
list_directory(Dir, Cache) ->
    case maps:find(Dir, Cache) of
        {ok, Files} ->
            {Cache, Files};
        error ->
            case file:list_dir(Dir) of
                {ok, DirFiles} ->
                    %% create a full list of *.erl files under Dir.
                    {NewFs, Files} = lists:foldl(
                        fun (File, {DirCache, Files} = Acc) ->
                            FullName = filename:join(Dir, File),
                            case filelib:is_dir(FullName) of
                                true ->
                                    %% We assume the include paths carry all recursive directories
                                    %% so we don't need this resolution to be recursive.
                                    Acc;
                                false ->
                                    %% ignore all but *.erl files
                                    case filename:extension(File) =:= ".erl" of
                                        true ->
                                            {DirCache, [{Dir, File} | Files]};
                                        false ->
                                            Acc
                                    end
                            end
                        end,
                        {Cache, []}, DirFiles),
                    {NewFs#{Dir => Files}, Files};
                {error, Reason} ->
                    ?DIAGNOSTIC("Failed to list ~s, ~p", [Dir, Reason]),
                    {Cache, []}
            end
    end.

%%%%%%%%%%%%%%%
%%% OTP 19  %%%
handle_info(_Request, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%%%%%%%%%%%%%
%%% PRIVATE %%%
%%%%%%%%%%%%%%%

default_attrs() ->
    #{include => [],
      missing_include_file => [],
      missing_include_lib => [],
      behaviour => [],
      parse_transform => [],
      is_behaviour => false}.

normalize(Map) ->
    #{include := Incl,
      missing_include_file := InclF,
      missing_include_lib := InclL,
      behaviour := Behaviour,
      parse_transform := PTrans} = Map,
    Map#{include => lists:usort(Incl),
         missing_include_file => lists:usort(InclF),
         missing_include_lib => lists:usort(InclL),
         behaviour => lists:usort(Behaviour),
         parse_transform => lists:usort(PTrans)}.

handle_forms([File|Forms], Map, Opts) ->
    lists:foldl(fun(Form, M) -> handle_form(Form, M, Opts) end,
                Map, drop_self_file(File, Forms)).

drop_self_file(_, []) ->
    [];
drop_self_file({attribute, _, file, {Path,_}} = File,
               [{attribute,_, file, {Path,_}} | Rest]) ->
    drop_self_file(File, Rest);
drop_self_file(File, [Keep|Rest]) ->
    [Keep | drop_self_file(File, Rest)].

%% Included files (both libs and direct includes);
%% There are also references to the module's own file declaration
%% in there, but this is dropped by `drop_self_file/2' and assumed
%% to be gone here.
handle_form({attribute, _Line, file, {Path, Ln}}, Map, Opts) ->
    %% Some people think they're funny and they go include attributes
    %% like:
    %%  -file("fake/file.hrl", Ln).
    %% Which are expanded to the very clause we have here, which in
    %% turn is impossible to distinguish from actual included files
    %% once checked through epp. The way we work around that here
    %% is to check if the path is absolute, and if so, keep it in since
    %% epp has expanded it; otherwise consider it to be a failed include.
    %% This is not perfect but we can't do much more without touching the
    %% disk and hopefully nobody else in the community has relied on this
    %% thing.
    case filename:absname(Path) of
        Path ->
            maps:update_with(include, fun(L) -> [Path|L] end, [Path], Map);
        _ -> % argh!
            handle_form({error, {Ln, {epp, {include, file, Path}}}}, Map, Opts)
    end;
%% Include files that EPP couldn't resolve
handle_form({error, {_Line, epp, {include, file, Name}}}, Map, _Opts) ->
    maps:update_with(missing_include_file, fun(L) -> [Name|L] end, [Name], Map);
handle_form({error, {_Line, epp, {include, lib, Path}}}, Map, Opts) ->
    %% This file might still exist in the regular paths not in
    %% code:lib_dir, which depend on options we pass to this module.
    %% recursively seek it, and add it to the paths to expand here.
    case find_include_with_opts(Path, Opts) of
        {ok, File} ->
            %% we can't go and figure out the contents within that include
            %% file because we'd need its own compiler opts and app opts
            %% to do it safely. Tracking that file is still better
            %% than nothing though.
            maps:update_with(include, fun(L) -> [File|L] end, [File], Map);
        {error, not_found} ->
            maps:update_with(missing_include_lib, fun(L) -> [Path|L] end, [Path], Map)
    end;
%% Behaviour implementation declaration
handle_form({attribute, _Line, behaviour, Name}, Map, _Opts) ->
    maps:update_with(behaviour, fun(L) -> [Name|L] end, [Name], Map);
handle_form({attribute, _Line, behavior, Name}, Map, _Opts) ->
    maps:update_with(behaviour, fun(L) -> [Name|L] end, [Name], Map);
%% Extract parse transforms
handle_form({attribute, Line, compile, Attr}, Map, _Opts) when not is_list(Attr) ->
    handle_form({attribute, Line, compile, [Attr]}, Map, _Opts);
handle_form({attribute, _Line, compile, Attrs}, Map, _Opts) ->
    Mods = [case T of
                {_, {M,_}} -> M;
                {_, M} -> M
            end || T <- proplists:lookup_all(parse_transform, Attrs)],
    maps:update_with(parse_transform, fun(L) -> Mods++L end, Mods, Map);
%% Current style behaviour specification declaration
handle_form({attribute, _Line, callback, _}, Map, _Opts) ->
    Map#{is_behaviour => true};
%% Old style behaviour specification, both spellings supported
%% The function needs to be exported, but we skip over that logic
%% for now.
handle_form({function, _Line, behaviour_info, 1, _}, Map, _Opts) ->
    Map#{is_behaviour => true};
handle_form({function, _Line, behavior_info, 1, _}, Map, _Opts) ->
    Map#{is_behaviour => true};
%% Skip the rest
handle_form(_, Map, _Opts) ->
    Map.

split_opts(Opts) ->
    %% Extra Opts are options we added to palliate to issues we had
    %% with resolving include_libs and other things in EPP.
    lists:partition(
        fun({OptName, _}) ->
            not lists:member(OptName, [include_libs, parse_transforms])
        end,
        Opts
    ).

find_include_with_opts(Path, Opts) ->
    InclPaths = proplists:get_value(include_libs, Opts, []),
    find_include_lib(InclPaths, Path).

find_include_lib([], _) ->
    {error, not_found};
find_include_lib([H|T], File) ->
    Abs = filename:join([H, File]),
    case filelib:is_regular(Abs) of
        true -> {ok, Abs};
        false -> find_include_lib(T, File)
    end.
