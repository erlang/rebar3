%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2009, 2010 Dave Smith (dizzyd@dizzyd.com)
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%% -------------------------------------------------------------------
-module(rebar_utils).

-export([sort_deps/1,
         droplast/1,
         filtermap/2,
         is_arch/1,
         sh/2,
         sh_send/3,
         abort/0,
         abort/2,
         escript_foldl/3,
         find_files/2,
         find_files/3,
         beam_to_mod/1,
         beam_to_mod/2,
         erl_to_mod/1,
         beams/1,
         find_executable/1,
         vcs_vsn/3,
         deprecated/3,
         deprecated/4,
         erl_opts/1,
         indent/1,
         update_code/1,
         remove_from_code_path/1,
         cleanup_code_path/1,
         args_to_tasks/1,
         expand_env_variable/3,
         get_arch/0,
         wordsize/0,
         tup_umerge/2,
         tup_sort/1,
         line_count/1]).

%% for internal use only
-export([otp_release/0]).

-include("rebar.hrl").

-define(ONE_LEVEL_INDENT, "     ").
-define(APP_NAME_INDEX, 2).

%% ====================================================================
%% Public API
%% ====================================================================

sort_deps(Deps) ->
    %% We need a sort stable, based on the name. So that for multiple deps on
    %% the same level with the same name, we keep the order the parents had.
    %% `lists:keysort/2' is documented as stable in the stdlib.
    %% The list of deps is revered when we get it. For the proper stable
    %% result, re-reverse it.
    lists:keysort(?APP_NAME_INDEX, lists:reverse(Deps)).

droplast(L) ->
    lists:reverse(tl(lists:reverse(L))).

filtermap(F, [Hd|Tail]) ->
    case F(Hd) of
        true ->
            [Hd|filtermap(F, Tail)];
        {true,Val} ->
            [Val|filtermap(F, Tail)];
        false ->
            filtermap(F, Tail)
    end;
filtermap(F, []) when is_function(F, 1) -> [].

is_arch(ArchRegex) ->
    case re:run(get_arch(), ArchRegex, [{capture, none}]) of
        match ->
            true;
        nomatch ->
            false
    end.

get_arch() ->
    Words = wordsize(),
    otp_release() ++ "-"
        ++ erlang:system_info(system_architecture) ++ "-" ++ Words.

wordsize() ->
    try erlang:system_info({wordsize, external}) of
        Val ->
            integer_to_list(8 * Val)
    catch
        error:badarg ->
            integer_to_list(8 * erlang:system_info(wordsize))
    end.

sh_send(Command0, String, Options0) ->
    ?INFO("sh_send info:\n\tcwd: ~p\n\tcmd: ~s < ~s\n",
          [rebar_dir:get_cwd(), Command0, String]),
    ?DEBUG("\topts: ~p\n", [Options0]),

    DefaultOptions = [use_stdout, abort_on_error],
    Options = [expand_sh_flag(V)
               || V <- proplists:compact(Options0 ++ DefaultOptions)],

    Command = lists:flatten(patch_on_windows(Command0, proplists:get_value(env, Options, []))),
    PortSettings = proplists:get_all_values(port_settings, Options) ++
        [exit_status, {line, 16384}, use_stdio, stderr_to_stdout, hide],
    Port = open_port({spawn, Command}, PortSettings),

    %% allow us to send some data to the shell command's STDIN
    %% Erlang doesn't let us get any reply after sending an EOF, though...
    Port ! {self(), {command, String}},
    port_close(Port).

%%
%% Options = [Option] -- defaults to [use_stdout, abort_on_error]
%% Option = ErrorOption | OutputOption | {cd, string()} | {env, Env}
%% ErrorOption = return_on_error | abort_on_error | {abort_on_error, string()}
%% OutputOption = use_stdout | {use_stdout, bool()}
%% Env = [{string(), Val}]
%% Val = string() | false
%%
sh(Command0, Options0) ->
    ?DEBUG("sh info:\n\tcwd: ~p\n\tcmd: ~s\n", [rebar_dir:get_cwd(), Command0]),
    ?DEBUG("\topts: ~p\n", [Options0]),

    DefaultOptions = [{use_stdout, false}, debug_and_abort_on_error],
    Options = [expand_sh_flag(V)
               || V <- proplists:compact(Options0 ++ DefaultOptions)],

    ErrorHandler = proplists:get_value(error_handler, Options),
    OutputHandler = proplists:get_value(output_handler, Options),

    Command = lists:flatten(patch_on_windows(Command0, proplists:get_value(env, Options, []))),
    PortSettings = proplists:get_all_values(port_settings, Options) ++
        [exit_status, {line, 16384}, use_stdio, stderr_to_stdout, hide],
    ?DEBUG("Port Cmd: ~s\nPort Opts: ~p\n", [Command, PortSettings]),
    Port = open_port({spawn, Command}, PortSettings),

    case sh_loop(Port, OutputHandler, []) of
        {ok, _Output} = Ok ->
            Ok;
        {error, {_Rc, _Output}=Err} ->
            ErrorHandler(Command, Err)
    end.

find_files(Dir, Regex) ->
    find_files(Dir, Regex, true).

find_files(Dir, Regex, Recursive) ->
    filelib:fold_files(Dir, Regex, Recursive,
                       fun(F, Acc) -> [F | Acc] end, []).

find_executable(Name) ->
    case os:find_executable(Name) of
        false -> false;
        Path ->
            "\"" ++ filename:nativename(Path) ++ "\""
    end.

deprecated(Old, New, Opts, When) when is_list(Opts) ->
    case lists:member(Old, Opts) of
        true ->
            deprecated(Old, New, When);
        false ->
            ok
    end;
deprecated(Old, New, Config, When) ->
    case rebar_state:get(Config, Old, undefined) of
        undefined ->
            ok;
        _ ->
            deprecated(Old, New, When)
    end.

deprecated(Old, New, When) ->
    io:format(
      <<"WARNING: deprecated ~p option used~n"
        "Option '~p' has been deprecated~n"
        "in favor of '~p'.~n"
        "'~p' will be removed ~s.~n">>,
      [Old, Old, New, Old, When]).

%% @doc Return list of erl_opts
-spec erl_opts(rebar_state:t()) -> list().
erl_opts(Config) ->
    RawErlOpts = filter_defines(rebar_state:get(Config, erl_opts, []), []),
    Defines = [{d, list_to_atom(D)} ||
                  D <- rebar_state:get(Config, defines, [])],
    Opts = Defines ++ RawErlOpts,
    case proplists:is_defined(no_debug_info, Opts) of
        true ->
            [O || O <- Opts, O =/= no_debug_info];
        false ->
            [debug_info|Opts]
    end.

%% for use by `do` task

%% note: this does not handle the case where you have an argument that
%%  was enclosed in quotes and might have commas but should not be split.
args_to_tasks(Args) -> new_task(Args, []).

%% Sort the list in proplist-order, meaning that `{a,b}' and `{a,c}'
%% both compare as usual, and `a' and `b' do the same, but `a' and `{a,b}' will
%% compare based on the first element of the key, and in order. So the following
%% list will sort as:
%% - `[native, {native,o3}, check]' -> `[check, native, {native, o3}]'
%% - `[native, {native,o3}, {native, o2}, check]' -> `[check,native,{native,o3},{native,o2}]'
%% Meaning that:
%% a) no deduplication takes place
%% b) the key of a tuple is what counts in being sorted, but atoms are seen as {atom}
%%    as far as comparison is concerned (departing from lists:ukeysort/2)
%% c) order is preserved for similar keys and tuples no matter their size (sort is stable)
%%
%% These properties let us merge proplists fairly easily.
tup_sort(List) ->
    lists:sort(fun(A, B) when is_tuple(A), is_tuple(B) -> element(1, A) =< element(1, B)
               ;  (A, B) when is_tuple(A) -> element(1, A) =< B
               ;  (A, B) when is_tuple(B) -> A =< element(1, B)
               ;  (A, B) -> A =< B
               end, List).

%% Custom merge functions. The objective is to behave like lists:umerge/2,
%% except that we also compare the merge elements based on the key if they're a
%% tuple, such that `{key, val1}' is always prioritized over `{key, val0}' if
%% the former is from the 'new' list.
%%
%% This lets us apply proper overrides to list of elements according to profile
%% priority. This function depends on a stable proplist sort.
tup_umerge([], Olds) ->
    Olds;
tup_umerge([New|News], Olds) ->
    lists:reverse(umerge(News, Olds, [], New)).

%% This is equivalent to umerge2_2 in the stdlib, except we use the expanded
%% value/key only to compare
umerge(News, [Old|Olds], Merged, Cmp) when element(1, Cmp) == element(1, Old);
                                           element(1, Cmp) == Old;
                                           Cmp == element(1, Old);
                                           Cmp =< Old ->
    umerge(News, Olds, [Cmp | Merged], Cmp, Old);
umerge(News, [Old|Olds], Merged, Cmp) ->
    umerge(News, Olds, [Old | Merged], Cmp);
umerge(News, [], Merged, Cmp) ->
    lists:reverse(News, [Cmp | Merged]).

%% Similar to stdlib's umerge2_1 in the stdlib, except that when the expanded
%% value/keys compare equal, we check if the element is a full dupe to clear it
%% (like the stdlib function does) or otherwise keep the duplicate around in
%% an order that prioritizes 'New' elements.
umerge([New|News], Olds, Merged, CmpMerged, Cmp) when CmpMerged == Cmp ->
    umerge(News, Olds, Merged, New);
umerge([New|News], Olds, Merged, _CmpMerged, Cmp) when element(1,New) == element(1, Cmp);
                                                       element(1,New) == Cmp;
                                                       New == element(1, Cmp);
                                                       New =< Cmp ->
    umerge(News, Olds, [New | Merged], New, Cmp);
umerge([New|News], Olds, Merged, _CmpMerged, Cmp) -> % >
    umerge(News, Olds, [Cmp | Merged], New);
umerge([], Olds, Merged, CmpMerged, Cmp) when CmpMerged == Cmp ->
    lists:reverse(Olds, Merged);
umerge([], Olds, Merged, _CmpMerged, Cmp) ->
    lists:reverse(Olds, [Cmp | Merged]).

%% Implements wc -l functionality used to determine patchcount from git output
line_count(PatchLines) ->
    Tokenized = string:tokens(PatchLines, "\n"),
    {ok, length(Tokenized)}.

%% ====================================================================
%% Internal functions
%% ====================================================================

otp_release() ->
    otp_release1(erlang:system_info(otp_release)).

%% If OTP <= R16, otp_release is already what we want.
otp_release1([$R,N|_]=Rel) when is_integer(N) ->
    Rel;
%% If OTP >= 17.x, erlang:system_info(otp_release) returns just the
%% major version number, we have to read the full version from
%% a file. See http://www.erlang.org/doc/system_principles/versions.html
%% Read vsn string from the 'OTP_VERSION' file and return as list without
%% the "\n".
otp_release1(Rel) ->
    File = filename:join([code:root_dir(), "releases", Rel, "OTP_VERSION"]),
    case file:read_file(File) of
        {error, _} ->
            Rel;
        {ok, Vsn} ->
            %% It's fine to rely on the binary module here because we can
            %% be sure that it's available when the otp_release string does
            %% not begin with $R.
            Size = byte_size(Vsn),
            %% The shortest vsn string consists of at least two digits
            %% followed by "\n". Therefore, it's safe to assume Size >= 3.
            case binary:part(Vsn, {Size, -3}) of
                <<"**\n">> ->
                    %% The OTP documentation mentions that a system patched
                    %% using the otp_patch_apply tool available to licensed
                    %% customers will leave a '**' suffix in the version as a
                    %% flag saying the system consists of application versions
                    %% from multiple OTP versions. We ignore this flag and
                    %% drop the suffix, given for all intents and purposes, we
                    %% cannot obtain relevant information from it as far as
                    %% tooling is concerned.
                    binary:bin_to_list(Vsn, {0, Size - 3});
                _ ->
                    binary:bin_to_list(Vsn, {0, Size - 1})
            end
    end.

%% We do the shell variable substitution ourselves on Windows and hope that the
%% command doesn't use any other shell magic.
patch_on_windows(Cmd, Env) ->
    case os:type() of
        {win32,nt} ->
            Cmd1 = "cmd /q /c "
                ++ lists:foldl(fun({Key, Value}, Acc) ->
                                       expand_env_variable(Acc, Key, Value)
                               end, Cmd, Env),
            %% Remove left-over vars
            re:replace(Cmd1, "\\\$\\w+|\\\${\\w+}", "",
                       [global, {return, list}]);
        _ ->
            Cmd
    end.

%%
%% Given env. variable FOO we want to expand all references to
%% it in InStr. References can have two forms: $FOO and ${FOO}
%% The end of form $FOO is delimited with whitespace or eol
%%
expand_env_variable(InStr, VarName, RawVarValue) ->
    case string:chr(InStr, $$) of
        0 ->
            %% No variables to expand
            InStr;
        _ ->
            ReOpts = [global, unicode, {return, list}],
            VarValue = re:replace(RawVarValue, "\\\\", "\\\\\\\\", ReOpts),
            %% Use a regex to match/replace:
            %% Given variable "FOO": match $FOO\s | $FOOeol | ${FOO}
            RegEx = io_lib:format("\\\$(~s(\\W|$)|{~s})", [VarName, VarName]),
            re:replace(InStr, RegEx, [VarValue, "\\2"], ReOpts)
    end.

expand_sh_flag(return_on_error) ->
    {error_handler,
     fun(_Command, Err) ->
             {error, Err}
     end};
expand_sh_flag(abort_on_error) ->
    {error_handler,
     fun log_and_abort/2};
expand_sh_flag({abort_on_error, Message}) ->
    {error_handler,
     log_msg_and_abort(Message)};
expand_sh_flag({debug_abort_on_error, Message}) ->
    {error_handler,
     debug_log_msg_and_abort(Message)};
expand_sh_flag(debug_and_abort_on_error) ->
    {error_handler,
     fun debug_and_abort/2};
expand_sh_flag(use_stdout) ->
    {output_handler,
     fun(Line, Acc) ->
             %% Line already has a newline so don't use ?CONSOLE which adds one
             io:format("~s", [Line]),
             [Line | Acc]
     end};
expand_sh_flag({use_stdout, false}) ->
    {output_handler,
     fun(Line, Acc) ->
             [Line | Acc]
     end};
expand_sh_flag({cd, _CdArg} = Cd) ->
    {port_settings, Cd};
expand_sh_flag({env, _EnvArg} = Env) ->
    {port_settings, Env}.

-type err_handler() :: fun((string(), {integer(), string()}) -> no_return()).
-spec log_msg_and_abort(string()) -> err_handler().
log_msg_and_abort(Message) ->
    fun(_Command, {_Rc, _Output}) ->
            ?ABORT(Message, [])
    end.

debug_log_msg_and_abort(Message) ->
    fun(Command, {Rc, Output}) ->
            ?DEBUG("sh(~s)~n"
                  "failed with return code ~w and the following output:~n"
                  "~s", [Command, Rc, Output]),
            ?ABORT(Message, [])
    end.

-spec log_and_abort(string(), {integer(), string()}) -> no_return().
log_and_abort(Command, {Rc, Output}) ->
    ?ABORT("sh(~s)~n"
          "failed with return code ~w and the following output:~n"
          "~s", [Command, Rc, Output]).

-spec debug_and_abort(string(), {integer(), string()}) -> no_return().
debug_and_abort(Command, {Rc, Output}) ->
    ?DEBUG("sh(~s)~n"
          "failed with return code ~w and the following output:~n"
          "~s", [Command, Rc, Output]),
    throw(rebar_abort).

sh_loop(Port, Fun, Acc) ->
    receive
        {Port, {data, {eol, Line}}} ->
            sh_loop(Port, Fun, Fun(Line ++ "\n", Acc));
        {Port, {data, {noeol, Line}}} ->
            sh_loop(Port, Fun, Fun(Line, Acc));
        {Port, {exit_status, 0}} ->
            {ok, lists:flatten(lists:reverse(Acc))};
        {Port, {exit_status, 1}} ->
            {ok, lists:flatten(lists:reverse(Acc))};
        {Port, {exit_status, 3}} ->
            {ok, lists:flatten(lists:reverse(Acc))};
        {Port, {exit_status, Rc}} ->
            {error, {Rc, lists:flatten(lists:reverse(Acc))}}
    end.

beam_to_mod(Dir, Filename) ->
    [Dir | Rest] = filename:split(Filename),
    list_to_atom(filename:basename(string:join(Rest, "."), ".beam")).

beam_to_mod(Filename) ->
    list_to_atom(filename:basename(Filename, ".beam")).

erl_to_mod(Filename) ->
    list_to_atom(filename:rootname(filename:basename(Filename))).

beams(Dir) ->
    filelib:wildcard(filename:join(Dir, "*.beam")).

-spec abort() -> no_return().
abort() ->
    throw(rebar_abort).

-spec abort(string(), [term()]) -> no_return().
abort(String, Args) ->
    ?ERROR(String, Args),
    abort().

escript_foldl(Fun, Acc, File) ->
    case escript:extract(File, [compile_source]) of
        {ok, [_Shebang, _Comment, _EmuArgs, Body]} ->
            case Body of
                {source, BeamCode} ->
                    GetInfo = fun() -> file:read_file_info(File) end,
                    GetBin = fun() -> BeamCode end,
                    {ok, Fun(".", GetInfo, GetBin, Acc)};
                {beam, BeamCode} ->
                    GetInfo = fun() -> file:read_file_info(File) end,
                    GetBin = fun() -> BeamCode end,
                    {ok, Fun(".", GetInfo, GetBin, Acc)};
                {archive, ArchiveBin} ->
                    zip:foldl(Fun, Acc, {File, ArchiveBin})
            end;
        {error, _} = Error ->
            Error
    end.

vcs_vsn(Vcs, Dir, Resources) ->
    case vcs_vsn_cmd(Vcs, Dir, Resources) of
        {plain, VsnString} ->
            VsnString;
        {cmd, CmdString} ->
            vcs_vsn_invoke(CmdString, Dir);
        unknown ->
            ?ABORT("vcs_vsn: Unknown vsn format: ~p", [Vcs]);
        {error, Reason} ->
            ?ABORT("vcs_vsn: ~s", [Reason])
    end.

%% Temp work around for repos like relx that use "semver"
vcs_vsn_cmd(VCS, Dir, Resources) when VCS =:= semver ; VCS =:= "semver" ->
    vcs_vsn_cmd(git, Dir, Resources);
vcs_vsn_cmd({cmd, _Cmd}=Custom, _, _) ->
    Custom;
vcs_vsn_cmd(VCS, Dir, Resources) when is_atom(VCS) ->
    case find_resource_module(VCS, Resources) of
        {ok, Module} ->
            Module:make_vsn(Dir);
        {error, _} ->
            unknown
    end;
vcs_vsn_cmd(VCS, Dir, Resources) when is_list(VCS) ->
    try list_to_existing_atom(VCS) of
        AVCS ->
            case vcs_vsn_cmd(AVCS, Dir, Resources) of
                unknown -> {plain, VCS};
                Other -> Other
            end
    catch
        error:badarg ->
            {plain, VCS}
    end;
vcs_vsn_cmd(_, _, _) ->
    unknown.

vcs_vsn_invoke(Cmd, Dir) ->
    {ok, VsnString} = rebar_utils:sh(Cmd, [{cd, Dir}, {use_stdout, false}]),
    string:strip(VsnString, right, $\n).

find_resource_module(Type, Resources) ->
    case lists:keyfind(Type, 1, Resources) of
        false ->
            case code:which(Type) of
                non_existing ->
                    {error, unknown};
                _ ->
                    {ok, Type}
            end;
        {Type, Module} ->
            {ok, Module}
    end.

%%
%% Filter a list of erl_opts platform_define options such that only
%% those which match the provided architecture regex are returned.
%%
filter_defines([], Acc) ->
    lists:reverse(Acc);
filter_defines([{platform_define, ArchRegex, Key} | Rest], Acc) ->
    case rebar_utils:is_arch(ArchRegex) of
        true ->
            filter_defines(Rest, [{d, Key} | Acc]);
        false ->
            filter_defines(Rest, Acc)
    end;
filter_defines([{platform_define, ArchRegex, Key, Value} | Rest], Acc) ->
    case rebar_utils:is_arch(ArchRegex) of
        true ->
            filter_defines(Rest, [{d, Key, Value} | Acc]);
        false ->
            filter_defines(Rest, Acc)
    end;
filter_defines([Opt | Rest], Acc) ->
    filter_defines(Rest, [Opt | Acc]).

%% @doc ident to the level specified
-spec indent(non_neg_integer()) -> iolist().
indent(Amount) when erlang:is_integer(Amount) ->
    [?ONE_LEVEL_INDENT || _ <- lists:seq(1, Amount)].

%% Replace code paths with new paths for existing apps and
%% purge code of the old modules from those apps.
update_code(Paths) ->
    lists:foreach(fun(Path) ->
                          Name = filename:basename(Path, "/ebin"),
                          App = list_to_atom(Name),
                          application:load(App),
                          case application:get_key(App, modules) of
                              undefined ->
                                  code:add_patha(Path),
                                  ok;
                              {ok, Modules} ->
                                  code:replace_path(Name, Path),
                                  [begin code:purge(M), code:delete(M) end || M <- Modules]
                          end
                  end, Paths).

remove_from_code_path(Paths) ->
    lists:foreach(fun(Path) ->
                          Name = filename:basename(Path, "/ebin"),
                          App = list_to_atom(Name),
                          application:load(App),
                          case application:get_key(App, modules) of
                              undefined ->
                                  application:unload(App),
                                  ok;
                              {ok, Modules} ->
                                  application:unload(App),
                                  [begin code:purge(M), code:delete(M) end || M <- Modules]
                          end,
                          code:del_path(Path)
                  end, Paths).

cleanup_code_path(OrigPath) ->
    CurrentPath = code:get_path(),
    AddedPaths = CurrentPath -- OrigPath,
    %% If someone has removed paths, it's hard to get them back into
    %% the right order, but since this is currently rare, we can just
    %% fall back to code:set_path/1.
    case CurrentPath -- AddedPaths of
        OrigPath ->
            _ = [code:del_path(Path) || Path <- AddedPaths],
            true;
        _ ->
            code:set_path(OrigPath)
    end.

new_task([], Acc) -> lists:reverse(Acc);
new_task([TaskList|Rest], Acc) ->
    case re:split(TaskList, ",", [{return, list}, {parts, 2}]) of
        %% `do` consumes all remaining args
        ["do" = Task] ->
            lists:reverse([{Task, Rest}|Acc]);
        %% single task terminated by a comma
        [Task, ""]    -> new_task(Rest, [{Task, []}|Acc]);
        %% sequence of two or more tasks
        [Task, More]  -> new_task([More|Rest], [{Task, []}|Acc]);
        %% single task not terminated by a comma
        [Task]        -> arg_or_flag(Rest, [{Task, []}|Acc])
    end.

arg_or_flag([], [{Task, Args}|Acc]) ->
    lists:reverse([{Task, lists:reverse(Args)}|Acc]);
%% case where you have `foo , bar`
arg_or_flag([","|Rest], Acc) -> new_task(Rest, Acc);
%% case where you have `foo ,bar`
arg_or_flag(["," ++ Task|Rest], Acc) -> new_task([Task|Rest], Acc);
%% a flag
arg_or_flag(["-" ++ _ = Flag|Rest], [{Task, Args}|Acc]) ->
    case maybe_ends_in_comma(Flag) of
        false   -> arg_or_flag(Rest, [{Task, [Flag|Args]}|Acc]);
        NewFlag -> new_task(Rest, [{Task,
                                    lists:reverse([NewFlag|Args])}|Acc])
    end;
%% an argument or a sequence of arguments
arg_or_flag([ArgList|Rest], [{Task, Args}|Acc]) ->
    case re:split(ArgList, ",", [{return, list}, {parts, 2}]) of
        %% single arg terminated by a comma
        [Arg, ""]   -> new_task(Rest, [{Task,
                                        lists:reverse([Arg|Args])}|Acc]);
        %% sequence of two or more args/tasks
        [Arg, More] -> new_task([More|Rest], [{Task,
                                              lists:reverse([Arg|Args])}|Acc]);
        %% single arg not terminated by a comma
        [Arg] -> arg_or_flag(Rest, [{Task, [Arg|Args]}|Acc])
    end.

maybe_ends_in_comma(H) ->
    case lists:reverse(H) of
        "," ++ Flag -> lists:reverse(Flag);
        _           -> false
    end.
