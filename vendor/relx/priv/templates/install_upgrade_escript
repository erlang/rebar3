#!/usr/bin/env escript
%%! -noinput
%% -*- mode: erlang;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ft=erlang ts=4 sw=4 et

-define(TIMEOUT, 300000).
-define(INFO(Fmt,Args), io:format(Fmt,Args)).

main([Command0, DistInfoStr | CommandArgs]) ->
    %% convert the distribution info arguments string to an erlang term
    {ok, Tokens, _} = erl_scan:string(DistInfoStr ++ "."),
    {ok, DistInfo} = erl_parse:parse_term(Tokens),
    %% convert arguments into a proplist
    Opts = parse_arguments(CommandArgs),
    %% invoke the command passed as argument
    F = case Command0 of
        "install" -> fun(A, B) -> install(A, B) end;
        "unpack" -> fun(A, B) -> unpack(A, B) end;
        "upgrade" -> fun(A, B) -> upgrade(A, B) end;
        "downgrade" -> fun(A, B) -> downgrade(A, B) end;
        "uninstall" -> fun(A, B) -> uninstall(A, B) end;
        "versions" -> fun(A, B) -> versions(A, B) end
    end,
    F(DistInfo, Opts);
main(Args) ->
    ?INFO("unknown args: ~p\n", [Args]),
    erlang:halt(1).

unpack({RelName, NameTypeArg, NodeName, Cookie}, Opts) ->
    TargetNode = start_distribution(NodeName, NameTypeArg, Cookie),
    Version = proplists:get_value(version, Opts),
    case unpack_release(RelName, TargetNode, Version) of
        {ok, Vsn} ->
            ?INFO("Unpacked successfully: ~p~n", [Vsn]);
        old ->
            %% no need to unpack, has been installed previously
            ?INFO("Release ~s is marked old.~n",[Version]);
        unpacked ->
            ?INFO("Release ~s is already unpacked.~n",[Version]);
        current ->
            ?INFO("Release ~s is already installed and current.~n",[Version]);
        permanent ->
            ?INFO("Release ~s is already installed and set permanent.~n",[Version]);
        {error, Reason} ->
            ?INFO("Unpack failed: ~p~n",[Reason]),
            print_existing_versions(TargetNode),
            erlang:halt(2)
    end;
unpack(_, Args) ->
    ?INFO("unpack: unknown args ~p\n", [Args]).

install({RelName, NameTypeArg, NodeName, Cookie}, Opts) ->
    TargetNode = start_distribution(NodeName, NameTypeArg, Cookie),
    Version = proplists:get_value(version, Opts),
    case unpack_release(RelName, TargetNode, Version) of
        {ok, Vsn} ->
            ?INFO("Unpacked successfully: ~p~n", [Vsn]),
            check_and_install(TargetNode, Vsn),
            maybe_permafy(TargetNode, RelName, Vsn, Opts);
        old ->
            %% no need to unpack, has been installed previously
            ?INFO("Release ~s is marked old, switching to it.~n",[Version]),
            check_and_install(TargetNode, Version),
            maybe_permafy(TargetNode, RelName, Version, Opts);
        unpacked ->
            ?INFO("Release ~s is already unpacked, now installing.~n",[Version]),
            check_and_install(TargetNode, Version),
            maybe_permafy(TargetNode, RelName, Version, Opts);
        current ->
            case proplists:get_value(permanent, Opts, true) of
                true ->
                    ?INFO("Release ~s is already installed and current, making permanent.~n",
                        [Version]),
                    permafy(TargetNode, RelName, Version);
                false ->
                    ?INFO("Release ~s is already installed and current.~n",
                        [Version])
            end;
        permanent ->
            %% this release is marked permanent, however it might not the
            %% one currently running
            case current_release_version(TargetNode) of
                Version ->
                    ?INFO("Release ~s is already installed, running and set permanent.~n",
                        [Version]);
                CurrentVersion ->
                    ?INFO("Release ~s is the currently running version.~n",
                        [CurrentVersion]),
                    check_and_install(TargetNode, Version),
                    maybe_permafy(TargetNode, RelName, Version, Opts)
            end;
        {error, Reason} ->
            ?INFO("Unpack failed: ~p~n",[Reason]),
            print_existing_versions(TargetNode),
            erlang:halt(2)
    end;
install(_, Args) ->
    ?INFO("install: unknown args ~p\n", [Args]).

upgrade(DistInfo, Args) ->
    install(DistInfo, Args).

downgrade(DistInfo, Args) ->
    install(DistInfo, Args).

uninstall({_RelName, NameTypeArg, NodeName, Cookie}, Opts) ->
    TargetNode = start_distribution(NodeName, NameTypeArg, Cookie),
    WhichReleases = which_releases(TargetNode),
    Version = proplists:get_value(version, Opts),
    case proplists:get_value(Version, WhichReleases) of
        undefined ->
            ?INFO("Release ~s is already uninstalled.~n", [Version]);
        old ->
            ?INFO("Release ~s is marked old, uninstalling it.~n", [Version]),
            remove_release(TargetNode, Version);
        unpacked ->
            ?INFO("Release ~s is marked unpacked, uninstalling it~n",
                [Version]),
            remove_release(TargetNode, Version);
        current ->
            ?INFO("Uninstall failed: Release ~s is marked current.~n", [Version]),
            erlang:halt(2);
        permanent ->
            ?INFO("Uninstall failed: Release ~s is running.~n", [Version]),
            erlang:halt(2)
    end;
uninstall(_, Args) ->
    ?INFO("uninstall: unknown args ~p\n", [Args]).

versions({_RelName, NameTypeArg, NodeName, Cookie}, []) ->
    TargetNode = start_distribution(NodeName, NameTypeArg, Cookie),
    print_existing_versions(TargetNode).

parse_arguments(Args) ->
    parse_arguments(Args, []).

parse_arguments([], Acc) -> Acc;
parse_arguments(["--no-permanent"|Rest], Acc) ->
    parse_arguments(Rest, [{permanent, false}] ++ Acc);
parse_arguments([VersionStr|Rest], Acc) ->
    Version = parse_version(VersionStr),
    parse_arguments(Rest, [{version, Version}] ++ Acc).

unpack_release(RelName, TargetNode, Version) ->
    WhichReleases = which_releases(TargetNode),
    case proplists:get_value(Version, WhichReleases) of
        undefined ->
            %% not installed, so unpack tarball:
            %% look for a release package with the intended version in the following order:
            %%      releases/<relname>-<version>.tar.gz
            %%      releases/<version>/<relname>-<version>.tar.gz
            %%      releases/<version>/<relname>.tar.gz
            case find_and_link_release_package(Version, RelName) of
                {_, undefined} ->
                    {error, release_package_not_found};
                {ReleasePackage, ReleasePackageLink} ->
                    ?INFO("Release ~s not found, attempting to unpack ~s~n",
                        [Version, ReleasePackage]),
                    case rpc:call(TargetNode, release_handler, unpack_release,
                                  [ReleasePackageLink], ?TIMEOUT) of
                        {ok, Vsn} -> {ok, Vsn};
                        {error, _} = Error -> Error
                    end
            end;
        Other -> Other
    end.

%% 1. look for a release package tarball with the provided version in the following order:
%%      releases/<relname>-<version>.tar.gz
%%      releases/<version>/<relname>-<version>.tar.gz
%%      releases/<version>/<relname>.tar.gz
%% 2. create a symlink from a fixed location (ie. releases/<version>/<relname>.tar.gz)
%%    to the release package tarball found in 1.
%% 3. return a tuple with the paths to the release package and
%%    to the symlink that is to be provided to release handler
find_and_link_release_package(Version, RelName) ->
    RelNameStr = atom_to_list(RelName),
    %% regardless of the location of the release package, we'll
    %% always give release handler the same path which is the symlink
    %% the path to the package link is relative to "releases/" because
    %% that's what release handler is expecting
    ReleaseHandlerPackageLink = filename:join(Version, RelNameStr),
    %% this is the symlink name we'll create once
    %% we've found where the actual release package is located
    ReleaseLink = filename:join(["releases", Version,
                                 RelNameStr ++ ".tar.gz"]),
    case first_value(fun filelib:is_file/1,
                     [filename:join(["releases",
                                     RelNameStr ++ "-" ++ Version ++ ".tar.gz"]),
                      filename:join(["releases", Version,
                                     RelNameStr ++ "-" ++ Version ++ ".tar.gz"]),
                      filename:join(["releases", Version,
                                     RelNameStr ++ ".tar.gz"])]) of
        no_value ->
            {undefined, undefined};
        %% no need to create the link since the release package we
        %% found is located in the same place as the link would be
        {ok, Filename} when is_list(Filename) andalso
                            Filename =:= ReleaseLink ->
            {Filename, ReleaseHandlerPackageLink};
        {ok, Filename} when is_list(Filename) ->
            %% we now have the location of the release package, however
            %% release handler expects a fixed nomenclature (<relname>.tar.gz)
            %% so give it just that by creating a symlink to the tarball
            %% we found.
            %% make sure that the dir where we're creating the link in exists
            ok = filelib:ensure_dir(filename:join([filename:dirname(ReleaseLink), "dummy"])),
            %% create the symlink pointing to the full path name of the
            %% release package we found
            case file:make_symlink(filename:absname(Filename), ReleaseLink) of
                ok ->
                    ok;
                {error, eperm} -> % windows!
                    {ok,_} = file:copy(filename:absname(Filename), ReleaseLink)
            end,
            {Filename, ReleaseHandlerPackageLink}
    end.

first_value(_Fun, []) -> no_value;
first_value(Fun, [Value | Rest]) ->
    case Fun(Value) of
        false ->
            first_value(Fun, Rest);
        true ->
            {ok, Value}
    end.

parse_version(V) when is_list(V) ->
    hd(string:tokens(V,"/")).

check_and_install(TargetNode, Vsn) ->
    case rpc:call(TargetNode, release_handler,
                  check_install_release, [Vsn], ?TIMEOUT) of
        {ok, _OtherVsn, _Desc} ->
            ok;
        {error, Reason} ->
            ?INFO("ERROR: release_handler:check_install_release failed: ~p~n",[Reason]),
            erlang:halt(3)
    end,
    case rpc:call(TargetNode, release_handler, install_release,
                  [Vsn, [{update_paths, true}]], ?TIMEOUT) of
        {ok, _, _} ->
            ?INFO("Installed Release: ~s~n", [Vsn]),
            ok;
        {error, {no_such_release, Vsn}} ->
            VerList =
                iolist_to_binary(
                    [io_lib:format("* ~s\t~s~n",[V,S]) ||  {V,S} <- which_releases(TargetNode)]),
            ?INFO("Installed versions:~n~s", [VerList]),
            ?INFO("ERROR: Unable to revert to '~s' - not installed.~n", [Vsn]),
            erlang:halt(2);
        %% as described in http://erlang.org/doc/man/appup.html, when performing a relup
        %% with soft purge:
        %%      If the value is soft_purge, release_handler:install_release/1
        %%      returns {error,{old_processes,Mod}}
        {error, {old_processes, Mod}} ->
            ?INFO("ERROR: unable to install '~s' - old processes still running code from module ~p~n",
                [Vsn, Mod]),
            erlang:halt(3);
        {error, Reason1} ->
            ?INFO("ERROR: release_handler:install_release failed: ~p~n",[Reason1]),
            erlang:halt(4)
    end.

maybe_permafy(TargetNode, RelName, Vsn, Opts) ->
    case proplists:get_value(permanent, Opts, true) of
        true ->
            permafy(TargetNode, RelName, Vsn);
        false -> ok
    end.

permafy(TargetNode, RelName, Vsn) ->
    ok = rpc:call(TargetNode, release_handler,
                  make_permanent, [Vsn], ?TIMEOUT),
    file:copy(filename:join(["bin", atom_to_list(RelName)++"-"++Vsn]),
              filename:join(["bin", atom_to_list(RelName)])),
    ?INFO("Made release permanent: ~p~n", [Vsn]),
    ok.

remove_release(TargetNode, Vsn) ->
    case rpc:call(TargetNode, release_handler, remove_release, [Vsn], ?TIMEOUT) of
        ok ->
            ?INFO("Uninstalled Release: ~s~n", [Vsn]),
            ok;
        {error, Reason} ->
            ?INFO("ERROR: release_handler:remove_release failed: ~p~n", [Reason]),
            erlang:halt(3)
    end.

which_releases(TargetNode) ->
    R = rpc:call(TargetNode, release_handler, which_releases, [], ?TIMEOUT),
    [ {V, S} ||  {_,V,_, S} <- R ].

%% the running release version is either the only one marked `current´
%% or, if none exists, the one marked `permanent`
current_release_version(TargetNode) ->
    R = rpc:call(TargetNode, release_handler, which_releases,
                [], ?TIMEOUT),
    Versions = [ {S, V} ||  {_,V,_, S} <- R ],
    %% current version takes priority over the permanent
    proplists:get_value(current, Versions,
        proplists:get_value(permanent, Versions)).

print_existing_versions(TargetNode) ->
    VerList = iolist_to_binary([
            io_lib:format("* ~s\t~s~n",[V,S])
            ||  {V,S} <- which_releases(TargetNode) ]),
    ?INFO("Installed versions:~n~s", [VerList]).

start_distribution(TargetNode, NameTypeArg, Cookie) ->
    MyNode = make_script_node(TargetNode),
    {ok, _Pid} = net_kernel:start([MyNode, get_name_type(NameTypeArg)]),
    erlang:set_cookie(node(), Cookie),
    case {net_kernel:connect_node(TargetNode),
          net_adm:ping(TargetNode)} of
        {true, pong} ->
            ok;
        {_, pang} ->
            ?INFO("Node ~p not responding to pings.\n", [TargetNode]),
            erlang:halt(1)
    end,
    {ok, Cwd} = file:get_cwd(),
    ok = rpc:call(TargetNode, file, set_cwd, [Cwd], ?TIMEOUT),
    TargetNode.

make_script_node(Node) ->
    [Name, Host] = string:tokens(atom_to_list(Node), "@"),
    list_to_atom(lists:concat([Name, "_upgrader_", os:getpid(), "@", Host])).

%% get name type from arg
get_name_type(NameTypeArg) ->
	case NameTypeArg of
		"-sname" ->
			shortnames;
		_ ->
			longnames
	end.
