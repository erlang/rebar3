-module(rebar_resource_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

all() -> [{group, git}, {group, pkg}, {group, hg}].

groups() ->
    [{all, [], [change_type_upgrade]},
     {git, [], [{group, all}]},
     {pkg, [], [{group, all}]},
     {hg, [], [{group, all}]}].

init_per_group(all, Config) ->
    Config;
init_per_group(Name, Config) ->
    [{type, Name},
     {resource, {Name, "https://example.org/user/app", "vsn"}} | Config].

%% Changing the resource type is seen as an upgrade
init_per_testcase(change_type_upgrade, Config) ->
    Type = ?config(type, Config),
    TypeStr = atom_to_list(Type),
    DirName = filename:join([?config(priv_dir, Config), "resource_"++TypeStr]),
    ec_file:mkdir_path(DirName),
    [{path, DirName} | Config].

end_per_testcase(_, Config) ->
    Config.

change_type_upgrade(Config) ->
    ?assert(rebar_fetch:needs_update(?config(path, Config),
                                     ?config(resource, Config))).
