-module(rebar_manifest_SUITE).

-export([all/0,
         init_per_testcase/2,
         end_per_testcase/2,
         basic_check/1,
         write_to_file_erlang/1,
         write_to_file_eetf/1,
         write_to_file_json/1,
         non_supported_format/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-define(NAMESPACE, "experimental").

all() -> [basic_check,
          write_to_file_erlang,
          write_to_file_eetf,
          write_to_file_json,
          non_supported_format].

init_per_testcase(Case, Config0) ->
    %% Create a project directory in the test run's priv_dir
    Config = rebar_test_utils:init_rebar_state(Config0),
    %% Create toy applications
    AppDir = ?config(apps, Config),
    Name = rebar_test_utils:create_random_name("app1_"++atom_to_list(Case)),
    Vsn = rebar_test_utils:create_random_vsn(),
    rebar_test_utils:create_app(AppDir, Name, Vsn, [kernel, stdlib]),
    %% Add the data to the test config
    [{name, unicode:characters_to_binary(Name)} | Config].

end_per_testcase(_, Config) ->
    Config.

basic_check(Config) ->
    case rebar_prv_manifest:is_json_available() of
        true ->
            rebar_test_utils:run_and_check(Config, [],
                                           [?NAMESPACE, "manifest"],
                                           {ok, []});
        false ->
            rebar_test_utils:run_and_check(Config, [],
                                           [?NAMESPACE, "manifest"],
                                           {error, {rebar_prv_manifest, no_json_module}})
    end.

write_to_file_erlang(Config) ->
    AppName = proplists:get_value(name, Config),
    PrivDir = proplists:get_value(priv_dir, Config),
    FilePath = filename:join([PrivDir, "manifest"]),
    rebar_test_utils:run_and_check(Config, [],
                                   [?NAMESPACE, "manifest", "--to", FilePath, "--format", "erlang"],
                                   {ok, []}),
    {ok, [Manifest]} = file:consult(FilePath),
    ?assertMatch(#{deps := [], apps := [#{name := AppName}]}, Manifest).

write_to_file_eetf(Config) ->
    AppName = proplists:get_value(name, Config),
    PrivDir = proplists:get_value(priv_dir, Config),
    FilePath = filename:join([PrivDir, "manifest"]),
    rebar_test_utils:run_and_check(Config, [],
                                   [?NAMESPACE, "manifest", "--to", FilePath, "--format", "eetf"],
                                   {ok, []}),
    {ok, Content} = file:read_file(FilePath),
    Manifest = binary_to_term(Content),
    ?assertMatch(#{deps := [], apps := [#{name := AppName}]}, Manifest).

write_to_file_json(Config) ->
    AppName = proplists:get_value(name, Config),
    PrivDir = proplists:get_value(priv_dir, Config),
    FilePath = filename:join([PrivDir, "manifest"]),
    case rebar_prv_manifest:is_json_available() of
        true ->
            rebar_test_utils:run_and_check(Config, [],
                                           [?NAMESPACE, "manifest", "--to", FilePath],
                                           {ok, []}),
            {ok, Content} = file:read_file(FilePath),
            Manifest = erlang:apply(json, decode, [Content]),
            ?assertMatch(#{<<"deps">> := [], <<"apps">> := [#{<<"name">> := AppName}]}, Manifest);
       false ->
           rebar_test_utils:run_and_check(Config, [],
                                          [?NAMESPACE, "manifest", "--to", FilePath, "--format", "json"],
                                          {error, {rebar_prv_manifest, no_json_module}})
    end.

non_supported_format(Config) ->
    PrivDir = proplists:get_value(priv_dir, Config),
    FilePath = filename:join([PrivDir, "manifest"]),
    rebar_test_utils:run_and_check(Config, [],
                                   [?NAMESPACE, "manifest", "--to", FilePath, "--format", "non-existing"],
                                   {error, {rebar_prv_manifest, {format_not_supported, 'non-existing'}}}).
