%%%-------------------------------------------------------------------
%% @copyright {{copyright_holder}} ({{copyright_year}})
%% @author {{author_name}} <{{author_email}}>
%% @doc CommonTest test suite for {{test}}
%% @end
%%%-------------------------------------------------------------------

-module({{test}}_SUITE).
-include_lib("common_test/include/ct.hrl").
-compile(export_all).

%%%%%%%%%%%%%%%%%%%%
%%% Tests to run %%%
%%%%%%%%%%%%%%%%%%%%
%% Specific test cases or groups to run. The test case is named as
%% a single atom.

all() ->
    [default_case].

%%%%%%%%%%%%%%%%%%%%%%
%%% Setup/Teardown %%%
%%%%%%%%%%%%%%%%%%%%%%
%% Runs once at the beginning of the suite. The process is different
%% from the one the case will run in.
init_per_suite(Config) ->
    Config.

%% Runs once at the end of the suite. The process is different
%% from the one the case will run in.
end_per_suite(Config) ->
    Config.

%% Runs before the test case. Runs in the same process.
init_per_testcase(_CaseName, Config) ->
    Config.

%% Runs after the test case. Runs in the same process.
end_per_testcase(_CaseName, Config) ->
    Config.

%%%%%%%%%%%%%
%%% TESTS %%%
%%%%%%%%%%%%%
default_case(Config) ->
    _Priv = ?config(priv_dir, Config),
    _Data = ?config(data_dir, Config),
    ok.
