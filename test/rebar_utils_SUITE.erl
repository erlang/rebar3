-module(rebar_utils_SUITE).

-export([all/0,
         groups/0,
         init_per_group/2,
         end_per_group/2,
         empty_arglist/1,
         single_task/1,
         single_task_with_immediate_comma/1,
         single_task_with_trailing_comma/1,
         multiple_task/1,
         multiple_task_no_spaces/1,
         multiple_task_with_immediate_comma/1,
         multiple_task_with_trailing_comma/1,
         task_with_arg/1,
         task_with_arg_with_immediate_comma/1,
         task_with_arg_with_trailing_comma/1,
         task_with_multiple_args/1,
         task_with_flag/1,
         task_with_flag_with_immediate_comma/1,
         task_with_flag_with_trailing_comma/1,
         task_with_flag_with_commas/1,
         task_with_multiple_flags/1,
         special_task_do/1,
         trivial_umerge/1,
         three_tuple_umerge/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").


all() ->
    [{group, args_to_tasks},
     trivial_umerge,
     three_tuple_umerge
    ].

groups() ->
    [{args_to_tasks, [], [empty_arglist,
                          single_task,
                          single_task_with_immediate_comma,
                          single_task_with_trailing_comma,
                          multiple_task,
                          multiple_task_no_spaces,
                          multiple_task_with_immediate_comma,
                          multiple_task_with_trailing_comma,
                          task_with_arg,
                          task_with_arg_with_immediate_comma,
                          task_with_arg_with_trailing_comma,
                          task_with_multiple_args,
                          task_with_flag,
                          task_with_flag_with_immediate_comma,
                          task_with_flag_with_trailing_comma,
                          task_with_flag_with_commas,
                          task_with_multiple_flags,
                          special_task_do]}].

init_per_group(_, Config) -> Config.
end_per_group(_, Config) -> Config.

empty_arglist(_Config) ->
    [] = rebar_utils:args_to_tasks([]).

single_task(_Config) ->
    [{"foo", []}] = rebar_utils:args_to_tasks(["foo"]).

single_task_with_immediate_comma(_Config) ->
    [{"foo", []}] = rebar_utils:args_to_tasks(["foo,"]).

single_task_with_trailing_comma(_Config) ->
    [{"foo", []}] = rebar_utils:args_to_tasks(["foo", ","]).

multiple_task(_Config) ->
    [{"foo", []}, {"bar", []}, {"baz", []}] = rebar_utils:args_to_tasks(["foo,",
                                                                         "bar,",
                                                                         "baz"]).

multiple_task_no_spaces(_Config) ->
    [{"foo", []}, {"bar", []}, {"baz", []}] = rebar_utils:args_to_tasks(["foo,bar,baz"]).

multiple_task_with_immediate_comma(_Config) ->
    [{"foo", []}, {"bar", []}, {"baz", []}] = rebar_utils:args_to_tasks(["foo,",
                                                                         "bar,",
                                                                         "baz,"]).

multiple_task_with_trailing_comma(_Config) ->
    [{"foo", []}, {"bar", []}, {"baz", []}] = rebar_utils:args_to_tasks(["foo",
                                                                         ",",
                                                                         "bar",
                                                                         ",",
                                                                         "baz",
                                                                         ","]).
task_with_arg(_Config) ->
    [{"foo", ["bar"]}] = rebar_utils:args_to_tasks(["foo", "bar"]).

task_with_arg_with_immediate_comma(_Config) ->
    [{"foo", ["bar"]}, {"baz", []}] = rebar_utils:args_to_tasks(["foo", "bar,", "baz"]).

task_with_arg_with_trailing_comma(_Config) ->
    [{"foo", ["bar"]}, {"baz", []}] = rebar_utils:args_to_tasks(["foo", "bar", ",", "baz"]).

task_with_multiple_args(_Config) ->
    [{"foo", ["bar", "baz"]}] = rebar_utils:args_to_tasks(["foo", "bar", "baz"]).

task_with_flag(_Config) ->
    [{"foo", ["--bar"]}] = rebar_utils:args_to_tasks(["foo", "--bar"]).

task_with_flag_with_immediate_comma(_Config) ->
    [{"foo", ["--bar"]}, {"baz", []}] = rebar_utils:args_to_tasks(["foo", "--bar,", "baz"]).

task_with_flag_with_trailing_comma(_Config) ->
    [{"foo", ["--bar"]}, {"baz", []}] = rebar_utils:args_to_tasks(["foo", "--bar", ",", "baz"]).

task_with_flag_with_commas(_Config) ->
    [{"foo", ["--bar=baz,qux"]}] = rebar_utils:args_to_tasks(["foo", "--bar=baz,qux"]).

task_with_multiple_flags(_Config) ->
    [{"foo", ["--bar", "--baz"]}] = rebar_utils:args_to_tasks(["foo", "--bar", "--baz"]).

special_task_do(_Config) ->
    [{"foo", []}, {"do", ["bar,", "baz"]}] = rebar_utils:args_to_tasks(["foo,",
                                                                        "do",
                                                                        "bar,",
                                                                        "baz"]).

trivial_umerge(_Config) ->
    New = [{key, foo}],
    Old = [{key, bar}],
    Result = rebar_utils:tup_umerge(New, Old),
    ?assertEqual([{key, foo}], Result).

three_tuple_umerge(_Config) ->
    New = rebar_utils:tup_sort([{d, foo, true}, {d, bar, true}]),
    Old = rebar_utils:tup_sort([{d, foo, false}, {d, bar, true}]),
    Result = rebar_utils:tup_umerge(New, Old),
    ?assertEqual(
        rebar_utils:tup_sort([{do, foo, true}, {d, bar, true}]),
        Result
    ).
