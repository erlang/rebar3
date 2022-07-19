-module(rlx_relup).

-export([do/4,
         format_error/1]).

-include("relx.hrl").
-include("rlx_log.hrl").

-spec do(atom(), string(), string() | undefined, rlx_state:t()) -> {ok, rlx_state:t()} | relx:error().
do(RelName, ToVsn, undefined, State) ->
    OutputDir = rlx_state:base_output_dir(State),
    ReleasesDir = filename:join([OutputDir, RelName, "releases"]),
    LastRelVsn = get_version_before(RelName, ToVsn, ReleasesDir),
    make_upfrom_script(RelName, ToVsn, LastRelVsn, State);
do(RelName, ToVsn, UpFromVsn, State) ->
    make_upfrom_script(RelName, ToVsn, UpFromVsn, State).

format_error({relfile_not_found, {Name, Vsn}}) ->
    io_lib:format("Release ~p-~s not found", [Name, Vsn]);
format_error({bad_rel_tuple, Release}) ->
    io_lib:format("Release format ~p not recognized. Should be of the form {Name, Vsn}.", [Release]);
format_error({relup_generation_error, CurrentName, UpFromName}) ->
    io_lib:format("Unknown internal release error generating the relup from ~s to ~s",
                  [UpFromName, CurrentName]);
format_error({no_upfrom_release_found, undefined}) ->
    io_lib:format("No earlier release for relup found", []);
format_error({no_upfrom_release_found, Vsn}) ->
    io_lib:format("Upfrom release version (~s) for relup not found", [Vsn]);
format_error({relup_script_generation_error, systools_relup, {missing_sasl, _}}) ->
    "Unfortunately, due to requirements in systools, you need to have the sasl application "
        "in both the current release and the release to upgrade from.";
format_error({relup_script_generation_error, Module, Error}) ->
    ["Error generating relup: \n", rlx_util:indent(2), Module:format_error(Error)].

%%

make_upfrom_script(Name, ToVsn, UpFromVsn, State) ->
    OutputDir = rlx_state:base_output_dir(State),
    OutDir = filename:join([OutputDir, atom_to_list(Name), "releases", ToVsn]),
    WarningsAsErrors = rlx_state:warnings_as_errors(State),
    Options = [{outdir, OutDir},
               {path, [filename:join([OutputDir, "*", "lib", "*", "ebin"])]},
               {silent, true} | case WarningsAsErrors of
                                    true -> [warnings_as_errors];
                                    false -> []
                                end],
    CurrentRel = strip_dot_rel(find_rel_file(Name, ToVsn, OutputDir)),
    UpFromRel = strip_dot_rel(find_rel_file(Name, UpFromVsn, OutputDir)),
    ?log_debug("systools:make_relup(~p, ~p, ~p, ~p)", [CurrentRel, UpFromRel, UpFromRel, Options]),
    case systools:make_relup(CurrentRel, [UpFromRel], [UpFromRel], [no_warn_sasl | Options]) of
        ok ->
            ?log_info("relup from ~s to ~s successfully created!", [UpFromRel, CurrentRel]),
            {ok, State};
        error ->
            erlang:error(?RLX_ERROR({relup_generation_error, CurrentRel, UpFromRel}));
        {ok, _RelUp, _, []} ->
            ?log_info("relup successfully created!"),
            {ok, State};
        {ok, _RelUp, Module, Warnings} ->
            ?log_warn("Warnings generating relup:~n~s", [[Module:format_warning(W) || W <- Warnings]]),
            {ok, State};

        {error, Module, Error} ->
            erlang:error(?RLX_ERROR({relup_script_generation_error, Module, Error}))
    end.

%% return path to rel file without the .rel extension as a string (not binary)
strip_dot_rel(Name) ->
    rlx_util:to_string(filename:join(filename:dirname(Name),
                                     filename:basename(Name, ".rel"))).

find_rel_file(Name, Vsn, Dir) when is_atom(Name) ,
                                   is_list(Vsn) ->
    RelFile = filename:join([Dir, atom_to_list(Name), "releases", Vsn, atom_to_list(Name) ++ ".rel"]),
    case filelib:is_regular(RelFile) of
        true ->
            RelFile;
        _ ->
            erlang:error(?RLX_ERROR({relfile_not_found, {Name, Vsn}}))
    end;
find_rel_file(Name, Vsn, _) ->
    erlang:error(?RLX_ERROR({bad_rel_tuple, {Name, Vsn}})).

get_version_before(Name, Vsn, ReleasesDir) ->
    %% Given directory where all releases for `Name' are find all versions of the release.
    %% Since the releases directory will have other files like `RELEASES', use the wildcard
    %% `*/Name.rel' to find all version directories and then trim down to just the version
    %% string with `filename:dirname/1'
    Vsns = [begin
                V = filename:dirname(R),
                {rlx_util:parse_vsn(V), V}
            end || R <- filelib:wildcard(filename:join("*", [Name, ".rel"]), ReleasesDir)],

    %% sort all the versions
    SortedVsns = lists:sort(fun vsn_lte/2, Vsns),

    %% take the last element of a list that has every element up to the `Vsn' we are building
    %% the relup for. This will be the most recent version of the same release found.
    case lists:reverse(lists:takewhile(fun({_, X}) -> X =/= Vsn end, SortedVsns)) of
        [{_, LastVersion} | _] ->
            LastVersion;
        _ ->
            erlang:error(?RLX_ERROR({no_upfrom_release_found, Vsn}))
    end.

vsn_lte({VsnA, _}, {VsnB, _}) ->
    rlx_util:parsed_vsn_lte(VsnA, VsnB).
