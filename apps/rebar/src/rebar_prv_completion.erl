%% @doc Generates shell completion files based on available providers and their opts.
%% @end
-module(rebar_prv_completion).

-behaviour(provider).

-export([init/1,
         do/1,
         format_error/1]).

-include_lib("providers/include/providers.hrl").
-include("rebar.hrl").

-define(PROVIDER, completion).
-define(DEPS, [app_discovery]).
-define(DEF_SHELL, bash).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    AliasesHelp = "Comma separated list of OS level aliases on which rebar3 completion will be triggered (e.g. \"rebar\" or \"r3\").",
    AliasesOpt = {aliases, $a, "aliases", string, AliasesHelp},

    FileHelp = "Completion file name. Relative to \"_build/\".",
    FileOpt = {file, $f, "file", string, FileHelp},

    ShellHelp = "Shell type, 'bash' or 'zsh'.",
    ShellOpt = {shell, $s, "shell", atom, ShellHelp},

    Provider = providers:create([{name, ?PROVIDER},
                                 {module, ?MODULE},
                                 {bare, true},
                                 {deps, ?DEPS},
                                 {example, "rebar3 completion"},
                                 {short_desc, "Generate completion file for your shell."},
                                 {desc, "Generate completion file for your shell."},
                                 {opts, [AliasesOpt, FileOpt, ShellOpt]}]),
    State1 = rebar_state:add_provider(State,Provider),
    {ok, State1}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    DefaultOpts = #{aliases => [],
                    file => "_rebar3",
                    shell => detect_shell()},
    {CliOptsList, _} = rebar_state:command_parsed_args(State),
    CliOpts0 = maps:from_list(CliOptsList),
    CliOpts = process_cli_opts(CliOpts0),
    Conf = maps:from_list(rebar_state:get(State, completion, [])),
    %% Opts passed in CLI override config
    CmplOpts0 = maps:merge(DefaultOpts, Conf),
    CmplOpts = maps:merge(CmplOpts0, CliOpts),

    Providers0 = rebar_state:providers(State),
    BareProviders = lists:filter(fun(P) -> provider_get(P, bare) end, Providers0),
    ByNamespace = maps:groups_from_list(fun(P) -> provider_get(P, namespace) end, BareProviders),
    Cmds0 = maps:fold(
                fun(NS,Ps,CmdAcc) -> namespace_to_cmpl_cmds(NS, Ps)++CmdAcc end,
                [],
                ByNamespace),
    Cmds1 = [oracle(Cmd, CmplOpts, State) || Cmd <- Cmds0],
    {[Do],RestCmds} = lists:partition(
                         fun(Cmd) -> maps:get(name, Cmd) =:= "do" end,
                         Cmds1),
    Cmds = [Do#{cmds:=RestCmds} | RestCmds],
    Compl = rebar_completion:generate(Cmds, CmplOpts),
    write_completion(Compl,State,CmplOpts),
    {ok, State}.

detect_shell() ->
    case os:getenv("SHELL") of
        false ->
            ?DIAGNOSTIC("SHELL variable not set, default shell will be used.",
                        []),
            ?DEF_SHELL;
        Path  ->
            to_shell(filename:basename(Path))
    end.

to_shell("bash") -> bash;
to_shell("zsh") -> zsh;
to_shell(Unsupp) ->
        ?WARN("Unsupported shell found: ~p, default shell will be used.",
                    [Unsupp]),
        ?DEF_SHELL.

process_cli_opts(#{aliases:=AStr}=Cli) ->
    As = [string:trim(A) || A <- string:split(AStr, ",", all)],
    Cli#{aliases:=As};
process_cli_opts(Cli) ->
    Cli.

-spec namespace_to_cmpl_cmds(atom(), [providers:t()]) -> [rebar_completion:cmpl_cmd()].
namespace_to_cmpl_cmds(default,Providers) ->
    lists:map(fun(P)->provider_to_cmpl_cmd(P) end,Providers);
namespace_to_cmpl_cmds(Namespace,Providers) ->
    Name = atom_to_list(Namespace),
    [#{name=>Name,
      cmds=>lists:map(fun(P)->provider_to_cmpl_cmd(P) end, Providers),
      args=>[],
      help=>Name++" namespace"}].

-spec provider_to_cmpl_cmd(providers:t()) -> rebar_completion:cmpl_cmd().
provider_to_cmpl_cmd(Provider) ->
    Opts = providers:opts(Provider),
    Name = providers:impl(Provider),
    Cmd = getopt_to_cmpl_cmd(atom_to_list(Name),Opts),
    Help = provider_get(Provider, short_desc),
    Cmd#{help=>Help}.

-spec getopt_to_cmpl_cmd(string(), [tuple()]) -> rebar_completion:cmpl_cmd().
getopt_to_cmpl_cmd(Name, Opts) ->
    Args = [#{short=>S,
            long=>L,
            type=>cmpl_arg_type(Spec),
            help=>H} || {_,S,L,Spec,H} <- Opts],
    #{name => Name,
    args => Args,
    cmds => [],
    help => undefined}.

cmpl_arg_type({Type,_Default}) ->
    Type;
cmpl_arg_type(Type) ->
    Type.

%% ad-hoc injection of data for some known providers!
-spec oracle(rebar_completion:cmpl_cmd(),
             rebar_completion:cmpl_opts(),
             rebar_state:t()) -> rebar_completion:cmpl_cmd().
oracle(#{name:="as"}=Cmd, _CmplOpts, State) ->
    %% profile completion
    ConfigProfiles = rebar_opts:get(rebar_state:opts(State), profiles, []),
    Cmds = [#{name=>atom_to_list(ProfileName),
              help=>"",
              cmds=>[],
              args=>[]} || {ProfileName,_} <- ConfigProfiles],
    Cmd#{cmds=>Cmds};
oracle(Cmd,_,_) ->
    Cmd.

-spec write_completion(iolist(), rebar_state:t(), rebar_completion:cmpl_opts()) -> ok.
write_completion(CompletionStr, State, #{shell:=Shell, file:=Filename}) ->
    BaseDir = rebar_dir:base_dir(State),
    Dest = filename:join(BaseDir, Filename),
    case filelib:ensure_dir(Dest) of
        ok ->
            ?DEBUG("Writing completion file for ~p shell to: ~p~n",
                        [Shell, Dest]),
            case file:write_file(Dest, CompletionStr, [write, raw]) of
                ok ->
                    ok;
                {error,Err} ->
                    throw(?PRV_ERROR({error_writing_file,Dest,Err}))
            end;
        {error,Err} ->
            throw(?PRV_ERROR({error_creating_dir,filename:dirname(Dest),Err}))
    end.


%% for some reason providers don't expose some of their attributes via API
provider_get(P, bare) ->
    element(5, P);
provider_get(P, short_desc) ->
    element(8, P);
provider_get(P, namespace) ->
    element(12, P).

-spec format_error(any()) ->  iolist().
format_error({error_writing_file,File,Err}) ->
    io_lib:format("Error occurred when trying to write into ~p file.~nReason: ~p~n", [File,Err]);
format_error({error_creating_dir,Dir,Err}) ->
    io_lib:format("Error occurred when trying to create dir: ~p.~nReason: ~p~n", [Dir,Err]).
