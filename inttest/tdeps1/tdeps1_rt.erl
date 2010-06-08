-module(tdeps1_rt).

-compile(export_all).

%% Exercise transitive dependencies
%% A -> B -> C, where A includes a .hrl from B which includes .hrl from C

files() ->
    [
     %% A application
     {create, "ebin/a.app", app(a)},
     {copy, "a.rebar.config", "rebar.config"},
     {copy, "a.erl", "src/a.erl"},
     {copy, "../../rebar", "rebar"},

     %% B application
     {create, "repo/b/ebin/b.app", app(b)},
     {copy, "b.rebar.config", "repo/b/rebar.config"},
     {copy, "b.hrl", "repo/b/include/b.hrl"},

     %% C application
     {create, "repo/c/ebin/c.app", app(c)},
     {copy, "c.hrl", "repo/c/include/c.hrl"}
    ].

run(_Dir) ->
    %% Initialize the b/c apps as mercurial repos so that dependencies pull
    %% properly
    HgCmd = "hg init && hg add && hg commit -m 'Initial commit'",
    retest_log:log(debug, "~s\n", [os:cmd("(cd repo/b && " ++ HgCmd ++ ")")]),
    retest_log:log(debug, "~s\n", [os:cmd("(cd repo/c && " ++ HgCmd ++ ")")]),

    retest_log:log(debug, "~s\n", [os:cmd("./rebar get-deps compile")]),
    ok.



%%
%% Generate the contents of a simple .app file
%%
app(Name) ->
    App = {application, Name,
           [{description, atom_to_list(Name)},
            {vsn, "1"},
            {modules, []},
            {registered, []},
            {applications, [kernel, stdlib]}]},
    io_lib:format("~p.\n", [App]).
