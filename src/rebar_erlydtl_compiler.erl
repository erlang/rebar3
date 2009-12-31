%% -*- tab-width: 4;erlang-indent-level: 4;indent-tabs-mode: nil -*-
%% ex: ts=4 sw=4 et
%% -------------------------------------------------------------------
%%
%% rebar: Erlang Build Tools
%%
%% Copyright (c) 2009 Dave Smith (dizzyd@dizzyd.com),
%%                    Bryan Fink (bryan@basho.com)
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

%% The rebar_erlydtl_compiler module is a plugin for rebar that compiles
%% ErlyDTL templates.  By default, it compiles all templates/*.dtl
%% to ebin/*_dtl.beam.
%%
%% Configuration options should be placed in rebar.config under
%% 'erlydtl_opts'.  Available options include:
%%
%%  doc_root: where to find templates to compile
%%            "templates" by default
%%
%%  out_dir: where to put compiled template beam files
%%           "ebin" by default
%%
%%  source_ext: the file extension the template sources have
%%              ".dtl" by default
%%
%%  module_ext: characters to append to the template's module name
%%              "_dtl" by default
%%
%% For example, if you had:
%%   /t_src/
%%          base.html
%%          foo.html
%%
%% And you wanted them compiled to:
%%   /priv/
%%         base.beam
%%         foo.beam
%%
%% You would add to your rebar.config:
%%   {erlydtl_opts, [
%%               {doc_root,   "t_src"},
%%               {out_dir,    "priv"},
%%               {source_ext, ".html"},
%%               {module_ext, ""}
%%              ]}.
%%
%% The default settings are the equivalent of:
%%   {erlydtl_opts, [
%%               {doc_root,   "templates"},
%%               {out_dir,    "ebin"},
%%               {source_ext, ".dtl"},
%%               {module_ext, "_dtl"}
%%              ]}.
-module(rebar_erlydtl_compiler).

-export([compile/2]).

-include("rebar.hrl").

%% ===================================================================
%% Public API
%% ===================================================================

compile(Config, _AppFile) ->
    DtlOpts = erlydtl_opts(Config),
    rebar_erlc_compiler:do_compile(
      Config,
      filename:join([option(doc_root, DtlOpts),
                     ["*",option(source_ext, DtlOpts)]]),
      option(out_dir, DtlOpts),
      option(source_ext, DtlOpts),
      option(module_ext, DtlOpts)++".beam",
      fun referenced_dtls/2, fun compile_dtl/2, []).


%% ===================================================================
%% Internal functions
%% ===================================================================

erlydtl_opts(Config) ->
    rebar_config:get(Config, erlydtl_opts, []).

option(Opt, DtlOpts) ->
    proplists:get_value(Opt, DtlOpts, default(Opt)).

default(doc_root) -> "templates";
default(out_dir)  -> "ebin";
default(source_ext) -> ".dtl";
default(module_ext) -> "_dtl".

referenced_dtls(Source, Config) ->
    Set = referenced_dtls([Source], Config,
                          sets:add_element(Source, sets:new())),
    Final = sets:to_list(sets:del_element(Source, Set)),
    Final.

referenced_dtls(Step, Config, Seen) ->
    DtlOpts = erlydtl_opts(Config),
    ExtMatch = re:replace(option(source_ext, DtlOpts), "\.", "\\\\.",
                          [{return, list}]),
    AllRefs = lists:append(
                [ string:tokens(
                    os:cmd(["grep -o [^\\\"]*",ExtMatch," ",F]),
                    "\n")
                  || F <- Step]),
    DocRoot = option(doc_root, DtlOpts),
    WithPaths = [ filename:join([DocRoot, F]) || F <- AllRefs ],
    Existing = lists:filter(fun filelib:is_file/1, WithPaths),
    New = sets:subtract(sets:from_list(Existing), Seen),
    case sets:size(New) of
        0 -> Seen;
        _ -> referenced_dtls(sets:to_list(New), Config,
                             sets:union(New, Seen))
    end.

compile_dtl(Source, Config) ->
    case code:which(erlydtl) of
        non_existing ->
            ?CONSOLE(
               "~n===============================================~n"
               " You need to install erlydtl to comple DTL templates~n"
               " Download the latest tarball release from github~n"
               "    http://code.google.com/p/erlydtl/~n"
               " and install it into your erlang library dir~n"
               "===============================================~n~n", []),
            ?FAIL;
        _ ->
            DtlOpts = erlydtl_opts(Config),
            %% ensure that doc_root and out_dir are defined,
            %% using defaults if necessary
            Opts = [{out_dir, option(out_dir, DtlOpts)},
                    {doc_root, option(doc_root, DtlOpts)},
                    report, return],
            case erlydtl:compile(Source,
                                 module_name(Source,DtlOpts),
                                 Opts++DtlOpts) of
                ok -> ok;
                Reason ->
                    ?CONSOLE("Compiling template ~s failed:~n  ~p~n",
                             [Source, Reason]),
                    ?FAIL
            end
    end.

module_name(DtlPath, DtlOpts) ->
    F = filename:basename(DtlPath),
    SourceExt = option(source_ext, DtlOpts),
    ModuleExt = option(module_ext, DtlOpts),
    list_to_atom(lists:sublist(F, length(F)-length(SourceExt))
                 ++ModuleExt).
