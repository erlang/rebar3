## copy this to ~/.config/fish/completions

function __fish_rebar3_needs_command
  set cmd (commandline -opc)
  if [ (count $cmd) -eq 1 -a $cmd[1] = 'rebar3' -o $cmd[1] = './rebar3' ]
    return 0
  end
  return 1
end

function __fish_rebar3_using_command
  set cmd (commandline -opc)
  if [ (count $cmd) -gt 1 ]
    if [ $argv[1] = $cmd[2] ]
      return 0
    end
  end
  return 1
end

## ➜ ~ rebar3 --help
## Rebar3 is a tool for working with Erlang projects.
##
##
## Usage: rebar [-h] [-v] [<task>]
##
##   -h, --help     Print this help.
##   -v, --version  Show version information.
##   <task>         Task to run.
##
##
## Several tasks are available:
##
## as                Higher order provider for running multiple tasks in a sequence as a certain profiles.
## clean             Remove compiled beam files from apps.
## compile           Compile apps .app.src and .erl files.
## cover             Perform coverage analysis.
## ct                Run Common Tests.
## deps              List dependencies
## dialyzer          Run the Dialyzer analyzer on the project.
## do                Higher order provider for running multiple tasks in a sequence.
## edoc              Generate documentation using edoc.
## escriptize        Generate escript archive.
## eunit             Run EUnit Tests.
## help              Display a list of tasks or help for a given task or subtask.
## new               Create new project from templates.
## path              Print paths to build dirs in current profile.
## pkgs              Get information on a given package.
## release           Build release of project.
## relup             Create relup of releases.
## report            Provide a crash report to be sent to the rebar3 issues page.
## shell             Run shell with project apps and deps in path.
## tar               Tar archive of release built of project.
## tree              Print dependency tree.
## unlock            Unlock dependencies.
## unstable          Namespace providing commands that are still in flux.
## update            Update package index.
## upgrade           Upgrade dependencies.
## version           Print version for rebar and current Erlang.
## xref              Run cross reference analysis.
##
## plugins <task>:
##   list           List local and global plugins for this project
##   upgrade        Upgrade plugins
##
## Run 'rebar3 help <TASK>' for details.
# general options
complete -f -c 'rebar3' -n 'not __fish_rebar3_needs_command' -l help -d 'Display the manual of a rebar3 command'


complete -f -c 'rebar3' -s h -l help        -d "Show the program options"
complete -f -c 'rebar3' -s v -l version     -d "Show version information"

## included tasks

complete -f -c 'rebar3' -n '__fish_rebar3_needs_command' -a as -d "Higher order task which takes a profile name and list of tasks to run under that profile."
## TODO: 'as' needs to inspect the rebar.config's profiles element

complete -f -c 'rebar3' -n '__fish_rebar3_needs_command' -a clean -d "Removes compiled beam files from apps."
complete -f -c 'rebar3' -n '__fish_rebar3_using_command clean' -s a -l all -d "Clean all apps, including the dependencies"

complete -f -c 'rebar3' -n '__fish_rebar3_needs_command' -a compile -d "Compile apps .app.src and .erl files."

complete -f -c 'rebar3' -n '__fish_rebar3_needs_command' -a cover -d "Perform coverage analysis."
complete -f -c 'rebar3' -n '__fish_rebar3_using_command cover' -s r -l reset -d "Resets all cover data"
complete -f -c 'rebar3' -n '__fish_rebar3_using_command cover' -s v -l verbose -d "Prints coverage analysis in the terminal."

## ct
complete -f -c 'rebar3' -n '__fish_rebar3_needs_command' -a ct -d "Run Common Tests."
complete -f -c 'rebar3' -n '__fish_rebar3_using_command ct' -l dir -d "Compile and run all test suites in the specified directories."
complete -f -c 'rebar3' -n '__fish_rebar3_using_command ct' -l suites -d "Compile and run all test suites specified. Must be specified by full path, either absolute or relative to the current directory."
complete -f -c 'rebar3' -n '__fish_rebar3_using_command ct' -l group -d "Test groups to run."
complete -f -c 'rebar3' -n '__fish_rebar3_using_command ct' -l label -d "Test label."
complete -f -c 'rebar3' -n '__fish_rebar3_using_command ct' -l config -d "Config files to use when running tests."
complete -f -c 'rebar3' -n '__fish_rebar3_using_command ct' -l allow_user_terms -d "Allow user defined terms in config files."
complete -f -c 'rebar3' -n '__fish_rebar3_using_command ct' -l logdir -d "The directory in which test logs will be written. Default: _build/test/logs"
complete -f -c 'rebar3' -n '__fish_rebar3_using_command ct' -l logopts -d "Options for common test logging."
complete -f -c 'rebar3' -n '__fish_rebar3_using_command ct' -l verbosity -d "Verbosity."
complete -f -c 'rebar3' -n '__fish_rebar3_using_command ct' -s c -l cover -d "Generate cover data."
complete -f -c 'rebar3' -n '__fish_rebar3_using_command ct' -l include -d "Include folders."
complete -f -c 'rebar3' -n '__fish_rebar3_using_command ct' -l repeat -d "How often to repeat tests."
complete -f -c 'rebar3' -n '__fish_rebar3_using_command ct' -l duration -d "Max runtime (format: HHMMSS)."
complete -f -c 'rebar3' -n '__fish_rebar3_using_command ct' -l until -d "Run until (format: HHMMSS)."
complete -f -c 'rebar3' -n '__fish_rebar3_using_command ct' -l force_stop -d "Force stop on test timeout."
complete -f -c 'rebar3' -n '__fish_rebar3_using_command ct' -l basic_html -d "Show basic HTML."
complete -f -c 'rebar3' -n '__fish_rebar3_using_command ct' -l stylesheet -d "CSS stylesheet to apply to html output."
complete -f -c 'rebar3' -n '__fish_rebar3_using_command ct' -l decrypt_key -d "Path to key for decrypting config."
complete -f -c 'rebar3' -n '__fish_rebar3_using_command ct' -l decrypt_file -d "Path to file containing key for decrypting config."
complete -f -c 'rebar3' -n '__fish_rebar3_using_command ct' -l abort_if_missing_suites -d "Abort if suites are missing."
complete -f -c 'rebar3' -n '__fish_rebar3_using_command ct' -l multiply_timetraps -d "Multiply timetraps."
complete -f -c 'rebar3' -n '__fish_rebar3_using_command ct' -l scale_timetraps -d "Scale timetraps."
complete -f -c 'rebar3' -n '__fish_rebar3_using_command ct' -l create_priv_dir -d "Create priv dir (auto_per_run | auto_per_tc | manual_per_tc)."
complete -f -c 'rebar3' -n '__fish_rebar3_using_command ct' -l include -d "Directories containing additional include files."
complete -f -c 'rebar3' -n '__fish_rebar3_using_command ct' -s v -l verbose -d "Enable verbose output. Default: false."
complete -f -c 'rebar3' -n '__fish_rebar3_using_command ct' -l auto_compile -d "Let common test compile test suites instead of rebar3."

complete -f -c 'rebar3' -n '__fish_rebar3_needs_command' -a deps -d "List dependencies"

complete -f -c 'rebar3' -n '__fish_rebar3_needs_command' -a dialyzer -d "Run the Dialyzer analyzer on the project."
complete -f -c 'rebar3' -n '__fish_rebar3_using_command dialyzer' -s u -l update-plt -d "Enable updating the PLT. Default: true"
complete -f -c 'rebar3' -n '__fish_rebar3_using_command dialyzer' -s s -l succ-typings -d "Enable success typing analysis. Default: true"

complete -f -c 'rebar3' -n '__fish_rebar3_needs_command' -a do -d "Higher order provider for running multiple tasks in a sequence."
complete -f -c 'rebar3' -n '__fish_rebar3_using_command do' -a 'compile, clean, ct, cover, deps, dialyzer, edoc, eunit, help, new, pkgs, release, relup, report, shell, tar, unlock, unstable, update, upgrade, version, xref,'
## TODO: do should understand plugins, but now it does not.

complete -f -c 'rebar3' -n '__fish_rebar3_needs_command' -a edoc -d "Generate documentation using edoc."
complete -f -c 'rebar3' -n '__fish_rebar3_needs_command' -a escriptize -d "Generate escript archive."

complete -f -c 'rebar3' -n '__fish_rebar3_needs_command' -a eunit -d "Run EUnit Tests."
complete -f -c 'rebar3' -n '__fish_rebar3_using_command eunit' -l app -d "Comma separated list of application test suites to run. Equivalent to `[{application, App}]`"
complete -f -c 'rebar3' -n '__fish_rebar3_using_command eunit' -l application -d "Comma separated list of application test suites to run. Equivalent to `[{application, App}]`"
complete -f -c 'rebar3' -n '__fish_rebar3_using_command eunit' -s c -l cover -d "Generate cover data"
complete -f -c 'rebar3' -n '__fish_rebar3_using_command eunit' -s d -l dir -d "Comma separated list of dirs to load tests from. Equivalent to `[{dir, Dir}]`"
complete -f -c 'rebar3' -n '__fish_rebar3_using_command eunut' -s e -l error_on_warning -d "Error on invalid test specifications instead of warning"
complete -f -c 'rebar3' -n '__fish_rebar3_using_command eunit' -s f -l file -d "Comma separated list of files to load tests from. Equivalent to `[{file, File}]`"
complete -f -c 'rebar3' -n '__fish_rebar3_using_command eunit' -s m -l module -d "Comma separated list of modules to load tests from. Equivalent to `[{module, Module}]`"
complete -f -c 'rebar3' -n '__fish_rebar3_using_command eunit' -s s -l suite -d "Comma separated list of modules to load tests from. Equivalent to `[{module, Module}]`"
complete -f -c 'rebar3' -n '__fish_rebar3_using_command eunit' -s g -l generator -d "Comma separated list of generators (the format is `module:function`) to load tests from. Equivalent to `[{generator, Module, Function}]`"
complete -f -c 'rebar3' -n '__fish_rebar3_using_command eunit' -s v -l verbose -d "Verbose output"

complete -f -c 'rebar3' -n '__fish_rebar3_using_command eunit' -l suite -d "Lists of test suites to run"

complete -f -c 'rebar3' -n '__fish_rebar3_needs_command' -a help -d "Display a list of tasks or help for a given task or subtask."

complete -f -c 'rebar3' -n '__fish_rebar3_needs_command' -a new -d "Create new project from templates."
complete -f -c 'rebar3' -n '__fish_rebar3_using_command new' -s f -l force -d "Overwrite existing files"
complete -f -c 'rebar3' -n '__fish_rebar3_using_command new' -a help -d "Display all variables and arguments for each template"

complete -f -c 'rebar3' -n '__fish_rebar3_needs_command' -a paths -d "Print paths to build dirs in current profile."
complete -f -c 'rebar3' -n '__fish_rebar3_needs_command paths' -l app -d "Comma separated list of applications to return paths for."
complete -f -c 'rebar3' -n '__fish_rebar3_needs_command paths' -l base -d "Return the `base` path of the current profile."
complete -f -c 'rebar3' -n '__fish_rebar3_needs_command paths' -l bin -d "Return the `bin` path of the current profile."
complete -f -c 'rebar3' -n '__fish_rebar3_needs_command paths' -l ebin -d "Return all `ebin` paths of the current profile`s applications."
complete -f -c 'rebar3' -n '__fish_rebar3_needs_command paths' -l lib -d "Return the `lib` path of the current profile."
complete -f -c 'rebar3' -n '__fish_rebar3_needs_command paths' -l priv -d "Return the `priv` path of the current profile`s applications."
complete -f -c 'rebar3' -n '__fish_rebar3_needs_command paths' -s s -l separator -d "In case of multiple return paths, the separator character to use to join them."
complete -f -c 'rebar3' -n '__fish_rebar3_needs_command paths' -l src -d "Return the `src` path of the current profile`s applications."
complete -f -c 'rebar3' -n '__fish_rebar3_needs_command paths' -l rel -d "Return the `rel` path of the current profile."

complete -f -c 'rebar3' -n '__fish_rebar3_needs_command' -a pkgs -d "List information for a hex package."
complete -f -c 'rebar3' -n '__fish_rebar3_needs_command' -a release -d "Build release of project."
complete -f -c 'rebar3' -n '__fish_rebar3_needs_command' -a relup -d "Create relup of releases."
complete -f -c 'rebar3' -n '__fish_rebar3_needs_command' -a report -d "Provide a crash report to be sent to the rebar3 issues page."

complete -f -c 'rebar3' -n '__fish_rebar3_needs_command' -a shell -d "Run shell with project apps and deps in path."
complete -f -c 'rebar3' -n '__fish_rebar3_using_command shell' -l config -d "Allows to load a config file, if any. Defaults to the sys_config entry defined for relx if present."
complete -f -c 'rebar3' -n '__fish_rebar3_using_command shell' -l name -d "equivalent to erlang`s -name"
complete -f -c 'rebar3' -n '__fish_rebar3_using_command shell' -l sname -d "equivalent to erlang`s -sname"

complete -f -c 'rebar3' -n '__fish_rebar3_needs_command' -a tar -d "Tar archive of release built of project."
complete -f -c 'rebar3' -n '__fish_rebar3_using_command tar' -s n -l relname -d "Specify the name for the release that will be generated"
complete -f -c 'rebar3' -n '__fish_rebar3_using_command tar' -s v -l relvsn        -d "Specify the version for the release"
complete -f -c 'rebar3' -n '__fish_rebar3_using_command tar' -s g -l goal          -d "Specify a target constraint on the system. These are usually the OTP"
complete -f -c 'rebar3' -n '__fish_rebar3_using_command tar' -s u -l upfrom        -d "Only valid with relup target, specify the release to upgrade from"
complete -f -c 'rebar3' -n '__fish_rebar3_using_command tar' -s o -l output-dir    -d "The output directory for the release. This is `./` by default."
complete -f -c 'rebar3' -n '__fish_rebar3_using_command tar' -s h -l help          -d "Print usage"
complete -f -c 'rebar3' -n '__fish_rebar3_using_command tar' -s l -l lib-dir       -d "Additional dir that should be searched for OTP Apps"
complete -f -c 'rebar3' -n '__fish_rebar3_using_command tar' -s p -l path          -d "Additional dir to add to the code path"
complete -f -c 'rebar3' -n '__fish_rebar3_using_command tar' -l default-libs       -d "Whether to use the default system added lib dirs (means you must add them all manually). Default is true"
complete -f -c 'rebar3' -n '__fish_rebar3_using_command tar' -s V -l verbose       -d "Verbosity level, maybe between 0 and 3 [default: 2]"
complete -f -c 'rebar3' -n '__fish_rebar3_using_command tar' -s d -l dev-mode      -d "Symlink the applications and configuration into the release instead of copying"
complete -f -c 'rebar3' -n '__fish_rebar3_using_command tar' -s i -l include-erts  -d "If true include a copy of erts used to build with, if a path include erts at that path. If false, do not include erts"
complete -f -c 'rebar3' -n '__fish_rebar3_using_command tar' -s a -l override      -d "Provide an app name and a directory to override in the form <appname>:<app directory>"
complete -f -c 'rebar3' -n '__fish_rebar3_using_command tar' -s c -l config        -d "The path to a config file [default: ]"
complete -f -c 'rebar3' -n '__fish_rebar3_using_command tar' -l overlay_vars       -d "Path to a file of overlay variables"
complete -f -c 'rebar3' -n '__fish_rebar3_using_command tar' -l vm_args            -d "Path to a file to use for vm.args"
complete -f -c 'rebar3' -n '__fish_rebar3_using_command tar' -l sys_config         -d "Path to a file to use for sys.config"
complete -f -c 'rebar3' -n '__fish_rebar3_using_command tar' -l system_libs        -d "Path to dir of Erlang system libs"
complete -f -c 'rebar3' -n '__fish_rebar3_using_command tar' -l version            -d "Print relx version"
complete -f -c 'rebar3' -n '__fish_rebar3_using_command tar' -s r -l root          -d "The project root directory"

complete -f -c 'rebar3' -n '__fish_rebar3_needs_command' -a tree -d "Print dependency tree."

complete -f -c 'rebar3' -n '__fish_rebar3_needs_command tree' -s v -l verbose  -d "Print repo and branch/tag/ref for git and hg deps."

complete -f -c 'rebar3' -n '__fish_rebar3_needs_command' -a unlock -d "Unlock dependencies."

complete -f -c 'rebar3' -n '__fish_rebar3_needs_command' -a unstable -d "Namespace providing commands that are still in flux."

complete -f -c 'rebar3' -n '__fish_rebar3_using_command unstable' -a 'install upgrade'

complete -f -c 'rebar3' -n '__fish_rebar3_needs_command' -a update -d "Update package index."

complete -f -c 'rebar3' -n '__fish_rebar3_needs_command' -a upgrade -d "Upgrade dependencies."

complete -f -c 'rebar3' -n '__fish_rebar3_needs_command' -a version -d "Print version for rebar and current Erlang."

complete -f -c 'rebar3' -n '__fish_rebar3_needs_command' -a xref -d "Run cross reference analysis."
