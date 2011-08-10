@setlocal

@rem Parse arguments. erlsrv.exe prepends erl arguments prior to first ++.
@rem Other args are position dependent.
@set args="%*"
@for /F "delims=++ tokens=1,2,3" %%I in (%args%) do @(
    @call :set_trim erl_args %%I
    @call :set_trim node_name %%J
    @call :set_trim node_root %%K
)

@set releases_dir=%node_root%\releases

@rem parse ERTS version and release version from start_erl.dat
@for /F "tokens=1,2" %%I in (%releases_dir%\start_erl.data) do @(
    @call :set_trim erts_version %%I
    @call :set_trim release_version %%J
)

@set erl_exe=%node_root%\erts-%erts_version%\bin\erl.exe
@set boot_file=%releases_dir%\%release_version%\%node_name%
@set app_config=%node_root%\etc\app.config
@set vm_args=%node_root%\etc\vm.args

@%erl_exe% %erl_args% -boot %boot_file% -config %app_config% -args_file %vm_args%

:set_trim
@set %1=%2
@goto :EOF
