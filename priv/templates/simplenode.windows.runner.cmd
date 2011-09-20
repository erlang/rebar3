@setlocal

@set node_name={{nodeid}}

@rem Get the abolute path to the parent directory,
@rem which is assumed to be the node root.
@for /F "delims=" %%I in ("%~dp0..") do @set node_root=%%~fI

@set releases_dir=%node_root%\releases

@rem Parse ERTS version and release version from start_erl.data
@for /F "tokens=1,2" %%I in (%releases_dir%\start_erl.data) do @(
    @call :set_trim erts_version %%I
    @call :set_trim release_version %%J
)

@set erts_bin=%node_root%\erts-%erts_version%\bin

@set service_name=%node_name%_%release_version%

@if "%1"=="install" @goto install
@if "%1"=="uninstall" @goto uninstall
@if "%1"=="start" @goto start
@if "%1"=="stop" @goto stop
@if "%1"=="restart" @call :stop && @goto start
@if "%1"=="console" @goto console
@rem TODO: attach, ping, restart and reboot

:usage
@echo Usage: %0 {install|uninstall|start|stop|restart|console}
@goto :EOF

:install
@%erts_bin%\erlsrv.exe add %service_name% -c "Erlang node %node_name% in %node_root%" -w %node_root% -m %node_root%\bin\start_erl.cmd -args " ++ %node_name% ++ %node_root%" -stopaction "init:stop()."
@goto :EOF

:uninstall
@%erts_bin%\erlsrv.exe remove %service_name%
@%erts_bin%\epmd.exe -kill
@goto :EOF

:start
@%erts_bin%\erlsrv.exe start %service_name%
@goto :EOF

:stop
@%erts_bin%\erlsrv.exe stop %service_name%
@goto :EOF

:console
@start %erts_bin%\werl.exe -boot %releases_dir%\%release_version%\%node_name%
@goto :EOF

:set_trim
@set %1=%2
@goto :EOF
