@rem Do not use "echo off" to not affect any child calls.
@setlocal

@set node_name={{nodeid}}

@rem Get the abolute path to the parent directory, which is assumed to be the node root.
@for /F "delims=" %%I in ("%~dp0..") do @set node_root=%%~fI

@set releases_dir=%node_root%\releases

@rem parse ERTS version and release version from start_erl.dat
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
@rem @if "%1"=="attach" @goto attach
@rem TODO: ping, restart and reboot?

:usage
@echo Usage: %0 {install|uninstall|start|stop|restart}
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

@rem this relies on a system install of Erlang to be on the PATH.
@rem also, node-naming issues make this difficult to automate
@rem :attach
@rem @werl.exe -remsh %node_name%@localhost -setcookie %COOKIE% -sname console
@rem @goto quit

:set_trim
@set %1=%2
@goto :EOF
