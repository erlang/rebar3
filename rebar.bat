@echo off
setlocal
set rebarscript=%0
escript.exe %rebarscript:.bat=% %*
