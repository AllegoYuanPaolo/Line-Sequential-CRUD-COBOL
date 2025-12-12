@echo off

set "exec=%~n1.exe"

start "%exec%" cmd /c "%exec% & pause"