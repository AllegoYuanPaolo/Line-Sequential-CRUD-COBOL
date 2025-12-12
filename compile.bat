@echo off

setlocal
set "file=%~n1.cbl"
set "exeName=%~1"


cobc -x %file%
if errorlevel 1 (
    echo ^> Compilation failed.
    exit /b 1
) else (
    echo ^> Compilation succeeded.
    start cmd /c ".\%exeName%.exe & pause"
)


