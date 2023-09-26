@echo off
setlocal enabledelayedexpansion

set "FLAGS="

for /f "delims=" %%a in (compile_flags.txt) do (
    set "FLAGS=!FLAGS! %%a"
)



echo =============== making the build directory =================
rd /s /q build
mkdir build
echo ================ compiling the source code =================
clang.exe !FLAGS! -O2 src\*.c src\utils\*.c -o build\compiler.exe
echo ============ copying the standard library files ============
xcopy /s /i std build\std
echo =============== finished building the project ===============
