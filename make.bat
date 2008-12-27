@echo off
mkdir derived\bin 2> NUL
mkdir derived\lst 2> NUL
IF %1.==. GOTO NOOPT

rem -- TODO: get the CVS version of Changelog into version.asm
mkdir derived\asm 2> NUL
del derived\asm\version.asm 2> NUL
echo db "CBIOS v0.22         cbios.sf.net" > derived\asm\version.asm

cd src
IF %1.==pasmo. GOTO PASM
IF %1.==tniasm. GOTO TASM
:NOOPT
echo usage make.bat [pasmo or tniasm]
GOTO QUIT
:TASM
FOR %%i IN (main_msx1;main_msx2;main_msx2+) DO call ..\build %%i
FOR %%i IN (basic;sub;music;disk;logo_msx1;logo_msx2;logo_msx2+) DO call ..\build %%i
FOR %%i IN (main_msx1_jp;main_msx2_jp;main_msx2+_jp) DO call ..\build %%i
GOTO DEL_TEMP
:PASM
FOR %%i IN (main_msx1;main_msx2;main_msx2+) DO call ..\build %%i pasmo
FOR %%i IN (basic;sub;music;disk;logo_msx1;logo_msx2;logo_msx2+) DO call ..\build %%i pasmo
FOR %%i IN (main_msx1_jp;main_msx2_jp;main_msx2+_jp) DO call ..\build %%i pasmo
GOTO END
:DEL_TEMP
del *.tmp >> NUL
:END
cd ..
:QUIT
