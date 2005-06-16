@echo off
mkdir derived\bin 2> NUL
mkdir derived\lst 2> NUL
IF %1.==. GOTO NOOPT
cd src
IF %1.==pasmo. GOTO PASM
IF %1.==tniasm. GOTO TASM
:NOOPT
echo usage make.bat [pasmo or tniasm]
GOTO QUIT
:TASM
FOR %%i IN (main_msx1;main_msx2;main_msx2+;sub;music;disk;logo_msx1;logo_msx2;logo_msx2+) DO call ..\build %%i
GOTO END
:PASM
FOR %%i IN (main_msx1;main_msx2;main_msx2+;sub;music;disk;logo_msx1;logo_msx2;logo_msx2+) DO call ..\build %%i pasmo
:END
del *.tmp >> NUL
cd ..
:QUIT
