@echo off
mkdir derived\bin 2> NUL
mkdir derived\lst 2> NUL
cd src
IF %1.==pasmo. GOTO PASM
FOR %%i IN (main_msx1;main_msx2;main_msx2+;sub;music;disk) DO call ..\build %%i
GOTO END
:PASM
FOR %%i IN (main_msx1;main_msx2;main_msx2+;sub;music;disk) DO call ..\build %%i pasmo
:END
del *.tmp >> NUL
cd ..
