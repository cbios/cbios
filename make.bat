@echo off
mkdir derived\bin 2> NUL
mkdir derived\lst 2> NUL
cd src
IF %1.==pasmo. GOTO PASM
FOR %%i IN (sub;main) DO call ..\build %%i
GOTO END
:PASM
FOR %%i IN (sub;main) DO call ..\build %%i pasmo
:END
del *.tmp >> NUL
cd ..
