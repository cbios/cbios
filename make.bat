@echo off
mkdir derived\bin 2> NUL
mkdir derived\lst 2> NUL
cd src
FOR %%i IN (sub;main) DO call ..\build %%i
del *.tmp >> NUL
cd ..
