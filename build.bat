@echo off
IF %1.==. GOTO NOOPT
IF %2.==pasmo. GOTO PASM
tniasm %1.asm ..\derived\bin\cbios_%1.rom
if errorlevel 1 goto error
move tniasm.sym ..\derived\lst\cbios_%1.sym
goto no_error
:NOOPT
echo Error! build.bat should calls from make.bat,please run make.bat
GOTO no_error
:PASM
echo Assembling %1.asm
pasmo -d %1.asm ..\derived\bin\cbios_%1.rom > ..\derived\lst\cbios_%1.sym
if errorlevel 1 goto error
goto no_error
:error
pause
:no_error
