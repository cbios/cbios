tniasm %1.asm ..\derived\bin\cbios_%1.rom
if errorlevel 1 goto error
move tniasm.sym ..\derived\sym\cbios_%1.sym
goto no_error
:error
pause
:no_error
