@echo off
mode 50
echo Setting up openMSX configs
mkdir derived\configs\openMSX\share\machines 2>nul
xcopy configs\openMSX\*.* derived\configs\openMSX\share\machines /s /e /y >nul
del derived\configs\openMSX\share\machines\README.TXT

echo Setting up blueMSX configs
mkdir derived\configs\blueMSX\Machines 2>nul
xcopy configs\blueMSX\*.* derived\configs\blueMSX\Machines /s /e /y >nul
del derived\configs\blueMSX\Machines\README.TXT

echo Setting up NLMSX configs
mkdir derived\configs\NLMSX\ROMS 2>nul
xcopy configs\NLMSX\*.* derived\configs\NLMSX /s /e /y >nul
del derived\configs\NLMSX\README.TXT

echo Setting up RuMSX configs
mkdir derived\configs\RuMSX\SYSTEM 2>nul
xcopy configs\RuMSX\*.* derived\configs\RuMSX /s /e /y >nul
del derived\configs\RuMSX\README.TXT

echo ------------------------
echo Copying system files
echo ------------------------
echo - openMSX
copy "derived\bin\cbios_logo_msx1.rom" "derived\configs\openMSX\share\machines\C-BIOS_MSX1\roms" /y >nul
copy "derived\bin\cbios_logo_msx2.rom" "derived\configs\openMSX\share\machines\C-BIOS_MSX2\roms" /y >nul
copy "derived\bin\cbios_main_msx1.rom" "derived\configs\openMSX\share\machines\C-BIOS_MSX1\roms" /y >nul
copy "derived\bin\cbios_main_msx2.rom" "derived\configs\openMSX\share\machines\C-BIOS_MSX2\roms" /y >nul
copy "derived\bin\cbios_sub.rom" "derived\configs\openMSX\share\machines\C-BIOS_MSX2\roms" /y >nul

rem copy derived\bin\cbios_disk.rom derived\configs\openMSX\share\machines\C-BIOS_MSX2\roms /y >nul
copy "derived\bin\cbios_logo_msx2+.rom" "derived\configs\openMSX\share\machines\C-BIOS_MSX2+\roms" /y >nul
copy "derived\bin\cbios_main_msx2+.rom" "derived\configs\openMSX\share\machines\C-BIOS_MSX2+\roms" /y >nul
copy "derived\bin\cbios_sub.rom" "derived\configs\openMSX\share\machines\C-BIOS_MSX2+\roms" /y >nul
copy "derived\bin\cbios_music.rom" "derived\configs\openMSX\share\machines\C-BIOS_MSX2+\roms" /y >nul

echo - BlueMSX
rem copy derived\bin\cbios_disk.rom "derived\configs\openMSX\share\machines\C-BIOS_MSX2+\roms" /y >nul
copy "derived\bin\cbios_logo_msx1.rom" "derived\configs\blueMSX\Machines\MSX - C-BIOS" /y >nul
copy "derived\bin\cbios_logo_msx2.rom" "derived\configs\blueMSX\Machines\MSX2 - C-BIOS" /y >nul
copy "derived\bin\cbios_main_msx1.rom" "derived\configs\blueMSX\Machines\MSX - C-BIOS" /y >nul
copy "derived\bin\cbios_main_msx2.rom" "derived\configs\blueMSX\Machines\MSX2 - C-BIOS" /y >nul
copy "derived\bin\cbios_sub.rom" "derived\configs\blueMSX\Machines\MSX2 - C-BIOS" /y >nul
rem copy derived\bin\cbios_disk.rom "derived\configs\blueMSX\Machines\MSX2 - C-BIOS" /y >nul
copy "derived\bin\cbios_logo_msx2+.rom" "derived\configs\blueMSX\Machines\MSX2+ - C-BIOS" /y >nul
copy "derived\bin\cbios_main_msx2+.rom" "derived\configs\blueMSX\Machines\MSX2+ - C-BIOS" /y >nul
copy "derived\bin\cbios_sub.rom" "derived\configs\blueMSX\Machines\MSX2+ - C-BIOS" /y >nul
copy "derived\bin\cbios_music.rom" "derived\configs\blueMSX\Machines\MSX2+ - C-BIOS" /y >nul

echo - NLMSX
rem copy derived\bin\cbios_disk.rom "derived\configs\blueMSX\Machines\MSX2+ - C-BIOS" /y >nul
copy "derived\bin\cbios_*.rom" "derived\configs\NLMSX\ROMS" /y >nul

echo - RuMSX
copy "derived\bin\cbios_*.rom" "derived\configs\RuMSX\SYSTEM" /y >nul

echo Done...
pause
