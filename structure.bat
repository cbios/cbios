echo off
rem make base dirs
echo --------------------------
echo - OpenMSX dir structures -
echo --------------------------
echo - make config base dirs 
mkdir derived\configs\openMSX\share\machines
mkdir derived\configs\openMSX\share\machines\C-BIOS_MSX1
mkdir derived\configs\openMSX\share\machines\C-BIOS_MSX2
mkdir derived\configs\openMSX\share\machines\C-BIOS_MSX2+

rem make rom dirs
echo - make rom dirs
mkdir derived\configs\openMSX\share\machines\C-BIOS_MSX1\rom
mkdir derived\configs\openMSX\share\machines\C-BIOS_MSX2\rom
mkdir derived\configs\openMSX\share\machines\C-BIOS_MSX2+\rom

rem copy system files
echo - setting up msx1 structures
copy derived\bin\cbios_main_msx1.rom derived\configs\openMSX\share\machines\C-BIOS_MSX1\rom

echo - setting up msx2 structures
copy "derived\bin\cbios_main_msx2.rom"  "derived\configs\openMSX\share\machines\C-BIOS_MSX2\rom"
copy "derived\bin\cbios_disk.rom"       "derived\configs\openMSX\share\machines\C-BIOS_MSX2\rom"
copy "derived\bin\cbios_sub.rom"        "derived\configs\openMSX\share\machines\C-BIOS_MSX2\rom"
copy "derived\bin\cbios_music.rom"      "derived\configs\openMSX\share\machines\C-BIOS_MSX2+\rom"

echo - setting up msx2+ structures
copy "derived\bin\cbios_main_msx2+.rom" "derived\configs\openMSX\share\machines\C-BIOS_MSX2+\rom"
copy "derived\bin\cbios_disk.rom"       "derived\configs\openMSX\share\machines\C-BIOS_MSX2+\rom"
copy "derived\bin\cbios_sub.rom"        "derived\configs\openMSX\share\machines\C-BIOS_MSX2+\rom"
copy "derived\bin\cbios_music.rom"      "derived\configs\openMSX\share\machines\C-BIOS_MSX2+\rom"

echo - copy XML config files to the right dirs
copy "configs\openMSX\C-BIOS_MSX1\hardwareconfig.xml" "derived\configs\openMSX\share\machines\C-BIOS_MSX1"
copy "configs\openMSX\C-BIOS_MSX2\hardwareconfig.xml" "derived\configs\openMSX\share\machines\C-BIOS_MSX2"
copy "configs\openMSX\C-BIOS_MSX2+\hardwareconfig.xml" "derived\configs\openMSX\share\machines\C-BIOS_MSX2+"

pause