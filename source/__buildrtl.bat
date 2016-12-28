@echo off

rem Determine the Ultibo install path from the ULTIBO_DIR variable or use the default
IF "%ULTIBO_DIR%"=="" GOTO DefaultPath

set mypath=%ULTIBO_DIR%
set myfpc=%mypath%\bin\i386-win32
GOTO CheckPath

:DefaultPath
set mypath=C:\Ultibo\Core\fpc\3.1.1
set myfpc=%mypath%\bin\i386-win32

:CheckPath
IF not exist %mypath%\nul.x GOTO PathError
IF not exist %myfpc%\fpc.exe GOTO FPCError

set path=%mypath%\bin\i386-win32

echo .
echo ======================Start of Build Script======================

echo .
echo Building ARMv6 RTL
echo ==================
echo .

make rtl_clean CROSSINSTALL=1 OS_TARGET=ultibo CPU_TARGET=arm SUBARCH=armv6 FPCFPMAKE=%myfpc%/fpc.exe CROSSOPT="-CpARMV6 -CfVFPV2 -CIARM -CaEABIHF -OoFASTMATH" FPC=%myfpc%/fpc.exe

IF %errorlevel% NEQ 0 GOTO Error
echo .

make rtl OS_TARGET=ultibo CPU_TARGET=arm SUBARCH=armv6 FPCFPMAKE=%myfpc%/fpc.exe CROSSOPT="-CpARMV6 -CfVFPV2 -CIARM -CaEABIHF -OoFASTMATH" FPC=%myfpc%/fpc.exe

IF %errorlevel% NEQ 0 GOTO Error
echo .

make rtl_install CROSSINSTALL=1 FPCFPMAKE=%myfpc%/fpc.exe CROSSOPT="-CpARMV6 -CfVFPV2 -CIARM -CaEABIHF -OoFASTMATH" OS_TARGET=ultibo CPU_TARGET=arm SUBARCH=armv6 FPC=%myfpc%/fpc.exe INSTALL_BASEDIR=%mypath% INSTALL_UNITDIR=%mypath%/units/armv6-ultibo/rtl

IF %errorlevel% NEQ 0 GOTO Error
echo .


echo .
echo Building ARMv6 packages
echo =======================
echo .

make rtl_clean CROSSINSTALL=1 OS_TARGET=ultibo CPU_TARGET=arm SUBARCH=armv6 FPCFPMAKE=%myfpc%/fpc.exe CROSSOPT="-CpARMV6 -CfVFPV2 -CIARM -CaEABIHF -OoFASTMATH" FPC=%myfpc%/fpc.exe

IF %errorlevel% NEQ 0 GOTO Error
echo .

make packages_clean CROSSINSTALL=1 OS_TARGET=ultibo CPU_TARGET=arm SUBARCH=armv6 FPCFPMAKE=%myfpc%/fpc.exe CROSSOPT="-CpARMV6 -CfVFPV2 -CIARM -CaEABIHF -OoFASTMATH" FPC=%myfpc%/fpc.exe

IF %errorlevel% NEQ 0 GOTO Error
echo .

make packages OS_TARGET=ultibo CPU_TARGET=arm SUBARCH=armv6 FPCFPMAKE=%myfpc%/fpc.exe CROSSOPT="-CpARMV6 -CfVFPV2 -CIARM -CaEABIHF -OoFASTMATH -Fu%mypath%\units\armv6-ultibo\rtl" FPC=%myfpc%/fpc.exe

IF %errorlevel% NEQ 0 GOTO Error
echo .

make packages_install CROSSINSTALL=1 FPCFPMAKE=%myfpc%/fpc.exe CROSSOPT="-CpARMV6 -CfVFPV2 -CIARM -CaEABIHF -OoFASTMATH" OS_TARGET=ultibo CPU_TARGET=arm SUBARCH=armv6 FPC=%myfpc%/fpc.exe INSTALL_BASEDIR=%mypath% INSTALL_UNITDIR=%mypath%/units/armv6-ultibo/packages

IF %errorlevel% NEQ 0 GOTO Error
echo .


echo .
echo Building ARMv7 RTL
echo ==================
echo .

make rtl_clean CROSSINSTALL=1 OS_TARGET=ultibo CPU_TARGET=arm SUBARCH=armv7a FPCFPMAKE=%myfpc%/fpc.exe CROSSOPT="-CpARMV7A -CfVFPV3 -CIARM -CaEABIHF -OoFASTMATH" FPC=%myfpc%/fpc.exe

IF %errorlevel% NEQ 0 GOTO Error
echo .

make rtl OS_TARGET=ultibo CPU_TARGET=arm SUBARCH=armv7a FPCFPMAKE=%myfpc%/fpc.exe CROSSOPT="-CpARMV7A -CfVFPV3 -CIARM -CaEABIHF -OoFASTMATH" FPC=%myfpc%/fpc.exe

IF %errorlevel% NEQ 0 GOTO Error
echo .

make rtl_install CROSSINSTALL=1 FPCFPMAKE=%myfpc%/fpc.exe CROSSOPT="-CpARMV7A -CfVFPV3 -CIARM -CaEABIHF -OoFASTMATH" OS_TARGET=ultibo CPU_TARGET=arm SUBARCH=armv7a FPC=%myfpc%/fpc.exe INSTALL_BASEDIR=%mypath% INSTALL_UNITDIR=%mypath%/units/armv7-ultibo/rtl

IF %errorlevel% NEQ 0 GOTO Error
echo .


echo .
echo Building ARMv7 Packages
echo =======================
echo .

make rtl_clean CROSSINSTALL=1 OS_TARGET=ultibo CPU_TARGET=arm SUBARCH=armv7a FPCFPMAKE=%myfpc%/fpc.exe CROSSOPT="-CpARMV7A -CfVFPV3 -CIARM -CaEABIHF -OoFASTMATH" FPC=%myfpc%/fpc.exe

IF %errorlevel% NEQ 0 GOTO Error
echo .

make packages_clean CROSSINSTALL=1 OS_TARGET=ultibo CPU_TARGET=arm SUBARCH=armv7a FPCFPMAKE=%myfpc%/fpc.exe CROSSOPT="-CpARMV7A -CfVFPV3 -CIARM -CaEABIHF -OoFASTMATH" FPC=%myfpc%/fpc.exe

IF %errorlevel% NEQ 0 GOTO Error
echo .

make packages OS_TARGET=ultibo CPU_TARGET=arm SUBARCH=armv7a FPCFPMAKE=%myfpc%/fpc.exe CROSSOPT="-CpARMV7A -CfVFPV3 -CIARM -CaEABIHF -OoFASTMATH -Fu%mypath%\units\armv7-ultibo\rtl" FPC=%myfpc%/fpc.exe

IF %errorlevel% NEQ 0 GOTO Error
echo .

make packages_install CROSSINSTALL=1 FPCFPMAKE=%myfpc%/fpc.exe CROSSOPT="-CpARMV7A -CfVFPV3 -CIARM -CaEABIHF -OoFASTMATH" OS_TARGET=ultibo CPU_TARGET=arm SUBARCH=armv7a FPC=%myfpc%/fpc.exe INSTALL_BASEDIR=%mypath% INSTALL_UNITDIR=%mypath%/units/armv7-ultibo/packages

IF %errorlevel% NEQ 0 GOTO Error
echo .


echo .
echo Building ARMv8 RTL
echo ==================
echo .

make rtl_clean CROSSINSTALL=1 OS_TARGET=ultibo CPU_TARGET=aarch64 SUBARCH=armv8 FPCFPMAKE=%myfpc%/fpc.exe CROSSOPT="-CpARMV8 -CfVFP -OoFASTMATH" FPC=%myfpc%/fpc.exe

IF %errorlevel% NEQ 0 GOTO Error
echo .

make rtl OS_TARGET=ultibo CPU_TARGET=aarch64 SUBARCH=armv8 FPCFPMAKE=%myfpc%/fpc.exe CROSSOPT="-CpARMV8 -CfVFP -OoFASTMATH" FPC=%myfpc%/fpc.exe

IF %errorlevel% NEQ 0 GOTO Error
echo .

make rtl_install CROSSINSTALL=1 FPCFPMAKE=%myfpc%/fpc.exe CROSSOPT="-CpARMV8 -CfVFP -OoFASTMATH" OS_TARGET=ultibo CPU_TARGET=aarch64 SUBARCH=armv8 FPC=%myfpc%/fpc.exe INSTALL_BASEDIR=%mypath% INSTALL_UNITDIR=%mypath%/units/armv8-ultibo/rtl

IF %errorlevel% NEQ 0 GOTO Error
echo .


echo .
echo Building ARMv8 Packages
echo =======================
echo .

make rtl_clean CROSSINSTALL=1 OS_TARGET=ultibo CPU_TARGET=aarch64 SUBARCH=armv8 FPCFPMAKE=%myfpc%/fpc.exe CROSSOPT="-CpARMV8 -CfVFP -OoFASTMATH" FPC=%myfpc%/fpc.exe

IF %errorlevel% NEQ 0 GOTO Error
echo .

make packages_clean CROSSINSTALL=1 OS_TARGET=ultibo CPU_TARGET=aarch64 SUBARCH=armv8 FPCFPMAKE=%myfpc%/fpc.exe CROSSOPT="-CpARMV8 -CfVFP -OoFASTMATH" FPC=%myfpc%/fpc.exe

IF %errorlevel% NEQ 0 GOTO Error
echo .

make packages OS_TARGET=ultibo CPU_TARGET=aarch64 SUBARCH=armv8 FPCFPMAKE=%myfpc%/fpc.exe CROSSOPT="-CpARMV8 -CfVFP -OoFASTMATH -Fu%mypath%\units\armv8-ultibo\rtl" FPC=%myfpc%/fpc.exe

IF %errorlevel% NEQ 0 GOTO Error
echo .

make packages_install CROSSINSTALL=1 FPCFPMAKE=%myfpc%/fpc.exe CROSSOPT="-CpARMV8 -CfVFP -OoFASTMATH" OS_TARGET=ultibo CPU_TARGET=aarch64 SUBARCH=armv8 FPC=%myfpc%/fpc.exe INSTALL_BASEDIR=%mypath% INSTALL_UNITDIR=%mypath%/units/armv8-ultibo/packages

IF %errorlevel% NEQ 0 GOTO Error
echo .


echo .
echo Build RTL completed successfully
GOTO End

:PathError
echo .
echo Error cannot find path "%mypath%"
GOTO Error

:FPCError
echo .
echo Error cannot find FPC compiler at "%myfpc%\fpc.exe"
GOTO Error

:Error
echo .
echo Build RTL failed, see above for errors

:End
echo .
echo =======================End of Build Script=======================
echo !!!EndOfBuild!!!

