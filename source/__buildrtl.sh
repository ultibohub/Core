#!/bin/bash

export PATH=$HOME/ultibo/core/fpc/bin:$PATH

function exitFailure() {
    if [ $? -ne 0 ]; then
        echo "."
        echo "Build RTL failed, see above for errors"
        echo "."
        echo "=======================End of Build Script======================="
        echo "!!!EndOfBuild!!!"
        exit 1
    fi
}
echo "."
echo "======================Start of Build Script======================"

echo "."
echo "Building ARMv6 RTL"
echo "=================="
echo "."

make rtl_clean CROSSINSTALL=1 OS_TARGET=ultibo CPU_TARGET=arm SUBARCH=armv6 BINUTILSPREFIX=arm-none-eabi- FPCFPMAKE=$HOME/ultibo/core/fpc/bin/fpc CROSSOPT="-CpARMV6 -CfVFPV2 -CIARM -CaEABIHF -OoFASTMATH" FPC=$HOME/ultibo/core/fpc/bin/fpc
exitFailure
echo "!!!Progress7"

make rtl OS_TARGET=ultibo CPU_TARGET=arm SUBARCH=armv6 BINUTILSPREFIX=arm-none-eabi- FPCFPMAKE=$HOME/ultibo/core/fpc/bin/fpc CROSSOPT="-CpARMV6 -CfVFPV2 -CIARM -CaEABIHF -OoFASTMATH" FPC=$HOME/ultibo/core/fpc/bin/fpc
exitFailure
echo "!!!Progress14"

make rtl_install CROSSINSTALL=1 BINUTILSPREFIX=arm-none-eabi- FPCFPMAKE=$HOME/ultibo/core/fpc/bin/fpc CROSSOPT="-CpARMV6 -CfVFPV2 -CIARM -CaEABIHF -OoFASTMATH" OS_TARGET=ultibo CPU_TARGET=arm SUBARCH=armv6 FPC=$HOME/ultibo/core/fpc/bin/fpc INSTALL_PREFIX=$HOME/ultibo/core/fpc INSTALL_UNITDIR=$HOME/ultibo/core/fpc/units/armv6-ultibo/rtl
exitFailure
echo "!!!Progress21"


echo "."
echo "Building ARMv6 packages"
echo "======================="
echo "."

make rtl_clean CROSSINSTALL=1 OS_TARGET=ultibo CPU_TARGET=arm SUBARCH=armv6 BINUTILSPREFIX=arm-none-eabi- FPCFPMAKE=$HOME/ultibo/core/fpc/bin/fpc CROSSOPT="-CpARMV6 -CfVFPV2 -CIARM -CaEABIHF -OoFASTMATH" FPC=$HOME/ultibo/core/fpc/bin/fpc
exitFailure
echo "!!!Progress28"

make packages_clean CROSSINSTALL=1 OS_TARGET=ultibo CPU_TARGET=arm SUBARCH=armv6 BINUTILSPREFIX=arm-none-eabi- FPCFPMAKE=$HOME/ultibo/core/fpc/bin/fpc CROSSOPT="-CpARMV6 -CfVFPV2 -CIARM -CaEABIHF -OoFASTMATH" FPC=$HOME/ultibo/core/fpc/bin/fpc
exitFailure
echo "!!!Progress35"

make packages OS_TARGET=ultibo CPU_TARGET=arm SUBARCH=armv6 BINUTILSPREFIX=arm-none-eabi- FPCFPMAKE=$HOME/ultibo/core/fpc/bin/fpc CROSSOPT="-CpARMV6 -CfVFPV2 -CIARM -CaEABIHF -OoFASTMATH -Fu$HOME/ultibo/core/fpc/units/armv6-ultibo/rtl" FPC=$HOME/ultibo/core/fpc/bin/fpc
exitFailure
echo "!!!Progress42"

make packages_install CROSSINSTALL=1 BINUTILSPREFIX=arm-none-eabi- FPCFPMAKE=$HOME/ultibo/core/fpc/bin/fpc CROSSOPT="-CpARMV6 -CfVFPV2 -CIARM -CaEABIHF -OoFASTMATH" OS_TARGET=ultibo CPU_TARGET=arm SUBARCH=armv6 FPC=$HOME/ultibo/core/fpc/bin/fpc INSTALL_PREFIX=$HOME/ultibo/core/fpc INSTALL_UNITDIR=$HOME/ultibo/core/fpc/units/armv6-ultibo/packages
exitFailure
echo "!!!Progress49"


echo "."
echo "Building ARMv7 RTL"
echo "=================="
echo "."

make rtl_clean CROSSINSTALL=1 OS_TARGET=ultibo CPU_TARGET=arm SUBARCH=armv7a BINUTILSPREFIX=arm-none-eabi- FPCFPMAKE=$HOME/ultibo/core/fpc/bin/fpc CROSSOPT="-CpARMV7A -CfVFPV3 -CIARM -CaEABIHF -OoFASTMATH" FPC=$HOME/ultibo/core/fpc/bin/fpc
exitFailure
echo "!!!Progress56"

make rtl OS_TARGET=ultibo CPU_TARGET=arm SUBARCH=armv7a BINUTILSPREFIX=arm-none-eabi- FPCFPMAKE=$HOME/ultibo/core/fpc/bin/fpc CROSSOPT="-CpARMV7A -CfVFPV3 -CIARM -CaEABIHF -OoFASTMATH" FPC=$HOME/ultibo/core/fpc/bin/fpc
exitFailure
echo "!!!Progress63"

make rtl_install CROSSINSTALL=1 BINUTILSPREFIX=arm-none-eabi- FPCFPMAKE=$HOME/ultibo/core/fpc/bin/fpc CROSSOPT="-CpARMV7A -CfVFPV3 -CIARM -CaEABIHF -OoFASTMATH" OS_TARGET=ultibo CPU_TARGET=arm SUBARCH=armv7a FPC=$HOME/ultibo/core/fpc/bin/fpc INSTALL_PREFIX=$HOME/ultibo/core/fpc INSTALL_UNITDIR=$HOME/ultibo/core/fpc/units/armv7-ultibo/rtl
exitFailure
echo "!!!Progress70"


echo "."
echo "Building ARMv7 Packages"
echo "======================="
echo "."

make rtl_clean CROSSINSTALL=1 OS_TARGET=ultibo CPU_TARGET=arm SUBARCH=armv7a BINUTILSPREFIX=arm-none-eabi- FPCFPMAKE=$HOME/ultibo/core/fpc/bin/fpc CROSSOPT="-CpARMV7A -CfVFPV3 -CIARM -CaEABIHF -OoFASTMATH" FPC=$HOME/ultibo/core/fpc/bin/fpc
exitFailure
echo "!!!Progress77"

make packages_clean CROSSINSTALL=1 OS_TARGET=ultibo CPU_TARGET=arm SUBARCH=armv7a BINUTILSPREFIX=arm-none-eabi- FPCFPMAKE=$HOME/ultibo/core/fpc/bin/fpc CROSSOPT="-CpARMV7A -CfVFPV3 -CIARM -CaEABIHF -OoFASTMATH" FPC=$HOME/ultibo/core/fpc/bin/fpc
exitFailure
echo "!!!Progress84"

make packages OS_TARGET=ultibo CPU_TARGET=arm SUBARCH=armv7a BINUTILSPREFIX=arm-none-eabi- FPCFPMAKE=$HOME/ultibo/core/fpc/bin/fpc CROSSOPT="-CpARMV7A -CfVFPV3 -CIARM -CaEABIHF -OoFASTMATH -Fu$HOME/ultibo/core/fpc/units/armv7-ultibo/rtl" FPC=$HOME/ultibo/core/fpc/bin/fpc
exitFailure
echo "!!!Progress91"

make packages_install CROSSINSTALL=1 BINUTILSPREFIX=arm-none-eabi- FPCFPMAKE=$HOME/ultibo/core/fpc/bin/fpc CROSSOPT="-CpARMV7A -CfVFPV3 -CIARM -CaEABIHF -OoFASTMATH" OS_TARGET=ultibo CPU_TARGET=arm SUBARCH=armv7a FPC=$HOME/ultibo/core/fpc/bin/fpc INSTALL_PREFIX=$HOME/ultibo/core/fpc INSTALL_UNITDIR=$HOME/ultibo/core/fpc/units/armv7-ultibo/packages
exitFailure
echo "!!!Progress98"

echo "!!!Progress100"

echo "."
echo "Build RTL completed successfully"

echo "."
echo "=======================End of Build Script======================="
echo "!!!EndOfBuild!!!"

