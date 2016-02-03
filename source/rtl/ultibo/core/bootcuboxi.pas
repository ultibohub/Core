{
Ultibo Initialization code for Cubox-i.

Copyright (C) 2015 - SoftOz Pty Ltd.

Arch
====

 ARMv7 (Cortex A9)

Boards
======

 Cubox-i1
 Cubox-i2
 Cubox-i2Ex
 Cubox-i4Pro
 Cubox-i4x4
 Hummingboard
 
Licence
=======

 LGPLv2.1 with static linking exception (See COPYING.modifiedLGPL.txt)
 
Credits
=======

 Information for this unit was obtained from:

 ????
 
 
References
==========

 Linux Device Tree files in /arch/arm/boot/dts

  imx6dl.dtsi
  imx6dl-cubox-i.dts
  imx6dl-hummingboard.dts
  
  imx6qdl.dtsi
  imx6qdl-cubox-i.dtsi
  imx6qdl-hummingboard.dtsi
  imx6qdl-microsom.dtsi
  imx6qdl-microsom-ar8035.dtsi
  
  imx6q.dtsi
  imx6q-cubox-i.dts
  imx6q-hummingboard.dts
  
 Solid Run
 
  http://solid-run.com/freescale-imx6-family/cubox-i/cubox-i-specifications/
  

Cubox-i
=======

 SoC: Freescale i.MX6
 
 CPU: ARM Cortex A9  (ARMv7) (1, 2 or 4 @ ???)
 
 GPU: Vivante GC880 or GC2000
 
 RAM:
 
 USB: 4 x EHCI 
 
 LAN: Qualcomm / Atheros AR8035 
  
 SD/MMC: 
 
 WiFi: AzureWave AW-NH660 (BCM4330)
 
 Bluetooth: AzureWave AW-NH660 (BCM4330)
 

}

unit BootCuboxI;

interface

{==============================================================================}
{Global definitions} {Must be prior to uses}
{$INCLUDE GlobalDefines.inc}

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,PlatformCuboxI,PlatformARM,PlatformARMv7,Threads{$IFDEF CONSOLE_EARLY_INIT},Devices,Framebuffer,Console{$ENDIF}{$IFDEF LOGGING_EARLY_INIT},Logging{$ENDIF}; 

//To Do //See: \u-boot-HEAD-5745f8c\arch\arm\imx-common
//             \u-boot-HEAD-5745f8c\arch\arm\cpu\armv7\mx6

{==============================================================================}
{Boot Functions}

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
{Boot Functions}

{==============================================================================}
{==============================================================================}
 
end.

