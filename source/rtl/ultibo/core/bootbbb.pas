{
Ultibo Initialization code for BeagleBone Black.

Copyright (C) 2015 - SoftOz Pty Ltd.

Arch
====

 ARMv7 (Cortex A8)

Boards
======

 BeagleBone Black

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

  am33xx.dtsi
  am335x-bone-common.dtsi
  am335x-boneblack.dts
  
 ????
 
BeagleBone Black
================

 SoC: 
 
 CPU: Cortex A8 (ARMv7) (1 @ 700MHz)
 
 GPU: 
 
 RAM:
 
 USB:  
 
 LAN: SMSC LAN8710
  
 SD/MMC: 
 
 WiFi: 
 
 Bluetooth: 

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString} 
{$inline on}   {Allow use of Inline procedures}

unit BootBBB;

interface

{==============================================================================}
{Global definitions} {Must be prior to uses}
{$INCLUDE GlobalDefines.inc}

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,PlatformBBB,PlatformARM,PlatformARMv7,Threads{$IFDEF CONSOLE_EARLY_INIT},Devices,Framebuffer,Console{$ENDIF}{$IFDEF LOGGING_EARLY_INIT},Logging{$ENDIF}; 

//To Do //See: \u-boot-HEAD-5745f8c\arch\arm\cpu\armv7\am33xx
//             BBB_SRM.pdf
//             \u-boot-HEAD-5745f8c\arch\arm\dts\am335x-boneblack.dts


//Note: //The BBB uses the SMSC LAN8710 Ethernet Controller
//      //See: \u-boot-HEAD-5745f8c\drivers\net\phy\smsc.c
//             \u-boot-HEAD-5745f8c\drivers\net\phy\phy.c

//See also: NetBSD arch\evbarm\beagle\beagle_start.S

//See: http://stackoverflow.com/questions/18653215/how-to-start-beaglebone-black-without-os

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
