{
Ultibo Initialization code for pcDuino.

Copyright (C) 2015 - SoftOz Pty Ltd.

Arch
====

 ARMv7 (Cortex A7)

Boards
======

 pcDuino1/2/3

Licence
=======

 LGPLv2.1 with static linking exception (See COPYING.modifiedLGPL.txt)
 
Credits
=======

 Information for this unit was obtained from:

 ????
 
References
==========

 Allwinner A20 Datasheet V1.41 20131230.pdf
 
 Allwinner A20 user manual v1.3 20141010.pdf
 
 Linux Device Tree files in /arch/arm/boot/dts

 ????
 
pcDuino
=======

 SoC: Allwinner A20
 
 CPU: ARM Cortex A7 (ARMv7) (2 @ 1GHz)
 
 GPU: Mali400 MP2
 
 RAM: ??
 
 USB: 2 x EHCI/OHCI
 
 LAN: ??
  
 SD/MMC: 
 
 WiFi: 
 
 Bluetooth: 

 Other: AHCI / RTC / VIC
 
}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString} 
{$inline on}   {Allow use of Inline procedures}

unit BootpcDuino;

interface

{==============================================================================}
{Global definitions} {Must be prior to uses}
{$INCLUDE GlobalDefines.inc}

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,PlatformpcDuino,PlatformARM,PlatformARMv7,Threads{$IFDEF CONSOLE_EARLY_INIT},Devices,Framebuffer,Console{$ENDIF}{$IFDEF LOGGING_EARLY_INIT},Logging{$ENDIF};  

//To Do 

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
