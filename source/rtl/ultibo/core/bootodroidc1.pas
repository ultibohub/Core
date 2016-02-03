{
Ultibo Initialization code for Odroid-C1.

Copyright (C) 2015 - SoftOz Pty Ltd.

Arch
====

 ARMv7 (Cortex A8)

Boards
======

 Odroid C1

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

 ????


Odroid-C1
=========

 SoC: 
 
 CPU:  
 
 GPU: 
 
 RAM:
 
 USB:  
 
 LAN: 
  
 SD/MMC: 
 
 WiFi: 
 
 Bluetooth: 

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString} 
{$inline on}   {Allow use of Inline procedures}

unit BootOdroidC1;

interface

{==============================================================================}
{Global definitions} {Must be prior to uses}
{$INCLUDE GlobalDefines.inc}

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,PlatformOdroidC1,PlatformARM,PlatformARMv7,Threads{$IFDEF CONSOLE_EARLY_INIT},Devices,Framebuffer,Console{$ENDIF}{$IFDEF LOGGING_EARLY_INIT},Logging{$ENDIF}; 

{==============================================================================}
{Boot Functions}
//To Do

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
{Boot Functions}

{==============================================================================}
{==============================================================================}
 
end.

