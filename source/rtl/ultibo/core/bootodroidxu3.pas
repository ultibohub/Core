{
Ultibo Initialization code for Odroid-XU3.

Copyright (C) 2015 - SoftOz Pty Ltd.

Arch
====

 ARMv7 (Cortex A8)

Boards
======

 Odroid XU3

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


Odroid-XU3
==========

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

unit BootOdroidXU3;

interface

{==============================================================================}
{Global definitions} {Must be prior to uses}
{$INCLUDE GlobalDefines.inc}

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,PlatformOdroidXU3,PlatformARM,PlatformARMv7,Threads{$IFDEF CONSOLE_EARLY_INIT},Devices,Framebuffer,Console{$ENDIF}{$IFDEF LOGGING_EARLY_INIT},Logging{$ENDIF}; 

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

