{
Ultibo Initialization code for Nexus 7 (2012)

Copyright (C) 2016 - Rob Judd <judd@ob-wan.com>

Arch
====

 ARMv7-A (Cortex A9)

Boards
======

 Nexus 7 (2012)

Licence
=======

 LGPLv2.1 with static linking exception (See COPYING.modifiedLGPL.txt)
 
Credits
=======

 Information for this unit was obtained from:

 ????
 
References
==========

 Linux Device Tree files in ...

 ????


Nexus 7 (2012)
==============

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

unit BootNexus7_2012;

interface

{==============================================================================}
{Global definitions} {Must be prior to uses}
{$INCLUDE GlobalDefines.inc}

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,PlatformNexus7_2012,PlatformARM,PlatformARMv7,Threads{$IFDEF CONSOLE_EARLY_INIT},Devices,Framebuffer,Console{$ENDIF}{$IFDEF LOGGING_EARLY_INIT},Logging{$ENDIF}; 

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

