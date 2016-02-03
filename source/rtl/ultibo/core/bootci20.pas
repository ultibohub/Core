{
Ultibo Initialization code for MIPS Creator CI20.

Copyright (C) 2015 - SoftOz Pty Ltd.

Arch
====

 MIPS32

Boards
======

 Banana Pi

Licence
=======

 LGPLv2.1 with static linking exception (See COPYING.modifiedLGPL.txt)
 
Credits
=======

 Information for this unit was obtained from:

 ????
 
References
==========

 ????
 
MIPS Creator CI20
=================

 SoC: 
 
 CPU: 
 
 GPU: 
 
 RAM: 
 
 USB: 
 
 LAN: 
  
 SD/MMC: 
 
 WiFi: 
 
 Bluetooth: 

 Other: 
 
}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString} 
{$inline on}   {Allow use of Inline procedures}

unit BootCI20;

interface

{==============================================================================}
{Global definitions} {Must be prior to uses}
{$INCLUDE GlobalDefines.inc}

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,PlatformCI20,PlatformMIPS,PlatformMIPS32,Threads{$IFDEF CONSOLE_EARLY_INIT},Devices,Framebuffer,Console{$ENDIF}{$IFDEF LOGGING_EARLY_INIT},Logging{$ENDIF};  

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



