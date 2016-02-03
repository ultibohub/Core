{
Ultibo Broadcom VideoCoreIV interface unit.

Copyright (C) 2015 - SoftOz Pty Ltd.

Arch
====

 ARMv6 (ARM1176)
 ARMv7 (Cortex A8)

Boards
======

 Raspberry Pi - Model A/B/A+/B+
 Raspberry Pi 2 - Model B

Licence
=======

 LGPLv2.1 with static linking exception (See COPYING.modifiedLGPL.txt)
 
Credits
=======

 Information for this unit was obtained from:

 
 
References
==========

 http://elinux.org/Raspberry_Pi_VideoCore_APIs

VideoCore IV
============

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

//See:  \linux-rpi-3.12.y\drivers\misc\vc04_services\interface\vchi
//      \linux-rpi-3.12.y\drivers\misc\vc04_services\interface\vchiq_arm


//See: http://elinux.org/Raspberry_Pi_VideoCore_APIs

unit VC4;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,Audio,Video;

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
//const
 {VC4 specific constants}

//To Do //A generic VC4 unit 
                           
              
{==============================================================================}
//type
 {VC4 specific types}
//To Do

{==============================================================================}
var
 {VC4 specific variables}
 VC4Initialized:Boolean;

 //To Do
 
{==============================================================================}
{Initialization Functions}
procedure VC4Init;

{==============================================================================}
{VC4 Functions}
//To Do

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure VC4Init;
begin
 {}
 {Check Initialized}
 if VC4Initialized then Exit;
 
 //To Do
 
 VC4Initialized:=True;
end;

{==============================================================================}
{==============================================================================}
{VC4 Functions}
//To Do

{==============================================================================}
{==============================================================================}

initialization
 VC4Init;

{==============================================================================}
 
finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.

