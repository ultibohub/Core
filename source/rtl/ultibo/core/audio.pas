{
Ultibo Audio interface unit.

Copyright (C) 2015 - SoftOz Pty Ltd.

Arch
====

 <All>

Boards
======

 <All>

Licence
=======

 LGPLv2.1 with static linking exception (See COPYING.modifiedLGPL.txt)
 
Credits
=======

 Information for this unit was obtained from:

 
References
==========


Audio Devices
=============

This unit provides both the Audio device interface and the generic USB audio device driver.

USB Audio Devices
=================

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit Audio;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,SysUtils;
     //To Do //Add PCM/I2S, PWM, MPEG etc

//To Do //This unit should also include the generic USB Audio driver ?
     

//To Do //See also: \source\packages\a52\src\a52.pas
//To Do //See also: \source\packages\fcl-sound\src
     
{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
//const
 {Audio specific constants}
//To Do

{==============================================================================}
//type
 {Audio specific types}
//To Do

{==============================================================================}
var
 {Audio specific variables}
 AudioInitialized:Boolean;

 //To Do
 
{==============================================================================}
{Initialization Functions}
procedure AudioInit;

{==============================================================================}
{Audio Functions}
//To Do

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure AudioInit;
begin
 {}
 {Check Initialized}
 if AudioInitialized then Exit;
 
 //To Do
 
 AudioInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{Audio Functions}
//To Do

{==============================================================================}
{==============================================================================}

initialization
 AudioInit;

{==============================================================================}
 
finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
