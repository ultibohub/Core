{
Ultibo Bluetooth interface unit.

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


Bluetooth Devices
=================

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

{$IFNDEF FPC_DOTTEDUNITS}
unit Bluetooth;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  Core.GlobalConfig,
  Core.GlobalConst,
  Core.GlobalTypes,
  Core.Platform,
  Core.Threads,
  Core.Devices,
  Core.USB,
  Core.Keyboard,
  Core.Mouse,
  Core.Storage,
  Core.Network,
  Core.Audio,
  Core.Video,
  Core.Serial;
{$ELSE FPC_DOTTEDUNITS}
uses
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  Devices,
  USB,
  Keyboard,
  Mouse,
  Storage,
  Network,
  Audio,
  Video,
  Serial;
{$ENDIF FPC_DOTTEDUNITS}

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
//const
 {Bluetooth specific constants}
//To Do

{==============================================================================}
//type
 {Bluetooth specific types}
//To Do

{==============================================================================}
var
 {Bluetooth specific variables}
 BluetoothInitialized:Boolean;

 //To Do

{==============================================================================}
{Initialization Functions}
procedure BluetoothInit;

{==============================================================================}
{Bluetooth Functions}
//To Do

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure BluetoothInit;
begin
 {}
 {Check Initialized}
 if BluetoothInitialized then Exit;

 //To Do

 BluetoothInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{Bluetooth Functions}
//To Do

{==============================================================================}
{==============================================================================}

initialization
 BluetoothInit;

{==============================================================================}

finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
