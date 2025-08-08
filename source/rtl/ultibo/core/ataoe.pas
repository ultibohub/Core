{
Ultibo ATA Over Ethernet (ATAoE) interface unit.

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


ATA Over Ethernet (ATAoE)
=========================

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit ATAoE;

interface

uses
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  Devices,
  Storage,
  Network;

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
//const
 {ATAoE specific constants}

//To Do //A generic ATAoE unit


{==============================================================================}
//type
 {ATAoE specific types}
//To Do

{==============================================================================}
var
 {ATAoE specific variables}
 ATAoEInitialized:Boolean;

 //To Do

{==============================================================================}
{Initialization Functions}
procedure ATAoEInit;

{==============================================================================}
{ATAoE Functions}
//To Do

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure ATAoEInit;
begin
 {}
 {Check Initialized}
 if ATAoEInitialized then Exit;

 //To Do

 ATAoEInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{ATAoE Functions}
//To Do

{==============================================================================}
{==============================================================================}

initialization
 ATAoEInit;

{==============================================================================}

finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
