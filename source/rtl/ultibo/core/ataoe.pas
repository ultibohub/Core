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

{$IFNDEF FPC_DOTTEDUNITS}
unit ATAoE;
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
  Core.Storage,
  Core.Network;
{$ELSE FPC_DOTTEDUNITS}
uses
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  Devices,
  Storage,
  Network;
{$ENDIF FPC_DOTTEDUNITS}

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
