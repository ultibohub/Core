{
Ultibo VPN interface unit.

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


VPN
===

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

{$IFNDEF FPC_DOTTEDUNITS}
unit VPN;
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
  System.SysUtils;
{$ELSE FPC_DOTTEDUNITS}
uses
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  Devices,
  SysUtils;
{$ENDIF FPC_DOTTEDUNITS}

//To Do //A generic VPN client //This will most likely be some form of NetworkAdapter module (See Network / Loopback etc)

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
//const
 {VPN specific constants}

{==============================================================================}
//type
 {VPN specific types}

{==============================================================================}
var
 {VPN specific variables}
 VPNInitialized:Boolean;

{==============================================================================}
{Initialization Functions}
procedure VPNInit;

{==============================================================================}
{VPN Functions}

{==============================================================================}
{VPN Helper Functions}

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure VPNInit;
begin
 {}
 {Check Initialized}
 if VPNInitialized then Exit;

 //To Do

 VPNInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{VPN Functions}

{==============================================================================}
{==============================================================================}
{VPN Helper Functions}

{==============================================================================}
{==============================================================================}

initialization
 VPNInit;

{==============================================================================}

finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
