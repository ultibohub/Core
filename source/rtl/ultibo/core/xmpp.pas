{
Ultibo XMPP interface unit.

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


XMPP
====

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

{$IFNDEF FPC_DOTTEDUNITS}
unit XMPP;
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

//To Do //A generic XMPP interface (Client and Server)

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
//const
 {XMPP specific constants}

{==============================================================================}
//type
 {XMPP specific types}

{==============================================================================}
var
 {XMPP specific variables}
 XMPPInitialized:Boolean;

{==============================================================================}
{Initialization Functions}
procedure XMPPInit;

{==============================================================================}
{XMPP Functions}

{==============================================================================}
{XMPP Helper Functions}

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure XMPPInit;
begin
 {}
 {Check Initialized}
 if XMPPInitialized then Exit;

 //To Do

 XMPPInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{XMPP Functions}

{==============================================================================}
{==============================================================================}
{XMPP Helper Functions}

{==============================================================================}
{==============================================================================}

initialization
 XMPPInit;

{==============================================================================}

finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.



