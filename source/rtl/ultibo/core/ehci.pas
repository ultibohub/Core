{
Ultibo Enhanced Host Controller Interface (EHCI) (USB) interface unit.

Copyright (C) 2021 - SoftOz Pty Ltd.

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


Enhanced Host Controller Interface (EHCI)
=========================================


}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

{$IFNDEF FPC_DOTTEDUNITS}
unit EHCI;
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
  Core.PCI,
  Core.USB;
{$ELSE FPC_DOTTEDUNITS}
uses
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  Devices,
  PCI,
  USB;
{$ENDIF FPC_DOTTEDUNITS}

//To Do //See: \u-boot-HEAD-5745f8c\drivers\usb\host

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
//const
 {EHCI specific constants}

//To Do //A generic EHCI unit


{==============================================================================}
//type
 {EHCI specific types}
//To Do

{==============================================================================}
var
 {EHCI specific variables}
 EHCIInitialized:Boolean;

 //To Do

{==============================================================================}
{Initialization Functions}
procedure EHCIInit;

{==============================================================================}
{EHCI Functions}
//To Do

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure EHCIInit;
begin
 {}
 {Check Initialized}
 if EHCIInitialized then Exit;

 //To Do

 EHCIInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{EHCI Functions}
//To Do

{==============================================================================}
{==============================================================================}

initialization
 EHCIInit;

{==============================================================================}

finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
