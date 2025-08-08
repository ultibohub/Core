{
Ultibo Extensible Host Controller Interface (XHCI) (USB 3.0) interface unit.

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


Extensible Host Controller Interface (XHCI)
===========================================


}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit XHCI;

interface

uses
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  Devices,
  PCI,
  USB;

//To Do //See: \u-boot-HEAD-5745f8c\drivers\usb\host
//To Do //Pi4

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
//const
 {XHCI specific constants}

//To Do //A generic XHCI unit


{==============================================================================}
//type
 {XHCI specific types}
//To Do

{==============================================================================}
var
 {XHCI specific variables}
 XHCIInitialized:Boolean;

 //To Do

{==============================================================================}
{Initialization Functions}
procedure XHCIInit;

{==============================================================================}
{XHCI Functions}
//To Do

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure XHCIInit;
begin
 {}
 {Check Initialized}
 if XHCIInitialized then Exit;

 //To Do

 XHCIInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{XHCI Functions}
//To Do

{==============================================================================}
{==============================================================================}

initialization
 XHCIInit;

{==============================================================================}

finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
