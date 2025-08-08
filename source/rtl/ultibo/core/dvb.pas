{
Ultibo Digital Video Broadcast (DVB) interface unit.

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


Digital Video Broadcast (DVB)
=============================

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit DVB;

interface

uses
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  Devices,
  Audio,
  Video,
  USB;

//To Do

 //Useful references:

 //   \linux-rpi-3.12.y\drivers\media\dvb-core
 //   \linux-rpi-3.12.y\drivers\media\dvb-frontends

 //USB
 //
 //   \linux-rpi-3.12.y\drivers\media\usb\dvb-usb
 //   \linux-rpi-3.12.y\drivers\media\usb\dvb-usb-v2
 //
 //Drivers
 //
 //   \linux-rpi-3.12.y\drivers\media\usb\dvb-usb-v2\af9015.c
 //   \linux-rpi-3.12.y\drivers\media\usb\dvb-usb-v2\af9035.c
 //   \linux-rpi-3.12.y\drivers\media\usb\dvb-usb-v2\rtl28xxu.c

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
//const
 {DVB specific constants}

//To Do //A generic DVB unit


{==============================================================================}
//type
 {DVB specific types}
//To Do

{==============================================================================}
var
 {DVB specific variables}
 DVBInitialized:Boolean;

 //To Do

{==============================================================================}
{Initialization Functions}
procedure DVBInit;

{==============================================================================}
{DVB Functions}
//To Do

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure DVBInit;
begin
 {}
 {Check Initialized}
 if DVBInitialized then Exit;

 //To Do

 DVBInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{DVB Functions}
//To Do

{==============================================================================}
{==============================================================================}

initialization
 DVBInit;

{==============================================================================}

finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.







