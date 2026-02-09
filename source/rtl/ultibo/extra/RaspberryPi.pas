{
Ultibo Raspberry Pi unit.

Copyright (C) 2024 - SoftOz Pty Ltd.

Arch
====

 ARMv6 (ARM1176)

Boards
======

 Raspberry Pi - Model A/B/A+/B+
 Raspberry Pi - Model Zero/ZeroW

Licence
=======

 LGPLv2.1 with static linking exception (See COPYING.modifiedLGPL.txt)

Credits
=======

 Information for this unit was obtained from:


References
==========



Raspberry Pi
============

 This unit has no functionality other than to include all units relevant to the Raspberry Pi.

 This includes standard interfaces such as network, filesystem and storage as well as drivers
 that are specific to the BCM2835 and are not included by anything else.

 Additional units can be included anywhere within a program and they will be linked during the
 compile process. This unit simply provides a convenient way to ensure all relevant units have
 been included.

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

{$IFNDEF FPC_DOTTEDUNITS}
unit RaspberryPi;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  Core.GlobalConfig,
  Core.GlobalConst,
  Core.GlobalTypes,
  Platforms.BCM2835,
  Core.Platform,
  Core.Threads,
  Core.MMC,
  Drivers.BCM2708,
  Drivers.BCMSDHOST,
  Core.USB,
  Drivers.DWCOTG,
  Drivers.SMSC95XX,
  Drivers.USBStorage,
  Core.Framebuffer,
  Core.Console,
  Core.Keyboard,
  Core.Mouse,
  Core.HID,
  Core.USBHID,
  Drivers.HIDKeyboard,
  Drivers.HIDMouse,
  Core.FileSystem,
  Core.EXTFS,
  Core.FATFS,
  Core.NTFS,
  Core.CDFS,
  Core.VirtualDisk,
  Core.Logging,
  Core.Sockets,
  Core.Winsock2,
  Core.Services,
  System.SysUtils;
{$ELSE FPC_DOTTEDUNITS}
uses
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  BCM2835,
  Platform,
  Threads,
  MMC,
  BCM2708,
  BCMSDHOST,
  USB,
  DWCOTG,
  SMSC95XX,
  USBStorage,
  Framebuffer,
  Console,
  Keyboard,
  Mouse,
  HID,
  USBHID,
  HIDKeyboard,
  HIDMouse,
  Filesystem,
  EXTFS,
  FATFS,
  NTFS,
  CDFS,
  VirtualDisk,
  Logging,
  Sockets,
  Winsock2,
  Services,
  SysUtils;
{$ENDIF FPC_DOTTEDUNITS}

{==============================================================================}
{Initialization Functions}
procedure RaspberryPiInit;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {RaspberryPi specific variables}
 RaspberryPiInitialized:Boolean;

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure RaspberryPiInit;
{Initialize the RaspberryPi unit and parameters}

{Note: Called only during system startup}
begin
 {}
 {Check Initialized}
 if RaspberryPiInitialized then Exit;

 {Check SDHOST}
 {Note: SDHOST initialization moved to BCM2708Init}

 RaspberryPiInitialized:=True;
end;

{==============================================================================}
{==============================================================================}

initialization
 RaspberryPiInit;

{==============================================================================}

{finalization}
 {Nothing}

end.
