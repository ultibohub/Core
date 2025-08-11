{
Ultibo Raspberry Pi 2 unit.

Copyright (C) 2024 - SoftOz Pty Ltd.

Arch
====

 ARMv7 (Cortex A7)

Boards
======

 Raspberry Pi 2 - Model B
 Raspberry Pi 3 - Model B/B+/A+

Licence
=======

 LGPLv2.1 with static linking exception (See COPYING.modifiedLGPL.txt)

Credits
=======

 Information for this unit was obtained from:


References
==========



Raspberry Pi 2
==============

 This unit has no functionality other than to include all units relevant to the Raspberry Pi 2.

 This includes standard interfaces such as network, filesystem and storage as well as drivers
 that are specific to the BCM2836 and are not included by anything else.

 Additional units can be included anywhere within a program and they will be linked during the
 compile process. This unit simply provides a convenient way to ensure all relevant units have
 been included.

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

{$IFNDEF FPC_DOTTEDUNITS}
unit RaspberryPi2;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  Core.GlobalConfig,
  Core.GlobalConst,
  Core.GlobalTypes,
  Platforms.BCM2836,
  Core.Platform,
  Core.Threads,
  Core.MMC,
  Drivers.BCM2709,
  Drivers.BCMSDHOST,
  Core.USB,
  Drivers.DWCOTG,
  Drivers.SMSC95XX,
  Drivers.LAN78XX,
  Drivers.USBStorage,
  Drivers.RPiGPIOExpander,
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
  BCM2836,
  Platform,
  Threads,
  MMC,
  BCM2709,
  BCMSDHOST,
  USB,
  DWCOTG,
  SMSC95XX,
  LAN78XX,
  USBStorage,
  RPiGPIOExpander,
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
procedure RaspberryPi2Init;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {RaspberryPi2 specific variables}
 RaspberryPi2Initialized:Boolean;

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure RaspberryPi2Init;
{Initialize the RaspberryPi2 unit and parameters}

{Note: Called only during system startup}
begin
 {}
 {Check Initialized}
 if RaspberryPi2Initialized then Exit;

 {Check SDHOST}
 {Note: SDHOST initialization moved to BCM2709Init}

 RaspberryPi2Initialized:=True;
end;

{==============================================================================}
{==============================================================================}

initialization
 RaspberryPi2Init;

{==============================================================================}

{finalization}
 {Nothing}

end.
