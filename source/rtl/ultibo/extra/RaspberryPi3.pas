{
Ultibo Raspberry Pi 3 unit.

Copyright (C) 2024 - SoftOz Pty Ltd.

Arch
====

 ARMv8 (Cortex A53)

Boards
======

 Raspberry Pi 3 - Model B/B+/A+

Licence
=======

 LGPLv2.1 with static linking exception (See COPYING.modifiedLGPL.txt)

Credits
=======

 Information for this unit was obtained from:


References
==========



Raspberry Pi 3
==============

 This unit has no functionality other than to include all units relevant to the Raspberry Pi 3.

 This includes standard interfaces such as network, filesystem and storage as well as drivers
 that are specific to the BCM2837 and are not included by anything else.

 Additional units can be included anywhere within a program and they will be linked during the
 compile process. This unit simply provides a convenient way to ensure all relevant units have
 been included.

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit RaspberryPi3;

interface

uses 
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  BCM2837,
  Platform,
  Threads,
  MMC,
  BCM2710,
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

{==============================================================================}
{Initialization Functions}
procedure RaspberryPi3Init;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {RaspberryPi3 specific variables}
 RaspberryPi3Initialized:Boolean;

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure RaspberryPi3Init;
{Initialize the RaspberryPi3 unit and parameters}

{Note: Called only during system startup}
begin
 {}
 {Check Initialized}
 if RaspberryPi3Initialized then Exit;

 {Check SDHOST}
 {Note: SDHOST initialization moved to BCM2710Init}

 RaspberryPi3Initialized:=True;
end;

{==============================================================================}
{==============================================================================}

initialization
 RaspberryPi3Init;

{==============================================================================}

{finalization}
 {Nothing}

end.
