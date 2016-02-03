{
Ultibo Raspberry Pi unit.

Copyright (C) 2016 - SoftOz Pty Ltd.

Arch
====

 ARMv6 (ARM1176)

Boards
======

 Raspberry Pi - Model A/B/A+/B+
 Raspberry Pi - Model Zero

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
 that are specific to the BCM2835 and ae not included by aything else.
 
 Additional units can be included anywhere within a program and they will be linked during the
 compile process. This unit simply provides a convenient wat to ensure all relevant units have
 been included.
 
}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString} 
{$inline on}   {Allow use of Inline procedures}

unit RaspberryPi;

interface

uses GlobalConfig,
     GlobalConst,
     GlobalTypes,
     BCM2835,
     Platform,
     Threads,
     MMC,
     BCM2708,
     USB,
     DWCOTG,
     SMSC95XX,
     Framebuffer,
     Console,
     Keyboard,
     Mouse,
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
{Nothing}
{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
 
end.

