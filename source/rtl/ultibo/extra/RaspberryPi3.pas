{
Ultibo Raspberry Pi 3 unit.

Copyright (C) 2020 - SoftOz Pty Ltd.

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

uses GlobalConfig,
     GlobalConst,
     GlobalTypes,
     BCM2837,
     Platform,
     Threads,
     MMC,
     BCM2710,
     USB,
     DWCOTG,
     SMSC95XX,
     LAN78XX,
     RPiGPIOExpander,
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

