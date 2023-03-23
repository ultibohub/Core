{
Ultibo Raspberry Pi unit.

Copyright (C) 2023 - SoftOz Pty Ltd.

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
     BCMSDHOST,
     USB,
     DWCOTG,
     SMSC95XX,
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
var
 GPIOFirst:LongWord;
 GPIOLast:LongWord;
 ClockMaximum:LongWord;
begin
 {}
 {Check Initialized}
 if RaspberryPiInitialized then Exit;

 {Initialize BCM2708SDHOST_FIQ_ENABLED}
 if not(FIQ_ENABLED) then BCM2708SDHOST_FIQ_ENABLED:=False;

 {Check SDHOST}
 if BCM2708_REGISTER_SDHOST then
  begin
   {Set Parameters}
   {Check SDHCI Enabled}
   if BCM2708_REGISTER_SDHCI then
    begin
     {Use GPIO 22 to 27}
     GPIOFirst:=GPIO_PIN_22; 
     GPIOLast:=GPIO_PIN_27;
    end
   else
    begin
     {Use GPIO 48 to 53}
     GPIOFirst:=GPIO_PIN_48; 
     GPIOLast:=GPIO_PIN_53;
    end;
   {Get Clock Maximum}
   ClockMaximum:=ClockGetRate(CLOCK_ID_MMC1);
   if ClockMaximum = 0 then ClockMaximum:=BCM2708_SDHOST_MAX_FREQ;
   
   {Create Device}
   BCMSDHOSTCreate(BCM2835_SDHOST_REGS_BASE,BCM2708_SDHOST_DESCRIPTION,BCM2835_IRQ_SDHOST,DMA_DREQ_ID_SDHOST,BCM2708_SDHOST_MIN_FREQ,ClockMaximum,GPIOFirst,GPIOLast,GPIO_FUNCTION_ALT0,BCM2708SDHOST_FIQ_ENABLED);
  end;

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
