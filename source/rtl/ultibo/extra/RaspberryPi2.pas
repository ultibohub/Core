{
Ultibo Raspberry Pi 2 unit.

Copyright (C) 2022 - SoftOz Pty Ltd.

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

unit RaspberryPi2;

interface

uses GlobalConfig,
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
var
 GPIOFirst:LongWord;
 GPIOLast:LongWord;
 ClockMaximum:LongWord;
begin
 {}
 {Check Initialized}
 if RaspberryPi2Initialized then Exit;

 {Check SDHOST}
 if BCM2709_REGISTER_SDHOST then
  begin
   {Set Parameters}
   {Check SDHCI Enabled}
   if BCM2709_REGISTER_SDHCI then
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
   if ClockMaximum = 0 then ClockMaximum:=BCM2709_SDHOST_MAX_FREQ;
   
   {Create Device}
   BCMSDHOSTCreate(BCM2836_SDHOST_REGS_BASE,BCM2709_SDHOST_DESCRIPTION,BCM2836_IRQ_SDHOST,DMA_DREQ_ID_SDHOST,BCM2709_SDHOST_MIN_FREQ,ClockMaximum,GPIOFirst,GPIOLast,GPIO_FUNCTION_ALT0,BCM2709SDHOST_FIQ_ENABLED);
  end;

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
