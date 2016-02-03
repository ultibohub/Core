{
Ultibo BCM2709 interface unit.

Copyright (C) 2015 - SoftOz Pty Ltd.

Arch
====

 <All>

Boards
======

 Raspberry Pi 2 - Model B

Licence
=======

 LGPLv2.1 with static linking exception (See COPYING.modifiedLGPL.txt)
 
Credits
=======

 Information for this unit was obtained from:

   Linux MMC/SDHCI drivers
   U-Boot MMC/SDHCI drivers
 
References
==========

 BCM2835 ARM Peripherals

 Raspberry Pi Mailboxes
 
  https://github.com/raspberrypi/firmware/wiki/Mailboxes
 
 RPi Low-level peripherals
  
  http://elinux.org/RPi_Low-level_peripherals
  
 RPi SPI
 
  http://elinux.org/RPi_SPI
 
BCM2709 Devices
===============
 
 This unit provides the BCM2709 specific implementations of the following devices:

  SPI
  I2C
  DMA
  PWM
  PCM
  GPIO
  SDHCI (eMMC)
 
  Clock
  Timer
  Random
  Mailbox
  Watchdog
  Framebuffer
  
  And MIPI CSI-2 (Camera Serial Interface) ?
  And DSI (Display Serial Interface) ?
 
 
BCM2709 SPI Device
==================


BCM2709 I2C Device
==================

 
BCM2709 DMA Device
==================


BCM2709 PWM Device
==================


BCM2709 PCM Device
==================

 
BCM2709 GPIO Device
===================


BCM2709 SDHCI Device
====================

 The SDHCI controller on the BCM2709 is an Arasan SD Host controller.

 The Card Detect pin is not connected.

 The Write Protect pin is not connected.


BCM2709 Clock Device
====================


BCM2709 Timer Device
====================


BCM2709 Random Device
=====================


BCM2709 Mailbox Device
======================


BCM2709 Watchdog Device
=======================


BCM2709 Framebuffer Device
==========================
 
 
}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit BCM2709;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,BCM2836,Platform,Threads,HeapManager,Devices,SPI,I2C,DMA,GPIO,MMC,Framebuffer,SysUtils; 

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
const
 {BCM2709 specific constants}

 {BCM2709 SPI constants}
 
 {BCM2709 I2C constants}
 
 {BCM2709 DMA constants}
 
 {BCM2709 PWM constants}
 
 {BCM2709 PCM constants}
 
 {BCM2709 GPIO constants}
 
 {BCM2709 SDHCI constants}
 BCM2709_EMMC_MIN_FREQ = 400000;    {Default minimum of 400KHz}
 BCM2709_EMMC_MAX_FREQ = 250000000; //To Do //Get the current frequency from the command line or mailbox instead ? //Peripheral init could get from Mailbox like SMSC95XX ?
 
 {BCM2709 Clock constants}
 
 {BCM2709 Timer constants}
 
 {BCM2709 Random constants}
 BCM2709_RANDOM_WARMUP_COUNT  = $00040000; {The initial numbers generated are "less random" so will be discarded}

 {BCM2709 Mailbox constants}
 
 {BCM2709 Watchdog constants}
 
 {BCM2709 Framebuffer constants}
 
{==============================================================================}
type
 {BCM2709 specific types}
 
 {BCM2709 SPI types}
 
 {BCM2709 I2C types}
 
 {BCM2709 DMA types}
 
 {BCM2709 PWM types}
 
 {BCM2709 PCM types}
 
 {BCM2709 GPIO types}
 
 {BCM2709 SDHCI types}
 PBCM2709SDHCIHost = ^TBCM2709SDHCIHost;
 TBCM2709SDHCIHost = record
  {SDHCI Properties}
  SDHCI:TSDHCIHost;
  {BCM2709 Properties}
  //Lock:TSpinHandle; //To Do //Not Needed ? //See: DWCOTG etc
  WriteDelay:LongWord;
  LastWrite:LongWord;
  ShadowRegister:LongWord;
 end;
 
 {BCM2709 Clock types}
 PBCM2709Clock = ^TBCM2709Clock;
 TBCM2709Clock = record
  {Clock Properties}
  Clock:TClockDevice;
  {BCM2709 Properties}
   {Nothing}
 end; 

 {BCM2709 Timer types}
 PBCM2709Timer = ^TBCM2709Timer;
 TBCM2709Timer = record
  {Timer Properties}
  Timer:TTimerDevice;
  {BCM2709 Properties}
   {Nothing}
 end; 
 
 {BCM2709 Random types}
 PBCM2709Random = ^TBCM2709Random;
 TBCM2709Random = record
  {Random Properties}
  Random:TRandomDevice;
  {BCM2709 Properties}
   {Nothing}
 end; 

 {BCM2709 Mailbox types}
 PBCM2709Mailbox = ^TBCM2709Mailbox;
 TBCM2709Mailbox = record
  {Mailbox Properties}
  Mailbox:TMailboxDevice;
  {BCM2709 Properties}
   {Nothing}
 end; 
 
 {BCM2709 Watchdog types}
 PBCM2709Watchdog = ^TBCM2709Watchdog;
 TBCM2709Watchdog = record
  {Watchdog Properties}
  Watchdog:TWatchdogDevice;
  {BCM2709 Properties}
   {Nothing}
 end; 

 {BCM2709 Framebuffer types}
 PBCM2709Framebuffer = ^TBCM2709Framebuffer;
 TBCM2709Framebuffer = record
  {Framebuffer Properties}
  Framebuffer:TFramebufferDevice;
  {BCM2709 Properties}
   {Nothing}
 end; 
 
{==============================================================================}
{var}
 {BCM2709 specific variables}
 
{==============================================================================}
{Initialization Functions}
procedure BCM2709Init;
 
{==============================================================================}
{BCM2709 Functions}

{==============================================================================}
{BCM2709 SPI Functions}
 
{==============================================================================}
{BCM2709 I2C Functions}

{==============================================================================}
{BCM2709 DMA Functions}

{==============================================================================}
{BCM2709 PWM Functions}

{==============================================================================}
{BCM2709 PCM Functions}

{==============================================================================}
{BCM2709 GPIO Functions}

{==============================================================================}
{BCM2709 SDHCI Functions}
function BCM2709SDHCIHostStart(SDHCI:PSDHCIHost):LongWord;
function BCM2709SDHCIHostStop(SDHCI:PSDHCIHost):LongWord;

function BCM2709SDHCIHostReadByte(SDHCI:PSDHCIHost;Reg:LongWord):Byte; 
function BCM2709SDHCIHostReadWord(SDHCI:PSDHCIHost;Reg:LongWord):Word; 
function BCM2709SDHCIHostReadLong(SDHCI:PSDHCIHost;Reg:LongWord):LongWord; 
procedure BCM2709SDHCIHostWriteByte(SDHCI:PSDHCIHost;Reg:LongWord;Value:Byte); 
procedure BCM2709SDHCIHostWriteWord(SDHCI:PSDHCIHost;Reg:LongWord;Value:Word); 
procedure BCM2709SDHCIHostWriteLong(SDHCI:PSDHCIHost;Reg:LongWord;Value:LongWord); 
 
procedure BCM2709SDHCIInterruptHandler(SDHCI:PSDHCIHost);
function BCM2709SDHCISetupInterrupts(SDHCI:PSDHCIHost):LongWord;
 
{==============================================================================}
{BCM2709 Clock Functions}
function BCM2709ClockRead(Clock:PClockDevice):LongWord;
function BCM2709ClockRead64(Clock:PClockDevice):Int64;

{==============================================================================}
{BCM2709 Timer Functions}
//To Do

{==============================================================================}
{BCM2709 Random Functions}
function BCM2709RandomStart(Random:PRandomDevice):LongWord;
function BCM2709RandomStop(Random:PRandomDevice):LongWord;

function BCM2709RandomReadLongWord(Random:PRandomDevice):LongWord;

{==============================================================================}
{BCM2709 Mailbox Functions}
//To Do

{==============================================================================}
{BCM2709 Watchdog Functions}
function BCM2709WatchdogStart(Watchdog:PWatchdogDevice):LongWord;
function BCM2709WatchdogStop(Watchdog:PWatchdogDevice):LongWord;
function BCM2709WatchdogRefresh(Watchdog:PWatchdogDevice):LongWord;

function BCM2709WatchdogGetRemain(Watchdog:PWatchdogDevice):LongWord;

{==============================================================================}
{BCM2709 Framebuffer Functions}
function BCM2709FramebufferAllocate(Framebuffer:PFramebufferDevice;Properties:PFramebufferProperties):LongWord;
function BCM2709FramebufferRelease(Framebuffer:PFramebufferDevice):LongWord;

function BCM2709FramebufferSetProperties(Framebuffer:PFramebufferDevice;Properties:PFramebufferProperties):LongWord;

{==============================================================================}
{BCM2709 Helper Functions}
  
{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {BCM2709 specific variables}
 BCM2709Initialized:Boolean;

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure BCM2709Init;
var
 Status:LongWord;
 
 BCM2709SDHCIHost:PBCM2709SDHCIHost; 

 BCM2709Clock:PBCM2709Clock;
 BCM2709Timer:PBCM2709Timer;
 BCM2709Random:PBCM2709Random;
 BCM2709Mailbox:PBCM2709Mailbox;
 BCM2709Watchdog:PBCM2709Watchdog;
 BCM2709Framebuffer:PBCM2709Framebuffer;
begin
 {}
 {Check Initialized}
 if BCM2709Initialized then Exit;
 
 {Initialize BCM2709SDHCI_FIQ_ENABLED}
 if not(FIQ_ENABLED) then BCM2709SDHCI_FIQ_ENABLED:=False;
 
 {Create SPI}
 if BCM2709_REGISTER_SPI then
  begin
   //To Do
  end; 
 
 {Create I2C}
 if BCM2709_REGISTER_I2C then
  begin
   //To Do
  end;
 
 {Create DMA}
 if BCM2709_REGISTER_DMA then
  begin
   //To Do
  end;
  
 {Create PWM}
 if BCM2709_REGISTER_PWM then
  begin
   //To Do
  end;
  
 {Create PCM}
 if BCM2709_REGISTER_PCM then
  begin
   //To Do
  end;
  
 {Create GPIO}
 if BCM2709_REGISTER_GPIO then
  begin
   //To Do
  end;
 
 {Create SDHCI}
 if BCM2709_REGISTER_SDHCI then
  begin
   BCM2709SDHCIHost:=PBCM2709SDHCIHost(SDHCIHostCreateEx(SizeOf(TBCM2709SDHCIHost)));
   if BCM2709SDHCIHost <> nil then
    begin
     {Update SDHCI}
     {Device}
     BCM2709SDHCIHost.SDHCI.Device.DeviceBus:=DEVICE_BUS_MMIO; 
     BCM2709SDHCIHost.SDHCI.Device.DeviceType:=SDHCI_TYPE_NONE;
     BCM2709SDHCIHost.SDHCI.Device.DeviceFlags:=SDHCI_FLAG_NONE;
     BCM2709SDHCIHost.SDHCI.Device.DeviceData:=nil;
     {SDHCI}
     BCM2709SDHCIHost.SDHCI.SDHCIState:=SDHCI_STATE_DISABLED;
     BCM2709SDHCIHost.SDHCI.HostStart:=BCM2709SDHCIHostStart;
     BCM2709SDHCIHost.SDHCI.HostStop:=BCM2709SDHCIHostStop;
     BCM2709SDHCIHost.SDHCI.HostReadByte:=BCM2709SDHCIHostReadByte;
     BCM2709SDHCIHost.SDHCI.HostReadWord:=BCM2709SDHCIHostReadWord;
     BCM2709SDHCIHost.SDHCI.HostReadLong:=BCM2709SDHCIHostReadLong;
     BCM2709SDHCIHost.SDHCI.HostWriteByte:=BCM2709SDHCIHostWriteByte;
     BCM2709SDHCIHost.SDHCI.HostWriteWord:=BCM2709SDHCIHostWriteWord;
     BCM2709SDHCIHost.SDHCI.HostWriteLong:=BCM2709SDHCIHostWriteLong;
     BCM2709SDHCIHost.SDHCI.HostSetClockDivider:=nil;
     BCM2709SDHCIHost.SDHCI.HostSetControlRegister:=nil;
     BCM2709SDHCIHost.SDHCI.DeviceInitialize:=nil;
     BCM2709SDHCIHost.SDHCI.DeviceGetCardDetect:=nil;
     BCM2709SDHCIHost.SDHCI.DeviceGetWriteProtect:=nil;
     BCM2709SDHCIHost.SDHCI.DeviceSendCommand:=nil;
     BCM2709SDHCIHost.SDHCI.DeviceSetIOS:=nil;
     {Driver}
     BCM2709SDHCIHost.SDHCI.Address:=Pointer(BCM2836_SDHCI_REGS_BASE);
   
     {Register SDHCI}
     Status:=SDHCIHostRegister(@BCM2709SDHCIHost.SDHCI);
     if Status <> ERROR_SUCCESS then
      begin
       if MMC_LOG_ENABLED then MMCLogError(nil,'BCM2709: Failed to register new SDHCI host: ' + ErrorToString(Status));
      end;
    end
   else 
    begin
     if MMC_LOG_ENABLED then MMCLogError(nil,'BCM2709: Failed to create new SDHCI host');
    end;
  end;

 {Create Clock}
 if BCM2709_REGISTER_CLOCK then
  begin
   BCM2709Clock:=PBCM2709Clock(ClockDeviceCreateEx(SizeOf(TBCM2709Clock)));
   if BCM2709Clock <> nil then
    begin
     {Update Clock}
     {Device}
     BCM2709Clock.Clock.Device.DeviceBus:=DEVICE_BUS_MMIO; 
     BCM2709Clock.Clock.Device.DeviceType:=CLOCK_TYPE_HARDWARE;
     BCM2709Clock.Clock.Device.DeviceFlags:=CLOCK_FLAG_NONE;
     BCM2709Clock.Clock.Device.DeviceData:=nil;
     {Clock}
     BCM2709Clock.Clock.ClockState:=CLOCK_STATE_DISABLED;
     BCM2709Clock.Clock.DeviceRead:=BCM2709ClockRead;
     BCM2709Clock.Clock.DeviceRead64:=BCM2709ClockRead64;
     {Driver}
     BCM2709Clock.Clock.Address:=Pointer(BCM2836_SYSTEM_TIMER_REGS_BASE);
     BCM2709Clock.Clock.Rate:=BCM2836_SYSTEM_TIMER_FREQUENCY;
    
     {Register Clock}
     Status:=ClockDeviceRegister(@BCM2709Clock.Clock);
     if Status = ERROR_SUCCESS then
      begin
       {Start Clock}
       Status:=ClockDeviceStart(@BCM2709Clock.Clock);
       if Status <> ERROR_SUCCESS then
        begin
         if DEVICE_LOG_ENABLED then DeviceLogError(nil,'BCM2709: Failed to start new clock device: ' + ErrorToString(Status));
        end;
      end
     else 
      begin
       if DEVICE_LOG_ENABLED then DeviceLogError(nil,'BCM2709: Failed to register new clock device: ' + ErrorToString(Status));
      end;
    end
   else 
    begin
     if DEVICE_LOG_ENABLED then DeviceLogError(nil,'BCM2709: Failed to create new clock device');
    end;
  end;
  
 {Create Timer}
 if BCM2709_REGISTER_TIMER then
  begin
   //To Do
  end; 
 
 {Create Random}
 if BCM2709_REGISTER_RANDOM then
  begin
   BCM2709Random:=PBCM2709Random(RandomDeviceCreateEx(SizeOf(TBCM2709Random)));
   if BCM2709Random <> nil then
    begin
     {Update Random}
     {Device}
     BCM2709Random.Random.Device.DeviceBus:=DEVICE_BUS_MMIO; 
     BCM2709Random.Random.Device.DeviceType:=RANDOM_TYPE_HARDWARE;
     BCM2709Random.Random.Device.DeviceFlags:=RANDOM_FLAG_NONE;
     BCM2709Random.Random.Device.DeviceData:=nil;
     {Random}
     BCM2709Random.Random.RandomState:=RANDOM_STATE_DISABLED;
     BCM2709Random.Random.DeviceStart:=BCM2709RandomStart;
     BCM2709Random.Random.DeviceStop:=BCM2709RandomStop;
     BCM2709Random.Random.DeviceReadLongWord:=BCM2709RandomReadLongWord;
     {Driver}
     BCM2709Random.Random.Address:=Pointer(BCM2836_RNG_REGS_BASE);
     
     {Register Random}
     Status:=RandomDeviceRegister(@BCM2709Random.Random);
     if Status = ERROR_SUCCESS then
      begin
       {Start Random}
       Status:=RandomDeviceStart(@BCM2709Random.Random);
       if Status <> ERROR_SUCCESS then
        begin
         if DEVICE_LOG_ENABLED then DeviceLogError(nil,'BCM2709: Failed to start new random device: ' + ErrorToString(Status));
        end;
      end
     else 
      begin
       if DEVICE_LOG_ENABLED then DeviceLogError(nil,'BCM2709: Failed to register new random device: ' + ErrorToString(Status));
      end;
    end
   else 
    begin
     if DEVICE_LOG_ENABLED then DeviceLogError(nil,'BCM2709: Failed to create new random device');
    end;
  end;
  
 {Create Mailbox}
 if BCM2709_REGISTER_MAILBOX then
  begin
   //To Do
  end; 
  
 {Create Watchdog}
 if BCM2709_REGISTER_WATCHDOG then
  begin
   BCM2709Watchdog:=PBCM2709Watchdog(WatchdogDeviceCreateEx(SizeOf(TBCM2709Watchdog)));
   if BCM2709Watchdog <> nil then
    begin
     {Device}
     BCM2709Watchdog.Watchdog.Device.DeviceBus:=DEVICE_BUS_MMIO; 
     BCM2709Watchdog.Watchdog.Device.DeviceType:=WATCHDOG_TYPE_HARDWARE;
     BCM2709Watchdog.Watchdog.Device.DeviceFlags:=WATCHDOG_FLAG_NONE;
     BCM2709Watchdog.Watchdog.Device.DeviceData:=nil;
     {Watchdog}
     BCM2709Watchdog.Watchdog.WatchdogState:=WATCHDOG_STATE_DISABLED;
     BCM2709Watchdog.Watchdog.DeviceStart:=BCM2709WatchdogStart;
     BCM2709Watchdog.Watchdog.DeviceStop:=BCM2709WatchdogStop;
     BCM2709Watchdog.Watchdog.DeviceRefresh:=BCM2709WatchdogRefresh;
     BCM2709Watchdog.Watchdog.DeviceGetRemain:=BCM2709WatchdogGetRemain;
     {Driver}
     BCM2709Watchdog.Watchdog.Address:=Pointer(BCM2836_PM_REGS_BASE);
     
     {Register Watchdog}
     Status:=WatchdogDeviceRegister(@BCM2709Watchdog.Watchdog);
     if Status <> ERROR_SUCCESS then
      begin
       if DEVICE_LOG_ENABLED then DeviceLogError(nil,'BCM2709: Failed to register new watchdog device: ' + ErrorToString(Status));
      end;
    end
   else 
    begin
     if DEVICE_LOG_ENABLED then DeviceLogError(nil,'BCM2709: Failed to create new watchdog device');
    end;
  end;
 
 {$IFNDEF CONSOLE_EARLY_INIT}
 {Create Framebuffer}
 if BCM2709_REGISTER_FRAMEBUFFER then
  begin
   BCM2709Framebuffer:=PBCM2709Framebuffer(FramebufferDeviceCreateEx(SizeOf(TBCM2709Framebuffer)));
   if BCM2709Framebuffer <> nil then
    begin
     {Device}
     BCM2709Framebuffer.Framebuffer.Device.DeviceBus:=DEVICE_BUS_MMIO; 
     BCM2709Framebuffer.Framebuffer.Device.DeviceType:=FRAMEBUFFER_TYPE_HARDWARE;
     BCM2709Framebuffer.Framebuffer.Device.DeviceFlags:=FRAMEBUFFER_FLAG_NONE;
     BCM2709Framebuffer.Framebuffer.Device.DeviceData:=nil;
     {Framebuffer}
     BCM2709Framebuffer.Framebuffer.FramebufferState:=FRAMEBUFFER_STATE_DISABLED;
     BCM2709Framebuffer.Framebuffer.DeviceAllocate:=BCM2709FramebufferAllocate;
     BCM2709Framebuffer.Framebuffer.DeviceRelease:=BCM2709FramebufferRelease;
     BCM2709Framebuffer.Framebuffer.DeviceSetProperties:=BCM2709FramebufferSetProperties;
     {Driver}
     
     {Setup Flags}
     if BCM2709FRAMEBUFFER_CACHED then BCM2709Framebuffer.Framebuffer.Device.DeviceFlags:=BCM2709Framebuffer.Framebuffer.Device.DeviceFlags or FRAMEBUFFER_FLAG_CACHED;
     if SysUtils.GetEnvironmentVariable('bcm2708_fb.fbswap') <> '1' then BCM2709Framebuffer.Framebuffer.Device.DeviceFlags:=BCM2709Framebuffer.Framebuffer.Device.DeviceFlags or FRAMEBUFFER_FLAG_SWAP;
     
     {Register Framebuffer}
     Status:=FramebufferDeviceRegister(@BCM2709Framebuffer.Framebuffer);
     if Status = ERROR_SUCCESS then
      begin
       {Allocate Framebuffer}
       Status:=FramebufferDeviceAllocate(@BCM2709Framebuffer.Framebuffer,nil);
       if Status <> ERROR_SUCCESS then
        begin
         if DEVICE_LOG_ENABLED then DeviceLogError(nil,'BCM2709: Failed to allocate new framebuffer device: ' + ErrorToString(Status));
        end;
      end
     else
      begin     
       if DEVICE_LOG_ENABLED then DeviceLogError(nil,'BCM2709: Failed to register new framebuffer device: ' + ErrorToString(Status));
      end;
    end
   else 
    begin
     if DEVICE_LOG_ENABLED then DeviceLogError(nil,'BCM2709: Failed to create new framebuffer device');
    end;
  end;
 {$ENDIF}
 
 BCM2709Initialized:=True;
end;
 
{==============================================================================}
{==============================================================================}
{BCM2709 Functions}

{==============================================================================}
{==============================================================================}
{BCM2709 SPI Functions}
 
{==============================================================================}
{==============================================================================}
{BCM2709 I2C Functions}

{==============================================================================}
{==============================================================================}
{BCM2709 DMA Functions}

{==============================================================================}
{==============================================================================}
{BCM2709 PWM Functions}

{==============================================================================}
{==============================================================================}
{BCM2709 PCM Functions}

{==============================================================================}
{==============================================================================}
{BCM2709 GPIO Functions}

{==============================================================================}
{==============================================================================}
{BCM2709 SDHCI Functions}
function BCM2709SDHCIHostStart(SDHCI:PSDHCIHost):LongWord;
var
 Status:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check SDHCI}
 if SDHCI = nil then Exit;
 
 if MMC_LOG_ENABLED then MMCLogInfo(nil,'BCM2709 Powering on Arasan SD Host Controller');

 {Power On SD}
 Status:=PowerOn(POWER_ID_MMC0);
 if Status <> ERROR_SUCCESS then
  begin
   if MMC_LOG_ENABLED then MMCLogError(nil,'Failed to power on Arasan SD Host Controller');
   
   Result:=Status;
   Exit;
  end;
 
 {Update SDHCI}
 {Driver Properties}
 if BCM2709SDHCI_FIQ_ENABLED then
  begin
   SDHCI.Wait:=SemaphoreCreateEx(0,SEMAPHORE_DEFAULT_MAXIMUM,SEMAPHORE_FLAG_IRQFIQ);
  end
 else
  begin
   SDHCI.Wait:=SemaphoreCreateEx(0,SEMAPHORE_DEFAULT_MAXIMUM,SEMAPHORE_FLAG_IRQ);
  end;  
 SDHCI.Version:=SDHCIHostReadWord(SDHCI,SDHCI_HOST_VERSION);
 SDHCI.Quirks:=SDHCI_QUIRK_NO_HISPD_BIT or SDHCI_QUIRK_MISSING_CAPS;  //To Do //More ?
 SDHCI.Quirks2:=SDHCI_QUIRK2_BROKEN_R1B or SDHCI_QUIRK2_WAIT_SEND_CMD;
 {Configuration Properties}
 SDHCI.PresetVoltages:=MMC_VDD_32_33 or MMC_VDD_33_34 or MMC_VDD_165_195;
 SDHCI.PresetCapabilities:=0;   //To Do //See: Linux ?
 SDHCI.ClockMinimum:=BCM2709_EMMC_MIN_FREQ;
 SDHCI.ClockMaximum:=BCM2709_EMMC_MAX_FREQ; //To Do //Get from somewhere //See above
 
 {$IF DEFINED(BCM2709_DEBUG) or DEFINED(MMC_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'BCM2709 host version = ' + IntToHex(SDHCIGetVersion(SDHCI),4));
 {$ENDIF}
 
 {Update BCM2709}
 PBCM2709SDHCIHost(SDHCI).WriteDelay:=((2 * 1000000) div BCM2709_EMMC_MIN_FREQ) + 1;  //To Do //Get sdhci-BCM2709.emmc_clock_freq from command line (or get from Mailbox Properties ?) //No, probably command line is best. Platform startup can get from Mailbox and place in command line //see above
 PBCM2709SDHCIHost(SDHCI).LastWrite:=0;
 
 {$IF DEFINED(BCM2709_DEBUG) or DEFINED(MMC_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'BCM2709 host write delay =  ' + IntToStr(PBCM2709SDHCIHost(SDHCI).WriteDelay));
 {$ENDIF}
 
 {Reset Host}
 SDHCIHostReset(SDHCI,SDHCI_RESET_ALL);
 
 {$IF DEFINED(BCM2709_DEBUG) or DEFINED(MMC_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'BCM2709 host reset completed');
 {$ENDIF}
 
 {Setup Interrupts}
 Result:=BCM2709SDHCISetupInterrupts(SDHCI);
 
 //See: bcm2835_sdhci_init in bcm2835_sdhci.c
end;

{==============================================================================}

function BCM2709SDHCIHostStop(SDHCI:PSDHCIHost):LongWord;
var
 Status:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check SDHCI}
 if SDHCI = nil then Exit;

 {Release the IRQ/FIQ}
 if BCM2709SDHCI_FIQ_ENABLED then
  begin
   ReleaseFIQ(FIQ_ROUTING,BCM2836_IRQ_SDHCI,TInterruptHandler(BCM2709SDHCIInterruptHandler),SDHCI);
  end
 else
  begin
   ReleaseIRQ(IRQ_ROUTING,BCM2836_IRQ_SDHCI,TInterruptHandler(BCM2709SDHCIInterruptHandler),SDHCI);
  end;  
 
 {Reset Host}
 SDHCIHostReset(SDHCI,SDHCI_RESET_ALL);
 
 {$IF DEFINED(BCM2709_DEBUG) or DEFINED(MMC_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'BCM2709 host reset completed');
 {$ENDIF}

 {Update SDHCI}
 {Driver Properties}
 SemaphoreDestroy(SDHCI.Wait);
 SDHCI.Wait:=INVALID_HANDLE_VALUE;
 
 if MMC_LOG_ENABLED then MMCLogInfo(nil,'BCM2709 Powering off Arasan SD Host Controller');

 {Power Off SD}
 Status:=PowerOff(POWER_ID_MMC0);
 if Status <> ERROR_SUCCESS then
  begin
   if MMC_LOG_ENABLED then MMCLogError(nil,'Failed to power off Arasan SD Host Controller');
   
   Result:=Status;
   Exit;
  end;
  
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function BCM2709SDHCIHostReadByte(SDHCI:PSDHCIHost;Reg:LongWord):Byte; 
{Note: The Broadcom document BCM2835-ARM-Peripherals page 66 states the following:

 Contrary to Arasans documentation the EMMC module registers can only be accessed as
 32 bit registers, i.e. the two LSBs of the address are always zero.

 For this reason this code must simulate Byte and Word reads using LongWord reads.
}
var
 Value:LongWord;
 ByteNo:LongWord;
 ByteShift:LongWord;
begin
 {}
 {Read LongWord}
 Value:=PLongWord(PtrUInt(SDHCI.Address) + PtrUInt(Reg and not(3)))^;
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
 
 {Get Byte and Shift}
 ByteNo:=(Reg and 3);
 ByteShift:=(ByteNo shl 3);
 
 {Get Result}
 Result:=(Value shr ByteShift) and $FF;

 //See: bcm2835_sdhci_readb in bcm2835_sdhci.c
end;

{==============================================================================}

function BCM2709SDHCIHostReadWord(SDHCI:PSDHCIHost;Reg:LongWord):Word; 
{Note: The Broadcom document BCM2835-ARM-Peripherals page 66 states the following:

 Contrary to Arasans documentation the EMMC module registers can only be accessed as
 32 bit registers, i.e. the two LSBs of the address are always zero.

 For this reason this code must simulate Byte and Word reads using LongWord reads.
}
var
 Value:LongWord;
 WordNo:LongWord;
 WordShift:LongWord;
begin
 {}
 {Read LongWord}
 Value:=PLongWord(PtrUInt(SDHCI.Address) + PtrUInt(Reg and not(3)))^;
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
 
 {Get Word and Shift}
 WordNo:=((Reg shr 1) and 1);
 WordShift:=(WordNo shl 4);
 
 {Get Result}
 Result:=(Value shr WordShift) and $FFFF;

 //See: bcm2835_sdhci_readw in bcm2835_sdhci.c
end;

{==============================================================================}

function BCM2709SDHCIHostReadLong(SDHCI:PSDHCIHost;Reg:LongWord):LongWord; 
begin
 {}
 {Read LongWord}
 Result:=PLongWord(PtrUInt(SDHCI.Address) + PtrUInt(Reg))^;
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
 
 //See: bcm2835_sdhci_raw_readl in bcm2835_sdhci.c
 //     bcm2835_sdhci_readl in bcm2835_sdhci.c
end;

{==============================================================================}

procedure BCM2709SDHCIHostWriteByte(SDHCI:PSDHCIHost;Reg:LongWord;Value:Byte); 
{Note: The Broadcom document BCM2835-ARM-Peripherals page 66 states the following:

 Contrary to Arasans documentation the EMMC module registers can only be accessed as
 32 bit registers, i.e. the two LSBs of the address are always zero.

 For this reason this code must simulate Byte and Word writes using LongWord writes.
}
var
 Mask:LongWord;
 ByteNo:LongWord;
 ByteShift:LongWord;
 OldValue:LongWord;
 NewValue:LongWord;
begin
 {}
 {Read LongWord}
 OldValue:=PLongWord(PtrUInt(SDHCI.Address) + PtrUInt(Reg and not(3)))^;
 
 {Memory Barrier}
 DataMemoryBarrier;{After the Last Read} 
 
 {Get Byte, Shift and Mask}
 ByteNo:=(Reg and 3);
 ByteShift:=(ByteNo shl 3);
 Mask:=($FF shl ByteShift);

 {Get Value}
 NewValue:=(OldValue and not(Mask)) or (Value shl ByteShift);
 
 {Write LongWord}
 BCM2709SDHCIHostWriteLong(SDHCI,Reg and not(3),NewValue);

 //See: bcm2835_sdhci_writeb in bcm2835_sdhci.c
end;

{==============================================================================}

procedure BCM2709SDHCIHostWriteWord(SDHCI:PSDHCIHost;Reg:LongWord;Value:Word); 
{Note: The Broadcom document BCM2835-ARM-Peripherals page 66 states the following:

 Contrary to Arasans documentation the EMMC module registers can only be accessed as
 32 bit registers, i.e. the two LSBs of the address are always zero.

 For this reason this code must simulate Byte and Word writes using LongWord writes.
}
var
 Mask:LongWord;
 WordNo:LongWord;
 WordShift:LongWord;
 OldValue:LongWord;
 NewValue:LongWord;
begin
 {}
 {Check Register}
 if Reg = SDHCI_COMMAND then
  begin
   {Get LongWord}
   OldValue:=PBCM2709SDHCIHost(SDHCI).ShadowRegister;
  end
 else
  begin
   {Read LongWord}
   OldValue:=PLongWord(PtrUInt(SDHCI.Address) + PtrUInt(Reg and not(3)))^;
   
   {Memory Barrier}
   DataMemoryBarrier; {After the Last Read} 
  end;

 {Get Word, Shift and Mask}
 WordNo:=((Reg shr 1) and 1);
 WordShift:=(WordNo shl 4); 
 Mask:=($FFFF shl WordShift);

 {Get Value}
 NewValue:=(OldValue and not(Mask)) or (Value shl WordShift);

 {Check Register}
 if Reg = SDHCI_TRANSFER_MODE then
  begin
   {Save LongWord}
   PBCM2709SDHCIHost(SDHCI).ShadowRegister:=NewValue;
  end
 else
  begin
   {Write LongWord}
   BCM2709SDHCIHostWriteLong(SDHCI,Reg and not(3),NewValue);
  end;  
  
 //See: bcm2835_sdhci_writew in bcm2835_sdhci.c
end;

{==============================================================================}

procedure BCM2709SDHCIHostWriteLong(SDHCI:PSDHCIHost;Reg:LongWord;Value:LongWord); 
{Note: The source code of U-Boot and Linux kernel drivers have this comment

 The Arasan has a bugette whereby it may lose the content of
 successive writes to registers that are within two SD-card clock
 cycles of each other (a clock domain crossing problem).
 It seems, however, that the data register does not have this problem.
 (Which is just as well - otherwise we'd have to nobble the DMA engine too)
 
 For this reason this code must delay after each write to the registers.
}
begin
 {}
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Write LongWord}
 PLongWord(PtrUInt(SDHCI.Address) + PtrUInt(Reg))^:=Value;
 
 {Wait Delay}
 MicrosecondDelay(PBCM2709SDHCIHost(SDHCI).WriteDelay);
 
 //To Do //Need GetTimerMicroseconds() in Platform (with a Since value as a Parameter ?) //Then use the LastWrite value in SDHCI
         //Also add GetTimerMilliseconds() in Platform as well
               
 //See: bcm2835_sdhci_raw_writel in bcm2835_sdhci.c
 //     bcm2835_sdhci_writel in bcm2835_sdhci.c
end;

{==============================================================================}

procedure BCM2709SDHCIInterruptHandler(SDHCI:PSDHCIHost);
var
 Count:Integer;
 Present:Boolean;
 InterruptMask:LongWord;
 UnexpectedMask:LongWord;
 AcknowledgeMask:LongWord;
begin
 {}
 {Check SDHCI}
 if SDHCI = nil then Exit;
 
 {Update Statistics}
 Inc(SDHCI.InterruptCount); 
 
 {$IF (DEFINED(BCM2709_DEBUG) or DEFINED(MMC_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI BCM2709 Interrupt Handler');
 {$ENDIF}
 
 {Get Interrupt Mask}
 InterruptMask:=SDHCIHostReadLong(SDHCI,SDHCI_INT_STATUS);

 {$IF (DEFINED(BCM2709_DEBUG) or DEFINED(MMC_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI BCM2709 Interrupt Handler (InterruptMask=' + IntToHex(InterruptMask,8) + ')');
 {$ENDIF}
 
 {Check for No Interrupts}
 if (InterruptMask = 0) or (InterruptMask = $FFFFFFFF) then Exit;

 Count:=16;
 UnexpectedMask:=0;
 while InterruptMask <> 0 do
  begin
   {Clear selected interrupts}
   AcknowledgeMask:=(InterruptMask and (SDHCI_INT_CMD_MASK or SDHCI_INT_DATA_MASK or SDHCI_INT_BUS_POWER));
   SDHCIHostWriteLong(SDHCI,SDHCI_INT_STATUS,AcknowledgeMask);
   
   {$IF (DEFINED(BCM2709_DEBUG) or DEFINED(MMC_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
   if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI BCM2709 Interrupt Handler (AcknowledgeMask=' + IntToHex(AcknowledgeMask,8) + ')');
   {$ENDIF}
   
   {Check for insert / remove interrupts}
   if (InterruptMask and (SDHCI_INT_CARD_INSERT or SDHCI_INT_CARD_REMOVE)) <> 0 then
    begin
     {$IF (DEFINED(BCM2709_DEBUG) or DEFINED(MMC_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
     if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI BCM2709 Insert / Remove Interrupt (InterruptMask=' + IntToHex(InterruptMask,8) + ')');
     {$ENDIF}
     
     {There is a observation on i.mx esdhc. INSERT bit will be immediately set again when it gets cleared, if a card is inserted.
      We have to mask the irq to prevent interrupt storm which will freeze the system. And the REMOVE gets the same situation.
	
      More testing are needed here to ensure it works for other platforms though}
      
     {Get Card Present}
     Present:=(SDHCIHostReadLong(SDHCI,SDHCI_PRESENT_STATE) and SDHCI_CARD_PRESENT) <> 0;
     
     {Disable insert / remove interrupts}
     SDHCI.Interrupts:=SDHCI.Interrupts and not(SDHCI_INT_CARD_INSERT or SDHCI_INT_CARD_REMOVE);
     
     {Enable insert / remove depending on presence}
     if Present then SDHCI.Interrupts:=SDHCI.Interrupts or SDHCI_INT_CARD_REMOVE;
     if not(Present) then SDHCI.Interrupts:=SDHCI.Interrupts or SDHCI_INT_CARD_INSERT;
     
     {Update interrupts}
	 SDHCIHostWriteLong(SDHCI,SDHCI_INT_ENABLE,SDHCI.Interrupts);
	 SDHCIHostWriteLong(SDHCI,SDHCI_SIGNAL_ENABLE,SDHCI.Interrupts);
     
     {Acknowledge interrupts}
     SDHCIHostWriteLong(SDHCI,SDHCI_INT_STATUS,InterruptMask and (SDHCI_INT_CARD_INSERT or SDHCI_INT_CARD_REMOVE));
                     
     {Signal insert or remove}
     //To Do //Needs an MMC Thread for Insert/Remove Handling and Polling Card Detect (Timer/Worker possibly ?)
    end;
    
   {Check for command iterrupts}
   if (InterruptMask and SDHCI_INT_CMD_MASK) <> 0 then
    begin
     {$IF (DEFINED(BCM2709_DEBUG) or DEFINED(MMC_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
     if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI BCM2709 Command Interrupt (InterruptMask=' + IntToHex(InterruptMask,8) + ')');
     {$ENDIF}
     
     SDHCIHostCommandInterrupt(SDHCI,InterruptMask and SDHCI_INT_CMD_MASK,InterruptMask);
    end;
    
   {Check for data interrupts} 
   if (InterruptMask and SDHCI_INT_DATA_MASK) <> 0 then
    begin
     {$IF (DEFINED(BCM2709_DEBUG) or DEFINED(MMC_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
     if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI BCM2709 Data Interrupt (InterruptMask=' + IntToHex(InterruptMask,8) + ')');
     {$ENDIF}
     
     SDHCIHostDataInterrupt(SDHCI,InterruptMask and SDHCI_INT_DATA_MASK);
    end;
   
   {Check for bus power interrupt}
   if (InterruptMask and SDHCI_INT_BUS_POWER) <> 0 then
    begin
     {$IF (DEFINED(BCM2709_DEBUG) or DEFINED(MMC_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
     if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI BCM2709 Bus Power Interrupt (InterruptMask=' + IntToHex(InterruptMask,8) + ')');
     {$ENDIF}
     
     //To Do //Log Error
    end;
 
   {Check for card interrupt}
   if (InterruptMask and SDHCI_INT_CARD_INT) <> 0 then
    begin
     {$IF (DEFINED(BCM2709_DEBUG) or DEFINED(MMC_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
     if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI BCM2709 Card Interrupt (InterruptMask=' + IntToHex(InterruptMask,8) + ')');
     {$ENDIF}
     
     //To Do //Signal another thread ? //Is this only for SDIO ?
    end;
   
   {Check for unexpected interrupts}
   InterruptMask:=InterruptMask and not(SDHCI_INT_CARD_INSERT or SDHCI_INT_CARD_REMOVE or SDHCI_INT_CMD_MASK or SDHCI_INT_DATA_MASK or SDHCI_INT_ERROR or SDHCI_INT_BUS_POWER or SDHCI_INT_CARD_INT);
   if InterruptMask <> 0 then
    begin
     UnexpectedMask:=UnexpectedMask or InterruptMask;
     SDHCIHostWriteLong(SDHCI,SDHCI_INT_STATUS,InterruptMask);
    end;
    
   {Check Count}
   Dec(Count);
   if Count <= 0 then Break;
   
   {Get Interrupt Mask}
   InterruptMask:=SDHCIHostReadLong(SDHCI,SDHCI_INT_STATUS);
  end;

 if UnexpectedMask <> 0 then
  begin
   {$IF (DEFINED(BCM2709_DEBUG) or DEFINED(MMC_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
   if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI BCM2709 Unexpected Interrupt (UnexpectedMask=' + IntToHex(UnexpectedMask,8) + ')');
   {$ENDIF}
   
   //To Do //Log Error
  end;
  
 {$IF (DEFINED(BCM2709_DEBUG) or DEFINED(MMC_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI BCM2709 Interrupt Handler completed');
 {$ENDIF}
  
 //See: bcm2835_mmc_irq in \linux-rpi-3.18.y\drivers\mmc\host\bcm2835-mmc.c
end;

{==============================================================================}

function BCM2709SDHCISetupInterrupts(SDHCI:PSDHCIHost):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check SDHCI}
 if SDHCI = nil then Exit;
 
 {Setup the Interrupts to handle}
 SDHCI.Interrupts:=SDHCI_INT_BUS_POWER or SDHCI_INT_DATA_END_BIT or SDHCI_INT_DATA_CRC or SDHCI_INT_DATA_TIMEOUT or SDHCI_INT_INDEX or SDHCI_INT_END_BIT or SDHCI_INT_CRC or SDHCI_INT_TIMEOUT or SDHCI_INT_DATA_END or SDHCI_INT_RESPONSE;
                   //SDHCI_INT_CARD_INSERT or SDHCI_INT_CARD_REMOVE or //See sdhci_set_card_detection in \linux-rpi-3.18.y\drivers\mmc\host\sdhci.c
                   //Note: SDHCI_INT_CARD_INSERT seems to hang everything, why?
                   
 {Enable the Interrupts to handle}
 SDHCIHostWriteLong(SDHCI,SDHCI_INT_ENABLE,SDHCI.Interrupts);
 SDHCIHostWriteLong(SDHCI,SDHCI_SIGNAL_ENABLE,SDHCI.Interrupts);

 {Request the IRQ/FIQ} 
 if BCM2709SDHCI_FIQ_ENABLED then
  begin
   RequestFIQ(FIQ_ROUTING,BCM2836_IRQ_SDHCI,TInterruptHandler(BCM2709SDHCIInterruptHandler),SDHCI);
  end
 else
  begin
   RequestIRQ(IRQ_ROUTING,BCM2836_IRQ_SDHCI,TInterruptHandler(BCM2709SDHCIInterruptHandler),SDHCI);
  end;  
 
 {Return Result}
 Result:=ERROR_SUCCESS;
 
 //See: \linux-rpi-3.18.y\drivers\mmc\host\bcm2835-mmc.c
end;
 
{==============================================================================}
{==============================================================================}
{BCM2709 Clock Functions}
function BCM2709ClockRead(Clock:PClockDevice):LongWord;
begin
 {}
 Result:=0;
 
 {Check Clock}
 if Clock = nil then Exit;
 if Clock.Address = nil then Exit;

 if MutexLock(Clock.Lock) <> ERROR_SUCCESS then Exit;
 
 {Read Clock}
 Result:=PBCM2836SystemTimerRegisters(Clock.Address).CLO;
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read}
 
 {Update Statistics}
 Inc(Clock.ReadCount);
 
 MutexUnlock(Clock.Lock);
end;
 
{==============================================================================}

function BCM2709ClockRead64(Clock:PClockDevice):Int64;
var
 Check:LongWord;
begin
 {}
 Result:=0;
 
 {Check Clock}
 if Clock = nil then Exit;
 if Clock.Address = nil then Exit;
 
 if MutexLock(Clock.Lock) <> ERROR_SUCCESS then Exit;
 
 {Get High Value}
 Int64Rec(Result).Hi:=PBCM2836SystemTimerRegisters(Clock.Address).CHI;
 
 {Get Low Value}
 Int64Rec(Result).Lo:=PBCM2836SystemTimerRegisters(Clock.Address).CLO;
 
 {Check High Value}
 Check:=PBCM2836SystemTimerRegisters(Clock.Address).CHI;
 if Check <> Int64Rec(Result).Hi then
  begin
   {Rollover Occurred, Get Low Value Again}
   Int64Rec(Result).Hi:=Check;
   Int64Rec(Result).Lo:=PBCM2836SystemTimerRegisters(Clock.Address).CLO;
  end;
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read}
 
 {Update Statistics}
 Inc(Clock.ReadCount);
 
 MutexUnlock(Clock.Lock);
end;

{==============================================================================}
{==============================================================================}
{BCM2709 Timer Functions}
 
{==============================================================================}
{==============================================================================}
{BCM2709 Random Functions}
function BCM2709RandomStart(Random:PRandomDevice):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Random}
 if Random = nil then Exit;
 if Random.Address = nil then Exit;
 
 if MutexLock(Random.Lock) = ERROR_SUCCESS then 
  begin
   try
    {Memory Barrier}
    DataMemoryBarrier; {Before the First Write}
  
    {Enable Random}
    PBCM2836RNGRegisters(Random.Address).Status:=BCM2709_RANDOM_WARMUP_COUNT;
    PBCM2836RNGRegisters(Random.Address).Control:=BCM2836_RANDOM_ENABLE;
   
    Result:=ERROR_SUCCESS;
   finally
    MutexUnlock(Random.Lock);
   end; 
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function BCM2709RandomStop(Random:PRandomDevice):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Random}
 if Random = nil then Exit;
 if Random.Address = nil then Exit;

 if MutexLock(Random.Lock) = ERROR_SUCCESS then 
  begin
   try
    {Memory Barrier}
    DataMemoryBarrier; {Before the First Write}
   
    {Disable Random}
    PBCM2836RNGRegisters(Random.Address).Control:=BCM2836_RANDOM_DISABLE;
   
    Result:=ERROR_SUCCESS;
   finally
    MutexUnlock(Random.Lock);
   end; 
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;
 
{==============================================================================}

function BCM2709RandomReadLongWord(Random:PRandomDevice):LongWord;
begin
 {}
 Result:=0;
 
 {Check Random}
 if Random = nil then Exit;
 if Random.Address = nil then Exit;
 
 if MutexLock(Random.Lock) <> ERROR_SUCCESS then Exit;
 
 {Check Status}
 while (PBCM2836RNGRegisters(Random.Address).Status shr 24) = 0 do
  begin
   ThreadSleep(0);
  end;
  
 {Read Random}
 Result:=PBCM2836RNGRegisters(Random.Address).Data; 

 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
 
 {Update Statistics}
 Inc(Random.ReadCount);
 
 MutexUnlock(Random.Lock);
end;

{==============================================================================}
{==============================================================================}
{BCM2709 Mailbox Functions}

{==============================================================================}
{==============================================================================}
{BCM2709 Watchdog Functions}
function BCM2709WatchdogStart(Watchdog:PWatchdogDevice):LongWord;
var
 Current:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Watchdog}
 if Watchdog = nil then Exit;
 if Watchdog.Address = nil then Exit;
 
 if MutexLock(Watchdog.Lock) = ERROR_SUCCESS then 
  begin
   try
    {Check Timeout}
    Result:=ERROR_NOT_SUPPORTED;
    if Watchdog.Timeout = 0 then Exit;
 
    {Memory Barrier}
    DataMemoryBarrier; {Before the First Write}
 
    {Enable Watchdog}
    PBCM2836PMWatchdogRegisters(Watchdog.Address).WDOG:=BCM2836_PM_PASSWORD or ((Watchdog.Timeout * BCM2836_PM_WDOG_TICKS_PER_MILLISECOND) and BCM2836_PM_WDOG_TIME_MASK);
    
    Current:=PBCM2836PMWatchdogRegisters(Watchdog.Address).RSTC;
    
    PBCM2836PMWatchdogRegisters(Watchdog.Address).RSTC:=BCM2836_PM_PASSWORD or (Current and BCM2836_PM_RSTC_WRCFG_CLR) or BCM2836_PM_RSTC_WRCFG_FULL_RESET;

    {Memory Barrier}
    DataMemoryBarrier; {After the Last Read} 
    
    {Update Statistics}
    Inc(Watchdog.StartCount);
    
    Result:=ERROR_SUCCESS;
   finally
    MutexUnlock(Watchdog.Lock);
   end; 
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function BCM2709WatchdogStop(Watchdog:PWatchdogDevice):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Watchdog}
 if Watchdog = nil then Exit;
 if Watchdog.Address = nil then Exit;
 
 if MutexLock(Watchdog.Lock) = ERROR_SUCCESS then 
  begin
   try
    {Memory Barrier}
    DataMemoryBarrier; {Before the First Write}
 
    {Disable Watchdog}
    PBCM2836PMWatchdogRegisters(Watchdog.Address).RSTC:=BCM2836_PM_PASSWORD or BCM2836_PM_RSTC_RESET;
    
    {Update Statistics}
    Inc(Watchdog.StopCount);
    
    Result:=ERROR_SUCCESS;
   finally
    MutexUnlock(Watchdog.Lock);
   end; 
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function BCM2709WatchdogRefresh(Watchdog:PWatchdogDevice):LongWord;
var
 Current:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Watchdog}
 if Watchdog = nil then Exit;
 if Watchdog.Address = nil then Exit;

 if MutexLock(Watchdog.Lock) = ERROR_SUCCESS then 
  begin
   try
    {Check Timeout}
    Result:=ERROR_NOT_SUPPORTED;
    if Watchdog.Timeout = 0 then Exit;

    {Memory Barrier}
    DataMemoryBarrier; {Before the First Write}
   
    {Refresh Watchdog}
    PBCM2836PMWatchdogRegisters(Watchdog.Address).WDOG:=BCM2836_PM_PASSWORD or ((Watchdog.Timeout * BCM2836_PM_WDOG_TICKS_PER_MILLISECOND) and BCM2836_PM_WDOG_TIME_MASK);
    
    Current:=PBCM2836PMWatchdogRegisters(Watchdog.Address).RSTC;
    
    PBCM2836PMWatchdogRegisters(Watchdog.Address).RSTC:=BCM2836_PM_PASSWORD or (Current and BCM2836_PM_RSTC_WRCFG_CLR) or BCM2836_PM_RSTC_WRCFG_FULL_RESET;

    {Memory Barrier}
    DataMemoryBarrier; {After the Last Read} 
 
    {Update Statistics}
    Inc(Watchdog.RefreshCount);

    Result:=ERROR_SUCCESS;
   finally
    MutexUnlock(Watchdog.Lock);
   end; 
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function BCM2709WatchdogGetRemain(Watchdog:PWatchdogDevice):LongWord;
begin
 {}
 Result:=0;
 
 {Check Watchdog}
 if Watchdog = nil then Exit;
 if Watchdog.Address = nil then Exit;

 if MutexLock(Watchdog.Lock) = ERROR_SUCCESS then 
  begin
   try
    {Get Remain}
    Result:=(PBCM2836PMWatchdogRegisters(Watchdog.Address).WDOG and BCM2836_PM_WDOG_TIME_MASK) div BCM2836_PM_WDOG_TICKS_PER_MILLISECOND;

    {Memory Barrier}
    DataMemoryBarrier; {After the Last Read} 
   finally
    MutexUnlock(Watchdog.Lock);
   end; 
  end;
end;

{==============================================================================}
{==============================================================================}
{BCM2709 Framebuffer Functions}
function BCM2709FramebufferAllocate(Framebuffer:PFramebufferDevice;Properties:PFramebufferProperties):LongWord;
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2836MailboxHeader;
 Footer:PBCM2836MailboxFooter;
 Defaults:TFramebufferProperties;
 Tag:PBCM2836MailboxTagCreateBuffer;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 if MutexLock(Framebuffer.Lock) = ERROR_SUCCESS then 
  begin
   try
    {Check Properties}
    if Properties = nil then
     begin
      {Use Defaults}
      Defaults.Depth:=FRAMEBUFFER_DEFAULT_DEPTH;
      Defaults.Order:=FRAMEBUFFER_DEFAULT_ORDER;
      Defaults.Mode:=FRAMEBUFFER_DEFAULT_MODE;
      Defaults.PhysicalWidth:=FRAMEBUFFER_DEFAULT_WIDTH;
      Defaults.PhysicalHeight:=FRAMEBUFFER_DEFAULT_HEIGHT;
      Defaults.VirtualWidth:=FRAMEBUFFER_DEFAULT_WIDTH;
      Defaults.VirtualHeight:=FRAMEBUFFER_DEFAULT_HEIGHT;
      Defaults.OffsetX:=FRAMEBUFFER_DEFAULT_OFFSET_X;
      Defaults.OffsetY:=FRAMEBUFFER_DEFAULT_OFFSET_Y;
      Defaults.OverscanTop:=FRAMEBUFFER_DEFAULT_OVERSCAN_TOP;
      Defaults.OverscanBottom:=FRAMEBUFFER_DEFAULT_OVERSCAN_BOTTOM;
      Defaults.OverscanLeft:=FRAMEBUFFER_DEFAULT_OVERSCAN_LEFT;
      Defaults.OverscanRight:=FRAMEBUFFER_DEFAULT_OVERSCAN_RIGHT;
     end
    else
     begin
      {Use Properties}
      Defaults.Depth:=Properties.Depth;
      Defaults.Order:=Properties.Order;
      Defaults.Mode:=Properties.Mode;
      Defaults.PhysicalWidth:=Properties.PhysicalWidth;
      Defaults.PhysicalHeight:=Properties.PhysicalHeight;
      Defaults.VirtualWidth:=Properties.VirtualWidth;
      Defaults.VirtualHeight:=Properties.VirtualHeight;
      Defaults.OffsetX:=Properties.OffsetX;
      Defaults.OffsetY:=Properties.OffsetY;
      Defaults.OverscanTop:=Properties.OverscanTop;
      Defaults.OverscanBottom:=Properties.OverscanBottom;
      Defaults.OverscanLeft:=Properties.OverscanLeft;
      Defaults.OverscanRight:=Properties.OverscanRight;
     end;   

    {Check Defaults}
    if (Defaults.PhysicalWidth = 0) or (Defaults.PhysicalHeight = 0) then
     begin
      {Get Dimensions Width and Height}
      Result:=FramebufferGetDimensions(Defaults.PhysicalWidth,Defaults.PhysicalHeight,Defaults.OverscanTop,Defaults.OverscanBottom,Defaults.OverscanLeft,Defaults.OverscanRight);
      if Result <> ERROR_SUCCESS then
       begin
        if DEVICE_LOG_ENABLED then DeviceLogError(nil,'BCM2709: FramebufferAllocate - FramebufferGetDimensions failed: ' + ErrorToString(Result));
        {Exit;} {Do not fail}
        
        {Set Defaults}
        Defaults.PhysicalWidth:=640;
        Defaults.PhysicalHeight:=480;
       end;
      
      {Set Defaults}
      Defaults.VirtualWidth:=Defaults.PhysicalWidth;
      Defaults.VirtualHeight:=Defaults.PhysicalHeight;
     end;
    
    {Calculate Size}
    Size:=SizeOf(TBCM2836MailboxHeader) + SizeOf(TBCM2836MailboxTagCreateBuffer) + SizeOf(TBCM2836MailboxFooter);
    
    {Allocate Mailbox Buffer}
    Result:=ERROR_NOT_ENOUGH_MEMORY;
    Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
    if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
    if Header = nil then Exit;
    try
     {Clear Buffer}
     FillChar(Header^,Size,0);
    
     {Setup Header}
     Header.Size:=Size;
     Header.Code:=BCM2836_MBOX_REQUEST_CODE;
    
     {Setup Tag}
     Tag:=PBCM2836MailboxTagCreateBuffer(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2836MailboxHeader)));
     
     {Setup Tag (Physical)}
     Tag.Physical.Header.Tag:=BCM2836_MBOX_TAG_SET_PHYSICAL_W_H;
     Tag.Physical.Header.Size:=SizeOf(TBCM2836MailboxTagSetPhysical) - SizeOf(TBCM2836MailboxTagHeader);
     Tag.Physical.Header.Length:=SizeOf(Tag.Physical.Request);
     Tag.Physical.Request.Width:=Defaults.PhysicalWidth;
     Tag.Physical.Request.Height:=Defaults.PhysicalHeight;
     
     {Setup Tag (Virtual)}
     Tag.Vertual.Header.Tag:=BCM2836_MBOX_TAG_SET_VIRTUAL_W_H;
     Tag.Vertual.Header.Size:=SizeOf(TBCM2836MailboxTagSetVirtual) - SizeOf(TBCM2836MailboxTagHeader);
     Tag.Vertual.Header.Length:=SizeOf(Tag.Vertual.Request);
     Tag.Vertual.Request.Width:=Defaults.VirtualWidth;
     Tag.Vertual.Request.Height:=Defaults.VirtualHeight;

     {Setup Tag (Depth)}
     Tag.Depth.Header.Tag:=BCM2836_MBOX_TAG_SET_DEPTH;
     Tag.Depth.Header.Size:=SizeOf(TBCM2836MailboxTagSetDepth) - SizeOf(TBCM2836MailboxTagHeader);
     Tag.Depth.Header.Length:=SizeOf(Tag.Depth.Request);
     Tag.Depth.Request.Depth:=Defaults.Depth;
     
     {Setup Tag (Order)}
     Tag.Order.Header.Tag:=BCM2836_MBOX_TAG_SET_PIXEL_ORDER;
     Tag.Order.Header.Size:=SizeOf(TBCM2836MailboxTagSetPixelOrder) - SizeOf(TBCM2836MailboxTagHeader);
     Tag.Order.Header.Length:=SizeOf(Tag.Order.Request);
     Tag.Order.Request.Order:=Defaults.Order;
     
     {Setup Tag (Mode)}
     Tag.Mode.Header.Tag:=BCM2836_MBOX_TAG_SET_ALPHA_MODE;
     Tag.Mode.Header.Size:=SizeOf(TBCM2836MailboxTagSetAlphaMode) - SizeOf(TBCM2836MailboxTagHeader);
     Tag.Mode.Header.Length:=SizeOf(Tag.Mode.Request);
     Tag.Mode.Request.Mode:=Defaults.Mode;
     
     {Setup Tag (Offset)}
     Tag.Offset.Header.Tag:=BCM2836_MBOX_TAG_SET_VIRTUAL_OFFSET;
     Tag.Offset.Header.Size:=SizeOf(TBCM2836MailboxTagSetVirtualOffset) - SizeOf(TBCM2836MailboxTagHeader);
     Tag.Offset.Header.Length:=SizeOf(Tag.Offset.Request);
     Tag.Offset.Request.X:=Defaults.OffsetX;
     Tag.Offset.Request.Y:=Defaults.OffsetY;
     
     {Setup Tag (Overscan)}
     Tag.Overscan.Header.Tag:=BCM2836_MBOX_TAG_SET_OVERSCAN;
     Tag.Overscan.Header.Size:=SizeOf(TBCM2836MailboxTagSetOverscan) - SizeOf(TBCM2836MailboxTagHeader);
     Tag.Overscan.Header.Length:=SizeOf(Tag.Overscan.Request);
     Tag.Overscan.Request.Top:=Defaults.OverscanTop;
     Tag.Overscan.Request.Bottom:=Defaults.OverscanBottom;
     Tag.Overscan.Request.Left:=Defaults.OverscanLeft;
     Tag.Overscan.Request.Right:=Defaults.OverscanRight;
     
     {Setup Tag (Allocate)}
     Tag.Allocate.Header.Tag:=BCM2836_MBOX_TAG_ALLOCATE_BUFFER;
     Tag.Allocate.Header.Size:=SizeOf(TBCM2836MailboxTagAllocateBuffer) - SizeOf(TBCM2836MailboxTagHeader);
     Tag.Allocate.Header.Length:=SizeOf(Tag.Allocate.Request);
     Tag.Allocate.Request.Alignment:=BCM2709FRAMEBUFFER_ALIGNEMENT;
     
     {Setup Tag (Pitch)}
     Tag.Pitch.Header.Tag:=BCM2836_MBOX_TAG_GET_PITCH;
     Tag.Pitch.Header.Size:=SizeOf(TBCM2836MailboxTagGetPitch) - SizeOf(TBCM2836MailboxTagHeader);
     Tag.Pitch.Header.Length:=SizeOf(Tag.Pitch.Request);
     
     {Setup Footer}
     Footer:=PBCM2836MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2836MailboxTagCreateBuffer)));
     Footer.Tag:=BCM2836_MBOX_TAG_END;
     
     {Call Mailbox}
     Result:=MailboxPropertyCall(BCM2836_MAILBOX_0,BCM2836_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
     if Result <> ERROR_SUCCESS then
      begin
       if PLATFORM_LOG_ENABLED then PlatformLogError('BCM2709: FramebufferAllocate - MailboxPropertyCall failed: ' + ErrorToString(Result));
       Exit;
      end; 
     
     {Update Framebuffer}
     Framebuffer.Address:=BusAddressToPhysical(Pointer(Tag.Allocate.Response.Address)); {Firmware may return address as a Bus address, writes must be to the Physical address}
     Framebuffer.Size:=Tag.Allocate.Response.Size;
     Framebuffer.Pitch:=Tag.Pitch.Response.Pitch;
     Framebuffer.Depth:=Tag.Depth.Response.Depth;
     Framebuffer.Order:=Tag.Order.Response.Order;
     Framebuffer.Mode:=Tag.Mode.Response.Mode;
     Framebuffer.PhysicalWidth:=Tag.Physical.Response.Width;
     Framebuffer.PhysicalHeight:=Tag.Physical.Response.Height;
     Framebuffer.VirtualWidth:=Tag.Vertual.Response.Width;
     Framebuffer.VirtualHeight:=Tag.Vertual.Response.Height;
     Framebuffer.OffsetX:=Tag.Offset.Response.X;
     Framebuffer.OffsetY:=Tag.Offset.Response.Y;
     Framebuffer.OverscanTop:=Tag.Overscan.Response.Top;
     Framebuffer.OverscanBottom:=Tag.Overscan.Response.Bottom;
     Framebuffer.OverscanLeft:=Tag.Overscan.Response.Left;
     Framebuffer.OverscanRight:=Tag.Overscan.Response.Right;
    
     {Update Statistics}
     Inc(Framebuffer.AllocateCount);
    
     {Get Result}
     Result:=ERROR_SUCCESS;
    finally
     FreeMem(Header);
    end;
    
   finally
    MutexUnlock(Framebuffer.Lock);
   end; 
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;
   
{==============================================================================}

function BCM2709FramebufferRelease(Framebuffer:PFramebufferDevice):LongWord;
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2836MailboxHeader;
 Footer:PBCM2836MailboxFooter;
 Tag:PBCM2836MailboxTagReleaseBuffer;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 if MutexLock(Framebuffer.Lock) = ERROR_SUCCESS then 
  begin
   try
    {Calculate Size}
    Size:=SizeOf(TBCM2836MailboxHeader) + SizeOf(TBCM2836MailboxTagReleaseBuffer) + SizeOf(TBCM2836MailboxFooter);

    {Allocate Mailbox Buffer}
    Result:=ERROR_NOT_ENOUGH_MEMORY;
    Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
    if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
    if Header = nil then Exit;
    try
     {Clear Buffer}
     FillChar(Header^,Size,0);
    
     {Setup Header}
     Header.Size:=Size;
     Header.Code:=BCM2836_MBOX_REQUEST_CODE;
    
     {Setup Tag}
     Tag:=PBCM2836MailboxTagReleaseBuffer(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2836MailboxHeader)));
     Tag.Header.Tag:=BCM2836_MBOX_TAG_RELEASE_BUFFER;
     Tag.Header.Size:=SizeOf(TBCM2836MailboxTagReleaseBuffer) - SizeOf(TBCM2836MailboxTagHeader);
     Tag.Header.Length:=SizeOf(Tag.Request);
    
     {Setup Footer}
     Footer:=PBCM2836MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2836MailboxTagReleaseBuffer)));
     Footer.Tag:=BCM2836_MBOX_TAG_END;
     
     {Call Mailbox}
     Result:=MailboxPropertyCall(BCM2836_MAILBOX_0,BCM2836_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
     if Result <> ERROR_SUCCESS then
      begin
       if DEVICE_LOG_ENABLED then DeviceLogError(nil,'BCM2709: FramebufferRelease: MailboxPropertyCall failed: ' + ErrorToString(Result));
       Exit;
      end; 
     
     {Update Statistics}
     Inc(Framebuffer.ReleaseCount);
     
     {Get Result}
     Result:=ERROR_SUCCESS;
    finally
     FreeMem(Header);
    end;
   finally
    MutexUnlock(Framebuffer.Lock);
   end; 
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function BCM2709FramebufferSetProperties(Framebuffer:PFramebufferDevice;Properties:PFramebufferProperties):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Properties}
 if Properties = nil then Exit;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 if MutexLock(Framebuffer.Lock) = ERROR_SUCCESS then 
  begin
   try
    
    //To Do //Check Properties against current, modify if possible, otherwise reallocate ? (and Notify Resize)
    
   finally
    MutexUnlock(Framebuffer.Lock);
   end; 
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}
{==============================================================================}
{BCM2709 Helper Functions}

{==============================================================================}
{==============================================================================}

initialization
 BCM2709Init;
 
{==============================================================================}
 
finalization
 {Nothing}
 
{==============================================================================}
{==============================================================================}

end.
 