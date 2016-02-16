{
Ultibo BCM2708 interface unit.

Copyright (C) 2015 - SoftOz Pty Ltd.

Arch
====

 <All>

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

   Linux - MMC/SDHCI drivers
   U-Boot - MMC/SDHCI drivers
 
   Linux - \drivers\video\bcm2708_fb.c
   U-Boot - \drivers\video\bcm2835.c 
   
References
==========

 BCM2835 ARM Peripherals
 
 Raspberry Pi Mailboxes
 
  https://github.com/raspberrypi/firmware/wiki/Mailboxes
 
 RPi Low-level peripherals
  
  http://elinux.org/RPi_Low-level_peripherals
  
 RPi SPI
 
  http://elinux.org/RPi_SPI
 
BCM2708 Devices
===============

 This unit provides the BCM2708 specific implementations of the following devices:

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
 
 
BCM2708 SPI Device
==================


BCM2708 I2C Device
==================

 
BCM2708 DMA Device
==================


BCM2708 PWM Device
==================


BCM2708 PCM Device
==================

 
BCM2708 GPIO Device
===================


BCM2708 SDHCI Device
====================

 The SDHCI controller on the BCM2708 is an Arasan SD Host controller.

 The Card Detect pin is connected to GPIO pin 47 (on the RPi Model A/B)(Not connected on the RPi Model A+/B+)

 The Write Protect pin is not connected on any RPi model.


BCM2708 Clock Device
====================


BCM2708 Timer Device
====================


BCM2708 Random Device
=====================


BCM2708 Mailbox Device
======================


BCM2708 Watchdog Device
=======================


BCM2708 Framebuffer Device
==========================
 
 
}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit BCM2708;
                   
//To Do

 //See Also: \u-boot-HEAD-5745f8c\board\raspberrypi\rpi\rpi.c
 //board_mmc_init 
 //   Power on SDHCI
 //   Get EMMC Clock Rate
 
 //For SDHCI Host Linux driver see: \linux-rpi-3.12.y\drivers\mmc\host\bcm2835-mmc.c
 //                                 \linux-rpi-3.12.y\drivers\mmc\host\sdhci-bcm2708.c
 //                                 \linux-rpi-3.18.y\drivers\mmc\host\bcm2835-mmc.c   <- This one appears to be the latest 
 //                                 \linux-rpi-3.18.y\drivers\mmc\host\sdhci-bcm2835.c
 //                                 \linux-rpi-3.18.y\drivers\mmc\host\sdhci.c         <- This one for the more universal stuff
  
 //For SPI Host Linux driver see: \linux-rpi-3.12.y\drivers\spi\spi-bcm2835.c
 //                               \linux-rpi-3.12.y\drivers\spi\spi-bcm2708.c
 
 //For PCM Host Linux driver see: 
 
 //For PWM Host Linux driver see: \linux-rpi-3.12.y\drivers\pwm
 
 //For DMA Host Linux driver see: \linux-rpi-3.12.y\drivers\dma\bcm2708-dmaengine.c
 
 //For I2C Host Linux driver see: \linux-rpi-3.12.y\drivers\i2c\busses\i2c-bcm2708.c
 //                               \linux-rpi-3.12.y\drivers\i2c\busses\i2c-bcm2835.c
                                  
 //For GPIO Host U-Boot driver see: \u-boot-HEAD-5745f8c\drivers\gpio\bcm2835_gpio.c
 //For GPIO Host Linux driver see:  \linux-rpi-3.12.y\drivers\pinctrl\pinctrl-bcm2835.c
                        //See Also: \u-boot-master\drivers\gpio\bcm2835_gpio.c
          
 //For Framebuffer Host Linux driver see: \linux-rpi-3.12.y\drivers\video\bcm2708_fb.c
 //For Framebuffer Host U-Boot driver see: \u-boot-HEAD-5745f8c\drivers\video\bcm2835.c
 
 //For Watchdog Host Linux driver see: \linux-rpi-3.12.y\drivers\watchdog\bcm2708_wdog.c
 //                                    \linux-rpi-3.12.y\drivers\watchdog\bcm2835_wdt.c
 
 //Watchdog See Also:                  \linux-rpi-3.12.y\arch\arm\mach-bcm2835\bcm2835.c
 //                                    \u-boot-master\arch\arm\include\asm\arch-bcm2835\wdog.h
 //                                    \u-boot-master\arch\arm\cpu\arm1176\bcm2835\reset.c
 
               //bcm2835_restart  see: \linux-rpi-3.12.y\arch\arm\mach-bcm2835\bcm2835.c
               //bcm2835_power_off
 
 //Other useful references:
 
 //Clock:    \linux-rpi-3.12.y\drivers\clk\clk-bcm2835.c
 //
 //Random:   \linux-rpi-3.12.y\drivers\char\hw_random\bcm2835-rng.c
 //          \linux-rpi-3.12.y\drivers\char\hw_random\bcm2835-rng.c
 //
 //Hardware Monitor:  \linux-rpi-3.12.y\drivers\hwmon\bcm2835-hwmon.c (Temp/Max Temp)
 //
 //IRQ:      \linux-rpi-3.12.y\drivers\irqchip\irq-bcm2835.c
 //
 //DVB:      \linux-rpi-3.12.y\drivers\media\dvb-core
 //          \linux-rpi-3.12.y\drivers\media\dvb-frontends
 //
 //DAB:      \linux-rpi-3.12.y\drivers\media\tuners
 //
 //VC4:      \linux-rpi-3.12.y\drivers\misc\vc04_services
 //
 //Camera:   \linux-rpi-3.12.y\drivers\media\platform\bcm2835
 //
 //Thermal:   \linux-rpi-3.12.y\drivers\thermal\bcm2835-thermal.c (Temp/Max Temp)
 //
 //Timer:     \linux-rpi-3.18.y\drivers\clocksource\bcm2835_timer.c
 
 //           \linux-rpi-3.18.y\arch\arm\mach-bcm2708
 //           \linux-rpi-3.18.y\arch\arm\mach-bcm2709
 
 
interface

uses GlobalConfig,GlobalConst,GlobalTypes,BCM2835,Platform{$IFNDEF CONSOLE_EARLY_INIT},PlatformRPi{$ENDIF},Threads,HeapManager,Devices,SPI,I2C,DMA,PWM,GPIO,MMC,Framebuffer,SysUtils; 

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
const
 {BCM2708 specific constants}
 
 {BCM2708 SPI constants}
 
 {BCM2708 I2C constants}
 
 {BCM2708 DMA constants}
 
 {BCM2708 PWM constants}
 
 {BCM2708 PCM constants}
 
 {BCM2708 GPIO constants}
 
 {BCM2708 SDHCI constants}
 BCM2708_EMMC_MIN_FREQ = 400000;    {Default minimum of 400KHz}
 BCM2708_EMMC_MAX_FREQ = 250000000; //To Do //Get the current frequency from the command line or mailbox instead ? //Peripheral init could get from Mailbox like SMSC95XX ?
 
 {BCM2708 Clock constants}
 
 {BCM2708 Timer constants}
 
 {BCM2708 Random constants}
 BCM2708_RANDOM_WARMUP_COUNT  = $00040000; {The initial numbers generated are "less random" so will be discarded}

 {BCM2708 Mailbox constants}
 
 {BCM2708 Watchdog constants}
 
 {BCM2708 Framebuffer constants}
 
{==============================================================================}
type
 {BCM2708 specific types}
 
 {BCM2708 SPI types}
 
 {BCM2708 I2C types}
 
 {BCM2708 DMA types}
 
 {BCM2708 PWM types}
 
 {BCM2708 PCM types}
 
 {BCM2708 GPIO types}
 
 {BCM2708 SDHCI types}
 PBCM2708SDHCIHost = ^TBCM2708SDHCIHost;
 TBCM2708SDHCIHost = record
  {SDHCI Properties}
  SDHCI:TSDHCIHost;
  {BCM2708 Properties}
  //Lock:TSpinHandle; //To Do //Not Needed ? //See: DWCOTG etc
  WriteDelay:LongWord;
  LastWrite:LongWord;
  ShadowRegister:LongWord;
 end;
 
 {BCM2708 Clock types}
 PBCM2708Clock = ^TBCM2708Clock;
 TBCM2708Clock = record
  {Clock Properties}
  Clock:TClockDevice;
  {BCM2708 Properties}
   {Nothing}
 end; 

 {BCM2708 Timer types}
 PBCM2708Timer = ^TBCM2708Timer;
 TBCM2708Timer = record
  {Timer Properties}
  Timer:TTimerDevice;
  {BCM2708 Properties}
   {Nothing}
 end; 
 
 {BCM2708 Random types}
 PBCM2708Random = ^TBCM2708Random;
 TBCM2708Random = record
  {Random Properties}
  Random:TRandomDevice;
  {BCM2708 Properties}
   {Nothing}
 end; 

 {BCM2708 Mailbox types}
 PBCM2708Mailbox = ^TBCM2708Mailbox;
 TBCM2708Mailbox = record
  {Mailbox Properties}
  Mailbox:TMailboxDevice;
  {BCM2708 Properties}
   {Nothing}
 end; 
 
 {BCM2708 Watchdog types}
 PBCM2708Watchdog = ^TBCM2708Watchdog;
 TBCM2708Watchdog = record
  {Watchdog Properties}
  Watchdog:TWatchdogDevice;
  {BCM2708 Properties}
   {Nothing}
 end; 

 {BCM2708 Framebuffer types}
 PBCM2708Framebuffer = ^TBCM2708Framebuffer;
 TBCM2708Framebuffer = record
  {Framebuffer Properties}
  Framebuffer:TFramebufferDevice;
  {BCM2708 Properties}
   {Nothing}
 end; 
 
{==============================================================================}
{var}
 {BCM2708 specific variables}
 
{==============================================================================}
{Initialization Functions}
procedure BCM2708Init;
 
{==============================================================================}
{BCM2708 Functions}

{==============================================================================}
{BCM2708 SPI Functions}
 
{==============================================================================}
{BCM2708 I2C Functions}

{==============================================================================}
{BCM2708 DMA Functions}

{==============================================================================}
{BCM2708 PWM Functions}

{==============================================================================}
{BCM2708 PCM Functions}

{==============================================================================}
{BCM2708 GPIO Functions}

{==============================================================================}
{BCM2708 SDHCI Functions}
function BCM2708SDHCIHostStart(SDHCI:PSDHCIHost):LongWord;
function BCM2708SDHCIHostStop(SDHCI:PSDHCIHost):LongWord;

function BCM2708SDHCIHostReadByte(SDHCI:PSDHCIHost;Reg:LongWord):Byte; 
function BCM2708SDHCIHostReadWord(SDHCI:PSDHCIHost;Reg:LongWord):Word; 
function BCM2708SDHCIHostReadLong(SDHCI:PSDHCIHost;Reg:LongWord):LongWord; 
procedure BCM2708SDHCIHostWriteByte(SDHCI:PSDHCIHost;Reg:LongWord;Value:Byte); 
procedure BCM2708SDHCIHostWriteWord(SDHCI:PSDHCIHost;Reg:LongWord;Value:Word); 
procedure BCM2708SDHCIHostWriteLong(SDHCI:PSDHCIHost;Reg:LongWord;Value:LongWord); 
 
procedure BCM2708SDHCIInterruptHandler(SDHCI:PSDHCIHost);
function BCM2708SDHCISetupInterrupts(SDHCI:PSDHCIHost):LongWord;
 
{==============================================================================}
{BCM2708 Clock Functions}
function BCM2708ClockRead(Clock:PClockDevice):LongWord;
function BCM2708ClockRead64(Clock:PClockDevice):Int64;

{==============================================================================}
{BCM2708 Timer Functions}
//To Do

{==============================================================================}
{BCM2708 Random Functions}
function BCM2708RandomStart(Random:PRandomDevice):LongWord;
function BCM2708RandomStop(Random:PRandomDevice):LongWord;

function BCM2708RandomReadLongWord(Random:PRandomDevice):LongWord;

{==============================================================================}
{BCM2708 Mailbox Functions}
//To Do

{==============================================================================}
{BCM2708 Watchdog Functions}
function BCM2708WatchdogStart(Watchdog:PWatchdogDevice):LongWord;
function BCM2708WatchdogStop(Watchdog:PWatchdogDevice):LongWord;
function BCM2708WatchdogRefresh(Watchdog:PWatchdogDevice):LongWord;

function BCM2708WatchdogGetRemain(Watchdog:PWatchdogDevice):LongWord;

{==============================================================================}
{BCM2708 Framebuffer Functions}
function BCM2708FramebufferAllocate(Framebuffer:PFramebufferDevice;Properties:PFramebufferProperties):LongWord;
function BCM2708FramebufferRelease(Framebuffer:PFramebufferDevice):LongWord;

function BCM2708FramebufferSetProperties(Framebuffer:PFramebufferDevice;Properties:PFramebufferProperties):LongWord;

{==============================================================================}
{BCM2708 Helper Functions}
 
{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {BCM2708 specific variables}
 BCM2708Initialized:Boolean;

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure BCM2708Init;
var
 Status:LongWord;
 
 BCM2708SDHCIHost:PBCM2708SDHCIHost; 

 BCM2708Clock:PBCM2708Clock;
 BCM2708Timer:PBCM2708Timer;
 BCM2708Random:PBCM2708Random;
 BCM2708Mailbox:PBCM2708Mailbox;
 BCM2708Watchdog:PBCM2708Watchdog;
 BCM2708Framebuffer:PBCM2708Framebuffer;
begin
 {}
 {Check Initialized}
 if BCM2708Initialized then Exit;
 
 {Initialize BCM2708SDHCI_FIQ_ENABLED}
 if not(FIQ_ENABLED) then BCM2708SDHCI_FIQ_ENABLED:=False;
 
 {$IFNDEF CONSOLE_EARLY_INIT}
 {Register Platform GPU Memory Handlers}
 GPUMemoryAllocateHandler:=RPiGPUMemoryAllocate;
 GPUMemoryReleaseHandler:=RPiGPUMemoryRelease;
 GPUMemoryLockHandler:=RPiGPUMemoryLock;
 GPUMemoryUnlockHandler:=RPiGPUMemoryUnlock;
 
 {Register Platform GPU Misc Handlers}
 GPUExecuteCodeHandler:=RPiGPUExecuteCode;
 DispmanxHandleGetHandler:=RPiDispmanxHandleGet;
 EDIDBlockGetHandler:=RPiEDIDBlockGet;

 {Register Platform Framebuffer Handlers}
 FramebufferAllocateHandler:=RPiFramebufferAllocate;
 FramebufferReleaseHandler:=RPiFramebufferRelease;
 FramebufferSetStateHandler:=RPiFramebufferSetState;

 FramebufferGetDimensionsHandler:=RPiFramebufferGetDimensions;
 
 FramebufferGetPhysicalHandler:=RPiFramebufferGetPhysical;
 FramebufferSetPhysicalHandler:=RPiFramebufferSetPhysical;
 FramebufferTestPhysicalHandler:=RPiFramebufferTestPhysical;
 
 FramebufferGetVirtualHandler:=RPiFramebufferGetVirtual;
 FramebufferSetVirtualHandler:=RPiFramebufferSetVirtual;
 FramebufferTestVirtualHandler:=RPiFramebufferTestVirtual;
 
 FramebufferGetDepthHandler:=RPiFramebufferGetDepth;
 FramebufferSetDepthHandler:=RPiFramebufferSetDepth;
 FramebufferTestDepthHandler:=RPiFramebufferTestDepth;
 
 FramebufferGetPixelOrderHandler:=RPiFramebufferGetPixelOrder;
 FramebufferSetPixelOrderHandler:=RPiFramebufferSetPixelOrder;
 FramebufferTestPixelOrderHandler:=RPiFramebufferTestPixelOrder;
 
 FramebufferGetAlphaModeHandler:=RPiFramebufferGetAlphaMode;
 FramebufferSetAlphaModeHandler:=RPiFramebufferSetAlphaMode;
 FramebufferTestAlphaModeHandler:=RPiFramebufferTestAlphaMode;
 
 FramebufferGetPitchHandler:=RPiFramebufferGetPitch;
 
 FramebufferGetOffsetHandler:=RPiFramebufferGetOffset;
 FramebufferSetOffsetHandler:=RPiFramebufferSetOffset;
 FramebufferTestOffsetHandler:=RPiFramebufferTestOffset;
 
 FramebufferGetOverscanHandler:=RPiFramebufferGetOverscan;
 FramebufferSetOverscanHandler:=RPiFramebufferSetOverscan;
 FramebufferTestOverscanHandler:=RPiFramebufferTestOverscan;
 
 FramebufferGetPaletteHandler:=RPiFramebufferGetPalette;
 FramebufferSetPaletteHandler:=RPiFramebufferSetPalette;
 FramebufferTestPaletteHandler:=RPiFramebufferTestPalette;

 {Register Platform Cursor Handlers}
 CursorSetInfoHandler:=RPiCursorSetInfo;
 CursorSetStateHandler:=RPiCursorSetState;
 {$ENDIF}
 
 {Create SPI}
 if BCM2708_REGISTER_SPI then
  begin
   //To Do
  end; 
 
 {Create I2C}
 if BCM2708_REGISTER_I2C then
  begin
   //To Do
  end;
 
 {Create DMA}
 if BCM2708_REGISTER_DMA then
  begin
   //To Do
  end;
  
 {Create PWM}
 if BCM2708_REGISTER_PWM then
  begin
   //To Do
  end;
  
 {Create PCM}
 if BCM2708_REGISTER_PCM then
  begin
   //To Do
  end;
  
 {Create GPIO}
 if BCM2708_REGISTER_GPIO then
  begin
   //To Do
  end;
 
 {Create SDHCI}
 if BCM2708_REGISTER_SDHCI then
  begin
   BCM2708SDHCIHost:=PBCM2708SDHCIHost(SDHCIHostCreateEx(SizeOf(TBCM2708SDHCIHost)));
   if BCM2708SDHCIHost <> nil then
    begin
     {Update SDHCI}
     {Device}
     BCM2708SDHCIHost.SDHCI.Device.DeviceBus:=DEVICE_BUS_MMIO; 
     BCM2708SDHCIHost.SDHCI.Device.DeviceType:=SDHCI_TYPE_NONE;
     BCM2708SDHCIHost.SDHCI.Device.DeviceFlags:=SDHCI_FLAG_NONE;
     BCM2708SDHCIHost.SDHCI.Device.DeviceData:=nil;
     {SDHCI}
     BCM2708SDHCIHost.SDHCI.SDHCIState:=SDHCI_STATE_DISABLED;
     BCM2708SDHCIHost.SDHCI.HostStart:=BCM2708SDHCIHostStart;
     BCM2708SDHCIHost.SDHCI.HostStop:=BCM2708SDHCIHostStop;
     BCM2708SDHCIHost.SDHCI.HostReadByte:=BCM2708SDHCIHostReadByte;
     BCM2708SDHCIHost.SDHCI.HostReadWord:=BCM2708SDHCIHostReadWord;
     BCM2708SDHCIHost.SDHCI.HostReadLong:=BCM2708SDHCIHostReadLong;
     BCM2708SDHCIHost.SDHCI.HostWriteByte:=BCM2708SDHCIHostWriteByte;
     BCM2708SDHCIHost.SDHCI.HostWriteWord:=BCM2708SDHCIHostWriteWord;
     BCM2708SDHCIHost.SDHCI.HostWriteLong:=BCM2708SDHCIHostWriteLong;
     BCM2708SDHCIHost.SDHCI.HostSetClockDivider:=nil;
     BCM2708SDHCIHost.SDHCI.HostSetControlRegister:=nil;
     BCM2708SDHCIHost.SDHCI.DeviceInitialize:=nil;
     BCM2708SDHCIHost.SDHCI.DeviceGetCardDetect:=nil;
     BCM2708SDHCIHost.SDHCI.DeviceGetWriteProtect:=nil;
     BCM2708SDHCIHost.SDHCI.DeviceSendCommand:=nil;
     BCM2708SDHCIHost.SDHCI.DeviceSetIOS:=nil;
     {Driver}
     BCM2708SDHCIHost.SDHCI.Address:=Pointer(BCM2835_SDHCI_REGS_BASE);
   
     {Register SDHCI}
     Status:=SDHCIHostRegister(@BCM2708SDHCIHost.SDHCI);
     if Status <> ERROR_SUCCESS then
      begin
       if MMC_LOG_ENABLED then MMCLogError(nil,'BCM2708: Failed to register new SDHCI host: ' + ErrorToString(Status));
      end;
    end
   else 
    begin
     if MMC_LOG_ENABLED then MMCLogError(nil,'BCM2708: Failed to create new SDHCI host');
    end;
  end;

 {Create Clock}
 if BCM2708_REGISTER_CLOCK then
  begin
   BCM2708Clock:=PBCM2708Clock(ClockDeviceCreateEx(SizeOf(TBCM2708Clock)));
   if BCM2708Clock <> nil then
    begin
     {Update Clock}
     {Device}
     BCM2708Clock.Clock.Device.DeviceBus:=DEVICE_BUS_MMIO; 
     BCM2708Clock.Clock.Device.DeviceType:=CLOCK_TYPE_HARDWARE;
     BCM2708Clock.Clock.Device.DeviceFlags:=CLOCK_FLAG_NONE;
     BCM2708Clock.Clock.Device.DeviceData:=nil;
     {Clock}
     BCM2708Clock.Clock.ClockState:=CLOCK_STATE_DISABLED;
     BCM2708Clock.Clock.DeviceRead:=BCM2708ClockRead;
     BCM2708Clock.Clock.DeviceRead64:=BCM2708ClockRead64;
     {Driver}
     BCM2708Clock.Clock.Address:=Pointer(BCM2835_SYSTEM_TIMER_REGS_BASE);
     BCM2708Clock.Clock.Rate:=BCM2835_SYSTEM_TIMER_FREQUENCY;
    
     {Register Clock}
     Status:=ClockDeviceRegister(@BCM2708Clock.Clock);
     if Status = ERROR_SUCCESS then
      begin
       {Start Clock}
       Status:=ClockDeviceStart(@BCM2708Clock.Clock);
       if Status <> ERROR_SUCCESS then
        begin
         if DEVICE_LOG_ENABLED then DeviceLogError(nil,'BCM2708: Failed to start new clock device: ' + ErrorToString(Status));
        end;
      end
     else 
      begin
       if DEVICE_LOG_ENABLED then DeviceLogError(nil,'BCM2708: Failed to register new clock device: ' + ErrorToString(Status));
      end;
    end
   else 
    begin
     if DEVICE_LOG_ENABLED then DeviceLogError(nil,'BCM2708: Failed to create new clock device');
    end;
  end;
  
 {Create Timer}
 if BCM2708_REGISTER_TIMER then
  begin
   //To Do
  end; 
 
 {Create Random}
 if BCM2708_REGISTER_RANDOM then
  begin
   BCM2708Random:=PBCM2708Random(RandomDeviceCreateEx(SizeOf(TBCM2708Random)));
   if BCM2708Random <> nil then
    begin
     {Update Random}
     {Device}
     BCM2708Random.Random.Device.DeviceBus:=DEVICE_BUS_MMIO; 
     BCM2708Random.Random.Device.DeviceType:=RANDOM_TYPE_HARDWARE;
     BCM2708Random.Random.Device.DeviceFlags:=RANDOM_FLAG_NONE;
     BCM2708Random.Random.Device.DeviceData:=nil;
     {Random}
     BCM2708Random.Random.RandomState:=RANDOM_STATE_DISABLED;
     BCM2708Random.Random.DeviceStart:=BCM2708RandomStart;
     BCM2708Random.Random.DeviceStop:=BCM2708RandomStop;
     BCM2708Random.Random.DeviceReadLongWord:=BCM2708RandomReadLongWord;
     {Driver}
     BCM2708Random.Random.Address:=Pointer(BCM2835_RNG_REGS_BASE);
     
     {Register Random}
     Status:=RandomDeviceRegister(@BCM2708Random.Random);
     if Status = ERROR_SUCCESS then
      begin
       {Start Random}
       Status:=RandomDeviceStart(@BCM2708Random.Random);
       if Status <> ERROR_SUCCESS then
        begin
         if DEVICE_LOG_ENABLED then DeviceLogError(nil,'BCM2708: Failed to start new random device: ' + ErrorToString(Status));
        end;
      end
     else 
      begin
       if DEVICE_LOG_ENABLED then DeviceLogError(nil,'BCM2708: Failed to register new random device: ' + ErrorToString(Status));
      end;
    end
   else 
    begin
     if DEVICE_LOG_ENABLED then DeviceLogError(nil,'BCM2708: Failed to create new random device');
    end;
  end;
  
 {Create Mailbox}
 if BCM2708_REGISTER_MAILBOX then
  begin
   //To Do
  end; 
  
 {Create Watchdog}
 if BCM2708_REGISTER_WATCHDOG then
  begin
   BCM2708Watchdog:=PBCM2708Watchdog(WatchdogDeviceCreateEx(SizeOf(TBCM2708Watchdog)));
   if BCM2708Watchdog <> nil then
    begin
     {Device}
     BCM2708Watchdog.Watchdog.Device.DeviceBus:=DEVICE_BUS_MMIO; 
     BCM2708Watchdog.Watchdog.Device.DeviceType:=WATCHDOG_TYPE_HARDWARE;
     BCM2708Watchdog.Watchdog.Device.DeviceFlags:=WATCHDOG_FLAG_NONE;
     BCM2708Watchdog.Watchdog.Device.DeviceData:=nil;
     {Watchdog}
     BCM2708Watchdog.Watchdog.WatchdogState:=WATCHDOG_STATE_DISABLED;
     BCM2708Watchdog.Watchdog.DeviceStart:=BCM2708WatchdogStart;
     BCM2708Watchdog.Watchdog.DeviceStop:=BCM2708WatchdogStop;
     BCM2708Watchdog.Watchdog.DeviceRefresh:=BCM2708WatchdogRefresh;
     BCM2708Watchdog.Watchdog.DeviceGetRemain:=BCM2708WatchdogGetRemain;
     {Driver}
     BCM2708Watchdog.Watchdog.Address:=Pointer(BCM2835_PM_REGS_BASE);
     
     {Register Watchdog}
     Status:=WatchdogDeviceRegister(@BCM2708Watchdog.Watchdog);
     if Status <> ERROR_SUCCESS then
      begin
       if DEVICE_LOG_ENABLED then DeviceLogError(nil,'BCM2708: Failed to register new watchdog device: ' + ErrorToString(Status));
      end;
    end
   else 
    begin
     if DEVICE_LOG_ENABLED then DeviceLogError(nil,'BCM2708: Failed to create new watchdog device');
    end;
  end;
 
 {$IFNDEF CONSOLE_EARLY_INIT}
 {Create Framebuffer}
 if BCM2708_REGISTER_FRAMEBUFFER then
  begin
   BCM2708Framebuffer:=PBCM2708Framebuffer(FramebufferDeviceCreateEx(SizeOf(TBCM2708Framebuffer)));
   if BCM2708Framebuffer <> nil then
    begin
     {Device}
     BCM2708Framebuffer.Framebuffer.Device.DeviceBus:=DEVICE_BUS_MMIO; 
     BCM2708Framebuffer.Framebuffer.Device.DeviceType:=FRAMEBUFFER_TYPE_HARDWARE;
     BCM2708Framebuffer.Framebuffer.Device.DeviceFlags:=FRAMEBUFFER_FLAG_NONE;
     BCM2708Framebuffer.Framebuffer.Device.DeviceData:=nil;
     {Framebuffer}
     BCM2708Framebuffer.Framebuffer.FramebufferState:=FRAMEBUFFER_STATE_DISABLED;
     BCM2708Framebuffer.Framebuffer.DeviceAllocate:=BCM2708FramebufferAllocate;
     BCM2708Framebuffer.Framebuffer.DeviceRelease:=BCM2708FramebufferRelease;
     BCM2708Framebuffer.Framebuffer.DeviceSetProperties:=BCM2708FramebufferSetProperties;
     {Driver}
     
     {Setup Flags}
     if BCM2708FRAMEBUFFER_CACHED then BCM2708Framebuffer.Framebuffer.Device.DeviceFlags:=BCM2708Framebuffer.Framebuffer.Device.DeviceFlags or FRAMEBUFFER_FLAG_CACHED;
     if SysUtils.GetEnvironmentVariable('bcm2708_fb.fbswap') <> '1' then BCM2708Framebuffer.Framebuffer.Device.DeviceFlags:=BCM2708Framebuffer.Framebuffer.Device.DeviceFlags or FRAMEBUFFER_FLAG_SWAP; 
     
     {Register Framebuffer}
     Status:=FramebufferDeviceRegister(@BCM2708Framebuffer.Framebuffer);
     if Status = ERROR_SUCCESS then
      begin
       {Allocate Framebuffer}
       Status:=FramebufferDeviceAllocate(@BCM2708Framebuffer.Framebuffer,nil);
       if Status <> ERROR_SUCCESS then
        begin
         if DEVICE_LOG_ENABLED then DeviceLogError(nil,'BCM2708: Failed to allocate new framebuffer device: ' + ErrorToString(Status));
        end;
      end
     else
      begin     
       if DEVICE_LOG_ENABLED then DeviceLogError(nil,'BCM2708: Failed to register new framebuffer device: ' + ErrorToString(Status));
      end;
    end
   else 
    begin
     if DEVICE_LOG_ENABLED then DeviceLogError(nil,'BCM2708: Failed to create new framebuffer device');
    end;
  end;
 {$ENDIF}
 
 BCM2708Initialized:=True;
end;
 
{==============================================================================}
{==============================================================================}
{BCM2708 Functions}

{==============================================================================}
{==============================================================================}
{BCM2708 SPI Functions}
 
{==============================================================================}
{==============================================================================}
{BCM2708 I2C Functions}

{==============================================================================}
{==============================================================================}
{BCM2708 DMA Functions}

{==============================================================================}
{==============================================================================}
{BCM2708 PWM Functions}

{==============================================================================}
{==============================================================================}
{BCM2708 PCM Functions}

{==============================================================================}
{==============================================================================}
{BCM2708 GPIO Functions}

{==============================================================================}
{==============================================================================}
{BCM2708 SDHCI Functions}
function BCM2708SDHCIHostStart(SDHCI:PSDHCIHost):LongWord;
var
 Status:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check SDHCI}
 if SDHCI = nil then Exit;
 
 if MMC_LOG_ENABLED then MMCLogInfo(nil,'BCM2708 Powering on Arasan SD Host Controller');

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
 if BCM2708SDHCI_FIQ_ENABLED then
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
 SDHCI.ClockMinimum:=BCM2708_EMMC_MIN_FREQ;
 SDHCI.ClockMaximum:=BCM2708_EMMC_MAX_FREQ; //To Do //Get from somewhere //See above
 
 {$IF DEFINED(BCM2708_DEBUG) or DEFINED(MMC_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'BCM2708 host version = ' + IntToHex(SDHCIGetVersion(SDHCI),4));
 {$ENDIF}
 
 {Update BCM2708}
 PBCM2708SDHCIHost(SDHCI).WriteDelay:=((2 * 1000000) div BCM2708_EMMC_MIN_FREQ) + 1;  //To Do //Get sdhci-bcm2708.emmc_clock_freq from command line (or get from Mailbox Properties ?) //No, probably command line is best. Platform startup can get from Mailbox and place in command line //see above
 PBCM2708SDHCIHost(SDHCI).LastWrite:=0;
 
 {$IF DEFINED(BCM2708_DEBUG) or DEFINED(MMC_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'BCM2708 host write delay =  ' + IntToStr(PBCM2708SDHCIHost(SDHCI).WriteDelay));
 {$ENDIF}
 
 {Reset Host}
 SDHCIHostReset(SDHCI,SDHCI_RESET_ALL);
 
 {$IF DEFINED(BCM2708_DEBUG) or DEFINED(MMC_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'BCM2708 host reset completed');
 {$ENDIF}
 
 {Setup Interrupts}
 Result:=BCM2708SDHCISetupInterrupts(SDHCI);
 
 //See: bcm2835_sdhci_init in bcm2835_sdhci.c
end;

{==============================================================================}

function BCM2708SDHCIHostStop(SDHCI:PSDHCIHost):LongWord;
var
 Status:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check SDHCI}
 if SDHCI = nil then Exit;

 {Release the IRQ/FIQ}
 if BCM2708SDHCI_FIQ_ENABLED then
  begin
   ReleaseFIQ(FIQ_ROUTING,BCM2835_IRQ_SDHCI,TInterruptHandler(BCM2708SDHCIInterruptHandler),SDHCI);
  end
 else
  begin
   ReleaseIRQ(IRQ_ROUTING,BCM2835_IRQ_SDHCI,TInterruptHandler(BCM2708SDHCIInterruptHandler),SDHCI);
  end;  
 
 {Reset Host}
 SDHCIHostReset(SDHCI,SDHCI_RESET_ALL);
 
 {$IF DEFINED(BCM2708_DEBUG) or DEFINED(MMC_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'BCM2708 host reset completed');
 {$ENDIF}
 
 {Update SDHCI}
 {Driver Properties}
 SemaphoreDestroy(SDHCI.Wait);
 SDHCI.Wait:=INVALID_HANDLE_VALUE;
 
 if MMC_LOG_ENABLED then MMCLogInfo(nil,'BCM2708 Powering off Arasan SD Host Controller');

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

function BCM2708SDHCIHostReadByte(SDHCI:PSDHCIHost;Reg:LongWord):Byte; 
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

function BCM2708SDHCIHostReadWord(SDHCI:PSDHCIHost;Reg:LongWord):Word; 
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

function BCM2708SDHCIHostReadLong(SDHCI:PSDHCIHost;Reg:LongWord):LongWord; 
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

procedure BCM2708SDHCIHostWriteByte(SDHCI:PSDHCIHost;Reg:LongWord;Value:Byte); 
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
 BCM2708SDHCIHostWriteLong(SDHCI,Reg and not(3),NewValue);

 //See: bcm2835_sdhci_writeb in bcm2835_sdhci.c
end;

{==============================================================================}

procedure BCM2708SDHCIHostWriteWord(SDHCI:PSDHCIHost;Reg:LongWord;Value:Word); 
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
   OldValue:=PBCM2708SDHCIHost(SDHCI).ShadowRegister;
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
   PBCM2708SDHCIHost(SDHCI).ShadowRegister:=NewValue;
  end
 else
  begin
   {Write LongWord}
   BCM2708SDHCIHostWriteLong(SDHCI,Reg and not(3),NewValue);
  end;  
  
 //See: bcm2835_sdhci_writew in bcm2835_sdhci.c
end;

{==============================================================================}

procedure BCM2708SDHCIHostWriteLong(SDHCI:PSDHCIHost;Reg:LongWord;Value:LongWord); 
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
 MicrosecondDelay(PBCM2708SDHCIHost(SDHCI).WriteDelay);
 
 //To Do //Need GetTimerMicroseconds() in Platform (with a Since value as a Parameter ?) //Then use the LastWrite value in SDHCI
         //Also add GetTimerMilliseconds() in Platform as well
               
 //See: bcm2835_sdhci_raw_writel in bcm2835_sdhci.c
 //     bcm2835_sdhci_writel in bcm2835_sdhci.c
end;

{==============================================================================}

procedure BCM2708SDHCIInterruptHandler(SDHCI:PSDHCIHost);
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
 
 {$IF (DEFINED(BCM2708_DEBUG) or DEFINED(MMC_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI BCM2708 Interrupt Handler');
 {$ENDIF}
 
 {Get Interrupt Mask}
 InterruptMask:=SDHCIHostReadLong(SDHCI,SDHCI_INT_STATUS);

 {$IF (DEFINED(BCM2708_DEBUG) or DEFINED(MMC_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI BCM2708 Interrupt Handler (InterruptMask=' + IntToHex(InterruptMask,8) + ')');
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
   
   {$IF (DEFINED(BCM2708_DEBUG) or DEFINED(MMC_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
   if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI BCM2708 Interrupt Handler (AcknowledgeMask=' + IntToHex(AcknowledgeMask,8) + ')');
   {$ENDIF}
   
   {Check for insert / remove interrupts}
   if (InterruptMask and (SDHCI_INT_CARD_INSERT or SDHCI_INT_CARD_REMOVE)) <> 0 then
    begin
     {$IF (DEFINED(BCM2708_DEBUG) or DEFINED(MMC_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
     if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI BCM2708 Insert / Remove Interrupt (InterruptMask=' + IntToHex(InterruptMask,8) + ')');
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
     {$IF (DEFINED(BCM2708_DEBUG) or DEFINED(MMC_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
     if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI BCM2708 Command Interrupt (InterruptMask=' + IntToHex(InterruptMask,8) + ')');
     {$ENDIF}
     
     SDHCIHostCommandInterrupt(SDHCI,InterruptMask and SDHCI_INT_CMD_MASK,InterruptMask);
    end;
    
   {Check for data interrupts} 
   if (InterruptMask and SDHCI_INT_DATA_MASK) <> 0 then
    begin
     {$IF (DEFINED(BCM2708_DEBUG) or DEFINED(MMC_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
     if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI BCM2708 Data Interrupt (InterruptMask=' + IntToHex(InterruptMask,8) + ')');
     {$ENDIF}
     
     SDHCIHostDataInterrupt(SDHCI,InterruptMask and SDHCI_INT_DATA_MASK);
    end;
   
   {Check for bus power interrupt}
   if (InterruptMask and SDHCI_INT_BUS_POWER) <> 0 then
    begin
     {$IF (DEFINED(BCM2708_DEBUG) or DEFINED(MMC_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
     if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI BCM2708 Bus Power Interrupt (InterruptMask=' + IntToHex(InterruptMask,8) + ')');
     {$ENDIF}
     
     //To Do //Log Error
    end;
 
   {Check for card interrupt}
   if (InterruptMask and SDHCI_INT_CARD_INT) <> 0 then
    begin
     {$IF (DEFINED(BCM2708_DEBUG) or DEFINED(MMC_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
     if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI BCM2708 Card Interrupt (InterruptMask=' + IntToHex(InterruptMask,8) + ')');
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
   {$IF (DEFINED(BCM2708_DEBUG) or DEFINED(MMC_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
   if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI BCM2708 Unexpected Interrupt (UnexpectedMask=' + IntToHex(UnexpectedMask,8) + ')');
   {$ENDIF}
   
   //To Do //Log Error
  end;
  
 {$IF (DEFINED(BCM2708_DEBUG) or DEFINED(MMC_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI BCM2708 Interrupt Handler completed');
 {$ENDIF}
  
 //See: bcm2835_mmc_irq in \linux-rpi-3.18.y\drivers\mmc\host\bcm2835-mmc.c
end;

{==============================================================================}

function BCM2708SDHCISetupInterrupts(SDHCI:PSDHCIHost):LongWord;
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
 if BCM2708SDHCI_FIQ_ENABLED then
  begin
   RequestFIQ(FIQ_ROUTING,BCM2835_IRQ_SDHCI,TInterruptHandler(BCM2708SDHCIInterruptHandler),SDHCI);
  end
 else
  begin
   RequestIRQ(IRQ_ROUTING,BCM2835_IRQ_SDHCI,TInterruptHandler(BCM2708SDHCIInterruptHandler),SDHCI);
  end;  
 
 {Return Result}
 Result:=ERROR_SUCCESS;
 
 //See: \linux-rpi-3.18.y\drivers\mmc\host\bcm2835-mmc.c
end;
 
{==============================================================================}
{==============================================================================}
{BCM2708 Clock Functions}
function BCM2708ClockRead(Clock:PClockDevice):LongWord;
begin
 {}
 Result:=0;
 
 {Check Clock}
 if Clock = nil then Exit;
 if Clock.Address = nil then Exit;

 if MutexLock(Clock.Lock) <> ERROR_SUCCESS then Exit;
 
 {Read Clock}
 Result:=PBCM2835SystemTimerRegisters(Clock.Address).CLO;
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read}
 
 {Update Statistics}
 Inc(Clock.ReadCount);
 
 MutexUnlock(Clock.Lock);
end;
 
{==============================================================================}

function BCM2708ClockRead64(Clock:PClockDevice):Int64;
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
 Int64Rec(Result).Hi:=PBCM2835SystemTimerRegisters(Clock.Address).CHI;
 
 {Get Low Value}
 Int64Rec(Result).Lo:=PBCM2835SystemTimerRegisters(Clock.Address).CLO;
 
 {Check High Value}
 Check:=PBCM2835SystemTimerRegisters(Clock.Address).CHI;
 if Check <> Int64Rec(Result).Hi then
  begin
   {Rollover Occurred, Get Low Value Again}
   Int64Rec(Result).Hi:=Check;
   Int64Rec(Result).Lo:=PBCM2835SystemTimerRegisters(Clock.Address).CLO;
  end;
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read}
 
 {Update Statistics}
 Inc(Clock.ReadCount);
 
 MutexUnlock(Clock.Lock);
end;

{==============================================================================}
{==============================================================================}
{BCM2708 Timer Functions}
 
{==============================================================================}
{==============================================================================}
{BCM2708 Random Functions}
function BCM2708RandomStart(Random:PRandomDevice):LongWord;
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
    PBCM2835RNGRegisters(Random.Address).Status:=BCM2708_RANDOM_WARMUP_COUNT;
    PBCM2835RNGRegisters(Random.Address).Control:=BCM2835_RANDOM_ENABLE;
   
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

function BCM2708RandomStop(Random:PRandomDevice):LongWord;
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
    PBCM2835RNGRegisters(Random.Address).Control:=BCM2835_RANDOM_DISABLE;
   
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

function BCM2708RandomReadLongWord(Random:PRandomDevice):LongWord;
begin
 {}
 Result:=0;
 
 {Check Random}
 if Random = nil then Exit;
 if Random.Address = nil then Exit;
 
 if MutexLock(Random.Lock) <> ERROR_SUCCESS then Exit;
 
 {Check Status}
 while (PBCM2835RNGRegisters(Random.Address).Status shr 24) = 0 do
  begin
   ThreadSleep(0);
  end;
  
 {Read Random}
 Result:=PBCM2835RNGRegisters(Random.Address).Data; 

 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
 
 {Update Statistics}
 Inc(Random.ReadCount);
 
 MutexUnlock(Random.Lock);
end;

{==============================================================================}
{==============================================================================}
{BCM2708 Mailbox Functions}

{==============================================================================}
{==============================================================================}
{BCM2708 Watchdog Functions}
function BCM2708WatchdogStart(Watchdog:PWatchdogDevice):LongWord;
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
    PBCM2835PMWatchdogRegisters(Watchdog.Address).WDOG:=BCM2835_PM_PASSWORD or ((Watchdog.Timeout * BCM2835_PM_WDOG_TICKS_PER_MILLISECOND) and BCM2835_PM_WDOG_TIME_MASK);
    
    Current:=PBCM2835PMWatchdogRegisters(Watchdog.Address).RSTC;
    
    PBCM2835PMWatchdogRegisters(Watchdog.Address).RSTC:=BCM2835_PM_PASSWORD or (Current and BCM2835_PM_RSTC_WRCFG_CLR) or BCM2835_PM_RSTC_WRCFG_FULL_RESET;

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

function BCM2708WatchdogStop(Watchdog:PWatchdogDevice):LongWord;
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
    PBCM2835PMWatchdogRegisters(Watchdog.Address).RSTC:=BCM2835_PM_PASSWORD or BCM2835_PM_RSTC_RESET;
    
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

function BCM2708WatchdogRefresh(Watchdog:PWatchdogDevice):LongWord;
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
    PBCM2835PMWatchdogRegisters(Watchdog.Address).WDOG:=BCM2835_PM_PASSWORD or ((Watchdog.Timeout * BCM2835_PM_WDOG_TICKS_PER_MILLISECOND) and BCM2835_PM_WDOG_TIME_MASK);
    
    Current:=PBCM2835PMWatchdogRegisters(Watchdog.Address).RSTC;
    
    PBCM2835PMWatchdogRegisters(Watchdog.Address).RSTC:=BCM2835_PM_PASSWORD or (Current and BCM2835_PM_RSTC_WRCFG_CLR) or BCM2835_PM_RSTC_WRCFG_FULL_RESET;

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

function BCM2708WatchdogGetRemain(Watchdog:PWatchdogDevice):LongWord;
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
    Result:=(PBCM2835PMWatchdogRegisters(Watchdog.Address).WDOG and BCM2835_PM_WDOG_TIME_MASK) div BCM2835_PM_WDOG_TICKS_PER_MILLISECOND;

    {Memory Barrier}
    DataMemoryBarrier; {After the Last Read} 
   finally
    MutexUnlock(Watchdog.Lock);
   end; 
  end;
end;

{==============================================================================}
{==============================================================================}
{BCM2708 Framebuffer Functions}
function BCM2708FramebufferAllocate(Framebuffer:PFramebufferDevice;Properties:PFramebufferProperties):LongWord;
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2835MailboxHeader;
 Footer:PBCM2835MailboxFooter;
 Defaults:TFramebufferProperties;
 Tag:PBCM2835MailboxTagCreateBuffer;
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
        if DEVICE_LOG_ENABLED then DeviceLogError(nil,'BCM2708: FramebufferAllocate - FramebufferGetDimensions failed: ' + ErrorToString(Result));
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
    Size:=SizeOf(TBCM2835MailboxHeader) + SizeOf(TBCM2835MailboxTagCreateBuffer) + SizeOf(TBCM2835MailboxFooter);
    
    {Allocate Mailbox Buffer}
    Result:=ERROR_NOT_ENOUGH_MEMORY;
    Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
    if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
    if Header = nil then Exit;
    try
     {Clear Buffer}
     FillChar(Header^,Size,0);
    
     {Setup Header}
     Header.Size:=Size;
     Header.Code:=BCM2835_MBOX_REQUEST_CODE;
    
     {Setup Tag}
     Tag:=PBCM2835MailboxTagCreateBuffer(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));
     
     {Setup Tag (Physical)}
     Tag.Physical.Header.Tag:=BCM2835_MBOX_TAG_SET_PHYSICAL_W_H;
     Tag.Physical.Header.Size:=SizeOf(TBCM2835MailboxTagSetPhysical) - SizeOf(TBCM2835MailboxTagHeader);
     Tag.Physical.Header.Length:=SizeOf(Tag.Physical.Request);
     Tag.Physical.Request.Width:=Defaults.PhysicalWidth;
     Tag.Physical.Request.Height:=Defaults.PhysicalHeight;
     
     {Setup Tag (Virtual)}
     Tag.Vertual.Header.Tag:=BCM2835_MBOX_TAG_SET_VIRTUAL_W_H;
     Tag.Vertual.Header.Size:=SizeOf(TBCM2835MailboxTagSetVirtual) - SizeOf(TBCM2835MailboxTagHeader);
     Tag.Vertual.Header.Length:=SizeOf(Tag.Vertual.Request);
     Tag.Vertual.Request.Width:=Defaults.VirtualWidth;
     Tag.Vertual.Request.Height:=Defaults.VirtualHeight;

     {Setup Tag (Depth)}
     Tag.Depth.Header.Tag:=BCM2835_MBOX_TAG_SET_DEPTH;
     Tag.Depth.Header.Size:=SizeOf(TBCM2835MailboxTagSetDepth) - SizeOf(TBCM2835MailboxTagHeader);
     Tag.Depth.Header.Length:=SizeOf(Tag.Depth.Request);
     Tag.Depth.Request.Depth:=Defaults.Depth;
     
     {Setup Tag (Order)}
     Tag.Order.Header.Tag:=BCM2835_MBOX_TAG_SET_PIXEL_ORDER;
     Tag.Order.Header.Size:=SizeOf(TBCM2835MailboxTagSetPixelOrder) - SizeOf(TBCM2835MailboxTagHeader);
     Tag.Order.Header.Length:=SizeOf(Tag.Order.Request);
     Tag.Order.Request.Order:=Defaults.Order;
     
     {Setup Tag (Mode)}
     Tag.Mode.Header.Tag:=BCM2835_MBOX_TAG_SET_ALPHA_MODE;
     Tag.Mode.Header.Size:=SizeOf(TBCM2835MailboxTagSetAlphaMode) - SizeOf(TBCM2835MailboxTagHeader);
     Tag.Mode.Header.Length:=SizeOf(Tag.Mode.Request);
     Tag.Mode.Request.Mode:=Defaults.Mode;
     
     {Setup Tag (Offset)}
     Tag.Offset.Header.Tag:=BCM2835_MBOX_TAG_SET_VIRTUAL_OFFSET;
     Tag.Offset.Header.Size:=SizeOf(TBCM2835MailboxTagSetVirtualOffset) - SizeOf(TBCM2835MailboxTagHeader);
     Tag.Offset.Header.Length:=SizeOf(Tag.Offset.Request);
     Tag.Offset.Request.X:=Defaults.OffsetX;
     Tag.Offset.Request.Y:=Defaults.OffsetY;
     
     {Setup Tag (Overscan)}
     Tag.Overscan.Header.Tag:=BCM2835_MBOX_TAG_SET_OVERSCAN;
     Tag.Overscan.Header.Size:=SizeOf(TBCM2835MailboxTagSetOverscan) - SizeOf(TBCM2835MailboxTagHeader);
     Tag.Overscan.Header.Length:=SizeOf(Tag.Overscan.Request);
     Tag.Overscan.Request.Top:=Defaults.OverscanTop;
     Tag.Overscan.Request.Bottom:=Defaults.OverscanBottom;
     Tag.Overscan.Request.Left:=Defaults.OverscanLeft;
     Tag.Overscan.Request.Right:=Defaults.OverscanRight;
     
     {Setup Tag (Allocate)}
     Tag.Allocate.Header.Tag:=BCM2835_MBOX_TAG_ALLOCATE_BUFFER;
     Tag.Allocate.Header.Size:=SizeOf(TBCM2835MailboxTagAllocateBuffer) - SizeOf(TBCM2835MailboxTagHeader);
     Tag.Allocate.Header.Length:=SizeOf(Tag.Allocate.Request);
     Tag.Allocate.Request.Alignment:=BCM2708FRAMEBUFFER_ALIGNEMENT;
     
     {Setup Tag (Pitch)}
     Tag.Pitch.Header.Tag:=BCM2835_MBOX_TAG_GET_PITCH;
     Tag.Pitch.Header.Size:=SizeOf(TBCM2835MailboxTagGetPitch) - SizeOf(TBCM2835MailboxTagHeader);
     Tag.Pitch.Header.Length:=SizeOf(Tag.Pitch.Request);
     
     {Setup Footer}
     Footer:=PBCM2835MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2835MailboxTagCreateBuffer)));
     Footer.Tag:=BCM2835_MBOX_TAG_END;
     
     {Call Mailbox}
     Result:=MailboxPropertyCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
     if Result <> ERROR_SUCCESS then
      begin
       if PLATFORM_LOG_ENABLED then PlatformLogError('BCM2708: FramebufferAllocate - MailboxPropertyCall failed: ' + ErrorToString(Result));
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

function BCM2708FramebufferRelease(Framebuffer:PFramebufferDevice):LongWord;
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2835MailboxHeader;
 Footer:PBCM2835MailboxFooter;
 Tag:PBCM2835MailboxTagReleaseBuffer;
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
    Size:=SizeOf(TBCM2835MailboxHeader) + SizeOf(TBCM2835MailboxTagReleaseBuffer) + SizeOf(TBCM2835MailboxFooter);

    {Allocate Mailbox Buffer}
    Result:=ERROR_NOT_ENOUGH_MEMORY;
    Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
    if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
    if Header = nil then Exit;
    try
     {Clear Buffer}
     FillChar(Header^,Size,0);
    
     {Setup Header}
     Header.Size:=Size;
     Header.Code:=BCM2835_MBOX_REQUEST_CODE;
    
     {Setup Tag}
     Tag:=PBCM2835MailboxTagReleaseBuffer(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2835MailboxHeader)));
     Tag.Header.Tag:=BCM2835_MBOX_TAG_RELEASE_BUFFER;
     Tag.Header.Size:=SizeOf(TBCM2835MailboxTagReleaseBuffer) - SizeOf(TBCM2835MailboxTagHeader);
     Tag.Header.Length:=SizeOf(Tag.Request);
    
     {Setup Footer}
     Footer:=PBCM2835MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2835MailboxTagReleaseBuffer)));
     Footer.Tag:=BCM2835_MBOX_TAG_END;
     
     {Call Mailbox}
     Result:=MailboxPropertyCall(BCM2835_MAILBOX_0,BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
     if Result <> ERROR_SUCCESS then
      begin
       if DEVICE_LOG_ENABLED then DeviceLogError(nil,'BCM2708: FramebufferRelease: MailboxPropertyCall failed: ' + ErrorToString(Result));
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

function BCM2708FramebufferSetProperties(Framebuffer:PFramebufferDevice;Properties:PFramebufferProperties):LongWord;
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
{BCM2708 Helper Functions}

{==============================================================================}
{==============================================================================}

initialization
 BCM2708Init;

{==============================================================================}
 
finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
 
 
 
 
 