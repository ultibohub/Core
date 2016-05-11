{
Ultibo BCM2710 interface unit.

Copyright (C) 2016 - SoftOz Pty Ltd.

Arch
====

 <All>

Boards
======

 Raspberry Pi 3 - Model B
 
Licence
=======

 LGPLv2.1 with static linking exception (See COPYING.modifiedLGPL.txt)
 
Credits
=======

 Information for this unit was obtained from:

   Linux MMC/SDHCI drivers
   U-Boot MMC/SDHCI drivers
 
   Linux - \drivers\video\bcm2708_fb.c
   U-Boot - \drivers\video\bcm2835.c 
 
   Linux - \drivers\dma\bcm2708-dmaengine.c - Copyright 2013-2014 Florian Meier and Gellert Weisz
   Linux - \drivers\dma\bcm2835-dma.c - Copyright 2013 Florian Meier
 
References
==========

 BCM2835 ARM Peripherals

 Raspberry Pi Mailboxes
 
  https://github.com/raspberrypi/firmware/wiki/Mailboxes
 
 RPi Low-level peripherals
  
  http://elinux.org/RPi_Low-level_peripherals
  
 RPi SPI
 
  http://elinux.org/RPi_SPI
 
BCM2710 Devices
===============
 
 This unit provides the BCM2710 specific implementations of the following devices:

  SPI
  I2C
  DMA
  PWM
  PCM
  GPIO
  UART0
  UART1
  SDHCI (eMMC)
 
  Clock
  Timer
  Random
  Mailbox
  Watchdog
  Framebuffer
  
  And MIPI CSI-2 (Camera Serial Interface) ?
  And DSI (Display Serial Interface) ?
 
 
BCM2710 SPI Device
==================


BCM2710 I2C Device
==================

 
BCM2710 DMA Device
==================

 The DMA controller has 16 channels in total although not all are available for software to use as some are already used by the GPU.
 
 The firmware will pass the value dma.dmachans on the command line which will indicate which channels are available for our use.
 
 Channels 0 to 6 are normal channels which support 2D stride and transfers up to 1GB per control block
 
 Channels 7 to 14 are Lite channels which do not support stride and only allow transfers up to 64KB per control block

 Channel 15 is not mentioned in most documentation and is shown as not available in the mask passed in dma.dmachans
 
 Channel 0 and 15 are Bulk channels which have an additional FIFO for faster transfers (8 beat burst per read)


BCM2710 PWM Device
==================


BCM2710 PCM Device
==================

 
BCM2710 GPIO Device
===================

 The GPIO has 54 pins available each with multiple alternate functions. All pins can be configured as input or output
 and all can have pull up or down applied.
 
 Not all pins are exposed on the 26 or 40 pin header of the Raspberry Pi, for details of which pins are available see:
 
  Raspberry Pi A and B - https://www.raspberrypi.org/documentation/usage/gpio/README.md
  Raspberry Pi A+/B+/2B/3B/Zero - https://www.raspberrypi.org/documentation/usage/gpio-plus-and-raspi2/README.md
 
 Some of the 54 pins are used for peripheral communication (such as the SD card) and are not available for general use,
 take care when changing function selects on pins to avoid disabling certain system peripherals.
 
 Event detection can be enabled for both high and low levels as well as rising and falling edges, there is also an
 asynchronous rising or falling edge detection which can detect edges of very short duration.

 
BCM2710 UART0 Device
====================

 The UART0 device is an ARM PL011 UART which supports programmable baud rates, start, stop and parity bits and hardware
 flow control and many others. The UART0 is similar to the industry standard 16C650 but with a number of differences, the
 PL011 has a some optional features such as IrDA, Serial InfraRed and DMA which are not supported by the Broadcom implementation.

 In the standard configuration the UART0 TX and RX lines are connected to GPIO pins 14 and 15 respectively (Alternate function
 0) but they can be remapped via GPIO function selects to a number of other locations. On the Raspberry Pi (all models) none of
 these alternate pin mappings are exposed via the 26 or 40 pin header and therefore cannot be used easily. This means that UART0
 and UART1 cannot be used at the same time.
 
 On the Raspberry Pi 3B the UART0 can be mapped to GPIO pins 32 and 33 (Alternate function 3) to communicate with the built in
 Bluetooth module.

 
BCM2710 UART1 Device
====================

 The UART1 device is a Broadcom implementation that is part of the AUX device which also includes the SPI1 and SPI2 devices.
 This device is termed a Mini UART and has a smaller feature set than the PL011 UART but still supports a fairly standard
 communication protocol with programmable baud rate and hardware flow control.
 
 The Mini UART is similar to the standard 16550 device but is missing some of the features, the device also has no DMA support
 so high speed transfers will produce a higher CPU load.

 In the standard configuration the UART1 TX and RX lines are connected to GPIO pins 14 and 15 respectively (Alternate function
 5) but they can be remapped via GPIO function selects to a number of other locations. On the Raspberry Pi (all models) none of
 these alternate pin mappings are exposed via the 26 or 40 pin header and therefore cannot be used easily. This means that UART0
 and UART1 cannot be used at the same time.
 
 On the Raspberry Pi 3B the UART1 can be mapped to GPIO pins 32 and 33 (Alternate function 5) to communicate with the built in
 Bluetooth module.

 
BCM2710 SDHCI Device
====================

 The SDHCI controller on the BCM2710 is an Arasan SD Host controller.

 The Card Detect pin is not connected.

 The Write Protect pin is not connected.


BCM2710 Clock Device
====================


BCM2710 Timer Device
====================


BCM2710 Random Device
=====================


BCM2710 Mailbox Device
======================


BCM2710 Watchdog Device
=======================


BCM2710 Framebuffer Device
==========================
 
 
}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit BCM2710;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,BCM2837,Platform{$IFNDEF CONSOLE_EARLY_INIT},PlatformRPi3{$ENDIF},Threads,HeapManager,Devices,SPI,I2C,DMA,PWM,GPIO,UART,MMC,Framebuffer,SysUtils; 

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
const
 {BCM2710 specific constants}

 {BCM2710 SPI constants}
 
 {BCM2710 I2C constants}
 
 {BCM2710 DMA constants}
 BCM2710_DMA_DESCRIPTION = 'BCM2837 DMA';
 
 BCM2710_DMA_CHANNEL_COUNT = 16;                 {Total number of DMA channels (Not all are usable)}
 
 BCM2710_DMA_LITE_CHANNELS   = $7F80;            {Mask of DMA Lite channels (7 to 14)}
 BCM2710_DMA_NORMAL_CHANNELS = $007E; {807F}     {Mask of normal channels (1 to 6)}
 BCM2710_DMA_BULK_CHANNELS   = $8001;            {Mask of DMA Bulk channels (0 and 15)}
 
 BCM2710_DMA_SHARED_CHANNELS = $7800;            {Mask of channels with shared interrupt (11 to 14)}
 
 BCM2710_DMA_MAX_LITE_TRANSFER   = 65536;        {Maximum transfer length for a DMA Lite channel}
 BCM2710_DMA_MAX_NORMAL_TRANSFER = 1073741824;   {Maximum transfer length for a normal channel}
 
 BCM2710_DMA_MAX_STRIDE   = $FFFF;               {Maximum stride value (Increment between rows) (Note this is a signed value (Min -32768 / Max 32767)}
 BCM2710_DMA_MAX_Y_COUNT  = $3FFF;               {Maximum number of X length transfers in 2D stride}
 BCM2710_DMA_MAX_X_LENGTH = $FFFF;               {Maximum X transfer length in 2D stride}
 
 BCM2710_DMA_CB_ALIGNMENT = 32;                  {Alignement required for DMA control blocks}
 
 BCM2710_DMA_LITE_BURST_LENGTH = 1;              {Burst length for DMA Lite channels}
 BCM2710_DMA_NORMAL_BURST_LENGTH = 2;            {Burst length for normal channels}
 BCM2710_DMA_BULK_BURST_LENGTH = 8;              {Burst length for DMA Bulk channels}
 
 {BCM2710 PWM constants}
 
 {BCM2710 PCM constants}
 
 {BCM2710 GPIO constants}
 BCM2710_GPIO_DESCRIPTION = 'BCM2837 GPIO';
 
 BCM2710_GPIO_MIN_PIN = GPIO_PIN_0;
 BCM2710_GPIO_MAX_PIN = GPIO_PIN_53;
 
 BCM2710_GPIO_MAX_LEVEL = GPIO_LEVEL_HIGH;
 
 BCM2710_GPIO_MAX_PULL = GPIO_PULL_DOWN;
  
 BCM2710_GPIO_MIN_FUNCTION = GPIO_FUNCTION_IN;
 BCM2710_GPIO_MAX_FUNCTION = GPIO_FUNCTION_ALT5;

 BCM2710_GPIO_MIN_TRIGGER = GPIO_TRIGGER_LOW;
 BCM2710_GPIO_MAX_TRIGGER = GPIO_TRIGGER_ASYNC_FALLING;

 BCM2710_GPIO_PULL_MAP:array[GPIO_PULL_NONE..GPIO_PULL_DOWN] of LongWord = (
  {GPIO pull up/down to BCM2837 pull up/down}
  BCM2837_GPPUD_NONE,
  BCM2837_GPPUD_UP,
  BCM2837_GPPUD_DOWN);
 
 BCM2710_GPIO_FUNCTION_MAP:array[BCM2710_GPIO_MIN_FUNCTION..BCM2710_GPIO_MAX_FUNCTION] of LongWord = (
  {GPIO functions to BCM2837 functions}
  BCM2837_GPFSEL_IN,
  BCM2837_GPFSEL_OUT,
  BCM2837_GPFSEL_ALT0,
  BCM2837_GPFSEL_ALT1,
  BCM2837_GPFSEL_ALT2,
  BCM2837_GPFSEL_ALT3,
  BCM2837_GPFSEL_ALT4,
  BCM2837_GPFSEL_ALT5);
 
 BCM2710_GPIO_FUNCTION_UNMAP:array[BCM2710_GPIO_MIN_FUNCTION..BCM2710_GPIO_MAX_FUNCTION] of LongWord = (
  {BCM2837 functions to GPIO functions}
  GPIO_FUNCTION_IN,
  GPIO_FUNCTION_OUT,
  GPIO_FUNCTION_ALT5,
  GPIO_FUNCTION_ALT4,
  GPIO_FUNCTION_ALT0,
  GPIO_FUNCTION_ALT1,
  GPIO_FUNCTION_ALT2,
  GPIO_FUNCTION_ALT3);
  
 BCM2710_GPIO_TRIGGER_MAP:array[BCM2710_GPIO_MIN_TRIGGER..BCM2710_GPIO_MAX_TRIGGER] of LongWord = (
  {GPIO triggers to BCM2837 event registers}
  BCM2837_GPLEN0,
  BCM2837_GPHEN0,
  BCM2837_GPREN0,
  BCM2837_GPFEN0,
  BCM2837_GPAREN0,
  BCM2837_GPAFEN0);
 
 {BCM2710 UART0 (PL011) constants}
 BCM2710_UART0_DESCRIPTION = 'BCM2837 PL011 UART';
 
 BCM2710_UART0_MIN_BAUD = 300;      {Default minimum of 300 baud}
 BCM2710_UART0_MAX_BAUD = 187500;   {Default maximum based on the default settings from the firmware (Recalculated during open)}
 
 BCM2710_UART0_MIN_DATABITS = SERIAL_DATA_5BIT;
 BCM2710_UART0_MAX_DATABITS = SERIAL_DATA_8BIT;
 
 BCM2710_UART0_MIN_STOPBITS = SERIAL_STOP_1BIT;
 BCM2710_UART0_MAX_STOPBITS = SERIAL_STOP_2BIT;
 
 BCM2710_UART0_MAX_PARITY = SERIAL_PARITY_EVEN;
 
 BCM2710_UART0_MAX_FLOW = SERIAL_FLOW_RTS_CTS;
 
 BCM2710_UART0_CLOCK_RATE = 3000000; {Default clock rate based on the default settings from the firmware (Requested from firmware during open)}
 
 {BCM2710 UART1 (AUX) constants}
 BCM2710_UART1_DESCRIPTION = 'BCM2837 AUX (Mini) UART';
 
 {BCM2710 SDHCI constants}
 BCM2710_EMMC_DESCRIPTION = 'BCM2837 Arasan SD Host';
 
 BCM2710_EMMC_MIN_FREQ = 400000;    {Default minimum of 400KHz}
 BCM2710_EMMC_MAX_FREQ = 250000000; //To Do //Get the current frequency from the command line or mailbox instead ? //Peripheral init could get from Mailbox like SMSC95XX ?
 
 {BCM2710 Clock constants}
 
 {BCM2710 Timer constants}
 
 {BCM2710 Random constants}
 BCM2710_RANDOM_WARMUP_COUNT  = $00040000; {The initial numbers generated are "less random" so will be discarded}

 {BCM2710 Mailbox constants}
 
 {BCM2710 Watchdog constants}
 
 {BCM2710 Framebuffer constants}
 
{==============================================================================}
type
 {BCM2710 specific types}
 
 {BCM2710 SPI types}
 
 {BCM2710 I2C types}
 
 {BCM2710 DMA types}
 PBCM2710DMAHost = ^TBCM2710DMAHost;
 
 PBCM2710DMAChannel = ^TBCM2710DMAChannel;
 TBCM2710DMAChannel = record
  Host:PBCM2710DMAHost;            {DMA host this channel belongs to}
  Request:PDMARequest;             {Current DMA request pending on this channel (or nil of no request is pending)} 
  Number:LongWord;                 {The channel number of this channel}
  Interrupt:LongWord;              {The interrupt number of this channel}
  Registers:PBCM2837DMARegisters;  {The channel registers for configuration}
 end;
 
 TBCM2710DMAHost = record
  {DMA Properties}
  DMA:TDMAHost;
  {BCM2710 Properties}
  ChannelMask:LongWord;                                                   {Mask of available channels (Passed from GPU firmware)}
  ChannelFree:LongWord;                                                   {Bitmap of current free channels}
  ChannelLock:TMutexHandle;                                               {Lock for access to ChannelFree}
  ChannelWait:TSemaphoreHandle;                                           {Number of free normal channels in ChannelFree}
  ChannelLite:TSemaphoreHandle;                                           {Number of free DMA Lite channels in ChannelFree}
  ChannelBulk:TSemaphoreHandle;                                           {Number of free DMA Bulk channels in ChannelFree}
  Channels:array[0..BCM2710_DMA_CHANNEL_COUNT - 1] of TBCM2710DMAChannel; {Channel information for each DMA channel on the host}
  EnableRegister:PLongWord;
  InterruptRegister:PLongWord;
  {Statistics Properties}                                        
  InterruptCount:LongWord;                                                {Number of interrupt requests received by the host controller}
 end;
 
 {BCM2710 PWM types}
 
 {BCM2710 PCM types}
 
 {BCM2710 GPIO types}
 PBCM2710GPIODevice = ^TBCM2710GPIODevice;
 
 PBCM2710GPIOBank = ^TBCM2710GPIOBank;
 TBCM2710GPIOBank = record
  GPIO:PGPIODevice;
  Bank:LongWord;
  Address:LongWord;
  PinStart:LongWord;
 end;
 
 TBCM2710GPIODevice = record
  {GPIO Properties}
  GPIO:TGPIODevice;
  {BCM2710 Properties}
  Lock:TSpinHandle;                                                       {Device lock (Differs from lock in Device portion) (Spin lock due to use by interrupt handler)}
  Banks:array[0..BCM2837_GPIO_BANK_COUNT - 1] of TBCM2710GPIOBank;
  {Statistics Properties}                                        
  InterruptCount:LongWord;                                                {Number of interrupt requests received by the device}
 end;
 
 {BCM2710 UART0 types}
 PBCM2710UART0Device = ^TBCM2710UART0Device;
 TBCM2710UART0Device = record
  {UART Properties}
  UART:TUARTDevice;
  {BCM2710 Properties}
  Address:Pointer;                                                        {Device register base address}
  ClockRate:LongWord;                                                     {Device clock rate}
  {Statistics Properties}                                        
  InterruptCount:LongWord;                                                {Number of interrupt requests received by the device}
 end;
 
 {BCM2710 UART1 types}
 
 {BCM2710 SDHCI types}
 PBCM2710SDHCIHost = ^TBCM2710SDHCIHost;
 TBCM2710SDHCIHost = record
  {SDHCI Properties}
  SDHCI:TSDHCIHost;
  {BCM2710 Properties}
  //Lock:TSpinHandle; //To Do //Not Needed ? //See: DWCOTG etc
  WriteDelay:LongWord;
  LastWrite:LongWord;
  ShadowRegister:LongWord;
 end;
 
 {BCM2710 Clock types}
 PBCM2710Clock = ^TBCM2710Clock;
 TBCM2710Clock = record
  {Clock Properties}
  Clock:TClockDevice;
  {BCM2710 Properties}
   {Nothing}
 end; 

 {BCM2710 Timer types}
 PBCM2710Timer = ^TBCM2710Timer;
 TBCM2710Timer = record
  {Timer Properties}
  Timer:TTimerDevice;
  {BCM2710 Properties}
   {Nothing}
 end; 
 
 {BCM2710 Random types}
 PBCM2710Random = ^TBCM2710Random;
 TBCM2710Random = record
  {Random Properties}
  Random:TRandomDevice;
  {BCM2710 Properties}
   {Nothing}
 end; 

 {BCM2710 Mailbox types}
 PBCM2710Mailbox = ^TBCM2710Mailbox;
 TBCM2710Mailbox = record
  {Mailbox Properties}
  Mailbox:TMailboxDevice;
  {BCM2710 Properties}
   {Nothing}
 end; 
 
 {BCM2710 Watchdog types}
 PBCM2710Watchdog = ^TBCM2710Watchdog;
 TBCM2710Watchdog = record
  {Watchdog Properties}
  Watchdog:TWatchdogDevice;
  {BCM2710 Properties}
   {Nothing}
 end; 

 {BCM2710 Framebuffer types}
 PBCM2710Framebuffer = ^TBCM2710Framebuffer;
 TBCM2710Framebuffer = record
  {Framebuffer Properties}
  Framebuffer:TFramebufferDevice;
  {BCM2710 Properties}
   {Nothing}
 end; 
 
{==============================================================================}
{var}
 {BCM2710 specific variables}
 
{==============================================================================}
{Initialization Functions}
procedure BCM2710Init;
 
{==============================================================================}
{BCM2710 Functions}

{==============================================================================}
{BCM2710 SPI Functions}
 
{==============================================================================}
{BCM2710 I2C Functions}

{==============================================================================}
{BCM2710 DMA Functions}
function BCM2710DMAHostStart(DMA:PDMAHost):LongWord;
function BCM2710DMAHostStop(DMA:PDMAHost):LongWord;

function BCM2710DMAHostSubmit(DMA:PDMAHost;Request:PDMARequest):LongWord;
function BCM2710DMAHostCancel(DMA:PDMAHost;Request:PDMARequest):LongWord;

procedure BCM2710DMAInterruptHandler(Channel:PBCM2710DMAChannel);
procedure BCM2710DMASharedInterruptHandler(DMA:PBCM2710DMAHost);

procedure BCM2710DMARequestComplete(Channel:PBCM2710DMAChannel);

function BCM2710DMAPeripheralToDREQ(Peripheral:LongWord):LongWord;
procedure BCM2710DMADataToControlBlock(Request:PDMARequest;Data:PDMAData;Block:PBCM2837DMAControlBlock;Bulk,Lite:Boolean);

{==============================================================================}
{BCM2710 PWM Functions}

{==============================================================================}
{BCM2710 PCM Functions}

{==============================================================================}
{BCM2710 GPIO Functions}
function BCM2710GPIOStart(GPIO:PGPIODevice):LongWord; 
function BCM2710GPIOStop(GPIO:PGPIODevice):LongWord; 
 
function BCM2710GPIOInputGet(GPIO:PGPIODevice;Pin:LongWord):LongWord;
function BCM2710GPIOInputWait(GPIO:PGPIODevice;Pin,Trigger,Timeout:LongWord):LongWord;
function BCM2710GPIOInputEvent(GPIO:PGPIODevice;Pin,Trigger,Flags,Timeout:LongWord;Callback:TGPIOCallback;Data:Pointer):LongWord;
function BCM2710GPIOInputCancel(GPIO:PGPIODevice;Pin:LongWord):LongWord;

function BCM2710GPIOOutputSet(GPIO:PGPIODevice;Pin,Level:LongWord):LongWord;

function BCM2710GPIOPullSelect(GPIO:PGPIODevice;Pin,Mode:LongWord):LongWord;

function BCM2710GPIOFunctionGet(GPIO:PGPIODevice;Pin:LongWord):LongWord;
function BCM2710GPIOFunctionSelect(GPIO:PGPIODevice;Pin,Mode:LongWord):LongWord;

procedure BCM2710GPIOInterruptHandler(Bank:PBCM2710GPIOBank);

procedure BCM2710GPIOEventTrigger(Pin:PGPIOPin);
procedure BCM2710GPIOEventTimeout(Event:PGPIOEvent);

{==============================================================================}
{BCM2710 UART0 Functions}
function BCM2710UART0Open(UART:PUARTDevice;BaudRate,DataBits,StopBits,Parity,FlowControl:LongWord):LongWord;
function BCM2710UART0Close(UART:PUARTDevice):LongWord;
 
function BCM2710UART0Read(UART:PUARTDevice;Buffer:Pointer;Size,Flags:LongWord;var Count:LongWord):LongWord;
function BCM2710UART0Write(UART:PUARTDevice;Buffer:Pointer;Size,Flags:LongWord;var Count:LongWord):LongWord;
 
function BCM2710UART0Status(UART:PUARTDevice):LongWord;

procedure BCM2710UART0InterruptHandler(UART:PUARTDevice);

procedure BCM2710UART0Receive(UART:PUARTDevice);
procedure BCM2710UART0Transmit(UART:PUARTDevice);

{==============================================================================}
{BCM2710 UART1 Functions}

{==============================================================================}
{BCM2710 SDHCI Functions}
function BCM2710SDHCIHostStart(SDHCI:PSDHCIHost):LongWord;
function BCM2710SDHCIHostStop(SDHCI:PSDHCIHost):LongWord;

function BCM2710SDHCIHostReadByte(SDHCI:PSDHCIHost;Reg:LongWord):Byte; 
function BCM2710SDHCIHostReadWord(SDHCI:PSDHCIHost;Reg:LongWord):Word; 
function BCM2710SDHCIHostReadLong(SDHCI:PSDHCIHost;Reg:LongWord):LongWord; 
procedure BCM2710SDHCIHostWriteByte(SDHCI:PSDHCIHost;Reg:LongWord;Value:Byte); 
procedure BCM2710SDHCIHostWriteWord(SDHCI:PSDHCIHost;Reg:LongWord;Value:Word); 
procedure BCM2710SDHCIHostWriteLong(SDHCI:PSDHCIHost;Reg:LongWord;Value:LongWord); 
 
procedure BCM2710SDHCIInterruptHandler(SDHCI:PSDHCIHost);
function BCM2710SDHCISetupInterrupts(SDHCI:PSDHCIHost):LongWord;

function BCM2710MMCDeviceGetCardDetect(MMC:PMMCDevice):LongWord;
 
{==============================================================================}
{BCM2710 Clock Functions}
function BCM2710ClockRead(Clock:PClockDevice):LongWord;
function BCM2710ClockRead64(Clock:PClockDevice):Int64;

{==============================================================================}
{BCM2710 Timer Functions}
//To Do

{==============================================================================}
{BCM2710 Random Functions}
function BCM2710RandomStart(Random:PRandomDevice):LongWord;
function BCM2710RandomStop(Random:PRandomDevice):LongWord;

function BCM2710RandomReadLongWord(Random:PRandomDevice):LongWord;

{==============================================================================}
{BCM2710 Mailbox Functions}
//To Do

{==============================================================================}
{BCM2710 Watchdog Functions}
function BCM2710WatchdogStart(Watchdog:PWatchdogDevice):LongWord;
function BCM2710WatchdogStop(Watchdog:PWatchdogDevice):LongWord;
function BCM2710WatchdogRefresh(Watchdog:PWatchdogDevice):LongWord;

function BCM2710WatchdogGetRemain(Watchdog:PWatchdogDevice):LongWord;

{==============================================================================}
{BCM2710 Framebuffer Functions}
function BCM2710FramebufferAllocate(Framebuffer:PFramebufferDevice;Properties:PFramebufferProperties):LongWord;
function BCM2710FramebufferRelease(Framebuffer:PFramebufferDevice):LongWord;

function BCM2710FramebufferSetProperties(Framebuffer:PFramebufferDevice;Properties:PFramebufferProperties):LongWord;

{==============================================================================}
{BCM2710 Helper Functions}
  
{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {BCM2710 specific variables}
 BCM2710Initialized:Boolean;

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure BCM2710Init;
var
 Status:LongWord;
 
 BCM2710DMAHost:PBCM2710DMAHost;
 BCM2710SDHCIHost:PBCM2710SDHCIHost; 

 BCM2710GPIO:PBCM2710GPIODevice;
 BCM2710UART0:PBCM2710UART0Device;
 
 BCM2710Clock:PBCM2710Clock;
 BCM2710Timer:PBCM2710Timer;
 BCM2710Random:PBCM2710Random;
 BCM2710Mailbox:PBCM2710Mailbox;
 BCM2710Watchdog:PBCM2710Watchdog;
 BCM2710Framebuffer:PBCM2710Framebuffer;
begin
 {}
 {Check Initialized}
 if BCM2710Initialized then Exit;
 
 {Initialize BCM2710SDHCI_FIQ_ENABLED}
 if not(FIQ_ENABLED) then BCM2710SDHCI_FIQ_ENABLED:=False;
 
 {$IFNDEF CONSOLE_EARLY_INIT}
 {Register Platform GPU Memory Handlers}
 GPUMemoryAllocateHandler:=RPi3GPUMemoryAllocate;
 GPUMemoryReleaseHandler:=RPi3GPUMemoryRelease;
 GPUMemoryLockHandler:=RPi3GPUMemoryLock;
 GPUMemoryUnlockHandler:=RPi3GPUMemoryUnlock;
 
 {Register Platform GPU Misc Handlers}
 GPUExecuteCodeHandler:=RPi3GPUExecuteCode;
 DispmanxHandleGetHandler:=RPi3DispmanxHandleGet;
 EDIDBlockGetHandler:=RPi3EDIDBlockGet;

 {Register Platform Framebuffer Handlers}
 FramebufferAllocateHandler:=RPi3FramebufferAllocate;
 FramebufferReleaseHandler:=RPi3FramebufferRelease;
 FramebufferSetStateHandler:=RPi3FramebufferSetState;

 FramebufferGetDimensionsHandler:=RPi3FramebufferGetDimensions;
 
 FramebufferGetPhysicalHandler:=RPi3FramebufferGetPhysical;
 FramebufferSetPhysicalHandler:=RPi3FramebufferSetPhysical;
 FramebufferTestPhysicalHandler:=RPi3FramebufferTestPhysical;
 
 FramebufferGetVirtualHandler:=RPi3FramebufferGetVirtual;
 FramebufferSetVirtualHandler:=RPi3FramebufferSetVirtual;
 FramebufferTestVirtualHandler:=RPi3FramebufferTestVirtual;
 
 FramebufferGetDepthHandler:=RPi3FramebufferGetDepth;
 FramebufferSetDepthHandler:=RPi3FramebufferSetDepth;
 FramebufferTestDepthHandler:=RPi3FramebufferTestDepth;
 
 FramebufferGetPixelOrderHandler:=RPi3FramebufferGetPixelOrder;
 FramebufferSetPixelOrderHandler:=RPi3FramebufferSetPixelOrder;
 FramebufferTestPixelOrderHandler:=RPi3FramebufferTestPixelOrder;
 
 FramebufferGetAlphaModeHandler:=RPi3FramebufferGetAlphaMode;
 FramebufferSetAlphaModeHandler:=RPi3FramebufferSetAlphaMode;
 FramebufferTestAlphaModeHandler:=RPi3FramebufferTestAlphaMode;
 
 FramebufferGetPitchHandler:=RPi3FramebufferGetPitch;
 
 FramebufferGetOffsetHandler:=RPi3FramebufferGetOffset;
 FramebufferSetOffsetHandler:=RPi3FramebufferSetOffset;
 FramebufferTestOffsetHandler:=RPi3FramebufferTestOffset;
 
 FramebufferGetOverscanHandler:=RPi3FramebufferGetOverscan;
 FramebufferSetOverscanHandler:=RPi3FramebufferSetOverscan;
 FramebufferTestOverscanHandler:=RPi3FramebufferTestOverscan;
 
 FramebufferGetPaletteHandler:=RPi3FramebufferGetPalette;
 FramebufferSetPaletteHandler:=RPi3FramebufferSetPalette;
 FramebufferTestPaletteHandler:=RPi3FramebufferTestPalette;

 {Register Platform Cursor Handlers}
 CursorSetInfoHandler:=RPi3CursorSetInfo;
 CursorSetStateHandler:=RPi3CursorSetState;
 {$ENDIF}
 
 {Create SPI}
 if BCM2710_REGISTER_SPI then
  begin
   //To Do
  end; 
 
 {Create I2C}
 if BCM2710_REGISTER_I2C then
  begin
   //To Do
  end;
 
 {Create DMA}
 if BCM2710_REGISTER_DMA then
  begin
   BCM2710DMAHost:=PBCM2710DMAHost(DMAHostCreateEx(SizeOf(TBCM2710DMAHost)));
   if BCM2710DMAHost <> nil then
    begin
     {Update DMA}
     {Device}
     BCM2710DMAHost.DMA.Device.DeviceBus:=DEVICE_BUS_MMIO; 
     BCM2710DMAHost.DMA.Device.DeviceType:=DMA_TYPE_NONE;
     BCM2710DMAHost.DMA.Device.DeviceFlags:=DMA_FLAG_STRIDE or DMA_FLAG_DREQ or DMA_FLAG_NOINCREMENT or DMA_FLAG_NOREAD or DMA_FLAG_NOWRITE or DMA_FLAG_WIDE;
     BCM2710DMAHost.DMA.Device.DeviceData:=nil;
     BCM2710DMAHost.DMA.Device.DeviceDescription:=BCM2710_DMA_DESCRIPTION;
     if BCM2710DMA_SHARED_MEMORY then BCM2710DMAHost.DMA.Device.DeviceFlags:=BCM2710DMAHost.DMA.Device.DeviceFlags or DMA_FLAG_SHARED;
     if BCM2710DMA_NOCACHE_MEMORY then BCM2710DMAHost.DMA.Device.DeviceFlags:=BCM2710DMAHost.DMA.Device.DeviceFlags or DMA_FLAG_NOCACHE;
     if BCM2710DMA_CACHE_COHERENT then BCM2710DMAHost.DMA.Device.DeviceFlags:=BCM2710DMAHost.DMA.Device.DeviceFlags or DMA_FLAG_COHERENT;
     {DMA}
     BCM2710DMAHost.DMA.DMAState:=DMA_STATE_DISABLED;
     BCM2710DMAHost.DMA.HostStart:=BCM2710DMAHostStart;
     BCM2710DMAHost.DMA.HostStop:=BCM2710DMAHostStop;
     BCM2710DMAHost.DMA.HostReset:=nil;
     BCM2710DMAHost.DMA.HostSubmit:=BCM2710DMAHostSubmit;
     BCM2710DMAHost.DMA.HostCancel:=BCM2710DMAHostCancel;
     BCM2710DMAHost.DMA.HostProperties:=nil;
     BCM2710DMAHost.DMA.Alignment:=BCM2710DMA_ALIGNMENT;
     BCM2710DMAHost.DMA.Multiplier:=BCM2710DMA_MULTIPLIER;
     BCM2710DMAHost.DMA.Properties.Flags:=BCM2710DMAHost.DMA.Device.DeviceFlags;
     BCM2710DMAHost.DMA.Properties.Alignment:=BCM2710DMAHost.DMA.Alignment;
     BCM2710DMAHost.DMA.Properties.Multiplier:=BCM2710DMAHost.DMA.Multiplier;
     BCM2710DMAHost.DMA.Properties.Channels:=BCM2710_DMA_CHANNEL_COUNT;
     BCM2710DMAHost.DMA.Properties.MaxSize:=BCM2710_DMA_MAX_NORMAL_TRANSFER;
     BCM2710DMAHost.DMA.Properties.MaxCount:=BCM2710_DMA_MAX_Y_COUNT;
     BCM2710DMAHost.DMA.Properties.MaxLength:=BCM2710_DMA_MAX_X_LENGTH;
     BCM2710DMAHost.DMA.Properties.MinStride:=-32768;
     BCM2710DMAHost.DMA.Properties.MaxStride:=32767;
     {BCM2710}
     BCM2710DMAHost.ChannelLock:=INVALID_HANDLE_VALUE;
     BCM2710DMAHost.ChannelWait:=INVALID_HANDLE_VALUE;
     BCM2710DMAHost.ChannelLite:=INVALID_HANDLE_VALUE;
     BCM2710DMAHost.ChannelBulk:=INVALID_HANDLE_VALUE;
     
     {Register DMA}
     Status:=DMAHostRegister(@BCM2710DMAHost.DMA);
     if Status = ERROR_SUCCESS then
      begin
       {Start DMA}
       Status:=DMAHostStart(@BCM2710DMAHost.DMA);
       if Status <> ERROR_SUCCESS then
        begin
         if DMA_LOG_ENABLED then DMALogError(nil,'BCM2710: Failed to start new DMA host: ' + ErrorToString(Status));
        end;
      end
     else
      begin
       if DMA_LOG_ENABLED then DMALogError(nil,'BCM2710: Failed to register new DMA host: ' + ErrorToString(Status));
      end;
    end
   else 
    begin
     if DMA_LOG_ENABLED then DMALogError(nil,'BCM2710: Failed to create new DMA host');
    end;
  end;
  
 {Create PWM}
 if BCM2710_REGISTER_PWM then
  begin
   //To Do
  end;
  
 {Create PCM}
 if BCM2710_REGISTER_PCM then
  begin
   //To Do
  end;
  
 {Create GPIO}
 if BCM2710_REGISTER_GPIO then
  begin
   BCM2710GPIO:=PBCM2710GPIODevice(GPIODeviceCreateEx(SizeOf(TBCM2710GPIODevice)));
   if BCM2710GPIO <> nil then
    begin
     {Update GPIO}
     {Device}
     BCM2710GPIO.GPIO.Device.DeviceBus:=DEVICE_BUS_MMIO; 
     BCM2710GPIO.GPIO.Device.DeviceType:=GPIO_TYPE_NONE;
     BCM2710GPIO.GPIO.Device.DeviceFlags:=GPIO_FLAG_PULL_UP or GPIO_FLAG_PULL_DOWN or GPIO_FLAG_TRIGGER_LOW or GPIO_FLAG_TRIGGER_HIGH or GPIO_FLAG_TRIGGER_RISING or GPIO_FLAG_TRIGGER_FALLING or GPIO_FLAG_TRIGGER_ASYNC or GPIO_FLAG_TRIGGER_EDGE;
     BCM2710GPIO.GPIO.Device.DeviceData:=nil;
     BCM2710GPIO.GPIO.Device.DeviceDescription:=BCM2710_GPIO_DESCRIPTION;
     {GPIO}
     BCM2710GPIO.GPIO.GPIOState:=GPIO_STATE_DISABLED;
     BCM2710GPIO.GPIO.DeviceStart:=BCM2710GPIOStart;
     BCM2710GPIO.GPIO.DeviceStop:=BCM2710GPIOStop;
     BCM2710GPIO.GPIO.DeviceInputGet:=BCM2710GPIOInputGet;
     BCM2710GPIO.GPIO.DeviceInputWait:=BCM2710GPIOInputWait; 
     BCM2710GPIO.GPIO.DeviceInputEvent:=BCM2710GPIOInputEvent;
     BCM2710GPIO.GPIO.DeviceInputCancel:=BCM2710GPIOInputCancel;
     BCM2710GPIO.GPIO.DeviceOutputSet:=BCM2710GPIOOutputSet;
     BCM2710GPIO.GPIO.DevicePullSelect:=BCM2710GPIOPullSelect;  
     BCM2710GPIO.GPIO.DeviceFunctionGet:=BCM2710GPIOFunctionGet;
     BCM2710GPIO.GPIO.DeviceFunctionSelect:=BCM2710GPIOFunctionSelect;    
     {Driver}
     BCM2710GPIO.GPIO.Address:=Pointer(BCM2837_GPIO_REGS_BASE);
     BCM2710GPIO.GPIO.Properties.Flags:=BCM2710GPIO.GPIO.Device.DeviceFlags;
     BCM2710GPIO.GPIO.Properties.PinMin:=BCM2710_GPIO_MIN_PIN;
     BCM2710GPIO.GPIO.Properties.PinMax:=BCM2710_GPIO_MAX_PIN;
     BCM2710GPIO.GPIO.Properties.PinCount:=BCM2837_GPIO_PIN_COUNT;
     BCM2710GPIO.GPIO.Properties.FunctionMin:=BCM2710_GPIO_MIN_FUNCTION;
     BCM2710GPIO.GPIO.Properties.FunctionMax:=BCM2710_GPIO_MAX_FUNCTION;
     BCM2710GPIO.GPIO.Properties.FunctionCount:=8;
     {BCM2710}
     BCM2710GPIO.Lock:=INVALID_HANDLE_VALUE;
     
     {Register GPIO}
     Status:=GPIODeviceRegister(@BCM2710GPIO.GPIO);
     if Status = ERROR_SUCCESS then
      begin
       {Start GPIO}
       Status:=GPIODeviceStart(@BCM2710GPIO.GPIO);
       if Status <> ERROR_SUCCESS then
        begin
         if GPIO_LOG_ENABLED then GPIOLogError(nil,'BCM2710: Failed to start new GPIO device: ' + ErrorToString(Status));
        end;
      end
     else
      begin
       if GPIO_LOG_ENABLED then GPIOLogError(nil,'BCM2710: Failed to register new GPIO device: ' + ErrorToString(Status));
      end;
    end
   else 
    begin
     if GPIO_LOG_ENABLED then GPIOLogError(nil,'BCM2710: Failed to create new GPIO device');
    end;
  end;

 {Create UART0}
 if BCM2710_REGISTER_UART0 then
  begin
   BCM2710UART0:=PBCM2710UART0Device(UARTDeviceCreateEx(SizeOf(TBCM2710UART0Device)));
   if BCM2710UART0 <> nil then
    begin
     {Update UART0}
     {Device}
     BCM2710UART0.UART.Device.DeviceBus:=DEVICE_BUS_MMIO; 
     BCM2710UART0.UART.Device.DeviceType:=UART_TYPE_16650;
     BCM2710UART0.UART.Device.DeviceFlags:=UART_FLAG_DATA_8BIT or UART_FLAG_DATA_7BIT or UART_FLAG_DATA_6BIT or UART_FLAG_DATA_5BIT or UART_FLAG_STOP_1BIT or UART_FLAG_STOP_2BIT or UART_FLAG_PARITY_ODD or UART_FLAG_PARITY_EVEN or UART_FLAG_FLOW_RTS_CTS;
     BCM2710UART0.UART.Device.DeviceData:=nil;
     BCM2710UART0.UART.Device.DeviceDescription:=BCM2710_UART0_DESCRIPTION;
     {UART}
     BCM2710UART0.UART.UARTMode:=UART_MODE_NONE;
     BCM2710UART0.UART.UARTState:=UART_STATE_DISABLED;
     BCM2710UART0.UART.UARTStatus:=UART_STATUS_NONE;
     BCM2710UART0.UART.DeviceOpen:=BCM2710UART0Open;
     BCM2710UART0.UART.DeviceClose:=BCM2710UART0Close;
     BCM2710UART0.UART.DeviceRead:=BCM2710UART0Read;
     BCM2710UART0.UART.DeviceWrite:=BCM2710UART0Write;
     BCM2710UART0.UART.DeviceStatus:=BCM2710UART0Status;
     {Driver}
     BCM2710UART0.UART.Properties.Flags:=BCM2710UART0.UART.Device.DeviceFlags;
     BCM2710UART0.UART.Properties.MinRate:=BCM2710_UART0_MIN_BAUD;
     BCM2710UART0.UART.Properties.MaxRate:=BCM2710_UART0_MAX_BAUD;
     BCM2710UART0.UART.Properties.BaudRate:=SERIAL_BAUD_RATE_DEFAULT;
     BCM2710UART0.UART.Properties.DataBits:=SERIAL_DATA_8BIT;
     BCM2710UART0.UART.Properties.StopBits:=SERIAL_STOP_1BIT;
     BCM2710UART0.UART.Properties.Parity:=SERIAL_PARITY_NONE;
     BCM2710UART0.UART.Properties.FlowControl:=SERIAL_FLOW_NONE;
     {BCM2710}
     BCM2710UART0.Address:=Pointer(BCM2837_PL011_REGS_BASE);
     BCM2710UART0.ClockRate:=BCM2710_UART0_CLOCK_RATE;
     
     {Register UART0}
     Status:=UARTDeviceRegister(@BCM2710UART0.UART);
     if Status <> ERROR_SUCCESS then
      begin
       if UART_LOG_ENABLED then UARTLogError(nil,'BCM2710: Failed to register new UART0 device: ' + ErrorToString(Status));
      end;
    end
   else 
    begin
     if UART_LOG_ENABLED then UARTLogError(nil,'BCM2710: Failed to create new UART0 device');
    end;
  end;

 {Create UART1}
 if BCM2710_REGISTER_UART1 then
  begin
   //To Do
  end;
  
 {Create SDHCI}
 if BCM2710_REGISTER_SDHCI then
  begin
   BCM2710SDHCIHost:=PBCM2710SDHCIHost(SDHCIHostCreateEx(SizeOf(TBCM2710SDHCIHost)));
   if BCM2710SDHCIHost <> nil then
    begin
     {Update SDHCI}
     {Device}
     BCM2710SDHCIHost.SDHCI.Device.DeviceBus:=DEVICE_BUS_MMIO; 
     BCM2710SDHCIHost.SDHCI.Device.DeviceType:=SDHCI_TYPE_NONE;
     BCM2710SDHCIHost.SDHCI.Device.DeviceFlags:=SDHCI_FLAG_NONE;
     BCM2710SDHCIHost.SDHCI.Device.DeviceData:=nil;
     BCM2710SDHCIHost.SDHCI.Device.DeviceDescription:=BCM2710_EMMC_DESCRIPTION;
     {SDHCI}
     BCM2710SDHCIHost.SDHCI.SDHCIState:=SDHCI_STATE_DISABLED;
     BCM2710SDHCIHost.SDHCI.HostStart:=BCM2710SDHCIHostStart;
     BCM2710SDHCIHost.SDHCI.HostStop:=BCM2710SDHCIHostStop;
     BCM2710SDHCIHost.SDHCI.HostReadByte:=BCM2710SDHCIHostReadByte;
     BCM2710SDHCIHost.SDHCI.HostReadWord:=BCM2710SDHCIHostReadWord;
     BCM2710SDHCIHost.SDHCI.HostReadLong:=BCM2710SDHCIHostReadLong;
     BCM2710SDHCIHost.SDHCI.HostWriteByte:=BCM2710SDHCIHostWriteByte;
     BCM2710SDHCIHost.SDHCI.HostWriteWord:=BCM2710SDHCIHostWriteWord;
     BCM2710SDHCIHost.SDHCI.HostWriteLong:=BCM2710SDHCIHostWriteLong;
     BCM2710SDHCIHost.SDHCI.HostSetClockDivider:=nil;
     BCM2710SDHCIHost.SDHCI.HostSetControlRegister:=nil;
     BCM2710SDHCIHost.SDHCI.DeviceInitialize:=nil;
     BCM2710SDHCIHost.SDHCI.DeviceDeinitialize:=nil;
     BCM2710SDHCIHost.SDHCI.DeviceGetCardDetect:=BCM2710MMCDeviceGetCardDetect;
     BCM2710SDHCIHost.SDHCI.DeviceGetWriteProtect:=nil;
     BCM2710SDHCIHost.SDHCI.DeviceSendCommand:=nil;
     BCM2710SDHCIHost.SDHCI.DeviceSetIOS:=nil;
     {Driver}
     BCM2710SDHCIHost.SDHCI.Address:=Pointer(BCM2837_SDHCI_REGS_BASE);
   
     {Register SDHCI}
     Status:=SDHCIHostRegister(@BCM2710SDHCIHost.SDHCI);
     if Status <> ERROR_SUCCESS then
      begin
       if MMC_LOG_ENABLED then MMCLogError(nil,'BCM2710: Failed to register new SDHCI host: ' + ErrorToString(Status));
      end;
    end
   else 
    begin
     if MMC_LOG_ENABLED then MMCLogError(nil,'BCM2710: Failed to create new SDHCI host');
    end;
  end;

 {Create Clock}
 if BCM2710_REGISTER_CLOCK then
  begin
   BCM2710Clock:=PBCM2710Clock(ClockDeviceCreateEx(SizeOf(TBCM2710Clock)));
   if BCM2710Clock <> nil then
    begin
     {Update Clock}
     {Device}
     BCM2710Clock.Clock.Device.DeviceBus:=DEVICE_BUS_MMIO; 
     BCM2710Clock.Clock.Device.DeviceType:=CLOCK_TYPE_HARDWARE;
     BCM2710Clock.Clock.Device.DeviceFlags:=CLOCK_FLAG_NONE;
     BCM2710Clock.Clock.Device.DeviceData:=nil;
     {Clock}
     BCM2710Clock.Clock.ClockState:=CLOCK_STATE_DISABLED;
     BCM2710Clock.Clock.DeviceRead:=BCM2710ClockRead;
     BCM2710Clock.Clock.DeviceRead64:=BCM2710ClockRead64;
     {Driver}
     BCM2710Clock.Clock.Address:=Pointer(BCM2837_SYSTEM_TIMER_REGS_BASE);
     BCM2710Clock.Clock.Rate:=BCM2837_SYSTEM_TIMER_FREQUENCY;
    
     {Register Clock}
     Status:=ClockDeviceRegister(@BCM2710Clock.Clock);
     if Status = ERROR_SUCCESS then
      begin
       {Start Clock}
       Status:=ClockDeviceStart(@BCM2710Clock.Clock);
       if Status <> ERROR_SUCCESS then
        begin
         if DEVICE_LOG_ENABLED then DeviceLogError(nil,'BCM2710: Failed to start new clock device: ' + ErrorToString(Status));
        end;
      end
     else 
      begin
       if DEVICE_LOG_ENABLED then DeviceLogError(nil,'BCM2710: Failed to register new clock device: ' + ErrorToString(Status));
      end;
    end
   else 
    begin
     if DEVICE_LOG_ENABLED then DeviceLogError(nil,'BCM2710: Failed to create new clock device');
    end;
  end;
  
 {Create Timer}
 if BCM2710_REGISTER_TIMER then
  begin
   //To Do
  end; 
 
 {Create Random}
 if BCM2710_REGISTER_RANDOM then
  begin
   BCM2710Random:=PBCM2710Random(RandomDeviceCreateEx(SizeOf(TBCM2710Random)));
   if BCM2710Random <> nil then
    begin
     {Update Random}
     {Device}
     BCM2710Random.Random.Device.DeviceBus:=DEVICE_BUS_MMIO; 
     BCM2710Random.Random.Device.DeviceType:=RANDOM_TYPE_HARDWARE;
     BCM2710Random.Random.Device.DeviceFlags:=RANDOM_FLAG_NONE;
     BCM2710Random.Random.Device.DeviceData:=nil;
     {Random}
     BCM2710Random.Random.RandomState:=RANDOM_STATE_DISABLED;
     BCM2710Random.Random.DeviceStart:=BCM2710RandomStart;
     BCM2710Random.Random.DeviceStop:=BCM2710RandomStop;
     BCM2710Random.Random.DeviceReadLongWord:=BCM2710RandomReadLongWord;
     {Driver}
     BCM2710Random.Random.Address:=Pointer(BCM2837_RNG_REGS_BASE);
     
     {Register Random}
     Status:=RandomDeviceRegister(@BCM2710Random.Random);
     if Status = ERROR_SUCCESS then
      begin
       {Start Random}
       Status:=RandomDeviceStart(@BCM2710Random.Random);
       if Status <> ERROR_SUCCESS then
        begin
         if DEVICE_LOG_ENABLED then DeviceLogError(nil,'BCM2710: Failed to start new random device: ' + ErrorToString(Status));
        end;
      end
     else 
      begin
       if DEVICE_LOG_ENABLED then DeviceLogError(nil,'BCM2710: Failed to register new random device: ' + ErrorToString(Status));
      end;
    end
   else 
    begin
     if DEVICE_LOG_ENABLED then DeviceLogError(nil,'BCM2710: Failed to create new random device');
    end;
  end;
  
 {Create Mailbox}
 if BCM2710_REGISTER_MAILBOX then
  begin
   //To Do
  end; 
  
 {Create Watchdog}
 if BCM2710_REGISTER_WATCHDOG then
  begin
   BCM2710Watchdog:=PBCM2710Watchdog(WatchdogDeviceCreateEx(SizeOf(TBCM2710Watchdog)));
   if BCM2710Watchdog <> nil then
    begin
     {Device}
     BCM2710Watchdog.Watchdog.Device.DeviceBus:=DEVICE_BUS_MMIO; 
     BCM2710Watchdog.Watchdog.Device.DeviceType:=WATCHDOG_TYPE_HARDWARE;
     BCM2710Watchdog.Watchdog.Device.DeviceFlags:=WATCHDOG_FLAG_NONE;
     BCM2710Watchdog.Watchdog.Device.DeviceData:=nil;
     {Watchdog}
     BCM2710Watchdog.Watchdog.WatchdogState:=WATCHDOG_STATE_DISABLED;
     BCM2710Watchdog.Watchdog.DeviceStart:=BCM2710WatchdogStart;
     BCM2710Watchdog.Watchdog.DeviceStop:=BCM2710WatchdogStop;
     BCM2710Watchdog.Watchdog.DeviceRefresh:=BCM2710WatchdogRefresh;
     BCM2710Watchdog.Watchdog.DeviceGetRemain:=BCM2710WatchdogGetRemain;
     {Driver}
     BCM2710Watchdog.Watchdog.Address:=Pointer(BCM2837_PM_REGS_BASE);
     
     {Register Watchdog}
     Status:=WatchdogDeviceRegister(@BCM2710Watchdog.Watchdog);
     if Status <> ERROR_SUCCESS then
      begin
       if DEVICE_LOG_ENABLED then DeviceLogError(nil,'BCM2710: Failed to register new watchdog device: ' + ErrorToString(Status));
      end;
    end
   else 
    begin
     if DEVICE_LOG_ENABLED then DeviceLogError(nil,'BCM2710: Failed to create new watchdog device');
    end;
  end;
 
 {$IFNDEF CONSOLE_EARLY_INIT}
 {Create Framebuffer}
 if BCM2710_REGISTER_FRAMEBUFFER then
  begin
   BCM2710Framebuffer:=PBCM2710Framebuffer(FramebufferDeviceCreateEx(SizeOf(TBCM2710Framebuffer)));
   if BCM2710Framebuffer <> nil then
    begin
     {Device}
     BCM2710Framebuffer.Framebuffer.Device.DeviceBus:=DEVICE_BUS_MMIO; 
     BCM2710Framebuffer.Framebuffer.Device.DeviceType:=FRAMEBUFFER_TYPE_HARDWARE;
     BCM2710Framebuffer.Framebuffer.Device.DeviceFlags:=FRAMEBUFFER_FLAG_NONE;
     BCM2710Framebuffer.Framebuffer.Device.DeviceData:=nil;
     {Framebuffer}
     BCM2710Framebuffer.Framebuffer.FramebufferState:=FRAMEBUFFER_STATE_DISABLED;
     BCM2710Framebuffer.Framebuffer.DeviceAllocate:=BCM2710FramebufferAllocate;
     BCM2710Framebuffer.Framebuffer.DeviceRelease:=BCM2710FramebufferRelease;
     BCM2710Framebuffer.Framebuffer.DeviceSetProperties:=BCM2710FramebufferSetProperties;
     {Driver}
     
     {Setup Flags}
     if BCM2710FRAMEBUFFER_CACHED then BCM2710Framebuffer.Framebuffer.Device.DeviceFlags:=BCM2710Framebuffer.Framebuffer.Device.DeviceFlags or FRAMEBUFFER_FLAG_CACHED;
     if SysUtils.GetEnvironmentVariable('bcm2708_fb.fbswap') <> '1' then BCM2710Framebuffer.Framebuffer.Device.DeviceFlags:=BCM2710Framebuffer.Framebuffer.Device.DeviceFlags or FRAMEBUFFER_FLAG_SWAP;
     
     {Register Framebuffer}
     Status:=FramebufferDeviceRegister(@BCM2710Framebuffer.Framebuffer);
     if Status = ERROR_SUCCESS then
      begin
       {Allocate Framebuffer}
       Status:=FramebufferDeviceAllocate(@BCM2710Framebuffer.Framebuffer,nil);
       if Status <> ERROR_SUCCESS then
        begin
         if DEVICE_LOG_ENABLED then DeviceLogError(nil,'BCM2710: Failed to allocate new framebuffer device: ' + ErrorToString(Status));
        end;
      end
     else
      begin     
       if DEVICE_LOG_ENABLED then DeviceLogError(nil,'BCM2710: Failed to register new framebuffer device: ' + ErrorToString(Status));
      end;
    end
   else 
    begin
     if DEVICE_LOG_ENABLED then DeviceLogError(nil,'BCM2710: Failed to create new framebuffer device');
    end;
  end;
 {$ENDIF}
 
 BCM2710Initialized:=True;
end;
 
{==============================================================================}
{==============================================================================}
{BCM2710 Functions}

{==============================================================================}
{==============================================================================}
{BCM2710 SPI Functions}
 
{==============================================================================}
{==============================================================================}
{BCM2710 I2C Functions}

{==============================================================================}
{==============================================================================}
{BCM2710 DMA Functions}
function BCM2710DMAHostStart(DMA:PDMAHost):LongWord;
var
 Mask:LongWord;
 Count:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check DMA}
 if DMA = nil then Exit;
 
 {Get Channel Mask}
 PBCM2710DMAHost(DMA).ChannelMask:=DMAGetChannels;

 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(DMA_DEBUG)}
 if DMA_LOG_ENABLED then DMALogDebug(nil,'BCM2710: Channel mask = ' + IntToHex(PBCM2710DMAHost(DMA).ChannelMask,8));
 {$ENDIF}
 
 {Get Channel Free}
 PBCM2710DMAHost(DMA).ChannelFree:=PBCM2710DMAHost(DMA).ChannelMask;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(DMA_DEBUG)}
 if DMA_LOG_ENABLED then DMALogDebug(nil,'BCM2710: Channel free = ' + IntToHex(PBCM2710DMAHost(DMA).ChannelFree,8));
 {$ENDIF}
 
 {Create Channel Lock}
 PBCM2710DMAHost(DMA).ChannelLock:=MutexCreateEx(False,MUTEX_DEFAULT_SPINCOUNT,MUTEX_FLAG_RECURSIVE);
 if PBCM2710DMAHost(DMA).ChannelLock = INVALID_HANDLE_VALUE then
  begin
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;
  
 {Count Free Normal Channels}
 Mask:=(PBCM2710DMAHost(DMA).ChannelMask and BCM2710_DMA_NORMAL_CHANNELS);
 Count:=0;
 while Mask <> 0 do
  begin
   if (Mask and 1) <> 0 then
    begin
     Inc(Count);
    end;
   
   Mask:=(Mask shr 1);
  end;
  
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(DMA_DEBUG)}
 if DMA_LOG_ENABLED then DMALogDebug(nil,'BCM2710: Normal channel free count = ' + IntToStr(Count));
 {$ENDIF}
  
 {Create Normal Channel Semaphore}
 PBCM2710DMAHost(DMA).ChannelWait:=SemaphoreCreate(Count);
 if PBCM2710DMAHost(DMA).ChannelWait = INVALID_HANDLE_VALUE then
  begin
   {Destroy Channel Lock}
   MutexDestroy(PBCM2710DMAHost(DMA).ChannelLock);
   
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;

 {Count Free DMA Lite Channels}
 Mask:=(PBCM2710DMAHost(DMA).ChannelMask and BCM2710_DMA_LITE_CHANNELS);
 Count:=0;
 while Mask <> 0 do
  begin
   if (Mask and 1) <> 0 then
    begin
     Inc(Count);
    end;
   
   Mask:=(Mask shr 1);
  end;
  
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(DMA_DEBUG)}
 if DMA_LOG_ENABLED then DMALogDebug(nil,'BCM2710: DMA Lite channel free count = ' + IntToStr(Count));
 {$ENDIF}
  
 {Create DMA Lite Channel Semaphore}
 PBCM2710DMAHost(DMA).ChannelLite:=SemaphoreCreate(Count);
 if PBCM2710DMAHost(DMA).ChannelLite = INVALID_HANDLE_VALUE then
  begin
   {Destroy Normal Channel Semaphore}
   SemaphoreDestroy(PBCM2710DMAHost(DMA).ChannelWait);
   
   {Destroy Channel Lock}
   MutexDestroy(PBCM2710DMAHost(DMA).ChannelLock);
   
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;

 {Count Free DMA Bulk Channels}
 Mask:=(PBCM2710DMAHost(DMA).ChannelMask and BCM2710_DMA_BULK_CHANNELS);
 Count:=0;
 while Mask <> 0 do
  begin
   if (Mask and 1) <> 0 then
    begin
     Inc(Count);
    end;
   
   Mask:=(Mask shr 1);
  end;
  
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(DMA_DEBUG)}
 if DMA_LOG_ENABLED then DMALogDebug(nil,'BCM2710: DMA Bulk channel free count = ' + IntToStr(Count));
 {$ENDIF}
  
 {Create DMA Bulk Channel Semaphore}
 PBCM2710DMAHost(DMA).ChannelBulk:=SemaphoreCreate(Count);
 if PBCM2710DMAHost(DMA).ChannelBulk = INVALID_HANDLE_VALUE then
  begin
   {Destroy DMA Lite Channel Semaphore}
   SemaphoreDestroy(PBCM2710DMAHost(DMA).ChannelLite);
  
   {Destroy Normal Channel Semaphore}
   SemaphoreDestroy(PBCM2710DMAHost(DMA).ChannelWait);
   
   {Destroy Channel Lock}
   MutexDestroy(PBCM2710DMAHost(DMA).ChannelLock);
   
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;
 
 {Setup Enable Register}
 PBCM2710DMAHost(DMA).EnableRegister:=PLongWord(BCM2837_DMA_ENABLE_BASE);
 
 {Setup Interrupt Register}
 PBCM2710DMAHost(DMA).InterruptRegister:=PLongWord(BCM2837_DMA_INT_STATUS_BASE);
 
 {Start Channels}
 for Count:=0 to BCM2710_DMA_CHANNEL_COUNT - 1 do
  begin
   {Host}
   PBCM2710DMAHost(DMA).Channels[Count].Host:=PBCM2710DMAHost(DMA);
   
   {Channel No}
   PBCM2710DMAHost(DMA).Channels[Count].Number:=Count;
   
   {Check Available}
   if (PBCM2710DMAHost(DMA).ChannelMask and (1 shl Count)) <> 0 then
    begin
     {Check Channel}
     case Count of
      {Channels 0 to 10}
      0..10:begin
        {Interrupt No}
        PBCM2710DMAHost(DMA).Channels[Count].Interrupt:=BCM2837_IRQ_DMA0 + Count;
      
        {Registers}
        PBCM2710DMAHost(DMA).Channels[Count].Registers:=PBCM2837DMARegisters(BCM2837_DMA0_REGS_BASE + ($100 * Count));
      
        {Request IRQ}
        RequestIRQ(IRQ_ROUTING,PBCM2710DMAHost(DMA).Channels[Count].Interrupt,TInterruptHandler(BCM2710DMAInterruptHandler),@PBCM2710DMAHost(DMA).Channels[Count]);
       end;
      {Channels 11 to 14}
      11..14:begin
        {Interrupt No}
        PBCM2710DMAHost(DMA).Channels[Count].Interrupt:=BCM2837_IRQ_DMA11_14;
      
        {Registers}
        PBCM2710DMAHost(DMA).Channels[Count].Registers:=PBCM2837DMARegisters(BCM2837_DMA0_REGS_BASE + ($100 * Count));
      
        {Request IRQ}
        RequestIRQ(IRQ_ROUTING,PBCM2710DMAHost(DMA).Channels[Count].Interrupt,TInterruptHandler(BCM2710DMASharedInterruptHandler),DMA);
       end; 
      {Channel 15}
      15:begin
        {Interrupt No (Only available on the all channels interrupt)} 
        PBCM2710DMAHost(DMA).Channels[Count].Interrupt:=BCM2837_IRQ_DMA_ALL;

        {Registers}
        PBCM2710DMAHost(DMA).Channels[Count].Registers:=PBCM2837DMARegisters(BCM2837_DMA15_REGS_BASE);
        
        {No Request IRQ}
       end;      
     end;
     
     {Memory Barrier}
     DataMemoryBarrier; {Before the First Write}
     
     {Check the Channel}
     if (PBCM2710DMAHost(DMA).EnableRegister^ and (1 shl Count)) = 0 then
      begin
       {Enable the Channel}
       PBCM2710DMAHost(DMA).EnableRegister^:=PBCM2710DMAHost(DMA).EnableRegister^ or (1 shl Count);
       MicrosecondDelay(1000);
     
       {Reset the Channel}
       PBCM2710DMAHost(DMA).Channels[Count].Registers.CS:=BCM2837_DMA_CS_RESET;
      end; 
     
     {Memory Barrier}
     DataMemoryBarrier; {After the Last Read} 
    end
   else
    begin
     {Interrupt No}
     PBCM2710DMAHost(DMA).Channels[Count].Interrupt:=LongWord(INVALID_HANDLE_VALUE);
     
     {Registers}
     PBCM2710DMAHost(DMA).Channels[Count].Registers:=nil;
    end;
  end;

 Result:=ERROR_SUCCESS;  
end; 

{==============================================================================}

function BCM2710DMAHostStop(DMA:PDMAHost):LongWord;
var
 Count:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check DMA}
 if DMA = nil then Exit;
 
 {Stop Channels}
 for Count:=0 to BCM2710_DMA_CHANNEL_COUNT - 1 do
  begin
   {Check Available}
   if (PBCM2710DMAHost(DMA).ChannelMask and (1 shl Count)) <> 0 then
    begin
     {Check Channel}
     case Count of
      {Channels 0 to 10}
      0..10:begin
        {Release IRQ}
        ReleaseIRQ(IRQ_ROUTING,PBCM2710DMAHost(DMA).Channels[Count].Interrupt,TInterruptHandler(BCM2710DMAInterruptHandler),@PBCM2710DMAHost(DMA).Channels[Count]);
       end;
      {Channels 11 to 14}
      11..14:begin
        {Release IRQ}
        ReleaseIRQ(IRQ_ROUTING,PBCM2710DMAHost(DMA).Channels[Count].Interrupt,TInterruptHandler(BCM2710DMASharedInterruptHandler),DMA);
       end;
      {Channel 15}
      15:begin
        {No Release IRQ}
       end;
     end;
    end;
  end; 

 {Destroy DMA Bulk Channel Semaphore}
 SemaphoreDestroy(PBCM2710DMAHost(DMA).ChannelBulk);
 PBCM2710DMAHost(DMA).ChannelBulk:=INVALID_HANDLE_VALUE;
  
 {Destroy DMA Lite Channel Semaphore}
 SemaphoreDestroy(PBCM2710DMAHost(DMA).ChannelLite);
 PBCM2710DMAHost(DMA).ChannelLite:=INVALID_HANDLE_VALUE;
  
 {Destroy Normal Channel Semaphore}
 SemaphoreDestroy(PBCM2710DMAHost(DMA).ChannelWait);
 PBCM2710DMAHost(DMA).ChannelWait:=INVALID_HANDLE_VALUE;
 
 {Destroy Channel Lock}
 MutexDestroy(PBCM2710DMAHost(DMA).ChannelLock);
 PBCM2710DMAHost(DMA).ChannelLock:=INVALID_HANDLE_VALUE;
 
 Result:=ERROR_SUCCESS;  
end; 

{==============================================================================}

function BCM2710DMAHostSubmit(DMA:PDMAHost;Request:PDMARequest):LongWord;
var
 Bulk:Boolean;
 Lite:Boolean;
 Flags:LongWord;
 Count:LongWord;
 Channel:LongWord;
 Maximum:LongWord;
 Data:PDMAData;
 Block:PBCM2837DMAControlBlock;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check DMA}
 if DMA = nil then Exit;
 
 {Check Request}
 if Request = nil then Exit;
 if Request.Host <> DMA then Exit;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(DMA_DEBUG)}
 if DMA_LOG_ENABLED then DMALogDebug(DMA,'BCM2710: Submitting request (Request=' + IntToHex(LongWord(Request),8) + ')');
 {$ENDIF}
 
 {Get Data Count}
 Count:=DMADataCount(Request.Data);
 if Count = 0 then Exit;

 {Get Data Flags}
 Flags:=DMADataFlags(Request.Data);
 
 {Get Data Maximum}
 Maximum:=DMADataMaximum(Request.Data);
 
 Bulk:=False;
 Lite:=False;
 
 {Check for "Bulk" channel request}
 if (Flags and DMA_DATA_FLAG_BULK) <> 0 then
  begin
   Bulk:=True;
  end
 else
  begin 
   {Check for "Lite" suitable request (No Stride, No Ignore, Size less then 64K)}
   if (Flags and (DMA_DATA_FLAG_STRIDE or DMA_DATA_FLAG_NOREAD or DMA_DATA_FLAG_NOWRITE) = 0) and (Maximum <= BCM2710_DMA_MAX_LITE_TRANSFER) then
    begin
     Lite:=True;
    end;
  end;  
 
 {Get Maximum Size}
 Maximum:=BCM2710_DMA_MAX_NORMAL_TRANSFER;
 if Lite then Maximum:=BCM2710_DMA_MAX_LITE_TRANSFER;
 
 Result:=ERROR_OPERATION_FAILED;
 
 {Create Control Blocks}
 if BCM2710DMA_SHARED_MEMORY then
  begin
   Request.ControlBlocks:=GetSharedAlignedMem(Count * SizeOf(TBCM2837DMAControlBlock),BCM2710_DMA_CB_ALIGNMENT);
  end
 else if BCM2710DMA_NOCACHE_MEMORY then
  begin
   Request.ControlBlocks:=GetNoCacheAlignedMem(Count * SizeOf(TBCM2837DMAControlBlock),BCM2710_DMA_CB_ALIGNMENT);
  end
 else 
  begin
   Request.ControlBlocks:=GetAlignedMem(Count * SizeOf(TBCM2837DMAControlBlock),BCM2710_DMA_CB_ALIGNMENT);
  end;  
 if Request.ControlBlocks = nil then Exit;
 try
  {Update Control Blocks}
  Data:=Request.Data;
  Block:=PBCM2837DMAControlBlock(Request.ControlBlocks);
  while Data <> nil do
   begin
    {Check Size}
    if Data.Size = 0 then Exit;
    if Data.Size > Maximum then Exit;
    if ((Data.Flags and DMA_DATA_FLAG_STRIDE) <> 0) and (Data.StrideLength = 0) then Exit;
    
    {Setup Control Block}
    BCM2710DMADataToControlBlock(Request,Data,Block,Bulk,Lite);
    
    {$IF DEFINED(BCM2710_DEBUG) or DEFINED(DMA_DEBUG)}
    if DMA_LOG_ENABLED then DMALogDebug(DMA,'BCM2710: Data block (Source=' + IntToHex(LongWord(Data.Source),8) + ' Dest=' + IntToHex(LongWord(Data.Dest),8) + ' Size=' + IntToStr(Data.Size) + ')');
    if DMA_LOG_ENABLED then DMALogDebug(DMA,'BCM2710: Control block (SourceAddress=' + IntToHex(Block.SourceAddress,8) + ' DestinationAddress=' + IntToHex(Block.DestinationAddress,8) + ' TransferLength=' + IntToHex(Block.TransferLength,8) + ')');
    {$ENDIF}
    
    {Get Next}
    Data:=Data.Next;
    if Data <> nil then
     begin
      {Get Next Block}
      Block:=PBCM2837DMAControlBlock(LongWord(Block) + SizeOf(TBCM2837DMAControlBlock));
     end;
   end; 
 
  {Flush Control Blocks}
  if not(BCM2710DMA_CACHE_COHERENT) then
   begin
    CleanDataCacheRange(LongWord(Request.ControlBlocks),Count * SizeOf(TBCM2837DMAControlBlock));
   end;
  
  {Wait for Channel}
  if Bulk then
   begin
    if SemaphoreWait(PBCM2710DMAHost(DMA).ChannelBulk) <> ERROR_SUCCESS then Exit;
   end
  else if Lite then
   begin
    if SemaphoreWait(PBCM2710DMAHost(DMA).ChannelLite) <> ERROR_SUCCESS then Exit;
   end
  else
   begin  
    if SemaphoreWait(PBCM2710DMAHost(DMA).ChannelWait) <> ERROR_SUCCESS then Exit;
   end; 
  
  {Acquire the Lock}
  if MutexLock(PBCM2710DMAHost(DMA).ChannelLock) = ERROR_SUCCESS then
   begin
    try
     {Get Free Channel}
     if Bulk then
      begin
       Channel:=FirstBitSet(PBCM2710DMAHost(DMA).ChannelFree and BCM2710_DMA_BULK_CHANNELS);
      end
     else if Lite then
      begin
       Channel:=FirstBitSet(PBCM2710DMAHost(DMA).ChannelFree and BCM2710_DMA_LITE_CHANNELS);
      end
     else
      begin
       Channel:=FirstBitSet(PBCM2710DMAHost(DMA).ChannelFree and BCM2710_DMA_NORMAL_CHANNELS);
      end;
      
     {$IF DEFINED(BCM2710_DEBUG) or DEFINED(DMA_DEBUG)}
     if DMA_LOG_ENABLED then DMALogDebug(DMA,'BCM2710: Allocated channel (Channel=' + IntToStr(Channel) + ')');
     {$ENDIF}
      
     {Check Free Channel} 
     if Channel <> LongWord(INVALID_HANDLE_VALUE) then 
      begin
       {Update Channel Free}
       PBCM2710DMAHost(DMA).ChannelFree:=PBCM2710DMAHost(DMA).ChannelFree xor (1 shl Channel);
      
       {Update Channel}
       PBCM2710DMAHost(DMA).Channels[Channel].Request:=Request;
       
       {Memory Barrier}
       DataMemoryBarrier; {Before the First Write}
       
       {Set Control Block}
       if BCM2710DMA_BUS_ADDRESSES then
        begin
         PBCM2710DMAHost(DMA).Channels[Channel].Registers.CONBLK_AD:=PhysicalToBusAddress(Request.ControlBlocks);
        end
       else
        begin
         PBCM2710DMAHost(DMA).Channels[Channel].Registers.CONBLK_AD:=LongWord(Request.ControlBlocks);
        end; 
       
       {Note: Broadcom documentation states that BCM2837_DMA_CS_ERROR bit should be cleared by writing
              to the error bits in the debug register, this doesn't seem to be neccessary in practice}
              
       {Enable Channel}
       PBCM2710DMAHost(DMA).Channels[Channel].Registers.CS:=BCM2837_DMA_CS_ACTIVE;
       
       {Note: Broadcom documentation states that the BCM2837_DMA_CS_END bit will be set when a transfer
              is completed and should be cleared by writing 1 to it, this doesn't seem to be the case}
                            
       {Update Status}
       Request.Status:=ERROR_NOT_COMPLETED;
      
       {Return Result}
       Result:=ERROR_SUCCESS;
      end
     else
      begin
       {Signal Semaphore}
       if Bulk then
        begin
         SemaphoreSignal(PBCM2710DMAHost(DMA).ChannelBulk);
        end
       else if Lite then
        begin
         SemaphoreSignal(PBCM2710DMAHost(DMA).ChannelLite);
        end
       else
        begin
         SemaphoreSignal(PBCM2710DMAHost(DMA).ChannelWait);
        end;
      end;     
    finally
     {Release the Lock}
     MutexUnlock(PBCM2710DMAHost(DMA).ChannelLock);
    end;   
   end;
 finally
  if Result <> ERROR_SUCCESS then
   begin
    FreeMem(Request.ControlBlocks);
   end;
 end; 
end; 

{==============================================================================}

function BCM2710DMAHostCancel(DMA:PDMAHost;Request:PDMARequest):LongWord;
var
 CS:LongWord;
 Count:LongWord;
 Channel:LongWord;
 Timeout:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check DMA}
 if DMA = nil then Exit;
 
 {Check Request}
 if Request = nil then Exit;
 if Request.Host <> DMA then Exit;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(DMA_DEBUG)}
 if DMA_LOG_ENABLED then DMALogDebug(DMA,'BCM2710: Cancelling request (Request=' + IntToHex(LongWord(Request),8) + ')');
 {$ENDIF}
 
 {Acquire the Lock}
 if MutexLock(PBCM2710DMAHost(DMA).ChannelLock) = ERROR_SUCCESS then
  begin
   try
    {Check Request}
    if Request.Status = ERROR_NOT_PROCESSED then
     begin
      {Update Request}
      Request.Status:=ERROR_CANCELLED;
     
      {Return Result}
      Result:=ERROR_SUCCESS;
     end
    else if Request.Status = ERROR_NOT_COMPLETED then
     begin
      {Update Request}
      Request.Status:=ERROR_CANCELLED;
     
      {Find Channel}
      Channel:=LongWord(INVALID_HANDLE_VALUE);
      for Count:=0 to BCM2710_DMA_CHANNEL_COUNT - 1 do
       begin
        if PBCM2710DMAHost(DMA).Channels[Channel].Request = Request then
         begin
          Channel:=Count;
          Break;
         end;
       end;
       
      {Check Channel}
      if Channel <> LongWord(INVALID_HANDLE_VALUE) then
       begin
        {$IF DEFINED(BCM2710_DEBUG) or DEFINED(DMA_DEBUG)}
        if DMA_LOG_ENABLED then DMALogDebug(DMA,'BCM2710: Located channel (Channel=' + IntToStr(Channel) + ')');
        {$ENDIF}
      
        {Memory Barrier}
        DataMemoryBarrier; {Before the First Write}
      
        {Get Status}
        CS:=PBCM2710DMAHost(DMA).Channels[Channel].Registers.CS;
      
        {Check Active}
        if (CS and BCM2837_DMA_CS_ACTIVE) <> 0 then
         begin
          {Pause the Channel}
          PBCM2710DMAHost(DMA).Channels[Channel].Registers.CS:=CS and not(BCM2837_DMA_CS_ACTIVE);
          
          {Wait for Paused}
          Timeout:=10000;
          while ((CS and BCM2837_DMA_CS_PAUSED) = 0) and (Timeout > 0) do
           begin
            CS:=PBCM2710DMAHost(DMA).Channels[Channel].Registers.CS;
            
            Dec(Timeout);
           end;
          
          {Check Paused}
          if (CS and BCM2837_DMA_CS_PAUSED) = 0 then
           begin
            Result:=ERROR_TIMEOUT;
            Exit;
           end;
           
          {Clear the Next Control Block}
          PBCM2710DMAHost(DMA).Channels[Channel].Registers.NEXTCONBK:=0;
          
          {Set the Interrupt Enable}
          PBCM2710DMAHost(DMA).Channels[Channel].Registers.TI:=PBCM2710DMAHost(DMA).Channels[Channel].Registers.TI or BCM2837_DMA_TI_INTEN;
          
          {Enable and Abort the Channel}
          PBCM2710DMAHost(DMA).Channels[Channel].Registers.CS:=PBCM2710DMAHost(DMA).Channels[Channel].Registers.CS or BCM2837_DMA_CS_ACTIVE or BCM2837_DMA_CS_ABORT;
         end;
         
        {Memory Barrier}
        DataMemoryBarrier; {After the Last Read} 
           
        {Interrupt handler will complete cancel}
       end
      else
       begin
        {$IF DEFINED(BCM2710_DEBUG) or DEFINED(DMA_DEBUG)}
        if DMA_LOG_ENABLED then DMALogDebug(DMA,'BCM2710: No channel');
        {$ENDIF}
       
        {Interrupt handler will complete cancel}
       end;
              
      {Return Result}
      Result:=ERROR_SUCCESS;
     end
    else
     begin
      {Return Result}
      Result:=ERROR_OPERATION_FAILED;
     end;     
   finally
    {Release the Lock}
    MutexUnlock(PBCM2710DMAHost(DMA).ChannelLock);
   end;   
  end
 else
  begin
   Result:=ERROR_OPERATION_FAILED;
  end;
end; 

{==============================================================================}

procedure BCM2710DMAInterruptHandler(Channel:PBCM2710DMAChannel);
{DMA Channels 0 to 10 each have a dedicated interrupt, this handler simply
 clears the interrupt and sends a completion on the associated channel}
begin
 {}
 {Check Channel}
 if Channel = nil then Exit;
 if Channel.Registers = nil then Exit;
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Acknowledge Interrupt}
 Channel.Registers.CS:=BCM2837_DMA_CS_INT;
 
 {Send Completion}
 WorkerScheduleIRQ(CPU_AFFINITY_NONE,TWorkerTask(BCM2710DMARequestComplete),Channel,nil);
end; 

{==============================================================================}

procedure BCM2710DMASharedInterruptHandler(DMA:PBCM2710DMAHost);
{DMA Channels 11 to 14 share a common interrupt, this alternate handler determines
 which ones triggered the current interrupt and sends a completion on that channel}
var
 Channel:LongWord;
 Interrupts:LongWord; 
begin
 {}
 {Check DMA}
 if DMA = nil then Exit;

 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Get Interrupt Status}
 Interrupts:=(DMA.InterruptRegister^ and BCM2710_DMA_SHARED_CHANNELS);
 while Interrupts <> 0 do
  begin
   {Get Channel}
   Channel:=FirstBitSet(Interrupts);
   
   {Check Channel}
   if DMA.Channels[Channel].Registers <> nil then
    begin
     {Acknowledge Interrupt}
     DMA.Channels[Channel].Registers.CS:=BCM2837_DMA_CS_INT;
     
     {Send Completion}
     WorkerScheduleIRQ(CPU_AFFINITY_NONE,TWorkerTask(BCM2710DMARequestComplete),@DMA.Channels[Channel],nil);
    end;
   
   {Clear the Interrupt}
   Interrupts:=Interrupts xor (1 shl Channel);
  end; 
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
end;

{==============================================================================}

procedure BCM2710DMARequestComplete(Channel:PBCM2710DMAChannel);
var
 CS:LongWord;
 Data:PDMAData;
 Offset:LongInt; {Allow for negative stride}
 DMA:PBCM2710DMAHost;
 Request:PDMARequest;
begin
 {}
 {Check Channel}
 if Channel = nil then Exit;
 if Channel.Registers = nil then Exit;
 
 {Get Host}
 DMA:=Channel.Host;
 if DMA = nil then Exit;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(DMA_DEBUG)}
 if DMA_LOG_ENABLED then DMALogDebug(@DMA.DMA,'BCM2710: Request completed (Request=' + IntToHex(LongWord(Channel.Request),8) + ')');
 {$ENDIF}

 {Get Status}
 CS:=Channel.Registers.CS;

 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(DMA_DEBUG)}
 if DMA_LOG_ENABLED then DMALogDebug(@DMA.DMA,'BCM2710:  (Registers.CS=' + IntToHex(Channel.Registers.CS,8) + ')');
 if DMA_LOG_ENABLED then DMALogDebug(@DMA.DMA,'BCM2710:  (Registers.CONBLK_AD=' + IntToHex(Channel.Registers.CONBLK_AD,8) + ')');
 if DMA_LOG_ENABLED then DMALogDebug(@DMA.DMA,'BCM2710:  (Registers.TI=' + IntToHex(Channel.Registers.TI,8) + ')');
 if DMA_LOG_ENABLED then DMALogDebug(@DMA.DMA,'BCM2710:  (Registers.SOURCE_AD=' + IntToHex(Channel.Registers.SOURCE_AD,8) + ')');
 if DMA_LOG_ENABLED then DMALogDebug(@DMA.DMA,'BCM2710:  (Registers.DEST_AD=' + IntToHex(Channel.Registers.DEST_AD,8) + ')');
 if DMA_LOG_ENABLED then DMALogDebug(@DMA.DMA,'BCM2710:  (Registers.TXFR_LEN=' + IntToHex(Channel.Registers.TXFR_LEN,8) + ')');
 if DMA_LOG_ENABLED then DMALogDebug(@DMA.DMA,'BCM2710:  (Registers.STRIDE=' + IntToHex(Channel.Registers.STRIDE,8) + ')');
 if DMA_LOG_ENABLED then DMALogDebug(@DMA.DMA,'BCM2710:  (Registers.NEXTCONBK=' + IntToHex(Channel.Registers.NEXTCONBK,8) + ')');
 if DMA_LOG_ENABLED then DMALogDebug(@DMA.DMA,'BCM2710:  (Registers.DEBUG=' + IntToHex(Channel.Registers.DEBUG,8) + ')');
 {$ENDIF}
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
 
 {Get Request}
 Request:=Channel.Request;
 
 {Acquire the Lock}
 if MutexLock(DMA.ChannelLock) = ERROR_SUCCESS then
  begin
   try
    {Update Statistics}
    Inc(DMA.InterruptCount);
    
    {Check Channel}
    if Channel.Number < BCM2710_DMA_CHANNEL_COUNT then
     begin
      {$IF DEFINED(BCM2710_DEBUG) or DEFINED(DMA_DEBUG)}
      if DMA_LOG_ENABLED then DMALogDebug(@DMA.DMA,'BCM2710: Released channel (Channel=' + IntToStr(Channel.Number) + ')');
      {$ENDIF}
      
      {Update Channel}
      DMA.Channels[Channel.Number].Request:=nil;
      
      {Update Channel Free}
      DMA.ChannelFree:=DMA.ChannelFree or (1 shl Channel.Number);
      
      {Check Bulk}
      if ((1 shl Channel.Number) and BCM2710_DMA_BULK_CHANNELS) <> 0 then
       begin
        {Signal Semaphore}
        SemaphoreSignal(DMA.ChannelBulk);
       end
      {Check Lite}
      else if ((1 shl Channel.Number) and BCM2710_DMA_LITE_CHANNELS) <> 0 then
       begin
        {Signal Semaphore}
        SemaphoreSignal(DMA.ChannelLite);
       end
      else
       begin
        {Signal Semaphore}
        SemaphoreSignal(DMA.ChannelWait);
       end;
     end; 
    
   finally
    {Release the Lock}
    MutexUnlock(DMA.ChannelLock);
   end;   
  end;
  
 {Check Request}
 if Request <> nil then
  begin
   {Check Status}
   if (CS and BCM2837_DMA_CS_ERROR) <> 0 then
    begin
     Request.Status:=ERROR_OPERATION_FAILED;
    end
   else
    begin
     Request.Status:=ERROR_SUCCESS;
    end;       
   
   {Release Control Blocks}
   if Request.ControlBlocks <> nil then
    begin
     FreeMem(Request.ControlBlocks);
     Request.ControlBlocks:=nil;
    end; 
   
   {Flush Dest} 
   case Request.Direction of
    DMA_DIR_MEM_TO_MEM,DMA_DIR_DEV_TO_MEM:begin
      if not(BCM2710DMA_CACHE_COHERENT) then
       begin
        Data:=Request.Data;
        while Data <> nil do
         begin
          if (Data.Flags and DMA_DATA_FLAG_NOINVALIDATE) = 0 then
           begin
            if ((Data.Flags and DMA_DATA_FLAG_STRIDE) = 0) or (Data.DestStride = 0) then
             begin
              InvalidateDataCacheRange(LongWord(Data.Dest),Data.Size);
             end
            else
             begin
              Offset:=0;
              while Offset < Data.Size do
               begin
                InvalidateDataCacheRange(LongWord(Data.Dest + Offset),Data.StrideLength);
                
                Inc(Offset,Data.DestStride);
               end;
             end;
           end; 
          
          Data:=Data.Next;
         end; 
       end;
     end;
   end;
           
   {Complete the request}
   DMARequestComplete(Request);
  end;
end; 

{==============================================================================}

function BCM2710DMAPeripheralToDREQ(Peripheral:LongWord):LongWord;
begin
 {}
 Result:=BCM2837_DMA_DREQ_NONE;
 
 case Peripheral of
  DMA_DREQ_ID_UART_TX:Result:=BCM2837_DMA_DREQ_UARTTX;
  DMA_DREQ_ID_UART_RX:Result:=BCM2837_DMA_DREQ_UARTRX;
  DMA_DREQ_ID_SPI_TX:Result:=BCM2837_DMA_DREQ_SPITX;
  DMA_DREQ_ID_SPI_RX:Result:=BCM2837_DMA_DREQ_SPIRX;
  DMA_DREQ_ID_SPI_SLAVE_TX:Result:=BCM2837_DMA_DREQ_BSCSPITX;
  DMA_DREQ_ID_SPI_SLAVE_RX:Result:=BCM2837_DMA_DREQ_BSCSPIRX;
  DMA_DREQ_ID_PCM_TX:Result:=BCM2837_DMA_DREQ_PCMTX;
  DMA_DREQ_ID_PCM_RX:Result:=BCM2837_DMA_DREQ_PCMRX;
  DMA_DREQ_ID_PWM:Result:=BCM2837_DMA_DREQ_PWM;
  DMA_DREQ_ID_MMC:Result:=BCM2837_DMA_DREQ_EMMC;
  DMA_DREQ_ID_SDHOST:Result:=BCM2837_DMA_DREQ_SDHOST;
 end;
end;

{==============================================================================}

procedure BCM2710DMADataToControlBlock(Request:PDMARequest;Data:PDMAData;Block:PBCM2837DMAControlBlock;Bulk,Lite:Boolean);
var
 Count:LongWord;
 Offset:LongInt; {Allow for negative stride}
begin
 {}
 if Request = nil then Exit;
 if Data = nil then Exit;
 if Block = nil then Exit;
 
 {Clear Transfer Information}
 Block.TransferInformation:=0;
 
 {Setup Source and Destination}
 if BCM2710DMA_BUS_ADDRESSES then
  begin
   case Request.Direction of
    DMA_DIR_NONE:begin
      Block.SourceAddress:=LongWord(Data.Source);
      Block.DestinationAddress:=LongWord(Data.Dest);
     end;
    DMA_DIR_MEM_TO_MEM:begin
      Block.SourceAddress:=PhysicalToBusAddress(Data.Source);
      Block.DestinationAddress:=PhysicalToBusAddress(Data.Dest);
     end;
    DMA_DIR_MEM_TO_DEV:begin
      Block.SourceAddress:=PhysicalToBusAddress(Data.Source);
      Block.DestinationAddress:=PhysicalToIOAddress(Data.Dest);
     end;
    DMA_DIR_DEV_TO_MEM:begin
      Block.SourceAddress:=PhysicalToIOAddress(Data.Source);
      Block.DestinationAddress:=PhysicalToBusAddress(Data.Dest);
     end;
    DMA_DIR_DEV_TO_DEV:begin
      Block.SourceAddress:=PhysicalToIOAddress(Data.Source);
      Block.DestinationAddress:=PhysicalToIOAddress(Data.Dest);
     end;     
   end;
  end
 else
  begin
   Block.SourceAddress:=LongWord(Data.Source);
   Block.DestinationAddress:=LongWord(Data.Dest);
  end;  
   
 {Setup Transfer Length and Stride}
 if (Data.Flags and DMA_DATA_FLAG_STRIDE) = 0 then
  begin
   {Linear Mode}
   Block.TransferLength:=Data.Size;
   Block.ModeStide:=0;
  end
 else
  begin
   {Stride Mode}
   Block.TransferInformation:=Block.TransferInformation or BCM2837_DMA_TI_2DMODE;
   
   {Get Count (minus 1)}
   Count:=(Data.Size div (Data.StrideLength and BCM2710_DMA_MAX_X_LENGTH)) - 1;
   
   {Set Length and Count}
   Block.TransferLength:=((Count and BCM2710_DMA_MAX_Y_COUNT) shl 16) or (Data.StrideLength and BCM2710_DMA_MAX_X_LENGTH);
   
   {Set Source and Dest Stride}
   Block.ModeStide:=((Data.DestStride and BCM2710_DMA_MAX_STRIDE) shl 16) or (Data.SourceStride and BCM2710_DMA_MAX_STRIDE);
  end;  
 
 {Setup Transfer Information}
 {Source Data Request}
 if (Data.Flags and DMA_DATA_FLAG_SOURCE_DREQ) <> 0 then
  begin
   Block.TransferInformation:=Block.TransferInformation or BCM2837_DMA_TI_WAIT_RESP or BCM2837_DMA_TI_SRC_DREQ;
  end;
 {Dest Data Request} 
 if (Data.Flags and DMA_DATA_FLAG_DEST_DREQ) <> 0 then
  begin
   Block.TransferInformation:=Block.TransferInformation or BCM2837_DMA_TI_WAIT_RESP or BCM2837_DMA_TI_DEST_DREQ;
  end;
 {Source Increment} 
 if (Data.Flags and DMA_DATA_FLAG_SOURCE_NOINCREMENT) = 0 then
  begin
   Block.TransferInformation:=Block.TransferInformation or BCM2837_DMA_TI_SRC_INC;
  end;
 {Dest Increment} 
 if (Data.Flags and DMA_DATA_FLAG_DEST_NOINCREMENT) = 0 then
  begin
   Block.TransferInformation:=Block.TransferInformation or BCM2837_DMA_TI_DEST_INC;
  end;
 {Source Width}
 if (Data.Flags and DMA_DATA_FLAG_SOURCE_WIDE) <> 0 then
  begin
   Block.TransferInformation:=Block.TransferInformation or BCM2837_DMA_TI_SRC_WIDTH;
  end;
 {Dest Width}
 if (Data.Flags and DMA_DATA_FLAG_DEST_WIDE) <> 0 then
  begin
   Block.TransferInformation:=Block.TransferInformation or BCM2837_DMA_TI_DEST_WIDTH;
  end;
 {Source Ignore}
 if (Data.Flags and DMA_DATA_FLAG_NOREAD) <> 0 then
  begin
   Block.TransferInformation:=Block.TransferInformation or BCM2837_DMA_TI_SRC_IGNORE;
  end;
 {Dest Ignore}
 if (Data.Flags and DMA_DATA_FLAG_NOWRITE) <> 0 then
  begin
   Block.TransferInformation:=Block.TransferInformation or BCM2837_DMA_TI_DEST_IGNORE;
  end;
 {Peripheral Map}
 if Request.Peripheral <> DMA_DREQ_ID_NONE then
  begin
   Block.TransferInformation:=Block.TransferInformation or (BCM2710DMAPeripheralToDREQ(Request.Peripheral) shl BCM2837_DMA_TI_PERMAP_SHIFT);
  end; 
 {Burst Length}
 if Bulk then
  begin
   Block.TransferInformation:=Block.TransferInformation or (BCM2710_DMA_BULK_BURST_LENGTH shl BCM2837_DMA_TI_BURST_LENGTH_SHIFT);
  end
 else if Lite then
  begin
   Block.TransferInformation:=Block.TransferInformation or (BCM2710_DMA_LITE_BURST_LENGTH shl BCM2837_DMA_TI_BURST_LENGTH_SHIFT);
  end
 else
  begin
   Block.TransferInformation:=Block.TransferInformation or (BCM2710_DMA_NORMAL_BURST_LENGTH shl BCM2837_DMA_TI_BURST_LENGTH_SHIFT);
  end;  
 {Interrupt Enable}
 if Data.Next = nil then
  begin
   Block.TransferInformation:=Block.TransferInformation or BCM2837_DMA_TI_INTEN;
  end;
 
 {Setup Next Control Block}
 if Data.Next <> nil then
  begin
   {Set Next Block}
   if BCM2710DMA_BUS_ADDRESSES then
    begin
     Block.NextControlBlockAddress:=PhysicalToBusAddress(Pointer(LongWord(Block) + SizeOf(TBCM2837DMAControlBlock)));
    end
   else
    begin
     Block.NextControlBlockAddress:=LongWord(Block) + SizeOf(TBCM2837DMAControlBlock);
    end;
  end
 else
  begin
   Block.NextControlBlockAddress:=0;
  end;  
  
 {Setup Reserved} 
 Block.Reserved1:=0;
 Block.Reserved2:=0;
 
 {Flush Source} 
 case Request.Direction of
  DMA_DIR_MEM_TO_MEM,DMA_DIR_MEM_TO_DEV:begin
    if not(BCM2710DMA_CACHE_COHERENT) and ((Data.Flags and DMA_DATA_FLAG_NOCLEAN) = 0) then
     begin
      if ((Data.Flags and DMA_DATA_FLAG_STRIDE) = 0) or (Data.SourceStride = 0) then
       begin
        CleanDataCacheRange(LongWord(Data.Source),Data.Size);
       end
      else
       begin
        Offset:=0;
        while Offset < Data.Size do
         begin
          CleanDataCacheRange(LongWord(Data.Source + Offset),Data.StrideLength);
          
          Inc(Offset,Data.SourceStride);
         end; 
       end;
     end;
   end;
 end;  
end;

{==============================================================================}
{==============================================================================}
{BCM2710 PWM Functions}

{==============================================================================}
{==============================================================================}
{BCM2710 PCM Functions}

{==============================================================================}
{==============================================================================}
{BCM2710 GPIO Functions}
function BCM2710GPIOStart(GPIO:PGPIODevice):LongWord; 
var
 Pin:LongWord;
 Count:LongWord;
 Value:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check GPIO}
 if GPIO = nil then Exit;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'BCM2710: GPIO Start');
 {$ENDIF}
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Clear Registers}
 for Count:=0 to BCM2837_GPIO_BANK_COUNT - 1 do
  begin
   {Event Detect Registers}
   PLongWord(GPIO.Address + BCM2837_GPREN0 + (Count * SizeOf(LongWord)))^:=0;
   PLongWord(GPIO.Address + BCM2837_GPFEN0 + (Count * SizeOf(LongWord)))^:=0;
   PLongWord(GPIO.Address + BCM2837_GPHEN0 + (Count * SizeOf(LongWord)))^:=0;
   PLongWord(GPIO.Address + BCM2837_GPLEN0 + (Count * SizeOf(LongWord)))^:=0;
   PLongWord(GPIO.Address + BCM2837_GPAREN0 + (Count * SizeOf(LongWord)))^:=0;
   PLongWord(GPIO.Address + BCM2837_GPAFEN0 + (Count * SizeOf(LongWord)))^:=0;
   
   {Event Detect Status}
   Value:=PLongWord(GPIO.Address + BCM2837_GPEDS0 + (Count * SizeOf(LongWord)))^;
   while Value <> 0 do
    begin
     {Get Pin}
     Pin:=FirstBitSet(Value);

     {Clear Status}
     PLongWord(GPIO.Address + BCM2837_GPEDS0 + (Count * SizeOf(LongWord)))^:=(BCM2837_GPEDS_MASK shl Pin);
     
     {Clear Pin}
     Value:=Value xor (1 shl Pin);
    end;
  end; 
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
 
 {Create Lock}
 PBCM2710GPIODevice(GPIO).Lock:=SpinCreate;
 if PBCM2710GPIODevice(GPIO).Lock = INVALID_HANDLE_VALUE then
  begin
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end; 
 
 {Setup Banks}
 for Count:=0 to BCM2837_GPIO_BANK_COUNT - 1 do
  begin
   PBCM2710GPIODevice(GPIO).Banks[Count].GPIO:=GPIO;
   PBCM2710GPIODevice(GPIO).Banks[Count].Bank:=Count;
   PBCM2710GPIODevice(GPIO).Banks[Count].Address:=PtrUInt(GPIO.Address) + BCM2837_GPEDS0 + (Count * SizeOf(LongWord));
   PBCM2710GPIODevice(GPIO).Banks[Count].PinStart:=Count * 32;
  end;
  
 {Create Pins}
 SetLength(GPIO.Pins,BCM2837_GPIO_PIN_COUNT);
 
 {Setup Pins}
 for Count:=0 to BCM2837_GPIO_PIN_COUNT - 1 do
  begin
   GPIO.Pins[Count].GPIO:=GPIO;
   GPIO.Pins[Count].Pin:=Count;
   GPIO.Pins[Count].Flags:=GPIO_EVENT_FLAG_NONE;
   GPIO.Pins[Count].Trigger:=GPIO_TRIGGER_NONE;
   GPIO.Pins[Count].Count:=0;
   GPIO.Pins[Count].Event:=INVALID_HANDLE_VALUE;
   GPIO.Pins[Count].Events:=nil;
  end;
  
 {Request Bank0 IRQ}
 RequestIRQ(IRQ_ROUTING,BCM2837_IRQ_GPIO_0,TInterruptHandler(BCM2710GPIOInterruptHandler),@PBCM2710GPIODevice(GPIO).Banks[0]);

 {Request Bank1 IRQ}
 RequestIRQ(IRQ_ROUTING,BCM2837_IRQ_GPIO_1,TInterruptHandler(BCM2710GPIOInterruptHandler),@PBCM2710GPIODevice(GPIO).Banks[1]);

 Result:=ERROR_SUCCESS;  
end;

{==============================================================================}

function BCM2710GPIOStop(GPIO:PGPIODevice):LongWord;
var
 Count:LongWord;
 Event:PGPIOEvent;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check GPIO}
 if GPIO = nil then Exit;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'BCM2710: GPIO Stop');
 {$ENDIF}
 
 {Release Bank0 IRQ}
 ReleaseIRQ(IRQ_ROUTING,BCM2837_IRQ_GPIO_0,TInterruptHandler(BCM2710GPIOInterruptHandler),@PBCM2710GPIODevice(GPIO).Banks[0]);

 {Release Bank1 IRQ}
 ReleaseIRQ(IRQ_ROUTING,BCM2837_IRQ_GPIO_1,TInterruptHandler(BCM2710GPIOInterruptHandler),@PBCM2710GPIODevice(GPIO).Banks[1]);
 
 {Release Pins}
 for Count:=0 to BCM2837_GPIO_PIN_COUNT - 1 do
  begin
   if GPIO.Pins[Count].Event <> INVALID_HANDLE_VALUE then
    begin
     EventDestroy(GPIO.Pins[Count].Event);
    end;
   
   if GPIO.Pins[Count].Events <> nil then
    begin
     Event:=GPIO.Pins[Count].Events;
     while Event <> nil do
      begin
       {Deregister Event}
       GPIODeviceDeregisterEvent(GPIO,@GPIO.Pins[Count],Event);
       
       {Destroy Event}
       GPIODeviceDestroyEvent(GPIO,Event);
       
       Event:=GPIO.Pins[Count].Events;
      end;
    end;
  end; 
 
 {Destroy Pins}
 SetLength(GPIO.Pins,0);
 
 {Destroy Lock}
 if PBCM2710GPIODevice(GPIO).Lock <> INVALID_HANDLE_VALUE then
  begin
   SpinDestroy(PBCM2710GPIODevice(GPIO).Lock);
  end;
 
 Result:=ERROR_SUCCESS;  
end;

{==============================================================================}
 
function BCM2710GPIOInputGet(GPIO:PGPIODevice;Pin:LongWord):LongWord;
var
 Reg:LongWord;
 Shift:LongWord;
begin
 {}
 Result:=GPIO_LEVEL_UNKNOWN;
 
 {Check GPIO}
 if GPIO = nil then Exit;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'BCM2710: GPIO Input Get (Pin=' + GPIOPinToString(Pin) + ')');
 {$ENDIF}
 
 {Check Pin}
 if Pin > BCM2710_GPIO_MAX_PIN then Exit;

 {Update Statistics}
 Inc(GPIO.GetCount);
 
 {Get Shift}
 Shift:=Pin mod 32;
 
 {Get Register}
 Reg:=BCM2837_GPLEV0 + ((Pin div 32) * SizeOf(LongWord));
 
 {Read Register}
 Result:=(PLongWord(GPIO.Address + Reg)^ shr Shift) and BCM2837_GPLEV_MASK;

 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
end;

{==============================================================================}

function BCM2710GPIOInputWait(GPIO:PGPIODevice;Pin,Trigger,Timeout:LongWord):LongWord;
var
 Reg:LongWord;
 Shift:LongWord;
begin
 {}
 Result:=GPIO_LEVEL_UNKNOWN;
 
 {Check GPIO}
 if GPIO = nil then Exit;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'BCM2710: GPIO  Input Wait (Pin=' + GPIOPinToString(Pin) + ' Trigger=' + GPIOTriggerToString(Trigger) + ' Timeout=' + IntToStr(Timeout) + ')');
 {$ENDIF}
 
 {Check Pin}
 if Pin > BCM2710_GPIO_MAX_PIN then Exit;
 
 {Check Timeout}
 if Timeout = 0 then Timeout:=INFINITE;
 
 {Check Trigger}
 if ((Trigger < BCM2710_GPIO_MIN_TRIGGER) or (Trigger > BCM2710_GPIO_MAX_TRIGGER)) and (Trigger <> GPIO_TRIGGER_EDGE) then Exit;
 
 {Check Existing}
 if GPIO.Pins[Pin].Trigger <> GPIO_TRIGGER_NONE then
  begin
   if GPIO.Pins[Pin].Trigger <> Trigger then Exit;
   if (GPIO.Pins[Pin].Flags and (GPIO_EVENT_FLAG_REPEAT or GPIO_EVENT_FLAG_INTERRUPT)) <> 0 then Exit;
  end;  
 
 {Check Lock}
 if (MutexOwner(GPIO.Lock) <> ThreadGetCurrent) or (MutexCount(GPIO.Lock) > 1) then Exit;
 
 {Check Event}
 if GPIO.Pins[Pin].Event = INVALID_HANDLE_VALUE then
  begin
   {Create Event (Manual Reset)}
   GPIO.Pins[Pin].Event:=EventCreate(True,False);
   
   {Check Event}
   if GPIO.Pins[Pin].Event = INVALID_HANDLE_VALUE then Exit;
  end;
  
 {Update Statistics}
 Inc(GPIO.WaitCount);

 {Check Trigger} 
 if GPIO.Pins[Pin].Trigger = GPIO_TRIGGER_NONE then
  begin
   {Get Shift}
   Shift:=Pin mod 32;
   
   {Set the Flags}
   GPIO.Pins[Pin].Flags:=GPIO_EVENT_FLAG_NONE;
   
   {Set the Trigger}
   GPIO.Pins[Pin].Trigger:=Trigger;
   
   {Acquire the Lock}
   if SpinLockIRQ(PBCM2710GPIODevice(GPIO).Lock) <> ERROR_SUCCESS then Exit;
   
   {Memory Barrier}
   DataMemoryBarrier; {Before the First Write}
   
   {Check Trigger} 
   if Trigger <> GPIO_TRIGGER_EDGE then
    begin
     {Get Register (Trigger)}
     Reg:=BCM2710_GPIO_TRIGGER_MAP[Trigger] + ((Pin div 32) * SizeOf(LongWord));
     
     {Add Trigger (Trigger)}
     PLongWord(GPIO.Address + Reg)^:=PLongWord(GPIO.Address + Reg)^ or (1 shl Shift);
    end
   else
    begin 
     {Get Register (Rising)}
     Reg:=BCM2837_GPREN0 + ((Pin div 32) * SizeOf(LongWord));
     
     {Add Trigger (Rising)}
     PLongWord(GPIO.Address + Reg)^:=PLongWord(GPIO.Address + Reg)^ or (1 shl Shift);
   
     {Get Register (Falling)}
     Reg:=BCM2837_GPFEN0 + ((Pin div 32) * SizeOf(LongWord));
     
     {Add Trigger (Falling)}
     PLongWord(GPIO.Address + Reg)^:=PLongWord(GPIO.Address + Reg)^ or (1 shl Shift);
    end; 
    
   {Memory Barrier}
   DataMemoryBarrier; {After the Last Read} 
   
   {Release the Lock}
   SpinUnlockIRQ(PBCM2710GPIODevice(GPIO).Lock);
  end;
  
 {Increment Count}
 Inc(GPIO.Pins[Pin].Count);
 
 {Release the Lock}
 MutexUnlock(GPIO.Lock);
 
 {Wait for Event}
 if EventWaitEx(GPIO.Pins[Pin].Event,Timeout) = ERROR_SUCCESS then
  begin
   {Get Register (Level)}
   Reg:=BCM2837_GPLEV0 + ((Pin div 32) * SizeOf(LongWord));
   
   {Read Register}
   Result:=(PLongWord(GPIO.Address + Reg)^ shr Shift) and BCM2837_GPLEV_MASK;
   
   {Memory Barrier}
   DataMemoryBarrier; {After the Last Read} 
  end
 else
  begin
   {Acquire the Lock}
   if MutexLock(GPIO.Lock) <> ERROR_SUCCESS then Exit;
   
   {Decrement Count}
   Dec(GPIO.Pins[Pin].Count);
   
   {Check Count}
   if GPIO.Pins[Pin].Count = 0 then
    begin
     {Check Trigger}
     if GPIO.Pins[Pin].Trigger = Trigger then
      begin
       {Get Shift}
       Shift:=Pin mod 32;
      
       {Acquire the Lock}
       if SpinLockIRQ(PBCM2710GPIODevice(GPIO).Lock) <> ERROR_SUCCESS then Exit;
       
       {Memory Barrier}
       DataMemoryBarrier; {Before the First Write}
       
       {Check Trigger} 
       if Trigger <> GPIO_TRIGGER_EDGE then
        begin
         {Get Register (Trigger)}
         Reg:=BCM2710_GPIO_TRIGGER_MAP[Trigger] + ((Pin div 32) * SizeOf(LongWord));
         
         {Remove Trigger (Trigger)}
         PLongWord(GPIO.Address + Reg)^:=PLongWord(GPIO.Address + Reg)^ and not(1 shl Shift);
        end
       else
        begin 
         {Get Register (Rising)}
         Reg:=BCM2837_GPREN0 + ((Pin div 32) * SizeOf(LongWord));
         
         {Remove Trigger (Rising)}
         PLongWord(GPIO.Address + Reg)^:=PLongWord(GPIO.Address + Reg)^ and not(1 shl Shift);
       
         {Get Register (Falling)}
         Reg:=BCM2837_GPFEN0 + ((Pin div 32) * SizeOf(LongWord));
         
         {Remove Trigger (Falling)}
         PLongWord(GPIO.Address + Reg)^:=PLongWord(GPIO.Address + Reg)^ and not(1 shl Shift);
        end; 
        
       {Memory Barrier}
       DataMemoryBarrier; {After the Last Read} 
       
       {Release the Lock}
       SpinUnlockIRQ(PBCM2710GPIODevice(GPIO).Lock);
      
       {Reset the Flags}
       GPIO.Pins[Pin].Flags:=GPIO_EVENT_FLAG_NONE;
       
       {Reset the Trigger}
       GPIO.Pins[Pin].Trigger:=GPIO_TRIGGER_NONE;
      end; 
    end; 
  end;  
end;

{==============================================================================}

function BCM2710GPIOInputEvent(GPIO:PGPIODevice;Pin,Trigger,Flags,Timeout:LongWord;Callback:TGPIOCallback;Data:Pointer):LongWord;
var
 Reg:LongWord;
 Shift:LongWord;
 Event:PGPIOEvent;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check GPIO}
 if GPIO = nil then Exit;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'BCM2710: GPIO Input Event (Pin=' + GPIOPinToString(Pin) + ' Trigger=' + GPIOTriggerToString(Trigger) + ' Flags=' + IntToHex(Flags,8) + ' Timeout=' + IntToStr(Timeout) + ')');
 {$ENDIF}
 
 {Check Pin}
 if Pin > BCM2710_GPIO_MAX_PIN then Exit;
 
 {Check Timeout}
 if Timeout = 0 then Timeout:=INFINITE;
 
 {Check Flags}
 if ((Flags and GPIO_EVENT_FLAG_REPEAT) <> 0) and ((Trigger = GPIO_TRIGGER_LOW) or (Trigger = GPIO_TRIGGER_HIGH)) then Exit;
 if ((Flags and GPIO_EVENT_FLAG_INTERRUPT) <> 0) and ((Flags and GPIO_EVENT_FLAG_REPEAT) = 0) then Exit;
 if ((Flags and GPIO_EVENT_FLAG_REPEAT) <> 0) and (Timeout <> INFINITE) then Exit;
                      
 {Check Trigger}
 if ((Trigger < BCM2710_GPIO_MIN_TRIGGER) or (Trigger > BCM2710_GPIO_MAX_TRIGGER)) and (Trigger <> GPIO_TRIGGER_EDGE) then Exit;
 
 {Check Existing}
 if GPIO.Pins[Pin].Trigger <> GPIO_TRIGGER_NONE then
  begin
   Result:=ERROR_IN_USE;
   if GPIO.Pins[Pin].Trigger <> Trigger then Exit;
   if (Flags and (GPIO_EVENT_FLAG_REPEAT or GPIO_EVENT_FLAG_INTERRUPT)) <> 0 then Exit;
   if (GPIO.Pins[Pin].Flags and (GPIO_EVENT_FLAG_REPEAT or GPIO_EVENT_FLAG_INTERRUPT)) <> 0 then Exit;
  end; 

 {Create Event}
 Event:=GPIODeviceCreateEvent(GPIO,@GPIO.Pins[Pin],Callback,Data,Timeout);
 if Event = nil then
  begin
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;
 
 {Register Event}
 if GPIODeviceRegisterEvent(GPIO,@GPIO.Pins[Pin],Event) <> ERROR_SUCCESS then
  begin
   GPIODeviceDestroyEvent(GPIO,Event);
   
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;
  
 {Update Statistics}
 Inc(GPIO.EventCount);

 {Check Trigger} 
 if GPIO.Pins[Pin].Trigger = GPIO_TRIGGER_NONE then
  begin
   {Get Shift}
   Shift:=Pin mod 32;

   {Set the Flags}
   GPIO.Pins[Pin].Flags:=Flags;
   
   {Set the Trigger}
   GPIO.Pins[Pin].Trigger:=Trigger;

   {Acquire the Lock}
   if SpinLockIRQ(PBCM2710GPIODevice(GPIO).Lock) <> ERROR_SUCCESS then Exit;
   
   {Memory Barrier}
   DataMemoryBarrier; {Before the First Write}
   
   {Check Trigger} 
   if Trigger <> GPIO_TRIGGER_EDGE then
    begin
     {Get Register (Trigger)}
     Reg:=BCM2710_GPIO_TRIGGER_MAP[Trigger] + ((Pin div 32) * SizeOf(LongWord));
     
     {Add Trigger (Trigger)}
     PLongWord(GPIO.Address + Reg)^:=PLongWord(GPIO.Address + Reg)^ or (1 shl Shift);
    end
   else
    begin 
     {Get Register (Rising)}
     Reg:=BCM2837_GPREN0 + ((Pin div 32) * SizeOf(LongWord));
     
     {Add Trigger (Rising)}
     PLongWord(GPIO.Address + Reg)^:=PLongWord(GPIO.Address + Reg)^ or (1 shl Shift);
   
     {Get Register (Falling)}
     Reg:=BCM2837_GPFEN0 + ((Pin div 32) * SizeOf(LongWord));
     
     {Add Trigger (Falling)}
     PLongWord(GPIO.Address + Reg)^:=PLongWord(GPIO.Address + Reg)^ or (1 shl Shift);
    end; 
  
   {Memory Barrier}
   DataMemoryBarrier; {After the Last Read} 
   
   {Release the Lock}
   SpinUnlockIRQ(PBCM2710GPIODevice(GPIO).Lock);
  end; 
 
 {Increment Count}
 Inc(GPIO.Pins[Pin].Count);
 
 {Check Timeout}
 if Timeout <> INFINITE then
  begin
   {Schedule Worker}
   WorkerSchedule(Timeout,TWorkerTask(BCM2710GPIOEventTimeout),Event,nil);
  end;
  
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function BCM2710GPIOInputCancel(GPIO:PGPIODevice;Pin:LongWord):LongWord;
var
 Reg:LongWord;
 Shift:LongWord;
 Event:PGPIOEvent;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check GPIO}
 if GPIO = nil then Exit;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'BCM2710: GPIO Input Cancel (Pin=' + GPIOPinToString(Pin) + ')');
 {$ENDIF}
 
 {Check Pin}
 if Pin > BCM2710_GPIO_MAX_PIN then Exit;

 {Check Trigger}
 if GPIO.Pins[Pin].Trigger = GPIO_TRIGGER_NONE then
  begin
   Result:=ERROR_NOT_FOUND;
   Exit;
  end;
 
 {Check Flags}
 if (GPIO.Pins[Pin].Flags and GPIO_EVENT_FLAG_REPEAT) = 0 then
  begin
   Result:=ERROR_NOT_FOUND;
   Exit;
  end;
 
 {Get Event}
 Event:=GPIO.Pins[Pin].Events;
 if Event <> nil then
  begin
   {Deregister Event}
   GPIODeviceDeregisterEvent(GPIO,@GPIO.Pins[Pin],Event);
   
   {Check Timeout}
   if Event.Timeout = INFINITE then
    begin
     {Destroy Event}
     GPIODeviceDestroyEvent(GPIO,Event);
    end
   else
    begin
     {Set Timeout (Timeout will destroy event)}
     Event.Timeout:=INFINITE;
    end;    
    
   {Decrement Count}
   Dec(GPIO.Pins[Pin].Count);
   
   {Check Count}
   if GPIO.Pins[Pin].Count = 0 then
    begin
     {Check Trigger}
     if GPIO.Pins[Pin].Trigger <> GPIO_TRIGGER_NONE then
      begin
       {Get Shift}
       Shift:=Pin mod 32;
      
       {Acquire the Lock}
       if SpinLockIRQ(PBCM2710GPIODevice(GPIO).Lock) <> ERROR_SUCCESS then Exit;
       
       {Memory Barrier}
       DataMemoryBarrier; {Before the First Write}
       
       {Check Trigger} 
       if GPIO.Pins[Pin].Trigger <> GPIO_TRIGGER_EDGE then
        begin
         {Get Register (Trigger)}
         Reg:=BCM2710_GPIO_TRIGGER_MAP[GPIO.Pins[Pin].Trigger] + ((Pin div 32) * SizeOf(LongWord));
         
         {Remove Trigger (Trigger)}
         PLongWord(GPIO.Address + Reg)^:=PLongWord(GPIO.Address + Reg)^ and not(1 shl Shift);
        end
       else
        begin 
         {Get Register (Rising)}
         Reg:=BCM2837_GPREN0 + ((Pin div 32) * SizeOf(LongWord));
         
         {Remove Trigger (Rising)}
         PLongWord(GPIO.Address + Reg)^:=PLongWord(GPIO.Address + Reg)^ and not(1 shl Shift);
       
         {Get Register (Falling)}
         Reg:=BCM2837_GPFEN0 + ((Pin div 32) * SizeOf(LongWord));
         
         {Remove Trigger (Falling)}
         PLongWord(GPIO.Address + Reg)^:=PLongWord(GPIO.Address + Reg)^ and not(1 shl Shift);
        end; 
        
       {Memory Barrier}
       DataMemoryBarrier; {After the Last Read} 
       
       {Release the Lock}
       SpinUnlockIRQ(PBCM2710GPIODevice(GPIO).Lock);
      
       {Reset the Flags}
       GPIO.Pins[Pin].Flags:=GPIO_EVENT_FLAG_NONE;
       
       {Reset the Trigger}
       GPIO.Pins[Pin].Trigger:=GPIO_TRIGGER_NONE;
      end; 
    end; 
  end;
 
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function BCM2710GPIOOutputSet(GPIO:PGPIODevice;Pin,Level:LongWord):LongWord;
var
 Reg:LongWord;
 Shift:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check GPIO}
 if GPIO = nil then Exit;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'BCM2710: GPIO Output Set (Pin=' + GPIOPinToString(Pin) + ' Level=' + GPIOLevelToString(Level) + ')');
 {$ENDIF}
 
 {Check Pin}
 if Pin > BCM2710_GPIO_MAX_PIN then Exit;
 
 {Check Level}
 if Level > BCM2710_GPIO_MAX_LEVEL then Exit;
 
 {Update Statistics}
 Inc(GPIO.SetCount);
 
 {Get Shift}
 Shift:=Pin mod 32;
 
 {Get Register}
 if Level = GPIO_LEVEL_HIGH then
  begin
   Reg:=BCM2837_GPSET0 + ((Pin div 32) * SizeOf(LongWord));
  end
 else
  begin
   Reg:=BCM2837_GPCLR0 + ((Pin div 32) * SizeOf(LongWord));
  end;
  
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
  
 {Write Register}
 PLongWord(GPIO.Address + Reg)^:=(BCM2837_GPSET_MASK shl Shift);

 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function BCM2710GPIOPullSelect(GPIO:PGPIODevice;Pin,Mode:LongWord):LongWord;
var
 Reg:LongWord;
 Shift:LongWord;
 Select:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check GPIO}
 if GPIO = nil then Exit;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'BCM2710: GPIO Pull Select (Pin=' + GPIOPinToString(Pin) + ' Mode=' + GPIOPullToString(Mode) + ')');
 {$ENDIF}
 
 {Check Pin}
 if Pin > BCM2710_GPIO_MAX_PIN then Exit;
 
 {Check Mode}
 if Mode > BCM2710_GPIO_MAX_PULL then Exit;
 
 {Get Select}
 Select:=BCM2710_GPIO_PULL_MAP[Mode];
 
 {Get Shift}
 Shift:=Pin mod 32;
 
 {Get Register}
 Reg:=BCM2837_GPPUDCLK0 + ((Pin div 32) * SizeOf(LongWord));
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Write Mode}
 PLongWord(GPIO.Address + BCM2837_GPPUD)^:=Select;
 
 {Wait 150 microseconds (150 cycles)}
 MicrosecondDelay(150);
 
 {Write Clock}
 PLongWord(GPIO.Address + Reg)^:=(BCM2837_GPPUDCLK_MASK shl Shift);
 
 {Wait 150 microseconds (150 cycles)}
 MicrosecondDelay(150);
 
 {Reset Mode}
 PLongWord(GPIO.Address + BCM2837_GPPUD)^:=0;
 
 {Reset Clock}
 PLongWord(GPIO.Address + Reg)^:=0;
 
 Result:=ERROR_SUCCESS; 
end;

{==============================================================================}

function BCM2710GPIOFunctionGet(GPIO:PGPIODevice;Pin:LongWord):LongWord;
var
 Reg:LongWord;
 Shift:LongWord;
 Current:LongWord;
begin
 {}
 Result:=GPIO_FUNCTION_UNKNOWN;
 
 {Check GPIO}
 if GPIO = nil then Exit;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'BCM2710: GPIO Function Get (Pin=' + GPIOPinToString(Pin) + ')');
 {$ENDIF}
 
 {Check Pin}
 if Pin > BCM2710_GPIO_MAX_PIN then Exit;
 
 {Get Shift}
 Shift:=(Pin mod 10) * 3;
 
 {Get Register}
 Reg:=BCM2837_GPFSEL0 + ((Pin div 10) * SizeOf(LongWord));
 
 {Read Register}
 Current:=(PLongWord(GPIO.Address + Reg)^ shr Shift) and BCM2837_GPFSEL_MASK;

 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
 
 {Return Result}
 Result:=BCM2710_GPIO_FUNCTION_UNMAP[Current];
end;

{==============================================================================}

function BCM2710GPIOFunctionSelect(GPIO:PGPIODevice;Pin,Mode:LongWord):LongWord;
var
 Reg:LongWord;
 Shift:LongWord;
 Value:LongWord;
 Select:LongWord;
 Current:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check GPIO}
 if GPIO = nil then Exit;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'BCM2710: GPIO Function Select (Pin=' + GPIOPinToString(Pin) + ' Mode=' + GPIOFunctionToString(Mode) + ')');
 {$ENDIF}
 
 {Check Pin}
 if Pin > BCM2710_GPIO_MAX_PIN then Exit;
 
 {Check Mode}
 if Mode > BCM2710_GPIO_MAX_FUNCTION then Exit;
 
 {Get Select}
 Select:=BCM2710_GPIO_FUNCTION_MAP[Mode];
 
 {Get Shift}
 Shift:=(Pin mod 10) * 3;
 
 {Get Register}
 Reg:=BCM2837_GPFSEL0 + ((Pin div 10) * SizeOf(LongWord));
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Read Value}
 Value:=PLongWord(GPIO.Address + Reg)^;
 
 {Get Current}
 Current:=(Value shr Shift) and BCM2837_GPFSEL_MASK;
 
 {Check Current}
 if Select <> Current then
  begin
   {Check Mode}
   if (Select <> BCM2837_GPFSEL_IN) and (Current <> BCM2837_GPFSEL_IN) then
    begin
     {Select Input}
     Value:=Value and not(BCM2837_GPFSEL_MASK shl Shift);
     Value:=Value or (BCM2837_GPFSEL_IN shl Shift);
     
     {Write Value}
     PLongWord(GPIO.Address + Reg)^:=Value;
    end;
   
   {Select Mode}
   Value:=Value and not(BCM2837_GPFSEL_MASK shl Shift);
   Value:=Value or (Select shl Shift);
   
   {Write Value}
   PLongWord(GPIO.Address + Reg)^:=Value;
  end;
  
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
  
 Result:=ERROR_SUCCESS; 
end;

{==============================================================================}

procedure BCM2710GPIOInterruptHandler(Bank:PBCM2710GPIOBank);
var
 Bit:LongWord;
 Pin:LongWord;
 Reg:LongWord;
 Shift:LongWord;
 Flags:LongWord;
 Status:LongWord;
 Trigger:LongWord;
 GPIO:PGPIODevice;
 Event:PGPIOEvent;
begin
 {}
 {Check Bank}
 if Bank = nil then Exit;

 {Get GPIO}
 GPIO:=Bank.GPIO;
 if GPIO = nil then Exit;
 
 {Acquire the Lock}
 if SpinLockIRQ(PBCM2710GPIODevice(GPIO).Lock) = ERROR_SUCCESS then
  begin
   try
    {Update Statistics}
    Inc(PBCM2710GPIODevice(GPIO).InterruptCount);
    
    {Memory Barrier}
    DataMemoryBarrier; {Before the First Write}
    
    {Get Status}
    Status:=PLongWord(Bank.Address)^;
    while Status <> 0 do
     begin
      {Get Bit}
      Bit:=FirstBitSet(Status);
      
      {Get Pin}
      Pin:=GPIO.Pins[Bank.PinStart + Bit].Pin;
      
      {Get Flags}
      Flags:=GPIO.Pins[Bank.PinStart + Bit].Flags;
      
      {Get Trigger}
      Trigger:=GPIO.Pins[Bank.PinStart + Bit].Trigger;
      
      {Check Trigger}
      if Trigger <> GPIO_TRIGGER_NONE then
       begin
        {Get Shift}
        Shift:=Pin mod 32;
        
        {Remove Triggers}
        if Trigger <> GPIO_TRIGGER_EDGE then
         begin
          {Check Flags}
          if ((Flags and GPIO_EVENT_FLAG_REPEAT) = 0) or (Trigger = GPIO_TRIGGER_LOW) or (Trigger = GPIO_TRIGGER_HIGH) then
           begin
            {Get Register (Trigger)}
            Reg:=BCM2710_GPIO_TRIGGER_MAP[Trigger] + ((Pin div 32) * SizeOf(LongWord));
            
            {Remove Trigger (Trigger)}
            PLongWord(GPIO.Address + Reg)^:=PLongWord(GPIO.Address + Reg)^ and not(1 shl Shift);
           end; 
         end
        else
         begin
          {Check Flags}
          if (Flags and GPIO_EVENT_FLAG_REPEAT) = 0 then
           begin
            {Get Register (Rising)}
            Reg:=BCM2837_GPREN0 + ((Pin div 32) * SizeOf(LongWord));
            
            {Remove Trigger (Rising)}
            PLongWord(GPIO.Address + Reg)^:=PLongWord(GPIO.Address + Reg)^ and not(1 shl Shift);
            
            {Get Register (Falling)}
            Reg:=BCM2837_GPFEN0 + ((Pin div 32) * SizeOf(LongWord));
            
            {Remove Trigger (Falling)}
            PLongWord(GPIO.Address + Reg)^:=PLongWord(GPIO.Address + Reg)^ and not(1 shl Shift);
           end; 
         end;     
       end; 
      
      {Clear Status}
      PLongWord(Bank.Address)^:=(BCM2837_GPEDS_MASK shl Bit);
      
      {Check Flags}
      if ((Flags and GPIO_EVENT_FLAG_INTERRUPT) = 0) or ((Flags and GPIO_EVENT_FLAG_REPEAT) = 0) then
       begin
        {Send Event}
        WorkerScheduleIRQ(CPU_AFFINITY_NONE,TWorkerTask(BCM2710GPIOEventTrigger),@GPIO.Pins[Bank.PinStart + Bit],nil);
       end
      else
       begin
        {Call Event (Only for Repeating Interrupt events)}
        Event:=GPIO.Pins[Bank.PinStart + Bit].Events;
        if (Event <> nil) and Assigned(Event.Callback) then
         begin
          Event.Callback(Event.Data,Pin,Trigger);
         end;
       end;    
      
      {Clear Bit}
      Status:=Status xor (1 shl Bit);
     end;
    
    {Memory Barrier}
    DataMemoryBarrier; {After the Last Read} 
   finally
    {Release the Lock}
    SpinUnlockIRQ(PBCM2710GPIODevice(GPIO).Lock);
   end;   
  end; 
end;

{==============================================================================}

procedure BCM2710GPIOEventTrigger(Pin:PGPIOPin);
var
 Count:LongWord;
 Flags:LongWord;
 Trigger:LongWord;
 GPIO:PGPIODevice;
 Next:PGPIOEvent;
 Event:PGPIOEvent;
 Events:PGPIOEvent;
 Single:TGPIOEvent;
 Current:PGPIOEvent;
begin
 {}
 {Check Pin}
 if Pin = nil then Exit;

 {Get GPIO}
 GPIO:=Pin.GPIO;
 if GPIO = nil then Exit;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'BCM2710: GPIO Event Trigger (Pin=' + GPIOPinToString(Pin.Pin) + ')');
 {$ENDIF}
 
 {Setup Count}
 Count:=0;
 
 {Setup Events}
 Events:=nil;

 {Setup Single}
 FillChar(Single,SizeOf(TGPIOEvent),0);
 
 {Get Flags}
 Flags:=Pin.Flags;
 
 {Get Trigger}
 Trigger:=Pin.Trigger;
 
 {Acquire the Lock}
 if MutexLock(GPIO.Lock) = ERROR_SUCCESS then
  begin
   try
    {Check Flags}
    if (Flags and GPIO_EVENT_FLAG_REPEAT) = 0 then
     begin
      {Signal Event}
      if Pin.Event <> INVALID_HANDLE_VALUE then
       begin
        EventPulse(Pin.Event);
       end;
      
      {Count Events}
      Event:=Pin.Events;
      while Event <> nil do
       begin
        Inc(Count);
        {Get Next}
        Event:=Event.Next;
       end;
      
      {Check Count}
      if Count > 0 then
       begin
        if Count = 1 then
         begin
          {Get Single}
          Event:=Pin.Events;
          if Event <> nil then
           begin
            Single.Callback:=Event.Callback;
            Single.Data:=Event.Data;
            
            {Save Next}
            Next:=Event.Next;
            
            {Deregister Event}
            GPIODeviceDeregisterEvent(GPIO,Pin,Event);
            
            {Check Timeout}
            if Event.Timeout = INFINITE then
             begin
              {Destroy Event}
              GPIODeviceDestroyEvent(GPIO,Event);
             end
            else
             begin
              {Set Timeout (Timeout will destroy event)}
              Event.Timeout:=INFINITE;
             end;
            
            {Get Next}
            Event:=Next;
           end;
         end
        else
         begin        
          {Allocate Events}
          Events:=GetMem(Count * SizeOf(TGPIOEvent));
          Current:=Events;
          
          {Get Events}
          Event:=Pin.Events;
          while Event <> nil do
           begin
            Current.Callback:=Event.Callback;
            Current.Data:=Event.Data;
            Current.Next:=nil;
            if Event.Next <> nil then
             begin
              Current.Next:=PGPIOEvent(PtrUInt(Current) + SizeOf(TGPIOEvent));
              Current:=Current.Next;
             end;
            
            {Save Next}
            Next:=Event.Next;
            
            {Deregister Event}
            GPIODeviceDeregisterEvent(GPIO,Pin,Event);
            
            {Check Timeout}
            if Event.Timeout = INFINITE then
             begin
              {Destroy Event}
              GPIODeviceDestroyEvent(GPIO,Event);
             end
            else
             begin
              {Set Timeout (Timeout will destroy event)}
              Event.Timeout:=INFINITE;
             end;
             
            {Get Next}
            Event:=Next;
           end;
         end;  
       end;
       
      {Reset Flags}
      Pin.Flags:=GPIO_EVENT_FLAG_NONE;
      
      {Reset Trigger}
      Pin.Trigger:=GPIO_TRIGGER_NONE;
      
      {Reset Count}
      Pin.Count:=0;
     end
    else
     begin    
      {Get Single}
      Event:=Pin.Events;
      if Event <> nil then
       begin
        Single.Callback:=Event.Callback;
        Single.Data:=Event.Data;
       end; 
     end; 
   finally
    {Release the Lock}
    MutexUnlock(GPIO.Lock);
   end; 
  end; 

 {Check Flags}  
 if (Flags and GPIO_EVENT_FLAG_REPEAT) = 0 then
  begin
   if Count > 0 then
    begin
     if Count = 1 then
      begin
       {Call Event}
       if Assigned(Single.Callback) then
        begin
         Single.Callback(Single.Data,Pin.Pin,Trigger);
        end;
      end
     else
      begin  
       {Get Events}
       Event:=Events;
       while Event <> nil do
        begin
         {Call Event}
         if Assigned(Event.Callback) then
          begin
           Event.Callback(Event.Data,Pin.Pin,Trigger);
          end;
         {Get Next} 
         Event:=Event.Next;
        end;
       
       {Free Events}
       FreeMem(Events);
      end; 
    end; 
  end
 else
  begin
   {Call Event}
   if Assigned(Single.Callback) then
    begin
     Single.Callback(Single.Data,Pin.Pin,Trigger);
    end;
  end;  
end;

{==============================================================================}

procedure BCM2710GPIOEventTimeout(Event:PGPIOEvent);
var
 Reg:LongWord;
 Pin:PGPIOPin;
 Shift:LongWord;
 GPIO:PGPIODevice;
begin
 {}
 {Check Event}
 if Event = nil then Exit;

 {Get Pin}
 Pin:=Event.Pin;
 if Pin = nil then Exit;
 
 {Get GPIO}
 GPIO:=Pin.GPIO;
 if GPIO = nil then Exit;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'BCM2710: GPIO Event Timeout (Pin=' + GPIOPinToString(Pin.Pin) + ' Event=' + IntToHex(LongWord(Event),8) + ')');
 {$ENDIF}
 
 {Acquire the Lock}
 if MutexLock(GPIO.Lock) = ERROR_SUCCESS then
  begin
   try
    {Check Timeout}
    if Event.Timeout = INFINITE then
     begin
      {Event must have been handled by trigger}
      
      {Destroy Event}
      GPIODeviceDestroyEvent(GPIO,Event);
     end
    else
     begin
      {Deregister Event}
      GPIODeviceDeregisterEvent(GPIO,Pin,Event);
      
      {Destroy Event}
      GPIODeviceDestroyEvent(GPIO,Event);
      
      {Decrement Count}
      Dec(Pin.Count);
    
      {Check Count}
      if Pin.Count = 0 then 
       begin
        {Check Trigger}
        if Pin.Trigger <> GPIO_TRIGGER_NONE then
         begin
          {Get Shift}
          Shift:=Pin.Pin mod 32;
         
          {Acquire the Lock}
          if SpinLockIRQ(PBCM2710GPIODevice(GPIO).Lock) <> ERROR_SUCCESS then Exit;
          
          {Memory Barrier}
          DataMemoryBarrier; {Before the First Write}
          
          {Check Trigger} 
          if Pin.Trigger <> GPIO_TRIGGER_EDGE then
           begin
            {Get Register (Trigger)}
            Reg:=BCM2710_GPIO_TRIGGER_MAP[Pin.Trigger] + ((Pin.Pin div 32) * SizeOf(LongWord));
            
            {Remove Trigger (Trigger)}
            PLongWord(GPIO.Address + Reg)^:=PLongWord(GPIO.Address + Reg)^ and not(1 shl Shift);
           end
          else
           begin 
            {Get Register (Rising)}
            Reg:=BCM2837_GPREN0 + ((Pin.Pin div 32) * SizeOf(LongWord));
            
            {Remove Trigger (Rising)}
            PLongWord(GPIO.Address + Reg)^:=PLongWord(GPIO.Address + Reg)^ and not(1 shl Shift);
          
            {Get Register (Falling)}
            Reg:=BCM2837_GPFEN0 + ((Pin.Pin div 32) * SizeOf(LongWord));
            
            {Remove Trigger (Falling)}
            PLongWord(GPIO.Address + Reg)^:=PLongWord(GPIO.Address + Reg)^ and not(1 shl Shift);
           end; 
          
          {Memory Barrier}
          DataMemoryBarrier; {After the Last Read} 
          
          {Release the Lock}
          SpinUnlockIRQ(PBCM2710GPIODevice(GPIO).Lock);
         
          {Reset the Flags}
          Pin.Flags:=GPIO_EVENT_FLAG_NONE;
          
          {Reset the Trigger}
          Pin.Trigger:=GPIO_TRIGGER_NONE;
         end; 
       end;
     end;
   finally
    {Release the Lock}
    MutexUnlock(GPIO.Lock);
   end; 
  end;    
end;

{==============================================================================}
{==============================================================================}
{BCM2710 UART0 Functions}
function BCM2710UART0Open(UART:PUARTDevice;BaudRate,DataBits,StopBits,Parity,FlowControl:LongWord):LongWord;
var
 Control:LongWord;
 Divisor:LongWord;
 LineControl:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check UART}
 if UART = nil then Exit;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(UART_DEBUG)}
 if UART_LOG_ENABLED then UARTLogDebug(UART,'BCM2710: UART0 Open (BaudRate=' + IntToStr(BaudRate) + ' DataBits=' + IntToStr(DataBits) + ' StopBits=' + IntToStr(StopBits) + ' Parity=' + IntToStr(Parity) + ' FlowControl=' + IntToStr(FlowControl) + ')');
 {$ENDIF}
 
 {Update Clock Rate}
 PBCM2710UART0Device(UART).ClockRate:=ClockGetRate(CLOCK_ID_UART0);
 if PBCM2710UART0Device(UART).ClockRate = 0 then PBCM2710UART0Device(UART).ClockRate:=BCM2710_UART0_CLOCK_RATE; 
 
 {Update Properties}
 UART.Properties.MaxRate:=PBCM2710UART0Device(UART).ClockRate div 16;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(UART_DEBUG)}
 if UART_LOG_ENABLED then UARTLogDebug(UART,'BCM2710:  ClockRate=' + IntToStr(PBCM2710UART0Device(UART).ClockRate) + ' MaxRate=' + IntToStr(UART.Properties.MaxRate));
 {$ENDIF}
 
 {Check Baud Rate}
 if ((BaudRate < BCM2710_UART0_MIN_BAUD) or (BaudRate > UART.Properties.MaxRate)) and (BaudRate <> SERIAL_BAUD_RATE_DEFAULT) then Exit;
 
 {Check Data Bits}
 if (DataBits < BCM2710_UART0_MIN_DATABITS) or (DataBits > BCM2710_UART0_MAX_DATABITS) then Exit;
 
 {Check Stop Bits}
 if (StopBits < BCM2710_UART0_MIN_STOPBITS) or (StopBits > BCM2710_UART0_MAX_STOPBITS) then Exit;
 
 {Check Parity}
 if Parity > BCM2710_UART0_MAX_PARITY then Exit;
 
 {Check Flow Control}
 if FlowControl > BCM2710_UART0_MAX_FLOW then Exit;
 
 {Adjust Baud Rate}
 if BaudRate = SERIAL_BAUD_RATE_DEFAULT then
  begin
   BaudRate:=SERIAL_BAUD_RATE_STANDARD;
   if (BaudRate > UART.Properties.MaxRate) then BaudRate:=SERIAL_BAUD_RATE_FALLBACK;
  end; 

 {Enable GPIO Pins}
 if BoardGetType = BOARD_TYPE_RPI3B then
  begin
   {On Raspberry Pi 3B UART0 may be connected to the Bluetooth on pins 32 and 33}
   GPIOFunctionSelect(GPIO_PIN_32,GPIO_FUNCTION_IN);
   GPIOFunctionSelect(GPIO_PIN_33,GPIO_FUNCTION_IN);
  end;
 GPIOPullSelect(GPIO_PIN_14,GPIO_PULL_NONE);
 GPIOFunctionSelect(GPIO_PIN_14,GPIO_FUNCTION_ALT0);
 GPIOPullSelect(GPIO_PIN_15,GPIO_PULL_UP);
 GPIOFunctionSelect(GPIO_PIN_15,GPIO_FUNCTION_ALT0);
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
  
 {Reset Control (Disable UART)}
 PBCM2837PL011Registers(PBCM2710UART0Device(UART).Address).CR:=0;
 
 {Reset Interrupt Mask (Disable Interrupts)}
 PBCM2837PL011Registers(PBCM2710UART0Device(UART).Address).IMSC:=0;
 
 {Ackowledge Interrupts}
 PBCM2837PL011Registers(PBCM2710UART0Device(UART).Address).ICR:=$7FF;
 
 {Reset Line Control (Flush FIFOs)}
 PBCM2837PL011Registers(PBCM2710UART0Device(UART).Address).LCRH:=0;
 
 {Calculate Divisor}
 if BaudRate > (PBCM2710UART0Device(UART).ClockRate div 16) then
  begin
   Divisor:=DivRoundClosest(PBCM2710UART0Device(UART).ClockRate * 8,BaudRate);
  end
 else
  begin
   Divisor:=DivRoundClosest(PBCM2710UART0Device(UART).ClockRate * 4,BaudRate);
  end;
  
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(UART_DEBUG)}
 if UART_LOG_ENABLED then UARTLogDebug(UART,'BCM2710:  BaudRate=' + IntToStr(BaudRate) + ' Divisor=' + IntToStr(Divisor) + ' Divisor shr 6=' + IntToStr(Divisor shr 6) + ' Divisor and $3F=' + IntToStr(Divisor and $3f));
 {$ENDIF}

 {Set Baud Rate}
 PBCM2837PL011Registers(PBCM2710UART0Device(UART).Address).FBRD:=Divisor and $3f;
 PBCM2837PL011Registers(PBCM2710UART0Device(UART).Address).IBRD:=Divisor shr 6;
  
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(UART_DEBUG)}
 if UART_LOG_ENABLED then UARTLogDebug(UART,'BCM2710:  Integer Divisor=' + IntToStr(PBCM2837PL011Registers(PBCM2710UART0Device(UART).Address).IBRD));
 if UART_LOG_ENABLED then UARTLogDebug(UART,'BCM2710:  Fractional Divisor=' + IntToStr(PBCM2837PL011Registers(PBCM2710UART0Device(UART).Address).FBRD));
 {$ENDIF}
  
 {Get Line Control}
 LineControl:=BCM2837_PL011_LCRH_FEN;
 {Data Bits}
 case DataBits of
  SERIAL_DATA_8BIT:LineControl:=LineControl or BCM2837_PL011_LCRH_WLEN8;
  SERIAL_DATA_7BIT:LineControl:=LineControl or BCM2837_PL011_LCRH_WLEN7;
  SERIAL_DATA_6BIT:LineControl:=LineControl or BCM2837_PL011_LCRH_WLEN6;
  SERIAL_DATA_5BIT:LineControl:=LineControl or BCM2837_PL011_LCRH_WLEN5;
 end;
 {Stop Bits}
 case StopBits of
  SERIAL_STOP_2BIT:LineControl:=LineControl or BCM2837_PL011_LCRH_STP2;
 end;
 {Parity}
 case Parity of
  SERIAL_PARITY_ODD:LineControl:=LineControl or BCM2837_PL011_LCRH_PEN;
  SERIAL_PARITY_EVEN:LineControl:=LineControl or BCM2837_PL011_LCRH_PEN or BCM2837_PL011_LCRH_EPS;
 end;
 
 {Set Line Control}
 PBCM2837PL011Registers(PBCM2710UART0Device(UART).Address).LCRH:=LineControl;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(UART_DEBUG)}
 if UART_LOG_ENABLED then UARTLogDebug(UART,'BCM2710:  Line Control=' + IntToHex(PBCM2837PL011Registers(PBCM2710UART0Device(UART).Address).LCRH,8));
 {$ENDIF}
 
 {Set Interrupt FIFO Level}
 PBCM2837PL011Registers(PBCM2710UART0Device(UART).Address).IFLS:=BCM2837_PL011_IFLS_RXIFLSEL1_8 or BCM2837_PL011_IFLS_TXIFLSEL1_2; {BCM2837_PL011_IFLS_RXIFLSEL1_2}

 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(UART_DEBUG)}
 if UART_LOG_ENABLED then UARTLogDebug(UART,'BCM2710:  Interrupt FIFO Level=' + IntToHex(PBCM2837PL011Registers(PBCM2710UART0Device(UART).Address).IFLS,8));
 {$ENDIF}
 
 {Get Control} 
 Control:=BCM2837_PL011_CR_RXE or BCM2837_PL011_CR_TXE or BCM2837_PL011_CR_UARTEN;
 {Flow Control}
 case FlowControl of
  SERIAL_FLOW_RTS_CTS:Control:=Control or BCM2837_PL011_CR_CTSEN or BCM2837_PL011_CR_RTSEN;
 end;
 
 {Create Receive Event (Manual Reset)}
 UART.ReceiveWait:=EventCreate(True,False);
 if UART.ReceiveWait = INVALID_HANDLE_VALUE then
  begin
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;

 {Create Transmit Event (Manual Reset / Intitial State)}
 UART.TransmitWait:=EventCreate(True,True);
 if UART.TransmitWait = INVALID_HANDLE_VALUE then
  begin
   EventDestroy(UART.ReceiveWait);
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;
 
 {Set Control (Enable UART)}
 PBCM2837PL011Registers(PBCM2710UART0Device(UART).Address).CR:=Control;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(UART_DEBUG)}
 if UART_LOG_ENABLED then UARTLogDebug(UART,'BCM2710:  Control=' + IntToHex(PBCM2837PL011Registers(PBCM2710UART0Device(UART).Address).CR,8));
 {$ENDIF}
 
 {Request IRQ}
 RequestIRQ(IRQ_ROUTING,BCM2837_IRQ_PL011,TInterruptHandler(BCM2710UART0InterruptHandler),UART);
 
 {Set Interrupt Mask (Enable Interrupts)}
 PBCM2837PL011Registers(PBCM2710UART0Device(UART).Address).IMSC:=BCM2837_PL011_IMSC_TXIM or BCM2837_PL011_IMSC_RXIM;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(UART_DEBUG)}
 if UART_LOG_ENABLED then UARTLogDebug(UART,'BCM2710:  Interrupt Mask=' + IntToHex(PBCM2837PL011Registers(PBCM2710UART0Device(UART).Address).IMSC,8));
 {$ENDIF}
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
 
 {Update Properties}
 UART.Properties.BaudRate:=BaudRate;
 UART.Properties.DataBits:=DataBits;
 UART.Properties.StopBits:=StopBits;
 UART.Properties.Parity:=Parity;
 UART.Properties.FlowControl:=FlowControl;

 {Return Result}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function BCM2710UART0Close(UART:PUARTDevice):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check UART}
 if UART = nil then Exit;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(UART_DEBUG)}
 if UART_LOG_ENABLED then UARTLogDebug(UART,'BCM2710: UART0 Close');
 {$ENDIF}
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Reset Interrupt Mask (Disable Interrupts)}
 PBCM2837PL011Registers(PBCM2710UART0Device(UART).Address).IMSC:=0;
 
 {Acknowledge Interrupts}
 PBCM2837PL011Registers(PBCM2710UART0Device(UART).Address).ICR:=$7FF;
 
 {Release IRQ}
 ReleaseIRQ(IRQ_ROUTING,BCM2837_IRQ_PL011,TInterruptHandler(BCM2710UART0InterruptHandler),UART);
 
 {Reset Control (Disable UART)}
 PBCM2837PL011Registers(PBCM2710UART0Device(UART).Address).CR:=0;
 
 {Destroy Transmit Event}
 EventDestroy(UART.TransmitWait);
 
 {Destroy Receive Event}
 EventDestroy(UART.ReceiveWait);
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
 
 {Update Properties}
 UART.Properties.BaudRate:=SERIAL_BAUD_RATE_DEFAULT;
 UART.Properties.DataBits:=SERIAL_DATA_8BIT;
 UART.Properties.StopBits:=SERIAL_STOP_1BIT;
 UART.Properties.Parity:=SERIAL_PARITY_NONE;
 UART.Properties.FlowControl:=SERIAL_FLOW_NONE;
 
 {Return Result}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}
 
function BCM2710UART0Read(UART:PUARTDevice;Buffer:Pointer;Size,Flags:LongWord;var Count:LongWord):LongWord;
var
 Value:LongWord;
 Total:LongWord;
 Offset:LongWord;
 Status:LongWord;
begin
 {}
 {Setup Result}
 Count:=0;
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Buffer}
 if Buffer = nil then Exit;
 
 {Check UART}
 if UART = nil then Exit;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(UART_DEBUG)}
 if UART_LOG_ENABLED then UARTLogDebug(UART,'BCM2710: UART0 Read (Size=' + IntToStr(Size) + ')');
 {$ENDIF}
 
 {Read to Buffer}
 Offset:=0;
 Total:=Size;
 while Size > 0 do
  begin
   {Check Non Blocking}
   if ((Flags and UART_READ_NON_BLOCK) <> 0) and (EventState(UART.ReceiveWait) <> EVENT_STATE_SIGNALED) then
    begin
     Result:=ERROR_NO_MORE_ITEMS;
     Break;
    end;
 
   {Release the Lock}
   MutexUnlock(UART.Lock);
   
   {Wait for Data}
   if EventWait(UART.ReceiveWait) = ERROR_SUCCESS then
    begin
     {Acquire the Lock}
     if MutexLock(UART.Lock) = ERROR_SUCCESS then
      begin
       {Memory Barrier}
       DataMemoryBarrier; {Before the First Write}
 
       {Get Status}
       Status:=PBCM2837PL011Registers(PBCM2710UART0Device(UART).Address).FR;
       while ((Status and BCM2837_PL011_FR_RXFE) = 0) and (Size > 0) do
        begin
         {Read Data}
         Value:=PBCM2837PL011Registers(PBCM2710UART0Device(UART).Address).DR;
         
         {Check for Error}
         if (Value and BCM2837_PL011_DR_ERROR) <> 0 then
          begin
           {Check Error}
           if (Value and BCM2837_PL011_DR_OE) <> 0 then
            begin
             if UART_LOG_ENABLED then UARTLogError(UART,'BCM2710: Overrun error on receive character'); 
            end;
           if (Value and BCM2837_PL011_DR_BE) <> 0 then
            begin
             if UART_LOG_ENABLED then UARTLogError(UART,'BCM2710: Break error on receive character'); 
            end;
           if (Value and BCM2837_PL011_DR_PE) <> 0 then
            begin
             if UART_LOG_ENABLED then UARTLogError(UART,'BCM2710: Parity error on receive character'); 
            end;
           if (Value and BCM2837_PL011_DR_FE) <> 0 then
            begin
             if UART_LOG_ENABLED then UARTLogError(UART,'BCM2710: Framing error on receive character'); 
            end;
           
           {Update Statistics}
           Inc(UART.ReceiveErrors);
          end;
          
         {Save Data}
         PByte(Buffer + Offset)^:=Value and BCM2837_PL011_DR_DATA;
         
         {Update Statistics}
         Inc(UART.ReceiveCount);
         
         {Update Count}
         Inc(Count);
         
         {Update Size and Offset}
         Dec(Size);
         Inc(Offset);
         
         {Get Status}
         Status:=PBCM2837PL011Registers(PBCM2710UART0Device(UART).Address).FR;
        end;
        
       {Check Status}
       if (Status and BCM2837_PL011_FR_RXFE) <> 0 then
        begin
         {Reset Event}
         EventReset(UART.ReceiveWait);
        end;        
 
       {Memory Barrier}
       DataMemoryBarrier; {After the Last Read} 
      end
     else
      begin
       Result:=ERROR_CAN_NOT_COMPLETE;
       Exit;
      end;      
    end
   else
    begin
     Result:=ERROR_CAN_NOT_COMPLETE;
     Exit;
    end;    
  end;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(UART_DEBUG)}
 if UART_LOG_ENABLED then UARTLogDebug(UART,'BCM2710:  Return Count=' + IntToStr(Count));
 {$ENDIF}
 
 {Return Result}
 if (Total = Count) then Result:=ERROR_SUCCESS; 
end;

{==============================================================================}

function BCM2710UART0Write(UART:PUARTDevice;Buffer:Pointer;Size,Flags:LongWord;var Count:LongWord):LongWord;
var
 Total:LongWord;
 Offset:LongWord;
 Status:LongWord;
begin
 {}
 {Setup Result}
 Count:=0;
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Buffer}
 if Buffer = nil then Exit;
 
 {Check UART}
 if UART = nil then Exit;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(UART_DEBUG)}
 if UART_LOG_ENABLED then UARTLogDebug(UART,'BCM2710: UART0 Write (Size=' + IntToStr(Size) + ')');
 {$ENDIF}
 
 {Write from Buffer}
 Offset:=0;
 Total:=Size;
 while Size > 0 do
  begin
   {Check Non Blocking}
   if ((Flags and UART_WRITE_NON_BLOCK) <> 0) and (EventState(UART.TransmitWait) <> EVENT_STATE_SIGNALED) then
    begin
     Result:=ERROR_INSUFFICIENT_BUFFER;
     Break;
    end;
   
   {Release the Lock}
   MutexUnlock(UART.Lock);
   
   {Wait for Space}
   if EventWait(UART.TransmitWait) = ERROR_SUCCESS then
    begin
     {Acquire the Lock}
     if MutexLock(UART.Lock) = ERROR_SUCCESS then
      begin
       {Memory Barrier}
       DataMemoryBarrier; {Before the First Write}
      
       {Get Status}
       Status:=PBCM2837PL011Registers(PBCM2710UART0Device(UART).Address).FR;
       while ((Status and BCM2837_PL011_FR_TXFF) = 0) and (Size > 0) do
        begin
         {Write Data}
         PBCM2837PL011Registers(PBCM2710UART0Device(UART).Address).DR:=PByte(Buffer + Offset)^;
         
         {Update Statistics}
         Inc(UART.TransmitCount);
         
         {Update Count}
         Inc(Count);
         
         {Update Size and Offset}
         Dec(Size);
         Inc(Offset);
         
         {Get Status}
         Status:=PBCM2837PL011Registers(PBCM2710UART0Device(UART).Address).FR;
        end;
        
       {Check Status}
       if (Status and BCM2837_PL011_FR_TXFF) <> 0 then
        begin
         {Reset Event}
         EventReset(UART.TransmitWait);
        end;        
      
       {Memory Barrier}
       DataMemoryBarrier; {After the Last Read} 
      end
     else
      begin
       Result:=ERROR_CAN_NOT_COMPLETE;
       Exit;
      end;      
    end
   else
    begin
     Result:=ERROR_CAN_NOT_COMPLETE;
     Exit;
    end;    
  end;
  
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(UART_DEBUG)}
 if UART_LOG_ENABLED then UARTLogDebug(UART,'BCM2710:  Return Count=' + IntToStr(Count));
 {$ENDIF}
 
 {Return Result}
 if (Total = Count) then Result:=ERROR_SUCCESS;
end;

{==============================================================================}
 
function BCM2710UART0Status(UART:PUARTDevice):LongWord;
var
 Flags:LongWord;
 Status:LongWord;
 Control:LongWord;
begin
 {}
 Result:=UART_STATUS_NONE;
 
 {Check UART}
 if UART = nil then Exit;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(UART_DEBUG)}
 if UART_LOG_ENABLED then UARTLogDebug(UART,'BCM2710: UART0 Status');
 {$ENDIF}
 
 {Get Flags}
 Flags:=PBCM2837PL011Registers(PBCM2710UART0Device(UART).Address).FR;
 if (Flags and BCM2837_PL011_FR_CTS) <> 0 then
  begin
   Result:=Result or UART_STATUS_CTS;
  end;
 if (Flags and BCM2837_PL011_FR_RXFF) <> 0 then
  begin
   Result:=Result or UART_STATUS_RX_FULL;
  end;
 if (Flags and BCM2837_PL011_FR_RXFE) <> 0 then
  begin
   Result:=Result or UART_STATUS_RX_EMPTY;
  end;
 if (Flags and BCM2837_PL011_FR_TXFF) <> 0 then
  begin
   Result:=Result or UART_STATUS_TX_FULL;
  end;
 if (Flags and BCM2837_PL011_FR_TXFE) <> 0 then
  begin
   Result:=Result or UART_STATUS_TX_EMPTY;
  end;
 if (Flags and BCM2837_PL011_FR_BUSY) <> 0 then
  begin
   Result:=Result or UART_STATUS_BUSY;
  end;
 
 {Get Status}
 Status:=PBCM2837PL011Registers(PBCM2710UART0Device(UART).Address).RSRECR;
 if (Status and BCM2837_PL011_RSRECR_OE) <> 0 then
  begin
   Result:=Result or UART_STATUS_OVERRUN_ERROR;
  end;
 if (Status and BCM2837_PL011_RSRECR_BE) <> 0 then
  begin
   Result:=Result or UART_STATUS_BREAK_ERROR;
  end;
 if (Status and BCM2837_PL011_RSRECR_PE) <> 0 then
  begin
   Result:=Result or UART_STATUS_PARITY_ERROR;
  end;
 if (Status and BCM2837_PL011_RSRECR_FE) <> 0 then
  begin
   Result:=Result or UART_STATUS_FRAMING_ERROR;
  end;

 {Get Control}
 Control:=PBCM2837PL011Registers(PBCM2710UART0Device(UART).Address).CR;
 if (Control and BCM2837_PL011_CR_RTS) <> 0 then
  begin
   Result:=Result or UART_STATUS_RTS;
  end;
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
end;

{==============================================================================}

procedure BCM2710UART0InterruptHandler(UART:PUARTDevice);
var
 Status:LongWord;
begin
 {}
 {Check UART}
 if UART = nil then Exit;
 
 {Update Statistics}
 Inc(PBCM2710UART0Device(UART).InterruptCount);
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 {Get Interrupt Status}
 Status:=PBCM2837PL011Registers(PBCM2710UART0Device(UART).Address).MIS;
 if Status <> 0 then
  begin
   {Acknowledge Interrupts}
   PBCM2837PL011Registers(PBCM2710UART0Device(UART).Address).ICR:=Status and not(BCM2837_PL011_ICR_TXIC or BCM2837_PL011_ICR_RXIC);
   
   {Check Transmit}
   if (Status and BCM2837_PL011_MIS_TXMIS) <> 0 then
    begin
     {Acknowledge Transmit}
     PBCM2837PL011Registers(PBCM2710UART0Device(UART).Address).ICR:=BCM2837_PL011_ICR_TXIC;
     
     {Send Transmit}
     WorkerScheduleIRQ(CPU_AFFINITY_NONE,TWorkerTask(BCM2710UART0Transmit),UART,nil);
    end;
    
   {Check Receive}
   if (Status and BCM2837_PL011_MIS_RXMIS) <> 0 then
    begin
     {Acknowledge Receive}
     PBCM2837PL011Registers(PBCM2710UART0Device(UART).Address).ICR:=BCM2837_PL011_ICR_RXIC;

     {Send Receive}
     WorkerScheduleIRQ(CPU_AFFINITY_NONE,TWorkerTask(BCM2710UART0Receive),UART,nil);
    end;
  end; 
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
end;

{==============================================================================}

procedure BCM2710UART0Receive(UART:PUARTDevice);
begin
 {}
 {Check UART}
 if UART = nil then Exit;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(UART_DEBUG)}
 if UART_LOG_ENABLED then UARTLogDebug(UART,'BCM2710: UART0 Receive');
 {$ENDIF}
 
 {Acquire the Lock}
 if MutexLock(UART.Lock) = ERROR_SUCCESS then
  begin
   {Set Event}
   EventSet(UART.ReceiveWait);
   
   {Check Mode}
   if UART.UARTMode = UART_MODE_SERIAL then
    begin
     {Serial Receive}
     UARTSerialDeviceReceive(UART);
    end;

   {Release the Lock}
   MutexUnlock(UART.Lock);
  end;
end;

{==============================================================================}

procedure BCM2710UART0Transmit(UART:PUARTDevice);
begin
 {}
 {Check UART}
 if UART = nil then Exit;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(UART_DEBUG)}
 if UART_LOG_ENABLED then UARTLogDebug(UART,'BCM2710: UART0 Transmit');
 {$ENDIF}
 
 {Acquire the Lock}
 if MutexLock(UART.Lock) = ERROR_SUCCESS then
  begin
   {Set Event}
   EventSet(UART.TransmitWait);
   
   {Check Mode}
   if UART.UARTMode = UART_MODE_SERIAL then
    begin
     {Serial Transmit}
     UARTSerialDeviceTransmit(UART);
    end;    

   {Release the Lock}
   MutexUnlock(UART.Lock);
  end;
end;

{==============================================================================}
{==============================================================================}
{BCM2710 UART1 Functions}

{==============================================================================}
{==============================================================================}
{BCM2710 SDHCI Functions}
function BCM2710SDHCIHostStart(SDHCI:PSDHCIHost):LongWord;
var
 Status:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check SDHCI}
 if SDHCI = nil then Exit;
 
 if MMC_LOG_ENABLED then MMCLogInfo(nil,'SDHCI BCM2710 Powering on Arasan SD Host Controller');

 {Power On SD}
 Status:=PowerOn(POWER_ID_MMC0);
 if Status <> ERROR_SUCCESS then
  begin
   if MMC_LOG_ENABLED then MMCLogError(nil,'SDHCI BCM2710 Failed to power on Arasan SD Host Controller');
   
   Result:=Status;
   Exit;
  end;
 
 {Update SDHCI}
 {Driver Properties}
 if BCM2710SDHCI_FIQ_ENABLED then
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
 SDHCI.ClockMinimum:=BCM2710_EMMC_MIN_FREQ;
 SDHCI.ClockMaximum:=BCM2710_EMMC_MAX_FREQ; //To Do //Get from somewhere //See above
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(MMC_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI BCM2710 host version = ' + IntToHex(SDHCIGetVersion(SDHCI),4));
 {$ENDIF}
 
 {Update BCM2710}
 PBCM2710SDHCIHost(SDHCI).WriteDelay:=((2 * 1000000) div BCM2710_EMMC_MIN_FREQ) + 1;  //To Do //Get sdhci-BCM2710.emmc_clock_freq from command line (or get from Mailbox Properties ?) //No, probably command line is best. Platform startup can get from Mailbox and place in command line //see above
 PBCM2710SDHCIHost(SDHCI).LastWrite:=0;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(MMC_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI BCM2710 host write delay =  ' + IntToStr(PBCM2710SDHCIHost(SDHCI).WriteDelay));
 {$ENDIF}
 
 {Reset Host}
 SDHCIHostReset(SDHCI,SDHCI_RESET_ALL);
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(MMC_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI BCM2710 host reset completed');
 {$ENDIF}
 
 {Setup Interrupts}
 Result:=BCM2710SDHCISetupInterrupts(SDHCI);
 
 //See: bcm2835_sdhci_init in bcm2835_sdhci.c
end;

{==============================================================================}

function BCM2710SDHCIHostStop(SDHCI:PSDHCIHost):LongWord;
var
 Status:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check SDHCI}
 if SDHCI = nil then Exit;

 {Release the IRQ/FIQ}
 if BCM2710SDHCI_FIQ_ENABLED then
  begin
   ReleaseFIQ(FIQ_ROUTING,BCM2837_IRQ_SDHCI,TInterruptHandler(BCM2710SDHCIInterruptHandler),SDHCI);
  end
 else
  begin
   ReleaseIRQ(IRQ_ROUTING,BCM2837_IRQ_SDHCI,TInterruptHandler(BCM2710SDHCIInterruptHandler),SDHCI);
  end;  
 
 {Clear Interrupts}
 SDHCI.Interrupts:=0;
 
 {Reset Host}
 SDHCIHostReset(SDHCI,SDHCI_RESET_ALL);
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(MMC_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI BCM2710 host reset completed');
 {$ENDIF}

 {Update SDHCI}
 {Driver Properties}
 SemaphoreDestroy(SDHCI.Wait);
 SDHCI.Wait:=INVALID_HANDLE_VALUE;
 
 if MMC_LOG_ENABLED then MMCLogInfo(nil,'SDHCI BCM2710 Powering off Arasan SD Host Controller');

 {Power Off SD}
 Status:=PowerOff(POWER_ID_MMC0);
 if Status <> ERROR_SUCCESS then
  begin
   if MMC_LOG_ENABLED then MMCLogError(nil,'SDHCI BCM2710 Failed to power off Arasan SD Host Controller');
   
   Result:=Status;
   Exit;
  end;
  
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function BCM2710SDHCIHostReadByte(SDHCI:PSDHCIHost;Reg:LongWord):Byte; 
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

function BCM2710SDHCIHostReadWord(SDHCI:PSDHCIHost;Reg:LongWord):Word; 
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

function BCM2710SDHCIHostReadLong(SDHCI:PSDHCIHost;Reg:LongWord):LongWord; 
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

procedure BCM2710SDHCIHostWriteByte(SDHCI:PSDHCIHost;Reg:LongWord;Value:Byte); 
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
 BCM2710SDHCIHostWriteLong(SDHCI,Reg and not(3),NewValue);

 //See: bcm2835_sdhci_writeb in bcm2835_sdhci.c
end;

{==============================================================================}

procedure BCM2710SDHCIHostWriteWord(SDHCI:PSDHCIHost;Reg:LongWord;Value:Word); 
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
   OldValue:=PBCM2710SDHCIHost(SDHCI).ShadowRegister;
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
   PBCM2710SDHCIHost(SDHCI).ShadowRegister:=NewValue;
  end
 else
  begin
   {Write LongWord}
   BCM2710SDHCIHostWriteLong(SDHCI,Reg and not(3),NewValue);
  end;  
  
 //See: bcm2835_sdhci_writew in bcm2835_sdhci.c
end;

{==============================================================================}

procedure BCM2710SDHCIHostWriteLong(SDHCI:PSDHCIHost;Reg:LongWord;Value:LongWord); 
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
 MicrosecondDelay(PBCM2710SDHCIHost(SDHCI).WriteDelay);
 
 //To Do //Need GetTimerMicroseconds() in Platform (with a Since value as a Parameter ?) //Then use the LastWrite value in SDHCI
         //Also add GetTimerMilliseconds() in Platform as well
               
 //See: bcm2835_sdhci_raw_writel in bcm2835_sdhci.c
 //     bcm2835_sdhci_writel in bcm2835_sdhci.c
end;

{==============================================================================}

procedure BCM2710SDHCIInterruptHandler(SDHCI:PSDHCIHost);
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
 
 {$IF (DEFINED(BCM2710_DEBUG) or DEFINED(MMC_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI BCM2710 Interrupt Handler');
 {$ENDIF}
 
 {Get Interrupt Mask}
 InterruptMask:=SDHCIHostReadLong(SDHCI,SDHCI_INT_STATUS);

 {$IF (DEFINED(BCM2710_DEBUG) or DEFINED(MMC_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI BCM2710 Interrupt Handler (InterruptMask=' + IntToHex(InterruptMask,8) + ')');
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
   
   {$IF (DEFINED(BCM2710_DEBUG) or DEFINED(MMC_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
   if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI BCM2710 Interrupt Handler (AcknowledgeMask=' + IntToHex(AcknowledgeMask,8) + ')');
   {$ENDIF}
   
   {Check for insert / remove interrupts}
   if (InterruptMask and (SDHCI_INT_CARD_INSERT or SDHCI_INT_CARD_REMOVE)) <> 0 then
    begin
     {$IF (DEFINED(BCM2710_DEBUG) or DEFINED(MMC_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
     if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI BCM2710 Insert / Remove Interrupt (InterruptMask=' + IntToHex(InterruptMask,8) + ')');
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
     {$IF (DEFINED(BCM2710_DEBUG) or DEFINED(MMC_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
     if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI BCM2710 Command Interrupt (InterruptMask=' + IntToHex(InterruptMask,8) + ')');
     {$ENDIF}
     
     SDHCIHostCommandInterrupt(SDHCI,InterruptMask and SDHCI_INT_CMD_MASK,InterruptMask);
    end;
    
   {Check for data interrupts} 
   if (InterruptMask and SDHCI_INT_DATA_MASK) <> 0 then
    begin
     {$IF (DEFINED(BCM2710_DEBUG) or DEFINED(MMC_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
     if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI BCM2710 Data Interrupt (InterruptMask=' + IntToHex(InterruptMask,8) + ')');
     {$ENDIF}
     
     SDHCIHostDataInterrupt(SDHCI,InterruptMask and SDHCI_INT_DATA_MASK);
    end;
   
   {Check for bus power interrupt}
   if (InterruptMask and SDHCI_INT_BUS_POWER) <> 0 then
    begin
     {$IF (DEFINED(BCM2710_DEBUG) or DEFINED(MMC_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
     if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI BCM2710 Bus Power Interrupt (InterruptMask=' + IntToHex(InterruptMask,8) + ')');
     {$ENDIF}
     
     //To Do //Log Error
    end;
 
   {Check for card interrupt}
   if (InterruptMask and SDHCI_INT_CARD_INT) <> 0 then
    begin
     {$IF (DEFINED(BCM2710_DEBUG) or DEFINED(MMC_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
     if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI BCM2710 Card Interrupt (InterruptMask=' + IntToHex(InterruptMask,8) + ')');
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
   {$IF (DEFINED(BCM2710_DEBUG) or DEFINED(MMC_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
   if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI BCM2710 Unexpected Interrupt (UnexpectedMask=' + IntToHex(UnexpectedMask,8) + ')');
   {$ENDIF}
   
   //To Do //Log Error
  end;
  
 {$IF (DEFINED(BCM2710_DEBUG) or DEFINED(MMC_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'SDHCI BCM2710 Interrupt Handler completed');
 {$ENDIF}
  
 //See: bcm2835_mmc_irq in \linux-rpi-3.18.y\drivers\mmc\host\bcm2835-mmc.c
end;

{==============================================================================}

function BCM2710SDHCISetupInterrupts(SDHCI:PSDHCIHost):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check SDHCI}
 if SDHCI = nil then Exit;
 
 {Setup Interrupts}
 SDHCI.Interrupts:=SDHCI_INT_BUS_POWER or SDHCI_INT_DATA_END_BIT or SDHCI_INT_DATA_CRC or SDHCI_INT_DATA_TIMEOUT or SDHCI_INT_INDEX or SDHCI_INT_END_BIT or SDHCI_INT_CRC or SDHCI_INT_TIMEOUT or SDHCI_INT_DATA_END or SDHCI_INT_RESPONSE;
                   //SDHCI_INT_CARD_INSERT or SDHCI_INT_CARD_REMOVE or //See sdhci_set_card_detection in \linux-rpi-3.18.y\drivers\mmc\host\sdhci.c
                   //Note: SDHCI_INT_CARD_INSERT seems to hang everything, why? //Because the SDHCI_CARD_PRESENT bit is never updated !
                   
 {Enable Interrupts}
 SDHCIHostWriteLong(SDHCI,SDHCI_INT_ENABLE,SDHCI.Interrupts);
 SDHCIHostWriteLong(SDHCI,SDHCI_SIGNAL_ENABLE,SDHCI.Interrupts);

 {Request the IRQ/FIQ} 
 if BCM2710SDHCI_FIQ_ENABLED then
  begin
   RequestFIQ(FIQ_ROUTING,BCM2837_IRQ_SDHCI,TInterruptHandler(BCM2710SDHCIInterruptHandler),SDHCI);
  end
 else
  begin
   RequestIRQ(IRQ_ROUTING,BCM2837_IRQ_SDHCI,TInterruptHandler(BCM2710SDHCIInterruptHandler),SDHCI);
  end;  
 
 {Return Result}
 Result:=ERROR_SUCCESS;
 
 //See: \linux-rpi-3.18.y\drivers\mmc\host\bcm2835-mmc.c
end;
 
{==============================================================================}
 
function BCM2710MMCDeviceGetCardDetect(MMC:PMMCDevice):LongWord;
{Implementation of MMC GetCardDetect for the BCM2710 which does not update the
 bits in the SDHCI_PRESENT_STATE register to reflect card insertion or removal}
var
 SDHCI:PSDHCIHost;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;
 
 {Check MMC}
 if MMC = nil then Exit;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(MMC_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC BCM2710 Get Card Detect');
 {$ENDIF}
 
 {Get SDHCI}
 SDHCI:=PSDHCIHost(MMC.Device.DeviceData);
 if SDHCI = nil then Exit;
 
 {$IF DEFINED(BCM2710_DEBUG) or DEFINED(MMC_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC BCM2710 Get Card Detect (SDHCI_PRESENT_STATE=' + IntToHex(SDHCIHostReadLong(SDHCI,SDHCI_PRESENT_STATE),8) + ')');
 {$ENDIF}
 
 {Check MMC State}
 if MMC.MMCState = MMC_STATE_INSERTED then
  begin
   {Get Card Status}
   if MMCDeviceSendCardStatus(MMC) <> MMC_STATUS_SUCCESS then
    begin
     {Update Flags}
     MMC.Device.DeviceFlags:=MMC.Device.DeviceFlags and not(MMC_FLAG_CARD_PRESENT);
     
     {Reset Host}
     SDHCIHostReset(SDHCI,SDHCI_RESET_ALL);

     {$IF DEFINED(BCM2710_DEBUG) or DEFINED(MMC_DEBUG)}
     if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC BCM2710 Get Card Detect (Flags=not MMC_FLAG_CARD_PRESENT)');
     {$ENDIF}
    end;
  end
 else
  begin
   {Get Card Present}
   if (SDHCIHostReadLong(SDHCI,SDHCI_PRESENT_STATE) and SDHCI_CARD_PRESENT) <> 0 then
    begin
     {Update Flags}
     MMC.Device.DeviceFlags:=(MMC.Device.DeviceFlags or MMC_FLAG_CARD_PRESENT);
     
     {$IF DEFINED(BCM2710_DEBUG) or DEFINED(MMC_DEBUG)}
     if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC BCM2710 Get Card Detect (Flags=MMC_FLAG_CARD_PRESENT)');
     {$ENDIF}
    end
   else
    begin
     {Update Flags}
     MMC.Device.DeviceFlags:=MMC.Device.DeviceFlags and not(MMC_FLAG_CARD_PRESENT);
     
     {$IF DEFINED(BCM2710_DEBUG) or DEFINED(MMC_DEBUG)}
     if MMC_LOG_ENABLED then MMCLogDebug(nil,'MMC BCM2710 Get Card Detect (Flags=not MMC_FLAG_CARD_PRESENT)');
     {$ENDIF}
    end;    
  end;

 Result:=MMC_STATUS_SUCCESS;  
end;
 
{==============================================================================}
{==============================================================================}
{BCM2710 Clock Functions}
function BCM2710ClockRead(Clock:PClockDevice):LongWord;
begin
 {}
 Result:=0;
 
 {Check Clock}
 if Clock = nil then Exit;
 if Clock.Address = nil then Exit;

 if MutexLock(Clock.Lock) <> ERROR_SUCCESS then Exit;
 
 {Read Clock}
 Result:=PBCM2837SystemTimerRegisters(Clock.Address).CLO;
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read}
 
 {Update Statistics}
 Inc(Clock.ReadCount);
 
 MutexUnlock(Clock.Lock);
end;
 
{==============================================================================}

function BCM2710ClockRead64(Clock:PClockDevice):Int64;
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
 Int64Rec(Result).Hi:=PBCM2837SystemTimerRegisters(Clock.Address).CHI;
 
 {Get Low Value}
 Int64Rec(Result).Lo:=PBCM2837SystemTimerRegisters(Clock.Address).CLO;
 
 {Check High Value}
 Check:=PBCM2837SystemTimerRegisters(Clock.Address).CHI;
 if Check <> Int64Rec(Result).Hi then
  begin
   {Rollover Occurred, Get Low Value Again}
   Int64Rec(Result).Hi:=Check;
   Int64Rec(Result).Lo:=PBCM2837SystemTimerRegisters(Clock.Address).CLO;
  end;
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read}
 
 {Update Statistics}
 Inc(Clock.ReadCount);
 
 MutexUnlock(Clock.Lock);
end;

{==============================================================================}
{==============================================================================}
{BCM2710 Timer Functions}
 
{==============================================================================}
{==============================================================================}
{BCM2710 Random Functions}
function BCM2710RandomStart(Random:PRandomDevice):LongWord;
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
    PBCM2837RNGRegisters(Random.Address).Status:=BCM2710_RANDOM_WARMUP_COUNT;
    PBCM2837RNGRegisters(Random.Address).Control:=BCM2837_RANDOM_ENABLE;
   
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

function BCM2710RandomStop(Random:PRandomDevice):LongWord;
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
    PBCM2837RNGRegisters(Random.Address).Control:=BCM2837_RANDOM_DISABLE;
   
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

function BCM2710RandomReadLongWord(Random:PRandomDevice):LongWord;
begin
 {}
 Result:=0;
 
 {Check Random}
 if Random = nil then Exit;
 if Random.Address = nil then Exit;
 
 if MutexLock(Random.Lock) <> ERROR_SUCCESS then Exit;
 
 {Check Status}
 while (PBCM2837RNGRegisters(Random.Address).Status shr 24) = 0 do
  begin
   ThreadSleep(0);
  end;
  
 {Read Random}
 Result:=PBCM2837RNGRegisters(Random.Address).Data; 

 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
 
 {Update Statistics}
 Inc(Random.ReadCount);
 
 MutexUnlock(Random.Lock);
end;

{==============================================================================}
{==============================================================================}
{BCM2710 Mailbox Functions}

{==============================================================================}
{==============================================================================}
{BCM2710 Watchdog Functions}
function BCM2710WatchdogStart(Watchdog:PWatchdogDevice):LongWord;
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
    PBCM2837PMWatchdogRegisters(Watchdog.Address).WDOG:=BCM2837_PM_PASSWORD or ((Watchdog.Timeout * BCM2837_PM_WDOG_TICKS_PER_MILLISECOND) and BCM2837_PM_WDOG_TIME_MASK);
    
    Current:=PBCM2837PMWatchdogRegisters(Watchdog.Address).RSTC;
    
    PBCM2837PMWatchdogRegisters(Watchdog.Address).RSTC:=BCM2837_PM_PASSWORD or (Current and BCM2837_PM_RSTC_WRCFG_CLR) or BCM2837_PM_RSTC_WRCFG_FULL_RESET;

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

function BCM2710WatchdogStop(Watchdog:PWatchdogDevice):LongWord;
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
    PBCM2837PMWatchdogRegisters(Watchdog.Address).RSTC:=BCM2837_PM_PASSWORD or BCM2837_PM_RSTC_RESET;
    
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

function BCM2710WatchdogRefresh(Watchdog:PWatchdogDevice):LongWord;
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
    PBCM2837PMWatchdogRegisters(Watchdog.Address).WDOG:=BCM2837_PM_PASSWORD or ((Watchdog.Timeout * BCM2837_PM_WDOG_TICKS_PER_MILLISECOND) and BCM2837_PM_WDOG_TIME_MASK);
    
    Current:=PBCM2837PMWatchdogRegisters(Watchdog.Address).RSTC;
    
    PBCM2837PMWatchdogRegisters(Watchdog.Address).RSTC:=BCM2837_PM_PASSWORD or (Current and BCM2837_PM_RSTC_WRCFG_CLR) or BCM2837_PM_RSTC_WRCFG_FULL_RESET;

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

function BCM2710WatchdogGetRemain(Watchdog:PWatchdogDevice):LongWord;
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
    Result:=(PBCM2837PMWatchdogRegisters(Watchdog.Address).WDOG and BCM2837_PM_WDOG_TIME_MASK) div BCM2837_PM_WDOG_TICKS_PER_MILLISECOND;

    {Memory Barrier}
    DataMemoryBarrier; {After the Last Read} 
   finally
    MutexUnlock(Watchdog.Lock);
   end; 
  end;
end;

{==============================================================================}
{==============================================================================}
{BCM2710 Framebuffer Functions}
function BCM2710FramebufferAllocate(Framebuffer:PFramebufferDevice;Properties:PFramebufferProperties):LongWord;
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2837MailboxHeader;
 Footer:PBCM2837MailboxFooter;
 Defaults:TFramebufferProperties;
 Tag:PBCM2837MailboxTagCreateBuffer;
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
        if DEVICE_LOG_ENABLED then DeviceLogError(nil,'BCM2710: FramebufferAllocate - FramebufferGetDimensions failed: ' + ErrorToString(Result));
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
    Size:=SizeOf(TBCM2837MailboxHeader) + SizeOf(TBCM2837MailboxTagCreateBuffer) + SizeOf(TBCM2837MailboxFooter);
    
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
     Header.Code:=BCM2837_MBOX_REQUEST_CODE;
    
     {Setup Tag}
     Tag:=PBCM2837MailboxTagCreateBuffer(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
     
     {Setup Tag (Physical)}
     Tag.Physical.Header.Tag:=BCM2837_MBOX_TAG_SET_PHYSICAL_W_H;
     Tag.Physical.Header.Size:=SizeOf(TBCM2837MailboxTagSetPhysical) - SizeOf(TBCM2837MailboxTagHeader);
     Tag.Physical.Header.Length:=SizeOf(Tag.Physical.Request);
     Tag.Physical.Request.Width:=Defaults.PhysicalWidth;
     Tag.Physical.Request.Height:=Defaults.PhysicalHeight;
     
     {Setup Tag (Virtual)}
     Tag.Vertual.Header.Tag:=BCM2837_MBOX_TAG_SET_VIRTUAL_W_H;
     Tag.Vertual.Header.Size:=SizeOf(TBCM2837MailboxTagSetVirtual) - SizeOf(TBCM2837MailboxTagHeader);
     Tag.Vertual.Header.Length:=SizeOf(Tag.Vertual.Request);
     Tag.Vertual.Request.Width:=Defaults.VirtualWidth;
     Tag.Vertual.Request.Height:=Defaults.VirtualHeight;

     {Setup Tag (Depth)}
     Tag.Depth.Header.Tag:=BCM2837_MBOX_TAG_SET_DEPTH;
     Tag.Depth.Header.Size:=SizeOf(TBCM2837MailboxTagSetDepth) - SizeOf(TBCM2837MailboxTagHeader);
     Tag.Depth.Header.Length:=SizeOf(Tag.Depth.Request);
     Tag.Depth.Request.Depth:=Defaults.Depth;
     
     {Setup Tag (Order)}
     Tag.Order.Header.Tag:=BCM2837_MBOX_TAG_SET_PIXEL_ORDER;
     Tag.Order.Header.Size:=SizeOf(TBCM2837MailboxTagSetPixelOrder) - SizeOf(TBCM2837MailboxTagHeader);
     Tag.Order.Header.Length:=SizeOf(Tag.Order.Request);
     Tag.Order.Request.Order:=Defaults.Order;
     
     {Setup Tag (Mode)}
     Tag.Mode.Header.Tag:=BCM2837_MBOX_TAG_SET_ALPHA_MODE;
     Tag.Mode.Header.Size:=SizeOf(TBCM2837MailboxTagSetAlphaMode) - SizeOf(TBCM2837MailboxTagHeader);
     Tag.Mode.Header.Length:=SizeOf(Tag.Mode.Request);
     Tag.Mode.Request.Mode:=Defaults.Mode;
     
     {Setup Tag (Offset)}
     Tag.Offset.Header.Tag:=BCM2837_MBOX_TAG_SET_VIRTUAL_OFFSET;
     Tag.Offset.Header.Size:=SizeOf(TBCM2837MailboxTagSetVirtualOffset) - SizeOf(TBCM2837MailboxTagHeader);
     Tag.Offset.Header.Length:=SizeOf(Tag.Offset.Request);
     Tag.Offset.Request.X:=Defaults.OffsetX;
     Tag.Offset.Request.Y:=Defaults.OffsetY;
     
     {Setup Tag (Overscan)}
     Tag.Overscan.Header.Tag:=BCM2837_MBOX_TAG_SET_OVERSCAN;
     Tag.Overscan.Header.Size:=SizeOf(TBCM2837MailboxTagSetOverscan) - SizeOf(TBCM2837MailboxTagHeader);
     Tag.Overscan.Header.Length:=SizeOf(Tag.Overscan.Request);
     Tag.Overscan.Request.Top:=Defaults.OverscanTop;
     Tag.Overscan.Request.Bottom:=Defaults.OverscanBottom;
     Tag.Overscan.Request.Left:=Defaults.OverscanLeft;
     Tag.Overscan.Request.Right:=Defaults.OverscanRight;
     
     {Setup Tag (Allocate)}
     Tag.Allocate.Header.Tag:=BCM2837_MBOX_TAG_ALLOCATE_BUFFER;
     Tag.Allocate.Header.Size:=SizeOf(TBCM2837MailboxTagAllocateBuffer) - SizeOf(TBCM2837MailboxTagHeader);
     Tag.Allocate.Header.Length:=SizeOf(Tag.Allocate.Request);
     Tag.Allocate.Request.Alignment:=BCM2710FRAMEBUFFER_ALIGNEMENT;
     
     {Setup Tag (Pitch)}
     Tag.Pitch.Header.Tag:=BCM2837_MBOX_TAG_GET_PITCH;
     Tag.Pitch.Header.Size:=SizeOf(TBCM2837MailboxTagGetPitch) - SizeOf(TBCM2837MailboxTagHeader);
     Tag.Pitch.Header.Length:=SizeOf(Tag.Pitch.Request);
     
     {Setup Footer}
     Footer:=PBCM2837MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2837MailboxTagCreateBuffer)));
     Footer.Tag:=BCM2837_MBOX_TAG_END;
     
     {Call Mailbox}
     Result:=MailboxPropertyCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
     if Result <> ERROR_SUCCESS then
      begin
       if PLATFORM_LOG_ENABLED then PlatformLogError('BCM2710: FramebufferAllocate - MailboxPropertyCall failed: ' + ErrorToString(Result));
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

function BCM2710FramebufferRelease(Framebuffer:PFramebufferDevice):LongWord;
var
 Size:LongWord;
 Response:LongWord;
 Header:PBCM2837MailboxHeader;
 Footer:PBCM2837MailboxFooter;
 Tag:PBCM2837MailboxTagReleaseBuffer;
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
    Size:=SizeOf(TBCM2837MailboxHeader) + SizeOf(TBCM2837MailboxTagReleaseBuffer) + SizeOf(TBCM2837MailboxFooter);

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
     Header.Code:=BCM2837_MBOX_REQUEST_CODE;
    
     {Setup Tag}
     Tag:=PBCM2837MailboxTagReleaseBuffer(PtrUInt(Header) + PtrUInt(SizeOf(TBCM2837MailboxHeader)));
     Tag.Header.Tag:=BCM2837_MBOX_TAG_RELEASE_BUFFER;
     Tag.Header.Size:=SizeOf(TBCM2837MailboxTagReleaseBuffer) - SizeOf(TBCM2837MailboxTagHeader);
     Tag.Header.Length:=SizeOf(Tag.Request);
    
     {Setup Footer}
     Footer:=PBCM2837MailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TBCM2837MailboxTagReleaseBuffer)));
     Footer.Tag:=BCM2837_MBOX_TAG_END;
     
     {Call Mailbox}
     Result:=MailboxPropertyCall(BCM2837_MAILBOX_0,BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response);
     if Result <> ERROR_SUCCESS then
      begin
       if DEVICE_LOG_ENABLED then DeviceLogError(nil,'BCM2710: FramebufferRelease: MailboxPropertyCall failed: ' + ErrorToString(Result));
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

function BCM2710FramebufferSetProperties(Framebuffer:PFramebufferDevice;Properties:PFramebufferProperties):LongWord;
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
{BCM2710 Helper Functions}

{==============================================================================}
{==============================================================================}

initialization
 BCM2710Init;
 
{==============================================================================}
 
finalization
 {Nothing}
 
{==============================================================================}
{==============================================================================}

end.
 