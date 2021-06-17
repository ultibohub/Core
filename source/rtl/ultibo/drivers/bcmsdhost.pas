{
Broadcom BCM27XX SDHOST driver

Copyright (C) 2021 - SoftOz Pty Ltd.

Arch
====

 <All>

Boards
======

 Raspberry Pi - Model A/B/A+/B+/CM1
 Raspberry Pi - Model Zero/ZeroW
 Raspberry Pi 2 - Model B
 Raspberry Pi 3 - Model B/B+/A+
 Raspberry Pi CM3/CM3+
 Raspberry Pi 4 - Model B
 Raspberry Pi 400
 Raspberry Pi CM4

Licence
=======

 LGPLv2.1 with static linking exception (See COPYING.modifiedLGPL.txt)

Credits
=======

 Information for this unit was obtained from:

  Linux - \drivers\mmc\host\bcm2835.c (SDHOST) - Copyright (C) 2015-2016 Raspberry Pi (Trading) Ltd.
  Linux - \drivers\mmc\host\bcm2835-sdhost.c (SDHOST) - Copyright (C) 2015-2016 Raspberry Pi (Trading) Ltd.

References
==========

BCM27XX SDHOST Device
=====================

 The SDHOST controller on the BCM27XX is a non SDHCI-compliant device which requires a specific
 driver.

 It can be routed to GPIO pins 22 to 27 (ALT0) or 48 to 53 (ALT0) in order to control the SD card
 slot when the SDHCI device is being used for the on board WiFi.

 Note that on the Raspberry Pi 4 the SD card is no longer connected to pins 48 to 53 so the SDHOST
 controller cannot control the primary SD card, there is an additional SDHCI controller (EMMC2) for
 that purpose.

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit BCMSDHOST;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,DMA,Storage,MMC,GPIO,SysUtils;

{==============================================================================}
{Global definitions}
{$INCLUDE ..\core\GlobalDefines.inc}

{==============================================================================}
const
 {BCMSDHOST specific constants}
 BCMSDHOST_DESCRIPTION = 'Broadcom BCM27XX SDHOST';  {Description of BCMSDHOST device}

const
 BCMSDHOST_FIFO_READ_THRESHOLD   = 4;
 BCMSDHOST_FIFO_WRITE_THRESHOLD  = 4;
 BCMSDHOST_ALLOW_CMD23_READ      = 1;
 BCMSDHOST_ALLOW_CMD23_WRITE     = 0;
 BCMSDHOST_SDDATA_FIFO_PIO_BURST = 8;
 BCMSDHOST_CMD_DALLY_US          = 1;

 {BCMSDHOST register constants}
 BCMSDHOST_SDCMD  = $00; {Command to SD card              - 16 R/W}
 BCMSDHOST_SDARG  = $04; {Argument to SD card             - 32 R/W}
 BCMSDHOST_SDTOUT = $08; {Start value for timeout counter - 32 R/W}
 BCMSDHOST_SDCDIV = $0c; {Start value for clock divider   - 11 R/W}
 BCMSDHOST_SDRSP0 = $10; {SD card response (31:0)         - 32 R  }
 BCMSDHOST_SDRSP1 = $14; {SD card response (63:32)        - 32 R  }
 BCMSDHOST_SDRSP2 = $18; {SD card response (95:64)        - 32 R  }
 BCMSDHOST_SDRSP3 = $1c; {SD card response (127:96)       - 32 R  }
 BCMSDHOST_SDHSTS = $20; {SD host status                  - 11 R  }
 BCMSDHOST_SDVDD  = $30; {SD card power control           -  1 R/W}
 BCMSDHOST_SDEDM  = $34; {Emergency Debug Mode            - 13 R/W}
 BCMSDHOST_SDHCFG = $38; {Host configuration              -  2 R/W}
 BCMSDHOST_SDHBCT = $3c; {Host byte count (debug)         - 32 R/W}
 BCMSDHOST_SDDATA = $40; {Data to/from SD card            - 32 R/W}
 BCMSDHOST_SDHBLC = $50; {Host block count (SDIO/SDHC)    -  9 R/W}

 {BCMSDHOST command register constants}
 BCMSDHOST_SDCMD_NEW_FLAG      = $8000;
 BCMSDHOST_SDCMD_FAIL_FLAG     = $4000;
 BCMSDHOST_SDCMD_BUSYWAIT      = $800;
 BCMSDHOST_SDCMD_NO_RESPONSE   = $400;
 BCMSDHOST_SDCMD_LONG_RESPONSE = $200;
 BCMSDHOST_SDCMD_WRITE_CMD     = $80;
 BCMSDHOST_SDCMD_READ_CMD      = $40;
 BCMSDHOST_SDCMD_CMD_MASK      = $3f;

 {BCMSDHOST clock divider register constants}
 BCMSDHOST_SDCDIV_MAX_CDIV     = $7ff;

 {BCMSDHOST host status register constants}
 BCMSDHOST_SDHSTS_BUSY_IRPT           = $400;
 BCMSDHOST_SDHSTS_BLOCK_IRPT          = $200;
 BCMSDHOST_SDHSTS_SDIO_IRPT           = $100;
 BCMSDHOST_SDHSTS_REW_TIME_OUT        = $80;
 BCMSDHOST_SDHSTS_CMD_TIME_OUT        = $40;
 BCMSDHOST_SDHSTS_CRC16_ERROR         = $20;
 BCMSDHOST_SDHSTS_CRC7_ERROR          = $10;
 BCMSDHOST_SDHSTS_FIFO_ERROR          = $08;
  {04 Reserved}
  {02 Reserved}
 BCMSDHOST_SDHSTS_DATA_FLAG           = $01;

 BCMSDHOST_SDHSTS_TRANSFER_ERROR_MASK = BCMSDHOST_SDHSTS_CRC7_ERROR or BCMSDHOST_SDHSTS_CRC16_ERROR or BCMSDHOST_SDHSTS_REW_TIME_OUT or BCMSDHOST_SDHSTS_FIFO_ERROR;
 BCMSDHOST_SDHSTS_ERROR_MASK          = BCMSDHOST_SDHSTS_CMD_TIME_OUT or BCMSDHOST_SDHSTS_TRANSFER_ERROR_MASK;

 {BCMSDHOST host configuration register constants}
 BCMSDHOST_SDHCFG_BUSY_IRPT_EN  = (1 shl 10);
 BCMSDHOST_SDHCFG_BLOCK_IRPT_EN = (1 shl 8);
 BCMSDHOST_SDHCFG_SDIO_IRPT_EN  = (1 shl 5);
 BCMSDHOST_SDHCFG_DATA_IRPT_EN  = (1 shl 4);
 BCMSDHOST_SDHCFG_SLOW_CARD     = (1 shl 3);
 BCMSDHOST_SDHCFG_WIDE_EXT_BUS  = (1 shl 2);
 BCMSDHOST_SDHCFG_WIDE_INT_BUS  = (1 shl 1);
 BCMSDHOST_SDHCFG_REL_CMD_LINE  = (1 shl 0);

 {BCMSDHOST emergency debug mode register constants}
 BCMSDHOST_SDEDM_FORCE_DATA_MODE = (1 shl 19);
 BCMSDHOST_SDEDM_CLOCK_PULSE     = (1 shl 20);
 BCMSDHOST_SDEDM_BYPASS          = (1 shl 21);

 BCMSDHOST_SDEDM_WRITE_THRESHOLD_SHIFT = 9;
 BCMSDHOST_SDEDM_READ_THRESHOLD_SHIFT  = 14;
 BCMSDHOST_SDEDM_THRESHOLD_MASK        = $1f;

 BCMSDHOST_SDEDM_FSM_MASK         = $f;
 BCMSDHOST_SDEDM_FSM_IDENTMODE    = $0;
 BCMSDHOST_SDEDM_FSM_DATAMODE     = $1;
 BCMSDHOST_SDEDM_FSM_READDATA     = $2;
 BCMSDHOST_SDEDM_FSM_WRITEDATA    = $3;
 BCMSDHOST_SDEDM_FSM_READWAIT     = $4;
 BCMSDHOST_SDEDM_FSM_READCRC      = $5;
 BCMSDHOST_SDEDM_FSM_WRITECRC     = $6;
 BCMSDHOST_SDEDM_FSM_WRITEWAIT1   = $7;
 BCMSDHOST_SDEDM_FSM_POWERDOWN    = $8;
 BCMSDHOST_SDEDM_FSM_POWERUP      = $9;
 BCMSDHOST_SDEDM_FSM_WRITESTART1  = $a;
 BCMSDHOST_SDEDM_FSM_WRITESTART2  = $b;
 BCMSDHOST_SDEDM_FSM_GENPULSES    = $c;
 BCMSDHOST_SDEDM_FSM_WRITEWAIT2   = $d;
 BCMSDHOST_SDEDM_FSM_STARTPOWDOWN = $f;

 BCMSDHOST_SDDATA_FIFO_WORDS = 16;

 BCMSDHOST_USE_CMD23_FLAGS = (BCMSDHOST_ALLOW_CMD23_READ * MMC_DATA_READ) or (BCMSDHOST_ALLOW_CMD23_WRITE * MMC_DATA_WRITE);

 {Mxilbox constants}
 BCMSDHOST_MBOX_TAG_SET_SDHOST_CLOCK  = $00038042; {Tell the firmware the SD Host clock setting so it will be adjusted for changes in core frequency}

{==============================================================================}
type
 {BCMSDHOST specific types}
 {Mailbox tag request for BCMSDHOST_MBOX_TAG_SET_SDHOST_CLOCK}
 TBCMSDHOSTMailboxTagSetSDHostClock = record
  Clock:LongWord;
  Value1:LongWord;
  Value2:LongWord;
 end;

 PBCMSDHOSTHost = ^TBCMSDHOSTHost;
 TBCMSDHOSTHost = record
  {SDHCI Properties}
  SDHCI:TSDHCIHost;
  {BCMSDHOST Properties}
  IRQ:LongWord;                               {The IRQ number for this device}
  DREQ:LongWord;                              {The DMA DREQ ID for this device}
  Lock:TSpinHandle;                           {Host lock (Differs from lock in Host portion) (Spin lock due to use by interrupt handler)}
  EnableFIQ:LongBool;                         {Use FIQ instead of IRQ}
  GPIOFirst:LongWord;                         {The starting pin number for GPIO assignments (or GPIO_PIN_UNKNOWN if externally configured)}
  GPIOLast:LongWord;                          {The ending pin number for GPIO assignments (or GPIO_PIN_UNKNOWN if externally configured)}
  GPIOFunction:LongWord;                      {The function number for GPIO assignments (or GPIO_FUNCTION_UNKNOWN if externally configured)}
  ClockDivider:LongWord;                      {Cached Clock Divider (CDIV) register}
  HostConfiguration:LongWord;                 {Cached Host Configuration (HCFG) register}
  FirmwareSetsClockDivider:LongBool;          {True if the firmware controls the clock divider}
  CommandQuickPollRetries:LongWord;
  NanosecondsPerFifoWord:LongWord;
  AllowDMA:LongBool;
  ResetClock:LongBool;                        {Reset the clock for the next request}
  MaxDelay:LongWord;                          {Maximum length of time spent waiting (in Milliseconds)}
  StopTime:Int64;                             {When the last stop was issued (in Clock Ticks)}
  DrainWords:LongWord;                        {Last words of FIFO to drain on DMA read}
  DrainOffset:LongWord;                       {Offset for data during FIFO drain on DMA read}
  UseDMA:LongBool;                            {Use DMA for the current data transfer}
  UseBusy:LongBool;                           {Wait for busy interrupt on current command}
  DelayAfterStop:LongWord;                    {Minimum time between stop and subsequent data transfer (in Microseconds)}
  DelayAfterThisStop:LongWord;                {Minimum time between this stop and subsequent data transfer (in Microseconds)}
  UserOverclock50:LongWord;                   {User's preferred frequency to use when 50MHz is requested (in MHz)}
  Overclock50:LongWord;                       {Frequency to use when 50MHz is requested (in MHz)}
  Overclock:LongWord;                         {Current frequency if overclocked, else zero (in Hz)}
  PIOLimit:LongWord;                          {Maximum block count for PIO (0 = always DMA / 0x7FFFFFF = always PIO)}
  PIOTimeout:LongWord;                        {PIO Read or Write block timeout (in milliseconds)}
  DMAData:TDMAData;                           {DMA data descriptor for current request (If applicable)}
  DMADirection:LongWord;                      {DMA data direction for current request (If applicable)}
  DMABuffer:Pointer;                          {DMA bounce buffer for the current request (If applicable)}
 end;

{==============================================================================}
{var}
 {BCMSDHOST specific variables}

{==============================================================================}
{Initialization Functions}
procedure BCMSDHOSTInit;

{==============================================================================}
{BCMSDHOST Functions}
function BCMSDHOSTCreate(Address:PtrUInt;const Name:String;IRQ,DREQ,ClockMinimum,ClockMaximum,GPIOFirst,GPIOLast,GPIOFunction:LongWord;EnableFIQ:Boolean):PSDHCIHost;
function BCMSDHOSTDestroy(SDHCI:PSDHCIHost):LongWord;

{==============================================================================}
{BCMSDHOST MMC Functions}
function BCMSDHOSTSendCommand(MMC:PMMCDevice;Command:PMMCCommand):LongWord;
function BCMSDHOSTSetIOS(MMC:PMMCDevice):LongWord;
function BCMSDHOSTGetCardDetect(MMC:PMMCDevice):LongWord;
function BCMSDHOSTGetWriteProtect(MMC:PMMCDevice):LongWord;

{==============================================================================}
{BCMSDHOST SDHCI Functions}
function BCMSDHOSTHostStart(SDHCI:PSDHCIHost):LongWord;
function BCMSDHOSTHostStop(SDHCI:PSDHCIHost):LongWord;

function BCMSDHOSTReset(SDHCI:PSDHCIHost;Mask:Byte):LongWord;
function BCMSDHOSTHardwareReset(SDHCI:PSDHCIHost):LongWord;

function BCMSDHOSTSetPower(SDHCI:PSDHCIHost;Power:Word):LongWord;
function BCMSDHOSTSetClock(SDHCI:PSDHCIHost;Clock:LongWord):LongWord;

procedure BCMSDHOSTCommandWaitWorker(SDHCI:PSDHCIHost);
procedure BCMSDHOSTDMARequestCompleted(Request:PDMARequest);
function BCMSDHOSTSharedInterruptHandler(Number,CPUID,Flags:LongWord;SDHCI:PSDHCIHost):LongWord;

{==============================================================================}
{BCMSDHOST Helper Functions}
function BCMSDHOSTSetSDHostClock(var Clock,Value1,Value2:LongWord):LongWord;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {BCMSDHOST specific variables}
 BCMSDHOSTInitialized:Boolean;
 
{==============================================================================}
{==============================================================================}
{Forward Declarations}
function BCMSDHOSTSetPowerInternal(SDHCI:PSDHCIHost;PowerOn:Boolean):LongWord; forward;
function BCMSDHOSTResetInternal(SDHCI:PSDHCIHost):LongWord; forward;
function BCMSDHOSTInitInternal(SDHCI:PSDHCIHost;Soft:Boolean):LongWord; forward;

function BCMSDHOSTPrepareDMA(SDHCI:PSDHCIHost;Command:PMMCCommand):LongWord; forward;

function BCMSDHOSTTransferPIO(SDHCI:PSDHCIHost):LongWord; forward;
function BCMSDHOSTReadBlockPIO(SDHCI:PSDHCIHost):LongWord; forward;
function BCMSDHOSTWriteBlockPIO(SDHCI:PSDHCIHost):LongWord; forward;

function BCMSDHOSTTransferComplete(SDHCI:PSDHCIHost):LongWord; forward;
function BCMSDHOSTWaitTransferComplete(SDHCI:PSDHCIHost):LongWord; forward;

function BCMSDHOSTFinishCommand(SDHCI:PSDHCIHost;AllowWait:Boolean):LongWord; forward;
function BCMSDHOSTFinishData(SDHCI:PSDHCIHost):LongWord; forward;

function BCMSDHOSTBusyInterrupt(SDHCI:PSDHCIHost;InterruptMask:LongWord):LongWord; forward;
function BCMSDHOSTDataInterrupt(SDHCI:PSDHCIHost;InterruptMask:LongWord):LongWord; forward;
function BCMSDHOSTBlockInterrupt(SDHCI:PSDHCIHost;InterruptMask:LongWord):LongWord; forward;

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure BCMSDHOSTInit;
{Initialize the BCMSDHOST unit and version table}

{Note: Called only during system startup}
var
 WorkInt:LongWord;
 WorkBool:LongBool;
begin
 {}
 {Check Initialized}
 if BCMSDHOSTInitialized then Exit;

 {Check Environment Variables}
 {BCMSDHOST_DELAY_AFTER_STOP}
 WorkInt:=StrToIntDef(SysUtils.GetEnvironmentVariable('BCMSDHOST_DELAY_AFTER_STOP'),BCMSDHOST_DELAY_AFTER_STOP);
 if WorkInt <> BCMSDHOST_DELAY_AFTER_STOP then BCMSDHOST_DELAY_AFTER_STOP:=WorkInt;

 {BCMSDHOST_OVERCLOCK_50}
 WorkInt:=StrToIntDef(SysUtils.GetEnvironmentVariable('BCMSDHOST_OVERCLOCK_50'),BCMSDHOST_OVERCLOCK_50);
 if WorkInt <> BCMSDHOST_OVERCLOCK_50 then BCMSDHOST_OVERCLOCK_50:=WorkInt;

 {BCMSDHOST_PIO_LIMIT}
 WorkInt:=StrToIntDef(SysUtils.GetEnvironmentVariable('BCMSDHOST_PIO_LIMIT'),BCMSDHOST_PIO_LIMIT);
 if WorkInt <> BCMSDHOST_PIO_LIMIT then BCMSDHOST_PIO_LIMIT:=WorkInt;

 {BCMSDHOST_FORCE_PIO}
 WorkBool:=StrToBoolDef(SysUtils.GetEnvironmentVariable('BCMSDHOST_FORCE_PIO'),BCMSDHOST_FORCE_PIO);
 if WorkBool <> BCMSDHOST_FORCE_PIO then BCMSDHOST_FORCE_PIO:=WorkBool;

 BCMSDHOSTInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{BCMSDHOST Functions}
function BCMSDHOSTCreate(Address:PtrUInt;const Name:String;IRQ,DREQ,ClockMinimum,ClockMaximum,GPIOFirst,GPIOLast,GPIOFunction:LongWord;EnableFIQ:Boolean):PSDHCIHost;
{Create and register a new BCMSDHOST SDHCI device which can be accessed using the SDHCI API}
{Address: The address of the BCMSDHOST registers}
{Name: The text description of this device which will show in the device list (Optional)}
{IRQ: The interrupt number for the BCMSDHOST}
{DREQ: The DMA data request ID for the BCMSDHOST}
{ClockMinimum: The minimum frequency for the BCMSDHOST clock}
{ClockMaximum: The maximum frequency for the BCMSDHOST clock}
{GPIOFirst: The starting pin number for GPIO assignments (or GPIO_PIN_UNKNOWN if externally configured)}
{GPIOLast: The ending pin number for GPIO assignments (or GPIO_PIN_UNKNOWN if externally configured)}
{GPIOFunction: The function number for GPIO assignments (or GPIO_FUNCTION_UNKNOWN if externally configured)}
{EnableFIQ: Enable fast interrupt support for the BCMSDHOST}
{Return: Pointer to the new SDHCI device or nil if the SDHCI device could not be created}
var
 Status:LongWord;
 BCMSDHOSTHost:PBCMSDHOSTHost;
begin
 {}
 Result:=nil;

 {$IF DEFINED(BCMSDHOST_DEBUG) or DEFINED(MMC_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'BCMSDHOST: SDHCI Create (Address=' + AddrToHex(Address) + ' Name=' + Name + ' IRQ=' + IntToStr(IRQ) + ' DREQ=' + IntToStr(DREQ) + ')');
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'BCMSDHOST:              (Clock Minimum=' + IntToStr(ClockMinimum) + ' Maximum=' + IntToStr(ClockMaximum) + ')');
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'BCMSDHOST:              (GPIO First=' + GPIOPinToString(GPIOFirst) + ' Last=' + GPIOPinToString(GPIOLast) + ' Function=' + GPIOFunctionToString(GPIOFunction) + ')');
 {$ENDIF}

 {Check Address}
 if Address = 0 then Exit;

 {Check IRQ}
 {if IRQ = 0 then Exit;} {IRQ 0 is valid}

 {Check Clock Minimum}
 if ClockMinimum = 0 then ClockMinimum:=400000;

 {Check Clock Maximum}
 if ClockMaximum = 0 then ClockMaximum:=ClockMinimum;

 {Create SDHCI}
 BCMSDHOSTHost:=PBCMSDHOSTHost(SDHCIHostCreateEx(SizeOf(TBCMSDHOSTHost)));
 if BCMSDHOSTHost <> nil then
  begin
   {Update SDHCI}
   {Device}
   BCMSDHOSTHost.SDHCI.Device.DeviceBus:=DEVICE_BUS_MMIO;
   BCMSDHOSTHost.SDHCI.Device.DeviceType:=SDHCI_TYPE_SD;
   BCMSDHOSTHost.SDHCI.Device.DeviceFlags:=SDHCI_FLAG_NON_STANDARD or SDHCI_FLAG_EXTERNAL_DMA;
   if BCMSDHOST_USE_CMD23_FLAGS = 0 then BCMSDHOSTHost.SDHCI.Device.DeviceFlags:=BCMSDHOSTHost.SDHCI.Device.DeviceFlags or SDHCI_FLAG_AUTO_CMD23;
   if BCMSDHOST_USE_CMD23_FLAGS <> 0 then BCMSDHOSTHost.SDHCI.Device.DeviceFlags:=BCMSDHOSTHost.SDHCI.Device.DeviceFlags or SDHCI_FLAG_AUTO_CMD12;
   BCMSDHOSTHost.SDHCI.Device.DeviceData:=nil;
   if Length(Name) <> 0 then BCMSDHOSTHost.SDHCI.Device.DeviceDescription:=Name else BCMSDHOSTHost.SDHCI.Device.DeviceDescription:=BCMSDHOST_DESCRIPTION;
   {SDHCI}
   BCMSDHOSTHost.SDHCI.SDHCIState:=SDHCI_STATE_DISABLED;
   BCMSDHOSTHost.SDHCI.HostStart:=BCMSDHOSTHostStart;
   BCMSDHOSTHost.SDHCI.HostStop:=BCMSDHOSTHostStop;
   BCMSDHOSTHost.SDHCI.HostReadByte:=nil;
   BCMSDHOSTHost.SDHCI.HostReadWord:=nil;
   BCMSDHOSTHost.SDHCI.HostReadLong:=nil;
   BCMSDHOSTHost.SDHCI.HostWriteByte:=nil;
   BCMSDHOSTHost.SDHCI.HostWriteWord:=nil;
   BCMSDHOSTHost.SDHCI.HostWriteLong:=nil;
   BCMSDHOSTHost.SDHCI.HostReset:=BCMSDHOSTReset;
   BCMSDHOSTHost.SDHCI.HostHardwareReset:=BCMSDHOSTHardwareReset;
   BCMSDHOSTHost.SDHCI.HostSetPower:=BCMSDHOSTSetPower;
   BCMSDHOSTHost.SDHCI.HostSetClock:=BCMSDHOSTSetClock;
   BCMSDHOSTHost.SDHCI.HostSetClockDivider:=nil;
   BCMSDHOSTHost.SDHCI.HostSetControlRegister:=nil;
   BCMSDHOSTHost.SDHCI.DeviceInitialize:=nil;
   BCMSDHOSTHost.SDHCI.DeviceDeinitialize:=nil;
   BCMSDHOSTHost.SDHCI.DeviceGetCardDetect:=BCMSDHOSTGetCardDetect;
   BCMSDHOSTHost.SDHCI.DeviceGetWriteProtect:=BCMSDHOSTGetWriteProtect;
   BCMSDHOSTHost.SDHCI.DeviceSendCommand:=BCMSDHOSTSendCommand;
   BCMSDHOSTHost.SDHCI.DeviceSetIOS:=BCMSDHOSTSetIOS;
   {Driver}
   BCMSDHOSTHost.SDHCI.Address:=Pointer(Address);
   {Configuration}
   BCMSDHOSTHost.SDHCI.ClockMinimum:=ClockMinimum;
   BCMSDHOSTHost.SDHCI.ClockMaximum:=ClockMaximum;
   {BCMSDHOST}
   BCMSDHOSTHost.IRQ:=IRQ;
   BCMSDHOSTHost.DREQ:=DREQ;
   BCMSDHOSTHost.Lock:=INVALID_HANDLE_VALUE;
   BCMSDHOSTHost.EnableFIQ:=EnableFIQ;
   BCMSDHOSTHost.GPIOFirst:=GPIOFirst;
   BCMSDHOSTHost.GPIOLast:=GPIOLast;
   BCMSDHOSTHost.GPIOFunction:=GPIOFunction;
   BCMSDHOSTHost.AllowDMA:=True;
   if BCMSDHOST_FORCE_PIO then BCMSDHOSTHost.AllowDMA:=False;
   BCMSDHOSTHost.PIOTimeout:=500; {Milliseconds}
   BCMSDHOSTHost.PIOLimit:=BCMSDHOST_PIO_LIMIT;
   if BCMSDHOSTHost.PIOLimit = $7fffffff then BCMSDHOSTHost.AllowDMA:=False;
   BCMSDHOSTHost.MaxDelay:=1;
   BCMSDHOSTHost.DelayAfterStop:=BCMSDHOST_DELAY_AFTER_STOP;
   BCMSDHOSTHost.UserOverclock50:=BCMSDHOST_OVERCLOCK_50;

   {$IF DEFINED(BCMSDHOST_DEBUG) or DEFINED(MMC_DEBUG)}
   if MMC_LOG_ENABLED then
    begin
     MMCLogDebug(nil,'BCMSDHOST: AllowDMA = ' + BooleanToString(BCMSDHOSTHost.AllowDMA));
     MMCLogDebug(nil,'BCMSDHOST: PIOTimeout = ' + IntToStr(BCMSDHOSTHost.PIOTimeout));
     MMCLogDebug(nil,'BCMSDHOST: PIOLimit = ' + IntToStr(BCMSDHOSTHost.PIOLimit));
     MMCLogDebug(nil,'BCMSDHOST: MaxDelay = ' + IntToStr(BCMSDHOSTHost.MaxDelay));
     MMCLogDebug(nil,'BCMSDHOST: DelayAfterStop = ' + IntToStr(BCMSDHOSTHost.DelayAfterStop));
     MMCLogDebug(nil,'BCMSDHOST: UserOverclock50 = ' + IntToStr(BCMSDHOSTHost.UserOverclock50));
    end; 
   {$ENDIF}

   {Register SDHCI}
   Status:=SDHCIHostRegister(@BCMSDHOSTHost.SDHCI);
   if Status <> ERROR_SUCCESS then
    begin
     if MMC_LOG_ENABLED then MMCLogError(nil,'BCMSDHOST: Failed to register SD host controller: ' + ErrorToString(Status));
    end;
  end
 else
  begin
   if MMC_LOG_ENABLED then MMCLogError(nil,'BCMSDHOST: Failed to create SD host controller');
  end;
end;

{==============================================================================}

function BCMSDHOSTDestroy(SDHCI:PSDHCIHost):LongWord;
{Stop, deregister and destroy a BCMSDHOST SDHCI device created by this driver}
{SDHCI: The SDHCI device to destroy}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check SDHCI}
 if SDHCI = nil then Exit;

 {$IF DEFINED(BCMSDHOST_DEBUG) or DEFINED(MMC_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'BCMSDHOST: SDHCI Destroy');
 {$ENDIF}

 {Stop SDHCI}
 Result:=SDHCIHostStop(SDHCI);
 if Result = ERROR_SUCCESS then
  begin
   {Deregister SDHCI}
   Result:=SDHCIHostDeregister(SDHCI);
   if Result = ERROR_SUCCESS then
    begin
     {Destroy SDHCI}
     Result:=SDHCIHostDestroy(SDHCI);
     if Result <> ERROR_SUCCESS then
      begin
       if MMC_LOG_ENABLED then MMCLogError(nil,'BCMSDHOST: Failed to destroy SD host controller: ' + ErrorToString(Result));
      end;
    end
   else
    begin
     if MMC_LOG_ENABLED then MMCLogError(nil,'BCMSDHOST: Failed to deregister SD host controller: ' + ErrorToString(Result));
    end;
  end
 else
  begin
   if MMC_LOG_ENABLED then MMCLogError(nil,'BCMSDHOST: Failed to stop SD host controller: ' + ErrorToString(Result));
  end;
end;

{==============================================================================}
{==============================================================================}
{BCMSDHOST Internal Functions}
function BCMSDHOSTRead(SDHCI:PSDHCIHost;Reg:LongWord):LongWord;
{Read from a BCMSDHOST register}
{Note: Not intended to be called directly by applications}
begin
 {}
 Result:=PLongWord(PtrUInt(SDHCI.Address) + PtrUInt(Reg))^;
end;

{==============================================================================}

procedure BCMSDHOSTWrite(SDHCI:PSDHCIHost;Reg,Value:LongWord);
{Write to a BCMSDHOST register}
{Note: Not intended to be called directly by applications}
begin
 {}
 PLongWord(PtrUInt(SDHCI.Address) + PtrUInt(Reg))^:=Value;
end;

{==============================================================================}

function BCMSDHOSTLock(SDHCI:PSDHCIHost):LongWord;
{Acquire the BCMSDHOST host lock}
{Note: Not intended to be called directly by applications}
begin
 {}
 if PBCMSDHOSTHost(SDHCI).EnableFIQ then
  begin
   Result:=SpinLockIRQFIQ(PBCMSDHOSTHost(SDHCI).Lock);
  end
 else
  begin
   Result:=SpinLockIRQ(PBCMSDHOSTHost(SDHCI).Lock);
  end;
end;

{==============================================================================}

procedure BCMSDHOSTUnlock(SDHCI:PSDHCIHost);
{Release the BCMSDHOST host lock}
{Note: Not intended to be called directly by applications}
begin
 {}
 if PBCMSDHOSTHost(SDHCI).EnableFIQ then
  begin
   SpinUnlockIRQFIQ(PBCMSDHOSTHost(SDHCI).Lock);
  end
 else
  begin
   SpinUnlockIRQ(PBCMSDHOSTHost(SDHCI).Lock);
  end;
end;

{==============================================================================}

function BCMSDHOSTSetPowerInternal(SDHCI:PSDHCIHost;PowerOn:Boolean):LongWord;
{Internal set power function for the BCMSDHOST SDHCI}
{Note: Not intended to be called directly by applications}
var
 Value:LongWord;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check SDHCI}
 if SDHCI = nil then Exit;

 {$IF DEFINED(BCMSDHOST_DEBUG) or DEFINED(MMC_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'BCMSDHOST: Set Power Internal (PowerOn=' + BooleanToString(PowerOn) + ')');
 {$ENDIF}

 {Check Power On}
 if PowerOn then Value:=1 else Value:=0;

 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}

 {Set Power}
 BCMSDHOSTWrite(SDHCI,BCMSDHOST_SDVDD,Value);

 Result:=MMC_STATUS_SUCCESS;

 //bcm2835_sdhost_set_power
end;

{==============================================================================}

function BCMSDHOSTResetInternal(SDHCI:PSDHCIHost):LongWord;
{Internal reset function for the BCMSDHOST SDHCI}
{Note: Not intended to be called directly by applications}
var
 Value:LongWord;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check SDHCI}
 if SDHCI = nil then Exit;

 {$IF DEFINED(BCMSDHOST_DEBUG) or DEFINED(MMC_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'BCMSDHOST: Reset Internal');
 {$ENDIF}

 {Power Off}
 BCMSDHOSTSetPowerInternal(SDHCI,False);

 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}

 {Reset Registers}
 BCMSDHOSTWrite(SDHCI,BCMSDHOST_SDCMD,0);
 BCMSDHOSTWrite(SDHCI,BCMSDHOST_SDARG,0);
 BCMSDHOSTWrite(SDHCI,BCMSDHOST_SDTOUT,$f00000);
 BCMSDHOSTWrite(SDHCI,BCMSDHOST_SDCDIV,0);
 BCMSDHOSTWrite(SDHCI,BCMSDHOST_SDHSTS,$7f8); {Write 1s to clear}
 BCMSDHOSTWrite(SDHCI,BCMSDHOST_SDHCFG,0);
 BCMSDHOSTWrite(SDHCI,BCMSDHOST_SDHBCT,0);
 BCMSDHOSTWrite(SDHCI,BCMSDHOST_SDHBLC,0);

 {Limit FIFO usage due to silicon bug}
 Value:=BCMSDHOSTRead(SDHCI,BCMSDHOST_SDEDM);
 Value:=Value and not((BCMSDHOST_SDEDM_THRESHOLD_MASK shl BCMSDHOST_SDEDM_READ_THRESHOLD_SHIFT)
                  or (BCMSDHOST_SDEDM_THRESHOLD_MASK shl BCMSDHOST_SDEDM_WRITE_THRESHOLD_SHIFT));
 Value:=Value or (BCMSDHOST_FIFO_READ_THRESHOLD shl BCMSDHOST_SDEDM_READ_THRESHOLD_SHIFT)
              or (BCMSDHOST_FIFO_WRITE_THRESHOLD shl BCMSDHOST_SDEDM_WRITE_THRESHOLD_SHIFT);
 BCMSDHOSTWrite(SDHCI,BCMSDHOST_SDEDM,Value);
 MillisecondDelay(10);

 {Power On}
 BCMSDHOSTSetPowerInternal(SDHCI,True);
 MillisecondDelay(10);

 {Reset Clock / Clear Configuration / Reset Divider}
 SDHCI.Clock:=0;
 BCMSDHOSTWrite(SDHCI,BCMSDHOST_SDHCFG,PBCMSDHOSTHost(SDHCI).HostConfiguration);
 BCMSDHOSTWrite(SDHCI,BCMSDHOST_SDCDIV,BCMSDHOST_SDCDIV_MAX_CDIV);

 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 

 Result:=MMC_STATUS_SUCCESS;

 //bcm2835_sdhost_reset_internal
end;

{==============================================================================}

function BCMSDHOSTInitInternal(SDHCI:PSDHCIHost;Soft:Boolean):LongWord;
{Internal initialization function for the BCMSDHOST SDHCI}
{Note: Not intended to be called directly by applications}
var
 MMC:PMMCDevice;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check SDHCI}
 if SDHCI = nil then Exit;

 {$IF DEFINED(BCMSDHOST_DEBUG) or DEFINED(MMC_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'BCMSDHOST: Init Internal (Soft=' + BooleanToString(Soft) + ')');
 {$ENDIF}

 {Set interrupt enables}
 PBCMSDHOSTHost(SDHCI).HostConfiguration:=BCMSDHOST_SDHCFG_BUSY_IRPT_EN;

 {Reset Host}
 Result:=BCMSDHOSTResetInternal(SDHCI);
 if Result <> MMC_STATUS_SUCCESS then Exit;

 if Soft then
  begin
   {Force clock reconfiguration}
   SDHCI.Clock:=0;

   {Get MMC}
   MMC:=MMCDeviceFindByDevice(@SDHCI.Device);
   if MMC <> nil then
    begin
     Result:=BCMSDHOSTSetIOS(MMC);
    end
   else
    begin
     Result:=MMC_STATUS_INVALID_PARAMETER;
    end;
  end;

 //bcm2835_sdhost_init
end;

{==============================================================================}

function BCMSDHOSTPrepareDMA(SDHCI:PSDHCIHost;Command:PMMCCommand):LongWord; 
{Prepare DMA function for the BCMSDHOST SDHCI}
{Note: Not intended to be called directly by applications}
var
 DrainLen:LongWord;
 DMAData:PDMAData;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check SDHCI}
 if SDHCI = nil then Exit;

 {$IF DEFINED(BCMSDHOST_DEBUG) or DEFINED(MMC_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'BCMSDHOST: Prepare DMA');
 {$ENDIF}

 {Check Command}
 if Command = nil then Exit;

 {Check Data}
 if Command.Data = nil then Exit;
 
 {Check DMA Buffer}
 if PBCMSDHOSTHost(SDHCI).DMABuffer <> nil then
  begin
   DMABufferRelease(PBCMSDHOSTHost(SDHCI).DMABuffer);
   
   PBCMSDHOSTHost(SDHCI).DMABuffer:=nil;
  end;  
 
 {Check Data Buffer}
 if DMABufferValidate(DMAHostGetDefault,Command.Data.Data,Command.Data.BlockCount * Command.Data.BlockSize) = ERROR_NOT_COMPATIBLE then
  begin
   PBCMSDHOSTHost(SDHCI).DMABuffer:=DMABufferAllocate(DMAHostGetDefault,Command.Data.BlockCount * Command.Data.BlockSize);
  end;
 
 {Clear DMA Data}
 FillChar(PBCMSDHOSTHost(SDHCI).DMAData,SizeOf(TDMAData),0);
 DMAData:=@PBCMSDHOSTHost(SDHCI).DMAData;

 {Check Direction}
 if (Command.Data.Flags and MMC_DATA_READ) <> 0 then
  begin
   {Device to Memory}
   PBCMSDHOSTHost(SDHCI).DMADirection:=DMA_DIR_DEV_TO_MEM;

   {Setup DMA Data}
   DMAData.Source:=Pointer(PtrUInt(SDHCI.Address) + PtrUInt(BCMSDHOST_SDDATA));
   DMAData.Dest:=Command.Data.Data;
   DMAData.Flags:=DMA_DATA_FLAG_SOURCE_NOINCREMENT or DMA_DATA_FLAG_SOURCE_DREQ or DMA_DATA_FLAG_DEST_WIDE or DMA_DATA_FLAG_NOCLEAN;
   DMAData.StrideLength:=0;
   DMAData.SourceStride:=0;
   DMAData.DestStride:=0;
   DMAData.Size:=Command.Data.BlockCount * Command.Data.BlockSize;
   
   {Check DMA Buffer}
   if PBCMSDHOSTHost(SDHCI).DMABuffer <> nil then
    begin
     if MMC_LOG_ENABLED then MMCLogWarn(nil,'BCMSDHOST: Using DMA bounce buffer for read data destination');

     {Use DMA Buffer}
     DMAData.Dest:=PBCMSDHOSTHost(SDHCI).DMABuffer;
    end; 
  end
 else
  begin
   {Memory to Device}
   PBCMSDHOSTHost(SDHCI).DMADirection:=DMA_DIR_MEM_TO_DEV;
   
   {Setup DMA Data}
   DMAData.Source:=Command.Data.Data;
   DMAData.Dest:=Pointer(PtrUInt(SDHCI.Address) + PtrUInt(BCMSDHOST_SDDATA));
   DMAData.Flags:=DMA_DATA_FLAG_DEST_NOINCREMENT or DMA_DATA_FLAG_DEST_DREQ or DMA_DATA_FLAG_SOURCE_WIDE or DMA_DATA_FLAG_NOINVALIDATE;
   DMAData.StrideLength:=0;
   DMAData.SourceStride:=0;
   DMAData.DestStride:=0;
   DMAData.Size:=Command.Data.BlockCount * Command.Data.BlockSize;
   
   {Check DMA Buffer}
   if PBCMSDHOSTHost(SDHCI).DMABuffer <> nil then
    begin
     if MMC_LOG_ENABLED then MMCLogWarn(nil,'BCMSDHOST: Copying data to DMA bounce buffer from write data source');

     {Copy Data to DMA Buffer}
     DMAData.Source:=PBCMSDHOSTHost(SDHCI).DMABuffer;
     System.Move(Command.Data.Data^,DMAData.Source^,DMAData.Size);
    end; 
  end;

 {The block doesn't manage the FIFO DREQs properly for multi-block transfers, so don't attempt to DMA the final few words.
  Unfortunately this requires the final sg entry to be trimmed. This code demands that the overspill is contained in a single sg entry}
 PBCMSDHOSTHost(SDHCI).DrainWords:=0;
 if (Command.Data.BlockCount > 1) and ((Command.Data.Flags and MMC_DATA_READ) <> 0) then
  begin
   DrainLen:=Min((BCMSDHOST_FIFO_READ_THRESHOLD - 1) * 4,Command.Data.BlockCount * Command.Data.BlockSize);
   
   {Trim DMA Size}
   Dec(DMAData.Size,DrainLen);
   
   {Get Drain Words and Offset}
   PBCMSDHOSTHost(SDHCI).DrainWords:=DrainLen div 4;
   PBCMSDHOSTHost(SDHCI).DrainOffset:=DMAData.Size;
   
   {$IF DEFINED(BCMSDHOST_DEBUG) or DEFINED(MMC_DEBUG)}
   if MMC_LOG_ENABLED then MMCLogDebug(nil,'BCMSDHOST: DMA FIFO Drain (Words = ' + IntToStr(PBCMSDHOSTHost(SDHCI).DrainWords) + ' Offset = ' + AddrToHex(PBCMSDHOSTHost(SDHCI).DrainOffset) + ')');
   {$ENDIF}
  end;
 
 Result:=MMC_STATUS_SUCCESS; 

 //bcm2835_sdhost_prepare_dma
end;

{==============================================================================}

function BCMSDHOSTTransferPIO(SDHCI:PSDHCIHost):LongWord;
{PIO transfer handler for the BCMSDHOST SDHCI}
{Note: Not intended to be called directly by applications}

{Note: Caller must hold the host lock}
var
 Status:LongWord;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check SDHCI}
 if SDHCI = nil then Exit;

 {$IF (DEFINED(BCMSDHOST_DEBUG) or DEFINED(MMC_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'BCMSDHOST: Transfer PIO');
 {$ENDIF}

 {Check Command}
 if SDHCI.Command = nil then
  begin
   {$IFDEF INTERRUPT_DEBUG}
   if MMC_LOG_ENABLED then MMCLogError(nil,'BCMSDHOST: Transfer PIO when no current command');
   {$ENDIF}
   Exit; 
  end; 

 {Check Data}
 if SDHCI.Command.Data = nil then
  begin
   {$IFDEF INTERRUPT_DEBUG}
   if MMC_LOG_ENABLED then MMCLogError(nil,'BCMSDHOST: Transfer PIO  when no current data');
   {$ENDIF}
   Exit; 
  end; 

 {Check Data Buffer}
 if SDHCI.Command.Data.Data = nil then Exit;
 
 {Check Blocks Remaining}
 if SDHCI.Command.Data.BlocksRemaining = 0 then Exit;

 {Check Direction}
 if (SDHCI.Command.Data.Flags and MMC_DATA_READ) <> 0 then
  begin
   BCMSDHOSTReadBlockPIO(SDHCI);
  end
 else
  begin
   BCMSDHOSTWriteBlockPIO(SDHCI);
  end;  
  
 {Read Status Register} 
 Status:=BCMSDHOSTRead(SDHCI,BCMSDHOST_SDHSTS);
 if (Status and (BCMSDHOST_SDHSTS_CRC16_ERROR or BCMSDHOST_SDHSTS_CRC7_ERROR or BCMSDHOST_SDHSTS_FIFO_ERROR)) <> 0 then
  begin
   {$IFDEF INTERRUPT_DEBUG}
   if MMC_LOG_ENABLED then MMCLogError(nil,'BCMSDHOST: PIO Transfer error');
   {$ENDIF}
  
   SDHCI.Command.Status:=MMC_STATUS_INVALID_SEQUENCE;
  
   SemaphoreSignal(SDHCI.Wait);
  end
 else if (Status and (BCMSDHOST_SDHSTS_CMD_TIME_OUT or BCMSDHOST_SDHSTS_REW_TIME_OUT)) <> 0 then
  begin
   {$IFDEF INTERRUPT_DEBUG}
   if MMC_LOG_ENABLED then MMCLogError(nil,'BCMSDHOST: PIO Timeout error');
   {$ENDIF}

   SDHCI.Command.Status:=MMC_STATUS_TIMEOUT;
   
   SemaphoreSignal(SDHCI.Wait);
  end;
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
 
 Result:=MMC_STATUS_SUCCESS; 

 //bcm2835_sdhost_transfer_pio
end;

{==============================================================================}

function BCMSDHOSTReadBlockPIO(SDHCI:PSDHCIHost):LongWord;
{PIO block read handler for the BCMSDHOST SDHCI}
{Note: Not intended to be called directly by applications}

{Note: Caller must hold the host lock}
var
 Wait:Int64;
 Debug:LongWord;
 State:LongWord;
 Status:LongWord;
 BlockLen:LongWord;
 BlockSize:LongWord;
 BlockOffset:LongWord;
 ReadWords:LongWord;
 CopyWords:LongWord;
 BurstWords:LongWord;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check SDHCI}
 if SDHCI = nil then Exit;

 {$IF (DEFINED(BCMSDHOST_DEBUG) or DEFINED(MMC_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'BCMSDHOST: Read Block PIO');
 {$ENDIF}

 {Calculate Timeout}
 Wait:=ClockGetTotal + (PBCMSDHOSTHost(SDHCI).PIOTimeout * CLOCK_CYCLES_PER_MILLISECOND);
 
 {Get Block Size}
 BlockSize:=SDHCI.Command.Data.BlockSize;
 BlockOffset:=0;
 while BlockSize > 0 do
  begin
   {Set Default}
   Status:=0;
   
   {Calculate Block Length}
   BlockLen:=Min(SDHCI.Command.Data.BytesRemaining,BlockSize);
   if (BlockLen mod 4) <> 0 then
    begin
     {$IFDEF INTERRUPT_DEBUG}
     if MMC_LOG_ENABLED then MMCLogError(nil,'BCMSDHOST: PIO Read Invalid error');
     {$ENDIF}
     
     SDHCI.Command.Status:=MMC_STATUS_INVALID_DATA;
     
     SemaphoreSignal(SDHCI.Wait);
     Exit;
    end;
  
   {Update Block Size}
   Dec(BlockSize,BlockLen);
   
   {Calculate Copy Words}
   CopyWords:=BlockLen div 4;
   while CopyWords > 0 do
    begin
     {Calculate Burst Words}
     BurstWords:=BCMSDHOST_SDDATA_FIFO_PIO_BURST;
     if BurstWords > CopyWords then BurstWords:=CopyWords;
      
     {Read Debug Mode Register} 
     Debug:=BCMSDHOSTRead(SDHCI,BCMSDHOST_SDEDM);
     
     {Calculate Read Words}
     ReadWords:=(Debug shr 4) and $1f;
     if ReadWords < BurstWords then
      begin
       {Get FSM State}
       State:=Debug and BCMSDHOST_SDEDM_FSM_MASK;
       if (State <> BCMSDHOST_SDEDM_FSM_READDATA) and (State <> BCMSDHOST_SDEDM_FSM_READWAIT) and (State <> BCMSDHOST_SDEDM_FSM_READCRC) then
        begin
         {Read Status Register}
         Status:=BCMSDHOSTRead(SDHCI,BCMSDHOST_SDHSTS);
         if (Status and BCMSDHOST_SDHSTS_ERROR_MASK) <> 0 then Exit;
        end;
      
       {Check Timeout}
       if ClockGetTotal > Wait then
        begin
         {$IFDEF INTERRUPT_DEBUG}
         if MMC_LOG_ENABLED then MMCLogError(nil,'BCMSDHOST: PIO Read Timeout error');
         {$ENDIF}
         
         SDHCI.Command.Status:=MMC_STATUS_TIMEOUT;
         
         SemaphoreSignal(SDHCI.Wait);
         Exit;
        end; 
       
       {Wait for FIFO}
       NanosecondDelay((BurstWords - ReadWords) * PBCMSDHOSTHost(SDHCI).NanosecondsPerFifoWord);
       Continue;
      end
     else if ReadWords > CopyWords then
      begin
       {Update Read Words}
       ReadWords:=CopyWords;
      end;
     
     {Update Copy Words}
     Dec(CopyWords,ReadWords);
     
     while ReadWords > 0 do
      begin
       {Read Word}
       PLongWord(PtrUInt(SDHCI.Command.Data.Data) + PtrUInt(SDHCI.Command.Data.BlockOffset) + PtrUInt(BlockOffset))^:=BCMSDHOSTRead(SDHCI,BCMSDHOST_SDDATA);
       
       Dec(ReadWords);
       Inc(BlockOffset,4);
      end;
    end;
   
   if (Status and BCMSDHOST_SDHSTS_ERROR_MASK) <> 0 then Exit;
  end;
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
 
 {Update State}
 Inc(SDHCI.Command.Data.BlockOffset,SDHCI.Command.Data.BlockSize);
 Dec(SDHCI.Command.Data.BlocksRemaining);
 Dec(SDHCI.Command.Data.BytesRemaining,SDHCI.Command.Data.BlockSize);
 
 {$IF (DEFINED(BCMSDHOST_DEBUG) or DEFINED(MMC_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'BCMSDHOST: Read Block PIO completed (BlocksRemaining=' + IntToStr(SDHCI.Command.Data.BlocksRemaining) + ')'); 
 {$ENDIF}
 
 Result:=MMC_STATUS_SUCCESS; 

 //bcm2835_sdhost_read_block_pio
end;

{==============================================================================}

function BCMSDHOSTWriteBlockPIO(SDHCI:PSDHCIHost):LongWord;
{PIO block write handler for the BCMSDHOST SDHCI}
{Note: Not intended to be called directly by applications}

{Note: Caller must hold the host lock}
var
 Wait:Int64;
 Debug:LongWord;
 State:LongWord;
 Status:LongWord;
 BlockLen:LongWord;
 BlockSize:LongWord;
 BlockOffset:LongWord;
 WriteWords:LongWord;
 CopyWords:LongWord;
 BurstWords:LongWord;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check SDHCI}
 if SDHCI = nil then Exit;

 {$IF (DEFINED(BCMSDHOST_DEBUG) or DEFINED(MMC_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'BCMSDHOST: Write Block PIO');
 {$ENDIF}

 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}

 {Calculate Timeout}
 Wait:=ClockGetTotal + (PBCMSDHOSTHost(SDHCI).PIOTimeout * CLOCK_CYCLES_PER_MILLISECOND);

 {Get Block Size}
 BlockSize:=SDHCI.Command.Data.BlockSize;
 BlockOffset:=0;
 while BlockSize > 0 do
  begin
   {Set Default}
   Status:=0;

   {Calculate Block Length}
   BlockLen:=Min(SDHCI.Command.Data.BytesRemaining,BlockSize);
   if (BlockLen mod 4) <> 0 then
    begin
     {$IFDEF INTERRUPT_DEBUG}
     if MMC_LOG_ENABLED then MMCLogError(nil,'BCMSDHOST: PIO Write Invalid error');
     {$ENDIF}
     
     SDHCI.Command.Status:=MMC_STATUS_INVALID_DATA;
     
     SemaphoreSignal(SDHCI.Wait);
     Exit;
    end;

   {Update Block Size}
   Dec(BlockSize,BlockLen);
   
   {Calculate Copy Words}
   CopyWords:=BlockLen div 4;
   while CopyWords > 0 do
    begin
     {Calculate Burst Words}
     BurstWords:=BCMSDHOST_SDDATA_FIFO_PIO_BURST;
     if BurstWords > CopyWords then BurstWords:=CopyWords;
      
     {Read Debug Mode Register} 
     Debug:=BCMSDHOSTRead(SDHCI,BCMSDHOST_SDEDM);

     {Calculate Write Words}
     WriteWords:=BCMSDHOST_SDDATA_FIFO_WORDS - ((Debug shr 4) and $1f);
     if WriteWords < BurstWords then
      begin
       {Get FSM State}
       State:=Debug and BCMSDHOST_SDEDM_FSM_MASK;
       if (State <> BCMSDHOST_SDEDM_FSM_WRITEDATA) and (State <> BCMSDHOST_SDEDM_FSM_WRITESTART1) and (State <> BCMSDHOST_SDEDM_FSM_WRITESTART2) then
        begin
         {Read Status Register}
         Status:=BCMSDHOSTRead(SDHCI,BCMSDHOST_SDHSTS);
         if (Status and BCMSDHOST_SDHSTS_ERROR_MASK) <> 0 then Exit;
        end;

       {Check Timeout}
       if ClockGetTotal > Wait then
        begin
         {$IFDEF INTERRUPT_DEBUG}
         if MMC_LOG_ENABLED then MMCLogError(nil,'BCMSDHOST: PIO Write Timeout error');
         {$ENDIF}
         
         SDHCI.Command.Status:=MMC_STATUS_TIMEOUT;
         
         SemaphoreSignal(SDHCI.Wait);
         Exit;
        end; 
       {Wait for FIFO}
       NanosecondDelay((BurstWords - WriteWords) * PBCMSDHOSTHost(SDHCI).NanosecondsPerFifoWord);
       Continue;
      end
     else if WriteWords > CopyWords then
      begin
       {Update Write Words}
       WriteWords:=CopyWords;
      end;

     {Update Copy Words}
     Dec(CopyWords,WriteWords);

     while WriteWords > 0 do
      begin
       {Write Word}
       BCMSDHOSTWrite(SDHCI,BCMSDHOST_SDDATA,PLongWord(PtrUInt(SDHCI.Command.Data.Data) + PtrUInt(SDHCI.Command.Data.BlockOffset) + PtrUInt(BlockOffset))^);
       
       Dec(WriteWords);
       Inc(BlockOffset,4);
      end;
    end;
   
   if (Status and BCMSDHOST_SDHSTS_ERROR_MASK) <> 0 then Exit;
  end;
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
 
 {Update State}
 Inc(SDHCI.Command.Data.BlockOffset,SDHCI.Command.Data.BlockSize);
 Dec(SDHCI.Command.Data.BlocksRemaining);
 Dec(SDHCI.Command.Data.BytesRemaining,SDHCI.Command.Data.BlockSize);
 
 {$IF (DEFINED(BCMSDHOST_DEBUG) or DEFINED(MMC_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'BCMSDHOST: Write Block PIO completed (BlocksRemaining=' + IntToStr(SDHCI.Command.Data.BlocksRemaining) + ')'); 
 {$ENDIF}
 
 Result:=MMC_STATUS_SUCCESS; 

 //bcm2835_sdhost_write_block_pio
end;

{==============================================================================}

function BCMSDHOSTTransferComplete(SDHCI:PSDHCIHost):LongWord;
{Transfer complete handler for the BCMSDHOST SDHCI}
{Note: Not intended to be called directly by applications}

{Note: Caller must hold the host lock}
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check SDHCI}
 if SDHCI = nil then Exit;

 {$IF (DEFINED(BCMSDHOST_DEBUG) or DEFINED(MMC_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'BCMSDHOST: Transfer Complete');
 {$ENDIF}

 BCMSDHOSTWaitTransferComplete(SDHCI);
 
 SemaphoreSignal(SDHCI.Wait);
 
 Result:=MMC_STATUS_SUCCESS; 
 
 //bcm2835_sdhost_transfer_complete
end;

{==============================================================================}

function BCMSDHOSTWaitTransferComplete(SDHCI:PSDHCIHost):LongWord;
{Wait transfer complete handler for the BCMSDHOST SDHCI}
{Note: Not intended to be called directly by applications}

{Note: Caller must hold the host lock}
var
 Idle:LongWord;
 Debug:LongWord;
 State:LongWord;
 Timeout:LongWord;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check SDHCI}
 if SDHCI = nil then Exit;

 {$IF (DEFINED(BCMSDHOST_DEBUG) or DEFINED(MMC_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'BCMSDHOST: Wait Transfer Complete');
 {$ENDIF}

 {Check Command}
 if SDHCI.Command = nil then
  begin
   {$IFDEF INTERRUPT_DEBUG}
   if MMC_LOG_ENABLED then MMCLogError(nil,'BCMSDHOST: Wait transfer complete when no current command');
   {$ENDIF}
   Exit; 
  end; 

 {Check Data}
 if SDHCI.Command.Data = nil then
  begin
   {$IFDEF INTERRUPT_DEBUG}
   if MMC_LOG_ENABLED then MMCLogError(nil,'BCMSDHOST: Wait transfer complete when no current data');
   {$ENDIF}
   Exit; 
  end; 

 {Get Idle State}
 Idle:=BCMSDHOST_SDEDM_FSM_WRITESTART1;
 if (SDHCI.Command.Data.Flags and MMC_DATA_READ) <> 0 then Idle:=BCMSDHOST_SDEDM_FSM_READWAIT;
  
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
  
 {Read Debug Mode Register} 
 Debug:=BCMSDHOSTRead(SDHCI,BCMSDHOST_SDEDM);
 
 Timeout:=0;
 while True do
  begin
   {Get FSM State}
   State:=Debug and BCMSDHOST_SDEDM_FSM_MASK;

   {Check State}
   if (State = BCMSDHOST_SDEDM_FSM_IDENTMODE) or (State = BCMSDHOST_SDEDM_FSM_DATAMODE) then Break;
  
   {Check State}
   if State = Idle then
    begin
     {Update Debug Mode}
     BCMSDHOSTWrite(SDHCI,BCMSDHOST_SDEDM,Debug or BCMSDHOST_SDEDM_FORCE_DATA_MODE);
     
     Break;
    end;
    
   {Check Timeout} 
   Inc(Timeout);
   if Timeout = 100000 then
    begin
     {$IFDEF INTERRUPT_DEBUG}
     if MMC_LOG_ENABLED then MMCLogError(nil,'BCMSDHOST: Wait transfer complete still waiting after ' + IntToStr(Timeout) +  ' retries');
     {$ENDIF}
     
     SDHCI.Command.Status:=MMC_STATUS_TIMEOUT;
     
     {Semaphore signaled by BCMSDHOSTTransferComplete}
     Exit;
    end;
    
   {Memory Barrier}
   DataMemoryBarrier;
   
   {Read Debug Mode Register} 
   Debug:=BCMSDHOSTRead(SDHCI,BCMSDHOST_SDEDM);
  end;
  
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
  
 Result:=MMC_STATUS_SUCCESS; 
 
 //bcm2835_sdhost_wait_transfer_complete
end;

{==============================================================================}

function BCMSDHOSTFinishCommand(SDHCI:PSDHCIHost;AllowWait:Boolean):LongWord;
{Finish command handler for the BCMSDHOST SDHCI}
{Note: Not intended to be called directly by applications}

{Note: Caller must hold the host lock}
var
 Debug:LongWord;
 State:LongWord;
 Status:LongWord;
 Command:LongWord;
 Retries:LongWord;
 Count:LongWord;
 Wait:Int64;
 Start:Int64;
 Current:Int64;
 Difference:Int64;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check SDHCI}
 if SDHCI = nil then Exit;

 {$IF (DEFINED(BCMSDHOST_DEBUG) or DEFINED(MMC_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'BCMSDHOST: Finish Command');
 {$ENDIF}

 {Check Command}
 if SDHCI.Command = nil then
  begin
   {$IFDEF INTERRUPT_DEBUG}
   if MMC_LOG_ENABLED then MMCLogError(nil,'BCMSDHOST: Finish command when no current command');
   {$ENDIF}
   Exit; 
  end; 

 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}

 {Poll quickly at first}
 Retries:=PBCMSDHOSTHost(SDHCI).CommandQuickPollRetries;
 if Retries = 0 then
  begin
   {Work out how many polls take 1us by timing 10us}
   Retries:=1;
   Difference:=0;
   
   repeat
    Retries:=Retries * 2;
    
    Start:=ClockGetTotal;
    
    for Count:=0 to Retries - 1 do
     begin
      {Memory Barrier}
      DataMemoryBarrier;
      
      Command:=BCMSDHOSTRead(SDHCI,BCMSDHOST_SDCMD);
     end;     
    
    Current:=ClockGetTotal;
    Difference:=(Current - Start) div CLOCK_CYCLES_PER_MICROSECOND;
    
   until (Difference >= 10);
   
   PBCMSDHOSTHost(SDHCI).CommandQuickPollRetries:=((Retries * Difference + 9) * BCMSDHOST_CMD_DALLY_US) div 10 + 1;

   {$IF (DEFINED(BCMSDHOST_DEBUG) or DEFINED(MMC_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
   if MMC_LOG_ENABLED then MMCLogDebug(nil,'BCMSDHOST: Command quick poll retries = ' + IntToStr(PBCMSDHOSTHost(SDHCI).CommandQuickPollRetries));
   {$ENDIF}
   
   Retries:=1; {We've already waited long enough this time}
  end;
 
 {Check New Flag in Command Register} 
 Command:=BCMSDHOSTRead(SDHCI,BCMSDHOST_SDCMD);
 while (Command and BCMSDHOST_SDCMD_NEW_FLAG) <> 0 do
  begin
   Dec(Retries);
   if Retries = 0 then Break;
   
   {Memory Barrier}
   DataMemoryBarrier;
   
   {Read Command Register}
   Command:=BCMSDHOSTRead(SDHCI,BCMSDHOST_SDCMD);
  end;
 
 {Check Retries} 
 if Retries = 0 then
  begin
   if not AllowWait then
    begin
     {Schedule Wait Worker}
     WorkerSchedule(0,TWorkerTask(BCMSDHOSTCommandWaitWorker),SDHCI,nil);
     
     Exit;
    end;
    
   {Wait max 100 ms}
   Wait:=ClockGetTotal + (100 * CLOCK_CYCLES_PER_MILLISECOND);
   while ClockGetTotal < Wait do
    begin
     {Release the Lock}
     BCMSDHOSTUnlock(SDHCI);
     
     {Wait 10 microseconds}
     MicrosecondDelay(10);
     
     {Acquire the Lock}
     if BCMSDHOSTLock(SDHCI) <> ERROR_SUCCESS then Exit;
     
     {Read Command Register}
     Command:=BCMSDHOSTRead(SDHCI,BCMSDHOST_SDCMD);
     
     {Check for New Flag}
     if (Command and BCMSDHOST_SDCMD_NEW_FLAG) = 0 then Break;
    end;
  end;
  
 {Check for errors}
 if (Command and BCMSDHOST_SDCMD_NEW_FLAG) <> 0 then
  begin
   {$IFDEF INTERRUPT_DEBUG}
   if MMC_LOG_ENABLED then MMCLogError(nil,'BCMSDHOST: Command never completed');
   {$ENDIF}
   
   SDHCI.Command.Status:=MMC_STATUS_HARDWARE_ERROR;
   
   SemaphoreSignal(SDHCI.Wait);
   Exit;
  end
 else if (Command and BCMSDHOST_SDCMD_FAIL_FLAG) <> 0 then 
  begin
   {Read Status Register}
   Status:=BCMSDHOSTRead(SDHCI,BCMSDHOST_SDHSTS);
   
   {Clear the errors}
   BCMSDHOSTWrite(SDHCI,BCMSDHOST_SDHSTS,BCMSDHOST_SDHSTS_ERROR_MASK);
   
   if ((Status and BCMSDHOST_SDHSTS_CRC7_ERROR) <> 0) and (SDHCI.Command.Command = MMC_CMD_SEND_OP_COND) then
    begin
     {$IFDEF INTERRUPT_DEBUG}
     if MMC_LOG_ENABLED then MMCLogError(nil,'BCMSDHOST: Ignoring CRC7 error for MMC_CMD_SEND_OP_COND');
     {$ENDIF}
    end
   else 
    begin
     if (Status and BCMSDHOST_SDHSTS_CMD_TIME_OUT) <> 0 then
      begin
       {$IFDEF INTERRUPT_DEBUG}
       if MMC_LOG_ENABLED then MMCLogError(nil,'BCMSDHOST: Command Timeout');
       {$ENDIF}

       SDHCI.Command.Status:=MMC_STATUS_TIMEOUT;
       
       SemaphoreSignal(SDHCI.Wait);
      end
     else
      begin
       {$IFDEF INTERRUPT_DEBUG}
       if MMC_LOG_ENABLED then MMCLogError(nil,'BCMSDHOST: Unexpected Command Error');
       {$ENDIF}

       SDHCI.Command.Status:=MMC_STATUS_INVALID_SEQUENCE;
       
       SemaphoreSignal(SDHCI.Wait);
      end;
      
     {Read Debug Mode Register}
     Debug:=BCMSDHOSTRead(SDHCI,BCMSDHOST_SDEDM);
     
     {Get FSM State}
     State:=Debug and BCMSDHOST_SDEDM_FSM_MASK;
     if (State = BCMSDHOST_SDEDM_FSM_READWAIT) or (State = BCMSDHOST_SDEDM_FSM_WRITESTART1) then
      begin
       {Update Debug Mode}
       BCMSDHOSTWrite(SDHCI,BCMSDHOST_SDEDM,Debug or BCMSDHOST_SDEDM_FORCE_DATA_MODE);
      end;
     
     Exit;
    end;
  end;
  
 {Check for Response}
 if (SDHCI.Command.ResponseType and MMC_RSP_PRESENT) <> 0 then
  begin
   {Check for 136 bit Response}
   if (SDHCI.Command.ResponseType and MMC_RSP_136) <> 0 then
    begin
     for Count:=0 to 3 do
      begin
       SDHCI.Command.Response[3 - Count]:=BCMSDHOSTRead(SDHCI,BCMSDHOST_SDRSP0 + Count * 4);
      end;
    end
   else
    begin
     SDHCI.Command.Response[0]:=BCMSDHOSTRead(SDHCI,BCMSDHOST_SDRSP0);
    end;    
  end;
 
 SDHCI.Command.CommandCompleted:=True;
  
 if SDHCI.Command.Data = nil then
  begin
   SDHCI.Command.Status:=MMC_STATUS_SUCCESS;
   
   SemaphoreSignal(SDHCI.Wait);
  end
 else if SDHCI.Command.DataCompleted then
  begin
   SDHCI.Command.Status:=MMC_STATUS_SUCCESS;
   
   BCMSDHOSTTransferComplete(SDHCI);
  end;

 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 

 Result:=MMC_STATUS_SUCCESS; 

 //bcm2835_sdhost_finish_command
end;

{==============================================================================}

function BCMSDHOSTFinishData(SDHCI:PSDHCIHost):LongWord;
{Finish data handler for the BCMSDHOST SDHCI}
{Note: Not intended to be called directly by applications}

{Note: Caller must hold the host lock}
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check SDHCI}
 if SDHCI = nil then Exit;

 {$IF (DEFINED(BCMSDHOST_DEBUG) or DEFINED(MMC_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'BCMSDHOST: Finish Data');
 {$ENDIF}

 {Check Command}
 if SDHCI.Command = nil then
  begin
   {$IFDEF INTERRUPT_DEBUG}
   if MMC_LOG_ENABLED then MMCLogError(nil,'BCMSDHOST: Finish data when no current command');
   {$ENDIF}
   Exit; 
  end; 

 {Check Data}
 if SDHCI.Command.Data = nil then
  begin
   {$IFDEF INTERRUPT_DEBUG}
   if MMC_LOG_ENABLED then MMCLogError(nil,'BCMSDHOST: Finish data when no current data');
   {$ENDIF}
   Exit; 
  end; 

 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}

 {Disable Block and Data Interrupts}
 PBCMSDHOSTHost(SDHCI).HostConfiguration:=PBCMSDHOSTHost(SDHCI).HostConfiguration and not(BCMSDHOST_SDHCFG_DATA_IRPT_EN or BCMSDHOST_SDHCFG_BLOCK_IRPT_EN);
 BCMSDHOSTWrite(SDHCI,BCMSDHOST_SDHCFG,PBCMSDHOSTHost(SDHCI).HostConfiguration);
 
 {Check for Error}
 if SDHCI.Command.Status <> MMC_STATUS_NOT_PROCESSED then
  begin
   SDHCI.Command.Data.BytesTransfered:=0;
  end   
 else
  begin
   SDHCI.Command.Data.BytesTransfered:=(SDHCI.Command.Data.BlockSize * SDHCI.Command.Data.BlockCount);
  end;  
 
 if SDHCI.Command.Status <> MMC_STATUS_NOT_PROCESSED then Exit;
 
 SDHCI.Command.DataCompleted:=True;
 
 if not SDHCI.Command.CommandCompleted then
  begin
   {Data managed to finish before the command completed. Make sure we do things in the proper order}
   {$IFDEF INTERRUPT_DEBUG}
   if MMC_LOG_ENABLED then MMCLogError(nil,'BCMSDHOST: Data finished early');
   {$ENDIF}
  end
 else
  begin
   SDHCI.Command.Status:=MMC_STATUS_SUCCESS;
   
   BCMSDHOSTTransferComplete(SDHCI);
  end;  
 
 Result:=MMC_STATUS_SUCCESS; 

 //bcm2835_sdhost_finish_data
end;

{==============================================================================}

function BCMSDHOSTBusyInterrupt(SDHCI:PSDHCIHost;InterruptMask:LongWord):LongWord;
{Busy interrupt handler for the BCMSDHOST SDHCI}
{Note: Not intended to be called directly by applications}

{Note: Caller must hold the host lock}
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check SDHCI}
 if SDHCI = nil then Exit;

 {$IF (DEFINED(BCMSDHOST_DEBUG) or DEFINED(MMC_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'BCMSDHOST: Busy Interrupt (InterruptMask=' + IntToHex(InterruptMask,8) + ')');
 {$ENDIF}

 {Check Command}
 if SDHCI.Command = nil then
  begin
   {$IFDEF INTERRUPT_DEBUG}
   if MMC_LOG_ENABLED then MMCLogError(nil,'BCMSDHOST: Busy interrupt when no current command (InterruptMask=' + IntToHex(InterruptMask,8) + ')');
   {$ENDIF}
   Exit; 
  end; 

 {Check Busy}
 if not PBCMSDHOSTHost(SDHCI).UseBusy then
  begin
   {$IFDEF INTERRUPT_DEBUG}
   if MMC_LOG_ENABLED then MMCLogError(nil,'BCMSDHOST: Busy interrupt when not expecting one (InterruptMask=' + IntToHex(InterruptMask,8) + ')');
   {$ENDIF}
   Exit; 
  end;
  
 {Clear Busy}
 PBCMSDHOSTHost(SDHCI).UseBusy:=False;
 
 {Check Errors}
 if (InterruptMask and BCMSDHOST_SDHSTS_ERROR_MASK) <> 0 then
  begin
   if (InterruptMask and BCMSDHOST_SDHSTS_CRC7_ERROR) <> 0 then
    begin
     SDHCI.Command.Status:=MMC_STATUS_INVALID_SEQUENCE;
    
     SemaphoreSignal(SDHCI.Wait);
    end
   else if (InterruptMask and (BCMSDHOST_SDHSTS_CRC16_ERROR or BCMSDHOST_SDHSTS_FIFO_ERROR)) <> 0 then  
    begin
     SDHCI.Command.Status:=MMC_STATUS_INVALID_SEQUENCE;
     
     SemaphoreSignal(SDHCI.Wait);
    end
   else if (InterruptMask and BCMSDHOST_SDHSTS_REW_TIME_OUT) <> 0 then  
    begin
     SDHCI.Command.Status:=MMC_STATUS_TIMEOUT;
    
     SemaphoreSignal(SDHCI.Wait);
    end
   else if (InterruptMask and BCMSDHOST_SDHSTS_CMD_TIME_OUT) <> 0 then 
    begin
     SDHCI.Command.Status:=MMC_STATUS_TIMEOUT;
    
     SemaphoreSignal(SDHCI.Wait);
    end;
  end
 else
  begin
   BCMSDHOSTFinishCommand(SDHCI,False);
  end;  
 
 Result:=MMC_STATUS_SUCCESS; 

 //bcm2835_sdhost_busy_irq
end;

{==============================================================================}

function BCMSDHOSTDataInterrupt(SDHCI:PSDHCIHost;InterruptMask:LongWord):LongWord;
{Data interrupt handler for the BCMSDHOST SDHCI}
{Note: Not intended to be called directly by applications}

{Note: Caller must hold the host lock}
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check SDHCI}
 if SDHCI = nil then Exit;

 {$IF (DEFINED(BCMSDHOST_DEBUG) or DEFINED(MMC_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'BCMSDHOST: Data Interrupt (InterruptMask=' + IntToHex(InterruptMask,8) + ')');
 {$ENDIF}

 {There are no dedicated data/space available interrupt status bits, so it is
  necessary to use the single shared data/space available FIFO status bits. It
  is therefore not an error to get here when there is no data transfer in progress}
  
 {Check Command}
 if SDHCI.Command = nil then
  begin
   {$IFDEF INTERRUPT_DEBUG}
   if MMC_LOG_ENABLED then MMCLogError(nil,'BCMSDHOST: Data interrupt when no current command (InterruptMask=' + IntToHex(InterruptMask,8) + ')');
   {$ENDIF}
   Exit; 
  end; 

 {Check Data}
 if SDHCI.Command.Data = nil then
  begin
   {$IFDEF INTERRUPT_DEBUG}
   if MMC_LOG_ENABLED then MMCLogError(nil,'BCMSDHOST: Data interrupt when no current data (InterruptMask=' + IntToHex(InterruptMask,8) + ')');
   {$ENDIF}
   Exit; 
  end; 
  
 {Check Errors}
 if (InterruptMask and (BCMSDHOST_SDHSTS_CRC16_ERROR or BCMSDHOST_SDHSTS_FIFO_ERROR or BCMSDHOST_SDHSTS_REW_TIME_OUT)) <> 0 then
  begin
   if (InterruptMask and (BCMSDHOST_SDHSTS_CRC16_ERROR or BCMSDHOST_SDHSTS_FIFO_ERROR)) <> 0 then
    begin
     SDHCI.Command.Status:=MMC_STATUS_INVALID_SEQUENCE;
     
     BCMSDHOSTFinishData(SDHCI);
     SemaphoreSignal(SDHCI.Wait);
     Exit;
    end
   else
    begin
     SDHCI.Command.Status:=MMC_STATUS_TIMEOUT;
    
     BCMSDHOSTFinishData(SDHCI);
     SemaphoreSignal(SDHCI.Wait);
     Exit;
    end;
  end;  
  
 if (SDHCI.Command.Data.Flags and MMC_DATA_WRITE) <> 0 then
  begin
   {Use the block interrupt for writes after the first block}
   PBCMSDHOSTHost(SDHCI).HostConfiguration:=PBCMSDHOSTHost(SDHCI).HostConfiguration and not(BCMSDHOST_SDHCFG_DATA_IRPT_EN);
   PBCMSDHOSTHost(SDHCI).HostConfiguration:=PBCMSDHOSTHost(SDHCI).HostConfiguration or BCMSDHOST_SDHCFG_BLOCK_IRPT_EN;
   BCMSDHOSTWrite(SDHCI,BCMSDHOST_SDHCFG,PBCMSDHOSTHost(SDHCI).HostConfiguration);
    
   BCMSDHOSTTransferPIO(SDHCI);
  end
 else
  begin
   BCMSDHOSTTransferPIO(SDHCI);
   
   if SDHCI.Command.Data.BlocksRemaining = 0 then
    begin
     BCMSDHOSTFinishData(SDHCI);
    end;
  end;
  
 Result:=MMC_STATUS_SUCCESS; 
 
 //bcm2835_sdhost_data_irq
end;

{==============================================================================}

function BCMSDHOSTBlockInterrupt(SDHCI:PSDHCIHost;InterruptMask:LongWord):LongWord;
{Block interrupt handler for the BCMSDHOST SDHCI}
{Note: Not intended to be called directly by applications}

{Note: Caller must hold the host lock}
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check SDHCI}
 if SDHCI = nil then Exit;

 {$IF (DEFINED(BCMSDHOST_DEBUG) or DEFINED(MMC_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'BCMSDHOST: Block Interrupt (InterruptMask=' + IntToHex(InterruptMask,8) + ')');
 {$ENDIF}

 {Check Command}
 if SDHCI.Command = nil then
  begin
   {$IFDEF INTERRUPT_DEBUG}
   if MMC_LOG_ENABLED then MMCLogError(nil,'BCMSDHOST: Block interrupt when no current command (InterruptMask=' + IntToHex(InterruptMask,8) + ')');
   {$ENDIF}
   Exit; 
  end; 

 {Check Data}
 if SDHCI.Command.Data = nil then
  begin
   {$IFDEF INTERRUPT_DEBUG}
   if MMC_LOG_ENABLED then MMCLogError(nil,'BCMSDHOST: Block interrupt when no current data (InterruptMask=' + IntToHex(InterruptMask,8) + ')');
   {$ENDIF}
   Exit; 
  end; 

 {Check Errors}
 if (InterruptMask and (BCMSDHOST_SDHSTS_CRC16_ERROR or BCMSDHOST_SDHSTS_FIFO_ERROR or BCMSDHOST_SDHSTS_REW_TIME_OUT)) <> 0 then
  begin
   if (InterruptMask and (BCMSDHOST_SDHSTS_CRC16_ERROR or BCMSDHOST_SDHSTS_FIFO_ERROR)) <> 0 then
    begin
     SDHCI.Command.Status:=MMC_STATUS_INVALID_SEQUENCE;
     
     BCMSDHOSTFinishData(SDHCI);
     SemaphoreSignal(SDHCI.Wait);
     Exit;
    end
   else
    begin
     SDHCI.Command.Status:=MMC_STATUS_TIMEOUT;
     
     BCMSDHOSTFinishData(SDHCI);
     SemaphoreSignal(SDHCI.Wait);
     Exit;
    end;
  end;  
  
 if not PBCMSDHOSTHost(SDHCI).UseDMA then
  begin
   if SDHCI.Command.Data.BlocksRemaining = 0 then
    begin
     BCMSDHOSTFinishData(SDHCI);
    end
   else
    begin
     BCMSDHOSTTransferPIO(SDHCI);
    end;
  end
 else if (SDHCI.Command.Data.Flags and MMC_DATA_WRITE) <> 0 then
  begin
   BCMSDHOSTFinishData(SDHCI);
  end;

 Result:=MMC_STATUS_SUCCESS; 

 //bcm2835_sdhost_block_irq
end;

{==============================================================================}
{==============================================================================}
{BCMSDHOST MMC Functions}
function BCMSDHOSTSendCommand(MMC:PMMCDevice;Command:PMMCCommand):LongWord;
{Implementation of MMCDeviceSendCommand API for BCMSDHOST SDHCI}
{Note: Not intended to be called directly by applications, use MMCDeviceSendCommand instead}
var
 Debug:LongWord;
 State:LongWord;
 Value:LongWord;
 Status:LongWord;
 Delay:LongWord;
 Timeout:LongWord;
 Interrupts:LongWord;
 FirstBlock:LongWord;
 LastBlock:LongWord;
 TimeSinceStop:Int64;
 SDHCI:PSDHCIHost;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IF DEFINED(BCMSDHOST_DEBUG) or DEFINED(MMC_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'BCMSDHOST: MMC Send Command');
 {$ENDIF}

 {Check Command}
 if Command = nil then Exit;

 {Get SDHCI}
 SDHCI:=PSDHCIHost(MMC.Device.DeviceData);
 if SDHCI = nil then Exit;
 
 {Acquire the Lock}
 if MutexLock(MMC.Lock) = ERROR_SUCCESS then
  begin
   try
    {Check Block Size}
    if (Command.Data <> nil) and not(IsPowerOf2(Command.Data.BlockSize)) then
     begin
      if MMC_LOG_ENABLED then MMCLogError(nil,'BCMSDHOST: Unsupported Block Size');
         
      Command.Status:=MMC_STATUS_INVALID_DATA;
      Exit;
     end;

    {Check Response Type}
    if ((Command.ResponseType and MMC_RSP_136) <> 0) and ((Command.ResponseType and MMC_RSP_BUSY) <> 0) then 
     begin
      if MMC_LOG_ENABLED then MMCLogError(nil,'BCMSDHOST: Unsupported Response Type');
         
      Command.Status:=MMC_STATUS_INVALID_DATA;
      Exit;
     end;

    {Setup Status}
    Command.Status:=MMC_STATUS_NOT_PROCESSED;
    try
     {Check Reset Clock}
     if PBCMSDHOSTHost(SDHCI).ResetClock then
      begin
       BCMSDHOSTSetClock(SDHCI,SDHCI.Clock);
      end;

     {Memory Barrier}
     DataMemoryBarrier; {Before the First Write}
      
     {Acquire the Lock}
     if BCMSDHOSTLock(SDHCI) <> ERROR_SUCCESS then Exit;
     try
      {Read Debug Mode Register} 
      Debug:=BCMSDHOSTRead(SDHCI,BCMSDHOST_SDEDM);
     
      {Get FSM State}
      State:=Debug and BCMSDHOST_SDEDM_FSM_MASK;
      if (State <> BCMSDHOST_SDEDM_FSM_IDENTMODE) and (State <> BCMSDHOST_SDEDM_FSM_DATAMODE) then
       begin
        {$IFDEF INTERRUPT_DEBUG}
        if MMC_LOG_ENABLED then MMCLogError(nil,'BCMSDHOST: Previous command not complete');
        {$ENDIF}
        
        Command.Status:=MMC_STATUS_HARDWARE_ERROR;
        Exit;
       end;

      {Wait max 100 ms}
      Timeout:=10000;
      while (BCMSDHOSTRead(SDHCI,BCMSDHOST_SDCMD) and BCMSDHOST_SDCMD_NEW_FLAG) <> 0 do
       begin
        {Check Timeout}
        if Timeout = 0 then
         begin
          {$IFDEF INTERRUPT_DEBUG}
          if MMC_LOG_ENABLED then MMCLogError(nil,'BCMSDHOST: Previous command never completed');
          {$ENDIF}
          
          Command.Status:=MMC_STATUS_HARDWARE_ERROR;
          Exit;
         end;
        
        Dec(Timeout); 
        MicrosecondDelay(10);
       end;
       
      {Clear any error flags} 
      Status:=BCMSDHOSTRead(SDHCI,BCMSDHOST_SDHSTS);
      if (Status and BCMSDHOST_SDHSTS_ERROR_MASK) <> 0 then
       begin
        BCMSDHOSTWrite(SDHCI,BCMSDHOST_SDHSTS,Status);
       end;
     finally
      {Release the Lock}
      BCMSDHOSTUnlock(SDHCI);
     end;
     
     {Recalculate Delay}
     Delay:=(10000 - Timeout) div 100;
     if Delay > PBCMSDHOSTHost(SDHCI).MaxDelay then
      begin
       PBCMSDHOSTHost(SDHCI).MaxDelay:=Delay;
       
       if MMC_LOG_ENABLED then MMCLogWarn(nil,'BCMSDHOST: Controller hung for ' + IntToStr(Delay) + ' milliseconds');
      end;

     {Setup Command}
     Value:=Command.Command and BCMSDHOST_SDCMD_CMD_MASK;
     
     {Setup Response}
     PBCMSDHOSTHost(SDHCI).UseBusy:=False;
     if (Command.ResponseType and MMC_RSP_PRESENT) = 0 then
      begin
       Value:=Value or BCMSDHOST_SDCMD_NO_RESPONSE;
      end
     else
      begin
       if (Command.ResponseType and MMC_RSP_136) <> 0 then
        begin
         Value:=Value or BCMSDHOST_SDCMD_LONG_RESPONSE;
        end;
       if (Command.ResponseType and MMC_RSP_BUSY) <> 0 then
        begin
         Value:=Value or BCMSDHOST_SDCMD_BUSYWAIT;
         
         PBCMSDHOSTHost(SDHCI).UseBusy:=True;
        end;
      end;
    
     {Check Data}
     if Command.Data = nil then
      begin
       {Setup Command}
       PBCMSDHOSTHost(SDHCI).UseDMA:=False;
      end
     else 
      begin
       {Setup Data}
       Command.Data.BlockOffset:=0;
       Command.Data.BlocksRemaining:=Command.Data.BlockCount;
       Command.Data.BytesRemaining:=Command.Data.BlockSize * Command.Data.BlockCount;
       Command.Data.BytesTransfered:=0;

       {Check DMA}
       PBCMSDHOSTHost(SDHCI).UseDMA:=PBCMSDHOSTHost(SDHCI).AllowDMA and (Command.Data.BlockCount > PBCMSDHOSTHost(SDHCI).PIOLimit); 
       if PBCMSDHOSTHost(SDHCI).UseDMA then
        begin
         {Prepare DMA}
         Status:=BCMSDHOSTPrepareDMA(SDHCI,Command);
         if Status <> MMC_STATUS_SUCCESS then
          begin
           Result:=Status;
           Exit;
          end;
        end;

       {$IF DEFINED(BCMSDHOST_DEBUG) or DEFINED(MMC_DEBUG)}
       if MMC_LOG_ENABLED then MMCLogDebug(nil,'BCMSDHOST: MMC Send Command (BlockSize=' + IntToStr(Command.Data.BlockSize) + ')');
       if MMC_LOG_ENABLED then MMCLogDebug(nil,'BCMSDHOST: MMC Send Command (BlockCount=' + IntToStr(Command.Data.BlockCount) + ')');
       {$ENDIF}
        
       {Acquire the Lock}
       if BCMSDHOSTLock(SDHCI) <> ERROR_SUCCESS then Exit;
       
       {Setup Transfer Interrupts}
       Interrupts:=BCMSDHOST_SDHCFG_DATA_IRPT_EN or BCMSDHOST_SDHCFG_BLOCK_IRPT_EN or BCMSDHOST_SDHCFG_BUSY_IRPT_EN;
       if PBCMSDHOSTHost(SDHCI).UseDMA then
        begin
         PBCMSDHOSTHost(SDHCI).HostConfiguration:=(PBCMSDHOSTHost(SDHCI).HostConfiguration and not(Interrupts)) or BCMSDHOST_SDHCFG_BUSY_IRPT_EN; 
        end
       else
        begin
         PBCMSDHOSTHost(SDHCI).HostConfiguration:=(PBCMSDHOSTHost(SDHCI).HostConfiguration and not(Interrupts)) or BCMSDHOST_SDHCFG_DATA_IRPT_EN or BCMSDHOST_SDHCFG_BUSY_IRPT_EN; 
        end;
       BCMSDHOSTWrite(SDHCI,BCMSDHOST_SDHCFG,PBCMSDHOSTHost(SDHCI).HostConfiguration);
       
       {Write Block Size}
       BCMSDHOSTWrite(SDHCI,BCMSDHOST_SDHBCT,Command.Data.BlockSize);
       
       {Write Block Count}
       BCMSDHOSTWrite(SDHCI,BCMSDHOST_SDHBLC,Command.Data.BlockCount);

       {Release the Lock}
       BCMSDHOSTUnlock(SDHCI);
       
       {Check Delay}
       if PBCMSDHOSTHost(SDHCI).DelayAfterThisStop > 0 then
        begin
         TimeSinceStop:=(ClockGetTotal - PBCMSDHOSTHost(SDHCI).StopTime) div CLOCK_CYCLES_PER_MICROSECOND;
         if TimeSinceStop < PBCMSDHOSTHost(SDHCI).DelayAfterThisStop then
          begin
           MicrosecondDelay(PBCMSDHOSTHost(SDHCI).DelayAfterThisStop - TimeSinceStop);
          end;
        end;
       PBCMSDHOSTHost(SDHCI).DelayAfterThisStop:=PBCMSDHOSTHost(SDHCI).DelayAfterStop;
       
       {Check Hazardous Blocks}
       if ((Command.Data.Flags and MMC_DATA_READ) <> 0) and (BCMSDHOST_USE_CMD23_FLAGS = 0) then
        begin
         if (MMC.Storage <> nil) and (MMC.Storage.BlockCount > 0) then
          begin
           {Intentionally include the following sector because without CMD23/SBC the read may run on}
           FirstBlock:=Command.Argument;
           LastBlock:=FirstBlock + Command.Data.BlockCount;
           
           {See if read crosses one of the hazardous sectors}
           if (LastBlock >= (MMC.Storage.BlockCount - 64)) and (FirstBlock <= (MMC.Storage.BlockCount - 64))
            or (LastBlock >= (MMC.Storage.BlockCount - 32)) and (FirstBlock <= (MMC.Storage.BlockCount - 32)) then
            begin
             if MMC_LOG_ENABLED then MMCLogWarn(nil,'BCMSDHOST: Adding stop delay for hazardous sector read');
             
             {Add Delay}
             PBCMSDHOSTHost(SDHCI).DelayAfterThisStop:=Max(250,PBCMSDHOSTHost(SDHCI).DelayAfterStop);
            end;
          end;
        end; 
    
       {Check Direction}
       if (Command.Data.Flags and MMC_DATA_WRITE) <> 0 then
        begin
         Value:=Value or BCMSDHOST_SDCMD_WRITE_CMD;
        end;
       if (Command.Data.Flags and MMC_DATA_READ) <> 0 then
        begin
         Value:=Value or BCMSDHOST_SDCMD_READ_CMD;
        end;
      end;     
       
     {Setup Command}
     SDHCI.Command:=Command;
     try
      {$IF DEFINED(BCMSDHOST_DEBUG) or DEFINED(MMC_DEBUG)}
      if MMC_LOG_ENABLED then MMCLogDebug(nil,'BCMSDHOST: MMC Send Command (Argument=' + IntToHex(Command.Argument,8) + ')');
      if MMC_LOG_ENABLED then MMCLogDebug(nil,'BCMSDHOST: MMC Send Command (Command=' + IntToHex(Value,8) + ')');
      {$ENDIF}

      {Acquire the Lock}
      if BCMSDHOSTLock(SDHCI) <> ERROR_SUCCESS then Exit;
     
      {Write Argument}
      BCMSDHOSTWrite(SDHCI,BCMSDHOST_SDARG,Command.Argument);
      
      {Write Command}
      BCMSDHOSTWrite(SDHCI,BCMSDHOST_SDCMD,Value or BCMSDHOST_SDCMD_NEW_FLAG);

      {Memory Barrier}
      DataMemoryBarrier; {After the Last Read} 

      {Release the Lock}
      BCMSDHOSTUnlock(SDHCI);

      {Wait for Completion}
      if Command.Data = nil then
       begin
        {Check Use Busy}
        if not PBCMSDHOSTHost(SDHCI).UseBusy then
         begin
          {Acquire the Lock}
          if BCMSDHOSTLock(SDHCI) <> ERROR_SUCCESS then Exit;
          
          {Poll for Completion}
          BCMSDHOSTFinishCommand(SDHCI,True);
          
          {Release the Lock}
          BCMSDHOSTUnlock(SDHCI);
         end;

        {Wait for Signal with Timeout (100ms)}
        Status:=SemaphoreWaitEx(SDHCI.Wait,100);
        if Status <> ERROR_SUCCESS then
         begin
          if Status = ERROR_WAIT_TIMEOUT then
           begin
            if MMC_LOG_ENABLED then MMCLogError(nil,'BCMSDHOST: MMC Send Command Response Timeout');
              
            Command.Status:=MMC_STATUS_TIMEOUT;
           end
          else
           begin
            if MMC_LOG_ENABLED then MMCLogError(nil,'BCMSDHOST: MMC Send Command Response Failure');
              
            Command.Status:=MMC_STATUS_HARDWARE_ERROR;
           end;          
         end;
       end
      else
       begin
        {Check DMA}
        if PBCMSDHOSTHost(SDHCI).UseDMA then
         begin
          {Start DMA}
          if DMATransferRequestEx(DMAHostGetDefault,@PBCMSDHOSTHost(SDHCI).DMAData,BCMSDHOSTDMARequestCompleted,SDHCI,PBCMSDHOSTHost(SDHCI).DMADirection,PBCMSDHOSTHost(SDHCI).DREQ,DMA_REQUEST_FLAG_NONE) <> ERROR_SUCCESS then Exit;
         end;
        
        {Check Use Busy}
        if not PBCMSDHOSTHost(SDHCI).UseBusy then
         begin
          {Acquire the Lock}
          if BCMSDHOSTLock(SDHCI) <> ERROR_SUCCESS then Exit;
          
          {Poll for Completion}
          BCMSDHOSTFinishCommand(SDHCI,True);
          
          {Release the Lock}
          BCMSDHOSTUnlock(SDHCI);
         end;
        
        {Wait for Signal with Timeout (5000ms)}
        Status:=SemaphoreWaitEx(SDHCI.Wait,5000);
        if Status <> ERROR_SUCCESS then
         begin
          if Status = ERROR_WAIT_TIMEOUT then
           begin
            if MMC_LOG_ENABLED then MMCLogError(nil,'BCMSDHOST: MMC Send Data Response Timeout');
            
            Command.Status:=MMC_STATUS_TIMEOUT;
           end
          else
           begin
            if MMC_LOG_ENABLED then MMCLogError(nil,'BCMSDHOST: MMC Send Data Response Failure');
            
            Command.Status:=MMC_STATUS_HARDWARE_ERROR;
           end;          
         end;
       end;
     finally
      {Reset Command}
      SDHCI.Command:=nil;
     end;
     
    finally
     {Check Status}
     if Command.Status = MMC_STATUS_SUCCESS then
      begin
       {Check for Stop}
       if Command.Command = MMC_CMD_STOP_TRANSMISSION then
        begin
         {Record Stop Time}
         PBCMSDHOSTHost(SDHCI).StopTime:=ClockGetTotal;
        end;
       
       {The SDHOST block doesn't report any errors for a disconnected interface. All cards and SDIO devices should report some supported
        voltage range, so a zero response to MMC_CMD_SEND_OP_COND, SDIO_CMD_SEND_OP_COND or SD_CMD_APP_SEND_OP_COND can be treated as an error}
       if (Command.Command = MMC_CMD_SEND_OP_COND) or (Command.Command = SDIO_CMD_SEND_OP_COND) or (Command.Command = SD_CMD_APP_SEND_OP_COND) then
        begin
         if Command.Response[0] = 0 then
          begin
           Command.Status:=MMC_STATUS_TIMEOUT;
          end;
        end;
      end
     else
      begin
       {Drop the overclock after any data corruption, or after any error while overclocked.
        Ignore errors for status commands, as they are likely when a card is ejected}
       if (PBCMSDHOSTHost(SDHCI).Overclock > 0) and (Command.Command <> MMC_CMD_SEND_STATUS) then
        begin
         if MMC_LOG_ENABLED then MMCLogWarn(nil,'BCMSDHOST: Reducing overclock due to error');

         Dec(PBCMSDHOSTHost(SDHCI).Overclock50);
         PBCMSDHOSTHost(SDHCI).ResetClock:=True;
        end;
      end;
    end;
       
   finally
    {Release the Lock}
    MutexUnlock(MMC.Lock);
   end;
  end;

 {$IF DEFINED(BCMSDHOST_DEBUG) or DEFINED(MMC_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'BCMSDHOST: MMC Send Command completed: ' + MMCStatusToString(Command.Status));
 {$ENDIF}
 if Command.Status = MMC_STATUS_SUCCESS then Result:=MMC_STATUS_SUCCESS;

 //bcm2835_sdhost_request
 //bcm2835_sdhost_send_command
 //bcm2835_sdhost_start_dma
 //bcm2835_sdhost_prepare_data
 //bcm2835_sdhost_set_transfer_irqs
 //bcm2835_sdhost_tasklet_finish
end;

{==============================================================================}

function BCMSDHOSTSetIOS(MMC:PMMCDevice):LongWord;
{Implementation of MMCDeviceSetIOS API for BCMSDHOST SDHCI}
{Note: Not intended to be called directly by applications, use MMCDeviceSetIOS instead}
var
 SDHCI:PSDHCIHost;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IF DEFINED(BCMSDHOST_DEBUG) or DEFINED(MMC_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'BCMSDHOST: MMC Set IOS');
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'BCMSDHOST: MMC Set IOS (Clock=' + IntToStr(MMC.Clock) + ' BusWidth=' + MMCBusWidthToString(MMC.BusWidth) + ')');
 {$ENDIF}
 
 {Get SDHCI}
 SDHCI:=PSDHCIHost(MMC.Device.DeviceData);
 if SDHCI = nil then Exit;

 {Acquire the Lock}
 if BCMSDHOSTLock(SDHCI) <> ERROR_SUCCESS then Exit;
 try
  {Set bus width}
  PBCMSDHOSTHost(SDHCI).HostConfiguration:=PBCMSDHOSTHost(SDHCI).HostConfiguration and not(BCMSDHOST_SDHCFG_WIDE_EXT_BUS);
  if MMC.BusWidth = MMC_BUS_WIDTH_4 then 
   begin
    PBCMSDHOSTHost(SDHCI).HostConfiguration:=PBCMSDHOSTHost(SDHCI).HostConfiguration or BCMSDHOST_SDHCFG_WIDE_EXT_BUS;
   end;
   
  PBCMSDHOSTHost(SDHCI).HostConfiguration:=PBCMSDHOSTHost(SDHCI).HostConfiguration or BCMSDHOST_SDHCFG_WIDE_INT_BUS; 
  
  {Disable clever clock switching, to cope with fast core clocks}
  PBCMSDHOSTHost(SDHCI).HostConfiguration:=PBCMSDHOSTHost(SDHCI).HostConfiguration or BCMSDHOST_SDHCFG_SLOW_CARD;
  
  {Memory Barrier}
  DataMemoryBarrier; {Before the First Write}

  {Update Host Configuration}
  BCMSDHOSTWrite(SDHCI,BCMSDHOST_SDHCFG,PBCMSDHOSTHost(SDHCI).HostConfiguration);

  {Update Bus Width}
  SDHCI.BusWidth:=MMC.BusWidth;
  
  Result:=MMC_STATUS_SUCCESS;
 finally
  {Release the Lock}
  BCMSDHOSTUnlock(SDHCI);
 end; 
 
 {Update Clock}
 if (MMC.Clock = 0) or (MMC.Clock <> SDHCI.Clock) then
  begin
   Result:=BCMSDHOSTSetClock(SDHCI,MMC.Clock);
  end;

 //bcm2835_sdhost_set_ios
end;

{==============================================================================}

function BCMSDHOSTGetCardDetect(MMC:PMMCDevice):LongWord;
{Implementation of MMCDeviceGetCardDetect API for BCMSDHOST SDHCI}
{Note: Not intended to be called directly by applications, use MMCDeviceGetCardDetect instead}
var
 SDHCI:PSDHCIHost;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check MMC}
 if MMC = nil then Exit;

 {$IF DEFINED(BCMSDHOST_DEBUG) or DEFINED(MMC_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'BCMSDHOST: Get Card Detect');
 {$ENDIF}

 {Get SDHCI}
 SDHCI:=PSDHCIHost(MMC.Device.DeviceData);
 if SDHCI = nil then Exit;

 {Check MMC State}
 if MMC.MMCState = MMC_STATE_INSERTED then
  begin
   {Get Card Status}
   if MMCDeviceSendCardStatus(MMC) <> MMC_STATUS_SUCCESS then
    begin
     {Update Flags}
     MMC.Device.DeviceFlags:=MMC.Device.DeviceFlags and not(MMC_FLAG_CARD_PRESENT);

     {$IF DEFINED(BCMSDHOST_DEBUG) or DEFINED(MMC_DEBUG)}
     if MMC_LOG_ENABLED then MMCLogDebug(nil,'BCMSDHOST: Get Card Detect (Flags=not MMC_FLAG_CARD_PRESENT)');
     {$ENDIF}
    end;
  end
 else
  begin
   {Check Flags}
   if (MMC.Device.DeviceFlags and MMC_FLAG_CARD_PRESENT) = 0 then
    begin
     {Update Flags}
     MMC.Device.DeviceFlags:=(MMC.Device.DeviceFlags or MMC_FLAG_CARD_PRESENT);

     {$IF DEFINED(BCMSDHOST_DEBUG) or DEFINED(MMC_DEBUG)}
     if MMC_LOG_ENABLED then MMCLogDebug(nil,'BCMSDHOST: Get Card Detect (Flags=MMC_FLAG_CARD_PRESENT)');
     {$ENDIF}
    end;
  end;

 Result:=MMC_STATUS_SUCCESS;
end;

{==============================================================================}

function BCMSDHOSTGetWriteProtect(MMC:PMMCDevice):LongWord;
{Implementation of MMCDeviceGetWriteProtect API for BCMSDHOST SDHCI}
{Note: Not intended to be called directly by applications, use MMCDeviceGetWriteProtect instead}
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;
 
 {Check MMC}
 if MMC = nil then Exit;

 {$IF DEFINED(BCMSDHOST_DEBUG) or DEFINED(MMC_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'BCMSDHOST: MMC Device Get Write Protect');
 {$ENDIF}
 
 Result:=MMC_STATUS_SUCCESS;
end;

{==============================================================================}
{==============================================================================}
{BCMSDHOST SDHCI Functions}
function BCMSDHOSTHostStart(SDHCI:PSDHCIHost):LongWord;
{Implementation of SDHCIHostStart API for BCMSDHOST SDHCI}
{Note: Not intended to be called directly by applications, use SDHCIHostStart instead}
var
 Count:LongWord;
 Status:LongWord;
 MMC:PMMCDevice;
 Clock:LongWord;
 Value1:LongWord;
 Value2:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check SDHCI}
 if SDHCI = nil then Exit;

 {$IF DEFINED(BCMSDHOST_DEBUG) or DEFINED(MMC_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'BCMSDHOST: Host Start');
 {$ENDIF}

 if MMC_LOG_ENABLED then MMCLogInfo(nil,'BCMSDHOST Starting SD host controller (' + SDHCI.Device.DeviceDescription + ')');

 {Check GPIO}
 if (PBCMSDHOSTHost(SDHCI).GPIOFirst <> GPIO_PIN_UNKNOWN) and (PBCMSDHOSTHost(SDHCI).GPIOLast <> GPIO_PIN_UNKNOWN) then
  begin
   {Connect GPIO to BCMSDHOST}
   for Count:=PBCMSDHOSTHost(SDHCI).GPIOFirst to PBCMSDHOSTHost(SDHCI).GPIOLast do
    begin
     GPIOFunctionSelect(Count,PBCMSDHOSTHost(SDHCI).GPIOFunction);

     if Count = PBCMSDHOSTHost(SDHCI).GPIOFirst then
      begin
       GPIOPullSelect(Count,GPIO_PULL_NONE);
      end
     else
      begin
       GPIOPullSelect(Count,GPIO_PULL_UP);
      end;
    end;
  end;

 {Update SDHCI}
 {Driver Properties}
 if PBCMSDHOSTHost(SDHCI).EnableFIQ then
  begin
   SDHCI.Wait:=SemaphoreCreateEx(0,SEMAPHORE_DEFAULT_MAXIMUM,SEMAPHORE_FLAG_IRQFIQ);
  end
 else
  begin
   SDHCI.Wait:=SemaphoreCreateEx(0,SEMAPHORE_DEFAULT_MAXIMUM,SEMAPHORE_FLAG_IRQ);
  end;
 {Configuration Properties}
 SDHCI.PresetVoltages:=MMC_VDD_32_33 or MMC_VDD_33_34;
 SDHCI.PresetCapabilities:=MMC_CAP_4_BIT_DATA;

 if MMC_LOG_ENABLED then MMCLogInfo(nil,'BCMSDHOST Maximum clock rate = ' + IntToStr(SDHCI.ClockMaximum));

 {Reset SD Host Clock}
 Clock:=0;
 Value1:=$FFFFFFFF;
 Value2:=$FFFFFFFF;
 BCMSDHOSTSetSDHostClock(Clock,Value1,Value2);

 if MMC_LOG_ENABLED then MMCLogInfo(nil,'BCMSDHOST Set SD Host Clock (Clock=' + IntToStr(Clock) + ' Value1=' + IntToStr(Value1) + ' Value2=' + IntToStr(Value2) + ')');

 {Update BCMSDHOST}
 PBCMSDHOSTHost(SDHCI).FirmwareSetsClockDivider:=(Value1 <> $FFFFFFFF);

 {$IF DEFINED(BCMSDHOST_DEBUG) or DEFINED(MMC_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'BCMSDHOST: Firmware sets clock divider = ' + BooleanToString(PBCMSDHOSTHost(SDHCI).FirmwareSetsClockDivider));
 {$ENDIF}

 {Create the Lock}
 PBCMSDHOSTHost(SDHCI).Lock:=SpinCreate;

 {SDHCI Host Start operations (Non standard host)}
 {Check Clock Maximum}
 if SDHCI.ClockMaximum <> 0 then
  begin
   SDHCI.MaximumFrequency:=SDHCI.ClockMaximum;
  end;
 if SDHCI.MaximumFrequency = 0 then
  begin
   if MMC_LOG_ENABLED then MMCLogError(nil,'BCMSDHOST Host does not specify a maximum clock frequency');

   SDHCI.HostStop(SDHCI);
   Exit;
  end;
 {$IF DEFINED(BCMSDHOST_DEBUG) or DEFINED(MMC_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'BCMSDHOST: Host maximum frequency = ' + IntToStr(SDHCI.MaximumFrequency));
 {$ENDIF}

 {Check Clock Minimum}
 if SDHCI.ClockMinimum <> 0 then
  begin
   SDHCI.MinimumFrequency:=SDHCI.ClockMinimum;
  end;
 {$IF DEFINED(BCMSDHOST_DEBUG) or DEFINED(MMC_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'BCMSDHOST: Host minimum frequency = ' + IntToStr(SDHCI.MinimumFrequency));
 {$ENDIF}

 {Determine Voltages}
 SDHCI.Voltages:=0;
 {Check Presets}
 if SDHCI.PresetVoltages <> 0 then
  begin
   SDHCI.Voltages:=SDHCI.Voltages or SDHCI.PresetVoltages;
  end;
 {$IF DEFINED(BCMSDHOST_DEBUG) or DEFINED(MMC_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'BCMSDHOST: Host voltages = ' + IntToHex(SDHCI.Voltages,8));
 {$ENDIF}

 {Determine Capabilities}
 SDHCI.Capabilities:=MMC_CAP_SD_HIGHSPEED or MMC_CAP_MMC_HIGHSPEED or MMC_CAP_NEEDS_POLL or MMC_CAP_HW_RESET or ((BCMSDHOST_ALLOW_CMD23_READ or BCMSDHOST_ALLOW_CMD23_WRITE) * MMC_CAP_CMD23);
 {Check Presets}
 if SDHCI.PresetCapabilities <> 0 then
  begin
   SDHCI.Capabilities:=SDHCI.Capabilities or SDHCI.PresetCapabilities;
  end;
 {$IF DEFINED(BCMSDHOST_DEBUG) or DEFINED(MMC_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'BCMSDHOST: Host capabilities = ' + IntToHex(SDHCI.Capabilities,8));
 {$ENDIF}

 {Determine Maximum Blocks}
 SDHCI.MaximumBlockCount:=MMC_MAX_BLOCK_COUNT;
 {$IF DEFINED(BCMSDHOST_DEBUG) or DEFINED(MMC_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'BCMSDHOST: Host maximum blocks = ' + IntToStr(SDHCI.MaximumBlockCount));
 {$ENDIF}

 {Initialize Host}
 if BCMSDHOSTInitInternal(SDHCI,False) <> MMC_STATUS_SUCCESS then
  begin
   if MMC_LOG_ENABLED then MMCLogError(nil,'BCMSDHOST Failed to initialize host');

   Result:=ERROR_OPERATION_FAILED;
   SDHCI.HostStop(SDHCI);
   Exit;
  end;

 {Request the IRQ/FIQ}
 if PBCMSDHOSTHost(SDHCI).EnableFIQ then
  begin
   RegisterInterrupt(PBCMSDHOSTHost(SDHCI).IRQ,CPUIDToMask(FIQ_ROUTING),INTERRUPT_PRIORITY_DEFAULT,INTERRUPT_FLAG_SHARED or INTERRUPT_FLAG_FIQ,TSharedInterruptHandler(BCMSDHOSTSharedInterruptHandler),SDHCI);
  end
 else
  begin
   RegisterInterrupt(PBCMSDHOSTHost(SDHCI).IRQ,CPUIDToMask(IRQ_ROUTING),INTERRUPT_PRIORITY_DEFAULT,INTERRUPT_FLAG_SHARED,TSharedInterruptHandler(BCMSDHOSTSharedInterruptHandler),SDHCI);
  end;

 {Create MMC}
 MMC:=MMCDeviceCreate;
 if MMC = nil then
  begin
   if MMC_LOG_ENABLED then MMCLogError(nil,'BCMSDHOST Failed to create new MMC device');

   SDHCI.HostStop(SDHCI);
   Exit;
  end;

 {Update MMC}
 {Device}
 MMC.Device.DeviceBus:=DEVICE_BUS_MMC;
 MMC.Device.DeviceType:=MMC_TYPE_MMC;
 MMC.Device.DeviceFlags:=MMC_FLAG_NONE;
 MMC.Device.DeviceData:=SDHCI;
 MMC.Device.DeviceDescription:=MMC_DEVICE_DESCRIPTION;
 {MMC}
 MMC.MMCState:=MMC_STATE_EJECTED;
 MMC.DeviceInitialize:=SDHCI.DeviceInitialize;
 MMC.DeviceDeinitialize:=SDHCI.DeviceDeinitialize;
 MMC.DeviceGetCardDetect:=SDHCI.DeviceGetCardDetect;
 MMC.DeviceGetWriteProtect:=SDHCI.DeviceGetWriteProtect;
 MMC.DeviceSendCommand:=SDHCI.DeviceSendCommand;
 MMC.DeviceSetIOS:=SDHCI.DeviceSetIOS;

 {Create Storage}
 MMC.Storage:=StorageDeviceCreate;
 if MMC.Storage = nil then
  begin
   if MMC_LOG_ENABLED then MMCLogError(nil,'BCMSDHOST Failed to create new Storage device');

   MMCDeviceDestroy(MMC);
   SDHCI.HostStop(SDHCI);
   Exit;
  end;

 {Update Storage}
 {Device}
 MMC.Storage.Device.DeviceBus:=DEVICE_BUS_MMC;
 MMC.Storage.Device.DeviceType:=STORAGE_TYPE_REMOVABLE;
 MMC.Storage.Device.DeviceFlags:=STORAGE_FLAG_REMOVABLE or STORAGE_FLAG_NOT_READY or STORAGE_FLAG_NO_MEDIA;
 MMC.Storage.Device.DeviceData:=MMC;
 MMC.Storage.Device.DeviceDescription:=MMC_STORAGE_DESCRIPTION;
 {Storage}
 MMC.Storage.StorageState:=STORAGE_STATE_EJECTED;
 MMC.Storage.DeviceRead:=MMCStorageDeviceRead;
 MMC.Storage.DeviceWrite:=MMCStorageDeviceWrite;
 MMC.Storage.DeviceErase:=MMCStorageDeviceErase;
 MMC.Storage.DeviceControl:=MMCStorageDeviceControl;

 {Initialize MMC}
 Status:=MMCDeviceInitialize(MMC);
 if (Status <> MMC_STATUS_SUCCESS) and (Status <> MMC_STATUS_NO_MEDIA) then
  begin
   if MMC_LOG_ENABLED then MMCLogError(nil,'BCMSDHOST Failed to initialize new MMC device');

   StorageDeviceDestroy(MMC.Storage);
   MMCDeviceDestroy(MMC);
   SDHCI.HostStop(SDHCI);
   Exit;
  end;

 {Check MMC Type}
 if MMC.Device.DeviceType = MMC_TYPE_SDIO then
  begin
   {Destroy Storage}
   StorageDeviceDestroy(MMC.Storage);
   MMC.Storage:=nil;
  end;

 {Register MMC}
 if MMCDeviceRegister(MMC) <> MMC_STATUS_SUCCESS then
  begin
   if MMC_LOG_ENABLED then MMCLogError(nil,'BCMSDHOST Failed to register new MMC device');

   StorageDeviceDestroy(MMC.Storage);
   MMCDeviceDestroy(MMC);
   SDHCI.HostStop(SDHCI);
   Exit;
  end;

 {Enable Host}
 SDHCI.SDHCIState:=SDHCI_STATE_ENABLED;

 {Notify Enable}
 NotifierNotify(@SDHCI.Device,DEVICE_NOTIFICATION_ENABLE);

 {Notify Insert MMC}
 if MMC.MMCState = MMC_STATE_INSERTED then
  begin
   NotifierNotify(@MMC.Device,DEVICE_NOTIFICATION_INSERT);
  end;

 {Check Storage}
 if MMC.Storage <> nil then
  begin
   {Set Storage State to Inserted}
   if MMC.MMCState = MMC_STATE_INSERTED then
    begin
     StorageDeviceSetState(MMC.Storage,STORAGE_STATE_INSERTED);
    end;

   {Start Storage Status Checking}
   if StorageDeviceStartStatus(MMC.Storage,MMC_STATUS_TIMER_INTERVAL) <> ERROR_SUCCESS then
    begin
     if MMC_LOG_ENABLED then MMCLogError(nil,'BCMSDHOST Failed start status for new MMC device');
    end;
  end;

 Result:=ERROR_SUCCESS;

 //bcm2835_sdhost_probe
 //bcm2835_sdhost_add_host
end;

{==============================================================================}

function BCMSDHOSTHostStop(SDHCI:PSDHCIHost):LongWord;
{Implementation of SDHCIHostStop API for BCMSDHOST SDHCI}
{Note: Not intended to be called directly by applications, use SDHCIHostStop instead}
var
 MMC:PMMCDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check SDHCI}
 if SDHCI = nil then Exit;

 {$IF DEFINED(BCMSDHOST_DEBUG) or DEFINED(MMC_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'BCMSDHOST: Host Stop');
 {$ENDIF}

 {Get MMC}
 MMC:=MMCDeviceFindByDevice(@SDHCI.Device);
 if MMC <> nil then
  begin
   {Check Storage}
   if MMC.Storage <> nil then
    begin
     {Stop Storage Status Checking}
     StorageDeviceStopStatus(MMC.Storage);

     {Set Storage State to Inserted}
     StorageDeviceSetState(MMC.Storage,STORAGE_STATE_EJECTED);
    end;

   {Eject MMC}
   MMC.MMCState:=MMC_STATE_EJECTED;

   {Notify Eject MMC}
   NotifierNotify(@MMC.Device,DEVICE_NOTIFICATION_EJECT);
  end;

 {Disable Host}
 SDHCI.SDHCIState:=SDHCI_STATE_DISABLED;

 {Notify Disable}
 NotifierNotify(@SDHCI.Device,DEVICE_NOTIFICATION_DISABLE);

 {Power Off}
 BCMSDHOSTSetPowerInternal(SDHCI,False);
 
 {Release the IRQ/FIQ}
 if PBCMSDHOSTHost(SDHCI).EnableFIQ then
  begin
   DeregisterInterrupt(PBCMSDHOSTHost(SDHCI).IRQ,CPUIDToMask(FIQ_ROUTING),INTERRUPT_PRIORITY_DEFAULT,INTERRUPT_FLAG_SHARED or INTERRUPT_FLAG_FIQ,TSharedInterruptHandler(BCMSDHOSTSharedInterruptHandler),SDHCI);
  end
 else
  begin
   DeregisterInterrupt(PBCMSDHOSTHost(SDHCI).IRQ,CPUIDToMask(IRQ_ROUTING),INTERRUPT_PRIORITY_DEFAULT,INTERRUPT_FLAG_SHARED,TSharedInterruptHandler(BCMSDHOSTSharedInterruptHandler),SDHCI);
  end;

 {Check MMC}
 if MMC <> nil then
  begin
   {Dergister MMC}
   MMCDeviceDeregister(MMC);

   {Check Storage}
   if MMC.Storage <> nil then
    begin
     {Destroy Storage}
     StorageDeviceDestroy(MMC.Storage);
    end;

   {Destroy MMC}
   MMCDeviceDestroy(MMC);
  end;

 {Destroy the Lock}
 if PBCMSDHOSTHost(SDHCI).Lock <> INVALID_HANDLE_VALUE then
  begin
   SpinDestroy(PBCMSDHOSTHost(SDHCI).Lock);

   PBCMSDHOSTHost(SDHCI).Lock:=INVALID_HANDLE_VALUE
  end;

 {Destroy the Semaphore}
 if SDHCI.Wait <> INVALID_HANDLE_VALUE then
  begin
   SemaphoreDestroy(SDHCI.Wait);

   SDHCI.Wait:=INVALID_HANDLE_VALUE;
  end;

 if MMC_LOG_ENABLED then MMCLogInfo(nil,'BCMSDHOST Stopped SD host controller (' + SDHCI.Device.DeviceDescription + ')');

 Result:=ERROR_SUCCESS;

 //bcm2835_sdhost_remove
end;

{==============================================================================}

function BCMSDHOSTReset(SDHCI:PSDHCIHost;Mask:Byte):LongWord;
{Implementation of SDHCIReset API for BCMSDHOST SDHCI}
{Note: Not intended to be called directly by applications, use SDHCIReset instead}
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check SDHCI}
 if SDHCI = nil then Exit;

 {$IF DEFINED(BCMSDHOST_DEBUG) or DEFINED(MMC_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'BCMSDHOST: Reset (Mask=' + IntToHex(Mask,2) + ')');
 {$ENDIF}

 {No function, return success}
 Result:=MMC_STATUS_SUCCESS;
end;

{==============================================================================}

function BCMSDHOSTHardwareReset(SDHCI:PSDHCIHost):LongWord;
{Implementation of SDHCIHardwareReset API for BCMSDHOST SDHCI}
{Note: Not intended to be called directly by applications, use SDHCIHardwareReset instead}
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check SDHCI}
 if SDHCI = nil then Exit;

 {$IF DEFINED(BCMSDHOST_DEBUG) or DEFINED(MMC_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'BCMSDHOST: Hardware Reset');
 {$ENDIF}

 {Acquire the Lock}
 if BCMSDHOSTLock(SDHCI) <> ERROR_SUCCESS then Exit;
 try
  Result:=BCMSDHOSTResetInternal(SDHCI);
 finally
  {Release the Lock}
  BCMSDHOSTUnlock(SDHCI);
 end;

 //bcm2835_sdhost_reset
end;

{==============================================================================}

function BCMSDHOSTSetPower(SDHCI:PSDHCIHost;Power:Word):LongWord;
{Implementation of SDHCISetPower API for BCMSDHOST SDHCI}
{Note: Not intended to be called directly by applications, use SDHCISetPower instead}
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check SDHCI}
 if SDHCI = nil then Exit;

 {$IF DEFINED(BCMSDHOST_DEBUG) or DEFINED(MMC_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'BCMSDHOST: Set Power (Power=' + IntToStr(Power) + ')');
 {$ENDIF}

 {No function, return success}
 Result:=MMC_STATUS_SUCCESS;
end;

{==============================================================================}

function BCMSDHOSTSetClock(SDHCI:PSDHCIHost;Clock:LongWord):LongWord;
{Implementation of SDHCISetClock API for BCMSDHOST SDHCI}
{Note: Not intended to be called directly by applications, use SDHCISetClock instead}
var
 Value1:LongWord;
 Value2:LongWord;
 Divider:LongWord;
 InputClock:LongWord;
 OperationsPerWord:LongWord;
begin
 {}
 Result:=MMC_STATUS_INVALID_PARAMETER;

 {Check SDHCI}
 if SDHCI = nil then Exit;

 {$IF DEFINED(BCMSDHOST_DEBUG) or DEFINED(MMC_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'BCMSDHOST: Set Clock (Clock=' + IntToStr(Clock) + ')');
 {$ENDIF}

 Divider:=0;
 InputClock:=Clock;

 {Check Overclock}
 if (PBCMSDHOSTHost(SDHCI).Overclock50 > 0) and (Clock = (50 * FREQUENCY_MHZ)) then
  begin
   Clock:=PBCMSDHOSTHost(SDHCI).Overclock50 * FREQUENCY_MHZ + (FREQUENCY_MHZ - 1);
  end;

 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}

 {The SDCDIV register has 11 bits, and holds (Divider - 2).
  But in data mode the max is 50MHz wihout a minimum, and only the
  bottom 3 bits are used. Since the switch over is automatic (unless
  we have marked the card as slow...), chosen values have to make
  sense in both modes.
  Ident mode must be 100-400KHz, so can range check the requested
  clock. CMD15 must be used to return to data mode, so this can be
  monitored.

  clock 250MHz -> 0->125MHz, 1->83.3MHz, 2->62.5MHz, 3->50.0MHz
                  4->41.7MHz, 5->35.7MHz, 6->31.3MHz, 7->27.8MHz

        623->400KHz/27.8MHz
  reset value (507)->491159/50MHz

  BUT, the 3-bit clock divisor in data mode is too small if the
  core clock is higher than 250MHz, so instead use the SLOW_CARD
  configuration bit to force the use of the ident clock divisor
  at all times
 }

 if PBCMSDHOSTHost(SDHCI).FirmwareSetsClockDivider then
  begin
   {Set SD Host Clock}
   Value1:=0;
   Value2:=0;
   BCMSDHOSTSetSDHostClock(Clock,Value1,Value2);

   if MMC_LOG_ENABLED then MMCLogInfo(nil,'BCMSDHOST Set SD Host Clock (Clock=' + IntToStr(Clock) + ' Value1=' + IntToStr(Value1) + ' Value2=' + IntToStr(Value2) + ')');
   
   {Update Clock}
   Clock:=Max(Value1,Value2);

   {Acquire the Lock}
   if BCMSDHOSTLock(SDHCI) <> ERROR_SUCCESS then Exit;
  end
 else
  begin
   {Acquire the Lock}
   if BCMSDHOSTLock(SDHCI) <> ERROR_SUCCESS then Exit;

   if Clock < 100000 then
    begin
     {Can't stop the clock, but make it as slow as possible}
     {Update Divider}
     PBCMSDHOSTHost(SDHCI).ClockDivider:=BCMSDHOST_SDCDIV_MAX_CDIV;
     BCMSDHOSTWrite(SDHCI,BCMSDHOST_SDCDIV,PBCMSDHOSTHost(SDHCI).ClockDivider);
     
     {Release the Lock}
     BCMSDHOSTUnlock(SDHCI);
     Exit;
    end;
    
   {Calculate the Divider}
   Divider:=SDHCI.ClockMaximum div Clock;
   
   if Divider < 2 then Divider:=2;
   if (SDHCI.ClockMaximum div Divider) > Clock then Inc(Divider);
   
   Dec(Divider,2); {SDCDIV is Divider - 2}
   
   if Divider > BCMSDHOST_SDCDIV_MAX_CDIV then Divider:=BCMSDHOST_SDCDIV_MAX_CDIV;
   
   {Update Clock}
   Clock:=SDHCI.ClockMaximum div (Divider + 2);
   
   {Update Divider}
   PBCMSDHOSTHost(SDHCI).ClockDivider:=Divider;
   BCMSDHOSTWrite(SDHCI,BCMSDHOST_SDCDIV,PBCMSDHOSTHost(SDHCI).ClockDivider);
  end;
 try
  {Calibrate some delays}
  if (SDHCI.Capabilities and MMC_CAP_4_BIT_DATA) <> 0 then OperationsPerWord:=8 else OperationsPerWord:=32;
  PBCMSDHOSTHost(SDHCI).NanosecondsPerFifoWord:=(1000000000 div Clock) * OperationsPerWord;
  
  {$IF DEFINED(BCMSDHOST_DEBUG) or DEFINED(MMC_DEBUG)}
  if MMC_LOG_ENABLED then MMCLogDebug(nil,'BCMSDHOST: Nanoseconds per FIFO word = ' + IntToStr(PBCMSDHOSTHost(SDHCI).NanosecondsPerFifoWord));
  {$ENDIF}
  
  if InputClock = (50 * FREQUENCY_MHZ) then
   begin
    if Clock > InputClock then
     begin
      {Save the closest value, to make it easier to reduce in the event of error}
      PBCMSDHOSTHost(SDHCI).Overclock50:=(Clock div FREQUENCY_MHZ);
      
      if Clock <> PBCMSDHOSTHost(SDHCI).Overclock then 
       begin
        {Update Overclock}
        PBCMSDHOSTHost(SDHCI).Overclock:=Clock;
        
        if MMC_LOG_ENABLED then MMCLogInfo(nil,'BCMSDHOST Overclocking to ' + IntToStr(Clock) + 'Hz');
       end;
     end
    else if PBCMSDHOSTHost(SDHCI).Overclock > 0 then 
     begin
      {Cancel Overclock}
      PBCMSDHOSTHost(SDHCI).Overclock:=0;
      
      if Clock = (50 * FREQUENCY_MHZ) then
       begin
        if MMC_LOG_ENABLED then MMCLogWarn(nil,'BCMSDHOST Cancelling Overclock');
       end;
     end
   end
  else if InputClock = 0 then
   begin
    {Reset the preferred overclock when the clock is stopped}
    {This always happens during initialisation}
    PBCMSDHOSTHost(SDHCI).Overclock50:=PBCMSDHOSTHost(SDHCI).UserOverclock50;
    PBCMSDHOSTHost(SDHCI).Overclock:=0;
   end;   
 
  {$IF DEFINED(BCMSDHOST_DEBUG) or DEFINED(MMC_DEBUG)}
  if MMC_LOG_ENABLED then MMCLogDebug(nil,'BCMSDHOST: Overclock50 = ' + IntToStr(PBCMSDHOSTHost(SDHCI).Overclock50));
  if MMC_LOG_ENABLED then MMCLogDebug(nil,'BCMSDHOST: Overclock = ' + IntToStr(PBCMSDHOSTHost(SDHCI).Overclock));
  {$ENDIF}
 
  {Set the timeout to 500ms}
  BCMSDHOSTWrite(SDHCI,BCMSDHOST_SDTOUT,Clock div 2);
  
  {Update Clock}
  SDHCI.Clock:=InputClock;
  PBCMSDHOSTHost(SDHCI).ResetClock:=False;
  
  Result:=MMC_STATUS_SUCCESS;
 finally
  {Release the Lock}
  BCMSDHOSTUnlock(SDHCI);
 end;

 //bcm2835_sdhost_set_clock
end;

{==============================================================================}

procedure BCMSDHOSTCommandWaitWorker(SDHCI:PSDHCIHost);
{Worker thread task to wait for BCMSDHOST command completion}
{Note: Not intended to be called directly by applications}
begin
 {}
 {Check SDHCI}
 if SDHCI = nil then Exit;

 {$IF DEFINED(BCMSDHOST_DEBUG) or DEFINED(MMC_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'BCMSDHOST: Command Wait Worker');
 {$ENDIF}
 
 {Check Command}
 if SDHCI.Command = nil then Exit;
 
 {Acquire the Lock}
 if BCMSDHOSTLock(SDHCI) <> ERROR_SUCCESS then Exit;
 
 {Finish Command}
 BCMSDHOSTFinishCommand(SDHCI,True);
 
 {Release the Lock}
 BCMSDHOSTUnlock(SDHCI);
 
 //bcm2835_sdhost_cmd_wait_work
end;

{==============================================================================}

procedure BCMSDHOSTDMARequestCompleted(Request:PDMARequest);
{DMA Request completion callback for the BCMSDHOST host controller}
{Note: Not intended to be called directly by applications}
var
 Debug:LongWord;
 Offset:LongWord;
 SDHCI:PSDHCIHost;
begin
 {}
 {Check Request}
 if Request = nil then Exit;

 {Get SDHCI}
 SDHCI:=PSDHCIHost(Request.DriverData);
 if SDHCI = nil then Exit;

 {$IF DEFINED(BCMSDHOST_DEBUG) or DEFINED(MMC_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'BCMSDHOST: DMA Request Completed');
 {$ENDIF}

 {Check Command}
 if SDHCI.Command = nil then Exit;

 {Check DMA Buffer}
 if (PBCMSDHOSTHost(SDHCI).DMABuffer <> nil) and ((SDHCI.Command.Data.Flags and MMC_DATA_READ) <> 0) then
  begin
   if MMC_LOG_ENABLED then MMCLogWarn(nil,'BCMSDHOST: Copying data from DMA bounce buffer to read data destination');
   
   {Copy Data from DMA Buffer}
   System.Move(PBCMSDHOSTHost(SDHCI).DMABuffer^,SDHCI.Command.Data.Data^,SDHCI.Command.Data.BlockCount * SDHCI.Command.Data.BlockSize);
  end;
 
 {Acquire the Lock}
 if BCMSDHOSTLock(SDHCI) <> ERROR_SUCCESS then Exit;

 {Check FIFO Drain}
 if PBCMSDHOSTHost(SDHCI).DrainWords > 0 then
  begin
   {Get Drain Offset}
   Offset:=PBCMSDHOSTHost(SDHCI).DrainOffset;
   
   {Read Drain Words}
   while PBCMSDHOSTHost(SDHCI).DrainWords > 0 do
    begin
     {Read Debug Mode Register} 
     Debug:=BCMSDHOSTRead(SDHCI,BCMSDHOST_SDEDM);
     if ((Debug shr 4) and $1f) <> 0 then
      begin
       {Read Word}
       PLongWord(PtrUInt(SDHCI.Command.Data.Data) + PtrUInt(Offset))^:=BCMSDHOSTRead(SDHCI,BCMSDHOST_SDDATA);
       
       Inc(Offset,4);
      end;
    
     Dec(PBCMSDHOSTHost(SDHCI).DrainWords);
    end;  
  end; 

 {Finish Data}
 BCMSDHOSTFinishData(SDHCI);

 {Release the Lock}
 BCMSDHOSTUnlock(SDHCI);

 //bcm2835_sdhost_dma_complete
end;

{==============================================================================}

function BCMSDHOSTSharedInterruptHandler(Number,CPUID,Flags:LongWord;SDHCI:PSDHCIHost):LongWord;
{Interrupt handler for the BCMSDHOST host controller}
{Note: Not intended to be called directly by applications}
var
 InterruptMask:LongWord;
begin
 {}
 Result:=INTERRUPT_RETURN_NONE;

 {Check SDHCI}
 if SDHCI = nil then Exit;

 {Update Statistics}
 Inc(SDHCI.InterruptCount); 

 {$IF (DEFINED(BCMSDHOST_DEBUG) or DEFINED(MMC_DEBUG)) and DEFINED(INTERRUPT_DEBUG)}
 if MMC_LOG_ENABLED then MMCLogDebug(nil,'BCMSDHOST: Interrupt Handler');
 {$ENDIF}

 {Acquire the Lock}
 if BCMSDHOSTLock(SDHCI) <> ERROR_SUCCESS then Exit;

 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}

 {Get Interrupt Status}
 InterruptMask:=BCMSDHOSTRead(SDHCI,BCMSDHOST_SDHSTS);
 
 {Clear Interrupt Status}
 BCMSDHOSTWrite(SDHCI,BCMSDHOST_SDHSTS,BCMSDHOST_SDHSTS_BUSY_IRPT or BCMSDHOST_SDHSTS_BLOCK_IRPT or BCMSDHOST_SDHSTS_SDIO_IRPT or BCMSDHOST_SDHSTS_DATA_FLAG);
 
 {Check Block Interrupt}
 if (InterruptMask and BCMSDHOST_SDHSTS_BLOCK_IRPT) <> 0 then
  begin
   BCMSDHOSTBlockInterrupt(SDHCI,InterruptMask);
   
   Result:=INTERRUPT_RETURN_HANDLED;
  end;

 {Check Busy Interrupt}
 if (InterruptMask and BCMSDHOST_SDHSTS_BUSY_IRPT) <> 0 then
  begin
   BCMSDHOSTBusyInterrupt(SDHCI,InterruptMask);
   
   Result:=INTERRUPT_RETURN_HANDLED;
  end;
  
 {There is no true data interrupt status bit, so it is necessary to qualify the data flag with the interrupt enable bit}
 if ((InterruptMask and BCMSDHOST_SDHSTS_DATA_FLAG) <> 0) and ((PBCMSDHOSTHost(SDHCI).HostConfiguration and BCMSDHOST_SDHCFG_DATA_IRPT_EN) <> 0) then
  begin
   BCMSDHOSTDataInterrupt(SDHCI,InterruptMask);
   
   Result:=INTERRUPT_RETURN_HANDLED;
  end;
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
 
 {Release the Lock}
 BCMSDHOSTUnlock(SDHCI);

 //bcm2835_sdhost_irq
end;

{==============================================================================}
{==============================================================================}
{BCMSDHOST Helper Functions}
function BCMSDHOSTSetSDHostClock(var Clock,Value1,Value2:LongWord):LongWord;
{Set the SD Host Clock value in the Mailbox property tags channel}
var
 Request:TBCMSDHOSTMailboxTagSetSDHostClock;
begin
 {}
 {Setup Request}
 Request.Clock:=Clock;
 Request.Value1:=Value1;
 Request.Value2:=Value2;

 {Perform Request}
 Result:=MailboxPropertyTag(BCMSDHOST_MBOX_TAG_SET_SDHOST_CLOCK,@Request,SizeOf(Request));
 if Result = ERROR_SUCCESS then
  begin
   {Get Result}
   Clock:=Request.Clock;
   Value1:=Request.Value1;
   Value2:=Request.Value2;
  end;
end;

{==============================================================================}
{==============================================================================}

initialization
 BCMSDHOSTInit;

{==============================================================================}

{finalization}
 {Nothing}

{==============================================================================}
{==============================================================================}

end.



