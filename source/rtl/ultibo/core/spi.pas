{
Ultibo SPI interface unit.

Copyright (C) 2025 - SoftOz Pty Ltd.

Arch
====

 <All>

Boards
======

 <All>

Licence
=======

 LGPLv2.1 with static linking exception (See COPYING.modifiedLGPL.txt)

Credits
=======

 Information for this unit was obtained from:


References
==========

 SPI - https://en.wikipedia.org/wiki/Serial_Peripheral_Interface_Bus

SPI Devices
===========

 SPI (Serial Peripheral Interface) is a synchronous serial bus for communication with peripheral
 components.

 The SPI protocol is not defined by any actual standard but some pseudo standards exist with all
 of the available devices. SPI is a master-slave protocol where the master asserts the chip select
 line (CS) to select the slave device before sending data one byte at a time.

 For every byte written to the bus by the master the selected slave returns a byte as well so for
 every write there is an equivalent read. SPI can also operate in either 4 wire (standard) or 3 wire
 (bidirectional) modes.

 Due to the lack of formal standards and the range of devices that exist various options are provided
 to allow setting clock phase and polarity as well chip select polarity.

 For the purpose of this interface a device is the SPI controller attached to the local system
 and may be either a master or a slave. Since the protocol does not include any form of enumeration
 or identification the interface does not attempt to represent the devices connected to the bus,
 any driver written to communicate with a connected SPI device should know (or allow configuration
 of) the chip select for the for the device and the specific message format required for that device.

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

{$IFNDEF FPC_DOTTEDUNITS}
unit SPI;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  Core.GlobalConfig,
  Core.GlobalConst,
  Core.GlobalTypes,
  Core.Platform,
  Core.Threads,
  Core.Devices,
  System.SysUtils;
{$ELSE FPC_DOTTEDUNITS}
uses
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  Devices,
  SysUtils;
{$ENDIF FPC_DOTTEDUNITS}

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
const
 {SPI specific constants}
 SPI_NAME_PREFIX = 'SPI';            {Name prefix for SPI Devices}
 SPISLAVE_NAME_PREFIX = 'SPISlave';  {Name prefix for SPI Slave Devices}

 {SPI Device Types}
 SPI_TYPE_NONE      = 0;
 SPI_TYPE_MASTER    = 1;
 SPI_TYPE_SLAVE     = 2;

 SPI_TYPE_MAX       = 2;

 {SPI Type Names}
 SPI_TYPE_NAMES:array[SPI_TYPE_NONE..SPI_TYPE_MAX] of String = (
  'SPI_TYPE_NONE',
  'SPI_TYPE_MASTER',
  'SPI_TYPE_SLAVE');

 {SPI Device States}
 SPI_STATE_DISABLED = 0;
 SPI_STATE_ENABLED  = 1;

 SPI_STATE_MAX      = 1;

 {SPI State Names}
 SPI_STATE_NAMES:array[SPI_STATE_DISABLED..SPI_STATE_MAX] of String = (
  'SPI_STATE_DISABLED',
  'SPI_STATE_ENABLED');

 {SPI Device Flags}
 SPI_FLAG_NONE          = $00000000;
 SPI_FLAG_SLAVE         = $00000001; {Device is a slave not a master}
 SPI_FLAG_4WIRE         = $00000002; {Device supports 4 wire operation (CS/MISO/MOSI/SCLK)}
 SPI_FLAG_3WIRE         = $00000004; {Device supports 3 wire operation (CS/MIMO/SCLK)}
 SPI_FLAG_LOSSI         = $00000008; {Device supports LoSSI (Low Speed Serial) mode (CS/SCL/SDA)}
 SPI_FLAG_CPOL          = $00000010; {Device supports Clock Polarity setting}
 SPI_FLAG_CPHA          = $00000020; {Device supports Clock Phase setting}
 SPI_FLAG_CSPOL         = $00000040; {Device supports Chip Select Polarity setting}
 SPI_FLAG_NO_CS         = $00000080; {Device supports Chip Select None (CS handled externally)}
 SPI_FLAG_DMA           = $00000100; {Device supports DMA transfers}

 {SPI Transfer Flags}
 SPI_TRANSFER_NONE  = $00000000;
 SPI_TRANSFER_DMA   = $00000001; {Use DMA for transfer (Write/Read) (If supported) (Note: Buffers must be DMA compatible)}
 SPI_TRANSFER_PIO   = $00000002; {Use PIO (Polling) for transfer (Write/Read)}
 SPI_TRANSFER_DELAY = $00000004; {Add a delay after each byte written (Write/Read) (Note: Only available with PIO transfer unless provided directly by hardware)}

 {SPI logging}
 SPI_LOG_LEVEL_DEBUG     = LOG_LEVEL_DEBUG;  {SPI debugging messages}
 SPI_LOG_LEVEL_INFO      = LOG_LEVEL_INFO;   {SPI informational messages, such as a device being attached or detached}
 SPI_LOG_LEVEL_WARN      = LOG_LEVEL_WARN;   {SPI warning messages}
 SPI_LOG_LEVEL_ERROR     = LOG_LEVEL_ERROR;  {SPI error messages}
 SPI_LOG_LEVEL_NONE      = LOG_LEVEL_NONE;   {No SPI messages}

var
 SPI_DEFAULT_LOG_LEVEL:LongWord = SPI_LOG_LEVEL_DEBUG; {Minimum level for SPI messages.  Only messages with level greater than or equal to this will be printed}

var
 {SPI logging}
 SPI_LOG_ENABLED:Boolean;

{==============================================================================}
type
 {SPI specific types}

 {SPI Properties}
 PSPIProperties = ^TSPIProperties;
 TSPIProperties = record
  Flags:LongWord;          {Device flags (eg SPI_FLAG_SLAVE)}
  MaxSize:LongWord;        {Maximum supported data transfer size}
  MinClock:LongWord;       {Minimum supported clock rate}
  MaxClock:LongWord;       {Maximum supported clock rate}
  SelectCount:LongWord;    {Number of chip selects supported}
  Mode:LongWord;           {Current mode (eg SPI_MODE_4WIRE)}
  ClockRate:LongWord;      {Current clock rate}
  ClockPhase:LongWord;     {Current clock phase (CPHA) (eg SPI_CLOCK_PHASE_LOW)}
  ClockPolarity:LongWord;  {Current clock polarity (CPOL) (eg SPI_CLOCK_POLARITY_LOW)}
  SelectPolarity:LongWord; {Default chip select polarity (eg SPI_CS_POLARITY_LOW)}
  ByteDelay:LongWord;      {Delay between bytes written (Microseconds)}
 end;

 {SPI Chip Select}
 PSPIChipSelect = ^TSPIChipSelect;
 TSPIChipSelect = record
  Pin:LongWord;            {The GPIO pin for this chip select (eg GPIO_PIN_46)(GPIO_PIN_UNKNOWN for internal)}
  Mode:LongWord;           {The mode for this chip select (eg SPI_MODE_0)}
  Divider:LongWord;        {The clock divider for this chip select (Used internally by drivers)}
  ClockRate:LongWord;      {The clock rate for this chip select}
  ClockPhase:LongWord;     {The clock phase (CPHA) for this chip select (eg SPI_CLOCK_PHASE_LOW)}
  ClockPolarity:LongWord;  {The clock polarity (CPOL) for this chip select (eg SPI_CLOCK_POLARITY_LOW)}
  SelectPolarity:LongWord; {The chip select polarity for this chip select (eg SPI_CS_POLARITY_LOW)}
  ByteDelay:LongWord;      {Delay between bytes written for this chip select (Microseconds)}
 end;

 {SPI Device}
 PSPIDevice = ^TSPIDevice;

 {SPI Enumeration Callback}
 TSPIEnumerate = function(SPI:PSPIDevice;Data:Pointer):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 {SPI Notification Callback}
 TSPINotification = function(Device:PDevice;Data:Pointer;Notification:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}

 {SPI Device Methods}
 TSPIDeviceStart = function(SPI:PSPIDevice;Mode,ClockRate,ClockPhase,ClockPolarity:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TSPIDeviceStop = function(SPI:PSPIDevice):LongWord;{$IFDEF i386} stdcall;{$ENDIF}

 TSPIDeviceRead = function(SPI:PSPIDevice;ChipSelect:Word;Dest:Pointer;Size,Flags:LongWord;var Count:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TSPIDeviceWrite = function(SPI:PSPIDevice;ChipSelect:Word;Source:Pointer;Size,Flags:LongWord;var Count:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TSPIDeviceWriteRead = function(SPI:PSPIDevice;ChipSelect:Word;Source,Dest:Pointer;Size,Flags:LongWord;var Count:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}

 TSPIDeviceGetMode = function(SPI:PSPIDevice):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TSPIDeviceSetMode = function(SPI:PSPIDevice;Mode:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}

 TSPIDeviceGetClockRate = function(SPI:PSPIDevice;ChipSelect:Word):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TSPIDeviceSetClockRate = function(SPI:PSPIDevice;ChipSelect:Word;ClockRate:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}

 TSPIDeviceGetClockPhase = function(SPI:PSPIDevice):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TSPIDeviceSetClockPhase = function(SPI:PSPIDevice;ClockPhase:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}

 TSPIDeviceGetClockPolarity = function(SPI:PSPIDevice):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TSPIDeviceSetClockPolarity = function(SPI:PSPIDevice;ClockPolarity:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}

 TSPIDeviceGetSelectPolarity = function(SPI:PSPIDevice;ChipSelect:Word):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TSPIDeviceSetSelectPolarity = function(SPI:PSPIDevice;ChipSelect:Word;SelectPolarity:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}

 TSPIDeviceGetByteDelay = function(SPI:PSPIDevice):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TSPIDeviceSetByteDelay = function(SPI:PSPIDevice;Delay:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}

 TSPIDeviceGetProperties = function(SPI:PSPIDevice;Properties:PSPIProperties):LongWord;{$IFDEF i386} stdcall;{$ENDIF}

 TSPIDevice = record
  {Device Properties}
  Device:TDevice;                                      {The Device entry for this SPI}
  {SPI Properties}
  SPIId:LongWord;                                      {Unique Id of this SPI in the SPI table}
  SPIState:LongWord;                                   {SPI state (eg SPI_STATE_ENABLED)}
  SPIMode:LongWord;                                    {SPI mode (eg SPI_MODE_4WIRE)}
  DeviceStart:TSPIDeviceStart;                         {A Device specific DeviceStart method implementing the standard SPI device interface (Mandatory)}
  DeviceStop:TSPIDeviceStop;                           {A Device specific DeviceStop method implementing the standard SPI device interface (Mandatory)}
  DeviceRead:TSPIDeviceRead;                           {A Device specific DeviceRead method implementing the standard SPI device interface (Or nil if the default method is suitable)}
  DeviceWrite:TSPIDeviceWrite;                         {A Device specific DeviceWrite method implementing the standard SPI device interface (Or nil if the default method is suitable)}
  DeviceWriteRead:TSPIDeviceWriteRead;                 {A Device specific DeviceWriteRead method implementing the standard SPI device interface (Mandatory)}
  DeviceGetMode:TSPIDeviceGetMode;                     {A Device specific DeviceGetMode method implementing the standard SPI device interface (Or nil if the default method is suitable)}
  DeviceSetMode:TSPIDeviceSetMode;                     {A Device specific DeviceSetMode method implementing the standard SPI device interface (Or nil if the default method is suitable)}
  DeviceGetClockRate:TSPIDeviceGetClockRate;           {A Device specific DeviceGetClockRate method implementing the standard SPI device interface (Or nil if the default method is suitable)}
  DeviceSetClockRate:TSPIDeviceSetClockRate;           {A Device specific DeviceSetClockRate method implementing the standard SPI device interface (Or nil if the default method is suitable)}
  DeviceGetClockPhase:TSPIDeviceGetClockPhase;         {A Device specific DeviceGetClockPhase method implementing the standard SPI device interface (Or nil if the default method is suitable)}
  DeviceSetClockPhase:TSPIDeviceSetClockPhase;         {A Device specific DeviceSetClockPhase method implementing the standard SPI device interface (Or nil if the default method is suitable)}
  DeviceGetClockPolarity:TSPIDeviceGetClockPolarity;   {A Device specific DeviceGetClockPolarity method implementing the standard SPI device interface (Or nil if the default method is suitable)}
  DeviceSetClockPolarity:TSPIDeviceSetClockPolarity;   {A Device specific DeviceSetClockPolarity method implementing the standard SPI device interface (Or nil if the default method is suitable)}
  DeviceGetSelectPolarity:TSPIDeviceGetSelectPolarity; {A Device specific DeviceGetSelectPolarity method implementing the standard SPI device interface (Or nil if the default method is suitable)}
  DeviceSetSelectPolarity:TSPIDeviceSetSelectPolarity; {A Device specific DeviceSetSelectPolarity method implementing the standard SPI device interface (Or nil if the default method is suitable)}
  DeviceGetByteDelay:TSPIDeviceGetByteDelay;           {A Device specific DeviceGetByteDelay method implementing the standard SPI device interface (Or nil if the default method is suitable)}
  DeviceSetByteDelay:TSPIDeviceSetByteDelay;           {A Device specific DeviceSetByteDelay method implementing the standard SPI device interface (Or nil if the default method is suitable)}
  DeviceGetProperties:TSPIDeviceGetProperties;         {A Device specific DeviceGetProperties method implementing the standard SPI device interface (Or nil if the default method is suitable)}
  {Statistics Properties}
  TransferCount:LongWord;
  TransferErrors:LongWord;
  {Driver Properties}
  Lock:TMutexHandle;                                   {Device lock}
  Wait:TSemaphoreHandle;                               {Write/Read wait event}
  Divider:LongWord;                                    {Clock divider (Used internally by drivers)}
  ClockRate:LongWord;                                  {Clock rate (Hz)}
  ClockPhase:LongWord;                                 {Clock Phase (eg SPI_CLOCK_PHASE_LOW)}
  ClockPolarity:LongWord;                              {Clock Polarity (eg SPI_CLOCK_POLARITY_LOW)}
  SelectPolarity:LongWord;                             {Default Chip Select Polarity (eg SPI_CS_POLARITY_LOW)}
  ByteDelay:LongWord;                                  {Delay between bytes written (Microseconds)}
  Properties:TSPIProperties;                           {Device properties}
  ChipSelects:array[0..SPI_CS_MAX] of TSPIChipSelect;  {Chip selects}
  {Internal Properties}
  Prev:PSPIDevice;                                     {Previous entry in SPI table}
  Next:PSPIDevice;                                     {Next entry in SPI table}
 end;

{==============================================================================}
{var}
 {SPI specific variables}

{==============================================================================}
{Initialization Functions}
procedure SPIInit;

{==============================================================================}
{SPI Functions}
function SPIDeviceStart(SPI:PSPIDevice;Mode,ClockRate,ClockPhase,ClockPolarity:LongWord):LongWord;
function SPIDeviceStop(SPI:PSPIDevice):LongWord;

function SPIDeviceRead(SPI:PSPIDevice;ChipSelect:Word;Dest:Pointer;Size,Flags:LongWord;var Count:LongWord):LongWord;
function SPIDeviceWrite(SPI:PSPIDevice;ChipSelect:Word;Source:Pointer;Size,Flags:LongWord;var Count:LongWord):LongWord;
function SPIDeviceWriteRead(SPI:PSPIDevice;ChipSelect:Word;Source,Dest:Pointer;Size,Flags:LongWord;var Count:LongWord):LongWord;

function SPIDeviceGetMode(SPI:PSPIDevice):LongWord;
function SPIDeviceSetMode(SPI:PSPIDevice;Mode:LongWord):LongWord;

function SPIDeviceGetClockRate(SPI:PSPIDevice;ChipSelect:Word):LongWord;
function SPIDeviceSetClockRate(SPI:PSPIDevice;ChipSelect:Word;ClockRate:LongWord):LongWord;

function SPIDeviceGetClockPhase(SPI:PSPIDevice):LongWord;
function SPIDeviceSetClockPhase(SPI:PSPIDevice;ClockPhase:LongWord):LongWord;

function SPIDeviceGetClockPolarity(SPI:PSPIDevice):LongWord;
function SPIDeviceSetClockPolarity(SPI:PSPIDevice;ClockPolarity:LongWord):LongWord;

function SPIDeviceGetSelectPolarity(SPI:PSPIDevice;ChipSelect:Word):LongWord;
function SPIDeviceSetSelectPolarity(SPI:PSPIDevice;ChipSelect:Word;SelectPolarity:LongWord):LongWord;

function SPIDeviceGetByteDelay(SPI:PSPIDevice):LongWord;
function SPIDeviceSetByteDelay(SPI:PSPIDevice;Delay:LongWord):LongWord;

function SPIDeviceProperties(SPI:PSPIDevice;Properties:PSPIProperties):LongWord; inline;
function SPIDeviceGetProperties(SPI:PSPIDevice;Properties:PSPIProperties):LongWord;

function SPIDeviceCreate:PSPIDevice;
function SPIDeviceCreateEx(Size:LongWord):PSPIDevice;
function SPIDeviceDestroy(SPI:PSPIDevice):LongWord;

function SPIDeviceRegister(SPI:PSPIDevice):LongWord;
function SPIDeviceDeregister(SPI:PSPIDevice):LongWord;

function SPIDeviceFind(SPIId:LongWord):PSPIDevice;
function SPIDeviceFindByName(const Name:String):PSPIDevice; inline;
function SPIDeviceFindByDescription(const Description:String):PSPIDevice; inline;
function SPIDeviceEnumerate(Callback:TSPIEnumerate;Data:Pointer):LongWord;

function SPIDeviceNotification(SPI:PSPIDevice;Callback:TSPINotification;Data:Pointer;Notification,Flags:LongWord):LongWord;

{==============================================================================}
{RTL SPI Functions}
function SysSPIAvailable:Boolean;

function SysSPIStart(Mode,ClockRate,ClockPhase,ClockPolarity:LongWord):LongWord;
function SysSPIStop:LongWord;

function SysSPIRead(ChipSelect:Word;Dest:Pointer;Size:LongWord;var Count:LongWord):LongWord;
function SysSPIWrite(ChipSelect:Word;Source:Pointer;Size:LongWord;var Count:LongWord):LongWord;
function SysSPIWriteRead(ChipSelect:Word;Source,Dest:Pointer;Size:LongWord;var Count:LongWord):LongWord;

function SysSPIGetMode:LongWord;
function SysSPISetMode(Mode:LongWord):LongWord;

function SysSPIGetClockRate(ChipSelect:Word):LongWord;
function SysSPISetClockRate(ChipSelect:Word;ClockRate:LongWord):LongWord;

function SysSPIGetClockPhase:LongWord;
function SysSPISetClockPhase(ClockPhase:LongWord):LongWord;

function SysSPIGetClockPolarity:LongWord;
function SysSPISetClockPolarity(ClockPolarity:LongWord):LongWord;

function SysSPIGetSelectPolarity(ChipSelect:Word):LongWord;
function SysSPISetSelectPolarity(ChipSelect:Word;SelectPolarity:LongWord):LongWord;

{==============================================================================}
{SPI Helper Functions}
function SPIGetCount:LongWord;
function SPIDeviceGetDefault:PSPIDevice;
function SPIDeviceSetDefault(SPI:PSPIDevice):LongWord;

function SPIDeviceCheck(SPI:PSPIDevice):PSPIDevice;

function SPITypeToString(SPIType:LongWord):String;
function SPIStateToString(SPIState:LongWord):String;

procedure SPILog(Level:LongWord;SPI:PSPIDevice;const AText:String);
procedure SPILogInfo(SPI:PSPIDevice;const AText:String); inline;
procedure SPILogWarn(SPI:PSPIDevice;const AText:String); inline;
procedure SPILogError(SPI:PSPIDevice;const AText:String); inline;
procedure SPILogDebug(SPI:PSPIDevice;const AText:String); inline;

function SPIChipSelectToString(ChipSelect:Word):String;
function SPIModeToString(Mode:LongWord):String;
function SPIClockPhaseToString(Phase:LongWord):String;
function SPIClockPolarityToString(Polarity:LongWord):String;
function SPISelectPolarityToString(Polarity:LongWord):String;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {SPI specific variables}
 SPIInitialized:Boolean;

 SPIDeviceTable:PSPIDevice;
 SPIDeviceTableLock:TCriticalSectionHandle = INVALID_HANDLE_VALUE;
 SPIDeviceTableCount:LongWord;

 SPIDeviceDefault:PSPIDevice;

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure SPIInit;
{Initialize the SPI unit and SPI device table}

{Note: Called only during system startup}
begin
 {}
 {Check Initialized}
 if SPIInitialized then Exit;

 {Initialize Logging}
 SPI_LOG_ENABLED:=(SPI_DEFAULT_LOG_LEVEL <> SPI_LOG_LEVEL_NONE);

 {Initialize SPI Table}
 SPIDeviceTable:=nil;
 SPIDeviceTableLock:=CriticalSectionCreate;
 SPIDeviceTableCount:=0;
 if SPIDeviceTableLock = INVALID_HANDLE_VALUE then
  begin
   if SPI_LOG_ENABLED then SPILogError(nil,'Failed to create SPI table lock');
  end;
 SPIDeviceDefault:=nil;

 {Register Platform SPI Handlers}
 SPIAvailableHandler:=SysSPIAvailable;
 SPIStartHandler:=SysSPIStart;
 SPIStopHandler:=SysSPIStop;
 SPIReadHandler:=SysSPIRead;
 SPIWriteHandler:=SysSPIWrite;
 SPIWriteReadHandler:=SysSPIWriteRead;
 SPIGetModeHandler:=SysSPIGetMode;
 SPISetModeHandler:=SysSPISetMode;
 SPIGetClockRateHandler:=SysSPIGetClockRate;
 SPISetClockRateHandler:=SysSPISetClockRate;
 SPIGetClockPhaseHandler:=SysSPIGetClockPhase;
 SPISetClockPhaseHandler:=SysSPISetClockPhase;
 SPIGetClockPolarityHandler:=SysSPIGetClockPolarity;
 SPISetClockPolarityHandler:=SysSPISetClockPolarity;
 SPIGetSelectPolarityHandler:=SysSPIGetSelectPolarity;
 SPISetSelectPolarityHandler:=SysSPISetSelectPolarity;

 SPIInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{SPI Functions}
function SPIDeviceStart(SPI:PSPIDevice;Mode,ClockRate,ClockPhase,ClockPolarity:LongWord):LongWord;
{Start the specified SPI device ready for writing and reading}
{SPI: The SPI device to start}
{Mode: The device mode to set (eg SPI_MODE_4WIRE)}
{ClockRate: The clock rate to set for the device}
{ClockPhase: The clock phase to set (eg SPI_CLOCK_PHASE_LOW)}
{ClockPolarity: The clock polarity to set (eg SPI_CLOCK_POLARITY_LOW)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check SPI}
 if SPI = nil then Exit;
 if SPI.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF SPI_DEBUG}
 if SPI_LOG_ENABLED then SPILogDebug(SPI,'SPI Device Start (Mode=' + SPIModeToString(Mode) + ' ClockRate=' + IntToStr(ClockRate) + ' ClockPhase=' + SPIClockPhaseToString(ClockPhase) + ' ClockPolarity=' + SPIClockPolarityToString(ClockPolarity) + ')');
 {$ENDIF}

 {Check Disabled}
 Result:=ERROR_SUCCESS;
 if SPI.SPIState <> SPI_STATE_DISABLED then Exit;

 if MutexLock(SPI.Lock) = ERROR_SUCCESS then
  begin
   try
    if Assigned(SPI.DeviceStart) then
     begin
      {Call Device Start}
      Result:=SPI.DeviceStart(SPI,Mode,ClockRate,ClockPhase,ClockPolarity);
      if Result <> ERROR_SUCCESS then Exit;
     end
    else
     begin
      Result:=ERROR_INVALID_PARAMETER;
      Exit;
     end;

    {Enable Device}
    SPI.SPIState:=SPI_STATE_ENABLED;

    {Notify Enable}
    NotifierNotify(@SPI.Device,DEVICE_NOTIFICATION_ENABLE);

    Result:=ERROR_SUCCESS;
   finally
    MutexUnlock(SPI.Lock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function SPIDeviceStop(SPI:PSPIDevice):LongWord;
{Stop the specified SPI device and terminate writing and reading}
{SPI: The SPI device to stop}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check SPI}
 if SPI = nil then Exit;
 if SPI.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF SPI_DEBUG}
 if SPI_LOG_ENABLED then SPILogDebug(SPI,'SPI Device Stop');
 {$ENDIF}

 {Check Enabled}
 Result:=ERROR_SUCCESS;
 if SPI.SPIState <> SPI_STATE_ENABLED then Exit;

 if MutexLock(SPI.Lock) = ERROR_SUCCESS then
  begin
   try
    if Assigned(SPI.DeviceStop) then
     begin
      {Call Device Stop}
      Result:=SPI.DeviceStop(SPI);
      if Result <> ERROR_SUCCESS then Exit;
     end
    else
     begin
      Result:=ERROR_INVALID_PARAMETER;
      Exit;
     end;

    {Disable Device}
    SPI.SPIState:=SPI_STATE_DISABLED;

    {Notify Disable}
    NotifierNotify(@SPI.Device,DEVICE_NOTIFICATION_DISABLE);

    Result:=ERROR_SUCCESS;
   finally
    MutexUnlock(SPI.Lock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function SPIDeviceRead(SPI:PSPIDevice;ChipSelect:Word;Dest:Pointer;Size,Flags:LongWord;var Count:LongWord):LongWord;
{Read data from the specified SPI device}
{Because SPI writes and then reads for each byte, dummy data will be written for each byte to be read}
{SPI: The SPI device to read from}
{ChipSelect: The chip select for the slave to read from (eg SPI_CS_0)}
{Dest: Pointer to a buffer to receive the data}
{Size: The size of the buffer}
{Flags: The flags for this transfer (eg SPI_TRANSFER_DMA)}
{Count: The number of bytes read on return}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 {Setup Result}
 Count:=0;
 Result:=ERROR_INVALID_PARAMETER;

 {Check Buffer}
 if Dest = nil then Exit;

 {Check SPI}
 if SPI = nil then Exit;
 if SPI.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF SPI_DEBUG}
 if SPI_LOG_ENABLED then SPILogDebug(SPI,'SPI Device Read (ChipSelect=' + SPIChipSelectToString(ChipSelect) + ' Size=' + IntToStr(Size) + ')');
 {$ENDIF}

 {Check Enabled}
 Result:=ERROR_NOT_SUPPORTED;
 if SPI.SPIState <> SPI_STATE_ENABLED then Exit;

 if MutexLock(SPI.Lock) = ERROR_SUCCESS then
  begin
   if Assigned(SPI.DeviceRead) then
    begin
     {Call Device Read}
     Result:=SPI.DeviceRead(SPI,ChipSelect,Dest,Size,Flags,Count);
    end
   else
    begin
     {Default Method}
     if Assigned(SPI.DeviceWriteRead) then
      begin
       {Call Device Write Read}
       Result:=SPI.DeviceWriteRead(SPI,ChipSelect,nil,Dest,Size,Flags,Count);
      end
     else
      begin
       Result:=ERROR_INVALID_PARAMETER;
      end;
    end;

   MutexUnlock(SPI.Lock);
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function SPIDeviceWrite(SPI:PSPIDevice;ChipSelect:Word;Source:Pointer;Size,Flags:LongWord;var Count:LongWord):LongWord;
{Write data to the specified SPI device}
{Because SPI writes and then reads for each byte, received data will be discarded for each byte written}
{SPI: The SPI device to write to}
{ChipSelect: The chip select for the slave to write to (eg SPI_CS_0)}
{Source: Pointer to a buffer of data to transmit}
{Size: The size of the buffer}
{Flags: The flags for this transfer (eg SPI_TRANSFER_DMA)}
{Count: The number of bytes written on return}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 {Setup Result}
 Count:=0;
 Result:=ERROR_INVALID_PARAMETER;

 {Check Buffer}
 if Source = nil then Exit;

 {Check SPI}
 if SPI = nil then Exit;
 if SPI.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF SPI_DEBUG}
 if SPI_LOG_ENABLED then SPILogDebug(SPI,'SPI Device Write (ChipSelect=' + SPIChipSelectToString(ChipSelect) + ' Size=' + IntToStr(Size) + ')');
 {$ENDIF}

 {Check Enabled}
 Result:=ERROR_NOT_SUPPORTED;
 if SPI.SPIState <> SPI_STATE_ENABLED then Exit;

 if MutexLock(SPI.Lock) = ERROR_SUCCESS then
  begin
   if Assigned(SPI.DeviceWrite) then
    begin
     {Call Device Write}
     Result:=SPI.DeviceWrite(SPI,ChipSelect,Source,Size,Flags,Count);
    end
   else
    begin
     {Default Method}
     if Assigned(SPI.DeviceWriteRead) then
      begin
       {Call Device Write Read}
       Result:=SPI.DeviceWriteRead(SPI,ChipSelect,Source,nil,Size,Flags,Count);
      end
     else
      begin
       Result:=ERROR_INVALID_PARAMETER;
      end;
    end;

   MutexUnlock(SPI.Lock);
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function SPIDeviceWriteRead(SPI:PSPIDevice;ChipSelect:Word;Source,Dest:Pointer;Size,Flags:LongWord;var Count:LongWord):LongWord;
{Write data to and Read data from the specified SPI device in one operation}
{Because SPI writes and then reads for each byte, both the source and dest buffers must be the same size}
{SPI: The SPI device to write to and read from}
{ChipSelect: The chip select for the slave to write to and read from (eg SPI_CS_0)}
{Source: Pointer to a buffer of data to transmit}
{Dest: Pointer to a buffer to receive the data}
{Size: The size of the buffer}
{Flags: The flags for this transfer (eg SPI_TRANSFER_DMA)}
{Count: The number of bytes written and read on return}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 {Setup Result}
 Count:=0;
 Result:=ERROR_INVALID_PARAMETER;

 {Check Buffers}
 if (Source = nil) and (Dest = nil) then Exit;

 {Check SPI}
 if SPI = nil then Exit;
 if SPI.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF SPI_DEBUG}
 if SPI_LOG_ENABLED then SPILogDebug(SPI,'SPI Device Write Read (ChipSelect=' + SPIChipSelectToString(ChipSelect) + ' Size=' + IntToStr(Size) + ')');
 {$ENDIF}

 {Check Enabled}
 Result:=ERROR_NOT_SUPPORTED;
 if SPI.SPIState <> SPI_STATE_ENABLED then Exit;

 if MutexLock(SPI.Lock) = ERROR_SUCCESS then
  begin
   if Assigned(SPI.DeviceWriteRead) then
    begin
     {Call Device Write Read}
     Result:=SPI.DeviceWriteRead(SPI,ChipSelect,Source,Dest,Size,Flags,Count);
    end
   else
    begin
     Result:=ERROR_INVALID_PARAMETER;
    end;

   MutexUnlock(SPI.Lock);
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function SPIDeviceGetMode(SPI:PSPIDevice):LongWord;
{Get the device mode of the specified SPI device}
{SPI: The SPI device to get device mode from}
{Return: The device mode or SPI_MODE_UNKNOWN on failure}
begin
 {}
 Result:=SPI_MODE_UNKNOWN;

 {Check SPI}
 if SPI = nil then Exit;
 if SPI.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF SPI_DEBUG}
 if SPI_LOG_ENABLED then SPILogDebug(SPI,'SPI Device Get Mode');
 {$ENDIF}

 {Check Enabled}
 {if SPI.SPIState <> SPI_STATE_ENABLED then Exit;} {Allow when disabled}

 if MutexLock(SPI.Lock) = ERROR_SUCCESS then
  begin
   if Assigned(SPI.DeviceGetMode) then
    begin
     {Call Device Get Mode}
     Result:=SPI.DeviceGetMode(SPI);
    end
   else
    begin
     {Get Mode}
     Result:=SPI.SPIMode;
    end;

   MutexUnlock(SPI.Lock);
  end;
end;

{==============================================================================}

function SPIDeviceSetMode(SPI:PSPIDevice;Mode:LongWord):LongWord;
{Set the device mode for the specified SPI device}
{SPI: The SPI device to set device mode for}
{Mode: The device mode to set (eg SPI_MODE_4WIRE)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check SPI}
 if SPI = nil then Exit;
 if SPI.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF SPI_DEBUG}
 if SPI_LOG_ENABLED then SPILogDebug(SPI,'SPI Device Set Mode (Mode=' + SPIModeToString(Mode) + ')');
 {$ENDIF}

 {Check Enabled}
 {Result:=ERROR_NOT_SUPPORTED;}
 {if SPI.SPIState <> SPI_STATE_ENABLED then Exit;} {Allow when disabled}

 if MutexLock(SPI.Lock) = ERROR_SUCCESS then
  begin
   try
    if Assigned(SPI.DeviceSetMode) then
     begin
      {Call Device Set Mode}
      Result:=SPI.DeviceSetMode(SPI,Mode);
     end
    else
     begin
      {Check Mode}
      if Mode = SPI_MODE_UNKNOWN then Exit;

      {Set Mode}
      SPI.SPIMode:=Mode;
      SPI.Properties.Mode:=Mode;

      Result:=ERROR_SUCCESS;
     end;
   finally
    MutexUnlock(SPI.Lock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function SPIDeviceGetClockRate(SPI:PSPIDevice;ChipSelect:Word):LongWord;
{Get the clock rate of the specified SPI device}
{SPI: The SPI device to get clock rate from}
{ChipSelect: The chip select number to get clock rate from (SPI_CS_NONE for default)}
{Return: The clock rate in Hz or 0 on failure}
begin
 {}
 Result:=0;

 {Check SPI}
 if SPI = nil then Exit;
 if SPI.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF SPI_DEBUG}
 if SPI_LOG_ENABLED then SPILogDebug(SPI,'SPI Device Get Clock Rate (ChipSelect=' + SPIChipSelectToString(ChipSelect) + ')');
 {$ENDIF}

 {Check Enabled}
 {if SPI.SPIState <> SPI_STATE_ENABLED then Exit;} {Allow when disabled}

 if MutexLock(SPI.Lock) = ERROR_SUCCESS then
  begin
   try
    if Assigned(SPI.DeviceGetClockRate) then
     begin
      {Call Device Get Clock Rate}
      Result:=SPI.DeviceGetClockRate(SPI,ChipSelect);
     end
    else
     begin
      {Check Chip Select}
      if (ChipSelect <> SPI_CS_NONE) and (ChipSelect > SPI_CS_MAX) then Exit;

      if ChipSelect = SPI_CS_NONE then
       begin
        {Get Default Clock Rate}
        Result:=SPI.ClockRate;
       end
      else
       begin
        {Get Chip Select Clock Rate}
        Result:=SPI.ChipSelects[ChipSelect].ClockRate;
       end;
     end;
   finally
    MutexUnlock(SPI.Lock);
   end;
  end;
end;

{==============================================================================}

function SPIDeviceSetClockRate(SPI:PSPIDevice;ChipSelect:Word;ClockRate:LongWord):LongWord;
{Set the clock rate for the specified SPI device}
{SPI: The SPI device to set clock rate for}
{ChipSelect: The chip select number to set clock rate for (SPI_CS_NONE for default)}
{ClockRate: The clock rate to set in Hz}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check SPI}
 if SPI = nil then Exit;
 if SPI.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF SPI_DEBUG}
 if SPI_LOG_ENABLED then SPILogDebug(SPI,'SPI Device Set Clock Rate (ChipSelect=' + SPIChipSelectToString(ChipSelect) + ' ClockRate=' + IntToStr(ClockRate) + ')');
 {$ENDIF}

 {Check Enabled}
 {Result:=ERROR_NOT_SUPPORTED;}
 {if SPI.SPIState <> SPI_STATE_ENABLED then Exit;} {Allow when disabled}

 if MutexLock(SPI.Lock) = ERROR_SUCCESS then
  begin
   try
    if Assigned(SPI.DeviceSetClockRate) then
     begin
      {Call Device Set Clock Rate}
      Result:=SPI.DeviceSetClockRate(SPI,ChipSelect,ClockRate);
     end
    else
     begin
      {Check Chip Select}
      if (ChipSelect <> SPI_CS_NONE) and (ChipSelect > SPI_CS_MAX) then Exit;

      if ChipSelect = SPI_CS_NONE then
       begin
        {Check Clock Rate}
        if ClockRate = 0 then Exit;

        {Set Default Clock Rate}
        SPI.ClockRate:=ClockRate;
        SPI.Properties.ClockRate:=ClockRate;
       end
      else
       begin
        {Set Chip Select Clock Rate}
        SPI.ChipSelects[ChipSelect].ClockRate:=ClockRate;
       end;

      Result:=ERROR_SUCCESS;
     end;
   finally
    MutexUnlock(SPI.Lock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function SPIDeviceGetClockPhase(SPI:PSPIDevice):LongWord;
{Get the clock phase of the specified SPI device}
{SPI: The SPI device to get clock phase from}
{Return: The clock phase or SPI_CLOCK_PHASE_UNKNOWN on failure}
begin
 {}
 Result:=SPI_CLOCK_PHASE_UNKNOWN;

 {Check SPI}
 if SPI = nil then Exit;
 if SPI.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF SPI_DEBUG}
 if SPI_LOG_ENABLED then SPILogDebug(SPI,'SPI Device Get Clock Phase');
 {$ENDIF}

 {Check Enabled}
 {if SPI.SPIState <> SPI_STATE_ENABLED then Exit;} {Allow when disabled}

 if MutexLock(SPI.Lock) = ERROR_SUCCESS then
  begin
   if Assigned(SPI.DeviceGetClockPhase) then
    begin
     {Call Device Get Clock Phase}
     Result:=SPI.DeviceGetClockPhase(SPI);
    end
   else
    begin
     {Get Clock Phase}
     Result:=SPI.ClockPhase;
    end;

   MutexUnlock(SPI.Lock);
  end;
end;

{==============================================================================}

function SPIDeviceSetClockPhase(SPI:PSPIDevice;ClockPhase:LongWord):LongWord;
{Set the clock phase for the specified SPI device}
{SPI: The SPI device to set clock phase for}
{ClockPhase: The clock phase to set (eg SPI_CLOCK_PHASE_LOW)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check SPI}
 if SPI = nil then Exit;
 if SPI.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF SPI_DEBUG}
 if SPI_LOG_ENABLED then SPILogDebug(SPI,'SPI Device Set Clock Phase (ClockPhase=' + SPIClockPhaseToString(ClockPhase) + ')');
 {$ENDIF}

 {Check Enabled}
 {Result:=ERROR_NOT_SUPPORTED;}
 {if SPI.SPIState <> SPI_STATE_ENABLED then Exit;} {Allow when disabled}

 if MutexLock(SPI.Lock) = ERROR_SUCCESS then
  begin
   try
    if Assigned(SPI.DeviceSetClockPhase) then
     begin
      {Call Device Set Clock Phase}
      Result:=SPI.DeviceSetClockPhase(SPI,ClockPhase);
     end
    else
     begin
      {Check Clock Phase}
      if ClockPhase = SPI_CLOCK_PHASE_UNKNOWN then Exit;

      {Set Clock Phase}
      SPI.ClockPhase:=ClockPhase;
      SPI.Properties.ClockPhase:=ClockPhase;

      Result:=ERROR_SUCCESS;
     end;
   finally
    MutexUnlock(SPI.Lock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function SPIDeviceGetClockPolarity(SPI:PSPIDevice):LongWord;
{Get the clock polarity of the specified SPI device}
{SPI: The SPI device to get clock polarity from}
{Return: The clock polarity or SPI_CLOCK_POLARITY_UNKNOWN on failure}
begin
 {}
 Result:=SPI_CLOCK_POLARITY_UNKNOWN;

 {Check SPI}
 if SPI = nil then Exit;
 if SPI.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF SPI_DEBUG}
 if SPI_LOG_ENABLED then SPILogDebug(SPI,'SPI Device Get Clock Polarity');
 {$ENDIF}

 {Check Enabled}
 {if SPI.SPIState <> SPI_STATE_ENABLED then Exit;} {Allow when disabled}

 if MutexLock(SPI.Lock) = ERROR_SUCCESS then
  begin
   if Assigned(SPI.DeviceGetClockPolarity) then
    begin
     {Call Device Get Clock Polarity}
     Result:=SPI.DeviceGetClockPolarity(SPI);
    end
   else
    begin
     {Get Clock Polarity}
     Result:=SPI.ClockPolarity;
    end;

   MutexUnlock(SPI.Lock);
  end;
end;

{==============================================================================}

function SPIDeviceSetClockPolarity(SPI:PSPIDevice;ClockPolarity:LongWord):LongWord;
{Set the clock polarity for the specified SPI device}
{SPI: The SPI device to set clock polarity for}
{ClockPolarity: The clock polarity to set (eg SPI_CLOCK_POLARITY_LOW)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check SPI}
 if SPI = nil then Exit;
 if SPI.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF SPI_DEBUG}
 if SPI_LOG_ENABLED then SPILogDebug(SPI,'SPI Device Set Clock Polarity (ClockPolarity=' + SPIClockPolarityToString(ClockPolarity) + ')');
 {$ENDIF}

 {Check Enabled}
 {Result:=ERROR_NOT_SUPPORTED;}
 {if SPI.SPIState <> SPI_STATE_ENABLED then Exit;} {Allow when disabled}

 if MutexLock(SPI.Lock) = ERROR_SUCCESS then
  begin
   try
    if Assigned(SPI.DeviceSetClockPolarity) then
     begin
      {Call Device Set Clock Polarity}
      Result:=SPI.DeviceSetClockPolarity(SPI,ClockPolarity);
     end
    else
     begin
      {Check Clock Polarity}
      if ClockPolarity = SPI_CLOCK_POLARITY_UNKNOWN then Exit;

      {Set Clock Polarity}
      SPI.ClockPolarity:=ClockPolarity;
      SPI.Properties.ClockPolarity:=ClockPolarity;

      Result:=ERROR_SUCCESS;
     end;
   finally
    MutexUnlock(SPI.Lock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function SPIDeviceGetSelectPolarity(SPI:PSPIDevice;ChipSelect:Word):LongWord;
{Get the chip select polarity of the specified SPI device}
{SPI: The SPI device to get chip select polarity from}
{ChipSelect: The chip select number to get polarity from (SPI_CS_NONE for default)}
{Return: The chip select polarity or SPI_CS_POLARITY_UNKNOWN on failure}
begin
 {}
 Result:=SPI_CS_POLARITY_UNKNOWN;

 {Check SPI}
 if SPI = nil then Exit;
 if SPI.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF SPI_DEBUG}
 if SPI_LOG_ENABLED then SPILogDebug(SPI,'SPI Device Get Select Polarity (ChipSelect=' + SPIChipSelectToString(ChipSelect) + ')');
 {$ENDIF}

 {Check Enabled}
 {if SPI.SPIState <> SPI_STATE_ENABLED then Exit;} {Allow when disabled}

 if MutexLock(SPI.Lock) = ERROR_SUCCESS then
  begin
   try
    if Assigned(SPI.DeviceGetSelectPolarity) then
     begin
      {Call Device Get Select Polarity}
      Result:=SPI.DeviceGetSelectPolarity(SPI,ChipSelect);
     end
    else
     begin
      {Check Chip Select}
      if (ChipSelect <> SPI_CS_NONE) and (ChipSelect > SPI_CS_MAX) then Exit;

      if ChipSelect = SPI_CS_NONE then
       begin
        {Get Default Select Polarity}
        Result:=SPI.SelectPolarity;
       end
      else
       begin
        {Get Chip Select Polarity}
        Result:=SPI.ChipSelects[ChipSelect].SelectPolarity;
       end;
     end;
   finally
    MutexUnlock(SPI.Lock);
   end;
  end;
end;

{==============================================================================}

function SPIDeviceSetSelectPolarity(SPI:PSPIDevice;ChipSelect:Word;SelectPolarity:LongWord):LongWord;
{Set the chip select polarity for the specified SPI device}
{SPI: The SPI device to set chip select polarity for}
{ChipSelect: The chip select number to set polarity for (SPI_CS_NONE for default)}
{SelectPolarity: The chip select polarity to set (eg SPI_CS_POLARITY_LOW)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check SPI}
 if SPI = nil then Exit;
 if SPI.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF SPI_DEBUG}
 if SPI_LOG_ENABLED then SPILogDebug(SPI,'SPI Device Set Select Polarity (ChipSelect=' + SPIChipSelectToString(ChipSelect) + ' SelectPolarity=' + SPISelectPolarityToString(SelectPolarity) + ')');
 {$ENDIF}

 {Check Enabled}
 {Result:=ERROR_NOT_SUPPORTED;}
 {if SPI.SPIState <> SPI_STATE_ENABLED then Exit;} {Allow when disabled}

 if MutexLock(SPI.Lock) = ERROR_SUCCESS then
  begin
   try
    if Assigned(SPI.DeviceSetSelectPolarity) then
     begin
      {Call Device Set Select Polarity}
      Result:=SPI.DeviceSetSelectPolarity(SPI,ChipSelect,SelectPolarity);
     end
    else
     begin
      {Check Chip Select}
      if (ChipSelect <> SPI_CS_NONE) and (ChipSelect > SPI_CS_MAX) then Exit;

      {Check Select Polarity}
      if SelectPolarity = SPI_CS_POLARITY_UNKNOWN then Exit;

      if ChipSelect = SPI_CS_NONE then
       begin
        {Set Default Select Polarity}
        SPI.SelectPolarity:=SelectPolarity;
        SPI.Properties.SelectPolarity:=SelectPolarity;
       end
      else
       begin
        {Set Chip Select Polarity}
        SPI.ChipSelects[ChipSelect].SelectPolarity:=SelectPolarity;
       end;

      Result:=ERROR_SUCCESS;
     end;
   finally
    MutexUnlock(SPI.Lock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function SPIDeviceGetByteDelay(SPI:PSPIDevice):LongWord;
{Get the delay between bytes written for the specified SPI device}
{SPI: The SPI device to get the byte delay from}
{Return: The byte delay in microseconds, 0 if not set or on failure}
begin
 //To Do //Should be per device plus per ChipSelect ? - Yes
end;

{==============================================================================}

function SPIDeviceSetByteDelay(SPI:PSPIDevice;Delay:LongWord):LongWord;
{Set the delay between bytes written for the specified SPI device}
{SPI: The SPI device to set byte delay for}
{Delay: The byte delay to set in microseconds}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 //To Do //Should be per device plus per ChipSelect ? - Yes
end;

{==============================================================================}

function SPIDeviceProperties(SPI:PSPIDevice;Properties:PSPIProperties):LongWord; inline;
{Get the properties for the specified SPI device}
{SPI: The SPI device to get properties from}
{Properties: Pointer to a TSPIProperties structure to fill in}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: Replaced by SPIDeviceGetProperties for consistency}
begin
 {}
 Result:=SPIDeviceGetProperties(SPI,Properties);
end;

{==============================================================================}

function SPIDeviceGetProperties(SPI:PSPIDevice;Properties:PSPIProperties):LongWord;
{Get the properties for the specified SPI device}
{SPI: The SPI device to get properties from}
{Properties: Pointer to a TSPIProperties structure to fill in}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Properties}
 if Properties = nil then Exit;

 {Check SPI}
 if SPI = nil then Exit;
 if SPI.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF SPI_DEBUG}
 if SPI_LOG_ENABLED then SPILogDebug(SPI,'SPI Device Get Properties');
 {$ENDIF}

 {Check Enabled}
 {Result:=ERROR_NOT_SUPPORTED;}
 {if SPI.SPIState <> SPI_STATE_ENABLED then Exit;} {Allow when disabled}

 if MutexLock(SPI.Lock) = ERROR_SUCCESS then
  begin
   if Assigned(SPI.DeviceGetProperties) then
    begin
     {Call Device Get Properites}
     Result:=SPI.DeviceGetProperties(SPI,Properties);
    end
   else
    begin
     {Get Properties}
     System.Move(SPI.Properties,Properties^,SizeOf(TSPIProperties));

     {Return Result}
     Result:=ERROR_SUCCESS;
    end;

   MutexUnlock(SPI.Lock);
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function SPIDeviceCreate:PSPIDevice;
{Create a new SPI entry}
{Return: Pointer to new SPI entry or nil if SPI could not be created}
begin
 {}
 Result:=SPIDeviceCreateEx(SizeOf(TSPIDevice));
end;

{==============================================================================}

function SPIDeviceCreateEx(Size:LongWord):PSPIDevice;
{Create a new SPI entry}
{Size: Size in bytes to allocate for new SPI (Including the SPI entry)}
{Return: Pointer to new SPI entry or nil if SPI could not be created}
var
 Count:LongWord;
begin
 {}
 Result:=nil;

 {Check Size}
 if Size < SizeOf(TSPIDevice) then Exit;

 {Create SPI}
 Result:=PSPIDevice(DeviceCreateEx(Size));
 if Result = nil then Exit;

 {Update Device}
 Result.Device.DeviceBus:=DEVICE_BUS_NONE;
 Result.Device.DeviceType:=SPI_TYPE_NONE;
 Result.Device.DeviceFlags:=SPI_FLAG_NONE;
 Result.Device.DeviceData:=nil;

 {Update SPI}
 Result.SPIId:=DEVICE_ID_ANY;
 Result.SPIState:=SPI_STATE_DISABLED;
 Result.SPIMode:=SPI_MODE_UNKNOWN;
 Result.DeviceStart:=nil;
 Result.DeviceStop:=nil;
 Result.DeviceRead:=nil;
 Result.DeviceWrite:=nil;
 Result.DeviceWriteRead:=nil;
 Result.DeviceGetMode:=nil;
 Result.DeviceSetMode:=nil;
 Result.DeviceGetClockRate:=nil;
 Result.DeviceSetClockRate:=nil;
 Result.DeviceGetClockPhase:=nil;
 Result.DeviceSetClockPhase:=nil;
 Result.DeviceGetClockPolarity:=nil;
 Result.DeviceSetClockPolarity:=nil;
 Result.DeviceGetSelectPolarity:=nil;
 Result.DeviceSetSelectPolarity:=nil;
 Result.DeviceGetByteDelay:=nil;
 Result.DeviceSetByteDelay:=nil;
 Result.DeviceGetProperties:=nil;
 Result.Lock:=INVALID_HANDLE_VALUE;
 Result.Wait:=INVALID_HANDLE_VALUE;
 Result.Divider:=0;
 Result.ClockRate:=0;
 Result.ClockPhase:=SPI_CLOCK_PHASE_UNKNOWN;
 Result.ClockPolarity:=SPI_CLOCK_POLARITY_UNKNOWN;
 Result.SelectPolarity:=SPI_CS_POLARITY_UNKNOWN;
 Result.ByteDelay:=0;
 for Count:=0 to SPI_CS_MAX do
  begin
   Result.ChipSelects[Count].Mode:=SPI_MODE_UNKNOWN;
   Result.ChipSelects[Count].Divider:=0;
   Result.ChipSelects[Count].ClockRate:=0;
   Result.ChipSelects[Count].ClockPhase:=SPI_CLOCK_PHASE_UNKNOWN;
   Result.ChipSelects[Count].ClockPolarity:=SPI_CLOCK_POLARITY_UNKNOWN;
   Result.ChipSelects[Count].SelectPolarity:=SPI_CS_POLARITY_UNKNOWN;
   Result.ChipSelects[Count].ByteDelay:=0;
  end;

 {Create Lock}
 Result.Lock:=MutexCreateEx(False,MUTEX_DEFAULT_SPINCOUNT,MUTEX_FLAG_RECURSIVE);
 if Result.Lock = INVALID_HANDLE_VALUE then
  begin
   if SPI_LOG_ENABLED then SPILogError(nil,'Failed to create lock for SPI device');
   SPIDeviceDestroy(Result);
   Result:=nil;
   Exit;
  end;
end;

{==============================================================================}

function SPIDeviceDestroy(SPI:PSPIDevice):LongWord;
{Destroy an existing SPI entry}
{SPI: The SPI device to destroy}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check SPI}
 if SPI = nil then Exit;
 if SPI.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check SPI}
 Result:=ERROR_IN_USE;
 if SPIDeviceCheck(SPI) = SPI then Exit;

 {Check State}
 if SPI.Device.DeviceState <> DEVICE_STATE_UNREGISTERED then Exit;

 {Destroy Lock}
 if SPI.Lock <> INVALID_HANDLE_VALUE then
  begin
   MutexDestroy(SPI.Lock);
  end;

 {Destroy SPI}
 Result:=DeviceDestroy(@SPI.Device);
end;

{==============================================================================}

function SPIDeviceRegister(SPI:PSPIDevice):LongWord;
{Register a new SPI in the SPI table}
{SPI: The SPI device to register}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 SPIId:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check SPI}
 if SPI = nil then Exit;
 if SPI.SPIId <> DEVICE_ID_ANY then Exit;
 if SPI.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Interfaces}
 if not(Assigned(SPI.DeviceStart)) then Exit;
 if not(Assigned(SPI.DeviceStop)) then Exit;
 if not(Assigned(SPI.DeviceWriteRead)) then Exit;

 {Check SPI}
 Result:=ERROR_ALREADY_EXISTS;
 if SPIDeviceCheck(SPI) = SPI then Exit;

 {Check State}
 if SPI.Device.DeviceState <> DEVICE_STATE_UNREGISTERED then Exit;

 {Insert SPI}
 if CriticalSectionLock(SPIDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Update SPI}
    SPIId:=0;
    while SPIDeviceFind(SPIId) <> nil do
     begin
      Inc(SPIId);
     end;
    SPI.SPIId:=SPIId;

    {Update Device}
    SPI.Device.DeviceName:=SPI_NAME_PREFIX + IntToStr(SPI.SPIId);
    SPI.Device.DeviceClass:=DEVICE_CLASS_SPI;

    {Register Device}
    Result:=DeviceRegister(@SPI.Device);
    if Result <> ERROR_SUCCESS then
     begin
      SPI.SPIId:=DEVICE_ID_ANY;
      Exit;
     end;

    {Link SPI}
    if SPIDeviceTable = nil then
     begin
      SPIDeviceTable:=SPI;
     end
    else
     begin
      SPI.Next:=SPIDeviceTable;
      SPIDeviceTable.Prev:=SPI;
      SPIDeviceTable:=SPI;
     end;

    {Increment Count}
    Inc(SPIDeviceTableCount);

    {Check Default}
    if SPIDeviceDefault = nil then
     begin
      SPIDeviceDefault:=SPI;
     end;

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    CriticalSectionUnlock(SPIDeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function SPIDeviceDeregister(SPI:PSPIDevice):LongWord;
{Deregister an SPI from the SPI table}
{SPI: The SPI device to deregister}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Prev:PSPIDevice;
 Next:PSPIDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check SPI}
 if SPI = nil then Exit;
 if SPI.SPIId = DEVICE_ID_ANY then Exit;
 if SPI.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check SPI}
 Result:=ERROR_NOT_FOUND;
 if SPIDeviceCheck(SPI) <> SPI then Exit;

 {Check State}
 if SPI.Device.DeviceState <> DEVICE_STATE_REGISTERED then Exit;

 {Remove SPI}
 if CriticalSectionLock(SPIDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Deregister Device}
    Result:=DeviceDeregister(@SPI.Device);
    if Result <> ERROR_SUCCESS then Exit;

    {Unlink SPI}
    Prev:=SPI.Prev;
    Next:=SPI.Next;
    if Prev = nil then
     begin
      SPIDeviceTable:=Next;
      if Next <> nil then
       begin
        Next.Prev:=nil;
       end;
     end
    else
     begin
      Prev.Next:=Next;
      if Next <> nil then
       begin
        Next.Prev:=Prev;
       end;
     end;

    {Decrement Count}
    Dec(SPIDeviceTableCount);

    {Check Default}
    if SPIDeviceDefault = SPI then
     begin
      SPIDeviceDefault:=SPIDeviceTable;
     end;

    {Update SPI}
    SPI.SPIId:=DEVICE_ID_ANY;

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    CriticalSectionUnlock(SPIDeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function SPIDeviceFind(SPIId:LongWord):PSPIDevice;
{Find an SPI device by ID in the SPI table}
{SPIId: The ID number of the SPI device to find}
{Return: Pointer to SPI device entry or nil if not found}
var
 SPI:PSPIDevice;
begin
 {}
 Result:=nil;

 {Check Id}
 if SPIId = DEVICE_ID_ANY then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(SPIDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get SPI}
    SPI:=SPIDeviceTable;
    while SPI <> nil do
     begin
      {Check State}
      if SPI.Device.DeviceState = DEVICE_STATE_REGISTERED then
       begin
        {Check Id}
        if SPI.SPIId = SPIId then
         begin
          Result:=SPI;
          Exit;
         end;
       end;

      {Get Next}
      SPI:=SPI.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(SPIDeviceTableLock);
   end;
  end;
end;

{==============================================================================}

function SPIDeviceFindByName(const Name:String):PSPIDevice; inline;
{Find an SPI device by name in the device table}
{Name: The name of the SPI device to find (eg SPI0)}
{Return: Pointer to SPI device entry or nil if not found}
begin
 {}
 Result:=PSPIDevice(DeviceFindByName(Name));
end;

{==============================================================================}

function SPIDeviceFindByDescription(const Description:String):PSPIDevice; inline;
{Find an SPI device by description in the device table}
{Description: The description of the SPI to find (eg BCM2837 SPI0 Master)}
{Return: Pointer to SPI device entry or nil if not found}
begin
 {}
 Result:=PSPIDevice(DeviceFindByDescription(Description));
end;

{==============================================================================}

function SPIDeviceEnumerate(Callback:TSPIEnumerate;Data:Pointer):LongWord;
{Enumerate all SPI devices in the SPI table}
{Callback: The callback function to call for each SPI device in the table}
{Data: A private data pointer to pass to callback for each SPI device in the table}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 SPI:PSPIDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Callback}
 if not Assigned(Callback) then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(SPIDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get SPI}
    SPI:=SPIDeviceTable;
    while SPI <> nil do
     begin
      {Check State}
      if SPI.Device.DeviceState = DEVICE_STATE_REGISTERED then
       begin
        if Callback(SPI,Data) <> ERROR_SUCCESS then Exit;
       end;

      {Get Next}
      SPI:=SPI.Next;
     end;

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    CriticalSectionUnlock(SPIDeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function SPIDeviceNotification(SPI:PSPIDevice;Callback:TSPINotification;Data:Pointer;Notification,Flags:LongWord):LongWord;
{Register a notification for SPI device changes}
{Device: The SPI device to notify changes for (Optional, pass nil for all SPI devices)}
{Callback: The function to call when a notification event occurs}
{Data: A private data pointer to pass to callback when a notification event occurs}
{Notification: The events to register for notification of (eg DEVICE_NOTIFICATION_REGISTER)}
{Flags: The flags to control the notification (eg NOTIFIER_FLAG_WORKER)}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check SPI}
 if SPI = nil then
  begin
   Result:=DeviceNotification(nil,DEVICE_CLASS_SPI,Callback,Data,Notification,Flags);
  end
 else
  begin
   {Check SPI}
   if SPI.Device.Signature <> DEVICE_SIGNATURE then Exit;

   Result:=DeviceNotification(@SPI.Device,DEVICE_CLASS_SPI,Callback,Data,Notification,Flags);
  end;
end;

{==============================================================================}
{==============================================================================}
{RTL SPI Functions}
function SysSPIAvailable:Boolean;
{Check if an SPI device is available}
begin
 {}
 Result:=(SPIDeviceDefault <> nil);
end;

{==============================================================================}

function SysSPIStart(Mode,ClockRate,ClockPhase,ClockPolarity:LongWord):LongWord;
{Start the default SPI device ready for writing and reading}
{Mode: The device mode to set (eg SPI_MODE_4WIRE)}
{ClockRate: The clock rate to set for the device}
{ClockPhase: The clock phase to set (eg SPI_CLOCK_PHASE_LOW)}
{ClockPolarity: The clock polarity to set (eg SPI_CLOCK_POLARITY_LOW)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 if SPIDeviceDefault = nil then Exit;

 Result:=SPIDeviceStart(SPIDeviceDefault,Mode,ClockRate,ClockPhase,ClockPolarity);
end;

{==============================================================================}

function SysSPIStop:LongWord;
{Stop the default SPI device and terminate writing and reading}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 if SPIDeviceDefault = nil then Exit;

 Result:=SPIDeviceStop(SPIDeviceDefault);
end;

{==============================================================================}

function SysSPIRead(ChipSelect:Word;Dest:Pointer;Size:LongWord;var Count:LongWord):LongWord;
{Read data from the default SPI device}
{Because SPI writes and then reads for each byte, dummy data will be written for each byte to be read}
{ChipSelect: The chip select for the slave to read from (eg SPI_CS_0)}
{Dest: Pointer to a buffer to receive the data}
{Size: The size of the buffer}
{Count: The number of bytes read on return}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 {Setup Result}
 Count:=0;
 Result:=ERROR_INVALID_PARAMETER;

 if SPIDeviceDefault = nil then Exit;

 Result:=SPIDeviceRead(SPIDeviceDefault,ChipSelect,Dest,Size,SPI_TRANSFER_NONE,Count);
end;

{==============================================================================}

function SysSPIWrite(ChipSelect:Word;Source:Pointer;Size:LongWord;var Count:LongWord):LongWord;
{Write data to the default SPI device}
{Because SPI writes and then reads for each byte, received data will be discarded for each byte written}
{ChipSelect: The chip select for the slave to write to (eg SPI_CS_0)}
{Source: Pointer to a buffer of data to transmit}
{Size: The size of the buffer}
{Count: The number of bytes written on return}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 {Setup Result}
 Count:=0;
 Result:=ERROR_INVALID_PARAMETER;

 if SPIDeviceDefault = nil then Exit;

 Result:=SPIDeviceWrite(SPIDeviceDefault,ChipSelect,Source,Size,SPI_TRANSFER_NONE,Count);
end;

{==============================================================================}

function SysSPIWriteRead(ChipSelect:Word;Source,Dest:Pointer;Size:LongWord;var Count:LongWord):LongWord;
{Write data to and Read data from the default SPI device in one operation}
{Because SPI writes and then reads for each byte, both the source and dest buffers must be the same size}
{ChipSelect: The chip select for the slave to write to and read from (eg SPI_CS_0)}
{Source: Pointer to a buffer of data to transmit}
{Dest: Pointer to a buffer to receive the data}
{Size: The size of the buffer}
{Count: The number of bytes written and read on return}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 {Setup Result}
 Count:=0;
 Result:=ERROR_INVALID_PARAMETER;

 if SPIDeviceDefault = nil then Exit;

 Result:=SPIDeviceWriteRead(SPIDeviceDefault,ChipSelect,Source,Dest,Size,SPI_TRANSFER_NONE,Count);
end;

{==============================================================================}

function SysSPIGetMode:LongWord;
{Get the device mode of the default SPI device}
{Return: The device mode or SPI_MODE_UNKNOWN on failure}
begin
 {}
 Result:=SPI_MODE_UNKNOWN;

 if SPIDeviceDefault = nil then Exit;

 Result:=SPIDeviceGetMode(SPIDeviceDefault);
end;

{==============================================================================}

function SysSPISetMode(Mode:LongWord):LongWord;
{Set the device mode for the default SPI device}
{Mode: The device mode to set (eg SPI_MODE_4WIRE)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 if SPIDeviceDefault = nil then Exit;

 Result:=SPIDeviceSetMode(SPIDeviceDefault,Mode);
end;

{==============================================================================}

function SysSPIGetClockRate(ChipSelect:Word):LongWord;
{Get the clock rate of the default SPI device}
{ChipSelect: The chip select number to get clock rate from (SPI_CS_NONE for default)}
{Return: The clock rate in Hz or 0 on failure}
begin
 {}
 Result:=0;

 if SPIDeviceDefault = nil then Exit;

 Result:=SPIDeviceGetClockRate(SPIDeviceDefault,ChipSelect);
end;

{==============================================================================}

function SysSPISetClockRate(ChipSelect:Word;ClockRate:LongWord):LongWord;
{Set the clock rate for the default SPI device}
{ChipSelect: The chip select number to set clock rate for (SPI_CS_NONE for default)}
{ClockRate: The clock rate to set in Hz}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 if SPIDeviceDefault = nil then Exit;

 Result:=SPIDeviceSetClockRate(SPIDeviceDefault,ChipSelect,ClockRate);
end;

{==============================================================================}

function SysSPIGetClockPhase:LongWord;
{Get the clock phase of the default SPI device}
{Return: The clock phase or SPI_CLOCK_PHASE_UNKNOWN on failure}
begin
 {}
 Result:=SPI_CLOCK_PHASE_UNKNOWN;

 if SPIDeviceDefault = nil then Exit;

 Result:=SPIDeviceGetClockPhase(SPIDeviceDefault);
end;

{==============================================================================}

function SysSPISetClockPhase(ClockPhase:LongWord):LongWord;
{Set the clock phase for the default SPI device}
{ClockPhase: The clock phase to set (eg SPI_CLOCK_PHASE_LOW)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 if SPIDeviceDefault = nil then Exit;

 Result:=SPIDeviceSetClockPhase(SPIDeviceDefault,ClockPhase);
end;

{==============================================================================}

function SysSPIGetClockPolarity:LongWord;
{Get the clock polarity of the default SPI device}
{Return: The clock polarity or SPI_CLOCK_POLARITY_UNKNOWN on failure}
begin
 {}
 Result:=SPI_CLOCK_POLARITY_UNKNOWN;

 if SPIDeviceDefault = nil then Exit;

 Result:=SPIDeviceGetClockPolarity(SPIDeviceDefault);
end;

{==============================================================================}

function SysSPISetClockPolarity(ClockPolarity:LongWord):LongWord;
{Set the clock polarity for the default SPI device}
{ClockPolarity: The clock polarity to set (eg SPI_CLOCK_POLARITY_LOW)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 if SPIDeviceDefault = nil then Exit;

 Result:=SPIDeviceSetClockPolarity(SPIDeviceDefault,ClockPolarity);
end;

{==============================================================================}

function SysSPIGetSelectPolarity(ChipSelect:Word):LongWord;
{Get the chip select polarity of the default SPI device}
{ChipSelect: The chip select number to get polarity from (SPI_CS_NONE for default)}
{Return: The chip select polarity or SPI_CS_POLARITY_UNKNOWN on failure}
begin
 {}
 Result:=SPI_CS_POLARITY_UNKNOWN;

 if SPIDeviceDefault = nil then Exit;

 Result:=SPIDeviceGetSelectPolarity(SPIDeviceDefault,ChipSelect);
end;

{==============================================================================}

function SysSPISetSelectPolarity(ChipSelect:Word;SelectPolarity:LongWord):LongWord;
{Set the chip select polarity for the default SPI device}
{ChipSelect: The chip select number to set polarity for (SPI_CS_NONE for default)}
{SelectPolarity: The chip select polarity to set (eg SPI_CS_POLARITY_LOW)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 if SPIDeviceDefault = nil then Exit;

 Result:=SPIDeviceSetSelectPolarity(SPIDeviceDefault,ChipSelect,SelectPolarity);
end;

{==============================================================================}
{==============================================================================}
{SPI Helper Functions}
function SPIGetCount:LongWord;
{Get the current SPI count}
begin
 {}
 Result:=SPIDeviceTableCount;
end;

{==============================================================================}

function SPIDeviceGetDefault:PSPIDevice;
{Get the current default SPI device}
begin
 {}
 Result:=SPIDeviceDefault;
end;

{==============================================================================}

function SPIDeviceSetDefault(SPI:PSPIDevice):LongWord;
{Set the current default SPI device}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check SPI}
 if SPI = nil then Exit;
 if SPI.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(SPIDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Check SPI}
    if SPIDeviceCheck(SPI) <> SPI then Exit;

    {Set SPI Default}
    SPIDeviceDefault:=SPI;

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    CriticalSectionUnlock(SPIDeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function SPIDeviceCheck(SPI:PSPIDevice):PSPIDevice;
{Check if the supplied SPI is in the SPI table}
var
 Current:PSPIDevice;
begin
 {}
 Result:=nil;

 {Check SPI}
 if SPI = nil then Exit;
 if SPI.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(SPIDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get SPI}
    Current:=SPIDeviceTable;
    while Current <> nil do
     begin
      {Check SPI}
      if Current = SPI then
       begin
        Result:=SPI;
        Exit;
       end;

      {Get Next}
      Current:=Current.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(SPIDeviceTableLock);
   end;
  end;
end;

{==============================================================================}

function SPITypeToString(SPIType:LongWord):String;
{Convert an SPI type value to a string}
begin
 {}
 Result:='SPI_TYPE_UNKNOWN';

 if SPIType <= SPI_TYPE_MAX then
  begin
   Result:=SPI_TYPE_NAMES[SPIType];
  end;
end;

{==============================================================================}

function SPIStateToString(SPIState:LongWord):String;
{Convert an SPI state value to a string}
begin
 {}
 Result:='SPI_STATE_UNKNOWN';

 if SPIState <= SPI_STATE_MAX then
  begin
   Result:=SPI_STATE_NAMES[SPIState];
  end;
end;

{==============================================================================}

procedure SPILog(Level:LongWord;SPI:PSPIDevice;const AText:String);
var
 WorkBuffer:String;
begin
 {}
 {Check Level}
 if Level < SPI_DEFAULT_LOG_LEVEL then Exit;

 WorkBuffer:='';
 {Check Level}
 if Level = SPI_LOG_LEVEL_DEBUG then
  begin
   WorkBuffer:=WorkBuffer + '[DEBUG] ';
  end
 else if Level = SPI_LOG_LEVEL_WARN then
  begin
   WorkBuffer:=WorkBuffer + '[WARN] ';
  end
 else if Level = SPI_LOG_LEVEL_ERROR then
  begin
   WorkBuffer:=WorkBuffer + '[ERROR] ';
  end;

 {Add Prefix}
 WorkBuffer:=WorkBuffer + 'SPI: ';

 {Check SPI}
 if SPI <> nil then
  begin
   WorkBuffer:=WorkBuffer + SPI_NAME_PREFIX + IntToStr(SPI.SPIId) + ': ';
  end;

 {Output Logging}
 LoggingOutputEx(LOGGING_FACILITY_SPI,LogLevelToLoggingSeverity(Level),'SPI',WorkBuffer + AText);
end;

{==============================================================================}

procedure SPILogInfo(SPI:PSPIDevice;const AText:String); inline;
begin
 {}
 SPILog(SPI_LOG_LEVEL_INFO,SPI,AText);
end;

{==============================================================================}

procedure SPILogWarn(SPI:PSPIDevice;const AText:String); inline;
begin
 {}
 SPILog(SPI_LOG_LEVEL_WARN,SPI,AText);
end;

{==============================================================================}

procedure SPILogError(SPI:PSPIDevice;const AText:String); inline;
begin
 {}
 SPILog(SPI_LOG_LEVEL_ERROR,SPI,AText);
end;

{==============================================================================}

procedure SPILogDebug(SPI:PSPIDevice;const AText:String); inline;
begin
 {}
 SPILog(SPI_LOG_LEVEL_DEBUG,SPI,AText);
end;

{==============================================================================}

function SPIChipSelectToString(ChipSelect:Word):String;
begin
 {}
 Result:='SPI_CS_UNKNOWN';

 if ChipSelect > SPI_CS_MAX then Exit;

 Result:='SPI_CS_' + IntToStr(ChipSelect);
end;

{==============================================================================}

function SPIModeToString(Mode:LongWord):String;
begin
 {}
 Result:='SPI_MODE_UNKNOWN';

 case Mode of
  SPI_MODE_4WIRE:Result:='SPI_MODE_4WIRE';
  SPI_MODE_3WIRE:Result:='SPI_MODE_3WIRE';
  SPI_MODE_LOSSI:Result:='SPI_MODE_LOSSI';
 end;
end;

{==============================================================================}

function SPIClockPhaseToString(Phase:LongWord):String;
begin
 {}
 Result:='SPI_CLOCK_PHASE_UNKNOWN';

 case Phase of
  SPI_CLOCK_PHASE_LOW:Result:='SPI_CLOCK_PHASE_LOW';
  SPI_CLOCK_PHASE_HIGH:Result:='SPI_CLOCK_PHASE_HIGH';
 end;
end;

{==============================================================================}

function SPIClockPolarityToString(Polarity:LongWord):String;
begin
 {}
 Result:='SPI_CLOCK_POLARITY_UNKNOWN';

 case Polarity of
  SPI_CLOCK_POLARITY_LOW:Result:='SPI_CLOCK_POLARITY_LOW';
  SPI_CLOCK_POLARITY_HIGH:Result:='SPI_CLOCK_POLARITY_HIGH';
 end;
end;

{==============================================================================}

function SPISelectPolarityToString(Polarity:LongWord):String;
begin
 {}
 Result:='SPI_CS_POLARITY_UNKNOWN';

 case Polarity of
  SPI_CS_POLARITY_LOW:Result:='SPI_CS_POLARITY_LOW';
  SPI_CS_POLARITY_HIGH:Result:='SPI_CS_POLARITY_HIGH';
 end;
end;

{==============================================================================}
{==============================================================================}

initialization
 SPIInit;

{==============================================================================}

finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
