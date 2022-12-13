{
Ultibo I2C interface unit.

Copyright (C) 2022 - SoftOz Pty Ltd.

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

 I2C - https://en.wikipedia.org/wiki/I%C2%B2C
  
 I2C Bus - https://www.i2c-bus.org/

I2C Devices
===========

 I2C (Inter-Integrated Circuit) is a serial bus for communication between peripheral components.
 
 Originally invented by Phillips the I2C protocol is used by thousands of common chips that perform
 a wide range of tasks such as real time clocks, temperature and other sensors, small LCD displays
 and many more.
 
 Each device is assigned a 7bit address which is used by the host (or master) to signal the device
 that a message written to the bus is intended for that device or that the host wants to read data
 from that device. 
 
 A small number of devices and hosts support 10bit addressing which expands the number of available
 addresses. The 10bit address format is defined here https://www.i2c-bus.org/addressing/10-bit-addressing/
 and the reserved addresses are defined on this page https://www.i2c-bus.org/addressing/ 
 
 Speeds range from 10Kbps to 3.4Mbps although the typical speed is either 100Kbps or 400Kbps.
 
 This unit implements the standardized interface for I2C devices and allows reading or writing to
 a specific address, setting a clock rate for the communication and determining device properties.
 
 For the purpose of this interface a device is the I2C controller attached to the local system
 and may be either a master or a slave. Since the protocol does not include any form of enumeration
 or identification the interface does not attempt to represent the devices connected to the bus,
 any driver written to communicate with a connected I2C device should know (or allow configuration
 of) the address to read and write to and the specific message format required for that device.

 The Intel System Management Bus (SMBus) is a variation of the I2C bus and in certain cases the 
 two are compatible with each other.
 
}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit I2C; 

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,SysUtils;

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
const
 {I2C specific constants}
 I2C_NAME_PREFIX = 'I2C';            {Name prefix for I2C Devices}
 I2CSLAVE_NAME_PREFIX = 'I2CSlave';  {Name prefix for I2C Slave Devices}

 {I2C Device Types}
 I2C_TYPE_NONE      = 0;
 I2C_TYPE_MASTER    = 1;
 I2C_TYPE_SLAVE     = 2;
 
 I2C_TYPE_MAX       = 2;
  
 {I2C Type Names}
 I2C_TYPE_NAMES:array[I2C_TYPE_NONE..I2C_TYPE_MAX] of String = (
  'I2C_TYPE_NONE',
  'I2C_TYPE_MASTER',
  'I2C_TYPE_SLAVE');
 
 {I2C Device States}
 I2C_STATE_DISABLED = 0;
 I2C_STATE_ENABLED  = 1;
 
 I2C_STATE_MAX      = 1;
 
 {I2C State Names}
 I2C_STATE_NAMES:array[I2C_STATE_DISABLED..I2C_STATE_MAX] of String = (
  'I2C_STATE_DISABLED',
  'I2C_STATE_ENABLED');
 
 {I2C Device Flags}
 I2C_FLAG_NONE          = $00000000;
 I2C_FLAG_SLAVE         = $00000001; {Device is a slave not a master}
 I2C_FLAG_10BIT         = $00000002; {Device supports 10bit addressing}
 I2C_FLAG_16BIT         = $00000004; {Device supports 16bit addressing}
 
 {I2C logging}
 I2C_LOG_LEVEL_DEBUG     = LOG_LEVEL_DEBUG;  {I2C debugging messages}
 I2C_LOG_LEVEL_INFO      = LOG_LEVEL_INFO;   {I2C informational messages, such as a device being attached or detached}
 I2C_LOG_LEVEL_WARN      = LOG_LEVEL_WARN;   {I2C warning messages}
 I2C_LOG_LEVEL_ERROR     = LOG_LEVEL_ERROR;  {I2C error messages}
 I2C_LOG_LEVEL_NONE      = LOG_LEVEL_NONE;   {No I2C messages}
 
var 
 I2C_DEFAULT_LOG_LEVEL:LongWord = I2C_LOG_LEVEL_DEBUG; {Minimum level for I2C messages.  Only messages with level greater than or equal to this will be printed}
 
var 
 {I2C logging}
 I2C_LOG_ENABLED:Boolean; 
 
{==============================================================================}
type
 {I2C specific types}
 
 {I2C Properties}
 PI2CProperties = ^TI2CProperties;
 TI2CProperties = record
  Flags:LongWord;        {Device flags (eg I2C_FLAG_SLAVE)}
  MaxSize:LongWord;      {Maximum supported data transfer size}
  MinClock:LongWord;     {Minimum supported clock rate}
  MaxClock:LongWord;     {Maximum supported clock rate}
  ClockRate:LongWord;    {Current clock rate}
  SlaveAddress:Word;     {Current slave address}
 end;
 
 {I2C Device}
 PI2CDevice = ^TI2CDevice;
 
 {I2C Enumeration Callback}
 TI2CEnumerate = function(I2C:PI2CDevice;Data:Pointer):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 {I2C Notification Callback}
 TI2CNotification = function(Device:PDevice;Data:Pointer;Notification:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 
 {I2C Device Methods}
 TI2CDeviceStart = function(I2C:PI2CDevice;Rate:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TI2CDeviceStop = function(I2C:PI2CDevice):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 
 TI2CDeviceRead = function(I2C:PI2CDevice;Address:Word;Buffer:Pointer;Size:LongWord;var Count:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TI2CDeviceWrite = function(I2C:PI2CDevice;Address:Word;Buffer:Pointer;Size:LongWord;var Count:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TI2CDeviceWriteRead = function(I2C:PI2CDevice;Address:Word;Initial:Pointer;Len:LongWord;Data:Pointer;Size:LongWord;var Count:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TI2CDeviceWriteWrite = function(I2C:PI2CDevice;Address:Word;Initial:Pointer;Len:LongWord;Data:Pointer;Size:LongWord;var Count:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
  
 TI2CDeviceGetRate = function(I2C:PI2CDevice):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TI2CDeviceSetRate = function(I2C:PI2CDevice;Rate:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 
 TI2CDeviceGetAddress = function(I2C:PI2CDevice):Word;{$IFDEF i386} stdcall;{$ENDIF}
 TI2CDeviceSetAddress = function(I2C:PI2CDevice;Address:Word):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 
 TI2CDeviceGetProperties = function(I2C:PI2CDevice;Properties:PI2CProperties):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 
 TI2CDevice = record
  {Device Properties}
  Device:TDevice;                                 {The Device entry for this I2C}
  {I2C Properties}
  I2CId:LongWord;                                 {Unique Id of this I2C in the I2C table}
  I2CState:LongWord;                              {I2C state (eg I2C_STATE_ENABLED)}
  DeviceStart:TI2CDeviceStart;                    {A Device specific DeviceStart method implementing the standard I2C device interface (Mandatory)}
  DeviceStop:TI2CDeviceStop;                      {A Device specific DeviceStop method implementing the standard I2C device interface (Mandatory)}
  DeviceRead:TI2CDeviceRead;                      {A Device specific DeviceRead method implementing the standard I2C device interface (Mandatory)}
  DeviceWrite:TI2CDeviceWrite;                    {A Device specific DeviceWrite method implementing the standard I2C device interface (Mandatory)}
  DeviceWriteRead:TI2CDeviceWriteRead;            {A Device specific DeviceWriteRead method implementing the standard I2C device interface (Or nil if the default method is suitable)}
  DeviceWriteWrite:TI2CDeviceWriteWrite;          {A Device specific DeviceWriteWrite method implementing the standard I2C device interface (Or nil if the default method is suitable)}
  DeviceGetRate:TI2CDeviceGetRate;                {A Device specific DeviceGetRate method implementing the standard I2C device interface (Or nil if the default method is suitable)}
  DeviceSetRate:TI2CDeviceSetRate;                {A Device specific DeviceSetRate method implementing the standard I2C device interface (Or nil if the default method is suitable)}
  DeviceGetAddress:TI2CDeviceGetAddress;          {A Device specific DeviceGetAddress method implementing the standard I2C device interface (Or nil if the default method is suitable)}
  DeviceSetAddress:TI2CDeviceSetAddress;          {A Device specific DeviceSetAddress method implementing the standard I2C device interface (Or nil if the default method is suitable)}
  DeviceGetProperties:TI2CDeviceGetProperties;    {A Device specific DeviceGetProperties method implementing the standard I2C device interface (Or nil if the default method is suitable)}
  {Statistics Properties}
  ReadCount:LongWord;
  WriteCount:LongWord;
  ReadErrors:LongWord;
  WriteErrors:LongWord;
  {Driver Properties}
  Lock:TMutexHandle;                              {Device lock}
  Wait:TSemaphoreHandle;                          {Read/Write wait event}
  ClockRate:LongWord;                             {Clock rate (Hz)}
  SlaveAddress:Word;                              {Slave address}             
  Properties:TI2CProperties;                      {Device properties}
  {Internal Properties}                                                                        
  Prev:PI2CDevice;                                {Previous entry in I2C table}
  Next:PI2CDevice;                                {Next entry in I2C table}
 end; 
 
{==============================================================================}
{var}
 {I2C specific variables}

{==============================================================================}
{Initialization Functions}
procedure I2CInit;
 
{==============================================================================}
{I2C Functions}
function I2CDeviceStart(I2C:PI2CDevice;Rate:LongWord):LongWord;
function I2CDeviceStop(I2C:PI2CDevice):LongWord;
 
function I2CDeviceRead(I2C:PI2CDevice;Address:Word;Buffer:Pointer;Size:LongWord;var Count:LongWord):LongWord;
function I2CDeviceWrite(I2C:PI2CDevice;Address:Word;Buffer:Pointer;Size:LongWord;var Count:LongWord):LongWord;
function I2CDeviceWriteRead(I2C:PI2CDevice;Address:Word;Initial:Pointer;Len:LongWord;Data:Pointer;Size:LongWord;var Count:LongWord):LongWord;
function I2CDeviceWriteWrite(I2C:PI2CDevice;Address:Word;Initial:Pointer;Len:LongWord;Data:Pointer;Size:LongWord;var Count:LongWord):LongWord;
 
function I2CDeviceGetRate(I2C:PI2CDevice):LongWord;
function I2CDeviceSetRate(I2C:PI2CDevice;Rate:LongWord):LongWord;
 
function I2CDeviceGetAddress(I2C:PI2CDevice):Word;
function I2CDeviceSetAddress(I2C:PI2CDevice;Address:Word):LongWord;

function I2CDeviceProperties(I2C:PI2CDevice;Properties:PI2CProperties):LongWord; inline;
function I2CDeviceGetProperties(I2C:PI2CDevice;Properties:PI2CProperties):LongWord;
  
function I2CDeviceCreate:PI2CDevice;
function I2CDeviceCreateEx(Size:LongWord):PI2CDevice;
function I2CDeviceDestroy(I2C:PI2CDevice):LongWord;

function I2CDeviceRegister(I2C:PI2CDevice):LongWord;
function I2CDeviceDeregister(I2C:PI2CDevice):LongWord;

function I2CDeviceFind(I2CId:LongWord):PI2CDevice;
function I2CDeviceFindByName(const Name:String):PI2CDevice; inline;
function I2CDeviceFindByDescription(const Description:String):PI2CDevice; inline;
function I2CDeviceEnumerate(Callback:TI2CEnumerate;Data:Pointer):LongWord;
 
function I2CDeviceNotification(I2C:PI2CDevice;Callback:TI2CNotification;Data:Pointer;Notification,Flags:LongWord):LongWord;

{==============================================================================}
{I2C Slave Functions}
function I2CSlaveStart(I2C:PI2CDevice):LongWord; inline;
function I2CSlaveStop(I2C:PI2CDevice):LongWord; inline;

function I2CSlaveRead(I2C:PI2CDevice;Buffer:Pointer;Size:LongWord;var Count:LongWord):LongWord; inline;
function I2CSlaveWrite(I2C:PI2CDevice;Buffer:Pointer;Size:LongWord;var Count:LongWord):LongWord; inline;

function I2CSlaveGetAddress(I2C:PI2CDevice):Word; inline;
function I2CSlaveSetAddress(I2C:PI2CDevice;Address:Word):LongWord; inline;

function I2CSlaveGetProperties(I2C:PI2CDevice;Properties:PI2CProperties):LongWord; inline;

function I2CSlaveCreate:PI2CDevice;
function I2CSlaveCreateEx(Size:LongWord):PI2CDevice;
function I2CSlaveDestroy(I2C:PI2CDevice):LongWord; inline;

function I2CSlaveRegister(I2C:PI2CDevice):LongWord; inline;
function I2CSlaveDeregister(I2C:PI2CDevice):LongWord; inline;

function I2CSlaveFind(I2CId:LongWord):PI2CDevice;
function I2CSlaveFindByName(const Name:String):PI2CDevice; inline;
function I2CSlaveFindByDescription(const Description:String):PI2CDevice; inline;

{==============================================================================}
{RTL I2C Functions}
function SysI2CAvailable:Boolean; 
 
function SysI2CStart(Rate:LongWord):LongWord; 
function SysI2CStop:LongWord; 
 
function SysI2CRead(Address:Word;Buffer:Pointer;Size:LongWord;var Count:LongWord):LongWord; 
function SysI2CWrite(Address:Word;Buffer:Pointer;Size:LongWord;var Count:LongWord):LongWord; 
function SysI2CWriteRead(Address:Word;Initial:Pointer;Len:LongWord;Data:Pointer;Size:LongWord;var Count:LongWord):LongWord; 
function SysI2CWriteWrite(Address:Word;Initial:Pointer;Len:LongWord;Data:Pointer;Size:LongWord;var Count:LongWord):LongWord; 

function SysI2CGetRate:LongWord; 
function SysI2CSetRate(Rate:LongWord):LongWord; 
 
function SysI2CGetAddress:Word; 
function SysI2CSetAddress(Address:Word):LongWord; 
 
{==============================================================================}
{I2C Helper Functions}
function I2CGetCount:LongWord;
function I2CDeviceGetDefault:PI2CDevice;
function I2CDeviceSetDefault(I2C:PI2CDevice):LongWord; 

function I2CDeviceCheck(I2C:PI2CDevice):PI2CDevice;

function I2CDeviceIsSlave(I2C:PI2CDevice):Boolean;

function I2CTypeToString(I2CType:LongWord):String;
function I2CStateToString(I2CState:LongWord):String;

function I2CIs7BitAddress(Address:Word):Boolean;
function I2CIs10BitAddress(Address:Word):Boolean;

procedure I2CLog(Level:LongWord;I2C:PI2CDevice;const AText:String);
procedure I2CLogInfo(I2C:PI2CDevice;const AText:String); inline;
procedure I2CLogWarn(I2C:PI2CDevice;const AText:String); inline;
procedure I2CLogError(I2C:PI2CDevice;const AText:String); inline;
procedure I2CLogDebug(I2C:PI2CDevice;const AText:String); inline;
 
{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {I2C specific variables}
 I2CInitialized:Boolean;

 I2CDeviceTable:PI2CDevice;
 I2CDeviceTableLock:TCriticalSectionHandle = INVALID_HANDLE_VALUE;
 I2CDeviceTableCount:LongWord;

 I2CDeviceDefault:PI2CDevice;

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure I2CInit;
{Initialize the I2C unit and I2C device table}

{Note: Called only during system startup}
begin
 {}
 {Check Initialized}
 if I2CInitialized then Exit;
 
 {Initialize Logging}
 I2C_LOG_ENABLED:=(I2C_DEFAULT_LOG_LEVEL <> I2C_LOG_LEVEL_NONE); 
 
 {Initialize I2C Table}
 I2CDeviceTable:=nil;
 I2CDeviceTableLock:=CriticalSectionCreate; 
 I2CDeviceTableCount:=0;
 if I2CDeviceTableLock = INVALID_HANDLE_VALUE then
  begin
   if I2C_LOG_ENABLED then I2CLogError(nil,'Failed to create I2C table lock');
  end;
 I2CDeviceDefault:=nil;
 
 {Register Platform I2C Handlers}
 I2CAvailableHandler:=SysI2CAvailable;
 I2CStartHandler:=SysI2CStart;
 I2CStopHandler:=SysI2CStop;
 I2CReadHandler:=SysI2CRead;
 I2CWriteHandler:=SysI2CWrite;
 I2CWriteReadHandler:=SysI2CWriteRead;
 I2CWriteWriteHandler:=SysI2CWriteWrite;
 I2CGetRateHandler:=SysI2CGetRate;
 I2CSetRateHandler:=SysI2CSetRate;
 I2CGetAddressHandler:=SysI2CGetAddress;
 I2CSetAddressHandler:=SysI2CSetAddress;
 
 I2CInitialized:=True;
end;
 
{==============================================================================}
{==============================================================================}
{I2C Functions}
function I2CDeviceStart(I2C:PI2CDevice;Rate:LongWord):LongWord;
{Start the specified I2C device ready for reading and writing}
{I2C: The I2C device to start}
{Rate: The clock rate to set for the device (0 to use the default rate)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check I2C}
 if I2C = nil then Exit;
 if I2C.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IFDEF I2C_DEBUG}
 if I2C_LOG_ENABLED then I2CLogDebug(I2C,'I2C Device Start (Rate=' + IntToStr(Rate) + ')');
 {$ENDIF}
 
 {Check Disabled}
 Result:=ERROR_SUCCESS;
 if I2C.I2CState <> I2C_STATE_DISABLED then Exit;
 
 if MutexLock(I2C.Lock) = ERROR_SUCCESS then
  begin
   try
    if Assigned(I2C.DeviceStart) then
     begin
      {Call Device Start}
      Result:=I2C.DeviceStart(I2C,Rate);
      if Result <> ERROR_SUCCESS then Exit;
     end
    else
     begin
      Result:=ERROR_INVALID_PARAMETER;
      Exit;
     end;
     
    {Enable Device}
    I2C.I2CState:=I2C_STATE_ENABLED;
    
    {Notify Enable}
    NotifierNotify(@I2C.Device,DEVICE_NOTIFICATION_ENABLE);
    
    Result:=ERROR_SUCCESS;
   finally
    MutexUnlock(I2C.Lock);
   end; 
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;    
end;

{==============================================================================}

function I2CDeviceStop(I2C:PI2CDevice):LongWord;
{Stop the specified I2C device and terminate reading and writing}
{I2C: The I2C device to stop}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check I2C}
 if I2C = nil then Exit;
 if I2C.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IFDEF I2C_DEBUG}
 if I2C_LOG_ENABLED then I2CLogDebug(I2C,'I2C Device Stop');
 {$ENDIF}
 
 {Check Enabled}
 Result:=ERROR_SUCCESS;
 if I2C.I2CState <> I2C_STATE_ENABLED then Exit;
 
 if MutexLock(I2C.Lock) = ERROR_SUCCESS then
  begin
   try
    if Assigned(I2C.DeviceStop) then
     begin
      {Call Device Stop}
      Result:=I2C.DeviceStop(I2C);
      if Result <> ERROR_SUCCESS then Exit;
     end
    else
     begin
      Result:=ERROR_INVALID_PARAMETER;
      Exit;
     end;    
  
    {Disable Device}
    I2C.I2CState:=I2C_STATE_DISABLED;
    
    {Notify Disable}
    NotifierNotify(@I2C.Device,DEVICE_NOTIFICATION_DISABLE);
    
    Result:=ERROR_SUCCESS;
   finally
    MutexUnlock(I2C.Lock);
   end; 
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;    
end;

{==============================================================================}
 
function I2CDeviceRead(I2C:PI2CDevice;Address:Word;Buffer:Pointer;Size:LongWord;var Count:LongWord):LongWord;
{Read data from the specified I2C device}
{I2C: The I2C device to read from}
{Address: The slave address to read from (I2C_ADDRESS_INVALID to use the current address)}
{Buffer: Pointer to a buffer to receive the data}
{Size: The size of the buffer}
{Count: The number of bytes read on return}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 {Setup Result}
 Count:=0;
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Buffer}
 if Buffer = nil then Exit;
 
 {Check I2C}
 if I2C = nil then Exit;
 if I2C.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IFDEF I2C_DEBUG}
 if I2C_LOG_ENABLED then I2CLogDebug(I2C,'I2C Device Read (Address=' + IntToHex(Address,4) + ' Size=' + IntToStr(Size) + ')');
 {$ENDIF}
 
 {Check Enabled}
 Result:=ERROR_NOT_SUPPORTED;
 if I2C.I2CState <> I2C_STATE_ENABLED then Exit;
 
 if MutexLock(I2C.Lock) = ERROR_SUCCESS then
  begin
   if Assigned(I2C.DeviceRead) then
    begin
     {Call Device Read}
     Result:=I2C.DeviceRead(I2C,Address,Buffer,Size,Count);
    end
   else
    begin
     Result:=ERROR_INVALID_PARAMETER;
    end;    
    
   MutexUnlock(I2C.Lock);
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;    
end;

{==============================================================================}

function I2CDeviceWrite(I2C:PI2CDevice;Address:Word;Buffer:Pointer;Size:LongWord;var Count:LongWord):LongWord;
{Write data to the specified I2C device}
{I2C: The I2C device to write to}
{Address: The slave address to write to (I2C_ADDRESS_INVALID to use the current address)}
{Buffer: Pointer to a buffer of data to transmit}
{Size: The size of the buffer}
{Count: The number of bytes written on return}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 {Setup Result}
 Count:=0;
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Buffer}
 if Buffer = nil then Exit;
 
 {Check I2C}
 if I2C = nil then Exit;
 if I2C.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IFDEF I2C_DEBUG}
 if I2C_LOG_ENABLED then I2CLogDebug(I2C,'I2C Device Write (Address=' + IntToHex(Address,4) + ' Size=' + IntToStr(Size) + ')');
 {$ENDIF}
 
 {Check Enabled}
 Result:=ERROR_NOT_SUPPORTED;
 if I2C.I2CState <> I2C_STATE_ENABLED then Exit;
 
 if MutexLock(I2C.Lock) = ERROR_SUCCESS then
  begin
   if Assigned(I2C.DeviceWrite) then
    begin
     {Call Device Write}
     Result:=I2C.DeviceWrite(I2C,Address,Buffer,Size,Count);
    end
   else
    begin
     Result:=ERROR_INVALID_PARAMETER;
    end;    
    
   MutexUnlock(I2C.Lock);
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;    
end;

{==============================================================================}

function I2CDeviceWriteRead(I2C:PI2CDevice;Address:Word;Initial:Pointer;Len:LongWord;Data:Pointer;Size:LongWord;var Count:LongWord):LongWord;
{Write data to and Read data from the specified I2C device in one operation}
{Useful for devices that require a register address specified before a read (eg EEPROM devices)}
{I2C: The I2C device to write to and read from}
{Address: The slave address to write to (I2C_ADDRESS_INVALID to use the current address)}
{Initial: Pointer to the initial buffer to transmit}
{Len: The size of the initial buffer}
{Data: Pointer to a buffer to receive the data}
{Size: The size of the data buffer}
{Count: The number of bytes read on return}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Written:LongWord;
begin
 {}
 {Setup Result}
 Count:=0;
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Buffers}
 if Initial = nil then Exit;
 if Data = nil then Exit;
 
 {Check I2C}
 if I2C = nil then Exit;
 if I2C.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IFDEF I2C_DEBUG}
 if I2C_LOG_ENABLED then I2CLogDebug(I2C,'I2C Device Write Read (Address=' + IntToHex(Address,4) + ' Len=' + IntToStr(Len) + ' Size=' + IntToStr(Size) + ')');
 {$ENDIF}
 
 {Check Enabled}
 Result:=ERROR_NOT_SUPPORTED;
 if I2C.I2CState <> I2C_STATE_ENABLED then Exit;
 
 {Check Slave}
 if (I2C.Device.DeviceFlags and I2C_FLAG_SLAVE) <> 0 then Exit;

 if MutexLock(I2C.Lock) = ERROR_SUCCESS then
  begin
   if Assigned(I2C.DeviceWriteRead) then
    begin
     {Call Device Write Read}
     Result:=I2C.DeviceWriteRead(I2C,Address,Initial,Len,Data,Size,Count);
    end
   else
    begin
     {Default Method}
     if Assigned(I2C.DeviceWrite) and Assigned(I2C.DeviceRead) then
      begin
       Written:=0;

       {Call Device Write}
       Result:=I2C.DeviceWrite(I2C,Address,Initial,Len,Written);
       if Result = ERROR_SUCCESS then
        begin
         {Call Device Read}
         Result:=I2C.DeviceRead(I2C,Address,Data,Size,Count);
        end;
      end
     else
      begin     
       Result:=ERROR_INVALID_PARAMETER;
      end; 
    end;    
    
   MutexUnlock(I2C.Lock);
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;    
end;

{==============================================================================}

function I2CDeviceWriteWrite(I2C:PI2CDevice;Address:Word;Initial:Pointer;Len:LongWord;Data:Pointer;Size:LongWord;var Count:LongWord):LongWord;
{Write 2 data blocks to the specified I2C device in one operation}
{Useful for devices that require a register address specified before a write (eg EEPROM devices)}
{I2C: The I2C device to write to}
{Address: The slave address to write to (I2C_ADDRESS_INVALID to use the current address)}
{Initial: Pointer to the initial buffer to transmit}
{Len: The size of the initial buffer}
{Data: Pointer to a buffer of data to transmit}
{Size: The size of the data buffer}
{Count: The number of bytes of data written on return}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 {Setup Result}
 Count:=0;
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Buffers}
 if Initial = nil then Exit;
 if Data = nil then Exit;
 
 {Check I2C}
 if I2C = nil then Exit;
 if I2C.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IFDEF I2C_DEBUG}
 if I2C_LOG_ENABLED then I2CLogDebug(I2C,'I2C Device Write Write (Address=' + IntToHex(Address,4) + ' Len=' + IntToStr(Len) + ' Size=' + IntToStr(Size) + ')');
 {$ENDIF}
 
 {Check Enabled}
 Result:=ERROR_NOT_SUPPORTED;
 if I2C.I2CState <> I2C_STATE_ENABLED then Exit;
 
 {Check Slave}
 if (I2C.Device.DeviceFlags and I2C_FLAG_SLAVE) <> 0 then Exit;

 if MutexLock(I2C.Lock) = ERROR_SUCCESS then
  begin
   if Assigned(I2C.DeviceWriteWrite) then
    begin
     {Call Device Write Write}
     Result:=I2C.DeviceWriteWrite(I2C,Address,Initial,Len,Data,Size,Count);
    end
   else
    begin
     Result:=ERROR_CALL_NOT_IMPLEMENTED;
    end;    
    
   MutexUnlock(I2C.Lock);
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;    
end;

{==============================================================================}
 
function I2CDeviceGetRate(I2C:PI2CDevice):LongWord;
{Get the clock rate of the specified I2C device}
{I2C: The I2C device to get the clock rate from}
{Return: The clock rate in Hz or 0 on failure}
begin
 {}
 Result:=0;
 
 {Check I2C}
 if I2C = nil then Exit;
 if I2C.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IFDEF I2C_DEBUG}
 if I2C_LOG_ENABLED then I2CLogDebug(I2C,'I2C Device Get Rate');
 {$ENDIF}
 
 {Check Enabled}
 {if I2C.I2CState <> I2C_STATE_ENABLED then Exit;} {Allow when disabled}
 
 {Check Slave}
 if (I2C.Device.DeviceFlags and I2C_FLAG_SLAVE) <> 0 then Exit;

 if MutexLock(I2C.Lock) = ERROR_SUCCESS then
  begin
   if Assigned(I2C.DeviceGetRate) then
    begin
     {Call Device Get Rate}
     Result:=I2C.DeviceGetRate(I2C);
    end
   else
    begin
     {Get Rate}
     Result:=I2C.ClockRate;
    end;  
    
   MutexUnlock(I2C.Lock);
  end;
end;

{==============================================================================}

function I2CDeviceSetRate(I2C:PI2CDevice;Rate:LongWord):LongWord;
{Set the clock rate for the specified I2C device}
{I2C: The I2C device to set the clock rate for}
{Rate: The clock rate to set in Hz}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check I2C}
 if I2C = nil then Exit;
 if I2C.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IFDEF I2C_DEBUG}
 if I2C_LOG_ENABLED then I2CLogDebug(I2C,'I2C Device Set Rate (Rate=' + IntToStr(Rate) + ')');
 {$ENDIF}
 
 {Check Enabled}
 Result:=ERROR_NOT_SUPPORTED;
 {if I2C.I2CState <> I2C_STATE_ENABLED then Exit;} {Allow when disabled}
 
 {Check Slave}
 if (I2C.Device.DeviceFlags and I2C_FLAG_SLAVE) <> 0 then Exit;

 if MutexLock(I2C.Lock) = ERROR_SUCCESS then
  begin
   try
    if Assigned(I2C.DeviceSetRate) then
     begin
      {Call Device Set Rate}
      Result:=I2C.DeviceSetRate(I2C,Rate);
     end
    else
     begin
      {Check Rate}
      if Rate = 0 then Exit;
      
      {Set Rate}
      I2C.ClockRate:=Rate;
      I2C.Properties.ClockRate:=Rate;
      
      Result:=ERROR_SUCCESS;
     end;  
   finally  
    MutexUnlock(I2C.Lock);
   end; 
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;
 
{==============================================================================}

function I2CDeviceGetAddress(I2C:PI2CDevice):Word;
{Get the slave address for the specified I2C device}
{I2C: The I2C device to get the slave address from}
{Return: The slave address or I2C_ADDRESS_INVALID on failure}
begin
 {}
 Result:=I2C_ADDRESS_INVALID;
 
 {Check I2C}
 if I2C = nil then Exit;
 if I2C.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IFDEF I2C_DEBUG}
 if I2C_LOG_ENABLED then I2CLogDebug(I2C,'I2C Device Get Address');
 {$ENDIF}
 
 {Check Enabled}
 {if I2C.I2CState <> I2C_STATE_ENABLED then Exit;} {Allow when disabled}
 
 if MutexLock(I2C.Lock) = ERROR_SUCCESS then
  begin
   if Assigned(I2C.DeviceGetAddress) then
    begin
     {Call Device Get Address}
     Result:=I2C.DeviceGetAddress(I2C);
    end
   else
    begin
     {Get Address}
     Result:=I2C.SlaveAddress;
    end;  
    
   MutexUnlock(I2C.Lock);
  end;
end;

{==============================================================================}

function I2CDeviceSetAddress(I2C:PI2CDevice;Address:Word):LongWord;
{Set the slave address for the specified I2C device}
{I2C: The I2C device to set the slave address for}
{Address: The slave address to set}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check I2C}
 if I2C = nil then Exit;
 if I2C.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IFDEF I2C_DEBUG}
 if I2C_LOG_ENABLED then I2CLogDebug(I2C,'I2C Device Set Address (Address=' + IntToHex(Address,4) + ')');
 {$ENDIF}
 
 {Check Enabled}
 {Result:=ERROR_NOT_SUPPORTED;}
 {if I2C.I2CState <> I2C_STATE_ENABLED then Exit;} {Allow when disabled}
 
 if MutexLock(I2C.Lock) = ERROR_SUCCESS then
  begin
   try
    if Assigned(I2C.DeviceSetAddress) then
     begin
      {Call Device Set Address}
      Result:=I2C.DeviceSetAddress(I2C,Address);
     end
    else
     begin
      {Check Address}
      if Address = I2C_ADDRESS_INVALID then Exit;
      
      {Set Address}
      I2C.SlaveAddress:=Address;
      I2C.Properties.SlaveAddress:=Address;
      
      Result:=ERROR_SUCCESS;
     end;  
   finally  
    MutexUnlock(I2C.Lock);
   end; 
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function I2CDeviceProperties(I2C:PI2CDevice;Properties:PI2CProperties):LongWord; inline;
{Get the properties for the specified I2C device}
{I2C: The I2C device to get properties from}
{Properties: Pointer to a TI2CProperties structure to fill in}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: Replaced by I2CDeviceGetProperties for consistency}
begin
 {}
 Result:=I2CDeviceGetProperties(I2C,Properties);
end;

{==============================================================================}

function I2CDeviceGetProperties(I2C:PI2CDevice;Properties:PI2CProperties):LongWord;
{Get the properties for the specified I2C device}
{I2C: The I2C device to get properties from}
{Properties: Pointer to a TI2CProperties structure to fill in}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Properties}
 if Properties = nil then Exit;
 
 {Check I2C}
 if I2C = nil then Exit;
 if I2C.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IFDEF I2C_DEBUG}
 if I2C_LOG_ENABLED then I2CLogDebug(I2C,'I2C Device Get Properties');
 {$ENDIF}
 
 {Check Enabled}
 {Result:=ERROR_NOT_SUPPORTED;}
 {if I2C.I2CState <> I2C_STATE_ENABLED then Exit;} {Allow when disabled}
 
 if MutexLock(I2C.Lock) = ERROR_SUCCESS then
  begin
   if Assigned(I2C.DeviceGetProperties) then
    begin
     {Call Device Get Properites}
     Result:=I2C.DeviceGetProperties(I2C,Properties);
    end
   else
    begin
     {Get Properties}
     System.Move(I2C.Properties,Properties^,SizeOf(TI2CProperties));
       
     {Return Result}
     Result:=ERROR_SUCCESS;
    end;  
    
   MutexUnlock(I2C.Lock);
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;    
end;

{==============================================================================}

function I2CDeviceCreate:PI2CDevice;
{Create a new I2C entry}
{Return: Pointer to new I2C entry or nil if I2C could not be created}
begin
 {}
 Result:=I2CDeviceCreateEx(SizeOf(TI2CDevice));
end;

{==============================================================================}

function I2CDeviceCreateEx(Size:LongWord):PI2CDevice;
{Create a new I2C entry}
{Size: Size in bytes to allocate for new I2C (Including the I2C entry)}
{Return: Pointer to new I2C entry or nil if I2C could not be created}
begin
 {}
 Result:=nil;
 
 {Check Size}
 if Size < SizeOf(TI2CDevice) then Exit;
 
 {Create I2C}
 Result:=PI2CDevice(DeviceCreateEx(Size));
 if Result = nil then Exit;
 
 {Update Device}
 Result.Device.DeviceBus:=DEVICE_BUS_NONE;   
 Result.Device.DeviceType:=I2C_TYPE_MASTER;
 Result.Device.DeviceFlags:=I2C_FLAG_NONE;
 Result.Device.DeviceData:=nil;

 {Update I2C}
 Result.I2CId:=DEVICE_ID_ANY;
 Result.I2CState:=I2C_STATE_DISABLED;
 Result.DeviceStart:=nil;
 Result.DeviceStop:=nil;
 Result.DeviceRead:=nil;
 Result.DeviceWrite:=nil;
 Result.DeviceGetRate:=nil;
 Result.DeviceSetRate:=nil;
 Result.DeviceGetAddress:=nil;
 Result.DeviceSetAddress:=nil;
 Result.DeviceGetProperties:=nil;
 Result.Lock:=INVALID_HANDLE_VALUE;
 Result.Wait:=INVALID_HANDLE_VALUE;
 Result.ClockRate:=0;
 Result.SlaveAddress:=I2C_ADDRESS_INVALID;
 
 {Create Lock}
 Result.Lock:=MutexCreateEx(False,MUTEX_DEFAULT_SPINCOUNT,MUTEX_FLAG_RECURSIVE);
 if Result.Lock = INVALID_HANDLE_VALUE then
  begin
   if I2C_LOG_ENABLED then I2CLogError(nil,'Failed to create lock for I2C device');
   I2CDeviceDestroy(Result);
   Result:=nil;
   Exit;
  end;
end;

{==============================================================================}

function I2CDeviceDestroy(I2C:PI2CDevice):LongWord;
{Destroy an existing I2C entry}
{I2C: The I2C device to destroy}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check I2C}
 if I2C = nil then Exit;
 if I2C.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check I2C}
 Result:=ERROR_IN_USE;
 if I2CDeviceCheck(I2C) = I2C then Exit;

 {Check State}
 if I2C.Device.DeviceState <> DEVICE_STATE_UNREGISTERED then Exit;
 
 {Destroy Lock}
 if I2C.Lock <> INVALID_HANDLE_VALUE then
  begin
   MutexDestroy(I2C.Lock);
  end;
 
 {Destroy I2C} 
 Result:=DeviceDestroy(@I2C.Device);
end;

{==============================================================================}

function I2CDeviceRegister(I2C:PI2CDevice):LongWord;
{Register a new I2C in the I2C table}
{I2C: The I2C device to register}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Slave:Boolean;
 I2CId:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check I2C}
 if I2C = nil then Exit;
 if I2C.I2CId <> DEVICE_ID_ANY then Exit;
 if I2C.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check Interfaces}
 if not(Assigned(I2C.DeviceStart)) then Exit;
 if not(Assigned(I2C.DeviceStop)) then Exit;
 if not(Assigned(I2C.DeviceRead)) then Exit;
 if not(Assigned(I2C.DeviceWrite)) then Exit;
 
 {Check I2C}
 Result:=ERROR_ALREADY_EXISTS;
 if I2CDeviceCheck(I2C) = I2C then Exit;
 
 {Check State}
 if I2C.Device.DeviceState <> DEVICE_STATE_UNREGISTERED then Exit;
 
 {Insert I2C}
 if CriticalSectionLock(I2CDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Check Slave}
    Slave:=(I2C.Device.DeviceFlags and I2C_FLAG_SLAVE) <> 0;
    if not Slave then
     begin
      {Update I2C}
      I2CId:=0;
      while I2CDeviceFind(I2CId) <> nil do
       begin
        Inc(I2CId);
       end;
      I2C.I2CId:=I2CId;

      {Update Device}
      I2C.Device.DeviceName:=I2C_NAME_PREFIX + IntToStr(I2C.I2CId); 
      I2C.Device.DeviceClass:=DEVICE_CLASS_I2C;
     end
    else
     begin
      {Update I2C}
      I2CId:=0;
      while I2CSlaveFind(I2CId) <> nil do
       begin
        Inc(I2CId);
       end;
      I2C.I2CId:=I2CId;

      {Update Device}
      I2C.Device.DeviceName:=I2CSLAVE_NAME_PREFIX + IntToStr(I2C.I2CId); 
      I2C.Device.DeviceClass:=DEVICE_CLASS_I2C;
     end;

    {Register Device}
    Result:=DeviceRegister(@I2C.Device);
    if Result <> ERROR_SUCCESS then
     begin
      I2C.I2CId:=DEVICE_ID_ANY;
      Exit;
     end; 
    
    {Link I2C}
    if I2CDeviceTable = nil then
     begin
      I2CDeviceTable:=I2C;
     end
    else
     begin
      I2C.Next:=I2CDeviceTable;
      I2CDeviceTable.Prev:=I2C;
      I2CDeviceTable:=I2C;
     end;
 
    {Increment Count}
    Inc(I2CDeviceTableCount);
    
    {Check Default}
    if not(Slave) and (I2CDeviceDefault = nil) then
     begin
      I2CDeviceDefault:=I2C;
     end;
    
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    CriticalSectionUnlock(I2CDeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;

{==============================================================================}

function I2CDeviceDeregister(I2C:PI2CDevice):LongWord;
{Deregister an I2C from the I2C table}
{I2C: The I2C device to deregister}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Prev:PI2CDevice;
 Next:PI2CDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check I2C}
 if I2C = nil then Exit;
 if I2C.I2CId = DEVICE_ID_ANY then Exit;
 if I2C.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check I2C}
 Result:=ERROR_NOT_FOUND;
 if I2CDeviceCheck(I2C) <> I2C then Exit;
 
 {Check State}
 if I2C.Device.DeviceState <> DEVICE_STATE_REGISTERED then Exit;
 
 {Remove I2C}
 if CriticalSectionLock(I2CDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Deregister Device}
    Result:=DeviceDeregister(@I2C.Device);
    if Result <> ERROR_SUCCESS then Exit;
    
    {Unlink I2C}
    Prev:=I2C.Prev;
    Next:=I2C.Next;
    if Prev = nil then
     begin
      I2CDeviceTable:=Next;
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
    Dec(I2CDeviceTableCount);
 
    {Check Default}
    if I2CDeviceDefault = I2C then
     begin
      I2CDeviceDefault:=I2CDeviceTable;
     end;
 
    {Update I2C}
    I2C.I2CId:=DEVICE_ID_ANY;
 
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    CriticalSectionUnlock(I2CDeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;

{==============================================================================}

function I2CDeviceFind(I2CId:LongWord):PI2CDevice;
{Find an I2C device by ID in the I2C table}
{I2CId: The ID number of the I2C device to find}
{Return: Pointer to I2C device entry or nil if not found}
var
 I2C:PI2CDevice;
begin
 {}
 Result:=nil;
 
 {Check Id}
 if I2CId = DEVICE_ID_ANY then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(I2CDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get I2C}
    I2C:=I2CDeviceTable;
    while I2C <> nil do
     begin
      {Check State}
      if I2C.Device.DeviceState = DEVICE_STATE_REGISTERED then
       begin
        {Check Master}
        if (I2C.Device.DeviceFlags and I2C_FLAG_SLAVE) = 0 then
         begin
          {Check Id}
          if I2C.I2CId = I2CId then
           begin
            Result:=I2C;
            Exit;
           end;
         end;
        end; 
       
      {Get Next}
      I2C:=I2C.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(I2CDeviceTableLock);
   end;
  end;
end;
     
{==============================================================================}
     
function I2CDeviceFindByName(const Name:String):PI2CDevice; inline;
{Find an I2C device by name in the device table}
{Name: The name of the I2C device to find (eg I2C0)}
{Return: Pointer to I2C device entry or nil if not found}
begin
 {}
 Result:=PI2CDevice(DeviceFindByName(Name));
end;

{==============================================================================}

function I2CDeviceFindByDescription(const Description:String):PI2CDevice; inline;
{Find an I2C device by description in the device table}
{Description: The description of the I2C to find (eg BCM2837 BSC1 Master I2C)}
{Return: Pointer to I2C device entry or nil if not found}
begin
 {}
 Result:=PI2CDevice(DeviceFindByDescription(Description));
end;
       
{==============================================================================}

function I2CDeviceEnumerate(Callback:TI2CEnumerate;Data:Pointer):LongWord;
{Enumerate all I2C devices in the I2C table}
{Callback: The callback function to call for each I2C device in the table}
{Data: A private data pointer to pass to callback for each I2C device in the table}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 I2C:PI2CDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Callback}
 if not Assigned(Callback) then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(I2CDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get I2C}
    I2C:=I2CDeviceTable;
    while I2C <> nil do
     begin
      {Check State}
      if I2C.Device.DeviceState = DEVICE_STATE_REGISTERED then
       begin
        if Callback(I2C,Data) <> ERROR_SUCCESS then Exit;
       end;
       
      {Get Next}
      I2C:=I2C.Next;
     end;
     
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    CriticalSectionUnlock(I2CDeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;

{==============================================================================}

function I2CDeviceNotification(I2C:PI2CDevice;Callback:TI2CNotification;Data:Pointer;Notification,Flags:LongWord):LongWord;
{Register a notification for I2C device changes}
{Device: The I2C device to notify changes for (Optional, pass nil for all I2C devices)}
{Callback: The function to call when a notification event occurs}
{Data: A private data pointer to pass to callback when a notification event occurs}
{Notification: The events to register for notification of (eg DEVICE_NOTIFICATION_REGISTER)}
{Flags: The flags to control the notification (eg NOTIFIER_FLAG_WORKER)}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check I2C}
 if I2C = nil then
  begin
   Result:=DeviceNotification(nil,DEVICE_CLASS_I2C,Callback,Data,Notification,Flags);
  end
 else
  begin 
   {Check I2C}
   if I2C.Device.Signature <> DEVICE_SIGNATURE then Exit;

   Result:=DeviceNotification(@I2C.Device,DEVICE_CLASS_I2C,Callback,Data,Notification,Flags);
  end; 
end;

{==============================================================================}
{==============================================================================}
{I2C Slave Functions}
function I2CSlaveStart(I2C:PI2CDevice):LongWord; inline;
{Start the specified I2C slave ready for reading and writing}
{I2C: The I2C slave to start}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=I2CDeviceStart(I2C,0);
end;

{==============================================================================}

function I2CSlaveStop(I2C:PI2CDevice):LongWord; inline;
{Stop the specified I2C slave and terminate reading and writing}
{I2C: The I2C slave to stop}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=I2CDeviceStop(I2C);
end;

{==============================================================================}

function I2CSlaveRead(I2C:PI2CDevice;Buffer:Pointer;Size:LongWord;var Count:LongWord):LongWord; inline;
{Read data from the specified I2C slave}
{I2C: The I2C slave to read from}
{Buffer: Pointer to a buffer to receive the data}
{Size: The size of the buffer}
{Count: The number of bytes read on return}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=I2CDeviceRead(I2C,I2C_ADDRESS_INVALID,Buffer,Size,Count);
end;

{==============================================================================}

function I2CSlaveWrite(I2C:PI2CDevice;Buffer:Pointer;Size:LongWord;var Count:LongWord):LongWord; inline;
{Write data to the specified I2C slave}
{I2C: The I2C slave to write to}
{Buffer: Pointer to a buffer of data to transmit}
{Size: The size of the buffer}
{Count: The number of bytes written on return}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=I2CDeviceWrite(I2C,I2C_ADDRESS_INVALID,Buffer,Size,Count);
end;

{==============================================================================}

function I2CSlaveGetAddress(I2C:PI2CDevice):Word; inline;
{Get the address for the specified I2C slave}
{I2C: The I2C slave to get the address from}
{Return: The address or I2C_ADDRESS_INVALID on failure}
begin
 {}
 Result:=I2CDeviceGetAddress(I2C);
end;

{==============================================================================}

function I2CSlaveSetAddress(I2C:PI2CDevice;Address:Word):LongWord; inline;
{Set the address for the specified I2C slave}
{I2C: The I2C slave to set the address for}
{Address: The address to set}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=I2CDeviceSetAddress(I2C,Address);
end;

{==============================================================================}

function I2CSlaveGetProperties(I2C:PI2CDevice;Properties:PI2CProperties):LongWord; inline;
{Get the properties for the specified I2C slave}
{I2C: The I2C slave to get properties from}
{Properties: Pointer to a TI2CProperties structure to fill in}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=I2CDeviceGetProperties(I2C,Properties);
end;

{==============================================================================}

function I2CSlaveCreate:PI2CDevice;
{Create a new I2C slave entry}
{Return: Pointer to new I2C slave entry or nil if I2C could not be created}
begin
 {}
 Result:=I2CSlaveCreateEx(SizeOf(TI2CDevice));
end;

{==============================================================================}

function I2CSlaveCreateEx(Size:LongWord):PI2CDevice;
{Create a new I2C slave entry}
{Size: Size in bytes to allocate for new I2C (Including the I2C slave entry)}
{Return: Pointer to new I2C slave entry or nil if I2C could not be created}
begin
 {}
 Result:=I2CDeviceCreateEx(Size);
 if Result <> nil then
  begin
   {Update Device}
   Result.Device.DeviceType:=I2C_TYPE_SLAVE;
   Result.Device.DeviceFlags:=Result.Device.DeviceFlags or I2C_FLAG_SLAVE; {Don't override defaults}
  end;
end;

{==============================================================================}

function I2CSlaveDestroy(I2C:PI2CDevice):LongWord; inline;
{Destroy an existing I2C slave entry}
{I2C: The I2C slave to destroy}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=I2CDeviceDestroy(I2C);
end;

{==============================================================================}

function I2CSlaveRegister(I2C:PI2CDevice):LongWord; inline;
{Register a new I2C slave in the I2C table}
{I2C: The I2C slave to register}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=I2CDeviceRegister(I2C);
end;

{==============================================================================}

function I2CSlaveDeregister(I2C:PI2CDevice):LongWord; inline;
{Deregister an I2C slave from the I2C table}
{I2C: The I2C slave to deregister}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=I2CDeviceDeregister(I2C);
end;

{==============================================================================}

function I2CSlaveFind(I2CId:LongWord):PI2CDevice;
{Find an I2C slave by ID in the I2C table}
{I2CId: The ID number of the I2C slave to find}
{Return: Pointer to I2C slave entry or nil if not found}
var
 I2C:PI2CDevice;
begin
 {}
 Result:=nil;

 {Check Id}
 if I2CId = DEVICE_ID_ANY then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(I2CDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get I2C}
    I2C:=I2CDeviceTable;
    while I2C <> nil do
     begin
      {Check State}
      if I2C.Device.DeviceState = DEVICE_STATE_REGISTERED then
       begin
        {Check Slave}
        if (I2C.Device.DeviceFlags and I2C_FLAG_SLAVE) <> 0 then
         begin
          {Check Id}
          if I2C.I2CId = I2CId then
           begin
            Result:=I2C;
            Exit;
           end;
         end;
       end;  

      {Get Next}
      I2C:=I2C.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(I2CDeviceTableLock);
   end;
  end;
end;

{==============================================================================}

function I2CSlaveFindByName(const Name:String):PI2CDevice; inline;
{Find an I2C slave by name in the device table}
{Name: The name of the I2C slave to find (eg I2CSlave0)}
{Return: Pointer to I2C slave entry or nil if not found}
begin
 {}
 Result:=PI2CDevice(DeviceFindByName(Name));
end;

{==============================================================================}

function I2CSlaveFindByDescription(const Description:String):PI2CDevice; inline;
{Find an I2C slave by description in the device table}
{Description: The description of the I2C slave to find (eg BCM2837 I2C Slave)}
{Return: Pointer to I2C slave entry or nil if not found}
begin
 {}
 Result:=PI2CDevice(DeviceFindByDescription(Description));
end;

{==============================================================================}
{==============================================================================}
{RTL I2C Functions}
function SysI2CAvailable:Boolean; 
{Check if an I2C device is available}
begin
 {}
 Result:=(I2CDeviceDefault <> nil);
end;
 
{==============================================================================}
 
function SysI2CStart(Rate:LongWord):LongWord; 
{Start the default I2C device ready for reading and writing}
{Rate: The clock rate to set for the device (0 to use the default rate)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 if I2CDeviceDefault = nil then Exit;
 
 Result:=I2CDeviceStart(I2CDeviceDefault,Rate);
end;

{==============================================================================}

function SysI2CStop:LongWord; 
{Stop the default I2C device and terminate reading and writing}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 if I2CDeviceDefault = nil then Exit;
 
 Result:=I2CDeviceStop(I2CDeviceDefault);
end;

{==============================================================================}
 
function SysI2CRead(Address:Word;Buffer:Pointer;Size:LongWord;var Count:LongWord):LongWord; 
{Read data from the default I2C device}
{Address: The slave address to read from (I2C_ADDRESS_INVALID to use the current address)}
{Buffer: Pointer to a buffer to receive the data}
{Size: The size of the buffer}
{Count: The number of bytes read on return}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 {Setup Result}
 Count:=0;
 Result:=ERROR_INVALID_PARAMETER;
 
 if I2CDeviceDefault = nil then Exit;
 
 Result:=I2CDeviceRead(I2CDeviceDefault,Address,Buffer,Size,Count);
end;

{==============================================================================}

function SysI2CWrite(Address:Word;Buffer:Pointer;Size:LongWord;var Count:LongWord):LongWord; 
{Write data to the default I2C device}
{Address: The slave address to write to (I2C_ADDRESS_INVALID to use the current address)}
{Buffer: Pointer to a buffer of data to transmit}
{Size: The size of the buffer}
{Count: The number of bytes written on return}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 {Setup Result}
 Count:=0;
 Result:=ERROR_INVALID_PARAMETER;
 
 if I2CDeviceDefault = nil then Exit;
 
 Result:=I2CDeviceWrite(I2CDeviceDefault,Address,Buffer,Size,Count);
end;

{==============================================================================}

function SysI2CWriteRead(Address:Word;Initial:Pointer;Len:LongWord;Data:Pointer;Size:LongWord;var Count:LongWord):LongWord;  
{Write data to and Read data from the default I2C device in one operation}
{Useful for devices that require a register address specified before a read (eg EEPROM devices)}
{Address: The slave address to write to (I2C_ADDRESS_INVALID to use the current address)}
{Initial: Pointer to the initial buffer to transmit}
{Len: The size of the initial buffer}
{Data: Pointer to a buffer to receive the data}
{Size: The size of the data buffer}
{Count: The number of bytes read on return}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 {Setup Result}
 Count:=0;
 Result:=ERROR_INVALID_PARAMETER;
 
 if I2CDeviceDefault = nil then Exit;
 
 Result:=I2CDeviceWriteRead(I2CDeviceDefault,Address,Initial,Len,Data,Size,Count);
end;

{==============================================================================}

function SysI2CWriteWrite(Address:Word;Initial:Pointer;Len:LongWord;Data:Pointer;Size:LongWord;var Count:LongWord):LongWord; 
{Write 2 data blocks to the default I2C device in one operation}
{Useful for devices that require a register address specified before a write (eg EEPROM devices)}
{Address: The slave address to write to (I2C_ADDRESS_INVALID to use the current address)}
{Initial: Pointer to the initial buffer to transmit}
{Len: The size of the initial buffer}
{Data: Pointer to a buffer of data to transmit}
{Size: The size of the data buffer}
{Count: The number of bytes of data written on return}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 {Setup Result}
 Count:=0;
 Result:=ERROR_INVALID_PARAMETER;
 
 if I2CDeviceDefault = nil then Exit;
 
 Result:=I2CDeviceWriteWrite(I2CDeviceDefault,Address,Initial,Len,Data,Size,Count);
end;

{==============================================================================}
 
function SysI2CGetRate:LongWord; 
{Get the clock rate of the default I2C device}
{Return: The clock rate in Hz or 0 on failure}
begin
 {}
 Result:=0;
 
 if I2CDeviceDefault = nil then Exit;
 
 Result:=I2CDeviceGetRate(I2CDeviceDefault);
end;

{==============================================================================}

function SysI2CSetRate(Rate:LongWord):LongWord; 
{Set the clock rate for the default I2C device}
{Rate: The clock rate to set in Hz}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 if I2CDeviceDefault = nil then Exit;
 
 Result:=I2CDeviceSetRate(I2CDeviceDefault,Rate);
end;

{==============================================================================}
 
function SysI2CGetAddress:Word; 
{Get the slave address for the default I2C device}
{Return: The slave address or I2C_ADDRESS_INVALID on failure}
begin
 {}
 Result:=I2C_ADDRESS_INVALID;
 
 if I2CDeviceDefault = nil then Exit;
 
 Result:=I2CDeviceGetAddress(I2CDeviceDefault);
end;

{==============================================================================}

function SysI2CSetAddress(Address:Word):LongWord; 
{Set the slave address for the default I2C device}
{Address: The slave address to set}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 if I2CDeviceDefault = nil then Exit;
 
 Result:=I2CDeviceSetAddress(I2CDeviceDefault,Address);
end;

{==============================================================================}
{==============================================================================}
{I2C Helper Functions}
function I2CGetCount:LongWord;
{Get the current I2C count}
begin
 {}
 Result:=I2CDeviceTableCount;
end;

{==============================================================================}

function I2CDeviceGetDefault:PI2CDevice;
{Get the current default I2C device}
begin
 {}
 Result:=I2CDeviceDefault;
end;

{==============================================================================}

function I2CDeviceSetDefault(I2C:PI2CDevice):LongWord; 
{Set the current default I2C device}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check I2C}
 if I2C = nil then Exit;
 if I2C.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(I2CDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Check I2C}
    if I2CDeviceCheck(I2C) <> I2C then Exit;
    
    {Set I2C Default}
    I2CDeviceDefault:=I2C;
    
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    CriticalSectionUnlock(I2CDeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function I2CDeviceCheck(I2C:PI2CDevice):PI2CDevice;
{Check if the supplied I2C is in the I2C table}
var
 Current:PI2CDevice;
begin
 {}
 Result:=nil;
 
 {Check I2C}
 if I2C = nil then Exit;
 if I2C.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(I2CDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get I2C}
    Current:=I2CDeviceTable;
    while Current <> nil do
     begin
      {Check I2C}
      if Current = I2C then
       begin
        Result:=I2C;
        Exit;
       end;
      
      {Get Next}
      Current:=Current.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(I2CDeviceTableLock);
   end;
  end;
end;

{==============================================================================}

function I2CDeviceIsSlave(I2C:PI2CDevice):Boolean;
{Check if the supplied I2C is a slave device}
begin
 {}
 Result:=False;

 {Check I2C}
 if I2C = nil then Exit;
 if I2C.Device.Signature <> DEVICE_SIGNATURE then Exit;

 Result:=(I2C.Device.DeviceFlags and I2C_FLAG_SLAVE) <> 0;
end;

{==============================================================================}

function I2CTypeToString(I2CType:LongWord):String;
{Convert an I2C type value to a string}
begin
 {}
 Result:='I2C_TYPE_UNKNOWN';
 
 if I2CType <= I2C_TYPE_MAX then
  begin
   Result:=I2C_TYPE_NAMES[I2CType];
  end;
end;

{==============================================================================}

function I2CStateToString(I2CState:LongWord):String;
{Convert an I2C state value to a string}
begin
 {}
 Result:='I2C_STATE_UNKNOWN';
 
 if I2CState <= I2C_STATE_MAX then
  begin
   Result:=I2C_STATE_NAMES[I2CState];
  end;
end;

{==============================================================================}

function I2CIs7BitAddress(Address:Word):Boolean;
{Determine if the supplied address is a 7bit address}
begin
 {}
 Result:=False;
 
 {Check for bits outside of the 7bit range (10bit = 0x3FF / 7bit = 0x7F)}
 if (Address and $0380) <> 0 then Exit;
 
 {Check for bits in the reserved ranges (1111XXX and 0000XXX)}
 if (Address and $78) = $78 then Exit;
 if (Address <= $07) then Exit;
 
 Result:=True;
end;

{==============================================================================}

function I2CIs10BitAddress(Address:Word):Boolean;
{Determine if the supplied address is a 10bit address}
begin
 {}
 Result:=True;

 {Check for bits outside of the 7bit range (10bit = 0x3FF / 7bit = 0x7F)}
 if (Address and $0380) <> 0 then Exit;

 {Check for bits in the reserved ranges (1111XXX and 0000XXX)}
 if (Address and $78) = $78 then Exit;
 if (Address <= $07) then Exit;

 Result:=False;
end;

{==============================================================================}

procedure I2CLog(Level:LongWord;I2C:PI2CDevice;const AText:String);
var
 WorkBuffer:String;
begin
 {}
 {Check Level}
 if Level < I2C_DEFAULT_LOG_LEVEL then Exit;
 
 WorkBuffer:='';
 {Check Level}
 if Level = I2C_LOG_LEVEL_DEBUG then
  begin
   WorkBuffer:=WorkBuffer + '[DEBUG] ';
  end
 else if Level = I2C_LOG_LEVEL_WARN then
  begin
   WorkBuffer:=WorkBuffer + '[WARN] ';
  end
 else if Level = I2C_LOG_LEVEL_ERROR then
  begin
   WorkBuffer:=WorkBuffer + '[ERROR] ';
  end;
 
 {Add Prefix}
 WorkBuffer:=WorkBuffer + 'I2C: ';
 
 {Check I2C}
 if I2C <> nil then
  begin
   WorkBuffer:=WorkBuffer + I2C_NAME_PREFIX + IntToStr(I2C.I2CId) + ': ';
  end;

 {Output Logging}  
 LoggingOutputEx(LOGGING_FACILITY_I2C,LogLevelToLoggingSeverity(Level),'I2C',WorkBuffer + AText);
end;

{==============================================================================}

procedure I2CLogInfo(I2C:PI2CDevice;const AText:String); inline;
begin
 {}
 I2CLog(I2C_LOG_LEVEL_INFO,I2C,AText);
end;

{==============================================================================}

procedure I2CLogWarn(I2C:PI2CDevice;const AText:String); inline;
begin
 {}
 I2CLog(I2C_LOG_LEVEL_WARN,I2C,AText);
end;

{==============================================================================}

procedure I2CLogError(I2C:PI2CDevice;const AText:String); inline;
begin
 {}
 I2CLog(I2C_LOG_LEVEL_ERROR,I2C,AText);
end;

{==============================================================================}

procedure I2CLogDebug(I2C:PI2CDevice;const AText:String); inline;
begin
 {}
 I2CLog(I2C_LOG_LEVEL_DEBUG,I2C,AText);
end;

{==============================================================================}
{==============================================================================}

initialization
 I2CInit;

{==============================================================================}
 
finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.