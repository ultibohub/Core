{
Ultibo UART interface unit.

Copyright (C) 2015 - SoftOz Pty Ltd.

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


UART Devices
============

 UART (Universal Asynchronous Receiver Transmitter) devices represent the industry standard serial
 communications devices that are available on almost every system.
 
 This unit implements the framework for UART devices and provides a standardized API to allow driver
 specific implementation to be abstracted. All UART devices are also represented as a serial device
 and this unit handles the relationship between the two devices.
 
 Each UART device returns a set of properties that describe the capabilities of the device and includes
 a set of flags that indicate what features are supported.
 
 Reads and writes to UART devices are unbuffered and simply pass raw data to and from the caller unless
 the serial interface is used instead which includes buffering of transmitted and received data.

 Both reads and writes allow for non blocking so that a caller can avoid waiting for received data to be
 available or the device to be ready to transmit
 
}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit UART; 

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,Serial,SysUtils;

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
const
 {UART specific constants}
 UART_NAME_PREFIX = 'UART';  {Name prefix for UART Devices}

 UART_PUSH_TIMEOUT = SERIAL_PUSH_TIMEOUT; {Timeout (Milliseconds) for Push RX/TX (Implementation specific)}
 
 {UART Device Types}
 UART_TYPE_NONE      = 0;
 UART_TYPE_8250      = 1; {8250 UART and similar variants (Differences are handled by driver)}
 UART_TYPE_16550     = 2; {16550 UART and similar variants (eg 16550A) (Differences are handled by driver)}
 UART_TYPE_16650     = 3; {16650 UART and similar variants (eg 16C650) (Differences are handled by driver)}
 
 {UART Device Modes}
 UART_MODE_NONE      = 0;
 UART_MODE_UART      = 1; {The UART was opened as a UART device and all reads and writes are direct}
 UART_MODE_SERIAL    = 2; {The UART was opened as a Serial device so reads and writes are being buffered}
 
 {UART Device States}
 UART_STATE_DISABLED = 0;
 UART_STATE_ENABLED  = 1;
 
 {UART Device Flags}
 UART_FLAG_NONE         = SERIAL_FLAG_NONE;
 UART_FLAG_DATA_8BIT    = SERIAL_FLAG_DATA_8BIT;    {Device supports 8 data bits}
 UART_FLAG_DATA_7BIT    = SERIAL_FLAG_DATA_7BIT;    {Device supports 7 data bits}
 UART_FLAG_DATA_6BIT    = SERIAL_FLAG_DATA_6BIT;    {Device supports 6 data bits}
 UART_FLAG_DATA_5BIT    = SERIAL_FLAG_DATA_5BIT;    {Device supports 5 data bits}
 UART_FLAG_STOP_1BIT    = SERIAL_FLAG_STOP_1BIT;    {Device supports 1 stop bit}
 UART_FLAG_STOP_2BIT    = SERIAL_FLAG_STOP_2BIT;    {Device supports 2 stop bits}
 UART_FLAG_STOP_1BIT5   = SERIAL_FLAG_STOP_1BIT5;   {Device supports 1.5 stop bits}
 UART_FLAG_PARITY_ODD   = SERIAL_FLAG_PARITY_ODD;   {Device supports odd parity}
 UART_FLAG_PARITY_EVEN  = SERIAL_FLAG_PARITY_EVEN;  {Device supports even parity}
 UART_FLAG_PARITY_MARK  = SERIAL_FLAG_PARITY_MARK;  {Device supports mark parity}
 UART_FLAG_PARITY_SPACE = SERIAL_FLAG_PARITY_SPACE; {Device supports space parity}
 UART_FLAG_FLOW_RTS_CTS = SERIAL_FLAG_FLOW_RTS_CTS; {Device supports RTS/CTS flow control}
 UART_FLAG_FLOW_DSR_DTR = SERIAL_FLAG_FLOW_DSR_DTR; {Device supports DSR/DTR flow control}
 UART_FLAG_PUSH_RX      = SERIAL_FLAG_PUSH_RX;      {Device requires pushed receive (Implementation specific)}
 UART_FLAG_PUSH_TX      = SERIAL_FLAG_PUSH_TX;      {Device requires pushed transmit (Implementation specific)}
 
 {UART Read Flags}
 UART_READ_NONE      = SERIAL_READ_NONE;
 UART_READ_NON_BLOCK = SERIAL_READ_NON_BLOCK; {Do not block when receiving, if the FIFO is empty return immediately}
 
 {UART Write Flags}
 UART_WRITE_NONE      = SERIAL_WRITE_NONE;
 UART_WRITE_NON_BLOCK = SERIAL_WRITE_NON_BLOCK; {Do not block when transmitting, if the FIFO is full return immediately}
 
 {UART Wait Directions}
 UART_WAIT_NONE     = SERIAL_WAIT_NONE;
 UART_WAIT_RECEIVE  = SERIAL_WAIT_RECEIVE;  {Wait for data to be available in the receive FIFO}
 UART_WAIT_TRANSMIT = SERIAL_WAIT_TRANSMIT; {Wait for space to be available in the transmit FIFO}
 
 {UART Status Flags}
 UART_STATUS_NONE          = SERIAL_STATUS_NONE;
 UART_STATUS_RTS           = SERIAL_STATUS_RTS;
 UART_STATUS_CTS           = SERIAL_STATUS_CTS;
 UART_STATUS_DSR           = SERIAL_STATUS_DSR;
 UART_STATUS_DTR           = SERIAL_STATUS_DTR;
 UART_STATUS_RX_FULL       = SERIAL_STATUS_RX_FULL;
 UART_STATUS_RX_EMPTY      = SERIAL_STATUS_RX_EMPTY;
 UART_STATUS_TX_FULL       = SERIAL_STATUS_TX_FULL;
 UART_STATUS_TX_EMPTY      = SERIAL_STATUS_TX_EMPTY;
 UART_STATUS_BUSY          = SERIAL_STATUS_BUSY;
 UART_STATUS_BREAK_ERROR   = SERIAL_STATUS_BREAK_ERROR;
 UART_STATUS_PARITY_ERROR  = SERIAL_STATUS_PARITY_ERROR;
 UART_STATUS_FRAMING_ERROR = SERIAL_STATUS_FRAMING_ERROR;
 UART_STATUS_OVERRUN_ERROR = SERIAL_STATUS_OVERRUN_ERROR;
 UART_STATUS_DCD           = SERIAL_STATUS_DCD;
 UART_STATUS_RI            = SERIAL_STATUS_RI;
 
 {UART logging}
 UART_LOG_LEVEL_DEBUG     = LOG_LEVEL_DEBUG;  {UART debugging messages}
 UART_LOG_LEVEL_INFO      = LOG_LEVEL_INFO;   {UART informational messages, such as a device being attached or detached}
 UART_LOG_LEVEL_WARN      = LOG_LEVEL_WARN;   {UART warning messages}
 UART_LOG_LEVEL_ERROR     = LOG_LEVEL_ERROR;  {UART error messages}
 UART_LOG_LEVEL_NONE      = LOG_LEVEL_NONE;   {No UART messages}
 
var 
 UART_DEFAULT_LOG_LEVEL:LongWord = UART_LOG_LEVEL_DEBUG; {Minimum level for UART messages.  Only messages with level greater than or equal to this will be printed}
 
var 
 {UART logging}
 UART_LOG_ENABLED:Boolean; 
 
{==============================================================================}
type
 {UART specific types}
 
 {UART Properties}
 PUARTProperties = ^TUARTProperties;
 TUARTProperties = record
  Flags:LongWord;        {Device flags (eg UART_FLAG_DATA_8BIT)}
  MinRate:LongWord;      {Minimum supported baud rate}
  MaxRate:LongWord;      {Maximum supported baud rate}
  BaudRate:LongWord;     {Current baud rate setting}
  DataBits:LongWord;     {Current data bits setting}
  StopBits:LongWord;     {Current stop bits setting}
  Parity:LongWord;       {Current parity setting} 
  FlowControl:LongWord;  {Current flow control setting}
 end;
 
 {UART Device}
 PUARTDevice = ^TUARTDevice;
 
 {UART Enumeration Callback}
 TUARTEnumerate = function(UART:PUARTDevice;Data:Pointer):LongWord;
 {UART Notification Callback}
 TUARTNotification = function(Device:PDevice;Data:Pointer;Notification:LongWord):LongWord;
 
 {UART Device Methods}
 TUARTDeviceOpen = function(UART:PUARTDevice;BaudRate,DataBits,StopBits,Parity,FlowControl:LongWord):LongWord;
 TUARTDeviceClose = function(UART:PUARTDevice):LongWord;
 
 TUARTDeviceRead = function(UART:PUARTDevice;Buffer:Pointer;Size,Flags:LongWord;var Count:LongWord):LongWord;
 TUARTDeviceWrite = function(UART:PUARTDevice;Buffer:Pointer;Size,Flags:LongWord;var Count:LongWord):LongWord;
 
 TUARTDeviceWait = function(UART:PUARTDevice;Direction,Timeout:LongWord):LongWord;
 
 TUARTDeviceGetStatus = function(UART:PUARTDevice):LongWord;
 TUARTDeviceSetStatus = function(UART:PUARTDevice;Status:LongWord):LongWord;
 
 TUARTDeviceGetProperties = function(UART:PUARTDevice;Properties:PUARTProperties):LongWord;
 TUARTDeviceSetProperties = function(UART:PUARTDevice;Properties:PUARTProperties):LongWord;
 
 TUARTDevice = record
  {Device Properties}
  Device:TDevice;                                 {The Device entry for this UART}
  {UART Properties}
  UARTId:LongWord;                                {Unique Id of this UART in the UART table}
  UARTMode:LongWord;                              {UART mode (eg UART_MODE_SERIAL)}
  UARTState:LongWord;                             {UART state (eg UART_STATE_ENABLED)}
  UARTStatus:LongWord;                            {UART status (eg UART_STATUS_RX_FULL)(May not be real time status depending on the driver)}
  DeviceOpen:TUARTDeviceOpen;                     {A Device specific DeviceOpen method implementing the standard UART device interface (Mandatory)}
  DeviceClose:TUARTDeviceClose;                   {A Device specific DeviceClose method implementing the standard UART device interface (Mandatory)}
  DeviceRead:TUARTDeviceRead;                     {A Device specific DeviceRead method implementing the standard UART device interface (Mandatory)}
  DeviceWrite:TUARTDeviceWrite;                   {A Device specific DeviceWrite method implementing the standard UART device interface (Mandatory)}
  DeviceWait:TUARTDeviceWait;                     {A Device specific DeviceWait method implementing the standard UART device interface (Or nil if the default method is suitable)}
  DeviceGetStatus:TUARTDeviceGetStatus;           {A Device specific DeviceGetStatus method implementing the standard UART device interface (Or nil if the default method is suitable)}
  DeviceSetStatus:TUARTDeviceSetStatus;           {A Device specific DeviceSetStatus method implementing the standard UART device interface (Optional)}
  DeviceGetProperties:TUARTDeviceGetProperties;   {A Device specific DeviceGetProperties method implementing the standard UART device interface (Or nil if the default method is suitable)}
  DeviceSetProperties:TUARTDeviceSetProperties;   {A Device specific DeviceSetProperties method implementing the standard UART device interface (Or nil if the default method is suitable)}
  {Driver Properties}
  Lock:TMutexHandle;                              {Device lock}
  ReceiveWait:TEventHandle;                       {Read wait event}
  TransmitWait:TEventHandle;                      {Write wait event}
  Properties:TUARTProperties;                     {Device properties}
  {Serial Properties}
  Serial:PSerialDevice;                           {The Serial device represented by this UART}
  {Statistics Properties}
  ReceiveCount:LongWord;
  ReceiveErrors:LongWord;
  TransmitCount:LongWord;
  TransmitErrors:LongWord;
  {Internal Properties}                                                                        
  Prev:PUARTDevice;                               {Previous entry in UART table}
  Next:PUARTDevice;                               {Next entry in UART table}
 end; 
 
{==============================================================================}
{var}
 {UART specific variables}

{==============================================================================}
{Initialization Functions}
procedure UARTInit;
 
{==============================================================================}
{UART Functions}
function UARTDeviceOpen(UART:PUARTDevice;BaudRate,DataBits,StopBits,Parity,FlowControl:LongWord):LongWord;
function UARTDeviceClose(UART:PUARTDevice):LongWord;
 
function UARTDeviceRead(UART:PUARTDevice;Buffer:Pointer;Size,Flags:LongWord;var Count:LongWord):LongWord;
function UARTDeviceWrite(UART:PUARTDevice;Buffer:Pointer;Size,Flags:LongWord;var Count:LongWord):LongWord;
 
function UARTDeviceWait(UART:PUARTDevice;Direction,Timeout:LongWord):LongWord;
 
function UARTDeviceStatus(UART:PUARTDevice):LongWord; inline;
function UARTDeviceGetStatus(UART:PUARTDevice):LongWord;
function UARTDeviceSetStatus(UART:PUARTDevice;Status:LongWord):LongWord;

function UARTDeviceProperties(UART:PUARTDevice;Properties:PUARTProperties):LongWord; inline;
function UARTDeviceGetProperties(UART:PUARTDevice;Properties:PUARTProperties):LongWord;
function UARTDeviceSetProperties(UART:PUARTDevice;Properties:PUARTProperties):LongWord;
  
function UARTDeviceCreate:PUARTDevice;
function UARTDeviceCreateEx(Size:LongWord):PUARTDevice;
function UARTDeviceDestroy(UART:PUARTDevice):LongWord;

function UARTDeviceRegister(UART:PUARTDevice):LongWord;
function UARTDeviceDeregister(UART:PUARTDevice):LongWord;

function UARTDeviceFind(UARTId:LongWord):PUARTDevice;
function UARTDeviceFindByName(const Name:String):PUARTDevice; inline;
function UARTDeviceFindByDescription(const Description:String):PUARTDevice; inline;
function UARTDeviceEnumerate(Callback:TUARTEnumerate;Data:Pointer):LongWord;
 
function UARTDeviceNotification(UART:PUARTDevice;Callback:TUARTNotification;Data:Pointer;Notification,Flags:LongWord):LongWord;

{==============================================================================}
{UART Serial Functions}
function UARTSerialDeviceOpen(Serial:PSerialDevice;BaudRate,DataBits,StopBits,Parity,FlowControl,ReceiveDepth,TransmitDepth:LongWord):LongWord;
function UARTSerialDeviceClose(Serial:PSerialDevice):LongWord;

function UARTSerialDeviceRead(Serial:PSerialDevice;Buffer:Pointer;Size,Flags:LongWord;var Count:LongWord):LongWord;
function UARTSerialDeviceWrite(Serial:PSerialDevice;Buffer:Pointer;Size,Flags:LongWord;var Count:LongWord):LongWord;

function UARTSerialDeviceWait(Serial:PSerialDevice;Direction,Timeout:LongWord;var Count:LongWord):LongWord;

function UARTSerialDeviceStatus(Serial:PSerialDevice):LongWord; inline;
function UARTSerialDeviceGetStatus(Serial:PSerialDevice):LongWord;
function UARTSerialDeviceSetStatus(Serial:PSerialDevice;Status:LongWord):LongWord;

function UARTSerialDeviceGetProperties(Serial:PSerialDevice;Properties:PSerialProperties):LongWord;
 
{==============================================================================}
{UART Helper Functions}
function UARTGetCount:LongWord; inline;
function UARTDeviceGetDefault:PUARTDevice; inline;
function UARTDeviceSetDefault(UART:PUARTDevice):LongWord; 

function UARTDeviceCheck(UART:PUARTDevice):PUARTDevice;

procedure UARTLog(Level:LongWord;UART:PUARTDevice;const AText:String);
procedure UARTLogInfo(UART:PUARTDevice;const AText:String); inline;
procedure UARTLogWarn(UART:PUARTDevice;const AText:String); inline;
procedure UARTLogError(UART:PUARTDevice;const AText:String); inline;
procedure UARTLogDebug(UART:PUARTDevice;const AText:String); inline;
 
{==============================================================================}
{UART Serial Helper Functions}
function UARTSerialDeviceReceive(UART:PUARTDevice):LongWord;
function UARTSerialDeviceTransmit(UART:PUARTDevice):LongWord;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {UART specific variables}
 UARTInitialized:Boolean;

 UARTDeviceTable:PUARTDevice;
 UARTDeviceTableLock:TCriticalSectionHandle = INVALID_HANDLE_VALUE;
 UARTDeviceTableCount:LongWord;

 UARTDeviceDefault:PUARTDevice;

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure UARTInit;
{Initialize the UART unit and UART device table}

{Note: Called only during system startup}
begin
 {}
 {Check Initialized}
 if UARTInitialized then Exit;
 
 {Initialize Logging}
 UART_LOG_ENABLED:=(UART_DEFAULT_LOG_LEVEL <> UART_LOG_LEVEL_NONE); 
 
 {Initialize UART Table}
 UARTDeviceTable:=nil;
 UARTDeviceTableLock:=CriticalSectionCreate; 
 UARTDeviceTableCount:=0;
 if UARTDeviceTableLock = INVALID_HANDLE_VALUE then
  begin
   if UART_LOG_ENABLED then UARTLogError(nil,'Failed to create UART table lock');
  end;
 UARTDeviceDefault:=nil;
 
 UARTInitialized:=True;
end;
 
{==============================================================================}
{==============================================================================}
{UART Functions}
function UARTDeviceOpen(UART:PUARTDevice;BaudRate,DataBits,StopBits,Parity,FlowControl:LongWord):LongWord;
{Open a UART device ready for sending and receiving}
{UART: The UART device to open}
{BaudRate: Baud rate for the connection (eg 9600, 57600, 115200 etc}
{DataBits: Size of the data (eg SERIAL_DATA_8BIT)}
{StopBits: Number of stop bits (eg SERIAL_STOP_1BIT)}
{Parity: Parity type for the data (eg SERIAL_PARITY_NONE)}
{FlowControl: Flow control for the connection (eg SERIAL_FLOW_NONE)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check UART}
 if UART = nil then Exit;
 if UART.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IFDEF UART_DEBUG}
 if UART_LOG_ENABLED then UARTLogDebug(UART,'UART Device Open (BaudRate=' + IntToStr(BaudRate) + ' DataBits=' + IntToStr(DataBits) + ' StopBits=' + IntToStr(StopBits) + ' Parity=' + SerialParityToString(Parity) + ' FlowControl=' + SerialFlowControlToString(FlowControl) + ')');
 {$ENDIF}
 
 {Check Mode}
 Result:=ERROR_INVALID_FUNCTION;
 if UART.UARTMode = UART_MODE_SERIAL then Exit;
 
 {Check Disabled}
 Result:=ERROR_SUCCESS;
 if UART.UARTState <> UART_STATE_DISABLED then Exit;
 
 if MutexLock(UART.Lock) = ERROR_SUCCESS then
  begin
   try
    if Assigned(UART.DeviceOpen) then
     begin
      {Set Mode}
      UART.UARTMode:=UART_MODE_UART;
     
      {Call Device Open}
      Result:=UART.DeviceOpen(UART,BaudRate,DataBits,StopBits,Parity,FlowControl);
      if Result <> ERROR_SUCCESS then
       begin
        {Reset Mode}
        UART.UARTMode:=UART_MODE_NONE;
        Exit;
       end; 
     end
    else
     begin    
      Result:=ERROR_INVALID_PARAMETER;
      Exit;
     end; 
 
    {Enable Device}
    UART.UARTState:=UART_STATE_ENABLED;
    
    {Notify Enable}
    NotifierNotify(@UART.Device,DEVICE_NOTIFICATION_ENABLE);
    
    Result:=ERROR_SUCCESS;
   finally
    MutexUnlock(UART.Lock);
   end; 
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;    
end;

{==============================================================================}

function UARTDeviceClose(UART:PUARTDevice):LongWord;
{Close a UART device and terminate sending and receiving}
{UART: The UART device to close}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check UART}
 if UART = nil then Exit;
 if UART.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IFDEF UART_DEBUG}
 if UART_LOG_ENABLED then UARTLogDebug(UART,'UART Device Close');
 {$ENDIF}
 
 {Check Mode}
 Result:=ERROR_INVALID_FUNCTION;
 if UART.UARTMode = UART_MODE_SERIAL then Exit;
 
 {Check Enabled}
 Result:=ERROR_SUCCESS;
 if UART.UARTState <> UART_STATE_ENABLED then Exit;
 
 if MutexLock(UART.Lock) = ERROR_SUCCESS then
  begin
   try
    if Assigned(UART.DeviceClose) then
     begin
      {Call Device Close}
      Result:=UART.DeviceClose(UART);
      if Result <> ERROR_SUCCESS then Exit;
     end
    else
     begin
      Result:=ERROR_INVALID_PARAMETER;
      Exit;
     end;    
   
    {Reset Mode}
    UART.UARTMode:=UART_MODE_NONE;
   
    {Disable Device}
    UART.UARTState:=UART_STATE_DISABLED;
    
    {Notify Disable}
    NotifierNotify(@UART.Device,DEVICE_NOTIFICATION_DISABLE);
    
    Result:=ERROR_SUCCESS;
   finally
    MutexUnlock(UART.Lock);
   end; 
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;    
end;

{==============================================================================}
 
function UARTDeviceRead(UART:PUARTDevice;Buffer:Pointer;Size,Flags:LongWord;var Count:LongWord):LongWord;
{Read data from a UART device}
{UART: The UART device to read from}
{Buffer: Pointer to a buffer to receive the data}
{Size: The size of the buffer}
{Flags: The flags to control reading (eg UART_READ_NON_BLOCK)}
{Count: The number of bytes read on return}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 {Setup Result}
 Count:=0;
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Buffer}
 if Buffer = nil then Exit;
 
 {Check UART}
 if UART = nil then Exit;
 if UART.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IFDEF UART_DEBUG}
 if UART_LOG_ENABLED then UARTLogDebug(UART,'UART Device Read (Size=' + IntToStr(Size) + ')');
 {$ENDIF}
 
 {Check Mode}
 Result:=ERROR_INVALID_FUNCTION;
 if UART.UARTMode = UART_MODE_SERIAL then Exit;
 
 {Check Enabled}
 Result:=ERROR_NOT_SUPPORTED;
 if UART.UARTState <> UART_STATE_ENABLED then Exit;
 
 if MutexLock(UART.Lock) = ERROR_SUCCESS then
  begin
   if Assigned(UART.DeviceRead) then
    begin
     {Call Device Read}
     Result:=UART.DeviceRead(UART,Buffer,Size,Flags,Count);
    end
   else
    begin
     Result:=ERROR_INVALID_PARAMETER;
    end;    
    
   MutexUnlock(UART.Lock);
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;    
end;

{==============================================================================}

function UARTDeviceWrite(UART:PUARTDevice;Buffer:Pointer;Size,Flags:LongWord;var Count:LongWord):LongWord;
{Write data to a UART device}
{UART: The UART device to write to}
{Buffer: Pointer to a buffer of data to transmit}
{Size: The size of the buffer}
{Flags: The flags to control writing (eg UART_WRITE_NON_BLOCK)}
{Count: The number of bytes written on return}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 {Setup Result}
 Count:=0;
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Buffer}
 if Buffer = nil then Exit;
 
 {Check UART}
 if UART = nil then Exit;
 if UART.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IFDEF UART_DEBUG}
 if UART_LOG_ENABLED then UARTLogDebug(UART,'UART Device Write (Size=' + IntToStr(Size) + ')');
 {$ENDIF}
 
 {Check Mode}
 Result:=ERROR_INVALID_FUNCTION;
 if UART.UARTMode = UART_MODE_SERIAL then Exit;
 
 {Check Enabled}
 Result:=ERROR_NOT_SUPPORTED;
 if UART.UARTState <> UART_STATE_ENABLED then Exit;
 
 if MutexLock(UART.Lock) = ERROR_SUCCESS then
  begin
   if Assigned(UART.DeviceWrite) then
    begin
     {Call Device Write}
     Result:=UART.DeviceWrite(UART,Buffer,Size,Flags,Count);
    end
   else
    begin
     Result:=ERROR_INVALID_PARAMETER;
    end;    
    
   MutexUnlock(UART.Lock);
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;    
end;

{==============================================================================}

function UARTDeviceWait(UART:PUARTDevice;Direction,Timeout:LongWord):LongWord;
{Wait for data to be available in the receive or transmit FIFO of a UART device}
{UART: The UART device to wait for}
{Direction: The direction of data to wait for (eg UART_WAIT_RECEIVE)}
{Timeout: The number of milliseconds to wait for data (INFINITE to wait forever)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Unlock:Boolean;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check UART}
 if UART = nil then Exit;
 if UART.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IFDEF UART_DEBUG}
 if UART_LOG_ENABLED then UARTLogDebug(UART,'UART Device Wait (Direction=' + IntToStr(Direction) + ' Timeout=' + IntToStr(Timeout) + ')');
 {$ENDIF}
 
 {Check Timeout}
 if Timeout = 0 then Timeout:=INFINITE;
 
 {Check Mode}
 Result:=ERROR_INVALID_FUNCTION;
 if UART.UARTMode = UART_MODE_SERIAL then Exit;
 
 {Check Enabled}
 Result:=ERROR_NOT_SUPPORTED;
 if UART.UARTState <> UART_STATE_ENABLED then Exit;

 if MutexLock(UART.Lock) = ERROR_SUCCESS then
  begin
   try
    Unlock:=True;
    
    if Assigned(UART.DeviceWait) then
     begin
      {Call Device Wait}
      Result:=UART.DeviceWait(UART,Direction,Timeout);
     end
    else
     begin 
      {Check Receive}
      if Direction = UART_WAIT_RECEIVE then
       begin
        {Release the Lock}
        MutexUnlock(UART.Lock);
        Unlock:=False;
        
        {Wait for Data}
        Result:=EventWaitEx(UART.ReceiveWait,Timeout);
       end
      {Check Transmit}
      else if Direction = UART_WAIT_TRANSMIT then
       begin
        {Release the Lock}
        MutexUnlock(UART.Lock);
        Unlock:=False;
        
        {Wait for Space}
        Result:=EventWaitEx(UART.TransmitWait,Timeout);
       end;
     end;
   finally  
    if Unlock then MutexUnlock(UART.Lock);
   end; 
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function UARTDeviceStatus(UART:PUARTDevice):LongWord; inline;
{Get the current line status of a UART device}
{UART: The UART device to get the status from}
{Return: A set of flags containing the device status (eg UART_STATUS_RTS)}

{Note: Replaced by UARTDeviceGetStatus for consistency}
begin
 {}
 Result:=UARTDeviceGetStatus(UART);
end;

{==============================================================================}

function UARTDeviceGetStatus(UART:PUARTDevice):LongWord;
{Get the current line status of a UART device}
{UART: The UART device to get the status from}
{Return: A set of flags containing the device status (eg UART_STATUS_RTS)}
begin
 {}
 Result:=UART_STATUS_NONE;
 
 {Check UART}
 if UART = nil then Exit;
 if UART.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IFDEF UART_DEBUG}
 if UART_LOG_ENABLED then UARTLogDebug(UART,'UART Device Get Status');
 {$ENDIF}
 
 {Check Mode}
 if UART.UARTMode = UART_MODE_SERIAL then Exit;
 
 {Check Enabled}
 if UART.UARTState <> UART_STATE_ENABLED then Exit;
 
 if MutexLock(UART.Lock) = ERROR_SUCCESS then
  begin
   if Assigned(UART.DeviceGetStatus) then
    begin
     {Call Device Get Status}
     Result:=UART.DeviceGetStatus(UART);
    end
   else
    begin
     {Get Status}
     Result:=UART.UARTStatus;
    end;  
    
   MutexUnlock(UART.Lock);
  end;
end;

{==============================================================================}

function UARTDeviceSetStatus(UART:PUARTDevice;Status:LongWord):LongWord;
{Set the current line status of a UART device}
{UART: The UART device to set the status for}
{Status: The device status flags to be set (eg UART_STATUS_RTS)}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: Not all UART_STATUS_* flags can be set, the device may ignore invalid values}
{Note: Not all UART devices support set status, returns ERROR_CALL_NOT_IMPLEMENTED if not supported}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check UART}
 if UART = nil then Exit;
 if UART.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {$IFDEF UART_DEBUG}
 if UART_LOG_ENABLED then UARTLogDebug(UART,'UART Device Set Status (Status=' + IntToHex(Status,8) + ')');
 {$ENDIF}

 {Check Mode}
 Result:=ERROR_INVALID_FUNCTION;
 if UART.UARTMode = UART_MODE_SERIAL then Exit;
 
 {Check Enabled}
 Result:=ERROR_NOT_READY;
 if UART.UARTState <> UART_STATE_ENABLED then Exit;
 
 if MutexLock(UART.Lock) = ERROR_SUCCESS then
  begin
   if Assigned(UART.DeviceSetStatus) then
    begin
     {Call Device Set Status}
     Result:=UART.DeviceSetStatus(UART,Status);
    end
   else
    begin
     Result:=ERROR_CALL_NOT_IMPLEMENTED;
    end;  
    
   MutexUnlock(UART.Lock);
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function UARTDeviceProperties(UART:PUARTDevice;Properties:PUARTProperties):LongWord; inline;
{Get the properties for the specified UART device}
{UART: The UART device to get properties from}
{Properties: Pointer to a PUARTProperties structure to fill in}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: Replaced by UARTDeviceGetProperties for consistency}
begin
 {}
 Result:=UARTDeviceGetProperties(UART,Properties);
end;

{==============================================================================}

function UARTDeviceGetProperties(UART:PUARTDevice;Properties:PUARTProperties):LongWord;
{Get the properties for the specified UART device}
{UART: The UART device to get properties from}
{Properties: Pointer to a PUARTProperties structure to fill in}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Properties}
 if Properties = nil then Exit;
 
 {Check UART}
 if UART = nil then Exit;
 if UART.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IFDEF UART_DEBUG}
 if UART_LOG_ENABLED then UARTLogDebug(UART,'UART Device Get Properties');
 {$ENDIF}
 
 {Check Mode}
 {Result:=ERROR_INVALID_FUNCTION;}
 {if UART.UARTMode = UART_MODE_SERIAL then Exit;} {Allow when in serial mode}
 
 {Check Enabled}
 {Result:=ERROR_NOT_SUPPORTED;}
 {if UART.UARTState <> UART_STATE_ENABLED then Exit;} {Allow when disabled}
 
 if MutexLock(UART.Lock) = ERROR_SUCCESS then
  begin
   if Assigned(UART.DeviceGetProperties) then
    begin
     {Call Device Get Properites}
     Result:=UART.DeviceGetProperties(UART,Properties);
    end
   else
    begin
     {Get Properties}
     System.Move(UART.Properties,Properties^,SizeOf(TUARTProperties));
       
     {Return Result}
     Result:=ERROR_SUCCESS;
    end;  
    
   MutexUnlock(UART.Lock);
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;    
end;

{==============================================================================}

function UARTDeviceSetProperties(UART:PUARTDevice;Properties:PUARTProperties):LongWord;
{Set the properties for the specified UART device}
{UART: The UART device to set properties for}
{Properties: Pointer to a PUARTProperties structure to use}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Properties}
 if Properties = nil then Exit;
 
 {Check UART}
 if UART = nil then Exit;
 if UART.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IFDEF UART_DEBUG}
 if UART_LOG_ENABLED then UARTLogDebug(UART,'UART Device Set Properties');
 {$ENDIF}
 
 {Check Mode}
 Result:=ERROR_INVALID_FUNCTION;
 if UART.UARTMode = UART_MODE_SERIAL then Exit;
 
 {Check Enabled}
 Result:=ERROR_NOT_SUPPORTED;
 if UART.UARTState <> UART_STATE_ENABLED then Exit;
 
 if MutexLock(UART.Lock) = ERROR_SUCCESS then
  begin
   if Assigned(UART.DeviceSetProperties) then
    begin
     {Call Device Set Properites}
     Result:=UART.DeviceSetProperties(UART,Properties);
    end
   else
    begin
     {Close Device}
     UARTDeviceClose(UART);
     
     {Open Device}
     Result:=UARTDeviceOpen(UART,Properties.BaudRate,Properties.DataBits,Properties.StopBits,Properties.Parity,Properties.FlowControl);
    end;  
    
   MutexUnlock(UART.Lock);
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;    
end;

{==============================================================================}

function UARTDeviceCreate:PUARTDevice;
{Create a new UART entry}
{Return: Pointer to new UART entry or nil if UART could not be created}
begin
 {}
 Result:=UARTDeviceCreateEx(SizeOf(TUARTDevice));
end;

{==============================================================================}

function UARTDeviceCreateEx(Size:LongWord):PUARTDevice;
{Create a new UART entry}
{Size: Size in bytes to allocate for new UART (Including the UART entry)}
{Return: Pointer to new UART entry or nil if UART could not be created}
begin
 {}
 Result:=nil;
 
 {Check Size}
 if Size < SizeOf(TUARTDevice) then Exit;
 
 {Create UART}
 Result:=PUARTDevice(DeviceCreateEx(Size));
 if Result = nil then Exit;
 
 {Update Device}
 Result.Device.DeviceBus:=DEVICE_BUS_NONE;   
 Result.Device.DeviceType:=UART_TYPE_NONE;
 Result.Device.DeviceFlags:=UART_FLAG_NONE;
 Result.Device.DeviceData:=nil;

 {Update UART}
 Result.UARTId:=DEVICE_ID_ANY;
 Result.UARTMode:=UART_MODE_NONE;
 Result.UARTState:=UART_STATE_DISABLED;
 Result.UARTStatus:=UART_STATUS_NONE;
 Result.DeviceOpen:=nil;
 Result.DeviceClose:=nil;
 Result.DeviceRead:=nil;
 Result.DeviceWrite:=nil;
 Result.DeviceWait:=nil;
 Result.DeviceGetStatus:=nil;
 Result.DeviceSetStatus:=nil;
 Result.DeviceGetProperties:=nil;
 Result.DeviceSetProperties:=nil;
 Result.Lock:=INVALID_HANDLE_VALUE;
 Result.ReceiveWait:=INVALID_HANDLE_VALUE;
 Result.TransmitWait:=INVALID_HANDLE_VALUE;
 
 {Create Lock}
 Result.Lock:=MutexCreateEx(False,MUTEX_DEFAULT_SPINCOUNT,MUTEX_FLAG_RECURSIVE);
 if Result.Lock = INVALID_HANDLE_VALUE then
  begin
   if UART_LOG_ENABLED then UARTLogError(nil,'Failed to create lock for UART device');
   UARTDeviceDestroy(Result);
   Result:=nil;
   Exit;
  end;
  
 {Create Serial}
 Result.Serial:=SerialDeviceCreate;
 if Result.Serial = nil then
  begin
   if UART_LOG_ENABLED then UARTLogError(nil,'Failed to create serial device for UART device');
   UARTDeviceDestroy(Result);
   Result:=nil;
   Exit;
  end;
  
 {Update Serial}
 {Device}
 Result.Serial.Device.DeviceType:=SERIAL_TYPE_UART;
 Result.Serial.Device.DeviceData:=Result;
 {Serial}
 Result.Serial.DeviceOpen:=UARTSerialDeviceOpen;
 Result.Serial.DeviceClose:=UARTSerialDeviceClose;
 Result.Serial.DeviceRead:=UARTSerialDeviceRead;
 Result.Serial.DeviceWrite:=UARTSerialDeviceWrite;
 Result.Serial.DeviceWait:=UARTSerialDeviceWait;
 Result.Serial.DeviceGetStatus:=UARTSerialDeviceGetStatus;
 Result.Serial.DeviceSetStatus:=UARTSerialDeviceSetStatus;
 Result.Serial.DeviceGetProperties:=UARTSerialDeviceGetProperties;
 {Driver}
 Result.Serial.Properties.ReceiveDepth:=SERIAL_RECEIVE_DEPTH_DEFAULT;
 Result.Serial.Properties.TransmitDepth:=SERIAL_TRANSMIT_DEPTH_DEFAULT;
 {Other properites updated during Register}
end;

{==============================================================================}

function UARTDeviceDestroy(UART:PUARTDevice):LongWord;
{Destroy an existing UART entry}
{UART: The UART device to destroy}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check UART}
 if UART = nil then Exit;
 if UART.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check UART}
 Result:=ERROR_IN_USE;
 if UARTDeviceCheck(UART) = UART then Exit;

 {Check State}
 if UART.Device.DeviceState <> DEVICE_STATE_UNREGISTERED then Exit;
 
 {Destroy Serial}
 if UART.Serial <> nil then
  begin
   Result:=SerialDeviceDestroy(UART.Serial);
   if Result <> ERROR_SUCCESS then Exit;
  end; 
 
 {Destroy Lock}
 if UART.Lock <> INVALID_HANDLE_VALUE then
  begin
   MutexDestroy(UART.Lock);
  end;
 
 {Destroy UART} 
 Result:=DeviceDestroy(@UART.Device);
end;

{==============================================================================}

function UARTDeviceRegister(UART:PUARTDevice):LongWord;
{Register a new UART in the UART table}
{UART: The UART device to register}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 UARTId:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check UART}
 if UART = nil then Exit;
 if UART.UARTId <> DEVICE_ID_ANY then Exit;
 if UART.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check Serial}
 if UART.Serial = nil then Exit;
 
 {Check Interfaces}
 if not(Assigned(UART.DeviceOpen)) then Exit;
 if not(Assigned(UART.DeviceClose)) then Exit;
 if not(Assigned(UART.DeviceRead)) then Exit;
 if not(Assigned(UART.DeviceWrite)) then Exit;
 
 {Check UART}
 Result:=ERROR_ALREADY_EXISTS;
 if UARTDeviceCheck(UART) = UART then Exit;
 
 {Check State}
 if UART.Device.DeviceState <> DEVICE_STATE_UNREGISTERED then Exit;
 
 {Insert UART}
 if CriticalSectionLock(UARTDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Update UART}
    UARTId:=0;
    while UARTDeviceFind(UARTId) <> nil do
     begin
      Inc(UARTId);
     end;
    UART.UARTId:=UARTId;
    
    {Update Device}
    UART.Device.DeviceName:=UART_NAME_PREFIX + IntToStr(UART.UARTId); 
    UART.Device.DeviceClass:=DEVICE_CLASS_UART;
    
    {Register Device}
    Result:=DeviceRegister(@UART.Device);
    if Result <> ERROR_SUCCESS then
     begin
      UART.UARTId:=DEVICE_ID_ANY;
      Exit;
     end; 

    {Update Serial}
    {Device}
    UART.Serial.Device.DeviceBus:=UART.Device.DeviceBus;
    UART.Serial.Device.DeviceFlags:=UART.Device.DeviceFlags;
    UART.Serial.Device.DeviceDescription:=UART.Device.DeviceDescription;
    {Serial}
    UART.Serial.SerialState:=SERIAL_STATE_CLOSED;
    UART.Serial.SerialStatus:=SERIAL_STATUS_NONE;
    {Driver}
    UART.Serial.Properties.Flags:=UART.Properties.Flags;
    UART.Serial.Properties.MinRate:=UART.Properties.MinRate;
    UART.Serial.Properties.MaxRate:=UART.Properties.MaxRate;
    UART.Serial.Properties.BaudRate:=UART.Properties.BaudRate;
    UART.Serial.Properties.DataBits:=UART.Properties.DataBits;
    UART.Serial.Properties.StopBits:=UART.Properties.StopBits;
    UART.Serial.Properties.Parity:=UART.Properties.Parity;
    UART.Serial.Properties.FlowControl:=UART.Properties.FlowControl;
    
    {Register Serial}
    Result:=SerialDeviceRegister(UART.Serial);
    if Result <> ERROR_SUCCESS then
     begin
      DeviceDeregister(@UART.Device);
      UART.UARTId:=DEVICE_ID_ANY;
      Exit;
     end;
    
    {Link UART}
    if UARTDeviceTable = nil then
     begin
      UARTDeviceTable:=UART;
     end
    else
     begin
      UART.Next:=UARTDeviceTable;
      UARTDeviceTable.Prev:=UART;
      UARTDeviceTable:=UART;
     end;
 
    {Increment Count}
    Inc(UARTDeviceTableCount);
    
    {Check Default}
    if UARTDeviceDefault = nil then
     begin
      UARTDeviceDefault:=UART;
     end;
    
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    CriticalSectionUnlock(UARTDeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;

{==============================================================================}

function UARTDeviceDeregister(UART:PUARTDevice):LongWord;
{Deregister a UART from the UART table}
{UART: The UART device to deregister}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Prev:PUARTDevice;
 Next:PUARTDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check UART}
 if UART = nil then Exit;
 if UART.UARTId = DEVICE_ID_ANY then Exit;
 if UART.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check Serial}
 if UART.Serial = nil then Exit;
 
 {Check UART}
 Result:=ERROR_NOT_FOUND;
 if UARTDeviceCheck(UART) <> UART then Exit;
 
 {Check State}
 if UART.Device.DeviceState <> DEVICE_STATE_REGISTERED then Exit;
 
 {Remove UART}
 if CriticalSectionLock(UARTDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Deregister Serial}
    Result:=SerialDeviceDeregister(UART.Serial);
    if Result <> ERROR_SUCCESS then Exit;
    
    {Deregister Device}
    Result:=DeviceDeregister(@UART.Device);
    if Result <> ERROR_SUCCESS then Exit;
    
    {Unlink UART}
    Prev:=UART.Prev;
    Next:=UART.Next;
    if Prev = nil then
     begin
      UARTDeviceTable:=Next;
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
    Dec(UARTDeviceTableCount);
 
    {Check Default}
    if UARTDeviceDefault = UART then
     begin
      UARTDeviceDefault:=UARTDeviceTable;
     end;
 
    {Update UART}
    UART.UARTId:=DEVICE_ID_ANY;
 
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    CriticalSectionUnlock(UARTDeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;

{==============================================================================}

function UARTDeviceFind(UARTId:LongWord):PUARTDevice;
{Find a UART device by ID in the UART table}
{UARTId: The ID number of the UART to find}
{Return: Pointer to UART device entry or nil if not found}
var
 UART:PUARTDevice;
begin
 {}
 Result:=nil;
 
 {Check Id}
 if UARTId = DEVICE_ID_ANY then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(UARTDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get UART}
    UART:=UARTDeviceTable;
    while UART <> nil do
     begin
      {Check State}
      if UART.Device.DeviceState = DEVICE_STATE_REGISTERED then
       begin
        {Check Id}
        if UART.UARTId = UARTId then
         begin
          Result:=UART;
          Exit;
         end;
       end;
       
      {Get Next}
      UART:=UART.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(UARTDeviceTableLock);
   end;
  end;
end;

{==============================================================================}

function UARTDeviceFindByName(const Name:String):PUARTDevice; inline;
{Find a UART device by name in the UART table}
{Name: The name of the UART to find (eg UART0)}
{Return: Pointer to UART device entry or nil if not found}
begin
 {}
 Result:=PUARTDevice(DeviceFindByName(Name));
end;

{==============================================================================}

function UARTDeviceFindByDescription(const Description:String):PUARTDevice; inline;
{Find a UART device by description in the UART table}
{Description: The description of the UART to find (eg BCM2836 PL011 UART)}
{Return: Pointer to UART device entry or nil if not found}
begin
 {}
 Result:=PUARTDevice(DeviceFindByDescription(Description));
end;
       
{==============================================================================}

function UARTDeviceEnumerate(Callback:TUARTEnumerate;Data:Pointer):LongWord;
{Enumerate all UART devices in the UART table}
{Callback: The callback function to call for each UART in the table}
{Data: A private data pointer to pass to callback for each UART in the table}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 UART:PUARTDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Callback}
 if not Assigned(Callback) then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(UARTDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get UART}
    UART:=UARTDeviceTable;
    while UART <> nil do
     begin
      {Check State}
      if UART.Device.DeviceState = DEVICE_STATE_REGISTERED then
       begin
        if Callback(UART,Data) <> ERROR_SUCCESS then Exit;
       end;
       
      {Get Next}
      UART:=UART.Next;
     end;
     
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    CriticalSectionUnlock(UARTDeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;

{==============================================================================}

function UARTDeviceNotification(UART:PUARTDevice;Callback:TUARTNotification;Data:Pointer;Notification,Flags:LongWord):LongWord;
{Register a notification for UART device changes}
{UART: The UART device to notify changes for (Optional, pass nil for all UART devices)}
{Callback: The function to call when a notification event occurs}
{Data: A private data pointer to pass to callback when a notification event occurs}
{Notification: The events to register for notification of (eg DEVICE_NOTIFICATION_REGISTER)}
{Flags: The flags to control the notification (eg NOTIFIER_FLAG_WORKER)}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check UART}
 if UART = nil then
  begin
   Result:=DeviceNotification(nil,DEVICE_CLASS_UART,Callback,Data,Notification,Flags);
  end
 else
  begin 
   {Check UART}
   if UART.Device.Signature <> DEVICE_SIGNATURE then Exit;

   Result:=DeviceNotification(@UART.Device,DEVICE_CLASS_UART,Callback,Data,Notification,Flags);
  end; 
end;

{==============================================================================}
{==============================================================================}
{UART Serial Functions}
function UARTSerialDeviceOpen(Serial:PSerialDevice;BaudRate,DataBits,StopBits,Parity,FlowControl,ReceiveDepth,TransmitDepth:LongWord):LongWord;
{Implementation of SerialDeviceOpen API for UART Serial}
{Note: Not intended to be called directly by applications, use SerialDeviceOpen instead}
var 
 UART:PUARTDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Serial}
 if Serial = nil then Exit;
 if Serial.Device.Signature <> DEVICE_SIGNATURE then Exit; 

 {$IFDEF SERIAL_DEBUG}
 if SERIAL_LOG_ENABLED then SerialLogDebug(Serial,'UART: Device Open (BaudRate=' + IntToStr(BaudRate) + ' DataBits=' + IntToStr(DataBits) + ' StopBits=' + IntToStr(StopBits) + ' Parity=' + SerialParityToString(Parity) + ' FlowControl=' + SerialFlowControlToString(FlowControl) + ')');
 {$ENDIF}

 {Get UART}
 UART:=PUARTDevice(Serial.Device.DeviceData);
 if UART = nil then Exit;
 if UART.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {Check Mode}
 Result:=ERROR_INVALID_FUNCTION;
 if UART.UARTMode = UART_MODE_UART then Exit;
 
 {Check Disabled}
 Result:=ERROR_SUCCESS;
 if UART.UARTState <> UART_STATE_DISABLED then Exit;
 
 {Check Receive Depth}
 if ReceiveDepth = 0 then ReceiveDepth:=SERIAL_RECEIVE_DEPTH_DEFAULT;
 
 {Check Transmit Depth}
 if TransmitDepth = 0 then TransmitDepth:=SERIAL_TRANSMIT_DEPTH_DEFAULT;
 
 if MutexLock(UART.Lock) = ERROR_SUCCESS then
  begin
   try
    if Assigned(UART.DeviceOpen) then
     begin
      {Allocate Receive}
      EventReset(Serial.Receive.Wait);
      Serial.Receive.Start:=0;
      Serial.Receive.Count:=0;
      Serial.Receive.Size:=ReceiveDepth;
      Serial.Receive.Data:=GetMem(ReceiveDepth);
      if Serial.Receive.Data = nil then
       begin
        Result:=ERROR_OPERATION_FAILED;
        Exit;
       end;
       
      {Allocate Transmit}
      EventSet(Serial.Transmit.Wait);
      Serial.Transmit.Start:=0;
      Serial.Transmit.Count:=0;
      Serial.Transmit.Size:=TransmitDepth;
      Serial.Transmit.Data:=GetMem(TransmitDepth);
      if Serial.Transmit.Data = nil then
       begin
        {Release Receive}
        FreeMem(Serial.Receive.Data);
        
        Result:=ERROR_OPERATION_FAILED;
        Exit;
       end;
      
      {Set Mode}
      UART.UARTMode:=UART_MODE_SERIAL;
      
      {Call Device Open}
      Result:=UART.DeviceOpen(UART,BaudRate,DataBits,StopBits,Parity,FlowControl);
      if Result <> ERROR_SUCCESS then
       begin
        {Reset Mode}
        UART.UARTMode:=UART_MODE_NONE;
        
        {Release Receive}
        Serial.Receive.Start:=0;
        Serial.Receive.Count:=0;
        Serial.Receive.Size:=0;
        FreeMem(Serial.Receive.Data);
        
        {Release Transmit}
        Serial.Transmit.Start:=0;
        Serial.Transmit.Count:=0;
        Serial.Transmit.Size:=0;
        FreeMem(Serial.Transmit.Data);
        
        Exit;
       end; 
     end
    else
     begin    
      Result:=ERROR_INVALID_PARAMETER;
      Exit;
     end; 
 
    {Update Properties}
    Serial.Properties.Flags:=UART.Properties.Flags;
    Serial.Properties.MinRate:=UART.Properties.MinRate;
    Serial.Properties.MaxRate:=UART.Properties.MaxRate;
    Serial.Properties.BaudRate:=UART.Properties.BaudRate;
    Serial.Properties.DataBits:=UART.Properties.DataBits;
    Serial.Properties.StopBits:=UART.Properties.StopBits;
    Serial.Properties.Parity:=UART.Properties.Parity;
    Serial.Properties.FlowControl:=UART.Properties.FlowControl;
    Serial.Properties.ReceiveDepth:=ReceiveDepth;
    Serial.Properties.TransmitDepth:=TransmitDepth;
    
    {Enable Device}
    UART.UARTState:=UART_STATE_ENABLED;
    
    {Notify Enable}
    NotifierNotify(@UART.Device,DEVICE_NOTIFICATION_ENABLE);
    
    Result:=ERROR_SUCCESS;
   finally
    MutexUnlock(UART.Lock);
   end; 
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;    
end;

{==============================================================================}

function UARTSerialDeviceClose(Serial:PSerialDevice):LongWord;
{Implementation of SerialDeviceClose API for UART Serial}
{Note: Not intended to be called directly by applications, use SerialDeviceClose instead}
var 
 UART:PUARTDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Serial}
 if Serial = nil then Exit;
 if Serial.Device.Signature <> DEVICE_SIGNATURE then Exit; 

 {$IFDEF SERIAL_DEBUG}
 if SERIAL_LOG_ENABLED then SerialLogDebug(Serial,'UART: Device Close');
 {$ENDIF}

 {Get UART}
 UART:=PUARTDevice(Serial.Device.DeviceData);
 if UART = nil then Exit;
 if UART.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {Check Mode}
 Result:=ERROR_INVALID_FUNCTION;
 if UART.UARTMode = UART_MODE_UART then Exit;
 
 {Check Enabled}
 Result:=ERROR_SUCCESS;
 if UART.UARTState <> UART_STATE_ENABLED then Exit;
 
 if MutexLock(UART.Lock) = ERROR_SUCCESS then
  begin
   try
    if Assigned(UART.DeviceClose) then
     begin
      {Call Device Close}
      Result:=UART.DeviceClose(UART);
      if Result <> ERROR_SUCCESS then Exit;
     end
    else
     begin
      Result:=ERROR_INVALID_PARAMETER;
      Exit;
     end;    
   
    {Reset Mode}
    UART.UARTMode:=UART_MODE_NONE;

    {Release Receive}
    Serial.Receive.Start:=0;
    Serial.Receive.Count:=0;
    Serial.Receive.Size:=0;
    FreeMem(Serial.Receive.Data);
    
    {Release Transmit}
    Serial.Transmit.Start:=0;
    Serial.Transmit.Count:=0;
    Serial.Transmit.Size:=0;
    FreeMem(Serial.Transmit.Data);
    
    {Reset Properties}
    Serial.Properties.Flags:=UART.Properties.Flags;
    Serial.Properties.MinRate:=UART.Properties.MinRate;
    Serial.Properties.MaxRate:=UART.Properties.MaxRate;
    Serial.Properties.BaudRate:=UART.Properties.BaudRate;
    Serial.Properties.DataBits:=UART.Properties.DataBits;
    Serial.Properties.StopBits:=UART.Properties.StopBits;
    Serial.Properties.Parity:=UART.Properties.Parity;
    Serial.Properties.FlowControl:=UART.Properties.FlowControl;
    Serial.Properties.ReceiveDepth:=SERIAL_RECEIVE_DEPTH_DEFAULT;
    Serial.Properties.TransmitDepth:=SERIAL_TRANSMIT_DEPTH_DEFAULT;
    
    {Disable Device}
    UART.UARTState:=UART_STATE_DISABLED;
    
    {Notify Disable}
    NotifierNotify(@UART.Device,DEVICE_NOTIFICATION_DISABLE);
    
    Result:=ERROR_SUCCESS;
   finally
    MutexUnlock(UART.Lock);
   end; 
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;    
end;

{==============================================================================}

function UARTSerialDeviceRead(Serial:PSerialDevice;Buffer:Pointer;Size,Flags:LongWord;var Count:LongWord):LongWord;
{Implementation of SerialDeviceRead API for UART Serial}
{Note: Not intended to be called directly by applications, use SerialDeviceRead instead}
var 
 Data:Pointer;
 Total:LongWord;
 Offset:PtrUint;
 UART:PUARTDevice;
 Status:LongWord;
 Timeout:LongWord;
 Removed:LongWord;
 Available:LongWord;
begin
 {}
 {Setup Result}
 Count:=0;
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Buffer}
 if Buffer = nil then Exit;
 
 {Check Serial}
 if Serial = nil then Exit;
 if Serial.Device.Signature <> DEVICE_SIGNATURE then Exit; 

 {$IFDEF SERIAL_DEBUG}
 if SERIAL_LOG_ENABLED then SerialLogDebug(Serial,'UART: Device Read (Size=' + IntToStr(Size) + ')');
 {$ENDIF}

 {Get UART}
 UART:=PUARTDevice(Serial.Device.DeviceData);
 if UART = nil then Exit;
 if UART.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {Check Mode}
 Result:=ERROR_INVALID_FUNCTION;
 if UART.UARTMode = UART_MODE_UART then Exit;
 
 {Check Enabled}
 Result:=ERROR_NOT_SUPPORTED;
 if UART.UARTState <> UART_STATE_ENABLED then Exit;
 
 {Read to Buffer}
 Offset:=0;
 Total:=Size;
 while Size > 0 do
  begin
   {Get Timeout}
   Timeout:=INFINITE;
   if (UART.Device.DeviceFlags and UART_FLAG_PUSH_RX) <> 0 then Timeout:=UART_PUSH_TIMEOUT;
   
   {Check State}
   if (Timeout <> INFINITE) and (EventState(Serial.Receive.Wait) <> EVENT_STATE_SIGNALED) then
    begin
     {Release the Lock}
     MutexUnlock(Serial.Lock);
     
     {Push Receive}
     UARTSerialDeviceReceive(UART);
     
     {Acquire the Lock}
     if MutexLock(Serial.Lock) <> ERROR_SUCCESS then
      begin
       Result:=ERROR_CAN_NOT_COMPLETE;
       Exit;
      end;      
    end;
   
   {Check Non Blocking}
   if ((Flags and SERIAL_READ_NON_BLOCK) <> 0) and (Serial.Receive.Count = 0) then
    begin
     Result:=ERROR_NO_MORE_ITEMS;
     Break;
    end;
 
   {Check Peek Buffer}
   if (Flags and SERIAL_READ_PEEK_BUFFER) <> 0 then
    begin
     Count:=Serial.Receive.Count;
     Result:=ERROR_SUCCESS;
     Break;
    end;
   
   {Release the Lock}
   MutexUnlock(Serial.Lock);
 
   {Wait for Data}
   Status:=EventWaitEx(Serial.Receive.Wait,Timeout);
   if Status = ERROR_SUCCESS then
    begin
     {Acquire the Lock}
     if MutexLock(Serial.Lock) = ERROR_SUCCESS then
      begin
       {Start Read}
       Data:=SerialBufferReadStart(@Serial.Receive,Available);
       while (Data <> nil) and (Available > 0) and (Size > 0) do
        begin
         {Get Removed}
         Removed:=Min(Size,Available);
         
         {Copy Data}
         System.Move(Data^,Pointer(Buffer + Offset)^,Removed);

         {Update Statistics}
         Inc(Serial.ReceiveCount,Removed);
         
         {Update Count}
         Inc(Count,Removed);
         
         {Update Size and Offset}
         Dec(Size,Removed);
         Inc(Offset,Removed);
         
         {Complete Read}
         SerialBufferReadComplete(@Serial.Receive,Removed);
         
         {Start Read}
         Data:=SerialBufferReadStart(@Serial.Receive,Available);
        end;
       
       {Check Available}
       if Available = 0 then
        begin
         {Reset Event}
         EventReset(Serial.Receive.Wait);
        end;
      end
     else
      begin
       Result:=ERROR_CAN_NOT_COMPLETE;
       Exit;
      end;      
    end
   else if Status = ERROR_WAIT_TIMEOUT then
    begin
     {Push Receive}
     UARTSerialDeviceReceive(UART);
     
     {Acquire the Lock}
     if MutexLock(Serial.Lock) <> ERROR_SUCCESS then
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
 
 {$IFDEF SERIAL_DEBUG}
 if SERIAL_LOG_ENABLED then SerialLogDebug(Serial,'UART:  Return Count=' + IntToStr(Count));
 {$ENDIF}
 
 {Return Result}
 if (Total = Count) then Result:=ERROR_SUCCESS; 
end;

{==============================================================================}

function UARTSerialDeviceWrite(Serial:PSerialDevice;Buffer:Pointer;Size,Flags:LongWord;var Count:LongWord):LongWord;
{Implementation of SerialDeviceWrite API for UART Serial}
{Note: Not intended to be called directly by applications, use SerialDeviceWrite instead}
var 
 Data:Pointer;
 Empty:Boolean;
 Total:LongWord;
 Offset:PtrUint;
 UART:PUARTDevice;
 Added:LongWord;
 Status:LongWord;
 Timeout:LongWord;
 Available:LongWord;
begin
 {}
 {Setup Result}
 Count:=0;
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Buffer}
 if Buffer = nil then Exit;
 
 {Check Serial}
 if Serial = nil then Exit;
 if Serial.Device.Signature <> DEVICE_SIGNATURE then Exit; 

 {$IFDEF SERIAL_DEBUG}
 if SERIAL_LOG_ENABLED then SerialLogDebug(Serial,'UART: Device Write (Size=' + IntToStr(Size) + ')');
 {$ENDIF}

 {Get UART}
 UART:=PUARTDevice(Serial.Device.DeviceData);
 if UART = nil then Exit;
 if UART.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {Check Mode}
 Result:=ERROR_INVALID_FUNCTION;
 if UART.UARTMode = UART_MODE_UART then Exit;
 
 {Check Enabled}
 Result:=ERROR_NOT_SUPPORTED;
 if UART.UARTState <> UART_STATE_ENABLED then Exit;
 
 {Write from Buffer}
 Offset:=0;
 Total:=Size;
 while Size > 0 do
  begin
   {Get Timeout}
   Timeout:=INFINITE;
   if (UART.Device.DeviceFlags and UART_FLAG_PUSH_TX) <> 0 then Timeout:=UART_PUSH_TIMEOUT;
   
   {Check State}
   if (Timeout <> INFINITE) and (EventState(Serial.Transmit.Wait) <> EVENT_STATE_SIGNALED) then
    begin
     {Release the Lock}
     MutexUnlock(Serial.Lock);
     
     {Push Transmit}
     UARTSerialDeviceTransmit(UART);
     
     {Acquire the Lock}
     if MutexLock(Serial.Lock) <> ERROR_SUCCESS then
      begin
       Result:=ERROR_CAN_NOT_COMPLETE;
       Exit;
      end;      
    end;    
   
   {Check Non Blocking}
   if ((Flags and SERIAL_WRITE_NON_BLOCK) <> 0) and ((Serial.Transmit.Size - Serial.Transmit.Count) = 0) then
    begin
     Result:=ERROR_INSUFFICIENT_BUFFER;
     Break;
    end;
 
   {Check Peek Buffer}
   if (Flags and SERIAL_WRITE_PEEK_BUFFER) <> 0 then
    begin
     Count:=Serial.Transmit.Size - Serial.Transmit.Count;
     Result:=ERROR_SUCCESS;
     Break;
    end;
   
   {Release the Lock}
   MutexUnlock(Serial.Lock);
   
   {Wait for Space}
   Status:=EventWaitEx(Serial.Transmit.Wait,Timeout);
   if Status = ERROR_SUCCESS then
    begin
     {Acquire the Lock}
     if MutexLock(Serial.Lock) = ERROR_SUCCESS then
      begin
       {Check Empty}
       Empty:=(Serial.Transmit.Count = 0);
       
       {Start Write}
       Data:=SerialBufferWriteStart(@Serial.Transmit,Available);
       while (Data <> nil) and (Available > 0) and (Size > 0) do
        begin
         {Get Added}
         Added:=Min(Size,Available);
         
         {Copy Data}
         System.Move(Pointer(Buffer + Offset)^,Data^,Added);
 
         {Update Statistics}
         Inc(Serial.TransmitCount,Added);
         
         {Update Count}
         Inc(Count,Added);
         
         {Update Size and Offset}
         Dec(Size,Added);
         Inc(Offset,Added);
 
         {Complete Write}
         SerialBufferWriteComplete(@Serial.Transmit,Added);
         
         {Start Write}
         Data:=SerialBufferWriteStart(@Serial.Transmit,Available);
        end;
       
       {Check Available}
       if Available = 0 then
        begin
         {Reset Event}
         EventReset(Serial.Transmit.Wait);
        end;
        
       {Check Empty}
       if Empty then
        begin
         {Send Serial Transmit}
         WorkerSchedule(0,TWorkerTask(UARTSerialDeviceTransmit),UART,nil);
        end;
      end
     else
      begin
       Result:=ERROR_CAN_NOT_COMPLETE;
       Exit;
      end;      
    end
   else if Status = ERROR_WAIT_TIMEOUT then
    begin
     {Push Transmit}
     UARTSerialDeviceTransmit(UART);
     
     {Acquire the Lock}
     if MutexLock(Serial.Lock) <> ERROR_SUCCESS then
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
  
 {$IFDEF SERIAL_DEBUG}
 if SERIAL_LOG_ENABLED then SerialLogDebug(Serial,'UART:  Return Count=' + IntToStr(Count));
 {$ENDIF}
 
 {Return Result}
 if (Total = Count) then Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function UARTSerialDeviceWait(Serial:PSerialDevice;Direction,Timeout:LongWord;var Count:LongWord):LongWord;
{Implementation of SerialDeviceWait API for UART Serial}
{Note: Not intended to be called directly by applications, use SerialDeviceWait instead}
var
 Start:Int64;
 Current:Int64;
 Retry:LongWord;
 Status:LongWord;
 UART:PUARTDevice;
begin
 {}
 {Setup Result}
 Count:=0;
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Serial}
 if Serial = nil then Exit;
 if Serial.Device.Signature <> DEVICE_SIGNATURE then Exit; 

 {$IFDEF SERIAL_DEBUG}
 if SERIAL_LOG_ENABLED then SerialLogDebug(Serial,'UART: Device Wait (Direction=' + IntToStr(Direction) + ' Timeout=' + IntToStr(Timeout) + ')');
 {$ENDIF}

 {Get UART}
 UART:=PUARTDevice(Serial.Device.DeviceData);
 if UART = nil then Exit;
 if UART.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {Check Mode}
 Result:=ERROR_INVALID_FUNCTION;
 if UART.UARTMode = UART_MODE_UART then Exit;
 
 {Check Enabled}
 Result:=ERROR_NOT_SUPPORTED;
 if UART.UARTState <> UART_STATE_ENABLED then Exit;
 
 {Check Receive}
 if Direction = SERIAL_WAIT_RECEIVE then
  begin
   {Get Retry Timeout}
   Retry:=INFINITE;
   if (UART.Device.DeviceFlags and UART_FLAG_PUSH_RX) <> 0 then Retry:=UART_PUSH_TIMEOUT;
   if Timeout < Retry then Retry:=Timeout;

   {Get Start}
   Start:=GetTickCount64;
   Current:=Start;
   while (Timeout = INFINITE) or (Current < (Start + Timeout)) do
    begin
     {Release the Lock}
     MutexUnlock(Serial.Lock);
     
     {Wait for Data}
     Status:=EventWaitEx(Serial.Receive.Wait,Retry);
     if Status = ERROR_SUCCESS then
      begin
       {Acquire the Lock}
       if MutexLock(Serial.Lock) = ERROR_SUCCESS then
        begin
         {Get Count}
         Count:=Serial.Receive.Count;
         
         {Return Result}
         Result:=ERROR_SUCCESS;
         Exit;
        end
       else
        begin
         Result:=ERROR_CAN_NOT_COMPLETE;
         Exit;
        end;      
      end
     else if Status = ERROR_WAIT_TIMEOUT then
      begin
       {Push Receive}
       UARTSerialDeviceReceive(UART);

       {Acquire the Lock}
       if MutexLock(Serial.Lock) <> ERROR_SUCCESS then
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
      
     {Get Current}
     Current:=GetTickCount64;     
    end;    
   
   {Return Timeout}
   Result:=ERROR_WAIT_TIMEOUT;
  end
 {Check Transmit}
 else if Direction = SERIAL_WAIT_TRANSMIT then
  begin
   {Get Retry Timeout}
   Retry:=INFINITE;
   if (UART.Device.DeviceFlags and UART_FLAG_PUSH_TX) <> 0 then Retry:=UART_PUSH_TIMEOUT;
   if Timeout < Retry then Retry:=Timeout;
   
   {Get Start}
   Start:=GetTickCount64;
   Current:=Start;
   while (Timeout = INFINITE) or (Current < (Start + Timeout)) do
    begin
     {Release the Lock}
     MutexUnlock(Serial.Lock);
     
     {Wait for Space}
     Status:=EventWaitEx(Serial.Transmit.Wait,Retry);
     if Status = ERROR_SUCCESS then
      begin
       {Acquire the Lock}
       if MutexLock(Serial.Lock) = ERROR_SUCCESS then
        begin
         {Get Count}
         Count:=Serial.Transmit.Size - Serial.Transmit.Count;
         
         {Return Result}
         Result:=ERROR_SUCCESS;
         Exit;
        end
       else
        begin
         Result:=ERROR_CAN_NOT_COMPLETE;
         Exit;
        end;
      end
     else if Status = ERROR_WAIT_TIMEOUT then
      begin
       {Push Transmit}
       UARTSerialDeviceTransmit(UART);
       
       {Acquire the Lock}
       if MutexLock(Serial.Lock) <> ERROR_SUCCESS then
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

     {Get Current}
     Current:=GetTickCount64;     
    end;

   {Return Timeout}
   Result:=ERROR_WAIT_TIMEOUT;
  end;
end;

{==============================================================================}

function UARTSerialDeviceStatus(Serial:PSerialDevice):LongWord; inline;
{Implementation of SerialDeviceStatus API for UART Serial}
{Note: Not intended to be called directly by applications, use SerialDeviceStatus instead}

{Note: Replaced by UARTSerialDeviceGetStatus for consistency}
begin
 {}
 Result:=UARTSerialDeviceGetStatus(Serial);
end;

{==============================================================================}

function UARTSerialDeviceGetStatus(Serial:PSerialDevice):LongWord;
{Implementation of SerialDeviceGetStatus API for UART Serial}
{Note: Not intended to be called directly by applications, use SerialDeviceGetStatus instead}
var 
 UART:PUARTDevice;
begin
 {}
 Result:=SERIAL_STATUS_NONE;
 
 {Check Serial}
 if Serial = nil then Exit;
 if Serial.Device.Signature <> DEVICE_SIGNATURE then Exit; 

 {$IFDEF SERIAL_DEBUG}
 if SERIAL_LOG_ENABLED then SerialLogDebug(Serial,'UART: Device Get Status');
 {$ENDIF}

 {Get UART}
 UART:=PUARTDevice(Serial.Device.DeviceData);
 if UART = nil then Exit;
 if UART.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {Check Mode}
 if UART.UARTMode = UART_MODE_UART then Exit;
 
 {Check Enabled}
 if UART.UARTState <> UART_STATE_ENABLED then Exit;
 
 if MutexLock(UART.Lock) = ERROR_SUCCESS then
  begin
   if Assigned(UART.DeviceGetStatus) then
    begin
     {Call Device Status}
     Result:=UART.DeviceGetStatus(UART);
    end
   else
    begin
     {Get Status}
     Result:=UART.UARTStatus;
    end;
     
   MutexUnlock(UART.Lock);
  end;
end;

{==============================================================================}

function UARTSerialDeviceSetStatus(Serial:PSerialDevice;Status:LongWord):LongWord;
{Implementation of SerialDeviceSetStatus API for UART Serial}
{Note: Not intended to be called directly by applications, use SerialDeviceSetStatus instead}
var 
 UART:PUARTDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Serial}
 if Serial = nil then Exit;
 if Serial.Device.Signature <> DEVICE_SIGNATURE then Exit; 

 {$IFDEF SERIAL_DEBUG}
 if SERIAL_LOG_ENABLED then SerialLogDebug(Serial,'UART: Device Set Status (Status=' + IntToHex(Status,8) + ')');
 {$ENDIF}

 {Get UART}
 UART:=PUARTDevice(Serial.Device.DeviceData);
 if UART = nil then Exit;
 if UART.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {Check Mode}
 Result:=ERROR_INVALID_FUNCTION;
 if UART.UARTMode = UART_MODE_UART then Exit;
 
 {Check Enabled}
 Result:=ERROR_NOT_SUPPORTED;
 if UART.UARTState <> UART_STATE_ENABLED then Exit;
 
 if MutexLock(UART.Lock) = ERROR_SUCCESS then
  begin
   if Assigned(UART.DeviceSetStatus) then
    begin
     {Call Device Status}
     Result:=UART.DeviceSetStatus(UART,Status);
    end
   else
    begin
     Result:=ERROR_CALL_NOT_IMPLEMENTED;
    end;
     
   MutexUnlock(UART.Lock);
  end;
end;

{==============================================================================}

function UARTSerialDeviceGetProperties(Serial:PSerialDevice;Properties:PSerialProperties):LongWord;
{Implementation of SerialDeviceGetProperties API for UART Serial}
{Note: Not intended to be called directly by applications, use SerialDeviceGetProperties instead}
var 
 UART:PUARTDevice;
 UARTProperties:TUARTProperties;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Properties}
 if Properties = nil then Exit;
 
 {Check Serial}
 if Serial = nil then Exit;
 if Serial.Device.Signature <> DEVICE_SIGNATURE then Exit; 

 {$IFDEF SERIAL_DEBUG}
 if SERIAL_LOG_ENABLED then SerialLogDebug(Serial,'UART: Device Get Properties');
 {$ENDIF}

 {Get UART}
 UART:=PUARTDevice(Serial.Device.DeviceData);
 if UART = nil then Exit;
 if UART.Device.Signature <> DEVICE_SIGNATURE then Exit; 

 {Check Mode}
 {Result:=ERROR_INVALID_FUNCTION;}
 {if UART.UARTMode = UART_MODE_UART then Exit;} {Allow when in UART mode}
 
 {Check Enabled}
 {Result:=ERROR_NOT_SUPPORTED;}
 {if UART.UARTState <> UART_STATE_ENABLED then Exit;} {Allow when disabled}
 
 if MutexLock(UART.Lock) = ERROR_SUCCESS then
  begin
   if Assigned(UART.DeviceGetProperties) then
    begin
     {Call Device Get Properites}
     Result:=UART.DeviceGetProperties(UART,@UARTProperties);
     if Result = ERROR_SUCCESS then
      begin
       {Get Properties}
       {UART}
       Properties.Flags:=UART.Properties.Flags;
       Properties.MinRate:=UART.Properties.MinRate;
       Properties.MaxRate:=UART.Properties.MaxRate;
       Properties.BaudRate:=UART.Properties.BaudRate;
       Properties.DataBits:=UART.Properties.DataBits;
       Properties.StopBits:=UART.Properties.StopBits;
       Properties.Parity:=UART.Properties.Parity;
       Properties.FlowControl:=UART.Properties.FlowControl;
       {Serial}
       Properties.ReceiveDepth:=Serial.Properties.ReceiveDepth;
       Properties.TransmitDepth:=Serial.Properties.TransmitDepth;
      end;
    end
   else
    begin
     {Get Properties}
     {UART}
     Properties.Flags:=UART.Properties.Flags;
     Properties.MinRate:=UART.Properties.MinRate;
     Properties.MaxRate:=UART.Properties.MaxRate;
     Properties.BaudRate:=UART.Properties.BaudRate;
     Properties.DataBits:=UART.Properties.DataBits;
     Properties.StopBits:=UART.Properties.StopBits;
     Properties.Parity:=UART.Properties.Parity;
     Properties.FlowControl:=UART.Properties.FlowControl;
     {Serial}
     Properties.ReceiveDepth:=Serial.Properties.ReceiveDepth;
     Properties.TransmitDepth:=Serial.Properties.TransmitDepth;
       
     {Return Result}
     Result:=ERROR_SUCCESS;
    end;  
    
   MutexUnlock(UART.Lock);
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;    
end;

{==============================================================================}
{==============================================================================}
{UART Helper Functions}
function UARTGetCount:LongWord; inline;
{Get the current UART count}
begin
 {}
 Result:=UARTDeviceTableCount;
end;

{==============================================================================}

function UARTDeviceGetDefault:PUARTDevice; inline;
{Get the current default UART device}
begin
 {}
 Result:=UARTDeviceDefault;
end;

{==============================================================================}

function UARTDeviceSetDefault(UART:PUARTDevice):LongWord; 
{Set the current default UART device}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check UART}
 if UART = nil then Exit;
 if UART.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(UARTDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Check UART}
    if UARTDeviceCheck(UART) <> UART then Exit;
    
    {Set UART Default}
    UARTDeviceDefault:=UART;
    
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    CriticalSectionUnlock(UARTDeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function UARTDeviceCheck(UART:PUARTDevice):PUARTDevice;
{Check if the supplied UART is in the UART table}
var
 Current:PUARTDevice;
begin
 {}
 Result:=nil;
 
 {Check UART}
 if UART = nil then Exit;
 if UART.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(UARTDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get UART}
    Current:=UARTDeviceTable;
    while Current <> nil do
     begin
      {Check UART}
      if Current = UART then
       begin
        Result:=UART;
        Exit;
       end;
      
      {Get Next}
      Current:=Current.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(UARTDeviceTableLock);
   end;
  end;
end;

{==============================================================================}

procedure UARTLog(Level:LongWord;UART:PUARTDevice;const AText:String);
var
 WorkBuffer:String;
begin
 {}
 {Check Level}
 if Level < UART_DEFAULT_LOG_LEVEL then Exit;
 
 WorkBuffer:='';
 {Check Level}
 if Level = UART_LOG_LEVEL_DEBUG then
  begin
   WorkBuffer:=WorkBuffer + '[DEBUG] ';
  end
 else if Level = UART_LOG_LEVEL_WARN then
  begin
   WorkBuffer:=WorkBuffer + '[WARN] ';
  end
 else if Level = UART_LOG_LEVEL_ERROR then
  begin
   WorkBuffer:=WorkBuffer + '[ERROR] ';
  end;
 
 {Add Prefix}
 WorkBuffer:=WorkBuffer + 'UART: ';
 
 {Check UART}
 if UART <> nil then
  begin
   WorkBuffer:=WorkBuffer + UART_NAME_PREFIX + IntToStr(UART.UARTId) + ': ';
  end;

 {Output Logging}  
 LoggingOutputEx(LOGGING_FACILITY_UART,LogLevelToLoggingSeverity(Level),'UART',WorkBuffer + AText);
end;

{==============================================================================}

procedure UARTLogInfo(UART:PUARTDevice;const AText:String); inline;
begin
 {}
 UARTLog(UART_LOG_LEVEL_INFO,UART,AText);
end;

{==============================================================================}

procedure UARTLogWarn(UART:PUARTDevice;const AText:String); inline;
begin
 {}
 UARTLog(UART_LOG_LEVEL_WARN,UART,AText);
end;

{==============================================================================}

procedure UARTLogError(UART:PUARTDevice;const AText:String); inline;
begin
 {}
 UARTLog(UART_LOG_LEVEL_ERROR,UART,AText);
end;

{==============================================================================}

procedure UARTLogDebug(UART:PUARTDevice;const AText:String); inline;
begin
 {}
 UARTLog(UART_LOG_LEVEL_DEBUG,UART,AText);
end;

{==============================================================================}
{==============================================================================}
{UART Serial Helper Functions}
function UARTSerialDeviceReceive(UART:PUARTDevice):LongWord;
{Read data from a UART device into the receive buffer of the associated Serial device}
{Note: Not intended to be called directly by applications}
var
 Lock:Boolean;
 Data:Pointer;
 Count:LongWord;
 Added:LongWord;
 Available:LongWord;
 Serial:PSerialDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check UART}
 if UART = nil then Exit;
 if UART.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {Get Serial}
 Serial:=UART.Serial;
 if Serial = nil then Exit;
 if Serial.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IFDEF SERIAL_DEBUG}
 if SERIAL_LOG_ENABLED then SerialLogDebug(Serial,'UART: Device Receive');
 {$ENDIF}
 
 {Check Read}
 if not Assigned(UART.DeviceRead) then Exit;
 
 {Check Mode}
 Result:=ERROR_INVALID_FUNCTION;
 if UART.UARTMode = UART_MODE_UART then Exit;
 
 {Check Enabled}
 Result:=ERROR_NOT_SUPPORTED;
 if UART.UARTState <> UART_STATE_ENABLED then Exit;
 
 {Check Lock}
 Lock:=(MutexOwner(UART.Lock) <> ThreadGetCurrent);
 
 {Acquire the Lock (UART)}
 if Lock and (MutexLock(UART.Lock) <> ERROR_SUCCESS) then
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
   Exit;
  end;
 
 {Acquire the Lock (Serial)}
 if MutexLock(Serial.Lock) = ERROR_SUCCESS then
  begin
   {Setup Count}
   Count:=0;
  
   {Start Write}
   Data:=SerialBufferWriteStart(@Serial.Receive,Available);
   while (Data <> nil) and (Available > 0) do
    begin
     {Read from UART}
     UART.DeviceRead(UART,Data,Available,UART_READ_NON_BLOCK,Added);
     
     {Complete Write}
     if Added > 0 then SerialBufferWriteComplete(@Serial.Receive,Added);
     
     {Update Count}
     Inc(Count,Added);
     
     {Check Added}
     if Added < Available then Break;
     
     {Start Write}
     Data:=SerialBufferWriteStart(@Serial.Receive,Available);
    end; 
 
   {Check Count}
   if Count > 0 then
    begin
     {Set Event}
     EventSet(Serial.Receive.Wait);
    end;
 
   Result:=ERROR_SUCCESS;
   
   {Release the Lock (Serial)}
   MutexUnlock(Serial.Lock);
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;      
 
 if Lock then
  begin
   {Release the Lock (UART))}
   MutexUnlock(UART.Lock);
  end; 
end;

{==============================================================================}

function UARTSerialDeviceTransmit(UART:PUARTDevice):LongWord;
{Write data to a UART device from the transmit buffer of the associated Serial device}
{Note: Not intended to be called directly by applications}
var
 Lock:Boolean;
 Data:Pointer;
 Count:LongWord;
 Removed:LongWord;
 Available:LongWord;
 Serial:PSerialDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check UART}
 if UART = nil then Exit;
 if UART.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {Get Serial}
 Serial:=UART.Serial;
 if Serial = nil then Exit;
 if Serial.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IFDEF SERIAL_DEBUG}
 if SERIAL_LOG_ENABLED then SerialLogDebug(Serial,'UART: Device Transmit');
 {$ENDIF}
 
 {Check Write}
 if not Assigned(UART.DeviceWrite) then Exit;
 
 {Check Mode}
 Result:=ERROR_INVALID_FUNCTION;
 if UART.UARTMode = UART_MODE_UART then Exit;
 
 {Check Enabled}
 Result:=ERROR_NOT_SUPPORTED;
 if UART.UARTState <> UART_STATE_ENABLED then Exit;
 
 {Check Lock}
 Lock:=(MutexOwner(UART.Lock) <> ThreadGetCurrent);
 
 {Acquire the Lock (UART)}
 if Lock and (MutexLock(UART.Lock) <> ERROR_SUCCESS) then
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
   Exit;
  end;
 
 {Acquire the Lock (Serial)}
 if MutexLock(Serial.Lock) = ERROR_SUCCESS then
  begin
   {Setup Count}
   Count:=0;
   
   {Start Read}
   Data:=SerialBufferReadStart(@Serial.Transmit,Available);
   while (Data <> nil) and (Available > 0) do
    begin
     {Write to UART}
     UART.DeviceWrite(UART,Data,Available,UART_WRITE_NON_BLOCK,Removed);
     
     {Complete Read}
     if Removed > 0 then SerialBufferReadComplete(@Serial.Transmit,Removed);
     
     {Update Count}
     Inc(Count,Removed);
     
     {Check Removed}
     if Removed < Available then Break;
     
     {Start Read}
     Data:=SerialBufferReadStart(@Serial.Transmit,Available);
    end; 
   
   {Check Count}
   if Count > 0 then
    begin
     {Set Event}
     EventSet(Serial.Transmit.Wait);
    end;
    
   Result:=ERROR_SUCCESS;
   
   {Release the Lock (Serial)}
   MutexUnlock(Serial.Lock);
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;      
  
 if Lock then
  begin
   {Release the Lock (UART))}
   MutexUnlock(UART.Lock);
  end; 
end;

{==============================================================================}
{==============================================================================}

initialization
 UARTInit;

{==============================================================================}
 
finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.