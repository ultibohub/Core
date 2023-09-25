{
Ultibo Serial interface unit.

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


Serial Devices
==============

 Serial devices represent a communication device that can be both read and written and has a set
 of common properties and capabilities. The underlying device may be an actual UART or it may be
 some other form of device such as a USB to Serial converter. As long as the device can implement
 the common capabilities then it can be accessed as a serial device without regard to the actual
 implementation.
 
 Each serial device returns a set of properties that describe the capabilities of the device and
 includes a set of flags that indicate what features are supported.
 
 Reads from and writes to serial devices are buffered so that varying data transfer rates can be
 accommodated and both reads and writes allow for non blocking so that a caller can avoid waiting
 for received data to be available or the device to be ready to transmit.
 
 This unit also implements the serial logging device which can be configured via parameters in the
 GlobalConfig unit or from the command line.
 
}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit Serial;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,Logging,SysUtils;

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
const
 {Serial specific constants}
 SERIAL_NAME_PREFIX = 'Serial';  {Name prefix for Serial Devices}

 SERIAL_LOGGING_DESCRIPTION = 'Serial Logging';
 
 SERIAL_RECEIVE_DEPTH_DEFAULT  =  SIZE_2K; {Default receive buffer size in bytes}
 SERIAL_TRANSMIT_DEPTH_DEFAULT =  SIZE_2K; {Default transmit buffer size in bytes}
 
 SERIAL_PUSH_TIMEOUT = 50; {Timeout (Milliseconds) for Push RX/TX (Implementation specific)}
 
 {Serial Device Types}
 SERIAL_TYPE_NONE      = 0;
 SERIAL_TYPE_UART      = 1;
 SERIAL_TYPE_USB       = 2;
 
 SERIAL_TYPE_MAX       = 2;
  
 {Serial Type Names}
 SERIAL_TYPE_NAMES:array[SERIAL_TYPE_NONE..SERIAL_TYPE_MAX] of String = (
  'SERIAL_TYPE_NONE',
  'SERIAL_TYPE_UART',
  'SERIAL_TYPE_USB');
 
 {Serial Device States}
 SERIAL_STATE_CLOSED  = 0;
 SERIAL_STATE_CLOSING = 1;
 SERIAL_STATE_OPENING = 2;
 SERIAL_STATE_OPEN    = 3;
 
 SERIAL_STATE_MAX     = 3;
 
 {Serial State Names}
 SERIAL_STATE_NAMES:array[SERIAL_STATE_CLOSED..SERIAL_STATE_MAX] of String = (
  'SERIAL_STATE_CLOSED',
  'SERIAL_STATE_CLOSING',
  'SERIAL_STATE_OPENING',
  'SERIAL_STATE_OPEN');
 
 {Serial Device Flags}
 SERIAL_FLAG_NONE         = $00000000;
 SERIAL_FLAG_DATA_8BIT    = $00000001; {Device supports 8 data bits}
 SERIAL_FLAG_DATA_7BIT    = $00000002; {Device supports 7 data bits}
 SERIAL_FLAG_DATA_6BIT    = $00000004; {Device supports 6 data bits}
 SERIAL_FLAG_DATA_5BIT    = $00000008; {Device supports 5 data bits}
 SERIAL_FLAG_STOP_1BIT    = $00000010; {Device supports 1 stop bit}
 SERIAL_FLAG_STOP_2BIT    = $00000020; {Device supports 2 stop bits}
 SERIAL_FLAG_STOP_1BIT5   = $00000040; {Device supports 1.5 stop bits}
 SERIAL_FLAG_PARITY_ODD   = $00000080; {Device supports odd parity}
 SERIAL_FLAG_PARITY_EVEN  = $00000100; {Device supports even parity}
 SERIAL_FLAG_PARITY_MARK  = $00000200; {Device supports mark parity}
 SERIAL_FLAG_PARITY_SPACE = $00000400; {Device supports space parity}
 SERIAL_FLAG_FLOW_RTS_CTS = $00000800; {Device supports RTS/CTS flow control}
 SERIAL_FLAG_FLOW_DSR_DTR = $00001000; {Device supports DSR/DTR flow control}
 SERIAL_FLAG_PUSH_RX      = $00002000; {Device requires pushed receive (Implementation specific)}
 SERIAL_FLAG_PUSH_TX      = $00004000; {Device requires pushed transmit (Implementation specific)}
 
 {Serial Read Flags}
 SERIAL_READ_NONE        = $00000000;
 SERIAL_READ_NON_BLOCK   = $00000001; {Do not block when reading, if the buffer is empty return immediately}
 SERIAL_READ_PEEK_BUFFER = $00000002; {Return the number of bytes available in the receive buffer without reading them}
 
 {Serial Write Flags}
 SERIAL_WRITE_NONE        = $00000000;
 SERIAL_WRITE_NON_BLOCK   = $00000001; {Do not block when writing, if the buffer is full return immediately}
 SERIAL_WRITE_PEEK_BUFFER = $00000002; {Return the number of bytes free in the transmit buffer without writing anything}
 
 {Serial Wait Directions}
 SERIAL_WAIT_NONE     = 0;
 SERIAL_WAIT_RECEIVE  = 1; {Wait for data to be available in the receive buffer}
 SERIAL_WAIT_TRANSMIT = 2; {Wait for space to be available in the transmit buffer}
 
 {Serial Flush Flags}
 SERIAL_FLUSH_NONE     = $00000000;
 SERIAL_FLUSH_RECEIVE  = $00000001; {Flush the receive buffer}
 SERIAL_FLUSH_TRANSMIT = $00000002; {Flush the transmit buffer}
 
 {Serial Status Flags}
 SERIAL_STATUS_NONE          = $00000000;
 SERIAL_STATUS_RTS           = $00000001; {RTS (Request to Send) is set (If applicable)}
 SERIAL_STATUS_CTS           = $00000002; {CTS (Clear to Send) is set (If applicable)}
 SERIAL_STATUS_DSR           = $00000004; {DSR (Data Set Ready) is set (If applicable)}
 SERIAL_STATUS_DTR           = $00000008; {DTR (Data Terminal Ready) is set (If applicable)}
 SERIAL_STATUS_RX_FULL       = $00000010; {Receive buffer is full}
 SERIAL_STATUS_RX_EMPTY      = $00000020; {Receive buffer is empty}
 SERIAL_STATUS_TX_FULL       = $00000040; {Transmit buffer is full}
 SERIAL_STATUS_TX_EMPTY      = $00000080; {Transmit buffer is empty}
 SERIAL_STATUS_BUSY          = $00000100; {Device is busy}
 SERIAL_STATUS_BREAK_ERROR   = $00000200; {Break error reported}
 SERIAL_STATUS_PARITY_ERROR  = $00000400; {Parity error reported}
 SERIAL_STATUS_FRAMING_ERROR = $00000800; {Framing error reported}
 SERIAL_STATUS_OVERRUN_ERROR = $00001000; {Overrun error reported}
 SERIAL_STATUS_DCD           = $00002000; {DCD (Data Carrier Detect) is set (If applicable)}
 SERIAL_STATUS_RI            = $00004000; {RI (Ring Indicator) is set (If applicable)}
 
 {Serial logging}
 SERIAL_LOG_LEVEL_DEBUG     = LOG_LEVEL_DEBUG;  {Serial debugging messages}
 SERIAL_LOG_LEVEL_INFO      = LOG_LEVEL_INFO;   {Serial informational messages, such as a device being attached or detached}
 SERIAL_LOG_LEVEL_WARN      = LOG_LEVEL_WARN;   {Serial warning messages}
 SERIAL_LOG_LEVEL_ERROR     = LOG_LEVEL_ERROR;  {Serial error messages}
 SERIAL_LOG_LEVEL_NONE      = LOG_LEVEL_NONE;   {No Serial messages}

var 
 SERIAL_DEFAULT_LOG_LEVEL:LongWord = SERIAL_LOG_LEVEL_DEBUG; {Minimum level for Serial messages.  Only messages with level greater than or equal to this will be printed}
 
var 
 {Serial logging}
 SERIAL_LOG_ENABLED:Boolean; 

{==============================================================================}
const
 {Serial Logging specific constants}
 SERIAL_LOGGING_LINE_END = Chr(13) + Chr(10); {CR LF}
 
{==============================================================================}
type
 {Serial specific types}
 
 {Serial Properties}
 PSerialProperties = ^TSerialProperties;
 TSerialProperties = record
  Flags:LongWord;         {Device flags (eg SERIAL_FLAG_DATA_8BIT)}
  MinRate:LongWord;       {Minimum supported baud rate (0 for any rate supported)}
  MaxRate:LongWord;       {Maximum supported baud rate (0 for any rate supported)}
  BaudRate:LongWord;      {Current baud rate setting}
  DataBits:LongWord;      {Current data bits setting}
  StopBits:LongWord;      {Current stop bits setting}
  Parity:LongWord;        {Current parity setting} 
  FlowControl:LongWord;   {Current flow control setting}
  ReceiveDepth:LongWord;  {Current receive depth setting}
  TransmitDepth:LongWord; {Current transmit depth setting}
 end;
 
 {Serial Buffer}
 PSerialBuffer = ^TSerialBuffer;
 TSerialBuffer = record
  Wait:TEventHandle;         {Data ready / Buffer free event}
  Start:LongWord;            {Index of first byte in buffer}
  Count:LongWord;            {Number of bytes in buffer}
  Size:LongWord;             {Size of buffer}
  Data:Pointer;              {Buffered data}
 end;
 
 {Serial Device}
 PSerialDevice = ^TSerialDevice;
 
 {Serial Enumeration Callback}
 TSerialEnumerate = function(Serial:PSerialDevice;Data:Pointer):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 {Serial Notification Callback}
 TSerialNotification = function(Device:PDevice;Data:Pointer;Notification:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 
 {Serial Device Methods}
 TSerialDeviceOpen = function(Serial:PSerialDevice;BaudRate,DataBits,StopBits,Parity,FlowControl,ReceiveDepth,TransmitDepth:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TSerialDeviceClose = function(Serial:PSerialDevice):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 
 TSerialDeviceRead = function(Serial:PSerialDevice;Buffer:Pointer;Size,Flags:LongWord;var Count:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TSerialDeviceWrite = function(Serial:PSerialDevice;Buffer:Pointer;Size,Flags:LongWord;var Count:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 
 TSerialDeviceWait = function(Serial:PSerialDevice;Direction,Timeout:LongWord;var Count:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TSerialDeviceFlush = function(Serial:PSerialDevice;Flags:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 
 TSerialDeviceGetStatus = function(Serial:PSerialDevice):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TSerialDeviceSetStatus = function(Serial:PSerialDevice;Status:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 
 TSerialDeviceGetProperties = function(Serial:PSerialDevice;Properties:PSerialProperties):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TSerialDeviceSetProperties = function(Serial:PSerialDevice;Properties:PSerialProperties):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 
 TSerialDevice = record
  {Device Properties}
  Device:TDevice;                                 {The Device entry for this Serial}
  {Serial Properties}
  SerialId:LongWord;                              {Unique Id of this Serial device in the Serial device table}
  SerialState:LongWord;                           {Serial state (eg SERIAL_STATE_OPEN)}
  SerialStatus:LongWord;                          {Serial status (eg SERIAL_STATUS_RX_FULL)(May not be real time status depending on the driver)}
  DeviceOpen:TSerialDeviceOpen;                   {A Device specific DeviceOpen method implementing the standard Serial device interface (Mandatory)}
  DeviceClose:TSerialDeviceClose;                 {A Device specific DeviceClose method implementing the standard Serial device interface (Mandatory)}
  DeviceRead:TSerialDeviceRead;                   {A Device specific DeviceRead method implementing the standard Serial device interface (Mandatory)}
  DeviceWrite:TSerialDeviceWrite;                 {A Device specific DeviceWrite method implementing the standard Serial device interface (Mandatory)}
  DeviceWait:TSerialDeviceWait;                   {A Device specific DeviceWait method implementing the standard Serial device interface (Or nil if the default method is suitable)}
  DeviceFlush:TSerialDeviceFlush;                 {A Device specific DeviceFlush method implementing the standard Serial device interface (Or nil if the default method is suitable)}
  DeviceGetStatus:TSerialDeviceGetStatus;         {A Device specific DeviceGetStatus method implementing the standard Serial device interface (Or nil if the default method is suitable)}
  DeviceSetStatus:TSerialDeviceSetStatus;         {A Device specific DeviceSetStatus method implementing the standard Serial device interface (Optional)}
  DeviceGetProperties:TSerialDeviceGetProperties; {A Device specific DeviceGetProperties method implementing the standard Serial device interface (Or nil if the default method is suitable)}
  DeviceSetProperties:TSerialDeviceSetProperties; {A Device specific DeviceSetProperties method implementing the standard Serial device interface (Or nil if the default method is suitable)}
  {Driver Properties}
  Lock:TMutexHandle;                              {Device lock}
  Receive:TSerialBuffer;                          {Serial receive buffer}
  Transmit:TSerialBuffer;                         {Serial transmit buffer}
  Properties:TSerialProperties;                   {Device properties}
  {Statistics Properties}
  ReceiveCount:LongWord;
  ReceiveErrors:LongWord;
  ReceiveOverruns:LongWord;  
  TransmitCount:LongWord;
  TransmitErrors:LongWord;
  TransmitOverruns:LongWord;
  {Internal Properties}                                                                     
  Prev:PSerialDevice;                             {Previous entry in Serial table}
  Next:PSerialDevice;                             {Next entry in Serial table}
 end; 

{==============================================================================}
type
 {Serial Logging specific types}
 PSerialLogging = ^TSerialLogging;
 
 {Serial Logging}
 TSerialLogging = record
  {Logging Properties}
  Logging:TLoggingDevice;
  {Serial Properties}
  Serial:PSerialDevice;
  BaudRate:LongWord;
  DataBits:LongWord;
  StopBits:LongWord;
  Parity:LongWord;
  FlowControl:LongWord;
 end;
 
{==============================================================================}
{var}
 {Serial specific variables}
 
{==============================================================================}
{Initialization Functions}
procedure SerialInit;

{==============================================================================}
{Serial Functions}
function SerialDeviceOpen(Serial:PSerialDevice;BaudRate,DataBits,StopBits,Parity,FlowControl,ReceiveDepth,TransmitDepth:LongWord):LongWord;
function SerialDeviceClose(Serial:PSerialDevice):LongWord;

function SerialDeviceRead(Serial:PSerialDevice;Buffer:Pointer;Size,Flags:LongWord;var Count:LongWord):LongWord;
function SerialDeviceWrite(Serial:PSerialDevice;Buffer:Pointer;Size,Flags:LongWord;var Count:LongWord):LongWord;

function SerialDeviceWait(Serial:PSerialDevice;Direction,Timeout:LongWord;var Count:LongWord):LongWord;
function SerialDeviceFlush(Serial:PSerialDevice;Flags:LongWord):LongWord;

function SerialDeviceStatus(Serial:PSerialDevice):LongWord; inline;
function SerialDeviceGetStatus(Serial:PSerialDevice):LongWord;
function SerialDeviceSetStatus(Serial:PSerialDevice;Status:LongWord):LongWord;

function SerialDeviceProperties(Serial:PSerialDevice;Properties:PSerialProperties):LongWord; inline;
function SerialDeviceGetProperties(Serial:PSerialDevice;Properties:PSerialProperties):LongWord;
function SerialDeviceSetProperties(Serial:PSerialDevice;Properties:PSerialProperties):LongWord;
  
function SerialDeviceCreate:PSerialDevice;
function SerialDeviceCreateEx(Size:LongWord):PSerialDevice;
function SerialDeviceDestroy(Serial:PSerialDevice):LongWord;

function SerialDeviceRegister(Serial:PSerialDevice):LongWord;
function SerialDeviceDeregister(Serial:PSerialDevice):LongWord;

function SerialDeviceFind(SerialId:LongWord):PSerialDevice;
function SerialDeviceFindByName(const Name:String):PSerialDevice; inline;
function SerialDeviceFindByDescription(const Description:String):PSerialDevice; inline;
function SerialDeviceEnumerate(Callback:TSerialEnumerate;Data:Pointer):LongWord;
 
function SerialDeviceNotification(Serial:PSerialDevice;Callback:TSerialNotification;Data:Pointer;Notification,Flags:LongWord):LongWord;

{==============================================================================}
{Serial Logging Functions}
function SerialLoggingStart(Logging:PLoggingDevice):LongWord;
function SerialLoggingStop(Logging:PLoggingDevice):LongWord;

function SerialLoggingOutput(Logging:PLoggingDevice;const Data:String):LongWord;

function SerialLoggingSetTarget(Logging:PLoggingDevice;const Target:String):LongWord;

{==============================================================================}
{RTL Text IO Functions}
function SysTextIOReadChar(var ACh:Char;AUserData:Pointer):Boolean;
function SysTextIOWriteChar(ACh:Char;AUserData:Pointer):Boolean;
function SysTextIOWriteBuffer(ABuffer:PChar;ACount:LongInt;AUserData:Pointer):LongInt;

{==============================================================================}
{RTL Serial Functions}
function SysSerialAvailable:Boolean;
 
function SysSerialOpen(BaudRate,DataBits,StopBits,Parity,FlowControl,ReceiveDepth,TransmitDepth:LongWord):LongWord;
function SysSerialClose:LongWord;
  
function SysSerialRead(Buffer:Pointer;Size:LongWord;var Count:LongWord):LongWord;
function SysSerialWrite(Buffer:Pointer;Size:LongWord;var Count:LongWord):LongWord;

{==============================================================================}
{Serial Helper Functions}
function SerialGetCount:LongWord;

function SerialDeviceGetDefault:PSerialDevice;
function SerialDeviceSetDefault(Serial:PSerialDevice):LongWord; 

function SerialDeviceCheck(Serial:PSerialDevice):PSerialDevice;

function SerialTypeToString(SerialType:LongWord):String;
function SerialStateToString(SerialState:LongWord):String;

function SerialDeviceRedirectInput(Serial:PSerialDevice):Boolean; 
function SerialDeviceRedirectOutput(Serial:PSerialDevice):Boolean; 

function SerialBufferReadStart(Buffer:PSerialBuffer;var Available:LongWord):Pointer;
function SerialBufferReadComplete(Buffer:PSerialBuffer;Removed:LongWord):Boolean;

function SerialBufferWriteStart(Buffer:PSerialBuffer;var Available:LongWord):Pointer;
function SerialBufferWriteComplete(Buffer:PSerialBuffer;Added:LongWord):Boolean;

procedure SerialLog(Level:LongWord;Serial:PSerialDevice;const AText:String);
procedure SerialLogInfo(Serial:PSerialDevice;const AText:String); inline;
procedure SerialLogWarn(Serial:PSerialDevice;const AText:String); inline;
procedure SerialLogError(Serial:PSerialDevice;const AText:String); inline;
procedure SerialLogDebug(Serial:PSerialDevice;const AText:String); inline;

function SerialDataBitsToString(Bits:LongWord):String;
function SerialStopBitsToString(Bits:LongWord):String;
function SerialParityToString(Parity:LongWord):String;
function SerialFlowControlToString(Flow:LongWord):String;

{==============================================================================}
{Serial Logging Helper Functions}
function SerialLoggingDeviceAdd(Serial:PSerialDevice):LongWord;
function SerialLoggingDeviceRemove(Serial:PSerialDevice):LongWord;
function SerialLoggingDeviceParameters(Serial:PSerialDevice;const Parameters:String;var BaudRate,Parity,DataBits,StopBits:LongWord):LongWord;

function SerialLoggingDeviceEnum(Serial:PSerialDevice;Data:Pointer):LongWord;
function SerialLoggingDeviceNotify(Device:PDevice;Data:Pointer;Notification:LongWord):LongWord;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {Serial specific variables}
 SerialInitialized:Boolean;

 SerialDeviceTable:PSerialDevice;
 SerialDeviceTableLock:TCriticalSectionHandle = INVALID_HANDLE_VALUE;
 SerialDeviceTableCount:LongWord;

 SerialDeviceDefault:PSerialDevice;

 SerialTextIOInputDevice:PSerialDevice;
 SerialTextIOOutputDevice:PSerialDevice;
 
{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure SerialInit;
{Initialize the Serial unit and Serial device table}

{Note: Called only during system startup}
var
 WorkInt:LongWord;
 WorkBuffer:String;
begin
 {}
 {Check Initialized}
 if SerialInitialized then Exit;
 
 {Initialize Logging}
 SERIAL_LOG_ENABLED:=(SERIAL_DEFAULT_LOG_LEVEL <> SERIAL_LOG_LEVEL_NONE); 
 
 {Initialize Serial Table}
 SerialDeviceTable:=nil;
 SerialDeviceTableLock:=CriticalSectionCreate; 
 SerialDeviceTableCount:=0;
 if SerialDeviceTableLock = INVALID_HANDLE_VALUE then
  begin
   if SERIAL_LOG_ENABLED then SerialLogError(nil,'Failed to create Serial table lock');
  end;
 SerialDeviceDefault:=nil;
 
 {Check Environment Variables}
 {SERIAL_REGISTER_LOGGING}
 WorkInt:=StrToIntDef(SysUtils.GetEnvironmentVariable('SERIAL_REGISTER_LOGGING'),0);
 if WorkInt <> 0 then SERIAL_REGISTER_LOGGING:=True;
 
 {SERIAL_LOGGING_DEFAULT}
 WorkInt:=StrToIntDef(SysUtils.GetEnvironmentVariable('SERIAL_LOGGING_DEFAULT'),0);
 if WorkInt <> 0 then SERIAL_LOGGING_DEFAULT:=True;
 
 {SERIAL_LOGGING_PARAMETERS}
 WorkBuffer:=SysUtils.GetEnvironmentVariable('SERIAL_LOGGING_PARAMETERS');
 if Length(WorkBuffer) <> 0 then SERIAL_LOGGING_PARAMETERS:=WorkBuffer;
 
 {Enumerate Serial Devices}
 SerialDeviceEnumerate(SerialLoggingDeviceEnum,nil);
 
 {Register Notification}
 SerialDeviceNotification(nil,SerialLoggingDeviceNotify,nil,DEVICE_NOTIFICATION_REGISTER or DEVICE_NOTIFICATION_DEREGISTER,NOTIFIER_FLAG_WORKER);
 
 {Register Platform Text IO Handlers}
 {TextIOReadCharHandler:=SysTextIOReadChar;}       {Only registered when calling SerialDeviceRedirectInput}
 {TextIOWriteCharHandler:=SysTextIOWriteChar;}     {Only registered when calling SerialDeviceRedirectOutput}
 {TextIOWriteBufferHandler:=SysTextIOWriteBuffer;} {Only registered when calling SerialDeviceRedirectOutput}
 
 {Register Platform Serial Handlers}
 SerialAvailableHandler:=SysSerialAvailable;
 SerialOpenHandler:=SysSerialOpen;
 SerialCloseHandler:=SysSerialClose;
 SerialReadHandler:=SysSerialRead;
 SerialWriteHandler:=SysSerialWrite;
 
 SerialInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{Serial Functions}
function SerialDeviceOpen(Serial:PSerialDevice;BaudRate,DataBits,StopBits,Parity,FlowControl,ReceiveDepth,TransmitDepth:LongWord):LongWord;
{Open a Serial device ready for sending and receiving}
{Serial: The Serial device to open}
{BaudRate: Baud rate for the connection (eg 9600, 57600, 115200 etc}
{DataBits: Size of the data (eg SERIAL_DATA_8BIT)}
{StopBits: Number of stop bits (eg SERIAL_STOP_1BIT)}
{Parity: Parity type for the data (eg SERIAL_PARITY_NONE)}
{FlowControl: Flow control for the connection (eg SERIAL_FLOW_NONE)}
{ReceiveDepth: Size of the receive buffer (0 = Default size)}
{TransmitDepth: Size of the transmit buffer (0 = Default size)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Serial}
 if Serial = nil then Exit;
 if Serial.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IFDEF SERIAL_DEBUG}
 if SERIAL_LOG_ENABLED then SerialLogDebug(Serial,'Serial Device Open (BaudRate=' + IntToStr(BaudRate) + ' DataBits=' + SerialDataBitsToString(DataBits) + ' StopBits=' + SerialStopBitsToString(StopBits) + ' Parity=' + SerialParityToString(Parity) + ' FlowControl=' + SerialFlowControlToString(FlowControl) + ')');
 {$ENDIF}
 
 {Check State}
 Result:=ERROR_ALREADY_OPEN;
 if Serial.SerialState <> SERIAL_STATE_CLOSED then Exit;
 
 if MutexLock(Serial.Lock) = ERROR_SUCCESS then
  begin
   try
    if Assigned(Serial.DeviceOpen) then
     begin
      {Set State to Opening}
      Serial.SerialState:=SERIAL_STATE_OPENING;
      
      {Notify Opening}
      NotifierNotify(@Serial.Device,DEVICE_NOTIFICATION_OPENING);
     
      {Call Device Open}
      Result:=Serial.DeviceOpen(Serial,BaudRate,DataBits,StopBits,Parity,FlowControl,ReceiveDepth,TransmitDepth);
      if Result <> ERROR_SUCCESS then
       begin
        {Reset State to Closed}
        Serial.SerialState:=SERIAL_STATE_CLOSED;
        Exit;
       end; 
     end
    else
     begin    
      Result:=ERROR_INVALID_PARAMETER;
      Exit;
     end; 
    
    {Set State to Open}
    Serial.SerialState:=SERIAL_STATE_OPEN;
    
    {Notify Open}
    NotifierNotify(@Serial.Device,DEVICE_NOTIFICATION_OPEN);
 
    Result:=ERROR_SUCCESS;
   finally
    MutexUnlock(Serial.Lock);
   end; 
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;    
end;

{==============================================================================}

function SerialDeviceClose(Serial:PSerialDevice):LongWord;
{Close a Serial device and terminate sending and receiving}
{Serial: The Serial device to close}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Serial}
 if Serial = nil then Exit;
 if Serial.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IFDEF SERIAL_DEBUG}
 if SERIAL_LOG_ENABLED then SerialLogDebug(Serial,'Serial Device Close');
 {$ENDIF}
 
 {Check State}
 Result:=ERROR_NOT_OPEN;
 if Serial.SerialState <> SERIAL_STATE_OPEN then Exit;
 
 if MutexLock(Serial.Lock) = ERROR_SUCCESS then
  begin
   try
    if Assigned(Serial.DeviceClose) then
     begin
      {Set State to Closing}
      Serial.SerialState:=SERIAL_STATE_CLOSING;
      
      {Notify Closing}
      NotifierNotify(@Serial.Device,DEVICE_NOTIFICATION_CLOSING);
     
      {Call Device Close}
      Result:=Serial.DeviceClose(Serial);
      if Result <> ERROR_SUCCESS then
       begin
        {Reset State to Open}
        Serial.SerialState:=SERIAL_STATE_OPEN;
        Exit;
       end; 
     end
    else
     begin
      Result:=ERROR_INVALID_PARAMETER;
      Exit;
     end;    
 
    {Set State to Closed}
    Serial.SerialState:=SERIAL_STATE_CLOSED;
    
    {Notify Close}
    NotifierNotify(@Serial.Device,DEVICE_NOTIFICATION_CLOSE);
    
    Result:=ERROR_SUCCESS;
   finally
    MutexUnlock(Serial.Lock);
   end; 
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;    
end;

{==============================================================================}

function SerialDeviceRead(Serial:PSerialDevice;Buffer:Pointer;Size,Flags:LongWord;var Count:LongWord):LongWord;
{Read data from a Serial device}
{Serial: The Serial device to read from}
{Buffer: Pointer to a buffer to receive the data}
{Size: The size of the buffer}
{Flags: The flags to control reading (eg SERIAL_READ_NON_BLOCK)}
{Count: The number of bytes read on return}
{Return: ERROR_SUCCESS if completed or another error code on failure}
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
 if SERIAL_LOG_ENABLED then SerialLogDebug(Serial,'Serial Device Read (Size=' + IntToStr(Size) + ')');
 {$ENDIF}
 
 {Check State}
 Result:=ERROR_NOT_READY;
 if Serial.SerialState <> SERIAL_STATE_OPEN then Exit;
 
 if MutexLock(Serial.Lock) = ERROR_SUCCESS then
  begin
   if Assigned(Serial.DeviceRead) then
    begin
     {Call Device Read}
     Result:=Serial.DeviceRead(Serial,Buffer,Size,Flags,Count);
    end
   else
    begin
     Result:=ERROR_INVALID_PARAMETER;
    end;    
    
   MutexUnlock(Serial.Lock);
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;    
end;

{==============================================================================}

function SerialDeviceWrite(Serial:PSerialDevice;Buffer:Pointer;Size,Flags:LongWord;var Count:LongWord):LongWord;
{Write data to a Serial device}
{Serial: The Serial device to write to}
{Buffer: Pointer to a buffer of data to transmit}
{Size: The size of the buffer}
{Flags: The flags to control writing (eg SERIAL_WRITE_NON_BLOCK)}
{Count: The number of bytes written on return}
{Return: ERROR_SUCCESS if completed or another error code on failure}
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
 if SERIAL_LOG_ENABLED then SerialLogDebug(Serial,'Serial Device Write (Size=' + IntToStr(Size) + ')');
 {$ENDIF}
 
 {Check State}
 Result:=ERROR_NOT_READY;
 if Serial.SerialState <> SERIAL_STATE_OPEN then Exit;
 
 if MutexLock(Serial.Lock) = ERROR_SUCCESS then
  begin
   if Assigned(Serial.DeviceWrite) then
    begin
     {Call Device Write}
     Result:=Serial.DeviceWrite(Serial,Buffer,Size,Flags,Count);
    end
   else
    begin
     Result:=ERROR_INVALID_PARAMETER;
    end;    
    
   MutexUnlock(Serial.Lock);
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;    
end;

{==============================================================================}

function SerialDeviceWait(Serial:PSerialDevice;Direction,Timeout:LongWord;var Count:LongWord):LongWord;
{Wait for data to be available in the receive or transmit buffers of a Serial device}
{Serial: The Serial device to wait for}
{Direction: The direction of data to wait for (eg SERIAL_WAIT_RECEIVE)}
{Timeout: The number of milliseconds to wait for data (INFINITE to wait forever)}
{Count: The number of bytes available on return}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Unlock:Boolean;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Serial}
 if Serial = nil then Exit;
 if Serial.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {$IFDEF SERIAL_DEBUG}
 if SERIAL_LOG_ENABLED then SerialLogDebug(Serial,'Serial Device Wait (Direction=' + IntToStr(Direction) + ' Timeout=' + IntToStr(Timeout) + ')');
 {$ENDIF}

 {Check Timeout}
 if Timeout = 0 then Timeout:=INFINITE;
 
 {Check State}
 Result:=ERROR_NOT_READY;
 if Serial.SerialState <> SERIAL_STATE_OPEN then Exit;
 
 if MutexLock(Serial.Lock) = ERROR_SUCCESS then
  begin
   try
    Unlock:=True;
    
    if Assigned(Serial.DeviceWait) then
     begin
      {Call Device Wait}
      Result:=Serial.DeviceWait(Serial,Direction,Timeout,Count);
     end
    else
     begin 
      {Setup Result}
      Count:=0;
      
      {Check Receive}
      if Direction = SERIAL_WAIT_RECEIVE then
       begin
        {Release the Lock}
        MutexUnlock(Serial.Lock);
        Unlock:=False;
        
        {Wait for Data}
        Result:=EventWaitEx(Serial.Receive.Wait,Timeout);
        if Result <> ERROR_SUCCESS then Exit;

        {Acquire the Lock}
        if MutexLock(Serial.Lock) = ERROR_SUCCESS then
         begin
          Unlock:=True;
          
          {Get Count}
          Count:=Serial.Receive.Count;
          
          {Return Result}
          Result:=ERROR_SUCCESS;
         end
        else
         begin
          Result:=ERROR_CAN_NOT_COMPLETE;
         end;
       end
      {Check Transmit}
      else if Direction = SERIAL_WAIT_TRANSMIT then
       begin
        {Release the Lock}
        MutexUnlock(Serial.Lock);
        Unlock:=False;
        
        {Wait for Space}
        Result:=EventWaitEx(Serial.Transmit.Wait,Timeout);
        if Result <> ERROR_SUCCESS then Exit;
        
        {Acquire the Lock}
        if MutexLock(Serial.Lock) = ERROR_SUCCESS then
         begin
          Unlock:=True;
          
          {Get Count}
          Count:=Serial.Transmit.Size - Serial.Transmit.Count;
          
          {Return Result}
          Result:=ERROR_SUCCESS;
         end
        else
         begin
          Result:=ERROR_CAN_NOT_COMPLETE;
         end;
       end;
     end;
   finally
    if Unlock then MutexUnlock(Serial.Lock);
   end; 
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function SerialDeviceFlush(Serial:PSerialDevice;Flags:LongWord):LongWord;
{Discard the contents of the receive and/or transmit buffers of a Serial device}
{Serial: The Serial device to flush}
{Flags: The flags to indicate what to flush (eg SERIAL_FLUSH_RECEIVE)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Serial}
 if Serial = nil then Exit;
 if Serial.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {$IFDEF SERIAL_DEBUG}
 if SERIAL_LOG_ENABLED then SerialLogDebug(Serial,'Serial Device Flush (Flags=' + IntToHex(Flags,8) + ')');
 {$ENDIF}
 
 {Check State}
 Result:=ERROR_NOT_READY;
 if Serial.SerialState <> SERIAL_STATE_OPEN then Exit;
 
 if MutexLock(Serial.Lock) = ERROR_SUCCESS then
  begin
   try
    if Assigned(Serial.DeviceFlush) then
     begin
      {Call Device Flush}
      Result:=Serial.DeviceFlush(Serial,Flags);
     end
    else
     begin 
      {Check Receive}
      if (Flags and SERIAL_FLUSH_RECEIVE) <> 0 then
       begin
        {Flush Receive}
        EventReset(Serial.Receive.Wait);
        Serial.Receive.Start:=0;
        Serial.Receive.Count:=0;
       end;
      
      {Check Transmit}
      if (Flags and SERIAL_FLUSH_TRANSMIT) <> 0 then  
       begin
        {Flush Transmit}
        EventSet(Serial.Transmit.Wait);
        Serial.Transmit.Start:=0;
        Serial.Transmit.Count:=0;
       end;
      
      {Return Result}
      Result:=ERROR_SUCCESS;
     end;
   finally
    MutexUnlock(Serial.Lock);
   end; 
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function SerialDeviceStatus(Serial:PSerialDevice):LongWord; inline;
{Get the current line status of a Serial device}
{Serial: The Serial device to get the status from}
{Return: A set of flags containing the device status (eg SERIAL_STATUS_RTS)}

{Note: Replaced by SerialDeviceGetStatus for consistency}
begin
 {}
 Result:=SerialDeviceGetStatus(Serial);
end;

{==============================================================================}

function SerialDeviceGetStatus(Serial:PSerialDevice):LongWord;
{Get the current line status of a Serial device}
{Serial: The Serial device to get the status from}
{Return: A set of flags containing the device status (eg SERIAL_STATUS_RTS)}
begin
 {}
 Result:=SERIAL_STATUS_NONE;
 
 {Check Serial}
 if Serial = nil then Exit;
 if Serial.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IFDEF SERIAL_DEBUG}
 if SERIAL_LOG_ENABLED then SerialLogDebug(Serial,'Serial Device Get Status');
 {$ENDIF}
 
 {Check State}
 if Serial.SerialState <> SERIAL_STATE_OPEN then Exit;
 
 if MutexLock(Serial.Lock) = ERROR_SUCCESS then
  begin
   if Assigned(Serial.DeviceGetStatus) then
    begin
     {Call Device Get Status}
     Result:=Serial.DeviceGetStatus(Serial);
    end
   else
    begin
     {Get Status}
     Result:=Serial.SerialStatus;
    end;  
    
   MutexUnlock(Serial.Lock);
  end;
end;

{==============================================================================}

function SerialDeviceSetStatus(Serial:PSerialDevice;Status:LongWord):LongWord;
{Set the current line status of a Serial device}
{Serial: The Serial device to set the status for}
{Status: The device status flags to be set (eg SERIAL_STATUS_RTS)}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: Not all SERIAL_STATUS_* flags can be set, the device may ignore invalid values}
{Note: Not all serial devices support set status, returns ERROR_CALL_NOT_IMPLEMENTED if not supported}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Serial}
 if Serial = nil then Exit;
 if Serial.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {$IFDEF SERIAL_DEBUG}
 if SERIAL_LOG_ENABLED then SerialLogDebug(Serial,'Serial Device Set Status (Status=' + IntToHex(Status,8) + ')');
 {$ENDIF}

 {Check State}
 Result:=ERROR_NOT_READY;
 if Serial.SerialState <> SERIAL_STATE_OPEN then Exit;
 
 if MutexLock(Serial.Lock) = ERROR_SUCCESS then
  begin
   if Assigned(Serial.DeviceSetStatus) then
    begin
     {Call Device Set Status}
     Result:=Serial.DeviceSetStatus(Serial,Status);
    end
   else
    begin
     Result:=ERROR_CALL_NOT_IMPLEMENTED;
    end;  
    
   MutexUnlock(Serial.Lock);
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}
 
function SerialDeviceProperties(Serial:PSerialDevice;Properties:PSerialProperties):LongWord; inline;
{Get the properties for the specified Serial device}
{Serial: The Serial device to get properties from}
{Properties: Pointer to a PSerialProperties structure to fill in}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: Replaced by SerialDeviceGetProperties for consistency}
begin
 {}
 Result:=SerialDeviceGetProperties(Serial,Properties);
end;

{==============================================================================}

function SerialDeviceGetProperties(Serial:PSerialDevice;Properties:PSerialProperties):LongWord;
{Get the properties for the specified Serial device}
{Serial: The Serial device to get properties from}
{Properties: Pointer to a PSerialProperties structure to fill in}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Properties}
 if Properties = nil then Exit;
 
 {Check Serial}
 if Serial = nil then Exit;
 if Serial.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IFDEF SERIAL_DEBUG}
 if SERIAL_LOG_ENABLED then SerialLogDebug(Serial,'Serial Device Get Properties');
 {$ENDIF}
 
 {Check Open}
 {Result:=ERROR_NOT_READY;}
 {if Serial.SerialState <> SERIAL_STATE_OPEN then Exit;} {Allow when closed}
 
 if MutexLock(Serial.Lock) = ERROR_SUCCESS then
  begin
   if Assigned(Serial.DeviceGetProperties) then
    begin
     {Call Device Get Properites}
     Result:=Serial.DeviceGetProperties(Serial,Properties);
    end
   else
    begin
     {Get Properties}
     System.Move(Serial.Properties,Properties^,SizeOf(TSerialProperties));
       
     {Return Result}
     Result:=ERROR_SUCCESS;
    end;  
    
   MutexUnlock(Serial.Lock);
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;    
end;

{==============================================================================}

function SerialDeviceSetProperties(Serial:PSerialDevice;Properties:PSerialProperties):LongWord;
{Set the properties for the specified Serial device}
{Serial: The Serial device to set properties for}
{Properties: Pointer to a PSerialProperties structure to use}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Properties}
 if Properties = nil then Exit;
 
 {Check Serial}
 if Serial = nil then Exit;
 if Serial.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IFDEF SERIAL_DEBUG}
 if SERIAL_LOG_ENABLED then SerialLogDebug(Serial,'Serial Device Set Properties');
 {$ENDIF}
 
 {Check Open}
 Result:=ERROR_NOT_READY;
 if Serial.SerialState <> SERIAL_STATE_OPEN then Exit;
 
 if MutexLock(Serial.Lock) = ERROR_SUCCESS then
  begin
   if Assigned(Serial.DeviceSetProperties) then
    begin
     {Call Device Set Properites}
     Result:=Serial.DeviceSetProperties(Serial,Properties);
    end
   else
    begin
     {Close Device}
     SerialDeviceClose(Serial);
     
     {Open Device}
     Result:=SerialDeviceOpen(Serial,Properties.BaudRate,Properties.DataBits,Properties.StopBits,Properties.Parity,Properties.FlowControl,Properties.ReceiveDepth,Properties.TransmitDepth);
    end;  
    
   MutexUnlock(Serial.Lock);
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;    
end;

{==============================================================================}

function SerialDeviceCreate:PSerialDevice;
{Create a new Serial entry}
{Return: Pointer to new Serial entry or nil if Serial could not be created}
begin
 {}
 Result:=SerialDeviceCreateEx(SizeOf(TSerialDevice));
end;

{==============================================================================}

function SerialDeviceCreateEx(Size:LongWord):PSerialDevice;
{Create a new Serial entry}
{Size: Size in bytes to allocate for new Serial (Including the Serial entry)}
{Return: Pointer to new Serial entry or nil if Serial could not be created}
begin
 {}
 Result:=nil;
 
 {Check Size}
 if Size < SizeOf(TSerialDevice) then Exit;
 
 {Create Serial}
 Result:=PSerialDevice(DeviceCreateEx(Size));
 if Result = nil then Exit;
 
 {Update Device}
 Result.Device.DeviceBus:=DEVICE_BUS_NONE;   
 Result.Device.DeviceType:=SERIAL_TYPE_NONE;
 Result.Device.DeviceFlags:=SERIAL_FLAG_NONE;
 Result.Device.DeviceData:=nil;

 {Update Serial}
 Result.SerialId:=DEVICE_ID_ANY;
 Result.SerialState:=SERIAL_STATE_CLOSED;
 Result.SerialStatus:=SERIAL_STATUS_NONE;
 Result.DeviceOpen:=nil;
 Result.DeviceClose:=nil;
 Result.DeviceRead:=nil;
 Result.DeviceWrite:=nil;
 Result.DeviceWait:=nil;
 Result.DeviceFlush:=nil;
 Result.DeviceGetStatus:=nil;
 Result.DeviceSetStatus:=nil;
 Result.DeviceGetProperties:=nil;
 Result.DeviceSetProperties:=nil;
 Result.Lock:=INVALID_HANDLE_VALUE;
 Result.Receive.Wait:=INVALID_HANDLE_VALUE;
 Result.Transmit.Wait:=INVALID_HANDLE_VALUE;
 
 {Create Lock}
 Result.Lock:=MutexCreateEx(False,MUTEX_DEFAULT_SPINCOUNT,MUTEX_FLAG_RECURSIVE);
 if Result.Lock = INVALID_HANDLE_VALUE then
  begin
   if SERIAL_LOG_ENABLED then SerialLogError(nil,'Failed to create lock for serial device');
   SerialDeviceDestroy(Result);
   Result:=nil;
   Exit;
  end;
  
 {Create Receive Event (Manual Reset)}
 Result.Receive.Wait:=EventCreate(True,False);
 if Result.Receive.Wait = INVALID_HANDLE_VALUE then
  begin
   if SERIAL_LOG_ENABLED then SerialLogError(nil,'Failed to create receive event for serial device');
   SerialDeviceDestroy(Result);
   Result:=nil;
   Exit;
  end;
  
 {Create Transmit Event (Manual Reset / Intitial State)}
 Result.Transmit.Wait:=EventCreate(True,True);
 if Result.Transmit.Wait = INVALID_HANDLE_VALUE then
  begin
   if SERIAL_LOG_ENABLED then SerialLogError(nil,'Failed to create transmit event for serial device');
   SerialDeviceDestroy(Result);
   Result:=nil;
   Exit;
  end;
end;

{==============================================================================}

function SerialDeviceDestroy(Serial:PSerialDevice):LongWord;
{Destroy an existing Serial entry}
{Serial: The serial device to destroy}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Serial}
 if Serial = nil then Exit;
 if Serial.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check Serial}
 Result:=ERROR_IN_USE;
 if SerialDeviceCheck(Serial) = Serial then Exit;

 {Check State}
 if Serial.Device.DeviceState <> DEVICE_STATE_UNREGISTERED then Exit;
 
 {Destroy Transmit Event}
 if Serial.Transmit.Wait <> INVALID_HANDLE_VALUE then
  begin
   EventDestroy(Serial.Transmit.Wait);
  end;
 
 {Destroy Receive Event}
 if Serial.Receive.Wait <> INVALID_HANDLE_VALUE then
  begin
   EventDestroy(Serial.Receive.Wait);
  end;
 
 {Destroy Lock}
 if Serial.Lock <> INVALID_HANDLE_VALUE then
  begin
   MutexDestroy(Serial.Lock);
  end;
 
 {Destroy Serial} 
 Result:=DeviceDestroy(@Serial.Device);
end;

{==============================================================================}

function SerialDeviceRegister(Serial:PSerialDevice):LongWord;
{Register a new Serial in the Serial table}
{Serial: The serial device to register}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 SerialId:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Serial}
 if Serial = nil then Exit;
 if Serial.SerialId <> DEVICE_ID_ANY then Exit;
 if Serial.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check Interfaces}
 if not(Assigned(Serial.DeviceOpen)) then Exit;
 if not(Assigned(Serial.DeviceClose)) then Exit;
 if not(Assigned(Serial.DeviceRead)) then Exit;
 if not(Assigned(Serial.DeviceWrite)) then Exit;
 
 {Check Serial}
 Result:=ERROR_ALREADY_EXISTS;
 if SerialDeviceCheck(Serial) = Serial then Exit;
 
 {Check State}
 if Serial.Device.DeviceState <> DEVICE_STATE_UNREGISTERED then Exit;
 
 {Insert Serial}
 if CriticalSectionLock(SerialDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Update Serial}
    SerialId:=0;
    while SerialDeviceFind(SerialId) <> nil do
     begin
      Inc(SerialId);
     end;
    Serial.SerialId:=SerialId;
    
    {Update Device}
    Serial.Device.DeviceName:=SERIAL_NAME_PREFIX + IntToStr(Serial.SerialId); 
    Serial.Device.DeviceClass:=DEVICE_CLASS_SERIAL;
    
    {Register Device}
    Result:=DeviceRegister(@Serial.Device);
    if Result <> ERROR_SUCCESS then
     begin
      Serial.SerialId:=DEVICE_ID_ANY;
      Exit;
     end; 
    
    {Link Serial}
    if SerialDeviceTable = nil then
     begin
      SerialDeviceTable:=Serial;
     end
    else
     begin
      Serial.Next:=SerialDeviceTable;
      SerialDeviceTable.Prev:=Serial;
      SerialDeviceTable:=Serial;
     end;
 
    {Increment Count}
    Inc(SerialDeviceTableCount);
    
    {Check Default}
    if SerialDeviceDefault = nil then
     begin
      SerialDeviceDefault:=Serial;
     end;
    
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    CriticalSectionUnlock(SerialDeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;

{==============================================================================}

function SerialDeviceDeregister(Serial:PSerialDevice):LongWord;
{Deregister a Serial from the Serial table}
{Serial: The serial device to deregister}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Prev:PSerialDevice;
 Next:PSerialDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Serial}
 if Serial = nil then Exit;
 if Serial.SerialId = DEVICE_ID_ANY then Exit;
 if Serial.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check Serial}
 Result:=ERROR_NOT_FOUND;
 if SerialDeviceCheck(Serial) <> Serial then Exit;
 
 {Check State}
 if Serial.Device.DeviceState <> DEVICE_STATE_REGISTERED then Exit;
 
 {Remove Serial}
 if CriticalSectionLock(SerialDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Deregister Device}
    Result:=DeviceDeregister(@Serial.Device);
    if Result <> ERROR_SUCCESS then Exit;
    
    {Unlink Serial}
    Prev:=Serial.Prev;
    Next:=Serial.Next;
    if Prev = nil then
     begin
      SerialDeviceTable:=Next;
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
    Dec(SerialDeviceTableCount);
 
    {Check Default}
    if SerialDeviceDefault = Serial then
     begin
      SerialDeviceDefault:=SerialDeviceTable;
     end;
 
    {Update Serial}
    Serial.SerialId:=DEVICE_ID_ANY;
 
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    CriticalSectionUnlock(SerialDeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;

{==============================================================================}

function SerialDeviceFind(SerialId:LongWord):PSerialDevice;
{Find a serial device by ID in the serial table}
{SerialId: The ID number of the serial to find}
{Return: Pointer to serial device entry or nil if not found}
var
 Serial:PSerialDevice;
begin
 {}
 Result:=nil;
 
 {Check Id}
 if SerialId = DEVICE_ID_ANY then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(SerialDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Serial}
    Serial:=SerialDeviceTable;
    while Serial <> nil do
     begin
      {Check State}
      if Serial.Device.DeviceState = DEVICE_STATE_REGISTERED then
       begin
        {Check Id}
        if Serial.SerialId = SerialId then
         begin
          Result:=Serial;
          Exit;
         end;
       end;
       
      {Get Next}
      Serial:=Serial.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(SerialDeviceTableLock);
   end;
  end;
end;
     
{==============================================================================}

function SerialDeviceFindByName(const Name:String):PSerialDevice; inline;
{Find a serial device by name in the serial table}
{Name: The name of the serial to find (eg Serial0)}
{Return: Pointer to serial device entry or nil if not found}
begin
 {}
 Result:=PSerialDevice(DeviceFindByName(Name));
end;

{==============================================================================}

function SerialDeviceFindByDescription(const Description:String):PSerialDevice; inline;
{Find a serial device by description in the serial table}
{Description: The description of the serial to find (eg BCM2836 PL011 UART)}
{Return: Pointer to serial device entry or nil if not found}
begin
 {}
 Result:=PSerialDevice(DeviceFindByDescription(Description));
end;
       
{==============================================================================}

function SerialDeviceEnumerate(Callback:TSerialEnumerate;Data:Pointer):LongWord;
{Enumerate all serial devices in the serial table}
{Callback: The callback function to call for each serial in the table}
{Data: A private data pointer to pass to callback for each serial in the table}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Serial:PSerialDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Callback}
 if not Assigned(Callback) then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(SerialDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Serial}
    Serial:=SerialDeviceTable;
    while Serial <> nil do
     begin
      {Check State}
      if Serial.Device.DeviceState = DEVICE_STATE_REGISTERED then
       begin
        if Callback(Serial,Data) <> ERROR_SUCCESS then Exit;
       end;
       
      {Get Next}
      Serial:=Serial.Next;
     end;
     
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    CriticalSectionUnlock(SerialDeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;

{==============================================================================}

function SerialDeviceNotification(Serial:PSerialDevice;Callback:TSerialNotification;Data:Pointer;Notification,Flags:LongWord):LongWord;
{Register a notification for serial device changes}
{Serial: The serial device to notify changes for (Optional, pass nil for all serial devices)}
{Callback: The function to call when a notification event occurs}
{Data: A private data pointer to pass to callback when a notification event occurs}
{Notification: The events to register for notification of (eg DEVICE_NOTIFICATION_REGISTER)}
{Flags: The flags to control the notification (eg NOTIFIER_FLAG_WORKER)}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Serial}
 if Serial = nil then
  begin
   Result:=DeviceNotification(nil,DEVICE_CLASS_SERIAL,Callback,Data,Notification,Flags);
  end
 else
  begin 
   {Check Serial}
   if Serial.Device.Signature <> DEVICE_SIGNATURE then Exit;

   Result:=DeviceNotification(@Serial.Device,DEVICE_CLASS_SERIAL,Callback,Data,Notification,Flags);
  end; 
end;

{==============================================================================}
{==============================================================================}
{Serial Logging Functions}
function SerialLoggingStart(Logging:PLoggingDevice):LongWord;
{Implementation of LoggingDeviceStart API for Serial Logging}
{Note: Not intended to be called directly by applications, use LoggingDeviceStart instead}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Logging}
 if Logging = nil then Exit;
 if Logging.Device.Signature <> DEVICE_SIGNATURE then Exit; 

 if MutexLock(Logging.Lock) = ERROR_SUCCESS then 
  begin
   try
    {Check Logging}
    if Logging.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
    {Check Serial}
    if PSerialLogging(Logging).Serial = nil then Exit;
    
    {Open Serial}
    Result:=SerialDeviceOpen(PSerialLogging(Logging).Serial,PSerialLogging(Logging).BaudRate,PSerialLogging(Logging).DataBits,PSerialLogging(Logging).StopBits,PSerialLogging(Logging).Parity,PSerialLogging(Logging).FlowControl,0,0);
   finally
    MutexUnlock(Logging.Lock);
   end; 
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function SerialLoggingStop(Logging:PLoggingDevice):LongWord;
{Implementation of LoggingDeviceStop API for Serial Logging}
{Note: Not intended to be called directly by applications, use LoggingDeviceStop instead}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Logging}
 if Logging = nil then Exit;
 if Logging.Device.Signature <> DEVICE_SIGNATURE then Exit; 

 if MutexLock(Logging.Lock) = ERROR_SUCCESS then 
  begin
   try
    {Check Logging}
    if Logging.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
    {Check Serial}
    if PSerialLogging(Logging).Serial = nil then Exit;
 
    {Close Serial}
    Result:=SerialDeviceClose(PSerialLogging(Logging).Serial);
   finally
    MutexUnlock(Logging.Lock);
   end; 
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function SerialLoggingOutput(Logging:PLoggingDevice;const Data:String):LongWord;
{Implementation of LoggingDeviceOutput API for Serial Logging}
{Note: Not intended to be called directly by applications, use LoggingDeviceOutput instead}
var
 Count:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Logging}
 if Logging = nil then Exit;
 if Logging.Device.Signature <> DEVICE_SIGNATURE then Exit; 

 if MutexLock(Logging.Lock) = ERROR_SUCCESS then 
  begin
   try
    {Check Logging}
    if Logging.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
    {Check Serial}
    if PSerialLogging(Logging).Serial = nil then Exit;
 
    {Serial Write}
    Result:=SerialDeviceWrite(PSerialLogging(Logging).Serial,PChar(Data),Length(Data),SERIAL_WRITE_NONE,Count);
    if Result <> ERROR_SUCCESS then Exit;
    
    {Serial Write}
    Result:=SerialDeviceWrite(PSerialLogging(Logging).Serial,PChar(SERIAL_LOGGING_LINE_END),Length(SERIAL_LOGGING_LINE_END),SERIAL_WRITE_NONE,Count);
    if Result <> ERROR_SUCCESS then Exit;
    
    {Update Statistics}
    Inc(Logging.OutputCount);
    
    Result:=ERROR_SUCCESS;
   finally
    MutexUnlock(Logging.Lock);
   end; 
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function SerialLoggingSetTarget(Logging:PLoggingDevice;const Target:String):LongWord;
{Implementation of LoggingDeviceSetTarget API for Serial Logging}
{Note: Not intended to be called directly by applications, use LoggingDeviceSetTarget instead}
var
 Serial:PSerialDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Logging}
 if Logging = nil then Exit;
 if Logging.Device.Signature <> DEVICE_SIGNATURE then Exit; 

 if MutexLock(Logging.Lock) = ERROR_SUCCESS then 
  begin
   try
    {Check Logging}
    if Logging.Device.Signature <> DEVICE_SIGNATURE then Exit;

    {Check Target}
    if Logging.Target <> Target then
     begin
      {Check Name}
      Serial:=SerialDeviceFindByName(Target);
      if Serial = nil then
       begin
        {Check Description}
        Serial:=SerialDeviceFindByDescription(Target);
       end;

      {Check Device}
      if Serial = nil then Exit;

      {Close Serial}
      SerialDeviceClose(PSerialLogging(Logging).Serial);

      {Set Target}
      Logging.Target:=Target;
      UniqueString(Logging.Target);

      {Update Parameters}
      PSerialLogging(Logging).Serial:=Serial;
      PSerialLogging(Logging).FlowControl:=SERIAL_FLOW_NONE;
      SerialLoggingDeviceParameters(Serial,SERIAL_LOGGING_PARAMETERS,PSerialLogging(Logging).BaudRate,PSerialLogging(Logging).Parity,PSerialLogging(Logging).DataBits,PSerialLogging(Logging).StopBits);

      {Open Serial}
      Result:=SerialDeviceOpen(PSerialLogging(Logging).Serial,PSerialLogging(Logging).BaudRate,PSerialLogging(Logging).DataBits,PSerialLogging(Logging).StopBits,PSerialLogging(Logging).Parity,PSerialLogging(Logging).FlowControl,0,0);
      if Result <> ERROR_SUCCESS then Exit;
     end;

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    MutexUnlock(Logging.Lock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}
{==============================================================================}
{RTL Text IO Functions}
function SysTextIOReadChar(var ACh:Char;AUserData:Pointer):Boolean;
{Handler for platform TextIOReadChar function}

{Note: Not intended to be called directly by applications}
var
 Count:LongWord;
begin
 {}
 Result:=(SerialDeviceRead(SerialTextIOInputDevice,@ACh,SizeOf(Char),SERIAL_READ_NONE,Count) = ERROR_SUCCESS);
end;

{==============================================================================}

function SysTextIOWriteChar(ACh:Char;AUserData:Pointer):Boolean;
{Handler for platform TextIOWriteChar function}

{Note: Not intended to be called directly by applications}
var
 Count:LongWord;
begin
 {}
 Result:=(SerialDeviceWrite(SerialTextIOOutputDevice,@ACh,SizeOf(Char),SERIAL_WRITE_NONE,Count) = ERROR_SUCCESS);
end;

{==============================================================================}

function SysTextIOWriteBuffer(ABuffer:PChar;ACount:LongInt;AUserData:Pointer):LongInt;
{Handler for platform TextIOWriteBuffer function}

{Note: Not intended to be called directly by applications}
var
 Count:LongWord;
begin
 {}
 Result:=0;
 
 if SerialDeviceWrite(SerialTextIOOutputDevice,ABuffer,ACount,SERIAL_WRITE_NONE,Count) = ERROR_SUCCESS then
  begin
   Result:=Count;
  end;
end;
 
{==============================================================================}
{==============================================================================}
{RTL Serial Functions}
function SysSerialAvailable:Boolean;
{Check if a Serial device is available}
begin
 {}
 Result:=(SerialDeviceDefault <> nil);
end;

{==============================================================================}

function SysSerialOpen(BaudRate,DataBits,StopBits,Parity,FlowControl,ReceiveDepth,TransmitDepth:LongWord):LongWord;
{Open the default Serial device ready for sending and receiving}
{BaudRate: Baud rate for the connection (eg 9600, 57600, 115200 etc}
{DataBits: Size of the data (eg SERIAL_DATA_8BIT)}
{StopBits: Number of stop bits (eg SERIAL_STOP_1BIT)}
{Parity: Parity type for the data (eg SERIAL_PARITY_NONE)}
{FlowControl: Flow control for the connection (eg SERIAL_FLOW_NONE)}
{ReceiveDepth: Size of the receive buffer (0 = Default size)}
{TransmitDepth: Size of the transmit buffer (0 = Default size)}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 if SerialDeviceDefault = nil then Exit;
 
 Result:=SerialDeviceOpen(SerialDeviceDefault,BaudRate,DataBits,StopBits,Parity,FlowControl,ReceiveDepth,TransmitDepth);
end;

{==============================================================================}

function SysSerialClose:LongWord;
{Close the default Serial device and terminate sending and receiving}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 if SerialDeviceDefault = nil then Exit;
 
 Result:=SerialDeviceClose(SerialDeviceDefault);
end;

{==============================================================================}
  
function SysSerialRead(Buffer:Pointer;Size:LongWord;var Count:LongWord):LongWord;
{Read data from the default Serial device}
{Buffer: Pointer to a buffer to receive the data}
{Size: The size of the buffer}
{Count: The number of bytes read on return}
begin
 {}
 {Setup Result}
 Count:=0;
 Result:=ERROR_INVALID_PARAMETER;
 
 if SerialDeviceDefault = nil then Exit;
 
 Result:=SerialDeviceRead(SerialDeviceDefault,Buffer,Size,SERIAL_READ_NONE,Count);
end;

{==============================================================================}

function SysSerialWrite(Buffer:Pointer;Size:LongWord;var Count:LongWord):LongWord;
{Write data to the default Serial device}
{Buffer: Pointer to a buffer of data to transmit}
{Size: The size of the buffer}
{Count: The number of bytes written on return}
begin
 {}
 {Setup Result}
 Count:=0;
 Result:=ERROR_INVALID_PARAMETER;
 
 if SerialDeviceDefault = nil then Exit;
 
 Result:=SerialDeviceWrite(SerialDeviceDefault,Buffer,Size,SERIAL_WRITE_NONE,Count);
end;

{==============================================================================}
{==============================================================================}
{Serial Helper Functions}
function SerialGetCount:LongWord;
{Get the current Serial count}
begin
 {}
 Result:=SerialDeviceTableCount;
end;

{==============================================================================}

function SerialDeviceGetDefault:PSerialDevice;
{Get the current default Serial device}
begin
 {}
 Result:=SerialDeviceDefault;
end;

{==============================================================================}

function SerialDeviceSetDefault(Serial:PSerialDevice):LongWord; 
{Set the current default Serial device}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Serial}
 if Serial = nil then Exit;
 if Serial.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(SerialDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Check Serial}
    if SerialDeviceCheck(Serial) <> Serial then Exit;
    
    {Set Serial Default}
    SerialDeviceDefault:=Serial;
    
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    CriticalSectionUnlock(SerialDeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function SerialDeviceCheck(Serial:PSerialDevice):PSerialDevice;
{Check if the supplied Serial is in the Serial table}
var
 Current:PSerialDevice;
begin
 {}
 Result:=nil;
 
 {Check Serial}
 if Serial = nil then Exit;
 if Serial.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(SerialDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Serial}
    Current:=SerialDeviceTable;
    while Current <> nil do
     begin
      {Check Serial}
      if Current = Serial then
       begin
        Result:=Serial;
        Exit;
       end;
      
      {Get Next}
      Current:=Current.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(SerialDeviceTableLock);
   end;
  end;
end;

{==============================================================================}

function SerialTypeToString(SerialType:LongWord):String;
{Convert a Serial type value to a string}
begin
 {}
 Result:='SERIAL_TYPE_UNKNOWN';
 
 if SerialType <= SERIAL_TYPE_MAX then
  begin
   Result:=SERIAL_TYPE_NAMES[SerialType];
  end;
end;

{==============================================================================}

function SerialStateToString(SerialState:LongWord):String;
{Convert a Serial state value to a string}
begin
 {}
 Result:='SERIAL_STATE_UNKNOWN';
 
 if SerialState <= SERIAL_STATE_MAX then
  begin
   Result:=SERIAL_STATE_NAMES[SerialState];
  end;
end;

{==============================================================================}

function SerialDeviceRedirectInput(Serial:PSerialDevice):Boolean; 
{Redirect standard input to the serial device specified by Serial}
{Serial: The serial device to redirect input to (or nil to stop redirection)}
{Return: True if completed successfully or False if an error occurred}

{Note: Redirects the input of the text file Input which also
       redirects the input of Read, ReadLn and the standard C library}
begin
 {}
 Result:=False;
 
 if Serial = nil then
  begin
   {Stop Redirection}
   TextIOReadCharHandler:=nil;
   
   SerialTextIOInputDevice:=nil;
  end
 else
  begin
   {Check Serial}
   if Serial.Device.Signature <> DEVICE_SIGNATURE then Exit;
  
   {Start Redirection}
   TextIOReadCharHandler:=SysTextIOReadChar;
  
   SerialTextIOInputDevice:=Serial;
  end;  
  
 Result:=True;
end;

{==============================================================================}

function SerialDeviceRedirectOutput(Serial:PSerialDevice):Boolean; 
{Redirect standard output to the serial device specified by Serial}
{Serial: The serial device to redirect output to (or nil to stop redirection)}
{Return: True if completed successfully or False if an error occurred}

{Note: Redirects the output of the text files Output, ErrOutput, StdOut and StdErr
       which also redirects the output of Write, WriteLn and the standard C library}
begin
 {}
 Result:=False;
 
 if Serial = nil then
  begin
   {Stop Redirection}
   TextIOWriteCharHandler:=nil;
   TextIOWriteBufferHandler:=nil;
   
   SerialTextIOOutputDevice:=nil;
  end
 else
  begin
   {Check Serial}
   if Serial.Device.Signature <> DEVICE_SIGNATURE then Exit;
   
   {Start Redirection}
   TextIOWriteCharHandler:=SysTextIOWriteChar;
   TextIOWriteBufferHandler:=SysTextIOWriteBuffer;
  
   SerialTextIOOutputDevice:=Serial;
  end;  
  
 Result:=True;
end;

{==============================================================================}

function SerialBufferReadStart(Buffer:PSerialBuffer;var Available:LongWord):Pointer;
{Return a pointer to the next read from the buffer and the number of bytes that can be read}

{Note: Caller must hold the lock on the serial device which owns the buffer}
begin
 {}
 {Setup Result}
 Available:=0;
 Result:=nil;

 {Check Buffer}
 if Buffer = nil then Exit;
 
 if Buffer.Count > 0 then
  begin
   {Check Wraparound}
   if (Buffer.Start + Buffer.Count) > Buffer.Size then
    begin  
     {Get Available}    
     Available:=Buffer.Count - ((Buffer.Start + Buffer.Count) mod Buffer.Size);
     
     {Get Pointer}
     Result:=Pointer(PtrUInt(Buffer.Data) + PtrUInt(Buffer.Start));
    end
   else
    begin
     {Get Available}
     Available:=Buffer.Count;
     
     {Get Pointer}
     Result:=Pointer(PtrUInt(Buffer.Data) + PtrUInt(Buffer.Start));
    end;
  end;
end;

{==============================================================================}

function SerialBufferReadComplete(Buffer:PSerialBuffer;Removed:LongWord):Boolean;
{Update the buffer to reflect the number of bytes removed when reading}

{Note: Caller must hold the lock on the serial device which owns the buffer}
begin
 {}
 Result:=False;
 
 {Check Buffer}
 if Buffer = nil then Exit;
 
 if Removed <= Buffer.Count then
  begin
   {Update Start}
   Buffer.Start:=(Buffer.Start + Removed) mod Buffer.Size;

   {Update Count}
   Dec(Buffer.Count,Removed);
   
   Result:=True;
  end;
end;

{==============================================================================}

function SerialBufferWriteStart(Buffer:PSerialBuffer;var Available:LongWord):Pointer;
{Return a pointer to the next write to the buffer and the number of bytes that can be written}

{Note: Caller must hold the lock on the serial device which owns the buffer}
begin
 {}
 {Setup Result}
 Available:=0;
 Result:=nil;
 
 {Check Buffer}
 if Buffer = nil then Exit;
 
 if Buffer.Count < Buffer.Size then
  begin
   {Check Wraparound}
   if (Buffer.Start + Buffer.Count) >= Buffer.Size then
    begin
     {Get Available}
     Available:=Buffer.Start - ((Buffer.Start + Buffer.Count) mod Buffer.Size);
     
     {Get Pointer}
     Result:=Pointer(PtrUInt(Buffer.Data) + PtrUInt((Buffer.Start + Buffer.Count) mod Buffer.Size));
    end
   else
    begin
     {Get Available}
     Available:=Buffer.Size - (Buffer.Start + Buffer.Count);
     
     {Get Pointer}
     Result:=Pointer(PtrUInt(Buffer.Data) + PtrUInt((Buffer.Start + Buffer.Count) mod Buffer.Size));
    end;    
  end;
end;

{==============================================================================}

function SerialBufferWriteComplete(Buffer:PSerialBuffer;Added:LongWord):Boolean;
{Update the buffer to reflect the number of bytes added when writing}

{Note: Caller must hold the lock on the serial device which owns the buffer}
begin
 {}
 Result:=False;
 
 {Check Buffer}
 if Buffer = nil then Exit;
 
 if (Buffer.Count + Added) <= Buffer.Size then
  begin
   {Update Count}
   Inc(Buffer.Count,Added);
   
   Result:=True;
  end;
end;

{==============================================================================}

procedure SerialLog(Level:LongWord;Serial:PSerialDevice;const AText:String);
var
 WorkBuffer:String;
begin
 {}
 {Check Level}
 if Level < SERIAL_DEFAULT_LOG_LEVEL then Exit;
 
 WorkBuffer:='';
 {Check Level}
 if Level = SERIAL_LOG_LEVEL_DEBUG then
  begin
   WorkBuffer:=WorkBuffer + '[DEBUG] ';
  end
 else if Level = SERIAL_LOG_LEVEL_WARN then
  begin
   WorkBuffer:=WorkBuffer + '[WARN] ';
  end
 else if Level = SERIAL_LOG_LEVEL_ERROR then
  begin
   WorkBuffer:=WorkBuffer + '[ERROR] ';
  end;
 
 {Add Prefix}
 WorkBuffer:=WorkBuffer + 'Serial: ';
 
 {Check Serial}
 if Serial <> nil then
  begin
   WorkBuffer:=WorkBuffer + SERIAL_NAME_PREFIX + IntToStr(Serial.SerialId) + ': ';
  end;

 {Output Logging}  
 LoggingOutputEx(LOGGING_FACILITY_SERIAL,LogLevelToLoggingSeverity(Level),'Serial',WorkBuffer + AText);
end;

{==============================================================================}

procedure SerialLogInfo(Serial:PSerialDevice;const AText:String); inline;
begin
 {}
 SerialLog(SERIAL_LOG_LEVEL_INFO,Serial,AText);
end;

{==============================================================================}

procedure SerialLogWarn(Serial:PSerialDevice;const AText:String); inline;
begin
 {}
 SerialLog(SERIAL_LOG_LEVEL_WARN,Serial,AText);
end;

{==============================================================================}

procedure SerialLogError(Serial:PSerialDevice;const AText:String); inline;
begin
 {}
 SerialLog(SERIAL_LOG_LEVEL_ERROR,Serial,AText);
end;

{==============================================================================}

procedure SerialLogDebug(Serial:PSerialDevice;const AText:String); inline;
begin
 {}
 SerialLog(SERIAL_LOG_LEVEL_DEBUG,Serial,AText);
end;

{==============================================================================}

function SerialDataBitsToString(Bits:LongWord):String;
begin
 {}
 Result:='SERIAL_DATA_UNKNOWN';
 
 case Bits of
  SERIAL_DATA_8BIT:Result:='SERIAL_DATA_8BIT';
  SERIAL_DATA_7BIT:Result:='SERIAL_DATA_7BIT';
  SERIAL_DATA_6BIT:Result:='SERIAL_DATA_6BIT';
  SERIAL_DATA_5BIT:Result:='SERIAL_DATA_5BIT';
 end;
end;

{==============================================================================}

function SerialStopBitsToString(Bits:LongWord):String;
begin
 {}
 Result:='SERIAL_STOP_UNKNOWN';
 
 case Bits of
  SERIAL_STOP_1BIT:Result:='SERIAL_STOP_1BIT';
  SERIAL_STOP_2BIT:Result:='SERIAL_STOP_2BIT';
  SERIAL_STOP_1BIT5:Result:='SERIAL_STOP_1BIT5';
 end;
end;

{==============================================================================}

function SerialParityToString(Parity:LongWord):String;
begin
 {}
 Result:='SERIAL_PARITY_NONE';
 
 case Parity of
  SERIAL_PARITY_ODD:Result:='SERIAL_PARITY_ODD';
  SERIAL_PARITY_EVEN:Result:='SERIAL_PARITY_EVEN';
  SERIAL_PARITY_MARK:Result:='SERIAL_PARITY_MARK';
  SERIAL_PARITY_SPACE:Result:='SERIAL_PARITY_SPACE';
 end;
end;

{==============================================================================}

function SerialFlowControlToString(Flow:LongWord):String;
begin
 {}
 Result:='SERIAL_FLOW_NONE';
 
 case Flow of
  SERIAL_FLOW_RTS_CTS:Result:='SERIAL_FLOW_RTS_CTS';
  SERIAL_FLOW_DSR_DTR:Result:='SERIAL_FLOW_DSR_DTR';
 end;
end;

{==============================================================================}
{==============================================================================}
{Serial Logging Helper Functions}
function SerialLoggingDeviceAdd(Serial:PSerialDevice):LongWord;
{Add a new serial logging device on receipt of a device register notification}
var
 Status:LongWord;
 Logging:PSerialLogging;
begin
 {}
 Result:=ERROR_SUCCESS;
 
 {Check Logging}
 if LoggingDeviceFindByDevice(@Serial.Device) = nil then
  begin
   {Check Register}
   if SERIAL_REGISTER_LOGGING then
    begin
     {Check Device}
     if Length(SERIAL_LOGGING_DEVICE) <> 0 then
      begin
       {Check Name}
       if SerialDeviceFindByName(SERIAL_LOGGING_DEVICE) <> Serial then
        begin
         {Check Description}
         if SerialDeviceFindByDescription(SERIAL_LOGGING_DEVICE) <> Serial then Exit;
        end;
      end
     else
      begin
       {Check Default}
       if SerialDeviceGetDefault <> Serial then Exit;
      end;

     {Create Logging}
     Logging:=PSerialLogging(LoggingDeviceCreateEx(SizeOf(TSerialLogging),SERIAL_LOGGING_DEFAULT));
     if Logging <> nil then
      begin
       {Update Logging}
       {Device}
       Logging.Logging.Device.DeviceBus:=DEVICE_BUS_NONE; 
       Logging.Logging.Device.DeviceType:=LOGGING_TYPE_SERIAL;
       Logging.Logging.Device.DeviceFlags:=LOGGING_FLAG_NONE;
       Logging.Logging.Device.DeviceData:=@Serial.Device;
       Logging.Logging.Device.DeviceDescription:=SERIAL_LOGGING_DESCRIPTION;
       {Logging}
       Logging.Logging.LoggingState:=LOGGING_STATE_DISABLED;
       Logging.Logging.DeviceStart:=SerialLoggingStart;
       Logging.Logging.DeviceStop:=SerialLoggingStop;
       Logging.Logging.DeviceOutput:=SerialLoggingOutput;
       Logging.Logging.DeviceSetTarget:=SerialLoggingSetTarget;
       Logging.Logging.Target:=DeviceGetName(@Serial.Device);
       {Serial}
       Logging.Serial:=Serial;
       Logging.FlowControl:=SERIAL_FLOW_NONE;
       SerialLoggingDeviceParameters(Serial,SERIAL_LOGGING_PARAMETERS,Logging.BaudRate,Logging.Parity,Logging.DataBits,Logging.StopBits);
       
       {Register Logging}
       Status:=LoggingDeviceRegister(@Logging.Logging);
       if Status = ERROR_SUCCESS then
        begin
         {Start Logging}
         Status:=LoggingDeviceStart(@Logging.Logging);
         if Status <> ERROR_SUCCESS then
          begin
           if DEVICE_LOG_ENABLED then DeviceLogError(nil,'Logging: Failed to start new serial logging device: ' + ErrorToString(Status));

           {Deregister Logging}
           LoggingDeviceDeregister(@Logging.Logging);

           {Destroy Logging}
           LoggingDeviceDestroy(@Logging.Logging);
          end;
        end
       else 
        begin
         if DEVICE_LOG_ENABLED then DeviceLogError(nil,'Logging: Failed to register new serial logging device: ' + ErrorToString(Status));

         {Destroy Logging}
         LoggingDeviceDestroy(@Logging.Logging);
        end;
      end
     else 
      begin
       if DEVICE_LOG_ENABLED then DeviceLogError(nil,'Logging: Failed to create new serial logging device');
      end;
    end;
  end; 
end;

{==============================================================================}

function SerialLoggingDeviceRemove(Serial:PSerialDevice):LongWord;
{Remove a serial logging device on receipt of a device deregister notification}
var
 Status:LongWord;
 Logging:PSerialLogging;
begin
 {}
 Result:=ERROR_SUCCESS;
 
 {Check Logging}
 Logging:=PSerialLogging(LoggingDeviceFindByDevice(@Serial.Device));
 if Logging <> nil then
  begin
   {Stop Logging}
   Status:=LoggingDeviceStop(@Logging.Logging);
   if Status = ERROR_SUCCESS then
    begin
     {Deregister Logging}
     Status:=LoggingDeviceDeregister(@Logging.Logging);
     if Status = ERROR_SUCCESS then
      begin
       {Destroy Logging}
       Status:=LoggingDeviceDestroy(@Logging.Logging);
       if Status <> ERROR_SUCCESS then
        begin
         if DEVICE_LOG_ENABLED then DeviceLogError(nil,'Logging: Failed to destroy serial logging device');
        end; 
      end
     else
      begin
       if DEVICE_LOG_ENABLED then DeviceLogError(nil,'Logging: Failed to deregister serial logging device: ' + ErrorToString(Status));
      end;      
    end
   else
    begin   
     if DEVICE_LOG_ENABLED then DeviceLogError(nil,'Logging: Failed to stop serial logging device: ' + ErrorToString(Status));
    end;
  end;
end;

{==============================================================================}

function SerialLoggingDeviceParameters(Serial:PSerialDevice;const Parameters:String;var BaudRate,Parity,DataBits,StopBits:LongWord):LongWord;
{Break down the serial parameters value into component parts of baud rate, parity, data bits and stop bits}
{The parameters must be in the form 'BaudRate,Parity,DataBits,StopBits' (eg '115200,N,8,1')}

 function SerialLoggingFirstWord(var Value:String;const Delimiter:String):String;
 var
  PosIdx:Integer;
 begin
  {}
  PosIdx:=Pos(Delimiter,Value);
  if PosIdx = 0 then
   begin
    Result:=Value;
    Value:='';
   end
  else
   begin
    Result:=Copy(Value,1,PosIdx - 1);
    Delete(Value,1,PosIdx + (Length(Delimiter) - 1));
   end;
 end;
 
 function SerialLoggingCharCount(const Value:String;Delimiter:Char):Integer;
 var
  Count:Integer;
 begin
  {}
  Result:=0;
  
  if Length(Value) = 0 then Exit;

  for Count:=1 to Length(Value) do
   begin
    if Value[Count] = Delimiter then Inc(Result);
   end;
 end;
 
var
 WorkValue:String;
 WorkBuffer:String;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Setup Defaults}
 BaudRate:=SERIAL_BAUD_RATE_DEFAULT;
 Parity:=SERIAL_PARITY_NONE;
 DataBits:=SERIAL_DATA_8BIT;
 StopBits:=SERIAL_STOP_1BIT;
 
 {Check Parameters}
 if Length(Parameters) = 0 then Exit;
 if SerialLoggingCharCount(Parameters,',') <> 3 then Exit;
 WorkBuffer:=Parameters;
 
 {Check Serial}
 if Serial = nil then Exit;
 
 {Get Baud Rate}
 BaudRate:=StrToIntDef(SerialLoggingFirstWord(WorkBuffer,','),BaudRate);
 
 {Get Parity}
 WorkValue:=Uppercase(SerialLoggingFirstWord(WorkBuffer,','));
 if WorkValue = 'N' then
  begin
   Parity:=SERIAL_PARITY_NONE;
  end
 else if WorkValue = 'O' then 
  begin
   Parity:=SERIAL_PARITY_ODD;
  end
 else if WorkValue = 'E' then 
  begin
   Parity:=SERIAL_PARITY_EVEN;
  end;
  
 {Get Data Bits} 
 DataBits:=StrToIntDef(SerialLoggingFirstWord(WorkBuffer,','),DataBits);
 
 {Get Stop Bits}
 StopBits:=StrToIntDef(SerialLoggingFirstWord(WorkBuffer,','),StopBits);
 
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function SerialLoggingDeviceEnum(Serial:PSerialDevice;Data:Pointer):LongWord;
{Enumeration callback for serial logging initialization}
{Note: Not intended to be called directly by applications}
begin
 {}
 Result:=ERROR_SUCCESS;
 
 {$IF DEFINED(LOGGING_DEBUG) or DEFINED(DEVICE_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Logging: Serial device enumeration');
 {$ENDIF}
 
 {Check Serial}
 if Serial = nil then Exit;
 if Serial.SerialState <> SERIAL_STATE_CLOSED then Exit; 
 
 {Add Serial}
 Result:=SerialLoggingDeviceAdd(Serial);
end;

{==============================================================================}

function SerialLoggingDeviceNotify(Device:PDevice;Data:Pointer;Notification:LongWord):LongWord;
{Notification callback for serial logging device creation or remove}
{Note: Not intended to be called directly by applications}
var
 Serial:PSerialDevice;
begin
 {}
 Result:=ERROR_SUCCESS;
 
 {$IF DEFINED(LOGGING_DEBUG) or DEFINED(DEVICE_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Logging: Serial device notification (Notification=' + NotificationToString(Notification) + ')');
 {$ENDIF}
 
 {Check Device}
 if Device = nil then Exit;
 
 {Get Serial}
 Serial:=PSerialDevice(Device);

 {Check Notification}
 if (Notification and DEVICE_NOTIFICATION_REGISTER) <> 0 then
  begin
   {Check Serial}
   if Serial.SerialState <> SERIAL_STATE_CLOSED then Exit; 
   
   {Add Serial}
   Result:=SerialLoggingDeviceAdd(Serial);
  end
 else if (Notification and DEVICE_NOTIFICATION_DEREGISTER) <> 0 then
  begin
   {Remove Serial}
   Result:=SerialLoggingDeviceRemove(Serial);
  end;
end;

{==============================================================================}
{==============================================================================}

initialization
 SerialInit;

{==============================================================================}
 
finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
