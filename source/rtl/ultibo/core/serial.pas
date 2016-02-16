{
Ultibo Serial interface unit.

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


Serial Devices
==============

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit Serial;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,SysUtils;

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
const
 {Serial specific constants}
 SERIAL_NAME_PREFIX = 'Serial';  {Name prefix for Serial Devices}
 
 {SERIAL Device Types}
 SERIAL_TYPE_NONE      = 0;
 
 {SERIAL Device States}
 SERIAL_STATE_DISABLED = 0;
 SERIAL_STATE_ENABLED  = 1;
 
 {SERIAL Device Flags}
 SERIAL_FLAG_NONE          = $00000000;
 
 {SERIAL logging}
 SERIAL_LOG_LEVEL_DEBUG     = LOG_LEVEL_DEBUG;  {Serial debugging messages}
 SERIAL_LOG_LEVEL_INFO      = LOG_LEVEL_INFO;   {Serial informational messages, such as a device being attached or detached}
 SERIAL_LOG_LEVEL_ERROR     = LOG_LEVEL_ERROR;  {Serial error messages}
 SERIAL_LOG_LEVEL_NONE      = LOG_LEVEL_NONE;   {No Serial messages}

var 
 SERIAL_DEFAULT_LOG_LEVEL:LongWord = SERIAL_LOG_LEVEL_INFO; {Minimum level for Serial messages.  Only messages with level greater than or equal to this will be printed}
 
var 
 {SERIAL logging}
 SERIAL_LOG_ENABLED:Boolean; 

{==============================================================================}
type
 {Serial specific types}
 
 {Serial Properties}
 PSerialProperties = ^TSerialProperties;
 TSerialProperties = record
  //To Do
 end;
 
 {Serial Device}
 PSerialDevice = ^TSerialDevice;
 
 {Serial Enumeration Callback}
 TSerialEnumerate = function(Serial:PSerialDevice;Data:Pointer):LongWord;
 {Serial Notification Callback}
 TSerialNotification = function(Device:PDevice;Data:Pointer;Notification:LongWord):LongWord;
 
 {Serial Device Methods}
 //To Do
 TSerialDeviceProperties = function(Serial:PSerialDevice;var Properties:PSerialProperties):LongWord;
 
 TSerialDevice = record
  {Device Properties}
  Device:TDevice;                                 {The Device entry for this Serial}
  {Serial Properties}
  SerialId:LongWord;                              {Unique Id of this Serial device in the Serial device table}
  SerialState:LongWord;                           {Serial state (eg SERIAL_STATE_ENABLED)}
  //To Do
  DeviceProperties:TSerialDeviceProperties;       {A Device specific DeviceProperties method implementing the standard Serial device interface}
  {Statistics Properties}
  //To Do
  {Driver Properties}
  Lock:TMutexHandle;                              {Device lock}
  //To Do
  Properties:TSerialProperties;                   {Device properties}
  {Internal Properties}                                                                     
  Prev:PSerialDevice;                             {Previous entry in Serial table}
  Next:PSerialDevice;                             {Next entry in Serial table}
 end; 

{==============================================================================}
{var}
 {Serial specific variables}
 
{==============================================================================}
{Initialization Functions}
procedure SerialInit;

{==============================================================================}
{Serial Functions}

//To Do

function SerialDeviceProperties(Serial:PSerialDevice;var Properties:PSerialProperties):LongWord;
  
function SerialDeviceCreate:PSerialDevice;
function SerialDeviceCreateEx(Size:LongWord):PSerialDevice;
function SerialDeviceDestroy(Serial:PSerialDevice):LongWord;

function SerialDeviceRegister(Serial:PSerialDevice):LongWord;
function SerialDeviceDeregister(Serial:PSerialDevice):LongWord;

function SerialDeviceFind(SerialId:LongWord):PSerialDevice;
function SerialDeviceEnumerate(Callback:TSerialEnumerate;Data:Pointer):LongWord;
 
function SerialDeviceNotification(Serial:PSerialDevice;Callback:TSerialNotification;Data:Pointer;Notification,Flags:LongWord):LongWord;

{==============================================================================}
{RTL Serial Functions}

{==============================================================================}
{Serial Helper Functions}
function SerialGetCount:LongWord; inline;
function SerialDeviceGetDefault:PSerialDevice; inline;

function SerialDeviceCheck(Serial:PSerialDevice):PSerialDevice;

procedure SerialLog(Level:LongWord;Serial:PSerialDevice;const AText:String);
procedure SerialLogInfo(Serial:PSerialDevice;const AText:String); inline;
procedure SerialLogError(Serial:PSerialDevice;const AText:String); inline;
procedure SerialLogDebug(Serial:PSerialDevice;const AText:String); inline;

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

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure SerialInit;
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
 
 {Register Platform Serial Handlers}
 //To Do
 
 SerialInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{Serial Functions}

//To Do

{==============================================================================}
 
function SerialDeviceProperties(Serial:PSerialDevice;var Properties:PSerialProperties):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 //To Do
 
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
 Result.SerialState:=SERIAL_STATE_DISABLED;
 //To Do
 Result.DeviceProperties:=nil;
 Result.Lock:=INVALID_HANDLE_VALUE;
 
 {Create Lock}
 Result.Lock:=MutexCreate;
 if Result.Lock = INVALID_HANDLE_VALUE then
  begin
   if SERIAL_LOG_ENABLED then SerialLogError(nil,'Failed to create lock for Serial device');
   SerialDeviceDestroy(Result);
   Result:=nil;
   Exit;
  end;
end;

{==============================================================================}

function SerialDeviceDestroy(Serial:PSerialDevice):LongWord;
{Destroy an existing Serial entry}
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
var
 SerialId:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Serial}
 if Serial = nil then Exit;
 if Serial.SerialId <> DEVICE_ID_ANY then Exit;
 if Serial.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
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

function SerialDeviceEnumerate(Callback:TSerialEnumerate;Data:Pointer):LongWord;
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
{RTL Serial Functions}

{==============================================================================}
{==============================================================================}
{Serial Helper Functions}
function SerialGetCount:LongWord; inline;
{Get the current Serial count}
begin
 {}
 Result:=SerialDeviceTableCount;
end;

{==============================================================================}

function SerialDeviceGetDefault:PSerialDevice; inline;
{Get the current default Serial device}
begin
 {}
 Result:=SerialDeviceDefault;
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
{==============================================================================}

initialization
 SerialInit;

{==============================================================================}
 
finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
