{
Ultibo I2C interface unit.

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


I2C Devices
===========

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
 I2C_NAME_PREFIX = 'I2C';  {Name prefix for I2C Devices}

 {I2C Device Types}
 I2C_TYPE_NONE      = 0;
 
 {I2C Device States}
 I2C_STATE_DISABLED = 0;
 I2C_STATE_ENABLED  = 1;
 
 {I2C Device Flags}
 I2C_FLAG_NONE          = $00000000;
 
 {I2C logging}
 I2C_LOG_LEVEL_DEBUG     = LOG_LEVEL_DEBUG;  {I2C debugging messages}
 I2C_LOG_LEVEL_INFO      = LOG_LEVEL_INFO;   {I2C informational messages, such as a device being attached or detached}
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
  //To Do
 end;
 
 {I2C Device}
 PI2CDevice = ^TI2CDevice;
 
 {I2C Enumeration Callback}
 TI2CEnumerate = function(I2C:PI2CDevice;Data:Pointer):LongWord;
 {I2C Notification Callback}
 TI2CNotification = function(Device:PDevice;Data:Pointer;Notification:LongWord):LongWord;
 
 {I2C Device Methods}
 //To Do
 TI2CDeviceProperties = function(I2C:PI2CDevice;Properties:PI2CProperties):LongWord;
 
 TI2CDevice = record
  {Device Properties}
  Device:TDevice;                                 {The Device entry for this I2C}
  {I2C Properties}
  I2CId:LongWord;                                 {Unique Id of this I2C in the I2C table}
  I2CState:LongWord;                              {I2C state (eg I2C_STATE_ENABLED)}
  //To Do
  DeviceProperties:TI2CDeviceProperties;          {A Device specific DeviceProperties method implementing the standard I2C device interface}
  {Statistics Properties}
  //To Do
  {Driver Properties}
  Lock:TMutexHandle;                              {Device lock}
  //To Do
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
//To Do

function I2CDeviceProperties(I2C:PI2CDevice;Properties:PI2CProperties):LongWord;
  
function I2CDeviceCreate:PI2CDevice;
function I2CDeviceCreateEx(Size:LongWord):PI2CDevice;
function I2CDeviceDestroy(I2C:PI2CDevice):LongWord;

function I2CDeviceRegister(I2C:PI2CDevice):LongWord;
function I2CDeviceDeregister(I2C:PI2CDevice):LongWord;

function I2CDeviceFind(I2CId:LongWord):PI2CDevice;
function I2CDeviceEnumerate(Callback:TI2CEnumerate;Data:Pointer):LongWord;
 
function I2CDeviceNotification(I2C:PI2CDevice;Callback:TI2CNotification;Data:Pointer;Notification,Flags:LongWord):LongWord;

{==============================================================================}
{RTL I2C Functions}
 
{==============================================================================}
{I2C Helper Functions}
function I2CGetCount:LongWord; inline;
function I2CDeviceGetDefault:PI2CDevice; inline;
function I2CDeviceSetDefault(I2C:PI2CDevice):LongWord; 

function I2CDeviceCheck(I2C:PI2CDevice):PI2CDevice;

procedure I2CLog(Level:LongWord;I2C:PI2CDevice;const AText:String);
procedure I2CLogInfo(I2C:PI2CDevice;const AText:String); inline;
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
 //To Do
 
 I2CInitialized:=True;
end;
 
{==============================================================================}
{==============================================================================}
{I2C Functions}

//To Do

{==============================================================================}

function I2CDeviceProperties(I2C:PI2CDevice;Properties:PI2CProperties):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 //To Do
 
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
 Result.Device.DeviceType:=I2C_TYPE_NONE;
 Result.Device.DeviceFlags:=I2C_FLAG_NONE;
 Result.Device.DeviceData:=nil;

 {Update I2C}
 Result.I2CId:=DEVICE_ID_ANY;
 Result.I2CState:=I2C_STATE_DISABLED;
 //To Do
 Result.DeviceProperties:=nil;
 Result.Lock:=INVALID_HANDLE_VALUE;
 
 {Create Lock}
 Result.Lock:=MutexCreate;
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
var
 I2CId:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check I2C}
 if I2C = nil then Exit;
 if I2C.I2CId <> DEVICE_ID_ANY then Exit;
 if I2C.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check I2C}
 Result:=ERROR_ALREADY_EXISTS;
 if I2CDeviceCheck(I2C) = I2C then Exit;
 
 {Check State}
 if I2C.Device.DeviceState <> DEVICE_STATE_UNREGISTERED then Exit;
 
 {Insert I2C}
 if CriticalSectionLock(I2CDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
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
    if I2CDeviceDefault = nil then
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
{Deregister a I2C from the I2C table}
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
        {Check Id}
        if I2C.I2CId = I2CId then
         begin
          Result:=I2C;
          Exit;
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

function I2CDeviceEnumerate(Callback:TI2CEnumerate;Data:Pointer):LongWord;
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
{RTL I2C Functions}

{==============================================================================}
{==============================================================================}
{I2C Helper Functions}
function I2CGetCount:LongWord; inline;
{Get the current I2C count}
begin
 {}
 Result:=I2CDeviceTableCount;
end;

{==============================================================================}

function I2CDeviceGetDefault:PI2CDevice; inline;
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