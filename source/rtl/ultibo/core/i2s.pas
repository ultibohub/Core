{
Ultibo I2S/PCM interface unit.

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


I2S Devices
===========

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit I2S;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,SysUtils;

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
const
 {I2S specific constants}
 I2S_NAME_PREFIX = 'I2S';  {Name prefix for I2S Devices}

 {I2S Device Types}
 I2S_TYPE_NONE      = 0;

 {I2S Device States}
 I2S_STATE_DISABLED = 0;
 I2S_STATE_ENABLED  = 1;

 {I2S Device Flags}
 I2S_FLAG_NONE          = $00000000;

 {I2S logging}
 I2S_LOG_LEVEL_DEBUG     = LOG_LEVEL_DEBUG;  {I2S debugging messages}
 I2S_LOG_LEVEL_INFO      = LOG_LEVEL_INFO;   {I2S informational messages, such as a device being attached or detached}
 I2S_LOG_LEVEL_WARN      = LOG_LEVEL_WARN;   {I2S warning messages}
 I2S_LOG_LEVEL_ERROR     = LOG_LEVEL_ERROR;  {I2S error messages}
 I2S_LOG_LEVEL_NONE      = LOG_LEVEL_NONE;   {No I2S messages}

var
 I2S_DEFAULT_LOG_LEVEL:LongWord = I2S_LOG_LEVEL_DEBUG; {Minimum level for I2S messages.  Only messages with level greater than or equal to this will be printed}

var
 {I2S logging}
 I2S_LOG_ENABLED:Boolean;

{==============================================================================}
type
 {I2S specific types}

 {I2S Properties}
 PI2SProperties = ^TI2SProperties;
 TI2SProperties = record
  //To Do
 end;

 {I2S Device}
 PI2SDevice = ^TI2SDevice;

 {I2S Enumeration Callback}
 TI2SEnumerate = function(I2S:PI2SDevice;Data:Pointer):LongWord;
 {I2S Notification Callback}
 TI2SNotification = function(Device:PDevice;Data:Pointer;Notification:LongWord):LongWord;

 {I2S Device Methods}
 //To Do
 TI2SDeviceGetProperties = function(I2S:PI2SDevice;Properties:PI2SProperties):LongWord;

 TI2SDevice = record
  {Device Properties}
  Device:TDevice;                                 {The Device entry for this I2S}
  {I2S Properties}
  I2SId:LongWord;                                 {Unique Id of this I2S in the I2S table}
  I2SState:LongWord;                              {I2S state (eg I2S_STATE_ENABLED)}
  //To Do
  DeviceGetProperties:TI2SDeviceGetProperties;    {A Device specific DeviceGetProperties method implementing the standard I2S device interface}
  {Statistics Properties}
  //To Do
  {Driver Properties}
  Lock:TMutexHandle;                              {Device lock}
  //To Do
  Properties:TI2SProperties;                      {Device properties}
  {Internal Properties}
  Prev:PI2SDevice;                                {Previous entry in I2S table}
  Next:PI2SDevice;                                {Next entry in I2S table}
 end;

{==============================================================================}
{var}
 {I2S specific variables}

{==============================================================================}
{Initialization Functions}
procedure I2SInit;

{==============================================================================}
{I2S Functions}
//To Do

function I2SDeviceGetProperties(I2S:PI2SDevice;Properties:PI2SProperties):LongWord;

function I2SDeviceCreate:PI2SDevice;
function I2SDeviceCreateEx(Size:LongWord):PI2SDevice;
function I2SDeviceDestroy(I2S:PI2SDevice):LongWord;

function I2SDeviceRegister(I2S:PI2SDevice):LongWord;
function I2SDeviceDeregister(I2S:PI2SDevice):LongWord;

function I2SDeviceFind(I2SId:LongWord):PI2SDevice;
function I2SDeviceFindByName(const Name:String):PI2SDevice; inline;
function I2SDeviceFindByDescription(const Description:String):PI2SDevice; inline;
function I2SDeviceEnumerate(Callback:TI2SEnumerate;Data:Pointer):LongWord;

function I2SDeviceNotification(I2S:PI2SDevice;Callback:TI2SNotification;Data:Pointer;Notification,Flags:LongWord):LongWord;

{==============================================================================}
{RTL I2S Functions}

{==============================================================================}
{I2S Helper Functions}
function I2SGetCount:LongWord;
function I2SDeviceGetDefault:PI2SDevice;
function I2SDeviceSetDefault(I2S:PI2SDevice):LongWord;

function I2SDeviceCheck(I2S:PI2SDevice):PI2SDevice;

procedure I2SLog(Level:LongWord;I2S:PI2SDevice;const AText:String);
procedure I2SLogInfo(I2S:PI2SDevice;const AText:String); inline;
procedure I2SLogWarn(I2S:PI2SDevice;const AText:String); inline;
procedure I2SLogError(I2S:PI2SDevice;const AText:String); inline;
procedure I2SLogDebug(I2S:PI2SDevice;const AText:String); inline;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {I2S specific variables}
 I2SInitialized:Boolean;

 I2SDeviceTable:PI2SDevice;
 I2SDeviceTableLock:TCriticalSectionHandle = INVALID_HANDLE_VALUE;
 I2SDeviceTableCount:LongWord;

 I2SDeviceDefault:PI2SDevice;

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure I2SInit;
begin
 {}
 {Check Initialized}
 if I2SInitialized then Exit;

 {Initialize Logging}
 I2S_LOG_ENABLED:=(I2S_DEFAULT_LOG_LEVEL <> I2S_LOG_LEVEL_NONE);

 {Initialize I2S Table}
 I2SDeviceTable:=nil;
 I2SDeviceTableLock:=CriticalSectionCreate;
 I2SDeviceTableCount:=0;
 if I2SDeviceTableLock = INVALID_HANDLE_VALUE then
  begin
   if I2S_LOG_ENABLED then I2SLogError(nil,'Failed to create I2S table lock');
  end;
 I2SDeviceDefault:=nil;

 {Register Platform I2S Handlers}
 //To Do

 I2SInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{I2S Functions}

//To Do

{==============================================================================}

function I2SDeviceGetProperties(I2S:PI2SDevice;Properties:PI2SProperties):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Properties}
 if Properties = nil then Exit;

 //To Do

end;

{==============================================================================}

function I2SDeviceCreate:PI2SDevice;
{Create a new I2S entry}
{Return: Pointer to new I2S entry or nil if I2S could not be created}
begin
 {}
 Result:=I2SDeviceCreateEx(SizeOf(TI2SDevice));
end;

{==============================================================================}

function I2SDeviceCreateEx(Size:LongWord):PI2SDevice;
{Create a new I2S entry}
{Size: Size in bytes to allocate for new I2S (Including the I2S entry)}
{Return: Pointer to new I2S entry or nil if I2S could not be created}
begin
 {}
 Result:=nil;

 {Check Size}
 if Size < SizeOf(TI2SDevice) then Exit;

 {Create I2S}
 Result:=PI2SDevice(DeviceCreateEx(Size));
 if Result = nil then Exit;

 {Update Device}
 Result.Device.DeviceBus:=DEVICE_BUS_NONE;
 Result.Device.DeviceType:=I2S_TYPE_NONE;
 Result.Device.DeviceFlags:=I2S_FLAG_NONE;
 Result.Device.DeviceData:=nil;

 {Update I2S}
 Result.I2SId:=DEVICE_ID_ANY;
 Result.I2SState:=I2S_STATE_DISABLED;
 //To Do
 Result.DeviceGetProperties:=nil;
 Result.Lock:=INVALID_HANDLE_VALUE;

 {Create Lock}
 Result.Lock:=MutexCreate;
 if Result.Lock = INVALID_HANDLE_VALUE then
  begin
   if I2S_LOG_ENABLED then I2SLogError(nil,'Failed to create lock for I2S device');
   I2SDeviceDestroy(Result);
   Result:=nil;
   Exit;
  end;
end;

{==============================================================================}

function I2SDeviceDestroy(I2S:PI2SDevice):LongWord;
{Destroy an existing I2S entry}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check I2S}
 if I2S = nil then Exit;
 if I2S.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check I2S}
 Result:=ERROR_IN_USE;
 if I2SDeviceCheck(I2S) = I2S then Exit;

 {Check State}
 if I2S.Device.DeviceState <> DEVICE_STATE_UNREGISTERED then Exit;

 {Destroy Lock}
 if I2S.Lock <> INVALID_HANDLE_VALUE then
  begin
   MutexDestroy(I2S.Lock);
  end;

 {Destroy I2S}
 Result:=DeviceDestroy(@I2S.Device);
end;

{==============================================================================}

function I2SDeviceRegister(I2S:PI2SDevice):LongWord;
{Register a new I2S in the I2S table}
var
 I2SId:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check I2S}
 if I2S = nil then Exit;
 if I2S.I2SId <> DEVICE_ID_ANY then Exit;
 if I2S.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check I2S}
 Result:=ERROR_ALREADY_EXISTS;
 if I2SDeviceCheck(I2S) = I2S then Exit;

 {Check State}
 if I2S.Device.DeviceState <> DEVICE_STATE_UNREGISTERED then Exit;

 {Insert I2S}
 if CriticalSectionLock(I2SDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Update I2S}
    I2SId:=0;
    while I2SDeviceFind(I2SId) <> nil do
     begin
      Inc(I2SId);
     end;
    I2S.I2SId:=I2SId;

    {Update Device}
    I2S.Device.DeviceName:=I2S_NAME_PREFIX + IntToStr(I2S.I2SId);
    I2S.Device.DeviceClass:=DEVICE_CLASS_I2S;

    {Register Device}
    Result:=DeviceRegister(@I2S.Device);
    if Result <> ERROR_SUCCESS then
     begin
      I2S.I2SId:=DEVICE_ID_ANY;
      Exit;
     end;

    {Link I2S}
    if I2SDeviceTable = nil then
     begin
      I2SDeviceTable:=I2S;
     end
    else
     begin
      I2S.Next:=I2SDeviceTable;
      I2SDeviceTable.Prev:=I2S;
      I2SDeviceTable:=I2S;
     end;

    {Increment Count}
    Inc(I2SDeviceTableCount);

    {Check Default}
    if I2SDeviceDefault = nil then
     begin
      I2SDeviceDefault:=I2S;
     end;

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    CriticalSectionUnlock(I2SDeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function I2SDeviceDeregister(I2S:PI2SDevice):LongWord;
{Deregister a I2S from the I2S table}
var
 Prev:PI2SDevice;
 Next:PI2SDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check I2S}
 if I2S = nil then Exit;
 if I2S.I2SId = DEVICE_ID_ANY then Exit;
 if I2S.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check I2S}
 Result:=ERROR_NOT_FOUND;
 if I2SDeviceCheck(I2S) <> I2S then Exit;

 {Check State}
 if I2S.Device.DeviceState <> DEVICE_STATE_REGISTERED then Exit;

 {Remove I2S}
 if CriticalSectionLock(I2SDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Deregister Device}
    Result:=DeviceDeregister(@I2S.Device);
    if Result <> ERROR_SUCCESS then Exit;

    {Unlink I2S}
    Prev:=I2S.Prev;
    Next:=I2S.Next;
    if Prev = nil then
     begin
      I2SDeviceTable:=Next;
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
    Dec(I2SDeviceTableCount);

    {Check Default}
    if I2SDeviceDefault = I2S then
     begin
      I2SDeviceDefault:=I2SDeviceTable;
     end;

    {Update I2S}
    I2S.I2SId:=DEVICE_ID_ANY;

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    CriticalSectionUnlock(I2SDeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function I2SDeviceFind(I2SId:LongWord):PI2SDevice;
var
 I2S:PI2SDevice;
begin
 {}
 Result:=nil;

 {Check Id}
 if I2SId = DEVICE_ID_ANY then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(I2SDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get I2S}
    I2S:=I2SDeviceTable;
    while I2S <> nil do
     begin
      {Check State}
      if I2S.Device.DeviceState = DEVICE_STATE_REGISTERED then
       begin
        {Check Id}
        if I2S.I2SId = I2SId then
         begin
          Result:=I2S;
          Exit;
         end;
       end;

      {Get Next}
      I2S:=I2S.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(I2SDeviceTableLock);
   end;
  end;
end;

{==============================================================================}

function I2SDeviceFindByName(const Name:String):PI2SDevice; inline;
begin
 {}
 Result:=PI2SDevice(DeviceFindByName(Name));
end;

{==============================================================================}

function I2SDeviceFindByDescription(const Description:String):PI2SDevice; inline;
begin
 {}
 Result:=PI2SDevice(DeviceFindByDescription(Description));
end;

{==============================================================================}

function I2SDeviceEnumerate(Callback:TI2SEnumerate;Data:Pointer):LongWord;
var
 I2S:PI2SDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Callback}
 if not Assigned(Callback) then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(I2SDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get I2S}
    I2S:=I2SDeviceTable;
    while I2S <> nil do
     begin
      {Check State}
      if I2S.Device.DeviceState = DEVICE_STATE_REGISTERED then
       begin
        if Callback(I2S,Data) <> ERROR_SUCCESS then Exit;
       end;

      {Get Next}
      I2S:=I2S.Next;
     end;

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    CriticalSectionUnlock(I2SDeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function I2SDeviceNotification(I2S:PI2SDevice;Callback:TI2SNotification;Data:Pointer;Notification,Flags:LongWord):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check I2S}
 if I2S = nil then
  begin
   Result:=DeviceNotification(nil,DEVICE_CLASS_I2S,Callback,Data,Notification,Flags);
  end
 else
  begin
   {Check I2S}
   if I2S.Device.Signature <> DEVICE_SIGNATURE then Exit;

   Result:=DeviceNotification(@I2S.Device,DEVICE_CLASS_I2S,Callback,Data,Notification,Flags);
  end;
end;

{==============================================================================}
{==============================================================================}
{RTL I2S Functions}

{==============================================================================}
{==============================================================================}
{I2S Helper Functions}
function I2SGetCount:LongWord;
{Get the current I2S count}
begin
 {}
 Result:=I2SDeviceTableCount;
end;

{==============================================================================}

function I2SDeviceGetDefault:PI2SDevice;
{Get the current default I2S device}
begin
 {}
 Result:=I2SDeviceDefault;
end;

{==============================================================================}

function I2SDeviceSetDefault(I2S:PI2SDevice):LongWord;
{Set the current default I2S device}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check I2S}
 if I2S = nil then Exit;
 if I2S.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(I2SDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Check I2S}
    if I2SDeviceCheck(I2S) <> I2S then Exit;

    {Set I2S Default}
    I2SDeviceDefault:=I2S;

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    CriticalSectionUnlock(I2SDeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function I2SDeviceCheck(I2S:PI2SDevice):PI2SDevice;
{Check if the supplied I2S is in the I2S table}
var
 Current:PI2SDevice;
begin
 {}
 Result:=nil;

 {Check I2S}
 if I2S = nil then Exit;
 if I2S.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(I2SDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get I2S}
    Current:=I2SDeviceTable;
    while Current <> nil do
     begin
      {Check I2S}
      if Current = I2S then
       begin
        Result:=I2S;
        Exit;
       end;

      {Get Next}
      Current:=Current.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(I2SDeviceTableLock);
   end;
  end;
end;

{==============================================================================}

procedure I2SLog(Level:LongWord;I2S:PI2SDevice;const AText:String);
var
 WorkBuffer:String;
begin
 {}
 {Check Level}
 if Level < I2S_DEFAULT_LOG_LEVEL then Exit;

 WorkBuffer:='';
 {Check Level}
 if Level = I2S_LOG_LEVEL_DEBUG then
  begin
   WorkBuffer:=WorkBuffer + '[DEBUG] ';
  end
 else if Level = I2S_LOG_LEVEL_WARN then
  begin
   WorkBuffer:=WorkBuffer + '[WARN] ';
  end
 else if Level = I2S_LOG_LEVEL_ERROR then
  begin
   WorkBuffer:=WorkBuffer + '[ERROR] ';
  end;

 {Add Prefix}
 WorkBuffer:=WorkBuffer + 'I2S: ';

 {Check I2S}
 if I2S <> nil then
  begin
   WorkBuffer:=WorkBuffer + I2S_NAME_PREFIX + IntToStr(I2S.I2SId) + ': ';
  end;

 {Output Logging}
 LoggingOutputEx(LOGGING_FACILITY_I2S,LogLevelToLoggingSeverity(Level),'I2S',WorkBuffer + AText);
end;

{==============================================================================}

procedure I2SLogInfo(I2S:PI2SDevice;const AText:String); inline;
begin
 {}
 I2SLog(I2S_LOG_LEVEL_INFO,I2S,AText);
end;

{==============================================================================}

procedure I2SLogWarn(I2S:PI2SDevice;const AText:String); inline;
begin
 {}
 I2SLog(I2S_LOG_LEVEL_WARN,I2S,AText);
end;

{==============================================================================}

procedure I2SLogError(I2S:PI2SDevice;const AText:String); inline;
begin
 {}
 I2SLog(I2S_LOG_LEVEL_ERROR,I2S,AText);
end;

{==============================================================================}

procedure I2SLogDebug(I2S:PI2SDevice;const AText:String); inline;
begin
 {}
 I2SLog(I2S_LOG_LEVEL_DEBUG,I2S,AText);
end;

{==============================================================================}
{==============================================================================}

initialization
 I2SInit;

{==============================================================================}

finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
