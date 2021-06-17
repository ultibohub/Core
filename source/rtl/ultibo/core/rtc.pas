{
Ultibo Real Time Clock device interface unit.

Copyright (C) 2021 - SoftOz Pty Ltd.

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


RTC Devices
===========

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit RTC; 

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,SysUtils;

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
const
 {RTC specific constants}
 RTC_NAME_PREFIX = 'RTC';  {Name prefix for RTC Devices}
 
 {RTC Device Types}
 RTC_TYPE_NONE      = 0;
 
 RTC_TYPE_MAX       = 0;
 
 {RTC Type Names}
 RTC_TYPE_NAMES:array[RTC_TYPE_NONE..RTC_TYPE_MAX] of String = (
  'RTC_TYPE_NONE');
 
 {RTC Device States}
 RTC_STATE_DISABLED = 0;
 RTC_STATE_ENABLED  = 1;
 
 RTC_STATE_MAX      = 1;
 
 {RTC State Names}
 RTC_STATE_NAMES:array[RTC_STATE_DISABLED..RTC_STATE_MAX] of String = (
  'RTC_STATE_DISABLED',
  'RTC_STATE_ENABLED');
 
 {RTC Device Flags}
 RTC_FLAG_NONE          = $00000000;
 RTC_FLAG_ALARM         = $00000001; {Device supports one or more alarms}
 RTC_FLAG_WATCHDOG      = $00000002; {Device has a watchdog timer function}
 
 {RTC logging}
 RTC_LOG_LEVEL_DEBUG     = LOG_LEVEL_DEBUG;  {RTC debugging messages}
 RTC_LOG_LEVEL_INFO      = LOG_LEVEL_INFO;   {RTC informational messages, such as a device being attached or detached}
 RTC_LOG_LEVEL_WARN      = LOG_LEVEL_WARN;   {RTC warning messages}
 RTC_LOG_LEVEL_ERROR     = LOG_LEVEL_ERROR;  {RTC error messages}
 RTC_LOG_LEVEL_NONE      = LOG_LEVEL_NONE;   {No RTC messages}

var 
 RTC_DEFAULT_LOG_LEVEL:LongWord = RTC_LOG_LEVEL_DEBUG; {Minimum level for RTC messages.  Only messages with level greater than or equal to this will be printed}
 
var 
 {RTC logging}
 RTC_LOG_ENABLED:Boolean; 

{==============================================================================}
type
 {RTC specific types}

 {RTC Properties}
 PRTCProperties = ^TRTCProperties;
 TRTCProperties = record
  Flags:LongWord;      {Device flags (eg RTC_FLAG_ALARM)}
  MinTime:Int64;       {Minimum time value represented by the device (Normally the power on reset value)}
  MaxTime:Int64;       {Maximum time value represented by the device (Time when a rollover will occur)}
  AlarmCount:LongWord; {Number of alarms supported by the device (0 if not supported)}
 end;
 
 {RTC Device}
 PRTCDevice = ^TRTCDevice;
 
 {RTC Enumeration Callback}
 TRTCEnumerate = function(RTC:PRTCDevice;Data:Pointer):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 {RTC Notification Callback}
 TRTCNotification = function(Device:PDevice;Data:Pointer;Notification:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 
 {RTC Device Methods}
 TRTCDeviceStart = function(RTC:PRTCDevice):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TRTCDeviceStop = function(RTC:PRTCDevice):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TRTCDeviceGetTime = function(RTC:PRTCDevice):Int64;{$IFDEF i386} stdcall;{$ENDIF}
 TRTCDeviceSetTime = function(RTC:PRTCDevice;const Time:Int64):Int64;{$IFDEF i386} stdcall;{$ENDIF}
 TRTCDeviceGetProperties = function(RTC:PRTCDevice;Properties:PRTCProperties):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 
 TRTCDevice = record
  {Device Properties}
  Device:TDevice;                                 {The Device entry for this RTC}
  {RTC Properties}
  RTCId:LongWord;                                 {Unique Id of this RTC in the RTC table}
  RTCState:LongWord;                              {RTC state (eg RTC_STATE_ENABLED)}
  DeviceStart:TRTCDeviceStart;                    {A Device specific DeviceStart method implementing the standard RTC device interface}
  DeviceStop:TRTCDeviceStop;                      {A Device specific DeviceStop method implementing the standard RTC device interface}
  DeviceGetTime:TRTCDeviceGetTime;                {A Device specific DeviceGetTime method implementing the standard RTC device interface}
  DeviceSetTime:TRTCDeviceSetTime;                {A Device specific DeviceSetTime method implementing the standard RTC device interface}
  DeviceGetProperties:TRTCDeviceGetProperties;    {A Device specific DeviceGetProperties method implementing the standard RTC device interface}
  {Statistics Properties}
  GetCount:LongWord;
  SetCount:LongWord;
  {Driver Properties}
  Lock:TMutexHandle;                              {Device lock}
  Properties:TRTCProperties;                      {Device properties}
  {Internal Properties}                                                                        
  Prev:PRTCDevice;                                {Previous entry in RTC table}
  Next:PRTCDevice;                                {Next entry in RTC table}
 end; 
  
{==============================================================================}
{var}
 {RTC specific variables}
 
{==============================================================================}
{Initialization Functions}
procedure RTCInit;
 
{==============================================================================}
{RTC Functions}
function RTCDeviceStart(RTC:PRTCDevice):LongWord;
function RTCDeviceStop(RTC:PRTCDevice):LongWord;

function RTCDeviceGetTime(RTC:PRTCDevice):Int64;
function RTCDeviceSetTime(RTC:PRTCDevice;const Time:Int64):Int64;
 
function RTCDeviceProperties(RTC:PRTCDevice;Properties:PRTCProperties):LongWord; inline;
function RTCDeviceGetProperties(RTC:PRTCDevice;Properties:PRTCProperties):LongWord;
  
function RTCDeviceCreate:PRTCDevice;
function RTCDeviceCreateEx(Size:LongWord):PRTCDevice;
function RTCDeviceDestroy(RTC:PRTCDevice):LongWord;

function RTCDeviceRegister(RTC:PRTCDevice):LongWord;
function RTCDeviceDeregister(RTC:PRTCDevice):LongWord;

function RTCDeviceFind(RTCId:LongWord):PRTCDevice;
function RTCDeviceFindByName(const Name:String):PRTCDevice; inline;
function RTCDeviceFindByDescription(const Description:String):PRTCDevice; inline;
function RTCDeviceEnumerate(Callback:TRTCEnumerate;Data:Pointer):LongWord;
 
function RTCDeviceNotification(RTC:PRTCDevice;Callback:TRTCNotification;Data:Pointer;Notification,Flags:LongWord):LongWord;

{==============================================================================}
{RTL RTC Functions}
function SysRTCAvailable:Boolean;

function SysRTCGetTime:Int64;
function SysRTCSetTime(const Time:Int64):Int64;

{==============================================================================}
{RTC Helper Functions}
function RTCGetCount:LongWord; inline;
function RTCDeviceGetDefault:PRTCDevice; inline;
function RTCDeviceSetDefault(RTC:PRTCDevice):LongWord; 

function RTCDeviceCheck(RTC:PRTCDevice):PRTCDevice;

function RTCDeviceTypeToString(RTCType:LongWord):String;
function RTCDeviceStateToString(RTCState:LongWord):String;

function RTCTimeIsValid(const Time:TSystemTime):Boolean;

function RTCSystemTimeToFileTime(const SystemTime:TSystemTime;var FileTime:Int64):Boolean;
function RTCFileTimeToSystemTime(const FileTime:Int64;var SystemTime:TSystemTime):Boolean;

procedure RTCLog(Level:LongWord;RTC:PRTCDevice;const AText:String);
procedure RTCLogInfo(RTC:PRTCDevice;const AText:String); inline;
procedure RTCLogWarn(RTC:PRTCDevice;const AText:String); inline;
procedure RTCLogError(RTC:PRTCDevice;const AText:String); inline;
procedure RTCLogDebug(RTC:PRTCDevice;const AText:String); inline;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {RTC specific variables}
 RTCInitialized:Boolean;

 RTCDeviceTable:PRTCDevice;
 RTCDeviceTableLock:TCriticalSectionHandle = INVALID_HANDLE_VALUE;
 RTCDeviceTableCount:LongWord;

 RTCDeviceDefault:PRTCDevice;
 
{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure RTCInit;
begin
 {}
 {Check Initialized}
 if RTCInitialized then Exit;
 
 {Initialize Logging}
 RTC_LOG_ENABLED:=(RTC_DEFAULT_LOG_LEVEL <> RTC_LOG_LEVEL_NONE); 
 
 {Initialize RTC Table}
 RTCDeviceTable:=nil;
 RTCDeviceTableLock:=CriticalSectionCreate; 
 RTCDeviceTableCount:=0;
 if RTCDeviceTableLock = INVALID_HANDLE_VALUE then
  begin
   if RTC_LOG_ENABLED then RTCLogError(nil,'Failed to create RTC table lock');
  end;
 RTCDeviceDefault:=nil;
 
 {Register Platform RTC Handlers}
 RTCAvailableHandler:=SysRTCAvailable;
 RTCGetTimeHandler:=SysRTCGetTime;
 RTCSetTimeHandler:=SysRTCSetTime;
 
 RTCInitialized:=True;
end;
 
{==============================================================================}
{==============================================================================}
{RTC Functions}
function RTCDeviceStart(RTC:PRTCDevice):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check RTC}
 if RTC = nil then Exit;
 if RTC.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IFDEF RTC_DEBUG}
 if RTC_LOG_ENABLED then RTCLogDebug(RTC,'RTC Device Start');
 {$ENDIF}
 
 {Check Disabled}
 Result:=ERROR_SUCCESS;
 if RTC.RTCState <> RTC_STATE_DISABLED then Exit;
 
 if MutexLock(RTC.Lock) = ERROR_SUCCESS then
  begin
   try
    if Assigned(RTC.DeviceStart) then
     begin
      {Call Device Start}
      Result:=RTC.DeviceStart(RTC);
      if Result <> ERROR_SUCCESS then Exit;
     end
    else
     begin
      Result:=ERROR_INVALID_PARAMETER;
      Exit;
     end;
     
    {Enable Device}
    RTC.RTCState:=RTC_STATE_ENABLED;
    
    {Notify Enable}
    NotifierNotify(@RTC.Device,DEVICE_NOTIFICATION_ENABLE);
    
    Result:=ERROR_SUCCESS;
   finally
    MutexUnlock(RTC.Lock);
   end; 
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;    
end;

{==============================================================================}

function RTCDeviceStop(RTC:PRTCDevice):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check RTC}
 if RTC = nil then Exit;
 if RTC.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IFDEF RTC_DEBUG}
 if RTC_LOG_ENABLED then RTCLogDebug(RTC,'RTC Device Stop');
 {$ENDIF}

 {Check Enabled}
 Result:=ERROR_SUCCESS;
 if RTC.RTCState <> RTC_STATE_ENABLED then Exit;
 
 if MutexLock(RTC.Lock) = ERROR_SUCCESS then
  begin
   try
    if Assigned(RTC.DeviceStop) then
     begin
      {Call Device Stop}
      Result:=RTC.DeviceStop(RTC);
      if Result <> ERROR_SUCCESS then Exit;
     end
    else
     begin
      Result:=ERROR_INVALID_PARAMETER;
      Exit;
     end;    
  
    {Disable Device}
    RTC.RTCState:=RTC_STATE_DISABLED;
    
    {Notify Disable}
    NotifierNotify(@RTC.Device,DEVICE_NOTIFICATION_DISABLE);
    
    Result:=ERROR_SUCCESS;
   finally
    MutexUnlock(RTC.Lock);
   end; 
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;    
end;

{==============================================================================}

function RTCDeviceGetTime(RTC:PRTCDevice):Int64;
begin
 {}
 Result:=0;
 
 {Check RTC}
 if RTC = nil then Exit;
 if RTC.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IFDEF RTC_DEBUG}
 if RTC_LOG_ENABLED then RTCLogDebug(RTC,'RTC Device Get Time');
 {$ENDIF}
 
 {Check Enabled}
 if RTC.RTCState <> RTC_STATE_ENABLED then Exit;
 
 if Assigned(RTC.DeviceGetTime) then
  begin
   Result:=RTC.DeviceGetTime(RTC);
  end;
end;

{==============================================================================}

function RTCDeviceSetTime(RTC:PRTCDevice;const Time:Int64):Int64;
begin
 {}
 Result:=0;
 
 {Check RTC}
 if RTC = nil then Exit;
 if RTC.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IFDEF RTC_DEBUG}
 if RTC_LOG_ENABLED then RTCLogDebug(RTC,'RTC Device Set Time (Time=' + IntToStr(Time) + ')');
 {$ENDIF}
 
 {Check Enabled}
 if RTC.RTCState <> RTC_STATE_ENABLED then Exit;
 
 if Assigned(RTC.DeviceSetTime) then
  begin
   Result:=RTC.DeviceSetTime(RTC,Time);
  end;
end;
 
{==============================================================================}
 
function RTCDeviceProperties(RTC:PRTCDevice;Properties:PRTCProperties):LongWord; inline;
{Get the properties for the specified RTC device}
{RTC: The RTC device to get properties from}
{Properties: Pointer to a PRTCProperties structure to fill in}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: Replaced by RTCDeviceGetProperties for consistency}
begin
 {}
 Result:=RTCDeviceGetProperties(RTC,Properties);
end;

{==============================================================================}

function RTCDeviceGetProperties(RTC:PRTCDevice;Properties:PRTCProperties):LongWord;
{Get the properties for the specified RTC device}
{RTC: The RTC device to get properties from}
{Properties: Pointer to a PRTCProperties structure to fill in}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Properties}
 if Properties = nil then Exit;
 
 {Check RTC}
 if RTC = nil then Exit;
 if RTC.Device.Signature <> DEVICE_SIGNATURE then Exit; 

 {$IFDEF RTC_DEBUG}
 if RTC_LOG_ENABLED then RTCLogDebug(RTC,'RTC Device Get Properties');
 {$ENDIF}
 
 {Check Enabled}
 {Result:=ERROR_NOT_SUPPORTED;}
 {if RTC.RTCState <> RTC_STATE_ENABLED then Exit;} {Allow when disabled}
 
 if Assigned(RTC.DeviceGetProperties) then
  begin
   Result:=RTC.DeviceGetProperties(RTC,Properties);
  end
 else
  begin
   if MutexLock(RTC.Lock) <> ERROR_SUCCESS then Exit;
   
   {Get Properties}
   System.Move(RTC.Properties,Properties^,SizeOf(TRTCProperties));

   {Return Result}
   Result:=ERROR_SUCCESS;
   
   MutexUnlock(RTC.Lock);
  end;  
end;

{==============================================================================}

function RTCDeviceCreate:PRTCDevice;
{Create a new RTC entry}
{Return: Pointer to new RTC entry or nil if RTC could not be created}
begin
 {}
 Result:=RTCDeviceCreateEx(SizeOf(TRTCDevice));
end;

{==============================================================================}

function RTCDeviceCreateEx(Size:LongWord):PRTCDevice;
{Create a new RTC entry}
{Size: Size in bytes to allocate for new RTC (Including the RTC entry)}
{Return: Pointer to new RTC entry or nil if RTC could not be created}
begin
 {}
 Result:=nil;
 
 {Check Size}
 if Size < SizeOf(TRTCDevice) then Exit;
 
 {Create RTC}
 Result:=PRTCDevice(DeviceCreateEx(Size));
 if Result = nil then Exit;
 
 {Update Device}
 Result.Device.DeviceBus:=DEVICE_BUS_NONE;   
 Result.Device.DeviceType:=RTC_TYPE_NONE;
 Result.Device.DeviceFlags:=RTC_FLAG_NONE;
 Result.Device.DeviceData:=nil;

 {Update RTC}
 Result.RTCId:=DEVICE_ID_ANY;
 Result.RTCState:=RTC_STATE_DISABLED;
 Result.DeviceStart:=nil;
 Result.DeviceStop:=nil;
 Result.DeviceGetTime:=nil;
 Result.DeviceSetTime:=nil;
 Result.DeviceGetProperties:=nil;
 Result.Lock:=INVALID_HANDLE_VALUE;
 
 {Create Lock}
 Result.Lock:=MutexCreateEx(False,MUTEX_DEFAULT_SPINCOUNT,MUTEX_FLAG_RECURSIVE);
 if Result.Lock = INVALID_HANDLE_VALUE then
  begin
   if RTC_LOG_ENABLED then RTCLogError(nil,'Failed to create lock for RTC device');
   RTCDeviceDestroy(Result);
   Result:=nil;
   Exit;
  end;
end;

{==============================================================================}

function RTCDeviceDestroy(RTC:PRTCDevice):LongWord;
{Destroy an existing RTC entry}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check RTC}
 if RTC = nil then Exit;
 if RTC.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check RTC}
 Result:=ERROR_IN_USE;
 if RTCDeviceCheck(RTC) = RTC then Exit;

 {Check State}
 if RTC.Device.DeviceState <> DEVICE_STATE_UNREGISTERED then Exit;
 
 {Destroy Lock}
 if RTC.Lock <> INVALID_HANDLE_VALUE then
  begin
   MutexDestroy(RTC.Lock);
  end;
 
 {Destroy RTC} 
 Result:=DeviceDestroy(@RTC.Device);
end;

{==============================================================================}

function RTCDeviceRegister(RTC:PRTCDevice):LongWord;
{Register a new RTC in the RTC table}
var
 RTCId:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check RTC}
 if RTC = nil then Exit;
 if RTC.RTCId <> DEVICE_ID_ANY then Exit;
 if RTC.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check Interfaces}
 if not(Assigned(RTC.DeviceStart)) then Exit;
 if not(Assigned(RTC.DeviceStop)) then Exit;
 if not(Assigned(RTC.DeviceGetTime)) then Exit;
 if not(Assigned(RTC.DeviceSetTime)) then Exit;
 
 {Check RTC}
 Result:=ERROR_ALREADY_EXISTS;
 if RTCDeviceCheck(RTC) = RTC then Exit;
 
 {Check State}
 if RTC.Device.DeviceState <> DEVICE_STATE_UNREGISTERED then Exit;
 
 {Insert RTC}
 if CriticalSectionLock(RTCDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Update RTC}
    RTCId:=0;
    while RTCDeviceFind(RTCId) <> nil do
     begin
      Inc(RTCId);
     end;
    RTC.RTCId:=RTCId;
    
    {Update Device}
    RTC.Device.DeviceName:=RTC_NAME_PREFIX + IntToStr(RTC.RTCId); 
    RTC.Device.DeviceClass:=DEVICE_CLASS_RTC;
    
    {Register Device}
    Result:=DeviceRegister(@RTC.Device);
    if Result <> ERROR_SUCCESS then
     begin
      RTC.RTCId:=DEVICE_ID_ANY;
      Exit;
     end; 
    
    {Link RTC}
    if RTCDeviceTable = nil then
     begin
      RTCDeviceTable:=RTC;
     end
    else
     begin
      RTC.Next:=RTCDeviceTable;
      RTCDeviceTable.Prev:=RTC;
      RTCDeviceTable:=RTC;
     end;
 
    {Increment Count}
    Inc(RTCDeviceTableCount);
    
    {Check Default}
    if RTCDeviceDefault = nil then
     begin
      RTCDeviceDefault:=RTC;
     end;
    
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    CriticalSectionUnlock(RTCDeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;

{==============================================================================}

function RTCDeviceDeregister(RTC:PRTCDevice):LongWord;
{Deregister a RTC from the RTC table}
var
 Prev:PRTCDevice;
 Next:PRTCDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check RTC}
 if RTC = nil then Exit;
 if RTC.RTCId = DEVICE_ID_ANY then Exit;
 if RTC.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check RTC}
 Result:=ERROR_NOT_FOUND;
 if RTCDeviceCheck(RTC) <> RTC then Exit;
 
 {Check State}
 if RTC.Device.DeviceState <> DEVICE_STATE_REGISTERED then Exit;
 
 {Remove RTC}
 if CriticalSectionLock(RTCDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Deregister Device}
    Result:=DeviceDeregister(@RTC.Device);
    if Result <> ERROR_SUCCESS then Exit;
    
    {Unlink RTC}
    Prev:=RTC.Prev;
    Next:=RTC.Next;
    if Prev = nil then
     begin
      RTCDeviceTable:=Next;
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
    Dec(RTCDeviceTableCount);
 
    {Check Default}
    if RTCDeviceDefault = RTC then
     begin
      RTCDeviceDefault:=RTCDeviceTable;
     end;
 
    {Update RTC}
    RTC.RTCId:=DEVICE_ID_ANY;
 
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    CriticalSectionUnlock(RTCDeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;

{==============================================================================}

function RTCDeviceFind(RTCId:LongWord):PRTCDevice;
var
 RTC:PRTCDevice;
begin
 {}
 Result:=nil;
 
 {Check Id}
 if RTCId = DEVICE_ID_ANY then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(RTCDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get RTC}
    RTC:=RTCDeviceTable;
    while RTC <> nil do
     begin
      {Check State}
      if RTC.Device.DeviceState = DEVICE_STATE_REGISTERED then
       begin
        {Check Id}
        if RTC.RTCId = RTCId then
         begin
          Result:=RTC;
          Exit;
         end;
       end;
       
      {Get Next}
      RTC:=RTC.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(RTCDeviceTableLock);
   end;
  end;
end;

{==============================================================================}

function RTCDeviceFindByName(const Name:String):PRTCDevice; inline;
begin
 {}
 Result:=PRTCDevice(DeviceFindByName(Name));
end;

{==============================================================================}

function RTCDeviceFindByDescription(const Description:String):PRTCDevice; inline;
begin
 {}
 Result:=PRTCDevice(DeviceFindByDescription(Description));
end;
       
{==============================================================================}

function RTCDeviceEnumerate(Callback:TRTCEnumerate;Data:Pointer):LongWord;
var
 RTC:PRTCDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Callback}
 if not Assigned(Callback) then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(RTCDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get RTC}
    RTC:=RTCDeviceTable;
    while RTC <> nil do
     begin
      {Check State}
      if RTC.Device.DeviceState = DEVICE_STATE_REGISTERED then
       begin
        if Callback(RTC,Data) <> ERROR_SUCCESS then Exit;
       end;
       
      {Get Next}
      RTC:=RTC.Next;
     end;
     
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    CriticalSectionUnlock(RTCDeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;

{==============================================================================}

function RTCDeviceNotification(RTC:PRTCDevice;Callback:TRTCNotification;Data:Pointer;Notification,Flags:LongWord):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check RTC}
 if RTC = nil then
  begin
   Result:=DeviceNotification(nil,DEVICE_CLASS_RTC,Callback,Data,Notification,Flags);
  end
 else
  begin 
   {Check RTC}
   if RTC.Device.Signature <> DEVICE_SIGNATURE then Exit;

   Result:=DeviceNotification(@RTC.Device,DEVICE_CLASS_RTC,Callback,Data,Notification,Flags);
  end; 
end;

{==============================================================================}
{==============================================================================}
{RTL RTC Functions}
function SysRTCAvailable:Boolean; 
{Check if an RTC device is available}
begin
 {}
 Result:=(RTCDeviceDefault <> nil);
end;

{==============================================================================}

function SysRTCGetTime:Int64;
{Get the current time from a RTC device}
{Returned time is 100 nanosecond ticks since 1 January 1601}
begin
 {}
 Result:=0;
 
 if RTCDeviceDefault = nil then Exit;

 Result:=RTCDeviceGetTime(RTCDeviceDefault);
end;

{==============================================================================}

function SysRTCSetTime(const Time:Int64):Int64;
{Set the current time for a RTC device}
{Time: The time to be set}
{Return: The device time after setting (or 0 on failure)}
{Time and returned time is 100 nanosecond ticks since 1 January 1601}
begin
 {}
 Result:=0;
 
 if RTCDeviceDefault = nil then Exit;

 Result:=RTCDeviceSetTime(RTCDeviceDefault,Time);
end;

{==============================================================================}
{==============================================================================}
{RTC Helper Functions}
function RTCGetCount:LongWord; inline;
{Get the current RTC count}
begin
 {}
 Result:=RTCDeviceTableCount;
end;

{==============================================================================}

function RTCDeviceGetDefault:PRTCDevice; inline;
{Get the current default RTC device}
begin
 {}
 Result:=RTCDeviceDefault;
end;

{==============================================================================}

function RTCDeviceSetDefault(RTC:PRTCDevice):LongWord; 
{Set the current default RTC device}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check RTC}
 if RTC = nil then Exit;
 if RTC.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(RTCDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Check RTC}
    if RTCDeviceCheck(RTC) <> RTC then Exit;
    
    {Set RTC Default}
    RTCDeviceDefault:=RTC;
    
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    CriticalSectionUnlock(RTCDeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function RTCDeviceCheck(RTC:PRTCDevice):PRTCDevice;
{Check if the supplied RTC is in the RTC table}
var
 Current:PRTCDevice;
begin
 {}
 Result:=nil;
 
 {Check RTC}
 if RTC = nil then Exit;
 if RTC.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(RTCDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get RTC}
    Current:=RTCDeviceTable;
    while Current <> nil do
     begin
      {Check RTC}
      if Current = RTC then
       begin
        Result:=RTC;
        Exit;
       end;
      
      {Get Next}
      Current:=Current.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(RTCDeviceTableLock);
   end;
  end;
end;

{==============================================================================}

function RTCDeviceTypeToString(RTCType:LongWord):String;
begin
 {}
 Result:='RTC_TYPE_UNKNOWN';
 
 if RTCType <= RTC_TYPE_MAX then
  begin
   Result:=RTC_TYPE_NAMES[RTCType];
  end;
end;

{==============================================================================}

function RTCDeviceStateToString(RTCState:LongWord):String;
begin
 {}
 Result:='RTC_STATE_UNKNOWN';
 
 if RTCState <= RTC_STATE_MAX then
  begin
   Result:=RTC_STATE_NAMES[RTCState];
  end;
end;

{==============================================================================}

function RTCTimeIsValid(const Time:TSystemTime):Boolean;
begin
 {}
 Result:=False;
 
 if Time.Year < 1900 then Exit;
 if (Time.Month < 1) or (Time.Month > 12) then Exit;
 if (Time.Day < 1) or (Time.Day > 31) then Exit;
 if (Time.Hour >= 24) then Exit;
 if (Time.Minute >= 60) then Exit;
 if (Time.Second >= 60) then Exit;
 
 Result:=True;
end;

{==============================================================================}

function RTCSystemTimeToFileTime(const SystemTime:TSystemTime;var FileTime:Int64):Boolean;
{System time is assumed to be UTC and returned file time is UTC}
var
 DateTime:TDateTime;
begin
 {}
 Result:=False;

 {Setup FileTime}
 FileTime:=0;
 
 {Convert to DateTime}
 DateTime:=SystemTimeToDateTime(SystemTime);
 
 {Convert to FileTime}
 FileTime:=((Trunc(DateTime) * TIME_TICKS_PER_DAY) + TIME_TICKS_TO_1899) + ((Round(Frac(DateTime) * PASCAL_TIME_MILLISECONDS_PER_DAY) * TIME_TICKS_PER_MILLISECOND));
 
 Result:=True;
end;

{==============================================================================}

function RTCFileTimeToSystemTime(const FileTime:Int64;var SystemTime:TSystemTime):Boolean;
{File time is assumed to be UTC and returned system time is UTC}
var
 DateTime:TDateTime;
begin
 {}
 Result:=False;

 {Setup SystemTime}
 FillChar(SystemTime,SizeOf(TSystemTime),0);
 
 if FileTime < TIME_TICKS_TO_1899 then Exit;
 
 {Convert to DateTime}
 DateTime:=((FileTime - TIME_TICKS_TO_1899) div TIME_TICKS_PER_DAY) + (((FileTime - TIME_TICKS_TO_1899) mod TIME_TICKS_PER_DAY) / TIME_TICKS_PER_DAY);
 
 {Convert to SystemTime}
 DateTimeToSystemTime(DateTime,SystemTime);
 
 Result:=True;
end;

{==============================================================================}

procedure RTCLog(Level:LongWord;RTC:PRTCDevice;const AText:String);
var
 WorkBuffer:String;
begin
 {}
 {Check Level}
 if Level < RTC_DEFAULT_LOG_LEVEL then Exit;
 
 WorkBuffer:='';
 {Check Level}
 if Level = RTC_LOG_LEVEL_DEBUG then
  begin
   WorkBuffer:=WorkBuffer + '[DEBUG] ';
  end
 else if Level = RTC_LOG_LEVEL_WARN then
  begin
   WorkBuffer:=WorkBuffer + '[WARN] ';
  end
 else if Level = RTC_LOG_LEVEL_ERROR then
  begin
   WorkBuffer:=WorkBuffer + '[ERROR] ';
  end;
 
 {Add Prefix}
 WorkBuffer:=WorkBuffer + 'RTC: ';
 
 {Check RTC}
 if RTC <> nil then
  begin
   WorkBuffer:=WorkBuffer + RTC_NAME_PREFIX + IntToStr(RTC.RTCId) + ': ';
  end;

 {Output Logging}  
 LoggingOutputEx(LOGGING_FACILITY_RTC,LogLevelToLoggingSeverity(Level),'RTC',WorkBuffer + AText);
end;

{==============================================================================}

procedure RTCLogInfo(RTC:PRTCDevice;const AText:String); inline;
begin
 {}
 RTCLog(RTC_LOG_LEVEL_INFO,RTC,AText);
end;

{==============================================================================}

procedure RTCLogWarn(RTC:PRTCDevice;const AText:String); inline;
begin
 {}
 RTCLog(RTC_LOG_LEVEL_WARN,RTC,AText);
end;

{==============================================================================}

procedure RTCLogError(RTC:PRTCDevice;const AText:String); inline;
begin
 {}
 RTCLog(RTC_LOG_LEVEL_ERROR,RTC,AText);
end;

{==============================================================================}

procedure RTCLogDebug(RTC:PRTCDevice;const AText:String); inline;
begin
 {}
 RTCLog(RTC_LOG_LEVEL_DEBUG,RTC,AText);
end;

{==============================================================================}
{==============================================================================}

initialization
 RTCInit;

{==============================================================================}
 
finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
  