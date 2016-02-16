{
Ultibo PWM interface unit.

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


PWM Hosts
=========

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit PWM; 

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,SysUtils;

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
const
 {PWM specific constants}
 PWM_NAME_PREFIX = 'PWM';  {Name prefix for PWM Devices}
 
 {PWM Device Types}
 PWM_TYPE_NONE      = 0;
 
 {PWM Device States}
 PWM_STATE_DISABLED = 0;
 PWM_STATE_ENABLED  = 1;
 
 {PWM Device Flags}
 PWM_FLAG_NONE          = $00000000;
 
 {PWM logging}
 PWM_LOG_LEVEL_DEBUG     = LOG_LEVEL_DEBUG;  {PWM debugging messages}
 PWM_LOG_LEVEL_INFO      = LOG_LEVEL_INFO;   {PWM informational messages, such as a device being attached or detached}
 PWM_LOG_LEVEL_ERROR     = LOG_LEVEL_ERROR;  {PWM error messages}
 PWM_LOG_LEVEL_NONE      = LOG_LEVEL_NONE;   {No PWM messages}

var 
 PWM_DEFAULT_LOG_LEVEL:LongWord = PWM_LOG_LEVEL_INFO; {Minimum level for PWM messages.  Only messages with level greater than or equal to this will be printed}
 
var 
 {PWM logging}
 PWM_LOG_ENABLED:Boolean; 
 
{==============================================================================}
type
 {PWM specific types}

 {PWM Properties}
 PPWMProperties = ^TPWMProperties;
 TPWMProperties = record
  //To Do
 end;
 
 {PWM Device}
 PPWMDevice = ^TPWMDevice;
 
 {PWM Enumeration Callback}
 TPWMEnumerate = function(PWM:PPWMDevice;Data:Pointer):LongWord;
 {PWM Notification Callback}
 TPWMNotification = function(Device:PDevice;Data:Pointer;Notification:LongWord):LongWord;
 
 {PWM Device Methods}
 //To Do
 TPWMDeviceProperties = function(PWM:PPWMDevice;var Properties:PPWMProperties):LongWord;
 
 TPWMDevice = record
  {Device Properties}
  Device:TDevice;                                 {The Device entry for this PWM}
  {PWM Properties}
  PWMId:LongWord;                                 {Unique Id of this PWM in the PWM table}
  PWMState:LongWord;                              {PWM state (eg PWM_STATE_ENABLED)}
  //To Do
  DeviceProperties:TPWMDeviceProperties;          {A Device specific DeviceProperties method implementing the standard PWM device interface}
  {Statistics Properties}
  //To Do
  {Driver Properties}
  Lock:TMutexHandle;                              {Device lock}
  //To Do
  Properties:TPWMProperties;                      {Device properties}
  {Internal Properties}                                                                        
  Prev:PPWMDevice;                                {Previous entry in PWM table}
  Next:PPWMDevice;                                {Next entry in PWM table}
 end; 
 
{==============================================================================}
{var}
 {PWM specific variables}

{==============================================================================}
{Initialization Functions}
procedure PWMInit;
 
{==============================================================================}
{PWM Functions}
 
//To Do
 
function PWMDeviceProperties(PWM:PPWMDevice;var Properties:PPWMProperties):LongWord;
  
function PWMDeviceCreate:PPWMDevice;
function PWMDeviceCreateEx(Size:LongWord):PPWMDevice;
function PWMDeviceDestroy(PWM:PPWMDevice):LongWord;

function PWMDeviceRegister(PWM:PPWMDevice):LongWord;
function PWMDeviceDeregister(PWM:PPWMDevice):LongWord;

function PWMDeviceFind(PWMId:LongWord):PPWMDevice;
function PWMDeviceEnumerate(Callback:TPWMEnumerate;Data:Pointer):LongWord;
 
function PWMDeviceNotification(PWM:PPWMDevice;Callback:TPWMNotification;Data:Pointer;Notification,Flags:LongWord):LongWord;
 
{==============================================================================}
{RTL PWM Functions}
 
{==============================================================================}
{PWM Helper Functions}
function PWMGetCount:LongWord; inline;
function PWMDeviceGetDefault:PPWMDevice; inline;

function PWMDeviceCheck(PWM:PPWMDevice):PPWMDevice;

procedure PWMLog(Level:LongWord;PWM:PPWMDevice;const AText:String);
procedure PWMLogInfo(PWM:PPWMDevice;const AText:String); inline;
procedure PWMLogError(PWM:PPWMDevice;const AText:String); inline;
procedure PWMLogDebug(PWM:PPWMDevice;const AText:String); inline;
 
{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {PWM specific variables}
 PWMInitialized:Boolean;

 PWMDeviceTable:PPWMDevice;
 PWMDeviceTableLock:TCriticalSectionHandle = INVALID_HANDLE_VALUE;
 PWMDeviceTableCount:LongWord;

 PWMDeviceDefault:PPWMDevice;

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure PWMInit;
begin
 {}
 {Check Initialized}
 if PWMInitialized then Exit;
 
 {Initialize Logging}
 PWM_LOG_ENABLED:=(PWM_DEFAULT_LOG_LEVEL <> PWM_LOG_LEVEL_NONE); 
 
 {Initialize PWM Table}
 PWMDeviceTable:=nil;
 PWMDeviceTableLock:=CriticalSectionCreate; 
 PWMDeviceTableCount:=0;
 if PWMDeviceTableLock = INVALID_HANDLE_VALUE then
  begin
   if PWM_LOG_ENABLED then PWMLogError(nil,'Failed to create PWM table lock');
  end;
 PWMDeviceDefault:=nil;
 
 {Register Platform PWM Handlers}
 //To Do
 
 PWMInitialized:=True;
end;
 
{==============================================================================}
{==============================================================================}
{PWM Functions}
{==============================================================================}
 
function PWMDeviceProperties(PWM:PPWMDevice;var Properties:PPWMProperties):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 //To Do
 
end;

{==============================================================================}

function PWMDeviceCreate:PPWMDevice;
{Create a new PWM entry}
{Return: Pointer to new PWM entry or nil if PWM could not be created}
begin
 {}
 Result:=PWMDeviceCreateEx(SizeOf(TPWMDevice));
end;

{==============================================================================}

function PWMDeviceCreateEx(Size:LongWord):PPWMDevice;
{Create a new PWM entry}
{Size: Size in bytes to allocate for new PWM (Including the PWM entry)}
{Return: Pointer to new PWM entry or nil if PWM could not be created}
begin
 {}
 Result:=nil;
 
 {Check Size}
 if Size < SizeOf(TPWMDevice) then Exit;
 
 {Create PWM}
 Result:=PPWMDevice(DeviceCreateEx(Size));
 if Result = nil then Exit;
 
 {Update Device}
 Result.Device.DeviceBus:=DEVICE_BUS_NONE;   
 Result.Device.DeviceType:=PWM_TYPE_NONE;
 Result.Device.DeviceFlags:=PWM_FLAG_NONE;
 Result.Device.DeviceData:=nil;

 {Update PWM}
 Result.PWMId:=DEVICE_ID_ANY;
 Result.PWMState:=PWM_STATE_DISABLED;
 //To Do
 Result.DeviceProperties:=nil;
 Result.Lock:=INVALID_HANDLE_VALUE;
 
 {Create Lock}
 Result.Lock:=MutexCreate;
 if Result.Lock = INVALID_HANDLE_VALUE then
  begin
   if PWM_LOG_ENABLED then PWMLogError(nil,'Failed to create lock for PWM device');
   PWMDeviceDestroy(Result);
   Result:=nil;
   Exit;
  end;
end;

{==============================================================================}

function PWMDeviceDestroy(PWM:PPWMDevice):LongWord;
{Destroy an existing PWM entry}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check PWM}
 if PWM = nil then Exit;
 if PWM.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check PWM}
 Result:=ERROR_IN_USE;
 if PWMDeviceCheck(PWM) = PWM then Exit;

 {Check State}
 if PWM.Device.DeviceState <> DEVICE_STATE_UNREGISTERED then Exit;
 
 {Destroy Lock}
 if PWM.Lock <> INVALID_HANDLE_VALUE then
  begin
   MutexDestroy(PWM.Lock);
  end;
 
 {Destroy PWM} 
 Result:=DeviceDestroy(@PWM.Device);
end;

{==============================================================================}

function PWMDeviceRegister(PWM:PPWMDevice):LongWord;
{Register a new PWM in the PWM table}
var
 PWMId:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check PWM}
 if PWM = nil then Exit;
 if PWM.PWMId <> DEVICE_ID_ANY then Exit;
 if PWM.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check PWM}
 Result:=ERROR_ALREADY_EXISTS;
 if PWMDeviceCheck(PWM) = PWM then Exit;
 
 {Check State}
 if PWM.Device.DeviceState <> DEVICE_STATE_UNREGISTERED then Exit;
 
 {Insert PWM}
 if CriticalSectionLock(PWMDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Update PWM}
    PWMId:=0;
    while PWMDeviceFind(PWMId) <> nil do
     begin
      Inc(PWMId);
     end;
    PWM.PWMId:=PWMId;
    
    {Update Device}
    PWM.Device.DeviceName:=PWM_NAME_PREFIX + IntToStr(PWM.PWMId); 
    PWM.Device.DeviceClass:=DEVICE_CLASS_PWM;
    
    {Register Device}
    Result:=DeviceRegister(@PWM.Device);
    if Result <> ERROR_SUCCESS then
     begin
      PWM.PWMId:=DEVICE_ID_ANY;
      Exit;
     end; 
    
    {Link PWM}
    if PWMDeviceTable = nil then
     begin
      PWMDeviceTable:=PWM;
     end
    else
     begin
      PWM.Next:=PWMDeviceTable;
      PWMDeviceTable.Prev:=PWM;
      PWMDeviceTable:=PWM;
     end;
 
    {Increment Count}
    Inc(PWMDeviceTableCount);
    
    {Check Default}
    if PWMDeviceDefault = nil then
     begin
      PWMDeviceDefault:=PWM;
     end;
    
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    CriticalSectionUnlock(PWMDeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;

{==============================================================================}

function PWMDeviceDeregister(PWM:PPWMDevice):LongWord;
{Deregister a PWM from the PWM table}
var
 Prev:PPWMDevice;
 Next:PPWMDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check PWM}
 if PWM = nil then Exit;
 if PWM.PWMId = DEVICE_ID_ANY then Exit;
 if PWM.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check PWM}
 Result:=ERROR_NOT_FOUND;
 if PWMDeviceCheck(PWM) <> PWM then Exit;
 
 {Check State}
 if PWM.Device.DeviceState <> DEVICE_STATE_REGISTERED then Exit;
 
 {Remove PWM}
 if CriticalSectionLock(PWMDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Deregister Device}
    Result:=DeviceDeregister(@PWM.Device);
    if Result <> ERROR_SUCCESS then Exit;
    
    {Unlink PWM}
    Prev:=PWM.Prev;
    Next:=PWM.Next;
    if Prev = nil then
     begin
      PWMDeviceTable:=Next;
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
    Dec(PWMDeviceTableCount);
 
    {Check Default}
    if PWMDeviceDefault = PWM then
     begin
      PWMDeviceDefault:=PWMDeviceTable;
     end;
 
    {Update PWM}
    PWM.PWMId:=DEVICE_ID_ANY;
 
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    CriticalSectionUnlock(PWMDeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;

{==============================================================================}

function PWMDeviceFind(PWMId:LongWord):PPWMDevice;
var
 PWM:PPWMDevice;
begin
 {}
 Result:=nil;
 
 {Check Id}
 if PWMId = DEVICE_ID_ANY then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(PWMDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get PWM}
    PWM:=PWMDeviceTable;
    while PWM <> nil do
     begin
      {Check State}
      if PWM.Device.DeviceState = DEVICE_STATE_REGISTERED then
       begin
        {Check Id}
        if PWM.PWMId = PWMId then
         begin
          Result:=PWM;
          Exit;
         end;
       end;
       
      {Get Next}
      PWM:=PWM.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(PWMDeviceTableLock);
   end;
  end;
end;
       
{==============================================================================}

function PWMDeviceEnumerate(Callback:TPWMEnumerate;Data:Pointer):LongWord;
var
 PWM:PPWMDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Callback}
 if not Assigned(Callback) then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(PWMDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get PWM}
    PWM:=PWMDeviceTable;
    while PWM <> nil do
     begin
      {Check State}
      if PWM.Device.DeviceState = DEVICE_STATE_REGISTERED then
       begin
        if Callback(PWM,Data) <> ERROR_SUCCESS then Exit;
       end;
       
      {Get Next}
      PWM:=PWM.Next;
     end;
     
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    CriticalSectionUnlock(PWMDeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;

{==============================================================================}

function PWMDeviceNotification(PWM:PPWMDevice;Callback:TPWMNotification;Data:Pointer;Notification,Flags:LongWord):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check PWM}
 if PWM = nil then
  begin
   Result:=DeviceNotification(nil,DEVICE_CLASS_PWM,Callback,Data,Notification,Flags);
  end
 else
  begin 
   {Check PWM}
   if PWM.Device.Signature <> DEVICE_SIGNATURE then Exit;

   Result:=DeviceNotification(@PWM.Device,DEVICE_CLASS_PWM,Callback,Data,Notification,Flags);
  end; 
end;

{==============================================================================}
{==============================================================================}
{RTL PWM Functions}

{==============================================================================}
{==============================================================================}
{PWM Helper Functions}
function PWMGetCount:LongWord; inline;
{Get the current PWM count}
begin
 {}
 Result:=PWMDeviceTableCount;
end;

{==============================================================================}

function PWMDeviceGetDefault:PPWMDevice; inline;
{Get the current default PWM device}
begin
 {}
 Result:=PWMDeviceDefault;
end;

{==============================================================================}

function PWMDeviceCheck(PWM:PPWMDevice):PPWMDevice;
{Check if the supplied PWM is in the PWM table}
var
 Current:PPWMDevice;
begin
 {}
 Result:=nil;
 
 {Check PWM}
 if PWM = nil then Exit;
 if PWM.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(PWMDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get PWM}
    Current:=PWMDeviceTable;
    while Current <> nil do
     begin
      {Check PWM}
      if Current = PWM then
       begin
        Result:=PWM;
        Exit;
       end;
      
      {Get Next}
      Current:=Current.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(PWMDeviceTableLock);
   end;
  end;
end;

{==============================================================================}

procedure PWMLog(Level:LongWord;PWM:PPWMDevice;const AText:String);
var
 WorkBuffer:String;
begin
 {}
 {Check Level}
 if Level < PWM_DEFAULT_LOG_LEVEL then Exit;
 
 WorkBuffer:='';
 {Check Level}
 if Level = PWM_LOG_LEVEL_DEBUG then
  begin
   WorkBuffer:=WorkBuffer + '[DEBUG] ';
  end
 else if Level = PWM_LOG_LEVEL_ERROR then
  begin
   WorkBuffer:=WorkBuffer + '[ERROR] ';
  end;
 
 {Add Prefix}
 WorkBuffer:=WorkBuffer + 'PWM: ';
 
 {Check PWM}
 if PWM <> nil then
  begin
   WorkBuffer:=WorkBuffer + PWM_NAME_PREFIX + IntToStr(PWM.PWMId) + ': ';
  end;

 {Output Logging}  
 LoggingOutputEx(LOGGING_FACILITY_PWM,LogLevelToLoggingSeverity(Level),'PWM',WorkBuffer + AText);
end;

{==============================================================================}

procedure PWMLogInfo(PWM:PPWMDevice;const AText:String); inline;
begin
 {}
 PWMLog(PWM_LOG_LEVEL_INFO,PWM,AText);
end;

{==============================================================================}

procedure PWMLogError(PWM:PPWMDevice;const AText:String); inline;
begin
 {}
 PWMLog(PWM_LOG_LEVEL_ERROR,PWM,AText);
end;

{==============================================================================}

procedure PWMLogDebug(PWM:PPWMDevice;const AText:String); inline;
begin
 {}
 PWMLog(PWM_LOG_LEVEL_DEBUG,PWM,AText);
end;

{==============================================================================}
{==============================================================================}

initialization
 PWMInit;

{==============================================================================}
 
finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
