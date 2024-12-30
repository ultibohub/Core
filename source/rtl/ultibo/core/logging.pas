{
Ultibo Logging interface unit.

Copyright (C) 2024 - SoftOz Pty Ltd.

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


Logging
=======

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit Logging;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Console,Devices,SysUtils;

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
const
 {Logging specific constants}
 LOGGING_NAME_PREFIX = 'Logging';                  {Name prefix for Logging Devices}

 LOGGING_THREAD_NAME = 'Logging';                  {Thread name for Logging threads}
 LOGGING_THREAD_PRIORITY = THREAD_PRIORITY_NORMAL; {Thread priority for Logging threads}
 
 {Logging Device Types}
 LOGGING_TYPE_NONE      = 0;
 LOGGING_TYPE_CONSOLE   = 1;
 LOGGING_TYPE_FILE      = 2;
 LOGGING_TYPE_SYSLOG    = 3;
 LOGGING_TYPE_SERIAL    = 4;
 
 LOGGING_TYPE_MAX       = 4;
  
 {Logging Type Names}
 LOGGING_TYPE_NAMES:array[LOGGING_TYPE_NONE..LOGGING_TYPE_MAX] of String = (
  'LOGGING_TYPE_NONE',
  'LOGGING_TYPE_CONSOLE',
  'LOGGING_TYPE_FILE',
  'LOGGING_TYPE_SYSLOG',
  'LOGGING_TYPE_SERIAL');
 
 {Logging Device States}
 LOGGING_STATE_DISABLED   = 0;
 LOGGING_STATE_ENABLED    = 1;
 
 LOGGING_STATE_MAX        = 1;
 
 {Logging State Names}
 LOGGING_STATE_NAMES:array[LOGGING_STATE_DISABLED..LOGGING_STATE_ENABLED] of String = (
  'LOGGING_STATE_DISABLED',
  'LOGGING_STATE_ENABLED');
 
 {Logging Device Flags}
 LOGGING_FLAG_NONE      = $00000000;

{==============================================================================}
const
 {Console Logging specific constants}
 CONSOLE_LOGGING_DESCRIPTION = 'Console Logging';
 
{==============================================================================}
type
 {Logging specific types}
 PLoggingEntry = ^TLoggingEntry;
 TLoggingEntry = record {Note: Overlaid on TMessage to avoid memory allocation on each output}
  Data:String;          {TMessage.Msg:PtrUInt}
  Reserved1:PtrUInt;    {TMessage.wParam:PtrUInt}
  Reserved2:PtrInt;     {TMessage.lParam:PtrInt}
  Reserved3:LongWord;   {TMessage.Time:LongWord}
 end;

 PLoggingEntryEx = ^TLoggingEntryEx;
 TLoggingEntryEx = record {Note: Overlaid on TMessage to avoid memory allocation on each output}
  Content:String;         {TMessage.Msg:PtrUInt}
  Tag:String;             {TMessage.wParam:PtrUInt}
  Severity:PtrInt;        {TMessage.lParam:PtrInt}
  Facility:LongWord;      {TMessage.Time:LongWord}
 end;
 
 PLoggingDevice = ^TLoggingDevice;
 
 {Logging Enumeration Callback}
 TLoggingEnumerate = function(Logging:PLoggingDevice;Data:Pointer):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 {Logging Notification Callback}
 TLoggingNotification = function(Device:PDevice;Data:Pointer;Notification:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 
 {Logging Device Methods}
 TLoggingDeviceStart = function(Logging:PLoggingDevice):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TLoggingDeviceStop = function(Logging:PLoggingDevice):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TLoggingDeviceOutput = function(Logging:PLoggingDevice;const Data:String):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TLoggingDeviceOutputEx = function(Logging:PLoggingDevice;Facility,Severity:LongWord;const Tag,Content:String):LongWord;{$IFDEF i386} stdcall;{$ENDIF} {Syslog compatible output}
 TLoggingDeviceGetTarget = function(Logging:PLoggingDevice):String;{$IFDEF i386} stdcall;{$ENDIF}
 TLoggingDeviceSetTarget = function(Logging:PLoggingDevice;const Target:String):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 
 {Logging Device}
 TLoggingDevice = record
  {Device Properties}
  Device:TDevice;                                {The Device entry for this Logging device}
  {Logging Properties}
  LoggingId:LongWord;                            {Unique Id of this Logging device in the Logging device table}
  LoggingState:LongWord;                         {Logging device state (eg LOGGING_STATE_ENABLED)}
  DeviceStart:TLoggingDeviceStart;               {A device specific DeviceStart method implementing a standard logging device interface (Or nil if the default method is suitable)}
  DeviceStop:TLoggingDeviceStop;                 {A device specific DeviceStop method implementing a standard logging device interface (Or nil if the default method is suitable)}
  DeviceOutput:TLoggingDeviceOutput;             {A device specific DeviceOutput method implementing a standard logging device interface}
  DeviceOutputEx:TLoggingDeviceOutputEx;         {A device specific DeviceOutputEx method implementing a standard logging device interface}
  DeviceGetTarget:TLoggingDeviceGetTarget;       {A device specific DeviceGetTarget method implementing a standard logging device interface (Or nil if the default method is suitable)}
  DeviceSetTarget:TLoggingDeviceSetTarget;       {A device specific DeviceSetTarget method implementing a standard logging device interface (Or nil if the default method is suitable)}
  {Statistics Properties}
  OutputCount:LongWord;
  {Driver Properties}
  Lock:TMutexHandle;                             {Device lock}
  Handle:THandle;                                {Device output handle}
  Target:String;                                 {Device output target}
  Default:LongBool;                              {Device can be the default logging device}
  {Internal Properties}
  Prev:PLoggingDevice;                           {Previous entry in Logging device table}
  Next:PLoggingDevice;                           {Next entry in Logging device table}
 end;

{==============================================================================}
type
 {Console Logging specific types}
 PConsoleLogging = ^TConsoleLogging;
 
 {Console Logging}
 TConsoleLogging = record
  {Logging Properties}
  Logging:TLoggingDevice;
  {Console Properties}
  Console:PConsoleDevice;  {The console device for logging output}
  Window:TWindowHandle;    {The console window for logging output}
  Existing:LongBool;       {True if the console window already existed when logging started}
 end;
 
{==============================================================================}
{var}
 {Logging specific variables}
 
{==============================================================================}
{Initialization Functions}
procedure LoggingInit;

function LoggingExecute(Parameter:Pointer):PtrInt;

{==============================================================================}
{Logging Functions}
function LoggingDeviceStart(Logging:PLoggingDevice):LongWord;
function LoggingDeviceStop(Logging:PLoggingDevice):LongWord;

function LoggingDeviceOutput(Logging:PLoggingDevice;const Data:String):LongWord;
function LoggingDeviceOutputEx(Logging:PLoggingDevice;Facility,Severity:LongWord;const Tag,Content:String):LongWord;

function LoggingDeviceGetTarget(Logging:PLoggingDevice):String;
function LoggingDeviceSetTarget(Logging:PLoggingDevice;const Target:String):LongWord;

function LoggingDeviceCreate(Default:Boolean):PLoggingDevice;
function LoggingDeviceCreateEx(Size:LongWord;Default:Boolean):PLoggingDevice;
function LoggingDeviceDestroy(Logging:PLoggingDevice):LongWord;

function LoggingDeviceRegister(Logging:PLoggingDevice):LongWord;
function LoggingDeviceDeregister(Logging:PLoggingDevice):LongWord;

function LoggingDeviceFind(LoggingId:LongWord):PLoggingDevice;
function LoggingDeviceFindByType(LoggingType:LongWord):PLoggingDevice;
function LoggingDeviceFindByDevice(Device:PDevice):PLoggingDevice;
function LoggingDeviceFindByName(const Name:String):PLoggingDevice; inline;
function LoggingDeviceFindByDescription(const Description:String):PLoggingDevice; inline;

function LoggingDeviceEnumerate(Callback:TLoggingEnumerate;Data:Pointer):LongWord;

function LoggingDeviceNotification(Logging:PLoggingDevice;Callback:TLoggingNotification;Data:Pointer;Notification,Flags:LongWord):LongWord;

{==============================================================================}
{Console Logging Functions}
function ConsoleLoggingStart(Logging:PLoggingDevice):LongWord;
function ConsoleLoggingStop(Logging:PLoggingDevice):LongWord;

function ConsoleLoggingOutput(Logging:PLoggingDevice;const Data:String):LongWord;

function ConsoleLoggingSetTarget(Logging:PLoggingDevice;const Target:String):LongWord;

{==============================================================================}
{RTL Text IO Functions}
function SysTextIOWriteChar(ACh:Char;AUserData:Pointer):Boolean;
function SysTextIOWriteBuffer(ABuffer:PChar;ACount:LongInt;AUserData:Pointer):LongInt;

{==============================================================================}
{RTL Logging Functions}
procedure SysLoggingOutput(const AText:String);
procedure SysLoggingOutputEx(AFacility,ASeverity:LongWord;const ATag,AContent:String);

{==============================================================================}
{Logging Helper Functions}
function LoggingDeviceGetCount:LongWord;

function LoggingDeviceGetDefault:PLoggingDevice;
function LoggingDeviceSetDefault(Logging:PLoggingDevice):LongWord; 

function LoggingDeviceCheck(Logging:PLoggingDevice):PLoggingDevice;

function LoggingTypeToString(LoggingType:LongWord):String;
function LoggingStateToString(LoggingState:LongWord):String;

function LoggingDeviceRedirectOutput(Logging:PLoggingDevice):Boolean; 

function LoggingGetMessageslotFlags:LongWord;

function LoggingConsoleDeviceAdd(Console:PConsoleDevice):LongWord;
function LoggingConsoleDeviceRemove(Console:PConsoleDevice):LongWord;

function LoggingConsoleDeviceEnum(Console:PConsoleDevice;Data:Pointer):LongWord;
function LoggingConsoleDeviceNotify(Device:PDevice;Data:Pointer;Notification:LongWord):LongWord;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {Logging specific variables}
 LoggingInitialized:Boolean;
 
 LoggingThread:TThreadHandle = INVALID_HANDLE_VALUE;
 LoggingMessageslot:TMessageslotHandle = INVALID_HANDLE_VALUE;
 
 LoggingDeviceTable:PLoggingDevice;
 LoggingDeviceTableLock:TCriticalSectionHandle = INVALID_HANDLE_VALUE;
 LoggingDeviceTableCount:LongWord;
 
 LoggingDeviceDefault:PLoggingDevice;

 LoggingDirectLock:TMutexHandle = INVALID_HANDLE_VALUE;
 LoggingOutputCount:LongWord;
 
 LoggingTextIOOutputDevice:PLoggingDevice;
 
{==============================================================================}
{==============================================================================}
threadvar
 {Logging specific thread variables}
 LoggingTextIOBuffer:String;
 
{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure LoggingInit;
var
 WorkInt:LongWord;
 WorkBuffer:String;
 Thread:TThreadHandle; 
begin
 {}
 {Check Initialized}
 if LoggingInitialized then Exit;
 
 {$IFDEF LOGGING_EARLY_INIT}
 {Initialize Device Support}
 DevicesInit;
 
 {Initialize Console Support}
 ConsoleInit;
 {$ENDIF}
 
 {Initialize Logging Device Table}
 LoggingDeviceTable:=nil;
 LoggingDeviceTableLock:=CriticalSectionCreate; 
 LoggingDeviceTableCount:=0;
 if LoggingDeviceTableLock = INVALID_HANDLE_VALUE then
  begin
   if DEVICE_LOG_ENABLED then DeviceLogError(nil,'Failed to create logging device table lock');
  end;
 LoggingDeviceDefault:=nil;
 
 {Create Logging Messageslot}
 LoggingMessageslot:=MessageslotCreateEx(LOGGING_MESSAGESLOT_MAXIMUM,LoggingGetMessageslotFlags);
 
 {Create Logging Thread} 
 LoggingThread:=BeginThread(LoggingExecute,nil,Thread,THREAD_STACK_DEFAULT_SIZE);
 if LoggingThread = INVALID_HANDLE_VALUE then 
  begin
   if DEVICE_LOG_ENABLED then DeviceLogError(nil,'Failed to create logging thread');
  end
 else
  begin 
   {Setup Logging Thread}
   ThreadSetName(LoggingThread,LOGGING_THREAD_NAME);
   ThreadSetPriority(LoggingThread,LOGGING_THREAD_PRIORITY);
  end;
 
 {Create Direct Logging Lock}
 LoggingDirectLock:=MutexCreate;
 if LoggingDirectLock = INVALID_HANDLE_VALUE then
  begin
   if DEVICE_LOG_ENABLED then DeviceLogError(nil,'Failed to create direct logging lock');
  end;

 {Check Environment Variables}
 {CONSOLE_REGISTER_LOGGING}
 WorkInt:=StrToIntDef(EnvironmentGet('CONSOLE_REGISTER_LOGGING'),0);
 if WorkInt <> 0 then CONSOLE_REGISTER_LOGGING:=True;
 
 {CONSOLE_LOGGING_DEFAULT}
 WorkInt:=StrToIntDef(EnvironmentGet('CONSOLE_LOGGING_DEFAULT'),0);
 if WorkInt <> 0 then CONSOLE_LOGGING_DEFAULT:=True;
 
 {CONSOLE_LOGGING_POSITION}
 WorkInt:=StrToIntDef(EnvironmentGet('CONSOLE_LOGGING_POSITION'),0);
 if WorkInt > 0 then CONSOLE_LOGGING_POSITION:=WorkInt;
 
 {CONSOLE_LOGGING_DEVICE}
 WorkBuffer:=EnvironmentGet('CONSOLE_LOGGING_DEVICE');
 if Length(WorkBuffer) <> 0 then CONSOLE_LOGGING_DEVICE:=WorkBuffer;
 
 {Enumerate Consoles}
 ConsoleDeviceEnumerate(LoggingConsoleDeviceEnum,nil);
 
 {Register Notification}
 ConsoleDeviceNotification(nil,LoggingConsoleDeviceNotify,nil,DEVICE_NOTIFICATION_REGISTER or DEVICE_NOTIFICATION_DEREGISTER or DEVICE_NOTIFICATION_OPEN or DEVICE_NOTIFICATION_CLOSE,NOTIFIER_FLAG_WORKER);
  
 {Register Platform Text IO Handlers}
 {TextIOWriteCharHandler:=SysTextIOWriteChar;}     {Only registered when calling LoggingDeviceRedirectOutput}
 {TextIOWriteBufferHandler:=SysTextIOWriteBuffer;} {Only registered when calling LoggingDeviceRedirectOutput}
  
 {Register Platform Logging Handlers}
 LoggingOutputHandler:=SysLoggingOutput;
 LoggingOutputExHandler:=SysLoggingOutputEx;
 
 LoggingInitialized:=True;
end;

{==============================================================================}

function LoggingExecute(Parameter:Pointer):PtrInt;
var
 Message:TMessage;
 WorkBuffer:String;

 LoggingEntry:PLoggingEntry;
 LoggingEntryEx:PLoggingEntryEx;
begin
 {}
 Result:=0;
 try
  while True do
   begin
    {Wait to Receive Message}
    FillChar(Message,SizeOf(TMessage),0);
    if MessageslotReceive(LoggingMessageslot,Message) = ERROR_SUCCESS then
     begin
      {Process Message}
      LoggingEntry:=PLoggingEntry(@Message); {Do not free}
      if (LoggingEntry.Reserved2 = LongInt(LOGGING_SEVERITY_INVALID)) or (LoggingEntry.Reserved3 = LOGGING_FACILITY_INVALID) then
       begin
        {Setup Logging}
        if LOGGING_INCLUDE_COUNTER then
         begin
          WorkBuffer:=IntToHex(LoggingOutputCount,8) + ' - ' + LoggingEntry.Data;
         end
        else
         begin
          WorkBuffer:=LoggingEntry.Data;
         end;
    
        {Output Logging}
        LoggingDeviceOutput(LoggingDeviceDefault,WorkBuffer);
   
        {Update Logging Count}
        Inc(LoggingOutputCount);
        
        {Release Logging}
        SetLength(LoggingEntry.Data,0);
       end
      else
       begin
        LoggingEntryEx:=PLoggingEntryEx(@Message); {Do not free}
        
        {Setup Logging}
        if LOGGING_INCLUDE_COUNTER then
         begin
          WorkBuffer:=IntToHex(LoggingOutputCount,8) + ' - ' + LoggingEntryEx.Content;
         end
        else
         begin
          WorkBuffer:=LoggingEntryEx.Content;
         end;
        
        {Output Logging}
        LoggingDeviceOutputEx(LoggingDeviceDefault,LoggingEntryEx.Facility,LoggingEntryEx.Severity,LoggingEntryEx.Tag,WorkBuffer);
   
        {Update Logging Count}
        Inc(LoggingOutputCount);
        
        {Release Logging}
        SetLength(LoggingEntryEx.Tag,0);
        SetLength(LoggingEntryEx.Content,0);
       end;
     end;
    
    {Yield}   
    {ThreadYield;}
   end; 
 except
  on E: Exception do
   begin
    if DEVICE_LOG_ENABLED then DeviceLogError(nil,'LoggingThread: Exception: ' + E.Message + ' at ' + PtrToHex(ExceptAddr));
   end;
 end; 
end;

{==============================================================================}
{==============================================================================}
{Logging Functions}
function LoggingDeviceStart(Logging:PLoggingDevice):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Logging}
 if Logging = nil then Exit;
 if Logging.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IF DEFINED(LOGGING_DEBUG) or DEFINED(DEVICE_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Logging Device Start');
 {$ENDIF}

 {Check Disabled}
 Result:=ERROR_SUCCESS;
 if Logging.LoggingState <> LOGGING_STATE_DISABLED then Exit;

 {Check Start}
 Result:=ERROR_INVALID_PARAMETER;
 if Assigned(Logging.DeviceStart) then
  begin
   {Call Device Start}
   Result:=Logging.DeviceStart(Logging);
   if Result <> ERROR_SUCCESS then Exit;
  end; 
 
 {Enable Device}
 Logging.LoggingState:=LOGGING_STATE_ENABLED;

 {Notify Enable}
 NotifierNotify(@Logging.Device,DEVICE_NOTIFICATION_ENABLE);
 
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function LoggingDeviceStop(Logging:PLoggingDevice):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Logging}
 if Logging = nil then Exit;
 if Logging.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IF DEFINED(LOGGING_DEBUG) or DEFINED(DEVICE_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Logging Device Stop');
 {$ENDIF}

 {Check Enabled}
 Result:=ERROR_SUCCESS;
 if Logging.LoggingState <> LOGGING_STATE_ENABLED then Exit;
 
 {Check Stop}
 if Assigned(Logging.DeviceStop) then
  begin
   {Call Device Stop}
   Result:=Logging.DeviceStop(Logging);
   if Result <> ERROR_SUCCESS then Exit;
  end; 
 
 {Disable Device}
 Logging.LoggingState:=LOGGING_STATE_DISABLED;

 {Notify Disable}
 NotifierNotify(@Logging.Device,DEVICE_NOTIFICATION_DISABLE);
 
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function LoggingDeviceOutput(Logging:PLoggingDevice;const Data:String):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Logging}
 if Logging = nil then Exit;
 if Logging.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {Check Enabled}
 Result:=ERROR_NOT_SUPPORTED;
 if Logging.LoggingState <> LOGGING_STATE_ENABLED then Exit;
 
 if Assigned(Logging.DeviceOutput) then
  begin
   Result:=Logging.DeviceOutput(Logging,Data);
  end;
end;

{==============================================================================}

function LoggingDeviceOutputEx(Logging:PLoggingDevice;Facility,Severity:LongWord;const Tag,Content:String):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Logging}
 if Logging = nil then Exit;
 if Logging.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {Check Enabled}
 Result:=ERROR_NOT_SUPPORTED;
 if Logging.LoggingState <> LOGGING_STATE_ENABLED then Exit;
 
 if Assigned(Logging.DeviceOutputEx) then
  begin
   Result:=Logging.DeviceOutputEx(Logging,Facility,Severity,Tag,Content);
  end
 else if Assigned(Logging.DeviceOutput) then 
  begin
   {Default to Output}
   Result:=Logging.DeviceOutput(Logging,Content);
  end;
end;

{==============================================================================}

function LoggingDeviceGetTarget(Logging:PLoggingDevice):String;
begin
 {}
 Result:='';
 
 {Check Logging}
 if Logging = nil then Exit;
 if Logging.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {$IF DEFINED(LOGGING_DEBUG) or DEFINED(DEVICE_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Logging Device Get Target');
 {$ENDIF}
 
 {Check Enabled}
 {if Logging.LoggingState <> LOGGING_STATE_ENABLED then Exit;} {Allow when disabled}
 
 if Assigned(Logging.DeviceGetTarget) then
  begin
   Result:=Logging.DeviceGetTarget(Logging);
  end
 else
  begin
   if MutexLock(Logging.Lock) <> ERROR_SUCCESS then Exit;
   
   Result:=Logging.Target;
   UniqueString(Result);
   
   MutexUnlock(Logging.Lock);
  end;  
end;

{==============================================================================}

function LoggingDeviceSetTarget(Logging:PLoggingDevice;const Target:String):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Logging}
 if Logging = nil then Exit;
 if Logging.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {$IF DEFINED(LOGGING_DEBUG) or DEFINED(DEVICE_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Logging Device Set Target');
 {$ENDIF}
 
 {Check Enabled}
 {if Logging.LoggingState <> LOGGING_STATE_ENABLED then Exit;} {Allow when disabled}
 
 if Assigned(Logging.DeviceSetTarget) then
  begin
   Result:=Logging.DeviceSetTarget(Logging,Target);
  end
 else
  begin
   if MutexLock(Logging.Lock) <> ERROR_SUCCESS then Exit;
   
   Logging.Target:=Target;
   UniqueString(Logging.Target);
   
   Result:=ERROR_SUCCESS;
   
   MutexUnlock(Logging.Lock);
  end;  
end;

{==============================================================================}

function LoggingDeviceCreate(Default:Boolean):PLoggingDevice;
{Create a new Logging device entry}
{Default: If true make the new device the default logging device if there is no current default}
{Return: Pointer to new Logging device entry or nil if Logging device could not be created}
begin
 {}
 Result:=LoggingDeviceCreateEx(SizeOf(TLoggingDevice),Default);
end;

{==============================================================================}

function LoggingDeviceCreateEx(Size:LongWord;Default:Boolean):PLoggingDevice;
{Create a new Logging device entry}
{Size: Size in bytes to allocate for new Logging (Including the Logging entry)}
{Default: If true make the new device the default logging device if there is no current default}
{Return: Pointer to new Logging device entry or nil if Logging device could not be created}
begin
 {}
 Result:=nil;
 
 {Check Size}
 if Size < SizeOf(TLoggingDevice) then Exit;
 
 {Create Logging}
 Result:=PLoggingDevice(DeviceCreateEx(Size));
 if Result = nil then Exit;
 
 {Update Device}
 Result.Device.DeviceBus:=DEVICE_BUS_NONE;   
 Result.Device.DeviceType:=LOGGING_TYPE_NONE;
 Result.Device.DeviceFlags:=LOGGING_FLAG_NONE;
 Result.Device.DeviceData:=nil;

 {Update Logging}
 Result.LoggingId:=DEVICE_ID_ANY;
 Result.LoggingState:=LOGGING_STATE_DISABLED;
 Result.DeviceStart:=nil;
 Result.DeviceStop:=nil;
 Result.DeviceOutput:=nil;
 Result.DeviceGetTarget:=nil;
 Result.DeviceSetTarget:=nil;
 Result.Lock:=INVALID_HANDLE_VALUE;
 Result.Handle:=INVALID_HANDLE_VALUE;
 Result.Target:='';
 Result.Default:=Default;
 
 {Create Lock}
 Result.Lock:=MutexCreate;
 if Result.Lock = INVALID_HANDLE_VALUE then
  begin
   if DEVICE_LOG_ENABLED then DeviceLogError(nil,'Failed to create lock for logging device');
   LoggingDeviceDestroy(Result);
   Result:=nil;
   Exit;
  end;
end;

{==============================================================================}

function LoggingDeviceDestroy(Logging:PLoggingDevice):LongWord;
{Destroy an existing Logging device entry}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Logging}
 if Logging = nil then Exit;
 if Logging.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check Logging}
 Result:=ERROR_IN_USE;
 if LoggingDeviceCheck(Logging) = Logging then Exit;

 {Check State}
 if Logging.Device.DeviceState <> DEVICE_STATE_UNREGISTERED then Exit;
 
 {Destroy Lock}
 if Logging.Lock <> INVALID_HANDLE_VALUE then
  begin
   MutexDestroy(Logging.Lock);
  end;
 
 {Destroy Logging} 
 Result:=DeviceDestroy(@Logging.Device);
end;

{==============================================================================}

function LoggingDeviceRegister(Logging:PLoggingDevice):LongWord;
{Register a new Logging device in the Logging table}
var
 LoggingId:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Logging}
 if Logging = nil then Exit;
 if Logging.LoggingId <> DEVICE_ID_ANY then Exit;
 if Logging.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check Interfaces}
 if not(Assigned(Logging.DeviceOutput)) then Exit;
 
 {Check Logging}
 Result:=ERROR_ALREADY_EXISTS;
 if LoggingDeviceCheck(Logging) = Logging then Exit;
 
 {Check State}
 if Logging.Device.DeviceState <> DEVICE_STATE_UNREGISTERED then Exit;
 
 {Insert Logging}
 if CriticalSectionLock(LoggingDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Update Logging}
    LoggingId:=0;
    while LoggingDeviceFind(LoggingId) <> nil do
     begin
      Inc(LoggingId);
     end;
    Logging.LoggingId:=LoggingId;
    
    {Update Device}
    Logging.Device.DeviceName:=LOGGING_NAME_PREFIX + IntToStr(Logging.LoggingId);
    Logging.Device.DeviceClass:=DEVICE_CLASS_LOGGING;
    
    {Register Device}
    Result:=DeviceRegister(@Logging.Device);
    if Result <> ERROR_SUCCESS then
     begin
      Logging.LoggingId:=DEVICE_ID_ANY;
      Exit;
     end; 
    
    {Link Logging}
    if LoggingDeviceTable = nil then
     begin
      LoggingDeviceTable:=Logging;
     end
    else
     begin
      Logging.Next:=LoggingDeviceTable;
      LoggingDeviceTable.Prev:=Logging;
      LoggingDeviceTable:=Logging;
     end;
 
    {Increment Count}
    Inc(LoggingDeviceTableCount);
    
    {Check Default}
    if (LoggingDeviceDefault = nil) and (Logging.Default) then
     begin
      LoggingDeviceDefault:=Logging;
     end;
     
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    CriticalSectionUnlock(LoggingDeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;

{==============================================================================}

function LoggingDeviceDeregister(Logging:PLoggingDevice):LongWord;
{Deregister a Logging device from the Logging table}
var
 Prev:PLoggingDevice;
 Next:PLoggingDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Logging}
 if Logging = nil then Exit;
 if Logging.LoggingId = DEVICE_ID_ANY then Exit;
 if Logging.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check Logging}
 Result:=ERROR_NOT_FOUND;
 if LoggingDeviceCheck(Logging) <> Logging then Exit;
 
 {Check State}
 if Logging.Device.DeviceState <> DEVICE_STATE_REGISTERED then Exit;
 
 {Remove Logging}
 if CriticalSectionLock(LoggingDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Deregister Device}
    Result:=DeviceDeregister(@Logging.Device);
    if Result <> ERROR_SUCCESS then Exit;
    
    {Unlink Logging}
    Prev:=Logging.Prev;
    Next:=Logging.Next;
    if Prev = nil then
     begin
      LoggingDeviceTable:=Next;
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
    Dec(LoggingDeviceTableCount);
 
    {Check Default}
    if LoggingDeviceDefault = Logging then
     begin
      LoggingDeviceDefault:=LoggingDeviceTable;
      
      while (LoggingDeviceDefault <> nil) do
       begin
        if LoggingDeviceDefault.Default then Break;
        
        LoggingDeviceDefault:=LoggingDeviceDefault.Next;
       end;
     end;
     
    {Update Logging}
    Logging.LoggingId:=DEVICE_ID_ANY;
 
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    CriticalSectionUnlock(LoggingDeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;

{==============================================================================}

function LoggingDeviceFind(LoggingId:LongWord):PLoggingDevice;
var
 Logging:PLoggingDevice;
begin
 {}
 Result:=nil;
 
 {Check Id}
 if LoggingId = DEVICE_ID_ANY then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(LoggingDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Logging}
    Logging:=LoggingDeviceTable;
    while Logging <> nil do
     begin
      {Check State}
      if Logging.Device.DeviceState = DEVICE_STATE_REGISTERED then
       begin
        {Check Id}
        if Logging.LoggingId = LoggingId then
         begin
          Result:=Logging;
          Exit;
         end;
       end;

       {Get Next}
      Logging:=Logging.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(LoggingDeviceTableLock);
   end;
  end;
end;

{==============================================================================}

function LoggingDeviceFindByType(LoggingType:LongWord):PLoggingDevice;
var
 Logging:PLoggingDevice;
begin
 {}
 Result:=nil;

 {Acquire the Lock}
 if CriticalSectionLock(LoggingDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Logging}
    Logging:=LoggingDeviceTable;
    while Logging <> nil do
     begin
      {Check State}
      if Logging.Device.DeviceState = DEVICE_STATE_REGISTERED then
       begin
        {Check Type}
        if Logging.Device.DeviceType = LoggingType then
         begin
          Result:=Logging;
          Exit;
         end;
       end;

       {Get Next}
      Logging:=Logging.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(LoggingDeviceTableLock);
   end;
  end;
end;

{==============================================================================}

function LoggingDeviceFindByDevice(Device:PDevice):PLoggingDevice;
var
 Logging:PLoggingDevice;
begin
 {}
 Result:=nil;
 
 {Check Device}
 if Device = nil then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(LoggingDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Logging}
    Logging:=LoggingDeviceTable;
    while Logging <> nil do
     begin
      {Check State}
      if Logging.Device.DeviceState = DEVICE_STATE_REGISTERED then
       begin
        {Check Device}
        if Logging.Device.DeviceData = Device then
         begin
          Result:=Logging;
          Exit;
         end;
       end;

       {Get Next}
      Logging:=Logging.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(LoggingDeviceTableLock);
   end;
  end;
end;

{==============================================================================}

function LoggingDeviceFindByName(const Name:String):PLoggingDevice; inline;
begin
 {}
 Result:=PLoggingDevice(DeviceFindByName(Name));
end;

{==============================================================================}

function LoggingDeviceFindByDescription(const Description:String):PLoggingDevice; inline;
begin
 {}
 Result:=PLoggingDevice(DeviceFindByDescription(Description));
end;

{==============================================================================}

function LoggingDeviceEnumerate(Callback:TLoggingEnumerate;Data:Pointer):LongWord;
var
 Logging:PLoggingDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Callback}
 if not Assigned(Callback) then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(LoggingDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Logging}
    Logging:=LoggingDeviceTable;
    while Logging <> nil do
     begin
      {Check State}
      if Logging.Device.DeviceState = DEVICE_STATE_REGISTERED then
       begin
        if Callback(Logging,Data) <> ERROR_SUCCESS then Exit;
       end;
       
      {Get Next}
      Logging:=Logging.Next;
     end;
     
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    CriticalSectionUnlock(LoggingDeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;

{==============================================================================}

function LoggingDeviceNotification(Logging:PLoggingDevice;Callback:TLoggingNotification;Data:Pointer;Notification,Flags:LongWord):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Logging}
 if Logging = nil then
  begin
   Result:=DeviceNotification(nil,DEVICE_CLASS_LOGGING,Callback,Data,Notification,Flags);
  end
 else
  begin 
   {Check Logging}
   if Logging.Device.Signature <> DEVICE_SIGNATURE then Exit;

   Result:=DeviceNotification(@Logging.Device,DEVICE_CLASS_LOGGING,Callback,Data,Notification,Flags);
  end; 
end;

{==============================================================================}
{==============================================================================}
{Console Logging Functions}
function ConsoleLoggingStart(Logging:PLoggingDevice):LongWord;
{Implementation of LoggingDeviceStart API for Console Logging}
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
 
    {Check Console}
    if PConsoleLogging(Logging).Console = nil then Exit;

    {Find Window}
    PConsoleLogging(Logging).Window:=ConsoleWindowFind(PConsoleLogging(Logging).Console,CONSOLE_LOGGING_POSITION);
    PConsoleLogging(Logging).Existing:=True;
    if PConsoleLogging(Logging).Window = INVALID_HANDLE_VALUE then
     begin
      {Create Window}
      PConsoleLogging(Logging).Window:=ConsoleWindowCreate(PConsoleLogging(Logging).Console,CONSOLE_LOGGING_POSITION,False);
      PConsoleLogging(Logging).Existing:=False;
      if PConsoleLogging(Logging).Window = INVALID_HANDLE_VALUE then Exit;
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

function ConsoleLoggingStop(Logging:PLoggingDevice):LongWord;
{Implementation of LoggingDeviceStop API for Console Logging}
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
 
    {Check Console}
    if PConsoleLogging(Logging).Console = nil then Exit;
 
    {Check Window}
    if PConsoleLogging(Logging).Window = INVALID_HANDLE_VALUE then Exit;

    {Check Existing}
    if PConsoleLogging(Logging).Existing then
     begin
      Result:=ERROR_SUCCESS;
     end
    else
     begin
      {Destroy Window}
      Result:=ConsoleWindowDestroy(PConsoleLogging(Logging).Window);
     end;
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

function ConsoleLoggingOutput(Logging:PLoggingDevice;const Data:String):LongWord;
{Implementation of LoggingDeviceOutput API for Console Logging}
{Note: Not intended to be called directly by applications, use LoggingDeviceOutput instead}
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
 
    {Check Window}
    if PConsoleLogging(Logging).Window = INVALID_HANDLE_VALUE then Exit;
 
    {Check Console}
    if PConsoleLogging(Logging).Console = nil then Exit;
 
    {Console WriteLn}
    Result:=ConsoleWindowWriteLn(PConsoleLogging(Logging).Window,Data);
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

function ConsoleLoggingSetTarget(Logging:PLoggingDevice;const Target:String):LongWord;
{Implementation of LoggingDeviceSetTarget API for Console Logging}
{Note: Not intended to be called directly by applications, use LoggingDeviceSetTarget instead}
var
 Console:PConsoleDevice;
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
      Console:=ConsoleDeviceFindByName(Target);
      if Console = nil then
       begin
        {Check Description}
        Console:=ConsoleDeviceFindByDescription(Target);
       end;

      {Check Device}
      if Console = nil then Exit;

      {Check Window}
      if not(PConsoleLogging(Logging).Existing) and (PConsoleLogging(Logging).Window <> INVALID_HANDLE_VALUE) then
       begin
        {Destroy Window}
        ConsoleWindowDestroy(PConsoleLogging(Logging).Window);
       end;

      {Set Target}
      Logging.Target:=Target;
      UniqueString(Logging.Target);

      Result:=ERROR_OPERATION_FAILED;

      {Update Parameters}
      PConsoleLogging(Logging).Console:=Console;

      {Find Window}
      PConsoleLogging(Logging).Window:=ConsoleWindowFind(PConsoleLogging(Logging).Console,CONSOLE_LOGGING_POSITION);
      PConsoleLogging(Logging).Existing:=True;
      if PConsoleLogging(Logging).Window = INVALID_HANDLE_VALUE then
       begin
        {Create Window}
        PConsoleLogging(Logging).Window:=ConsoleWindowCreate(PConsoleLogging(Logging).Console,CONSOLE_LOGGING_POSITION,False);
        PConsoleLogging(Logging).Existing:=False;
        if PConsoleLogging(Logging).Window = INVALID_HANDLE_VALUE then Exit;
       end;
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
function SysTextIOWriteChar(ACh:Char;AUserData:Pointer):Boolean;
{Handler for platform TextIOWriteChar function}

{Note: Not intended to be called directly by applications}
begin
 {}
 Result:=True;
 
 {Check Char}
 case ACh of
  #10:begin
    case DefaultTextLineBreakStyle of
     tlbsLF,tlbsCRLF:begin
       {Check Default}
       if LoggingTextIOOutputDevice = LoggingDeviceDefault then
        begin
         {Output Logging}
         LoggingOutput(LoggingTextIOBuffer);
        end
       else
        begin       
         {Output Logging}
         LoggingDeviceOutput(LoggingTextIOOutputDevice,LoggingTextIOBuffer);
        end; 
       
       {Clear Buffer}
       SetLength(LoggingTextIOBuffer,0);
      end;
    end;  
   end; 
  #13:begin
    case DefaultTextLineBreakStyle of
     tlbsCR:begin
       {Check Default}
       if LoggingTextIOOutputDevice = LoggingDeviceDefault then
        begin
         {Output Logging}
         LoggingOutput(LoggingTextIOBuffer);
        end
       else
        begin       
         {Output Logging}
         LoggingDeviceOutput(LoggingTextIOOutputDevice,LoggingTextIOBuffer);
        end; 
       
       {Clear Buffer}
       SetLength(LoggingTextIOBuffer,0);
      end;
    end;
   end;  
  else
   begin
    {Add to Buffer}
    LoggingTextIOBuffer:=LoggingTextIOBuffer + ACh;
   end;
 end;  
end;

{==============================================================================}

function SysTextIOWriteBuffer(ABuffer:PChar;ACount:LongInt;AUserData:Pointer):LongInt;
{Handler for platform TextIOWriteBuffer function}

{Note: Not intended to be called directly by applications}
var
 WorkBuffer:String;
begin
 {}
 Result:=0;
 
 if (ABuffer <> nil) and (ACount > 0) then
  begin
   {Allocate Buffer}
   SetLength(WorkBuffer,ACount);
   
   {Copy Buffer}
   StrLCopy(PChar(WorkBuffer),ABuffer,ACount);
   
   {Check Default}
   if LoggingTextIOOutputDevice = LoggingDeviceDefault then
    begin
     {Output Logging}
     LoggingOutput(WorkBuffer);
    end
   else
    begin   
     {Output Logging}
     if LoggingDeviceOutput(LoggingTextIOOutputDevice,WorkBuffer) <> ERROR_SUCCESS then Exit;
    end; 
  end;

 Result:=ACount;  
end;

{==============================================================================}
{==============================================================================}
{RTL Logging Functions}
procedure SysLoggingOutput(const AText:String);
var
 Message:TMessage;
 WorkBuffer:String;
 LoggingEntry:PLoggingEntry;
begin
 {}
 {$IF not(DEFINED(INTERRUPT_DEBUG)) and not(DEFINED(HEAP_DEBUG)) and not(DEFINED(THREAD_DEBUG)) and not(DEFINED(PLATFORM_DEBUG))}
 if LOGGING_DIRECT_ENABLE then
  begin
   if MutexLock(LoggingDirectLock) <> ERROR_SUCCESS then Exit;
   
   {Setup Logging}
   if CPUGetCount > 1 then
    begin
     WorkBuffer:=IntToHex(ThreadGetCurrent,8) + ':' + IntToHex(CPUGetCurrent,1) + ' - ' + AText;
    end
   else
    begin
     WorkBuffer:=IntToHex(ThreadGetCurrent,8) + ' - ' + AText;
    end;
   
   {Check Options}
   if LOGGING_INCLUDE_TICKCOUNT then
    begin
     WorkBuffer:=IntToHex(GetTickCount64,16) + ' - ' + WorkBuffer;
    end; 
   if LOGGING_INCLUDE_DATETIME or (LOGGING_INCLUDE_DATE and LOGGING_INCLUDE_TIME) then
    begin
     WorkBuffer:=SystemDateTimeToString(Now) + ' - ' + WorkBuffer;
    end
   else
    begin
     if LOGGING_INCLUDE_DATE then
      begin
       WorkBuffer:=SystemDateToString(Now) + ' - ' + WorkBuffer;
      end;
     if LOGGING_INCLUDE_TIME then
      begin
       WorkBuffer:=SystemTimeToString(Now) + ' - ' + WorkBuffer;
      end;
    end;
   if LOGGING_INCLUDE_COUNTER then
    begin
     WorkBuffer:=IntToHex(LoggingOutputCount,8) + ' - ' + WorkBuffer;
    end; 

   {Remove CRLF}
   if WorkBuffer[Length(WorkBuffer)] = Chr(10) then SetLength(WorkBuffer,Length(WorkBuffer) - 1);
   if WorkBuffer[Length(WorkBuffer)] = Chr(13) then SetLength(WorkBuffer,Length(WorkBuffer) - 1);
   
   {Output Logging}
   LoggingDeviceOutput(LoggingDeviceDefault,WorkBuffer);
   
   {Update Logging Count}
   Inc(LoggingOutputCount);
   
   MutexUnlock(LoggingDirectLock);
  end
 else 
  begin
 {$ENDIF} 
   {Initialize Message}
   FillChar(Message,SizeOf(TMessage),0);
   
   {Setup Logging}
   if CPUGetCount > 1 then
    begin
     WorkBuffer:=IntToHex(ThreadGetCurrent,8) + ':' + IntToHex(CPUGetCurrent,1) + ' - ' + AText;
    end
   else
    begin
     WorkBuffer:=IntToHex(ThreadGetCurrent,8) + ' - ' + AText;
     end;
     
   {Check Options}
   if LOGGING_INCLUDE_TICKCOUNT then
    begin
     WorkBuffer:=IntToHex(GetTickCount64,16) + ' - ' + WorkBuffer;
    end; 
   if LOGGING_INCLUDE_DATETIME or (LOGGING_INCLUDE_DATE and LOGGING_INCLUDE_TIME) then
    begin
     WorkBuffer:=SystemDateTimeToString(Now) + ' - ' + WorkBuffer;
    end
   else
    begin
     if LOGGING_INCLUDE_DATE then
      begin
       WorkBuffer:=SystemDateToString(Now) + ' - ' + WorkBuffer;
      end;
     if LOGGING_INCLUDE_TIME then
      begin
       WorkBuffer:=SystemTimeToString(Now) + ' - ' + WorkBuffer;
      end;
    end; 

   {Remove CRLF}
   if WorkBuffer[Length(WorkBuffer)] = Chr(10) then SetLength(WorkBuffer,Length(WorkBuffer) - 1);
   if WorkBuffer[Length(WorkBuffer)] = Chr(13) then SetLength(WorkBuffer,Length(WorkBuffer) - 1);
   
   {Create Logging Entry}
   LoggingEntry:=PLoggingEntry(@Message); {Do not free}
   LoggingEntry.Data:=WorkBuffer;
   LoggingEntry.Reserved2:=LongInt(LOGGING_SEVERITY_INVALID);   {Severity}
   LoggingEntry.Reserved3:=LOGGING_FACILITY_INVALID;            {Facility}
   
   {Submit Logging Entry}
   if MessageslotSend(LoggingMessageslot,Message) <> ERROR_SUCCESS then
    begin
     SetLength(LoggingEntry.Data,0);
    end;
 {$IF not(DEFINED(INTERRUPT_DEBUG)) and not(DEFINED(HEAP_DEBUG)) and not(DEFINED(THREAD_DEBUG)) and not(DEFINED(PLATFORM_DEBUG))}  
  end;
 {$ENDIF}  
end;

{==============================================================================}

procedure SysLoggingOutputEx(AFacility,ASeverity:LongWord;const ATag,AContent:String);
var
 Message:TMessage;
 WorkBuffer:String;
 LoggingEntryEx:PLoggingEntryEx;
begin
 {}
 {$IF not(DEFINED(INTERRUPT_DEBUG)) and not(DEFINED(HEAP_DEBUG)) and not(DEFINED(THREAD_DEBUG)) and not(DEFINED(PLATFORM_DEBUG))}
 if LOGGING_DIRECT_ENABLE then
  begin
   if MutexLock(LoggingDirectLock) <> ERROR_SUCCESS then Exit;
   
   {Setup Logging}
   if CPUGetCount > 1 then
    begin
     WorkBuffer:=IntToHex(ThreadGetCurrent,8) + ':' + IntToHex(CPUGetCurrent,1) + ' - ' + AContent;
    end
   else
    begin
     WorkBuffer:=IntToHex(ThreadGetCurrent,8) + ' - ' + AContent;
    end;
   
   {Check Options}
   if LOGGING_INCLUDE_TICKCOUNT then
    begin
     WorkBuffer:=IntToHex(GetTickCount64,16) + ' - ' + WorkBuffer;
    end; 
   if LOGGING_INCLUDE_DATETIME or (LOGGING_INCLUDE_DATE and LOGGING_INCLUDE_TIME) then
    begin
     WorkBuffer:=SystemDateTimeToString(Now) + ' - ' + WorkBuffer;
    end
   else
    begin
     if LOGGING_INCLUDE_DATE then
      begin
       WorkBuffer:=SystemDateToString(Now) + ' - ' + WorkBuffer;
      end;
     if LOGGING_INCLUDE_TIME then
      begin
       WorkBuffer:=SystemTimeToString(Now) + ' - ' + WorkBuffer;
      end;
    end; 
   if LOGGING_INCLUDE_COUNTER then
    begin
     WorkBuffer:=IntToHex(LoggingOutputCount,8) + ' - ' + WorkBuffer;
    end; 

   {Remove CRLF}
   if WorkBuffer[Length(WorkBuffer)] = Chr(10) then SetLength(WorkBuffer,Length(WorkBuffer) - 1);
   if WorkBuffer[Length(WorkBuffer)] = Chr(13) then SetLength(WorkBuffer,Length(WorkBuffer) - 1);
   
   {Output Logging}
   LoggingDeviceOutputEx(LoggingDeviceDefault,AFacility,ASeverity,ATag,WorkBuffer);
   
   {Update Logging Count}
   Inc(LoggingOutputCount);
   
   MutexUnlock(LoggingDirectLock);
  end
 else 
  begin
 {$ENDIF} 
   {Initialize Message}
   FillChar(Message,SizeOf(TMessage),0);
   
   {Setup Logging}
   if CPUGetCount > 1 then
    begin
     WorkBuffer:=IntToHex(ThreadGetCurrent,8) + ':' + IntToHex(CPUGetCurrent,1) + ' - ' + AContent;
    end
   else
    begin
     WorkBuffer:=IntToHex(ThreadGetCurrent,8) + ' - ' + AContent;
    end;
    
   {Check Options}
   if LOGGING_INCLUDE_TICKCOUNT then
    begin
     WorkBuffer:=IntToHex(GetTickCount64,16) + ' - ' + WorkBuffer;
    end; 
   if LOGGING_INCLUDE_DATETIME or (LOGGING_INCLUDE_DATE and LOGGING_INCLUDE_TIME) then
    begin
     WorkBuffer:=SystemDateTimeToString(Now) + ' - ' + WorkBuffer;
    end
   else
    begin
     if LOGGING_INCLUDE_DATE then
      begin
       WorkBuffer:=SystemDateToString(Now) + ' - ' + WorkBuffer;
      end;
     if LOGGING_INCLUDE_TIME then
      begin
       WorkBuffer:=SystemTimeToString(Now) + ' - ' + WorkBuffer;
      end;
    end; 

   {Remove CRLF}
   if WorkBuffer[Length(WorkBuffer)] = Chr(10) then SetLength(WorkBuffer,Length(WorkBuffer) - 1);
   if WorkBuffer[Length(WorkBuffer)] = Chr(13) then SetLength(WorkBuffer,Length(WorkBuffer) - 1);
   
   {Create Logging Entry}
   LoggingEntryEx:=PLoggingEntryEx(@Message); {Do not free}
   LoggingEntryEx.Content:=WorkBuffer;
   LoggingEntryEx.Tag:=ATag;
   LoggingEntryEx.Severity:=ASeverity;
   LoggingEntryEx.Facility:=AFacility;
   
   {Submit Logging Entry}
   if MessageslotSend(LoggingMessageslot,Message) <> ERROR_SUCCESS then
    begin
     SetLength(LoggingEntryEx.Tag,0);
     SetLength(LoggingEntryEx.Content,0);
    end;
 {$IF not(DEFINED(INTERRUPT_DEBUG)) and not(DEFINED(HEAP_DEBUG)) and not(DEFINED(THREAD_DEBUG)) and not(DEFINED(PLATFORM_DEBUG))}  
  end;
 {$ENDIF}  
end;

{==============================================================================}
{==============================================================================}
{Logging Helper Functions}
function LoggingDeviceGetCount:LongWord;
{Get the current logging device count}
begin
 {}
 Result:=LoggingDeviceTableCount;
end;

{==============================================================================}

function LoggingDeviceGetDefault:PLoggingDevice;
{Get the current default logging device}
begin
 {}
 Result:=LoggingDeviceDefault;
end;

{==============================================================================}

function LoggingDeviceSetDefault(Logging:PLoggingDevice):LongWord; 
{Set the current default logging device}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Logging}
 if Logging = nil then Exit;
 if Logging.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(LoggingDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Check Logging}
    if LoggingDeviceCheck(Logging) <> Logging then Exit;
    
    {Set Logging Default}
    LoggingDeviceDefault:=Logging;
    
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    CriticalSectionUnlock(LoggingDeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function LoggingDeviceCheck(Logging:PLoggingDevice):PLoggingDevice;
{Check if the supplied Logging device is in the Logging table}
var
 Current:PLoggingDevice;
begin
 {}
 Result:=nil;
 
 {Check Logging}
 if Logging = nil then Exit;
 if Logging.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(LoggingDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Logging}
    Current:=LoggingDeviceTable;
    while Current <> nil do
     begin
      {Check Logging}
      if Current = Logging then
       begin
        Result:=Logging;
        Exit;
       end;
      
      {Get Next}
      Current:=Current.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(LoggingDeviceTableLock);
   end;
  end;
end;

{==============================================================================}

function LoggingTypeToString(LoggingType:LongWord):String;
{Convert a Logging type value to a string}
begin
 {}
 Result:='LOGGING_TYPE_UNKNOWN';
 
 if LoggingType <= LOGGING_TYPE_MAX then
  begin
   Result:=LOGGING_TYPE_NAMES[LoggingType];
  end;
end;

{==============================================================================}

function LoggingStateToString(LoggingState:LongWord):String;
{Convert a Logging state value to a string}
begin
 {}
 Result:='LOGGING_STATE_UNKNOWN';
 
 if LoggingState <= LOGGING_STATE_MAX then
  begin
   Result:=LOGGING_STATE_NAMES[LoggingState];
  end;
end;

{==============================================================================}

function LoggingDeviceRedirectOutput(Logging:PLoggingDevice):Boolean; 
{Redirect standard output to the logging device specified by Logging}
{Logging: The logging device to redirect output to (or nil to stop redirection)}
{Return: True if completed successfully or False if an error occurred}

{Note: Redirects the output of the text files Output, ErrOutput, StdOut and StdErr
       which also redirects the output of Write, WriteLn and the standard C library}
begin
 {}
 Result:=False;
 
 if Logging = nil then
  begin
   {Stop Redirection}
   TextIOWriteCharHandler:=nil;
   TextIOWriteBufferHandler:=nil;
   
   LoggingTextIOOutputDevice:=nil;
  end
 else
  begin
   {Check Logging}
   if Logging.Device.Signature <> DEVICE_SIGNATURE then Exit;
   
   {Start Redirection}
   TextIOWriteCharHandler:=SysTextIOWriteChar;
   TextIOWriteBufferHandler:=SysTextIOWriteBuffer;
  
   LoggingTextIOOutputDevice:=Logging;
  end;  
  
 Result:=True;
end;

{==============================================================================}

function LoggingGetMessageslotFlags:LongWord;
{Get the lock flags for the logging messageslot}
begin
 {}
 {$IFDEF LOGGING_LOCK_IRQFIQ}
 if FIQ_ENABLED then Result:=MESSAGESLOT_FLAG_IRQFIQ else Result:=MESSAGESLOT_FLAG_IRQ;
 {$ELSE}
  {$IFDEF LOGGING_LOCK_FIQ}
  if FIQ_ENABLED then Result:=MESSAGESLOT_FLAG_FIQ else Result:=MESSAGESLOT_FLAG_IRQ;
  {$ELSE}
   {$IFDEF LOGGING_LOCK_IRQ}
   Result:=MESSAGESLOT_FLAG_IRQ;
   {$ELSE}
   Result:=MESSAGESLOT_FLAG_NONE;
   {$ENDIF}
  {$ENDIF}
 {$ENDIF}
end;

{==============================================================================}

function LoggingConsoleDeviceAdd(Console:PConsoleDevice):LongWord;
var
 Status:LongWord;
 Logging:PConsoleLogging;
begin
 {}
 Result:=ERROR_SUCCESS;
 
 {Check Logging}
 if LoggingDeviceFindByDevice(@Console.Device) = nil then
  begin
   {Check Register}
   if CONSOLE_REGISTER_LOGGING then
    begin
     {Check Device}
     if Length(CONSOLE_LOGGING_DEVICE) <> 0 then
      begin
       {Check Name}
       if ConsoleDeviceFindByName(CONSOLE_LOGGING_DEVICE) <> Console then
        begin
         {Check Description}
         if ConsoleDeviceFindByDescription(CONSOLE_LOGGING_DEVICE) <> Console then Exit;
        end; 
      end
     else
      begin
       {Check Default}
       if ConsoleDeviceGetDefault <> Console then Exit;
      end;    
    
     {Create Logging}
     Logging:=PConsoleLogging(LoggingDeviceCreateEx(SizeOf(TConsoleLogging),CONSOLE_LOGGING_DEFAULT));
     if Logging <> nil then
      begin
       {Update Logging}
       {Device}
       Logging.Logging.Device.DeviceBus:=DEVICE_BUS_NONE; 
       Logging.Logging.Device.DeviceType:=LOGGING_TYPE_CONSOLE;
       Logging.Logging.Device.DeviceFlags:=LOGGING_FLAG_NONE;
       Logging.Logging.Device.DeviceData:=@Console.Device;
       Logging.Logging.Device.DeviceDescription:=CONSOLE_LOGGING_DESCRIPTION;
       {Logging}
       Logging.Logging.LoggingState:=LOGGING_STATE_DISABLED;
       Logging.Logging.DeviceStart:=ConsoleLoggingStart;
       Logging.Logging.DeviceStop:=ConsoleLoggingStop;
       Logging.Logging.DeviceOutput:=ConsoleLoggingOutput;
       Logging.Logging.DeviceSetTarget:=ConsoleLoggingSetTarget;
       Logging.Logging.Target:=DeviceGetName(@Console.Device);
       {Console}
       Logging.Console:=Console;
       Logging.Window:=INVALID_HANDLE_VALUE;
       
       {Register Logging}
       Status:=LoggingDeviceRegister(@Logging.Logging);
       if Status = ERROR_SUCCESS then
        begin
         {Start Logging}
         Status:=LoggingDeviceStart(@Logging.Logging);
         if Status <> ERROR_SUCCESS then
          begin
           if DEVICE_LOG_ENABLED then DeviceLogError(nil,'Logging: Failed to start new console logging device: ' + ErrorToString(Status));

           {Deregister Logging}
           LoggingDeviceDeregister(@Logging.Logging);

           {Destroy Logging}
           LoggingDeviceDestroy(@Logging.Logging);
          end;
        end
       else 
        begin
         if DEVICE_LOG_ENABLED then DeviceLogError(nil,'Logging: Failed to register new console logging device: ' + ErrorToString(Status));

         {Destroy Logging}
         LoggingDeviceDestroy(@Logging.Logging);
        end;
      end
     else 
      begin
       if DEVICE_LOG_ENABLED then DeviceLogError(nil,'Logging: Failed to create new console logging device');
      end;
    end;
  end; 
end;

{==============================================================================}

function LoggingConsoleDeviceRemove(Console:PConsoleDevice):LongWord;
var
 Status:LongWord;
 Logging:PConsoleLogging;
begin
 {}
 Result:=ERROR_SUCCESS;
 
 {Check Logging}
 Logging:=PConsoleLogging(LoggingDeviceFindByDevice(@Console.Device));
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
         if DEVICE_LOG_ENABLED then DeviceLogError(nil,'Logging: Failed to destroy console logging device');
        end; 
      end
     else
      begin
       if DEVICE_LOG_ENABLED then DeviceLogError(nil,'Logging: Failed to deregister console logging device: ' + ErrorToString(Status));
      end;      
    end
   else
    begin   
     if DEVICE_LOG_ENABLED then DeviceLogError(nil,'Logging: Failed to stop console logging device: ' + ErrorToString(Status));
    end;
  end;
end;
  
{==============================================================================}

function LoggingConsoleDeviceEnum(Console:PConsoleDevice;Data:Pointer):LongWord;
begin
 {}
 Result:=ERROR_SUCCESS;
 
 {$IF DEFINED(LOGGING_DEBUG) or DEFINED(DEVICE_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Logging: Console device enumeration');
 {$ENDIF}
 
 {Check Console}
 if Console = nil then Exit;
 if Console.ConsoleState <> CONSOLE_STATE_OPEN then Exit; 
 
 {Add Console}
 Result:=LoggingConsoleDeviceAdd(Console);
end;

{==============================================================================}

function LoggingConsoleDeviceNotify(Device:PDevice;Data:Pointer;Notification:LongWord):LongWord;
var
 Console:PConsoleDevice;
begin
 {}
 Result:=ERROR_SUCCESS;
 
 {$IF DEFINED(LOGGING_DEBUG) or DEFINED(DEVICE_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Logging: Console device notification (Notification=' + NotificationToString(Notification) + ')');
 {$ENDIF}
 
 {Check Device}
 if Device = nil then Exit;
 
 {Get Console}
 Console:=PConsoleDevice(Device);

 {Check Notification}
 if (Notification and DEVICE_NOTIFICATION_REGISTER) <> 0 then
  begin
   {Check Console}
   if Console.ConsoleState <> CONSOLE_STATE_OPEN then Exit; 
   
   {Add Console}
   Result:=LoggingConsoleDeviceAdd(Console);
  end
 else if (Notification and DEVICE_NOTIFICATION_OPEN) <> 0 then
  begin
   {Check Console}
   if Console.ConsoleState <> CONSOLE_STATE_OPEN then Exit; 

   {Add Console}
   Result:=LoggingConsoleDeviceAdd(Console);
  end
 else if (Notification and DEVICE_NOTIFICATION_DEREGISTER) <> 0 then
  begin
   {Remove Console}
   Result:=LoggingConsoleDeviceRemove(Console);
  end
 else if (Notification and DEVICE_NOTIFICATION_CLOSE) <> 0 then
  begin
   {Remove Console}
   Result:=LoggingConsoleDeviceRemove(Console);
  end;
end;
 
{==============================================================================}
{==============================================================================}

initialization
 LoggingInit;
 
{==============================================================================}
 
finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
