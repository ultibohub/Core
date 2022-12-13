{
Ultibo 1-Wire interface unit.

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

 1-Wire - https://en.wikipedia.org/wiki/1-Wire

1-Wire Hosts
============

 1-Wire is a communications bus designed by Dallas Semiconductor Corp that combines low speed signalling,
 data and power over a single signal line. It is often used for small devices such as digital thermometers.
 
 Devices can actually be powered over the 1-Wire interface allowing the use of just data and ground lines.

 Note: Because FreePascal does not permit the use of functions or variables that begin with a numeric value
 such as 1WIRE we use the term W1 throughout this unit to refer to a 1-Wire device. This is the same terminology
 as used in Linux and other systems.
  
}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit W1; 

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,SysUtils;

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
const
 {W1 specific constants}
 W1_NAME_PREFIX = '1WIRE';  {Name prefix for 1-Wire Devices}
 
 {W1 Device Types}
 W1_TYPE_NONE      = 0;
 
 {W1 Device States}
 W1_STATE_DISABLED = 0;
 W1_STATE_ENABLED  = 1;

 {W1 Device Flags}
 W1_FLAG_NONE          = $00000000;

 {W1 logging}
 W1_LOG_LEVEL_DEBUG     = LOG_LEVEL_DEBUG;  {W1 debugging messages}
 W1_LOG_LEVEL_INFO      = LOG_LEVEL_INFO;   {W1 informational messages, such as a device being attached or detached}
 W1_LOG_LEVEL_WARN      = LOG_LEVEL_WARN;   {W1 warning messages}
 W1_LOG_LEVEL_ERROR     = LOG_LEVEL_ERROR;  {W1 error messages}
 W1_LOG_LEVEL_NONE      = LOG_LEVEL_NONE;   {No W1 messages}

var 
 W1_DEFAULT_LOG_LEVEL:LongWord = W1_LOG_LEVEL_DEBUG; {Minimum level for W1 messages.  Only messages with level greater than or equal to this will be printed}
 
var 
 {W1 logging}
 W1_LOG_ENABLED:Boolean; 
 
{==============================================================================}
type
 {W1 specific types}
 
 {W1 Properties}
 PW1Properties = ^TW1Properties;
 TW1Properties = record
  Flags:LongWord;        {Device flags (eg W1_FLAG_????)}
  //To Do 
 end;
 
 {W1 Device}
 PW1Device = ^TW1Device;
 
 {W1 Enumeration Callback}
 TW1Enumerate = function(W1:PW1Device;Data:Pointer):LongWord;
 {W1 Notification Callback}
 TW1Notification = function(Device:PDevice;Data:Pointer;Notification:LongWord):LongWord;
 
 {W1 Device Methods}
 //To Do 
 
 TW1DeviceGetProperties = function(W1:PW1Device;Properties:PW1Properties):LongWord;
 
 TW1Device = record
  {Device Properties}
  Device:TDevice;                                 {The Device entry for this W1}
  {W1 Properties}
  W1Id:LongWord;                                  {Unique Id of this W1 in the W1 table}
  W1State:LongWord;                               {W1 state (eg W1_STATE_ENABLED)}
  //To Do 
  DeviceGetProperties:TW1DeviceGetProperties;     {A Device specific DeviceGetProperties method implementing the standard W1 device interface (Or nil if the default method is suitable)}
  {Statistics Properties}
  //To Do 
  {Driver Properties}
  Lock:TMutexHandle;                              {Device lock}
  //To Do 
  Properties:TW1Properties;                       {Device properties}
  {Internal Properties}                                                                        
  Prev:PW1Device;                                 {Previous entry in W1 table}
  Next:PW1Device;                                 {Next entry in W1 table}
 end; 
 
{==============================================================================}
{var}
 {W1 specific variables}

{==============================================================================}
{Initialization Functions}
procedure W1Init;
 
{==============================================================================}
{W1 Functions}
//To Do
  
function W1DeviceGetProperties(W1:PW1Device;Properties:PW1Properties):LongWord;
  
function W1DeviceCreate:PW1Device;
function W1DeviceCreateEx(Size:LongWord):PW1Device;
function W1DeviceDestroy(W1:PW1Device):LongWord;

function W1DeviceRegister(W1:PW1Device):LongWord;
function W1DeviceDeregister(W1:PW1Device):LongWord;

function W1DeviceFind(W1Id:LongWord):PW1Device;
function W1DeviceFindByName(const Name:String):PW1Device; inline;
function W1DeviceFindByDescription(const Description:String):PW1Device; inline;
function W1DeviceEnumerate(Callback:TW1Enumerate;Data:Pointer):LongWord;
 
function W1DeviceNotification(W1:PW1Device;Callback:TW1Notification;Data:Pointer;Notification,Flags:LongWord):LongWord;
 
{==============================================================================}
{RTL W1 Functions}
  
{==============================================================================}
{W1 Helper Functions}
function W1GetCount:LongWord;
function W1DeviceGetDefault:PW1Device;
function W1DeviceSetDefault(W1:PW1Device):LongWord; 

function W1DeviceCheck(W1:PW1Device):PW1Device;

procedure W1Log(Level:LongWord;W1:PW1Device;const AText:String);
procedure W1LogInfo(W1:PW1Device;const AText:String); inline;
procedure W1LogWarn(W1:PW1Device;const AText:String); inline;
procedure W1LogError(W1:PW1Device;const AText:String); inline;
procedure W1LogDebug(W1:PW1Device;const AText:String); inline;
 
{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {W1 specific variables}
 W1Initialized:Boolean;

 W1DeviceTable:PW1Device;
 W1DeviceTableLock:TCriticalSectionHandle = INVALID_HANDLE_VALUE;
 W1DeviceTableCount:LongWord;

 W1DeviceDefault:PW1Device;

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure W1Init;
{Initialize the W1 unit and W1 device table}

{Note: Called only during system startup}
begin
 {}
 {Check Initialized}
 if W1Initialized then Exit;
 
 {Initialize Logging}
 W1_LOG_ENABLED:=(W1_DEFAULT_LOG_LEVEL <> W1_LOG_LEVEL_NONE); 
 
 {Initialize W1 Table}
 W1DeviceTable:=nil;
 W1DeviceTableLock:=CriticalSectionCreate; 
 W1DeviceTableCount:=0;
 if W1DeviceTableLock = INVALID_HANDLE_VALUE then
  begin
   if W1_LOG_ENABLED then W1LogError(nil,'Failed to create W1 table lock');
  end;
 W1DeviceDefault:=nil;
 
 {Register Platform W1 Handlers}
 //To do
 
 
 W1Initialized:=True;
end;
 
{==============================================================================}
{==============================================================================}
{W1 Functions}

{==============================================================================}
 
function W1DeviceGetProperties(W1:PW1Device;Properties:PW1Properties):LongWord;
{Get the properties for the specified W1 device}
{W1: The W1 device to get properties from}
{Properties: Pointer to a TW1Properties structure to fill in}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Properties}
 if Properties = nil then Exit;
 
 {Check W1}
 if W1 = nil then Exit;
 if W1.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IFDEF W1_DEBUG}
 if W1_LOG_ENABLED then W1LogDebug(W1,'W1 Device Get Properties');
 {$ENDIF}
 
 {Check Enabled}
 {Result:=ERROR_NOT_SUPPORTED;}
 {if W1.W1State <> W1_STATE_ENABLED then Exit;} {Allow when disabled}
 
 if MutexLock(W1.Lock) = ERROR_SUCCESS then
  begin
   if Assigned(W1.DeviceGetProperties) then
    begin
     {Call Device Get Properites}
     Result:=W1.DeviceGetProperties(W1,Properties);
    end
   else
    begin
     {Get Properties}
     System.Move(W1.Properties,Properties^,SizeOf(TW1Properties));
       
     {Return Result}
     Result:=ERROR_SUCCESS;
    end;  
    
   MutexUnlock(W1.Lock);
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;    
end;

{==============================================================================}

function W1DeviceCreate:PW1Device;
{Create a new W1 entry}
{Return: Pointer to new W1 entry or nil if W1 could not be created}
begin
 {}
 Result:=W1DeviceCreateEx(SizeOf(TW1Device));
end;

{==============================================================================}

function W1DeviceCreateEx(Size:LongWord):PW1Device;
{Create a new W1 entry}
{Size: Size in bytes to allocate for new W1 (Including the W1 entry)}
{Return: Pointer to new W1 entry or nil if W1 could not be created}
begin
 {}
 Result:=nil;
 
 {Check Size}
 if Size < SizeOf(TW1Device) then Exit;
 
 {Create W1}
 Result:=PW1Device(DeviceCreateEx(Size));
 if Result = nil then Exit;
 
 {Update Device}
 Result.Device.DeviceBus:=DEVICE_BUS_NONE;   
 Result.Device.DeviceType:=W1_TYPE_NONE;
 Result.Device.DeviceFlags:=W1_FLAG_NONE;
 Result.Device.DeviceData:=nil;

 {Update W1}
 Result.W1Id:=DEVICE_ID_ANY;
 Result.W1State:=W1_STATE_DISABLED;
 //To Do
 Result.DeviceGetProperties:=nil;
 //To Do
 Result.Lock:=INVALID_HANDLE_VALUE;
 
 {Create Lock}
 Result.Lock:=MutexCreateEx(False,MUTEX_DEFAULT_SPINCOUNT,MUTEX_FLAG_RECURSIVE);
 if Result.Lock = INVALID_HANDLE_VALUE then
  begin
   if W1_LOG_ENABLED then W1LogError(nil,'Failed to create lock for W1 device');
   W1DeviceDestroy(Result);
   Result:=nil;
   Exit;
  end;
end;

{==============================================================================}

function W1DeviceDestroy(W1:PW1Device):LongWord;
{Destroy an existing W1 entry}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check W1}
 if W1 = nil then Exit;
 if W1.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check W1}
 Result:=ERROR_IN_USE;
 if W1DeviceCheck(W1) = W1 then Exit;

 {Check State}
 if W1.Device.DeviceState <> DEVICE_STATE_UNREGISTERED then Exit;
 
 {Destroy Lock}
 if W1.Lock <> INVALID_HANDLE_VALUE then
  begin
   MutexDestroy(W1.Lock);
  end;
 
 {Destroy W1} 
 Result:=DeviceDestroy(@W1.Device);
end;

{==============================================================================}

function W1DeviceRegister(W1:PW1Device):LongWord;
{Register a new W1 in the W1 table}
var
 W1Id:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check W1}
 if W1 = nil then Exit;
 if W1.W1Id <> DEVICE_ID_ANY then Exit;
 if W1.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check Interfaces}
 //To Do
 
 {Check W1}
 Result:=ERROR_ALREADY_EXISTS;
 if W1DeviceCheck(W1) = W1 then Exit;
 
 {Check State}
 if W1.Device.DeviceState <> DEVICE_STATE_UNREGISTERED then Exit;
 
 {Insert W1}
 if CriticalSectionLock(W1DeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Update W1}
    W1Id:=0;
    while W1DeviceFind(W1Id) <> nil do
     begin
      Inc(W1Id);
     end;
    W1.W1Id:=W1Id;
    
    {Update Device}
    W1.Device.DeviceName:=W1_NAME_PREFIX + IntToStr(W1.W1Id); 
    W1.Device.DeviceClass:=DEVICE_CLASS_1WIRE;
    
    {Register Device}
    Result:=DeviceRegister(@W1.Device);
    if Result <> ERROR_SUCCESS then
     begin
      W1.W1Id:=DEVICE_ID_ANY;
      Exit;
     end; 
    
    {Link W1}
    if W1DeviceTable = nil then
     begin
      W1DeviceTable:=W1;
     end
    else
     begin
      W1.Next:=W1DeviceTable;
      W1DeviceTable.Prev:=W1;
      W1DeviceTable:=W1;
     end;
 
    {Increment Count}
    Inc(W1DeviceTableCount);
    
    {Check Default}
    if W1DeviceDefault = nil then
     begin
      W1DeviceDefault:=W1;
     end;
    
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    CriticalSectionUnlock(W1DeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;

{==============================================================================}

function W1DeviceDeregister(W1:PW1Device):LongWord;
{Deregister a W1 from the W1 table}
var
 Prev:PW1Device;
 Next:PW1Device;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check W1}
 if W1 = nil then Exit;
 if W1.W1Id = DEVICE_ID_ANY then Exit;
 if W1.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check W1}
 Result:=ERROR_NOT_FOUND;
 if W1DeviceCheck(W1) <> W1 then Exit;
 
 {Check State}
 if W1.Device.DeviceState <> DEVICE_STATE_REGISTERED then Exit;
 
 {Remove W1}
 if CriticalSectionLock(W1DeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Deregister Device}
    Result:=DeviceDeregister(@W1.Device);
    if Result <> ERROR_SUCCESS then Exit;
    
    {Unlink W1}
    Prev:=W1.Prev;
    Next:=W1.Next;
    if Prev = nil then
     begin
      W1DeviceTable:=Next;
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
    Dec(W1DeviceTableCount);
 
    {Check Default}
    if W1DeviceDefault = W1 then
     begin
      W1DeviceDefault:=W1DeviceTable;
     end;
 
    {Update W1}
    W1.W1Id:=DEVICE_ID_ANY;
 
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    CriticalSectionUnlock(W1DeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;

{==============================================================================}

function W1DeviceFind(W1Id:LongWord):PW1Device;
var
 W1:PW1Device;
begin
 {}
 Result:=nil;
 
 {Check Id}
 if W1Id = DEVICE_ID_ANY then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(W1DeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get W1}
    W1:=W1DeviceTable;
    while W1 <> nil do
     begin
      {Check State}
      if W1.Device.DeviceState = DEVICE_STATE_REGISTERED then
       begin
        {Check Id}
        if W1.W1Id = W1Id then
         begin
          Result:=W1;
          Exit;
         end;
       end;
       
      {Get Next}
      W1:=W1.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(W1DeviceTableLock);
   end;
  end;
end;

{==============================================================================}

function W1DeviceFindByName(const Name:String):PW1Device; inline;
begin
 {}
 Result:=PW1Device(DeviceFindByName(Name));
end;

{==============================================================================}

function W1DeviceFindByDescription(const Description:String):PW1Device; inline;
begin
 {}
 Result:=PW1Device(DeviceFindByDescription(Description));
end;
       
{==============================================================================}

function W1DeviceEnumerate(Callback:TW1Enumerate;Data:Pointer):LongWord;
var
 W1:PW1Device;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Callback}
 if not Assigned(Callback) then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(W1DeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get W1}
    W1:=W1DeviceTable;
    while W1 <> nil do
     begin
      {Check State}
      if W1.Device.DeviceState = DEVICE_STATE_REGISTERED then
       begin
        if Callback(W1,Data) <> ERROR_SUCCESS then Exit;
       end;
       
      {Get Next}
      W1:=W1.Next;
     end;
     
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    CriticalSectionUnlock(W1DeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;

{==============================================================================}

function W1DeviceNotification(W1:PW1Device;Callback:TW1Notification;Data:Pointer;Notification,Flags:LongWord):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check W1}
 if W1 = nil then
  begin
   Result:=DeviceNotification(nil,DEVICE_CLASS_1WIRE,Callback,Data,Notification,Flags);
  end
 else
  begin 
   {Check W1}
   if W1.Device.Signature <> DEVICE_SIGNATURE then Exit;

   Result:=DeviceNotification(@W1.Device,DEVICE_CLASS_1WIRE,Callback,Data,Notification,Flags);
  end; 
end;

{==============================================================================}
{==============================================================================}
{RTL W1 Functions}

{==============================================================================}
{==============================================================================}
{W1 Helper Functions}
function W1GetCount:LongWord;
{Get the current W1 count}
begin
 {}
 Result:=W1DeviceTableCount;
end;

{==============================================================================}

function W1DeviceGetDefault:PW1Device;
{Get the current default W1 device}
begin
 {}
 Result:=W1DeviceDefault;
end;

{==============================================================================}

function W1DeviceSetDefault(W1:PW1Device):LongWord; 
{Set the current default W1 device}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check W1}
 if W1 = nil then Exit;
 if W1.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(W1DeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Check W1}
    if W1DeviceCheck(W1) <> W1 then Exit;
    
    {Set W1 Default}
    W1DeviceDefault:=W1;
    
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    CriticalSectionUnlock(W1DeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function W1DeviceCheck(W1:PW1Device):PW1Device;
{Check if the supplied W1 is in the W1 table}
var
 Current:PW1Device;
begin
 {}
 Result:=nil;
 
 {Check W1}
 if W1 = nil then Exit;
 if W1.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(W1DeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get W1}
    Current:=W1DeviceTable;
    while Current <> nil do
     begin
      {Check W1}
      if Current = W1 then
       begin
        Result:=W1;
        Exit;
       end;
      
      {Get Next}
      Current:=Current.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(W1DeviceTableLock);
   end;
  end;
end;

{==============================================================================}

procedure W1Log(Level:LongWord;W1:PW1Device;const AText:String);
var
 WorkBuffer:String;
begin
 {}
 {Check Level}
 if Level < W1_DEFAULT_LOG_LEVEL then Exit;
 
 WorkBuffer:='';
 {Check Level}
 if Level = W1_LOG_LEVEL_DEBUG then
  begin
   WorkBuffer:=WorkBuffer + '[DEBUG] ';
  end
 else if Level = W1_LOG_LEVEL_WARN then
  begin
   WorkBuffer:=WorkBuffer + '[WARN] ';
  end
 else if Level = W1_LOG_LEVEL_ERROR then
  begin
   WorkBuffer:=WorkBuffer + '[ERROR] ';
  end;
 
 {Add Prefix}
 WorkBuffer:=WorkBuffer + '1WIRE: ';
 
 {Check W1}
 if W1 <> nil then
  begin
   WorkBuffer:=WorkBuffer + W1_NAME_PREFIX + IntToStr(W1.W1Id) + ': ';
  end;

 {Output Logging}  
 LoggingOutputEx(LOGGING_FACILITY_1WIRE,LogLevelToLoggingSeverity(Level),'1WIRE',WorkBuffer + AText);
end;

{==============================================================================}

procedure W1LogInfo(W1:PW1Device;const AText:String); inline;
begin
 {}
 W1Log(W1_LOG_LEVEL_INFO,W1,AText);
end;

{==============================================================================}

procedure W1LogWarn(W1:PW1Device;const AText:String); inline;
begin
 {}
 W1Log(W1_LOG_LEVEL_WARN,W1,AText);
end;

{==============================================================================}

procedure W1LogError(W1:PW1Device;const AText:String); inline;
begin
 {}
 W1Log(W1_LOG_LEVEL_ERROR,W1,AText);
end;

{==============================================================================}

procedure W1LogDebug(W1:PW1Device;const AText:String); inline;
begin
 {}
 W1Log(W1_LOG_LEVEL_DEBUG,W1,AText);
end;

{==============================================================================}
{==============================================================================}

initialization
 W1Init;

{==============================================================================}
 
finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
