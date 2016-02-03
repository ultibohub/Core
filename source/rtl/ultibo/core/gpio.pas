{
Ultibo GPIO interface unit.

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


GPIO Devices
============

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit GPIO; 

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,SysUtils;

//See: \u-boot-HEAD-5745f8c\drivers\gpio\bcm2835_gpio.c

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
const
 {GPIO specific constants}
 GPIO_NAME_PREFIX = 'GPIO';  {Name prefix for GPIO Devices}
 
 {GPIO Device Types}
 GPIO_TYPE_NONE      = 0;
 
 {GPIO Device States}
 GPIO_STATE_DISABLED = 0;
 GPIO_STATE_ENABLED  = 1;
 
 {GPIO Device Flags}
 GPIO_FLAG_NONE          = $00000000;
 
 {GPIO logging}
 GPIO_LOG_LEVEL_DEBUG     = LOG_LEVEL_DEBUG;  {GPIO debugging messages}
 GPIO_LOG_LEVEL_INFO      = LOG_LEVEL_INFO;  {GPIO informational messages, such as a device being attached or detached}
 GPIO_LOG_LEVEL_ERROR     = LOG_LEVEL_ERROR;  {GPIO error messages}
 GPIO_LOG_LEVEL_NONE      = LOG_LEVEL_NONE;  {No GPIO messages}

var 
 GPIO_DEFAULT_LOG_LEVEL:LongWord = GPIO_LOG_LEVEL_INFO; {Minimum level for GPIO messages.  Only messages with level greater than or equal to this will be printed}
 
var 
 {GPIO logging}
 GPIO_LOG_ENABLED:Boolean; 
 
{==============================================================================}
type
 {GPIO specific types}
 
 {GPIO Properties}
 PGPIOProperties = ^TGPIOProperties;
 TGPIOProperties = record
  PinCount:LongWord;
  //To Do
 end;
 
 {GPIO Device}
 PGPIODevice = ^TGPIODevice;
 
 {GPIO Enumeration Callback}
 TGPIOEnumerate = function(GPIO:PGPIODevice;Data:Pointer):LongWord;
 {GPIO Notification Callback}
 TGPIONotification = function(Device:PDevice;Data:Pointer;Notification:LongWord):LongWord;
 
 {GPIO Device Methods}
 TGPIODeviceRead = function(GPIO:PGPIODevice;Reg:LongWord):LongWord; 
 TGPIODeviceWrite = procedure(GPIO:PGPIODevice;Reg,Value:LongWord);
 
 TGPIODeviceOutputSet = function(GPIO:PGPIODevice;Pin:LongWord):LongWord;
 TGPIODeviceOutputClear = function(GPIO:PGPIODevice;Pin:LongWord):LongWord;
 TGPIODeviceFunctionSelect = function(GPIO:PGPIODevice;Pin,Mode:LongWord):LongWord;
 
 TGPIODeviceProperties = function(GPIO:PGPIODevice;var Properties:PGPIOProperties):LongWord;
 
 TGPIODevice = record
  {Device Properties}
  Device:TDevice;                                 {The Device entry for this GPIO}
  {GPIO Properties}
  GPIOId:LongWord;                                {Unique Id of this GPIO in the GPIO table}
  GPIOState:LongWord;                             {GPIO state (eg GPIO_STATE_ENABLED)}
  DeviceRead:TGPIODeviceRead;                     {A Device specific DeviceRead method implementing the standard GPIO device interface}
  DeviceWrite:TGPIODeviceWrite;                   {A Device specific DeviceWrite method implementing the standard GPIO device interface}
  DeviceOutputSet:TGPIODeviceOutputSet;           {A Device specific DeviceOutputSet method implementing the standard GPIO device interface}
  DeviceOutputClear:TGPIODeviceOutputClear;       {A Device specific DeviceOutputClear method implementing the standard GPIO device interface}
  DeviceFunctionSelect:TGPIODeviceFunctionSelect; {A Device specific DeviceFunctionSelect method implementing the standard GPIO device interface}
  DeviceProperties:TGPIODeviceProperties;         {A Device specific DeviceProperties method implementing the standard GPIO device interface}
  {Statistics Properties}
  //To Do
  {Driver Properties}
  Lock:TMutexHandle;                              {Device lock}
  Address:Pointer;                                {Device register base address}
  Properties:TGPIOProperties;                     {Device properties}
  {Internal Properties}                                                                        
  Prev:PGPIODevice;                               {Previous entry in GPIO table}
  Next:PGPIODevice;                               {Next entry in GPIO table}
 end; 
  
{==============================================================================}
{var}
 {GPIO specific variables}
 
{==============================================================================}
{Initialization Functions}
procedure GPIOInit;
 
{==============================================================================}
{GPIO Functions}
function GPIODeviceRead(GPIO:PGPIODevice;Reg:LongWord):LongWord; 
procedure GPIODeviceWrite(GPIO:PGPIODevice;Reg,Value:LongWord);
 
function GPIODeviceOutputSet(GPIO:PGPIODevice;Pin:LongWord):LongWord;
function GPIODeviceOutputClear(GPIO:PGPIODevice;Pin:LongWord):LongWord;
function GPIODeviceFunctionSelect(GPIO:PGPIODevice;Pin,Mode:LongWord):LongWord;

function GPIODeviceProperties(GPIO:PGPIODevice;var Properties:PGPIOProperties):LongWord;
 
function GPIODeviceCreate:PGPIODevice;
function GPIODeviceCreateEx(Size:LongWord):PGPIODevice;
function GPIODeviceDestroy(GPIO:PGPIODevice):LongWord;

function GPIODeviceRegister(GPIO:PGPIODevice):LongWord;
function GPIODeviceDeregister(GPIO:PGPIODevice):LongWord;

function GPIODeviceFind(GPIOId:LongWord):PGPIODevice;
function GPIODeviceEnumerate(Callback:TGPIOEnumerate;Data:Pointer):LongWord;
 
function GPIODeviceNotification(GPIO:PGPIODevice;Callback:TGPIONotification;Data:Pointer;Notification,Flags:LongWord):LongWord;

{==============================================================================}
{GPIO Helper Functions}
function GPIOGetCount:LongWord; inline;

function GPIODeviceCheck(GPIO:PGPIODevice):PGPIODevice;

procedure GPIOLog(Level:LongWord;GPIO:PGPIODevice;const AText:String);
procedure GPIOLogInfo(GPIO:PGPIODevice;const AText:String);
procedure GPIOLogError(GPIO:PGPIODevice;const AText:String);
procedure GPIOLogDebug(GPIO:PGPIODevice;const AText:String);

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {GPIO specific variables}
 GPIOInitialized:Boolean;

 GPIODeviceTable:PGPIODevice;
 GPIODeviceTableLock:TCriticalSectionHandle = INVALID_HANDLE_VALUE;
 GPIODeviceTableCount:LongWord;

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure GPIOInit;
begin
 {}
 {Check Initialized}
 if GPIOInitialized then Exit;
 
 {Initialize Logging}
 GPIO_LOG_ENABLED:=(GPIO_DEFAULT_LOG_LEVEL <> GPIO_LOG_LEVEL_NONE); 
 
 {Initialize GPIO Table}
 GPIODeviceTable:=nil;
 GPIODeviceTableLock:=CriticalSectionCreate; 
 GPIODeviceTableCount:=0;
 if GPIODeviceTableLock = INVALID_HANDLE_VALUE then
  begin
   if GPIO_LOG_ENABLED then GPIOLogError(nil,'Failed to create GPIO table lock');
  end;
 
 GPIOInitialized:=True;
end;
 
{==============================================================================}
{==============================================================================}
{GPIO Functions}
function GPIODeviceRead(GPIO:PGPIODevice;Reg:LongWord):LongWord; 
begin
 {}
 Result:=0;
 //To Do
 
end;

{==============================================================================}

procedure GPIODeviceWrite(GPIO:PGPIODevice;Reg,Value:LongWord);
begin
 {}
 //To Do
 
end;

{==============================================================================}
 
function GPIODeviceOutputSet(GPIO:PGPIODevice;Pin:LongWord):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 //To Do
 
end;

{==============================================================================}

function GPIODeviceOutputClear(GPIO:PGPIODevice;Pin:LongWord):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 //To Do
 
end;

{==============================================================================}

function GPIODeviceFunctionSelect(GPIO:PGPIODevice;Pin,Mode:LongWord):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 //To Do
 
end;

{==============================================================================}

function GPIODeviceProperties(GPIO:PGPIODevice;var Properties:PGPIOProperties):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 //To Do
 
end;

{==============================================================================}

function GPIODeviceCreate:PGPIODevice;
{Create a new GPIO entry}
{Return: Pointer to new GPIO entry or nil if GPIO could not be created}
begin
 {}
 Result:=GPIODeviceCreateEx(SizeOf(TGPIODevice));
end;

{==============================================================================}

function GPIODeviceCreateEx(Size:LongWord):PGPIODevice;
{Create a new GPIO entry}
{Size: Size in bytes to allocate for new GPIO (Including the GPIO entry)}
{Return: Pointer to new GPIO entry or nil if GPIO could not be created}
begin
 {}
 Result:=nil;
 
 {Check Size}
 if Size < SizeOf(TGPIODevice) then Exit;
 
 {Create GPIO}
 Result:=PGPIODevice(DeviceCreateEx(Size));
 if Result = nil then Exit;
 
 {Update Device}
 Result.Device.DeviceBus:=DEVICE_BUS_NONE;   
 Result.Device.DeviceType:=GPIO_TYPE_NONE;
 Result.Device.DeviceFlags:=GPIO_FLAG_NONE;
 Result.Device.DeviceData:=nil;

 {Update GPIO}
 Result.GPIOId:=DEVICE_ID_ANY;
 Result.GPIOState:=GPIO_STATE_DISABLED;
 Result.DeviceRead:=nil;
 Result.DeviceWrite:=nil;
 Result.DeviceOutputSet:=nil;
 Result.DeviceOutputClear:=nil;
 Result.DeviceFunctionSelect:=nil;
 Result.DeviceProperties:=nil;
 Result.Lock:=INVALID_HANDLE_VALUE;
 
 {Create Lock}
 Result.Lock:=MutexCreate;
 if Result.Lock = INVALID_HANDLE_VALUE then
  begin
   if GPIO_LOG_ENABLED then GPIOLogError(nil,'Failed to create lock for GPIO device');
   GPIODeviceDestroy(Result);
   Result:=nil;
   Exit;
  end;
end;

{==============================================================================}

function GPIODeviceDestroy(GPIO:PGPIODevice):LongWord;
{Destroy an existing GPIO entry}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check GPIO}
 if GPIO = nil then Exit;
 if GPIO.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check GPIO}
 Result:=ERROR_IN_USE;
 if GPIODeviceCheck(GPIO) = GPIO then Exit;

 {Check State}
 if GPIO.Device.DeviceState <> DEVICE_STATE_UNREGISTERED then Exit;
 
 {Destroy Lock}
 if GPIO.Lock <> INVALID_HANDLE_VALUE then
  begin
   MutexDestroy(GPIO.Lock);
  end;
 
 {Destroy GPIO} 
 Result:=DeviceDestroy(@GPIO.Device);
end;

{==============================================================================}

function GPIODeviceRegister(GPIO:PGPIODevice):LongWord;
{Register a new GPIO in the GPIO table}
var
 GPIOId:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check GPIO}
 if GPIO = nil then Exit;
 if GPIO.GPIOId <> DEVICE_ID_ANY then Exit;
 if GPIO.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check GPIO}
 Result:=ERROR_ALREADY_EXISTS;
 if GPIODeviceCheck(GPIO) = GPIO then Exit;
 
 {Check State}
 if GPIO.Device.DeviceState <> DEVICE_STATE_UNREGISTERED then Exit;
 
 {Insert GPIO}
 if CriticalSectionLock(GPIODeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Update GPIO}
    GPIOId:=0;
    while GPIODeviceFind(GPIOId) <> nil do
     begin
      Inc(GPIOId);
     end;
    GPIO.GPIOId:=GPIOId;
    
    {Update Device}
    GPIO.Device.DeviceName:=GPIO_NAME_PREFIX + IntToStr(GPIO.GPIOId); 
    GPIO.Device.DeviceClass:=DEVICE_CLASS_GPIO;
    
    {Register Device}
    Result:=DeviceRegister(@GPIO.Device);
    if Result <> ERROR_SUCCESS then
     begin
      GPIO.GPIOId:=DEVICE_ID_ANY;
      Exit;
     end; 
    
    {Link GPIO}
    if GPIODeviceTable = nil then
     begin
      GPIODeviceTable:=GPIO;
     end
    else
     begin
      GPIO.Next:=GPIODeviceTable;
      GPIODeviceTable.Prev:=GPIO;
      GPIODeviceTable:=GPIO;
     end;
 
    {Increment Count}
    Inc(GPIODeviceTableCount);
    
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    CriticalSectionUnlock(GPIODeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;

{==============================================================================}

function GPIODeviceDeregister(GPIO:PGPIODevice):LongWord;
{Deregister a GPIO from the GPIO table}
var
 Prev:PGPIODevice;
 Next:PGPIODevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check GPIO}
 if GPIO = nil then Exit;
 if GPIO.GPIOId = DEVICE_ID_ANY then Exit;
 if GPIO.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check GPIO}
 Result:=ERROR_NOT_FOUND;
 if GPIODeviceCheck(GPIO) <> GPIO then Exit;
 
 {Check State}
 if GPIO.Device.DeviceState <> DEVICE_STATE_REGISTERED then Exit;
 
 {Remove GPIO}
 if CriticalSectionLock(GPIODeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Deregister Device}
    Result:=DeviceDeregister(@GPIO.Device);
    if Result <> ERROR_SUCCESS then Exit;
    
    {Unlink GPIO}
    Prev:=GPIO.Prev;
    Next:=GPIO.Next;
    if Prev = nil then
     begin
      GPIODeviceTable:=Next;
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
    Dec(GPIODeviceTableCount);
 
    {Update GPIO}
    GPIO.GPIOId:=DEVICE_ID_ANY;
 
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    CriticalSectionUnlock(GPIODeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;

{==============================================================================}

function GPIODeviceFind(GPIOId:LongWord):PGPIODevice;
var
 GPIO:PGPIODevice;
begin
 {}
 Result:=nil;
 
 {Check Id}
 if GPIOId = DEVICE_ID_ANY then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(GPIODeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get GPIO}
    GPIO:=GPIODeviceTable;
    while GPIO <> nil do
     begin
      {Check State}
      if GPIO.Device.DeviceState = DEVICE_STATE_REGISTERED then
       begin
        {Check Id}
        if GPIO.GPIOId = GPIOId then
         begin
          Result:=GPIO;
          Exit;
         end;
       end;
       
      {Get Next}
      GPIO:=GPIO.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(GPIODeviceTableLock);
   end;
  end;
end;
       
{==============================================================================}

function GPIODeviceEnumerate(Callback:TGPIOEnumerate;Data:Pointer):LongWord;
var
 GPIO:PGPIODevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Callback}
 if not Assigned(Callback) then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(GPIODeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get GPIO}
    GPIO:=GPIODeviceTable;
    while GPIO <> nil do
     begin
      {Check State}
      if GPIO.Device.DeviceState = DEVICE_STATE_REGISTERED then
       begin
        if Callback(GPIO,Data) <> ERROR_SUCCESS then Exit;
       end;
       
      {Get Next}
      GPIO:=GPIO.Next;
     end;
     
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    CriticalSectionUnlock(GPIODeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;

{==============================================================================}

function GPIODeviceNotification(GPIO:PGPIODevice;Callback:TGPIONotification;Data:Pointer;Notification,Flags:LongWord):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check GPIO}
 if GPIO = nil then
  begin
   Result:=DeviceNotification(nil,DEVICE_CLASS_GPIO,Callback,Data,Notification,Flags);
  end
 else
  begin 
   {Check GPIO}
   if GPIO.Device.Signature <> DEVICE_SIGNATURE then Exit;

   Result:=DeviceNotification(@GPIO.Device,DEVICE_CLASS_GPIO,Callback,Data,Notification,Flags);
  end; 
end;

{==============================================================================}
{==============================================================================}
{GPIO Helper Functions}
function GPIOGetCount:LongWord; inline;
{Get the current GPIO count}
begin
 {}
 Result:=GPIODeviceTableCount;
end;

{==============================================================================}

function GPIODeviceCheck(GPIO:PGPIODevice):PGPIODevice;
{Check if the supplied GPIO is in the GPIO table}
var
 Current:PGPIODevice;
begin
 {}
 Result:=nil;
 
 {Check GPIO}
 if GPIO = nil then Exit;
 if GPIO.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(GPIODeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get GPIO}
    Current:=GPIODeviceTable;
    while Current <> nil do
     begin
      {Check GPIO}
      if Current = GPIO then
       begin
        Result:=GPIO;
        Exit;
       end;
      
      {Get Next}
      Current:=Current.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(GPIODeviceTableLock);
   end;
  end;
end;

{==============================================================================}

procedure GPIOLog(Level:LongWord;GPIO:PGPIODevice;const AText:String);
var
 WorkBuffer:String;
begin
 {}
 {Check Level}
 if Level < GPIO_DEFAULT_LOG_LEVEL then Exit;
 
 WorkBuffer:='';
 {Check Level}
 if Level = GPIO_LOG_LEVEL_DEBUG then
  begin
   WorkBuffer:=WorkBuffer + '[DEBUG] ';
  end
 else if Level = GPIO_LOG_LEVEL_ERROR then
  begin
   WorkBuffer:=WorkBuffer + '[ERROR] ';
  end;
 
 {Add Prefix}
 WorkBuffer:=WorkBuffer + 'GPIO: ';
 
 {Check GPIO}
 if GPIO <> nil then
  begin
   WorkBuffer:=WorkBuffer + GPIO_NAME_PREFIX + IntToStr(GPIO.GPIOId) + ': ';
  end;

 {Output Logging}  
 LoggingOutputEx(LOGGING_FACILITY_GPIO,LogLevelToLoggingSeverity(Level),'GPIO',WorkBuffer + AText);
end;

{==============================================================================}

procedure GPIOLogInfo(GPIO:PGPIODevice;const AText:String);
begin
 {}
 GPIOLog(GPIO_LOG_LEVEL_INFO,GPIO,AText);
end;

{==============================================================================}

procedure GPIOLogError(GPIO:PGPIODevice;const AText:String);
begin
 {}
 GPIOLog(GPIO_LOG_LEVEL_ERROR,GPIO,AText);
end;

{==============================================================================}

procedure GPIOLogDebug(GPIO:PGPIODevice;const AText:String);
begin
 {}
 GPIOLog(GPIO_LOG_LEVEL_DEBUG,GPIO,AText);
end;

{==============================================================================}
{==============================================================================}

initialization
 GPIOInit;

{==============================================================================}
 
finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
