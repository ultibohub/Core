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

 GPIO devices represent the external or internal pins available on most system on chip (SoC)
 devices to provide control and interfacing capabilities for both hardware and software.
 
 This unit maintains pin numbering exactly as per the SoC documentation but abstracts other 
 features such as alternate function selects to avoid exposing chip specific values via the
 API.
 
 Not all GPIO devices support the same feature set so the GPIODeviceProperties function returns
 a structure which describes the number of pins as well as minimum and maximum pin numbers along
 with a set of flags that indicate what functionality is supported by the device.
 
 Multiple GPIO devices can be accomodated, each one is registered with this unit when the driver
 for the device is loaded and initialized. This unit includes functions for enumerating the devices
 that are available and each function takes a GPIODevice parameter to allow specifying the exact
 device to control.
 
 Simplified versions of many of the functions in this unit are provided in the Platform unit to
 allow control of the default GPIO device and in cases where there is only one device registered
 these functions will provide most of the capability required.
 
}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit GPIO; 

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,SysUtils;

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
 GPIO_FLAG_NONE            = $00000000;
 GPIO_FLAG_PULL_UP         = $00000001; {Device supports Pull Up on a pin}
 GPIO_FLAG_PULL_DOWN       = $00000002; {Device supports Pull Down on a pin}
 GPIO_FLAG_TRIGGER_LOW     = $00000004; {Device supports Trigger on Low level on a pin}
 GPIO_FLAG_TRIGGER_HIGH    = $00000008; {Device supports Trigger on High level on a pin}
 GPIO_FLAG_TRIGGER_RISING  = $00000010; {Device supports Trigger on Rising edge on a pin}
 GPIO_FLAG_TRIGGER_FALLING = $00000020; {Device supports Trigger on Falling edge on a pin}
 GPIO_FLAG_TRIGGER_EDGE    = $00000040; {Device supports Trigger on any edge (Rising or Falling) on a pin}
 GPIO_FLAG_TRIGGER_ASYNC   = $00000080; {Device supports Trigger on Asynchronous Rising/Falling edge on a pin}
 
 {GPIO Event Flags}
 GPIO_EVENT_FLAG_NONE      = $00000000;
 GPIO_EVENT_FLAG_REPEAT    = $00000001; {Event will be repeated until cancelled}
 GPIO_EVENT_FLAG_INTERRUPT = $00000002; {Event will be dispatched by interrupt handler (If applicable)}
                                        {Caution: Events called by the interrupt handler must obey interrupt rules with regard to locks, memory allocation and latency}
 {GPIO logging}
 GPIO_LOG_LEVEL_DEBUG     = LOG_LEVEL_DEBUG;  {GPIO debugging messages}
 GPIO_LOG_LEVEL_INFO      = LOG_LEVEL_INFO;   {GPIO informational messages, such as a device being attached or detached}
 GPIO_LOG_LEVEL_ERROR     = LOG_LEVEL_ERROR;  {GPIO error messages}
 GPIO_LOG_LEVEL_NONE      = LOG_LEVEL_NONE;   {No GPIO messages}

var 
 GPIO_DEFAULT_LOG_LEVEL:LongWord = GPIO_LOG_LEVEL_DEBUG; {Minimum level for GPIO messages.  Only messages with level greater than or equal to this will be printed}
 
var 
 {GPIO logging}
 GPIO_LOG_ENABLED:Boolean; 
 
{==============================================================================}
type
 {GPIO specific types}
 
 {GPIO Properties}
 PGPIOProperties = ^TGPIOProperties;
 TGPIOProperties = record
  Flags:LongWord;        {Device flags (eg GPIO_FLAG_TRIGGER_HIGH)}
  PinMin:LongWord;
  PinMax:LongWord;
  PinCount:LongWord;
  FunctionMin:LongWord;
  FunctionMax:LongWord;
  FunctionCount:LongWord;
 end;
 
 {GPIO Device}
 PGPIODevice = ^TGPIODevice; {Forward declared for GPIOPin}
 PGPIOPin = ^TGPIOPin;       {Forward declared for GPIOEvent}
 
 {GPIO Event}
 PGPIOEvent = ^TGPIOEvent;
 TGPIOEvent = record
  Pin:PGPIOPin;            {GPIO Pin this event belongs to}
  Callback:TGPIOCallback;  {Callback function to call when trigger occurs}
  Data:Pointer;            {Pointer to pass to the callback function when trigger occurs}
  Timeout:LongWord;        {Timeout in milliseconds for this callback (or INFINITE for no timeout)}
  Prev:PGPIOEvent;         {Previous event in the list}
  Next:PGPIOEvent;         {Next event in the list}
 end;
 
 {GPIO Pin}
 TGPIOPin = record
  GPIO:PGPIODevice;        {GPIO device this pin belongs to}
  Pin:LongWord;            {Pin number of this pin on the device (May be used by drivers for internal numbering)}
  Flags:LongWord;          {Current flags for this pin (eg GPIO_EVENT_FLAG_REPEAT)}
  Trigger:LongWord;        {Current trigger value for this pin (or GPIO_TRIGGER_NONE if no triggers current)}
  Count:LongWord;          {Count of threads and events waiting for the trigger}
  Event:TEventHandle;      {Event for threads waiting for the trigger}
  Events:PGPIOEvent;       {List of events waiting for the trigger}
 end;
 
 {GPIO Enumeration Callback}
 TGPIOEnumerate = function(GPIO:PGPIODevice;Data:Pointer):LongWord;
 {GPIO Notification Callback}
 TGPIONotification = function(Device:PDevice;Data:Pointer;Notification:LongWord):LongWord;
 
 {GPIO Device Methods}
 TGPIODeviceStart = function(GPIO:PGPIODevice):LongWord; 
 TGPIODeviceStop = function(GPIO:PGPIODevice):LongWord; 
 
 TGPIODeviceRead = function(GPIO:PGPIODevice;Reg:LongWord):LongWord; 
 TGPIODeviceWrite = procedure(GPIO:PGPIODevice;Reg,Value:LongWord);
 
 TGPIODeviceInputGet = function(GPIO:PGPIODevice;Pin:LongWord):LongWord;
 TGPIODeviceInputWait = function(GPIO:PGPIODevice;Pin,Trigger,Timeout:LongWord):LongWord;
 TGPIODeviceInputEvent = function(GPIO:PGPIODevice;Pin,Trigger,Flags,Timeout:LongWord;Callback:TGPIOCallback;Data:Pointer):LongWord;
 TGPIODeviceInputCancel = function(GPIO:PGPIODevice;Pin:LongWord):LongWord;
 
 TGPIODeviceOutputSet = function(GPIO:PGPIODevice;Pin,Level:LongWord):LongWord;
 
 TGPIODevicePullGet = function(GPIO:PGPIODevice;Pin:LongWord):LongWord;
 TGPIODevicePullSelect = function(GPIO:PGPIODevice;Pin,Mode:LongWord):LongWord;
 
 TGPIODeviceFunctionGet = function(GPIO:PGPIODevice;Pin:LongWord):LongWord;
 TGPIODeviceFunctionSelect = function(GPIO:PGPIODevice;Pin,Mode:LongWord):LongWord;
 
 TGPIODeviceProperties = function(GPIO:PGPIODevice;Properties:PGPIOProperties):LongWord;
 
 TGPIODevice = record
  {Device Properties}
  Device:TDevice;                                 {The Device entry for this GPIO}
  {GPIO Properties}
  GPIOId:LongWord;                                {Unique Id of this GPIO in the GPIO table}
  GPIOState:LongWord;                             {GPIO state (eg GPIO_STATE_ENABLED)}
  DeviceStart:TGPIODeviceStart;                   {A Device specific DeviceStart method implementing the standard GPIO device interface (Mandatory)}
  DeviceStop:TGPIODeviceStop;                     {A Device specific DeviceStop method implementing the standard GPIO device interface (Mandatory)}
  DeviceRead:TGPIODeviceRead;                     {A Device specific DeviceRead method implementing the standard GPIO device interface (Or nil if the default method is suitable)}
  DeviceWrite:TGPIODeviceWrite;                   {A Device specific DeviceWrite method implementing the standard GPIO device interface (Or nil if the default method is suitable)}
  DeviceInputGet:TGPIODeviceInputGet;             {A Device specific DeviceInputGet method implementing the standard GPIO device interface (Mandatory)}
  DeviceInputWait:TGPIODeviceInputWait;           {A Device specific DeviceInputWait method implementing the standard GPIO device interface (Or nil if the operation is not supported)}
  DeviceInputEvent:TGPIODeviceInputEvent;         {A Device specific DeviceInputEvent method implementing the standard GPIO device interface (Or nil if the operation is not supported)}
  DeviceInputCancel:TGPIODeviceInputCancel;       {A Device specific DeviceInputCancel method implementing the standard GPIO device interface (Or nil if the operation is not supported)}
  DeviceOutputSet:TGPIODeviceOutputSet;           {A Device specific DeviceOutputSet method implementing the standard GPIO device interface (Mandatory)}
  DevicePullGet:TGPIODevicePullGet;               {A Device specific DevicePullGet method implementing the standard GPIO device interface (Or nil if the operation is not supported)}
  DevicePullSelect:TGPIODevicePullSelect;         {A Device specific DevicePullSelect method implementing the standard GPIO device interface (Or nil if the operation is not supported)}
  DeviceFunctionGet:TGPIODeviceFunctionGet;       {A Device specific DeviceFunctionGet method implementing the standard GPIO device interface (Or nil if the operation is not supported)}
  DeviceFunctionSelect:TGPIODeviceFunctionSelect; {A Device specific DeviceFunctionSelect method implementing the standard GPIO device interface (Or nil if the operation is not supported)}
  DeviceProperties:TGPIODeviceProperties;         {A Device specific DeviceProperties method implementing the standard GPIO device interfac (Or nil if the default method is suitable)e}
  {Driver Properties}
  Lock:TMutexHandle;                              {Device lock}
  Address:Pointer;                                {Device register base address}
  Pins:array of TGPIOPin;                         {Device pins}
  Properties:TGPIOProperties;                     {Device properties}
  {Statistics Properties}
  GetCount:LongWord;
  SetCount:LongWord;
  WaitCount:LongWord;
  EventCount:LongWord;
  {Internal Properties}                                                                        
  Prev:PGPIODevice;                               {Previous entry in GPIO table}
  Next:PGPIODevice;                               {Next entry in GPIO table}
 end; 
  
 {GPIO Info (Pin Information)} {Used by other units to pass complete details of a GPIO pin}
 PGPIOInfo = ^TGPIOInfo;
 TGPIOInfo = record
  GPIO:PGPIODevice;        {Device for this GPIO pin}
  Pin:LongWord;            {Pin number (eg GPIO_PIN_59)}
  Func:LongWord;           {Function value (or GPIO_FUNCTION_UNKNOWN)}
  Pull:LongWord;           {Pull Up/Down value (or GPIO_PULL_UNKNOWN)}
  Trigger:LongWord;        {Trigger value (or GPIO_TRIGGER_UNKNOWN)}
 end;

const
 {GPIO Info Unknown}
 GPIO_INFO_UNKNOWN:TGPIOInfo = (
  GPIO:nil;
  Pin:GPIO_PIN_UNKNOWN;
  Func:GPIO_FUNCTION_UNKNOWN;
  Pull:GPIO_PULL_UNKNOWN;
  Trigger:GPIO_TRIGGER_UNKNOWN
 );
 
{==============================================================================}
{var}
 {GPIO specific variables}
 
{==============================================================================}
{Initialization Functions}
procedure GPIOInit;
 
{==============================================================================}
{GPIO Functions}
function GPIODeviceStart(GPIO:PGPIODevice):LongWord; 
function GPIODeviceStop(GPIO:PGPIODevice):LongWord; 

function GPIODeviceRead(GPIO:PGPIODevice;Reg:LongWord):LongWord; 
procedure GPIODeviceWrite(GPIO:PGPIODevice;Reg,Value:LongWord);
 
function GPIODeviceInputGet(GPIO:PGPIODevice;Pin:LongWord):LongWord;
function GPIODeviceInputWait(GPIO:PGPIODevice;Pin,Trigger,Timeout:LongWord):LongWord;
function GPIODeviceInputEvent(GPIO:PGPIODevice;Pin,Trigger,Flags,Timeout:LongWord;Callback:TGPIOCallback;Data:Pointer):LongWord;
function GPIODeviceInputCancel(GPIO:PGPIODevice;Pin:LongWord):LongWord;

function GPIODeviceOutputSet(GPIO:PGPIODevice;Pin,Level:LongWord):LongWord;

function GPIODevicePullGet(GPIO:PGPIODevice;Pin:LongWord):LongWord;
function GPIODevicePullSelect(GPIO:PGPIODevice;Pin,Mode:LongWord):LongWord;

function GPIODeviceFunctionGet(GPIO:PGPIODevice;Pin:LongWord):LongWord;
function GPIODeviceFunctionSelect(GPIO:PGPIODevice;Pin,Mode:LongWord):LongWord;

function GPIODeviceProperties(GPIO:PGPIODevice;Properties:PGPIOProperties):LongWord;
 
function GPIODeviceCreate:PGPIODevice;
function GPIODeviceCreateEx(Size:LongWord):PGPIODevice;
function GPIODeviceDestroy(GPIO:PGPIODevice):LongWord;

function GPIODeviceRegister(GPIO:PGPIODevice):LongWord;
function GPIODeviceDeregister(GPIO:PGPIODevice):LongWord;

function GPIODeviceFind(GPIOId:LongWord):PGPIODevice;
function GPIODeviceFindByName(const Name:String):PGPIODevice; inline;
function GPIODeviceFindByDescription(const Description:String):PGPIODevice; inline;
function GPIODeviceEnumerate(Callback:TGPIOEnumerate;Data:Pointer):LongWord;
 
function GPIODeviceNotification(GPIO:PGPIODevice;Callback:TGPIONotification;Data:Pointer;Notification,Flags:LongWord):LongWord;

{==============================================================================}
{RTL GPIO Functions}
function SysGPIOAvailable:Boolean;

function SysGPIOInputGet(Pin:LongWord):LongWord;
function SysGPIOInputWait(Pin,Trigger,Timeout:LongWord):LongWord;
function SysGPIOInputEvent(Pin,Trigger,Timeout:LongWord;Callback:TGPIOCallback;Data:Pointer):LongWord;
 
function SysGPIOOutputSet(Pin,Level:LongWord):LongWord;   

function SysGPIOPullGet(Pin:LongWord):LongWord;
function SysGPIOPullSelect(Pin,Mode:LongWord):LongWord;
 
function SysGPIOFunctionGet(Pin:LongWord):LongWord;
function SysGPIOFunctionSelect(Pin,Mode:LongWord):LongWord;

{==============================================================================}
{GPIO Helper Functions}
function GPIOGetCount:LongWord; inline;
function GPIODeviceGetDefault:PGPIODevice; inline;
function GPIODeviceSetDefault(GPIO:PGPIODevice):LongWord; 

function GPIODeviceCheck(GPIO:PGPIODevice):PGPIODevice;

function GPIODeviceCreateEvent(GPIO:PGPIODevice;Pin:PGPIOPin;Callback:TGPIOCallback;Data:Pointer;Timeout:LongWord):PGPIOEvent;
function GPIODeviceDestroyEvent(GPIO:PGPIODevice;Event:PGPIOEvent):LongWord;

function GPIODeviceRegisterEvent(GPIO:PGPIODevice;Pin:PGPIOPin;Event:PGPIOEvent):LongWord;
function GPIODeviceDeregisterEvent(GPIO:PGPIODevice;Pin:PGPIOPin;Event:PGPIOEvent):LongWord;

procedure GPIOLog(Level:LongWord;GPIO:PGPIODevice;const AText:String);
procedure GPIOLogInfo(GPIO:PGPIODevice;const AText:String);
procedure GPIOLogError(GPIO:PGPIODevice;const AText:String);
procedure GPIOLogDebug(GPIO:PGPIODevice;const AText:String);

function GPIOPinToString(Pin:LongWord):String;
function GPIOLevelToString(Level:LongWord):String;
function GPIOTriggerToString(Trigger:LongWord):String;

function GPIOPullToString(Value:LongWord):String;
function GPIOFunctionToString(Value:LongWord):String;

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

 GPIODeviceDefault:PGPIODevice;
 
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
 GPIODeviceDefault:=nil;
 
 {Register Platform GPIO Handlers}
 GPIOAvailableHandler:=SysGPIOAvailable;
 {Do not register GPIOReadHandler}
 {Do not register GPIOWriteHandler}
 GPIOInputGetHandler:=SysGPIOInputGet;
 GPIOInputWaitHandler:=SysGPIOInputWait;
 GPIOInputEventHandler:=SysGPIOInputEvent;
 GPIOOutputSetHandler:=SysGPIOOutputSet; 
 GPIOPullGetHandler:=SysGPIOPullGet;
 GPIOPullSelectHandler:=SysGPIOPullSelect;
 GPIOFunctionGetHandler:=SysGPIOFunctionGet;
 GPIOFunctionSelectHandler:=SysGPIOFunctionSelect;
 
 GPIOInitialized:=True;
end;
 
{==============================================================================}
{==============================================================================}
{GPIO Functions}
function GPIODeviceStart(GPIO:PGPIODevice):LongWord; 
{Start the specified GPIO device and enable access}
{GPIO: The GPIO device to start}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check GPIO}
 if GPIO = nil then Exit;
 if GPIO.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IFDEF GPIO_DEBUG}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'GPIO Device Start');
 {$ENDIF}
 
 {Check Disabled}
 Result:=ERROR_SUCCESS;
 if GPIO.GPIOState <> GPIO_STATE_DISABLED then Exit;

 if MutexLock(GPIO.Lock) = ERROR_SUCCESS then
  begin
   try
    if Assigned(GPIO.DeviceStart) then
     begin
      {Call Device Start}
      Result:=GPIO.DeviceStart(GPIO);
      if Result <> ERROR_SUCCESS then Exit;
     end
    else
     begin    
      Result:=ERROR_INVALID_PARAMETER;
      Exit;
     end; 
 
    {Enable Device}
    GPIO.GPIOState:=GPIO_STATE_ENABLED;
    
    {Notify Enable}
    NotifierNotify(@GPIO.Device,DEVICE_NOTIFICATION_ENABLE);
    
    Result:=ERROR_SUCCESS;
   finally
    MutexUnlock(GPIO.Lock);
   end; 
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;    
end;

{==============================================================================}

function GPIODeviceStop(GPIO:PGPIODevice):LongWord; 
{Stop the specified GPIO device and disable access}
{GPIO: The GPIO device to stop}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check GPIO}
 if GPIO = nil then Exit;
 if GPIO.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IFDEF GPIO_DEBUG}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'GPIO Device Stop');
 {$ENDIF}
 
 {Check Enabled}
 Result:=ERROR_SUCCESS;
 if GPIO.GPIOState <> GPIO_STATE_ENABLED then Exit;
 
 if MutexLock(GPIO.Lock) = ERROR_SUCCESS then
  begin
   try
    if Assigned(GPIO.DeviceStop) then
     begin
      {Call Device Stop}
      Result:=GPIO.DeviceStop(GPIO);
      if Result <> ERROR_SUCCESS then Exit;
     end
    else
     begin
      Result:=ERROR_INVALID_PARAMETER;
      Exit;
     end;    
   
    {Disable Device}
    GPIO.GPIOState:=GPIO_STATE_DISABLED;
    
    {Notify Disable}
    NotifierNotify(@GPIO.Device,DEVICE_NOTIFICATION_DISABLE);
    
    Result:=ERROR_SUCCESS;
   finally
    MutexUnlock(GPIO.Lock);
   end; 
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;    
end;

{==============================================================================}

function GPIODeviceRead(GPIO:PGPIODevice;Reg:LongWord):LongWord; 
{Perform a direct read from a register of the specified GPIO device}
{GPIO: The GPIO device to read from}
{Reg: The memory register to read from}
{Return: The value of the memory register}
begin
 {}
 Result:=0;
 
 {Check GPIO}
 if GPIO = nil then Exit;
 if GPIO.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IFDEF GPIO_DEBUG}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'GPIO Device Read (Reg=' + IntToHex(Reg,8) + ')');
 {$ENDIF}
 
 {Check Enabled}
 if GPIO.GPIOState <> GPIO_STATE_ENABLED then Exit;
 
 if MutexLock(GPIO.Lock) = ERROR_SUCCESS then
  begin
   if Assigned(GPIO.DeviceRead) then
    begin
     {Call Device Read}
     Result:=GPIO.DeviceRead(GPIO,Reg);
    end;
    
   MutexUnlock(GPIO.Lock);
  end;    
end;

{==============================================================================}

procedure GPIODeviceWrite(GPIO:PGPIODevice;Reg,Value:LongWord);
{Perform a direct write to a register of the specified GPIO device}
{GPIO: The GPIO device to write to}
{Reg: The memory register to write to}
{Value: The value to write to the register}
begin
 {}
 {Check GPIO}
 if GPIO = nil then Exit;
 if GPIO.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IFDEF GPIO_DEBUG}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'GPIO Device Write (Reg=' + IntToHex(Reg,8) + ' Value=' + IntToHex(Value,8) + ')');
 {$ENDIF}
 
 {Check Enabled}
 if GPIO.GPIOState <> GPIO_STATE_ENABLED then Exit;
 
 if MutexLock(GPIO.Lock) = ERROR_SUCCESS then
  begin
   if Assigned(GPIO.DeviceWrite) then
    begin
     {Call Device Write}
     GPIO.DeviceWrite(GPIO,Reg,Value);
    end;
    
   MutexUnlock(GPIO.Lock);
  end;    
end;

{==============================================================================}

function GPIODeviceInputGet(GPIO:PGPIODevice;Pin:LongWord):LongWord;
{Get the current state of an input pin on the specified GPIO device}
{GPIO: The GPIO device to get from}
{Pin: The pin to get the state for (eg GPIO_PIN_1)}
{Return: The current state (eg GPIO_LEVEL_HIGH) or GPIO_LEVEL_UNKNOWN on failure}
begin
 {}
 Result:=GPIO_LEVEL_UNKNOWN;
 
 {Check GPIO}
 if GPIO = nil then Exit;
 if GPIO.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IFDEF GPIO_DEBUG}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'GPIO Device Input Get (Pin=' + GPIOPinToString(Pin) + ')');
 {$ENDIF}
 
 {Check Enabled}
 if GPIO.GPIOState <> GPIO_STATE_ENABLED then Exit;
 
 if MutexLock(GPIO.Lock) = ERROR_SUCCESS then
  begin
   if Assigned(GPIO.DeviceInputGet) then
    begin
     {Call Device Input Get}
     Result:=GPIO.DeviceInputGet(GPIO,Pin);
    end
   else
    begin
     Result:=ERROR_INVALID_PARAMETER;
    end;    
    
   MutexUnlock(GPIO.Lock);
  end;    
end;

{==============================================================================}

function GPIODeviceInputWait(GPIO:PGPIODevice;Pin,Trigger,Timeout:LongWord):LongWord;
{Wait for the state of a input pin to change on the specified GPIO device}
{GPIO: The GPIO device to wait for}
{Pin: The pin to wait for the state to change (eg GPIO_PIN_1)}
{Trigger: The trigger event to wait for (eg GPIO_TRIGGER_HIGH)}
{Timeout: Number of milliseconds to wait for the change (INFINITE to wait forever)}
{Return: The state after the change (eg GPIO_LEVEL_HIGH) or GPIO_LEVEL_UNKNOWN on failure or timeout}
begin
 {}
 Result:=GPIO_LEVEL_UNKNOWN;
 
 {Check GPIO}
 if GPIO = nil then Exit;
 if GPIO.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IFDEF GPIO_DEBUG}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'GPIO Device Input Wait (Pin=' + GPIOPinToString(Pin) + ' Trigger=' + GPIOTriggerToString(Trigger) + ' Timeout=' + IntToStr(Timeout) + ')');
 {$ENDIF}
 
 {Check Enabled}
 if GPIO.GPIOState <> GPIO_STATE_ENABLED then Exit;
 
 if MutexLock(GPIO.Lock) = ERROR_SUCCESS then
  begin
   if Assigned(GPIO.DeviceInputWait) then
    begin
     {Call Device Input Wait}
     Result:=GPIO.DeviceInputWait(GPIO,Pin,Trigger,Timeout);
    end;
    
   MutexUnlock(GPIO.Lock);
  end;    
end;

{==============================================================================}

function GPIODeviceInputEvent(GPIO:PGPIODevice;Pin,Trigger,Flags,Timeout:LongWord;Callback:TGPIOCallback;Data:Pointer):LongWord;
{Schedule a function to be called when the state of a input pin changes on the specified GPIO device}
{GPIO: The GPIO device to schedule the callback for}
{Pin: The pin to schedule the state change for (eg GPIO_PIN_1)}
{Trigger: The trigger event which will cause the function to be called (eg GPIO_TRIGGER_HIGH)}
{Timeout: The number of milliseconds before the scheduled trigger expires (INFINITE to never expire)}
{Callback: The function to be called when the trigger occurs}
{Data: A pointer to be pass to the function when the trigger occurs (Optional)}
{Return: ERROR_SUCCESS if the trigger was scheduled successfully or another error code on failure}

{Note: The pin and trigger that caused the event will be passed to the callback function}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check GPIO}
 if GPIO = nil then Exit;
 if GPIO.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IFDEF GPIO_DEBUG}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'GPIO Device Input Event (Pin=' + GPIOPinToString(Pin) + ' Trigger=' + GPIOTriggerToString(Trigger) + ' Flags=' + IntToHex(Flags,8) + ' Timeout=' + IntToStr(Timeout) + ')');
 {$ENDIF}
 
 {Check Enabled}
 Result:=ERROR_NOT_SUPPORTED;
 if GPIO.GPIOState <> GPIO_STATE_ENABLED then Exit;
 
 if MutexLock(GPIO.Lock) = ERROR_SUCCESS then
  begin
   if Assigned(GPIO.DeviceInputEvent) then
    begin
     {Call Device Input Event}
     Result:=GPIO.DeviceInputEvent(GPIO,Pin,Trigger,Flags,Timeout,Callback,Data);
    end;
    
   MutexUnlock(GPIO.Lock);
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;    
end;

{==============================================================================}

function GPIODeviceInputCancel(GPIO:PGPIODevice;Pin:LongWord):LongWord;
{Cancel a previously scheduled event callback function for an input pin on the specified GPIO device}
{GPIO: The GPIO device to cancel the callback for}
{Pin: The pin to cancel the state change for (eg GPIO_PIN_1)}
{Return: ERROR_SUCCESS if the callback was cancelled successfully or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check GPIO}
 if GPIO = nil then Exit;
 if GPIO.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IFDEF GPIO_DEBUG}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'GPIO Device Input Cancel (Pin=' + GPIOPinToString(Pin) + ')');
 {$ENDIF}
 
 {Check Enabled}
 Result:=ERROR_NOT_SUPPORTED;
 if GPIO.GPIOState <> GPIO_STATE_ENABLED then Exit;
 
 if MutexLock(GPIO.Lock) = ERROR_SUCCESS then
  begin
   if Assigned(GPIO.DeviceInputCancel) then
    begin
     {Call Device Input Cancel}
     Result:=GPIO.DeviceInputCancel(GPIO,Pin);
    end;
    
   MutexUnlock(GPIO.Lock);
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;    
end;

{==============================================================================}
 
function GPIODeviceOutputSet(GPIO:PGPIODevice;Pin,Level:LongWord):LongWord;
{Set the state of a output pin on the specified GPIO device}
{GPIO: The GPIO device to set for}
{Pin: The pin to set the state for (eg GPIO_PIN_1)}
{Level: The state to set the pin to (eg GPIO_LEVEL_HIGH)}
{Return: ERROR_SUCCESS if completed successfully or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check GPIO}
 if GPIO = nil then Exit;
 if GPIO.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IFDEF GPIO_DEBUG}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'GPIO Device Output Set (Pin=' + GPIOPinToString(Pin) + ' Level=' + GPIOLevelToString(Level) + ')');
 {$ENDIF}
 
 {Check Enabled}
 Result:=ERROR_NOT_SUPPORTED;
 if GPIO.GPIOState <> GPIO_STATE_ENABLED then Exit;
 
 if MutexLock(GPIO.Lock) = ERROR_SUCCESS then
  begin
   if Assigned(GPIO.DeviceOutputSet) then
    begin
     {Call Device Output Set}
     Result:=GPIO.DeviceOutputSet(GPIO,Pin,Level);
    end
   else
    begin
     Result:=ERROR_INVALID_PARAMETER;
    end;    
    
   MutexUnlock(GPIO.Lock);
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;    
end;

{==============================================================================}

function GPIODevicePullGet(GPIO:PGPIODevice;Pin:LongWord):LongWord;
{Get the current pull state of a pin on the specified GPIO device}
{GPIO: The GPIO device to get from}
{Pin: The pin to get the pull state for (eg GPIO_PIN_1)}
{Return: The current pull state of the pin (eg GPIO_PULL_UP) or GPIO_PULL_UNKNOWN on failure}
begin
 {}
 Result:=GPIO_PULL_UNKNOWN;
 
 {Check GPIO}
 if GPIO = nil then Exit;
 if GPIO.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IFDEF GPIO_DEBUG}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'GPIO Device Pull Get (Pin=' + GPIOPinToString(Pin) + ')');
 {$ENDIF}
 
 {Check Enabled}
 if GPIO.GPIOState <> GPIO_STATE_ENABLED then Exit;
 
 if MutexLock(GPIO.Lock) = ERROR_SUCCESS then
  begin
   if Assigned(GPIO.DevicePullGet) then
    begin
     {Call Device Pull Get}
     Result:=GPIO.DevicePullGet(GPIO,Pin);
    end;
    
   MutexUnlock(GPIO.Lock);
  end;    
end;

{==============================================================================}
 
function GPIODevicePullSelect(GPIO:PGPIODevice;Pin,Mode:LongWord):LongWord;
{Change the pull state of a pin on the specified GPIO device}
{GPIO: The GPIO device to set for}
{Pin: The pin to change the pull state for (eg GPIO_PIN_1)}
{Mode: The pull state to set for the pin (eg GPIO_PULL_UP)}
{Return: ERROR_SUCCESS if completed successfully or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check GPIO}
 if GPIO = nil then Exit;
 if GPIO.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IFDEF GPIO_DEBUG}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'GPIO Device Pull Select (Pin=' + GPIOPinToString(Pin) + ' Mode=' + GPIOPullToString(Mode) + ')');
 {$ENDIF}
 
 {Check Enabled}
 Result:=ERROR_NOT_SUPPORTED;
 if GPIO.GPIOState <> GPIO_STATE_ENABLED then Exit;
 
 if MutexLock(GPIO.Lock) = ERROR_SUCCESS then
  begin
   if Assigned(GPIO.DevicePullSelect) then
    begin
     {Call Device Pull Select}
     Result:=GPIO.DevicePullSelect(GPIO,Pin,Mode);
    end;
    
   MutexUnlock(GPIO.Lock);
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;    
end;

{==============================================================================}

function GPIODeviceFunctionGet(GPIO:PGPIODevice;Pin:LongWord):LongWord;
{Get the current function of a pin on the specified GPIO device}
{GPIO: The GPIO device to get from}
{Pin: The pin to get the function for (eg GPIO_PIN_1)}
{Return: The current function of the pin (eg GPIO_FUNCTION_IN) or GPIO_FUNCTION_UNKNOWN on failure}
begin
 {}
 Result:=GPIO_FUNCTION_UNKNOWN;
 
 {Check GPIO}
 if GPIO = nil then Exit;
 if GPIO.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IFDEF GPIO_DEBUG}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'GPIO Device Function Get (Pin=' + GPIOPinToString(Pin) + ')');
 {$ENDIF}
 
 {Check Enabled}
 if GPIO.GPIOState <> GPIO_STATE_ENABLED then Exit;
 
 if MutexLock(GPIO.Lock) = ERROR_SUCCESS then
  begin
   if Assigned(GPIO.DeviceFunctionGet) then
    begin
     {Call Device Function Get}
     Result:=GPIO.DeviceFunctionGet(GPIO,Pin);
    end;
    
   MutexUnlock(GPIO.Lock);
  end;    
end;

{==============================================================================}

function GPIODeviceFunctionSelect(GPIO:PGPIODevice;Pin,Mode:LongWord):LongWord;
{Change the function of a pin on the specified GPIO device}
{GPIO: The GPIO device to set for}
{Pin: The pin to change the function for (eg GPIO_PIN_1)}
{Mode: The function to set for the pin (eg GPIO_FUNCTION_OUT)}
{Return: ERROR_SUCCESS if completed successfully or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check GPIO}
 if GPIO = nil then Exit;
 if GPIO.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IFDEF GPIO_DEBUG}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'GPIO Device Function Select (Pin=' + GPIOPinToString(Pin) + ' Mode=' + GPIOFunctionToString(Mode) + ')');
 {$ENDIF}
 
 {Check Enabled}
 Result:=ERROR_NOT_SUPPORTED;
 if GPIO.GPIOState <> GPIO_STATE_ENABLED then Exit;
 
 if MutexLock(GPIO.Lock) = ERROR_SUCCESS then
  begin
   if Assigned(GPIO.DeviceFunctionSelect) then
    begin
     {Call Device Function Select}
     Result:=GPIO.DeviceFunctionSelect(GPIO,Pin,Mode);
    end;
    
   MutexUnlock(GPIO.Lock);
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;    
end;

{==============================================================================}

function GPIODeviceProperties(GPIO:PGPIODevice;Properties:PGPIOProperties):LongWord;
{Get the properties for the specified GPIO device}
{GPIO: The GPIO device to get properties from}
{Properties: Pointer to a TGPIOProperties structure to fill in}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Properties}
 if Properties = nil then Exit;
 
 {Check GPIO}
 if GPIO = nil then Exit;
 if GPIO.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IFDEF GPIO_DEBUG}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'GPIO Device Properties');
 {$ENDIF}
 
 {Check Open}
 {Result:=ERROR_NOT_SUPPORTED;}
 {if GPIO.GPIOState <> GPIO_STATE_ENABLED then Exit;} {Allow when disabled}
 
 if MutexLock(GPIO.Lock) = ERROR_SUCCESS then
  begin
   if Assigned(GPIO.DeviceProperties) then
    begin
     {Call Device Properites}
     Result:=GPIO.DeviceProperties(GPIO,Properties);
    end
   else
    begin
     {Get Properties}
     System.Move(GPIO.Properties,Properties^,SizeOf(TGPIOProperties));
       
     {Return Result}
     Result:=ERROR_SUCCESS;
    end;  
    
   MutexUnlock(GPIO.Lock);
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;    
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
 Result.DeviceStart:=nil;
 Result.DeviceStop:=nil;
 Result.DeviceRead:=nil;
 Result.DeviceWrite:=nil;
 Result.DeviceInputGet:=nil;
 Result.DeviceInputWait:=nil;
 Result.DeviceInputEvent:=nil;
 Result.DeviceInputCancel:=nil;
 Result.DeviceOutputSet:=nil;
 Result.DevicePullGet:=nil;
 Result.DevicePullSelect:=nil;
 Result.DeviceFunctionGet:=nil;
 Result.DeviceFunctionSelect:=nil;
 Result.DeviceProperties:=nil;
 Result.Lock:=INVALID_HANDLE_VALUE;
 
 {Create Lock}
 Result.Lock:=MutexCreateEx(False,MUTEX_DEFAULT_SPINCOUNT,MUTEX_FLAG_RECURSIVE);
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
{GPIO: The GPIO device to destroy}
{Return: ERROR_SUCCESS if completed or another error code on failure}
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
{GPIO: The GPIO device to register}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 GPIOId:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check GPIO}
 if GPIO = nil then Exit;
 if GPIO.GPIOId <> DEVICE_ID_ANY then Exit;
 if GPIO.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check Interfaces}
 if not(Assigned(GPIO.DeviceStart)) then Exit;
 if not(Assigned(GPIO.DeviceStop)) then Exit;
 if not(Assigned(GPIO.DeviceInputGet)) then Exit;
 if not(Assigned(GPIO.DeviceOutputSet)) then Exit;
 
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
    
    {Check Default}
    if GPIODeviceDefault = nil then
     begin
      GPIODeviceDefault:=GPIO;
     end;
    
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
{GPIO: The GPIO device to deregister}
{Return: ERROR_SUCCESS if completed or another error code on failure}
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
 
    {Check Default}
    if GPIODeviceDefault = GPIO then
     begin
      GPIODeviceDefault:=GPIODeviceTable;
     end;
 
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
{Find a GPIO device by ID in the GPIO table}
{GPIOId: The ID number of the GPIO device to find}
{Return: Pointer to GPIO device entry or nil if not found}
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
    
function GPIODeviceFindByName(const Name:String):PGPIODevice; inline;
{Find a GPIO device by name in the GPIO table}
{Name: The name of the GPIO to find (eg GPIO0)}
{Return: Pointer to GPIO device entry or nil if not found}
begin
 {}
 Result:=PGPIODevice(DeviceFindByName(Name));
end;

{==============================================================================}

function GPIODeviceFindByDescription(const Description:String):PGPIODevice; inline;
{Find a GPIO device by description in the GPIO table}
{Description: The description of the GPIO to find (eg BCM2836 GPIO)}
{Return: Pointer to GPIO device entry or nil if not found}
begin
 {}
 Result:=PGPIODevice(DeviceFindByDescription(Description));
end;
       
{==============================================================================}

function GPIODeviceEnumerate(Callback:TGPIOEnumerate;Data:Pointer):LongWord;
{Enumerate all GPIO devices in the GPIO table}
{Callback: The callback function to call for each GPIO in the table}
{Data: A private data pointer to pass to callback for each GPIO in the table}
{Return: ERROR_SUCCESS if completed or another error code on failure}
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
{Register a notification for GPIO device changes}
{GPIO: The GPIO device to notify changes for (Optional, pass nil for all GPIO devices)}
{Callback: The function to call when a notification event occurs}
{Data: A private data pointer to pass to callback when a notification event occurs}
{Notification: The events to register for notification of (eg DEVICE_NOTIFICATION_REGISTER)}
{Flags: The flags to control the notification (eg NOTIFIER_FLAG_WORKER)}
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
{RTL GPIO Functions}
function SysGPIOAvailable:Boolean;
{Check if a GPIO device is available}
begin
 {}
 Result:=(GPIODeviceDefault <> nil);
end;

{==============================================================================}

function SysGPIOInputGet(Pin:LongWord):LongWord;
{Get the current state of a GPIO input pin}
{Pin: The pin to get the state for (eg GPIO_PIN_1)}
{Return: The current state (eg GPIO_LEVEL_HIGH) or GPIO_LEVEL_UNKNOWN on failure}
begin
 {}
 Result:=GPIO_LEVEL_UNKNOWN;
 
 if GPIODeviceDefault = nil then Exit;

 Result:=GPIODeviceInputGet(GPIODeviceDefault,Pin);
end;

{==============================================================================}

function SysGPIOInputWait(Pin,Trigger,Timeout:LongWord):LongWord;
{Wait for the state of a GPIO input pin to change}
{Pin: The pin to wait for the state to change (eg GPIO_PIN_1)}
{Trigger: The trigger event to wait for (eg GPIO_TRIGGER_HIGH)}
{Timeout: Number of milliseconds to wait for the change (INFINITE to wait forever)}
{Return: The state after the change (eg GPIO_LEVEL_HIGH) or GPIO_LEVEL_UNKNOWN on failure or timeout}
begin
 {}
 Result:=GPIO_LEVEL_UNKNOWN;
 
 if GPIODeviceDefault = nil then Exit;

 Result:=GPIODeviceInputWait(GPIODeviceDefault,Pin,Trigger,Timeout);
end;

{==============================================================================}

function SysGPIOInputEvent(Pin,Trigger,Timeout:LongWord;Callback:TGPIOCallback;Data:Pointer):LongWord;
{Schedule a function to be called when the state of a GPIO input pin changes}
{Pin: The pin to schedule the state change for (eg GPIO_PIN_1)}
{Trigger: The trigger event which will cause the function to be called (eg GPIO_TRIGGER_HIGH)}
{Timeout: The number of milliseconds before the scheduled trigger expires (INFINITE to never expire)}
{Callback: The function to be called when the trigger occurs}
{Data: A pointer to be pass to the function when the trigger occurs (Optional)}
{Return: ERROR_SUCCESS if the trigger was scheduled successfully or another error code on failure}

{Note: The pin and trigger that caused the event will be passed to the callback function}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 if GPIODeviceDefault = nil then Exit;

 Result:=GPIODeviceInputEvent(GPIODeviceDefault,Pin,Trigger,GPIO_EVENT_FLAG_NONE,Timeout,Callback,Data);
end;

{==============================================================================}
 
function SysGPIOOutputSet(Pin,Level:LongWord):LongWord;   
{Set the state of a GPIO output pin}
{Pin: The pin to set the state for (eg GPIO_PIN_1)}
{Level: The state to set the pin to (eg GPIO_LEVEL_HIGH)}
{Return: ERROR_SUCCESS if completed successfully or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 if GPIODeviceDefault = nil then Exit;

 Result:=GPIODeviceOutputSet(GPIODeviceDefault,Pin,Level);
end;


{==============================================================================}

function SysGPIOPullGet(Pin:LongWord):LongWord;
{Get the current pull state of a GPIO pin}
{Pin: The pin to get the pull state for (eg GPIO_PIN_1)}
{Return: The current pull state of the pin (eg GPIO_PULL_UP) or GPIO_PULL_UNKNOWN on failure}
begin
 {}
 Result:=GPIO_PULL_UNKNOWN;
 
 if GPIODeviceDefault = nil then Exit;

 Result:=GPIODevicePullGet(GPIODeviceDefault,Pin);
end;

{==============================================================================}
 
function SysGPIOPullSelect(Pin,Mode:LongWord):LongWord;
{Change the pull state of a GPIO pin}
{Pin: The pin to change the pull state for (eg GPIO_PIN_1)}
{Mode: The pull state to set for the pin (eg GPIO_PULL_UP)}
{Return: ERROR_SUCCESS if completed successfully or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 if GPIODeviceDefault = nil then Exit;

 Result:=GPIODevicePullSelect(GPIODeviceDefault,Pin,Mode);
end;

{==============================================================================}

function SysGPIOFunctionGet(Pin:LongWord):LongWord;
{Get the current function of a GPIO pin}
{Pin: The pin to get the function for (eg GPIO_PIN_1)}
{Return: The current function of the pin (eg GPIO_FUNCTION_IN) or GPIO_FUNCTION_UNKNOWN on failure}
begin
 {}
 Result:=GPIO_FUNCTION_UNKNOWN;
 
 if GPIODeviceDefault = nil then Exit;

 Result:=GPIODeviceFunctionGet(GPIODeviceDefault,Pin);
end;

{==============================================================================}

function SysGPIOFunctionSelect(Pin,Mode:LongWord):LongWord;
{Change the function of a GPIO pin}
{Pin: The pin to change the function for (eg GPIO_PIN_1)}
{Mode: The function to set for the pin (eg GPIO_FUNCTION_OUT)}
{Return: ERROR_SUCCESS if completed successfully or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 if GPIODeviceDefault = nil then Exit;

 Result:=GPIODeviceFunctionSelect(GPIODeviceDefault,Pin,Mode);
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

function GPIODeviceGetDefault:PGPIODevice; inline;
{Get the current default GPIO device}
begin
 {}
 Result:=GPIODeviceDefault;
end;

{==============================================================================}

function GPIODeviceSetDefault(GPIO:PGPIODevice):LongWord; 
{Set the current default GPIO device}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check GPIO}
 if GPIO = nil then Exit;
 if GPIO.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(GPIODeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Check GPIO}
    if GPIODeviceCheck(GPIO) <> GPIO then Exit;
    
    {Set GPIO Default}
    GPIODeviceDefault:=GPIO;
    
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

function GPIODeviceCreateEvent(GPIO:PGPIODevice;Pin:PGPIOPin;Callback:TGPIOCallback;Data:Pointer;Timeout:LongWord):PGPIOEvent;
{Create a new event using the supplied parameters}

{Note: Event must be registered by calling GPIODeviceRegisterEvent}
{Note: Caller must hold the GPIO device lock}
begin
 {}
 Result:=nil;
 
 {Check GPIO}
 if GPIO = nil then Exit;
 if GPIO.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Pin}
 if Pin = nil then Exit;
 
 {Check Callback}
 if not Assigned(Callback) then Exit;
 
 {Create Event}
 Result:=PGPIOEvent(GetMem(SizeOf(TGPIOEvent)));
 if Result = nil then Exit;
 
 {Update Event}
 Result.Pin:=Pin;
 Result.Callback:=Callback;
 Result.Data:=Data;
 Result.Timeout:=Timeout;
 Result.Prev:=nil;
 Result.Next:=nil;
end;

{==============================================================================}

function GPIODeviceDestroyEvent(GPIO:PGPIODevice;Event:PGPIOEvent):LongWord;
{Destroy an existing event}

{Note: Event must be deregistered first by calling GPIODeviceDeregisterEvent}
{Note: Caller must hold the GPIO device lock}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check GPIO}
 if GPIO = nil then Exit;
 if GPIO.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Event}
 if Event = nil then Exit;

 {Destroy Event}
 FreeMem(Event);
 
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function GPIODeviceRegisterEvent(GPIO:PGPIODevice;Pin:PGPIOPin;Event:PGPIOEvent):LongWord;
{Register an event in the event list of the supplied Pin}

{Note: Event must be created by calling GPIODeviceCreateEvent}
{Note: Caller must hold the GPIO device lock}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check GPIO}
 if GPIO = nil then Exit;
 if GPIO.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Pin}
 if Pin = nil then Exit;
 
 {Check Event}
 if Event = nil then Exit;
 
 {Link Event}
 if Pin.Events = nil then
  begin
   Pin.Events:=Event;
  end
 else
  begin
   Event.Next:=Pin.Events;
   Pin.Events.Prev:=Event;
   Pin.Events:=Event;
  end;
  
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function GPIODeviceDeregisterEvent(GPIO:PGPIODevice;Pin:PGPIOPin;Event:PGPIOEvent):LongWord;
{Deregister an event from the event list of the supplied Pin}

{Note: Event must be destroyed by calling GPIODeviceDestroyEvent}
{Note: Caller must hold the GPIO device lock}
var
 Prev:PGPIOEvent;
 Next:PGPIOEvent;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check GPIO}
 if GPIO = nil then Exit;
 if GPIO.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Pin}
 if Pin = nil then Exit;

 {Check Event}
 if Event = nil then Exit;
 
 {Unlink Event}
 Prev:=Event.Prev;
 Next:=Event.Next;
 if Prev = nil then
  begin
   Pin.Events:=Next;
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
 
 {Update Event}
 Event.Prev:=nil;
 Event.Next:=nil;
 
 Result:=ERROR_SUCCESS;
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

function GPIOPinToString(Pin:LongWord):String;
begin
 {}
 Result:='GPIO_PIN_UNKNOWN';
 
 if Pin > GPIO_PIN_MAX then Exit;
 
 Result:='GPIO_PIN_' + IntToStr(Pin);
end;

{==============================================================================}

function GPIOLevelToString(Level:LongWord):String;
begin
 {}
 Result:='GPIO_LEVEL_UNKNOWN';
 
 case Level of
  GPIO_LEVEL_LOW:Result:='GPIO_LEVEL_LOW';
  GPIO_LEVEL_HIGH:Result:='GPIO_LEVEL_HIGH';
 end;
end;

{==============================================================================}

function GPIOTriggerToString(Trigger:LongWord):String;
begin
 {}
 Result:='GPIO_TRIGGER_UNKNOWN';
 
 case Trigger of
  GPIO_TRIGGER_NONE:Result:='GPIO_TRIGGER_NONE';
  GPIO_TRIGGER_LOW:Result:='GPIO_TRIGGER_LOW';
  GPIO_TRIGGER_HIGH:Result:='GPIO_TRIGGER_HIGH';
  GPIO_TRIGGER_RISING:Result:='GPIO_TRIGGER_RISING';
  GPIO_TRIGGER_FALLING:Result:='GPIO_TRIGGER_FALLING'; 
  GPIO_TRIGGER_ASYNC_RISING:Result:='GPIO_TRIGGER_ASYNC_RISING'; 
  GPIO_TRIGGER_ASYNC_FALLING:Result:='GPIO_TRIGGER_ASYNC_FALLING';
  GPIO_TRIGGER_EDGE:Result:='GPIO_TRIGGER_EDGE';
 end;
end;

{==============================================================================}

function GPIOPullToString(Value:LongWord):String;
begin
 {}
 Result:='GPIO_PULL_UNKNOWN';
 
 case Value of
  GPIO_PULL_NONE:Result:='GPIO_PULL_NONE';
  GPIO_PULL_UP:Result:='GPIO_PULL_UP';
  GPIO_PULL_DOWN:Result:='GPIO_PULL_DOWN';
 end;
end;

{==============================================================================}

function GPIOFunctionToString(Value:LongWord):String;
begin
 {}
 Result:='GPIO_FUNCTION_UNKNOWN';
 
 case Value of
  GPIO_FUNCTION_IN:Result:='GPIO_FUNCTION_IN';
  GPIO_FUNCTION_OUT:Result:='GPIO_FUNCTION_OUT';
  GPIO_FUNCTION_ALT0:Result:='GPIO_FUNCTION_ALT0';
  GPIO_FUNCTION_ALT1:Result:='GPIO_FUNCTION_ALT1';
  GPIO_FUNCTION_ALT2:Result:='GPIO_FUNCTION_ALT2';
  GPIO_FUNCTION_ALT3:Result:='GPIO_FUNCTION_ALT3';
  GPIO_FUNCTION_ALT4:Result:='GPIO_FUNCTION_ALT4';
  GPIO_FUNCTION_ALT5:Result:='GPIO_FUNCTION_ALT5';
 end;
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
