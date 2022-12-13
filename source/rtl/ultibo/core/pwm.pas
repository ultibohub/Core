{
Ultibo PWM interface unit.

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

 PWM - https://en.wikipedia.org/wiki/Pulse-width_modulation

PWM Hosts
=========

 Pulse Width Modulation (PWM) is a technique of encoding a pulsed signal so as to control the ratio
 of on to off by switching the signal between on and off at a very high rate. 
 
 This on and off switching allows control of the average power supplied to the load and therefore 
 gives fine grained control of things such as motor speeds, lamp dimming and heating elements.
 
 PWM is also used in many low power applications as well such as controlling servo motors and can
 even by used for audio applications such as class D amplifiers.
 
 This unit deals with the usage of PWM for control applications (for audio applications see the Audio
 unit) and provides methods to control the state, frequency, duty cycle, range and mode of PWM host
 controllers. Not all devices support all of these concepts so this API includes a properties function
 to allow obtaining information about a PWM device and its capabilities.
 
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
 
 PWM_TYPE_MAX       = 0;
  
 {PWM Type Names}
 PWM_TYPE_NAMES:array[PWM_TYPE_NONE..PWM_TYPE_MAX] of String = (
  'PWM_TYPE_NONE');
 
 {PWM Device States}
 PWM_STATE_DISABLED = 0;
 PWM_STATE_ENABLED  = 1;
 
 PWM_STATE_MAX      = 1;
 
 {PWM State Names}
 PWM_STATE_NAMES:array[PWM_STATE_DISABLED..PWM_STATE_MAX] of String = (
  'PWM_STATE_DISABLED',
  'PWM_STATE_ENABLED');
 
 {PWM Device Flags}
 PWM_FLAG_NONE          = $00000000;
 PWM_FLAG_GPIO          = $00000001; {Device supports Get/Set GPIO}
 PWM_FLAG_MODE          = $00000002; {Device supports Get/Set Mode}
 PWM_FLAG_RANGE         = $00000004; {Device supports Get/Set Range}
 PWM_FLAG_FREQUENCY     = $00000008; {Device supports Get/Set Frequency}
 PWM_FLAG_POLARITY      = $00000010; {Device supports Get/Set Polarity}
 
 {PWM Mode Values}
 PWM_MODE_MARKSPACE  = 0; {Standard PWM Mark / Space mode}
 PWM_MODE_BALANCED   = 1; {Balanced mode (Device specific)}
 PWM_MODE_SERIALIZED = 2; {Serialized mode (Device specific)}
 
 PWM_MODE_MAX        = 2;
 
 {PWM Mode Names}
 PWM_MODE_NAMES:array[PWM_MODE_MARKSPACE..PWM_MODE_MAX] of String = (
  'PWM_MODE_MARKSPACE',
  'PWM_MODE_BALANCED',
  'PWM_MODE_SERIALIZED');
 
 {PWM Polarity Values}
 PWM_POLARITY_NORMAL  = 0;
 PWM_POLARITY_INVERSE = 1;
 
 PWM_POLARITY_MAX     = 1;
  
 {PWM Polarity Names}
 PWM_POLARITY_NAMES:array[PWM_POLARITY_NORMAL..PWM_POLARITY_MAX] of String = (
  'PWM_POLARITY_NORMAL',
  'PWM_POLARITY_INVERSE');
 
 {PWM logging}
 PWM_LOG_LEVEL_DEBUG     = LOG_LEVEL_DEBUG;  {PWM debugging messages}
 PWM_LOG_LEVEL_INFO      = LOG_LEVEL_INFO;   {PWM informational messages, such as a device being attached or detached}
 PWM_LOG_LEVEL_WARN      = LOG_LEVEL_WARN;   {PWM warning messages}
 PWM_LOG_LEVEL_ERROR     = LOG_LEVEL_ERROR;  {PWM error messages}
 PWM_LOG_LEVEL_NONE      = LOG_LEVEL_NONE;   {No PWM messages}

var 
 PWM_DEFAULT_LOG_LEVEL:LongWord = PWM_LOG_LEVEL_DEBUG; {Minimum level for PWM messages.  Only messages with level greater than or equal to this will be printed}
 
var 
 {PWM logging}
 PWM_LOG_ENABLED:Boolean; 
 
{==============================================================================}
type
 {PWM specific types}

 {PWM Properties}
 PPWMProperties = ^TPWMProperties;
 TPWMProperties = record
  Flags:LongWord;        {Device flags (eg PWM_FLAG_MODE)}
  GPIO:LongWord;
  Mode:LongWord;
  Range:LongWord;
  Frequency:LongWord;
  Polarity:LongWord;
  DutyNS:LongWord;
  PeriodNS:LongWord;
  MinPeriod:LongWord;
 end;
 
 {PWM Device}
 PPWMDevice = ^TPWMDevice;
 
 {PWM Enumeration Callback}
 TPWMEnumerate = function(PWM:PPWMDevice;Data:Pointer):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 {PWM Notification Callback}
 TPWMNotification = function(Device:PDevice;Data:Pointer;Notification:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 
 {PWM Device Methods}
 TPWMDeviceStart = function(PWM:PPWMDevice):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TPWMDeviceStop = function(PWM:PPWMDevice):LongWord;{$IFDEF i386} stdcall;{$ENDIF}

 TPWMDeviceWrite = function(PWM:PPWMDevice;Value:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF} 
 
 TPWMDeviceGetGPIO = function(PWM:PPWMDevice):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TPWMDeviceSetGPIO = function(PWM:PPWMDevice;GPIO:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TPWMDeviceGetMode = function(PWM:PPWMDevice):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TPWMDeviceSetMode = function(PWM:PPWMDevice;Mode:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TPWMDeviceGetRange = function(PWM:PPWMDevice):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TPWMDeviceSetRange = function(PWM:PPWMDevice;Range:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TPWMDeviceGetFrequency = function(PWM:PPWMDevice):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TPWMDeviceSetFrequency = function(PWM:PPWMDevice;Frequency:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TPWMDeviceGetPolarity = function(PWM:PPWMDevice):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TPWMDeviceSetPolarity = function(PWM:PPWMDevice;Polarity:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
  
 TPWMDeviceConfigure = function(PWM:PPWMDevice;DutyNS,PeriodNS:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 
 TPWMDeviceGetProperties = function(PWM:PPWMDevice;Properties:PPWMProperties):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 
 TPWMDevice = record
  {Device Properties}
  Device:TDevice;                                 {The Device entry for this PWM}
  {PWM Properties}
  PWMId:LongWord;                                 {Unique Id of this PWM in the PWM table}
  PWMState:LongWord;                              {PWM state (eg PWM_STATE_ENABLED)}
  DeviceStart:TPWMDeviceStart;                    {A Device specific DeviceStart method implementing the standard PWM device interface (Mandatory)}
  DeviceStop:TPWMDeviceStop;                      {A Device specific DeviceStop method implementing the standard PWM device interface (Mandatory)}
  DeviceWrite:TPWMDeviceWrite;                    {A Device specific DeviceWrite method implementing the standard PWM device interface (Mandatory)}
  DeviceGetGPIO:TPWMDeviceGetGPIO;                {A Device specific DeviceGetGPIO method implementing the standard PWM device interface (Or nil if the operation is not supported)}
  DeviceSetGPIO:TPWMDeviceSetGPIO;                {A Device specific DeviceSetGPIO method implementing the standard PWM device interface (Or nil if the operation is not supported)}
  DeviceGetMode:TPWMDeviceGetMode;                {A Device specific DeviceGetMode method implementing the standard PWM device interface (Or nil if the operation is not supported)}
  DeviceSetMode:TPWMDeviceSetMode;                {A Device specific DeviceSetMode method implementing the standard PWM device interface (Or nil if the operation is not supported)}
  DeviceGetRange:TPWMDeviceGetRange;              {A Device specific DeviceGetRange method implementing the standard PWM device interface (Or nil if the operation is not supported)}
  DeviceSetRange:TPWMDeviceSetRange;              {A Device specific DeviceSetRange method implementing the standard PWM device interface (Or nil if the operation is not supported)}
  DeviceGetFrequency:TPWMDeviceGetFrequency;      {A Device specific DeviceGetFrequency method implementing the standard PWM device interface (Or nil if the operation is not supported)}
  DeviceSetFrequency:TPWMDeviceSetFrequency;      {A Device specific DeviceSetFrequency method implementing the standard PWM device interface (Or nil if the operation is not supported)}
  DeviceGetPolarity:TPWMDeviceGetPolarity;        {A Device specific DeviceGetPolarity method implementing the standard PWM device interface (Or nil if the operation is not supported)}
  DeviceSetPolarity:TPWMDeviceSetPolarity;        {A Device specific DeviceSetPolarity method implementing the standard PWM device interface (Or nil if the operation is not supported)}
  DeviceConfigure:TPWMDeviceConfigure;            {A Device specific DeviceConfigure method implementing the standard PWM device interface (Mandatory)}
  DeviceGetProperties:TPWMDeviceGetProperties;    {A Device specific DeviceGetProperties method implementing the standard PWM device interface (Or nil if the default method is suitable)}
  {Statistics Properties}
  GetCount:LongWord;
  SetCount:LongWord;
  WriteCount:LongWord;
  ConfigCount:LongWord;
  {Driver Properties}
  Lock:TMutexHandle;                              {Device lock}
  GPIO:LongWord;                                  {GPIO Pin}
  Mode:LongWord;                                  {Device Mode}
  Range:LongWord;                                 {Device Range}
  Frequency:LongWord;                             {Clock Frequency}
  Polarity:LongWord;                              {Output Polarity}
  DutyNS:LongWord;                                {Duty Nanoseconds}
  PeriodNS:LongWord;                              {Period Nanonseconds}
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
function PWMDeviceStart(PWM:PPWMDevice):LongWord; 
function PWMDeviceStop(PWM:PPWMDevice):LongWord; 

function PWMDeviceWrite(PWM:PPWMDevice;Value:LongWord):LongWord; 
 
function PWMDeviceGetGPIO(PWM:PPWMDevice):LongWord;
function PWMDeviceSetGPIO(PWM:PPWMDevice;GPIO:LongWord):LongWord;
function PWMDeviceGetMode(PWM:PPWMDevice):LongWord;
function PWMDeviceSetMode(PWM:PPWMDevice;Mode:LongWord):LongWord;
function PWMDeviceGetRange(PWM:PPWMDevice):LongWord;
function PWMDeviceSetRange(PWM:PPWMDevice;Range:LongWord):LongWord;
function PWMDeviceGetFrequency(PWM:PPWMDevice):LongWord;
function PWMDeviceSetFrequency(PWM:PPWMDevice;Frequency:LongWord):LongWord;
function PWMDeviceGetPolarity(PWM:PPWMDevice):LongWord;
function PWMDeviceSetPolarity(PWM:PPWMDevice;Polarity:LongWord):LongWord;

function PWMDeviceConfigure(PWM:PPWMDevice;DutyNS,PeriodNS:LongWord):LongWord;
 
function PWMDeviceProperties(PWM:PPWMDevice;Properties:PPWMProperties):LongWord; inline;
function PWMDeviceGetProperties(PWM:PPWMDevice;Properties:PPWMProperties):LongWord;
  
function PWMDeviceCreate:PPWMDevice;
function PWMDeviceCreateEx(Size:LongWord):PPWMDevice;
function PWMDeviceDestroy(PWM:PPWMDevice):LongWord;

function PWMDeviceRegister(PWM:PPWMDevice):LongWord;
function PWMDeviceDeregister(PWM:PPWMDevice):LongWord;

function PWMDeviceFind(PWMId:LongWord):PPWMDevice;
function PWMDeviceFindByName(const Name:String):PPWMDevice; inline;
function PWMDeviceFindByDescription(const Description:String):PPWMDevice; inline;
function PWMDeviceEnumerate(Callback:TPWMEnumerate;Data:Pointer):LongWord;
 
function PWMDeviceNotification(PWM:PPWMDevice;Callback:TPWMNotification;Data:Pointer;Notification,Flags:LongWord):LongWord;
 
{==============================================================================}
{RTL PWM Functions}
function SysPWMAvailable:Boolean; 
 
function SysPWMStart:LongWord; 
function SysPWMStop:LongWord; 
 
function SysPWMWrite(Value:LongWord):LongWord; 
 
function SysPWMSetMode(Mode:LongWord):LongWord; 
function SysPWMSetRange(Range:LongWord):LongWord; 
function SysPWMSetFrequency(Frequency:LongWord):LongWord; 
 
function SysPWMConfigure(DutyNS,PeriodNS:LongWord):LongWord; 

{==============================================================================}
{PWM Helper Functions}
function PWMGetCount:LongWord;
function PWMDeviceGetDefault:PPWMDevice;
function PWMDeviceSetDefault(PWM:PPWMDevice):LongWord; 

function PWMDeviceCheck(PWM:PPWMDevice):PPWMDevice;

function PWMTypeToString(PWMType:LongWord):String;
function PWMStateToString(PWMState:LongWord):String;
function PWMModeToString(PWMMode:LongWord):String;
function PWMPolarityToString(PWMPolarity:LongWord):String;

procedure PWMLog(Level:LongWord;PWM:PPWMDevice;const AText:String);
procedure PWMLogInfo(PWM:PPWMDevice;const AText:String); inline;
procedure PWMLogWarn(PWM:PPWMDevice;const AText:String); inline;
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
{Initialize the PWM unit and PWM device table}

{Note: Called only during system startup}
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
 PWMAvailableHandler:=SysPWMAvailable;
 PWMStartHandler:=SysPWMStart;
 PWMStopHandler:=SysPWMStop;
 PWMWriteHandler:=SysPWMWrite;
 PWMSetModeHandler:=SysPWMSetMode;
 PWMSetRangeHandler:=SysPWMSetRange;
 PWMSetFrequencyHandler:=SysPWMSetFrequency;
 PWMConfigureHandler:=SysPWMConfigure;
 
 PWMInitialized:=True;
end;
 
{==============================================================================}
{==============================================================================}
{PWM Functions}
function PWMDeviceStart(PWM:PPWMDevice):LongWord; 
{Start the specified PWM device}
{PWM: The PWM device to start}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check PWM}
 if PWM = nil then Exit;
 if PWM.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IFDEF PWM_DEBUG}
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'PWM Device Start');
 {$ENDIF}
 
 {Check Disabled}
 Result:=ERROR_SUCCESS;
 if PWM.PWMState <> PWM_STATE_DISABLED then Exit;
 
 if MutexLock(PWM.Lock) = ERROR_SUCCESS then
  begin
   try
    if Assigned(PWM.DeviceStart) then
     begin
      {Call Device Start}
      Result:=PWM.DeviceStart(PWM);
      if Result <> ERROR_SUCCESS then Exit;
     end
    else
     begin
      Result:=ERROR_INVALID_PARAMETER;
      Exit;
     end;
     
    {Enable Device}
    PWM.PWMState:=PWM_STATE_ENABLED;
    
    {Notify Enable}
    NotifierNotify(@PWM.Device,DEVICE_NOTIFICATION_ENABLE);
    
    Result:=ERROR_SUCCESS;
   finally
    MutexUnlock(PWM.Lock);
   end; 
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;    
end;

{==============================================================================}

function PWMDeviceStop(PWM:PPWMDevice):LongWord; 
{Stop the specified PWM device}
{PWM: The PWM device to stop}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check PWM}
 if PWM = nil then Exit;
 if PWM.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IFDEF PWM_DEBUG}
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'PWM Device Stop');
 {$ENDIF}
 
 {Check Enabled}
 Result:=ERROR_SUCCESS;
 if PWM.PWMState <> PWM_STATE_ENABLED then Exit;
 
 if MutexLock(PWM.Lock) = ERROR_SUCCESS then
  begin
   try
    if Assigned(PWM.DeviceStop) then
     begin
      {Call Device Stop}
      Result:=PWM.DeviceStop(PWM);
      if Result <> ERROR_SUCCESS then Exit;
     end
    else
     begin
      Result:=ERROR_INVALID_PARAMETER;
      Exit;
     end;    
  
    {Disable Device}
    PWM.PWMState:=PWM_STATE_DISABLED;
    
    {Notify Disable}
    NotifierNotify(@PWM.Device,DEVICE_NOTIFICATION_DISABLE);
    
    Result:=ERROR_SUCCESS;
   finally
    MutexUnlock(PWM.Lock);
   end; 
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;    
end;

{==============================================================================}

function PWMDeviceWrite(PWM:PPWMDevice;Value:LongWord):LongWord; 
{Write a value to the specified PWM device}
{PWM: The PWM device to write to}
{Value: The value to write}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: The exact meaning of value may depend on the device and other configured options,
       in many cases the value will represent the "on" time of each pulse with regard to 
       the duty cycle of the waveform output by the device}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check PWM}
 if PWM = nil then Exit;
 if PWM.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IFDEF PWM_DEBUG}
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'PWM Device Write (Value=' + IntToHex(Value,4) + ')');
 {$ENDIF}
 
 {Check Enabled}
 Result:=ERROR_NOT_SUPPORTED;
 if PWM.PWMState <> PWM_STATE_ENABLED then Exit;
 
 if MutexLock(PWM.Lock) = ERROR_SUCCESS then
  begin
   if Assigned(PWM.DeviceWrite) then
    begin
     {Call Device Write}
     Result:=PWM.DeviceWrite(PWM,Value);
    end
   else
    begin
     Result:=ERROR_INVALID_PARAMETER;
    end;    
    
   MutexUnlock(PWM.Lock);
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;    
end;

{==============================================================================}
 
function PWMDeviceGetGPIO(PWM:PPWMDevice):LongWord;
{Get the GPIO pin used by the specified PWM device}
{PWM: The PWM device to get the GPIO pin from}
{Return: The current GPIO pin or GPIO_PIN_UNKNOWN on failure}
begin
 {}
 Result:=GPIO_PIN_UNKNOWN;
 
 {Check PWM}
 if PWM = nil then Exit;
 if PWM.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IFDEF PWM_DEBUG}
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'PWM Device Get GPIO');
 {$ENDIF}
 
 {Check Enabled}
 {if PWM.PWMState <> PWM_STATE_ENABLED then Exit;} {Allow when disabled}
 
 if MutexLock(PWM.Lock) = ERROR_SUCCESS then
  begin
   if Assigned(PWM.DeviceGetGPIO) then
    begin
     {Call Device Get GPIO}
     Result:=PWM.DeviceGetGPIO(PWM);
    end
   else
    begin
     {Get GPIO}
     Result:=PWM.GPIO;
    end;    
    
   MutexUnlock(PWM.Lock);
  end;
end;

{==============================================================================}

function PWMDeviceSetGPIO(PWM:PPWMDevice;GPIO:LongWord):LongWord;
{Set the GPIO pin used by the specified PWM device}
{PWM: The PWM device to set the GPIO pin for}
{GPIO: The GPIO pin to set (eg GPIO_PIN_12)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check PWM}
 if PWM = nil then Exit;
 if PWM.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IFDEF PWM_DEBUG}
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'PWM Device Set GPIO (GPIO=' + IntToStr(GPIO) + ')');
 {$ENDIF}
 
 {Check Enabled}
 {Result:=ERROR_NOT_SUPPORTED;}
 {if PWM.PWMState <> PWM_STATE_ENABLED then Exit;} {Allow when disabled}
 
 if MutexLock(PWM.Lock) = ERROR_SUCCESS then
  begin
   try
    if Assigned(PWM.DeviceSetGPIO) then
     begin
      {Call Device Set GPIO}
      Result:=PWM.DeviceSetGPIO(PWM,GPIO);
     end;
   finally  
    MutexUnlock(PWM.Lock);
   end; 
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function PWMDeviceGetMode(PWM:PPWMDevice):LongWord;
{Get the current mode of the specified PWM device}
{PWM: The PWM device to get the mode from}
{Return: The current mode or a default value of PWM_MODE_MARKSPACE}
begin
 {}
 Result:=PWM_MODE_MARKSPACE;
 
 {Check PWM}
 if PWM = nil then Exit;
 if PWM.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IFDEF PWM_DEBUG}
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'PWM Device Get Mode');
 {$ENDIF}
 
 {Check Enabled}
 {if PWM.PWMState <> PWM_STATE_ENABLED then Exit;} {Allow when disabled}
 
 if MutexLock(PWM.Lock) = ERROR_SUCCESS then
  begin
   if Assigned(PWM.DeviceGetMode) then
    begin
     {Call Device Get Mode}
     Result:=PWM.DeviceGetMode(PWM);
    end
   else
    begin
     {Get Mode}
     Result:=PWM.Mode;
    end;  
    
   MutexUnlock(PWM.Lock);
  end;
end;

{==============================================================================}

function PWMDeviceSetMode(PWM:PPWMDevice;Mode:LongWord):LongWord;
{Set the current mode for the specified PWM device}
{PWM: The PWM device to set the mode for}
{Mode: The mode value to set (eg PWM_MODE_MARKSPACE)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check PWM}
 if PWM = nil then Exit;
 if PWM.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IFDEF PWM_DEBUG}
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'PWM Device Set Mode (Mode=' + IntToStr(Mode) + ')');
 {$ENDIF}
 
 {Check Enabled}
 {Result:=ERROR_NOT_SUPPORTED;}
 {if PWM.PWMState <> PWM_STATE_ENABLED then Exit;} {Allow when disabled}
 
 if MutexLock(PWM.Lock) = ERROR_SUCCESS then
  begin
   try
    if Assigned(PWM.DeviceSetMode) then
     begin
      {Call Device Set Mode}
      Result:=PWM.DeviceSetMode(PWM,Mode);
     end;
   finally  
    MutexUnlock(PWM.Lock);
   end; 
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function PWMDeviceGetRange(PWM:PPWMDevice):LongWord;
{Get the current range of the specified PWM device}
{PWM: The PWM device to get the range from}
{Return: The current range or 0 on failure}
begin
 {}
 Result:=0;
 
 {Check PWM}
 if PWM = nil then Exit;
 if PWM.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IFDEF PWM_DEBUG}
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'PWM Device Get Range');
 {$ENDIF}
 
 {Check Enabled}
 {if PWM.PWMState <> PWM_STATE_ENABLED then Exit;} {Allow when disabled}
 
 if MutexLock(PWM.Lock) = ERROR_SUCCESS then
  begin
   if Assigned(PWM.DeviceGetRange) then
    begin
     {Call Device Get Range}
     Result:=PWM.DeviceGetRange(PWM);
    end
   else
    begin
     {Get Range}
     Result:=PWM.Range;
    end;  
    
   MutexUnlock(PWM.Lock);
  end;
end;

{==============================================================================}

function PWMDeviceSetRange(PWM:PPWMDevice;Range:LongWord):LongWord;
{Set the current range for the specified PWM device}
{PWM: The PWM device to set the clock rate for}
{Range: The range value to set}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: The exact meaning of range may depend on the device and other configured options,
       in many cases the range will represent the period of one full cycle of the 
       waveform output by the device}   
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check PWM}
 if PWM = nil then Exit;
 if PWM.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IFDEF PWM_DEBUG}
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'PWM Device Set Range (Range=' + IntToStr(Range) + ')');
 {$ENDIF}
 
 {Check Enabled}
 {Result:=ERROR_NOT_SUPPORTED;}
 {if PWM.PWMState <> PWM_STATE_ENABLED then Exit;} {Allow when disabled}
 
 if MutexLock(PWM.Lock) = ERROR_SUCCESS then
  begin
   try
    if Assigned(PWM.DeviceSetRange) then
     begin
      {Call Device Set Range}
      Result:=PWM.DeviceSetRange(PWM,Range);
     end;
   finally  
    MutexUnlock(PWM.Lock);
   end; 
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function PWMDeviceGetFrequency(PWM:PPWMDevice):LongWord;
{Get the clock frequency of the specified PWM device}
{PWM: The PWM device to get the clock frequency from}
{Return: The clock frequency in Hz or 0 on failure}
begin
 {}
 Result:=0;
 
 {Check PWM}
 if PWM = nil then Exit;
 if PWM.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IFDEF PWM_DEBUG}
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'PWM Device Get Frequency');
 {$ENDIF}
 
 {Check Enabled}
 {if PWM.PWMState <> PWM_STATE_ENABLED then Exit;} {Allow when disabled}
 
 if MutexLock(PWM.Lock) = ERROR_SUCCESS then
  begin
   if Assigned(PWM.DeviceGetFrequency) then
    begin
     {Call Device Get Frequency}
     Result:=PWM.DeviceGetFrequency(PWM);
    end
   else
    begin
     {Get Frequency}
     Result:=PWM.Frequency;
    end;  
    
   MutexUnlock(PWM.Lock);
  end;
end;

{==============================================================================}

function PWMDeviceSetFrequency(PWM:PPWMDevice;Frequency:LongWord):LongWord;
{Set the clock frequency for the specified PWM device}
{PWM: The PWM device to set the clock frequency for}
{Frequency: The clock frequency to set in Hz}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=0;
 
 {Check PWM}
 if PWM = nil then Exit;
 if PWM.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IFDEF PWM_DEBUG}
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'PWM Device Set Frequency (Frequency=' + IntToStr(Frequency) + ')');
 {$ENDIF}
 
 {Check Enabled}
 {Result:=ERROR_NOT_SUPPORTED;}
 {if PWM.PWMState <> PWM_STATE_ENABLED then Exit;} {Allow when disabled}
 
 if MutexLock(PWM.Lock) = ERROR_SUCCESS then
  begin
   try
    if Assigned(PWM.DeviceSetFrequency) then
     begin
      {Call Device Set Frequency}
      Result:=PWM.DeviceSetFrequency(PWM,Frequency);
     end;
   finally  
    MutexUnlock(PWM.Lock);
   end; 
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function PWMDeviceGetPolarity(PWM:PPWMDevice):LongWord;
{Get the current polarity of the specified PWM device}
{PWM: The PWM device to get the polarity from}
{Return: The current polarity or a default value of PWM_POLARITY_NORMAL}
begin
 {}
 Result:=PWM_POLARITY_NORMAL;
 
 {Check PWM}
 if PWM = nil then Exit;
 if PWM.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IFDEF PWM_DEBUG}
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'PWM Device Get Polarity');
 {$ENDIF}
 
 {Check Enabled}
 {if PWM.PWMState <> PWM_STATE_ENABLED then Exit;} {Allow when disabled}
 
 if MutexLock(PWM.Lock) = ERROR_SUCCESS then
  begin
   if Assigned(PWM.DeviceGetPolarity) then
    begin
     {Call Device Get Polarity}
     Result:=PWM.DeviceGetPolarity(PWM);
    end
   else
    begin
     {Get Polarity}
     Result:=PWM.Polarity;
    end;  
    
   MutexUnlock(PWM.Lock);
  end;
end;

{==============================================================================}

function PWMDeviceSetPolarity(PWM:PPWMDevice;Polarity:LongWord):LongWord;
{Set the current polarity for the specified PWM device}
{PWM: The PWM device to set the polarity for}
{Polarity: The polarity value to set (eg PWM_POLARITY_NORMAL)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check PWM}
 if PWM = nil then Exit;
 if PWM.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IFDEF PWM_DEBUG}
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'PWM Device Set Polarity (Polarity=' + IntToStr(Polarity) + ')');
 {$ENDIF}
 
 {Check Enabled}
 {Result:=ERROR_NOT_SUPPORTED;}
 {if PWM.PWMState <> PWM_STATE_ENABLED then Exit;} {Allow when disabled}
 
 if MutexLock(PWM.Lock) = ERROR_SUCCESS then
  begin
   try
    if Assigned(PWM.DeviceSetPolarity) then
     begin
      {Call Device Set Polarity}
      Result:=PWM.DeviceSetPolarity(PWM,Polarity);
     end;
   finally  
    MutexUnlock(PWM.Lock);
   end; 
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function PWMDeviceConfigure(PWM:PPWMDevice;DutyNS,PeriodNS:LongWord):LongWord;
{Set the configuration of the specified PWM device}
{PWM: The PWM device to set the configuration for}
{DutyNS: The "on" time part of the cycle (Nanoseconds)}
{PeriodNS: The duration of one full cycle (Nanoseconds)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check PWM}
 if PWM = nil then Exit;
 if PWM.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IFDEF PWM_DEBUG}
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'PWM Device Configure (DutyNS=' + IntToStr(DutyNS) + ' PeriodNS=' + IntToStr(PeriodNS) + ')');
 {$ENDIF}
 
 {Check Enabled}
 {Result:=ERROR_NOT_SUPPORTED;}
 {if PWM.PWMState <> PWM_STATE_ENABLED then Exit;} {Allow when disabled}
 
 if MutexLock(PWM.Lock) = ERROR_SUCCESS then
  begin
   try
    if Assigned(PWM.DeviceConfigure) then
     begin
      {Call Device Configure}
      Result:=PWM.DeviceConfigure(PWM,DutyNS,PeriodNS);
     end;
   finally  
    MutexUnlock(PWM.Lock);
   end; 
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}
 
function PWMDeviceProperties(PWM:PPWMDevice;Properties:PPWMProperties):LongWord; inline;
{Get the properties for the specified PWM device}
{PWM: The PWM device to get properties from}
{Properties: Pointer to a TPWMProperties structure to fill in}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: Replaced by PWMDeviceGetProperties for consistency}
begin
 {}
 Result:=PWMDeviceGetProperties(PWM,Properties);
end;

{==============================================================================}

function PWMDeviceGetProperties(PWM:PPWMDevice;Properties:PPWMProperties):LongWord;
{Get the properties for the specified PWM device}
{PWM: The PWM device to get properties from}
{Properties: Pointer to a TPWMProperties structure to fill in}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Properties}
 if Properties = nil then Exit;
 
 {Check PWM}
 if PWM = nil then Exit;
 if PWM.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IFDEF PWM_DEBUG}
 if PWM_LOG_ENABLED then PWMLogDebug(PWM,'PWM Device Get Properties');
 {$ENDIF}
 
 {Check Enabled}
 {Result:=ERROR_NOT_SUPPORTED;}
 {if PWM.PWMState <> PWM_STATE_ENABLED then Exit;} {Allow when disabled}
 
 if MutexLock(PWM.Lock) = ERROR_SUCCESS then
  begin
   if Assigned(PWM.DeviceGetProperties) then
    begin
     {Call Device Get Properites}
     Result:=PWM.DeviceGetProperties(PWM,Properties);
    end
   else
    begin
     {Get Properties}
     System.Move(PWM.Properties,Properties^,SizeOf(TPWMProperties));
       
     {Return Result}
     Result:=ERROR_SUCCESS;
    end;  
    
   MutexUnlock(PWM.Lock);
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;    
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
 Result.DeviceStart:=nil;
 Result.DeviceStop:=nil;
 Result.DeviceWrite:=nil;
 Result.DeviceGetGPIO:=nil;
 Result.DeviceSetGPIO:=nil;
 Result.DeviceGetMode:=nil;
 Result.DeviceSetMode:=nil;
 Result.DeviceGetRange:=nil;
 Result.DeviceSetRange:=nil;
 Result.DeviceGetFrequency:=nil;
 Result.DeviceSetFrequency:=nil;
 Result.DeviceGetPolarity:=nil;
 Result.DeviceSetPolarity:=nil;
 Result.DeviceConfigure:=nil;
 Result.DeviceGetProperties:=nil;
 Result.GPIO:=GPIO_PIN_UNKNOWN;
 Result.Mode:=PWM_MODE_MARKSPACE;
 Result.Range:=0;
 Result.Frequency:=0;
 Result.Polarity:=PWM_POLARITY_NORMAL;
 Result.DutyNS:=0;
 Result.PeriodNS:=0;
 Result.Lock:=INVALID_HANDLE_VALUE;
 
 {Create Lock}
 Result.Lock:=MutexCreateEx(False,MUTEX_DEFAULT_SPINCOUNT,MUTEX_FLAG_RECURSIVE);
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
 
 {Check Interfaces}
 if not(Assigned(PWM.DeviceStart)) then Exit;
 if not(Assigned(PWM.DeviceStop)) then Exit;
 if not(Assigned(PWM.DeviceWrite)) then Exit;
 if not(Assigned(PWM.DeviceConfigure)) then Exit;
 
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

function PWMDeviceFindByName(const Name:String):PPWMDevice; inline;
begin
 {}
 Result:=PPWMDevice(DeviceFindByName(Name));
end;

{==============================================================================}

function PWMDeviceFindByDescription(const Description:String):PPWMDevice; inline;
begin
 {}
 Result:=PPWMDevice(DeviceFindByDescription(Description));
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
function SysPWMAvailable:Boolean; 
{Check if a PWM device is available}
begin
 {}
 Result:=(PWMDeviceDefault <> nil);
end;

{==============================================================================}

function SysPWMStart:LongWord; 
{Start the default PWM device}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 if PWMDeviceDefault = nil then Exit;
 
 Result:=PWMDeviceStart(PWMDeviceDefault);
end;

{==============================================================================}

function SysPWMStop:LongWord; 
{Stop the default PWM device}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 if PWMDeviceDefault = nil then Exit;
 
 Result:=PWMDeviceStop(PWMDeviceDefault);
end;

{==============================================================================}
 
function SysPWMWrite(Value:LongWord):LongWord; 
{Write a value to the default PWM device}
{Value: The value to write}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: The exact meaning of value may depend on the device and other configured options,
       in many cases the value will represent the "on" time of each pulse with regard to 
       the duty cycle of the waveform output by the device}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 if PWMDeviceDefault = nil then Exit;
 
 Result:=PWMDeviceWrite(PWMDeviceDefault,Value);
end;

{==============================================================================}
 
function SysPWMSetMode(Mode:LongWord):LongWord; 
{Set the mode for the default PWM device}
{Mode: The mode value to set (eg PWM_MODE_MARKSPACE)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 if PWMDeviceDefault = nil then Exit;
 
 Result:=PWMDeviceSetMode(PWMDeviceDefault,Mode);
end;

{==============================================================================}

function SysPWMSetRange(Range:LongWord):LongWord; 
{Set the range for the default PWM device}
{Range: The range value to set}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: The exact meaning of range may depend on the device and other configured options,
       in many cases the range will represent the period of one full cycle of the 
       waveform output by the device}   
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 if PWMDeviceDefault = nil then Exit;
 
 Result:=PWMDeviceSetRange(PWMDeviceDefault,Range);
end;

{==============================================================================}

function SysPWMSetFrequency(Frequency:LongWord):LongWord; 
{Set the clock frequency for the default PWM device}
{Frequency: The frequency to set in Hz}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 if PWMDeviceDefault = nil then Exit;
 
 Result:=PWMDeviceSetFrequency(PWMDeviceDefault,Frequency);
end;

{==============================================================================}
 
function SysPWMConfigure(DutyNS,PeriodNS:LongWord):LongWord; 
{Set the configuration of the default PWM device}
{DutyNS: The "on" time part of the cycle (Nanoseconds)}
{PeriodNS: The duration of one full cycle (Nanoseconds)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 if PWMDeviceDefault = nil then Exit;
 
 Result:=PWMDeviceConfigure(PWMDeviceDefault,DutyNS,PeriodNS);
end;

{==============================================================================}
{==============================================================================}
{PWM Helper Functions}
function PWMGetCount:LongWord;
{Get the current PWM count}
begin
 {}
 Result:=PWMDeviceTableCount;
end;

{==============================================================================}

function PWMDeviceGetDefault:PPWMDevice;
{Get the current default PWM device}
begin
 {}
 Result:=PWMDeviceDefault;
end;

{==============================================================================}

function PWMDeviceSetDefault(PWM:PPWMDevice):LongWord; 
{Set the current default PWM device}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check PWM}
 if PWM = nil then Exit;
 if PWM.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(PWMDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Check PWM}
    if PWMDeviceCheck(PWM) <> PWM then Exit;
    
    {Set PWM Default}
    PWMDeviceDefault:=PWM;
    
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

function PWMTypeToString(PWMType:LongWord):String;
{Convert a PWM type value to a string}
begin
 {}
 Result:='PWM_TYPE_UNKNOWN';
 
 if PWMType <= PWM_TYPE_MAX then
  begin
   Result:=PWM_TYPE_NAMES[PWMType];
  end;
end;

{==============================================================================}

function PWMStateToString(PWMState:LongWord):String;
{Convert a PWM state value to a string}
begin
 {}
 Result:='PWM_STATE_UNKNOWN';
 
 if PWMState <= PWM_STATE_MAX then
  begin
   Result:=PWM_STATE_NAMES[PWMState];
  end;
end;

{==============================================================================}

function PWMModeToString(PWMMode:LongWord):String;
{Convert a PWM mode value to a string}
begin
 {}
 Result:='PWM_MODE_UNKNOWN';
 
 if PWMMode <= PWM_MODE_MAX then
  begin
   Result:=PWM_MODE_NAMES[PWMMode];
  end;
end;

{==============================================================================}

function PWMPolarityToString(PWMPolarity:LongWord):String;
{Convert a PWM polarity value to a string}
begin
 {}
 Result:='PWM_POLARITY_UNKNOWN';
 
 if PWMPolarity <= PWM_POLARITY_MAX then
  begin
   Result:=PWM_POLARITY_NAMES[PWMPolarity];
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
 else if Level = PWM_LOG_LEVEL_WARN then
  begin
   WorkBuffer:=WorkBuffer + '[WARN] ';
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

procedure PWMLogWarn(PWM:PPWMDevice;const AText:String); inline;
begin
 {}
 PWMLog(PWM_LOG_LEVEL_WARN,PWM,AText);
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
