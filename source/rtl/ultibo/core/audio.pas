{
Ultibo Audio interface unit.

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


Audio Devices
=============

 This unit provides both the Audio device interface and the generic USB audio device driver.

USB Audio Devices
=================

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit Audio;

interface

uses
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  Devices,
  Codec,
  SysUtils;

//To Do //Add PCM/I2S, MPEG etc //No, they should include audio if required

//To Do //This unit should also include the generic USB Audio driver ? //No, seperate unit ?

//To Do //See also: \source\packages\a52\src\a52.pas
//To Do //See also: \source\packages\fcl-sound\src

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
const
 {Audio specific constants}
 AUDIO_NAME_PREFIX = 'Audio';  {Name prefix for Audio Devices}

 {Audio Device Types}
 AUDIO_TYPE_NONE      = 0;

 {Audio Device States}
 AUDIO_STATE_DISABLED = 0;
 AUDIO_STATE_ENABLED  = 1;

 {Audio Device Flags}
 AUDIO_FLAG_NONE      = $00000000;

 {Audio logging}
 AUDIO_LOG_LEVEL_DEBUG     = LOG_LEVEL_DEBUG;  {Audio debugging messages}
 AUDIO_LOG_LEVEL_INFO      = LOG_LEVEL_INFO;   {Audio informational messages, such as a device being attached or detached}
 AUDIO_LOG_LEVEL_WARN      = LOG_LEVEL_WARN;   {Audio warning messages}
 AUDIO_LOG_LEVEL_ERROR     = LOG_LEVEL_ERROR;  {Audio error messages}
 AUDIO_LOG_LEVEL_NONE      = LOG_LEVEL_NONE;   {No Audio messages}

var
 AUDIO_DEFAULT_LOG_LEVEL:LongWord = AUDIO_LOG_LEVEL_DEBUG; {Minimum level for Audio messages.  Only messages with level greater than or equal to this will be printed}

var
 {Audio logging}
 AUDIO_LOG_ENABLED:Boolean;

{==============================================================================}
type
 {Audio specific types}

 {Audio Properties}
 PAudioProperties = ^TAudioProperties;
 TAudioProperties = record
  Flags:LongWord;        {Device flags (eg AUDIO_FLAG_????)}
  //To do
 end;

 {Audio Device}
 PAudioDevice = ^TAudioDevice;

 {Audio Enumeration Callback}
 TAudioEnumerate = function(Audio:PAudioDevice;Data:Pointer):LongWord;
 {Audio Notification Callback}
 TAudioNotification = function(Device:PDevice;Data:Pointer;Notification:LongWord):LongWord;

 {Audio Device Methods}
 //To do

 TAudioDeviceGetProperties = function(Audio:PAudioDevice;Properties:PAudioProperties):LongWord;

 TAudioDevice = record
  {Device Properties}
  Device:TDevice;                                 {The Device entry for this Audio device}
  {Audio Properties}
  AudioId:LongWord;                               {Unique Id of this Audio device in the Audio device table}
  AudioState:LongWord;                            {Audio device state (eg AUDIO_STATE_ENABLED)}
  //To Do
  DeviceGetProperties:TAudioDeviceGetProperties;  {A Device specific DeviceGetProperties method implementing the standard Audio device interface (Or nil if the default method is suitable)}
  {Statistics Properties}
  //To Do
  {Driver Properties}
  Lock:TMutexHandle;                              {Device lock}
  //To Do
  Properties:TAudioProperties;                    {Device properties}
  {Internal Properties}
  Prev:PAudioDevice;                              {Previous entry in Audio device table}
  Next:PAudioDevice;                              {Next entry in Audio device table}
 end;

{==============================================================================}
{var}
 {Audio specific variables}

{==============================================================================}
{Initialization Functions}
procedure AudioInit;

{==============================================================================}
{Audio Functions}
//To Do

function AudioDeviceGetProperties(Audio:PAudioDevice;Properties:PAudioProperties):LongWord;

function AudioDeviceCreate:PAudioDevice;
function AudioDeviceCreateEx(Size:LongWord):PAudioDevice;
function AudioDeviceDestroy(Audio:PAudioDevice):LongWord;

function AudioDeviceRegister(Audio:PAudioDevice):LongWord;
function AudioDeviceDeregister(Audio:PAudioDevice):LongWord;

function AudioDeviceFind(AudioId:LongWord):PAudioDevice;
function AudioDeviceFindByName(const Name:String):PAudioDevice; inline;
function AudioDeviceFindByDescription(const Description:String):PAudioDevice; inline;
function AudioDeviceEnumerate(Callback:TAudioEnumerate;Data:Pointer):LongWord;

function AudioDeviceNotification(Audio:PAudioDevice;Callback:TAudioNotification;Data:Pointer;Notification,Flags:LongWord):LongWord;

{==============================================================================}
{RTL Audio Functions}
//To do

{==============================================================================}
{Audio Helper Functions}
function AudioGetCount:LongWord;
function AudioDeviceGetDefault:PAudioDevice;
function AudioDeviceSetDefault(Audio:PAudioDevice):LongWord;

function AudioDeviceCheck(Audio:PAudioDevice):PAudioDevice;

procedure AudioLog(Level:LongWord;Audio:PAudioDevice;const AText:String);
procedure AudioLogInfo(Audio:PAudioDevice;const AText:String); inline;
procedure AudioLogWarn(Audio:PAudioDevice;const AText:String); inline;
procedure AudioLogError(Audio:PAudioDevice;const AText:String); inline;
procedure AudioLogDebug(Audio:PAudioDevice;const AText:String); inline;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {Audio specific variables}
 AudioInitialized:Boolean;

 AudioDeviceTable:PAudioDevice;
 AudioDeviceTableLock:TCriticalSectionHandle = INVALID_HANDLE_VALUE;
 AudioDeviceTableCount:LongWord;

 AudioDeviceDefault:PAudioDevice;

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure AudioInit;
{Initialize the Audio unit and Audio device table}

{Note: Called only during system startup}
begin
 {}
 {Check Initialized}
 if AudioInitialized then Exit;

 {Initialize Logging}
 AUDIO_LOG_ENABLED:=(AUDIO_DEFAULT_LOG_LEVEL <> AUDIO_LOG_LEVEL_NONE);

 {Initialize Audio Device Table}
 AudioDeviceTable:=nil;
 AudioDeviceTableLock:=CriticalSectionCreate;
 AudioDeviceTableCount:=0;
 if AudioDeviceTableLock = INVALID_HANDLE_VALUE then
  begin
   if AUDIO_LOG_ENABLED then AudioLogError(nil,'Failed to create Audio device table lock');
  end;
 AudioDeviceDefault:=nil;

 {Register Platform Audio Handlers}
 //To Do

 AudioInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{Audio Device Functions}
//To Do

{==============================================================================}

function AudioDeviceGetProperties(Audio:PAudioDevice;Properties:PAudioProperties):LongWord;
{Get the properties for the specified Audio device}
{Audio: The Audio device to get properties from}
{Properties: Pointer to a TAudioProperties structure to fill in}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Properties}
 if Properties = nil then Exit;

 {Check Audio}
 if Audio = nil then Exit;
 if Audio.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF AUDIO_DEBUG}
 if AUDIO_LOG_ENABLED then AudioLogDebug(Audio,'Audio Device Get Properties');
 {$ENDIF}

 {Check Enabled}
 {Result:=ERROR_NOT_SUPPORTED;}
 {if Audio.AudioState <> AUDIO_STATE_ENABLED then Exit;} {Allow when disabled}

 if MutexLock(Audio.Lock) = ERROR_SUCCESS then
  begin
   if Assigned(Audio.DeviceGetProperties) then
    begin
     {Call Device Get Properites}
     Result:=Audio.DeviceGetProperties(Audio,Properties);
    end
   else
    begin
     {Get Properties}
     System.Move(Audio.Properties,Properties^,SizeOf(TAudioProperties));

     {Return Result}
     Result:=ERROR_SUCCESS;
    end;

   MutexUnlock(Audio.Lock);
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function AudioDeviceCreate:PAudioDevice;
{Create a new Audio device entry}
{Return: Pointer to new Audio device entry or nil if Audio device could not be created}
begin
 {}
 Result:=AudioDeviceCreateEx(SizeOf(TAudioDevice));
end;

{==============================================================================}

function AudioDeviceCreateEx(Size:LongWord):PAudioDevice;
{Create a new Audio device entry}
{Size: Size in bytes to allocate for new Audio device (Including the Audio device entry)}
{Return: Pointer to new Audio device entry or nil if Audio device could not be created}
begin
 {}
 Result:=nil;

 {Check Size}
 if Size < SizeOf(TAudioDevice) then Exit;

 {Create Audio}
 Result:=PAudioDevice(DeviceCreateEx(Size));
 if Result = nil then Exit;

 {Update Device}
 Result.Device.DeviceBus:=DEVICE_BUS_NONE;
 Result.Device.DeviceType:=AUDIO_TYPE_NONE;
 Result.Device.DeviceFlags:=AUDIO_FLAG_NONE;
 Result.Device.DeviceData:=nil;

 {Update Audio}
 Result.AudioId:=DEVICE_ID_ANY;
 Result.AudioState:=AUDIO_STATE_DISABLED;
 //To Do
 Result.DeviceGetProperties:=nil;
 Result.Lock:=INVALID_HANDLE_VALUE;

 {Create Lock}
 Result.Lock:=MutexCreateEx(False,MUTEX_DEFAULT_SPINCOUNT,MUTEX_FLAG_RECURSIVE);
 if Result.Lock = INVALID_HANDLE_VALUE then
  begin
   if AUDIO_LOG_ENABLED then AudioLogError(nil,'Failed to create lock for Audio device');
   AudioDeviceDestroy(Result);
   Result:=nil;
   Exit;
  end;
end;

{==============================================================================}

function AudioDeviceDestroy(Audio:PAudioDevice):LongWord;
{Destroy an existing Audio device entry}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Audio}
 if Audio = nil then Exit;
 if Audio.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Audio}
 Result:=ERROR_IN_USE;
 if AudioDeviceCheck(Audio) = Audio then Exit;

 {Check State}
 if Audio.Device.DeviceState <> DEVICE_STATE_UNREGISTERED then Exit;

 {Destroy Lock}
 if Audio.Lock <> INVALID_HANDLE_VALUE then
  begin
   MutexDestroy(Audio.Lock);
  end;

 {Destroy Audio}
 Result:=DeviceDestroy(@Audio.Device);
end;

{==============================================================================}

function AudioDeviceRegister(Audio:PAudioDevice):LongWord;
{Register a new Audio device in the Audio device table}
var
 AudioId:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Audio}
 if Audio = nil then Exit;
 if Audio.AudioId <> DEVICE_ID_ANY then Exit;
 if Audio.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Interfaces}
 //To Do

 {Check Audio}
 Result:=ERROR_ALREADY_EXISTS;
 if AudioDeviceCheck(Audio) = Audio then Exit;

 {Check State}
 if Audio.Device.DeviceState <> DEVICE_STATE_UNREGISTERED then Exit;

 {Insert Audio}
 if CriticalSectionLock(AudioDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Update Audio}
    AudioId:=0;
    while AudioDeviceFind(AudioId) <> nil do
     begin
      Inc(AudioId);
     end;
    Audio.AudioId:=AudioId;

    {Update Device}
    Audio.Device.DeviceName:=AUDIO_NAME_PREFIX + IntToStr(Audio.AudioId);
    Audio.Device.DeviceClass:=DEVICE_CLASS_AUDIO;

    {Register Device}
    Result:=DeviceRegister(@Audio.Device);
    if Result <> ERROR_SUCCESS then
     begin
      Audio.AudioId:=DEVICE_ID_ANY;
      Exit;
     end;

    {Link Audio}
    if AudioDeviceTable = nil then
     begin
      AudioDeviceTable:=Audio;
     end
    else
     begin
      Audio.Next:=AudioDeviceTable;
      AudioDeviceTable.Prev:=Audio;
      AudioDeviceTable:=Audio;
     end;

    {Increment Count}
    Inc(AudioDeviceTableCount);

    {Check Default}
    if AudioDeviceDefault = nil then
     begin
      AudioDeviceDefault:=Audio;
     end;

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    CriticalSectionUnlock(AudioDeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function AudioDeviceDeregister(Audio:PAudioDevice):LongWord;
{Deregister an Audio device from the Audio device table}
var
 Prev:PAudioDevice;
 Next:PAudioDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Audio}
 if Audio = nil then Exit;
 if Audio.AudioId = DEVICE_ID_ANY then Exit;
 if Audio.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Audio}
 Result:=ERROR_NOT_FOUND;
 if AudioDeviceCheck(Audio) <> Audio then Exit;

 {Check State}
 if Audio.Device.DeviceState <> DEVICE_STATE_REGISTERED then Exit;

 {Remove Audio}
 if CriticalSectionLock(AudioDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Deregister Device}
    Result:=DeviceDeregister(@Audio.Device);
    if Result <> ERROR_SUCCESS then Exit;

    {Unlink Audio}
    Prev:=Audio.Prev;
    Next:=Audio.Next;
    if Prev = nil then
     begin
      AudioDeviceTable:=Next;
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
    Dec(AudioDeviceTableCount);

    {Check Default}
    if AudioDeviceDefault = Audio then
     begin
      AudioDeviceDefault:=AudioDeviceTable;
     end;

    {Update Audio}
    Audio.AudioId:=DEVICE_ID_ANY;

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    CriticalSectionUnlock(AudioDeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function AudioDeviceFind(AudioId:LongWord):PAudioDevice;
var
 Audio:PAudioDevice;
begin
 {}
 Result:=nil;

 {Check Id}
 if AudioId = DEVICE_ID_ANY then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(AudioDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Audio}
    Audio:=AudioDeviceTable;
    while Audio <> nil do
     begin
      {Check State}
      if Audio.Device.DeviceState = DEVICE_STATE_REGISTERED then
       begin
        {Check Id}
        if Audio.AudioId = AudioId then
         begin
          Result:=Audio;
          Exit;
         end;
       end;

      {Get Next}
      Audio:=Audio.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(AudioDeviceTableLock);
   end;
  end;
end;

{==============================================================================}

function AudioDeviceFindByName(const Name:String):PAudioDevice; inline;
begin
 {}
 Result:=PAudioDevice(DeviceFindByName(Name));
end;

{==============================================================================}

function AudioDeviceFindByDescription(const Description:String):PAudioDevice; inline;
begin
 {}
 Result:=PAudioDevice(DeviceFindByDescription(Description));
end;

{==============================================================================}

function AudioDeviceEnumerate(Callback:TAudioEnumerate;Data:Pointer):LongWord;
var
 Audio:PAudioDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Callback}
 if not Assigned(Callback) then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(AudioDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Audio}
    Audio:=AudioDeviceTable;
    while Audio <> nil do
     begin
      {Check State}
      if Audio.Device.DeviceState = DEVICE_STATE_REGISTERED then
       begin
        if Callback(Audio,Data) <> ERROR_SUCCESS then Exit;
       end;

      {Get Next}
      Audio:=Audio.Next;
     end;

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    CriticalSectionUnlock(AudioDeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function AudioDeviceNotification(Audio:PAudioDevice;Callback:TAudioNotification;Data:Pointer;Notification,Flags:LongWord):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Audio}
 if Audio = nil then
  begin
   Result:=DeviceNotification(nil,DEVICE_CLASS_Audio,Callback,Data,Notification,Flags);
  end
 else
  begin
   {Check Audio}
   if Audio.Device.Signature <> DEVICE_SIGNATURE then Exit;

   Result:=DeviceNotification(@Audio.Device,DEVICE_CLASS_AUDIO,Callback,Data,Notification,Flags);
  end;
end;

{==============================================================================}
{==============================================================================}
{RTL Audio Functions}

{==============================================================================}
{==============================================================================}
{Audio Helper Functions}
function AudioGetCount:LongWord;
{Get the current Audio device count}
begin
 {}
 Result:=AudioDeviceTableCount;
end;

{==============================================================================}

function AudioDeviceGetDefault:PAudioDevice;
{Get the current default Audio device}
begin
 {}
 Result:=AudioDeviceDefault;
end;

{==============================================================================}

function AudioDeviceSetDefault(Audio:PAudioDevice):LongWord;
{Set the current default Audio device}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Audio}
 if Audio = nil then Exit;
 if Audio.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(AudioDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Check Audio}
    if AudioDeviceCheck(Audio) <> Audio then Exit;

    {Set Audio Default}
    AudioDeviceDefault:=Audio;

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    CriticalSectionUnlock(AudioDeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function AudioDeviceCheck(Audio:PAudioDevice):PAudioDevice;
{Check if the supplied Audio device is in the Audio device table}
var
 Current:PAudioDevice;
begin
 {}
 Result:=nil;

 {Check Audio}
 if Audio = nil then Exit;
 if Audio.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(AudioDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Audio}
    Current:=AudioDeviceTable;
    while Current <> nil do
     begin
      {Check Audio}
      if Current = Audio then
       begin
        Result:=Audio;
        Exit;
       end;

      {Get Next}
      Current:=Current.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(AudioDeviceTableLock);
   end;
  end;
end;

{==============================================================================}

procedure AudioLog(Level:LongWord;Audio:PAudioDevice;const AText:String);
var
 WorkBuffer:String;
begin
 {}
 {Check Level}
 if Level < AUDIO_DEFAULT_LOG_LEVEL then Exit;

 WorkBuffer:='';
 {Check Level}
 if Level = AUDIO_LOG_LEVEL_DEBUG then
  begin
   WorkBuffer:=WorkBuffer + '[DEBUG] ';
  end
 else if Level = AUDIO_LOG_LEVEL_WARN then
  begin
   WorkBuffer:=WorkBuffer + '[WARN] ';
  end
 else if Level = AUDIO_LOG_LEVEL_ERROR then
  begin
   WorkBuffer:=WorkBuffer + '[ERROR] ';
  end;

 {Add Prefix}
 WorkBuffer:=WorkBuffer + 'Audio: ';

 {Check Audio}
 if Audio <> nil then
  begin
   WorkBuffer:=WorkBuffer + AUDIO_NAME_PREFIX + IntToStr(Audio.AudioId) + ': ';
  end;

 {Output Logging}
 LoggingOutputEx(LOGGING_FACILITY_AUDIO,LogLevelToLoggingSeverity(Level),'Audio',WorkBuffer + AText);
end;

{==============================================================================}

procedure AudioLogInfo(Audio:PAudioDevice;const AText:String); inline;
begin
 {}
 AudioLog(AUDIO_LOG_LEVEL_INFO,Audio,AText);
end;

{==============================================================================}

procedure AudioLogWarn(Audio:PAudioDevice;const AText:String); inline;
begin
 {}
 AudioLog(AUDIO_LOG_LEVEL_WARN,Audio,AText);
end;

{==============================================================================}

procedure AudioLogError(Audio:PAudioDevice;const AText:String); inline;
begin
 {}
 AudioLog(AUDIO_LOG_LEVEL_ERROR,Audio,AText);
end;

{==============================================================================}

procedure AudioLogDebug(Audio:PAudioDevice;const AText:String); inline;
begin
 {}
 AudioLog(AUDIO_LOG_LEVEL_DEBUG,Audio,AText);
end;

{==============================================================================}
{==============================================================================}

initialization
 AudioInit;

{==============================================================================}

finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
