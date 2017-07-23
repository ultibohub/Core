{
Ultibo Video interface unit.

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


Video Devices
=============

 This unit provides both the Video device interface and the generic USB video device driver.

USB Video Devices
=================

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit Video;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,Codec,SysUtils;
    
//To Do //Add MPEG etc //No, they should include video if required

//To Do //This unit should also include the generic USB Video driver ? //No, seperate unit ?
     
{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
const
 {Video specific constants}
 VIDEO_NAME_PREFIX = 'Video';  {Name prefix for Video Devices}

 {Video Device Types}
 VIDEO_TYPE_NONE      = 0;
 
 {Video Device States}
 VIDEO_STATE_DISABLED = 0;
 VIDEO_STATE_ENABLED  = 1;
 
 {Video Device Flags}
 VIDEO_FLAG_NONE      = $00000000;
 
 {Video logging}
 VIDEO_LOG_LEVEL_DEBUG     = LOG_LEVEL_DEBUG;  {Video debugging messages}
 VIDEO_LOG_LEVEL_INFO      = LOG_LEVEL_INFO;   {Video informational messages, such as a device being attached or detached}
 VIDEO_LOG_LEVEL_ERROR     = LOG_LEVEL_ERROR;  {Video error messages}
 VIDEO_LOG_LEVEL_NONE      = LOG_LEVEL_NONE;   {No Video messages}

var 
 VIDEO_DEFAULT_LOG_LEVEL:LongWord = VIDEO_LOG_LEVEL_DEBUG; {Minimum level for Video messages.  Only messages with level greater than or equal to this will be printed}
 
var 
 {Video logging}
 VIDEO_LOG_ENABLED:Boolean; 

{==============================================================================}
type
 {Video specific types}

 {Video Properties}
 PVideoProperties = ^TVideoProperties;
 TVideoProperties = record
  Flags:LongWord;        {Device flags (eg VIDEO_FLAG_????)}
  //To do
 end;
 
 {Video Device}
 PVideoDevice = ^TVideoDevice;
 
 {Video Enumeration Callback}
 TVideoEnumerate = function(Video:PVideoDevice;Data:Pointer):LongWord;
 {Video Notification Callback}
 TVideoNotification = function(Device:PDevice;Data:Pointer;Notification:LongWord):LongWord;
 
 {Video Device Methods}
 //To do
 
 TVideoDeviceGetProperties = function(Video:PVideoDevice;Properties:PVideoProperties):LongWord;
 
 TVideoDevice = record
  {Device Properties}
  Device:TDevice;                                 {The Device entry for this Video device}
  {Video Properties}
  VideoId:LongWord;                               {Unique Id of this Video device in the Video device table}
  VideoState:LongWord;                            {Video device state (eg VIDEO_STATE_ENABLED)}
  //To Do
  DeviceGetProperties:TVideoDeviceGetProperties;  {A Device specific DeviceGetProperties method implementing the standard Video device interface (Or nil if the default method is suitable)}
  {Statistics Properties}
  //To Do
  {Driver Properties}
  Lock:TMutexHandle;                              {Device lock}
  //To Do
  Properties:TVideoProperties;                    {Device properties}
  {Internal Properties}                                                                        
  Prev:PVideoDevice;                              {Previous entry in Video device table}
  Next:PVideoDevice;                              {Next entry in Video device table}
 end; 

{==============================================================================}
{var}
 {Video specific variables}
 
{==============================================================================}
{Initialization Functions}
procedure VideoInit;

{==============================================================================}
{Video Functions}
//To Do

function VideoDeviceGetProperties(Video:PVideoDevice;Properties:PVideoProperties):LongWord;
  
function VideoDeviceCreate:PVideoDevice;
function VideoDeviceCreateEx(Size:LongWord):PVideoDevice;
function VideoDeviceDestroy(Video:PVideoDevice):LongWord;

function VideoDeviceRegister(Video:PVideoDevice):LongWord;
function VideoDeviceDeregister(Video:PVideoDevice):LongWord;

function VideoDeviceFind(VideoId:LongWord):PVideoDevice;
function VideoDeviceFindByName(const Name:String):PVideoDevice; inline;
function VideoDeviceFindByDescription(const Description:String):PVideoDevice; inline;
function VideoDeviceEnumerate(Callback:TVideoEnumerate;Data:Pointer):LongWord;
 
function VideoDeviceNotification(Video:PVideoDevice;Callback:TVideoNotification;Data:Pointer;Notification,Flags:LongWord):LongWord;

{==============================================================================}
{RTL Video Functions}
//To do

{==============================================================================}
{Video Helper Functions}
function VideoGetCount:LongWord; inline;
function VideoDeviceGetDefault:PVideoDevice; inline;
function VideoDeviceSetDefault(Video:PVideoDevice):LongWord; 

function VideoDeviceCheck(Video:PVideoDevice):PVideoDevice;

procedure VideoLog(Level:LongWord;Video:PVideoDevice;const AText:String);
procedure VideoLogInfo(Video:PVideoDevice;const AText:String); inline;
procedure VideoLogError(Video:PVideoDevice;const AText:String); inline;
procedure VideoLogDebug(Video:PVideoDevice;const AText:String); inline;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {Video specific variables}
 VideoInitialized:Boolean;

 VideoDeviceTable:PVideoDevice;
 VideoDeviceTableLock:TCriticalSectionHandle = INVALID_HANDLE_VALUE;
 VideoDeviceTableCount:LongWord;

 VideoDeviceDefault:PVideoDevice;

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure VideoInit;
{Initialize the Video unit and Video device table}

{Note: Called only during system startup}
begin
 {}
 {Check Initialized}
 if VideoInitialized then Exit;
 
 {Initialize Logging}
 VIDEO_LOG_ENABLED:=(VIDEO_DEFAULT_LOG_LEVEL <> VIDEO_LOG_LEVEL_NONE); 
 
 {Initialize Video Device Table}
 VideoDeviceTable:=nil;
 VideoDeviceTableLock:=CriticalSectionCreate; 
 VideoDeviceTableCount:=0;
 if VideoDeviceTableLock = INVALID_HANDLE_VALUE then
  begin
   if VIDEO_LOG_ENABLED then VideoLogError(nil,'Failed to create Video device table lock');
  end;
 VideoDeviceDefault:=nil;
 
 {Register Platform Video Handlers}
 //To Do
 
 VideoInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{Video Device Functions}
//To Do

{==============================================================================}
 
function VideoDeviceGetProperties(Video:PVideoDevice;Properties:PVideoProperties):LongWord;
{Get the properties for the specified Video device}
{Video: The Video device to get properties from}
{Properties: Pointer to a TVideoProperties structure to fill in}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Properties}
 if Properties = nil then Exit;
 
 {Check Video}
 if Video = nil then Exit;
 if Video.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IFDEF VIDEO_DEBUG}
 if VIDEO_LOG_ENABLED then VideoLogDebug(Video,'Video Device Get Properties');
 {$ENDIF}
 
 {Check Enabled}
 {Result:=ERROR_NOT_SUPPORTED;}
 {if Video.VideoState <> VIDEO_STATE_ENABLED then Exit;} {Allow when disabled}
 
 if MutexLock(Video.Lock) = ERROR_SUCCESS then
  begin
   if Assigned(Video.DeviceGetProperties) then
    begin
     {Call Device Get Properites}
     Result:=Video.DeviceGetProperties(Video,Properties);
    end
   else
    begin
     {Get Properties}
     System.Move(Video.Properties,Properties^,SizeOf(TVideoProperties));
       
     {Return Result}
     Result:=ERROR_SUCCESS;
    end;  
    
   MutexUnlock(Video.Lock);
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;    
end;

{==============================================================================}

function VideoDeviceCreate:PVideoDevice;
{Create a new Video device entry}
{Return: Pointer to new Video device entry or nil if Video device could not be created}
begin
 {}
 Result:=VideoDeviceCreateEx(SizeOf(TVideoDevice));
end;

{==============================================================================}

function VideoDeviceCreateEx(Size:LongWord):PVideoDevice;
{Create a new Video device entry}
{Size: Size in bytes to allocate for new Video device (Including the Video device entry)}
{Return: Pointer to new Video device entry or nil if Video device could not be created}
begin
 {}
 Result:=nil;
 
 {Check Size}
 if Size < SizeOf(TVideoDevice) then Exit;
 
 {Create Video}
 Result:=PVideoDevice(DeviceCreateEx(Size));
 if Result = nil then Exit;
 
 {Update Device}
 Result.Device.DeviceBus:=DEVICE_BUS_NONE;   
 Result.Device.DeviceType:=VIDEO_TYPE_NONE;
 Result.Device.DeviceFlags:=VIDEO_FLAG_NONE;
 Result.Device.DeviceData:=nil;

 {Update Video}
 Result.VideoId:=DEVICE_ID_ANY;
 Result.VideoState:=VIDEO_STATE_DISABLED;
 //To Do
 Result.DeviceGetProperties:=nil;
 Result.Lock:=INVALID_HANDLE_VALUE;
 
 {Create Lock}
 Result.Lock:=MutexCreateEx(False,MUTEX_DEFAULT_SPINCOUNT,MUTEX_FLAG_RECURSIVE);
 if Result.Lock = INVALID_HANDLE_VALUE then
  begin
   if VIDEO_LOG_ENABLED then VideoLogError(nil,'Failed to create lock for Video device');
   VideoDeviceDestroy(Result);
   Result:=nil;
   Exit;
  end;
end;

{==============================================================================}

function VideoDeviceDestroy(Video:PVideoDevice):LongWord;
{Destroy an existing Video device entry}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Video}
 if Video = nil then Exit;
 if Video.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check Video}
 Result:=ERROR_IN_USE;
 if VideoDeviceCheck(Video) = Video then Exit;

 {Check State}
 if Video.Device.DeviceState <> DEVICE_STATE_UNREGISTERED then Exit;
 
 {Destroy Lock}
 if Video.Lock <> INVALID_HANDLE_VALUE then
  begin
   MutexDestroy(Video.Lock);
  end;
 
 {Destroy Video} 
 Result:=DeviceDestroy(@Video.Device);
end;

{==============================================================================}

function VideoDeviceRegister(Video:PVideoDevice):LongWord;
{Register a new Video device in the Video device table}
var
 VideoId:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Video}
 if Video = nil then Exit;
 if Video.VideoId <> DEVICE_ID_ANY then Exit;
 if Video.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check Interfaces}
 //To Do
 
 {Check Video}
 Result:=ERROR_ALREADY_EXISTS;
 if VideoDeviceCheck(Video) = Video then Exit;
 
 {Check State}
 if Video.Device.DeviceState <> DEVICE_STATE_UNREGISTERED then Exit;
 
 {Insert Video}
 if CriticalSectionLock(VideoDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Update Video}
    VideoId:=0;
    while VideoDeviceFind(VideoId) <> nil do
     begin
      Inc(VideoId);
     end;
    Video.VideoId:=VideoId;
    
    {Update Device}
    Video.Device.DeviceName:=VIDEO_NAME_PREFIX + IntToStr(Video.VideoId); 
    Video.Device.DeviceClass:=DEVICE_CLASS_VIDEO;
    
    {Register Device}
    Result:=DeviceRegister(@Video.Device);
    if Result <> ERROR_SUCCESS then
     begin
      Video.VideoId:=DEVICE_ID_ANY;
      Exit;
     end; 
    
    {Link Video}
    if VideoDeviceTable = nil then
     begin
      VideoDeviceTable:=Video;
     end
    else
     begin
      Video.Next:=VideoDeviceTable;
      VideoDeviceTable.Prev:=Video;
      VideoDeviceTable:=Video;
     end;
 
    {Increment Count}
    Inc(VideoDeviceTableCount);
    
    {Check Default}
    if VideoDeviceDefault = nil then
     begin
      VideoDeviceDefault:=Video;
     end;
    
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    CriticalSectionUnlock(VideoDeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;

{==============================================================================}

function VideoDeviceDeregister(Video:PVideoDevice):LongWord;
{Deregister a Video device from the Video device table}
var
 Prev:PVideoDevice;
 Next:PVideoDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Video}
 if Video = nil then Exit;
 if Video.VideoId = DEVICE_ID_ANY then Exit;
 if Video.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check Video}
 Result:=ERROR_NOT_FOUND;
 if VideoDeviceCheck(Video) <> Video then Exit;
 
 {Check State}
 if Video.Device.DeviceState <> DEVICE_STATE_REGISTERED then Exit;
 
 {Remove Video}
 if CriticalSectionLock(VideoDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Deregister Device}
    Result:=DeviceDeregister(@Video.Device);
    if Result <> ERROR_SUCCESS then Exit;
    
    {Unlink Video}
    Prev:=Video.Prev;
    Next:=Video.Next;
    if Prev = nil then
     begin
      VideoDeviceTable:=Next;
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
    Dec(VideoDeviceTableCount);
 
    {Check Default}
    if VideoDeviceDefault = Video then
     begin
      VideoDeviceDefault:=VideoDeviceTable;
     end;
 
    {Update Video}
    Video.VideoId:=DEVICE_ID_ANY;
 
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    CriticalSectionUnlock(VideoDeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;

{==============================================================================}

function VideoDeviceFind(VideoId:LongWord):PVideoDevice;
var
 Video:PVideoDevice;
begin
 {}
 Result:=nil;
 
 {Check Id}
 if VideoId = DEVICE_ID_ANY then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(VideoDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Video}
    Video:=VideoDeviceTable;
    while Video <> nil do
     begin
      {Check State}
      if Video.Device.DeviceState = DEVICE_STATE_REGISTERED then
       begin
        {Check Id}
        if Video.VideoId = VideoId then
         begin
          Result:=Video;
          Exit;
         end;
       end;
       
      {Get Next}
      Video:=Video.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(VideoDeviceTableLock);
   end;
  end;
end;

{==============================================================================}

function VideoDeviceFindByName(const Name:String):PVideoDevice; inline;
begin
 {}
 Result:=PVideoDevice(DeviceFindByName(Name));
end;

{==============================================================================}

function VideoDeviceFindByDescription(const Description:String):PVideoDevice; inline;
begin
 {}
 Result:=PVideoDevice(DeviceFindByDescription(Description));
end;
       
{==============================================================================}

function VideoDeviceEnumerate(Callback:TVideoEnumerate;Data:Pointer):LongWord;
var
 Video:PVideoDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Callback}
 if not Assigned(Callback) then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(VideoDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Video}
    Video:=VideoDeviceTable;
    while Video <> nil do
     begin
      {Check State}
      if Video.Device.DeviceState = DEVICE_STATE_REGISTERED then
       begin
        if Callback(Video,Data) <> ERROR_SUCCESS then Exit;
       end;
       
      {Get Next}
      Video:=Video.Next;
     end;
     
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    CriticalSectionUnlock(VideoDeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;

{==============================================================================}

function VideoDeviceNotification(Video:PVideoDevice;Callback:TVideoNotification;Data:Pointer;Notification,Flags:LongWord):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Video}
 if Video = nil then
  begin
   Result:=DeviceNotification(nil,DEVICE_CLASS_Video,Callback,Data,Notification,Flags);
  end
 else
  begin 
   {Check Video}
   if Video.Device.Signature <> DEVICE_SIGNATURE then Exit;

   Result:=DeviceNotification(@Video.Device,DEVICE_CLASS_VIDEO,Callback,Data,Notification,Flags);
  end; 
end;

{==============================================================================}
{==============================================================================}
{RTL Video Functions}

{==============================================================================}
{==============================================================================}
{Video Helper Functions}
function VideoGetCount:LongWord; inline;
{Get the current Video device count}
begin
 {}
 Result:=VideoDeviceTableCount;
end;

{==============================================================================}

function VideoDeviceGetDefault:PVideoDevice; inline;
{Get the current default Video device}
begin
 {}
 Result:=VideoDeviceDefault;
end;

{==============================================================================}

function VideoDeviceSetDefault(Video:PVideoDevice):LongWord; 
{Set the current default Video device}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Video}
 if Video = nil then Exit;
 if Video.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(VideoDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Check Video}
    if VideoDeviceCheck(Video) <> Video then Exit;
    
    {Set Video Default}
    VideoDeviceDefault:=Video;
    
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    CriticalSectionUnlock(VideoDeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function VideoDeviceCheck(Video:PVideoDevice):PVideoDevice;
{Check if the supplied Video device is in the Video device table}
var
 Current:PVideoDevice;
begin
 {}
 Result:=nil;
 
 {Check Video}
 if Video = nil then Exit;
 if Video.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(VideoDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Video}
    Current:=VideoDeviceTable;
    while Current <> nil do
     begin
      {Check Video}
      if Current = Video then
       begin
        Result:=Video;
        Exit;
       end;
      
      {Get Next}
      Current:=Current.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(VideoDeviceTableLock);
   end;
  end;
end;

{==============================================================================}

procedure VideoLog(Level:LongWord;Video:PVideoDevice;const AText:String);
var
 WorkBuffer:String;
begin
 {}
 {Check Level}
 if Level < VIDEO_DEFAULT_LOG_LEVEL then Exit;
 
 WorkBuffer:='';
 {Check Level}
 if Level = VIDEO_LOG_LEVEL_DEBUG then
  begin
   WorkBuffer:=WorkBuffer + '[DEBUG] ';
  end
 else if Level = VIDEO_LOG_LEVEL_ERROR then
  begin
   WorkBuffer:=WorkBuffer + '[ERROR] ';
  end;
 
 {Add Prefix}
 WorkBuffer:=WorkBuffer + 'Video: ';
 
 {Check Video}
 if Video <> nil then
  begin
   WorkBuffer:=WorkBuffer + VIDEO_NAME_PREFIX + IntToStr(Video.VideoId) + ': ';
  end;

 {Output Logging}  
 LoggingOutputEx(LOGGING_FACILITY_VIDEO,LogLevelToLoggingSeverity(Level),'Video',WorkBuffer + AText);
end;

{==============================================================================}

procedure VideoLogInfo(Video:PVideoDevice;const AText:String); inline;
begin
 {}
 VideoLog(VIDEO_LOG_LEVEL_INFO,Video,AText);
end;

{==============================================================================}

procedure VideoLogError(Video:PVideoDevice;const AText:String); inline;
begin
 {}
 VideoLog(VIDEO_LOG_LEVEL_ERROR,Video,AText);
end;

{==============================================================================}

procedure VideoLogDebug(Video:PVideoDevice;const AText:String); inline;
begin
 {}
 VideoLog(VIDEO_LOG_LEVEL_DEBUG,Video,AText);
end;

{==============================================================================}
{==============================================================================}

initialization
 VideoInit;

{==============================================================================}
 
finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
