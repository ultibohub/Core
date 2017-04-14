{
Ultibo Codec interface unit.

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


Codec Devices
=============


}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit Codec;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,SysUtils;
     
//To Do //See also: \source\packages\a52\src\a52.pas
//To Do //See also: \source\packages\fcl-sound\src
     
{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
const
 {Codec specific constants}
 CODEC_NAME_PREFIX = 'Codec';  {Name prefix for Codec Devices}

 {Codec Device Types}
 CODEC_TYPE_NONE      = 0;
 
 {Codec Device States}
 CODEC_STATE_DISABLED = 0;
 CODEC_STATE_ENABLED  = 1;
 
 {Codec Device Flags}
 CODEC_FLAG_NONE      = $00000000;
 
 {Codec logging}
 CODEC_LOG_LEVEL_DEBUG     = LOG_LEVEL_DEBUG;  {Codec debugging messages}
 CODEC_LOG_LEVEL_INFO      = LOG_LEVEL_INFO;   {Codec informational messages, such as a device being attached or detached}
 CODEC_LOG_LEVEL_ERROR     = LOG_LEVEL_ERROR;  {Codec error messages}
 CODEC_LOG_LEVEL_NONE      = LOG_LEVEL_NONE;   {No Codec messages}

var 
 CODEC_DEFAULT_LOG_LEVEL:LongWord = CODEC_LOG_LEVEL_DEBUG; {Minimum level for Codec messages.  Only messages with level greater than or equal to this will be printed}
 
var 
 {Codec logging}
 CODEC_LOG_ENABLED:Boolean; 
 
{==============================================================================}
type
 {Codec specific types}

 {Codec Properties}
 PCodecProperties = ^TCodecProperties;
 TCodecProperties = record
  Flags:LongWord;        {Device flags (eg CODEC_FLAG_????)}
  //To do
 end;
 
 {Codec Device}
 PCodecDevice = ^TCodecDevice;
 
 {Codec Enumeration Callback}
 TCodecEnumerate = function(Codec:PCodecDevice;Data:Pointer):LongWord;
 {Codec Notification Callback}
 TCodecNotification = function(Device:PDevice;Data:Pointer;Notification:LongWord):LongWord;
 
 {Codec Device Methods}
 //To do
 
 TCodecDeviceProperties = function(Codec:PCodecDevice;Properties:PCodecProperties):LongWord;
 
 TCodecDevice = record
  {Device Properties}
  Device:TDevice;                                 {The Device entry for this Codec device}
  {Codec Properties}
  CodecId:LongWord;                               {Unique Id of this Codec device in the Codec device table}
  CodecState:LongWord;                            {Codec device state (eg CODEC_STATE_ENABLED)}
  //To Do
  DeviceProperties:TCodecDeviceProperties;     {A Device specific DeviceProperties method implementing the standard Codec device interface (Or nil if the default method is suitable)}
  {Statistics Properties}
  //To Do
  {Driver Properties}
  Lock:TMutexHandle;                              {Device lock}
  //To Do
  Properties:TCodecProperties;                    {Device properties}
  {Internal Properties}                                                                        
  Prev:PCodecDevice;                              {Previous entry in Codec device table}
  Next:PCodecDevice;                              {Next entry in Codec device table}
 end; 
  
{==============================================================================}
{var}
 {Codec specific variables}
 
{==============================================================================}
{Initialization Functions}
procedure CodecInit;

{==============================================================================}
{Codec Functions}
//To Do

function CodecDeviceProperties(Codec:PCodecDevice;Properties:PCodecProperties):LongWord;
  
function CodecDeviceCreate:PCodecDevice;
function CodecDeviceCreateEx(Size:LongWord):PCodecDevice;
function CodecDeviceDestroy(Codec:PCodecDevice):LongWord;

function CodecDeviceRegister(Codec:PCodecDevice):LongWord;
function CodecDeviceDeregister(Codec:PCodecDevice):LongWord;

function CodecDeviceFind(CodecId:LongWord):PCodecDevice;
function CodecDeviceFindByName(const Name:String):PCodecDevice; inline;
function CodecDeviceFindByDescription(const Description:String):PCodecDevice; inline;
function CodecDeviceEnumerate(Callback:TCodecEnumerate;Data:Pointer):LongWord;
 
function CodecDeviceNotification(Codec:PCodecDevice;Callback:TCodecNotification;Data:Pointer;Notification,Flags:LongWord):LongWord;

{==============================================================================}
{RTL Codec Functions}
//To do

{==============================================================================}
{Codec Helper Functions}
function CodecGetCount:LongWord; inline;
function CodecDeviceGetDefault:PCodecDevice; inline;
function CodecDeviceSetDefault(Codec:PCodecDevice):LongWord; 

function CodecDeviceCheck(Codec:PCodecDevice):PCodecDevice;

procedure CodecLog(Level:LongWord;Codec:PCodecDevice;const AText:String);
procedure CodecLogInfo(Codec:PCodecDevice;const AText:String); inline;
procedure CodecLogError(Codec:PCodecDevice;const AText:String); inline;
procedure CodecLogDebug(Codec:PCodecDevice;const AText:String); inline;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {Codec specific variables}
 CodecInitialized:Boolean;

 CodecDeviceTable:PCodecDevice;
 CodecDeviceTableLock:TCriticalSectionHandle = INVALID_HANDLE_VALUE;
 CodecDeviceTableCount:LongWord;

 CodecDeviceDefault:PCodecDevice;
 
{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure CodecInit;
{Initialize the Codec unit and Codec device table}

{Note: Called only during system startup}
begin
 {}
 {Check Initialized}
 if CodecInitialized then Exit;
 
 {Initialize Logging}
 CODEC_LOG_ENABLED:=(CODEC_DEFAULT_LOG_LEVEL <> CODEC_LOG_LEVEL_NONE); 
 
 {Initialize Codec Device Table}
 CodecDeviceTable:=nil;
 CodecDeviceTableLock:=CriticalSectionCreate; 
 CodecDeviceTableCount:=0;
 if CodecDeviceTableLock = INVALID_HANDLE_VALUE then
  begin
   if CODEC_LOG_ENABLED then CodecLogError(nil,'Failed to create Codec device table lock');
  end;
 CodecDeviceDefault:=nil;
 
 {Register Platform Codec Handlers}
 //To Do
 
 CodecInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{Codec Device Functions}
//To Do

{==============================================================================}
 
function CodecDeviceProperties(Codec:PCodecDevice;Properties:PCodecProperties):LongWord;
{Get the properties for the specified Codec device}
{Codec: The Codec device to get properties from}
{Properties: Pointer to a TCodecProperties structure to fill in}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Properties}
 if Properties = nil then Exit;
 
 {Check Codec}
 if Codec = nil then Exit;
 if Codec.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IFDEF CODEC_DEBUG}
 if CODEC_LOG_ENABLED then CodecLogDebug(Codec,'Codec Device Properties');
 {$ENDIF}
 
 {Check Enabled}
 {Result:=ERROR_NOT_SUPPORTED;}
 {if Codec.CodecState <> CODEC_STATE_ENABLED then Exit;} {Allow when disabled}
 
 if MutexLock(Codec.Lock) = ERROR_SUCCESS then
  begin
   if Assigned(Codec.DeviceProperties) then
    begin
     {Call Device Properites}
     Result:=Codec.DeviceProperties(Codec,Properties);
    end
   else
    begin
     {Get Properties}
     System.Move(Codec.Properties,Properties^,SizeOf(TCodecProperties));
       
     {Return Result}
     Result:=ERROR_SUCCESS;
    end;  
    
   MutexUnlock(Codec.Lock);
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;    
end;

{==============================================================================}

function CodecDeviceCreate:PCodecDevice;
{Create a new Codec device entry}
{Return: Pointer to new Codec device entry or nil if Codec device could not be created}
begin
 {}
 Result:=CodecDeviceCreateEx(SizeOf(TCodecDevice));
end;

{==============================================================================}

function CodecDeviceCreateEx(Size:LongWord):PCodecDevice;
{Create a new Codec device entry}
{Size: Size in bytes to allocate for new Codec device (Including the Codec device entry)}
{Return: Pointer to new Codec device entry or nil if Codec device could not be created}
begin
 {}
 Result:=nil;
 
 {Check Size}
 if Size < SizeOf(TCodecDevice) then Exit;
 
 {Create Codec}
 Result:=PCodecDevice(DeviceCreateEx(Size));
 if Result = nil then Exit;
 
 {Update Device}
 Result.Device.DeviceBus:=DEVICE_BUS_NONE;   
 Result.Device.DeviceType:=CODEC_TYPE_NONE;
 Result.Device.DeviceFlags:=CODEC_FLAG_NONE;
 Result.Device.DeviceData:=nil;

 {Update Codec}
 Result.CodecId:=DEVICE_ID_ANY;
 Result.CodecState:=CODEC_STATE_DISABLED;
 //To Do
 Result.DeviceProperties:=nil;
 Result.Lock:=INVALID_HANDLE_VALUE;
 
 {Create Lock}
 Result.Lock:=MutexCreateEx(False,MUTEX_DEFAULT_SPINCOUNT,MUTEX_FLAG_RECURSIVE);
 if Result.Lock = INVALID_HANDLE_VALUE then
  begin
   if CODEC_LOG_ENABLED then CodecLogError(nil,'Failed to create lock for Codec device');
   CodecDeviceDestroy(Result);
   Result:=nil;
   Exit;
  end;
end;

{==============================================================================}

function CodecDeviceDestroy(Codec:PCodecDevice):LongWord;
{Destroy an existing Codec device entry}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Codec}
 if Codec = nil then Exit;
 if Codec.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check Codec}
 Result:=ERROR_IN_USE;
 if CodecDeviceCheck(Codec) = Codec then Exit;

 {Check State}
 if Codec.Device.DeviceState <> DEVICE_STATE_UNREGISTERED then Exit;
 
 {Destroy Lock}
 if Codec.Lock <> INVALID_HANDLE_VALUE then
  begin
   MutexDestroy(Codec.Lock);
  end;
 
 {Destroy Codec} 
 Result:=DeviceDestroy(@Codec.Device);
end;

{==============================================================================}

function CodecDeviceRegister(Codec:PCodecDevice):LongWord;
{Register a new Codec device in the Codec device table}
var
 CodecId:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Codec}
 if Codec = nil then Exit;
 if Codec.CodecId <> DEVICE_ID_ANY then Exit;
 if Codec.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check Interfaces}
 //To Do
 
 {Check Codec}
 Result:=ERROR_ALREADY_EXISTS;
 if CodecDeviceCheck(Codec) = Codec then Exit;
 
 {Check State}
 if Codec.Device.DeviceState <> DEVICE_STATE_UNREGISTERED then Exit;
 
 {Insert Codec}
 if CriticalSectionLock(CodecDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Update Codec}
    CodecId:=0;
    while CodecDeviceFind(CodecId) <> nil do
     begin
      Inc(CodecId);
     end;
    Codec.CodecId:=CodecId;
    
    {Update Device}
    Codec.Device.DeviceName:=CODEC_NAME_PREFIX + IntToStr(Codec.CodecId); 
    Codec.Device.DeviceClass:=DEVICE_CLASS_CODEC;
    
    {Register Device}
    Result:=DeviceRegister(@Codec.Device);
    if Result <> ERROR_SUCCESS then
     begin
      Codec.CodecId:=DEVICE_ID_ANY;
      Exit;
     end; 
    
    {Link Codec}
    if CodecDeviceTable = nil then
     begin
      CodecDeviceTable:=Codec;
     end
    else
     begin
      Codec.Next:=CodecDeviceTable;
      CodecDeviceTable.Prev:=Codec;
      CodecDeviceTable:=Codec;
     end;
 
    {Increment Count}
    Inc(CodecDeviceTableCount);
    
    {Check Default}
    if CodecDeviceDefault = nil then
     begin
      CodecDeviceDefault:=Codec;
     end;
    
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    CriticalSectionUnlock(CodecDeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;

{==============================================================================}

function CodecDeviceDeregister(Codec:PCodecDevice):LongWord;
{Deregister an Codec device from the Codec device table}
var
 Prev:PCodecDevice;
 Next:PCodecDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Codec}
 if Codec = nil then Exit;
 if Codec.CodecId = DEVICE_ID_ANY then Exit;
 if Codec.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check Codec}
 Result:=ERROR_NOT_FOUND;
 if CodecDeviceCheck(Codec) <> Codec then Exit;
 
 {Check State}
 if Codec.Device.DeviceState <> DEVICE_STATE_REGISTERED then Exit;
 
 {Remove Codec}
 if CriticalSectionLock(CodecDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Deregister Device}
    Result:=DeviceDeregister(@Codec.Device);
    if Result <> ERROR_SUCCESS then Exit;
    
    {Unlink Codec}
    Prev:=Codec.Prev;
    Next:=Codec.Next;
    if Prev = nil then
     begin
      CodecDeviceTable:=Next;
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
    Dec(CodecDeviceTableCount);
 
    {Check Default}
    if CodecDeviceDefault = Codec then
     begin
      CodecDeviceDefault:=CodecDeviceTable;
     end;
 
    {Update Codec}
    Codec.CodecId:=DEVICE_ID_ANY;
 
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    CriticalSectionUnlock(CodecDeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;

{==============================================================================}

function CodecDeviceFind(CodecId:LongWord):PCodecDevice;
var
 Codec:PCodecDevice;
begin
 {}
 Result:=nil;
 
 {Check Id}
 if CodecId = DEVICE_ID_ANY then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(CodecDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Codec}
    Codec:=CodecDeviceTable;
    while Codec <> nil do
     begin
      {Check State}
      if Codec.Device.DeviceState = DEVICE_STATE_REGISTERED then
       begin
        {Check Id}
        if Codec.CodecId = CodecId then
         begin
          Result:=Codec;
          Exit;
         end;
       end;
       
      {Get Next}
      Codec:=Codec.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(CodecDeviceTableLock);
   end;
  end;
end;

{==============================================================================}

function CodecDeviceFindByName(const Name:String):PCodecDevice; inline;
begin
 {}
 Result:=PCodecDevice(DeviceFindByName(Name));
end;

{==============================================================================}

function CodecDeviceFindByDescription(const Description:String):PCodecDevice; inline;
begin
 {}
 Result:=PCodecDevice(DeviceFindByDescription(Description));
end;
       
{==============================================================================}

function CodecDeviceEnumerate(Callback:TCodecEnumerate;Data:Pointer):LongWord;
var
 Codec:PCodecDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Callback}
 if not Assigned(Callback) then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(CodecDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Codec}
    Codec:=CodecDeviceTable;
    while Codec <> nil do
     begin
      {Check State}
      if Codec.Device.DeviceState = DEVICE_STATE_REGISTERED then
       begin
        if Callback(Codec,Data) <> ERROR_SUCCESS then Exit;
       end;
       
      {Get Next}
      Codec:=Codec.Next;
     end;
     
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    CriticalSectionUnlock(CodecDeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;

{==============================================================================}

function CodecDeviceNotification(Codec:PCodecDevice;Callback:TCodecNotification;Data:Pointer;Notification,Flags:LongWord):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Codec}
 if Codec = nil then
  begin
   Result:=DeviceNotification(nil,DEVICE_CLASS_Codec,Callback,Data,Notification,Flags);
  end
 else
  begin 
   {Check Codec}
   if Codec.Device.Signature <> DEVICE_SIGNATURE then Exit;

   Result:=DeviceNotification(@Codec.Device,DEVICE_CLASS_CODEC,Callback,Data,Notification,Flags);
  end; 
end;

{==============================================================================}
{==============================================================================}
{RTL Codec Functions}

{==============================================================================}
{==============================================================================}
{Codec Helper Functions}
function CodecGetCount:LongWord; inline;
{Get the current Codec device count}
begin
 {}
 Result:=CodecDeviceTableCount;
end;

{==============================================================================}

function CodecDeviceGetDefault:PCodecDevice; inline;
{Get the current default Codec device}
begin
 {}
 Result:=CodecDeviceDefault;
end;

{==============================================================================}

function CodecDeviceSetDefault(Codec:PCodecDevice):LongWord; 
{Set the current default Codec device}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Codec}
 if Codec = nil then Exit;
 if Codec.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(CodecDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Check Codec}
    if CodecDeviceCheck(Codec) <> Codec then Exit;
    
    {Set Codec Default}
    CodecDeviceDefault:=Codec;
    
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    CriticalSectionUnlock(CodecDeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function CodecDeviceCheck(Codec:PCodecDevice):PCodecDevice;
{Check if the supplied Codec device is in the Codec device table}
var
 Current:PCodecDevice;
begin
 {}
 Result:=nil;
 
 {Check Codec}
 if Codec = nil then Exit;
 if Codec.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(CodecDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Codec}
    Current:=CodecDeviceTable;
    while Current <> nil do
     begin
      {Check Codec}
      if Current = Codec then
       begin
        Result:=Codec;
        Exit;
       end;
      
      {Get Next}
      Current:=Current.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(CodecDeviceTableLock);
   end;
  end;
end;

{==============================================================================}

procedure CodecLog(Level:LongWord;Codec:PCodecDevice;const AText:String);
var
 WorkBuffer:String;
begin
 {}
 {Check Level}
 if Level < CODEC_DEFAULT_LOG_LEVEL then Exit;
 
 WorkBuffer:='';
 {Check Level}
 if Level = CODEC_LOG_LEVEL_DEBUG then
  begin
   WorkBuffer:=WorkBuffer + '[DEBUG] ';
  end
 else if Level = CODEC_LOG_LEVEL_ERROR then
  begin
   WorkBuffer:=WorkBuffer + '[ERROR] ';
  end;
 
 {Add Prefix}
 WorkBuffer:=WorkBuffer + 'Codec: ';
 
 {Check Codec}
 if Codec <> nil then
  begin
   WorkBuffer:=WorkBuffer + CODEC_NAME_PREFIX + IntToStr(Codec.CodecId) + ': ';
  end;

 {Output Logging}  
 LoggingOutputEx(LOGGING_FACILITY_CODEC,LogLevelToLoggingSeverity(Level),'Codec',WorkBuffer + AText);
end;

{==============================================================================}

procedure CodecLogInfo(Codec:PCodecDevice;const AText:String); inline;
begin
 {}
 CodecLog(CODEC_LOG_LEVEL_INFO,Codec,AText);
end;

{==============================================================================}

procedure CodecLogError(Codec:PCodecDevice;const AText:String); inline;
begin
 {}
 CodecLog(CODEC_LOG_LEVEL_ERROR,Codec,AText);
end;

{==============================================================================}

procedure CodecLogDebug(Codec:PCodecDevice;const AText:String); inline;
begin
 {}
 CodecLog(CODEC_LOG_LEVEL_DEBUG,Codec,AText);
end;

{==============================================================================}
{==============================================================================}

initialization
 CodecInit;

{==============================================================================}
 
finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
