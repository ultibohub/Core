{
Ultibo Storage interface unit.

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


Storage Devices
===============

 This unit provides the generic Storage device interface used by all storage drivers.

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit Storage;

interface

uses
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  Devices,
  SysUtils;

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
const
 {Storage specific constants}
 STORAGE_NAME_PREFIX = 'Storage';  {Name prefix for Storage Devices}

 STORAGE_STATUS_TIMER_INTERVAL = 1000;

 {Storage Device Types}
 STORAGE_TYPE_NONE      = 0;
 STORAGE_TYPE_HDD       = 1;
 STORAGE_TYPE_FDD       = 2;
 STORAGE_TYPE_CDROM     = 3;
 STORAGE_TYPE_OPTICAL   = 4;
 STORAGE_TYPE_TAPE      = 5;
 STORAGE_TYPE_REMOVABLE = 6;

 STORAGE_TYPE_MAX       = 6;

 {Storage Type Names}
 STORAGE_TYPE_NAMES:array[STORAGE_TYPE_NONE..STORAGE_TYPE_MAX] of String = (
  'STORAGE_TYPE_NONE',
  'STORAGE_TYPE_HDD',
  'STORAGE_TYPE_FDD',
  'STORAGE_TYPE_CDROM',
  'STORAGE_TYPE_OPTICAL',
  'STORAGE_TYPE_TAPE',
  'STORAGE_TYPE_REMOVABLE');

 {Storage Device States}
 STORAGE_STATE_EJECTED   = 0;
 STORAGE_STATE_EJECTING  = 1;
 STORAGE_STATE_INSERTING = 2;
 STORAGE_STATE_INSERTED  = 3;

 STORAGE_STATE_MAX       = 3;

 {Storage State Names}
 STORAGE_STATE_NAMES:array[STORAGE_STATE_EJECTED..STORAGE_STATE_MAX] of String = (
  'STORAGE_STATE_EJECTED',
  'STORAGE_STATE_EJECTING',
  'STORAGE_STATE_INSERTING',
  'STORAGE_STATE_INSERTED');

 {Storage Device Flags}
 STORAGE_FLAG_NONE       = $00000000;
 STORAGE_FLAG_REMOVABLE  = $00000001;
 STORAGE_FLAG_LBA48      = $00000002;
 STORAGE_FLAG_NOT_READY  = $00000004;
 STORAGE_FLAG_NO_MEDIA   = $00000008;
 STORAGE_FLAG_READ_ONLY  = $00000010;
 STORAGE_FLAG_WRITE_ONLY = $00000020;
 STORAGE_FLAG_ERASEABLE  = $00000040;
 STORAGE_FLAG_LOCKABLE   = $00000080;
 STORAGE_FLAG_LOCKED     = $00000100;
 STORAGE_FLAG_EJECTABLE  = $00000200;
 STORAGE_FLAG_CHANGABLE  = $00000400;

 {Storage Device Control Codes}
 STORAGE_CONTROL_TEST_READY       = 1;  {Test Unit Ready}
 STORAGE_CONTROL_RESET            = 2;  {Reset Device}
 STORAGE_CONTROL_TEST_MEDIA       = 3;  {Test No Media}
 STORAGE_CONTROL_LOCK             = 4;  {Lock Media}
 STORAGE_CONTROL_UNLOCK           = 5;  {Unlock Media}
 STORAGE_CONTROL_EJECT            = 6;  {Eject Media}
 STORAGE_CONTROL_TEST_LOCKED      = 7;  {Test Media Locked}
 STORAGE_CONTROL_TEST_CHANGED     = 8;  {Test Media Changed}
 STORAGE_CONTROL_GET_VENDORID     = 9;  {Get Vendor ID}
 STORAGE_CONTROL_GET_PRODUCTID    = 10; {Get Product ID}
 STORAGE_CONTROL_GET_SERIAL       = 11; {Get Serial No}
 STORAGE_CONTROL_GET_REVISION     = 12; {Get Revision No}
 STORAGE_CONTROL_GET_PRODUCT      = 13; {Get Product Name}
 STORAGE_CONTROL_GET_MANUFACTURER = 14; {Get Manufacturer Name}
 //To Do //Stats/Info/Disable etc

 {Storage logging}
 STORAGE_LOG_LEVEL_DEBUG     = LOG_LEVEL_DEBUG;  {Storage debugging messages}
 STORAGE_LOG_LEVEL_INFO      = LOG_LEVEL_INFO;   {Storage informational messages, such as a device being attached or detached}
 STORAGE_LOG_LEVEL_WARN      = LOG_LEVEL_WARN;   {Storage warning messages}
 STORAGE_LOG_LEVEL_ERROR     = LOG_LEVEL_ERROR;  {Storage error messages}
 STORAGE_LOG_LEVEL_NONE      = LOG_LEVEL_NONE;   {No Storage messages}

var
 STORAGE_DEFAULT_LOG_LEVEL:LongWord = STORAGE_LOG_LEVEL_DEBUG; {Minimum level for Storage messages.  Only messages with level greater than or equal to this will be printed}

var
 {Storage logging}
 STORAGE_LOG_ENABLED:Boolean;

{==============================================================================}
type
 {Storage specific types}
 {Storage Device}
 PStorageDevice = ^TStorageDevice;

 {Storage Enumeration Callback}
 TStorageEnumerate = function(Storage:PStorageDevice;Data:Pointer):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 {Storage Notification Callback}
 TStorageNotification = function(Device:PDevice;Data:Pointer;Notification:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}

 {Storage Device Methods}
 TStorageDeviceRead = function(Storage:PStorageDevice;const Start,Count:Int64;Buffer:Pointer):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TStorageDeviceWrite = function(Storage:PStorageDevice;const Start,Count:Int64;Buffer:Pointer):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TStorageDeviceErase = function(Storage:PStorageDevice;const Start,Count:Int64):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TStorageDeviceControl = function(Storage:PStorageDevice;Request:Integer;Argument1:PtrUInt;var Argument2:PtrUInt):LongWord;{$IFDEF i386} stdcall;{$ENDIF}

 TStorageDevice = record
  {Device Properties}
  Device:TDevice;                      {The Device entry for this Storage}
  {Storage Properties}
  StorageId:LongWord;                  {Unique Id of this Storage in the Storage table}
  StorageState:LongWord;               {Storage state (eg STORAGE_STATE_INSERTED)}
  DeviceRead:TStorageDeviceRead;       {A Device specific DeviceRead method implementing a standard Storage device interface}
  DeviceWrite:TStorageDeviceWrite;     {A Device specific DeviceWrite method implementing a standard Storage device interface}
  DeviceErase:TStorageDeviceErase;     {A Device specific DeviceErase method implementing a standard Storage device interface}
  DeviceControl:TStorageDeviceControl; {A Device specific DeviceControl method implementing a standard Storage device interface}
  {Driver Properties}
  Lock:TMutexHandle;                   {Storage lock}
  TargetID:LongWord;                   {SCSI ID}
  TargetLUN:LongWord;                  {LUN}
  BlockSize:LongWord;                  {Block Size}
  BlockCount:Int64;                    {Number of Blocks}
  BlockShift:LongWord;                 {Shift Count for Blocks to Bytes conversion (eg 9 for 512 byte blocks)}
  Vendor:PChar;                        {ATA Model, SCSI Vendor}
  Product:PChar;                       {ATA Serial No, SCSI Product}
  Revision:PChar;                      {Firmware Revision}
  StatusTimer:TTimerHandle;            {Timer for status change detection}
  {Statistics Properties}
  ReadCount:UInt64;
  ReadErrors:UInt64;
  WriteCount:UInt64;
  WriteErrors:UInt64;
  EraseCount:UInt64;
  EraseErrors:UInt64;
  {Internal Properties}
  Prev:PStorageDevice;                 {Previous entry in Storage table}
  Next:PStorageDevice;                 {Next entry in Storage table}
 end;

{==============================================================================}
{var}
 {Storage specific variables}

{==============================================================================}
{Initialization Functions}
procedure StorageInit;

{==============================================================================}
{Storage Functions}
function StorageDeviceRead(Storage:PStorageDevice;const Start,Count:Int64;Buffer:Pointer):LongWord;
function StorageDeviceWrite(Storage:PStorageDevice;const Start,Count:Int64;Buffer:Pointer):LongWord;
function StorageDeviceErase(Storage:PStorageDevice;const Start,Count:Int64):LongWord;
function StorageDeviceControl(Storage:PStorageDevice;Request:Integer;Argument1:PtrUInt;var Argument2:PtrUInt):LongWord;

function StorageDeviceSetState(Storage:PStorageDevice;State:LongWord):LongWord;

function StorageDeviceStartStatus(Storage:PStorageDevice;Interval:LongWord):LongWord;
function StorageDeviceStopStatus(Storage:PStorageDevice):LongWord;

function StorageDeviceCreate:PStorageDevice;
function StorageDeviceCreateEx(Size:LongWord):PStorageDevice;
function StorageDeviceDestroy(Storage:PStorageDevice):LongWord;

function StorageDeviceRegister(Storage:PStorageDevice):LongWord;
function StorageDeviceDeregister(Storage:PStorageDevice):LongWord;

function StorageDeviceFind(StorageId:LongWord):PStorageDevice;
function StorageDeviceFindByDevice(Device:PDevice):PStorageDevice;
function StorageDeviceFindByName(const Name:String):PStorageDevice; inline;
function StorageDeviceFindByDescription(const Description:String):PStorageDevice; inline;
function StorageDeviceEnumerate(Callback:TStorageEnumerate;Data:Pointer):LongWord;

function StorageDeviceNotification(Storage:PStorageDevice;Callback:TStorageNotification;Data:Pointer;Notification,Flags:LongWord):LongWord;

{==============================================================================}
{Storage Helper Functions}
function StorageGetCount:LongWord;

function StorageDeviceCheck(Storage:PStorageDevice):PStorageDevice;

function StorageDeviceTypeToString(StorageType:LongWord):String;
function StorageDeviceStateToString(StorageState:LongWord):String;

function StorageDeviceStateToNotification(State:LongWord):LongWord;

procedure StorageLog(Level:LongWord;Storage:PStorageDevice;const AText:String);
procedure StorageLogInfo(Storage:PStorageDevice;const AText:String); inline;
procedure StorageLogWarn(Storage:PStorageDevice;const AText:String); inline;
procedure StorageLogError(Storage:PStorageDevice;const AText:String); inline;
procedure StorageLogDebug(Storage:PStorageDevice;const AText:String); inline;

procedure StorageStatusTimer(Storage:PStorageDevice);

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {Storage specific variables}
 StorageInitialized:Boolean;

 StorageTable:PStorageDevice;
 StorageTableLock:TCriticalSectionHandle = INVALID_HANDLE_VALUE;
 StorageTableCount:LongWord;

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure StorageInit;
begin
 {}
 {Check Initialized}
 if StorageInitialized then Exit;

 {Initialize Logging}
 STORAGE_LOG_ENABLED:=(STORAGE_DEFAULT_LOG_LEVEL <> STORAGE_LOG_LEVEL_NONE);

 {Initialize Storage Table}
 StorageTable:=nil;
 StorageTableLock:=CriticalSectionCreate;
 StorageTableCount:=0;
 if StorageTableLock = INVALID_HANDLE_VALUE then
  begin
   if STORAGE_LOG_ENABLED then StorageLogError(nil,'Failed to create storage table lock');
  end;

 StorageInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{Storage Functions}
function StorageDeviceRead(Storage:PStorageDevice;const Start,Count:Int64;Buffer:Pointer):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Storage}
 if Storage = nil then Exit;
 if Storage.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Method}
 if not Assigned(Storage.DeviceRead) then Exit;

 {Call Read}
 Result:=Storage.DeviceRead(Storage,Start,Count,Buffer);
end;

{==============================================================================}

function StorageDeviceWrite(Storage:PStorageDevice;const Start,Count:Int64;Buffer:Pointer):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Storage}
 if Storage = nil then Exit;
 if Storage.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Method}
 if not Assigned(Storage.DeviceWrite) then Exit;

 {Call Write}
 Result:=Storage.DeviceWrite(Storage,Start,Count,Buffer);
end;

{==============================================================================}

function StorageDeviceErase(Storage:PStorageDevice;const Start,Count:Int64):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Storage}
 if Storage = nil then Exit;
 if Storage.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Method}
 if not Assigned(Storage.DeviceErase) then Exit;

 {Call Erase}
 Result:=Storage.DeviceErase(Storage,Start,Count);
end;

{==============================================================================}

function StorageDeviceControl(Storage:PStorageDevice;Request:Integer;Argument1:PtrUInt;var Argument2:PtrUInt):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Storage}
 if Storage = nil then Exit;
 if Storage.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Method}
 if not Assigned(Storage.DeviceControl) then Exit;

 {Call Control}
 Result:=Storage.DeviceControl(Storage,Request,Argument1,Argument2);
end;

{==============================================================================}

function StorageDeviceSetState(Storage:PStorageDevice;State:LongWord):LongWord;
{Set the state of the specified storage and send a notification}
{Storage: The storage to set the state for}
{State: The new state to set and notify}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Storage}
 if Storage = nil then Exit;
 if Storage.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check State}
 if State > STORAGE_STATE_INSERTED then Exit;

 {Check State}
 if Storage.StorageState = State then
  begin
   {Return Result}
   Result:=ERROR_SUCCESS;
  end
 else
  begin
   {Acquire the Lock}
   if MutexLock(Storage.Lock) = ERROR_SUCCESS then
    begin
     try
      {Set State}
      Storage.StorageState:=State;

      {Notify State}
      NotifierNotify(@Storage.Device,StorageDeviceStateToNotification(State));

      {Return Result}
      Result:=ERROR_SUCCESS;
     finally
      {Release the Lock}
      MutexUnlock(Storage.Lock);
     end;
    end
   else
    begin
     Result:=ERROR_CAN_NOT_COMPLETE;
    end;
  end;
end;

{==============================================================================}

function StorageDeviceStartStatus(Storage:PStorageDevice;Interval:LongWord):LongWord;
{Start status monitoring on the specified storage for insert/eject notifications}
{Storage: The storage to start status monitoring for}
{Interval: The status monitoring interval in milliseconds}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Storage}
 if Storage = nil then Exit;
 if Storage.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Timer}
 if Storage.StatusTimer <> INVALID_HANDLE_VALUE then
  begin
   {Return Result}
   Result:=ERROR_SUCCESS;
  end
 else
  begin
   {Acquire the Lock}
   if MutexLock(Storage.Lock) = ERROR_SUCCESS then
    begin
     try
      {Check Interval}
      if Interval < 1 then Interval:=STORAGE_STATUS_TIMER_INTERVAL;

      {Create Timer}
      Storage.StatusTimer:=TimerCreateEx(Interval,TIMER_STATE_ENABLED,TIMER_FLAG_WORKER,TTimerEvent(StorageStatusTimer),Storage); {Rescheduled by Timer Event}
      if Storage.StatusTimer = INVALID_HANDLE_VALUE then
       begin
        Result:=ERROR_OPERATION_FAILED;
        Exit;
       end;

      {Return Result}
      Result:=ERROR_SUCCESS;
     finally
      {Release the Lock}
      MutexUnlock(Storage.Lock);
     end;
    end
   else
    begin
     Result:=ERROR_CAN_NOT_COMPLETE;
    end;
  end;
end;

{==============================================================================}

function StorageDeviceStopStatus(Storage:PStorageDevice):LongWord;
{Stop status monitoring on the specified storage for insert/eject notifications}
{Storage: The storage to stop status monitoring for}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Storage}
 if Storage = nil then Exit;
 if Storage.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Timer}
 if Storage.StatusTimer = INVALID_HANDLE_VALUE then
  begin
   {Return Result}
   Result:=ERROR_SUCCESS;
  end
 else
  begin
   {Acquire the Lock}
   if MutexLock(Storage.Lock) = ERROR_SUCCESS then
    begin
     try
      {Destroy Timer}
      Result:=TimerDestroy(Storage.StatusTimer);
      if Result <> ERROR_SUCCESS then Exit;

      Storage.StatusTimer:=INVALID_HANDLE_VALUE;
     finally
      {Release the Lock}
      MutexUnlock(Storage.Lock);
     end;
    end
   else
    begin
     Result:=ERROR_CAN_NOT_COMPLETE;
    end;
  end;
end;

{==============================================================================}

function StorageDeviceCreate:PStorageDevice;
{Create a new Storage entry}
{Return: Pointer to new Storage entry or nil if storage could not be created}
begin
 {}
 Result:=StorageDeviceCreateEx(SizeOf(TStorageDevice));
end;

{==============================================================================}

function StorageDeviceCreateEx(Size:LongWord):PStorageDevice;
{Create a new Storage entry}
{Size: Size in bytes to allocate for new storage (Including the storage entry)}
{Return: Pointer to new Storage entry or nil if storage could not be created}
begin
 {}
 Result:=nil;

 {Check Size}
 if Size < SizeOf(TStorageDevice) then Exit;

 {Create Storage}
 Result:=PStorageDevice(DeviceCreateEx(Size));
 if Result = nil then Exit;

 {Update Device}
 Result.Device.DeviceBus:=DEVICE_BUS_NONE;
 Result.Device.DeviceType:=STORAGE_TYPE_NONE;
 Result.Device.DeviceFlags:=STORAGE_FLAG_NONE;
 Result.Device.DeviceData:=nil;

 {Update Storage}
 Result.StorageId:=DEVICE_ID_ANY;
 Result.StorageState:=STORAGE_STATE_EJECTED;
 Result.DeviceRead:=nil;
 Result.DeviceWrite:=nil;
 Result.DeviceErase:=nil;
 Result.DeviceControl:=nil;
 Result.Lock:=INVALID_HANDLE_VALUE;
 Result.Vendor:=nil;
 Result.Product:=nil;
 Result.Revision:=nil;
 Result.StatusTimer:=INVALID_HANDLE_VALUE;

 {Create Lock}
 Result.Lock:=MutexCreateEx(False,MUTEX_DEFAULT_SPINCOUNT,MUTEX_FLAG_RECURSIVE);
 if Result.Lock = INVALID_HANDLE_VALUE then
  begin
   if STORAGE_LOG_ENABLED then StorageLogError(nil,'Failed to create lock for storage device');

   {Destroy Storage}
   StorageDeviceDestroy(Result);

   {Return Result}
   Result:=nil;
   Exit;
  end;
end;

{==============================================================================}

function StorageDeviceDestroy(Storage:PStorageDevice):LongWord;
{Destroy an existing Storage entry}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Storage}
 if Storage = nil then Exit;
 if Storage.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Storage}
 Result:=ERROR_IN_USE;
 if StorageDeviceCheck(Storage) = Storage then Exit;

 {Check State}
 if Storage.Device.DeviceState <> DEVICE_STATE_UNREGISTERED then Exit;

 {Destroy Lock}
 if Storage.Lock <> INVALID_HANDLE_VALUE then
  begin
   MutexDestroy(Storage.Lock);
  end;

 {Destroy Timer}
 if Storage.StatusTimer <> INVALID_HANDLE_VALUE then
  begin
   TimerDestroy(Storage.StatusTimer);
  end;

 {Update Storage}
 if Storage.Vendor <> nil then FreeMem(Storage.Vendor);
 if Storage.Product <> nil then FreeMem(Storage.Product);
 if Storage.Revision <> nil then FreeMem(Storage.Revision);

 {Destroy Storage}
 Result:=DeviceDestroy(@Storage.Device);
end;

{==============================================================================}

function StorageDeviceRegister(Storage:PStorageDevice):LongWord;
{Register a new Storage in the Storage table}
var
 StorageId:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Storage}
 if Storage = nil then Exit;
 if Storage.StorageId <> DEVICE_ID_ANY then Exit;
 if Storage.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Storage}
 Result:=ERROR_ALREADY_EXISTS;
 if StorageDeviceCheck(Storage) = Storage then Exit;

 {Check State}
 if Storage.Device.DeviceState <> DEVICE_STATE_UNREGISTERED then Exit;

 {Insert Storage}
 if CriticalSectionLock(StorageTableLock) = ERROR_SUCCESS then
  begin
   try
    {Update Storage}
    StorageId:=0;
    while StorageDeviceFind(StorageId) <> nil do
     begin
      Inc(StorageId);
     end;
    Storage.StorageId:=StorageId;

    {Update Device}
    Storage.Device.DeviceName:=STORAGE_NAME_PREFIX + IntToStr(Storage.StorageId);
    Storage.Device.DeviceClass:=DEVICE_CLASS_STORAGE;

    {Register Device}
    Result:=DeviceRegister(@Storage.Device);
    if Result <> ERROR_SUCCESS then
     begin
      Storage.StorageId:=DEVICE_ID_ANY;
      Exit;
     end;

    {Link Storage}
    if StorageTable = nil then
     begin
      StorageTable:=Storage;
     end
    else
     begin
      Storage.Next:=StorageTable;
      StorageTable.Prev:=Storage;
      StorageTable:=Storage;
     end;

    {Increment Count}
    Inc(StorageTableCount);

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    CriticalSectionUnlock(StorageTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function StorageDeviceDeregister(Storage:PStorageDevice):LongWord;
{Deregister a Storage from the Storage table}
var
 Prev:PStorageDevice;
 Next:PStorageDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Storage}
 if Storage = nil then Exit;
 if Storage.StorageId = DEVICE_ID_ANY then Exit;
 if Storage.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Storage}
 Result:=ERROR_NOT_FOUND;
 if StorageDeviceCheck(Storage) <> Storage then Exit;

 {Check State}
 if Storage.Device.DeviceState <> DEVICE_STATE_REGISTERED then Exit;

 {Remove Storage}
 if CriticalSectionLock(StorageTableLock) = ERROR_SUCCESS then
  begin
   try
    {Deregister Device}
    Result:=DeviceDeregister(@Storage.Device);
    if Result <> ERROR_SUCCESS then Exit;

    {Unlink Storage}
    Prev:=Storage.Prev;
    Next:=Storage.Next;
    if Prev = nil then
     begin
      StorageTable:=Next;
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
    Dec(StorageTableCount);

    {Update Storage}
    Storage.StorageId:=DEVICE_ID_ANY;

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    CriticalSectionUnlock(StorageTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function StorageDeviceFind(StorageId:LongWord):PStorageDevice;
var
 Storage:PStorageDevice;
begin
 {}
 Result:=nil;

 {Check Id}
 if StorageId = DEVICE_ID_ANY then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(StorageTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Storage}
    Storage:=StorageTable;
    while Storage <> nil do
     begin
      {Check State}
      if Storage.Device.DeviceState = DEVICE_STATE_REGISTERED then
       begin
        {Check Id}
        if Storage.StorageId = StorageId then
         begin
          Result:=Storage;
          Exit;
         end;
       end;

      {Get Next}
      Storage:=Storage.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(StorageTableLock);
   end;
  end;
end;

{==============================================================================}

function StorageDeviceFindByDevice(Device:PDevice):PStorageDevice;
{Find a Storage device by the matching DeviceData property}
{Device: The device entry to match with the DeviceData value}
{Return: The Storage device matched or nil if none found}
var
 Storage:PStorageDevice;
begin
 {}
 Result:=nil;

 {Check Device}
 if Device = nil then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(StorageTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Storage}
    Storage:=StorageTable;
    while Storage <> nil do
     begin
      {Check State}
      if Storage.Device.DeviceState = DEVICE_STATE_REGISTERED then
       begin
        {Check Device}
        if Storage.Device.DeviceData = Device then
         begin
          Result:=Storage;
          Exit;
         end;
       end;

      {Get Next}
      Storage:=Storage.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(StorageTableLock);
   end;
  end;
end;

{==============================================================================}

function StorageDeviceFindByName(const Name:String):PStorageDevice; inline;
begin
 {}
 Result:=PStorageDevice(DeviceFindByName(Name));
end;

{==============================================================================}

function StorageDeviceFindByDescription(const Description:String):PStorageDevice; inline;
begin
 {}
 Result:=PStorageDevice(DeviceFindByDescription(Description));
end;

{==============================================================================}

function StorageDeviceEnumerate(Callback:TStorageEnumerate;Data:Pointer):LongWord;
var
 Storage:PStorageDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Callback}
 if not Assigned(Callback) then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(StorageTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Storage}
    Storage:=StorageTable;
    while Storage <> nil do
     begin
      {Check State}
      if Storage.Device.DeviceState = DEVICE_STATE_REGISTERED then
       begin
        if Callback(Storage,Data) <> ERROR_SUCCESS then Exit;
       end;

      {Get Next}
      Storage:=Storage.Next;
     end;

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    CriticalSectionUnlock(StorageTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function StorageDeviceNotification(Storage:PStorageDevice;Callback:TStorageNotification;Data:Pointer;Notification,Flags:LongWord):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Storage}
 if Storage = nil then
  begin
   Result:=DeviceNotification(nil,DEVICE_CLASS_STORAGE,Callback,Data,Notification,Flags);
  end
 else
  begin
   {Check Storage}
   if Storage.Device.Signature <> DEVICE_SIGNATURE then Exit;

   Result:=DeviceNotification(@Storage.Device,DEVICE_CLASS_STORAGE,Callback,Data,Notification,Flags);
  end;
end;

{==============================================================================}
{==============================================================================}
{Storage Helper Functions}
function StorageGetCount:LongWord;
{Get the current storage count}
begin
 {}
 Result:=StorageTableCount;
end;

{==============================================================================}

function StorageDeviceCheck(Storage:PStorageDevice):PStorageDevice;
{Check if the supplied Storage is in the storage table}
var
 Current:PStorageDevice;
begin
 {}
 Result:=nil;

 {Check Storage}
 if Storage = nil then Exit;
 if Storage.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(StorageTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Storage}
    Current:=StorageTable;
    while Current <> nil do
     begin
      {Check Storage}
      if Current = Storage then
       begin
        Result:=Storage;
        Exit;
       end;

      {Get Next}
      Current:=Current.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(StorageTableLock);
   end;
  end;
end;

{==============================================================================}

function StorageDeviceTypeToString(StorageType:LongWord):String;
begin
 {}
 Result:='STORAGE_TYPE_UNKNOWN';

 if StorageType <= STORAGE_TYPE_MAX then
  begin
   Result:=STORAGE_TYPE_NAMES[StorageType];
  end;
end;

{==============================================================================}

function StorageDeviceStateToString(StorageState:LongWord):String;
begin
 {}
 Result:='STORAGE_STATE_UNKNOWN';

 if StorageState <= STORAGE_STATE_MAX then
  begin
   Result:=STORAGE_STATE_NAMES[StorageState];
  end;
end;

{==============================================================================}

function StorageDeviceStateToNotification(State:LongWord):LongWord;
{Convert a Storage state value into the notification code for device notifications}
begin
 {}
 Result:=DEVICE_NOTIFICATION_NONE;

 {Check State}
 case State of
  STORAGE_STATE_EJECTED:Result:=DEVICE_NOTIFICATION_EJECT;
  STORAGE_STATE_EJECTING:Result:=DEVICE_NOTIFICATION_EJECTING;
  STORAGE_STATE_INSERTING:Result:=DEVICE_NOTIFICATION_INSERTING;
  STORAGE_STATE_INSERTED:Result:=DEVICE_NOTIFICATION_INSERT;
 end;
end;

{==============================================================================}

procedure StorageLog(Level:LongWord;Storage:PStorageDevice;const AText:String);
var
 WorkBuffer:String;
begin
 {}
 {Check Level}
 if Level < STORAGE_DEFAULT_LOG_LEVEL then Exit;

 WorkBuffer:='';
 {Check Level}
 if Level = STORAGE_LOG_LEVEL_DEBUG then
  begin
   WorkBuffer:=WorkBuffer + '[DEBUG] ';
  end
 else if Level = STORAGE_LOG_LEVEL_WARN then
  begin
   WorkBuffer:=WorkBuffer + '[WARN] ';
  end
 else if Level = STORAGE_LOG_LEVEL_ERROR then
  begin
   WorkBuffer:=WorkBuffer + '[ERROR] ';
  end;

 {Add Prefix}
 WorkBuffer:=WorkBuffer + 'Storage: ';

 {Check Storage}
 if Storage <> nil then
  begin
   WorkBuffer:=WorkBuffer + STORAGE_NAME_PREFIX + IntToStr(Storage.StorageId) + ': ';
  end;

 {Output Logging}
 LoggingOutputEx(LOGGING_FACILITY_STORAGE,LogLevelToLoggingSeverity(Level),'Storage',WorkBuffer + AText);
end;

{==============================================================================}

procedure StorageLogInfo(Storage:PStorageDevice;const AText:String); inline;
begin
 {}
 StorageLog(STORAGE_LOG_LEVEL_INFO,Storage,AText);
end;

{==============================================================================}

procedure StorageLogWarn(Storage:PStorageDevice;const AText:String); inline;
begin
 {}
 StorageLog(STORAGE_LOG_LEVEL_WARN,Storage,AText);
end;

{==============================================================================}

procedure StorageLogError(Storage:PStorageDevice;const AText:String); inline;
begin
 {}
 StorageLog(STORAGE_LOG_LEVEL_ERROR,Storage,AText);
end;

{==============================================================================}

procedure StorageLogDebug(Storage:PStorageDevice;const AText:String); inline;
begin
 {}
 StorageLog(STORAGE_LOG_LEVEL_DEBUG,Storage,AText);
end;

{==============================================================================}

procedure StorageStatusTimer(Storage:PStorageDevice);
var
 Argument:PtrUInt;
begin
 {}
 {Check Storage}
 if Storage = nil then Exit;
 if Storage.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Status}
 StorageDeviceControl(Storage,STORAGE_CONTROL_TEST_READY,Argument,Argument);

 {Enable Timer}
 TimerEnable(Storage.StatusTimer);
end;

{==============================================================================}
{==============================================================================}

initialization
 StorageInit;

{==============================================================================}

finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
