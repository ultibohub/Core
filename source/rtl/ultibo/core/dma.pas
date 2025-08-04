{
Ultibo DMA interface unit.

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


DMA Hosts
=========

 The DMA interfaces are 1 tier (Host only) whereas the USB interface is 3 tier (Host, Device and Driver).

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit DMA;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,HeapManager,Devices,SysUtils;

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
const
 {DMA specific constants}
 DMA_NAME_PREFIX = 'DMA';      {Name prefix for DMA Hosts}

 {DMA Host Types}
 DMA_TYPE_NONE = 0;

 DMA_TYPE_MAX  = 0;

 {DMA Type Names}
 DMA_TYPE_NAMES:array[DMA_TYPE_NONE..DMA_TYPE_MAX] of String = (
  'DMA_TYPE_NONE');

 {DMA Host States}
 DMA_STATE_DISABLED = 0;
 DMA_STATE_ENABLED  = 1;

 DMA_STATE_MAX      = 1;

 {DMA State Names}
 DMA_STATE_NAMES:array[DMA_STATE_DISABLED..DMA_STATE_MAX] of String = (
  'DMA_STATE_DISABLED',
  'DMA_STATE_ENABLED');

 {DMA Host Flags}
 DMA_FLAG_NONE        = $00000000;
 DMA_FLAG_SHARED      = $00000001; {Host requires data buffers in shared memory}
 DMA_FLAG_NOCACHE     = $00000002; {Host requires data buffers in non cached memory}
 DMA_FLAG_COHERENT    = $00000004; {Data buffers are cache coherent if allocated according to host configuration}
 DMA_FLAG_STRIDE      = $00000008; {Host supports 2D stride on source and/or dest address}
 DMA_FLAG_DREQ        = $00000010; {Host supports data request gating (DREQ) on source and/or dest address}
 DMA_FLAG_NOINCREMENT = $00000020; {Host supports no increment on source and/or dest address}
 DMA_FLAG_NOREAD      = $00000040; {Host supports no read from source address (write to dest address only) (Zero fill the destination)}
 DMA_FLAG_NOWRITE     = $00000080; {Host supports no write to dest address (read from source address only) (Cache fill from the source)}
 DMA_FLAG_WIDE        = $00000100; {Host supports wide read and/or write}
 DMA_FLAG_BULK        = $00000200; {Host supports bulk transfer}
 DMA_FLAG_LITE        = $00000400; {Host supports "lite" transfer}
 DMA_FLAG_40BIT       = $00000800; {Host supports 40-bit address transfer}

 {DMA Data Flags}
 {See: Platform DMA_DATA_FLAG_*}

 {DMA Request Flags}
 DMA_REQUEST_FLAG_NONE       = $00000000;
 DMA_REQUEST_FLAG_RELEASE    = $00000001; {If set then release the request automatically after completion}
 DMA_REQUEST_FLAG_CYCLIC     = $00000002; {This is a cyclic request which loops around from tail to head}
 DMA_REQUEST_FLAG_COMPATIBLE = $00000004; {If set then all buffers supplied are host configuration compatible (Sizing, Alignment, Flags)}

 {DMA logging}
 DMA_LOG_LEVEL_DEBUG     = LOG_LEVEL_DEBUG;  {DMA debugging messages}
 DMA_LOG_LEVEL_INFO      = LOG_LEVEL_INFO;   {DMA informational messages, such as a device being attached or detached}
 DMA_LOG_LEVEL_WARN      = LOG_LEVEL_WARN;   {DMA warning messages}
 DMA_LOG_LEVEL_ERROR     = LOG_LEVEL_ERROR;  {DMA error messages}
 DMA_LOG_LEVEL_NONE      = LOG_LEVEL_NONE;   {No DMA messages}

var
 DMA_DEFAULT_LOG_LEVEL:LongWord = DMA_LOG_LEVEL_DEBUG; {Minimum level for DMA messages.  Only messages with level greater than or equal to this will be printed}

var
 {DMA logging}
 DMA_LOG_ENABLED:Boolean;

{==============================================================================}
type
 {DMA specific types}

 {DMA Properties}
 PDMAProperties = ^TDMAProperties;
 TDMAProperties = record
  Flags:LongWord;      {Host flags (eg DMA_FLAG_STRIDE)}
  Alignment:LongWord;  {Host data buffer alignment}
  Multiplier:LongWord; {Host data buffer multiplier}
  Channels:LongWord;   {Total number of host channels}
  MaxSize:LongWord;    {Maximum transfer size}
  MaxCount:LongWord;   {Maximum Y count for 2D stride}
  MaxLength:LongWord;  {Maximum X length for 2D stride}
  MinStride:LongInt;   {Minimum stride value (Increment between rows)(May be negative)}
  MaxStride:LongWord;  {Maximum stride value (Increment between rows)}
 end;

 {DMA Host}
 PDMAHost = ^TDMAHost;
 PDMARequest = ^TDMARequest; {Forward declared to satisfy DMAHost}

 {DMA Enumeration Callback}
 TDMAEnumerate = function(DMA:PDMAHost;Data:Pointer):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 {DMA Notification Callback}
 TDMANotification = function(Device:PDevice;Data:Pointer;Notification:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}

 {DMA Host Methods}
 TDMAHostStart = function(DMA:PDMAHost):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TDMAHostStop = function(DMA:PDMAHost):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TDMAHostReset = function(DMA:PDMAHost):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TDMAHostSubmit = function(DMA:PDMAHost;Request:PDMARequest):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TDMAHostCancel = function(DMA:PDMAHost;Request:PDMARequest):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TDMAHostProperties = function(DMA:PDMAHost;Properties:PDMAProperties):LongWord;{$IFDEF i386} stdcall;{$ENDIF}

 TDMAHost = record
  {Device Properties}
  Device:TDevice;                          {The Device entry for this DMA host}
  {DMA Properties}
  DMAId:LongWord;                          {Unique Id of this DMA host in the DMA host table}
  DMAState:LongWord;                       {DMA state (eg DMA_STATE_ENABLED)}
  HostStart:TDMAHostStart;                 {A Host specific HostStart method implementing the standard DMA host interface (Mandatory)}
  HostStop:TDMAHostStop;                   {A Host specific HostStop method implementing the standard DMA host interface (Mandatory)}
  HostReset:TDMAHostReset;                 {A Host specific HostReset method implementing the standard DMA host interface (Or nil if the default method is suitable)}
  HostSubmit:TDMAHostSubmit;               {A Host specific HostSubmit method implementing the standard DMA host interface (Mandatory)}
  HostCancel:TDMAHostCancel;               {A Host specific HostCancel method implementing the standard DMA host interface (Mandatory)}
  HostProperties:TDMAHostProperties;       {A Host specific HostProperties method implementing the standard DMA host interface (Or nil if the default method is suitable)}
  {Driver Properties}
  Lock:TMutexHandle;                       {Host lock}
  Alignment:LongWord;                      {Host data buffer alignment}
  Multiplier:LongWord;                     {Host data buffer multiplier}
  Properties:TDMAProperties;               {Host properties}
  LastError:LongWord;                      {Last error to occur on this host}
  PendingCount:LongWord;                   {Number of DMA requests pending for this host}
  WaiterThread:TThreadId;                  {Thread waiting for pending requests to complete}
  {Statistics Properties}
  RequestCount:LongWord;                   {Number of DMA requests that have been submitted to this host}
  RequestErrors:LongWord;                  {Number of DMA requests that have failed on this host}
  {Internal Properties}
  Prev:PDMAHost;                           {Previous entry in DMA host table}
  Next:PDMAHost;                           {Next entry in DMA host table}
 end;

 {DMA Request}
 {PDMARequest = ^TDMARequest;} {Declared above for DMAHost}

 {DMA Request Methods}
 TDMARequestCompleted = procedure(Request:PDMARequest);{$IFDEF i386} stdcall;{$ENDIF}

 TDMARequest = record
  {Request Properties}
  Host:PDMAHost;
  Data:PDMAData;                           {List of data blocks for this request}
  Flags:LongWord;                          {Flags for the request (eg DMA_REQUEST_FLAG_RELEASE)}
  Direction:LongWord;                      {The direction of the DMA request (eg DMA_DIR_MEM_TO_MEM)}
  Peripheral:LongWord;                     {The peripheral ID for data request gating (eg DMA_DREQ_ID_NONE)}
  Callback:TDMARequestCompleted;           {Callback function that will be called when this DMA request has been successfully completed or has failed}
  DriverData:Pointer;                      {Private data for the completion callback (Optional)}
  {Result Properties}
  Status:LongWord;                         {Status of the request (ERROR_SUCCESS if successful, or another error code if the request failed)}
  {Driver Properties}                      {Private variables for use by Host drivers}
  ControlBlocks:Pointer;
 end;

{==============================================================================}
{var}
 {DMA specific variables}

{==============================================================================}
{Initialization Functions}
procedure DMAInit;

{==============================================================================}
{DMA Host Functions}
function DMAHostStart(DMA:PDMAHost):LongWord;
function DMAHostStop(DMA:PDMAHost):LongWord;
function DMAHostReset(DMA:PDMAHost):LongWord;
function DMAHostProperties(DMA:PDMAHost;Properties:PDMAProperties):LongWord;

function DMAHostCreate:PDMAHost;
function DMAHostCreateEx(Size:LongWord):PDMAHost;
function DMAHostDestroy(DMA:PDMAHost):LongWord;

function DMAHostRegister(DMA:PDMAHost):LongWord;
function DMAHostDeregister(DMA:PDMAHost):LongWord;

function DMAHostFind(DMAId:LongWord):PDMAHost;
function DMAHostEnumerate(Callback:TDMAEnumerate;Data:Pointer):LongWord;

function DMAHostNotification(DMA:PDMAHost;Callback:TDMANotification;Data:Pointer;Notification,Flags:LongWord):LongWord;

{==============================================================================}
{DMA Data Functions}
function DMADataCount(Data:PDMAData):LongWord;
function DMADataFlags(Data:PDMAData):LongWord;
function DMADataMaximum(Data:PDMAData):LongWord;

{==============================================================================}
{DMA Buffer Functions}
function DMABufferAllocate(DMA:PDMAHost;Size:LongWord):Pointer; inline;
function DMABufferAllocateEx(DMA:PDMAHost;var Size:LongWord):Pointer;
function DMABufferValidate(DMA:PDMAHost;Buffer:Pointer;Size:LongWord):LongWord;
function DMABufferRelease(Buffer:Pointer):LongWord;

{==============================================================================}
{DMA Request Functions}
function DMARequestAllocate(DMA:PDMAHost;Data:PDMAData;Callback:TDMARequestCompleted;DriverData:Pointer;Direction,Peripheral,Flags:LongWord):PDMARequest;
function DMARequestRelease(Request:PDMARequest):LongWord;

function DMARequestSubmit(Request:PDMARequest):LongWord;
function DMARequestCancel(Request:PDMARequest):LongWord;
procedure DMARequestComplete(Request:PDMARequest);

{==============================================================================}
{DMA Transfer Functions}
function DMATransferRequest(DMA:PDMAHost;Data:PDMAData;Direction,Peripheral,Flags,Timeout:LongWord):LongWord;
function DMATransferRequestEx(DMA:PDMAHost;Data:PDMAData;Callback:TDMARequestCompleted;DriverData:Pointer;Direction,Peripheral,Flags:LongWord):LongWord;
procedure DMATransferRequestComplete(Request:PDMARequest);

{==============================================================================}
{RTL DMA Functions}
function SysDMAAvailable:Boolean;

function SysDMATransfer(Data:PDMAData;Direction,Peripheral:LongWord):LongWord;

function SysDMAFillMemory(Dest:Pointer;Size:LongWord;Value:Byte):LongWord;
function SysDMACopyMemory(Source,Dest:Pointer;Size:LongWord):LongWord;

function SysDMAReadPeripheral(Address,Dest:Pointer;Size,Peripheral:LongWord):LongWord;
function SysDMAWritePeripheral(Source,Address:Pointer;Size,Peripheral:LongWord):LongWord;

function SysDMAAllocateBuffer(Size:LongWord):Pointer;
function SysDMAAllocateBufferEx(var Size:LongWord):Pointer;
function SysDMAReleaseBuffer(Buffer:Pointer):LongWord;

{==============================================================================}
{DMA Helper Functions}
function DMAGetCount:LongWord;
function DMAHostGetDefault:PDMAHost;
function DMAHostSetDefault(DMA:PDMAHost):LongWord;

function DMAHostCheck(DMA:PDMAHost):PDMAHost;

function DMATypeToString(DMAType:LongWord):String;
function DMAStateToString(DMAState:LongWord):String;

procedure DMALog(Level:Integer;DMA:PDMAHost;const AText:String);
procedure DMALogInfo(DMA:PDMAHost;const AText:String); inline;
procedure DMALogWarn(DMA:PDMAHost;const AText:String); inline;
procedure DMALogError(DMA:PDMAHost;const AText:String); inline;
procedure DMALogDebug(DMA:PDMAHost;const AText:String); inline;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {DMA specific variables}
 DMAInitialized:Boolean;

 DMAHostTable:PDMAHost;
 DMAHostTableLock:TCriticalSectionHandle = INVALID_HANDLE_VALUE;
 DMAHostTableCount:LongWord;

 DMAHostDefault:PDMAHost;

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure DMAInit;
begin
 {}
 {Check Initialized}
 if DMAInitialized then Exit;

 {Initialize Logging}
 DMA_LOG_ENABLED:=(DMA_DEFAULT_LOG_LEVEL <> DMA_LOG_LEVEL_NONE);

 {Initialize DMA Host Table}
 DMAHostTable:=nil;
 DMAHostTableLock:=CriticalSectionCreate;
 DMAHostTableCount:=0;
 if DMAHostTableLock = INVALID_HANDLE_VALUE then
  begin
   if DMA_LOG_ENABLED then DMALogError(nil,'Failed to create DMA host table lock');
  end;
 DMAHostDefault:=nil;

 {Register Platform DMA Handlers}
 DMAAvailableHandler:=SysDMAAvailable;
 DMATransferHandler:=SysDMATransfer;
 DMAFillMemoryHandler:=SysDMAFillMemory;
 DMACopyMemoryHandler:=SysDMACopyMemory;
 DMAReadPeripheralHandler:=SysDMAReadPeripheral;
 DMAWritePeripheralHandler:=SysDMAWritePeripheral;
 DMAAllocateBufferHandler:=SysDMAAllocateBuffer;
 DMAAllocateBufferExHandler:=SysDMAAllocateBufferEx;
 DMAReleaseBufferHandler:=SysDMAReleaseBuffer;

 DMAInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{DMA Host Functions}
function DMAHostStart(DMA:PDMAHost):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check DMA}
 if DMA = nil then Exit;
 if DMA.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF DMA_DEBUG}
 if DMA_LOG_ENABLED then DMALogDebug(DMA,'DMA Host Start');
 {$ENDIF}

 {Check Disabled}
 Result:=ERROR_SUCCESS;
 if DMA.DMAState <> DMA_STATE_DISABLED then Exit;

 if MutexLock(DMA.Lock) = ERROR_SUCCESS then
  begin
   try
    if Assigned(DMA.HostStart) then
     begin
      {Call Host Start}
      Result:=DMA.HostStart(DMA);
      if Result <> ERROR_SUCCESS then Exit;
     end
    else
     begin
      Result:=ERROR_INVALID_PARAMETER;
      Exit;
     end;

    {Enable Host}
    DMA.DMAState:=DMA_STATE_ENABLED;

    {Notify Enable}
    NotifierNotify(@DMA.Device,DEVICE_NOTIFICATION_ENABLE);

    Result:=ERROR_SUCCESS;
   finally
    MutexUnlock(DMA.Lock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function DMAHostStop(DMA:PDMAHost):LongWord;
var
 Message:TMessage;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check DMA}
 if DMA = nil then Exit;
 if DMA.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF DMA_DEBUG}
 if DMA_LOG_ENABLED then DMALogDebug(DMA,'DMA Host Stop');
 {$ENDIF}

 {Check Enabled}
 Result:=ERROR_SUCCESS;
 if DMA.DMAState <> DMA_STATE_ENABLED then Exit;

 if MutexLock(DMA.Lock) = ERROR_SUCCESS then
  begin
   try
    {Check Pending}
    if DMA.PendingCount <> 0 then
     begin
      {$IFDEF DMA_DEBUG}
      if DMA_LOG_ENABLED then DMALogDebug(DMA,'Waiting for ' + IntToStr(DMA.PendingCount) + ' pending requests to complete');
      {$ENDIF}

      {Wait for Pending}

      {Setup Waiter}
      DMA.WaiterThread:=GetCurrentThreadId;

      {Release the Lock}
      MutexUnlock(DMA.Lock);

      {Wait for Message}
      ThreadReceiveMessage(Message);

      {Acquire the Lock}
      if MutexLock(DMA.Lock) <> ERROR_SUCCESS then Exit;
     end;

    if Assigned(DMA.HostStop) then
     begin
      {Call Host Stop}
      Result:=DMA.HostStop(DMA);
      if Result <> ERROR_SUCCESS then Exit;
     end
    else
     begin
      Result:=ERROR_INVALID_PARAMETER;
      Exit;
     end;

    {Disable Host}
    DMA.DMAState:=DMA_STATE_DISABLED;

    {Notify Disable}
    NotifierNotify(@DMA.Device,DEVICE_NOTIFICATION_DISABLE);

    Result:=ERROR_SUCCESS;
   finally
    MutexUnlock(DMA.Lock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function DMAHostReset(DMA:PDMAHost):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check DMA}
 if DMA = nil then Exit;
 if DMA.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF DMA_DEBUG}
 if DMA_LOG_ENABLED then DMALogDebug(DMA,'DMA Host Reset');
 {$ENDIF}

 {Check Enabled}
 {Result:=ERROR_NOT_SUPPORTED;}
 {if DMA.DMAState <> DMA_STATE_ENABLED then Exit;} {Allow when disabled}

 if MutexLock(DMA.Lock) = ERROR_SUCCESS then
  begin
   if Assigned(DMA.HostReset) then
    begin
     {Call Host Reset}
     Result:=DMA.HostReset(DMA);
    end
   else
    begin
     Result:=ERROR_NOT_SUPPORTED;
    end;

   MutexUnlock(DMA.Lock);
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function DMAHostProperties(DMA:PDMAHost;Properties:PDMAProperties):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Properties}
 if Properties = nil then Exit;

 {Check DMA}
 if DMA = nil then Exit;
 if DMA.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF DMA_DEBUG}
 if DMA_LOG_ENABLED then DMALogDebug(DMA,'DMA Host Properties');
 {$ENDIF}

 {Check Enabled}
 {Result:=ERROR_NOT_SUPPORTED;}
 {if DMA.DMAState <> DMA_STATE_ENABLED then Exit;} {Allow when disabled}

 if MutexLock(DMA.Lock) = ERROR_SUCCESS then
  begin
   if Assigned(DMA.HostProperties) then
    begin
     {Call Host Properites}
     Result:=DMA.HostProperties(DMA,Properties);
    end
   else
    begin
     {Get Properties}
     System.Move(DMA.Properties,Properties^,SizeOf(TDMAProperties));

     {Return Result}
     Result:=ERROR_SUCCESS;
    end;

   MutexUnlock(DMA.Lock);
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function DMAHostCreate:PDMAHost;
{Create a new DMA entry}
{Return: Pointer to new DMA entry or nil if DMA could not be created}
begin
 {}
 Result:=DMAHostCreateEx(SizeOf(TDMAHost));
end;

{==============================================================================}

function DMAHostCreateEx(Size:LongWord):PDMAHost;
{Create a new DMA entry}
{Size: Size in bytes to allocate for new DMA (Including the DMA entry)}
{Return: Pointer to new DMA entry or nil if DMA could not be created}
begin
 {}
 Result:=nil;

 {Check Size}
 if Size < SizeOf(TDMAHost) then Exit;

 {Create DMA}
 Result:=PDMAHost(DeviceCreateEx(Size));
 if Result = nil then Exit;

 {Update Device}
 Result.Device.DeviceBus:=DEVICE_BUS_NONE;
 Result.Device.DeviceType:=DMA_TYPE_NONE;
 Result.Device.DeviceFlags:=DMA_FLAG_NONE;
 Result.Device.DeviceData:=nil;

 {Update DMA}
 Result.DMAId:=DEVICE_ID_ANY;
 Result.DMAState:=DMA_STATE_DISABLED;
 Result.HostStart:=nil;
 Result.HostStop:=nil;
 Result.HostReset:=nil;
 Result.HostSubmit:=nil;
 Result.HostCancel:=nil;
 Result.HostProperties:=nil;
 Result.Lock:=INVALID_HANDLE_VALUE;
 Result.Alignment:=1;
 Result.Multiplier:=1;
 Result.WaiterThread:=INVALID_HANDLE_VALUE;

 {Create Lock}
 Result.Lock:=MutexCreateEx(False,MUTEX_DEFAULT_SPINCOUNT,MUTEX_FLAG_RECURSIVE);
 if Result.Lock = INVALID_HANDLE_VALUE then
  begin
   if DMA_LOG_ENABLED then DMALogError(nil,'Failed to create lock for DMA host');
   DMAHostDestroy(Result);
   Result:=nil;
   Exit;
  end;
end;

{==============================================================================}

function DMAHostDestroy(DMA:PDMAHost):LongWord;
{Destroy an existing DMA entry}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check DMA}
 if DMA = nil then Exit;
 if DMA.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check DMA}
 Result:=ERROR_IN_USE;
 if DMAHostCheck(DMA) = DMA then Exit;

 {Check State}
 if DMA.Device.DeviceState <> DEVICE_STATE_UNREGISTERED then Exit;

 {Destroy Lock}
 if DMA.Lock <> INVALID_HANDLE_VALUE then
  begin
   MutexDestroy(DMA.Lock);
  end;

 {Destroy DMA}
 Result:=DeviceDestroy(@DMA.Device);
end;

{==============================================================================}

function DMAHostRegister(DMA:PDMAHost):LongWord;
{Register a new DMA in the DMA host table}
var
 DMAId:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check DMA}
 if DMA = nil then Exit;
 if DMA.DMAId <> DEVICE_ID_ANY then Exit;
 if DMA.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Interfaces}
 if not(Assigned(DMA.HostStart)) then Exit;
 if not(Assigned(DMA.HostStop)) then Exit;
 if not(Assigned(DMA.HostSubmit)) then Exit;
 if not(Assigned(DMA.HostCancel)) then Exit;

 {Check DMA}
 Result:=ERROR_ALREADY_EXISTS;
 if DMAHostCheck(DMA) = DMA then Exit;

 {Check State}
 if DMA.Device.DeviceState <> DEVICE_STATE_UNREGISTERED then Exit;

 {Insert DMA}
 if CriticalSectionLock(DMAHostTableLock) = ERROR_SUCCESS then
  begin
   try
    {Update DMA}
    DMAId:=0;
    while DMAHostFind(DMAId) <> nil do
     begin
      Inc(DMAId);
     end;
    DMA.DMAId:=DMAId;

    {Update Device}
    DMA.Device.DeviceName:=DMA_NAME_PREFIX + IntToStr(DMA.DMAId);
    DMA.Device.DeviceClass:=DEVICE_CLASS_DMA;

    {Register Device}
    Result:=DeviceRegister(@DMA.Device);
    if Result <> ERROR_SUCCESS then
     begin
      DMA.DMAId:=DEVICE_ID_ANY;
      Exit;
     end;

    {Link DMA}
    if DMAHostTable = nil then
     begin
      DMAHostTable:=DMA;
     end
    else
     begin
      DMA.Next:=DMAHostTable;
      DMAHostTable.Prev:=DMA;
      DMAHostTable:=DMA;
     end;

    {Increment Count}
    Inc(DMAHostTableCount);

    {Check Default}
    if DMAHostDefault = nil then
     begin
      DMAHostDefault:=DMA;
     end;

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    CriticalSectionUnlock(DMAHostTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function DMAHostDeregister(DMA:PDMAHost):LongWord;
{Deregister a DMA from the DMA host table}
var
 Prev:PDMAHost;
 Next:PDMAHost;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check DMA}
 if DMA = nil then Exit;
 if DMA.DMAId = DEVICE_ID_ANY then Exit;
 if DMA.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check DMA}
 Result:=ERROR_NOT_FOUND;
 if DMAHostCheck(DMA) <> DMA then Exit;

 {Check State}
 if DMA.Device.DeviceState <> DEVICE_STATE_REGISTERED then Exit;

 {Remove DMA}
 if CriticalSectionLock(DMAHostTableLock) = ERROR_SUCCESS then
  begin
   try
    {Deregister Device}
    Result:=DeviceDeregister(@DMA.Device);
    if Result <> ERROR_SUCCESS then Exit;

    {Unlink DMA}
    Prev:=DMA.Prev;
    Next:=DMA.Next;
    if Prev = nil then
     begin
      DMAHostTable:=Next;
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
    Dec(DMAHostTableCount);

    {Check Default}
    if DMAHostDefault = DMA then
     begin
      DMAHostDefault:=DMAHostTable;
     end;

    {Update DMA}
    DMA.DMAId:=DEVICE_ID_ANY;

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    CriticalSectionUnlock(DMAHostTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function DMAHostFind(DMAId:LongWord):PDMAHost;
var
 DMA:PDMAHost;
begin
 {}
 Result:=nil;

 {Check Id}
 if DMAId = DEVICE_ID_ANY then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(DMAHostTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get DMA}
    DMA:=DMAHostTable;
    while DMA <> nil do
     begin
      {Check State}
      if DMA.Device.DeviceState = DEVICE_STATE_REGISTERED then
       begin
        {Check Id}
        if DMA.DMAId = DMAId then
         begin
          Result:=DMA;
          Exit;
         end;
       end;

      {Get Next}
      DMA:=DMA.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(DMAHostTableLock);
   end;
  end;
end;

{==============================================================================}

function DMAHostEnumerate(Callback:TDMAEnumerate;Data:Pointer):LongWord;
var
 DMA:PDMAHost;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Callback}
 if not Assigned(Callback) then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(DMAHostTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get DMA}
    DMA:=DMAHostTable;
    while DMA <> nil do
     begin
      {Check State}
      if DMA.Device.DeviceState = DEVICE_STATE_REGISTERED then
       begin
        if Callback(DMA,Data) <> ERROR_SUCCESS then Exit;
       end;

      {Get Next}
      DMA:=DMA.Next;
     end;

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    CriticalSectionUnlock(DMAHostTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function DMAHostNotification(DMA:PDMAHost;Callback:TDMANotification;Data:Pointer;Notification,Flags:LongWord):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check DMA}
 if DMA = nil then
  begin
   Result:=DeviceNotification(nil,DEVICE_CLASS_DMA,Callback,Data,Notification,Flags);
  end
 else
  begin
   {Check DMA}
   if DMA.Device.Signature <> DEVICE_SIGNATURE then Exit;

   Result:=DeviceNotification(@DMA.Device,DEVICE_CLASS_DMA,Callback,Data,Notification,Flags);
  end;
end;

{==============================================================================}
{==============================================================================}
{DMA Data Functions}
function DMADataCount(Data:PDMAData):LongWord;
{Return the total number of data blocks in the linked list}
var
 Next:PDMAData;
begin
 {}
 Result:=0;

 Next:=Data;
 while Next <> nil do
  begin
   Inc(Result);

   Next:=Next.Next;
  end;
end;

{==============================================================================}

function DMADataFlags(Data:PDMAData):LongWord;
{Return the combined flags of the data blocks in the linked list}
var
 Next:PDMAData;
begin
 {}
 Result:=DMA_DATA_FLAG_NONE;

 Next:=Data;
 while Next <> nil do
  begin
   Result:=Result or Next.Flags;

   Next:=Next.Next;
  end;
end;

{==============================================================================}

function DMADataMaximum(Data:PDMAData):LongWord;
{Return the size of the largest data block in the linked list}
var
 Next:PDMAData;
begin
 {}
 Result:=0;

 Next:=Data;
 while Next <> nil do
  begin
   if Next.Size > Result then Result:=Next.Size;

   Next:=Next.Next;
  end;
end;

{==============================================================================}
{==============================================================================}
{DMA Buffer Functions}
function DMABufferAllocate(DMA:PDMAHost;Size:LongWord):Pointer; inline;
{Allocate a data buffer for a DMA request}
{DMA: The DMA host that the request will be sent to}
{Size: The size of the data buffer to allocate}
{Return: The newly allocated buffer or nil on failure}
begin
 {}
 Result:=DMABufferAllocateEx(DMA,Size);
end;

{==============================================================================}

function DMABufferAllocateEx(DMA:PDMAHost;var Size:LongWord):Pointer;
{Allocate a data buffer for a DMA request}
{DMA: The DMA host that the request will be sent to}
{Size: The size of the data buffer to allocate (Updated on return to actual size)}
{Return: The newly allocated buffer or nil on failure}

{Note: This differs from DMABufferAllocate in that it updates the size value to reflect
       the actual size of the buffer allocated which may be required for some uses}
begin
 {}
 Result:=nil;

 {Check Size}
 if Size = 0 then Exit;

 {Check DMA}
 if DMA = nil then Exit;
 if DMA.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Size}
 Size:=RoundUp(Size,DMA.Multiplier);

 {Check Host Flags}
 if (DMA.Device.DeviceFlags and DMA_FLAG_SHARED) = DMA_FLAG_SHARED then
  begin
   {Allocate Shared}
   Result:=GetSharedAlignedMem(Size,DMA.Alignment);
  end
 else if (DMA.Device.DeviceFlags and DMA_FLAG_NOCACHE) = DMA_FLAG_NOCACHE then
  begin
   {Allocate Non Cached}
   Result:=GetNoCacheAlignedMem(Size,DMA.Alignment);
  end
 else
  begin
   {Allocate Normal}
   Result:=GetAlignedMem(Size,DMA.Alignment);
  end;
end;

{==============================================================================}

function DMABufferValidate(DMA:PDMAHost;Buffer:Pointer;Size:LongWord):LongWord;
{Validate a data buffer for a DMA request against the DMA host requirements}
{DMA: The DMA host that the request will be sent to}
{Buffer: The data buffer to validate}
{Size: The size of the data buffer}
{Return: ERROR_SUCCESS on success or another error code on failure (ERROR_NOT_COMPATIBLE if not compatible with host)}

{Note: Does not check for cache coherency requirements (Shared, Non Cached) only alignment and size.
       Buffers used as the source of a DMA request do not necessarily need to meet alignment and size
       requirements, however buffers used as a destination must meet these requirements or the caller
       must take appropriate actions to prevent undesirable side effects from cache invalidation.}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Size}
 if Size = 0 then Exit;

 {Check Buffer}
 if Buffer = nil then Exit;

 {Check DMA}
 if DMA = nil then Exit;
 if DMA.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Host Alignment}
 if Align(Buffer,DMA.Alignment) <> Buffer then
  begin
   Result:=ERROR_NOT_COMPATIBLE;
   Exit;
  end;

 {Check Host Multiplier}
 if RoundUp(Size,DMA.Multiplier) <> Size then
  begin
   Result:=ERROR_NOT_COMPATIBLE;
   Exit;
  end;

 {Return Result}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function DMABufferRelease(Buffer:Pointer):LongWord;
{Release a data buffer from a DMA request}
{Data: The buffer to be released}
{Return: ERROR_SUCCESS on success or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Buffer}
 if Buffer = nil then Exit;

 {Free Buffer}
 FreeMem(Buffer);

 {Return Result}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}
{==============================================================================}
{DMA Request Functions}
function DMARequestAllocate(DMA:PDMAHost;Data:PDMAData;Callback:TDMARequestCompleted;DriverData:Pointer;Direction,Peripheral,Flags:LongWord):PDMARequest;
{Allocate a new DMA request}
{DMA: The DMA host this request will be sent to}
{Data: A linked list of DMA data blocks for the transfer (Optional)}
{Callback: The callback function to be called on completion of the request}
{DriverData: Driver private data for the callback (Optional)}
{Direction: The direction of the DMA request (eg DMA_DIR_MEM_TO_MEM)}
{Peripheral: The peripheral ID for data request gating (eg DMA_DREQ_ID_NONE)}
{Flags: Additional flags for this request (eg DMA_REQUEST_FLAG_CYCLIC)}
{Return: The newly allocated request or nil on failure}
var
 Request:PDMARequest;
begin
 {}
 Result:=nil;

 {Check DMA}
 if DMA = nil then Exit;
 if DMA.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Data}
 {if Data = nil then Exit;} {Data may be nil}

 {Check Callback}
 if not Assigned(Callback) then Exit;

 {Check Driver Data}
 {if DriverData = nil then Exit;} {DriverData may be nil}

 {Allocate Request}
 Request:=AllocMem(SizeOf(TDMARequest));
 if Request = nil then Exit;

 {Initialize Request}
 Request.Host:=DMA;
 Request.Data:=Data;
 Request.Flags:=Flags;
 Request.Direction:=Direction;
 Request.Peripheral:=Peripheral;
 Request.Callback:=Callback;
 Request.DriverData:=DriverData;
 Request.Status:=ERROR_NOT_VALID;

 {Return Result}
 Result:=Request;
end;

{==============================================================================}

function DMARequestRelease(Request:PDMARequest):LongWord;
{Release and destroy a DMA request}
{Request: The request to be released}
{Return: ERROR_SUCCESS on success or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Request}
 if Request = nil then Exit;

 {Deinitialize Request}
 Request.Host:=nil;
 Request.Data:=nil;
 Request.Flags:=DMA_REQUEST_FLAG_NONE;
 Request.Direction:=DMA_DIR_NONE;
 Request.Peripheral:=DMA_DREQ_ID_NONE;
 Request.Callback:=nil;
 Request.DriverData:=nil;
 Request.Status:=ERROR_NOT_VALID;

 {Release Request}
 FreeMem(Request);

 {Return Result}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function DMARequestSubmit(Request:PDMARequest):LongWord;
{Submit a DMA request to a DMA host}
{Request: The request to be submitted}
{Return: ERROR_SUCCESS if completed or another error code on failure}
{Note: The request will be completed asynchronously by the DMA host and the
 completion callback will be called when the request has either succeeded or failed}
var
 Host:PDMAHost;
 Status:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Request}
 if (Request = nil) or (Request.Host = nil) or not(Assigned(Request.Callback)) then
  begin
   if DMA_LOG_ENABLED then DMALogError(nil,'Bad DMA request, host or completion callback function not specified');
   Exit;
  end;

 {Get Host}
 Host:=Request.Host;

 {Setup Request}
 Request.Status:=ERROR_NOT_PROCESSED;

 {Acquire the Lock}
 if MutexLock(Host.Lock) = ERROR_SUCCESS then
  begin
   try
    {Update Statistics}
    Inc(Host.RequestCount);

    {Update Pending}
    Inc(Host.PendingCount);

    {Release the Lock}
    MutexUnlock(Host.Lock);

    {Submit Request}
    Status:=Host.HostSubmit(Host,Request);

    {Acquire the Lock}
    if MutexLock(Host.Lock) <> ERROR_SUCCESS then Exit;

    {Check Status}
    if Status <> ERROR_SUCCESS then
     begin
      {Update Pending}
      Dec(Host.PendingCount);
     end;

    {Return Result}
    Result:=Status;
   finally
    {Release the Lock}
    MutexUnlock(Host.Lock);
   end;
  end;
end;

{==============================================================================}

function DMARequestCancel(Request:PDMARequest):LongWord;
{Cancel a DMA request previously submitted to a DMA host}
{Request: The request to be cancelled}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Host:PDMAHost;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Request}
 if (Request = nil) or (Request.Host = nil) or not(Assigned(Request.Callback)) then
  begin
   if DMA_LOG_ENABLED then DMALogError(nil,'Bad DMA request, host or completion callback function not specified');
   Exit;
  end;

 {Get Host}
 Host:=Request.Host;

 {Acquire the Lock}
 if MutexLock(Host.Lock) = ERROR_SUCCESS then
  begin
   try
    {Cancel Request}
    Result:=Host.HostCancel(Host,Request);
   finally
    {Release the Lock}
    MutexUnlock(Host.Lock);
   end;
  end;
end;

{==============================================================================}

procedure DMARequestComplete(Request:PDMARequest);
{Called by a DMA host when a DMA request completes}
{Request: The DMA request which has completed}

{Note: DMA host drivers may call this on a worker thread}
var
 Host:PDMAHost;
 Flags:LongWord;
 Message:TMessage;
begin
 {}
 {Check Request}
 if Request = nil then Exit;
 if Request.Host = nil then Exit;

 {$IFDEF DMA_DEBUG}
 if DMA_LOG_ENABLED then DMALogDebug(Request.Host,'DMA Request Complete (Request=' + PtrToHex(Request) + ' Status=' + ErrorToString(Request.Status) + ')');
 {$ENDIF}

 {Get Host}
 Host:=Request.Host;

 {Get Flags}
 Flags:=Request.Flags;

 {Acquire the Lock}
 if MutexLock(Host.Lock) = ERROR_SUCCESS then
  begin
   try
    {Check Result}
    if Request.Status <> ERROR_SUCCESS then
     begin
      {Update Statistics}
      Inc(Host.RequestErrors);

      {Update Status}
      Host.LastError:=Request.Status;
     end;

    {Release the Lock}
    MutexUnlock(Host.Lock);

    {Completion Callback}
    Request.Callback(Request);

    {Acquire the Lock}
    if MutexLock(Host.Lock) <> ERROR_SUCCESS then Exit;

    {Update Pending}
    Dec(Host.PendingCount);

    {Check Pending Requests}
    if (Host.PendingCount = 0) and (Host.WaiterThread <> INVALID_HANDLE_VALUE) then
     begin
      {$IFDEF DMA_DEBUG}
      if DMA_LOG_ENABLED then DMALogDebug(Host,'Sending message to waiter thread (Thread=' + IntToHex(Host.WaiterThread,8) + ')');
      {$ENDIF}

      {Send Message}
      FillChar(Message,SizeOf(TMessage),0);
      ThreadSendMessage(Host.WaiterThread,Message);
      Host.WaiterThread:=INVALID_HANDLE_VALUE;
     end;

    {Check for Release}
    if (Flags and DMA_REQUEST_FLAG_RELEASE) <> 0 then
     begin
      {Release Request}
      DMARequestRelease(Request);
     end;
   finally
    {Release the Lock}
    MutexUnlock(Host.Lock);
   end;
  end;
end;

{==============================================================================}
{==============================================================================}
{DMA Transfer Functions}
function DMATransferRequest(DMA:PDMAHost;Data:PDMAData;Direction,Peripheral,Flags,Timeout:LongWord):LongWord;
{Perform a DMA transfer request with the supplied data blocks on the supplied host}
{DMA: The DMA host to execute the request on}
{Data: A linked list of DMA data blocks for the transfer}
{Direction: The direction of the DMA request (eg DMA_DIR_MEM_TO_MEM)}
{Peripheral: The peripheral ID for data request gating (eg DMA_DREQ_ID_NONE)}
{Flags: Additional flags for this transfer request (eg DMA_REQUEST_FLAG_CYCLIC)}
{Timeout: Milliseconds to wait for request to complete (INFINITE to wait forever)}

{Note: An internal callback will be specified and the function will wait for the transfer to complete}
var
 Status:LongWord;
 Request:PDMARequest;
 ResultCode:LongWord;
 Semaphore:TSemaphoreHandle;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Data}
 if Data = nil then Exit;

 {Check DMA}
 if DMA = nil then Exit;
 if DMA.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF DMA_DEBUG}
 if DMA_LOG_ENABLED then DMALogDebug(DMA,'DMA Transfer Request');
 {$ENDIF}

 {Check Enabled}
 Result:=ERROR_NOT_SUPPORTED;
 if DMA.DMAState <> DMA_STATE_ENABLED then Exit;

 {Check Timeout}
 if Timeout = 0 then
  begin
   Timeout:=INFINITE;
  end;

 {Check Flags (Do not allow release flag)}
 Flags:=Flags and not(DMA_REQUEST_FLAG_RELEASE);

 {Create Semaphore}
 Semaphore:=SemaphoreCreate(0);
 if Semaphore = INVALID_HANDLE_VALUE then
  begin
   {Return Result}
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;

 {Create Request}
 Request:=DMARequestAllocate(DMA,Data,DMATransferRequestComplete,Pointer(Semaphore),Direction,Peripheral,Flags);
 if Request = nil then
  begin
   {Destroy Semaphore}
   SemaphoreDestroy(Semaphore);

   {Return Result}
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;

 {Submit Request}
 Status:=DMARequestSubmit(Request);
 if Status = ERROR_SUCCESS then
  begin
   {Wait for Completion}
   ResultCode:=SemaphoreWaitEx(Semaphore,Timeout);
   if ResultCode = ERROR_SUCCESS then
    begin
     {Get Status}
     Status:=Request.Status;
    end
   else if ResultCode = ERROR_WAIT_TIMEOUT then
    begin
     if DMA_LOG_ENABLED then DMALogError(DMA,'Transfer request timeout (Timeout=' + IntToStr(Timeout) + ')');

     {Get Status}
     Status:=ERROR_WAIT_TIMEOUT;

     {Cancel Request}
     DMARequestCancel(Request);

     {Wait for Cancel}
     SemaphoreWait(Semaphore);
    end
   else
    begin
     if DMA_LOG_ENABLED then DMALogError(DMA,'Transfer request failure (Error=' + ErrorToString(ResultCode) + ')');

     {Get Status}
     Status:=ERROR_OPERATION_FAILED;

     {Cancel Request}
     DMARequestCancel(Request);

     {Wait for Cancel}
     SemaphoreWait(Semaphore);
    end;
  end;

 {Check for Release (Release flag not allowed)}
 {if (Flags and DMA_REQUEST_FLAG_RELEASE) = 0 then
  begin}
   {Release Request}
   DMARequestRelease(Request);
  {end;}

 {Destroy Semaphore}
 SemaphoreDestroy(Semaphore);

 {$IFDEF DMA_DEBUG}
 if DMA_LOG_ENABLED then DMALogDebug(DMA,'Transfer request complete (Result=' + ErrorToString(Status) + ')');
 {$ENDIF}

 {Return Result}
 Result:=Status;
end;

{==============================================================================}

function DMATransferRequestEx(DMA:PDMAHost;Data:PDMAData;Callback:TDMARequestCompleted;DriverData:Pointer;Direction,Peripheral,Flags:LongWord):LongWord;
{Perform a DMA transfer request with the supplied data blocks on the supplied host}
{DMA: The DMA host to execute the request on}
{Data: A linked list of DMA data blocks for the transfer}
{Callback: The callback function to be called on completion of the request}
{DriverData: Driver private data for the callback (Optional)}
{Direction: The direction of the DMA request (eg DMA_DIR_MEM_TO_MEM)}
{Peripheral: The peripheral ID for data request gating (eg DMA_DREQ_ID_NONE)}
{Flags: Additional flags for this transfer request (eg DMA_REQUEST_FLAG_CYCLIC)}

{Note: The request will be passed to the callback with the DriverData field set to the DriverData passed to this function.
       On completion of the callback the request will automatically be released by the internal callback handler}
var
 Status:LongWord;
 Request:PDMARequest;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Data}
 if Data = nil then Exit;

 {Check DMA}
 if DMA = nil then Exit;
 if DMA.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF DMA_DEBUG}
 if DMA_LOG_ENABLED then DMALogDebug(DMA,'DMA Transfer Request Ex');
 {$ENDIF}

 {Check Enabled}
 Result:=ERROR_NOT_SUPPORTED;
 if DMA.DMAState <> DMA_STATE_ENABLED then Exit;

 {Create Request}
 Request:=DMARequestAllocate(DMA,Data,Callback,DriverData,Direction,Peripheral,Flags);
 if Request = nil then
  begin
   {Return Result}
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;

 {Update Request (Always add release flag)}
 Request.Flags:=Request.Flags or DMA_REQUEST_FLAG_RELEASE;

 {Submit Request}
 Status:=DMARequestSubmit(Request);
 if Status <> ERROR_SUCCESS then
  begin
   {Release Request}
   DMARequestRelease(Request);
  end;

 {$IFDEF DMA_DEBUG}
 if DMA_LOG_ENABLED then DMALogDebug(DMA,'Transfer request submitted (Result=' + ErrorToString(Status) + ')');
 {$ENDIF}

 {Return Result}
 Result:=Status;
end;

{==============================================================================}

procedure DMATransferRequestComplete(Request:PDMARequest);
{Called when a DMA transfer request completes}
{Request: The DMA request which has completed}

{Note: This is the internal callback for DMATransferRequest}
begin
 {}
 {Check Request}
 if Request = nil then Exit;

 {Check Semaphore}
 if Request.DriverData = nil then Exit;

 {Signal Semaphore}
 SemaphoreSignal(TSemaphoreHandle(Request.DriverData));
end;

{==============================================================================}
{==============================================================================}
{RTL DMA Functions}
function SysDMAAvailable:Boolean;
{Check if a DMA host is available}
begin
 {}
 Result:=(DMAHostDefault <> nil);
end;

{==============================================================================}

function SysDMATransfer(Data:PDMAData;Direction,Peripheral:LongWord):LongWord;
{Perform a DMA transfer using the list of DMA data blocks provided}
{Data: A linked list of DMA data blocks for the transfer}
{Direction: The direction of the DMA request (eg DMA_DIR_MEM_TO_MEM)}
{Peripheral: The peripheral ID for data request gating (eg DMA_DREQ_ID_NONE)}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 if DMAHostDefault = nil then Exit;

 Result:=DMATransferRequest(DMAHostDefault,Data,Direction,Peripheral,DMA_REQUEST_FLAG_NONE,INFINITE);
end;

{==============================================================================}

function SysDMAFillMemory(Dest:Pointer;Size:LongWord;Value:Byte):LongWord;
{Fill memory at the destination address using DMA}
{Dest: The address to start the memory fill}
{Size: The size of memory to fill in bytes}
{Value: The value to fill the memory with}
var
 Data:TDMAData;
 Len:LongWord;
 Source:Pointer;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 if DMAHostDefault = nil then Exit;

 {Check Dest and Size}
 if Dest = nil then Exit;
 if Size = 0 then Exit;

 {Get Size}
 Len:=SizeOf(LongWord);

 {Get Source}
 Source:=DMABufferAllocateEx(DMAHostDefault,Len);
 try
  {Create Data}
  FillChar(Data,SizeOf(TDMAData),0);
  Data.Source:=Source;
  Data.Dest:=Dest;
  Data.Size:=Size;
  Data.Flags:=DMA_DATA_FLAG_NONE;

  {Check No Increment}
  if (DMAHostDefault.Device.DeviceFlags and DMA_FLAG_NOINCREMENT) <> 0 then
   begin
    Data.Flags:=Data.Flags or DMA_DATA_FLAG_SOURCE_NOINCREMENT;

    {Check Value}
    if (Value = 0) and ((DMAHostDefault.Device.DeviceFlags and DMA_FLAG_NOREAD) <> 0) then
     begin
      Data.Flags:=Data.Flags or DMA_DATA_FLAG_NOREAD;
     end
    else
     begin
      Data.Flags:=Data.Flags or DMA_DATA_FLAG_NOCLEAN;

      {Set Value}
      FillChar(Source^,Len,Value);

      {Flush Cache}
      if (DMAHostDefault.Device.DeviceFlags and DMA_FLAG_COHERENT) = 0 then
       begin
        CleanDataCacheRange(PtrUInt(Source),Len);
       end;
     end;

    {Check Wide}
    if (DMAHostDefault.Device.DeviceFlags and DMA_FLAG_WIDE) <> 0 then
     begin
      Data.Flags:=Data.Flags or DMA_DATA_FLAG_DEST_WIDE;
     end;

    {Perform Transfer}
    Result:=DMATransferRequest(DMAHostDefault,@Data,DMA_DIR_MEM_TO_MEM,DMA_DREQ_ID_NONE,DMA_REQUEST_FLAG_NONE,INFINITE);
   end
  else
   begin
    Result:=ERROR_NOT_SUPPORTED;
   end;
 finally
  DMABufferRelease(Source);
 end;
end;

{==============================================================================}

function SysDMACopyMemory(Source,Dest:Pointer;Size:LongWord):LongWord;
{Copy memory from the source to the destination address using DMA}
{Source: The source address to start the memory copy}
{Dest: The destination address to start the memory copy}
{Size: The size of memory to copy in bytes}
var
 Data:TDMAData;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 if DMAHostDefault = nil then Exit;

 {Check Source, Dest and Size}
 if Source = nil then Exit;
 if Dest = nil then Exit;
 if Size = 0 then Exit;

 {Create Data}
 FillChar(Data,SizeOf(TDMAData),0);
 Data.Source:=Source;
 Data.Dest:=Dest;
 Data.Size:=Size;
 Data.Flags:=DMA_DATA_FLAG_NONE;

 {Check Wide}
 if (DMAHostDefault.Device.DeviceFlags and DMA_FLAG_WIDE) <> 0 then
  begin
   Data.Flags:=Data.Flags or DMA_DATA_FLAG_SOURCE_WIDE or DMA_DATA_FLAG_DEST_WIDE;
  end;

 {Check Bulk}
 if (DMAHostDefault.Device.DeviceFlags and DMA_FLAG_BULK) <> 0 then
  begin
   Data.Flags:=Data.Flags or DMA_DATA_FLAG_BULK;
  end;

 {Perform Transfer}
 Result:=DMATransferRequest(DMAHostDefault,@Data,DMA_DIR_MEM_TO_MEM,DMA_DREQ_ID_NONE,DMA_REQUEST_FLAG_NONE,INFINITE);
end;

{==============================================================================}

function SysDMAReadPeripheral(Address,Dest:Pointer;Size,Peripheral:LongWord):LongWord;
{Read from a periperal address to the destination address using DMA}
{Address: The address of the periperhal register to read from}
{Dest: The destination address to start writing to}
{Size: The size of the read in bytes}
{Peripheral: The peripheral ID for data request gating (eg DMA_DREQ_ID_UART_RX)}
var
 Data:TDMAData;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 if DMAHostDefault = nil then Exit;

 {Check Address, Dest and Size}
 if Address = nil then Exit;
 if Dest = nil then Exit;
 if Size = 0 then Exit;

 {Create Data}
 FillChar(Data,SizeOf(TDMAData),0);
 Data.Source:=Address;
 Data.Dest:=Dest;
 Data.Size:=Size;
 Data.Flags:=DMA_DATA_FLAG_NONE;

 {Check No Increment}
 if (DMAHostDefault.Device.DeviceFlags and DMA_FLAG_NOINCREMENT) <> 0 then
  begin
   Data.Flags:=Data.Flags or DMA_DATA_FLAG_SOURCE_NOINCREMENT;

   {Check Data Request}
   if (DMAHostDefault.Device.DeviceFlags and DMA_FLAG_DREQ) <> 0 then
    begin
     Data.Flags:=Data.Flags or DMA_DATA_FLAG_SOURCE_DREQ;
    end;

   {Check Wide}
   if (DMAHostDefault.Device.DeviceFlags and DMA_FLAG_WIDE) <> 0 then
    begin
     Data.Flags:=Data.Flags or DMA_DATA_FLAG_DEST_WIDE;
    end;

   {Perform Transfer}
   Result:=DMATransferRequest(DMAHostDefault,@Data,DMA_DIR_DEV_TO_MEM,Peripheral,DMA_REQUEST_FLAG_NONE,INFINITE);
  end
 else
  begin
   Result:=ERROR_NOT_SUPPORTED;
  end;
end;

{==============================================================================}

function SysDMAWritePeripheral(Source,Address:Pointer;Size,Peripheral:LongWord):LongWord;
{Write to a peripheral address from the source address using DMA}
{Source: The source address to start reading from}
{Address: The address of the peripheral register to write to}
{Size: The size of the write in bytes}
{Peripheral: The peripheral ID for data request gating (eg DMA_DREQ_ID_UART_TX)}
var
 Data:TDMAData;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 if DMAHostDefault = nil then Exit;

 {Check Source, Address and Size}
 if Source = nil then Exit;
 if Address = nil then Exit;
 if Size = 0 then Exit;

 {Create Data}
 FillChar(Data,SizeOf(TDMAData),0);
 Data.Source:=Source;
 Data.Dest:=Address;
 Data.Size:=Size;
 Data.Flags:=DMA_DATA_FLAG_NONE;

 {Check No Increment}
 if (DMAHostDefault.Device.DeviceFlags and DMA_FLAG_NOINCREMENT) <> 0 then
  begin
   Data.Flags:=Data.Flags or DMA_DATA_FLAG_DEST_NOINCREMENT;

   {Check Data Request}
   if (DMAHostDefault.Device.DeviceFlags and DMA_FLAG_DREQ) <> 0 then
    begin
     Data.Flags:=Data.Flags or DMA_DATA_FLAG_DEST_DREQ;
    end;

   {Check Wide}
   if (DMAHostDefault.Device.DeviceFlags and DMA_FLAG_WIDE) <> 0 then
    begin
     Data.Flags:=Data.Flags or DMA_DATA_FLAG_SOURCE_WIDE;
    end;

   {Perform Transfer}
   Result:=DMATransferRequest(DMAHostDefault,@Data,DMA_DIR_MEM_TO_DEV,Peripheral,DMA_REQUEST_FLAG_NONE,INFINITE);
  end
 else
  begin
   Result:=ERROR_NOT_SUPPORTED;
  end;
end;

{==============================================================================}

function SysDMAAllocateBuffer(Size:LongWord):Pointer;
{Allocate a buffer compatible with DMA memory reads or writes}
{Size: The size of the buffer to allocate}
begin
 {}
 Result:=nil;

 if DMAHostDefault = nil then Exit;

 Result:=DMABufferAllocate(DMAHostDefault,Size);
end;

{==============================================================================}

function SysDMAAllocateBufferEx(var Size:LongWord):Pointer;
{Allocate a buffer compatible with DMA memory reads or writes}
{Size: The size of the buffer to allocate (Updated on return to actual size)}
begin
 {}
 Result:=nil;

 if DMAHostDefault = nil then Exit;

 Result:=DMABufferAllocateEx(DMAHostDefault,Size);
end;

{==============================================================================}

function SysDMAReleaseBuffer(Buffer:Pointer):LongWord;
{Release a buffer allocated with DMAAllocateBuffer}
{Buffer: The buffer to be released}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 if DMAHostDefault = nil then Exit;

 Result:=DMABufferRelease(Buffer);
end;

{==============================================================================}
{==============================================================================}
{DMA Helper Functions}
function DMAGetCount:LongWord;
{Get the current DMA host count}
begin
 {}
 Result:=DMAHostTableCount;
end;

{==============================================================================}

function DMAHostGetDefault:PDMAHost;
{Get the current default DMA host}
begin
 {}
 Result:=DMAHostDefault;
end;

{==============================================================================}

function DMAHostSetDefault(DMA:PDMAHost):LongWord;
{Set the current default DMA device}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check DMA}
 if DMA = nil then Exit;
 if DMA.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(DMAHostTableLock) = ERROR_SUCCESS then
  begin
   try
    {Check DMA}
    if DMAHostCheck(DMA) <> DMA then Exit;

    {Set DMA Default}
    DMAHostDefault:=DMA;

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    CriticalSectionUnlock(DMAHostTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function DMAHostCheck(DMA:PDMAHost):PDMAHost;
{Check if the supplied DMA is in the DMA host table}
var
 Current:PDMAHost;
begin
 {}
 Result:=nil;

 {Check DMA}
 if DMA = nil then Exit;
 if DMA.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(DMAHostTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get DMA}
    Current:=DMAHostTable;
    while Current <> nil do
     begin
      {Check DMA}
      if Current = DMA then
       begin
        Result:=DMA;
        Exit;
       end;

      {Get Next}
      Current:=Current.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(DMAHostTableLock);
   end;
  end;
end;

{==============================================================================}

function DMATypeToString(DMAType:LongWord):String;
{Convert a DMA type value to a string}
begin
 {}
 Result:='DMA_TYPE_UNKNOWN';

 if DMAType <= DMA_TYPE_MAX then
  begin
   Result:=DMA_TYPE_NAMES[DMAType];
  end;
end;

{==============================================================================}

function DMAStateToString(DMAState:LongWord):String;
{Convert a DMA state value to a string}
begin
 {}
 Result:='DMA_STATE_UNKNOWN';

 if DMAState <= DMA_STATE_MAX then
  begin
   Result:=DMA_STATE_NAMES[DMAState];
  end;
end;

{==============================================================================}

procedure DMALog(Level:Integer;DMA:PDMAHost;const AText:String);
var
 WorkBuffer:String;
begin
 {}
 {Check Level}
 if Level < DMA_DEFAULT_LOG_LEVEL then Exit;

 WorkBuffer:='';
 {Check Level}
 if Level = DMA_LOG_LEVEL_DEBUG then
  begin
   WorkBuffer:=WorkBuffer + '[DEBUG] ';
  end
 else if Level = DMA_LOG_LEVEL_WARN then
  begin
   WorkBuffer:=WorkBuffer + '[WARN] ';
  end
 else if Level = DMA_LOG_LEVEL_ERROR then
  begin
   WorkBuffer:=WorkBuffer + '[ERROR] ';
  end;

 {Add Prefix}
 WorkBuffer:=WorkBuffer + 'DMA: ';

 {Check DMA}
 if DMA <> nil then
  begin
   WorkBuffer:=WorkBuffer + DMA_NAME_PREFIX + IntToStr(DMA.DMAId) + ': ';
  end;

 {Output Logging}
 LoggingOutputEx(LOGGING_FACILITY_DMA,LogLevelToLoggingSeverity(Level),'DMA',WorkBuffer + AText);
end;

{==============================================================================}

procedure DMALogInfo(DMA:PDMAHost;const AText:String); inline;
begin
 {}
 DMALog(DMA_LOG_LEVEL_INFO,DMA,AText);
end;

{==============================================================================}

procedure DMALogWarn(DMA:PDMAHost;const AText:String); inline;
begin
 {}
 DMALog(DMA_LOG_LEVEL_WARN,DMA,AText);
end;

{==============================================================================}

procedure DMALogError(DMA:PDMAHost;const AText:String); inline;
begin
 {}
 DMALog(DMA_LOG_LEVEL_ERROR,DMA,AText);
end;

{==============================================================================}

procedure DMALogDebug(DMA:PDMAHost;const AText:String); inline;
begin
 {}
 DMALog(DMA_LOG_LEVEL_DEBUG,DMA,AText);
end;

{==============================================================================}
{==============================================================================}

initialization
 DMAInit;

{==============================================================================}

finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.

