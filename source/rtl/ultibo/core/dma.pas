{
Ultibo DMA interface unit.

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


DMA Hosts
=========

?????

The DMA interfaces are 1 tier (Host only) whereas the USB interface is 3 tier (Host, Device and Driver).

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit DMA;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,SysUtils;

//To Do

//See: \linux-rpi-3.18.y\drivers\dma\bcm2835-dma.c
//     \linux-rpi-3.18.y\drivers\dma\bcm2708-dmaengine.c  (Newer version of above)

//     See bcm2835_dma_callback for IRQ handler


{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
const
 {DMA specific constants}
 DMA_NAME_PREFIX = 'DMA';      {Name prefix for DMA Hosts}

 {DMA Host Types}
 DMA_TYPE_NONE = 0;
 //To Do
 
 {DMA Host States}
 DMA_STATE_DISABLED = 0;
 DMA_STATE_ENABLED  = 1;
 
 {DMA Host Flags}
 DMA_FLAG_NONE = 0;
 //To Do
 
 {DMA logging}
 DMA_LOG_LEVEL_DEBUG     = LOG_LEVEL_DEBUG;  {DMA debugging messages}
 DMA_LOG_LEVEL_INFO      = LOG_LEVEL_INFO;   {DMA informational messages, such as a device being attached or detached}
 DMA_LOG_LEVEL_ERROR     = LOG_LEVEL_ERROR;  {DMA error messages}
 DMA_LOG_LEVEL_NONE      = LOG_LEVEL_NONE;   {No DMA messages}

var 
 DMA_DEFAULT_LOG_LEVEL:LongWord = DMA_LOG_LEVEL_INFO; {Minimum level for DMA messages.  Only messages with level greater than or equal to this will be printed}
 
var 
 {DMA logging}
 DMA_LOG_ENABLED:Boolean; 
 
{==============================================================================}
type
 {DMA specific types}
 {DMA Host}
 PDMAHost = ^TDMAHost;
 
 {DMA Enumeration Callback}
 TDMAEnumerate = function(DMA:PDMAHost;Data:Pointer):LongWord;
 {DMA Notification Callback}
 TDMANotification = function(Device:PDevice;Data:Pointer;Notification:LongWord):LongWord;

 {DMA Host Methods}
 //To Do
 
 TDMAHost = record
  {Device Properties}
  Device:TDevice;                          {The Device entry for this DMA host}
  {DMA Properties}
  DMAId:LongWord;                          {Unique Id of this DMA host in the DMA host table}
  DMAState:LongWord;                       {DMA state (eg DMA_STATE_ENABLED)}
  //To Do
  {Statistics Properties}
  //To Do
  {Driver Properties}
  //To Do
  {Internal Properties}                                                                        
  Prev:PDMAHost;                           {Previous entry in DMA host table}
  Next:PDMAHost;                           {Next entry in DMA host table}
 end;
 
{==============================================================================}
{var}
 {DMA specific variables}
 
{==============================================================================}
{Initialization Functions}
procedure DMAInit;
 
{==============================================================================}
{DMA Functions}
//To Do

function DMAHostCreate:PDMAHost;
function DMAHostCreateEx(Size:LongWord):PDMAHost;
function DMAHostDestroy(DMA:PDMAHost):LongWord;

function DMAHostRegister(DMA:PDMAHost):LongWord;
function DMAHostDeregister(DMA:PDMAHost):LongWord;

function DMAHostFind(DMAId:LongWord):PDMAHost;
function DMAHostEnumerate(Callback:TDMAEnumerate;Data:Pointer):LongWord;
 
function DMAHostNotification(DMA:PDMAHost;Callback:TDMANotification;Data:Pointer;Notification,Flags:LongWord):LongWord;
 
{==============================================================================}
{DMA Helper Functions}
function DMAGetCount:LongWord; inline;

function DMAHostCheck(DMA:PDMAHost):PDMAHost;

procedure DMALog(Level:Integer;DMA:PDMAHost;const AText:String);
procedure DMALogInfo(DMA:PDMAHost;const AText:String);
procedure DMALogError(DMA:PDMAHost;const AText:String);
procedure DMALogDebug(DMA:PDMAHost;const AText:String);
 
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
 
 DMAInitialized:=True;
end;
 
{==============================================================================}
{==============================================================================}
{DMA Functions}

//To Do

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
 //To Do
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
{DMA Helper Functions}
function DMAGetCount:LongWord; inline;
{Get the current DMA count}
begin
 {}
 Result:=DMAHostTableCount;
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

procedure DMALogInfo(DMA:PDMAHost;const AText:String);
begin
 {}
 DMALog(DMA_LOG_LEVEL_INFO,DMA,AText);
end;

{==============================================================================}

procedure DMALogError(DMA:PDMAHost;const AText:String);
begin
 {}
 DMALog(DMA_LOG_LEVEL_ERROR,DMA,AText);
end;

{==============================================================================}

procedure DMALogDebug(DMA:PDMAHost;const AText:String);
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
 