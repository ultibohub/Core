{
Ultibo UART interface unit.

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


UART Devices
============

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit UART; 

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,SysUtils;

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
const
 {UART specific constants}
 UART_NAME_PREFIX = 'UART';  {Name prefix for UART Devices}

 {UART Device Types}
 UART_TYPE_NONE      = 0;
 UART_TYPE_16550     = 1; {16550 UART and similar variants (eg 16550A) (Differences are handled by driver)}
 UART_TYPE_16650     = 2; {16650 UART and similar variants (eg 16C650) (Differences are handled by driver)}
 
 {UART Device States}
 UART_STATE_DISABLED = 0;
 UART_STATE_ENABLED  = 1;
 
 {UART Device Flags}
 UART_FLAG_NONE          = $00000000;
 
 {UART logging}
 UART_LOG_LEVEL_DEBUG     = LOG_LEVEL_DEBUG;  {UART debugging messages}
 UART_LOG_LEVEL_INFO      = LOG_LEVEL_INFO;   {UART informational messages, such as a device being attached or detached}
 UART_LOG_LEVEL_ERROR     = LOG_LEVEL_ERROR;  {UART error messages}
 UART_LOG_LEVEL_NONE      = LOG_LEVEL_NONE;   {No UART messages}
 
var 
 UART_DEFAULT_LOG_LEVEL:LongWord = UART_LOG_LEVEL_INFO; {Minimum level for UART messages.  Only messages with level greater than or equal to this will be printed}
 
var 
 {UART logging}
 UART_LOG_ENABLED:Boolean; 
 
{==============================================================================}
type
 {UART specific types}
 
 {UART Properties}
 PUARTProperties = ^TUARTProperties;
 TUARTProperties = record
  //To Do
 end;
 
 {UART Device}
 PUARTDevice = ^TUARTDevice;
 
 {UART Enumeration Callback}
 TUARTEnumerate = function(UART:PUARTDevice;Data:Pointer):LongWord;
 {UART Notification Callback}
 TUARTNotification = function(Device:PDevice;Data:Pointer;Notification:LongWord):LongWord;
 
 {UART Device Methods}
 //To Do
 TUARTDeviceProperties = function(UART:PUARTDevice;Properties:PUARTProperties):LongWord;
 
 TUARTDevice = record
  {Device Properties}
  Device:TDevice;                                 {The Device entry for this UART}
  {UART Properties}
  UARTId:LongWord;                                {Unique Id of this UART in the UART table}
  UARTState:LongWord;                             {UART state (eg UART_STATE_ENABLED)}
  //To Do
  DeviceProperties:TUARTDeviceProperties;         {A Device specific DeviceProperties method implementing the standard UART device interface}
  {Statistics Properties}
  //To Do
  {Driver Properties}
  Lock:TMutexHandle;                              {Device lock}
  //To Do
  Properties:TUARTProperties;                     {Device properties}
  {Internal Properties}                                                                        
  Prev:PUARTDevice;                               {Previous entry in UART table}
  Next:PUARTDevice;                               {Next entry in UART table}
 end; 
 
{==============================================================================}
{var}
 {UART specific variables}

{==============================================================================}
{Initialization Functions}
procedure UARTInit;
 
{==============================================================================}
{UART Functions}
//To Do

function UARTDeviceProperties(UART:PUARTDevice;Properties:PUARTProperties):LongWord;
  
function UARTDeviceCreate:PUARTDevice;
function UARTDeviceCreateEx(Size:LongWord):PUARTDevice;
function UARTDeviceDestroy(UART:PUARTDevice):LongWord;

function UARTDeviceRegister(UART:PUARTDevice):LongWord;
function UARTDeviceDeregister(UART:PUARTDevice):LongWord;

function UARTDeviceFind(UARTId:LongWord):PUARTDevice;
function UARTDeviceEnumerate(Callback:TUARTEnumerate;Data:Pointer):LongWord;
 
function UARTDeviceNotification(UART:PUARTDevice;Callback:TUARTNotification;Data:Pointer;Notification,Flags:LongWord):LongWord;

{==============================================================================}
{RTL UART Functions}
 
{==============================================================================}
{UART Helper Functions}
function UARTGetCount:LongWord; inline;
function UARTDeviceGetDefault:PUARTDevice; inline;
function UARTDeviceSetDefault(UART:PUARTDevice):LongWord; 

function UARTDeviceCheck(UART:PUARTDevice):PUARTDevice;

procedure UARTLog(Level:LongWord;UART:PUARTDevice;const AText:String);
procedure UARTLogInfo(UART:PUARTDevice;const AText:String); inline;
procedure UARTLogError(UART:PUARTDevice;const AText:String); inline;
procedure UARTLogDebug(UART:PUARTDevice;const AText:String); inline;
 
{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {UART specific variables}
 UARTInitialized:Boolean;

 UARTDeviceTable:PUARTDevice;
 UARTDeviceTableLock:TCriticalSectionHandle = INVALID_HANDLE_VALUE;
 UARTDeviceTableCount:LongWord;

 UARTDeviceDefault:PUARTDevice;

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure UARTInit;
begin
 {}
 {Check Initialized}
 if UARTInitialized then Exit;
 
 {Initialize Logging}
 UART_LOG_ENABLED:=(UART_DEFAULT_LOG_LEVEL <> UART_LOG_LEVEL_NONE); 
 
 {Initialize UART Table}
 UARTDeviceTable:=nil;
 UARTDeviceTableLock:=CriticalSectionCreate; 
 UARTDeviceTableCount:=0;
 if UARTDeviceTableLock = INVALID_HANDLE_VALUE then
  begin
   if UART_LOG_ENABLED then UARTLogError(nil,'Failed to create UART table lock');
  end;
 UARTDeviceDefault:=nil;
 
 {Register Platform UART Handlers}
 //To Do
 
 UARTInitialized:=True;
end;
 
{==============================================================================}
{==============================================================================}
{UART Functions}

//To Do

{==============================================================================}

function UARTDeviceProperties(UART:PUARTDevice;Properties:PUARTProperties):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 //To Do
 
end;

{==============================================================================}

function UARTDeviceCreate:PUARTDevice;
{Create a new UART entry}
{Return: Pointer to new UART entry or nil if UART could not be created}
begin
 {}
 Result:=UARTDeviceCreateEx(SizeOf(TUARTDevice));
end;

{==============================================================================}

function UARTDeviceCreateEx(Size:LongWord):PUARTDevice;
{Create a new UART entry}
{Size: Size in bytes to allocate for new UART (Including the UART entry)}
{Return: Pointer to new UART entry or nil if UART could not be created}
begin
 {}
 Result:=nil;
 
 {Check Size}
 if Size < SizeOf(TUARTDevice) then Exit;
 
 {Create UART}
 Result:=PUARTDevice(DeviceCreateEx(Size));
 if Result = nil then Exit;
 
 {Update Device}
 Result.Device.DeviceBus:=DEVICE_BUS_NONE;   
 Result.Device.DeviceType:=UART_TYPE_NONE;
 Result.Device.DeviceFlags:=UART_FLAG_NONE;
 Result.Device.DeviceData:=nil;

 {Update UART}
 Result.UARTId:=DEVICE_ID_ANY;
 Result.UARTState:=UART_STATE_DISABLED;
 //To Do
 Result.DeviceProperties:=nil;
 Result.Lock:=INVALID_HANDLE_VALUE;
 
 {Create Lock}
 Result.Lock:=MutexCreate;
 if Result.Lock = INVALID_HANDLE_VALUE then
  begin
   if UART_LOG_ENABLED then UARTLogError(nil,'Failed to create lock for UART device');
   UARTDeviceDestroy(Result);
   Result:=nil;
   Exit;
  end;
end;

{==============================================================================}

function UARTDeviceDestroy(UART:PUARTDevice):LongWord;
{Destroy an existing UART entry}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check UART}
 if UART = nil then Exit;
 if UART.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check UART}
 Result:=ERROR_IN_USE;
 if UARTDeviceCheck(UART) = UART then Exit;

 {Check State}
 if UART.Device.DeviceState <> DEVICE_STATE_UNREGISTERED then Exit;
 
 {Destroy Lock}
 if UART.Lock <> INVALID_HANDLE_VALUE then
  begin
   MutexDestroy(UART.Lock);
  end;
 
 {Destroy UART} 
 Result:=DeviceDestroy(@UART.Device);
end;

{==============================================================================}

function UARTDeviceRegister(UART:PUARTDevice):LongWord;
{Register a new UART in the UART table}
var
 UARTId:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check UART}
 if UART = nil then Exit;
 if UART.UARTId <> DEVICE_ID_ANY then Exit;
 if UART.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check UART}
 Result:=ERROR_ALREADY_EXISTS;
 if UARTDeviceCheck(UART) = UART then Exit;
 
 {Check State}
 if UART.Device.DeviceState <> DEVICE_STATE_UNREGISTERED then Exit;
 
 {Insert UART}
 if CriticalSectionLock(UARTDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Update UART}
    UARTId:=0;
    while UARTDeviceFind(UARTId) <> nil do
     begin
      Inc(UARTId);
     end;
    UART.UARTId:=UARTId;
    
    {Update Device}
    UART.Device.DeviceName:=UART_NAME_PREFIX + IntToStr(UART.UARTId); 
    UART.Device.DeviceClass:=DEVICE_CLASS_UART;
    
    {Register Device}
    Result:=DeviceRegister(@UART.Device);
    if Result <> ERROR_SUCCESS then
     begin
      UART.UARTId:=DEVICE_ID_ANY;
      Exit;
     end; 
    
    {Link UART}
    if UARTDeviceTable = nil then
     begin
      UARTDeviceTable:=UART;
     end
    else
     begin
      UART.Next:=UARTDeviceTable;
      UARTDeviceTable.Prev:=UART;
      UARTDeviceTable:=UART;
     end;
 
    {Increment Count}
    Inc(UARTDeviceTableCount);
    
    {Check Default}
    if UARTDeviceDefault = nil then
     begin
      UARTDeviceDefault:=UART;
     end;
    
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    CriticalSectionUnlock(UARTDeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;

{==============================================================================}

function UARTDeviceDeregister(UART:PUARTDevice):LongWord;
{Deregister a UART from the UART table}
var
 Prev:PUARTDevice;
 Next:PUARTDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check UART}
 if UART = nil then Exit;
 if UART.UARTId = DEVICE_ID_ANY then Exit;
 if UART.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check UART}
 Result:=ERROR_NOT_FOUND;
 if UARTDeviceCheck(UART) <> UART then Exit;
 
 {Check State}
 if UART.Device.DeviceState <> DEVICE_STATE_REGISTERED then Exit;
 
 {Remove UART}
 if CriticalSectionLock(UARTDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Deregister Device}
    Result:=DeviceDeregister(@UART.Device);
    if Result <> ERROR_SUCCESS then Exit;
    
    {Unlink UART}
    Prev:=UART.Prev;
    Next:=UART.Next;
    if Prev = nil then
     begin
      UARTDeviceTable:=Next;
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
    Dec(UARTDeviceTableCount);
 
    {Check Default}
    if UARTDeviceDefault = UART then
     begin
      UARTDeviceDefault:=UARTDeviceTable;
     end;
 
    {Update UART}
    UART.UARTId:=DEVICE_ID_ANY;
 
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    CriticalSectionUnlock(UARTDeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;

{==============================================================================}

function UARTDeviceFind(UARTId:LongWord):PUARTDevice;
var
 UART:PUARTDevice;
begin
 {}
 Result:=nil;
 
 {Check Id}
 if UARTId = DEVICE_ID_ANY then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(UARTDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get UART}
    UART:=UARTDeviceTable;
    while UART <> nil do
     begin
      {Check State}
      if UART.Device.DeviceState = DEVICE_STATE_REGISTERED then
       begin
        {Check Id}
        if UART.UARTId = UARTId then
         begin
          Result:=UART;
          Exit;
         end;
       end;
       
      {Get Next}
      UART:=UART.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(UARTDeviceTableLock);
   end;
  end;
end;
       
{==============================================================================}

function UARTDeviceEnumerate(Callback:TUARTEnumerate;Data:Pointer):LongWord;
var
 UART:PUARTDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Callback}
 if not Assigned(Callback) then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(UARTDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get UART}
    UART:=UARTDeviceTable;
    while UART <> nil do
     begin
      {Check State}
      if UART.Device.DeviceState = DEVICE_STATE_REGISTERED then
       begin
        if Callback(UART,Data) <> ERROR_SUCCESS then Exit;
       end;
       
      {Get Next}
      UART:=UART.Next;
     end;
     
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    CriticalSectionUnlock(UARTDeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;

{==============================================================================}

function UARTDeviceNotification(UART:PUARTDevice;Callback:TUARTNotification;Data:Pointer;Notification,Flags:LongWord):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check UART}
 if UART = nil then
  begin
   Result:=DeviceNotification(nil,DEVICE_CLASS_UART,Callback,Data,Notification,Flags);
  end
 else
  begin 
   {Check UART}
   if UART.Device.Signature <> DEVICE_SIGNATURE then Exit;

   Result:=DeviceNotification(@UART.Device,DEVICE_CLASS_UART,Callback,Data,Notification,Flags);
  end; 
end;

{==============================================================================}
{==============================================================================}
{RTL UART Functions}

{==============================================================================}
{==============================================================================}
{UART Helper Functions}
function UARTGetCount:LongWord; inline;
{Get the current UART count}
begin
 {}
 Result:=UARTDeviceTableCount;
end;

{==============================================================================}

function UARTDeviceGetDefault:PUARTDevice; inline;
{Get the current default UART device}
begin
 {}
 Result:=UARTDeviceDefault;
end;

{==============================================================================}

function UARTDeviceSetDefault(UART:PUARTDevice):LongWord; 
{Set the current default UART device}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check UART}
 if UART = nil then Exit;
 if UART.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(UARTDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Check UART}
    if UARTDeviceCheck(UART) <> UART then Exit;
    
    {Set UART Default}
    UARTDeviceDefault:=UART;
    
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    CriticalSectionUnlock(UARTDeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function UARTDeviceCheck(UART:PUARTDevice):PUARTDevice;
{Check if the supplied UART is in the UART table}
var
 Current:PUARTDevice;
begin
 {}
 Result:=nil;
 
 {Check UART}
 if UART = nil then Exit;
 if UART.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(UARTDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get UART}
    Current:=UARTDeviceTable;
    while Current <> nil do
     begin
      {Check UART}
      if Current = UART then
       begin
        Result:=UART;
        Exit;
       end;
      
      {Get Next}
      Current:=Current.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(UARTDeviceTableLock);
   end;
  end;
end;

{==============================================================================}

procedure UARTLog(Level:LongWord;UART:PUARTDevice;const AText:String);
var
 WorkBuffer:String;
begin
 {}
 {Check Level}
 if Level < UART_DEFAULT_LOG_LEVEL then Exit;
 
 WorkBuffer:='';
 {Check Level}
 if Level = UART_LOG_LEVEL_DEBUG then
  begin
   WorkBuffer:=WorkBuffer + '[DEBUG] ';
  end
 else if Level = UART_LOG_LEVEL_ERROR then
  begin
   WorkBuffer:=WorkBuffer + '[ERROR] ';
  end;
 
 {Add Prefix}
 WorkBuffer:=WorkBuffer + 'UART: ';
 
 {Check UART}
 if UART <> nil then
  begin
   WorkBuffer:=WorkBuffer + UART_NAME_PREFIX + IntToStr(UART.UARTId) + ': ';
  end;

 {Output Logging}  
 LoggingOutputEx(LOGGING_FACILITY_UART,LogLevelToLoggingSeverity(Level),'UART',WorkBuffer + AText);
end;

{==============================================================================}

procedure UARTLogInfo(UART:PUARTDevice;const AText:String); inline;
begin
 {}
 UARTLog(UART_LOG_LEVEL_INFO,UART,AText);
end;

{==============================================================================}

procedure UARTLogError(UART:PUARTDevice;const AText:String); inline;
begin
 {}
 UARTLog(UART_LOG_LEVEL_ERROR,UART,AText);
end;

{==============================================================================}

procedure UARTLogDebug(UART:PUARTDevice;const AText:String); inline;
begin
 {}
 UARTLog(UART_LOG_LEVEL_DEBUG,UART,AText);
end;

{==============================================================================}
{==============================================================================}

initialization
 UARTInit;

{==============================================================================}
 
finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.