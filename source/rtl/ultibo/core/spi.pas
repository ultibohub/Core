{
Ultibo SPI interface unit.

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


SPI Hosts
=========

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit SPI; 

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,SysUtils;

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
const
 {SPI specific constants}
 SPI_NAME_PREFIX = 'SPI';  {Name prefix for SPI Devices}
 
 {SPI Device Types}
 SPI_TYPE_NONE      = 0;
 
 {SPI Device States}
 SPI_STATE_DISABLED = 0;
 SPI_STATE_ENABLED  = 1;
 
 {SPI Device Flags}
 SPI_FLAG_NONE          = $00000000;
 
 {SPI logging}
 SPI_LOG_LEVEL_DEBUG     = LOG_LEVEL_DEBUG;  {SPI debugging messages}
 SPI_LOG_LEVEL_INFO      = LOG_LEVEL_INFO;   {SPI informational messages, such as a device being attached or detached}
 SPI_LOG_LEVEL_ERROR     = LOG_LEVEL_ERROR;  {SPI error messages}
 SPI_LOG_LEVEL_NONE      = LOG_LEVEL_NONE;   {No SPI messages}

var 
 SPI_DEFAULT_LOG_LEVEL:LongWord = SPI_LOG_LEVEL_INFO; {Minimum level for SPI messages.  Only messages with level greater than or equal to this will be printed}
 
var 
 {SPI logging}
 SPI_LOG_ENABLED:Boolean; 

{==============================================================================}
type
 {SPI specific types}

 {SPI Properties}
 PSPIProperties = ^TSPIProperties;
 TSPIProperties = record
  //To Do
 end;
 
 {SPI Device}
 PSPIDevice = ^TSPIDevice;
 
 {SPI Enumeration Callback}
 TSPIEnumerate = function(SPI:PSPIDevice;Data:Pointer):LongWord;
 {SPI Notification Callback}
 TSPINotification = function(Device:PDevice;Data:Pointer;Notification:LongWord):LongWord;
 
 {SPI Device Methods}
 //To Do
 TSPIDeviceProperties = function(SPI:PSPIDevice;var Properties:PSPIProperties):LongWord;
 
 TSPIDevice = record
  {Device Properties}
  Device:TDevice;                                 {The Device entry for this SPI}
  {SPI Properties}
  SPIId:LongWord;                                 {Unique Id of this SPI in the SPI table}
  SPIState:LongWord;                              {SPI state (eg SPI_STATE_ENABLED)}
  //To Do
  DeviceProperties:TSPIDeviceProperties;          {A Device specific DeviceProperties method implementing the standard SPI device interface}
  {Statistics Properties}
  //To Do
  {Driver Properties}
  Lock:TMutexHandle;                              {Device lock}
  //To Do
  Properties:TSPIProperties;                      {Device properties}
  {Internal Properties}                                                                        
  Prev:PSPIDevice;                                {Previous entry in SPI table}
  Next:PSPIDevice;                                {Next entry in SPI table}
 end; 
 
{==============================================================================}
{var}
 {SPI specific variables}

{==============================================================================}
{Initialization Functions}
procedure SPIInit;
 
{==============================================================================}
{SPI Functions}
 
//To Do

function SPIDeviceProperties(SPI:PSPIDevice;var Properties:PSPIProperties):LongWord;
  
function SPIDeviceCreate:PSPIDevice;
function SPIDeviceCreateEx(Size:LongWord):PSPIDevice;
function SPIDeviceDestroy(SPI:PSPIDevice):LongWord;

function SPIDeviceRegister(SPI:PSPIDevice):LongWord;
function SPIDeviceDeregister(SPI:PSPIDevice):LongWord;

function SPIDeviceFind(SPIId:LongWord):PSPIDevice;
function SPIDeviceEnumerate(Callback:TSPIEnumerate;Data:Pointer):LongWord;
 
function SPIDeviceNotification(SPI:PSPIDevice;Callback:TSPINotification;Data:Pointer;Notification,Flags:LongWord):LongWord;

{==============================================================================}
{RTL SPI Functions}

{==============================================================================}
{SPI Helper Functions}
function SPIGetCount:LongWord; inline;
function SPIDeviceGetDefault:PSPIDevice; inline;

function SPIDeviceCheck(SPI:PSPIDevice):PSPIDevice;

procedure SPILog(Level:LongWord;SPI:PSPIDevice;const AText:String);
procedure SPILogInfo(SPI:PSPIDevice;const AText:String); inline;
procedure SPILogError(SPI:PSPIDevice;const AText:String); inline;
procedure SPILogDebug(SPI:PSPIDevice;const AText:String); inline;
 
{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {SPI specific variables}
 SPIInitialized:Boolean;

 SPIDeviceTable:PSPIDevice;
 SPIDeviceTableLock:TCriticalSectionHandle = INVALID_HANDLE_VALUE;
 SPIDeviceTableCount:LongWord;

 SPIDeviceDefault:PSPIDevice;

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure SPIInit;
begin
 {}
 {Check Initialized}
 if SPIInitialized then Exit;
 
 {Initialize Logging}
 SPI_LOG_ENABLED:=(SPI_DEFAULT_LOG_LEVEL <> SPI_LOG_LEVEL_NONE); 
 
 {Initialize SPI Table}
 SPIDeviceTable:=nil;
 SPIDeviceTableLock:=CriticalSectionCreate; 
 SPIDeviceTableCount:=0;
 if SPIDeviceTableLock = INVALID_HANDLE_VALUE then
  begin
   if SPI_LOG_ENABLED then SPILogError(nil,'Failed to create SPI table lock');
  end;
 SPIDeviceDefault:=nil;
 
 {Register Platform SPI Handlers}
 //To Do
 
 SPIInitialized:=True;
end;
 
{==============================================================================}
{==============================================================================}
{SPI Functions}

//To Do

{==============================================================================}
 
function SPIDeviceProperties(SPI:PSPIDevice;var Properties:PSPIProperties):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 //To Do
 
end;

{==============================================================================}

function SPIDeviceCreate:PSPIDevice;
{Create a new SPI entry}
{Return: Pointer to new SPI entry or nil if SPI could not be created}
begin
 {}
 Result:=SPIDeviceCreateEx(SizeOf(TSPIDevice));
end;

{==============================================================================}

function SPIDeviceCreateEx(Size:LongWord):PSPIDevice;
{Create a new SPI entry}
{Size: Size in bytes to allocate for new SPI (Including the SPI entry)}
{Return: Pointer to new SPI entry or nil if SPI could not be created}
begin
 {}
 Result:=nil;
 
 {Check Size}
 if Size < SizeOf(TSPIDevice) then Exit;
 
 {Create SPI}
 Result:=PSPIDevice(DeviceCreateEx(Size));
 if Result = nil then Exit;
 
 {Update Device}
 Result.Device.DeviceBus:=DEVICE_BUS_NONE;   
 Result.Device.DeviceType:=SPI_TYPE_NONE;
 Result.Device.DeviceFlags:=SPI_FLAG_NONE;
 Result.Device.DeviceData:=nil;

 {Update SPI}
 Result.SPIId:=DEVICE_ID_ANY;
 Result.SPIState:=SPI_STATE_DISABLED;
 //To Do
 Result.DeviceProperties:=nil;
 Result.Lock:=INVALID_HANDLE_VALUE;
 
 {Create Lock}
 Result.Lock:=MutexCreate;
 if Result.Lock = INVALID_HANDLE_VALUE then
  begin
   if SPI_LOG_ENABLED then SPILogError(nil,'Failed to create lock for SPI device');
   SPIDeviceDestroy(Result);
   Result:=nil;
   Exit;
  end;
end;

{==============================================================================}

function SPIDeviceDestroy(SPI:PSPIDevice):LongWord;
{Destroy an existing SPI entry}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check SPI}
 if SPI = nil then Exit;
 if SPI.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check SPI}
 Result:=ERROR_IN_USE;
 if SPIDeviceCheck(SPI) = SPI then Exit;

 {Check State}
 if SPI.Device.DeviceState <> DEVICE_STATE_UNREGISTERED then Exit;
 
 {Destroy Lock}
 if SPI.Lock <> INVALID_HANDLE_VALUE then
  begin
   MutexDestroy(SPI.Lock);
  end;
 
 {Destroy SPI} 
 Result:=DeviceDestroy(@SPI.Device);
end;

{==============================================================================}

function SPIDeviceRegister(SPI:PSPIDevice):LongWord;
{Register a new SPI in the SPI table}
var
 SPIId:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check SPI}
 if SPI = nil then Exit;
 if SPI.SPIId <> DEVICE_ID_ANY then Exit;
 if SPI.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check SPI}
 Result:=ERROR_ALREADY_EXISTS;
 if SPIDeviceCheck(SPI) = SPI then Exit;
 
 {Check State}
 if SPI.Device.DeviceState <> DEVICE_STATE_UNREGISTERED then Exit;
 
 {Insert SPI}
 if CriticalSectionLock(SPIDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Update SPI}
    SPIId:=0;
    while SPIDeviceFind(SPIId) <> nil do
     begin
      Inc(SPIId);
     end;
    SPI.SPIId:=SPIId;
    
    {Update Device}
    SPI.Device.DeviceName:=SPI_NAME_PREFIX + IntToStr(SPI.SPIId); 
    SPI.Device.DeviceClass:=DEVICE_CLASS_SPI;
    
    {Register Device}
    Result:=DeviceRegister(@SPI.Device);
    if Result <> ERROR_SUCCESS then
     begin
      SPI.SPIId:=DEVICE_ID_ANY;
      Exit;
     end; 
    
    {Link SPI}
    if SPIDeviceTable = nil then
     begin
      SPIDeviceTable:=SPI;
     end
    else
     begin
      SPI.Next:=SPIDeviceTable;
      SPIDeviceTable.Prev:=SPI;
      SPIDeviceTable:=SPI;
     end;
 
    {Increment Count}
    Inc(SPIDeviceTableCount);
    
    {Check Default}
    if SPIDeviceDefault = nil then
     begin
      SPIDeviceDefault:=SPI;
     end;
    
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    CriticalSectionUnlock(SPIDeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;

{==============================================================================}

function SPIDeviceDeregister(SPI:PSPIDevice):LongWord;
{Deregister a SPI from the SPI table}
var
 Prev:PSPIDevice;
 Next:PSPIDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check SPI}
 if SPI = nil then Exit;
 if SPI.SPIId = DEVICE_ID_ANY then Exit;
 if SPI.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check SPI}
 Result:=ERROR_NOT_FOUND;
 if SPIDeviceCheck(SPI) <> SPI then Exit;
 
 {Check State}
 if SPI.Device.DeviceState <> DEVICE_STATE_REGISTERED then Exit;
 
 {Remove SPI}
 if CriticalSectionLock(SPIDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Deregister Device}
    Result:=DeviceDeregister(@SPI.Device);
    if Result <> ERROR_SUCCESS then Exit;
    
    {Unlink SPI}
    Prev:=SPI.Prev;
    Next:=SPI.Next;
    if Prev = nil then
     begin
      SPIDeviceTable:=Next;
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
    Dec(SPIDeviceTableCount);
 
    {Check Default}
    if SPIDeviceDefault = SPI then
     begin
      SPIDeviceDefault:=SPIDeviceTable;
     end;
 
    {Update SPI}
    SPI.SPIId:=DEVICE_ID_ANY;
 
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    CriticalSectionUnlock(SPIDeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;

{==============================================================================}

function SPIDeviceFind(SPIId:LongWord):PSPIDevice;
var
 SPI:PSPIDevice;
begin
 {}
 Result:=nil;
 
 {Check Id}
 if SPIId = DEVICE_ID_ANY then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(SPIDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get SPI}
    SPI:=SPIDeviceTable;
    while SPI <> nil do
     begin
      {Check State}
      if SPI.Device.DeviceState = DEVICE_STATE_REGISTERED then
       begin
        {Check Id}
        if SPI.SPIId = SPIId then
         begin
          Result:=SPI;
          Exit;
         end;
       end;
       
      {Get Next}
      SPI:=SPI.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(SPIDeviceTableLock);
   end;
  end;
end;
       
{==============================================================================}

function SPIDeviceEnumerate(Callback:TSPIEnumerate;Data:Pointer):LongWord;
var
 SPI:PSPIDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Callback}
 if not Assigned(Callback) then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(SPIDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get SPI}
    SPI:=SPIDeviceTable;
    while SPI <> nil do
     begin
      {Check State}
      if SPI.Device.DeviceState = DEVICE_STATE_REGISTERED then
       begin
        if Callback(SPI,Data) <> ERROR_SUCCESS then Exit;
       end;
       
      {Get Next}
      SPI:=SPI.Next;
     end;
     
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    CriticalSectionUnlock(SPIDeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;

{==============================================================================}

function SPIDeviceNotification(SPI:PSPIDevice;Callback:TSPINotification;Data:Pointer;Notification,Flags:LongWord):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check SPI}
 if SPI = nil then
  begin
   Result:=DeviceNotification(nil,DEVICE_CLASS_SPI,Callback,Data,Notification,Flags);
  end
 else
  begin 
   {Check SPI}
   if SPI.Device.Signature <> DEVICE_SIGNATURE then Exit;

   Result:=DeviceNotification(@SPI.Device,DEVICE_CLASS_SPI,Callback,Data,Notification,Flags);
  end; 
end;

{==============================================================================}
{==============================================================================}
{RTL SPI Functions}

{==============================================================================}
{==============================================================================}
{SPI Helper Functions}
function SPIGetCount:LongWord; inline;
{Get the current SPI count}
begin
 {}
 Result:=SPIDeviceTableCount;
end;

{==============================================================================}

function SPIDeviceGetDefault:PSPIDevice; inline;
{Get the current default SPI device}
begin
 {}
 Result:=SPIDeviceDefault;
end;

{==============================================================================}

function SPIDeviceCheck(SPI:PSPIDevice):PSPIDevice;
{Check if the supplied SPI is in the SPI table}
var
 Current:PSPIDevice;
begin
 {}
 Result:=nil;
 
 {Check SPI}
 if SPI = nil then Exit;
 if SPI.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(SPIDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get SPI}
    Current:=SPIDeviceTable;
    while Current <> nil do
     begin
      {Check SPI}
      if Current = SPI then
       begin
        Result:=SPI;
        Exit;
       end;
      
      {Get Next}
      Current:=Current.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(SPIDeviceTableLock);
   end;
  end;
end;

{==============================================================================}

procedure SPILog(Level:LongWord;SPI:PSPIDevice;const AText:String);
var
 WorkBuffer:String;
begin
 {}
 {Check Level}
 if Level < SPI_DEFAULT_LOG_LEVEL then Exit;
 
 WorkBuffer:='';
 {Check Level}
 if Level = SPI_LOG_LEVEL_DEBUG then
  begin
   WorkBuffer:=WorkBuffer + '[DEBUG] ';
  end
 else if Level = SPI_LOG_LEVEL_ERROR then
  begin
   WorkBuffer:=WorkBuffer + '[ERROR] ';
  end;
 
 {Add Prefix}
 WorkBuffer:=WorkBuffer + 'SPI: ';
 
 {Check SPI}
 if SPI <> nil then
  begin
   WorkBuffer:=WorkBuffer + SPI_NAME_PREFIX + IntToStr(SPI.SPIId) + ': ';
  end;

 {Output Logging}  
 LoggingOutputEx(LOGGING_FACILITY_SPI,LogLevelToLoggingSeverity(Level),'SPI',WorkBuffer + AText);
end;

{==============================================================================}

procedure SPILogInfo(SPI:PSPIDevice;const AText:String); inline;
begin
 {}
 SPILog(SPI_LOG_LEVEL_INFO,SPI,AText);
end;

{==============================================================================}

procedure SPILogError(SPI:PSPIDevice;const AText:String); inline;
begin
 {}
 SPILog(SPI_LOG_LEVEL_ERROR,SPI,AText);
end;

{==============================================================================}

procedure SPILogDebug(SPI:PSPIDevice;const AText:String); inline;
begin
 {}
 SPILog(SPI_LOG_LEVEL_DEBUG,SPI,AText);
end;

{==============================================================================}
{==============================================================================}

initialization
 SPIInit;

{==============================================================================}
 
finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.