{
Ultibo PCI/PCIe interface unit.

Copyright (C) 2021 - SoftOz Pty Ltd.

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
 
 //To Do
 
References
==========

 //To Do
 
PCI
===
 
 //To Do
 
PCI Device
==========

 //To Do

PCI Driver
==========

 //To Do

PCI Host
========

 //To Do
 
}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit PCI;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,HeapManager,Devices,Unicode,SysUtils;
 
{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}
        
{==============================================================================}
const
 {PCI Device, Driver and Host specific constants}
 PCI_DEVICE_PREFIX = 'PCI';    {Name prefix for PCI Devices}
 PCI_DRIVER_PREFIX = 'PCI';    {Name prefix for PCI Drivers}
 PCI_HOST_PREFIX = 'PCIHost';  {Name prefix for PCI Hosts}
 
 {PCI Device Types}
 PCI_TYPE_NONE       = 0;
 
 PCI_TYPE_MAX        = 0;
 
 {PCI Device Type Names}
 PCI_TYPE_NAMES:array[PCI_TYPE_NONE..PCI_TYPE_MAX] of String = (
  'PCI_TYPE_NONE');
 
 {PCI Device States}
 PCI_STATE_DETACHED  = 0;
 PCI_STATE_DETACHING = 1;
 PCI_STATE_ATTACHING = 2;
 PCI_STATE_ATTACHED  = 3;

 PCI_STATE_MAX       = 3;
 
 {PCI Device State Names}
 PCI_STATE_NAMES:array[PCI_STATE_DETACHED..PCI_STATE_MAX] of String = (
  'PCI_STATE_DETACHED',
  'PCI_STATE_DETACHING',
  'PCI_STATE_ATTACHING',
  'PCI_STATE_ATTACHED');
 
 {PCI Device Status}
 PCI_STATUS_UNBOUND   = 0; 
 PCI_STATUS_BOUND     = 1;
 
 PCI_STATUS_MAX       = 1;
 
 {PCI Device Status Names}
 PCI_STATUS_NAMES:array[PCI_STATUS_UNBOUND..PCI_STATUS_MAX] of String = (
  'PCI_STATUS_UNBOUND',
  'PCI_STATUS_BOUND');
 
 {PCI Device Flags}
 PCI_FLAG_NONE       = $00000000;
 
 {PCI Host Types}
 PCIHOST_TYPE_NONE   = 0;
 PCIHOST_TYPE_PCI    = 1;
 PCIHOST_TYPE_PCIX   = 2;
 PCIHOST_TYPE_PCIE   = 3;
 
 PCIHOST_TYPE_MAX    = 3;
 
 {PCI Host Type Names}
 PCIHOST_TYPE_NAMES:array[PCIHOST_TYPE_NONE..PCIHOST_TYPE_MAX] of String = (
  'PCIHOST_TYPE_NONE',
  'PCIHOST_TYPE_PCI',
  'PCIHOST_TYPE_PCIX',
  'PCIHOST_TYPE_PCIE');
 
 {PCI Host States}
 PCIHOST_STATE_DISABLED = 0;
 PCIHOST_STATE_ENABLED  = 1;
 
 PCIHOST_STATE_MAX      = 1;
 
 {PCI Host State Names}
 PCIHOST_STATE_NAMES:array[PCIHOST_STATE_DISABLED..PCIHOST_STATE_MAX] of String = (
  'PCIHOST_STATE_DISABLED',
  'PCIHOST_STATE_ENABLED');
 
 {PCI Host Flags}
 PCIHOST_FLAG_NONE      = $00000000;
 PCIHOST_FLAG_SHARED    = $00000001;
 PCIHOST_FLAG_NOCACHE   = $00000002;
 
 {PCI Status Codes}
 PCI_STATUS_SUCCESS                   = 0;  {Function successful}
 
 //To Do 
 
 {PCI logging}
 PCI_LOG_LEVEL_DEBUG     = LOG_LEVEL_DEBUG;  {PCI debugging messages}
 PCI_LOG_LEVEL_INFO      = LOG_LEVEL_INFO;   {PCI informational messages, such as a device being attached or detached}
 PCI_LOG_LEVEL_WARN      = LOG_LEVEL_WARN;   {PCI warning messages}
 PCI_LOG_LEVEL_ERROR     = LOG_LEVEL_ERROR;  {PCI error messages}
 PCI_LOG_LEVEL_NONE      = LOG_LEVEL_NONE;   {No PCI messages}

var 
 PCI_DEFAULT_LOG_LEVEL:LongWord = PCI_LOG_LEVEL_DEBUG; {Minimum level for PCI messages.  Only messages with level greater than or equal to this will be printed}
 
var 
 {PCI logging}
 PCI_LOG_ENABLED:Boolean; 

{==============================================================================}
type
 {PCI Device, Driver and Host specific types}
 {PCI Device}
 PPCIHost = ^TPCIHost;                   {Forward declared to satisfy PCIDevice}
 PPCIDriver = ^TPCIDriver;               {Forward declared to satisfy PCIDevice}
 PPCIDevice = ^TPCIDevice;
 
 {PCI Device Bind Callback}
 TPCIDeviceBind = function(Device:PPCIDevice):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 {PCI Device Unbind Callback}
 TPCIDeviceUnbind = function(Device:PPCIDevice;Driver:PPCIDriver):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 {PCI Device Enumeration Callback}
 TPCIDeviceEnumerate = function(Device:PPCIDevice;Data:Pointer):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 {PCI Device Notification Callback}
 TPCIDeviceNotification = function(Device:PDevice;Data:Pointer;Notification:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}

 {PCI Device Methods}
  {None}
 
 TPCIDevice = record 
  {Device Properties}
  Device:TDevice;                            {The Device entry for this PCI device}
  {PCI Properties}                           
  PCIId:LongWord;                            {Unique Id of this PCI in the PCI device table}
  PCIState:LongWord;                         {PCI device state (eg PCI_STATE_ATTACHED)}
  PCIStatus:LongWord;                        {PCI device status (eg PCI_STATUS_BOUND)}
  Host:PPCIHost;                             {Host controller this PCI device is connected to (Set by PCI core)}
  Parent:PPCIDevice;                         {Parent this PCI device is connected to, if any (Set by PCI core)}
  Driver:PPCIDriver;                         {Driver this PCI device is bound to, if any (Set by PCI core)} 
  {Driver Properties}                        
  Lock:TMutexHandle;                         {PCI device lock}
  //To Do 
  {Internal Properties}                                                                               
  Prev:PPCIDevice;                           {Previous entry in PCI device table}
  Next:PPCIDevice;                           {Next entry in PCI device table}
 end;
 
 {PCI Driver}
 {PPCIDriver = ^TPCIDriver;} {Declared above for PCIDevice}
 
 {PCI Driver Enumeration Callback}
 TPCIDriverEnumerate = function(Driver:PPCIDriver;Data:Pointer):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 
 {PCI Driver Methods}
 TPCIDriverBind = function(Device:PPCIDevice):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TPCIDriverUnbind = function(Device:PPCIDevice):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 
 TPCIDriver = record
  {Driver Properties}
  Driver:TDriver;                 {The Driver entry for this PCI Driver}
  {PCI Properties}
  DriverBind:TPCIDriverBind;      {A Driver specific DriverBind method implementing the standard PCI driver interface}
  DriverUnbind:TPCIDriverUnbind;  {A Driver specific DriverUnbind method implementing the standard PCI driver interface}
  {Interface Properties}
  Lock:TMutexHandle;              {Driver lock}
  {Internal Properties}                                                                        
  Prev:PPCIDriver;                {Previous entry in Driver table}
  Next:PPCIDriver;                {Next entry in Driver table}
 end;
 
 {PCI Host}
 {PPCIHost = ^TPCIHost;} {Declared above for PCIDevice}
 {PCI Host Enumeration Callback}
 TPCIHostEnumerate = function(Host:PPCIHost;Data:Pointer):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 {PCI Host Notification Callback}
 TPCIHostNotification = function(Device:PDevice;Data:Pointer;Notification:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 
 {PCI Host Methods}
 TPCIHostStart = function(Host:PPCIHost):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TPCIHostStop = function(Host:PPCIHost):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TPCIHostReset = function(Host:PPCIHost):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 //To Do
 
 TPCIHost = record
  {Device Properties}
  Device:TDevice;              {The Device entry for this PCI Host}
  {PCI Properties}
  HostId:LongWord;             {Unique Id of this Host in the Host table}
  HostState:LongWord;          {Host state (eg PCIHOST_STATE_ENABLED)}
  HostStart:TPCIHostStart;     {A Host specific HostStart method implementing the standard PCI host interface}
  HostStop:TPCIHostStop;       {A Host specific HostStop method implementing the standard PCI host interface}
  HostReset:TPCIHostReset;     {A Host specific HostReset method implementing the standard PCI host interface}
  //To Do
  {Driver Properties}
  Lock:TMutexHandle;           {Host lock}
  //To Do
  {Internal Properties}                                                                        
  Prev:PPCIHost;               {Previous entry in Host table}
  Next:PPCIHost;               {Next entry in Host table}
 end;
 
{==============================================================================}
type
 {PCI Logging specific types}
 TPCILogOutput = procedure(const AText:String;Data:Pointer);
 
{==============================================================================}
{var}
 {PCI Device, Driver and Host specific variables}
 
{==============================================================================}
{Initialization Functions}
procedure PCIInit;
function PCIStart:LongWord;
function PCIStop:LongWord;

procedure PCIAsyncStart(Host:PPCIHost);

{==============================================================================}
{PCI Device, Driver and Host Functions}
//Device Methods
//To Do
 
function PCIDeviceAllocate(Host:PPCIHost):PPCIDevice;
function PCIDeviceRelease(Device:PPCIDevice):LongWord;

function PCIDeviceFind(PCIId:LongWord):PPCIDevice;
function PCIDeviceFindById(VendorId,ProductId:Word):PPCIDevice;
function PCIDeviceFindByName(const Name:String):PPCIDevice; inline;
function PCIDeviceFindByDescription(const Description:String):PPCIDevice; inline;
function PCIDeviceEnumerate(Callback:TPCIDeviceEnumerate;Data:Pointer):LongWord;

function PCIDeviceNotification(Device:PPCIDevice;Callback:TPCIDeviceNotification;Data:Pointer;Notification,Flags:LongWord):LongWord;
 
//Driver Methods
function PCIDriverCreate:PPCIDriver;
function PCIDriverCreateEx(Size:LongWord):PPCIDriver;
function PCIDriverDestroy(Driver:PPCIDriver):LongWord;

function PCIDriverRegister(Driver:PPCIDriver):LongWord;
function PCIDriverDeregister(Driver:PPCIDriver):LongWord;

function PCIDriverFind(DriverId:LongWord):PPCIDriver;
function PCIDriverFindByName(const Name:String):PPCIDriver; inline;
function PCIDriverEnumerate(Callback:TPCIDriverEnumerate;Data:Pointer):LongWord;

//Host Methods
//To Do

function PCIHostCreate:PPCIHost;
function PCIHostCreateEx(Size:LongWord):PPCIHost;
function PCIHostDestroy(Host:PPCIHost):LongWord;

function PCIHostRegister(Host:PPCIHost):LongWord;
function PCIHostDeregister(Host:PPCIHost):LongWord;

function PCIHostFind(HostId:LongWord):PPCIHost;
function PCIHostFindByName(const Name:String):PPCIHost; inline;
function PCIHostFindByDescription(const Description:String):PPCIHost; inline;
function PCIHostEnumerate(Callback:TPCIHostEnumerate;Data:Pointer):LongWord;

function PCIHostNotification(Host:PPCIHost;Callback:TPCIHostNotification;Data:Pointer;Notification,Flags:LongWord):LongWord;

{==============================================================================}
{PCI Device, Driver and Host Helper Functions}
function PCIDeviceGetCount:LongWord; inline;

function PCIDeviceCheck(Device:PPCIDevice):PPCIDevice;

function PCIDriverGetCount:LongWord; inline;

function PCIDriverCheck(Driver:PPCIDriver):PPCIDriver;

function PCIHostGetCount:LongWord; inline;

function PCIHostCheck(Host:PPCIHost):PPCIHost;

function PCIStatusToString(Status:LongWord):String;

function PCIDeviceTypeToString(PCIType:LongWord):String;
function PCIDeviceStateToString(PCIState:LongWord):String;
function PCIDeviceStatusToString(PCIStatus:LongWord):String;

function PCIDeviceStateToNotification(State:LongWord):LongWord;
function PCIDeviceStatusToNotification(Status:LongWord):LongWord;

function PCIHostTypeToString(HostType:LongWord):String;
function PCIHostStateToString(HostState:LongWord):String;

function PCIHostStateToNotification(State:LongWord):LongWord;

procedure PCILog(Level:LongWord;Device:PPCIDevice;const AText:String);
procedure PCILogInfo(Device:PPCIDevice;const AText:String); inline;
procedure PCILogWarn(Device:PPCIDevice;const AText:String); inline;
procedure PCILogError(Device:PPCIDevice;const AText:String); inline;
procedure PCILogDebug(Device:PPCIDevice;const AText:String); inline;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {PCI Device, Driver and Host specific variables}
 PCIInitialized:Boolean;
 PCIStarted:Boolean;

 PCIDeviceTable:PPCIDevice;
 PCIDeviceTableLock:TCriticalSectionHandle = INVALID_HANDLE_VALUE;
 PCIDeviceTableCount:LongWord;
 
 PCIDriverTable:PPCIDriver;
 PCIDriverTableLock:TCriticalSectionHandle = INVALID_HANDLE_VALUE;
 PCIDriverTableCount:LongWord;
 
 PCIHostTable:PPCIHost;
 PCIHostTableLock:TCriticalSectionHandle = INVALID_HANDLE_VALUE;
 PCIHostTableCount:LongWord;

{==============================================================================}
{==============================================================================}
{Forward Declarations}
procedure PCILogOutput(const AText:String;Data:Pointer); forward;

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure PCIInit;
{Performs basic initialization of the PCI core driver, after this devices, hosts
 and drivers can be registered however nothing will work until PCIStart is called}
var
 Status:LongWord;
begin
 {}
 {Check Initialized}
 if PCIInitialized then Exit;
 
 {Initialize Logging}
 PCI_LOG_ENABLED:=(PCI_DEFAULT_LOG_LEVEL <> PCI_LOG_LEVEL_NONE); 
 
 {Initialize PCI Device Table}
 PCIDeviceTable:=nil;
 PCIDeviceTableLock:=CriticalSectionCreate; 
 PCIDeviceTableCount:=0;
 if PCIDeviceTableLock = INVALID_HANDLE_VALUE then
  begin
   if PCI_LOG_ENABLED then PCILogError(nil,'Failed to create PCI device table lock');
  end;

 {Initialize PCI Driver Table}
 PCIDriverTable:=nil;
 PCIDriverTableLock:=CriticalSectionCreate; 
 PCIDriverTableCount:=0;
 if PCIDriverTableLock = INVALID_HANDLE_VALUE then
  begin
   if PCI_LOG_ENABLED then PCILogError(nil,'Failed to create PCI driver table lock');
  end;
  
 {Initialize PCI Host Table}
 PCIHostTable:=nil;
 PCIHostTableLock:=CriticalSectionCreate; 
 PCIHostTableCount:=0;
 if PCIHostTableLock = INVALID_HANDLE_VALUE then
  begin
   if PCI_LOG_ENABLED then PCILogError(nil,'Failed to create PCI host table lock');
  end;
  
 PCIInitialized:=True;
end;

{==============================================================================}

function PCIStart:LongWord;
{Starts all registered PCI hosts and begins the PCI enumeration process}
var
 Host:PPCIHost;
 Status:LongWord;
begin
 {}
 Result:=ERROR_SUCCESS;
 
 {Check Started}
 if PCIStarted then Exit;
 
 Result:=ERROR_INVALID_PARAMETER;

 //To Do
 
 if PCI_LOG_ENABLED then PCILogInfo(nil,'Successfully initialized PCI subsystem'); 
 
 {Return Result} 
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function PCIStop:LongWord;
var
 Host:PPCIHost;
begin
 {}
 Result:=ERROR_SUCCESS;
 
 {Check Started}
 if not(PCIStarted) then Exit;

 Result:=ERROR_INVALID_PARAMETER;

 //To Do

 {Set Started}
 PCIStarted:=False;    
 
 {Return Result} 
 Result:=ERROR_SUCCESS;
end; 
 
{==============================================================================}
 
procedure PCIAsyncStart(Host:PPCIHost);
var
 Status:LongWord;
 RootHub:PPCIDevice;
begin
 {}
 {Wait for Ready}
 while not(SysInitCompleted) do
  begin
   ThreadSleep(0);
  end;
  
 {Check Host}
 if Host = nil then
  begin
   {Start PCI Subsystem}
   PCIStart;
  end
 else
  begin
   {Check Host}
   if PCIHostCheck(Host) <> Host then Exit;
   
   {Acquire the Lock}
   if CriticalSectionLock(PCIHostTableLock) = ERROR_SUCCESS then
    begin
     try
      {Start Host}
      //To Do
 
     finally
      {Release the Lock}
      CriticalSectionUnlock(PCIHostTableLock);
     end;
    end;
  end;  
end;

{==============================================================================}
{==============================================================================}
{PCI Device, Driver and Host Functions}
function PCIDeviceAllocate(Host:PPCIHost):PPCIDevice;
{Create and Register a new Device entry in the Device table}
{Host: The Host this device is attached to}
{Return: Pointer to new Device entry or nil if device could not be created}
var
 PCIId:LongWord;
 Device:PPCIDevice;
begin
 {}
 Result:=nil;
 
 {Check Host}
 if Host = nil then Exit;
 
 {Create PCI Device}
 Device:=PPCIDevice(DeviceCreateEx(SizeOf(TPCIDevice)));
 if Device = nil then Exit;
 
 {Update Device}
 Device.Device.DeviceBus:=DEVICE_BUS_PCI;   
 Device.Device.DeviceType:=PCI_TYPE_NONE;
 Device.Device.DeviceFlags:=PCI_FLAG_NONE;
 Device.Device.DeviceData:=nil;
 
 {Update PCI Device}
 Device.PCIId:=DEVICE_ID_ANY;
 //To Do
 
 {Create Lock}
 Device.Lock:=MutexCreate;
 if Device.Lock = INVALID_HANDLE_VALUE then
  begin
   if PCI_LOG_ENABLED then PCILogError(nil,'Failed to create lock for PCI device');
   
   DeviceDestroy(@Device.Device);
   Exit;
  end;
 
 {Insert PCI Device}
 if CriticalSectionLock(PCIDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Update PCI Device}
    PCIId:=0;
    while PCIDeviceFind(PCIId) <> nil do
     begin
      Inc(PCIId);
     end;
    Device.PCIId:=PCIId;
 
    {Update Device}
    Device.Device.DeviceName:=PCI_DEVICE_PREFIX + IntToStr(Device.PCIId);
    Device.Device.DeviceClass:=DEVICE_CLASS_PCI;
 
    {Register Device}
    if DeviceRegister(@Device.Device) <> ERROR_SUCCESS then
     begin
      Device.PCIId:=DEVICE_ID_ANY;
      Exit;
     end; 
 
    {Link PCI Device}
    if PCIDeviceTable = nil then
     begin
      PCIDeviceTable:=Device;
     end
    else
     begin
      Device.Next:=PCIDeviceTable;
      PCIDeviceTable.Prev:=Device;
      PCIDeviceTable:=Device;
     end;
 
    {Increment Count}
    Inc(PCIDeviceTableCount);
 
    {Return Result}
    Result:=Device;
   finally
    CriticalSectionUnlock(PCIDeviceTableLock);
   end;
  end
end;

{==============================================================================}

function PCIDeviceRelease(Device:PPCIDevice):LongWord;
{Deregister and Destroy a Device from the Device table}
{Device: The device to deregister and destroy}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Index:LongWord;
 Count:LongWord;
 Prev:PPCIDevice;
 Next:PPCIDevice;
 Alternate:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Device}
 if Device = nil then Exit;
 if Device.PCIId = DEVICE_ID_ANY then Exit;
 if Device.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check PCI Device}
 Result:=ERROR_NOT_FOUND;
 if PCIDeviceCheck(Device) <> Device then Exit;
 
 {Check State}
 if Device.Device.DeviceState <> DEVICE_STATE_REGISTERED then Exit;
 
 {Acquire the Lock}
 Result:=MutexLock(Device.Lock);
 if Result <> ERROR_SUCCESS then Exit;
 
 {Remove PCI Device}
 if CriticalSectionLock(PCIDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Deregister Device}
    Result:=DeviceDeregister(@Device.Device);
    if Result <> ERROR_SUCCESS then Exit;
 
    {Unlink PCI Device}
    Prev:=Device.Prev;
    Next:=Device.Next;
    if Prev = nil then
     begin
      PCIDeviceTable:=Next;
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
    Dec(PCIDeviceTableCount);
 
    {Update PCI Device}
    Device.PCIId:=DEVICE_ID_ANY;
    
    {Release the Lock}
    MutexUnlock(Device.Lock);
    
    {Free the Lock}
    MutexDestroy(Device.Lock);
 
    {Destroy PCI Device} 
    Result:=DeviceDestroy(@Device.Device);
   finally
    CriticalSectionUnlock(PCIDeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;

{==============================================================================}

function PCIDeviceFind(PCIId:LongWord):PPCIDevice;
var
 Device:PPCIDevice;
begin
 {}
 Result:=nil;
 
 {Check Id}
 if PCIId = DEVICE_ID_ANY then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(PCIDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Device}
    Device:=PCIDeviceTable;
    while Device <> nil do
     begin
      {Check State}
      if Device.Device.DeviceState = DEVICE_STATE_REGISTERED then
       begin
        {Check Id}
        if Device.PCIId = PCIId then
         begin
          Result:=Device;
          Exit;
         end;
       end;
       
      {Get Next}
      Device:=Device.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(PCIDeviceTableLock);
   end;
  end;
end;

{==============================================================================}

function PCIDeviceFindById(VendorId,ProductId:Word):PPCIDevice;
var
 Device:PPCIDevice;
begin
 {}
 Result:=nil;

 {Acquire the Lock}
 if CriticalSectionLock(PCIDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Device}
    Device:=PCIDeviceTable;
    while Device <> nil do
     begin
      {Check State}
      if Device.Device.DeviceState = DEVICE_STATE_REGISTERED then
       begin
        {Check Vendor and Product Id}
        //To Do
       end;
       
      {Get Next}
      Device:=Device.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(PCIDeviceTableLock);
   end;
  end;
end;

{==============================================================================}

function PCIDeviceFindByName(const Name:String):PPCIDevice; inline;
begin
 {}
 Result:=PPCIDevice(DeviceFindByNameEx(DEVICE_CLASS_PCI,Name));
end;

{==============================================================================}

function PCIDeviceFindByDescription(const Description:String):PPCIDevice; inline;
begin
 {}
 Result:=PPCIDevice(DeviceFindByDescriptionEx(DEVICE_CLASS_PCI,Description));
end;

{==============================================================================}

function PCIDeviceEnumerate(Callback:TPCIDeviceEnumerate;Data:Pointer):LongWord;
var
 Device:PPCIDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Callback}
 if not Assigned(Callback) then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(PCIDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Device}
    Device:=PCIDeviceTable;
    while Device <> nil do
     begin
      {Check State}
      if Device.Device.DeviceState = DEVICE_STATE_REGISTERED then
       begin
        if Callback(Device,Data) <> ERROR_SUCCESS then Exit;
       end;
       
      {Get Next}
      Device:=Device.Next;
     end;
     
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    CriticalSectionUnlock(PCIDeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;

{==============================================================================}

function PCIDeviceNotification(Device:PPCIDevice;Callback:TPCIDeviceNotification;Data:Pointer;Notification,Flags:LongWord):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Device}
 if Device = nil then
  begin
   Result:=DeviceNotification(nil,DEVICE_CLASS_PCI,Callback,Data,Notification,Flags);
  end
 else
  begin 
   {Check Device}
   if Device.Device.Signature <> DEVICE_SIGNATURE then Exit;

   Result:=DeviceNotification(@Device.Device,DEVICE_CLASS_PCI,Callback,Data,Notification,Flags);
  end; 
end;

{==============================================================================}

function PCIDriverCreate:PPCIDriver;
{Create a new PCI Driver entry}
{Return: Pointer to new Driver entry or nil if driver could not be created}
begin
 {}
 Result:=PCIDriverCreateEx(SizeOf(TPCIDriver));
end;

{==============================================================================}

function PCIDriverCreateEx(Size:LongWord):PPCIDriver;
{Create a new PCI Driver entry}
{Size: Size in bytes to allocate for new driver (Including the driver entry)}
{Return: Pointer to new Driver entry or nil if driver could not be created}
begin
 {}
 Result:=nil;
 
 {Check Size}
 if Size < SizeOf(TPCIDriver) then Exit;
 
 {Create Driver}
 Result:=PPCIDriver(DriverCreateEx(Size));
 if Result = nil then Exit;
 
 {Update Driver}
 Result.DriverBind:=nil;
 Result.DriverUnbind:=nil;
 Result.Lock:=INVALID_HANDLE_VALUE;
 
 {Create Lock}
 Result.Lock:=MutexCreate;
 if Result.Lock = INVALID_HANDLE_VALUE then
  begin
   if PCI_LOG_ENABLED then PCILogError(nil,'Failed to create lock for PCI driver');
   PCIDriverDestroy(Result);
   Result:=nil;
   Exit;
  end;
end;

{==============================================================================}

function PCIDriverDestroy(Driver:PPCIDriver):LongWord;
{Destroy an existing PCI Driver entry}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Driver}
 if Driver = nil then Exit;
 if Driver.Driver.Signature <> DRIVER_SIGNATURE then Exit;
 
 {Check Driver}
 Result:=ERROR_IN_USE;
 if PCIDriverCheck(Driver) = Driver then Exit;

 {Check State}
 if Driver.Driver.DriverState <> DRIVER_STATE_UNREGISTERED then Exit;
 
 {Destroy Lock}
 if Driver.Lock <> INVALID_HANDLE_VALUE then
  begin
   MutexDestroy(Driver.Lock);
  end;
 
 {Destroy Driver} 
 Result:=DriverDestroy(@Driver.Driver);
end;

{==============================================================================}

function PCIDriverRegister(Driver:PPCIDriver):LongWord;
{Register a new Driver in the PCI Driver table}
var
 Host:PPCIHost;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Driver}
 if Driver = nil then Exit;
 if Driver.Driver.DriverId <> DRIVER_ID_ANY then Exit;
 if Driver.Driver.Signature <> DRIVER_SIGNATURE then Exit;
 
 {Check Bind}
 if not(Assigned(Driver.DriverBind)) then
  begin
   if PCI_LOG_ENABLED then PCILogError(nil,'Cannot register driver, Bind function must be implemented');
   Exit;
  end;
 
 {Check Unbind}
 if not(Assigned(Driver.DriverUnbind)) then
  begin
   if PCI_LOG_ENABLED then PCILogError(nil,'Cannot register driver, Unbind function must be implemented');
   Exit;
  end;
  
 {Check Driver}
 Result:=ERROR_ALREADY_EXISTS;
 if PCIDriverCheck(Driver) = Driver then
  begin
   if PCI_LOG_ENABLED then PCILogError(nil,'Cannot register driver, already registered');
   Exit;
  end; 
 
 {Check State}
 if Driver.Driver.DriverState <> DRIVER_STATE_UNREGISTERED then Exit;
 
 {Insert Driver}
 if CriticalSectionLock(PCIDriverTableLock) = ERROR_SUCCESS then
  begin
   try
    {Update Driver}
    Driver.Driver.DriverClass:=DRIVER_CLASS_PCI;
    
    {Register Driver}
    Result:=DriverRegister(@Driver.Driver);
    if Result <> ERROR_SUCCESS then Exit;
    
    {Link Driver}
    if PCIDriverTable = nil then
     begin
      PCIDriverTable:=Driver;
     end
    else
     begin
      Driver.Next:=PCIDriverTable;
      PCIDriverTable.Prev:=Driver;
      PCIDriverTable:=Driver;
     end;
 
    {Increment Count}
    Inc(PCIDriverTableCount);
    
    if PCI_LOG_ENABLED then PCILogInfo(nil,'Registered ' + DriverGetName(@Driver.Driver) + ' (Id=' + IntToStr(Driver.Driver.DriverId) + ')');
    
    {Acquire the Lock}
    if CriticalSectionLock(PCIHostTableLock) = ERROR_SUCCESS then
     begin
      try
       {Get Host}
       Host:=PCIHostTable;
       while Host <> nil do
        begin
         
         //To Do
         
         {Get Next}
         Host:=Host.Next;
        end;
        
       {Return Result}
       Result:=ERROR_SUCCESS;
      finally
       {Release the Lock}
       CriticalSectionUnlock(PCIHostTableLock);
      end;
     end
    else
     begin
      Result:=ERROR_CAN_NOT_COMPLETE;
     end;     
   finally
    CriticalSectionUnlock(PCIDriverTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;

{==============================================================================}

function PCIDriverDeregister(Driver:PPCIDriver):LongWord;
{Deregister a Driver from the PCI Driver table}
var
 Host:PPCIHost;
 Prev:PPCIDriver;
 Next:PPCIDriver;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Driver}
 if Driver = nil then Exit;
 if Driver.Driver.DriverId = DRIVER_ID_ANY then Exit;
 if Driver.Driver.Signature <> DRIVER_SIGNATURE then Exit;
 
 {Check Driver}
 Result:=ERROR_NOT_FOUND;
 if PCIDriverCheck(Driver) <> Driver then Exit;
 
 {Check State}
 if Driver.Driver.DriverState <> DRIVER_STATE_REGISTERED then Exit;
 
 {Remove Driver}
 if CriticalSectionLock(PCIDriverTableLock) = ERROR_SUCCESS then
  begin
   try
    {Acquire the Lock}
    if CriticalSectionLock(PCIHostTableLock) = ERROR_SUCCESS then
     begin
      try
       {Get Host}
       Host:=PCIHostTable;
       while Host <> nil do
        begin
        
         //To Do
    
         {Get Next}
         Host:=Host.Next;
        end;
      finally
       {Release the Lock}
       CriticalSectionUnlock(PCIHostTableLock);
      end;
     end
    else
     begin
      Result:=ERROR_CAN_NOT_COMPLETE;
      Exit;
     end;     
    
    {Deregister Driver}
    Result:=DriverDeregister(@Driver.Driver);
    if Result <> ERROR_SUCCESS then Exit;
    
    {Unlink Driver}
    Prev:=Driver.Prev;
    Next:=Driver.Next;
    if Prev = nil then
     begin
      PCIDriverTable:=Next;
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
    Dec(PCIDriverTableCount);
 
    if PCI_LOG_ENABLED then PCILogInfo(nil,'Deregistered ' + DriverGetName(@Driver.Driver));
    
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    CriticalSectionUnlock(PCIDriverTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;

{==============================================================================}

function PCIDriverFind(DriverId:LongWord):PPCIDriver;
{Find a driver by Id in the PCI Driver table}
var
 Driver:PPCIDriver;
begin
 {}
 Result:=nil;
 
 {Check Id}
 if DriverId = DRIVER_ID_ANY then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(PCIDriverTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Driver}
    Driver:=PCIDriverTable;
    while Driver <> nil do
     begin
      {Check State}
      if Driver.Driver.DriverState = DRIVER_STATE_REGISTERED then
       begin
        {Check Id}
        if Driver.Driver.DriverId = DriverId then
         begin
          Result:=Driver;
          Exit;
         end;
       end;
       
      {Get Next}
      Driver:=Driver.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(PCIDriverTableLock);
   end;
  end;
end;

{==============================================================================}

function PCIDriverFindByName(const Name:String):PPCIDriver; inline;
{Find a driver by name in the Driver table}
begin
 {}
 Result:=PPCIDriver(DriverFindByName(Name));
end;

{==============================================================================}

function PCIDriverEnumerate(Callback:TPCIDriverEnumerate;Data:Pointer):LongWord;
{Enumerate all drivers in the PCI Driver table}
var
 Driver:PPCIDriver;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Callback}
 if not Assigned(Callback) then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(PCIDriverTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Driver}
    Driver:=PCIDriverTable;
    while Driver <> nil do
     begin
      {Check State}
      if Driver.Driver.DriverState = DRIVER_STATE_REGISTERED then
       begin
        if Callback(Driver,Data) <> ERROR_SUCCESS then Exit;
       end;
       
      {Get Next}
      Driver:=Driver.Next;
     end;
     
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    CriticalSectionUnlock(PCIDriverTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;

{==============================================================================}

function PCIHostCreate:PPCIHost;
{Create a new Host entry}
{Return: Pointer to new Host entry or nil if host could not be created}
begin
 {}
 Result:=PCIHostCreateEx(SizeOf(TPCIHost));
end;

{==============================================================================}

function PCIHostCreateEx(Size:LongWord):PPCIHost;
{Create a new Host entry}
{Size: Size in bytes to allocate for new host (Including the host entry)}
{Return: Pointer to new Host entry or nil if host could not be created}
begin
 {}
 Result:=nil;
 
 {Check Size}
 if Size < SizeOf(TPCIHost) then Exit;
 
 {Create Host}
 Result:=PPCIHost(DeviceCreateEx(Size));
 if Result = nil then Exit;
 
 {Update Device}
 Result.Device.DeviceBus:=DEVICE_BUS_NONE;   
 Result.Device.DeviceType:=PCIHOST_TYPE_NONE;
 Result.Device.DeviceFlags:=PCIHOST_FLAG_NONE;
 Result.Device.DeviceData:=nil;

 {Update Host}
 Result.HostId:=DEVICE_ID_ANY;
 Result.HostState:=PCIHOST_STATE_DISABLED;
 Result.HostStart:=nil;
 Result.HostStop:=nil;
 Result.HostReset:=nil;
 //To Do
 Result.Lock:=INVALID_HANDLE_VALUE;
 //To Do
 
 {Create Lock}
 Result.Lock:=MutexCreate;
 if Result.Lock = INVALID_HANDLE_VALUE then
  begin
   if PCI_LOG_ENABLED then PCILogError(nil,'Failed to create lock for PCI host');
   PCIHostDestroy(Result);
   Result:=nil;
   Exit;
  end;
end;

{==============================================================================}

function PCIHostDestroy(Host:PPCIHost):LongWord;
{Destroy an existing Host entry}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Host}
 if Host = nil then Exit;
 if Host.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check Host}
 Result:=ERROR_IN_USE;
 if PCIHostCheck(Host) = Host then Exit;

 {Check State}
 if Host.Device.DeviceState <> DEVICE_STATE_UNREGISTERED then Exit;
  
 {Destroy Lock}
 if Host.Lock <> INVALID_HANDLE_VALUE then
  begin
   MutexDestroy(Host.Lock);
  end;
 
 {Destroy Host} 
 Result:=DeviceDestroy(@Host.Device);
end;

{==============================================================================}

function PCIHostRegister(Host:PPCIHost):LongWord;
{Register a new Host in the Host table}
var
 HostId:LongWord;
 Status:LongWord;
 RootHub:PPCIDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Host}
 if Host = nil then Exit;
 if Host.HostId <> DEVICE_ID_ANY then Exit;
 if Host.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check Start}
 if not(Assigned(Host.HostStart)) then
  begin
   if PCI_LOG_ENABLED then PCILogError(nil,'Cannot register host, Start function must be implemented');
   Exit;
  end;

 {Check Stop}
 if not(Assigned(Host.HostStop)) then
  begin
   if PCI_LOG_ENABLED then PCILogError(nil,'Cannot register host, Stop function must be implemented');
   Exit;
  end;

 {Check Submit}
 //To Do
 //if not(Assigned(Host.HostSubmit)) then
 // begin
 //  if PCI_LOG_ENABLED then PCILogError(nil,'Cannot register host, Submit function must be implemented');
 //  Exit;
 // end;
  
 {Check Cancel}
 //To Do
 //if not(Assigned(Host.HostCancel)) then
 // begin
 //  if PCI_LOG_ENABLED then PCILogError(nil,'Cannot register host, Cancel function must be implemented');
 //  Exit;
 // end;
 
 {Check Host}
 Result:=ERROR_ALREADY_EXISTS;
 if PCIHostCheck(Host) = Host then
  begin
   if PCI_LOG_ENABLED then PCILogError(nil,'Cannot register host, already registered');
   Exit;
  end; 
 
 {Check State}
 if Host.Device.DeviceState <> DEVICE_STATE_UNREGISTERED then Exit;
 
 {Insert Host}
 if CriticalSectionLock(PCIHostTableLock) = ERROR_SUCCESS then
  begin
   try
    {Update Host}
    HostId:=0;
    while PCIHostFind(HostId) <> nil do
     begin
      Inc(HostId);
     end;
    Host.HostId:=HostId;
    
    {Update Device}
    Host.Device.DeviceName:=PCI_HOST_PREFIX + IntToStr(Host.HostId);
    Host.Device.DeviceClass:=DEVICE_CLASS_PCIHOST;
    
    {Register Device}
    Result:=DeviceRegister(@Host.Device);
    if Result <> ERROR_SUCCESS then
     begin
      Host.HostId:=DEVICE_ID_ANY;
      Exit;
     end; 
    
    {Link Host}
    if PCIHostTable = nil then
     begin
      PCIHostTable:=Host;
     end
    else
     begin
      Host.Next:=PCIHostTable;
      PCIHostTable.Prev:=Host;
      PCIHostTable:=Host;
     end;
 
    {Increment Count}
    Inc(PCIHostTableCount);
  
    {Check Started}
    if PCIStarted then
     begin
      if not PCI_ASYNCSTART then
       begin
        {Start Host}
        Status:=Host.HostStart(Host);
        if Status = PCI_STATUS_SUCCESS then
         begin
          if PCI_LOG_ENABLED then PCILogInfo(nil,'Successfully started PCI host ' + DeviceGetName(@Host.Device));
          
          //To Do
          
         end
        else
         begin
          if PCI_LOG_ENABLED then PCILogError(nil,'Failed to start PCI host ' + DeviceGetName(@Host.Device) + ' (Status=' + PCIStatusToString(Status) + ')');
         end;
       
        {Return Result}
        Result:=ERROR_SUCCESS;
       end
      else
       begin
        {Schedule Worker}
        Result:=WorkerSchedule(0,TWorkerTask(PCIAsyncStart),Host,nil)
       end;
     end
    else
     begin
      {Return Result}
      Result:=ERROR_SUCCESS;
     end; 
   finally
    CriticalSectionUnlock(PCIHostTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;

{==============================================================================}

function PCIHostDeregister(Host:PPCIHost):LongWord;
{Deregister a Host from the Host table}
var
 Prev:PPCIHost;
 Next:PPCIHost;
 Status:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Host}
 if Host = nil then Exit;
 if Host.HostId = DEVICE_ID_ANY then Exit;
 if Host.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check Host}
 Result:=ERROR_NOT_FOUND;
 if PCIHostCheck(Host) <> Host then Exit;
 
 {Check State}
 if Host.Device.DeviceState <> DEVICE_STATE_REGISTERED then Exit;
 
 {Remove Host}
 if CriticalSectionLock(PCIHostTableLock) = ERROR_SUCCESS then
  begin
   try
    {Check Started}
    if PCIStarted then
     begin
      
      //To Do
    
      {Stop Host}
      Status:=Host.HostStop(Host);
      if Status <> PCI_STATUS_SUCCESS then
       begin
        if PCI_LOG_ENABLED then PCILogError(nil,'Failed to stop PCI host ' + DeviceGetName(@Host.Device) + ' (Status=' + PCIStatusToString(Status) + ')');
        
        Result:=ERROR_OPERATION_FAILED;
        Exit;
       end;
     end; 
    
    {Deregister Device}
    Result:=DeviceDeregister(@Host.Device);
    if Result <> ERROR_SUCCESS then Exit;
    
    {Unlink Host}
    Prev:=Host.Prev;
    Next:=Host.Next;
    if Prev = nil then
     begin
      PCIHostTable:=Next;
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
    Dec(PCIHostTableCount);
 
    {Update Host}
    Host.HostId:=DEVICE_ID_ANY;
 
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    CriticalSectionUnlock(PCIHostTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;

{==============================================================================}

function PCIHostFind(HostId:LongWord):PPCIHost;
var
 Host:PPCIHost;
begin
 {}
 Result:=nil;
 
 {Check Id}
 if HostId = DEVICE_ID_ANY then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(PCIHostTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Host}
    Host:=PCIHostTable;
    while Host <> nil do
     begin
      {Check State}
      if Host.Device.DeviceState = DEVICE_STATE_REGISTERED then
       begin
        {Check Id}
        if Host.HostId = HostId then
         begin
          Result:=Host;
          Exit;
         end;
       end;
       
      {Get Next}
      Host:=Host.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(PCIHostTableLock);
   end;
  end;
end;

{==============================================================================}

function PCIHostFindByName(const Name:String):PPCIHost; inline;
begin
 {}
 Result:=PPCIHost(DeviceFindByNameEx(DEVICE_CLASS_PCIHOST,Name));
end;

{==============================================================================}

function PCIHostFindByDescription(const Description:String):PPCIHost; inline;
begin
 {}
 Result:=PPCIHost(DeviceFindByDescriptionEx(DEVICE_CLASS_PCIHOST,Description));
end;

{==============================================================================}

function PCIHostEnumerate(Callback:TPCIHostEnumerate;Data:Pointer):LongWord;
var
 Host:PPCIHost;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Callback}
 if not Assigned(Callback) then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(PCIHostTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Host}
    Host:=PCIHostTable;
    while Host <> nil do
     begin
      {Check State}
      if Host.Device.DeviceState = DEVICE_STATE_REGISTERED then
       begin
        if Callback(Host,Data) <> ERROR_SUCCESS then Exit;
       end;
       
      {Get Next}
      Host:=Host.Next;
     end;
     
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    CriticalSectionUnlock(PCIHostTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;

{==============================================================================}

function PCIHostNotification(Host:PPCIHost;Callback:TPCIHostNotification;Data:Pointer;Notification,Flags:LongWord):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Host}
 if Host = nil then
  begin
   Result:=DeviceNotification(nil,DEVICE_CLASS_PCIHOST,Callback,Data,Notification,Flags);
  end
 else
  begin 
   {Check Host}
   if Host.Device.Signature <> DEVICE_SIGNATURE then Exit;

   Result:=DeviceNotification(@Host.Device,DEVICE_CLASS_PCIHOST,Callback,Data,Notification,Flags);
  end; 
end;

{==============================================================================}
{==============================================================================}
{PCI Device, Driver and Host Helper Functions}
function PCIDeviceGetCount:LongWord; inline;
{Get the current device count}
begin
 {}
 Result:=PCIDeviceTableCount;
end;

{==============================================================================}

function PCIDeviceCheck(Device:PPCIDevice):PPCIDevice;
{Check if the supplied Device is in the device table}
var
 Current:PPCIDevice;
begin
 {}
 Result:=nil;
 
 {Check Device}
 if Device = nil then Exit;
 if Device.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(PCIDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Device}
    Current:=PCIDeviceTable;
    while Current <> nil do
     begin
      {Check Device}
      if Current = Device then
       begin
        Result:=Device;
        Exit;
       end;
      
      {Get Next}
      Current:=Current.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(PCIDeviceTableLock);
   end;
  end;
end;

{==============================================================================}

function PCIDriverGetCount:LongWord; inline;
{Get the current PCI driver count}
begin
 {}
 Result:=PCIDriverTableCount;
end;

{==============================================================================}

function PCIDriverCheck(Driver:PPCIDriver):PPCIDriver;
{Check if the supplied PCI Driver is in the driver table}
var
 Current:PPCIDriver;
begin
 {}
 Result:=nil;
 
 {Check Driver}
 if Driver = nil then Exit;
 if Driver.Driver.Signature <> DRIVER_SIGNATURE then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(PCIDriverTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Driver}
    Current:=PCIDriverTable;
    while Current <> nil do
     begin
      {Check Driver}
      if Current = Driver then
       begin
        Result:=Driver;
        Exit;
       end;
      
      {Get Next}
      Current:=Current.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(PCIDriverTableLock);
   end;
  end;
end;

{==============================================================================}

function PCIHostGetCount:LongWord; inline;
{Get the current host count}
begin
 {}
 Result:=PCIHostTableCount;
end;

{==============================================================================}

function PCIHostCheck(Host:PPCIHost):PPCIHost;
{Check if the supplied Host is in the host table}
var
 Current:PPCIHost;
begin
 {}
 Result:=nil;
 
 {Check Host}
 if Host = nil then Exit;
 if Host.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(PCIHostTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Host}
    Current:=PCIHostTable;
    while Current <> nil do
     begin
      {Check Host}
      if Current = Host then
       begin
        Result:=Host;
        Exit;
       end;
      
      {Get Next}
      Current:=Current.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(PCIHostTableLock);
   end;
  end;
end;

{==============================================================================}

function PCIStatusToString(Status:LongWord):String;
{Translates a PCI status code into a string}
begin
 {}
 Result:='PCI_STATUS_UNKNOWN';
 
 case Status of
  PCI_STATUS_SUCCESS:Result:='PCI_STATUS_SUCCESS';
  //To Do
 end;
end;

{==============================================================================}

function PCIDeviceTypeToString(PCIType:LongWord):String;
begin
 {}
 Result:='PCI_TYPE_UNKNOWN';
 
 if PCIType <= PCI_TYPE_MAX then
  begin
   Result:=PCI_TYPE_NAMES[PCIType];
  end;
end;

{==============================================================================}

function PCIDeviceStateToString(PCIState:LongWord):String;
begin
 {}
 Result:='PCI_STATE_UNKNOWN';
 
 if PCIState <= PCI_STATE_MAX then
  begin
   Result:=PCI_STATE_NAMES[PCIState];
  end;
end;

{==============================================================================}

function PCIDeviceStatusToString(PCIStatus:LongWord):String;
begin
 {}
 Result:='PCI_STATUS_UNKNOWN';
 
 if PCIStatus <= PCI_STATUS_MAX then
  begin
   Result:=PCI_STATUS_NAMES[PCIStatus];
  end;
end;

{==============================================================================}

function PCIDeviceStateToNotification(State:LongWord):LongWord;
{Convert a Device state value into the notification code for device notifications}
begin
 {}
 Result:=DEVICE_NOTIFICATION_NONE;
 
 {Check State}
 case State of
  PCI_STATE_DETACHED:Result:=DEVICE_NOTIFICATION_DETACH;
  PCI_STATE_DETACHING:Result:=DEVICE_NOTIFICATION_DETACHING;
  PCI_STATE_ATTACHING:Result:=DEVICE_NOTIFICATION_ATTACHING;
  PCI_STATE_ATTACHED:Result:=DEVICE_NOTIFICATION_ATTACH;
 end;
end;

{==============================================================================}

function PCIDeviceStatusToNotification(Status:LongWord):LongWord;
{Convert a Device status value into the notification code for device notifications}
begin
 {}
 Result:=DEVICE_NOTIFICATION_NONE;
 
 {Check Status}
 case Status of
  PCI_STATUS_UNBOUND:Result:=DEVICE_NOTIFICATION_UNBIND;
  PCI_STATUS_BOUND:Result:=DEVICE_NOTIFICATION_BIND;
 end;
end;

{==============================================================================}

function PCIHostTypeToString(HostType:LongWord):String;
begin
 {}
 Result:='PCIHOST_TYPE_UNKNOWN';
 
 if HostType <= PCIHOST_TYPE_MAX then
  begin
   Result:=PCIHOST_TYPE_NAMES[HostType];
  end;
end;

{==============================================================================}

function PCIHostStateToString(HostState:LongWord):String;
begin
 {}
 Result:='PCIHOST_STATE_UNKNOWN';
 
 if HostState <= PCIHOST_STATE_MAX then
  begin
   Result:=PCIHOST_STATE_NAMES[HostState];
  end;
end;

{==============================================================================}

function PCIHostStateToNotification(State:LongWord):LongWord;
{Convert a Host state value into the notification code for device notifications}
begin
 {}
 Result:=DEVICE_NOTIFICATION_NONE;
 
 {Check State}
 case State of
  PCIHOST_STATE_DISABLED:Result:=DEVICE_NOTIFICATION_DISABLE;
  PCIHOST_STATE_ENABLED:Result:=DEVICE_NOTIFICATION_ENABLE;
 end;
end;

{==============================================================================}

procedure PCILog(Level:LongWord;Device:PPCIDevice;const AText:String);
var
 WorkBuffer:String;
begin
 {}
 {Check Level}
 if Level < PCI_DEFAULT_LOG_LEVEL then Exit;
 
 WorkBuffer:='';
 {Check Level}
 if Level = PCI_LOG_LEVEL_DEBUG then
  begin
   WorkBuffer:=WorkBuffer + '[DEBUG] ';
  end
 else if Level = PCI_LOG_LEVEL_WARN then
  begin
   WorkBuffer:=WorkBuffer + '[WARN] ';
  end
 else if Level = PCI_LOG_LEVEL_ERROR then
  begin
   WorkBuffer:=WorkBuffer + '[ERROR] ';
  end;
 
 {Add Prefix}
 WorkBuffer:=WorkBuffer + 'PCI: ';
 
 {Check Device}
 if Device <> nil then
  begin
   //To Do
   //WorkBuffer:=WorkBuffer + 'Device' + IntToStr(Device.Address) + ': ';
  end;

 {Output Logging}  
 LoggingOutputEx(LOGGING_FACILITY_PCI,LogLevelToLoggingSeverity(Level),'PCI',WorkBuffer + AText);
end;

{==============================================================================}

procedure PCILogInfo(Device:PPCIDevice;const AText:String); inline;
begin
 {}
 PCILog(PCI_LOG_LEVEL_INFO,Device,AText);
end;

{==============================================================================}

procedure PCILogWarn(Device:PPCIDevice;const AText:String); inline;
begin
 {}
 PCILog(PCI_LOG_LEVEL_WARN,Device,AText);
end;

{==============================================================================}

procedure PCILogError(Device:PPCIDevice;const AText:String); inline;
begin
 {}
 PCILog(PCI_LOG_LEVEL_ERROR,Device,AText);
end;

{==============================================================================}

procedure PCILogDebug(Device:PPCIDevice;const AText:String); inline;
begin
 {}
 PCILog(PCI_LOG_LEVEL_DEBUG,Device,AText);
end;

{==============================================================================}

procedure PCILogOutput(const AText:String;Data:Pointer); 
{Default log output procedure for PCILogDevices etc}
begin
  {}
  LoggingOutput(AText);
end;

{==============================================================================}
{==============================================================================}

initialization
 PCIInit;
 if PCI_AUTOSTART then
  begin
   if not PCI_ASYNCSTART then
    begin
     {Start PCI}
     PCIStart;
    end
   else
    begin
     {Schedule Worker}
     WorkerSchedule(0,TWorkerTask(PCIAsyncStart),nil,nil);
    end;
  end; 

{==============================================================================}
 
finalization
 PCIStop;
 
{==============================================================================}
{==============================================================================}

end.
