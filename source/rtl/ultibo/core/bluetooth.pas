{
Ultibo Bluetooth interface unit.

Copyright (C) 2025 - SoftOz Pty Ltd.

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

Bluetooth Device
================

 //To Do

Bluetooth Driver
================

 //To Do

Bluetooth Host
==============

 //To Do

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

{$IFNDEF FPC_DOTTEDUNITS}
unit Bluetooth;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  Core.GlobalConfig,
  Core.GlobalConst,
  Core.GlobalTypes,
  Core.Platform,
  Core.Threads,
  Core.Devices,
  Core.USB,
  Core.Keyboard,
  Core.Mouse,
  Core.Storage,
  Core.Network,
  Core.Audio,
  Core.Video,
  Core.Serial,
  System.SysUtils;
{$ELSE FPC_DOTTEDUNITS}
uses
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  Devices,
  USB,
  Keyboard,
  Mouse,
  Storage,
  Network,
  Audio,
  Video,
  Serial,
  SysUtils;
{$ENDIF FPC_DOTTEDUNITS}

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
const
 {Bluetooth Device, Driver and Host specific constants}
 BLUETOOTH_DEVICE_PREFIX = 'Bluetooth';    {Name prefix for Bluetooth Devices}
 BLUETOOTH_DRIVER_PREFIX = 'Bluetooth';    {Name prefix for Bluetooth Drivers}
 BLUETOOTH_HOST_PREFIX = 'BluetoothHost';  {Name prefix for Bluetooth Hosts}

 BLUETOOTH_DRIVER_NAME = 'Bluetooth USB HCI Driver'; {Name of Bluetooth USB driver}

 {Bluetooth Device Types}
 BLUETOOTH_TYPE_NONE       = 0;

 BLUETOOTH_TYPE_MAX        = 0;

 {Bluetooth Device Type Names}
 BLUETOOTH_TYPE_NAMES:array[BLUETOOTH_TYPE_NONE..BLUETOOTH_TYPE_MAX] of String = (
  'BLUETOOTH_TYPE_NONE');

 {Bluetooth Device States}
 BLUETOOTH_STATE_DETACHED  = 0;
 BLUETOOTH_STATE_DETACHING = 1;
 BLUETOOTH_STATE_ATTACHING = 2;
 BLUETOOTH_STATE_ATTACHED  = 3;

 BLUETOOTH_STATE_MAX       = 3;

 {Bluetooth Device State Names}
 BLUETOOTH_STATE_NAMES:array[BLUETOOTH_STATE_DETACHED..BLUETOOTH_STATE_MAX] of String = (
  'BLUETOOTH_STATE_DETACHED',
  'BLUETOOTH_STATE_DETACHING',
  'BLUETOOTH_STATE_ATTACHING',
  'BLUETOOTH_STATE_ATTACHED');

 {Bluetooth Device Status}
 BLUETOOTH_STATUS_UNBOUND   = 0;
 BLUETOOTH_STATUS_BOUND     = 1;

 BLUETOOTH_STATUS_MAX       = 1;

 {Bluetooth Device Status Names}
 BLUETOOTH_STATUS_NAMES:array[BLUETOOTH_STATUS_UNBOUND..BLUETOOTH_STATUS_MAX] of String = (
  'BLUETOOTH_STATUS_UNBOUND',
  'BLUETOOTH_STATUS_BOUND');

 {Bluetooth Device Flags}
 BLUETOOTH_FLAG_NONE       = $00000000;

 {Bluetooth Host Types}
 BLUETOOTHHOST_TYPE_NONE  = 0;
 BLUETOOTHHOST_TYPE_UART  = 1; {Bluetooth HCI over RS232 with RTS/CTS flow control for reliable delivery}
 BLUETOOTHHOST_TYPE_USB   = 2; {Bluetooth HCI over USB}
 BLUETOOTHHOST_TYPE_SDIO  = 3; {Bluetooth HCI over SDIO}
 BLUETOOTHHOST_TYPE_3WIRE = 4; {Bluetooth HCI over 3 wire UART (TX/RX/GND only with RTS/CTS optional)}

 BLUETOOTHHOST_TYPE_MAX   = 4;

 {Bluetooth Host Type Names}
 BLUETOOTHHOST_TYPE_NAMES:array[BLUETOOTHHOST_TYPE_NONE..BLUETOOTHHOST_TYPE_MAX] of String = (
  'BLUETOOTHHOST_TYPE_NONE',
  'BLUETOOTHHOST_TYPE_UART',
  'BLUETOOTHHOST_TYPE_USB',
  'BLUETOOTHHOST_TYPE_SDIO',
  'BLUETOOTHHOST_TYPE_3WIRE');

 {Bluetooth Host States}
 BLUETOOTHHOST_STATE_DISABLED = 0;
 BLUETOOTHHOST_STATE_ENABLED  = 1;

 BLUETOOTHHOST_STATE_MAX      = 1;

 {Bluetooth Host State Names}
 BLUETOOTHHOST_STATE_NAMES:array[BLUETOOTHHOST_STATE_DISABLED..BLUETOOTHHOST_STATE_MAX] of String = (
  'BLUETOOTHHOST_STATE_DISABLED',
  'BLUETOOTHHOST_STATE_ENABLED');

 {Bluetooth Host Flags}
 BLUETOOTHHOST_FLAG_NONE      = $00000000;
 BLUETOOTHHOST_FLAG_SHARED    = $00000001;
 BLUETOOTHHOST_FLAG_NOCACHE   = $00000002;

 {Bluetooth Status Codes}
 BLUETOOTH_STATUS_SUCCESS                   = 0;  {Function successful}
 BLUETOOTH_STATUS_DEVICE_DETACHED           = 1;  {Bluetooth device was detached}
 BLUETOOTH_STATUS_DEVICE_UNSUPPORTED        = 2;  {Bluetooth device is unsupported by the driver}
 BLUETOOTH_STATUS_HARDWARE_ERROR            = 3;  {Hardware error of some form occurred}
 BLUETOOTH_STATUS_INVALID_DATA              = 4;  {Invalid data was received}
 BLUETOOTH_STATUS_INVALID_PARAMETER         = 5;  {An invalid parameter was passed to the function}

 {Bluetooth logging}
 BLUETOOTH_LOG_LEVEL_DEBUG     = LOG_LEVEL_DEBUG;  {Bluetooth debugging messages}
 BLUETOOTH_LOG_LEVEL_INFO      = LOG_LEVEL_INFO;   {Bluetooth informational messages, such as a device being attached or detached}
 BLUETOOTH_LOG_LEVEL_WARN      = LOG_LEVEL_WARN;   {Bluetooth warning messages}
 BLUETOOTH_LOG_LEVEL_ERROR     = LOG_LEVEL_ERROR;  {Bluetooth error messages}
 BLUETOOTH_LOG_LEVEL_NONE      = LOG_LEVEL_NONE;   {No Bluetooth messages}

var
 BLUETOOTH_DEFAULT_LOG_LEVEL:LongWord = BLUETOOTH_LOG_LEVEL_DEBUG; {Minimum level for Bluetooth messages.  Only messages with level greater than or equal to this will be printed}

var
 {Bluetooth logging}
 BLUETOOTH_LOG_ENABLED:Boolean;

{==============================================================================}
type
 {Bluetooth Device, Driver and Host specific types}
 {Bluetooth Device}
 PBluetoothHost = ^TBluetoothHost;           {Forward declared to satisfy BluetoothDevice}
 PBluetoothDriver = ^TBluetoothDriver;       {Forward declared to satisfy BluetoothDevice}
 PBluetoothDevice = ^TBluetoothDevice;

 {Bluetooth Device Bind Callback}
 TBluetoothDeviceBind = function(Device:PBluetoothDevice):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 {Bluetooth Device Unbind Callback}
 TBluetoothDeviceUnbind = function(Device:PBluetoothDevice;Driver:PBluetoothDriver):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 {Bluetooth Device Enumeration Callback}
 TBluetoothDeviceEnumerate = function(Device:PBluetoothDevice;Data:Pointer):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 {Bluetooth Device Notification Callback}
 TBluetoothDeviceNotification = function(Device:PDevice;Data:Pointer;Notification:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}

 {Bluetooth Device Methods}
  {None}

 TBluetoothDevice = record
  {Device Properties}
  Device:TDevice;                            {The Device entry for this Bluetooth device}
  {Bluetooth Properties}
  BluetoothId:LongWord;                      {Unique Id of this Bluetooth in the Bluetooth device table}
  BluetoothState:LongWord;                   {Bluetooth device state (eg BLUETOOTH_STATE_ATTACHED)}
  BluetoothStatus:LongWord;                  {Bluetooth device status (eg BLUETOOTH_STATUS_BOUND)}
  Host:PBluetoothHost;                       {Host controller this Bluetooth device is connected to (Set by Bluetooth core)}
  Driver:PBluetoothDriver;                   {Driver this Bluetooth device is bound to, if any (Set by Bluetooth core)}
  {Driver Properties}
  Lock:TMutexHandle;                         {Bluetooth device lock}
  //To Do
  {Internal Properties}
  Prev:PBluetoothDevice;                     {Previous entry in Bluetooth device table}
  Next:PBluetoothDevice;                     {Next entry in Bluetooth device table}
 end;

 {Bluetooth Driver}
 {PBluetoothDriver = ^TBluetoothDriver;} {Declared above for BluetoothDevice}

 {Bluetooth Driver Enumeration Callback}
 TBluetoothDriverEnumerate = function(Driver:PBluetoothDriver;Data:Pointer):LongWord;{$IFDEF i386} stdcall;{$ENDIF}

 {Bluetooth Driver Methods}
 TBluetoothDriverBind = function(Device:PBluetoothDevice):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TBluetoothDriverUnbind = function(Device:PBluetoothDevice):LongWord;{$IFDEF i386} stdcall;{$ENDIF}

 TBluetoothDriver = record
  {Driver Properties}
  Driver:TDriver;                      {The Driver entry for this Bluetooth Driver}
  {Bluetooth Properties}
  DriverBind:TBluetoothDriverBind;     {A Driver specific DriverBind method implementing the standard Bluetooth driver interface}
  DriverUnbind:TBluetoothDriverUnbind; {A Driver specific DriverUnbind method implementing the standard Bluetooth driver interface}
  {Interface Properties}
  Lock:TMutexHandle;                   {Driver lock}
  {Internal Properties}
  Prev:PBluetoothDriver;               {Previous entry in Driver table}
  Next:PBluetoothDriver;               {Next entry in Driver table}
 end;

 {Bluetooth Host}
 {PBluetoothHost = ^TBluetoothHost;} {Declared above for BluetoothDevice}
 {Bluetooth Host Enumeration Callback}
 TBluetoothHostEnumerate = function(Host:PBluetoothHost;Data:Pointer):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 {Bluetooth Host Notification Callback}
 TBluetoothHostNotification = function(Device:PDevice;Data:Pointer;Notification:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}

 {Bluetooth Host Methods}
 TBluetoothHostStart = function(Host:PBluetoothHost):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TBluetoothHostStop = function(Host:PBluetoothHost):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TBluetoothHostReset = function(Host:PBluetoothHost):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 //To Do

 TBluetoothHost = record
  {Device Properties}
  Device:TDevice;                {The Device entry for this Bluetooth Host}
  {Bluetooth Properties}
  HostId:LongWord;               {Unique Id of this Host in the Host table}
  HostState:LongWord;            {Host state (eg BLUETOOTHHOST_STATE_ENABLED)}
  HostStart:TBluetoothHostStart; {A Host specific HostStart method implementing the standard Bluetooth host interface}
  HostStop:TBluetoothHostStop;   {A Host specific HostStop method implementing the standard Bluetooth host interface}
  HostReset:TBluetoothHostReset; {A Host specific HostReset method implementing the standard Bluetooth host interface}
  //To Do
  {Driver Properties}
  Lock:TMutexHandle;             {Host lock}
  //To Do
  {Internal Properties}
  Prev:PBluetoothHost;           {Previous entry in Host table}
  Next:PBluetoothHost;           {Next entry in Host table}
 end;

{==============================================================================}
type
 {Bluetooth Logging specific types}
 TBluetoothLogOutput = procedure(const AText:String;Data:Pointer);

{==============================================================================}
{var}
 {Bluetooth Device, Driver and Host specific variables}

{==============================================================================}
{Initialization Functions}
procedure BluetoothInit;
function BluetoothStart:LongWord;
function BluetoothStop:LongWord;

procedure BluetoothAsyncStart(Host:PBluetoothHost);

{==============================================================================}
{Bluetooth Device, Driver and Host Functions}
//Device Methods
//To Do

function BluetoothDeviceAllocate(Host:PBluetoothHost):PBluetoothDevice;
function BluetoothDeviceRelease(Device:PBluetoothDevice):LongWord;

function BluetoothDeviceFind(BluetoothId:LongWord):PBluetoothDevice;
function BluetoothDeviceFindByName(const Name:String):PBluetoothDevice; inline;
function BluetoothDeviceFindByDescription(const Description:String):PBluetoothDevice; inline;
function BluetoothDeviceEnumerate(Callback:TBluetoothDeviceEnumerate;Data:Pointer):LongWord;

function BluetoothDeviceNotification(Device:PBluetoothDevice;Callback:TBluetoothDeviceNotification;Data:Pointer;Notification,Flags:LongWord):LongWord;

//Driver Methods
function BluetoothDriverCreate:PBluetoothDriver;
function BluetoothDriverCreateEx(Size:LongWord):PBluetoothDriver;
function BluetoothDriverDestroy(Driver:PBluetoothDriver):LongWord;

function BluetoothDriverRegister(Driver:PBluetoothDriver):LongWord;
function BluetoothDriverDeregister(Driver:PBluetoothDriver):LongWord;

function BluetoothDriverFind(DriverId:LongWord):PBluetoothDriver;
function BluetoothDriverFindByName(const Name:String):PBluetoothDriver; inline;
function BluetoothDriverEnumerate(Callback:TBluetoothDriverEnumerate;Data:Pointer):LongWord;

//Host Methods
//To Do

function BluetoothHostCreate:PBluetoothHost;
function BluetoothHostCreateEx(Size:LongWord):PBluetoothHost;
function BluetoothHostDestroy(Host:PBluetoothHost):LongWord;

function BluetoothHostRegister(Host:PBluetoothHost):LongWord;
function BluetoothHostDeregister(Host:PBluetoothHost):LongWord;

function BluetoothHostFind(HostId:LongWord):PBluetoothHost;
function BluetoothHostFindByName(const Name:String):PBluetoothHost; inline;
function BluetoothHostFindByDescription(const Description:String):PBluetoothHost; inline;
function BluetoothHostEnumerate(Callback:TBluetoothHostEnumerate;Data:Pointer):LongWord;

function BluetoothHostNotification(Host:PBluetoothHost;Callback:TBluetoothHostNotification;Data:Pointer;Notification,Flags:LongWord):LongWord;

{==============================================================================}
{Bluetooth Device, Driver and Host Helper Functions}
function BluetoothDeviceGetCount:LongWord;

function BluetoothDeviceCheck(Device:PBluetoothDevice):PBluetoothDevice;

function BluetoothDriverGetCount:LongWord;

function BluetoothDriverCheck(Driver:PBluetoothDriver):PBluetoothDriver;

function BluetoothHostGetCount:LongWord;

function BluetoothHostCheck(Host:PBluetoothHost):PBluetoothHost;

function BluetoothStatusToString(Status:LongWord):String;

function BluetoothDeviceTypeToString(BluetoothType:LongWord):String;
function BluetoothDeviceStateToString(BluetoothState:LongWord):String;
function BluetoothDeviceStatusToString(BluetoothStatus:LongWord):String;

function BluetoothDeviceStateToNotification(State:LongWord):LongWord;
function BluetoothDeviceStatusToNotification(Status:LongWord):LongWord;

function BluetoothHostTypeToString(HostType:LongWord):String;
function BluetoothHostStateToString(HostState:LongWord):String;

function BluetoothHostStateToNotification(State:LongWord):LongWord;

procedure BluetoothLog(Level:LongWord;Device:PBluetoothDevice;const AText:String);
procedure BluetoothLogInfo(Device:PBluetoothDevice;const AText:String); inline;
procedure BluetoothLogWarn(Device:PBluetoothDevice;const AText:String); inline;
procedure BluetoothLogError(Device:PBluetoothDevice;const AText:String); inline;
procedure BluetoothLogDebug(Device:PBluetoothDevice;const AText:String); inline;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {Bluetooth Device, Driver and Host specific variables}
 BluetoothInitialized:Boolean;
 BluetoothStarted:Boolean;

 BluetoothUSBDriver:PUSBDriver;  {Bluetooth USB Driver interface (Set by BluetoothInit)}

 BluetoothDeviceTable:PBluetoothDevice;
 BluetoothDeviceTableLock:TCriticalSectionHandle = INVALID_HANDLE_VALUE;
 BluetoothDeviceTableCount:LongWord;

 BluetoothDriverTable:PBluetoothDriver;
 BluetoothDriverTableLock:TCriticalSectionHandle = INVALID_HANDLE_VALUE;
 BluetoothDriverTableCount:LongWord;

 BluetoothHostTable:PBluetoothHost;
 BluetoothHostTableLock:TCriticalSectionHandle = INVALID_HANDLE_VALUE;
 BluetoothHostTableCount:LongWord;

{==============================================================================}
{==============================================================================}
{Forward Declarations}
procedure BluetoothLogOutput(const AText:String;Data:Pointer); forward;

{Bluetooth USB Functions}
function BluetoothUSBDriverBind(Device:PUSBDevice;Interrface:PUSBInterface):LongWord; forward;
function BluetoothUSBDriverUnbind(Device:PUSBDevice;Interrface:PUSBInterface):LongWord; forward;

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure BluetoothInit;
{Performs basic initialization of the Bluetooth core driver, after this devices, hosts
 and drivers can be registered however nothing will work until BluetoothStart is called}

{Note: Called only during system startup}
var
 Status:LongWord;
begin
 {}
 {Check Initialized}
 if BluetoothInitialized then Exit;

 {Initialize Logging}
 BLUETOOTH_LOG_ENABLED:=(BLUETOOTH_DEFAULT_LOG_LEVEL <> BLUETOOTH_LOG_LEVEL_NONE);

 {Initialize Bluetooth Device Table}
 BluetoothDeviceTable:=nil;
 BluetoothDeviceTableLock:=CriticalSectionCreate;
 BluetoothDeviceTableCount:=0;
 if BluetoothDeviceTableLock = INVALID_HANDLE_VALUE then
  begin
   if BLUETOOTH_LOG_ENABLED then BluetoothLogError(nil,'Failed to create Bluetooth device table lock');
  end;

 {Initialize Bluetooth Driver Table}
 BluetoothDriverTable:=nil;
 BluetoothDriverTableLock:=CriticalSectionCreate;
 BluetoothDriverTableCount:=0;
 if BluetoothDriverTableLock = INVALID_HANDLE_VALUE then
  begin
   if BLUETOOTH_LOG_ENABLED then BluetoothLogError(nil,'Failed to create Bluetooth driver table lock');
  end;

 {Initialize Bluetooth Host Table}
 BluetoothHostTable:=nil;
 BluetoothHostTableLock:=CriticalSectionCreate;
 BluetoothHostTableCount:=0;
 if BluetoothHostTableLock = INVALID_HANDLE_VALUE then
  begin
   if BLUETOOTH_LOG_ENABLED then BluetoothLogError(nil,'Failed to create Bluetooth host table lock');
  end;

 {Create Bluetooth USB Driver}
 BluetoothUSBDriver:=USBDriverCreate;
 if BluetoothUSBDriver <> nil then
  begin
   {Update Bluetooth USB Driver}
   {Driver}
   BluetoothUSBDriver.Driver.DriverName:=BLUETOOTH_DRIVER_NAME;
   {USB}
   BluetoothUSBDriver.DriverBind:=BluetoothUSBDriverBind;
   BluetoothUSBDriver.DriverUnbind:=BluetoothUSBDriverUnbind;

   {Register Bluetooth USB Driver}
   Status:=USBDriverRegister(BluetoothUSBDriver);
   if Status <> USB_STATUS_SUCCESS then
    begin
     if USB_LOG_ENABLED then USBLogError(nil,'Bluetooth: Failed to register Bluetooth USB driver: ' + USBStatusToString(Status));

     {Destroy Driver}
     USBDriverDestroy(BluetoothUSBDriver);
    end;
  end
 else
  begin
   if USB_LOG_ENABLED then USBLogError(nil,'Bluetooth: Failed to create Bluetooth USB driver');
  end;

 BluetoothInitialized:=True;
end;

{==============================================================================}

function BluetoothStart:LongWord;
{Starts all registered Bluetooth hosts and begins the Bluetooth discovery process}
var
 Status:LongWord;
 Host:PBluetoothHost;
begin
 {}
 Result:=ERROR_SUCCESS;

 {Check Started}
 if BluetoothStarted then Exit;

 Result:=ERROR_INVALID_PARAMETER;

 //To Do

 if BLUETOOTH_LOG_ENABLED then BluetoothLogInfo(nil,'Successfully initialized Bluetooth subsystem');

 {Return Result}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function BluetoothStop:LongWord;
var
 Host:PBluetoothHost;
begin
 {}
 Result:=ERROR_SUCCESS;

 {Check Started}
 if not(BluetoothStarted) then Exit;

 Result:=ERROR_INVALID_PARAMETER;

 //To Do

 {Set Started}
 BluetoothStarted:=False;

 {Return Result}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

procedure BluetoothAsyncStart(Host:PBluetoothHost);
var
 Status:LongWord;
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
   {Start Bluetooth Subsystem}
   BluetoothStart;
  end
 else
  begin
   {Check Host}
   if BluetoothHostCheck(Host) <> Host then Exit;

   {Acquire the Lock}
   if CriticalSectionLock(BluetoothHostTableLock) = ERROR_SUCCESS then
    begin
     try
      {Start Host}
      //To Do

     finally
      {Release the Lock}
      CriticalSectionUnlock(BluetoothHostTableLock);
     end;
    end;
  end;
end;

{==============================================================================}
{==============================================================================}
{Bluetooth Device, Driver and Host Functions}
function BluetoothDeviceAllocate(Host:PBluetoothHost):PBluetoothDevice;
{Create and Register a new Device entry in the Device table}
{Host: The Host this device is attached to}
{Parent: The Parent device this device is attached to (nil if this device has no parent)}
{Return: Pointer to new Device entry or nil if device could not be created}
var
 BluetoothId:LongWord;
 Device:PBluetoothDevice;
begin
 {}
 Result:=nil;

 {Check Host}
 if Host = nil then Exit;

 {Create Bluetooth Device}
 Device:=PBluetoothDevice(DeviceCreateEx(SizeOf(TBluetoothDevice)));
 if Device = nil then Exit;

 {Update Device}
 Device.Device.DeviceBus:=DEVICE_BUS_BLUETOOTH;
 Device.Device.DeviceType:=BLUETOOTH_TYPE_NONE;
 Device.Device.DeviceFlags:=BLUETOOTH_FLAG_NONE;
 Device.Device.DeviceData:=nil;

 {Update Bluetooth Device}
 Device.BluetoothId:=DEVICE_ID_ANY;
 Device.BluetoothState:=BLUETOOTH_STATE_DETACHED;
 Device.BluetoothStatus:=BLUETOOTH_STATUS_UNBOUND;
 Device.Host:=Host;
 Device.Lock:=INVALID_HANDLE_VALUE;
 //To Do

 {Create Lock}
 Device.Lock:=MutexCreate;
 if Device.Lock = INVALID_HANDLE_VALUE then
  begin
   if BLUETOOTH_LOG_ENABLED then BluetoothLogError(nil,'Failed to create lock for Bluetooth device');

   DeviceDestroy(@Device.Device);
   Exit;
  end;

 {Insert Bluetooth Device}
 if CriticalSectionLock(BluetoothDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Update Bluetooth Device}
    BluetoothId:=0;
    while BluetoothDeviceFind(BluetoothId) <> nil do
     begin
      Inc(BluetoothId);
     end;
    Device.BluetoothId:=BluetoothId;

    {Update Device}
    Device.Device.DeviceName:=BLUETOOTH_DEVICE_PREFIX + IntToStr(Device.BluetoothId);
    Device.Device.DeviceClass:=DEVICE_CLASS_BLUETOOTH;

    {Register Device}
    if DeviceRegister(@Device.Device) <> ERROR_SUCCESS then
     begin
      Device.BluetoothId:=DEVICE_ID_ANY;
      Exit;
     end;

    {Link Bluetooth Device}
    if BluetoothDeviceTable = nil then
     begin
      BluetoothDeviceTable:=Device;
     end
    else
     begin
      Device.Next:=BluetoothDeviceTable;
      BluetoothDeviceTable.Prev:=Device;
      BluetoothDeviceTable:=Device;
     end;

    {Increment Count}
    Inc(BluetoothDeviceTableCount);

    {Return Result}
    Result:=Device;
   finally
    CriticalSectionUnlock(BluetoothDeviceTableLock);
   end;
  end
end;

{==============================================================================}

function BluetoothDeviceRelease(Device:PBluetoothDevice):LongWord;
{Deregister and Destroy a Device from the Device table}
{Device: The device to deregister and destroy}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Prev:PBluetoothDevice;
 Next:PBluetoothDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;
 if Device.BluetoothId = DEVICE_ID_ANY then Exit;
 if Device.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Bluetooth Device}
 Result:=ERROR_NOT_FOUND;
 if BluetoothDeviceCheck(Device) <> Device then Exit;

 {Check State}
 if Device.Device.DeviceState <> DEVICE_STATE_REGISTERED then Exit;

 {Acquire the Lock}
 Result:=MutexLock(Device.Lock);
 if Result <> ERROR_SUCCESS then Exit;

 {Remove Bluetooth Device}
 if CriticalSectionLock(BluetoothDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Deregister Device}
    Result:=DeviceDeregister(@Device.Device);
    if Result <> ERROR_SUCCESS then Exit;

    {Unlink Bluetooth Device}
    Prev:=Device.Prev;
    Next:=Device.Next;
    if Prev = nil then
     begin
      BluetoothDeviceTable:=Next;
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
    Dec(BluetoothDeviceTableCount);

    {Update Bluetooth Device}
    Device.BluetoothId:=DEVICE_ID_ANY;

    {Release the Lock}
    MutexUnlock(Device.Lock);

    {Free the Lock}
    MutexDestroy(Device.Lock);

    {Destroy Bluetooth Device}
    Result:=DeviceDestroy(@Device.Device);
   finally
    CriticalSectionUnlock(BluetoothDeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function BluetoothDeviceFind(BluetoothId:LongWord):PBluetoothDevice;
var
 Device:PBluetoothDevice;
begin
 {}
 Result:=nil;

 {Check Id}
 if BluetoothId = DEVICE_ID_ANY then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(BluetoothDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Device}
    Device:=BluetoothDeviceTable;
    while Device <> nil do
     begin
      {Check State}
      if Device.Device.DeviceState = DEVICE_STATE_REGISTERED then
       begin
        {Check Id}
        if Device.BluetoothId = BluetoothId then
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
    CriticalSectionUnlock(BluetoothDeviceTableLock);
   end;
  end;
end;

{==============================================================================}

function BluetoothDeviceFindByName(const Name:String):PBluetoothDevice; inline;
begin
 {}
 Result:=PBluetoothDevice(DeviceFindByNameEx(DEVICE_CLASS_BLUETOOTH,Name));
end;

{==============================================================================}

function BluetoothDeviceFindByDescription(const Description:String):PBluetoothDevice; inline;
begin
 {}
 Result:=PBluetoothDevice(DeviceFindByDescriptionEx(DEVICE_CLASS_BLUETOOTH,Description));
end;

{==============================================================================}

function BluetoothDeviceEnumerate(Callback:TBluetoothDeviceEnumerate;Data:Pointer):LongWord;
var
 Device:PBluetoothDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Callback}
 if not Assigned(Callback) then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(BluetoothDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Device}
    Device:=BluetoothDeviceTable;
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
    CriticalSectionUnlock(BluetoothDeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function BluetoothDeviceNotification(Device:PBluetoothDevice;Callback:TBluetoothDeviceNotification;Data:Pointer;Notification,Flags:LongWord):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then
  begin
   Result:=DeviceNotification(nil,DEVICE_CLASS_BLUETOOTH,Callback,Data,Notification,Flags);
  end
 else
  begin
   {Check Device}
   if Device.Device.Signature <> DEVICE_SIGNATURE then Exit;

   Result:=DeviceNotification(@Device.Device,DEVICE_CLASS_BLUETOOTH,Callback,Data,Notification,Flags);
  end;
end;

{==============================================================================}

function BluetoothDriverCreate:PBluetoothDriver;
{Create a new Bluetooth Driver entry}
{Return: Pointer to new Driver entry or nil if driver could not be created}
begin
 {}
 Result:=BluetoothDriverCreateEx(SizeOf(TBluetoothDriver));
end;

{==============================================================================}

function BluetoothDriverCreateEx(Size:LongWord):PBluetoothDriver;
{Create a new Bluetooth Driver entry}
{Size: Size in bytes to allocate for new driver (Including the driver entry)}
{Return: Pointer to new Driver entry or nil if driver could not be created}
begin
 {}
 Result:=nil;

 {Check Size}
 if Size < SizeOf(TBluetoothDriver) then Exit;

 {Create Driver}
 Result:=PBluetoothDriver(DriverCreateEx(Size));
 if Result = nil then Exit;

 {Update Driver}
 Result.DriverBind:=nil;
 Result.DriverUnbind:=nil;
 Result.Lock:=INVALID_HANDLE_VALUE;

 {Create Lock}
 Result.Lock:=MutexCreate;
 if Result.Lock = INVALID_HANDLE_VALUE then
  begin
   if BLUETOOTH_LOG_ENABLED then BluetoothLogError(nil,'Failed to create lock for Bluetooth driver');
   BluetoothDriverDestroy(Result);
   Result:=nil;
   Exit;
  end;
end;

{==============================================================================}

function BluetoothDriverDestroy(Driver:PBluetoothDriver):LongWord;
{Destroy an existing Bluetooth Driver entry}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Driver}
 if Driver = nil then Exit;
 if Driver.Driver.Signature <> DRIVER_SIGNATURE then Exit;

 {Check Driver}
 Result:=ERROR_IN_USE;
 if BluetoothDriverCheck(Driver) = Driver then Exit;

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

function BluetoothDriverRegister(Driver:PBluetoothDriver):LongWord;
{Register a new Driver in the Bluetooth Driver table}
var
 Host:PBluetoothHost;
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
   if BLUETOOTH_LOG_ENABLED then BluetoothLogError(nil,'Cannot register driver, Bind function must be implemented');
   Exit;
  end;

 {Check Unbind}
 if not(Assigned(Driver.DriverUnbind)) then
  begin
   if BLUETOOTH_LOG_ENABLED then BluetoothLogError(nil,'Cannot register driver, Unbind function must be implemented');
   Exit;
  end;

 {Check Driver}
 Result:=ERROR_ALREADY_EXISTS;
 if BluetoothDriverCheck(Driver) = Driver then
  begin
   if BLUETOOTH_LOG_ENABLED then BluetoothLogError(nil,'Cannot register driver, already registered');
   Exit;
  end;

 {Check State}
 if Driver.Driver.DriverState <> DRIVER_STATE_UNREGISTERED then Exit;

 {Insert Driver}
 if CriticalSectionLock(BluetoothDriverTableLock) = ERROR_SUCCESS then
  begin
   try
    {Update Driver}
    Driver.Driver.DriverClass:=DRIVER_CLASS_BLUETOOTH;

    {Register Driver}
    Result:=DriverRegister(@Driver.Driver);
    if Result <> ERROR_SUCCESS then Exit;

    {Link Driver}
    if BluetoothDriverTable = nil then
     begin
      BluetoothDriverTable:=Driver;
     end
    else
     begin
      Driver.Next:=BluetoothDriverTable;
      BluetoothDriverTable.Prev:=Driver;
      BluetoothDriverTable:=Driver;
     end;

    {Increment Count}
    Inc(BluetoothDriverTableCount);

    if BLUETOOTH_LOG_ENABLED then BluetoothLogInfo(nil,'Registered ' + DriverGetName(@Driver.Driver) + ' (Id=' + IntToStr(Driver.Driver.DriverId) + ')');

    {Acquire the Lock}
    if CriticalSectionLock(BluetoothHostTableLock) = ERROR_SUCCESS then
     begin
      try
       {Get Host}
       Host:=BluetoothHostTable;
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
       CriticalSectionUnlock(BluetoothHostTableLock);
      end;
     end
    else
     begin
      Result:=ERROR_CAN_NOT_COMPLETE;
     end;
   finally
    CriticalSectionUnlock(BluetoothDriverTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function BluetoothDriverDeregister(Driver:PBluetoothDriver):LongWord;
{Deregister a Driver from the Bluetooth Driver table}
var
 Host:PBluetoothHost;
 Prev:PBluetoothDriver;
 Next:PBluetoothDriver;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Driver}
 if Driver = nil then Exit;
 if Driver.Driver.DriverId = DRIVER_ID_ANY then Exit;
 if Driver.Driver.Signature <> DRIVER_SIGNATURE then Exit;

 {Check Driver}
 Result:=ERROR_NOT_FOUND;
 if BluetoothDriverCheck(Driver) <> Driver then Exit;

 {Check State}
 if Driver.Driver.DriverState <> DRIVER_STATE_REGISTERED then Exit;

 {Remove Driver}
 if CriticalSectionLock(BluetoothDriverTableLock) = ERROR_SUCCESS then
  begin
   try
    {Acquire the Lock}
    if CriticalSectionLock(BluetoothHostTableLock) = ERROR_SUCCESS then
     begin
      try
       {Get Host}
       Host:=BluetoothHostTable;
       while Host <> nil do
        begin

         //To Do

         {Get Next}
         Host:=Host.Next;
        end;
      finally
       {Release the Lock}
       CriticalSectionUnlock(BluetoothHostTableLock);
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
      BluetoothDriverTable:=Next;
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
    Dec(BluetoothDriverTableCount);

    if BLUETOOTH_LOG_ENABLED then BluetoothLogInfo(nil,'Deregistered ' + DriverGetName(@Driver.Driver));

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    CriticalSectionUnlock(BluetoothDriverTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function BluetoothDriverFind(DriverId:LongWord):PBluetoothDriver;
{Find a driver by Id in the Bluetooth Driver table}
var
 Driver:PBluetoothDriver;
begin
 {}
 Result:=nil;

 {Check Id}
 if DriverId = DRIVER_ID_ANY then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(BluetoothDriverTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Driver}
    Driver:=BluetoothDriverTable;
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
    CriticalSectionUnlock(BluetoothDriverTableLock);
   end;
  end;
end;

{==============================================================================}

function BluetoothDriverFindByName(const Name:String):PBluetoothDriver; inline;
{Find a driver by name in the Driver table}
begin
 {}
 Result:=PBluetoothDriver(DriverFindByName(Name));
end;

{==============================================================================}

function BluetoothDriverEnumerate(Callback:TBluetoothDriverEnumerate;Data:Pointer):LongWord;
{Enumerate all drivers in the Bluetooth Driver table}
var
 Driver:PBluetoothDriver;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Callback}
 if not Assigned(Callback) then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(BluetoothDriverTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Driver}
    Driver:=BluetoothDriverTable;
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
    CriticalSectionUnlock(BluetoothDriverTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function BluetoothHostCreate:PBluetoothHost;
{Create a new Host entry}
{Return: Pointer to new Host entry or nil if host could not be created}
begin
 {}
 Result:=BluetoothHostCreateEx(SizeOf(TBluetoothHost));
end;

{==============================================================================}

function BluetoothHostCreateEx(Size:LongWord):PBluetoothHost;
{Create a new Host entry}
{Size: Size in bytes to allocate for new host (Including the host entry)}
{Return: Pointer to new Host entry or nil if host could not be created}
begin
 {}
 Result:=nil;

 {Check Size}
 if Size < SizeOf(TBluetoothHost) then Exit;

 {Create Host}
 Result:=PBluetoothHost(DeviceCreateEx(Size));
 if Result = nil then Exit;

 {Update Device}
 Result.Device.DeviceBus:=DEVICE_BUS_NONE;
 Result.Device.DeviceType:=BLUETOOTHHOST_TYPE_NONE;
 Result.Device.DeviceFlags:=BLUETOOTHHOST_FLAG_NONE;
 Result.Device.DeviceData:=nil;

 {Update Host}
 Result.HostId:=DEVICE_ID_ANY;
 Result.HostState:=BLUETOOTHHOST_STATE_DISABLED;
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
   if BLUETOOTH_LOG_ENABLED then BluetoothLogError(nil,'Failed to create lock for Bluetooth host');
   BluetoothHostDestroy(Result);
   Result:=nil;
   Exit;
  end;
end;

{==============================================================================}

function BluetoothHostDestroy(Host:PBluetoothHost):LongWord;
{Destroy an existing Host entry}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Host}
 if Host = nil then Exit;
 if Host.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Host}
 Result:=ERROR_IN_USE;
 if BluetoothHostCheck(Host) = Host then Exit;

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

function BluetoothHostRegister(Host:PBluetoothHost):LongWord;
{Register a new Host in the Host table}
var
 HostId:LongWord;
 Status:LongWord;
 RootHub:PBluetoothDevice;
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
   if BLUETOOTH_LOG_ENABLED then BluetoothLogError(nil,'Cannot register host, Start function must be implemented');
   Exit;
  end;

 {Check Stop}
 if not(Assigned(Host.HostStop)) then
  begin
   if BLUETOOTH_LOG_ENABLED then BluetoothLogError(nil,'Cannot register host, Stop function must be implemented');
   Exit;
  end;

 //To Do

 {Check Host}
 Result:=ERROR_ALREADY_EXISTS;
 if BluetoothHostCheck(Host) = Host then
  begin
   if BLUETOOTH_LOG_ENABLED then BluetoothLogError(nil,'Cannot register host, already registered');
   Exit;
  end;

 {Check State}
 if Host.Device.DeviceState <> DEVICE_STATE_UNREGISTERED then Exit;

 {Insert Host}
 if CriticalSectionLock(BluetoothHostTableLock) = ERROR_SUCCESS then
  begin
   try
    {Update Host}
    HostId:=0;
    while BluetoothHostFind(HostId) <> nil do
     begin
      Inc(HostId);
     end;
    Host.HostId:=HostId;

    {Update Device}
    Host.Device.DeviceName:=BLUETOOTH_HOST_PREFIX + IntToStr(Host.HostId);
    Host.Device.DeviceClass:=DEVICE_CLASS_BLUETOOTHHOST;

    {Register Device}
    Result:=DeviceRegister(@Host.Device);
    if Result <> ERROR_SUCCESS then
     begin
      Host.HostId:=DEVICE_ID_ANY;
      Exit;
     end;

    {Link Host}
    if BluetoothHostTable = nil then
     begin
      BluetoothHostTable:=Host;
     end
    else
     begin
      Host.Next:=BluetoothHostTable;
      BluetoothHostTable.Prev:=Host;
      BluetoothHostTable:=Host;
     end;

    {Increment Count}
    Inc(BluetoothHostTableCount);

    {Check Started}
    if BluetoothStarted then
     begin
      if not BLUETOOTH_ASYNCSTART then
       begin
        {Start Host}
        Status:=Host.HostStart(Host);
        if Status = BLUETOOTH_STATUS_SUCCESS then
         begin
          if BLUETOOTH_LOG_ENABLED then BluetoothLogInfo(nil,'Successfully started Bluetooth host ' + DeviceGetName(@Host.Device));

          //To Do

         end
        else
         begin
          if BLUETOOTH_LOG_ENABLED then BluetoothLogError(nil,'Failed to start Bluetooth host ' + DeviceGetName(@Host.Device) + ' (Status=' + BluetoothStatusToString(Status) + ')');
         end;

        {Return Result}
        Result:=ERROR_SUCCESS;
       end
      else
       begin
        {Schedule Worker}
        Result:=WorkerSchedule(0,TWorkerTask(BluetoothAsyncStart),Host,nil)
       end;
     end
    else
     begin
      {Return Result}
      Result:=ERROR_SUCCESS;
     end;
   finally
    CriticalSectionUnlock(BluetoothHostTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function BluetoothHostDeregister(Host:PBluetoothHost):LongWord;
{Deregister a Host from the Host table}
var
 Status:LongWord;
 Prev:PBluetoothHost;
 Next:PBluetoothHost;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Host}
 if Host = nil then Exit;
 if Host.HostId = DEVICE_ID_ANY then Exit;
 if Host.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Host}
 Result:=ERROR_NOT_FOUND;
 if BluetoothHostCheck(Host) <> Host then Exit;

 {Check State}
 if Host.Device.DeviceState <> DEVICE_STATE_REGISTERED then Exit;

 {Remove Host}
 if CriticalSectionLock(BluetoothHostTableLock) = ERROR_SUCCESS then
  begin
   try
    {Check Started}
    if BluetoothStarted then
     begin

      //To Do

      {Stop Host}
      Status:=Host.HostStop(Host);
      if Status <> BLUETOOTH_STATUS_SUCCESS then
       begin
        if BLUETOOTH_LOG_ENABLED then BluetoothLogError(nil,'Failed to stop Bluetooth host ' + DeviceGetName(@Host.Device) + ' (Status=' + BluetoothStatusToString(Status) + ')');

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
      BluetoothHostTable:=Next;
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
    Dec(BluetoothHostTableCount);

    {Update Host}
    Host.HostId:=DEVICE_ID_ANY;

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    CriticalSectionUnlock(BluetoothHostTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function BluetoothHostFind(HostId:LongWord):PBluetoothHost;
var
 Host:PBluetoothHost;
begin
 {}
 Result:=nil;

 {Check Id}
 if HostId = DEVICE_ID_ANY then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(BluetoothHostTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Host}
    Host:=BluetoothHostTable;
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
    CriticalSectionUnlock(BluetoothHostTableLock);
   end;
  end;
end;

{==============================================================================}

function BluetoothHostFindByName(const Name:String):PBluetoothHost; inline;
begin
 {}
 Result:=PBluetoothHost(DeviceFindByNameEx(DEVICE_CLASS_BLUETOOTHHOST,Name));
end;

{==============================================================================}

function BluetoothHostFindByDescription(const Description:String):PBluetoothHost; inline;
begin
 {}
 Result:=PBluetoothHost(DeviceFindByDescriptionEx(DEVICE_CLASS_BLUETOOTHHOST,Description));
end;

{==============================================================================}

function BluetoothHostEnumerate(Callback:TBluetoothHostEnumerate;Data:Pointer):LongWord;
var
 Host:PBluetoothHost;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Callback}
 if not Assigned(Callback) then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(BluetoothHostTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Host}
    Host:=BluetoothHostTable;
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
    CriticalSectionUnlock(BluetoothHostTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function BluetoothHostNotification(Host:PBluetoothHost;Callback:TBluetoothHostNotification;Data:Pointer;Notification,Flags:LongWord):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Host}
 if Host = nil then
  begin
   Result:=DeviceNotification(nil,DEVICE_CLASS_BLUETOOTHHOST,Callback,Data,Notification,Flags);
  end
 else
  begin
   {Check Host}
   if Host.Device.Signature <> DEVICE_SIGNATURE then Exit;

   Result:=DeviceNotification(@Host.Device,DEVICE_CLASS_BLUETOOTHHOST,Callback,Data,Notification,Flags);
  end;
end;

{==============================================================================}
{==============================================================================}
{Bluetooth Device, Driver and Host Helper Functions}
function BluetoothDeviceGetCount:LongWord;
{Get the current device count}
begin
 {}
 Result:=BluetoothDeviceTableCount;
end;

{==============================================================================}

function BluetoothDeviceCheck(Device:PBluetoothDevice):PBluetoothDevice;
{Check if the supplied Device is in the device table}
var
 Current:PBluetoothDevice;
begin
 {}
 Result:=nil;

 {Check Device}
 if Device = nil then Exit;
 if Device.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(BluetoothDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Device}
    Current:=BluetoothDeviceTable;
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
    CriticalSectionUnlock(BluetoothDeviceTableLock);
   end;
  end;
end;

{==============================================================================}

function BluetoothDriverGetCount:LongWord;
{Get the current Bluetooth driver count}
begin
 {}
 Result:=BluetoothDriverTableCount;
end;

{==============================================================================}

function BluetoothDriverCheck(Driver:PBluetoothDriver):PBluetoothDriver;
{Check if the supplied Bluetooth Driver is in the driver table}
var
 Current:PBluetoothDriver;
begin
 {}
 Result:=nil;

 {Check Driver}
 if Driver = nil then Exit;
 if Driver.Driver.Signature <> DRIVER_SIGNATURE then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(BluetoothDriverTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Driver}
    Current:=BluetoothDriverTable;
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
    CriticalSectionUnlock(BluetoothDriverTableLock);
   end;
  end;
end;

{==============================================================================}

function BluetoothHostGetCount:LongWord;
{Get the current host count}
begin
 {}
 Result:=BluetoothHostTableCount;
end;

{==============================================================================}

function BluetoothHostCheck(Host:PBluetoothHost):PBluetoothHost;
{Check if the supplied Host is in the host table}
var
 Current:PBluetoothHost;
begin
 {}
 Result:=nil;

 {Check Host}
 if Host = nil then Exit;
 if Host.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(BluetoothHostTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Host}
    Current:=BluetoothHostTable;
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
    CriticalSectionUnlock(BluetoothHostTableLock);
   end;
  end;
end;

{==============================================================================}

function BluetoothStatusToString(Status:LongWord):String;
{Translates a Bluetooth status code into a string}
begin
 {}
 Result:='BLUETOOTH_STATUS_UNKNOWN';

 case Status of
  BLUETOOTH_STATUS_SUCCESS:Result:='BLUETOOTH_STATUS_SUCCESS';
  //To Do
 end;
end;

{==============================================================================}

function BluetoothDeviceTypeToString(BluetoothType:LongWord):String;
begin
 {}
 Result:='BLUETOOTH_TYPE_UNKNOWN';

 if BluetoothType <= BLUETOOTH_TYPE_MAX then
  begin
   Result:=BLUETOOTH_TYPE_NAMES[BluetoothType];
  end;
end;

{==============================================================================}

function BluetoothDeviceStateToString(BluetoothState:LongWord):String;
begin
 {}
 Result:='BLUETOOTH_STATE_UNKNOWN';

 if BluetoothState <= BLUETOOTH_STATE_MAX then
  begin
   Result:=BLUETOOTH_STATE_NAMES[BluetoothState];
  end;
end;

{==============================================================================}

function BluetoothDeviceStatusToString(BluetoothStatus:LongWord):String;
begin
 {}
 Result:='BLUETOOTH_STATUS_UNKNOWN';

 if BluetoothStatus <= BLUETOOTH_STATUS_MAX then
  begin
   Result:=BLUETOOTH_STATUS_NAMES[BluetoothStatus];
  end;
end;

{==============================================================================}

function BluetoothDeviceStateToNotification(State:LongWord):LongWord;
{Convert a Device state value into the notification code for device notifications}
begin
 {}
 Result:=DEVICE_NOTIFICATION_NONE;

 {Check State}
 case State of
  BLUETOOTH_STATE_DETACHED:Result:=DEVICE_NOTIFICATION_DETACH;
  BLUETOOTH_STATE_DETACHING:Result:=DEVICE_NOTIFICATION_DETACHING;
  BLUETOOTH_STATE_ATTACHING:Result:=DEVICE_NOTIFICATION_ATTACHING;
  BLUETOOTH_STATE_ATTACHED:Result:=DEVICE_NOTIFICATION_ATTACH;
 end;
end;

{==============================================================================}

function BluetoothDeviceStatusToNotification(Status:LongWord):LongWord;
{Convert a Device status value into the notification code for device notifications}
begin
 {}
 Result:=DEVICE_NOTIFICATION_NONE;

 {Check Status}
 case Status of
  BLUETOOTH_STATUS_UNBOUND:Result:=DEVICE_NOTIFICATION_UNBIND;
  BLUETOOTH_STATUS_BOUND:Result:=DEVICE_NOTIFICATION_BIND;
 end;
end;

{==============================================================================}

function BluetoothHostTypeToString(HostType:LongWord):String;
begin
 {}
 Result:='BLUETOOTHHOST_TYPE_UNKNOWN';

 if HostType <= BLUETOOTHHOST_TYPE_MAX then
  begin
   Result:=BLUETOOTHHOST_TYPE_NAMES[HostType];
  end;
end;

{==============================================================================}

function BluetoothHostStateToString(HostState:LongWord):String;
begin
 {}
 Result:='BLUETOOTHHOST_STATE_UNKNOWN';

 if HostState <= BLUETOOTHHOST_STATE_MAX then
  begin
   Result:=BLUETOOTHHOST_STATE_NAMES[HostState];
  end;
end;

{==============================================================================}

function BluetoothHostStateToNotification(State:LongWord):LongWord;
{Convert a Host state value into the notification code for device notifications}
begin
 {}
 Result:=DEVICE_NOTIFICATION_NONE;

 {Check State}
 case State of
  BLUETOOTHHOST_STATE_DISABLED:Result:=DEVICE_NOTIFICATION_DISABLE;
  BLUETOOTHHOST_STATE_ENABLED:Result:=DEVICE_NOTIFICATION_ENABLE;
 end;
end;

{==============================================================================}

procedure BluetoothLog(Level:LongWord;Device:PBluetoothDevice;const AText:String);
var
 WorkBuffer:String;
begin
 {}
 {Check Level}
 if Level < BLUETOOTH_DEFAULT_LOG_LEVEL then Exit;

 WorkBuffer:='';
 {Check Level}
 if Level = BLUETOOTH_LOG_LEVEL_DEBUG then
  begin
   WorkBuffer:=WorkBuffer + '[DEBUG] ';
  end
 else if Level = BLUETOOTH_LOG_LEVEL_WARN then
  begin
   WorkBuffer:=WorkBuffer + '[WARN] ';
  end
 else if Level = BLUETOOTH_LOG_LEVEL_ERROR then
  begin
   WorkBuffer:=WorkBuffer + '[ERROR] ';
  end;

 {Add Prefix}
 WorkBuffer:=WorkBuffer + 'Bluetooth: ';

 {Check Device}
 if Device <> nil then
  begin
   //To Do
   //WorkBuffer:=WorkBuffer + 'Device' + IntToStr(Device.Address) + ': ';
  end;

 {Output Logging}
 LoggingOutputEx(LOGGING_FACILITY_BLUETOOTH,LogLevelToLoggingSeverity(Level),'Bluetooth',WorkBuffer + AText);
end;

{==============================================================================}

procedure BluetoothLogInfo(Device:PBluetoothDevice;const AText:String); inline;
begin
 {}
 BluetoothLog(BLUETOOTH_LOG_LEVEL_INFO,Device,AText);
end;

{==============================================================================}

procedure BluetoothLogWarn(Device:PBluetoothDevice;const AText:String); inline;
begin
 {}
 BluetoothLog(BLUETOOTH_LOG_LEVEL_WARN,Device,AText);
end;

{==============================================================================}

procedure BluetoothLogError(Device:PBluetoothDevice;const AText:String); inline;
begin
 {}
 BluetoothLog(BLUETOOTH_LOG_LEVEL_ERROR,Device,AText);
end;

{==============================================================================}

procedure BluetoothLogDebug(Device:PBluetoothDevice;const AText:String); inline;
begin
 {}
 BluetoothLog(BLUETOOTH_LOG_LEVEL_DEBUG,Device,AText);
end;

{==============================================================================}
{==============================================================================}
{Bluetooth Internal Functions}
procedure BluetoothLogOutput(const AText:String;Data:Pointer);
{Default log output procedure for BluetoothLogDevices etc}
begin
  {}
  LoggingOutput(AText);
end;

{==============================================================================}
{Bluetooth USB Functions}
function BluetoothUSBDriverBind(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;

 {Check Interface}
 //To Do
 Result:=USB_STATUS_DEVICE_UNSUPPORTED;

 //To Do
end;

{==============================================================================}

function BluetoothUSBDriverUnbind(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;

 {Check Interface}
 //To Do
 Result:=USB_STATUS_DEVICE_UNSUPPORTED;

 //To Do
end;

{==============================================================================}
{==============================================================================}

initialization
 BluetoothInit;
 if BLUETOOTH_AUTOSTART then
  begin
   if not BLUETOOTH_ASYNCSTART then
    begin
     {Start Bluetooth}
     BluetoothStart;
    end
   else
    begin
     {Schedule Worker}
     WorkerSchedule(BLUETOOTH_STARTDELAY,TWorkerTask(BluetoothAsyncStart),nil,nil);
    end;
  end;

{==============================================================================}

finalization
 BluetoothStop;

{==============================================================================}
{==============================================================================}

end.
