{
Ultibo Storage interface unit.

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
 
   Linux USB/SCSI drivers
   U-Boot USB/USB Mass Storage/SCSI drivers
   
References
==========

 USB Mass Storage Class Specification Overview 1.4
 
   http://www.usb.org/developers/docs/devclass_docs/Mass_Storage_Specification_Overview_v1.4_2-19-2010.pdf
   
 USB Mass Storage Bulk Only 1.0
  (BBB Subclass)

   http://www.usb.org/developers/docs/devclass_docs/usbmassbulk_10.pdf

 USB Mass Storage Control/Bulk/Interrupt (CBI) Specification 1.1
  (CB/CBI Subclass)

   http://www.usb.org/developers/docs/devclass_docs/usb_msc_cbi_1.1.pdf
  
 USB Mass Storage UFI Command Specification 1.0
  (UFI Protocol)
  
   http://www.usb.org/developers/docs/devclass_docs/usbmass-ufi10.pdf
  
 USB Mass Storage Bootability Specification 1.0
  (SCSI Protocol)
  
   http://www.usb.org/developers/docs/devclass_docs/usb_msc_boot_1.0.pdf

Storage Devices
===============

This unit provides both the Storage device interface and the generic USB mass storage driver.

   
USB Mass Storage Devices
========================

The USB Mass Storage Class Control/Bulk/Interrupt (CBI) Transport specification is approved for use only with
full-speed floppy disk drives. CBI shall not be used in high-speed capable devices, or in devices other than
floppy disk drives. CBI shall not be used in devices that implement LSDFS. Usage of CBI for any new design is
discouraged.

Therefore the majority of USB Mass Storage devices use the Bulk only interface.

USB mass storage class devices normally use the SCSI command set and therefore the USB storage driver consumes
the SCSI unit for the protocol and command definitions. USB mass storage devices are registered directly as
Storage devices not as SCSI devices.

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit Storage;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,USB,SysUtils;

//To Do               //USB Storage Read Protect / Write Protect
                      //USB Storage CBI Transport
                      //MMC-5 Command set extensions (For CD/DVD)
           
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
 STORAGE_LOG_LEVEL_ERROR     = LOG_LEVEL_ERROR;  {Storage error messages}
 STORAGE_LOG_LEVEL_NONE      = LOG_LEVEL_NONE;   {No Storage messages}

var 
 STORAGE_DEFAULT_LOG_LEVEL:LongWord = STORAGE_LOG_LEVEL_DEBUG; {Minimum level for Storage messages.  Only messages with level greater than or equal to this will be printed}
 
var 
 {Storage logging}
 STORAGE_LOG_ENABLED:Boolean; 
 
{==============================================================================}
const
 {USB Storage specific constants}
 USBSTORAGE_DRIVER_NAME = 'USB Mass Storage Driver'; {Name of USB storage driver}

 {CB/CBI (Control/Bulk/Interrupt) Requests}
 USB_STORAGE_REQUEST_CBI_ADSC = 0;
 
 {BBB (Bulk Only) Requests}
 USB_STORAGE_REQUEST_BBB_RESET       = $FF;
 USB_STORAGE_REQUEST_BBB_GET_MAX_LUN = $FE;
 
 {Command Block Wrapper constants}
 USB_STORAGE_CBW_SIGNATURE	= $43425355;
 
 USB_STORAGE_CBW_FLAGS_OUT  = $00;
 USB_STORAGE_CBW_FLAGS_IN   = $80;
 
 USB_STORAGE_CBW_CB_LENGTH  = 16;
 
 USB_STORAGE_CBW_SIZE = 31;
 
 {Command Status Wrapper}
 USB_STORAGE_CSW_SIGNATURE  = $53425355;

 USB_STORAGE_CSW_STATUS_GOOD   = $00;
 USB_STORAGE_CSW_STATUS_FAILED = $01;
 USB_STORAGE_CSW_STATUS_PHASE  = $02;
  
 USB_STORAGE_CSW_SIZE = 13;
 
 {Command Data}
 USB_STORAGE_COMMAND_MAX_SIZE = 16;
 
 {Command Retry Counts}
 USB_STORAGE_READ_RETRIES            = 2;
 USB_STORAGE_WRITE_RETRIES           = 2;
 USB_STORAGE_STATUS_RETRIES          = 2;
 USB_STORAGE_INQUIRY_RETRIES         = 5;
 USB_STORAGE_READ_CAPACITY_RETRIES   = 3;
 USB_STORAGE_TEST_UNIT_READY_RETRIES = 5; //10; TestingRPi
 
 {Peripheral Device Types}
 {These are a subset of the SCSI Peripheral Device Types}
 USB_STORAGE_DEVICE_TYPE_DISK    = $00; {SBC Direct-access device (e.g., UHD Floppy disk)}
 USB_STORAGE_DEVICE_TYPE_CDROM   = $05; {MMC-5 CD-ROM device}
 USB_STORAGE_DEVICE_TYPE_OPTICAL = $07; {Optical memory device (e.g., Non-CD optical disks)}
 USB_STORAGE_DEVICE_TYPE_RBC     = $0E; {RBC Direct-access device (e.g., UHD Floppy disk)}
 USB_STORAGE_DEVICE_TYPE_UNKNOWN = $1F; {Unknown or no device type}
 
{==============================================================================}
type
 {Storage specific types}
 {Storage Device}
 PStorageDevice = ^TStorageDevice;
 
 {Storage Enumeration Callback}
 TStorageEnumerate = function(Storage:PStorageDevice;Data:Pointer):LongWord;
 {Storage Notification Callback}
 TStorageNotification = function(Device:PDevice;Data:Pointer;Notification:LongWord):LongWord;
 
 {Storage Device Methods}
 TStorageDeviceRead = function(Storage:PStorageDevice;const Start,Count:Int64;Buffer:Pointer):LongWord; 
 TStorageDeviceWrite = function(Storage:PStorageDevice;const Start,Count:Int64;Buffer:Pointer):LongWord;
 TStorageDeviceErase = function(Storage:PStorageDevice;const Start,Count:Int64):LongWord;
 TStorageDeviceControl = function(Storage:PStorageDevice;Request:Integer;Argument1:LongWord;var Argument2:LongWord):LongWord;
 
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
  ReadCount:LongWord;
  ReadErrors:LongWord;
  WriteCount:LongWord;
  WriteErrors:LongWord;
  EraseCount:LongWord;
  EraseErrors:LongWord;
  {Internal Properties}                                                                        
  Prev:PStorageDevice;                 {Previous entry in Storage table}
  Next:PStorageDevice;                 {Next entry in Storage table}
 end;
 
{==============================================================================}
type
 {USB Storage specific types}
 {SBC Command Block (Derived from TSCSICommandBlock)}
 PUSBCommandBlock = ^TUSBCommandBlock;
 TUSBCommandBlock = record
  {SCSI Properties}
  Command:array[0..USB_STORAGE_COMMAND_MAX_SIZE - 1] of Byte;     {Command}
  SenseData:array[0..63] of Byte;   {Request Sense}
  Status:Byte;                      {SCSI Status}
  TargetID:Byte;                    {Target ID}
  TargetLUN:Byte;                   {Target LUN}
  CommandLength:Byte;               {Command Length}
  DataLength:LongWord;              {Data Length}
  Data:Pointer;                     {Pointer to Data}
  MessageOut:array[0..11] of Byte;  {Message out buffer}
  MessageIn:array[0..11] of Byte;   {Message in buffer}
  SenseCommandLength:Byte;          {Sense Command Length}
  SenseDataLength:LongWord;         {Sense Data Length}
  SenseCommand:array[0..5] of Byte; {Sense Command}
  ControllerStatus:LongWord;        {Controller Status}
  TransferredBytes:LongWord;        {Transferred Bytes}
  {USB Properties}
  Direction:Byte;                   {USB Direction (eg USB_DIRECTION_OUT or USB_DIRECTION_IN)}
 end;
 //To Do //Many of these may not be required. Handled instead in the Device descriptors ?

 {MMC5 Command Block (Derived from TATAPICommandBlock}
 //To Do

 {RBC Command Block (Derived from TSCSICommandBlock}
 //To Do
 
 {USB Storage Device}
 PUSBStorageDevice = ^TUSBStorageDevice;
 TUSBStorageDevice = record
  {Storage Properties}
  Storage:TStorageDevice;
  {USB Properties}
  Subclass:Byte;
  Protocol:Byte;
  MaxLUN:Byte;
  Sequence:LongWord;                          {For dCBWTag property}
  Primary:PUSBStorageDevice;                  {Primary storage device (for multi LUN devices)} //To Do
  StorageInterface:PUSBInterface;             {USB Mass Storage device Interface}
  ReadWait:TSemaphoreHandle;                  {Read completed semaphore}
  WriteWait:TSemaphoreHandle;                 {Write completed semaphore}
  InterruptWait:TSemaphoreHandle;             {Interrupt completed semaphore}
  ReadRequest:PUSBRequest;                    {Bulk IN Request}
  WriteRequest:PUSBRequest;                   {Bulk OUT Request}
  InterruptRequest:PUSBRequest;               {Interrupt IN Request}
  ReadEndpoint:PUSBEndpointDescriptor;        {Bulk IN Endpoint}
  WriteEndpoint:PUSBEndpointDescriptor;       {Bulk OUT Endpoint}
  InterruptEndpoint:PUSBEndpointDescriptor;   {Interrupt IN Endpoint}
  PendingCount:LongWord;                      {Number of USB requests pending for this storage}
  WaiterThread:TThreadId;                     {Thread waiting for pending requests to complete (for storage eject)}
  {SCSI Properties}
  CommandBlock:TUSBCommandBlock;
  {ATAPI Properties}
  //To Do //MMC-5 (ATAPI) is an extension of SCSI, see: SCSI Multi-Media Commands – 5 (MMC-5) (Draft) mmc5r02c.pdf
  {RBC Properties}
  //To Do //RBC is a subset of SCSI, see: Reduced Block Commands (RBC) (Draft) 97-260r2.pdf
 end;
 
 {Command Block Wrapper}
 PUSBCommandBlockWrapper = ^TUSBCommandBlockWrapper;
 TUSBCommandBlockWrapper = record
  dCBWSignature:LongWord;
  dCBWTag:LongWord;
  dCBWDataTransferLength:LongWord;
  bCBWFlags:Byte;
  bCBWLUN:Byte;
  bCBWCBLength:Byte;
  CBWCB:array[0..USB_STORAGE_CBW_CB_LENGTH - 1] of Byte;
 end;
 
 {Command Status Wrapper}
 PUSBCommandStatusWrapper = ^TUSBCommandStatusWrapper;
 TUSBCommandStatusWrapper = record
  dCSWSignature:LongWord;
  dCSWTag:LongWord;
  dCSWDataResidue:LongWord;
  bCSWStatus:Byte;
 end;
  
{==============================================================================}
{var}
 {Storage specific variables}
 
{==============================================================================}
{var}
 {USB Storage specific variables}
 
{==============================================================================}
{Initialization Functions}
procedure StorageInit;

{==============================================================================}
{Storage Functions}
function StorageDeviceRead(Storage:PStorageDevice;const Start,Count:Int64;Buffer:Pointer):LongWord; 
function StorageDeviceWrite(Storage:PStorageDevice;const Start,Count:Int64;Buffer:Pointer):LongWord;
function StorageDeviceErase(Storage:PStorageDevice;const Start,Count:Int64):LongWord;
function StorageDeviceControl(Storage:PStorageDevice;Request:Integer;Argument1:LongWord;var Argument2:LongWord):LongWord;

function StorageDeviceSetState(Storage:PStorageDevice;State:LongWord):LongWord;

function StorageDeviceStartStatus(Storage:PStorageDevice;Interval:LongWord):LongWord;
function StorageDeviceStopStatus(Storage:PStorageDevice):LongWord;

function StorageDeviceCreate:PStorageDevice;
function StorageDeviceCreateEx(Size:LongWord):PStorageDevice;
function StorageDeviceDestroy(Storage:PStorageDevice):LongWord;

function StorageDeviceRegister(Storage:PStorageDevice):LongWord;
function StorageDeviceDeregister(Storage:PStorageDevice):LongWord;

function StorageDeviceFind(StorageId:LongWord):PStorageDevice;
function StorageDeviceFindByName(const Name:String):PStorageDevice; inline;
function StorageDeviceFindByDescription(const Description:String):PStorageDevice; inline;
function StorageDeviceEnumerate(Callback:TStorageEnumerate;Data:Pointer):LongWord;

function StorageDeviceNotification(Storage:PStorageDevice;Callback:TStorageNotification;Data:Pointer;Notification,Flags:LongWord):LongWord;

{==============================================================================}
{USB Storage Functions}
function USBStorageDeviceRead(Storage:PStorageDevice;const Start,Count:Int64;Buffer:Pointer):LongWord; 
function USBStorageDeviceWrite(Storage:PStorageDevice;const Start,Count:Int64;Buffer:Pointer):LongWord;
function USBStorageDeviceControl(Storage:PStorageDevice;Request:Integer;Argument1:LongWord;var Argument2:LongWord):LongWord;

function USBStorageDriverBind(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;
function USBStorageDriverUnbind(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;

procedure USBStorageReadComplete(Request:PUSBRequest);
procedure USBStorageWriteComplete(Request:PUSBRequest);
procedure USBStorageInterruptComplete(Request:PUSBRequest);

{==============================================================================}
{Storage Helper Functions}
function StorageGetCount:LongWord; inline;

function StorageDeviceCheck(Storage:PStorageDevice):PStorageDevice;

function StorageDeviceTypeToString(StorageType:LongWord):String;
function StorageDeviceStateToString(StorageState:LongWord):String;

function StorageDeviceStateToNotification(State:LongWord):LongWord;

procedure StorageLog(Level:LongWord;Storage:PStorageDevice;const AText:String);
procedure StorageLogInfo(Storage:PStorageDevice;const AText:String);
procedure StorageLogError(Storage:PStorageDevice;const AText:String);
procedure StorageLogDebug(Storage:PStorageDevice;const AText:String);

procedure StorageStatusTimer(Storage:PStorageDevice);

{==============================================================================}
{USB Storage Helper Functions}
function USBStorageCheckSubclass(Subclass:Byte):Boolean;
function USBStorageCheckProtocol(Protocol:Byte):Boolean;

function USBStorageBlockSizeToBlockShift(BlockSize:LongWord):LongWord;

function USBStoragePatchDevice(Device:PUSBDevice;var Subclass,Protocol:Byte):LongWord;
function USBStorageFixupDevice(Device:PUSBDevice;var Vendor,Product:String):LongWord;

function USBStorageDeviceLock(Device:PUSBDevice;Storage:PUSBStorageDevice):LongWord;
function USBStorageDeviceUnlock(Device:PUSBDevice;Storage:PUSBStorageDevice):LongWord;

function USBStorageDeviceLoad(Device:PUSBDevice;Storage:PUSBStorageDevice):LongWord;
function USBStorageDeviceEject(Device:PUSBDevice;Storage:PUSBStorageDevice):LongWord;

function USBStorageDeviceReset(Device:PUSBDevice;Storage:PUSBStorageDevice):LongWord;

function USBStorageDeviceGetInfo(Device:PUSBDevice;Storage:PUSBStorageDevice):LongWord; 
function USBStorageDeviceGetStatus(Device:PUSBDevice;Storage:PUSBStorageDevice):LongWord; 
function USBStorageDeviceGetMaxLUN(Device:PUSBDevice;Storage:PUSBStorageDevice;var MaxLUN:Byte):LongWord;

function USBStorageDeviceClearStall(Device:PUSBDevice;Storage:PUSBStorageDevice;Endpoint:PUSBEndpointDescriptor):LongWord;

function USBStorageDeviceInquiry(Device:PUSBDevice;Storage:PUSBStorageDevice;var DeviceType,DeviceFlags:LongWord;var Vendor,Product,Revision:PChar):LongWord;  
function USBStorageDeviceRequestSense(Device:PUSBDevice;Storage:PUSBStorageDevice;var SenseKey,ASC,ASCQ:Byte):LongWord;
function USBStorageDeviceReadCapacity(Device:PUSBDevice;Storage:PUSBStorageDevice;var BlockSize,BlockShift:LongWord;var BlockCount:Int64):LongWord;
function USBStorageDeviceTestUnitReady(Device:PUSBDevice;Storage:PUSBStorageDevice;var DeviceFlags:LongWord):LongWord;

function USBStorageDeviceRead10(Device:PUSBDevice;Storage:PUSBStorageDevice;Start:LongWord;Count:Word;Buffer:Pointer):LongWord; 
function USBStorageDeviceWrite10(Device:PUSBDevice;Storage:PUSBStorageDevice;Start:LongWord;Count:Word;Buffer:Pointer):LongWord; 

function USBStorageDeviceRead16(Device:PUSBDevice;Storage:PUSBStorageDevice;const Start,Count:Int64;Buffer:Pointer):LongWord; 
function USBStorageDeviceWrite16(Device:PUSBDevice;Storage:PUSBStorageDevice;const Start,Count:Int64;Buffer:Pointer):LongWord; 

function USBStorageDeviceTransport(Device:PUSBDevice;Storage:PUSBStorageDevice;Command:PUSBCommandBlock):LongWord; 

{==============================================================================}
{==============================================================================}

implementation

uses SCSI;  {Storage includes SCSI to support the USB Mass Storage Driver}

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
var
 {USB Storage specific variables}
 USBStorageDriver:PUSBDriver;  {USB Storage Driver interface (Set by StorageInit)}

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure StorageInit;
var
 Status:LongWord;
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
 
 {Create USB Storage Driver}
 USBStorageDriver:=USBDriverCreate;
 if USBStorageDriver <> nil then
  begin
   {Update USB Storage Driver}
   {Driver}
   USBStorageDriver.Driver.DriverName:=USBSTORAGE_DRIVER_NAME; 
   {USB}
   USBStorageDriver.DriverBind:=USBStorageDriverBind;
   USBStorageDriver.DriverUnbind:=USBStorageDriverUnbind;
 
   {Register USB Storage driver}
   Status:=USBDriverRegister(USBStorageDriver); 
   if Status <> USB_STATUS_SUCCESS then
    begin
     if USB_LOG_ENABLED then USBLogError(nil,'Storage: Failed to register USB storage driver: ' + USBStatusToString(Status));
    end;
  end
 else
  begin
   if STORAGE_LOG_ENABLED then StorageLogError(nil,'Failed to create USB storage driver');
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

function StorageDeviceControl(Storage:PStorageDevice;Request:Integer;Argument1:LongWord;var Argument2:LongWord):LongWord;
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
{USB Storage Functions}
function USBStorageDeviceRead(Storage:PStorageDevice;const Start,Count:Int64;Buffer:Pointer):LongWord; 
{Note: According to 3.4 of USB Mass Storage Bulk Only 1.0, requests must be queued one at a time to a device}
var
 Status:LongWord;
 BlockCount:Word;
 ReadOffset:PtrUInt;
 ReadRemain:Int64;
 Device:PUSBDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {$IFDEF STORAGE_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(nil,'USBStorageDeviceRead (Start=' + IntToStr(Start) + ' Count=' + IntToStr(Count) + ')');
 {$ENDIF}
 
 {Check Storage}
 if Storage = nil then Exit;
 if Storage.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check Count}
 Result:=ERROR_SUCCESS;
 if Count = 0 then Exit;
 
 {Check Buffer}
 Result:=ERROR_INVALID_PARAMETER;
 if Buffer = nil then Exit;
 
 {Get Device}
 Device:=PUSBDevice(Storage.Device.DeviceData);
 if Device = nil then Exit;

 {Check Storage}
 if Storage.BlockSize = 0 then Exit;
 if Storage.BlockCount = 0 then Exit;
 
 {Check State}
 if Storage.StorageState <> STORAGE_STATE_INSERTED then Exit;
 
 {Acquire the Lock}
 if MutexLock(Storage.Lock) = ERROR_SUCCESS then
  begin
   try
    {Check Subclass}
    case PUSBStorageDevice(Storage).Subclass of
     USB_SUBCLASS_MASS_STORAGE_UFI,USB_SUBCLASS_MASS_STORAGE_SFF8070I,USB_SUBCLASS_MASS_STORAGE_SCSI:begin
       {Check Size}
       if Storage.BlockCount <= $FFFFFFFF then
        begin
         {Read 10}
         {Start Read}
         ReadOffset:=0;
         ReadRemain:=Count;
         while ReadRemain > 0 do
          begin
           {Get Count}
           BlockCount:=ReadRemain;
           if ReadRemain > SCSI_READ_10_MAX_BLOCKS then BlockCount:=SCSI_READ_10_MAX_BLOCKS;
        
           {Read 10}
           Status:=USBStorageDeviceRead10(Device,PUSBStorageDevice(Storage),(Start + ReadOffset),BlockCount,Pointer(PtrUInt(Buffer) + (ReadOffset shl Storage.BlockShift))); 
           if Status <> USB_STATUS_SUCCESS then
            begin
             Exit;
            end;
       
           Inc(ReadOffset,BlockCount);
           Dec(ReadRemain,BlockCount);
          end;
      
         Result:=ERROR_SUCCESS; 
        end
       else
        begin
         {Read 16}
         Status:=USBStorageDeviceRead16(Device,PUSBStorageDevice(Storage),Start,Count,Buffer);
         if Status <> USB_STATUS_SUCCESS then
          begin
           Exit;
          end;

         Result:=ERROR_SUCCESS; 
        end;
      end;
     else
      begin
       {Error Return}
       Result:=ERROR_INVALID_PARAMETER; 
      end;   
    end;
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
 
{==============================================================================}

function USBStorageDeviceWrite(Storage:PStorageDevice;const Start,Count:Int64;Buffer:Pointer):LongWord;
{Note: According to 3.4 of USB Mass Storage Bulk Only 1.0, requests must be queued one at a time to a device}
var
 Status:LongWord;
 BlockCount:Word;
 WriteOffset:PtrUInt;
 WriteRemain:Int64;
 Device:PUSBDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {$IFDEF STORAGE_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(nil,'USBStorageDeviceWrite (Start=' + IntToStr(Start) + ' Count=' + IntToStr(Count) + ')');
 {$ENDIF}
 
 {Check Storage}
 if Storage = nil then Exit;
 if Storage.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check Count}
 Result:=ERROR_SUCCESS;
 if Count = 0 then Exit;
 
 {Check Buffer}
 Result:=ERROR_INVALID_PARAMETER;
 if Buffer = nil then Exit;
 
 {Get Device}
 Device:=PUSBDevice(Storage.Device.DeviceData);
 if Device = nil then Exit;
 
 {Check Storage}
 if Storage.BlockSize = 0 then Exit;
 if Storage.BlockCount = 0 then Exit;
 
 {Check State}
 if Storage.StorageState <> STORAGE_STATE_INSERTED then Exit;
 
 {Acquire the Lock}
 if MutexLock(Storage.Lock) = ERROR_SUCCESS then
  begin
   try
    {Check Subclass}
    case PUSBStorageDevice(Storage).Subclass of
     USB_SUBCLASS_MASS_STORAGE_UFI,USB_SUBCLASS_MASS_STORAGE_SFF8070I,USB_SUBCLASS_MASS_STORAGE_SCSI:begin
       {Check Size}
       if Storage.BlockCount <= $FFFFFFFF then
        begin
         {Write 10}
         {Start Write}
         WriteOffset:=0;
         WriteRemain:=Count;
         while WriteRemain > 0 do
          begin
           {Get Count}
           BlockCount:=WriteRemain;
           if WriteRemain > SCSI_WRITE_10_MAX_BLOCKS then BlockCount:=SCSI_WRITE_10_MAX_BLOCKS;
        
           {Write 10}
           Status:=USBStorageDeviceWrite10(Device,PUSBStorageDevice(Storage),(Start + WriteOffset),BlockCount,Pointer(PtrUInt(Buffer) + (WriteOffset shl Storage.BlockShift))); 
           if Status <> USB_STATUS_SUCCESS then
            begin
             Exit;
            end;
       
           Inc(WriteOffset,BlockCount);
           Dec(WriteRemain,BlockCount);
          end;
      
         Result:=ERROR_SUCCESS; 
        end
       else
        begin
         {Write 16}
         Status:=USBStorageDeviceWrite16(Device,PUSBStorageDevice(Storage),Start,Count,Buffer);
         if Status <> USB_STATUS_SUCCESS then
          begin
           Exit;
          end;

         Result:=ERROR_SUCCESS; 
        end;
      end;
     else
      begin
       {Error Return}
       Result:=ERROR_INVALID_PARAMETER; 
      end;   
    end;
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

{==============================================================================}

function USBStorageDeviceControl(Storage:PStorageDevice;Request:Integer;Argument1:LongWord;var Argument2:LongWord):LongWord;
{Note: According to 3.4 of USB Mass Storage Bulk Only 1.0, requests must be queued one at a time to a device}
var
 Status:LongWord;
 Device:PUSBDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {$IFDEF STORAGE_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(nil,'USBStorageDeviceControl (Request=' + IntToStr(Request) + ' Argument1=' + IntToStr(Argument1) + ' Argument2=' + IntToStr(Argument2) + ')');
 {$ENDIF}
 
 {Check Storage}
 if Storage = nil then Exit;
 if Storage.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Get Device}
 Device:=PUSBDevice(Storage.Device.DeviceData);
 if Device = nil then Exit;
 
 {Acquire the Lock}
 if MutexLock(Storage.Lock) = ERROR_SUCCESS then
  begin
   try
    {Check Subclass}
    case PUSBStorageDevice(Storage).Subclass of
     USB_SUBCLASS_MASS_STORAGE_UFI,USB_SUBCLASS_MASS_STORAGE_SFF8070I,USB_SUBCLASS_MASS_STORAGE_SCSI:begin
       {Check Request}
       case Request of
        STORAGE_CONTROL_TEST_READY,STORAGE_CONTROL_TEST_MEDIA:begin
          {Check Flags}
          if (Storage.Device.DeviceFlags and (STORAGE_FLAG_NOT_READY or STORAGE_FLAG_NO_MEDIA)) = 0 then
           begin
            {Ready}
            {Test Unit Ready}
            Status:=USBStorageDeviceTestUnitReady(Device,PUSBStorageDevice(Storage),Storage.Device.DeviceFlags);
             
            {Check Status and Flags}
            if (Status <> USB_STATUS_SUCCESS) or ((Storage.Device.DeviceFlags and (STORAGE_FLAG_NOT_READY or STORAGE_FLAG_NO_MEDIA)) <> 0) then
             begin
              {Not Ready}
              Storage.BlockSize:=0;
              Storage.BlockShift:=0;
              Storage.BlockCount:=0;
              //To Do //Vendor etc
              
              {Set State to Ejected}
              StorageDeviceSetState(Storage,STORAGE_STATE_EJECTED);
              
              Result:=ERROR_NOT_READY;
              Exit;
             end;
           
            {Return Result}
            Result:=ERROR_SUCCESS; 
           end
          else
           begin
            {Not Ready}
            {Test Unit Ready}
            Status:=USBStorageDeviceTestUnitReady(Device,PUSBStorageDevice(Storage),Storage.Device.DeviceFlags);
            
            {Check Status and Flags}
            if (Status = USB_STATUS_SUCCESS) and ((Storage.Device.DeviceFlags and (STORAGE_FLAG_NOT_READY or STORAGE_FLAG_NO_MEDIA)) = 0) then
             begin
              {Ready}
              {Get Info}
              Status:=USBStorageDeviceGetInfo(Device,PUSBStorageDevice(Storage));
              if Status = USB_STATUS_SUCCESS then
               begin
               
                {Set State to Inserted}
                StorageDeviceSetState(Storage,STORAGE_STATE_INSERTED);
               end;
               
              Result:=ERROR_SUCCESS;
              Exit;
             end;

            {Return Result}
            Result:=ERROR_NOT_READY; 
           end;
         end;
        STORAGE_CONTROL_RESET:begin
          {Reset Device}
          Status:=USBStorageDeviceReset(Device,PUSBStorageDevice(Storage));
          if Status <> USB_STATUS_SUCCESS then
           begin
            {Return Result}
            Result:=ERROR_OPERATION_FAILED;
            Exit;
           end; 
          
          {Return Result}
          Result:=ERROR_SUCCESS; 
         end;
        STORAGE_CONTROL_LOCK:begin
          {Check Flags}
          if (Storage.Device.DeviceFlags and STORAGE_FLAG_LOCKABLE) <> 0 then
           begin
            if (Storage.Device.DeviceFlags and STORAGE_FLAG_LOCKED) = 0 then
             begin
              {Lock Media}
              Status:=USBStorageDeviceLock(Device,PUSBStorageDevice(Storage));
              if Status <> USB_STATUS_SUCCESS then
               begin
                {Return Result}
                Result:=ERROR_OPERATION_FAILED;
                Exit;
               end; 
              
              {Set Flag}
              Storage.Device.DeviceFlags:=Storage.Device.DeviceFlags or STORAGE_FLAG_LOCKED;
             end;
           end; 
          
          {Return Result}
          Result:=ERROR_SUCCESS; 
         end;
        STORAGE_CONTROL_UNLOCK:begin
          {Check Flags}
          if (Storage.Device.DeviceFlags and STORAGE_FLAG_LOCKABLE) <> 0 then
           begin
            if (Storage.Device.DeviceFlags and STORAGE_FLAG_LOCKED) <> 0 then
             begin
              {Unlock Media}
              Status:=USBStorageDeviceUnlock(Device,PUSBStorageDevice(Storage));
              if Status <> USB_STATUS_SUCCESS then
               begin
                {Return Result}
                Result:=ERROR_OPERATION_FAILED;
                Exit;
               end;  

              {Clear Flag}
              Storage.Device.DeviceFlags:=Storage.Device.DeviceFlags and not(STORAGE_FLAG_LOCKED);
             end;
           end; 
          
          {Return Result}
          Result:=ERROR_SUCCESS; 
         end;
        STORAGE_CONTROL_EJECT:begin
          {Check Flags}
          if (Storage.Device.DeviceFlags and STORAGE_FLAG_EJECTABLE) <> 0 then
           begin
            {Eject Media}
            Status:=USBStorageDeviceEject(Device,PUSBStorageDevice(Storage));
            if Status <> USB_STATUS_SUCCESS then
             begin
              {Return Result}
              Result:=ERROR_OPERATION_FAILED;
              Exit;
             end;  
           end; 
          
          {Return Result}
          Result:=ERROR_SUCCESS; 
         end;
        STORAGE_CONTROL_TEST_LOCKED:begin
          {Test Media Locked}
          if (Storage.Device.DeviceFlags and STORAGE_FLAG_LOCKED) = 0 then
           begin
            {Return Result}
            Result:=ERROR_NOT_LOCKED; 
            Exit;
           end;
          
          {Return Result}
          Result:=ERROR_SUCCESS; 
         end;
        STORAGE_CONTROL_TEST_CHANGED:begin
          {Not Supported}
          Result:=ERROR_NOT_SUPPORTED; 
         end;
        STORAGE_CONTROL_GET_VENDORID:begin
          {Get Vendor ID}
          if Device.Descriptor = nil then
           begin
            {Return Result}
            Result:=ERROR_OPERATION_FAILED; 
            Exit;
           end;
           
          Argument2:=Device.Descriptor.idVendor;
          
          {Return Result}
          Result:=ERROR_SUCCESS; 
         end;
        STORAGE_CONTROL_GET_PRODUCTID:begin
          {Get Product ID}
          if Device.Descriptor = nil then
           begin
            {Return Result}
            Result:=ERROR_OPERATION_FAILED; 
            Exit;
           end;

          Argument2:=Device.Descriptor.idProduct;
          
          {Return Result}
          Result:=ERROR_SUCCESS; 
         end;
        STORAGE_CONTROL_GET_SERIAL:begin
          {Get Serial No}
          Argument2:=LongWord(@Device.SerialNumber);
          
          {Return Result}
          Result:=ERROR_SUCCESS; 
         end;
        STORAGE_CONTROL_GET_REVISION:begin
          {Get Revision No}
          if Storage.Revision = nil then
           begin
            {Return Result}
            Result:=ERROR_OPERATION_FAILED; 
            Exit;
           end;
           
          Argument2:=LongWord(Storage.Revision);
          
          {Return Result}
          Result:=ERROR_SUCCESS; 
         end;
        STORAGE_CONTROL_GET_PRODUCT:begin
          {Get Product Name}
          if Storage.Product = nil then
           begin
            {Return Result}
            Result:=ERROR_OPERATION_FAILED; 
            Exit;
           end;
           
          Argument2:=LongWord(Storage.Product);
          
          {Return Result}
          Result:=ERROR_SUCCESS; 
         end;
        STORAGE_CONTROL_GET_MANUFACTURER:begin
          {Get Manufacturer Name}
          if Storage.Vendor = nil then
           begin
            {Return Result}
            Result:=ERROR_OPERATION_FAILED; 
            Exit;
           end;

          Argument2:=LongWord(Storage.Vendor);
          
          {Return Result}
          Result:=ERROR_SUCCESS; 
         end;
       end;
      end;
     else
      begin
       {Error Return}
       Result:=ERROR_INVALID_PARAMETER; 
      end;   
    end;
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

{==============================================================================}

function USBStorageDriverBind(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;
{Bind the Storage driver to a USB device if it is suitable}
{Device: The USB device to attempt to bind to}
{Interrface: The USB interface to attempt to bind to (or nil for whole device)}
{Return: USB_STATUS_SUCCESS if completed, USB_STATUS_DEVICE_UNSUPPORTED if unsupported or another error code on failure}
var
 MaxLUN:Byte;
 LUN:LongWord;
 Patch:Integer;
 Subclass:Byte;
 Protocol:Byte;
 Status:LongWord;
 Storage:PUSBStorageDevice;
 Current:PUSBStorageDevice;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;
                      
 {$IFDEF USB_DEBUG}                    
 if USB_LOG_ENABLED then USBLogDebug(Device,'Storage: Attempting to bind USB device (' + ': Address ' + IntToStr(Device.Address) + ')'); //To Do //Device.Manufacturer //Device.Product
 {$ENDIF}
 
 {Check Interface (Bind to interface only)}
 if Interrface = nil then
  begin
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;
 
 {Check for Storage (Must be interface specific)}
 if Device.Descriptor.bDeviceClass <> USB_CLASS_CODE_INTERFACE_SPECIFIC then
  begin
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;   
  end;

 {Check Device Patch}
 Patch:=USBStoragePatchDevice(Device,Subclass,Protocol);
  
 {Check Interface (Must be Mass Storage class)}
 if (Interrface.Descriptor.bInterfaceClass <> USB_CLASS_CODE_MASS_STORAGE) or not(USBStorageCheckSubclass(Interrface.Descriptor.bInterfaceSubClass)) or not(USBStorageCheckProtocol(Interrface.Descriptor.bInterfaceProtocol)) then
  begin
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;   
  end;
      
 {Create Storage}
 Storage:=PUSBStorageDevice(StorageDeviceCreateEx(SizeOf(TUSBStorageDevice)));
 if Storage = nil then
  begin
   if USB_LOG_ENABLED then USBLogError(Device,'Storage: Failed to create new storage device');
   
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;
   
 {Update Storage} 
 {Device}
 Storage.Storage.Device.DeviceBus:=DEVICE_BUS_USB;
 Storage.Storage.Device.DeviceType:=STORAGE_TYPE_HDD;
 Storage.Storage.Device.DeviceFlags:=STORAGE_FLAG_NONE;
 Storage.Storage.Device.DeviceData:=Device;
 {Storage}
 Storage.Storage.StorageState:=STORAGE_STATE_EJECTED;
 Storage.Storage.DeviceRead:=USBStorageDeviceRead;
 Storage.Storage.DeviceWrite:=USBStorageDeviceWrite;
 Storage.Storage.DeviceErase:=nil;
 Storage.Storage.DeviceControl:=USBStorageDeviceControl;
 {Driver}
 Storage.Storage.TargetID:=0;
 Storage.Storage.TargetLUN:=0;
 Storage.Storage.BlockSize:=0;
 Storage.Storage.BlockCount:=0;
 Storage.Storage.BlockShift:=0;
 {USB}
 Storage.Subclass:=Interrface.Descriptor.bInterfaceSubClass;
 if Patch = USB_STATUS_SUCCESS then Storage.Subclass:=Subclass;
 Storage.Protocol:=Interrface.Descriptor.bInterfaceProtocol;
 if Patch = USB_STATUS_SUCCESS then Storage.Protocol:=Protocol;
 Storage.MaxLUN:=0;
 Storage.Primary:=nil;
 Storage.StorageInterface:=Interrface;
 Storage.ReadWait:=INVALID_HANDLE_VALUE;
 Storage.WriteWait:=INVALID_HANDLE_VALUE;
 Storage.InterruptWait:=INVALID_HANDLE_VALUE;
 Storage.ReadEndpoint:=USBDeviceFindEndpointByType(Device,Interrface,USB_DIRECTION_IN,USB_TRANSFER_TYPE_BULK);
 Storage.WriteEndpoint:=USBDeviceFindEndpointByType(Device,Interrface,USB_DIRECTION_OUT,USB_TRANSFER_TYPE_BULK);
 Storage.InterruptEndpoint:=USBDeviceFindEndpointByType(Device,Interrface,USB_DIRECTION_IN,USB_TRANSFER_TYPE_INTERRUPT);
 Storage.WaiterThread:=INVALID_HANDLE_VALUE;
 
 {Check Storage}
 case Storage.Protocol of
  USB_PROTOCOL_MASS_STORAGE_CBI:begin
    {Update Device Type}
    Storage.Storage.Device.DeviceType:=STORAGE_TYPE_FDD;
    
    {Check Endpoints}
    if (Storage.ReadEndpoint = nil) or (Storage.WriteEndpoint = nil) or (Storage.InterruptEndpoint = nil) then
     begin
      if USB_LOG_ENABLED then USBLogError(Device,'Storage: Failed to locate storage device Control/Bulk/Interrupt endpoints');
      
      {Destroy Storage}
      StorageDeviceDestroy(@Storage.Storage);
      
      {Return Result}
      Result:=USB_STATUS_DEVICE_UNSUPPORTED;
      Exit;
     end;
   end;
  USB_PROTOCOL_MASS_STORAGE_CB:begin
    {Update Device Type}
    Storage.Storage.Device.DeviceType:=STORAGE_TYPE_FDD;
    
    {Check Endpoints}
    if (Storage.ReadEndpoint = nil) or (Storage.WriteEndpoint = nil) then
     begin
      if USB_LOG_ENABLED then USBLogError(Device,'Storage: Failed to locate storage device Control/Bulk endpoints');
      
      {Destroy Storage}
      StorageDeviceDestroy(@Storage.Storage);
      
      {Return Result}
      Result:=USB_STATUS_DEVICE_UNSUPPORTED;
      Exit;
     end;
   end;
  USB_PROTOCOL_MASS_STORAGE_BBB:begin
    {Update Max LUN}
    Status:=USBStorageDeviceGetMaxLUN(Device,Storage,Storage.MaxLUN);
    if Status <> USB_STATUS_SUCCESS then
     begin
      if USB_LOG_ENABLED then USBLogError(Device,'Storage: Failed to get storage max LUN');
      
      {Destroy Storage}
      StorageDeviceDestroy(@Storage.Storage);
      
      {Return Result}
      Result:=USB_STATUS_DEVICE_UNSUPPORTED;
      Exit;
     end;
    
    if USB_LOG_ENABLED then USBLogInfo(Device,'Storage: Max LUN = ' + IntToStr(Storage.MaxLUN)); 
    
    {Check Endpoints}
    if (Storage.ReadEndpoint = nil) or (Storage.WriteEndpoint = nil) then
     begin
      if USB_LOG_ENABLED then USBLogError(Device,'Storage: Failed to locate storage device Bulk endpoints');
      
      {Destroy Storage}
      StorageDeviceDestroy(@Storage.Storage);
      
      {Return Result}
      Result:=USB_STATUS_DEVICE_UNSUPPORTED;
      Exit;
     end;
   end;
 end;

 {Create Read Semaphore}
 Storage.ReadWait:=SemaphoreCreate(0);
 if Storage.ReadWait = INVALID_HANDLE_VALUE then
  begin
   if USB_LOG_ENABLED then USBLogError(Device,'Storage: Failed to allocate storage device read semaphore');
   
   {Destroy Storage}
   StorageDeviceDestroy(@Storage.Storage);
     
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;

 {Create Write Semaphore}
 Storage.WriteWait:=SemaphoreCreate(0);
 if Storage.WriteWait = INVALID_HANDLE_VALUE then
  begin
   if USB_LOG_ENABLED then USBLogError(Device,'Storage: Failed to allocate storage device write semaphore');
   
   {Destroy Read Semaphore}
   SemaphoreDestroy(Storage.ReadWait);
   
   {Destroy Storage}
   StorageDeviceDestroy(@Storage.Storage);
     
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;
 
 {Create Interrupt Semaphore} 
 if Storage.InterruptEndpoint <> nil then
  begin
   Storage.InterruptWait:=SemaphoreCreate(0);
   if Storage.InterruptWait = INVALID_HANDLE_VALUE then
    begin
     if USB_LOG_ENABLED then USBLogError(Device,'Storage: Failed to allocate storage device interrupt semaphore');
     
     {Destroy Read Semaphore}
     SemaphoreDestroy(Storage.ReadWait);
     
     {Destroy Write Semaphore}
     SemaphoreDestroy(Storage.WriteWait);
     
     {Destroy Storage}
     StorageDeviceDestroy(@Storage.Storage);
       
     {Return Result}
     Result:=USB_STATUS_DEVICE_UNSUPPORTED;
     Exit;
    end;
  end; 
 
 {Allocate Read Request}
 Storage.ReadRequest:=USBRequestAllocate(Device,Storage.ReadEndpoint,USBStorageReadComplete,0,Storage);
 if Storage.ReadRequest = nil then
  begin
   if USB_LOG_ENABLED then USBLogError(Device,'Storage: Failed to allocate storage device read request');
   
   {Destroy Read Semaphore}
   SemaphoreDestroy(Storage.ReadWait);

   {Destroy Write Semaphore}
   SemaphoreDestroy(Storage.WriteWait);
   
   {Destroy Interrupt Semaphore}
   if Storage.InterruptEndpoint <> nil then SemaphoreDestroy(Storage.InterruptWait);
   
   {Destroy Storage}
   StorageDeviceDestroy(@Storage.Storage);
   
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;
  
 {Allocate Write Request}
 Storage.WriteRequest:=USBRequestAllocate(Device,Storage.WriteEndpoint,USBStorageWriteComplete,0,Storage);
 if Storage.WriteRequest = nil then
  begin
   if USB_LOG_ENABLED then USBLogError(Device,'Storage: Failed to allocate storage device write request');
   
   {Release Read Request}
   USBRequestRelease(Storage.ReadRequest);

   {Destroy Read Semaphore}
   SemaphoreDestroy(Storage.ReadWait);

   {Destroy Write Semaphore}
   SemaphoreDestroy(Storage.WriteWait);
   
   {Destroy Interrupt Semaphore}
   if Storage.InterruptEndpoint <> nil then SemaphoreDestroy(Storage.InterruptWait);
   
   {Destroy Storage}
   StorageDeviceDestroy(@Storage.Storage);
   
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;
  
 {Allocate Interrupt Request}
 if Storage.InterruptEndpoint <> nil then
  begin
   //InterruptRequest //To Do //For CBI interface
  end; 
 
 {Update Storage}
 {Driver (BlockSize/BlockCount/BlockShift/Vendor/Product)}
 Status:=USBStorageDeviceGetInfo(Device,Storage);
 if Status <> USB_STATUS_SUCCESS then
  begin
   if USB_LOG_ENABLED then USBLogError(Device,'Storage: Failed to get storage device info');
  
   {Release Read Request}
   USBRequestRelease(Storage.ReadRequest);
   
   {Release Write Request}
   USBRequestRelease(Storage.WriteRequest);
   
   {Destroy Read Semaphore}
   SemaphoreDestroy(Storage.ReadWait);
   
   {Destroy Write Semaphore}
   SemaphoreDestroy(Storage.WriteWait);
   
   {Destroy Interrupt Semaphore}
   if Storage.InterruptEndpoint <> nil then SemaphoreDestroy(Storage.InterruptWait);
   
   {Destroy Storage}
   StorageDeviceDestroy(@Storage.Storage);
   
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;
                       
 {Register Storage} 
 if StorageDeviceRegister(@Storage.Storage) <> ERROR_SUCCESS then
  begin
   if USB_LOG_ENABLED then USBLogError(Device,'Storage: Failed to register new storage device');
  
   {Release Read Request}
   USBRequestRelease(Storage.ReadRequest);
   
   {Release Write Request}
   USBRequestRelease(Storage.WriteRequest);

   {Destroy Read Semaphore}
   SemaphoreDestroy(Storage.ReadWait);

   {Destroy Write Semaphore}
   SemaphoreDestroy(Storage.WriteWait);
   
   {Destroy Interrupt Semaphore}
   if Storage.InterruptEndpoint <> nil then SemaphoreDestroy(Storage.InterruptWait);
   
   {Destroy Storage}
   StorageDeviceDestroy(@Storage.Storage);
   
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;
 
 {Update Interface}
 Interrface.DriverData:=Storage;
 
 {Check Flags}
 if (Storage.Storage.Device.DeviceFlags and (STORAGE_FLAG_NOT_READY or STORAGE_FLAG_NO_MEDIA)) = 0 then
  begin
   {Set State to Inserted}
   if StorageDeviceSetState(@Storage.Storage,STORAGE_STATE_INSERTED) <> ERROR_SUCCESS then
    begin
     if USB_LOG_ENABLED then USBLogError(Device,'Storage: Failed to set state for new storage device');
    end;
  end; 
  
 {Start Status Checking}
 if StorageDeviceStartStatus(@Storage.Storage,STORAGE_STATUS_TIMER_INTERVAL) <> ERROR_SUCCESS then
  begin
   if USB_LOG_ENABLED then USBLogError(Device,'Storage: Failed start status for new storage device');
  end;
  
 {Check Max LUN}
 if Storage.MaxLUN > 0 then
  begin
   {Setup LUN}
   LUN:=1;
   MaxLUN:=Storage.MaxLUN;

   {Save Current}
   Current:=Storage;

   {Enumerate Remaining LUNs}
   while LUN <= MaxLUN do
    begin
     {Create Storage}
     Storage:=PUSBStorageDevice(StorageDeviceCreateEx(SizeOf(TUSBStorageDevice)));
     if Storage = nil then
      begin
       if USB_LOG_ENABLED then USBLogError(Device,'Storage: Failed to create new storage device (LUN=' + IntToStr(LUN) + ')');
       
       Break; {Do not fail the primary LUN}
      end;
     
     {Update Storage} 
     {Device}
     Storage.Storage.Device.DeviceBus:=DEVICE_BUS_USB;
     Storage.Storage.Device.DeviceType:=STORAGE_TYPE_HDD;
     Storage.Storage.Device.DeviceFlags:=STORAGE_FLAG_NONE;
     Storage.Storage.Device.DeviceData:=Device;
     {Storage}
     Storage.Storage.StorageState:=STORAGE_STATE_EJECTED;
     Storage.Storage.DeviceRead:=USBStorageDeviceRead;
     Storage.Storage.DeviceWrite:=USBStorageDeviceWrite;
     Storage.Storage.DeviceErase:=nil;
     Storage.Storage.DeviceControl:=USBStorageDeviceControl;
     {Driver}
     Storage.Storage.TargetID:=0;
     Storage.Storage.TargetLUN:=LUN;
     Storage.Storage.BlockSize:=0;
     Storage.Storage.BlockCount:=0;
     Storage.Storage.BlockShift:=0;
     {USB}
     Storage.Subclass:=Current.Subclass;
     Storage.Protocol:=Current.Protocol;
     Storage.MaxLUN:=Current.MaxLUN;
     Storage.Primary:=Current;
     Storage.StorageInterface:=Current.StorageInterface;
     Storage.ReadWait:=INVALID_HANDLE_VALUE;
     Storage.WriteWait:=INVALID_HANDLE_VALUE;
     Storage.InterruptWait:=INVALID_HANDLE_VALUE;
     Storage.ReadEndpoint:=Current.ReadEndpoint;
     Storage.WriteEndpoint:=Current.WriteEndpoint;
     Storage.InterruptEndpoint:=Current.InterruptEndpoint;
     Storage.WaiterThread:=INVALID_HANDLE_VALUE;
     
     {Create Read Semaphore}
     Storage.ReadWait:=SemaphoreCreate(0);
     if Storage.ReadWait = INVALID_HANDLE_VALUE then
      begin
       if USB_LOG_ENABLED then USBLogError(Device,'Storage: Failed to allocate storage device read semaphore (LUN=' + IntToStr(LUN) + ')');
   
       {Destroy Storage}
       StorageDeviceDestroy(@Storage.Storage);
     
       Break; {Do not fail the primary LUN}
      end;
     
     {Create Write Semaphore}
     Storage.WriteWait:=SemaphoreCreate(0);
     if Storage.WriteWait = INVALID_HANDLE_VALUE then
      begin
       if USB_LOG_ENABLED then USBLogError(Device,'Storage: Failed to allocate storage device write semaphore (LUN=' + IntToStr(LUN) + ')');
   
       {Destroy Read Semaphore}
       SemaphoreDestroy(Storage.ReadWait);
   
       {Destroy Storage}
       StorageDeviceDestroy(@Storage.Storage);
     
       Break; {Do not fail the primary LUN}
      end;
     
     {Create Interrupt Semaphore} 
     if Storage.InterruptEndpoint <> nil then
      begin
       //InterruptWait //To Do //For CBI interface
      end; 
     
     {Allocate Read Request}
     Storage.ReadRequest:=USBRequestAllocate(Device,Storage.ReadEndpoint,USBStorageReadComplete,0,Storage);
     if Storage.ReadRequest = nil then
      begin
       if USB_LOG_ENABLED then USBLogError(Device,'Storage: Failed to allocate storage device read request (LUN=' + IntToStr(LUN) + ')');
       
       {Destroy Read Semaphore}
       SemaphoreDestroy(Storage.ReadWait);

       {Destroy Write Semaphore}
       SemaphoreDestroy(Storage.WriteWait);
       
       {Destroy Storage}
       StorageDeviceDestroy(@Storage.Storage);
       
       Break; {Do not fail the primary LUN}
      end;
     
     {Allocate Write Request}
     Storage.WriteRequest:=USBRequestAllocate(Device,Storage.WriteEndpoint,USBStorageWriteComplete,0,Storage);
     if Storage.WriteRequest = nil then
      begin
       if USB_LOG_ENABLED then USBLogError(Device,'Storage: Failed to allocate storage device write request (LUN=' + IntToStr(LUN) + ')');
        
       {Release Read Request}
       USBRequestRelease(Storage.ReadRequest);

       {Destroy Read Semaphore}
       SemaphoreDestroy(Storage.ReadWait);

       {Destroy Write Semaphore}
       SemaphoreDestroy(Storage.WriteWait);
        
       {Destroy Storage}
       StorageDeviceDestroy(@Storage.Storage);
        
       Break; {Do not fail the primary LUN}
      end;
     
     {Allocate Interrupt Request}
     if Storage.InterruptEndpoint <> nil then
      begin
       //InterruptRequest //To Do //For CBI interface
      end; 
    
     {Update Storage}
     {Driver (BlockSize/BlockCount/BlockShift/Vendor/Product)}
     Status:=USBStorageDeviceGetInfo(Device,Storage);
     if Status <> USB_STATUS_SUCCESS then
      begin
       if USB_LOG_ENABLED then USBLogError(Device,'Storage: Failed to get storage device info (LUN=' + IntToStr(LUN) + ')');
       
       {Release Read Request}
       USBRequestRelease(Storage.ReadRequest);
       
       {Release Write Request}
       USBRequestRelease(Storage.WriteRequest);
       
       {Destroy Read Semaphore}
       SemaphoreDestroy(Storage.ReadWait);

       {Destroy Write Semaphore}
       SemaphoreDestroy(Storage.WriteWait);
       
       {Destroy Storage}
       StorageDeviceDestroy(@Storage.Storage);
       
       Break; {Do not fail the primary LUN}
      end;
    
     {Register Storage} 
     if StorageDeviceRegister(@Storage.Storage) <> ERROR_SUCCESS then
      begin
       if USB_LOG_ENABLED then USBLogError(Device,'Storage: Failed to register new storage device (LUN=' + IntToStr(LUN) + ')');
       
       {Release Read Request}
       USBRequestRelease(Storage.ReadRequest);
       
       {Release Write Request}
       USBRequestRelease(Storage.WriteRequest);
       
       {Destroy Read Semaphore}
       SemaphoreDestroy(Storage.ReadWait);

       {Destroy Write Semaphore}
       SemaphoreDestroy(Storage.WriteWait);
       
       {Destroy Storage}
       StorageDeviceDestroy(@Storage.Storage);
       
       Break; {Do not fail the primary LUN}
      end;
 
     {Update Interface}
     {Interrface.DriverData:=Storage;} {Only on primary device}
 
     {Check Flags}
     if (Storage.Storage.Device.DeviceFlags and (STORAGE_FLAG_NOT_READY or STORAGE_FLAG_NO_MEDIA)) = 0 then
      begin
       {Set State to Inserted}
       if StorageDeviceSetState(@Storage.Storage,STORAGE_STATE_INSERTED) <> ERROR_SUCCESS then
        begin
         if USB_LOG_ENABLED then USBLogError(Device,'Storage: Failed to set state for new storage device (LUN=' + IntToStr(LUN) + ')');
        end;
      end; 
      
     {Start Status Checking}
     if StorageDeviceStartStatus(@Storage.Storage,STORAGE_STATUS_TIMER_INTERVAL) <> ERROR_SUCCESS then
      begin
       if USB_LOG_ENABLED then USBLogError(Device,'Storage: Failed start status for new storage device (LUN=' + IntToStr(LUN) + ')');
      end;
     
     {Increment LUN}
     Inc(LUN);
    end;
  end; 
  
 {Return Result} 
 Result:=USB_STATUS_SUCCESS;
end;

{==============================================================================}

function USBStorageDriverUnbind(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;
{Unbind the Storage driver from a USB device}
{Device: The USB device to unbind from}
{Interrface: The USB interface to unbind from (or nil for whole device)}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}
var
 Message:TMessage;
 Storage:PUSBStorageDevice;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Device}
 if Device = nil then Exit;

 {Check Interface}
 if Interrface = nil then Exit;
 
 {Check Driver}
 if Interrface.Driver <> USBStorageDriver then Exit;
 
 {$IFDEF USB_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(Device,'Storage: Unbinding (' + ': Address ' + IntToStr(Device.Address) + ')'); //To Do //Device.Manufacturer //Device.Product
 {$ENDIF}
 
 {Get Storage}
 Storage:=PUSBStorageDevice(Interrface.DriverData);
 if Storage = nil then Exit;
 if Storage.Storage.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 //To Do //Check MaxLUN //Find Primary etc
 
 {Set State to Ejecting}
 Result:=USB_STATUS_OPERATION_FAILED;
 if StorageDeviceSetState(@Storage.Storage,STORAGE_STATE_EJECTING) <> ERROR_SUCCESS then Exit;

 {Acquire the Lock}
 if MutexLock(Storage.Storage.Lock) <> ERROR_SUCCESS then Exit;
 
 {Check Pending}
 if Storage.PendingCount <> 0 then
  begin
   {$IFDEF USB_DEBUG}
   if USB_LOG_ENABLED then USBLogDebug(Device,'Storage: Waiting for ' + IntToStr(Storage.PendingCount) + ' pending requests to complete');
   {$ENDIF}

   {Wait for Pending}
 
   {Setup Waiter}
   Storage.WaiterThread:=GetCurrentThreadId; 
   
   {Release the Lock}
   MutexUnlock(Storage.Storage.Lock);
   
   {Wait for Message}
   ThreadReceiveMessage(Message); 
  end
 else
  begin
   {Release the Lock}
   MutexUnlock(Storage.Storage.Lock);
  end;  
  
 {Set State to Ejected}
 if StorageDeviceSetState(@Storage.Storage,STORAGE_STATE_EJECTED) <> ERROR_SUCCESS then Exit;
 
 {Update Interface}
 Interrface.DriverData:=nil;

 //To Do //Detach and Deregister all Storage devices if MaxLUN > 0 //Do this prior to ejecting the primary storage device
 
 //To Do //Free Requests/Semaphores etc //See Mouse/Keyboard
 
 {Deregister Storage}
 if StorageDeviceDeregister(@Storage.Storage) <> ERROR_SUCCESS then Exit;
 
 {Destroy Storage}
 StorageDeviceDestroy(@Storage.Storage);
 
 {Return Result} 
 Result:=USB_STATUS_SUCCESS;
end;

{==============================================================================}

procedure USBStorageReadComplete(Request:PUSBRequest);
{Called when a USB request from a USB storage bulk IN endpoint completes}
{Request: The USB request which has completed}
{Note: The thread that submitted the read request will hold the storage lock}
var
 Message:TMessage;
 Storage:PUSBStorageDevice;
begin
 {}
 {Check Request}
 if Request = nil then Exit;
 
 {Get Storage}
 Storage:=PUSBStorageDevice(Request.DriverData);
 if Storage <> nil then
  begin
   {Update Pending (Before signal as the submitter has the lock)}
   Dec(Storage.PendingCount);
  
   {Check State}
   if Storage.Storage.StorageState = STORAGE_STATE_EJECTING then
    begin
     {$IFDEF USB_DEBUG}
     if USB_LOG_ENABLED then USBLogDebug(Request.Device,'Storage: Eject pending, setting read request status to USB_STATUS_DEVICE_DETACHED');
     {$ENDIF}
      
     {Update Request}
     Request.Status:=USB_STATUS_DEVICE_DETACHED;
    end;
 
   {Signal Semaphore}
   SemaphoreSignal(Storage.ReadWait);
   
   {Check State}
   if Storage.Storage.StorageState = STORAGE_STATE_EJECTING then
    begin
     {Check Pending}
     if Storage.PendingCount = 0 then
      begin
       {Check Waiter}
       if Storage.WaiterThread <> INVALID_HANDLE_VALUE then
        begin
         {$IFDEF USB_DEBUG}
         if USB_LOG_ENABLED then USBLogDebug(Request.Device,'Storage: Eject pending, sending message to waiter thread (Thread=' + IntToHex(Storage.WaiterThread,8) + ')');
         {$ENDIF}
            
         {Send Message}
         FillChar(Message,SizeOf(TMessage),0);
         ThreadSendMessage(Storage.WaiterThread,Message);
         Storage.WaiterThread:=INVALID_HANDLE_VALUE;
        end; 
      end;
    end;
  end
 else
  begin
   if USB_LOG_ENABLED then USBLogError(Request.Device,'Storage: Read request invalid');
  end;    
end;

{==============================================================================}

procedure USBStorageWriteComplete(Request:PUSBRequest);
{Called when a USB request from a USB storage bulk OUT endpoint completes}
{Request: The USB request which has completed}
{Note: The thread that submitted the write request will hold the storage lock}
var
 Message:TMessage;
 Storage:PUSBStorageDevice;
begin
 {}
 {Check Request}
 if Request = nil then Exit;
 
 {Get Storage}
 Storage:=PUSBStorageDevice(Request.DriverData);
 if Storage <> nil then
  begin
   {Update Pending (Before signal as the submitter has the lock)}
   Dec(Storage.PendingCount);

   {Check State}
   if Storage.Storage.StorageState = STORAGE_STATE_EJECTING then
    begin
     {$IFDEF USB_DEBUG}
     if USB_LOG_ENABLED then USBLogDebug(Request.Device,'Storage: Eject pending, setting write request status to USB_STATUS_DEVICE_DETACHED');
     {$ENDIF}
      
     {Update Request}
     Request.Status:=USB_STATUS_DEVICE_DETACHED;
    end;
 
   {Signal Semaphore}
   SemaphoreSignal(Storage.WriteWait);
   
   {Check State}
   if Storage.Storage.StorageState = STORAGE_STATE_EJECTING then
    begin
     {Check Pending}
     if Storage.PendingCount = 0 then
      begin
       {Check Waiter}
       if Storage.WaiterThread <> INVALID_HANDLE_VALUE then
        begin
         {$IFDEF USB_DEBUG}
         if USB_LOG_ENABLED then USBLogDebug(Request.Device,'Storage: Eject pending, sending message to waiter thread (Thread=' + IntToHex(Storage.WaiterThread,8) + ')');
         {$ENDIF}
            
         {Send Message}
         FillChar(Message,SizeOf(TMessage),0);
         ThreadSendMessage(Storage.WaiterThread,Message);
         Storage.WaiterThread:=INVALID_HANDLE_VALUE;
        end; 
      end;
    end;
  end
 else
  begin
   if USB_LOG_ENABLED then USBLogError(Request.Device,'Storage: Write request invalid');
  end;    
end;

{==============================================================================}

procedure USBStorageInterruptComplete(Request:PUSBRequest);
{Called when a USB request from a USB storage IN interrupt endpoint completes}
{Request: The USB request which has completed}
{Note: The thread that submitted the interrupt request will hold the storage lock}
var
 Message:TMessage;
 Storage:PUSBStorageDevice;
begin
 {}
 {Check Request}
 if Request = nil then Exit;
 
 {Get Storage}
 Storage:=PUSBStorageDevice(Request.DriverData);
 if Storage <> nil then
  begin
   {Update Pending (Before signal as the submitter has the lock)}
   Dec(Storage.PendingCount);
 
   {Check State}
   if Storage.Storage.StorageState = STORAGE_STATE_EJECTING then
    begin
     {$IFDEF USB_DEBUG}
     if USB_LOG_ENABLED then USBLogDebug(Request.Device,'Storage: Eject pending, setting interrupt request status to USB_STATUS_DEVICE_DETACHED');
     {$ENDIF}
      
     {Update Request}
     Request.Status:=USB_STATUS_DEVICE_DETACHED;
    end;
 
   {Signal Semaphore}
   SemaphoreSignal(Storage.InterruptWait);

   {Check State}
   if Storage.Storage.StorageState = STORAGE_STATE_EJECTING then
    begin
     {Check Pending}
     if Storage.PendingCount = 0 then
      begin
       {Check Waiter}
       if Storage.WaiterThread <> INVALID_HANDLE_VALUE then
        begin
         {$IFDEF USB_DEBUG}
         if USB_LOG_ENABLED then USBLogDebug(Request.Device,'Storage: Eject pending, sending message to waiter thread (Thread=' + IntToHex(Storage.WaiterThread,8) + ')');
         {$ENDIF}
            
         {Send Message}
         FillChar(Message,SizeOf(TMessage),0);
         ThreadSendMessage(Storage.WaiterThread,Message);
         Storage.WaiterThread:=INVALID_HANDLE_VALUE;
        end; 
      end;
    end;
  end
 else
  begin
   if USB_LOG_ENABLED then USBLogError(Request.Device,'Storage: Interrupt request invalid');
  end;    
end;

{==============================================================================}
{==============================================================================}
{Storage Helper Functions}
function StorageGetCount:LongWord; inline;
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

procedure StorageLogInfo(Storage:PStorageDevice;const AText:String);
begin
 {}
 StorageLog(STORAGE_LOG_LEVEL_INFO,Storage,AText);
end;

{==============================================================================}

procedure StorageLogError(Storage:PStorageDevice;const AText:String);
begin
 {}
 StorageLog(STORAGE_LOG_LEVEL_ERROR,Storage,AText);
end;

{==============================================================================}

procedure StorageLogDebug(Storage:PStorageDevice;const AText:String);
begin
 {}
 StorageLog(STORAGE_LOG_LEVEL_DEBUG,Storage,AText);
end;

{==============================================================================}

procedure StorageStatusTimer(Storage:PStorageDevice);
var
 Argument:LongWord;
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
{USB Storage Helper Functions}
function USBStorageCheckSubclass(Subclass:Byte):Boolean;
begin
 {}
 Result:=False;
 
 {Check Subclass}
 case Subclass of
  USB_SUBCLASS_MASS_STORAGE_UFI:Result:=True;
  USB_SUBCLASS_MASS_STORAGE_SFF8070I:Result:=True;
  USB_SUBCLASS_MASS_STORAGE_SCSI:Result:=True;
  //To Do //Probably need to support USB_SUBCLASS_MASS_STORAGE_RBC and USB_SUBCLASS_MASS_STORAGE_MMC5
                //Also Check USB_SUBCLASS_MASS_STORAGE_DEFAULT
 end;
end;

{==============================================================================}

function USBStorageCheckProtocol(Protocol:Byte):Boolean;
begin
 {}
 Result:=False;
 
 {Check Protocol}
 case Protocol of
  USB_PROTOCOL_MASS_STORAGE_CBI:Result:=True;
  USB_PROTOCOL_MASS_STORAGE_CB:Result:=True;
  USB_PROTOCOL_MASS_STORAGE_BBB:Result:=True;
 end;
end;

{==============================================================================}

function USBStorageBlockSizeToBlockShift(BlockSize:LongWord):LongWord;
begin
 {}
 Result:=0;
 
 {Check Block Size}
 if BlockSize = 0 then Exit;
 
 {Calculate Shift}
 while (1 shl Result) < BlockSize do
  begin
   Inc(Result);
  end;
  
 {Check Result} 
 if (1 shl Result) <> BlockSize then Result:=0;
end;

{==============================================================================}

function USBStoragePatchDevice(Device:PUSBDevice;var Subclass,Protocol:Byte):LongWord;
{A function to adjust the subclass and protocol parameters of known defective devices}
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Device}
 if Device = nil then Exit;

 {Setup Defaults}
 Subclass:=USB_SUBCLASS_MASS_STORAGE_DEFAULT;
 Protocol:=USB_PROTOCOL_MASS_STORAGE_CBI;
 
 {Shuttle Technology Inc / E-USB Bridge}
 if (Device.Descriptor.idVendor = $066b) and (Device.Descriptor.idProduct = $0103) then
  begin
   Subclass:=USB_SUBCLASS_MASS_STORAGE_UFI;
   Protocol:=USB_PROTOCOL_MASS_STORAGE_CB;
   
   {Return Result}
   Result:=USB_STATUS_SUCCESS;
   Exit;
  end;
 
 {Add Others Here}
 
 {Return Result}
 Result:=USB_STATUS_DEVICE_UNSUPPORTED;
end;

{==============================================================================}

function USBStorageFixupDevice(Device:PUSBDevice;var Vendor,Product:String):LongWord;
{A function to adjust the vendor and product parameters of known defective devices}
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Device}
 if Device = nil then Exit;

 {Setup Defaults}
 Vendor:='';
 Product:='';
 
 {SMSC / Flash Media Controller}
 if (Device.Descriptor.idVendor = $0424) and (Device.Descriptor.idProduct = $223a) then
  begin
   Vendor:='SMSC';
   Product:='Flash Media';
   
   {Return Result}
   Result:=USB_STATUS_SUCCESS;
   Exit;
  end;
 
 {Add Others Here}
 
 {Return Result}
 Result:=USB_STATUS_DEVICE_UNSUPPORTED;
end; 

{==============================================================================}

function USBStorageDeviceLock(Device:PUSBDevice;Storage:PUSBStorageDevice):LongWord;
{Issue a Prevent Media Removal request to a storage device}
var
 Status:LongWord;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Device}
 if Device = nil then Exit;

 {Check Storage}
 if Storage = nil then Exit;
 
 {$IFDEF STORAGE_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(Device,'USBStorageDeviceLock'); 
 {$ENDIF}
 
 FillChar(Storage.CommandBlock.Command[0],SizeOf(Storage.CommandBlock.Command),0);
 Storage.CommandBlock.Command[0]:=SCSI_COMMAND_MED_REMOVL;           {Command}
 Storage.CommandBlock.Command[1]:=(Storage.Storage.TargetLUN shl 5); {LUN}
 Storage.CommandBlock.Command[4]:=1;                                 {Prevent Media Removal}
 Storage.CommandBlock.CommandLength:=12;
 Storage.CommandBlock.DataLength:=0;
 Storage.CommandBlock.Data:=nil;
 Storage.CommandBlock.Direction:=USB_DIRECTION_OUT;
 Status:=USBStorageDeviceTransport(Device,Storage,@Storage.CommandBlock);
 if Status = USB_STATUS_SUCCESS then
  begin
   {Return Result}
   Result:=USB_STATUS_SUCCESS;
   Exit;
  end;   
   
 {Return Result}
 Result:=USB_STATUS_HARDWARE_ERROR;
end;

{==============================================================================}

function USBStorageDeviceUnlock(Device:PUSBDevice;Storage:PUSBStorageDevice):LongWord;
{Issue a Allow Media Removal request to a storage device}
var
 Status:LongWord;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Device}
 if Device = nil then Exit;

 {Check Storage}
 if Storage = nil then Exit;
 
 {$IFDEF STORAGE_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(Device,'USBStorageDeviceUnlock'); 
 {$ENDIF}
 
 FillChar(Storage.CommandBlock.Command[0],SizeOf(Storage.CommandBlock.Command),0);
 Storage.CommandBlock.Command[0]:=SCSI_COMMAND_MED_REMOVL;           {Command}
 Storage.CommandBlock.Command[1]:=(Storage.Storage.TargetLUN shl 5); {LUN}
 Storage.CommandBlock.Command[4]:=0;                                 {Allow Media Removal}
 Storage.CommandBlock.CommandLength:=12;
 Storage.CommandBlock.DataLength:=0;
 Storage.CommandBlock.Data:=nil;
 Storage.CommandBlock.Direction:=USB_DIRECTION_OUT;
 Status:=USBStorageDeviceTransport(Device,Storage,@Storage.CommandBlock);
 if Status = USB_STATUS_SUCCESS then
  begin
   {Return Result}
   Result:=USB_STATUS_SUCCESS;
   Exit;
  end;   
   
 {Return Result}
 Result:=USB_STATUS_HARDWARE_ERROR;
end;

{==============================================================================}

function USBStorageDeviceLoad(Device:PUSBDevice;Storage:PUSBStorageDevice):LongWord;
{Issue a Start Stop request to a storage device}
var
 Status:LongWord;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Device}
 if Device = nil then Exit;

 {Check Storage}
 if Storage = nil then Exit;
 
 {$IFDEF STORAGE_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(Device,'USBStorageDeviceLoad'); 
 {$ENDIF}
 
 FillChar(Storage.CommandBlock.Command[0],SizeOf(Storage.CommandBlock.Command),0);
 Storage.CommandBlock.Command[0]:=SCSI_COMMAND_START_STP;            {Command}
 Storage.CommandBlock.Command[1]:=(Storage.Storage.TargetLUN shl 5); {LUN}
 Storage.CommandBlock.Command[4]:=3;                                 {LoEj = 1 / Start = 1 (Load)}
 Storage.CommandBlock.CommandLength:=12;
 Storage.CommandBlock.DataLength:=0;
 Storage.CommandBlock.Data:=nil;
 Storage.CommandBlock.Direction:=USB_DIRECTION_OUT;
 Status:=USBStorageDeviceTransport(Device,Storage,@Storage.CommandBlock);
 if Status = USB_STATUS_SUCCESS then
  begin
   {Return Result}
   Result:=USB_STATUS_SUCCESS;
   Exit;
  end;   
   
 {Return Result}
 Result:=USB_STATUS_HARDWARE_ERROR;
end;

{==============================================================================}

function USBStorageDeviceEject(Device:PUSBDevice;Storage:PUSBStorageDevice):LongWord;
{Issue a Start Stop request to a storage device}
var
 Status:LongWord;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Device}
 if Device = nil then Exit;

 {Check Storage}
 if Storage = nil then Exit;
 
 {$IFDEF STORAGE_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(Device,'USBStorageDeviceEject'); 
 {$ENDIF}
 
 FillChar(Storage.CommandBlock.Command[0],SizeOf(Storage.CommandBlock.Command),0);
 Storage.CommandBlock.Command[0]:=SCSI_COMMAND_START_STP;            {Command}
 Storage.CommandBlock.Command[1]:=(Storage.Storage.TargetLUN shl 5); {LUN}
 Storage.CommandBlock.Command[4]:=2;                                 {LoEj = 1 / Start = 0 (Eject)}
 Storage.CommandBlock.CommandLength:=12;
 Storage.CommandBlock.DataLength:=0;
 Storage.CommandBlock.Data:=nil;
 Storage.CommandBlock.Direction:=USB_DIRECTION_OUT;
 Status:=USBStorageDeviceTransport(Device,Storage,@Storage.CommandBlock);
 if Status = USB_STATUS_SUCCESS then
  begin
   {Return Result}
   Result:=USB_STATUS_SUCCESS;
   Exit;
  end;   
   
 {Return Result}
 Result:=USB_STATUS_HARDWARE_ERROR;
end;

{==============================================================================}

function USBStorageDeviceReset(Device:PUSBDevice;Storage:PUSBStorageDevice):LongWord;
{Issue a Storage Reset request to a storage device}
{Reset recovery (5.3.4 in USB Mass Storage Bulk Only 1.0)

 For Reset Recovery the host shall issue in the following order:
 a) a Bulk-Only Mass Storage Reset
 b) a Clear Feature HALT to the Bulk-In endpoint
 c) a Clear Feature HALT to the Bulk-Out endpoint

 This is done in 3 steps
 
 If the reset doesn't succeed, the device should be port reset}
var
 Status:LongWord; 
 Command:array[0..11] of Byte;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Device}
 if Device = nil then Exit;

 {Check Storage}
 if Storage = nil then Exit;
 
 {$IFDEF STORAGE_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(Device,'USBStorageDeviceReset');
 {$ENDIF}
 
 {Check Protocol}
 case Storage.Protocol of
  USB_PROTOCOL_MASS_STORAGE_CBI,USB_PROTOCOL_MASS_STORAGE_CB:begin
    {Control/Bulk/Interrupt Request}
    {Send ADSC Message (Command Block Reset - See Section 4.14 of Mass Storage UFI Command Specification 1.0)}
    FillChar(Command[0],SizeOf(Command),$00); {Was $FF}
    Command[0]:=SCSI_COMMAND_SEND_DIAG;
    Command[1]:=$04;
    Status:=USBControlRequest(Device,nil,USB_STORAGE_REQUEST_CBI_ADSC,USB_BMREQUESTTYPE_DIR_OUT or USB_BMREQUESTTYPE_TYPE_CLASS or USB_BMREQUESTTYPE_RECIPIENT_INTERFACE,0,Storage.StorageInterface.Descriptor.bInterfaceNumber,@Command,SizeOf(Command));
    if Status <> USB_STATUS_SUCCESS then
     begin
      if USB_LOG_ENABLED then USBLogError(Device,'USBStorageDeviceReset - Control request failure (Status=' + USBStatusToString(Status) + ')');
      
      //To Do //Check for STALL ?
      
      {Return Result}
      Result:=Status;
      Exit;
     end;
    
    {Wait for Reset}
    MillisecondDelay(1500);
    
    {Clear Feature HALT on Bulk IN Endpoint}
    Status:=USBDeviceClearFeature(Device,Storage.ReadEndpoint,USB_DEVICE_FEATURE_ENDPOINT_HALT);
    if Status <> USB_STATUS_SUCCESS then
     begin
      if USB_LOG_ENABLED then USBLogError(Device,'USBStorageDeviceReset - Clear feature (IN) failure (Status=' + USBStatusToString(Status) + ')');
      
      {Return Result}
      Result:=Status;
      Exit;
     end;
    
    {Wait for Reset}
    MillisecondDelay(150);
    
    {Clear Feature HALT on Bulk OUT Endpoint}
    Status:=USBDeviceClearFeature(Device,Storage.WriteEndpoint,USB_DEVICE_FEATURE_ENDPOINT_HALT);
    if Status <> USB_STATUS_SUCCESS then
     begin
      if USB_LOG_ENABLED then USBLogError(Device,'USBStorageDeviceReset - Clear feature (OUT) failure (Status=' + USBStatusToString(Status) + ')');
      
      {Return Result}
      Result:=Status;
      Exit;
     end;
     
    {Wait for Reset}
    MillisecondDelay(150);
    
    {Return Result}
    Result:=USB_STATUS_SUCCESS;
   end;
  USB_PROTOCOL_MASS_STORAGE_BBB:begin
    {Bulk Only Request}
    {Send Bulk Only Reset}
    Status:=USBControlRequest(Device,nil,USB_STORAGE_REQUEST_BBB_RESET,USB_BMREQUESTTYPE_DIR_OUT or USB_BMREQUESTTYPE_TYPE_CLASS or USB_BMREQUESTTYPE_RECIPIENT_INTERFACE,0,Storage.StorageInterface.Descriptor.bInterfaceNumber,nil,0);
    if Status <> USB_STATUS_SUCCESS then
     begin
      if USB_LOG_ENABLED then USBLogError(Device,'USBStorageDeviceReset - Control request failure (Status=' + USBStatusToString(Status) + ')');
      
      //To Do //Check for STALL ?
      
      {Return Result}
      Result:=Status;
      Exit;
     end;
    
    {Wait for Reset}
    MillisecondDelay(150);
    
    {Clear Feature HALT on Bulk IN Endpoint}
    Status:=USBDeviceClearFeature(Device,Storage.ReadEndpoint,USB_DEVICE_FEATURE_ENDPOINT_HALT);
    if Status <> USB_STATUS_SUCCESS then
     begin
      if USB_LOG_ENABLED then USBLogError(Device,'USBStorageDeviceReset - Clear feature (IN) failure (Status=' + USBStatusToString(Status) + ')');
      
      {Return Result}
      Result:=Status;
      Exit;
     end;
     
    {Wait for Reset}
    MillisecondDelay(150);
    
    {Clear Feature HALT on Bulk OUT Endpoint}
    Status:=USBDeviceClearFeature(Device,Storage.WriteEndpoint,USB_DEVICE_FEATURE_ENDPOINT_HALT);
    if Status <> USB_STATUS_SUCCESS then
     begin
      if USB_LOG_ENABLED then USBLogError(Device,'USBStorageDeviceReset - Clear feature (OUT) failure (Status=' + USBStatusToString(Status) + ')');
      
      {Return Result}
      Result:=Status;
      Exit;
     end;
     
    {Wait for Reset}
    MillisecondDelay(150);
    
    {Return Result}
    Result:=USB_STATUS_SUCCESS;
   end;
  else
   begin
    {Error Return}
    Result:=USB_STATUS_DEVICE_UNSUPPORTED; 
   end;   
 end;
end; 

{==============================================================================}

function USBStorageDeviceGetInfo(Device:PUSBDevice;Storage:PUSBStorageDevice):LongWord; 
{Obtain basic information about a storage device (Type, Size, State, Product etc)}
var
 Status:LongWord;
 Vendor:String;
 Product:String;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Device}
 if Device = nil then Exit;

 {Check Storage}
 if Storage = nil then Exit;
 
 {$IFDEF STORAGE_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(Device,'USBStorageDeviceGetInfo');
 {$ENDIF}
 
 {Check Subclass}
 case Storage.Subclass of
  USB_SUBCLASS_MASS_STORAGE_UFI,USB_SUBCLASS_MASS_STORAGE_SFF8070I,USB_SUBCLASS_MASS_STORAGE_SCSI:begin
    {Inquiry}
    Status:=USBStorageDeviceInquiry(Device,Storage,Storage.Storage.Device.DeviceType,Storage.Storage.Device.DeviceFlags,Storage.Storage.Vendor,Storage.Storage.Product,Storage.Storage.Revision);
    if Status <> USB_STATUS_SUCCESS then
     begin
      {Return Result}
      Result:=Status;
      Exit;
     end; 
    
    {Fixup}
    Status:=USBStorageFixupDevice(Device,Vendor,Product);
    if Status = USB_STATUS_SUCCESS then
     begin
      {Fixup Vendor}
      StrLCopy(Storage.Storage.Vendor,PChar(Vendor),8); {Length from TSCSIStandardInquiryData}
      {Fixup Product}
      StrLCopy(Storage.Storage.Product,PChar(Product),16); {Length from TSCSIStandardInquiryData}
     end;
    
    {Check Type}
    case Storage.Storage.Device.DeviceType of
     STORAGE_TYPE_NONE:begin
       {Error Return}
       Result:=USB_STATUS_DEVICE_UNSUPPORTED;
      end;
     STORAGE_TYPE_HDD,STORAGE_TYPE_REMOVABLE:begin
       {Fixed or Removable}
       {Test Unit Ready}
       Status:=USBStorageDeviceTestUnitReady(Device,Storage,Storage.Storage.Device.DeviceFlags);
       if Status <> USB_STATUS_SUCCESS then
        begin
         {Return Result}
         {Result:=Status;} {Continue if Test Unit Ready fails}
         {Exit;}
        end; 
        
       {Read Capacity}
       Status:=USBStorageDeviceReadCapacity(Device,Storage,Storage.Storage.BlockSize,Storage.Storage.BlockShift,Storage.Storage.BlockCount);
       if Status <> USB_STATUS_SUCCESS then
        begin
         {Return Result}
         {Result:=Status;} {Continue if Read Capacity fails}
         {Exit;}
        end; 
     
       {Return Result} 
       Result:=USB_STATUS_SUCCESS; 
      end;
     STORAGE_TYPE_FDD,STORAGE_TYPE_CDROM,STORAGE_TYPE_OPTICAL,STORAGE_TYPE_TAPE:begin
       {Floppy, CDROM, Optical or Tape}
       {Test Unit Ready}
       Status:=USBStorageDeviceTestUnitReady(Device,Storage,Storage.Storage.Device.DeviceFlags);
       if Status <> USB_STATUS_SUCCESS then
        begin
         {Return Result}
         {Result:=Status;} {Continue if Test Unit Ready fails}
         {Exit;}
        end; 
       
       {Check Flags}
       if (Storage.Storage.Device.DeviceFlags and (STORAGE_FLAG_NOT_READY or STORAGE_FLAG_NO_MEDIA)) = 0 then
        begin
         {Read Capacity}
         Status:=USBStorageDeviceReadCapacity(Device,Storage,Storage.Storage.BlockSize,Storage.Storage.BlockShift,Storage.Storage.BlockCount);
         if Status <> USB_STATUS_SUCCESS then
          begin
           {Return Result}
           {Result:=Status;} {Continue if Read Capacity fails}
           {Exit;}
          end; 
        end;  
        
       {Return Result} 
       Result:=USB_STATUS_SUCCESS; 
      end;
    end;
   end;
  else
   begin
    {Error Return}
    Result:=USB_STATUS_DEVICE_UNSUPPORTED; 
   end;   
 end;
end; 

{==============================================================================}

function USBStorageDeviceGetStatus(Device:PUSBDevice;Storage:PUSBStorageDevice):LongWord; 
{Supported by Subclass SCSI/UFI/SFF8070}
begin 
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Device}
 if Device = nil then Exit;

 {Check Storage}
 if Storage = nil then Exit;
 
 {$IFDEF STORAGE_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(Device,'USBStorageDeviceGetStatus');
 {$ENDIF}
 
 //To Do //This may not be needed, possibly include in Transport ?
 
 //Perhaps we could modify the purpose of this to be a quick way to check status (Ready/Not Ready) on any device. Subclass independant like GetInfo above.
end; 

{==============================================================================}

function USBStorageDeviceGetMaxLUN(Device:PUSBDevice;Storage:PUSBStorageDevice;var MaxLUN:Byte):LongWord;
{Issue a Get Max LUN request to a storage device}
var
 Status:LongWord;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Device}
 if Device = nil then Exit;

 {Check Storage}
 if Storage = nil then Exit;
  
 {$IFDEF STORAGE_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(Device,'USBStorageDeviceGetMaxLUN');
 {$ENDIF}
 
 {Check Protocol}
 case Storage.Protocol of
  USB_PROTOCOL_MASS_STORAGE_CBI,USB_PROTOCOL_MASS_STORAGE_CB:begin
    {Default Return}
    MaxLUN:=0;
    Result:=USB_STATUS_SUCCESS;
   end;
  USB_PROTOCOL_MASS_STORAGE_BBB:begin
    {Bulk Only Request}
    Status:=USBControlRequest(Device,nil,USB_STORAGE_REQUEST_BBB_GET_MAX_LUN,USB_BMREQUESTTYPE_DIR_IN or USB_BMREQUESTTYPE_TYPE_CLASS or USB_BMREQUESTTYPE_RECIPIENT_INTERFACE,0,Storage.StorageInterface.Descriptor.bInterfaceNumber,@MaxLUN,SizeOf(Byte));
    if Status = USB_STATUS_SUCCESS then
     begin
      {Return Result}
      Result:=USB_STATUS_SUCCESS;
     end
    else 
     begin
      {Default Return}
      MaxLUN:=0;
      Result:=USB_STATUS_SUCCESS; {Some devices do not support USB_STORAGE_REQUEST_BBB_GET_MAX_LUN and may error or STALL}
     end;     
   end;
  else
   begin
    {Error Return}
    Result:=USB_STATUS_DEVICE_UNSUPPORTED; 
   end;   
 end;
end; 

{==============================================================================}

function USBStorageDeviceClearStall(Device:PUSBDevice;Storage:PUSBStorageDevice;Endpoint:PUSBEndpointDescriptor):LongWord;
{Issue a USB Clear Feature (HALT) to the supplied endpoint on a storage device}
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Device}
 if Device = nil then Exit;

 {Check Storage}
 if Storage = nil then Exit;
 
 {Check Endpoint}
 if Endpoint = nil then Exit;

 {$IFDEF STORAGE_DEBUG} 
 if USB_LOG_ENABLED then USBLogDebug(Device,'USBStorageDeviceClearStall');
 {$ENDIF}
 
 {Check Protocol}
 case Storage.Protocol of
  USB_PROTOCOL_MASS_STORAGE_CBI,USB_PROTOCOL_MASS_STORAGE_CB:begin
    {Default Return}
    Result:=USB_STATUS_SUCCESS; //To Do //??
   end;
  USB_PROTOCOL_MASS_STORAGE_BBB:begin
    {Bulk Only Request}
    Result:=USBControlRequest(Device,nil,USB_DEVICE_REQUEST_CLEAR_FEATURE,USB_BMREQUESTTYPE_DIR_OUT or USB_BMREQUESTTYPE_TYPE_STANDARD or USB_BMREQUESTTYPE_RECIPIENT_ENDPOINT,USB_DEVICE_FEATURE_ENDPOINT_HALT,Endpoint.bEndpointAddress,nil,0);
    if Result <> USB_STATUS_SUCCESS then
     begin
      if USB_LOG_ENABLED then USBLogError(Device,'USBStorageDeviceClearStall - Clear feature (HALT) failure (Result=' + USBStatusToString(Result) + ')');
      Exit;
     end;
     
    {Wait for Reset}
    MillisecondDelay(150);
    
    {Return Result}
    Result:=USB_STATUS_SUCCESS;
   end;
  else
   begin
    {Error Return}
    Result:=USB_STATUS_DEVICE_UNSUPPORTED; 
   end;   
 end;
end; 

{==============================================================================}

function USBStorageDeviceInquiry(Device:PUSBDevice;Storage:PUSBStorageDevice;var DeviceType,DeviceFlags:LongWord;var Vendor,Product,Revision:PChar):LongWord;  
{Issue a SCSI Inquiry command to a LUN on a storage device}
{Supported by Subclass SCSI/UFI/SFF8070}
var
 Status:LongWord;
 RetryCount:Integer;
 Inquiry:TSCSIStandardInquiryData;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Device}
 if Device = nil then Exit;

 {Check Storage}
 if Storage = nil then Exit;
 
 {Setup Defaults}
 DeviceType:=STORAGE_TYPE_NONE;
 {DeviceFlags:=STORAGE_FLAG_NONE;} {Allow flags to persist}
 Vendor:=nil;
 Product:=nil;
 Revision:=nil;
 
 {$IFDEF STORAGE_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(Device,'USBStorageDeviceInquiry');
 {$ENDIF}
 
 {Inquiry}
 RetryCount:=USB_STORAGE_INQUIRY_RETRIES;
 while RetryCount > 0 do
  begin
   FillChar(Inquiry,SizeOf(Inquiry),0);
   FillChar(Storage.CommandBlock.Command[0],SizeOf(Storage.CommandBlock.Command),0);
   Storage.CommandBlock.Command[0]:=SCSI_COMMAND_INQUIRY;              {Command}
   Storage.CommandBlock.Command[1]:=(Storage.Storage.TargetLUN shl 5); {LUN}
   Storage.CommandBlock.Command[2]:=SCSI_INQUIRY_STANDARD;             {Page Code}
   Storage.CommandBlock.Command[4]:=SCSI_STANDARD_INQUIRY_SIZE;        {Allocation Length}
   Storage.CommandBlock.CommandLength:=12;
   Storage.CommandBlock.DataLength:=SCSI_STANDARD_INQUIRY_SIZE;
   Storage.CommandBlock.Data:=@Inquiry;
   Storage.CommandBlock.Direction:=USB_DIRECTION_IN;
   Status:=USBStorageDeviceTransport(Device,Storage,@Storage.CommandBlock);
   if Status = USB_STATUS_SUCCESS then
    begin
     {Return Data}
     {Device Flags}
     if ((Inquiry.RemovableMediaBit and SCSI_REMOVABLE_MEDIA_BIT) <> 0) or USB_STORAGE_FORCE_REMOVABLE then DeviceFlags:=DeviceFlags or STORAGE_FLAG_REMOVABLE else DeviceFlags:=DeviceFlags and not(STORAGE_FLAG_REMOVABLE);
     
     {Device Type}
     DeviceType:=SCSIDeviceTypeToStorageType(Inquiry.DeviceType,((DeviceFlags and STORAGE_FLAG_REMOVABLE) <> 0),(Storage.Subclass = USB_SUBCLASS_MASS_STORAGE_UFI));
     
     {Device Flags}
     case DeviceType of
      STORAGE_TYPE_HDD,STORAGE_TYPE_REMOVABLE:begin
        {Nothing}
       end;
      STORAGE_TYPE_CDROM,STORAGE_TYPE_OPTICAL,STORAGE_TYPE_TAPE:begin
        {Lockable / Ejectable}
        DeviceFlags:=DeviceFlags or STORAGE_FLAG_LOCKABLE or STORAGE_FLAG_EJECTABLE;
       end;
      STORAGE_TYPE_FDD:begin
        {Lockable}
        DeviceFlags:=DeviceFlags or STORAGE_FLAG_LOCKABLE;
       end;
     end;
     
     {Vendor}
     Vendor:=AllocMem(SizeOf(Inquiry.Vendor) + 1);
     if Vendor <> nil then
      begin
       StrLCopy(Vendor,PChar(Inquiry.Vendor),SizeOf(Inquiry.Vendor));
      end;
     
     {Product}
     Product:=AllocMem(SizeOf(Inquiry.Product) + 1);
     if Product <> nil then
      begin
       StrLCopy(Product,PChar(Inquiry.Product),SizeOf(Inquiry.Product));
      end;
     
     {Revision}
     Revision:=AllocMem(SizeOf(Inquiry.Revision) + 1);
     if Revision <> nil then
      begin
       StrLCopy(Revision,PChar(Inquiry.Revision),SizeOf(Inquiry.Revision));
      end;
     
     {Return Result}
     Result:=USB_STATUS_SUCCESS;
     Exit;
    end;
   
   Dec(RetryCount);    
  end;
  
 {Return Result} 
 Result:=USB_STATUS_HARDWARE_ERROR;
end; 

{==============================================================================}

function USBStorageDeviceRequestSense(Device:PUSBDevice;Storage:PUSBStorageDevice;var SenseKey,ASC,ASCQ:Byte):LongWord;
{Issue a SCSI Request Sense command to a LUN on a storage device}
{Supported by Subclass SCSI/UFI/SFF8070}
var
 Status:LongWord;
 Sense:TSCSIRequestSenseData;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Device}
 if Device = nil then Exit;

 {Check Storage}
 if Storage = nil then Exit;
 
 {Setup Defaults}
 SenseKey:=0;
 ASC:=0;
 ASCQ:=0;
 
 {$IFDEF STORAGE_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(Device,'USBStorageDeviceRequestSense');
 {$ENDIF}
 
 {Request Sense}
 FillChar(Sense,SizeOf(Sense),0);
 FillChar(Storage.CommandBlock.Command[0],SizeOf(Storage.CommandBlock.Command),0);
 Storage.CommandBlock.Command[0]:=SCSI_COMMAND_REQ_SENSE;            {Command}
 Storage.CommandBlock.Command[1]:=(Storage.Storage.TargetLUN shl 5); {LUN}
 Storage.CommandBlock.Command[4]:=SCSI_REQUEST_SENSE_SIZE;           {Allocation Length}
 Storage.CommandBlock.CommandLength:=12;
 Storage.CommandBlock.DataLength:=SCSI_REQUEST_SENSE_SIZE;
 Storage.CommandBlock.Data:=@Sense;
 Storage.CommandBlock.Direction:=USB_DIRECTION_IN;
 Status:=USBStorageDeviceTransport(Device,Storage,@Storage.CommandBlock);
 if Status = USB_STATUS_SUCCESS then
  begin
   {Return Data}
   SenseKey:=Sense.SenseKey;
   ASC:=Sense.ASC;
   ASCQ:=Sense.ASCQ;
   
   {Return Result}
   Result:=USB_STATUS_SUCCESS;
   Exit;
  end;
  
 {Return Result} 
 Result:=USB_STATUS_HARDWARE_ERROR;
end; 

{==============================================================================}

function USBStorageDeviceReadCapacity(Device:PUSBDevice;Storage:PUSBStorageDevice;var BlockSize,BlockShift:LongWord;var BlockCount:Int64):LongWord;
{Issue a SCSI Read Capacity command to a LUN on a storage device}
{Supported by Subclass SCSI/UFI/SFF8070}
var
 Status:LongWord;
 RetryCount:Integer;
 Capacity:TSCSIReadCapacityData;
 Capacity16:TSCSIReadCapacity16Data;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Device}
 if Device = nil then Exit;

 {Check Storage}
 if Storage = nil then Exit;
 
 {Setup Defaults}
 BlockSize:=0;
 BlockShift:=0;
 BlockCount:=0;
 
 {$IFDEF STORAGE_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(Device,'USBStorageDeviceReadCapacity'); 
 {$ENDIF}
 
 {Read Capacity}
 RetryCount:=USB_STORAGE_READ_CAPACITY_RETRIES;
 while RetryCount > 0 do
  begin
   FillChar(Capacity,SizeOf(Capacity),0);
   FillChar(Storage.CommandBlock.Command[0],SizeOf(Storage.CommandBlock.Command),0);
   Storage.CommandBlock.Command[0]:=SCSI_COMMAND_RD_CAPAC;             {Command}
   Storage.CommandBlock.Command[1]:=(Storage.Storage.TargetLUN shl 5); {LUN}
   Storage.CommandBlock.CommandLength:=12;
   Storage.CommandBlock.DataLength:=SCSI_READ_CAPACITY_SIZE;
   Storage.CommandBlock.Data:=@Capacity;
   Storage.CommandBlock.Direction:=USB_DIRECTION_IN;
   Status:=USBStorageDeviceTransport(Device,Storage,@Storage.CommandBlock);
   if Status = USB_STATUS_SUCCESS then
    begin
     {Check Capacity}
     if Capacity.LastBlock = $FFFFFFFF then
      begin
       {Read Capacity 16}
       FillChar(Capacity16,SizeOf(Capacity16),0);
       FillChar(Storage.CommandBlock.Command[0],SizeOf(Storage.CommandBlock.Command),0);
       Storage.CommandBlock.Command[0]:=SCSI_COMMAND_SVC_ACT_IN;                                        {Command} 
       Storage.CommandBlock.Command[1]:=(Storage.Storage.TargetLUN shl 5) or SCSI_SAI_READ_CAPACITY_16; {LUN / Service Action}
       Storage.CommandBlock.Command[13]:=SCSI_READ_CAPACITY_16_SIZE;                                    {Allocation Length}
       Storage.CommandBlock.CommandLength:=16;
       Storage.CommandBlock.DataLength:=SCSI_READ_CAPACITY_16_SIZE;
       Storage.CommandBlock.Data:=@Capacity16;
       Storage.CommandBlock.Direction:=USB_DIRECTION_IN;
       Status:=USBStorageDeviceTransport(Device,Storage,@Storage.CommandBlock);
       if Status = USB_STATUS_SUCCESS then
        begin
         {Return Data}
         BlockSize:=LongWordBEtoN(Capacity16.BlockSize);       {Big Endian to Native}
         BlockShift:=USBStorageBlockSizeToBlockShift(BlockSize);
         BlockCount:=LongWordBEtoN(Capacity16.LastBlock) + 1;  {Big Endian to Native}
         {Return Result}
         Result:=USB_STATUS_SUCCESS;
         Exit;
        end;
      end
     else
      begin     
       {Return Data}
       BlockSize:=LongWordBEtoN(Capacity.BlockSize);       {Big Endian to Native}
       BlockShift:=USBStorageBlockSizeToBlockShift(BlockSize);
       BlockCount:=LongWordBEtoN(Capacity.LastBlock) + 1;  {Big Endian to Native}
       {Return Result}
       Result:=USB_STATUS_SUCCESS;
       Exit;
      end; 
    end;
   
   Dec(RetryCount);    
  end;
 
 {Return Result}
 Result:=USB_STATUS_HARDWARE_ERROR;
end; 

{==============================================================================}

function USBStorageDeviceTestUnitReady(Device:PUSBDevice;Storage:PUSBStorageDevice;var DeviceFlags:LongWord):LongWord;
{Issue a SCSI Test Unit Ready command to a LUN on a storage device}
{Supported by Subclass SCSI/UFI/SFF8070}

var
 ASC:Byte;
 ASCQ:Byte;
 SenseKey:Byte;
 Status:LongWord;
 RetryCount:Integer;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Device}
 if Device = nil then Exit;
 
 {Check Storage}
 if Storage = nil then Exit;

 {Setup Defaults}
 {DeviceFlags:=STORAGE_FLAG_NONE;} {Allow flags to persist}
 
 {$IFDEF STORAGE_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(Device,'USBStorageDeviceTestUnitReady');
 {$ENDIF}
 
 {Test Unit Ready}
 RetryCount:=USB_STORAGE_TEST_UNIT_READY_RETRIES;
 while RetryCount > 0 do
  begin
   FillChar(Storage.CommandBlock.Command[0],SizeOf(Storage.CommandBlock.Command),0);
   Storage.CommandBlock.Command[0]:=SCSI_COMMAND_TST_U_RDY;            {Command} 
   Storage.CommandBlock.Command[1]:=(Storage.Storage.TargetLUN shl 5); {LUN}
   Storage.CommandBlock.CommandLength:=12;
   Storage.CommandBlock.DataLength:=0;
   Storage.CommandBlock.Direction:=USB_DIRECTION_IN; {Data Length is 0}
   Status:=USBStorageDeviceTransport(Device,Storage,@Storage.CommandBlock);
   if Status = USB_STATUS_SUCCESS then
    begin
     {Return Data}
     DeviceFlags:=DeviceFlags and not(STORAGE_FLAG_NOT_READY or STORAGE_FLAG_NO_MEDIA);
     
     {Return Result}
     Result:=USB_STATUS_SUCCESS;
     Exit;
    end;
    
   {$IFDEF STORAGE_DEBUG}
   if USB_LOG_ENABLED then USBLogDebug(Device,'USBStorageDeviceTestUnitReady (Status=' + USBStatusToString(Status) + ')'); 
   {$ENDIF}
   
   {Perform a Request Sense to check for medium not present}
   Status:=USBStorageDeviceRequestSense(Device,Storage,SenseKey,ASC,ASCQ);
   if Status = USB_STATUS_SUCCESS then
    begin
     {$IFDEF STORAGE_DEBUG}
     if USB_LOG_ENABLED then USBLogDebug(Device,'USBStorageDeviceTestUnitReady (SenseKey=' + IntToHex(SenseKey,2) + ' ASC=' + IntToHex(ASC,2) + ' ASCQ=' + IntToHex(ASCQ,2) + ')');
     {$ENDIF}
     
     {Check for Medium Not Present}
     if (SenseKey = SCSI_SENSE_NOT_READY) and (ASC = SCSI_ASC_NOT_READY_MEDIUM_NOT_PRESENT) then
      begin
       {Return Error}
       DeviceFlags:=DeviceFlags or (STORAGE_FLAG_NOT_READY or STORAGE_FLAG_NO_MEDIA);
       
       {Return Result}
       Result:=USB_STATUS_NOT_READY;
       Exit;
      end;
    end; 
   
   {Delay next retry}
   MillisecondDelay(100);
   
   Dec(RetryCount);    
  end;
  
 {Return Default}
 DeviceFlags:=DeviceFlags or STORAGE_FLAG_NOT_READY;
 
 {Return Result}
 Result:=USB_STATUS_HARDWARE_ERROR;
end; 

{==============================================================================}

function USBStorageDeviceRead10(Device:PUSBDevice;Storage:PUSBStorageDevice;Start:LongWord;Count:Word;Buffer:Pointer):LongWord; 
{Issue a SCSI Read10 command to a LUN on a storage device}
{Supported by Subclass SCSI/UFI/SFF8070}
{Note: Caller must hold the storage lock}
var
 ASC:Byte;
 ASCQ:Byte;
 SenseKey:Byte;
 Status:LongWord;
 RetryCount:Integer;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Device}
 if Device = nil then Exit;

 {Check Storage}
 if Storage = nil then Exit;
 
 {$IFDEF STORAGE_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(Device,'USBStorageDeviceRead10 (Start=' + IntToStr(Start) + ' Count=' + IntToStr(Count) + ')');
 {$ENDIF}
 
 {Read 10}
 RetryCount:=USB_STORAGE_READ_RETRIES;
 while RetryCount > 0 do
  begin
   FillChar(Storage.CommandBlock.Command[0],SizeOf(Storage.CommandBlock.Command),0);
   Storage.CommandBlock.Command[0]:=SCSI_COMMAND_READ10;               {Command} 
   Storage.CommandBlock.Command[1]:=(Storage.Storage.TargetLUN shl 5); {LUN}
   Storage.CommandBlock.Command[2]:=(Start shr 24) and $FF;            {Logical Block Address}
   Storage.CommandBlock.Command[3]:=(Start shr 16) and $FF;            {Logical Block Address}
   Storage.CommandBlock.Command[4]:=(Start shr 8) and $FF;             {Logical Block Address}
   Storage.CommandBlock.Command[5]:=(Start shr 0) and $FF;             {Logical Block Address}
   Storage.CommandBlock.Command[7]:=(Count shr 8) and $FF;             {Transfer Length}
   Storage.CommandBlock.Command[8]:=(Count shr 0) and $FF;             {Transfer Length}
   Storage.CommandBlock.CommandLength:=12;
   Storage.CommandBlock.DataLength:=(Count shl Storage.Storage.BlockShift); 
   Storage.CommandBlock.Data:=Buffer;
   Storage.CommandBlock.Direction:=USB_DIRECTION_IN;
   Status:=USBStorageDeviceTransport(Device,Storage,@Storage.CommandBlock);
   if Status = USB_STATUS_SUCCESS then
    begin
     {Return Result}
     Result:=USB_STATUS_SUCCESS;
     Exit;
    end;
  
   {Perform a Request Sense on error}
   Status:=USBStorageDeviceRequestSense(Device,Storage,SenseKey,ASC,ASCQ);
   if Status <> USB_STATUS_SUCCESS then
    begin
     {Return Result}
     Result:=USB_STATUS_HARDWARE_ERROR;
     Exit;
    end;    
    
   Dec(RetryCount);    
  end; 
 
 {Return Result}
 Result:=USB_STATUS_HARDWARE_ERROR;
end; 

{==============================================================================}

function USBStorageDeviceWrite10(Device:PUSBDevice;Storage:PUSBStorageDevice;Start:LongWord;Count:Word;Buffer:Pointer):LongWord; 
{Issue a SCSI Write10 command to a LUN on a storage device}
{Supported by Subclass SCSI/UFI/SFF8070}
{Note: Caller must hold the storage lock}
var
 ASC:Byte;
 ASCQ:Byte;
 SenseKey:Byte;
 Status:LongWord;
 RetryCount:Integer;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Device}
 if Device = nil then Exit;

 {Check Storage}
 if Storage = nil then Exit;

 {$IFDEF STORAGE_DEBUG} 
 if USB_LOG_ENABLED then USBLogDebug(Device,'USBStorageDeviceWrite10 (Start=' + IntToStr(Start) + ' Count=' + IntToStr(Count) + ')');
 {$ENDIF}
 
 {Write 10}
 RetryCount:=USB_STORAGE_WRITE_RETRIES;
 while RetryCount > 0 do
  begin
   FillChar(Storage.CommandBlock.Command[0],SizeOf(Storage.CommandBlock.Command),0);
   Storage.CommandBlock.Command[0]:=SCSI_COMMAND_WRITE10;              {Command} 
   Storage.CommandBlock.Command[1]:=(Storage.Storage.TargetLUN shl 5); {LUN}
   Storage.CommandBlock.Command[2]:=(Start shr 24) and $FF;            {Logical Block Address}
   Storage.CommandBlock.Command[3]:=(Start shr 16) and $FF;            {Logical Block Address}
   Storage.CommandBlock.Command[4]:=(Start shr 8) and $FF;             {Logical Block Address}
   Storage.CommandBlock.Command[5]:=(Start shr 0) and $FF;             {Logical Block Address}
   Storage.CommandBlock.Command[7]:=(Count shr 8) and $FF;             {Transfer Length}
   Storage.CommandBlock.Command[8]:=(Count shr 0) and $FF;             {Transfer Length}
   Storage.CommandBlock.CommandLength:=12;
   Storage.CommandBlock.DataLength:=(Count shl Storage.Storage.BlockShift); 
   Storage.CommandBlock.Data:=Buffer;
   Storage.CommandBlock.Direction:=USB_DIRECTION_OUT;
   Status:=USBStorageDeviceTransport(Device,Storage,@Storage.CommandBlock);
   if Status = USB_STATUS_SUCCESS then
    begin
     {Return Result}
     Result:=USB_STATUS_SUCCESS;
     Exit;
    end;
 
   {Perform a Request Sense on error}
   Status:=USBStorageDeviceRequestSense(Device,Storage,SenseKey,ASC,ASCQ);
   if Status <> USB_STATUS_SUCCESS then
    begin
     {Return Result}
     Result:=USB_STATUS_HARDWARE_ERROR;
     Exit;
    end;    
    
   Dec(RetryCount);    
  end; 

 {Return Result}
 Result:=USB_STATUS_HARDWARE_ERROR;
end; 

{==============================================================================}

function USBStorageDeviceRead16(Device:PUSBDevice;Storage:PUSBStorageDevice;const Start,Count:Int64;Buffer:Pointer):LongWord; 
{Issue a SCSI Read16 command to a LUN on a storage device}
{Supported by Subclass SCSI only (Only on devices > 2TB)}
{Note: Caller must hold the storage lock}
var
 ASC:Byte;
 ASCQ:Byte;
 SenseKey:Byte;
 Status:LongWord;
 RetryCount:Integer;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Device}
 if Device = nil then Exit;

 {Check Storage}
 if Storage = nil then Exit;
 
 {$IFDEF STORAGE_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(Device,'USBStorageDeviceRead16 (Start=' + IntToStr(Start) + ' Count=' + IntToStr(Count) + ')');
 {$ENDIF}
 
 {Read 16}
 RetryCount:=USB_STORAGE_READ_RETRIES;
 while RetryCount > 0 do
  begin
   FillChar(Storage.CommandBlock.Command[0],SizeOf(Storage.CommandBlock.Command),0);
   Storage.CommandBlock.Command[0]:=SCSI_COMMAND_READ16;               {Command} 
   Storage.CommandBlock.Command[1]:=(Storage.Storage.TargetLUN shl 5); {LUN}
   Storage.CommandBlock.Command[2]:=(Start shr 56) and $FF;            {Logical Block Address}
   Storage.CommandBlock.Command[3]:=(Start shr 48) and $FF;            {Logical Block Address}
   Storage.CommandBlock.Command[4]:=(Start shr 40) and $FF;            {Logical Block Address}
   Storage.CommandBlock.Command[5]:=(Start shr 32) and $FF;            {Logical Block Address}
   Storage.CommandBlock.Command[6]:=(Start shr 24) and $FF;            {Logical Block Address}
   Storage.CommandBlock.Command[7]:=(Start shr 16) and $FF;            {Logical Block Address}
   Storage.CommandBlock.Command[8]:=(Start shr 8) and $FF;             {Logical Block Address}
   Storage.CommandBlock.Command[9]:=(Start shr 0) and $FF;             {Logical Block Address}
   Storage.CommandBlock.Command[10]:=(Count shr 24) and $FF;           {Transfer Length}
   Storage.CommandBlock.Command[11]:=(Count shr 16) and $FF;           {Transfer Length}
   Storage.CommandBlock.Command[12]:=(Count shr 8) and $FF;            {Transfer Length}
   Storage.CommandBlock.Command[13]:=(Count shr 0) and $FF;            {Transfer Length}
   Storage.CommandBlock.CommandLength:=16;
   Storage.CommandBlock.DataLength:=(Count shl Storage.Storage.BlockShift); 
   Storage.CommandBlock.Data:=Buffer;
   Storage.CommandBlock.Direction:=USB_DIRECTION_IN;
   Status:=USBStorageDeviceTransport(Device,Storage,@Storage.CommandBlock);
   if Status = USB_STATUS_SUCCESS then
    begin
     {Return Result}
     Result:=USB_STATUS_SUCCESS;
     Exit;
    end;
  
   {Perform a Request Sense on error}
   Status:=USBStorageDeviceRequestSense(Device,Storage,SenseKey,ASC,ASCQ);
   if Status <> USB_STATUS_SUCCESS then
    begin
     {Return Result}
     Result:=USB_STATUS_HARDWARE_ERROR;
     Exit;
    end;    
    
   Dec(RetryCount);    
  end; 

 {Return Result}
 Result:=USB_STATUS_HARDWARE_ERROR;
end;
 
{==============================================================================}

function USBStorageDeviceWrite16(Device:PUSBDevice;Storage:PUSBStorageDevice;const Start,Count:Int64;Buffer:Pointer):LongWord; 
{Issue a SCSI Write16 command to a LUN on a storage device}
{Supported by Subclass SCSI only (Only on devices > 2TB)}
{Note: Caller must hold the storage lock}
var
 ASC:Byte;
 ASCQ:Byte;
 SenseKey:Byte;
 Status:LongWord;
 RetryCount:Integer;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Device}
 if Device = nil then Exit;

 {Check Storage}
 if Storage = nil then Exit;
 
 {$IFDEF STORAGE_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(Device,'USBStorageDeviceWrite16 (Start=' + IntToStr(Start) + ' Count=' + IntToStr(Count) + ')');
 {$ENDIF}
 
 {Write 16}
 RetryCount:=USB_STORAGE_WRITE_RETRIES;
 while RetryCount > 0 do
  begin
   FillChar(Storage.CommandBlock.Command[0],SizeOf(Storage.CommandBlock.Command),0);
   Storage.CommandBlock.Command[0]:=SCSI_COMMAND_WRITE16;              {Command} 
   Storage.CommandBlock.Command[1]:=(Storage.Storage.TargetLUN shl 5); {LUN}
   Storage.CommandBlock.Command[2]:=(Start shr 56) and $FF;            {Logical Block Address}
   Storage.CommandBlock.Command[3]:=(Start shr 48) and $FF;            {Logical Block Address}
   Storage.CommandBlock.Command[4]:=(Start shr 40) and $FF;            {Logical Block Address}
   Storage.CommandBlock.Command[5]:=(Start shr 32) and $FF;            {Logical Block Address}
   Storage.CommandBlock.Command[6]:=(Start shr 24) and $FF;            {Logical Block Address}
   Storage.CommandBlock.Command[7]:=(Start shr 16) and $FF;            {Logical Block Address}
   Storage.CommandBlock.Command[8]:=(Start shr 8) and $FF;             {Logical Block Address}
   Storage.CommandBlock.Command[9]:=(Start shr 0) and $FF;             {Logical Block Address}
   Storage.CommandBlock.Command[10]:=(Count shr 24) and $FF;           {Transfer Length}
   Storage.CommandBlock.Command[11]:=(Count shr 16) and $FF;           {Transfer Length}
   Storage.CommandBlock.Command[12]:=(Count shr 8) and $FF;            {Transfer Length}
   Storage.CommandBlock.Command[13]:=(Count shr 0) and $FF;            {Transfer Length}
   Storage.CommandBlock.CommandLength:=16;
   Storage.CommandBlock.DataLength:=(Count shl Storage.Storage.BlockShift); 
   Storage.CommandBlock.Data:=Buffer;
   Storage.CommandBlock.Direction:=USB_DIRECTION_OUT;
   Status:=USBStorageDeviceTransport(Device,Storage,@Storage.CommandBlock);
   if Status = USB_STATUS_SUCCESS then
    begin
     {Return Result}
     Result:=USB_STATUS_SUCCESS;
     Exit;
    end;
  
   {Perform a Request Sense on error}
   Status:=USBStorageDeviceRequestSense(Device,Storage,SenseKey,ASC,ASCQ);
   if Status <> USB_STATUS_SUCCESS then
    begin
     {Return Result}
     Result:=USB_STATUS_HARDWARE_ERROR;
     Exit;
    end;    
    
   Dec(RetryCount);    
  end; 

 {Return Result}
 Result:=USB_STATUS_HARDWARE_ERROR;
end;

{==============================================================================}

function USBStorageDeviceTransport(Device:PUSBDevice;Storage:PUSBStorageDevice;Command:PUSBCommandBlock):LongWord; 
{Perform the appropriate transport sequence for the device based on subclass and protocol}
{Handles Command / Data / Status phases plus endpoint stall, reset and retry as per the specification}
{Supported by Subclass SCSI/UFI/SFF8070}
{Note: Caller must hold the storage lock}
var
 Status:LongWord;
 RetryCount:Integer;
 ActualSize:LongWord;
 Request:PUSBRequest;
 Wait:TSemaphoreHandle;
 CommandBlock:TUSBCommandBlockWrapper;
 CommandStatus:TUSBCommandStatusWrapper;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Device}
 if Device = nil then Exit;

 {Check Storage}
 if Storage = nil then Exit;
 
 {Check Command}
 if Command = nil then Exit;
 
 {$IFDEF STORAGE_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(Device,'USBStorageDeviceTransport'); 
 {$ENDIF}
 
 {Check Protocol}
 case Storage.Protocol of
  USB_PROTOCOL_MASS_STORAGE_CBI,USB_PROTOCOL_MASS_STORAGE_CB:begin
    {Control/Bulk/Interrupt Request}
 
    //To Do //CBI Transport see: \u-boot-HEAD-5745f8c\common\usb_storage.c etc
    
   end;
  USB_PROTOCOL_MASS_STORAGE_BBB:begin
    {Bulk Only Request}
    {Check Command}
    if Command.CommandLength > USB_STORAGE_CBW_CB_LENGTH then Exit;
    
    {Command Phase}
    {=============}
    {Update Sequence}
    Inc(Storage.Sequence);
    
    {Create Command Block Wrapper}
    CommandBlock.dCBWSignature:=USB_STORAGE_CBW_SIGNATURE;
    CommandBlock.dCBWTag:=Storage.Sequence;
    CommandBlock.dCBWDataTransferLength:=Command.DataLength;
    if Command.Direction = USB_DIRECTION_IN then CommandBlock.bCBWFlags:=USB_STORAGE_CBW_FLAGS_IN else CommandBlock.bCBWFlags:=USB_STORAGE_CBW_FLAGS_OUT;
    CommandBlock.bCBWLUN:=Storage.Storage.TargetLUN;
    CommandBlock.bCBWCBLength:=Command.CommandLength;
    {Copy the SCSI Command Data to the CBW Command Block}
    System.Move(Command.Command[0],CommandBlock.CBWCB[0],Command.CommandLength);
    
    {$IFDEF STORAGE_DEBUG}
    if USB_LOG_ENABLED then USBLogDebug(Device,'USBStorageDeviceTransport - Submitting Command request (Sequence=' + IntToStr(Storage.Sequence) + ')');
    {$ENDIF}
    
    {Update Request (Write)}
    Storage.WriteRequest.Data:=@CommandBlock;
    Storage.WriteRequest.Size:=USB_STORAGE_CBW_SIZE;

    {Update Pending}
    Inc(Storage.PendingCount);
    
    {Submit Request (Write)}
    Status:=USBRequestSubmit(Storage.WriteRequest);
    if Status <> USB_STATUS_SUCCESS then
     begin
      {Update Pending}
      Dec(Storage.PendingCount);
      
      {Return Result}
      Result:=Status;
      Exit;
     end;
    
    {Wait for Request (Write)}
    if SemaphoreWait(Storage.WriteWait) <> ERROR_SUCCESS then
     begin
      {Return Result}
      Result:=USB_STATUS_OPERATION_FAILED;
      Exit;
     end;
    
    {Get Status}
    Status:=Storage.WriteRequest.Status;
    if Status <> USB_STATUS_SUCCESS then
     begin
      if USB_LOG_ENABLED then USBLogError(Device,'USBStorageDeviceTransport - Command Phase error (Status=' + USBStatusToString(Status) + ')');
      
      {Reset Device}
      USBStorageDeviceReset(Device,Storage);
      
      {Return Result}
      Result:=Status;
      Exit;
     end;
     
    {Data Phase}
    {==========}
    {Check Length}
    ActualSize:=0;
    if Command.DataLength > 0 then
     begin
      {$IFDEF STORAGE_DEBUG}
      if USB_LOG_ENABLED then USBLogDebug(Device,'USBStorageDeviceTransport - Submitting Data request (Length=' + IntToStr(Command.DataLength) + ')');
      {$ENDIF}

      {Get Request (Read or Write)}
      if Command.Direction = USB_DIRECTION_IN then
       begin
        Wait:=Storage.ReadWait;
        Request:=Storage.ReadRequest;
       end 
      else
       begin
        Wait:=Storage.WriteWait;
        Request:=Storage.WriteRequest;
       end; 
      
      {Update Request}
      Request.Data:=Command.Data;
      Request.Size:=Command.DataLength;

      {Update Pending}
      Inc(Storage.PendingCount);
      
      {Submit Request}
      Status:=USBRequestSubmit(Request);
      if Status <> USB_STATUS_SUCCESS then
       begin
        {Update Pending}
        Dec(Storage.PendingCount);
        
        {Return Result}
        Result:=Status;
        Exit;
       end;
    
      {Wait for Request}
      if SemaphoreWait(Wait) <> ERROR_SUCCESS then
       begin
        {Return Result}
        Result:=USB_STATUS_OPERATION_FAILED;
        Exit;
       end;
      
      {Get Status}
      Status:=Request.Status;
      if Status <> USB_STATUS_SUCCESS then
       begin
        if USB_LOG_ENABLED then USBLogError(Device,'USBStorageDeviceTransport - Data Phase error (Status=' + USBStatusToString(Status) + ')');
        
        {Check for Endpoint STALL}
        if Status = USB_STATUS_HARDWARE_STALL then
         begin
          //Status:=USBStorageDeviceClearStall(Device,Storage,Request.Endpoint); //TestingRPi
          Status:=USBStorageDeviceClearStall(Device,Storage,Storage.ReadRequest.Endpoint);
          if Status = USB_STATUS_SUCCESS then
           begin
            Status:=USBStorageDeviceClearStall(Device,Storage,Storage.WriteRequest.Endpoint);
           end;
         end;
       end;  
      if Status <> USB_STATUS_SUCCESS then
       begin
        if USB_LOG_ENABLED then USBLogError(Device,'USBStorageDeviceTransport - Data Phase error (Status=' + USBStatusToString(Status) + ')');
        
        {If still not successful then reset device}
        USBStorageDeviceReset(Device,Storage);
        
        {Return Result}
        Result:=Status;
        Exit;
       end;
       
      {Get Actual Size}
      ActualSize:=Request.ActualSize;
      
      if ActualSize <> Command.DataLength then
       begin
        if USB_LOG_ENABLED then USBLogError(Device,'USBStorageDeviceTransport - Data Phase size not equal to length (Size=' + IntToStr(ActualSize) + ' Length=' + IntToStr(Command.DataLength) + ')');
       end;
     end;
   
    {Status Phase}
    {============}
    {Setup Retry}
    RetryCount:=USB_STORAGE_STATUS_RETRIES;
    while RetryCount > 0 do
     begin
      {$IFDEF STORAGE_DEBUG}
      if USB_LOG_ENABLED then USBLogDebug(Device,'USBStorageDeviceTransport - Submitting Status request (RetryCount=' + IntToStr(RetryCount) + ')');
      {$ENDIF}
      
      {Update Request (Read)}
      Storage.ReadRequest.Data:=@CommandStatus;
      Storage.ReadRequest.Size:=USB_STORAGE_CSW_SIZE;
    
      {Update Pending}
      Inc(Storage.PendingCount);
    
      {Submit Request (Read)}
      Status:=USBRequestSubmit(Storage.ReadRequest);
      if Status <> USB_STATUS_SUCCESS then
       begin
        {Update Pending}
        Dec(Storage.PendingCount);
    
        {Return Result}
        Result:=Status;
        Exit;
       end;
    
      {Wait for Request (Read)}
      if SemaphoreWait(Storage.ReadWait) <> ERROR_SUCCESS then
       begin
        {Return Result}
        Result:=USB_STATUS_OPERATION_FAILED;
        Exit;
       end;
      
      {Get Status}
      Status:=Storage.ReadRequest.Status;
      if Status = USB_STATUS_SUCCESS then
       begin
        Break;
       end
      else 
       begin
        if USB_LOG_ENABLED then USBLogError(Device,'USBStorageDeviceTransport - Status Phase error (Status=' + USBStatusToString(Status) + ')');
        
        {Check for Endpoint STALL}
        if Status = USB_STATUS_HARDWARE_STALL then
         begin
          //Status:=USBStorageDeviceClearStall(Device,Storage,Storage.ReadRequest.Endpoint); //TestingRPi
          Status:=USBStorageDeviceClearStall(Device,Storage,Storage.ReadRequest.Endpoint);
          if Status = USB_STATUS_SUCCESS then
           begin
            Status:=USBStorageDeviceClearStall(Device,Storage,Storage.WriteRequest.Endpoint);
           end;
         end;
       end;  
      if Status <> USB_STATUS_SUCCESS then
       begin
        if USB_LOG_ENABLED then USBLogError(Device,'USBStorageDeviceTransport - Status Phase error (Status=' + USBStatusToString(Status) + ')');
        
        {If still not successful then reset device}
        USBStorageDeviceReset(Device,Storage);
        
        {Return Result}
        Result:=Status;
        Exit;
       end;
   
      Dec(RetryCount);
     end;
     
    {Check Command Status Block}
    if CommandStatus.dCSWSignature <> USB_STORAGE_CSW_SIGNATURE then
     begin
      if USB_LOG_ENABLED then USBLogError(Device,'USBStorageDeviceTransport - Status Phase invalid signature (dCSWSignature=' + IntToHex(CommandStatus.dCSWSignature,8) + ')');
      
      {Reset Device}
      USBStorageDeviceReset(Device,Storage);
      
      {Return Result}
      Result:=USB_STATUS_HARDWARE_ERROR;
      Exit;
     end
    else if CommandStatus.dCSWTag <> Storage.Sequence then
     begin
      if USB_LOG_ENABLED then USBLogError(Device,'USBStorageDeviceTransport - Status Phase invalid tag (dCSWTag=' + IntToHex(CommandStatus.dCSWTag,8) + ')');
      
      {Reset Device}
      USBStorageDeviceReset(Device,Storage);
      
      {Return Result}
      Result:=USB_STATUS_HARDWARE_ERROR;
      Exit;
     end
    else if CommandStatus.bCSWStatus >= USB_STORAGE_CSW_STATUS_PHASE then
     begin
      if USB_LOG_ENABLED then USBLogError(Device,'USBStorageDeviceTransport - Status Phase invalid status (bCSWStatus=' + IntToHex(CommandStatus.bCSWStatus,2) + ')');
      
      {Reset Device}
      USBStorageDeviceReset(Device,Storage);
      
      {Return Result}
      Result:=USB_STATUS_HARDWARE_ERROR;
      Exit;
     end
    else if (CommandStatus.bCSWStatus = USB_STORAGE_CSW_STATUS_FAILED) then
     begin
      {$IFDEF STORAGE_DEBUG}
      if USB_LOG_ENABLED then USBLogError(Device,'USBStorageDeviceTransport - Status Phase status failure (bCSWStatus=' + IntToHex(CommandStatus.bCSWStatus,2) + ' dCSWDataResidue=' + IntToStr(CommandStatus.dCSWDataResidue) + ')');
      {$ENDIF}
      
      {Return Result}
      Result:=USB_STATUS_HARDWARE_ERROR;
      Exit;
     end
    else if (CommandStatus.bCSWStatus = USB_STORAGE_CSW_STATUS_GOOD) and (CommandStatus.dCSWDataResidue <> 0) then  
     begin
      if USB_LOG_ENABLED then USBLogError(Device,'USBStorageDeviceTransport - Status Phase data residue (bCSWStatus=' + IntToHex(CommandStatus.bCSWStatus,2) + ' dCSWDataResidue=' + IntToStr(CommandStatus.dCSWDataResidue) + ')');
      
      {Result Success}
     end
    else if ActualSize <> Command.DataLength then
     begin
      if USB_LOG_ENABLED then USBLogError(Device,'USBStorageDeviceTransport - Status Phase invalid data length (Length=' + IntToStr(ActualSize) + ')');
      
      {Return Result}
      Result:=USB_STATUS_HARDWARE_ERROR;
      Exit;
     end;

    {Return Result}
    Result:=USB_STATUS_SUCCESS;
   end;
  else
   begin
    {Error Return}
    Result:=USB_STATUS_DEVICE_UNSUPPORTED; 
   end;   
 end;
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
