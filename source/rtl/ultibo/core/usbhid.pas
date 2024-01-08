{
Ultibo USB Human Interface Device (HID) driver unit.

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

  USB HID - https://en.wikipedia.org/wiki/USB_human_interface_device_class
            https://www.usb.org/hid
            https://www.usb.org/document-library/device-class-definition-hid-111
            https://www.usb.org/document-library/hid-usage-tables-13


USB HID
=======

 The USB Human Interface Device (HID) class is one of the most widely used
 classes of USB devices. HID devices can appear as keyboards, mice, touch devices,
 gamepads, joysticks and many more.

 Importantly HID devices can appear as controls within other devices, for example
 a USB headset can describe the volume and mute buttons as a HID device and a
 generic driver can recognize it without the need for a custom driver.

 Unlike other USB devices where either an entire device or an interface on a
 device is used to provide a specific function, the HID standard allows multiple
 devices to coexist on the same interface. So a mouse and keyboard could be defined
 on a single USB interface and the HID implementation is able to route the reports
 they generate to the correct consumer.

 Some HID devices (such as proprietary game devices) may require more specific
 handling than can be provided by the generic device infrastructure included in
 this unit.

 In those cases a standard USB driver can be developed which claims the entire
 device by vendor and product (as USB offers the complete device to all drivers
 first). The driver can then configure the device as required and is able to use
 some of the services of both this unit and the HID unit to parse and examine the
 HID report descriptors.

 Alternatively a HID consumer can be developed that claims the HID device during
 the optional HID DeviceBind callback which prevents other consumers from binding
 to the HID collections it contains. The consumer can then use some or all of the
 services of this unit and the HID unit to handle the HID report descriptors when
 dealing with reports received from the device.

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit USBHID;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,HID,USB,SysUtils;

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
const
 {USB HID specific constants}
 USB_HID_DRIVER_NAME = 'USB HID Driver'; {Name of USB HID driver}

{==============================================================================}
type
 {USB HID specific types}
 PUSBHIDRequest = ^TUSBHIDRequest;

 PUSBHIDRequests = ^TUSBHIDRequests;
 TUSBHIDRequests = array[0..0] of PUSBHIDRequest;

 TUSBHIDRequest = record
  Collection:PHIDCollection;  {The collection this request corresponds to}
 end;

 PUSBHIDDevice = ^TUSBHIDDevice;
 TUSBHIDDevice = record
  {HID Properties}
  HID:THIDDevice;                        {The HID entry for this USB Device}
  {USB Properties}
  USBDevice:PUSBDevice;                  {The USB device}
  USBInterface:PUSBInterface;            {The USB interface}
  HIDDescriptor:PHIDDescriptor;          {The USB HID descriptor from the interface}
  {Driver Properties}
  ReportActive:LongWord;                 {The number of currently active report requests}
  ReportMaximum:LongWord;                {The maximum report id number this device supports}
  ReportRequest:PUSBRequest;             {The USB request submitted for this endpoint}
  ReportRequests:PUSBHIDRequests;        {Active input report requests from consumers}
  ReportEndpoint:PUSBEndpointDescriptor; {The USB endpoint for receiving input reports}
  OutputEndpoint:PUSBEndpointDescriptor; {The USB endpoint for receiving ouput reports}
  PendingCount:LongWord;                 {Number of USB requests pending for this HID device}
  WaiterThread:TThreadId;                {Thread waiting for pending requests to complete (for device detachment)}
 end;

{==============================================================================}
{var}
 {USB HID specific variables}

{==============================================================================}
{Initialization Functions}
procedure USBHIDInit;

{==============================================================================}
{USB HID Functions}

{==============================================================================}
{USB HID Helper Functions}
function USBHIDCheckDevice(Device:PUSBDevice):Boolean;
function USBHIDCheckInterface(Device:PUSBDevice;Interrface:PUSBInterface):Boolean;

function USBHIDGetHIDDescriptor(Device:PUSBDevice;Interrface:PUSBInterface;Descriptor:PHIDDescriptor):LongWord;

function USBHIDStatusToErrorCode(Status:LongWord):LongWord;
function USBHIDErrorCodeToUSBStatus(Error:LongWord):LongWord;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {USB HID specific variables}
 USBHIDInitialized:Boolean;

 USBHIDDriver:PUSBDriver;  {HID Driver interface (Set by HIDInit)}

{==============================================================================}
{==============================================================================}
{Forward Declarations}
function USBHIDDriverBind(Device:PUSBDevice;Interrface:PUSBInterface):LongWord; forward;
function USBHIDDriverUnbind(Device:PUSBDevice;Interrface:PUSBInterface):LongWord; forward;

procedure USBHIDDeviceReportWorker(Request:PUSBRequest); forward;
procedure USBHIDDeviceReportComplete(Request:PUSBRequest); forward;

function USBHIDDeviceGetIdle(Device:PHIDDevice;var Duration:Word;ReportId:Byte):LongWord; forward;
function USBHIDDeviceSetIdle(Device:PHIDDevice;Duration:Word;ReportId:Byte):LongWord; forward;

function USBHIDDeviceGetReport(Device:PHIDDevice;ReportType,ReportId:Byte;ReportData:Pointer;ReportSize:LongWord):LongWord; forward;
function USBHIDDeviceSetReport(Device:PHIDDevice;ReportType,ReportId:Byte;ReportData:Pointer;ReportSize:LongWord):LongWord; forward;

function USBHIDDeviceAllocateReport(Device:PHIDDevice;Collection:PHIDCollection;ReportId:Byte;ReportSize:LongWord):LongWord; forward;
function USBHIDDeviceReleaseReport(Device:PHIDDevice;ReportId:Byte):LongWord; forward;

function USBHIDDeviceSubmitReport(Device:PHIDDevice;ReportId:Byte):LongWord; forward;
function USBHIDDeviceCancelReport(Device:PHIDDevice;ReportId:Byte):LongWord; forward;

function USBHIDDeviceGetProtocol(Device:PHIDDevice;var Protocol:Byte):LongWord; forward;
function USBHIDDeviceSetProtocol(Device:PHIDDevice;Protocol:Byte):LongWord; forward;

function USBHIDDeviceGetInterval(Device:PHIDDevice;var Interval:LongWord):LongWord; forward;
function USBHIDDeviceSetInterval(Device:PHIDDevice;Interval:LongWord):LongWord; forward;

function USBHIDDeviceGetReportDescriptor(Device:PHIDDevice;Descriptor:PHIDReportDescriptor;Size:LongWord):LongWord; forward;
function USBHIDDeviceGetPhysicalDescriptorSet0(Device:PHIDDevice;Descriptor:PHIDPhysicalDescriptorSet0):LongWord; forward;
function USBHIDDeviceGetPhysicalDescriptorSet(Device:PHIDDevice;Descriptor:PHIDPhysicalDescriptorSet;Index:Byte;Size:LongWord):LongWord; forward;

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure USBHIDInit;
{Initialize the USB HID unit and USB HID driver}

{Note: Called only during system startup}
var
 Status:LongWord;
begin
 {}
 {Check Initialized}
 if USBHIDInitialized then Exit;

 {Create HID Driver}
 if USB_HID_REGISTER_DRIVER then
  begin
   USBHIDDriver:=USBDriverCreate;
   if USBHIDDriver <> nil then
    begin
     {Update USB HID Driver}
     {Driver}
     USBHIDDriver.Driver.DriverName:=USB_HID_DRIVER_NAME;
     {USB}
     USBHIDDriver.DriverBind:=USBHIDDriverBind;
     USBHIDDriver.DriverUnbind:=USBHIDDriverUnbind;

     {Register USB HID Driver}
     Status:=USBDriverRegister(USBHIDDriver);
     if Status <> USB_STATUS_SUCCESS then
      begin
       if USB_LOG_ENABLED then USBLogError(nil,'HID: Failed to register USB HID driver: ' + USBStatusToString(Status));

       {Destroy Driver}
       USBDriverDestroy(USBHIDDriver);
      end;
    end
   else
    begin
     if USB_LOG_ENABLED then USBLogError(nil,'HID: Failed to create USB HID driver');
    end;
  end;

 USBHIDInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{USB HID Functions}

{==============================================================================}
{==============================================================================}
{USB HID Helper Functions}
function USBHIDCheckDevice(Device:PUSBDevice):Boolean;
{Check if the supplied USB device is suitable for detection as a USB HID Device}
{Device: The USB device to check}
{Return: True if the device is suitable or False if it is not}
begin
 {}
 Result:=False;

 {Check Device}
 if Device = nil then Exit;

 {Check Class}
 case Device.Descriptor.bDeviceClass of
  USB_CLASS_CODE_HUB:Result:=False;
 else
  Result:=True;
 end;
end;

{==============================================================================}

function USBHIDCheckInterface(Device:PUSBDevice;Interrface:PUSBInterface):Boolean;
{Check if the supplied USB device and interface are a USB HID Device}
{Device: The USB device to check}
{Interrface: The USB interface to check}
{Return: True if the device and interface are a USB HID or False if not}
begin
 {}
 Result:=False;

 {Check Device}
 if Device = nil then Exit;

 {Check Interface}
 if Interrface = nil then Exit;

 {Check Interface Class}
 if Interrface.Descriptor.bInterfaceClass <> USB_CLASS_CODE_HID then Exit;

 Result:=True;
end;

{==============================================================================}

function USBHIDGetHIDDescriptor(Device:PUSBDevice;Interrface:PUSBInterface;Descriptor:PHIDDescriptor):LongWord;
{Get the HID Descriptor for a USB device and interface}
{Device: The USB device to get the descriptor for}
{Descriptor: Pointer to a USB HID Descriptor structure for the returned data}
{Return: USB_STATUS_SUCCESS if completed or another USB error code on failure}
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;

 {Check Interface}
 if Interrface = nil then Exit;

 {Check Descriptor}
 if Descriptor = nil then Exit;

 {Get Descriptor}
 Result:=USBControlRequest(Device,nil,USB_DEVICE_REQUEST_GET_DESCRIPTOR,USB_BMREQUESTTYPE_TYPE_STANDARD or USB_BMREQUESTTYPE_DIR_IN or USB_BMREQUESTTYPE_RECIPIENT_INTERFACE,(HID_DESCRIPTOR_TYPE_HID shl 8),Interrface.Descriptor.bInterfaceNumber,Descriptor,SizeOf(THIDDescriptor));
end;

{==============================================================================}

function USBHIDStatusToErrorCode(Status:LongWord):LongWord;
{Convert a USB_STATUS_* code to an ERROR_* code}
begin
 {}
 Result:=ERROR_OPERATION_FAILED;

 case Status of
  USB_STATUS_SUCCESS:Result:=ERROR_SUCCESS;
  USB_STATUS_DEVICE_DETACHED:Result:=ERROR_NOT_READY;
  USB_STATUS_DEVICE_UNSUPPORTED:Result:=ERROR_NOT_SUPPORTED;
  USB_STATUS_HARDWARE_ERROR:Result:=ERROR_RUNTIME_ERROR;
  USB_STATUS_INVALID_DATA:Result:=ERROR_INVALID_DATA;
  USB_STATUS_INVALID_PARAMETER:Result:=ERROR_INVALID_PARAMETER;
  USB_STATUS_NOT_PROCESSED:Result:=ERROR_NOT_PROCESSED;
  USB_STATUS_OUT_OF_MEMORY:Result:=ERROR_OUTOFMEMORY;
  USB_STATUS_TIMEOUT:Result:=ERROR_WAIT_TIMEOUT;
  USB_STATUS_UNSUPPORTED_REQUEST:Result:=ERROR_NOT_SUPPORTED;
  USB_STATUS_HARDWARE_STALL:Result:=ERROR_RUNTIME_ERROR;
  USB_STATUS_OPERATION_FAILED:Result:=ERROR_OPERATION_FAILED;
  USB_STATUS_NOT_BOUND:Result:=ERROR_NOT_ASSIGNED;
  USB_STATUS_ALREADY_BOUND:Result:=ERROR_ALREADY_ASSIGNED;
  USB_STATUS_NOT_READY:Result:=ERROR_NOT_READY;
  USB_STATUS_NOT_COMPLETED:Result:=ERROR_NOT_COMPLETED;
  USB_STATUS_CANCELLED:Result:=ERROR_CANCELLED;
  USB_STATUS_NOT_VALID:Result:=ERROR_NOT_VALID;
 end;
end;

{==============================================================================}

function USBHIDErrorCodeToUSBStatus(Error:LongWord):LongWord;
{Convert an ERROR_* code to a USB_STATUS_* code}
begin
 {}
 Result:=USB_STATUS_OPERATION_FAILED;

 case Error of
  ERROR_SUCCESS:Result:=USB_STATUS_SUCCESS;
  ERROR_NOT_READY:Result:=USB_STATUS_DEVICE_DETACHED;
  ERROR_NOT_SUPPORTED:Result:=USB_STATUS_DEVICE_UNSUPPORTED;
  ERROR_RUNTIME_ERROR:Result:=USB_STATUS_HARDWARE_ERROR;
  ERROR_INVALID_DATA:Result:=USB_STATUS_INVALID_DATA;
  ERROR_INVALID_PARAMETER:Result:=USB_STATUS_INVALID_PARAMETER;
  ERROR_NOT_PROCESSED:Result:=USB_STATUS_NOT_PROCESSED;
  ERROR_OUTOFMEMORY:Result:=USB_STATUS_OUT_OF_MEMORY;
  ERROR_WAIT_TIMEOUT:Result:=USB_STATUS_TIMEOUT;
  {ERROR_NOT_SUPPORTED:Result:=USB_STATUS_UNSUPPORTED_REQUEST;}
  {ERROR_RUNTIME_ERROR:Result:=USB_STATUS_HARDWARE_STALL;}
  ERROR_OPERATION_FAILED:Result:=USB_STATUS_OPERATION_FAILED;
  ERROR_NOT_ASSIGNED:Result:=USB_STATUS_NOT_BOUND;
  ERROR_ALREADY_ASSIGNED:Result:=USB_STATUS_ALREADY_BOUND;
  {ERROR_NOT_READY:Result:=USB_STATUS_NOT_READY;}
  ERROR_NOT_COMPLETED:Result:=USB_STATUS_NOT_COMPLETED;
  ERROR_CANCELLED:Result:=USB_STATUS_CANCELLED;
  ERROR_NOT_VALID:Result:=USB_STATUS_NOT_VALID;
 end;
end;

{==============================================================================}
{==============================================================================}
{USB HID Internal Functions}
function USBHIDDriverBind(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;
{Bind the USB HID driver to a USB device if it is suitable}
{Device: The USB device to attempt to bind to}
{Interrface: The USB interface to attempt to bind to (or nil for whole device)}
{Return: USB_STATUS_SUCCESS if completed, USB_STATUS_DEVICE_UNSUPPORTED if unsupported or another error code on failure}
var
 MinId:Byte;
 MaxId:Byte;
 Status:LongWord;
 HIDDevice:PUSBHIDDevice;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;

 {$IF DEFINED(USB_DEBUG) or DEFINED(HID_DEBUG)}
 if USB_LOG_ENABLED then USBLogDebug(Device,'HID: Attempting to bind USB device (Manufacturer=' + Device.Manufacturer + ' Product=' + Device.Product + ' Address=' + IntToStr(Device.Address) + ')');
 {$ENDIF}

 {Set Result}
 Result:=USB_STATUS_DEVICE_UNSUPPORTED;

 {Check Interface (Bind to interface only)}
 if Interrface = nil then Exit;

 {Check Device}
 if not USBHIDCheckDevice(Device) then Exit;

 {Check Interface}
 if not USBHIDCheckInterface(Device,Interrface) then Exit;

 {Create HID Device}
 HIDDevice:=PUSBHIDDevice(HIDDeviceCreateEx(SizeOf(TUSBHIDDevice)));
 if HIDDevice = nil then
  begin
   if USB_LOG_ENABLED then USBLogError(Device,'HID: Failed to create new HID device');

   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;

 {Update HID Device}
 {Device}
 HIDDevice.HID.Device.DeviceBus:=DEVICE_BUS_USB;
 HIDDevice.HID.Device.DeviceType:=HID_TYPE_USB;
 HIDDevice.HID.Device.DeviceFlags:=HIDDevice.HID.Device.DeviceFlags; {Don't override defaults (was MOUSE_FLAG_NONE)}
 HIDDevice.HID.Device.DeviceData:=Device;
 HIDDevice.HID.Device.DeviceDescription:=Trim(Device.Product);
 {HID}
 HIDDevice.HID.HIDState:=HID_STATE_ATTACHING;
 HIDDevice.HID.DeviceGetIdle:=USBHIDDeviceGetIdle;
 HIDDevice.HID.DeviceSetIdle:=USBHIDDeviceSetIdle;
 HIDDevice.HID.DeviceGetReport:=USBHIDDeviceGetReport;
 HIDDevice.HID.DeviceSetReport:=USBHIDDeviceSetReport;
 HIDDevice.HID.DeviceAllocateReport:=USBHIDDeviceAllocateReport;
 HIDDevice.HID.DeviceReleaseReport:=USBHIDDeviceReleaseReport;
 HIDDevice.HID.DeviceSubmitReport:=USBHIDDeviceSubmitReport;
 HIDDevice.HID.DeviceCancelReport:=USBHIDDeviceCancelReport;
 HIDDevice.HID.DeviceGetProtocol:=USBHIDDeviceGetProtocol;
 HIDDevice.HID.DeviceSetProtocol:=USBHIDDeviceSetProtocol;
 HIDDevice.HID.DeviceGetInterval:=USBHIDDeviceGetInterval;
 HIDDevice.HID.DeviceSetInterval:=USBHIDDeviceSetInterval;
 HIDDevice.HID.DeviceGetReportDescriptor:=USBHIDDeviceGetReportDescriptor;
 HIDDevice.HID.DeviceGetPhysicalDescriptorSet0:=USBHIDDeviceGetPhysicalDescriptorSet0;
 HIDDevice.HID.DeviceGetPhysicalDescriptorSet:=USBHIDDeviceGetPhysicalDescriptorSet;
 {USB}
 HIDDevice.USBDevice:=Device;
 HIDDevice.USBInterface:=Interrface;
 HIDDevice.HIDDescriptor:=nil;
 HIDDevice.WaiterThread:=INVALID_HANDLE_VALUE;

 {Register HID Device}
 if HIDDeviceRegister(@HIDDevice.HID) <> ERROR_SUCCESS then
  begin
   if USB_LOG_ENABLED then USBLogError(Device,'HID: Failed to register new HID device');

   {Destroy HID Device}
   HIDDeviceDestroy(@HIDDevice.HID);

   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;

 {Set Result}
 Result:=USB_STATUS_DEVICE_UNSUPPORTED;
 try
  {Check Endpoint (Must be IN interrupt)}
  HIDDevice.ReportEndpoint:=USBDeviceFindEndpointByType(Device,Interrface,USB_DIRECTION_IN,USB_TRANSFER_TYPE_INTERRUPT);
  if HIDDevice.ReportEndpoint = nil then Exit;

  {Check Endpoint (Optional OUT interrupt)}
  HIDDevice.OutputEndpoint:=USBDeviceFindEndpointByType(Device,Interrface,USB_DIRECTION_OUT,USB_TRANSFER_TYPE_INTERRUPT);

  {$IF DEFINED(USB_DEBUG) or DEFINED(HID_DEBUG)}
  if USB_LOG_ENABLED then USBLogDebug(Device,'HID: Checking class specific data');
  {$ENDIF}

  {Check Class Specific Data}
  if (Interrface.ClassData <> nil) and (Interrface.ClassSize >= SizeOf(THIDDescriptor)) then
   begin
    {Assign HID Descriptor}
    HIDDevice.HIDDescriptor:=PHIDDescriptor(Interrface.ClassData);
   end;

  {Check HID Descriptor}
  if (HIDDevice.HIDDescriptor = nil) or (HIDDevice.HIDDescriptor.bDescriptorType <> HID_DESCRIPTOR_TYPE_HID) or (HIDDevice.HIDDescriptor.bHIDDescriptorType <> HID_DESCRIPTOR_TYPE_REPORT) then
   begin
    {$IF DEFINED(USB_DEBUG) or DEFINED(HID_DEBUG)}
    if USB_LOG_ENABLED then USBLogDebug(Device,'HID: Reading HID descriptor');
    {$ENDIF}

    {Allocate HID Descriptor}
    HIDDevice.HIDDescriptor:=USBBufferAllocate(Device,SizeOf(THIDDescriptor));
    if HIDDevice.HIDDescriptor = nil then Exit;

    {Get HID Descriptor}
    Status:=USBHIDGetHIDDescriptor(Device,Interrface,HIDDevice.HIDDescriptor);
    if Status <> USB_STATUS_SUCCESS then
     begin
      if USB_LOG_ENABLED then USBLogError(Device,'HID: Failed to read HID descriptor: ' + USBStatusToString(Status));

      Exit;
     end;
   end;

  {Check HID Descriptor}
  if (HIDDevice.HIDDescriptor.bDescriptorType = HID_DESCRIPTOR_TYPE_HID) and (HIDDevice.HIDDescriptor.bHIDDescriptorType = HID_DESCRIPTOR_TYPE_REPORT) then
   begin
    {$IF DEFINED(USB_DEBUG) or DEFINED(HID_DEBUG)}
    if USB_LOG_ENABLED then USBLogDebug(Device,'HID: Reading HID report descriptors');
    {$ENDIF}

    {Allocate Report Descriptor}
    HIDDevice.HID.DescriptorSize:=HIDDevice.HIDDescriptor.wHIDDescriptorLength;
    HIDDevice.HID.Descriptor:=USBBufferAllocate(Device,HIDDevice.HID.DescriptorSize);
    if HIDDevice.HID.Descriptor = nil then Exit;

    {Get Report Descriptor}
    Status:=USBHIDErrorCodeToUSBStatus(HIDDeviceGetReportDescriptor(@HIDDevice.HID,HIDDevice.HID.Descriptor,HIDDevice.HID.DescriptorSize));
    if Status <> USB_STATUS_SUCCESS then
     begin
      if USB_LOG_ENABLED then USBLogError(Device,'HID: Failed to read HID report descriptor: ' + USBStatusToString(Status));

      Exit;
     end;

    {Update Interface}
    Interrface.DriverData:=HIDDevice;

    {Parse Collections}
    Status:=HIDParserParseCollections(@HIDDevice.HID,HIDDevice.HID.Collections,HIDDevice.HID.CollectionCount);
    if Status <> ERROR_SUCCESS then
     begin
      if USB_LOG_ENABLED then USBLogError(Device,'HID: Failed to parse collections from HID report descriptor: ' + ErrorToString(Status));

      Exit;
     end;

    {Get Maximum Report}
    Status:=HIDFindReportIds(@HIDDevice.HID,nil,MinId,MaxId);
    if Status <> ERROR_SUCCESS then
     begin
      if USB_LOG_ENABLED then USBLogError(Device,'HID: Failed to determine maximum report id from HID report descriptor: ' + ErrorToString(Status));

      Exit;
     end;

    {Allocate Requests}
    HIDDevice.ReportMaximum:=MaxId;
    HIDDevice.ReportRequests:=AllocMem((MaxId + 1) * SizeOf(PUSBHIDRequest));
    if HIDDevice.ReportRequests = nil then Exit;

    {Allocate Request}
    HIDDevice.ReportRequest:=USBRequestAllocate(HIDDevice.USBDevice,HIDDevice.ReportEndpoint,USBHIDDeviceReportComplete,USB_MAX_PACKET_SIZE,HIDDevice);
    if HIDDevice.ReportRequest = nil then Exit;

    {Check Interface SubClass (Get/SetProtocol only required for boot devices)}
    if Interrface.Descriptor.bInterfaceSubClass = HID_SUBCLASS_BOOT then
     begin
      {$IF DEFINED(USB_DEBUG) or DEFINED(HID_DEBUG)}
      if USB_LOG_ENABLED then USBLogDebug(Device,'HID: Enabling HID report protocol');
      {$ENDIF}

      {Set Report Protocol}
      Status:=USBHIDErrorCodeToUSBStatus(HIDDeviceSetProtocol(@HIDDevice.HID,HID_PROTOCOL_REPORT));
      if Status <> USB_STATUS_SUCCESS then
       begin
        if USB_LOG_ENABLED then USBLogError(Device,'HID: Failed to enable HID report protocol: ' + USBStatusToString(Status));

        Exit;
       end;
     end;

    {Bind Device}
    Status:=HIDDeviceBindDevice(@HIDDevice.HID);
    if Status <> ERROR_SUCCESS then
     begin
      if Status <> ERROR_NOT_SUPPORTED then
       begin
        if USB_LOG_ENABLED then USBLogError(Device,'HID: Failure during device bind: ' + ErrorToString(Status));

        {Don't fail the bind}
       end;

      {Bind Collections}
      Status:=HIDDeviceBindCollections(@HIDDevice.HID);
      if (Status <> ERROR_SUCCESS) and (Status <> ERROR_NOT_SUPPORTED) then
       begin
        if USB_LOG_ENABLED then USBLogError(Device,'HID: Failure during collection bind: ' + ErrorToString(Status));

        {Don't fail the bind}
       end;
     end;

    {Set State to Attached}
    if HIDDeviceSetState(@HIDDevice.HID,HID_STATE_ATTACHED) <> ERROR_SUCCESS then Exit;

    {Return Result}
    Result:=USB_STATUS_SUCCESS;
   end;
 finally
  if Result <> USB_STATUS_SUCCESS then
   begin
    {Release Request}
    if HIDDevice.ReportRequest <> nil then
     begin
      USBRequestRelease(HIDDevice.ReportRequest);
     end;

    {Free Requests}
    if HIDDevice.ReportRequests <> nil then
     begin
      FreeMem(HIDDevice.ReportRequests);
     end;

    {Free Collections}
    if HIDDevice.HID.Collections <> nil then
     begin
      HIDParserFreeCollections(HIDDevice.HID.Collections,HIDDevice.HID.CollectionCount);
     end;

    {Update Interface}
    Interrface.DriverData:=nil;

    {Release Report Descriptor}
    if HIDDevice.HID.Descriptor <> nil then
     begin
      USBBufferRelease(HIDDevice.HID.Descriptor);
     end;

    {Release HID Descriptor}
    if (HIDDevice.HIDDescriptor <> nil) and (HIDDevice.HIDDescriptor <> Pointer(Interrface.ClassData)) then
     begin
      USBBufferRelease(HIDDevice.HIDDescriptor);
     end;

    {Deregister HID Device}
    HIDDeviceDeregister(@HIDDevice.HID);

    {Destroy HID Device}
    HIDDeviceDestroy(@HIDDevice.HID);
   end;
 end;
end;

{==============================================================================}

function USBHIDDriverUnbind(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;
{Unbind the USB HID driver from a USB device}
{Device: The USB device to unbind from}
{Interrface: The USB interface to unbind from (or nil for whole device)}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}
var
 Count:LongWord;
 Message:TMessage;
 HIDDevice:PUSBHIDDevice;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;

 {Check Interface}
 if Interrface = nil then Exit;

 {Check Driver}
 if Interrface.Driver <> USBHIDDriver then Exit;

 {$IF DEFINED(USB_DEBUG) or DEFINED(HID_DEBUG)}
 if USB_LOG_ENABLED then USBLogDebug(Device,'HID: Unbinding USB device (Manufacturer=' + Device.Manufacturer + ' Product=' + Device.Product + ' Address=' + IntToStr(Device.Address) + ')');
 {$ENDIF}

 {Get HID Device}
 HIDDevice:=PUSBHIDDevice(Interrface.DriverData);
 if HIDDevice = nil then Exit;
 if HIDDevice.HID.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Set State to Detaching}
 Result:=USB_STATUS_OPERATION_FAILED;
 if HIDDeviceSetState(@HIDDevice.HID,HID_STATE_DETACHING) <> ERROR_SUCCESS then Exit;

 {Acquire the Lock}
 if MutexLock(HIDDevice.HID.Lock) <> ERROR_SUCCESS then Exit;

 {Cancel Reports}
 for Count:=0 to HIDDevice.ReportMaximum do
  begin
   HIDDeviceCancelReport(@HIDDevice.HID,Count);
  end;

 {Check Pending}
 if HIDDevice.PendingCount <> 0 then
  begin
   {$IF DEFINED(USB_DEBUG) or DEFINED(HID_DEBUG)}
   if USB_LOG_ENABLED then USBLogDebug(Device,'HID: Waiting for ' + IntToStr(HIDDevice.PendingCount) + ' pending requests to complete');
   {$ENDIF}

   {Wait for Pending}

   {Setup Waiter}
   HIDDevice.WaiterThread:=GetCurrentThreadId;

   {Release the Lock}
   MutexUnlock(HIDDevice.HID.Lock);

   {Wait for Message}
   ThreadReceiveMessage(Message);
  end
 else
  begin
   {Release the Lock}
   MutexUnlock(HIDDevice.HID.Lock);
  end;

 {Set State to Detached}
 if HIDDeviceSetState(@HIDDevice.HID,HID_STATE_DETACHED) <> ERROR_SUCCESS then Exit;

 {Unbind Collections}
 HIDDeviceUnbindCollections(@HIDDevice.HID,nil);

 {Unbind Device}
 HIDDeviceUnbindDevice(@HIDDevice.HID,nil);

 {Release Reports}
 for Count:=0 to HIDDevice.ReportMaximum do
  begin
   HIDDeviceReleaseReport(@HIDDevice.HID,Count);
  end;

 {Release Request}
 USBRequestRelease(HIDDevice.ReportRequest);

 {Free Requests}
 FreeMem(HIDDevice.ReportRequests);

 {Free Collections}
 HIDParserFreeCollections(HIDDevice.HID.Collections,HIDDevice.HID.CollectionCount);

 {Update Interface}
 Interrface.DriverData:=nil;

 {Release Report Descriptor}
 USBBufferRelease(HIDDevice.HID.Descriptor);

 {Release HID Descriptor}
 if HIDDevice.HIDDescriptor <> Pointer(Interrface.ClassData) then
  begin
   USBBufferRelease(HIDDevice.HIDDescriptor);
  end;

 {Deregister HID Device}
 if HIDDeviceDeregister(@HIDDevice.HID) <> ERROR_SUCCESS then Exit;

 {Destroy HID Device}
 HIDDeviceDestroy(@HIDDevice.HID);

 {Return Result}
 Result:=USB_STATUS_SUCCESS;
end;

{==============================================================================}

procedure USBHIDDeviceReportWorker(Request:PUSBRequest);
{Called (by a Worker thread) to process a completed USB request from a USB HID IN interrupt endpoint}
{Request: The USB request which has completed}
var
 Status:LongWord;
 Message:TMessage;
 ReportId:LongWord;
 HIDDevice:PUSBHIDDevice;
 HIDRequest:PUSBHIDRequest;
begin
 {}
 {Check Request}
 if Request = nil then Exit;

 {Get Device}
 HIDDevice:=PUSBHIDDevice(Request.DriverData);
 if HIDDevice <> nil then
  begin
   {Acquire the Lock}
   if MutexLock(HIDDevice.HID.Lock) = ERROR_SUCCESS then
    begin
     try
      {Update Statistics}
      Inc(HIDDevice.HID.ReceiveCount);

      {Check State}
      if HIDDevice.HID.HIDState = HID_STATE_DETACHING then
       begin
        {$IF DEFINED(USB_DEBUG) or DEFINED(HID_DEBUG)}
        if USB_LOG_ENABLED then USBLogDebug(Request.Device,'HID: Detachment pending, setting report request status to USB_STATUS_DEVICE_DETACHED');
        {$ENDIF}

        {Update Request}
        Request.Status:=USB_STATUS_DEVICE_DETACHED;
       end;

      {Check Result}
      if (Request.Status = USB_STATUS_SUCCESS) and (Request.ActualSize > 0) then
       begin
        {$IF DEFINED(USB_DEBUG) or DEFINED(HID_DEBUG)}
        if USB_LOG_ENABLED then USBLogDebug(Request.Device,'HID: Report received ' + IntToStr(Request.ActualSize) + ' bytes');
        {$ENDIF}

        {Get Report Id}
        ReportId:=0;
        if HIDDevice.ReportMaximum > 0 then
         begin
          ReportId:=PByte(Request.Data)^;

          {$IF DEFINED(USB_DEBUG) or DEFINED(HID_DEBUG)}
          if USB_LOG_ENABLED then USBLogDebug(Request.Device,'HID: Report Id is ' + IntToStr(ReportId));
          {$ENDIF}
         end;

        {Check Report Id}
        if ReportId <= HIDDevice.ReportMaximum then
         begin
          {Get Request}
          HIDRequest:=HIDDevice.ReportRequests[ReportId];

          {Check Collection}
          if (HIDRequest <> nil) and (HIDRequest.Collection <> nil) then
           begin
            {Check Consumer}
            if (HIDRequest.Collection.Consumer <> nil) and Assigned(HIDRequest.Collection.Consumer.ReportReceive) then
             begin
              {Deliver Report}
              Status:=HIDRequest.Collection.Consumer.ReportReceive(HIDRequest.Collection,ReportId,Request.Data,Request.ActualSize);
              if Status <> ERROR_SUCCESS then
               begin
                if USB_LOG_ENABLED then USBLogError(Request.Device,'HID: Consumer failed report receive (Status=' + ErrorToString(Status) + ')');

                {Update Statistics}
                Inc(HIDDevice.HID.ReceiveErrors);
               end;
             end;
           end;
         end
        else
         begin
          if USB_LOG_ENABLED then USBLogError(Request.Device,'HID: Invalid report id received, discarding report (Id=' + IntToStr(ReportId) + ')');

          {Update Statistics}
          Inc(HIDDevice.HID.ReceiveErrors);
         end;
       end
      else
       begin
        if USB_LOG_ENABLED then USBLogError(Request.Device,'HID: Failed report request (Status=' + USBStatusToString(Request.Status) + ', ActualSize=' + IntToStr(Request.ActualSize) + ')');

        {Update Statistics}
        Inc(HIDDevice.HID.ReceiveErrors);
       end;

      {Update Pending}
      Dec(HIDDevice.PendingCount);

      {Check State}
      if HIDDevice.HID.HIDState = HID_STATE_DETACHING then
       begin
        {Check Pending}
        if HIDDevice.PendingCount = 0 then
         begin
          {Check Waiter}
          if HIDDevice.WaiterThread <> INVALID_HANDLE_VALUE then
           begin
            {$IF DEFINED(USB_DEBUG) or DEFINED(HID_DEBUG)}
            if USB_LOG_ENABLED then USBLogDebug(Request.Device,'HID: Detachment pending, sending message to waiter thread (Thread=' + IntToHex(HIDDevice.WaiterThread,8) + ')');
            {$ENDIF}

            {Send Message}
            FillChar(Message,SizeOf(TMessage),0);
            ThreadSendMessage(HIDDevice.WaiterThread,Message);
            HIDDevice.WaiterThread:=INVALID_HANDLE_VALUE;
           end;
         end;
       end
      else
       begin
        {Update Pending}
        Inc(HIDDevice.PendingCount);

        {$IF DEFINED(USB_DEBUG) or DEFINED(HID_DEBUG)}
        if USB_LOG_ENABLED then USBLogDebug(Request.Device,'HID: Resubmitting report request');
        {$ENDIF}

        {Resubmit Request}
        Status:=USBRequestSubmit(Request);
        if Status <> USB_STATUS_SUCCESS then
         begin
          if USB_LOG_ENABLED then USBLogError(Request.Device,'HID: Failed to resubmit report request: ' + USBStatusToString(Status));

          {Update Pending}
          Dec(HIDDevice.PendingCount);
         end;
       end;
     finally
      {Release the Lock}
      MutexUnlock(HIDDevice.HID.Lock);
     end;
    end
   else
    begin
     if USB_LOG_ENABLED then USBLogError(Request.Device,'HID: Failed to acquire lock');
    end;
  end
 else
  begin
   if USB_LOG_ENABLED then USBLogError(Request.Device,'HID: Report request invalid');
  end;
end;

{==============================================================================}

procedure USBHIDDeviceReportComplete(Request:PUSBRequest);
{Called when a USB request from a HID device IN interrupt endpoint completes}
{Request: The USB request which has completed}

{Note: Request is passed to worker thread for processing to prevent blocking the USB completion}
begin
 {}
 {Check Request}
 if Request = nil then Exit;

 WorkerSchedule(0,TWorkerTask(USBHIDDeviceReportWorker),Request,nil)
end;

{==============================================================================}

function USBHIDDeviceGetIdle(Device:PHIDDevice;var Duration:Word;ReportId:Byte):LongWord;
{Get the idle rate from a USB HID device, interface and report id}
{Device: The USB HID device to get the idle rate from}
{Duration: A variable to receive the idle rate (in Milliseconds)}
{ReportId: The report id to get the idle rate from}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: HID_REQUEST_GET_IDLE is optional for all HID devices}
var
 Data:Byte;
 USBDevice:PUSBDevice;
 USBInterface:PUSBInterface;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Set Defaults}
 Data:=0;
 Duration:=0;

 {Check Device}
 if Device = nil then Exit;

 {Get Device and Interface}
 USBDevice:=PUSBHIDDevice(Device).USBDevice;
 USBInterface:=PUSBHIDDevice(Device).USBInterface;;

 {Check USB Device and Interface}
 if USBDevice = nil then Exit;
 if USBInterface = nil then Exit;

 {Get Idle}
 Result:=USBHIDStatusToErrorCode(USBControlRequest(USBDevice,nil,HID_REQUEST_GET_IDLE,USB_BMREQUESTTYPE_TYPE_CLASS or USB_BMREQUESTTYPE_DIR_IN or USB_BMREQUESTTYPE_RECIPIENT_INTERFACE,ReportId,USBInterface.Descriptor.bInterfaceNumber,@Data,SizeOf(Byte)));

 {Get Duration}
 Duration:=(Data * 4);
end;

{==============================================================================}

function USBHIDDeviceSetIdle(Device:PHIDDevice;Duration:Word;ReportId:Byte):LongWord;
{Set the idle rate for a USB HID device, interface and report id}
{Device: The USB HID device to set the idle rate for}
{Duration: The idle rate to set (in Milliseconds)}
{ReportId: The report id to set the idle rate for}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: HID_REQUEST_SET_IDLE is optional for all HID devices}
var
 USBDevice:PUSBDevice;
 USBInterface:PUSBInterface;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;

 {Get Device and Interface}
 USBDevice:=PUSBHIDDevice(Device).USBDevice;
 USBInterface:=PUSBHIDDevice(Device).USBInterface;;

 {Check USB Device and Interface}
 if USBDevice = nil then Exit;
 if USBInterface = nil then Exit;

 {Get Duration}
 Duration:=(Duration div 4);

 {Set Idle}
 Result:=USBHIDStatusToErrorCode(USBControlRequest(USBDevice,nil,HID_REQUEST_SET_IDLE,USB_BMREQUESTTYPE_TYPE_CLASS or USB_BMREQUESTTYPE_DIR_OUT or USB_BMREQUESTTYPE_RECIPIENT_INTERFACE,(Duration shl 8) or ReportId,USBInterface.Descriptor.bInterfaceNumber,nil,0));
end;

{==============================================================================}

function USBHIDDeviceGetReport(Device:PHIDDevice;ReportType,ReportId:Byte;ReportData:Pointer;ReportSize:LongWord):LongWord;
{Read a report by type and id from a USB HID device and interface}
{Device: The USB HID device to read the report from}
{ReportType: The report type to read (eg HID_REPORT_INPUT)}
{ReportId: The report id to read (eg HID_REPORTID_NONE)}
{ReportData: A pointer to a buffer to receive the report data}
{ReportSize: The size in bytes of the buffer pointed to by report data}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: HID_REQUEST_GET_REPORT is mandatory for all HID devices}
var
 USBDevice:PUSBDevice;
 USBInterface:PUSBInterface;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;

 {Get Device and Interface}
 USBDevice:=PUSBHIDDevice(Device).USBDevice;
 USBInterface:=PUSBHIDDevice(Device).USBInterface;;

 {Check USB Device and Interface}
 if USBDevice = nil then Exit;
 if USBInterface = nil then Exit;

 {Check Report Data}
 if ReportData = nil then Exit;

 {Get Report}
 Result:=USBHIDStatusToErrorCode(USBControlRequest(USBDevice,nil,HID_REQUEST_GET_REPORT,USB_BMREQUESTTYPE_TYPE_CLASS or USB_BMREQUESTTYPE_DIR_IN or USB_BMREQUESTTYPE_RECIPIENT_INTERFACE,(ReportType shl 8) or ReportId,USBInterface.Descriptor.bInterfaceNumber,ReportData,ReportSize));
end;

{==============================================================================}

function USBHIDDeviceSetReport(Device:PHIDDevice;ReportType,ReportId:Byte;ReportData:Pointer;ReportSize:LongWord):LongWord;
{Write a report by type and id to a USB HID device and interface}
{Device: The USB HID device to write the report to}
{ReportType: The report type to write (eg HID_REPORT_OUTPUT)}
{ReportId: The report id to write (eg HID_REPORTID_NONE)}
{ReportData: A pointer to a buffer containing the report data}
{ReportSize: The size in bytes of the buffer pointed to by report data}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: HID_REQUEST_SET_REPORT is optional for all HID devices, if a device declares
       an interrupt out endpoint then output reports are sent to the device using it}
var
 Count:LongWord;
 USBDevice:PUSBDevice;
 HIDDevice:PUSBHIDDevice;
 USBInterface:PUSBInterface;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;

 {Get Device and Interface}
 USBDevice:=PUSBHIDDevice(Device).USBDevice;
 USBInterface:=PUSBHIDDevice(Device).USBInterface;;

 {Check USB Device and Interface}
 if USBDevice = nil then Exit;
 if USBInterface = nil then Exit;

 {Get USB HID Device}
 HIDDevice:=PUSBHIDDevice(Device);

 {Check Report Data}
 if ReportData = nil then Exit;

 {Check Report Size}
 if ReportSize < 1 then Exit;

 {Check Report Type}
 if (ReportType = HID_REPORT_OUTPUT) and (HIDDevice.OutputEndpoint <> nil) then
  begin
   {Set Report Id}
   if HIDDevice.ReportMaximum > 0 then PByte(ReportData)^:=ReportId;

   {Set Report}
   Result:=USBHIDStatusToErrorCode(USBInterruptTransfer(USBDevice,HIDDevice.OutputEndpoint,ReportData,ReportSize,Count,INFINITE));
  end
 else
  begin
   {Set Report}
   Result:=USBHIDStatusToErrorCode(USBControlRequest(USBDevice,nil,HID_REQUEST_SET_REPORT,USB_BMREQUESTTYPE_TYPE_CLASS or USB_BMREQUESTTYPE_DIR_OUT or USB_BMREQUESTTYPE_RECIPIENT_INTERFACE,(ReportType shl 8) or ReportId,USBInterface.Descriptor.bInterfaceNumber,ReportData,ReportSize));
  end;
end;

{==============================================================================}

function USBHIDDeviceAllocateReport(Device:PHIDDevice;Collection:PHIDCollection;ReportId:Byte;ReportSize:LongWord):LongWord;
{Allocate and initialize an input report by id on a USB HID device}
{Device: The USB HID device to allocate the report on}
{Collection: The HID collection this request corresponds to}
{ReportId: The report id to allocate (eg HID_REPORTID_NONE)}
{ReportSize: The size in bytes to allocate for the report (Provider will handle alignment and other requirements)}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: An allocated report must be submitted before reports will be received from the device}
var
 HIDDevice:PUSBHIDDevice;
 HIDRequest:PUSBHIDRequest;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;

 {Check Collection}
 if Collection = nil then Exit;

 {Get USB HID Device}
 HIDDevice:=PUSBHIDDevice(Device);

 {Check Report Id}
 if ReportId > HIDDevice.ReportMaximum then Exit;

 {Check Report Size}
 if ReportSize = 0 then Exit;

 {Set Result}
 Result:=ERROR_NOT_VALID;

 {Check Allocated}
 if HIDDevice.ReportRequests[ReportId] <> nil then Exit;

 {Set Result}
 Result:=ERROR_OUTOFMEMORY;

 {Allocate Request}
 HIDRequest:=AllocMem(SizeOf(TUSBHIDRequest));
 if HIDRequest = nil then Exit;

 {Update Request}
 HIDRequest.Collection:=Collection;

 {Update Requests}
 HIDDevice.ReportRequests[ReportId]:=HIDRequest;

 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function USBHIDDeviceReleaseReport(Device:PHIDDevice;ReportId:Byte):LongWord;
{Release an input report by id from a USB HID device}
{Device: The USB HID device to release the report from}
{ReportId: The report id to allocate (eg HID_REPORTID_NONE)}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: If the report has been submitted it must be cancelled before being released}
var
 HIDDevice:PUSBHIDDevice;
 HIDRequest:PUSBHIDRequest;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;

 {Get USB HID Device}
 HIDDevice:=PUSBHIDDevice(Device);

 {Check Report Id}
 if ReportId > HIDDevice.ReportMaximum then Exit;

 {Set Result}
 Result:=ERROR_NOT_VALID;

 {Check Allocated}
 if HIDDevice.ReportRequests[ReportId] = nil then Exit;

 {Get Request}
 HIDRequest:=HIDDevice.ReportRequests[ReportId];

 {Update Requests}
 HIDDevice.ReportRequests[ReportId]:=nil;

 {Free Request}
 FreeMem(HIDRequest);

 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function USBHIDDeviceSubmitReport(Device:PHIDDevice;ReportId:Byte):LongWord;
{Submit an input report by id on a USB HID device}
{Device: The USB HID device to submit the report on}
{ReportId: The report id to submit (eg HID_REPORTID_NONE)}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: The report must be allocated then submitted before reports will be received from the device}
var
 Status:LongWord;
 HIDDevice:PUSBHIDDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;

 {Get USB HID Device}
 HIDDevice:=PUSBHIDDevice(Device);

 {Check Report Id}
 if ReportId > HIDDevice.ReportMaximum then Exit;

 {Set Result}
 Result:=ERROR_NOT_VALID;

 {Check Allocated}
 if HIDDevice.ReportRequests[ReportId] = nil then Exit;

 {Update Report Count}
 Inc(HIDDevice.ReportActive);

 {Check Report Count}
 if HIDDevice.ReportActive = 1 then
  begin
   {Update Pending}
   Inc(HIDDevice.PendingCount);

   {$IF DEFINED(USB_DEBUG) or DEFINED(HID_DEBUG)}
   if USB_LOG_ENABLED then USBLogDebug(HIDDevice.USBDevice,'HID: Submitting USB Request');
   {$ENDIF}

   {Submit USB Request}
   Status:=USBRequestSubmit(HIDDevice.ReportRequest);
   if Status <> USB_STATUS_SUCCESS then
    begin
     {Update Report Count}
     Dec(HIDDevice.ReportActive);

     {Update Pending}
     Dec(HIDDevice.PendingCount);

     if USB_LOG_ENABLED then USBLogError(HIDDevice.USBDevice,'HID: Failed to submit report request: ' + USBStatusToString(Status));

     Result:=ERROR_OPERATION_FAILED;
     Exit;
    end;
  end;

 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function USBHIDDeviceCancelReport(Device:PHIDDevice;ReportId:Byte):LongWord;
{Cancel an input report by id on a USB HID device}
{Device: The USB HID device to cancel the report on}
{ReportId: The report id to cancel (eg HID_REPORTID_NONE)}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: The report should be cancelled then released to stop receiving reports from the device}
var
 HIDDevice:PUSBHIDDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;

 {Get USB HID Device}
 HIDDevice:=PUSBHIDDevice(Device);

 {Check Report Id}
 if ReportId > HIDDevice.ReportMaximum then Exit;

 {Set Result}
 Result:=ERROR_NOT_VALID;

 {Check Allocated}
 if HIDDevice.ReportRequests[ReportId] = nil then Exit;

 {Check Report Count}
 if HIDDevice.ReportActive > 0 then
  begin
   {Update Report Count}
   Dec(HIDDevice.ReportActive);

   {Check Report Count}
   if HIDDevice.ReportActive = 0 then
    begin
     {$IF DEFINED(USB_DEBUG) or DEFINED(HID_DEBUG)}
     if USB_LOG_ENABLED then USBLogDebug(HIDDevice.USBDevice,'HID: Cancelling USB Request');
     {$ENDIF}

     {Cancel USB Request}
     USBRequestCancel(HIDDevice.ReportRequest);
    end;
  end;

 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function USBHIDDeviceGetProtocol(Device:PHIDDevice;var Protocol:Byte):LongWord;
{Get the report protocol from a USB HID device and interface}
{Device: The USB HID device to get the report protocol from}
{Protocol: A variable to receive the report protocol (eg HID_PROTOCOL_REPORT)}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: HID_REQUEST_GET_PROTOCOL is required only for HID boot devices}
var
 USBDevice:PUSBDevice;
 USBInterface:PUSBInterface;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Set Defaults}
 Protocol:=0;

 {Check Device}
 if Device = nil then Exit;

 {Get Device and Interface}
 USBDevice:=PUSBHIDDevice(Device).USBDevice;
 USBInterface:=PUSBHIDDevice(Device).USBInterface;;

 {Check USB Device and Interface}
 if USBDevice = nil then Exit;
 if USBInterface = nil then Exit;

 {Get Protocol}
 Result:=USBHIDStatusToErrorCode(USBControlRequest(USBDevice,nil,HID_REQUEST_GET_PROTOCOL,USB_BMREQUESTTYPE_TYPE_CLASS or USB_BMREQUESTTYPE_DIR_IN or USB_BMREQUESTTYPE_RECIPIENT_INTERFACE,0,USBInterface.Descriptor.bInterfaceNumber,@Protocol,SizeOf(Byte)));
end;

{==============================================================================}

function USBHIDDeviceSetProtocol(Device:PHIDDevice;Protocol:Byte):LongWord;
{Set the report protocol for a USB HID device and interface}
{Device: The USB HID device to set the report protocol for}
{Protocol: The report protocol to set (eg HID_PROTOCOL_REPORT)}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: HID_REQUEST_SET_PROTOCOL is required only for HID boot devices}
var
 USBDevice:PUSBDevice;
 USBInterface:PUSBInterface;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;

 {Get Device and Interface}
 USBDevice:=PUSBHIDDevice(Device).USBDevice;
 USBInterface:=PUSBHIDDevice(Device).USBInterface;;

 {Check USB Device and Interface}
 if USBDevice = nil then Exit;
 if USBInterface = nil then Exit;

 {Set Protocol}
 Result:=USBHIDStatusToErrorCode(USBControlRequest(USBDevice,nil,HID_REQUEST_SET_PROTOCOL,USB_BMREQUESTTYPE_TYPE_CLASS or USB_BMREQUESTTYPE_DIR_OUT or USB_BMREQUESTTYPE_RECIPIENT_INTERFACE,Protocol,USBInterface.Descriptor.bInterfaceNumber,nil,0));
end;

{==============================================================================}

function USBHIDDeviceGetInterval(Device:PHIDDevice;var Interval:LongWord):LongWord;
{Get the polling interval from a USB HID device and interface}
{Device: The USB HID device to get the polling interval from}
{Interval: A variable to receive the polling interval (in Milliseconds)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 USBDevice:PUSBDevice;
 ReportEndpoint:PUSBEndpointDescriptor;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Set Defaults}
 Interval:=0;

 {Check Device}
 if Device = nil then Exit;

 {Get Device and Endpoint}
 USBDevice:=PUSBHIDDevice(Device).USBDevice;
 ReportEndpoint:=PUSBHIDDevice(Device).ReportEndpoint;

 {Check USB Device and Endpoint}
 if USBDevice = nil then Exit;
 if ReportEndpoint = nil then Exit;

 {Get Interval}
 if USBDevice.Speed = USB_SPEED_HIGH then
  begin
   {Milliseconds = (1 shl (bInterval - 1)) div USB_UFRAMES_PER_MS}
   Interval:=(1 shl (ReportEndpoint.bInterval - 1)) div USB_UFRAMES_PER_MS;
  end
 else
  begin
   {Milliseconds = bInterval div USB_FRAMES_PER_MS}
   Interval:=ReportEndpoint.bInterval div USB_FRAMES_PER_MS;
  end;

 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function USBHIDDeviceSetInterval(Device:PHIDDevice;Interval:LongWord):LongWord;
{Set the polling interval for a USB HID device and interface}
{Device: The USB HID device to set the polling interval}
{Interval: The polling interval to set (in Milliseconds)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 USBDevice:PUSBDevice;
 ReportEndpoint:PUSBEndpointDescriptor;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;

 {Get Device and Endpoint}
 USBDevice:=PUSBHIDDevice(Device).USBDevice;
 ReportEndpoint:=PUSBHIDDevice(Device).ReportEndpoint;

 {Check USB Device and Endpoint}
 if USBDevice = nil then Exit;
 if ReportEndpoint = nil then Exit;

 {Get Interval}
 if USBDevice.Speed = USB_SPEED_HIGH then
  begin
   {Get Interval}
   Interval:=FirstBitSet(Interval * USB_UFRAMES_PER_MS) + 1;

   {Ensure no less than Interval} {Milliseconds = (1 shl (bInterval - 1)) div USB_UFRAMES_PER_MS}
   if ReportEndpoint.bInterval < Interval then ReportEndpoint.bInterval:=Interval;
  end
 else
  begin
   {Ensure no less than Interval} {Milliseconds = bInterval div USB_FRAMES_PER_MS}
   if ReportEndpoint.bInterval < Interval then ReportEndpoint.bInterval:=Interval;
  end;

 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function USBHIDDeviceGetReportDescriptor(Device:PHIDDevice;Descriptor:PHIDReportDescriptor;Size:LongWord):LongWord;
{Get the Report Descriptor for a USB HID device and interface}
{Device: The USB HID device to get the descriptor for}
{Descriptor: Pointer to a buffer to return the HID Report Descriptor}
{Size: The size in bytes of the buffer pointed to by Descriptor}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 USBDevice:PUSBDevice;
 USBInterface:PUSBInterface;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;

 {Get Device and Interface}
 USBDevice:=PUSBHIDDevice(Device).USBDevice;
 USBInterface:=PUSBHIDDevice(Device).USBInterface;;

 {Check USB Device and Interface}
 if USBDevice = nil then Exit;
 if USBInterface = nil then Exit;

 {Check Descriptor}
 if Descriptor = nil then Exit;

 {Get Descriptor}
 Result:=USBHIDStatusToErrorCode(USBControlRequest(USBDevice,nil,USB_DEVICE_REQUEST_GET_DESCRIPTOR,USB_BMREQUESTTYPE_TYPE_STANDARD or USB_BMREQUESTTYPE_DIR_IN or USB_BMREQUESTTYPE_RECIPIENT_INTERFACE,(HID_DESCRIPTOR_TYPE_REPORT shl 8),USBInterface.Descriptor.bInterfaceNumber,Descriptor,Size));
end;

{==============================================================================}

function USBHIDDeviceGetPhysicalDescriptorSet0(Device:PHIDDevice;Descriptor:PHIDPhysicalDescriptorSet0):LongWord;
{Get the HID Physical Descriptor Set 0 for a USB HID device and interface}
{Device: The USB HID device to get the descriptor for}
{Descriptor: Pointer to a USB HID Physical Descriptor Set 0 structure for the returned data}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=USBHIDDeviceGetPhysicalDescriptorSet(Device,PHIDPhysicalDescriptorSet(Descriptor),0,SizeOf(THIDPhysicalDescriptorSet0));
end;

{==============================================================================}

function USBHIDDeviceGetPhysicalDescriptorSet(Device:PHIDDevice;Descriptor:PHIDPhysicalDescriptorSet;Index:Byte;Size:LongWord):LongWord;
{Get a HID Physical Descriptor Set for a USB HID device and interface}
{Device: The USB HID device to get the descriptor for}
{Descriptor: Pointer to a USB HID Physical Descriptor Set structure for the returned data}
{Index: The index of the physical descriptor set to return}
{Size: The size in bytes of the buffer pointed to by Descriptor}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 USBDevice:PUSBDevice;
 USBInterface:PUSBInterface;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;

 {Get Device and Interface}
 USBDevice:=PUSBHIDDevice(Device).USBDevice;
 USBInterface:=PUSBHIDDevice(Device).USBInterface;;

 {Check USB Device and Interface}
 if USBDevice = nil then Exit;
 if USBInterface = nil then Exit;

 {Check Descriptor}
 if Descriptor = nil then Exit;

 {Get Descriptor}
 Result:=USBHIDStatusToErrorCode(USBControlRequest(USBDevice,nil,USB_DEVICE_REQUEST_GET_DESCRIPTOR,USB_BMREQUESTTYPE_TYPE_STANDARD or USB_BMREQUESTTYPE_DIR_IN or USB_BMREQUESTTYPE_RECIPIENT_INTERFACE,(HID_DESCRIPTOR_TYPE_PHYSICAL_DESCRIPTOR shl 8) or Index,USBInterface.Descriptor.bInterfaceNumber,Descriptor,Size));
end;

{==============================================================================}
{==============================================================================}

initialization
 USBHIDInit;

{==============================================================================}

finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
