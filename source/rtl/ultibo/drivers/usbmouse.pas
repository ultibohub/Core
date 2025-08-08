{
Ultibo USB Mouse driver unit.

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

 USB HID Device Class Definition 1_11.pdf

   http://www.usb.org/developers/hidpage/HID1_11.pdf

 USB HID Usage Tables 1_12v2.pdf

   http://www.usb.org/developers/hidpage/Hut1_12v2.pdf

USB Mouse Devices
=================

 The USB mouse driver in this unit uses HID Boot Protocol only and has been replaced by
 the HIDMouse unit which provides complete HID Report Protocol support for USB mice.

 It is retained here for legacy uses and backwards compatibility only.

 To use this driver in place of the default HID Mouse driver set the following
 configuration variables in your application during system initialization.

 HID_REGISTER_MOUSE := False;
 USB_MOUSE_REGISTER_DRIVER := True;

 This driver does not recognize devices that do not report themselves as boot mice, does
 not correctly recognize the wheel on many devices and is known to be incompatible with
 the touch pad included in some portable wireless keyboards.

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit USBMouse;

interface

uses
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  Devices,
  USB,
  Mouse,
  SysUtils;

{==============================================================================}
{Global definitions}
{$INCLUDE ..\core\GlobalDefines.inc}

{==============================================================================}
const
 {USB Mouse specific constants}
 USBMOUSE_DRIVER_NAME = 'USB Mouse Driver (HID boot protocol)'; {Name of USB mouse driver}

 USBMOUSE_MOUSE_DESCRIPTION = 'USB HID Mouse'; {Description of USB mouse device}

 {HID Interface Subclass types (See USB HID v1.11 specification)}
 USB_HID_SUBCLASS_NONE           = 0;     {Section 4.2}
 USB_HID_SUBCLASS_BOOT           = 1;     {Section 4.2}

 {HID Interface Protocol types (See USB HID v1.11 specification)}
 USB_HID_BOOT_PROTOCOL_NONE      = 0;     {Section 4.3}
 USB_HID_BOOT_PROTOCOL_KEYBOARD  = 1;     {Section 4.3}
 USB_HID_BOOT_PROTOCOL_MOUSE     = 2;     {Section 4.3}

 {HID Class Descriptor Types (See USB HID v1.11 specification)}
 USB_HID_DESCRIPTOR_TYPE_HID                  = $21;  {Section 7.1}
 USB_HID_DESCRIPTOR_TYPE_REPORT               = $22;  {Section 7.1}
 USB_HID_DESCRIPTOR_TYPE_PHYSICAL_DESCRIPTOR  = $23;  {Section 7.1}

 {HID Request types}
 USB_HID_REQUEST_GET_REPORT      = $01;
 USB_HID_REQUEST_GET_IDLE        = $02;
 USB_HID_REQUEST_GET_PROTOCOL    = $03;   {Section 7.2}
 USB_HID_REQUEST_SET_REPORT      = $09;
 USB_HID_REQUEST_SET_IDLE        = $0A;
 USB_HID_REQUEST_SET_PROTOCOL    = $0B;   {Section 7.2}

 {HID Protocol types}
 USB_HID_PROTOCOL_BOOT           = 0;     {Section 7.2.5}
 USB_HID_PROTOCOL_REPORT         = 1;     {Section 7.2.5}

 {HID Report types}
 USB_HID_REPORT_INPUT            = 1;     {Section 7.2.1}
 USB_HID_REPORT_OUTPUT           = 2;     {Section 7.2.1}
 USB_HID_REPORT_FEATURE          = 3;     {Section 7.2.1}

 {HID Report IDs}
 USB_HID_REPORTID_NONE           = 0;     {Section 7.2.1}

 {HID Boot Protocol Button bits}
 USB_HID_BOOT_LEFT_BUTTON    = (1 shl 0);
 USB_HID_BOOT_RIGHT_BUTTON   = (1 shl 1);
 USB_HID_BOOT_MIDDLE_BUTTON  = (1 shl 2);
 USB_HID_BOOT_SIDE_BUTTON    = (1 shl 3);
 USB_HID_BOOT_EXTRA_BUTTON   = (1 shl 4);

 {HID Boot Protocol Report data}
 USB_HID_BOOT_REPORT_SIZE  = 3;            {Appendix B of HID Device Class Definition 1.11}
 USB_HID_BOOT_DATA_SIZE    = 8;            {Allocate more than the minimum to allow for extra data}

{==============================================================================}
type
 {USB Mouse specific types}
 {USB HID Descriptor}
 PUSBHIDDescriptor = ^TUSBHIDDescriptor;
 TUSBHIDDescriptor = packed record
  bLength:Byte;
  bDescriptorType:Byte;
  bcdHID:Word;
  bCountryCode:Byte;
  bNumDescriptors:Byte;
  bHIDDescriptorType:Byte;
  wHIDDescriptorLength:Word;
  {Note: Up to two optional bHIDDescriptorType/wHIDDescriptorLength pairs after the Report descriptor details}
 end;

 {USB Mouse Device}
 PUSBMouseDevice = ^TUSBMouseDevice;
 TUSBMouseDevice = record
  {Mouse Properties}
  Mouse:TMouseDevice;
  {USB Properties}
  HIDInterface:PUSBInterface;            {USB HID Mouse Interface}
  ReportRequest:PUSBRequest;             {USB request for mouse report data}
  ReportEndpoint:PUSBEndpointDescriptor; {USB Mouse Interrupt IN Endpoint}
  HIDDescriptor:PUSBHIDDescriptor;       {USB HID Descriptor for mouse}
  ReportDescriptor:Pointer;              {USB HID Report Descriptor for mouse}
  PendingCount:LongWord;                 {Number of USB requests pending for this mouse}
  WaiterThread:TThreadId;                {Thread waiting for pending requests to complete (for mouse detachment)}
 end;

{==============================================================================}
{var}
 {USB Mouse specific variables}

{==============================================================================}
{Initialization Functions}
procedure USBMouseInit;

{==============================================================================}
{USB Mouse Functions}
function USBMouseDriverBind(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;
function USBMouseDriverUnbind(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;

procedure USBMouseReportWorker(Request:PUSBRequest);
procedure USBMouseReportComplete(Request:PUSBRequest);

{==============================================================================}
{USB Mouse Helper Functions}
function USBMouseCheckDevice(Device:PUSBDevice):Boolean;

function USBMouseDeviceSetProtocol(Mouse:PUSBMouseDevice;Protocol:Byte):LongWord;

function USBMouseDeviceGetHIDDescriptor(Mouse:PUSBMouseDevice;Descriptor:PUSBHIDDescriptor):LongWord;
function USBMouseDeviceGetReportDescriptor(Mouse:PUSBMouseDevice;Descriptor:Pointer;Size:LongWord):LongWord;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {USB Mouse specific variables}
 USBMouseInitialized:Boolean;

 USBMouseDriver:PUSBDriver;  {USB Mouse Driver interface (Set by USBMouseInit)}

{==============================================================================}
{==============================================================================}
{Forward Declarations}

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure USBMouseInit;
{Initialize the USB mouse driver}

{Note: Called only during system startup}
var
 Status:LongWord;
begin
 {}
 {Check Initialized}
 if USBMouseInitialized then Exit;

 {Create USB Mouse Driver}
 if USB_MOUSE_REGISTER_DRIVER then
  begin
   USBMouseDriver:=USBDriverCreate;
   if USBMouseDriver <> nil then
    begin
     {Update USB Mouse Driver}
     {Driver}
     USBMouseDriver.Driver.DriverName:=USBMOUSE_DRIVER_NAME;
     {USB}
     USBMouseDriver.DriverBind:=USBMouseDriverBind;
     USBMouseDriver.DriverUnbind:=USBMouseDriverUnbind;

     {Register USB Mouse Driver}
     Status:=USBDriverRegister(USBMouseDriver);
     if Status <> USB_STATUS_SUCCESS then
      begin
       if USB_LOG_ENABLED then USBLogError(nil,'Mouse: Failed to register USB mouse driver: ' + USBStatusToString(Status));

       {Destroy Driver}
       USBDriverDestroy(USBMouseDriver);
      end;
    end
   else
    begin
     if MOUSE_LOG_ENABLED then MouseLogError(nil,'Failed to create USB mouse driver');
    end;
  end;

 USBMouseInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{USB Mouse Functions}
function USBMouseDriverBind(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;
{Bind the Mouse driver to a USB device if it is suitable}
{Device: The USB device to attempt to bind to}
{Interrface: The USB interface to attempt to bind to (or nil for whole device)}
{Return: USB_STATUS_SUCCESS if completed, USB_STATUS_DEVICE_UNSUPPORTED if unsupported or another error code on failure}
var
 Status:LongWord;
 Interval:LongWord;
 Mouse:PUSBMouseDevice;
 ReportEndpoint:PUSBEndpointDescriptor;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;

 {$IF DEFINED(MOUSE_DEBUG) or DEFINED(USB_DEBUG)}
 if USB_LOG_ENABLED then USBLogDebug(Device,'Mouse: Attempting to bind USB device (Manufacturer=' + Device.Manufacturer + ' Product=' + Device.Product + ' Address=' + IntToStr(Device.Address) + ')');
 {$ENDIF}

 {Check Interface (Bind to interface only)}
 if Interrface = nil then
  begin
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;

 {Check Device}
 if not USBMouseCheckDevice(Device) then
  begin
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;

 {Check Interface (Must be HID boot protocol mouse)}
 if (Interrface.Descriptor.bInterfaceClass <> USB_CLASS_CODE_HID) or (Interrface.Descriptor.bInterfaceSubClass <> USB_HID_SUBCLASS_BOOT) or (Interrface.Descriptor.bInterfaceProtocol <> USB_HID_BOOT_PROTOCOL_MOUSE) then
  begin
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;

 {Check Endpoint (Must be IN interrupt)}
 ReportEndpoint:=USBDeviceFindEndpointByType(Device,Interrface,USB_DIRECTION_IN,USB_TRANSFER_TYPE_INTERRUPT);
 if ReportEndpoint = nil then
  begin
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;

 {Create Mouse}
 Mouse:=PUSBMouseDevice(MouseDeviceCreateEx(SizeOf(TUSBMouseDevice)));
 if Mouse = nil then
  begin
   if USB_LOG_ENABLED then USBLogError(Device,'Mouse: Failed to create new mouse device');

   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;

 {Update Mouse}
 {Device}
 Mouse.Mouse.Device.DeviceBus:=DEVICE_BUS_USB;
 Mouse.Mouse.Device.DeviceType:=MOUSE_TYPE_USB;
 Mouse.Mouse.Device.DeviceFlags:=Mouse.Mouse.Device.DeviceFlags; {Don't override defaults (was MOUSE_FLAG_NONE)}
 Mouse.Mouse.Device.DeviceData:=Device;
 Mouse.Mouse.Device.DeviceDescription:=USBMOUSE_MOUSE_DESCRIPTION;
 {Mouse}
 Mouse.Mouse.MouseState:=MOUSE_STATE_ATTACHING;
 {Driver}
 Mouse.Mouse.Properties.MaxButtons:=MOUSE_LEFT_BUTTON or MOUSE_RIGHT_BUTTON or MOUSE_MIDDLE_BUTTON;
 {USB}
 Mouse.HIDInterface:=Interrface;
 Mouse.ReportEndpoint:=ReportEndpoint;
 Mouse.WaiterThread:=INVALID_HANDLE_VALUE;

 {Allocate Report Request}
 Mouse.ReportRequest:=USBRequestAllocate(Device,ReportEndpoint,USBMouseReportComplete,USB_HID_BOOT_DATA_SIZE,Mouse);
 if Mouse.ReportRequest = nil then
  begin
   if USB_LOG_ENABLED then USBLogError(Device,'Mouse: Failed to allocate USB report request for mouse');

   {Destroy Mouse}
   MouseDeviceDestroy(@Mouse.Mouse);

   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;

 {Register Mouse}
 if MouseDeviceRegister(@Mouse.Mouse) <> ERROR_SUCCESS then
  begin
   if USB_LOG_ENABLED then USBLogError(Device,'Mouse: Failed to register new mouse device');

   {Release Report Request}
   USBRequestRelease(Mouse.ReportRequest);

   {Destroy Mouse}
   MouseDeviceDestroy(@Mouse.Mouse);

   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;

 {$IF DEFINED(MOUSE_DEBUG) or DEFINED(USB_DEBUG)}
 if USB_LOG_ENABLED then USBLogDebug(Device,'Mouse: Reading HID report descriptors');
 {$ENDIF}

 {Get HID Descriptor}
 Mouse.HIDDescriptor:=USBBufferAllocate(Device,SizeOf(TUSBHIDDescriptor));
 if Mouse.HIDDescriptor <> nil then
  begin
   Status:=USBMouseDeviceGetHIDDescriptor(Mouse,Mouse.HIDDescriptor);
   if Status <> USB_STATUS_SUCCESS then
    begin
     if USB_LOG_ENABLED then USBLogError(Device,'Mouse: Failed to read HID descriptor: ' + USBStatusToString(Status));

     {Don't fail the bind}
    end
   else
    begin
     if (Mouse.HIDDescriptor.bDescriptorType = USB_HID_DESCRIPTOR_TYPE_HID) and (Mouse.HIDDescriptor.bHIDDescriptorType = USB_HID_DESCRIPTOR_TYPE_REPORT) then
      begin
       {Get Report Descriptor}
       Mouse.ReportDescriptor:=USBBufferAllocate(Device,Mouse.HIDDescriptor.wHIDDescriptorLength);
       if Mouse.ReportDescriptor <> nil then
        begin
         Status:=USBMouseDeviceGetReportDescriptor(Mouse,Mouse.ReportDescriptor,Mouse.HIDDescriptor.wHIDDescriptorLength);
         if Status <> USB_STATUS_SUCCESS then
          begin
           if USB_LOG_ENABLED then USBLogError(Device,'Mouse: Failed to read HID report descriptor: ' + USBStatusToString(Status));

           {Don't fail the bind}
         {$IF DEFINED(MOUSE_DEBUG) or DEFINED(USB_DEBUG)}
          end
         else
          begin
           if USB_LOG_ENABLED then USBLogDebug(Device,'Mouse: Read ' + IntToStr(Mouse.HIDDescriptor.wHIDDescriptorLength) + ' byte HID report descriptor');
         {$ENDIF}
          end;
        end;
      end;
    end;
  end;

 {$IF DEFINED(MOUSE_DEBUG) or DEFINED(USB_DEBUG)}
 if USB_LOG_ENABLED then USBLogDebug(Device,'Mouse: Enabling HID boot protocol');
 {$ENDIF}

 {Set Boot Protocol}
 Status:=USBMouseDeviceSetProtocol(Mouse,USB_HID_PROTOCOL_BOOT);
 if Status <> USB_STATUS_SUCCESS then
  begin
   if USB_LOG_ENABLED then USBLogError(Device,'Mouse: Failed to enable HID boot protocol: ' + USBStatusToString(Status));

   {Release Report Request}
   USBRequestRelease(Mouse.ReportRequest);

   {Release HID Descriptor}
   USBBufferRelease(Mouse.HIDDescriptor);

   {Release Report Descriptor}
   USBBufferRelease(Mouse.ReportDescriptor);

   {Deregister Mouse}
   MouseDeviceDeregister(@Mouse.Mouse);

   {Destroy Mouse}
   MouseDeviceDestroy(@Mouse.Mouse);

   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;

 {Check Endpoint Interval}
 if USB_MOUSE_POLLING_INTERVAL > 0 then
  begin
   {Check Device Speed}
   if Device.Speed = USB_SPEED_HIGH then
    begin
     {Get Interval}
     Interval:=FirstBitSet(USB_MOUSE_POLLING_INTERVAL * USB_UFRAMES_PER_MS) + 1;

     {Ensure no less than Interval} {Milliseconds = (1 shl (bInterval - 1)) div USB_UFRAMES_PER_MS}
     if ReportEndpoint.bInterval < Interval then ReportEndpoint.bInterval:=Interval;
    end
   else
    begin
     {Ensure no less than USB_MOUSE_POLLING_INTERVAL} {Milliseconds = bInterval div USB_FRAMES_PER_MS}
     if ReportEndpoint.bInterval < USB_MOUSE_POLLING_INTERVAL then ReportEndpoint.bInterval:=USB_MOUSE_POLLING_INTERVAL;
    end;
  end;

 {Update Interface}
 Interrface.DriverData:=Mouse;

 {Update Pending}
 Inc(Mouse.PendingCount);

 {$IF DEFINED(MOUSE_DEBUG) or DEFINED(USB_DEBUG)}
 if USB_LOG_ENABLED then USBLogDebug(Device,'Mouse: Submitting report request');
 {$ENDIF}

 {Submit Request}
 Status:=USBRequestSubmit(Mouse.ReportRequest);
 if Status <> USB_STATUS_SUCCESS then
  begin
   if USB_LOG_ENABLED then USBLogError(Device,'Mouse: Failed to submit report request: ' + USBStatusToString(Status));

   {Update Pending}
   Dec(Mouse.PendingCount);

   {Release Report Request}
   USBRequestRelease(Mouse.ReportRequest);

   {Release HID Descriptor}
   USBBufferRelease(Mouse.HIDDescriptor);

   {Release Report Descriptor}
   USBBufferRelease(Mouse.ReportDescriptor);

   {Deregister Mouse}
   MouseDeviceDeregister(@Mouse.Mouse);

   {Destroy Mouse}
   MouseDeviceDestroy(@Mouse.Mouse);

   {Return Result}
   Result:=Status;
   Exit;
  end;

 {Set State to Attached}
 if MouseDeviceSetState(@Mouse.Mouse,MOUSE_STATE_ATTACHED) <> ERROR_SUCCESS then Exit;

 {Return Result}
 Result:=USB_STATUS_SUCCESS;
end;

{==============================================================================}

function USBMouseDriverUnbind(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;
{Unbind the Mouse driver from a USB device}
{Device: The USB device to unbind from}
{Interrface: The USB interface to unbind from (or nil for whole device)}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}
var
 Message:TMessage;
 Mouse:PUSBMouseDevice;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;

 {Check Interface}
 if Interrface = nil then Exit;

 {Check Driver}
 if Interrface.Driver <> USBMouseDriver then Exit;

 {$IF DEFINED(MOUSE_DEBUG) or DEFINED(USB_DEBUG)}
 if USB_LOG_ENABLED then USBLogDebug(Device,'Mouse: Unbinding USB device (Manufacturer=' + Device.Manufacturer + ' Product=' + Device.Product + ' Address=' + IntToStr(Device.Address) + ')');
 {$ENDIF}

 {Get Mouse}
 Mouse:=PUSBMouseDevice(Interrface.DriverData);
 if Mouse = nil then Exit;
 if Mouse.Mouse.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Set State to Detaching}
 Result:=USB_STATUS_OPERATION_FAILED;
 if MouseDeviceSetState(@Mouse.Mouse,MOUSE_STATE_DETACHING) <> ERROR_SUCCESS then Exit;

 {Acquire the Lock}
 if MutexLock(Mouse.Mouse.Lock) <> ERROR_SUCCESS then Exit;

 {Cancel Report Request}
 USBRequestCancel(Mouse.ReportRequest);

 {Check Pending}
 if Mouse.PendingCount <> 0 then
  begin
   {$IF DEFINED(MOUSE_DEBUG) or DEFINED(USB_DEBUG)}
   if USB_LOG_ENABLED then USBLogDebug(Device,'Mouse: Waiting for ' + IntToStr(Mouse.PendingCount) + ' pending requests to complete');
   {$ENDIF}

   {Wait for Pending}

   {Setup Waiter}
   Mouse.WaiterThread:=GetCurrentThreadId;

   {Release the Lock}
   MutexUnlock(Mouse.Mouse.Lock);

   {Wait for Message}
   ThreadReceiveMessage(Message);
  end
 else
  begin
   {Release the Lock}
   MutexUnlock(Mouse.Mouse.Lock);
  end;

 {Set State to Detached}
 if MouseDeviceSetState(@Mouse.Mouse,MOUSE_STATE_DETACHED) <> ERROR_SUCCESS then Exit;

 {Update Interface}
 Interrface.DriverData:=nil;

 {Release Report Request}
 USBRequestRelease(Mouse.ReportRequest);

 {Release HID Descriptor}
 USBBufferRelease(Mouse.HIDDescriptor);

 {Release Report Descriptor}
 USBBufferRelease(Mouse.ReportDescriptor);

 {Deregister Mouse}
 if MouseDeviceDeregister(@Mouse.Mouse) <> ERROR_SUCCESS then Exit;

 {Destroy Mouse}
 MouseDeviceDestroy(@Mouse.Mouse);

 {Return Result}
 Result:=USB_STATUS_SUCCESS;
end;

{==============================================================================}

procedure USBMouseReportWorker(Request:PUSBRequest);
{Called (by a Worker thread) to process a completed USB request from a USB mouse IN interrupt endpoint}
{Request: The USB request which has completed}
var
 Buffer:Pointer;
 Data:TMouseData;
 Status:LongWord;
 Message:TMessage;
 OffsetX:SmallInt;
 OffsetY:SmallInt;
 OffsetTemp:SmallInt;
 OffsetWheel:SmallInt;
 Mouse:PUSBMouseDevice;
begin
 {}
 {Check Request}
 if Request = nil then Exit;

 {Get Mouse}
 Mouse:=PUSBMouseDevice(Request.DriverData);
 if Mouse <> nil then
  begin
   {Acquire the Lock}
   if MutexLock(Mouse.Mouse.Lock) = ERROR_SUCCESS then
    begin
     try
      {Update Statistics}
      Inc(Mouse.Mouse.ReceiveCount);

      {Check State}
      if Mouse.Mouse.MouseState = MOUSE_STATE_DETACHING then
       begin
        {$IF DEFINED(MOUSE_DEBUG) or DEFINED(USB_DEBUG)}
        if USB_LOG_ENABLED then USBLogDebug(Request.Device,'Mouse: Detachment pending, setting report request status to USB_STATUS_DEVICE_DETACHED');
        {$ENDIF}

        {Update Request}
        Request.Status:=USB_STATUS_DEVICE_DETACHED;
       end;

      {Check Result}
      if (Request.Status = USB_STATUS_SUCCESS) and (Request.ActualSize >= USB_HID_BOOT_REPORT_SIZE) then
       begin
        {$IF DEFINED(MOUSE_DEBUG) or DEFINED(USB_DEBUG)}
        if USB_LOG_ENABLED then USBLogDebug(Request.Device,'Mouse: Report received');
        {$ENDIF}

        {A report was received from the USB mouse}
        Buffer:=Request.Data;

        {Clear Mouse Data}
        FillChar(Data,SizeOf(TMouseData),0);

        {Byte 0 is the Mouse buttons}
        Data.Buttons:=0;
        if (PByte(Buffer)^ and USB_HID_BOOT_LEFT_BUTTON) <> 0 then
         begin
          {Check Flags}
          if (Mouse.Mouse.Device.DeviceFlags and MOUSE_FLAG_SWAP_BUTTONS) = 0 then
           begin
            Data.Buttons:=Data.Buttons or MOUSE_LEFT_BUTTON;
           end
          else
           begin
            Data.Buttons:=Data.Buttons or MOUSE_RIGHT_BUTTON;
           end;
         end;
        if (PByte(Buffer)^ and USB_HID_BOOT_RIGHT_BUTTON) <> 0 then
         begin
          {Check Flags}
          if (Mouse.Mouse.Device.DeviceFlags and MOUSE_FLAG_SWAP_BUTTONS) = 0 then
           begin
            Data.Buttons:=Data.Buttons or MOUSE_RIGHT_BUTTON;
           end
          else
           begin
            Data.Buttons:=Data.Buttons or MOUSE_LEFT_BUTTON;
           end;
         end;
        if (PByte(Buffer)^ and USB_HID_BOOT_MIDDLE_BUTTON) <> 0 then Data.Buttons:=Data.Buttons or MOUSE_MIDDLE_BUTTON;
        if (PByte(Buffer)^ and USB_HID_BOOT_SIDE_BUTTON) <> 0 then Data.Buttons:=Data.Buttons or MOUSE_SIDE_BUTTON;
        if (PByte(Buffer)^ and USB_HID_BOOT_EXTRA_BUTTON) <> 0 then Data.Buttons:=Data.Buttons or MOUSE_EXTRA_BUTTON;

        {Byte 1 is the Mouse X offset}
        OffsetX:=PShortInt(PtrUInt(Buffer) + 1)^;

        {Byte 2 is the Mouse Y offset}
        OffsetY:=PShortInt(PtrUInt(Buffer) + 2)^;

        {Byte 3 is the Mouse Wheel offset}
        OffsetWheel:=PShortInt(PtrUInt(Buffer) + 3)^;

        {Check Swap}
        if (Mouse.Mouse.Device.DeviceFlags and MOUSE_FLAG_SWAP_XY) <> 0 then
         begin
          {Swap Offset X/Y}
          OffsetTemp:=OffsetX;
          OffsetX:=OffsetY;
          OffsetY:=OffsetTemp;
         end;

        {Check Invert}
        if (Mouse.Mouse.Device.DeviceFlags and  MOUSE_FLAG_INVERT_X) <> 0 then
         begin
          {Invert Offset X}
          OffsetX:=-OffsetX;
         end;
        if (Mouse.Mouse.Device.DeviceFlags and  MOUSE_FLAG_INVERT_Y) <> 0 then
         begin
          {Invert Offset Y}
          OffsetY:=-OffsetY;
         end;

        {Check Rotation}
        case Mouse.Mouse.Properties.Rotation of
         MOUSE_ROTATION_0:begin
           {Get Offset X and Y}
           Data.OffsetX:=OffsetX;
           Data.OffsetY:=OffsetY;
          end;
         MOUSE_ROTATION_90:begin
           {Swap Offset X and Y, Invert Offset X}
           Data.OffsetX:=-OffsetY;
           Data.OffsetY:=OffsetX;
          end;
         MOUSE_ROTATION_180:begin
           {Invert Offset X and Y}
           Data.OffsetX:=-OffsetX;
           Data.OffsetY:=-OffsetY;
          end;
         MOUSE_ROTATION_270:begin
           {Swap Offset X and Y, Invert Offset Y}
           Data.OffsetX:=OffsetY;
           Data.OffsetY:=-OffsetX;
          end;
        end;
        Data.OffsetWheel:=OffsetWheel;

        {Maximum X, Y and Wheel}
        Data.MaximumX:=0;
        Data.MaximumY:=0;
        Data.MaximumWheel:=0;

        {Insert Data}
        MouseInsertData(@Mouse.Mouse,@Data,True);
       end
      else
       begin
        if USB_LOG_ENABLED then USBLogError(Request.Device,'Mouse: Failed report request (Status=' + USBStatusToString(Request.Status) + ', ActualSize=' + IntToStr(Request.ActualSize) + ')');

        {Update Statistics}
        Inc(Mouse.Mouse.ReceiveErrors);
       end;

      {Update Pending}
      Dec(Mouse.PendingCount);

      {Check State}
      if Mouse.Mouse.MouseState = MOUSE_STATE_DETACHING then
       begin
        {Check Pending}
        if Mouse.PendingCount = 0 then
         begin
          {Check Waiter}
          if Mouse.WaiterThread <> INVALID_HANDLE_VALUE then
           begin
            {$IF DEFINED(MOUSE_DEBUG) or DEFINED(USB_DEBUG)}
            if USB_LOG_ENABLED then USBLogDebug(Request.Device,'Mouse: Detachment pending, sending message to waiter thread (Thread=' + IntToHex(Mouse.WaiterThread,8) + ')');
            {$ENDIF}

            {Send Message}
            FillChar(Message,SizeOf(TMessage),0);
            ThreadSendMessage(Mouse.WaiterThread,Message);
            Mouse.WaiterThread:=INVALID_HANDLE_VALUE;
           end;
         end;
       end
      else
       begin
        {Update Pending}
        Inc(Mouse.PendingCount);

        {$IF DEFINED(MOUSE_DEBUG) or DEFINED(USB_DEBUG)}
        if USB_LOG_ENABLED then USBLogDebug(Request.Device,'Mouse: Resubmitting report request');
        {$ENDIF}

        {Resubmit Request}
        Status:=USBRequestSubmit(Request);
        if Status <> USB_STATUS_SUCCESS then
         begin
          if USB_LOG_ENABLED then USBLogError(Request.Device,'Mouse: Failed to resubmit report request: ' + USBStatusToString(Status));

          {Update Pending}
          Dec(Mouse.PendingCount);
         end;
       end;
     finally
      {Release the Lock}
      MutexUnlock(Mouse.Mouse.Lock);
     end;
    end
   else
    begin
     if USB_LOG_ENABLED then USBLogError(Request.Device,'Mouse: Failed to acquire lock');
    end;
  end
 else
  begin
   if USB_LOG_ENABLED then USBLogError(Request.Device,'Mouse: Report request invalid');
  end;
end;

{==============================================================================}

procedure USBMouseReportComplete(Request:PUSBRequest);
{Called when a USB request from a USB mouse IN interrupt endpoint completes}
{Request: The USB request which has completed}
{Note: Request is passed to worker thread for processing to prevent blocking the USB completion}
begin
 {}
 {Check Request}
 if Request = nil then Exit;

 WorkerSchedule(0,TWorkerTask(USBMouseReportWorker),Request,nil)
end;

{==============================================================================}
{==============================================================================}
{USB Mouse Helper Functions}
function USBMouseCheckDevice(Device:PUSBDevice):Boolean;
{Check if the supplied USB device is suitable for detection as a HID Mouse Device}
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

function USBMouseDeviceSetProtocol(Mouse:PUSBMouseDevice;Protocol:Byte):LongWord;
{Set the report protocol for a USB mouse device}
{Mouse: The USB mouse device to set the report protocol for}
{Protocol: The report protocol to set (eg USB_HID_PROTOCOL_BOOT)}
{Return: USB_STATUS_SUCCESS if completed or another USB error code on failure}
var
 Device:PUSBDevice;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;

 {Check Mouse}
 if Mouse = nil then Exit;

 {Check Interface}
 if Mouse.HIDInterface = nil then Exit;

 {Get Device}
 Device:=PUSBDevice(Mouse.Mouse.Device.DeviceData);
 if Device = nil then Exit;

 {Set Protocol}
 Result:=USBControlRequest(Device,nil,USB_HID_REQUEST_SET_PROTOCOL,USB_BMREQUESTTYPE_TYPE_CLASS or USB_BMREQUESTTYPE_DIR_OUT or USB_BMREQUESTTYPE_RECIPIENT_INTERFACE,Protocol,Mouse.HIDInterface.Descriptor.bInterfaceNumber,nil,0);
end;

{==============================================================================}

function USBMouseDeviceGetHIDDescriptor(Mouse:PUSBMouseDevice;Descriptor:PUSBHIDDescriptor):LongWord;
{Get the HID Descriptor for a USB mouse device}
{Mouse: The USB mouse device to get the descriptor for}
{Descriptor: Pointer to a USB HID Descriptor structure for the returned data}
{Return: USB_STATUS_SUCCESS if completed or another USB error code on failure}
var
 Device:PUSBDevice;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;

 {Check Mouse}
 if Mouse = nil then Exit;

 {Check Descriptor}
 if Descriptor = nil then Exit;

 {Check Interface}
 if Mouse.HIDInterface = nil then Exit;

 {Get Device}
 Device:=PUSBDevice(Mouse.Mouse.Device.DeviceData);
 if Device = nil then Exit;

 {Get Descriptor}
 Result:=USBControlRequest(Device,nil,USB_DEVICE_REQUEST_GET_DESCRIPTOR,USB_BMREQUESTTYPE_TYPE_STANDARD or USB_BMREQUESTTYPE_DIR_IN or USB_BMREQUESTTYPE_RECIPIENT_INTERFACE,(USB_HID_DESCRIPTOR_TYPE_HID shl 8),Mouse.HIDInterface.Descriptor.bInterfaceNumber,Descriptor,SizeOf(TUSBHIDDescriptor));
end;

{==============================================================================}

function USBMouseDeviceGetReportDescriptor(Mouse:PUSBMouseDevice;Descriptor:Pointer;Size:LongWord):LongWord;
{Get the Report Descriptor for a USB mouse device}
{Mouse: The USB mouse device to get the descriptor for}
{Descriptor: Pointer to a buffer to return the USB Report Descriptor}
{Size: The size in bytes of the buffer pointed to by Descriptor}
{Return: USB_STATUS_SUCCESS if completed or another USB error code on failure}
var
 Device:PUSBDevice;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;

 {Check Mouse}
 if Mouse = nil then Exit;

 {Check Descriptor}
 if Descriptor = nil then Exit;

 {Check Interface}
 if Mouse.HIDInterface = nil then Exit;

 {Get Device}
 Device:=PUSBDevice(Mouse.Mouse.Device.DeviceData);
 if Device = nil then Exit;

 {Get Descriptor}
 Result:=USBControlRequest(Device,nil,USB_DEVICE_REQUEST_GET_DESCRIPTOR,USB_BMREQUESTTYPE_TYPE_STANDARD or USB_BMREQUESTTYPE_DIR_IN or USB_BMREQUESTTYPE_RECIPIENT_INTERFACE,(USB_HID_DESCRIPTOR_TYPE_REPORT shl 8),Mouse.HIDInterface.Descriptor.bInterfaceNumber,Descriptor,Size);
end;

{==============================================================================}
{==============================================================================}
{USB Mouse Internal Functions}

{==============================================================================}
{==============================================================================}

initialization
 USBMouseInit;

{==============================================================================}

finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.

