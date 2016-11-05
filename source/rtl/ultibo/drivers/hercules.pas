{
Hercules DJ Console Midi Device USB Driver

Copyright (C) 2016 - Rob Judd <judd@ob-wan.com>

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
 
 https://www.mixxx.org/wiki/doku.php/hardware_compatibility

  
References
==========

  
Hercules DJ Console
===================

 The Hercules DJ Console is designed to be used for live audio mixing. For use with Ultibo it is pressed into service as a general purpose interface to knobs, buttons and switches which can be used to simulate the final hardware on a product under design, or for actual production with complex products like Software Defined Radio. Of course, it can also be used as a mere DJ Console ... :)
 
 The following devices are notionally supported by this driver but only the Hercules DJ Console MP3 e2 Midi (VID:PID=06F8:B105) will be thoroughly tested. Others may not work since the hardware is unavailable for testing.
 
 Device name							ID                          Interface type

Hercules DJ Console MIDI			USB\VID_06F8&PID_b000&MI_04         HID
Hercules DJ Console Mk2 MIDI		USB\VID_06F8&PID_b100&MI_04 **      HID
Hercules DJ Console Rmx MIDI		USB\VID_06F8&PID_b101&MI_04         HID
Hercules DJ Control Steel MIDI		USB\VID_06F8&PID_b102&MI_01         HID
Hercules DJ Control MP3 e2 MIDI		USB\VID_06F8&PID_b105&MI_01 **      Bulk
Hercules DJ Console Mk4 MIDI		USB\VID_06F8&PID_b107&MI_05         Bulk
Hercules DJ Console 4-Mx MIDI		USB\VID_06F8&PID_b109&MI_05         MIDI
Hercules DJ Console 4-Mx MIDI		USB\VID_06F8&PID_b10a&MI_05         MIDI
Hercules DJ 4Set MIDI				USB\VID_06F8&PID_b10c&MI_05         HID
Hercules DJ 4Set MIDI				USB\VID_06F8&PID_b10d&MI_05         HID
Hercules DJ Control MP3 LE MIDI		USB\VID_06F8&PID_b114&MI_01 **      Bulk
Hercules DJ Control MP3 MIDI		USB\VID_06F8&PID_d000&MI_02         HID
Hercules DJ Control MP3 MIDI		USB\VID_06F8&PID_d001&MI_02         HID


}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit HERCULES; 

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,USB,Midi,SysUtils;

{==============================================================================}
{Global definitions}
{$INCLUDE ..\core\GlobalDefines.inc}

{==============================================================================}
const
 {Hercules specific constants}
 HERCULES_DRIVER_NAME = 'Hercules DJ Console USB Driver'; {Name of Hercules driver}
 
 HERCULES_DEVICE_ID_COUNT = 13; {Number of supported Device IDs}

 USB_VENDORID_HERCULES = $06F8; {Guillemot Corp.}
 
 HERCULES_DEVICE_ID:array[0..HERCULES_DEVICE_ID_COUNT - 1] of TUSBDeviceId = (
  (idVendor:USB_VENDORID_HERCULES;idProduct:$B000),  {Hercules DJ Console MIDI}
  (idVendor:USB_VENDORID_HERCULES;idProduct:$B100),  {Hercules DJ Console Mk2 MIDI}
  (idVendor:USB_VENDORID_HERCULES;idProduct:$B101),  {Hercules DJ Console Rmx MIDI}
  (idVendor:USB_VENDORID_HERCULES;idProduct:$B102),  {Hercules DJ Control Steel MIDI}
  (idVendor:USB_VENDORID_HERCULES;idProduct:$B105),  {Hercules DJ Control MP3 e2 MIDI}
  (idVendor:USB_VENDORID_HERCULES;idProduct:$B107),  {Hercules DJ Console Mk4 MIDI}
  (idVendor:USB_VENDORID_HERCULES;idProduct:$B109),  {Hercules DJ Console 4-Mx MIDI}
  (idVendor:USB_VENDORID_HERCULES;idProduct:$B10A),  {Hercules DJ Console 4-Mx MIDI}
  (idVendor:USB_VENDORID_HERCULES;idProduct:$B10C),  {Hercules DJ 4Set MIDI}                       
  (idVendor:USB_VENDORID_HERCULES;idProduct:$B10D),  {Hercules DJ 4Set MIDI}
  (idVendor:USB_VENDORID_HERCULES;idProduct:$B114),  {Hercules DJ Control MP3 LE MIDI}
  (idVendor:USB_VENDORID_HERCULES;idProduct:$D000),  {Hercules DJ Control MP3 MIDI}
  (idVendor:USB_VENDORID_HERCULES;idProduct:$D001)); {Hercules DJ Control MP3 MIDI}
 
{==============================================================================}
{type}
 {HERCULES specific types}
 
{==============================================================================}
{var}
 {HERCULES specific variables}
 
{==============================================================================}
{Initialization Functions}
procedure HERCULESInit;

{==============================================================================}
{HERCULES Midi Functions}
function HERCULESDeviceOpen(Midi:PMidiDevice):LongWord;
function HERCULESDeviceClose(Midi:PMidiDevice):LongWord;
function HERCULESDeviceRead(Midi:PMidiDevice;Buffer:Pointer;Size:LongWord;var Length:LongWord):LongWord;
function HERCULESDeviceWrite(Midi:PMidiDevice;Buffer:Pointer;Size:LongWord;var Length:LongWord):LongWord;
function HERCULESDeviceControl(Midi:PMidiDevice;Request:Integer;Argument1:LongWord;var Argument2:LongWord):LongWord;

{==============================================================================}
{HERCULES USB Functions}
function HERCULESDriverBind(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;
function HERCULESDriverUnbind(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;

{==============================================================================}
{HERCULES Helper Functions}
function HERCULESCheckDevice(Device:PUSBDevice):LongWord;
 
{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {HERCULES specific variables}
 HERCULESInitialized:Boolean; 
 
 HERCULESDriver:PUSBDriver;  {HERCULES Driver interface (Set by HERCULESInit)}

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure HERCULESInit;
var
 Status:LongWord;
begin
 {}
 {Check Initialized}
 if HERCULESInitialized then Exit;

 {Create HERCULES USB Driver}
 HERCULESDriver:=USBDriverCreate;
 if HERCULESDriver <> nil then
  begin
   {Update HERCULES USB Driver}
   {Driver}
   HERCULESDriver.Driver.DriverName:=HERCULES_DRIVER_NAME; 
   {USB}
   HERCULESDriver.DriverBind:=HERCULESDriverBind;
   HERCULESDriver.DriverUnbind:=HERCULESDriverUnbind;

   {Register HERCULES USB Driver}
   Status:=USBDriverRegister(HERCULESDriver);
   if Status <> USB_STATUS_SUCCESS then
    begin
     if USB_LOG_ENABLED then USBLogError(nil,'HERCULES: Failed to register HERCULES driver: ' + USBStatusToString(Status));
    end;
  end
 else
  begin
   if USB_LOG_ENABLED then USBLogError(nil,'HERCULES: Failed to create HERCULES driver');
  end;
 
 HERCULESInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{HERCULES Midi Functions}
function HERCULESDeviceOpen(Midi:PMidiDevice):LongWord;
{Implementation of MidiDeviceOpen for the HERCULES device}
var
 Device:PUSBDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Midi}
 if Midi = nil then Exit;
 if Midi.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Get Device}
 Device:=PUSBDevice(Midi.Device.DeviceData);
 if Device = nil then Exit;

 //To Do
end;
 
{==============================================================================}

function HERCULESDeviceClose(Midi:PMidiDevice):LongWord;
{Implementation of MidiDeviceClose for the HERCULES device}
var
 Device:PUSBDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Midi}
 if Midi = nil then Exit;
 if Midi.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Get Device}
 Device:=PUSBDevice(Midi.Device.DeviceData);
 if Device = nil then Exit;
 
 {Check State}
 Result:=ERROR_NOT_OPEN;
 if Midi.MidiState <> Midi_STATE_OPEN then Exit;
 
 {Set State to Closing}
 Result:=ERROR_OPERATION_FAILED;
 if MidiDeviceSetState(Midi,Midi_STATE_CLOSING) <> ERROR_SUCCESS then Exit;

 //To Do
end;
 
{==============================================================================}

function HERCULESDeviceRead(Midi:PMidiDevice;Buffer:Pointer;Size:LongWord;var Length:LongWord):LongWord;
{Implementation of MidiDeviceRead for the HERCULES device}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Setup Length}
 Length:=0;
 
 {Check Midi}
 if Midi = nil then Exit;
 if Midi.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check Buffer}
 if Buffer = nil then Exit;

 {Check Size}
 if Size = 0 then Exit;
 
 {Check State}
 Result:=ERROR_NOT_READY;
 if Midi.MidiState <> Midi_STATE_OPEN then Exit;

 //To Do
end;
 
{==============================================================================}

function HERCULESDeviceWrite(Midi:PMidiDevice;Buffer:Pointer;Size:LongWord;var Length:LongWord):LongWord;
{Implementation of MidiDeviceWrite for the HERCULES device}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Setup Length}
 Length:=0;
 
 {Check Midi}
 if Midi = nil then Exit;
 if Midi.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check Buffer}
 if Buffer = nil then Exit;

 //To Do
end;
 
{==============================================================================}

function HERCULESDeviceControl(Midi:PMidiDevice;Request:Integer;Argument1:LongWord;var Argument2:LongWord):LongWord;
{Implementation of MidiDeviceControl for the HERCULES device}
var
 Device:PUSBDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Midi}
 if Midi = nil then Exit;
 if Midi.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Get Device}
 Device:=PUSBDevice(Midi.Device.DeviceData);
 if Device = nil then Exit;

 //To Do
end;
 
{==============================================================================}
{==============================================================================}
{HERCULES USB Functions}
function HERCULESDriverBind(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;
{Bind the HERCULES driver to a USB device if it is suitable}
{Device: The USB device to attempt to bind to}
{Interrface: The USB interface to attempt to bind to (or nil for whole device)}
{Return: USB_STATUS_SUCCESS if completed, USB_STATUS_DEVICE_UNSUPPORTED if unsupported or another error code on failure}
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;

 {$IFDEF HERCULES_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(Device,'HERCULES: Attempting to bind USB device (' + ': Address ' + IntToStr(Device.Address) + ')'); //To Do //Device.Manufacturer //Device.Product
 {$ENDIF}
 
 {Check Interface (Bind to device only)}
 if Interrface <> nil then
  begin
   {$IFDEF HERCULES_DEBUG}
    if USB_LOG_ENABLED then USBLogDebug(Device,'HERCULES: Interface bind not supported by driver');
   {$ENDIF}
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;
 
 {Check HERCULES Device}
 if HERCULESCheckDevice(Device) <> USB_STATUS_SUCCESS then
  begin
   {$IFDEF HERCULES_DEBUG}
    if USB_LOG_ENABLED then USBLogDebug(Device,'HERCULES: Device not found in supported device list');
   {$ENDIF}
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;

 {Check Device Speed}
 if Device.Speed <> USB_SPEED_HIGH then
  begin
   {$IFDEF HERCULES_DEBUG}
    if USB_LOG_ENABLED then USBLogDebug(Device,'HERCULES: Device speed is not USB_SPEED_HIGH');
   {$ENDIF}
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;

 //To Do
 
 {Return Result}
 Result:=USB_STATUS_SUCCESS;
end;
  
{==============================================================================}

function HERCULESDriverUnbind(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;
{Unbind the HERCULES driver from a USB device}
{Device: The USB device to unbind from}
{Interrface: The USB interface to unbind from (or nil for whole device)}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Device}
 if Device = nil then Exit;

 {Check Interface}
 if Interrface <> nil then Exit;
 
 {Check Driver}
 if Device.Driver <> HERCULESDriver then Exit;
 
 {$IFDEF HERCULES_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(Device,'HERCULES: Unbinding (' + ': Address ' + IntToStr(Device.Address) + ')'); //To Do //Device.Manufacturer //Device.Product
 {$ENDIF}

 //To Do
end;
 
{==============================================================================}
{==============================================================================}
{HERCULES Helper Functions}
function HERCULESCheckDevice(Device:PUSBDevice):LongWord;
{Check the Vendor and Device ID against the supported devices}
{Device: USB device to check}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}      
var
 Count:Integer;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Device}
 if Device = nil then Exit;

 {Check Device IDs}
 for Count:=0 to HERCULES_DEVICE_ID_COUNT - 1 do
  begin
   if (HERCULES_DEVICE_ID[Count].idVendor = Device.Descriptor.idVendor) and (HERCULES_DEVICE_ID[Count].idProduct = Device.Descriptor.idProduct) then
    begin
     Result:=USB_STATUS_SUCCESS;
     Exit;
    end;
  end;

 Result:=USB_STATUS_DEVICE_UNSUPPORTED;
end;

{==============================================================================}
{==============================================================================}

initialization
 HERCULESInit;
 
{==============================================================================}
 
finalization
 {Nothing}
 
{==============================================================================}
{==============================================================================}

end.
 