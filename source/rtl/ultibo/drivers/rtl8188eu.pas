{
Realtek 8188EU USB Wireless Driver.

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

  Linux - \drivers\staging\rtl8188eu\* - Copyright(c) 2007 - 2012 Realtek Corporation.

References
==========


Realtek 8188EU
==============

 The list of USB supported device IDs shown below for this driver is taken from the equivalent Linux driver
 but not all have been tested. In general if your device works with the 8188EU kernel module under Linux then
 it should also work with this driver.

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

{$IFNDEF FPC_DOTTEDUNITS}
unit RTL8188EU;
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
  Core.Network,
  Core.WiFi,
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
  Network,
  WiFi,
  SysUtils;
{$ENDIF FPC_DOTTEDUNITS}

{==============================================================================}
{Global definitions}
{$INCLUDE ..\core\GlobalDefines.inc}

{==============================================================================}
const
 {RTL8188EU specific constants}
 RTL8188EU_DRIVER_NAME = 'Realtek 8188EU USB Wireless Driver'; {Name of RTL8188EU driver}

 RTL8188EU_DEVICE_ID_COUNT = 8; {Number of supported Device IDs}

 RTL8188EU_DEVICE_ID:array[0..RTL8188EU_DEVICE_ID_COUNT - 1] of TUSBDeviceId = (
  {=== Realtek demoboard ===}
  (idVendor:USB_VENDORID_REALTEK;idProduct:$8179),  {8188EUS}
  (idVendor:USB_VENDORID_REALTEK;idProduct:$0179),  {8188ETV}
  {=== Customer ID ===}
  {****** 8188EUS ********}
  (idVendor:$056e;idProduct:$4008),                 {Elecom WDC-150SU2M}
  (idVendor:$07b8;idProduct:$8179),                 {Abocom - Abocom}
  (idVendor:$2001;idProduct:$330F),                 {DLink DWA-125 REV D1}
  (idVendor:$2001;idProduct:$3310),                 {Dlink DWA-123 REV D1}
  (idVendor:$2001;idProduct:$3311),                 {DLink GO-USB-N150 REV B1}
  (idVendor:$0df6;idProduct:$0076));                {Sitecom N150 v2}

{==============================================================================}
{type}
 {RTL8188EU specific types}

{==============================================================================}
{var}
 {RTL8188EU specific variables}

{==============================================================================}
{Initialization Functions}
procedure RTL8188EUInit;

{==============================================================================}
{RTL8188EU Network Functions}
function RTL8188EUDeviceOpen(Network:PNetworkDevice):LongWord;
function RTL8188EUDeviceClose(Network:PNetworkDevice):LongWord;
function RTL8188EUDeviceRead(Network:PNetworkDevice;Buffer:Pointer;Size:LongWord;var Length:LongWord):LongWord;
function RTL8188EUDeviceWrite(Network:PNetworkDevice;Buffer:Pointer;Size:LongWord;var Length:LongWord):LongWord;
function RTL8188EUDeviceControl(Network:PNetworkDevice;Request:Integer;Argument1:PtrUInt;var Argument2:PtrUInt):LongWord;

{==============================================================================}
{RTL8188EU USB Functions}
function RTL8188EUDriverBind(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;
function RTL8188EUDriverUnbind(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;

{==============================================================================}
{RTL8188EU Helper Functions}
function RTL8188EUCheckDevice(Device:PUSBDevice):LongWord;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {RTL8188EU specific variables}
 RTL8188EUInitialized:Boolean;

 RTL8188EUDriver:PUSBDriver;  {RTL8188EU Driver interface (Set by RTL8188EUInit)}

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure RTL8188EUInit;
var
 Status:LongWord;
begin
 {}
 {Check Initialized}
 if RTL8188EUInitialized then Exit;

 {Create RTL8188EU Wireless Driver}
 RTL8188EUDriver:=USBDriverCreate;
 if RTL8188EUDriver <> nil then
  begin
   {Update RTL8188EU Wireless Driver}
   {Driver}
   RTL8188EUDriver.Driver.DriverName:=RTL8188EU_DRIVER_NAME;
   {USB}
   RTL8188EUDriver.DriverBind:=RTL8188EUDriverBind;
   RTL8188EUDriver.DriverUnbind:=RTL8188EUDriverUnbind;

   {Register RTL8188EU Wireless Driver}
   Status:=USBDriverRegister(RTL8188EUDriver);
   if Status <> USB_STATUS_SUCCESS then
    begin
     if USB_LOG_ENABLED then USBLogError(nil,'RTL8188EU: Failed to register RTL8188EU driver: ' + USBStatusToString(Status));

     {Destroy Driver}
     USBDriverDestroy(RTL8188EUDriver);
    end;
  end
 else
  begin
   if USB_LOG_ENABLED then USBLogError(nil,'RTL8188EU: Failed to create RTL8188EU driver');
  end;

 RTL8188EUInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{RTL8188EU Network Functions}
function RTL8188EUDeviceOpen(Network:PNetworkDevice):LongWord;
{Implementation of NetworkDeviceOpen for the RTL8188EU device}
var
 Device:PUSBDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Network}
 if Network = nil then Exit;
 if Network.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Get Device}
 Device:=PUSBDevice(Network.Device.DeviceData);
 if Device = nil then Exit;

 //To Do
end;

{==============================================================================}

function RTL8188EUDeviceClose(Network:PNetworkDevice):LongWord;
{Implementation of NetworkDeviceClose for the RTL8188EU device}
var
 Device:PUSBDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Network}
 if Network = nil then Exit;
 if Network.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Get Device}
 Device:=PUSBDevice(Network.Device.DeviceData);
 if Device = nil then Exit;

 {Check State}
 Result:=ERROR_NOT_OPEN;
 if Network.NetworkState <> NETWORK_STATE_OPEN then Exit;

 {Set State to Closing}
 Result:=ERROR_OPERATION_FAILED;
 if NetworkDeviceSetState(Network,NETWORK_STATE_CLOSING) <> ERROR_SUCCESS then Exit;

 //To Do
end;

{==============================================================================}

function RTL8188EUDeviceRead(Network:PNetworkDevice;Buffer:Pointer;Size:LongWord;var Length:LongWord):LongWord;
{Implementation of NetworkDeviceRead for the RTL8188EU device}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Setup Length}
 Length:=0;

 {Check Network}
 if Network = nil then Exit;
 if Network.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Buffer}
 if Buffer = nil then Exit;

 {Check Size}
 if Size = 0 then Exit;

 {Check State}
 Result:=ERROR_NOT_READY;
 if Network.NetworkState <> NETWORK_STATE_OPEN then Exit;

 //To Do
end;

{==============================================================================}

function RTL8188EUDeviceWrite(Network:PNetworkDevice;Buffer:Pointer;Size:LongWord;var Length:LongWord):LongWord;
{Implementation of NetworkDeviceWrite for the RTL8188EU device}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Setup Length}
 Length:=0;

 {Check Network}
 if Network = nil then Exit;
 if Network.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Buffer}
 if Buffer = nil then Exit;

 //To Do
end;

{==============================================================================}

function RTL8188EUDeviceControl(Network:PNetworkDevice;Request:Integer;Argument1:PtrUInt;var Argument2:PtrUInt):LongWord;
{Implementation of NetworkDeviceControl for the RTL8188EU device}
var
 Device:PUSBDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Network}
 if Network = nil then Exit;
 if Network.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Get Device}
 Device:=PUSBDevice(Network.Device.DeviceData);
 if Device = nil then Exit;

 //To Do
end;

{==============================================================================}
{==============================================================================}
{RTL8188EU USB Functions}
function RTL8188EUDriverBind(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;
{Bind the RTL8188EU driver to a USB device if it is suitable}
{Device: The USB device to attempt to bind to}
{Interrface: The USB interface to attempt to bind to (or nil for whole device)}
{Return: USB_STATUS_SUCCESS if completed, USB_STATUS_DEVICE_UNSUPPORTED if unsupported or another error code on failure}
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;

 {$IFDEF RTL8188EU_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(Device,'RTL8188EU: Attempting to bind USB device (Manufacturer=' + Device.Manufacturer + ' Product=' + Device.Product + ' Address=' + IntToStr(Device.Address) + ')');
 {$ENDIF}

 {Check Interface (Bind to device only)}
 if Interrface <> nil then
  begin
   {$IFDEF RTL8188EU_DEBUG}
    if USB_LOG_ENABLED then USBLogDebug(Device,'RTL8188EU: Interface bind not supported by driver');
   {$ENDIF}
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;

 {Check RTL8188EU Device}
 if RTL8188EUCheckDevice(Device) <> USB_STATUS_SUCCESS then
  begin
   {$IFDEF RTL8188EU_DEBUG}
    if USB_LOG_ENABLED then USBLogDebug(Device,'RTL8188EU: Device not found in supported device list');
   {$ENDIF}
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;

 {Check Device Speed}
 if Device.Speed <> USB_SPEED_HIGH then
  begin
   {$IFDEF RTL8188EU_DEBUG}
    if USB_LOG_ENABLED then USBLogDebug(Device,'RTL8188EU: Device speed is not USB_SPEED_HIGH');
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

function RTL8188EUDriverUnbind(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;
{Unbind the RTL8188EU driver from a USB device}
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
 if Device.Driver <> RTL8188EUDriver then Exit;

 {$IFDEF RTL8188EU_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(Device,'RTL8188EU: Unbinding USB device (Manufacturer=' + Device.Manufacturer + ' Product=' + Device.Product + ' Address=' + IntToStr(Device.Address) + ')');
 {$ENDIF}

 //To Do
end;

{==============================================================================}
{==============================================================================}
{RTL8188EU Helper Functions}
function RTL8188EUCheckDevice(Device:PUSBDevice):LongWord;
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
 for Count:=0 to RTL8188EU_DEVICE_ID_COUNT - 1 do
  begin
   if (RTL8188EU_DEVICE_ID[Count].idVendor = Device.Descriptor.idVendor) and (RTL8188EU_DEVICE_ID[Count].idProduct = Device.Descriptor.idProduct) then
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
 RTL8188EUInit;

{==============================================================================}

finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.

