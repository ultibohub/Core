{
Realtek 8192CU USB Wireless Driver.

Copyright (C) 2016 - SoftOz Pty Ltd.

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

  Linux - \drivers\net\wireless\rtl8192cu\* - Copyright(c) 2007 - 2011 Realtek Corporation.
  Linux - \drivers\net\wireless\realtek\rtl8xxxu\* - Copyright (c) 2014 - 2015 Jes Sorensen.
  Linux - \drivers\net\wireless\rtlwifi\* - Copyright(c) 2009-2012  Realtek Corporation.
  
References
==========

 RTL8192CU - http://www.realtek.com.tw/products/productsView.aspx?Langid=1&PFid=48&Level=5&Conn=4&ProdID=277
  
Realtek 8192CU
==============

 The Realtek RTL8192CU is a single chip IEEE802.11 b/g/n WLAN controller with USB2.0 interface.
 
 The list of USB supported device IDs shown below for this driver is taken from the equivalent Linux driver
 but not all have been tested. In general if your device works with the 8192cu kernel module under Linux then
 it should also work with this driver.
 
 Only USB devices are currently supported, PCI and other forms of the same chipset will require a separate 
 driver unit for support.
 
}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit RTL8192CU; 

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,USB,Network,WiFi,SysUtils;

{==============================================================================}
{Global definitions}
{$INCLUDE ..\core\GlobalDefines.inc}

{==============================================================================}
const
 {RTL8192CU specific constants}
 RTL8192CU_DRIVER_NAME = 'Realtek 8192CU USB Wireless Driver'; {Name of RTL8192CU driver}
 
 RTL8192CU_DEVICE_ID_COUNT = 80; {Number of supported Device IDs}

 RTL8192CU_DEVICE_ID:array[0..RTL8192CU_DEVICE_ID_COUNT - 1] of TUSBDeviceId = (
  (idVendor:USB_VENDORID_REALTEK;idProduct:$8191),  {Default ID}
  {****** 8188CUS ********}
  (idVendor:USB_VENDORID_REALTEK;idProduct:$8176),  {8188cu 1*1 dongole}
  (idVendor:USB_VENDORID_REALTEK;idProduct:$8170),  {8188CE-VAU USB minCard}
  (idVendor:USB_VENDORID_REALTEK;idProduct:$817E),  {8188CE-VAU USB minCard}
  (idVendor:USB_VENDORID_REALTEK;idProduct:$817A),  {8188cu Slim Solo}
  (idVendor:USB_VENDORID_REALTEK;idProduct:$817B),  {8188cu Slim Combo}
  (idVendor:USB_VENDORID_REALTEK;idProduct:$817D),  {8188RU High-power USB Dongle}
  (idVendor:USB_VENDORID_REALTEK;idProduct:$8754),  {8188 Combo for BC4}
  (idVendor:USB_VENDORID_REALTEK;idProduct:$817F),  {8188RU}
  (idVendor:USB_VENDORID_REALTEK;idProduct:$818A),  {RTL8188CUS-VL}
  (idVendor:USB_VENDORID_REALTEK;idProduct:$018A),  {RTL8188CTV}
  (idVendor:USB_VENDORID_REALTEK;idProduct:$17C0),  {RTK demoboard - USB-N10E}
  {****** 8192CUS ********}                         
  (idVendor:USB_VENDORID_REALTEK;idProduct:$8177),  {8191cu 1*2}
  (idVendor:USB_VENDORID_REALTEK;idProduct:$8178),  {8192cu 2*2}
  (idVendor:USB_VENDORID_REALTEK;idProduct:$817C),  {8192CE-VAU USB minCard}
  (idVendor:USB_VENDORID_REALTEK;idProduct:$8191),  {8192CU 2*2}
  (idVendor:$1058;idProduct:$0631),                 {Alpha, 8192CU}
  {****** 8188CUS Dongle ********}                  
  (idVendor:$2019;idProduct:$ED17),                 {PCI - Edimax}
  (idVendor:$0DF6;idProduct:$0052),                 {Sitecom - Edimax}
  (idVendor:$7392;idProduct:$7811),                 {Edimax - Edimax}
  (idVendor:$07B8;idProduct:$8189),                 {Abocom - Abocom}
  (idVendor:$0EB0;idProduct:$9071),                 {NO Brand - Etop}
  (idVendor:$06F8;idProduct:$E033),                 {Hercules - Edimax}
  (idVendor:$103C;idProduct:$1629),                 {HP - Lite-On ,8188CUS Slim Combo}
  (idVendor:$2001;idProduct:$3308),                 {D-Link - Alpha}
  (idVendor:$050D;idProduct:$1102),                 {Belkin - Edimax}
  (idVendor:$2019;idProduct:$AB2A),                 {Planex - Abocom}
  (idVendor:$20F4;idProduct:$648B),                 {TRENDnet - Cameo}
  (idVendor:$4855;idProduct:$0090),                 { - Feixun}
  (idVendor:$13D3;idProduct:$3357),                 { - AzureWave}
  (idVendor:$0DF6;idProduct:$005C),                 {Sitecom - Edimax}
  (idVendor:$0BDA;idProduct:$5088),                 {Thinkware - CC&C}
  (idVendor:$4856;idProduct:$0091),                 {NetweeN - Feixun}
  (idVendor:$0846;idProduct:$9041),                 {Netgear - Cameo}
  (idVendor:$2019;idProduct:$4902),                 {Planex - Etop}
  (idVendor:$2019;idProduct:$AB2E),                 {SW-WF02-AD15 -Abocom}
  (idVendor:$2001;idProduct:$330B),                 {D-LINK - T&W}
  (idVendor:$CDAB;idProduct:$8010),                 {- - compare}
  (idVendor:$0B05;idProduct:$17BA),                 {ASUS - Edimax}
  (idVendor:$0BDA;idProduct:$1E1E),                 {Intel - -}
  (idVendor:$04BB;idProduct:$094c),                 {I-O DATA - Edimax}
  {****** 8188CTV ********}                         
  (idVendor:$CDAB;idProduct:$8011),                 {- - compare}
  (idVendor:$0BDA;idProduct:$0A8A),                 {Sony - Foxconn}
  {****** 8188 RU ********}                         
  (idVendor:$0BDA;idProduct:$317F),                 {Netcore,Netcore}
  {****** 8188CE-VAU ********}                      
  (idVendor:$13D3;idProduct:$3359),                 { - Azwave}
  (idVendor:$13D3;idProduct:$3358),                 { - Azwave}
  {****** 8188CUS Slim Solo********}                
  (idVendor:$04F2;idProduct:$AFF7),                 {XAVI - XAVI}
  (idVendor:$04F2;idProduct:$AFF9),                 {XAVI - XAVI}
  (idVendor:$04F2;idProduct:$AFFA),                 {XAVI - XAVI}
  {****** 8188CUS Slim Combo ********}
  (idVendor:$04F2;idProduct:$AFF8),                 {XAVI - XAVI}
  (idVendor:$04F2;idProduct:$AFFB),                 {XAVI - XAVI}
  (idVendor:$04F2;idProduct:$AFFC),                 {XAVI - XAVI}
  (idVendor:$2019;idProduct:$1201),                 {Planex - Vencer}
  {****** 8192CUS Dongle ********}                  
  (idVendor:$2001;idProduct:$3307),                 {D-Link - Cameo}
  (idVendor:$2001;idProduct:$330A),                 {D-Link - Alpha}
  (idVendor:$2001;idProduct:$3309),                 {D-Link - Alpha}
  (idVendor:$2001;idProduct:$330D),                 {D-Link - DWA 131}
  (idVendor:$0586;idProduct:$341F),                 {Zyxel - Abocom}
  (idVendor:$7392;idProduct:$7822),                 {Edimax - Edimax}
  (idVendor:$2019;idProduct:$AB2B),                 {Planex - Abocom}
  (idVendor:$07B8;idProduct:$8178),                 {Abocom - Abocom}
  (idVendor:$07AA;idProduct:$0056),                 {ATKK - Gemtek}
  (idVendor:$4855;idProduct:$0091),                 { - Feixun}
  (idVendor:$050D;idProduct:$2102),                 {Belkin - Sercomm}
  (idVendor:$050D;idProduct:$2103),                 {Belkin - Edimax}
  (idVendor:$20F4;idProduct:$624D),                 {TRENDnet}
  (idVendor:$0DF6;idProduct:$0061),                 {Sitecom - Edimax}
  (idVendor:$0B05;idProduct:$17AB),                 {ASUS - Edimax}
  (idVendor:$0846;idProduct:$9021),                 {Netgear - Sercomm}
  (idVendor:$0846;idProduct:$F001),                 {Netgear - Sercomm}
  (idVendor:$0E66;idProduct:$0019),                 {Hawking,Edimax}
  (idVendor:$0E66;idProduct:$0020),                 {Hawking  - Edimax}
  (idVendor:$050D;idProduct:$1004),                 {Belkin - Edimax}
  (idVendor:$0BDA;idProduct:$2E2E),                 {Intel - -}
  (idVendor:$2357;idProduct:$0100),                 {TP-Link - TP-Link}
  (idVendor:$06F8;idProduct:$E035),                 {Hercules - Edimax}
  (idVendor:$04BB;idProduct:$0950),                 {IO-DATA - Edimax}
  (idVendor:$0DF6;idProduct:$0070),                 {Sitecom - Edimax}
  (idVendor:$0789;idProduct:$016D),                 {LOGITEC - Edimax}
  {****** 8192CE-VAU  ********}
  (idVendor:USB_VENDORID_REALTEK;idProduct:$8186)); {Intel-Xavi( Azwave)}
 
{==============================================================================}
{type}
 {RTL8192CU specific types}
 
{==============================================================================}
{var}
 {RTL8192CU specific variables}
 
{==============================================================================}
{Initialization Functions}
procedure RTL8192CUInit;

{==============================================================================}
{RTL8192CU Network Functions}
function RTL8192CUDeviceOpen(Network:PNetworkDevice):LongWord;
function RTL8192CUDeviceClose(Network:PNetworkDevice):LongWord;
function RTL8192CUDeviceRead(Network:PNetworkDevice;Buffer:Pointer;Size:LongWord;var Length:LongWord):LongWord;
function RTL8192CUDeviceWrite(Network:PNetworkDevice;Buffer:Pointer;Size:LongWord;var Length:LongWord):LongWord;
function RTL8192CUDeviceControl(Network:PNetworkDevice;Request:Integer;Argument1:LongWord;var Argument2:LongWord):LongWord;

{==============================================================================}
{RTL8192CU USB Functions}
function RTL8192CUDriverBind(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;
function RTL8192CUDriverUnbind(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;

{==============================================================================}
{RTL8192CU Helper Functions}
function RTL8192CUCheckDevice(Device:PUSBDevice):LongWord;
 
{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {RTL8192CU specific variables}
 RTL8192CUInitialized:Boolean; 
 
 RTL8192CUDriver:PUSBDriver;  {RTL8192CU Driver interface (Set by RTL8192CUInit)}

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure RTL8192CUInit;
var
 Status:LongWord;
begin
 {}
 {Check Initialized}
 if RTL8192CUInitialized then Exit;

 {Create RTL8192CU Wireless Driver}
 RTL8192CUDriver:=USBDriverCreate;
 if RTL8192CUDriver <> nil then
  begin
   {Update RTL8192CU Wireless Driver}
   {Driver}
   RTL8192CUDriver.Driver.DriverName:=RTL8192CU_DRIVER_NAME; 
   {USB}
   RTL8192CUDriver.DriverBind:=RTL8192CUDriverBind;
   RTL8192CUDriver.DriverUnbind:=RTL8192CUDriverUnbind;

   {Register RTL8192CU Wireless Driver}
   Status:=USBDriverRegister(RTL8192CUDriver);
   if Status <> USB_STATUS_SUCCESS then
    begin
     if USB_LOG_ENABLED then USBLogError(nil,'RTL8192CU: Failed to register RTL8192CU driver: ' + USBStatusToString(Status));
    end;
  end
 else
  begin
   if USB_LOG_ENABLED then USBLogError(nil,'RTL8192CU: Failed to create RTL8192CU driver');
  end;
 
 RTL8192CUInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{RTL8192CU Network Functions}
function RTL8192CUDeviceOpen(Network:PNetworkDevice):LongWord;
{Implementation of NetworkDeviceOpen for the RTL8192CU device}
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

function RTL8192CUDeviceClose(Network:PNetworkDevice):LongWord;
{Implementation of NetworkDeviceClose for the RTL8192CU device}
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

function RTL8192CUDeviceRead(Network:PNetworkDevice;Buffer:Pointer;Size:LongWord;var Length:LongWord):LongWord;
{Implementation of NetworkDeviceRead for the RTL8192CU device}
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

function RTL8192CUDeviceWrite(Network:PNetworkDevice;Buffer:Pointer;Size:LongWord;var Length:LongWord):LongWord;
{Implementation of NetworkDeviceWrite for the RTL8192CU device}
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

function RTL8192CUDeviceControl(Network:PNetworkDevice;Request:Integer;Argument1:LongWord;var Argument2:LongWord):LongWord;
{Implementation of NetworkDeviceControl for the RTL8192CU device}
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
{RTL8192CU USB Functions}
function RTL8192CUDriverBind(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;
{Bind the RTL8192CU driver to a USB device if it is suitable}
{Device: The USB device to attempt to bind to}
{Interrface: The USB interface to attempt to bind to (or nil for whole device)}
{Return: USB_STATUS_SUCCESS if completed, USB_STATUS_DEVICE_UNSUPPORTED if unsupported or another error code on failure}
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;

 {$IFDEF RTL8192CU_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(Device,'RTL8192CU: Attempting to bind USB device (' + ': Address ' + IntToStr(Device.Address) + ')'); //To Do //Device.Manufacturer //Device.Product
 {$ENDIF}
 
 {Check Interface (Bind to device only)}
 if Interrface <> nil then
  begin
   {$IFDEF RTL8192CU_DEBUG}
    if USB_LOG_ENABLED then USBLogDebug(Device,'RTL8192CU: Interface bind not supported by driver');
   {$ENDIF}
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;
 
 {Check RTL8192CU Device}
 if RTL8192CUCheckDevice(Device) <> USB_STATUS_SUCCESS then
  begin
   {$IFDEF RTL8192CU_DEBUG}
    if USB_LOG_ENABLED then USBLogDebug(Device,'RTL8192CU: Device not found in supported device list');
   {$ENDIF}
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;

 {Check Device Speed}
 if Device.Speed <> USB_SPEED_HIGH then
  begin
   {$IFDEF RTL8192CU_DEBUG}
    if USB_LOG_ENABLED then USBLogDebug(Device,'RTL8192CU: Device speed is not USB_SPEED_HIGH');
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

function RTL8192CUDriverUnbind(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;
{Unbind the RTL8192CU driver from a USB device}
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
 if Device.Driver <> RTL8192CUDriver then Exit;
 
 {$IFDEF RTL8192CU_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(Device,'RTL8192CU: Unbinding (' + ': Address ' + IntToStr(Device.Address) + ')'); //To Do //Device.Manufacturer //Device.Product
 {$ENDIF}

 //To Do
end;
 
{==============================================================================}
{==============================================================================}
{RTL8192CU Helper Functions}
function RTL8192CUCheckDevice(Device:PUSBDevice):LongWord;
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
 for Count:=0 to RTL8192CU_DEVICE_ID_COUNT - 1 do
  begin
   if (RTL8192CU_DEVICE_ID[Count].idVendor = Device.Descriptor.idVendor) and (RTL8192CU_DEVICE_ID[Count].idProduct = Device.Descriptor.idProduct) then
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
 RTL8192CUInit;
 
{==============================================================================}
 
finalization
 {Nothing}
 
{==============================================================================}
{==============================================================================}

end.
 