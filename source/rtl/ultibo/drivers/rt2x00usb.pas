{
Ralink RT2x00 USB Wireless Driver library.

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

  Linux - \drivers\net\wireless\rt2x00\* - Copyright (C) 2010 Willow Garage and others.

References
==========

 RT2x00 - http://ralink.rapla.net/ (Contains some patchy information about Ralink chipsets)

Ralink RT2x00
=============

 This unit provides functionality and definitions common to all USB implementations of the
 RT2x00 chipset series.

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit RT2X00USB;

interface

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
  RT2X00LIB,
  SysUtils;

{==============================================================================}
{Global definitions}
{$INCLUDE ..\core\GlobalDefines.inc}

{==============================================================================}
const
 {RT2X00USB specific constants}

 {USB timeouts}
 RT2X00USB_REGISTER_TIMEOUT          = 100;  {Register Read/Write timeout}
 RT2X00USB_REGISTER_TIMEOUT_FIRMWARE = 1000; {Timeout when loading Firmware}
 RT2X00USB_EEPROM_TIMEOUT            = 2000; {Timeout when reading EEPROM}

 {USB block sizes}
 RT2X00USB_REGISTER_BLOCK_SIZE       = 64;

 {USB request types}
 RT2X00USB_VENDOR_REQUEST     = (USB_BMREQUESTTYPE_TYPE_VENDOR or USB_BMREQUESTTYPE_RECIPIENT_DEVICE);
 RT2X00USB_VENDOR_REQUEST_IN  = (USB_BMREQUESTTYPE_DIR_IN or RT2X00USB_VENDOR_REQUEST);
 RT2X00USB_VENDOR_REQUEST_OUT = (USB_BMREQUESTTYPE_DIR_OUT or RT2X00USB_VENDOR_REQUEST);

 {USB vendor commands}
 RT2X00USB_DEVICE_MODE  = 1;
 RT2X00USB_SINGLE_WRITE = 2;
 RT2X00USB_SINGLE_READ  = 3;
 RT2X00USB_MULTI_WRITE  = 6;
 RT2X00USB_MULTI_READ   = 7;
 RT2X00USB_EEPROM_WRITE = 8;
 RT2X00USB_EEPROM_READ  = 9;
 RT2X00USB_LED_CONTROL  = 10; {RT73USB}
 RT2X00USB_RX_CONTROL   = 12;

 {USB Device modes offset}
 RT2X00USB_MODE_RESET    = 1;
 RT2X00USB_MODE_UNPLUG   = 2;
 RT2X00USB_MODE_FUNCTION = 3;
 RT2X00USB_MODE_TEST     = 4;
 RT2X00USB_MODE_SLEEP    = 7;  {RT73USB}
 RT2X00USB_MODE_FIRMWARE = 8;  {RT73USB}
 RT2X00USB_MODE_WAKEUP   = 9;  {RT73USB}
 RT2X00USB_MODE_AUTORUN  = 17; {RT2800USB}

{==============================================================================}
{type}
 {RT2X00USB specific types}

{==============================================================================}
{var}
 {RT2X00USB specific variables}

{==============================================================================}
{Initialization Functions}

{==============================================================================}
{RT2X00USB Functions}
function RT2X00USBDriverQuit(RT2X00:PRT2X00WiFiDevice):LongWord;

function RT2X00USBDisableRadio(RT2X00:PRT2X00WiFiDevice):LongWord;

function RT2X00USBEepromLoad(RT2X00:PRT2X00WiFiDevice;Data:PWord;Size:LongWord):LongWord;

function RT2X00USBRegisterRead(RT2X00:PRT2X00WiFiDevice;Offset:Word;Value:PLongWord):LongWord;
function RT2X00USBRegisterWrite(RT2X00:PRT2X00WiFiDevice;Offset:Word;Value:LongWord):LongWord;

function RT2X00USBRegisterMultiRead(RT2X00:PRT2X00WiFiDevice;Offset:Word;Data:Pointer;Size:LongWord):LongWord;
function RT2X00USBRegisterMultiWrite(RT2X00:PRT2X00WiFiDevice;Offset:Word;Data:Pointer;Size:LongWord):LongWord;

function RT2X00USBRegisterBusyRead(RT2X00:PRT2X00WiFiDevice;Offset,Mask:LongWord;Reg:PLongWord):Boolean;

function RT2X00USBVendorRequest(RT2X00:PRT2X00WiFiDevice;bRequest,bmRequestType:Byte;wValue,wIndex:Word;Data:Pointer;wLength:Word;Timeout:LongWord):LongWord;

{==============================================================================}
{RT2X00USB Helper Functions}

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
{Initialization Functions}

{==============================================================================}
{==============================================================================}
{RT2X00USB Functions}
function RT2X00USBDriverQuit(RT2X00:PRT2X00WiFiDevice):LongWord;
begin
 {}
 Result:=RT2X00DriverQuit(RT2X00);

 //To Do //rt2x00usb_disconnect
end;

{==============================================================================}

function RT2X00USBDisableRadio(RT2X00:PRT2X00WiFiDevice):LongWord;
begin
 {}
 Result:=RT2X00USBVendorRequest(RT2X00,RT2X00USB_RX_CONTROL,RT2X00USB_VENDOR_REQUEST_OUT,0,0,nil,0,RT2X00USB_REGISTER_TIMEOUT);
end;

{==============================================================================}

function RT2X00USBEepromLoad(RT2X00:PRT2X00WiFiDevice;Data:PWord;Size:LongWord):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Data}
 if Data = nil then Exit;

 Result:=RT2X00USBVendorRequest(RT2X00,RT2X00USB_EEPROM_READ,RT2X00USB_VENDOR_REQUEST_IN,0,0,Data,Size,RT2X00USB_EEPROM_TIMEOUT);
end;

{==============================================================================}

function RT2X00USBRegisterRead(RT2X00:PRT2X00WiFiDevice;Offset:Word;Value:PLongWord):LongWord;
var
 Reg:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Value}
 if Value = nil then Exit;

 Reg:=0;

 Result:=RT2X00USBVendorRequest(RT2X00,RT2X00USB_MULTI_READ,RT2X00USB_VENDOR_REQUEST_IN,0,Offset,@Reg,SizeOf(LongWord),RT2X00USB_REGISTER_TIMEOUT);

 {Return Value}
 Value^:=LongWordLEtoN(Reg);
end;

{==============================================================================}

function RT2X00USBRegisterWrite(RT2X00:PRT2X00WiFiDevice;Offset:Word;Value:LongWord):LongWord;
var
 Reg:LongWord;
begin
 {}
 {Copy Value}
 Reg:=LongWordNtoLE(Value);

 Result:=RT2X00USBVendorRequest(RT2X00,RT2X00USB_MULTI_WRITE,RT2X00USB_VENDOR_REQUEST_OUT,0,Offset,@Reg,SizeOf(LongWord),RT2X00USB_REGISTER_TIMEOUT);
end;

{==============================================================================}

function RT2X00USBRegisterMultiRead(RT2X00:PRT2X00WiFiDevice;Offset:Word;Data:Pointer;Size:LongWord):LongWord;
var
 Index:LongWord;
 Status:LongWord;
 Buffer:Pointer;
 Remain:LongWord;
 BlockSize:LongWord;
begin
 {}
 Status:=ERROR_SUCCESS;

 Index:=Offset;
 Buffer:=Data;
 Remain:=Size;

 while Remain > 0 do
  begin
   BlockSize:=Min(RT2X00USB_REGISTER_BLOCK_SIZE,Remain);

   Status:=RT2X00USBVendorRequest(RT2X00,RT2X00USB_MULTI_READ,RT2X00USB_VENDOR_REQUEST_IN,0,Index,Buffer,BlockSize,RT2X00USB_REGISTER_TIMEOUT);
   if Status <> USB_STATUS_SUCCESS then Break; //To Do //Change to ERROR_ returns  //Dont use USB_STATUS_ codes

   Inc(Index,BlockSize);
   Inc(Buffer,BlockSize);
   Dec(Remain,BlockSize);
  end;

 Result:=Status;
end;

{==============================================================================}

function RT2X00USBRegisterMultiWrite(RT2X00:PRT2X00WiFiDevice;Offset:Word;Data:Pointer;Size:LongWord):LongWord;
var
 Index:LongWord;
 Status:LongWord;
 Buffer:Pointer;
 Remain:LongWord;
 BlockSize:LongWord;
begin
 {}
 Status:=ERROR_SUCCESS;

 Index:=Offset;
 Buffer:=Data;
 Remain:=Size;

 while Remain > 0 do
  begin
   BlockSize:=Min(RT2X00USB_REGISTER_BLOCK_SIZE,Remain);

   Status:=RT2X00USBVendorRequest(RT2X00,RT2X00USB_MULTI_WRITE,RT2X00USB_VENDOR_REQUEST_OUT,0,Index,Buffer,BlockSize,RT2X00USB_REGISTER_TIMEOUT);
   if Status <> USB_STATUS_SUCCESS then Break; //To Do //Change to ERROR_ returns  //Dont use USB_STATUS_ codes

   Inc(Index,BlockSize);
   Inc(Buffer,BlockSize);
   Dec(Remain,BlockSize);
  end;

 Result:=Status;
end;

{==============================================================================}

function RT2X00USBRegisterBusyRead(RT2X00:PRT2X00WiFiDevice;Offset,Mask:LongWord;Reg:PLongWord):Boolean;
var
 Busy:LongWord;
begin
 {}
 Result:=False;

 {Check Device}
 if RT2X00 = nil then Exit;

 {Check Register}
 if Reg = nil then Exit;

 Busy:=0;
 while Busy < RT2X00_REGISTER_USB_BUSY_COUNT do
  begin
   RT2X00USBRegisterRead(RT2X00,Offset,Reg);
   if RT2X00GetRegister32(Reg^,Mask,0) = 0 then {Shift 0 because mask will determine result}
    begin
     Result:=True;
     Exit;
    end;

   MicrosecondDelay(RT2X00_REGISTER_BUSY_DELAY);

   Inc(Busy);
  end;

 if USB_LOG_ENABLED then USBLogError(PUSBDevice(RT2X00.WiFi.Network.Device.DeviceData),'RT2X00USB: Register busy read timeout');

 {Default return}
 Reg^:=LongWord(not(0));
end;

{==============================================================================}

function RT2X00USBVendorRequest(RT2X00:PRT2X00WiFiDevice;bRequest,bmRequestType:Byte;wValue,wIndex:Word;Data:Pointer;wLength:Word;Timeout:LongWord):LongWord;
var
 Status:LongWord;
 Device:PUSBDevice;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER; //TO Do //Change to ERROR_INVALID_PARAMETER; //Dont use USB_STATUS_ codes

 {Check Device}
 if RT2X00 = nil then Exit;

 {$IFDEF RT2800USB_DEBUG}
 //--if USB_LOG_ENABLED then USBLogDebug(PUSBDevice(RT2X00.WiFi.Network.Device.DeviceData),'RT2X00USB: Vendor request');
 {$ENDIF}

 {Get Device}
 Device:=PUSBDevice(RT2X00.WiFi.Network.Device.DeviceData);
 if Device = nil then Exit;

 {Send Vendor Request}
 Status:=USBControlRequestEx(Device,nil,bRequest,bmRequestType,wValue,wIndex,Data,wLength,Timeout,True);
 if Status <> USB_STATUS_SUCCESS then
  begin
   if USB_LOG_ENABLED then USBLogError(PUSBDevice(RT2X00.WiFi.Network.Device.DeviceData),'RT2X00USB: Vendor request failed (Status=' + USBStatusToString(Status) + ')');
  end;

 Result:=Status; //To Do //Change to ERROR_ returns  //Dont use USB_STATUS_ codes

 //To Do //Wrapper with timeout, keep retrying if return is not fatal ?
end;

{==============================================================================}
{==============================================================================}
{RT2X00USB Helper Functions}

{==============================================================================}
{==============================================================================}

end.

