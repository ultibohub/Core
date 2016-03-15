{
Ralink RT2x00 Wireless Driver library.

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

 This unit provides functionality and definitions common to all implementations of the RT2x00
 chipset series PCI, USB or other.
 
}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit RT2X00LIB; 

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,Network,WiFi,SysUtils;

{==============================================================================}
{Global definitions}
{$INCLUDE ..\core\GlobalDefines.inc}

{==============================================================================}
const
 {RT2X00LIB specific constants}

 {RT chip constants (The chipset on the device is composed of an RT and RF chip)}
 RT2X00_RT2460     = $2460;
 RT2X00_RT2560     = $2560;
 RT2X00_RT2570     = $2570;
 RT2X00_RT2661     = $2661;
 RT2X00_RT2573     = $2573;
 RT2X00_RT2860     = $2860;	{2.4GHz}
 RT2X00_RT2872     = $2872;	{WSOC}
 RT2X00_RT2883     = $2883;	{WSOC}
 RT2X00_RT3070     = $3070;
 RT2X00_RT3071     = $3071;
 RT2X00_RT3090     = $3090;	{2.4GHz PCIe}
 RT2X00_RT3290     = $3290;
 RT2X00_RT3352     = $3352; {WSOC}
 RT2X00_RT3390     = $3390;
 RT2X00_RT3572     = $3572;
 RT2X00_RT3593     = $3593;
 RT2X00_RT3883     = $3883;	{WSOC}
 RT2X00_RT5390     = $5390; {2.4GHz}
 RT2X00_RT5392     = $5392; {2.4GHz}
 RT2X00_RT5592     = $5592;
 
 {Requirements constants}
 RT2X00_REQUIRE_FIRMWARE      = 1 shl 0;
 RT2X00_REQUIRE_BEACON_GUARD  = 1 shl 1;
 RT2X00_REQUIRE_ATIM_QUEUE    = 1 shl 2;
 RT2X00_REQUIRE_DMA           = 1 shl 3;
 RT2X00_REQUIRE_COPY_IV       = 1 shl 4;
 RT2X00_REQUIRE_L2PAD         = 1 shl 5;
 RT2X00_REQUIRE_TXSTATUS_FIFO = 1 shl 6; //To Do //Remove ?
 RT2X00_REQUIRE_SW_SEQNO      = 1 shl 7;
 RT2X00_REQUIRE_HT_TX_DESC    = 1 shl 8;
 RT2X00_REQUIRE_PS_AUTOWAKE   = 1 shl 9;
 
 {Capabilities constants}
 RT2X00_CAPABILITY_HW_BUTTON             = 1 shl 0;
 RT2X00_CAPABILITY_HW_CRYPTO             = 1 shl 1;
 RT2X00_CAPABILITY_POWER_LIMIT           = 1 shl 2;
 RT2X00_CAPABILITY_CONTROL_FILTERS       = 1 shl 3;
 RT2X00_CAPABILITY_CONTROL_FILTER_PSPOLL = 1 shl 4;
 RT2X00_CAPABILITY_PRE_TBTT_INTERRUPT    = 1 shl 5;
 RT2X00_CAPABILITY_LINK_TUNING           = 1 shl 6;
 RT2X00_CAPABILITY_FRAME_TYPE            = 1 shl 7;
 RT2X00_CAPABILITY_RF_SEQUENCE           = 1 shl 8;
 RT2X00_CAPABILITY_EXTERNAL_LNA_A        = 1 shl 9;
 RT2X00_CAPABILITY_EXTERNAL_LNA_BG       = 1 shl 10;
 RT2X00_CAPABILITY_DOUBLE_ANTENNA        = 1 shl 11;
 RT2X00_CAPABILITY_BT_COEXIST            = 1 shl 12;
 RT2X00_CAPABILITY_VCO_RECALIBRATION     = 1 shl 13;
 
 {Busy delay constants}
 RT2X00_REGISTER_BUSY_COUNT = 100;
 RT2X00_REGISTER_USB_BUSY_COUNT = 20;
 RT2X00_REGISTER_BUSY_DELAY = 100;
 
{==============================================================================}
type
 {RT2X00LIB specific types}
 PRT2X00Chipset = ^TRT2X00Chipset;
 TRT2X00Chipset = record
  RTChip:Word;    {RT2X00_RT2460 etc}
  RFChip:Word;    {RT2800_RF2820 etc}
  Revision:Word;  {RT2800_REV_RT2860C etc}
 end;

 {RT2X00 Device}
 PRT2X00NetworkDevice = ^TRT2X00NetworkDevice;
 
 {RT2X00 Device Methods}
 TRT2X00DriverInit = function(RT2X00:PRT2X00NetworkDevice):LongWord;
 
 TRT2X00EepromLoad = function(RT2X00:PRT2X00NetworkDevice;Data:PWord;Size:LongWord):LongWord;
 
 TRT2X00RegisterRead = function(RT2X00:PRT2X00NetworkDevice;Offset:LongWord;Value:PLongWord):LongWord;
 TRT2X00RegisterWrite = function(RT2X00:PRT2X00NetworkDevice;Offset:LongWord;Value:LongWord):LongWord;
 
 TRT2X00RegisterMultiRead = function(RT2X00:PRT2X00NetworkDevice;Offset:LongWord;Data:Pointer;Size:LongWord):LongWord;
 TRT2X00RegisterMultiWrite = function(RT2X00:PRT2X00NetworkDevice;Offset:LongWord;Data:Pointer;Size:LongWord):LongWord;
 
 TRT2X00RegisterBusyRead = function(RT2X00:PRT2X00NetworkDevice;Offset,Mask:LongWord;Reg:PLongWord):Boolean; 
 
 TRT2X00GetFirmware = function(RT2X00:PRT2X00NetworkDevice;var Name:String;var Address:Pointer;var Size:LongWord):Boolean;
 TRT2X00CheckFirmware = function(RT2X00:PRT2X00NetworkDevice;Data:PByte;Size:LongWord):Boolean;
 TRT2X00LoadFirmware = function(RT2X00:PRT2X00NetworkDevice;Data:PByte;Size:LongWord):Boolean;
 TRT2X00WriteFirmware = function(RT2X00:PRT2X00NetworkDevice;Data:PByte;Size:LongWord):Boolean;

 //To Do
 
 TRT2X00NetworkDevice = record
  {Network Properties}
  Network:TNetworkDevice;
  {RT2X00 Methods}
  DriverInit:TRT2X00DriverInit;
  EepromLoad:TRT2X00EepromLoad;
  RegisterRead:TRT2X00RegisterRead;
  RegisterWrite:TRT2X00RegisterWrite;
  RegisterMultiRead:TRT2X00RegisterMultiRead;
  RegisterMultiWrite:TRT2X00RegisterMultiWrite;
  RegisterBusyRead:TRT2X00RegisterBusyRead;
  GetFirmware:TRT2X00GetFirmware;
  CheckFirmware:TRT2X00CheckFirmware;
  LoadFirmware:TRT2X00LoadFirmware;
  WriteFirmware:TRT2X00WriteFirmware;
  {RT2X00 Properties}
  Chipset:TRT2X00Chipset;
  Capabilities:LongWord;
  Requirements:LongWord;
  //To Do
  RFData:Pointer;
  RFSize:LongWord;
  EepromData:PWord;
  EepromSize:LongWord;
  CSRLock:TMutexHandle;      {Protect against concurrent indirect register access (BBP, RF, MCU)}
  //To Do
 end; 
  
 
{==============================================================================}
{var}
 {RT2X00LIB specific variables}
 
{==============================================================================}
{Initialization Functions}
 
{==============================================================================}
{RT2X00LIB Functions}
function RT2X00DriverInit(RT2X00:PRT2X00NetworkDevice):LongWord;
 
{==============================================================================}
{RT2X00LIB Helper Functions}
function RT2X00IsPCI(RT2X00:PRT2X00NetworkDevice):Boolean;
function RT2X00IsPCIe(RT2X00:PRT2X00NetworkDevice):Boolean;
function RT2X00IsUSB(RT2X00:PRT2X00NetworkDevice):Boolean;
function RT2X00IsMMIO(RT2X00:PRT2X00NetworkDevice):Boolean;

function RT2X00GetRTChip(RT2X00:PRT2X00NetworkDevice):Word;
procedure RT2X00SetRTChip(RT2X00:PRT2X00NetworkDevice;RTChip:Word);

function RT2X00GetRFChip(RT2X00:PRT2X00NetworkDevice):Word;
procedure RT2X00SetRFChip(RT2X00:PRT2X00NetworkDevice;RFChip:Word);

function RT2X00GetRevision(RT2X00:PRT2X00NetworkDevice):Word;
procedure RT2X00SetRevision(RT2X00:PRT2X00NetworkDevice;Revision:Word);

function RT2X00GetCapability(RT2X00:PRT2X00NetworkDevice;Capability:LongWord):Boolean;
procedure RT2X00SetCapability(RT2X00:PRT2X00NetworkDevice;Capability:LongWord);
procedure RT2X00ClearCapability(RT2X00:PRT2X00NetworkDevice;Capability:LongWord);

function RT2X00GetRequirement(RT2X00:PRT2X00NetworkDevice;Requirement:LongWord):Boolean;
procedure RT2X00SetRequirement(RT2X00:PRT2X00NetworkDevice;Requirement:LongWord);
procedure RT2X00ClearRequirement(RT2X00:PRT2X00NetworkDevice;Requirement:LongWord);

function RT2X00GetEeprom8(RT2X00:PRT2X00NetworkDevice;Offset:Byte):Byte;

function RT2X00GetEeprom16(RT2X00:PRT2X00NetworkDevice;Offset:Word):Word;
procedure RT2X00SetEeprom16(RT2X00:PRT2X00NetworkDevice;Offset,Value:Word);

function RT2X00GetEepromAddress(RT2X00:PRT2X00NetworkDevice;Offset:Word):Pointer;

function RT2X00GetRegister8(Reg,Mask,Shift:Byte):Byte; inline;
procedure RT2X00SetRegister8(var Reg:Byte;Mask,Shift,Value:Byte); inline;

function RT2X00GetRegister16(Reg,Mask,Shift:Word):Word; inline;
procedure RT2X00SetRegister16(var Reg:Word;Mask,Shift,Value:Word); inline;

function RT2X00GetRegister32(Reg,Mask,Shift:LongWord):LongWord; inline;
procedure RT2X00SetRegister32(var Reg:LongWord;Mask,Shift,Value:LongWord); inline;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
{Initialization Functions}
 
{==============================================================================}
{==============================================================================}
{RT2X00LIB Functions}
function RT2X00DriverInit(RT2X00:PRT2X00NetworkDevice):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Device}
 if RT2X00 = nil then Exit;
 
 {$IFDEF RT2800USB_DEBUG}
 if NETWORK_LOG_ENABLED then NetworkLogDebug(nil,'RT2X00: Driver init');
 {$ENDIF}
 
 {Detect Capabilities}
 if Assigned(RT2X00.DriverInit) then
  begin
   Result:=RT2X00.DriverInit(RT2X00);
   if Result <> ERROR_SUCCESS then Exit;
  end;
  
 //To Do //rt2x00lib_probe_dev
 
 Result:=ERROR_SUCCESS;
end;
 
{==============================================================================}
{==============================================================================}
{RT2X00LIB Helper Functions}
function RT2X00IsPCI(RT2X00:PRT2X00NetworkDevice):Boolean;
begin
 {}
 Result:=False;
 
 {Check Device}
 if RT2X00 = nil then Exit;
 
 Result:=(RT2X00.Network.Device.DeviceBus = DEVICE_BUS_PCI);
end;

{==============================================================================}

function RT2X00IsPCIe(RT2X00:PRT2X00NetworkDevice):Boolean;
begin
 {}
 Result:=False;
 
 {Check Device}
 if RT2X00 = nil then Exit;
 
 Result:=(RT2X00.Network.Device.DeviceBus = DEVICE_BUS_PCIE);
end;

{==============================================================================}

function RT2X00IsUSB(RT2X00:PRT2X00NetworkDevice):Boolean;
begin
 {}
 Result:=False;
 
 {Check Device}
 if RT2X00 = nil then Exit;
 
 Result:=(RT2X00.Network.Device.DeviceBus = DEVICE_BUS_USB);
end;

{==============================================================================}

function RT2X00IsMMIO(RT2X00:PRT2X00NetworkDevice):Boolean;
begin
 {}
 Result:=False;
 
 {Check Device}
 if RT2X00 = nil then Exit;
 
 Result:=(RT2X00.Network.Device.DeviceBus = DEVICE_BUS_MMIO);
end;
  
{==============================================================================}
  
function RT2X00GetRTChip(RT2X00:PRT2X00NetworkDevice):Word;
begin
 {}
 Result:=0;
 
 {Check Device}
 if RT2X00 = nil then Exit;

 Result:=RT2X00.Chipset.RTChip;
end;

{==============================================================================}

procedure RT2X00SetRTChip(RT2X00:PRT2X00NetworkDevice;RTChip:Word);
begin
 {}
 {Check Device}
 if RT2X00 = nil then Exit;

 RT2X00.Chipset.RTChip:=RTChip;
end;

{==============================================================================}

function RT2X00GetRFChip(RT2X00:PRT2X00NetworkDevice):Word;
begin
 {}
 Result:=0;
 
 {Check Device}
 if RT2X00 = nil then Exit;

 Result:=RT2X00.Chipset.RFChip;
end;

{==============================================================================}

procedure RT2X00SetRFChip(RT2X00:PRT2X00NetworkDevice;RFChip:Word);
begin
 {}
 {Check Device}
 if RT2X00 = nil then Exit;

 RT2X00.Chipset.RFChip:=RFChip;
end;

{==============================================================================}

function RT2X00GetRevision(RT2X00:PRT2X00NetworkDevice):Word;
begin
 {}
 Result:=0;
 
 {Check Device}
 if RT2X00 = nil then Exit;

 Result:=RT2X00.Chipset.Revision;
end;

{==============================================================================}

procedure RT2X00SetRevision(RT2X00:PRT2X00NetworkDevice;Revision:Word);
begin
 {}
 {Check Device}
 if RT2X00 = nil then Exit;

 RT2X00.Chipset.Revision:=Revision;
end;

{==============================================================================}

function RT2X00GetCapability(RT2X00:PRT2X00NetworkDevice;Capability:LongWord):Boolean;
begin
 {}
 Result:=False;
 
 {Check Device}
 if RT2X00 = nil then Exit;

 Result:=(RT2X00.Capabilities and Capability) <> 0;
end;

{==============================================================================}

procedure RT2X00SetCapability(RT2X00:PRT2X00NetworkDevice;Capability:LongWord);
begin
 {}
 {Check Device}
 if RT2X00 = nil then Exit;

 RT2X00.Capabilities:=(RT2X00.Capabilities or Capability);
end;

{==============================================================================}

procedure RT2X00ClearCapability(RT2X00:PRT2X00NetworkDevice;Capability:LongWord);
begin
 {}
 {Check Device}
 if RT2X00 = nil then Exit;

 RT2X00.Capabilities:=(RT2X00.Capabilities and not(Capability));
end;

{==============================================================================}

function RT2X00GetRequirement(RT2X00:PRT2X00NetworkDevice;Requirement:LongWord):Boolean;
begin
 {}
 Result:=False;
 
 {Check Device}
 if RT2X00 = nil then Exit;

 Result:=(RT2X00.Requirements and Requirement) <> 0;
end;

{==============================================================================}

procedure RT2X00SetRequirement(RT2X00:PRT2X00NetworkDevice;Requirement:LongWord);
begin
 {}
 {Check Device}
 if RT2X00 = nil then Exit;

 RT2X00.Capabilities:=(RT2X00.Requirements or Requirement);
end;

{==============================================================================}

procedure RT2X00ClearRequirement(RT2X00:PRT2X00NetworkDevice;Requirement:LongWord);
begin
 {}
 {Check Device}
 if RT2X00 = nil then Exit;

 RT2X00.Capabilities:=(RT2X00.Requirements and not(Requirement));
end;

{==============================================================================}

function RT2X00GetEeprom8(RT2X00:PRT2X00NetworkDevice;Offset:Byte):Byte;
begin
 {}
 Result:=0;
 
 {Check Device}
 if RT2X00 = nil then Exit;
 
 Result:=PByte(PtrUInt(RT2X00.EepromData) + Offset)^;
end;

{==============================================================================}

function RT2X00GetEeprom16(RT2X00:PRT2X00NetworkDevice;Offset:Word):Word;
begin
 {}
 Result:=0;
 
 {Check Device}
 if RT2X00 = nil then Exit;
 
 Result:=WordLEtoN(RT2X00.EepromData[Offset]);
end;

{==============================================================================}

procedure RT2X00SetEeprom16(RT2X00:PRT2X00NetworkDevice;Offset,Value:Word);
begin
 {}
 {Check Device}
 if RT2X00 = nil then Exit;

 RT2X00.EepromData[Offset]:=WordNtoLE(Value);
end;

{==============================================================================}

function RT2X00GetEepromAddress(RT2X00:PRT2X00NetworkDevice;Offset:Word):Pointer;
{Return a pointer to the EEPROM value at Offset}
begin
 {}
 Result:=nil;
 
 {Check Device}
 if RT2X00 = nil then Exit;

 Result:=@RT2X00.EepromData[Offset];
end;
  
{==============================================================================}
  
function RT2X00GetRegister8(Reg,Mask,Shift:Byte):Byte; inline;
{Shift is the number of bits to shift the result right (SHR)}
begin
 {}
 {Get}
 Result:=(Reg and Mask) shr Shift;
end;

{==============================================================================}

procedure RT2X00SetRegister8(var Reg:Byte;Mask,Shift,Value:Byte); inline;
{Shift is the number of bits to shift the value left (SHL)}
begin
 {}
 {Mask}
 Reg:=Reg and not(Mask);
 {Set}
 Reg:=Reg or ((Value shl Shift) and Mask);
end;

{==============================================================================}

function RT2X00GetRegister16(Reg,Mask,Shift:Word):Word; inline;
{Shift is the number of bits to shift the result right (SHR)}
begin
 {}
 {Get}
 Result:=(Reg and Mask) shr Shift;
end;

{==============================================================================}

procedure RT2X00SetRegister16(var Reg:Word;Mask,Shift,Value:Word); inline;
  {Shift is the number of bits to shift the value left (SHL)}
begin
 {}
 {Mask}
 Reg:=Reg and not(Mask);
 {Set}
 Reg:=Reg or ((Value shl Shift) and Mask);
end;

{==============================================================================}

function RT2X00GetRegister32(Reg,Mask,Shift:LongWord):LongWord; inline;
{Shift is the number of bits to shift the result right (SHR)}
begin
 {}
 {Get}
 Result:=(Reg and Mask) shr Shift;
end;

{==============================================================================}

procedure RT2X00SetRegister32(var Reg:LongWord;Mask,Shift,Value:LongWord); inline;
{Shift is the number of bits to shift the value left (SHL)}
begin
 {}
 {Mask}
 Reg:=Reg and not(Mask);
 {Set}
 Reg:=Reg or ((Value shl Shift) and Mask);
end;
  
{==============================================================================}
{==============================================================================}

end.
 