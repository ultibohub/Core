{
Ultibo implementation of the Wiring I2C API.

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

  wiringPiI2C.c - Copyright (c) 2013 Gordon Henderson

References
==========

 WiringPi (Current) - http://wiringpi.com/

Wiring I2C
==========

 This unit reproduces the functionality of Gordon Henderson's WiringPiI2C library which is part
 of WiringPi. See the Wiring unit for an implementation of the core WiringPi library and for
 more information.

 As with the core Wiring API this unit maintains function names and parameters as closely as
 possible to the original library to allow easy porting of existing code and examples.

 To use the Wiring I2C unit you must also include the driver unit for the specific board that
 you are using by adding it to the uses clause in your program.

  For Raspberry Pi A/B/A+/B+/Zero add BCM2708
  For Raspberry Pi 2B add BCM2709
  For Raspberry Pi 3B/3A+/3B+ add BCM2710
  For Raspberry Pi 4B/400 add BCM2711

 Currently based on WiringPi release 2.32

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

{$IFNDEF FPC_DOTTEDUNITS}
unit WiringI2C;
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
  Core.I2C,
  System.SysUtils,
  Core.Wiring;
{$ELSE FPC_DOTTEDUNITS}
uses
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  Devices,
  I2C,
  SysUtils,
  Wiring;
{$ENDIF FPC_DOTTEDUNITS}

{==============================================================================}
{Global definitions}
{$INCLUDE ..\core\GlobalDefines.inc}

{==============================================================================}
{const}
 {Wiring I2C specific constants}

{==============================================================================}
{type}
 {Wiring I2C specific types}

{==============================================================================}
{var}
 {Wiring I2C specific variables}

{==============================================================================}
{Initialization Functions}

{==============================================================================}
{Wiring I2C Functions}
function wiringPiI2CRead(fd:LongInt):LongInt;
function wiringPiI2CReadReg8(fd,reg:LongInt):LongInt;
function wiringPiI2CReadReg16(fd,reg:LongInt):LongInt;

function wiringPiI2CWrite(fd,data:LongInt):LongInt;
function wiringPiI2CWriteReg8(fd,reg,value:LongInt):LongInt;
function wiringPiI2CWriteReg16(fd,reg,value:LongInt):LongInt;

function wiringPiI2CSetupInterface(const device:String;devId:LongInt):LongInt;
function wiringPiI2CSetup(devId:LongInt):LongInt;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{var}
 {Wiring I2C specific variables}

{==============================================================================}
{==============================================================================}
{Initialization Functions}

{==============================================================================}
{==============================================================================}
{Wiring I2C Functions}
function wiringPiI2CRead(fd:LongInt):LongInt;
{Simple device read}
var
 Data:Byte;
 Count:LongWord;
begin
 {}
 Result:=-1;

 if I2CDeviceRead(PI2CDevice(fd),I2C_ADDRESS_INVALID,@Data,SizeOf(Byte),Count) = ERROR_SUCCESS then
  begin
   Result:=Data;
  end;
end;

{==============================================================================}

function wiringPiI2CReadReg8(fd,reg:LongInt):LongInt;
{Read an 8-bit value from a regsiter on the device}
var
 Data:Byte;
 Initial:Byte;
 Count:LongWord;
begin
 {}
 Result:=-1;

 Initial:=reg;
 if I2CDeviceWriteRead(PI2CDevice(fd),I2C_ADDRESS_INVALID,@Initial,SizeOf(Byte),@Data,SizeOf(Byte),Count) = ERROR_SUCCESS then
  begin
   Result:=Data;
  end;
end;

{==============================================================================}

function wiringPiI2CReadReg16(fd,reg:LongInt):LongInt;
{Read a 16-bit value from a regsiter on the device}
var
 Data:Word;
 Initial:Byte;
 Count:LongWord;
begin
 {}
 Result:=-1;

 Initial:=reg;
 if I2CDeviceWriteRead(PI2CDevice(fd),I2C_ADDRESS_INVALID,@Initial,SizeOf(Byte),@Data,SizeOf(Word),Count) = ERROR_SUCCESS then
  begin
   Result:=Data;
  end;
end;

{==============================================================================}

function wiringPiI2CWrite(fd,data:LongInt):LongInt;
{Simple device write}
var
 Buffer:Byte;
 Count:LongWord;
begin
 {}
 Result:=-1;

 Buffer:=data;
 if I2CDeviceWrite(PI2CDevice(fd),I2C_ADDRESS_INVALID,@Buffer,SizeOf(Byte),Count) = ERROR_SUCCESS then
  begin
   Result:=0;
  end;
end;

{==============================================================================}

function wiringPiI2CWriteReg8(fd,reg,value:LongInt):LongInt;
{Write an 8-bit value to the given register}
var
 Data:Byte;
 Initial:Byte;
 Count:LongWord;
begin
 {}
 Result:=-1;

 Data:=value;
 Initial:=reg;
 if I2CDeviceWriteWrite(PI2CDevice(fd),I2C_ADDRESS_INVALID,@Initial,SizeOf(Byte),@Data,SizeOf(Byte),Count) = ERROR_SUCCESS then
  begin
   Result:=0;
  end;
end;

{==============================================================================}

function wiringPiI2CWriteReg16(fd,reg,value:LongInt):LongInt;
{Write a 16-bit value to the given register}
var
 Data:Word;
 Initial:Byte;
 Count:LongWord;
begin
 {}
 Result:=-1;

 Data:=value;
 Initial:=reg;
 if I2CDeviceWriteWrite(PI2CDevice(fd),I2C_ADDRESS_INVALID,@Initial,SizeOf(Byte),@Data,SizeOf(Word),Count) = ERROR_SUCCESS then
  begin
   Result:=0;
  end;
end;

{==============================================================================}

function wiringPiI2CSetupInterface(const device:String;devId:LongInt):LongInt;
{Undocumented access to set the interface explicitly (might be used for the Pi's 2nd I2C interface)}
var
 I2CDevice:PI2CDevice;
begin
 {}
 Result:=-1;

 {Find I2C}
 I2CDevice:=I2CDeviceFindByName(device);
 if I2CDevice = nil then
  begin
   I2CDevice:=I2CDeviceFindByDescription(device);
   if I2CDevice = nil then Exit;
  end;

 {Start I2C (Default Rate)}
 if I2CDeviceStart(I2CDevice,0) <> ERROR_SUCCESS then Exit;

 {Set Address}
 if I2CDeviceSetAddress(I2CDevice,devId) <> ERROR_SUCCESS then
  begin
   {Stop I2C}
   I2CDeviceStop(I2CDevice);
   Exit;
  end;

 Result:=LongInt(I2CDevice);
end;

{==============================================================================}

function wiringPiI2CSetup(devId:LongInt):LongInt;
{Open the I2C device, and regsiter the target (slave) device}
var
 rev:LongInt;
 device:String;
begin
 {}
 Result:=-1;

 rev:=piBoardRev;
 if rev = 1 then
  begin
   device:='I2C0';
  end
 else if rev = 2 then
  begin
   device:='I2C0';
  end
 else
  begin
   Exit;
  end;

 Result:=wiringPiI2CSetupInterface(device,devId);
end;

{==============================================================================}
{==============================================================================}

initialization
 {Nothing}

{==============================================================================}

finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.

