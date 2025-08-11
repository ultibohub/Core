{
Ultibo implementation of the Wiring Serial API.

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

  wiringPiSerial.c - Copyright (c) 2012 Gordon Henderson

References
==========

 WiringPi (Current) - http://wiringpi.com/

Wiring Serial
=============

 This unit reproduces the functionality of Gordon Henderson's WiringSerial library which is part
 of WiringPi. See the Wiring unit for an implementation of the core WiringPi library and for
 more information.

 As with the core Wiring API this unit maintains function names and parameters as closely as
 possible to the original library to allow easy porting of existing code and examples.

 To use the Wiring Serial unit you must also include the driver unit for the specific board that
 you are using by adding it to the uses clause in your program.

  For Raspberry Pi A/B/A+/B+/Zero add BCM2708
  For Raspberry Pi 2B add BCM2709
  For Raspberry Pi 3B/3A+/3B+ add BCM2710
  For Raspberry Pi 4B/400 add BCM2711

 Note: Currently the serialPrintf function is not implemented due to the variable parameter list.

 Currently based on WiringPi release 2.32

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

{$IFNDEF FPC_DOTTEDUNITS}
unit WiringSerial;
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
  Core.Serial,
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
  Serial,
  SysUtils,
  Wiring;
{$ENDIF FPC_DOTTEDUNITS}

{==============================================================================}
{Global definitions}
{$INCLUDE ..\core\GlobalDefines.inc}

{==============================================================================}
{const}
 {Wiring Serial specific constants}

{==============================================================================}
{type}
 {Wiring Serial specific types}

{==============================================================================}
{var}
 {Wiring Serial specific variables}

{==============================================================================}
{Initialization Functions}

{==============================================================================}
{Wiring Serial Functions}
function serialOpen(const device:String;baud:LongInt):LongInt;
procedure serialFlush(fd:LongInt);
procedure serialClose(fd:LongInt);

procedure serialPutchar(fd:LongInt;c:Char);
procedure serialPuts(fd:LongInt;const s:String);

function serialDataAvail(fd:LongInt):LongInt;

function serialGetchar(fd:LongInt):LongInt;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{var}
 {Wiring Serial specific variables}

{==============================================================================}
{==============================================================================}
{Initialization Functions}

{==============================================================================}
{==============================================================================}
{Wiring Serial Functions}
function serialOpen(const device:String;baud:LongInt):LongInt;
{Open and initialise the serial port}
var
 SerialDevice:PSerialDevice;
begin
 {}
 Result:=-1;

 {Find Serial}
 SerialDevice:=SerialDeviceFindByName(device);
 if SerialDevice = nil then
  begin
   SerialDevice:=SerialDeviceFindByDescription(device);
   if SerialDevice = nil then Exit;
  end;

 {Open Serial}
 if SerialDeviceOpen(SerialDevice,baud,SERIAL_DATA_8BIT,SERIAL_STOP_1BIT,SERIAL_PARITY_NONE,SERIAL_FLOW_NONE,0,0) <> ERROR_SUCCESS then Exit;

 Result:=LongInt(SerialDevice);
end;

{==============================================================================}

procedure serialFlush(fd:LongInt);
{Flush the serial buffers (both tx & rx)}
begin
 {Not implemented}
end;

{==============================================================================}

procedure serialClose(fd:LongInt);
{Release the serial port}
begin
 {}
 SerialDeviceClose(PSerialDevice(fd));
end;

{==============================================================================}

procedure serialPutchar(fd:LongInt;c:Char);
{Send a single character to the serial port}
var
 Count:LongWord;
begin
 {}
 SerialDeviceWrite(PSerialDevice(fd),@c,SizeOf(Char),SERIAL_WRITE_NONE,Count);
end;

{==============================================================================}

procedure serialPuts(fd:LongInt;const s:String);
{Send a string to the serial port}
var
 Count:LongWord;
begin
 {}
 SerialDeviceWrite(PSerialDevice(fd),PChar(s),Length(s),SERIAL_WRITE_NONE,Count);
end;

{==============================================================================}

function serialDataAvail(fd:LongInt):LongInt;
{Return the number of bytes of data avalable to be read in the serial port}
var
 Data:Byte;
 Count:LongWord;
begin
 {}
 Result:=0;

 if SerialDeviceRead(PSerialDevice(fd),@Data,SizeOf(Byte),SERIAL_READ_PEEK_BUFFER,Count) = ERROR_SUCCESS then
  begin
   Result:=Count;
  end;
end;

{==============================================================================}

function serialGetchar(fd:LongInt):LongInt;
{Get a single character from the serial device}
{Note: Zero is a valid character and this function will time-out after 10 seconds}
var
 Data:Byte;
 Retry:Integer;
 Count:LongWord;
begin
 {}
 Retry:=10;

 while Retry > 0 do
  begin
   if SerialDeviceRead(PSerialDevice(fd),@Data,SizeOf(Byte),SERIAL_READ_NON_BLOCK,Count) = ERROR_SUCCESS then
    begin
     Result:=Data;
     Exit;
    end;

   Sleep(1000);
   Dec(Retry);
  end;
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

