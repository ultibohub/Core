{
Ultibo implementation of the Wiring SPI API.

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

  wiringPiSPI.c - Copyright (c) 2015 Gordon Henderson
 
References
==========

 WiringPi (Current) - http://wiringpi.com/
  
Wiring SPI
==========

 This unit reproduces the functionality of Gordon Henderson's WiringPiSPI library which is part
 of WiringPi. See the Wiring unit for an implementation of the core WiringPi library and for 
 more information.
 
 As with the core Wiring API this unit maintains function names and parameters as closely as 
 possible to the original library to allow easy porting of existing code and examples.
 
 To use the Wiring SPI unit you must also include the driver unit for the specific board that
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

unit WiringSPI;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,SPI,SysUtils,Wiring;

{==============================================================================}
{Global definitions}
{$INCLUDE ..\core\GlobalDefines.inc}

{==============================================================================}
{const}
 {Wiring SPI specific constants}

{==============================================================================}
{type}
 {Wiring SPI specific types}
 
{==============================================================================}
{var}
 {Wiring SPI specific variables}
 
{==============================================================================}
{Initialization Functions}

{==============================================================================}
{Wiring SPI Functions}
function wiringPiSPIDataRW(channel:LongInt;data:Pointer;len:LongInt):LongInt;

function wiringPiSPISetupMode(channel,speed,mode:LongInt):LongInt;
function wiringPiSPISetup(channel,speed:LongInt):LongInt;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
var
 {Wiring SPI specific variables}
 SPIDevice:PSPIDevice;
 
{==============================================================================}
{==============================================================================}
{Initialization Functions}

{==============================================================================}
{==============================================================================}
{Wiring SPI Functions}
function wiringPiSPIDataRW(channel:LongInt;data:Pointer;len:LongInt):LongInt;
{Write and Read a block of data over the SPI bus}
{Note the data ia being read into the transmit buffer, so will overwrite it!
 This is also a full-duplex operation}
var
 Count:LongWord;
begin
 {}
 Result:=-1;
 
 if SPIDeviceWriteRead(SPIDevice,channel,data,data,len,SPI_TRANSFER_NONE,Count) = ERROR_SUCCESS then
  begin
   Result:=0;
  end;
end;
 
{==============================================================================}

function wiringPiSPISetupMode(channel,speed,mode:LongInt):LongInt;
{Open the SPI device, and set it up, with the mode, etc}
var
 ClockPhase:LongWord;
 ClockPolarity:LongWord;
begin
 {}
 Result:=-1;
 
 {Update Mode}
 mode:=mode and 3;
 
 {Update Channel}
 channel:=channel and 1;
 
 {Find SPI}
 SPIDevice:=SPIDeviceFindByName('SPI0');
 if SPIDevice = nil then Exit;
 
 {Check Mode}
 if mode = 0 then
  begin
   {mode 0 (CPOL = 0 / CPHA = 0)}
   ClockPolarity:=SPI_CLOCK_POLARITY_LOW;
   ClockPhase:=SPI_CLOCK_PHASE_LOW;
  end
 else if mode = 1 then
  begin
   {mode 1 (CPOL = 0 / CPHA = 1)}
   ClockPolarity:=SPI_CLOCK_POLARITY_LOW;
   ClockPhase:=SPI_CLOCK_PHASE_HIGH;
  end
 else if mode = 2 then
  begin
   {mode 2 (CPOL = 1 / CPHA = 0)}
   ClockPolarity:=SPI_CLOCK_POLARITY_HIGH;
   ClockPhase:=SPI_CLOCK_PHASE_LOW;
  end
 else if mode = 3 then
  begin
   {mode 3 (CPOL = 1 / CPHA = 1)}
   ClockPolarity:=SPI_CLOCK_POLARITY_HIGH;
   ClockPhase:=SPI_CLOCK_PHASE_HIGH;
  end;  
 
 {Set SPI Clock Rate}
 if SPIDeviceSetClockRate(SPIDevice,channel,speed) <> ERROR_SUCCESS then Exit;
 
 {Start SPI}
 if SPIDeviceStart(SPIDevice,SPI_MODE_4WIRE,speed,ClockPhase,ClockPolarity) <> ERROR_SUCCESS then Exit;
 
 Result:=LongInt(SPIDevice);
end;

{==============================================================================}

function wiringPiSPISetup(channel,speed:LongInt):LongInt;
{Open the SPI device, and set it up, etc. in the default MODE 0}
begin
 {}
 Result:=wiringPiSPISetupMode(channel,speed,0)
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
 