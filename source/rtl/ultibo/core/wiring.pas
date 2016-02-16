{
Ultibo implementation of the Wiring API.

Copyright (C) 2015 - SoftOz Pty Ltd.

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

 wiringPi.c - Copyright (c) 2012 Gordon Henderson
 
References
==========

 WiringPi - https://projects.drogon.net/raspberry-pi/wiringpi/
 
 Lazarus WiringPi - http://forum.lazarus.freepascal.org/index.php/topic,17404.0.html
 
Wiring API
==========

 This unit is intended to produce similar functionality to Gordon Henderson's WiringPi library
 for Linux.

 This unit is specific to Ultibo, if you want to use the WiringPi library from FPC/Lazarus
 on Linux then see the Lazarus WiringPi wrapper by Alex Schaller which is available from 
 http://forum.lazarus.freepascal.org/index.php/topic,17404.0.html
 
 Pin number is maintained as per the original WiringPi library so that example code will
 port without changes. The pin numbering is different to the GPIO device unit which uses 
 a linear numbering scheme to make it hardware independant.
  
 Function names and parameters are also maintained exactly as per the original library.
 
 To use the Wiring unit you must also include the driver unit for the specific board that
 uou are using by adding it to the uses clause in your program. 
 
  For Raspberry Pi A/B/A+/B+/Zero add BCM2708
  For Raspberry Pi 2B add BCM2709
 
}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit Wiring;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,GPIO,PWM;

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
const
 {Wiring specific constants}
 NUM_PINS      = 17; //To Do //Check Latest version of WiringPi (Website is down right now)   
 
 WPI_MODE_PINS = 0;    
 WPI_MODE_GPIO = 1;    
 
 INPUT         = 0;    
 OUTPUT        = 1;    
 PWM_OUTPUT    = 2;    
 
 LOW           = 0;    
 HIGH          = 1;    
 
 PUD_OFF       = 0;    
 PUD_DOWN      = 1;    
 PUD_UP        = 2;
              
{==============================================================================}
{type}
 {Wiring specific types}

{==============================================================================}
var
 {Wiring specific variables}
 WiringInitialized:Boolean;
 
{==============================================================================}
{Initialization Functions}
procedure WiringInit;

{==============================================================================}
{Wiring Functions}
function wiringPiSetup:LongInt;
procedure wiringPiGpioMode(mode:LongInt);
procedure pullUpDnControl(pin:LongInt;pud:LongInt);
procedure pinMode(pin:LongInt;mode:LongInt);
procedure digitalWrite(pin:LongInt;value:LongInt);
procedure pwmWrite(pin:LongInt;value:LongInt);
function digitalRead(pin:LongInt):LongInt;

procedure delay(howLong:DWORD);
procedure delayMicroseconds(howLong:DWORD);
function millis:DWORD;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
var
 {Wiring specific variables}
 WiringPinMode:LongWord;
 WiringEpoch:LongWord;
 
var
 {Pin to GPIO mappings} {These are currently Raspberry Pi specific}
 WiringPinToGPIO:array[0..(NUM_PINS - 1)] of LongWord = (
  17, 18, 21, 22, 23, 24, 25, 4,	// GPIO 0 through 7
  0,  1,				            // I2C  - SDA0, SCL0
  8,  7,				            // SPI  - CE1, CE0
  10,  9, 11, 				        // SPI  - MOSI, MISO, SCLK
  14, 15);				            // UART - Tx, Rx
 
{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure WiringInit;
begin
 {}
 {Check Initialized}
 if WiringInitialized then Exit;
 
 {Nothing}
 
 WiringInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{Wiring Functions}
function wiringPiSetup:LongInt;
begin
 {}
 //WiringEpoch:= //To Do
 WiringPinMode:=WPI_MODE_PINS;
 
 Result:=0;
end;

{==============================================================================}

procedure wiringPiGpioMode(mode:LongInt);
begin
 {}
 WiringPinMode:=mode;
end;

{==============================================================================}

procedure pullUpDnControl(pin:LongInt;pud:LongInt);
var
 GPIOPin:LongWord;
begin
 {}
 {Check mode}
 if WiringPinMode = WPI_MODE_PINS then
  begin
   {Check pin}
   if (pin < 0) or (pin >= NUM_PINS) then Exit;
   
   GPIOPin:=WiringPinToGPIO[pin];
  end
 else
  begin
   GPIOPin:=pin;
  end;
  
 //To Do
end;

{==============================================================================}

procedure pinMode(pin:LongInt;mode:LongInt);
var
 GPIOPin:LongWord;
begin
 {}
 {Check mode}
 if WiringPinMode = WPI_MODE_PINS then
  begin
   {Check pin}
   if (pin < 0) or (pin >= NUM_PINS) then Exit;
   
   GPIOPin:=WiringPinToGPIO[pin];
  end
 else
  begin
   GPIOPin:=pin;
  end;

  //To Do
end;

{==============================================================================}

procedure digitalWrite(pin:LongInt;value:LongInt);
var
 GPIOPin:LongWord;
begin
 {}
 {Check mode}
 if WiringPinMode = WPI_MODE_PINS then
  begin
   {Check pin}
   if (pin < 0) or (pin >= NUM_PINS) then Exit;
   
   GPIOPin:=WiringPinToGPIO[pin];
  end
 else
  begin
   GPIOPin:=pin;
  end;

  //To Do
end;

{==============================================================================}

procedure pwmWrite(pin:LongInt;value:LongInt);
var
 GPIOPin:LongWord;
begin
 {}
 {Check mode}
 if WiringPinMode = WPI_MODE_PINS then
  begin
   {Check pin}
   if (pin < 0) or (pin >= NUM_PINS) then Exit;
   
   GPIOPin:=WiringPinToGPIO[pin];
  end
 else
  begin
   GPIOPin:=pin;
  end;

  //To Do
end;

{==============================================================================}

function digitalRead(pin:LongInt):LongInt;
var
 GPIOPin:LongWord;
begin
 {}
 {Check mode}
 if WiringPinMode = WPI_MODE_PINS then
  begin
   {Check pin}
   if (pin < 0) or (pin >= NUM_PINS) then Exit;
   
   GPIOPin:=WiringPinToGPIO[pin];
  end
 else
  begin
   GPIOPin:=pin;
  end;

  //To Do
end;

{==============================================================================}

procedure delay(howLong:DWORD);
begin
 {}
 MillisecondDelay(howLong);
end;

{==============================================================================}

procedure delayMicroseconds(howLong:DWORD);
begin
 {}
 MicrosecondDelay(howLong);
end;

{==============================================================================}

function millis:DWORD;
begin
 {}
 //To Do
end;

{==============================================================================}
{==============================================================================}

initialization
 WiringInit;

{==============================================================================}
 
finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
