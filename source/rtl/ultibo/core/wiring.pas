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
  wiringShift.c - Copyright (c) 2012 Gordon Henderson
  piHiPri.c - Copyright (c) 2012 Gordon Henderson
  piThread.c - Copyright (c) 2012 Gordon Henderson
 
References
==========

 WiringPi (Current) - http://wiringpi.com/
  
 Version 1 (Old) -  https://projects.drogon.net/raspberry-pi/wiringpi/  
 
 Lazarus WiringPi (Version 1) - http://forum.lazarus.freepascal.org/index.php/topic,17404.0.html
 
Wiring
======

 This unit is intended to produce similar functionality to Gordon Henderson's WiringPi library
 for Linux to allow easy porting of existing code or examples to Ultibo. The WiringPi library
 itself is based loosely on the Wiring API for Arduino and other microcontrollers.
 
 Note that Ultibo has an extensive API already that provides most of the same functionality
 as WiringPi by using the GPIO unit and some of the other device units such as I2C, SPI and PWM.
 It is not neccessary to use this unit for GPIO access, it is simply provided as an alternative
 for those who have existing code or are familiar with the WiringPi API.
 
 This unit is specific to Ultibo, if you want to use the WiringPi library from FPC/Lazarus
 on Linux then see the Lazarus WiringPi wrapper by Alex Schaller which is available from 
 http://forum.lazarus.freepascal.org/index.php/topic,17404.0.html
 
 Pin numbering is maintained as per the original WiringPi library so that example code will
 port without changes. The pin numbering is different to the GPIO device unit which uses 
 a linear numbering scheme to make it hardware independent.
  
 Function names and parameters are also maintained almost exactly as per the original library.
 
 To use the Wiring unit you must also include the driver unit for the specific board that
 you are using by adding it to the uses clause in your program. 
 
  For Raspberry Pi A/B/A+/B+/Zero add BCM2708
  For Raspberry Pi 2B add BCM2709
  For Raspberry Pi 3B add BCM2710
 
 Note that this unit implements the WiringPi version 2 API and not the old version 1 API.
 Since version 2 was released in 2013 most code should now be updated to use the new functions.
 
 Currently based on WiringPi release 2.32
 
}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit Wiring;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,GPIO,PWM,SysUtils;

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
const
 {Wiring specific constants}
 PI_GPIO_MASK = $FFFFFFC0; {Mask for the bottom 64 pins which belong to the Raspberry Pi}
 
 {wiringPi modes} 
 WPI_MODE_PINS          = 0;    
 WPI_MODE_GPIO          = 1;    
 WPI_MODE_GPIO_SYS      = 2;
 WPI_MODE_PHYS          = 3;
 WPI_MODE_PIFACE        = 4;  {Not implemented}
 WPI_MODE_UNINITIALISED = -1;
 
 {Pin modes}
 INPUT            = 0;    
 OUTPUT           = 1;    
 PWM_OUTPUT       = 2;    
 GPIO_CLOCK       = 3;
 SOFT_PWM_OUTPUT  = 4;  {Not implemented}
 SOFT_TONE_OUTPUT = 5;  {Not implemented}
 PWM_TONE_OUTPUT  = 6;
 
 {Pin levels}
 LOW           = 0;    
 HIGH          = 1;    
 
 {Pull up/down/none}
 PUD_OFF       = 0;    
 PUD_DOWN      = 1;    
 PUD_UP        = 2;
    
 {PWM modes}
 PWM_MODE_MS   = 0;
 PWM_MODE_BAL  = 1;
 
 {Interrupt levels}
 INT_EDGE_SETUP   = 0; {Not implemented}
 INT_EDGE_FALLING = 1;
 INT_EDGE_RISING  = 2;
 INT_EDGE_BOTH    = 3;
 
 {Shift contants}
 LSBFIRST = 0;
 MSBFIRST = 1;
 
 {Pi model types and version numbers}
 PI_MODEL_A    = 0;
 PI_MODEL_B    = 1;
 PI_MODEL_AP   = 2;
 PI_MODEL_BP   = 3;
 PI_MODEL_2    = 4;
 PI_ALPHA      = 5;
 PI_MODEL_CM   = 6;
 PI_MODEL_07   = 7;
 PI_MODEL_3    = 8;
 PI_MODEL_ZERO = 9;

 PI_VERSION_1   = 0;
 PI_VERSION_1_1 = 1;
 PI_VERSION_1_2 = 2;
 PI_VERSION_2   = 3;

 PI_MAKER_SONY    = 0;
 PI_MAKER_EGOMAN  = 1;
 PI_MAKER_MBEST   = 2;
 PI_MAKER_UNKNOWN = 3;
 
 piModelNames:array[0..15] of String = (
  'Model A',    //  0
  'Model B',    //  1
  'Model A',    //  2
  'Model B',    //  3
  'Pi 2',       //  4
  'Alpha',      //  5
  'CM',         //  6
  'Unknown07',  // 07
  'Pi 3',       // 08
  'Pi Zero',    // 09
  'Unknown10',  // 10
  'Unknown11',  // 11
  'Unknown12',  // 12
  'Unknown13',  // 13
  'Unknown14',  // 14
  'Unknown15'); // 15
 
 piRevisionNames:array[0..15] of String = (
  '00',
  '01',
  '02',
  '03',
  '04',
  '05',
  '06',
  '07',
  '08',
  '09',
  '10',
  '11',
  '12',
  '13',
  '14',
  '15');
 
 piMakerNames:array[0..15] of String = (
  'Sony',       //  0
  'Egoman',     //  1
  'Embest',     //  2
  'Unknown',    //  3
  'Embest',     //  4
  'Unknown05',  //  5
  'Unknown06',  //  6
  'Unknown07',  //  7
  'Unknown08',  //  8
  'Unknown09',  //  9
  'Unknown10',  // 10
  'Unknown11',  // 11
  'Unknown12',  // 12
  'Unknown13',  // 13
  'Unknown14',  // 14
  'Unknown15'); // 15
 
 piMemorySize:array[0..7] of LongInt = (
   256,  //  0
   512,  //  1
  1024,  //  2
     0,  //  3
     0,  //  4
     0,  //  5
     0,  //  6
     0); //  7
 
{==============================================================================}
const
 {Raspbery Pi (BCM2835) specific constants}
 {Note that these values are identical for Raspberry Pi 2 (BCM2836) and 3 (BCM2837)}
 
 {BCM2835 mailbox tag Get Board Revision values (See: http://elinux.org/RPi_HardwareHistory)}
 BCM2835_BOARD_REV_B_I2C0_2	= $00000002;
 BCM2835_BOARD_REV_B_I2C0_3	= $00000003;
 BCM2835_BOARD_REV_B_I2C1_4	= $00000004;
 BCM2835_BOARD_REV_B_I2C1_5	= $00000005;
 BCM2835_BOARD_REV_B_I2C1_6	= $00000006;
 BCM2835_BOARD_REV_A_7		= $00000007;
 BCM2835_BOARD_REV_A_8		= $00000008;
 BCM2835_BOARD_REV_A_9		= $00000009;
 BCM2835_BOARD_REV_B_REV2_d	= $0000000D;
 BCM2835_BOARD_REV_B_REV2_e	= $0000000E;
 BCM2835_BOARD_REV_B_REV2_f	= $0000000F;
 BCM2835_BOARD_REV_B_PLUS	= $00000010;
 BCM2835_BOARD_REV_CM		= $00000011;
 BCM2835_BOARD_REV_A_PLUS	= $00000012;
 BCM2835_BOARD_REV_B_PLUS_2 = $00000013;
 BCM2835_BOARD_REV_CM_2		= $00000014;
 BCM2835_BOARD_REV_A_PLUS_2	= $00000015;

 BCM2835_BOARD_REV_MASK     = $00FFFFFF; {Mask off the warranty bit}
 
 {BCM2835 mailbox tag Get Board Revision bit fields (See: https://github.com/AndrewFromMelbourne/raspberry_pi_revision)}
 BCM2835_BOARD_REVISION_PCB_MASK             = ($F shl 0);  {PCB Revision Number}
 BCM2835_BOARD_REVISION_MODEL_MASK           = ($FF shl 4); {Model Number}
 BCM2835_BOARD_REVISION_MANUFACTURER_MASK    = ($F shl 16); {Manufacturer}
 BCM2835_BOARD_REVISION_MEMORY_MASK          = ($7 shl 20); {Memory Size}
 
 BCM2835_BOARD_REVISION_ENCODED_FLAG         = (1 shl 23);  {Endcoded Flag, if set then revision uses this encoding}
 
 {Clock Management} 
 BCM2835_CM_PASSWORD               = $5A000000;
 
 BCM2835_CM_CTL_BUSY           = (1 shl 7); {Clock generator is running (To avoid glitches and lock-ups, clock sources and setups must not be changed while this flag is set)}
 BCM2835_CM_CTL_ENAB           = (1 shl 4); {Enable the clock generator}
 
 BCM2835_CM_CTL_SRC_OSC        = (1 shl 0); {Clock source - Oscillator}
 
 {Clock Manager Registers}
 BCM2835_CM_PWMCTL   =  $000000A0; {PWM Clock Control}
 BCM2835_CM_PWMDIV   =  $000000A4; {PWM Clock Divisor}
 
 {Pulse Width Modulator (PWM) Control register bits (See 9.6)}
 BCM2835_PWM_CTL_MSEN2 = (1 shl 15); {Channel 2 M/S Enable (0: PWM algorithm is used / 1: M/S transmission is used)}
 BCM2835_PWM_CTL_PWEN2 = (1 shl 8);  {Channel 2 Enable (0: Channel is disabled / 1: Channel is enabled)}
 BCM2835_PWM_CTL_MSEN1 = (1 shl 7);  {Channel 1 M/S Enable (0: PWM algorithm is used / 1: M/S transmission is used)}
 BCM2835_PWM_CTL_PWEN1 = (1 shl 0);  {Channel 1 Enable (0: Channel is disabled / 1: Channel is enabled)}
 
 {Pulse Width Modulator (PWM) Registers}
 BCM2835_PWM_CTL  = $00000000; {PWM Control}
 BCM2835_PWM_RNG1 = $00000010; {PWM Channel 1 Range}
 BCM2835_PWM_DAT1 = $00000014; {PWM Channel 1 Data}
 BCM2835_PWM_RNG2 = $00000020; {PWM Channel 2 Range}
 BCM2835_PWM_DAT2 = $00000024; {PWM Channel 2 Data}
  
 {Pin Output Set Registers}
 BCM2835_GPSET0 = $0000001C; {GPIO Pin Output Set 0}
 BCM2835_GPSET1 = $00000020; {GPIO Pin Output Set 1}
 
 {Pin Output Clear Registers}
 BCM2835_GPCLR0 = $00000028; {GPIO Pin Output Clear 0}
 BCM2835_GPCLR1 = $0000002C; {GPIO Pin Output Clear 1}
 
 {Pin Level Registers}
 BCM2835_GPLEV0 = $00000034; {GPIO Pin Level 0}
 BCM2835_GPLEV1 = $00000038; {GPIO Pin Level 1}
  
{==============================================================================}
type
 {Wiring specific types}
 {wiringPiNodeStruct}
 PwiringPiNodeStruct = ^TwiringPiNodeStruct; 
 
 TpinMode = procedure(node:PwiringPiNodeStruct;pin,mode:LongInt); 
 TpullUpDnControl = procedure(node:PwiringPiNodeStruct;pin,mode:LongInt); 
 TdigitalRead = function(node:PwiringPiNodeStruct;pin:LongInt):LongInt;
 TdigitalWrite = procedure(node:PwiringPiNodeStruct;pin,value:LongInt);
 TpwmWrite = procedure(node:PwiringPiNodeStruct;pin,value:LongInt);
 TanalogRead = function(node:PwiringPiNodeStruct;pin:LongInt):LongInt;
 TanalogWrite = procedure(node:PwiringPiNodeStruct;pin,value:LongInt);
 
 TwiringPiNodeStruct = record {This describes additional device nodes}
  pinBase:LongInt;
  pinMax:LongInt;
 
  fd:LongInt;     {Node specific}
  data0:LongWord; {Node specific}
  data1:LongWord; {Node specific}
  data2:LongWord; {Node specific}
  data3:LongWord; {Node specific}
 
  pinMode:TpinMode;
  pullUpDnControl:TpullUpDnControl;
  digitalRead:TdigitalRead;
  digitalWrite:TdigitalWrite;
  pwmWrite:TpwmWrite;
  analogRead:TanalogRead;
  analogWrite:TanalogWrite;

  next:PwiringPiNodeStruct;
 end;
 
 TwiringPiISRFunction = procedure;
 TpiThreadCreateFunction = procedure;
 
 {pinToGpio}
 {Take a Wiring pin (0 through X) and re-map it to the BCM_GPIO pin}
 PpinToGpio = ^TpinToGpio;
 TpinToGpio = array[0..63] of LongInt;

 {physToGpio}
 {Take a physical pin (1 through 26) and re-map it to the BCM_GPIO pin}
 PphysToGpio = ^TphysToGpio;
 TphysToGpio = array[0..63] of LongInt;
 
{==============================================================================}
{var}
 {Wiring specific variables}
 
{==============================================================================}
{Initialization Functions}

{==============================================================================}
{Wiring Setup Functions}
{See: http://wiringpi.com/reference/setup/}
function wiringPiSetup:LongInt;
function wiringPiSetupGpio:LongInt;
function wiringPiSetupPhys:LongInt;
function wiringPiSetupSys:LongInt; 

{==============================================================================}
{Wiring Core Functions}
{See: http://wiringpi.com/reference/core-functions/}
procedure pinModeAlt(pin,mode:LongInt);
procedure pinMode(pin,mode:LongInt);

procedure pullUpDnControl(pin,pud:LongInt);

function digitalRead(pin:LongInt):LongInt;
procedure digitalWrite(pin,value:LongInt);

procedure pwmWrite(pin,value:LongInt);

function analogRead(pin:LongInt):LongInt;
procedure analogWrite(pin,value:LongInt);

{==============================================================================}
{Wiring Timing Functions}
{See: http://wiringpi.com/reference/timing/}
procedure delay(howLong:LongWord);
procedure delayMicroseconds(howLong:LongWord);

function millis:LongWord;
function micros:LongWord; 

{==============================================================================}
{Wiring Node Functions}
{See: }
function wiringPiFindNode(pin:LongInt):PwiringPiNodeStruct;
function wiringPiNewNode(pinBase,numPins:LongInt):PwiringPiNodeStruct;

{==============================================================================}
{Wiring Shift Functions}
{See: http://wiringpi.com/reference/shift-library/}
function shiftIn(dPin,cPin,order:Byte):Byte;
procedure shiftOut(dPin,cPin,order,val:Byte);

{==============================================================================}
{Wiring Priority, Interrupts and Threads Functions}
{See: http://wiringpi.com/reference/priority-interrupts-and-threads/}
function wiringPiISR(pin,mode:LongInt;func:TwiringPiISRFunction):LongInt;

function piHiPri(priority:LongInt):LongInt;

function piThreadCreate(fn:TpiThreadCreateFunction):LongInt;

procedure piLock(key:LongInt);
procedure piUnlock(key:LongInt);

{==============================================================================}
{Wiring Raspberry Pi Specific Functions}
{See: http://wiringpi.com/reference/raspberry-pi-specifics/}
function piBoardRev:LongInt;
procedure piBoardId(var model,rev,mem,maker,warranty:LongInt);

function wpiPinToGpio(wpiPin:LongInt):LongInt;
function physPinToGpio(physPin:LongInt):LongInt;

procedure setPadDrive(group,value:LongInt);
function getAlt(pin:LongInt):LongInt;

procedure pwmSetMode(mode:LongInt);
procedure pwmSetRange(range:LongWord);
procedure pwmSetClock(divisor:LongInt);

procedure pwmToneWrite(pin,freq:LongInt);

procedure gpioClockSet(pin,freq:LongInt);

function digitalReadByte:LongInt;
procedure digitalWriteByte(value:LongInt);

function digitalReadByte2:LongInt;
procedure digitalWriteByte2(value:LongInt);

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
var
 {Wiring specific variables}
 WiringInitialized:Boolean;
 GPIODevice:PGPIODevice;
 
 wiringPiMode:LongWord = WPI_MODE_UNINITIALISED;
 epochMilli:Int64;
 epochMicro:Int64;
 alreadyCalled:Boolean;
 boardRev:LongInt = -1;
 
 pinToGpio:PpinToGpio;
 physToGpio:PphysToGpio;
 
 RASPBERRY_PI_PERI_BASE:PtrUInt;
 GPIO_PADS:PtrUInt;
 CLOCK_BASE:PtrUInt;
 GPIO_BASE:PtrUInt;
 TIMER_BASE:PtrUInt;
 PWM_BASE:PtrUInt;

 wiringPiNodes:PwiringPiNodeStruct;
 
var
 {Pin to GPIO mappings} {These are Raspberry Pi specific}
 {Revision 1, 1.1}
 pinToGpioR1:TpinToGpio = (
  17, 18, 21, 22, 23, 24, 25, 4, // From the Original Wiki - GPIO 0 through 7: wpi  0 -  7
   0,  1,                        // I2C  - SDA1, SCL1    wpi  8 -  9
   8,  7,                        // SPI  - CE1, CE0    wpi 10 - 11
  10,  9, 11,                    // SPI  - MOSI, MISO, SCLK   wpi 12 - 14
  14, 15,                        // UART - Tx, Rx    wpi 15 - 16

  // Padding:

      -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,  // ... 31
  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,  // ... 47
  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1); // ... 63

 {Revision 2}
 pinToGpioR2:TpinToGpio = (
  17, 18, 27, 22, 23, 24, 25, 4, // From the Original Wiki - GPIO 0 through 7: wpi  0 -  7
   2,  3,                        // I2C  - SDA0, SCL0    wpi  8 -  9
   8,  7,                        // SPI  - CE1, CE0    wpi 10 - 11
  10,  9, 11,                    // SPI  - MOSI, MISO, SCLK   wpi 12 - 14
  14, 15,                        // UART - Tx, Rx    wpi 15 - 16
  28, 29, 30, 31,                // Rev 2: New GPIOs 8 though 11   wpi 17 - 20
   5,  6, 13, 19, 26,            // B+      wpi 21, 22, 23, 24, 25
  12, 16, 20, 21,                // B+      wpi 26, 27, 28, 29
   0,  1,                        // B+      wpi 30, 31

  // Padding:

  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,  // ... 47
  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1); // ... 63
  
 {Physical to GPIO mappings} {These are Raspberry Pi specific}
 physToGpioR1:TphysToGpio = (
  -1,   // 0
  -1, -1, // 1, 2
   0, -1,
   1, -1,
   4, 14,
  -1, 15,
  17, 18,
  21, -1,
  22, 23,
  -1, 24,
  10, -1,
   9, 25,
  11,  8,
  -1,  7, // 25, 26

                                              -1, -1, -1, -1, -1,  // ... 31
  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,  // ... 47
  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1); // ... 63
 
 physToGpioR2:TphysToGpio = (
  -1,   // 0
  -1, -1, // 1, 2
   2, -1,
   3, -1,
   4, 14,
  -1, 15,
  17, 18,
  27, -1,
  22, 23,
  -1, 24,
  10, -1,
   9, 25,
  11,  8,
  -1,  7, // 25, 26

  // B+

   0,  1,
   5, -1,
   6, 12,
  13, -1,
  19, 16,
  26, 20,
  -1, 21,

  // the P5 connector on the Rev 2 boards:

  -1, -1,
  -1, -1,
  -1, -1,
  -1, -1,
  -1, -1,
  28, 29,
  30, 31,
  -1, -1,
  -1, -1,
  -1, -1,
  -1, -1,
  -1);
 
var
 {gpioToPwmALT (The ALT value to put a GPIO pin into PWM mode)}
 gpioToPwmALT:array[0..63] of LongWord = (
                   0,                  0,                  0,                  0,                  0,                  0,         0,         0,  //  0 ->  7
                   0,                  0,                  0,                  0, GPIO_FUNCTION_ALT0, GPIO_FUNCTION_ALT0,         0,         0,  //  8 -> 15
                   0,                  0, GPIO_FUNCTION_ALT5, GPIO_FUNCTION_ALT5,                  0,                  0,         0,         0,  // 16 -> 23
                   0,                  0,                  0,                  0,                  0,                  0,         0,         0,  // 24 -> 31
                   0,                  0,                  0,                  0,                  0,                  0,         0,         0,  // 32 -> 39
  GPIO_FUNCTION_ALT0, GPIO_FUNCTION_ALT0,                  0,                  0,                  0, GPIO_FUNCTION_ALT0,         0,         0,  // 40 -> 47
                   0,                  0,                  0,                  0,                  0,                  0,         0,         0,  // 48 -> 55
                   0,                  0,                  0,                  0,                  0,                  0,         0,         0); // 56 -> 63
 
 {gpioToPwmPort (The port value to put a GPIO pin into PWM mode)}
 gpioToPwmPort:array[0..63] of Byte = (
                 0,                0,                0,                0,                0,                0,         0,         0,  //  0 ->  7
                 0,                0,                0,                0, BCM2835_PWM_DAT1, BCM2835_PWM_DAT2,         0,         0,  //  8 -> 15
                 0,                0, BCM2835_PWM_DAT1, BCM2835_PWM_DAT2,                0,                0,         0,         0,  // 16 -> 23
                 0,                0,                0,                0,                0,                0,         0,         0,  // 24 -> 31
                 0,                0,                0,                0,                0,                0,         0,         0,  // 32 -> 39
  BCM2835_PWM_DAT1, BCM2835_PWM_DAT2,                0,                0,                0, BCM2835_PWM_DAT2,         0,         0,  // 40 -> 47
                 0,                0,                0,                0,                0,                0,         0,         0,  // 48 -> 55
                 0,                0,                0,                0,                0,                0,         0,         0); // 56 -> 63
 
var
 {gpioToGpClkALT (ALT value to put a GPIO pin into GP Clock mode)}
 gpioToGpClkALT0:array[0..63] of LongWord = (
                   0,         0,                  0,                  0, GPIO_FUNCTION_ALT0, GPIO_FUNCTION_ALT0, GPIO_FUNCTION_ALT0,         0,  //  0 ->  7
                   0,         0,                  0,                  0,                  0,                  0,                  0,         0,  //  8 -> 15
                   0,         0,                  0,                  0, GPIO_FUNCTION_ALT5, GPIO_FUNCTION_ALT5,                  0,         0,  // 16 -> 23
                   0,         0,                  0,                  0,                  0,                  0,                  0,         0,  // 24 -> 31
  GPIO_FUNCTION_ALT0,         0, GPIO_FUNCTION_ALT0,                  0,                  0,                  0,                  0,         0,  // 32 -> 39
                   0,         0, GPIO_FUNCTION_ALT0, GPIO_FUNCTION_ALT0, GPIO_FUNCTION_ALT0,                  0,                  0,         0,  // 40 -> 47
                   0,         0,                  0,                  0,                  0,                  0,                  0,         0,  // 48 -> 55
                   0,         0,                  0,                  0,                  0,                  0,                  0,         0); // 56 -> 63
 
 {gpioToClkCon (Offsets to the clock Control registers)}
 gpioToClkCon:array[0..63] of Byte = (
 
         -1,        -1,        -1,        -1,       $70,       $78,       $80,        -1,  //  0 ->  7
         -1,        -1,        -1,        -1,        -1,        -1,        -1,        -1,  //  8 -> 15
         -1,        -1,        -1,        -1,       $70,       $78,        -1,        -1,  // 16 -> 23
         -1,        -1,        -1,        -1,        -1,        -1,        -1,        -1,  // 24 -> 31
        $70,        -1,       $70,        -1,        -1,        -1,        -1,        -1,  // 32 -> 39
         -1,        -1,       $70,       $78,       $70,        -1,        -1,        -1,  // 40 -> 47 //To Do //Documentation says this should be GPCLK1/2/1 (This is 0/1/0 ?)
         -1,        -1,        -1,        -1,        -1,        -1,        -1,        -1,  // 48 -> 55
         -1,        -1,        -1,        -1,        -1,        -1,        -1,        -1); // 56 -> 63
 
 {gpioToClkDiv (Offsets to the clock Divisor registers)}
 gpioToClkDiv:array[0..63] of Byte = (
         -1,        -1,        -1,        -1,       $74,       $7C,       $84,        -1,  //  0 ->  7
         -1,        -1,        -1,        -1,        -1,        -1,        -1,        -1,  //  8 -> 15
         -1,        -1,        -1,        -1,       $74,       $7C,        -1,        -1,  // 16 -> 23
         -1,        -1,        -1,        -1,        -1,        -1,        -1,        -1,  // 24 -> 31
        $74,        -1,       $74,        -1,        -1,        -1,        -1,        -1,  // 32 -> 39
         -1,        -1,       $74,       $7C,       $74,        -1,        -1,        -1,  // 40 -> 47 //To Do //Documentation says this should be GPCLK1/2/1 (This is 0/1/0 ?)
         -1,        -1,        -1,        -1,        -1,        -1,        -1,        -1,  // 48 -> 55
         -1,        -1,        -1,        -1,        -1,        -1,        -1,        -1); // 56 -> 63
 
var
 piMutexes:array[0..3] of TMutexHandle;
 
{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure WiringInit;
{Initialize the wiring unit}

{Note: Called only during system startup}
var
 Count:LongWord;
begin
 {}
 {Check Initialized}
 if WiringInitialized then Exit;
 
 {Initialize Mutexes}
 for Count:=0 to 3 do
  begin
   piMutexes[Count]:=INVALID_HANDLE_VALUE;
  end;
 
 WiringInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{Wiring Setup Functions}
function wiringPiSetup:LongInt;
{Must be called once at the start of your program execution}
{Default setup: Initialises the system into wiringPi Pin mode}
var
 Total:Int64;
 boardRev:LongInt;
begin
 {}
 if alreadyCalled then
  begin
   Result:=0;
   Exit;
  end;

 alreadyCalled:=True;
  
 {Get Revision}
 boardRev:=piBoardRev;
 if boardRev = 1 then
  begin
   {A, B, Rev 1, 1.1}
   pinToGpio:=@pinToGpioR1;
   physToGpio:=@physToGpioR1;
  end
 else if boardRev = 2 then
  begin
   {A, B, Rev 2, B+, CM, Pi2, Zero}
   pinToGpio:=@pinToGpioR2;
   physToGpio:=@physToGpioR2;
  end
 else
  begin
   {Unknown}
   Result:=-1;
   Exit;
  end;  
  
 {Get Peripheral Base}
 RASPBERRY_PI_PERI_BASE:=PeripheralGetBase;
 
 {Setup the peripheral offsets}
 GPIO_PADS:=RASPBERRY_PI_PERI_BASE + $00100000;  {Power Management, Reset controller and Watchdog}
 CLOCK_BASE:=RASPBERRY_PI_PERI_BASE + $00101000; {Clock Management (See Section 6)}
 GPIO_BASE:=RASPBERRY_PI_PERI_BASE + $00200000;  {GPIO (See Section 6)}
 TIMER_BASE:=RASPBERRY_PI_PERI_BASE + $0000B000; {ARM Timer (See Section 14)}
 PWM_BASE:=RASPBERRY_PI_PERI_BASE + $0020C000;   {PWM (Pulse Width Modulator)(See Section 9)}
 
 {Get GPIO Device} 
 GPIODevice:=GPIODeviceGetDefault;
 
 {Calculate Epoch}
 Total:=ClockGetTotal;
 epochMilli:=Total div CLOCK_CYCLES_PER_MILLISECOND;
 epochMicro:=Total div CLOCK_CYCLES_PER_MICROSECOND;
 
 {Check for Compute}
 if BoardGetType = BOARD_TYPE_RPI_COMPUTE then
  begin
   wiringPiMode:=WPI_MODE_GPIO;
  end
 else
  begin 
   wiringPiMode:=WPI_MODE_PINS;
  end; 
 
 Result:=0;
end;

{==============================================================================}

function wiringPiSetupGpio:LongInt;
{Must be called once at the start of your program execution}
{GPIO setup: Initialises the system into GPIO Pin mode}
begin
 {}
 Result:=wiringPiSetup;
 
 wiringPiMode:=WPI_MODE_GPIO;
end; 

{==============================================================================}

function wiringPiSetupPhys:LongInt;
{Must be called once at the start of your program execution}
{Phys setup: Initialises the system into Physical Pin mode}
begin
 {}
 Result:=wiringPiSetup;
 
 wiringPiMode:=WPI_MODE_PHYS;
end; 

{==============================================================================}

function wiringPiSetupSys:LongInt; 
{Must be called once at the start of your program execution}
{GPIO setup: Initialises the system into GPIO Pin mode}

{Note: Under Ultibo this is the same as wiringPiSetupGpio}
begin
 {}
 Result:=wiringPiSetup;
 
 wiringPiMode:=WPI_MODE_GPIO_SYS;
end; 
 
{==============================================================================}
{==============================================================================}
{Wiring Core Functions}
procedure pinModeAlt(pin,mode:LongInt);
{This is an un-documented special to let you set any pin to any mode}
var
 GPIOPin:LongWord;
begin
 {}
 {Check Pin}
 if (pin and PI_GPIO_MASK) = 0 then
  begin
   {On-board pin}
   if wiringPiMode = WPI_MODE_PINS then
    begin
     GPIOPin:=pinToGpio[pin];
    end
   else if wiringPiMode = WPI_MODE_PHYS then
    begin
     GPIOPin:=physToGpio[pin];
    end
   else if (wiringPiMode = WPI_MODE_GPIO) or (wiringPiMode = WPI_MODE_GPIO_SYS) then
    begin
     GPIOPin:=pin;
    end
   else 
    begin
     Exit;
    end;
   
   GPIODeviceFunctionSelect(GPIODevice,GPIOPin,mode);
  end;
end; 

{==============================================================================}

procedure pinMode(pin,mode:LongInt);
{Sets the mode of a pin to be input, output or PWM output}
var
 GPIOPin:LongWord;
 GPIOFunction:LongWord;
 Node:PwiringPiNodeStruct;
begin
 {}
 {Check Pin}
 if (pin and PI_GPIO_MASK) = 0 then
  begin
   {On-board pin}
   if wiringPiMode = WPI_MODE_PINS then
    begin
     GPIOPin:=pinToGpio[pin];
    end
   else if wiringPiMode = WPI_MODE_PHYS then
    begin
     GPIOPin:=physToGpio[pin];
    end
   else if (wiringPiMode = WPI_MODE_GPIO) or (wiringPiMode = WPI_MODE_GPIO_SYS) then
    begin
     GPIOPin:=pin;
    end
   else 
    begin
     Exit;
    end;

   {Check Mode}
   if mode = INPUT then
    begin
     GPIODeviceFunctionSelect(GPIODevice,GPIOPin,GPIO_FUNCTION_IN);
    end
   else if mode = OUTPUT then
    begin
     GPIODeviceFunctionSelect(GPIODevice,GPIOPin,GPIO_FUNCTION_OUT);
    end
   else if mode = SOFT_PWM_OUTPUT then
    begin
     {Not implemented}
    end
   else if mode = SOFT_TONE_OUTPUT then
    begin
     {Not implemented}
    end
   else if mode = PWM_TONE_OUTPUT then
    begin
     if RASPBERRY_PI_PERI_BASE = 0 then Exit;
     
     {Enable PWM output}
     pinMode(pin,PWM_OUTPUT);
     
     {Set PWM mode}
     pwmSetMode(PWM_MODE_MS);
    end
   else if mode = PWM_OUTPUT then 
    begin
     if RASPBERRY_PI_PERI_BASE = 0 then Exit;
     
     {Set pin to PWM mode}
     GPIOFunction:=gpioToPwmALT[GPIOPin];
     if GPIOFunction = 0 then Exit;
     GPIODeviceFunctionSelect(GPIODevice,GPIOPin,GPIOFunction);
     delayMicroseconds(110); {See comments in pwmSetClock}
     
     {Set PWM mode to default}
     pwmSetMode(PWM_MODE_BAL);
     
     {Set PWM range to default}
     pwmSetRange(1024);
     
     {Set PWM clock to 600KHz (19.2MHz / 32)}
     pwmSetClock(32);
    end
   else if mode = GPIO_CLOCK then
    begin
     if RASPBERRY_PI_PERI_BASE = 0 then Exit;
     
     {Set pin to GPIO_CLOCK mode}
     GPIOFunction:=gpioToGpClkALT0[GPIOPin];
     if GPIOFunction = 0 then Exit;
     GPIODeviceFunctionSelect(GPIODevice,GPIOPin,GPIOFunction);
     delayMicroseconds(110);
     
     {set the clock frequency to 100KHz}
     gpioClockSet(pin,100000);
    end;
  end
 else
  begin
   Node:=wiringPiFindNode(pin);
   if Node <> nil then
    begin
     Node.pinMode(Node,pin,mode);
    end;
  end;
end;

{==============================================================================}

procedure pullUpDnControl(pin,pud:LongInt);
{Control the internal pull-up/down resistors on a GPIO pin}
var
 GPIOPin:LongWord;
 Node:PwiringPiNodeStruct;
begin
 {}
 {Check Pin}
 if (pin and PI_GPIO_MASK) = 0 then
  begin
   {On-board pin}
   if wiringPiMode = WPI_MODE_PINS then
    begin
     GPIOPin:=pinToGpio[pin];
    end
   else if wiringPiMode = WPI_MODE_PHYS then
    begin
     GPIOPin:=physToGpio[pin];
    end
   else if (wiringPiMode = WPI_MODE_GPIO) or (wiringPiMode = WPI_MODE_GPIO_SYS) then
    begin
     GPIOPin:=pin;
    end
   else 
    begin
     Exit;
    end;

   if pud = PUD_OFF then
    begin
     GPIODevicePullSelect(GPIODevice,GPIOPin,GPIO_PULL_NONE);
    end
   else if pud = PUD_DOWN then
    begin
     GPIODevicePullSelect(GPIODevice,GPIOPin,GPIO_PULL_DOWN);
    end
   else if pud = PUD_UP then
    begin 
     GPIODevicePullSelect(GPIODevice,GPIOPin,GPIO_PULL_UP);    
    end;
  end
 else
  begin
   Node:=wiringPiFindNode(pin);
   if Node <> nil then
    begin
     Node.pullUpDnControl(Node,pin,pud);
    end;
  end;
end;

{==============================================================================}

function digitalRead(pin:LongInt):LongInt;
{Read the value of a given Pin, returning HIGH or LOW}
var
 GPIOPin:LongWord;
 Node:PwiringPiNodeStruct;
begin
 {}
 {Check Pin}
 if (pin and PI_GPIO_MASK) = 0 then
  begin
   {On-board pin}
   if wiringPiMode = WPI_MODE_PINS then
    begin
     GPIOPin:=pinToGpio[pin];
    end
   else if wiringPiMode = WPI_MODE_PHYS then
    begin
     GPIOPin:=physToGpio[pin];
    end
   else if (wiringPiMode = WPI_MODE_GPIO) or (wiringPiMode = WPI_MODE_GPIO_SYS) then
    begin
     GPIOPin:=pin;
    end
   else 
    begin
     Result:=LOW;
     Exit;
    end;
 
   if GPIODeviceInputGet(GPIODevice,GPIOPin) = GPIO_LEVEL_HIGH then
    begin
     Result:=HIGH;
    end
   else 
    begin
     Result:=LOW;
    end;    
  end
 else
  begin
   Node:=wiringPiFindNode(pin);
   if Node = nil then
    begin
     Result:=LOW;
    end
   else
    begin
     Result:=Node.digitalRead(Node,pin);
    end;    
  end;
end;

{==============================================================================}

procedure digitalWrite(pin,value:LongInt);
{Set an output bit}
var
 GPIOPin:LongWord;
 Node:PwiringPiNodeStruct;
begin
 {}
 {Check Pin}
 if (pin and PI_GPIO_MASK) = 0 then
  begin
   {On-board pin}
   if wiringPiMode = WPI_MODE_PINS then
    begin
     GPIOPin:=pinToGpio[pin];
    end
   else if wiringPiMode = WPI_MODE_PHYS then
    begin
     GPIOPin:=physToGpio[pin];
    end
   else if (wiringPiMode = WPI_MODE_GPIO) or (wiringPiMode = WPI_MODE_GPIO_SYS) then
    begin
     GPIOPin:=pin;
    end
   else 
    begin
     Exit;
    end;
    
   if value = LOW then
    begin
     GPIODeviceOutputSet(GPIODevice,GPIOPin,GPIO_LEVEL_LOW);
    end
   else
    begin   
     GPIODeviceOutputSet(GPIODevice,GPIOPin,GPIO_LEVEL_HIGH);
    end; 
  end
 else
  begin
   Node:=wiringPiFindNode(pin);
   if Node <> nil then
    begin
     Node.digitalWrite(Node,pin,value);
    end;    
  end;
end;

{==============================================================================}

procedure pwmWrite(pin,value:LongInt);
{Set an output PWM value}
var
 GPIOPin:LongWord;
 Node:PwiringPiNodeStruct;
begin
 {}
 {Check Pin}
 if (pin and PI_GPIO_MASK) = 0 then
  begin
   {On-board pin}
   if wiringPiMode = WPI_MODE_PINS then
    begin
     GPIOPin:=pinToGpio[pin];
    end
   else if wiringPiMode = WPI_MODE_PHYS then
    begin
     GPIOPin:=physToGpio[pin];
    end
   else if (wiringPiMode = WPI_MODE_GPIO) or (wiringPiMode = WPI_MODE_GPIO_SYS) then
    begin
     GPIOPin:=pin;
    end
   else 
    begin
     Exit;
    end;

   {Memory Barrier}
   DataMemoryBarrier; {Before the First Write}
    
   {Set Value}
   PLongWord(PWM_BASE + gpioToPwmPort[GPIOPin])^:=value;
  end
 else
  begin
   Node:=wiringPiFindNode(pin);
   if Node <> nil then
    begin
     Node.pwmWrite(Node,pin,value);
    end;    
  end;
end;

{==============================================================================}

function analogRead(pin:LongInt):LongInt;
{Read the analog value of a given Pin}
{There is no on-board Pi analog hardware, so this needs to go to a node}
var
 Node:PwiringPiNodeStruct;
begin
 {}
 Node:=wiringPiFindNode(pin);
 if Node = nil then
  begin
   Result:=0;
  end
 else 
  begin
   Result:=Node.analogRead(Node,pin);
  end;    
end; 

{==============================================================================}

procedure analogWrite(pin,value:LongInt);
{Write the analog value to the given Pin}
{There is no on-board Pi analog hardware, so this needs to go to a node}
var
 Node:PwiringPiNodeStruct;
begin
 {}
 Node:=wiringPiFindNode(pin);
 if Node <> nil then
  begin
   Node.analogWrite(Node,pin,value);
  end;    
end; 

{==============================================================================}
{==============================================================================}
{Wiring Timing Functions}
procedure delay(howLong:LongWord);
{Wait for some number of milliseconds}
begin
 {}
 ThreadSleep(howLong);
end;

{==============================================================================}

procedure delayMicroseconds(howLong:LongWord);
{Wait for some number of microseconds}
begin
 {}
 MicrosecondDelay(howLong);
end;

{==============================================================================}

function millis:LongWord;
{Return a number of milliseconds as an unsigned int}
var
 Total:Int64;
begin
 {}
 Total:=(ClockGetTotal div CLOCK_CYCLES_PER_MILLISECOND);
 Result:=Total - epochMilli;
end;

{==============================================================================}

function micros:LongWord; 
{Return a number of microseconds as an unsigned int}
var
 Total:Int64;
begin
 {}
 Total:=(ClockGetTotal div CLOCK_CYCLES_PER_MICROSECOND);
 Result:=Total - epochMicro;
end;

{==============================================================================}
{==============================================================================}
{Wiring Node Functions}
{See: }
procedure pinModeDummy(node:PwiringPiNodeStruct;pin,mode:LongInt);
begin
 {Nothing}
end;

{==============================================================================}

procedure pullUpDnControlDummy(node:PwiringPiNodeStruct;pin,pud:LongInt);
begin
 {Nothing}
end;

{==============================================================================}

function digitalReadDummy(node:PwiringPiNodeStruct;pin:LongInt):LongInt;
begin
 {Nothing}
 Result:=LOW;
end;

{==============================================================================}

procedure digitalWriteDummy(node:PwiringPiNodeStruct;pin,value:LongInt);
begin
 {Nothing}
end;

{==============================================================================}

procedure pwmWriteDummy(node:PwiringPiNodeStruct;pin,value:LongInt);
begin
 {Nothing}
end;

{==============================================================================}

function analogReadDummy(node:PwiringPiNodeStruct;pin:LongInt):LongInt;
begin
 {Nothing}
 Result:=0;
end;

{==============================================================================}

procedure analogWriteDummy(node:PwiringPiNodeStruct;pin,value:LongInt);
begin
 {Nothing}
end;

{==============================================================================}

function wiringPiFindNode(pin:LongInt):PwiringPiNodeStruct;
{Locate our device node}
var
 node:PwiringPiNodeStruct;
begin
 {}
 node:=wiringPiNodes;
 while node <> nil do
  begin
   if (pin >= node.pinBase) and (pin <= node.pinMax) then
    begin
     Result:=node;
     Exit;
    end;
   
   node:=node.next;
  end;
  
 Result:=nil; 
end;

{==============================================================================}

function wiringPiNewNode(pinBase,numPins:LongInt):PwiringPiNodeStruct;
var
 pin:LongInt;
 node:PwiringPiNodeStruct;
begin
 {}
 {Minimum pin base is 64}
 if (pinBase < 64) or (numPins < 1) then
  begin
   Result:=nil;
   Exit;
  end;
  
 {Check all pins in-case there is overlap}
 for pin:=pinBase to (pinBase + (numPins - 1)) do
  begin
   if wiringPiFindNode(pin) <> nil then
    begin
     Result:=nil;
     Exit;
    end;
  end;
  
 {Allocate node}
 node:=PwiringPiNodeStruct(AllocMem(SizeOf(TwiringPiNodeStruct)));
 if node = nil then
  begin
   Result:=nil;
   Exit;
  end;
  
 {Setup node} 
 node.pinBase:=pinBase;
 node.pinMax:=(pinBase + (numPins - 1));
 node.pinMode:=pinModeDummy;
 node.pullUpDnControl:=pullUpDnControlDummy;
 node.digitalRead:=digitalReadDummy;
 node.digitalWrite:=digitalWriteDummy;
 node.pwmWrite:=pwmWriteDummy;
 node.analogRead:=analogReadDummy;
 node.analogWrite:=analogWriteDummy;
 
 {Add to list (Not thread safe!)}
 node.next:=wiringPiNodes;
 wiringPiNodes:=node;

 Result:=node;
end;

{==============================================================================}
{==============================================================================}
{Wiring Shift Functions}
{See: http://wiringpi.com/reference/shift-library/}
function shiftIn(dPin,cPin,order:Byte):Byte;
{Shift data in from a clocked source}
var
 Value:Byte;
 Count:Byte;
begin
 {}
 Value:=0;
 
 if order = MSBFIRST then
  begin
   for Count:=7 downto 0 do
    begin
     digitalWrite(cPin,HIGH);
     Value:=Value or (digitalRead(dPin) shl Count);
     digitalWrite(cPin,LOW);
    end; 
  end
 else
  begin
   for Count:=0 to 7 do
    begin
     digitalWrite(cPin,HIGH);
     Value:=Value or (digitalRead(dPin) shl Count);
     digitalWrite(cPin,LOW);
    end;
  end;  
  
 Result:=Value; 
end;

{==============================================================================}

procedure shiftOut(dPin,cPin,order,val:Byte);
{Shift data out to a clocked source}
var
 Count:Byte;
begin
 {}
 if order = MSBFIRST then
  begin
   for Count:=7 downto 0 do
    begin
     digitalWrite(dPin,val and (1 shl Count));
     digitalWrite(cPin,HIGH);
     digitalWrite(cPin,LOW);
    end; 
  end
 else
  begin
   for Count:=0 to 7 do
    begin
     digitalWrite(dPin,val and (1 shl Count));
     digitalWrite(cPin,HIGH);
     digitalWrite(cPin,LOW);
    end;
  end;  
end;

{==============================================================================}
{==============================================================================}
{Wiring Priority, Interrupts and Threads Functions}
procedure wiringPiISRCallback(func:TwiringPiISRFunction;pin,mode:LongInt);
{Callback used internally to handle the GPIO event}
begin
 {}
 if Assigned(func) then
  begin
   func;
  end;
end;

{==============================================================================}

function wiringPiISR(pin,mode:LongInt;func:TwiringPiISRFunction):LongInt;
{Take the details and create an interrupt handler that will do a call-back to the user supplied function}
var
 GPIOPin:LongWord;
 GPIOTrigger:LongWord;
begin
 {}
 {Check Pin}
 if (pin and PI_GPIO_MASK) = 0 then
  begin
   {On-board pin}
   if wiringPiMode = WPI_MODE_PINS then
    begin
     GPIOPin:=pinToGpio[pin];
    end
   else if wiringPiMode = WPI_MODE_PHYS then
    begin
     GPIOPin:=physToGpio[pin];
    end
   else if (wiringPiMode = WPI_MODE_GPIO) or (wiringPiMode = WPI_MODE_GPIO_SYS) then
    begin
     GPIOPin:=pin;
    end
   else 
    begin
     Result:=-1;
     Exit;
    end;
 
   {Check Mode}
   if mode <> INT_EDGE_SETUP then
    begin
     if mode = INT_EDGE_FALLING then
      begin
       {Falling}
       GPIOTrigger:=GPIO_TRIGGER_FALLING;
      end
     else if mode = INT_EDGE_RISING then
      begin
       {Rising}
       GPIOTrigger:=GPIO_TRIGGER_RISING;
      end
     else if mode = INT_EDGE_BOTH then
      begin
       {Both}
       GPIOTrigger:=GPIO_TRIGGER_EDGE;
      end
     else
      begin
       {Unknown}
       Result:=-1;
       Exit;
      end;
      
     GPIODeviceInputEvent(GPIODevice,GPIOPin,GPIOTrigger,GPIO_EVENT_FLAG_NONE,INFINITE,TGPIOCallback(wiringPiISRCallback),@func);
     
     Result:=0;
    end
   else
    begin
     Result:=-1;
    end;
  end
 else
  begin
   Result:=-1;
  end;  
end; 

{==============================================================================}

function piHiPri(priority:LongInt):LongInt;
{Attempt to set a high priority schedulling for the running program}
var
 Level:LongWord;
begin
 {}
 Result:=-1;
 
 if priority < 0 then Exit;
 if priority > 100 then Exit;
 
 {Turn the priority into a thread priority}
 case priority of
  0..15:Level:=THREAD_PRIORITY_IDLE;
  16..30:Level:=THREAD_PRIORITY_LOWEST;
  31..45:Level:=THREAD_PRIORITY_LOWER;
  
  46..60:Level:=THREAD_PRIORITY_NORMAL;
  
  61..75:Level:=THREAD_PRIORITY_HIGHER;
  76..90:Level:=THREAD_PRIORITY_HIGHEST;
  91..100:Level:=THREAD_PRIORITY_CRITICAL;
 end;
 
 {Set the priority}
 if ThreadSetPriority(ThreadGetCurrent,Level) <> LongWord(INVALID_HANDLE_VALUE) then
  begin
   Result:=0;
  end;
end; 

{==============================================================================}

function piThreadStart(fn:TpiThreadCreateFunction):PtrInt;
{Function called internally to start the thread}
begin
 {}
 Result:=0;
 try
  {Call the thread function}
  if Assigned(fn) then
   begin
    fn;
   end; 
 except
  on E: Exception do
   begin
    if THREAD_LOG_ENABLED then ThreadLogError('wiringPiThread: Exception: ' + E.Message + ' at ' + IntToHex(LongWord(ExceptAddr),8));
   end;
 end; 
end;

{==============================================================================}

function piThreadCreate(fn:TpiThreadCreateFunction):LongInt;
{Create and start a thread}
var
 myThread:TThreadHandle;
begin
 {}
 if BeginThread(TThreadFunc(piThreadStart),@fn,myThread,THREAD_STACK_DEFAULT_SIZE) = INVALID_HANDLE_VALUE then
  begin
   Result:=-1;
   Exit;
  end;
  
 Result:=0; 
end; 

{==============================================================================}

procedure piLock(key:LongInt);
{Activate (Lock) a mutex}
begin
 {}
 if (key < 0) or (key > 3) then Exit;
 
 if piMutexes[key] = INVALID_HANDLE_VALUE then
  begin
   piMutexes[key]:=MutexCreate;
  end;
  
 MutexLock(piMutexes[key]);
end;

{==============================================================================}

procedure piUnlock(key:LongInt);
{Deactivate (Unlock) a mutex}
begin
 {}
 if (key < 0) or (key > 3) then Exit;
 
 if piMutexes[key] = INVALID_HANDLE_VALUE then
  begin
   piMutexes[key]:=MutexCreate;
  end;
  
 MutexUnlock(piMutexes[key]);
end;

{==============================================================================}
{==============================================================================}
{Wiring Raspberry Pi Specific Functions}
function piBoardRev:LongInt;
{Return a number representing the hardware revision of the board}
{This is not strictly the board revision but is used to check the
 layout of the GPIO connector - and there are 2 types that we are
 really interested in here. The very earliest Pi's and the
 ones that came after that which switched some pins ....
 
 Revision 1 really means the early Model A and B's.
 Revision 2 is everything else - it covers the B, B+ and CM.
  ... and the Pi 2 - which is a B+ ++  ...
  ... and the Pi 0 - which is an A+ ...
 
 The main difference between the revision 1 and 2 system that I use here
 is the mapping of the GPIO pins. From revision 2, the Pi Foundation changed
 3 GPIO pins on the (original) 26-way header - BCM_GPIO 22 was dropped and
 replaced with 27, and 0 + 1 - I2C bus 0 was changed to 2 + 3; I2C bus 1}
 
{Note: Raspberry Pi specific}
var 
 Revision:LongWord;
 MachineType:LongWord;  
begin  
 {}
 if boardRev <> -1 then
  begin
   Result:=boardRev;
   Exit;
  end;
  
 {Check Machine Type}
 MachineType:=MachineGetType;
 case MachineType of
  MACHINE_TYPE_BCM2708:begin
    {Pi A/B/A+/B+/Zero}
    {Check Board Revision}
    Revision:=BoardGetRevision;
    case (Revision and BCM2835_BOARD_REV_MASK) of
     BCM2835_BOARD_REV_B_I2C0_2,BCM2835_BOARD_REV_B_I2C0_3:begin
       {Revision 1}
       boardRev:=1;
      end;
     else
      begin
       {Revision 2}
       boardRev:=2;
      end;      
    end;
   end;
  MACHINE_TYPE_BCM2709,MACHINE_TYPE_BCM2710:begin
    {Pi 2/3}
    {Revision 2}
    boardRev:=2;
   end;
  else
   begin
    {Unknown}
   end;
 end;
 
 Result:=boardRev;
end;
 
{==============================================================================}

procedure piBoardId(var model,rev,mem,maker,warranty:LongInt);
{Return the real details of the board we have}

{Note: Raspberry Pi specific}
var 
 Revision:LongWord;
begin
 {}
 {Setup Defaults}
 model:=0;
 rev:=0;
 mem:=0;
 maker:=0;
 warranty:=0;
 
 {Check Board Revision}
 Revision:=BoardGetRevision;
 if (Revision and BCM2835_BOARD_REVISION_ENCODED_FLAG) <> 0 then
  begin
   {New Encoding}
   model:=(Revision and BCM2835_BOARD_REVISION_MODEL_MASK) shr 4;
   rev:=(Revision and BCM2835_BOARD_REVISION_PCB_MASK) shr 0;
   mem:=(Revision and BCM2835_BOARD_REVISION_MEMORY_MASK) shr 20;
   maker:=(Revision and BCM2835_BOARD_REVISION_MANUFACTURER_MASK) shr 16;
  end
 else
  begin
   {Old Encoding}
   case (Revision and BCM2835_BOARD_REV_MASK) of
    BCM2835_BOARD_REV_B_I2C0_2:begin
      model:=PI_MODEL_B;
      rev:=PI_VERSION_1;
      mem:=0;
      maker:=PI_MAKER_EGOMAN;
     end;
    BCM2835_BOARD_REV_B_I2C0_3:begin
      model:=PI_MODEL_B;
      rev:=PI_VERSION_1_1;
      mem:=0;
      maker:=PI_MAKER_EGOMAN;
     end;
    BCM2835_BOARD_REV_B_I2C1_4:begin
      model:=PI_MODEL_B;
      rev:=PI_VERSION_2;
      mem:=0;
      maker:=PI_MAKER_SONY;
     end;
    BCM2835_BOARD_REV_B_I2C1_5:begin
      model:=PI_MODEL_B;
      rev:=PI_VERSION_2;
      mem:=0;
      maker:=PI_MAKER_UNKNOWN;
     end;
    BCM2835_BOARD_REV_B_I2C1_6:begin
      model:=PI_MODEL_B;
      rev:=PI_VERSION_2;
      mem:=0;
      maker:=PI_MAKER_EGOMAN;
     end;
    BCM2835_BOARD_REV_A_7:begin
      model:=PI_MODEL_A;
      rev:=PI_VERSION_2;
      mem:=0;
      maker:=PI_MAKER_EGOMAN;
     end;
    BCM2835_BOARD_REV_A_8:begin
      model:=PI_MODEL_A;
      rev:=PI_VERSION_2;
      mem:=0;
      maker:=PI_MAKER_SONY;
     end;
    BCM2835_BOARD_REV_A_9:begin
      model:=PI_MODEL_B;
      rev:=PI_VERSION_2;
      mem:=0;
      maker:=PI_MAKER_UNKNOWN;
     end;
    BCM2835_BOARD_REV_B_REV2_d:begin
      model:=PI_MODEL_B;
      rev:=PI_VERSION_2;
      mem:=1;
      maker:=PI_MAKER_EGOMAN;
     end;
    BCM2835_BOARD_REV_B_REV2_e:begin
      model:=PI_MODEL_B;
      rev:=PI_VERSION_2;
      mem:=1;
      maker:=PI_MAKER_SONY;
     end;
    BCM2835_BOARD_REV_B_REV2_f:begin
      model:=PI_MODEL_B;
      rev:=PI_VERSION_2;
      mem:=1;
      maker:=PI_MAKER_EGOMAN;
     end;
    BCM2835_BOARD_REV_B_PLUS:begin
      model:=PI_MODEL_BP;
      rev:=PI_VERSION_1_2;
      mem:=1;
      maker:=PI_MAKER_SONY;
     end;
    BCM2835_BOARD_REV_CM:begin
      model:=PI_MODEL_CM;
      rev:=PI_VERSION_1_2;
      mem:=1;
      maker:=PI_MAKER_SONY;
     end;
    BCM2835_BOARD_REV_A_PLUS:begin
      model:=PI_MODEL_AP;
      rev:=PI_VERSION_1_2;
      mem:=0;
      maker:=PI_MAKER_SONY;
     end;
    BCM2835_BOARD_REV_B_PLUS_2:begin
      model:=PI_MODEL_BP;
      rev:=PI_VERSION_1_2;
      mem:=1;
      maker:=PI_MAKER_EGOMAN;
     end;
    BCM2835_BOARD_REV_CM_2:begin
      model:=PI_MODEL_CM;
      rev:=PI_VERSION_1_2;
      mem:=1;
      maker:=PI_MAKER_SONY;
     end;
    BCM2835_BOARD_REV_A_PLUS_2:begin
      model:=PI_MODEL_AP;
      rev:=PI_VERSION_1_1;
      mem:=0;
      maker:=PI_MAKER_SONY;
     end;
   end; 
  end;  
end; 

{==============================================================================}

function wpiPinToGpio(wpiPin:LongInt):LongInt;
{Translate a wiringPi Pin number to native GPIO pin number}

{Note: Raspberry Pi specific}
begin
 {}
 Result:=pinToGpio[wpiPin and 63];
end; 

{==============================================================================}

function physPinToGpio(physPin:LongInt):LongInt;
{Translate a physical Pin number to native GPIO pin number}

{Note: Raspberry Pi specific}
begin
 {}
 Result:=physToGpio[physPin and 63];
end; 

{==============================================================================}

procedure setPadDrive(group,value:LongInt);
{Set the PAD driver value}

{Note: Raspberry Pi specific}
var
 wrVal:LongWord;
begin
 {}
 if (wiringPiMode = WPI_MODE_PINS) or (wiringPiMode = WPI_MODE_PHYS) or (wiringPiMode = WPI_MODE_GPIO) or (wiringPiMode = WPI_MODE_GPIO_SYS) then
  begin
   {Check Base}
   if RASPBERRY_PI_PERI_BASE = 0 then Exit;
   
   {Check Group}
   if (group < 0) or (group > 2) then Exit;
   
   {Memory Barrier}
   DataMemoryBarrier; {Before the First Write}
   
   {Setup Value}
   wrVal:=BCM2835_CM_PASSWORD or $18 or (value and 7);
   
   {Write Value}
   PLongWord(GPIO_PADS + group + 11)^:=wrVal;
  end; 
end; 

{==============================================================================}

function getAlt(pin:LongInt):LongInt;
{Returns the ALT bits for a given pin}

{Note: Raspberry Pi specific}
var
 GPIOPin:LongWord;
begin
 {}
 {Check Pin}
 if (pin and PI_GPIO_MASK) = 0 then
  begin
   {On-board pin}
   if wiringPiMode = WPI_MODE_PINS then
    begin
     GPIOPin:=pinToGpio[pin];
    end
   else if wiringPiMode = WPI_MODE_PHYS then
    begin
     GPIOPin:=physToGpio[pin];
    end
   else if (wiringPiMode = WPI_MODE_GPIO) or (wiringPiMode = WPI_MODE_GPIO_SYS) then
    begin
     GPIOPin:=pin;
    end
   else 
    begin
     Result:=0;
     Exit;
    end;

   Result:=GPIODeviceFunctionGet(GPIODevice,GPIOPin);
  end
 else
  begin
   Result:=0;
  end;  
end; 

{==============================================================================}

procedure pwmSetMode(mode:LongInt);
{Select the native "balanced" mode, or standard mark:space mode}

{Note: Raspberry Pi specific}
begin
 {}
 if (wiringPiMode = WPI_MODE_PINS) or (wiringPiMode = WPI_MODE_PHYS) or (wiringPiMode = WPI_MODE_GPIO) or (wiringPiMode = WPI_MODE_GPIO_SYS) then
  begin
   {Check Base}
   if RASPBERRY_PI_PERI_BASE = 0 then Exit;
   
   {Memory Barrier}
   DataMemoryBarrier; {Before the First Write}
   
   {Set Mode}
   if mode = PWM_MODE_MS then
    begin
     PLongWord(PWM_BASE + BCM2835_PWM_CTL)^:=BCM2835_PWM_CTL_PWEN1 or BCM2835_PWM_CTL_PWEN2 or BCM2835_PWM_CTL_MSEN1 or BCM2835_PWM_CTL_MSEN2;
    end
   else
    begin
     PLongWord(PWM_BASE + BCM2835_PWM_CTL)^:=BCM2835_PWM_CTL_PWEN1 or BCM2835_PWM_CTL_PWEN2;
    end;
  end;  
end; 

{==============================================================================}

procedure pwmSetRange(range:LongWord);
{Set the PWM range register}

{Note: Raspberry Pi specific}
begin
 {}
 if (wiringPiMode = WPI_MODE_PINS) or (wiringPiMode = WPI_MODE_PHYS) or (wiringPiMode = WPI_MODE_GPIO) or (wiringPiMode = WPI_MODE_GPIO_SYS) then
  begin
   {Check Base}
   if RASPBERRY_PI_PERI_BASE = 0 then Exit;
   
   {Memory Barrier}
   DataMemoryBarrier; {Before the First Write}

   {Set Range}
   PLongWord(PWM_BASE + BCM2835_PWM_RNG1)^:=range;
   delayMicroseconds(10);
   PLongWord(PWM_BASE + BCM2835_PWM_RNG2)^:=range;
   delayMicroseconds(10);
  end;  
end; 

{==============================================================================}

procedure pwmSetClock(divisor:LongInt);
{Set/Change the PWM clock}

{Note: Raspberry Pi specific}
var
 control:LongWord;
begin
 {}
 if (wiringPiMode = WPI_MODE_PINS) or (wiringPiMode = WPI_MODE_PHYS) or (wiringPiMode = WPI_MODE_GPIO) or (wiringPiMode = WPI_MODE_GPIO_SYS) then
  begin
   {Check Base}
   if RASPBERRY_PI_PERI_BASE = 0 then Exit;
   
   {Setup Divisor}
   divisor:=divisor and 4095;
   
   {Memory Barrier}
   DataMemoryBarrier; {Before the First Write}
   
   {Preserve Control register}
   control:=PLongWord(PWM_BASE + BCM2835_PWM_CTL)^;
   
   {Stop PWM prior to stopping PWM clock in MS mode otherwise BUSY stays high}
   PLongWord(PWM_BASE + BCM2835_PWM_CTL)^:=0;
   
   {Stop PWM clock before changing divisor}
   PLongWord(CLOCK_BASE + BCM2835_CM_PWMCTL)^:=BCM2835_CM_PASSWORD or (PLongWord(CLOCK_BASE + BCM2835_CM_PWMCTL)^ and not(BCM2835_CM_CTL_ENAB));
   delayMicroseconds(110);	
   
   {Wait for not Busy}
   while (PLongWord(CLOCK_BASE + BCM2835_CM_PWMCTL)^ and BCM2835_CM_CTL_BUSY) <> 0 do
    begin
     delayMicroseconds(1);
    end;
   
   {Set Divisor}
   PLongWord(CLOCK_BASE + BCM2835_CM_PWMDIV)^:=BCM2835_CM_PASSWORD or (divisor shl 12);
   
   {Start PWM clock}
   PLongWord(CLOCK_BASE + BCM2835_CM_PWMCTL)^:=BCM2835_CM_PASSWORD or PLongWord(CLOCK_BASE + BCM2835_CM_PWMCTL)^ or BCM2835_CM_CTL_ENAB;
   
   {Restore Control register}
   PLongWord(PWM_BASE + BCM2835_PWM_CTL)^:=control;
   
   {Memory Barrier}
   DataMemoryBarrier; {After the Last Read} 
  end;  
end; 

{==============================================================================}

procedure pwmToneWrite(pin,freq:LongInt);
{Output the given frequency on the Pi's PWM pin}

{Note: Raspberry Pi specific}
var
 range:LongInt;
begin
 {}
 if RASPBERRY_PI_PERI_BASE = 0 then Exit;
 
 if freq = 0 then
  begin
   {Off}
   pwmWrite(pin,0); 
  end 
 else
  begin
   range:=600000 div freq;
   pwmSetRange(range);
   pwmWrite(pin,freq div 2);
  end;
end; 

{==============================================================================}

procedure gpioClockSet(pin,freq:LongInt);
{Set the frequency on a GPIO clock pin}

{Note: Raspberry Pi specific}
var
 divi:LongInt;
 divr:LongInt;
 divf:LongInt;
 GPIOPin:LongWord;
begin
 {}
 {Check Pin}
 if (pin and PI_GPIO_MASK) = 0 then
  begin
   {On-board pin}
   if wiringPiMode = WPI_MODE_PINS then
    begin
     GPIOPin:=pinToGpio[pin];
    end
   else if wiringPiMode = WPI_MODE_PHYS then
    begin
     GPIOPin:=physToGpio[pin];
    end
   else if (wiringPiMode = WPI_MODE_GPIO) or (wiringPiMode = WPI_MODE_GPIO_SYS) then
    begin
     GPIOPin:=pin;
    end
   else 
    begin
     Exit;
    end;
    
   {Check Base}
   if RASPBERRY_PI_PERI_BASE = 0 then Exit;
    
   {Check Freq} 
   if freq < 1 then Exit;
   
   divi:=19200000 div freq;
   divr:=19200000 mod freq;
   divf:=Trunc((divr * 4096) / 19200000);
   
   if divi > 4095 then divi:=4095;
   
   {Memory Barrier}
   DataMemoryBarrier; {Before the First Write}
   
   {Stop GPIO Clock}
   PLongWord(CLOCK_BASE + gpioToClkCon[GPIOPin])^:=BCM2835_CM_PASSWORD or (PLongWord(CLOCK_BASE + gpioToClkCon[GPIOPin])^ and not(BCM2835_CM_CTL_ENAB));
   delayMicroseconds(110);
   
   {Wait for not Busy}
   while (PLongWord(CLOCK_BASE + gpioToClkCon[GPIOPin])^ and BCM2835_CM_CTL_BUSY) <> 0 do
    begin
     delayMicroseconds(1);
    end;
    
   {Set Dividers}
   PLongWord(CLOCK_BASE + gpioToClkDiv[GPIOPin])^:=BCM2835_CM_PASSWORD or (divi shl 12) or divf;
   delayMicroseconds(10);
   
   {Set Source}   
   PLongWord(CLOCK_BASE + gpioToClkCon[GPIOPin])^:=BCM2835_CM_PASSWORD or BCM2835_CM_CTL_SRC_OSC;
   delayMicroseconds(10);
   
   {Start Clock}   
   PLongWord(CLOCK_BASE + gpioToClkCon[GPIOPin])^:=BCM2835_CM_PASSWORD or PLongWord(CLOCK_BASE + gpioToClkCon[GPIOPin])^ or BCM2835_CM_CTL_ENAB;
   
   {Memory Barrier}
   DataMemoryBarrier; {After the Last Read} 
  end;
end; 

{==============================================================================}

function digitalReadByte:LongInt;
{Read an 8-bit byte form the first 8 GPIO pins}

{Note: Raspberry Pi specific}
var
 pin:LongInt;
 raw:LongWord;
 data:LongWord;
 GPIOPin:LongWord;
begin
 {}
 Result:=0;
 
 {Check Base}
 if RASPBERRY_PI_PERI_BASE = 0 then Exit;
 
 raw:=PLongWord(GPIO_BASE + BCM2835_GPLEV0)^; {First bank for these pins}
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
 
 data:=0;
 for pin:=0 to 7 do
  begin
   GPIOPin:=pinToGpio[pin];
   if (raw and (1 shl GPIOPin)) = 0 then //To Do //This should be shr instead?
    begin
     data:=(data shl 1) or 0;
    end
   else
    begin
     data:=(data shl 1) or 1;
    end;    
  end;
 
 Result:=data;
end; 

{==============================================================================}

procedure digitalWriteByte(value:LongInt);
{Write an 8-bit byte to the first 8 GPIO pins}

{Note: Raspberry Pi specific}
var
 pin:LongInt;
 mask:LongInt;
 pinSet:LongWord;
 pinClr:LongWord;
begin
 {}
 {Check Base}
 if RASPBERRY_PI_PERI_BASE = 0 then Exit;
 
 pinSet:=0;
 pinClr:=0;
 
 mask:=1;
 
 for pin:=0 to 7 do
  begin
   if (value and mask) = 0 then
    begin
     pinClr:=pinClr or (1 shl pinToGpio[pin]);
    end
   else
    begin
     pinSet:=pinSet or (1 shl pinToGpio[pin]);
    end;    
  
   mask:=mask shl 1;
  end;
  
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 PLongWord(GPIO_BASE + BCM2835_GPCLR0)^:=pinClr;
 PLongWord(GPIO_BASE + BCM2835_GPSET0)^:=pinSet;
end; 

{==============================================================================}

function digitalReadByte2:LongInt;
{Read an 8-bit byte from the second set of 8 GPIO pins}

{Note: Raspberry Pi specific}
var
 data:LongWord;
begin
 {}
 Result:=0;
 
 {Check Base}
 if RASPBERRY_PI_PERI_BASE = 0 then Exit;
 
 data:=(PLongWord(GPIO_BASE + BCM2835_GPLEV0)^ shr 20) and $FF; {First bank for these pins}
 
 {Memory Barrier}
 DataMemoryBarrier; {After the Last Read} 
 
 Result:=data;
end; 

{==============================================================================}

procedure digitalWriteByte2(value:LongInt);
{Write an 8-bit byte to the second set of 8 GPIO pins}

{Note: Raspberry Pi specific}
begin
 {}
 {Check Base}
 if RASPBERRY_PI_PERI_BASE = 0 then Exit;
 
 {Memory Barrier}
 DataMemoryBarrier; {Before the First Write}
 
 PLongWord(GPIO_BASE + BCM2835_GPCLR0)^:=$0FF00000;
 PLongWord(GPIO_BASE + BCM2835_GPSET0)^:=(value and $FF) shl 20;
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
