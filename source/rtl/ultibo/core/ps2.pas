{
PS2 Keyboard/Mouse Controller Support.

Copyright (C) 2017 - SoftOz Pty Ltd.

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

  
References
==========
 
 PS2 Keyboard - http://wiki.osdev.org/PS/2_Keyboard
                http://www.computer-engineering.org/ps2keyboard/
                
 PS2 Mouse - http://wiki.osdev.org/PS/2_Mouse
             http://www.computer-engineering.org/ps2mouse/
             
 PS2 Scancodes - http://www.computer-engineering.org/ps2keyboard/scancodes1.html
                 http://www.computer-engineering.org/ps2keyboard/scancodes2.html
                 http://www.computer-engineering.org/ps2keyboard/scancodes3.html
                 
PS2 Keyboard/Mouse Controller
=============================
 
 This unit provides supporting functions and defintions for PS/2 keyboard and mouse 
 controller drivers. 
 
 The constants defined here are not a complete set but represent the most commonly
 used PS/2 functions and operations. Of most importance is the mouse packet and the
 keyboard scancode structures which provide support for receiving the actual data
 from a keyboard or a mouse.
 
 This unit also includes the PS/2 scancode sets which define the actual values that
 are received in response to each key press and release. Currently only scancode set
 2 is defined as in many instances that is considered the standard scancode set and 
 some devices simply do not support scancode set 1 or 3.
 
}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit PS2; 

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,Keyboard,Mouse,SysUtils;
 
//To Do //Intellimouse extensions (Wheel plus Buttons 4 and 5) see http://www.computer-engineering.org/ps2mouse/
        //Scancode sets 1 and 3
        
{==============================================================================}
{Global definitions}
{$INCLUDE ..\core\GlobalDefines.inc}

{==============================================================================}
const
 {PS2 specific constants}
 {Keyboard command constants (See: http://wiki.osdev.org/PS/2_Keyboard)}
 PS2_KEYBOARD_COMMAND_SET_LEDS               = $ED; {Set LEDS (Data: See below)}
 PS2_KEYBOARD_COMMAND_ECHO                   = $EE; {Echo (Data: None)}
 PS2_KEYBOARD_COMMAND_SCANCODE               = $F0; {Get/set current scan code (Data: See below)}
 PS2_KEYBOARD_COMMAND_IDENTIFY               = $F2; {Identify (Data: None) (See: http://wiki.osdev.org/%228042%22_PS/2_Controller#Detecting_PS.2F2_Device_Types)}
 PS2_KEYBOARD_COMMAND_SET_RATE_DELAY         = $F3; {Set typematic rate and delay (Data: See below)}
 PS2_KEYBOARD_COMMAND_ENABLE_SCAN            = $F4; {Enable scanning (keyboard will send scan codes) (Data: None)}
 PS2_KEYBOARD_COMMAND_DISABLE_SCAN           = $F5; {Disable scanning (keyboard won't send scan codes) (Data: None) Note: May also restore default parameters}
 PS2_KEYBOARD_COMMAND_SET_DEFAULTS           = $F6; {Set default parameters  (Data: None)}
 PS2_KEYBOARD_COMMAND_SET_ALL_TYPEMATIC_ONLY = $F7; {Set all keys to typematic/autorepeat only (Scancode set 3 only) (Data: None)}
 PS2_KEYBOARD_COMMAND_SET_ALL_MAKE_RELEASE   = $F8; {Set all keys to make/release (Scancode set 3 only) (Data: None)}
 PS2_KEYBOARD_COMMAND_SET_ALL_MAKE_ONLY      = $F9; {Set all keys to make only (Scancode set 3 only) (Data: None)}
 PS2_KEYBOARD_COMMAND_SET_ALL_MAKE_TYPEMATIC = $FA; {Set all keys to typematic/autorepeat/make/release (Scancode set 3 only) (Data: None)}
 PS2_KEYBOARD_COMMAND_SET_TYPEMATIC_ONLY     = $FB; {Set specific key to typematic/autorepeat only (Scancode set 3 only)  (Data: Scancode for key)}
 PS2_KEYBOARD_COMMAND_SET_MAKE_RELEASE       = $FC; {Set specific key to make/release (Scancode set 3 only) (Data: Scancode for key)}
 PS2_KEYBOARD_COMMAND_SET_MAKE_ONLY          = $FD; {Set specific key to make only (Scancode set 3 only) (Data: Scancode for key)}
 PS2_KEYBOARD_COMMAND_RESEND                 = $FE; {Resend last byte (Data: None)}
 PS2_KEYBOARD_COMMAND_RESET                  = $FF; {Reset and start self-test (Data: None)}
 
 PS2_KEYBOARD_SET_LEDS_SCROLLLOCK = (1 shl 0); {ScrollLock}
 PS2_KEYBOARD_SET_LEDS_NUMLOCK    = (1 shl 1); {NumberLock}
 PS2_KEYBOARD_SET_LEDS_CAPSLOCK   = (1 shl 2); {CapsLock}
 
 PS2_KEYBOARD_SCANCODE_GET  = 0; {Get current scan code set}
 PS2_KEYBOARD_SCANCODE_SET1 = 1; {Set scan code set 1}
 PS2_KEYBOARD_SCANCODE_SET2 = 2; {Set scan code set 2}
 PS2_KEYBOARD_SCANCODE_SET3 = 3; {Set scan code set 3}
 
 PS2_KEYBOARD_SET_REPEAT_RATE_MASK = $0F; {Repeat rate (00000b = 30 Hz, ..., 11111b = 2 Hz) }
 PS2_KEYBOARD_SET_DELAY_MASK       = $70; {Delay before keys repeat (00b = 250 ms, 01b = 500 ms, 10b = 750 ms, 11b = 1000 ms)}
 {Bit 7 Must be zero }
  
 PS2_KEYBOARD_SET_REPEAT_RATE_30_0 = $00; {30.0cps}
 PS2_KEYBOARD_SET_REPEAT_RATE_26_7 = $01; {26.7cps}
 PS2_KEYBOARD_SET_REPEAT_RATE_24_0 = $02; {24.0cps}
 PS2_KEYBOARD_SET_REPEAT_RATE_21_8 = $03; {21.8cps}
 PS2_KEYBOARD_SET_REPEAT_RATE_20_7 = $04; {20.7cps}
 PS2_KEYBOARD_SET_REPEAT_RATE_18_5 = $05; {18.5cps}
 PS2_KEYBOARD_SET_REPEAT_RATE_17_1 = $06; {17.1cps}
 PS2_KEYBOARD_SET_REPEAT_RATE_16_0 = $07; {16.0cps}
 PS2_KEYBOARD_SET_REPEAT_RATE_15_0 = $08; {15.0cps}
 PS2_KEYBOARD_SET_REPEAT_RATE_13_3 = $09; {13.3cps}
 PS2_KEYBOARD_SET_REPEAT_RATE_12_0 = $0A; {12.0cps}
 PS2_KEYBOARD_SET_REPEAT_RATE_10_9 = $0B; {10.9cps}
 PS2_KEYBOARD_SET_REPEAT_RATE_10_0 = $0C; {10.0cps}
 PS2_KEYBOARD_SET_REPEAT_RATE_9_2  = $0D; {9.2cps}
 PS2_KEYBOARD_SET_REPEAT_RATE_8_6  = $0E; {8.6cps}
 PS2_KEYBOARD_SET_REPEAT_RATE_8_0  = $0F; {8.0cps}
 PS2_KEYBOARD_SET_REPEAT_RATE_7_5  = $10; {7.5cps}
 PS2_KEYBOARD_SET_REPEAT_RATE_6_7  = $11; {6.7cps}
 PS2_KEYBOARD_SET_REPEAT_RATE_6_0  = $12; {6.0cps}
 PS2_KEYBOARD_SET_REPEAT_RATE_5_5  = $13; {5.5cps}
 PS2_KEYBOARD_SET_REPEAT_RATE_5_0  = $14; {5.0cps}
 PS2_KEYBOARD_SET_REPEAT_RATE_4_6  = $15; {4.6cps}
 PS2_KEYBOARD_SET_REPEAT_RATE_4_3  = $16; {4.3cps}
 PS2_KEYBOARD_SET_REPEAT_RATE_4_0  = $17; {4.0cps}
 PS2_KEYBOARD_SET_REPEAT_RATE_3_7  = $18; {3.7cps}
 PS2_KEYBOARD_SET_REPEAT_RATE_3_3  = $19; {3.3cps}
 PS2_KEYBOARD_SET_REPEAT_RATE_3_0  = $1A; {3.0cps}
 PS2_KEYBOARD_SET_REPEAT_RATE_2_7  = $1B; {2.7cps}
 PS2_KEYBOARD_SET_REPEAT_RATE_2_5  = $1C; {2.5cps}
 PS2_KEYBOARD_SET_REPEAT_RATE_2_3  = $1D; {2.3cps}
 PS2_KEYBOARD_SET_REPEAT_RATE_2_1  = $1E; {2.1cps}
 PS2_KEYBOARD_SET_REPEAT_RATE_2_0  = $1F; {2.0cps}
  
 PS2_KEYBOARD_SET_DELAY_250  = $00; {250ms}
 PS2_KEYBOARD_SET_DELAY_500  = $10; {500ms}
 PS2_KEYBOARD_SET_DELAY_750  = $20; {750ms}
 PS2_KEYBOARD_SET_DELAY_1000 = $30; {1000ms}
 
 {Mouse command constants (See: http://wiki.osdev.org/PS/2_Mouse)}
 PS2_MOUSE_COMMAND_RESET           = $FF; {Reset (Data: None)}
 PS2_MOUSE_COMMAND_RESEND          = $FE; {Resend (Data: None)}
 PS2_MOUSE_COMMAND_SET_DEFAULTS    = $F6; {Set Defaults (Data: None)}
 PS2_MOUSE_COMMAND_DISABLE_REPORT  = $F5; {Disable Data Reporting (Data: None)}
 PS2_MOUSE_COMMAND_ENABLE_REPORT   = $F4; {Enable Data Reporting (Data: None)}
 PS2_MOUSE_COMMAND_SET_SAMPLE_RATE = $F3; {Set Sample Rate (Data: Sample rate, ranges from 10-200)}
 PS2_MOUSE_COMMAND_GET_DEVICE_ID   = $F2; {Get Device ID (Data: None) (See: http://wiki.osdev.org/%228042%22_PS/2_Controller#Detecting_PS.2F2_Device_Types)}
 PS2_MOUSE_COMMAND_SET_REMOTE_MODE = $F0; {Set Remote Mode (Data: None)}
 PS2_MOUSE_COMMAND_SET_WRAP_MODE   = $EE; {Set Wrap Mode (Data: None)}
 PS2_MOUSE_COMMAND_RESET_WRAP_MODE = $EC; {Reset Wrap Mode (Data: None)}
 PS2_MOUSE_COMMAND_READ_DATA       = $EB; {Read Data (Data: None)}
 PS2_MOUSE_COMMAND_SET_STREAM_MODE = $EA; {Set Stream Mode (Data: None)}
 PS2_MOUSE_COMMAND_STATUS_REQUEST  = $E9; {Status Request (Data: None)}
 PS2_MOUSE_COMMAND_SET_RESOLUTION  = $E8; {Set Resolution (Data: See below)}
 
 PS2_MOUSE_COMMAND_SAMPLE_RATE_10  = 10;  {10 samples/sec}
 PS2_MOUSE_COMMAND_SAMPLE_RATE_20  = 20;  {20 samples/sec}
 PS2_MOUSE_COMMAND_SAMPLE_RATE_40  = 40;  {40 samples/sec}
 PS2_MOUSE_COMMAND_SAMPLE_RATE_60  = 60;  {60 samples/sec}
 PS2_MOUSE_COMMAND_SAMPLE_RATE_80  = 80;  {80 samples/sec}
 PS2_MOUSE_COMMAND_SAMPLE_RATE_100 = 100; {100 samples/sec}
 PS2_MOUSE_COMMAND_SAMPLE_RATE_200 = 200; {200 samples/sec}
 
 PS2_MOUSE_COMMAND_RESOLUTION_1 = $00; {1 count/mm}
 PS2_MOUSE_COMMAND_RESOLUTION_2 = $01; {2 count/mm}
 PS2_MOUSE_COMMAND_RESOLUTION_4 = $02; {4 count/mm}
 PS2_MOUSE_COMMAND_RESOLUTION_5 = $03; {8 count/mm}
 
 {Response constants}
 PS2_RESPONSE_NONE           = $00; {Key detection error or internal buffer overrun}
 PS2_RESPONSE_SELF_TEST_PASS = $AA; {Self test passed (sent after "0xFF (reset)" command or keyboard power up)}
 PS2_RESPONSE_ECHO           = $EE; {Response to "0xEE (echo)" command}
 PS2_RESPONSE_ACK            = $FA; {Command acknowledged (ACK) }
 PS2_RESPONSE_SELFTEST_FAIL1 = $FC; {Self test failed (sent after "0xFF (reset)" command or keyboard power up)}
 PS2_RESPONSE_SELFTEST_FAIL2 = $FD; {Self test failed (sent after "0xFF (reset)" command or keyboard power up)}
 PS2_RESPONSE_RESEND         = $FE; {Resend (keyboard wants controller to repeat last command it sent)}
 PS2_RESPONSE_ERROR          = $FF; {Key detection error or internal buffer overrun}
 
 {Mouse packet bits}
 PS2_MOUSE_BITS_YO = (1 shl 7); {Y-Axis Overflow}
 PS2_MOUSE_BITS_XO = (1 shl 6); {X-Axis Overflow}
 PS2_MOUSE_BITS_YS = (1 shl 5); {Y-Axis Sign Bit (9-Bit Y-Axis Relative Offset)}
 PS2_MOUSE_BITS_XS = (1 shl 4); {X-Axis Sign Bit (9-Bit X-Axis Relative Offset)}
 PS2_MOUSE_BITS_AO = (1 shl 3); {Always One} 
 PS2_MOUSE_BITS_BM = (1 shl 2); {Button Middle (Normally Off = 0)}
 PS2_MOUSE_BITS_BR = (1 shl 1); {Button Right (Normally Off = 0)} 
 PS2_MOUSE_BITS_BL = (1 shl 0); {Button Left (Normally Off = 0)} 
 
 {Keyboard scancode types}
 PS2_SCANCODE_MAKE = 0;  {Key Down (Press)}
 PS2_SCANCODE_BREAK = 1; {Key Up (Release)}
 
 {Keyboard scancode counts} 
 PS2_SCANCODE_PC104_COUNT = 104;
 PS2_SCANCODE_ACPI_COUNT = 3;
 PS2_SCANCODE_MULTIMEDIA_COUNT = 18;
 PS2_SCANCODE_COUNT = PS2_SCANCODE_PC104_COUNT + PS2_SCANCODE_ACPI_COUNT + PS2_SCANCODE_MULTIMEDIA_COUNT;
 
{==============================================================================}
type
 {PS2 specific types}
 PPS2ScancodeData = ^TPS2ScancodeData;
 TPS2ScancodeData = array[0..(PS2_SCANCODE_COUNT * 2) - 1] of array[0..9] of Byte;
 
 {Keyboard types}
 PPS2KeyboardScancode = ^TPS2KeyboardScancode;
 TPS2KeyboardScancode = record
  ScancodeSet:Byte;
  Scancode:array[0..7] of Byte;
 end;
 
 {Mouse types}
 PPS2MousePacket = ^TPS2MousePacket;
 TPS2MousePacket = record
  MouseBits:Byte; {Bits (See PS2_MOUSE_BITS_* above)}
  MouseX:Byte;    {X-Axis Movement Value (Relative)}
  MouseY:Byte;    {Y-Axis Movement Value (Relative)}
 end;

{==============================================================================}
{var}
 {PS2 specific variables}
 
{==============================================================================}
{Initialization Functions}
 
{==============================================================================}
{PS2 Functions}

{==============================================================================}
{PS2 Helper Functions}
function KeyboardLEDsToPS2LEDs(LEDs:LongWord;var PS2LEDs:Byte):LongWord;
function KeyboardRateAndDelayToPS2Typematic(Rate,Delay:LongWord;var PS2Typematic:Byte):LongWord;

function PS2KeyboardScancodeMatch(KeyboardScancode:PPS2KeyboardScancode;var Index:LongInt):LongWord;

function PS2KeyboardScancodeToScanCode(KeyboardScancode:PPS2KeyboardScancode;Index:LongInt;var ScanCode:Word):LongWord;
function PS2KeyboardScancodeToModifiers(KeyboardScancode:PPS2KeyboardScancode;Index:LongInt;var Modifiers:LongWord):LongWord;

function MouseSampleRateToPS2SampleRate(Rate:LongWord;var PS2Rate:Byte):LongWord;

function PS2MousePacketToMouseData(MousePacket:PPS2MousePacket;MouseData:PMouseData;Flags:LongWord):LongWord;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {PS2 specific variables}
 {Keyboard scancode set 1} //To Do //See: http://www.computer-engineering.org/ps2keyboard/scancodes1.html
 PS2_KEYBOARD_SCANCODE_1:TPS2ScancodeData = (
  {Make (Press) scancodes}
  {PC101,102,104}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  
  {ACPI}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  
  {Windows Multimedia}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  
  {Break (Release) scancodes}
  {PC101,102,104}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  
  {ACPI}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  
  {Windows Multimedia}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE)               {}
 );
 
 {Keyboard scancode set 2}
 PS2_KEYBOARD_SCANCODE_2:TPS2ScancodeData = (
  {Make (Press) scancodes}
  {PC101,102,104}
  ($1C, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_A),               {A}
  ($32, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_B),               {B}
  ($21, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_C),               {C}
  ($23, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_D),               {D}
  ($24, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_E),               {E}
  ($2B, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_F),               {F}
  ($34, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_G),               {G}
  ($33, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_H),               {H}
  ($43, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_I),               {I}
  ($3B, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_J),               {J}
  ($42, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_K),               {K}
  ($4B, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_L),               {L}
  ($3A, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_M),               {M}
  ($31, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_N),               {N}
  ($44, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_O),               {O}
  ($4D, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_P),               {P}
  ($15, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_Q),               {Q}
  ($2D, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_R),               {R}
  ($1B, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_S),               {S}
  ($2C, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_T),               {T}
  ($3C, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_U),               {U}
  ($2A, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_V),               {V}
  ($1D, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_W),               {W}
  ($22, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_X),               {X}
  ($35, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_Y),               {Y}
  ($1A, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_Z),               {Z}
  ($45, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_0),               {0}
  ($16, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_1),               {1}
  ($1E, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_2),               {2}
  ($26, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_3),               {3}
  ($25, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_4),               {4}
  ($2E, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_5),               {5}
  ($36, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_6),               {6}
  ($3D, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_7),               {7}
  ($3E, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_8),               {8}
  ($46, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_9),               {9}
  
  ($0E, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_GRAVE),           {`}
  ($4E, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_MINUS),           {-}
  ($55, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_EQUALS),          {=}
  ($5D, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_BACKSLASH),       {\}
  ($66, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_BACKSPACE),       {BKSP}
  ($29, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_SPACE),           {SPACE}
  ($0D, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_TAB),             {TAB}
  ($58, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_CAPSLOCK),        {CAPS}
  ($12, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_LEFT_SHIFT),      {L SHFT}
  ($14, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_LEFT_CTRL),       {L CTRL}
  ($E0, $1F, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_LEFT_GUI),        {L GUI}
  ($11, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_LEFT_ALT),        {L ALT}
  ($59, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_RIGHT_SHIFT),     {R SHFT}
  ($E0, $14, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_RIGHT_CTRL),      {R CTRL}
  ($E0, $27, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_RIGHT_GUI),       {R GUI}
  ($E0, $11, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_RIGHT_ALT),       {R ALT}
  ($E0, $2F, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_APPLICATION),     {APPS}
  ($5A, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_ENTER),           {ENTER}
  ($76, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_ESCAPE),          {ESC}
  ($05, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_F1),              {F1}
  ($06, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_F2),              {F2}
  ($04, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_F3),              {F3}
  ($0C, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_F4),              {F4}
  ($03, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_F5),              {F5}
  ($0B, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_F6),              {F6}
  ($83, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_F7),              {F7}
  ($0A, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_F8),              {F8}
  ($01, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_F9),              {F9}
  ($09, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_F10),             {F10}
  ($78, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_F11),             {F11}
  ($07, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_F12),             {F12}
  ($E0, $12, $E0, $7C, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_PRINTSCREEN),     {PRNT SCRN}
  ($7E, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_SCROLLLOCK),      {SCROLL}
  ($E1, $14, $77, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_PAUSE),           {PAUSE}
  
  ($54, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_LEFT_SQUARE),     {[}
  ($E0, $70, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_INSERT),          {INSERT}
  ($E0, $6C, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_HOME),            {HOME}
  ($E0, $7D, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_PAGEUP),          {PG UP}
  ($E0, $71, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_DELETE),          {DELETE}
  ($E0, $69, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_END),             {END}
  ($E0, $7A, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_PAGEDN),          {PG DN}
  ($E0, $75, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_UP_ARROW),        {U ARROW}
  ($E0, $6B, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_LEFT_ARROW),      {L ARROW}
  ($E0, $72, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_DOWN_ARROW),      {D ARROW}
  ($E0, $74, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_RIGHT_ARROW),     {R ARROW}
  ($77, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NUMLOCK),         {NUM}
  ($E0, $4A, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_KEYPAD_SLASH),    {KP /}
  ($7C, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_KEYPAD_ASTERISK), {KP *}
  ($7B, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_KEYPAD_MINUS),    {KP -}
  ($79, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_KEYPAD_PLUS),     {KP +}
  ($E0, $5A, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_KEYPAD_ENTER),    {KP EN}
  ($71, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_KEYPAD_PERIOD),   {KP .}
  ($70, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_KEYPAD_0),        {KP 0}
  ($69, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_KEYPAD_1),        {KP 1}
  ($72, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_KEYPAD_2),        {KP 2}
  ($7A, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_KEYPAD_3),        {KP 3}
  ($6B, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_KEYPAD_4),        {KP 4}
  ($73, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_KEYPAD_5),        {KP 5}
  ($74, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_KEYPAD_6),        {KP 6}
  ($6C, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_KEYPAD_7),        {KP 7}
  ($75, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_KEYPAD_8),        {KP 8}
  ($7D, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_KEYPAD_9),        {KP 9}
  ($5B, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_RIGHT_SQUARE),    {]}
  ($4C, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_SEMICOLON),       {;}
  ($52, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_APOSTROPHE),      {'}
  ($41, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_COMMA),           {,}
  ($49, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_PERIOD),          {.}
  ($4A, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_SLASH),           {/}
  
  {ACPI} //To Do //Allocate SCAN_CODE_* values for these?
  ($E0, $37, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_POWER),           {Power}
  ($E0, $3F, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),            {Sleep}
  ($E0, $5E, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),            {Wake}
  
  {Windows Multimedia} //To Do //Allocate SCAN_CODE_* values for these?
  ($E0, $4D, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),            {Next Track}
  ($E0, $15, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),            {Previous Track}
  ($E0, $3B, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),            {Stop}
  ($E0, $34, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),            {Play/Pause}
  ($E0, $23, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_MUTE),            {Mute}
  ($E0, $32, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_VOLUMEUP),        {Volume Up}
  ($E0, $21, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_VOLUMEDN),        {Volume Down}
  ($E0, $50, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),            {Media Select}
  ($E0, $48, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),            {E-Mail}
  ($E0, $2B, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),            {Calculator}
  ($E0, $40, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),            {My Computer}
  ($E0, $10, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),            {WWW Search}
  ($E0, $3A, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),            {WWW Home}
  ($E0, $38, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),            {WWW Back}
  ($E0, $30, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),            {WWW Forward}
  ($E0, $28, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),            {WWW Stop}
  ($E0, $20, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),            {WWW Refresh}
  ($E0, $18, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),            {WWW Favorites}
  
  {Break (Release) scancodes}
  {PC101,102,104}
  ($F0, $1C, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_A),               {A}
  ($F0, $32, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_B),               {B}
  ($F0, $21, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_C),               {C}
  ($F0, $23, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_D),               {D}
  ($F0, $24, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_E),               {E}
  ($F0, $2B, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_F),               {F}
  ($F0, $34, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_G),               {G}
  ($F0, $33, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_H),               {H}
  ($F0, $43, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_I),               {I}
  ($F0, $3B, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_J),               {J}
  ($F0, $42, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_K),               {K}
  ($F0, $4B, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_L),               {L}
  ($F0, $3A, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_M),               {M}
  ($F0, $31, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_N),               {N}
  ($F0, $44, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_O),               {O}
  ($F0, $4D, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_P),               {P}
  ($F0, $15, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_Q),               {Q}
  ($F0, $2D, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_R),               {R}
  ($F0, $1B, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_S),               {S}
  ($F0, $2C, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_T),               {T}
  ($F0, $3C, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_U),               {U}
  ($F0, $2A, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_V),               {V}
  ($F0, $1D, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_W),               {W}
  ($F0, $22, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_X),               {X}
  ($F0, $35, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_Y),               {Y}
  ($F0, $1A, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_Z),               {Z}
  ($F0, $45, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_0),               {0}
  ($F0, $16, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_1),               {1}
  ($F0, $1E, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_2),               {2}
  ($F0, $26, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_3),               {3}
  ($F0, $25, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_4),               {4}
  ($F0, $2E, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_5),               {5}
  ($F0, $36, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_6),               {6}
  ($F0, $3D, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_7),               {7}
  ($F0, $3E, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_8),               {8}
  ($F0, $46, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_9),               {9}
  
  ($F0, $0E, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_GRAVE),           {`}
  ($F0, $4E, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_MINUS),           {-}
  ($F0, $55, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_EQUALS),          {=}
  ($F0, $5D, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_BACKSLASH),       {\}
  ($F0, $66, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_BACKSPACE),       {BKSP}
  ($F0, $29, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_SPACE),           {SPACE}
  ($F0, $0D, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_TAB),             {TAB}
  ($F0, $58, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_CAPSLOCK),        {CAPS}
  ($F0, $12, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_LEFT_SHIFT),      {L SHFT}
  ($F0, $14, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_LEFT_CTRL),       {L CTRL}
  ($E0, $F0, $1F, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_LEFT_GUI),        {L GUI}
  ($F0, $11, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_LEFT_ALT),        {L ALT}
  ($F0, $59, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_RIGHT_SHIFT),     {R SHFT}
  ($E0, $F0, $14, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_RIGHT_CTRL),      {R CTRL}
  ($E0, $F0, $27, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_RIGHT_GUI),       {R GUI}
  ($E0, $F0, $11, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_RIGHT_ALT),       {R ALT}
  ($E0, $F0, $2F, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_APPLICATION),     {APPS}
  ($F0, $5A, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_ENTER),           {ENTER}
  ($F0, $76, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_ESCAPE),          {ESC}
  ($F0, $05, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_F1),              {F1}
  ($F0, $06, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_F2),              {F2}
  ($F0, $04, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_F3),              {F3}
  ($F0, $0C, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_F4),              {F4}
  ($F0, $03, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_F5),              {F5}
  ($F0, $0B, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_F6),              {F6}
  ($F0, $83, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_F7),              {F7}
  ($F0, $0A, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_F8),              {F8}
  ($F0, $01, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_F9),              {F9}
  ($F0, $09, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_F10),             {F10}
  ($F0, $78, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_F11),             {F11}
  ($F0, $07, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_F12),             {F12}
  ($E0, $F0, $7C, $E0, $F0, $12, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_PRINTSCREEN),     {PRNT SCRN} //To Do //This seems to be backwards?
  ($F0, $7E, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_SCROLLLOCK),      {SCROLL}
  ($E1, $F0, $14, $F0, $77, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_PAUSE),           {PAUSE} 
  
  ($F0, $54, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_LEFT_SQUARE),     {[}
  ($E0, $F0, $70, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_INSERT),          {INSERT}
  ($E0, $F0, $6C, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_HOME),            {HOME}
  ($E0, $F0, $7D, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_PAGEUP),          {PG UP}
  ($E0, $F0, $71, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_DELETE),          {DELETE}
  ($E0, $F0, $69, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_END),             {END}
  ($E0, $F0, $7A, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_PAGEDN),          {PG DN}
  ($E0, $F0, $75, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_UP_ARROW),        {U ARROW}
  ($E0, $F0, $6B, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_LEFT_ARROW),      {L ARROW}
  ($E0, $F0, $72, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_DOWN_ARROW),      {D ARROW}
  ($E0, $F0, $74, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_RIGHT_ARROW),     {R ARROW}
  ($F0, $77, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NUMLOCK),         {NUM}
  ($E0, $F0, $4A, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_KEYPAD_SLASH),    {KP /}
  ($F0, $7C, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_KEYPAD_ASTERISK), {KP *}
  ($F0, $7B, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_KEYPAD_MINUS),    {KP -}
  ($F0, $79, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_KEYPAD_PLUS),     {KP +}
  ($E0, $F0, $5A, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_KEYPAD_ENTER),    {KP EN}
  ($F0, $71, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_KEYPAD_PERIOD),   {KP .}
  ($F0, $70, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_KEYPAD_0),        {KP 0}
  ($F0, $69, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_KEYPAD_1),        {KP 1}
  ($F0, $72, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_KEYPAD_2),        {KP 2}
  ($F0, $7A, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_KEYPAD_3),        {KP 3}
  ($F0, $6B, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_KEYPAD_4),        {KP 4}
  ($F0, $73, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_KEYPAD_5),        {KP 5}
  ($F0, $74, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_KEYPAD_6),        {KP 6}
  ($F0, $6C, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_KEYPAD_7),        {KP 7}
  ($F0, $75, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_KEYPAD_8),        {KP 8}
  ($F0, $7D, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_KEYPAD_9),        {KP 9}
  ($F0, $5B, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_RIGHT_SQUARE),    {]}
  ($F0, $4C, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_SEMICOLON),       {;}
  ($F0, $52, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_APOSTROPHE),      {'}
  ($F0, $41, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_COMMA),           {,}
  ($F0, $49, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_PERIOD),          {.}
  ($F0, $4A, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_SLASH),           {/}
  
  {ACPI} //To Do //Allocate SCAN_CODE_* values for these?
  ($E0, $F0, $37, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_POWER),           {Power}
  ($E0, $F0, $3F, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),            {Sleep}
  ($E0, $F0, $5E, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),            {Wake}
  
  {Windows Multimedia} //To Do //Allocate SCAN_CODE_* values for these?
  ($E0, $F0, $4D, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),            {Next Track}
  ($E0, $F0, $15, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),            {Previous Track}
  ($E0, $F0, $3B, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),            {Stop}
  ($E0, $F0, $34, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),            {Play/Pause}
  ($E0, $F0, $23, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_MUTE),            {Mute}
  ($E0, $F0, $32, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_VOLUMEUP),        {Volume Up}
  ($E0, $F0, $21, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_VOLUMEDN),        {Volume Down}
  ($E0, $F0, $50, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),            {Media Select}
  ($E0, $F0, $48, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),            {E-Mail}
  ($E0, $F0, $2B, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),            {Calculator}
  ($E0, $F0, $40, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),            {My Computer}
  ($E0, $F0, $10, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),            {WWW Search}
  ($E0, $F0, $3A, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),            {WWW Home}
  ($E0, $F0, $38, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),            {WWW Back}
  ($E0, $F0, $30, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),            {WWW Forward}
  ($E0, $F0, $28, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),            {WWW Stop}
  ($E0, $F0, $20, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),            {WWW Refresh}
  ($E0, $F0, $18, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE)             {WWW Favorites}
  
 );

 {Keyboard scancode set 3} //To Do //See: http://www.computer-engineering.org/ps2keyboard/scancodes3.html
 PS2_KEYBOARD_SCANCODE_3:TPS2ScancodeData = (
  {Make (Press) scancodes}
  {PC101,102,104}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  
  {ACPI}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  
  {Windows Multimedia}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_MAKE,  SCAN_CODE_NONE),              {}
  
  {Break (Release) scancodes}
  {PC101,102,104}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  
  {ACPI}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  
  {Windows Multimedia}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE),              {}
  ($00, $00, $00, $00, $00, $00, $00, $00, PS2_SCANCODE_BREAK, SCAN_CODE_NONE)               {}
 );
 
{==============================================================================}
{==============================================================================}
{Initialization Functions}
 
{==============================================================================}
{==============================================================================}
{PS2 Functions}

{==============================================================================}
{==============================================================================}
{PS2 Helper Functions}
function KeyboardLEDsToPS2LEDs(LEDs:LongWord;var PS2LEDs:Byte):LongWord;
{Map the Keyboard LED values to the PS/2 Keyboard LED values}
{LEDs: The Keyboard LED values to map (eg KEYBOARD_LED_NUMLOCK)}
{PS2LEDs: The returned PS/2 Keyboard LED values (eg PS2_KEYBOARD_SET_LEDS_NUMLOCK)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Get LEDs}
 PS2LEDs:=0;
 if (LEDs and KEYBOARD_LED_NUMLOCK) <> 0 then PS2LEDs:=PS2LEDs or PS2_KEYBOARD_SET_LEDS_NUMLOCK;
 if (LEDs and KEYBOARD_LED_CAPSLOCK) <> 0 then PS2LEDs:=PS2LEDs or PS2_KEYBOARD_SET_LEDS_CAPSLOCK;
 if (LEDs and KEYBOARD_LED_SCROLLLOCK) <> 0 then PS2LEDs:=PS2LEDs or PS2_KEYBOARD_SET_LEDS_SCROLLLOCK;
 
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function KeyboardRateAndDelayToPS2Typematic(Rate,Delay:LongWord;var PS2Typematic:Byte):LongWord;
{Translate the Keyboard Repeat Rate and Delay values to the PS/2 Keyboard Typematic value}
{Rate: The Keyboard Repeat Rate to translate (Milliseconds between repeats)}
{Delay: The Keyboard Repeat Delay to translate (Number of Repeat Rate intervals before first repeat)}
{PS2Typematic: The translated PS/2 Typematic value returned}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 RateValue:LongWord;
 DelayValue:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Rate}
 if Rate = 0 then Exit;
 
 {Check Delay}
 if Delay = 0 then Exit;
 
 {Get Rate Value (Milliseconds to CPS)}
 RateValue:=1000 div Rate;
 
 {Get Delay Value (Intervals to Milliseconds)}
 DelayValue:=Rate * Delay;
 
 PS2Typematic:=0;
 
 {Get Typematic Rate}
 if RateValue <= 2 then
  begin
   PS2Typematic:=PS2Typematic or PS2_KEYBOARD_SET_REPEAT_RATE_2_7;
  end
 else if RateValue <= 3 then 
  begin
   PS2Typematic:=PS2Typematic or PS2_KEYBOARD_SET_REPEAT_RATE_3_7;
  end
 else if RateValue <= 4 then 
  begin
   PS2Typematic:=PS2Typematic or PS2_KEYBOARD_SET_REPEAT_RATE_4_6;
  end
 else if RateValue <= 5 then 
  begin
   PS2Typematic:=PS2Typematic or PS2_KEYBOARD_SET_REPEAT_RATE_5_5;
  end
 else if RateValue <= 6 then 
  begin
   PS2Typematic:=PS2Typematic or PS2_KEYBOARD_SET_REPEAT_RATE_6_7;
  end
 else if RateValue <= 7 then 
  begin
   PS2Typematic:=PS2Typematic or PS2_KEYBOARD_SET_REPEAT_RATE_7_5;  
  end
 else if RateValue <= 8 then 
  begin
   PS2Typematic:=PS2Typematic or PS2_KEYBOARD_SET_REPEAT_RATE_8_6;  
  end
 else if RateValue <= 9 then 
  begin
   PS2Typematic:=PS2Typematic or PS2_KEYBOARD_SET_REPEAT_RATE_9_2;  
  end
 else if RateValue <= 10 then 
  begin
   PS2Typematic:=PS2Typematic or PS2_KEYBOARD_SET_REPEAT_RATE_10_9;  
  end
 else if RateValue <= 12 then 
  begin
   PS2Typematic:=PS2Typematic or PS2_KEYBOARD_SET_REPEAT_RATE_12_0;  
  end
 else if RateValue <= 13 then 
  begin
   PS2Typematic:=PS2Typematic or PS2_KEYBOARD_SET_REPEAT_RATE_13_3;  
  end
 else if RateValue <= 15 then 
  begin
   PS2Typematic:=PS2Typematic or PS2_KEYBOARD_SET_REPEAT_RATE_15_0;  
  end
 else if RateValue <= 16 then 
  begin
   PS2Typematic:=PS2Typematic or PS2_KEYBOARD_SET_REPEAT_RATE_16_0;  
  end
 else if RateValue <= 17 then 
  begin
   PS2Typematic:=PS2Typematic or PS2_KEYBOARD_SET_REPEAT_RATE_17_1;  
  end
 else if RateValue <= 18 then 
  begin
   PS2Typematic:=PS2Typematic or PS2_KEYBOARD_SET_REPEAT_RATE_18_5;  
  end
 else if RateValue <= 20 then 
  begin
   PS2Typematic:=PS2Typematic or PS2_KEYBOARD_SET_REPEAT_RATE_20_7;  
  end
 else if RateValue <= 21 then 
  begin
   PS2Typematic:=PS2Typematic or PS2_KEYBOARD_SET_REPEAT_RATE_21_8;  
  end
 else if RateValue <= 24 then 
  begin
   PS2Typematic:=PS2Typematic or PS2_KEYBOARD_SET_REPEAT_RATE_24_0;  
  end
 else if RateValue <= 26 then 
  begin
   PS2Typematic:=PS2Typematic or PS2_KEYBOARD_SET_REPEAT_RATE_26_7;  
  end
 else
  begin
   PS2Typematic:=PS2Typematic or PS2_KEYBOARD_SET_REPEAT_RATE_30_0;
  end;  
  
 {Get Typematic Delay}
 if DelayValue <= 250 then 
  begin
   PS2Typematic:=PS2Typematic or PS2_KEYBOARD_SET_DELAY_250;
  end
 else if DelayValue <= 500 then   
  begin
   PS2Typematic:=PS2Typematic or PS2_KEYBOARD_SET_DELAY_500;
  end
 else if DelayValue <= 750 then   
  begin
   PS2Typematic:=PS2Typematic or PS2_KEYBOARD_SET_DELAY_750;
  end
 else
  begin
   PS2Typematic:=PS2Typematic or PS2_KEYBOARD_SET_DELAY_1000;
  end;
 
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function PS2KeyboardScancodeMatch(KeyboardScancode:PPS2KeyboardScancode;var Index:LongInt):LongWord;
{Check a set of scancode bytes against the specified PS/2 Scancode set for a match}
{KeyboardScancode: Pointer to the scancode bytes and scancode set information}
{Index: The index of the matching scancode on success or the nearest match on not found
        (Pass -1 to start search from first entry, on subsequent calls pass the previous value to continue)}
{Return: ERROR_SUCCESS if matched, ERROR_NOT_FOUND if not matched or another error code on failure}
var
 Item:LongInt;
 Len:LongWord;
 Found:Boolean;
 Count:LongWord;
 Offset:LongWord;
 Data:PPS2ScancodeData;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Keyboard Scancode}
 if KeyboardScancode = nil then Exit;
 
 {Check Scancode Set}
 case KeyboardScancode.ScancodeSet of
  1:begin
    //Data:=@PS2_KEYBOARD_SCANCODE_1; //To Do //Scancode set 1 
    Exit;
   end;
  2:begin
    Data:=@PS2_KEYBOARD_SCANCODE_2;
   end;
  3:begin
    //Data:=@PS2_KEYBOARD_SCANCODE_3; //To Do //Scancode set 3
    Exit; 
   end;  
 else
   begin
    Exit;
   end;   
 end;

 {Setup Start}
 Item:=-1;
 Len:=0;
 Found:=False;
 Count:=0;
 if Index <> -1 then Count:=Index;
 if Count > (PS2_SCANCODE_COUNT * 2) - 1 then Count:=0;
 
 {Check Data}
 while Count < (PS2_SCANCODE_COUNT * 2) do
  begin
   {Compare Scancode}
   Offset:=0;
   while Offset <= 7 do
    begin
     if KeyboardScancode.Scancode[Offset] = Data[Count,Offset] then
      begin
       {Check Item}
       if (Offset + 1) > Len then
        begin
         Item:=Count;
         Len:=Offset + 1;
        end;
       
       {Check Found}
       if (KeyboardScancode.Scancode[Offset] = 0) and (Data[Count,Offset + 1] = 0) then
        begin
         Item:=Count;
         Found:=True;
         Break;
        end;
      end
     else
      begin
       Break;
      end;
      
     {Udpate Offset}
     Inc(Offset);
    end;
   
   {Check Found}   
   if Found then Break;
   
   {Update Count}
   Inc(Count);
  end;

 {Return Index}  
 Index:=Item;

 {Return Result} 
 if Found then Result:=ERROR_SUCCESS else Result:=ERROR_NOT_FOUND;
end;

{==============================================================================}

function PS2KeyboardScancodeToScanCode(KeyboardScancode:PPS2KeyboardScancode;Index:LongInt;var ScanCode:Word):LongWord;
{Return the Keyboard Scan Code value for a PS/2 scancode value}
{KeyboardScancode: Pointer to the scancode bytes and scancode set information}
{Index: The index value returned by PS2KeyboardScancodeMatch (-1 to search for match)}
{ScanCode: The returned keyboard scan code value (eg SCAN_CODE_A)}
{Return: ERROR_SUCCESS if completed or another error code on failure (ERROR_NOT_FOUND if not matched)}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Keyboard Scancode}
 if KeyboardScancode = nil then Exit;
 
 {Check Index}
 if Index = -1 then
  begin
   Result:=PS2KeyboardScancodeMatch(KeyboardScancode,Index);
   if Result <> ERROR_SUCCESS then Exit;
  end;
  
 {Check Scancode Set}
 case KeyboardScancode.ScancodeSet of
  1:begin
    {Get Scan Code}
    //ScanCode:=PS2_KEYBOARD_SCANCODE_1[Index,9]; //To Do //Scancode set 1  
    Exit;
   end;
  2:begin
    {Get Scan Code}
    ScanCode:=PS2_KEYBOARD_SCANCODE_2[Index,9]; 
   end;
  3:begin
    {Get Scan Code}
    //ScanCode:=PS2_KEYBOARD_SCANCODE_3[Index,9]; //To Do //Scancode set 3 
    Exit; 
   end;  
 else
   begin
    Exit;
   end;   
 end;
 
 {Return Result} 
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function PS2KeyboardScancodeToModifiers(KeyboardScancode:PPS2KeyboardScancode;Index:LongInt;var Modifiers:LongWord):LongWord;
{Return the Keyboard Modifiers flags for a PS/2 scancode value}
{Index: The index value returned by PS2KeyboardScancodeMatch (-1 to search for match)}
{Modifiers: The returned keyboard modifiers flags (eg KEYBOARD_LEFT_CTRL)}
{Return: ERROR_SUCCESS if completed or another error code on failure (ERROR_NOT_FOUND if not matched)}
var
 ScanType:Byte;
 ScanCode:Word;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Keyboard Scancode}
 if KeyboardScancode = nil then Exit;
 
 {Check Index}
 if Index = -1 then
  begin
   Result:=PS2KeyboardScancodeMatch(KeyboardScancode,Index);
   if Result <> ERROR_SUCCESS then Exit;
  end;
 
 {Check Scancode Set}
 case KeyboardScancode.ScancodeSet of
  1:begin
    {Get Scan Type and Code}
    //ScanType:=PS2_KEYBOARD_SCANCODE_1[Index,8]; //To Do //Scancode set 1 
    //ScanCode:=PS2_KEYBOARD_SCANCODE_1[Index,9]; //To Do //Scancode set 1  
    Exit;
   end;
  2:begin
    {Get Scan Type and Code}
    ScanType:=PS2_KEYBOARD_SCANCODE_2[Index,8]; 
    ScanCode:=PS2_KEYBOARD_SCANCODE_2[Index,9]; 
   end;
  3:begin
    {Get Scan Type and Code}
    //ScanType:=PS2_KEYBOARD_SCANCODE_3[Index,8]; //To Do //Scancode set 3  
    //ScanCode:=PS2_KEYBOARD_SCANCODE_3[Index,9]; //To Do //Scancode set 3  
    Exit; 
   end;  
 else
   begin
    Exit;
   end;   
 end;

 {Get Modifiers}
 {Key Up/Down}
 if ScanType = PS2_SCANCODE_MAKE then
  begin
   Modifiers:=KEYBOARD_KEYDOWN;
  end
 else if ScanType = PS2_SCANCODE_BREAK then
  begin
   Modifiers:=KEYBOARD_KEYUP;
  end;
 {Shift/Ctrl/Alt}
 case ScanCode of
  SCAN_CODE_LEFT_CTRL:Modifiers:=Modifiers or KEYBOARD_LEFT_CTRL;
  SCAN_CODE_LEFT_SHIFT:Modifiers:=Modifiers or KEYBOARD_LEFT_SHIFT;
  SCAN_CODE_LEFT_ALT:Modifiers:=Modifiers or KEYBOARD_LEFT_ALT;
  SCAN_CODE_LEFT_GUI:Modifiers:=Modifiers or KEYBOARD_LEFT_GUI;
  SCAN_CODE_RIGHT_CTRL:Modifiers:=Modifiers or KEYBOARD_RIGHT_CTRL;
  SCAN_CODE_RIGHT_SHIFT:Modifiers:=Modifiers or KEYBOARD_RIGHT_SHIFT;
  SCAN_CODE_RIGHT_ALT:Modifiers:=Modifiers or KEYBOARD_RIGHT_ALT;
  SCAN_CODE_RIGHT_GUI:Modifiers:=Modifiers or KEYBOARD_RIGHT_GUI;
 end; 
 
 {Return Result} 
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function MouseSampleRateToPS2SampleRate(Rate:LongWord;var PS2Rate:Byte):LongWord;
{Translate a Mouse Sample Rate value to the PS/2 Mouse Sample Rate value}
{Rate: The Mouse Sample Rate to translate (Samples per second)}
{PS2Rate: The translated PS/2 Sample Rate value returned}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Rate}
 if Rate = 0 then Exit;

 PS2Rate:=0;
 
 {Get Sample Rate}
 if Rate <= 10 then
  begin
   PS2Rate:=PS2_MOUSE_COMMAND_SAMPLE_RATE_10;  
  end
 else if Rate <= 20 then
  begin
   PS2Rate:=PS2_MOUSE_COMMAND_SAMPLE_RATE_20;
  end  
 else if Rate <= 40 then
  begin
   PS2Rate:=PS2_MOUSE_COMMAND_SAMPLE_RATE_40; 
  end  
 else if Rate <= 60 then
  begin
   PS2Rate:=PS2_MOUSE_COMMAND_SAMPLE_RATE_60; 
  end  
 else if Rate <= 80 then
  begin
   PS2Rate:=PS2_MOUSE_COMMAND_SAMPLE_RATE_80;  
  end  
 else if Rate <= 100 then
  begin
   PS2Rate:=PS2_MOUSE_COMMAND_SAMPLE_RATE_100; 
  end  
 else
  begin
   PS2Rate:=PS2_MOUSE_COMMAND_SAMPLE_RATE_200;
  end;  
 
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function PS2MousePacketToMouseData(MousePacket:PPS2MousePacket;MouseData:PMouseData;Flags:LongWord):LongWord;
{Translate a PS/2 Mouse Packet into a Mouse Data structure}
{MousePacket: Pointer to the PS/2 Mouse Packet received from the mouse}
{MouseData: Pointer to the Mouse Data structure to return}
{Flags: The Mouse device flags (eg MOUSE_FLAG_SWAP_BUTTONS)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 State:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Mouse Packet}
 if MousePacket = nil then Exit;
 
 {Check Mouse Data}
 if MouseData = nil then Exit;
 
 {Get Buttons}
 MouseData.Buttons:=0;
 if (MousePacket.MouseBits and PS2_MOUSE_BITS_BL) <> 0 then
  begin
   {Check Flags}
   if (Flags and MOUSE_FLAG_SWAP_BUTTONS) = 0 then
    begin
     MouseData.Buttons:=MouseData.Buttons or MOUSE_LEFT_BUTTON;
    end
   else
    begin
     MouseData.Buttons:=MouseData.Buttons or MOUSE_RIGHT_BUTTON;
    end;
  end;
 if (MousePacket.MouseBits and PS2_MOUSE_BITS_BR) <> 0 then
  begin
   {Check Flags}
   if (Flags and MOUSE_FLAG_SWAP_BUTTONS) = 0 then
    begin
     MouseData.Buttons:=MouseData.Buttons or MOUSE_RIGHT_BUTTON;
    end
   else
    begin
     MouseData.Buttons:=MouseData.Buttons or MOUSE_LEFT_BUTTON;
    end;
  end;
 if (MousePacket.MouseBits and PS2_MOUSE_BITS_BM) <> 0 then
  begin
   MouseData.Buttons:=MouseData.Buttons or MOUSE_MIDDLE_BUTTON;  
  end;
 
 {Get State}
 State:=MousePacket.MouseBits;
 
 {Get Offset X}
 MouseData.OffsetX:=MousePacket.MouseX - ((State shl 4) and $100);

 {Get Offset Y} 
 MouseData.OffsetY:=-(MousePacket.MouseY - ((State shl 3) and $100)); 
 
 {Get Offset Wheel}
 MouseData.OffsetWheel:=0;
 //To Do //Intellimouse extensions (see http://www.computer-engineering.org/ps2mouse/)
 
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}
{==============================================================================}

{initialization}
 {Nothing}
 
{==============================================================================}
 
{finalization}
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
