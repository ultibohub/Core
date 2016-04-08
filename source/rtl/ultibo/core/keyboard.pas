{
Ultibo Keyboard interface unit.

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

References
==========
 
 USB HID Device Class Definition 1_11.pdf
 
   http://www.usb.org/developers/hidpage/HID1_11.pdf

 USB HID Usage Tables 1_12v2.pdf

   http://www.usb.org/developers/hidpage/Hut1_12v2.pdf

Keyboard Devices
================

This unit provides both the Keyboard device interface and the generic USB HID keyboard driver.

The keyboard unit also provides the STDIN interface for the Run Time Library (RTL)

USB Keyboard Devices
====================


}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit Keyboard;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,USB,Keymap,SysUtils;

//To Do //Note: This driver currently uses HID Boot Protocol only and could be redesigned to
        //      use HID Report Protocol instead for proper Keyboard support (including multi language etc)
              
//To Do //Keyboard Logging
              
//To Do //Handle Keyboard Output report (LEDs) - see: HID Device Class Definition 1.11 Appendix B.1
        //See: usb_kbd_setled in \u-boot-HEAD-5745f8c\common\usb_kbd.c
              
//To Do //Handle Keyboard repeat etc
              
//To Do //Get a complete ScanCode table from Linux ? or elsewhere

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}
              
{==============================================================================}
const
 {Keyboard specific constants}
 KEYBOARD_NAME_PREFIX = 'Keyboard';  {Name prefix for Keyboard Devices}
 
 {Keyboard Device Types}
 KEYBOARD_TYPE_NONE     = 0;
 KEYBOARD_TYPE_USB      = 1;
 KEYBOARD_TYPE_PS2      = 2;
 KEYBOARD_TYPE_SERIAL   = 3;
 
 KEYBOARD_TYPE_MAX      = 3;
 
 {Keyboard Type Names}
 KEYBOARD_TYPE_NAMES:array[KEYBOARD_TYPE_NONE..KEYBOARD_TYPE_MAX] of String = (
  'KEYBOARD_TYPE_NONE',
  'KEYBOARD_TYPE_USB',
  'KEYBOARD_TYPE_PS2',
  'KEYBOARD_TYPE_SERIAL');
 
 {Keyboard Device States}
 KEYBOARD_STATE_DETACHED  = 0;
 KEYBOARD_STATE_DETACHING = 1;
 KEYBOARD_STATE_ATTACHING = 2;
 KEYBOARD_STATE_ATTACHED  = 3;
 
 KEYBOARD_STATE_MAX       = 3;
 
 {Keyboard State Names}
 KEYBOARD_STATE_NAMES:array[KEYBOARD_STATE_DETACHED..KEYBOARD_STATE_MAX] of String = (
  'KEYBOARD_STATE_DETACHED',
  'KEYBOARD_STATE_DETACHING',
  'KEYBOARD_STATE_ATTACHING',
  'KEYBOARD_STATE_ATTACHED');
 
 {Keyboard Device Flags}
 KEYBOARD_FLAG_NONE        = $00000000;
 KEYBOARD_FLAG_NON_BLOCK   = $00000001;
 KEYBOARD_FLAG_DIRECT_READ = $00000002;
 KEYBOARD_FLAG_PEEK_BUFFER = $00000004;
 
 KEYBOARD_FLAG_MASK = KEYBOARD_FLAG_NON_BLOCK or KEYBOARD_FLAG_DIRECT_READ or KEYBOARD_FLAG_PEEK_BUFFER;
 
 {Keyboard Device Control Codes}
 KEYBOARD_CONTROL_GET_FLAG         = 1;  {Get Flag}
 KEYBOARD_CONTROL_SET_FLAG         = 2;  {Set Flag}
 KEYBOARD_CONTROL_CLEAR_FLAG       = 3;  {Clear Flag}
 KEYBOARD_CONTROL_FLUSH_BUFFER     = 4;  {Flush Buffer}
 KEYBOARD_CONTROL_GET_LED          = 5;  {Get LED}
 KEYBOARD_CONTROL_SET_LED          = 6;  {Set LED}
 KEYBOARD_CONTROL_CLEAR_LED        = 7;  {Clear LED}
 KEYBOARD_CONTROL_GET_REPEAT_RATE  = 8;  {Get Repeat Rate}
 KEYBOARD_CONTROL_SET_REPEAT_RATE  = 9;  {Set Repeat Rate}
 KEYBOARD_CONTROL_GET_REPEAT_DELAY = 10; {Get Repeat Delay}
 KEYBOARD_CONTROL_SET_REPEAT_DELAY = 11; {Set Repeat Delay}

 {Keyboard Device LEDs}
 KEYBOARD_LED_NONE       = $00000000;
 KEYBOARD_LED_NUMLOCK    = $00000001;
 KEYBOARD_LED_CAPSLOCK   = $00000002;
 KEYBOARD_LED_SCROLLLOCK = $00000004;
 KEYBOARD_LED_COMPOSE    = $00000008;
 KEYBOARD_LED_KANA       = $00000010;
 
 KEYBOARD_LED_MASK = KEYBOARD_LED_NUMLOCK or KEYBOARD_LED_CAPSLOCK or KEYBOARD_LED_SCROLLLOCK or KEYBOARD_LED_COMPOSE or KEYBOARD_LED_KANA;
 
 {Keyboard Buffer Size}
 KEYBOARD_BUFFER_SIZE = 512; 

 {Keyboard Sampling Rate}
 KEYBOARD_REPEAT_RATE   = (40 div 4); {40msec -> 25cps}
 KEYBOARD_REPEAT_DELAY  = 10;         {10 x KEYBOARD_REPEAT_RATE = 400msec}
 
 {Keyboard Data Definitions}
 KEYBOARD_LEFT_CTRL    =  $00000001;
 KEYBOARD_LEFT_SHIFT   =  $00000002;
 KEYBOARD_LEFT_ALT     =  $00000004;
 KEYBOARD_LEFT_GUI     =  $00000008;
 KEYBOARD_RIGHT_CTRL   =  $00000010;
 KEYBOARD_RIGHT_SHIFT  =  $00000020;
 KEYBOARD_RIGHT_ALT    =  $00000040;
 KEYBOARD_RIGHT_GUI    =  $00000080;
 KEYBOARD_NUM_LOCK     =  $00000100; 
 KEYBOARD_CAPS_LOCK    =  $00000200; 
 KEYBOARD_SCROLL_LOCK  =  $00000400; 
 KEYBOARD_COMPOSE      =  $00000800; 
 KEYBOARD_KANA         =  $00001000; 
 KEYBOARD_KEYUP        =  $00002000; 
 KEYBOARD_KEYDOWN      =  $00004000; 
 
 {Keyboard logging}
 KEYBOARD_LOG_LEVEL_DEBUG     = LOG_LEVEL_DEBUG;  {Keyboard debugging messages}
 KEYBOARD_LOG_LEVEL_INFO      = LOG_LEVEL_INFO;   {Keyboard informational messages, such as a device being attached or detached}
 KEYBOARD_LOG_LEVEL_ERROR     = LOG_LEVEL_ERROR;  {Keyboard error messages}
 KEYBOARD_LOG_LEVEL_NONE      = LOG_LEVEL_NONE;   {No Keyboard messages}

var 
 KEYBOARD_DEFAULT_LOG_LEVEL:LongWord = KEYBOARD_LOG_LEVEL_INFO; {Minimum level for Keyboard messages.  Only messages with level greater than or equal to this will be printed}
 
var 
 {Keyboard logging}
 KEYBOARD_LOG_ENABLED:Boolean; 
 
{==============================================================================}
const
 {USB Keyboard specific constants}
 USBKEYBOARD_DRIVER_NAME = 'USB Keyboard Driver (HID boot protocol)'; {Name of USB keyboard driver}
 
 {HID Interface Subclass types (See USB HID v1.11 specification)}
 USB_HID_SUBCLASS_BOOT           = 1;     {Section 4.2}
 
 {HID Interface Protocol types (See USB HID v1.11 specification)}
 USB_HID_BOOT_PROTOCOL_KEYBOARD  = 1;     {Section 4.3}
 USB_HID_BOOT_PROTOCOL_MOUSE     = 2;     {Section 4.3}

 {HID Request types}
 USB_HID_REQUEST_GET_REPORT      = $01;
 USB_HID_REQUEST_GET_IDLE        = $02;
 USB_HID_REQUEST_GET_PROTOCOL    = $03;   {Section 7.2}
 USB_HID_REQUEST_SET_REPORT      = $09;
 USB_HID_REQUEST_SET_IDLE        = $0A;
 USB_HID_REQUEST_SET_PROTOCOL    = $0B;   {Section 7.2}
 
 {HID Protocol types}
 USB_HID_PROTOCOL_BOOT           = 0;     {Section 7.2.5}
 USB_HID_PROTOCOL_REPORT         = 1;     {Section 7.2.5}
 
 {HID Report types}
 USB_HID_REPORT_INPUT            = 1;     {Section 7.2.1}
 USB_HID_REPORT_OUTPUT           = 2;     {Section 7.2.1}
 USB_HID_REPORT_FEATURE          = 3;     {Section 7.2.1}
 
 {HID Report IDs}
 USB_HID_REPORTID_NONE           = 0;     {Section 7.2.1}
  
 {HID Boot Protocol Modifier bits}
 USB_HID_BOOT_LEFT_CTRL   = (1 shl 0);
 USB_HID_BOOT_LEFT_SHIFT  = (1 shl 1);
 USB_HID_BOOT_LEFT_ALT    = (1 shl 2);
 USB_HID_BOOT_LEFT_GUI    = (1 shl 3);
 USB_HID_BOOT_RIGHT_CTRL  = (1 shl 4);
 USB_HID_BOOT_RIGHT_SHIFT = (1 shl 5);
 USB_HID_BOOT_RIGHT_ALT   = (1 shl 6);
 USB_HID_BOOT_RIGHT_GUI   = (1 shl 7);

 {HID Boot Protocol Report data}
 USB_HID_BOOT_REPORT_SIZE  = 8;            {Appendix B of HID Device Class Definition 1.11}
 
 {HID Boot Protocol Output bits}
 USB_HID_BOOT_NUMLOCK_LED     = (1 shl 0);
 USB_HID_BOOT_CAPSLOCK_LED    = (1 shl 1);
 USB_HID_BOOT_SCROLLLOCK_LED  = (1 shl 2);
 USB_HID_BOOT_COMPOSE_LED     = (1 shl 3);
 USB_HID_BOOT_KANA_LED        = (1 shl 4);
 
 USB_HIB_BOOT_LEDMASK = USB_HID_BOOT_NUMLOCK_LED or USB_HID_BOOT_CAPSLOCK_LED or USB_HID_BOOT_SCROLLLOCK_LED or USB_HID_BOOT_COMPOSE_LED or USB_HID_BOOT_KANA_LED;
 
 {HID Boot Protocol Output data}
 USB_HID_BOOT_OUTPUT_SIZE  = 1;            {Appendix B of HID Device Class Definition 1.11}
 
 //To Do //Restructure to Keymap etc
 {Map of HID Boot Protocol keyboard Usage IDs to Characters}
 {Entries not filled in are left 0 and are interpreted as unrecognized input and ignored (Section 10 of the Universal Serial Bus HID Usage Tables v1.11)}
 USB_HID_BOOT_USAGE_ID:array[0..255] of array[0..1] of Char = (
    {0}   (#0, #0),
    {1}   (#0, #0),
    {2}   (#0, #0),
    {3}   (#0, #0),
    {4}   ('a', 'A'),
    {5}   ('b', 'B'),
    {6}   ('c', 'C'),
    {7}   ('d', 'D'),
    {8}   ('e', 'E'),
    {9}   ('f', 'F'),
    {10}  ('g', 'G'),
    {11}  ('h', 'H'),
    {12}  ('i', 'I'),
    {13}  ('j', 'J'),
    {14}  ('k', 'K'),
    {15}  ('l', 'L'),
    {16}  ('m', 'M'),
    {17}  ('n', 'N'),
    {18}  ('o', 'O'),
    {19}  ('p', 'P'),
    {20}  ('q', 'Q'),
    {21}  ('r', 'R'),
    {22}  ('s', 'S'),
    {23}  ('t', 'T'),
    {24}  ('u', 'U'),
    {25}  ('v', 'V'),
    {26}  ('w', 'W'),
    {27}  ('x', 'X'),
    {28}  ('y', 'Y'),
    {29}  ('z', 'Z'),
    {30}  ('1', '!'),
    {31}  ('2', '@'),
    {32}  ('3', '#'),
    {33}  ('4', '$'),
    {34}  ('5', '%'),
    {35}  ('6', '^'),
    {36}  ('7', '&'),
    {37}  ('8', '*'),
    {38}  ('9', '('),
    {39}  ('0', ')'),
    {40}  (#13, #13),     {Enter}
    {41}  (#0, #0),
    {42}  (#0, #0),       {Backspace}
    {43}  (#9, #9),       {Tab}       
    {44}  (' ', ' '),     {Space}  
    {45}  ('-', '_'),
    {46}  ('=', '+'),
    {47}  ('{', '('),
    {48}  ('}', ')'),
    {49}  ('\', '|'),
    {50}  (#0, #0),
    {51}  (';', ':'),
    {52}  ('''', '"'),
    {53}  ('`', '~'),
    {54}  (',', '<'),
    {55}  ('.', '>'),
    {56}  ('/', '?'),
    {57}  (#0, #0),       {Caps lock}
    {58}  (#0, #0),
    {59}  (#0, #0),
    {60}  (#0, #0),
    {61}  (#0, #0),
    {62}  (#0, #0),
    {63}  (#0, #0),
    {64}  (#0, #0),
    {65}  (#0, #0),
    {66}  (#0, #0),
    {67}  (#0, #0),
    {68}  (#0, #0),
    {69}  (#0, #0),
    {70}  (#0, #0),
    {71}  (#0, #0),      {Scroll Lock}
    {72}  (#0, #0),
    {73}  (#0, #0),
    {74}  (#0, #0),
    {75}  (#0, #0),
    {76}  (#0, #0),       {Delete}
    {77}  (#0, #0),
    {78}  (#0, #0),
    {79}  (#0, #0),
    {80}  (#0, #0),
    {81}  (#0, #0),
    {82}  (#0, #0),
    {83}  (#0, #0),       {Num Lock}
    {84}  ('/', '/'),     {Keypad /}                 
    {85}  ('*', '*'),     {Keypad *}                 
    {86}  ('-', '-'),     {Keypad -}                 
    {87}  ('+', '+'),     {Keypad +}                
    {88}  (#13,#13),      {Keypad Enter}             
    {89}  ('1', '1'),     {Keypad 1 and End}         
    {90}  ('2', '2'),     {Keypad 2 and Down Arrow}  
    {91}  ('3', '3'),     {...}                       
    {92}  ('4', '4'),
    {93}  ('5', '5'),
    {94}  ('6', '6'),
    {95}  ('7', '7'),
    {96}  ('8', '8'),
    {97}  ('9', '9'),
    {98}  ('0', '0'),
    {99}  ('.', #0),      {Keypad . and Delete}
    {100} (#0, #0),
    {101} (#0, #0),
    {102} (#0, #0),
    {103} ('=', '='),     {Keypad =}
    {104} (#0, #0),
    {105} (#0, #0),
    {106} (#0, #0),
    {107} (#0, #0),
    {108} (#0, #0),
    {109} (#0, #0),
    {110} (#0, #0),
    {111} (#0, #0),
    {112} (#0, #0),
    {113} (#0, #0),
    {114} (#0, #0),
    {115} (#0, #0),
    {116} (#0, #0),
    {117} (#0, #0),
    {118} (#0, #0),
    {119} (#0, #0),
    {120} (#0, #0),
    {121} (#0, #0),
    {122} (#0, #0),
    {123} (#0, #0),
    {124} (#0, #0),
    {125} (#0, #0),
    {126} (#0, #0),
    {127} (#0, #0),
    {128} (#0, #0),
    {129} (#0, #0),
    {130} (#0, #0),
    {131} (#0, #0),
    {132} (#0, #0),
    {133} (#0, #0),
    {134} (#0, #0),
    {135} (#0, #0),
    {136} (#0, #0),
    {137} (#0, #0),
    {138} (#0, #0),
    {139} (#0, #0),
    {140} (#0, #0),
    {141} (#0, #0),
    {142} (#0, #0),
    {143} (#0, #0),
    {144} (#0, #0),
    {145} (#0, #0),
    {146} (#0, #0),
    {147} (#0, #0),
    {148} (#0, #0),
    {149} (#0, #0),
    {150} (#0, #0),
    {151} (#0, #0),
    {152} (#0, #0),
    {153} (#0, #0),
    {154} (#0, #0),
    {155} (#0, #0),
    {156} (#0, #0),
    {157} (#0, #0),
    {158} (#0, #0),
    {159} (#0, #0),
    {160} (#0, #0),
    {161} (#0, #0),
    {162} (#0, #0),
    {163} (#0, #0),
    {164} (#0, #0),
    {165} (#0, #0),
    {166} (#0, #0),
    {167} (#0, #0),
    {168} (#0, #0),
    {169} (#0, #0),
    {170} (#0, #0),
    {171} (#0, #0),
    {172} (#0, #0),
    {173} (#0, #0),
    {174} (#0, #0),
    {175} (#0, #0),
    {176} (#0, #0),
    {177} (#0, #0),
    {178} (#0, #0),
    {179} (#0, #0),
    {180} (#0, #0),
    {181} (#0, #0),
    {182} (#0, #0),
    {183} (#0, #0),
    {184} (#0, #0),
    {185} (#0, #0),
    {186} (#0, #0),
    {187} (#0, #0),
    {188} (#0, #0),
    {189} (#0, #0),
    {190} (#0, #0),
    {191} (#0, #0),
    {192} (#0, #0),
    {193} (#0, #0),
    {194} (#0, #0),
    {195} (#0, #0),
    {196} (#0, #0),
    {197} (#0, #0),
    {198} (#0, #0),
    {199} (#0, #0),
    {200} (#0, #0),
    {201} (#0, #0),
    {202} (#0, #0),
    {203} (#0, #0),
    {204} (#0, #0),
    {205} (#0, #0),
    {206} (#0, #0),
    {207} (#0, #0),
    {208} (#0, #0),
    {209} (#0, #0),
    {210} (#0, #0),
    {211} (#0, #0),
    {212} (#0, #0),
    {213} (#0, #0),
    {214} (#0, #0),
    {215} (#0, #0),
    {216} (#0, #0),
    {217} (#0, #0),
    {218} (#0, #0),
    {219} (#0, #0),
    {220} (#0, #0),
    {221} (#0, #0),
    {222} (#0, #0),
    {223} (#0, #0),
    {224} (#0, #0),
    {225} (#0, #0),
    {226} (#0, #0),
    {227} (#0, #0),
    {228} (#0, #0),
    {229} (#0, #0),
    {230} (#0, #0),
    {231} (#0, #0),
    {232} (#0, #0),
    {233} (#0, #0),
    {234} (#0, #0),
    {235} (#0, #0),
    {236} (#0, #0),
    {237} (#0, #0),
    {238} (#0, #0),
    {239} (#0, #0),
    {240} (#0, #0),
    {241} (#0, #0),
    {242} (#0, #0),
    {243} (#0, #0),
    {244} (#0, #0),
    {245} (#0, #0),
    {246} (#0, #0),
    {247} (#0, #0),
    {248} (#0, #0),
    {249} (#0, #0),
    {250} (#0, #0),
    {251} (#0, #0),
    {252} (#0, #0),
    {253} (#0, #0),
    {254} (#0, #0),
    {255} (#0, #0)
  );
 
 USB_HID_BOOT_USAGE_NUMLOCK    = 83;
 USB_HID_BOOT_USAGE_CAPSLOCK   = 57;
 USB_HID_BOOT_USAGE_SCROLLLOCK = 71;
 
{==============================================================================}
type
 {Keyboard specific types}
 {Keyboard Data}
 PKeyboardData = ^TKeyboardData;
 TKeyboardData = record
  Modifiers:LongWord;
  ScanCode:Word;
  KeyCode:Word;
 end;

 {Keyboard Buffer}
 PKeyboardBuffer = ^TKeyboardBuffer;
 TKeyboardBuffer = record
  Wait:TSemaphoreHandle;     {Data ready semahore}
  Start:LongWord;            {Index of first buffer ready}
  Count:LongWord;            {Number of messages ready in buffer}
  Buffer:array[0..(KEYBOARD_BUFFER_SIZE - 1)] of TKeyboardData; 
 end;
 
 {Keyboard Device}
 PKeyboardDevice = ^TKeyboardDevice;
 
 {Keyboard Enumeration Callback}
 TKeyboardEnumerate = function(Keyboard:PKeyboardDevice;Data:Pointer):LongWord;
 {Keyboard Notification Callback}
 TKeyboardNotification = function(Device:PDevice;Data:Pointer;Notification:LongWord):LongWord;
 
 {Keyboard Device Methods}
 TKeyboardDeviceGet = function(Keyboard:PKeyboardDevice;var Character:Word):LongWord;
 TKeyboardDeviceRead = function(Keyboard:PKeyboardDevice;Buffer:Pointer;Size:LongWord;var Count:LongWord):LongWord;
 TKeyboardDeviceControl = function(Keyboard:PKeyboardDevice;Request:Integer;Argument1:LongWord;var Argument2:LongWord):LongWord;
 
 TKeyboardDevice = record
  {Device Properties}
  Device:TDevice;                      {The Device entry for this Keyboard}
  {Keyboard Properties}
  KeyboardId:LongWord;                 {Unique Id of this Keyboard in the Keyboard table}
  KeyboardState:LongWord;              {Keyboard state (eg KEYBOARD_STATE_ATTACHED)}
  KeyboardLEDs:LongWord;               {Keyboard LEDs (eg KEYBOARD_LED_NUMLOCK)}
  KeyboardRate:LongWord;               {Keyboard repeat rate}
  KeyboardDelay:LongWord;              {Keyboard repeat delay}
  DeviceGet:TKeyboardDeviceGet;        {A Device specific DeviceGet method implementing a standard Keyboard device interface} 
  DeviceRead:TKeyboardDeviceRead;      {A Device specific DeviceRead method implementing a standard Keyboard device interface} 
  DeviceControl:TKeyboardDeviceControl;{A Device specific DeviceControl method implementing a standard Keyboard device interface}
  {Driver Properties}
  Lock:TMutexHandle;                   {Keyboard lock}
  Buffer:TKeyboardBuffer;              {Keyboard input buffer}
  {Statistics Properties}
  ReceiveCount:LongWord;
  ReceiveErrors:LongWord;
  BufferOverruns:LongWord;
  {Internal Properties}                                                                                
  Prev:PKeyboardDevice;                {Previous entry in Keyboard table}
  Next:PKeyboardDevice;                {Next entry in Keyboard table}
 end;                                                                                          
 
{==============================================================================}
type
 {USB Keyboard specific types}
 PUSBKeyboardDevice = ^TUSBKeyboardDevice;
 TUSBKeyboardDevice = record
  {Keyboard Properties}
  Keyboard:TKeyboardDevice;
  {USB Properties}
  HIDInterface:PUSBInterface;            {USB HID Keyboard Interface}
  ReportRequest:PUSBRequest;             {USB request for keyboard report data}
  ReportEndpoint:PUSBEndpointDescriptor; {USB Keyboard Interrupt IN Endpoint}
  RecentKeys:array[0..5] of Byte;        {Keys that were reported as down in the last report} //To Do //Change for Repeat and Up/Down indication
  PendingCount:LongWord;                 {Number of USB requests pending for this keyboard}
  WaiterThread:TThreadId;                {Thread waiting for pending requests to complete (for keyboard detachment)}
 end;
 
{==============================================================================}
{var}
 {Keyboard specific variables}
 
{==============================================================================}
{var}
 {USB Keyboard specific variables}
 
{==============================================================================}
{Initialization Functions}
procedure KeyboardInit;

{==============================================================================}
{Keyboard Functions}
function KeyboardGet(var Character:Word):LongWord;
function KeyboardPeek:LongWord;
function KeyboardRead(Buffer:Pointer;Size:LongWord;var Count:LongWord):LongWord;
function KeyboardReadEx(Buffer:Pointer;Size,Flags:LongWord;var Count:LongWord):LongWord;

function KeyboardPut(Character:Word):LongWord;
function KeyboardWrite(Buffer:Pointer;Size,Count:LongWord):LongWord;

function KeyboardFlush:LongWord;

function KeyboardDeviceGet(Keyboard:PKeyboardDevice;var Character:Word):LongWord;
function KeyboardDeviceRead(Keyboard:PKeyboardDevice;Buffer:Pointer;Size:LongWord;var Count:LongWord):LongWord;
function KeyboardDeviceControl(Keyboard:PKeyboardDevice;Request:Integer;Argument1:LongWord;var Argument2:LongWord):LongWord;

function KeyboardDeviceSetState(Keyboard:PKeyboardDevice;State:LongWord):LongWord;

function KeyboardDeviceCreate:PKeyboardDevice;
function KeyboardDeviceCreateEx(Size:LongWord):PKeyboardDevice;
function KeyboardDeviceDestroy(Keyboard:PKeyboardDevice):LongWord;

function KeyboardDeviceRegister(Keyboard:PKeyboardDevice):LongWord;
function KeyboardDeviceDeregister(Keyboard:PKeyboardDevice):LongWord;

function KeyboardDeviceFind(KeyboardId:LongWord):PKeyboardDevice;
function KeyboardDeviceEnumerate(Callback:TKeyboardEnumerate;Data:Pointer):LongWord;

function KeyboardDeviceNotification(Keyboard:PKeyboardDevice;Callback:TKeyboardNotification;Data:Pointer;Notification,Flags:LongWord):LongWord;

{==============================================================================}
{RTL Console Functions}
function SysConsoleReadChar(var ACh:Char;AUserData:Pointer):Boolean;

{==============================================================================}
{USB Keyboard Functions}
function USBKeyboardDeviceGet(Keyboard:PKeyboardDevice;var Character:Word):LongWord;
function USBKeyboardDeviceRead(Keyboard:PKeyboardDevice;Buffer:Pointer;Size:LongWord;var Count:LongWord):LongWord;
function USBKeyboardDeviceControl(Keyboard:PKeyboardDevice;Request:Integer;Argument1:LongWord;var Argument2:LongWord):LongWord;

function USBKeyboardDriverBind(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;
function USBKeyboardDriverUnbind(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;

procedure USBKeyboardReportComplete(Request:PUSBRequest); 

{==============================================================================}
{Keyboard Helper Functions}
function KeyboardGetCount:LongWord; inline;

function KeyboardDeviceCheck(Keyboard:PKeyboardDevice):PKeyboardDevice;

function KeyboardDeviceTypeToString(KeyboardType:LongWord):String;
function KeyboardDeviceStateToString(KeyboardState:LongWord):String;

function KeyboardDeviceStateToNotification(State:LongWord):LongWord;

procedure KeyboardLog(Level:LongWord;Keyboard:PKeyboardDevice;const AText:String);
procedure KeyboardLogInfo(Keyboard:PKeyboardDevice;const AText:String);
procedure KeyboardLogError(Keyboard:PKeyboardDevice;const AText:String);
procedure KeyboardLogDebug(Keyboard:PKeyboardDevice;const AText:String);

{==============================================================================}
{USB Helper Functions}
function USBKeyboardCheckRecent(Keyboard:PUSBKeyboardDevice;UsageId:Byte):Boolean;

function USBKeyboardDeviceSetLEDs(Keyboard:PUSBKeyboardDevice;LEDs:Byte):LongWord;
function USBKeyboardDeviceSetIdle(Keyboard:PUSBKeyboardDevice;Duration,ReportId:Byte):LongWord;
function USBKeyboardDeviceSetProtocol(Keyboard:PUSBKeyboardDevice;Protocol:Byte):LongWord;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {Keyboard specific variables}
 KeyboardInitialized:Boolean;

 KeyboardTable:PKeyboardDevice;
 KeyboardTableLock:TCriticalSectionHandle = INVALID_HANDLE_VALUE;
 KeyboardTableCount:LongWord;

 KeyboardBuffer:PKeyboardBuffer;                          {Global keyboard input buffer}
 KeyboardBufferLock:TMutexHandle = INVALID_HANDLE_VALUE;  {Global keyboard buffer lock}
 
{==============================================================================}
{==============================================================================}
var
 {USB Keyboard specific variables}
 USBKeyboardDriver:PUSBDriver;  {USB Keyboard Driver interface (Set by KeyboardInit)}

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure KeyboardInit;
var
 Status:LongWord;
begin
 {}
 {Check Initialized}
 if KeyboardInitialized then Exit;
 
 {Initialize Logging}
 KEYBOARD_LOG_ENABLED:=(KEYBOARD_DEFAULT_LOG_LEVEL <> KEYBOARD_LOG_LEVEL_NONE); 
 
 {Initialize Keyboard Table}
 KeyboardTable:=nil;
 KeyboardTableLock:=CriticalSectionCreate; 
 KeyboardTableCount:=0;
 if KeyboardTableLock = INVALID_HANDLE_VALUE then
  begin
   if KEYBOARD_LOG_ENABLED then KeyboardLogError(nil,'Failed to create keyboard table lock');
  end;

 {Initialize Keyboard Buffer}
 KeyboardBuffer:=AllocMem(SizeOf(TKeyboardBuffer));
 KeyboardBufferLock:=INVALID_HANDLE_VALUE;
 if KeyboardBuffer = nil then
  begin
   if KEYBOARD_LOG_ENABLED then KeyboardLogError(nil,'Failed to allocate keyboard buffer');
  end
 else
  begin
   {Create Semaphore}
   KeyboardBuffer.Wait:=SemaphoreCreate(0);
   if KeyboardBuffer.Wait = INVALID_HANDLE_VALUE then
    begin
     if KEYBOARD_LOG_ENABLED then KeyboardLogError(nil,'Failed to create keyboard buffer semaphore');
    end;

   {Create Lock} 
   KeyboardBufferLock:=MutexCreate; 
   if KeyboardBufferLock = INVALID_HANDLE_VALUE then
    begin
     if KEYBOARD_LOG_ENABLED then KeyboardLogError(nil,'Failed to create keyboard buffer lock');
    end;
  end;  
 
 {Create USB Keyboard Driver}
 USBKeyboardDriver:=USBDriverCreate;
 if USBKeyboardDriver <> nil then
  begin
   {Update USB Keyboard Driver}
   {Driver}
   USBKeyboardDriver.Driver.DriverName:=USBKEYBOARD_DRIVER_NAME; 
   {USB}
   USBKeyboardDriver.DriverBind:=USBKeyboardDriverBind;
   USBKeyboardDriver.DriverUnbind:=USBKeyboardDriverUnbind;
   
   {Register USB Keyboard Driver}
   Status:=USBDriverRegister(USBKeyboardDriver); 
   if Status <> USB_STATUS_SUCCESS then
    begin
     if USB_LOG_ENABLED then USBLogError(nil,'Keyboard: Failed to register USB keyboard driver: ' + USBStatusToString(Status));
    end;
  end
 else
  begin
   if KEYBOARD_LOG_ENABLED then KeyboardLogError(nil,'Failed to create USB keyboard driver');
  end;
  
 {Setup Platform Console Handlers}
 ConsoleReadCharHandler:=SysConsoleReadChar;
  
 KeyboardInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{Keyboard Functions}
function KeyboardGet(var Character:Word):LongWord;
{Get the first character from the global keyboard buffer}
{Character: The returned character read from the buffer}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Count:LongWord;
 Data:TKeyboardData;
begin
 {}
 Result:=KeyboardReadEx(@Data,SizeOf(TKeyboardData),KEYBOARD_FLAG_NONE,Count);
 if Result = ERROR_SUCCESS then
  begin
   Character:=Data.KeyCode
  end;
end;

{==============================================================================}

function KeyboardPeek:LongWord;
{Peek at the global keyboard buffer to see if any data packets are ready}
{Return: ERROR_SUCCESS if packets are ready, ERROR_NO_MORE_ITEMS if not or another error code on failure}
var
 Count:LongWord;
 Data:TKeyboardData;
begin
 {}
 Result:=KeyboardReadEx(@Data,SizeOf(TKeyboardData),KEYBOARD_FLAG_NON_BLOCK or KEYBOARD_FLAG_PEEK_BUFFER,Count);
end;

{==============================================================================}

function KeyboardRead(Buffer:Pointer;Size:LongWord;var Count:LongWord):LongWord;
{Read keyboard data packets from the global keyboard buffer}
{Buffer: Pointer to a buffer to copy the keyboard data packets to}
{Size: The size of the buffer in bytes (Must be at least TKeyboardData or greater)}
{Count: The number of keyboard data packets copied to the buffer}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=KeyboardReadEx(Buffer,Size,KEYBOARD_FLAG_NONE,Count);
end;

{==============================================================================}

function KeyboardReadEx(Buffer:Pointer;Size,Flags:LongWord;var Count:LongWord):LongWord;
{Read keyboard data packets from the global keyboard buffer}
{Buffer: Pointer to a buffer to copy the keyboard data packets to}
{Size: The size of the buffer in bytes (Must be at least TKeyboardData or greater)}
{Count: The number of keyboard data packets copied to the buffer}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Offset:PtrUInt;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Buffer}
 if Buffer = nil then Exit;
 
 {Check Size}
 if Size < SizeOf(TKeyboardData) then Exit;
 
 {$IFDEF KEYBOARD_DEBUG}
 if KEYBOARD_LOG_ENABLED then KeyboardLogDebug(nil,'Attempting to read ' + IntToStr(Size) + ' bytes from keyboard');
 {$ENDIF}
 
 {Read to Buffer}
 Count:=0;
 Offset:=0;
 while Size >= SizeOf(TKeyboardData) do
  begin
   {Check Non Blocking}
   if ((Flags and KEYBOARD_FLAG_NON_BLOCK) <> 0) and (KeyboardBuffer.Count = 0) then
    begin
     if Count = 0 then Result:=ERROR_NO_MORE_ITEMS;
     Break;
    end;

   {Check Peek Buffer}
   if (Flags and KEYBOARD_FLAG_PEEK_BUFFER) <> 0 then
    begin
     Result:=ERROR_SUCCESS;
     Break;
    end; 
    
   {Wait for Keyboard Data}
   if SemaphoreWait(KeyboardBuffer.Wait) = ERROR_SUCCESS then
    begin
     {Acquire the Lock}
     if MutexLock(KeyboardBufferLock) = ERROR_SUCCESS then
      begin
       try
        {Copy Data}
        PKeyboardData(PtrUInt(Buffer) + Offset)^:=KeyboardBuffer.Buffer[KeyboardBuffer.Start];
          
        {Update Start}
        KeyboardBuffer.Start:=(KeyboardBuffer.Start + 1) mod KEYBOARD_BUFFER_SIZE;
        
        {Update Count}
        Dec(KeyboardBuffer.Count);
   
        {Update Count}
        Inc(Count);
          
        {Upate Size and Offset}
        Dec(Size,SizeOf(TKeyboardData));
        Inc(Offset,SizeOf(TKeyboardData));
       finally
        {Release the Lock}
        MutexUnlock(KeyboardBufferLock);
       end;
      end
     else
      begin
       Result:=ERROR_CAN_NOT_COMPLETE;
       Exit;
      end;
    end;
   
   {$IFDEF KEYBOARD_DEBUG}
   if KEYBOARD_LOG_ENABLED then KeyboardLogDebug(nil,'Return count=' + IntToStr(Count));
   {$ENDIF}
   
   {Return Result}
   Result:=ERROR_SUCCESS;
  end;
end;

{==============================================================================}

function KeyboardPut(Character:Word):LongWord;
{Put a character in the global keyboard buffer}
{Character: The character to write to the buffer}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 //To Do
end;

{==============================================================================}

function KeyboardWrite(Buffer:Pointer;Size,Count:LongWord):LongWord;
{Write keyboard data packets to the global keyboard buffer}
{Buffer: Pointer to a buffer to copy the keyboard data packets from}
{Size: The size of the buffer in bytes (Must be at least TKeyboardData or greater)}
{Count: The number of keyboard data packets to copy from the buffer}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Offset:PtrUInt;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Buffer}
 if Buffer = nil then Exit;
 
 {Check Size}
 if Size < SizeOf(TKeyboardData) then Exit;
 
 {Check Count}
 if Count < 1 then Exit;
 
 {$IFDEF KEYBOARD_DEBUG}
 if KEYBOARD_LOG_ENABLED then KeyboardLogDebug(nil,'Attempting to write ' + IntToStr(Size) + ' bytes to keyboard');
 {$ENDIF}
 
 {Write from Buffer}

 //To Do
end;
 
{==============================================================================}
 
function KeyboardFlush:LongWord;
{Flush the contents of the global keyboard buffer}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 //To Do
end;

{==============================================================================}

function KeyboardDeviceGet(Keyboard:PKeyboardDevice;var Character:Word):LongWord;
var
 Count:LongWord;
 Data:TKeyboardData;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Keyboard}
 if Keyboard = nil then Exit;
 if Keyboard.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check Method}
 if Assigned(Keyboard.DeviceGet) then
  begin
   {Provided Method}
   Result:=Keyboard.DeviceGet(Keyboard,Character);
  end
 else
  begin 
   {Default Method}
   Result:=KeyboardDeviceRead(Keyboard,@Data,SizeOf(TKeyboardData),Count);
   if Result = ERROR_SUCCESS then
    begin
     Character:=Data.KeyCode
    end;
  end; 
end;

{==============================================================================}

function KeyboardDeviceRead(Keyboard:PKeyboardDevice;Buffer:Pointer;Size:LongWord;var Count:LongWord):LongWord;
var
 Offset:PtrUInt;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Keyboard}
 if Keyboard = nil then Exit;
 if Keyboard.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check Buffer}
 if Buffer = nil then Exit;
 
 {Check Size}
 if Size < SizeOf(TKeyboardData) then Exit;

 {Check Method}
 if Assigned(Keyboard.DeviceRead) then
  begin
   {Provided Method}
   Result:=Keyboard.DeviceRead(Keyboard,Buffer,Size,Count);
  end
 else
  begin 
   {Default Method}
   {Check Keyboard Attached}
   if Keyboard.KeyboardState <> KEYBOARD_STATE_ATTACHED then Exit;

   {$IFDEF KEYBOARD_DEBUG}
   if KEYBOARD_LOG_ENABLED then KeyboardLogDebug(Keyboard,'Attempting to read ' + IntToStr(Size) + ' bytes from keyboard');
   {$ENDIF}
   
   {Read to Buffer}
   Count:=0;
   Offset:=0;
   while Size >= SizeOf(TKeyboardData) do
    begin
     {Check Non Blocking}
     if ((Keyboard.Device.DeviceFlags and KEYBOARD_FLAG_NON_BLOCK) <> 0) and (Keyboard.Buffer.Count = 0) then
      begin
       Break;
      end;
    
     {Wait for Keyboard Data}
     if SemaphoreWait(Keyboard.Buffer.Wait) = ERROR_SUCCESS then
      begin
       {Acquire the Lock}
       if MutexLock(Keyboard.Lock) = ERROR_SUCCESS then
        begin
         try
          {Copy Data}
          PKeyboardData(PtrUInt(Buffer) + Offset)^:=Keyboard.Buffer.Buffer[Keyboard.Buffer.Start];
          
          {Update Start}
          Keyboard.Buffer.Start:=(Keyboard.Buffer.Start + 1) mod KEYBOARD_BUFFER_SIZE;
        
          {Update Count}
          Dec(Keyboard.Buffer.Count);
  
          {Update Count}
          Inc(Count);
          
          {Upate Size and Offset}
          Dec(Size,SizeOf(TKeyboardData));
          Inc(Offset,SizeOf(TKeyboardData));
         finally
          {Release the Lock}
          MutexUnlock(Keyboard.Lock);
         end;
        end
       else
        begin
         Result:=ERROR_CAN_NOT_COMPLETE;
         Exit;
        end;
      end;
      
     {$IFDEF KEYBOARD_DEBUG}
     if KEYBOARD_LOG_ENABLED then KeyboardLogDebug(Keyboard,'Return count=' + IntToStr(Count));
     {$ENDIF}
     
     {Return Result}
     Result:=ERROR_SUCCESS;
    end;
  end;  
end;
 
{==============================================================================}

function KeyboardDeviceControl(Keyboard:PKeyboardDevice;Request:Integer;Argument1:LongWord;var Argument2:LongWord):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Keyboard}
 if Keyboard = nil then Exit;
 if Keyboard.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check Method}
 if Assigned(Keyboard.DeviceControl) then
  begin
   {Provided Method}
   Result:=Keyboard.DeviceControl(Keyboard,Request,Argument1,Argument2);
  end
 else
  begin 
   {Default Method}
   {Check Keyboard Attached}
   if Keyboard.KeyboardState <> KEYBOARD_STATE_ATTACHED then Exit;

   {Acquire the Lock}
   if MutexLock(Keyboard.Lock) = ERROR_SUCCESS then
    begin
     try
      case Request of
       KEYBOARD_CONTROL_GET_FLAG:begin
         {Get Flag}
         LongBool(Argument2):=False;
         if (Keyboard.Device.DeviceFlags and Argument1) <> 0 then
          begin
           LongBool(Argument2):=True;
           
           {Return Result}
           Result:=ERROR_SUCCESS;
          end;
        end;
       KEYBOARD_CONTROL_SET_FLAG:begin 
         {Set Flag}
         if (Argument1 and not(KEYBOARD_FLAG_MASK)) = 0 then
          begin
           Keyboard.Device.DeviceFlags:=(Keyboard.Device.DeviceFlags or Argument1);
         
           {Return Result}
           Result:=ERROR_SUCCESS;
          end; 
        end;
       KEYBOARD_CONTROL_CLEAR_FLAG:begin 
         {Clear Flag}
         if (Argument1 and not(KEYBOARD_FLAG_MASK)) = 0 then
          begin
           Keyboard.Device.DeviceFlags:=(Keyboard.Device.DeviceFlags and not(Argument1));
         
           {Return Result}
           Result:=ERROR_SUCCESS;
          end; 
        end;
       KEYBOARD_CONTROL_FLUSH_BUFFER:begin
         {Flush Buffer}
         //To Do
         
        end;
       KEYBOARD_CONTROL_GET_LED:begin
         {Get LED}
         //To Do
         
        end;
       KEYBOARD_CONTROL_SET_LED:begin
         {Set LED}
         //To Do
         
         {Return Result}
         Result:=ERROR_SUCCESS;
        end;
       KEYBOARD_CONTROL_CLEAR_LED:begin
         {Clear LED}
         //To Do
         
         {Return Result}
         Result:=ERROR_SUCCESS;
        end;
       KEYBOARD_CONTROL_GET_REPEAT_RATE:begin
         {Get Repeat Rate}
         //To Do
         
         {Return Result}
         Result:=ERROR_SUCCESS;
        end;
       KEYBOARD_CONTROL_SET_REPEAT_RATE:begin
         {Set Repeat Rate}
         //To Do
         
         {Return Result}
         Result:=ERROR_SUCCESS;
        end;
       KEYBOARD_CONTROL_GET_REPEAT_DELAY:begin
         {Get Repeat Delay}
         //To Do
         
         {Return Result}
         Result:=ERROR_SUCCESS;
        end;
       KEYBOARD_CONTROL_SET_REPEAT_DELAY:begin
         {Set Repeat Delay}
         //To Do
         
         {Return Result}
         Result:=ERROR_SUCCESS;
        end;
      end;
     finally
      {Release the Lock}
      MutexUnlock(Keyboard.Lock);
     end;
    end
   else
    begin
     Result:=ERROR_CAN_NOT_COMPLETE;
     Exit;
    end;
  end; 
end;

{==============================================================================}

function KeyboardDeviceSetState(Keyboard:PKeyboardDevice;State:LongWord):LongWord;
{Set the state of the specified keyboard and send a notification}
{Keyboard: The keyboard to set the state for}
{State: The new state to set and notify}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Keyboard}
 if Keyboard = nil then Exit;
 if Keyboard.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check State}
 if State > KEYBOARD_STATE_ATTACHED then Exit;
 
 {Check State}
 if Keyboard.KeyboardState = State then
  begin
   {Return Result}
   Result:=ERROR_SUCCESS;
  end
 else
  begin
   {Acquire the Lock}
   if MutexLock(Keyboard.Lock) = ERROR_SUCCESS then
    begin
     try 
      {Set State}
      Keyboard.KeyboardState:=State;
  
      {Notify State}
      NotifierNotify(@Keyboard.Device,KeyboardDeviceStateToNotification(State));

      {Return Result}
      Result:=ERROR_SUCCESS;
     finally
      {Release the Lock}
      MutexUnlock(Keyboard.Lock);
     end;
    end
   else
    begin
     Result:=ERROR_CAN_NOT_COMPLETE;
    end;
  end;  
end;

{==============================================================================}

function KeyboardDeviceCreate:PKeyboardDevice;
{Create a new Keyboard entry}
{Return: Pointer to new Keyboard entry or nil if keyboard could not be created}
begin
 {}
 Result:=KeyboardDeviceCreateEx(SizeOf(TKeyboardDevice));
end;

{==============================================================================}

function KeyboardDeviceCreateEx(Size:LongWord):PKeyboardDevice;
{Create a new Keyboard entry}
{Size: Size in bytes to allocate for new keyboard (Including the keyboard entry)}
{Return: Pointer to new Keyboard entry or nil if keyboard could not be created}
begin
 {}
 Result:=nil;
 
 {Check Size}
 if Size < SizeOf(TKeyboardDevice) then Exit;
 
 {Create Keyboard}
 Result:=PKeyboardDevice(DeviceCreateEx(Size));
 if Result = nil then Exit;
 
 {Update Device}
 Result.Device.DeviceBus:=DEVICE_BUS_NONE;   
 Result.Device.DeviceType:=KEYBOARD_TYPE_NONE;
 Result.Device.DeviceFlags:=KEYBOARD_FLAG_NONE;
 Result.Device.DeviceData:=nil;

 {Update Keyboard}
 Result.KeyboardId:=DEVICE_ID_ANY;
 Result.KeyboardState:=KEYBOARD_STATE_DETACHED;
 Result.KeyboardLEDs:=KEYBOARD_LED_NONE;
 Result.KeyboardRate:=KEYBOARD_REPEAT_RATE;
 Result.KeyboardDelay:=KEYBOARD_REPEAT_DELAY;
 Result.DeviceGet:=nil;
 Result.DeviceRead:=nil;
 Result.DeviceControl:=nil;
 Result.Lock:=INVALID_HANDLE_VALUE;
 Result.Buffer.Wait:=INVALID_HANDLE_VALUE;
 
 {Create Lock}
 Result.Lock:=MutexCreate;
 if Result.Lock = INVALID_HANDLE_VALUE then
  begin
   if KEYBOARD_LOG_ENABLED then KeyboardLogError(nil,'Failed to create lock for keyboard');
   KeyboardDeviceDestroy(Result);
   Result:=nil;
   Exit;
  end;
 
 {Create Buffer Semaphore}
 Result.Buffer.Wait:=SemaphoreCreate(0);
 if Result.Buffer.Wait = INVALID_HANDLE_VALUE then
  begin
   if KEYBOARD_LOG_ENABLED then KeyboardLogError(nil,'Failed to create buffer semaphore for keyboard');
   KeyboardDeviceDestroy(Result);
   Result:=nil;
   Exit;
  end;
end;

{==============================================================================}

function KeyboardDeviceDestroy(Keyboard:PKeyboardDevice):LongWord;
{Destroy an existing Keyboard entry}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Keyboard}
 if Keyboard = nil then Exit;
 if Keyboard.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check Keyboard}
 Result:=ERROR_IN_USE;
 if KeyboardDeviceCheck(Keyboard) = Keyboard then Exit;

 {Check State}
 if Keyboard.Device.DeviceState <> DEVICE_STATE_UNREGISTERED then Exit;
 
 {Destroy Buffer Semaphore}
 if Keyboard.Buffer.Wait <> INVALID_HANDLE_VALUE then
  begin
   SemaphoreDestroy(Keyboard.Buffer.Wait);
  end;
  
 {Destroy Lock}
 if Keyboard.Lock <> INVALID_HANDLE_VALUE then
  begin
   MutexDestroy(Keyboard.Lock);
  end;
 
 {Destroy Keyboard} 
 Result:=DeviceDestroy(@Keyboard.Device);
end;

{==============================================================================}

function KeyboardDeviceRegister(Keyboard:PKeyboardDevice):LongWord;
{Register a new Keyboard in the Keyboard table}
var
 KeyboardId:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Keyboard}
 if Keyboard = nil then Exit;
 if Keyboard.KeyboardId <> DEVICE_ID_ANY then Exit;
 if Keyboard.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check Keyboard}
 Result:=ERROR_ALREADY_EXISTS;
 if KeyboardDeviceCheck(Keyboard) = Keyboard then Exit;
 
 {Check State}
 if Keyboard.Device.DeviceState <> DEVICE_STATE_UNREGISTERED then Exit;
 
 {Insert Keyboard}
 if CriticalSectionLock(KeyboardTableLock) = ERROR_SUCCESS then
  begin
   try
    {Update Keyboard}
    KeyboardId:=0;
    while KeyboardDeviceFind(KeyboardId) <> nil do
     begin
      Inc(KeyboardId);
     end;
    Keyboard.KeyboardId:=KeyboardId;
    
    {Update Device}
    Keyboard.Device.DeviceName:=KEYBOARD_NAME_PREFIX + IntToStr(Keyboard.KeyboardId); 
    Keyboard.Device.DeviceClass:=DEVICE_CLASS_KEYBOARD;
    
    {Register Device}
    Result:=DeviceRegister(@Keyboard.Device);
    if Result <> ERROR_SUCCESS then
     begin
      Keyboard.KeyboardId:=DEVICE_ID_ANY;
      Exit;
     end; 
    
    {Link Keyboard}
    if KeyboardTable = nil then
     begin
      KeyboardTable:=Keyboard;
     end
    else
     begin
      Keyboard.Next:=KeyboardTable;
      KeyboardTable.Prev:=Keyboard;
      KeyboardTable:=Keyboard;
     end;
 
    {Increment Count}
    Inc(KeyboardTableCount);
    
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    CriticalSectionUnlock(KeyboardTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;

{==============================================================================}

function KeyboardDeviceDeregister(Keyboard:PKeyboardDevice):LongWord;
{Deregister a Keyboard from the Keyboard table}
var
 Prev:PKeyboardDevice;
 Next:PKeyboardDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Keyboard}
 if Keyboard = nil then Exit;
 if Keyboard.KeyboardId = DEVICE_ID_ANY then Exit;
 if Keyboard.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check Keyboard}
 Result:=ERROR_NOT_FOUND;
 if KeyboardDeviceCheck(Keyboard) <> Keyboard then Exit;
 
 {Check State}
 if Keyboard.Device.DeviceState <> DEVICE_STATE_REGISTERED then Exit;
 
 {Remove Keyboard}
 if CriticalSectionLock(KeyboardTableLock) = ERROR_SUCCESS then
  begin
   try
    {Deregister Device}
    Result:=DeviceDeregister(@Keyboard.Device);
    if Result <> ERROR_SUCCESS then Exit;
    
    {Unlink Keyboard}
    Prev:=Keyboard.Prev;
    Next:=Keyboard.Next;
    if Prev = nil then
     begin
      KeyboardTable:=Next;
      if Next <> nil then
       begin
        Next.Prev:=nil;
       end;       
     end
    else
     begin
      Prev.Next:=Next;
      if Next <> nil then
       begin
        Next.Prev:=Prev;
       end;       
     end;     
 
    {Decrement Count}
    Dec(KeyboardTableCount);
 
    {Update Keyboard}
    Keyboard.KeyboardId:=DEVICE_ID_ANY;
 
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    CriticalSectionUnlock(KeyboardTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;

{==============================================================================}

function KeyboardDeviceFind(KeyboardId:LongWord):PKeyboardDevice;
var
 Keyboard:PKeyboardDevice;
begin
 {}
 Result:=nil;
 
 {Check Id}
 if KeyboardId = DEVICE_ID_ANY then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(KeyboardTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Keyboard}
    Keyboard:=KeyboardTable;
    while Keyboard <> nil do
     begin
      {Check State}
      if Keyboard.Device.DeviceState = DEVICE_STATE_REGISTERED then
       begin
        {Check Id}
        if Keyboard.KeyboardId = KeyboardId then
         begin
          Result:=Keyboard;
          Exit;
         end;
       end;
       
      {Get Next}
      Keyboard:=Keyboard.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(KeyboardTableLock);
   end;
  end;
end;
       
{==============================================================================}

function KeyboardDeviceEnumerate(Callback:TKeyboardEnumerate;Data:Pointer):LongWord;
var
 Keyboard:PKeyboardDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Callback}
 if not Assigned(Callback) then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(KeyboardTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Keyboard}
    Keyboard:=KeyboardTable;
    while Keyboard <> nil do
     begin
      {Check State}
      if Keyboard.Device.DeviceState = DEVICE_STATE_REGISTERED then
       begin
        if Callback(Keyboard,Data) <> ERROR_SUCCESS then Exit;
       end;
       
      {Get Next}
      Keyboard:=Keyboard.Next;
     end;
     
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    CriticalSectionUnlock(KeyboardTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;

{==============================================================================}

function KeyboardDeviceNotification(Keyboard:PKeyboardDevice;Callback:TKeyboardNotification;Data:Pointer;Notification,Flags:LongWord):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Keyboard}
 if Keyboard = nil then
  begin
   Result:=DeviceNotification(nil,DEVICE_CLASS_KEYBOARD,Callback,Data,Notification,Flags);
  end
 else
  begin 
   {Check Keyboard}
   if Keyboard.Device.Signature <> DEVICE_SIGNATURE then Exit;

   Result:=DeviceNotification(@Keyboard.Device,DEVICE_CLASS_KEYBOARD,Callback,Data,Notification,Flags);
  end; 
end;

{==============================================================================}
{==============================================================================}
{RTL Console Functions}
function SysConsoleReadChar(var ACh:Char;AUserData:Pointer):Boolean;
var
 Count:LongWord;
 Data:TKeyboardData;
begin
 {}
 Result:=True;
 
 if KeyboardRead(@Data,SizeOf(TKeyboardData),Count) = ERROR_SUCCESS then
  begin
   ACh:=Chr(Data.KeyCode);
  end
 else
  begin
   ACh:=#0;
  end;  
end;

{==============================================================================}
{==============================================================================}
{USB Keyboard Functions}
function USBKeyboardDeviceGet(Keyboard:PKeyboardDevice;var Character:Word):LongWord;
var
 Count:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Keyboard}
 if Keyboard = nil then Exit;
 if Keyboard.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 Result:=USBKeyboardDeviceRead(Keyboard,@Character,1,Count);
 if (Result = ERROR_SUCCESS) and (Count = 1) then
  begin
   Result:=ERROR_SUCCESS;
  end;
end;

{==============================================================================}

function USBKeyboardDeviceRead(Keyboard:PKeyboardDevice;Buffer:Pointer;Size:LongWord;var Count:LongWord):LongWord;
var
 Offset:PtrUInt;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Keyboard}
 if Keyboard = nil then Exit;
 if Keyboard.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check Buffer}
 if Buffer = nil then Exit;
 
 {Check Size}
 if Size < SizeOf(TKeyboardData) then Exit;
 
 {Check Keyboard Attached}
 if Keyboard.KeyboardState <> KEYBOARD_STATE_ATTACHED then Exit;
  
 {$IFDEF KEYBOARD_DEBUG}
 if KEYBOARD_LOG_ENABLED then KeyboardLogDebug(Keyboard,'Attempting to read ' + IntToStr(Size) + ' bytes from keyboard');
 {$ENDIF}
 
 {Read to Buffer}
 Count:=0;
 Offset:=0;
 while Size >= SizeOf(TKeyboardData) do
  begin
   {Check Non Blocking}
   if ((Keyboard.Device.DeviceFlags and KEYBOARD_FLAG_NON_BLOCK) <> 0) and (Keyboard.Buffer.Count = 0) then
    begin
     Break;
    end;

   {Wait for Keyboard Data}
   if SemaphoreWait(Keyboard.Buffer.Wait) = ERROR_SUCCESS then
    begin
     {Acquire the Lock}
     if MutexLock(Keyboard.Lock) = ERROR_SUCCESS then
      begin
       try
        {Copy Data}
        PKeyboardData(PtrUInt(Buffer) + Offset)^:=Keyboard.Buffer.Buffer[Keyboard.Buffer.Start];
          
        {Update Start}
        Keyboard.Buffer.Start:=(Keyboard.Buffer.Start + 1) mod KEYBOARD_BUFFER_SIZE;
        
        {Update Count}
        Dec(Keyboard.Buffer.Count);
  
        {Update Count}
        Inc(Count);
          
        {Upate Size and Offset}
        Dec(Size,SizeOf(TKeyboardData));
        Inc(Offset,SizeOf(TKeyboardData));
       finally
        {Release the Lock}
        MutexUnlock(Keyboard.Lock);
       end;
      end
     else
      begin
       Result:=ERROR_CAN_NOT_COMPLETE;
       Exit;
      end;
    end;
    
   {$IFDEF KEYBOARD_DEBUG}
   if KEYBOARD_LOG_ENABLED then KeyboardLogDebug(Keyboard,'Return count=' + IntToStr(Count));
   {$ENDIF}
   
   {Return Result}
   Result:=ERROR_SUCCESS;
  end;
end;

{==============================================================================}

function USBKeyboardDeviceControl(Keyboard:PKeyboardDevice;Request:Integer;Argument1:LongWord;var Argument2:LongWord):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Keyboard}
 if Keyboard = nil then Exit;
 if Keyboard.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Check Keyboard Attached}
 if Keyboard.KeyboardState <> KEYBOARD_STATE_ATTACHED then Exit;
 
 //To Do
end;

{==============================================================================}

function USBKeyboardDriverBind(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;
{Bind the Keyboard driver to a USB device if it is suitable}
{Device: The USB device to attempt to bind to}
{Interrface: The USB interface to attempt to bind to (or nil for whole device)}
{Return: USB_STATUS_SUCCESS if completed, USB_STATUS_DEVICE_UNSUPPORTED if unsupported or another error code on failure}
var
 Status:LongWord;
 Interval:LongWord;
 Keyboard:PUSBKeyboardDevice;
 ReportEndpoint:PUSBEndpointDescriptor;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;

 {Check Device}
 if Device = nil then Exit;
      
 {$IFDEF USB_DEBUG}      
 if USB_LOG_ENABLED then USBLogDebug(Device,'Keyboard: Attempting to bind USB device (' + ': Address ' + IntToStr(Device.Address) + ')'); //To Do //Device.Manufacturer //Device.Product
 {$ENDIF}
 
 {Check Interface (Bind to interface only)}
 if Interrface = nil then
  begin
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;

 {Check for Keyboard (Must be interface specific)}
 if Device.Descriptor.bDeviceClass <> USB_CLASS_CODE_INTERFACE_SPECIFIC then
  begin
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;   
  end;

 {Check Interface (Must be HID boot protocol keyboard)}
 if (Interrface.Descriptor.bInterfaceClass <> USB_CLASS_CODE_HID) or (Interrface.Descriptor.bInterfaceSubClass <> USB_HID_SUBCLASS_BOOT) or (Interrface.Descriptor.bInterfaceProtocol <> USB_HID_BOOT_PROTOCOL_KEYBOARD) then
  begin
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;   
  end;
  
 {Check Endpoint (Must be IN interrupt)}
 ReportEndpoint:=USBDeviceFindEndpointByType(Device,Interrface,USB_DIRECTION_IN,USB_TRANSFER_TYPE_INTERRUPT);
 if ReportEndpoint = nil then
  begin
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;
 
 {Create Keyboard}
 Keyboard:=PUSBKeyboardDevice(KeyboardDeviceCreateEx(SizeOf(TUSBKeyboardDevice)));
 if Keyboard = nil then
  begin
   if USB_LOG_ENABLED then USBLogError(Device,'Keyboard: Failed to create new keyboard device');
   
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;
 
 {Update Keyboard} 
 {Device}
 Keyboard.Keyboard.Device.DeviceBus:=DEVICE_BUS_USB;
 Keyboard.Keyboard.Device.DeviceType:=KEYBOARD_TYPE_USB;
 Keyboard.Keyboard.Device.DeviceFlags:=KEYBOARD_FLAG_NONE;
 Keyboard.Keyboard.Device.DeviceData:=Device;
 {Keyboard}
 Keyboard.Keyboard.KeyboardState:=KEYBOARD_STATE_ATTACHING;
 Keyboard.Keyboard.DeviceGet:=USBKeyboardDeviceGet;
 Keyboard.Keyboard.DeviceRead:=USBKeyboardDeviceRead;
 Keyboard.Keyboard.DeviceControl:=USBKeyboardDeviceControl;
 {Driver}
 {USB}
 Keyboard.HIDInterface:=Interrface;
 Keyboard.ReportEndpoint:=ReportEndpoint;
 Keyboard.WaiterThread:=INVALID_HANDLE_VALUE;
 
 {Allocate Report Request}
 Keyboard.ReportRequest:=USBRequestAllocate(Device,ReportEndpoint,USBKeyboardReportComplete,USB_HID_BOOT_REPORT_SIZE,Keyboard);
 if Keyboard.ReportRequest = nil then
  begin
   if USB_LOG_ENABLED then USBLogError(Device,'Keyboard: Failed to allocate USB report request for keyboard');
   
   {Destroy Keyboard}
   KeyboardDeviceDestroy(@Keyboard.Keyboard);
   
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;

 {Register Keyboard} 
 if KeyboardDeviceRegister(@Keyboard.Keyboard) <> ERROR_SUCCESS then
  begin
   if USB_LOG_ENABLED then USBLogError(Device,'Keyboard: Failed to register new keyboard device');
   
   {Release Report Request}
   USBRequestRelease(Keyboard.ReportRequest);
   
   {Destroy Keyboard}
   KeyboardDeviceDestroy(@Keyboard.Keyboard);
   
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;
 
 {$IFDEF USB_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(Device,'Keyboard: Enabling HID boot protocol');
 {$ENDIF}
 
 {Set Boot Protocol}
 Status:=USBKeyboardDeviceSetProtocol(Keyboard,USB_HID_PROTOCOL_BOOT);
 if Status <> USB_STATUS_SUCCESS then
  begin
   if USB_LOG_ENABLED then USBLogError(Device,'Keyboard: Failed to enable HID boot protocol: ' + USBStatusToString(Status));
   
   {Release Report Request}
   USBRequestRelease(Keyboard.ReportRequest);
   
   {Deregister Keyboard}
   KeyboardDeviceDeregister(@Keyboard.Keyboard);
   
   {Destroy Keyboard}
   KeyboardDeviceDestroy(@Keyboard.Keyboard);
   
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;

 {$IFDEF USB_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(Device,'Keyboard: Setting idle rate');
 {$ENDIF}
 
 {Set Repeat Rate}
 Status:=USBKeyboardDeviceSetIdle(Keyboard,Keyboard.Keyboard.KeyboardRate,USB_HID_REPORTID_NONE);
 if Status <> USB_STATUS_SUCCESS then
  begin
   if USB_LOG_ENABLED then USBLogError(Device,'Keyboard: Failed to set idle rate: ' + USBStatusToString(Status));
   
   {Release Report Request}
   USBRequestRelease(Keyboard.ReportRequest);
   
   {Deregister Keyboard}
   KeyboardDeviceDeregister(@Keyboard.Keyboard);
   
   {Destroy Keyboard}
   KeyboardDeviceDestroy(@Keyboard.Keyboard);
   
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;
 
 {Check Endpoint Interval}
 if USB_KEYBOARD_POLLING_INTERVAL > 0 then
  begin
   {Check Device Speed}
   if Device.Speed = USB_SPEED_HIGH then
    begin
     {Get Interval}
     Interval:=FirstBitSet(USB_KEYBOARD_POLLING_INTERVAL * USB_UFRAMES_PER_MS) + 1;
     
     {Ensure no less than Interval} {Milliseconds = (1 shl (bInterval - 1)) div USB_UFRAMES_PER_MS}
     if ReportEndpoint.bInterval < Interval then ReportEndpoint.bInterval:=Interval;
    end
   else
    begin
     {Ensure no less than USB_KEYBOARD_POLLING_INTERVAL} {Milliseconds = bInterval div USB_FRAMES_PER_MS}
     if ReportEndpoint.bInterval < USB_KEYBOARD_POLLING_INTERVAL then ReportEndpoint.bInterval:=USB_KEYBOARD_POLLING_INTERVAL;
    end;  
  end;  
 
 {Update Interface}
 Interrface.DriverData:=Keyboard;
 
 {Update Pending}
 Inc(Keyboard.PendingCount);

 {$IFDEF USB_DEBUG} 
 if USB_LOG_ENABLED then USBLogDebug(Device,'Keyboard: Submitting report request');
 {$ENDIF}
 
 {Submit Request}
 Status:=USBRequestSubmit(Keyboard.ReportRequest);
 if Status <> USB_STATUS_SUCCESS then
  begin
   if USB_LOG_ENABLED then USBLogError(Device,'Keyboard: Failed to submit report request: ' + USBStatusToString(Status));
   
   {Update Pending}
   Dec(Keyboard.PendingCount);
   
   {Release Report Request}
   USBRequestRelease(Keyboard.ReportRequest);
   
   {Deregister Keyboard}
   KeyboardDeviceDeregister(@Keyboard.Keyboard);
   
   {Destroy Keyboard}
   KeyboardDeviceDestroy(@Keyboard.Keyboard);
   
   {Return Result}
   Result:=Status;
   Exit;
  end;  
 
 {Set State to Attached}
 if KeyboardDeviceSetState(@Keyboard.Keyboard,KEYBOARD_STATE_ATTACHED) <> ERROR_SUCCESS then Exit;
 
 {Return Result}
 Result:=USB_STATUS_SUCCESS;
end;
 
{==============================================================================}

function USBKeyboardDriverUnbind(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;
{Unbind the Keyboard driver from a USB device}
{Device: The USB device to unbind from}
{Interrface: The USB interface to unbind from (or nil for whole device)}
{Return: USB_STATUS_SUCCESS if completed or another error code on failure}
var
 Message:TMessage;
 Keyboard:PUSBKeyboardDevice;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Device}
 if Device = nil then Exit;
 
 {Check Interface}
 if Interrface = nil then Exit;
 
 {Check Driver}
 if Interrface.Driver <> USBKeyboardDriver then Exit;
 
 {$IFDEF USB_DEBUG}
 if USB_LOG_ENABLED then USBLogDebug(Device,'Keyboard: Unbinding (' + ': Address ' + IntToStr(Device.Address) + ')'); //To Do //Device.Manufacturer //Device.Product
 {$ENDIF}
 
 {Get Keyboard}
 Keyboard:=PUSBKeyboardDevice(Interrface.DriverData);
 if Keyboard = nil then Exit;
 if Keyboard.Keyboard.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Set State to Detaching}
 Result:=USB_STATUS_OPERATION_FAILED;
 if KeyboardDeviceSetState(@Keyboard.Keyboard,KEYBOARD_STATE_DETACHING) <> ERROR_SUCCESS then Exit;

 {Acquire the Lock}
 if MutexLock(Keyboard.Keyboard.Lock) <> ERROR_SUCCESS then Exit;
 
 {Cancel Report Request}
 USBRequestCancel(Keyboard.ReportRequest);
 
 {Check Pending}
 if Keyboard.PendingCount <> 0 then
  begin
   {$IFDEF USB_DEBUG}
   if USB_LOG_ENABLED then USBLogDebug(Device,'Keyboard: Waiting for ' + IntToStr(Keyboard.PendingCount) + ' pending requests to complete');
   {$ENDIF}
   
   {Wait for Pending}
 
   {Setup Waiter}
   Keyboard.WaiterThread:=GetCurrentThreadId; 
   
   {Release the Lock}
   MutexUnlock(Keyboard.Keyboard.Lock);
   
   {Wait for Message}
   ThreadReceiveMessage(Message); 
  end
 else
  begin
   {Release the Lock}
   MutexUnlock(Keyboard.Keyboard.Lock);
  end;  

 {Set State to Detached}
 if KeyboardDeviceSetState(@Keyboard.Keyboard,KEYBOARD_STATE_DETACHED) <> ERROR_SUCCESS then Exit;
 
 {Update Interface}
 Interrface.DriverData:=nil;

 {Release Report Request}
 USBRequestRelease(Keyboard.ReportRequest);
 
 {Deregister Keyboard}
 if KeyboardDeviceDeregister(@Keyboard.Keyboard) <> ERROR_SUCCESS then Exit;
 
 {Destroy Keyboard}
 KeyboardDeviceDestroy(@Keyboard.Keyboard);
 
 {Return Result}
 Result:=USB_STATUS_SUCCESS;
end;

{==============================================================================}

procedure USBKeyboardReportComplete(Request:PUSBRequest);
{Called when a USB request from a USB keyboard IN interrupt endpoint completes}
{Request: The USB request which has completed}
var
 UsageId:Byte;
 Count:Integer;
 Buffer:Pointer;
 Status:LongWord;
 Counter:Integer;
 Message:TMessage;
 UsageIndex:Integer;
 Data:PKeyboardData;
 Keyboard:PUSBKeyboardDevice;
begin
 {}
 {Check Request}
 if Request = nil then Exit;

 {Get Keyboard}
 Keyboard:=PUSBKeyboardDevice(Request.DriverData);
 if Keyboard <> nil then
  begin
   {Acquire the Lock}
   if MutexLock(Keyboard.Keyboard.Lock) = ERROR_SUCCESS then
    begin
     try
      {Update Statistics}
      Inc(Keyboard.Keyboard.ReceiveCount); 
      
      {Check State}
      if Keyboard.Keyboard.KeyboardState = KEYBOARD_STATE_DETACHING then
       begin
        {$IFDEF USB_DEBUG}
        if USB_LOG_ENABLED then USBLogDebug(Request.Device,'Keyboard: Detachment pending, setting report request status to USB_STATUS_DEVICE_DETACHED');
        {$ENDIF}
        
        {Update Request}
        Request.Status:=USB_STATUS_DEVICE_DETACHED;
       end;
       
      {Check Result}
      if (Request.Status = USB_STATUS_SUCCESS) and (Request.ActualSize = USB_HID_BOOT_REPORT_SIZE) then
       begin
        {$IFDEF USB_DEBUG}
        if USB_LOG_ENABLED then USBLogDebug(Request.Device,'Keyboard: Report received'); 
        {$ENDIF}
        
        {A report was received from the USB keyboard}
        Buffer:=Request.Data;
        Counter:=0;
        UsageIndex:=0;
     
        {Byte 0 is modifiers mask}
        if (PByte(Buffer)^ and (USB_HID_BOOT_LEFT_SHIFT or USB_HID_BOOT_RIGHT_SHIFT)) <> 0 then
         begin
          UsageIndex:=1;
         end;
    
        {Byte 1 must be ignored}    
     
        {Bytes 2 through 7 are the Usage IDs of non modifier keys currently pressed, or 0 if no key pressed}
        {Note that the keyboard sends a full report when any key is pressed or released, if
         a key is down in two consecutive reports, it should be interpreted as one keypress}
        for Count:=2 to USB_HID_BOOT_REPORT_SIZE - 1 do
         begin
          UsageId:=PByte(PtrUInt(Buffer) + PtrUInt(Count))^;
          if UsageId <> 0 then //To Do //USBKeyboardCheckUsage (Check for valid UsageIds)
           begin
            
            //To Do //USBKeyboardCheckPressed (Check for Pressed Key)
            //To Do //USBKeyboardCheckReleased (Check for Released Key)
            //To Do //USBKeyboardCheckRepeated (Check for Repeated Key)
            
            //To Do //Check for CAPS/NUM/SCROLL etc //Build LEDs mask, check if different from current, set if different
            
            {Check Recent}
            if not(USBKeyboardCheckRecent(Keyboard,UsageId)) then
             begin
              {Check Flags}
              if (Keyboard.Keyboard.Device.DeviceFlags and KEYBOARD_FLAG_DIRECT_READ) = 0 then
               begin
                {Global Buffer}
                {Acquire the Lock}
                if MutexLock(KeyboardBufferLock) = ERROR_SUCCESS then
                 begin
                  try
                   {Check Buffer}
                   if (KeyboardBuffer.Count < KEYBOARD_BUFFER_SIZE) then
                    begin
                     Data:=@KeyboardBuffer.Buffer[(KeyboardBuffer.Start + KeyboardBuffer.Count) mod KEYBOARD_BUFFER_SIZE];
                     if Data <> nil then
                      begin
                       {Get Modifiers}
                       Data.Modifiers:=0;
                       if (PByte(Buffer)^ and USB_HID_BOOT_LEFT_CTRL) <> 0 then Data.Modifiers:=Data.Modifiers or KEYBOARD_LEFT_CTRL;
                       if (PByte(Buffer)^ and USB_HID_BOOT_LEFT_SHIFT) <> 0 then Data.Modifiers:=Data.Modifiers or KEYBOARD_LEFT_SHIFT;
                       if (PByte(Buffer)^ and USB_HID_BOOT_LEFT_ALT) <> 0 then Data.Modifiers:=Data.Modifiers or KEYBOARD_LEFT_ALT;
                       if (PByte(Buffer)^ and USB_HID_BOOT_LEFT_GUI) <> 0 then Data.Modifiers:=Data.Modifiers or KEYBOARD_LEFT_GUI;
                       if (PByte(Buffer)^ and USB_HID_BOOT_RIGHT_CTRL) <> 0 then Data.Modifiers:=Data.Modifiers or KEYBOARD_RIGHT_CTRL;
                       if (PByte(Buffer)^ and USB_HID_BOOT_RIGHT_SHIFT) <> 0 then Data.Modifiers:=Data.Modifiers or KEYBOARD_RIGHT_SHIFT;
                       if (PByte(Buffer)^ and USB_HID_BOOT_RIGHT_ALT) <> 0 then Data.Modifiers:=Data.Modifiers or KEYBOARD_RIGHT_ALT;
                       if (PByte(Buffer)^ and USB_HID_BOOT_RIGHT_GUI) <> 0 then Data.Modifiers:=Data.Modifiers or KEYBOARD_RIGHT_GUI;
                   
                       {Get Scan Code}
                       Data.ScanCode:=UsageId;
                  
                       {Get Key Code}
                       Data.KeyCode:=Ord(USB_HID_BOOT_USAGE_ID[UsageId][UsageIndex]);
                   
                       {Update Count}
                       Inc(KeyboardBuffer.Count);
                   
                       {Update Count}
                       Inc(Counter);
                      end;
                    end
                   else
                    begin
                     if USB_LOG_ENABLED then USBLogError(Request.Device,'Keyboard: Buffer overflow, key discarded');
                     
                     {Update Statistics}
                     Inc(Keyboard.Keyboard.BufferOverruns); 
                    end;            
                  finally
                   {Release the Lock}
                   MutexUnlock(KeyboardBufferLock);
                  end;
                 end
                else
                 begin
                  if USB_LOG_ENABLED then USBLogError(Request.Device,'Keyboard: Failed to acquire lock on buffer');
                 end;
               end
              else
               begin              
                {Direct Buffer}
                {Check Buffer}
                if (Keyboard.Keyboard.Buffer.Count < KEYBOARD_BUFFER_SIZE) then
                 begin
                  Data:=@Keyboard.Keyboard.Buffer.Buffer[(Keyboard.Keyboard.Buffer.Start + Keyboard.Keyboard.Buffer.Count) mod KEYBOARD_BUFFER_SIZE];
                  if Data <> nil then
                   begin
                    {Get Modifiers}
                    Data.Modifiers:=0;
                    if (PByte(Buffer)^ and USB_HID_BOOT_LEFT_CTRL) <> 0 then Data.Modifiers:=Data.Modifiers or KEYBOARD_LEFT_CTRL;
                    if (PByte(Buffer)^ and USB_HID_BOOT_LEFT_SHIFT) <> 0 then Data.Modifiers:=Data.Modifiers or KEYBOARD_LEFT_SHIFT;
                    if (PByte(Buffer)^ and USB_HID_BOOT_LEFT_ALT) <> 0 then Data.Modifiers:=Data.Modifiers or KEYBOARD_LEFT_ALT;
                    if (PByte(Buffer)^ and USB_HID_BOOT_LEFT_GUI) <> 0 then Data.Modifiers:=Data.Modifiers or KEYBOARD_LEFT_GUI;
                    if (PByte(Buffer)^ and USB_HID_BOOT_RIGHT_CTRL) <> 0 then Data.Modifiers:=Data.Modifiers or KEYBOARD_RIGHT_CTRL;
                    if (PByte(Buffer)^ and USB_HID_BOOT_RIGHT_SHIFT) <> 0 then Data.Modifiers:=Data.Modifiers or KEYBOARD_RIGHT_SHIFT;
                    if (PByte(Buffer)^ and USB_HID_BOOT_RIGHT_ALT) <> 0 then Data.Modifiers:=Data.Modifiers or KEYBOARD_RIGHT_ALT;
                    if (PByte(Buffer)^ and USB_HID_BOOT_RIGHT_GUI) <> 0 then Data.Modifiers:=Data.Modifiers or KEYBOARD_RIGHT_GUI;
                  
                    {Get Scan Code}
                    Data.ScanCode:=UsageId;
                 
                    {Get Key Code}
                    Data.KeyCode:=Ord(USB_HID_BOOT_USAGE_ID[UsageId][UsageIndex]);
                  
                    {Update Count}
                    Inc(Keyboard.Keyboard.Buffer.Count);
                  
                    {Update Count}
                    Inc(Counter);
                   end;
                 end
                else
                 begin
                  if USB_LOG_ENABLED then USBLogError(Request.Device,'Keyboard: Buffer overflow, key discarded');
                  
                  {Update Statistics}
                  Inc(Keyboard.Keyboard.BufferOverruns); 
                 end;            
               end;
             end;
           end;
         end;  
     
        {Save Recent Keys}
        System.Move(PByte(PtrUInt(Buffer) + 2)^,Keyboard.RecentKeys[0],6);
        
        {$IFDEF USB_DEBUG}
        if USB_LOG_ENABLED then USBLogDebug(Request.Device,'Keyboard: Reported ' + IntToStr(Counter) + ' new keys');
        {$ENDIF}
        
        {Check Flags}
        if (Keyboard.Keyboard.Device.DeviceFlags and KEYBOARD_FLAG_DIRECT_READ) = 0 then
         begin
          {Global Buffer}
          {Signal Data Received}
          SemaphoreSignalEx(KeyboardBuffer.Wait,Counter,nil);
         end
        else
         begin
          {Direct Buffer}
          {Signal Data Received}
          SemaphoreSignalEx(Keyboard.Keyboard.Buffer.Wait,Counter,nil);
         end; 
       end
      else
       begin
        if USB_LOG_ENABLED then USBLogError(Request.Device,'Keyboard: Failed report request (Status=' + USBStatusToString(Request.Status) + ', ActualSize=' + IntToStr(Request.ActualSize) + ')'); 
        
        {Update Statistics}
        Inc(Keyboard.Keyboard.ReceiveErrors); 
       end;  
 
      {Update Pending}
      Dec(Keyboard.PendingCount);
 
      {Check State}
      if Keyboard.Keyboard.KeyboardState = KEYBOARD_STATE_DETACHING then
       begin
        {Check Pending}
        if Keyboard.PendingCount = 0 then
         begin
          {Check Waiter}
          if Keyboard.WaiterThread <> INVALID_HANDLE_VALUE then
           begin
            {$IFDEF USB_DEBUG}
            if USB_LOG_ENABLED then USBLogDebug(Request.Device,'Keyboard: Detachment pending, sending message to waiter thread (Thread=' + IntToHex(Keyboard.WaiterThread,8) + ')');
            {$ENDIF}
            
            {Send Message}
            FillChar(Message,SizeOf(TMessage),0);
            ThreadSendMessage(Keyboard.WaiterThread,Message);
            Keyboard.WaiterThread:=INVALID_HANDLE_VALUE;
           end; 
         end;
       end
      else
       begin      
        {Update Pending}
        Inc(Keyboard.PendingCount);
      
        {$IFDEF USB_DEBUG}
        if USB_LOG_ENABLED then USBLogDebug(Request.Device,'Keyboard: Resubmitting report request');
        {$ENDIF}

        {Resubmit Request}
        Status:=USBRequestSubmit(Request);
        if Status <> USB_STATUS_SUCCESS then
         begin
          if USB_LOG_ENABLED then USBLogError(Request.Device,'Keyboard: Failed to resubmit report request: ' + USBStatusToString(Status));
   
          {Update Pending}
          Dec(Keyboard.PendingCount);
         end;
       end;  
     finally
      {Release the Lock}
      MutexUnlock(Keyboard.Keyboard.Lock);
     end;
    end
   else
    begin
     if USB_LOG_ENABLED then USBLogError(Request.Device,'Keyboard: Failed to acquire lock');
    end;
  end
 else
  begin
   if USB_LOG_ENABLED then USBLogError(Request.Device,'Keyboard: Report request invalid');
  end;    
end;

{==============================================================================}
{==============================================================================}
{Keyboard Helper Functions}
function KeyboardGetCount:LongWord; inline;
{Get the current keyboard count}
begin
 {}
 Result:=KeyboardTableCount;
end;

{==============================================================================}

function KeyboardDeviceCheck(Keyboard:PKeyboardDevice):PKeyboardDevice;
{Check if the supplied Keyboard is in the keyboard table}
var
 Current:PKeyboardDevice;
begin
 {}
 Result:=nil;
 
 {Check Keyboard}
 if Keyboard = nil then Exit;
 if Keyboard.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(KeyboardTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Keyboard}
    Current:=KeyboardTable;
    while Current <> nil do
     begin
      {Check Keyboard}
      if Current = Keyboard then
       begin
        Result:=Keyboard;
        Exit;
       end;
      
      {Get Next}
      Current:=Current.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(KeyboardTableLock);
   end;
  end;
end;

{==============================================================================}

function KeyboardDeviceTypeToString(KeyboardType:LongWord):String;
begin
 {}
 Result:='KEYBOARD_TYPE_UNKNOWN';
 
 if KeyboardType <= KEYBOARD_TYPE_MAX then
  begin
   Result:=KEYBOARD_TYPE_NAMES[KeyboardType];
  end;
end;

{==============================================================================}

function KeyboardDeviceStateToString(KeyboardState:LongWord):String;
begin
 {}
 Result:='KEYBOARD_STATE_UNKNOWN';
 
 if KeyboardState <= KEYBOARD_STATE_MAX then
  begin
   Result:=KEYBOARD_STATE_NAMES[KeyboardState];
  end;
end;

{==============================================================================}

function KeyboardDeviceStateToNotification(State:LongWord):LongWord;
{Convert a Keyboard state value into the notification code for device notifications}
begin
 {}
 Result:=DEVICE_NOTIFICATION_NONE;
 
 {Check State}
 case State of
  KEYBOARD_STATE_DETACHED:Result:=DEVICE_NOTIFICATION_DETACH;
  KEYBOARD_STATE_DETACHING:Result:=DEVICE_NOTIFICATION_DETACHING;
  KEYBOARD_STATE_ATTACHING:Result:=DEVICE_NOTIFICATION_ATTACHING;
  KEYBOARD_STATE_ATTACHED:Result:=DEVICE_NOTIFICATION_ATTACH;
 end;
end;

{==============================================================================}

procedure KeyboardLog(Level:LongWord;Keyboard:PKeyboardDevice;const AText:String);
var
 WorkBuffer:String;
begin
 {}
 {Check Level}
 if Level < KEYBOARD_DEFAULT_LOG_LEVEL then Exit;
 
 WorkBuffer:='';
 {Check Level}
 if Level = KEYBOARD_LOG_LEVEL_DEBUG then
  begin
   WorkBuffer:=WorkBuffer + '[DEBUG] ';
  end
 else if Level = KEYBOARD_LOG_LEVEL_ERROR then
  begin
   WorkBuffer:=WorkBuffer + '[ERROR] ';
  end;
 
 {Add Prefix}
 WorkBuffer:=WorkBuffer + 'Keyboard: ';
 
 {Check Keyboard}
 if Keyboard <> nil then
  begin
   WorkBuffer:=WorkBuffer + KEYBOARD_NAME_PREFIX + IntToStr(Keyboard.KeyboardId) + ': ';
  end;
  
 {Output Logging} 
 LoggingOutputEx(LOGGING_FACILITY_KEYBOARD,LogLevelToLoggingSeverity(Level),'Keyboard',WorkBuffer + AText);
end;

{==============================================================================}

procedure KeyboardLogInfo(Keyboard:PKeyboardDevice;const AText:String);
begin
 {}
 KeyboardLog(KEYBOARD_LOG_LEVEL_INFO,Keyboard,AText);
end;

{==============================================================================}

procedure KeyboardLogError(Keyboard:PKeyboardDevice;const AText:String);
begin
 {}
 KeyboardLog(KEYBOARD_LOG_LEVEL_ERROR,Keyboard,AText);
end;

{==============================================================================}

procedure KeyboardLogDebug(Keyboard:PKeyboardDevice;const AText:String);
begin
 {}
 KeyboardLog(KEYBOARD_LOG_LEVEL_DEBUG,Keyboard,AText);
end;
    
{==============================================================================}
{==============================================================================}
{USB Keyboard Helper Functions}
function USBKeyboardCheckRecent(Keyboard:PUSBKeyboardDevice;UsageId:Byte):Boolean;
var
 Count:Integer;
begin
 {}
 Result:=False;
 
 {Check Keyboard}
 if Keyboard = nil then Exit;
 
 for Count:=0 to 5 do {6 bytes of Keyboard data}
  begin
   if Keyboard.RecentKeys[Count] = UsageId then
    begin
     Result:=True;
     Exit;
    end;
  end;
end;

{==============================================================================}

function USBKeyboardDeviceSetLEDs(Keyboard:PUSBKeyboardDevice;LEDs:Byte):LongWord;
var
 Data:Byte;
 Device:PUSBDevice;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Keyboard}
 if Keyboard = nil then Exit;
 
 {Check Interface}
 if Keyboard.HIDInterface = nil then Exit;
 
 {Get Device}
 Device:=PUSBDevice(Keyboard.Keyboard.Device.DeviceData);
 if Device = nil then Exit;
 
 {Get Data}
 Data:=0;
 if (LEDs and KEYBOARD_LED_NUMLOCK) <> 0 then Data:=Data or USB_HID_BOOT_NUMLOCK_LED;
 if (LEDs and KEYBOARD_LED_CAPSLOCK) <> 0 then Data:=Data or USB_HID_BOOT_CAPSLOCK_LED;
 if (LEDs and KEYBOARD_LED_SCROLLLOCK) <> 0 then Data:=Data or USB_HID_BOOT_SCROLLLOCK_LED;
 if (LEDs and KEYBOARD_LED_COMPOSE) <> 0 then Data:=Data or USB_HID_BOOT_COMPOSE_LED;
 if (LEDs and KEYBOARD_LED_KANA) <> 0 then Data:=Data or USB_HID_BOOT_KANA_LED;
 
 {Set Report}
 Result:=USBControlRequest(Device,nil,USB_HID_REQUEST_SET_REPORT,USB_BMREQUESTTYPE_TYPE_CLASS or USB_BMREQUESTTYPE_DIR_OUT or USB_BMREQUESTTYPE_RECIPIENT_INTERFACE,(USB_HID_REPORT_OUTPUT shl 8) or USB_HID_REPORTID_NONE,Keyboard.HIDInterface.Descriptor.bInterfaceNumber,@Data,SizeOf(Byte));
end;

{==============================================================================}

function USBKeyboardDeviceSetIdle(Keyboard:PUSBKeyboardDevice;Duration,ReportId:Byte):LongWord;
var
 Device:PUSBDevice;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Keyboard}
 if Keyboard = nil then Exit;
 
 {Check Interface}
 if Keyboard.HIDInterface = nil then Exit;
 
 {Get Device}
 Device:=PUSBDevice(Keyboard.Keyboard.Device.DeviceData);
 if Device = nil then Exit;
 
 {Set Idle}
 Result:=USBControlRequest(Device,nil,USB_HID_REQUEST_SET_IDLE,USB_BMREQUESTTYPE_TYPE_CLASS or USB_BMREQUESTTYPE_DIR_OUT or USB_BMREQUESTTYPE_RECIPIENT_INTERFACE,(Duration shl 8) or ReportId,Keyboard.HIDInterface.Descriptor.bInterfaceNumber,nil,0);
end;

{==============================================================================}

function USBKeyboardDeviceSetProtocol(Keyboard:PUSBKeyboardDevice;Protocol:Byte):LongWord;
var
 Device:PUSBDevice;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;
 
 {Check Keyboard}
 if Keyboard = nil then Exit;
 
 {Check Interface}
 if Keyboard.HIDInterface = nil then Exit;
 
 {Get Device}
 Device:=PUSBDevice(Keyboard.Keyboard.Device.DeviceData);
 if Device = nil then Exit;
 
 {Set Protocol}
 Result:=USBControlRequest(Device,nil,USB_HID_REQUEST_SET_PROTOCOL,USB_BMREQUESTTYPE_TYPE_CLASS or USB_BMREQUESTTYPE_DIR_OUT or USB_BMREQUESTTYPE_RECIPIENT_INTERFACE,Protocol,Keyboard.HIDInterface.Descriptor.bInterfaceNumber,nil,0);
end;

{==============================================================================}
{==============================================================================}

initialization
 KeyboardInit;

{==============================================================================}
 
finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
