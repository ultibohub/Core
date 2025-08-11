{
Ultibo USB Keyboard driver unit.

Copyright (C) 2022 - SoftOz Pty Ltd.

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

 U-Boot - \common\usb_kbd.c

References
==========

 USB HID Device Class Definition 1_11.pdf

   http://www.usb.org/developers/hidpage/HID1_11.pdf

 USB HID Usage Tables 1_12v2.pdf

   http://www.usb.org/developers/hidpage/Hut1_12v2.pdf

USB Keyboard Devices
====================

 The USB keyboard driver in this unit uses HID Boot Protocol only and has been replaced by
 the HIDKeyboard unit which provides complete HID Report Protocol support for USB keyboards.

 It is retained here for legacy uses and backwards compatibility only.

 To use this driver in place of the default HID Keyboard driver set the following
 configuration variables in your application during system initialization.

 HID_REGISTER_KEYBOARD := False;
 USB_KEYBOARD_REGISTER_DRIVER := True;

 This driver does not recognize devices that do not report themselves as boot keyboards.

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

{$IFNDEF FPC_DOTTEDUNITS}
unit USBKeyboard;
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
  Core.USB,
  Core.Keyboard,
  Core.Keymap,
  System.SysUtils;
{$ELSE FPC_DOTTEDUNITS}
uses
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  Devices,
  USB,
  Keyboard,
  Keymap,
  SysUtils;
{$ENDIF FPC_DOTTEDUNITS}

{==============================================================================}
{Global definitions}
{$INCLUDE ..\core\GlobalDefines.inc}

{==============================================================================}
const
 {USB Keyboard specific constants}
 USBKEYBOARD_DRIVER_NAME = 'USB Keyboard Driver (HID boot protocol)'; {Name of USB keyboard driver}

 USBKEYBOARD_KEYBOARD_DESCRIPTION = 'USB HID Keyboard'; {Description of USB keyboard device}

 {HID Interface Subclass types (See USB HID v1.11 specification)}
 USB_HID_SUBCLASS_NONE           = 0;     {Section 4.2}
 USB_HID_SUBCLASS_BOOT           = 1;     {Section 4.2}

 {HID Interface Protocol types (See USB HID v1.11 specification)}
 USB_HID_BOOT_PROTOCOL_NONE      = 0;     {Section 4.3}
 USB_HID_BOOT_PROTOCOL_KEYBOARD  = 1;     {Section 4.3}
 USB_HID_BOOT_PROTOCOL_MOUSE     = 2;     {Section 4.3}

 {HID Class Descriptor Types (See USB HID v1.11 specification)}
 USB_HID_DESCRIPTOR_TYPE_HID                  = $21;  {Section 7.1}
 USB_HID_DESCRIPTOR_TYPE_REPORT               = $22;  {Section 7.1}
 USB_HID_DESCRIPTOR_TYPE_PHYSICAL_DESCRIPTOR  = $23;  {Section 7.1}

 {HID Request types}
 USB_HID_REQUEST_GET_REPORT      = $01;   {Section 7.2}
 USB_HID_REQUEST_GET_IDLE        = $02;   {Section 7.2}
 USB_HID_REQUEST_GET_PROTOCOL    = $03;   {Section 7.2}
 USB_HID_REQUEST_SET_REPORT      = $09;   {Section 7.2}
 USB_HID_REQUEST_SET_IDLE        = $0A;   {Section 7.2}
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

 USB_HID_BOOT_LEDMASK = USB_HID_BOOT_NUMLOCK_LED or USB_HID_BOOT_CAPSLOCK_LED or USB_HID_BOOT_SCROLLLOCK_LED or USB_HID_BOOT_COMPOSE_LED or USB_HID_BOOT_KANA_LED;

 {HID Boot Protocol Output data}
 USB_HID_BOOT_OUTPUT_SIZE  = 1;            {Appendix B of HID Device Class Definition 1.11}

 {Map of HID Boot Protocol keyboard Usage IDs to Characters}
 {Entries not filled in are left 0 and are interpreted as unrecognized input and ignored (Section 10 of the Universal Serial Bus HID Usage Tables v1.12)}
 {Note: These are no longer used, see the Keymap unit for scan code to key code translation tables}
 (*USB_HID_BOOT_USAGE_ID:array[0..255] of array[0..1] of Char = (
    {0}   (#0, #0),       {Reserved (no event indicated)}
    {1}   (#0, #0),       {Keyboard ErrorRollOver}
    {2}   (#0, #0),       {Keyboard POSTFail}
    {3}   (#0, #0),       {Keyboard ErrorUndefined}
    {4}   ('a', 'A'),     {Keyboard a or A}
    {5}   ('b', 'B'),     {Keyboard b or B}
    {6}   ('c', 'C'),     {Keyboard c or C}
    {7}   ('d', 'D'),     {Keyboard d or D}
    {8}   ('e', 'E'),     {Keyboard e or E}
    {9}   ('f', 'F'),     {Keyboard f or F}
    {10}  ('g', 'G'),     {Keyboard g or G}
    {11}  ('h', 'H'),     {Keyboard h or H}
    {12}  ('i', 'I'),     {Keyboard i or I}
    {13}  ('j', 'J'),     {Keyboard j or J}
    {14}  ('k', 'K'),     {Keyboard k or K}
    {15}  ('l', 'L'),     {Keyboard l or L}
    {16}  ('m', 'M'),     {Keyboard m or M}
    {17}  ('n', 'N'),     {Keyboard n or N}
    {18}  ('o', 'O'),     {Keyboard o or O}
    {19}  ('p', 'P'),     {Keyboard p or P}
    {20}  ('q', 'Q'),     {Keyboard q or Q}
    {21}  ('r', 'R'),     {Keyboard r or R}
    {22}  ('s', 'S'),     {Keyboard s or S}
    {23}  ('t', 'T'),     {Keyboard t or T}
    {24}  ('u', 'U'),     {Keyboard u or U}
    {25}  ('v', 'V'),     {Keyboard v or V}
    {26}  ('w', 'W'),     {Keyboard w or W}
    {27}  ('x', 'X'),     {Keyboard x or X}
    {28}  ('y', 'Y'),     {Keyboard y or Y}
    {29}  ('z', 'Z'),     {Keyboard z or Z}
    {30}  ('1', '!'),     {Keyboard 1 or !}
    {31}  ('2', '@'),     {Keyboard 2 or @}
    {32}  ('3', '#'),     {Keyboard 3 or #}
    {33}  ('4', '$'),     {Keyboard 4 or $}
    {34}  ('5', '%'),     {Keyboard 5 or %}
    {35}  ('6', '^'),     {Keyboard 6 or ^}
    {36}  ('7', '&'),     {Keyboard 7 or &}
    {37}  ('8', '*'),     {Keyboard 8 or *}
    {38}  ('9', '('),     {Keyboard 9 or (}
    {39}  ('0', ')'),     {Keyboard 0 or )}
    {40}  (#13, #13),     {Keyboard Enter)}
    {41}  (#27, #27),     {Keyboard Escape}
    {42}  (#8, #8),       {Keyboard Backspace}
    {43}  (#9, #9),       {Keyboard Tab}
    {44}  (' ', ' '),     {Keyboard Spacebar}
    {45}  ('-', '_'),     {Keyboard - or _}
    {46}  ('=', '+'),     {Keyboard = or +}
    {47}  ('[', '{'),     {Keyboard [ or Left Brace}
    {48}  (']', '}'),     {Keyboard ] or Right Brace}
    {49}  ('\', '|'),     {Keyboard \ or |}
    {50}  ('#', '~'),     {Keyboard Non-US # and ~}
    {51}  (';', ':'),     {Keyboard ; or :}
    {52}  ('''', '"'),    {Keyboard ' or "}
    {53}  ('`', '~'),     {Keyboard ` or ~}
    {54}  (',', '<'),     {Keyboard , or <}
    {55}  ('.', '>'),     {Keyboard . or >}
    {56}  ('/', '?'),     {Keyboard / or ?}
    {57}  (#0, #0),       {Keyboard Caps Lock}
    {58}  (#0, #0),       {Keyboard F1}
    {59}  (#0, #0),       {Keyboard F2}
    {60}  (#0, #0),       {Keyboard F3}
    {61}  (#0, #0),       {Keyboard F4}
    {62}  (#0, #0),       {Keyboard F5}
    {63}  (#0, #0),       {Keyboard F6}
    {64}  (#0, #0),       {Keyboard F7}
    {65}  (#0, #0),       {Keyboard F8}
    {66}  (#0, #0),       {Keyboard F9}
    {67}  (#0, #0),       {Keyboard F10}
    {68}  (#0, #0),       {Keyboard F11}
    {69}  (#0, #0),       {Keyboard F12}
    {70}  (#0, #0),       {Keyboard Print Screen}
    {71}  (#0, #0),       {Keyboard Scroll Lock}
    {72}  (#0, #0),       {Keyboard Pause}
    {73}  (#0, #0),       {Keyboard Insert}
    {74}  (#0, #0),       {Keyboard Home}
    {75}  (#0, #0),       {Keyboard PageUp}
    {76}  (#127, #127),   {Keyboard Delete}
    {77}  (#0, #0),       {Keyboard End}
    {78}  (#0, #0),       {Keyboard PageDn}
    {79}  (#0, #0),       {Keyboard Right Arrow}
    {80}  (#0, #0),       {Keyboard Left Arrow}
    {81}  (#0, #0),       {Keyboard Down Arrow}
    {82}  (#0, #0),       {Keyboard Up Arrow}
    {83}  (#0, #0),       {Keyboard Num Lock}
    {84}  ('/', '/'),     {Keypad /}
    {85}  ('*', '*'),     {Keypad *}
    {86}  ('-', '-'),     {Keypad -}
    {87}  ('+', '+'),     {Keypad +}
    {88}  (#13,#13),      {Keypad Enter}
    {89}  ('1', '1'),     {Keypad 1 and End}
    {90}  ('2', '2'),     {Keypad 2 and Down Arrow}
    {91}  ('3', '3'),     {Keypad 3 and PageDn}
    {92}  ('4', '4'),     {Keypad 4 and Left Arrow}
    {93}  ('5', '5'),     {Keypad 5}
    {94}  ('6', '6'),     {Keypad 6 and Right Arrow}
    {95}  ('7', '7'),     {Keypad 7 and Home}
    {96}  ('8', '8'),     {Keypad 8 and Up Arrow}
    {97}  ('9', '9'),     {Keypad 9 and PageUp}
    {98}  ('0', '0'),     {Keypad 0 and Insert}
    {99}  ('.', #127),    {Keypad . and Delete}
    {100} ('\', '|'),     {Keyboard Non-US \ and |}
    {101} (#0, #0),       {Keyboard Application}
    {102} (#0, #0),       {Keyboard Power}
    {103} ('=', '='),     {Keypad =}
    {104} (#0, #0),       {Keyboard F13}
    {105} (#0, #0),       {Keyboard F14}
    {106} (#0, #0),       {Keyboard F15}
    {107} (#0, #0),       {Keyboard F16}
    {108} (#0, #0),       {Keyboard F17}
    {109} (#0, #0),       {Keyboard F18}
    {110} (#0, #0),       {Keyboard F19}
    {111} (#0, #0),       {Keyboard F20}
    {112} (#0, #0),       {Keyboard F21}
    {113} (#0, #0),       {Keyboard F22}
    {114} (#0, #0),       {Keyboard F23}
    {115} (#0, #0),       {Keyboard F24}
    {116} (#0, #0),       {Keyboard Execute}
    {117} (#0, #0),       {Keyboard Help}
    {118} (#0, #0),       {Keyboard Menu}
    {119} (#0, #0),       {Keyboard Select}
    {120} (#0, #0),       {Keyboard Stop}
    {121} (#0, #0),       {Keyboard Again}
    {122} (#0, #0),       {Keyboard Undo}
    {123} (#0, #0),       {Keyboard Cut}
    {124} (#0, #0),       {Keyboard Copy}
    {125} (#0, #0),       {Keyboard Paste}
    {126} (#0, #0),       {Keyboard Find}
    {127} (#0, #0),       {Keyboard Mute}
    {128} (#0, #0),       {Keyboard Volume Up}
    {129} (#0, #0),       {Keyboard Volume Down}
    {130} (#0, #0),       {Keyboard Locking Caps Lock}
    {131} (#0, #0),       {Keyboard Locking Num Lock}
    {132} (#0, #0),       {Keyboard Locking Scroll Lock}
    {133} (',', ','),     {Keypad Comma}
    {134} (#0, #0),       {Keypad Equal Sign}
    {135} (#0, #0),       {Keyboard International1}
    {136} (#0, #0),       {Keyboard International2}
    {137} (#0, #0),       {Keyboard International3}
    {138} (#0, #0),       {Keyboard International4}
    {139} (#0, #0),       {Keyboard International5}
    {140} (#0, #0),       {Keyboard International6}
    {141} (#0, #0),       {Keyboard International7}
    {142} (#0, #0),       {Keyboard International8}
    {143} (#0, #0),       {Keyboard International9}
    {144} (#0, #0),       {Keyboard LANG1}
    {145} (#0, #0),       {Keyboard LANG2}
    {146} (#0, #0),       {Keyboard LANG3}
    {147} (#0, #0),       {Keyboard LANG4}
    {148} (#0, #0),       {Keyboard LANG5}
    {149} (#0, #0),       {Keyboard LANG6}
    {150} (#0, #0),       {Keyboard LANG7}
    {151} (#0, #0),       {Keyboard LANG8}
    {152} (#0, #0),       {Keyboard LANG9}
    {153} (#0, #0),       {Keyboard Alternate Erase}
    {154} (#0, #0),       {Keyboard SysReq/Attention}
    {155} (#0, #0),       {Keyboard Cancel}
    {156} (#0, #0),       {Keyboard Clear}
    {157} (#0, #0),       {Keyboard Prior}
    {158} (#0, #0),       {Keyboard Return}
    {159} (#0, #0),       {Keyboard Separator}
    {160} (#0, #0),       {Keyboard Out}
    {161} (#0, #0),       {Keyboard Oper}
    {162} (#0, #0),       {Keyboard Clear/Again}
    {163} (#0, #0),       {Keyboard CrSel/Props}
    {164} (#0, #0),       {Keyboard ExSel}
    {165} (#0, #0),       {Reserved}
    {166} (#0, #0),       {Reserved}
    {167} (#0, #0),       {Reserved}
    {168} (#0, #0),       {Reserved}
    {169} (#0, #0),       {Reserved}
    {170} (#0, #0),       {Reserved}
    {171} (#0, #0),       {Reserved}
    {172} (#0, #0),       {Reserved}
    {173} (#0, #0),       {Reserved}
    {174} (#0, #0),       {Reserved}
    {175} (#0, #0),       {Reserved}
    {176} (#0, #0),       {Keypad 00}
    {177} (#0, #0),       {Keypad 000}
    {178} (#0, #0),       {Thousands Separator}
    {179} (#0, #0),       {Decimal Separator}
    {180} (#0, #0),       {Currency Unit}
    {181} (#0, #0),       {Currenct Sub-unit}
    {182} (#0, #0),       {Keypad (}
    {183} (#0, #0),       {Keypad )}
    {184} (#0, #0),       {Keypad Left Brace}
    {185} (#0, #0),       {Keypad Right Brace}
    {186} (#0, #0),       {Keypad Tab}
    {187} (#0, #0),       {Keypad Backspace}
    {188} (#0, #0),       {Keypad A}
    {189} (#0, #0),       {Keypad B}
    {190} (#0, #0),       {Keypad C}
    {191} (#0, #0),       {Keypad D}
    {192} (#0, #0),       {Keypad E}
    {193} (#0, #0),       {Keypad F}
    {194} (#0, #0),       {Keypad XOR}
    {195} (#0, #0),       {Keypad ^}
    {196} (#0, #0),       {Keypad %}
    {197} (#0, #0),       {Keypad <}
    {198} (#0, #0),       {Keypad >}
    {199} (#0, #0),       {Keypad &}
    {200} (#0, #0),       {Keypad &&}
    {201} (#0, #0),       {Keypad |}
    {202} (#0, #0),       {Keypad ||}
    {203} (#0, #0),       {Keypad :}
    {204} (#0, #0),       {Keypad #}
    {205} (#0, #0),       {Keypad Space}
    {206} (#0, #0),       {Keypad @}
    {207} (#0, #0),       {Keypad !}
    {208} (#0, #0),       {Keypad Memory Store}
    {209} (#0, #0),       {Keypad Memory Recall}
    {210} (#0, #0),       {Keypad Memory Clear}
    {211} (#0, #0),       {Keypad Memory Add}
    {212} (#0, #0),       {Keypad Memory Subtract}
    {213} (#0, #0),       {Keypad Memory Multiply}
    {214} (#0, #0),       {Keypad Memory Divide}
    {215} (#0, #0),       {Keypad +/-}
    {216} (#0, #0),       {Keypad Clear}
    {217} (#0, #0),       {Keypad Clear Entry}
    {218} (#0, #0),       {Keypad Binary}
    {219} (#0, #0),       {Keypad Octal}
    {220} (#0, #0),       {Keypad Decimal}
    {221} (#0, #0),       {Keypad Hexadecimal}
    {222} (#0, #0),       {Reserved}
    {223} (#0, #0),       {Reserved}
    {224} (#0, #0),       {Keyboard LeftControl}
    {225} (#0, #0),       {Keyboard LeftShift}
    {226} (#0, #0),       {Keyboard LeftAlt}
    {227} (#0, #0),       {Keyboard Left GUI}
    {228} (#0, #0),       {Keyboard RightControl}
    {229} (#0, #0),       {Keyboard RightShift}
    {230} (#0, #0),       {Keyboard RightAlt}
    {231} (#0, #0),       {Keyboard Right GUI}
    {232} (#0, #0),       {Reserved}
    {233} (#0, #0),       {Reserved}
    {234} (#0, #0),       {Reserved}
    {235} (#0, #0),       {Reserved}
    {236} (#0, #0),       {Reserved}
    {237} (#0, #0),       {Reserved}
    {238} (#0, #0),       {Reserved}
    {239} (#0, #0),       {Reserved}
    {240} (#0, #0),       {Reserved}
    {241} (#0, #0),       {Reserved}
    {242} (#0, #0),       {Reserved}
    {243} (#0, #0),       {Reserved}
    {244} (#0, #0),       {Reserved}
    {245} (#0, #0),       {Reserved}
    {246} (#0, #0),       {Reserved}
    {247} (#0, #0),       {Reserved}
    {248} (#0, #0),       {Reserved}
    {249} (#0, #0),       {Reserved}
    {250} (#0, #0),       {Reserved}
    {251} (#0, #0),       {Reserved}
    {252} (#0, #0),       {Reserved}
    {253} (#0, #0),       {Reserved}
    {254} (#0, #0),       {Reserved}
    {255} (#0, #0)        {Reserved (256 to 65535 Reserved)}
  );*)

 USB_HID_BOOT_USAGE_NUMLOCK    = SCAN_CODE_NUMLOCK;    {83}
 USB_HID_BOOT_USAGE_CAPSLOCK   = SCAN_CODE_CAPSLOCK;   {57}
 USB_HID_BOOT_USAGE_SCROLLLOCK = SCAN_CODE_SCROLLLOCK; {71}

{==============================================================================}
type
 {USB Keyboard specific types}
 {USB HID Descriptor}
 PUSBHIDDescriptor = ^TUSBHIDDescriptor;
 TUSBHIDDescriptor = packed record
  bLength:Byte;
  bDescriptorType:Byte;
  bcdHID:Word;
  bCountryCode:Byte;
  bNumDescriptors:Byte;
  bHIDDescriptorType:Byte;
  wHIDDescriptorLength:Word;
  {Note: Up to two optional bHIDDescriptorType/wHIDDescriptorLength pairs after the Report descriptor details}
 end;

 {USB Boot Keyboard Report}
 PUSBKeyboardReport = ^TUSBKeyboardReport;
 TUSBKeyboardReport = array[0..7] of Byte;

 {USB Keyboard Device}
 PUSBKeyboardDevice = ^TUSBKeyboardDevice;
 TUSBKeyboardDevice = record
  {Keyboard Properties}
  Keyboard:TKeyboardDevice;
  {USB Properties}
  HIDInterface:PUSBInterface;            {USB HID Keyboard Interface}
  ReportRequest:PUSBRequest;             {USB request for keyboard report data}
  ReportEndpoint:PUSBEndpointDescriptor; {USB Keyboard Interrupt IN Endpoint}
  HIDDescriptor:PUSBHIDDescriptor;       {USB HID Descriptor for keyboard}
  ReportDescriptor:Pointer;              {USB HID Report Descriptor for keyboard}
  LastCode:Word;                         {The scan code of the last key pressed}
  LastCount:LongWord;                    {The repeat count of the last key pressed}
  LastReport:TUSBKeyboardReport;         {The last keyboard report received}
  PendingCount:LongWord;                 {Number of USB requests pending for this keyboard}
  WaiterThread:TThreadId;                {Thread waiting for pending requests to complete (for keyboard detachment)}
 end;

{==============================================================================}
{var}
 {USB Keyboard specific variables}

{==============================================================================}
{Initialization Functions}
procedure USBKeyboardInit;

{==============================================================================}
{USB Keyboard Functions}
function USBKeyboardDeviceRead(Keyboard:PKeyboardDevice;Buffer:Pointer;Size:LongWord;var Count:LongWord):LongWord;
function USBKeyboardDeviceControl(Keyboard:PKeyboardDevice;Request:Integer;Argument1:PtrUInt;var Argument2:PtrUInt):LongWord;

function USBKeyboardDriverBind(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;
function USBKeyboardDriverUnbind(Device:PUSBDevice;Interrface:PUSBInterface):LongWord;

procedure USBKeyboardReportWorker(Request:PUSBRequest);
procedure USBKeyboardReportComplete(Request:PUSBRequest);

{==============================================================================}
{USB Helper Functions}
function USBKeyboardCheckDevice(Device:PUSBDevice):Boolean;

function USBKeyboardCheckPressed(Keyboard:PUSBKeyboardDevice;ScanCode:Byte):Boolean;
function USBKeyboardCheckRepeated(Keyboard:PUSBKeyboardDevice;ScanCode:Byte):Boolean;
function USBKeyboardCheckReleased(Keyboard:PUSBKeyboardDevice;Report:PUSBKeyboardReport;ScanCode:Byte):Boolean;

function USBKeyboardDeviceSetLEDs(Keyboard:PUSBKeyboardDevice;LEDs:Byte):LongWord;
function USBKeyboardDeviceSetIdle(Keyboard:PUSBKeyboardDevice;Duration,ReportId:Byte):LongWord;
function USBKeyboardDeviceSetProtocol(Keyboard:PUSBKeyboardDevice;Protocol:Byte):LongWord;

function USBKeyboardDeviceGetHIDDescriptor(Keyboard:PUSBKeyboardDevice;Descriptor:PUSBHIDDescriptor):LongWord;
function USBKeyboardDeviceGetReportDescriptor(Keyboard:PUSBKeyboardDevice;Descriptor:Pointer;Size:LongWord):LongWord;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {USB Keyboard specific variables}
 USBKeyboardInitialized:Boolean;

 USBKeyboardDriver:PUSBDriver;  {USB Keyboard Driver interface (Set by KeyboardInit)}

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure USBKeyboardInit;
{Initialize the USB keyboard driver}

{Note: Called only during system startup}
var
 Status:LongWord;
begin
 {}
 {Check Initialized}
 if USBKeyboardInitialized then Exit;

 {Create USB Keyboard Driver}
 if USB_KEYBOARD_REGISTER_DRIVER then
  begin
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

       {Destroy Driver}
       USBDriverDestroy(USBKeyboardDriver);
      end;
    end
   else
    begin
     if KEYBOARD_LOG_ENABLED then KeyboardLogError(nil,'Failed to create USB keyboard driver');
    end;
  end;

 USBKeyboardInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{USB Keyboard Functions}
function USBKeyboardDeviceRead(Keyboard:PKeyboardDevice;Buffer:Pointer;Size:LongWord;var Count:LongWord):LongWord;
{Implementation of KeyboardDeviceRead API for USB Keyboard}
{Note: Not intended to be called directly by applications, use KeyboardDeviceRead instead}
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
     if Count = 0 then Result:=ERROR_NO_MORE_ITEMS;
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

        {Update Size and Offset}
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
    end
   else
    begin
     Result:=ERROR_CAN_NOT_COMPLETE;
     Exit;
    end;

   {Return Result}
   Result:=ERROR_SUCCESS;
  end;

 {$IFDEF KEYBOARD_DEBUG}
 if KEYBOARD_LOG_ENABLED then KeyboardLogDebug(Keyboard,'Return count=' + IntToStr(Count));
 {$ENDIF}
end;

{==============================================================================}

function USBKeyboardDeviceControl(Keyboard:PKeyboardDevice;Request:Integer;Argument1:PtrUInt;var Argument2:PtrUInt):LongWord;
{Implementation of KeyboardDeviceControl API for USB Keyboard}
{Note: Not intended to be called directly by applications, use KeyboardDeviceControl instead}
var
 Status:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Keyboard}
 if Keyboard = nil then Exit;
 if Keyboard.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Keyboard Attached}
 if Keyboard.KeyboardState <> KEYBOARD_STATE_ATTACHED then Exit;

 {Acquire the Lock}
 if MutexLock(Keyboard.Lock) = ERROR_SUCCESS then
  begin
   try
    case Request of
     KEYBOARD_CONTROL_GET_FLAG:begin
       {Get Flag}
       Argument2:=Ord(False);
       if (Keyboard.Device.DeviceFlags and Argument1) <> 0 then
        begin
         Argument2:=Ord(True);

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
       while Keyboard.Buffer.Count > 0 do
        begin
         {Wait for Data (Should not Block)}
         if SemaphoreWait(Keyboard.Buffer.Wait) = ERROR_SUCCESS then
          begin
           {Update Start}
           Keyboard.Buffer.Start:=(Keyboard.Buffer.Start + 1) mod KEYBOARD_BUFFER_SIZE;

           {Update Count}
           Dec(Keyboard.Buffer.Count);
          end
         else
          begin
           Result:=ERROR_CAN_NOT_COMPLETE;
           Exit;
          end;
        end;

       {Return Result}
       Result:=ERROR_SUCCESS;
      end;
     KEYBOARD_CONTROL_GET_LED:begin
       {Get LED}
       Argument2:=Ord(False);
       if (Keyboard.KeyboardLEDs and Argument1) <> 0 then
        begin
         Argument2:=Ord(True);

         {Return Result}
         Result:=ERROR_SUCCESS;
        end;
      end;
     KEYBOARD_CONTROL_SET_LED:begin
       {Set LED}
       if (Argument1 and not(KEYBOARD_LED_MASK)) = 0 then
        begin
         Keyboard.KeyboardLEDs:=(Keyboard.KeyboardLEDs or Argument1);

         {Set LEDs}
         Status:=USBKeyboardDeviceSetLEDs(PUSBKeyboardDevice(Keyboard),Keyboard.KeyboardLEDs);
         if Status <> USB_STATUS_SUCCESS then
          begin
           Result:=ERROR_OPERATION_FAILED;
           Exit;
          end;

         {Return Result}
         Result:=ERROR_SUCCESS;
        end;
      end;
     KEYBOARD_CONTROL_CLEAR_LED:begin
       {Clear LED}
       if (Argument1 and not(KEYBOARD_LED_MASK)) = 0 then
        begin
         Keyboard.KeyboardLEDs:=(Keyboard.KeyboardLEDs and not(Argument1));

         {Set LEDs}
         Status:=USBKeyboardDeviceSetLEDs(PUSBKeyboardDevice(Keyboard),Keyboard.KeyboardLEDs);
         if Status <> USB_STATUS_SUCCESS then
          begin
           Result:=ERROR_OPERATION_FAILED;
           Exit;
          end;

         {Return Result}
         Result:=ERROR_SUCCESS;
        end;
      end;
     KEYBOARD_CONTROL_GET_REPEAT_RATE:begin
       {Get Repeat Rate}
       Argument2:=Keyboard.KeyboardRate;

       {Return Result}
       Result:=ERROR_SUCCESS;
      end;
     KEYBOARD_CONTROL_SET_REPEAT_RATE:begin
       {Set Repeat Rate}
       Keyboard.KeyboardRate:=Argument1;

       {Set Idle}
       Status:=USBKeyboardDeviceSetIdle(PUSBKeyboardDevice(Keyboard),Keyboard.KeyboardRate,USB_HID_REPORTID_NONE);
       if Status <> USB_STATUS_SUCCESS then
        begin
         Result:=ERROR_OPERATION_FAILED;
         Exit;
        end;

       {Return Result}
       Result:=ERROR_SUCCESS;
      end;
     KEYBOARD_CONTROL_GET_REPEAT_DELAY:begin
       {Get Repeat Delay}
       Argument2:=Keyboard.KeyboardDelay;

       {Return Result}
       Result:=ERROR_SUCCESS;
      end;
     KEYBOARD_CONTROL_SET_REPEAT_DELAY:begin
       {Set Repeat Delay}
       Keyboard.KeyboardDelay:=Argument1;

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

 {$IF DEFINED(KEYBOARD_DEBUG) or DEFINED(USB_DEBUG)}
 if USB_LOG_ENABLED then USBLogDebug(Device,'Keyboard: Attempting to bind USB device (Manufacturer=' + Device.Manufacturer + ' Product=' + Device.Product + ' Address=' + IntToStr(Device.Address) + ')');
 {$ENDIF}

 {Check Interface (Bind to interface only)}
 if Interrface = nil then
  begin
   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;

 {Check Device}
 if not USBKeyboardCheckDevice(Device) then
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
 Keyboard.Keyboard.Device.DeviceFlags:=Keyboard.Keyboard.Device.DeviceFlags; {Don't override defaults (was KEYBOARD_FLAG_NONE)}
 Keyboard.Keyboard.Device.DeviceData:=Device;
 Keyboard.Keyboard.Device.DeviceDescription:=USBKEYBOARD_KEYBOARD_DESCRIPTION;
 {Keyboard}
 Keyboard.Keyboard.KeyboardState:=KEYBOARD_STATE_ATTACHING;
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

 {$IF DEFINED(KEYBOARD_DEBUG) or DEFINED(USB_DEBUG)}
 if USB_LOG_ENABLED then USBLogDebug(Device,'Keyboard: Reading HID report descriptors');
 {$ENDIF}

 {Get HID Descriptor}
 Keyboard.HIDDescriptor:=USBBufferAllocate(Device,SizeOf(TUSBHIDDescriptor));
 if Keyboard.HIDDescriptor <> nil then
  begin
   Status:=USBKeyboardDeviceGetHIDDescriptor(Keyboard,Keyboard.HIDDescriptor);
   if Status <> USB_STATUS_SUCCESS then
    begin
     if USB_LOG_ENABLED then USBLogError(Device,'Keyboard: Failed to read HID descriptor: ' + USBStatusToString(Status));

     {Don't fail the bind}
    end
   else
    begin
     if (Keyboard.HIDDescriptor.bDescriptorType = USB_HID_DESCRIPTOR_TYPE_HID) and (Keyboard.HIDDescriptor.bHIDDescriptorType = USB_HID_DESCRIPTOR_TYPE_REPORT) then
      begin
       {Get Report Descriptor}
       Keyboard.ReportDescriptor:=USBBufferAllocate(Device,Keyboard.HIDDescriptor.wHIDDescriptorLength);
       if Keyboard.ReportDescriptor <> nil then
        begin
         Status:=USBKeyboardDeviceGetReportDescriptor(Keyboard,Keyboard.ReportDescriptor,Keyboard.HIDDescriptor.wHIDDescriptorLength);
         if Status <> USB_STATUS_SUCCESS then
          begin
           if USB_LOG_ENABLED then USBLogError(Device,'Keyboard: Failed to read HID report descriptor: ' + USBStatusToString(Status));

           {Don't fail the bind}
         {$IF DEFINED(KEYBOARD_DEBUG) or DEFINED(USB_DEBUG)}
          end
         else
          begin
           if USB_LOG_ENABLED then USBLogDebug(Device,'Keyboard: Read ' + IntToStr(Keyboard.HIDDescriptor.wHIDDescriptorLength) + ' byte HID report descriptor');
         {$ENDIF}
          end;
        end;
      end;
    end;
  end;

 {$IF DEFINED(KEYBOARD_DEBUG) or DEFINED(USB_DEBUG)}
 if USB_LOG_ENABLED then USBLogDebug(Device,'Keyboard: Enabling HID boot protocol');
 {$ENDIF}

 {Set Boot Protocol}
 Status:=USBKeyboardDeviceSetProtocol(Keyboard,USB_HID_PROTOCOL_BOOT);
 if Status <> USB_STATUS_SUCCESS then
  begin
   if USB_LOG_ENABLED then USBLogError(Device,'Keyboard: Failed to enable HID boot protocol: ' + USBStatusToString(Status));

   {Release Report Request}
   USBRequestRelease(Keyboard.ReportRequest);

   {Release HID Descriptor}
   USBBufferRelease(Keyboard.HIDDescriptor);

   {Release Report Descriptor}
   USBBufferRelease(Keyboard.ReportDescriptor);

   {Deregister Keyboard}
   KeyboardDeviceDeregister(@Keyboard.Keyboard);

   {Destroy Keyboard}
   KeyboardDeviceDestroy(@Keyboard.Keyboard);

   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;

 {$IF DEFINED(KEYBOARD_DEBUG) or DEFINED(USB_DEBUG)}
 if USB_LOG_ENABLED then USBLogDebug(Device,'Keyboard: Setting idle rate');
 {$ENDIF}

 {Set Repeat Rate}
 Status:=USBKeyboardDeviceSetIdle(Keyboard,Keyboard.Keyboard.KeyboardRate,USB_HID_REPORTID_NONE);
 if Status <> USB_STATUS_SUCCESS then
  begin
   if USB_LOG_ENABLED then USBLogError(Device,'Keyboard: Failed to set idle rate: ' + USBStatusToString(Status));

   {Release Report Request}
   USBRequestRelease(Keyboard.ReportRequest);

   {Release HID Descriptor}
   USBBufferRelease(Keyboard.HIDDescriptor);

   {Release Report Descriptor}
   USBBufferRelease(Keyboard.ReportDescriptor);

   {Deregister Keyboard}
   KeyboardDeviceDeregister(@Keyboard.Keyboard);

   {Destroy Keyboard}
   KeyboardDeviceDestroy(@Keyboard.Keyboard);

   {Return Result}
   Result:=USB_STATUS_DEVICE_UNSUPPORTED;
   Exit;
  end;

 {Set LEDs}
 Status:=USBKeyboardDeviceSetLEDs(Keyboard,Keyboard.Keyboard.KeyboardLEDs);
 if Status <> USB_STATUS_SUCCESS then
  begin
   if USB_LOG_ENABLED then USBLogError(Device,'Keyboard: Failed to set LEDs: ' + USBStatusToString(Status));

   {Release Report Request}
   USBRequestRelease(Keyboard.ReportRequest);

   {Release HID Descriptor}
   USBBufferRelease(Keyboard.HIDDescriptor);

   {Release Report Descriptor}
   USBBufferRelease(Keyboard.ReportDescriptor);

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

 {$IF DEFINED(KEYBOARD_DEBUG) or DEFINED(USB_DEBUG)}
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

   {Release HID Descriptor}
   USBBufferRelease(Keyboard.HIDDescriptor);

   {Release Report Descriptor}
   USBBufferRelease(Keyboard.ReportDescriptor);

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

 {$IF DEFINED(KEYBOARD_DEBUG) or DEFINED(USB_DEBUG)}
 if USB_LOG_ENABLED then USBLogDebug(Device,'Keyboard: Unbinding USB device (Manufacturer=' + Device.Manufacturer + ' Product=' + Device.Product + ' Address=' + IntToStr(Device.Address) + ')');
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
   {$IF DEFINED(KEYBOARD_DEBUG) or DEFINED(USB_DEBUG)}
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

 {Release HID Descriptor}
 USBBufferRelease(Keyboard.HIDDescriptor);

 {Release Report Descriptor}
 USBBufferRelease(Keyboard.ReportDescriptor);

 {Deregister Keyboard}
 if KeyboardDeviceDeregister(@Keyboard.Keyboard) <> ERROR_SUCCESS then Exit;

 {Destroy Keyboard}
 KeyboardDeviceDestroy(@Keyboard.Keyboard);

 {Return Result}
 Result:=USB_STATUS_SUCCESS;
end;

{==============================================================================}

procedure USBKeyboardReportWorker(Request:PUSBRequest);
{Called (by a Worker thread) to process a completed USB request from a USB keyboard IN interrupt endpoint}
{Request: The USB request which has completed}
var
 Index:Byte;
 Saved:Byte;
 Count:Integer;
 LEDs:LongWord;
 KeyCode:Word;
 ScanCode:Byte;
 Status:LongWord;
 Counter:Integer;
 Message:TMessage;
 Modifiers:LongWord;
 Data:TKeyboardData;
 Keymap:TKeymapHandle;
 Report:PUSBKeyboardReport;
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
        {$IF DEFINED(KEYBOARD_DEBUG) or DEFINED(USB_DEBUG)}
        if USB_LOG_ENABLED then USBLogDebug(Request.Device,'Keyboard: Detachment pending, setting report request status to USB_STATUS_DEVICE_DETACHED');
        {$ENDIF}

        {Update Request}
        Request.Status:=USB_STATUS_DEVICE_DETACHED;
       end;

      {Check Result}
      if (Request.Status = USB_STATUS_SUCCESS) and (Request.ActualSize = USB_HID_BOOT_REPORT_SIZE) then
       begin
        {$IF DEFINED(KEYBOARD_DEBUG) or DEFINED(USB_DEBUG)}
        if USB_LOG_ENABLED then USBLogDebug(Request.Device,'Keyboard: Report received');
        {$ENDIF}

        {A report was received from the USB keyboard}
        Report:=Request.Data;
        Counter:=0;
        Keymap:=KeymapGetDefault;
        LEDs:=Keyboard.Keyboard.KeyboardLEDs;

        {Clear Keyboard Data}
        FillChar(Data,SizeOf(TKeyboardData),0);

        {Byte 0 is modifiers mask}
        {Get Modifiers}
        Modifiers:=0;

        {LED Modifiers}
        if Keyboard.Keyboard.KeyboardLEDs <> KEYBOARD_LED_NONE then
         begin
          if (Keyboard.Keyboard.KeyboardLEDs and KEYBOARD_LED_NUMLOCK) <> 0 then Modifiers:=Modifiers or KEYBOARD_NUM_LOCK;
          if (Keyboard.Keyboard.KeyboardLEDs and KEYBOARD_LED_CAPSLOCK) <> 0 then Modifiers:=Modifiers or KEYBOARD_CAPS_LOCK;
          if (Keyboard.Keyboard.KeyboardLEDs and KEYBOARD_LED_SCROLLLOCK) <> 0 then Modifiers:=Modifiers or KEYBOARD_SCROLL_LOCK;
          if (Keyboard.Keyboard.KeyboardLEDs and KEYBOARD_LED_COMPOSE) <> 0 then Modifiers:=Modifiers or KEYBOARD_COMPOSE;
          if (Keyboard.Keyboard.KeyboardLEDs and KEYBOARD_LED_KANA) <> 0 then Modifiers:=Modifiers or KEYBOARD_KANA;
         end;

        {Report Modifiers}
        if Report[0] <> 0 then
         begin
          if (Report[0] and USB_HID_BOOT_LEFT_CTRL) <> 0 then Modifiers:=Modifiers or KEYBOARD_LEFT_CTRL;
          if (Report[0] and USB_HID_BOOT_LEFT_SHIFT) <> 0 then Modifiers:=Modifiers or KEYBOARD_LEFT_SHIFT;
          if (Report[0] and USB_HID_BOOT_LEFT_ALT) <> 0 then Modifiers:=Modifiers or KEYBOARD_LEFT_ALT;
          if (Report[0] and USB_HID_BOOT_LEFT_GUI) <> 0 then Modifiers:=Modifiers or KEYBOARD_LEFT_GUI;
          if (Report[0] and USB_HID_BOOT_RIGHT_CTRL) <> 0 then Modifiers:=Modifiers or KEYBOARD_RIGHT_CTRL;
          if (Report[0] and USB_HID_BOOT_RIGHT_SHIFT) <> 0 then Modifiers:=Modifiers or KEYBOARD_RIGHT_SHIFT;
          if (Report[0] and USB_HID_BOOT_RIGHT_ALT) <> 0 then Modifiers:=Modifiers or KEYBOARD_RIGHT_ALT;
          if (Report[0] and USB_HID_BOOT_RIGHT_GUI) <> 0 then Modifiers:=Modifiers or KEYBOARD_RIGHT_GUI;
         end;

        {Get Keymap Index}
        Index:=KEYMAP_INDEX_NORMAL;

        {Check for Shift}
        if (Modifiers and (KEYBOARD_LEFT_SHIFT or KEYBOARD_RIGHT_SHIFT)) <> 0 then
         begin
          Index:=KEYMAP_INDEX_SHIFT;

          {Check Shift behavior}
          if KEYBOARD_SHIFT_IS_CAPS_LOCK_OFF then
           begin
            {Check for Caps Lock}
            if (Modifiers and (KEYBOARD_CAPS_LOCK)) <> 0 then
             begin
              {Update LEDs}
              Keyboard.Keyboard.KeyboardLEDs:=Keyboard.Keyboard.KeyboardLEDs and not(KEYBOARD_LED_CAPSLOCK);
             end;
           end;
         end;

        {Check AltGr behavior}
        if KeymapCheckFlag(Keymap,KEYMAP_FLAG_ALTGR) then
         begin
          if not(KEYBOARD_CTRL_ALT_IS_ALTGR) then
           begin
            {Check for Right Alt}
            if (Modifiers and (KEYBOARD_RIGHT_ALT)) <> 0 then
             begin
              if Index <> KEYMAP_INDEX_SHIFT then Index:=KEYMAP_INDEX_ALTGR else Index:=KEYMAP_INDEX_SHIFT_ALTGR;
             end;
           end
          else
           begin
            {Check for Ctrl and Alt}
            if ((Modifiers and (KEYBOARD_LEFT_CTRL or KEYBOARD_RIGHT_CTRL)) <> 0) and ((Modifiers and (KEYBOARD_LEFT_ALT or KEYBOARD_RIGHT_ALT)) <> 0) then
             begin
              if Index <> KEYMAP_INDEX_SHIFT then Index:=KEYMAP_INDEX_ALTGR else Index:=KEYMAP_INDEX_SHIFT_ALTGR;
             end;
           end;

          {Check Keymap Index}
          if (Index = KEYMAP_INDEX_ALTGR) or (Index = KEYMAP_INDEX_SHIFT_ALTGR) then
           begin
            Modifiers:=Modifiers or KEYBOARD_ALTGR;
           end;
         end;

        {Save Keymap Index}
        Saved:=Index;

        {Byte 1 must be ignored}

        {Bytes 2 through 7 are the Usage IDs of non modifier keys currently pressed, or 0 if no key pressed}
        {Note that the keyboard sends a full report when any key is pressed or released, if a key is down in
         two consecutive reports, it should be interpreted as one keypress unless the repeat delay has elapsed}

        {Check for Keys Pressed}
        for Count:=2 to USB_HID_BOOT_REPORT_SIZE - 1 do
         begin
          {Load Keymap Index}
          Index:=Saved;

          {Get Scan Code}
          ScanCode:=Report[Count];

          {Ignore SCAN_CODE_NONE to SCAN_CODE_ERROR}
          if ScanCode > SCAN_CODE_ERROR then
           begin
            {Check for Caps Lock Shifted Key}
            if KeymapCheckCapskey(Keymap,ScanCode) then
             begin
              {Check for Caps Lock}
              if (Modifiers and (KEYBOARD_CAPS_LOCK)) <> 0 then
               begin
                {Modify Normal and Shift}
                if Index = KEYMAP_INDEX_NORMAL then
                 begin
                  Index:=KEYMAP_INDEX_SHIFT;
                 end
                else if Index = KEYMAP_INDEX_SHIFT then
                 begin
                  Index:=KEYMAP_INDEX_NORMAL;
                 end
                {Modify AltGr and Shift}
                else if Index = KEYMAP_INDEX_ALTGR then
                 begin
                  Index:=KEYMAP_INDEX_SHIFT_ALTGR;
                 end
                else if Index = KEYMAP_INDEX_SHIFT_ALTGR then
                 begin
                  Index:=KEYMAP_INDEX_ALTGR;
                 end;
               end;
             end;

            {Check for Numeric Keypad Key}
            if (ScanCode >= SCAN_CODE_KEYPAD_FIRST) and (ScanCode <= SCAN_CODE_KEYPAD_LAST) then
             begin
              {Check for Num Lock}
              if (Modifiers and (KEYBOARD_NUM_LOCK)) <> 0 then
               begin
                {Check for Shift}
                if (Modifiers and (KEYBOARD_LEFT_SHIFT or KEYBOARD_RIGHT_SHIFT)) <> 0 then
                 begin
                  Index:=KEYMAP_INDEX_NORMAL;
                 end
                else
                 begin
                  Index:=KEYMAP_INDEX_SHIFT;
                 end;
               end
              else
               begin
                Index:=KEYMAP_INDEX_NORMAL;
               end;
             end;

            {Check Pressed}
            if USBKeyboardCheckPressed(Keyboard,ScanCode) then
             begin
              {$IF DEFINED(KEYBOARD_DEBUG) or DEFINED(USB_DEBUG)}
              if USB_LOG_ENABLED then USBLogDebug(Request.Device,'Keyboard: Key Pressed (ScanCode=' + IntToStr(ScanCode) + ' Modifiers=' + IntToHex(Modifiers,8) + ' Index=' + IntToStr(Index) + ')');
              {$ENDIF}

              {Check for NumLock}
              if ScanCode = USB_HID_BOOT_USAGE_NUMLOCK then
               begin
                {Update LEDs}
                Keyboard.Keyboard.KeyboardLEDs:=Keyboard.Keyboard.KeyboardLEDs xor KEYBOARD_LED_NUMLOCK;
               end
              else if ScanCode = USB_HID_BOOT_USAGE_CAPSLOCK then
               begin
                {Update LEDs}
                Keyboard.Keyboard.KeyboardLEDs:=Keyboard.Keyboard.KeyboardLEDs xor KEYBOARD_LED_CAPSLOCK;
               end
              else if ScanCode = USB_HID_BOOT_USAGE_SCROLLLOCK then
               begin
                {Update LEDs}
                Keyboard.Keyboard.KeyboardLEDs:=Keyboard.Keyboard.KeyboardLEDs xor KEYBOARD_LED_SCROLLLOCK;
               end
              else
               begin
                {Update Last}
                Keyboard.LastCode:=ScanCode;
                Keyboard.LastCount:=0;

                {Check for Deadkey}
                if (Keyboard.Keyboard.Code = SCAN_CODE_NONE) and KeymapCheckDeadkey(Keymap,ScanCode,Index) then
                 begin
                  {$IF DEFINED(KEYBOARD_DEBUG) or DEFINED(USB_DEBUG)}
                  if USB_LOG_ENABLED then USBLogDebug(Request.Device,'Keyboard: Deadkey Pressed (ScanCode=' + IntToStr(ScanCode) + ' Modifiers=' + IntToHex(Modifiers,8) + ' Index=' + IntToStr(Index) + ')');
                  {$ENDIF}

                  {Update Deadkey}
                  Keyboard.Keyboard.Code:=ScanCode;
                  Keyboard.Keyboard.Index:=Index;
                  Keyboard.Keyboard.Modifiers:=Modifiers;

                  {Get Data}
                  Data.Modifiers:=Modifiers or KEYBOARD_KEYDOWN or KEYBOARD_DEADKEY;
                  Data.ScanCode:=ScanCode;
                  Data.KeyCode:=KeymapGetKeyCode(Keymap,ScanCode,Index);
                  Data.CharCode:=KeymapGetCharCode(Keymap,Data.KeyCode);
                  Data.CharUnicode:=KeymapGetCharUnicode(Keymap,Data.KeyCode);

                  {Insert Data}
                  if KeyboardInsertData(@Keyboard.Keyboard,@Data,True) = ERROR_SUCCESS then
                   begin
                    {Update Count}
                    Inc(Counter);
                   end;
                 end
                else
                 begin
                  {Check Deadkey}
                  KeyCode:=KEY_CODE_NONE;
                  if Keyboard.Keyboard.Code <> SCAN_CODE_NONE then
                   begin
                    {Resolve Deadkey}
                    if not KeymapResolveDeadkey(Keymap,Keyboard.Keyboard.Code,ScanCode,Keyboard.Keyboard.Index,Index,KeyCode) then
                     begin
                      {Get Data}
                      Data.Modifiers:=Keyboard.Keyboard.Modifiers or KEYBOARD_KEYDOWN;
                      Data.ScanCode:=Keyboard.Keyboard.Code;
                      Data.KeyCode:=KeymapGetKeyCode(Keymap,Keyboard.Keyboard.Code,Keyboard.Keyboard.Index);
                      Data.CharCode:=KeymapGetCharCode(Keymap,Data.KeyCode);
                      Data.CharUnicode:=KeymapGetCharUnicode(Keymap,Data.KeyCode);

                      {Insert Data}
                      if KeyboardInsertData(@Keyboard.Keyboard,@Data,True) = ERROR_SUCCESS then
                       begin
                        {Update Count}
                        Inc(Counter);
                       end;
                     end;
                   end;

                  {Reset Deadkey}
                  Keyboard.Keyboard.Code:=SCAN_CODE_NONE;

                  {Get Data}
                  Data.Modifiers:=Modifiers or KEYBOARD_KEYDOWN;
                  Data.ScanCode:=ScanCode;
                  Data.KeyCode:=KeymapGetKeyCode(Keymap,ScanCode,Index);
                  if KeyCode <> KEY_CODE_NONE then Data.KeyCode:=KeyCode;
                  Data.CharCode:=KeymapGetCharCode(Keymap,Data.KeyCode);
                  Data.CharUnicode:=KeymapGetCharUnicode(Keymap,Data.KeyCode);

                  {$IF DEFINED(KEYBOARD_DEBUG) or DEFINED(USB_DEBUG)}
                  if USB_LOG_ENABLED then USBLogDebug(Request.Device,'Keyboard: Key Pressed (KeyCode=' + IntToHex(Data.KeyCode,4) + ' CharCode=' + IntToHex(Byte(Data.CharCode),2) + ' CharUnicode=' + IntToHex(Word(Data.CharUnicode),4) + ')');
                  {$ENDIF}

                  {Insert Data}
                  if KeyboardInsertData(@Keyboard.Keyboard,@Data,True) = ERROR_SUCCESS then
                   begin
                    {Update Count}
                    Inc(Counter);
                   end;
                 end;
               end;
             end
            else
             begin
              {Check Repeated}
              if USBKeyboardCheckRepeated(Keyboard,ScanCode) then
               begin
                {$IF DEFINED(KEYBOARD_DEBUG) or DEFINED(USB_DEBUG)}
                if USB_LOG_ENABLED then USBLogDebug(Request.Device,'Keyboard: Key Repeated (ScanCode=' + IntToStr(ScanCode) + ' Modifiers=' + IntToHex(Modifiers,8) + ' Index=' + IntToStr(Index) + ')');
                {$ENDIF}

                {Get Data}
                Data.Modifiers:=Modifiers or KEYBOARD_KEYREPEAT;
                Data.ScanCode:=ScanCode;
                Data.KeyCode:=KeymapGetKeyCode(Keymap,ScanCode,Index);
                Data.CharCode:=KeymapGetCharCode(Keymap,Data.KeyCode);
                Data.CharUnicode:=KeymapGetCharUnicode(Keymap,Data.KeyCode);

                {Insert Data}
                if KeyboardInsertData(@Keyboard.Keyboard,@Data,True) = ERROR_SUCCESS then
                 begin
                  {Update Count}
                  Inc(Counter);
                 end;
               end;
             end;
           end;
         end;

        {Check for Keys Released}
        for Count:=2 to USB_HID_BOOT_REPORT_SIZE - 1 do
         begin
          {Load Keymap Index}
          Index:=Saved;

          {Get Scan Code}
          ScanCode:=Keyboard.LastReport[Count];

          {Ignore SCAN_CODE_NONE to SCAN_CODE_ERROR}
          if ScanCode > SCAN_CODE_ERROR then
           begin
            {Check for Caps Lock Shifted Key}
            if KeymapCheckCapskey(Keymap,ScanCode) then
             begin
              {Check for Caps Lock}
              if (Modifiers and (KEYBOARD_CAPS_LOCK)) <> 0 then
               begin
                {Modify Normal and Shift}
                if Index = KEYMAP_INDEX_NORMAL then
                 begin
                  Index:=KEYMAP_INDEX_SHIFT;
                 end
                else if Index = KEYMAP_INDEX_SHIFT then
                 begin
                  Index:=KEYMAP_INDEX_NORMAL;
                 end
                {Modify AltGr and Shift}
                else if Index = KEYMAP_INDEX_ALTGR then
                 begin
                  Index:=KEYMAP_INDEX_SHIFT_ALTGR;
                 end
                else if Index = KEYMAP_INDEX_SHIFT_ALTGR then
                 begin
                  Index:=KEYMAP_INDEX_ALTGR;
                 end;
               end;
             end;

            {Check for Numeric Keypad Key}
            if (ScanCode >= SCAN_CODE_KEYPAD_FIRST) and (ScanCode <= SCAN_CODE_KEYPAD_LAST) then
             begin
              {Check for Num Lock}
              if (Modifiers and (KEYBOARD_NUM_LOCK)) <> 0 then
               begin
                {Check for Shift}
                if (Modifiers and (KEYBOARD_LEFT_SHIFT or KEYBOARD_RIGHT_SHIFT)) <> 0 then
                 begin
                  Index:=KEYMAP_INDEX_NORMAL;
                 end
                else
                 begin
                  Index:=KEYMAP_INDEX_SHIFT;
                 end;
               end
              else
               begin
                Index:=KEYMAP_INDEX_NORMAL;
               end;
             end;

            {Check Released}
            if USBKeyboardCheckReleased(Keyboard,Report,ScanCode) then
             begin
              {$IF DEFINED(KEYBOARD_DEBUG) or DEFINED(USB_DEBUG)}
              if USB_LOG_ENABLED then USBLogDebug(Request.Device,'Keyboard: Key Released (ScanCode=' + IntToStr(ScanCode) + ' Modifiers=' + IntToHex(Modifiers,8) + ' Index=' + IntToStr(Index)+ ')');
              {$ENDIF}

              {Reset Last}
              Keyboard.LastCode:=SCAN_CODE_NONE;
              Keyboard.LastCount:=0;

              {Get Data}
              Data.Modifiers:=Modifiers or KEYBOARD_KEYUP;
              Data.ScanCode:=ScanCode;
              Data.KeyCode:=KeymapGetKeyCode(Keymap,ScanCode,Index);
              Data.CharCode:=KeymapGetCharCode(Keymap,Data.KeyCode);
              Data.CharUnicode:=KeymapGetCharUnicode(Keymap,Data.KeyCode);

              {Insert Data}
              if KeyboardInsertData(@Keyboard.Keyboard,@Data,True) = ERROR_SUCCESS then
               begin
                {Update Count}
                Inc(Counter);
               end;
             end;
           end;
         end;

        {Save Last Report}
        System.Move(Report[0],Keyboard.LastReport[0],SizeOf(TUSBKeyboardReport));

        {$IF DEFINED(KEYBOARD_DEBUG) or DEFINED(USB_DEBUG)}
        if USB_LOG_ENABLED then USBLogDebug(Request.Device,'Keyboard: Reported ' + IntToStr(Counter) + ' new keys');
        {$ENDIF}

        {Check LEDs}
        if LEDs <> Keyboard.Keyboard.KeyboardLEDs then
         begin
          {Update LEDs}
          Status:=USBKeyboardDeviceSetLEDs(Keyboard,Keyboard.Keyboard.KeyboardLEDs);
          if Status <> USB_STATUS_SUCCESS then
           begin
            if USB_LOG_ENABLED then USBLogError(Request.Device,'Keyboard: Failed to set LEDs: ' + USBStatusToString(Status));
           end;
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
            {$IF DEFINED(KEYBOARD_DEBUG) or DEFINED(USB_DEBUG)}
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

        {$IF DEFINED(KEYBOARD_DEBUG) or DEFINED(USB_DEBUG)}
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

procedure USBKeyboardReportComplete(Request:PUSBRequest);
{Called when a USB request from a USB keyboard IN interrupt endpoint completes}
{Request: The USB request which has completed}
{Note: Request is passed to worker thread for processing to prevent blocking the USB completion}
begin
 {}
 {Check Request}
 if Request = nil then Exit;

 WorkerSchedule(0,TWorkerTask(USBKeyboardReportWorker),Request,nil)
end;

{==============================================================================}
{==============================================================================}
{USB Keyboard Helper Functions}
function USBKeyboardCheckDevice(Device:PUSBDevice):Boolean;
{Check if the supplied USB device is suitable for detection as a HID Keyboard Device}
{Device: The USB device to check}
{Return: True if the device is suitable or False if it is not}
begin
 {}
 Result:=False;

 {Check Device}
 if Device = nil then Exit;

 {Check Class}
 case Device.Descriptor.bDeviceClass of
  USB_CLASS_CODE_HUB:Result:=False;
 else
  Result:=True;
 end;
end;

{==============================================================================}

function USBKeyboardCheckPressed(Keyboard:PUSBKeyboardDevice;ScanCode:Byte):Boolean;
{Check if the passed scan code has been pressed (True if not pressed in last report)}
{Keyboard: The USB keyboard device to check for}
{ScanCode: The keyboard scan code to check}

{Note: Caller must hold the keyboard lock}
var
 Count:Integer;
begin
 {}
 Result:=True;

 {Check Keyboard}
 if Keyboard = nil then Exit;

 for Count:=2 to USB_HID_BOOT_REPORT_SIZE - 1 do {6 bytes of Keyboard data}
  begin
   if Keyboard.LastReport[Count] = ScanCode then
    begin
     Result:=False;
     Exit;
    end;
  end;
end;

{==============================================================================}

function USBKeyboardCheckRepeated(Keyboard:PUSBKeyboardDevice;ScanCode:Byte):Boolean;
{Check if the passed scan code was the last key pressed and if the repeat delay has expired}
{Keyboard: The USB keyboard device to check for}
{ScanCode: The keyboard scan code to check}

{Note: Caller must hold the keyboard lock}
begin
 {}
 Result:=False;

 {Check Keyboard}
 if Keyboard = nil then Exit;

 if ScanCode = Keyboard.LastCode then
  begin
   if Keyboard.LastCount < Keyboard.Keyboard.KeyboardDelay then
    begin
     Inc(Keyboard.LastCount);
    end
   else
    begin
     Result:=True;
    end;
  end;
end;

{==============================================================================}

function USBKeyboardCheckReleased(Keyboard:PUSBKeyboardDevice;Report:PUSBKeyboardReport;ScanCode:Byte):Boolean;
{Check if the passed scan code has been released (True if not pressed in current report)}
{Keyboard: The USB keyboard device to check for}
{Report: The USB keyboard report to compare against (Current)}
{ScanCode: The keyboard scan code to check}

{Note: Caller must hold the keyboard lock}
var
 Count:Integer;
begin
 {}
 Result:=True;

 {Check Keyboard}
 if Keyboard = nil then Exit;

 {Check Report}
 if Report = nil then Exit;

 for Count:=2 to USB_HID_BOOT_REPORT_SIZE - 1 do {6 bytes of Keyboard data}
  begin
   if Report[Count] = ScanCode then
    begin
     Result:=False;
     Exit;
    end;
  end;
end;

{==============================================================================}

function USBKeyboardDeviceSetLEDs(Keyboard:PUSBKeyboardDevice;LEDs:Byte):LongWord;
{Set the state of the LEDs for a USB keyboard device}
{Keyboard: The USB keyboard device to set the LEDs for}
{LEDs: The LED state to set (eg KEYBOARD_LED_NUMLOCK)}
{Return: USB_STATUS_SUCCESS if completed or another USB error code on failure}
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
{Set the idle duration (Time between reports when no changes) for a USB keyboard device}
{Keyboard: The USB keyboard device to set the idle duration for}
{Duration: The idle duration to set (Milliseconds divided by 4)}
{ReportId: The report Id to set the idle duration for (eg USB_HID_REPORTID_NONE)}
{Return: USB_STATUS_SUCCESS if completed or another USB error code on failure}
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

 {Get Duration}
 Duration:=(Duration div 4);

 {Set Idle}
 Result:=USBControlRequest(Device,nil,USB_HID_REQUEST_SET_IDLE,USB_BMREQUESTTYPE_TYPE_CLASS or USB_BMREQUESTTYPE_DIR_OUT or USB_BMREQUESTTYPE_RECIPIENT_INTERFACE,(Duration shl 8) or ReportId,Keyboard.HIDInterface.Descriptor.bInterfaceNumber,nil,0);
end;

{==============================================================================}

function USBKeyboardDeviceSetProtocol(Keyboard:PUSBKeyboardDevice;Protocol:Byte):LongWord;
{Set the report protocol for a USB keyboard device}
{Keyboard: The USB keyboard device to set the report protocol for}
{Protocol: The report protocol to set (eg USB_HID_PROTOCOL_BOOT)}
{Return: USB_STATUS_SUCCESS if completed or another USB error code on failure}
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

function USBKeyboardDeviceGetHIDDescriptor(Keyboard:PUSBKeyboardDevice;Descriptor:PUSBHIDDescriptor):LongWord;
{Get the HID Descriptor for a USB keyboard device}
{Keyboard: The USB keyboard device to get the descriptor for}
{Descriptor: Pointer to a USB HID Descriptor structure for the returned data}
{Return: USB_STATUS_SUCCESS if completed or another USB error code on failure}
var
 Device:PUSBDevice;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;

 {Check Keyboard}
 if Keyboard = nil then Exit;

 {Check Descriptor}
 if Descriptor = nil then Exit;

 {Check Interface}
 if Keyboard.HIDInterface = nil then Exit;

 {Get Device}
 Device:=PUSBDevice(Keyboard.Keyboard.Device.DeviceData);
 if Device = nil then Exit;

 {Get Descriptor}
 Result:=USBControlRequest(Device,nil,USB_DEVICE_REQUEST_GET_DESCRIPTOR,USB_BMREQUESTTYPE_TYPE_STANDARD or USB_BMREQUESTTYPE_DIR_IN or USB_BMREQUESTTYPE_RECIPIENT_INTERFACE,(USB_HID_DESCRIPTOR_TYPE_HID shl 8),Keyboard.HIDInterface.Descriptor.bInterfaceNumber,Descriptor,SizeOf(TUSBHIDDescriptor));
end;

{==============================================================================}

function USBKeyboardDeviceGetReportDescriptor(Keyboard:PUSBKeyboardDevice;Descriptor:Pointer;Size:LongWord):LongWord;
{Get the Report Descriptor for a USB keyboard device}
{Keyboard: The USB keyboard device to get the descriptor for}
{Descriptor: Pointer to a buffer to return the USB Report Descriptor}
{Size: The size in bytes of the buffer pointed to by Descriptor}
{Return: USB_STATUS_SUCCESS if completed or another USB error code on failure}
var
 Device:PUSBDevice;
begin
 {}
 Result:=USB_STATUS_INVALID_PARAMETER;

 {Check Keyboard}
 if Keyboard = nil then Exit;

 {Check Descriptor}
 if Descriptor = nil then Exit;

 {Check Interface}
 if Keyboard.HIDInterface = nil then Exit;

 {Get Device}
 Device:=PUSBDevice(Keyboard.Keyboard.Device.DeviceData);
 if Device = nil then Exit;

 {Get Descriptor}
 Result:=USBControlRequest(Device,nil,USB_DEVICE_REQUEST_GET_DESCRIPTOR,USB_BMREQUESTTYPE_TYPE_STANDARD or USB_BMREQUESTTYPE_DIR_IN or USB_BMREQUESTTYPE_RECIPIENT_INTERFACE,(USB_HID_DESCRIPTOR_TYPE_REPORT shl 8),Keyboard.HIDInterface.Descriptor.bInterfaceNumber,Descriptor,Size);
end;

{==============================================================================}
{==============================================================================}

initialization
 USBKeyboardInit;

{==============================================================================}

finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
