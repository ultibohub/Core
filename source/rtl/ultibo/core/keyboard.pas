{
Ultibo Keyboard interface unit.

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

References
==========
 
 USB HID Device Class Definition 1_11.pdf
 
   http://www.usb.org/developers/hidpage/HID1_11.pdf

 USB HID Usage Tables 1_12v2.pdf

   http://www.usb.org/developers/hidpage/Hut1_12v2.pdf

 Pascal Keyboard scan codes
 
   http://www.freepascal.org/docs-html/rtl/keyboard/kbdscancode.html
  
 ASCII Control codes
 
   https://en.wikipedia.org/wiki/ASCII
 
Keyboard Devices
================

 This unit provides the Keyboard device interface and keyboard API to be used
 by both drivers and applications.
 
 The API includes functions to create, register, locate, read, write,
 control and configure each connected keyboard device.
 
 The API supports a global keyboard buffer so multiple devices can feed data
 into a common buffer as well as a buffer per device to allow each device
 to be used for a specific purpose. For example an application with two
 separate displays may choose to have a keyboard device to control
 each one individually.

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit Keyboard;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,Keymap,SysUtils;

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
 KEYBOARD_FLAG_NON_BLOCK   = $00000001; {If set device reads are non blocking (Also supported in Flags parameter of KeyboardReadEx)}
 KEYBOARD_FLAG_DIRECT_READ = $00000002; {If set device writes keyboard data to its local buffer which must be read using KeyboardDeviceRead}
 KEYBOARD_FLAG_PEEK_BUFFER = $00000004; {Peek at the buffer to see if any data is available, don't remove it (Used only in Flags parameter of KeyboardReadEx)}
 
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
 KEYBOARD_REPEAT_RATE   = 40;         {40msec -> 25cps}
 KEYBOARD_REPEAT_DELAY  = 10;         {10 x KEYBOARD_REPEAT_RATE = 400msec initial delay before repeat}
 
 {Keyboard Data Definitions (Values for TKeyboardData.Modifiers)}
 KEYBOARD_LEFT_CTRL    =  $00000001; {The Left Control key is pressed}
 KEYBOARD_LEFT_SHIFT   =  $00000002; {The Left Shift key is pressed}
 KEYBOARD_LEFT_ALT     =  $00000004; {The Left Alt key is pressed}
 KEYBOARD_LEFT_GUI     =  $00000008; {The Left GUI (or Windows) key is pressed}
 KEYBOARD_RIGHT_CTRL   =  $00000010; {The Right Control key is pressed}
 KEYBOARD_RIGHT_SHIFT  =  $00000020; {The Right Shift key is pressed}
 KEYBOARD_RIGHT_ALT    =  $00000040; {The Right Alt key is pressed}
 KEYBOARD_RIGHT_GUI    =  $00000080; {The Right GUI (or Windows) key is pressed}
 KEYBOARD_NUM_LOCK     =  $00000100; {Num Lock is currently on}
 KEYBOARD_CAPS_LOCK    =  $00000200; {Caps Lock is currently on} 
 KEYBOARD_SCROLL_LOCK  =  $00000400; {Scroll Lock is currently on}
 KEYBOARD_COMPOSE      =  $00000800; {Compose is currently on} 
 KEYBOARD_KANA         =  $00001000; {Kana is currently on}
 KEYBOARD_KEYUP        =  $00002000; {The key state changed to up}
 KEYBOARD_KEYDOWN      =  $00004000; {The key state changed to down}
 KEYBOARD_KEYREPEAT    =  $00008000; {The key is being repeated}
 KEYBOARD_DEADKEY      =  $00010000; {The key is a being handled as a deadkey}
 KEYBOARD_ALTGR        =  $00020000; {The AltGr key is pressed (Normally also Right Alt but may be Ctrl-Alt)}
 
 {Keyboard logging}
 KEYBOARD_LOG_LEVEL_DEBUG     = LOG_LEVEL_DEBUG;  {Keyboard debugging messages}
 KEYBOARD_LOG_LEVEL_INFO      = LOG_LEVEL_INFO;   {Keyboard informational messages, such as a device being attached or detached}
 KEYBOARD_LOG_LEVEL_WARN      = LOG_LEVEL_WARN;   {Keyboard warning messages}
 KEYBOARD_LOG_LEVEL_ERROR     = LOG_LEVEL_ERROR;  {Keyboard error messages}
 KEYBOARD_LOG_LEVEL_NONE      = LOG_LEVEL_NONE;   {No Keyboard messages}

var 
 KEYBOARD_DEFAULT_LOG_LEVEL:LongWord = KEYBOARD_LOG_LEVEL_DEBUG; {Minimum level for Keyboard messages.  Only messages with level greater than or equal to this will be printed}
 
var 
 {Keyboard logging}
 KEYBOARD_LOG_ENABLED:Boolean; 
 
{==============================================================================}
const
 {USB Keyboard specific constants}
 {Note: The following constants are duplicated with the USBKeyboard unit for backwards compatibility}
 
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
 {Keyboard specific types}
 {Keyboard Data}
 PKeyboardData = ^TKeyboardData;
 TKeyboardData = record
  Modifiers:LongWord;   {Keyboard modifier flags for Shift, Alt, Control etc (eg KEYBOARD_LEFT_CTRL)}
  ScanCode:Word;        {Untranslated scan code value from keyboard (See SCAN_CODE_* constants)}
  KeyCode:Word;         {Translated key code value from keyboard (See KEY_CODE_* constants)}
  CharCode:Char;        {ANSI character representing the translated key code}
  CharUnicode:WideChar; {Unicode character representing the translated key code}
 end;

 {Keyboard Buffer}
 PKeyboardBuffer = ^TKeyboardBuffer;
 TKeyboardBuffer = record
  Wait:TSemaphoreHandle;     {Data ready semaphore}
  Start:LongWord;            {Index of first buffer ready}
  Count:LongWord;            {Number of messages ready in buffer}
  Buffer:array[0..(KEYBOARD_BUFFER_SIZE - 1)] of TKeyboardData; 
 end;
 
 {Keyboard Device}
 PKeyboardDevice = ^TKeyboardDevice;
 
 {Keyboard Enumeration Callback}
 TKeyboardEnumerate = function(Keyboard:PKeyboardDevice;Data:Pointer):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 {Keyboard Notification Callback}
 TKeyboardNotification = function(Device:PDevice;Data:Pointer;Notification:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 
 {Keyboard Device Methods}
 TKeyboardDeviceGet = function(Keyboard:PKeyboardDevice;var KeyCode:Word):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TKeyboardDeviceRead = function(Keyboard:PKeyboardDevice;Buffer:Pointer;Size:LongWord;var Count:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TKeyboardDeviceControl = function(Keyboard:PKeyboardDevice;Request:Integer;Argument1:PtrUInt;var Argument2:PtrUInt):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 
 TKeyboardDevice = record
  {Device Properties}
  Device:TDevice;                      {The Device entry for this Keyboard}
  {Keyboard Properties}
  KeyboardId:LongWord;                 {Unique Id of this Keyboard in the Keyboard table}
  KeyboardState:LongWord;              {Keyboard state (eg KEYBOARD_STATE_ATTACHED)}
  KeyboardLEDs:LongWord;               {Keyboard LEDs (eg KEYBOARD_LED_NUMLOCK)}
  KeyboardRate:LongWord;               {Keyboard repeat rate (Milliseconds)}
  KeyboardDelay:LongWord;              {Keyboard repeat delay (Number of KeyboardRate intervals before first repeat)}
  DeviceGet:TKeyboardDeviceGet;        {A Device specific DeviceGet method implementing a standard Keyboard device interface (Or nil if the default method is suitable)} 
  DeviceRead:TKeyboardDeviceRead;      {A Device specific DeviceRead method implementing a standard Keyboard device interface (Or nil if the default method is suitable)} 
  DeviceControl:TKeyboardDeviceControl;{A Device specific DeviceControl method implementing a standard Keyboard device interface (Or nil if the default method is suitable)}
  {Driver Properties}
  Lock:TMutexHandle;                   {Keyboard lock}
  Code:Word;                           {Scan code of current deadkey (If Applicable)}
  Index:Word;                          {Index state for current deadkey (If Applicable)}
  Modifiers:LongWord;                  {Modifier state for current deadkey (If Applicable)}
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
 {Note: The following structure is duplicated with the USBKeyboard unit for backwards compatibility}
 
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
 
{==============================================================================}
{var}
 {Keyboard specific variables}
 
{==============================================================================}
{Initialization Functions}
procedure KeyboardInit;

{==============================================================================}
{Keyboard Functions}
function KeyboardGet(var KeyCode:Word):LongWord;
function KeyboardPeek:LongWord;
function KeyboardRead(Buffer:Pointer;Size:LongWord;var Count:LongWord):LongWord; inline;
function KeyboardReadEx(Buffer:Pointer;Size,Flags:LongWord;var Count:LongWord):LongWord;

function KeyboardPut(ScanCode,KeyCode:Word;Modifiers:LongWord):LongWord;
function KeyboardWrite(Buffer:Pointer;Size,Count:LongWord):LongWord;

function KeyboardFlush:LongWord;

function KeyboardDeviceGet(Keyboard:PKeyboardDevice;var KeyCode:Word):LongWord;
function KeyboardDeviceRead(Keyboard:PKeyboardDevice;Buffer:Pointer;Size:LongWord;var Count:LongWord):LongWord;
function KeyboardDeviceControl(Keyboard:PKeyboardDevice;Request:Integer;Argument1:PtrUInt;var Argument2:PtrUInt):LongWord;

function KeyboardDeviceSetState(Keyboard:PKeyboardDevice;State:LongWord):LongWord;

function KeyboardDeviceCreate:PKeyboardDevice;
function KeyboardDeviceCreateEx(Size:LongWord):PKeyboardDevice;
function KeyboardDeviceDestroy(Keyboard:PKeyboardDevice):LongWord;

function KeyboardDeviceRegister(Keyboard:PKeyboardDevice):LongWord;
function KeyboardDeviceDeregister(Keyboard:PKeyboardDevice):LongWord;

function KeyboardDeviceFind(KeyboardId:LongWord):PKeyboardDevice;
function KeyboardDeviceFindByName(const Name:String):PKeyboardDevice; inline;
function KeyboardDeviceFindByDescription(const Description:String):PKeyboardDevice; inline;
function KeyboardDeviceEnumerate(Callback:TKeyboardEnumerate;Data:Pointer):LongWord;

function KeyboardDeviceNotification(Keyboard:PKeyboardDevice;Callback:TKeyboardNotification;Data:Pointer;Notification,Flags:LongWord):LongWord;

{==============================================================================}
{RTL Console Functions}
function SysConsoleGetKey(var ACh:Char;AUserData:Pointer):Boolean;
function SysConsolePeekKey(var ACh:Char;AUserData:Pointer):Boolean;

function SysConsoleReadChar(var ACh:Char;AUserData:Pointer):Boolean;
function SysConsoleReadWideChar(var ACh:WideChar;AUserData:Pointer):Boolean;

{==============================================================================}
{Keyboard Helper Functions}
function KeyboardGetCount:LongWord;

function KeyboardDeviceCheck(Keyboard:PKeyboardDevice):PKeyboardDevice;

function KeyboardDeviceTypeToString(KeyboardType:LongWord):String;
function KeyboardDeviceStateToString(KeyboardState:LongWord):String;

function KeyboardDeviceStateToNotification(State:LongWord):LongWord;

function KeyboardRemapCtrlCode(KeyCode,CharCode:Word):Word;
function KeyboardRemapKeyCode(ScanCode,KeyCode:Word;var CharCode:Byte;Modifiers:LongWord):Boolean;
function KeyboardRemapScanCode(ScanCode,KeyCode:Word;var CharCode:Byte;Modifiers:LongWord):Boolean;

function KeyboardInsertData(Keyboard:PKeyboardDevice;Data:PKeyboardData;Signal:Boolean):LongWord;

procedure KeyboardLog(Level:LongWord;Keyboard:PKeyboardDevice;const AText:String);
procedure KeyboardLogInfo(Keyboard:PKeyboardDevice;const AText:String); inline;
procedure KeyboardLogWarn(Keyboard:PKeyboardDevice;const AText:String); inline;
procedure KeyboardLogError(Keyboard:PKeyboardDevice;const AText:String); inline;
procedure KeyboardLogDebug(Keyboard:PKeyboardDevice;const AText:String); inline;

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
 {RTL Console specific variables}
 SysConsoleLastCode:Byte;
 
{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure KeyboardInit;
{Initialize the keyboard unit and device table}

{Note: Called only during system startup}
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
  
 {Setup Platform Console Handlers}
 ConsoleGetKeyHandler:=SysConsoleGetKey;
 ConsolePeekKeyHandler:=SysConsolePeekKey;
 ConsoleReadCharHandler:=SysConsoleReadChar;
 ConsoleReadWideCharHandler:=SysConsoleReadWideChar;
 
 KeyboardInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{Keyboard Functions}
function KeyboardGet(var KeyCode:Word):LongWord;
{Get the first key code from the global keyboard buffer}
{KeyCode: The returned key code read from the buffer (eg KEY_CODE_A)}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: Key code is the value translated from the scan code using the current keymap
       it may not be a character code and it may include non printable characters.
       
       To translate a key code to a character call KeymapGetCharCode()}
var
 Count:LongWord;
 Data:TKeyboardData;
begin
 {}
 Result:=KeyboardReadEx(@Data,SizeOf(TKeyboardData),KEYBOARD_FLAG_NONE,Count);
 if Result = ERROR_SUCCESS then
  begin
   {Check for Key Up and Dead Key events}
   if (Data.Modifiers and (KEYBOARD_KEYUP or KEYBOARD_DEADKEY)) = 0 then
    begin
     KeyCode:=Data.KeyCode;
    end
   else
    begin
     KeyCode:=KEY_CODE_NONE;
    end;  
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

function KeyboardRead(Buffer:Pointer;Size:LongWord;var Count:LongWord):LongWord; inline;
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
{Flags: The flags to use for the read (eg KEYBOARD_FLAG_NON_BLOCK)}
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
     {Acquire the Lock}
     if MutexLock(KeyboardBufferLock) = ERROR_SUCCESS then
      begin
       try
        if KeyboardBuffer.Count > 0 then
         begin
          {Copy Data}
          PKeyboardData(PtrUInt(Buffer) + Offset)^:=KeyboardBuffer.Buffer[KeyboardBuffer.Start];
          
          {Update Count}
          Inc(Count);
          
          Result:=ERROR_SUCCESS;
          Break;
         end
        else
         begin
          Result:=ERROR_NO_MORE_ITEMS;
          Break;
         end;
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
    end
   else
    begin   
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
            
          {Update Size and Offset}
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
  
 {$IFDEF KEYBOARD_DEBUG}
 if KEYBOARD_LOG_ENABLED then KeyboardLogDebug(nil,'Return count=' + IntToStr(Count));
 {$ENDIF}
end;

{==============================================================================}

function KeyboardPut(ScanCode,KeyCode:Word;Modifiers:LongWord):LongWord;
{Put a scan code and key code in the global keyboard buffer}
{ScanCode: The scan code to write to the buffer (eg SCAN_CODE_A)}
{KeyCode: The key code to write to the buffer (eg KEY_CODE_A)}
{Modifiers: The modifier keys to write to the buffer (eg KEYBOARD_LEFT_CTRL)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Data:TKeyboardData;
 Keymap:TKeymapHandle;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Get Keymap}
 Keymap:=KeymapGetDefault;
 if Keymap = INVALID_HANDLE_VALUE then Exit;
 
 {Setup Data}
 FillChar(Data,SizeOf(TKeyboardData),0);
 Data.Modifiers:=Modifiers;
 Data.ScanCode:=ScanCode;
 Data.KeyCode:=KeyCode;
 Data.CharCode:=KeymapGetCharCode(Keymap,Data.KeyCode);
 Data.CharUnicode:=KeymapGetCharUnicode(Keymap,Data.KeyCode);
 
 {Write Data}
 Result:=KeyboardWrite(@Data,SizeOf(TKeyboardData),1);
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
 Offset:=0;
 while (Size >= SizeOf(TKeyboardData)) and (Count > 0) do
  begin
   {Acquire the Lock}
   if MutexLock(KeyboardBufferLock) = ERROR_SUCCESS then
    begin
     try
      {Check Buffer}
      if (KeyboardBuffer.Count < KEYBOARD_BUFFER_SIZE) then
       begin
        {Copy Data}
        KeyboardBuffer.Buffer[(KeyboardBuffer.Start + KeyboardBuffer.Count) mod KEYBOARD_BUFFER_SIZE]:=PKeyboardData(PtrUInt(Buffer) + Offset)^;
        
        {Update Count}
        Inc(KeyboardBuffer.Count);
        
        {Update Count}
        Dec(Count);
        
        {Update Size and Offset}
        Dec(Size,SizeOf(TKeyboardData));
        Inc(Offset,SizeOf(TKeyboardData));
        
        {Signal Data Received}
        SemaphoreSignal(KeyboardBuffer.Wait); 
       end
      else
       begin
        Result:=ERROR_INSUFFICIENT_BUFFER;
        Exit;
       end;
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
    
   {Return Result}
   Result:=ERROR_SUCCESS;
  end;
end;
 
{==============================================================================}
 
function KeyboardFlush:LongWord;
{Flush the contents of the global keyboard buffer}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Acquire the Lock}
 if MutexLock(KeyboardBufferLock) = ERROR_SUCCESS then
  begin
   try
    while KeyboardBuffer.Count > 0 do
     begin
      {Wait for Data (Should not Block)}
      if SemaphoreWait(KeyboardBuffer.Wait) = ERROR_SUCCESS then
       begin
        {Update Start} 
        KeyboardBuffer.Start:=(KeyboardBuffer.Start + 1) mod KEYBOARD_BUFFER_SIZE;
        
        {Update Count}
        Dec(KeyboardBuffer.Count);
       end
      else
       begin
        Result:=ERROR_CAN_NOT_COMPLETE;
        Exit;
       end;    
     end; 
    
    {Return Result}
    Result:=ERROR_SUCCESS;
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

{==============================================================================}

function KeyboardDeviceGet(Keyboard:PKeyboardDevice;var KeyCode:Word):LongWord;
{Get the first key code from the buffer of the specified keyboard}
{Keyboard: The keyboard device to get from}
{KeyCode: The returned key code read from the buffer (eg KEY_CODE_A)}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: Key code is the value translated from the scan code using the current keymap
       it may not be a character code and it may include non printable characters.
       
       To translate a key code to a character call KeymapGetCharCode()}
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
   Result:=Keyboard.DeviceGet(Keyboard,KeyCode);
  end
 else
  begin 
   {Default Method}
   Result:=KeyboardDeviceRead(Keyboard,@Data,SizeOf(TKeyboardData),Count);
   while Result = ERROR_SUCCESS do
    begin
     {Exclude Key Up and Dead Key events}
     if (Data.Modifiers and (KEYBOARD_KEYUP or KEYBOARD_DEADKEY)) = 0 then
      begin
       KeyCode:=Data.KeyCode;
       Break;
      end; 
     
     {Get Next Key}
     Result:=KeyboardDeviceRead(Keyboard,@Data,SizeOf(TKeyboardData),Count);
    end;
  end; 
end;

{==============================================================================}

function KeyboardDeviceRead(Keyboard:PKeyboardDevice;Buffer:Pointer;Size:LongWord;var Count:LongWord):LongWord;
{Read keyboard data packets from the buffer of the specified keyboard}
{Keyboard: The keyboard device to read from}
{Buffer: Pointer to a buffer to copy the keyboard data packets to}
{Size: The size of the buffer in bytes (Must be at least TKeyboardData or greater)}
{Count: The number of keyboard data packets copied to the buffer}
{Return: ERROR_SUCCESS if completed or another error code on failure}
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
end;
 
{==============================================================================}

function KeyboardDeviceControl(Keyboard:PKeyboardDevice;Request:Integer;Argument1:PtrUInt;var Argument2:PtrUInt):LongWord;
{Perform a control request on the specified keyboard device}
{Keyboard: The keyboard device to control}
{Request: The request code for the operation (eg KEYBOARD_CONTROL_GET_FLAG)}
{Argument1: The first argument for the operation (Dependent on request code)}
{Argument2: The second argument for the operation (Dependent on request code)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
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
         
           {Return Result}
           Result:=ERROR_SUCCESS;
          end; 
        end;
       KEYBOARD_CONTROL_CLEAR_LED:begin
         {Clear LED}
         if (Argument1 and not(KEYBOARD_LED_MASK)) = 0 then
          begin
           Keyboard.KeyboardLEDs:=(Keyboard.KeyboardLEDs and not(Argument1));
         
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
{Create a new Keyboard device entry}
{Return: Pointer to new Keyboard device entry or nil if keyboard could not be created}
begin
 {}
 Result:=KeyboardDeviceCreateEx(SizeOf(TKeyboardDevice));
end;

{==============================================================================}

function KeyboardDeviceCreateEx(Size:LongWord):PKeyboardDevice;
{Create a new Keyboard device entry}
{Size: Size in bytes to allocate for new keyboard (Including the keyboard device entry)}
{Return: Pointer to new Keyboard device entry or nil if keyboard could not be created}
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
 
 {Check Defaults}
 if KEYBOARD_NUM_LOCK_DEFAULT then Result.KeyboardLEDs:=Result.KeyboardLEDs or KEYBOARD_LED_NUMLOCK;
 if KEYBOARD_CAPS_LOCK_DEFAULT then Result.KeyboardLEDs:=Result.KeyboardLEDs or KEYBOARD_LED_CAPSLOCK;
 if KEYBOARD_SCROLL_LOCK_DEFAULT then Result.KeyboardLEDs:=Result.KeyboardLEDs or KEYBOARD_LED_SCROLLLOCK;
 
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
{Destroy an existing Keyboard device entry}
{Keyboard: The keyboard device to destroy}
{Return: ERROR_SUCCESS if completed or another error code on failure}
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
{Register a new Keyboard device in the Keyboard table}
{Keyboard: The keyboard device to register}
{Return: ERROR_SUCCESS if completed or another error code on failure}
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
{Deregister a Keyboard device from the Keyboard table}
{Keyboard: The keyboard device to deregister}
{Return: ERROR_SUCCESS if completed or another error code on failure}
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
{Find a keyboard device by ID in the keyboard table}
{KeyboardId: The ID number of the keyboard to find}
{Return: Pointer to keyboard device entry or nil if not found}
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
   
function KeyboardDeviceFindByName(const Name:String):PKeyboardDevice; inline;
{Find a keyboard device by name in the keyboard table}
{Name: The name of the keyboard to find (eg Keyboard0)}
{Return: Pointer to keyboard device entry or nil if not found}
begin
 {}
 Result:=PKeyboardDevice(DeviceFindByName(Name));
end;

{==============================================================================}

function KeyboardDeviceFindByDescription(const Description:String):PKeyboardDevice; inline;
{Find a keyboard device by description in the keyboard table}
{Description: The description of the keyboard to find (eg USB HID Keyboard)}
{Return: Pointer to keyboard device entry or nil if not found}
begin
 {}
 Result:=PKeyboardDevice(DeviceFindByDescription(Description));
end;
      
{==============================================================================}

function KeyboardDeviceEnumerate(Callback:TKeyboardEnumerate;Data:Pointer):LongWord;
{Enumerate all keyboard devices in the keyboard table}
{Callback: The callback function to call for each keyboard in the table}
{Data: A private data pointer to pass to callback for each keyboard in the table}
{Return: ERROR_SUCCESS if completed or another error code on failure}
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
{Register a notification for keyboard device changes}
{Keyboard: The keyboard device to notify changes for (Optional, pass nil for all keyboards)}
{Callback: The function to call when a notification event occurs}
{Data: A private data pointer to pass to callback when a notification event occurs}
{Notification: The events to register for notification of (eg DEVICE_NOTIFICATION_REGISTER)}
{Flags: The flags to control the notification (eg NOTIFIER_FLAG_WORKER)}
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
function SysConsoleGetKey(var ACh:Char;AUserData:Pointer):Boolean;
{Handler for Platform ConsoleGetKey function}
var
 CharCode:Char;
 Count:LongWord;
 Status:LongWord;
 Data:TKeyboardData;
begin
 {}
 Result:=True;
 
 {Check Last}
 if SysConsoleLastCode <> 0 then
  begin
   {Return Key}
   ACh:=Char(SysConsoleLastCode);
   if SysConsoleLastCode = $FF then ACh:=#0;
   
   {Clear Last}
   SysConsoleLastCode:=0;
  end
 else
  begin
   {Get Next Key}
   Status:=KeyboardRead(@Data,SizeOf(TKeyboardData),Count);
   while Status = ERROR_SUCCESS do
    begin
     {Exclude Key Up and Dead Key events}
     if (Data.Modifiers and (KEYBOARD_KEYUP or KEYBOARD_DEADKEY)) = 0 then
      begin
       {Get Char Code}
       CharCode:=Data.CharCode;
       
       {Remap Key Code}
       if KeyboardRemapKeyCode(Data.ScanCode,Data.KeyCode,Byte(CharCode),Data.Modifiers) then
        begin
         {Save Last}
         SysConsoleLastCode:=Byte(CharCode);
         
         {Key is Extended}
         ACh:=#0;
        end
       else
        begin
         {Check Char Code}
         if CharCode = #0 then
          begin
           {Save Last (No Key)}
           SysConsoleLastCode:=$FF;
           
           {Key is Extended}
           ACh:=#0;
          end
         else
          begin  
           {Check Ctrl Keys}
           if (Data.Modifiers and (KEYBOARD_LEFT_CTRL or KEYBOARD_RIGHT_CTRL)) <> 0 then
            begin
             {Remap Ctrl Code}
             CharCode:=Char(KeyboardRemapCtrlCode(Data.KeyCode,Byte(CharCode)));
            end;         

           {Return Key}
           ACh:=CharCode;
          end; 
        end;        
       
       Break;
      end; 
   
     {Get Next Key}
     Status:=KeyboardRead(@Data,SizeOf(TKeyboardData),Count);
    end;
   if Status <> ERROR_SUCCESS then 
    begin
     ACh:=#0;
     
     Result:=False;
    end;  
  end;  
end;

{==============================================================================}

function SysConsolePeekKey(var ACh:Char;AUserData:Pointer):Boolean;
{Handler for Platform ConsolePeekKey function}
var
 CharCode:Char;
 Count:LongWord;
 Status:LongWord;
 Data:TKeyboardData;
begin
 {}
 Result:=False;
 
 {Check Last}
 if SysConsoleLastCode <> 0 then
  begin
   {Return Next Key}
   ACh:=Char(SysConsoleLastCode);
   
   Result:=True;   
  end
 else
  begin
   {Peek Next Key}
   Status:=KeyboardReadEx(@Data,SizeOf(TKeyboardData),KEYBOARD_FLAG_NON_BLOCK or KEYBOARD_FLAG_PEEK_BUFFER,Count);
   while Status = ERROR_SUCCESS do
    begin
     {Discard Key Up and Dead Key events}
     if (Data.Modifiers and (KEYBOARD_KEYUP or KEYBOARD_DEADKEY)) <> 0 then
      begin
       {Get Next Key}
       KeyboardReadEx(@Data,SizeOf(TKeyboardData),KEYBOARD_FLAG_NON_BLOCK,Count);
      end
     else
      begin
       {Get Char Code}
       CharCode:=Data.CharCode;
       
       {Remap Key Code}
       if KeyboardRemapKeyCode(Data.ScanCode,Data.KeyCode,Byte(CharCode),Data.Modifiers) then
        begin
         {Next Key is Extended}
         ACh:=#0;
        end
       else
        begin
         {Check Char Code}
         if CharCode = #0 then
          begin
           {Next Key is Extended}
           ACh:=#0;
          end
         else
          begin         
           {Check Ctrl Keys}
           if (Data.Modifiers and (KEYBOARD_LEFT_CTRL or KEYBOARD_RIGHT_CTRL)) <> 0 then
            begin
             {Remap Ctrl Code}
             CharCode:=Char(KeyboardRemapCtrlCode(Data.KeyCode,Byte(CharCode)));
            end;         
            
           {Return Next Key}
           ACh:=CharCode;
          end; 
        end;        
       
       Result:=True;
       Break;
      end;      
   
     {Peek Next Key}
     Status:=KeyboardReadEx(@Data,SizeOf(TKeyboardData),KEYBOARD_FLAG_NON_BLOCK or KEYBOARD_FLAG_PEEK_BUFFER,Count);
    end;
   if Status <> ERROR_SUCCESS then 
    begin
     ACh:=#0;
    end;  
  end;  
end;

{==============================================================================}

function SysConsoleReadChar(var ACh:Char;AUserData:Pointer):Boolean;
{Handler for Platform ConsoleReadChar function}
var
 CharCode:Char;
 Count:LongWord;
 Status:LongWord;
 Data:TKeyboardData;
begin
 {}
 Result:=True;
 
 {Get Next Key}
 Status:=KeyboardRead(@Data,SizeOf(TKeyboardData),Count);
 while Status = ERROR_SUCCESS do
  begin
   {Exclude Key Up and Dead Key events}
   if (Data.Modifiers and (KEYBOARD_KEYUP or KEYBOARD_DEADKEY)) = 0 then
    begin
     if Data.CharCode <> #0 then
      begin
       {Get Char Code}
       CharCode:=Data.CharCode;
       
       {Check Ctrl Keys}
       if (Data.Modifiers and (KEYBOARD_LEFT_CTRL or KEYBOARD_RIGHT_CTRL)) <> 0 then
        begin
         {Remap Ctrl Code}
         CharCode:=Char(KeyboardRemapCtrlCode(Data.KeyCode,Byte(CharCode)));
        end;         
       
       ACh:=CharCode; 
       Break;
      end; 
    end; 
   
   {Get Next Key}
   Status:=KeyboardRead(@Data,SizeOf(TKeyboardData),Count);
  end;
  
 if Status <> ERROR_SUCCESS then 
  begin
   ACh:=#0;
  end
 else
  begin
   {Echo to Console}
   ConsoleWriteChar(ACh,AUserData);
  end;  
end;

{==============================================================================}

function SysConsoleReadWideChar(var ACh:WideChar;AUserData:Pointer):Boolean;
{Handler for Platform ConsoleReadWideChar function}
var
 Count:LongWord;
 Status:LongWord;
 Data:TKeyboardData;
 CharUnicode:WideChar;
begin
 {}
 Result:=True;
 
 {Get Next Key}
 Status:=KeyboardRead(@Data,SizeOf(TKeyboardData),Count);
 while Status = ERROR_SUCCESS do
  begin
   {Exclude Key Up and Dead Key events}
   if (Data.Modifiers and (KEYBOARD_KEYUP or KEYBOARD_DEADKEY)) = 0 then
    begin
     if Data.CharUnicode <> #0 then
      begin
       {Get Char Unicode}
       CharUnicode:=Data.CharUnicode;
       
       {Check Ctrl Keys}
       if (Data.Modifiers and (KEYBOARD_LEFT_CTRL or KEYBOARD_RIGHT_CTRL)) <> 0 then
        begin
         {Remap Ctrl Code}
         CharUnicode:=WideChar(KeyboardRemapCtrlCode(Data.KeyCode,Word(CharUnicode)));
        end;
       
       ACh:=CharUnicode;
       Break;
      end; 
    end; 
   
   {Get Next Key}
   Status:=KeyboardRead(@Data,SizeOf(TKeyboardData),Count);
  end;
  
 if Status <> ERROR_SUCCESS then 
  begin
   ACh:=#0;
  end
 else
  begin
   {Echo to Console}
   ConsoleWriteChar(ACh,AUserData);
  end;  
end;

{==============================================================================}
{==============================================================================}
{Keyboard Helper Functions}
function KeyboardGetCount:LongWord;
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

function KeyboardRemapCtrlCode(KeyCode,CharCode:Word):Word;
{Remap Ctrl-<Key> combinations to ASCII control codes}

{Note: Caller must check for Left-Ctrl or Right-Ctrl modifiers}
begin
 {}
 Result:=CharCode;
 
 if (KeyCode >= KEY_CODE_A) and (KeyCode <= KEY_CODE_Z) then
  begin
   {Convert to Ctrl-A to Ctrl-Z (^A to ^Z)}
   Result:=KeyCode - (KEY_CODE_A - 1); {Minus 0x60}
  end
 else if (KeyCode >= KEY_CODE_CAPITAL_A) and (KeyCode <= KEY_CODE_CAPITAL_Z) then
  begin
   {Convert to Ctrl-A to Ctrl-Z (^A to ^Z)}
   Result:=KeyCode - (KEY_CODE_CAPITAL_A - 1);  {Minus 0x40}
  end
 else if (KeyCode = KEY_CODE_LEFT_SQUARE) then
  begin
   {Convert to Ctrl-[ (^[)}
   Result:=27;
  end
 else if (KeyCode = KEY_CODE_BACKSLASH) then 
  begin
   {Convert to Ctrl-\ (^\)}
   Result:=28;
  end
 else if (KeyCode = KEY_CODE_RIGHT_SQUARE) then 
  begin
   {Convert to Ctrl-] (^])}
   Result:=29;
  end
 else if (KeyCode = KEY_CODE_6) or (KeyCode = KEY_CODE_CARET) then 
  begin
   {Convert to Ctrl-^ (^^)}
   Result:=30;
  end
 else if (KeyCode = KEY_CODE_MINUS) or (KeyCode = KEY_CODE_UNDERSCORE) then 
  begin
   {Convert to Ctrl-_ (^_)}
   Result:=31;
  end;
end;

{==============================================================================}

function KeyboardRemapKeyCode(ScanCode,KeyCode:Word;var CharCode:Byte;Modifiers:LongWord):Boolean;
{Remap the SCAN_CODE_* and KEY_CODE_* values to DOS compatible scan codes}
{Returns True is the key was remapped, False if it was not}

{See: http://www.freepascal.org/docs-html/rtl/keyboard/kbdscancode.html}
{See also: \source\packages\rtl-console\src\inc\keyscan.inc}

{Note: See below for a version that uses SCAN_CODE_* values instead of translated KEY_CODE_* values}
begin
 {}
 Result:=False;

 {Check for Alt}
 if (Modifiers and (KEYBOARD_LEFT_ALT or KEYBOARD_RIGHT_ALT)) <> 0 then 
  begin
   {Check Key Code}
   case KeyCode of
    {Alt F1-F10}
    KEY_CODE_F1..KEY_CODE_F10:begin
      CharCode:=KeyCode - (KEY_CODE_F1 - $68);
      Result:=True;
     end;
    {Alt F11-F12} 
    KEY_CODE_F11..KEY_CODE_F12:begin
      CharCode:=KeyCode - (KEY_CODE_F11 - $8B);
      Result:=True;
     end;
    {Alt ESC/Space/Back} 
    KEY_CODE_ESCAPE:begin
      CharCode:=$01;
      Result:=True;
     end;
    KEY_CODE_SPACE:begin
      CharCode:=$02;
      Result:=True;
     end;
    KEY_CODE_BACKSPACE:begin
      {Check for Shift}
      if (Modifiers and (KEYBOARD_LEFT_SHIFT or KEYBOARD_RIGHT_SHIFT)) <> 0 then
       begin
        CharCode:=$09;
       end
      else
       begin
        CharCode:=$08;
       end;
      Result:=True;
     end;
    {Alt Home/Up/PgUp/Left/Right/End/Down/PgDn/Ins/Del}
    KEY_CODE_HOME:begin
      CharCode:=$97;
      Result:=True;
     end;
    KEY_CODE_UP_ARROW:begin
      CharCode:=$98;
      Result:=True;
     end;
    KEY_CODE_PAGEUP:begin
      CharCode:=$99;
      Result:=True;
     end;
    KEY_CODE_LEFT_ARROW:begin
      CharCode:=$9B;
      Result:=True;
     end;
    KEY_CODE_RIGHT_ARROW:begin
      CharCode:=$9D;
      Result:=True;
     end;
    KEY_CODE_END:begin
      CharCode:=$9F;
      Result:=True;
     end;
    KEY_CODE_DOWN_ARROW:begin
      CharCode:=$A0;
      Result:=True;
     end;
    KEY_CODE_PAGEDN:begin
      CharCode:=$A1;
      Result:=True;
     end;
    KEY_CODE_INSERT:begin
      CharCode:=$A2;
      Result:=True;
     end;
    KEY_CODE_DELETE:begin
      CharCode:=$A3;
      Result:=True;
     end;
    KEY_CODE_TAB:begin
      CharCode:=$A5;
      Result:=True;
     end;
   end;
   
   {Check for AltGr}
   if (Modifiers and KEYBOARD_ALTGR) = 0 then
    begin
     {Check Key Code}
     case KeyCode of
      {Alt QWERTYUIOP[]}
      KEY_CODE_Q,KEY_CODE_CAPITAL_Q:begin
        CharCode:=$10;
        Result:=True;
       end;
      KEY_CODE_W,KEY_CODE_CAPITAL_W:begin
        CharCode:=$11;
        Result:=True;
       end;
      KEY_CODE_E,KEY_CODE_CAPITAL_E:begin
        CharCode:=$12;
        Result:=True;
       end;
      KEY_CODE_R,KEY_CODE_CAPITAL_R:begin
        CharCode:=$13;
        Result:=True;
       end;
      KEY_CODE_T,KEY_CODE_CAPITAL_T:begin
        CharCode:=$14;
        Result:=True;
       end;
      KEY_CODE_Y,KEY_CODE_CAPITAL_Y:begin
        CharCode:=$15;
        Result:=True;
       end;
      KEY_CODE_U,KEY_CODE_CAPITAL_U:begin
        CharCode:=$16;
        Result:=True;
       end;
      KEY_CODE_I,KEY_CODE_CAPITAL_I:begin
        CharCode:=$17;
        Result:=True;
       end;
      KEY_CODE_O,KEY_CODE_CAPITAL_O:begin
        CharCode:=$18;
        Result:=True;
       end;
      KEY_CODE_P,KEY_CODE_CAPITAL_P:begin
        CharCode:=$19;
        Result:=True;
       end;
      KEY_CODE_LEFT_SQUARE,KEY_CODE_LEFT_BRACE:begin
        CharCode:=$1A;
        Result:=True;
       end;
      KEY_CODE_RIGHT_SQUARE,KEY_CODE_RIGHT_BRACE:begin
        CharCode:=$1B;
        Result:=True;
       end;
      {Alt ASDFGHJKL;'\}
      KEY_CODE_A,KEY_CODE_CAPITAL_A:begin
        CharCode:=$1E;
        Result:=True;
       end;
      KEY_CODE_S,KEY_CODE_CAPITAL_S:begin
        CharCode:=$1F;
        Result:=True;
       end;
      KEY_CODE_D,KEY_CODE_CAPITAL_D:begin
        CharCode:=$20;
        Result:=True;
       end;
      KEY_CODE_F,KEY_CODE_CAPITAL_F:begin
        CharCode:=$21;
        Result:=True;
       end;
      KEY_CODE_G,KEY_CODE_CAPITAL_G:begin
        CharCode:=$22;
        Result:=True;
       end;
      KEY_CODE_H,KEY_CODE_CAPITAL_H:begin
        CharCode:=$23;
        Result:=True;
       end;
      KEY_CODE_J,KEY_CODE_CAPITAL_J:begin
        CharCode:=$24;
        Result:=True;
       end;
      KEY_CODE_K,KEY_CODE_CAPITAL_K:begin
        CharCode:=$25;
        Result:=True;
       end;
      KEY_CODE_L,KEY_CODE_CAPITAL_L:begin
        CharCode:=$26;
        Result:=True;
       end;
      KEY_CODE_SEMICOLON,KEY_CODE_COLON:begin
        CharCode:=$27;
        Result:=True;
       end;
      KEY_CODE_QUOTATION,KEY_CODE_APOSTROPHE:begin
        CharCode:=$28;
        Result:=True;
       end;
      KEY_CODE_BACKSLASH,KEY_CODE_PIPE:begin
        CharCode:=$2B;
        Result:=True;
       end;
      {Alt ZXCVBNM,./}
      KEY_CODE_Z,KEY_CODE_CAPITAL_Z:begin
        CharCode:=$2C;
        Result:=True;
       end;
      KEY_CODE_X,KEY_CODE_CAPITAL_X:begin
        CharCode:=$2D;
        Result:=True;
       end;
      KEY_CODE_C,KEY_CODE_CAPITAL_C:begin
        CharCode:=$2E;
        Result:=True;
       end;
      KEY_CODE_V,KEY_CODE_CAPITAL_V:begin
        CharCode:=$2F;
        Result:=True;
       end;
      KEY_CODE_B,KEY_CODE_CAPITAL_B:begin
        CharCode:=$30;
        Result:=True;
       end;
      KEY_CODE_N,KEY_CODE_CAPITAL_N:begin
        CharCode:=$31;
        Result:=True;
       end;
      KEY_CODE_M,KEY_CODE_CAPITAL_M:begin
        CharCode:=$32;
        Result:=True;
       end;
      KEY_CODE_COMMA,KEY_CODE_LESSTHAN:begin
        CharCode:=$33;
        Result:=True;
       end;
      KEY_CODE_PERIOD,KEY_CODE_GREATERTHAN:begin
        CharCode:=$34;
        Result:=True;
       end;
      KEY_CODE_SLASH,KEY_CODE_QUESTION:begin
        CharCode:=$35;
        Result:=True;
       end;
      {Alt 1/2/3/4/5/6/7/8/9/0/Minus/Equal}
      KEY_CODE_1:begin
        CharCode:=$78;
        Result:=True;
       end;
      KEY_CODE_2:begin
        CharCode:=$79;
        Result:=True;
       end;
      KEY_CODE_3:begin
        CharCode:=$7A;
        Result:=True;
       end;
      KEY_CODE_4:begin
        CharCode:=$7B;
        Result:=True;
       end;
      KEY_CODE_5:begin
        CharCode:=$7C;
        Result:=True;
       end;
      KEY_CODE_6:begin
        CharCode:=$7D;
        Result:=True;
       end;
      KEY_CODE_7:begin
        CharCode:=$7E;
        Result:=True;
       end;
      KEY_CODE_8:begin
        CharCode:=$7F;
        Result:=True;
       end;
      KEY_CODE_9:begin
        CharCode:=$80;
        Result:=True;
       end;
      KEY_CODE_0:begin
        CharCode:=$81;
        Result:=True;
       end;
      KEY_CODE_MINUS:begin
        CharCode:=$82;
        Result:=True;
       end;
      KEY_CODE_EQUALS:begin
        CharCode:=$83;
        Result:=True;
       end;
     end; 
    end;
   
   {Check Scan Code}
   case ScanCode of
    {Alt Asterisk/Plus}
    SCAN_CODE_KEYPAD_ASTERISK:begin
      CharCode:=$37;
      Result:=True;
     end;
    SCAN_CODE_KEYPAD_PLUS:begin
      CharCode:=$4E;
      Result:=True;
     end;
   end;
  end
  
 {Check for Ctrl}
 else if (Modifiers and (KEYBOARD_LEFT_CTRL or KEYBOARD_RIGHT_CTRL)) <> 0 then
  begin
   {Check Key Code}
   case KeyCode of
    {Ctrl F1-F10}
    KEY_CODE_F1..KEY_CODE_F10:begin
      CharCode:=KeyCode - (KEY_CODE_F1 - $5E);
      Result:=True;
     end;
    {Ctrl F11-F12} 
    KEY_CODE_F11..KEY_CODE_F12:begin
      CharCode:=KeyCode - (KEY_CODE_F11 - $89);
      Result:=True;
     end;
    {Ctrl Ins/Del} 
    KEY_CODE_INSERT:begin
      CharCode:=$04;
      Result:=True;
     end;
    KEY_CODE_DELETE:begin
      CharCode:=$06;
      Result:=True;
     end;
    {Ctrl PrtSc/Left/Right/End/PgDn/Home/PgUp/Up/Minus/Down/Tab}
    KEY_CODE_PRINTSCREEN:begin
      CharCode:=$72;
      Result:=True;
     end;
    KEY_CODE_LEFT_ARROW:begin
      CharCode:=$73;
      Result:=True;
     end;
    KEY_CODE_RIGHT_ARROW:begin
      CharCode:=$74;
      Result:=True;
     end;
    KEY_CODE_END:begin
      CharCode:=$75;
      Result:=True;
     end;
    KEY_CODE_PAGEDN:begin
      CharCode:=$76;
      Result:=True;
     end;
    KEY_CODE_HOME:begin
      CharCode:=$77;
      Result:=True;
     end;
    KEY_CODE_PAGEUP:begin
      CharCode:=$84;
      Result:=True;
     end;
    KEY_CODE_UP_ARROW:begin
      CharCode:=$8D;
      Result:=True;
     end;
    KEY_CODE_MINUS:begin
      CharCode:=$8E;
      Result:=True;
     end;
    KEY_CODE_CENTER:begin
      CharCode:=$8F;
      Result:=True;
     end;    
    KEY_CODE_DOWN_ARROW:begin
      CharCode:=$91;
      Result:=True;
     end;
    KEY_CODE_TAB:begin
      CharCode:=$94;
      Result:=True;
     end;
    {Ctrl 2} 
    KEY_CODE_2:begin
      CharCode:=$03;
      Result:=True;
     end;
   end; 
   
   {Check Scan Code}
   case ScanCode of
    {Ctrl Plus}
    SCAN_CODE_KEYPAD_PLUS:begin
      CharCode:=$90;
      Result:=True;
     end;
   end;
  end
  
 {Check for Shift}
 else if (Modifiers and (KEYBOARD_LEFT_SHIFT or KEYBOARD_RIGHT_SHIFT)) <> 0 then
  begin
   {Check Key Code}
   case KeyCode of
    {Shift F1-F10}
    KEY_CODE_F1..KEY_CODE_F10:begin
      CharCode:=KeyCode - (KEY_CODE_F1 - $54);
      Result:=True;
     end;
    {Shift F11-F12} 
    KEY_CODE_F11..KEY_CODE_F12:begin
      CharCode:=KeyCode - (KEY_CODE_F11 - $87);
      Result:=True;
     end;
    {Shift Ins/Del/Tab} 
    KEY_CODE_INSERT:begin
      CharCode:=$05;
      Result:=True;
     end;
    KEY_CODE_DELETE:begin
      CharCode:=$07;
      Result:=True;
     end;
    KEY_CODE_TAB:begin
      CharCode:=$0F;
      Result:=True;
     end;
   end; 
  end
  
 {Check Normal}
 else
  begin
   {Check Key Code}
   case KeyCode of
    {F1-F10}
    KEY_CODE_F1..KEY_CODE_F10:begin
      CharCode:=KeyCode - (KEY_CODE_F1 - $3B);
      Result:=True;
     end;
    {F11-F12} 
    KEY_CODE_F11..KEY_CODE_F12:begin
      CharCode:=KeyCode - (KEY_CODE_F11 - $85);
      Result:=True;
     end;
    {Home/Up/PgUp/Left/Right/End/Down/PgDn/Ins/Del}
    KEY_CODE_HOME:begin
      CharCode:=$47;
      Result:=True;
     end;
    KEY_CODE_UP_ARROW:begin
      CharCode:=$48;
      Result:=True;
     end;
    KEY_CODE_PAGEUP:begin
      CharCode:=$49;
      Result:=True;
     end;
    KEY_CODE_LEFT_ARROW:begin
      CharCode:=$4B;
      Result:=True;
     end;
    KEY_CODE_CENTER:begin
      CharCode:=$4C;
      Result:=True;
     end;
    KEY_CODE_RIGHT_ARROW:begin
      CharCode:=$4D;
      Result:=True;
     end;
    KEY_CODE_END:begin
      CharCode:=$4F;
      Result:=True;
     end;
    KEY_CODE_DOWN_ARROW:begin
      CharCode:=$50;
      Result:=True;
     end;
    KEY_CODE_PAGEDN:begin
      CharCode:=$51;
      Result:=True;
     end;
    KEY_CODE_INSERT:begin
      CharCode:=$52;
      Result:=True;
     end;
    KEY_CODE_DELETE:begin
      CharCode:=$53;
      Result:=True;
     end;
   end; 
  end;
end;

{==============================================================================}

function KeyboardRemapScanCode(ScanCode,KeyCode:Word;var CharCode:Byte;Modifiers:LongWord):Boolean;
{Remap the SCAN_CODE_* and KEY_CODE_* values to DOS compatible scan codes}
{Returns True is the key was remapped, False if it was not}

{See: http://www.freepascal.org/docs-html/rtl/keyboard/kbdscancode.html}
{See also: \source\packages\rtl-console\src\inc\keyscan.inc}

{Note: Same as above except using SCAN_CODE_* values instead of translated KEY_CODE_* values}
begin
 {}
 Result:=False;
 
 {Check for Alt}
 if (Modifiers and (KEYBOARD_LEFT_ALT or KEYBOARD_RIGHT_ALT)) <> 0 then 
  begin
   {Check Scan Code}
   case ScanCode of
    {Alt F1-F10}
    SCAN_CODE_F1..SCAN_CODE_F10:begin
      CharCode:=ScanCode + 46; { $68 }
      Result:=True;
     end;
    {Alt F11-F12} 
    SCAN_CODE_F11..SCAN_CODE_F12:begin
      CharCode:=ScanCode + 71; { $8B }
      Result:=True;
     end;
    {Alt ESC/Space/Back} 
    SCAN_CODE_ESCAPE:begin
      CharCode:=$01;
      Result:=True;
     end;
    SCAN_CODE_SPACE:begin
      CharCode:=$02;
      Result:=True;
     end;
    SCAN_CODE_BACKSPACE:begin
      {Check for Shift}
      if (Modifiers and (KEYBOARD_LEFT_SHIFT or KEYBOARD_RIGHT_SHIFT)) <> 0 then
       begin
        CharCode:=$09;
       end
      else
       begin
        CharCode:=$08;
       end;
      Result:=True;
     end;
    {Alt Home/Up/PgUp/Left/Right/End/Down/PgDn/Ins/Del}
    SCAN_CODE_HOME:begin
      CharCode:=$97;
      Result:=True;
     end;
    SCAN_CODE_UP_ARROW:begin
      CharCode:=$98;
      Result:=True;
     end;
    SCAN_CODE_PAGEUP:begin
      CharCode:=$99;
      Result:=True;
     end;
    SCAN_CODE_LEFT_ARROW:begin
      CharCode:=$9B;
      Result:=True;
     end;
    SCAN_CODE_RIGHT_ARROW:begin
      CharCode:=$9D;
      Result:=True;
     end;
    SCAN_CODE_END:begin
      CharCode:=$9F;
      Result:=True;
     end;
    SCAN_CODE_DOWN_ARROW:begin
      CharCode:=$A0;
      Result:=True;
     end;
    SCAN_CODE_PAGEDN:begin
      CharCode:=$A1;
      Result:=True;
     end;
    SCAN_CODE_INSERT:begin
      CharCode:=$A2;
      Result:=True;
     end;
    SCAN_CODE_DELETE:begin
      CharCode:=$A3;
      Result:=True;
     end;
    SCAN_CODE_TAB:begin
      CharCode:=$A5;
      Result:=True;
     end;
   end;
   
   {Check for AltGr}
   if (Modifiers and KEYBOARD_ALTGR) = 0 then
    begin
     {Check Scan Code}
     case ScanCode of
      {Alt QWERTYUIOP[]}
      SCAN_CODE_Q:begin
        CharCode:=$10;
        Result:=True;
       end;
      SCAN_CODE_W:begin
        CharCode:=$11;
        Result:=True;
       end;
      SCAN_CODE_E:begin
        CharCode:=$12;
        Result:=True;
       end;
      SCAN_CODE_R:begin
        CharCode:=$13;
        Result:=True;
       end;
      SCAN_CODE_T:begin
        CharCode:=$14;
        Result:=True;
       end;
      SCAN_CODE_Y:begin
        CharCode:=$15;
        Result:=True;
       end;
      SCAN_CODE_U:begin
        CharCode:=$16;
        Result:=True;
       end;
      SCAN_CODE_I:begin
        CharCode:=$17;
        Result:=True;
       end;
      SCAN_CODE_O:begin
        CharCode:=$18;
        Result:=True;
       end;
      SCAN_CODE_P:begin
        CharCode:=$19;
        Result:=True;
       end;
      SCAN_CODE_LEFT_SQUARE:begin
        CharCode:=$1A;
        Result:=True;
       end;
      SCAN_CODE_RIGHT_SQUARE:begin
        CharCode:=$1B;
        Result:=True;
       end;
      {Alt ASDFGHJKL;'\}
      SCAN_CODE_A:begin
        CharCode:=$1E;
        Result:=True;
       end;
      SCAN_CODE_S:begin
        CharCode:=$1F;
        Result:=True;
       end;
      SCAN_CODE_D:begin
        CharCode:=$20;
        Result:=True;
       end;
      SCAN_CODE_F:begin
        CharCode:=$21;
        Result:=True;
       end;
      SCAN_CODE_G:begin
        CharCode:=$22;
        Result:=True;
       end;
      SCAN_CODE_H:begin
        CharCode:=$23;
        Result:=True;
       end;
      SCAN_CODE_J:begin
        CharCode:=$24;
        Result:=True;
       end;
      SCAN_CODE_K:begin
        CharCode:=$25;
        Result:=True;
       end;
      SCAN_CODE_L:begin
        CharCode:=$26;
        Result:=True;
       end;
      SCAN_CODE_SEMICOLON:begin
        CharCode:=$27;
        Result:=True;
       end;
      SCAN_CODE_APOSTROPHE:begin
        CharCode:=$28;
        Result:=True;
       end;
      SCAN_CODE_BACKSLASH,SCAN_CODE_NONUS_BACKSLASH:begin
        CharCode:=$2B;
        Result:=True;
       end;
      {Alt ZXCVBNM,./}
      SCAN_CODE_Z:begin
        CharCode:=$2C;
        Result:=True;
       end;
      SCAN_CODE_X:begin
        CharCode:=$2D;
        Result:=True;
       end;
      SCAN_CODE_C:begin
        CharCode:=$2E;
        Result:=True;
       end;
      SCAN_CODE_V:begin
        CharCode:=$2F;
        Result:=True;
       end;
      SCAN_CODE_B:begin
        CharCode:=$30;
        Result:=True;
       end;
      SCAN_CODE_N:begin
        CharCode:=$31;
        Result:=True;
       end;
      SCAN_CODE_M:begin
        CharCode:=$32;
        Result:=True;
       end;
      SCAN_CODE_COMMA:begin
        CharCode:=$33;
        Result:=True;
       end;
      SCAN_CODE_PERIOD:begin
        CharCode:=$34;
        Result:=True;
       end;
      SCAN_CODE_SLASH:begin
        CharCode:=$35;
        Result:=True;
       end;
      {Alt 1/2/3/4/5/6/7/8/9/0/Minus/Equal}
      SCAN_CODE_1:begin
        CharCode:=$78;
        Result:=True;
       end;
      SCAN_CODE_2:begin
        CharCode:=$79;
        Result:=True;
       end;
      SCAN_CODE_3:begin
        CharCode:=$7A;
        Result:=True;
       end;
      SCAN_CODE_4:begin
        CharCode:=$7B;
        Result:=True;
       end;
      SCAN_CODE_5:begin
        CharCode:=$7C;
        Result:=True;
       end;
      SCAN_CODE_6:begin
        CharCode:=$7D;
        Result:=True;
       end;
      SCAN_CODE_7:begin
        CharCode:=$7E;
        Result:=True;
       end;
      SCAN_CODE_8:begin
        CharCode:=$7F;
        Result:=True;
       end;
      SCAN_CODE_9:begin
        CharCode:=$80;
        Result:=True;
       end;
      SCAN_CODE_0:begin
        CharCode:=$81;
        Result:=True;
       end;
      SCAN_CODE_MINUS:begin
        CharCode:=$82;
        Result:=True;
       end;
      SCAN_CODE_EQUALS:begin
        CharCode:=$83;
        Result:=True;
       end;
     end; 
    end;
   
   {Check Scan Code}
   case ScanCode of
    {Alt Asterisk/Plus}
    SCAN_CODE_KEYPAD_ASTERISK:begin
      CharCode:=$37;
      Result:=True;
     end;
    SCAN_CODE_KEYPAD_PLUS:begin
      CharCode:=$4E;
      Result:=True;
     end;
   end;
  end
 
 {Check for Ctrl}
 else if (Modifiers and (KEYBOARD_LEFT_CTRL or KEYBOARD_RIGHT_CTRL)) <> 0 then
  begin
   {Check Scan Code}
   case ScanCode of
    {Ctrl F1-F10}
    SCAN_CODE_F1..SCAN_CODE_F10:begin
      CharCode:=ScanCode + 36; { $5E }
      Result:=True;
     end;
    {Ctrl F11-F12} 
    SCAN_CODE_F11..SCAN_CODE_F12:begin
      CharCode:=ScanCode + 69; { $89 }
      Result:=True;
     end;
    {Ctrl Ins/Del} 
    SCAN_CODE_INSERT:begin
      CharCode:=$04;
      Result:=True;
     end;
    SCAN_CODE_DELETE:begin
      CharCode:=$06;
      Result:=True;
     end;
    {Ctrl PrtSc/Left/Right/End/PgDn/Home/PgUp/Up/Minus/Down/Tab}
    SCAN_CODE_PRINTSCREEN:begin
      CharCode:=$72;
      Result:=True;
     end;
    SCAN_CODE_LEFT_ARROW:begin
      CharCode:=$73;
      Result:=True;
     end;
    SCAN_CODE_RIGHT_ARROW:begin
      CharCode:=$74;
      Result:=True;
     end;
    SCAN_CODE_END:begin
      CharCode:=$75;
      Result:=True;
     end;
    SCAN_CODE_PAGEDN:begin
      CharCode:=$76;
      Result:=True;
     end;
    SCAN_CODE_HOME:begin
      CharCode:=$77;
      Result:=True;
     end;
    SCAN_CODE_PAGEUP:begin
      CharCode:=$84;
      Result:=True;
     end;
    SCAN_CODE_UP_ARROW:begin
      CharCode:=$8D;
      Result:=True;
     end;
    SCAN_CODE_MINUS:begin
      CharCode:=$8E;
      Result:=True;
     end;
    {SCAN_CODE_KEYPAD_5:begin
      CharCode:=$8F;
      Result:=True;
     end;}
    SCAN_CODE_DOWN_ARROW:begin
      CharCode:=$91;
      Result:=True;
     end;
    SCAN_CODE_TAB:begin
      CharCode:=$94;
      Result:=True;
     end;
    {Ctrl 2} 
    SCAN_CODE_2:begin
      CharCode:=$03;
      Result:=True;
     end;
   end; 
   
   {Check Scan Code}
   case ScanCode of
    {Ctrl Plus}
    SCAN_CODE_KEYPAD_PLUS:begin
      CharCode:=$90;
      Result:=True;
     end;
   end;
  end
 
 {Check for Shift}
 else if (Modifiers and (KEYBOARD_LEFT_SHIFT or KEYBOARD_RIGHT_SHIFT)) <> 0 then
  begin
   {Check Scan Code}
   case ScanCode of
    {Shift F1-F10}
    SCAN_CODE_F1..SCAN_CODE_F10:begin
      CharCode:=ScanCode + 26; { $54 }
      Result:=True;
     end;
    {Shift F11-F12} 
    SCAN_CODE_F11..SCAN_CODE_F12:begin
      CharCode:=ScanCode + 67; { $87 }
      Result:=True;
     end;
    {Shift Ins/Del/Tab} 
    SCAN_CODE_INSERT:begin
      CharCode:=$05;
      Result:=True;
     end;
    SCAN_CODE_DELETE:begin
      CharCode:=$07;
      Result:=True;
     end;
    SCAN_CODE_TAB:begin
      CharCode:=$0F;
      Result:=True;
     end;
   end;
  end
 
 {Check Normal}
 else
  begin
   {Check Scan Code}
   case ScanCode of
    {F1-F10}
    SCAN_CODE_F1..SCAN_CODE_F10:begin
      CharCode:=ScanCode + 1; { $3B }
      Result:=True;
     end;
    {F11-F12} 
    SCAN_CODE_F11..SCAN_CODE_F12:begin
      CharCode:=ScanCode + 65; { $85 }
      Result:=True;
     end;
    {Home/Up/PgUp/Left/Right/End/Down/PgDn/Ins/Del}
    SCAN_CODE_HOME:begin
      CharCode:=$47;
      Result:=True;
     end;
    SCAN_CODE_UP_ARROW:begin
      CharCode:=$48;
      Result:=True;
     end;
    SCAN_CODE_PAGEUP:begin
      CharCode:=$49;
      Result:=True;
     end;
    SCAN_CODE_LEFT_ARROW:begin
      CharCode:=$4B;
      Result:=True;
     end;
    {SCAN_CODE_KEYPAD_5:begin
      CharCode:=$4C;
      Result:=True;
     end;}
    SCAN_CODE_RIGHT_ARROW:begin
      CharCode:=$4D;
      Result:=True;
     end;
    SCAN_CODE_END:begin
      CharCode:=$4F;
      Result:=True;
     end;
    SCAN_CODE_DOWN_ARROW:begin
      CharCode:=$50;
      Result:=True;
     end;
    SCAN_CODE_PAGEDN:begin
      CharCode:=$51;
      Result:=True;
     end;
    SCAN_CODE_INSERT:begin
      CharCode:=$52;
      Result:=True;
     end;
    SCAN_CODE_DELETE:begin
      CharCode:=$53;
      Result:=True;
     end;
   end; 
  end;
end;

{==============================================================================}

function KeyboardInsertData(Keyboard:PKeyboardDevice;Data:PKeyboardData;Signal:Boolean):LongWord;
{Insert a TKeyboardData entry into the keyboard buffer (Direct or Global)}
{Keyboard: The keyboard device to insert data for}
{Data: The TKeyboardData entry to insert}
{Signal: If True then signal that new data is availale in the buffer}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: Caller must hold the keyboard lock}
var
 Next:PKeyboardData;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Keyboard}
 if Keyboard = nil then Exit;
 
 {Check Data}
 if Data = nil then Exit;
 
 {Check Flags}
 if (Keyboard.Device.DeviceFlags and KEYBOARD_FLAG_DIRECT_READ) = 0 then
  begin
   {Global Buffer}
   {Acquire the Lock}
   if MutexLock(KeyboardBufferLock) = ERROR_SUCCESS then
    begin
     try
      {Check Buffer}
      if (KeyboardBuffer.Count < KEYBOARD_BUFFER_SIZE) then
       begin
        {Get Next}
        Next:=@KeyboardBuffer.Buffer[(KeyboardBuffer.Start + KeyboardBuffer.Count) mod KEYBOARD_BUFFER_SIZE];
        if Next <> nil then
         begin
          {Copy Data}
          Next^:=Data^;
      
          {Update Count}
          Inc(KeyboardBuffer.Count);
          
          {Signal Data Received}
          if Signal then SemaphoreSignal(KeyboardBuffer.Wait);
          
          {Return Result}
          Result:=ERROR_SUCCESS;
         end;
       end
      else
       begin
        if KEYBOARD_LOG_ENABLED then KeyboardLogError(Keyboard,'Buffer overflow, key discarded');
        
        {Update Statistics}
        Inc(Keyboard.BufferOverruns); 
        
        Result:=ERROR_INSUFFICIENT_BUFFER;
       end;            
     finally
      {Release the Lock}
      MutexUnlock(KeyboardBufferLock);
     end;
    end
   else
    begin
     if KEYBOARD_LOG_ENABLED then KeyboardLogError(Keyboard,'Failed to acquire lock on buffer');
     
     Result:=ERROR_CAN_NOT_COMPLETE;
    end;
  end
 else
  begin              
   {Direct Buffer}
   {Check Buffer}
   if (Keyboard.Buffer.Count < KEYBOARD_BUFFER_SIZE) then
    begin
     {Get Next}
     Next:=@Keyboard.Buffer.Buffer[(Keyboard.Buffer.Start + Keyboard.Buffer.Count) mod KEYBOARD_BUFFER_SIZE];
     if Next <> nil then
      begin
       {Copy Data}
       Next^:=Data^;
       
       {Update Count}
       Inc(Keyboard.Buffer.Count);
       
       {Signal Data Received}
       if Signal then SemaphoreSignal(Keyboard.Buffer.Wait);
       
       {Return Result}
       Result:=ERROR_SUCCESS;
      end;
    end
   else
    begin
     if KEYBOARD_LOG_ENABLED then KeyboardLogError(Keyboard,'Buffer overflow, key discarded');
     
     {Update Statistics}
     Inc(Keyboard.BufferOverruns); 
     
     Result:=ERROR_INSUFFICIENT_BUFFER;
    end;            
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
 else if Level = KEYBOARD_LOG_LEVEL_WARN then
  begin
   WorkBuffer:=WorkBuffer + '[WARN] ';
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

procedure KeyboardLogInfo(Keyboard:PKeyboardDevice;const AText:String); inline;
begin
 {}
 KeyboardLog(KEYBOARD_LOG_LEVEL_INFO,Keyboard,AText);
end;

{==============================================================================}

procedure KeyboardLogWarn(Keyboard:PKeyboardDevice;const AText:String); inline;
begin
 {}
 KeyboardLog(KEYBOARD_LOG_LEVEL_WARN,Keyboard,AText);
end;


{==============================================================================}

procedure KeyboardLogError(Keyboard:PKeyboardDevice;const AText:String); inline;
begin
 {}
 KeyboardLog(KEYBOARD_LOG_LEVEL_ERROR,Keyboard,AText);
end;

{==============================================================================}

procedure KeyboardLogDebug(Keyboard:PKeyboardDevice;const AText:String); inline;
begin
 {}
 KeyboardLog(KEYBOARD_LOG_LEVEL_DEBUG,Keyboard,AText);
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
