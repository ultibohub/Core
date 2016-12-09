{
Ultibo Keymap interface unit.

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

 
References
==========

 Keyboard layouts - https://en.wikipedia.org/wiki/Keyboard_layout

 QWERTY keyboard - https://en.wikipedia.org/wiki/QWERTY
 
 QWERTZ keyboard - https://en.wikipedia.org/wiki/QWERTZ
 
 AZERTY keyboard - https://en.wikipedia.org/wiki/AZERTY
 
 AltGr Key - https://en.wikipedia.org/wiki/AltGr_key
 
 Caps Lock - https://en.wikipedia.org/wiki/Caps_lock
 
 Dead Key - https://en.wikipedia.org/wiki/Dead_key
 
 Windows Keyboard layouts - https://msdn.microsoft.com/en-au/goglobal/bb964651
 
Keymaps
=======

 Keymaps define the translation of a keyboard scan code (the SCAN_CODE_* values) to a key code
 value (the KEY_CODE_* values) and provide the ability to handle different keyboard layouts in
 different countries and regions.
 
 The keyboard scan codes are based on the values in Section 10 of the Universal Serial Bus HID
 Usage Tables v1.12 and are the actual values returned by the keyboard when a key is pressed.
 
 The key code values are based on the Unicode standard with each key code mapped to the code point
 for that character.
 
 This allows almost infinite flexibility in the way keyboard scan codes are mapped to actual 
 characters and avoids the need to make unreliable assumptions about ASCII characters and upper
 or lower case handling.
 
 Since the output of the keyboard is a stream of TKeyboardData structures containing both the
 scan code and the key code then higher level functions can retranslate the data in any way required.
 
 Caps Keys
 ---------
 
 Caps keys account for the set of keys which are affected by Caps Lock in any given keyboard layout.
 
 In some layouts only the alphabetic keys are affected, in other layouts some or all of the numeric
 and punctuation keys are also affected.
 
 The caps keys data for any keyboard layout allows defining ranges of keys that are affected by the
 Caps Lock state.
  
 Dead Keys
 ---------
 
 Dead keys account for the set of keys which behave as dead keys in any given keyboard layout.
 
 On pressing a deadkey it will be recognized by the keyboard as such and stored until the next key
 press occurs. If the next keypress is one of the resolves for the pressed dead key then the output
 character will be the key code value of the resolve not for the key itself.
 
 If the next keypress after a dead key is not one of the resolves that the dead key and the pressed
 key will both be output to the keyboard buffer.
 
 Setting the default keymap for the system
 -----------------------------------------
 
 Additional keymaps are provided as units which can be included in a program and will auto load
 themselves if included.
 
 All keymap units are configured to check for both the environment variable KEYMAP_DEFAULT and the
 global configuration variable KEYMAP_DEFAULT to determine if either of them is set to the name of
 that keymap. If either is set to the keymap name then that keymap will also set itself as the default
 during system startup.
 
 The environment variable KEYMAP_DEFAULT can be set by adding KEYMAP_DEFAULT=XX (where XX is the 
 name of the keymap, eg DE for Keymap_DE) to the command line of the application (dependent on the
 system, for a Raspberry Pi use the cmdline.txt file on the SD card).
 
 The global configuration variable KEYMAP_DEFAULT can be set in code by including the GlobalConfig
 unit in a program and setting the variable during startup.
 
 At any time after startup the default keymap can be changed by call the KeymapSetDefault function.
 
}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit Keymap;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,SysUtils;

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
const
 {Keymap specific constants}
 KEYMAP_KEY_COUNT = 256;   {Number of key mappings in a keymap entry}
 KEYMAP_ROW_COUNT = 4;     {Number of mapping rows per key in a keymap entry (Normal, Shift, AltGr, Shift_AltGr)}
 KEYMAP_ROW_SIZE  = SizeOf(Word);
 
 KEYMAP_MAX_CAPSKEYS = 50; {Maximum number of keys that can be listed in the TKeymapCapskeys structure}
 KEYMAP_MAX_DEADKEYS = 5;  {Maximum number of keys that can be listed in the TKeymapDeadkeys structure}
 KEYMAP_MAX_RESOLVES = 20; {Maximum number of resolves that can be listed in the TKeymapDeadkey structure}
 
 {Keymap Signature}
 KEYMAP_SIGNATURE = $863FDBA1;
 
 {Keymap Mode constants}
 KEYMAP_MODE_NONE = 0;
 
 {Keymap Flag constants}
 KEYMAP_FLAG_NONE       = $00000000;
 KEYMAP_FLAG_ALTGR      = $00000001; {The key mapping uses the AltGr key}
 KEYMAP_FLAG_CAPS_ALL   = $00000002; {Caps Lock shifts all characters not just alphabetic}
 KEYMAP_FLAG_CAPS_ASCII = $00000004; {Caps Lock shifts only ASCII characters A to Z}
 
 {Keymap Index constants}
 KEYMAP_INDEX_NORMAL      = 0; {Normal (unshifted) value}
 KEYMAP_INDEX_SHIFT       = 1; {Shifted value}
 KEYMAP_INDEX_ALTGR       = 2; {AltGr value}
 KEYMAP_INDEX_SHIFT_ALTGR = 3; {Shift plus AltGr value}
 
{==============================================================================}
type
 {Keymap specific types}

 {Keymap Header}
 PKeymapHeader = ^TKeymapHeader;
 TKeymapHeader = record
  Mode:LongWord;           {Keymap mode (eg KEYMAP_MODE_NONE)}
  Flags:LongWord;          {Keymap flags (eg KEYMAP_FLAG_ALTGR)}
  KeyCount:LongWord;       {Number of keys in keymap (Default: KEYMAP_KEY_COUNT)}
  RowCount:LongWord;       {Number of index rows in keymap (Default: KEYMAP_ROW_COUNT)}
  Name:String[255];        {Keymap name}
  Description:String[255]; {Keymap description}
 end;
 
 {Keymap Data}
 PKeymapData = ^TKeymapData;
 TKeymapData = record
  Data:array[0..255,KEYMAP_INDEX_NORMAL..KEYMAP_INDEX_SHIFT_ALTGR] of Word; {Key mapping data, 4 words for Normal, Shift, AltGr, Shift+AltGr for each of the 256 keys (See KEY_CODE_* constants)}
 end;
 
 {Keymap Chars}
 PKeymapChars = ^TKeymapChars; 
 TKeymapChars = array[0..0] of Word;
 
 {Keymap Capskey (Keys affected by the Caps Lock function)}
 PKeymapCapskey = ^TKeymapCapskey;
 TKeymapCapskey = record
  First:Word;                    {First key in caps key range (SCAN_CODE_* value)}
  Last:Word;                     {Last key in caps key range (SCAN_CODE_* value)(Make first and last the same for a single key)}
 end;
 
 {Keymap Capskeys}
 PKeymapCapskeys = ^TKeymapCapskeys;
 TKeymapCapskeys = record
  Count:LongWord;
  Keys:array[0..KEYMAP_MAX_CAPSKEYS - 1] of TKeymapCapskey;
 end;
 
 {Keymap Resolve}
 PKeymapResolve = ^TKeymapResolve;
 TKeymapResolve = record
  Key:Word;   {The scan code of the key pressed after the deadkey (SCAN_CODE_* value)}
  Index:Byte; {The index state of the key pressed after the deadkey (eg KEYMAP_INDEX_NORMAL)}
  Code:Word;  {The key code of the resulting character (KEY_CODE_* value)}
 end;
 
 {Keymap Deadkey (Keys which behave as dead keys)}
 PKeymapDeadkey = ^TKeymapDeadkey;
 TKeymapDeadkey = record
  Key:Word;                      {The scan code of the key which behaves as a deadkey (SCAN_CODE_* value)}
  Index:Byte;                    {The index state in which the key behaves as a deadkey (eg KEYMAP_INDEX_NORMAL)}
  Resolves:array[0..KEYMAP_MAX_RESOLVES - 1] of TKeymapResolve;
 end;
 
 {Keymap Deadkeys}
 PKeymapDeadkeys = ^TKeymapDeadkeys;
 TKeymapDeadkeys = record
  Count:LongWord;
  Keys:array[0..KEYMAP_MAX_DEADKEYS - 1] of TKeymapDeadkey;
 end;
 
 {Keymap Properties}
 PKeymapProperties = ^TKeymapProperties;
 TKeymapProperties = record
  KeymapMode:LongWord;           {Keymap mode (eg KEYMAP_MODE_NONE)}
  KeymapFlags:LongWord;          {Keymap flags (eg KEYMAP_FLAG_ALTGR)}
  KeyCount:LongWord;             {Number of keys in keymap (Default: KEYMAP_KEY_COUNT)}
  RowCount:LongWord;             {Number of index rows in keymap (Default: KEYMAP_ROW_COUNT)}
  KeymapName:String;             {Keymap name}
  KeymapDescription:String;      {Keymap description}
 end;

 PKeymapEntry = ^TKeymapEntry;
 
 {Keymap Enumeration Callback}
 TKeymapEnumerate = function(Handle:TKeymapHandle;Data:Pointer):LongWord;
 
 {Keymap Entry}
 TKeymapEntry = record
  {Keymap Properties}
  Signature:LongWord;            {Signature for entry validation}
  KeymapMode:LongWord;           {Keymap mode (eg KEYMAP_MODE_NONE)}
  KeymapFlags:LongWord;          {Keymap flags (eg KEYMAP_FLAG_ALTGR)}
  KeymapName:String;             {Keymap name}
  KeymapDescription:String;      {Keymap description}
  {Driver Properties}
  KeyData:Pointer;               {Keymap key data}
  KeyCount:LongWord;             {Number of keys in key data (Default: KEYMAP_KEY_COUNT)}
  RowCount:LongWord;             {Number of index rows in key data (Default: KEYMAP_ROW_COUNT)}
  CapskeysData:PKeymapCapskeys;  {Keymap Capskeys data (Keys affected by Caps Lock)}
  DeadkeysData:PKeymapDeadkeys;  {Keymap Deadkeys data (Keys which behave as Dead keys)}
  {Internal Properties}
  Prev:PKeymapEntry;             {Previous entry in Keymap table}
  Next:PKeymapEntry;             {Next entry in Keymap table}
 end;
 
{==============================================================================}
{var}
 {Keymap specific variables}
 
{==============================================================================}
{Initialization Functions}
procedure KeymapInit;

{==============================================================================}
{Keymap Functions}
function KeymapLoad(Header:PKeymapHeader;Data:PKeymapData;Size:LongWord):TKeymapHandle;
function KeymapLoadEx(Header:PKeymapHeader;Data:PKeymapData;Capskeys:PKeymapCapskeys;Deadkeys:PKeymapDeadkeys;Size:LongWord;Properties:PKeymapProperties):TKeymapHandle;
function KeymapUnload(Handle:TKeymapHandle):LongWord;

function KeymapGetName(Handle:TKeymapHandle):String;
function KeymapGetDescription(Handle:TKeymapHandle):String;

function KeymapCheckFlag(Handle:TKeymapHandle;Flag:LongWord):Boolean;

function KeymapGetKeyCode(Handle:TKeymapHandle;ScanCode:Word;Index:Byte):Word;

function KeymapGetCharCode(Handle:TKeymapHandle;KeyCode:Word):Char;
function KeymapGetCharUnicode(Handle:TKeymapHandle;KeyCode:Word):WideChar;

function KeymapCheckCapskey(Handle:TKeymapHandle;ScanCode:Word):Boolean;

function KeymapCheckDeadkey(Handle:TKeymapHandle;ScanCode:Word;Index:Byte):Boolean;
function KeymapResolveDeadkey(Handle:TKeymapHandle;DeadCode,ScanCode:Word;DeadIndex,ScanIndex:Byte;var KeyCode:Word):Boolean;

function KeymapGetProperties(Handle:TKeymapHandle;Properties:PKeymapProperties):LongWord;

function KeymapFindByName(const Name:String):TKeymapHandle; 
function KeymapFindByDescription(const Description:String):TKeymapHandle; 
function KeymapEnumerate(Callback:TKeymapEnumerate;Data:Pointer):LongWord;

{==============================================================================}
{Keymap Helper Functions}
function KeymapGetCount:LongWord; inline;
function KeymapGetDefault:TKeymapHandle; inline;
function KeymapSetDefault(Handle:TKeymapHandle):LongWord; 

function KeymapCheck(Keymap:PKeymapEntry):PKeymapEntry;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {Keymap specific variables}
 KeymapInitialized:Boolean;

 KeymapTable:PKeymapEntry;
 KeymapTableLock:TCriticalSectionHandle = INVALID_HANDLE_VALUE;
 KeymapTableCount:LongWord;
 
 KeymapDefault:TKeymapHandle = INVALID_HANDLE_VALUE;
 
var 
 {Default Keymap - US English}
 KEYMAP_US_ENGLISH_HEADER:TKeymapHeader = (
  Mode:KEYMAP_MODE_NONE;
  Flags:KEYMAP_FLAG_CAPS_ASCII;
  KeyCount:KEYMAP_KEY_COUNT;
  RowCount:KEYMAP_ROW_COUNT;
  Name:('US');
  Description:('US English')
  );
 
 KEYMAP_US_ENGLISH_DATA:TKeymapData = (
        {Scan Code mappings:                 Normal                         Shift                          AltGr                          Shift+AltGr  }
  Data:({SCAN_CODE_NONE                   } (KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_ROLLOVER               } (KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_POSTFAIL               } (KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_ERROR                  } (KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_A                      } (KEY_CODE_A,                    KEY_CODE_CAPITAL_A,            KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_B                      } (KEY_CODE_B,                    KEY_CODE_CAPITAL_B,            KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_C                      } (KEY_CODE_C,                    KEY_CODE_CAPITAL_C,            KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_D                      } (KEY_CODE_D,                    KEY_CODE_CAPITAL_D,            KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_E                      } (KEY_CODE_E,                    KEY_CODE_CAPITAL_E,            KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_F                      } (KEY_CODE_F,                    KEY_CODE_CAPITAL_F,            KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_G                      } (KEY_CODE_G,                    KEY_CODE_CAPITAL_G,            KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_H                      } (KEY_CODE_H,                    KEY_CODE_CAPITAL_H,            KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_I                      } (KEY_CODE_I,                    KEY_CODE_CAPITAL_I,            KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_J                      } (KEY_CODE_J,                    KEY_CODE_CAPITAL_J,            KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_K                      } (KEY_CODE_K,                    KEY_CODE_CAPITAL_K,            KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_L                      } (KEY_CODE_L,                    KEY_CODE_CAPITAL_L,            KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_M                      } (KEY_CODE_M,                    KEY_CODE_CAPITAL_M,            KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_N                      } (KEY_CODE_N,                    KEY_CODE_CAPITAL_N,            KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_O                      } (KEY_CODE_O,                    KEY_CODE_CAPITAL_O,            KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_P                      } (KEY_CODE_P,                    KEY_CODE_CAPITAL_P,            KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_Q                      } (KEY_CODE_Q,                    KEY_CODE_CAPITAL_Q,            KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_R                      } (KEY_CODE_R,                    KEY_CODE_CAPITAL_R,            KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_S                      } (KEY_CODE_S,                    KEY_CODE_CAPITAL_S,            KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_T                      } (KEY_CODE_T,                    KEY_CODE_CAPITAL_T,            KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_U                      } (KEY_CODE_U,                    KEY_CODE_CAPITAL_U,            KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_V                      } (KEY_CODE_V,                    KEY_CODE_CAPITAL_V,            KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_W                      } (KEY_CODE_W,                    KEY_CODE_CAPITAL_W,            KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_X                      } (KEY_CODE_X,                    KEY_CODE_CAPITAL_X,            KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_Y                      } (KEY_CODE_Y,                    KEY_CODE_CAPITAL_Y,            KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_Z                      } (KEY_CODE_Z,                    KEY_CODE_CAPITAL_Z,            KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_1                      } (KEY_CODE_1,                    KEY_CODE_EXCLAMATION,          KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_2                      } (KEY_CODE_2,                    KEY_CODE_AT,                   KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_3                      } (KEY_CODE_3,                    KEY_CODE_NUMBER,               KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_4                      } (KEY_CODE_4,                    KEY_CODE_DOLLAR,               KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_5                      } (KEY_CODE_5,                    KEY_CODE_PERCENT,              KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_6                      } (KEY_CODE_6,                    KEY_CODE_CARET,                KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_7                      } (KEY_CODE_7,                    KEY_CODE_AMPERSAND,            KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_8                      } (KEY_CODE_8,                    KEY_CODE_ASTERISK,             KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_9                      } (KEY_CODE_9,                    KEY_CODE_LEFT_BRACKET,         KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_0                      } (KEY_CODE_0,                    KEY_CODE_RIGHT_BRACKET,        KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_ENTER                  } (KEY_CODE_ENTER,                KEY_CODE_ENTER,                KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_ESCAPE                 } (KEY_CODE_ESCAPE,               KEY_CODE_ESCAPE,               KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_BACKSPACE              } (KEY_CODE_BACKSPACE,            KEY_CODE_BACKSPACE,            KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_TAB                    } (KEY_CODE_TAB,                  KEY_CODE_TAB,                  KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_SPACE                  } (KEY_CODE_SPACE,                KEY_CODE_SPACE,                KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_MINUS                  } (KEY_CODE_MINUS,                KEY_CODE_UNDERSCORE,           KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_EQUALS                 } (KEY_CODE_EQUALS,               KEY_CODE_PLUS,                 KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_LEFT_SQUARE            } (KEY_CODE_LEFT_SQUARE,          KEY_CODE_LEFT_BRACE,           KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_RIGHT_SQUARE           } (KEY_CODE_RIGHT_SQUARE,         KEY_CODE_RIGHT_BRACE,          KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_BACKSLASH              } (KEY_CODE_BACKSLASH,            KEY_CODE_PIPE,                 KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_NONUS_NUMBER           } (KEY_CODE_NUMBER,               KEY_CODE_TILDE,                KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_SEMICOLON              } (KEY_CODE_SEMICOLON,            KEY_CODE_COLON,                KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_APOSTROPHE             } (KEY_CODE_APOSTROPHE,           KEY_CODE_QUOTATION,            KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_GRAVE                  } (KEY_CODE_GRAVE,                KEY_CODE_TILDE,                KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_COMMA                  } (KEY_CODE_COMMA,                KEY_CODE_LESSTHAN,             KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_PERIOD                 } (KEY_CODE_PERIOD,               KEY_CODE_GREATERTHAN,          KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_SLASH                  } (KEY_CODE_SLASH,                KEY_CODE_QUESTION,             KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_CAPSLOCK               } (KEY_CODE_CAPSLOCK,             KEY_CODE_CAPSLOCK,             KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_F1                     } (KEY_CODE_F1,                   KEY_CODE_F1,                   KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_F2                     } (KEY_CODE_F2,                   KEY_CODE_F2,                   KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_F3                     } (KEY_CODE_F3,                   KEY_CODE_F3,                   KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_F4                     } (KEY_CODE_F4,                   KEY_CODE_F4,                   KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_F5                     } (KEY_CODE_F5,                   KEY_CODE_F5,                   KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_F6                     } (KEY_CODE_F6,                   KEY_CODE_F6,                   KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_F7                     } (KEY_CODE_F7,                   KEY_CODE_F7,                   KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_F8                     } (KEY_CODE_F8,                   KEY_CODE_F8,                   KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_F9                     } (KEY_CODE_F9,                   KEY_CODE_F9,                   KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_F10                    } (KEY_CODE_F10,                  KEY_CODE_F10,                  KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_F11                    } (KEY_CODE_F11,                  KEY_CODE_F11,                  KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_F12                    } (KEY_CODE_F12,                  KEY_CODE_F12,                  KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_PRINTSCREEN            } (KEY_CODE_PRINTSCREEN,          KEY_CODE_PRINTSCREEN,          KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_SCROLLLOCK             } (KEY_CODE_SCROLLLOCK,           KEY_CODE_SCROLLLOCK,           KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_PAUSE                  } (KEY_CODE_PAUSE,                KEY_CODE_PAUSE,                KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_INSERT                 } (KEY_CODE_INSERT,               KEY_CODE_INSERT,               KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_HOME                   } (KEY_CODE_HOME,                 KEY_CODE_HOME,                 KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_PAGEUP                 } (KEY_CODE_PAGEUP,               KEY_CODE_PAGEUP,               KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_DELETE                 } (KEY_CODE_DELETE,               KEY_CODE_DELETE,               KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_END                    } (KEY_CODE_END,                  KEY_CODE_END,                  KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_PAGEDN                 } (KEY_CODE_PAGEDN,               KEY_CODE_PAGEDN,               KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_RIGHT_ARROW            } (KEY_CODE_RIGHT_ARROW,          KEY_CODE_RIGHT_ARROW,          KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_LEFT_ARROW             } (KEY_CODE_LEFT_ARROW,           KEY_CODE_LEFT_ARROW,           KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_DOWN_ARROW             } (KEY_CODE_DOWN_ARROW,           KEY_CODE_DOWN_ARROW,           KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_UP_ARROW               } (KEY_CODE_UP_ARROW,             KEY_CODE_UP_ARROW,             KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_NUMLOCK                } (KEY_CODE_NUMLOCK,              KEY_CODE_NUMLOCK,              KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_KEYPAD_SLASH           } (KEY_CODE_SLASH,                KEY_CODE_SLASH,                KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_KEYPAD_ASTERISK        } (KEY_CODE_ASTERISK,             KEY_CODE_ASTERISK,             KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_KEYPAD_MINUS           } (KEY_CODE_MINUS,                KEY_CODE_MINUS,                KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_KEYPAD_PLUS            } (KEY_CODE_PLUS,                 KEY_CODE_PLUS,                 KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_KEYPAD_ENTER           } (KEY_CODE_ENTER,                KEY_CODE_ENTER,                KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_KEYPAD_1               } (KEY_CODE_END,                  KEY_CODE_1,                    KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_KEYPAD_2               } (KEY_CODE_DOWN_ARROW,           KEY_CODE_2,                    KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_KEYPAD_3               } (KEY_CODE_PAGEDN,               KEY_CODE_3,                    KEY_CODE_NONE,                 KEY_CODE_NONE),             
        {SCAN_CODE_KEYPAD_4               } (KEY_CODE_LEFT_ARROW,           KEY_CODE_4,                    KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_KEYPAD_5               } (KEY_CODE_CENTER,               KEY_CODE_5,                    KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_KEYPAD_6               } (KEY_CODE_RIGHT_ARROW,          KEY_CODE_6,                    KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_KEYPAD_7               } (KEY_CODE_HOME,                 KEY_CODE_7,                    KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_KEYPAD_8               } (KEY_CODE_UP_ARROW,             KEY_CODE_8,                    KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_KEYPAD_9               } (KEY_CODE_PAGEUP,               KEY_CODE_9,                    KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_KEYPAD_0               } (KEY_CODE_INSERT,               KEY_CODE_0,                    KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_KEYPAD_PERIOD          } (KEY_CODE_DELETE,               KEY_CODE_PERIOD,               KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_NONUS_BACKSLASH        } (KEY_CODE_BACKSLASH,            KEY_CODE_PIPE,                 KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_APPLICATION            } (KEY_CODE_APPLICATION,          KEY_CODE_APPLICATION,          KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_POWER                  } (KEY_CODE_POWER,                KEY_CODE_POWER,                KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_KEYPAD_EQUALS          } (KEY_CODE_EQUALS,               KEY_CODE_EQUALS,               KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_F13                    } (KEY_CODE_F13,                  KEY_CODE_F13,                  KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_F14                    } (KEY_CODE_F14,                  KEY_CODE_F14,                  KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_F15                    } (KEY_CODE_F15,                  KEY_CODE_F15,                  KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_F16                    } (KEY_CODE_F16,                  KEY_CODE_F16,                  KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_F17                    } (KEY_CODE_F17,                  KEY_CODE_F17,                  KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_F18                    } (KEY_CODE_F18,                  KEY_CODE_F18,                  KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_F19                    } (KEY_CODE_F19,                  KEY_CODE_F19,                  KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_F20                    } (KEY_CODE_F20,                  KEY_CODE_F20,                  KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_F21                    } (KEY_CODE_F21,                  KEY_CODE_F21,                  KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_F22                    } (KEY_CODE_F22,                  KEY_CODE_F22,                  KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_F23                    } (KEY_CODE_F23,                  KEY_CODE_F23,                  KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_F24                    } (KEY_CODE_F24,                  KEY_CODE_F24,                  KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_EXECUTE                } (KEY_CODE_EXECUTE,              KEY_CODE_EXECUTE,              KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_HELP                   } (KEY_CODE_HELP,                 KEY_CODE_HELP,                 KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_MENU                   } (KEY_CODE_MENU,                 KEY_CODE_MENU,                 KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_SELECT                 } (KEY_CODE_SELECT,               KEY_CODE_SELECT,               KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_STOP                   } (KEY_CODE_STOP,                 KEY_CODE_STOP,                 KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_AGAIN                  } (KEY_CODE_AGAIN,                KEY_CODE_AGAIN,                KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_UNDO                   } (KEY_CODE_UNDO,                 KEY_CODE_UNDO,                 KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_CUT                    } (KEY_CODE_CUT,                  KEY_CODE_CUT,                  KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_COPY                   } (KEY_CODE_COPY,                 KEY_CODE_COPY,                 KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_PASTE                  } (KEY_CODE_PASTE,                KEY_CODE_PASTE,                KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_FIND                   } (KEY_CODE_FIND,                 KEY_CODE_FIND,                 KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_MUTE                   } (KEY_CODE_MUTE,                 KEY_CODE_MUTE,                 KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_VOLUMEUP               } (KEY_CODE_VOLUMEUP,             KEY_CODE_VOLUMEUP,             KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_VOLUMEDN               } (KEY_CODE_VOLUMEDOWN,           KEY_CODE_VOLUMEDOWN,           KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_LOCKING_CAPSLOCK       } (KEY_CODE_LOCKING_CAPSLOCK,     KEY_CODE_LOCKING_CAPSLOCK,     KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_LOCKING_NUMLOCK        } (KEY_CODE_LOCKING_NUMLOCK,      KEY_CODE_LOCKING_NUMLOCK,      KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_LOCKING_SCROLLLOCK     } (KEY_CODE_LOCKING_SCROLLLOCK,   KEY_CODE_LOCKING_SCROLLLOCK,   KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_KEYPAD_COMMA           } (KEY_CODE_COMMA,                KEY_CODE_COMMA,                KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_KEYPAD_EQUAL_SIGN      } (KEY_CODE_EQUALS,               KEY_CODE_EQUALS,               KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_INTERNATIONAL1         } (KEY_CODE_INTERNATIONAL1,       KEY_CODE_INTERNATIONAL1,       KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_INTERNATIONAL2         } (KEY_CODE_INTERNATIONAL2,       KEY_CODE_INTERNATIONAL2,       KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_INTERNATIONAL3         } (KEY_CODE_INTERNATIONAL3,       KEY_CODE_INTERNATIONAL3,       KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_INTERNATIONAL4         } (KEY_CODE_INTERNATIONAL4,       KEY_CODE_INTERNATIONAL4,       KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_INTERNATIONAL5         } (KEY_CODE_INTERNATIONAL5,       KEY_CODE_INTERNATIONAL5,       KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_INTERNATIONAL6         } (KEY_CODE_INTERNATIONAL6,       KEY_CODE_INTERNATIONAL6,       KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_INTERNATIONAL7         } (KEY_CODE_INTERNATIONAL7,       KEY_CODE_INTERNATIONAL7,       KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_INTERNATIONAL8         } (KEY_CODE_INTERNATIONAL8,       KEY_CODE_INTERNATIONAL8,       KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_INTERNATIONAL9         } (KEY_CODE_INTERNATIONAL9,       KEY_CODE_INTERNATIONAL9,       KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_LANG1                  } (KEY_CODE_LANG1,                KEY_CODE_LANG1,                KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_LANG2                  } (KEY_CODE_LANG2,                KEY_CODE_LANG2,                KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_LANG3                  } (KEY_CODE_LANG3,                KEY_CODE_LANG3,                KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_LANG4                  } (KEY_CODE_LANG4,                KEY_CODE_LANG4,                KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_LANG5                  } (KEY_CODE_LANG5,                KEY_CODE_LANG5,                KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_LANG6                  } (KEY_CODE_LANG6,                KEY_CODE_LANG6,                KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_LANG7                  } (KEY_CODE_LANG7,                KEY_CODE_LANG7,                KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_LANG8                  } (KEY_CODE_LANG8,                KEY_CODE_LANG8,                KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_LANG9                  } (KEY_CODE_LANG9,                KEY_CODE_LANG9,                KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_ALT_ERASE              } (KEY_CODE_ALT_ERASE,            KEY_CODE_ALT_ERASE,            KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_SYSREQ                 } (KEY_CODE_SYSREQ,               KEY_CODE_SYSREQ,               KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_CANCEL                 } (KEY_CODE_CANCEL,               KEY_CODE_CANCEL,               KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_CLEAR                  } (KEY_CODE_CLEAR,                KEY_CODE_CLEAR,                KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_PRIOR                  } (KEY_CODE_PRIOR,                KEY_CODE_PRIOR,                KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_RETURN                 } (KEY_CODE_RETURN,               KEY_CODE_RETURN,               KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_SEPARATOR              } (KEY_CODE_SEPARATOR,            KEY_CODE_SEPARATOR,            KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_OUT                    } (KEY_CODE_OUT,                  KEY_CODE_OUT,                  KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_OPER                   } (KEY_CODE_OPER,                 KEY_CODE_OPER,                 KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_CLEAR_AGAIN            } (KEY_CODE_CLEAR_AGAIN,          KEY_CODE_CLEAR_AGAIN,          KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_CRSEL_PROPS            } (KEY_CODE_CRSEL_PROPS,          KEY_CODE_CRSEL_PROPS,          KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_EXSEL                  } (KEY_CODE_EXSEL,                KEY_CODE_EXSEL,                KEY_CODE_NONE,                 KEY_CODE_NONE),
        {Codes 165 to 175 Reserved}         (KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE),
                                            (KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE),
                                            (KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE),
                                            (KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE),
                                            (KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE),
                                            (KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE),
                                            (KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE),
                                            (KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE),
                                            (KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE),
                                            (KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE),
                                            (KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_KEYPAD_00              } (KEY_CODE_00,                   KEY_CODE_00,                   KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_KEYPAD_000             } (KEY_CODE_000,                  KEY_CODE_000,                  KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_THOUSANDS_SEPARATOR    } (KEY_CODE_THOUSANDS_SEPARATOR,  KEY_CODE_THOUSANDS_SEPARATOR,  KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_DECIMAL_SEPARATOR      } (KEY_CODE_DECIMAL_SEPARATOR,    KEY_CODE_DECIMAL_SEPARATOR,    KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_CURRENCY_UNIT          } (KEY_CODE_CURRENCY_UNIT,        KEY_CODE_CURRENCY_UNIT,        KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_CURRENCY_SUBUNIT       } (KEY_CODE_CURRENCY_SUBUNIT,     KEY_CODE_CURRENCY_SUBUNIT,     KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_KEYPAD_LEFT_BRACKET    } (KEY_CODE_LEFT_BRACKET,         KEY_CODE_LEFT_BRACKET,         KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_KEYPAD_RIGHT_BRACKET   } (KEY_CODE_RIGHT_BRACKET,        KEY_CODE_RIGHT_BRACKET,        KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_KEYPAD_LEFT_BRACE      } (KEY_CODE_LEFT_BRACE,           KEY_CODE_LEFT_BRACE,           KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_KEYPAD_RIGHT_BRACE     } (KEY_CODE_RIGHT_BRACE,          KEY_CODE_RIGHT_BRACE,          KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_KEYPAD_TAB             } (KEY_CODE_TAB,                  KEY_CODE_TAB,                  KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_KEYPAD_BACKSPACE       } (KEY_CODE_BACKSPACE,            KEY_CODE_BACKSPACE,            KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_KEYPAD_A               } (KEY_CODE_CAPITAL_A,            KEY_CODE_CAPITAL_A,            KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_KEYPAD_B               } (KEY_CODE_CAPITAL_B,            KEY_CODE_CAPITAL_B,            KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_KEYPAD_C               } (KEY_CODE_CAPITAL_C,            KEY_CODE_CAPITAL_C,            KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_KEYPAD_D               } (KEY_CODE_CAPITAL_D,            KEY_CODE_CAPITAL_D,            KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_KEYPAD_E               } (KEY_CODE_CAPITAL_E,            KEY_CODE_CAPITAL_E,            KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_KEYPAD_F               } (KEY_CODE_CAPITAL_F,            KEY_CODE_CAPITAL_F,            KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_KEYPAD_XOR             } (KEY_CODE_XOR,                  KEY_CODE_XOR,                  KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_KEYPAD_CARET           } (KEY_CODE_CARET,                KEY_CODE_CARET,                KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_KEYPAD_PERCENT         } (KEY_CODE_PERCENT,              KEY_CODE_PERCENT,              KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_KEYPAD_LESSTHAN        } (KEY_CODE_LESSTHAN,             KEY_CODE_LESSTHAN,             KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_KEYPAD_GREATERTHAN     } (KEY_CODE_GREATERTHAN,          KEY_CODE_GREATERTHAN,          KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_KEYPAD_AMPERSAND       } (KEY_CODE_AMPERSAND,            KEY_CODE_AMPERSAND,            KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_KEYPAD_DOUBLE_AMPERSAND} (KEY_CODE_DOUBLE_AMPERSAND,     KEY_CODE_DOUBLE_AMPERSAND,     KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_KEYPAD_PIPE            } (KEY_CODE_PIPE,                 KEY_CODE_PIPE,                 KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_KEYPAD_DOUBLE_PIPE     } (KEY_CODE_DOUBLE_PIPE,          KEY_CODE_DOUBLE_PIPE,          KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_KEYPAD_COLON           } (KEY_CODE_COLON,                KEY_CODE_COLON,                KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_KEYPAD_NUMBER          } (KEY_CODE_NUMBER,               KEY_CODE_NUMBER,               KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_KEYPAD_SPACE           } (KEY_CODE_SPACE,                KEY_CODE_SPACE,                KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_KEYPAD_AT              } (KEY_CODE_AT,                   KEY_CODE_AT,                   KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_KEYPAD_EXCLAMATION     } (KEY_CODE_EXCLAMATION,          KEY_CODE_EXCLAMATION,          KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_KEYPAD_MEM_STORE       } (KEY_CODE_MEM_STORE,            KEY_CODE_MEM_STORE,            KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_KEYPAD_MEM_RECALL      } (KEY_CODE_MEM_RECALL,           KEY_CODE_MEM_RECALL,           KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_KEYPAD_MEM_CLEAR       } (KEY_CODE_MEM_CLEAR,            KEY_CODE_MEM_CLEAR,            KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_KEYPAD_MEM_ADD         } (KEY_CODE_MEM_ADD,              KEY_CODE_MEM_ADD,              KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_KEYPAD_MEM_SUB         } (KEY_CODE_MEM_SUBTRACT,         KEY_CODE_MEM_SUBTRACT,         KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_KEYPAD_MEM_MULTIPLY    } (KEY_CODE_MEM_MULTIPLY,         KEY_CODE_MEM_MULTIPLY,         KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_KEYPAD_MEM_DIVIDE      } (KEY_CODE_MEM_DIVIDE,           KEY_CODE_MEM_DIVIDE,           KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_KEYPAD_PLUS_MINUS      } (KEY_CODE_PLUS_MINUS,           KEY_CODE_PLUS_MINUS,           KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_KEYPAD_CLEAR           } (KEY_CODE_CLEAR,                KEY_CODE_CLEAR,                KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_KEYPAD_CLEAR_ENTRY     } (KEY_CODE_CLEAR_ENTRY,          KEY_CODE_CLEAR_ENTRY,          KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_KEYPAD_BINARY          } (KEY_CODE_BINARY,               KEY_CODE_BINARY,               KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_KEYPAD_OCTAL           } (KEY_CODE_OCTAL,                KEY_CODE_OCTAL,                KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_KEYPAD_DECIMAL         } (KEY_CODE_DECIMAL,              KEY_CODE_DECIMAL,              KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_KEYPAD_HEX             } (KEY_CODE_HEX,                  KEY_CODE_HEX,                  KEY_CODE_NONE,                 KEY_CODE_NONE),
        {Codes 222 to 223 Reserved}         (KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE),
                                            (KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_LEFT_CTRL              } (KEY_CODE_CTRL,                 KEY_CODE_CTRL,                 KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_LEFT_SHIFT             } (KEY_CODE_SHIFT,                KEY_CODE_SHIFT,                KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_LEFT_ALT               } (KEY_CODE_ALT,                  KEY_CODE_ALT,                  KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_LEFT_GUI               } (KEY_CODE_GUI,                  KEY_CODE_GUI,                  KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_RIGHT_CTRL             } (KEY_CODE_CTRL,                 KEY_CODE_CTRL,                 KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_RIGHT_SHIFT            } (KEY_CODE_SHIFT,                KEY_CODE_SHIFT,                KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_RIGHT_ALT              } (KEY_CODE_ALT,                  KEY_CODE_ALT,                  KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_RIGHT_GUI              } (KEY_CODE_GUI,                  KEY_CODE_GUI,                  KEY_CODE_NONE,                 KEY_CODE_NONE),
        {Codes 232 to 255 Reserved}         (KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE),
                                            (KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE),
                                            (KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE),
                                            (KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE),
                                            (KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE),
                                            (KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE),
                                            (KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE),
                                            (KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE),
                                            (KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE),
                                            (KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE),
                                            (KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE),
                                            (KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE),
                                            (KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE),
                                            (KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE),
                                            (KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE),
                                            (KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE),
                                            (KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE),
                                            (KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE),
                                            (KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE),
                                            (KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE),
                                            (KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE),
                                            (KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE),
                                            (KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE),
                                            (KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE))
  );
 
{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure KeymapInit;
{Initialize the keymap unit, keymap table and default keymap}

{Note: Called only during system startup}
begin
 {}
 {Check Initialized}
 if KeymapInitialized then Exit;
 
 {Initialize Keymap Table}
 KeymapTable:=nil;
 KeymapTableLock:=CriticalSectionCreate; 
 KeymapTableCount:=0;
 if KeymapTableLock = INVALID_HANDLE_VALUE then
  begin
   if PLATFORM_LOG_ENABLED then PlatformLogError('Failed to create keymap table lock');
  end;
 KeymapDefault:=INVALID_HANDLE_VALUE;
 
 {Load Default Keymap}
 KeymapDefault:=KeymapLoad(@KEYMAP_US_ENGLISH_HEADER,@KEYMAP_US_ENGLISH_DATA,SizeOf(TKeymapData));
 if KeymapDefault = INVALID_HANDLE_VALUE then
  begin
   if PLATFORM_LOG_ENABLED then PlatformLogError('Failed to load default keymap');
  end;
 
 KeymapInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{Keymap Functions}
function KeymapLoad(Header:PKeymapHeader;Data:PKeymapData;Size:LongWord):TKeymapHandle;
{Load a Keymap from a keymap data block and add to the Keymap table}
{Header: Pointer to the keymap header}
{Data: Pointer to the keymap data}
{Size: Size of the keymap data}
{Return: Handle of the newly loaded keymap or INVALID_HANDLE_VALUE on failure}
begin
 {}
 Result:=KeymapLoadEx(Header,Data,nil,nil,Size,nil);
end;

{==============================================================================}

function KeymapLoadEx(Header:PKeymapHeader;Data:PKeymapData;Capskeys:PKeymapCapskeys;Deadkeys:PKeymapDeadkeys;Size:LongWord;Properties:PKeymapProperties):TKeymapHandle;
{Load a Keymap from a keymap data block and add to the Keymap table}
{Header: Pointer to the keymap header (See TKeymapHeader)}
{Data: Pointer to the keymap data (See TKeymapData)}
{Capskeys: Pointer to the capskeys table (Optional)}
{Deadkeys: Pointer to the deadkeys table (Optional)}
{Size: Size of the keymap data}
{Properties: Pointer to a keymap properties record to use instead of the header (Optional)}
{Return: Handle of the newly loaded keymap or INVALID_HANDLE_VALUE on failure}
var
 Keymap:PKeymapEntry;
 TotalSize:LongWord;
begin
 {}
 Result:=INVALID_HANDLE_VALUE;

 {Check Data}
 if Data = nil then Exit;
 
 {Check Header}
 if (Header = nil) and (Properties = nil) then Exit;
 
 {Check Capskeys}
 {if Capskeys = nil then Exit;} {May be nil}

 {Check Deadkeys}
 {if Deadkeys = nil then Exit;} {May be nil}
 
 {Check Properties}
 if Properties = nil then
  begin
   {Check Count}
   if Header.KeyCount <> KEYMAP_KEY_COUNT then Exit;
   if Header.RowCount <> KEYMAP_ROW_COUNT then Exit;
   
   {Get Size}
   TotalSize:=Header.KeyCount * (Header.RowCount * KEYMAP_ROW_SIZE); {Default 2K}
   
   {Check Size}
   if Size < TotalSize then Exit;
   
   {Create Keymap}
   Keymap:=PKeymapEntry(AllocMem(SizeOf(TKeymapEntry)));
   if Keymap = nil then Exit;
   
   {Update Keymap}
   Keymap.Signature:=KEYMAP_SIGNATURE;
   Keymap.KeymapMode:=Header.Mode;
   Keymap.KeymapFlags:=Header.Flags;
   Keymap.KeymapName:=Header.Name;
   Keymap.KeymapDescription:=Header.Description;
   Keymap.KeyData:=nil;
   Keymap.KeyCount:=Header.KeyCount;
   Keymap.RowCount:=Header.RowCount;
   Keymap.CapskeysData:=nil;
   Keymap.DeadkeysData:=nil;
  end
 else
  begin 
   {Check Count}
   if Properties.KeyCount <> KEYMAP_KEY_COUNT then Exit;
   if Properties.RowCount <> KEYMAP_ROW_COUNT then Exit;

   {Get Size}
   TotalSize:=Properties.KeyCount * (Properties.RowCount * KEYMAP_ROW_SIZE); {Default 2K}
   
   {Check Size}
   if Size < TotalSize then Exit;

   {Create Keymap}
   Keymap:=PKeymapEntry(AllocMem(SizeOf(TKeymapEntry)));
   if Keymap = nil then Exit;
   
   {Update Keymap}
   Keymap.Signature:=KEYMAP_SIGNATURE;
   Keymap.KeymapMode:=Properties.KeymapMode;
   Keymap.KeymapFlags:=Properties.KeymapFlags;
   Keymap.KeymapName:=Properties.KeymapName;
   Keymap.KeymapDescription:=Properties.KeymapDescription;
   Keymap.KeyData:=nil;
   Keymap.KeyCount:=Properties.KeyCount;
   Keymap.RowCount:=Properties.RowCount;
   Keymap.CapskeysData:=nil;
   Keymap.DeadkeysData:=nil;
  end;
 
 {Update Keymap}
 UniqueString(Keymap.KeymapName);
 UniqueString(Keymap.KeymapDescription);
 Keymap.KeyData:=GetMem(TotalSize);
 if Keymap.KeyData = nil then
  begin
   FreeMem(Keymap);
   Exit;
  end;
  
 {Copy Data}
 System.Move(PKeymapChars(Data)[0],Keymap.KeyData^,TotalSize);
 
 {Copy Capskeys}
 if Capskeys <> nil then
  begin
   Keymap.CapskeysData:=GetMem(SizeOf(TKeymapCapskeys));
   if Keymap.CapskeysData = nil then
    begin
     {Free Keys}
     FreeMem(Keymap.KeyData);
    
     FreeMem(Keymap);
     Exit;
    end;
  
   System.Move(Capskeys^,Keymap.CapskeysData^,SizeOf(TKeymapCapskeys));
  end;
 
 {Copy Deadkeys}
 if Deadkeys <> nil then
  begin
   Keymap.DeadkeysData:=GetMem(SizeOf(TKeymapDeadkeys));
   if Keymap.DeadkeysData = nil then
    begin
     {Free Capskeys data}
     if Keymap.CapskeysData <> nil then
      begin
       FreeMem(Keymap.CapskeysData);
      end;
     
     {Free Keys}
     FreeMem(Keymap.KeyData);
    
     FreeMem(Keymap);
     Exit;
    end;
   
   System.Move(Deadkeys^,Keymap.DeadkeysData^,SizeOf(TKeymapDeadkeys));
  end; 
  
 {Insert Keymap}
 if CriticalSectionLock(KeymapTableLock) = ERROR_SUCCESS then
  begin
   try
    {Link Keymap}
    if KeymapTable = nil then
     begin
      KeymapTable:=Keymap;
     end
    else
     begin
      Keymap.Next:=KeymapTable;
      KeymapTable.Prev:=Keymap;
      KeymapTable:=Keymap;
     end;
 
    {Increment Count}
    Inc(KeymapTableCount);
    
    {Check Default}
    if KeymapDefault = INVALID_HANDLE_VALUE then
     begin
      KeymapDefault:=TKeymapHandle(Keymap);
     end;
     
    {Return Result}
    Result:=TKeymapHandle(Keymap);
   finally
    CriticalSectionUnlock(KeymapTableLock);
   end;
  end
 else
  begin
   {Free Deadkeys data}
   if Keymap.DeadkeysData <> nil then
    begin
     FreeMem(Keymap.DeadkeysData);
    end;

   {Free Capskeys data}
   if Keymap.CapskeysData <> nil then
    begin
     FreeMem(Keymap.CapskeysData);
    end;
   
   {Free Keys}
   FreeMem(Keymap.KeyData);
   
   {Free Keymap}
   FreeMem(Keymap);
  end;  
end;
  
{==============================================================================}

function KeymapUnload(Handle:TKeymapHandle):LongWord;
{Unload an existing keymap and remove from the Keymap table}
{Handle: The handle of the keymap to unload}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Prev:PKeymapEntry;
 Next:PKeymapEntry;
 Keymap:PKeymapEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;
 
 {Get Keymap}
 Keymap:=PKeymapEntry(Handle);
 if Keymap = nil then Exit;
 if Keymap.Signature <> KEYMAP_SIGNATURE then Exit;
 
 {Check Keymap}
 Result:=ERROR_NOT_FOUND;
 if KeymapCheck(Keymap) <> Keymap then Exit;
 
 {Remove Keymap}
 if CriticalSectionLock(KeymapTableLock) = ERROR_SUCCESS then
  begin
   try
    {Unlink Keymap}
    Prev:=Keymap.Prev;
    Next:=Keymap.Next;
    if Prev = nil then
     begin
      KeymapTable:=Next;
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
    Dec(KeymapTableCount);
 
    {Check Default}
    if KeymapDefault = Handle then
     begin
      KeymapDefault:=TKeymapHandle(KeymapTable);
     end;
     
    {Update Keymap}
    Keymap.Signature:=0;

    {Free Deadkeys data}
    if Keymap.DeadkeysData <> nil then
     begin
      FreeMem(Keymap.DeadkeysData);
     end;
    
    {Free Capskeys data}
    if Keymap.CapskeysData <> nil then
     begin
      FreeMem(Keymap.CapskeysData);
     end;
 
    {Free Keys}
    FreeMem(Keymap.KeyData);
   
    {Free Keymap}
    FreeMem(Keymap);
 
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    CriticalSectionUnlock(KeymapTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;

{==============================================================================}

function KeymapGetName(Handle:TKeymapHandle):String;
{Get the name of the specified keymap}
{Handle: The handle of the keymap to get the name for}
{Return: The name of the keymap (eg US)}
var
 Keymap:PKeymapEntry;
begin
 {}
 Result:='';
 
 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;
 
 {Get Keymap}
 Keymap:=PKeymapEntry(Handle);
 if Keymap = nil then Exit;
 if Keymap.Signature <> KEYMAP_SIGNATURE then Exit;
 
 {Acquire Lock}
 if CriticalSectionLock(KeymapTableLock) <> ERROR_SUCCESS then Exit;

 {Get Name}
 Result:=Keymap.KeymapName;
    
 UniqueString(Result);

 {Release Lock}
 CriticalSectionUnlock(KeymapTableLock);
end;

{==============================================================================}

function KeymapGetDescription(Handle:TKeymapHandle):String;
{Get the description of the specified keymap}
{Handle: The handle of the keymap to get the description for}
{Return: The description of the keymap (eg US English)}
var
 Keymap:PKeymapEntry;
begin
 {}
 Result:='';
 
 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;
 
 {Get Keymap}
 Keymap:=PKeymapEntry(Handle);
 if Keymap = nil then Exit;
 if Keymap.Signature <> KEYMAP_SIGNATURE then Exit;
 
 {Acquire Lock}
 if CriticalSectionLock(KeymapTableLock) <> ERROR_SUCCESS then Exit;

 {Get Description}
 Result:=Keymap.KeymapDescription;
    
 UniqueString(Result);

 {Release Lock}
 CriticalSectionUnlock(KeymapTableLock);
end;

{==============================================================================}

function KeymapCheckFlag(Handle:TKeymapHandle;Flag:LongWord):Boolean;
{Check if a specified keymap has a particular flag set or not}
{Handle: The handle of the keymap to check the flag for}
{Flag: The flag value to check (eg KEYMAP_FLAG_CAPS_ASCII)}
{Return: True if the flag is set and False if not set}
var
 Keymap:PKeymapEntry;
begin
 {}
 Result:=False;
 
 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;
 
 {Get Keymap}
 Keymap:=PKeymapEntry(Handle);
 if Keymap = nil then Exit;
 if Keymap.Signature <> KEYMAP_SIGNATURE then Exit;
 
 {Acquire Lock}
 if CriticalSectionLock(KeymapTableLock) <> ERROR_SUCCESS then Exit;
 
 {Check Flag}
 Result:=((Keymap.KeymapFlags and Flag) <> 0);
 
 {Release Lock}
 CriticalSectionUnlock(KeymapTableLock);
end;

{==============================================================================}

function KeymapGetKeyCode(Handle:TKeymapHandle;ScanCode:Word;Index:Byte):Word;
{Resolve a scan code and index value to a key code using the specified keymap}
{Handle: The handle of the keymap to use for translation}
{ScanCode: The keyboard scan code value to resolve (eg SCAN_CODE_A)}
{Index: The keymap index to use for the translation (eg KEYMAP_INDEX_SHIFT)}
{Return: The translated key code value (eg KEY_CODE_A) or KEY_CODE_NONE on failure}
var
 Keymap:PKeymapEntry;
begin
 {}
 Result:=KEY_CODE_NONE;
 
 {Check Scan Code}
 if ScanCode = SCAN_CODE_NONE then Exit;
 
 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;
 
 {Get Keymap}
 Keymap:=PKeymapEntry(Handle);
 if Keymap = nil then Exit;
 if Keymap.Signature <> KEYMAP_SIGNATURE then Exit;
 
 {Acquire Lock}
 if CriticalSectionLock(KeymapTableLock) = ERROR_SUCCESS then
  begin
   try
    {Check ScanCode}
    if ScanCode >= Keymap.KeyCount then Exit;
    
    {Check Index}
    if Index >= Keymap.RowCount then Exit;
    
    {Get KeyCode}
    Result:=PKeymapData(Keymap.KeyData).Data[ScanCode,Index];
   finally
    CriticalSectionUnlock(KeymapTableLock);
   end;
  end;
end;
   
{==============================================================================}
   
function KeymapGetCharCode(Handle:TKeymapHandle;KeyCode:Word):Char;
{Resolve a key code value to an ANSI character code using the specified keymap}
{Handle: The handle of the keymap to use for translation}
{KeyCode: The key code value to resolve (eg KEY_CODE_A)}
{Return: The ANSI character value for the keycode or 0 on failure}
begin
 {}
 Result:=#0;

 {Check Key Code}
 if KeyCode = KEY_CODE_NONE then Exit;
 
 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;
 
 {Do not lock}
 
 {Check KeyCode}
 if KeyCode < KEY_CODE_TRANSLATE_START then
  begin
   {Get CharCode (No Translation)}
   Result:=Chr(KeyCode);
  end
 else
  begin 
   {Check Private Area}
   if (KeyCode < KEY_CODE_PRIVATE_START) or (KeyCode > KEY_CODE_PRIVATE_END) then
    begin
     {Get CharCode (Translated)}
     Result:=WideCharToCodePage(WideChar(KeyCode));
    end;
  end; 
end;
 
{==============================================================================}

function KeymapGetCharUnicode(Handle:TKeymapHandle;KeyCode:Word):WideChar;
{Resolve a key code value to a Unicode character code using the specified keymap}
{Handle: The handle of the keymap to use for translation}
{KeyCode: The key code value to resolve (eg KEY_CODE_A)}
{Return: The Unicode character value for the keycode or 0 on failure}
begin
 {}
 Result:=#0;

 {Check Key Code}
 if KeyCode = KEY_CODE_NONE then Exit;
 
 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;
 
 {Do not lock}
 
 {Check Private Area}
 if (KeyCode < KEY_CODE_PRIVATE_START) or (KeyCode > KEY_CODE_PRIVATE_END) then
  begin
   {Get CharCode (No Translation)}
   Result:=WideChar(KeyCode);
  end;
end;
   
{==============================================================================}
   
function KeymapCheckCapskey(Handle:TKeymapHandle;ScanCode:Word):Boolean;
{Check if a scan code is affected by the Caps Lock key in the specified keymap}
{Handle: The handle of the keymap to check}
{ScanCode: The scan code value to check (eg SCAN_CODE_A)}
{Return: True if affected by Caps Lock, False if not}
var
 Count:LongWord;
 Keymap:PKeymapEntry;
begin
 {}
 Result:=False;
 
 {Check Scan Code}
 if ScanCode = SCAN_CODE_NONE then Exit;
 
 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;
 
 {Get Keymap}
 Keymap:=PKeymapEntry(Handle);
 if Keymap = nil then Exit;
 if Keymap.Signature <> KEYMAP_SIGNATURE then Exit;
 
 {Acquire Lock}
 if CriticalSectionLock(KeymapTableLock) = ERROR_SUCCESS then
  begin
   try
    {Check Flags}
    if (Keymap.KeymapFlags and KEYMAP_FLAG_CAPS_ALL) <> 0 then
     begin
      Result:=True;
     end
    else if (Keymap.KeymapFlags and KEYMAP_FLAG_CAPS_ASCII) <> 0 then  
     begin
      {Check for ASCII (A to Z)}
      if (ScanCode >= SCAN_CODE_A) and (ScanCode <= SCAN_CODE_Z) then
       begin
        Result:=True;
       end;
     end
    else
     begin    
      {Check Capskeys}
      if (Keymap.CapskeysData <> nil) and (Keymap.CapskeysData.Count > 0) then
       begin
        for Count:=0 to Keymap.CapskeysData.Count - 1 do
         begin
          {Check First and Last}
          if (Keymap.CapskeysData.Keys[Count].First <> SCAN_CODE_NONE) and (Keymap.CapskeysData.Keys[Count].Last <> SCAN_CODE_NONE) then
           begin
            if (ScanCode >= Keymap.CapskeysData.Keys[Count].First) and (ScanCode <= Keymap.CapskeysData.Keys[Count].Last) then
             begin
              Result:=True;
              Exit;
             end; 
           end; 
         end;
       end;
     end;  
   finally
    CriticalSectionUnlock(KeymapTableLock);
   end;
  end;
end;
   
{==============================================================================}

function KeymapCheckDeadkey(Handle:TKeymapHandle;ScanCode:Word;Index:Byte):Boolean;
{Check if a scan code represents a Dead Key in the specified keymap}
{Handle: The handle of the keymap to check}
{ScanCode: The scan code value to check (eg SCAN_CODE_A)}
{Index: The keymap index to check (eg KEYMAP_INDEX_SHIFT)}
{Return: True if the scan key is a Dead Key, False if not}
var
 Count:LongWord;
 Keymap:PKeymapEntry;
begin
 {}
 Result:=False;
 
 {Check Scan Code}
 if ScanCode = SCAN_CODE_NONE then Exit;
 
 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;
 
 {Get Keymap}
 Keymap:=PKeymapEntry(Handle);
 if Keymap = nil then Exit;
 if Keymap.Signature <> KEYMAP_SIGNATURE then Exit;
 
 {Acquire Lock}
 if CriticalSectionLock(KeymapTableLock) = ERROR_SUCCESS then
  begin
   try
    {Check Deadkeys}
    if (Keymap.DeadkeysData <> nil) and (Keymap.DeadkeysData.Count > 0) then
     begin
      for Count:=0 to Keymap.DeadkeysData.Count - 1 do
       begin
        {Check Key and Index}
        if (Keymap.DeadkeysData.Keys[Count].Key = ScanCode) and (Keymap.DeadkeysData.Keys[Count].Index = Index) then
         begin
          Result:=True;
          Exit;
         end;
       end; 
     end; 
   finally
    CriticalSectionUnlock(KeymapTableLock);
   end;
  end;
end;

{==============================================================================}

function KeymapResolveDeadkey(Handle:TKeymapHandle;DeadCode,ScanCode:Word;DeadIndex,ScanIndex:Byte;var KeyCode:Word):Boolean;
{Resolve a Dead Key and the next scan code to a key code value}
{Handle: The handle of the keymap to use for resolution}
{DeadCode: The scan code value of the dead key (eg SCAN_CODE_GRAVE)}
{ScanCode: The scan code value of the next key (eg (SCAN_CODE_A)}
{DeadIndex: The keymap index of the dead key (eg KEYMAP_INDEX_SHIFT)}
{ScanIndex: The keymap index of the next key (eg KEYMAP_INDEX_SHIFT)}
{KeyCode: Return value for the key code represented by the dead key / next key combination (or KEY_CODE_NONE)}
{Return: True if the dead key / next key combination resolves to a key code or False if not}
var
 Count:LongWord;
 Counter:LongWord;
 Keymap:PKeymapEntry;
begin
 {}
 {Setup KeyCode}
 KeyCode:=KEY_CODE_NONE;
 Result:=False;
 
 {Check Dead Code}
 if DeadCode = SCAN_CODE_NONE then Exit;
 
 {Check Scan Code}
 if ScanCode = SCAN_CODE_NONE then Exit;
 
 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;
 
 {Get Keymap}
 Keymap:=PKeymapEntry(Handle);
 if Keymap = nil then Exit;
 if Keymap.Signature <> KEYMAP_SIGNATURE then Exit;
 
 {Acquire Lock}
 if CriticalSectionLock(KeymapTableLock) = ERROR_SUCCESS then
  begin
   try
    {Check Deadkeys}
    if Keymap.DeadkeysData <> nil then
     begin
      for Count:=0 to Keymap.DeadkeysData.Count - 1 do
       begin
        {Check Key and Index}
        if (Keymap.DeadkeysData.Keys[Count].Key = DeadCode) and (Keymap.DeadkeysData.Keys[Count].Index = DeadIndex) then
         begin
          Counter:=0;
          while Keymap.DeadkeysData.Keys[Count].Resolves[Counter].Key <> SCAN_CODE_NONE do
           begin
            {Check Resolve Key and Index}
            if (Keymap.DeadkeysData.Keys[Count].Resolves[Counter].Key = ScanCode) and (Keymap.DeadkeysData.Keys[Count].Resolves[Counter].Index = ScanIndex) then
             begin
              {Return Key Code}
              KeyCode:=Keymap.DeadkeysData.Keys[Count].Resolves[Counter].Code;
              Result:=True;
              Exit;
             end;
            
            Inc(Counter);
           end; 
          
          Exit;
         end; 
       end;  
     end; 
   finally
    CriticalSectionUnlock(KeymapTableLock);
   end;
  end;
end;

{==============================================================================}

function KeymapGetProperties(Handle:TKeymapHandle;Properties:PKeymapProperties):LongWord;
{Get the properties of the specified keymap}
{Handle: The handle of the keymap to get the properties for}
{Properties: Pointer to a keymap properties structure to return the properties}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Keymap:PKeymapEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {Check Properties}
 if Properties = nil then Exit;
 
 {Get Keymap}
 Keymap:=PKeymapEntry(Handle);
 if Keymap = nil then Exit;
 if Keymap.Signature <> KEYMAP_SIGNATURE then Exit;
 
 {Acquire Lock}
 if CriticalSectionLock(KeymapTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Properties}
    Properties.KeymapMode:=Keymap.KeymapMode;
    Properties.KeymapFlags:=Keymap.KeymapFlags;
    Properties.KeymapName:=Keymap.KeymapName;
    Properties.KeymapDescription:=Keymap.KeymapDescription;
    
    UniqueString(Properties.KeymapName);
    UniqueString(Properties.KeymapDescription);
    
    Result:=ERROR_SUCCESS;
   finally
    CriticalSectionUnlock(KeymapTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;

{==============================================================================}

function KeymapFindByName(const Name:String):TKeymapHandle; 
{Find a keymap by name}
{Name: The name of the keymap to find (eg US)}
{Return: The handle of the matching keymap or INVALID_HANDLE_VALUE if not found}
var
 Keymap:PKeymapEntry;
begin
 {}
 Result:=INVALID_HANDLE_VALUE;
 
 {Acquire Lock}
 if CriticalSectionLock(KeymapTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Keymap}
    Keymap:=KeymapTable;
    while Keymap <> nil do
     begin
      {Check State}
      if Keymap.Signature = KEYMAP_SIGNATURE then
       begin
        {Check Name}
        if Uppercase(Keymap.KeymapName) = Uppercase(Name) then
         begin
          Result:=TKeymapHandle(Keymap);
          Exit;
         end;
       end;
       
      {Get Next}
      Keymap:=Keymap.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(KeymapTableLock);
   end;
  end;
end;

{==============================================================================}

function KeymapFindByDescription(const Description:String):TKeymapHandle; 
{Find a keymap by description}
{Description: The description of the keymap to find (eg US English)}
{Return: The handle of the matching keymap or INVALID_HANDLE_VALUE if not found}
var
 Keymap:PKeymapEntry;
begin
 {}
 Result:=INVALID_HANDLE_VALUE;
 
 {Acquire Lock}
 if CriticalSectionLock(KeymapTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Keymap}
    Keymap:=KeymapTable;
    while Keymap <> nil do
     begin
      {Check State}
      if Keymap.Signature = KEYMAP_SIGNATURE then
       begin
        {Check Description}
        if Uppercase(Keymap.KeymapDescription) = Uppercase(Description) then
         begin
          Result:=TKeymapHandle(Keymap);
          Exit;
         end;
       end;
       
      {Get Next}
      Keymap:=Keymap.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(KeymapTableLock);
   end;
  end;
end;

{==============================================================================}

function KeymapEnumerate(Callback:TKeymapEnumerate;Data:Pointer):LongWord;
{Enumerate all loaded keymaps}
{Callback: The function to call for each loaded keymap}
{Data: A private data pointer to pass to callback for each loaded keymap}
var
 Keymap:PKeymapEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Callback}
 if not Assigned(Callback) then Exit;

 {Acquire Lock}
 if CriticalSectionLock(KeymapTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Keymap}
    Keymap:=KeymapTable;
    while Keymap <> nil do
     begin
      {Check State}
      if Keymap.Signature = KEYMAP_SIGNATURE then
       begin
        if Callback(TKeymapHandle(Keymap),Data) <> ERROR_SUCCESS then Exit;
       end;
       
      {Get Next}
      Keymap:=Keymap.Next;
     end;
    
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    CriticalSectionUnlock(KeymapTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;  
end;

{==============================================================================}
{==============================================================================}
{Keymap Helper Functions}
function KeymapGetCount:LongWord; inline;
{Get the current keymap count}
begin
 {}
 Result:=KeymapTableCount;
end;

{==============================================================================}

function KeymapGetDefault:TKeymapHandle; inline;
{Get the current default keymap}
begin
 {}
 Result:=KeymapDefault;
end;

{==============================================================================}

function KeymapSetDefault(Handle:TKeymapHandle):LongWord; 
{Set the current default keymap}
var
 Keymap:PKeymapEntry;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;
 
 {Get Keymap}
 Keymap:=PKeymapEntry(Handle);
 if Keymap = nil then Exit;
 if Keymap.Signature <> KEYMAP_SIGNATURE then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(KeymapTableLock) = ERROR_SUCCESS then
  begin
   try
    {Check Keymap}
    if KeymapCheck(Keymap) <> Keymap then Exit;
    
    {Set Keymap Default}
    KeymapDefault:=TKeymapHandle(Keymap);
    
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    CriticalSectionUnlock(KeymapTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function KeymapCheck(Keymap:PKeymapEntry):PKeymapEntry;
{Check if the supplied Keymap is in the Keymap table}
var
 Current:PKeymapEntry;
begin
 {}
 Result:=nil;
 
 {Check Keymap}
 if Keymap = nil then Exit;
 if Keymap.Signature <> KEYMAP_SIGNATURE then Exit;
 
 {Acquire the Lock}
 if CriticalSectionLock(KeymapTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Keymap}
    Current:=KeymapTable;
    while Current <> nil do
     begin
      {Check Keymap}
      if Current = Keymap then
       begin
        Result:=Keymap;
        Exit;
       end;
      
      {Get Next}
      Current:=Current.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(KeymapTableLock);
   end;
  end;
end;

{==============================================================================}
{==============================================================================}

initialization
 KeymapInit;

{==============================================================================}
 
finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
