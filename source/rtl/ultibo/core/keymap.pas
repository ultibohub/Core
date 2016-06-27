{
Ultibo Keymap interface unit.

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


Keymaps
=======

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
 KEYMAP_ROW_COUNT = 4;     {Number of mapping rows per key in a keymap entry (Plain, Shift, AltGr, Shift_AltGr)}
 KEYMAP_ROW_SIZE  = SizeOf(Word);
 
 {Keymap Signature}
 KEYMAP_SIGNATURE = $863FDBA1;
 
 {Keymap Mode constants}
 KEYMAP_MODE_NONE = 0;
 
 {Keymap Flag constants}
 KEYMAP_FLAG_NONE = $00000000;
             
{==============================================================================}
type
 {Keymap specific types}

 {Keymap Header}
 PKeymapHeader = ^TKeymapHeader;
 TKeymapHeader = record
  Mode:LongWord;           {Keymap mode}
  Flags:LongWord;          {Keymap flags}
  Name:String[255];        {Keymap name}
  Description:String[255]; {Keymap description}
 end;
 
 {Keymap Data}
 PKeymapData = ^TKeymapData;
 TKeymapData = record
  Data:array[0..255,0..3] of Word; {Key mapping data, 4 words for Plain, Shift, AltGr, Shift+AltGr for each of the 256 keys (See KEY_CODE_* constants)}
 end;
 
 {Keymap Chars}
 PKeymapChars = ^TKeymapChars; 
 TKeymapChars = array[0..0] of Word;
 
 {Keymap Properties}
 PKeymapProperties = ^TKeymapProperties;
 TKeymapProperties = record
  KeymapMode:LongWord;           {Keymap mode}
  KeymapFlags:LongWord;          {Keymap flags}
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
  KeymapMode:LongWord;           {Keymap mode}
  KeymapFlags:LongWord;          {Keymap flags}
  KeymapName:String;             {Keymap name}
  KeymapDescription:String;      {Keymap description}
  {Driver Properties}
  KeyData:Pointer;               {Keymap key data}
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
function KeymapLoadEx(Header:PKeymapHeader;Data:PKeymapData;Size:LongWord;Properties:PKeymapProperties):TKeymapHandle;
function KeymapUnload(Handle:TKeymapHandle):LongWord;

function KeymapGetName(Handle:TKeymapHandle):String;
function KeymapGetDescription(Handle:TKeymapHandle):String;

function KeymapGetProperties(Handle:TKeymapHandle;Properties:PKeymapProperties):LongWord;

function KeymapFindByName(const Name:String):TKeymapHandle; 
function KeymapFindByDescription(const Description:String):TKeymapHandle; 
function KeymapEnumerate(Callback:TKeymapEnumerate;Data:Pointer):LongWord;

{==============================================================================}
{Keymap Helper Functions}
function KeymapGetCount:LongWord; inline;
function KeymapGetDefault:TKeymapHandle; inline;
function KeymapSetDefault(Keymap:PKeymapEntry):LongWord; 

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
  Flags:KEYMAP_FLAG_NONE;
  Name:('US-English');
  Description:('US English')
  );
 
 KEYMAP_US_ENGLISH_DATA:TKeymapData = (
        {Scan Code mappings:                 Plain                          Shift                          AltGr                          Shift+AltGr  }
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
        {SCAN_CODE_KEYPAD_1               } (KEY_CODE_1,                    KEY_CODE_END,                  KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_KEYPAD_2               } (KEY_CODE_2,                    KEY_CODE_DOWN_ARROW,           KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_KEYPAD_3               } (KEY_CODE_3,                    KEY_CODE_PAGEDN,               KEY_CODE_NONE,                 KEY_CODE_NONE),             
        {SCAN_CODE_KEYPAD_4               } (KEY_CODE_4,                    KEY_CODE_LEFT_ARROW,           KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_KEYPAD_5               } (KEY_CODE_5,                    KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_KEYPAD_6               } (KEY_CODE_6,                    KEY_CODE_RIGHT_ARROW,          KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_KEYPAD_7               } (KEY_CODE_7,                    KEY_CODE_HOME,                 KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_KEYPAD_8               } (KEY_CODE_8,                    KEY_CODE_UP_ARROW,             KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_KEYPAD_9               } (KEY_CODE_9,                    KEY_CODE_PAGEUP,               KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_KEYPAD_0               } (KEY_CODE_0,                    KEY_CODE_INSERT,               KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_KEYPAD_PERIOD          } (KEY_CODE_PERIOD,               KEY_CODE_DELETE,               KEY_CODE_NONE,                 KEY_CODE_NONE),
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
begin
 {}
 Result:=KeymapLoadEx(Header,Data,Size,nil);
end;

{==============================================================================}

function KeymapLoadEx(Header:PKeymapHeader;Data:PKeymapData;Size:LongWord;Properties:PKeymapProperties):TKeymapHandle;
{Load a Keymap from a keymap data block and add to the Keymap table}
{Header: Pointer to the keymap header}
{Data: Pointer to the keymap data}
{Size: Size of the keymap data}
{Properties: Pointer to a keymap properties record to use instead of the header (Optional)}
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
 
 {Check Properties}
 if Properties = nil then
  begin
   {Get Size}
   TotalSize:=KEYMAP_KEY_COUNT * (KEYMAP_ROW_COUNT * KEYMAP_ROW_SIZE); {Default 2K}
   
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
  end
 else
  begin 
   {Get Size}
   TotalSize:=KEYMAP_KEY_COUNT * (KEYMAP_ROW_COUNT * KEYMAP_ROW_SIZE); {Default 2K}
   
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
   {Free Keys}
   FreeMem(Keymap.KeyData);
   
   {Free Keymap}
   FreeMem(Keymap);
  end;  
end;
  
{==============================================================================}

function KeymapUnload(Handle:TKeymapHandle):LongWord;
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
 if CriticalSectionLock(KeymapTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Name}
    Result:=Keymap.KeymapName;
    
    UniqueString(Result);
   finally
    CriticalSectionUnlock(KeymapTableLock);
   end;
  end;
end;

{==============================================================================}

function KeymapGetDescription(Handle:TKeymapHandle):String;
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
 if CriticalSectionLock(KeymapTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Description}
    Result:=Keymap.KeymapDescription;
    
    UniqueString(Result);
   finally
    CriticalSectionUnlock(KeymapTableLock);
   end;
  end;
end;

{==============================================================================}

function KeymapGetProperties(Handle:TKeymapHandle;Properties:PKeymapProperties):LongWord;
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

function KeymapSetDefault(Keymap:PKeymapEntry):LongWord; 
{Set the current default keymap}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Keymap}
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
