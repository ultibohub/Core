{
Ultibo Keymap (FR) interface unit.

Copyright (C) 2024 - SoftOz Pty Ltd.

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

 AZERTY keyboard - https://en.wikipedia.org/wiki/AZERTY

 Franch keyboard layout - https://www.microsoft.com/resources/msdn/goglobal/keyboards/kbdfr.html
 
 
French Keymap (FR)
==================


}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit Keymap_FR;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Keymap,SysUtils;

{==============================================================================}
{Global definitions}
{$INCLUDE ..\core\GlobalDefines.inc}
        
{==============================================================================}
{const}
 {Keymap_FR specific constants}

{==============================================================================}
{type}
 {Keymap_FR specific types}

{==============================================================================}
{var}
 {Keymap_FR specific variables}

{==============================================================================}
{Initialization Functions}
procedure Keymap_FRInit;
 
{==============================================================================}
{==============================================================================}

implementation
 
{==============================================================================}
{==============================================================================}
var
 {Keymap_FR specific variables}
 Keymap_FRInitialized:Boolean;
 
var 
 {Keymap FR - French}
 KEYMAP_FR_HEADER:TKeymapHeader = (
  Mode:KEYMAP_MODE_NONE;
  Flags:KEYMAP_FLAG_ALTGR;
  KeyCount:KEYMAP_KEY_COUNT;
  RowCount:KEYMAP_ROW_COUNT;
  Name:('FR');
  Description:('French')
  );
 
 KEYMAP_FR_DATA:TKeymapData = (
        {Scan Code mappings:                 Normal                         Shift                          AltGr                          Shift+AltGr  }
  Data:({SCAN_CODE_NONE                   } (KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_ROLLOVER               } (KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_POSTFAIL               } (KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_ERROR                  } (KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_A                      } (KEY_CODE_Q,                    KEY_CODE_CAPITAL_Q,            KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_B                      } (KEY_CODE_B,                    KEY_CODE_CAPITAL_B,            KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_C                      } (KEY_CODE_C,                    KEY_CODE_CAPITAL_C,            KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_D                      } (KEY_CODE_D,                    KEY_CODE_CAPITAL_D,            KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_E                      } (KEY_CODE_E,                    KEY_CODE_CAPITAL_E,            KEY_CODE_EURO,                 KEY_CODE_NONE),
        {SCAN_CODE_F                      } (KEY_CODE_F,                    KEY_CODE_CAPITAL_F,            KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_G                      } (KEY_CODE_G,                    KEY_CODE_CAPITAL_G,            KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_H                      } (KEY_CODE_H,                    KEY_CODE_CAPITAL_H,            KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_I                      } (KEY_CODE_I,                    KEY_CODE_CAPITAL_I,            KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_J                      } (KEY_CODE_J,                    KEY_CODE_CAPITAL_J,            KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_K                      } (KEY_CODE_K,                    KEY_CODE_CAPITAL_K,            KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_L                      } (KEY_CODE_L,                    KEY_CODE_CAPITAL_L,            KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_M                      } (KEY_CODE_COMMA,                KEY_CODE_QUESTION,             KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_N                      } (KEY_CODE_N,                    KEY_CODE_CAPITAL_N,            KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_O                      } (KEY_CODE_O,                    KEY_CODE_CAPITAL_O,            KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_P                      } (KEY_CODE_P,                    KEY_CODE_CAPITAL_P,            KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_Q                      } (KEY_CODE_A,                    KEY_CODE_CAPITAL_A,            KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_R                      } (KEY_CODE_R,                    KEY_CODE_CAPITAL_R,            KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_S                      } (KEY_CODE_S,                    KEY_CODE_CAPITAL_S,            KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_T                      } (KEY_CODE_T,                    KEY_CODE_CAPITAL_T,            KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_U                      } (KEY_CODE_U,                    KEY_CODE_CAPITAL_U,            KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_V                      } (KEY_CODE_V,                    KEY_CODE_CAPITAL_V,            KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_W                      } (KEY_CODE_Z,                    KEY_CODE_CAPITAL_Z,            KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_X                      } (KEY_CODE_X,                    KEY_CODE_CAPITAL_X,            KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_Y                      } (KEY_CODE_Y,                    KEY_CODE_CAPITAL_Y,            KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_Z                      } (KEY_CODE_W,                    KEY_CODE_CAPITAL_W,            KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_1                      } (KEY_CODE_AMPERSAND,            KEY_CODE_1,                    KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_2                      } (KEY_CODE_ACUTE_E,              KEY_CODE_2,                    KEY_CODE_TILDE,                KEY_CODE_NONE),
        {SCAN_CODE_3                      } (KEY_CODE_QUOTATION,            KEY_CODE_3,                    KEY_CODE_NUMBER,               KEY_CODE_NONE),
        {SCAN_CODE_4                      } (KEY_CODE_APOSTROPHE,           KEY_CODE_4,                    KEY_CODE_LEFT_BRACE,           KEY_CODE_NONE),
        {SCAN_CODE_5                      } (KEY_CODE_LEFT_BRACKET,         KEY_CODE_5,                    KEY_CODE_LEFT_SQUARE,          KEY_CODE_NONE),
        {SCAN_CODE_6                      } (KEY_CODE_MINUS,                KEY_CODE_6,                    KEY_CODE_PIPE,                 KEY_CODE_NONE),
        {SCAN_CODE_7                      } (KEY_CODE_GRAVE_E,              KEY_CODE_7,                    KEY_CODE_GRAVE,                KEY_CODE_NONE),
        {SCAN_CODE_8                      } (KEY_CODE_UNDERSCORE,           KEY_CODE_8,                    KEY_CODE_BACKSLASH,            KEY_CODE_NONE),
        {SCAN_CODE_9                      } (KEY_CODE_CEDILLA_C,            KEY_CODE_9,                    KEY_CODE_CARET,                KEY_CODE_NONE),
        {SCAN_CODE_0                      } (KEY_CODE_GRAVE_A,              KEY_CODE_0,                    KEY_CODE_AT,                   KEY_CODE_NONE),
        {SCAN_CODE_ENTER                  } (KEY_CODE_ENTER,                KEY_CODE_ENTER,                KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_ESCAPE                 } (KEY_CODE_ESCAPE,               KEY_CODE_ESCAPE,               KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_BACKSPACE              } (KEY_CODE_BACKSPACE,            KEY_CODE_BACKSPACE,            KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_TAB                    } (KEY_CODE_TAB,                  KEY_CODE_TAB,                  KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_SPACE                  } (KEY_CODE_SPACE,                KEY_CODE_SPACE,                KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_MINUS                  } (KEY_CODE_RIGHT_BRACKET,        KEY_CODE_DEGREE,               KEY_CODE_RIGHT_SQUARE,         KEY_CODE_NONE),
        {SCAN_CODE_EQUALS                 } (KEY_CODE_EQUALS,               KEY_CODE_PLUS,                 KEY_CODE_RIGHT_BRACE,          KEY_CODE_NONE),
        {SCAN_CODE_LEFT_SQUARE            } (KEY_CODE_CARET,                KEY_CODE_DIAERESIS,            KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_RIGHT_SQUARE           } (KEY_CODE_DOLLAR,               KEY_CODE_POUND,                KEY_CODE_CURRENCY,             KEY_CODE_NONE),
        {SCAN_CODE_BACKSLASH              } (KEY_CODE_ASTERISK,             KEY_CODE_MICRO,                KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_NONUS_NUMBER           } (KEY_CODE_ASTERISK,             KEY_CODE_MICRO,                KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_SEMICOLON              } (KEY_CODE_M,                    KEY_CODE_CAPITAL_M,            KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_APOSTROPHE             } (KEY_CODE_GRAVE_U,              KEY_CODE_PERCENT,              KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_GRAVE                  } (KEY_CODE_SUPERSCRIPT_2,        KEY_CODE_NONE,                 KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_COMMA                  } (KEY_CODE_SEMICOLON,            KEY_CODE_PERIOD,               KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_PERIOD                 } (KEY_CODE_COLON,                KEY_CODE_SLASH,                KEY_CODE_NONE,                 KEY_CODE_NONE),
        {SCAN_CODE_SLASH                  } (KEY_CODE_EXCLAMATION,          KEY_CODE_SECTION,              KEY_CODE_NONE,                 KEY_CODE_NONE),
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
        {SCAN_CODE_NONUS_BACKSLASH        } (KEY_CODE_LESSTHAN,             KEY_CODE_GREATERTHAN,          KEY_CODE_NONE,                 KEY_CODE_NONE),
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
 
 KEYMAP_FR_CAPSKEYS:TKeymapCapskeys = (
  Count:8;
  Keys:((First:SCAN_CODE_A;             Last:SCAN_CODE_Z),
        (First:SCAN_CODE_1;             Last:SCAN_CODE_0),
        (First:SCAN_CODE_MINUS;         Last:SCAN_CODE_EQUALS),
        (First:SCAN_CODE_LEFT_SQUARE;   Last:SCAN_CODE_RIGHT_SQUARE),
        (First:SCAN_CODE_SEMICOLON;     Last:SCAN_CODE_APOSTROPHE),
        (First:SCAN_CODE_BACKSLASH;     Last:SCAN_CODE_BACKSLASH),
        (First:SCAN_CODE_NONUS_NUMBER;  Last:SCAN_CODE_NONUS_NUMBER),
        (First:SCAN_CODE_COMMA;         Last:SCAN_CODE_SLASH),
        (First:SCAN_CODE_NONE;          Last:SCAN_CODE_NONE),
        (First:SCAN_CODE_NONE;          Last:SCAN_CODE_NONE),
        (First:SCAN_CODE_NONE;          Last:SCAN_CODE_NONE),
        (First:SCAN_CODE_NONE;          Last:SCAN_CODE_NONE),
        (First:SCAN_CODE_NONE;          Last:SCAN_CODE_NONE),
        (First:SCAN_CODE_NONE;          Last:SCAN_CODE_NONE),
        (First:SCAN_CODE_NONE;          Last:SCAN_CODE_NONE),
        (First:SCAN_CODE_NONE;          Last:SCAN_CODE_NONE),
        (First:SCAN_CODE_NONE;          Last:SCAN_CODE_NONE),
        (First:SCAN_CODE_NONE;          Last:SCAN_CODE_NONE),
        (First:SCAN_CODE_NONE;          Last:SCAN_CODE_NONE),
        (First:SCAN_CODE_NONE;          Last:SCAN_CODE_NONE),
        (First:SCAN_CODE_NONE;          Last:SCAN_CODE_NONE),
        (First:SCAN_CODE_NONE;          Last:SCAN_CODE_NONE),
        (First:SCAN_CODE_NONE;          Last:SCAN_CODE_NONE),
        (First:SCAN_CODE_NONE;          Last:SCAN_CODE_NONE),
        (First:SCAN_CODE_NONE;          Last:SCAN_CODE_NONE),
        (First:SCAN_CODE_NONE;          Last:SCAN_CODE_NONE),
        (First:SCAN_CODE_NONE;          Last:SCAN_CODE_NONE),
        (First:SCAN_CODE_NONE;          Last:SCAN_CODE_NONE),
        (First:SCAN_CODE_NONE;          Last:SCAN_CODE_NONE),
        (First:SCAN_CODE_NONE;          Last:SCAN_CODE_NONE),
        (First:SCAN_CODE_NONE;          Last:SCAN_CODE_NONE),
        (First:SCAN_CODE_NONE;          Last:SCAN_CODE_NONE),
        (First:SCAN_CODE_NONE;          Last:SCAN_CODE_NONE),
        (First:SCAN_CODE_NONE;          Last:SCAN_CODE_NONE),
        (First:SCAN_CODE_NONE;          Last:SCAN_CODE_NONE),
        (First:SCAN_CODE_NONE;          Last:SCAN_CODE_NONE),
        (First:SCAN_CODE_NONE;          Last:SCAN_CODE_NONE),
        (First:SCAN_CODE_NONE;          Last:SCAN_CODE_NONE),
        (First:SCAN_CODE_NONE;          Last:SCAN_CODE_NONE),
        (First:SCAN_CODE_NONE;          Last:SCAN_CODE_NONE),
        (First:SCAN_CODE_NONE;          Last:SCAN_CODE_NONE),
        (First:SCAN_CODE_NONE;          Last:SCAN_CODE_NONE),
        (First:SCAN_CODE_NONE;          Last:SCAN_CODE_NONE),
        (First:SCAN_CODE_NONE;          Last:SCAN_CODE_NONE),
        (First:SCAN_CODE_NONE;          Last:SCAN_CODE_NONE),
        (First:SCAN_CODE_NONE;          Last:SCAN_CODE_NONE),
        (First:SCAN_CODE_NONE;          Last:SCAN_CODE_NONE),
        (First:SCAN_CODE_NONE;          Last:SCAN_CODE_NONE),
        (First:SCAN_CODE_NONE;          Last:SCAN_CODE_NONE),
        (First:SCAN_CODE_NONE;          Last:SCAN_CODE_NONE))
  );
       
 KEYMAP_FR_DEADKEYS:TKeymapDeadkeys = (
  Count:4;
  Keys:((Key:SCAN_CODE_LEFT_SQUARE; {Circumflex Accent (Normal)}
         Index:KEYMAP_INDEX_NORMAL;
         Resolves:((Key:SCAN_CODE_Q;              Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_CIRCUMFLEX_A),
                   (Key:SCAN_CODE_Q;              Index:KEYMAP_INDEX_SHIFT;               Code:KEY_CODE_CAPITAL_CIRCUMFLEX_A),
                   (Key:SCAN_CODE_E;              Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_CIRCUMFLEX_E),
                   (Key:SCAN_CODE_E;              Index:KEYMAP_INDEX_SHIFT;               Code:KEY_CODE_CAPITAL_CIRCUMFLEX_E),
                   (Key:SCAN_CODE_I;              Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_CIRCUMFLEX_I),
                   (Key:SCAN_CODE_I;              Index:KEYMAP_INDEX_SHIFT;               Code:KEY_CODE_CAPITAL_CIRCUMFLEX_I),
                   (Key:SCAN_CODE_O;              Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_CIRCUMFLEX_O),
                   (Key:SCAN_CODE_O;              Index:KEYMAP_INDEX_SHIFT;               Code:KEY_CODE_CAPITAL_CIRCUMFLEX_O),
                   (Key:SCAN_CODE_U;              Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_CIRCUMFLEX_U),
                   (Key:SCAN_CODE_U;              Index:KEYMAP_INDEX_SHIFT;               Code:KEY_CODE_CAPITAL_CIRCUMFLEX_U),
                   (Key:SCAN_CODE_NONE;           Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_NONE),
                   (Key:SCAN_CODE_NONE;           Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_NONE),
                   (Key:SCAN_CODE_NONE;           Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_NONE),
                   (Key:SCAN_CODE_NONE;           Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_NONE),
                   (Key:SCAN_CODE_NONE;           Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_NONE),
                   (Key:SCAN_CODE_NONE;           Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_NONE),
                   (Key:SCAN_CODE_NONE;           Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_NONE),
                   (Key:SCAN_CODE_NONE;           Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_NONE),
                   (Key:SCAN_CODE_NONE;           Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_NONE),
                   (Key:SCAN_CODE_NONE;           Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_NONE))
        ),                                        
        (Key:SCAN_CODE_LEFT_SQUARE; {Diaeresis (Shift)}
         Index:KEYMAP_INDEX_SHIFT;               
         Resolves:((Key:SCAN_CODE_Q;              Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_DIAERESIS_A),
                   (Key:SCAN_CODE_Q;              Index:KEYMAP_INDEX_SHIFT;               Code:KEY_CODE_CAPITAL_DIAERESIS_A),
                   (Key:SCAN_CODE_E;              Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_DIAERESIS_E),
                   (Key:SCAN_CODE_E;              Index:KEYMAP_INDEX_SHIFT;               Code:KEY_CODE_CAPITAL_DIAERESIS_E),
                   (Key:SCAN_CODE_I;              Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_DIAERESIS_I),
                   (Key:SCAN_CODE_I;              Index:KEYMAP_INDEX_SHIFT;               Code:KEY_CODE_CAPITAL_DIAERESIS_I),
                   (Key:SCAN_CODE_O;              Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_DIAERESIS_O),
                   (Key:SCAN_CODE_O;              Index:KEYMAP_INDEX_SHIFT;               Code:KEY_CODE_CAPITAL_DIAERESIS_O),
                   (Key:SCAN_CODE_U;              Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_DIAERESIS_U),
                   (Key:SCAN_CODE_U;              Index:KEYMAP_INDEX_SHIFT;               Code:KEY_CODE_CAPITAL_DIAERESIS_U),
                   (Key:SCAN_CODE_Y;              Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_DIAERESIS_Y),
                   (Key:SCAN_CODE_NONE;           Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_NONE),
                   (Key:SCAN_CODE_NONE;           Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_NONE),
                   (Key:SCAN_CODE_NONE;           Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_NONE),
                   (Key:SCAN_CODE_NONE;           Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_NONE),
                   (Key:SCAN_CODE_NONE;           Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_NONE),
                   (Key:SCAN_CODE_NONE;           Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_NONE),
                   (Key:SCAN_CODE_NONE;           Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_NONE),
                   (Key:SCAN_CODE_NONE;           Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_NONE),
                   (Key:SCAN_CODE_NONE;           Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_NONE))
        ),                                        
        (Key:SCAN_CODE_2; {Tilde (AltGr)}                      
         Index:KEYMAP_INDEX_ALTGR;               
         Resolves:((Key:SCAN_CODE_Q;              Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_TILDE_A),
                   (Key:SCAN_CODE_Q;              Index:KEYMAP_INDEX_SHIFT;               Code:KEY_CODE_CAPITAL_TILDE_A),
                   (Key:SCAN_CODE_N;              Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_TILDE_N),
                   (Key:SCAN_CODE_N;              Index:KEYMAP_INDEX_SHIFT;               Code:KEY_CODE_CAPITAL_TILDE_N),
                   (Key:SCAN_CODE_O;              Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_TILDE_O),
                   (Key:SCAN_CODE_O;              Index:KEYMAP_INDEX_SHIFT;               Code:KEY_CODE_CAPITAL_TILDE_O),
                   (Key:SCAN_CODE_NONE;           Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_NONE),
                   (Key:SCAN_CODE_NONE;           Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_NONE),
                   (Key:SCAN_CODE_NONE;           Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_NONE),
                   (Key:SCAN_CODE_NONE;           Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_NONE),
                   (Key:SCAN_CODE_NONE;           Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_NONE),
                   (Key:SCAN_CODE_NONE;           Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_NONE),
                   (Key:SCAN_CODE_NONE;           Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_NONE),
                   (Key:SCAN_CODE_NONE;           Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_NONE),
                   (Key:SCAN_CODE_NONE;           Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_NONE),
                   (Key:SCAN_CODE_NONE;           Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_NONE),
                   (Key:SCAN_CODE_NONE;           Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_NONE),
                   (Key:SCAN_CODE_NONE;           Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_NONE),
                   (Key:SCAN_CODE_NONE;           Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_NONE),
                   (Key:SCAN_CODE_NONE;           Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_NONE))
        ),                                        
        (Key:SCAN_CODE_7; {Grave Accent (AltGr)}                      
         Index:KEYMAP_INDEX_ALTGR;               
         Resolves:((Key:SCAN_CODE_Q;              Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_GRAVE_A),
                   (Key:SCAN_CODE_Q;              Index:KEYMAP_INDEX_SHIFT;               Code:KEY_CODE_CAPITAL_GRAVE_A),
                   (Key:SCAN_CODE_E;              Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_GRAVE_E),
                   (Key:SCAN_CODE_E;              Index:KEYMAP_INDEX_SHIFT;               Code:KEY_CODE_CAPITAL_GRAVE_E),
                   (Key:SCAN_CODE_I;              Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_GRAVE_I),
                   (Key:SCAN_CODE_I;              Index:KEYMAP_INDEX_SHIFT;               Code:KEY_CODE_CAPITAL_GRAVE_I),
                   (Key:SCAN_CODE_O;              Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_GRAVE_O),
                   (Key:SCAN_CODE_O;              Index:KEYMAP_INDEX_SHIFT;               Code:KEY_CODE_CAPITAL_GRAVE_O),
                   (Key:SCAN_CODE_U;              Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_GRAVE_U),
                   (Key:SCAN_CODE_U;              Index:KEYMAP_INDEX_SHIFT;               Code:KEY_CODE_CAPITAL_GRAVE_U),
                   (Key:SCAN_CODE_NONE;           Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_NONE),
                   (Key:SCAN_CODE_NONE;           Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_NONE),
                   (Key:SCAN_CODE_NONE;           Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_NONE),
                   (Key:SCAN_CODE_NONE;           Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_NONE),
                   (Key:SCAN_CODE_NONE;           Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_NONE),
                   (Key:SCAN_CODE_NONE;           Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_NONE),
                   (Key:SCAN_CODE_NONE;           Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_NONE),
                   (Key:SCAN_CODE_NONE;           Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_NONE),
                   (Key:SCAN_CODE_NONE;           Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_NONE),
                   (Key:SCAN_CODE_NONE;           Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_NONE))
        ),                                        
        (Key:SCAN_CODE_NONE; {Unused}                      
         Index:KEYMAP_INDEX_NORMAL;               
         Resolves:((Key:SCAN_CODE_NONE;           Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_NONE),
                   (Key:SCAN_CODE_NONE;           Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_NONE),
                   (Key:SCAN_CODE_NONE;           Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_NONE),
                   (Key:SCAN_CODE_NONE;           Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_NONE),
                   (Key:SCAN_CODE_NONE;           Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_NONE),
                   (Key:SCAN_CODE_NONE;           Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_NONE),
                   (Key:SCAN_CODE_NONE;           Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_NONE),
                   (Key:SCAN_CODE_NONE;           Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_NONE),
                   (Key:SCAN_CODE_NONE;           Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_NONE),
                   (Key:SCAN_CODE_NONE;           Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_NONE),
                   (Key:SCAN_CODE_NONE;           Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_NONE),
                   (Key:SCAN_CODE_NONE;           Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_NONE),
                   (Key:SCAN_CODE_NONE;           Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_NONE),
                   (Key:SCAN_CODE_NONE;           Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_NONE),
                   (Key:SCAN_CODE_NONE;           Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_NONE),
                   (Key:SCAN_CODE_NONE;           Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_NONE),
                   (Key:SCAN_CODE_NONE;           Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_NONE),
                   (Key:SCAN_CODE_NONE;           Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_NONE),
                   (Key:SCAN_CODE_NONE;           Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_NONE),
                   (Key:SCAN_CODE_NONE;           Index:KEYMAP_INDEX_NORMAL;              Code:KEY_CODE_NONE))
        ))
  );
 
{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure Keymap_FRInit;
var
 KeymapHandle:TKeymapHandle;
begin
 {}
 {Check Initialized}
 if Keymap_FRInitialized then Exit;

 {Load French (FR) Keymap}
 KeymapHandle:=KeymapLoadEx(@KEYMAP_FR_HEADER,@KEYMAP_FR_DATA,@KEYMAP_FR_CAPSKEYS,@KEYMAP_FR_DEADKEYS,SizeOf(TKeymapData),nil);
 if KeymapHandle = INVALID_HANDLE_VALUE then
  begin
   if PLATFORM_LOG_ENABLED then PlatformLogError('Failed to load keymap FR');
  end;
  
 {Check Environment Variables}
 if (Uppercase(EnvironmentGet('KEYMAP_DEFAULT')) = 'FR') or (Uppercase(KEYMAP_DEFAULT) = 'FR') then
  begin
   {Update Default}
   KEYMAP_DEFAULT:='FR';
   
   {Set Default}
   KeymapSetDefault(KeymapHandle);
  end;  
  
 Keymap_FRInitialized:=True;
end;

{==============================================================================}
{==============================================================================}

initialization
 Keymap_FRInit;
 
{==============================================================================}
 
finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
         