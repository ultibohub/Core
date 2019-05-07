{
Ultibo Global Constant Definitions.

Copyright (C) 2019 - SoftOz Pty Ltd.

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


Global Constants
================


}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit GlobalConst; 

interface

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
{Global constants}
const
 {Version constants}
 ULTIBO_RELEASE_DATE             = '8 May 2019';
 ULTIBO_RELEASE_NAME             = 'Beetroot';
 ULTIBO_RELEASE_VERSION          = '2.0.653';
 ULTIBO_RELEASE_VERSION_MAJOR    = 2;
 ULTIBO_RELEASE_VERSION_MINOR    = 0;
 ULTIBO_RELEASE_VERSION_REVISION = 653;
 
{==============================================================================}
const
 {Universal error constants}
 ERROR_SUCCESS             = 0;    {Success}
 NO_ERROR                  = 0;    {Success}
 
 ERROR_INVALID_FUNCTION    = 1;    {Invalid function}
 ERROR_FILE_NOT_FOUND      = 2;    {The file cannot be found}
 ERROR_PATH_NOT_FOUND      = 3;    {The path cannot be found}
 ERROR_TOO_MANY_OPEN_FILES = 4;    {Too many open files}
 ERROR_ACCESS_DENIED       = 5;    {Access is denied}
 ERROR_INVALID_HANDLE      = 6;    {Invalid handle}
 ERROR_NOT_ENOUGH_MEMORY   = DWORD(8);  {Not enough storage is available to process this command}
 
 ERROR_INVALID_ACCESS      = 12;   {Invalid access}
 ERROR_INVALID_DATA        = 13;   {The data is invalid}
 ERROR_OUTOFMEMORY         = 14;   {Not enough memory is available}
 ERROR_INVALID_DRIVE       = 15;   {Cannot find the drive specified}
 ERROR_CURRENT_DIRECTORY   = 16;   {Current directory cannot be removed}
 ERROR_NOT_SAME_DEVICE     = 17;   {Cannot move the file to a different disk drive}
 ERROR_NO_MORE_FILES       = 18;   {There are no more files}
 ERROR_WRITE_PROTECT       = 19;   {Media is write protected}
 ERROR_BAD_UNIT            = 20;   {Cannot find the device specified}
 ERROR_NOT_READY           = 21;   {The device is not ready}
 ERROR_BAD_COMMAND         = 22;   {The device does not recognise the command}
 
 ERROR_WRITE_FAULT         = 29;   {The device cannot be written to}
 ERROR_READ_FAULT          = 30;   {The device cannot be read from}
 ERROR_GEN_FAILURE         = 31;   {The device has failed}
 
 ERROR_NOT_SUPPORTED       = 50;   {The request is not supported}
 
 ERROR_DEV_NOT_EXIST       = 55;   {The device does not exist}
 
 ERROR_BAD_DEV_TYPE        = 66;   {Invalid device type}
 
 ERROR_ALREADY_ASSIGNED    = 85;   {The device name is already in use}
 ERROR_INVALID_PASSWORD    = 86;   {Invalid pasword}
 ERROR_INVALID_PARAMETER   = 87;   {Invalid parameter}
  
 ERROR_SEM_IS_SET          = 102;  {The semaphore is in use and cannot be closed}
 ERROR_OPEN_FAILED         = 110;  {The file or device could not be opened}
 ERROR_CALL_NOT_IMPLEMENTED = 120; {The function is not currently implemented}
 ERROR_INSUFFICIENT_BUFFER = 122;  {The buffer passed is too small for the requested data}
 ERROR_WAIT_NO_CHILDREN    = 128;  {There are no child processes to wait for}
 
 ERROR_NOT_LOCKED          = 158;  {The entry is not locked}
 
 ERROR_LOCK_FAILED         = 167;  {The lock operation failed}
 
 ERROR_ALREADY_EXISTS      = 183;  {The file or object already exists}
 
 ERROR_ENVVAR_NOT_FOUND    = 203;  {The environment variable could not be found}
 
 ERROR_LOCKED              = 212;  {The entry is already locked}

 ERROR_MORE_DATA           = 234;  {More data is available than the provided buffer}

 ERROR_WAIT_TIMEOUT        = 258;  {The operation timed out}
 ERROR_NO_MORE_ITEMS       = 259;  {No more items available}
 
 ERROR_NOT_OWNER           = 288;  {The current thread is not the owner}
 
 ERROR_OPERATION_ABORTED   = DWORD(995); {The I/O operation has been aborted because of either a thread exit or an application request}
 ERROR_IO_INCOMPLETE       = DWORD(996); {Overlapped I/O event is not in a signaled state}
 ERROR_IO_PENDING          = DWORD(997); {Overlapped I/O operation is in progress}
 
 ERROR_CAN_NOT_COMPLETE    = 1003; {Cannot complete the function}
 
 ERROR_NOT_FOUND           = 1168; {The entry or device was not found}
 
 ERROR_INVALID_ACL            = DWORD(1336);  {The access control list (ACL) structure is invalid}
 ERROR_INVALID_SID            = DWORD(1337);  {The security ID structure is invalid}
 ERROR_INVALID_SECURITY_DESCR = DWORD(1338);  {The security descriptor structure is invalid}
 
 ERROR_TIMEOUT             = 1460; {The operation returned because the timeout expired}
 
 ERROR_FUNCTION_FAILED     = 1627; {The function call failed}
 
 {Errors below here have no compatibility equivalent}
 ERROR_NOT_VALID           = 1000001;    {The entry or device is not valid}
 ERROR_NOT_ASSIGNED        = 1000002;    {The device is not assigned}
 ERROR_IN_USE              = 1000003;    {The device is in use}
 ERROR_OPERATION_FAILED    = 1000004;    {The operation failed}
 ERROR_NOT_OPEN            = 1000005;    {The file or device is not open}
 ERROR_ALREADY_OPEN        = 1000006;    {The file or device is already open}
 ERROR_WAIT_ABANDONED      = 1000007;    {The operation was abandoned}
 ERROR_IN_PROGRESS         = 1000008;    {An operation is already in progress}
 ERROR_RUNTIME_ERROR       = 1000009;    {A run time occurred}
 ERROR_EXCEPTION           = 1000010;    {An exception occurred}
 ERROR_NOT_PROCESSED       = 1000011;    {The entry has not been processed}
 ERROR_NOT_COMPLETED       = 1000012;    {The entry or operation has not completed}
 ERROR_NOT_COMPATIBLE      = 1000013;    {The entry is not compatible for the operation}
 ERROR_CANCELLED           = 1000014;    {The entry or operation has been cancelled}
 ERROR_NOT_EXACT           = 1000015;    {The result of the operation is not exact}
 ERROR_ALREADY_OWNER       = 1000016;    {The current thread is already the owner}
 
 ERROR_UNKNOWN             = $FFFFFFFF;
 
{==============================================================================}
const
 {Universal value constants}
 INVALID_HANDLE_VALUE     = THandle(-1); {DWORD(-1);}
 INVALID_FILE_SIZE        = DWORD($FFFFFFFF);
 INVALID_SET_FILE_POINTER = DWORD(-1);
 INVALID_FILE_ATTRIBUTES  = DWORD(-1);
 
 {File position constants}
 FILE_BEGIN   = 0;
 FILE_CURRENT = 1;
 FILE_END     = 2;
 
 {File open/create constants}
 CREATE_NEW        = 1;
 CREATE_ALWAYS     = 2;
 OPEN_EXISTING     = 3;
 OPEN_ALWAYS       = 4;
 TRUNCATE_EXISTING = 5;
 
 {File creation flag constants}
 FILE_FLAG_WRITE_THROUGH       = DWORD($80000000);
 FILE_FLAG_OVERLAPPED          = $40000000;
 FILE_FLAG_NO_BUFFERING        = $20000000;
 FILE_FLAG_RANDOM_ACCESS       = $10000000;
 FILE_FLAG_SEQUENTIAL_SCAN     = $08000000;
 FILE_FLAG_DELETE_ON_CLOSE     = $04000000;
 FILE_FLAG_BACKUP_SEMANTICS    = $02000000;
 FILE_FLAG_POSIX_SEMANTICS     = $01000000;
 FILE_FLAG_OPEN_REPARSE_POINT  = $00200000;
 FILE_FLAG_OPEN_NO_RECALL      = $00100000;
 FILE_FLAG_FIRST_PIPE_INSTANCE = $00080000;
 
 {File attribute constants}
 FILE_ATTRIBUTE_READONLY            = $00000001;
 FILE_ATTRIBUTE_HIDDEN              = $00000002;
 FILE_ATTRIBUTE_SYSTEM              = $00000004;
 FILE_ATTRIBUTE_DIRECTORY           = $00000010;
 FILE_ATTRIBUTE_ARCHIVE             = $00000020;
 FILE_ATTRIBUTE_DEVICE              = $00000040;
 FILE_ATTRIBUTE_NORMAL              = $00000080;
 FILE_ATTRIBUTE_TEMPORARY           = $00000100;
 FILE_ATTRIBUTE_SPARSE_FILE         = $00000200;
 FILE_ATTRIBUTE_REPARSE_POINT       = $00000400;
 FILE_ATTRIBUTE_COMPRESSED          = $00000800;
 FILE_ATTRIBUTE_OFFLINE             = $00001000;
 FILE_ATTRIBUTE_NOT_CONTENT_INDEXED = $00002000;
 FILE_ATTRIBUTE_ENCRYPTED           = $00004000;
 
 {Filesystem attribute constants}
 FS_CASE_SENSITIVE_SEARCH           = $00000001;
 FS_CASE_PRESERVED_NAMES            = $00000002;
 FS_UNICODE_ON_DISK                 = $00000004;
 FS_PERSISTENT_ACLS                 = $00000008;
 FS_FILE_COMPRESSION                = $00000010;
 FS_VOLUME_QUOTAS                   = $00000020;
 FS_SUPPORTS_SPARSE_FILES           = $00000040;
 FS_SUPPORTS_REPARSE_POINTS         = $00000080;
 FS_SUPPORTS_REMOTE_STORAGE         = $00000100;
 FS_VOLUME_IS_COMPRESSED            = $00008000;
 FS_SUPPORTS_OBJECT_IDS             = $00010000;
 FS_SUPPORTS_ENCRYPTION             = $00020000;
 FS_NAMED_STREAMS                   = $00040000;
 FS_READ_ONLY_VOLUME                = $00080000;
 
 {File Access Constants} 
 GENERIC_READ    = DWORD($80000000);
 GENERIC_WRITE   = $40000000;
 GENERIC_EXECUTE = $20000000;
 GENERIC_ALL     = $10000000;
 
 {File Share Constants} 
 FILE_SHARE_READ                    = $00000001;
 FILE_SHARE_WRITE                   = $00000002;
 FILE_SHARE_DELETE                  = $00000004;
 
 {Timeout constants}
 INFINITE = LongWord(-1);
 
 {TLS constants}
 TLS_OUT_OF_INDEXES = DWORD($FFFFFFFF);
 
 {Wait constants}
 WAIT_OBJECT_0      = ERROR_SUCCESS + 0;
 WAIT_ABANDONED     = $00000080 + 0; {STATUS_ABANDONED_WAIT_0}
 WAIT_ABANDONED_0   = $00000080 + 0; {STATUS_ABANDONED_WAIT_0}
 WAIT_TIMEOUT       = ERROR_WAIT_TIMEOUT;
 WAIT_FAILED        = DWORD($FFFFFFFF);

 WAIT_IO_COMPLETION = $000000C0; {STATUS_USER_APC}
 
 MAXIMUM_WAIT_OBJECTS  = 64;      {Maximum number of wait objects}
 
 {Thread state constants}
 STILL_ACTIVE     = ERROR_NO_MORE_ITEMS;
 
 {TimeZone constants}
 TIME_ZONE_ID_INVALID = DWORD($FFFFFFFF);
 
{==============================================================================}
const
 {Universal key code constants}
 {Keyboard key codes are based on the Unicode standard with each key code mapped to the code point for that character (See http://unicode.org or http://unicode-table.com)}
 {Keyboard keymaps map the scan code value to the key code value for the specific keyboard layout and include alternate mappings for Shift and AltGr}
 {Non character codes for keys like Home, End, Arrows, F1..F24, Shift, Caps Lock etc are mapped in the Unicode Private Use Area (E000-F8FF)}
 {These are the values reported in the KeyCode field of the TKeyboardData structure returned by the KeyboardRead function}
 {Control Characters (0000-001F)}
 KEY_CODE_NONE      = $0000; {0}
 KEY_CODE_BACKSPACE = $0008; {8}
 KEY_CODE_TAB       = $0009; {9}
 KEY_CODE_ENTER     = $000D; {13}
 KEY_CODE_ESCAPE    = $001B; {27}
 KEY_CODE_DELETE    = $007F; {127}

 {Basic Latin Characters (0020-007F)}
 {Punctuation}
 KEY_CODE_SPACE         = $0020; {32}
 KEY_CODE_EXCLAMATION   = $0021; {33}
 KEY_CODE_QUOTATION     = $0022; {34}
 KEY_CODE_NUMBER        = $0023; {35} {Hash / Pound}
 KEY_CODE_DOLLAR        = $0024; {36}
 KEY_CODE_PERCENT       = $0025; {37}
 KEY_CODE_AMPERSAND     = $0026; {38}
 KEY_CODE_APOSTROPHE    = $0027; {39}
 KEY_CODE_LEFT_BRACKET  = $0028; {40}
 KEY_CODE_RIGHT_BRACKET = $0029; {41}
 KEY_CODE_ASTERISK      = $002A; {42}
 KEY_CODE_PLUS          = $002B; {43}
 KEY_CODE_COMMA         = $002C; {44}
 KEY_CODE_MINUS         = $002D; {45}
 KEY_CODE_PERIOD        = $002E; {46}
 KEY_CODE_SLASH         = $002F; {47}
 {Numerals}
 KEY_CODE_0             = $0030; {48}
 KEY_CODE_1             = $0031; {49}
 KEY_CODE_2             = $0032; {50}
 KEY_CODE_3             = $0033; {51}
 KEY_CODE_4             = $0034; {52}
 KEY_CODE_5             = $0035; {53}
 KEY_CODE_6             = $0036; {54}
 KEY_CODE_7             = $0037; {55}
 KEY_CODE_8             = $0038; {56}
 KEY_CODE_9             = $0039; {57}
 {Punctuation}
 KEY_CODE_COLON         = $003A; {58}
 KEY_CODE_SEMICOLON     = $003B; {59}
 KEY_CODE_LESSTHAN      = $003C; {60}
 KEY_CODE_EQUALS        = $003D; {61}
 KEY_CODE_GREATERTHAN   = $003E; {62}
 KEY_CODE_QUESTION      = $003F; {63}
 KEY_CODE_AT            = $0040; {64}
 {Capital Characters}
 KEY_CODE_CAPITAL_A     = $0041; {65}
 KEY_CODE_CAPITAL_B     = $0042; {66}
 KEY_CODE_CAPITAL_C     = $0043; {67}
 KEY_CODE_CAPITAL_D     = $0044; {68}
 KEY_CODE_CAPITAL_E     = $0045; {69}
 KEY_CODE_CAPITAL_F     = $0046; {70}
 KEY_CODE_CAPITAL_G     = $0047; {71}
 KEY_CODE_CAPITAL_H     = $0048; {72}
 KEY_CODE_CAPITAL_I     = $0049; {73}
 KEY_CODE_CAPITAL_J     = $004A; {74}
 KEY_CODE_CAPITAL_K     = $004B; {75}
 KEY_CODE_CAPITAL_L     = $004C; {76}
 KEY_CODE_CAPITAL_M     = $004D; {77}
 KEY_CODE_CAPITAL_N     = $004E; {78}
 KEY_CODE_CAPITAL_O     = $004F; {79}
 KEY_CODE_CAPITAL_P     = $0050; {80}
 KEY_CODE_CAPITAL_Q     = $0051; {81}
 KEY_CODE_CAPITAL_R     = $0052; {82}
 KEY_CODE_CAPITAL_S     = $0053; {83}
 KEY_CODE_CAPITAL_T     = $0054; {84}
 KEY_CODE_CAPITAL_U     = $0055; {85}
 KEY_CODE_CAPITAL_V     = $0056; {86}
 KEY_CODE_CAPITAL_W     = $0057; {87}
 KEY_CODE_CAPITAL_X     = $0058; {88}
 KEY_CODE_CAPITAL_Y     = $0059; {89}
 KEY_CODE_CAPITAL_Z     = $005A; {90}
 {Punctuation}
 KEY_CODE_LEFT_SQUARE   = $005B; {91}
 KEY_CODE_BACKSLASH     = $005C; {92}
 KEY_CODE_RIGHT_SQUARE  = $005D; {93}
 KEY_CODE_CARET         = $005E; {94} {Circumflex}
 KEY_CODE_UNDERSCORE    = $005F; {95}
 KEY_CODE_GRAVE         = $0060; {96}
 {Lowercase Characters}
 KEY_CODE_A             = $0061; {97}
 KEY_CODE_B             = $0062; {98}
 KEY_CODE_C             = $0063; {99}
 KEY_CODE_D             = $0064; {100}
 KEY_CODE_E             = $0065; {101}
 KEY_CODE_F             = $0066; {102}
 KEY_CODE_G             = $0067; {103}
 KEY_CODE_H             = $0068; {104}
 KEY_CODE_I             = $0069; {105}
 KEY_CODE_J             = $006A; {106}
 KEY_CODE_K             = $006B; {107}
 KEY_CODE_L             = $006C; {108}
 KEY_CODE_M             = $006D; {109}
 KEY_CODE_N             = $006E; {110}
 KEY_CODE_O             = $006F; {111}
 KEY_CODE_P             = $0070; {112}
 KEY_CODE_Q             = $0071; {113}
 KEY_CODE_R             = $0072; {114}
 KEY_CODE_S             = $0073; {115}
 KEY_CODE_T             = $0074; {116}
 KEY_CODE_U             = $0075; {117}
 KEY_CODE_V             = $0076; {118}
 KEY_CODE_W             = $0077; {119}
 KEY_CODE_X             = $0078; {120}
 KEY_CODE_Y             = $0079; {121}
 KEY_CODE_Z             = $007A; {122}
 {Punctuation}
 KEY_CODE_LEFT_BRACE    = $007B; {123} {Curly Bracket}
 KEY_CODE_PIPE          = $007C; {124} {Vertical Bar}
 KEY_CODE_RIGHT_BRACE   = $007D; {125} {Curly Bracket}
 KEY_CODE_TILDE         = $007E; {126}
 {KEY_CODE_DELETE (See above)}
 
 {Latin-1 Supplement Characters (0080-00FF) (Partial, add extras as required)}
 KEY_CODE_INVERTED_EXCLAMATION = $00A1;
 KEY_CODE_CENT                 = $00A2;
 KEY_CODE_POUND                = $00A3;
 KEY_CODE_CURRENCY             = $00A4;
 KEY_CODE_YEN                  = $00A5;
 KEY_CODE_BROKEN_BAR           = $00A6;
 KEY_CODE_SECTION              = $00A7;
 KEY_CODE_DIAERESIS            = $00A8;
 KEY_CODE_COPYRIGHT            = $00A9;
 KEY_CODE_FEMININE             = $00AA;
 KEY_CODE_LEFT_DOUBLE_ANGLE    = $00AB;
 KEY_CODE_NOT                  = $00AC;
 KEY_CODE_REGISTERED           = $00AE;
 KEY_CODE_MACRON               = $00AF;
 KEY_CODE_DEGREE               = $00B0;
 KEY_CODE_PLUS_MINUS           = $00B1;
 KEY_CODE_SUPERSCRIPT_2        = $00B2;
 KEY_CODE_SUPERSCRIPT_3        = $00B3;
 KEY_CODE_ACUTE                = $00B4;
 KEY_CODE_MICRO                = $00B5;
 KEY_CODE_PILCROW              = $00B6;
 KEY_CODE_MIDDLE_DOT           = $00B7;
 KEY_CODE_CEDILLA              = $00B8;
 KEY_CODE_SUPERSCRIPT_1        = $00B9;
 KEY_CODE_MASCULINE            = $00BA;
 KEY_CODE_RIGHT_DOUBLE_ANGLE   = $00BB;
 KEY_CODE_ONE_QUARTER          = $00BC;
 KEY_CODE_ONE_HALF             = $00BD;
 KEY_CODE_THREE_QUARTER        = $00BE;
 KEY_CODE_INVERTED_QUESTION    = $00BF;
 KEY_CODE_CAPITAL_GRAVE_A      = $00C0;
 KEY_CODE_CAPITAL_ACUTE_A      = $00C1;
 KEY_CODE_CAPITAL_CIRCUMFLEX_A = $00C2;
 KEY_CODE_CAPITAL_TILDE_A      = $00C3;
 KEY_CODE_CAPITAL_DIAERESIS_A  = $00C4;
 KEY_CODE_CAPITAL_RING_A       = $00C5;
 KEY_CODE_CAPITAL_AE           = $00C6;
 KEY_CODE_CAPITAL_CEDILLA_C    = $00C7;
 KEY_CODE_CAPITAL_GRAVE_E      = $00C8;
 KEY_CODE_CAPITAL_ACUTE_E      = $00C9;
 KEY_CODE_CAPITAL_CIRCUMFLEX_E = $00CA;
 KEY_CODE_CAPITAL_DIAERESIS_E  = $00CB;
 KEY_CODE_CAPITAL_GRAVE_I      = $00CC;
 KEY_CODE_CAPITAL_ACUTE_I      = $00CD;
 KEY_CODE_CAPITAL_CIRCUMFLEX_I = $00CE;
 KEY_CODE_CAPITAL_DIAERESIS_I  = $00CF;
 KEY_CODE_CAPITAL_ETH          = $00D0;
 KEY_CODE_CAPITAL_TILDE_N      = $00D1;
 KEY_CODE_CAPITAL_GRAVE_O      = $00D2;
 KEY_CODE_CAPITAL_ACUTE_O      = $00D3;
 KEY_CODE_CAPITAL_CIRCUMFLEX_O = $00D4;
 KEY_CODE_CAPITAL_TILDE_O      = $00D5;
 KEY_CODE_CAPITAL_DIAERESIS_O  = $00D6;
 KEY_CODE_MULTIPLY             = $00D7;
 KEY_CODE_CAPITAL_STROKE_O     = $00D8;
 KEY_CODE_CAPITAL_GRAVE_U      = $00D9;
 KEY_CODE_CAPITAL_ACUTE_U      = $00DA;
 KEY_CODE_CAPITAL_CIRCUMFLEX_U = $00DB;
 KEY_CODE_CAPITAL_DIAERESIS_U  = $00DC;
 KEY_CODE_CAPITAL_ACUTE_Y      = $00DD;
 KEY_CODE_CAPITAL_THORN        = $00DE;
 KEY_CODE_SHARP_S              = $00DF;
 KEY_CODE_GRAVE_A              = $00E0;
 KEY_CODE_ACUTE_A              = $00E1;
 KEY_CODE_CIRCUMFLEX_A         = $00E2;
 KEY_CODE_TILDE_A              = $00E3;
 KEY_CODE_DIAERESIS_A          = $00E4;
 KEY_CODE_RING_A               = $00E5;
 KEY_CODE_AE                   = $00E6;
 KEY_CODE_CEDILLA_C            = $00E7;
 KEY_CODE_GRAVE_E              = $00E8;
 KEY_CODE_ACUTE_E              = $00E9;
 KEY_CODE_CIRCUMFLEX_E         = $00EA;
 KEY_CODE_DIAERESIS_E          = $00EB;
 KEY_CODE_GRAVE_I              = $00EC;
 KEY_CODE_ACUTE_I              = $00ED;
 KEY_CODE_CIRCUMFLEX_I         = $00EE;
 KEY_CODE_DIAERESIS_I          = $00EF;
 KEY_CODE_ETH                  = $00F0;
 KEY_CODE_TILDE_N              = $00F1;
 KEY_CODE_GRAVE_O              = $00F2;
 KEY_CODE_ACUTE_O              = $00F3;
 KEY_CODE_CIRCUMFLEX_O         = $00F4;
 KEY_CODE_TILDE_O              = $00F5;
 KEY_CODE_DIAERESIS_O          = $00F6;
 KEY_CODE_DIVISION             = $00F7;
 KEY_CODE_STROKE_O             = $00F8;
 KEY_CODE_GRAVE_U              = $00F9;
 KEY_CODE_ACUTE_U              = $00FA;
 KEY_CODE_CIRCUMFLEX_U         = $00FB;
 KEY_CODE_DIAERESIS_U          = $00FC;
 KEY_CODE_ACUTE_Y              = $00FD;
 KEY_CODE_THORN                = $00FE;
 KEY_CODE_DIAERESIS_Y          = $00FF;

 {General Punctuation (2000-206F) (Partial, add extras as required)}
 KEY_CODE_LEFT_QUOTE          = $2018;
 KEY_CODE_RIGHT_QUOTE         = $2019;
 
 {Currency Symbols (20A0-20CF) (Partial, add extras as required)}
 KEY_CODE_EURO                = $20AC;
 
 {Private Area (E000-F8FF)}
 {Non Character Codes}
 KEY_CODE_CAPSLOCK            = $E000; 
 KEY_CODE_F1                  = $E001; 
 KEY_CODE_F2                  = $E002; 
 KEY_CODE_F3                  = $E003; 
 KEY_CODE_F4                  = $E004; 
 KEY_CODE_F5                  = $E005; 
 KEY_CODE_F6                  = $E006; 
 KEY_CODE_F7                  = $E007; 
 KEY_CODE_F8                  = $E008; 
 KEY_CODE_F9                  = $E009; 
 KEY_CODE_F10                 = $E00A; 
 KEY_CODE_F11                 = $E00B; 
 KEY_CODE_F12                 = $E00C; 
 KEY_CODE_PRINTSCREEN         = $E00D; 
 KEY_CODE_SCROLLLOCK          = $E00E; 
 KEY_CODE_PAUSE               = $E00F; 
 KEY_CODE_INSERT              = $E010; 
 KEY_CODE_HOME                = $E011; 
 KEY_CODE_PAGEUP              = $E012; 
 KEY_CODE_END                 = $E013; 
 KEY_CODE_PAGEDN              = $E014; 
 KEY_CODE_RIGHT_ARROW         = $E015; 
 KEY_CODE_LEFT_ARROW          = $E016; 
 KEY_CODE_DOWN_ARROW          = $E017; 
 KEY_CODE_UP_ARROW            = $E018; 
 KEY_CODE_NUMLOCK             = $E019; 
 KEY_CODE_APPLICATION         = $E01A; 
 KEY_CODE_POWER               = $E01B; 
 KEY_CODE_F13                 = $E01C; 
 KEY_CODE_F14                 = $E01D; 
 KEY_CODE_F15                 = $E01E; 
 KEY_CODE_F16                 = $E01F; 
 KEY_CODE_F17                 = $E020; 
 KEY_CODE_F18                 = $E021; 
 KEY_CODE_F19                 = $E022; 
 KEY_CODE_F20                 = $E023; 
 KEY_CODE_F21                 = $E024; 
 KEY_CODE_F22                 = $E025; 
 KEY_CODE_F23                 = $E026; 
 KEY_CODE_F24                 = $E027; 
 KEY_CODE_EXECUTE             = $E028; 
 KEY_CODE_HELP                = $E029; 
 KEY_CODE_MENU                = $E02A; 
 KEY_CODE_SELECT              = $E02B; 
 KEY_CODE_STOP                = $E02C; 
 KEY_CODE_AGAIN               = $E02D; 
 KEY_CODE_UNDO                = $E02E; 
 KEY_CODE_CUT                 = $E02F; 
 KEY_CODE_COPY                = $E030; 
 KEY_CODE_PASTE               = $E031; 
 KEY_CODE_FIND                = $E032; 
 KEY_CODE_MUTE                = $E033; 
 KEY_CODE_VOLUMEUP            = $E034; 
 KEY_CODE_VOLUMEDOWN          = $E035; 
 KEY_CODE_LOCKING_CAPSLOCK    = $E036; 
 KEY_CODE_LOCKING_NUMLOCK     = $E037; 
 KEY_CODE_LOCKING_SCROLLLOCK  = $E038; 
 KEY_CODE_INTERNATIONAL1      = $E039; 
 KEY_CODE_INTERNATIONAL2      = $E03A; 
 KEY_CODE_INTERNATIONAL3      = $E03B; 
 KEY_CODE_INTERNATIONAL4      = $E03C; 
 KEY_CODE_INTERNATIONAL5      = $E03D; 
 KEY_CODE_INTERNATIONAL6      = $E03E; 
 KEY_CODE_INTERNATIONAL7      = $E03F; 
 KEY_CODE_INTERNATIONAL8      = $E040; 
 KEY_CODE_INTERNATIONAL9      = $E041; 
 KEY_CODE_LANG1               = $E042; 
 KEY_CODE_LANG2               = $E043; 
 KEY_CODE_LANG3               = $E044; 
 KEY_CODE_LANG4               = $E045; 
 KEY_CODE_LANG5               = $E046; 
 KEY_CODE_LANG6               = $E047; 
 KEY_CODE_LANG7               = $E048; 
 KEY_CODE_LANG8               = $E049; 
 KEY_CODE_LANG9               = $E04A; 
 KEY_CODE_ALT_ERASE           = $E04B; 
 KEY_CODE_SYSREQ              = $E04C; 
 KEY_CODE_CANCEL              = $E04D; 
 KEY_CODE_CLEAR               = $E04E; 
 KEY_CODE_PRIOR               = $E04F; 
 KEY_CODE_RETURN              = $E050; 
 KEY_CODE_SEPARATOR           = $E051; 
 KEY_CODE_OUT                 = $E052; 
 KEY_CODE_OPER                = $E053; 
 KEY_CODE_CLEAR_AGAIN         = $E054; 
 KEY_CODE_CRSEL_PROPS         = $E055; 
 KEY_CODE_EXSEL               = $E056; 
 KEY_CODE_00                  = $E057; 
 KEY_CODE_000                 = $E058; 
 KEY_CODE_THOUSANDS_SEPARATOR = $E059; 
 KEY_CODE_DECIMAL_SEPARATOR   = $E05A; 
 KEY_CODE_CURRENCY_UNIT       = $E05B; 
 KEY_CODE_CURRENCY_SUBUNIT    = $E05C; 
 KEY_CODE_XOR                 = $E05D; 
 KEY_CODE_MEM_STORE           = $E05E; 
 KEY_CODE_MEM_RECALL          = $E05F; 
 KEY_CODE_MEM_CLEAR           = $E060; 
 KEY_CODE_MEM_ADD             = $E061; 
 KEY_CODE_MEM_SUBTRACT        = $E062; 
 KEY_CODE_MEM_MULTIPLY        = $E063; 
 KEY_CODE_MEM_DIVIDE          = $E064; 
 KEY_CODE_CLEAR_ENTRY         = $E065; 
 KEY_CODE_BINARY              = $E066; 
 KEY_CODE_OCTAL               = $E067; 
 KEY_CODE_DECIMAL             = $E068; 
 KEY_CODE_HEX                 = $E069; 
 KEY_CODE_CTRL                = $E06A; 
 KEY_CODE_SHIFT               = $E06B; 
 KEY_CODE_ALT                 = $E06C; 
 KEY_CODE_GUI                 = $E06D; 
 KEY_CODE_DOUBLE_AMPERSAND    = $E06E;  
 KEY_CODE_DOUBLE_PIPE         = $E06F;  
 KEY_CODE_CENTER              = $E070;
  
 KEY_CODE_TRANSLATE_START     = $0080; {Key codes below this are direct characters in all code pages}
 KEY_CODE_PRIVATE_START       = $E000; {Key codes in this range are private area mappings for non character keys}
 KEY_CODE_PRIVATE_END         = $F8FF;
 
const 
 {Universal scan code constants}
 {Keyboard scan codes are based on the USB HID Usages (See Section 10 of the Universal Serial Bus HID Usage Tables v1.12)}
 {These are the values reported in the ScanCode field of the TKeyboardData structure returned by the KeyboardRead function}
 {Any keyboard driver supporting legacy keyboards (eg PC/AT or PS/2) should translate the reported codes to be compatible with this set}
 SCAN_CODE_NONE                    = 0;   {Reserved (no event indicated)}
 SCAN_CODE_ROLLOVER                = 1;   {Keyboard ErrorRollOver}
 SCAN_CODE_POSTFAIL                = 2;   {Keyboard POSTFail}
 SCAN_CODE_ERROR                   = 3;   {Keyboard ErrorUndefined}
 SCAN_CODE_A                       = 4;   {Keyboard a or A}
 SCAN_CODE_B                       = 5;   {Keyboard b or B}
 SCAN_CODE_C                       = 6;   {Keyboard c or C}
 SCAN_CODE_D                       = 7;   {Keyboard d or D}
 SCAN_CODE_E                       = 8;   {Keyboard e or E}
 SCAN_CODE_F                       = 9;   {Keyboard f or F}
 SCAN_CODE_G                       = 10;  {Keyboard g or G}
 SCAN_CODE_H                       = 11;  {Keyboard h or H}
 SCAN_CODE_I                       = 12;  {Keyboard i or I}
 SCAN_CODE_J                       = 13;  {Keyboard j or J}
 SCAN_CODE_K                       = 14;  {Keyboard k or K}
 SCAN_CODE_L                       = 15;  {Keyboard l or L}
 SCAN_CODE_M                       = 16;  {Keyboard m or M}
 SCAN_CODE_N                       = 17;  {Keyboard n or N}
 SCAN_CODE_O                       = 18;  {Keyboard o or O}
 SCAN_CODE_P                       = 19;  {Keyboard p or P}
 SCAN_CODE_Q                       = 20;  {Keyboard q or Q}
 SCAN_CODE_R                       = 21;  {Keyboard r or R}
 SCAN_CODE_S                       = 22;  {Keyboard s or S}
 SCAN_CODE_T                       = 23;  {Keyboard t or T}
 SCAN_CODE_U                       = 24;  {Keyboard u or U}
 SCAN_CODE_V                       = 25;  {Keyboard v or V}
 SCAN_CODE_W                       = 26;  {Keyboard w or W}
 SCAN_CODE_X                       = 27;  {Keyboard x or X}
 SCAN_CODE_Y                       = 28;  {Keyboard y or Y}
 SCAN_CODE_Z                       = 29;  {Keyboard z or Z}
 SCAN_CODE_1                       = 30;  {Keyboard 1 or !}
 SCAN_CODE_2                       = 31;  {Keyboard 2 or @}
 SCAN_CODE_3                       = 32;  {Keyboard 3 or #}
 SCAN_CODE_4                       = 33;  {Keyboard 4 or $}
 SCAN_CODE_5                       = 34;  {Keyboard 5 or %}
 SCAN_CODE_6                       = 35;  {Keyboard 6 or ^}
 SCAN_CODE_7                       = 36;  {Keyboard 7 or &}
 SCAN_CODE_8                       = 37;  {Keyboard 8 or *}
 SCAN_CODE_9                       = 38;  {Keyboard 9 or (}
 SCAN_CODE_0                       = 39;  {Keyboard 0 or )}
 SCAN_CODE_ENTER                   = 40;  {Keyboard Enter)}
 SCAN_CODE_ESCAPE                  = 41;  {Keyboard Escape}
 SCAN_CODE_BACKSPACE               = 42;  {Keyboard Backspace}
 SCAN_CODE_TAB                     = 43;  {Keyboard Tab}       
 SCAN_CODE_SPACE                   = 44;  {Keyboard Spacebar}  
 SCAN_CODE_MINUS                   = 45;  {Keyboard - or _}
 SCAN_CODE_EQUALS                  = 46;  {Keyboard = or +}
 SCAN_CODE_LEFT_SQUARE             = 47;  {Keyboard [ or Left Brace}
 SCAN_CODE_RIGHT_SQUARE            = 48;  {Keyboard ] or Right Brace}
 SCAN_CODE_BACKSLASH               = 49;  {Keyboard \ or |}
 SCAN_CODE_NONUS_NUMBER            = 50;  {Keyboard Non-US # and ~}
 SCAN_CODE_SEMICOLON               = 51;  {Keyboard ; or :}
 SCAN_CODE_APOSTROPHE              = 52;  {Keyboard ' or "}
 SCAN_CODE_GRAVE                   = 53;  {Keyboard ` or ~}
 SCAN_CODE_COMMA                   = 54;  {Keyboard , or <}
 SCAN_CODE_PERIOD                  = 55;  {Keyboard . or >}
 SCAN_CODE_SLASH                   = 56;  {Keyboard / or ?}
 SCAN_CODE_CAPSLOCK                = 57;  {Keyboard Caps Lock}
 SCAN_CODE_F1                      = 58;  {Keyboard F1}
 SCAN_CODE_F2                      = 59;  {Keyboard F2}
 SCAN_CODE_F3                      = 60;  {Keyboard F3}
 SCAN_CODE_F4                      = 61;  {Keyboard F4}
 SCAN_CODE_F5                      = 62;  {Keyboard F5}
 SCAN_CODE_F6                      = 63;  {Keyboard F6}
 SCAN_CODE_F7                      = 64;  {Keyboard F7}
 SCAN_CODE_F8                      = 65;  {Keyboard F8}
 SCAN_CODE_F9                      = 66;  {Keyboard F9}
 SCAN_CODE_F10                     = 67;  {Keyboard F10}
 SCAN_CODE_F11                     = 68;  {Keyboard F11}
 SCAN_CODE_F12                     = 69;  {Keyboard F12}
 SCAN_CODE_PRINTSCREEN             = 70;  {Keyboard Print Screen}
 SCAN_CODE_SCROLLLOCK              = 71;  {Keyboard Scroll Lock}
 SCAN_CODE_PAUSE                   = 72;  {Keyboard Pause}
 SCAN_CODE_INSERT                  = 73;  {Keyboard Insert}
 SCAN_CODE_HOME                    = 74;  {Keyboard Home}
 SCAN_CODE_PAGEUP                  = 75;  {Keyboard PageUp}
 SCAN_CODE_DELETE                  = 76;  {Keyboard Delete}
 SCAN_CODE_END                     = 77;  {Keyboard End}
 SCAN_CODE_PAGEDN                  = 78;  {Keyboard PageDn}
 SCAN_CODE_RIGHT_ARROW             = 79;  {Keyboard Right Arrow}
 SCAN_CODE_LEFT_ARROW              = 80;  {Keyboard Left Arrow}
 SCAN_CODE_DOWN_ARROW              = 81;  {Keyboard Down Arrow}
 SCAN_CODE_UP_ARROW                = 82;  {Keyboard Up Arrow}
 SCAN_CODE_NUMLOCK                 = 83;  {Keyboard Num Lock}
 SCAN_CODE_KEYPAD_SLASH            = 84;  {Keypad /}                 
 SCAN_CODE_KEYPAD_ASTERISK         = 85;  {Keypad *}                 
 SCAN_CODE_KEYPAD_MINUS            = 86;  {Keypad -}                 
 SCAN_CODE_KEYPAD_PLUS             = 87;  {Keypad +}                
 SCAN_CODE_KEYPAD_ENTER            = 88;  {Keypad Enter}             
 SCAN_CODE_KEYPAD_1                = 89;  {Keypad 1 and End}         
 SCAN_CODE_KEYPAD_2                = 90;  {Keypad 2 and Down Arrow}  
 SCAN_CODE_KEYPAD_3                = 91;  {Keypad 3 and PageDn}                       
 SCAN_CODE_KEYPAD_4                = 92;  {Keypad 4 and Left Arrow}
 SCAN_CODE_KEYPAD_5                = 93;  {Keypad 5 and Center}
 SCAN_CODE_KEYPAD_6                = 94;  {Keypad 6 and Right Arrow}
 SCAN_CODE_KEYPAD_7                = 95;  {Keypad 7 and Home}
 SCAN_CODE_KEYPAD_8                = 96;  {Keypad 8 and Up Arrow}
 SCAN_CODE_KEYPAD_9                = 97;  {Keypad 9 and PageUp}
 SCAN_CODE_KEYPAD_0                = 98;  {Keypad 0 and Insert}
 SCAN_CODE_KEYPAD_PERIOD           = 99;  {Keypad . and Delete}
 SCAN_CODE_NONUS_BACKSLASH         = 100; {Keyboard Non-US \ and |}
 SCAN_CODE_APPLICATION             = 101; {Keyboard Application}
 SCAN_CODE_POWER                   = 102; {Keyboard Power}
 SCAN_CODE_KEYPAD_EQUALS           = 103; {Keypad =}
 SCAN_CODE_F13                     = 104; {Keyboard F13}
 SCAN_CODE_F14                     = 105; {Keyboard F14}
 SCAN_CODE_F15                     = 106; {Keyboard F15}
 SCAN_CODE_F16                     = 107; {Keyboard F16}
 SCAN_CODE_F17                     = 108; {Keyboard F17}
 SCAN_CODE_F18                     = 109; {Keyboard F18}
 SCAN_CODE_F19                     = 110; {Keyboard F19}
 SCAN_CODE_F20                     = 111; {Keyboard F20}
 SCAN_CODE_F21                     = 112; {Keyboard F21}
 SCAN_CODE_F22                     = 113; {Keyboard F22}
 SCAN_CODE_F23                     = 114; {Keyboard F23}
 SCAN_CODE_F24                     = 115; {Keyboard F24}
 SCAN_CODE_EXECUTE                 = 116; {Keyboard Execute}
 SCAN_CODE_HELP                    = 117; {Keyboard Help}
 SCAN_CODE_MENU                    = 118; {Keyboard Menu}
 SCAN_CODE_SELECT                  = 119; {Keyboard Select}
 SCAN_CODE_STOP                    = 120; {Keyboard Stop}
 SCAN_CODE_AGAIN                   = 121; {Keyboard Again}
 SCAN_CODE_UNDO                    = 122; {Keyboard Undo}
 SCAN_CODE_CUT                     = 123; {Keyboard Cut}
 SCAN_CODE_COPY                    = 124; {Keyboard Copy}
 SCAN_CODE_PASTE                   = 125; {Keyboard Paste}
 SCAN_CODE_FIND                    = 126; {Keyboard Find}
 SCAN_CODE_MUTE                    = 127; {Keyboard Mute}
 SCAN_CODE_VOLUMEUP                = 128; {Keyboard Volume Up}
 SCAN_CODE_VOLUMEDN                = 129; {Keyboard Volume Down}
 SCAN_CODE_LOCKING_CAPSLOCK        = 130; {Keyboard Locking Caps Lock}
 SCAN_CODE_LOCKING_NUMLOCK         = 131; {Keyboard Locking Num Lock}
 SCAN_CODE_LOCKING_SCROLLLOCK      = 132; {Keyboard Locking Scroll Lock}
 SCAN_CODE_KEYPAD_COMMA            = 133; {Keypad Comma}
 SCAN_CODE_KEYPAD_EQUAL_SIGN       = 134; {Keypad Equal Sign}
 SCAN_CODE_INTERNATIONAL1          = 135; {Keyboard International1}
 SCAN_CODE_INTERNATIONAL2          = 136; {Keyboard International2}
 SCAN_CODE_INTERNATIONAL3          = 137; {Keyboard International3}
 SCAN_CODE_INTERNATIONAL4          = 138; {Keyboard International4}
 SCAN_CODE_INTERNATIONAL5          = 139; {Keyboard International5}
 SCAN_CODE_INTERNATIONAL6          = 140; {Keyboard International6}
 SCAN_CODE_INTERNATIONAL7          = 141; {Keyboard International7}
 SCAN_CODE_INTERNATIONAL8          = 142; {Keyboard International8}
 SCAN_CODE_INTERNATIONAL9          = 143; {Keyboard International9}
 SCAN_CODE_LANG1                   = 144; {Keyboard LANG1}
 SCAN_CODE_LANG2                   = 145; {Keyboard LANG2}
 SCAN_CODE_LANG3                   = 146; {Keyboard LANG3}
 SCAN_CODE_LANG4                   = 147; {Keyboard LANG4}
 SCAN_CODE_LANG5                   = 148; {Keyboard LANG5}
 SCAN_CODE_LANG6                   = 149; {Keyboard LANG6}
 SCAN_CODE_LANG7                   = 150; {Keyboard LANG7}
 SCAN_CODE_LANG8                   = 151; {Keyboard LANG8}
 SCAN_CODE_LANG9                   = 152; {Keyboard LANG9}
 SCAN_CODE_ALT_ERASE               = 153; {Keyboard Alternate Erase}
 SCAN_CODE_SYSREQ                  = 154; {Keyboard SysReq/Attention}
 SCAN_CODE_CANCEL                  = 155; {Keyboard Cancel}
 SCAN_CODE_CLEAR                   = 156; {Keyboard Clear}
 SCAN_CODE_PRIOR                   = 157; {Keyboard Prior}
 SCAN_CODE_RETURN                  = 158; {Keyboard Return}
 SCAN_CODE_SEPARATOR               = 159; {Keyboard Separator}
 SCAN_CODE_OUT                     = 160; {Keyboard Out}
 SCAN_CODE_OPER                    = 161; {Keyboard Oper}
 SCAN_CODE_CLEAR_AGAIN             = 162; {Keyboard Clear/Again}
 SCAN_CODE_CRSEL_PROPS             = 163; {Keyboard CrSel/Props}
 SCAN_CODE_EXSEL                   = 164; {Keyboard ExSel}
 {Codes 165 to 175 Reserved}
 SCAN_CODE_KEYPAD_00               = 176; {Keypad 00}
 SCAN_CODE_KEYPAD_000              = 177; {Keypad 000}
 SCAN_CODE_THOUSANDS_SEPARATOR     = 178; {Thousands Separator}
 SCAN_CODE_DECIMAL_SEPARATOR       = 179; {Decimal Separator}
 SCAN_CODE_CURRENCY_UNIT           = 180; {Currency Unit}
 SCAN_CODE_CURRENCY_SUBUNIT        = 181; {Currenct Sub-unit}
 SCAN_CODE_KEYPAD_LEFT_BRACKET     = 182; {Keypad (}
 SCAN_CODE_KEYPAD_RIGHT_BRACKET    = 183; {Keypad )}
 SCAN_CODE_KEYPAD_LEFT_BRACE       = 184; {Keypad Left Brace}
 SCAN_CODE_KEYPAD_RIGHT_BRACE      = 185; {Keypad Right Brace}
 SCAN_CODE_KEYPAD_TAB              = 186; {Keypad Tab}
 SCAN_CODE_KEYPAD_BACKSPACE        = 187; {Keypad Backspace}
 SCAN_CODE_KEYPAD_A                = 188; {Keypad A}
 SCAN_CODE_KEYPAD_B                = 189; {Keypad B}
 SCAN_CODE_KEYPAD_C                = 190; {Keypad C}
 SCAN_CODE_KEYPAD_D                = 191; {Keypad D}
 SCAN_CODE_KEYPAD_E                = 192; {Keypad E}
 SCAN_CODE_KEYPAD_F                = 193; {Keypad F}
 SCAN_CODE_KEYPAD_XOR              = 194; {Keypad XOR}
 SCAN_CODE_KEYPAD_CARET            = 195; {Keypad ^}
 SCAN_CODE_KEYPAD_PERCENT          = 196; {Keypad %}
 SCAN_CODE_KEYPAD_LESSTHAN         = 197; {Keypad <}
 SCAN_CODE_KEYPAD_GREATERTHAN      = 198; {Keypad >}
 SCAN_CODE_KEYPAD_AMPERSAND        = 199; {Keypad &}
 SCAN_CODE_KEYPAD_DOUBLE_AMPERSAND = 200; {Keypad &&}
 SCAN_CODE_KEYPAD_PIPE             = 201; {Keypad |}
 SCAN_CODE_KEYPAD_DOUBLE_PIPE      = 202; {Keypad ||}
 SCAN_CODE_KEYPAD_COLON            = 203; {Keypad :}
 SCAN_CODE_KEYPAD_NUMBER           = 204; {Keypad #}
 SCAN_CODE_KEYPAD_SPACE            = 205; {Keypad Space}
 SCAN_CODE_KEYPAD_AT               = 206; {Keypad @}
 SCAN_CODE_KEYPAD_EXCLAMATION      = 207; {Keypad !}
 SCAN_CODE_KEYPAD_MEM_STORE        = 208; {Keypad Memory Store}
 SCAN_CODE_KEYPAD_MEM_RECALL       = 209; {Keypad Memory Recall}
 SCAN_CODE_KEYPAD_MEM_CLEAR        = 210; {Keypad Memory Clear}
 SCAN_CODE_KEYPAD_MEM_ADD          = 211; {Keypad Memory Add}
 SCAN_CODE_KEYPAD_MEM_SUB          = 212; {Keypad Memory Subtract}
 SCAN_CODE_KEYPAD_MEM_MULTIPLY     = 213; {Keypad Memory Multiply}
 SCAN_CODE_KEYPAD_MEM_DIVIDE       = 214; {Keypad Memory Divide}
 SCAN_CODE_KEYPAD_PLUS_MINUS       = 215; {Keypad +/-}
 SCAN_CODE_KEYPAD_CLEAR            = 216; {Keypad Clear}
 SCAN_CODE_KEYPAD_CLEAR_ENTRY      = 217; {Keypad Clear Entry}
 SCAN_CODE_KEYPAD_BINARY           = 218; {Keypad Binary}
 SCAN_CODE_KEYPAD_OCTAL            = 219; {Keypad Octal}
 SCAN_CODE_KEYPAD_DECIMAL          = 220; {Keypad Decimal}
 SCAN_CODE_KEYPAD_HEX              = 221; {Keypad Hexadecimal}
 {Codes 222 to 223 Reserved}
 SCAN_CODE_LEFT_CTRL               = 224; {Keyboard LeftControl}
 SCAN_CODE_LEFT_SHIFT              = 225; {Keyboard LeftShift}
 SCAN_CODE_LEFT_ALT                = 226; {Keyboard LeftAlt}
 SCAN_CODE_LEFT_GUI                = 227; {Keyboard Left GUI}
 SCAN_CODE_RIGHT_CTRL              = 228; {Keyboard RightControl}
 SCAN_CODE_RIGHT_SHIFT             = 229; {Keyboard RightShift}
 SCAN_CODE_RIGHT_ALT               = 230; {Keyboard RightAlt}
 SCAN_CODE_RIGHT_GUI               = 231; {Keyboard Right GUI}
 {Codes 232 to 65535 Reserved}
 
 {Alternate names for above}
 SCAN_CODE_EXCLAMATION             = 30;  {Keyboard 1 or !}
 SCAN_CODE_AT                      = 31;  {Keyboard 2 or @}
 SCAN_CODE_NUMBER                  = 32;  {Keyboard 3 or #}
 SCAN_CODE_CURRENCY                = 33;  {Keyboard 4 or $}
 SCAN_CODE_PERCENT                 = 34;  {Keyboard 5 or %}
 SCAN_CODE_CARET                   = 35;  {Keyboard 6 or ^}
 SCAN_CODE_AMPERSAND               = 36;  {Keyboard 7 or &}
 SCAN_CODE_ASTERISK                = 37;  {Keyboard 8 or *}
 SCAN_CODE_LEFT_BRACKET            = 38;  {Keyboard 9 or (}
 SCAN_CODE_RIGHT_BRACKET           = 39;  {Keyboard 0 or )}
 
 SCAN_CODE_DASH                    = 45;  {Keyboard - or _}
 SCAN_CODE_UNDERSCORE              = 45;  {Keyboard - or _}
 SCAN_CODE_PLUS                    = 46;  {Keyboard = or +}
 SCAN_CODE_LEFT_BRACE              = 47;  {Keyboard [ or Left Brace}
 SCAN_CODE_RIGHT_BRACE             = 48;  {Keyboard ] or Right Brace}
 SCAN_CODE_PIPE                    = 49;  {Keyboard \ or |}
 SCAN_CODE_NONUS_TILDE             = 50;  {Keyboard Non-US # and ~}
 SCAN_CODE_COLON                   = 51;  {Keyboard ; or :}
 SCAN_CODE_QUOTATION               = 52;  {Keyboard ' or "}
 SCAN_CODE_TILDE                   = 53;  {Keyboard ` or ~}
 SCAN_CODE_LESSTHAN                = 54;  {Keyboard , or <}
 SCAN_CODE_GREATERTHAN             = 55;  {Keyboard . or >}
 SCAN_CODE_QUESTION                = 56;  {Keyboard / or ?}
 
 SCAN_CODE_ALTGR                   = SCAN_CODE_RIGHT_ALT; {Keyboard RightAlt}
 
 SCAN_CODE_KEYPAD_FIRST            = SCAN_CODE_NUMLOCK;
 SCAN_CODE_KEYPAD_LAST             = SCAN_CODE_KEYPAD_PERIOD;
 
{==============================================================================}
const
 {Universal color constants}
 {Basic 32 bit RGB colors (8 bit Transparency, 8 bit Red, 8 bit Green, 8 bit Blue)}
 COLOR_NONE      = $00000000;
 COLOR_BLACK     = $FF000000;
 COLOR_RED       = $FFFF0000;
 COLOR_ORANGE    = $FFFF8500; 
 COLOR_LEAFGREEN = $FF009900;
 COLOR_GREEN     = $FF00FF00;
 COLOR_YELLOW    = $FFFFFF00;
 COLOR_DARKGREEN = $FF254117;
 COLOR_DARKGRAY  = $FF595959; 
 COLOR_BROWN     = $FF6F4E37;
 COLOR_INDIGO    = $FF2E0854;
 COLOR_RASPBERRY = $FFE30B5C;
 COLOR_GRAY      = $FF808080;
 COLOR_PURPLE    = $FF4B0082;
 COLOR_PINK      = $FFFF0090;
 COLOR_DARKBLUE  = $FF0000A0;
 COLOR_SILVER    = $FFC0C0C0;
 COLOR_BLUEIVY   = $FF3090C7;
 COLOR_MIDGRAY   = $FFE0E0E0;
 COLOR_LIGHTGRAY = $FFF7F7F7;
 COLOR_BLUE      = $FF0000FF;
 COLOR_MAGENTA   = $FFFF00FF;
 COLOR_CYAN      = $FF00FFFF;
 COLOR_WHITE     = $FFFFFFFF;
 
 {Ultibo release RGB colors}
 COLOR_ULTIBO_LETTUCE  = $FF000000; {Ultibo Version 0.0 (Lettuce)}
 COLOR_ULTIBO_POTATO   = $FF0000A0; {Ultibo Version 1.0 (Potato)}
 COLOR_ULTIBO_CUCUMBER = $FFFF8500; {Ultibo Version 1.x (Cucumber)}
 COLOR_ULTIBO_BEETROOT = $FF00BB00; {Ultibo Version 2.x (Beetroot)}
 
 COLOR_ULTIBO = COLOR_ULTIBO_BEETROOT;
 
 {Color format constants}
 {See: https://en.wikipedia.org/wiki/Color_depth and https://en.wikipedia.org/wiki/RGBA_color_space}
 COLOR_FORMAT_ARGB32  = 0;  {32 bits per pixel Alpha/Red/Green/Blue (ARGB8888)}
 COLOR_FORMAT_ABGR32  = 1;  {32 bits per pixel Alpha/Blue/Green/Red (ABGR8888)}
 COLOR_FORMAT_RGBA32  = 2;  {32 bits per pixel Red/Green/Blue/Alpha (RGBA8888)}
 COLOR_FORMAT_BGRA32  = 3;  {32 bits per pixel Blue/Green/Red/Alpha (BGRA8888)}
 COLOR_FORMAT_URGB32  = 4;  {32 bits per pixel Unused/Red/Green/Blue (URGB8888)}
 COLOR_FORMAT_UBGR32  = 5;  {32 bits per pixel Unused/Blue/Green/Red (UBGR8888)}
 COLOR_FORMAT_RGBU32  = 6;  {32 bits per pixel Red/Green/Blue/Unused (RGBU8888)}
 COLOR_FORMAT_BGRU32  = 7;  {32 bits per pixel Blue/Green/Red/Unused (BGRU8888)}
 COLOR_FORMAT_RGB24   = 8;  {24 bits per pixel Red/Green/Blue (RGB888)}
 COLOR_FORMAT_BGR24   = 9;  {24 bits per pixel Blue/Green/Red (BGR888)}
 COLOR_FORMAT_RGB16   = 10; {16 bits per pixel Red/Green/Blue (RGB565)}
 COLOR_FORMAT_BGR16   = 11; {16 bits per pixel Blue/Green/Red (BGR565)}
 COLOR_FORMAT_RGB15   = 12; {15 bits per pixel Red/Green/Blue (RGB555)}
 COLOR_FORMAT_BGR15   = 13; {15 bits per pixel Blue/Green/Red (BGR555)}
 COLOR_FORMAT_RGB8    = 14; {8 bits per pixel Red/Green/Blue (RGB332)} 
 COLOR_FORMAT_BGR8    = 15; {8 bits per pixel Blue/Green/Red (BGR233)} 
 COLOR_FORMAT_GRAY16  = 16; {16 bits per pixel grayscale}
 COLOR_FORMAT_GRAY8   = 17; {8 bits per pixel grayscale}
 COLOR_FORMAT_INDEX16 = 18; {16 bits per pixel palette index}
 COLOR_FORMAT_INDEX8  = 19; {8 bits per pixel palette index}
 
 COLOR_FORMAT_MAX     = 19;
 
 COLOR_FORMAT_DEFAULT = COLOR_FORMAT_ARGB32; {The default color format (Used for the COLOR_* constants above)}
 
 COLOR_FORMAT_UNKNOWN = LongWord(-1); 
 
{==============================================================================}
const
 {Universal size constants (From /include/linux/sizes.h)}
 SIZE_0     = $00000000;
 SIZE_1     = $00000001;
 SIZE_2     = $00000002;
 SIZE_4     = $00000004;
 SIZE_8     = $00000008;
 SIZE_16    = $00000010;
 SIZE_32    = $00000020;
 SIZE_64    = $00000040;
 SIZE_128   = $00000080;
 SIZE_256   = $00000100;
 SIZE_512   = $00000200;

 SIZE_1K    = $00000400;
 SIZE_2K    = $00000800;
 SIZE_4K    = $00001000;
 SIZE_8K    = $00002000;
 SIZE_16K   = $00004000;
 SIZE_32K   = $00008000;
 SIZE_64K   = $00010000;
 SIZE_128K  = $00020000;
 SIZE_256K  = $00040000;
 SIZE_512K  = $00080000;

 SIZE_1M    = $00100000;
 SIZE_2M    = $00200000;
 SIZE_4M    = $00400000;
 SIZE_8M    = $00800000;
 SIZE_16M   = $01000000;
 SIZE_32M   = $02000000;
 SIZE_64M   = $04000000;
 SIZE_128M  = $08000000;
 SIZE_256M  = $10000000;
 SIZE_512M  = $20000000;

 SIZE_1G    = $40000000;
 SIZE_2G    = $80000000;
 
{==============================================================================}
const
 {Universal time constants}
 MILLISECONDS_PER_SECOND = 1000;
 MICROSECONDS_PER_SECOND = 1000000;
 NANOSECONDS_PER_SECOND  = 1000000000;
 
const
 {Ultibo time constants (100 nanosecond ticks since 1/1/1601)}
 {Note: Nanoseconds is 10^9 so 100 nanosecond ticks is 10^7}
 TIME_TICKS_PER_MICROSECOND = 10;           {10^7 / 10^6}
 TIME_TICKS_PER_MILLISECOND = 10000;        {10^7 / 10^3}
 TIME_TICKS_PER_SECOND      = 10000000;     {10^7}
 TIME_TICKS_PER_MINUTE      = 600000000;    {60 * 10^7}
 TIME_TICKS_PER_HOUR        = 36000000000;  {60 * 60 * 10^7}
 TIME_TICKS_PER_DAY         = 864000000000; {24 * 60 * 60 * 10^7}

 TIME_TICKS_TO_1899 = 94353120000000000;    {Offset between 1/1/1601 (Ultibo) and 30/12/1899 (FreePascal)}
 TIME_TICKS_TO_1970 = 116444736000000000;   {Offset between 1/1/1601 (Ultibo) and 1/1/1970 (Unix/Linux)}
 TIME_TICKS_TO_1980 = 119600064000000000;   {Offset between 1/1/1601 (Ultibo) and 1/1/1980 (DOS)}
 TIME_TICKS_TO_2001 = 126227808000000000;   {Offset between 1/1/1601 (Ultibo) and 1/1/2001 (Clock is assumed not set if time is less than this)}
 
 TIME_TICKS_PER_10MILLISECONDS = 100000;    {10^7 / 10^2}
 
const
 {Unix/Linux time constants (Seconds since 1/1/1970)}
 UNIX_TIME_SECONDS_PER_DAY =  86400;    {60*60*24;}
 UNIX_TIME_DAYS_TO_1970    =  25569.0;  {Offset between 1899 (FreePascal) and 1970 (Unix/Linux)}
 
const
 {FreePascal time constants (TDateTime starts at 30/12/1899)}
 PASCAL_TIME_MILLISECONDS_PER_DAY   =  86400000; {60*60*24*1000;}
 PASCAL_TIME_SECONDS_PER_DAY        =  86400;    {60*60*24;}
 PASCAL_TIME_DOS_TIME_START         =  2162688;  {DOS date time value for start of DOS time (1/1/1980)}

 PASCAL_DAY_OFFSET = 1.0;                        {TDateTime value 1 day} 
 PASCAL_MINUTE_OFFSET = 0.000694444444444444;    {TDateTime value of 1 minute}

{==============================================================================}
const
 {System Call constants} 
 SYSTEM_CALL_UNDEFINED      = $00000000;
 SYSTEM_CALL_CONTEXT_SWITCH = $00000001;
 
{==============================================================================}
const
 {Machine Type constants} 
 MACHINE_TYPE_UNKNOWN     = 0;
 MACHINE_TYPE_BCM2708     = 1;  {Broadcom BCM2708 (Raspberry Pi)}
 MACHINE_TYPE_BCM2709     = 2;  {Broadcom BCM2709 (Raspberry Pi 2)}
 MACHINE_TYPE_BCM2710     = 3;  {Broadcom BCM2710 (Raspberry Pi 3)}
 MACHINE_TYPE_VERSATILEPB = 4;  {ARM Versatile PB (QEMU)}
 
{==============================================================================}
const
 {Board Type constants}
 BOARD_TYPE_UNKNOWN           = 0;
 BOARD_TYPE_RPIA              = 1;  {Raspberry Pi Model A}
 BOARD_TYPE_RPIB              = 2;  {Raspberry Pi Model B}
 BOARD_TYPE_RPI_COMPUTE       = 3;  {Raspberry Pi Compute Module}
 BOARD_TYPE_RPIA_PLUS         = 4;  {Raspberry Pi Model A+}
 BOARD_TYPE_RPIB_PLUS         = 5;  {Raspberry Pi Model B+}
 BOARD_TYPE_RPI2B             = 6;  {Raspberry Pi 2 Model B}
 BOARD_TYPE_RPI_ZERO          = 7;  {Raspberry Pi Model Zero}
 BOARD_TYPE_PC_X86            = 22; {PC x86}
 BOARD_TYPE_PC_X86_64         = 23; {PC x86 64bit}
 BOARD_TYPE_RPI3B             = 24; {Raspberry Pi 3 Model B}
 BOARD_TYPE_QEMUVPB           = 25; {QEMU Versatile PB}
 BOARD_TYPE_RPI_COMPUTE3      = 26; {Raspberry Pi Compute Module 3}
 BOARD_TYPE_RPI_ZERO_W        = 27; {Raspberry Pi Model Zero W}
 BOARD_TYPE_RPI3B_PLUS        = 28; {Raspberry Pi 3 Model B+}
 BOARD_TYPE_RPI3A_PLUS        = 29; {Raspberry Pi 3 Model A+}
 BOARD_TYPE_RPI_COMPUTE3_PLUS = 30; {Raspberry Pi Compute Module 3+}
 
{==============================================================================}
const 
 {CPU Arch constants}
 CPU_ARCH_UNKNOWN   = 0;
 CPU_ARCH_ARM32     = 1; {ARM Arch 32 (ARMv6/ARMv7)(ARMv8 in 32bit mode)}
 CPU_ARCH_ARM64     = 2; {ARM Arch 64 (ARMv8}
 
const 
 {CPU Type constants}
 CPU_TYPE_UNKNOWN   = 0;
 CPU_TYPE_ARMV6     = 1; {ARMv6 (ARM1176 etc)}
 CPU_TYPE_ARMV7     = 2; {ARMv7 (Cortex A5/A7/A8/A9/A15/A17 etc)}
 CPU_TYPE_ARMV8     = 3; {ARMv8 (Cortex A53/A57/A72 etc)}
 
const
 {CPU Model constants}
 CPU_MODEL_UNKNOWN     = 0;
 CPU_MODEL_ARM1176JZFS = 1;  {ARM1176JZF-S}
 CPU_MODEL_CORTEX_A5   = 2;  {ARM Cortex-A5}
 CPU_MODEL_CORTEX_A7   = 3;  {ARM Cortex-A7}
 CPU_MODEL_CORTEX_A8   = 4;  {ARM Cortex-A8}
 CPU_MODEL_CORTEX_A9   = 5;  {ARM Cortex-A9}
 CPU_MODEL_CORTEX_A15  = 6;  {ARM Cortex-A15}
 CPU_MODEL_CORTEX_A17  = 7;  {ARM Cortex-A17}
 CPU_MODEL_CORTEX_A53  = 8;  {ARM Cortex-A53}
 CPU_MODEL_CORTEX_A57  = 9;  {ARM Cortex-A57}
 CPU_MODEL_CORTEX_A72  = 10; {ARM Cortex-A72}
 
const
 {CPU Description constants}
 CPU_DESCRIPTION_UNKNOWN      = 'Unknown';
 CPU_DESCRIPTION_ARM1176JZFS  = 'ARM1176JZF-S';
 CPU_DESCRIPTION_CORTEX_A5    = 'ARM Cortex-A5';
 CPU_DESCRIPTION_CORTEX_A5_MP = 'ARM Cortex-A5 MPCore';
 CPU_DESCRIPTION_CORTEX_A7    = 'ARM Cortex-A7 MPCore';
 CPU_DESCRIPTION_CORTEX_A8    = 'ARM Cortex-A8';
 CPU_DESCRIPTION_CORTEX_A9    = 'ARM Cortex-A9'; 
 CPU_DESCRIPTION_CORTEX_A9_MP = 'ARM Cortex-A9 MPCore'; 
 CPU_DESCRIPTION_CORTEX_A15   = 'ARM Cortex-A15 MPCore';
 CPU_DESCRIPTION_CORTEX_A17   = 'ARM Cortex-A17 MPCore'; 
 CPU_DESCRIPTION_CORTEX_A53   = 'ARM Cortex-A53 MPCore'; 
 CPU_DESCRIPTION_CORTEX_A57   = 'ARM Cortex-A57 MPCore'; 
 CPU_DESCRIPTION_CORTEX_A72   = 'ARM Cortex-A72 MPCore'; 
 
const 
 {CPU State constants}
 CPU_STATE_NONE                      = (0 shl 0);
 CPU_STATE_MMU_ENABLED               = (1 shl 0);
 CPU_STATE_DATA_CACHE_ENABLED        = (1 shl 1);
 CPU_STATE_INSTRUCTION_CACHE_ENABLED = (1 shl 2);
 CPU_STATE_BRANCH_PREDICTION_ENABLED = (1 shl 3);
 
const
 {CPU Group constants}
 CPU_GROUP_0   = 0;
 CPU_GROUP_1   = 1;
 CPU_GROUP_2   = 2;
 CPU_GROUP_3   = 3;
 CPU_GROUP_4   = 4;
 CPU_GROUP_5   = 5;
 CPU_GROUP_6   = 6;
 CPU_GROUP_7   = 7;
 CPU_GROUP_8   = 8;
 CPU_GROUP_9   = 9;
 CPU_GROUP_10  = 10;
 CPU_GROUP_11  = 11;
 CPU_GROUP_12  = 12;
 CPU_GROUP_13  = 13;
 CPU_GROUP_14  = 14;
 CPU_GROUP_15  = 15;
 CPU_GROUP_16  = 16;
 CPU_GROUP_17  = 17;
 CPU_GROUP_18  = 18;
 CPU_GROUP_19  = 19;
 CPU_GROUP_20  = 20;
 CPU_GROUP_21  = 21;
 CPU_GROUP_22  = 22;
 CPU_GROUP_23  = 23;
 CPU_GROUP_24  = 24;
 CPU_GROUP_25  = 25;
 CPU_GROUP_26  = 26;
 CPU_GROUP_27  = 27;
 CPU_GROUP_28  = 28;
 CPU_GROUP_29  = 29;
 CPU_GROUP_30  = 30;
 CPU_GROUP_31  = 31;
 
 CPU_GROUP_ALL =  $FFFFFFFF;
 
const
 {CPU ID constants}
 CPU_ID_0   = 0;
 CPU_ID_1   = 1;
 CPU_ID_2   = 2;
 CPU_ID_3   = 3;
 CPU_ID_4   = 4;
 CPU_ID_5   = 5;
 CPU_ID_6   = 6;
 CPU_ID_7   = 7;
 CPU_ID_8   = 8;
 CPU_ID_9   = 9;
 CPU_ID_10  = 10;
 CPU_ID_11  = 11;
 CPU_ID_12  = 12;
 CPU_ID_13  = 13;
 CPU_ID_14  = 14;
 CPU_ID_15  = 15;
 CPU_ID_16  = 16;
 CPU_ID_17  = 17;
 CPU_ID_18  = 18;
 CPU_ID_19  = 19;
 CPU_ID_20  = 20;
 CPU_ID_21  = 21;
 CPU_ID_22  = 22;
 CPU_ID_23  = 23;
 CPU_ID_24  = 24;
 CPU_ID_25  = 25;
 CPU_ID_26  = 26;
 CPU_ID_27  = 27;
 CPU_ID_28  = 28;
 CPU_ID_29  = 29;
 CPU_ID_30  = 30;
 CPU_ID_31  = 31;
 
 CPU_ID_ALL =  $FFFFFFFF;
 
const
 {CPU Affinity constants}
 CPU_AFFINITY_0  = (1 shl CPU_ID_0);
 CPU_AFFINITY_1  = (1 shl CPU_ID_1);
 CPU_AFFINITY_2  = (1 shl CPU_ID_2);
 CPU_AFFINITY_3  = (1 shl CPU_ID_3);
 CPU_AFFINITY_4  = (1 shl CPU_ID_4);
 CPU_AFFINITY_5  = (1 shl CPU_ID_5);
 CPU_AFFINITY_6  = (1 shl CPU_ID_6);
 CPU_AFFINITY_7  = (1 shl CPU_ID_7);
 CPU_AFFINITY_8  = (1 shl CPU_ID_8);
 CPU_AFFINITY_9  = (1 shl CPU_ID_9);
 CPU_AFFINITY_10 = (1 shl CPU_ID_10);
 CPU_AFFINITY_11 = (1 shl CPU_ID_11);
 CPU_AFFINITY_12 = (1 shl CPU_ID_12);
 CPU_AFFINITY_13 = (1 shl CPU_ID_13);
 CPU_AFFINITY_14 = (1 shl CPU_ID_14);
 CPU_AFFINITY_15 = (1 shl CPU_ID_15);
 CPU_AFFINITY_16 = (1 shl CPU_ID_16);
 CPU_AFFINITY_17 = (1 shl CPU_ID_17);
 CPU_AFFINITY_18 = (1 shl CPU_ID_18);
 CPU_AFFINITY_19 = (1 shl CPU_ID_19);
 CPU_AFFINITY_20 = (1 shl CPU_ID_20);
 CPU_AFFINITY_21 = (1 shl CPU_ID_21);
 CPU_AFFINITY_22 = (1 shl CPU_ID_22);
 CPU_AFFINITY_23 = (1 shl CPU_ID_23);
 CPU_AFFINITY_24 = (1 shl CPU_ID_24);
 CPU_AFFINITY_25 = (1 shl CPU_ID_25);
 CPU_AFFINITY_26 = (1 shl CPU_ID_26);
 CPU_AFFINITY_27 = (1 shl CPU_ID_27);
 CPU_AFFINITY_28 = (1 shl CPU_ID_28);
 CPU_AFFINITY_29 = (1 shl CPU_ID_29);
 CPU_AFFINITY_30 = (1 shl CPU_ID_30);
 CPU_AFFINITY_31 = (1 shl CPU_ID_31);
 
 CPU_AFFINITY_NONE = $00000000;
 CPU_AFFINITY_ALL  = $FFFFFFFF;
 
{==============================================================================}
const
 {FPU Type constants}
 FPU_TYPE_UNKNOWN  = 0;
 FPU_TYPE_SOFT     = 1;
 FPU_TYPE_VFPV2    = 2;
 FPU_TYPE_VFPV3    = 3;
 FPU_TYPE_VFPV4    = 4;
 
const 
 {FPU State constants}
 FPU_STATE_NONE    = (0 shl 0);
 FPU_STATE_ENABLED = (1 shl 0);

{==============================================================================}
const
 {GPU Type constants}
 GPU_TYPE_UNKNOWN  = 0;
 GPU_TYPE_VC4      = 1; {Broadcom VideoCore IV} 
 GPU_TYPE_MALI400  = 2; {ARM Mali 400}
 GPU_TYPE_MALI450  = 3; {ARM Mali 450}
 GPU_TYPE_GC880    = 4; {Vivante GC880}
 GPU_TYPE_GC2000   = 5; {Vivante GC2000}
 
const 
 {GPU State constants}
 GPU_STATE_NONE    = (0 shl 0);
 GPU_STATE_ENABLED = (1 shl 0);
 
{==============================================================================}
const
 {Cache Type constants}
 CACHE_TYPE_NONE        = 0; {No Cache}
 CACHE_TYPE_DATA        = 1; {Data Cache Only}
 CACHE_TYPE_INSTRUCTION = 2; {Instruction Cache Only}
 CACHE_TYPE_SEPARATE    = 3; {Separate Data and Instruction Caches}
 CACHE_TYPE_UNIFIED     = 4; {Unified Data and Instruction Cache}
 
{==============================================================================}
const
 {DMA Direction constants}
 DMA_DIR_NONE       = 0; {No direction (No special handling by controller)}
 DMA_DIR_MEM_TO_MEM = 1;
 DMA_DIR_MEM_TO_DEV = 2;
 DMA_DIR_DEV_TO_MEM = 3;
 DMA_DIR_DEV_TO_DEV = 4;
 
 {DMA DREQ ID constants}
 DMA_DREQ_ID_NONE          =  0;  {No peripheral gating (memory to memory transfer)}
 DMA_DREQ_ID_UART_TX       =  1;
 DMA_DREQ_ID_UART_RX       =  2;
 DMA_DREQ_ID_SPI_TX        =  3;
 DMA_DREQ_ID_SPI_RX        =  4;
 DMA_DREQ_ID_SPI_SLAVE_TX  =  5;
 DMA_DREQ_ID_SPI_SLAVE_RX  =  6;
 DMA_DREQ_ID_PCM_TX        =  7;
 DMA_DREQ_ID_PCM_RX        =  8;
 DMA_DREQ_ID_PWM           =  9;
 DMA_DREQ_ID_MMC           =  10;
 DMA_DREQ_ID_SDHOST        =  11;
 
{==============================================================================}
const
 {GPIO Pin constants}
 GPIO_PIN_0   =  0;
 GPIO_PIN_1   =  1;
 GPIO_PIN_2   =  2;
 GPIO_PIN_3   =  3;
 GPIO_PIN_4   =  4;
 GPIO_PIN_5   =  5;
 GPIO_PIN_6   =  6;
 GPIO_PIN_7   =  7;
 GPIO_PIN_8   =  8;
 GPIO_PIN_9   =  9;
 GPIO_PIN_10  =  10;
 GPIO_PIN_11  =  11;
 GPIO_PIN_12  =  12;
 GPIO_PIN_13  =  13;
 GPIO_PIN_14  =  14;
 GPIO_PIN_15  =  15;
 GPIO_PIN_16  =  16;
 GPIO_PIN_17  =  17;
 GPIO_PIN_18  =  18;
 GPIO_PIN_19  =  19;
 GPIO_PIN_20  =  20;
 GPIO_PIN_21  =  21;
 GPIO_PIN_22  =  22;
 GPIO_PIN_23  =  23;
 GPIO_PIN_24  =  24;
 GPIO_PIN_25  =  25;
 GPIO_PIN_26  =  26;
 GPIO_PIN_27  =  27;
 GPIO_PIN_28  =  28;
 GPIO_PIN_29  =  29;
 GPIO_PIN_30  =  30;
 GPIO_PIN_31  =  31;
 GPIO_PIN_32  =  32;
 GPIO_PIN_33  =  33;
 GPIO_PIN_34  =  34;
 GPIO_PIN_35  =  35;
 GPIO_PIN_36  =  36;
 GPIO_PIN_37  =  37;
 GPIO_PIN_38  =  38;
 GPIO_PIN_39  =  39;
 GPIO_PIN_40  =  40;
 GPIO_PIN_41  =  41;
 GPIO_PIN_42  =  42;
 GPIO_PIN_43  =  43;
 GPIO_PIN_44  =  44;
 GPIO_PIN_45  =  45;
 GPIO_PIN_46  =  46;
 GPIO_PIN_47  =  47;
 GPIO_PIN_48  =  48;
 GPIO_PIN_49  =  49;
 GPIO_PIN_50  =  50;
 GPIO_PIN_51  =  51;
 GPIO_PIN_52  =  52;
 GPIO_PIN_53  =  53;
 GPIO_PIN_54  =  54;
 GPIO_PIN_55  =  55;
 GPIO_PIN_56  =  56;
 GPIO_PIN_57  =  57;
 GPIO_PIN_58  =  58;
 GPIO_PIN_59  =  59;
 GPIO_PIN_60  =  60;
 
 GPIO_PIN_MAX =  60;
 
 GPIO_PIN_UNKNOWN = LongWord(-1); 
 
const
 {GPIO Function constants}
 GPIO_FUNCTION_IN    =  0;
 GPIO_FUNCTION_OUT   =  1;
 GPIO_FUNCTION_ALT0  =  2;
 GPIO_FUNCTION_ALT1  =  3;
 GPIO_FUNCTION_ALT2  =  4;
 GPIO_FUNCTION_ALT3  =  5;
 GPIO_FUNCTION_ALT4  =  6;
 GPIO_FUNCTION_ALT5  =  7;
 
 GPIO_FUNCTION_UNKNOWN = LongWord(-1); {Returned by GPIOFunctionGet on error (eg device does not support reading the function state)}
 
const
 {GPIO Level constants}
 GPIO_LEVEL_LOW  =  0;
 GPIO_LEVEL_HIGH =  1;

 GPIO_LEVEL_UNKNOWN = LongWord(-1); {Returned by GPIOInputGet/Wait on error (eg device does not exist)}
 
const
 {GPIO Pull constants}
 GPIO_PULL_NONE  =  0;
 GPIO_PULL_UP    =  1;
 GPIO_PULL_DOWN  =  2;
 
 GPIO_PULL_UNKNOWN = LongWord(-1); {Returned by GPIOPullGet on error (eg device does not support reading the PullUp/Down state)}
 
const 
 {GPIO Trigger constants}
 GPIO_TRIGGER_NONE          = 0;
 GPIO_TRIGGER_LOW           = 1;
 GPIO_TRIGGER_HIGH          = 2;
 GPIO_TRIGGER_RISING        = 3;
 GPIO_TRIGGER_FALLING       = 4;
 GPIO_TRIGGER_ASYNC_RISING  = 5;
 GPIO_TRIGGER_ASYNC_FALLING = 6;
 GPIO_TRIGGER_EDGE          = 7;
 
 GPIO_TRIGGER_UNKNOWN       = LongWord(-1); {Passed to GPIO callback event when device does not support determining the trigger source}
 
{==============================================================================}
const
 {Virtual GPIO Pin constants}
 VIRTUAL_GPIO_PIN_0  =  0;
 VIRTUAL_GPIO_PIN_1  =  1;
 VIRTUAL_GPIO_PIN_2  =  2;
 VIRTUAL_GPIO_PIN_3  =  3;
 VIRTUAL_GPIO_PIN_4  =  4;
 VIRTUAL_GPIO_PIN_5  =  5;
 VIRTUAL_GPIO_PIN_6  =  6;
 VIRTUAL_GPIO_PIN_7  =  7;
 
const
 {Virtual GPIO Function constants}
 VIRTUAL_GPIO_FUNCTION_IN  =  0;
 VIRTUAL_GPIO_FUNCTION_OUT =  1;
 
{==============================================================================}
const
 {Serial Baud Rate constants}
 SERIAL_BAUD_RATE_DEFAULT = 0;
 
 SERIAL_BAUD_RATE_STANDARD = 115200;  {If SERIAL_BAUD_RATE_DEFAULT is passed to SerialOpen then this is the baud rate to use}
 SERIAL_BAUD_RATE_FALLBACK = 9600;    {The fallback baud rate if SERIAL_BAUD_RATE_STANDARD is not supported by the device}
 
 {Serial Data bit constants}
 SERIAL_DATA_8BIT = 8; 
 SERIAL_DATA_7BIT = 7;
 SERIAL_DATA_6BIT = 6;
 SERIAL_DATA_5BIT = 5;

 {Serial Stop bit constants}
 SERIAL_STOP_1BIT  = 1;
 SERIAL_STOP_2BIT  = 2;
 SERIAL_STOP_1BIT5 = 3;  {1.5 Stop bits}
 
 {Serial Parity constants}
 SERIAL_PARITY_NONE  = 0;
 SERIAL_PARITY_ODD   = 1;
 SERIAL_PARITY_EVEN  = 2;
 SERIAL_PARITY_MARK  = 3;
 SERIAL_PARITY_SPACE = 4;
  
 {Serial Flow Control constants}
 SERIAL_FLOW_NONE    = 0;
 SERIAL_FLOW_RTS_CTS = 1;
 SERIAL_FLOW_DSR_DTR = 2;
 
{==============================================================================}
const
 {I2C Address constants}
 I2C_ADDRESS_INVALID = Word(-1);

{==============================================================================}
const
 {SPI Protocol constants}
 //SPI_PROTOCOL_ //To Do //Continuing //4WIRE/3WIRE/LOSSI etc //SPI_MODE_* becomes 0/1/2/3 instead
 
 {SPI Mode constants}
 SPI_MODE_4WIRE = 0;
 SPI_MODE_3WIRE = 1;
 SPI_MODE_LOSSI = 2;
 
 SPI_MODE_UNKNOWN = LongWord(-1); {Returned by SPIGetMode on error (eg device does not exist)}
 
 {SPI Chip Select constants}
 SPI_CS_0    = 0;
 SPI_CS_1    = 1;
 SPI_CS_2    = 2;
 SPI_CS_3    = 3;
 SPI_CS_4    = 4;
 SPI_CS_5    = 5;
 SPI_CS_6    = 6;
 SPI_CS_7    = 7;   
 SPI_CS_8    = 8;   
 SPI_CS_9    = 9;   
 SPI_CS_10   = 10;   
 SPI_CS_11   = 11;   
 SPI_CS_12   = 12;   
 SPI_CS_13   = 13;   
 SPI_CS_14   = 14;   
 SPI_CS_15   = 15;   
 
 SPI_CS_MAX  = 15;
 
 SPI_CS_NONE = Word(-1); {Special value for No Chip Select to allow external control of additional CS lines}
 
 {SPI Clock Phase (CPHA) constants}
 SPI_CLOCK_PHASE_LOW  = 0; {Clock edge rising or falling for data input/output}
 SPI_CLOCK_PHASE_HIGH = 1; {See https://en.wikipedia.org/wiki/Serial_Peripheral_Interface_Bus for more information}
 
 SPI_CLOCK_PHASE_UNKNOWN = LongWord(-1); {Returned by SPIGetClockPhase on error (eg device does not exist)}
 
 {SPI Clock Polarity (CPOL) constants}
 SPI_CLOCK_POLARITY_LOW  = 0; {Clock is low when not transmitting}
 SPI_CLOCK_POLARITY_HIGH = 1; {Clock is high when not transmitting}
 
 SPI_CLOCK_POLARITY_UNKNOWN = LongWord(-1); {Returned by SPIGetClockPolarity on error (eg device does not exist)}
 
 {SPI Chip Select Polarity (CSPOL) constants}
 SPI_CS_POLARITY_LOW  = 0; {Chip select is active low (Default)}
 SPI_CS_POLARITY_HIGH = 1; {Chip select is active high}
 
 SPI_CS_POLARITY_UNKNOWN = LongWord(-1); {Returned by SPIGetSelectPolarity on error (eg device does not exist)}
 
{==============================================================================}
const
 {Power ID constants}
 POWER_ID_MMC0   = 0;
 POWER_ID_MMC1   = 1;
 POWER_ID_MMC2   = 2;
 POWER_ID_MMC3   = 3;
 POWER_ID_UART0  = 4;
 POWER_ID_UART1  = 5;
 POWER_ID_UART2  = 6;
 POWER_ID_UART3  = 7;
 POWER_ID_USB0   = 8;
 POWER_ID_USB1   = 9;
 POWER_ID_USB2   = 10;
 POWER_ID_USB3   = 11;
 POWER_ID_I2C0   = 12;
 POWER_ID_I2C1   = 13;
 POWER_ID_I2C2   = 14;
 POWER_ID_I2C3   = 15;
 POWER_ID_SPI0   = 16;
 POWER_ID_SPI1   = 17;
 POWER_ID_SPI2   = 18;
 POWER_ID_SPI3   = 19;
 POWER_ID_CCP2TX = 20;
 
const
 {Power State constants}
 POWER_STATE_OFF   = 0;
 POWER_STATE_ON    = 1;
 
{==============================================================================}
const
 {Clock ID constants}
 CLOCK_ID_MMC0    = 0;
 CLOCK_ID_MMC1    = 1;
 CLOCK_ID_MMC2    = 2;
 CLOCK_ID_MMC3    = 3;
 CLOCK_ID_UART0   = 4;
 CLOCK_ID_UART1   = 5;
 CLOCK_ID_UART2   = 6;
 CLOCK_ID_UART3   = 7;
 CLOCK_ID_CPU     = 8;
 CLOCK_ID_CORE    = 9;
 CLOCK_ID_GPU     = 10;
 CLOCK_ID_V3D     = 11;
 CLOCK_ID_H264    = 12;
 CLOCK_ID_ISP     = 13;
 CLOCK_ID_SDRAM   = 14;
 CLOCK_ID_PIXEL   = 15;
 CLOCK_ID_PWM0    = 16;
 CLOCK_ID_PWM1    = 17;
 CLOCK_ID_I2C0    = 18;
 CLOCK_ID_I2C1    = 19;
 CLOCK_ID_I2C2    = 20;
 CLOCK_ID_I2C3    = 21;
 CLOCK_ID_SPI0    = 22;
 CLOCK_ID_SPI1    = 23;
 CLOCK_ID_SPI2    = 24;
 CLOCK_ID_SPI3    = 25;
 
const
 {Clock State constants}
 CLOCK_STATE_OFF   = 0;
 CLOCK_STATE_ON    = 1;
 
{==============================================================================}
const
 {Turbo ID constants}
 TURBO_ID_SOC  = 0;
 
{==============================================================================}
const
 {Voltage ID constants}
 VOLTAGE_ID_CORE     = 0;
 VOLTAGE_ID_SDRAM_C  = 1;
 VOLTAGE_ID_SDRAM_P  = 2;
 VOLTAGE_ID_SDRAM_I  = 3;
 
{==============================================================================}
const
 {Temperature ID constants}
 TEMPERATURE_ID_SOC  = 0;

{==============================================================================}
const
 {Console Direction constants}
 CONSOLE_DIRECTION_UP    = 0;  {Scroll Console Up}
 CONSOLE_DIRECTION_DOWN  = 1;  {Scroll Console Down}
 CONSOLE_DIRECTION_LEFT  = 2;  {Scroll Console Left}
 CONSOLE_DIRECTION_RIGHT = 3;  {Scroll Console Right}
 
const
 {Console Position constants}
 CONSOLE_POSITION_FULL        = 0;  {Console Window will appear in the full console}
 CONSOLE_POSITION_TOP         = 1;  {Console Window will appear in the top half of the console}
 CONSOLE_POSITION_BOTTOM      = 2;  {Console Window will appear in the bottom half of the console}
 CONSOLE_POSITION_LEFT        = 3;  {Console Window will appear in the left half of the console}
 CONSOLE_POSITION_RIGHT       = 4;  {Console Window will appear in the right half of the console}
 CONSOLE_POSITION_TOPLEFT     = 5;  {Console Window will appear in the top left corner of the console}
 CONSOLE_POSITION_TOPRIGHT    = 6;  {Console Window will appear in the top right corner of the console}
 CONSOLE_POSITION_BOTTOMLEFT  = 7;  {Console Window will appear in the bottom left corner of the console}
 CONSOLE_POSITION_BOTTOMRIGHT = 8;  {Console Window will appear in the bottom right corner of the console}

 CONSOLE_POSITION_FULLSCREEN  = 9;  {Console Window will occupy the entire screen (Without any border or desktop)(If supported)}
 
 CONSOLE_POSITION_UNKNOWN = LongWord(-1);
 
{==============================================================================}
const
 {Framebuffer Depth constants}
 FRAMEBUFFER_DEPTH_8  = 8;
 FRAMEBUFFER_DEPTH_16 = 16;
 FRAMEBUFFER_DEPTH_24 = 24;
 FRAMEBUFFER_DEPTH_32 = 32;
 
const
 {Framebuffer Pixel Order constants}
 FRAMEBUFFER_ORDER_BGR = 0;
 FRAMEBUFFER_ORDER_RGB = 1;
 
const
 {Framebuffer Alpha Mode constants}
 FRAMEBUFFER_MODE_ENABLED  = 0;  {Alpha channel enabled (0 = Fully opaque)}
 FRAMEBUFFER_MODE_REVERSED = 1;  {Alpha channel reversed (0 = Fully transparent)}
 FRAMEBUFFER_MODE_IGNORED  = 2;  {Alpha channel ignored}
 
const 
 {Framebuffer Rotation constants}
 FRAMEBUFFER_ROTATION_0   = 0;   {No rotation}
 FRAMEBUFFER_ROTATION_90  = 1;   {90 degree rotation}
 FRAMEBUFFER_ROTATION_180 = 2;   {180 degree rotation}
 FRAMEBUFFER_ROTATION_270 = 3;   {270 degree rotation}
 
{==============================================================================}
const
 {Log Level constants}
 LOG_LEVEL_DEBUG     = 1;  {Debugging messages}
 LOG_LEVEL_INFO      = 2;  {Informational messages}
 LOG_LEVEL_WARN      = 3;  {Warning messages}
 LOG_LEVEL_ERROR     = 4;  {Error messages}
 LOG_LEVEL_NONE      = 5;  {No messages}
 
const
 {Logging Protocol constants}
 LOGGING_PROTOCOL_UDP = 0;
 LOGGING_PROTOCOL_TCP = 1;
 
const
 {Logging Facility constants}
 LOGGING_FACILITY_KERNEL     = 0;  {Core "kernel" log messages}
 LOGGING_FACILITY_PLATFORM   = 1;  {Platform log messages}
 LOGGING_FACILITY_THREADS    = 2;  {Thread log messages}
 LOGGING_FACILITY_DEVICES    = 3;  {Device log messages}
 LOGGING_FACILITY_NETWORK    = 4;  {Network log messages}
 LOGGING_FACILITY_STORAGE    = 5;  {Storage log messages}
 LOGGING_FACILITY_FILESYSTEM = 6;  {Filesystem log messages}
 LOGGING_FACILITY_KEYBOARD   = 7;  {Keyboard log messages}
 LOGGING_FACILITY_MOUSE      = 8;  {Mouse log messages}
 LOGGING_FACILITY_SCSI       = 9;  {SCSI log messages}
 LOGGING_FACILITY_DMA        = 10; {DMA log messages}
 LOGGING_FACILITY_GPIO       = 11; {GPIO log messages}
 LOGGING_FACILITY_MMC        = 12; {MMC/SD log messages}
 LOGGING_FACILITY_USB        = 13; {USB log messages}
 LOGGING_FACILITY_SERVICES   = 14; {Services log messages}
 LOGGING_FACILITY_HTTP       = 15; {HTTP log messages}
 LOGGING_FACILITY_IMAP       = 16; {IMAP4 log messages}
 LOGGING_FACILITY_POP        = 17; {POP3 log messages}
 LOGGING_FACILITY_SMTP       = 18; {SMTP log messages}
 LOGGING_FACILITY_TELNET     = 19; {Telnet log messages}
 LOGGING_FACILITY_SSH        = 20; {SSH log messages}
 LOGGING_FACILITY_SHELL      = 21; {Shell log messages}
 LOGGING_FACILITY_NTP        = 22; {NTP log messages}
 LOGGING_FACILITY_FTP        = 23; {FTP log messages}
 LOGGING_FACILITY_RTC        = 24; {RTC log messages}
 LOGGING_FACILITY_I2C        = 25; {I2C log messages}
 LOGGING_FACILITY_I2S        = 26; {I2S log messages}
 LOGGING_FACILITY_PWM        = 27; {PWM log messages}
 LOGGING_FACILITY_SERIAL     = 28; {Serial log messages}
 LOGGING_FACILITY_SPI        = 29; {SPI log messages}
 LOGGING_FACILITY_UART       = 30; {UART log messages}
 LOGGING_FACILITY_AUDIO      = 31; {Audio log messages}
 LOGGING_FACILITY_1WIRE      = 32; {1-Wire log messages}
 LOGGING_FACILITY_TOUCH      = 33; {Touch log messages}
 LOGGING_FACILITY_VIDEO      = 34; {Video log messages}
 LOGGING_FACILITY_CODEC      = 35; {Codec log messages}
 LOGGING_FACILITY_NFS        = 36; {NFS log messages}
 LOGGING_FACILITY_RPC        = 37; {RPC log messages}
 
 LOGGING_FACILITY_USER       = 1000; {User log messages}

 LOGGING_FACILITY_INVALID    = $FFFFFFFF;
 
const
 {Logging Severity constants}
 LOGGING_SEVERITY_ERROR = 0;    {Error log messages}
 LOGGING_SEVERITY_WARN  = 1;    {Warning log messages}
 LOGGING_SEVERITY_INFO  = 2;    {Informational log messages}
 LOGGING_SEVERITY_DEBUG = 3;    {Debugging log messages}
 
 LOGGING_SEVERITY_INVALID    = $FFFFFFFF;
 
{==============================================================================}
const
 {Handle Type constants}
 HANDLE_TYPE_SPIN         = 1;
 HANDLE_TYPE_MUTEX        = 2;
 HANDLE_TYPE_SECTION      = 3;
 HANDLE_TYPE_SEMAPHORE    = 4;
 HANDLE_TYPE_SYNCHRONIZER = 5;
 HANDLE_TYPE_CONDITION    = 6;
 HANDLE_TYPE_COMPLETION   = 7;
 HANDLE_TYPE_LIST         = 8;
 HANDLE_TYPE_QUEUE        = 9;
 HANDLE_TYPE_THREAD       = 10;
 HANDLE_TYPE_MESSAGESLOT  = 11;
 HANDLE_TYPE_MAILSLOT     = 12;
 HANDLE_TYPE_BUFFER       = 13;
 HANDLE_TYPE_EVENT        = 14;

 HANDLE_TYPE_TIMER        = 15;
 HANDLE_TYPE_WORKER       = 16;
 HANDLE_TYPE_WINDOW       = 17;
 HANDLE_TYPE_FONT         = 18;
 HANDLE_TYPE_KEYMAP       = 19;

 HANDLE_TYPE_FILE         = 100;
 HANDLE_TYPE_PIPE         = 101;
 HANDLE_TYPE_SOCKET       = 102;
 HANDLE_TYPE_DEVICE       = 103;
 
 HANDLE_TYPE_USER_BASE    = 1000; {Base value for user or application defined handle types}
 
{==============================================================================}
const
 {Filesystem Cache Mode constants}
 FILESYS_CACHE_MODE_NONE      = 0;
 FILESYS_CACHE_MODE_READONLY  = 1;
 FILESYS_CACHE_MODE_READWRITE = 2;

{==============================================================================}
const
 {Cursor constants}
 {Standard Arrow in 32 bit ARGB}
 CURSOR_ARROW_DEFAULT_WIDTH = 16;  {Must be >= 16 and <= 64 on Raspberry Pi}
 CURSOR_ARROW_DEFAULT_HEIGHT = 18; {Must be >= 16 and <= 64 on Raspberry Pi}
 CURSOR_ARROW_DEFAULT_FORMAT = COLOR_FORMAT_DEFAULT; {COLOR_FORMAT_ARGB32}
 CURSOR_ARROW_DEFAULT:array[0..CURSOR_ARROW_DEFAULT_HEIGHT - 1,0..CURSOR_ARROW_DEFAULT_WIDTH - 1] of LongWord = (
  (COLOR_BLACK,COLOR_NONE, COLOR_NONE, COLOR_NONE, COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE),
  (COLOR_BLACK,COLOR_BLACK,COLOR_NONE, COLOR_NONE, COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE),
  (COLOR_BLACK,COLOR_WHITE,COLOR_BLACK,COLOR_NONE, COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE),
  (COLOR_BLACK,COLOR_WHITE,COLOR_WHITE,COLOR_BLACK,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE),
  (COLOR_BLACK,COLOR_WHITE,COLOR_WHITE,COLOR_WHITE,COLOR_BLACK,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE),
  (COLOR_BLACK,COLOR_WHITE,COLOR_WHITE,COLOR_WHITE,COLOR_WHITE,COLOR_BLACK,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE),
  (COLOR_BLACK,COLOR_WHITE,COLOR_WHITE,COLOR_WHITE,COLOR_WHITE,COLOR_WHITE,COLOR_BLACK,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE),
  (COLOR_BLACK,COLOR_WHITE,COLOR_WHITE,COLOR_WHITE,COLOR_WHITE,COLOR_WHITE,COLOR_WHITE,COLOR_BLACK,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE),
  (COLOR_BLACK,COLOR_WHITE,COLOR_WHITE,COLOR_WHITE,COLOR_WHITE,COLOR_WHITE,COLOR_WHITE,COLOR_WHITE,COLOR_BLACK,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE),
  (COLOR_BLACK,COLOR_WHITE,COLOR_WHITE,COLOR_WHITE,COLOR_WHITE,COLOR_WHITE,COLOR_WHITE,COLOR_WHITE,COLOR_WHITE,COLOR_BLACK,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE),
  (COLOR_BLACK,COLOR_WHITE,COLOR_WHITE,COLOR_WHITE,COLOR_WHITE,COLOR_WHITE,COLOR_WHITE,COLOR_WHITE,COLOR_WHITE,COLOR_WHITE,COLOR_BLACK,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE),
  (COLOR_BLACK,COLOR_WHITE,COLOR_WHITE,COLOR_WHITE,COLOR_WHITE,COLOR_WHITE,COLOR_WHITE,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_BLACK,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE),
  (COLOR_BLACK,COLOR_WHITE,COLOR_WHITE,COLOR_WHITE,COLOR_BLACK,COLOR_WHITE,COLOR_WHITE,COLOR_BLACK,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE),
  (COLOR_BLACK,COLOR_WHITE,COLOR_WHITE,COLOR_BLACK,COLOR_NONE ,COLOR_BLACK,COLOR_WHITE,COLOR_WHITE,COLOR_BLACK,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE),
  (COLOR_BLACK,COLOR_WHITE,COLOR_BLACK,COLOR_NONE ,COLOR_NONE ,COLOR_BLACK,COLOR_WHITE,COLOR_WHITE,COLOR_BLACK,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE),
  (COLOR_BLACK,COLOR_BLACK,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_BLACK,COLOR_WHITE,COLOR_WHITE,COLOR_BLACK,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE),
  (COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_BLACK,COLOR_WHITE,COLOR_WHITE,COLOR_BLACK,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE),
  (COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_BLACK,COLOR_BLACK,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE ,COLOR_NONE));

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}

end.
