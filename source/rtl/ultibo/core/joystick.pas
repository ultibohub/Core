{
Ultibo Joystick and Gamepad interface unit.

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

 Joystick

  https://en.wikipedia.org/wiki/Joystick

 Gamepad

  https://en.wikipedia.org/wiki/Gamepad

 D-pad

  https://en.wikipedia.org/wiki/D-pad

Joystick and Gamepad Devices
============================

 Gamepads are almost universally used to control game consoles and other gaming
 platforms as they provide an easy way to interact with many different types of
 movement in gaming scenarios.

 They range from simple devices with a 4 way rocker switch (often known as the
 D-pad) and a small number of push buttons to elaborate units with multi-axis
 controls and buttons that cover a whole range of uses.

 Joysticks are used for more than just gaming, many modern industrial machines
 use a joystick as the primary control interface. Even common machines like
 ride on lawn mowers and earth moving equipment often feature a joystick type
 of control mechanism.

 Devices such as flight simulator yokes will often also appear as a joystick
 to the system as the available ranges of movement and the reported axes are
 the same.

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

{$IFNDEF FPC_DOTTEDUNITS}
unit Joystick;
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
  System.SysUtils;
{$ELSE FPC_DOTTEDUNITS}
uses
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  Devices,
  SysUtils;
{$ENDIF FPC_DOTTEDUNITS}

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
const
 {Joystick specific constants}
 JOYSTICK_NAME_PREFIX = 'Joystick';  {Name prefix for Joystick Devices}

 {Joystick Device Types}
 JOYSTICK_TYPE_NONE       = 0;
 JOYSTICK_TYPE_JOYSTICK   = 1;
 JOYSTICK_TYPE_GAMEPAD    = 2;

 JOYSTICK_TYPE_MAX        = 2;

 {Joystick Type Names}
 JOYSTICK_TYPE_NAMES:array[JOYSTICK_TYPE_NONE..JOYSTICK_TYPE_MAX] of String = (
  'JOYSTICK_TYPE_NONE',
  'JOYSTICK_TYPE_JOYSTICK',
  'JOYSTICK_TYPE_GAMEPAD');

 {Joystick Device States}
 JOYSTICK_STATE_DISABLED  = 0;
 JOYSTICK_STATE_ENABLED   = 1;

 JOYSTICK_STATE_MAX       = 1;

 {Joystick State Names}
 JOYSTICK_STATE_NAMES:array[JOYSTICK_STATE_DISABLED..JOYSTICK_STATE_MAX] of String = (
  'JOYSTICK_STATE_DISABLED',
  'JOYSTICK_STATE_ENABLED');

 {Joystick Device Flags}
 JOYSTICK_FLAG_NONE         = $00000000;
 JOYSTICK_FLAG_NON_BLOCK    = $00000001; {If set device reads are non blocking (Also supported in Flags parameter of JoystickDeviceRead)}
 JOYSTICK_FLAG_PEEK_BUFFER  = $00000002; {Peek at the buffer to see if any data is available, don't remove it (Used only in Flags parameter of JoystickDeviceRead)}
 JOYSTICK_FLAG_LED          = $00000004; {If set the device contains one or more LEDs}
 JOYSTICK_FLAG_RGBLED       = $00000008; {If set the device contains one or more RGB LEDs}
 JOYSTICK_FLAG_RUMBLE       = $00000010; {If set the device has rumble or force feedback}
 JOYSTICK_FLAG_GYROSCOPE    = $00000020; {If set the device contains a gyroscope}
 JOYSTICK_FLAG_TOUCHPAD     = $00000040; {If set the device contains a touchpad}

 {Flags supported by JOYSTICK_CONTROL_GET/SET/CLEAR_FLAG}
 JOYSTICK_FLAG_MASK = JOYSTICK_FLAG_NON_BLOCK or JOYSTICK_FLAG_LED or JOYSTICK_FLAG_RGBLED or JOYSTICK_FLAG_RUMBLE or JOYSTICK_FLAG_GYROSCOPE or JOYSTICK_FLAG_TOUCHPAD;

 {Joystick Device Control Codes}
 JOYSTICK_CONTROL_GET_FLAG         = 1;  {Get Flag}
 JOYSTICK_CONTROL_SET_FLAG         = 2;  {Set Flag}
 JOYSTICK_CONTROL_CLEAR_FLAG       = 3;  {Clear Flag}
 JOYSTICK_CONTROL_FLUSH_BUFFER     = 4;  {Flush Buffer}
 JOYSTICK_CONTROL_GET_HAT          = 5;  {Get the name (identifier) associated with a Hat}
 JOYSTICK_CONTROL_SET_HAT          = 6;  {Set the name (identifier) associated with a Hat}
 JOYSTICK_CONTROL_GET_AXIS         = 7;  {Get the name (identifier) associated with an Axis}
 JOYSTICK_CONTROL_SET_AXIS         = 8;  {Set the name (identifier) associated with an Axis}
 JOYSTICK_CONTROL_GET_BUTTON       = 9;  {Get the name (identifier) associated with a Button}
 JOYSTICK_CONTROL_SET_BUTTON       = 10; {Set the name (identifier) associated with a Button}
 JOYSTICK_CONTROL_GET_CALLBACK     = 11; {Get the registered callback function for joystick events}
 JOYSTICK_CONTROL_SET_CALLBACK     = 12; {Set the registered callback function for joystick events}

 {Joystick Buffer Size}
 JOYSTICK_BUFFER_SIZE = 2048;

 {Joystick Axis, Hat and Button Maximum}
 JOYSTICK_MAX_AXIS   = 16;
 JOYSTICK_MAX_HAT    = 4;
 JOYSTICK_MAX_BUTTON = 32;

 {Joystick Default Minimum and Maximum}
 JOYSTICK_DEFAULT_MINIMUM = 0;
 JOYSTICK_DEFAULT_MAXIMUM = 255;

 {Joystick and Gamepad Axis Names (Includes Buttons, Sliders, Wheels etc that report an analog value)}
 JOYSTICK_AXIS_NONE       = 0;
 JOYSTICK_AXIS_1          = 1;
 JOYSTICK_AXIS_2          = 2;
 JOYSTICK_AXIS_3          = 3;
 JOYSTICK_AXIS_4          = 4;
 JOYSTICK_AXIS_5          = 5;
 JOYSTICK_AXIS_6          = 6;
 JOYSTICK_AXIS_7          = 7;
 JOYSTICK_AXIS_8          = 8;
 JOYSTICK_AXIS_9          = 9;
 JOYSTICK_AXIS_10         = 10;
 JOYSTICK_AXIS_11         = 11;
 JOYSTICK_AXIS_12         = 12;
 JOYSTICK_AXIS_13         = 13;
 JOYSTICK_AXIS_14         = 14;
 JOYSTICK_AXIS_15         = 15;
 JOYSTICK_AXIS_16         = 16;

 JOYSTICK_AXIS_X          = 17; {X}
 JOYSTICK_AXIS_Y          = 18; {Y}
 JOYSTICK_AXIS_Z          = 19; {Z}
 JOYSTICK_ROTATION_X      = 20; {Rotation X}
 JOYSTICK_ROTATION_Y      = 21; {Rotation Y}
 JOYSTICK_ROTATION_Z      = 22; {Rotation Z}
 JOYSTICK_SLIDER          = 23; {Slider}

 GAMEPAD_AXIS_LEFT_X      = 24; {Left X}
 GAMEPAD_AXIS_LEFT_Y      = 25; {Left Y}
 GAMEPAD_AXIS_X           = GAMEPAD_AXIS_LEFT_X;
 GAMEPAD_AXIS_Y           = GAMEPAD_AXIS_LEFT_Y;
 GAMEPAD_AXIS_RIGHT_X     = 26; {Right X}
 GAMEPAD_AXIS_RIGHT_Y     = 27; {Right Y}
 GAMEPAD_CONTROL_LT       = 28; {Left Trigger analog button}
 GAMEPAD_CONTROL_L2       = GAMEPAD_CONTROL_LT;
 GAMEPAD_CONTROL_RT       = 29; {Right Trigger analog button}
 GAMEPAD_CONTROL_R2       = GAMEPAD_CONTROL_RT;
 GAMEPAD_CONTROL_UP       = 30; {Up analog button}
 GAMEPAD_CONTROL_RIGHT    = 31; {Right analog button}
 GAMEPAD_CONTROL_DOWN     = 32; {Down analog button}
 GAMEPAD_CONTROL_LEFT     = 33; {Left analog button}
 GAMEPAD_CONTROL_L1       = 34; {L1 analog button}
 GAMEPAD_CONTROL_R1       = 35; {R1 analog button}
 GAMEPAD_CONTROL_TRIANGLE = 36; {Triangle analog button}
 GAMEPAD_CONTROL_CIRCLE   = 37; {Circle analog button}
 GAMEPAD_CONTROL_CROSS    = 38; {Cross analog button}
 GAMEPAD_CONTROL_SQUARE   = 39; {Square analog button}

 JOYSTICK_AXIS_MAX        = 39;

 {Joystick and Gamepad Axis Names}
 JOYSTICK_AXIS_NAMES:array[JOYSTICK_AXIS_NONE..JOYSTICK_AXIS_MAX] of String = (
  'JOYSTICK_AXIS_NONE',
  'JOYSTICK_AXIS_1',
  'JOYSTICK_AXIS_2',
  'JOYSTICK_AXIS_3',
  'JOYSTICK_AXIS_4',
  'JOYSTICK_AXIS_5',
  'JOYSTICK_AXIS_6',
  'JOYSTICK_AXIS_7',
  'JOYSTICK_AXIS_8',
  'JOYSTICK_AXIS_9',
  'JOYSTICK_AXIS_10',
  'JOYSTICK_AXIS_11',
  'JOYSTICK_AXIS_12',
  'JOYSTICK_AXIS_13',
  'JOYSTICK_AXIS_14',
  'JOYSTICK_AXIS_15',
  'JOYSTICK_AXIS_16',

  'JOYSTICK_AXIS_X',
  'JOYSTICK_AXIS_Y',
  'JOYSTICK_AXIS_Z',
  'JOYSTICK_ROTATION_X',
  'JOYSTICK_ROTATION_Y',
  'JOYSTICK_ROTATION_Z',
  'JOYSTICK_SLIDER',

  'GAMEPAD_AXIS_LEFT_X',
  'GAMEPAD_AXIS_LEFT_Y',
  'GAMEPAD_AXIS_RIGHT_X',
  'GAMEPAD_AXIS_RIGHT_Y',
  'GAMEPAD_CONTROL_LT',
  'GAMEPAD_CONTROL_RT',
  'GAMEPAD_CONTROL_UP',
  'GAMEPAD_CONTROL_RIGHT',
  'GAMEPAD_CONTROL_DOWN',
  'GAMEPAD_CONTROL_LEFT',
  'GAMEPAD_CONTROL_L1',
  'GAMEPAD_CONTROL_R1',
  'GAMEPAD_CONTROL_TRIANGLE',
  'GAMEPAD_CONTROL_CIRCLE',
  'GAMEPAD_CONTROL_CROSS',
  'GAMEPAD_CONTROL_SQUARE');

 {Joystick and Gamepad Hat Names}
 JOYSTICK_HAT_NONE = 0;
 JOYSTICK_HAT_1    = 1;
 JOYSTICK_HAT_2    = 2;
 JOYSTICK_HAT_3    = 3;
 JOYSTICK_HAT_4    = 4;

 JOYSTICK_HAT_POV  = 5; {Point of View (POV)}

 JOYSTICK_HAT_MAX  = 5;

 {Joystick and Gamepad Hat Names}
 JOYSTICK_HAT_NAMES:array[JOYSTICK_HAT_NONE..JOYSTICK_HAT_MAX] of String = (
  'JOYSTICK_HAT_NONE',
  'JOYSTICK_HAT_1',
  'JOYSTICK_HAT_2',
  'JOYSTICK_HAT_3',
  'JOYSTICK_HAT_4',

  'JOYSTICK_HAT_POV');

 {Joystick and Gamepad Button Names}
 JOYSTICK_BUTTON_NONE    = 0;
 JOYSTICK_BUTTON_1       = 1;
 JOYSTICK_BUTTON_2       = 2;
 JOYSTICK_BUTTON_3       = 3;
 JOYSTICK_BUTTON_4       = 4;
 JOYSTICK_BUTTON_5       = 5;
 JOYSTICK_BUTTON_6       = 6;
 JOYSTICK_BUTTON_7       = 7;
 JOYSTICK_BUTTON_8       = 8;
 JOYSTICK_BUTTON_9       = 9;
 JOYSTICK_BUTTON_10      = 10;
 JOYSTICK_BUTTON_11      = 11;
 JOYSTICK_BUTTON_12      = 12;
 JOYSTICK_BUTTON_13      = 13;
 JOYSTICK_BUTTON_14      = 14;
 JOYSTICK_BUTTON_15      = 15;
 JOYSTICK_BUTTON_16      = 16;
 JOYSTICK_BUTTON_17      = 17;
 JOYSTICK_BUTTON_18      = 18;
 JOYSTICK_BUTTON_19      = 19;
 JOYSTICK_BUTTON_20      = 20;
 JOYSTICK_BUTTON_21      = 21;
 JOYSTICK_BUTTON_22      = 22;
 JOYSTICK_BUTTON_23      = 23;
 JOYSTICK_BUTTON_24      = 24;
 JOYSTICK_BUTTON_25      = 25;
 JOYSTICK_BUTTON_26      = 26;
 JOYSTICK_BUTTON_27      = 27;
 JOYSTICK_BUTTON_28      = 28;
 JOYSTICK_BUTTON_29      = 29;
 JOYSTICK_BUTTON_30      = 30;
 JOYSTICK_BUTTON_31      = 31;
 JOYSTICK_BUTTON_32      = 32;

 GAMEPAD_BUTTON_HOME     = 33;                    {Home or special function button}
 GAMEPAD_BUTTON_PS       = GAMEPAD_BUTTON_HOME;   {Playstation button}
 GAMEPAD_BUTTON_XBOX     = GAMEPAD_BUTTON_HOME;   {XBox button}
 GAMEPAD_BUTTON_GUIDE    = GAMEPAD_BUTTON_HOME;   {Guide button}
 GAMEPAD_BUTTON_LT       = 34;                    {Left Trigger button}
 GAMEPAD_BUTTON_L2       = GAMEPAD_BUTTON_LT;
 GAMEPAD_BUTTON_LZ       = GAMEPAD_BUTTON_LT;
 GAMEPAD_BUTTON_RT       = 35;                    {Right Trigger button}
 GAMEPAD_BUTTON_R2       = GAMEPAD_BUTTON_RT;
 GAMEPAD_BUTTON_RZ       = GAMEPAD_BUTTON_RT;
 GAMEPAD_BUTTON_LB       = 36;                    {Left Bumper button}
 GAMEPAD_BUTTON_L1       = GAMEPAD_BUTTON_LB;
 GAMEPAD_BUTTON_L        = GAMEPAD_BUTTON_LB;
 GAMEPAD_BUTTON_RB       = 37;                    {Right Bumper button}
 GAMEPAD_BUTTON_R1       = GAMEPAD_BUTTON_RB;
 GAMEPAD_BUTTON_R        = GAMEPAD_BUTTON_RB;
 GAMEPAD_BUTTON_Y        = 38;                    {Y button}
 GAMEPAD_BUTTON_TRIANGLE = GAMEPAD_BUTTON_Y;      {Triangle button}
 GAMEPAD_BUTTON_B        = 39;                    {B button}
 GAMEPAD_BUTTON_CIRCLE   = GAMEPAD_BUTTON_B;      {Circle button}
 GAMEPAD_BUTTON_A        = 40;                    {A button}
 GAMEPAD_BUTTON_CROSS    = GAMEPAD_BUTTON_A;      {Cross button}
 GAMEPAD_BUTTON_X        = 41;                    {X button}
 GAMEPAD_BUTTON_SQUARE   = GAMEPAD_BUTTON_X;      {Square button}
 GAMEPAD_BUTTON_SELECT   = 42;                    {Select button}
 GAMEPAD_BUTTON_BACK     = GAMEPAD_BUTTON_SELECT; {Back button}
 GAMEPAD_BUTTON_SHARE    = GAMEPAD_BUTTON_SELECT; {Share button}
 GAMEPAD_BUTTON_CAPTURE  = GAMEPAD_BUTTON_SELECT; {Capture button}
 GAMEPAD_BUTTON_L3       = 43;                    {Left Stick or Left Axis button}
 GAMEPAD_BUTTON_SL       = GAMEPAD_BUTTON_L3;
 GAMEPAD_BUTTON_R3       = 44;                    {Right Stick or Right Axis button}
 GAMEPAD_BUTTON_SR       = GAMEPAD_BUTTON_R3;
 GAMEPAD_BUTTON_START    = 45;                    {Start button}
 GAMEPAD_BUTTON_OPTIONS  = GAMEPAD_BUTTON_START;
 GAMEPAD_BUTTON_UP       = 46;                    {Up button}
 GAMEPAD_BUTTON_RIGHT    = 47;                    {Right button}
 GAMEPAD_BUTTON_DOWN     = 48;                    {Down button}
 GAMEPAD_BUTTON_LEFT     = 49;                    {Left button}
 GAMEPAD_BUTTON_PLUS     = 50;                    {Plus button}
 GAMEPAD_BUTTON_MINUS    = 51;                    {Minus button}
 GAMEPAD_BUTTON_TOUCHPAD = 52;                    {Touchpad button}

 JOYSTICK_BUTTON_MAX     = 52;

 {Joystick and Gamepad Button Names}
 JOYSTICK_BUTTON_NAMES:array[JOYSTICK_BUTTON_NONE..JOYSTICK_BUTTON_MAX] of String = (
  'JOYSTICK_BUTTON_NONE',
  'JOYSTICK_BUTTON_1',
  'JOYSTICK_BUTTON_2',
  'JOYSTICK_BUTTON_3',
  'JOYSTICK_BUTTON_4',
  'JOYSTICK_BUTTON_5',
  'JOYSTICK_BUTTON_6',
  'JOYSTICK_BUTTON_7',
  'JOYSTICK_BUTTON_8',
  'JOYSTICK_BUTTON_9',
  'JOYSTICK_BUTTON_10',
  'JOYSTICK_BUTTON_11',
  'JOYSTICK_BUTTON_12',
  'JOYSTICK_BUTTON_13',
  'JOYSTICK_BUTTON_14',
  'JOYSTICK_BUTTON_15',
  'JOYSTICK_BUTTON_16',
  'JOYSTICK_BUTTON_17',
  'JOYSTICK_BUTTON_18',
  'JOYSTICK_BUTTON_19',
  'JOYSTICK_BUTTON_20',
  'JOYSTICK_BUTTON_21',
  'JOYSTICK_BUTTON_22',
  'JOYSTICK_BUTTON_23',
  'JOYSTICK_BUTTON_24',
  'JOYSTICK_BUTTON_25',
  'JOYSTICK_BUTTON_26',
  'JOYSTICK_BUTTON_27',
  'JOYSTICK_BUTTON_28',
  'JOYSTICK_BUTTON_29',
  'JOYSTICK_BUTTON_30',
  'JOYSTICK_BUTTON_31',
  'JOYSTICK_BUTTON_32',

  'GAMEPAD_BUTTON_HOME',
  'GAMEPAD_BUTTON_LT',
  'GAMEPAD_BUTTON_RT',
  'GAMEPAD_BUTTON_LB',
  'GAMEPAD_BUTTON_RB',
  'GAMEPAD_BUTTON_Y',
  'GAMEPAD_BUTTON_B',
  'GAMEPAD_BUTTON_A',
  'GAMEPAD_BUTTON_X',
  'GAMEPAD_BUTTON_SELECT',
  'GAMEPAD_BUTTON_L3',
  'GAMEPAD_BUTTON_R3',
  'GAMEPAD_BUTTON_START',
  'GAMEPAD_BUTTON_UP',
  'GAMEPAD_BUTTON_RIGHT',
  'GAMEPAD_BUTTON_DOWN',
  'GAMEPAD_BUTTON_LEFT',
  'GAMEPAD_BUTTON_PLUS',
  'GAMEPAD_BUTTON_MINUS',
  'GAMEPAD_BUTTON_TOUCHPAD');

 {Joystick logging}
 JOYSTICK_LOG_LEVEL_DEBUG     = LOG_LEVEL_DEBUG;  {Joystick debugging messages}
 JOYSTICK_LOG_LEVEL_INFO      = LOG_LEVEL_INFO;   {Joystick informational messages, such as a device being attached or detached}
 JOYSTICK_LOG_LEVEL_WARN      = LOG_LEVEL_WARN;   {Joystick warning messages}
 JOYSTICK_LOG_LEVEL_ERROR     = LOG_LEVEL_ERROR;  {Joystick error messages}
 JOYSTICK_LOG_LEVEL_NONE      = LOG_LEVEL_NONE;   {No Joystick messages}

var
 JOYSTICK_DEFAULT_LOG_LEVEL:LongWord = JOYSTICK_LOG_LEVEL_DEBUG; {Minimum level for Joystick messages.  Only messages with level greater than or equal to this will be printed}

var
 {Joystick logging}
 JOYSTICK_LOG_ENABLED:Boolean;

{==============================================================================}
type
 {Joystick specific types}
 {Joystick Data}
 PJoystickData = ^TJoystickData;
 TJoystickData = record
  AxisCount:LongInt;                                 {The number of Axes reported by this device}
  Axes:array[0..JOYSTICK_MAX_AXIS - 1] of SmallInt;  {The currently reported value of each Axis}
  HatCount:LongInt;                                  {The number of Hats reported by this device}
  Hats:array[0..JOYSTICK_MAX_HAT - 1] of SmallInt;   {The currently reported value of each Hat}
  ButtonCount:LongInt;                               {The number of Buttons reported by this device}
  Buttons:LongWord;                                  {The currently reported state of each Button}
  Parameter:Pointer;                                 {The parameter for the event callback (If applicable)}
 end;

 {Joystick Buffer}
 PJoystickBuffer = ^TJoystickBuffer;
 TJoystickBuffer = record
  Wait:TSemaphoreHandle;     {Buffer ready semaphore}
  Start:LongWord;            {Index of first buffer ready}
  Count:LongWord;            {Number of entries ready in buffer}
  Buffer:array[0..(JOYSTICK_BUFFER_SIZE - 1)] of TJoystickData;
 end;

 {Joystick Properties}
 PJoystickExtent = ^TJoystickExtent;
 TJoystickExtent = record
  Minimum:LongInt;          {The minimum value for this extent}
  Maximum:LongInt;          {The maximum value for this extent}
 end;

 PJoystickAxis = ^TJoystickAxis;
 TJoystickAxis = record
  Name:LongWord;            {The associated name (identifier) for this Axis}
  Logical:TJoystickExtent;  {The minimum and maximum logical values for this Axis}
  Physical:TJoystickExtent; {The minimum and maximum physical values for this Axis}
  Multiplier:Double;        {The conversion multiplier for this Axis from logical to physical units}
  Resolution:Double;        {The unit resolution for this Axis in counts per physical unit}
 end;

 PJoystickHat = ^TJoystickHat;
 TJoystickHat = record
  Name:LongWord;            {The associated name (identifier) for this Hat}
  Logical:TJoystickExtent;  {The minimum and maximum logical values for this Hat}
  Physical:TJoystickExtent; {The minimum and maximum physical values for this Hat}
  Multiplier:Double;        {The conversion multiplier for this Hat from logical to physical units}
  Resolution:Double;        {The unit resolution for this Hat in counts per physical unit}
 end;

 PJoystickProperties = ^TJoystickProperties;
 TJoystickProperties = record
  Flags:LongWord;                                         {Device flags (eg JOYSTICK_FLAG_LED)}
  AxisCount:LongWord;                                     {The number of Axes reported by this device}
  Axes:array[0..JOYSTICK_MAX_AXIS - 1] of TJoystickAxis;  {The current properties of each Axis}
  HatCount:LongWord;                                      {The number of Hats reported by this device}
  Hats:array[0..JOYSTICK_MAX_HAT - 1] of TJoystickHat;    {The current properties of each Hat}
  ButtonCount:LongWord;                                   {The number of Buttons reported by this device}
  Buttons:array[0..JOYSTICK_MAX_BUTTON - 1] of LongWord;  {The current name (identifier) of each Button}
 end;

 {Joystick Device}
 PJoystickDevice = ^TJoystickDevice;

 {Joystick Event Callback}
 TJoystickEvent = function(Joystick:PJoystickDevice;Data:PJoystickData):LongWord;{$IFDEF i386} stdcall;{$ENDIF}

 {Joystick Enumeration Callback}
 TJoystickEnumerate = function(Joystick:PJoystickDevice;Data:Pointer):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 {Joystick Notification Callback}
 TJoystickNotification = function(Device:PDevice;Data:Pointer;Notification:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}

 {Joystick Device Methods}
 TJoystickDeviceStart = function(Joystick:PJoystickDevice):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TJoystickDeviceStop = function(Joystick:PJoystickDevice):LongWord;{$IFDEF i386} stdcall;{$ENDIF}

 TJoystickDevicePeek = function(Joystick:PJoystickDevice):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TJoystickDeviceRead = function(Joystick:PJoystickDevice;Buffer:Pointer;Size,Flags:LongWord;var Count:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TJoystickDeviceWrite = function(Joystick:PJoystickDevice;Buffer:Pointer;Size,Count:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TJoystickDeviceFlush = function(Joystick:PJoystickDevice):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TJoystickDeviceUpdate = function(Joystick:PJoystickDevice):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TJoystickDeviceControl = function(Joystick:PJoystickDevice;Request:Integer;Argument1:PtrUInt;var Argument2:PtrUInt):LongWord;{$IFDEF i386} stdcall;{$ENDIF}

 TJoystickDeviceGetProperties = function(Joystick:PJoystickDevice;Properties:PJoystickProperties):LongWord;{$IFDEF i386} stdcall;{$ENDIF}

 TJoystickDevice = record
  {Device Properties}
  Device:TDevice;                                   {The Device entry for this Joystick device}
  {Joystick Properties}
  JoystickId:LongWord;                              {Unique Id of this Joystick device in the Joystick device table}
  JoystickState:LongWord;                           {Joystick device state (eg JOYSTICK_STATE_ENABLED)}
  DeviceStart:TJoystickDeviceStart;                 {A Device specific DeviceStart method implementing the standard Joystick device interface (Mandatory)}
  DeviceStop:TJoystickDeviceStop;                   {A Device specific DeviceStop method implementing the standard Joystick device interface (Mandatory)}
  DevicePeek:TJoystickDevicePeek;                   {A Device specific DevicePeek method implementing a standard Joystick device interface (Or nil if the default method is suitable)}
  DeviceRead:TJoystickDeviceRead;                   {A Device specific DeviceRead method implementing a standard Joystick device interface (Or nil if the default method is suitable)}
  DeviceWrite:TJoystickDeviceWrite;                 {A Device specific DeviceWrite method implementing a standard Joystick device interface (Or nil if the default method is suitable)}
  DeviceFlush:TJoystickDeviceFlush;                 {A Device specific DeviceFlush method implementing a standard Joystick device interface (Or nil if the default method is suitable)}
  DeviceUpdate:TJoystickDeviceUpdate;               {A Device specific DeviceUpdate method implementing a standard Joystick device interface (Or nil if the default method is suitable)}
  DeviceControl:TJoystickDeviceControl;             {A Device specific DeviceControl method implementing a standard Joystick device interface (Or nil if the default method is suitable)}
  DeviceGetProperties:TJoystickDeviceGetProperties; {A Device specific DeviceGetProperties method implementing a standard Joystick device interface (Or nil if the default method is suitable)}
  {Driver Properties}
  Lock:TMutexHandle;                                {Device lock}
  Event:TJoystickEvent;                             {Event callback function (If assigned)}
  Parameter:Pointer;                                {Parameter for the event callback (or nil)}
  Buffer:TJoystickBuffer;                           {Joystick input buffer}
  Properties:TJoystickProperties;                   {Device properties}
  {Statistics Properties}
  ReceiveCount:LongWord;
  ReceiveErrors:LongWord;
  BufferOverruns:LongWord;
  {Internal Properties}
  Prev:PJoystickDevice;                             {Previous entry in Joystick device table}
  Next:PJoystickDevice;                             {Next entry in Joystick device table}
 end;

{==============================================================================}
{var}
 {Joystick specific variables}

{==============================================================================}
{Initialization Functions}
procedure JoystickInit;

{==============================================================================}
{Joystick Functions}
function JoystickDeviceStart(Joystick:PJoystickDevice):LongWord;
function JoystickDeviceStop(Joystick:PJoystickDevice):LongWord;

function JoystickDevicePeek(Joystick:PJoystickDevice):LongWord;

function JoystickDeviceRead(Joystick:PJoystickDevice;Buffer:Pointer;Size,Flags:LongWord;var Count:LongWord):LongWord;
function JoystickDeviceWrite(Joystick:PJoystickDevice;Buffer:Pointer;Size,Count:LongWord):LongWord;

function JoystickDeviceFlush(Joystick:PJoystickDevice):LongWord;
function JoystickDeviceUpdate(Joystick:PJoystickDevice):LongWord;

function JoystickDeviceControl(Joystick:PJoystickDevice;Request:Integer;Argument1:PtrUInt;var Argument2:PtrUInt):LongWord;

function JoystickDeviceGetProperties(Joystick:PJoystickDevice;Properties:PJoystickProperties):LongWord;

function JoystickDeviceSetState(Joystick:PJoystickDevice;State:LongWord):LongWord;

function JoystickDeviceCreate:PJoystickDevice;
function JoystickDeviceCreateEx(Size:LongWord):PJoystickDevice;
function JoystickDeviceDestroy(Joystick:PJoystickDevice):LongWord;

function JoystickDeviceRegister(Joystick:PJoystickDevice):LongWord;
function JoystickDeviceDeregister(Joystick:PJoystickDevice):LongWord;

function JoystickDeviceFind(JoystickId:LongWord):PJoystickDevice;
function JoystickDeviceFindByName(const Name:String):PJoystickDevice; inline;
function JoystickDeviceFindByDescription(const Description:String):PJoystickDevice; inline;
function JoystickDeviceEnumerate(Callback:TJoystickEnumerate;Data:Pointer):LongWord;

function JoystickDeviceNotification(Joystick:PJoystickDevice;Callback:TJoystickNotification;Data:Pointer;Notification,Flags:LongWord):LongWord;

{==============================================================================}
{RTL Joystick Functions}

{==============================================================================}
{Joystick Helper Functions}
function JoystickGetCount:LongWord;
function JoystickDeviceGetDefault:PJoystickDevice;
function JoystickDeviceSetDefault(Joystick:PJoystickDevice):LongWord;

function JoystickDeviceCheck(Joystick:PJoystickDevice):PJoystickDevice;

function JoystickDeviceAxisToString(Name:LongWord):String;
function JoystickDeviceHatToString(Name:LongWord):String;
function JoystickDeviceButtonToString(Name:LongWord):String;

function JoystickDeviceTypeToString(JoystickType:LongWord):String;
function JoystickDeviceStateToString(JoystickState:LongWord):String;

function JoystickDeviceStateToNotification(State:LongWord):LongWord;

function JoystickDeviceGetAxis(Joystick:PJoystickDevice;Index:LongWord):LongWord;
function JoystickDeviceSetAxis(Joystick:PJoystickDevice;Index,Name:LongWord):LongWord;

function JoystickDeviceGetHat(Joystick:PJoystickDevice;Index:LongWord):LongWord;
function JoystickDeviceSetHat(Joystick:PJoystickDevice;Index,Name:LongWord):LongWord;

function JoystickDeviceGetButton(Joystick:PJoystickDevice;Index:LongWord):LongWord;
function JoystickDeviceSetButton(Joystick:PJoystickDevice;Index,Name:LongWord):LongWord;

function JoystickDeviceSetCallback(Joystick:PJoystickDevice;Event:TJoystickEvent;Parameter:Pointer):LongWord;

function JoystickInsertData(Joystick:PJoystickDevice;Data:PJoystickData;Signal:Boolean):LongWord;

procedure JoystickLog(Level:LongWord;Joystick:PJoystickDevice;const AText:String);
procedure JoystickLogInfo(Joystick:PJoystickDevice;const AText:String); inline;
procedure JoystickLogWarn(Joystick:PJoystickDevice;const AText:String); inline;
procedure JoystickLogError(Joystick:PJoystickDevice;const AText:String); inline;
procedure JoystickLogDebug(Joystick:PJoystickDevice;const AText:String); inline;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {Joystick specific variables}
 JoystickInitialized:Boolean;

 JoystickDeviceTable:PJoystickDevice;
 JoystickDeviceTableLock:TCriticalSectionHandle = INVALID_HANDLE_VALUE;
 JoystickDeviceTableCount:LongWord;

 JoystickDeviceDefault:PJoystickDevice;

{==============================================================================}
{==============================================================================}
{Forward Declarations}

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure JoystickInit;
{Initialize the Joystick unit and Joystick device table}

{Note: Called only during system startup}
begin
 {}
 {Check Initialized}
 if JoystickInitialized then Exit;

 {Initialize Logging}
 JOYSTICK_LOG_ENABLED:=(JOYSTICK_DEFAULT_LOG_LEVEL <> JOYSTICK_LOG_LEVEL_NONE);

 {Initialize Joystick Device Table}
 JoystickDeviceTable:=nil;
 JoystickDeviceTableLock:=CriticalSectionCreate;
 JoystickDeviceTableCount:=0;
 if JoystickDeviceTableLock = INVALID_HANDLE_VALUE then
  begin
   if JOYSTICK_LOG_ENABLED then JoystickLogError(nil,'Failed to create Joystick device table lock');
  end;
 JoystickDeviceDefault:=nil;

 {Register Platform Joystick Handlers}
 {Nothing}

 JoystickInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{Joystick Functions}
function JoystickDeviceStart(Joystick:PJoystickDevice):LongWord;
{Start the specified Joystick device ready for receiving events}
{Joystick: The Joystick device to start}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Joystick}
 if Joystick = nil then Exit;
 if Joystick.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF JOYSTICK_DEBUG}
 if JOYSTICK_LOG_ENABLED then JoystickLogDebug(Joystick,'Joystick Device Start');
 {$ENDIF}

 {Check Disabled}
 Result:=ERROR_SUCCESS;
 if Joystick.JoystickState <> JOYSTICK_STATE_DISABLED then Exit;

 if MutexLock(Joystick.Lock) = ERROR_SUCCESS then
  begin
   try
    if Assigned(Joystick.DeviceStart) then
     begin
      {Call Device Start}
      Result:=Joystick.DeviceStart(Joystick);
      if Result <> ERROR_SUCCESS then Exit;
     end
    else
     begin
      Result:=ERROR_INVALID_PARAMETER;
      Exit;
     end;

    {Enable Device}
    Joystick.JoystickState:=JOYSTICK_STATE_ENABLED;

    {Notify Enable}
    NotifierNotify(@Joystick.Device,DEVICE_NOTIFICATION_ENABLE);

    Result:=ERROR_SUCCESS;
   finally
    MutexUnlock(Joystick.Lock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function JoystickDeviceStop(Joystick:PJoystickDevice):LongWord;
{Stop the specified Joystick device and terminate receiving events}
{Joystick: The Joystick device to stop}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Joystick}
 if Joystick = nil then Exit;
 if Joystick.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF JOYSTICK_DEBUG}
 if JOYSTICK_LOG_ENABLED then JoystickLogDebug(Joystick,'Joystick Device Stop');
 {$ENDIF}

 {Check Enabled}
 Result:=ERROR_SUCCESS;
 if Joystick.JoystickState <> JOYSTICK_STATE_ENABLED then Exit;

 if MutexLock(Joystick.Lock) = ERROR_SUCCESS then
  begin
   try
    if Assigned(Joystick.DeviceStop) then
     begin
      {Call Device Stop}
      Result:=Joystick.DeviceStop(Joystick);
      if Result <> ERROR_SUCCESS then Exit;
     end
    else
     begin
      Result:=ERROR_INVALID_PARAMETER;
      Exit;
     end;

    {Disable Device}
    Joystick.JoystickState:=JOYSTICK_STATE_DISABLED;

    {Notify Disable}
    NotifierNotify(@Joystick.Device,DEVICE_NOTIFICATION_DISABLE);

    Result:=ERROR_SUCCESS;
   finally
    MutexUnlock(Joystick.Lock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function JoystickDevicePeek(Joystick:PJoystickDevice):LongWord;
{Peek at the buffer of the specified joystick device to see if any data packets are ready}
{Joystick: The Joystick device to peek at}
{Return: ERROR_SUCCESS if packets are ready, ERROR_NO_MORE_ITEMS if not or another error code on failure}
var
 Count:LongWord;
 Data:TJoystickData;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Joystick}
 if Joystick = nil then Exit;
 if Joystick.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF JOYSTICK_DEBUG}
 if JOYSTICK_LOG_ENABLED then JoystickLogDebug(Joystick,'Joystick Device Peek');
 {$ENDIF}

 {Check Method}
 if Assigned(Joystick.DevicePeek) then
  begin
   {Provided Method}
   Result:=Joystick.DevicePeek(Joystick);
  end
 else
  begin
   {Default Method}
   Result:=JoystickDeviceRead(Joystick,@Data,SizeOf(TJoystickData),JOYSTICK_FLAG_NON_BLOCK or JOYSTICK_FLAG_PEEK_BUFFER,Count);
  end;
end;

{==============================================================================}

function JoystickDeviceRead(Joystick:PJoystickDevice;Buffer:Pointer;Size,Flags:LongWord;var Count:LongWord):LongWord;
{Read joystick data packets from the buffer of the specified joystick device}
{Joystick: The Joystick device to read from}
{Buffer: Pointer to a buffer to copy the joystick data packets to}
{Size: The size of the buffer in bytes (Must be at least TJoystickData or greater)}
{Flags: The flags for the behaviour of the read (eg JOYSTICK_FLAG_NON_BLOCK)}
{Count: The number of joystick data packets copied to the buffer}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Offset:PtrUInt;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Joystick}
 if Joystick = nil then Exit;
 if Joystick.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF JOYSTICK_DEBUG}
 if JOYSTICK_LOG_ENABLED then JoystickLogDebug(Joystick,'Joystick Device Read (Size=' + IntToStr(Size) + ')');
 {$ENDIF}

 {Check Buffer}
 if Buffer = nil then Exit;

 {Check Size}
 if Size < SizeOf(TJoystickData) then Exit;

 {Check Method}
 if Assigned(Joystick.DeviceRead) then
  begin
   {Provided Method}
   Result:=Joystick.DeviceRead(Joystick,Buffer,Size,Flags,Count);
  end
 else
  begin
   {Default Method}
   {Check Joystick Enabled}
   if Joystick.JoystickState <> JOYSTICK_STATE_ENABLED then Exit;

   {$IFDEF JOYSTICK_DEBUG}
   if JOYSTICK_LOG_ENABLED then JoystickLogDebug(Joystick,'Attempting to read ' + IntToStr(Size) + ' bytes from joystick');
   {$ENDIF}

   {Read to Buffer}
   Count:=0;
   Offset:=0;
   while Size >= SizeOf(TJoystickData) do
    begin
     {Check Non Blocking}
     if (((Joystick.Device.DeviceFlags and JOYSTICK_FLAG_NON_BLOCK) <> 0) or ((Flags and JOYSTICK_FLAG_NON_BLOCK) <> 0)) and (Joystick.Buffer.Count = 0) then
      begin
       if Count = 0 then Result:=ERROR_NO_MORE_ITEMS;
       Break;
      end;

     {Check Peek Buffer}
     if (Flags and JOYSTICK_FLAG_PEEK_BUFFER) <> 0 then
      begin
       {Acquire the Lock}
       if MutexLock(Joystick.Lock) = ERROR_SUCCESS then
        begin
         try
          if Joystick.Buffer.Count > 0 then
           begin
            {Copy Data}
            PJoystickData(PtrUInt(Buffer) + Offset)^:=Joystick.Buffer.Buffer[Joystick.Buffer.Start];

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
          MutexUnlock(Joystick.Lock);
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
       {Wait for Joystick Data}
       if SemaphoreWait(Joystick.Buffer.Wait) = ERROR_SUCCESS then
        begin
         {Acquire the Lock}
         if MutexLock(Joystick.Lock) = ERROR_SUCCESS then
          begin
           try
            {Copy Data}
            PJoystickData(PtrUInt(Buffer) + Offset)^:=Joystick.Buffer.Buffer[Joystick.Buffer.Start];

            {Update Start}
            Joystick.Buffer.Start:=(Joystick.Buffer.Start + 1) mod JOYSTICK_BUFFER_SIZE;

            {Update Count}
            Dec(Joystick.Buffer.Count);

            {Update Count}
            Inc(Count);

            {Update Size and Offset}
            Dec(Size,SizeOf(TJoystickData));
            Inc(Offset,SizeOf(TJoystickData));
           finally
            {Release the Lock}
            MutexUnlock(Joystick.Lock);
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

   {$IFDEF JOYSTICK_DEBUG}
   if JOYSTICK_LOG_ENABLED then JoystickLogDebug(Joystick,'Return count=' + IntToStr(Count));
   {$ENDIF}
  end;
end;

{==============================================================================}

function JoystickDeviceWrite(Joystick:PJoystickDevice;Buffer:Pointer;Size,Count:LongWord):LongWord;
{Write joystick data packets to the buffer of the specified joystick device}
{Joystick: The Joystick device to write to}
{Buffer: Pointer to a buffer to copy the joystick data packets from}
{Size: The size of the buffer in bytes (Must be at least TJoystickData or greater)}
{Count: The number of joystick data packets to copy from the buffer}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Offset:PtrUInt;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Joystick}
 if Joystick = nil then Exit;
 if Joystick.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF JOYSTICK_DEBUG}
 if JOYSTICK_LOG_ENABLED then JoystickLogDebug(Joystick,'Joystick Device Write (Size=' + IntToStr(Size) + ')');
 {$ENDIF}

 {Check Buffer}
 if Buffer = nil then Exit;

 {Check Size}
 if Size < SizeOf(TJoystickData) then Exit;

 {Check Count}
 if Count < 1 then Exit;

 {Check Method}
 if Assigned(Joystick.DeviceWrite) then
  begin
   {Provided Method}
   Result:=Joystick.DeviceWrite(Joystick,Buffer,Size,Count);
  end
 else
  begin
   {Default Method}
   {Check Joystick Enabled}
   if Joystick.JoystickState <> JOYSTICK_STATE_ENABLED then Exit;

   {$IFDEF JOYSTICK_DEBUG}
   if JOYSTICK_LOG_ENABLED then JoystickLogDebug(Joystick,'Attempting to write ' + IntToStr(Size) + ' bytes to joystick');
   {$ENDIF}

   {Write from Buffer}
   Offset:=0;
   while (Size >= SizeOf(TJoystickData)) and (Count > 0) do
    begin
     {Acquire the Lock}
     if MutexLock(Joystick.Lock) = ERROR_SUCCESS then
      begin
       try
        {Check Buffer}
        if (Joystick.Buffer.Count < JOYSTICK_BUFFER_SIZE) then
         begin
          {Copy Data}
          Joystick.Buffer.Buffer[(Joystick.Buffer.Start + Joystick.Buffer.Count) mod JOYSTICK_BUFFER_SIZE]:=PJoystickData(PtrUInt(Buffer) + Offset)^;

          {Update Count}
          Inc(Joystick.Buffer.Count);

          {Update Count}
          Dec(Count);

          {Update Size and Offset}
          Dec(Size,SizeOf(TJoystickData));
          Inc(Offset,SizeOf(TJoystickData));

          {Signal Data Received}
          SemaphoreSignal(Joystick.Buffer.Wait);
         end
        else
         begin
          Result:=ERROR_INSUFFICIENT_BUFFER;
          Exit;
         end;
       finally
        {Release the Lock}
        MutexUnlock(Joystick.Lock);
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
end;

{==============================================================================}

function JoystickDeviceFlush(Joystick:PJoystickDevice):LongWord;
{Flush the contents of the buffer of the specified joystick device}
{Joystick: The Joystick device to flush}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Joystick}
 if Joystick = nil then Exit;
 if Joystick.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF JOYSTICK_DEBUG}
 if JOYSTICK_LOG_ENABLED then JoystickLogDebug(Joystick,'Joystick Device Flush');
 {$ENDIF}

 {Check Method}
 if Assigned(Joystick.DeviceFlush) then
  begin
   {Provided Method}
   Result:=Joystick.DeviceFlush(Joystick);
  end
 else
  begin
   {Default Method}
   {Check Joystick Enabled}
   if Joystick.JoystickState <> JOYSTICK_STATE_ENABLED then Exit;

   {Acquire the Lock}
   if MutexLock(Joystick.Lock) = ERROR_SUCCESS then
    begin
     try
      while Joystick.Buffer.Count > 0 do
       begin
        {Wait for Data (Should not Block)}
        if SemaphoreWait(Joystick.Buffer.Wait) = ERROR_SUCCESS then
         begin
          {Update Start}
          Joystick.Buffer.Start:=(Joystick.Buffer.Start + 1) mod JOYSTICK_BUFFER_SIZE;

          {Update Count}
          Dec(Joystick.Buffer.Count);
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
      MutexUnlock(Joystick.Lock);
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

function JoystickDeviceUpdate(Joystick:PJoystickDevice):LongWord;
{Request the specified Joystick device to update the current configuration}
{Joystick: The Joystick device to update}
{Return: ERROR_SUCCESS if completed or another error code on failure}
{Note: Items updated can include rotation, maximum X and Y and flags (If supported)}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Joystick}
 if Joystick = nil then Exit;
 if Joystick.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF JOYSTICK_DEBUG}
 if JOYSTICK_LOG_ENABLED then JoystickLogDebug(Joystick,'Joystick Device Update');
 {$ENDIF}

 {Check Method}
 if Assigned(Joystick.DeviceUpdate) then
  begin
   {Provided Method}
   Result:=Joystick.DeviceUpdate(Joystick);
  end
 else
  begin
   {Default Method}
   {Check Joystick Enabled}
   if Joystick.JoystickState <> JOYSTICK_STATE_ENABLED then Exit;

   {Acquire the Lock}
   if MutexLock(Joystick.Lock) = ERROR_SUCCESS then
    begin
     try
      {Nothing by default}

      {Return Result}
      Result:=ERROR_SUCCESS;
     finally
      {Release the Lock}
      MutexUnlock(Joystick.Lock);
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

function JoystickDeviceControl(Joystick:PJoystickDevice;Request:Integer;Argument1:PtrUInt;var Argument2:PtrUInt):LongWord;
{Perform a control request on the specified Joystick device}
{Joystick: The Joystick device to control}
{Request: The request code for the operation (eg JOYSTICK_CONTROL_GET_FLAG)}
{Argument1: The first argument for the operation (Dependent on request code)}
{Argument2: The second argument for the operation (Dependent on request code)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Joystick}
 if Joystick = nil then Exit;
 if Joystick.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Method}
 if Assigned(Joystick.DeviceControl) then
  begin
   {Provided Method}
   Result:=Joystick.DeviceControl(Joystick,Request,Argument1,Argument2);
  end
 else
  begin
   {Default Method}
   {Check Joystick Enabled}
   if Joystick.JoystickState <> JOYSTICK_STATE_ENABLED then Exit;

   {Acquire the Lock}
   if MutexLock(Joystick.Lock) = ERROR_SUCCESS then
    begin
     try
      case Request of
       JOYSTICK_CONTROL_GET_FLAG:begin
         {Get Flag}
         Argument2:=Ord(False);
         if (Joystick.Device.DeviceFlags and Argument1) <> 0 then
          begin
           Argument2:=Ord(True);

           {Return Result}
           Result:=ERROR_SUCCESS;
          end;
        end;
       JOYSTICK_CONTROL_SET_FLAG:begin
         {Set Flag}
         if (Argument1 and not(JOYSTICK_FLAG_MASK)) = 0 then
          begin
           Joystick.Device.DeviceFlags:=(Joystick.Device.DeviceFlags or Argument1);
           Joystick.Properties.Flags:=Joystick.Device.DeviceFlags;

           {Request Update}
           Result:=JoystickDeviceUpdate(Joystick);
          end;
        end;
       JOYSTICK_CONTROL_CLEAR_FLAG:begin
         {Clear Flag}
         if (Argument1 and not(JOYSTICK_FLAG_MASK)) = 0 then
          begin
           Joystick.Device.DeviceFlags:=(Joystick.Device.DeviceFlags and not(Argument1));
           Joystick.Properties.Flags:=Joystick.Device.DeviceFlags;

           {Request Update}
           Result:=JoystickDeviceUpdate(Joystick);
          end;
        end;
       JOYSTICK_CONTROL_FLUSH_BUFFER:begin
         {Flush Buffer}
         while Joystick.Buffer.Count > 0 do
          begin
           {Wait for Data (Should not Block)}
           if SemaphoreWait(Joystick.Buffer.Wait) = ERROR_SUCCESS then
            begin
             {Update Start}
             Joystick.Buffer.Start:=(Joystick.Buffer.Start + 1) mod JOYSTICK_BUFFER_SIZE;

             {Update Count}
             Dec(Joystick.Buffer.Count);
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
       JOYSTICK_CONTROL_GET_HAT:begin
         {Get Hat}
         if Argument1 <= JOYSTICK_MAX_HAT then
          begin
           Argument2:=Joystick.Properties.Hats[Argument1].Name;

           {Return Result}
           Result:=ERROR_SUCCESS;
          end;
        end;
       JOYSTICK_CONTROL_SET_HAT:begin
         {Set Hat}
         if Argument1 <= JOYSTICK_MAX_HAT then
          begin
           Joystick.Properties.Hats[Argument1].Name:=Argument2;

           {Return Result}
           Result:=ERROR_SUCCESS;
          end;
        end;
       JOYSTICK_CONTROL_GET_AXIS:begin
         {Get Axis}
         if Argument1 <= JOYSTICK_MAX_AXIS then
          begin
           Argument2:=Joystick.Properties.Axes[Argument1].Name;

           {Return Result}
           Result:=ERROR_SUCCESS;
          end;
        end;
       JOYSTICK_CONTROL_SET_AXIS:begin
         {Set Axis}
         if Argument1 <= JOYSTICK_MAX_AXIS then
          begin
           Joystick.Properties.Axes[Argument1].Name:=Argument2;

           {Return Result}
           Result:=ERROR_SUCCESS;
          end;
        end;
       JOYSTICK_CONTROL_GET_BUTTON:begin
         {Get Button}
         if Argument1 <= JOYSTICK_MAX_BUTTON then
          begin
           Argument2:=Joystick.Properties.Buttons[Argument1];

           {Return Result}
           Result:=ERROR_SUCCESS;
          end;
        end;
       JOYSTICK_CONTROL_SET_BUTTON:begin
         {Set Button}
         if Argument1 <= JOYSTICK_MAX_BUTTON then
          begin
           Joystick.Properties.Buttons[Argument1]:=Argument2;

           {Return Result}
           Result:=ERROR_SUCCESS;
          end;
        end;
       JOYSTICK_CONTROL_GET_CALLBACK:begin
         {Get Callback}
         Argument2:=PtrUInt(@Joystick.Event);

         {Return Result}
         Result:=ERROR_SUCCESS;
        end;
       JOYSTICK_CONTROL_SET_CALLBACK:begin
         {Set Callback}
         Joystick.Event:=TJoystickEvent(Argument1);
         Joystick.Parameter:=Pointer(Argument2);

         {Return Result}
         Result:=ERROR_SUCCESS;
        end;
      end;
     finally
      {Release the Lock}
      MutexUnlock(Joystick.Lock);
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

function JoystickDeviceGetProperties(Joystick:PJoystickDevice;Properties:PJoystickProperties):LongWord;
{Get the properties for the specified Joystick device}
{Joystick: The Joystick device to get properties from}
{Properties: Pointer to a TJoystickProperties structure to fill in}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Properties}
 if Properties = nil then Exit;

 {Check Joystick}
 if Joystick = nil then Exit;
 if Joystick.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF JOYSTICK_DEBUG}
 if JOYSTICK_LOG_ENABLED then JoystickLogDebug(Joystick,'Joystick Device Get Properties');
 {$ENDIF}

 {Check Enabled}
 {Result:=ERROR_NOT_SUPPORTED;}
 {if Joystick.JoystickStatus <> JOYSTICK_STATUS_ENABLED then Exit;} {Allow when disabled}

 if MutexLock(Joystick.Lock) = ERROR_SUCCESS then
  begin
   if Assigned(Joystick.DeviceGetProperties) then
    begin
     {Call Device Get Properites}
     Result:=Joystick.DeviceGetProperties(Joystick,Properties);
    end
   else
    begin
     {Get Properties}
     System.Move(Joystick.Properties,Properties^,SizeOf(TJoystickProperties));

     {Return Result}
     Result:=ERROR_SUCCESS;
    end;

   MutexUnlock(Joystick.Lock);
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function JoystickDeviceSetState(Joystick:PJoystickDevice;State:LongWord):LongWord;
{Set the state of the specified joystick and send a notification}
{Joystick: The joystick to set the state for}
{State: The new state to set and notify}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Joystick}
 if Joystick = nil then Exit;
 if Joystick.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check State}
 if State > JOYSTICK_STATE_MAX then Exit;

 {Check State}
 if Joystick.JoystickState = State then
  begin
   {Return Result}
   Result:=ERROR_SUCCESS;
  end
 else
  begin
   {Acquire the Lock}
   if MutexLock(Joystick.Lock) = ERROR_SUCCESS then
    begin
     try
      {Set State}
      Joystick.JoystickState:=State;

      {Notify State}
      NotifierNotify(@Joystick.Device,JoystickDeviceStateToNotification(State));

      {Return Result}
      Result:=ERROR_SUCCESS;
     finally
      {Release the Lock}
      MutexUnlock(Joystick.Lock);
     end;
    end
   else
    begin
     Result:=ERROR_CAN_NOT_COMPLETE;
    end;
  end;
end;

{==============================================================================}

function JoystickDeviceCreate:PJoystickDevice;
{Create a new Joystick device entry}
{Return: Pointer to new Joystick device entry or nil if Joystick device could not be created}
begin
 {}
 Result:=JoystickDeviceCreateEx(SizeOf(TJoystickDevice));
end;

{==============================================================================}

function JoystickDeviceCreateEx(Size:LongWord):PJoystickDevice;
{Create a new Joystick device entry}
{Size: Size in bytes to allocate for new Joystick device (Including the Joystick device entry)}
{Return: Pointer to new Joystick device entry or nil if Joystick device could not be created}
begin
 {}
 Result:=nil;

 {Check Size}
 if Size < SizeOf(TJoystickDevice) then Exit;

 {Create Joystick}
 Result:=PJoystickDevice(DeviceCreateEx(Size));
 if Result = nil then Exit;

 {Update Device}
 Result.Device.DeviceBus:=DEVICE_BUS_NONE;
 Result.Device.DeviceType:=JOYSTICK_TYPE_NONE;
 Result.Device.DeviceFlags:=JOYSTICK_FLAG_NONE;
 Result.Device.DeviceData:=nil;

 {Update Joystick}
 Result.JoystickId:=DEVICE_ID_ANY;
 Result.JoystickState:=JOYSTICK_STATE_DISABLED;
 Result.DeviceStart:=nil;
 Result.DeviceStop:=nil;
 Result.DevicePeek:=nil;
 Result.DeviceRead:=nil;
 Result.DeviceWrite:=nil;
 Result.DeviceFlush:=nil;
 Result.DeviceUpdate:=nil;
 Result.DeviceControl:=nil;
 Result.DeviceGetProperties:=nil;
 Result.Lock:=INVALID_HANDLE_VALUE;
 Result.Event:=nil;
 Result.Parameter:=nil;
 Result.Buffer.Wait:=INVALID_HANDLE_VALUE;

 {Update Properties}
 Result.Properties.Flags:=Result.Device.DeviceFlags;

 {Create Lock}
 Result.Lock:=MutexCreateEx(False,MUTEX_DEFAULT_SPINCOUNT,MUTEX_FLAG_RECURSIVE);
 if Result.Lock = INVALID_HANDLE_VALUE then
  begin
   if JOYSTICK_LOG_ENABLED then JoystickLogError(nil,'Failed to create lock for Joystick device');

   JoystickDeviceDestroy(Result);

   Result:=nil;
   Exit;
  end;

 {Create Buffer Semaphore}
 Result.Buffer.Wait:=SemaphoreCreate(0);
 if Result.Buffer.Wait = INVALID_HANDLE_VALUE then
  begin
   if JOYSTICK_LOG_ENABLED then JoystickLogError(nil,'Failed to create buffer semaphore for Joystick device');

   JoystickDeviceDestroy(Result);

   Result:=nil;
   Exit;
  end;
end;

{==============================================================================}

function JoystickDeviceDestroy(Joystick:PJoystickDevice):LongWord;
{Destroy an existing Joystick device entry}
{Joystick: The Joystick device to destroy}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Joystick}
 if Joystick = nil then Exit;
 if Joystick.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Joystick}
 Result:=ERROR_IN_USE;
 if JoystickDeviceCheck(Joystick) = Joystick then Exit;

 {Check State}
 if Joystick.Device.DeviceState <> DEVICE_STATE_UNREGISTERED then Exit;

 {Destroy Buffer Semaphore}
 if Joystick.Buffer.Wait <> INVALID_HANDLE_VALUE then
  begin
   SemaphoreDestroy(Joystick.Buffer.Wait);
  end;

 {Destroy Lock}
 if Joystick.Lock <> INVALID_HANDLE_VALUE then
  begin
   MutexDestroy(Joystick.Lock);
  end;

 {Destroy Joystick}
 Result:=DeviceDestroy(@Joystick.Device);
end;

{==============================================================================}

function JoystickDeviceRegister(Joystick:PJoystickDevice):LongWord;
{Register a new Joystick device in the Joystick device table}
{Joystick: The Joystick device to register}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 JoystickId:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Joystick}
 if Joystick = nil then Exit;
 if Joystick.JoystickId <> DEVICE_ID_ANY then Exit;
 if Joystick.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Interfaces}
 if not(Assigned(Joystick.DeviceStart)) then Exit;
 if not(Assigned(Joystick.DeviceStop)) then Exit;

 {Check Joystick}
 Result:=ERROR_ALREADY_EXISTS;
 if JoystickDeviceCheck(Joystick) = Joystick then Exit;

 {Check State}
 if Joystick.Device.DeviceState <> DEVICE_STATE_UNREGISTERED then Exit;

 {Insert Joystick}
 if CriticalSectionLock(JoystickDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Update Joystick}
    JoystickId:=0;
    while JoystickDeviceFind(JoystickId) <> nil do
     begin
      Inc(JoystickId);
     end;
    Joystick.JoystickId:=JoystickId;

    {Update Device}
    Joystick.Device.DeviceName:=JOYSTICK_NAME_PREFIX + IntToStr(Joystick.JoystickId);
    Joystick.Device.DeviceClass:=DEVICE_CLASS_JOYSTICK;

    {Register Device}
    Result:=DeviceRegister(@Joystick.Device);
    if Result <> ERROR_SUCCESS then
     begin
      Joystick.JoystickId:=DEVICE_ID_ANY;
      Exit;
     end;

    {Link Joystick}
    if JoystickDeviceTable = nil then
     begin
      JoystickDeviceTable:=Joystick;
     end
    else
     begin
      Joystick.Next:=JoystickDeviceTable;
      JoystickDeviceTable.Prev:=Joystick;
      JoystickDeviceTable:=Joystick;
     end;

    {Increment Count}
    Inc(JoystickDeviceTableCount);

    {Check Default}
    if JoystickDeviceDefault = nil then
     begin
      JoystickDeviceDefault:=Joystick;
     end;

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    CriticalSectionUnlock(JoystickDeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function JoystickDeviceDeregister(Joystick:PJoystickDevice):LongWord;
{Deregister a Joystick device from the Joystick device table}
{Joystick: The Joystick device to deregister}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Prev:PJoystickDevice;
 Next:PJoystickDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Joystick}
 if Joystick = nil then Exit;
 if Joystick.JoystickId = DEVICE_ID_ANY then Exit;
 if Joystick.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Joystick}
 Result:=ERROR_NOT_FOUND;
 if JoystickDeviceCheck(Joystick) <> Joystick then Exit;

 {Check State}
 if Joystick.Device.DeviceState <> DEVICE_STATE_REGISTERED then Exit;

 {Remove Joystick}
 if CriticalSectionLock(JoystickDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Deregister Device}
    Result:=DeviceDeregister(@Joystick.Device);
    if Result <> ERROR_SUCCESS then Exit;

    {Unlink Joystick}
    Prev:=Joystick.Prev;
    Next:=Joystick.Next;
    if Prev = nil then
     begin
      JoystickDeviceTable:=Next;
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
    Dec(JoystickDeviceTableCount);

    {Check Default}
    if JoystickDeviceDefault = Joystick then
     begin
      JoystickDeviceDefault:=JoystickDeviceTable;
     end;

    {Update Joystick}
    Joystick.JoystickId:=DEVICE_ID_ANY;

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    CriticalSectionUnlock(JoystickDeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function JoystickDeviceFind(JoystickId:LongWord):PJoystickDevice;
{Find a Joystick device by ID in the Joystick device table}
{JoystickId: The ID number of the Joystick device to find}
{Return: Pointer to Joystick device entry or nil if not found}
var
 Joystick:PJoystickDevice;
begin
 {}
 Result:=nil;

 {Check Id}
 if JoystickId = DEVICE_ID_ANY then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(JoystickDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Joystick}
    Joystick:=JoystickDeviceTable;
    while Joystick <> nil do
     begin
      {Check State}
      if Joystick.Device.DeviceState = DEVICE_STATE_REGISTERED then
       begin
        {Check Id}
        if Joystick.JoystickId = JoystickId then
         begin
          Result:=Joystick;
          Exit;
         end;
       end;

      {Get Next}
      Joystick:=Joystick.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(JoystickDeviceTableLock);
   end;
  end;
end;

{==============================================================================}

function JoystickDeviceFindByName(const Name:String):PJoystickDevice; inline;
{Find a Joystick device by name in the device table}
{Name: The name of the Joystick device to find (eg Joystick0)}
{Return: Pointer to Joystick device entry or nil if not found}
begin
 {}
 Result:=PJoystickDevice(DeviceFindByName(Name));
end;

{==============================================================================}

function JoystickDeviceFindByDescription(const Description:String):PJoystickDevice; inline;
{Find a Joystick device by description in the device table}
{Description: The description of the Joystick to find (eg USB Gamepad)}
{Return: Pointer to Joystick device entry or nil if not found}
begin
 {}
 Result:=PJoystickDevice(DeviceFindByDescription(Description));
end;

{==============================================================================}

function JoystickDeviceEnumerate(Callback:TJoystickEnumerate;Data:Pointer):LongWord;
{Enumerate all Joystick devices in the Joystick device table}
{Callback: The callback function to call for each Joystick device in the table}
{Data: A private data pointer to pass to callback for each Joystick device in the table}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Joystick:PJoystickDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Callback}
 if not Assigned(Callback) then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(JoystickDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Joystick}
    Joystick:=JoystickDeviceTable;
    while Joystick <> nil do
     begin
      {Check State}
      if Joystick.Device.DeviceState = DEVICE_STATE_REGISTERED then
       begin
        if Callback(Joystick,Data) <> ERROR_SUCCESS then Exit;
       end;

      {Get Next}
      Joystick:=Joystick.Next;
     end;

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    CriticalSectionUnlock(JoystickDeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function JoystickDeviceNotification(Joystick:PJoystickDevice;Callback:TJoystickNotification;Data:Pointer;Notification,Flags:LongWord):LongWord;
{Register a notification for Joystick device changes}
{Joystick: The Joystick device to notify changes for (Optional, pass nil for all Joystick devices)}
{Callback: The function to call when a notification event occurs}
{Data: A private data pointer to pass to callback when a notification event occurs}
{Notification: The events to register for notification of (eg DEVICE_NOTIFICATION_REGISTER)}
{Flags: The flags to control the notification (eg NOTIFIER_FLAG_WORKER)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Joystick}
 if Joystick = nil then
  begin
   Result:=DeviceNotification(nil,DEVICE_CLASS_JOYSTICK,Callback,Data,Notification,Flags);
  end
 else
  begin
   {Check Joystick}
   if Joystick.Device.Signature <> DEVICE_SIGNATURE then Exit;

   Result:=DeviceNotification(@Joystick.Device,DEVICE_CLASS_JOYSTICK,Callback,Data,Notification,Flags);
  end;
end;

{==============================================================================}
{==============================================================================}
{RTL Joystick Functions}

{==============================================================================}
{==============================================================================}
{Joystick Helper Functions}
function JoystickGetCount:LongWord;
{Get the current Joystick device count}
{Return: The number of Joystick devices}
begin
 {}
 Result:=JoystickDeviceTableCount;
end;

{==============================================================================}

function JoystickDeviceGetDefault:PJoystickDevice;
{Get the current default Joystick device}
{Return: Pointer to default Joystick device entry}
begin
 {}
 Result:=JoystickDeviceDefault;
end;

{==============================================================================}

function JoystickDeviceSetDefault(Joystick:PJoystickDevice):LongWord;
{Set the current default Joystick device}
{Joystick: The Joystick device to set as default}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Joystick}
 if Joystick = nil then Exit;
 if Joystick.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(JoystickDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Check Joystick}
    if JoystickDeviceCheck(Joystick) <> Joystick then Exit;

    {Set Joystick Default}
    JoystickDeviceDefault:=Joystick;

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    CriticalSectionUnlock(JoystickDeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function JoystickDeviceCheck(Joystick:PJoystickDevice):PJoystickDevice;
{Check if the supplied Joystick device is in the Joystick device table}
{Joystick: The Joystick device to check}
{Return: Pointer to Joystick device entry or nil if not found}
var
 Current:PJoystickDevice;
begin
 {}
 Result:=nil;

 {Check Joystick}
 if Joystick = nil then Exit;
 if Joystick.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(JoystickDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Joystick}
    Current:=JoystickDeviceTable;
    while Current <> nil do
     begin
      {Check Joystick}
      if Current = Joystick then
       begin
        Result:=Joystick;
        Exit;
       end;

      {Get Next}
      Current:=Current.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(JoystickDeviceTableLock);
   end;
  end;
end;

{==============================================================================}

function JoystickDeviceAxisToString(Name:LongWord):String;
{Return a string describing a Joystick or Gamepad Axis (eg JOYSTICK_AXIS_X)}
begin
 {}
 Result:='JOYSTICK_AXIS_NONE';

 if Name <= JOYSTICK_AXIS_MAX then
  begin
   Result:=JOYSTICK_AXIS_NAMES[Name];
  end;
end;

{==============================================================================}

function JoystickDeviceHatToString(Name:LongWord):String;
{Return a string describing a Joystick or Gamepad Hat (eg JOYSTICK_HAT_POV)}
begin
 {}
 Result:='JOYSTICK_HAT_NONE';

 if Name <= JOYSTICK_HAT_MAX then
  begin
   Result:=JOYSTICK_HAT_NAMES[Name];
  end;
end;

{==============================================================================}

function JoystickDeviceButtonToString(Name:LongWord):String;
{Return a string describing a Joystick or Gamepad Button (eg GAMEPAD_BUTTON_UP)}
begin
 {}
 Result:='JOYSTICK_BUTTON_NONE';

 if Name <= JOYSTICK_BUTTON_MAX then
  begin
   Result:=JOYSTICK_BUTTON_NAMES[Name];
  end;
end;

{==============================================================================}

function JoystickDeviceTypeToString(JoystickType:LongWord):String;
{Return a string describing the Joystick device type (eg JOYSTICK_TYPE_JOYSTICK)}
begin
 {}
 Result:='JOYSTICK_TYPE_UNKNOWN';

 if JoystickType <= JOYSTICK_TYPE_MAX then
  begin
   Result:=JOYSTICK_TYPE_NAMES[JoystickType];
  end;
end;

{==============================================================================}

function JoystickDeviceStateToString(JoystickState:LongWord):String;
{Return a string describing the Joystick device state (eg JOYSTICK_STATE_ENABLED)}
begin
 {}
 Result:='JOYSTICK_STATE_UNKNOWN';

 if JoystickState <= JOYSTICK_STATE_MAX then
  begin
   Result:=JOYSTICK_STATE_NAMES[JoystickState];
  end;
end;

{==============================================================================}

function JoystickDeviceStateToNotification(State:LongWord):LongWord;
{Convert a Joystick state value into the notification code for device notifications}
begin
 {}
 Result:=DEVICE_NOTIFICATION_NONE;

 {Check State}
 case State of
  JOYSTICK_STATE_DISABLED:Result:=DEVICE_NOTIFICATION_DISABLE;
  JOYSTICK_STATE_ENABLED:Result:=DEVICE_NOTIFICATION_ENABLE;
 end;
end;

{==============================================================================}

function JoystickDeviceGetAxis(Joystick:PJoystickDevice;Index:LongWord):LongWord;
{Get the name (identifier) of an Axis on the specified Joystick}
{Joystick: The Joystick device to get the name from}
{Index: The index of the Axis in the Joystick properties (First Axis is 0)}
{Return: The current name of the Axis (eg GAMEPAD_AXIS_LEFT_X)}
var
 Argument2:PtrUInt;
begin
 {}
 Result:=JOYSTICK_AXIS_NONE;

 {Get Axis}
 if JoystickDeviceControl(Joystick,JOYSTICK_CONTROL_GET_AXIS,Index,Argument2) = ERROR_SUCCESS then
  begin
   Result:=Argument2;
  end;
end;

{==============================================================================}

function JoystickDeviceSetAxis(Joystick:PJoystickDevice;Index,Name:LongWord):LongWord;
{Set the name (identifier) of an Axis on the specified Joystick}
{Joystick: The Joystick device to set the name for}
{Index: The index of the Axis in the Joystick properties (First Axis is 0)}
{Name: The name (identifier) to set for the Axis (eg JOYSTICK_AXIS_X}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Argument2:PtrUInt;
begin
 {}
 Argument2:=Name;

 {Set Axis}
 Result:=JoystickDeviceControl(Joystick,JOYSTICK_CONTROL_SET_AXIS,Index,Argument2);
end;

{==============================================================================}

function JoystickDeviceGetHat(Joystick:PJoystickDevice;Index:LongWord):LongWord;
{Get the name (identifier) of a Hat on the specified Joystick}
{Joystick: The Joystick device to get the name from}
{Index: The index of the Hat in the Joystick properties (First Hat is 0)}
{Return: The current name of the Hat (eg GAMEPAD_HAT_POV)}
var
 Argument2:PtrUInt;
begin
 {}
 Result:=JOYSTICK_HAT_NONE;

 {Get Hat}
 if JoystickDeviceControl(Joystick,JOYSTICK_CONTROL_GET_HAT,Index,Argument2) = ERROR_SUCCESS then
  begin
   Result:=Argument2;
  end;
end;

{==============================================================================}

function JoystickDeviceSetHat(Joystick:PJoystickDevice;Index,Name:LongWord):LongWord;
{Set the name (identifier) of a Hat on the specified Joystick}
{Joystick: The Joystick device to set the name for}
{Index: The index of the Hat in the Joystick properties (First Hat is 0)}
{Name: The name (identifier) to set for the Hat (eg JOYSTICK_HAT_POV)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Argument2:PtrUInt;
begin
 {}
 Argument2:=Name;

 {Set Hat}
 Result:=JoystickDeviceControl(Joystick,JOYSTICK_CONTROL_SET_HAT,Index,Argument2);
end;

{==============================================================================}

function JoystickDeviceGetButton(Joystick:PJoystickDevice;Index:LongWord):LongWord;
{Get the name (identifier) of a Button on the specified Joystick}
{Joystick: The Joystick device to get the name from}
{Index: The index of the Button in the Joystick properties (First Button is 0)}
{Return: The current name of the Button (eg GAMEPAD_BUTTON_B)}
var
 Argument2:PtrUInt;
begin
 {}
 Result:=JOYSTICK_BUTTON_NONE;

 {Get Button}
 if JoystickDeviceControl(Joystick,JOYSTICK_CONTROL_GET_BUTTON,Index,Argument2) = ERROR_SUCCESS then
  begin
   Result:=Argument2;
  end;
end;

{==============================================================================}

function JoystickDeviceSetButton(Joystick:PJoystickDevice;Index,Name:LongWord):LongWord;
{Set the name (identifier) of a Button on the specified Joystick}
{Joystick: The Joystick device to set the name for}
{Index: The index of the Button in the Joystick properties (First Button is 0)}
{Name: The name (identifier) to set for the Button (eg GAMEPAD_BUTTON_LT)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Argument2:PtrUInt;
begin
 {}
 Argument2:=Name;

 {Set Button}
 Result:=JoystickDeviceControl(Joystick,JOYSTICK_CONTROL_SET_BUTTON,Index,Argument2);
end;

{==============================================================================}

function JoystickDeviceSetCallback(Joystick:PJoystickDevice;Event:TJoystickEvent;Parameter:Pointer):LongWord;
{Set the event callback function for the specified Joystick}
{Joystick: The Joystick device to set the event callback for}
{Event: The event callback function to be called when Joystick data is received}
{Parameter: A pointer to private data to be passed to the callback with each event}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 {Set Callback}
 Result:=JoystickDeviceControl(Joystick,JOYSTICK_CONTROL_SET_CALLBACK,PtrUInt(@Event),PtrUInt(Parameter));
end;

{==============================================================================}

function JoystickInsertData(Joystick:PJoystickDevice;Data:PJoystickData;Signal:Boolean):LongWord;
{Insert a TJoystickData entry into the joystick device buffer}
{Joystick: The joystick device to insert data for}
{Data: The TJoystickData entry to insert}
{Signal: If True then signal that new data is available in the buffer}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: Caller must hold the joystick device lock}
var
 Next:PJoystickData;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Joystick}
 if Joystick = nil then Exit;

 {Check Data}
 if Data = nil then Exit;

 {Check Buffer}
 if (Joystick.Buffer.Count < JOYSTICK_BUFFER_SIZE) then
  begin
   {Get Next}
   Next:=@Joystick.Buffer.Buffer[(Joystick.Buffer.Start + Joystick.Buffer.Count) mod JOYSTICK_BUFFER_SIZE];
   if Next <> nil then
    begin
     {Copy Data}
     Next^:=Data^;

     {Update Count}
     Inc(Joystick.Buffer.Count);

     {Signal Data Received}
     if Signal then SemaphoreSignal(Joystick.Buffer.Wait);

     {Return Result}
     Result:=ERROR_SUCCESS;
    end;
  end
 else
  begin
   if JOYSTICK_LOG_ENABLED then JoystickLogError(Joystick,'Buffer overflow, data discarded');

   {Update Statistics}
   Inc(Joystick.BufferOverruns);

   Result:=ERROR_INSUFFICIENT_BUFFER;
  end;
end;

{==============================================================================}

procedure JoystickLog(Level:LongWord;Joystick:PJoystickDevice;const AText:String);
var
 WorkBuffer:String;
begin
 {}
 {Check Level}
 if Level < JOYSTICK_DEFAULT_LOG_LEVEL then Exit;

 WorkBuffer:='';
 {Check Level}
 if Level = JOYSTICK_LOG_LEVEL_DEBUG then
  begin
   WorkBuffer:=WorkBuffer + '[DEBUG] ';
  end
 else if Level = JOYSTICK_LOG_LEVEL_WARN then
  begin
   WorkBuffer:=WorkBuffer + '[WARN] ';
  end
 else if Level = JOYSTICK_LOG_LEVEL_ERROR then
  begin
   WorkBuffer:=WorkBuffer + '[ERROR] ';
  end;

 {Add Prefix}
 WorkBuffer:=WorkBuffer + 'Joystick: ';

 {Check Joystick}
 if Joystick <> nil then
  begin
   WorkBuffer:=WorkBuffer + JOYSTICK_NAME_PREFIX + IntToStr(Joystick.JoystickId) + ': ';
  end;

 {Output Logging}
 LoggingOutputEx(LOGGING_FACILITY_JOYSTICK,LogLevelToLoggingSeverity(Level),'Joystick',WorkBuffer + AText);
end;

{==============================================================================}

procedure JoystickLogInfo(Joystick:PJoystickDevice;const AText:String); inline;
begin
 {}
 JoystickLog(JOYSTICK_LOG_LEVEL_INFO,Joystick,AText);
end;

{==============================================================================}

procedure JoystickLogWarn(Joystick:PJoystickDevice;const AText:String); inline;
begin
 {}
 JoystickLog(JOYSTICK_LOG_LEVEL_WARN,Joystick,AText);
end;

{==============================================================================}

procedure JoystickLogError(Joystick:PJoystickDevice;const AText:String); inline;
begin
 {}
 JoystickLog(JOYSTICK_LOG_LEVEL_ERROR,Joystick,AText);
end;

{==============================================================================}

procedure JoystickLogDebug(Joystick:PJoystickDevice;const AText:String); inline;
begin
 {}
 JoystickLog(JOYSTICK_LOG_LEVEL_DEBUG,Joystick,AText);
end;

{==============================================================================}
{==============================================================================}
{Joystick Internal Functions}

{==============================================================================}
{==============================================================================}

initialization
 JoystickInit;

{==============================================================================}

finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
