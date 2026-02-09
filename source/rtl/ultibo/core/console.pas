{
Ultibo Console interface unit.

Copyright (C) 2025 - SoftOz Pty Ltd.

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


Console
=======

 Notes: Console coordinates X,Y are based on either pixels or characters depending on the console mode (Pixel or Character)
        Console coordinates begin at 0,0 and extend to Width - 1, Height - 1

        Console Window coordinates X,Y are always based on characters, beginning at 1,1 and extending to Cols,Rows


}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

{$IFNDEF FPC_DOTTEDUNITS}
unit Console;
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
  Core.Framebuffer,
  Core.Font,
  System.Types,
  System.SysUtils;
{$ELSE FPC_DOTTEDUNITS}
uses
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  Devices,
  Framebuffer,
  Font,
  Types,
  SysUtils;
{$ENDIF FPC_DOTTEDUNITS}

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
const
 {Console specific constants}
 CONSOLE_NAME_PREFIX = 'Console';    {Name prefix for Console Devices}

 {Console Device Types}
 CONSOLE_TYPE_NONE        = 0;
 CONSOLE_TYPE_FRAMEBUFFER = 1;
 CONSOLE_TYPE_SERIAL      = 2;
 CONSOLE_TYPE_REMOTE      = 3;
 CONSOLE_TYPE_LCD         = 4;

 CONSOLE_TYPE_MAX         = 4;

 {Console Type Names}
 CONSOLE_TYPE_NAMES:array[CONSOLE_TYPE_NONE..CONSOLE_TYPE_MAX] of String = (
  'CONSOLE_TYPE_NONE',
  'CONSOLE_TYPE_FRAMEBUFFER',
  'CONSOLE_TYPE_SERIAL',
  'CONSOLE_TYPE_REMOTE',
  'CONSOLE_TYPE_LCD');

 {Console Device States}
 CONSOLE_STATE_CLOSED   = 0;
 CONSOLE_STATE_OPEN     = 1;

 CONSOLE_STATE_MAX      = 1;

 {Console State Names}
 CONSOLE_STATE_NAMES:array[CONSOLE_STATE_CLOSED..CONSOLE_STATE_MAX] of String = (
  'CONSOLE_STATE_CLOSED',
  'CONSOLE_STATE_OPEN');

 {Console Device Flags}
 CONSOLE_FLAG_NONE            = $00000000;
 CONSOLE_FLAG_LINE_WRAP       = $00000001; {Wrap long lines to the next line if set}
 CONSOLE_FLAG_DMA_BOX         = $00000002; {Use DMA to draw boxes (Where applicable)}
 CONSOLE_FLAG_DMA_LINE        = $00000004; {Use DMA to draw lines (Where applicable)}
 CONSOLE_FLAG_DMA_FILL        = $00000008; {Use DMA to fill blocks (Where applicable)}
 CONSOLE_FLAG_DMA_CLEAR       = $00000010; {Use DMA to clear blocks (Where applicable)}
 CONSOLE_FLAG_DMA_SCROLL      = $00000020; {Use DMA to scroll blocks (Where applicable)}
 CONSOLE_FLAG_SINGLE_WINDOW   = $00000040; {Console supports only one window (Not multiple)}
 CONSOLE_FLAG_HARDWARE_CURSOR = $00000080; {Console supports a hardware cursor (Mouse pointer) (Character mode only)}
 CONSOLE_FLAG_HARDWARE_CARET  = $00000100; {Console supports a hardware caret (Text cursor)}
 CONSOLE_FLAG_BLINK_CARET     = $00000200; {Console supports blinking caret}
 CONSOLE_FLAG_TEXT_MODE       = $00000400; {Console supports text mode settings}
 CONSOLE_FLAG_TEXT_BLINK      = $00000800; {Console supports blinking text}
 CONSOLE_FLAG_COLOR           = $00001000; {Console supports colors}
 CONSOLE_FLAG_FONT            = $00002000; {Console supports fonts}
 CONSOLE_FLAG_FULLSCREEN      = $00004000; {Console supports creating a fullscreen window}
 CONSOLE_FLAG_AUTO_SCROLL     = $00008000; {Automatically scroll up on reaching the last line}
 CONSOLE_FLAG_DMA_TEXT        = $00010000; {Use DMA to draw text (Where applicable)}
 CONSOLE_FLAG_COLOR_REVERSE   = $00020000; {Console requires colors to be reversed for underlying hardware}
 CONSOLE_FLAG_TEXT_CARET      = $00040000; {Console supports a caret (Text cursor}
 CONSOLE_FLAG_FOCUS_CARET     = $00080000; {Only show caret on the focused (active) window}

 CONSOLE_FLAG_DMA_MASK = CONSOLE_FLAG_DMA_BOX or CONSOLE_FLAG_DMA_LINE or CONSOLE_FLAG_DMA_FILL or CONSOLE_FLAG_DMA_CLEAR or CONSOLE_FLAG_DMA_SCROLL or CONSOLE_FLAG_DMA_TEXT;

 {Flags that cannot be changed by ConsoleDeviceUpdateFlag}
 CONSOLE_FLAG_INTERNAL = CONSOLE_FLAG_SINGLE_WINDOW or CONSOLE_FLAG_HARDWARE_CURSOR or CONSOLE_FLAG_HARDWARE_CARET or CONSOLE_FLAG_BLINK_CARET or CONSOLE_FLAG_TEXT_MODE
                         or CONSOLE_FLAG_TEXT_BLINK or CONSOLE_FLAG_COLOR or CONSOLE_FLAG_FONT or CONSOLE_FLAG_FULLSCREEN or CONSOLE_FLAG_COLOR_REVERSE or CONSOLE_FLAG_TEXT_CARET;

 {Console Device Modes}
 CONSOLE_MODE_NONE      = 0;
 CONSOLE_MODE_PIXEL     = 1;
 CONSOLE_MODE_CHARACTER = 2;

 {Console Caret Signature}
 CARET_SIGNATURE = $9A2D40E3;

 {Console Window Signature}
 WINDOW_SIGNATURE = $DE3A5C04;

 {Console Window States}
 WINDOW_STATE_INVISIBLE = 0;
 WINDOW_STATE_VISIBLE   = 1;

 WINDOW_STATE_MAX       = 1;

 {Window State Names}
 WINDOW_STATE_NAMES:array[WINDOW_STATE_INVISIBLE..WINDOW_STATE_MAX] of String = (
  'WINDOW_STATE_INVISIBLE',
  'WINDOW_STATE_VISIBLE');

 {Console Window Modes}
 WINDOW_MODE_NONE       = 0;
 WINDOW_MODE_TEXT       = 1;
 WINDOW_MODE_GRAPHICS   = 2;

 WINDOW_MODE_MAX        = 2;

 {Window Mode Names}
 WINDOW_MODE_NAMES:array[WINDOW_MODE_NONE..WINDOW_MODE_MAX] of String = (
  'WINDOW_MODE_NONE',
  'WINDOW_MODE_TEXT',
  'WINDOW_MODE_GRAPHICS');

 {Console Window Flags}
 WINDOW_FLAG_NONE          = $00000000;
 WINDOW_FLAG_LINE_WRAP     = $00000001; {Wrap long lines to the next line if set}
 WINDOW_FLAG_BUFFERED      = $00000002; {Buffer output for scroll back and redraw}
 WINDOW_FLAG_FULLSCREEN    = $00000004; {Window occupies the full screen}
 WINDOW_FLAG_AUTO_SCROLL   = $00000008; {Automatically scroll up on reaching the last line}
 WINDOW_FLAG_CHARACTER     = $00000010; {Console for this Window is character mode only}
 WINDOW_FLAG_AUTO_UPDATE   = $00000020; {Automatically update output of buffered window}
 WINDOW_FLAG_FOCUS_CURSOR  = $00000040; {Only show cursor (caret) on the focused (active) window}

 {Flags that cannot be changed by ConsoleWindowUpdateFlag}
 WINDOW_FLAG_INTERNAL = WINDOW_FLAG_FULLSCREEN or WINDOW_FLAG_CHARACTER;

 {Console Window Draw Flags}
 WINDOW_DRAW_FLAG_NONE   = $00000000;
 WINDOW_DRAW_FLAG_BODY   = $00000001; {Draw the Window body}
 WINDOW_DRAW_FLAG_TITLE  = $00000002; {Draw the Window title}
 WINDOW_DRAW_FLAG_BORDER = $00000004; {Draw the Window border}

 WINDOW_DRAW_FLAG_ALL = WINDOW_DRAW_FLAG_BODY or WINDOW_DRAW_FLAG_TITLE or WINDOW_DRAW_FLAG_BORDER;

 {Console Window History}
 WINDOW_HISTORY_MAX_COUNT = 100;

{==============================================================================}
const
 {Framebuffer Console specific constants}
 FRAMEBUFFER_CONSOLE_TITLE = 'Ultibo Core (Release: ' + ULTIBO_RELEASE_NAME + ' Version: ' + ULTIBO_RELEASE_VERSION + ' Date: ' + ULTIBO_RELEASE_DATE + ')';
 FRAMEBUFFER_CONSOLE_DESCRIPTION = 'Framebuffer Console';  {Description of the Framebuffer Console device}

{==============================================================================}
type
 {Console specific types}
 TCursorMode  = (CURSOR_MODE_INSERT,CURSOR_MODE_OVERWRITE);
 TCursorState = (CURSOR_STATE_ON,CURSOR_STATE_OFF);
 TCursorShape = (CURSOR_SHAPE_LINE,CURSOR_SHAPE_BAR,CURSOR_SHAPE_BLOCK); {Line is a vertical line, Bar is a horizontal bar and Block is a solid block (All shapes inverse any character data under them)}

 PConsoleChar = ^TConsoleChar;
 TConsoleChar = record
  Ch:Char;
  Forecolor:LongWord;
  Backcolor:LongWord;
 end;

 TConsolePoint = record
  X:LongWord;
  Y:LongWord;
 end;

 TConsoleRect = record
  X1:LongWord;
  Y1:LongWord;
  X2:LongWord;
  Y2:LongWord;
 end;

 PConsoleProperties = ^TConsoleProperties;
 TConsoleProperties = record
  Flags:LongWord;               {Console device flags (eg CONSOLE_FLAG_FULLSCREEN)}
  Mode:LongWord;                {Console device mode (eg CONSOLE_MODE_PIXEL)}
  Width:LongWord;               {Console Width (Pixels for CONSOLE_MODE_PIXEL / Characters for CONSOLE_MODE_CHARACTER)}
  Height:LongWord;              {Console Height (Pixels for CONSOLE_MODE_PIXEL / Characters for CONSOLE_MODE_CHARACTER)}
  Format:LongWord;              {Color Format (eg COLOR_FORMAT_ARGB32)(Only applicable if CONSOLE_MODE_PIXEL)}
 end;

 PConsoleDevice = ^TConsoleDevice;

 {Console Enumeration Callback}
 TConsoleEnumerate = function(Console:PConsoleDevice;Data:Pointer):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 {Console Notification Callback}
 TConsoleNotification = function(Device:PDevice;Data:Pointer;Notification:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}

 {Console Device Methods}
 TConsoleDeviceOpen = function(Console:PConsoleDevice):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TConsoleDeviceClose = function(Console:PConsoleDevice):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TConsoleDeviceClear = function(Console:PConsoleDevice;Color:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TConsoleDeviceScroll = function(Console:PConsoleDevice;X1,Y1,X2,Y2,Count,Direction:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TConsoleDeviceDrawBox = function(Console:PConsoleDevice;X1,Y1,X2,Y2,Color,Width:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TConsoleDeviceDrawLine = function(Console:PConsoleDevice;X1,Y1,X2,Y2,Color,Width:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TConsoleDevicePlotLine = function(Console:PConsoleDevice;X1,Y1,X2,Y2,Color,Width:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TConsoleDeviceDrawChar = function(Console:PConsoleDevice;Handle:TFontHandle;Ch:Char;X,Y,Forecolor,Backcolor:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TConsoleDeviceDrawText = function(Console:PConsoleDevice;Handle:TFontHandle;const Text:String;X,Y,Forecolor,Backcolor,Len:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TConsoleDeviceDrawPixel = function(Console:PConsoleDevice;X,Y,Color:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TConsoleDeviceDrawBlock = function(Console:PConsoleDevice;X1,Y1,X2,Y2,Color:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TConsoleDeviceDrawCircle = function(Console:PConsoleDevice;X,Y,Color,Width,Radius:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TConsoleDeviceDrawImage = function(Console:PConsoleDevice;X,Y:LongWord;Buffer:Pointer;Width,Height,Format,Skip:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TConsoleDeviceDrawWindow = function(Console:PConsoleDevice;Handle:TWindowHandle;Flags:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}

 TConsoleDeviceGetPixel = function(Console:PConsoleDevice;X,Y:LongWord;var Color:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TConsoleDeviceGetImage = function(Console:PConsoleDevice;X,Y:LongWord;Buffer:Pointer;Width,Height,Format,Skip:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}

 TConsoleDevicePutText = function(Console:PConsoleDevice;Handle:TFontHandle;const Source,Dest:TConsolePoint;Buffer:PConsoleChar;Width,Height,Skip:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}

 TConsoleDeviceCopyImage = function(Console:PConsoleDevice;const Source,Dest:TConsolePoint;Width,Height:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}

 TConsoleDeviceAddCaret = function(Console:PConsoleDevice;Width,Height,OffsetX,OffsetY:LongWord):THandle;{$IFDEF i386} stdcall;{$ENDIF}
 TConsoleDeviceDeleteCaret = function(Console:PConsoleDevice;Handle:THandle):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TConsoleDeviceUpdateCaret = function(Console:PConsoleDevice;Handle:THandle;X,Y:LongWord;Visible,Blink:Boolean):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TConsoleDeviceUpdateCaretEx = function(Console:PConsoleDevice;Handle:THandle;X,Y,Forecolor,Backcolor:LongWord;Visible,Blink,Reverse:Boolean):LongWord;{$IFDEF i386} stdcall;{$ENDIF}

 TConsoleDeviceSetCursor = function(Console:PConsoleDevice;Width,Height:LongWord;Chars:PChar):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TConsoleDeviceUpdateCursor = function(Console:PConsoleDevice;Enabled:Boolean;X,Y:LongInt;Relative:Boolean):LongWord;{$IFDEF i386} stdcall;{$ENDIF}

 TConsoleDeviceGetPosition = function(Console:PConsoleDevice;Position:LongWord;var X1,Y1,X2,Y2:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TConsoleDeviceGetProperties = function(Console:PConsoleDevice;Properties:PConsoleProperties):LongWord;{$IFDEF i386} stdcall;{$ENDIF}

 PConsoleCaret = ^TConsoleCaret;
 PConsoleWindow = ^TConsoleWindow;
 PConsoleHistory = ^TConsoleHistory;

 {Console Device}
 TConsoleDevice = record
  {Device Properties}
  Device:TDevice;                                {The Device entry for this Console device}
  {Console Properties}
  ConsoleId:LongWord;                            {Unique Id of this Console device in the Console device table}
  ConsoleState:LongWord;                         {Console device state (eg CONSOLE_STATE_OPEN)}
  ConsoleMode:LongWord;                          {Console device mode (eg CONSOLE_MODE_PIXEL)}
  DeviceOpen:TConsoleDeviceOpen;                 {A device specific DeviceOpen method implementing a standard console device interface (Mandatory)}
  DeviceClose:TConsoleDeviceClose;               {A device specific DeviceClose method implementing a standard console device interface (Mandatory)}
  DeviceClear:TConsoleDeviceClear;               {A device specific DeviceClear method implementing a standard console device interface (Mandatory)}
  DeviceScroll:TConsoleDeviceScroll;             {A device specific DeviceScroll method implementing a standard console device interface (Mandatory)}
  DeviceDrawBox:TConsoleDeviceDrawBox;           {A device specific DeviceDrawBox method implementing a standard console device interface (Mandatory for CONSOLE_MODE_PIXEL)}
  DeviceDrawLine:TConsoleDeviceDrawLine;         {A device specific DeviceDrawLine method implementing a standard console device interface (Mandatory for CONSOLE_MODE_PIXEL)}
  DevicePlotLine:TConsoleDevicePlotLine;         {A device specific DevicePlotLine method implementing a standard console device interface (Or nil if the default method is suitable)}
  DeviceDrawChar:TConsoleDeviceDrawChar;         {A device specific DeviceDrawChar method implementing a standard console device interface (Mandatory)}
  DeviceDrawText:TConsoleDeviceDrawText;         {A device specific DeviceDrawText method implementing a standard console device interface (Mandatory)}
  DeviceDrawPixel:TConsoleDeviceDrawPixel;       {A device specific DeviceDrawPixel method implementing a standard console device interface (Mandatory for CONSOLE_MODE_PIXEL)}
  DeviceDrawBlock:TConsoleDeviceDrawBlock;       {A device specific DeviceDrawBlock method implementing a standard console device interface (Mandatory)}
  DeviceDrawCircle:TConsoleDeviceDrawCircle;     {A device specific DeviceDrawCircle method implementing a standard console device interface (Or nil if the default method is suitable)}
  DeviceDrawImage:TConsoleDeviceDrawImage;       {A device specific DeviceDrawImage method implementing a standard console device interface (Mandatory for CONSOLE_MODE_PIXEL)}
  DeviceDrawWindow:TConsoleDeviceDrawWindow;     {A device specific DeviceDrawWindow method implementing a standard console device interface (Mandatory)}
  DeviceGetPixel:TConsoleDeviceGetPixel;         {A device specific DeviceGetPixel method implementing a standard console device interface (Mandatory for CONSOLE_MODE_PIXEL)}
  DeviceGetImage:TConsoleDeviceGetImage;         {A device specific DeviceGetImage method implementing a standard console device interface (Mandatory for CONSOLE_MODE_PIXEL)}
  DevicePutText:TConsoleDevicePutText;           {A device specific DevicePutText method implementing a standard console device interface (Mandatory)}
  DeviceCopyImage:TConsoleDeviceCopyImage;       {A device specific DeviceCopyImage method implementing a standard console device interface (Mandatory for CONSOLE_MODE_PIXEL)}
  DeviceAddCaret:TConsoleDeviceAddCaret;         {A device specific DeviceAddCaret method implementing a standard console device interface (Optional)}
  DeviceDeleteCaret:TConsoleDeviceDeleteCaret;   {A device specific DeviceDeleteCaret method implementing a standard console device interface (Optional)}
  DeviceUpdateCaret:TConsoleDeviceUpdateCaret;   {A device specific DeviceUpdateCaret method implementing a standard console device interface (Optional)}
  DeviceUpdateCaretEx:TConsoleDeviceUpdateCaretEx; {A device specific DeviceUpdateCaretEx method implementing a standard console device interface (Optional)}
  DeviceSetCursor:TConsoleDeviceSetCursor;       {A device specific DeviceSetCursor method implementing a standard console device interface (Or nil if the default method is suitable)(CONSOLE_MODE_CHARACTER only)}
  DeviceUpdateCursor:TConsoleDeviceUpdateCursor; {A device specific DeviceUpdateCursor method implementing a standard console device interface (Or nil if the default method is suitable)(CONSOLE_MODE_CHARACTER only)}
  DeviceGetPosition:TConsoleDeviceGetPosition;   {A device specific DeviceGetPosition method implementing a standard console device interface (Mandatory)}
  DeviceGetProperties:TConsoleDeviceGetProperties; {A device specific DeviceGetProperties method implementing a standard console device interface (Or nil if the default method is suitable)}
  {Statistics Properties}
  OpenCount:LongWord;
  CloseCount:LongWord;
  ClearCount:LongWord;
  ScrollCount:LongWord;
  DrawCount:LongWord;
  GetCount:LongWord;
  PutCount:LongWord;
  CopyCount:LongWord;
  {Driver Properties}
  Lock:TMutexHandle;                             {Device lock}
  Width:LongWord;                                {Console Width (Pixels for CONSOLE_MODE_PIXEL / Characters for CONSOLE_MODE_CHARACTER)}
  Height:LongWord;                               {Console Height (Pixels for CONSOLE_MODE_PIXEL / Characters for CONSOLE_MODE_CHARACTER)}
  Format:LongWord;                               {Color Format (eg COLOR_FORMAT_ARGB32)(Only applicable if CONSOLE_MODE_PIXEL)}
  Forecolor:LongWord;                            {Foreground Color}
  Backcolor:LongWord;                            {Background Color}
  Borderwidth:LongWord;                          {Border Width (Pixels for CONSOLE_MODE_PIXEL / Characters for CONSOLE_MODE_CHARACTER)}
  Bordercolor:LongWord;                          {Border Color}
  {Font Properties}
  Font:TFontHandle;                              {Console Font}
  FontRatio:LongWord;                            {Font Characters to Pixels Ratio (Normally 1 for Pixel Console / 0 for Character Console)}
  {Cursor Properties}
  CursorUpdate:LongBool;                         {Flag to indicate if cursor update (Show/Hide) is in progress (CONSOLE_MODE_CHARACTER only)}
  CursorX:LongWord;                              {Cursor X (Characters)(CONSOLE_MODE_CHARACTER only)}
  CursorY:LongWord;                              {Cursor Y (Characters)(CONSOLE_MODE_CHARACTER only)}
  CursorWidth:LongWord;                          {Cursor Width (Characters)(CONSOLE_MODE_CHARACTER only)}
  CursorHeight:LongWord;                         {Cursor Height (Characters)(CONSOLE_MODE_CHARACTER only)}
  CursorVisible:LongBool;                        {Cursor Visible On/Off (CONSOLE_MODE_CHARACTER only)}
  CursorChars:PChar;                             {Buffer for cursor characters (CONSOLE_MODE_CHARACTER only)}
  CursorBuffer:PChar;                            {Buffer for characters currently under cursor (CONSOLE_MODE_CHARACTER only)}
  {Caret Properties}
  CaretFirst:PConsoleCaret;
  CaretLock:TCriticalSectionHandle;
  CaretCount:LongWord;
  {Window Properties}
  WindowFirst:PConsoleWindow;
  WindowLock:TCriticalSectionHandle;
  WindowCount:LongWord;
  WindowActive:PConsoleWindow;                   {The active console Window (ie the Window that is shown as selected)}
  WindowDefault:TWindowHandle;                   {The default console Window (ie the Window that receives standard output)(WINDOW_MODE_TEXT only)}
  {Internal Properties}
  Prev:PConsoleDevice;                           {Previous entry in Console device table}
  Next:PConsoleDevice;                           {Next entry in Console device table}
 end;

 {Console Caret}
 TConsoleCaret = record
  {Caret Properties}
  Signature:LongWord;                            {Signature for entry validation}
  X:LongWord;                                    {Caret X (Pixels for CONSOLE_MODE_PIXEL / Characters for CONSOLE_MODE_CHARACTER)}
  Y:LongWord;                                    {Caret Y (Pixels for CONSOLE_MODE_PIXEL / Characters for CONSOLE_MODE_CHARACTER)}
  Width:LongWord;                                {Caret Width (Pixels for CONSOLE_MODE_PIXEL / Always 1 for CONSOLE_MODE_CHARACTER)}
  Height:LongWord;                               {Caret Height (Pixels for CONSOLE_MODE_PIXEL / Always 1 for CONSOLE_MODE_CHARACTER)}
  OffsetX:LongWord;                              {Caret Offset X (Pixels for CONSOLE_MODE_PIXEL / Always 0 for CONSOLE_MODE_CHARACTER)}
  OffsetY:LongWord;                              {Caret Offset Y (Pixels for CONSOLE_MODE_PIXEL / Always 0 for CONSOLE_MODE_CHARACTER)}
  Visible:LongBool;                              {Caret Visible On/Off}
  Blink:LongBool;                                {Caret Blink On/Off}
  Reverse:LongBool;                              {Caret Color Reverse or Inverse}
  Forecolor:LongWord;                            {Caret Foreground Color}
  Backcolor:LongWord;                            {Caret Background Color}
  Console:PConsoleDevice;                        {Console device}
  {Driver Properties}
  Handle:THandle;                                {Device specific handle}
  Active:LongBool;                               {Caret currently active (displayed) (Independent of Visible to account for Blink)}
  Image:Pointer;                                 {Device specific buffer for caret image}
  Buffer:Pointer;                                {Device specific buffer for area underneath caret}
  Output:Pointer;                                {Device specific buffer for curently displayed caret}
  {Internal Properties}
  Prev:PConsoleCaret;                            {Previous entry in Console Caret table}
  Next:PConsoleCaret;                            {Next entry in Console Caret table}
 end;

 PWindowProperties = ^TWindowProperties;
 TWindowProperties = record
  Position:LongWord;                             {Console Window Position (eg CONSOLE_POSITION_TOP)}
  State:LongWord;                                {Console Window State (eg WINDOW_STATE_VISIBLE)}
  Mode:LongWord;                                 {Console Window Mode (eg WINDOW_MODE_TEXT)}
  Flags:LongWord;                                {Console Window Flags (eg WINDOW_FLAG_LINE_WRAP)}
  X1:LongWord;                                   {Window X1} {Console Relative (Pixels for CONSOLE_MODE_PIXEL / Characters for CONSOLE_MODE_CHARACTER)}
  Y1:LongWord;                                   {Window Y1} {Console Relative (Pixels for CONSOLE_MODE_PIXEL / Characters for CONSOLE_MODE_CHARACTER)}
  X2:LongWord;                                   {Window X2} {Console Relative (Pixels for CONSOLE_MODE_PIXEL / Characters for CONSOLE_MODE_CHARACTER)}
  Y2:LongWord;                                   {Window Y2} {Console Relative (Pixels for CONSOLE_MODE_PIXEL / Characters for CONSOLE_MODE_CHARACTER)}
  Width:LongWord;                                {Window Width in Columns (Characters for WINDOW_MODE_TEXT / Pixels for WINDOW_MODE_GRAPHICS)}
  Height:LongWord;                               {Window Height in Rows (Characters for WINDOW_MODE_TEXT / Pixels for WINDOW_MODE_GRAPHICS)}
  OffsetX:LongWord;                              {Window X Offset (Pixels for CONSOLE_MODE_PIXEL / Characters for CONSOLE_MODE_CHARACTER)}
  OffsetY:LongWord;                              {Window Y Offset (Pixels for CONSOLE_MODE_PIXEL / Characters for CONSOLE_MODE_CHARACTER)}
  FontWidth:LongWord;                            {Font Width (Pixels for CONSOLE_MODE_PIXEL / Always 1 for CONSOLE_MODE_CHARACTER)}
  FontHeight:LongWord;                           {Font Height (Pixels for CONSOLE_MODE_PIXEL / Always 1 for CONSOLE_MODE_CHARACTER)}
  Borderwidth:LongWord;                          {Current Border Width}
  Font:TFontHandle;                              {Window Font}
  Console:PConsoleDevice;                        {Window console}
 end;

 {Console Window Enumeration Callback}
 TConsoleWindowEnumerate = function(Console:PConsoleDevice;Handle:TWindowHandle;Data:Pointer):LongWord;{$IFDEF i386} stdcall;{$ENDIF}

 {Console Window Completion Callback}
 TConsoleWindowCompletion = function(Handle:TWindowHandle;var Buffer:String;Data:Pointer):LongWord;{$IFDEF i386} stdcall;{$ENDIF}

 {Console Window}
 TConsoleWindow = record
  {Window Properties}
  Signature:LongWord;                            {Signature for entry validation}
  Position:LongWord;                             {Console Window Position (eg CONSOLE_POSITION_TOP)}
  WindowState:LongWord;                          {Console Window State (eg WINDOW_STATE_VISIBLE)}
  WindowMode:LongWord;                           {Console Window Mode (eg WINDOW_MODE_TEXT)}
  WindowFlags:LongWord;                          {Console Window Flags (eg WINDOW_FLAG_LINE_WRAP)}
  X1:LongWord;                                   {Window X1} {Console Relative (Pixels for CONSOLE_MODE_PIXEL / Characters for CONSOLE_MODE_CHARACTER)}
  Y1:LongWord;                                   {Window Y1} {Console Relative (Pixels for CONSOLE_MODE_PIXEL / Characters for CONSOLE_MODE_CHARACTER)}
  X2:LongWord;                                   {Window X2} {Console Relative (Pixels for CONSOLE_MODE_PIXEL / Characters for CONSOLE_MODE_CHARACTER)}
  Y2:LongWord;                                   {Window Y2} {Console Relative (Pixels for CONSOLE_MODE_PIXEL / Characters for CONSOLE_MODE_CHARACTER)}
  Width:LongWord;                                {Window Width in Columns (Characters for WINDOW_MODE_TEXT / Pixels for WINDOW_MODE_GRAPHICS)}
  Height:LongWord;                               {Window Height in Rows (Characters for WINDOW_MODE_TEXT / Pixels for WINDOW_MODE_GRAPHICS)}
  OffsetX:LongWord;                              {Window X Offset (Pixels for CONSOLE_MODE_PIXEL / Characters for CONSOLE_MODE_CHARACTER)}
  OffsetY:LongWord;                              {Window Y Offset (Pixels for CONSOLE_MODE_PIXEL / Characters for CONSOLE_MODE_CHARACTER)}
  MinX:LongWord;                                 {Viewport X1} {Window Relative (Characters for WINDOW_MODE_TEXT / Pixels for WINDOW_MODE_GRAPHICS)}
  MinY:LongWord;                                 {Viewport Y1} {Window Relative (Characters for WINDOW_MODE_TEXT / Pixels for WINDOW_MODE_GRAPHICS)}
  MaxX:LongWord;                                 {Viewport X2} {Window Relative (Characters for WINDOW_MODE_TEXT / Pixels for WINDOW_MODE_GRAPHICS)}
  MaxY:LongWord;                                 {Viewport Y2} {Window Relative (Characters for WINDOW_MODE_TEXT / Pixels for WINDOW_MODE_GRAPHICS)}
  X:LongWord;                                    {Current X} {Window Relative (Characters for WINDOW_MODE_TEXT / Not used for WINDOW_MODE_GRAPHICS)}
  Y:LongWord;                                    {Current Y} {Window Relative (Characters for WINDOW_MODE_TEXT / Not used for WINDOW_MODE_GRAPHICS)}
  Cols:LongWord;                                 {Viewport Columns (Characters for WINDOW_MODE_TEXT / Pixels for WINDOW_MODE_GRAPHICS)}
  Rows:LongWord;                                 {Viewport Rows (Characters for WINDOW_MODE_TEXT / Pixels for WINDOW_MODE_GRAPHICS)}
  Format:LongWord;                               {Color Format (eg COLOR_FORMAT_ARGB32)(Only applicable if CONSOLE_MODE_PIXEL)}
  Forecolor:LongWord;                            {Current Foreground Color}
  Backcolor:LongWord;                            {Current Background Color}
  Borderwidth:LongWord;                          {Current Border Width}
  Bordercolor:LongWord;                          {Current Border Color}
  {Font Properties}
  Font:TFontHandle;                              {Window Font}
  FontWidth:LongWord;                            {Font Width (Pixels for CONSOLE_MODE_PIXEL / Always 1 for CONSOLE_MODE_CHARACTER)}
  FontHeight:LongWord;                           {Font Height (Pixels for CONSOLE_MODE_PIXEL / Always 1 for CONSOLE_MODE_CHARACTER)}
  {Cursor Properties}
  CursorX:LongWord;                              {Cursor X} {Window Relative (Characters for WINDOW_MODE_TEXT / Pixels for WINDOW_MODE_GRAPHICS)}
  CursorY:LongWord;                              {Cursor Y} {Window Relative (Characters for WINDOW_MODE_TEXT / Pixels for WINDOW_MODE_GRAPHICS)}
  CursorMode:TCursorMode;                        {Cursor Mode Insert/Overwrite}
  CursorBlink:LongBool;                          {Cursor Blink On/Off}
  CursorState:TCursorState;                      {Cursor State On/Off}
  CursorShape:TCursorShape;                      {Cursor Shape Line/Bar/Block}
  CursorReverse:LongBool;                        {Cursor Color Reverse or Inverse (WINDOW_MODE_TEXT only)}
  CursorForecolor:LongWord;                      {Cursor Foreground Color (WINDOW_MODE_TEXT only)}
  CursorBackcolor:LongWord;                      {Cursor Background Color (WINDOW_MODE_TEXT only)}
  {Caret Properties}
  CaretX:LongWord;                               {Caret X} {Console Relative (Pixels for CONSOLE_MODE_PIXEL / Characters for CONSOLE_MODE_CHARACTER)}
  CaretY:LongWord;                               {Caret Y} {Console Relative (Pixels for CONSOLE_MODE_PIXEL / Characters for CONSOLE_MODE_CHARACTER)}
  CaretHandle:THandle;                           {Caret (Cursor) Handle (or INVALID_HANDLE_VALUE)}
  {History Properties}
  HistoryFirst:PConsoleHistory;
  HistoryLast:PConsoleHistory;
  HistoryCurrent:PConsoleHistory;
  HistoryCount:LongWord;
  {Driver Properties}
  Lock:TMutexHandle;                             {Window lock}
  Console:PConsoleDevice;                        {Window console}
  {Internal Properties}
  Prev:PConsoleWindow;                           {Previous entry in Console Window table}
  Next:PConsoleWindow;                           {Next entry in Console Window table}
 end;

 TConsoleHistory = record
  {History Properties}
  Value:PChar;
  Length:Integer;
  {Internal Properties}
  Prev:PConsoleHistory;                          {Previous entry in History table}
  Next:PConsoleHistory;                          {Next entry in History table}
 end;

{==============================================================================}
type
 {Framebuffer Console specific types}
 PFramebufferConsole = ^TFramebufferConsole;
 TFramebufferConsole = record
  {Console Properties}
  Console:TConsoleDevice;
  {Framebuffer Properties}
  Framebuffer:PFramebufferDevice;
  DesktopX:LongWord;                             {Desktop X (Left)} {Console Relative (Pixels)}
  DesktopY:LongWord;                             {Desktop Y (Right)} {Console Relative (Pixels)}
  DesktopWidth:LongWord;                         {Desktop Width} {Console Relative (Pixels)}
  DesktopHeight:LongWord;                        {Desktop Height} {Console Relative (Pixels)}
  DesktopOffset:LongWord;                        {Desktop Offset}
  DesktopColor:LongWord;                         {Desktop Color}
  {Buffer Properties}
  LineBuffer:Pointer;                            {Buffer for device reads and writes (Size of one line)}
 end;

{==============================================================================}
{var}
 {Console specific variables}

{==============================================================================}
{Initialization Functions}
procedure ConsoleInit;

{==============================================================================}
{Console Functions}
function ConsoleDeviceOpen(Console:PConsoleDevice):LongWord;
function ConsoleDeviceClose(Console:PConsoleDevice):LongWord;

function ConsoleDeviceClear(Console:PConsoleDevice;Color:LongWord):LongWord;
function ConsoleDeviceScroll(Console:PConsoleDevice;X1,Y1,X2,Y2,Count,Direction:LongWord):LongWord;

function ConsoleDeviceDrawBox(Console:PConsoleDevice;X1,Y1,X2,Y2,Color,Width:LongWord):LongWord;
function ConsoleDeviceDrawLine(Console:PConsoleDevice;X1,Y1,X2,Y2,Color,Width:LongWord):LongWord;
function ConsoleDevicePlotLine(Console:PConsoleDevice;X1,Y1,X2,Y2,Color,Width:LongWord):LongWord;
function ConsoleDeviceDrawChar(Console:PConsoleDevice;Handle:TFontHandle;Ch:Char;X,Y,Forecolor,Backcolor:LongWord):LongWord;
function ConsoleDeviceDrawText(Console:PConsoleDevice;Handle:TFontHandle;const Text:String;X,Y,Forecolor,Backcolor,Len:LongWord):LongWord;
function ConsoleDeviceDrawPixel(Console:PConsoleDevice;X,Y,Color:LongWord):LongWord;
function ConsoleDeviceDrawBlock(Console:PConsoleDevice;X1,Y1,X2,Y2,Color:LongWord):LongWord;
function ConsoleDeviceDrawCircle(Console:PConsoleDevice;X,Y,Color,Width,Radius:LongWord):LongWord;
function ConsoleDeviceDrawImage(Console:PConsoleDevice;X,Y:LongWord;Buffer:Pointer;Width,Height,Format,Skip:LongWord):LongWord;
function ConsoleDeviceDrawWindow(Console:PConsoleDevice;Handle:TWindowHandle;Flags:LongWord):LongWord;

function ConsoleDeviceGetPixel(Console:PConsoleDevice;X,Y:LongWord;var Color:LongWord):LongWord;
function ConsoleDeviceGetImage(Console:PConsoleDevice;X,Y:LongWord;Buffer:Pointer;Width,Height,Format,Skip:LongWord):LongWord;

function ConsoleDevicePutText(Console:PConsoleDevice;Handle:TFontHandle;const Source,Dest:TConsolePoint;Buffer:PConsoleChar;Width,Height,Skip:LongWord):LongWord;

function ConsoleDeviceCopyImage(Console:PConsoleDevice;const Source,Dest:TConsolePoint;Width,Height:LongWord):LongWord;

function ConsoleDeviceAddCaret(Console:PConsoleDevice;Width,Height,OffsetX,OffsetY:LongWord):THandle;
function ConsoleDeviceDeleteCaret(Console:PConsoleDevice;Handle:THandle):LongWord;
function ConsoleDeviceUpdateCaret(Console:PConsoleDevice;Handle:THandle;X,Y:LongWord;Visible,Blink:Boolean):LongWord; inline;
function ConsoleDeviceUpdateCaretEx(Console:PConsoleDevice;Handle:THandle;X,Y,Forecolor,Backcolor:LongWord;Visible,Blink,Reverse:Boolean):LongWord;

function ConsoleDeviceSetCursor(Console:PConsoleDevice;Width,Height:LongWord;Chars:PChar):LongWord;
function ConsoleDeviceUpdateCursor(Console:PConsoleDevice;Enabled:Boolean;X,Y:LongInt;Relative:Boolean):LongWord;

function ConsoleDeviceGetPosition(Console:PConsoleDevice;Position:LongWord;var X1,Y1,X2,Y2:LongWord):LongWord;
function ConsoleDeviceGetProperties(Console:PConsoleDevice;Properties:PConsoleProperties):LongWord;

function ConsoleDeviceCheckFlag(Console:PConsoleDevice;Flag:LongWord):Boolean;
function ConsoleDeviceUpdateFlag(Console:PConsoleDevice;Flag:LongWord;Clear:Boolean):LongWord;

function ConsoleDeviceGetMode(Console:PConsoleDevice):LongWord;
function ConsoleDeviceGetState(Console:PConsoleDevice):LongWord;

function ConsoleDeviceCreate:PConsoleDevice;
function ConsoleDeviceCreateEx(Size:LongWord):PConsoleDevice;
function ConsoleDeviceDestroy(Console:PConsoleDevice):LongWord;

function ConsoleDeviceRegister(Console:PConsoleDevice):LongWord;
function ConsoleDeviceDeregister(Console:PConsoleDevice):LongWord;

function ConsoleDeviceFind(ConsoleId:LongWord):PConsoleDevice;
function ConsoleDeviceFindByDevice(Device:PDevice):PConsoleDevice;
function ConsoleDeviceFindByName(const Name:String):PConsoleDevice; inline;
function ConsoleDeviceFindByDescription(const Description:String):PConsoleDevice; inline;
function ConsoleDeviceEnumerate(Callback:TConsoleEnumerate;Data:Pointer):LongWord;

function ConsoleDeviceNotification(Console:PConsoleDevice;Callback:TConsoleNotification;Data:Pointer;Notification,Flags:LongWord):LongWord;

{==============================================================================}
{Text Console Functions}
function ConsoleWindowCreate(Console:PConsoleDevice;Position:LongWord;Default:Boolean):TWindowHandle;
function ConsoleWindowCreateEx(Console:PConsoleDevice;Font:TFontHandle;Size,State,Mode,Position:LongWord;Default:Boolean):TWindowHandle;
function ConsoleWindowDestroy(Handle:TWindowHandle):LongWord;

function ConsoleWindowShow(Handle:TWindowHandle):LongWord;
function ConsoleWindowHide(Handle:TWindowHandle):LongWord;

function ConsoleWindowActivate(Handle:TWindowHandle):LongWord;
function ConsoleWindowDeactivate(Handle:TWindowHandle):LongWord;

function ConsoleWindowNext(Console:PConsoleDevice;Visible:Boolean):TWindowHandle;
function ConsoleWindowPrevious(Console:PConsoleDevice;Visible:Boolean):TWindowHandle;

function ConsoleWindowAt(Console:PConsoleDevice;X,Y:LongWord;Visible:Boolean):TWindowHandle;
function ConsoleWindowFind(Console:PConsoleDevice;Position:LongWord):TWindowHandle;
function ConsoleWindowEnumerate(Console:PConsoleDevice;Callback:TConsoleWindowEnumerate;Data:Pointer):LongWord;

function ConsoleWindowCheckFlag(Handle:TWindowHandle;Flag:LongWord):Boolean;
function ConsoleWindowUpdateFlag(Handle:TWindowHandle;Flag:LongWord;Clear:Boolean):LongWord;

function ConsoleWindowGetMode(Handle:TWindowHandle):LongWord;
function ConsoleWindowGetState(Handle:TWindowHandle):LongWord;

function ConsoleWindowGetPosition(Handle:TWindowHandle):LongWord;
function ConsoleWindowSetPosition(Handle:TWindowHandle;Position:LongWord):LongWord;

function ConsoleWindowGetProperties(Handle:TWindowHandle;Properties:PWindowProperties):LongWord;

function ConsoleWindowGetMinX(Handle:TWindowHandle):LongWord;
function ConsoleWindowGetMinY(Handle:TWindowHandle):LongWord;
function ConsoleWindowGetMaxX(Handle:TWindowHandle):LongWord;
function ConsoleWindowGetMaxY(Handle:TWindowHandle):LongWord;

function ConsoleWindowGetRect(Handle:TWindowHandle):TConsoleRect; inline;
function ConsoleWindowSetRect(Handle:TWindowHandle;const ARect:TConsoleRect):LongWord; inline;
function ConsoleWindowResetRect(Handle:TWindowHandle):LongWord; inline;

function ConsoleWindowGetViewport(Handle:TWindowHandle;var X1,Y1,X2,Y2:LongWord):LongWord;
function ConsoleWindowSetViewport(Handle:TWindowHandle;X1,Y1,X2,Y2:LongWord):LongWord;
function ConsoleWindowResetViewport(Handle:TWindowHandle):LongWord;

function ConsoleWindowGetX(Handle:TWindowHandle):LongWord;
function ConsoleWindowSetX(Handle:TWindowHandle;X:LongWord):LongWord;
function ConsoleWindowGetY(Handle:TWindowHandle):LongWord;
function ConsoleWindowSetY(Handle:TWindowHandle;Y:LongWord):LongWord;

function ConsoleWindowGetXY(Handle:TWindowHandle;var X,Y:LongWord):LongWord;
function ConsoleWindowSetXY(Handle:TWindowHandle;X,Y:LongWord):LongWord;

function ConsoleWindowGetPoint(Handle:TWindowHandle):TConsolePoint;
function ConsoleWindowSetPoint(Handle:TWindowHandle;const APoint:TConsolePoint):LongWord;

function ConsoleWindowGetCols(Handle:TWindowHandle):LongWord;
function ConsoleWindowGetRows(Handle:TWindowHandle):LongWord;

function ConsoleWindowGetWidth(Handle:TWindowHandle):LongWord;
function ConsoleWindowGetHeight(Handle:TWindowHandle):LongWord;
function ConsoleWindowGetFormat(Handle:TWindowHandle):LongWord;

function ConsoleWindowGetForecolor(Handle:TWindowHandle):LongWord;
function ConsoleWindowSetForecolor(Handle:TWindowHandle;Color:LongWord):LongWord;
function ConsoleWindowGetBackcolor(Handle:TWindowHandle):LongWord;
function ConsoleWindowSetBackcolor(Handle:TWindowHandle;Color:LongWord):LongWord;

function ConsoleWindowGetFont(Handle:TWindowHandle):TFontHandle;
function ConsoleWindowSetFont(Handle:TWindowHandle;Font:TFontHandle):LongWord;

function ConsoleWindowGetCursorXY(Handle:TWindowHandle;var X,Y:LongWord):LongWord;
function ConsoleWindowSetCursorXY(Handle:TWindowHandle;X,Y:LongWord):LongWord;
function ConsoleWindowGetCursorMode(Handle:TWindowHandle):TCursorMode;
function ConsoleWindowSetCursorMode(Handle:TWindowHandle;CursorMode:TCursorMode):LongWord;
function ConsoleWindowGetCursorBlink(Handle:TWindowHandle):Boolean;
function ConsoleWindowSetCursorBlink(Handle:TWindowHandle;CursorBlink:Boolean):LongWord;
function ConsoleWindowGetCursorState(Handle:TWindowHandle):TCursorState;
function ConsoleWindowSetCursorState(Handle:TWindowHandle;CursorState:TCursorState):LongWord;
function ConsoleWindowGetCursorShape(Handle:TWindowHandle):TCursorShape;
function ConsoleWindowSetCursorShape(Handle:TWindowHandle;CursorShape:TCursorShape):LongWord;
function ConsoleWindowGetCursorColor(Handle:TWindowHandle):LongWord;
function ConsoleWindowSetCursorColor(Handle:TWindowHandle;Color:LongWord):LongWord;
function ConsoleWindowGetCursorReverse(Handle:TWindowHandle):Boolean;
function ConsoleWindowSetCursorReverse(Handle:TWindowHandle;CursorReverse:Boolean):LongWord;

function ConsoleWindowCursorOn(Handle:TWindowHandle):LongWord;
function ConsoleWindowCursorOff(Handle:TWindowHandle):LongWord;
function ConsoleWindowCursorLine(Handle:TWindowHandle):LongWord;
function ConsoleWindowCursorBar(Handle:TWindowHandle):LongWord;
function ConsoleWindowCursorBlock(Handle:TWindowHandle):LongWord;
function ConsoleWindowCursorMove(Handle:TWindowHandle;X,Y:LongWord):LongWord;
function ConsoleWindowCursorBlink(Handle:TWindowHandle;Enabled:Boolean):LongWord;
function ConsoleWindowCursorColor(Handle:TWindowHandle;Color:LongWord):LongWord;
function ConsoleWindowCursorReverse(Handle:TWindowHandle;Enabled:Boolean):LongWord;

function ConsoleWindowAddHistory(Handle:TWindowHandle;const Value:String):LongWord;
function ConsoleWindowClearHistory(Handle:TWindowHandle):LongWord;
function ConsoleWindowFirstHistory(Handle:TWindowHandle):String;
function ConsoleWindowLastHistory(Handle:TWindowHandle):String;
function ConsoleWindowNextHistory(Handle:TWindowHandle):String;
function ConsoleWindowPreviousHistory(Handle:TWindowHandle):String;
function ConsoleWindowCurrentHistory(Handle:TWindowHandle):String;

function ConsoleWindowScrollUp(Handle:TWindowHandle;Row,Lines:LongWord):LongWord;
function ConsoleWindowScrollDown(Handle:TWindowHandle;Row,Lines:LongWord):LongWord;

function ConsoleWindowScrollLeft(Handle:TWindowHandle;Row,Col,Lines,Chars:LongWord):LongWord;
function ConsoleWindowScrollRight(Handle:TWindowHandle;Row,Col,Lines,Chars:LongWord):LongWord;

function ConsoleWindowClear(Handle:TWindowHandle):LongWord;
function ConsoleWindowClearEx(Handle:TWindowHandle;X1,Y1,X2,Y2:LongWord;Cursor:Boolean):LongWord;

function ConsoleWindowWrite(Handle:TWindowHandle;const AText:String):LongWord;
function ConsoleWindowWriteEx(Handle:TWindowHandle;const AText:String;X,Y,Forecolor,Backcolor:LongWord):LongWord;

function ConsoleWindowWriteLn(Handle:TWindowHandle;const AText:String):LongWord;
function ConsoleWindowWriteLnEx(Handle:TWindowHandle;const AText:String;X,Y,Forecolor,Backcolor:LongWord):LongWord;

function ConsoleWindowWriteChr(Handle:TWindowHandle;AChr:Char):LongWord;
function ConsoleWindowWriteChrEx(Handle:TWindowHandle;AChr:Char;X,Y,Forecolor,Backcolor:LongWord):LongWord;

function ConsoleWindowOutput(Handle:TWindowHandle;const Source,Dest:TConsolePoint;Buffer:PConsoleChar;Width,Height,Skip:LongWord):LongWord;

function ConsoleWindowRead(Handle:TWindowHandle;var AText:String):LongWord;

function ConsoleWindowReadLn(Handle:TWindowHandle;var AText:String):LongWord;
function ConsoleWindowReadLnEx(Handle:TWindowHandle;var AText:String;const Prompt:String;X,Y,Forecolor,Backcolor:LongWord;Scroll,History:Boolean;Completion:TConsoleWindowCompletion;Data:Pointer):LongWord;

function ConsoleWindowReadChr(Handle:TWindowHandle;var AChr:Char):LongWord;
function ConsoleWindowReadChrEx(Handle:TWindowHandle;var AChr:Char;const Prompt:String;X,Y,Forecolor,Backcolor:LongWord;Echo,Scroll:Boolean):LongWord;

{==============================================================================}
{CRT Console Functions}
procedure ConsoleAssignCrt(var F:Text);

procedure ConsoleClrEol;
procedure ConsoleClrScr;
procedure ConsoleDelay(MS:Word);
procedure ConsoleDelLine;
procedure ConsoleGotoXY(X,Y:Integer);
procedure ConsoleHighVideo;
procedure ConsoleInsLine;
function ConsoleKeypressed:Boolean;
procedure ConsoleLowVideo;
procedure ConsoleNormVideo;
procedure ConsoleNoSound;
function ConsoleReadKey:Char;
procedure ConsoleSound(Hz:Word);
procedure ConsoleTextBackground(Color:LongWord);
procedure ConsoleTextColor(Color:LongWord);
procedure ConsoleTextMode(Mode:Integer);
function ConsoleWhereX:Integer;
function ConsoleWhereY:Integer;
procedure ConsoleWindow(X1,Y1,X2,Y2:Integer);
procedure ConsoleScrollUp(Row,Lines:Integer);
procedure ConsoleScrollDown(Row,Lines:Integer);

procedure ConsoleWrite(const AText:String);
procedure ConsoleWriteLn(const AText:String);
procedure ConsoleWriteChr(AChr:Char);

procedure ConsoleRead(var AText:String);
procedure ConsoleReadLn(var AText:String);
procedure ConsoleReadChr(var AChr:Char);

{==============================================================================}
{RTL Text IO Functions}
function SysTextIOWriteChar(ACh:Char;AUserData:Pointer):Boolean;

{==============================================================================}
{RTL Console Functions}
function SysConsoleWriteChar(ACh:Char;AUserData:Pointer):Boolean;

{==============================================================================}
{Framebuffer Console Functions}
function FramebufferConsoleOpen(Console:PConsoleDevice):LongWord;
function FramebufferConsoleClose(Console:PConsoleDevice):LongWord;

function FramebufferConsoleClear(Console:PConsoleDevice;Color:LongWord):LongWord;
function FramebufferConsoleScroll(Console:PConsoleDevice;X1,Y1,X2,Y2,Count,Direction:LongWord):LongWord;

function FramebufferConsoleDrawBox(Console:PConsoleDevice;X1,Y1,X2,Y2,Color,Width:LongWord):LongWord;
function FramebufferConsoleDrawLine(Console:PConsoleDevice;X1,Y1,X2,Y2,Color,Width:LongWord):LongWord;
function FramebufferConsoleDrawChar(Console:PConsoleDevice;Handle:TFontHandle;Ch:Char;X,Y,Forecolor,Backcolor:LongWord):LongWord;
function FramebufferConsoleDrawText(Console:PConsoleDevice;Handle:TFontHandle;const Text:String;X,Y,Forecolor,Backcolor,Len:LongWord):LongWord;
function FramebufferConsoleDrawPixel(Console:PConsoleDevice;X,Y,Color:LongWord):LongWord;
function FramebufferConsoleDrawBlock(Console:PConsoleDevice;X1,Y1,X2,Y2,Color:LongWord):LongWord;
function FramebufferConsoleDrawImage(Console:PConsoleDevice;X,Y:LongWord;Buffer:Pointer;Width,Height,Format,Skip:LongWord):LongWord;
function FramebufferConsoleDrawWindow(Console:PConsoleDevice;Handle:TWindowHandle;Flags:LongWord):LongWord;
function FramebufferConsoleDrawDesktop(Console:PConsoleDevice):LongWord;

function FramebufferConsoleGetPixel(Console:PConsoleDevice;X,Y:LongWord;var Color:LongWord):LongWord;
function FramebufferConsoleGetImage(Console:PConsoleDevice;X,Y:LongWord;Buffer:Pointer;Width,Height,Format,Skip:LongWord):LongWord;

function FramebufferConsolePutText(Console:PConsoleDevice;Handle:TFontHandle;const Source,Dest:TConsolePoint;Buffer:PConsoleChar;Width,Height,Skip:LongWord):LongWord;

function FramebufferConsoleCopyImage(Console:PConsoleDevice;const Source,Dest:TConsolePoint;Width,Height:LongWord):LongWord;

function FramebufferConsoleAddCaret(Console:PConsoleDevice;Width,Height,OffsetX,OffsetY:LongWord):THandle;
function FramebufferConsoleDeleteCaret(Console:PConsoleDevice;Handle:THandle):LongWord;
function FramebufferConsoleUpdateCaretEx(Console:PConsoleDevice;Handle:THandle;X,Y,Forecolor,Backcolor:LongWord;Visible,Blink,Reverse:Boolean):LongWord;

function FramebufferConsoleGetPosition(Console:PConsoleDevice;Position:LongWord;var X1,Y1,X2,Y2:LongWord):LongWord;

{==============================================================================}
{Console Helper Functions}
function ConsoleDeviceGetCount:LongWord;
function ConsoleDeviceGetDefault:PConsoleDevice;
function ConsoleDeviceSetDefault(Console:PConsoleDevice):LongWord;

function ConsoleDeviceCheck(Console:PConsoleDevice):PConsoleDevice;
function ConsoleDeviceCaretCheck(Console:PConsoleDevice;Caret:PConsoleCaret):PConsoleCaret;

function ConsoleTypeToString(ConsoleType:LongWord):String;
function ConsoleStateToString(ConsoleState:LongWord):String;

function ConsoleDeviceGetDefaultFont:TFontHandle;

function ConsolePositionToString(Position:LongWord):String;

function ConsoleFramebufferDeviceAdd(Framebuffer:PFramebufferDevice):LongWord;
function ConsoleFramebufferDeviceRemove(Framebuffer:PFramebufferDevice):LongWord;

function ConsoleFramebufferDeviceEnum(Framebuffer:PFramebufferDevice;Data:Pointer):LongWord;
function ConsoleFramebufferDeviceNotify(Device:PDevice;Data:Pointer;Notification:LongWord):LongWord;

{==============================================================================}
{Text Console Helper Functions}
function ConsoleWindowGetCount(Console:PConsoleDevice):LongWord; inline;
function ConsoleWindowGetActive(Console:PConsoleDevice):TWindowHandle; inline;
function ConsoleWindowGetDefault(Console:PConsoleDevice):TWindowHandle; inline;
function ConsoleWindowSetDefault(Console:PConsoleDevice;Handle:TWindowHandle):LongWord;

function ConsoleWindowCheck(Console:PConsoleDevice;Window:PConsoleWindow):PConsoleWindow;

function ConsoleWindowStateToString(WindowState:LongWord):String;
function ConsoleWindowModeToString(WindowMode:LongWord):String;

function ConsoleWindowGetDefaultFont:TFontHandle;

function ConsoleWindowRedirectOutput(Handle:TWindowHandle):Boolean;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {Console specific variables}
 ConsoleInitialized:Boolean;

 ConsoleDeviceTable:PConsoleDevice;
 ConsoleDeviceTableLock:TCriticalSectionHandle = INVALID_HANDLE_VALUE;
 ConsoleDeviceTableCount:LongWord;

 ConsoleDeviceDefault:PConsoleDevice;

 ConsoleTextIOOutputHandle:TWindowHandle = INVALID_HANDLE_VALUE;

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure ConsoleInit;
{Initialize the Console unit and Console device table}

{Note: Called only during system startup}
var
 WorkInt:LongWord;
 WorkBuffer:String;
begin
 {}
 {Check Initialized}
 if ConsoleInitialized then Exit;

 {$IFDEF CONSOLE_EARLY_INIT}
 {Initialize Device Support}
 DevicesInit;

 {Initialize Framebuffer Support}
 FramebufferInit;

 {Initialize Font Support}
 FontInit;
 {$ENDIF}

 {Initialize Console Device Table}
 ConsoleDeviceTable:=nil;
 ConsoleDeviceTableLock:=CriticalSectionCreate;
 ConsoleDeviceTableCount:=0;
 if ConsoleDeviceTableLock = INVALID_HANDLE_VALUE then
  begin
   if DEVICE_LOG_ENABLED then DeviceLogError(nil,'Failed to create console device table lock');
  end;
 ConsoleDeviceDefault:=nil;

 {Check Environment Variables}
 {CONSOLE_DEFAULT_FORECOLOR}
 WorkInt:=StrToIntDef(EnvironmentGet('CONSOLE_DEFAULT_FORECOLOR'),CONSOLE_DEFAULT_FORECOLOR);
 if WorkInt <> CONSOLE_DEFAULT_FORECOLOR then CONSOLE_DEFAULT_FORECOLOR:=WorkInt;

 {CONSOLE_DEFAULT_BACKCOLOR}
 WorkInt:=StrToIntDef(EnvironmentGet('CONSOLE_DEFAULT_BACKCOLOR'),CONSOLE_DEFAULT_BACKCOLOR);
 if WorkInt <> CONSOLE_DEFAULT_BACKCOLOR then CONSOLE_DEFAULT_BACKCOLOR:=WorkInt;

 {CONSOLE_DEFAULT_BORDERWIDTH}
 WorkInt:=StrToIntDef(EnvironmentGet('CONSOLE_DEFAULT_BORDERWIDTH'),CONSOLE_DEFAULT_BORDERWIDTH);
 if WorkInt <> CONSOLE_DEFAULT_BORDERWIDTH then CONSOLE_DEFAULT_BORDERWIDTH:=WorkInt;

 {CONSOLE_DEFAULT_BORDERCOLOR}
 WorkInt:=StrToIntDef(EnvironmentGet('CONSOLE_DEFAULT_BORDERCOLOR'),CONSOLE_DEFAULT_BORDERCOLOR);
 if WorkInt <> CONSOLE_DEFAULT_BORDERCOLOR then CONSOLE_DEFAULT_BORDERCOLOR:=WorkInt;

 {CONSOLE_DEFAULT_FONT_NAME}
 WorkBuffer:=EnvironmentGet('CONSOLE_DEFAULT_FONT_NAME');
 if Length(WorkBuffer) > 0 then CONSOLE_DEFAULT_FONT_NAME:=WorkBuffer;

 {WINDOW_DEFAULT_FORECOLOR}
 WorkInt:=StrToIntDef(EnvironmentGet('WINDOW_DEFAULT_FORECOLOR'),WINDOW_DEFAULT_FORECOLOR);
 if WorkInt <> WINDOW_DEFAULT_FORECOLOR then WINDOW_DEFAULT_FORECOLOR:=WorkInt;

 {WINDOW_DEFAULT_BACKCOLOR}
 WorkInt:=StrToIntDef(EnvironmentGet('WINDOW_DEFAULT_BACKCOLOR'),WINDOW_DEFAULT_BACKCOLOR);
 if WorkInt <> WINDOW_DEFAULT_BACKCOLOR then WINDOW_DEFAULT_BACKCOLOR:=WorkInt;

 {WINDOW_DEFAULT_BORDERWIDTH}
 WorkInt:=StrToIntDef(EnvironmentGet('WINDOW_DEFAULT_BORDERWIDTH'),WINDOW_DEFAULT_BORDERWIDTH);
 if WorkInt <> WINDOW_DEFAULT_BORDERWIDTH then WINDOW_DEFAULT_BORDERWIDTH:=WorkInt;

 {WINDOW_DEFAULT_BORDERCOLOR}
 WorkInt:=StrToIntDef(EnvironmentGet('WINDOW_DEFAULT_BORDERCOLOR'),WINDOW_DEFAULT_BORDERCOLOR);
 if WorkInt <> WINDOW_DEFAULT_BORDERCOLOR then WINDOW_DEFAULT_BORDERCOLOR:=WorkInt;

 {WINDOW_DEFAULT_ACTIVEBORDER}
 WorkInt:=StrToIntDef(EnvironmentGet('WINDOW_DEFAULT_ACTIVEBORDER'),WINDOW_DEFAULT_ACTIVEBORDER);
 if WorkInt <> WINDOW_DEFAULT_ACTIVEBORDER then WINDOW_DEFAULT_ACTIVEBORDER:=WorkInt;

 {WINDOW_DEFAULT_FONT_NAME}
 WorkBuffer:=EnvironmentGet('WINDOW_DEFAULT_FONT_NAME');
 if Length(WorkBuffer) > 0 then WINDOW_DEFAULT_FONT_NAME:=WorkBuffer;

 {FRAMEBUFFER_CONSOLE_DEFAULT_DESKTOPOFFSET}
 WorkInt:=StrToIntDef(EnvironmentGet('FRAMEBUFFER_CONSOLE_DEFAULT_DESKTOPOFFSET'),FRAMEBUFFER_CONSOLE_DEFAULT_DESKTOPOFFSET);
 if WorkInt <> FRAMEBUFFER_CONSOLE_DEFAULT_DESKTOPOFFSET then FRAMEBUFFER_CONSOLE_DEFAULT_DESKTOPOFFSET:=WorkInt;

 {FRAMEBUFFER_CONSOLE_DEFAULT_DESKTOPCOLOR}
 WorkInt:=StrToIntDef(EnvironmentGet('FRAMEBUFFER_CONSOLE_DEFAULT_DESKTOPCOLOR'),FRAMEBUFFER_CONSOLE_DEFAULT_DESKTOPCOLOR);
 if WorkInt <> FRAMEBUFFER_CONSOLE_DEFAULT_DESKTOPCOLOR then FRAMEBUFFER_CONSOLE_DEFAULT_DESKTOPCOLOR:=WorkInt;

 {Setup Console Default Font}
 CONSOLE_DEFAULT_FONT:=ConsoleDeviceGetDefaultFont;

 {Setup Window Default Font}
 WINDOW_DEFAULT_FONT:=ConsoleWindowGetDefaultFont;

 {Enumerate Framebuffers}
 FramebufferDeviceEnumerate(ConsoleFramebufferDeviceEnum,nil);

 {Register Notification}
 FramebufferDeviceNotification(nil,ConsoleFramebufferDeviceNotify,nil,DEVICE_NOTIFICATION_REGISTER or DEVICE_NOTIFICATION_DEREGISTER or DEVICE_NOTIFICATION_ENABLE or DEVICE_NOTIFICATION_DISABLE,NOTIFIER_FLAG_UNLOCK);

 {Setup Platform Text IO Handlers}
 {TextIOWriteCharHandler:=SysTextIOWriteChar;} {Only registered when calling ConsoleWindowRedirectOutput}

 {Setup Platform Console Handlers}
 ConsoleWriteCharHandler:=SysConsoleWriteChar;

 ConsoleInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{Console Functions}
function ConsoleDeviceOpen(Console:PConsoleDevice):LongWord;
{Open a console device ready for drawing}
{Console: The console device to open}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console Device Open');
 {$ENDIF}

 {Check Closed}
 Result:=ERROR_SUCCESS;
 if Console.ConsoleState <> CONSOLE_STATE_CLOSED then Exit;

 {Check Open}
 Result:=ERROR_INVALID_PARAMETER;
 if Assigned(Console.DeviceOpen) then
  begin
   {Call Device Open}
   Result:=Console.DeviceOpen(Console);
   if Result <> ERROR_SUCCESS then Exit;
  end;

 {Open Device}
 Console.ConsoleState:=CONSOLE_STATE_OPEN;

 {Notify Open}
 NotifierNotify(@Console.Device,DEVICE_NOTIFICATION_OPEN);

 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function ConsoleDeviceClose(Console:PConsoleDevice):LongWord;
{Close a console device to prevent drawing}
{Console: The console device to close}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console Device Close');
 {$ENDIF}

 {Check Open}
 Result:=ERROR_SUCCESS;
 if Console.ConsoleState <> CONSOLE_STATE_OPEN then Exit;

 {Check Close}
 if Assigned(Console.DeviceClose) then
  begin
   {Call Device Close}
   Result:=Console.DeviceClose(Console);
   if Result <> ERROR_SUCCESS then Exit;
  end;

 {Close Device}
 Console.ConsoleState:=CONSOLE_STATE_CLOSED;

 {Notify Close}
 NotifierNotify(@Console.Device,DEVICE_NOTIFICATION_CLOSE);

 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function ConsoleDeviceClear(Console:PConsoleDevice;Color:LongWord):LongWord;
{Clear a console device using the specified color}
{Console: The console device to clear}
{Color: The color to use when clearing the console}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: Color must be specified in the default color format (See COLOR_FORMAT_DEFAULT)}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console Device Clear');
 {$ENDIF}

 {Check Open}
 Result:=ERROR_NOT_SUPPORTED;
 if Console.ConsoleState <> CONSOLE_STATE_OPEN then Exit;

 if Assigned(Console.DeviceClear) then
  begin
   Result:=Console.DeviceClear(Console,Color);
  end;
end;

{==============================================================================}

function ConsoleDeviceScroll(Console:PConsoleDevice;X1,Y1,X2,Y2,Count,Direction:LongWord):LongWord;
{Scroll all or part of a console device in the specified direction}
{Console: The console device to scroll}
{X1: The left edge of the area to scroll (Pixels for CONSOLE_MODE_PIXEL / Characters for CONSOLE_MODE_CHARACTER)}
{Y1: The top edge of the area to scroll (Pixels for CONSOLE_MODE_PIXEL / Characters for CONSOLE_MODE_CHARACTER)}
{X2: The right edge of the area to scroll (Pixels for CONSOLE_MODE_PIXEL / Characters for CONSOLE_MODE_CHARACTER)}
{Y2: The bottom edge of the area to scroll (Pixels for CONSOLE_MODE_PIXEL / Characters for CONSOLE_MODE_CHARACTER)}
{Count: The number of pixels or characters (depending on console mode) to scroll}
{Direction: The direction to scroll (eg CONSOLE_DIRECTION_UP)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console Device Scroll');
 {$ENDIF}

 {Check Open}
 Result:=ERROR_NOT_SUPPORTED;
 if Console.ConsoleState <> CONSOLE_STATE_OPEN then Exit;

 if Assigned(Console.DeviceScroll) then
  begin
   Result:=Console.DeviceScroll(Console,X1,Y1,X2,Y2,Count,Direction);
  end;
end;

{==============================================================================}

function ConsoleDeviceDrawBox(Console:PConsoleDevice;X1,Y1,X2,Y2,Color,Width:LongWord):LongWord;
{Draw an outline of a box on a console device}
{Console: The console device to draw on}
{X1: The left edge of the box (Pixels for CONSOLE_MODE_PIXEL / Characters for CONSOLE_MODE_CHARACTER)}
{Y1: The top edge of the box (Pixels for CONSOLE_MODE_PIXEL / Characters for CONSOLE_MODE_CHARACTER)}
{X2: The right edge of the box (Pixels for CONSOLE_MODE_PIXEL / Characters for CONSOLE_MODE_CHARACTER)}
{Y2: The bottom edge of the box (Pixels for CONSOLE_MODE_PIXEL / Characters for CONSOLE_MODE_CHARACTER)}
{Color: The color to draw with (eg COLOR_WHITE)}
{Width: The width of the box outline (Pixels for CONSOLE_MODE_PIXEL / Characters for CONSOLE_MODE_CHARACTER)}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: Color must be specified in the default color format (See COLOR_FORMAT_DEFAULT)}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console Device Draw Box');
 {$ENDIF}

 {Check Open}
 Result:=ERROR_NOT_SUPPORTED;
 if Console.ConsoleState <> CONSOLE_STATE_OPEN then Exit;

 if Assigned(Console.DeviceDrawBox) then
  begin
   Result:=Console.DeviceDrawBox(Console,X1,Y1,X2,Y2,Color,Width);
  end
 else
  begin
   Result:=ERROR_CALL_NOT_IMPLEMENTED;
  end;
end;

{==============================================================================}

function ConsoleDeviceDrawLine(Console:PConsoleDevice;X1,Y1,X2,Y2,Color,Width:LongWord):LongWord;
{Draw a horizontal or vertical line on a console device}
{Console: The console device to draw on}
{X1: The left starting point of the line (Pixels for CONSOLE_MODE_PIXEL / Characters for CONSOLE_MODE_CHARACTER)}
{Y1: The top starting point of the line (Pixels for CONSOLE_MODE_PIXEL / Characters for CONSOLE_MODE_CHARACTER)}
{X2: The right ending point of the line (Pixels for CONSOLE_MODE_PIXEL / Characters for CONSOLE_MODE_CHARACTER)}
{Y2: The bottom ending point of the line (Pixels for CONSOLE_MODE_PIXEL / Characters for CONSOLE_MODE_CHARACTER)}
{Color: The color to draw with (eg COLOR_WHITE)}
{Width: The width of the line (Pixels for CONSOLE_MODE_PIXEL / Characters for CONSOLE_MODE_CHARACTER)}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: Color must be specified in the default color format (See COLOR_FORMAT_DEFAULT)}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console Device Draw Line');
 {$ENDIF}

 {Check Open}
 Result:=ERROR_NOT_SUPPORTED;
 if Console.ConsoleState <> CONSOLE_STATE_OPEN then Exit;

 if Assigned(Console.DeviceDrawLine) then
  begin
   Result:=Console.DeviceDrawLine(Console,X1,Y1,X2,Y2,Color,Width);
  end
 else
  begin
   Result:=ERROR_CALL_NOT_IMPLEMENTED;
  end;
end;

{==============================================================================}

function ConsoleDevicePlotLine(Console:PConsoleDevice;X1,Y1,X2,Y2,Color,Width:LongWord):LongWord;
{Draw a line in any direction on a console device}
{Console: The console device to draw on}
{X1: The left starting point of the line (Pixels for CONSOLE_MODE_PIXEL / Characters for CONSOLE_MODE_CHARACTER)}
{Y1: The top starting point of the line (Pixels for CONSOLE_MODE_PIXEL / Characters for CONSOLE_MODE_CHARACTER)}
{X2: The right ending point of the line (Pixels for CONSOLE_MODE_PIXEL / Characters for CONSOLE_MODE_CHARACTER)}
{Y2: The bottom ending point of the line (Pixels for CONSOLE_MODE_PIXEL / Characters for CONSOLE_MODE_CHARACTER)}
{Color: The color to draw with (eg COLOR_WHITE)}
{Width: The width of the line (Pixels for CONSOLE_MODE_PIXEL / Characters for CONSOLE_MODE_CHARACTER)}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: Color must be specified in the default color format (See COLOR_FORMAT_DEFAULT)}
{Note: Character consoles may implement plot line but the default method only supports pixel consoles}
var
 X,Y:LongInt;
 A,B,E:LongInt;
 DeltaX,DeltaY:LongInt;
 OffsetX,OffsetY:LongInt;

 Temp:LongInt;
 Count:Integer;
 LineEnd:LongInt;
 Bisecting:Boolean;
 WidthStart,WidthEnd:LongInt;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console Device Plot Line');
 {$ENDIF}

 {Check Open}
 Result:=ERROR_NOT_SUPPORTED;
 if Console.ConsoleState <> CONSOLE_STATE_OPEN then Exit;

 if Assigned(Console.DevicePlotLine) then
  begin
   Result:=Console.DevicePlotLine(Console,X1,Y1,X2,Y2,Color,Width);
  end
 else
  begin
   {Default Method}
   {Check Mode}
   if Console.ConsoleMode <> CONSOLE_MODE_PIXEL then Exit;

   {Check Color}
   {if Color = COLOR_NONE then Exit;}

   {Check Width}
   if Width < 1 then Exit;

   if MutexLock(Console.Lock) = ERROR_SUCCESS then
    begin
     try
      {Setup Defaults}
      X:=X1;
      Y:=Y1;
      OffsetX:=1;
      OffsetY:=1;
      LineEnd:=X2;
      DeltaX:=X2 - X1;
      DeltaY:=Y2 - Y1;
      Bisecting:=False;

      {Flip on the Y-axis}
      if DeltaX < 0 then
       begin
        DeltaX:=-DeltaX;
        OffsetX:=-1;
       end;

      {Flip on the X-axis}
      if DeltaY < 0 then
       begin
        DeltaY:=-DeltaY;
        OffsetY:=-1;
       end;

      {Flip on the bisectrix of the 1 quadrant}
      if DeltaX < DeltaY then
       begin
        Temp:=X;
        X:=Y;
        Y:=Temp;

        Temp:=DeltaX;
        DeltaX:=DeltaY;
        DeltaY:=Temp;

        Temp:=OffsetX;
        OffsetX:=OffsetY;
        OffsetY:=Temp;

        LineEnd:=Y2;
        Bisecting:=True;
       end;

      {Precalculated values for the loop}
      A:=DeltaY shl 1;
      B:=DeltaX shl 1 - A;
      E:=A - DeltaX;

      {Calculate Width}
      WidthStart:=-(Width div 2);
      WidthEnd:=((Width div 2) - 1) + (Width mod 2);

      {Plot the Line}
      if Bisecting then
       begin
        if Width = 1 then
         begin
          ConsoleDeviceDrawPixel(Console,Y,X,Color);
         end
        else
         begin
          for Count:=WidthStart to WidthEnd do
           begin
            ConsoleDeviceDrawPixel(Console,Y + Count,X,Color);
           end;
         end;
       end
      else
       begin
        if Width = 1 then
         begin
          ConsoleDeviceDrawPixel(Console,X,Y,Color);
         end
        else
         begin
          for Count:=WidthStart to WidthEnd do
           begin
            ConsoleDeviceDrawPixel(Console,X + Count,Y,Color);
           end;
         end;
       end;

      while X <> LineEnd do
       begin
        X:=X + OffsetX;
        if E > 0 then
         begin
          Y:=Y + OffsetY;
          E:=E - B;
         end
        else
         begin
          E:=E + A;
         end;

        if Bisecting then
         begin
          if Width = 1 then
           begin
            ConsoleDeviceDrawPixel(Console,Y,X,Color);
           end
          else
           begin
            for Count:=WidthStart to WidthEnd do
             begin
              ConsoleDeviceDrawPixel(Console,Y + Count,X,Color);
             end;
           end;
         end
        else
         begin
          if Width = 1 then
           begin
            ConsoleDeviceDrawPixel(Console,X,Y,Color);
           end
          else
           begin
            for Count:=WidthStart to WidthEnd do
             begin
              ConsoleDeviceDrawPixel(Console,X + Count,Y,Color);
             end;
           end;
         end;
       end;

      {Get Result}
      Result:=ERROR_SUCCESS;
     finally
      MutexUnlock(Console.Lock);
     end;
    end
   else
    begin
     Result:=ERROR_CAN_NOT_COMPLETE;
    end;
  end;
end;

{==============================================================================}

function ConsoleDeviceDrawChar(Console:PConsoleDevice;Handle:TFontHandle;Ch:Char;X,Y,Forecolor,Backcolor:LongWord):LongWord;
{Draw a character on a console device}
{Console: The console device to draw on}
{Handle: The handle of the font to draw with}
{Ch: The character to draw}
{X: The left starting point of the character (Pixels for CONSOLE_MODE_PIXEL / Characters for CONSOLE_MODE_CHARACTER)}
{Y: The top starting point of the character (Pixels for CONSOLE_MODE_PIXEL / Characters for CONSOLE_MODE_CHARACTER)}
{Forecolor: The foreground color for the character (eg COLOR_WHITE)}
{Backcolor: The background color for the character (eg COLOR_BLACK)}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: Forecolor and Backcolor must be specified in the default color format (See COLOR_FORMAT_DEFAULT)}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console Device Draw Char');
 {$ENDIF}

 {Check Open}
 Result:=ERROR_NOT_SUPPORTED;
 if Console.ConsoleState <> CONSOLE_STATE_OPEN then Exit;

 if Assigned(Console.DeviceDrawChar) then
  begin
   Result:=Console.DeviceDrawChar(Console,Handle,Ch,X,Y,Forecolor,Backcolor);
  end;
end;

{==============================================================================}

function ConsoleDeviceDrawText(Console:PConsoleDevice;Handle:TFontHandle;const Text:String;X,Y,Forecolor,Backcolor,Len:LongWord):LongWord;
{Draw a text string on a console device}
{Console: The console device to draw on}
{Handle: The handle of the font to draw with}
{Text: The text to draw}
{X: The left starting point of the text (Pixels for CONSOLE_MODE_PIXEL / Characters for CONSOLE_MODE_CHARACTER)}
{Y: The top starting point of the text (Pixels for CONSOLE_MODE_PIXEL / Characters for CONSOLE_MODE_CHARACTER)}
{Forecolor: The foreground color for the text (eg COLOR_WHITE)}
{Backcolor: The background color for the text (eg COLOR_BLACK)}
{Len: The length of the text (Pixels for CONSOLE_MODE_PIXEL / Characters for CONSOLE_MODE_CHARACTER)}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: Forecolor and Backcolor must be specified in the default color format (See COLOR_FORMAT_DEFAULT)}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console Device Draw Text');
 {$ENDIF}

 {Check Open}
 Result:=ERROR_NOT_SUPPORTED;
 if Console.ConsoleState <> CONSOLE_STATE_OPEN then Exit;

 if Assigned(Console.DeviceDrawText) then
  begin
   Result:=Console.DeviceDrawText(Console,Handle,Text,X,Y,Forecolor,Backcolor,Len);
  end;
end;

{==============================================================================}

function ConsoleDeviceDrawPixel(Console:PConsoleDevice;X,Y,Color:LongWord):LongWord;
{Draw a pixel on a console device}
{Console: The console device to draw on}
{X: The column to draw the pixel (Pixels for CONSOLE_MODE_PIXEL / Characters for CONSOLE_MODE_CHARACTER)}
{Y: The row to draw the pixel (Pixels for CONSOLE_MODE_PIXEL / Characters for CONSOLE_MODE_CHARACTER)}
{Color: The color to draw with (eg COLOR_WHITE)}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: Color must be specified in the default color format (See COLOR_FORMAT_DEFAULT)}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console Device Draw Pixel');
 {$ENDIF}

 {Check Open}
 Result:=ERROR_NOT_SUPPORTED;
 if Console.ConsoleState <> CONSOLE_STATE_OPEN then Exit;

 if Assigned(Console.DeviceDrawPixel) then
  begin
   Result:=Console.DeviceDrawPixel(Console,X,Y,Color);
  end
 else
  begin
   Result:=ERROR_CALL_NOT_IMPLEMENTED;
  end;
end;

{==============================================================================}

function ConsoleDeviceDrawBlock(Console:PConsoleDevice;X1,Y1,X2,Y2,Color:LongWord):LongWord;
{Draw a filled block on a console device}
{Console: The console device to draw on}
{X1: The left edge of the block (Pixels for CONSOLE_MODE_PIXEL / Characters for CONSOLE_MODE_CHARACTER)}
{Y1: The top edge of the block (Pixels for CONSOLE_MODE_PIXEL / Characters for CONSOLE_MODE_CHARACTER)}
{X2: The right edge of the block (Pixels for CONSOLE_MODE_PIXEL / Characters for CONSOLE_MODE_CHARACTER)}
{Y2: The bottom edge of the block (Pixels for CONSOLE_MODE_PIXEL / Characters for CONSOLE_MODE_CHARACTER)}
{Color: The color to draw with (eg COLOR_WHITE)}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: Color must be specified in the default color format (See COLOR_FORMAT_DEFAULT)}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console Device Draw Block');
 {$ENDIF}

 {Check Open}
 Result:=ERROR_NOT_SUPPORTED;
 if Console.ConsoleState <> CONSOLE_STATE_OPEN then Exit;

 if Assigned(Console.DeviceDrawBlock) then
  begin
   Result:=Console.DeviceDrawBlock(Console,X1,Y1,X2,Y2,Color);
  end;
end;

{==============================================================================}

function ConsoleDeviceDrawCircle(Console:PConsoleDevice;X,Y,Color,Width,Radius:LongWord):LongWord;
{Draw a circle on a console device}
{Console: The console device to draw on}
{X: The column center point of the circle (Pixels for CONSOLE_MODE_PIXEL / Characters for CONSOLE_MODE_CHARACTER)}
{Y: The row center point of the circle (Pixels for CONSOLE_MODE_PIXEL / Characters for CONSOLE_MODE_CHARACTER)}
{Color: The color to draw with (eg COLOR_WHITE)}
{Width: The width of the circle outline (Pixels for CONSOLE_MODE_PIXEL / Characters for CONSOLE_MODE_CHARACTER)}
{Radius: The radius of the circle (Pixels for CONSOLE_MODE_PIXEL / Characters for CONSOLE_MODE_CHARACTER)}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: Color must be specified in the default color format (See COLOR_FORMAT_DEFAULT)}
{Note: Character consoles may implement draw circle but the default method only supports pixel consoles}

 procedure FillX(Console:PConsoleDevice;X1,X2,Y:LongInt;Color:LongWord);
 begin
  {}
  while X1 <= X2 do
   begin
    ConsoleDeviceDrawPixel(Console,X1,Y,Color);
    Inc(X1);
   end;
 end;

 procedure FillY(Console:PConsoleDevice;X,Y1,Y2:LongInt;Color:LongWord);
 begin
  {}
  while Y1 <= Y2 do
   begin
    ConsoleDeviceDrawPixel(Console,X,Y1,Color);
    Inc(Y1);
   end;
 end;

var
 Count:Integer;
 Decision:LongInt;
 DeltaX,DeltaY:LongInt;
 OffsetX,OffsetY:LongInt;

 InnerRadius:LongWord;
 InnerX,OuterX:LongInt;
 DecisionInner,DecisionOuter:LongInt;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console Device Draw Circle');
 {$ENDIF}

 {Check Open}
 Result:=ERROR_NOT_SUPPORTED;
 if Console.ConsoleState <> CONSOLE_STATE_OPEN then Exit;

 if Assigned(Console.DeviceDrawCircle) then
  begin
   Result:=Console.DeviceDrawCircle(Console,X,Y,Color,Width,Radius);
  end
 else
  begin
   {Default Method}
   {Check Mode}
   if Console.ConsoleMode <> CONSOLE_MODE_PIXEL then Exit;

   {Check Color}
   {if Color = COLOR_NONE then Exit;}

   {Check Width}
   if Width < 1 then Exit;

   {Check Radius}
   if Radius < 1 then Exit;

   {Check Width}
   if Width > Radius then Width:=Radius;

   if MutexLock(Console.Lock) = ERROR_SUCCESS then
    begin
     try
      {Check Width}
      if Width = 1 then
       begin
        {Setup Defaults}
        OffsetX:=Radius - 1;
        OffsetY:=0;
        DeltaX:=1;
        DeltaY:=1;
        Decision:=DeltaX - (Radius shl 1);

        {Draw the Circle}
        while OffsetX >= OffsetY do
         begin
          ConsoleDeviceDrawPixel(Console,X + OffsetX,Y + OffsetY,Color);
          ConsoleDeviceDrawPixel(Console,X + OffsetY,Y + OffsetX,Color);
          ConsoleDeviceDrawPixel(Console,X - OffsetY,Y + OffsetX,Color);
          ConsoleDeviceDrawPixel(Console,X - OffsetX,Y + OffsetY,Color);
          ConsoleDeviceDrawPixel(Console,X - OffsetX,Y - OffsetY,Color);
          ConsoleDeviceDrawPixel(Console,X - OffsetY,Y - OffsetX,Color);
          ConsoleDeviceDrawPixel(Console,X + OffsetY,Y - OffsetX,Color);
          ConsoleDeviceDrawPixel(Console,X + OffsetX,Y - OffsetY,Color);

          if Decision <= 0 then
           begin
            Inc(OffsetY);
            Inc(Decision,DeltaY);
            Inc(DeltaY,2);
           end;
          if Decision > 0 then
           begin
            Dec(OffsetX);
            Inc(DeltaX,2);
            Inc(Decision,(-Radius shl 1) + DeltaX);
           end;
         end;
       end
      else
       begin
        {Get Radius}
        InnerRadius:=Radius - Width + 1;

        {Setup Defaults}
        OuterX:=Radius;
        InnerX:=InnerRadius;
        OffsetY:=0;
        DecisionOuter:=1 - OuterX;
        DecisionInner:=1 - InnerX;

        while OuterX >= OffsetY do
         begin
          FillX(Console,X + InnerX,X + OuterX,Y + OffsetY,Color);
          FillY(Console,X + OffsetY,Y + InnerX,Y + OuterX,Color);
          FillX(Console,X - OuterX,X - InnerX,Y + OffsetY,Color);
          FillY(Console,X - OffsetY,Y + InnerX,Y + OuterX,Color);
          FillX(Console,X - OuterX,X - InnerX,Y - OffsetY,Color);
          FillY(Console,X - OffsetY,Y - OuterX,Y - InnerX,Color);
          FillX(Console,X + InnerX,X + OuterX,Y - OffsetY,Color);
          FillY(Console,X + OffsetY,Y - OuterX,Y - InnerX,Color);

          Inc(OffsetY);

          if DecisionOuter < 0 then
           begin
            Inc(DecisionOuter,(OffsetY shl 1) + 1);
           end
          else
           begin
            Dec(OuterX);
            Inc(DecisionOuter,(OffsetY - OuterX + 1) shl 1);
           end;

          if OffsetY > InnerRadius then
           begin
            InnerX:=OffsetY;
           end
          else
           begin
            if DecisionInner < 0 then
             begin
              Inc(DecisionInner,(OffsetY shl 1) + 1);
             end
            else
             begin
              Dec(InnerX);
              Inc(DecisionInner,(OffsetY - InnerX + 1) shl 1);
             end;
           end;
         end;
       end;

      {Get Result}
      Result:=ERROR_SUCCESS;
     finally
      MutexUnlock(Console.Lock);
     end;
    end
   else
    begin
     Result:=ERROR_CAN_NOT_COMPLETE;
    end;
  end;
end;

{==============================================================================}

function ConsoleDeviceDrawImage(Console:PConsoleDevice;X,Y:LongWord;Buffer:Pointer;Width,Height,Format,Skip:LongWord):LongWord;
{Draw an image on a console device}
{Console: The console device to draw on}
{X: The starting column of the image (Pixels for CONSOLE_MODE_PIXEL / Characters for CONSOLE_MODE_CHARACTER)}
{Y: The starting row of the image (Pixels for CONSOLE_MODE_PIXEL / Characters for CONSOLE_MODE_CHARACTER)}
{Buffer: Pointer to a block of memory containing the pixels of the image in a contiguous block of rows}
{Width: The number of columns in the image (Pixels for CONSOLE_MODE_PIXEL / Characters for CONSOLE_MODE_CHARACTER)}
{Height: The number of rows in the image (Pixels for CONSOLE_MODE_PIXEL / Characters for CONSOLE_MODE_CHARACTER)}
{Format: The color format of the image (eg COLOR_FORMAT_RGB24)}
{Skip: The number of pixels to skip in the buffer after each row (Optional)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console Device Draw Image');
 {$ENDIF}

 {Check Open}
 Result:=ERROR_NOT_SUPPORTED;
 if Console.ConsoleState <> CONSOLE_STATE_OPEN then Exit;

 if Assigned(Console.DeviceDrawImage) then
  begin
   Result:=Console.DeviceDrawImage(Console,X,Y,Buffer,Width,Height,Format,Skip);
  end
 else
  begin
   Result:=ERROR_CALL_NOT_IMPLEMENTED;
  end;
end;

{==============================================================================}

function ConsoleDeviceDrawWindow(Console:PConsoleDevice;Handle:TWindowHandle;Flags:LongWord):LongWord;
{Draw a console window on a console device}
{Console: The console device to draw on}
{Handle: The handle of the console window to draw}
{Flags: Flags to specify what should be drawn (eg WINDOW_DRAW_FLAG_BORDER)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console Device Draw Window');
 {$ENDIF}

 {Check Open}
 Result:=ERROR_NOT_SUPPORTED;
 if Console.ConsoleState <> CONSOLE_STATE_OPEN then Exit;

 if Assigned(Console.DeviceDrawWindow) then
  begin
   Result:=Console.DeviceDrawWindow(Console,Handle,Flags);
  end;
end;

{==============================================================================}

function ConsoleDeviceGetPixel(Console:PConsoleDevice;X,Y:LongWord;var Color:LongWord):LongWord;
{Read a pixel from a console device}
{Console: The console device to read from}
{X: The column to read the pixel from (Pixels for CONSOLE_MODE_PIXEL / Characters for CONSOLE_MODE_CHARACTER)}
{Y: The row to read the pixel from (Pixels for CONSOLE_MODE_PIXEL / Characters for CONSOLE_MODE_CHARACTER)}
{Color: The color value read from the console (eg COLOR_WHITE)}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: Color is returned in the default color format (See COLOR_FORMAT_DEFAULT)}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console Device Get Pixel');
 {$ENDIF}

 {Check Open}
 Result:=ERROR_NOT_SUPPORTED;
 if Console.ConsoleState <> CONSOLE_STATE_OPEN then Exit;

 if Assigned(Console.DeviceGetPixel) then
  begin
   Result:=Console.DeviceGetPixel(Console,X,Y,Color);
  end
 else
  begin
   Result:=ERROR_CALL_NOT_IMPLEMENTED;
  end;
end;

{==============================================================================}

function ConsoleDeviceGetImage(Console:PConsoleDevice;X,Y:LongWord;Buffer:Pointer;Width,Height,Format,Skip:LongWord):LongWord;
{Read an image from a console device}
{Console: The console device to read from}
{X: The starting column to read the image from (Pixels for CONSOLE_MODE_PIXEL / Characters for CONSOLE_MODE_CHARACTER)}
{Y: The starting row to read the image from (Pixels for CONSOLE_MODE_PIXEL / Characters for CONSOLE_MODE_CHARACTER)}
{Buffer: Pointer to a block of memory large enough to hold the pixels of the image in a contiguous block of rows}
{Width: The number of columns to store in the image (Pixels for CONSOLE_MODE_PIXEL / Characters for CONSOLE_MODE_CHARACTER)}
{Height: The number of rows to store in the image (Pixels for CONSOLE_MODE_PIXEL / Characters for CONSOLE_MODE_CHARACTER)}
{Format: The color format to store the image in (eg COLOR_FORMAT_RGB24)}
{Skip: The number of pixels to skip in the buffer after each row (Optional)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console Device Get Image');
 {$ENDIF}

 {Check Open}
 Result:=ERROR_NOT_SUPPORTED;
 if Console.ConsoleState <> CONSOLE_STATE_OPEN then Exit;

 if Assigned(Console.DeviceGetImage) then
  begin
   Result:=Console.DeviceGetImage(Console,X,Y,Buffer,Width,Height,Format,Skip);
  end
 else
  begin
   Result:=ERROR_CALL_NOT_IMPLEMENTED;
  end;
end;

{==============================================================================}

function ConsoleDevicePutText(Console:PConsoleDevice;Handle:TFontHandle;const Source,Dest:TConsolePoint;Buffer:PConsoleChar;Width,Height,Skip:LongWord):LongWord;
{Output a rectangular area of text to a console device}
{Console: The console device to output to}
{Source: The X and Y point in the source buffer to copy text from (Characters)}
{Dest: The X and Y point on the console device to copy text to (Pixels for CONSOLE_MODE_PIXEL / Characters for CONSOLE_MODE_CHARACTER)}
{Buffer: A pointer to a buffer of TConsoleChar structures which represent rows of text}
{Width: The width of the area to be output (Characters)}
{Height: The height of the area to be output (Characters)}
{Skip: The number of characters to skip in the buffer after each row (Optional)}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: Source, Width, Height and Skip are based on character rows and columns not screen pixels}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console Device Put Text');
 {$ENDIF}

 {Check Open}
 Result:=ERROR_NOT_SUPPORTED;
 if Console.ConsoleState <> CONSOLE_STATE_OPEN then Exit;

 if Assigned(Console.DevicePutText) then
  begin
   Result:=Console.DevicePutText(Console,Handle,Source,Dest,Buffer,Width,Height,Skip);
  end
 else
  begin
   Result:=ERROR_CALL_NOT_IMPLEMENTED;
  end;
end;

{==============================================================================}

function ConsoleDeviceCopyImage(Console:PConsoleDevice;const Source,Dest:TConsolePoint;Width,Height:LongWord):LongWord;
{Copy an image within a console device}
{Console: The console device to copy on}
{Source: The starting point for the source of the copy (Pixels for CONSOLE_MODE_PIXEL / Characters for CONSOLE_MODE_CHARACTER)}
{Dest: The starting point for the destination of the copy (Pixels for CONSOLE_MODE_PIXEL / Characters for CONSOLE_MODE_CHARACTER)}
{Width: The number of columns in the image (Pixels for CONSOLE_MODE_PIXEL / Characters for CONSOLE_MODE_CHARACTER)}
{Height: The number of rows in the image (Pixels for CONSOLE_MODE_PIXEL / Characters for CONSOLE_MODE_CHARACTER)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console Device Copy Image');
 {$ENDIF}

 {Check Open}
 Result:=ERROR_NOT_SUPPORTED;
 if Console.ConsoleState <> CONSOLE_STATE_OPEN then Exit;

 if Assigned(Console.DeviceCopyImage) then
  begin
   Result:=Console.DeviceCopyImage(Console,Source,Dest,Width,Height);
  end
 else
  begin
   Result:=ERROR_CALL_NOT_IMPLEMENTED;
  end;
end;

{==============================================================================}

function ConsoleDeviceAddCaret(Console:PConsoleDevice;Width,Height,OffsetX,OffsetY:LongWord):THandle;
{Create a new caret (cursor) of the specified size}
{Console: The console device to create the caret on}
{Width: The width of the new caret (Pixels for CONSOLE_MODE_PIXEL / Always 1 for CONSOLE_MODE_CHARACTER)}
{Height: The height of the new caret (Pixels for CONSOLE_MODE_PIXEL / Always 1 for CONSOLE_MODE_CHARACTER)}
{OffsetX: The X offset of the new caret (Optional)(Pixels for CONSOLE_MODE_PIXEL / Always 0 for CONSOLE_MODE_CHARACTER)}
{OffsetY: The Y offset of the new caret (Optional)(Pixels for CONSOLE_MODE_PIXEL / Always 0 for CONSOLE_MODE_CHARACTER)}
{Return: Handle to new caret on success or INVALID_HANDLE_VALUE on failure}
begin
 {}
 Result:=INVALID_HANDLE_VALUE;

 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console Device Add Caret');
 {$ENDIF}

 {Check Open}
 Result:=ERROR_NOT_SUPPORTED;
 if Console.ConsoleState <> CONSOLE_STATE_OPEN then Exit;

 if Assigned(Console.DeviceAddCaret) then
  begin
   Result:=Console.DeviceAddCaret(Console,Width,Height,OffsetX,OffsetY);
  end
 else
  begin
   Result:=ERROR_CALL_NOT_IMPLEMENTED;
  end;
end;

{==============================================================================}

function ConsoleDeviceDeleteCaret(Console:PConsoleDevice;Handle:THandle):LongWord;
{Delete an existing caret (cursor)}
{Console: The console device to delete the caret on}
{Handle: The handle of the caret to delete (as returned from ConsoleDeviceAddCaret)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console Device Delete Caret');
 {$ENDIF}

 {Check Open}
 Result:=ERROR_NOT_SUPPORTED;
 if Console.ConsoleState <> CONSOLE_STATE_OPEN then Exit;

 if Assigned(Console.DeviceDeleteCaret) then
  begin
   Result:=Console.DeviceDeleteCaret(Console,Handle);
  end
 else
  begin
   Result:=ERROR_CALL_NOT_IMPLEMENTED;
  end;
end;

{==============================================================================}

function ConsoleDeviceUpdateCaret(Console:PConsoleDevice;Handle:THandle;X,Y:LongWord;Visible,Blink:Boolean):LongWord; inline;
{Update an existing carets position, visibility or blink}
{Console: The console device to update the caret on}
{Handle: The handle of the caret to update (as returned from ConsoleDeviceAddCaret)}
{X: The X position of the caret (Pixels for CONSOLE_MODE_PIXEL / Characters for CONSOLE_MODE_CHARACTER)}
{Y: The Y position of the caret (Pixels for CONSOLE_MODE_PIXEL / Characters for CONSOLE_MODE_CHARACTER)}
{Visible: If true then show the caret else hide it}
{Blink: If true then blink the caret at the default blink rate}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ConsoleDeviceUpdateCaretEx(Console,Handle,X,Y,COLOR_NONE,COLOR_NONE,Visible,Blink,False);
end;

{==============================================================================}

function ConsoleDeviceUpdateCaretEx(Console:PConsoleDevice;Handle:THandle;X,Y,Forecolor,Backcolor:LongWord;Visible,Blink,Reverse:Boolean):LongWord;
{Update an existing carets position, colors, visibility, blink or reverse}
{Console: The console device to update the caret on}
{Handle: The handle of the caret to update (as returned from ConsoleDeviceAddCaret)}
{X: The X position of the caret (Pixels for CONSOLE_MODE_PIXEL / Characters for CONSOLE_MODE_CHARACTER)}
{Y: The Y position of the caret (Pixels for CONSOLE_MODE_PIXEL / Characters for CONSOLE_MODE_CHARACTER)}
{Forecolor: The cursor foreground color if set or COLOR_NONE to disable}
{Backcolor: The cursor background color if set or COLOR_NONE to disable}
{Visible: If true then show the caret else hide it}
{Blink: If true then blink the caret at the default blink rate}
{Reverse: If true then enable reverse color else enable inverse color}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: Forecolor and Backcolor must be specified in the default color format (See COLOR_FORMAT_DEFAULT)}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console Device Update Caret Ex');
 {$ENDIF}

 {Check Open}
 Result:=ERROR_NOT_SUPPORTED;
 if Console.ConsoleState <> CONSOLE_STATE_OPEN then Exit;

 if Assigned(Console.DeviceUpdateCaretEx) then
  begin
   Result:=Console.DeviceUpdateCaretEx(Console,Handle,X,Y,Forecolor,Backcolor,Visible,Blink,Reverse);
  end;
 if Assigned(Console.DeviceUpdateCaret) then
  begin
   Result:=Console.DeviceUpdateCaret(Console,Handle,X,Y,Visible,Blink);
  end
 else
  begin
   Result:=ERROR_CALL_NOT_IMPLEMENTED;
  end;
end;

{==============================================================================}

function ConsoleDeviceSetCursor(Console:PConsoleDevice;Width,Height:LongWord;Chars:PChar):LongWord;
{Set the mouse cursor properties of a console device (CONSOLE_MODE_CHARACTER only)}
{Console: The console device to set the cursor}
{Width: The width of the cursor in characters}
{Height: The height of the cursor in characters}
{Chars: A buffer containing the cursor characters}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: For devices that don't support hardware mouse cursor a software cursor will be implemented
       If chars is nil then the default cursor will be used}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console Device Set Cursor');
 {$ENDIF}

 {Check Open}
 Result:=ERROR_NOT_SUPPORTED;
 if Console.ConsoleState <> CONSOLE_STATE_OPEN then Exit;

 if Assigned(Console.DeviceSetCursor) then
  begin
   Result:=Console.DeviceSetCursor(Console,Width,Height,Chars);
  end
 else
  begin
   {Lock Console}
   if MutexLock(Console.Lock) <> ERROR_SUCCESS then Exit;
   try

    //To Do

   finally
    {Unlock Console}
    MutexUnlock(Console.Lock);
   end;
  end;
end;

{==============================================================================}

function ConsoleDeviceUpdateCursor(Console:PConsoleDevice;Enabled:Boolean;X,Y:LongInt;Relative:Boolean):LongWord;
{Update the position and state for the mouse cursor of a console device (CONSOLE_MODE_CHARACTER only)}
{Console: The console device to update the cursor}
{Enabled: If true then show the cursor else hide it}
{X: The cursor X location in characters}
{Y: The cursor Y location in characters}
{Relative: If true then X and Y are considered relative to the current position}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: For devices that don't support hardware mouse cursor a software cursor will be implemented}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console Device Update Cursor');
 {$ENDIF}

 {Check Open}
 Result:=ERROR_NOT_SUPPORTED;
 if Console.ConsoleState <> CONSOLE_STATE_OPEN then Exit;

 if Assigned(Console.DeviceUpdateCursor) then
  begin
   Result:=Console.DeviceUpdateCursor(Console,Enabled,X,Y,Relative);
  end
 else
  begin
   {Lock Console}
   if MutexLock(Console.Lock) <> ERROR_SUCCESS then Exit;
   try

    //To Do

   finally
    {Unlock Console}
    MutexUnlock(Console.Lock);
   end;
  end;
end;

{==============================================================================}

function ConsoleDeviceGetPosition(Console:PConsoleDevice;Position:LongWord;var X1,Y1,X2,Y2:LongWord):LongWord;
{Get the coordinates of a console position from a console device}
{Console: The console device to get from}
{Position: The console position to get the coordinates for (eg CONSOLE_POSITION_FULL)}
{X1: The left edge of the console position (Pixels for CONSOLE_MODE_PIXEL / Characters for CONSOLE_MODE_CHARACTER)}
{Y1: The top edge of the console position (Pixels for CONSOLE_MODE_PIXEL / Characters for CONSOLE_MODE_CHARACTER)}
{X2: The right edge of the console position (Pixels for CONSOLE_MODE_PIXEL / Characters for CONSOLE_MODE_CHARACTER)}
{Y2: The bottom edge of the console position (Pixels for CONSOLE_MODE_PIXEL / Characters for CONSOLE_MODE_CHARACTER)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console Device Get Position');
 {$ENDIF}

 {Check Open}
 Result:=ERROR_NOT_SUPPORTED;
 if Console.ConsoleState <> CONSOLE_STATE_OPEN then Exit;

 if Assigned(Console.DeviceGetPosition) then
  begin
   Result:=Console.DeviceGetPosition(Console,Position,X1,Y1,X2,Y2);
  end;
end;

{==============================================================================}

function ConsoleDeviceGetProperties(Console:PConsoleDevice;Properties:PConsoleProperties):LongWord;
{Get the current properties from a console device}
{Console: The console device to get properties from}
{Properties: Pointer to a TConsoleProperties structure to return}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Properties}
 if Properties = nil then Exit;

 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console Device Get Properties');
 {$ENDIF}

 {Check Open}
 {Result:=ERROR_NOT_SUPPORTED;}
 {if Console.ConsoleState <> CONSOLE_STATE_OPEN then Exit;} {Allow when closed}

 if Assigned(Console.DeviceGetProperties) then
  begin
   Result:=Console.DeviceGetProperties(Console,Properties);
  end
 else
  begin
   {Default Method}
   if MutexLock(Console.Lock) <> ERROR_SUCCESS then Exit;

   {Get Properties}
   Properties.Flags:=Console.Device.DeviceFlags;
   Properties.Mode:=Console.ConsoleMode;
   Properties.Width:=Console.Width;
   Properties.Height:=Console.Height;
   Properties.Format:=Console.Format;

   {Return Result}
   Result:=ERROR_SUCCESS;

   MutexUnlock(Console.Lock);
  end;
end;

{==============================================================================}

function ConsoleDeviceCheckFlag(Console:PConsoleDevice;Flag:LongWord):Boolean;
{Check if a console device supports a flag value}
{Console: The console device to check}
{Flag: The console flag to check (eg CONSOLE_FLAG_FULLSCREEN)}
{Return: True if flag is supported, False if not or on error}
begin
 {}
 Result:=False;

 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Open}
 {if Console.ConsoleState <> CONSOLE_STATE_OPEN then Exit;} {Allow when closed}

 if MutexLock(Console.Lock) <> ERROR_SUCCESS then Exit;

 {Check Flag}
 Result:=((Console.Device.DeviceFlags and Flag) <> 0);

 MutexUnlock(Console.Lock);
end;

{==============================================================================}

function ConsoleDeviceUpdateFlag(Console:PConsoleDevice;Flag:LongWord;Clear:Boolean):LongWord;
{Set or clear a flag on a console device}
{Console: The console device to set or clear the flag on}
{Flag: The console flag to set or clear (eg CONSOLE_FLAG_LINE_WRAP)}
{Clear: If true clear the flag, else set it}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Flag}
 if (Flag and CONSOLE_FLAG_INTERNAL) <> 0 then Exit;

 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Open}
 {if Console.ConsoleState <> CONSOLE_STATE_OPEN then Exit;} {Allow when closed}

 if MutexLock(Console.Lock) <> ERROR_SUCCESS then Exit;

 if Clear then
  begin
   {Clear Flag}
   Console.Device.DeviceFlags:=Console.Device.DeviceFlags and not(Flag);
  end
 else
  begin
   {Set Flag}
   Console.Device.DeviceFlags:=Console.Device.DeviceFlags or Flag;
  end;

 MutexUnlock(Console.Lock);

 {Return Result}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function ConsoleDeviceGetMode(Console:PConsoleDevice):LongWord;
{Get the mode of a console device}
{Console: The console device to get from}
{Return: The mode of the console (eg CONSOLE_MODE_PIXEL) or CONSOLE_MODE_NONE on error}
begin
 {}
 Result:=CONSOLE_MODE_NONE;

 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Open}
 {if Console.ConsoleState <> CONSOLE_STATE_OPEN then Exit;} {Allow when closed}

 if MutexLock(Console.Lock) <> ERROR_SUCCESS then Exit;

 {Get Mode}
 Result:=Console.ConsoleMode;

 MutexUnlock(Console.Lock);
end;

{==============================================================================}

function ConsoleDeviceGetState(Console:PConsoleDevice):LongWord;
{Get the state of a console device}
{Console: The console device to get from}
{Return: The current state of the console (eg CONSOLE_STATE_OPEN)}
begin
 {}
 Result:=CONSOLE_STATE_CLOSED;

 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Open}
 {if Console.ConsoleState <> CONSOLE_STATE_OPEN then Exit;} {Allow when closed}

 if MutexLock(Console.Lock) <> ERROR_SUCCESS then Exit;

 {Get State}
 Result:=Console.ConsoleState;

 MutexUnlock(Console.Lock);
end;

{==============================================================================}

function ConsoleDeviceCreate:PConsoleDevice;
{Create a new Console entry}
{Return: Pointer to new Console entry or nil if Console could not be created}
begin
 {}
 Result:=ConsoleDeviceCreateEx(SizeOf(TConsoleDevice));
end;

{==============================================================================}

function ConsoleDeviceCreateEx(Size:LongWord):PConsoleDevice;
{Create a new Console entry}
{Size: Size in bytes to allocate for new Console (Including the Console entry)}
{Return: Pointer to new Console entry or nil if Console could not be created}
begin
 {}
 Result:=nil;

 {Check Size}
 if Size < SizeOf(TConsoleDevice) then Exit;

 {Create Console}
 Result:=PConsoleDevice(DeviceCreateEx(Size));
 if Result = nil then Exit;

 {Update Device}
 Result.Device.DeviceBus:=DEVICE_BUS_NONE;
 Result.Device.DeviceType:=CONSOLE_TYPE_NONE;
 Result.Device.DeviceFlags:=CONSOLE_FLAG_NONE;
 Result.Device.DeviceData:=nil;

 {Update Console}
 Result.ConsoleId:=DEVICE_ID_ANY;
 Result.ConsoleState:=CONSOLE_STATE_CLOSED;
 Result.ConsoleMode:=CONSOLE_MODE_NONE;
 Result.DeviceOpen:=nil;
 Result.DeviceClose:=nil;
 Result.DeviceClear:=nil;
 Result.DeviceScroll:=nil;
 Result.DeviceDrawBox:=nil;
 Result.DeviceDrawLine:=nil;
 Result.DeviceDrawChar:=nil;
 Result.DeviceDrawText:=nil;
 Result.DeviceDrawPixel:=nil;
 Result.DeviceDrawBlock:=nil;
 Result.DeviceDrawImage:=nil;
 Result.DeviceDrawWindow:=nil;
 Result.DeviceGetPixel:=nil;
 Result.DeviceGetImage:=nil;
 Result.DevicePutText:=nil;
 Result.DeviceCopyImage:=nil;
 Result.DeviceAddCaret:=nil;
 Result.DeviceDeleteCaret:=nil;
 Result.DeviceUpdateCaret:=nil;
 Result.DeviceUpdateCaretEx:=nil;
 Result.DeviceSetCursor:=nil;
 Result.DeviceUpdateCursor:=nil;
 Result.DeviceGetPosition:=nil;
 Result.DeviceGetProperties:=nil;
 Result.Lock:=INVALID_HANDLE_VALUE;
 Result.Width:=0;
 Result.Height:=0;
 Result.Format:=COLOR_FORMAT_UNKNOWN;
 Result.Forecolor:=CONSOLE_DEFAULT_FORECOLOR;
 Result.Backcolor:=CONSOLE_DEFAULT_BACKCOLOR;
 Result.Borderwidth:=CONSOLE_DEFAULT_BORDERWIDTH;
 Result.Bordercolor:=CONSOLE_DEFAULT_BORDERCOLOR;
 Result.Font:=INVALID_HANDLE_VALUE;
 Result.FontRatio:=1;
 Result.CursorUpdate:=False;
 Result.CursorX:=1;
 Result.CursorY:=1;
 Result.CursorWidth:=1;
 Result.CursorHeight:=1;
 Result.CursorVisible:=False;
 Result.CaretFirst:=nil;
 Result.CaretLock:=INVALID_HANDLE_VALUE;
 Result.CaretCount:=0;
 Result.WindowFirst:=nil;
 Result.WindowLock:=INVALID_HANDLE_VALUE;
 Result.WindowCount:=0;
 Result.WindowActive:=nil;
 Result.WindowDefault:=INVALID_HANDLE_VALUE;

 {Create Lock}
 Result.Lock:=MutexCreateEx(False,MUTEX_DEFAULT_SPINCOUNT,MUTEX_FLAG_RECURSIVE);
 if Result.Lock = INVALID_HANDLE_VALUE then
  begin
   if DEVICE_LOG_ENABLED then DeviceLogError(nil,'Failed to create lock for console device');
   ConsoleDeviceDestroy(Result);
   Result:=nil;
   Exit;
  end;

 {Create Window Lock}
 Result.WindowLock:=CriticalSectionCreate;
 if Result.WindowLock = INVALID_HANDLE_VALUE then
  begin
   if DEVICE_LOG_ENABLED then DeviceLogError(nil,'Failed to create window table lock for console device');
   ConsoleDeviceDestroy(Result);
   Result:=nil;
   Exit;
  end;

 {Create Caret Lock}
 Result.CaretLock:=CriticalSectionCreate;
 if Result.CaretLock = INVALID_HANDLE_VALUE then
  begin
   if DEVICE_LOG_ENABLED then DeviceLogError(nil,'Failed to create caret table lock for console device');
   ConsoleDeviceDestroy(Result);
   Result:=nil;
   Exit;
  end;
end;

{==============================================================================}

function ConsoleDeviceDestroy(Console:PConsoleDevice):LongWord;
{Destroy an existing Console entry}
{Console: The console device to destroy}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Console}
 Result:=ERROR_IN_USE;
 if ConsoleDeviceCheck(Console) = Console then Exit;

 {Check State}
 if Console.Device.DeviceState <> DEVICE_STATE_UNREGISTERED then Exit;

 {Destroy Caret Lock}
 if Console.CaretLock <> INVALID_HANDLE_VALUE then
  begin
   CriticalSectionDestroy(Console.CaretLock);
  end;

 {Destroy Window Lock}
 if Console.WindowLock <> INVALID_HANDLE_VALUE then
  begin
   CriticalSectionDestroy(Console.WindowLock);
  end;

 {Destroy Lock}
 if Console.Lock <> INVALID_HANDLE_VALUE then
  begin
   MutexDestroy(Console.Lock);
  end;

 {Destroy Console}
 Result:=DeviceDestroy(@Console.Device);
end;

{==============================================================================}

function ConsoleDeviceRegister(Console:PConsoleDevice):LongWord;
{Register a new Console in the Console table}
{Console: The console device to register}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 ConsoleId:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Console}
 if Console = nil then Exit;
 if Console.ConsoleId <> DEVICE_ID_ANY then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Interfaces}
 if Console.ConsoleMode = CONSOLE_MODE_CHARACTER then
  begin
   if not(Assigned(Console.DeviceOpen)) then Exit;
   if not(Assigned(Console.DeviceClose)) then Exit;
   if not(Assigned(Console.DeviceClear)) then Exit;
   if not(Assigned(Console.DeviceScroll)) then Exit;
   if not(Assigned(Console.DeviceDrawChar)) then Exit;
   if not(Assigned(Console.DeviceDrawText)) then Exit;
   if not(Assigned(Console.DeviceDrawBlock)) then Exit;
   if not(Assigned(Console.DeviceDrawWindow)) then Exit;
   if not(Assigned(Console.DevicePutText)) then Exit;
   if not(Assigned(Console.DeviceGetPosition)) then Exit;
  end
 else
  begin
   if not(Assigned(Console.DeviceOpen)) then Exit;
   if not(Assigned(Console.DeviceClose)) then Exit;
   if not(Assigned(Console.DeviceClear)) then Exit;
   if not(Assigned(Console.DeviceScroll)) then Exit;
   if not(Assigned(Console.DeviceDrawBox)) then Exit;
   if not(Assigned(Console.DeviceDrawLine)) then Exit;
   if not(Assigned(Console.DeviceDrawChar)) then Exit;
   if not(Assigned(Console.DeviceDrawText)) then Exit;
   if not(Assigned(Console.DeviceDrawPixel)) then Exit;
   if not(Assigned(Console.DeviceDrawBlock)) then Exit;
   if not(Assigned(Console.DeviceDrawImage)) then Exit;
   if not(Assigned(Console.DeviceDrawWindow)) then Exit;
   if not(Assigned(Console.DeviceGetPixel)) then Exit;
   if not(Assigned(Console.DeviceGetImage)) then Exit;
   if not(Assigned(Console.DevicePutText)) then Exit;
   if not(Assigned(Console.DeviceCopyImage)) then Exit;
   if not(Assigned(Console.DeviceGetPosition)) then Exit;
  end;

 {Check Console}
 Result:=ERROR_ALREADY_EXISTS;
 if ConsoleDeviceCheck(Console) = Console then Exit;

 {Check State}
 if Console.Device.DeviceState <> DEVICE_STATE_UNREGISTERED then Exit;

 {Insert Console}
 if CriticalSectionLock(ConsoleDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Update Console}
    ConsoleId:=0;
    while ConsoleDeviceFind(ConsoleId) <> nil do
     begin
      Inc(ConsoleId);
     end;
    Console.ConsoleId:=ConsoleId;

    {Update Device}
    Console.Device.DeviceName:=CONSOLE_NAME_PREFIX + IntToStr(Console.ConsoleId);
    Console.Device.DeviceClass:=DEVICE_CLASS_CONSOLE;

    {Register Device}
    Result:=DeviceRegister(@Console.Device);
    if Result <> ERROR_SUCCESS then
     begin
      Console.ConsoleId:=DEVICE_ID_ANY;
      Exit;
     end;

    {Link Console}
    if ConsoleDeviceTable = nil then
     begin
      ConsoleDeviceTable:=Console;
     end
    else
     begin
      Console.Next:=ConsoleDeviceTable;
      ConsoleDeviceTable.Prev:=Console;
      ConsoleDeviceTable:=Console;
     end;

    {Increment Count}
    Inc(ConsoleDeviceTableCount);

    {Check Default}
    if ConsoleDeviceDefault = nil then
     begin
      ConsoleDeviceDefault:=Console;
     end;

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    CriticalSectionUnlock(ConsoleDeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function ConsoleDeviceDeregister(Console:PConsoleDevice):LongWord;
{Deregister a Console from the Console table}
{Console: The console device to deregister}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Prev:PConsoleDevice;
 Next:PConsoleDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Console}
 if Console = nil then Exit;
 if Console.ConsoleId = DEVICE_ID_ANY then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Console}
 Result:=ERROR_NOT_FOUND;
 if ConsoleDeviceCheck(Console) <> Console then Exit;

 {Check State}
 if Console.Device.DeviceState <> DEVICE_STATE_REGISTERED then Exit;

 {Remove Console}
 if CriticalSectionLock(ConsoleDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Deregister Device}
    Result:=DeviceDeregister(@Console.Device);
    if Result <> ERROR_SUCCESS then Exit;

    {Unlink Console}
    Prev:=Console.Prev;
    Next:=Console.Next;
    if Prev = nil then
     begin
      ConsoleDeviceTable:=Next;
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
    Dec(ConsoleDeviceTableCount);

    {Check Default}
    if ConsoleDeviceDefault = Console then
     begin
      ConsoleDeviceDefault:=ConsoleDeviceTable;
     end;

    {Update Console}
    Console.ConsoleId:=DEVICE_ID_ANY;

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    CriticalSectionUnlock(ConsoleDeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function ConsoleDeviceFind(ConsoleId:LongWord):PConsoleDevice;
{Find a console device by ID in the console table}
{ConsoleId: The ID number of the console to find}
{Return: Pointer to console device entry or nil if not found}
var
 Console:PConsoleDevice;
begin
 {}
 Result:=nil;

 {Check Id}
 if ConsoleId = DEVICE_ID_ANY then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(ConsoleDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Console}
    Console:=ConsoleDeviceTable;
    while Console <> nil do
     begin
      {Check State}
      if Console.Device.DeviceState = DEVICE_STATE_REGISTERED then
       begin
        {Check Id}
        if Console.ConsoleId = ConsoleId then
         begin
          Result:=Console;
          Exit;
         end;
       end;

       {Get Next}
      Console:=Console.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(ConsoleDeviceTableLock);
   end;
  end;
end;

{==============================================================================}

function ConsoleDeviceFindByDevice(Device:PDevice):PConsoleDevice;
{Find a console device by its related device in the console table}
{Device: Pointer to the related device to find}
{Return: Pointer to console device entry or nil if not found}
var
 Console:PConsoleDevice;
begin
 {}
 Result:=nil;

 {Check Device}
 if Device = nil then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(ConsoleDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Console}
    Console:=ConsoleDeviceTable;
    while Console <> nil do
     begin
      {Check State}
      if Console.Device.DeviceState = DEVICE_STATE_REGISTERED then
       begin
        {Check Device}
        if Console.Device.DeviceData = Device then
         begin
          Result:=Console;
          Exit;
         end;
       end;

       {Get Next}
      Console:=Console.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(ConsoleDeviceTableLock);
   end;
  end;
end;

{==============================================================================}

function ConsoleDeviceFindByName(const Name:String):PConsoleDevice; inline;
{Find a console device by name in the console table}
{Name: The name of the console to find (eg Console0)}
{Return: Pointer to console device entry or nil if not found}
begin
 {}
 Result:=PConsoleDevice(DeviceFindByName(Name));
end;

{==============================================================================}

function ConsoleDeviceFindByDescription(const Description:String):PConsoleDevice; inline;
{Find a console device by description in the console table}
{Description: The description of the console to find (eg Framebuffer Console (Framebuffer0))}
{Return: Pointer to console device entry or nil if not found}
begin
 {}
 Result:=PConsoleDevice(DeviceFindByDescription(Description));
end;

{==============================================================================}

function ConsoleDeviceEnumerate(Callback:TConsoleEnumerate;Data:Pointer):LongWord;
{Enumerate all console devices in the console table}
{Callback: The callback function to call for each console in the table}
{Data: A private data pointer to pass to callback for each console in the table}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Console:PConsoleDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Callback}
 if not Assigned(Callback) then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(ConsoleDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Console}
    Console:=ConsoleDeviceTable;
    while Console <> nil do
     begin
      {Check State}
      if Console.Device.DeviceState = DEVICE_STATE_REGISTERED then
       begin
        if Callback(Console,Data) <> ERROR_SUCCESS then Exit;
       end;

      {Get Next}
      Console:=Console.Next;
     end;

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    CriticalSectionUnlock(ConsoleDeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function ConsoleDeviceNotification(Console:PConsoleDevice;Callback:TConsoleNotification;Data:Pointer;Notification,Flags:LongWord):LongWord;
{Register a notification for console device changes}
{Console: The console device to notify changes for (Optional, pass nil for all console devices)}
{Callback: The function to call when a notification event occurs}
{Data: A private data pointer to pass to callback when a notification event occurs}
{Notification: The events to register for notification of (eg DEVICE_NOTIFICATION_REGISTER)}
{Flags: The flags to control the notification (eg NOTIFIER_FLAG_WORKER)}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Console}
 if Console = nil then
  begin
   Result:=DeviceNotification(nil,DEVICE_CLASS_CONSOLE,Callback,Data,Notification,Flags);
  end
 else
  begin
   {Check Console}
   if Console.Device.Signature <> DEVICE_SIGNATURE then Exit;

   Result:=DeviceNotification(@Console.Device,DEVICE_CLASS_CONSOLE,Callback,Data,Notification,Flags);
  end;
end;

{==============================================================================}
{==============================================================================}
{Text Console Functions}
function ConsoleWindowCreate(Console:PConsoleDevice;Position:LongWord;Default:Boolean):TWindowHandle;
{Create a new Console window}
{Console: The console device to create the new window on}
{Position: The console position to create the new window at (eg CONSOLE_POSITION_FULL)}
{Default: If true allow the new window to be the default window if there is no current default}
{Return: Handle to new Console window or INVALID_HANDLE_VALUE if Console window could not be created}
begin
 {}
 Result:=ConsoleWindowCreateEx(Console,INVALID_HANDLE_VALUE,SizeOf(TConsoleWindow),WINDOW_STATE_VISIBLE,WINDOW_MODE_TEXT,Position,Default);
end;

{==============================================================================}

function ConsoleWindowCreateEx(Console:PConsoleDevice;Font:TFontHandle;Size,State,Mode,Position:LongWord;Default:Boolean):TWindowHandle;
{Create a new Console window}
{Console: The console device to create the new window on}
{Font: The handle of the default font for the new console window}
{Size: The size in bytes to allocate for the new window entry (Defaults to SizeOf(TConsoleWindow))}
{State: The state of the new console window (WINDOW_STATE_VISIBLE or WINDOW_STATE_INVISIBLE)}
{Mode: The mode of the new console window (Normally WINDOW_MODE_TEXT)}
{Position: The console position to create the new window at (eg CONSOLE_POSITION_FULL)}
{Default: If true allow the new window to be the default window if there is no current default}
{Return: Handle to new Console window or INVALID_HANDLE_VALUE if Console window could not be created}
var
 X1:LongWord;
 Y1:LongWord;
 X2:LongWord;
 Y2:LongWord;
 RemainX:LongWord;
 RemainY:LongWord;
 Handle:TWindowHandle;
 Window:PConsoleWindow;
begin
 {}
 Result:=INVALID_HANDLE_VALUE;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console Window Create');
 {$ENDIF}

 {Check Size}
 if Size = 0 then Size:=SizeOf(TConsoleWindow);
 if Size < SizeOf(TConsoleWindow) then Exit;

 {Check Mode}
 {if Mode <> WINDOW_MODE_TEXT then Exit;} {Do not check mode, allow for expansion}

 {Check Position}
 if Position = CONSOLE_POSITION_FULLSCREEN then
  begin
   if ConsoleWindowGetCount(Console) <> 0 then Exit;
   if not ConsoleDeviceCheckFlag(Console,CONSOLE_FLAG_FULLSCREEN) then Exit;
  end
 else
  begin
   {if Position < CONSOLE_POSITION_FULL then Exit;}
   if Position > CONSOLE_POSITION_BOTTOMRIGHT then Exit;
   if ConsoleWindowFind(Console,Position) <> INVALID_HANDLE_VALUE then Exit;
   if ConsoleWindowFind(Console,CONSOLE_POSITION_FULLSCREEN) <> INVALID_HANDLE_VALUE then Exit;

   {Check Position}
   case Position of
    CONSOLE_POSITION_FULL:begin
      {Fail if any other windows exist}
      if ConsoleWindowGetCount(Console) <> 0 then Exit;
     end;
    CONSOLE_POSITION_TOP:begin
      {Fail on top positions}
      if ConsoleWindowFind(Console,CONSOLE_POSITION_TOPLEFT) <> INVALID_HANDLE_VALUE then Exit;
      if ConsoleWindowFind(Console,CONSOLE_POSITION_TOPRIGHT) <> INVALID_HANDLE_VALUE then Exit;

      {Get Full}
      Handle:=ConsoleWindowFind(Console,CONSOLE_POSITION_FULL);
      if Handle <> INVALID_HANDLE_VALUE then
       begin
        {Move to Bottom}
        if ConsoleWindowSetPosition(Handle,CONSOLE_POSITION_BOTTOM) <> ERROR_SUCCESS then Exit;
       end;

      {Get Left}
      Handle:=ConsoleWindowFind(Console,CONSOLE_POSITION_LEFT);
      if Handle <> INVALID_HANDLE_VALUE then
       begin
        {Move to Bottom Left}
        if ConsoleWindowSetPosition(Handle,CONSOLE_POSITION_BOTTOMLEFT) <> ERROR_SUCCESS then Exit;
       end;

      {Get Right}
      Handle:=ConsoleWindowFind(Console,CONSOLE_POSITION_RIGHT);
      if Handle <> INVALID_HANDLE_VALUE then
       begin
        {Move to Bottom Right}
        if ConsoleWindowSetPosition(Handle,CONSOLE_POSITION_BOTTOMRIGHT) <> ERROR_SUCCESS then Exit;
       end;
     end;
    CONSOLE_POSITION_BOTTOM:begin
      {Fail on bottom positions}
      if ConsoleWindowFind(Console,CONSOLE_POSITION_BOTTOMLEFT) <> INVALID_HANDLE_VALUE then Exit;
      if ConsoleWindowFind(Console,CONSOLE_POSITION_BOTTOMRIGHT) <> INVALID_HANDLE_VALUE then Exit;

      {Get Full}
      Handle:=ConsoleWindowFind(Console,CONSOLE_POSITION_FULL);
      if Handle <> INVALID_HANDLE_VALUE then
       begin
        {Move to Top}
        if ConsoleWindowSetPosition(Handle,CONSOLE_POSITION_TOP) <> ERROR_SUCCESS then Exit;
       end;

      {Get Left}
      Handle:=ConsoleWindowFind(Console,CONSOLE_POSITION_LEFT);
      if Handle <> INVALID_HANDLE_VALUE then
       begin
        {Move to Top Left}
        if ConsoleWindowSetPosition(Handle,CONSOLE_POSITION_TOPLEFT) <> ERROR_SUCCESS then Exit;
       end;

      {Get Right}
      Handle:=ConsoleWindowFind(Console,CONSOLE_POSITION_RIGHT);
      if Handle <> INVALID_HANDLE_VALUE then
       begin
        {Move to Top Right}
        if ConsoleWindowSetPosition(Handle,CONSOLE_POSITION_TOPRIGHT) <> ERROR_SUCCESS then Exit;
       end;
     end;
    CONSOLE_POSITION_LEFT:begin
      {Fail on left positions}
      if ConsoleWindowFind(Console,CONSOLE_POSITION_TOPLEFT) <> INVALID_HANDLE_VALUE then Exit;
      if ConsoleWindowFind(Console,CONSOLE_POSITION_BOTTOMLEFT) <> INVALID_HANDLE_VALUE then Exit;

      {Get Full}
      Handle:=ConsoleWindowFind(Console,CONSOLE_POSITION_FULL);
      if Handle <> INVALID_HANDLE_VALUE then
       begin
        {Move to Right}
        if ConsoleWindowSetPosition(Handle,CONSOLE_POSITION_RIGHT) <> ERROR_SUCCESS then Exit;
       end;

      {Get Top}
      Handle:=ConsoleWindowFind(Console,CONSOLE_POSITION_TOP);
      if Handle <> INVALID_HANDLE_VALUE then
       begin
        {Move to Top Right}
        if ConsoleWindowSetPosition(Handle,CONSOLE_POSITION_TOPRIGHT) <> ERROR_SUCCESS then Exit;
       end;

      {Get Bottom}
      Handle:=ConsoleWindowFind(Console,CONSOLE_POSITION_BOTTOM);
      if Handle <> INVALID_HANDLE_VALUE then
       begin
        {Move to Bottom Right}
        if ConsoleWindowSetPosition(Handle,CONSOLE_POSITION_BOTTOMRIGHT) <> ERROR_SUCCESS then Exit;
       end;
     end;
    CONSOLE_POSITION_RIGHT:begin
      {Fail on right positions}
      if ConsoleWindowFind(Console,CONSOLE_POSITION_TOPRIGHT) <> INVALID_HANDLE_VALUE then Exit;
      if ConsoleWindowFind(Console,CONSOLE_POSITION_BOTTOMRIGHT) <> INVALID_HANDLE_VALUE then Exit;

      {Get Full}
      Handle:=ConsoleWindowFind(Console,CONSOLE_POSITION_FULL);
      if Handle <> INVALID_HANDLE_VALUE then
       begin
        {Move to Left}
        if ConsoleWindowSetPosition(Handle,CONSOLE_POSITION_LEFT) <> ERROR_SUCCESS then Exit;
       end;

      {Get Top}
      Handle:=ConsoleWindowFind(Console,CONSOLE_POSITION_TOP);
      if Handle <> INVALID_HANDLE_VALUE then
       begin
        {Move to Top Left}
        if ConsoleWindowSetPosition(Handle,CONSOLE_POSITION_TOPLEFT) <> ERROR_SUCCESS then Exit;
       end;

      {Get Bottom}
      Handle:=ConsoleWindowFind(Console,CONSOLE_POSITION_BOTTOM);
      if Handle <> INVALID_HANDLE_VALUE then
       begin
        {Move to Bottom Left}
        if ConsoleWindowSetPosition(Handle,CONSOLE_POSITION_BOTTOMLEFT) <> ERROR_SUCCESS then Exit;
       end;
     end;
    CONSOLE_POSITION_TOPLEFT:begin
      {Get Full}
      Handle:=ConsoleWindowFind(Console,CONSOLE_POSITION_FULL);
      if Handle <> INVALID_HANDLE_VALUE then
       begin
        {Move to Right}
        if ConsoleWindowSetPosition(Handle,CONSOLE_POSITION_RIGHT) <> ERROR_SUCCESS then Exit;
       end;

      {Get Top}
      Handle:=ConsoleWindowFind(Console,CONSOLE_POSITION_TOP);
      if Handle <> INVALID_HANDLE_VALUE then
       begin
        {Move to Top Right}
        if ConsoleWindowSetPosition(Handle,CONSOLE_POSITION_TOPRIGHT) <> ERROR_SUCCESS then Exit;
       end;

      {Get Left}
      Handle:=ConsoleWindowFind(Console,CONSOLE_POSITION_LEFT);
      if Handle <> INVALID_HANDLE_VALUE then
       begin
        {Move to Bottom Left}
        if ConsoleWindowSetPosition(Handle,CONSOLE_POSITION_BOTTOMLEFT) <> ERROR_SUCCESS then Exit;
       end;
     end;
    CONSOLE_POSITION_TOPRIGHT:begin
      {Get Full}
      Handle:=ConsoleWindowFind(Console,CONSOLE_POSITION_FULL);
      if Handle <> INVALID_HANDLE_VALUE then
       begin
        {Move to Left}
        if ConsoleWindowSetPosition(Handle,CONSOLE_POSITION_LEFT) <> ERROR_SUCCESS then Exit;
       end;

      {Get Top}
      Handle:=ConsoleWindowFind(Console,CONSOLE_POSITION_TOP);
      if Handle <> INVALID_HANDLE_VALUE then
       begin
        {Move to Top Left}
        if ConsoleWindowSetPosition(Handle,CONSOLE_POSITION_TOPLEFT) <> ERROR_SUCCESS then Exit;
       end;

      {Get Right}
      Handle:=ConsoleWindowFind(Console,CONSOLE_POSITION_RIGHT);
      if Handle <> INVALID_HANDLE_VALUE then
       begin
        {Move to Bottom Right}
        if ConsoleWindowSetPosition(Handle,CONSOLE_POSITION_BOTTOMRIGHT) <> ERROR_SUCCESS then Exit;
       end;
     end;
    CONSOLE_POSITION_BOTTOMLEFT:begin
      {Get Full}
      Handle:=ConsoleWindowFind(Console,CONSOLE_POSITION_FULL);
      if Handle <> INVALID_HANDLE_VALUE then
       begin
        {Move to Right}
        if ConsoleWindowSetPosition(Handle,CONSOLE_POSITION_RIGHT) <> ERROR_SUCCESS then Exit;
       end;

      {Get Bottom}
      Handle:=ConsoleWindowFind(Console,CONSOLE_POSITION_BOTTOM);
      if Handle <> INVALID_HANDLE_VALUE then
       begin
        {Move to Bottom Right}
        if ConsoleWindowSetPosition(Handle,CONSOLE_POSITION_BOTTOMRIGHT) <> ERROR_SUCCESS then Exit;
       end;

      {Get Left}
      Handle:=ConsoleWindowFind(Console,CONSOLE_POSITION_LEFT);
      if Handle <> INVALID_HANDLE_VALUE then
       begin
        {Move to Top Left}
        if ConsoleWindowSetPosition(Handle,CONSOLE_POSITION_TOPLEFT) <> ERROR_SUCCESS then Exit;
       end;
     end;
    CONSOLE_POSITION_BOTTOMRIGHT:begin
      {Get Full}
      Handle:=ConsoleWindowFind(Console,CONSOLE_POSITION_FULL);
      if Handle <> INVALID_HANDLE_VALUE then
       begin
        {Move to Left}
        if ConsoleWindowSetPosition(Handle,CONSOLE_POSITION_LEFT) <> ERROR_SUCCESS then Exit;
       end;

      {Get Bottom}
      Handle:=ConsoleWindowFind(Console,CONSOLE_POSITION_BOTTOM);
      if Handle <> INVALID_HANDLE_VALUE then
       begin
        {Move to Bottom Left}
        if ConsoleWindowSetPosition(Handle,CONSOLE_POSITION_BOTTOMLEFT) <> ERROR_SUCCESS then Exit;
       end;

      {Get Right}
      Handle:=ConsoleWindowFind(Console,CONSOLE_POSITION_RIGHT);
      if Handle <> INVALID_HANDLE_VALUE then
       begin
        {Move to Top Right}
        if ConsoleWindowSetPosition(Handle,CONSOLE_POSITION_TOPRIGHT) <> ERROR_SUCCESS then Exit;
       end;
     end;
   end;
  end;

 {Get Position}
 if ConsoleDeviceGetPosition(Console,Position,X1,Y1,X2,Y2) <> ERROR_SUCCESS then Exit;

 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit;

 if MutexLock(Console.Lock) = ERROR_SUCCESS then
  begin
   try
    {Create Window}
    Window:=PConsoleWindow(AllocMem(Size));
    if Window = nil then Exit;

    {Update Window}
    Window.Signature:=WINDOW_SIGNATURE;
    Window.Position:=Position;
    Window.WindowState:=State;
    Window.WindowMode:=Mode;
    Window.WindowFlags:=WINDOW_FLAG_NONE;
    Window.X1:=X1;
    Window.Y1:=Y1;
    Window.X2:=X2;
    Window.Y2:=Y2;
    Window.Width:=0;
    Window.Height:=0;
    Window.OffsetX:=0;
    Window.OffsetY:=0;
    Window.MinX:=1;
    Window.MinY:=1;
    Window.MaxX:=0;
    Window.MaxY:=0;
    Window.X:=1;
    Window.Y:=1;
    Window.Cols:=0;
    Window.Rows:=0;
    Window.Format:=Console.Format;
    Window.Forecolor:=WINDOW_DEFAULT_FORECOLOR;
    Window.Backcolor:=WINDOW_DEFAULT_BACKCOLOR;
    Window.Borderwidth:=WINDOW_DEFAULT_BORDERWIDTH;
    Window.Bordercolor:=WINDOW_DEFAULT_BORDERCOLOR;
    {Font}
    Window.Font:=Font;
    Window.FontWidth:=0;
    Window.FontHeight:=0;
    {Cursor}
    Window.CursorX:=1;
    Window.CursorY:=1;
    Window.CursorMode:=CURSOR_MODE_INSERT;
    Window.CursorBlink:=True;
    Window.CursorState:=CURSOR_STATE_OFF;
    Window.CursorShape:=CURSOR_SHAPE_LINE;
    Window.CursorReverse:=False;
    Window.CursorForecolor:=COLOR_NONE;
    Window.CursorBackcolor:=COLOR_NONE;
    {Caret}
    Window.CaretX:=0;
    Window.CaretY:=0;
    Window.CaretHandle:=INVALID_HANDLE_VALUE;
    {Driver}
    Window.Lock:=INVALID_HANDLE_VALUE;
    Window.Console:=Console;

    {Setup Flags}
    if Position = CONSOLE_POSITION_FULLSCREEN then Window.WindowFlags:=Window.WindowFlags or WINDOW_FLAG_FULLSCREEN;
    if Console.ConsoleMode = CONSOLE_MODE_CHARACTER then Window.WindowFlags:=Window.WindowFlags or WINDOW_FLAG_CHARACTER;
    if (CONSOLE_LINE_WRAP) or ((Console.Device.DeviceFlags and CONSOLE_FLAG_LINE_WRAP) <> 0) then Window.WindowFlags:=Window.WindowFlags or WINDOW_FLAG_LINE_WRAP;
    if (CONSOLE_AUTO_SCROLL) or ((Console.Device.DeviceFlags and CONSOLE_FLAG_AUTO_SCROLL) <> 0) then Window.WindowFlags:=Window.WindowFlags or WINDOW_FLAG_AUTO_SCROLL;
    if (CONSOLE_FOCUS_CURSOR) or ((Console.Device.DeviceFlags and CONSOLE_FLAG_FOCUS_CARET) <> 0) then Window.WindowFlags:=Window.WindowFlags or WINDOW_FLAG_FOCUS_CURSOR;

    {Check Border}
    if Position = CONSOLE_POSITION_FULLSCREEN then Window.Borderwidth:=0; //To do //Would this be better based on another criteria ?

    {Get Font}
    if Window.Font = INVALID_HANDLE_VALUE then Window.Font:=ConsoleWindowGetDefaultFont;
    if Window.Font = INVALID_HANDLE_VALUE then Window.Font:=Console.Font;
    if Window.Font = INVALID_HANDLE_VALUE then Window.Font:=FontGetDefault;
    if Window.Font = INVALID_HANDLE_VALUE then
     begin
      if DEVICE_LOG_ENABLED then DeviceLogError(nil,'Failed to get font for console window');
      {Free Window}
      FreeMem(Window);
      Exit;
     end;

    {Get Font Width / Height}
    Window.FontWidth:=FontGetWidth(Window.Font);
    Window.FontHeight:=FontGetHeight(Window.Font);
    if (Window.FontWidth = 0) or (Window.FontHeight = 0) then
     begin
      if DEVICE_LOG_ENABLED then DeviceLogError(nil,'Failed to get font width and height for console window');
      {Free Window}
      FreeMem(Window);
      Exit;
     end;
    Window.FontWidth:=Window.FontWidth * Console.FontRatio;
    if Window.FontWidth = 0 then Window.FontWidth:=1;
    Window.FontHeight:=Window.FontHeight * Console.FontRatio;
    if Window.FontHeight = 0 then Window.FontHeight:=1;

    {Get Width / Height}
    Window.Width:=(((Window.X2 - Window.X1) + 1) - (2 * Window.Borderwidth)) div Window.FontWidth;
    Window.Height:=(((Window.Y2 - Window.Y1) + 1) - (2 * Window.Borderwidth)) div Window.FontHeight;
    if (Window.Width < 2) or (Window.Height < 2) then
     begin
      if DEVICE_LOG_ENABLED then DeviceLogError(nil,'Failed to get window width and height for console window');
      {Free Window}
      FreeMem(Window);
      Exit;
     end;

    {Check Font Ratio}
    if Console.FontRatio > 0 then
     begin
      {Get RemainX,Y}
      RemainX:=(((Window.X2 - Window.X1) + 1) - (2 * Window.Borderwidth)) mod Window.FontWidth;
      RemainY:=(((Window.Y2 - Window.Y1) + 1) - (2 * Window.Borderwidth)) mod Window.FontHeight;

      {Check Remain X}
      if RemainX < 4 then
       begin
        {Decrease Width}
        Dec(Window.Width);
        Inc(RemainX,Window.FontWidth);
       end;
      {Get Offset X}
      Window.OffsetX:=(RemainX div 2) + (RemainX mod 2);

      {Check Remain Y}
      if RemainY < 4 then
       begin
        {Decrease Height}
        Dec(Window.Height);
        Inc(RemainY,Window.FontHeight);
       end;
      {Get Offset Y}
      Window.OffsetY:=(RemainY div 2) + (RemainY mod 2);
     end;

    {Get MaxX,Y}
    Window.MaxX:=Window.Width;
    Window.MaxY:=Window.Height;

    {Get Cols / Rows}
    Window.Cols:=Window.Width;
    Window.Rows:=Window.Height;

    {Get CaretX / CaretY}
    Window.CaretX:=Window.X1 + Window.Borderwidth + Window.OffsetX + ((Window.MinX - 1) * Window.FontWidth) + ((Window.CursorX - 1) * Window.FontWidth);
    Window.CaretY:=Window.Y1 + Window.Borderwidth + Window.OffsetY + ((Window.MinY - 1) * Window.FontHeight) + ((Window.CursorY - 1) * Window.FontHeight);

    {Create Lock}
    Window.Lock:=MutexCreateEx(False,MUTEX_DEFAULT_SPINCOUNT,MUTEX_FLAG_RECURSIVE);
    if Window.Lock = INVALID_HANDLE_VALUE then
     begin
      if DEVICE_LOG_ENABLED then DeviceLogError(nil,'Failed to create lock for console window');
      {Free Window}
      FreeMem(Window);
      Exit;
     end;

    {Insert Window}
    if CriticalSectionLock(Console.WindowLock) = ERROR_SUCCESS then
     begin
      try
       {Link Window}
       if Console.WindowFirst = nil then
        begin
         Console.WindowFirst:=Window;
        end
       else
        begin
         Window.Next:=Console.WindowFirst;
         Console.WindowFirst.Prev:=Window;
         Console.WindowFirst:=Window;
        end;

       {Increment Count}
       Inc(Console.WindowCount);

       {Check Default}
       if (Console.WindowDefault = INVALID_HANDLE_VALUE) and (Default) then
        begin
         {Set Default}
         Console.WindowDefault:=TWindowHandle(Window);
        end;

       {Check Visible}
       if State = WINDOW_STATE_VISIBLE then
        begin
         {Set Active}
         if Console.WindowActive = nil then Console.WindowActive:=Window;

         {Draw Window}
         ConsoleDeviceDrawWindow(Console,TWindowHandle(Window),WINDOW_DRAW_FLAG_ALL);
        end;

       {Return Result}
       Result:=TWindowHandle(Window);
      finally
       CriticalSectionUnlock(Console.WindowLock);
      end;
     end
    else
     begin
      {Free Lock}
      if Window.Lock <> INVALID_HANDLE_VALUE then
       begin
        MutexDestroy(Window.Lock);
       end;

      {Free Window}
      FreeMem(Window);
     end;
   finally
    MutexUnlock(Console.Lock);
   end;
  end;
end;

{==============================================================================}

function ConsoleWindowDestroy(Handle:TWindowHandle):LongWord;
{Close and Destroy an existing console window}
{Handle: The handle of the window to destroy}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Unlock:Boolean;
 Active:TWindowHandle;
 Prev:PConsoleWindow;
 Next:PConsoleWindow;
 Window:PConsoleWindow;
 Console:PConsoleDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console Window Destroy');
 {$ENDIF}

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;

 {Get Console}
 Console:=Window.Console;
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Remove Window}
 if CriticalSectionLock(Console.WindowLock) = ERROR_SUCCESS then
  begin
   try
    {Check Window}
    Result:=ERROR_NOT_FOUND;
    if ConsoleWindowCheck(Console,Window) <> Window then Exit;

    {Lock Window}
    if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;
    try
     Unlock:=True;

     {Check State}
     if Window.WindowState = WINDOW_STATE_VISIBLE then
      begin
       {Update Caret}
       ConsoleDeviceUpdateCaretEx(Window.Console,Window.CaretHandle,Window.CaretX,Window.CaretY,Window.CursorForecolor,Window.CursorBackcolor,False,Window.CursorBlink,Window.CursorReverse);

       {Set State}
       Window.WindowState:=WINDOW_STATE_INVISIBLE;

       {Draw Window}
       Result:=ConsoleDeviceDrawWindow(Window.Console,TWindowHandle(Window),WINDOW_DRAW_FLAG_ALL);
       if Result <> ERROR_SUCCESS then Exit;
      end;

     {Delete Caret}
     if Window.CaretHandle <> INVALID_HANDLE_VALUE then
      begin
       ConsoleDeviceDeleteCaret(Window.Console,Window.CaretHandle);
       Window.CaretHandle:=INVALID_HANDLE_VALUE;
      end;

     {Unlink Window}
     Prev:=Window.Prev;
     Next:=Window.Next;
     if Prev = nil then
      begin
       Console.WindowFirst:=Next;
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
     Dec(Console.WindowCount);

     {Check Default}
     if Console.WindowDefault = Handle then
      begin
       if Next <> nil then
        begin
         Console.WindowDefault:=TWindowHandle(Next);
        end
       else if Prev <> nil then
        begin
         Console.WindowDefault:=TWindowHandle(Prev);
        end
       else
        begin
         Console.WindowDefault:=INVALID_HANDLE_VALUE;
        end;
      end;

     {Check Active}
     if Console.WindowActive = Window then
      begin
       {Get Active}
       Active:=ConsoleWindowNext(Console,True);
       if Active <> INVALID_HANDLE_VALUE then
        begin
         {Set Active}
         Console.WindowActive:=PConsoleWindow(Active);

         {Lock Window}
         if MutexLock(Console.WindowActive.Lock) = ERROR_SUCCESS then
          begin
           {Draw Active}
           ConsoleDeviceDrawWindow(Console,Active,WINDOW_DRAW_FLAG_BORDER);

           {Unlock Window}
           MutexUnlock(Console.WindowActive.Lock);
          end;
        end
       else
        begin
         {Clear Active}
         Console.WindowActive:=nil;
        end;
      end;

     {Update Window}
     Window.Signature:=0;
     Window.Console:=nil;

     {Unlock Window}
     MutexUnlock(Window.Lock);
     Unlock:=False;

     {Free Lock}
     if Window.Lock <> INVALID_HANDLE_VALUE then
      begin
       MutexDestroy(Window.Lock);
      end;

     {Free Window}
     FreeMem(Window);

     {Return Result}
     Result:=ERROR_SUCCESS;
    finally
     {Unlock Window}
     if Unlock then MutexUnlock(Window.Lock);
    end;
   finally
    CriticalSectionUnlock(Console.WindowLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function ConsoleWindowShow(Handle:TWindowHandle):LongWord;
{Make an existing console window visible and show it on screen}
{Handle: The handle of the window to show}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Window:PConsoleWindow;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console Window Show');
 {$ENDIF}

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;

 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;
 try
  {Check Console}
  if Window.Console = nil then Exit;

  if Window.WindowState = WINDOW_STATE_INVISIBLE then
   begin
    {Set State}
    Window.WindowState:=WINDOW_STATE_VISIBLE;

    {Draw Window}
    Result:=ConsoleDeviceDrawWindow(Window.Console,TWindowHandle(Window),WINDOW_DRAW_FLAG_ALL);
    if Result <> ERROR_SUCCESS then Exit;

    {Check Flag}
    if ((Window.WindowFlags and WINDOW_FLAG_FOCUS_CURSOR) = 0) or (Window.Console.WindowActive = Window) then
     begin
      {Update Caret}
      ConsoleDeviceUpdateCaretEx(Window.Console,Window.CaretHandle,Window.CaretX,Window.CaretY,Window.CursorForecolor,Window.CursorBackcolor,(Window.CursorState = CURSOR_STATE_ON),Window.CursorBlink,Window.CursorReverse);
     end;
   end;

  {Return Result}
  Result:=ERROR_SUCCESS;
 finally
  {Unlock Window}
  MutexUnlock(Window.Lock);
 end;
end;

{==============================================================================}

function ConsoleWindowHide(Handle:TWindowHandle):LongWord;
{Make an existing console window invisible and hide it on screen}
{Handle: The handle of the window to hide}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Window:PConsoleWindow;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console Window Hide');
 {$ENDIF}

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;

 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;
 try
  {Check Console}
  if Window.Console = nil then Exit;

  if Window.WindowState = WINDOW_STATE_VISIBLE then
   begin
    {Update Caret}
    ConsoleDeviceUpdateCaretEx(Window.Console,Window.CaretHandle,Window.CaretX,Window.CaretY,Window.CursorForecolor,Window.CursorBackcolor,False,Window.CursorBlink,Window.CursorReverse);

    {Set State}
    Window.WindowState:=WINDOW_STATE_INVISIBLE;

    {Draw Window}
    Result:=ConsoleDeviceDrawWindow(Window.Console,TWindowHandle(Window),WINDOW_DRAW_FLAG_ALL);
    if Result <> ERROR_SUCCESS then Exit;
   end;

  {Return Result}
  Result:=ERROR_SUCCESS;
 finally
  {Unlock Window}
  MutexUnlock(Window.Lock);
 end;
end;

{==============================================================================}

function ConsoleWindowActivate(Handle:TWindowHandle):LongWord;
{Make an existing console window the active window}
{Handle: The handle of the window to activate}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Window:PConsoleWindow;
 Current:PConsoleWindow;
 Console:PConsoleDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console Window Activate');
 {$ENDIF}

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;

 {Get Console}
 Console:=Window.Console;
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(Console.WindowLock) = ERROR_SUCCESS then
  begin
   try
    {Check Window}
    Result:=ERROR_NOT_FOUND;
    if ConsoleWindowCheck(Console,Window) <> Window then Exit;

    {Lock Window}
    if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;
    try
     {Get Current}
     Current:=Console.WindowActive;

     {Set Active}
     Console.WindowActive:=Window;

     {Check Current}
     if Current <> nil then
      begin
       {Lock Window}
       if MutexLock(Current.Lock) = ERROR_SUCCESS then
        begin
         {Check Visible}
         if Current.WindowState = WINDOW_STATE_VISIBLE then
          begin
           {Check Flag}
           if (Current.WindowFlags and WINDOW_FLAG_FOCUS_CURSOR) <> 0 then
            begin
             {Update Caret}
             ConsoleDeviceUpdateCaretEx(Current.Console,Current.CaretHandle,Current.CaretX,Current.CaretY,Current.CursorForecolor,Current.CursorBackcolor,False,Current.CursorBlink,Window.CursorReverse);
            end;

           {Draw Current}
           ConsoleDeviceDrawWindow(Console,TWindowHandle(Current),WINDOW_DRAW_FLAG_BORDER);
          end;

         {Unlock Window}
         MutexUnlock(Current.Lock);
        end;
      end;

     {Check Visible}
     if Window.WindowState = WINDOW_STATE_VISIBLE then
      begin
       {Draw Active}
       ConsoleDeviceDrawWindow(Console,TWindowHandle(Window),WINDOW_DRAW_FLAG_BORDER);

       {Check Flag}
       if (Window.WindowFlags and WINDOW_FLAG_FOCUS_CURSOR) <> 0 then
        begin
         {Update Caret}
         ConsoleDeviceUpdateCaretEx(Window.Console,Window.CaretHandle,Window.CaretX,Window.CaretY,Window.CursorForecolor,Window.CursorBackcolor,(Window.CursorState = CURSOR_STATE_ON),Window.CursorBlink,Window.CursorReverse);
        end;
      end;

     {Return Result}
     Result:=ERROR_SUCCESS;
    finally
     {Unlock Window}
     MutexUnlock(Window.Lock);
    end;
   finally
    CriticalSectionUnlock(Console.WindowLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function ConsoleWindowDeactivate(Handle:TWindowHandle):LongWord;
{Make an existing console window inactive}
{Handle: The handle of the window to deactivate}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: On success there will be no active window set}
var
 Window:PConsoleWindow;
 Console:PConsoleDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console Window Deactivate');
 {$ENDIF}

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;

 {Get Console}
 Console:=Window.Console;
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(Console.WindowLock) = ERROR_SUCCESS then
  begin
   try
    {Check Window}
    Result:=ERROR_NOT_FOUND;
    if ConsoleWindowCheck(Console,Window) <> Window then Exit;

    {Lock Window}
    if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;
    try
     {Check Current}
     if Console.WindowActive <> Window then Exit;

     {Clear Active}
     Console.WindowActive:=nil;

     {Check Visible}
     if Window.WindowState = WINDOW_STATE_VISIBLE then
      begin
       {Check Flag}
       if (Window.WindowFlags and WINDOW_FLAG_FOCUS_CURSOR) <> 0 then
        begin
         {Update Caret}
         ConsoleDeviceUpdateCaretEx(Window.Console,Window.CaretHandle,Window.CaretX,Window.CaretY,Window.CursorForecolor,Window.CursorBackcolor,False,Window.CursorBlink,Window.CursorReverse);
        end;

       {Draw Window}
       ConsoleDeviceDrawWindow(Console,TWindowHandle(Window),WINDOW_DRAW_FLAG_BORDER);
      end;

     {Return Result}
     Result:=ERROR_SUCCESS;
    finally
     {Unlock Window}
     MutexUnlock(Window.Lock);
    end;
   finally
    CriticalSectionUnlock(Console.WindowLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function ConsoleWindowNext(Console:PConsoleDevice;Visible:Boolean):TWindowHandle;
{Get the next console window starting with the active window}
{Console: The console device to change the active window on}
{Visible: If true only return windows that are visible}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Start:Boolean;
 Window:PConsoleWindow;
 Current:PConsoleWindow;
begin
 {}
 Result:=INVALID_HANDLE_VALUE;

 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console Window Next');
 {$ENDIF}

 {Acquire the Lock}
 if CriticalSectionLock(Console.WindowLock) = ERROR_SUCCESS then
  begin
   try
    Start:=False;

    {Get Current}
    Current:=Console.WindowActive;

    {Get Window}
    Window:=Current;
    if Window = nil then
     begin
      Start:=True;
      Window:=Console.WindowFirst;
     end;

    {Find Window}
    while Window <> nil do
     begin
      {Check Window}
      if (Window <> Current) and ((not Visible) or (Window.WindowState = WINDOW_STATE_VISIBLE)) then
       begin
        {Return Result}
        Result:=TWindowHandle(Window);
        Exit;
       end;

      {Get Next}
      Window:=Window.Next;

      {Check Start}
      if not(Start) and (Window = nil) and (Current <> nil) then
       begin
        Start:=True;
        Window:=Console.WindowFirst;
       end;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(Console.WindowLock);
   end;
  end;
end;

{==============================================================================}

function ConsoleWindowPrevious(Console:PConsoleDevice;Visible:Boolean):TWindowHandle;
{Get the previous console window starting with the active window}
{Console: The console device to change the active window on}
{Visible: If true only return windows that are visible}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Start:Boolean;
 Window:PConsoleWindow;
 Current:PConsoleWindow;
begin
 {}
 Result:=INVALID_HANDLE_VALUE;

 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console Window Previous');
 {$ENDIF}

 {Acquire the Lock}
 if CriticalSectionLock(Console.WindowLock) = ERROR_SUCCESS then
  begin
   try
    Start:=False;

    {Get Current}
    Current:=Console.WindowActive;

    {Get Window}
    Window:=Current;
    if Window = nil then
     begin
      Start:=True;
      Window:=Console.WindowFirst;

      {Find Last}
      while (Window <> nil) and (Window.Next <> nil) do
       begin
        {Get Next}
        Window:=Window.Next;
       end;
     end;

    {Find Window}
    while Window <> nil do
     begin
      {Check Window}
      if (Window <> Current) and ((not Visible) or (Window.WindowState = WINDOW_STATE_VISIBLE)) then
       begin
        {Return Result}
        Result:=TWindowHandle(Window);
        Exit;
       end;

      {Get Previous}
      Window:=Window.Prev;

      {Check Start}
      if not(Start) and (Window = nil) and (Current <> nil) then
       begin
        Start:=True;
        Window:=Console.WindowFirst;
       end;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(Console.WindowLock);
   end;
  end;
end;

{==============================================================================}

function ConsoleWindowAt(Console:PConsoleDevice;X,Y:LongWord;Visible:Boolean):TWindowHandle;
{Find the console window that X and Y coordinates are within}
{Console: The console device to find the window on}
{X: The X coordinate to find the window for}
{Y: The Y coordinate to find the window for}
{Visible: If true only return windows that are visible}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: For Text Console functions, X and Y are based on character columns not screen pixels}
var
 Window:PConsoleWindow;
begin
 {}
 Result:=INVALID_HANDLE_VALUE;

 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console Window At');
 {$ENDIF}

 {Acquire the Lock}
 if CriticalSectionLock(Console.WindowLock) = ERROR_SUCCESS then
  begin
   try
    {Get Window}
    Window:=Console.WindowFirst;
    while Window <> nil do
     begin
      {Check Visible}
      if (not Visible) or (Window.WindowState = WINDOW_STATE_VISIBLE) then
       begin
        {Check Coordinates}
        if PtInRect(Rect(Window.X1,Window.Y1,Window.X2,Window.Y2),Point(X,Y)) then
         begin
          Result:=TWindowHandle(Window);
          Exit;
         end;
       end;

      {Get Next}
      Window:=Window.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(Console.WindowLock);
   end;
  end;
end;

{==============================================================================}

function ConsoleWindowFind(Console:PConsoleDevice;Position:LongWord):TWindowHandle;
{Find an existing console window in the position specified}
{Console: The console device to find the window on}
{Position: The window position to find (eg CONSOLE_POSITION_FULL)}
{Return: The handle of the existing window or INVALID_HANDLE_VALUE if not found}
var
 Window:PConsoleWindow;
begin
 {}
 Result:=INVALID_HANDLE_VALUE;

 {Check Position}
 {if Position < CONSOLE_POSITION_FULL then Exit;}
 if (Position <> CONSOLE_POSITION_FULLSCREEN) and (Position > CONSOLE_POSITION_BOTTOMRIGHT) then Exit;

 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console Window Find');
 {$ENDIF}

 {Acquire the Lock}
 if CriticalSectionLock(Console.WindowLock) = ERROR_SUCCESS then
  begin
   try
    {Get Window}
    Window:=Console.WindowFirst;
    while Window <> nil do
     begin
      {Check Position}
      if Window.Position = Position then
       begin
        Result:=TWindowHandle(Window);
        Exit;
       end;

      {Get Next}
      Window:=Window.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(Console.WindowLock);
   end;
  end;
end;

{==============================================================================}

function ConsoleWindowEnumerate(Console:PConsoleDevice;Callback:TConsoleWindowEnumerate;Data:Pointer):LongWord;
{Enumerate existing console windows on the specified console device}
{Console: The console device to enumerate windows for}
{Callback: The function to call for each window enumerated}
{Data: A pointer to private data to be passed to the callback (Optional)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Window:PConsoleWindow;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Callback}
 if not Assigned(Callback) then Exit;

 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console Window Enumerate');
 {$ENDIF}

 {Acquire the Lock}
 if CriticalSectionLock(Console.WindowLock) = ERROR_SUCCESS then
  begin
   try
    {Get Window}
    Window:=Console.WindowFirst;
    while Window <> nil do
     begin
      if Callback(Console,TWindowHandle(Window),Data) <> ERROR_SUCCESS then Exit;

      {Get Next}
      Window:=Window.Next;
     end;

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    CriticalSectionUnlock(Console.WindowLock);
   end;
  end;
end;

{==============================================================================}

function ConsoleWindowCheckFlag(Handle:TWindowHandle;Flag:LongWord):Boolean;
{Check an existing console window to determine if a flag is set or not}
{Handle: The handle of the window to check}
{Flag: The window flag to check for (eg WINDOW_FLAG_LINE_WRAP)}
{Return: True if the flag is set, False if not set}
var
 Window:PConsoleWindow;
begin
 {}
 Result:=False;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;

 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;

 {Check Flag}
 Result:=((Window.WindowFlags and Flag) <> 0);

 {Unlock Window}
 MutexUnlock(Window.Lock);
end;

{==============================================================================}

function ConsoleWindowUpdateFlag(Handle:TWindowHandle;Flag:LongWord;Clear:Boolean):LongWord;
{Set or clear a flag on an existing console window}
{Handle: The handle of the window to set or clear the flag on}
{Flag: The window flag to set or clear (eg WINDOW_FLAG_LINE_WRAP)}
{Clear: If true clear the flag, else set it}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Window:PConsoleWindow;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Flag}
 if (Flag and WINDOW_FLAG_INTERNAL) <> 0 then Exit;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;

 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;

 if Clear then
  begin
   {Clear Flag}
   Window.WindowFlags:=Window.WindowFlags and not(Flag);
  end
 else
  begin
   {Set Flag}
   Window.WindowFlags:=Window.WindowFlags or Flag;
  end;

 {Unlock Window}
 MutexUnlock(Window.Lock);

 {Return Result}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function ConsoleWindowGetMode(Handle:TWindowHandle):LongWord;
{Get the window mode of an existing console window}
{Handle: The handle of the window to get the mode for}
{Return: The window mode (eg WINDOW_MODE_TEXT)}
var
 Window:PConsoleWindow;
begin
 {}
 Result:=WINDOW_MODE_NONE;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;

 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;

 {Get Mode}
 Result:=Window.WindowMode;

 {Unlock Window}
 MutexUnlock(Window.Lock);
end;

{==============================================================================}

function ConsoleWindowGetState(Handle:TWindowHandle):LongWord;
{Get the window state of an existing console window}
{Handle: The handle of the window to get the state for}
{Return: The window state (eg WINDOW_STATE_INVISIBLE)}
var
 Window:PConsoleWindow;
begin
 {}
 Result:=WINDOW_STATE_INVISIBLE;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;

 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;

 {Get State}
 Result:=Window.WindowState;

 {Unlock Window}
 MutexUnlock(Window.Lock);
end;

{==============================================================================}

function ConsoleWindowGetPosition(Handle:TWindowHandle):LongWord;
{Get the position of an existing console window}
{Handle: The handle of the window to get the position for}
{Return: The window position (eg CONSOLE_POSITION_FULL)}
var
 Window:PConsoleWindow;
begin
 {}
 Result:=CONSOLE_POSITION_UNKNOWN;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;

 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;

 {Get Position}
 Result:=Window.Position;

 {Unlock Window}
 MutexUnlock(Window.Lock);
end;

{==============================================================================}

function ConsoleWindowSetPosition(Handle:TWindowHandle;Position:LongWord):LongWord;
{Set the position of an existing console window}
{Handle: The handle of the window to set the position for}
{Position: The new window position to set}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: The function will return ERROR_INVALID_PARAMETER if another window exists at the position}
var
 X1:LongWord;
 Y1:LongWord;
 X2:LongWord;
 Y2:LongWord;
 State:LongWord;
 RemainX:LongWord;
 RemainY:LongWord;
 Viewport:Boolean;
 Window:PConsoleWindow;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Position}
 {if Position < CONSOLE_POSITION_FULL then Exit;}
 if (Position <> CONSOLE_POSITION_FULLSCREEN) and (Position > CONSOLE_POSITION_BOTTOMRIGHT) then Exit;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console Window Set Position');
 {$ENDIF}

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_TEXT then Exit;

 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;
 try
  {Check Console}
  if Window.Console = nil then Exit;

  {Check Position}
  if ConsoleWindowFind(Window.Console,Position) <> INVALID_HANDLE_VALUE then Exit;

  {Save State}
  State:=Window.WindowState;
  try
   {Update Caret}
   ConsoleDeviceUpdateCaretEx(Window.Console,Window.CaretHandle,Window.CaretX,Window.CaretY,Window.CursorForecolor,Window.CursorBackcolor,False,Window.CursorBlink,Window.CursorReverse);

   {Hide Window}
   Window.WindowState:=WINDOW_STATE_INVISIBLE;

   {Draw Window}
   if State = WINDOW_STATE_VISIBLE then ConsoleDeviceDrawWindow(Window.Console,TWindowHandle(Window),WINDOW_DRAW_FLAG_ALL);

   {Get Position}
   if ConsoleDeviceGetPosition(Window.Console,Position,X1,Y1,X2,Y2) <> ERROR_SUCCESS then Exit;

   {Check Viewport}
   Viewport:=False;
   if (Window.MinX <> 1) or (Window.MinY <> 1) or (Window.MaxX <> Window.Width) or (Window.MaxY <> Window.Height) then
    begin
     Viewport:=True;
    end;

   {Update Window}
   Window.X1:=X1;
   Window.Y1:=Y1;
   Window.X2:=X2;
   Window.Y2:=Y2;

   {Get Width / Height}
   Window.Width:=(((Window.X2 - Window.X1) + 1) - (2 * Window.Borderwidth)) div Window.FontWidth;
   Window.Height:=(((Window.Y2 - Window.Y1) + 1) - (2 * Window.Borderwidth)) div Window.FontHeight;

   {Check Font Ratio}
   if Window.Console.FontRatio > 0 then
    begin
     {Get RemainX,Y}
     RemainX:=(((Window.X2 - Window.X1) + 1) - (2 * Window.Borderwidth)) mod Window.FontWidth;
     RemainY:=(((Window.Y2 - Window.Y1) + 1) - (2 * Window.Borderwidth)) mod Window.FontHeight;

     {Check Remain X}
     if RemainX < 4 then
      begin
       {Decrease Width}
       Dec(Window.Width);
       Inc(RemainX,Window.FontWidth);
      end;
     {Get Offset X}
     Window.OffsetX:=(RemainX div 2) + (RemainX mod 2);

     {Check Remain Y}
     if RemainY < 4 then
      begin
       {Decrease Height}
       Dec(Window.Height);
       Inc(RemainY,Window.FontHeight);
      end;
     {Get Offset Y}
     Window.OffsetY:=(RemainY div 2) + (RemainY mod 2);
    end;

   if not Viewport then
    begin
     {Set MinX,Y / MaxX,Y}
     Window.MinX:=1;
     Window.MinY:=1;
     Window.MaxX:=Window.Width;
     Window.MaxY:=Window.Height;

     {Set Cols / Rows}
     Window.Cols:=Window.Width;
     Window.Rows:=Window.Height;
    end
   else
    begin
     {Get MinX,Y / MaxX,Y}
     if Window.MinX > Window.Width then Window.MinX:=1;
     if Window.MinY > Window.Height then Window.MinY:=1;
     if Window.MaxX > Window.Width then Window.MaxX:=Window.Width;
     if Window.MaxY > Window.Height then Window.MaxY:=Window.Height;

     {Get Cols / Rows}
     Window.Cols:=Window.MaxX - (Window.MinX - 1);
     Window.Rows:=Window.MaxY - (Window.MinY - 1);
    end;

   {Get X,Y / Cursor X,Y}
   if Window.X > Window.Cols then Window.X:=1;
   if Window.Y > Window.Rows then Window.Y:=1;
   if Window.CursorX > Window.Cols then Window.CursorX:=1;
   if Window.CursorY > Window.Rows then Window.CursorY:=1;

   {Get CaretX / CaretY}
   Window.CaretX:=Window.X1 + Window.Borderwidth + Window.OffsetX + ((Window.MinX - 1) * Window.FontWidth) + ((Window.CursorX - 1) * Window.FontWidth);
   Window.CaretY:=Window.Y1 + Window.Borderwidth + Window.OffsetY + ((Window.MinY - 1) * Window.FontHeight) + ((Window.CursorY - 1) * Window.FontHeight);

   {Return Result}
   Result:=ERROR_SUCCESS;
  finally
   {Restore State}
   Window.WindowState:=State;

   {Check Visible}
   if State = WINDOW_STATE_VISIBLE then
    begin
     {Draw Window}
     ConsoleDeviceDrawWindow(Window.Console,TWindowHandle(Window),WINDOW_DRAW_FLAG_ALL);

     {Check Flag}
     if ((Window.WindowFlags and WINDOW_FLAG_FOCUS_CURSOR) = 0) or (Window.Console.WindowActive = Window) then
      begin
       {Update Caret}
       ConsoleDeviceUpdateCaretEx(Window.Console,Window.CaretHandle,Window.CaretX,Window.CaretY,Window.CursorForecolor,Window.CursorBackcolor,(Window.CursorState = CURSOR_STATE_ON),Window.CursorBlink,Window.CursorReverse);
      end;
    end;
  end;
 finally
  {Unlock Window}
  MutexUnlock(Window.Lock);
 end;
end;

{==============================================================================}

function ConsoleWindowGetProperties(Handle:TWindowHandle;Properties:PWindowProperties):LongWord;
{Get the properties for the specified console window}
{Handle: The handle of the window to get the properties from}
{Properties: Pointer to a TWindowProperties structure to fill in}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Window:PConsoleWindow;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Properties}
 if Properties = nil then Exit;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console Window Get Properties');
 {$ENDIF}

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 {if Window.WindowMode <> WINDOW_MODE_TEXT then Exit;} {Allow any mode, other window classes should override if required}

 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;
 try
  {Get Properties}
  Properties.Position:=Window.Position;
  Properties.State:=Window.WindowState;
  Properties.Mode:=Window.WindowMode;
  Properties.Flags:=Window.WindowFlags;
  Properties.X1:=Window.X1;
  Properties.Y1:=Window.Y1;
  Properties.X2:=Window.X2;
  Properties.Y2:=Window.Y2;
  Properties.Width:=Window.Width;
  Properties.Height:=Window.Height;
  Properties.OffsetX:=Window.OffsetX;
  Properties.OffsetY:=Window.OffsetY;
  Properties.FontWidth:=Window.FontWidth;
  Properties.FontHeight:=Window.FontHeight;
  Properties.Borderwidth:=Window.Borderwidth;
  Properties.Font:=Window.Font;
  Properties.Console:=Window.Console;

  {Return Result}
  Result:=ERROR_SUCCESS;
 finally
  {Unlock Window}
  MutexUnlock(Window.Lock);
 end;
end;

{==============================================================================}

function ConsoleWindowGetMinX(Handle:TWindowHandle):LongWord;
{Get the current minimum X of the window viewport for an existing console window}
{Handle: The handle of the window to get MinX for}
{Return: The minimum X value for the current window viewport}

{Note: For Text Console functions, X is based on character columns not screen pixels}
var
 Window:PConsoleWindow;
begin
 {}
 Result:=0;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_TEXT then Exit;

 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;

 {Get MinX}
 Result:=Window.MinX;

 {Unlock Window}
 MutexUnlock(Window.Lock);
end;

{==============================================================================}

function ConsoleWindowGetMinY(Handle:TWindowHandle):LongWord;
{Get the current minimum Y of the window viewport for an existing console window}
{Handle: The handle of the window to get MinY for}
{Return: The minimum Y value for the current window viewport}

{Note: For Text Console functions, Y is based on character rows not screen pixels}
var
 Window:PConsoleWindow;
begin
 {}
 Result:=0;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_TEXT then Exit;

 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;

 {Get MinY}
 Result:=Window.MinY;

 {Unlock Window}
 MutexUnlock(Window.Lock);
end;

{==============================================================================}

function ConsoleWindowGetMaxX(Handle:TWindowHandle):LongWord;
{Get the current maximum X of the window viewport for an existing console window}
{Handle: The handle of the window to get MaxX for}
{Return: The maximum X value for the current window viewport}

{Note: For Text Console functions, X is based on character columns not screen pixels}
var
 Window:PConsoleWindow;
begin
 {}
 Result:=0;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_TEXT then Exit;

 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;

 {Get MaxX}
 Result:=Window.MaxX;

 {Unlock Window}
 MutexUnlock(Window.Lock);
end;

{==============================================================================}

function ConsoleWindowGetMaxY(Handle:TWindowHandle):LongWord;
{Get the current maximum Y of the window viewport for an existing console window}
{Handle: The handle of the window to get MaxY for}
{Return: The maximum Y value for the current window viewport}

{Note: For Text Console functions, Y is based on character rows not screen pixels}
var
 Window:PConsoleWindow;
begin
 {}
 Result:=0;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_TEXT then Exit;

 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;

 {Get MaxY}
 Result:=Window.MaxY;

 {Unlock Window}
 MutexUnlock(Window.Lock);
end;

{==============================================================================}

function ConsoleWindowGetRect(Handle:TWindowHandle):TConsoleRect; inline;
{Get the rectangle X1,Y1,X2,Y2 of the window viewport for an existing console window}
{Handle: The handle of the window to get the rectangle for}
{Return: The rectangle of the current window viewport}

{Note: For Text Console functions, Rect is based on character rows and columns not screen pixels}
begin
 {}
 ConsoleWindowGetViewport(Handle,Result.X1,Result.Y1,Result.X2,Result.Y2);
end;

{==============================================================================}

function ConsoleWindowSetRect(Handle:TWindowHandle;const ARect:TConsoleRect):LongWord; inline;
{Set the rectangle X1,Y1,X2,Y2 of the window viewport for an existing console window}
{Handle: The handle of the window to set the rectangle for}
{Rect: The rectangle to set for the window viewport}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: For Text Console functions, Rect is based on character rows and columns not screen pixels}
begin
 {}
 Result:=ConsoleWindowSetViewport(Handle,ARect.X1,ARect.Y1,ARect.X2,ARect.Y2);
end;

{==============================================================================}

function ConsoleWindowResetRect(Handle:TWindowHandle):LongWord; inline;
{Reset the window viewport for an existing console window to the maximum size}
{Handle: The handle of the window to reset the viewport for}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ConsoleWindowResetViewport(Handle);
end;

{==============================================================================}

function ConsoleWindowGetViewport(Handle:TWindowHandle;var X1,Y1,X2,Y2:LongWord):LongWord;
{Get the X1,Y1,X2,Y2 of the window viewport for an existing console window}
{Handle: The handle of the window to get the viewport for}
{X1: The left edge of the current viewport}
{Y1: The top edge of the current viewport}
{X2: The right edge of the current viewport}
{Y2: The bottom edge of the current viewport}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: For Text Console functions, Viewport is based on character rows and columns not screen pixels}
var
 Window:PConsoleWindow;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_TEXT then Exit;

 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;

 {Get Viewport}
 X1:=Window.MinX;
 Y1:=Window.MinY;
 X2:=Window.MaxX;
 Y2:=Window.MaxY;

 Result:=ERROR_SUCCESS;

 {Unlock Window}
 MutexUnlock(Window.Lock);
end;

{==============================================================================}

function ConsoleWindowSetViewport(Handle:TWindowHandle;X1,Y1,X2,Y2:LongWord):LongWord;
{Set the X1,Y1,X2,Y2 of the window viewport for an existing console window}
{Handle: The handle of the window to get the viewport for}
{X1: The left edge of the window viewport}
{Y1: The top edge of the window viewport}
{X2: The right edge of the window viewport}
{Y2: The bottom edge of the window viewport}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: For Text Console functions, Viewport is based on character rows and columns not screen pixels}
var
 Window:PConsoleWindow;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console Window Set Viewport');
 {$ENDIF}

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_TEXT then Exit;

 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;

 {Check X1,Y1,X2,Y2}
 if (X1 > 0) and (Y1 > 0) and (X1 <= X2) and (Y1 <= Y2) and (X2 <= Window.Width) and (Y2 <= Window.Height) then
  begin
   {Set Viewport}
   Window.MinX:=X1;
   Window.MinY:=Y1;
   Window.MaxX:=X2;
   Window.MaxY:=Y2;
   Window.X:=1;
   Window.Y:=1;
   Window.Cols:=Window.MaxX - (Window.MinX - 1);
   Window.Rows:=Window.MaxY - (Window.MinY - 1);

   {Unlock Window}
   MutexUnlock(Window.Lock);

   {Set Cursor XY}
   Result:=ConsoleWindowSetCursorXY(Handle,Window.X,Window.Y);
  end
 else
  begin
   {Unlock Window}
   MutexUnlock(Window.Lock);
  end;
end;

{==============================================================================}

function ConsoleWindowResetViewport(Handle:TWindowHandle):LongWord;
{Reset the window viewport for an existing console window to the maximum size}
{Handle: The handle of the window to reset the viewport for}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Window:PConsoleWindow;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console Window Reset Viewport');
 {$ENDIF}

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_TEXT then Exit;

 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;

 {Reset Viewport}
 Window.MinX:=1;
 Window.MinY:=1;
 Window.MaxX:=Window.Width;
 Window.MaxY:=Window.Height;
 Window.X:=1;
 Window.Y:=1;
 Window.Cols:=Window.Width;
 Window.Rows:=Window.Height;

 {Unlock Window}
 MutexUnlock(Window.Lock);

 {Set Cursor XY}
 Result:=ConsoleWindowSetCursorXY(Handle,Window.X,Window.Y);
end;

{==============================================================================}

function ConsoleWindowGetX(Handle:TWindowHandle):LongWord;
{Get the current X (Column) position of an existing console window}
{Handle: The handle of the window to get X for}
{Return: The X value for the window}

{Note: For Text Console functions, X is based on character columns not screen pixels}
var
 Window:PConsoleWindow;
begin
 {}
 Result:=0;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_TEXT then Exit;

 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;

 {Get X}
 Result:=Window.X;

 {Unlock Window}
 MutexUnlock(Window.Lock);
end;

{==============================================================================}

function ConsoleWindowSetX(Handle:TWindowHandle;X:LongWord):LongWord;
{Set the current X (Column) position of an existing console window}
{Handle: The handle of the window to set X for}
{X: The new X value to set}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: For Text Console functions, X is based on character columns not screen pixels}
var
 Window:PConsoleWindow;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console Window Set X');
 {$ENDIF}

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_TEXT then Exit;

 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;

 {Check X}
 if (X > 0) and (((Window.MinX - 1) + X) <= Window.MaxX) then
  begin
   {Set X}
   Window.X:=X;

   {Unlock Window}
   MutexUnlock(Window.Lock);

   {Set Cursor XY}
   Result:=ConsoleWindowSetCursorXY(Handle,Window.X,Window.Y);
  end
 else
  begin
   {Unlock Window}
   MutexUnlock(Window.Lock);
  end;
end;

{==============================================================================}

function ConsoleWindowGetY(Handle:TWindowHandle):LongWord;
{Get the current Y (Row) position of an existing console window}
{Handle: The handle of the window to get Y for}
{Return: The Y value for the window}

{Note: For Text Console functions, Y is based on character rows not screen pixels}
var
 Window:PConsoleWindow;
begin
 {}
 Result:=0;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_TEXT then Exit;

 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;

 {Get Y}
 Result:=Window.Y;

 {Unlock Window}
 MutexUnlock(Window.Lock);
end;

{==============================================================================}

function ConsoleWindowSetY(Handle:TWindowHandle;Y:LongWord):LongWord;
{Set the current Y (Row) position of an existing console window}
{Handle: The handle of the window to set Y for}
{Y: The new Y value to set}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: For Text Console functions, Y is based on character rows not screen pixels}
var
 Window:PConsoleWindow;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console Window Set Y');
 {$ENDIF}

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_TEXT then Exit;

 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;

 {Check Y}
 if (Y > 0) and (((Window.MinY - 1) + Y) <= Window.MaxY) then
  begin
   {Set Y}
   Window.Y:=Y;

   {Unlock Window}
   MutexUnlock(Window.Lock);

   {Set Cursor XY}
   Result:=ConsoleWindowSetCursorXY(Handle,Window.X,Window.Y);
  end
 else
  begin
   {Unlock Window}
   MutexUnlock(Window.Lock);
  end;
end;

{==============================================================================}

function ConsoleWindowGetXY(Handle:TWindowHandle;var X,Y:LongWord):LongWord;
{Get the current X and Y positions of an existing console window}
{Handle: The handle of the window to get X and Y for}
{X: The returned X value}
{Y: The returned Y value}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: For Text Console functions, X and Y are based on character rows and columns not screen pixels}
var
 Window:PConsoleWindow;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_TEXT then Exit;

 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;

 {Get XY}
 X:=Window.X;
 Y:=Window.Y;

 Result:=ERROR_SUCCESS;

 {Unlock Window}
 MutexUnlock(Window.Lock);
end;

{==============================================================================}

function ConsoleWindowSetXY(Handle:TWindowHandle;X,Y:LongWord):LongWord;
{Set the current X and Y positions of an existing console window}
{Handle: The handle of the window to set X and Y for}
{X: The new X value}
{Y: The new Y value}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: For Text Console functions, X and Y are based on character rows and columns not screen pixels}
var
 Window:PConsoleWindow;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console Window Set XY');
 {$ENDIF}

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_TEXT then Exit;

 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;

 {Check X,Y}
 if (X > 0) and (Y > 0) and (((Window.MinX - 1) + X) <= Window.MaxX) and (((Window.MinY - 1) + Y) <= Window.MaxY) then
  begin
   {Set X,Y}
   Window.X:=X;
   Window.Y:=Y;

   {Unlock Window}
   MutexUnlock(Window.Lock);

   {Set Cursor XY}
   Result:=ConsoleWindowSetCursorXY(Handle,Window.X,Window.Y);
  end
 else
  begin
   {Unlock Window}
   MutexUnlock(Window.Lock);
  end;
end;

{==============================================================================}

function ConsoleWindowGetPoint(Handle:TWindowHandle):TConsolePoint;
{Get the point X,Y of an existing console window}
{Handle: The handle of the window to get the point for}
{Return: The current point of the window}

{Note: For Text Console functions, Point is based on character rows and columns not screen pixels}
begin
 {}
 ConsoleWindowGetXY(Handle,Result.X,Result.Y);
end;

{==============================================================================}

function ConsoleWindowSetPoint(Handle:TWindowHandle;const APoint:TConsolePoint):LongWord;
{Set the point X,Y of an existing console window}
{Handle: The handle of the window to set the point for}
{Point: The new point to set for the window}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: For Text Console functions, Point is based on character rows and columns not screen pixels}
begin
 {}
 Result:=ConsoleWindowSetXY(Handle,APoint.X,APoint.Y);
end;

{==============================================================================}

function ConsoleWindowGetCols(Handle:TWindowHandle):LongWord;
{Get the current columns of the window viewport for an existing console window}
{Handle: The handle of the window to get columns for}
{Return: The columns value for the current window viewport}

{Note: For Text Console functions, Columns is based on character columns not screen pixels}
var
 Window:PConsoleWindow;
begin
 {}
 Result:=0;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_TEXT then Exit;

 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;

 {Get Cols}
 Result:=Window.Cols;

 {Unlock Window}
 MutexUnlock(Window.Lock);
end;

{==============================================================================}

function ConsoleWindowGetRows(Handle:TWindowHandle):LongWord;
{Get the current rows of the window viewport for an existing console window}
{Handle: The handle of the window to get rows for}
{Return: The rows value for the current window viewport}

{Note: For Text Console functions, Rows is based on character rows not screen pixels}
var
 Window:PConsoleWindow;
begin
 {}
 Result:=0;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_TEXT then Exit;

 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;

 {Get Rows}
 Result:=Window.Rows;

 {Unlock Window}
 MutexUnlock(Window.Lock);
end;

{==============================================================================}

function ConsoleWindowGetWidth(Handle:TWindowHandle):LongWord;
{Get the absolute width of an existing console window}
{Handle: The handle of the window to get the width for}
{Return: The absolute width of the window}

{Note: For Text Console functions, Width is based on character columns not screen pixels}
var
 Window:PConsoleWindow;
begin
 {}
 Result:=0;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_TEXT then Exit;

 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;

 {Get Width}
 Result:=Window.Width;

 {Unlock Window}
 MutexUnlock(Window.Lock);
end;

{==============================================================================}

function ConsoleWindowGetHeight(Handle:TWindowHandle):LongWord;
{Get the absolute height of an existing console window}
{Handle: The handle of the window to get the height for}
{Return: The absolute height of the window}

{Note: For Text Console functions, Height is based on character rows not screen pixels}
var
 Window:PConsoleWindow;
begin
 {}
 Result:=0;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_TEXT then Exit;

 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;

 {Get Height}
 Result:=Window.Height;

 {Unlock Window}
 MutexUnlock(Window.Lock);
end;

{==============================================================================}

function ConsoleWindowGetFormat(Handle:TWindowHandle):LongWord;
{Get the color format of an existing console window}
{Handle: The handle of the window to get the format for}
{Return: The color format of the window (eg COLOR_FORMAT_ARGB32)}
var
 Window:PConsoleWindow;
begin
 {}
 Result:=0;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_TEXT then Exit;

 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;

 {Get Format}
 Result:=Window.Format;

 {Unlock Window}
 MutexUnlock(Window.Lock);
end;

{==============================================================================}

function ConsoleWindowGetForecolor(Handle:TWindowHandle):LongWord;
{Get the current foreground color of an existing console window}
{Handle: The handle of the window to get the foreground color for}
{Return: The foreground color of the window (eg COLOR_WHITE)}
var
 Window:PConsoleWindow;
begin
 {}
 Result:=0;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_TEXT then Exit;

 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;

 {Get Forecolor}
 Result:=Window.Forecolor;

 {Unlock Window}
 MutexUnlock(Window.Lock);
end;

{==============================================================================}

function ConsoleWindowSetForecolor(Handle:TWindowHandle;Color:LongWord):LongWord;
{Set the current foreground color of an existing console window}
{Handle: The handle of the window to set the foreground color for}
{Color: The foreground color to set (eg COLOR_WHITE)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Window:PConsoleWindow;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Color}
 {if Color = COLOR_NONE then Exit;}

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console Window Set Forecolor');
 {$ENDIF}

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_TEXT then Exit;

 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;

 {Set Forecolor}
 Window.Forecolor:=Color;

 Result:=ERROR_SUCCESS;

 {Unlock Window}
 MutexUnlock(Window.Lock);
end;

{==============================================================================}

function ConsoleWindowGetBackcolor(Handle:TWindowHandle):LongWord;
{Get the current background color of an existing console window}
{Handle: The handle of the window to get the background color for}
{Return: The background color of the window (eg COLOR_BLACK)}
var
 Window:PConsoleWindow;
begin
 {}
 Result:=0;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_TEXT then Exit;

 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;

 {Get Backcolor}
 Result:=Window.Backcolor;

 {Unlock Window}
 MutexUnlock(Window.Lock);
end;

{==============================================================================}

function ConsoleWindowSetBackcolor(Handle:TWindowHandle;Color:LongWord):LongWord;
{Set the current background color of an existing console window}
{Handle: The handle of the window to set the background color for}
{Color: The background color to set (eg COLOR_BLACK)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Window:PConsoleWindow;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Color}
 {if Color = COLOR_NONE then Exit;}

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console Window Set Backcolor');
 {$ENDIF}

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_TEXT then Exit;

 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;

 {Set Backcolor}
 Window.Backcolor:=Color;

 Result:=ERROR_SUCCESS;

 {Unlock Window}
 MutexUnlock(Window.Lock);
end;

{==============================================================================}

function ConsoleWindowGetFont(Handle:TWindowHandle):TFontHandle;
{Get the default font of an existing console window}
{Handle: The handle of the window to get the default font for}
{Return: The font handle of the default font or INVALID_HANDLE_VALUE on error}
var
 Window:PConsoleWindow;
begin
 {}
 Result:=INVALID_HANDLE_VALUE;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_TEXT then Exit;

 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;

 {Get Font}
 Result:=Window.Font;

 {Unlock Window}
 MutexUnlock(Window.Lock);
end;

{==============================================================================}

function ConsoleWindowSetFont(Handle:TWindowHandle;Font:TFontHandle):LongWord;
{Set the default font of an existing console window}
{Handle: The handle of the window to set the default font for}
{Font: The font handle of the default font to set}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: For Text Console windows, setting the font also clears the window}
var
 RemainX:LongWord;
 RemainY:LongWord;
 Viewport:Boolean;
 FontWidth:LongWord;
 FontHeight:LongWord;
 Window:PConsoleWindow;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Font}
 if Font = INVALID_HANDLE_VALUE then Exit;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console Window Set Font');
 {$ENDIF}

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_TEXT then Exit;

 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;
 try
  {Check Console}
  if Window.Console = nil then Exit;

  {Check Font}
  FontWidth:=FontGetWidth(Font);
  FontHeight:=FontGetHeight(Font);
  if (FontWidth <> 0) and (FontHeight <> 0) then
   begin
    {Check Viewport}
    Viewport:=False;
    if (Window.MinX <> 1) or (Window.MinY <> 1) or (Window.MaxX <> Window.Width) or (Window.MaxY <> Window.Height) then
     begin
      Viewport:=True;
     end;

    {Set Font}
    Window.Font:=Font;
    Window.FontWidth:=FontWidth * Window.Console.FontRatio;
    if Window.FontWidth = 0 then Window.FontWidth:=1;
    Window.FontHeight:=FontHeight * Window.Console.FontRatio;
    if Window.FontHeight = 0 then Window.FontHeight:=1;

    {Get Width / Height}
    Window.Width:=(((Window.X2 - Window.X1) + 1) - (2 * Window.Borderwidth)) div Window.FontWidth;
    Window.Height:=(((Window.Y2 - Window.Y1) + 1) - (2 * Window.Borderwidth)) div Window.FontHeight;

    {Check Font Ratio}
    if Window.Console.FontRatio > 0 then
     begin
      {Get RemainX,Y}
      RemainX:=(((Window.X2 - Window.X1) + 1) - (2 * Window.Borderwidth)) mod Window.FontWidth;
      RemainY:=(((Window.Y2 - Window.Y1) + 1) - (2 * Window.Borderwidth)) mod Window.FontHeight;

      {Check Remain X}
      if RemainX < 4 then
       begin
        {Decrease Width}
        Dec(Window.Width);
        Inc(RemainX,Window.FontWidth);
       end;
      {Get Offset X}
      Window.OffsetX:=(RemainX div 2) + (RemainX mod 2);

      {Check Remain Y}
      if RemainY < 4 then
       begin
        {Decrease Height}
        Dec(Window.Height);
        Inc(RemainY,Window.FontHeight);
       end;
      {Get Offset Y}
      Window.OffsetY:=(RemainY div 2) + (RemainY mod 2);
     end;

    if not Viewport then
     begin
      {Set MinX,Y / MaxX,Y}
      Window.MinX:=1;
      Window.MinY:=1;
      Window.MaxX:=Window.Width;
      Window.MaxY:=Window.Height;

      {Set Cols / Rows}
      Window.Cols:=Window.Width;
      Window.Rows:=Window.Height;
     end
    else
     begin
      {Get MinX,Y / MaxX,Y}
      if Window.MinX > Window.Width then Window.MinX:=1;
      if Window.MinY > Window.Height then Window.MinY:=1;
      if Window.MaxX > Window.Width then Window.MaxX:=Window.Width;
      if Window.MaxY > Window.Height then Window.MaxY:=Window.Height;

      {Get Cols / Rows}
      Window.Cols:=Window.MaxX - (Window.MinX - 1);
      Window.Rows:=Window.MaxY - (Window.MinY - 1);
     end;

    {Get X,Y / Cursor X,Y}
    if Window.X > Window.Cols then Window.X:=1;
    if Window.Y > Window.Rows then Window.Y:=1;
    if Window.CursorX > Window.Cols then Window.CursorX:=1;
    if Window.CursorY > Window.Rows then Window.CursorY:=1;

    {Get CaretX / CaretY}
    Window.CaretX:=Window.X1 + Window.Borderwidth + Window.OffsetX + ((Window.MinX - 1) * Window.FontWidth) + ((Window.CursorX - 1) * Window.FontWidth);
    Window.CaretY:=Window.Y1 + Window.Borderwidth + Window.OffsetY + ((Window.MinY - 1) * Window.FontHeight) + ((Window.CursorY - 1) * Window.FontHeight);

    {Delete Caret}
    if Window.CaretHandle <> INVALID_HANDLE_VALUE then
     begin
      ConsoleDeviceDeleteCaret(Window.Console,Window.CaretHandle);
      Window.CaretHandle:=INVALID_HANDLE_VALUE;
     end;

    {Check Visible}
    if Window.WindowState = WINDOW_STATE_VISIBLE then
     begin
      {Draw Window}
      ConsoleDeviceDrawWindow(Window.Console,TWindowHandle(Window),WINDOW_DRAW_FLAG_ALL);

      {Update Cursor}
      ConsoleWindowCursorMove(Handle,Window.CursorX,Window.CursorY);
     end;

    {Return Result}
    Result:=ERROR_SUCCESS;
   end;
 finally
  {Unlock Window}
  MutexUnlock(Window.Lock);
 end;
end;

{==============================================================================}

function ConsoleWindowGetCursorXY(Handle:TWindowHandle;var X,Y:LongWord):LongWord;
{Get the current cursor X and Y positions of an existing console window}
{Handle: The handle of the window to get cursor X and Y for}
{X: The returned cursor X value}
{Y: The returned cursor Y value}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: For Text Console functions, cursor X and Y are based on character rows and columns not screen pixels}
var
 Window:PConsoleWindow;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 {if Window.WindowMode <> WINDOW_MODE_TEXT then Exit;} {Allow any mode, other window classes should override if required}

 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;

 {Get Cursor XY}
 X:=Window.CursorX;
 Y:=Window.CursorY;

 Result:=ERROR_SUCCESS;

 {Unlock Window}
 MutexUnlock(Window.Lock);
end;

{==============================================================================}

function ConsoleWindowSetCursorXY(Handle:TWindowHandle;X,Y:LongWord):LongWord;
{Set the current cursor X and Y positions of an existing console window}
{Handle: The handle of the window to set cursor X and Y for}
{X: The new cursor X value}
{Y: The new cursor Y value}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: For Text Console functions, cursor X and Y are based on character rows and columns not screen pixels}
var
 Window:PConsoleWindow;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console Window Set Cursor XY');
 {$ENDIF}

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_TEXT then Exit;

 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;

 {Check Cursor State}
 if Window.CursorState = CURSOR_STATE_ON then
  begin
   {Unlock Window}
   MutexUnlock(Window.Lock);

   {Cursor Move}
   Result:=ConsoleWindowCursorMove(Handle,X,Y);
  end
 else
  begin
   {Check Cursor X,Y}
   if (X > 0) and (Y > 0) and (((Window.MinX - 1) + X) <= Window.MaxX) and (((Window.MinY - 1) + Y) <= Window.MaxY) then
    begin
     {Set Cursor XY}
     Window.CursorX:=X;
     Window.CursorY:=Y;

     {Get CaretX / CaretY}
     Window.CaretX:=Window.X1 + Window.Borderwidth + Window.OffsetX + ((Window.MinX - 1) * Window.FontWidth) + ((Window.CursorX - 1) * Window.FontWidth);
     Window.CaretY:=Window.Y1 + Window.Borderwidth + Window.OffsetY + ((Window.MinY - 1) * Window.FontHeight) + ((Window.CursorY - 1) * Window.FontHeight);

     Result:=ERROR_SUCCESS;

     {Unlock Window}
     MutexUnlock(Window.Lock);
    end
   else
    begin
     {Unlock Window}
     MutexUnlock(Window.Lock);
    end;
  end;
end;

{==============================================================================}

function ConsoleWindowGetCursorMode(Handle:TWindowHandle):TCursorMode;
{Get the current cursor mode of an existing console window}
{Handle: The handle of the window to get the mode for}
{Return: The current cursor mode (eg CURSOR_MODE_INSERT)}
var
 Window:PConsoleWindow;
begin
 {}
 Result:=CURSOR_MODE_INSERT;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 {if Window.WindowMode <> WINDOW_MODE_TEXT then Exit;} {Allow any mode, other window classes should override if required}

 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;

 {Get Cursor Mode}
 Result:=Window.CursorMode;

 {Unlock Window}
 MutexUnlock(Window.Lock);
end;

{==============================================================================}

function ConsoleWindowSetCursorMode(Handle:TWindowHandle;CursorMode:TCursorMode):LongWord;
{Set the current cursor mode of an existing console window}
{Handle: The handle of the window to set the mode for}
{CursorMode: The cursor mode to set (eg CURSOR_MODE_INSERT)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Window:PConsoleWindow;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console Window Set Cursor Mode');
 {$ENDIF}

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 {if Window.WindowMode <> WINDOW_MODE_TEXT then Exit;} {Allow any mode, other window classes should override if required}

 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;

 {Set Cursor Mode}
 Window.CursorMode:=CursorMode;

 {Unlock Window}
 MutexUnlock(Window.Lock);
end;

{==============================================================================}

function ConsoleWindowGetCursorBlink(Handle:TWindowHandle):Boolean;
{Get the current cursor blink state of an existing console window}
{Handle: The handle of the window to get blink state for}
{Return: True if blink is enabled, False if not or on error}
var
 Window:PConsoleWindow;
begin
 {}
 Result:=False;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 {if Window.WindowMode <> WINDOW_MODE_TEXT then Exit;} {Allow any mode, other window classes should override if required}

 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;

 {Get Cursor Blink}
 Result:=Window.CursorBlink;

 {Unlock Window}
 MutexUnlock(Window.Lock);
end;

{==============================================================================}

function ConsoleWindowSetCursorBlink(Handle:TWindowHandle;CursorBlink:Boolean):LongWord;
{Set the current cursor blink state of an existing console window}
{Handle: The handle of the window to set the blink state for}
{CursorBlink: True to enable blink, False to disable}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Window:PConsoleWindow;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console Window Set Cursor Blink');
 {$ENDIF}

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 {if Window.WindowMode <> WINDOW_MODE_TEXT then Exit;} {Allow any mode, other window classes should override if required}

 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;

 {Check Cursor Blink}
 if Window.CursorBlink then
  begin
   {Unlock Window}
   MutexUnlock(Window.Lock);

   {Check Cursor Blink}
   if not(CursorBlink) then Result:=ConsoleWindowCursorBlink(Handle,CursorBlink) else Result:=ERROR_SUCCESS;
  end
 else
  begin
   {Unlock Window}
   MutexUnlock(Window.Lock);

   {Check Cursor Blink}
   if CursorBlink then Result:=ConsoleWindowCursorBlink(Handle,CursorBlink) else Result:=ERROR_SUCCESS;
  end;
end;

{==============================================================================}

function ConsoleWindowGetCursorState(Handle:TWindowHandle):TCursorState;
{Get the current cursor state of an existing console window}
{Handle: The handle of the window to get the state for}
{Return: The current cursor state (eg CURSOR_STATE_ON)}
var
 Window:PConsoleWindow;
begin
 {}
 Result:=CURSOR_STATE_OFF;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 {if Window.WindowMode <> WINDOW_MODE_TEXT then Exit;} {Allow any mode, other window classes should override if required}

 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;

 {Get Cursor State}
 Result:=Window.CursorState;

 {Unlock Window}
 MutexUnlock(Window.Lock);
end;

{==============================================================================}

function ConsoleWindowSetCursorState(Handle:TWindowHandle;CursorState:TCursorState):LongWord;
{Set the current cursor state of an existing console window}
{Handle: The handle of the window to set the state for}
{CursorState: The cursor state to set (eg CURSOR_STATE_ON)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Window:PConsoleWindow;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console Window Set Cursor State');
 {$ENDIF}

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 {if Window.WindowMode <> WINDOW_MODE_TEXT then Exit;} {Allow any mode, other window classes should override if required}

 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;

 {Check Cursor State}
 if Window.CursorState = CURSOR_STATE_ON then
  begin
   {Unlock Window}
   MutexUnlock(Window.Lock);

   {Check Cursor State}
   if CursorState = CURSOR_STATE_OFF then Result:=ConsoleWindowCursorOff(Handle) else Result:=ERROR_SUCCESS;
  end
 else
  begin
   {Unlock Window}
   MutexUnlock(Window.Lock);

   {Check Cursor State}
   if CursorState = CURSOR_STATE_ON then Result:=ConsoleWindowCursorOn(Handle) else Result:=ERROR_SUCCESS;
  end;
end;

{==============================================================================}

function ConsoleWindowGetCursorShape(Handle:TWindowHandle):TCursorShape;
{Get the current cursor shape of an existing console window}
{Handle: The handle of the window to get the shape for}
{Return: The current cursor shape (eg CURSOR_SHAPE_LINE)}
var
 Window:PConsoleWindow;
begin
 {}
 Result:=CURSOR_SHAPE_LINE;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 {if Window.WindowMode <> WINDOW_MODE_TEXT then Exit;} {Allow any mode, other window classes should override if required}

 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;

 {Get Cursor Shape}
 Result:=Window.CursorShape;

 {Unlock Window}
 MutexUnlock(Window.Lock);
end;

{==============================================================================}

function ConsoleWindowSetCursorShape(Handle:TWindowHandle;CursorShape:TCursorShape):LongWord;
{Set the current cursor shape of an existing console window}
{Handle: The handle of the window to set the shape for}
{CursorShape: The cursor shape to set (eg CURSOR_SHAPE_LINE)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Window:PConsoleWindow;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console Window Set Cursor Shape');
 {$ENDIF}

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 {if Window.WindowMode <> WINDOW_MODE_TEXT then Exit;} {Allow any mode, other window classes should override if required}

 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;

 {Check Cursor Shape}
 if CursorShape = CURSOR_SHAPE_LINE then
  begin
   {Unlock Window}
   MutexUnlock(Window.Lock);

   {Check Cursor Shape}
   if Window.CursorShape <> CURSOR_SHAPE_LINE then Result:=ConsoleWindowCursorLine(Handle) else Result:=ERROR_SUCCESS;
  end
 else if CursorShape = CURSOR_SHAPE_BAR then
  begin
   {Unlock Window}
   MutexUnlock(Window.Lock);

   {Check Cursor Shape}
   if Window.CursorShape <> CURSOR_SHAPE_BAR then Result:=ConsoleWindowCursorBar(Handle) else Result:=ERROR_SUCCESS;
  end
 else if CursorShape = CURSOR_SHAPE_BLOCK then
  begin
   {Unlock Window}
   MutexUnlock(Window.Lock);

   {Check Cursor Shape}
   if Window.CursorShape <> CURSOR_SHAPE_BLOCK then Result:=ConsoleWindowCursorBlock(Handle) else Result:=ERROR_SUCCESS;
  end;
end;

{==============================================================================}

function ConsoleWindowGetCursorColor(Handle:TWindowHandle):LongWord;
{Get the current cursor color of an existing console window}
{Handle: The handle of the window to get cursor color for}
{Return: The cursor color of the window (eg COLOR_WHITE)}

{Note: Color will be returned in the default color format (See COLOR_FORMAT_DEFAULT)}
var
 Window:PConsoleWindow;
begin
 {}
 Result:=COLOR_NONE;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_TEXT then Exit; {Text mode only}

 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;

 {Get Cursor Backcolor}
 Result:=Window.CursorBackcolor;

 {Unlock Window}
 MutexUnlock(Window.Lock);
end;

{==============================================================================}

function ConsoleWindowSetCursorColor(Handle:TWindowHandle;Color:LongWord):LongWord;
{Set the current cursor color of an existing console window}
{Handle: The handle of the window to set the cursor color for}
{Color: The cursor color to set (eg COLOR_WHITE)}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: Color must be specified in the default color format (See COLOR_FORMAT_DEFAULT)}
var
 Window:PConsoleWindow;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console Window Set Cursor Color');
 {$ENDIF}

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_TEXT then Exit; {Text mode only}

 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;

 {Check Cursor State}
 if Window.CursorState = CURSOR_STATE_ON then
  begin
   {Unlock Window}
   MutexUnlock(Window.Lock);

   {Cursor Color}
   Result:=ConsoleWindowCursorColor(Handle,Color);
  end
 else
  begin
   {Set Cursor Color}
   Window.CursorBackcolor:=Color;

   Result:=ERROR_SUCCESS;

   {Unlock Window}
   MutexUnlock(Window.Lock);
  end;
end;

{==============================================================================}

function ConsoleWindowGetCursorReverse(Handle:TWindowHandle):Boolean;
{Get the current cursor reverse state of an existing console window}
{Handle: The handle of the window to get reverse state for}
{Return: True if reverse color is enabled, False if inverse color is enabled}
var
 Window:PConsoleWindow;
begin
 {}
 Result:=False;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_TEXT then Exit; {Text mode only}

 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;

 {Get Cursor Reverse}
 Result:=Window.CursorReverse;

 {Unlock Window}
 MutexUnlock(Window.Lock);
end;

{==============================================================================}

function ConsoleWindowSetCursorReverse(Handle:TWindowHandle;CursorReverse:Boolean):LongWord;
{Set the current cursor reverse state of an existing console window}
{Handle: The handle of the window to set the reverse state for}
{CursorReverse: True to enable reverse color, False to enable inverse color}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Window:PConsoleWindow;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console Window Set Cursor Reverse');
 {$ENDIF}

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_TEXT then Exit; {Text mode only}

 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;

 {Check Cursor Reverse}
 if Window.CursorReverse then
  begin
   {Unlock Window}
   MutexUnlock(Window.Lock);

   {Check Cursor Reverse}
   if not(CursorReverse) then Result:=ConsoleWindowCursorReverse(Handle,CursorReverse) else Result:=ERROR_SUCCESS;
  end
 else
  begin
   {Unlock Window}
   MutexUnlock(Window.Lock);

   {Check Cursor Reverse}
   if CursorReverse then Result:=ConsoleWindowCursorReverse(Handle,CursorReverse) else Result:=ERROR_SUCCESS;
  end;
end;

{==============================================================================}

function ConsoleWindowCursorOn(Handle:TWindowHandle):LongWord;
{Enable the cursor on an existing console window}
{Handle: The handle of the window to enable the cursor for}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Width:LongWord;
 Height:LongWord;
 OffsetX:LongWord;
 OffsetY:LongWord;
 Window:PConsoleWindow;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console Window Cursor On');
 {$ENDIF}

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 {if Window.WindowMode <> WINDOW_MODE_TEXT then Exit;} {Allow any mode, other window classes should override if required}

 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;
 try
  {Check Console}
  if Window.Console = nil then Exit;

  {Check Cursor State}
  if Window.CursorState = CURSOR_STATE_OFF then
   begin
    {Set Cursor State}
    Window.CursorState:=CURSOR_STATE_ON;

    {Check Visible}
    if Window.WindowState = WINDOW_STATE_VISIBLE then
     begin
      {Check Caret}
      if Window.CaretHandle = INVALID_HANDLE_VALUE then
       begin
        {Get Dimensions}
        Width:=Window.FontWidth;
        Height:=Window.FontHeight;
        OffsetX:=0;
        OffsetY:=0;
        if Window.CursorShape = CURSOR_SHAPE_LINE then
         begin
          Width:=1;
         end
        else if Window.CursorShape = CURSOR_SHAPE_BAR then
         begin
          Height:=1;
          OffsetY:=Window.FontHeight - 1;
         end;

        {Add Caret}
        Window.CaretHandle:=ConsoleDeviceAddCaret(Window.Console,Width,Height,OffsetX,OffsetY);
       end;

      {Check Flag}
      if ((Window.WindowFlags and WINDOW_FLAG_FOCUS_CURSOR) = 0) or (Window.Console.WindowActive = Window) then
       begin
        {Update Caret}
        Result:=ConsoleDeviceUpdateCaretEx(Window.Console,Window.CaretHandle,Window.CaretX,Window.CaretY,Window.CursorForecolor,Window.CursorBackcolor,(Window.CursorState = CURSOR_STATE_ON),Window.CursorBlink,Window.CursorReverse);
        if Result <> ERROR_SUCCESS then Exit;
       end;
     end;
   end;

  {Return Result}
  Result:=ERROR_SUCCESS;
 finally
  {Unlock Window}
  MutexUnlock(Window.Lock);
 end;
end;

{==============================================================================}

function ConsoleWindowCursorOff(Handle:TWindowHandle):LongWord;
{Disable the cursor on an existing console window}
{Handle: The handle of the window to disable the cursor for}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Window:PConsoleWindow;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console Window Cursor Off');
 {$ENDIF}

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 {if Window.WindowMode <> WINDOW_MODE_TEXT then Exit;} {Allow any mode, other window classes should override if required}

 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;
 try
  {Check Console}
  if Window.Console = nil then Exit;

  {Check Cursor State}
  if Window.CursorState = CURSOR_STATE_ON then
   begin
    {Set Cursor State}
    Window.CursorState:=CURSOR_STATE_OFF;

    {Check Visible}
    if Window.WindowState = WINDOW_STATE_VISIBLE then
     begin
      {Update Caret}
      Result:=ConsoleDeviceUpdateCaretEx(Window.Console,Window.CaretHandle,Window.CaretX,Window.CaretY,Window.CursorForecolor,Window.CursorBackcolor,(Window.CursorState = CURSOR_STATE_ON),Window.CursorBlink,Window.CursorReverse);
      if Result <> ERROR_SUCCESS then Exit;
     end;
   end;

  {Return Result}
  Result:=ERROR_SUCCESS;
 finally
  {Unlock Window}
  MutexUnlock(Window.Lock);
 end;
end;

{==============================================================================}

function ConsoleWindowCursorLine(Handle:TWindowHandle):LongWord;
{Change the cursor to a vertical line on an existing console window}
{Handle: The handle of the window to change the cursor for}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Window:PConsoleWindow;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console Window Cursor Line');
 {$ENDIF}

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 {if Window.WindowMode <> WINDOW_MODE_TEXT then Exit;} {Allow any mode, other window classes should override if required}

 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;
 try
  {Check Console}
  if Window.Console = nil then Exit;

  {Check Cursor Shape}
  if Window.CursorShape <> CURSOR_SHAPE_LINE then
   begin
    {Set Cursor Shape}
    Window.CursorShape:=CURSOR_SHAPE_LINE;

    {Check Caret}
    if Window.CaretHandle <> INVALID_HANDLE_VALUE then
     begin
      {Delete Caret}
      ConsoleDeviceDeleteCaret(Window.Console,Window.CaretHandle);
      Window.CaretHandle:=INVALID_HANDLE_VALUE;
     end;

    {Check Visible}
    if Window.WindowState = WINDOW_STATE_VISIBLE then
     begin
      {Add Caret}
      Window.CaretHandle:=ConsoleDeviceAddCaret(Window.Console,1,Window.FontHeight,0,0);

      {Check Flag}
      if ((Window.WindowFlags and WINDOW_FLAG_FOCUS_CURSOR) = 0) or (Window.Console.WindowActive = Window) then
       begin
        {Update Caret}
        Result:=ConsoleDeviceUpdateCaretEx(Window.Console,Window.CaretHandle,Window.CaretX,Window.CaretY,Window.CursorForecolor,Window.CursorBackcolor,(Window.CursorState = CURSOR_STATE_ON),Window.CursorBlink,Window.CursorReverse);
        if Result <> ERROR_SUCCESS then Exit;
       end;
     end;
   end;

  {Return Result}
  Result:=ERROR_SUCCESS;
 finally
  {Unlock Window}
  MutexUnlock(Window.Lock);
 end;
end;

{==============================================================================}

function ConsoleWindowCursorBar(Handle:TWindowHandle):LongWord;
{Change the cursor to a horizontal bar on an existing console window}
{Handle: The handle of the window to change the cursor for}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Window:PConsoleWindow;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console Window Cursor Bar');
 {$ENDIF}

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 {if Window.WindowMode <> WINDOW_MODE_TEXT then Exit;} {Allow any mode, other window classes should override if required}

 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;
 try
  {Check Console}
  if Window.Console = nil then Exit;

  {Check Cursor Shape}
  if Window.CursorShape <> CURSOR_SHAPE_BAR then
   begin
    {Set Cursor Shape}
    Window.CursorShape:=CURSOR_SHAPE_BAR;

    {Check Caret}
    if Window.CaretHandle <> INVALID_HANDLE_VALUE then
     begin
      {Delete Caret}
      ConsoleDeviceDeleteCaret(Window.Console,Window.CaretHandle);
      Window.CaretHandle:=INVALID_HANDLE_VALUE;
     end;

    {Check Visible}
    if Window.WindowState = WINDOW_STATE_VISIBLE then
     begin
      {Add Caret}
      Window.CaretHandle:=ConsoleDeviceAddCaret(Window.Console,Window.FontWidth,1,0,Window.FontHeight - 1);

      {Check Flag}
      if ((Window.WindowFlags and WINDOW_FLAG_FOCUS_CURSOR) = 0) or (Window.Console.WindowActive = Window) then
       begin
        {Update Caret}
        Result:=ConsoleDeviceUpdateCaretEx(Window.Console,Window.CaretHandle,Window.CaretX,Window.CaretY,Window.CursorForecolor,Window.CursorBackcolor,(Window.CursorState = CURSOR_STATE_ON),Window.CursorBlink,Window.CursorReverse);
        if Result <> ERROR_SUCCESS then Exit;
       end;
     end;
   end;

  {Return Result}
  Result:=ERROR_SUCCESS;
 finally
  {Unlock Window}
  MutexUnlock(Window.Lock);
 end;
end;

{==============================================================================}

function ConsoleWindowCursorBlock(Handle:TWindowHandle):LongWord;
{Change the cursor to a solid block on an existing console window}
{Handle: The handle of the window to change the cursor for}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Window:PConsoleWindow;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console Window Cursor Block');
 {$ENDIF}

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 {if Window.WindowMode <> WINDOW_MODE_TEXT then Exit;} {Allow any mode, other window classes should override if required}

 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;
 try
  {Check Console}
  if Window.Console = nil then Exit;

  {Check Cursor Shape}
  if Window.CursorShape <> CURSOR_SHAPE_BLOCK then
   begin
    {Set Cursor Shape}
    Window.CursorShape:=CURSOR_SHAPE_BLOCK;

    {Check Caret}
    if Window.CaretHandle <> INVALID_HANDLE_VALUE then
     begin
      {Delete Caret}
      ConsoleDeviceDeleteCaret(Window.Console,Window.CaretHandle);
      Window.CaretHandle:=INVALID_HANDLE_VALUE;
     end;

    {Check Visible}
    if Window.WindowState = WINDOW_STATE_VISIBLE then
     begin
      {Add Caret}
      Window.CaretHandle:=ConsoleDeviceAddCaret(Window.Console,Window.FontWidth,Window.FontHeight,0,0);

      {Check Flag}
      if ((Window.WindowFlags and WINDOW_FLAG_FOCUS_CURSOR) = 0) or (Window.Console.WindowActive = Window) then
       begin
        {Update Caret}
        Result:=ConsoleDeviceUpdateCaretEx(Window.Console,Window.CaretHandle,Window.CaretX,Window.CaretY,Window.CursorForecolor,Window.CursorBackcolor,(Window.CursorState = CURSOR_STATE_ON),Window.CursorBlink,Window.CursorReverse);
        if Result <> ERROR_SUCCESS then Exit;
       end;
     end;
   end;

  {Return Result}
  Result:=ERROR_SUCCESS;
 finally
  {Unlock Window}
  MutexUnlock(Window.Lock);
 end;
end;

{==============================================================================}

function ConsoleWindowCursorMove(Handle:TWindowHandle;X,Y:LongWord):LongWord;
{Move the cursor on an existing console window}
{Handle: The handle of the window to move the cursor for}
{X: The column to move the cursor to}
{Y: The row to move the cursor to}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: For Text Console functions, X and Y are based on character rows and columns not screen pixels}
var
 Window:PConsoleWindow;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console Window Cursor Move');
 {$ENDIF}

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_TEXT then Exit;

 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;
 try
  {Check Console}
  if Window.Console = nil then Exit;

  {Check Cursor X,Y}
  if (X > 0) and (Y > 0) and (((Window.MinX - 1) + X) <= Window.MaxX) and (((Window.MinY - 1) + Y) <= Window.MaxY) then
   begin
    {Set Cursor XY}
    Window.CursorX:=X;
    Window.CursorY:=Y;

    {Get CaretX / CaretY}
    Window.CaretX:=Window.X1 + Window.Borderwidth + Window.OffsetX + ((Window.MinX - 1) * Window.FontWidth) + ((Window.CursorX - 1) * Window.FontWidth);
    Window.CaretY:=Window.Y1 + Window.Borderwidth + Window.OffsetY + ((Window.MinY - 1) * Window.FontHeight) + ((Window.CursorY - 1) * Window.FontHeight);

    {Check Cursor State and Visible}
    if (Window.CursorState = CURSOR_STATE_ON) and (Window.WindowState = WINDOW_STATE_VISIBLE) then
     begin
      {Check Flag}
      if ((Window.WindowFlags and WINDOW_FLAG_FOCUS_CURSOR) = 0) or (Window.Console.WindowActive = Window) then
       begin
        {Update Caret}
        Result:=ConsoleDeviceUpdateCaretEx(Window.Console,Window.CaretHandle,Window.CaretX,Window.CaretY,Window.CursorForecolor,Window.CursorBackcolor,(Window.CursorState = CURSOR_STATE_ON),Window.CursorBlink,Window.CursorReverse);
        if Result <> ERROR_SUCCESS then Exit;
       end;
     end;

    {Return Result}
    Result:=ERROR_SUCCESS;
   end;
 finally
  {Unlock Window}
  MutexUnlock(Window.Lock);
 end;
end;

{==============================================================================}

function ConsoleWindowCursorBlink(Handle:TWindowHandle;Enabled:Boolean):LongWord;
{Set the blink state of the cursor on an existing console window}
{Handle: The handle of the window to set the blink state for}
{Enabled: True if the cursor is blinking, False if not}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Window:PConsoleWindow;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console Window Cursor Blink');
 {$ENDIF}

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 {if Window.WindowMode <> WINDOW_MODE_TEXT then Exit;} {Allow any mode, other window classes should override if required}

 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;
 try
  {Check Console}
  if Window.Console = nil then Exit;

  {Check Cursor Blink}
  if Window.CursorBlink <> Enabled then
   begin
    {Set Cursor Blink}
    Window.CursorBlink:=Enabled;

    {Check Visible}
    if (Window.CursorState = CURSOR_STATE_ON) and (Window.WindowState = WINDOW_STATE_VISIBLE) then
     begin
      {Check Flag}
      if ((Window.WindowFlags and WINDOW_FLAG_FOCUS_CURSOR) = 0) or (Window.Console.WindowActive = Window) then
       begin
        {Update Caret}
        Result:=ConsoleDeviceUpdateCaretEx(Window.Console,Window.CaretHandle,Window.CaretX,Window.CaretY,Window.CursorForecolor,Window.CursorBackcolor,(Window.CursorState = CURSOR_STATE_ON),Window.CursorBlink,Window.CursorReverse);
        if Result <> ERROR_SUCCESS then Exit;
       end;
     end;
   end;

  {Return Result}
  Result:=ERROR_SUCCESS;
 finally
  {Unlock Window}
  MutexUnlock(Window.Lock);
 end;
end;

{==============================================================================}

function ConsoleWindowCursorColor(Handle:TWindowHandle;Color:LongWord):LongWord;
{Set the color of the cursor on an existing console window}
{Handle: The handle of the window to set the color for}
{Color: The cursor color to set (eg COLOR_WHITE)}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: Color must be specified in the default color format (See COLOR_FORMAT_DEFAULT)}
var
 Window:PConsoleWindow;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console Window Cursor Color');
 {$ENDIF}

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_TEXT then Exit; {Text mode only}

 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;
 try
  {Check Console}
  if Window.Console = nil then Exit;

  {Check Cursor Backcolor}
  if Window.CursorBackcolor <> Color then
   begin
    {Set Cursor Backcolor}
    Window.CursorBackcolor:=Color;

    {Check Visible}
    if (Window.CursorState = CURSOR_STATE_ON) and (Window.WindowState = WINDOW_STATE_VISIBLE) then
     begin
      {Check Flag}
      if ((Window.WindowFlags and WINDOW_FLAG_FOCUS_CURSOR) = 0) or (Window.Console.WindowActive = Window) then
       begin
        {Update Caret}
        Result:=ConsoleDeviceUpdateCaretEx(Window.Console,Window.CaretHandle,Window.CaretX,Window.CaretY,Window.CursorForecolor,Window.CursorBackcolor,(Window.CursorState = CURSOR_STATE_ON),Window.CursorBlink,Window.CursorReverse);
        if Result <> ERROR_SUCCESS then Exit;
       end;
     end;
   end;

  {Return Result}
  Result:=ERROR_SUCCESS;
 finally
  {Unlock Window}
  MutexUnlock(Window.Lock);
 end;
end;

{==============================================================================}

function ConsoleWindowCursorReverse(Handle:TWindowHandle;Enabled:Boolean):LongWord;
{Set the reverse state of the cursor on an existing console window}
{Handle: The handle of the window to set the reverse state for}
{Enabled: True if the cursor shows in reverse colors, False if it shows in inverse colors}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Window:PConsoleWindow;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console Window Cursor Reverse');
 {$ENDIF}

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_TEXT then Exit; {Text mode only}

 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;
 try
  {Check Console}
  if Window.Console = nil then Exit;

  {Check Cursor Reverse}
  if Window.CursorReverse <> Enabled then
   begin
    {Set Cursor Reverse}
    Window.CursorReverse:=Enabled;

    {Check Visible}
    if (Window.CursorState = CURSOR_STATE_ON) and (Window.WindowState = WINDOW_STATE_VISIBLE) then
     begin
      {Check Flag}
      if ((Window.WindowFlags and WINDOW_FLAG_FOCUS_CURSOR) = 0) or (Window.Console.WindowActive = Window) then
       begin
        {Update Caret}
        Result:=ConsoleDeviceUpdateCaretEx(Window.Console,Window.CaretHandle,Window.CaretX,Window.CaretY,Window.CursorForecolor,Window.CursorBackcolor,(Window.CursorState = CURSOR_STATE_ON),Window.CursorBlink,Window.CursorReverse);
        if Result <> ERROR_SUCCESS then Exit;
       end;
     end;
   end;

  {Return Result}
  Result:=ERROR_SUCCESS;
 finally
  {Unlock Window}
  MutexUnlock(Window.Lock);
 end;
end;

{==============================================================================}

function ConsoleWindowAddHistory(Handle:TWindowHandle;const Value:String):LongWord;
{Add a value to the command history table of an existing console window}
{Handle: The handle of the window to add to}
{Value: The text to add to the command history}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: When the number of entries in the table reaches the maximum the first entry will be removed}
var
 Window:PConsoleWindow;
 Prev:PConsoleHistory;
 Next:PConsoleHistory;
 History:PConsoleHistory;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console Window Add History');
 {$ENDIF}

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 {if Window.WindowMode <> WINDOW_MODE_TEXT then Exit;} {Allow any mode, other window classes should override if required}

 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;
 try
  {Check Length}
  if Length(Value) > 0 then
   begin
    {Check Last}
    History:=Window.HistoryLast;
    if History <> nil then
     begin
      if (Length(Value) = History.Length) and (Value = History.Value) then
       begin
        {Clear Current}
        Window.HistoryCurrent:=nil;

        {Return Result}
        Result:=ERROR_SUCCESS;
        Exit;
       end;
     end;

    {Check Maximum}
    if Window.HistoryCount >= WINDOW_HISTORY_MAX_COUNT then
     begin
      {Get First}
      History:=Window.HistoryFirst;
      if History = nil then Exit;

      {Delete First}
      if History.Next <> nil then
       begin
        {Not Last}
        Next:=History.Next;
        Next.Prev:=nil;
        Window.HistoryFirst:=Next;
       end
      else
       begin
        {Is Last}
        Window.HistoryFirst:=nil;
        Window.HistoryLast:=nil;
       end;

      {Update Count}
      Dec(Window.HistoryCount);

      {Free History}
      FreeMem(History);
     end;

    {Add History}
    History:=AllocMem(SizeOf(TConsoleHistory) + Length(Value) + 1);
    History.Value:=PChar(PtrUInt(History) + SizeOf(TConsoleHistory));
    History.Length:=Length(Value);
    StrLCopy(History.Value,PChar(Value),History.Length);

    {Link History}
    Prev:=Window.HistoryLast;
    if Prev = nil then
     begin
      {Is First}
      History.Prev:=nil;
      History.Next:=nil;
      Window.HistoryFirst:=History;
      Window.HistoryLast:=History;
     end
    else
     begin
      {Not First}
      Prev.Next:=History;
      History.Prev:=Prev;
      History.Next:=nil;
      Window.HistoryLast:=History;
     end;

    {Update Count}
    Inc(Window.HistoryCount);

    {Clear Current}
    Window.HistoryCurrent:=nil;
   end;

  {Return Result}
  Result:=ERROR_SUCCESS;
 finally
  {Unlock Window}
  MutexUnlock(Window.Lock);
 end;
end;

{==============================================================================}

function ConsoleWindowClearHistory(Handle:TWindowHandle):LongWord;
{Remove all entries from the command history table of an existing console window}
{Handle: The handle of the window to clear}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Window:PConsoleWindow;
 Current:PConsoleHistory;
 History:PConsoleHistory;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console Window Add History');
 {$ENDIF}

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 {if Window.WindowMode <> WINDOW_MODE_TEXT then Exit;} {Allow any mode, other window classes should override if required}

 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;
 try
  {Get First}
  History:=Window.HistoryFirst;
  while History <> nil do
   begin
    {Get Next}
    Current:=History;
    History:=Current.Next;

    {Free History}
    FreeMem(Current);
   end;

  {Reset First, Last and Current}
  Window.HistoryFirst:=nil;
  Window.HistoryLast:=nil;
  Window.HistoryCurrent:=nil;

  {Reset Count}
  Window.HistoryCount:=0;

  {Return Result}
  Result:=ERROR_SUCCESS;
 finally
  {Unlock Window}
  MutexUnlock(Window.Lock);
 end;
end;

{==============================================================================}

function ConsoleWindowFirstHistory(Handle:TWindowHandle):String;
{Get the first (oldest) command history value from an existing console window}
{Handle: The handle of the window to get from}
{Return: The command history value or an empty string on failure}
var
 Window:PConsoleWindow;
 History:PConsoleHistory;
begin
 {}
 Result:='';

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console Window First History');
 {$ENDIF}

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 {if Window.WindowMode <> WINDOW_MODE_TEXT then Exit;} {Allow any mode, other window classes should override if required}

 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;
 try
  {Get First}
  History:=Window.HistoryFirst;
  if History = nil then Exit;

  {Set Current}
  Window.HistoryCurrent:=History;

  {Return Result}
  Result:=StrPas(History.Value);
 finally
  {Unlock Window}
  MutexUnlock(Window.Lock);
 end;
end;

{==============================================================================}

function ConsoleWindowLastHistory(Handle:TWindowHandle):String;
{Get the last (most recent) command history value from an existing console window}
{Handle: The handle of the window to get from}
{Return: The command history value or an empty string on failure}
var
 Window:PConsoleWindow;
 History:PConsoleHistory;
begin
 {}
 Result:='';

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console Window Last History');
 {$ENDIF}

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 {if Window.WindowMode <> WINDOW_MODE_TEXT then Exit;} {Allow any mode, other window classes should override if required}

 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;
 try
  {Get Last}
  History:=Window.HistoryLast;
  if History = nil then Exit;

  {Set Current}
  Window.HistoryCurrent:=History;

  {Return Result}
  Result:=StrPas(History.Value);
 finally
  {Unlock Window}
  MutexUnlock(Window.Lock);
 end;
end;

{==============================================================================}

function ConsoleWindowNextHistory(Handle:TWindowHandle):String;
{Get the next (after current) command history value from an existing console window}
{Handle: The handle of the window to get from}
{Return: The command history value or an empty string on failure}
var
 Window:PConsoleWindow;
begin
 {}
 Result:='';

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console Window Next History');
 {$ENDIF}

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 {if Window.WindowMode <> WINDOW_MODE_TEXT then Exit;} {Allow any mode, other window classes should override if required}

 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;
 try
  {Check Current}
  if Window.HistoryCurrent = nil then
   begin
    {Nothing}
   end
  else if Window.HistoryCurrent = Window.HistoryLast then
   begin
    {Nothing}
   end
  else
   begin
    {Get Next}
    Window.HistoryCurrent:=Window.HistoryCurrent.Next;
    if Window.HistoryCurrent = nil then Exit;

    {Return Result}
    Result:=StrPas(Window.HistoryCurrent.Value);
   end;
 finally
  {Unlock Window}
  MutexUnlock(Window.Lock);
 end;
end;

{==============================================================================}

function ConsoleWindowPreviousHistory(Handle:TWindowHandle):String;
{Get the next (before current) command history value from an existing console window}
{Handle: The handle of the window to get from}
{Return: The command history value or an empty string on failure}

{Note: If there is no current history value the last value is returned}
var
 Window:PConsoleWindow;
begin
 {}
 Result:='';

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console Window Previous History');
 {$ENDIF}

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 {if Window.WindowMode <> WINDOW_MODE_TEXT then Exit;} {Allow any mode, other window classes should override if required}

 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;
 try
  {Check Current}
  if Window.HistoryCurrent = nil then
   begin
    {Get Last}
    Window.HistoryCurrent:=Window.HistoryLast;
    if Window.HistoryCurrent = nil then Exit;

    {Return Result}
    Result:=StrPas(Window.HistoryCurrent.Value);
   end
  else if Window.HistoryCurrent = Window.HistoryFirst then
   begin
    {Nothing}
   end
  else
   begin
    {Get Previous}
    Window.HistoryCurrent:=Window.HistoryCurrent.Prev;
    if Window.HistoryCurrent = nil then Exit;

    {Return Result}
    Result:=StrPas(Window.HistoryCurrent.Value);
   end;
 finally
  {Unlock Window}
  MutexUnlock(Window.Lock);
 end;
end;

{==============================================================================}

function ConsoleWindowCurrentHistory(Handle:TWindowHandle):String;
{Get the current command history value from an existing console window}
{Handle: The handle of the window to get from}
{Return: The command history value or an empty string on failure}

{Note: If there is no current history value the last value is returned}
var
 Window:PConsoleWindow;
 History:PConsoleHistory;
begin
 {}
 Result:='';

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console Window Previous History');
 {$ENDIF}

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 {if Window.WindowMode <> WINDOW_MODE_TEXT then Exit;} {Allow any mode, other window classes should override if required}

 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;
 try
  {Get Current}
  History:=Window.HistoryCurrent;
  if History = nil then
   begin
    {Get Last}
    History:=Window.HistoryLast;
    if History = nil then Exit;
   end;

  {Return Result}
  if History <> nil then Result:=StrPas(History.Value);
 finally
  {Unlock Window}
  MutexUnlock(Window.Lock);
 end;
end;

{==============================================================================}

function ConsoleWindowScrollUp(Handle:TWindowHandle;Row,Lines:LongWord):LongWord;
{Scroll the current viewport of an existing console window up}
{Handle: The handle of the window to scroll}
{Row: The starting row (Y) for the scroll up, all rows from top plus Lines down to Row will be scrolled up}
{Lines: The number of character lines to scroll up, Lines number of rows at the top will be discarded}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: The starting Row will be blanked with the background color}
var
 X1:LongWord;
 Y1:LongWord;
 X2:LongWord;
 Y2:LongWord;
 Count:LongWord;
 Window:PConsoleWindow;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console Window Scroll Up');
 {$ENDIF}

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_TEXT then Exit;

 {Check Visible}
 if Window.WindowState <> WINDOW_STATE_VISIBLE then
  begin
   Result:=ERROR_SUCCESS;
   Exit;
  end;

 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;
 try
  {Check Console}
  if Window.Console = nil then Exit;

  {Check Row}
  if Row < 2 then Exit; {Cannot scroll Row 1}
  if ((Window.MinY - 1) + Row) > Window.MaxY then Exit;

  {Check Lines}
  if Lines < 1 then Exit; {Must be at least 1 line}
  if Lines >= Row then Exit; {Cannot discard the starting Row}

  {Update Caret}
  if Window.CursorState = CURSOR_STATE_ON then
   begin
    ConsoleDeviceUpdateCaretEx(Window.Console,Window.CaretHandle,Window.CaretX,Window.CaretY,Window.CursorForecolor,Window.CursorBackcolor,False,Window.CursorBlink,Window.CursorReverse);
   end;

  {Calculate Count}
  Count:=Lines * (Window.FontHeight);

  {Calculate X1,Y1,X2,Y2}
  X1:=Window.X1 + Window.Borderwidth + Window.OffsetX + ((Window.MinX - 1) * Window.FontWidth);
  Y1:=Window.Y1 + Window.Borderwidth + Window.OffsetY + ((Window.MinY - 1) * Window.FontHeight) + (Lines * Window.FontHeight); {Start at top plus Lines}
  X2:=Window.X1 + Window.Borderwidth + Window.OffsetX + (Window.MaxX * Window.FontWidth);
  Y2:=Window.Y1 + Window.Borderwidth + Window.OffsetY + ((Window.MinY - 1) * Window.FontHeight) + (Row * Window.FontHeight); {Start at bottom of Row}

  {Check Character Mode}
  if (Window.WindowFlags and WINDOW_FLAG_CHARACTER) <> 0 then
   begin
    {Allow for Character mode}
    Dec(X2);
    Dec(Y2);
   end;

  {Console Scroll}
  Result:=ConsoleDeviceScroll(Window.Console,X1,Y1,X2,Y2,Count,CONSOLE_DIRECTION_UP);
  if Result <> ERROR_SUCCESS then Exit;

  {Recalculate Y1}
  Y1:=Window.Y1 + Window.Borderwidth + Window.OffsetY + ((Window.MinY - 1) * Window.FontHeight) + ((Row - Lines) * Window.FontHeight);

  {Console Draw Block}
  Result:=ConsoleDeviceDrawBlock(Window.Console,X1,Y1,X2,Y2,Window.Backcolor);

  {Update Caret}
  if Window.CursorState = CURSOR_STATE_ON then
   begin
    ConsoleDeviceUpdateCaretEx(Window.Console,Window.CaretHandle,Window.CaretX,Window.CaretY,Window.CursorForecolor,Window.CursorBackcolor,True,Window.CursorBlink,Window.CursorReverse);
   end;
 finally
  {Unlock Window}
  MutexUnlock(Window.Lock);
 end;
end;

{==============================================================================}

function ConsoleWindowScrollDown(Handle:TWindowHandle;Row,Lines:LongWord):LongWord;
{Scroll the current viewport of an existing console window down}
{Handle: The handle of the window to scroll}
{Row: The starting row (Y) for the scroll down, all rows from bottom minus Lines up to Row will be scrolled down}
{Lines: The number of character lines to scroll down, Lines number of rows at the bottom will be discarded}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: The starting Row will be blanked with the background color}
var
 X1:LongWord;
 Y1:LongWord;
 X2:LongWord;
 Y2:LongWord;
 Count:LongWord;
 Window:PConsoleWindow;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console Window Scroll Down');
 {$ENDIF}

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_TEXT then Exit;

 {Check Visible}
 if Window.WindowState <> WINDOW_STATE_VISIBLE then
  begin
   Result:=ERROR_SUCCESS;
   Exit;
  end;

 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;
 try
  {Check Console}
  if Window.Console = nil then Exit;

  {Check Row}
  if Row < 1 then Exit;
  if ((Window.MinY - 1) + Row) >= Window.MaxY then Exit; {Cannot scroll last Row}

  {Check Lines}
  if Lines < 1 then Exit; {Must be at least 1 line}
  if ((Window.MinY - 1) + Row + Lines) > Window.MaxY then Exit; {Cannot discard the starting Row} {Note: Previously >= MaxY}

  {Update Caret}
  if Window.CursorState = CURSOR_STATE_ON then
   begin
    ConsoleDeviceUpdateCaretEx(Window.Console,Window.CaretHandle,Window.CaretX,Window.CaretY,Window.CursorForecolor,Window.CursorBackcolor,False,Window.CursorBlink,Window.CursorReverse);
   end;

  {Calculate Count}
  Count:=Lines * (Window.FontHeight);

  {Calculate X1,Y1,X2,Y2}
  X1:=Window.X1 + Window.Borderwidth + Window.OffsetX + ((Window.MinX - 1) * Window.FontWidth);
  Y1:=Window.Y1 + Window.Borderwidth + Window.OffsetY + ((Window.MinY - 1) * Window.FontHeight) + ((Row - 1) * Window.FontHeight); {Start at top of Row}
  X2:=Window.X1 + Window.Borderwidth + Window.OffsetX + (Window.MaxX * Window.FontWidth);
  Y2:=(Window.Y1 + Window.Borderwidth + Window.OffsetY + (Window.MaxY * Window.FontHeight)) - (Lines * Window.FontHeight); {Start at bottom minus Lines}

  {Check Character Mode}
  if (Window.WindowFlags and WINDOW_FLAG_CHARACTER) <> 0 then
   begin
    {Allow for Character mode}
    Dec(X2);
    Dec(Y2);
   end;

  {Console Scroll}
  Result:=ConsoleDeviceScroll(Window.Console,X1,Y1,X2,Y2,Count,CONSOLE_DIRECTION_DOWN);
  if Result <> ERROR_SUCCESS then Exit;

  {Recalculate Y2}
  Y2:=Window.Y1 + Window.Borderwidth + Window.OffsetY + ((Window.MinY - 1) * Window.FontHeight) + ((Row + Lines - 1) * Window.FontHeight);

  {Check Character Mode}
  if (Window.WindowFlags and WINDOW_FLAG_CHARACTER) <> 0 then
   begin
    {Allow for Character mode}
    Dec(Y2);
   end;

  {Console Draw Block}
  Result:=ConsoleDeviceDrawBlock(Window.Console,X1,Y1,X2,Y2,Window.Backcolor);

  {Update Caret}
  if Window.CursorState = CURSOR_STATE_ON then
   begin
    ConsoleDeviceUpdateCaretEx(Window.Console,Window.CaretHandle,Window.CaretX,Window.CaretY,Window.CursorForecolor,Window.CursorBackcolor,True,Window.CursorBlink,Window.CursorReverse);
   end;
 finally
  {Unlock Window}
  MutexUnlock(Window.Lock);
 end;
end;

{==============================================================================}

function ConsoleWindowScrollLeft(Handle:TWindowHandle;Row,Col,Lines,Chars:LongWord):LongWord;
{Scroll the current viewport of an existing console window left}
{Handle: The handle of the window to scroll}
{Row: The starting row (Y) for the scroll left, all rows from Row down to Row + Lines will be scrolled left}
{Lines: The number of rows to scroll left, all rows from Row down to Row + Lines will be scrolled left}
{Col: The starting column (X) for the scroll left, all cols from left plus Chars to Col with be scrolled left}
{Chars: The number of characters to scroll left, Chars number of columns at the left will be discarded}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: The starting Col will be blanked with the background color}
var
 X1:LongWord;
 Y1:LongWord;
 X2:LongWord;
 Y2:LongWord;
 Count:LongWord;
 Window:PConsoleWindow;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console Window Scroll Left');
 {$ENDIF}

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_TEXT then Exit;

 {Check Visible}
 if Window.WindowState <> WINDOW_STATE_VISIBLE then
  begin
   Result:=ERROR_SUCCESS;
   Exit;
  end;

 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;
 try
  {Check Console}
  if Window.Console = nil then Exit;

  {Check Row}
  if Row < 1 then Exit;
  if ((Window.MinY - 1) + Row) > Window.MaxY then Exit;

  {Check Lines}
  if Lines < 1 then Exit; {Must be at least 1 line}
  if ((Window.MinY - 1) + (Row - 1) + Lines) > Window.MaxY then Exit;

  {Check Col}
  if Col < 2 then Exit; {Cannot scroll Col 1}
  if ((Window.MinX - 1) + Col) > Window.MaxX then Exit;

  {Check Chars}
  if Chars < 1 then Exit; {Must be at least 1 character}
  if Chars >= Col then Exit; {Cannot discard the starting Col}

  {Update Caret}
  if Window.CursorState = CURSOR_STATE_ON then
   begin
    ConsoleDeviceUpdateCaretEx(Window.Console,Window.CaretHandle,Window.CaretX,Window.CaretY,Window.CursorForecolor,Window.CursorBackcolor,False,Window.CursorBlink,Window.CursorReverse);
   end;

  {Calculate Count}
  Count:=Chars * (Window.FontWidth);

  {Calculate X1,Y1,X2,Y2}
  X1:=Window.X1 + Window.Borderwidth + Window.OffsetX + ((Window.MinX - 1) * Window.FontWidth) + (Chars * Window.FontWidth); {Start at left plus Chars};
  Y1:=Window.Y1 + Window.Borderwidth + Window.OffsetY + ((Window.MinY - 1) * Window.FontHeight) + ((Row - 1) * Window.FontHeight); {Start at top of Row}
  X2:=Window.X1 + Window.Borderwidth + Window.OffsetX + ((Window.MinX - 1) * Window.FontWidth) + (Col * Window.FontWidth); {Start at right of Col}
  Y2:=Window.Y1 + Window.Borderwidth + Window.OffsetY + ((Window.MinY - 1) * Window.FontHeight) + (((Row - 1) + Lines) * Window.FontHeight); {Start at bottom of Row plus Lines}

  {Check Character Mode}
  if (Window.WindowFlags and WINDOW_FLAG_CHARACTER) <> 0 then
   begin
    {Allow for Character mode}
    Dec(X2);
    Dec(Y2);
   end;

  {Console Scroll}
  Result:=ConsoleDeviceScroll(Window.Console,X1,Y1,X2,Y2,Count,CONSOLE_DIRECTION_LEFT);
  if Result <> ERROR_SUCCESS then Exit;

  {Recalculate X1}
  X1:=Window.X1 + Window.Borderwidth + Window.OffsetX + ((Window.MinX - 1) * Window.FontWidth) + ((Col - Chars) * Window.FontWidth);

  {Console Draw Block}
  Result:=ConsoleDeviceDrawBlock(Window.Console,X1,Y1,X2,Y2,Window.Backcolor);

  {Update Caret}
  if Window.CursorState = CURSOR_STATE_ON then
   begin
    ConsoleDeviceUpdateCaretEx(Window.Console,Window.CaretHandle,Window.CaretX,Window.CaretY,Window.CursorForecolor,Window.CursorBackcolor,True,Window.CursorBlink,Window.CursorReverse);
   end;
 finally
  {Unlock Window}
  MutexUnlock(Window.Lock);
 end;
end;

{==============================================================================}

function ConsoleWindowScrollRight(Handle:TWindowHandle;Row,Col,Lines,Chars:LongWord):LongWord;
{Scroll the current viewport of an existing console window right}
{Handle: The handle of the window to scroll}
{Row: The starting row (Y) for the scroll right, all rows from Row down to Row + Lines will be scrolled right}
{Lines: The number of rows to scroll right, all rows from Row down to Row + Lines will be scrolled right}
{Col: The starting column (X) for the scroll right, all rows from right minus Chars to Col will be scrolled right}
{Chars: The number of characters to scroll right, Chars number of columns at the right will be discarded}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: The starting Col will be blanked with the background color}
var
 X1:LongWord;
 Y1:LongWord;
 X2:LongWord;
 Y2:LongWord;
 Count:LongWord;
 Window:PConsoleWindow;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console Window Scroll Right');
 {$ENDIF}

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_TEXT then Exit;

 {Check Visible}
 if Window.WindowState <> WINDOW_STATE_VISIBLE then
  begin
   Result:=ERROR_SUCCESS;
   Exit;
  end;

 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;
 try
  {Check Console}
  if Window.Console = nil then Exit;

  {Check Row}
  if Row < 1 then Exit;
  if ((Window.MinY - 1) + Row) > Window.MaxY then Exit;

  {Check Lines}
  if Lines < 1 then Exit; {Must be at least 1 line}
  if ((Window.MinY - 1) + (Row - 1) + Lines) > Window.MaxY then Exit;

  {Check Col}
  if Col < 1 then Exit;
  if ((Window.MinX - 1) + Col) >= Window.MaxX then Exit; {Cannot scroll last Col}

  {Check Chars}
  if Lines < 1 then Exit; {Must be at least 1 character}
  if ((Window.MinX - 1) + Col + Chars) > Window.MaxX then Exit; {Cannot discard the starting Col}

  {Update Caret}
  if Window.CursorState = CURSOR_STATE_ON then
   begin
    ConsoleDeviceUpdateCaretEx(Window.Console,Window.CaretHandle,Window.CaretX,Window.CaretY,Window.CursorForecolor,Window.CursorBackcolor,False,Window.CursorBlink,Window.CursorReverse);
   end;

  {Calculate Count}
  Count:=Chars * (Window.FontWidth);

  {Calculate X1,Y1,X2,Y2}
  X1:=Window.X1 + Window.Borderwidth + Window.OffsetX + ((Window.MinX - 1) * Window.FontWidth) + ((Col - 1) * Window.FontWidth); {Start at left of Col}
  Y1:=Window.Y1 + Window.Borderwidth + Window.OffsetY + ((Window.MinY - 1) * Window.FontHeight) + ((Row - 1) * Window.FontHeight); {Start at top of Row}
  X2:=(Window.X1 + Window.Borderwidth + Window.OffsetX + (Window.MaxX * Window.FontWidth)) - (Chars * Window.FontWidth); {Start at right minus Chars}
  Y2:=Window.Y1 + Window.Borderwidth + Window.OffsetY + ((Window.MinY - 1) * Window.FontHeight) + (((Row - 1) + Lines) * Window.FontHeight); {Start at bottom of Row plus Lines}

  {Check Character Mode}
  if (Window.WindowFlags and WINDOW_FLAG_CHARACTER) <> 0 then
   begin
    {Allow for Character mode}
    Dec(X2);
    Dec(Y2);
   end;

  {Console Scroll}
  Result:=ConsoleDeviceScroll(Window.Console,X1,Y1,X2,Y2,Count,CONSOLE_DIRECTION_RIGHT);
  if Result <> ERROR_SUCCESS then Exit;

  {Recalculate X2}
  X2:=Window.X1 + Window.Borderwidth + Window.OffsetX + ((Window.MinX - 1) * Window.FontWidth) + ((Col + Chars - 1) * Window.FontWidth);

  {Check Character Mode}
  if (Window.WindowFlags and WINDOW_FLAG_CHARACTER) <> 0 then
   begin
    {Allow for Character mode}
    Dec(Y2);
   end;

  {Console Draw Block}
  Result:=ConsoleDeviceDrawBlock(Window.Console,X1,Y1,X2,Y2,Window.Backcolor);

  {Update Caret}
  if Window.CursorState = CURSOR_STATE_ON then
   begin
    ConsoleDeviceUpdateCaretEx(Window.Console,Window.CaretHandle,Window.CaretX,Window.CaretY,Window.CursorForecolor,Window.CursorBackcolor,True,Window.CursorBlink,Window.CursorReverse);
   end;
 finally
  {Unlock Window}
  MutexUnlock(Window.Lock);
 end;
end;

{==============================================================================}

function ConsoleWindowClear(Handle:TWindowHandle):LongWord;
{Clear the current viewport of an existing console window}
{Handle: The handle of the window to clear}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 ClearX1:LongWord;
 ClearY1:LongWord;
 ClearX2:LongWord;
 ClearY2:LongWord;
 Window:PConsoleWindow;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console Window Clear');
 {$ENDIF}

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_TEXT then Exit;

 {Check Visible}
 if Window.WindowState <> WINDOW_STATE_VISIBLE then
  begin
   Result:=ERROR_SUCCESS;
   Exit;
  end;

 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;
 try
  {Check Console}
  if Window.Console = nil then Exit;

  {Update Caret}
  if Window.CursorState = CURSOR_STATE_ON then
   begin
    ConsoleDeviceUpdateCaretEx(Window.Console,Window.CaretHandle,Window.CaretX,Window.CaretY,Window.CursorForecolor,Window.CursorBackcolor,False,Window.CursorBlink,Window.CursorReverse);
   end;

  {Calculate X1,Y1,X2,Y2}
  ClearX1:=Window.X1 + Window.Borderwidth + Window.OffsetX + ((Window.MinX - 1) * Window.FontWidth);
  ClearY1:=Window.Y1 + Window.Borderwidth + Window.OffsetY + ((Window.MinY - 1) * Window.FontHeight);
  ClearX2:=Window.X1 + Window.Borderwidth + Window.OffsetX + (Window.MaxX * Window.FontWidth);
  ClearY2:=Window.Y1 + Window.Borderwidth + Window.OffsetY + (Window.MaxY * Window.FontHeight);

  {Check Character Mode}
  if (Window.WindowFlags and WINDOW_FLAG_CHARACTER) <> 0 then
   begin
    {Allow for Character mode}
    Dec(ClearX2);
    Dec(ClearY2);
   end;

  {Console Draw Block}
  Result:=ConsoleDeviceDrawBlock(Window.Console,ClearX1,ClearY1,ClearX2,ClearY2,Window.Backcolor);
  if Result <> ERROR_SUCCESS then Exit;

  {Update X,Y}
  Window.X:=1;
  Window.Y:=1;

  {Update Cursor}
  ConsoleWindowCursorMove(Handle,Window.X,Window.Y);
 finally
  {Unlock Window}
  MutexUnlock(Window.Lock);
 end;
end;

{==============================================================================}

function ConsoleWindowClearEx(Handle:TWindowHandle;X1,Y1,X2,Y2:LongWord;Cursor:Boolean):LongWord;
{Clear part of the the current viewport of an existing console window}
{Handle: The handle of the window to clear}
{X1: The left edge of the area to clear (relative to current viewport)}
{Y1: The top edge of the area to clear (relative to current viewport)}
{X2: The right edge of the area to clear (relative to current viewport)}
{Y2: The bottom edge of the area to clear (relative to current viewport)}
{Cursor: If True update the cursor position after clearing}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: For Text Console functions, Window is based on screen character rows and columns not screen pixels}
var
 ClearX1:LongWord;
 ClearY1:LongWord;
 ClearX2:LongWord;
 ClearY2:LongWord;
 Window:PConsoleWindow;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console Window Clear Ex');
 {$ENDIF}

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_TEXT then Exit;

 {Check Visible}
 if Window.WindowState <> WINDOW_STATE_VISIBLE then
  begin
   Result:=ERROR_SUCCESS;
   Exit;
  end;

 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;
 try
  {Check Console}
  if Window.Console = nil then Exit;

  {Check X1,Y1,X2,Y2}
  if X1 < 1 then Exit;
  if Y1 < 1 then Exit;
  if X1 > X2 then Exit;
  if Y1 > Y2 then Exit;
  if ((Window.MinX - 1) + X1) > Window.MaxX then Exit;
  if ((Window.MinY - 1) + Y1) > Window.MaxY then Exit;
  if ((Window.MinX - 1) + X2) > Window.MaxX then Exit;
  if ((Window.MinY - 1) + Y2) > Window.MaxY then Exit;

  {Update Caret}
  if Window.CursorState = CURSOR_STATE_ON then
   begin
    ConsoleDeviceUpdateCaretEx(Window.Console,Window.CaretHandle,Window.CaretX,Window.CaretY,Window.CursorForecolor,Window.CursorBackcolor,False,Window.CursorBlink,Window.CursorReverse);
   end;

  {Calculate X1,Y1,X2,Y2}
  ClearX1:=Window.X1 + Window.Borderwidth + Window.OffsetX + ((Window.MinX - 1) * Window.FontWidth) + ((X1 - 1) * Window.FontWidth);
  ClearY1:=Window.Y1 + Window.Borderwidth + Window.OffsetY + ((Window.MinY - 1) * Window.FontHeight) + ((Y1 - 1) * Window.FontHeight);
  ClearX2:=Window.X1 + Window.Borderwidth + Window.OffsetX + ((Window.MinX - 1) * Window.FontWidth) + (X2 * Window.FontWidth);
  ClearY2:=Window.Y1 + Window.Borderwidth + Window.OffsetY + ((Window.MinY - 1) * Window.FontHeight) + (Y2 * Window.FontHeight);

  {Check Character Mode}
  if (Window.WindowFlags and WINDOW_FLAG_CHARACTER) <> 0 then
   begin
    {Allow for Character mode}
    Dec(ClearX2);
    Dec(ClearY2);
   end;

  {Console Draw Block}
  Result:=ConsoleDeviceDrawBlock(Window.Console,ClearX1,ClearY1,ClearX2,ClearY2,Window.Backcolor);
  if Result <> ERROR_SUCCESS then Exit;

  {Check Cursor}
  if Cursor then
   begin
    {Update X,Y}
    Window.X:=1;
    Window.Y:=1;

    {Update Cursor}
    ConsoleWindowCursorMove(Handle,Window.X,Window.Y);
   end
  else
   begin
    {Update Caret}
    if Window.CursorState = CURSOR_STATE_ON then
     begin
      ConsoleDeviceUpdateCaretEx(Window.Console,Window.CaretHandle,Window.CaretX,Window.CaretY,Window.CursorForecolor,Window.CursorBackcolor,True,Window.CursorBlink,Window.CursorReverse);
     end;
   end;
 finally
  {Unlock Window}
  MutexUnlock(Window.Lock);
 end;
end;

{==============================================================================}

function ConsoleWindowWrite(Handle:TWindowHandle;const AText:String):LongWord;
{Write text on an existing console window at the current position in the current color}
{Handle: The handle of the window to write text on}
{Text: The text to write}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: The window will not scroll up at the end of the line}
var
 WriteX:LongWord;
 WriteY:LongWord;
 WriteBuffer:String;
 WriteLength:LongWord;
 Window:PConsoleWindow;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console Window Write');
 {$ENDIF}

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_TEXT then Exit;

 {Check Visible}
 if Window.WindowState <> WINDOW_STATE_VISIBLE then
  begin
   Result:=ERROR_SUCCESS;
   Exit;
  end;

 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;
 try
  {Check Console}
  if Window.Console = nil then Exit;

  {Update Caret}
  if Window.CursorState = CURSOR_STATE_ON then
   begin
    ConsoleDeviceUpdateCaretEx(Window.Console,Window.CaretHandle,Window.CaretX,Window.CaretY,Window.CursorForecolor,Window.CursorBackcolor,False,Window.CursorBlink,Window.CursorReverse);
   end;

  {Get Length}
  WriteBuffer:=AText;
  WriteLength:=Length(WriteBuffer);
  if ((Window.MinX - 1) + (Window.X - 1) + WriteLength) > Window.MaxX then WriteLength:=Window.MaxX - ((Window.MinX - 1) + (Window.X - 1));

  {Check Length}
  while (WriteLength > 0) do
   begin
    {Calculate X,Y}
    WriteX:=Window.X1 + Window.Borderwidth + Window.OffsetX + ((Window.MinX - 1) * Window.FontWidth) + ((Window.X - 1) * Window.FontWidth);
    WriteY:=Window.Y1 + Window.Borderwidth + Window.OffsetY + ((Window.MinY - 1) * Window.FontHeight) + ((Window.Y - 1) * Window.FontHeight);

    {Console Draw Text}
    Result:=ConsoleDeviceDrawText(Window.Console,Window.Font,WriteBuffer,WriteX,WriteY,Window.Forecolor,Window.Backcolor,WriteLength);
    if Result <> ERROR_SUCCESS then Exit;

    {Update X}
    Inc(Window.X,WriteLength);
    if Window.X > Window.Cols then
     begin
      Window.X:=1;
      Inc(Window.Y);
     end;

    {Update Y}
    if Window.Y > Window.Rows then
     begin
      Window.Y:=Window.Rows;

      {Check Scroll}
      if (Window.WindowFlags and WINDOW_FLAG_AUTO_SCROLL) <> 0 then
       begin
        {Console Scroll Up}
        Result:=ConsoleWindowScrollUp(Handle,Window.Y,1);
        if Result <> ERROR_SUCCESS then Exit;
       end;
     end;

    {Check Wrap}
    if (Window.WindowFlags and WINDOW_FLAG_LINE_WRAP) = 0 then Break;

    {Get Length}
    Delete(WriteBuffer,1,WriteLength);
    WriteLength:=Length(WriteBuffer);
    if ((Window.MinX - 1) + (Window.X - 1) + WriteLength) > Window.MaxX then WriteLength:=Window.MaxX - ((Window.MinX - 1) + (Window.X - 1));
   end;

  {Update Cursor}
  ConsoleWindowCursorMove(Handle,Window.X,Window.Y);

  {Return Result}
  Result:=ERROR_SUCCESS;
 finally
  {Unlock Window}
  MutexUnlock(Window.Lock);
 end;
end;

{==============================================================================}

function ConsoleWindowWriteEx(Handle:TWindowHandle;const AText:String;X,Y,Forecolor,Backcolor:LongWord):LongWord;
{Write text on an existing console window}
{Handle: The handle of the window to write text on}
{Text: The text to write}
{X: The column to start writing the text at}
{Y: The row to start writing the text at}
{Forecolor: The foreground color to use (eg COLOR_WHITE)}
{Backcolor: The background color to use (eg COLOR_BLACK)}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: For Text Console functions, X and Y are based on screen character rows and columns not screen pixels}
{Note: The window will not scroll up at the end of the line}
var
 WriteX:LongWord;
 WriteY:LongWord;
 WriteBuffer:String;
 WriteLength:LongWord;
 Window:PConsoleWindow;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Colors}
 {if Forecolor = COLOR_NONE then Exit;}
 {if Backcolor = COLOR_NONE then Exit;}

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console Window Write Ex');
 {$ENDIF}

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_TEXT then Exit;

 {Check Visible}
 if Window.WindowState <> WINDOW_STATE_VISIBLE then
  begin
   Result:=ERROR_SUCCESS;
   Exit;
  end;

 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;
 try
  {Check Console}
  if Window.Console = nil then Exit;

  {Check X,Y}
  if X < 1 then Exit;
  if Y < 1 then Exit;
  if ((Window.MinX - 1) + X) > Window.MaxX then Exit;
  if ((Window.MinY - 1) + Y) > Window.MaxY then Exit;

  {Update Caret}
  if Window.CursorState = CURSOR_STATE_ON then
   begin
    ConsoleDeviceUpdateCaretEx(Window.Console,Window.CaretHandle,Window.CaretX,Window.CaretY,Window.CursorForecolor,Window.CursorBackcolor,False,Window.CursorBlink,Window.CursorReverse);
   end;

  {Get Length}
  WriteBuffer:=AText;
  WriteLength:=Length(WriteBuffer);
  if ((Window.MinX - 1) + (X - 1) + WriteLength) > Window.MaxX then WriteLength:=Window.MaxX - ((Window.MinX - 1) + (X - 1));

  {Check Length}
  while (WriteLength > 0) do
   begin
    {Calculate X,Y}
    WriteX:=Window.X1 + Window.Borderwidth + Window.OffsetX + ((Window.MinX - 1) * Window.FontWidth) + ((X - 1) * Window.FontWidth);
    WriteY:=Window.Y1 + Window.Borderwidth + Window.OffsetY + ((Window.MinY - 1) * Window.FontHeight) + ((Y - 1) * Window.FontHeight);

    {Console Draw Text}
    Result:=ConsoleDeviceDrawText(Window.Console,Window.Font,WriteBuffer,WriteX,WriteY,Forecolor,Backcolor,WriteLength);
    if Result <> ERROR_SUCCESS then Exit;

    {Update X}
    Inc(X,WriteLength);
    if X > Window.Cols then
     begin
      X:=1;
      Inc(Y);
     end;

    {Update Y}
    if Y > Window.Rows then
     begin
      Y:=Window.Rows;

      {Check Scroll}
      if (Window.WindowFlags and WINDOW_FLAG_AUTO_SCROLL) <> 0 then
       begin
        {Console Scroll Up}
        Result:=ConsoleWindowScrollUp(Handle,Y,1);
        if Result <> ERROR_SUCCESS then Exit;
       end;
     end;

    {Check Wrap}
    if (Window.WindowFlags and WINDOW_FLAG_LINE_WRAP) = 0 then Break;

    {Get Length}
    Delete(WriteBuffer,1,WriteLength);
    WriteLength:=Length(WriteBuffer);
    if ((Window.MinX - 1) + (X - 1) + WriteLength) > Window.MaxX then WriteLength:=Window.MaxX - ((Window.MinX - 1) + (X - 1));
   end;

  {Update Caret}
  if Window.CursorState = CURSOR_STATE_ON then
   begin
    ConsoleDeviceUpdateCaretEx(Window.Console,Window.CaretHandle,Window.CaretX,Window.CaretY,Window.CursorForecolor,Window.CursorBackcolor,True,Window.CursorBlink,Window.CursorReverse);
   end;

  {No Cursor Update}

  {Return Result}
  Result:=ERROR_SUCCESS;
 finally
  {Unlock Window}
  MutexUnlock(Window.Lock);
 end;
end;

{==============================================================================}

function ConsoleWindowWriteLn(Handle:TWindowHandle;const AText:String):LongWord;
{Write text on an existing console window at the current position in the current color}
{Handle: The handle of the window to write text on}
{Text: The text to write}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: The window will scroll up at the end of the line}
var
 WriteX:LongWord;
 WriteY:LongWord;
 WriteBuffer:String;
 WriteLength:LongWord;
 Window:PConsoleWindow;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console Window WriteLn');
 {$ENDIF}

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_TEXT then Exit;

 {Check Visible}
 if Window.WindowState <> WINDOW_STATE_VISIBLE then
  begin
   Result:=ERROR_SUCCESS;
   Exit;
  end;

 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;
 try
  {Check Console}
  if Window.Console = nil then Exit;

  {Update Caret}
  if Window.CursorState = CURSOR_STATE_ON then
   begin
    ConsoleDeviceUpdateCaretEx(Window.Console,Window.CaretHandle,Window.CaretX,Window.CaretY,Window.CursorForecolor,Window.CursorBackcolor,False,Window.CursorBlink,Window.CursorReverse);
   end;

  {Get Length}
  WriteBuffer:=AText;
  WriteLength:=Length(WriteBuffer);
  if ((Window.MinX - 1) + (Window.X - 1) + WriteLength) > Window.MaxX then WriteLength:=Window.MaxX - ((Window.MinX - 1) + (Window.X - 1));

  {Check Length}
  while (WriteLength > 0) do
   begin
    {Calculate X,Y}
    WriteX:=Window.X1 + Window.Borderwidth + Window.OffsetX + ((Window.MinX - 1) * Window.FontWidth) + ((Window.X - 1) * Window.FontWidth);
    WriteY:=Window.Y1 + Window.Borderwidth + Window.OffsetY + ((Window.MinY - 1) * Window.FontHeight) + ((Window.Y - 1) * Window.FontHeight);

    {Console Draw Text}
    Result:=ConsoleDeviceDrawText(Window.Console,Window.Font,WriteBuffer,WriteX,WriteY,Window.Forecolor,Window.Backcolor,WriteLength);
    if Result <> ERROR_SUCCESS then Exit;

    {Update X}
    Inc(Window.X,WriteLength);
    if Window.X > Window.Cols then
     begin
      Window.X:=1;
      Inc(Window.Y);
     end;

    {Update Y}
    if Window.Y > Window.Rows then
     begin
      Window.Y:=Window.Rows;

      {Check Scroll}
      if (Window.WindowFlags and WINDOW_FLAG_AUTO_SCROLL) <> 0 then
       begin
        {Console Scroll Up}
        Result:=ConsoleWindowScrollUp(Handle,Window.Y,1);
        if Result <> ERROR_SUCCESS then Exit;
       end;
     end;

    {Check Wrap}
    if (Window.WindowFlags and WINDOW_FLAG_LINE_WRAP) = 0 then Break;

    {Get Length}
    Delete(WriteBuffer,1,WriteLength);
    WriteLength:=Length(WriteBuffer);
    if ((Window.MinX - 1) + (Window.X - 1) + WriteLength) > Window.MaxX then WriteLength:=Window.MaxX - ((Window.MinX - 1) + (Window.X - 1));
   end;

  {Update X,Y}
  Window.X:=1;
  Inc(Window.Y);
  if Window.Y > Window.Rows then
   begin
    Window.Y:=Window.Rows;

    {Check Scroll}
    if (Window.WindowFlags and WINDOW_FLAG_AUTO_SCROLL) <> 0 then
     begin
      {Console Scroll Up}
      Result:=ConsoleWindowScrollUp(Handle,Window.Y,1);
      if Result <> ERROR_SUCCESS then Exit;
     end;
   end;

  {Update Cursor}
  ConsoleWindowCursorMove(Handle,Window.X,Window.Y);

  {Return Result}
  Result:=ERROR_SUCCESS;
 finally
  {Unlock Window}
  MutexUnlock(Window.Lock);
 end;
end;

{==============================================================================}

function ConsoleWindowWriteLnEx(Handle:TWindowHandle;const AText:String;X,Y,Forecolor,Backcolor:LongWord):LongWord;
{Write text on an existing console window}
{Handle: The handle of the window to write text on}
{Text: The text to write}
{X: The column to start writing the text at}
{Y: The row to start writing the text at}
{Forecolor: The foreground color to use (eg COLOR_WHITE)}
{Backcolor: The background color to use (eg COLOR_BLACK)}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: For Text Console functions, X and Y are based on character rows and columns not screen pixels}
{Note: The window will scroll up at the end of the line}
var
 WriteX:LongWord;
 WriteY:LongWord;
 WriteBuffer:String;
 WriteLength:LongWord;
 Window:PConsoleWindow;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Colors}
 {if Forecolor = COLOR_NONE then Exit;}
 {if Backcolor = COLOR_NONE then Exit;}

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console Window WriteLn Ex');
 {$ENDIF}

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_TEXT then Exit;

 {Check Visible}
 if Window.WindowState <> WINDOW_STATE_VISIBLE then
  begin
   Result:=ERROR_SUCCESS;
   Exit;
  end;

 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;
 try
  {Check Console}
  if Window.Console = nil then Exit;

  {Check X,Y}
  if X < 1 then Exit;
  if Y < 1 then Exit;
  if ((Window.MinX - 1) + X) > Window.MaxX then Exit;
  if ((Window.MinY - 1) + Y) > Window.MaxY then Exit;

  {Update Caret}
  if Window.CursorState = CURSOR_STATE_ON then
   begin
    ConsoleDeviceUpdateCaretEx(Window.Console,Window.CaretHandle,Window.CaretX,Window.CaretY,Window.CursorForecolor,Window.CursorBackcolor,False,Window.CursorBlink,Window.CursorReverse);
   end;

  {Get Length}
  WriteBuffer:=AText;
  WriteLength:=Length(WriteBuffer);
  if ((Window.MinX - 1) + (X - 1) + WriteLength) > Window.MaxX then WriteLength:=Window.MaxX - ((Window.MinX - 1) + (X - 1));

  {Check Length}
  while (WriteLength > 0) do
   begin
    {Calculate X,Y}
    WriteX:=Window.X1 + Window.Borderwidth + Window.OffsetX + ((Window.MinX - 1) * Window.FontWidth) + ((X - 1) * Window.FontWidth);
    WriteY:=Window.Y1 + Window.Borderwidth + Window.OffsetY + ((Window.MinY - 1) * Window.FontHeight) + ((Y - 1) * Window.FontHeight);

    {Console Draw Text}
    Result:=ConsoleDeviceDrawText(Window.Console,Window.Font,WriteBuffer,WriteX,WriteY,Forecolor,Backcolor,WriteLength);
    if Result <> ERROR_SUCCESS then Exit;

    {Update X}
    Inc(X,WriteLength);
    if X > Window.Cols then
     begin
      X:=1;
      Inc(Y);
     end;

    {Update Y}
    if Y > Window.Rows then
     begin
      Y:=Window.Rows;

      {Check Scroll}
      if (Window.WindowFlags and WINDOW_FLAG_AUTO_SCROLL) <> 0 then
       begin
        {Console Scroll Up}
        Result:=ConsoleWindowScrollUp(Handle,Y,1);
        if Result <> ERROR_SUCCESS then Exit;
       end;
     end;

    {Check Wrap}
    if (Window.WindowFlags and WINDOW_FLAG_LINE_WRAP) = 0 then Break;

    {Get Length}
    Delete(WriteBuffer,1,WriteLength);
    WriteLength:=Length(WriteBuffer);
    if ((Window.MinX - 1) + (X - 1) + WriteLength) > Window.MaxX then WriteLength:=Window.MaxX - ((Window.MinX - 1) + (X - 1));
   end;

  {Update X,Y}
  X:=1;
  Inc(Y);
  if Y > Window.Rows then
   begin
    Y:=Window.Rows;

    {Check Scroll}
    if (Window.WindowFlags and WINDOW_FLAG_AUTO_SCROLL) <> 0 then
     begin
      {Console Scroll Up}
      Result:=ConsoleWindowScrollUp(Handle,Y,1);
      if Result <> ERROR_SUCCESS then Exit;
     end;
   end;

  {Update Caret}
  if Window.CursorState = CURSOR_STATE_ON then
   begin
    ConsoleDeviceUpdateCaretEx(Window.Console,Window.CaretHandle,Window.CaretX,Window.CaretY,Window.CursorForecolor,Window.CursorBackcolor,True,Window.CursorBlink,Window.CursorReverse);
   end;

  {No Cursor Update}

  {Return Result}
  Result:=ERROR_SUCCESS;
 finally
  {Unlock Window}
  MutexUnlock(Window.Lock);
 end;
end;

{==============================================================================}

function ConsoleWindowWriteChr(Handle:TWindowHandle;AChr:Char):LongWord;
{Write a character on an existing console window at the current position in the current color}
{Handle: The handle of the window to write the character on}
{Chr: The character to write}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 WriteX:LongWord;
 WriteY:LongWord;
 Window:PConsoleWindow;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console Window WriteChr');
 {$ENDIF}

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_TEXT then Exit;

 {Check Visible}
 if Window.WindowState <> WINDOW_STATE_VISIBLE then
  begin
   Result:=ERROR_SUCCESS;
   Exit;
  end;

 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;
 try
  {Check Console}
  if Window.Console = nil then Exit;

  {Update Caret}
  if Window.CursorState = CURSOR_STATE_ON then
   begin
    ConsoleDeviceUpdateCaretEx(Window.Console,Window.CaretHandle,Window.CaretX,Window.CaretY,Window.CursorForecolor,Window.CursorBackcolor,False,Window.CursorBlink,Window.CursorReverse);
   end;

  {Check Char}
  case AChr of
   #10:begin
     case DefaultTextLineBreakStyle of
      tlbsLF,tlbsCRLF:begin
        {Update X,Y}
        Window.X:=1;
        Inc(Window.Y);
        if Window.Y > Window.Rows then
         begin
          Window.Y:=Window.Rows;

          {Check Scroll}
          if (Window.WindowFlags and WINDOW_FLAG_AUTO_SCROLL) <> 0 then
           begin
            {Console Scroll Up}
            Result:=ConsoleWindowScrollUp(Handle,Window.Y,1);
            if Result <> ERROR_SUCCESS then Exit;
           end;
         end;
       end;
     end;
    end;
   #13:begin
     case DefaultTextLineBreakStyle of
      tlbsCR:begin
        {Update X,Y}
        Window.X:=1;
        Inc(Window.Y);
        if Window.Y > Window.Rows then
         begin
          Window.Y:=Window.Rows;

          {Check Scroll}
          if (Window.WindowFlags and WINDOW_FLAG_AUTO_SCROLL) <> 0 then
           begin
            {Console Scroll Up}
            Result:=ConsoleWindowScrollUp(Handle,Window.Y,1);
            if Result <> ERROR_SUCCESS then Exit;
           end;
         end;
       end;
     end;
    end;
   else
    begin
     {Calculate X,Y}
     WriteX:=Window.X1 + Window.Borderwidth + Window.OffsetX + ((Window.MinX - 1) * Window.FontWidth) + ((Window.X - 1) * Window.FontWidth);
     WriteY:=Window.Y1 + Window.Borderwidth + Window.OffsetY + ((Window.MinY - 1) * Window.FontHeight) + ((Window.Y - 1) * Window.FontHeight);

     {Console Draw Char}
     Result:=ConsoleDeviceDrawChar(Window.Console,Window.Font,AChr,WriteX,WriteY,Window.Forecolor,Window.Backcolor);
     if Result <> ERROR_SUCCESS then Exit;

     {Update X}
     Inc(Window.X);
     if Window.X > Window.Cols then
      begin
       Window.X:=1;
       Inc(Window.Y);
      end;

     {Update Y}
     if Window.Y > Window.Rows then
      begin
       Window.Y:=Window.Rows;

       {Check Scroll}
       if (Window.WindowFlags and WINDOW_FLAG_AUTO_SCROLL) <> 0 then
        begin
         {Console Scroll Up}
         Result:=ConsoleWindowScrollUp(Handle,Window.Y,1);
         if Result <> ERROR_SUCCESS then Exit;
        end;
      end;
    end;
  end;

  {Update Cursor}
  ConsoleWindowCursorMove(Handle,Window.X,Window.Y);

  {Return Result}
  Result:=ERROR_SUCCESS;
 finally
  {Unlock Window}
  MutexUnlock(Window.Lock);
 end;
end;

{==============================================================================}

function ConsoleWindowWriteChrEx(Handle:TWindowHandle;AChr:Char;X,Y,Forecolor,Backcolor:LongWord):LongWord;
{Write a character on an existing console window}
{Handle: The handle of the window to write the character on}
{Chr: The character to write}
{X: The column to start writing the character at}
{Y: The row to start writing the character at}
{Forecolor: The foreground color to use (eg COLOR_WHITE)}
{Backcolor: The background color to use (eg COLOR_BLACK)}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: For Text Console functions, X and Y are based on character rows and columns not screen pixels}
var
 WriteX:LongWord;
 WriteY:LongWord;
 Window:PConsoleWindow;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Colors}
 {if Forecolor = COLOR_NONE then Exit;}
 {if Backcolor = COLOR_NONE then Exit;}

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console Window WriteChr Ex');
 {$ENDIF}

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_TEXT then Exit;

 {Check Visible}
 if Window.WindowState <> WINDOW_STATE_VISIBLE then
  begin
   Result:=ERROR_SUCCESS;
   Exit;
  end;

 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;
 try
  {Check Console}
  if Window.Console = nil then Exit;

  {Check X,Y}
  if X < 1 then Exit;
  if Y < 1 then Exit;
  if ((Window.MinX - 1) + X) > Window.MaxX then Exit;
  if ((Window.MinY - 1) + Y) > Window.MaxY then Exit;

  {Update Caret}
  if Window.CursorState = CURSOR_STATE_ON then
   begin
    ConsoleDeviceUpdateCaretEx(Window.Console,Window.CaretHandle,Window.CaretX,Window.CaretY,Window.CursorForecolor,Window.CursorBackcolor,False,Window.CursorBlink,Window.CursorReverse);
   end;

  {Check Char}
  case AChr of
   #10:begin
     case DefaultTextLineBreakStyle of
      tlbsLF,tlbsCRLF:begin
        {Update X,Y}
        X:=1;
        Inc(Y);
        if Y > Window.Rows then
         begin
          Y:=Window.Rows;

          {Check Scroll}
          if (Window.WindowFlags and WINDOW_FLAG_AUTO_SCROLL) <> 0 then
           begin
            {Console Scroll Up}
            Result:=ConsoleWindowScrollUp(Handle,Y,1);
            if Result <> ERROR_SUCCESS then Exit;
           end;
         end;
       end;
     end;
    end;
   #13:begin
     case DefaultTextLineBreakStyle of
      tlbsCR:begin
        {Update X,Y}
        X:=1;
        Inc(Y);
        if Y > Window.Rows then
         begin
          Y:=Window.Rows;

          {Check Scroll}
          if (Window.WindowFlags and WINDOW_FLAG_AUTO_SCROLL) <> 0 then
           begin
            {Console Scroll Up}
            Result:=ConsoleWindowScrollUp(Handle,Y,1);
            if Result <> ERROR_SUCCESS then Exit;
           end;
         end;
       end;
     end;
    end;
   else
    begin
     {Calculate X,Y}
     WriteX:=Window.X1 + Window.Borderwidth + Window.OffsetX + ((Window.MinX - 1) * Window.FontWidth) + ((X - 1) * Window.FontWidth);
     WriteY:=Window.Y1 + Window.Borderwidth + Window.OffsetY + ((Window.MinY - 1) * Window.FontHeight) + ((Y - 1) * Window.FontHeight);

     {Console Draw Char}
     Result:=ConsoleDeviceDrawChar(Window.Console,Window.Font,AChr,WriteX,WriteY,Forecolor,Backcolor);
     if Result <> ERROR_SUCCESS then Exit;

     {Update X}
     Inc(X);
     if X > Window.Cols then
      begin
       X:=1;
       Inc(Y);
      end;

     {Update Y}
     if Y > Window.Rows then
      begin
       Y:=Window.Rows;

       {Check Scroll}
       if (Window.WindowFlags and WINDOW_FLAG_AUTO_SCROLL) <> 0 then
        begin
         {Console Scroll Up}
         Result:=ConsoleWindowScrollUp(Handle,Y,1);
         if Result <> ERROR_SUCCESS then Exit;
        end;
      end;
    end;
  end;

  {Update Caret}
  if Window.CursorState = CURSOR_STATE_ON then
   begin
    ConsoleDeviceUpdateCaretEx(Window.Console,Window.CaretHandle,Window.CaretX,Window.CaretY,Window.CursorForecolor,Window.CursorBackcolor,True,Window.CursorBlink,Window.CursorReverse);
   end;

  {No cursor Update}

  {Return Result}
  Result:=ERROR_SUCCESS;
 finally
  {Unlock Window}
  MutexUnlock(Window.Lock);
 end;
end;

{==============================================================================}

function ConsoleWindowOutput(Handle:TWindowHandle;const Source,Dest:TConsolePoint;Buffer:PConsoleChar;Width,Height,Skip:LongWord):LongWord;
{Output a rectangular area of text to a console window}
{Handle: The console window to output to}
{Source: The X and Y point in the source buffer to copy text from (Characters)}
{Dest: The X and Y point on the console window to copy text to (Characters)}
{Buffer: A pointer to a buffer of TConsoleChar structures which represent rows of text}
{Width: The width of the area to be output (Characters)}
{Height: The height of the area to be output (Characters)}
{Skip: The number of characters to skip in the buffer after each row (Optional)}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: For Text Console functions, Source, Dest, Width, Height and Skip are based on character rows and columns not screen pixels}
var
 OutputWidth:LongWord;
 OutputHeight:LongWord;
 OutputDest:TConsolePoint;
 Window:PConsoleWindow;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Buffer}
 if Buffer = nil then Exit;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console: Console Window Output (Width=' + IntToStr(Width) + ' Height=' + IntToStr(Height) + ' Skip=' + IntToStr(Skip) + ')');
 {$ENDIF}

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_TEXT then Exit;

 {Check Visible}
 if Window.WindowState <> WINDOW_STATE_VISIBLE then
  begin
   Result:=ERROR_SUCCESS;
   Exit;
  end;

 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;
 try
  {Check Console}
  if Window.Console = nil then Exit;

  {Check Dest}
  if Dest.X < 1 then Exit;
  if Dest.Y < 1 then Exit;
  if ((Window.MinX - 1) + Dest.X) > Window.MaxX then Exit;
  if ((Window.MinY - 1) + Dest.Y) > Window.MaxY then Exit;

  {Update Caret}
  if Window.CursorState = CURSOR_STATE_ON then
   begin
    ConsoleDeviceUpdateCaretEx(Window.Console,Window.CaretHandle,Window.CaretX,Window.CaretY,Window.CursorForecolor,Window.CursorBackcolor,False,Window.CursorBlink,Window.CursorReverse);
   end;

  {Get Width}
  OutputWidth:=Width;
  if ((Window.MinX - 1) + (Dest.X - 1) + OutputWidth) > Window.MaxX then OutputWidth:=Window.MaxX - ((Window.MinX - 1) + (Dest.X - 1));

  {Update Skip}
  Inc(Skip,Width - OutputWidth);

  {Get Height}
  OutputHeight:=Height;
  if ((Window.MinY - 1) + (Dest.Y - 1) + OutputHeight) > Window.MaxY then OutputHeight:=Window.MaxY - ((Window.MinY - 1) + (Dest.Y - 1));

  {Calculate Dest X,Y}
  OutputDest.X:=Window.X1 + Window.Borderwidth + Window.OffsetX + ((Window.MinX - 1) * Window.FontWidth) + ((Dest.X - 1) * Window.FontWidth);
  OutputDest.Y:=Window.Y1 + Window.Borderwidth + Window.OffsetY + ((Window.MinY - 1) * Window.FontHeight) + ((Dest.Y - 1) * Window.FontHeight);

  {Console Put Text}
  Result:=ConsoleDevicePutText(Window.Console,Window.Font,Source,OutputDest,Buffer,OutputWidth,OutputHeight,Skip);
  if Result <> ERROR_SUCCESS then Exit;

  {Update Caret}
  if Window.CursorState = CURSOR_STATE_ON then
   begin
    ConsoleDeviceUpdateCaretEx(Window.Console,Window.CaretHandle,Window.CaretX,Window.CaretY,Window.CursorForecolor,Window.CursorBackcolor,True,Window.CursorBlink,Window.CursorReverse);
   end;

  {No cursor Update}

  {Return Result}
  Result:=ERROR_SUCCESS;
 finally
  {Unlock Window}
  MutexUnlock(Window.Lock);
 end;
end;

{==============================================================================}

function ConsoleWindowRead(Handle:TWindowHandle;var AText:String):LongWord;
{Read text input from the console and echo to an existing console window at the current position in the current color}
{Handle: The handle of the window to echo input to}
{Text: The text read from the console on return}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: The console window will not scroll up on return}
begin
 {}
 Result:=ConsoleWindowReadLnEx(Handle,AText,'',0,0,COLOR_NONE,COLOR_NONE,False,True,nil,nil);
end;

{==============================================================================}

function ConsoleWindowReadLn(Handle:TWindowHandle;var AText:String):LongWord;
{Read text input from the console and echo to an existing console window at the current position in the current color}
{Handle: The handle of the window to echo input to}
{Text: The text read from the console on return}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: The console window will scroll up one line on return}
begin
 {}
 Result:=ConsoleWindowReadLnEx(Handle,AText,'',0,0,COLOR_NONE,COLOR_NONE,True,True,nil,nil);
end;

{==============================================================================}

function ConsoleWindowReadLnEx(Handle:TWindowHandle;var AText:String;const Prompt:String;X,Y,Forecolor,Backcolor:LongWord;Scroll,History:Boolean;Completion:TConsoleWindowCompletion;Data:Pointer):LongWord;
{Read text input from the console and echo to an existing console window at the specified position in the specified color}
{Handle: The handle of the window to echo input to}
{Text: The text read from the console on return}
{Prompt: An optional text prompt to display at the start of the line}
{X: The starting X position for the output (0 for current position)}
{Y: The starting Y position for the output (0 for current position)}
{Forecolor: The text forecolor for the output (COLOR_NONE for current color)}
{Backcolor: The text backcolor for the output (COLOR_NONE for current color)}
{Scroll: If true then scroll up one line on return}
{History: If true then support console history buffer using Up, Down and F3 keys}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: Supports common line editing behaviour including Home, End, Left, Right, Up, Down, Insert, Backspace and Delete}

var
 Ch:Char;
 Buffer:String;
 MaxX:LongWord;
 MaxY:LongWord;
 LastX:LongWord;
 LastY:LongWord;
 FirstX:LongWord;
 FirstY:LongWord;
 CurrentX:LongWord;
 CurrentY:LongWord;
 Mode:TCursorMode;
 Shape:TCursorShape;
 State:TCursorState;
 Window:PConsoleWindow;

 procedure MoveFirst;
 {Move the cursor to the starting position of the line}
 begin
  {}
  {$IFDEF CONSOLE_DEBUG}
  if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console: MoveFirst (FirstX=' + IntToStr(FirstX) + ' FirstY=' + IntToStr(FirstY) + ' LastX=' + IntToStr(LastX) + ' LastY=' + IntToStr(LastY) + ' CurrentX=' + IntToStr(CurrentX) + ' CurrentY=' + IntToStr(CurrentY) + ' MaxX=' + IntToStr(MaxX) + ' MaxY=' + IntToStr(MaxY) + ')');
  {$ENDIF}

  CurrentX:=FirstX;
  CurrentY:=FirstY;
  ConsoleWindowSetXY(Handle,CurrentX,CurrentY);
 end;

 procedure MoveLast;
 {Move the cursor to the ending position of the line}
 begin
  {}
  {$IFDEF CONSOLE_DEBUG}
  if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console: MoveLast (FirstX=' + IntToStr(FirstX) + ' FirstY=' + IntToStr(FirstY) + ' LastX=' + IntToStr(LastX) + ' LastY=' + IntToStr(LastY) + ' CurrentX=' + IntToStr(CurrentX) + ' CurrentY=' + IntToStr(CurrentY) + ' MaxX=' + IntToStr(MaxX) + ' MaxY=' + IntToStr(MaxY) + ')');
  {$ENDIF}

  CurrentX:=LastX;
  CurrentY:=LastY;
  ConsoleWindowSetXY(Handle,CurrentX,CurrentY);
 end;

 procedure MoveLeft;
 {Move the cursor one position to the left in the line}
 var
  First:LongWord;
  Current:LongWord;
 begin
  {}
  {$IFDEF CONSOLE_DEBUG}
  if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console: MoveLeft (FirstX=' + IntToStr(FirstX) + ' FirstY=' + IntToStr(FirstY) + ' LastX=' + IntToStr(LastX) + ' LastY=' + IntToStr(LastY) + ' CurrentX=' + IntToStr(CurrentX) + ' CurrentY=' + IntToStr(CurrentY) + ' MaxX=' + IntToStr(MaxX) + ' MaxY=' + IntToStr(MaxY) + ')');
  {$ENDIF}

  {Get Offsets}
  First:=(FirstY * MaxX) + FirstX;
  Current:=(CurrentY * MaxX) + CurrentX;

  {Check Offset}
  if Current = First then Exit;

  {Update Position}
  Dec(CurrentX);
  if (CurrentY > FirstY) and (CurrentX < 1) then
   begin
    CurrentX:=MaxX;
    Dec(CurrentY);
   end;
  ConsoleWindowSetXY(Handle,CurrentX,CurrentY);
 end;

 procedure MoveRight;
 {Move the cursor one position to the right in the line}
 var
  Last:LongWord;
  Current:LongWord;
 begin
  {}
  {$IFDEF CONSOLE_DEBUG}
  if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console: MoveRight (FirstX=' + IntToStr(FirstX) + ' FirstY=' + IntToStr(FirstY) + ' LastX=' + IntToStr(LastX) + ' LastY=' + IntToStr(LastY) + ' CurrentX=' + IntToStr(CurrentX) + ' CurrentY=' + IntToStr(CurrentY) + ' MaxX=' + IntToStr(MaxX) + ' MaxY=' + IntToStr(MaxY) + ')');
  {$ENDIF}

  {Get Offsets}
  Last:=(LastY * MaxX) + LastX;
  Current:=(CurrentY * MaxX) + CurrentX;

  {Check Offset}
  if Current = Last then Exit;

  {Update Position}
  Inc(CurrentX);
  if (CurrentY < LastY) and (CurrentX > MaxX) then
   begin
    CurrentX:=1;
    Inc(CurrentY);
   end;
  ConsoleWindowSetXY(Handle,CurrentX,CurrentY);
 end;

 procedure EraseLine;
 {Erase all characters in the line and reset the cursor position}
 begin
  {}
  {$IFDEF CONSOLE_DEBUG}
  if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console: EraseLine (FirstX=' + IntToStr(FirstX) + ' FirstY=' + IntToStr(FirstY) + ' LastX=' + IntToStr(LastX) + ' LastY=' + IntToStr(LastY) + ' CurrentX=' + IntToStr(CurrentX) + ' CurrentY=' + IntToStr(CurrentY) + ' MaxX=' + IntToStr(MaxX) + ' MaxY=' + IntToStr(MaxY) + ')');
  {$ENDIF}

  {Update Position}
  CurrentX:=FirstX;
  CurrentY:=FirstY;
  LastX:=FirstX;
  LastY:=FirstY;
  ConsoleWindowSetXY(Handle,CurrentX,CurrentY);

  {Update Line}
  ConsoleWindowWrite(Handle,StringOfChar(' ',Length(Buffer)));
  ConsoleWindowSetXY(Handle,CurrentX,CurrentY);

  {Update Buffer}
  Buffer:='';
 end;

 procedure OutputLine(const AValue:String);
 {Output all characters in value and update the cursor position}
 var
  Count:Integer;
  Value:String;
 begin
  {}
  {$IFDEF CONSOLE_DEBUG}
  if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console: OutputLine (FirstX=' + IntToStr(FirstX) + ' FirstY=' + IntToStr(FirstY) + ' LastX=' + IntToStr(LastX) + ' LastY=' + IntToStr(LastY) + ' CurrentX=' + IntToStr(CurrentX) + ' CurrentY=' + IntToStr(CurrentY) + ' MaxX=' + IntToStr(MaxX) + ' MaxY=' + IntToStr(MaxY) + ')');
  {$ENDIF}

  {Check Value}
  if Length(AValue) = 0 then Exit;

  {Move Last}
  MoveLast;

  {Output Line}
  Value:='';
  for Count:=1 to Length(AValue) do
   begin
    {Update Position}
    Inc(LastX);
    if LastX > MaxX then
     begin
      if LastY < MaxY then
       begin
        LastX:=1;
        Inc(LastY);
       end
      else
       begin
        if FirstY > 1 then
         begin
          LastX:=1;
          Dec(FirstY);
         end
        else
         begin
          Dec(LastX);
          Break;
         end;
       end;
     end;

    {Get Character}
    Value:=Value + AValue[Count];
   end;

  {Update Position}
  CurrentX:=LastX;
  CurrentY:=LastY;

  {Update Line}
  ConsoleWindowWrite(Handle,Value);

  {Update Buffer}
  Buffer:=Value;
 end;

 function ExpandLine:Boolean;
 {Expand the tab key to a command completion if available}
 var
  Value:String;
 begin
  {}
  Result:=False;

  {$IFDEF CONSOLE_DEBUG}
  if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console: ExpandLine (FirstX=' + IntToStr(FirstX) + ' FirstY=' + IntToStr(FirstY) + ' LastX=' + IntToStr(LastX) + ' LastY=' + IntToStr(LastY) + ' CurrentX=' + IntToStr(CurrentX) + ' CurrentY=' + IntToStr(CurrentY) + ' MaxX=' + IntToStr(MaxX) + ' MaxY=' + IntToStr(MaxY) + ')');
  {$ENDIF}

  {Check Completion}
  if Assigned(Completion) then
   begin
    {Get Buffer}
    Value:=Buffer;

    {Call Completion}
    Result:=(Completion(Handle,Value,Data) = ERROR_SUCCESS);
    if not Result then Exit;

    {Erase Current}
    EraseLine;

    {Update Line}
    ConsoleWindowWrite(Handle,Value);
    ConsoleWindowGetXY(Handle,CurrentX,CurrentY);
    LastX:=CurrentX;
    LastY:=CurrentY;

    {Update Buffer}
    Buffer:=Value;
   end
 end;

 procedure EraseCharacter;
 {Erase the character to the left of the cursor position}
 var
  Head:String;
  Tail:String;
  Last:LongWord;
  First:LongWord;
  Current:LongWord;
 begin
  {}
  {$IFDEF CONSOLE_DEBUG}
  if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console: EraseCharacter (FirstX=' + IntToStr(FirstX) + ' FirstY=' + IntToStr(FirstY) + ' LastX=' + IntToStr(LastX) + ' LastY=' + IntToStr(LastY) + ' CurrentX=' + IntToStr(CurrentX) + ' CurrentY=' + IntToStr(CurrentY) + ' MaxX=' + IntToStr(MaxX) + ' MaxY=' + IntToStr(MaxY) + ')');
  {$ENDIF}

  {Get Offsets}
  First:=(FirstY * MaxX) + FirstX;
  Last:=(LastY * MaxX) + LastX;
  Current:=(CurrentY * MaxX) + CurrentX;

  {Check Offset}
  if Current = First then Exit;

  {Get Head and Tail}
  Head:=Copy(Buffer,1,(Current - First) - 1);
  Tail:=Copy(Buffer,(Current - First) + 1,Length(Buffer));

  {Update Position}
  Dec(CurrentX);
  if (CurrentY > FirstY) and (CurrentX < 1) then
   begin
    CurrentX:=MaxX;
    Dec(CurrentY);
   end;

  Dec(LastX);
  if (LastY > FirstY) and (LastX < 1) then
   begin
    LastX:=MaxX;
    Dec(LastY);
   end;
  ConsoleWindowSetXY(Handle,CurrentX,CurrentY);

  {Update Line}
  ConsoleWindowWrite(Handle,Tail + ' ');
  ConsoleWindowSetXY(Handle,CurrentX,CurrentY);

  {Update Buffer}
  Buffer:=Head + Tail;
 end;

 procedure DeleteCharacter;
 {Delete the character to the right of the cursor position}
 var
  Head:String;
  Tail:String;
  Last:LongWord;
  First:LongWord;
  Current:LongWord;
 begin
  {}
  {$IFDEF CONSOLE_DEBUG}
  if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console: DeleteCharacter (FirstX=' + IntToStr(FirstX) + ' FirstY=' + IntToStr(FirstY) + ' LastX=' + IntToStr(LastX) + ' LastY=' + IntToStr(LastY) + ' CurrentX=' + IntToStr(CurrentX) + ' CurrentY=' + IntToStr(CurrentY) + ' MaxX=' + IntToStr(MaxX) + ' MaxY=' + IntToStr(MaxY) + ')');
  {$ENDIF}

  {Get Offsets}
  First:=(FirstY * MaxX) + FirstX;
  Last:=(LastY * MaxX) + LastX;
  Current:=(CurrentY * MaxX) + CurrentX;

  {Check Offset}
  if Current = Last then Exit;

  {Get Head and Tail}
  Head:=Copy(Buffer,1,Current - First);
  Tail:=Copy(Buffer,(Current - First) + 2,Length(Buffer));

  {Update Position}
  Dec(LastX);
  if (LastY > FirstY) and (LastX < 1) then
   begin
    LastX:=MaxX;
    Dec(LastY);
   end;

  {Update Line}
  ConsoleWindowWrite(Handle,Tail + ' ');
  ConsoleWindowSetXY(Handle,CurrentX,CurrentY);

  {Update Buffer}
  Buffer:=Head + Tail;
 end;

 procedure InsertCharacter;
 {Insert a character at the cursor position}
 var
  Head:String;
  Tail:String;
  Last:LongWord;
  First:LongWord;
  Current:LongWord;
  Scroll:Boolean;
 begin
  {}
  {$IFDEF CONSOLE_DEBUG}
  if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console: InsertCharacter (FirstX=' + IntToStr(FirstX) + ' FirstY=' + IntToStr(FirstY) + ' LastX=' + IntToStr(LastX) + ' LastY=' + IntToStr(LastY) + ' CurrentX=' + IntToStr(CurrentX) + ' CurrentY=' + IntToStr(CurrentY) + ' MaxX=' + IntToStr(MaxX) + ' MaxY=' + IntToStr(MaxY) + ')');
  {$ENDIF}

  {Get Offsets}
  First:=(FirstY * MaxX) + FirstX;
  Last:=(LastY * MaxX) + LastX;
  Current:=(CurrentY * MaxX) + CurrentX;

  {Get Head and Tail}
  Head:=Copy(Buffer,1,Current - First);
  Tail:=Copy(Buffer,(Current - First) + 1,Length(Buffer));

  Scroll:=False;

  {Update Position}
  Inc(LastX);
  if LastX > MaxX then
   begin
    if LastY < MaxY then
     begin
      LastX:=1;
      Inc(LastY);
     end
    else
     begin
      if FirstY > 1 then
       begin
        LastX:=1;
        Dec(FirstY);
        Dec(CurrentY);
        Scroll:=True;
       end
      else
       begin
        Dec(LastX);
        Exit;
       end;
     end;
   end;

  Inc(CurrentX);
  if CurrentX > MaxX then
   begin
    if CurrentY < MaxY then
     begin
      CurrentX:=1;
      Inc(CurrentY);
     end
    else
     begin
      if Scroll then
       begin
        CurrentX:=1;
       end
      else
       begin
        Dec(CurrentX);
        Exit;
       end;
     end;
   end;

  {Update Line}
  ConsoleWindowWrite(Handle,Ch + Tail);
  ConsoleWindowSetXY(Handle,CurrentX,CurrentY);

  {Update Buffer}
  Buffer:=Head + Ch + Tail;
 end;

 procedure OverwriteCharacter;
 {Overwrite the character at the cursor position}
 var
  Head:String;
  Tail:String;
  Last:LongWord;
  First:LongWord;
  Current:LongWord;
  Scroll:Boolean;
 begin
  {}
  {$IFDEF CONSOLE_DEBUG}
  if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console: OverwriteCharacter (FirstX=' + IntToStr(FirstX) + ' FirstY=' + IntToStr(FirstY) + ' LastX=' + IntToStr(LastX) + ' LastY=' + IntToStr(LastY) + ' CurrentX=' + IntToStr(CurrentX) + ' CurrentY=' + IntToStr(CurrentY) + ' MaxX=' + IntToStr(MaxX) + ' MaxY=' + IntToStr(MaxY) + ')');
  {$ENDIF}

  {Get Offsets}
  First:=(FirstY * MaxX) + FirstX;
  Last:=(LastY * MaxX) + LastX;
  Current:=(CurrentY * MaxX) + CurrentX;

  {Get Head and Tail}
  Head:=Copy(Buffer,1,Current - First);
  Tail:=Copy(Buffer,(Current - First) + 2,Length(Buffer));

  Scroll:=False;

  {Update Position}
  if (CurrentX = LastX) and (CurrentY = LastY) then
   begin
    Inc(LastX);
   end;

  Inc(CurrentX);
  if CurrentX > MaxX then
   begin
    if (CurrentX = LastX) and (CurrentY = LastY) then
     begin
      if LastY < MaxY then
       begin
        LastX:=1;
        Inc(LastY);
       end
      else
       begin
        if FirstY > 1 then
         begin
          LastX:=1;
          Dec(FirstY);
          Dec(CurrentY);
          Scroll:=True;
         end
        else
         begin
          Dec(LastX);
         end;
       end;
     end;

    if CurrentY < MaxY then
     begin
      CurrentX:=1;
      Inc(CurrentY);
     end
    else
     begin
      if Scroll then
       begin
        CurrentX:=1;
       end
      else
       begin
        Dec(CurrentX);
        Exit;
       end;
     end;
   end;

  {Update Line}
  ConsoleWindowWrite(Handle,Ch + Tail);
  ConsoleWindowSetXY(Handle,CurrentX,CurrentY);

  {Update Buffer}
  Buffer:=Head + Ch + Tail;
 end;

 procedure PrevHistory;
 {Get the previous command history value}
 var
  Value:String;
 begin
  {}
  {$IFDEF CONSOLE_DEBUG}
  if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console: PrevHistory (FirstX=' + IntToStr(FirstX) + ' FirstY=' + IntToStr(FirstY) + ' LastX=' + IntToStr(LastX) + ' LastY=' + IntToStr(LastY) + ' CurrentX=' + IntToStr(CurrentX) + ' CurrentY=' + IntToStr(CurrentY) + ' MaxX=' + IntToStr(MaxX) + ' MaxY=' + IntToStr(MaxY) + ')');
  {$ENDIF}

  {Get History}
  Value:=ConsoleWindowPreviousHistory(Handle);
  if Length(Value) = 0 then Exit;

  {Erase Current}
  EraseLine;

  {Output Line}
  OutputLine(Value);
 end;

 procedure NextHistory;
 {Get the next command history value}
 var
  Value:String;
 begin
  {}
  {$IFDEF CONSOLE_DEBUG}
  if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console: NextHistory (FirstX=' + IntToStr(FirstX) + ' FirstY=' + IntToStr(FirstY) + ' LastX=' + IntToStr(LastX) + ' LastY=' + IntToStr(LastY) + ' CurrentX=' + IntToStr(CurrentX) + ' CurrentY=' + IntToStr(CurrentY) + ' MaxX=' + IntToStr(MaxX) + ' MaxY=' + IntToStr(MaxY) + ')');
  {$ENDIF}

  {Get History}
  Value:=ConsoleWindowNextHistory(Handle);
  if Length(Value) = 0 then Exit;

  {Erase Current}
  EraseLine;

  {Output Line}
  OutputLine(Value);
 end;

 procedure FirstHistory;
 {Get the first command history value}
 var
  Value:String;
 begin
  {}
  {$IFDEF CONSOLE_DEBUG}
  if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console: FirstHistory (FirstX=' + IntToStr(FirstX) + ' FirstY=' + IntToStr(FirstY) + ' LastX=' + IntToStr(LastX) + ' LastY=' + IntToStr(LastY) + ' CurrentX=' + IntToStr(CurrentX) + ' CurrentY=' + IntToStr(CurrentY) + ' MaxX=' + IntToStr(MaxX) + ' MaxY=' + IntToStr(MaxY) + ')');
  {$ENDIF}

  {Get History}
  Value:=ConsoleWindowFirstHistory(Handle);
  if Length(Value) = 0 then Exit;

  {Erase Current}
  EraseLine;

  {Output Line}
  OutputLine(Value);
 end;

 procedure LastHistory;
 {Get the last command history value}
 var
  Value:String;
 begin
  {}
  {$IFDEF CONSOLE_DEBUG}
  if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console: LastHistory (FirstX=' + IntToStr(FirstX) + ' FirstY=' + IntToStr(FirstY) + ' LastX=' + IntToStr(LastX) + ' LastY=' + IntToStr(LastY) + ' CurrentX=' + IntToStr(CurrentX) + ' CurrentY=' + IntToStr(CurrentY) + ' MaxX=' + IntToStr(MaxX) + ' MaxY=' + IntToStr(MaxY) + ')');
  {$ENDIF}

  {Get History}
  Value:=ConsoleWindowLastHistory(Handle);
  if Length(Value) = 0 then Exit;

  {Erase Current}
  EraseLine;

  {Output Line}
  OutputLine(Value);
 end;

 procedure CurrentHistory;
 {Get the current command history value}
 var
  Value:String;
 begin
  {}
  {$IFDEF CONSOLE_DEBUG}
  if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console: CurrentHistory (FirstX=' + IntToStr(FirstX) + ' FirstY=' + IntToStr(FirstY) + ' LastX=' + IntToStr(LastX) + ' LastY=' + IntToStr(LastY) + ' CurrentX=' + IntToStr(CurrentX) + ' CurrentY=' + IntToStr(CurrentY) + ' MaxX=' + IntToStr(MaxX) + ' MaxY=' + IntToStr(MaxY) + ')');
  {$ENDIF}

  {Get History}
  Value:=ConsoleWindowCurrentHistory(Handle);
  if Length(Value) = 0 then Exit;

  {Erase Current}
  EraseLine;

  {Output Line}
  OutputLine(Value);
 end;

begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Get Defaults}
 AText:='';

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console: Console Window ReadLn Ex (Handle=' + IntToHex(Handle,8) + ' Prompt=' + Prompt + ' X=' + IntToStr(X) + ' Y=' + IntToStr(Y) + ')');
 {$ENDIF}

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_TEXT then Exit;

 Result:=ERROR_NOT_READY;

 {Check Visible}
 if Window.WindowState <> WINDOW_STATE_VISIBLE then Exit;

 {Check Active}
 if ConsoleWindowCheckFlag(Handle,WINDOW_FLAG_FOCUS_CURSOR) and (ConsoleWindowGetActive(Window.Console) <> Handle) then Exit;

 {Check X, Y}
 if (X > 0) or (Y > 0) then
  begin
   {Get X, Y}
   if X = 0 then X:=ConsoleWindowGetX(Handle);
   if Y = 0 then Y:=ConsoleWindowGetY(Handle);

   {Set X, Y}
   ConsoleWindowSetXY(Handle,X,Y);
  end;

 {Check Colors}
 if (Forecolor <> COLOR_NONE) or (Backcolor <> COLOR_NONE) then
  begin
   {Get Colors}
   if Forecolor = COLOR_NONE then Forecolor:=ConsoleWindowGetForecolor(Handle);
   if Backcolor = COLOR_NONE then Backcolor:=ConsoleWindowGetBackcolor(Handle);

   {Set Colors}
   ConsoleWindowSetForecolor(Handle,Forecolor);
   ConsoleWindowSetBackcolor(Handle,Backcolor);
  end;

 {Check Prompt}
 if Length(Prompt) > 0 then
  begin
   ConsoleWindowWrite(Handle,Prompt);
  end;

 {Setup Start}
 Buffer:='';
 MaxX:=ConsoleWindowGetCols(Handle);
 MaxY:=ConsoleWindowGetRows(Handle);
 CurrentX:=ConsoleWindowGetX(Handle);
 CurrentY:=ConsoleWindowGetY(Handle);
 FirstX:=CurrentX;
 FirstY:=CurrentY;
 LastX:=CurrentX;
 LastY:=CurrentY;
 Mode:=CURSOR_MODE_INSERT;
 Shape:=CURSOR_SHAPE_LINE;
 State:=ConsoleWindowGetCursorState(Handle);

 {Setup Cursor}
 ConsoleWindowSetCursorMode(Handle,Mode);
 ConsoleWindowSetCursorShape(Handle,Shape);
 ConsoleWindowSetCursorState(Handle,CURSOR_STATE_ON);

 {Read Keys}
 while True do
  begin
   Ch:=ConsoleReadKey;
   case Ch of
    #0:begin  {Extended Scan Code}
      Ch:=ConsoleReadKey;
      case Ch of
       #75:begin {Left Arrow}
         MoveLeft;
        end;
       #77:begin {Right Arrow}
         MoveRight;
        end;
       #72:begin {Up Arrow}
         if History then PrevHistory;
        end;
       #80:begin {Down Arrow}
         if History then NextHistory;
        end;
       #82:begin {Insert}
         {Check Mode}
         if Mode = CURSOR_MODE_INSERT then
          begin
           Mode:=CURSOR_MODE_OVERWRITE;
           Shape:=CURSOR_SHAPE_BAR;
          end
         else
          begin
           Mode:=CURSOR_MODE_INSERT;
           Shape:=CURSOR_SHAPE_LINE;
          end;

         {Set Mode}
         ConsoleWindowSetCursorMode(Handle,Mode);
         ConsoleWindowSetCursorShape(Handle,Shape);
        end;
       #83:begin {Delete}
         DeleteCharacter;
        end;
       #71:begin {Home}
         MoveFirst;
        end;
       #79:begin {End}
         MoveLast;
        end;
       #73:begin {Page Up}
         if History then FirstHistory;
        end;
       #81:begin {Page Down}
         if History then LastHistory;
        end;
       #59:begin {F1}
         {Nothing}
        end;
       #60:begin {F2}
         {Nothing}
        end;
       #61:begin {F3}
         if History then CurrentHistory;
        end;
       #62:begin {F4}
         {Nothing}
        end;
       #63:begin {F5}
         {Nothing}
        end;
       #64:begin {F6}
         {Nothing}
        end;
       #65:begin {F7}
         {Nothing}
        end;
       #66:begin {F8}
         {Nothing}
        end;
       #67:begin {F9}
         {Nothing}
        end;
       #68:begin {F10}
         {Nothing}
        end;
      end;
     end;
    #8:begin  {Backspace}
      EraseCharacter;
     end;
    #9:begin  {Tab}
      if not ExpandLine then
       begin
        if Mode = CURSOR_MODE_INSERT then
         begin
          InsertCharacter;
         end
        else
         begin
          OverwriteCharacter;
         end;
       end;
     end;
    #13:begin {Return}
      if Scroll then
       begin
        MoveLast;
        ConsoleWindowWriteLn(Handle,'');
       end;
      if History then ConsoleWindowAddHistory(Handle,Buffer);
      AText:=Buffer;
      Break;
     end;
    #27:begin {Escape}
      EraseLine;
      if Scroll then ConsoleWindowWriteLn(Handle,'');
      AText:=Buffer;
      Break;
     end;
    else
     begin    {Any Other Key}
      if Mode = CURSOR_MODE_INSERT then
       begin
        InsertCharacter;
       end
      else
       begin
        OverwriteCharacter;
       end;
     end;
   end;
  end;

 {Reset Cursor}
 ConsoleWindowSetCursorState(Handle,State);

 {Return Result}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function ConsoleWindowReadChr(Handle:TWindowHandle;var AChr:Char):LongWord;
{Read one character input from the console and echo to an existing console window at the current position in the current color}
{Handle: The handle of the window to echo input to}
{Chr: The character read from the console on return}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: The console window will not scroll up on return}
begin
 {}
 Result:=ConsoleWindowReadChrEx(Handle,AChr,'',0,0,COLOR_NONE,COLOR_NONE,True,False);
end;

{==============================================================================}

function ConsoleWindowReadChrEx(Handle:TWindowHandle;var AChr:Char;const Prompt:String;X,Y,Forecolor,Backcolor:LongWord;Echo,Scroll:Boolean):LongWord;
{Read one character input from the console and optionally echo to an existing console window at the specified position in the specified color}
{Handle: The handle of the window to echo input to}
{Chr: The character read from the console on return}
{Prompt: An optional text prompt to display at the start of the line}
{X: The starting X position for the output (0 for current position)}
{Y: The starting Y position for the output (0 for current position)}
{Forecolor: The text forecolor for the output (COLOR_NONE for current color)}
{Backcolor: The text backcolor for the output (COLOR_NONE for current color)}
{Echo: If true then echo the character to the console window}
{Scroll: If true then scroll up one line on return}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Ch:Char;
 Mode:TCursorMode;
 Shape:TCursorShape;
 State:TCursorState;
 Window:PConsoleWindow;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {Get Defaults}
 AChr:=#0;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console: Console Window ReadChr Ex (Handle=' + IntToHex(Handle,8) + ' Prompt=' + Prompt + ' X=' + IntToStr(X) + ' Y=' + IntToStr(Y) + ')');
 {$ENDIF}

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_TEXT then Exit;

 {Check Visible}
 if Window.WindowState <> WINDOW_STATE_VISIBLE then Exit;

 {Check Active}
 if ConsoleWindowCheckFlag(Handle,WINDOW_FLAG_FOCUS_CURSOR) and (ConsoleWindowGetActive(Window.Console) <> Handle) then Exit;

 {Check X, Y}
 if (X > 0) or (Y > 0) then
  begin
   {Get X, Y}
   if X = 0 then X:=ConsoleWindowGetX(Handle);
   if Y = 0 then Y:=ConsoleWindowGetY(Handle);

   {Set X, Y}
   ConsoleWindowSetXY(Handle,X,Y);
  end;

 {Check Colors}
 if (Forecolor = COLOR_NONE) or (Backcolor = COLOR_NONE) then
  begin
   {Get Colors}
   if Forecolor = COLOR_NONE then Forecolor:=ConsoleWindowGetForecolor(Handle);
   if Backcolor = COLOR_NONE then Backcolor:=ConsoleWindowGetBackcolor(Handle);

   {Set Colors}
   ConsoleWindowSetForecolor(Handle,Forecolor);
   ConsoleWindowSetBackcolor(Handle,Backcolor);
  end;

 {Check Prompt}
 if Length(Prompt) > 0 then
  begin
   ConsoleWindowWrite(Handle,Prompt);
  end;

 {Setup Start}
 Mode:=CURSOR_MODE_INSERT;
 Shape:=CURSOR_SHAPE_LINE;
 State:=ConsoleWindowGetCursorState(Handle);

 {Setup Cursor}
 ConsoleWindowSetCursorMode(Handle,Mode);
 ConsoleWindowSetCursorShape(Handle,Shape);
 ConsoleWindowSetCursorState(Handle,CURSOR_STATE_ON);

 {Read Key}
 while True do
  begin
   Ch:=ConsoleReadKey;
   case Ch of
    #0:begin  {Extended Scan Code}
      Ch:=ConsoleReadKey;
     end;
    #13:begin {Return}
      if Scroll then ConsoleWindowWriteLn(Handle,'');
      Break;
     end;
    #27:begin {Escape}
      if Scroll then ConsoleWindowWriteLn(Handle,'');
      Break;
     end;
    else
     begin    {Any Other Key}
      {Others}
      if Echo then ConsoleWindowWriteChr(Handle,Ch);
      if Scroll then ConsoleWindowWriteLn(Handle,'');
      AChr:=Ch;
      Break;
     end;
   end;
  end;

 {Reset Cursor}
 ConsoleWindowSetCursorState(Handle,State);

 {Return Result}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}
{==============================================================================}
{CRT Console Functions}
procedure ConsoleAssignCrt(var F:Text);
{Compatible with RTL Crt unit function AssignCrt}
{See: http://www.freepascal.org/docs-html-3.0.0/rtl/crt/assigncrt.html}
begin
 {}
 TextIOOpen(F,ConsoleWriteChar,ConsoleReadChar,fmOutput,nil);
end;

{==============================================================================}

procedure ConsoleClrEol;
{Compatible with RTL Crt unit function ClrEol}
{See: http://www.freepascal.org/docs-html-3.0.0/rtl/crt/clreol.html}
var
 Handle:TWindowHandle;
begin
 {}
 {Get Window}
 Handle:=ConsoleWindowGetDefault(ConsoleDeviceDefault);
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {Clear Line}
 ConsoleWindowClearEx(Handle,ConsoleWindowGetX(Handle),ConsoleWindowGetY(Handle),ConsoleWindowGetMaxX(Handle),ConsoleWindowGetY(Handle),False);
end;

{==============================================================================}

procedure ConsoleClrScr;
{Compatible with RTL Crt unit function ClrScr}
{See: http://www.freepascal.org/docs-html-3.0.0/rtl/crt/clrscr.html}
var
 Handle:TWindowHandle;
begin
 {}
 {Get Window}
 Handle:=ConsoleWindowGetDefault(ConsoleDeviceDefault);
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {Clear Window}
 ConsoleWindowClear(Handle);
end;

{==============================================================================}

procedure ConsoleDelay(MS:Word);
{Compatible with RTL Crt unit function Delay}
{See: http://www.freepascal.org/docs-html-3.0.0/rtl/crt/delay.html}
begin
 {}
 ThreadSleep(MS);
end;

{==============================================================================}

procedure ConsoleDelLine;
{Compatible with RTL Crt unit function DelLine}
{See: http://www.freepascal.org/docs-html-3.0.0/rtl/crt/delline.html}
var
 Handle:TWindowHandle;
begin
 {}
 {Get Window}
 Handle:=ConsoleWindowGetDefault(ConsoleDeviceDefault);
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {Scroll Up}
 ConsoleWindowScrollUp(Handle,ConsoleWindowGetY(Handle),1);
end;

{==============================================================================}

procedure ConsoleGotoXY(X,Y:Integer);
{Compatible with RTL Crt unit function GotoXY}
{See: http://www.freepascal.org/docs-html-3.0.0/rtl/crt/gotoxy.html}

{Note: For CRT Console functions, X and Y are based on character rows and columns not screen pixels}
var
 Handle:TWindowHandle;
begin
 {}
 {Get Window}
 Handle:=ConsoleWindowGetDefault(ConsoleDeviceDefault);
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {Set XY}
 ConsoleWindowSetXY(Handle,X,Y);
end;

{==============================================================================}

procedure ConsoleHighVideo;
{Compatible with RTL Crt unit function HighVideo}
{See: http://www.freepascal.org/docs-html-3.0.0/rtl/crt/highvideo.html}
begin
 {}
 {No function under Ultibo}
end;

{==============================================================================}

procedure ConsoleInsLine;
{Compatible with RTL Crt unit function InsLine}
{See: http://www.freepascal.org/docs-html-3.0.0/rtl/crt/insline.html}
var
 Handle:TWindowHandle;
begin
 {}
 {Get Window}
 Handle:=ConsoleWindowGetDefault(ConsoleDeviceDefault);
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {Scroll Down}
 ConsoleWindowScrollDown(Handle,ConsoleWindowGetY(Handle),1);
end;

{==============================================================================}

function ConsoleKeypressed:Boolean;
{Compatible with RTL Crt unit function KeyPressed}
{See: http://www.freepascal.org/docs-html-3.0.0/rtl/crt/keypressed.html}
var
 Ch:Char;
begin
 {}
 Result:=ConsolePeekKey(Ch,nil);
end;

{==============================================================================}

procedure ConsoleLowVideo;
{Compatible with RTL Crt unit function LowVideo}
{See: http://www.freepascal.org/docs-html-3.0.0/rtl/crt/lowvideo.html}
begin
 {}
 {No function under Ultibo}
end;

{==============================================================================}

procedure ConsoleNormVideo;
{Compatible with RTL Crt unit function NormVideo}
{See: http://www.freepascal.org/docs-html-3.0.0/rtl/crt/normvideo.html}
begin
 {}
 {No function under Ultibo}
end;

{==============================================================================}

procedure ConsoleNoSound;
{Compatible with RTL Crt unit function NoSound}
{See: http://www.freepascal.org/docs-html-3.0.0/rtl/crt/nosound.html}
begin
 {}
 {No function under Ultibo}
end;

{==============================================================================}

function ConsoleReadKey:Char;
{Compatible with RTL Crt unit function ReadKey}
{See: http://www.freepascal.org/docs-html-3.0.0/rtl/crt/readkey.html}
{Note: For extended key scan codes see: http://www.freepascal.org/docs-html/rtl/keyboard/kbdscancode.html}
begin
 {}
 ConsoleGetKey(Result,nil);
end;

{==============================================================================}

procedure ConsoleSound(Hz:Word);
{Compatible with RTL Crt unit function Sound}
{See: http://www.freepascal.org/docs-html-3.0.0/rtl/crt/sound.html}
begin
 {}
 {No function under Ultibo}
end;

{==============================================================================}

procedure ConsoleTextBackground(Color:LongWord);
{Compatible with RTL Crt unit function TextBackground}
{See: http://www.freepascal.org/docs-html-3.0.0/rtl/crt/textbackground.html}
var
 Handle:TWindowHandle;
begin
 {}
 {Get Window}
 Handle:=ConsoleWindowGetDefault(ConsoleDeviceDefault);
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {Set Backcolor}
 ConsoleWindowSetBackcolor(Handle,Color);
end;

{==============================================================================}

procedure ConsoleTextColor(Color:LongWord);
{Compatible with RTL Crt unit function TextColor}
{See: http://www.freepascal.org/docs-html-3.0.0/rtl/crt/textcolor.html}
var
 Handle:TWindowHandle;
begin
 {}
 {Get Window}
 Handle:=ConsoleWindowGetDefault(ConsoleDeviceDefault);
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {Set Forecolor}
 ConsoleWindowSetForecolor(Handle,Color);
end;

{==============================================================================}

procedure ConsoleTextMode(Mode:Integer);
{Compatible with RTL Crt unit function TextMode}
{See: http://www.freepascal.org/docs-html-3.0.0/rtl/crt/textmode.html}
begin
 {}
 {No function under Ultibo}
end;

{==============================================================================}

function ConsoleWhereX:Integer;
{Compatible with RTL Crt unit function WhereX}
{See: http://www.freepascal.org/docs-html-3.0.0/rtl/crt/wherex.html}

{Note: For CRT Console functions, X is based on character columns not screen pixels}
var
 Handle:TWindowHandle;
begin
 {}
 Result:=0;

 {Get Window}
 Handle:=ConsoleWindowGetDefault(ConsoleDeviceDefault);
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {Get X}
 Result:=ConsoleWindowGetX(Handle);
end;

{==============================================================================}

function ConsoleWhereY:Integer;
{Compatible with RTL Crt unit function WhereY}
{See: http://www.freepascal.org/docs-html-3.0.0/rtl/crt/wherey.html}

{Note: For CRT Console functions, Y is based on character row not screen pixels}
var
 Handle:TWindowHandle;
begin
 {}
 Result:=0;

 {Get Window}
 Handle:=ConsoleWindowGetDefault(ConsoleDeviceDefault);
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {Get Y}
 Result:=ConsoleWindowGetY(Handle);
end;

{==============================================================================}

procedure ConsoleWindow(X1,Y1,X2,Y2:Integer);
{Compatible with RTL Crt unit function Window}
{See: http://www.freepascal.org/docs-html-3.0.0/rtl/crt/window.html}

{Note: For CRT Console functions, X1, Y1, X2 and Y2 are based on character rows and columns not screen pixels}
var
 Handle:TWindowHandle;
begin
 {}
 {Get Window}
 Handle:=ConsoleWindowGetDefault(ConsoleDeviceDefault);
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {Set Viewport}
 ConsoleWindowSetViewport(Handle,X1,Y1,X2,Y2);
end;

{==============================================================================}

procedure ConsoleScrollUp(Row,Lines:Integer);
{Scroll the default console window up}
{Row: The starting row (Y) for the scroll up, all rows from top plus Lines down to Row will be scrolled up}
{Lines: The number of character lines to scroll up, Lines number of rows at the top will be discarded}

{Note: For CRT Console functions, Row and Lines are based on character rows and columns not screen pixels}
var
 Handle:TWindowHandle;
begin
 {}
 {Get Window}
 Handle:=ConsoleWindowGetDefault(ConsoleDeviceDefault);
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {Scroll Up}
 ConsoleWindowScrollUp(Handle,Row,Lines);
end;

{==============================================================================}

procedure ConsoleScrollDown(Row,Lines:Integer);
{Scroll the default console window down}
{Row: The starting row (Y) for the scroll down, all rows from bottom minus Lines up to Row will be scrolled down}
{Lines: The number of character lines to scroll down, Lines number of rows at the bottom will be discarded}

{Note: For CRT Console functions, Row and Lines are based on character rows and columns not screen pixels}
var
 Handle:TWindowHandle;
begin
 {}
 {Get Window}
 Handle:=ConsoleWindowGetDefault(ConsoleDeviceDefault);
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {Scroll Down}
 ConsoleWindowScrollDown(Handle,Row,Lines);
end;

{==============================================================================}

procedure ConsoleWrite(const AText:String);
{Write text on the default console window at the current position in the current color}
{Text: The text to write}

{Note: The window will not scroll up at the end of the line}
var
 Handle:TWindowHandle;
begin
 {}
 {Get Window}
 Handle:=ConsoleWindowGetDefault(ConsoleDeviceDefault);
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {Write}
 ConsoleWindowWrite(Handle,AText);
end;

{==============================================================================}

procedure ConsoleWriteLn(const AText:String);
{Write text on the default console window at the current position in the current color}
{Text: The text to write}

{Note: The window will scroll up at the end of the line}
var
 Handle:TWindowHandle;
begin
 {}
 {Get Window}
 Handle:=ConsoleWindowGetDefault(ConsoleDeviceDefault);
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {WriteLn}
 ConsoleWindowWriteLn(Handle,AText);
end;

{==============================================================================}

procedure ConsoleWriteChr(AChr:Char);
{Write a character on the default console window at the current position in the current color}
{Chr: The character to write}
var
 Handle:TWindowHandle;
begin
 {}
 {Get Window}
 Handle:=ConsoleWindowGetDefault(ConsoleDeviceDefault);
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {WriteChr}
 ConsoleWindowWriteChr(Handle,AChr);
end;

{==============================================================================}

procedure ConsoleRead(var AText:String);
{Read text from console input and echo to the screen}
{Text: The text read from the console input}
begin
 {}
 ConsoleWindowReadLnEx(ConsoleWindowGetDefault(ConsoleDeviceDefault),AText,'',0,0,COLOR_NONE,COLOR_NONE,False,True,nil,nil);
end;

{==============================================================================}

procedure ConsoleReadLn(var AText:String);
{Read text from console input and echo to the screen}
{Text: The text read from the console input}
begin
 {}
 ConsoleWindowReadLnEx(ConsoleWindowGetDefault(ConsoleDeviceDefault),AText,'',0,0,COLOR_NONE,COLOR_NONE,True,True,nil,nil);
end;

{==============================================================================}

procedure ConsoleReadChr(var AChr:Char);
{Read a character from console input and echo to the screen}
{Chr: The character read from the console input}
begin
 {}
 ConsoleWindowReadChrEx(ConsoleWindowGetDefault(ConsoleDeviceDefault),AChr,'',0,0,COLOR_NONE,COLOR_NONE,True,False);
end;

{==============================================================================}
{==============================================================================}
{RTL Text IO Functions}
function SysTextIOWriteChar(ACh:Char;AUserData:Pointer):Boolean;
{Handler for platform TextIOWriteChar function}

{Note: Not intended to be called directly by applications}
var
 Handle:TWindowHandle;
begin
 {}
 Result:=True;

 {WriteChr}
 ConsoleWindowWriteChr(ConsoleTextIOOutputHandle,ACh);
end;

{==============================================================================}
{==============================================================================}
{RTL Console Functions}
function SysConsoleWriteChar(ACh:Char;AUserData:Pointer):Boolean;
{Handler for platform ConsoleWriteChar function}

{Note: Not intended to be called directly by applications}
var
 Handle:TWindowHandle;
begin
 {}
 Result:=True;

 {Get Window}
 Handle:=ConsoleWindowGetDefault(ConsoleDeviceDefault);
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {WriteChr}
 ConsoleWindowWriteChr(Handle,ACh);
end;

{==============================================================================}
{==============================================================================}
{Framebuffer Console Functions}
function FramebufferConsoleOpen(Console:PConsoleDevice):LongWord;
{Implementation of ConsoleDeviceOpen API for FramebufferConsole}
{Note: Not intended to be called directly by applications, use ConsoleDeviceOpen instead}
var
 Framebuffer:PFramebufferDevice;
 Properties:TFramebufferProperties;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'FramebufferConsole: Console Open');
 {$ENDIF}

 if MutexLock(Console.Lock) = ERROR_SUCCESS then
  begin
   try
    {Get Framebuffer}
    Framebuffer:=PFramebufferConsole(Console).Framebuffer;

    {Check Framebuffer}
    if Framebuffer = nil then Exit;
    if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit;
    if Framebuffer.Address = 0 then Exit;

    {Get Properties}
    Result:=FramebufferDeviceGetProperties(Framebuffer,@Properties);
    if Result = ERROR_SUCCESS then
     begin
      {Update Console}
      Console.Width:=Properties.PhysicalWidth;
      Console.Height:=Properties.PhysicalHeight;
      Console.Format:=Properties.Format;

      {Check Colors}
      if Console.Forecolor = COLOR_NONE then Console.Forecolor:=CONSOLE_DEFAULT_FORECOLOR;
      if Console.Backcolor = COLOR_NONE then Console.Backcolor:=CONSOLE_DEFAULT_BACKCOLOR;
      if Console.Bordercolor = COLOR_NONE then Console.Bordercolor:=CONSOLE_DEFAULT_BORDERCOLOR;
      if PFramebufferConsole(Console).DesktopColor = COLOR_NONE then PFramebufferConsole(Console).DesktopColor:=FRAMEBUFFER_CONSOLE_DEFAULT_DESKTOPCOLOR;

      {Check Font}
      if Console.Font = INVALID_HANDLE_VALUE then Console.Font:=ConsoleDeviceGetDefaultFont;
      if Console.Font = INVALID_HANDLE_VALUE then Console.Font:=FontGetDefault;

      {Check Offset}
      if PFramebufferConsole(Console).DesktopOffset < Console.Borderwidth then PFramebufferConsole(Console).DesktopOffset:=Console.Borderwidth;
      if (PFramebufferConsole(Console).DesktopOffset * 2) >= Console.Width then Exit;
      if (PFramebufferConsole(Console).DesktopOffset * 2) >= Console.Height then Exit;

      {Update Console}
      PFramebufferConsole(Console).DesktopX:=PFramebufferConsole(Console).DesktopOffset;
      PFramebufferConsole(Console).DesktopY:=PFramebufferConsole(Console).DesktopOffset;
      PFramebufferConsole(Console).DesktopWidth:=Console.Width - (PFramebufferConsole(Console).DesktopOffset * 2);
      PFramebufferConsole(Console).DesktopHeight:=Console.Height - (PFramebufferConsole(Console).DesktopOffset * 2);

      {Update Statistics}
      Inc(Console.OpenCount);

      {Draw Desktop}
      Result:=FramebufferConsoleDrawDesktop(Console);
     end;
   finally
    MutexUnlock(Console.Lock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function FramebufferConsoleClose(Console:PConsoleDevice):LongWord;
{Implementation of ConsoleDeviceClose API for FramebufferConsole}
{Note: Not intended to be called directly by applications, use ConsoleDeviceClose instead}
var
 Framebuffer:PFramebufferDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'FramebufferConsole: Console Close');
 {$ENDIF}

 if MutexLock(Console.Lock) = ERROR_SUCCESS then
  begin
   try
    {Get Framebuffer}
    Framebuffer:=PFramebufferConsole(Console).Framebuffer;

    {Check Framebuffer}
    if Framebuffer = nil then Exit;
    if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit;
    {if Framebuffer.Address = 0 then Exit;} {Do not fail}

    {Clear Console}
    Result:=FramebufferConsoleClear(Console,COLOR_BLACK);
    {if Result <> ERROR_SUCCESS then Exit;} {Do not fail}

    {Update Console}
    {Console}
    Console.Width:=0;
    Console.Height:=0;
    Console.Format:=COLOR_FORMAT_UNKNOWN;
    {Framebuffer}
    PFramebufferConsole(Console).DesktopX:=0;
    PFramebufferConsole(Console).DesktopY:=0;
    PFramebufferConsole(Console).DesktopWidth:=0;
    PFramebufferConsole(Console).DesktopHeight:=0;
    {Buffer}
    if PFramebufferConsole(Console).LineBuffer <> nil then
     begin
      if ((Console.Device.DeviceFlags and CONSOLE_FLAG_DMA_MASK) <> 0) and (DMAAvailable) then
       begin
        DMAReleaseBuffer(PFramebufferConsole(Console).LineBuffer);
       end
      else
       begin
        FreeMem(PFramebufferConsole(Console).LineBuffer);
       end;
      PFramebufferConsole(Console).LineBuffer:=nil;
     end;

    {Update Statistics}
    Inc(Console.CloseCount);

    {Get Result}
    Result:=ERROR_SUCCESS;
   finally
    MutexUnlock(Console.Lock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function FramebufferConsoleClear(Console:PConsoleDevice;Color:LongWord):LongWord;
{Implementation of ConsoleDeviceClear API for FramebufferConsole}
{Note: Not intended to be called directly by applications, use ConsoleDeviceClear instead}
var
 Flags:LongWord;
 ColorFormat:LongWord;
 Framebuffer:PFramebufferDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Color}
 {if Color = COLOR_NONE then Exit;}

 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'FramebufferConsole: Console Clear');
 {$ENDIF}

 if MutexLock(Console.Lock) = ERROR_SUCCESS then
  begin
   try
    {Get Framebuffer}
    Framebuffer:=PFramebufferConsole(Console).Framebuffer;

    {Check Framebuffer}
    if Framebuffer = nil then Exit;

    {Get Flags}
    Flags:=FRAMEBUFFER_TRANSFER_NONE;
    if (Console.Device.DeviceFlags and CONSOLE_FLAG_DMA_CLEAR) <> 0 then
     begin
      Flags:=FRAMEBUFFER_TRANSFER_DMA;
     end;

    {Get Color}
    ColorDefaultToFormat(Console.Format,Color,@ColorFormat,(Console.Device.DeviceFlags and CONSOLE_FLAG_COLOR_REVERSE) <> 0);

    {Framebuffer Fill}
    Result:=FramebufferDeviceFillRect(Framebuffer,0,0,Console.Width,Console.Height,ColorFormat,Flags);
    if Result <> ERROR_SUCCESS then Exit;

    {Update Statistics}
    Inc(Console.ClearCount);

    {Get Result}
    Result:=ERROR_SUCCESS;
   finally
    MutexUnlock(Console.Lock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function FramebufferConsoleScroll(Console:PConsoleDevice;X1,Y1,X2,Y2,Count,Direction:LongWord):LongWord;
{Implementation of ConsoleDeviceScroll API for FramebufferConsole}
{Note: Not intended to be called directly by applications, use ConsoleDeviceScroll instead}
var
 Flags:LongWord;
 Framebuffer:PFramebufferDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check X1, Y1, X2, Y2 (Must be a line or box)}
 if X1 > X2 then Exit;
 if Y1 > Y2 then Exit;
 if (X1 = X2) and (Y1 = Y2) then Exit; {This would be a pixel}

 {Check Count}
 if Count < 1 then Exit;

 {Check Direction}
 if Direction > CONSOLE_DIRECTION_RIGHT then Exit;

 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'FramebufferConsole: Console Scroll');
 {$ENDIF}

 if MutexLock(Console.Lock) = ERROR_SUCCESS then
  begin
   try
    {Check X1, Y1, X2, Y2}
    if X1 >= Console.Width then Exit;
    if Y1 >= Console.Height then Exit;
    if X2 >= Console.Width then Exit;
    if Y2 >= Console.Height then Exit;

    {Get Framebuffer}
    Framebuffer:=PFramebufferConsole(Console).Framebuffer;

    {Check Framebuffer}
    if Framebuffer = nil then Exit;

    {Get Flags}
    Flags:=FRAMEBUFFER_TRANSFER_NONE;
    if (Console.Device.DeviceFlags and CONSOLE_FLAG_DMA_SCROLL) <> 0 then
     begin
      Flags:=FRAMEBUFFER_TRANSFER_DMA;
     end;

    {Check Direction}
    case Direction of
     CONSOLE_DIRECTION_UP:begin
       {Check Count}
       if Count > Y1 then Exit;

       {Framebuffer Copy}
       Result:=FramebufferDeviceCopyRect(Framebuffer,X1,Y1,X1,(Y1 - Count),((X2 - X1) + 1),((Y2 - Y1) + 1),Flags);
       if Result <> ERROR_SUCCESS then Exit;
      end;
     CONSOLE_DIRECTION_DOWN:begin
       {Check Count}
       if (Y2 + Count) >= Console.Height then Exit;

       {Framebuffer Copy}
       Result:=FramebufferDeviceCopyRect(Framebuffer,X1,Y1,X1,(Y1 + Count),((X2 - X1) + 1),((Y2 - Y1) + 1),Flags);
       if Result <> ERROR_SUCCESS then Exit;
      end;
     CONSOLE_DIRECTION_LEFT:begin
       {Check Count}
       if Count > X1 then Exit;

       {Framebuffer Copy}
       Result:=FramebufferDeviceCopyRect(Framebuffer,X1,Y1,(X1 - Count),Y1,((X2 - X1) + 1),((Y2 - Y1) + 1),Flags);
       if Result <> ERROR_SUCCESS then Exit;
      end;
     CONSOLE_DIRECTION_RIGHT:begin
       {Check Count}
       if (X2 + Count) >= Console.Width then Exit;

       {Framebuffer Copy}
       Result:=FramebufferDeviceCopyRect(Framebuffer,X1,Y1,(X1 + Count),Y1,((X2 - X1) + 1),((Y2 - Y1) + 1),Flags);
       if Result <> ERROR_SUCCESS then Exit;
      end;
    end;

    {Update Statistics}
    Inc(Console.ScrollCount);

    {Get Result}
    Result:=ERROR_SUCCESS;
   finally
    MutexUnlock(Console.Lock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function FramebufferConsoleDrawBox(Console:PConsoleDevice;X1,Y1,X2,Y2,Color,Width:LongWord):LongWord;
{Implementation of ConsoleDeviceDrawBox API for FramebufferConsole}
{Note: Not intended to be called directly by applications, use ConsoleDeviceDrawBox instead}
var
 Flags:LongWord;
 ColorFormat:LongWord;
 Framebuffer:PFramebufferDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check X1, Y1, X2, Y2 (Must be a box}
 if X1 >= X2 then Exit;
 if Y1 >= Y2 then Exit;

 {Check Color}
 {if Color = COLOR_NONE then Exit;}

 {Check Width}
 if Width < 1 then Exit;

 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'FramebufferConsole: Console Draw Box');
 {$ENDIF}

 if MutexLock(Console.Lock) = ERROR_SUCCESS then
  begin
   try
    {Check X1, Y1, X2, Y2}
    if X1 >= Console.Width then Exit;
    if Y1 >= Console.Height then Exit;
    if X2 >= Console.Width then Exit;
    if Y2 >= Console.Height then Exit;

    {Get Framebuffer}
    Framebuffer:=PFramebufferConsole(Console).Framebuffer;

    {Check Framebuffer}
    if Framebuffer = nil then Exit;

    {Get Flags}
    Flags:=FRAMEBUFFER_TRANSFER_NONE;
    if (Console.Device.DeviceFlags and CONSOLE_FLAG_DMA_BOX) <> 0 then
     begin
      Flags:=FRAMEBUFFER_TRANSFER_DMA;
     end;

    {Get Color}
    ColorDefaultToFormat(Console.Format,Color,@ColorFormat,(Console.Device.DeviceFlags and CONSOLE_FLAG_COLOR_REVERSE) <> 0);

    {Lock Framebuffer}
    if MutexLock(Framebuffer.Lock) <> ERROR_SUCCESS then Exit;

    {Draw Top}
    Result:=FramebufferDeviceFillRect(Framebuffer,X1,Y1,((X2 - X1) + 1),Width,ColorFormat,Flags);
    if Result = ERROR_SUCCESS then
     begin
      {Draw Bottom}
      Result:=FramebufferDeviceFillRect(Framebuffer,X1,((Y2 - Width) + 1),((X2 - X1) + 1),Width,ColorFormat,Flags);
      if Result = ERROR_SUCCESS then
       begin
        {Draw Left}
        Result:=FramebufferDeviceFillRect(Framebuffer,X1,Y1,Width,((Y2 - Y1) + 1),ColorFormat,Flags);
        if Result = ERROR_SUCCESS then
         begin
          {Draw Right}
          Result:=FramebufferDeviceFillRect(Framebuffer,((X2 - Width) + 1),Y1,Width,((Y2 - Y1) + 1),ColorFormat,Flags);
         end;
       end;
     end;

    {Unlock Framebuffer}
    MutexUnlock(Framebuffer.Lock);
    if Result <> ERROR_SUCCESS then Exit;

    {Update Statistics}
    Inc(Console.DrawCount);

    {Get Result}
    Result:=ERROR_SUCCESS;
   finally
    MutexUnlock(Console.Lock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function FramebufferConsoleDrawLine(Console:PConsoleDevice;X1,Y1,X2,Y2,Color,Width:LongWord):LongWord;
{Implementation of ConsoleDeviceDrawLine API for FramebufferConsole}
{Note: Not intended to be called directly by applications, use ConsoleDeviceDrawLine instead}
var
 Flags:LongWord;
 ColorFormat:LongWord;
 Framebuffer:PFramebufferDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check X1, Y1, X2, Y2 (Must be a line)}
 if X1 > X2 then Exit;
 if Y1 > Y2 then Exit;
 if (X1 = X2) and (Y1 = Y2) then Exit;   {This would be a pixel}
 if (X1 <> X2) and (Y1 <> Y2) then Exit; {This would be a box or a diagonal line (See: ConsoleDevicePlotLine)}

 {Check Color}
 {if Color = COLOR_NONE then Exit;}

 {Check Width}
 if Width < 1 then Exit;

 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'FramebufferConsole: Console Draw Line');
 {$ENDIF}

 if MutexLock(Console.Lock) = ERROR_SUCCESS then
  begin
   try
    {Check X1, Y1, X2, Y2}
    if X1 >= Console.Width then Exit;
    if Y1 >= Console.Height then Exit;
    if X2 >= Console.Width then Exit;
    if Y2 >= Console.Height then Exit;

    {Get Framebuffer}
    Framebuffer:=PFramebufferConsole(Console).Framebuffer;

    {Check Framebuffer}
    if Framebuffer = nil then Exit;

    {Get Flags}
    Flags:=FRAMEBUFFER_TRANSFER_NONE;
    if (Console.Device.DeviceFlags and CONSOLE_FLAG_DMA_LINE) <> 0 then
     begin
      Flags:=FRAMEBUFFER_TRANSFER_DMA;
     end;

    {Get Color}
    ColorDefaultToFormat(Console.Format,Color,@ColorFormat,(Console.Device.DeviceFlags and CONSOLE_FLAG_COLOR_REVERSE) <> 0);

    {Get Direction}
    if X1 = X2 then
     begin
      {Vertical}
      Result:=FramebufferDeviceFillRect(Framebuffer,X1,Y1,Width,((Y2 - Y1) + 1),ColorFormat,Flags);
      if Result <> ERROR_SUCCESS then Exit;
     end
    else if Y1 = Y2 then
     begin
      {Horizontal}
      Result:=FramebufferDeviceFillRect(Framebuffer,X1,Y1,((X2 - X1) + 1),Width,ColorFormat,Flags);
      if Result <> ERROR_SUCCESS then Exit;
     end;

    {Update Statistics}
    Inc(Console.DrawCount);

    {Get Result}
    Result:=ERROR_SUCCESS;
   finally
    MutexUnlock(Console.Lock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function FramebufferConsoleDrawChar(Console:PConsoleDevice;Handle:TFontHandle;Ch:Char;X,Y,Forecolor,Backcolor:LongWord):LongWord;
{Implementation of ConsoleDeviceDrawChar API for FramebufferConsole}
{Note: Not intended to be called directly by applications, use ConsoleDeviceDrawChar instead}
var
 Row:LongWord;
 Column:LongWord;
 Buffer:Pointer;
 Offset:LongWord;
 Character:LongWord;

 Flags:LongWord;
 Bytes:LongWord;
 Reverse:Boolean;
 Font:PFontEntry;
 Framebuffer:PFramebufferDevice;
 Properties:TFramebufferProperties;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {Check Colors}
 {if Forecolor = COLOR_NONE then Exit;}
 {if Backcolor = COLOR_NONE then Exit;}

 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'FramebufferConsole: Console Draw Char');
 {$ENDIF}

 if MutexLock(Console.Lock) = ERROR_SUCCESS then
  begin
   try
    {Get Font}
    Font:=PFontEntry(Handle);
    if Font = nil then Exit;
    if Font.Signature <> FONT_SIGNATURE then Exit;

    {Check X, Y}
    if X >= Console.Width then Exit;
    if Y >= Console.Height then Exit;
    if X + Font.CharWidth > Console.Width then Exit;
    if Y + Font.CharHeight > Console.Height then Exit;

    {Get Framebuffer}
    Framebuffer:=PFramebufferConsole(Console).Framebuffer;

    {Check Framebuffer}
    if Framebuffer = nil then Exit;

    {Get Flags}
    Flags:=FRAMEBUFFER_TRANSFER_NONE;
    if (Console.Device.DeviceFlags and CONSOLE_FLAG_DMA_TEXT) <> 0 then
     begin
      Flags:=FRAMEBUFFER_TRANSFER_DMA;
     end;

    {Get Bytes}
    Bytes:=ColorFormatToBytes(Console.Format);
    if Bytes = 0 then Exit;

    {Get Reverse}
    Reverse:=(Console.Device.DeviceFlags and CONSOLE_FLAG_COLOR_REVERSE) <> 0;

    {Check Buffer}
    if PFramebufferConsole(Console).LineBuffer = nil then
     begin
      {Get Properties}
      if FramebufferDeviceGetProperties(Framebuffer,@Properties) <> ERROR_SUCCESS then Exit;

      {Check DMA}
      if ((Console.Device.DeviceFlags and CONSOLE_FLAG_DMA_MASK) <> 0) and (DMAAvailable) then
       begin
        PFramebufferConsole(Console).LineBuffer:=DMAAllocateBuffer(Properties.Pitch);
       end
      else
       begin
        PFramebufferConsole(Console).LineBuffer:=GetMem(Properties.Pitch);
       end;
     end;
    {Get Buffer}
    Buffer:=PFramebufferConsole(Console).LineBuffer;
    if Buffer = nil then Exit;

    {Lock Framebuffer}
    if MutexLock(Framebuffer.Lock) <> ERROR_SUCCESS then Exit;

    {Check Cursor}
    if (Framebuffer.CursorState = FRAMEBUFFER_CURSOR_ENABLED) and ((Framebuffer.Device.DeviceFlags and FRAMEBUFFER_FLAG_CURSOR) = 0) then
     begin
      {Hide Cursor}
      FramebufferDeviceHideCursor(Framebuffer);

      {Set Update}
      Framebuffer.CursorUpdate:=True;
     end;

    {Set Result}
    Result:=ERROR_SUCCESS;

    {Get Rows}
    for Row:=0 to Font.CharHeight - 1 do
     begin
      {Get Character}
      Character:=0;
      case Font.CharWidth of
       8:Character:=PFontChars8(Font.CharData)[(Byte(Ch) * Font.CharHeight) + Row];
       9..16:Character:=PFontChars16(Font.CharData)[(Byte(Ch) * Font.CharHeight) + Row];
       17..32:Character:=PFontChars32(Font.CharData)[(Byte(Ch) * Font.CharHeight) + Row];
      end;

      {Map Character}
      Offset:=(Font.CharWidth - 1) * Bytes;
      for Column:=Font.CharWidth - 1 downto 0 do
       begin
        if (Character and $01) = $01 then
         begin
          ColorDefaultToFormat(Console.Format,Forecolor,Pointer(Buffer + Offset),Reverse);
         end
        else
         begin
          ColorDefaultToFormat(Console.Format,Backcolor,Pointer(Buffer + Offset),Reverse);
         end;

        Dec(Offset,Bytes);
        Character:=Character shr 1;
       end;

      {Framebuffer Write}
      Result:=FramebufferDeviceWrite(Framebuffer,X,Y + Row,Buffer,Font.CharWidth,Flags);
      if Result <> ERROR_SUCCESS then Break;
     end;

    {Check Cursor}
    if (Framebuffer.CursorState = FRAMEBUFFER_CURSOR_ENABLED) and ((Framebuffer.Device.DeviceFlags and FRAMEBUFFER_FLAG_CURSOR) = 0) then
     begin
      {Clear Update}
      Framebuffer.CursorUpdate:=False;

      {Show Cursor}
      FramebufferDeviceShowCursor(Framebuffer);
     end;

    {Unlock Framebuffer}
    MutexUnlock(Framebuffer.Lock);
    if Result <> ERROR_SUCCESS then Exit;

    {Update Statistics}
    Inc(Console.DrawCount);
   finally
    MutexUnlock(Console.Lock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function FramebufferConsoleDrawText(Console:PConsoleDevice;Handle:TFontHandle;const Text:String;X,Y,Forecolor,Backcolor,Len:LongWord):LongWord;
{Implementation of ConsoleDeviceDrawText API for FramebufferConsole}
{Note: Not intended to be called directly by applications, use ConsoleDeviceDrawText instead}
var
 Row:LongWord;
 Count:LongWord;
 Column:LongWord;
 Buffer:Pointer;
 Offset:LongWord;
 Character:LongWord;

 Flags:LongWord;
 Bytes:LongWord;
 Reverse:Boolean;
 Font:PFontEntry;
 Framebuffer:PFramebufferDevice;
 Properties:TFramebufferProperties;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {Check Text}
 if Length(Text) = 0 then Exit;

 {Check Colors}
 {if Forecolor = COLOR_NONE then Exit;}
 {if Backcolor = COLOR_NONE then Exit;}

 {Check Length}
 if Len < 1 then Exit;
 if Len > Length(Text) then Len:=Length(Text);

 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'FramebufferConsole: Console Draw Text');
 {$ENDIF}

 if MutexLock(Console.Lock) = ERROR_SUCCESS then
  begin
   try
    {Get Font}
    Font:=PFontEntry(Handle);
    if Font = nil then Exit;
    if Font.Signature <> FONT_SIGNATURE then Exit;

    {Check X, Y}
    if X >= Console.Width then Exit;
    if Y >= Console.Height then Exit;
    if X + (Font.CharWidth * Len) > Console.Width then Exit;
    if Y + Font.CharHeight > Console.Height then Exit;

    {Get Framebuffer}
    Framebuffer:=PFramebufferConsole(Console).Framebuffer;

    {Check Framebuffer}
    if Framebuffer = nil then Exit;

    {Get Flags}
    Flags:=FRAMEBUFFER_TRANSFER_NONE;
    if (Console.Device.DeviceFlags and CONSOLE_FLAG_DMA_TEXT) <> 0 then
     begin
      Flags:=FRAMEBUFFER_TRANSFER_DMA;
     end;

    {Get Bytes}
    Bytes:=ColorFormatToBytes(Console.Format);
    if Bytes = 0 then Exit;

    {Get Reverse}
    Reverse:=(Console.Device.DeviceFlags and CONSOLE_FLAG_COLOR_REVERSE) <> 0;

    {Check Buffer}
    if PFramebufferConsole(Console).LineBuffer = nil then
     begin
      {Get Properties}
      if FramebufferDeviceGetProperties(Framebuffer,@Properties) <> ERROR_SUCCESS then Exit;

      {Check DMA}
      if ((Console.Device.DeviceFlags and CONSOLE_FLAG_DMA_MASK) <> 0) and (DMAAvailable) then
       begin
        PFramebufferConsole(Console).LineBuffer:=DMAAllocateBuffer(Properties.Pitch);
       end
      else
       begin
        PFramebufferConsole(Console).LineBuffer:=GetMem(Properties.Pitch);
       end;
     end;
    {Get Buffer}
    Buffer:=PFramebufferConsole(Console).LineBuffer;
    if Buffer = nil then Exit;

    {Lock Framebuffer}
    if MutexLock(Framebuffer.Lock) <> ERROR_SUCCESS then Exit;

    {Check Cursor}
    if (Framebuffer.CursorState = FRAMEBUFFER_CURSOR_ENABLED) and ((Framebuffer.Device.DeviceFlags and FRAMEBUFFER_FLAG_CURSOR) = 0) then
     begin
      {Hide Cursor}
      FramebufferDeviceHideCursor(Framebuffer);

      {Set Update}
      Framebuffer.CursorUpdate:=True;
     end;

    {Set Result}
    Result:=ERROR_SUCCESS;

    {Get Rows}
    for Row:=0 to Font.CharHeight - 1 do
     begin
      {Get Text}
      Offset:=0;
      for Count:=1 to Len do
       begin
        {Get Character}
        Character:=0;
        case Font.CharWidth of
         8:Character:=PFontChars8(Font.CharData)[(Byte(Text[Count]) * Font.CharHeight) + Row];
         9..16:Character:=PFontChars16(Font.CharData)[(Byte(Text[Count]) * Font.CharHeight) + Row];
         17..32:Character:=PFontChars32(Font.CharData)[(Byte(Text[Count]) * Font.CharHeight) + Row];
        end;

        {Map Character}
        Inc(Offset,Font.CharWidth * Bytes);
        for Column:=Font.CharWidth - 1 downto 0 do
         begin
          Dec(Offset,Bytes);
          if (Character and $01) = $01 then
           begin
            ColorDefaultToFormat(Console.Format,Forecolor,Pointer(Buffer + Offset),Reverse);
           end
          else
           begin
            ColorDefaultToFormat(Console.Format,Backcolor,Pointer(Buffer + Offset),Reverse);
           end;

          Character:=Character shr 1;
         end;

        Inc(Offset,Font.CharWidth * Bytes);
       end;

      {Framebuffer Write}
      Result:=FramebufferDeviceWrite(Framebuffer,X,Y + Row,Buffer,Font.CharWidth * Len,Flags);
      if Result <> ERROR_SUCCESS then Break;
     end;

    {Check Cursor}
    if (Framebuffer.CursorState = FRAMEBUFFER_CURSOR_ENABLED) and ((Framebuffer.Device.DeviceFlags and FRAMEBUFFER_FLAG_CURSOR) = 0) then
     begin
      {Clear Update}
      Framebuffer.CursorUpdate:=False;

      {Show Cursor}
      FramebufferDeviceShowCursor(Framebuffer);
     end;

    {Unlock Framebuffer}
    MutexUnlock(Framebuffer.Lock);
    if Result <> ERROR_SUCCESS then Exit;

    {Update Statistics}
    Inc(Console.DrawCount);
   finally
    MutexUnlock(Console.Lock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function FramebufferConsoleDrawPixel(Console:PConsoleDevice;X,Y,Color:LongWord):LongWord;
{Implementation of ConsoleDeviceDrawPixel API for FramebufferConsole}
{Note: Not intended to be called directly by applications, use ConsoleDeviceDrawPixel instead}
var
 Address:Pointer;
 Framebuffer:PFramebufferDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Color}
 {if Color = COLOR_NONE then Exit;}

 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'FramebufferConsole: Console Draw Pixel');
 {$ENDIF}

 if MutexLock(Console.Lock) = ERROR_SUCCESS then
  begin
   try
    {Check X, Y}
    if X >= Console.Width then Exit;
    if Y >= Console.Height then Exit;

    {Get Framebuffer}
    Framebuffer:=PFramebufferConsole(Console).Framebuffer;

    {Check Framebuffer}
    if Framebuffer = nil then Exit;

    {Set Result}
    Result:=ERROR_SUCCESS;

    {Lock Framebuffer}
    if MutexLock(Framebuffer.Lock) <> ERROR_SUCCESS then Exit;

    {Check Cursor}
    if (Framebuffer.CursorState = FRAMEBUFFER_CURSOR_ENABLED) and ((Framebuffer.Device.DeviceFlags and FRAMEBUFFER_FLAG_CURSOR) = 0) then
     begin
      {Hide Cursor}
      if not Framebuffer.CursorUpdate then FramebufferDeviceHideCursor(Framebuffer);
     end;

    {Get Address}
    Address:=FramebufferDeviceGetPoint(Framebuffer,X,Y);
    if Address <> nil then
     begin
      {Memory Barrier}
      DataMemoryBarrier;  {Before the First Write}

      {Draw Pixel}
      ColorDefaultToFormat(Console.Format,Color,Address,(Console.Device.DeviceFlags and CONSOLE_FLAG_COLOR_REVERSE) <> 0);

      {Check Commit}
      if ((Framebuffer.Device.DeviceFlags and FRAMEBUFFER_FLAG_COMMIT) <> 0) and Assigned(Framebuffer.DeviceCommit) then
       begin
        Framebuffer.DeviceCommit(Framebuffer,PtrUInt(Address),ColorFormatToBytes(Console.Format),FRAMEBUFFER_TRANSFER_NONE);
       end;

      {Check Mark}
      if ((Framebuffer.Device.DeviceFlags and FRAMEBUFFER_FLAG_MARK) <> 0) and Assigned(Framebuffer.DeviceMark) then
       begin
        Framebuffer.DeviceMark(Framebuffer,X,Y,1,1,FRAMEBUFFER_TRANSFER_NONE);
       end;
     end
    else
     begin
      Result:=ERROR_OPERATION_FAILED;
     end;

    {Check Cursor}
    if (Framebuffer.CursorState = FRAMEBUFFER_CURSOR_ENABLED) and ((Framebuffer.Device.DeviceFlags and FRAMEBUFFER_FLAG_CURSOR) = 0) then
     begin
      {Show Cursor}
      if not Framebuffer.CursorUpdate then FramebufferDeviceShowCursor(Framebuffer);
     end;

    {Unlock Framebuffer}
    MutexUnlock(Framebuffer.Lock);
    if Result <> ERROR_SUCCESS then Exit;

    {Update Statistics}
    Inc(Console.DrawCount);
   finally
    MutexUnlock(Console.Lock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function FramebufferConsoleDrawBlock(Console:PConsoleDevice;X1,Y1,X2,Y2,Color:LongWord):LongWord;
{Implementation of ConsoleDeviceDrawBlock API for FramebufferConsole}
{Note: Not intended to be called directly by applications, use ConsoleDeviceDrawBlock instead}
var
 Flags:LongWord;
 ColorFormat:LongWord;
 Framebuffer:PFramebufferDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check X1, Y1, X2, Y2 (Must be a line, box or pixel)}
 if X1 > X2 then Exit;
 if Y1 > Y2 then Exit;

 {Check Color}
 {if Color = COLOR_NONE then Exit;}

 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'FramebufferConsole: Console Draw Block');
 {$ENDIF}

 if MutexLock(Console.Lock) = ERROR_SUCCESS then
  begin
   try
    {Check X1, Y1, X2, Y2}
    if X1 >= Console.Width then Exit;
    if Y1 >= Console.Height then Exit;
    if X2 >= Console.Width then Exit;
    if Y2 >= Console.Height then Exit;

    {Get Framebuffer}
    Framebuffer:=PFramebufferConsole(Console).Framebuffer;

    {Check Framebuffer}
    if Framebuffer = nil then Exit;

    {Get Flags}
    Flags:=FRAMEBUFFER_TRANSFER_NONE;
    if (Console.Device.DeviceFlags and CONSOLE_FLAG_DMA_FILL) <> 0 then
     begin
      Flags:=FRAMEBUFFER_TRANSFER_DMA;
     end;

    {Get Color}
    ColorDefaultToFormat(Console.Format,Color,@ColorFormat,(Console.Device.DeviceFlags and CONSOLE_FLAG_COLOR_REVERSE) <> 0);

    {Framebuffer Fill}
    Result:=FramebufferDeviceFillRect(Framebuffer,X1,Y1,((X2 - X1) + 1),((Y2 - Y1) + 1),ColorFormat,Flags);
    if Result <> ERROR_SUCCESS then Exit;

    {Update Statistics}
    Inc(Console.DrawCount);

    {Get Result}
    Result:=ERROR_SUCCESS;
   finally
    MutexUnlock(Console.Lock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function FramebufferConsoleDrawImage(Console:PConsoleDevice;X,Y:LongWord;Buffer:Pointer;Width,Height,Format,Skip:LongWord):LongWord;
{Implementation of ConsoleDeviceDrawImage API for FramebufferConsole}
{Note: Not intended to be called directly by applications, use ConsoleDeviceDrawImage instead}
var
 Count:LongWord;
 Offset:LongWord;
 Line:Pointer;
 Address:Pointer;
 Reverse:Boolean;
 FormatBytes:LongWord;
 ConsoleBytes:LongWord;
 Framebuffer:PFramebufferDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Buffer}
 if Buffer = nil then Exit;

 {Check Width and Height}
 if (Width = 0) or (Height = 0) then Exit;

 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'FramebufferConsole: Console Draw Image');
 {$ENDIF}

 if MutexLock(Console.Lock) = ERROR_SUCCESS then
  begin
   try
    {Check X, Y}
    if X >= Console.Width then Exit;
    if Y >= Console.Height then Exit;
    if X + Width > Console.Width then Exit;
    if Y + Height > Console.Height then Exit;

    {Get Framebuffer}
    Framebuffer:=PFramebufferConsole(Console).Framebuffer;

    {Get Reverse}
    Reverse:=(Console.Device.DeviceFlags and CONSOLE_FLAG_COLOR_REVERSE) <> 0;

    {Check Format}
    if (Format = COLOR_FORMAT_UNKNOWN) or ((Format = Console.Format) and not(Reverse)) then
     begin
      {Framebuffer Put}
      Result:=FramebufferDevicePutRect(Framebuffer,X,Y,Buffer,Width,Height,Skip,FRAMEBUFFER_TRANSFER_DMA); {Buffer is safe for DMA put}
      if Result <> ERROR_SUCCESS then Exit;
     end
    else
     begin
      {Get Format Bytes}
      FormatBytes:=ColorFormatToBytes(Format);

      {Get Console Bytes}
      ConsoleBytes:=ColorFormatToBytes(Console.Format);

      {Lock Framebuffer}
      if MutexLock(Framebuffer.Lock) <> ERROR_SUCCESS then Exit;

      {Check Cursor}
      if (Framebuffer.CursorState = FRAMEBUFFER_CURSOR_ENABLED) and ((Framebuffer.Device.DeviceFlags and FRAMEBUFFER_FLAG_CURSOR) = 0) then
       begin
        {Hide Cursor}
        if not Framebuffer.CursorUpdate then FramebufferDeviceHideCursor(Framebuffer);
       end;

      {Set Result}
      Result:=ERROR_SUCCESS;

      {Set Offset}
      Offset:=0;

      {Check Format}
      if Format = COLOR_FORMAT_DEFAULT then
       begin
        {Draw Lines}
        for Count:=0 to Height - 1 do
         begin
          {Get Address}
          Address:=FramebufferDeviceGetPoint(Framebuffer,X,Y + Count);
          if Address <> nil then
           begin
            {Memory Barrier}
            DataMemoryBarrier;  {Before the First Write}

            {Convert from Format (COLOR_FORMAT_DEFAULT) to Console.Format}
            PixelsDefaultToFormat(Console.Format,Pointer(Buffer + Offset),Address,Width,Reverse);

            {Check Commit}
            if ((Framebuffer.Device.DeviceFlags and FRAMEBUFFER_FLAG_COMMIT) <> 0) and Assigned(Framebuffer.DeviceCommit) then
             begin
              Framebuffer.DeviceCommit(Framebuffer,PtrUInt(Address),Width * ConsoleBytes,FRAMEBUFFER_TRANSFER_NONE);
             end;

            {Check Mark}
            if ((Framebuffer.Device.DeviceFlags and FRAMEBUFFER_FLAG_MARK) <> 0) and Assigned(Framebuffer.DeviceMark) then
             begin
              Framebuffer.DeviceMark(Framebuffer,X,Y + Count,Width,1,FRAMEBUFFER_TRANSFER_NONE);
             end;
           end
          else
           begin
            Result:=ERROR_OPERATION_FAILED;
            Break;
           end;

          {Update Offset}
          Inc(Offset,(Width + Skip) * FormatBytes);
         end;
       end
      else if Console.Format = COLOR_FORMAT_DEFAULT then
       begin
        {Draw Lines}
        for Count:=0 to Height - 1 do
         begin
          {Get Address}
          Address:=FramebufferDeviceGetPoint(Framebuffer,X,Y + Count);
          if Address <> nil then
           begin
            {Memory Barrier}
            DataMemoryBarrier;  {Before the First Write}

            {Convert from Format to Console.Format (COLOR_FORMAT_DEFAULT)}
            PixelsFormatToDefault(Format,Pointer(Buffer + Offset),Address,Width,Reverse);

            {Check Commit}
            if ((Framebuffer.Device.DeviceFlags and FRAMEBUFFER_FLAG_COMMIT) <> 0) and Assigned(Framebuffer.DeviceCommit) then
             begin
              Framebuffer.DeviceCommit(Framebuffer,PtrUInt(Address),Width * ConsoleBytes,FRAMEBUFFER_TRANSFER_NONE);
             end;

            {Check Mark}
            if ((Framebuffer.Device.DeviceFlags and FRAMEBUFFER_FLAG_MARK) <> 0) and Assigned(Framebuffer.DeviceMark) then
             begin
              Framebuffer.DeviceMark(Framebuffer,X,Y + Count,Width,1,FRAMEBUFFER_TRANSFER_NONE);
             end;
           end
          else
           begin
            Result:=ERROR_OPERATION_FAILED;
            Break;
           end;

          {Update Offset}
          Inc(Offset,(Width + Skip) * FormatBytes);
         end;
       end
      else
       begin
        {Allocate Line}
        Line:=GetMem(Width * ColorFormatToBytes(COLOR_FORMAT_DEFAULT));
        if Line <> nil then
         begin
          {Draw Lines}
          for Count:=0 to Height - 1 do
           begin
            {Get Address}
            Address:=FramebufferDeviceGetPoint(Framebuffer,X,Y + Count);
            if Address <> nil then
             begin
              {Memory Barrier}
              DataMemoryBarrier;  {Before the First Write}

              {Convert from Format to Default (COLOR_FORMAT_DEFAULT)}
              PixelsFormatToDefault(Format,Pointer(Buffer + Offset),Line,Width,False);

              {Convert from Default (COLOR_FORMAT_DEFAULT) to Console.Format}
              PixelsDefaultToFormat(Console.Format,Line,Address,Width,Reverse);

              {Check Commit}
              if ((Framebuffer.Device.DeviceFlags and FRAMEBUFFER_FLAG_COMMIT) <> 0) and Assigned(Framebuffer.DeviceCommit) then
               begin
                Framebuffer.DeviceCommit(Framebuffer,PtrUInt(Address),Width * ConsoleBytes,FRAMEBUFFER_TRANSFER_NONE);
               end;

              {Check Mark}
              if ((Framebuffer.Device.DeviceFlags and FRAMEBUFFER_FLAG_MARK) <> 0) and Assigned(Framebuffer.DeviceMark) then
               begin
                Framebuffer.DeviceMark(Framebuffer,X,Y + Count,Width,1,FRAMEBUFFER_TRANSFER_NONE);
               end;
             end
            else
             begin
              Result:=ERROR_OPERATION_FAILED;
              Break;
             end;

            {Update Offset}
            Inc(Offset,(Width + Skip) * FormatBytes);
           end;

          {Free Line}
          FreeMem(Line);
         end;
       end;

      {Check Cursor}
      if (Framebuffer.CursorState = FRAMEBUFFER_CURSOR_ENABLED) and ((Framebuffer.Device.DeviceFlags and FRAMEBUFFER_FLAG_CURSOR) = 0) then
       begin
        {Show Cursor}
        if not Framebuffer.CursorUpdate then FramebufferDeviceShowCursor(Framebuffer);
       end;

      {Unlock Framebuffer}
      MutexUnlock(Framebuffer.Lock);
      if Result <> ERROR_SUCCESS then Exit;
     end;

    {Update Statistics}
    Inc(Console.DrawCount);
   finally
    MutexUnlock(Console.Lock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function FramebufferConsoleDrawWindow(Console:PConsoleDevice;Handle:TWindowHandle;Flags:LongWord):LongWord;
{Implementation of ConsoleDeviceDrawWindow API for FramebufferConsole}
{Note: Not intended to be called directly by applications, use ConsoleDeviceDrawWindow instead}
{Note: Caller must hold the Window lock}
var
 Color:LongWord;
 Window:PConsoleWindow;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'FramebufferConsole: Console Draw Window');
 {$ENDIF}

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;

 {Check Window State}
 if Window.WindowState = WINDOW_STATE_INVISIBLE then
  begin
   {Check Flags}
   if (Flags and WINDOW_DRAW_FLAG_ALL) = WINDOW_DRAW_FLAG_ALL then
    begin
     {Draw Window}
     Result:=FramebufferConsoleDrawBlock(Console,Window.X1,Window.Y1,Window.X2,Window.Y2,PFramebufferConsole(Console).DesktopColor);
    end
   else
    begin
     Result:=ERROR_SUCCESS;
    end;
  end
 else if Window.WindowState = WINDOW_STATE_VISIBLE then
  begin
   {Check Border and Flags}
   if (Window.Borderwidth > 0) and ((Flags and WINDOW_DRAW_FLAG_BORDER) = WINDOW_DRAW_FLAG_BORDER) then
    begin
     {Check Active}
     if Console.WindowActive = Window then
      begin
       Color:=WINDOW_DEFAULT_ACTIVEBORDER;
      end
     else
      begin
       Color:=Window.Bordercolor;
      end;

     {Draw Border}
     Result:=FramebufferConsoleDrawBox(Console,Window.X1,Window.Y1,Window.X2,Window.Y2,Color,Window.Borderwidth);
     if Result <> ERROR_SUCCESS then Exit;
    end;

   {Check Flags}
   if (Flags and WINDOW_DRAW_FLAG_ALL) = WINDOW_DRAW_FLAG_ALL then
    begin
     {Draw Window}
     Result:=FramebufferConsoleDrawBlock(Console,Window.X1 + Window.Borderwidth,Window.Y1 + Window.Borderwidth,Window.X2 - Window.Borderwidth,Window.Y2 - Window.Borderwidth,Window.Backcolor);
    end
   else
    begin
     Result:=ERROR_SUCCESS;
    end;
  end;
end;

{==============================================================================}

function FramebufferConsoleDrawDesktop(Console:PConsoleDevice):LongWord;
{Internal function used by FramebufferConsole to draw the console desktop}
{Note: Not intended to be called directly by applications}
var
 Title:String;
 TitleX:LongWord;
 TitleY:LongWord;
 TitleLen:LongWord;
 TitleOffset:LongWord;
 TitleFont:TFontHandle;
 TitleForecolor:LongWord;
 TitleBackcolor:LongWord;

 BorderX1:LongWord;
 BorderY1:LongWord;
 BorderX2:LongWord;
 BorderY2:LongWord;
 BorderColor:LongWord;
 BorderWidth:LongWord;

 DesktopColor:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'FramebufferConsole: Console Draw Desktop');
 {$ENDIF}

 if MutexLock(Console.Lock) = ERROR_SUCCESS then
  begin
   try
    {Get Title}
    Title:=FRAMEBUFFER_CONSOLE_TITLE;
    if Length(FRAMEBUFFER_CONSOLE_MESSAGE) > 0 then Title:=FRAMEBUFFER_CONSOLE_MESSAGE;

    {Get Desktop}
    DesktopColor:=PFramebufferConsole(Console).DesktopColor;

    {Get Border}
    BorderX1:=(PFramebufferConsole(Console).DesktopOffset - Console.Borderwidth);
    BorderY1:=(PFramebufferConsole(Console).DesktopOffset - Console.Borderwidth);
    BorderX2:=(Console.Width - 1) - (PFramebufferConsole(Console).DesktopOffset - Console.Borderwidth);
    BorderY2:=(Console.Height - 1) - (PFramebufferConsole(Console).DesktopOffset - Console.Borderwidth);
    BorderColor:=Console.Bordercolor;
    BorderWidth:=Console.Borderwidth;

    {Get Title}
    TitleX:=PFramebufferConsole(Console).DesktopOffset;
    TitleY:=(PFramebufferConsole(Console).DesktopOffset - FontGetHeight(Console.Font)) div 2;
    TitleLen:=Length(Title);
    TitleFont:=Console.Font;
    TitleOffset:=PFramebufferConsole(Console).DesktopOffset;
    if FontGetHeight(Console.Font) > TitleOffset then TitleOffset:=0;
    TitleForecolor:=Console.Forecolor;
    TitleBackcolor:=PFramebufferConsole(Console).DesktopColor;

    {Draw Desktop}
    Result:=FramebufferConsoleClear(Console,DesktopColor);
    if Result <> ERROR_SUCCESS then Exit;

    {Check Border}
    if BorderWidth > 0 then
     begin
      {Draw Border}
      Result:=FramebufferConsoleDrawBox(Console,BorderX1,BorderY1,BorderX2,BorderY2,BorderColor,BorderWidth);
      if Result <> ERROR_SUCCESS then Exit;
     end;

    {Check Title}
    if TitleOffset > 0 then
     begin
      {Check Length}
      if (TitleX + (TitleLen * FontGetWidth(Console.Font))) > Console.Width then
       begin
        TitleLen:=(Console.Width - (TitleX + 1)) div FontGetWidth(Console.Font);
       end;

      {Draw Title}
      Result:=FramebufferConsoleDrawText(Console,TitleFont,Title,TitleX,TitleY,TitleForecolor,TitleBackcolor,TitleLen);
     end;
   finally
    MutexUnlock(Console.Lock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function FramebufferConsoleGetPixel(Console:PConsoleDevice;X,Y:LongWord;var Color:LongWord):LongWord;
{Implementation of ConsoleDeviceGetPixel API for FramebufferConsole}
{Note: Not intended to be called directly by applications, use ConsoleDeviceGetPixel instead}
var
 Address:Pointer;
 Framebuffer:PFramebufferDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'FramebufferConsole: Console Get Pixel');
 {$ENDIF}

 if MutexLock(Console.Lock) = ERROR_SUCCESS then
  begin
   try
    {Check X, Y}
    if X >= Console.Width then Exit;
    if Y >= Console.Height then Exit;

    {Get Framebuffer}
    Framebuffer:=PFramebufferConsole(Console).Framebuffer;

    {Check Framebuffer}
    if Framebuffer = nil then Exit;

    {Set Result}
    Result:=ERROR_SUCCESS;

    {Lock Framebuffer}
    if MutexLock(Framebuffer.Lock) <> ERROR_SUCCESS then Exit;

    {Check Cursor}
    if (Framebuffer.CursorState = FRAMEBUFFER_CURSOR_ENABLED) and ((Framebuffer.Device.DeviceFlags and FRAMEBUFFER_FLAG_CURSOR) = 0) then
     begin
      {Hide Cursor}
      if not Framebuffer.CursorUpdate then FramebufferDeviceHideCursor(Framebuffer);
     end;

    {Get Address}
    Address:=FramebufferDeviceGetPoint(Framebuffer,X,Y);
    if Address <> nil then
     begin
      {Get Pixel}
      ColorFormatAltToDefault(Console.Format,Address,Color,(Console.Device.DeviceFlags and CONSOLE_FLAG_COLOR_REVERSE) <> 0);

      {Memory Barrier}
      DataMemoryBarrier;  {After the Last Read}
     end
    else
     begin
      Result:=ERROR_OPERATION_FAILED;
     end;

    {Check Cursor}
    if (Framebuffer.CursorState = FRAMEBUFFER_CURSOR_ENABLED) and ((Framebuffer.Device.DeviceFlags and FRAMEBUFFER_FLAG_CURSOR) = 0) then
     begin
      {Show Cursor}
      if not Framebuffer.CursorUpdate then FramebufferDeviceShowCursor(Framebuffer);
     end;

    {Unlock Framebuffer}
    MutexUnlock(Framebuffer.Lock);
    if Result <> ERROR_SUCCESS then Exit;

    {Update Statistics}
    Inc(Console.GetCount);
   finally
    MutexUnlock(Console.Lock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function FramebufferConsoleGetImage(Console:PConsoleDevice;X,Y:LongWord;Buffer:Pointer;Width,Height,Format,Skip:LongWord):LongWord;
{Implementation of ConsoleDeviceGetImage API for FramebufferConsole}
{Note: Not intended to be called directly by applications, use ConsoleDeviceGetImage instead}
var
 Count:LongWord;
 Offset:LongWord;
 Line:Pointer;
 Address:Pointer;
 Reverse:Boolean;
 FormatBytes:LongWord;
 ConsoleBytes:LongWord;
 Framebuffer:PFramebufferDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Buffer}
 if Buffer = nil then Exit;

 {Check Width and Height}
 if (Width = 0) or (Height = 0) then Exit;

 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'FramebufferConsole: Console Get Image');
 {$ENDIF}

 if MutexLock(Console.Lock) = ERROR_SUCCESS then
  begin
   try
    {Check X, Y}
    if X >= Console.Width then Exit;
    if Y >= Console.Height then Exit;
    if X + Width > Console.Width then Exit;
    if Y + Height > Console.Height then Exit;

    {Get Framebuffer}
    Framebuffer:=PFramebufferConsole(Console).Framebuffer;

    {Get Reverse}
    Reverse:=(Console.Device.DeviceFlags and CONSOLE_FLAG_COLOR_REVERSE) <> 0;

    {Check Format}
    if (Format = COLOR_FORMAT_UNKNOWN) or ((Format = Console.Format) and not(Reverse)) then
     begin
      {Framebuffer Get}
      Result:=FramebufferDeviceGetRect(Framebuffer,X,Y,Buffer,Width,Height,Skip,FRAMEBUFFER_TRANSFER_NONE); {No DMA due to unknown buffer}
      if Result <> ERROR_SUCCESS then Exit;
     end
    else
     begin
      {Get Format Bytes}
      FormatBytes:=ColorFormatToBytes(Format);

      {Get Console Bytes}
      ConsoleBytes:=ColorFormatToBytes(Console.Format);

      {Lock Framebuffer}
      if MutexLock(Framebuffer.Lock) <> ERROR_SUCCESS then Exit;

      {Check Cursor}
      if (Framebuffer.CursorState = FRAMEBUFFER_CURSOR_ENABLED) and ((Framebuffer.Device.DeviceFlags and FRAMEBUFFER_FLAG_CURSOR) = 0) then
       begin
        {Hide Cursor}
        if not Framebuffer.CursorUpdate then FramebufferDeviceHideCursor(Framebuffer);
       end;

      {Set Result}
      Result:=ERROR_SUCCESS;

      {Set Offset}
      Offset:=0;

      {Check Format}
      if Format = COLOR_FORMAT_DEFAULT then
       begin
        {Get Lines}
        for Count:=0 to Height - 1 do
         begin
          {Get Address}
          Address:=FramebufferDeviceGetPoint(Framebuffer,X,Y + Count);
          if Address <> nil then
           begin
            {Convert from Console.Format to Format (COLOR_FORMAT_DEFAULT)}
            PixelsFormatAltToDefault(Console.Format,Address,Pointer(Buffer + Offset),Width,Reverse);

            {Memory Barrier}
            DataMemoryBarrier;  {After the Last Read}
           end
          else
           begin
            Result:=ERROR_OPERATION_FAILED;
            Break;
           end;

          {Update Offset}
          Inc(Offset,(Width + Skip) * FormatBytes);
         end;
       end
      else if Console.Format = COLOR_FORMAT_DEFAULT then
       begin
        {Get Lines}
        for Count:=0 to Height - 1 do
         begin
          {Get Address}
          Address:=FramebufferDeviceGetPoint(Framebuffer,X,Y + Count);
          if Address <> nil then
           begin
            {Convert from Console.Format (COLOR_FORMAT_DEFAULT) to Format}
            PixelsDefaultAltToFormat(Format,Address,Pointer(Buffer + Offset),Width,Reverse);

            {Memory Barrier}
            DataMemoryBarrier;  {After the Last Read}
           end
          else
           begin
            Result:=ERROR_OPERATION_FAILED;
            Break;
           end;

          {Update Offset}
          Inc(Offset,(Width + Skip) * FormatBytes);
         end;
       end
      else
       begin
        {Allocate Line}
        Line:=GetMem(Width * ColorFormatToBytes(COLOR_FORMAT_DEFAULT));
        if Line <> nil then
         begin
          {Draw Lines}
          for Count:=0 to Height - 1 do
           begin
            {Get Address}
            Address:=FramebufferDeviceGetPoint(Framebuffer,X,Y + Count);
            if Address <> nil then
             begin
              {Convert from Console.Format to Default (COLOR_FORMAT_DEFAULT)}
              PixelsFormatAltToDefault(Console.Format,Address,Line,Width,Reverse);

              {Convert from Default (COLOR_FORMAT_DEFAULT) to Format}
              PixelsDefaultToFormat(Format,Line,Pointer(Buffer + Offset),Width,False);

              {Memory Barrier}
              DataMemoryBarrier;  {After the Last Read}
             end
            else
             begin
              Result:=ERROR_OPERATION_FAILED;
              Break;
             end;

            {Update Offset}
            Inc(Offset,(Width + Skip) * FormatBytes);
           end;

          {Free Line}
          FreeMem(Line);
         end;
       end;

      {Check Cursor}
      if (Framebuffer.CursorState = FRAMEBUFFER_CURSOR_ENABLED) and ((Framebuffer.Device.DeviceFlags and FRAMEBUFFER_FLAG_CURSOR) = 0) then
       begin
        {Show Cursor}
        if not Framebuffer.CursorUpdate then FramebufferDeviceShowCursor(Framebuffer);
       end;

      {Unlock Framebuffer}
      MutexUnlock(Framebuffer.Lock);
      if Result <> ERROR_SUCCESS then Exit;
     end;

    {Update Statistics}
    Inc(Console.GetCount);
   finally
    MutexUnlock(Console.Lock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function FramebufferConsolePutText(Console:PConsoleDevice;Handle:TFontHandle;const Source,Dest:TConsolePoint;Buffer:PConsoleChar;Width,Height,Skip:LongWord):LongWord;
{Implementation of ConsoleDevicePutText API for FramebufferConsole}
{Note: Not intended to be called directly by applications, use ConsoleDevicePutText instead}
var
 Row:LongWord;
 Line:LongWord;
 Count:LongWord;
 Column:LongWord;
 Offset:LongWord;
 CurrentY:LongWord;
 Character:LongWord;
 LineBuffer:Pointer;
 TextBuffer:PConsoleChar;

 Flags:LongWord;
 Bytes:LongWord;
 Reverse:Boolean;
 Font:PFontEntry;
 Framebuffer:PFramebufferDevice;
 Properties:TFramebufferProperties;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {Check Buffer}
 if Buffer = nil then Exit;

 {Check Width and Height}
 if (Width = 0) or (Height = 0) then Exit;

 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'FramebufferConsole: Console Put Text (Width=' + IntToStr(Width) + ' Height=' + IntToStr(Height) + ' Skip=' + IntToStr(Skip) + ')');
 {$ENDIF}

 if MutexLock(Console.Lock) = ERROR_SUCCESS then
  begin
   try
    {Get Font}
    Font:=PFontEntry(Handle);
    if Font = nil then Exit;
    if Font.Signature <> FONT_SIGNATURE then Exit;

    {Check Source}
    if Source.X < 1 then Exit;
    if Source.Y < 1 then Exit;

    {Check Dest}
    if Dest.X >= Console.Width then Exit;
    if Dest.Y >= Console.Height then Exit;
    if Dest.X + (Font.CharWidth * Width) > Console.Width then Exit;
    if Dest.Y + (Font.CharHeight * Height) > Console.Height then Exit;

    {Get Framebuffer}
    Framebuffer:=PFramebufferConsole(Console).Framebuffer;

    {Check Framebuffer}
    if Framebuffer = nil then Exit;

    {Get Flags}
    Flags:=FRAMEBUFFER_TRANSFER_NONE;
    if (Console.Device.DeviceFlags and CONSOLE_FLAG_DMA_TEXT) <> 0 then
     begin
      Flags:=FRAMEBUFFER_TRANSFER_DMA;
     end;

    {Get Bytes}
    Bytes:=ColorFormatToBytes(Console.Format);
    if Bytes = 0 then Exit;

    {Get Reverse}
    Reverse:=(Console.Device.DeviceFlags and CONSOLE_FLAG_COLOR_REVERSE) <> 0;

    {Check Line Buffer}
    if PFramebufferConsole(Console).LineBuffer = nil then
     begin
      {Get Properties}
      if FramebufferDeviceGetProperties(Framebuffer,@Properties) <> ERROR_SUCCESS then Exit;

      {Check DMA}
      if ((Console.Device.DeviceFlags and CONSOLE_FLAG_DMA_MASK) <> 0) and (DMAAvailable) then
       begin
        PFramebufferConsole(Console).LineBuffer:=DMAAllocateBuffer(Properties.Pitch);
       end
      else
       begin
        PFramebufferConsole(Console).LineBuffer:=GetMem(Properties.Pitch);
       end;
     end;
    {Get Line Buffer}
    LineBuffer:=PFramebufferConsole(Console).LineBuffer;
    if LineBuffer = nil then Exit;

    {Lock Framebuffer}
    if MutexLock(Framebuffer.Lock) <> ERROR_SUCCESS then Exit;

    {Check Cursor}
    if (Framebuffer.CursorState = FRAMEBUFFER_CURSOR_ENABLED) and ((Framebuffer.Device.DeviceFlags and FRAMEBUFFER_FLAG_CURSOR) = 0) then
     begin
      {Hide Cursor}
      FramebufferDeviceHideCursor(Framebuffer);

      {Set Update}
      Framebuffer.CursorUpdate:=True;
     end;

    {Set Result}
    Result:=ERROR_SUCCESS;

    {Get Text Buffer}
    Offset:=((Width + Skip) * (Source.Y - 1)) + (Source.X - 1);
    TextBuffer:=PConsoleChar(PtrUInt(Buffer) + (Offset  * SizeOf(TConsoleChar)));

    {Get Lines}
    CurrentY:=Dest.Y;
    for Line:=1 to Height do
     begin
      {Get Rows}
      for Row:=0 to Font.CharHeight - 1 do
       begin
        {Get Text}
        Offset:=0;
        for Count:=1 to Width do
         begin
          {Get Character}
          Character:=0;
          case Font.CharWidth of
           8:Character:=PFontChars8(Font.CharData)[(Byte(TextBuffer.Ch) * Font.CharHeight) + Row];
           9..16:Character:=PFontChars16(Font.CharData)[(Byte(TextBuffer.Ch) * Font.CharHeight) + Row];
           17..32:Character:=PFontChars32(Font.CharData)[(Byte(TextBuffer.Ch) * Font.CharHeight) + Row];
          end;

          {Map Character}
          Inc(Offset,Font.CharWidth * Bytes);
          for Column:=Font.CharWidth - 1 downto 0 do
           begin
            Dec(Offset,Bytes);
            if (Character and $01) = $01 then
             begin
              ColorDefaultToFormat(Console.Format,TextBuffer.Forecolor,Pointer(LineBuffer + Offset),Reverse);
             end
            else
             begin
              ColorDefaultToFormat(Console.Format,TextBuffer.Backcolor,Pointer(LineBuffer + Offset),Reverse);
             end;

            Character:=Character shr 1;
           end;

          Inc(Offset,Font.CharWidth * Bytes);

          Inc(TextBuffer);
         end;

        {Framebuffer Write}
        Result:=FramebufferDeviceWrite(Framebuffer,Dest.X,CurrentY,LineBuffer,Font.CharWidth * Width,Flags);
        if Result <> ERROR_SUCCESS then Break;

        Dec(TextBuffer,Width);

        Inc(CurrentY);
       end;

      {Update Text Buffer}
      Inc(TextBuffer,Width + Skip);
     end;

    {Check Cursor}
    if (Framebuffer.CursorState = FRAMEBUFFER_CURSOR_ENABLED) and ((Framebuffer.Device.DeviceFlags and FRAMEBUFFER_FLAG_CURSOR) = 0) then
     begin
      {Clear Update}
      Framebuffer.CursorUpdate:=False;

      {Show Cursor}
      FramebufferDeviceShowCursor(Framebuffer);
     end;

    {Unlock Framebuffer}
    MutexUnlock(Framebuffer.Lock);
    if Result <> ERROR_SUCCESS then Exit;

    {Update Statistics}
    Inc(Console.PutCount);
   finally
    MutexUnlock(Console.Lock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function FramebufferConsoleCopyImage(Console:PConsoleDevice;const Source,Dest:TConsolePoint;Width,Height:LongWord):LongWord;
{Implementation of ConsoleDeviceCopyImage API for FramebufferConsole}
{Note: Not intended to be called directly by applications, use ConsoleDeviceCopyImage instead}
var
 Framebuffer:PFramebufferDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Width and Height}
 if (Width = 0) or (Height = 0) then Exit;

 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'FramebufferConsole: Console Copy Image');
 {$ENDIF}

 if MutexLock(Console.Lock) = ERROR_SUCCESS then
  begin
   try
    {Check Source}
    if Source.X >= Console.Width then Exit;
    if Source.Y >= Console.Height then Exit;
    if Source.X + Width > Console.Width then Exit;
    if Source.Y + Height > Console.Height then Exit;

    {Check Dest}
    if Dest.X >= Console.Width then Exit;
    if Dest.Y >= Console.Height then Exit;
    if Dest.X + Width > Console.Width then Exit;
    if Dest.Y + Height > Console.Height then Exit;

    {Get Framebuffer}
    Framebuffer:=PFramebufferConsole(Console).Framebuffer;

    {Framebuffer Copy}
    Result:=FramebufferDeviceCopyRect(Framebuffer,Source.X,Source.Y,Dest.X,Dest.Y,Width,Height,FRAMEBUFFER_TRANSFER_DMA);
    if Result <> ERROR_SUCCESS then Exit;

    {Update Statistics}
    Inc(Console.CopyCount);

    {Get Result}
    Result:=ERROR_SUCCESS;
   finally
    MutexUnlock(Console.Lock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

procedure FramebufferConsoleCaretTimer(Caret:PConsoleCaret);
{Internal function used by FramebufferConsole device}
{Note: Not intended to be called directly by applications}
var
 Console:PConsoleDevice;
 Framebuffer:PFramebufferDevice;
begin
 {}
 {Check Caret}
 if Caret = nil then Exit;
 if Caret.Signature <> CARET_SIGNATURE then Exit;

 {Get Console}
 Console:=Caret.Console;
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'FramebufferConsole: Caret Timer (X=' + IntToStr(Caret.X) + ' Y=' + IntToStr(Caret.Y) + ')');
 {$ENDIF}

 if MutexLock(Console.Lock) = ERROR_SUCCESS then
  begin
   try
    {Check Caret}
    if Caret.Signature <> CARET_SIGNATURE then Exit;

    {Check Visible}
    if Caret.Visible then
     begin
      {Get Framebuffer}
      Framebuffer:=PFramebufferConsole(Console).Framebuffer;

      {Check Active}
      if Caret.Active then
       begin
        {Clear Caret}
        FramebufferDevicePutRect(Framebuffer,Caret.X,Caret.Y,Caret.Buffer,Caret.Width,Caret.Height,0,FRAMEBUFFER_TRANSFER_NONE);

        {Clear Active}
        Caret.Active:=False;
       end
      else
       begin
        {Display Caret}
        FramebufferDevicePutRect(Framebuffer,Caret.X,Caret.Y,Caret.Output,Caret.Width,Caret.Height,0,FRAMEBUFFER_TRANSFER_NONE);

        {Set Active}
        Caret.Active:=True;
       end;

      {Enable Timer}
      if Caret.Blink then TimerEnableEx(Caret.Handle,CONSOLE_CURSOR_BLINK_RATE,TTimerEvent(FramebufferConsoleCaretTimer),Caret);
     end;
   finally
    MutexUnlock(Console.Lock);
   end;
  end;
end;

{==============================================================================}

procedure FramebufferConsoleShowCaret(Console:PConsoleDevice;Caret:PConsoleCaret);
{Internal function used by FramebufferConsole device}
{Note: Not intended to be called directly by applications}
{Note: Caller must hold the console lock}

 function GetReverseColours(Console:PConsoleDevice;Caret:PConsoleCaret;var Color1,Color2:LongWord):Boolean;
 var
  Size:LongWord;
  First:Boolean;
  Value:LongWord;
  Offset:LongWord;
  Framebuffer:PFramebufferDevice;
 begin
  {}
  Result:=True;

  {Set Defaults}
  Color1:=0;
  Color2:=0;

  {Get Framebuffer}
  Framebuffer:=PFramebufferConsole(Console).Framebuffer;

  {Find Colors}
  Size:=Caret.Width * Caret.Height * ColorFormatToBytes(Console.Format);
  First:=True;
  Offset:=0;
  while Offset < Size do
   begin
    {Get Value}
    case Framebuffer.Depth of
     FRAMEBUFFER_DEPTH_8:Value:=PByte(Caret.Buffer + Offset)^;
     FRAMEBUFFER_DEPTH_16:Value:=PWord(Caret.Buffer + Offset)^;
     FRAMEBUFFER_DEPTH_24:Value:=PWord(Caret.Buffer + Offset)^ or (PByte(Caret.Buffer + Offset + 2)^ shl 16);
     FRAMEBUFFER_DEPTH_32:Value:=PLongWord(Caret.Buffer + Offset)^;
     else
      Break;
    end;

    {Check Colors}
    if First then
     begin
      Color1:=Value;
      First:=False;
     end
    else if Value <> Color1 then
     begin
      Color2:=Value;
      Exit;
     end;

    {Update Offset}
    case Framebuffer.Depth of
     FRAMEBUFFER_DEPTH_8:Inc(Offset,1);
     FRAMEBUFFER_DEPTH_16:Inc(Offset,2);
     FRAMEBUFFER_DEPTH_24:Inc(Offset,3);
     FRAMEBUFFER_DEPTH_32:Inc(Offset,4);
     else
      Break;
    end;
   end;

  Result:=False;
 end;

var
 Size:LongWord;
 Mask:LongWord;
 Value:LongWord;
 Offset:LongWord;
 Color:LongWord;
 Color1:LongWord;
 Color2:LongWord;
 Reverse:Boolean;
 Backcolor:Boolean;
 Framebuffer:PFramebufferDevice;
begin
 {}
 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Caret}
 if Caret = nil then Exit;
 if Caret.Signature <> CARET_SIGNATURE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'FramebufferConsole: Show Caret (X=' + IntToStr(Caret.X) + ' Y=' + IntToStr(Caret.Y) + ')');
 {$ENDIF}

 {Get Framebuffer}
 Framebuffer:=PFramebufferConsole(Console).Framebuffer;

 {Get Buffer}
 FramebufferDeviceGetRect(Framebuffer,Caret.X,Caret.Y,Caret.Buffer,Caret.Width,Caret.Height,0,FRAMEBUFFER_TRANSFER_NONE);

 {Get Mask}
 Mask:=ColorFormatToMask(Console.Format,(Console.Device.DeviceFlags and CONSOLE_FLAG_COLOR_REVERSE) <> 0);

 {Check Options}
 Reverse:=False;
 Backcolor:=False;
 if Caret.Reverse then
  begin
   {Get Colors}
   Reverse:=GetReverseColours(Console,Caret,Color1,Color2);
  end;
 if Caret.Backcolor <> COLOR_NONE then
  begin
   {Get Colour}
   Backcolor:=True;
   ColorDefaultToFormat(Console.Format,Caret.Backcolor,@Color,(Console.Device.DeviceFlags and CONSOLE_FLAG_COLOR_REVERSE) <> 0);
  end;

 {Create Output}
 Size:=Caret.Width * Caret.Height * ColorFormatToBytes(Console.Format);
 Offset:=0;
 while Offset < Size do
  begin
    {Get Value}
    case Framebuffer.Depth of
     FRAMEBUFFER_DEPTH_8:Value:=PByte(Caret.Buffer + Offset)^;
     FRAMEBUFFER_DEPTH_16:Value:=PWord(Caret.Buffer + Offset)^;
     FRAMEBUFFER_DEPTH_24:Value:=PWord(Caret.Buffer + Offset)^ or (PByte(Caret.Buffer + Offset + 2)^ shl 16);
     FRAMEBUFFER_DEPTH_32:Value:=PLongWord(Caret.Buffer + Offset)^;
     else
      Exit;
    end;

    {Check Mode}
    if Reverse then
     begin
      {Reverse}
      if Value = Color1 then Value:=Color2 else Value:=Color1;
     end
    else if Backcolor then
     begin
      {Backcolor}
      if Value <> Color then Value:=Color else Value:=Value xor Mask;
     end
    else
     begin
      {Inverse}
      Value:=Value xor Mask;
     end;

    {Set Value / Update Offset}
    case Framebuffer.Depth of
     FRAMEBUFFER_DEPTH_8:begin
       PByte(Caret.Output + Offset)^:=Value;
       Inc(Offset,1);
      end;
     FRAMEBUFFER_DEPTH_16:begin
       PWord(Caret.Output + Offset)^:=Value;
       Inc(Offset,2);
      end;
     FRAMEBUFFER_DEPTH_24:begin
       PWord(Caret.Output + Offset)^:=Value;
       PByte(Caret.Output + Offset + 2)^:=(Value shr 16);
       Inc(Offset,3);
      end;
     FRAMEBUFFER_DEPTH_32:begin
       PLongWord(Caret.Output + Offset)^:=Value;
       Inc(Offset,4);
      end;
     else
      Exit;
    end;
  end;

 {Set Active}
 Caret.Active:=True;

 {Show Caret}
 FramebufferDevicePutRect(Framebuffer,Caret.X,Caret.Y,Caret.Output,Caret.Width,Caret.Height,0,FRAMEBUFFER_TRANSFER_NONE);
end;

{==============================================================================}

procedure FramebufferConsoleHideCaret(Console:PConsoleDevice;Caret:PConsoleCaret);
{Internal function used by FramebufferConsole device}
{Note: Not intended to be called directly by applications}
{Note: Caller must hold the console lock}
var
 Framebuffer:PFramebufferDevice;
begin
 {}
 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Caret}
 if Caret = nil then Exit;
 if Caret.Signature <> CARET_SIGNATURE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'FramebufferConsole: Hide Caret (X=' + IntToStr(Caret.X) + ' Y=' + IntToStr(Caret.Y) + ')');
 {$ENDIF}

 {Get Framebuffer}
 Framebuffer:=PFramebufferConsole(Console).Framebuffer;

 {Clear Active}
 Caret.Active:=False;

 {Hide Caret}
 FramebufferDevicePutRect(Framebuffer,Caret.X,Caret.Y,Caret.Buffer,Caret.Width,Caret.Height,0,FRAMEBUFFER_TRANSFER_NONE);
end;

{==============================================================================}

function FramebufferConsoleAddCaret(Console:PConsoleDevice;Width,Height,OffsetX,OffsetY:LongWord):THandle;
{Implementation of ConsoleDeviceAddCaret API for FramebufferConsole}
{Note: Not intended to be called directly by applications, use ConsoleDeviceAddCaret instead}
var
 Size:LongWord;
 Caret:PConsoleCaret;
begin
 {}
 Result:=INVALID_HANDLE_VALUE;

 {Check Width and Height}
 if (Width = 0) or (Height = 0) then Exit;

 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'FramebufferConsole: Console Add Caret (Width=' + IntToStr(Width) + ' Height=' + IntToStr(Height) + ')');
 {$ENDIF}

 if MutexLock(Console.Lock) = ERROR_SUCCESS then
  begin
   try
    {Create Caret}
    Caret:=PConsoleCaret(AllocMem(SizeOf(TConsoleCaret)));
    if Caret = nil then Exit;

    {Update Caret}
    Caret.Signature:=CARET_SIGNATURE;
    Caret.X:=0;
    Caret.Y:=0;
    Caret.Width:=Width;
    Caret.Height:=Height;
    Caret.OffsetX:=OffsetX;
    Caret.OffsetY:=OffsetY;
    Caret.Visible:=False;
    Caret.Blink:=False;
    Caret.Reverse:=False;
    Caret.Forecolor:=COLOR_NONE;
    Caret.Backcolor:=COLOR_NONE;
    Caret.Console:=Console;
    Caret.Handle:=INVALID_HANDLE_VALUE;
    Caret.Active:=False;
    Caret.Image:=nil;

    {Calculate size}
    Size:=(Width * Height * ColorFormatToBytes(Console.Format));

    {Allocate Buffer}
    Caret.Buffer:=GetMem(Size);
    if Caret.Buffer = nil then
     begin
      {Free Caret}
      FreeMem(Caret);
     end;
    FillChar(Caret.Buffer^,Size,1);

    {Allocate Output}
    Caret.Output:=GetMem(Size);
    if Caret.Output = nil then
     begin
      {Free Buffer}
      FreeMem(Caret.Buffer);

      {Free Caret}
      FreeMem(Caret);
     end;

    {Create Timer}
    Caret.Handle:=TimerCreateEx(CONSOLE_CURSOR_BLINK_RATE,TIMER_STATE_DISABLED,TIMER_FLAG_WORKER,TTimerEvent(FramebufferConsoleCaretTimer),Caret);
    if Caret.Handle = INVALID_HANDLE_VALUE then
     begin
      {Delete Timer}
      TimerDestroy(Caret.Handle);

      {Free Output}
      FreeMem(Caret.Output);

      {Free Buffer}
      FreeMem(Caret.Buffer);

      {Free Caret}
      FreeMem(Caret);
     end;

    {Insert Caret}
    if CriticalSectionLock(Console.CaretLock) = ERROR_SUCCESS then
     begin
      try
       {Link Caret}
       if Console.CaretFirst = nil then
        begin
         Console.CaretFirst:=Caret;
        end
       else
        begin
         Caret.Next:=Console.CaretFirst;
         Console.CaretFirst.Prev:=Caret;
         Console.CaretFirst:=Caret;
        end;

       {Increment Count}
       Inc(Console.CaretCount);

       {Return Result}
       Result:=THandle(Caret);
      finally
       CriticalSectionUnlock(Console.CaretLock);
      end;
     end
    else
     begin
      {Free Output}
      FreeMem(Caret.Output);

      {Free Buffer}
      FreeMem(Caret.Buffer);

      {Free Caret}
      FreeMem(Caret);
     end;
   finally
    MutexUnlock(Console.Lock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function FramebufferConsoleDeleteCaret(Console:PConsoleDevice;Handle:THandle):LongWord;
{Implementation of ConsoleDeviceDeleteCaret API for FramebufferConsole}
{Note: Not intended to be called directly by applications, use ConsoleDeviceDeleteCaret instead}
var
 Prev:PConsoleCaret;
 Next:PConsoleCaret;
 Caret:PConsoleCaret;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'FramebufferConsole: Console Delete Caret');
 {$ENDIF}

 {Get Caret}
 Caret:=PConsoleCaret(Handle);
 if Caret = nil then Exit;

 if MutexLock(Console.Lock) = ERROR_SUCCESS then
  begin
   try
    {Check Caret}
    if Caret.Signature <> CARET_SIGNATURE then Exit;

    {Check Visible}
    if Caret.Visible then
     begin
      {Hide Caret}
      FramebufferConsoleHideCaret(Console,Caret);
     end;

    {Remove Caret}
    if CriticalSectionLock(Console.CaretLock) = ERROR_SUCCESS then
     begin
      try
       {Check Caret}
       Result:=ERROR_NOT_FOUND;
       if ConsoleDeviceCaretCheck(Console,Caret) <> Caret then Exit;

       {Delete Timer}
       if Caret.Handle <> INVALID_HANDLE_VALUE then
        begin
         TimerDestroy(Caret.Handle);
         Caret.Handle:=INVALID_HANDLE_VALUE;
        end;

       {Unlink Caret}
       Prev:=Caret.Prev;
       Next:=Caret.Next;
       if Prev = nil then
        begin
         Console.CaretFirst:=Next;
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
       Dec(Console.CaretCount);

       {Update Caret}
       Caret.Signature:=0;
       Caret.Console:=nil;

       {Free Caret}
       FreeMem(Caret);

       {Return Result}
       Result:=ERROR_SUCCESS;
      finally
       CriticalSectionUnlock(Console.CaretLock);
      end;
     end
    else
     begin
      Result:=ERROR_CAN_NOT_COMPLETE;
     end;
   finally
    MutexUnlock(Console.Lock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function FramebufferConsoleUpdateCaretEx(Console:PConsoleDevice;Handle:THandle;X,Y,Forecolor,Backcolor:LongWord;Visible,Blink,Reverse:Boolean):LongWord;
{Implementation of ConsoleDeviceUpdateCaretEx API for FramebufferConsole}
{Note: Not intended to be called directly by applications, use ConsoleDeviceUpdateCaretEx instead}
var
 Change:Boolean;
 Caret:PConsoleCaret;
 BlinkChange:Boolean;
 VisibleChange:Boolean;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'FramebufferConsole: Console Update Caret (Handle=' + IntToHex(Handle,8) + ' X=' + IntToStr(X) + ' Y=' + IntToStr(Y) + ' Forecolor=' + IntToHex(Forecolor,8) + ' Backcolor=' + IntToHex(Backcolor,8) + ' Visible=' + BoolToStr(Visible,True) + ' Blink=' + BoolToStr(Blink,True) + ' Reverse=' + BoolToStr(Reverse,True) + ')');
 {$ENDIF}

 {Get Caret}
 Caret:=PConsoleCaret(Handle);
 if Caret = nil then Exit;

 if MutexLock(Console.Lock) = ERROR_SUCCESS then
  begin
   try
    {Check Caret}
    if Caret.Signature <> CARET_SIGNATURE then Exit;

    {Check X, Y}
    if X >= Console.Width then Exit;
    if Y >= Console.Height then Exit;
    if X + Caret.Width > Console.Width then Exit;
    if Y + Caret.Height > Console.Height then Exit;

    {Check Visible}
    VisibleChange:=Visible <> Caret.Visible;
    Change:=VisibleChange;

    {Check X}
    if not Change then Change:=X <> Caret.X;

    {Check Y}
    if not Change then Change:=Y <> Caret.Y;

    {Check Forecolor}
    if not Change then Change:=Forecolor <> Caret.Forecolor;

    {Check Backcolor}
    if not Change then Change:=Backcolor <> Caret.Backcolor;

    {Check Blink}
    BlinkChange:=Blink <> Caret.Blink;
    if not Change then Change:=BlinkChange;

    {Check Reverse}
    if not Change then Change:=Reverse <> Caret.Reverse;

    {Check Change}
    if Change then
     begin
      {Check Visible}
      if Caret.Visible then
       begin
        {Hide Caret}
        FramebufferConsoleHideCaret(Console,Caret);
       end;

      {Update Properties}
      Caret.Visible:=Visible;
      Caret.X:=X + Caret.OffsetX;
      Caret.Y:=Y + Caret.OffsetY;
      Caret.Forecolor:=Forecolor;
      Caret.Backcolor:=Backcolor;
      Caret.Blink:=Blink;
      Caret.Reverse:=Reverse;

      {Check Visible}
      if Visible then
       begin
        {Show Caret}
        FramebufferConsoleShowCaret(Console,Caret);
       end;

      {Check Visible or Blink Change}
      if VisibleChange or BlinkChange then
       begin
        {Check Blink}
        if Caret.Visible and Caret.Blink then
         begin
          {Enable Timer}
          TimerEnableEx(Caret.Handle,CONSOLE_CURSOR_BLINK_RATE,TTimerEvent(FramebufferConsoleCaretTimer),Caret);
         end
        else
         begin
          {Disable Timer}
          TimerDisable(Caret.Handle);
         end;
       end;
     end;

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    MutexUnlock(Console.Lock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function FramebufferConsoleGetPosition(Console:PConsoleDevice;Position:LongWord;var X1,Y1,X2,Y2:LongWord):LongWord;
{Implementation of ConsoleDeviceGetPosition API for FramebufferConsole}
{Note: Not intended to be called directly by applications, use ConsoleDeviceGetPosition instead}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Position}
 {if Position < CONSOLE_POSITION_FULL then Exit;}
 if (Position <> CONSOLE_POSITION_FULLSCREEN) and (Position > CONSOLE_POSITION_BOTTOMRIGHT) then Exit;

 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'FramebufferConsole: Console Get Position');
 {$ENDIF}

 if MutexLock(Console.Lock) = ERROR_SUCCESS then
  begin
   try
    {Check Position}
    case Position of
     CONSOLE_POSITION_FULL:begin
       {Full Window}
       X1:=PFramebufferConsole(Console).DesktopX + Console.Borderwidth;
       Y1:=PFramebufferConsole(Console).DesktopY + Console.Borderwidth;
       X2:=(PFramebufferConsole(Console).DesktopX + (PFramebufferConsole(Console).DesktopWidth - 1)) - Console.Borderwidth;
       Y2:=(PFramebufferConsole(Console).DesktopY + (PFramebufferConsole(Console).DesktopHeight - 1)) - Console.Borderwidth;

       Result:=ERROR_SUCCESS;
      end;
     CONSOLE_POSITION_TOP:begin
       {Top Window}
       X1:=PFramebufferConsole(Console).DesktopX + Console.Borderwidth;
       Y1:=PFramebufferConsole(Console).DesktopY + Console.Borderwidth;
       X2:=(PFramebufferConsole(Console).DesktopX + (PFramebufferConsole(Console).DesktopWidth - 1)) - Console.Borderwidth;
       Y2:=(PFramebufferConsole(Console).DesktopY + ((PFramebufferConsole(Console).DesktopHeight div 2) - 1)) - Console.Borderwidth;

       Result:=ERROR_SUCCESS;
      end;
     CONSOLE_POSITION_BOTTOM:begin
       {Bottom Window}
       X1:=PFramebufferConsole(Console).DesktopX + Console.Borderwidth;
       Y1:=PFramebufferConsole(Console).DesktopY + ((PFramebufferConsole(Console).DesktopHeight div 2) - 1) + Console.Borderwidth;
       X2:=(PFramebufferConsole(Console).DesktopX + (PFramebufferConsole(Console).DesktopWidth - 1)) - Console.Borderwidth;
       Y2:=(PFramebufferConsole(Console).DesktopY + (PFramebufferConsole(Console).DesktopHeight - 1)) - Console.Borderwidth;

       Result:=ERROR_SUCCESS;
      end;
     CONSOLE_POSITION_LEFT:begin
       {Left Window}
       X1:=PFramebufferConsole(Console).DesktopX + Console.Borderwidth;
       Y1:=PFramebufferConsole(Console).DesktopY + Console.Borderwidth;
       X2:=(PFramebufferConsole(Console).DesktopX + ((PFramebufferConsole(Console).DesktopWidth div 2) - 1)) - Console.Borderwidth;
       Y2:=(PFramebufferConsole(Console).DesktopY + (PFramebufferConsole(Console).DesktopHeight - 1)) - Console.Borderwidth;

       Result:=ERROR_SUCCESS;
      end;
     CONSOLE_POSITION_RIGHT:begin
       {Right Window}
       X1:=PFramebufferConsole(Console).DesktopX + ((PFramebufferConsole(Console).DesktopWidth div 2) - 1) + Console.Borderwidth;
       Y1:=PFramebufferConsole(Console).DesktopY + Console.Borderwidth;
       X2:=(PFramebufferConsole(Console).DesktopX + (PFramebufferConsole(Console).DesktopWidth - 1)) - Console.Borderwidth;
       Y2:=(PFramebufferConsole(Console).DesktopY + (PFramebufferConsole(Console).DesktopHeight - 1)) - Console.Borderwidth;

       Result:=ERROR_SUCCESS;
      end;
     CONSOLE_POSITION_TOPLEFT:begin
       {Top Left Window}
       X1:=PFramebufferConsole(Console).DesktopX + Console.Borderwidth;
       Y1:=PFramebufferConsole(Console).DesktopY + Console.Borderwidth;
       X2:=(PFramebufferConsole(Console).DesktopX + ((PFramebufferConsole(Console).DesktopWidth div 2) - 1)) - Console.Borderwidth;
       Y2:=(PFramebufferConsole(Console).DesktopY + ((PFramebufferConsole(Console).DesktopHeight div 2) - 1)) - Console.Borderwidth;

       Result:=ERROR_SUCCESS;
      end;
     CONSOLE_POSITION_TOPRIGHT:begin
       {Top Right Window}
       X1:=PFramebufferConsole(Console).DesktopX + ((PFramebufferConsole(Console).DesktopWidth div 2) - 1) + Console.Borderwidth;
       Y1:=PFramebufferConsole(Console).DesktopY + Console.Borderwidth;
       X2:=(PFramebufferConsole(Console).DesktopX + (PFramebufferConsole(Console).DesktopWidth - 1)) - Console.Borderwidth;
       Y2:=(PFramebufferConsole(Console).DesktopY + ((PFramebufferConsole(Console).DesktopHeight div 2) - 1)) - Console.Borderwidth;

       Result:=ERROR_SUCCESS;
      end;
     CONSOLE_POSITION_BOTTOMLEFT:begin
       {Bottom Left Window}
       X1:=PFramebufferConsole(Console).DesktopX + Console.Borderwidth;
       Y1:=PFramebufferConsole(Console).DesktopY + ((PFramebufferConsole(Console).DesktopHeight div 2) - 1) + Console.Borderwidth;
       X2:=(PFramebufferConsole(Console).DesktopX + ((PFramebufferConsole(Console).DesktopWidth div 2) - 1)) - Console.Borderwidth;
       Y2:=(PFramebufferConsole(Console).DesktopY + (PFramebufferConsole(Console).DesktopHeight - 1)) - Console.Borderwidth;

       Result:=ERROR_SUCCESS;
      end;
     CONSOLE_POSITION_BOTTOMRIGHT:begin
       {Bottom Right Window}
       X1:=PFramebufferConsole(Console).DesktopX + ((PFramebufferConsole(Console).DesktopWidth div 2) - 1) + Console.Borderwidth;
       Y1:=PFramebufferConsole(Console).DesktopY + ((PFramebufferConsole(Console).DesktopHeight div 2) - 1) + Console.Borderwidth;
       X2:=(PFramebufferConsole(Console).DesktopX + (PFramebufferConsole(Console).DesktopWidth - 1)) - Console.Borderwidth;
       Y2:=(PFramebufferConsole(Console).DesktopY + (PFramebufferConsole(Console).DesktopHeight - 1)) - Console.Borderwidth;

       Result:=ERROR_SUCCESS;
      end;
     CONSOLE_POSITION_FULLSCREEN:begin
       {Fullscreen Window}
       X1:=0;
       Y1:=0;
       X2:=Console.Width - 1;
       Y2:=Console.Height - 1;

       Result:=ERROR_SUCCESS;
      end;
    end;
   finally
    MutexUnlock(Console.Lock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}
{==============================================================================}
{Console Helper Functions}
function ConsoleDeviceGetCount:LongWord;
{Get the current console device count}
begin
 {}
 Result:=ConsoleDeviceTableCount;
end;

{==============================================================================}

function ConsoleDeviceGetDefault:PConsoleDevice;
{Get the current default console device}
begin
 {}
 Result:=ConsoleDeviceDefault;
end;

{==============================================================================}

function ConsoleDeviceSetDefault(Console:PConsoleDevice):LongWord;
{Set the current default console device}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(ConsoleDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Check Console}
    if ConsoleDeviceCheck(Console) <> Console then Exit;

    {Set Console Default}
    ConsoleDeviceDefault:=Console;

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    CriticalSectionUnlock(ConsoleDeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function ConsoleDeviceCheck(Console:PConsoleDevice):PConsoleDevice;
{Check if the supplied Console device is in the Console table}
var
 Current:PConsoleDevice;
begin
 {}
 Result:=nil;

 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(ConsoleDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Console}
    Current:=ConsoleDeviceTable;
    while Current <> nil do
     begin
      {Check Console}
      if Current = Console then
       begin
        Result:=Console;
        Exit;
       end;

      {Get Next}
      Current:=Current.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(ConsoleDeviceTableLock);
   end;
  end;
end;

{==============================================================================}

function ConsoleDeviceCaretCheck(Console:PConsoleDevice;Caret:PConsoleCaret):PConsoleCaret;
{Check if a console caret entry is valid}
{Console: The console device to search for the caret}
{Caret: The caret entry to check for validity}
{Return: The supplied caret if successful or nil on failure}
var
 Current:PConsoleCaret;
begin
 {}
 Result:=nil;

 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Caret}
 if Caret = nil then Exit;
 if Caret.Signature <> CARET_SIGNATURE then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(Console.CaretLock) = ERROR_SUCCESS then
  begin
   try
    {Get Caret}
    Current:=Console.CaretFirst;
    while Current <> nil do
     begin
      {Check Caret}
      if Current = Caret then
       begin
        Result:=Caret;
        Exit;
       end;

      {Get Next}
      Current:=Current.Next;
     end;
   finally
    CriticalSectionUnlock(Console.CaretLock);
   end;
  end;
end;

{==============================================================================}

function ConsoleTypeToString(ConsoleType:LongWord):String;
{Convert a Console type value to a string}
begin
 {}
 Result:='CONSOLE_TYPE_UNKNOWN';

 if ConsoleType <= CONSOLE_TYPE_MAX then
  begin
   Result:=CONSOLE_TYPE_NAMES[ConsoleType];
  end;
end;

{==============================================================================}

function ConsoleStateToString(ConsoleState:LongWord):String;
{Convert a Console state value to a string}
begin
 {}
 Result:='CONSOLE_STATE_UNKNOWN';

 if ConsoleState <= CONSOLE_STATE_MAX then
  begin
   Result:=CONSOLE_STATE_NAMES[ConsoleState];
  end;
end;

{==============================================================================}

function ConsoleDeviceGetDefaultFont:TFontHandle;
{Get the default console font}
begin
 {}
 if (CONSOLE_DEFAULT_FONT = INVALID_HANDLE_VALUE) and (Length(CONSOLE_DEFAULT_FONT_NAME) <> 0 ) then
  begin
   CONSOLE_DEFAULT_FONT:=FontFindByName(CONSOLE_DEFAULT_FONT_NAME);
   if CONSOLE_DEFAULT_FONT = INVALID_HANDLE_VALUE then
    begin
     CONSOLE_DEFAULT_FONT:=FontFindByDescription(CONSOLE_DEFAULT_FONT_NAME);
    end;
  end;

 Result:=CONSOLE_DEFAULT_FONT;
end;

{==============================================================================}

function ConsolePositionToString(Position:LongWord):String;
begin
 {}
 Result:='CONSOLE_POSITION_UNKNOWN';

 case Position of
  CONSOLE_POSITION_FULL:Result:='CONSOLE_POSITION_FULL';
  CONSOLE_POSITION_TOP:Result:='CONSOLE_POSITION_TOP';
  CONSOLE_POSITION_BOTTOM:Result:='CONSOLE_POSITION_BOTTOM';
  CONSOLE_POSITION_LEFT:Result:='CONSOLE_POSITION_LEFT';
  CONSOLE_POSITION_RIGHT:Result:='CONSOLE_POSITION_RIGHT';
  CONSOLE_POSITION_TOPLEFT:Result:='CONSOLE_POSITION_TOPLEFT';
  CONSOLE_POSITION_TOPRIGHT:Result:='CONSOLE_POSITION_TOPRIGHT';
  CONSOLE_POSITION_BOTTOMLEFT:Result:='CONSOLE_POSITION_BOTTOMLEFT';
  CONSOLE_POSITION_BOTTOMRIGHT:Result:='CONSOLE_POSITION_BOTTOMRIGHT';
  CONSOLE_POSITION_FULLSCREEN:Result:='CONSOLE_POSITION_FULLSCREEN';
 end;
end;

{==============================================================================}

function ConsoleFramebufferDeviceAdd(Framebuffer:PFramebufferDevice):LongWord;
var
 Status:LongWord;
 Console:PFramebufferConsole;
begin
 {}
 Result:=ERROR_SUCCESS;

 {Check Console}
 if ConsoleDeviceFindByDevice(@Framebuffer.Device) = nil then
  begin
   {Create Console}
   Console:=PFramebufferConsole(ConsoleDeviceCreateEx(SizeOf(TFramebufferConsole)));
   if Console <> nil then
    begin
     {Update Console}
     {Device}
     Console.Console.Device.DeviceBus:=Framebuffer.Device.DeviceBus;
     Console.Console.Device.DeviceType:=CONSOLE_TYPE_FRAMEBUFFER;
     Console.Console.Device.DeviceFlags:=CONSOLE_FLAG_BLINK_CARET or CONSOLE_FLAG_COLOR or CONSOLE_FLAG_FONT or CONSOLE_FLAG_FULLSCREEN or CONSOLE_FLAG_TEXT_CARET;
     Console.Console.Device.DeviceData:=@Framebuffer.Device;
     Console.Console.Device.DeviceDescription:=FRAMEBUFFER_CONSOLE_DESCRIPTION + ' (' + DeviceGetName(@Framebuffer.Device) + ')';
     {Console}
     Console.Console.ConsoleState:=CONSOLE_STATE_CLOSED;
     Console.Console.ConsoleMode:=CONSOLE_MODE_PIXEL;
     Console.Console.DeviceOpen:=FramebufferConsoleOpen;
     Console.Console.DeviceClose:=FramebufferConsoleClose;
     Console.Console.DeviceClear:=FramebufferConsoleClear;
     Console.Console.DeviceScroll:=FramebufferConsoleScroll;
     Console.Console.DeviceDrawBox:=FramebufferConsoleDrawBox;
     Console.Console.DeviceDrawLine:=FramebufferConsoleDrawLine;
     Console.Console.DeviceDrawChar:=FramebufferConsoleDrawChar;
     Console.Console.DeviceDrawText:=FramebufferConsoleDrawText;
     Console.Console.DeviceDrawPixel:=FramebufferConsoleDrawPixel;
     Console.Console.DeviceDrawBlock:=FramebufferConsoleDrawBlock;
     Console.Console.DeviceDrawImage:=FramebufferConsoleDrawImage;
     Console.Console.DeviceDrawWindow:=FramebufferConsoleDrawWindow;
     Console.Console.DeviceGetPixel:=FramebufferConsoleGetPixel;
     Console.Console.DeviceGetImage:=FramebufferConsoleGetImage;
     Console.Console.DevicePutText:=FramebufferConsolePutText;
     Console.Console.DeviceCopyImage:=FramebufferConsoleCopyImage;
     Console.Console.DeviceAddCaret:=FramebufferConsoleAddCaret;
     Console.Console.DeviceDeleteCaret:=FramebufferConsoleDeleteCaret;
     Console.Console.DeviceUpdateCaretEx:=FramebufferConsoleUpdateCaretEx;
     Console.Console.DeviceGetPosition:=FramebufferConsoleGetPosition;
     Console.Console.FontRatio:=1; {Font ratio 1 for Pixel console}
     {Framebuffer}
     Console.Framebuffer:=Framebuffer;
     Console.DesktopOffset:=FRAMEBUFFER_CONSOLE_DEFAULT_DESKTOPOFFSET;
     Console.DesktopColor:=FRAMEBUFFER_CONSOLE_DEFAULT_DESKTOPCOLOR;

     {Setup Flags}
     if CONSOLE_LINE_WRAP then Console.Console.Device.DeviceFlags:=Console.Console.Device.DeviceFlags or CONSOLE_FLAG_LINE_WRAP;
     if CONSOLE_AUTO_SCROLL then Console.Console.Device.DeviceFlags:=Console.Console.Device.DeviceFlags or CONSOLE_FLAG_AUTO_SCROLL;
     if CONSOLE_FOCUS_CURSOR then Console.Console.Device.DeviceFlags:=Console.Console.Device.DeviceFlags or CONSOLE_FLAG_FOCUS_CARET;
     if CONSOLE_DMA_BOX then Console.Console.Device.DeviceFlags:=Console.Console.Device.DeviceFlags or CONSOLE_FLAG_DMA_BOX;
     if CONSOLE_DMA_TEXT then Console.Console.Device.DeviceFlags:=Console.Console.Device.DeviceFlags or CONSOLE_FLAG_DMA_TEXT;
     if CONSOLE_DMA_LINE then Console.Console.Device.DeviceFlags:=Console.Console.Device.DeviceFlags or CONSOLE_FLAG_DMA_LINE;
     if CONSOLE_DMA_FILL then Console.Console.Device.DeviceFlags:=Console.Console.Device.DeviceFlags or CONSOLE_FLAG_DMA_FILL;
     if CONSOLE_DMA_CLEAR then Console.Console.Device.DeviceFlags:=Console.Console.Device.DeviceFlags or CONSOLE_FLAG_DMA_CLEAR;
     if CONSOLE_DMA_SCROLL then Console.Console.Device.DeviceFlags:=Console.Console.Device.DeviceFlags or CONSOLE_FLAG_DMA_SCROLL;
     if (Framebuffer.Device.DeviceFlags and FRAMEBUFFER_FLAG_SWAP) <> 0 then Console.Console.Device.DeviceFlags:=Console.Console.Device.DeviceFlags or CONSOLE_FLAG_COLOR_REVERSE;

     {Register Console}
     Status:=ConsoleDeviceRegister(@Console.Console);
     if Status = ERROR_SUCCESS then
      begin
       {Open Console}
       Status:=ConsoleDeviceOpen(@Console.Console);
       if Status <> ERROR_SUCCESS then
        begin
         if DEVICE_LOG_ENABLED then DeviceLogError(nil,'Console: Failed to open new framebuffer console device: ' + ErrorToString(Status));

         {Deregister Console}
         ConsoleDeviceDeregister(@Console.Console);

         {Destroy Console}
         ConsoleDeviceDestroy(@Console.Console);
        end;
      end
     else
      begin
       if DEVICE_LOG_ENABLED then DeviceLogError(nil,'Console: Failed to register new framebuffer console device: ' + ErrorToString(Status));

       {Destroy Console}
       ConsoleDeviceDestroy(@Console.Console);
      end;
    end
   else
    begin
     if DEVICE_LOG_ENABLED then DeviceLogError(nil,'Console: Failed to create new framebuffer console device');
    end;
  end;
end;

{==============================================================================}

function ConsoleFramebufferDeviceRemove(Framebuffer:PFramebufferDevice):LongWord;
var
 Status:LongWord;
 Console:PFramebufferConsole;
begin
 {}
 Result:=ERROR_SUCCESS;

 {Check Console}
 Console:=PFramebufferConsole(ConsoleDeviceFindByDevice(@Framebuffer.Device));
 if Console <> nil then
  begin
   {Close Console}
   Status:=ConsoleDeviceClose(@Console.Console);
   if Status = ERROR_SUCCESS then
    begin
     {Deregister Console}
     Status:=ConsoleDeviceDeregister(@Console.Console);
     if Status = ERROR_SUCCESS then
      begin
       {Destroy Console}
       Status:=ConsoleDeviceDestroy(@Console.Console);
       if Status <> ERROR_SUCCESS then
        begin
         if DEVICE_LOG_ENABLED then DeviceLogError(nil,'Console: Failed to destroy framebuffer console device');
        end;
      end
     else
      begin
       if DEVICE_LOG_ENABLED then DeviceLogError(nil,'Console: Failed to deregister framebuffer console device: ' + ErrorToString(Status));
      end;
    end
   else
    begin
     if DEVICE_LOG_ENABLED then DeviceLogError(nil,'Console: Failed to close framebuffer console device: ' + ErrorToString(Status));
    end;
  end;
end;

{==============================================================================}

function ConsoleFramebufferDeviceEnum(Framebuffer:PFramebufferDevice;Data:Pointer):LongWord;
begin
 {}
 Result:=ERROR_SUCCESS;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console: Framebuffer device enumeration');
 {$ENDIF}

 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.FramebufferState <> FRAMEBUFFER_STATE_ENABLED then Exit;

 {Check Autocreate}
 if FRAMEBUFFER_CONSOLE_AUTOCREATE then
  begin
   {Add Framebuffer}
   Result:=ConsoleFramebufferDeviceAdd(Framebuffer);
  end;
end;

{==============================================================================}

function ConsoleFramebufferDeviceNotify(Device:PDevice;Data:Pointer;Notification:LongWord):LongWord;
var
 Framebuffer:PFramebufferDevice;
begin
 {}
 Result:=ERROR_SUCCESS;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console: Framebuffer device notification (Notification=' + NotificationToString(Notification) + ')');
 {$ENDIF}

 {Check Device}
 if Device = nil then Exit;

 {Get Framebuffer}
 Framebuffer:=PFramebufferDevice(Device);

 {Check Notification}
 if (Notification and DEVICE_NOTIFICATION_REGISTER) <> 0 then
  begin
   {Check Framebuffer}
   if Framebuffer.FramebufferState <> FRAMEBUFFER_STATE_ENABLED then Exit;

   {Check Autocreate}
   if FRAMEBUFFER_CONSOLE_AUTOCREATE then
    begin
     {Add Framebuffer}
     Result:=ConsoleFramebufferDeviceAdd(Framebuffer);
    end;
  end
 else if (Notification and DEVICE_NOTIFICATION_ENABLE) <> 0 then
  begin
   {Check Framebuffer}
   if Framebuffer.FramebufferState <> FRAMEBUFFER_STATE_ENABLED then Exit;

   {Check Autocreate}
   if FRAMEBUFFER_CONSOLE_AUTOCREATE then
    begin
     {Add Framebuffer}
     Result:=ConsoleFramebufferDeviceAdd(Framebuffer);
    end;
  end
 else if (Notification and DEVICE_NOTIFICATION_DEREGISTER) <> 0 then
  begin
   {Remove Framebuffer}
   Result:=ConsoleFramebufferDeviceRemove(Framebuffer);
  end
 else if (Notification and DEVICE_NOTIFICATION_DISABLE) <> 0 then
  begin
   {Remove Framebuffer}
   Result:=ConsoleFramebufferDeviceRemove(Framebuffer);
  end;
end;

{==============================================================================}
{==============================================================================}
{Text Console Helper Functions}
function ConsoleWindowGetCount(Console:PConsoleDevice):LongWord; inline;
{Get the current console window count}
{Console: The console device to get the window count for}
{Return: The current number of console windows on the specified console device}
begin
 {}
 Result:=0;

 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Get Window Count}
 Result:=Console.WindowCount;
end;

{==============================================================================}

function ConsoleWindowGetActive(Console:PConsoleDevice):TWindowHandle; inline;
{Get the current console active window}
{Console: The console device to get the active window for}
{Return: The window handle of the current active window or INVALID_HANDLE_VALUE on failure}
begin
 {}
 Result:=INVALID_HANDLE_VALUE;

 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Window Active}
 if Console.WindowActive = nil then Exit;

 {Get Window Active}
 Result:=TWindowHandle(Console.WindowActive);
end;

{==============================================================================}

function ConsoleWindowGetDefault(Console:PConsoleDevice):TWindowHandle; inline;
{Get the current console default window}
{Console: The console device to get the default window for}
{Return: The window handle of the current default window or INVALID_HANDLE_VALUE on failure}
begin
 {}
 Result:=INVALID_HANDLE_VALUE;

 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Get Window Default}
 Result:=Console.WindowDefault;
end;

{==============================================================================}

function ConsoleWindowSetDefault(Console:PConsoleDevice;Handle:TWindowHandle):LongWord;
{Set the current console default window}
{Console: The console device to set the default window for}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Window:PConsoleWindow;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_TEXT then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(Console.WindowLock) = ERROR_SUCCESS then
  begin
   try
    {Check Window}
    if ConsoleWindowCheck(Console,Window) <> Window then Exit;

    {Set Window Default}
    Console.WindowDefault:=Handle;

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    CriticalSectionUnlock(Console.WindowLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function ConsoleWindowCheck(Console:PConsoleDevice;Window:PConsoleWindow):PConsoleWindow;
{Check if a console window entry is valid}
{Console: The console device to search for the window}
{Window: The window entry to check for validity}
{Return: The supplied window if successful or nil on failure}
var
 Current:PConsoleWindow;
begin
 {}
 Result:=nil;

 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Window}
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(Console.WindowLock) = ERROR_SUCCESS then
  begin
   try
    {Get Window}
    Current:=Console.WindowFirst;
    while Current <> nil do
     begin
      {Check Window}
      if Current = Window then
       begin
        Result:=Window;
        Exit;
       end;

      {Get Next}
      Current:=Current.Next;
     end;
   finally
    CriticalSectionUnlock(Console.WindowLock);
   end;
  end;
end;

{==============================================================================}

function ConsoleWindowStateToString(WindowState:LongWord):String;
{Convert a Console Window state value to a string}
begin
 {}
 Result:='WINDOW_STATE_UNKNOWN';

 if WindowState <= WINDOW_STATE_MAX then
  begin
   Result:=WINDOW_STATE_NAMES[WindowState];
  end;
end;

{==============================================================================}

function ConsoleWindowModeToString(WindowMode:LongWord):String;
{Convert a Console Window mode value to a string}
begin
 {}
 Result:='WINDOW_MODE_UNKNOWN';

 if WindowMode <= WINDOW_MODE_MAX then
  begin
   Result:=WINDOW_MODE_NAMES[WindowMode];
  end;
end;

{==============================================================================}

function ConsoleWindowGetDefaultFont:TFontHandle;
{Get the default console window font}
begin
 {}
 if (WINDOW_DEFAULT_FONT = INVALID_HANDLE_VALUE) and (Length(WINDOW_DEFAULT_FONT_NAME) <> 0 ) then
  begin
   WINDOW_DEFAULT_FONT:=FontFindByName(WINDOW_DEFAULT_FONT_NAME);
   if WINDOW_DEFAULT_FONT = INVALID_HANDLE_VALUE then
    begin
     WINDOW_DEFAULT_FONT:=FontFindByDescription(WINDOW_DEFAULT_FONT_NAME);
    end;
  end;

 Result:=WINDOW_DEFAULT_FONT;
end;

{==============================================================================}

function ConsoleWindowRedirectOutput(Handle:TWindowHandle):Boolean;
{Redirect standard output to the console window specified by Handle}
{Handle: The window handle to redirect output to (or INVALID_HANDLE_VALUE to stop redirection)}
{Return: True if completed successfully or False if an error occurred}

{Note: Redirects the output of the text files Output, ErrOutput, StdOut and StdErr
       which also redirects the output of Write, WriteLn and the standard C library}
var
 Window:PConsoleWindow;
begin
 {}
 Result:=False;

 if Handle = INVALID_HANDLE_VALUE then
  begin
   {Stop Redirection}
   TextIOWriteCharHandler:=nil;
   TextIOWriteBufferHandler:=nil;

   ConsoleTextIOOutputHandle:=INVALID_HANDLE_VALUE;
  end
 else
  begin
   {Get Window}
   Window:=PConsoleWindow(Handle);
   if Window = nil then Exit;
   if Window.Signature <> WINDOW_SIGNATURE then Exit;

   {Start Redirection}
   TextIOWriteCharHandler:=SysTextIOWriteChar;
   TextIOWriteBufferHandler:=nil;

   ConsoleTextIOOutputHandle:=Handle;
  end;

 Result:=True;
end;

{==============================================================================}
{==============================================================================}

initialization
 ConsoleInit;

{==============================================================================}

finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
