{
Ultibo Console interface unit.

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


Console
=======

 Notes: Console coordinates X,Y are based on either pixels or characters depending on the console mode (Pixel or Character)
        Console coordinates begin at 0,0 and extend to Width - 1, Height - 1
        
        Console Window coordinates X,Y are always based on characters, beginning at 1,1 and extending to Cols,Rows
      

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit Console; 

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,Framebuffer,Font,SysUtils;

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
 
 {Console Device States}
 CONSOLE_STATE_CLOSED   = 0;
 CONSOLE_STATE_OPEN     = 1;
 
 {Console Device Flags}
 CONSOLE_FLAG_NONE            = $00000000;
 CONSOLE_FLAG_LINE_WRAP       = $00000001; {Wrap long lines to the next line if set}
 CONSOLE_FLAG_DMA_BOX         = $00000002; {Use DMA to draw boxes (Where applicable)}
 CONSOLE_FLAG_DMA_LINE        = $00000004; {Use DMA to draw lines (Where applicable)}
 CONSOLE_FLAG_DMA_FILL        = $00000008; {Use DMA to fill blocks (Where applicable)}
 CONSOLE_FLAG_DMA_CLEAR       = $00000010; {Use DMA to clear blocks (Where applicable)} 
 CONSOLE_FLAG_DMA_SCROLL      = $00000020; {Use DMA to scroll blocks (Where applicable)}
 CONSOLE_FLAG_SINGLE_WINDOW   = $00000040; {Console supports only one window (Not multiple)}
 CONSOLE_FLAG_HARDWARE_CURSOR = $00000080; {Console supports a hardware cursor}
 CONSOLE_FLAG_SINGLE_CURSOR   = $00000100; {Console supports only one cursor (Not one per window)}
 CONSOLE_FLAG_BLINK_CURSOR    = $00000200; {Console supports blinking cursor}
 CONSOLE_FLAG_TEXT_MODE       = $00000400; {Console supports text mode settings}
 CONSOLE_FLAG_TEXT_BLINK      = $00000800; {Console supports blinking text}
 CONSOLE_FLAG_COLOR           = $00001000; {Console supports colors}
 CONSOLE_FLAG_FONT            = $00002000; {Console supports fonts} 
 CONSOLE_FLAG_FULLSCREEN      = $00004000; {Console supports creating a fullscreen window}
 CONSOLE_FLAG_AUTO_SCROLL     = $00008000; {Automatically scroll up on reaching the last line}
 CONSOLE_FLAG_DMA_TEXT        = $00010000; {Use DMA to draw text (Where applicable)}
 CONSOLE_FLAG_COLOR_REVERSE   = $00020000; {Console requires colors to be reversed for underlying hardware}
 
 CONSOLE_FLAG_DMA_MASK = CONSOLE_FLAG_DMA_BOX or CONSOLE_FLAG_DMA_LINE or CONSOLE_FLAG_DMA_FILL or CONSOLE_FLAG_DMA_CLEAR or CONSOLE_FLAG_DMA_SCROLL or CONSOLE_FLAG_DMA_TEXT;
 
 {Console Device Modes}
 CONSOLE_MODE_NONE      = 0;
 CONSOLE_MODE_PIXEL     = 1;
 CONSOLE_MODE_CHARACTER = 2;
 
 {Console Window Signature}
 WINDOW_SIGNATURE = $DE3A5C04;
 
 {Console Window States}
 WINDOW_STATE_INVISIBLE = 0;
 WINDOW_STATE_VISIBLE   = 1;
 
 {Console Window Modes}
 WINDOW_MODE_NONE       = 0;
 WINDOW_MODE_TEXT       = 1;
 WINDOW_MODE_GRAPHICS   = 2;
 
 {Console Window Flags}
 WINDOW_FLAG_NONE        = $00000000;
 WINDOW_FLAG_LINE_WRAP   = $00000001; {Wrap long lines to the next line if set}
 WINDOW_FLAG_BUFFERED    = $00000002; {Buffer output for scroll back and redraw}
 WINDOW_FLAG_FULLSCREEN  = $00000004; {Window occupies the full screen}
 WINDOW_FLAG_AUTO_SCROLL = $00000008; {Automatically scroll up on reaching the last line}
 WINDOW_FLAG_CHARACTER   = $00000010; {Console for this Window is character mode only}
 
 {Console Cursor Modes}
 CURSOR_MODE_INSERT    = 0;
 CURSOR_MODE_OVERWRITE = 1;
 
{==============================================================================}
const
 {Framebuffer Console specific constants}
 FRAMEBUFFER_CONSOLE_TITLE = 'Ultibo Core (Release: ' + ULTIBO_RELEASE_NAME + ' Version: ' + ULTIBO_RELEASE_VERSION + ' Date: ' + ULTIBO_RELEASE_DATE + ')';
 
{==============================================================================}
type
 {Console specific types}
 TCursorState = (CURSORON,CURSOROFF);
 
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
 TConsoleEnumerate = function(Console:PConsoleDevice;Data:Pointer):LongWord;
 {Console Notification Callback}
 TConsoleNotification = function(Device:PDevice;Data:Pointer;Notification:LongWord):LongWord;
 
 {Console Device Methods}
 TConsoleDeviceOpen = function(Console:PConsoleDevice):LongWord;
 TConsoleDeviceClose = function(Console:PConsoleDevice):LongWord;
 TConsoleDeviceClear = function(Console:PConsoleDevice;Color:LongWord):LongWord;
 TConsoleDeviceScroll = function(Console:PConsoleDevice;X1,Y1,X2,Y2,Count,Direction:LongWord):LongWord;
 TConsoleDeviceDrawBox = function(Console:PConsoleDevice;X1,Y1,X2,Y2,Color,Width:LongWord):LongWord;
 TConsoleDeviceDrawLine = function(Console:PConsoleDevice;X1,Y1,X2,Y2,Color,Width:LongWord):LongWord;
 TConsoleDeviceDrawChar = function(Console:PConsoleDevice;Handle:TFontHandle;Ch:Char;X,Y,Forecolor,Backcolor:LongWord):LongWord;
 TConsoleDeviceDrawText = function(Console:PConsoleDevice;Handle:TFontHandle;const Text:String;X,Y,Forecolor,Backcolor,Len:LongWord):LongWord;
 TConsoleDeviceDrawPixel = function(Console:PConsoleDevice;X,Y,Color:LongWord):LongWord;
 TConsoleDeviceDrawBlock = function(Console:PConsoleDevice;X1,Y1,X2,Y2,Color:LongWord):LongWord;
 TConsoleDeviceDrawImage = function(Console:PConsoleDevice;X,Y:LongWord;Buffer:Pointer;Width,Height,Format,Skip:LongWord):LongWord;
 TConsoleDeviceDrawWindow = function(Console:PConsoleDevice;Handle:TWindowHandle):LongWord;
 
 TConsoleDeviceGetPixel = function(Console:PConsoleDevice;X,Y:LongWord;var Color:LongWord):LongWord;
 TConsoleDeviceGetImage = function(Console:PConsoleDevice;X,Y:LongWord;Buffer:Pointer;Width,Height,Format,Skip:LongWord):LongWord;
 
 TConsoleDevicePutText = function(Console:PConsoleDevice;Handle:TFontHandle;const Source,Dest:TConsolePoint;Buffer:PConsoleChar;Width,Height,Skip:LongWord):LongWord;
 
 TConsoleDeviceCopyImage = function(Console:PConsoleDevice;const Source,Dest:TConsolePoint;Width,Height:LongWord):LongWord;
 
 TConsoleDeviceGetPosition = function(Console:PConsoleDevice;Position:LongWord;var X1,Y1,X2,Y2:LongWord):LongWord;
 TConsoleDeviceGetProperties = function(Console:PConsoleDevice;Properties:PConsoleProperties):LongWord;
 
 PConsoleWindow = ^TConsoleWindow;
 
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
  DeviceDrawChar:TConsoleDeviceDrawChar;         {A device specific DeviceDrawChar method implementing a standard console device interface (Mandatory)}
  DeviceDrawText:TConsoleDeviceDrawText;         {A device specific DeviceDrawText method implementing a standard console device interface (Mandatory)}
  DeviceDrawPixel:TConsoleDeviceDrawPixel;       {A device specific DeviceDrawPixel method implementing a standard console device interface (Mandatory for CONSOLE_MODE_PIXEL)}
  DeviceDrawBlock:TConsoleDeviceDrawBlock;       {A device specific DeviceDrawBlock method implementing a standard console device interface (Mandatory)}
  DeviceDrawImage:TConsoleDeviceDrawImage;       {A device specific DeviceDrawImage method implementing a standard console device interface (Mandatory for CONSOLE_MODE_PIXEL)}
  DeviceDrawWindow:TConsoleDeviceDrawWindow;     {A device specific DeviceDrawWindow method implementing a standard console device interface (Mandatory)}
  DeviceGetPixel:TConsoleDeviceGetPixel;         {A device specific DeviceGetPixel method implementing a standard console device interface (Mandatory for CONSOLE_MODE_PIXEL)}
  DeviceGetImage:TConsoleDeviceGetImage;         {A device specific DeviceGetImage method implementing a standard console device interface (Mandatory for CONSOLE_MODE_PIXEL)}
  DevicePutText:TConsoleDevicePutText;           {A device specific DevicePutText method implementing a standard console device interface (Mandatory)}
  DeviceCopyImage:TConsoleDeviceCopyImage;       {A device specific DeviceCopyImage method implementing a standard console device interface (Mandatory for CONSOLE_MODE_PIXEL)}
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
  {Window Properties}
  WindowFirst:PConsoleWindow;
  WindowLock:TCriticalSectionHandle;
  WindowCount:LongWord;
  WindowDefault:TWindowHandle;
  {Internal Properties}
  Prev:PConsoleDevice;                           {Previous entry in Console device table}
  Next:PConsoleDevice;                           {Next entry in Console device table}
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
  FontWidth:LongWord;                            {Font Width (Pixels)}
  FontHeight:LongWord;                           {Font Height (Pixels)}
  Borderwidth:LongWord;                          {Current Border Width}
  Font:TFontHandle;                              {Window Font}
  Console:PConsoleDevice;                        {Window console}
 end;
 
 {Console Window Enumeration Callback}
 TConsoleWindowEnumerate = function(Console:PConsoleDevice;Handle:TWindowHandle;Data:Pointer):LongWord;
 
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
  X:LongWord;                                    {Current X}  {Window Relative (Characters for WINDOW_MODE_TEXT / Not used for WINDOW_MODE_GRAPHICS)}
  Y:LongWord;                                    {Current Y}  {Window Relative (Characters for WINDOW_MODE_TEXT / Not used for WINDOW_MODE_GRAPHICS)}
  Cols:LongWord;                                 {Viewport Columns (Characters for WINDOW_MODE_TEXT / Pixels for WINDOW_MODE_GRAPHICS)}
  Rows:LongWord;                                 {Viewport Rows (Characters for WINDOW_MODE_TEXT / Pixels for WINDOW_MODE_GRAPHICS)}
  Format:LongWord;                               {Color Format (eg COLOR_FORMAT_ARGB32)(Only applicable if CONSOLE_MODE_PIXEL)}
  Forecolor:LongWord;                            {Current Foreground Color}
  Backcolor:LongWord;                            {Current Background Color}
  Borderwidth:LongWord;                          {Current Border Width}
  Bordercolor:LongWord;                          {Current Border Color}
  {Font Properties}
  Font:TFontHandle;                              {Window Font}
  FontWidth:LongWord;                            {Font Width (Pixels)}
  FontHeight:LongWord;                           {Font Height (Pixels)}
  {Cursor Properties}
  CursorX:LongWord;                              {Cursor X}  {Window Relative (Characters for WINDOW_MODE_TEXT / Not used for WINDOW_MODE_GRAPHICS)}
  CursorY:LongWord;                              {Cursor Y}  {Window Relative (Characters for WINDOW_MODE_TEXT / Not used for WINDOW_MODE_GRAPHICS)}
  CursorMode:LongWord;                           {Cursor Mode (eg CURSOR_MODE_INSERT)}
  CursorBlink:Boolean;                           {Cursor Blink On/Off}
  CursorState:TCursorState;                      {Cursor State On/Off}
  CursorTimer:TTimerHandle;                      {Cursor Timer (or INVALID_HANDLE_VALUE)}
  {Driver Properties}
  Lock:TMutexHandle;                             {Window lock}
  Console:PConsoleDevice;                        {Window console}
  {Internal Properties}
  Prev:PConsoleWindow;                           {Previous entry in Console Window table}
  Next:PConsoleWindow;                           {Next entry in Console Window table}
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
  {DMA Properties}
  //FillSize:LongWord;                             {Size of the DMA fill buffer (From DMAAllocateBufferEx)}  //To Do //Remove
  //FillBuffer:Pointer;                            {Buffer for DMA memory fills}                             //To Do //Remove
  //ScrollBuffer:Pointer;                          {Buffer for DMA scroll right (Overlapped)}                //To Do //Remove
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
function ConsoleDeviceDrawChar(Console:PConsoleDevice;Handle:TFontHandle;Ch:Char;X,Y,Forecolor,Backcolor:LongWord):LongWord;
function ConsoleDeviceDrawText(Console:PConsoleDevice;Handle:TFontHandle;const Text:String;X,Y,Forecolor,Backcolor,Len:LongWord):LongWord;
function ConsoleDeviceDrawPixel(Console:PConsoleDevice;X,Y,Color:LongWord):LongWord;
function ConsoleDeviceDrawBlock(Console:PConsoleDevice;X1,Y1,X2,Y2,Color:LongWord):LongWord;
function ConsoleDeviceDrawImage(Console:PConsoleDevice;X,Y:LongWord;Buffer:Pointer;Width,Height,Format,Skip:LongWord):LongWord;
function ConsoleDeviceDrawWindow(Console:PConsoleDevice;Handle:TWindowHandle):LongWord;

function ConsoleDeviceGetPixel(Console:PConsoleDevice;X,Y:LongWord;var Color:LongWord):LongWord;
function ConsoleDeviceGetImage(Console:PConsoleDevice;X,Y:LongWord;Buffer:Pointer;Width,Height,Format,Skip:LongWord):LongWord;
 
function ConsoleDevicePutText(Console:PConsoleDevice;Handle:TFontHandle;const Source,Dest:TConsolePoint;Buffer:PConsoleChar;Width,Height,Skip:LongWord):LongWord;

function ConsoleDeviceCopyImage(Console:PConsoleDevice;const Source,Dest:TConsolePoint;Width,Height:LongWord):LongWord; 

function ConsoleDeviceGetPosition(Console:PConsoleDevice;Position:LongWord;var X1,Y1,X2,Y2:LongWord):LongWord;
function ConsoleDeviceGetProperties(Console:PConsoleDevice;Properties:PConsoleProperties):LongWord;

function ConsoleDeviceCheckFlag(Console:PConsoleDevice;Flag:LongWord):Boolean;

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

function ConsoleWindowFind(Console:PConsoleDevice;Position:LongWord):TWindowHandle;
function ConsoleWindowEnumerate(Console:PConsoleDevice;Callback:TConsoleWindowEnumerate;Data:Pointer):LongWord;

function ConsoleWindowCheckFlag(Handle:TWindowHandle;Flag:LongWord):Boolean;

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
function ConsoleWindowGetCursorBlink(Handle:TWindowHandle):Boolean;
function ConsoleWindowSetCursorBlink(Handle:TWindowHandle;CursorBlink:Boolean):LongWord;
function ConsoleWindowGetCursorState(Handle:TWindowHandle):TCursorState;
function ConsoleWindowSetCursorState(Handle:TWindowHandle;CursorState:TCursorState):LongWord;

function ConsoleWindowCursorOn(Handle:TWindowHandle):LongWord;
function ConsoleWindowCursorOff(Handle:TWindowHandle):LongWord;
function ConsoleWindowCursorMove(Handle:TWindowHandle;X,Y:LongWord):LongWord;
function ConsoleWindowCursorBlink(Handle:TWindowHandle;Enabled:Boolean):LongWord;

function ConsoleWindowScrollUp(Handle:TWindowHandle;Row,Lines:LongWord):LongWord;
function ConsoleWindowScrollDown(Handle:TWindowHandle;Row,Lines:LongWord):LongWord;

function ConsoleWindowScrollLeft(Handle:TWindowHandle;Row,Col,Lines,Chars:LongWord):LongWord;
function ConsoleWindowScrollRight(Handle:TWindowHandle;Row,Col,Lines,Chars:LongWord):LongWord;

function ConsoleWindowClear(Handle:TWindowHandle):LongWord;
function ConsoleWindowClearEx(Handle:TWindowHandle;X1,Y1,X2,Y2:LongWord;Cursor:Boolean):LongWord;

function ConsoleWindowWrite(Handle:TWindowHandle;const AText:String):LongWord;
function ConsoleWindowWriteEx(Handle:TWindowHandle;const AText:String;X,Y:LongWord;Forecolor,Backcolor:LongWord):LongWord;

function ConsoleWindowWriteLn(Handle:TWindowHandle;const AText:String):LongWord;
function ConsoleWindowWriteLnEx(Handle:TWindowHandle;const AText:String;X,Y:LongWord;Forecolor,Backcolor:LongWord):LongWord;

function ConsoleWindowWriteChr(Handle:TWindowHandle;AChr:Char):LongWord;
function ConsoleWindowWriteChrEx(Handle:TWindowHandle;AChr:Char;X,Y:LongWord;Forecolor,Backcolor:LongWord):LongWord;

function ConsoleWindowOutput(Handle:TWindowHandle;const Source,Dest:TConsolePoint;Buffer:PConsoleChar;Width,Height,Skip:LongWord):LongWord;

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
{RTL Console Functions}
function SysConsoleWriteChar(ACh:Char;AUserData:Pointer):Boolean;

{==============================================================================}
{Framebuffer Console Functions}
function FramebufferConsoleOpen(Console:PConsoleDevice):LongWord;
function FramebufferConsoleClose(Console:PConsoleDevice):LongWord;

function FramebufferConsoleClear(Console:PConsoleDevice;Color:LongWord):LongWord;
//function FramebufferConsoleClearOld(Console:PConsoleDevice;Color:LongWord):LongWord; //To Do //Remove
function FramebufferConsoleScroll(Console:PConsoleDevice;X1,Y1,X2,Y2,Count,Direction:LongWord):LongWord;
//function FramebufferConsoleScrollOld(Console:PConsoleDevice;X1,Y1,X2,Y2,Count,Direction:LongWord):LongWord; //To Do //Remove

function FramebufferConsoleDrawBox(Console:PConsoleDevice;X1,Y1,X2,Y2,Color,Width:LongWord):LongWord;
//function FramebufferConsoleDrawBoxOld(Console:PConsoleDevice;X1,Y1,X2,Y2,Color,Width:LongWord):LongWord; //To Do //Remove
function FramebufferConsoleDrawLine(Console:PConsoleDevice;X1,Y1,X2,Y2,Color,Width:LongWord):LongWord;
//function FramebufferConsoleDrawLineOld(Console:PConsoleDevice;X1,Y1,X2,Y2,Color,Width:LongWord):LongWord; //To Do //Remove
function FramebufferConsoleDrawChar(Console:PConsoleDevice;Handle:TFontHandle;Ch:Char;X,Y,Forecolor,Backcolor:LongWord):LongWord;
//function FramebufferConsoleDrawCharOld(Console:PConsoleDevice;Handle:TFontHandle;Ch:Char;X,Y,Forecolor,Backcolor:LongWord):LongWord; //To Do //Remove
function FramebufferConsoleDrawText(Console:PConsoleDevice;Handle:TFontHandle;const Text:String;X,Y,Forecolor,Backcolor,Len:LongWord):LongWord;
//function FramebufferConsoleDrawTextOld(Console:PConsoleDevice;Handle:TFontHandle;const Text:String;X,Y,Forecolor,Backcolor,Len:LongWord):LongWord; //To Do //Remove
function FramebufferConsoleDrawPixel(Console:PConsoleDevice;X,Y,Color:LongWord):LongWord;
//function FramebufferConsoleDrawPixelOld(Console:PConsoleDevice;X,Y,Color:LongWord):LongWord; //To Do //Remove
function FramebufferConsoleDrawBlock(Console:PConsoleDevice;X1,Y1,X2,Y2,Color:LongWord):LongWord;
//function FramebufferConsoleDrawBlockOld(Console:PConsoleDevice;X1,Y1,X2,Y2,Color:LongWord):LongWord; //To Do //Remove
function FramebufferConsoleDrawImage(Console:PConsoleDevice;X,Y:LongWord;Buffer:Pointer;Width,Height,Format,Skip:LongWord):LongWord;
function FramebufferConsoleDrawWindow(Console:PConsoleDevice;Handle:TWindowHandle):LongWord;
function FramebufferConsoleDrawDesktop(Console:PConsoleDevice):LongWord;

function FramebufferConsoleGetPixel(Console:PConsoleDevice;X,Y:LongWord;var Color:LongWord):LongWord;
//function FramebufferConsoleGetPixelOld(Console:PConsoleDevice;X,Y:LongWord;var Color:LongWord):LongWord; //To Do //Remove
function FramebufferConsoleGetImage(Console:PConsoleDevice;X,Y:LongWord;Buffer:Pointer;Width,Height,Format,Skip:LongWord):LongWord;
 
function FramebufferConsolePutText(Console:PConsoleDevice;Handle:TFontHandle;const Source,Dest:TConsolePoint;Buffer:PConsoleChar;Width,Height,Skip:LongWord):LongWord;
 
function FramebufferConsoleCopyImage(Console:PConsoleDevice;const Source,Dest:TConsolePoint;Width,Height:LongWord):LongWord; 

function FramebufferConsoleGetPosition(Console:PConsoleDevice;Position:LongWord;var X1,Y1,X2,Y2:LongWord):LongWord;

{==============================================================================}
{Console Helper Functions}
function ConsoleDeviceGetCount:LongWord; inline;
function ConsoleDeviceGetDefault:PConsoleDevice; inline;
function ConsoleDeviceSetDefault(Console:PConsoleDevice):LongWord; 

function ConsoleDeviceCheck(Console:PConsoleDevice):PConsoleDevice;

function ConsoleFramebufferDeviceAdd(Framebuffer:PFramebufferDevice):LongWord;
function ConsoleFramebufferDeviceRemove(Framebuffer:PFramebufferDevice):LongWord;

function ConsoleFramebufferDeviceEnum(Framebuffer:PFramebufferDevice;Data:Pointer):LongWord;
function ConsoleFramebufferDeviceNotify(Device:PDevice;Data:Pointer;Notification:LongWord):LongWord;

{==============================================================================}
{Text Console Helper Functions}
function ConsoleWindowGetCount(Console:PConsoleDevice):LongWord; inline;
function ConsoleWindowGetDefault(Console:PConsoleDevice):TWindowHandle; inline;
function ConsoleWindowSetDefault(Console:PConsoleDevice;Handle:TWindowHandle):LongWord;

function ConsoleWindowCheck(Console:PConsoleDevice;Window:PConsoleWindow):PConsoleWindow;

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
 
{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure ConsoleInit;
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
 
 {Setup Console Defaults}
 CONSOLE_DEFAULT_FORECOLOR:=COLOR_LIGHTGRAY;
 CONSOLE_DEFAULT_BACKCOLOR:=COLOR_BLACK;
 
 CONSOLE_DEFAULT_BORDERWIDTH:=2;
 CONSOLE_DEFAULT_BORDERCOLOR:=COLOR_WHITE;
 
 CONSOLE_DEFAULT_FONT:=FontGetDefault;
 
 {Setup Window Defaults}
 WINDOW_DEFAULT_FORECOLOR:=COLOR_DARKGRAY;
 WINDOW_DEFAULT_BACKCOLOR:=COLOR_WHITE;
 
 WINDOW_DEFAULT_BORDERWIDTH:=2;
 WINDOW_DEFAULT_BORDERCOLOR:=COLOR_MIDGRAY;
 
 WINDOW_DEFAULT_FONT:=FontGetDefault;
 
 {Setup Framebuffer Console}
 FRAMEBUFFER_CONSOLE_DEFAULT_DESKTOPOFFSET:=48; 
 FRAMEBUFFER_CONSOLE_DEFAULT_DESKTOPCOLOR:=COLOR_ORANGE;
  
 {Check Environment Variables}
 //To Do
 
 {Enumerate Framebuffers}
 FramebufferDeviceEnumerate(ConsoleFramebufferDeviceEnum,nil);
 
 {Register Notification}
 FramebufferDeviceNotification(nil,ConsoleFramebufferDeviceNotify,nil,DEVICE_NOTIFICATION_REGISTER or DEVICE_NOTIFICATION_DEREGISTER or DEVICE_NOTIFICATION_ENABLE or DEVICE_NOTIFICATION_DISABLE,NOTIFIER_FLAG_NONE);
 
 {Setup Platform Console Handlers}
 ConsoleWriteCharHandler:=SysConsoleWriteChar;
 
 ConsoleInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{Console Functions}
function ConsoleDeviceOpen(Console:PConsoleDevice):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IF DEFINED(CONSOLE_DEBUG) or DEFINED(DEVICE_DEBUG)}
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
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IF DEFINED(CONSOLE_DEBUG) or DEFINED(DEVICE_DEBUG)}
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
{Note: Color must be specified in the default color format (See COLOR_FORMAT_DEFAULT)}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
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
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
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
{Note: Color must be specified in the default color format (See COLOR_FORMAT_DEFAULT)}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
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
{Note: Color must be specified in the default color format (See COLOR_FORMAT_DEFAULT)}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
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

function ConsoleDeviceDrawChar(Console:PConsoleDevice;Handle:TFontHandle;Ch:Char;X,Y,Forecolor,Backcolor:LongWord):LongWord;
{Note: Forecolor and Backcolor must be specified in the default color format (See COLOR_FORMAT_DEFAULT)}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
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
{Note: Forecolor and Backcolor must be specified in the default color format (See COLOR_FORMAT_DEFAULT)}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
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
{Note: Color must be specified in the default color format (See COLOR_FORMAT_DEFAULT)}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
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
{Note: Color must be specified in the default color format (See COLOR_FORMAT_DEFAULT)}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {Check Open}
 Result:=ERROR_NOT_SUPPORTED;
 if Console.ConsoleState <> CONSOLE_STATE_OPEN then Exit;
 
 if Assigned(Console.DeviceDrawBlock) then
  begin
   Result:=Console.DeviceDrawBlock(Console,X1,Y1,X2,Y2,Color);
  end;
end;

{==============================================================================}

function ConsoleDeviceDrawImage(Console:PConsoleDevice;X,Y:LongWord;Buffer:Pointer;Width,Height,Format,Skip:LongWord):LongWord;
//To Do //Continuing
{Skip: The number of pixels to skip in the buffer after each row (Optional)}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
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

function ConsoleDeviceDrawWindow(Console:PConsoleDevice;Handle:TWindowHandle):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {Check Open}
 Result:=ERROR_NOT_SUPPORTED;
 if Console.ConsoleState <> CONSOLE_STATE_OPEN then Exit;
 
 if Assigned(Console.DeviceDrawWindow) then
  begin
   Result:=Console.DeviceDrawWindow(Console,Handle);
  end;
end;
 
{==============================================================================}
 
function ConsoleDeviceGetPixel(Console:PConsoleDevice;X,Y:LongWord;var Color:LongWord):LongWord;
{Note: Color is returned in the default color format (See COLOR_FORMAT_DEFAULT)}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
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
//To Do //Continuing
{Skip: The number of pixels to skip in the buffer after each row (Optional)}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
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

{Note: Source, Width, Heigth and Skip are based on character rows and columns not screen pixels}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
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
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
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
 
function ConsoleDeviceGetPosition(Console:PConsoleDevice;Position:LongWord;var X1,Y1,X2,Y2:LongWord):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
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
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Properties}
 if Properties = nil then Exit;
 
 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {$IFDEF DEVICE_DEBUG}
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

function ConsoleDeviceGetMode(Console:PConsoleDevice):LongWord;
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
 Result.WindowFirst:=nil;
 Result.WindowLock:=INVALID_HANDLE_VALUE;
 Result.WindowCount:=0;
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
end;

{==============================================================================}

function ConsoleDeviceDestroy(Console:PConsoleDevice):LongWord;
{Destroy an existing Console entry}
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
begin
 {}
 Result:=PConsoleDevice(DeviceFindByName(Name));
end;

{==============================================================================}

function ConsoleDeviceFindByDescription(const Description:String):PConsoleDevice; inline;
begin
 {}
 Result:=PConsoleDevice(DeviceFindByDescription(Description));
end;

{==============================================================================}

function ConsoleDeviceEnumerate(Callback:TConsoleEnumerate;Data:Pointer):LongWord;
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
    Window.CursorBlink:=False;
    Window.CursorState:=CURSOROFF;
    Window.CursorTimer:=INVALID_HANDLE_VALUE;
    {Driver}
    Window.Lock:=INVALID_HANDLE_VALUE;
    Window.Console:=Console;
    
    {Setup Flags}
    if Position = CONSOLE_POSITION_FULLSCREEN then Window.WindowFlags:=Window.WindowFlags or WINDOW_FLAG_FULLSCREEN;
    if Console.ConsoleMode = CONSOLE_MODE_CHARACTER then Window.WindowFlags:=Window.WindowFlags or WINDOW_FLAG_CHARACTER;
    if (CONSOLE_LINE_WRAP) or ((Console.Device.DeviceFlags and CONSOLE_FLAG_LINE_WRAP) <> 0) then Window.WindowFlags:=Window.WindowFlags or WINDOW_FLAG_LINE_WRAP;
    if (CONSOLE_AUTO_SCROLL) or ((Console.Device.DeviceFlags and CONSOLE_FLAG_AUTO_SCROLL) <> 0) then Window.WindowFlags:=Window.WindowFlags or WINDOW_FLAG_AUTO_SCROLL;
    
    {Check Border}
    if Position = CONSOLE_POSITION_FULLSCREEN then Window.Borderwidth:=0; //To do //Would this be better based on another criteria ?
    
    {Get Font}
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
    Window.Width:=(((Window.X2 - Window.X1) + 1) - Window.Borderwidth) div Window.FontWidth;
    Window.Height:=(((Window.Y2 - Window.Y1) + 1) - Window.Borderwidth) div Window.FontHeight;
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
      RemainX:=(((Window.X2 - Window.X1) + 1) - Window.Borderwidth) mod Window.FontWidth;
      RemainY:=(((Window.Y2 - Window.Y1) + 1) - Window.Borderwidth) mod Window.FontHeight;
      
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
         Console.WindowDefault:=TWindowHandle(Window);
        end;
        
       {Draw Window}
       if State = WINDOW_STATE_VISIBLE then ConsoleDeviceDrawWindow(Console,TWindowHandle(Window));
       
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
 Prev:PConsoleWindow;
 Next:PConsoleWindow;
 Window:PConsoleWindow;
 Console:PConsoleDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;
 
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
       {Set State}
       Window.WindowState:=WINDOW_STATE_INVISIBLE;
    
       {Draw Window}
       Result:=ConsoleDeviceDrawWindow(Window.Console,TWindowHandle(Window));
       if Result <> ERROR_SUCCESS then Exit;
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
    Result:=ConsoleDeviceDrawWindow(Window.Console,TWindowHandle(Window));
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
    {Set State}
    Window.WindowState:=WINDOW_STATE_INVISIBLE;
    
    {Draw Window}
    Result:=ConsoleDeviceDrawWindow(Window.Console,TWindowHandle(Window));
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
 Window:PConsoleWindow;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Position}
 {if Position < CONSOLE_POSITION_FULL then Exit;}
 if (Position <> CONSOLE_POSITION_FULLSCREEN) and (Position > CONSOLE_POSITION_BOTTOMRIGHT) then Exit;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;
 
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
   {Hide Window}
   Window.WindowState:=WINDOW_STATE_INVISIBLE;
   
   {Draw Window}
   if State = WINDOW_STATE_VISIBLE then ConsoleDeviceDrawWindow(Window.Console,TWindowHandle(Window));
   
   {Get Position}
   if ConsoleDeviceGetPosition(Window.Console,Position,X1,Y1,X2,Y2) <> ERROR_SUCCESS then Exit;
   
   {Update Window}
   Window.X1:=X1;
   Window.Y1:=Y1;
   Window.X2:=X2;
   Window.Y2:=Y2;
  
   {Get Width / Height}
   Window.Width:=(((Window.X2 - Window.X1) + 1) - Window.Borderwidth) div Window.FontWidth;
   Window.Height:=(((Window.Y2 - Window.Y1) + 1) - Window.Borderwidth) div Window.FontHeight;
   
   {Check Font Ratio}
   if Window.Console.FontRatio > 0 then
    begin
     {Get RemainX,Y}
     RemainX:=(((Window.X2 - Window.X1) + 1) - Window.Borderwidth) mod Window.FontWidth;
     RemainY:=(((Window.Y2 - Window.Y1) + 1) - Window.Borderwidth) mod Window.FontHeight;
     
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
   
   {Get MinX,Y / MaxX,Y}
   if Window.MinX > Window.Width then Window.MinX:=1;
   if Window.MinY > Window.Height then Window.MinY:=1;
   if Window.MaxX > Window.Width then Window.MaxX:=Window.Width;
   if Window.MaxY > Window.Height then Window.MaxY:=Window.Height;
   
   {Get Cols / Rows}
   Window.Cols:=Window.MaxX - (Window.MinX - 1);
   Window.Rows:=Window.MaxY - (Window.MinY - 1);
   
   {Get X,Y / Cursor X,Y}
   if Window.X > Window.Cols then Window.X:=1;
   if Window.Y > Window.Rows then Window.Y:=1;
   if Window.CursorX > Window.Cols then Window.CursorX:=1;
   if Window.CursorY > Window.Rows then Window.CursorY:=1;
  
   {Return Result}
   Result:=ERROR_SUCCESS;
  finally
   {Restore State}
   Window.WindowState:=State;
   
   {Draw Window}
   if State = WINDOW_STATE_VISIBLE then ConsoleDeviceDrawWindow(Window.Console,TWindowHandle(Window));
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
 
 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_TEXT then Exit;
 
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
 if Color = COLOR_NONE then Exit;
 
 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;
 
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
 if Color = COLOR_NONE then Exit;
 
 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;
 
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
    {Set Font}
    Window.Font:=Font;
    Window.FontWidth:=FontWidth * Window.Console.FontRatio;
    if Window.FontWidth = 0 then Window.FontWidth:=1;
    Window.FontHeight:=FontHeight * Window.Console.FontRatio;
    if Window.FontHeight = 0 then Window.FontHeight:=1;
   
    {Get Width / Height}
    Window.Width:=(((Window.X2 - Window.X1) + 1) - Window.Borderwidth) div Window.FontWidth;
    Window.Height:=(((Window.Y2 - Window.Y1) + 1) - Window.Borderwidth) div Window.FontHeight;
    
    {Check Font Ratio}
    if Window.Console.FontRatio > 0 then
     begin
      {Get RemainX,Y}
      RemainX:=(((Window.X2 - Window.X1) + 1) - Window.Borderwidth) mod Window.FontWidth;
      RemainY:=(((Window.Y2 - Window.Y1) + 1) - Window.Borderwidth) mod Window.FontHeight;
      
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
    
    {Get MinX,Y / MaxX,Y}
    if Window.MinX > Window.Width then Window.MinX:=1;
    if Window.MinY > Window.Height then Window.MinY:=1;
    if Window.MaxX > Window.Width then Window.MaxX:=Window.Width;
    if Window.MaxY > Window.Height then Window.MaxY:=Window.Height;
    
    {Get Cols / Rows}
    Window.Cols:=Window.MaxX - (Window.MinX - 1);
    Window.Rows:=Window.MaxY - (Window.MinY - 1);
    
    {Get X,Y / Cursor X,Y}
    if Window.X > Window.Cols then Window.X:=1;
    if Window.Y > Window.Rows then Window.Y:=1;
    if Window.CursorX > Window.Cols then Window.CursorX:=1;
    if Window.CursorY > Window.Rows then Window.CursorY:=1;
   
    {Draw Window}
    if Window.WindowState = WINDOW_STATE_VISIBLE then ConsoleDeviceDrawWindow(Window.Console,TWindowHandle(Window));
    
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
 
 {Get Cursor XY}
 X:=Window.CursorX;
 Y:=Window.CursorY;
 
 Result:=ERROR_SUCCESS;
 
 {Unlock Window}
 MutexUnlock(Window.Lock);
end;

{==============================================================================}

function ConsoleWindowSetCursorXY(Handle:TWindowHandle;X,Y:LongWord):LongWord;
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
 
 {Check Cursor State}
 if Window.CursorState = CURSORON then
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

function ConsoleWindowGetCursorBlink(Handle:TWindowHandle):Boolean;
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
 if Window.WindowMode <> WINDOW_MODE_TEXT then Exit;
 
 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;
 
 {Get Cursor Blink}
 Result:=Window.CursorBlink;
 
 {Unlock Window}
 MutexUnlock(Window.Lock);
end;

{==============================================================================}

function ConsoleWindowSetCursorBlink(Handle:TWindowHandle;CursorBlink:Boolean):LongWord;
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
var
 Window:PConsoleWindow;
begin
 {}
 Result:=CURSOROFF;
 
 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;
 
 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_TEXT then Exit;
 
 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;
 
 {Get Cursor State}
 Result:=Window.CursorState;
 
 {Unlock Window}
 MutexUnlock(Window.Lock);
end;

{==============================================================================}

function ConsoleWindowSetCursorState(Handle:TWindowHandle;CursorState:TCursorState):LongWord;
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
 
 {Check Cursor State}
 if Window.CursorState = CURSORON then
  begin
   {Unlock Window}
   MutexUnlock(Window.Lock);
 
   {Check Cursor State}
   if CursorState = CURSOROFF then Result:=ConsoleWindowCursorOff(Handle) else Result:=ERROR_SUCCESS;
  end
 else
  begin
   {Unlock Window}
   MutexUnlock(Window.Lock);
   
   {Check Cursor State}
   if CursorState = CURSORON then Result:=ConsoleWindowCursorOn(Handle) else Result:=ERROR_SUCCESS;
  end; 
end;

{==============================================================================}

function ConsoleWindowCursorOn(Handle:TWindowHandle):LongWord;
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

 {Check Cursor State}
 if Window.CursorState = CURSOROFF then
  begin
   {Set Cursor State}
   Window.CursorState:=CURSORON;
   
   {Check Visible}
   if Window.WindowState = WINDOW_STATE_VISIBLE then
    begin
     //To Do //Check Hardware Cursor / Create timer etc
    end; 
 
   Result:=ERROR_SUCCESS;

   {Unlock Window}
   MutexUnlock(Window.Lock);
  end
 else
  begin
   Result:=ERROR_SUCCESS;
   
   {Unlock Window}
   MutexUnlock(Window.Lock);
  end; 
end;

{==============================================================================}

function ConsoleWindowCursorOff(Handle:TWindowHandle):LongWord;
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

 {Check Cursor State}
 if Window.CursorState = CURSORON then
  begin
   {Set Cursor State}
   Window.CursorState:=CURSOROFF;
   
   {Check Visible}
   if Window.WindowState = WINDOW_STATE_VISIBLE then
    begin
     //To Do //Check Hardware Cursor / Destroy timer etc
    end; 
 
   Result:=ERROR_SUCCESS;

   {Unlock Window}
   MutexUnlock(Window.Lock);
  end
 else
  begin
   Result:=ERROR_SUCCESS;
   
   {Unlock Window}
   MutexUnlock(Window.Lock);
  end; 
end;

{==============================================================================}

function ConsoleWindowCursorMove(Handle:TWindowHandle;X,Y:LongWord):LongWord;
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

 {Check Cursor X,Y}
 if (X > 0) and (Y > 0) and (((Window.MinX - 1) + X) <= Window.MaxX) and (((Window.MinY - 1) + Y) <= Window.MaxY) then
  begin
   {Set Cursor XY}
   Window.CursorX:=X;
   Window.CursorY:=Y;
  
   {Check Cursor State}
   if Window.CursorState = CURSORON then
    begin
     {Check Visible}
     if Window.WindowState = WINDOW_STATE_VISIBLE then
      begin
       //To Do //Check Hardware Cursor / Move Cursor
      end; 
    end; 
   
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

{==============================================================================}

function ConsoleWindowCursorBlink(Handle:TWindowHandle;Enabled:Boolean):LongWord;
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

 {Check Cursor State}
 if Window.CursorState = CURSORON then
  begin
   {Check Cursor Blink}
   if Window.CursorBlink and not(Enabled) then
    begin
     {Set Cursor Blink}
     Window.CursorBlink:=Enabled;
     
     {Check Visible}
     if Window.WindowState = WINDOW_STATE_VISIBLE then
      begin
       //To Do //Check Hardware Cursor / Destroy timer etc
      end; 
    end
   else if not(Window.CursorBlink) and Enabled then
    begin
     {Set Cursor Blink}
     Window.CursorBlink:=Enabled;
     
     {Check Visible}
     if Window.WindowState = WINDOW_STATE_VISIBLE then
      begin
       //To Do //Check Hardware Cursor / Create timer etc
      end; 
    end;
   
   Result:=ERROR_SUCCESS;

   {Unlock Window}
   MutexUnlock(Window.Lock);
  end
 else
  begin
   {Set Cursor Blink}
   Window.CursorBlink:=Enabled;
   
   Result:=ERROR_SUCCESS;
   
   {Unlock Window}
   MutexUnlock(Window.Lock);
  end; 
end;
  
{==============================================================================}

function ConsoleWindowScrollUp(Handle:TWindowHandle;Row,Lines:LongWord):LongWord;
{Row is the starting row (Y) for the scroll up, all rows from top plus Lines down to Row will be scrolled up}
{Lines is the number of character lines to scroll up, Lines number of rows at the top will be discarded}
{The starting Row will be blanked with the background color}
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
 finally
  {Unlock Window}
  MutexUnlock(Window.Lock);
 end; 
end;

{==============================================================================}

function ConsoleWindowScrollDown(Handle:TWindowHandle;Row,Lines:LongWord):LongWord;
{Row is the starting row (Y) for the scroll down, all rows from bottom minus Lines up to Row will be scrolled down}
{Lines is the number of character lines to scroll down, Lines number of rows at the bottom will be discarded}
{The starting Row will be blanked with the background color}
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
 finally
  {Unlock Window}
  MutexUnlock(Window.Lock);
 end; 
end;

{==============================================================================}

function ConsoleWindowScrollLeft(Handle:TWindowHandle;Row,Col,Lines,Chars:LongWord):LongWord;
{Row is the starting row (Y) for the scroll left, all rows from Row down to Row + Lines will be scrolled left}
{Lines is the number of rows to scroll left, all rows from Row down to Row + Lines will be scrolled left}
{Col is the starting column (X) for the scroll left, all cols from left plus Chars to Col with be scrolled left}
{Chars is the number of characters to scroll left, Chars number of columnss at the left will be discarded}
{The starting Col will be blanked with the background color}
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
 finally
  {Unlock Window}
  MutexUnlock(Window.Lock);
 end; 
end;

{==============================================================================}

function ConsoleWindowScrollRight(Handle:TWindowHandle;Row,Col,Lines,Chars:LongWord):LongWord;
{Row is the starting row (Y) for the scroll right, all rows from Row down to Row + Lines will be scrolled right}
{Lines is the number of rows to scroll right, all rows from Row down to Row + Lines will be scrolled right}
{Col is the starting column (X) for the scroll right, all rows from right minus Chars to Col will be scrolled right}
{Chars is the number of characters to scroll right, Chars number of columns at the right will be discarded}
{The starting Col will be blanked with the background color}
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
  //To Do //Set CursorX,CursorY, move cursor if on etc
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
    //To Do //Set CursorX,CursorY, move cursor if on etc
   end;   
 finally
  {Unlock Window}
  MutexUnlock(Window.Lock);
 end; 
end;

{==============================================================================}

function ConsoleWindowWrite(Handle:TWindowHandle;const AText:String):LongWord;
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
  //To Do //Check cursor, Move Cursor etc 
  
  {Return Result}
  Result:=ERROR_SUCCESS;
 finally
  {Unlock Window}
  MutexUnlock(Window.Lock);
 end; 
end;
  
{==============================================================================}

function ConsoleWindowWriteEx(Handle:TWindowHandle;const AText:String;X,Y:LongWord;Forecolor,Backcolor:LongWord):LongWord;
{Note: For Text Console functions, X and Y are based on screen character rows and columns not screen pixels}
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
 if Forecolor = COLOR_NONE then Exit;
 if Backcolor = COLOR_NONE then Exit;
 
 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;
 
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
  //To Do //Check cursor, Move Cursor etc 
  
  {Return Result}
  Result:=ERROR_SUCCESS;
 finally
  {Unlock Window}
  MutexUnlock(Window.Lock);
 end; 
end;
  
{==============================================================================}

function ConsoleWindowWriteLnEx(Handle:TWindowHandle;const AText:String;X,Y:LongWord;Forecolor,Backcolor:LongWord):LongWord;
{Note: For Text Console functions, X and Y are based on character rows and columns not screen pixels}
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
 if Forecolor = COLOR_NONE then Exit;
 if Backcolor = COLOR_NONE then Exit;
 
 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;
 
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
var
 WriteX:LongWord;
 WriteY:LongWord;
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
  //To Do //Check cursor, Move Cursor etc 
  
  {Return Result}
  Result:=ERROR_SUCCESS;
 finally
  {Unlock Window}
  MutexUnlock(Window.Lock);
 end; 
end;
  
{==============================================================================}

function ConsoleWindowWriteChrEx(Handle:TWindowHandle;AChr:Char;X,Y:LongWord;Forecolor,Backcolor:LongWord):LongWord;
{Note: For Text Console functions, X and Y are based on character rows and columns not screen pixels}
var
 WriteX:LongWord;
 WriteY:LongWord;
 Window:PConsoleWindow;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Colors}
 if Forecolor = COLOR_NONE then Exit;
 if Backcolor = COLOR_NONE then Exit;
 
 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;
 
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
 
 {Get Window}
 Window:=PConsoleWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_TEXT then Exit;

 {$IFDEF CONSOLE_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'Console: Console Window Output (Width=' + IntToStr(Width) + ' Height=' + IntToStr(Height) + ' Skip=' + IntToStr(Skip) + ')');
 {$ENDIF}
 
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
 
  {No cursor Update}
  
  {Return Result}
  Result:=ERROR_SUCCESS;
 finally
  {Unlock Window}
  MutexUnlock(Window.Lock);
 end; 
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
{Read text from console input and echo to screen}
begin
 {}
 ConsoleReadLn(AText);
end;

{==============================================================================}

procedure ConsoleReadLn(var AText:String);
{Read text from console input and echo to screen}
var
 Ch:Char;
 Done:Boolean;
 SaveX:Integer;
 SaveY:Integer;
 CurrentX:Integer;
begin
 {}
 AText:='';
 Done:=False;
 SaveX:=ConsoleWhereX;
 SaveY:=ConsoleWhereY;
 repeat
  Ch:=ConsoleReadKey;
  case Ch of
   #0:begin
     {Extended key}
     Ch:=ConsoleReadKey;
    end;
   #8:begin
     {Backspace}
     CurrentX:=ConsoleWhereX;
     if (Length(AText) > 0) and (CurrentX > 1) then
      begin
       ConsoleGotoXY(CurrentX - 1,ConsoleWhereY);
       ConsoleWriteChr(#32);
       ConsoleGotoXY(CurrentX - 1,ConsoleWhereY);
       Delete(AText,Length(AText),1);
      end;
    end;
   #9:begin
     {Tab}
     {Ignore}
    end;
   #13:begin
     {Return}
     ConsoleWriteLn('');
     Done:=True;
    end;
   #27:begin
     {Escape}
     ConsoleGotoXY(SaveX,SaveY);
     ConsoleClrEol;
     AText:='';
    end;
   else 
    begin
     {Others}
     ConsoleWriteChr(Ch);
     AText:=AText + Ch;
    end;
  end;
 until Done;
end;

{==============================================================================}

procedure ConsoleReadChr(var AChr:Char);
{Read characters from console input and echo to screen}
var
 Ch:Char;
 Done:Boolean;
begin
 {}
 AChr:=#0;
 Done:=False;
 repeat
  Ch:=ConsoleReadKey;
  case Ch of
   #0:begin
     {Extended key}
     Ch:=ConsoleReadKey;
    end;
   #13:begin
     {Return}
     ConsoleWriteLn('');
     Done:=True;
    end;
   else
    begin  
     {Others}    
     ConsoleWriteChr(Ch);
     AChr:=Ch;
     ConsoleWriteLn('');
     Done:=True;
    end;
  end;
 until Done;
 
 {ConsoleReadChar(AChr,nil);} {Modified to behave similar to Read()}
end;

{==============================================================================}
{==============================================================================}
{RTL Console Functions}
function SysConsoleWriteChar(ACh:Char;AUserData:Pointer):Boolean;
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
    {DMA} //To Do //Remove
    (*if PFramebufferConsole(Console).FillBuffer <> nil then 
     begin
      DMAReleaseBuffer(PFramebufferConsole(Console).FillBuffer);
      PFramebufferConsole(Console).FillSize:=0;
      PFramebufferConsole(Console).FillBuffer:=nil;
     end;
    if PFramebufferConsole(Console).ScrollBuffer <> nil then 
     begin
      DMAReleaseBuffer(PFramebufferConsole(Console).ScrollBuffer);
      PFramebufferConsole(Console).ScrollBuffer:=nil;
     end;*)
     
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
 if Color = COLOR_NONE then Exit;
 
 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
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
//To Do //Remove
(*function FramebufferConsoleClearOld(Console:PConsoleDevice;Color:LongWord):LongWord;
{Implementation of ConsoleDeviceClear API for FramebufferConsole}
{Note: Not intended to be called directly by applications, use ConsoleDeviceClear instead}
var
 Data:TDMAData;
 Count:LongWord;
 Address:LongWord;
 EndAddress:LongWord;
 Framebuffer:PFramebufferDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Color}
 if Color = COLOR_NONE then Exit;
 
 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 if MutexLock(Console.Lock) = ERROR_SUCCESS then 
  begin
   try
    {Get Framebuffer}
    Framebuffer:=PFramebufferConsole(Console).Framebuffer;
    
    {Check Framebuffer}
    if Framebuffer = nil then Exit;
    if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 
    if Framebuffer.Address = 0 then Exit;

    {Lock Framebuffer}
    if MutexLock(Framebuffer.Lock) <> ERROR_SUCCESS then Exit;

    {Check Swap}
    if (Framebuffer.Device.DeviceFlags and FRAMEBUFFER_FLAG_SWAP) <> 0 then
     begin
      Color:=FramebufferDeviceSwap(Color);
     end;
     
    {Check DMA}
    if ((Console.Device.DeviceFlags and CONSOLE_FLAG_DMA_CLEAR) <> 0) and (DMAAvailable) and SysInitCompleted then
     begin
      {Check Buffer}
      if PFramebufferConsole(Console).FillBuffer = nil then
       begin
        {Get Size}
        PFramebufferConsole(Console).FillSize:=(Framebuffer.Depth shr 3);
      
        {Get Buffer}
        PFramebufferConsole(Console).FillBuffer:=DMAAllocateBufferEx(PFramebufferConsole(Console).FillSize);
       end;
       
      {Fill Source}
      Count:=0;
      while Count < PFramebufferConsole(Console).FillSize do
       begin
        PLongWord(PFramebufferConsole(Console).FillBuffer + Count)^:=Color;
        
        Inc(Count,(Framebuffer.Depth shr 3));
       end;
      
      {Check Cache}
      if not(DMA_CACHE_COHERENT) then
       begin
        {Clean Cache}
        CleanDataCacheRange(LongWord(PFramebufferConsole(Console).FillBuffer),PFramebufferConsole(Console).FillSize);
       end;
      
      {Create Data}
      FillChar(Data,SizeOf(TDMAData),0);
      Data.Source:=PFramebufferConsole(Console).FillBuffer;
      Data.Dest:=Pointer(Framebuffer.Address);
      Data.Flags:=DMA_DATA_FLAG_SOURCE_NOINCREMENT or DMA_DATA_FLAG_SOURCE_WIDE or DMA_DATA_FLAG_DEST_WIDE or DMA_DATA_FLAG_NOCLEAN or DMA_DATA_FLAG_NOINVALIDATE;
      Data.Size:=(Console.Height * Framebuffer.Pitch) + (Console.Width * (Framebuffer.Depth shr 3));
      
      {Perform Fill}
      DMATransfer(@Data,DMA_DIR_MEM_TO_MEM,DMA_DREQ_ID_NONE);
     end
    else
     begin    
      {Memory Barrier}
      DataMemoryBarrier;  {Before the First Write}

      {Get Address}   
      Address:=Framebuffer.Address; 
      EndAddress:=Framebuffer.Address + (Console.Height * Framebuffer.Pitch) + (Console.Width * (Framebuffer.Depth shr 3));
 
      {Clear Console}
      while (Address < EndAddress) do
       begin
        PLongWord(Address)^:=Color;
        
        Inc(Address,4);
       end;   
      
      {Check Cached}
      if (Framebuffer.Device.DeviceFlags and FRAMEBUFFER_FLAG_CACHED) <> 0 then
       begin
        {Clean Cache}
        CleanAndInvalidateDataCacheRange(Framebuffer.Address,Framebuffer.Size);
       end;
     end;
    
    {Unlock Framebuffer}
    MutexUnlock(Framebuffer.Lock);
    
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
end;*)

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
//To Do //Remove
(*function FramebufferConsoleScrollOld(Console:PConsoleDevice;X1,Y1,X2,Y2,Count,Direction:LongWord):LongWord; 
{Implementation of ConsoleDeviceScroll API for FramebufferConsole}
{Note: Not intended to be called directly by applications, use ConsoleDeviceScroll instead}
var
 Data:TDMAData;
 Next:PDMAData;
 Start:PDMAData;
 Lines:LongWord;
 Size:LongWord;
 Dest:LongWord;
 Source:LongWord;
 Buffer:Pointer;
 CurrentY:LongWord;
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
    if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 
    if Framebuffer.Address = 0 then Exit;
 
    {Lock Framebuffer}
    if MutexLock(Framebuffer.Lock) <> ERROR_SUCCESS then Exit;
    try
     {Check DMA}
     if ((Console.Device.DeviceFlags and CONSOLE_FLAG_DMA_SCROLL) <> 0) and (DMAAvailable) and SysInitCompleted then
      begin
       {Check Direction}
       case Direction of
        CONSOLE_DIRECTION_UP:begin
          {Check Count}
          if Count > Y1 then Exit;
      
          {Create Data}
          FillChar(Data,SizeOf(TDMAData),0);
          Data.Source:=Pointer(Framebuffer.Address + (Y1 * Framebuffer.Pitch) + (X1 * (Framebuffer.Depth shr 3)));
          Data.Dest:=Pointer(Framebuffer.Address + ((Y1 - Count) * Framebuffer.Pitch) + (X1 * (Framebuffer.Depth shr 3)));
          Data.Flags:=DMA_DATA_FLAG_STRIDE or DMA_DATA_FLAG_SOURCE_WIDE or DMA_DATA_FLAG_DEST_WIDE or DMA_DATA_FLAG_NOCLEAN or DMA_DATA_FLAG_NOINVALIDATE or DMA_DATA_FLAG_BULK;
          Data.StrideLength:=((X2 - X1) + 1) * (Framebuffer.Depth shr 3);
          Data.SourceStride:=Framebuffer.Pitch - Data.StrideLength;
          Data.DestStride:=Framebuffer.Pitch - Data.StrideLength;
          Data.Size:=Data.StrideLength * ((Y2 - Y1) + 1);
          
          {Check Stride}
          if Data.StrideLength < 1 then Exit;
          
          {Perform Copy}
          DMATransfer(@Data,DMA_DIR_MEM_TO_MEM,DMA_DREQ_ID_NONE);
         end;
        CONSOLE_DIRECTION_DOWN:begin
          {Check Count}
          if (Y2 + Count) >= Console.Height then Exit;
          
          {Create Data}
          FillChar(Data,SizeOf(TDMAData),0);
          Data.Source:=Pointer(Framebuffer.Address + (Y2 * Framebuffer.Pitch) + (X1 * (Framebuffer.Depth shr 3)));
          Data.Dest:=Pointer(Framebuffer.Address + ((Y2 + Count) * Framebuffer.Pitch) + (X1 * (Framebuffer.Depth shr 3)));
          Data.Flags:=DMA_DATA_FLAG_STRIDE or DMA_DATA_FLAG_SOURCE_WIDE or DMA_DATA_FLAG_DEST_WIDE or DMA_DATA_FLAG_NOCLEAN or DMA_DATA_FLAG_NOINVALIDATE or DMA_DATA_FLAG_BULK;
          Data.StrideLength:=((X2 - X1) + 1) * (Framebuffer.Depth shr 3);
          Data.SourceStride:=-(Framebuffer.Pitch + Data.StrideLength); {Negative stride}
          Data.DestStride:=-(Framebuffer.Pitch + Data.StrideLength); {Negative stride}
          Data.Size:=Data.StrideLength * ((Y2 - Y1) + 1);
          
          {Check Stride}
          if Data.StrideLength < 1 then Exit;
          
          {Perform Copy}
          DMATransfer(@Data,DMA_DIR_MEM_TO_MEM,DMA_DREQ_ID_NONE);
         end;
        CONSOLE_DIRECTION_LEFT:begin
          {Check Count}
          if Count > X1 then Exit;
          
          {Create Data}
          FillChar(Data,SizeOf(TDMAData),0);
          Data.Source:=Pointer(Framebuffer.Address + (Y1 * Framebuffer.Pitch) + (X1 * (Framebuffer.Depth shr 3)));
          Data.Dest:=Pointer(Framebuffer.Address + (Y1 * Framebuffer.Pitch) + ((X1 - Count) * (Framebuffer.Depth shr 3)));
          Data.Flags:=DMA_DATA_FLAG_STRIDE or DMA_DATA_FLAG_SOURCE_WIDE or DMA_DATA_FLAG_DEST_WIDE or DMA_DATA_FLAG_NOCLEAN or DMA_DATA_FLAG_NOINVALIDATE or DMA_DATA_FLAG_BULK;
          Data.StrideLength:=((X2 - X1) + 1) * (Framebuffer.Depth shr 3);
          Data.SourceStride:=Framebuffer.Pitch - Data.StrideLength;
          Data.DestStride:=Framebuffer.Pitch - Data.StrideLength;
          Data.Size:=Data.StrideLength * ((Y2 - Y1) + 1);
          
          {Check Stride}
          if Data.StrideLength < 1 then Exit;
          
          {Perform Copy}
          DMATransfer(@Data,DMA_DIR_MEM_TO_MEM,DMA_DREQ_ID_NONE);
         end;
        CONSOLE_DIRECTION_RIGHT:begin
          {Check Count}
          if (X2 + Count) >= Console.Width then Exit;
          
          {Get Size}
          Size:=((X2 - X1) + 1) * (Framebuffer.Depth shr 3);
          if Size < 1 then Exit;
          
          {Get Lines}
          Lines:=(SIZE_64K - SIZE_16K) div Size;
          
          {Check Buffer}
          if PFramebufferConsole(Console).ScrollBuffer = nil then
           begin
            {Get Buffer}
            PFramebufferConsole(Console).ScrollBuffer:=DMAAllocateBuffer(SIZE_64K);
           end;
          
          {Get Buffer}
          Buffer:=PFramebufferConsole(Console).ScrollBuffer;
          
          {Get Start}
          Start:=PDMAData(Buffer + (Size * Lines));
          Next:=Start;
          
          {Scroll Right}
          CurrentY:=Y1;
          while CurrentY <= Y2 do
           begin
            {Check Lines}
            if ((Y2 - CurrentY) + 1) < Lines then Lines:=((Y2 - CurrentY) + 1);
            
            {Create Data (To Buffer)}
            FillChar(Next^,SizeOf(TDMAData),0);
            Next.Source:=Pointer(Framebuffer.Address + (CurrentY * Framebuffer.Pitch) + (X1 * (Framebuffer.Depth shr 3)));
            Next.Dest:=Buffer;
            Next.Flags:=DMA_DATA_FLAG_STRIDE or DMA_DATA_FLAG_SOURCE_WIDE or DMA_DATA_FLAG_DEST_WIDE or DMA_DATA_FLAG_NOCLEAN or DMA_DATA_FLAG_NOINVALIDATE or DMA_DATA_FLAG_BULK;
            Next.StrideLength:=Size;
            Next.SourceStride:=Framebuffer.Pitch - Size;
            Next.DestStride:=0; //Framebuffer.Pitch - Size; //No stride on buffer
            Next.Size:=Size * Lines;
            Next.Next:=PDMAData(LongWord(Next) + SizeOf(TDMAData));
            
            {Get Next}
            Next:=Next.Next;
            
            {Create Data (From Buffer)}
            FillChar(Next^,SizeOf(TDMAData),0);
            Next.Source:=Buffer;
            Next.Dest:=Pointer(Framebuffer.Address + (CurrentY * Framebuffer.Pitch) + ((X1 + Count) * (Framebuffer.Depth shr 3)));
            Next.Flags:=DMA_DATA_FLAG_STRIDE or DMA_DATA_FLAG_SOURCE_WIDE or DMA_DATA_FLAG_DEST_WIDE or DMA_DATA_FLAG_NOCLEAN or DMA_DATA_FLAG_NOINVALIDATE or DMA_DATA_FLAG_BULK;
            Next.StrideLength:=Size;
            Next.SourceStride:=0; //Framebuffer.Pitch - Size; //No stride on buffer
            Next.DestStride:=Framebuffer.Pitch - Size;
            Next.Size:=Size * Lines;
            Next.Next:=PDMAData(LongWord(Next) + SizeOf(TDMAData));
            
            Inc(CurrentY,Lines);
            
            if CurrentY > Y2 then
             begin
              Next.Next:=nil;
             end
            else
             begin
              Next:=Next.Next;
             end;
           end;
          
          {Perform Copy}
          DMATransfer(Start,DMA_DIR_MEM_TO_MEM,DMA_DREQ_ID_NONE);
         end;
       end;
      end
     else
      begin    
       {Memory Barrier}
       DataMemoryBarrier;  {Before the First Write}
       
       {Check Direction}
       case Direction of
        CONSOLE_DIRECTION_UP:begin
          {Check Count}
          if Count > Y1 then Exit;
          
          {Get Size}
          Size:=((X2 - X1) + 1) * (Framebuffer.Depth shr 3);
          if Size < 1 then Exit;
          
          {Scroll Up}
          CurrentY:=Y1;
          while CurrentY <= Y2 do
           begin
            {Get Source}
            Source:=(Framebuffer.Address + (CurrentY * Framebuffer.Pitch) + (X1 * (Framebuffer.Depth shr 3)));
           
            {Get Dest}
            Dest:=(Framebuffer.Address + ((CurrentY - Count) * Framebuffer.Pitch) + (X1 * (Framebuffer.Depth shr 3)));
            
            {Copy Pixels}
            System.Move(PLongWord(Source)^,PLongWord(Dest)^,Size);
            
            {Check Cached}
            if (Framebuffer.Device.DeviceFlags and FRAMEBUFFER_FLAG_CACHED) <> 0 then
             begin
              {Clean Cache}
              CleanAndInvalidateDataCacheRange(Dest,Size); 
             end; 
            
            Inc(CurrentY);
           end;
         end;
        CONSOLE_DIRECTION_DOWN:begin
          {Check Count}
          if (Y2 + Count) >= Console.Height then Exit;
 
          {Get Size}
          Size:=((X2 - X1) + 1) * (Framebuffer.Depth shr 3);
          if Size < 1 then Exit;
          
          {Scroll Down}
          CurrentY:=Y2;
          while CurrentY >= Y1 do
           begin
            {Get Source}
            Source:=(Framebuffer.Address + (CurrentY * Framebuffer.Pitch) + (X1 * (Framebuffer.Depth shr 3)));
 
            {Get Dest}
            Dest:=(Framebuffer.Address + ((CurrentY + Count) * Framebuffer.Pitch) + (X1 * (Framebuffer.Depth shr 3)));
 
            {Copy Pixels}
            System.Move(PLongWord(Source)^,PLongWord(Dest)^,Size);
            
            {Check Cached}
            if (Framebuffer.Device.DeviceFlags and FRAMEBUFFER_FLAG_CACHED) <> 0 then
             begin
              {Clean Cache}
              CleanAndInvalidateDataCacheRange(Dest,Size);
             end; 
            
            Dec(CurrentY);
           end;
         end;
        CONSOLE_DIRECTION_LEFT:begin
          {Check Count}
          if Count > X1 then Exit;
          
          {Get Size}
          Size:=((X2 - X1) + 1) * (Framebuffer.Depth shr 3);
          if Size < 1 then Exit;
          
          {Allocate Buffer}
          Buffer:=GetMem(Size);
          if Buffer = nil then Exit;
          try
           {Scroll Left}
           CurrentY:=Y1;
           while CurrentY <= Y2 do
            begin
             {Get Source}
             Source:=(Framebuffer.Address + (CurrentY * Framebuffer.Pitch) + (X1 * (Framebuffer.Depth shr 3)));
            
             {Get Dest}
             Dest:=(Framebuffer.Address + (CurrentY * Framebuffer.Pitch) + ((X1 - Count) * (Framebuffer.Depth shr 3)));
             
             {Copy Pixels to Buffer}
             System.Move(PLongWord(Source)^,PLongWord(Buffer)^,Size);
             
             {Copy Buffer to Pixels}
             System.Move(PLongWord(Buffer)^,PLongWord(Dest)^,Size);
            
             {Check Cached}
             if (Framebuffer.Device.DeviceFlags and FRAMEBUFFER_FLAG_CACHED) <> 0 then
              begin
               {Clean Cache}
               CleanAndInvalidateDataCacheRange(Dest,Size); 
              end; 
            
             Inc(CurrentY);
            end;
          finally
           FreeMem(Buffer);
          end;         
         end;
        CONSOLE_DIRECTION_RIGHT:begin
          {Check Count}
          if (X2 + Count) >= Console.Width then Exit;
 
          {Get Size}
          Size:=((X2 - X1) + 1) * (Framebuffer.Depth shr 3);
          if Size < 1 then Exit;
          
          {Allocate Buffer}
          Buffer:=GetMem(Size);
          if Buffer = nil then Exit;
          try
           {Scroll Right}
           CurrentY:=Y1;
           while CurrentY <= Y2 do
            begin
             {Get Source}
             Source:=(Framebuffer.Address + (CurrentY * Framebuffer.Pitch) + (X1 * (Framebuffer.Depth shr 3)));
            
             {Get Dest}
             Dest:=(Framebuffer.Address + (CurrentY * Framebuffer.Pitch) + ((X1 + Count) * (Framebuffer.Depth shr 3)));
             
             {Copy Pixels to Buffer}
             System.Move(PLongWord(Source)^,PLongWord(Buffer)^,Size);
             
             {Copy Buffer to Pixels}
             System.Move(PLongWord(Buffer)^,PLongWord(Dest)^,Size);
             
             {Check Cached}
             if (Framebuffer.Device.DeviceFlags and FRAMEBUFFER_FLAG_CACHED) <> 0 then
              begin
               {Clean Cache}
               CleanAndInvalidateDataCacheRange(Dest,Size); 
              end; 
             
             Inc(CurrentY);
            end;
          finally
           FreeMem(Buffer);
          end;         
         end;
       end;
      end; 
     
     {Update Statistics}
     Inc(Console.ScrollCount);
     
     {Get Result}
     Result:=ERROR_SUCCESS;
    finally
     {Unlock Framebuffer}
     MutexUnlock(Framebuffer.Lock);
    end; 
   finally
    MutexUnlock(Console.Lock);
   end; 
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;*)

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
 if Color = COLOR_NONE then Exit;
 
 {Check Width}
 if Width < 1 then Exit;
 
 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
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
//To Do //Remove
(*function FramebufferConsoleDrawBoxOld(Console:PConsoleDevice;X1,Y1,X2,Y2,Color,Width:LongWord):LongWord;
{Implementation of ConsoleDeviceDrawBox API for FramebufferConsole}
{Note: Not intended to be called directly by applications, use ConsoleDeviceDrawBox instead}
var
 Size:LongWord;
 Count:LongWord;
 TopData:TDMAData;
 LeftData:TDMAData;
 RightData:TDMAData;
 BottomData:TDMAData;
 Address:LongWord;
 CurrentX:LongWord;
 CurrentY:LongWord;
 Framebuffer:PFramebufferDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check X1, Y1, X2, Y2 (Must be a box}
 if X1 >= X2 then Exit;
 if Y1 >= Y2 then Exit;
 
 {Check Color}
 if Color = COLOR_NONE then Exit;
 
 {Check Width}
 if Width < 1 then Exit;
 
 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
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
    if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 
    if Framebuffer.Address = 0 then Exit;
 
    {Lock Framebuffer}
    if MutexLock(Framebuffer.Lock) <> ERROR_SUCCESS then Exit;
   
    {Check Swap}
    if (Framebuffer.Device.DeviceFlags and FRAMEBUFFER_FLAG_SWAP) <> 0 then
     begin
      Color:=FramebufferDeviceSwap(Color);
     end;
   
    {Check DMA}
    if ((Console.Device.DeviceFlags and CONSOLE_FLAG_DMA_BOX) <> 0) and (DMAAvailable) and SysInitCompleted then
     begin
      {Check Buffer}
      if PFramebufferConsole(Console).FillBuffer = nil then
       begin
        {Get Size}
        PFramebufferConsole(Console).FillSize:=(Framebuffer.Depth shr 3);
      
        {Get Buffer}
        PFramebufferConsole(Console).FillBuffer:=DMAAllocateBufferEx(PFramebufferConsole(Console).FillSize);
       end;
      
      {Fill Source}
      Count:=0;
      while Count < PFramebufferConsole(Console).FillSize do
       begin
        PLongWord(PFramebufferConsole(Console).FillBuffer + Count)^:=Color;
        
        Inc(Count,(Framebuffer.Depth shr 3));
       end;
      
      {Check Cache}
      if not(DMA_CACHE_COHERENT) then
       begin
        {Clean Cache}
        CleanDataCacheRange(LongWord(PFramebufferConsole(Console).FillBuffer),PFramebufferConsole(Console).FillSize);
       end;
    
      {Draw Top}
      {Create Data}
      FillChar(TopData,SizeOf(TDMAData),0);
      TopData.Source:=PFramebufferConsole(Console).FillBuffer;
      TopData.Dest:=Pointer(Framebuffer.Address + (Y1 * Framebuffer.Pitch) + (X1 * (Framebuffer.Depth shr 3)));
      TopData.Flags:=DMA_DATA_FLAG_STRIDE or DMA_DATA_FLAG_SOURCE_NOINCREMENT or DMA_DATA_FLAG_SOURCE_WIDE or DMA_DATA_FLAG_DEST_WIDE or DMA_DATA_FLAG_NOCLEAN or DMA_DATA_FLAG_NOINVALIDATE;
      TopData.StrideLength:=((X2 - X1) + 1) * (Framebuffer.Depth shr 3);
      TopData.SourceStride:=0;
      TopData.DestStride:=Framebuffer.Pitch - TopData.StrideLength;
      TopData.Size:=TopData.StrideLength * Width; {Not + 1}
      TopData.Next:=@BottomData;
      
      {Draw Bottom}
      {Create Data}
      FillChar(BottomData,SizeOf(TDMAData),0);
      BottomData.Source:=PFramebufferConsole(Console).FillBuffer;
      BottomData.Dest:=Pointer(Framebuffer.Address + (((Y2 - Width) + 1) * Framebuffer.Pitch) + (X1 * (Framebuffer.Depth shr 3)));
      BottomData.Flags:=DMA_DATA_FLAG_STRIDE or DMA_DATA_FLAG_SOURCE_NOINCREMENT or DMA_DATA_FLAG_SOURCE_WIDE or DMA_DATA_FLAG_DEST_WIDE or DMA_DATA_FLAG_NOCLEAN or DMA_DATA_FLAG_NOINVALIDATE;
      BottomData.StrideLength:=((X2 - X1) + 1) * (Framebuffer.Depth shr 3);
      BottomData.SourceStride:=0;
      BottomData.DestStride:=Framebuffer.Pitch - BottomData.StrideLength;
      BottomData.Size:=BottomData.StrideLength * Width; {Not + 1}
      BottomData.Next:=@LeftData;
      
      {Draw Left}
      {Create Data}
      FillChar(LeftData,SizeOf(TDMAData),0);
      LeftData.Source:=PFramebufferConsole(Console).FillBuffer;
      LeftData.Dest:=Pointer(Framebuffer.Address + (Y1 * Framebuffer.Pitch) + (X1 * (Framebuffer.Depth shr 3)));
      LeftData.Flags:=DMA_DATA_FLAG_STRIDE or DMA_DATA_FLAG_SOURCE_NOINCREMENT or DMA_DATA_FLAG_SOURCE_WIDE or DMA_DATA_FLAG_DEST_WIDE or DMA_DATA_FLAG_NOCLEAN or DMA_DATA_FLAG_NOINVALIDATE;
      LeftData.StrideLength:=Width * (Framebuffer.Depth shr 3);
      LeftData.SourceStride:=0;
      LeftData.DestStride:=Framebuffer.Pitch - LeftData.StrideLength;
      LeftData.Size:=LeftData.StrideLength * ((Y2 - Y1) + 1);
      LeftData.Next:=@RightData;
      
      {Draw Right}
      {Create Data}
      FillChar(RightData,SizeOf(TDMAData),0);
      RightData.Source:=PFramebufferConsole(Console).FillBuffer;
      RightData.Dest:=Pointer(Framebuffer.Address + (Y1 * Framebuffer.Pitch) + (((X2 - Width) + 1) * (Framebuffer.Depth shr 3)));
      RightData.Flags:=DMA_DATA_FLAG_STRIDE or DMA_DATA_FLAG_SOURCE_NOINCREMENT or DMA_DATA_FLAG_SOURCE_WIDE or DMA_DATA_FLAG_DEST_WIDE or DMA_DATA_FLAG_NOCLEAN or DMA_DATA_FLAG_NOINVALIDATE;
      RightData.StrideLength:=Width * (Framebuffer.Depth shr 3);
      RightData.SourceStride:=0;
      RightData.DestStride:=Framebuffer.Pitch - RightData.StrideLength;
      RightData.Size:=RightData.StrideLength * ((Y2 - Y1) + 1);
      
      {Perform Fill}
      DMATransfer(@TopData,DMA_DIR_MEM_TO_MEM,DMA_DREQ_ID_NONE);
     end
    else
     begin    
      {Memory Barrier}
      DataMemoryBarrier;  {Before the First Write}
      
      {Draw Top / Bottom}
      CurrentX:=X1;
      while CurrentX <= X2 do
       begin
        for Count:=0 to Width - 1 do
         begin
          {Get Address Y1}
          Address:=(Framebuffer.Address + ((Y1 + Count) * Framebuffer.Pitch) + (CurrentX * (Framebuffer.Depth shr 3)));
          
          {Write Pixel Y1}
          PLongWord(Address)^:=Color;
      
          {Get Address Y2}
          Address:=(Framebuffer.Address + ((Y2 - Count) * Framebuffer.Pitch) + (CurrentX * (Framebuffer.Depth shr 3)));
          
          {Write Pixel Y2}
          PLongWord(Address)^:=Color;
         end;
         
        Inc(CurrentX);
       end;
      
      {Draw Left / Right}
      CurrentY:=Y1;
      while CurrentY <= Y2 do
       begin
        for Count:=0 to Width - 1 do
         begin
          {Get Address X1}
          Address:=(Framebuffer.Address + (CurrentY * Framebuffer.Pitch) + ((X1 + Count) * (Framebuffer.Depth shr 3)));
          
          {Write Pixel X1}
          PLongWord(Address)^:=Color;
      
          {Get Address X2}
          Address:=(Framebuffer.Address + (CurrentY * Framebuffer.Pitch) + ((X2 - Count) * (Framebuffer.Depth shr 3)));
          
          {Write Pixel X2}
          PLongWord(Address)^:=Color;
         end;
         
        Inc(CurrentY);
       end;
      
      {Check Cached}
      if (Framebuffer.Device.DeviceFlags and FRAMEBUFFER_FLAG_CACHED) <> 0 then
       begin
        {Get Address}
        Address:=(Framebuffer.Address + (Y1 * Framebuffer.Pitch) + (X1 * (Framebuffer.Depth shr 3))); //To Do //See DrawBlock
        
        {Get Size}
        Size:=((Y2 - Y1) + 1) * Framebuffer.Pitch; //To Do //See DrawBlock
        
        {Clean Cache}
        CleanAndInvalidateDataCacheRange(Address,Size);
       end;
     end;
    
    {Unlock Framebuffer}
    MutexUnlock(Framebuffer.Lock);
    
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
end;*)

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
 if (X1 <> X2) and (Y1 <> Y2) then Exit; {This would be a box}
 
 {Check Color}
 if Color = COLOR_NONE then Exit;
 
 {Check Width}
 if Width < 1 then Exit;
 
 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
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
//To Do //Remove
(*function FramebufferConsoleDrawLineOld(Console:PConsoleDevice;X1,Y1,X2,Y2,Color,Width:LongWord):LongWord;
{Implementation of ConsoleDeviceDrawLine API for FramebufferConsole}
{Note: Not intended to be called directly by applications, use ConsoleDeviceDrawLine instead}
var
 Size:LongWord;
 Data:TDMAData;
 Count:LongWord;
 Address:LongWord;
 CurrentX:LongWord;
 CurrentY:LongWord;
 Framebuffer:PFramebufferDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check X1, Y1, X2, Y2 (Must be a line)}
 if X1 > X2 then Exit;
 if Y1 > Y2 then Exit;
 if (X1 = X2) and (Y1 = Y2) then Exit;   {This would be a pixel}
 if (X1 <> X2) and (Y1 <> Y2) then Exit; {This would be a box}
 
 {Check Color}
 if Color = COLOR_NONE then Exit;
 
 {Check Width}
 if Width < 1 then Exit;
 
 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
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
    if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 
    if Framebuffer.Address = 0 then Exit;
 
    {Lock Framebuffer}
    if MutexLock(Framebuffer.Lock) <> ERROR_SUCCESS then Exit;
   
    {Check Swap}
    if (Framebuffer.Device.DeviceFlags and FRAMEBUFFER_FLAG_SWAP) <> 0 then
     begin
      Color:=FramebufferDeviceSwap(Color);
     end;
   
    {Check DMA}
    if ((Console.Device.DeviceFlags and CONSOLE_FLAG_DMA_LINE) <> 0) and (DMAAvailable) and SysInitCompleted then
     begin
      {Get Direction}
      if X1 = X2 then
       begin
        {Vertical}
        {Check Buffer}
        if PFramebufferConsole(Console).FillBuffer = nil then
         begin
          {Get Size}
          PFramebufferConsole(Console).FillSize:=(Framebuffer.Depth shr 3);
        
          {Get Buffer}
          PFramebufferConsole(Console).FillBuffer:=DMAAllocateBufferEx(PFramebufferConsole(Console).FillSize);
         end;
        
        {Fill Source}
        Count:=0;
        while Count < PFramebufferConsole(Console).FillSize do
         begin
          PLongWord(PFramebufferConsole(Console).FillBuffer + Count)^:=Color;
          
          Inc(Count,(Framebuffer.Depth shr 3));
         end;
        
        {Check Cache}
        if not(DMA_CACHE_COHERENT) then
         begin
          {Clean Cache}
          CleanDataCacheRange(LongWord(PFramebufferConsole(Console).FillBuffer),PFramebufferConsole(Console).FillSize);
         end;
        
        {Create Data}
        FillChar(Data,SizeOf(TDMAData),0);
        Data.Source:=PFramebufferConsole(Console).FillBuffer;
        Data.Dest:=Pointer(Framebuffer.Address + (Y1 * Framebuffer.Pitch) + (X1 * (Framebuffer.Depth shr 3)));
        Data.Flags:=DMA_DATA_FLAG_STRIDE or DMA_DATA_FLAG_SOURCE_NOINCREMENT or DMA_DATA_FLAG_SOURCE_WIDE or DMA_DATA_FLAG_DEST_WIDE or DMA_DATA_FLAG_NOCLEAN or DMA_DATA_FLAG_NOINVALIDATE;
        Data.StrideLength:=Width * (Framebuffer.Depth shr 3);
        Data.SourceStride:=0;
        Data.DestStride:=Framebuffer.Pitch - Data.StrideLength;
        Data.Size:=Data.StrideLength * ((Y2 - Y1) + 1);
        
        {Perform Fill}
        DMATransfer(@Data,DMA_DIR_MEM_TO_MEM,DMA_DREQ_ID_NONE);
       end
      else if Y1 = Y2 then 
       begin
        {Horizontal}
        {Check Buffer}
        if PFramebufferConsole(Console).FillBuffer = nil then
         begin
          {Get Size}
          PFramebufferConsole(Console).FillSize:=(Framebuffer.Depth shr 3);
        
          {Get Buffer}
          PFramebufferConsole(Console).FillBuffer:=DMAAllocateBufferEx(PFramebufferConsole(Console).FillSize);
         end;
        
        {Fill Source}
        Count:=0;
        while Count < PFramebufferConsole(Console).FillSize do
         begin
          PLongWord(PFramebufferConsole(Console).FillBuffer + Count)^:=Color;
          
          Inc(Count,(Framebuffer.Depth shr 3));
         end;
        
        {Check Cache}
        if not(DMA_CACHE_COHERENT) then
         begin
          {Clean Cache}
          CleanDataCacheRange(LongWord(PFramebufferConsole(Console).FillBuffer),PFramebufferConsole(Console).FillSize);
         end;
        
        {Create Data}
        FillChar(Data,SizeOf(TDMAData),0);
        Data.Source:=PFramebufferConsole(Console).FillBuffer;
        Data.Dest:=Pointer(Framebuffer.Address + (Y1 * Framebuffer.Pitch) + (X1 * (Framebuffer.Depth shr 3)));
        Data.Flags:=DMA_DATA_FLAG_STRIDE or DMA_DATA_FLAG_SOURCE_NOINCREMENT or DMA_DATA_FLAG_SOURCE_WIDE or DMA_DATA_FLAG_DEST_WIDE or DMA_DATA_FLAG_NOCLEAN or DMA_DATA_FLAG_NOINVALIDATE;
        Data.StrideLength:=((X2 - X1) + 1) * (Framebuffer.Depth shr 3);
        Data.SourceStride:=0;
        Data.DestStride:=Framebuffer.Pitch - Data.StrideLength;
        Data.Size:=Data.StrideLength * Width; {Not + 1}
        
        {Perform Fill}
        DMATransfer(@Data,DMA_DIR_MEM_TO_MEM,DMA_DREQ_ID_NONE);
       end;
     end
    else
     begin    
      {Memory Barrier}
      DataMemoryBarrier;  {Before the First Write}
      
      {Get Direction}
      if X1 = X2 then
       begin
        {Vertical}
        CurrentY:=Y1;
        while CurrentY <= Y2 do
         begin
          for Count:=0 to Width - 1 do
           begin
            {Get Address}
            Address:=(Framebuffer.Address + (CurrentY * Framebuffer.Pitch) + ((X1 + Count) * (Framebuffer.Depth shr 3)));
            
            {Write Pixel}
            PLongWord(Address)^:=Color;
           end;
         
          Inc(CurrentY);
         end;
         
        {Check Cached}
        if (Framebuffer.Device.DeviceFlags and FRAMEBUFFER_FLAG_CACHED) <> 0 then
         begin
          {Get Address}
          Address:=(Framebuffer.Address + (Y1 * Framebuffer.Pitch) + (X1 * (Framebuffer.Depth shr 3))); //To Do //See DrawBlock
      
          {Get Size}
          Size:=((Y2 - Y1) + 1) * Framebuffer.Pitch; //To Do //See DrawBlock
      
          {Clean Cache}
          CleanAndInvalidateDataCacheRange(Address,Size);
         end;
       end
      else if Y1 = Y2 then 
       begin
        {Horizontal}
        CurrentX:=X1;
        while CurrentX <= X2 do
         begin
          for Count:=0 to Width - 1 do
           begin
            {Get Address}
            Address:=(Framebuffer.Address + ((Y1 + Count) * Framebuffer.Pitch) + (CurrentX * (Framebuffer.Depth shr 3)));
            
            {Write Pixel}
            PLongWord(Address)^:=Color;
           end;
           
          Inc(CurrentX);
         end;
         
        {Check Cached}
        if (Framebuffer.Device.DeviceFlags and FRAMEBUFFER_FLAG_CACHED) <> 0 then
         begin
          {Get Address}
          Address:=(Framebuffer.Address + (Y1 * Framebuffer.Pitch) + (X1 * (Framebuffer.Depth shr 3))); //To Do //See DrawBlock
          
          {Get Size}
          Size:=((Y2 - Y1) + Width) * Framebuffer.Pitch; //To Do //See DrawBlock
          
          {Clean Cache}
          CleanAndInvalidateDataCacheRange(Address,Size);
         end;
       end;
     end;
     
    {Unlock Framebuffer}
    MutexUnlock(Framebuffer.Lock);
    
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
end;*)

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
 if Forecolor = COLOR_NONE then Exit;
 if Backcolor = COLOR_NONE then Exit;
 
 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
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
//To Do //Remove
(*function FramebufferConsoleDrawCharOld(Console:PConsoleDevice;Handle:TFontHandle;Ch:Char;X,Y,Forecolor,Backcolor:LongWord):LongWord; 
{Implementation of ConsoleDeviceDrawChar API for FramebufferConsole}
{Note: Not intended to be called directly by applications, use ConsoleDeviceDrawChar instead}
var
 Row:LongWord;
 Column:LongWord;
 Address:LongWord;
 CurrentY:LongWord;
 Character:LongWord;
 Data:array[0..FONT_MAX_WIDTH - 1] of LongWord;
 
 Font:PFontEntry;
 Framebuffer:PFramebufferDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;
 
 {Check Colors}
 if Forecolor = COLOR_NONE then Exit;
 if Backcolor = COLOR_NONE then Exit;
 
 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 if MutexLock(Console.Lock) = ERROR_SUCCESS then 
  begin
   try
    {Get Font}
    Font:=PFontEntry(Handle);
    if Font = nil then Exit;
    if Font.Signature <> FONT_SIGNATURE then Exit;
    
    {Get Framebuffer}
    Framebuffer:=PFramebufferConsole(Console).Framebuffer;
    
    {Check Framebuffer}
    if Framebuffer = nil then Exit;
    if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 
    if Framebuffer.Address = 0 then Exit;
 
    {Lock Framebuffer}
    if MutexLock(Framebuffer.Lock) <> ERROR_SUCCESS then Exit;

    {Check Swap}
    if (Framebuffer.Device.DeviceFlags and FRAMEBUFFER_FLAG_SWAP) <> 0 then
     begin
      Forecolor:=FramebufferDeviceSwap(Forecolor);
      Backcolor:=FramebufferDeviceSwap(Backcolor);
     end;
    
    {Memory Barrier}
    DataMemoryBarrier; {Before the First Write}
    
    {Get Rows}
    CurrentY:=Y;
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
      for Column:=Font.CharWidth - 1 downto 0 do
       begin
        if (Character and $01) = $01 then
         begin
          Data[Column]:=Forecolor;
         end
        else
         begin
          Data[Column]:=Backcolor;
         end;  
         
        Character:=Character shr 1;
       end;
      
      {Draw Character}
      if X <= (Console.Width - Font.CharWidth) then //To Do //Less than only, starts at 0 ?
       begin
        if CurrentY <= Console.Height then //To Do //Less than only, starts at 0 ?
         begin
          {Get Address}
          Address:=(Framebuffer.Address + (CurrentY * Framebuffer.Pitch) + (X * (Framebuffer.Depth shr 3)));
          
          {Write Pixels}
          System.Move(Data[0],PLongWord(Address)^,Font.CharWidth * SizeOf(LongWord));
          
          {Check Cached}
          if (Framebuffer.Device.DeviceFlags and FRAMEBUFFER_FLAG_CACHED) <> 0 then
           begin
            {Clean Cache}
            CleanAndInvalidateDataCacheRange(Address,Font.CharWidth * SizeOf(LongWord));
           end;
         end;
       end;
       
      Inc(CurrentY); 
     end;
    
    {Unlock Framebuffer}
    MutexUnlock(Framebuffer.Lock);
    
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
end;*)

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
 if Forecolor = COLOR_NONE then Exit;
 if Backcolor = COLOR_NONE then Exit;
 
 {Check Length}
 if Len < 1 then Exit;
 if Len > Length(Text) then Len:=Length(Text);
 
 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
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
//To Do //Remove
(*function FramebufferConsoleDrawTextOld(Console:PConsoleDevice;Handle:TFontHandle;const Text:String;X,Y,Forecolor,Backcolor,Len:LongWord):LongWord; 
{Implementation of ConsoleDeviceDrawText API for FramebufferConsole}
{Note: Not intended to be called directly by applications, use ConsoleDeviceDrawText instead}
var
 Row:LongWord;
 Count:LongWord;
 Column:LongWord;
 Address:LongWord;
 CurrentX:LongWord;
 CurrentY:LongWord;
 Character:LongWord;
 Data:array[0..FONT_MAX_WIDTH - 1] of LongWord;

 Font:PFontEntry;
 Framebuffer:PFramebufferDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;
 
 {Check Text}
 if Length(Text) = 0 then Exit;
 
 {Check Colors}
 if Forecolor = COLOR_NONE then Exit;
 if Backcolor = COLOR_NONE then Exit;
 
 {Check Length}
 if Len < 1 then Exit;
 if Len > Length(Text) then Len:=Length(Text);
 
 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 if MutexLock(Console.Lock) = ERROR_SUCCESS then 
  begin
   try
    {Get Font}
    Font:=PFontEntry(Handle);
    if Font = nil then Exit;
    if Font.Signature <> FONT_SIGNATURE then Exit;
    
    {Get Framebuffer}
    Framebuffer:=PFramebufferConsole(Console).Framebuffer;
    
    {Check Framebuffer}
    if Framebuffer = nil then Exit;
    if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 
    if Framebuffer.Address = 0 then Exit;
 
    {Lock Framebuffer}
    if MutexLock(Framebuffer.Lock) <> ERROR_SUCCESS then Exit;

    {Check Swap}
    if (Framebuffer.Device.DeviceFlags and FRAMEBUFFER_FLAG_SWAP) <> 0 then
     begin
      Forecolor:=FramebufferDeviceSwap(Forecolor);
      Backcolor:=FramebufferDeviceSwap(Backcolor);
     end;
    
    {Memory Barrier}
    DataMemoryBarrier; {Before the First Write}
    
    {Get Text}
    CurrentX:=X;
    for Count:=1 to Len do
     begin
      {Get Rows}
      CurrentY:=Y;
      for Row:=0 to Font.CharHeight - 1 do
       begin
        {Get Character}
        Character:=0;
        case Font.CharWidth of
         8:Character:=PFontChars8(Font.CharData)[(Byte(Text[Count]) * Font.CharHeight) + Row];
         9..16:Character:=PFontChars16(Font.CharData)[(Byte(Text[Count]) * Font.CharHeight) + Row];
         17..32:Character:=PFontChars32(Font.CharData)[(Byte(Text[Count]) * Font.CharHeight) + Row];
        end;
        
        {Map Character}
        for Column:=Font.CharWidth - 1 downto 0 do
         begin
          if (Character and $01) = $01 then
           begin
            Data[Column]:=Forecolor;
           end
          else
           begin
            Data[Column]:=Backcolor;
           end;
           
          Character:=Character shr 1;
         end;
        
        {Draw Character}
        if CurrentX <= (Console.Width - Font.CharWidth) then //To Do //Less than only, starts at 0 ?
         begin
          if CurrentY <= Console.Height then //To Do //Less than only, starts at 0 ?
           begin
            {Get Address}
            Address:=(Framebuffer.Address + (CurrentY * Framebuffer.Pitch) + (CurrentX * (Framebuffer.Depth shr 3)));
            
            {Write Pixels}
            System.Move(Data[0],PLongWord(Address)^,Font.CharWidth * SizeOf(LongWord));
            
            {Check Cached}
            if (Framebuffer.Device.DeviceFlags and FRAMEBUFFER_FLAG_CACHED) <> 0 then
             begin
              {Clean Cache}
              CleanAndInvalidateDataCacheRange(Address,Font.CharWidth * SizeOf(LongWord));
             end;
           end;
         end;
         
        Inc(CurrentY); 
       end;
       
      Inc(CurrentX,Font.CharWidth);
     end;
 
    {Unlock Framebuffer}
    MutexUnlock(Framebuffer.Lock);
    
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
end;*)

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
 if Color = COLOR_NONE then Exit;
 
 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
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
        Framebuffer.DeviceCommit(Framebuffer,LongWord(Address),ColorFormatToBytes(Console.Format),FRAMEBUFFER_TRANSFER_NONE);
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
//To Do //Remove
(*function FramebufferConsoleDrawPixelOld(Console:PConsoleDevice;X,Y,Color:LongWord):LongWord;
{Implementation of ConsoleDeviceDrawPixel API for FramebufferConsole}
{Note: Not intended to be called directly by applications, use ConsoleDeviceDrawPixel instead}
var
 Address:LongWord;
 Framebuffer:PFramebufferDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Color}
 if Color = COLOR_NONE then Exit;
 
 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
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
    if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 
    if Framebuffer.Address = 0 then Exit;
 
    {Lock Framebuffer}
    if MutexLock(Framebuffer.Lock) <> ERROR_SUCCESS then Exit;
 
    {Check Swap}
    if (Framebuffer.Device.DeviceFlags and FRAMEBUFFER_FLAG_SWAP) <> 0 then
     begin
      Color:=FramebufferDeviceSwap(Color);
     end;
 
    {Memory Barrier}
    DataMemoryBarrier;  {Before the First Write}

    {Get Address}
    Address:=(Framebuffer.Address + (Y * Framebuffer.Pitch) + (X * (Framebuffer.Depth shr 3)));
 
    {Write Pixel}
    PLongWord(Address)^:=Color;
    
    {Check Cached}
    if (Framebuffer.Device.DeviceFlags and FRAMEBUFFER_FLAG_CACHED) <> 0 then
     begin
      {Clean Cache}
      CleanAndInvalidateDataCacheRange(Address,SizeOf(LongWord));
     end;
    
    {Unlock Framebuffer}
    MutexUnlock(Framebuffer.Lock);
    
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
end;*)

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
 if Color = COLOR_NONE then Exit;
 
 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
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
//To Do //Remove
(*function FramebufferConsoleDrawBlockOld(Console:PConsoleDevice;X1,Y1,X2,Y2,Color:LongWord):LongWord; 
{Implementation of ConsoleDeviceDrawBlock API for FramebufferConsole}
{Note: Not intended to be called directly by applications, use ConsoleDeviceDrawBlock instead}
var
 Size:LongWord;
 Data:TDMAData;
 Count:LongWord;
 Address:LongWord;
 CurrentX:LongWord;
 CurrentY:LongWord;
 Framebuffer:PFramebufferDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check X1, Y1, X2, Y2 (Must be a box)}
 if X1 >= X2 then Exit;
 if Y1 >= Y2 then Exit;
 
 {Check Color}
 if Color = COLOR_NONE then Exit;
 
 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
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
    if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 
    if Framebuffer.Address = 0 then Exit;
 
    {Lock Framebuffer}
    if MutexLock(Framebuffer.Lock) <> ERROR_SUCCESS then Exit;

    {Check Swap}
    if (Framebuffer.Device.DeviceFlags and FRAMEBUFFER_FLAG_SWAP) <> 0 then
     begin
      Color:=FramebufferDeviceSwap(Color);
     end;
    
    {Check DMA}
    if ((Console.Device.DeviceFlags and CONSOLE_FLAG_DMA_FILL) <> 0) and (DMAAvailable) and SysInitCompleted then
     begin
      {Check Buffer}
      if PFramebufferConsole(Console).FillBuffer = nil then
       begin
        {Get Size}
        PFramebufferConsole(Console).FillSize:=(Framebuffer.Depth shr 3);
      
        {Get Buffer}
        PFramebufferConsole(Console).FillBuffer:=DMAAllocateBufferEx(PFramebufferConsole(Console).FillSize);
       end;
       
      {Fill Source}
      Count:=0;
      while Count < PFramebufferConsole(Console).FillSize do
       begin
        PLongWord(PFramebufferConsole(Console).FillBuffer + Count)^:=Color;
        
        Inc(Count,(Framebuffer.Depth shr 3));
       end;
      
      {Check Cached}
      if not(DMA_CACHE_COHERENT) then
       begin
        {Clean Cache}
        CleanDataCacheRange(LongWord(PFramebufferConsole(Console).FillBuffer),PFramebufferConsole(Console).FillSize);
       end;
      
      {Create Data}
      FillChar(Data,SizeOf(TDMAData),0);
      Data.Source:=PFramebufferConsole(Console).FillBuffer;
      Data.Dest:=Pointer(Framebuffer.Address + (Y1 * Framebuffer.Pitch) + (X1 * (Framebuffer.Depth shr 3)));
      Data.Flags:=DMA_DATA_FLAG_STRIDE or DMA_DATA_FLAG_SOURCE_NOINCREMENT or DMA_DATA_FLAG_SOURCE_WIDE or DMA_DATA_FLAG_DEST_WIDE or DMA_DATA_FLAG_NOCLEAN or DMA_DATA_FLAG_NOINVALIDATE;
      Data.StrideLength:=((X2 - X1) + 1) * (Framebuffer.Depth shr 3);
      Data.SourceStride:=0;
      Data.DestStride:=Framebuffer.Pitch - Data.StrideLength;
      Data.Size:=Data.StrideLength * ((Y2 - Y1) + 1);
      
      {Perform Fill}
      DMATransfer(@Data,DMA_DIR_MEM_TO_MEM,DMA_DREQ_ID_NONE);
     end
    else
     begin    
      {Memory Barrier}
      DataMemoryBarrier;  {Before the First Write}
   
      {Draw Block}
      CurrentX:=X1;
      while CurrentX <= X2 do
       begin
        CurrentY:=Y1;
        
        while CurrentY <= Y2 do
         begin
          {Get Address}
          Address:=(Framebuffer.Address + (CurrentY * Framebuffer.Pitch) + (CurrentX * (Framebuffer.Depth shr 3)));
          
          {Write Pixel}
          PLongWord(Address)^:=Color;
          
          Inc(CurrentY);
         end; 
         
        Inc(CurrentX);
       end;  
       
      {Check Cached}
      if (Framebuffer.Device.DeviceFlags and FRAMEBUFFER_FLAG_CACHED) <> 0 then
       begin
        {Get Address}
        Address:=(Framebuffer.Address + (Y1 * Framebuffer.Pitch) + (X1 * (Framebuffer.Depth shr 3))); //To Do //Should this be just Y1 to start at the left hand edge ?
        
        {Get Size}
        Size:=((Y2 - Y1) + 1) * Framebuffer.Pitch; //To Do //Or should this be + X2 * (Framebuffer.Depth shr 3) ? (and not Y + 1)
        
        {Clean Cache}
        CleanAndInvalidateDataCacheRange(Address,Size);
       end;
     end;  
 
    {Unlock Framebuffer}
    MutexUnlock(Framebuffer.Lock);
    
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
end;*)

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
              Framebuffer.DeviceCommit(Framebuffer,LongWord(Address),Width * ConsoleBytes,FRAMEBUFFER_TRANSFER_NONE);
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
              Framebuffer.DeviceCommit(Framebuffer,LongWord(Address),Width * ConsoleBytes,FRAMEBUFFER_TRANSFER_NONE);
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
                Framebuffer.DeviceCommit(Framebuffer,LongWord(Address),Width * ConsoleBytes,FRAMEBUFFER_TRANSFER_NONE);
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

function FramebufferConsoleDrawWindow(Console:PConsoleDevice;Handle:TWindowHandle):LongWord;
{Implementation of ConsoleDeviceDrawWindow API for FramebufferConsole}
{Note: Not intended to be called directly by applications, use ConsoleDeviceDrawWindow instead}
{Note: Caller must hold the Window lock}
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
 
 {Check Window State}
 if Window.WindowState = WINDOW_STATE_INVISIBLE then
  begin
   {Draw Window}
   Result:=FramebufferConsoleDrawBlock(Console,Window.X1,Window.Y1,Window.X2,Window.Y2,PFramebufferConsole(Console).DesktopColor);
  end
 else if Window.WindowState = WINDOW_STATE_VISIBLE then
  begin 
   {Draw Border}
   if Window.Borderwidth > 0 then
    begin
     Result:=FramebufferConsoleDrawBox(Console,Window.X1,Window.Y1,Window.X2,Window.Y2,Window.Bordercolor,Window.Borderwidth);
     if Result <> ERROR_SUCCESS then Exit;
    end; 
   
   {Draw Window}
   Result:=FramebufferConsoleDrawBlock(Console,Window.X1 + Window.Borderwidth,Window.Y1 + Window.Borderwidth,Window.X2 - Window.Borderwidth,Window.Y2 - Window.Borderwidth,Window.Backcolor);
  end; 
end;

{==============================================================================}

function FramebufferConsoleDrawDesktop(Console:PConsoleDevice):LongWord;
{Internal function used by FramebufferConsole to draw the console desktop}
{Note: Not intended to be called directly by applications}
var
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

 if MutexLock(Console.Lock) = ERROR_SUCCESS then 
  begin
   try
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
    TitleLen:=Length(FRAMEBUFFER_CONSOLE_TITLE);
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
      Result:=FramebufferConsoleDrawText(Console,TitleFont,FRAMEBUFFER_CONSOLE_TITLE,TitleX,TitleY,TitleForecolor,TitleBackcolor,TitleLen);
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
//To Do //Remove
(*function FramebufferConsoleGetPixelOld(Console:PConsoleDevice;X,Y:LongWord;var Color:LongWord):LongWord; 
{Implementation of ConsoleDeviceGetPixel API for FramebufferConsole}
{Note: Not intended to be called directly by applications, use ConsoleDeviceGetPixel instead}
var
 Address:LongWord;
 Framebuffer:PFramebufferDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
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
    if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 
    if Framebuffer.Address = 0 then Exit;
 
    {Lock Framebuffer}
    if MutexLock(Framebuffer.Lock) <> ERROR_SUCCESS then Exit;
 
    {Get Address}
    Address:=(Framebuffer.Address + (Y * Framebuffer.Pitch) + (X * (Framebuffer.Depth shr 3)));
 
    {Read Pixel}
    Color:=PLongWord(Address)^;
 
    {Memory Barrier}
    DataMemoryBarrier;  {After the Last Read}
 
    {Check Swap}
    if (Framebuffer.Device.DeviceFlags and FRAMEBUFFER_FLAG_SWAP) <> 0 then
     begin
      Color:=FramebufferDeviceSwap(Color);
     end;
 
    {Unlock Framebuffer}
    MutexUnlock(Framebuffer.Lock);
    
    {Update Statistics}
    Inc(Console.GetCount);
    
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
end;*)

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
function ConsoleDeviceGetCount:LongWord; inline;
{Get the current console device count}
begin
 {}
 Result:=ConsoleDeviceTableCount;
end;

{==============================================================================}

function ConsoleDeviceGetDefault:PConsoleDevice; inline;
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
     Console.Console.Device.DeviceFlags:=CONSOLE_FLAG_BLINK_CURSOR or CONSOLE_FLAG_COLOR or CONSOLE_FLAG_FONT or CONSOLE_FLAG_FULLSCREEN;
     Console.Console.Device.DeviceData:=@Framebuffer.Device;
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
     Console.Console.DeviceGetPosition:=FramebufferConsoleGetPosition;
     Console.Console.FontRatio:=1; {Font ratio 1 for Pixel console}
     {Framebuffer}
     Console.Framebuffer:=Framebuffer;
     Console.DesktopOffset:=FRAMEBUFFER_CONSOLE_DEFAULT_DESKTOPOFFSET;
     Console.DesktopColor:=FRAMEBUFFER_CONSOLE_DEFAULT_DESKTOPCOLOR;
     
     {Setup Flags}
     if CONSOLE_LINE_WRAP then Console.Console.Device.DeviceFlags:=Console.Console.Device.DeviceFlags or CONSOLE_FLAG_LINE_WRAP;
     if CONSOLE_AUTO_SCROLL then Console.Console.Device.DeviceFlags:=Console.Console.Device.DeviceFlags or CONSOLE_FLAG_AUTO_SCROLL;
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
        end;
      end
     else 
      begin
       if DEVICE_LOG_ENABLED then DeviceLogError(nil,'Console: Failed to register new framebuffer console device: ' + ErrorToString(Status));
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
 
 {$IF DEFINED(CONSOLE_DEBUG) or DEFINED(DEVICE_DEBUG)}
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
 
 {$IF DEFINED(CONSOLE_DEBUG) or DEFINED(DEVICE_DEBUG)}
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
begin
 {}
 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Get Window Count}
 Result:=Console.WindowCount;
end;

{==============================================================================}

function ConsoleWindowGetDefault(Console:PConsoleDevice):TWindowHandle; inline;
{Get the current console default window}
begin
 {}
 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {Get Window Default}
 Result:=Console.WindowDefault;
end;

{==============================================================================}

function ConsoleWindowSetDefault(Console:PConsoleDevice;Handle:TWindowHandle):LongWord;
{Set the current console default window}
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
{==============================================================================}

initialization
 ConsoleInit;
 
{==============================================================================}
 
finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
