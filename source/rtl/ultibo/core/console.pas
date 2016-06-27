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
        
        Console Window coordinates X,Y are always based on characters, beginning at 1,1 and extend to Cols,Rows
      

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
 CONSOLE_FLAG_DMA_FILL        = $00000008; {Use DMA to draw fill blocks (Where applicable)}
 CONSOLE_FLAG_DMA_CLEAR       = $00000010; {Use DMA to draw clock blocks (Where applicable)}
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
 TConsoleDeviceDrawWindow = function(Console:PConsoleDevice;Handle:TWindowHandle):LongWord;
 TConsoleDeviceGetPosition = function(Console:PConsoleDevice;Position:LongWord;var X1,Y1,X2,Y2:LongWord):LongWord;
 
 PConsoleWindow = ^TConsoleWindow;
 
 {Console Device}
 TConsoleDevice = record
  {Device Properties}
  Device:TDevice;                                {The Device entry for this Console device}
  {Console Properties}
  ConsoleId:LongWord;                            {Unique Id of this Console device in the Console device table}
  ConsoleState:LongWord;                         {Console device state (eg CONSOLE_STATE_OPEN)}
  ConsoleMode:LongWord;                          {Console device mode (eg CONSOLE_MODE_PIXEL)}
  DeviceOpen:TConsoleDeviceOpen;                 {A device specific DeviceOpen method implementing a standard console device interface}
  DeviceClose:TConsoleDeviceClose;               {A device specific DeviceClose method implementing a standard console device interface}
  DeviceClear:TConsoleDeviceClear;               {A device specific DeviceClear method implementing a standard console device interface}
  DeviceScroll:TConsoleDeviceScroll;             {A device specific DeviceScroll method implementing a standard console device interface}
  DeviceDrawBox:TConsoleDeviceDrawBox;           {A device specific DeviceDrawBox method implementing a standard console device interface}
  DeviceDrawLine:TConsoleDeviceDrawLine;         {A device specific DeviceDrawLine method implementing a standard console device interface}
  DeviceDrawChar:TConsoleDeviceDrawChar;         {A device specific DeviceDrawChar method implementing a standard console device interface}
  DeviceDrawText:TConsoleDeviceDrawText;         {A device specific DeviceDrawText method implementing a standard console device interface}
  DeviceDrawPixel:TConsoleDeviceDrawPixel;       {A device specific DeviceDrawPixel method implementing a standard console device interface}
  DeviceDrawBlock:TConsoleDeviceDrawBlock;       {A device specific DeviceDrawBlock method implementing a standard console device interface}
  DeviceDrawWindow:TConsoleDeviceDrawWindow;     {A device specific DeviceDrawWindow method implementing a standard console device interface}
  DeviceGetPosition:TConsoleDeviceGetPosition;   {A device specific DeviceGetPosition method implementing a standard console device interface}
  {Statistics Properties}
  OpenCount:LongWord;
  CloseCount:LongWord;
  ClearCount:LongWord;
  ScrollCount:LongWord;
  DrawCount:LongWord;
  {Driver Properties}
  Lock:TMutexHandle;                             {Device lock}
  Width:LongWord;                                {Console Width}
  Height:LongWord;                               {Console Height}
  Forecolor:LongWord;                            {Foreground Color}
  Backcolor:LongWord;                            {Background Color}
  Borderwidth:LongWord;                          {Border Width}
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
  X1:LongWord;                                   {Window X1} {Console Relative (Pixels / Characters depending on Console mode)} 
  Y1:LongWord;                                   {Window Y1} {Console Relative (Pixels / Characters depending on Console mode)}  
  X2:LongWord;                                   {Window X2} {Console Relative (Pixels / Characters depending on Console mode)}  
  Y2:LongWord;                                   {Window Y2} {Console Relative (Pixels / Characters depending on Console mode)}  
  Width:LongWord;                                {Window Width in Columns (Characters)}
  Height:LongWord;                               {Window Height in Rows (Characters)}
  OffsetX:LongWord;                              {Window X Offset (Pixels / Characters depending on Console mode)}
  OffsetY:LongWord;                              {Window Y Offset (Pixels / Characters depending on Console mode)}
  MinX:LongWord;                                 {Viewport X1} {Window Relative (Characters)}
  MinY:LongWord;                                 {Viewport Y1} {Window Relative (Characters)}
  MaxX:LongWord;                                 {Viewport X2} {Window Relative (Characters)}
  MaxY:LongWord;                                 {Viewport Y2} {Window Relative (Characters)}
  X:LongWord;                                    {Current X}  {Window Relative (Characters)}
  Y:LongWord;                                    {Current Y}  {Window Relative (Characters)}
  Cols:LongWord;                                 {Viewport Columns (Characters)}
  Rows:LongWord;                                 {Viewport Rows (Characters)}
  Forecolor:LongWord;                            {Current Foreground Color}
  Backcolor:LongWord;                            {Current Background Color}
  Borderwidth:LongWord;                          {Current Border Width}
  Bordercolor:LongWord;                          {Current Border Color}
  {Font Properties}
  Font:TFontHandle;                              {Window Font}
  FontWidth:LongWord;                            {Font Width (Pixels)}
  FontHeight:LongWord;                           {Font Height (Pixels)}
  {Cursor Properties}
  CursorX:LongWord;                              {Cursor X}  {Window Relative (Characters)}
  CursorY:LongWord;                              {Cursor Y}  {Window Relative (Characters)}
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
  {DMA Properties}
  FillSize:LongWord;                             {Size of the DMA fill buffer (From DMAAllocateBufferEx)}
  FillBuffer:Pointer;                            {Buffer for DMA memory fills}
  ScrollBuffer:Pointer;                          {Buffer for DMA scroll right (Overlapped)}
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
function ConsoleDeviceDrawWindow(Console:PConsoleDevice;Handle:TWindowHandle):LongWord;

function ConsoleDeviceGetPosition(Console:PConsoleDevice;Position:LongWord;var X1,Y1,X2,Y2:LongWord):LongWord;

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

function ConsoleWindowGetMinX(Handle:TWindowHandle):LongWord;
function ConsoleWindowGetMinY(Handle:TWindowHandle):LongWord;
function ConsoleWindowGetMaxX(Handle:TWindowHandle):LongWord;
function ConsoleWindowGetMaxY(Handle:TWindowHandle):LongWord;

function ConsoleWindowGetRect(Handle:TWindowHandle):TConsoleRect;
function ConsoleWindowSetRect(Handle:TWindowHandle;const ARect:TConsoleRect):LongWord;

function ConsoleWindowGetViewport(Handle:TWindowHandle;var X1,Y1,X2,Y2:LongWord):LongWord;
function ConsoleWindowSetViewport(Handle:TWindowHandle;X1,Y1,X2,Y2:LongWord):LongWord;

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
function FramebufferConsoleScroll(Console:PConsoleDevice;X1,Y1,X2,Y2,Count,Direction:LongWord):LongWord;

function FramebufferConsoleDrawBox(Console:PConsoleDevice;X1,Y1,X2,Y2,Color,Width:LongWord):LongWord;
function FramebufferConsoleDrawLine(Console:PConsoleDevice;X1,Y1,X2,Y2,Color,Width:LongWord):LongWord;
function FramebufferConsoleDrawChar(Console:PConsoleDevice;Handle:TFontHandle;Ch:Char;X,Y,Forecolor,Backcolor:LongWord):LongWord;
function FramebufferConsoleDrawText(Console:PConsoleDevice;Handle:TFontHandle;const Text:String;X,Y,Forecolor,Backcolor,Len:LongWord):LongWord;
function FramebufferConsoleDrawPixel(Console:PConsoleDevice;X,Y,Color:LongWord):LongWord;
function FramebufferConsoleDrawBlock(Console:PConsoleDevice;X1,Y1,X2,Y2,Color:LongWord):LongWord;
function FramebufferConsoleDrawWindow(Console:PConsoleDevice;Handle:TWindowHandle):LongWord;
function FramebufferConsoleDrawDesktop(Console:PConsoleDevice):LongWord;

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
  end;
end;

{==============================================================================}

function ConsoleDeviceDrawLine(Console:PConsoleDevice;X1,Y1,X2,Y2,Color,Width:LongWord):LongWord;
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
  end;
end;

{==============================================================================}

function ConsoleDeviceDrawChar(Console:PConsoleDevice;Handle:TFontHandle;Ch:Char;X,Y,Forecolor,Backcolor:LongWord):LongWord;
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
  end;
end;

{==============================================================================}

function ConsoleDeviceDrawBlock(Console:PConsoleDevice;X1,Y1,X2,Y2,Color:LongWord):LongWord;
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
 Result.DeviceDrawWindow:=nil;
 Result.DeviceGetPosition:=nil;
 Result.Lock:=INVALID_HANDLE_VALUE;
 Result.Width:=0;        
 Result.Height:=0;       
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
   if not(Assigned(Console.DeviceDrawWindow)) then Exit;
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
{Return: Handle to new Console window or INVALID_HANDLE_VALUE if Console window could not be created}
begin
 {}
 Result:=ConsoleWindowCreateEx(Console,INVALID_HANDLE_VALUE,SizeOf(TConsoleWindow),WINDOW_STATE_VISIBLE,WINDOW_MODE_TEXT,Position,Default);
end;

{==============================================================================}

function ConsoleWindowCreateEx(Console:PConsoleDevice;Font:TFontHandle;Size,State,Mode,Position:LongWord;Default:Boolean):TWindowHandle;
{Create a new Console window}
{Return: Handle to new Console window or INVALID_HANDLE_VALUE if Console window could not be created}
var
 X1:LongWord;
 Y1:LongWord;
 X2:LongWord;
 Y2:LongWord;
 Unlock:Boolean;
 RemainX:LongWord;
 RemainY:LongWord;
 Handle:TWindowHandle;
 Window:PConsoleWindow;
begin
 {}
 Result:=INVALID_HANDLE_VALUE;
 
 {Check Size}
 if Size < SizeOf(TConsoleWindow) then Exit;
 
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
      {Fail on all other positions}
      if ConsoleWindowFind(Console,CONSOLE_POSITION_TOP) <> INVALID_HANDLE_VALUE then Exit;
      if ConsoleWindowFind(Console,CONSOLE_POSITION_BOTTOM) <> INVALID_HANDLE_VALUE then Exit;
      if ConsoleWindowFind(Console,CONSOLE_POSITION_LEFT) <> INVALID_HANDLE_VALUE then Exit;
      if ConsoleWindowFind(Console,CONSOLE_POSITION_RIGHT) <> INVALID_HANDLE_VALUE then Exit;
      if ConsoleWindowFind(Console,CONSOLE_POSITION_TOPLEFT) <> INVALID_HANDLE_VALUE then Exit;
      if ConsoleWindowFind(Console,CONSOLE_POSITION_TOPRIGHT) <> INVALID_HANDLE_VALUE then Exit;
      if ConsoleWindowFind(Console,CONSOLE_POSITION_BOTTOMLEFT) <> INVALID_HANDLE_VALUE then Exit;
      if ConsoleWindowFind(Console,CONSOLE_POSITION_BOTTOMRIGHT) <> INVALID_HANDLE_VALUE then Exit;
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
    Unlock:=True;
    
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

       {Release Lock}
       MutexUnlock(Console.Lock);
       Unlock:=False;
        
       {Draw Window}
       ConsoleDeviceDrawWindow(Console,TWindowHandle(Window));
       
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
    if Unlock then MutexUnlock(Console.Lock);
   end; 
  end;
end;

{==============================================================================}

function ConsoleWindowDestroy(Handle:TWindowHandle):LongWord;
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
     
     {Set State}
     Window.WindowState:=WINDOW_STATE_INVISIBLE;
    
     {Draw Window}
     Result:=ConsoleDeviceDrawWindow(Window.Console,TWindowHandle(Window));
     if Result <> ERROR_SUCCESS then Exit;

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
 //To Do //Check for WINDOW_MODE_TEXT //All of these ? //Maybe not this one ?
 
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
   ConsoleDeviceDrawWindow(Window.Console,TWindowHandle(Window));
   
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
   ConsoleDeviceDrawWindow(Window.Console,TWindowHandle(Window));
  end;
 finally
  {Unlock Window}
  MutexUnlock(Window.Lock);
 end; 
end;
 
{==============================================================================}

function ConsoleWindowGetMinX(Handle:TWindowHandle):LongWord;
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
 
 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;
 
 {Get MinX}
 Result:=Window.MinX;
 
 {Unlock Window}
 MutexUnlock(Window.Lock);
end;

{==============================================================================}

function ConsoleWindowGetMinY(Handle:TWindowHandle):LongWord;
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
 
 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;
 
 {Get MinY}
 Result:=Window.MinY;
 
 {Unlock Window}
 MutexUnlock(Window.Lock);
end;

{==============================================================================}

function ConsoleWindowGetMaxX(Handle:TWindowHandle):LongWord;
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
 
 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;
 
 {Get MaxX}
 Result:=Window.MaxX;
 
 {Unlock Window}
 MutexUnlock(Window.Lock);
end;

{==============================================================================}

function ConsoleWindowGetMaxY(Handle:TWindowHandle):LongWord;
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
 
 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;
 
 {Get MaxY}
 Result:=Window.MaxY;
 
 {Unlock Window}
 MutexUnlock(Window.Lock);
end;

{==============================================================================}

function ConsoleWindowGetRect(Handle:TWindowHandle):TConsoleRect;
{Note: For Text Console functions, Rect is based on character rows and columns not screen pixels}
begin
 {}
 ConsoleWindowGetViewport(Handle,Result.X1,Result.Y1,Result.X2,Result.Y2);
end;

{==============================================================================}

function ConsoleWindowSetRect(Handle:TWindowHandle;const ARect:TConsoleRect):LongWord;
{Note: For Text Console functions, Rect is based on character rows and columns not screen pixels}
begin
 {}
 Result:=ConsoleWindowSetViewport(Handle,ARect.X1,ARect.Y1,ARect.X2,ARect.Y2);
end;

{==============================================================================}

function ConsoleWindowGetViewport(Handle:TWindowHandle;var X1,Y1,X2,Y2:LongWord):LongWord;
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

function ConsoleWindowGetX(Handle:TWindowHandle):LongWord;
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
 
 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;

 {Get X}
 Result:=Window.X;
 
 {Unlock Window}
 MutexUnlock(Window.Lock);
end;

{==============================================================================}

function ConsoleWindowSetX(Handle:TWindowHandle;X:LongWord):LongWord;
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
 
 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;

 {Get Y}
 Result:=Window.Y;
 
 {Unlock Window}
 MutexUnlock(Window.Lock);
end;

{==============================================================================}

function ConsoleWindowSetY(Handle:TWindowHandle;Y:LongWord):LongWord;
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
{Note: For Text Console functions, Point is based on character rows and columns not screen pixels}
begin
 {}
 ConsoleWindowGetXY(Handle,Result.X,Result.Y);
end;

{==============================================================================}

function ConsoleWindowSetPoint(Handle:TWindowHandle;const APoint:TConsolePoint):LongWord;
{Note: For Text Console functions, Point is based on character rows and columns not screen pixels}
begin
 {}
 Result:=ConsoleWindowSetXY(Handle,APoint.X,APoint.Y);
end;

{==============================================================================}

function ConsoleWindowGetCols(Handle:TWindowHandle):LongWord;
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
 
 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;
 
 {Get Cols}
 Result:=Window.Cols;
 
 {Unlock Window}
 MutexUnlock(Window.Lock);
end;

{==============================================================================}

function ConsoleWindowGetRows(Handle:TWindowHandle):LongWord;
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
 
 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;
 
 {Get Rows}
 Result:=Window.Rows;
 
 {Unlock Window}
 MutexUnlock(Window.Lock);
end;

{==============================================================================}

function ConsoleWindowGetForecolor(Handle:TWindowHandle):LongWord;
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
 
 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;

 {Get Forecolor}
 Result:=Window.Forecolor;
 
 {Unlock Window}
 MutexUnlock(Window.Lock);
end;

{==============================================================================}

function ConsoleWindowSetForecolor(Handle:TWindowHandle;Color:LongWord):LongWord;
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
 
 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;

 {Get Backcolor}
 Result:=Window.Backcolor;
 
 {Unlock Window}
 MutexUnlock(Window.Lock);
end;

{==============================================================================}

function ConsoleWindowSetBackcolor(Handle:TWindowHandle;Color:LongWord):LongWord;
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
 
 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;

 {Get Font}
 Result:=Window.Font;
 
 {Unlock Window}
 MutexUnlock(Window.Lock);
end;

{==============================================================================}

function ConsoleWindowSetFont(Handle:TWindowHandle;Font:TFontHandle):LongWord;
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
    ConsoleDeviceDrawWindow(Window.Console,TWindowHandle(Window));
    
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
 
 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;

 {Check Cursor State}
 if Window.CursorState = CURSOROFF then
  begin
   {Set Cursor State}
   Window.CursorState:=CURSORON;
   
   //To Do //Check WINDOW_STATE_VISIBLE
   
   //To Do //Check Hardware Cursor / Create timer etc
 
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
 
 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;

 {Check Cursor State}
 if Window.CursorState = CURSORON then
  begin
   {Set Cursor State}
   Window.CursorState:=CURSOROFF;
   
   //To Do //Check WINDOW_STATE_VISIBLE
   
   //To Do //Check Hardware Cursor / Destroy timer etc
 
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
   
     //To Do //Check WINDOW_STATE_VISIBLE
     
     //To Do //Check Hardware Cursor / Move Cursor
     
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
 
 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;

 {Check Cursor State}
 if Window.CursorState = CURSORON then
  begin
   {Check Cursor Blink}
   if Window.CursorBlink and not(Enabled) then
    begin
    
     //To Do //Check WINDOW_STATE_VISIBLE
     
     //To Do //Check Hardware Cursor / Destroy timer etc
     
    end
   else if not(Window.CursorBlink) and Enabled then
    begin
    
     //To Do //Check WINDOW_STATE_VISIBLE
     
     //To Do //Check Hardware Cursor / Create timer etc
     
    end;

   {Set Cursor Blink}
   Window.CursorBlink:=Enabled;
   
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
 //To Do //Check WINDOW_STATE_VISIBLE
 
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
 //To Do //Check WINDOW_STATE_VISIBLE
 
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
 //To Do //Check WINDOW_STATE_VISIBLE
 
 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;
 try
  {Check Console}
  if Window.Console = nil then Exit;
 
  //To Do //
  
 finally
  {Unlock Window}
  MutexUnlock(Window.Lock);
 end; 
end;

{==============================================================================}

function ConsoleWindowScrollRight(Handle:TWindowHandle;Row,Col,Lines,Chars:LongWord):LongWord;
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
 //To Do //Check WINDOW_STATE_VISIBLE
 
 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;
 try
  {Check Console}
  if Window.Console = nil then Exit;
 
  //To Do //
  
 finally
  {Unlock Window}
  MutexUnlock(Window.Lock);
 end; 
end;

{==============================================================================}

function ConsoleWindowClear(Handle:TWindowHandle):LongWord;
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
 //To Do //Check WINDOW_STATE_VISIBLE
 
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
 //To Do //Check WINDOW_STATE_VISIBLE
 
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
 
  {Console Draw Block}
  Result:=ConsoleDeviceDrawBlock(Window.Console,ClearX1,ClearY1,ClearX2,ClearY2,Window.Backcolor);
  if Result <> ERROR_SUCCESS then Exit;
  
  {Update X,Y}
  Window.X:=1;
  Window.Y:=1;
  
  {Check Cursor}
  if Cursor then
   begin
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
 Unlock:Boolean;
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
 //To Do //Check WINDOW_STATE_VISIBLE
 
 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;
 try
  Unlock:=True;
  
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

      {Release Lock}
      MutexUnlock(Window.Lock);
      Unlock:=False;
      
      {Console Scroll Up}
      Result:=ConsoleWindowScrollUp(Handle,Window.Y,1);
      if Result <> ERROR_SUCCESS then Exit;
      
      {Acquire Lock}
      if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;
      Unlock:=True;
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
  if Unlock then MutexUnlock(Window.Lock);
 end; 
end;
  
{==============================================================================}

function ConsoleWindowWriteEx(Handle:TWindowHandle;const AText:String;X,Y:LongWord;Forecolor,Backcolor:LongWord):LongWord;
{Note: For Text Console functions, X and Y are based on screen character rows and columns not screen pixels}
var
 Unlock:Boolean;
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
 //To Do //Check WINDOW_STATE_VISIBLE
 
 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;
 try
  Unlock:=True;
  
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

      {Release Lock}
      MutexUnlock(Window.Lock);
      Unlock:=False;
      
      {Console Scroll Up}
      Result:=ConsoleWindowScrollUp(Handle,Y,1);
      if Result <> ERROR_SUCCESS then Exit;
      
      {Acquire Lock}
      if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;
      Unlock:=True;
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
  if Unlock then MutexUnlock(Window.Lock);
 end; 
end;

{==============================================================================}

function ConsoleWindowWriteLn(Handle:TWindowHandle;const AText:String):LongWord;
var
 Unlock:Boolean;
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
 //To Do //Check WINDOW_STATE_VISIBLE
 
 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;
 try
  Unlock:=True;

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

      {Release Lock}
      MutexUnlock(Window.Lock);
      Unlock:=False;
      
      {Console Scroll Up}
      Result:=ConsoleWindowScrollUp(Handle,Window.Y,1);
      if Result <> ERROR_SUCCESS then Exit;
      
      {Acquire Lock}
      if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;
      Unlock:=True;
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

    {Release Lock}
    MutexUnlock(Window.Lock);
    Unlock:=False;
    
    {Console Scroll Up}
    Result:=ConsoleWindowScrollUp(Handle,Window.Y,1);
    if Result <> ERROR_SUCCESS then Exit;
    
    {Acquire Lock}
    if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;
    Unlock:=True;
   end;     
  
  {Update Cursor} 
  //To Do //Check cursor, Move Cursor etc 
  
  {Return Result}
  Result:=ERROR_SUCCESS;
 finally
  {Unlock Window}
  if Unlock then MutexUnlock(Window.Lock);
 end; 
end;
  
{==============================================================================}

function ConsoleWindowWriteLnEx(Handle:TWindowHandle;const AText:String;X,Y:LongWord;Forecolor,Backcolor:LongWord):LongWord;
{Note: For Text Console functions, X and Y are based on character rows and columns not screen pixels}
var
 Unlock:Boolean;
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
 //To Do //Check WINDOW_STATE_VISIBLE
 
 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;
 try
  Unlock:=True;

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

      {Release Lock}
      MutexUnlock(Window.Lock);
      Unlock:=False;
      
      {Console Scroll Up}
      Result:=ConsoleWindowScrollUp(Handle,Y,1);
      if Result <> ERROR_SUCCESS then Exit;
      
      {Acquire Lock}
      if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;
      Unlock:=True;
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

    {Release Lock}
    MutexUnlock(Window.Lock);
    Unlock:=False;
    
    {Console Scroll Up}
    Result:=ConsoleWindowScrollUp(Handle,Y,1);
    if Result <> ERROR_SUCCESS then Exit;
    
    {Acquire Lock}
    if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;
    Unlock:=True;
   end;     
  
  {No Cursor Update}
  
  {Return Result}
  Result:=ERROR_SUCCESS;
 finally
  {Unlock Window}
  if Unlock then MutexUnlock(Window.Lock);
 end; 
end;

{==============================================================================}

function ConsoleWindowWriteChr(Handle:TWindowHandle;AChr:Char):LongWord;
var
 Unlock:Boolean;
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
 //To Do //Check WINDOW_STATE_VISIBLE
 
 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;
 try
  Unlock:=True;

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
      
          {Release Lock}
          MutexUnlock(Window.Lock);
          Unlock:=False;
          
          {Console Scroll Up}
          Result:=ConsoleWindowScrollUp(Handle,Window.Y,1);
          if Result <> ERROR_SUCCESS then Exit;
          
          {Acquire Lock}
          if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;
          Unlock:=True;
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
      
          {Release Lock}
          MutexUnlock(Window.Lock);
          Unlock:=False;
          
          {Console Scroll Up}
          Result:=ConsoleWindowScrollUp(Handle,Window.Y,1);
          if Result <> ERROR_SUCCESS then Exit;
          
          {Acquire Lock}
          if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;
          Unlock:=True;
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
   
       {Release Lock}
       MutexUnlock(Window.Lock);
       Unlock:=False;
       
       {Console Scroll Up}
       Result:=ConsoleWindowScrollUp(Handle,Window.Y,1);
       if Result <> ERROR_SUCCESS then Exit;
       
       {Acquire Lock}
       if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;
       Unlock:=True;
      end; 
    end;
  end;
  
  {Update Cursor} 
  //To Do //Check cursor, Move Cursor etc 
  
  {Return Result}
  Result:=ERROR_SUCCESS;
 finally
  {Unlock Window}
  if Unlock then MutexUnlock(Window.Lock);
 end; 
end;
  
{==============================================================================}

function ConsoleWindowWriteChrEx(Handle:TWindowHandle;AChr:Char;X,Y:LongWord;Forecolor,Backcolor:LongWord):LongWord;
{Note: For Text Console functions, X and Y are based on character rows and columns not screen pixels}
var
 Unlock:Boolean;
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
 //To Do //Check WINDOW_STATE_VISIBLE
 
 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;
 try
  Unlock:=True;

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
      
          {Release Lock}
          MutexUnlock(Window.Lock);
          Unlock:=False;
          
          {Console Scroll Up}
          Result:=ConsoleWindowScrollUp(Handle,Y,1);
          if Result <> ERROR_SUCCESS then Exit;
          
          {Acquire Lock}
          if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;
          Unlock:=True;
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
      
          {Release Lock}
          MutexUnlock(Window.Lock);
          Unlock:=False;
          
          {Console Scroll Up}
          Result:=ConsoleWindowScrollUp(Handle,Y,1);
          if Result <> ERROR_SUCCESS then Exit;
          
          {Acquire Lock}
          if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;
          Unlock:=True;
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
   
       {Release Lock}
       MutexUnlock(Window.Lock);
       Unlock:=False;
       
       {Console Scroll Up}
       Result:=ConsoleWindowScrollUp(Handle,Y,1);
       if Result <> ERROR_SUCCESS then Exit;
       
       {Acquire Lock}
       if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;
       Unlock:=True;
      end;     
    end;
  end;
  
  {No cursor Update}
  
  {Return Result}
  Result:=ERROR_SUCCESS;
  
 finally
  {Unlock Window}
  if Unlock then MutexUnlock(Window.Lock);
 end; 
end;

{==============================================================================}
{==============================================================================}
{CRT Console Functions}
procedure ConsoleAssignCrt(var F:Text);
begin
 {}
 TextIOOpen(F,ConsoleWriteChar,ConsoleReadChar,fmOutput,nil);
end;

{==============================================================================}

procedure ConsoleClrEol;
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
begin
 {}
 ThreadSleep(MS);
end;

{==============================================================================}

procedure ConsoleDelLine;
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
begin
 {}
 {No function under Ultibo}
end;

{==============================================================================}

procedure ConsoleInsLine;
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
begin
 {}
 Result:=False; //To Do //Need ConsolePeekChar ?
                //See notes in ConsoleReadChar
                //This needs to use the function registered by Keyboard to handle console reads
                //Call ConsoleReadChar, it will call the handler
end;

{==============================================================================}

procedure ConsoleLowVideo;
begin
 {}
 {No function under Ultibo}
end;

{==============================================================================}

procedure ConsoleNormVideo;
begin
 {}
 {No function under Ultibo}
end;

{==============================================================================}

procedure ConsoleNoSound;
begin
 {}
 {No function under Ultibo}
end;

{==============================================================================}

function ConsoleReadKey:Char;
begin
 {}
 ConsoleReadChar(Result,nil);
end;

{==============================================================================}

procedure ConsoleSound(Hz:Word);
begin
 {}
 {No function under Ultibo}
end;

{==============================================================================}

procedure ConsoleTextBackground(Color:LongWord);
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
begin
 {}
 {No function under Ultibo}
end;

{==============================================================================}

function ConsoleWhereX:Integer;
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
begin
 {}
 //To Do //See notes in ConsoleReadChar
         //This needs to use the function registered by Keyboard to handle console reads
         //Call ConsoleReadChar, it will call the handler
               
         //while ConsolePeekChar do ConsoleReadChar etc ?
end;

{==============================================================================}

procedure ConsoleReadLn(var AText:String);
begin
 {}
 //To Do //See notes in ConsoleReadChar
         //This needs to use the function registered by Keyboard to handle console reads
         //Call ConsoleReadChar, it will call the handler
               
         //ConsoleReadChar until line end character ? (see ConsoleWindowWriteChr etc)
end;

{==============================================================================}

procedure ConsoleReadChr(var AChr:Char);
begin
 {}
 ConsoleReadChar(AChr,nil);
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
var
 Unlock:Boolean;
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
    Unlock:=True;
    
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
      
      {Release Lock}
      MutexUnlock(Console.Lock);
      Unlock:=False;
      
      {Draw Desktop}
      Result:=FramebufferConsoleDrawDesktop(Console);
     end;
   finally
    if Unlock then MutexUnlock(Console.Lock);
   end; 
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function FramebufferConsoleClose(Console:PConsoleDevice):LongWord;
var
 Unlock:Boolean;
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
    Unlock:=True;

    {Get Framebuffer}
    Framebuffer:=PFramebufferConsole(Console).Framebuffer;
    
    {Check Framebuffer}
    if Framebuffer = nil then Exit;
    if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 
    if Framebuffer.Address = 0 then Exit;
   
    {Release Lock}
    MutexUnlock(Console.Lock);
    Unlock:=False;

    {Clear Console}
    Result:=FramebufferConsoleClear(Console,COLOR_BLACK);
    if Result <> ERROR_SUCCESS then Exit;
    
    {Acquire Lock}
    if MutexLock(Console.Lock) <> ERROR_SUCCESS then Exit;
    Unlock:=True;
    
    {Update Console}
    {Console}
    Console.Width:=0;
    Console.Height:=0;
    {Framebuffer}
    PFramebufferConsole(Console).DesktopX:=0;
    PFramebufferConsole(Console).DesktopY:=0;
    PFramebufferConsole(Console).DesktopWidth:=0;
    PFramebufferConsole(Console).DesktopHeight:=0;
    {DMA}
    if PFramebufferConsole(Console).FillBuffer <> nil then
     begin
      DMAReleaseBuffer(PFramebufferConsole(Console).FillBuffer);
      PFramebufferConsole(Console).FillSize:=0;
      PFramebufferConsole(Console).FillBuffer:=nil;
     end;
    if PFramebufferConsole(Console).ScrollBuffer <> nil then
     begin
      DMAReleaseBuffer(PFramebufferConsole(Console).ScrollBuffer);
      PFramebufferConsole(Console).ScrollBuffer:=nil;
     end;
     
    {Update Statistics}
    Inc(Console.CloseCount);
    
    {Get Result}
    Result:=ERROR_SUCCESS;
   finally
    if Unlock then MutexUnlock(Console.Lock);
   end; 
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function FramebufferConsoleClear(Console:PConsoleDevice;Color:LongWord):LongWord;
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
end;

{==============================================================================}

function FramebufferConsoleScroll(Console:PConsoleDevice;X1,Y1,X2,Y2,Count,Direction:LongWord):LongWord;
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
            Next.DestStride:=Framebuffer.Pitch - Size;
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
            Next.SourceStride:=Framebuffer.Pitch - Size;
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
end;

{==============================================================================}

function FramebufferConsoleDrawBox(Console:PConsoleDevice;X1,Y1,X2,Y2,Color,Width:LongWord):LongWord;
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
end;

{==============================================================================}

function FramebufferConsoleDrawLine(Console:PConsoleDevice;X1,Y1,X2,Y2,Color,Width:LongWord):LongWord;
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
end;

{==============================================================================}

function FramebufferConsoleDrawChar(Console:PConsoleDevice;Handle:TFontHandle;Ch:Char;X,Y,Forecolor,Backcolor:LongWord):LongWord;
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
end;

{==============================================================================}

function FramebufferConsoleDrawText(Console:PConsoleDevice;Handle:TFontHandle;const Text:String;X,Y,Forecolor,Backcolor,Len:LongWord):LongWord;
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
end;

{==============================================================================}

function FramebufferConsoleDrawPixel(Console:PConsoleDevice;X,Y,Color:LongWord):LongWord;
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
end;

{==============================================================================}

function FramebufferConsoleDrawBlock(Console:PConsoleDevice;X1,Y1,X2,Y2,Color:LongWord):LongWord;
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
end;

{==============================================================================}

function FramebufferConsoleDrawWindow(Console:PConsoleDevice;Handle:TWindowHandle):LongWord;
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
var
 Unlock:Boolean;

 TitleX:LongWord;
 TitleY:LongWord;
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
    Unlock:=True;
     
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
    TitleFont:=Console.Font;
    TitleOffset:=PFramebufferConsole(Console).DesktopOffset;
    if FontGetHeight(Console.Font) > TitleOffset then TitleOffset:=0;
    TitleForecolor:=Console.Forecolor;
    TitleBackcolor:=PFramebufferConsole(Console).DesktopColor;
    
    {Release Lock}
    MutexUnlock(Console.Lock);
    Unlock:=False;
    
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
      {Draw Title}
      Result:=FramebufferConsoleDrawText(Console,TitleFont,FRAMEBUFFER_CONSOLE_TITLE,TitleX,TitleY,TitleForecolor,TitleBackcolor,Length(FRAMEBUFFER_CONSOLE_TITLE));
     end; 
   finally
    if Unlock then MutexUnlock(Console.Lock);
   end; 
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function FramebufferConsoleGetPosition(Console:PConsoleDevice;Position:LongWord;var X1,Y1,X2,Y2:LongWord):LongWord;
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
       
       //To Do //Continuing
       
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
     Console.Console.Device.DeviceBus:=DEVICE_BUS_MMIO; 
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
     Console.Console.DeviceDrawWindow:=FramebufferConsoleDrawWindow;
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
     if CONSOLE_DMA_LINE then Console.Console.Device.DeviceFlags:=Console.Console.Device.DeviceFlags or CONSOLE_FLAG_DMA_LINE;
     if CONSOLE_DMA_FILL then Console.Console.Device.DeviceFlags:=Console.Console.Device.DeviceFlags or CONSOLE_FLAG_DMA_FILL;
     if CONSOLE_DMA_CLEAR then Console.Console.Device.DeviceFlags:=Console.Console.Device.DeviceFlags or CONSOLE_FLAG_DMA_CLEAR;
     if CONSOLE_DMA_SCROLL then Console.Console.Device.DeviceFlags:=Console.Console.Device.DeviceFlags or CONSOLE_FLAG_DMA_SCROLL;
     
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
