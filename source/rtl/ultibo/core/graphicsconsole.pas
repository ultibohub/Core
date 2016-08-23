{
Ultibo Graphics Console interface unit.

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

 Inspired by the original work of Ronald Daleske (http://www.projekte.daleske.de/)
 
References
==========


Graphics Console
================

 The Graphics console unit provides access to the underlying framebuffer device as a pixel interface that
 allows putting blocks (or rectangles) of pixel data on to the screen or getting rectangles of pixel data
 from the screen. Functions are also included to copy a rectangle from one part of the screen to another
 and to put text (with a specified font) on to the screen.
 
 Most image formats either consist of, or can be decoded to, a block of data that is a number of horizontal
 rows each with a number of pixels (columns) with each pixel containing color information in a certain bit
 size (8/16/24/32) and format (RGB/BGR/RGBA etc). 
 
 If the data exists in memory as a single contiguous block with each row following directly after the previous
 row then that can be passed directly to the functions in this unit (eg GraphicsWindowDrawImage) to be drawn on
 the screen.
 
 Many graphics libraries do image manipulation, rendering and compositing to a memory buffer in a form that
 is compatible with the functions in this unit so a complex image can be constructed in memory and then
 transferred to the screen in a single operation. This is sometimes referred to as a bit block transfer or
 BitBlt and is a very standard technique for rendering both text and images to a framebuffer device.
 
 This unit extends the console unit by creating a new type of console window (WINDOW_MODE_GRAPHICS) and 
 therefore allows both text console windows and graphics console windows to co-exist on the screen at once.
 
 The functions in this unit cannot be used to draw on a text console window (WINDOW_MODE_TEXT) and vice
 versa the functions in the console unit cannot be used to draw on a graphics console window.
 
 Like the console unit, the graphics console supports setting a viewport for a specified window to allow
 masking the area to be written to. However the graphics console does not support a tracking a cursor X
 and Y position. Setting a window as the default is also not supported since that functionality is purely
 intended to support the RTL functions for text output.
 
 Graphics windows can only be created on console devices which have a mode of CONSOLE_MODE_PIXEL.
 
 Notes: Unlike the Console unit, Graphics console coordinates are always based on pixels not characters
        Graphics console coordinates begin at 0,0 and extend to Width - 1, Height - 1
        
        Graphics Window coordinates X,Y are always based on pixels, beginning at 0,0 and extending to Cols - 1, Rows - 1

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit GraphicsConsole; 

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,Framebuffer,Font,Console,SysUtils,Classes;

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}
            
{==============================================================================}
{const}
 {Graphics Console specific constants}

{==============================================================================}
type
 {Graphics Console specific types}
 {Graphics Window Enumeration Callback}
 TGraphicsWindowEnumerate = TConsoleWindowEnumerate;
 
 {Graphics Window}
 PGraphicsWindow = ^TGraphicsWindow;
 TGraphicsWindow = TConsoleWindow;
 
{==============================================================================}
{var}
 {Graphics Console specific variables}
 
{==============================================================================}
type
 {Graphics Console specific classes}
 TGraphicImage = class(TObject)
  private
   {Internal Variables}
 
   {Internal Methods}
   
  protected
   {Protected Variables}
  
   {Protected Methods}
   
  public
   {Public Properties}
   
   {Public Methods}
   //To Do //Continuing
 end;
 
{==============================================================================}
{Initialization Functions}
procedure GraphicsConsoleInit;

{==============================================================================}
{Graphics Console Functions}
function GraphicsWindowCreate(Console:PConsoleDevice;Position:LongWord):TWindowHandle;
function GraphicsWindowCreateEx(Console:PConsoleDevice;Font:TFontHandle;Size,State,Mode,Position:LongWord):TWindowHandle;
function GraphicsWindowDestroy(Handle:TWindowHandle):LongWord; inline;

function GraphicsWindowShow(Handle:TWindowHandle):LongWord; inline;
function GraphicsWindowHide(Handle:TWindowHandle):LongWord; inline;

function GraphicsWindowFind(Console:PConsoleDevice;Position:LongWord):TWindowHandle; inline;
function GraphicsWindowEnumerate(Console:PConsoleDevice;Callback:TGraphicsWindowEnumerate;Data:Pointer):LongWord; inline;

function GraphicsWindowCheckFlag(Handle:TWindowHandle;Flag:LongWord):Boolean; inline;

function GraphicsWindowGetMode(Handle:TWindowHandle):LongWord; inline;
function GraphicsWindowGetState(Handle:TWindowHandle):LongWord; inline;

function GraphicsWindowGetPosition(Handle:TWindowHandle):LongWord; inline;
function GraphicsWindowSetPosition(Handle:TWindowHandle;Position:LongWord):LongWord;

function GraphicsWindowGetMinX(Handle:TWindowHandle):LongWord;
function GraphicsWindowGetMinY(Handle:TWindowHandle):LongWord;
function GraphicsWindowGetMaxX(Handle:TWindowHandle):LongWord;
function GraphicsWindowGetMaxY(Handle:TWindowHandle):LongWord;

function GraphicsWindowGetRect(Handle:TWindowHandle):TConsoleRect; inline;
function GraphicsWindowSetRect(Handle:TWindowHandle;const ARect:TConsoleRect):LongWord; inline;
function GraphicsWindowResetRect(Handle:TWindowHandle):LongWord; inline;

function GraphicsWindowGetViewport(Handle:TWindowHandle;var X1,Y1,X2,Y2:LongWord):LongWord;
function GraphicsWindowSetViewport(Handle:TWindowHandle;X1,Y1,X2,Y2:LongWord):LongWord;
function GraphicsWindowResetViewport(Handle:TWindowHandle):LongWord; 

function GraphicsWindowGetCols(Handle:TWindowHandle):LongWord;
function GraphicsWindowGetRows(Handle:TWindowHandle):LongWord;

function GraphicsWindowGetWidth(Handle:TWindowHandle):LongWord;
function GraphicsWindowGetHeight(Handle:TWindowHandle):LongWord;
function GraphicsWindowGetFormat(Handle:TWindowHandle):LongWord;

function GraphicsWindowGetForecolor(Handle:TWindowHandle):LongWord;
function GraphicsWindowSetForecolor(Handle:TWindowHandle;Color:LongWord):LongWord;
function GraphicsWindowGetBackcolor(Handle:TWindowHandle):LongWord;
function GraphicsWindowSetBackcolor(Handle:TWindowHandle;Color:LongWord):LongWord;

function GraphicsWindowGetFont(Handle:TWindowHandle):TFontHandle;
function GraphicsWindowSetFont(Handle:TWindowHandle;Font:TFontHandle):LongWord;

function GraphicsWindowClear(Handle:TWindowHandle):LongWord;
function GraphicsWindowClearEx(Handle:TWindowHandle;X1,Y1,X2,Y2,Color:LongWord):LongWord;

function GraphicsWindowDrawBox(Handle:TWindowHandle;X1,Y1,X2,Y2,Color,Width:LongWord):LongWord;
function GraphicsWindowDrawLine(Handle:TWindowHandle;X1,Y1,X2,Y2,Color,Width:LongWord):LongWord;
function GraphicsWindowDrawBlock(Handle:TWindowHandle;X1,Y1,X2,Y2,Color:LongWord):LongWord;

function GraphicsWindowDrawChar(Handle:TWindowHandle;Ch:Char;X,Y:LongWord):LongWord;
function GraphicsWindowDrawCharEx(Handle:TWindowHandle;Font:TFontHandle;Ch:Char;X,Y,Forecolor,Backcolor:LongWord):LongWord;

function GraphicsWindowDrawText(Handle:TWindowHandle;const Text:String;X,Y:LongWord):LongWord;
function GraphicsWindowDrawTextEx(Handle:TWindowHandle;Font:TFontHandle;const Text:String;X,Y,Forecolor,Backcolor:LongWord):LongWord;

function GraphicsWindowDrawPixel(Handle:TWindowHandle;X,Y,Color:LongWord):LongWord;
function GraphicsWindowDrawImage(Handle:TWindowHandle;X,Y:LongWord;Image:Pointer;Width,Height,Format:LongWord):LongWord;

function GraphicsWindowGetPixel(Handle:TWindowHandle;X,Y:LongWord):LongWord;
function GraphicsWindowGetImage(Handle:TWindowHandle;X,Y:LongWord;Image:Pointer;Width,Height,Format:LongWord):LongWord;

function GraphicsWindowCopyImage(Handle:TWindowHandle;const Source,Dest:TConsolePoint;Width,Height:LongWord):LongWord; inline;
function GraphicsWindowMoveImage(Handle:TWindowHandle;const Source,Dest:TConsolePoint;Width,Height,Fillcolor:LongWord):LongWord;

function GraphicsWindowImageSize(Handle:TWindowHandle;Width,Height,Format:LongWord):LongWord;

function GraphicsWindowImageFromStream(Handle:TWindowHandle;X,Y:LongWord;Stream:TStream;Width,Height,Format:LongWord):LongWord;
function GraphicsWindowImageToStream(Handle:TWindowHandle;X,Y:LongWord;Stream:TStream;Width,Height,Format:LongWord):LongWord;

{==============================================================================}
{Graphics Console Helper Functions}
function GraphicsWindowGetCount(Console:PConsoleDevice):LongWord; inline;

function GraphicsWindowCheck(Console:PConsoleDevice;Window:PGraphicsWindow):PGraphicsWindow; inline;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {Graphics Console specific variables}
 GraphicsConsoleInitialized:Boolean;

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure GraphicsConsoleInit;
begin
 {}
 {Check Initialized}
 if GraphicsConsoleInitialized then Exit;
 
 {Nothing}
 
 GraphicsConsoleInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{Graphics Console Functions}
function GraphicsWindowCreate(Console:PConsoleDevice;Position:LongWord):TWindowHandle;
{Create a new Graphics window}
{Console: The console device to create the new window on}
{Position: The console position to create the new window at (eg CONSOLE_POSITION_FULL)}
{Return: Handle to new Graphics window or INVALID_HANDLE_VALUE if the window could not be created}
begin
 {}
 Result:=GraphicsWindowCreateEx(Console,INVALID_HANDLE_VALUE,SizeOf(TGraphicsWindow),WINDOW_STATE_VISIBLE,WINDOW_MODE_GRAPHICS,Position);
end;

{==============================================================================}

function GraphicsWindowCreateEx(Console:PConsoleDevice;Font:TFontHandle;Size,State,Mode,Position:LongWord):TWindowHandle;
{Create a new Graphics window}
{Console: The console device to create the new window on}
{Font: The handle of the default font for the new console window}
{Size: The size in bytes to allocate for the new window entry (Defaults to SizeOf(TGraphicsWindow))}
{State: The state of the new console window (WINDOW_STATE_VISIBLE or WINDOW_STATE_INVISIBLE)}
{Mode: The mode of the new console window (Normally WINDOW_MODE_GRAPHICS)}
{Position: The console position to create the new window at (eg CONSOLE_POSITION_FULL)}
{Return: Handle to new Graphics window or INVALID_HANDLE_VALUE if the window could not be created}
var
 X1:LongWord;
 Y1:LongWord;
 X2:LongWord;
 Y2:LongWord;
 Handle:TWindowHandle;
 Window:PGraphicsWindow;
begin
 {}
 Result:=INVALID_HANDLE_VALUE;
 
 {Check Size}
 if Size = 0 then Size:=SizeOf(TGraphicsWindow);
 if Size < SizeOf(TGraphicsWindow) then Exit;
 
 {Check Mode}
 {if Mode <> WINDOW_MODE_GRAPHICS then Exit;} {Do not check mode, allow for expansion}
 
 {Check Console}
 if ConsoleDeviceGetMode(Console) <> CONSOLE_MODE_PIXEL then Exit;
 
 {Check Position}
 if Position = CONSOLE_POSITION_FULLSCREEN then
  begin
   if GraphicsWindowGetCount(Console) <> 0 then Exit;
   if not ConsoleDeviceCheckFlag(Console,CONSOLE_FLAG_FULLSCREEN) then Exit;
  end
 else
  begin 
   {if Position < CONSOLE_POSITION_FULL then Exit;}
   if Position > CONSOLE_POSITION_BOTTOMRIGHT then Exit;
   if GraphicsWindowFind(Console,Position) <> INVALID_HANDLE_VALUE then Exit;
   if GraphicsWindowFind(Console,CONSOLE_POSITION_FULLSCREEN) <> INVALID_HANDLE_VALUE then Exit;
 
   {Check Position}
   case Position of
    CONSOLE_POSITION_FULL:begin
      {Fail if any other windows exist}
      if GraphicsWindowGetCount(Console) <> 0 then Exit;
     end;
    CONSOLE_POSITION_TOP:begin
      {Fail on top positions}
      if GraphicsWindowFind(Console,CONSOLE_POSITION_TOPLEFT) <> INVALID_HANDLE_VALUE then Exit;
      if GraphicsWindowFind(Console,CONSOLE_POSITION_TOPRIGHT) <> INVALID_HANDLE_VALUE then Exit;
      
      {Get Full}
      Handle:=GraphicsWindowFind(Console,CONSOLE_POSITION_FULL);
      if Handle <> INVALID_HANDLE_VALUE then
       begin
        {Move to Bottom}
        if GraphicsWindowSetPosition(Handle,CONSOLE_POSITION_BOTTOM) <> ERROR_SUCCESS then Exit;
       end;
      
      {Get Left}
      Handle:=GraphicsWindowFind(Console,CONSOLE_POSITION_LEFT);
      if Handle <> INVALID_HANDLE_VALUE then
       begin
        {Move to Bottom Left}
        if GraphicsWindowSetPosition(Handle,CONSOLE_POSITION_BOTTOMLEFT) <> ERROR_SUCCESS then Exit;
       end;
       
      {Get Right} 
      Handle:=GraphicsWindowFind(Console,CONSOLE_POSITION_RIGHT);
      if Handle <> INVALID_HANDLE_VALUE then
       begin
        {Move to Bottom Right}
        if GraphicsWindowSetPosition(Handle,CONSOLE_POSITION_BOTTOMRIGHT) <> ERROR_SUCCESS then Exit;
       end;
     end;
    CONSOLE_POSITION_BOTTOM:begin
      {Fail on bottom positions}
      if GraphicsWindowFind(Console,CONSOLE_POSITION_BOTTOMLEFT) <> INVALID_HANDLE_VALUE then Exit;
      if GraphicsWindowFind(Console,CONSOLE_POSITION_BOTTOMRIGHT) <> INVALID_HANDLE_VALUE then Exit;
    
      {Get Full}
      Handle:=GraphicsWindowFind(Console,CONSOLE_POSITION_FULL);
      if Handle <> INVALID_HANDLE_VALUE then
       begin
        {Move to Top}
        if GraphicsWindowSetPosition(Handle,CONSOLE_POSITION_TOP) <> ERROR_SUCCESS then Exit;
       end;
      
      {Get Left}
      Handle:=GraphicsWindowFind(Console,CONSOLE_POSITION_LEFT);
      if Handle <> INVALID_HANDLE_VALUE then
       begin
        {Move to Top Left}
        if GraphicsWindowSetPosition(Handle,CONSOLE_POSITION_TOPLEFT) <> ERROR_SUCCESS then Exit;
       end;
       
      {Get Right} 
      Handle:=GraphicsWindowFind(Console,CONSOLE_POSITION_RIGHT);
      if Handle <> INVALID_HANDLE_VALUE then
       begin
        {Move to Top Right}
        if GraphicsWindowSetPosition(Handle,CONSOLE_POSITION_TOPRIGHT) <> ERROR_SUCCESS then Exit;
       end;
     end;
    CONSOLE_POSITION_LEFT:begin
      {Fail on left positions}
      if GraphicsWindowFind(Console,CONSOLE_POSITION_TOPLEFT) <> INVALID_HANDLE_VALUE then Exit;
      if GraphicsWindowFind(Console,CONSOLE_POSITION_BOTTOMLEFT) <> INVALID_HANDLE_VALUE then Exit;
    
      {Get Full}
      Handle:=GraphicsWindowFind(Console,CONSOLE_POSITION_FULL);
      if Handle <> INVALID_HANDLE_VALUE then
       begin
        {Move to Right}
        if GraphicsWindowSetPosition(Handle,CONSOLE_POSITION_RIGHT) <> ERROR_SUCCESS then Exit;
       end;
    
      {Get Top}
      Handle:=GraphicsWindowFind(Console,CONSOLE_POSITION_TOP);
      if Handle <> INVALID_HANDLE_VALUE then
       begin
        {Move to Top Right}
        if GraphicsWindowSetPosition(Handle,CONSOLE_POSITION_TOPRIGHT) <> ERROR_SUCCESS then Exit;
       end;
       
      {Get Bottom} 
      Handle:=GraphicsWindowFind(Console,CONSOLE_POSITION_BOTTOM);
      if Handle <> INVALID_HANDLE_VALUE then
       begin
        {Move to Bottom Right}
        if GraphicsWindowSetPosition(Handle,CONSOLE_POSITION_BOTTOMRIGHT) <> ERROR_SUCCESS then Exit;
       end;
     end;
    CONSOLE_POSITION_RIGHT:begin
      {Fail on right positions}
      if GraphicsWindowFind(Console,CONSOLE_POSITION_TOPRIGHT) <> INVALID_HANDLE_VALUE then Exit;
      if GraphicsWindowFind(Console,CONSOLE_POSITION_BOTTOMRIGHT) <> INVALID_HANDLE_VALUE then Exit;
    
      {Get Full}
      Handle:=GraphicsWindowFind(Console,CONSOLE_POSITION_FULL);
      if Handle <> INVALID_HANDLE_VALUE then
       begin
        {Move to Left}
        if GraphicsWindowSetPosition(Handle,CONSOLE_POSITION_LEFT) <> ERROR_SUCCESS then Exit;
       end;
    
      {Get Top}
      Handle:=GraphicsWindowFind(Console,CONSOLE_POSITION_TOP);
      if Handle <> INVALID_HANDLE_VALUE then
       begin
        {Move to Top Left}
        if GraphicsWindowSetPosition(Handle,CONSOLE_POSITION_TOPLEFT) <> ERROR_SUCCESS then Exit;
       end;
       
      {Get Bottom} 
      Handle:=GraphicsWindowFind(Console,CONSOLE_POSITION_BOTTOM);
      if Handle <> INVALID_HANDLE_VALUE then
       begin
        {Move to Bottom Left}
        if GraphicsWindowSetPosition(Handle,CONSOLE_POSITION_BOTTOMLEFT) <> ERROR_SUCCESS then Exit;
       end;
     end;
    CONSOLE_POSITION_TOPLEFT:begin
      {Get Full}
      Handle:=GraphicsWindowFind(Console,CONSOLE_POSITION_FULL);
      if Handle <> INVALID_HANDLE_VALUE then
       begin
        {Move to Right}
        if GraphicsWindowSetPosition(Handle,CONSOLE_POSITION_RIGHT) <> ERROR_SUCCESS then Exit;
       end;
    
      {Get Top}
      Handle:=GraphicsWindowFind(Console,CONSOLE_POSITION_TOP);
      if Handle <> INVALID_HANDLE_VALUE then
       begin
        {Move to Top Right}
        if GraphicsWindowSetPosition(Handle,CONSOLE_POSITION_TOPRIGHT) <> ERROR_SUCCESS then Exit;
       end;
    
      {Get Left}
      Handle:=GraphicsWindowFind(Console,CONSOLE_POSITION_LEFT);
      if Handle <> INVALID_HANDLE_VALUE then
       begin
        {Move to Bottom Left}
        if GraphicsWindowSetPosition(Handle,CONSOLE_POSITION_BOTTOMLEFT) <> ERROR_SUCCESS then Exit;
       end;
     end;
    CONSOLE_POSITION_TOPRIGHT:begin 
      {Get Full}
      Handle:=GraphicsWindowFind(Console,CONSOLE_POSITION_FULL);
      if Handle <> INVALID_HANDLE_VALUE then
       begin
        {Move to Left}
        if GraphicsWindowSetPosition(Handle,CONSOLE_POSITION_LEFT) <> ERROR_SUCCESS then Exit;
       end;
    
      {Get Top}
      Handle:=GraphicsWindowFind(Console,CONSOLE_POSITION_TOP);
      if Handle <> INVALID_HANDLE_VALUE then
       begin
        {Move to Top Left}
        if GraphicsWindowSetPosition(Handle,CONSOLE_POSITION_TOPLEFT) <> ERROR_SUCCESS then Exit;
       end;
       
      {Get Right} 
      Handle:=GraphicsWindowFind(Console,CONSOLE_POSITION_RIGHT);
      if Handle <> INVALID_HANDLE_VALUE then
       begin
        {Move to Bottom Right}
        if GraphicsWindowSetPosition(Handle,CONSOLE_POSITION_BOTTOMRIGHT) <> ERROR_SUCCESS then Exit;
       end;
     end;
    CONSOLE_POSITION_BOTTOMLEFT:begin 
      {Get Full}
      Handle:=GraphicsWindowFind(Console,CONSOLE_POSITION_FULL);
      if Handle <> INVALID_HANDLE_VALUE then
       begin
        {Move to Right}
        if GraphicsWindowSetPosition(Handle,CONSOLE_POSITION_RIGHT) <> ERROR_SUCCESS then Exit;
       end;
    
      {Get Bottom} 
      Handle:=GraphicsWindowFind(Console,CONSOLE_POSITION_BOTTOM);
      if Handle <> INVALID_HANDLE_VALUE then
       begin
        {Move to Bottom Right}
        if GraphicsWindowSetPosition(Handle,CONSOLE_POSITION_BOTTOMRIGHT) <> ERROR_SUCCESS then Exit;
       end;
    
      {Get Left}
      Handle:=GraphicsWindowFind(Console,CONSOLE_POSITION_LEFT);
      if Handle <> INVALID_HANDLE_VALUE then
       begin
        {Move to Top Left}
        if GraphicsWindowSetPosition(Handle,CONSOLE_POSITION_TOPLEFT) <> ERROR_SUCCESS then Exit;
       end;
     end;
    CONSOLE_POSITION_BOTTOMRIGHT:begin 
      {Get Full}
      Handle:=GraphicsWindowFind(Console,CONSOLE_POSITION_FULL);
      if Handle <> INVALID_HANDLE_VALUE then
       begin
        {Move to Left}
        if GraphicsWindowSetPosition(Handle,CONSOLE_POSITION_LEFT) <> ERROR_SUCCESS then Exit;
       end;
    
      {Get Bottom} 
      Handle:=GraphicsWindowFind(Console,CONSOLE_POSITION_BOTTOM);
      if Handle <> INVALID_HANDLE_VALUE then
       begin
        {Move to Bottom Left}
        if GraphicsWindowSetPosition(Handle,CONSOLE_POSITION_BOTTOMLEFT) <> ERROR_SUCCESS then Exit;
       end;
       
      {Get Right} 
      Handle:=GraphicsWindowFind(Console,CONSOLE_POSITION_RIGHT);
      if Handle <> INVALID_HANDLE_VALUE then
       begin
        {Move to Top Right}
        if GraphicsWindowSetPosition(Handle,CONSOLE_POSITION_TOPRIGHT) <> ERROR_SUCCESS then Exit;
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
    Window:=PGraphicsWindow(AllocMem(Size));
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
    Window.MinX:=0;
    Window.MinY:=0;
    Window.MaxX:=0;
    Window.MaxY:=0;
    {Window.X}{No X,Y on graphics windows}
    {Window.Y}
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
    {Window.CursorX}{No Cursor on graphics windows}
    {Window.CursorY}
    {Window.CursorMode}
    {Window.CursorBlink}
    {Window.CursorState}
    Window.CursorTimer:=INVALID_HANDLE_VALUE;
    {Driver}
    Window.Lock:=INVALID_HANDLE_VALUE;
    Window.Console:=Console;
   
    {Setup Flags}
    if Position = CONSOLE_POSITION_FULLSCREEN then Window.WindowFlags:=Window.WindowFlags or WINDOW_FLAG_FULLSCREEN;
    {No Character, Line Wrap or Auto Scroll on graphics windows}
    
    {Check Border}
    if Position = CONSOLE_POSITION_FULLSCREEN then Window.Borderwidth:=0; //To do //Would this be better based on another criteria ?
    
    {Get Font}
    if Window.Font = INVALID_HANDLE_VALUE then Window.Font:=Console.Font;
    if Window.Font = INVALID_HANDLE_VALUE then Window.Font:=FontGetDefault;
    if Window.Font = INVALID_HANDLE_VALUE then 
     begin
      if DEVICE_LOG_ENABLED then DeviceLogError(nil,'Failed to get font for graphics window');
      {Free Window}
      FreeMem(Window);
      Exit;
     end;
    
    {Get Font Width / Height}
    Window.FontWidth:=FontGetWidth(Window.Font);
    Window.FontHeight:=FontGetHeight(Window.Font);
    if (Window.FontWidth = 0) or (Window.FontHeight = 0) then
     begin
      if DEVICE_LOG_ENABLED then DeviceLogError(nil,'Failed to get font width and height for graphics window');
      {Free Window}
      FreeMem(Window);
      Exit;
     end;
    Window.FontWidth:=Window.FontWidth * Console.FontRatio;
    if Window.FontWidth = 0 then Window.FontWidth:=1;
    Window.FontHeight:=Window.FontHeight * Console.FontRatio;
    if Window.FontHeight = 0 then Window.FontHeight:=1;
    
    {Get Width / Height}
    Window.Width:=((Window.X2 - Window.X1) + 1) - Window.Borderwidth;
    Window.Height:=((Window.Y2 - Window.Y1) + 1) - Window.Borderwidth;
    if (Window.Width < 2) or (Window.Height < 2) then
     begin
      if DEVICE_LOG_ENABLED then DeviceLogError(nil,'Failed to get window width and height for graphics window');
      {Free Window}
      FreeMem(Window);
      Exit;
     end;
    
    {Get MaxX,Y}
    Window.MaxX:=Window.Width - 1;
    Window.MaxY:=Window.Height - 1;

    {Get Cols / Rows}
    Window.Cols:=Window.Width;
    Window.Rows:=Window.Height;
    
    {Create Lock}
    Window.Lock:=MutexCreateEx(False,MUTEX_DEFAULT_SPINCOUNT,MUTEX_FLAG_RECURSIVE);
    if Window.Lock = INVALID_HANDLE_VALUE then
     begin
      if DEVICE_LOG_ENABLED then DeviceLogError(nil,'Failed to create lock for graphics window');
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
       {No default for graphics windows}
        
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
 
function GraphicsWindowDestroy(Handle:TWindowHandle):LongWord; inline;
{Close and Destroy an existing console window}
{Handle: The handle of the window to destroy}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ConsoleWindowDestroy(Handle);
end;

{==============================================================================}

function GraphicsWindowShow(Handle:TWindowHandle):LongWord; inline;
{Make an existing console window visible and show it on screen}
{Handle: The handle of the window to show}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ConsoleWindowShow(Handle);
end;

{==============================================================================}

function GraphicsWindowHide(Handle:TWindowHandle):LongWord; inline;
{Make an existing console window invisible and hide it on screen}
{Handle: The handle of the window to hide}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ConsoleWindowHide(Handle);
end;

{==============================================================================}

function GraphicsWindowFind(Console:PConsoleDevice;Position:LongWord):TWindowHandle; inline;
{Find an existing console window in the position specified}
{Console: The console device to find the window on}
{Position: The window position to find (eg CONSOLE_POSITION_FULL)}
{Return: The handle of the existing window or INVALID_HANDLE_VALUE if not found}
begin
 {}
 Result:=ConsoleWindowFind(Console,Position);
end;

{==============================================================================}

function GraphicsWindowEnumerate(Console:PConsoleDevice;Callback:TGraphicsWindowEnumerate;Data:Pointer):LongWord; inline;
{Enumerate existing console windows on the specified console device}
{Console: The console device to enumerate windows for}
{Callback: The function to call for each window enumerated}
{Data: A pointer to private data to be passed to the callback (Optional)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ConsoleWindowEnumerate(Console,Callback,Data);
end;

{==============================================================================}

function GraphicsWindowCheckFlag(Handle:TWindowHandle;Flag:LongWord):Boolean; inline;
{Check an existing console window to determine if a flag is set or not}
{Handle: The handle of the window to check}
{Flag: The window flag to check for (eg WINDOW_FLAG_LINE_WRAP)}
{Return: True if the flag is set, False if not set}
begin
 {}
 Result:=ConsoleWindowCheckFlag(Handle,Flag);
end;

{==============================================================================}

function GraphicsWindowGetMode(Handle:TWindowHandle):LongWord; inline;
{Get the window mode of an existing console window}
{Handle: The handle of the window to get the mode for}
{Return: The window mode (eg WINDOW_MODE_GRAPHICS)}
begin
 {}
 Result:=ConsoleWindowGetMode(Handle);
end;

{==============================================================================}

function GraphicsWindowGetState(Handle:TWindowHandle):LongWord; inline;
{Get the window state of an existing console window}
{Handle: The handle of the window to get the state for}
{Return: The window state (eg WINDOW_STATE_INVISIBLE)}
begin
 {}
 Result:=ConsoleWindowGetState(Handle);
end;

{==============================================================================}

function GraphicsWindowGetPosition(Handle:TWindowHandle):LongWord; inline;
{Get the position of an existing console window}
{Handle: The handle of the window to get the position for}
{Return: The window position (eg CONSOLE_POSITION_FULL)}
begin
 {}
 Result:=ConsoleWindowGetPosition(Handle);
end;

{==============================================================================}

function GraphicsWindowSetPosition(Handle:TWindowHandle;Position:LongWord):LongWord;
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
 Window:PGraphicsWindow;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Position}
 {if Position < CONSOLE_POSITION_FULL then Exit;}
 if (Position <> CONSOLE_POSITION_FULLSCREEN) and (Position > CONSOLE_POSITION_BOTTOMRIGHT) then Exit;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;
 
 {Get Window}
 Window:=PGraphicsWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_GRAPHICS then Exit;
 
 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;
 try 
  {Check Console}
  if Window.Console = nil then Exit;
  
  {Check Position}
  if GraphicsWindowFind(Window.Console,Position) <> INVALID_HANDLE_VALUE then Exit;
 
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
   Window.Width:=((Window.X2 - Window.X1) + 1) - Window.Borderwidth;
   Window.Height:=((Window.Y2 - Window.Y1) + 1) - Window.Borderwidth;
   
   {Get MinX,Y / MaxX,Y}
   if Window.MinX >= Window.Width then Window.MinX:=0;
   if Window.MinY >= Window.Height then Window.MinY:=0;
   if Window.MaxX >= Window.Width then Window.MaxX:=Window.Width - 1;
   if Window.MaxY >= Window.Height then Window.MaxY:=Window.Height - 1;
   
   {Get Cols / Rows}
   Window.Cols:=(Window.MaxX - Window.MinX) + 1;
   Window.Rows:=(Window.MaxY - Window.MinY) + 1;
   
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

function GraphicsWindowGetMinX(Handle:TWindowHandle):LongWord;
{Get the current minimum X of the window viewport for an existing console window}
{Handle: The handle of the window to get MinX for}
{Return: The minimum X value for the current window viewport}

{Note: For Graphics Console functions, X is based on screen pixels not characters}
var
 Window:PGraphicsWindow;
begin
 {}
 Result:=0;
 
 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;
 
 {Get Window}
 Window:=PGraphicsWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_GRAPHICS then Exit;
 
 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;
 
 {Get MinX}
 Result:=Window.MinX;
 
 {Unlock Window}
 MutexUnlock(Window.Lock);
end;

{==============================================================================}

function GraphicsWindowGetMinY(Handle:TWindowHandle):LongWord;
{Get the current minimum Y of the window viewport for an existing console window}
{Handle: The handle of the window to get MinY for}
{Return: The minimum Y value for the current window viewport}

{Note: For Graphics Console functions, Y is based on screen pixels not characters}
var
 Window:PGraphicsWindow;
begin
 {}
 Result:=0;
 
 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;
 
 {Get Window}
 Window:=PGraphicsWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_GRAPHICS then Exit;
 
 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;
 
 {Get MinY}
 Result:=Window.MinY;
 
 {Unlock Window}
 MutexUnlock(Window.Lock);
end;

{==============================================================================}

function GraphicsWindowGetMaxX(Handle:TWindowHandle):LongWord;
{Get the current maximum X of the window viewport for an existing console window}
{Handle: The handle of the window to get MaxX for}
{Return: The maximum X value for the current window viewport}

{Note: For Graphics Console functions, X is based on screen pixels not characters}
var
 Window:PGraphicsWindow;
begin
 {}
 Result:=0;
 
 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;
 
 {Get Window}
 Window:=PGraphicsWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_GRAPHICS then Exit;
 
 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;
 
 {Get MaxX}
 Result:=Window.MaxX;
 
 {Unlock Window}
 MutexUnlock(Window.Lock);
end;

{==============================================================================}

function GraphicsWindowGetMaxY(Handle:TWindowHandle):LongWord;
{Get the current maximum Y of the window viewport for an existing console window}
{Handle: The handle of the window to get MaxY for}
{Return: The maximum Y value for the current window viewport}

{Note: For Graphics Console functions, Y is based on screen pixels not characters}
var
 Window:PGraphicsWindow;
begin
 {}
 Result:=0;
 
 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;
 
 {Get Window}
 Window:=PGraphicsWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_GRAPHICS then Exit;
 
 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;
 
 {Get MaxY}
 Result:=Window.MaxY;
 
 {Unlock Window}
 MutexUnlock(Window.Lock);
end;

{==============================================================================}

function GraphicsWindowGetRect(Handle:TWindowHandle):TConsoleRect; inline;
{Get the rectangle X1,Y1,X2,Y2 of the window viewport for an existing console window}
{Handle: The handle of the window to get the rectangle for}
{Return: The rectangle of the current window viewport}

{Note: For Graphics Console functions, Rect is based on screen pixels not characters}
begin
 {}
 GraphicsWindowGetViewport(Handle,Result.X1,Result.Y1,Result.X2,Result.Y2);
end;

{==============================================================================}

function GraphicsWindowSetRect(Handle:TWindowHandle;const ARect:TConsoleRect):LongWord; inline;
{Set the rectangle X1,Y1,X2,Y2 of the window viewport for an existing console window}
{Handle: The handle of the window to set the rectangle for}
{Rect: The rectangle to set for the window viewport}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: For Graphics Console functions, Rect is based on screen pixels not characters}
begin
 {}
 Result:=GraphicsWindowSetViewport(Handle,ARect.X1,ARect.Y1,ARect.X2,ARect.Y2);
end;

{==============================================================================}

function GraphicsWindowResetRect(Handle:TWindowHandle):LongWord; inline;
{Reset the window viewport for an existing console window to the maximum size}
{Handle: The handle of the window to reset the viewport for}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=GraphicsWindowResetViewport(Handle);
end;

{==============================================================================}

function GraphicsWindowGetViewport(Handle:TWindowHandle;var X1,Y1,X2,Y2:LongWord):LongWord;
{Get the X1,Y1,X2,Y2 of the window viewport for an existing console window}
{Handle: The handle of the window to get the viewport for}
{X1: The left edge of the current viewport}
{Y1: The top edge of the current viewport}
{X2: The right edge of the current viewport}
{Y2: The bottom edge of the current viewport}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: For Graphics Console functions, Viewport is based on screen pixels not characters}
var
 Window:PGraphicsWindow;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;
 
 {Get Window}
 Window:=PGraphicsWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_GRAPHICS then Exit;
 
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

function GraphicsWindowSetViewport(Handle:TWindowHandle;X1,Y1,X2,Y2:LongWord):LongWord;
{Set the X1,Y1,X2,Y2 of the window viewport for an existing console window}
{Handle: The handle of the window to get the viewport for}
{X1: The left edge of the window viewport}
{Y1: The top edge of the window viewport}
{X2: The right edge of the window viewport}
{Y2: The bottom edge of the window viewport}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: For Graphics Console functions, Viewport is based on screen pixels not characters}
var
 Window:PGraphicsWindow;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;
  
 {Get Window}
 Window:=PGraphicsWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_GRAPHICS then Exit;
 
 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;

 {Check X1,Y1,X2,Y2}
 if (X1 >= 0) and (Y1 >= 0) and (X1 <= X2) and (Y1 <= Y2) and (X2 < Window.Width) and (Y2 < Window.Height) then
  begin
   {Set Viewport}
   Window.MinX:=X1;
   Window.MinY:=Y1;
   Window.MaxX:=X2;
   Window.MaxY:=Y2;
   {Window.X}{No X,Y on graphics windows}
   {Window.Y}
   Window.Cols:=(Window.MaxX - Window.MinX) + 1;
   Window.Rows:=(Window.MaxY - Window.MinY) + 1;
   
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

function GraphicsWindowResetViewport(Handle:TWindowHandle):LongWord; 
{Reset the window viewport for an existing console window to the maximum size}
{Handle: The handle of the window to reset the viewport for}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Window:PGraphicsWindow;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;
  
 {Get Window}
 Window:=PGraphicsWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_GRAPHICS then Exit;
 
 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;

 {Reset Viewport}
 Window.MinX:=0;
 Window.MinY:=0;
 Window.MaxX:=Window.Width - 1;
 Window.MaxY:=Window.Height - 1;
 {Window.X}{No X,Y on graphics windows}
 {Window.Y}
 Window.Cols:=Window.Width;
 Window.Rows:=Window.Height;
 
 Result:=ERROR_SUCCESS;

 {Unlock Window}
 MutexUnlock(Window.Lock);
end; 
 
{==============================================================================}

function GraphicsWindowGetCols(Handle:TWindowHandle):LongWord;
{Get the current columns of the window viewport for an existing console window}
{Handle: The handle of the window to get columns for}
{Return: The columns value for the current window viewport}

{Note: For Graphics Console functions, Columns is based on screen pixels not characters}
var
 Window:PGraphicsWindow;
begin
 {}
 Result:=0;
 
 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;
 
 {Get Window}
 Window:=PGraphicsWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_GRAPHICS then Exit;
 
 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;
 
 {Get Cols}
 Result:=Window.Cols;
 
 {Unlock Window}
 MutexUnlock(Window.Lock);
end;

{==============================================================================}

function GraphicsWindowGetRows(Handle:TWindowHandle):LongWord;
{Get the current rows of the window viewport for an existing console window}
{Handle: The handle of the window to get rows for}
{Return: The rows value for the current window viewport}

{Note: For Graphics Console functions, Rows is based on screen pixels not characters}
var
 Window:PGraphicsWindow;
begin
 {}
 Result:=0;
 
 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;
 
 {Get Window}
 Window:=PGraphicsWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_GRAPHICS then Exit;
 
 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;
 
 {Get Rows}
 Result:=Window.Rows;
 
 {Unlock Window}
 MutexUnlock(Window.Lock);
end;

{==============================================================================}

function GraphicsWindowGetWidth(Handle:TWindowHandle):LongWord;
{Get the absolute width of an existing console window}
{Handle: The handle of the window to get the width for}
{Return: The absolute width of the window}

{Note: For Graphics Console functions, Width is based on screen pixels not characters}
var
 Window:PGraphicsWindow;
begin
 {}
 Result:=0;
 
 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;
 
 {Get Window}
 Window:=PGraphicsWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_GRAPHICS then Exit;
 
 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;
 
 {Get Width}
 Result:=Window.Width;
 
 {Unlock Window}
 MutexUnlock(Window.Lock);
end;

{==============================================================================}

function GraphicsWindowGetHeight(Handle:TWindowHandle):LongWord;
{Get the absolute height of an existing console window}
{Handle: The handle of the window to get the height for}
{Return: The absolute height of the window}

{Note: For Graphics Console functions, Height is based on screen pixels not characters}
var
 Window:PGraphicsWindow;
begin
 {}
 Result:=0;
 
 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;
 
 {Get Window}
 Window:=PGraphicsWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_GRAPHICS then Exit;
 
 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;
 
 {Get Height}
 Result:=Window.Height;
 
 {Unlock Window}
 MutexUnlock(Window.Lock);
end;

{==============================================================================}

function GraphicsWindowGetFormat(Handle:TWindowHandle):LongWord;
{Get the color format of an existing console window}
{Handle: The handle of the window to get the format for}
{Return: The color format of the window (eg COLOR_FORMAT_ARGB32)}
var
 Window:PGraphicsWindow;
begin
 {}
 Result:=0;
 
 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;
 
 {Get Window}
 Window:=PGraphicsWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_GRAPHICS then Exit;
 
 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;
 
 {Get Format}
 Result:=Window.Format;
 
 {Unlock Window}
 MutexUnlock(Window.Lock);
end;

{==============================================================================}

function GraphicsWindowGetForecolor(Handle:TWindowHandle):LongWord;
{Get the current foreground color of an existing console window}
{Handle: The handle of the window to get the foreground color for}
{Return: The foreground color of the window (eg COLOR_WHITE)}
var
 Window:PGraphicsWindow;
begin
 {}
 Result:=0;
 
 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;
 
 {Get Window}
 Window:=PGraphicsWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_GRAPHICS then Exit;
 
 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;

 {Get Forecolor}
 Result:=Window.Forecolor;
 
 {Unlock Window}
 MutexUnlock(Window.Lock);
end;

{==============================================================================}

function GraphicsWindowSetForecolor(Handle:TWindowHandle;Color:LongWord):LongWord;
{Set the current foreground color of an existing console window}
{Handle: The handle of the window to set the foreground color for}
{Color: The foreground color to set (eg COLOR_WHITE)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Window:PGraphicsWindow;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Color}
 if Color = COLOR_NONE then Exit;
 
 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;
 
 {Get Window}
 Window:=PGraphicsWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_GRAPHICS then Exit;
 
 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;

 {Set Forecolor}
 Window.Forecolor:=Color;
 
 Result:=ERROR_SUCCESS;
 
 {Unlock Window}
 MutexUnlock(Window.Lock);
end;

{==============================================================================}

function GraphicsWindowGetBackcolor(Handle:TWindowHandle):LongWord;
{Get the current background color of an existing console window}
{Handle: The handle of the window to get the background color for}
{Return: The background color of the window (eg COLOR_BLACK)}
var
 Window:PGraphicsWindow;
begin
 {}
 Result:=0;
 
 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;
 
 {Get Window}
 Window:=PGraphicsWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_GRAPHICS then Exit;
 
 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;

 {Get Backcolor}
 Result:=Window.Backcolor;
 
 {Unlock Window}
 MutexUnlock(Window.Lock);
end;

{==============================================================================}

function GraphicsWindowSetBackcolor(Handle:TWindowHandle;Color:LongWord):LongWord;
{Set the current background color of an existing console window}
{Handle: The handle of the window to set the background color for}
{Color: The background color to set (eg COLOR_BLACK)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Window:PGraphicsWindow;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Color}
 if Color = COLOR_NONE then Exit;
 
 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;
 
 {Get Window}
 Window:=PGraphicsWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_GRAPHICS then Exit;
 
 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;

 {Set Backcolor}
 Window.Backcolor:=Color;
 
 Result:=ERROR_SUCCESS;
 
 {Unlock Window}
 MutexUnlock(Window.Lock);
end;

{==============================================================================}

function GraphicsWindowGetFont(Handle:TWindowHandle):TFontHandle;
{Get the default font of an existing console window}
{Handle: The handle of the window to get the default font for}
{Return: The font handle of the default font or INVALID_HANDLE_VALUE on error}
var
 Window:PGraphicsWindow;
begin
 {}
 Result:=INVALID_HANDLE_VALUE;
 
 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;
 
 {Get Window}
 Window:=PGraphicsWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_GRAPHICS then Exit;
 
 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;

 {Get Font}
 Result:=Window.Font;
 
 {Unlock Window}
 MutexUnlock(Window.Lock);
end;

{==============================================================================}

function GraphicsWindowSetFont(Handle:TWindowHandle;Font:TFontHandle):LongWord;
{Set the default font of an existing console window}
{Handle: The handle of the window to set the default font for}
{Font: The font handle of the default font to set}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 FontWidth:LongWord;
 FontHeight:LongWord;
 Window:PGraphicsWindow;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Font}
 if Font = INVALID_HANDLE_VALUE then Exit;
 
 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;
 
 {Get Window}
 Window:=PGraphicsWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_GRAPHICS then Exit;
 
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
  
    {Return Result}
    Result:=ERROR_SUCCESS;
   end; 
 finally 
  {Unlock Window}
  MutexUnlock(Window.Lock);
 end; 
end;
  
{==============================================================================}

function GraphicsWindowClear(Handle:TWindowHandle):LongWord;
{Clear the current viewport of an existing console window}
{Handle: The handle of the window to clear}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 ClearX1:LongWord;
 ClearY1:LongWord;
 ClearX2:LongWord;
 ClearY2:LongWord;
 Window:PGraphicsWindow;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;
 
 {Get Window}
 Window:=PGraphicsWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_GRAPHICS then Exit;
 
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
  ClearX1:=Window.X1 + Window.Borderwidth + Window.OffsetX + Window.MinX;
  ClearY1:=Window.Y1 + Window.Borderwidth + Window.OffsetY + Window.MinY;
  ClearX2:=Window.X1 + Window.Borderwidth + Window.OffsetX + Window.MaxX; 
  ClearY2:=Window.Y1 + Window.Borderwidth + Window.OffsetY + Window.MaxY;
 
  {Console Draw Block}
  Result:=ConsoleDeviceDrawBlock(Window.Console,ClearX1,ClearY1,ClearX2,ClearY2,Window.Backcolor);
 finally
  {Unlock Window}
  MutexUnlock(Window.Lock);
 end; 
end;

{==============================================================================}

function GraphicsWindowClearEx(Handle:TWindowHandle;X1,Y1,X2,Y2,Color:LongWord):LongWord;
{Clear part of the the current viewport of an existing console window}
{Handle: The handle of the window to clear}
{X1: The left edge of the area to clear (relative to current viewport)}
{Y1: The top edge of the area to clear (relative to current viewport)}
{X2: The right edge of the area to clear (relative to current viewport)}
{Y2: The bottom edge of the area to clear (relative to current viewport)}
{Color: The fill color for the area to clear (eg COLOR_WHITE)}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: For Graphics Console functions, Viewport is based on screen pixels not characters}
var
 ClearX1:LongWord;
 ClearY1:LongWord;
 ClearX2:LongWord;
 ClearY2:LongWord;
 Window:PGraphicsWindow;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;
 
 {Get Window}
 Window:=PGraphicsWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_GRAPHICS then Exit;

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
  if X1 > X2 then Exit;
  if Y1 > Y2 then Exit;
  if (Window.MinX + X1) > Window.MaxX then Exit;
  if (Window.MinY + Y1) > Window.MaxY then Exit;
  if (Window.MinX + X2) > Window.MaxX then Exit;
  if (Window.MinY + Y2) > Window.MaxY then Exit;
  
  {Calculate X1,Y1,X2,Y2}
  ClearX1:=Window.X1 + Window.Borderwidth + Window.OffsetX + Window.MinX + X1;
  ClearY1:=Window.Y1 + Window.Borderwidth + Window.OffsetY + Window.MinY + Y1;
  ClearX2:=Window.X1 + Window.Borderwidth + Window.OffsetX + Window.MinX + X2; 
  ClearY2:=Window.Y1 + Window.Borderwidth + Window.OffsetY + Window.MinY + Y2;
 
  {Console Draw Block}
  Result:=ConsoleDeviceDrawBlock(Window.Console,ClearX1,ClearY1,ClearX2,ClearY2,Color);
 finally
  {Unlock Window}
  MutexUnlock(Window.Lock);
 end; 
end;
  
{==============================================================================}

function GraphicsWindowDrawBox(Handle:TWindowHandle;X1,Y1,X2,Y2,Color,Width:LongWord):LongWord;
{Draw a rectangular box outline on an existing console window}
{Handle: The handle of the window to draw on}
{X1: The left edge of the box (relative to current viewport)}
{Y1: The top edge of the box (relative to current viewport)}
{X2: The right edge of the box (relative to current viewport)}
{Y2: The bottom edge of the box (relative to current viewport)}
{Color: The color to draw with (eg COLOR_WHITE)}
{Width: The width of the box outline}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: For Graphics Console functions, Viewport is based on screen pixels not characters}
var
 BoxX1:LongWord;
 BoxY1:LongWord;
 BoxX2:LongWord;
 BoxY2:LongWord;
 Window:PGraphicsWindow;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;
 
 {Get Window}
 Window:=PGraphicsWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_GRAPHICS then Exit;

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
  if X1 > X2 then Exit;
  if Y1 > Y2 then Exit;
  if (Window.MinX + X1) > Window.MaxX then Exit;
  if (Window.MinY + Y1) > Window.MaxY then Exit;
  if (Window.MinX + X2) > Window.MaxX then Exit;
  if (Window.MinY + Y2) > Window.MaxY then Exit;
  
  {Calculate X1,Y1,X2,Y2}
  BoxX1:=Window.X1 + Window.Borderwidth + Window.OffsetX + Window.MinX + X1;
  BoxY1:=Window.Y1 + Window.Borderwidth + Window.OffsetY + Window.MinY + Y1;
  BoxX2:=Window.X1 + Window.Borderwidth + Window.OffsetX + Window.MinX + X2; 
  BoxY2:=Window.Y1 + Window.Borderwidth + Window.OffsetY + Window.MinY + Y2;
 
  {Console Draw Box}
  Result:=ConsoleDeviceDrawBox(Window.Console,BoxX1,BoxY1,BoxX2,BoxY2,Color,Width);
 finally
  {Unlock Window}
  MutexUnlock(Window.Lock);
 end; 
end;

{==============================================================================}

function GraphicsWindowDrawLine(Handle:TWindowHandle;X1,Y1,X2,Y2,Color,Width:LongWord):LongWord;
{Draw a line on an existing console window}
{Handle: The handle of the window to draw on}
{X1: The left starting point of the line (relative to current viewport)}
{Y1: The top starting point of the box (relative to current viewport)}
{X2: The right ending point of the box (relative to current viewport)}
{Y2: The bottom ending point of the box (relative to current viewport)}
{Color: The color to draw with (eg COLOR_WHITE)}
{Width: The width of the line}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: For Graphics Console functions, Viewport is based on screen pixels not characters}
var
 LineX1:LongWord;
 LineY1:LongWord;
 LineX2:LongWord;
 LineY2:LongWord;
 Window:PGraphicsWindow;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;
 
 {Get Window}
 Window:=PGraphicsWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_GRAPHICS then Exit;

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
  if X1 > X2 then Exit;
  if Y1 > Y2 then Exit;
  if (Window.MinX + X1) > Window.MaxX then Exit;
  if (Window.MinY + Y1) > Window.MaxY then Exit;
  if (Window.MinX + X2) > Window.MaxX then Exit;
  if (Window.MinY + Y2) > Window.MaxY then Exit;
 
  {Calculate X1,Y1,X2,Y2}
  LineX1:=Window.X1 + Window.Borderwidth + Window.OffsetX + Window.MinX + X1;
  LineY1:=Window.Y1 + Window.Borderwidth + Window.OffsetY + Window.MinY + Y1;
  LineX2:=Window.X1 + Window.Borderwidth + Window.OffsetX + Window.MinX + X2; 
  LineY2:=Window.Y1 + Window.Borderwidth + Window.OffsetY + Window.MinY + Y2;
 
  {Console Draw Line}
  Result:=ConsoleDeviceDrawLine(Window.Console,LineX1,LineY1,LineX2,LineY2,Color,Width);
 finally
  {Unlock Window}
  MutexUnlock(Window.Lock);
 end; 
end;

{==============================================================================}

function GraphicsWindowDrawBlock(Handle:TWindowHandle;X1,Y1,X2,Y2,Color:LongWord):LongWord;
{Draw a rectangular filled block on an existing console window}
{Handle: The handle of the window to draw on}
{X1: The left edge of the block (relative to current viewport)}
{Y1: The top edge of the block (relative to current viewport)}
{X2: The right edge of the block (relative to current viewport)}
{Y2: The bottom edge of the block (relative to current viewport)}
{Color: The color to draw with (eg COLOR_WHITE)}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: For Graphics Console functions, Viewport is based on screen pixels not characters}
var
 BlockX1:LongWord;
 BlockY1:LongWord;
 BlockX2:LongWord;
 BlockY2:LongWord;
 Window:PGraphicsWindow;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;
 
 {Get Window}
 Window:=PGraphicsWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_GRAPHICS then Exit;

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
  if X1 > X2 then Exit;
  if Y1 > Y2 then Exit;
  if (Window.MinX + X1) > Window.MaxX then Exit;
  if (Window.MinY + Y1) > Window.MaxY then Exit;
  if (Window.MinX + X2) > Window.MaxX then Exit;
  if (Window.MinY + Y2) > Window.MaxY then Exit;
 
  {Calculate X1,Y1,X2,Y2}
  BlockX1:=Window.X1 + Window.Borderwidth + Window.OffsetX + Window.MinX + X1;
  BlockY1:=Window.Y1 + Window.Borderwidth + Window.OffsetY + Window.MinY + Y1;
  BlockX2:=Window.X1 + Window.Borderwidth + Window.OffsetX + Window.MinX + X2; 
  BlockY2:=Window.Y1 + Window.Borderwidth + Window.OffsetY + Window.MinY + Y2;
 
  {Console Draw Block}
  Result:=ConsoleDeviceDrawBlock(Window.Console,BlockX1,BlockY1,BlockX2,BlockY2,Color);
 finally
  {Unlock Window}
  MutexUnlock(Window.Lock);
 end; 
end;

{==============================================================================}

function GraphicsWindowDrawChar(Handle:TWindowHandle;Ch:Char;X,Y:LongWord):LongWord;
{Draw a character on an existing console window}
{Handle: The handle of the window to draw on}
{Ch: The character to draw}
{X: The left starting point of the character (relative to current viewport)}
{Y: The top starting point of the character (relative to current viewport)}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: For Graphics Console functions, Viewport is based on screen pixels not characters}
var
 CharX:LongWord;
 CharY:LongWord;
 Window:PGraphicsWindow;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;
 
 {Get Window}
 Window:=PGraphicsWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_GRAPHICS then Exit;

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
  if (Window.MinX + X) > Window.MaxX then Exit;
  if (Window.MinY + Y) > Window.MaxY then Exit;
 
  {Check Height}
  if (Window.MinY + Y + Window.FontHeight) > Window.MaxY then Exit;
  
  {Check Width}
  if (Window.MinX + X + Window.FontWidth) > Window.MaxX then Exit;
  
  {Calculate X,Y}
  CharX:=Window.X1 + Window.Borderwidth + Window.OffsetX + Window.MinX + X;
  CharY:=Window.Y1 + Window.Borderwidth + Window.OffsetY + Window.MinY + Y;

  {Console Draw Char}
  Result:=ConsoleDeviceDrawChar(Window.Console,Window.Font,Ch,CharX,CharY,Window.Forecolor,Window.Backcolor);
 finally
  {Unlock Window}
  MutexUnlock(Window.Lock);
 end; 
end;

{==============================================================================}

function GraphicsWindowDrawCharEx(Handle:TWindowHandle;Font:TFontHandle;Ch:Char;X,Y,Forecolor,Backcolor:LongWord):LongWord;
{Draw a character on an existing console window}
{Handle: The handle of the window to draw on}
{Font: The handle of the font to draw the character with}
{Ch: The character to draw}
{X: The left starting point of the character (relative to current viewport)}
{Y: The top starting point of the character (relative to current viewport)}
{Forecolor: The foreground color for the character (eg COLOR_WHITE)}
{Backcolor: The background color for the character (eg COLOR_BLACK)}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: For Graphics Console functions, Viewport is based on screen pixels not characters}
var
 CharX:LongWord;
 CharY:LongWord;
 Window:PGraphicsWindow;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Font}
 if Font = INVALID_HANDLE_VALUE then Exit;
 
 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;
 
 {Get Window}
 Window:=PGraphicsWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_GRAPHICS then Exit;

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
  if (Window.MinX + X) > Window.MaxX then Exit;
  if (Window.MinY + Y) > Window.MaxY then Exit;
 
  {Check Height}
  if (Window.MinY + Y + FontGetHeight(Font)) > Window.MaxY then Exit;

  {Check Width}
  if (Window.MinX + X + FontGetWidth(Font)) > Window.MaxX then Exit; 
  
  {Calculate X,Y}
  CharX:=Window.X1 + Window.Borderwidth + Window.OffsetX + Window.MinX + X;
  CharY:=Window.Y1 + Window.Borderwidth + Window.OffsetY + Window.MinY + Y;

  {Console Draw Char}
  Result:=ConsoleDeviceDrawChar(Window.Console,Font,Ch,CharX,CharY,Forecolor,Backcolor);
 finally
  {Unlock Window}
  MutexUnlock(Window.Lock);
 end; 
end;

{==============================================================================}

function GraphicsWindowDrawText(Handle:TWindowHandle;const Text:String;X,Y:LongWord):LongWord;
{Draw a text string on an existing console window}
{Handle: The handle of the window to draw on}
{Text: The text to draw}
{X: The left starting point of the text (relative to current viewport)}
{Y: The top starting point of the text (relative to current viewport)}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: For Graphics Console functions, Viewport is based on screen pixels not characters}
var
 TextX:LongWord;
 TextY:LongWord;
 TextLength:LongWord;
 Window:PGraphicsWindow;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;
 
 {Get Window}
 Window:=PGraphicsWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_GRAPHICS then Exit;

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
  if (Window.MinX + X) > Window.MaxX then Exit;
  if (Window.MinY + Y) > Window.MaxY then Exit;
  
  {Check Height}
  if (Window.MinY + Y + Window.FontHeight) > Window.MaxY then Exit;
  
  {Get Length}
  TextLength:=Length(Text);
  if (Window.MinX + X + (TextLength * Window.FontWidth)) > Window.MaxX then TextLength:=(Window.MaxX - (Window.MinX + X) + 1) div Window.FontWidth;
  
  {Check Length}
  if (TextLength > 0) then
   begin
    {Calculate X,Y}
    TextX:=Window.X1 + Window.Borderwidth + Window.OffsetX + Window.MinX + X;
    TextY:=Window.Y1 + Window.Borderwidth + Window.OffsetY + Window.MinY + Y;
  
    {Console Draw Text}
    Result:=ConsoleDeviceDrawText(Window.Console,Window.Font,Text,TextX,TextY,Window.Forecolor,Window.Backcolor,TextLength);
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

function GraphicsWindowDrawTextEx(Handle:TWindowHandle;Font:TFontHandle;const Text:String;X,Y,Forecolor,Backcolor:LongWord):LongWord;
{Draw a text string on an existing console window}
{Handle: The handle of the window to draw on}
{Font: The handle of the font to draw the text with}
{Text: The text to draw}
{X: The left starting point of the text (relative to current viewport)}
{Y: The top starting point of the text (relative to current viewport)}
{Forecolor: The foreground color for the text (eg COLOR_WHITE)}
{Backcolor: The background color for the text (eg COLOR_BLACK)}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: For Graphics Console functions, Viewport is based on screen pixels not characters}
var
 TextX:LongWord;
 TextY:LongWord;
 TextLength:LongWord;
 Window:PGraphicsWindow;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Font}
 if Font = INVALID_HANDLE_VALUE then Exit;
 
 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;
 
 {Get Window}
 Window:=PGraphicsWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_GRAPHICS then Exit;

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
  if (Window.MinX + X) > Window.MaxX then Exit;
  if (Window.MinY + Y) > Window.MaxY then Exit;
  
  {Check Height}
  if (Window.MinY + Y + FontGetHeight(Font)) > Window.MaxY then Exit;
  
  {Get Length}
  TextLength:=Length(Text);
  if (Window.MinX + X + (TextLength * FontGetWidth(Font))) > Window.MaxX then TextLength:=(Window.MaxX - (Window.MinX + X) + 1) div FontGetWidth(Font);
  
  {Check Length}
  if (TextLength > 0) then
   begin
    {Calculate X,Y}
    TextX:=Window.X1 + Window.Borderwidth + Window.OffsetX + Window.MinX + X;
    TextY:=Window.Y1 + Window.Borderwidth + Window.OffsetY + Window.MinY + Y;
  
    {Console Draw Text}
    Result:=ConsoleDeviceDrawText(Window.Console,Font,Text,TextX,TextY,Forecolor,Backcolor,TextLength);
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

function GraphicsWindowDrawPixel(Handle:TWindowHandle;X,Y,Color:LongWord):LongWord;
{Draw a single pixel on an existing console window}
{Handle: The handle of the window to draw on}
{X: The column for the pixel (relative to current viewport)}
{Y: The row for the pixel (relative to current viewport)}
{Color: The color for the pixel (eg COLOR_WHITE)}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: For Graphics Console functions, Viewport is based on screen pixels not characters}
var
 PixelX:LongWord;
 PixelY:LongWord;
 Window:PGraphicsWindow;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;
 
 {Get Window}
 Window:=PGraphicsWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_GRAPHICS then Exit;

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
  if (Window.MinX + X) > Window.MaxX then Exit;
  if (Window.MinY + Y) > Window.MaxY then Exit;
 
  {Calculate X,Y}
  PixelX:=Window.X1 + Window.Borderwidth + Window.OffsetX + Window.MinX + X;
  PixelY:=Window.Y1 + Window.Borderwidth + Window.OffsetY + Window.MinY + Y;

  {Console Draw Pixel}
  Result:=ConsoleDeviceDrawPixel(Window.Console,PixelX,PixelY,Color);
 finally
  {Unlock Window}
  MutexUnlock(Window.Lock);
 end; 
end;

{==============================================================================}

function GraphicsWindowDrawImage(Handle:TWindowHandle;X,Y:LongWord;Image:Pointer;Width,Height,Format:LongWord):LongWord;
{Draw an image on an existing console window}
{Handle: The handle of the window to draw on}
{X: The left starting point of the image (relative to current viewport)}
{Y: The top starting point of the image (relative to current viewport)}
{Image: Pointer to the image data in a contiguous block of pixel rows}
{Width: The width in pixels of a row in the image data}
{Height: The height in pixels of all rows in the image data}
{Format: The color format of the image data (eg COLOR_FORMAT_ARGB32) Pass COLOR_FORMAT_UNKNOWN to use the window format}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: For Graphics Console functions, Viewport is based on screen pixels not characters}
var
 ImageX:LongWord;
 ImageY:LongWord;
 ImageSkip:LongWord;
 ImageWidth:LongWord;
 ImageHeight:LongWord;
 ImageFormat:LongWord;
 Window:PGraphicsWindow;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Image}
 if Image = nil then Exit;
 
 {Check Width and Height}
 if (Width = 0) or (Height = 0) then Exit;
 
 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;
 
 {Get Window}
 Window:=PGraphicsWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_GRAPHICS then Exit;

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
  if (Window.MinX + X) > Window.MaxX then Exit;
  if (Window.MinY + Y) > Window.MaxY then Exit;
 
  {Check Width}
  ImageWidth:=Width;
  if (Window.MinX + X + (Width - 1)) > Window.MaxX then
   begin
    ImageWidth:=(Window.MaxX - (Window.MinX + X)) + 1;
   end;
  
  {Check Height}
  ImageHeight:=Height;
  if (Window.MinY + Y + (Height - 1)) > Window.MaxY then 
   begin
    ImageHeight:=(Window.MaxY - (Window.MinY + Y)) + 1;
   end;
  
  {Get Skip}
  ImageSkip:=Width - ImageWidth;
  
  {Get Format}
  ImageFormat:=Format;
  if ImageFormat = COLOR_FORMAT_UNKNOWN then ImageFormat:=Window.Format;
  
  {Calculate X,Y}
  ImageX:=Window.X1 + Window.Borderwidth + Window.OffsetX + Window.MinX + X;
  ImageY:=Window.Y1 + Window.Borderwidth + Window.OffsetY + Window.MinY + Y;

  {Console Draw Image}
  Result:=ConsoleDeviceDrawImage(Window.Console,ImageX,ImageY,Image,ImageWidth,ImageHeight,ImageFormat,ImageSkip);
 finally
  {Unlock Window}
  MutexUnlock(Window.Lock);
 end; 
end;

{==============================================================================}

function GraphicsWindowGetPixel(Handle:TWindowHandle;X,Y:LongWord):LongWord;
{Get a single pixel from an existing console window}
{Handle: The handle of the window to get from}
{X: The column of the pixel (relative to current viewport)}
{Y: The row of the pixel (relative to current viewport)}
{Return: The color of the pixel at X,Y (eg COLOR_WHITE)}

{Note: For Graphics Console functions, Viewport is based on screen pixels not characters}
var
 PixelX:LongWord;
 PixelY:LongWord;
 Window:PGraphicsWindow;
begin
 {}
 Result:=COLOR_NONE;
 
 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;
 
 {Get Window}
 Window:=PGraphicsWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_GRAPHICS then Exit;

 {Check Visible}
 if Window.WindowState <> WINDOW_STATE_VISIBLE then Exit;
 
 {Lock Window}
 if MutexLock(Window.Lock) <> ERROR_SUCCESS then Exit;
 try
  {Check Console}
  if Window.Console = nil then Exit;
 
  {Check X,Y}
  if (Window.MinX + X) > Window.MaxX then Exit;
  if (Window.MinY + Y) > Window.MaxY then Exit;
 
  {Calculate X,Y}
  PixelX:=Window.X1 + Window.Borderwidth + Window.OffsetX + Window.MinX + X;
  PixelY:=Window.Y1 + Window.Borderwidth + Window.OffsetY + Window.MinY + Y;
 
  {Console Get Pixel}
  ConsoleDeviceGetPixel(Window.Console,PixelX,PixelY,Result);
 finally
  {Unlock Window}
  MutexUnlock(Window.Lock);
 end; 
end;

{==============================================================================}

function GraphicsWindowGetImage(Handle:TWindowHandle;X,Y:LongWord;Image:Pointer;Width,Height,Format:LongWord):LongWord;
{Get an image from an existing console window}
{Handle: The handle of the window to get from}
{X: The left starting point of the image (relative to current viewport)}
{Y: The top starting point of the image (relative to current viewport)}
{Image: Pointer to a block of memory large enough to hold the image in a contiguous block of pixel rows}
{Width: The width in pixels of a row of the image}
{Height: The height in pixels of all rows of the image}
{Format: The color format to store in the image data (eg COLOR_FORMAT_ARGB32) Pass COLOR_FORMAT_UNKNOWN to use the window format}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: For Graphics Console functions, Viewport is based on screen pixels not characters}
var
 ImageX:LongWord;
 ImageY:LongWord;
 ImageSkip:LongWord;
 ImageWidth:LongWord;
 ImageHeight:LongWord;
 ImageFormat:LongWord;
 Window:PGraphicsWindow;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Image}
 if Image = nil then Exit;
 
 {Check Width and Height}
 if (Width = 0) or (Height = 0) then Exit;
 
 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;
 
 {Get Window}
 Window:=PGraphicsWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_GRAPHICS then Exit;

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
  if (Window.MinX + X) > Window.MaxX then Exit;
  if (Window.MinY + Y) > Window.MaxY then Exit;
 
  {Check Width}
  ImageWidth:=Width;
  if (Window.MinX + X + (Width - 1)) > Window.MaxX then
   begin
    ImageWidth:=(Window.MaxX - (Window.MinX + X)) + 1;
   end;
 
  {Check Height}
  ImageHeight:=Height;
  if (Window.MinY + Y + (Height - 1)) > Window.MaxY then 
   begin
    ImageHeight:=(Window.MaxY - (Window.MinY + Y)) + 1;
   end;
  
  {Get Skip}
  ImageSkip:=Width - ImageWidth;
  
  {Get Format}
  ImageFormat:=Format;
  if ImageFormat = COLOR_FORMAT_UNKNOWN then ImageFormat:=Window.Format;
  
  {Calculate X,Y}
  ImageX:=Window.X1 + Window.Borderwidth + Window.OffsetX + Window.MinX + X;
  ImageY:=Window.Y1 + Window.Borderwidth + Window.OffsetY + Window.MinY + Y;

  {Console Get Image}
  Result:=ConsoleDeviceGetImage(Window.Console,ImageX,ImageY,Image,ImageWidth,ImageHeight,ImageFormat,ImageSkip);
 finally
  {Unlock Window}
  MutexUnlock(Window.Lock);
 end; 
end;

{==============================================================================}

function GraphicsWindowCopyImage(Handle:TWindowHandle;const Source,Dest:TConsolePoint;Width,Height:LongWord):LongWord; inline;
{Copy an image from one place to another in a existing console window}
{Handle: The handle of the window to copy from and to}
{Source: The starting point (X,Y) to copy the image from}
{Dest: The starting point (X,Y) to copy the image to}
{Width: The width in pixels of each row of the image}
{Height: The height in pixels of all rows of the image}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: For Graphics Console functions, Viewport is based on screen pixels not characters}
begin
 {}
 Result:=GraphicsWindowMoveImage(Handle,Source,Dest,Width,Height,COLOR_NONE);
end;

{==============================================================================}

function GraphicsWindowMoveImage(Handle:TWindowHandle;const Source,Dest:TConsolePoint;Width,Height,Fillcolor:LongWord):LongWord;
{Move an image from one place to another in a existing console window}
{Handle: The handle of the window to copy from and to}
{Source: The starting point (X,Y) to copy the image from}
{Dest: The starting point (X,Y) to copy the image to}
{Width: The width in pixels of each row of the image}
{Height: The height in pixels of all rows of the image}
{Fillcolor: The color to fill the area where the image was moved from (COLOR_NONE for no fill, same as copy)}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: For Graphics Console functions, Viewport is based on screen pixels not characters}
var
 ImageWidth:LongWord;
 ImageHeight:LongWord;
 ImageDest:TConsolePoint;
 ImageSource:TConsolePoint;
 Window:PGraphicsWindow;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;
 
 {Get Window}
 Window:=PGraphicsWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_GRAPHICS then Exit;

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
 
  {Check Source}
  if (Window.MinX + Source.X) > Window.MaxX then Exit;
  if (Window.MinY + Source.Y) > Window.MaxY then Exit;

  {Check Dest}
  if (Window.MinX + Dest.X) > Window.MaxX then Exit;
  if (Window.MinY + Dest.Y) > Window.MaxY then Exit;
  
  {Check Width}
  ImageWidth:=Width;
  {Source}
  if (Window.MinX + Source.X + (ImageWidth - 1)) > Window.MaxX then
   begin
    ImageWidth:=(Window.MaxX - (Window.MinX + Source.X)) + 1;
   end;
  {Dest} 
  if (Window.MinX + Dest.X + (ImageWidth - 1)) > Window.MaxX then
   begin
    ImageWidth:=(Window.MaxX - (Window.MinX + Dest.X)) + 1;
   end;
  
  {Check Height}
  ImageHeight:=Height;
  {Source}
  if (Window.MinY + Source.Y + (ImageHeight - 1)) > Window.MaxY then 
   begin
    ImageHeight:=(Window.MaxY - (Window.MinY + Source.Y)) + 1;
   end;
  {Dest}  
  if (Window.MinY + Dest.Y + (ImageHeight - 1)) > Window.MaxY then 
   begin
    ImageHeight:=(Window.MaxY - (Window.MinY + Dest.Y)) + 1;
   end;
  
  {Calculate Source}
  ImageSource.X:=Window.X1 + Window.Borderwidth + Window.OffsetX + Window.MinX + Source.X;
  ImageSource.Y:=Window.Y1 + Window.Borderwidth + Window.OffsetY + Window.MinY + Source.Y;
  
  {Calculate Dest}
  ImageDest.X:=Window.X1 + Window.Borderwidth + Window.OffsetX + Window.MinX + Dest.X;
  ImageDest.Y:=Window.Y1 + Window.Borderwidth + Window.OffsetY + Window.MinY + Dest.Y;
  
  {Console Copy Image}
  Result:=ConsoleDeviceCopyImage(Window.Console,ImageSource,ImageDest,ImageWidth,ImageHeight);
  if Result <> ERROR_SUCCESS then Exit;
  
  {Check Fill}
  if Fillcolor <> COLOR_NONE then
   begin
    //To Do //Continuing 
   end;
 finally
  {Unlock Window}
  MutexUnlock(Window.Lock);
 end; 
end;

{==============================================================================}

function GraphicsWindowImageSize(Handle:TWindowHandle;Width,Height,Format:LongWord):LongWord;
{Calculate the size in bytes of an image that is Width by Height in the color format specified}
{Handle: The handle of the window for the image}
{Width: The width of the image in pixels}
{Height: The height of the image in pixels}
{Format: The color format to use for the calculation (eg COLOR_FORMAT_ARGB32) Pass COLOR_FORMAT_UNKNOWN to use the window format}
{Return: The size in bytes for an image of the specified size and format or 0 on error}
var
 Window:PGraphicsWindow;
begin
 {}
 Result:=0;
 
 {Check Width and Height}
 if (Width = 0) or (Height = 0) then Exit;
 
 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;
 
 {Get Window}
 Window:=PGraphicsWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_GRAPHICS then Exit;
 
 //To Do //Continuing
end;

{==============================================================================}

function GraphicsWindowImageFromStream(Handle:TWindowHandle;X,Y:LongWord;Stream:TStream;Width,Height,Format:LongWord):LongWord;
{Draw an image to an existing console window from a supplied stream}
{Handle: The handle of the window to draw on}
{X: The left starting point of the image (relative to current viewport)}
{Y: The top starting point of the image (relative to current viewport)}
{Stream: A stream containing the image data in a contiguous block of pixel rows}
{Width: The width in pixels of a row in the image data}
{Height: The height in pixels of all rows in the image data}
{Format: The color format of the image data (eg COLOR_FORMAT_ARGB32) Pass COLOR_FORMAT_UNKNOWN to use the window format}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: For Graphics Console functions, Viewport is based on screen pixels not characters}
var
 Window:PGraphicsWindow;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Stream}
 if Stream = nil then Exit;
 
 {Check Width and Height}
 if (Width = 0) or (Height = 0) then Exit;
 
 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;
 
 {Get Window}
 Window:=PGraphicsWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_GRAPHICS then Exit;

 {Check Visible}
 if Window.WindowState <> WINDOW_STATE_VISIBLE then
  begin
   Result:=ERROR_SUCCESS;
   Exit;
  end;
 
 //To Do //Continuing
end;

{==============================================================================}

function GraphicsWindowImageToStream(Handle:TWindowHandle;X,Y:LongWord;Stream:TStream;Width,Height,Format:LongWord):LongWord;
{Get an image from an existing console window to a supplied stream}
{Handle: The handle of the window to get from}
{X: The left starting point of the image (relative to current viewport)}
{Y: The top starting point of the image (relative to current viewport)}
{Stream: A stream large enough to hold the image in a contiguous block of pixel rows}
{Width: The width in pixels of a row of the image}
{Height: The height in pixels of all rows of the image}
{Format: The color format to store in the image data (eg COLOR_FORMAT_ARGB32) Pass COLOR_FORMAT_UNKNOWN to use the window format}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: For Graphics Console functions, Viewport is based on screen pixels not characters}
var
 Window:PGraphicsWindow;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Stream}
 if Stream = nil then Exit;
 
 {Check Width and Height}
 if (Width = 0) or (Height = 0) then Exit;
 
 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Exit;
 
 {Get Window}
 Window:=PGraphicsWindow(Handle);
 if Window = nil then Exit;
 if Window.Signature <> WINDOW_SIGNATURE then Exit;
 if Window.WindowMode <> WINDOW_MODE_GRAPHICS then Exit;

 {Check Visible}
 if Window.WindowState <> WINDOW_STATE_VISIBLE then
  begin
   Result:=ERROR_SUCCESS;
   Exit;
  end;
 
 //To Do //Continuing
end;

{==============================================================================}
{==============================================================================}
{Graphics Console Helper Functions}
function GraphicsWindowGetCount(Console:PConsoleDevice):LongWord; inline;
{Get the current console window count}
{Console: The console device to get the window count for}
{Return: The current number of console windows on the specified console device}
begin
 {}
 Result:=ConsoleWindowGetCount(Console);
end;

{==============================================================================}

function GraphicsWindowCheck(Console:PConsoleDevice;Window:PGraphicsWindow):PGraphicsWindow; inline;
{Check if a console window entry is valid}
{Console: The console device to search for the window}
{Window: The window entry to check for validity}
{Return: The supplied window if successful or nil on failure}
begin
 {}
 Result:=ConsoleWindowCheck(Console,Window);
end;

{==============================================================================}
{==============================================================================}

initialization
 GraphicsConsoleInit;
 
{==============================================================================}
 
finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
 