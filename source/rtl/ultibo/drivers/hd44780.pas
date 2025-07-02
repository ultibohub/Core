{
Hitachi HD44780 LCD controller Driver.

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

  Adafruit - https://github.com/adafruit/Adafruit_Python_CharLCD
 
References
==========

  HD44780 - https://www.sparkfun.com/datasheets/LCD/HD44780.pdf
            https://en.wikipedia.org/wiki/Hitachi_HD44780_LCD_controller
  
Hitachi HD44780
===============
 
 The Hitachi HD44780 is a dot matrix liquid crystal display controller. It supports 5x8
 or 5x10 characters and includes a 240 character font in ROM, 80 bytes of character RAM
 and 64 bytes of character generator RAM for custom characters.
 
 The device support 4 bit or 8 bit modes, and has a range of standard functions for display
 clear, display on/off, cursor on/off, character blink, cursor shift etc.
 
 This unit creates a Console device to represent the LCD and provides the standard console
 device functions (where appropriate) to draw text, clear the screen, scroll text and 
 manage the cursor.
 
 Currently only 4 bit mode is supported but extension to include 8 bit mode is possible
 with some additional setup and support functions.
 
}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit HD44780; 

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,Console,Font,GPIO,SysUtils;

{==============================================================================}
{Global definitions}
{$INCLUDE ..\core\GlobalDefines.inc}

{==============================================================================}
const
 {HD44780 specific constants}
 HD44780_CONSOLE_DESCRIPTION = 'Hitachi HD44780 LCD Controller';  {Description of HD44780 device}
 
 {HD44780 Instruction constants}
 HD44780_CLEARDISPLAY   = $01;
 HD44780_RETURNHOME     = $02;
 HD44780_ENTRYMODESET   = $04;
 HD44780_DISPLAYCONTROL = $08;
 HD44780_CURSORSHIFT    = $10;
 HD44780_FUNCTIONSET    = $20;
 HD44780_SETCGRAMADDR   = $40;
 HD44780_SETDDRAMADDR   = $80;
 
 {HD44780 Entry Mode Set constants}
 HD44780_ENTRYMODESET_RIGHT    = $00;
 HD44780_ENTRYMODESET_LEFT     = $02;
 HD44780_ENTRYMODESET_SHIFTINC = $01;
 HD44780_ENTRYMODESET_SHIFTDEC = $00;
 
 {HD44780 Display Control constants}
 HD44780_DISPLAYCONTROL_DISPLAYON  = $04;
 HD44780_DISPLAYCONTROL_DISPLAYOFF = $00;
 HD44780_DISPLAYCONTROL_CURSORON   = $02;
 HD44780_DISPLAYCONTROL_CURSOROFF  = $00;
 HD44780_DISPLAYCONTROL_BLINKON    = $01;
 HD44780_DISPLAYCONTROL_BLINKOFF   = $00;
 
 {HD44780 Cursor Shift constants}
 HD44780_CURSORSHIFT_DISPLAY = $08;
 HD44780_CURSORSHIFT_CURSOR  = $00;
 HD44780_CURSORSHIFT_RIGHT   = $04;
 HD44780_CURSORSHIFT_LEFT    = $00;

 {HD44780 Function Set constants}
 HD44780_FUNCTIONSET_8BITMODE = $10;
 HD44780_FUNCTIONSET_4BITMODE = $00;
 HD44780_FUNCTIONSET_2LINE    = $08;
 HD44780_FUNCTIONSET_1LINE    = $00;
 HD44780_FUNCTIONSET_5X10DOTS = $04;
 HD44780_FUNCTIONSET_5X8DOTS  = $00;
 
 {HD44780 Row Offset constants (4 rows)}
 HD44780_ROW_OFFSETS:array[0..3] of Byte = ($00,$40,$14,$54);
 
{==============================================================================}
type
 {HD44780 specific types}
 PHD44780Character = ^THD44780Character;
 THD44780Character = array[0..7] of Byte;
 
 PHD44780Console = ^THD44780Console;
 THD44780Console = record
  {Console Properties}
  Console:TConsoleDevice;
  {HD44780 Properties}
  GPIO:PGPIODevice;           {The GPIO device this Console is connected to}
  RS:LongWord;                {The GPIO pin for the RS line}
  RW:LongWord;                {The GPIO pin for the RW line}
  EN:LongWord;                {The GPIO pin for the EN line}
  D4:LongWord;                {The GPIO pin for the D4 line}
  D5:LongWord;                {The GPIO pin for the D5 line}
  D6:LongWord;                {The GPIO pin for the D6 line}
  D7:LongWord;                {The GPIO pin for the D7 line}
  EntryMode:Byte;             {Current value of the Entry Mode settings}
  FunctionSet:Byte;           {Current value of the Function Set settings}
  DisplayControl:Byte;        {Current value of the Display Control settings}
  {Cursor Properties}
  CursorX:Byte;
  CursorY:Byte;
  {Buffer Properties}
  Size:LongWord;
  Buffer:PByte;
 end; 

{==============================================================================}
{var}
 {HD44780 specific variables}
 
{==============================================================================}
{Initialization Functions}
 
{==============================================================================}
{HD44780 Functions}
function HD44780ConsoleCreate(GPIO:PGPIODevice;const Name:String;Width,Height,RS,RW,EN,D4,D5,D6,D7:LongWord):PConsoleDevice;{$IFDEF API_EXPORT_HD44780} stdcall; public name 'hd44780_console_create';{$ENDIF}
function HD44780ConsoleDestroy(Console:PConsoleDevice):LongWord;{$IFDEF API_EXPORT_HD44780} stdcall; public name 'hd44780_console_destroy';{$ENDIF}

{==============================================================================}
{HD44780 Console Functions}
function HD44780ConsoleOpen(Console:PConsoleDevice):LongWord;
function HD44780ConsoleClose(Console:PConsoleDevice):LongWord;

function HD44780ConsoleClear(Console:PConsoleDevice;Color:LongWord):LongWord;
function HD44780ConsoleScroll(Console:PConsoleDevice;X1,Y1,X2,Y2,Count,Direction:LongWord):LongWord;
 
function HD44780ConsoleDrawChar(Console:PConsoleDevice;Handle:TFontHandle;Ch:Char;X,Y,Forecolor,Backcolor:LongWord):LongWord;
function HD44780ConsoleDrawText(Console:PConsoleDevice;Handle:TFontHandle;const Text:String;X,Y,Forecolor,Backcolor,Len:LongWord):LongWord;
function HD44780ConsoleDrawBlock(Console:PConsoleDevice;X1,Y1,X2,Y2,Color:LongWord):LongWord;
function HD44780ConsoleDrawWindow(Console:PConsoleDevice;Handle:TWindowHandle;Flags:LongWord):LongWord;

function HD44780ConsolePutText(Console:PConsoleDevice;Handle:TFontHandle;const Source,Dest:TConsolePoint;Buffer:PConsoleChar;Width,Height,Skip:LongWord):LongWord;

function HD44780ConsoleGetPosition(Console:PConsoleDevice;Position:LongWord;var X1,Y1,X2,Y2:LongWord):LongWord;
 
{==============================================================================}
{HD44780 Helper Functions}
function HD44780WriteChar(Console:PHD44780Console;Ch:Char):LongWord;
function HD44780WriteData(Console:PHD44780Console;Value:Byte):LongWord;
function HD44780PulseEnable(Console:PHD44780Console):LongWord;
 
function HD44780CursorHome(Console:PHD44780Console):LongWord;
function HD44780CursorBlink(Console:PHD44780Console;Blink:Boolean):LongWord;
function HD44780CursorVisible(Console:PHD44780Console;Visible:Boolean):LongWord;
function HD44780CursorPosition(Console:PHD44780Console;Row,Col:Byte):LongWord;

function HD44780DisplayEnabled(Console:PHD44780Console;Enabled:Boolean):LongWord;

function HD44780AddCharacter(Console:PHD44780Console;Location:Byte;Character:THD44780Character):LongWord;

//To do //RightToLeft/Scroll/MoveLeft/MoveRight

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
{var}
 {HD44780 specific variables}
 
{==============================================================================}
{==============================================================================}
{Initialization Functions}
 
{==============================================================================}
{==============================================================================}
{HD44780 Functions}
function HD44780ConsoleCreate(GPIO:PGPIODevice;const Name:String;Width,Height,RS,RW,EN,D4,D5,D6,D7:LongWord):PConsoleDevice;{$IFDEF API_EXPORT_HD44780} stdcall;{$ENDIF}
var
 Status:LongWord;

 HD44780Console:PHD44780Console;
begin
 {}
 Result:=nil;
 
 {$IF DEFINED(HD44780_DEBUG) or DEFINED(CONSOLE_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'HD44780: Console Create (Name=' + Name + ' Width=' + IntToStr(Width) + ' Height=' + IntToStr(Height) + ')');
 {$ENDIF}
 
 {Check Width and Height}
 if Width < 1 then Exit;
 if Height < 1 then Exit;
 
 {Check GPIO}
 if GPIO = nil then Exit;
 
 {Setup RS, EN, D4..D7 Pins}
 if GPIODeviceFunctionSelect(GPIO,RS,GPIO_FUNCTION_OUT) <> ERROR_SUCCESS then Exit;
 if GPIODeviceFunctionSelect(GPIO,EN,GPIO_FUNCTION_OUT) <> ERROR_SUCCESS then Exit;
 if GPIODeviceFunctionSelect(GPIO,D4,GPIO_FUNCTION_OUT) <> ERROR_SUCCESS then Exit;
 if GPIODeviceFunctionSelect(GPIO,D5,GPIO_FUNCTION_OUT) <> ERROR_SUCCESS then Exit;
 if GPIODeviceFunctionSelect(GPIO,D6,GPIO_FUNCTION_OUT) <> ERROR_SUCCESS then Exit;
 if GPIODeviceFunctionSelect(GPIO,D7,GPIO_FUNCTION_OUT) <> ERROR_SUCCESS then Exit;
 
 {Setup RW Pin (Write Only)}
 if RW <> GPIO_PIN_UNKNOWN then
  begin
   if GPIODeviceFunctionSelect(GPIO,RW,GPIO_FUNCTION_OUT) <> ERROR_SUCCESS then Exit;
   if GPIODeviceOutputSet(GPIO,RW,GPIO_LEVEL_LOW) <> ERROR_SUCCESS then Exit;
  end; 
 
 {Create Console}
 HD44780Console:=PHD44780Console(ConsoleDeviceCreateEx(SizeOf(THD44780Console)));
 if HD44780Console <> nil then
  begin
   {Update Console}
   {Device}
   HD44780Console.Console.Device.DeviceBus:=GPIO.Device.DeviceBus; 
   HD44780Console.Console.Device.DeviceType:=CONSOLE_TYPE_LCD;
   HD44780Console.Console.Device.DeviceFlags:=CONSOLE_FLAG_SINGLE_WINDOW or CONSOLE_FLAG_HARDWARE_CARET or CONSOLE_FLAG_BLINK_CARET or CONSOLE_FLAG_FULLSCREEN;
   HD44780Console.Console.Device.DeviceData:=nil;
   if Length(Name) <> 0 then HD44780Console.Console.Device.DeviceDescription:=Name else HD44780Console.Console.Device.DeviceDescription:=HD44780_CONSOLE_DESCRIPTION;
   {Console}
   HD44780Console.Console.ConsoleState:=CONSOLE_STATE_CLOSED;
   HD44780Console.Console.ConsoleMode:=CONSOLE_MODE_CHARACTER;
   HD44780Console.Console.DeviceOpen:=HD44780ConsoleOpen;
   HD44780Console.Console.DeviceClose:=HD44780ConsoleClose;
   HD44780Console.Console.DeviceClear:=HD44780ConsoleClear;
   HD44780Console.Console.DeviceScroll:=HD44780ConsoleScroll;
   HD44780Console.Console.DeviceDrawChar:=HD44780ConsoleDrawChar;
   HD44780Console.Console.DeviceDrawText:=HD44780ConsoleDrawText;
   HD44780Console.Console.DeviceDrawBlock:=HD44780ConsoleDrawBlock;
   HD44780Console.Console.DeviceDrawWindow:=HD44780ConsoleDrawWindow;
   HD44780Console.Console.DevicePutText:=HD44780ConsolePutText;
   HD44780Console.Console.DeviceGetPosition:=HD44780ConsoleGetPosition;
   HD44780Console.Console.Width:=Width;
   HD44780Console.Console.Height:=Height;
   HD44780Console.Console.FontRatio:=0; {Font ratio 0 for Character console}
   {HD44780}
   HD44780Console.GPIO:=GPIO;
   HD44780Console.RS:=RS;
   HD44780Console.RW:=RW;
   HD44780Console.EN:=EN;
   HD44780Console.D4:=D4;
   HD44780Console.D5:=D5;
   HD44780Console.D6:=D6;
   HD44780Console.D7:=D7;
   
   {Setup Flags}
   if CONSOLE_LINE_WRAP then HD44780Console.Console.Device.DeviceFlags:=HD44780Console.Console.Device.DeviceFlags or CONSOLE_FLAG_LINE_WRAP;
   if CONSOLE_AUTO_SCROLL then HD44780Console.Console.Device.DeviceFlags:=HD44780Console.Console.Device.DeviceFlags or CONSOLE_FLAG_AUTO_SCROLL;
   if CONSOLE_FOCUS_CURSOR then HD44780Console.Console.Device.DeviceFlags:=HD44780Console.Console.Device.DeviceFlags or CONSOLE_FLAG_FOCUS_CARET;
   
   {Register Console}
   Status:=ConsoleDeviceRegister(@HD44780Console.Console);
   if Status = ERROR_SUCCESS then
    begin
     {Open Console}
     Status:=ConsoleDeviceOpen(@HD44780Console.Console);
     if Status = ERROR_SUCCESS then
      begin
       {Return Result}
       Result:=PConsoleDevice(HD44780Console);
      end
     else 
      begin
       if DEVICE_LOG_ENABLED then DeviceLogError(nil,'HD44780: Failed to open new console device: ' + ErrorToString(Status));
     
       {Deregister Console}
       ConsoleDeviceDeregister(@HD44780Console.Console);
       
       {Destroy Console}
       ConsoleDeviceDestroy(@HD44780Console.Console);
      end;
    end
   else 
    begin
     if DEVICE_LOG_ENABLED then DeviceLogError(nil,'HD44780: Failed to register new console device: ' + ErrorToString(Status));
     
     {Destroy Console}
     ConsoleDeviceDestroy(@HD44780Console.Console);
    end;
  end
 else
  begin
   if DEVICE_LOG_ENABLED then DeviceLogError(nil,'HD44780: Failed to create new console device');
  end;
end;
 
{==============================================================================}
 
function HD44780ConsoleDestroy(Console:PConsoleDevice):LongWord;{$IFDEF API_EXPORT_HD44780} stdcall;{$ENDIF}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Console}
 if Console = nil then Exit;
 
 {$IF DEFINED(HD44780_DEBUG) or DEFINED(CONSOLE_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'HD44780: Console Destroy');
 {$ENDIF}
 
 {Close Console}
 Result:=ConsoleDeviceClose(Console);
 if Result = ERROR_SUCCESS then
  begin
   {Deregister Console}
   Result:=ConsoleDeviceDeregister(Console);
   if Result = ERROR_SUCCESS then
    begin
     {Destroy Console}
     Result:=ConsoleDeviceDestroy(Console);
     if Result <> ERROR_SUCCESS then
      begin
       if DEVICE_LOG_ENABLED then DeviceLogError(nil,'HD44780: Failed to destroy console device: ' + ErrorToString(Result));
      end;
    end
   else
    begin
     if DEVICE_LOG_ENABLED then DeviceLogError(nil,'HD44780: Failed to deregister console device: ' + ErrorToString(Result));
    end;    
  end
 else
  begin
   if DEVICE_LOG_ENABLED then DeviceLogError(nil,'HD44780: Failed to close console device: ' + ErrorToString(Result));
  end;  
end;
 
{==============================================================================}
{==============================================================================}
{HD44780 Console Functions}
function HD44780ConsoleOpen(Console:PConsoleDevice):LongWord;
{Implementation of ConsoleDeviceOpen API for HD44780}
{Note: Not intended to be called directly by applications, use ConsoleDeviceOpen instead}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IF DEFINED(HD44780_DEBUG) or DEFINED(CONSOLE_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'HD44780: Console Open');
 {$ENDIF}
 
 if MutexLock(Console.Lock) = ERROR_SUCCESS then 
  begin
   try
    {Update Console}
    {Console.Width} {Done by Create}
    {Console.Height} {Done by Create}
    
    {Check Colors}
    if Console.Forecolor = COLOR_NONE then Console.Forecolor:=CONSOLE_DEFAULT_FORECOLOR;
    if Console.Backcolor = COLOR_NONE then Console.Backcolor:=CONSOLE_DEFAULT_BACKCOLOR;
    if Console.Bordercolor = COLOR_NONE then Console.Bordercolor:=CONSOLE_DEFAULT_BORDERCOLOR;
    
    {Check Font}
    if Console.Font = INVALID_HANDLE_VALUE then Console.Font:=FontGetDefault;
    {Console.FontRatio} {Done by Create}
    
    {Allocate Buffer}
    PHD44780Console(Console).Size:=Console.Width * Console.Height;
    PHD44780Console(Console).Buffer:=AllocMem(PHD44780Console(Console).Size);
    if PHD44780Console(Console).Buffer = nil then Exit;
    
    {Initialize Control Registers}
    PHD44780Console(Console).EntryMode:=HD44780_ENTRYMODESET_LEFT or HD44780_ENTRYMODESET_SHIFTDEC;
    PHD44780Console(Console).FunctionSet:=HD44780_FUNCTIONSET_4BITMODE or HD44780_FUNCTIONSET_2LINE or HD44780_FUNCTIONSET_5X8DOTS;
    PHD44780Console(Console).DisplayControl:=HD44780_DISPLAYCONTROL_DISPLAYON or HD44780_DISPLAYCONTROL_CURSOROFF or HD44780_DISPLAYCONTROL_BLINKOFF;
    
    {Initialize Display}
    HD44780WriteData(PHD44780Console(Console),$33);
    HD44780WriteData(PHD44780Console(Console),$32);
    
    {Write Control Registers}
    HD44780WriteData(PHD44780Console(Console),HD44780_DISPLAYCONTROL or PHD44780Console(Console).DisplayControl);
    HD44780WriteData(PHD44780Console(Console),HD44780_FUNCTIONSET or PHD44780Console(Console).FunctionSet);
    HD44780WriteData(PHD44780Console(Console),HD44780_ENTRYMODESET or PHD44780Console(Console).EntryMode);
    
    {Update Statistics}
    Inc(Console.OpenCount);
    
    {Clear Console}
    Result:=HD44780ConsoleClear(Console,Console.Backcolor);
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

function HD44780ConsoleClose(Console:PConsoleDevice):LongWord;
{Implementation of ConsoleDeviceClose API for HD44780}
{Note: Not intended to be called directly by applications, use ConsoleDeviceClose instead}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IF DEFINED(HD44780_DEBUG) or DEFINED(CONSOLE_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'HD44780: Console Close');
 {$ENDIF}
 
 if MutexLock(Console.Lock) = ERROR_SUCCESS then 
  begin
   try
    {Clear Console}
    Result:=HD44780ConsoleClear(Console,Console.Backcolor);
    if Result <> ERROR_SUCCESS then Exit;
    
    {Update Console}
    {Console.Width:=0;} {Do not reset}
    {Console.Height:=0;} {Do not reset}
    
    {Release Buffer}
    if PHD44780Console(Console).Buffer <> nil then
     begin
      FreeMem(PHD44780Console(Console).Buffer);
      PHD44780Console(Console).Size:=0;
      PHD44780Console(Console).Buffer:=nil;
     end; 
    
    {Update Statistics}
    Inc(Console.CloseCount);
    
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

function HD44780ConsoleClear(Console:PConsoleDevice;Color:LongWord):LongWord;
{Implementation of ConsoleDeviceClear API for HD44780}
{Note: Not intended to be called directly by applications, use ConsoleDeviceClear instead}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Color}
 if Color = COLOR_NONE then Exit;
 
 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IF DEFINED(HD44780_DEBUG) or DEFINED(CONSOLE_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'HD44780: Console Clear (Color=' + IntToHex(Color,8) + ')');
 {$ENDIF}
 
 if MutexLock(Console.Lock) = ERROR_SUCCESS then 
  begin
   try
    {Clear Buffer}
    FillChar(PHD44780Console(Console).Buffer^,PHD44780Console(Console).Size,32);
    
    {Clear Display}
    HD44780WriteData(PHD44780Console(Console),HD44780_CLEARDISPLAY);
    
    {Delay 3mS}
    MillisecondDelay(3);
    
    {Cursor Home}
    HD44780CursorHome(PHD44780Console(Console));
    
    {Update Statistics}
    Inc(Console.ClearCount);
    
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

function HD44780ConsoleScroll(Console:PConsoleDevice;X1,Y1,X2,Y2,Count,Direction:LongWord):LongWord;
{Implementation of ConsoleDeviceScroll API for HD44780}
{Note: Not intended to be called directly by applications, use ConsoleDeviceScroll instead}
var
 Dest:PByte;
 Source:PByte;
 Buffer:PByte;
 Size:LongWord;
 CurrentX:LongWord;
 CurrentY:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check X1, Y1, X2, Y2 (Must be a line or box)}
 if X1 > X2 then Exit;
 if Y1 > Y2 then Exit;
 if (X1 = X2) and (Y1 = Y2) then Exit; {This would be a character}
 
 {Check Count}
 if Count < 1 then Exit;
 
 {Check Direction}
 if Direction > CONSOLE_DIRECTION_RIGHT then Exit;
 
 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {$IF DEFINED(HD44780_DEBUG) or DEFINED(CONSOLE_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'HD44780: Console Scroll (X1=' + IntToStr(X1) + ' Y1=' + IntToStr(Y1) + ' X2=' + IntToStr(X2) + ' Y2=' + IntToStr(Y2) + ' Count=' + IntToStr(Count) + ')');
 {$ENDIF}
 
 if MutexLock(Console.Lock) = ERROR_SUCCESS then 
  begin
   try
    {Check X1, Y1, X2, Y2}
    if X1 >= Console.Width then Exit;
    if Y1 >= Console.Height then Exit;
    if X2 >= Console.Width then Exit;
    if Y2 >= Console.Height then Exit;

    {Check Direction}
    case Direction of
     CONSOLE_DIRECTION_UP:begin
       {Check Count}
       if Count > Y1 then Exit;
    
       {Get Size}
       Size:=(X2 - X1) + 1;
       if Size < 1 then Exit;
       
       {Scroll Up}
       CurrentY:=Y1;
       while CurrentY <= Y2 do
        begin
         {Get Source}
         Source:=PByte(PHD44780Console(Console).Buffer + (X1 + (CurrentY * Console.Width)));
         
         {Get Dest}
         Dest:=PByte(PHD44780Console(Console).Buffer + (X1 + ((CurrentY - Count) * Console.Width)));
         
         {Copy Buffer}
         System.Move(Source^,Dest^,Size);
         
         {Cursor Position}
         HD44780CursorPosition(PHD44780Console(Console),CurrentY - Count,X1);
         
         {Copy Characters}
         CurrentX:=X1;
         while CurrentX <= X2 do
          begin
           {Write Char}
           HD44780WriteChar(PHD44780Console(Console),Chr(PByte(PHD44780Console(Console).Buffer + (CurrentX + ((CurrentY - Count) * Console.Width)))^));
           
           Inc(CurrentX);
           
           {Update Cursor}
           PHD44780Console(Console).CursorX:=CurrentX;
           
           if CurrentX > X2 then Break;
          end;
          
         if CurrentY = Y2 then Break;
         Inc(CurrentY);
        end;
      end;
     CONSOLE_DIRECTION_DOWN:begin
       {Check Count}
       if (Y2 + Count) >= Console.Height then Exit;
    
       {Get Size}
       Size:=(X2 - X1) + 1;
       if Size < 1 then Exit;
       
       {Scroll Down}
       CurrentY:=Y2;
       while CurrentY >= Y1 do
        begin
         {Get Source}
         Source:=PByte(PHD44780Console(Console).Buffer + (X1 + (CurrentY * Console.Width)));
 
         {Get Dest}
         Dest:=PByte(PHD44780Console(Console).Buffer + (X1 + ((CurrentY + Count) * Console.Width)));

         {Copy Buffer}
         System.Move(Source^,Dest^,Size);
       
         {Cursor Position}
         HD44780CursorPosition(PHD44780Console(Console),CurrentY + Count,X1);
       
         {Copy Characters}
         CurrentX:=X1;
         while CurrentX <= X2 do
          begin
           {Write Char}
           HD44780WriteChar(PHD44780Console(Console),Chr(PByte(PHD44780Console(Console).Buffer + (CurrentX + ((CurrentY + Count) * Console.Width)))^));
           
           Inc(CurrentX);
           
           {Update Cursor}
           PHD44780Console(Console).CursorX:=CurrentX;
           
           if CurrentX > X2 then Break;
          end;
          
         if CurrentY = Y1 then Break;
         Dec(CurrentY);
        end;
      end;
     CONSOLE_DIRECTION_LEFT:begin
       {Check Count}
       if Count > X1 then Exit;
    
       {Get Size}
       Size:=(X2 - X1) + 1;
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
          Source:=PByte(PHD44780Console(Console).Buffer + (X1 + (CurrentY * Console.Width)));
    
          {Get Dest}
          Dest:=PByte(PHD44780Console(Console).Buffer + ((X1 - Count) + (CurrentY * Console.Width)));
    
          {Copy Buffer to Temp}
          System.Move(Source^,Buffer^,Size);
    
          {Copy Temp to Buffer}
          System.Move(Buffer^,Dest^,Size);
    
          {Cursor Position}
          HD44780CursorPosition(PHD44780Console(Console),CurrentY,X1 - Count);
    
          {Copy Characters}
          CurrentX:=X1;
          while CurrentX <= X2 do
           begin
            {Write Char}
            HD44780WriteChar(PHD44780Console(Console),Chr(PByte(PHD44780Console(Console).Buffer + ((CurrentX - Count) + (CurrentY * Console.Width)))^));
            
            Inc(CurrentX);
            
            {Update Cursor}
            PHD44780Console(Console).CursorX:=CurrentX - Count;
            
            if CurrentX > X2 then Break;
           end;
       
          if CurrentY = Y2 then Break;
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
       Size:=(X2 - X1) + 1;
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
          Source:=PByte(PHD44780Console(Console).Buffer + (X1 + (CurrentY * Console.Width)));
    
          {Get Dest}
          Dest:=PByte(PHD44780Console(Console).Buffer + ((X1 + Count) + (CurrentY * Console.Width)));
    
          {Copy Buffer to Temp}
          System.Move(Source^,Buffer^,Size);
    
          {Copy Temp to Buffer}
          System.Move(Buffer^,Dest^,Size);
    
          {Cursor Position}
          HD44780CursorPosition(PHD44780Console(Console),CurrentY,X1 + Count);
    
          {Copy Characters}
          CurrentX:=X1;
          while CurrentX <= X2 do
           begin
            {Write Char}
            HD44780WriteChar(PHD44780Console(Console),Chr(PByte(PHD44780Console(Console).Buffer + ((CurrentX + Count) + (CurrentY * Console.Width)))^));
            
            Inc(CurrentX);
            
            {Update Cursor}
            PHD44780Console(Console).CursorX:=CurrentX + Count;
            
            if CurrentX > X2 then Break;
           end;
       
          if CurrentY = Y2 then Break;
          Inc(CurrentY);
         end;
       finally
        FreeMem(Buffer);
       end;         
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
 
function HD44780ConsoleDrawChar(Console:PConsoleDevice;Handle:TFontHandle;Ch:Char;X,Y,Forecolor,Backcolor:LongWord):LongWord;
{Implementation of ConsoleDeviceDrawChar API for HD44780}
{Note: Not intended to be called directly by applications, use ConsoleDeviceDrawChar instead}
var
 CurrentX:LongWord;
 CurrentY:LongWord;
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
 
 {$IF DEFINED(HD44780_DEBUG) or DEFINED(CONSOLE_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'HD44780: Console Draw Char (X=' + IntToStr(X) + ' Y=' + IntToStr(Y) + ' Forecolor=' + IntToHex(Forecolor,8) + ' Backcolor=' + IntToHex(Backcolor,8) + ')');
 {$ENDIF}
 
 if MutexLock(Console.Lock) = ERROR_SUCCESS then 
  begin
   try
    {Check X, Y}
    if X >= Console.Width then Exit;
    if Y >= Console.Height then Exit;
    
    {Cursor Position}
    HD44780CursorPosition(PHD44780Console(Console),Y,X);

    {Get Position}
    CurrentX:=X;
    CurrentY:=Y;
    
    {Update Buffer}
    PByte(PHD44780Console(Console).Buffer + (CurrentX + (CurrentY * Console.Width)))^:=Ord(Ch);
    
    {Write Char}
    HD44780WriteChar(PHD44780Console(Console),Ch);
    
    {Update Position}
    Inc(CurrentX);
    if CurrentX >= Console.Width then
     begin
      CurrentX:=0;
      Inc(CurrentY);
      
      if CurrentY >= Console.Height then
       begin
        CurrentY:=Console.Height - 1;
       end;
      
      {Cursor Position}
      HD44780CursorPosition(PHD44780Console(Console),CurrentY,CurrentX);
     end
    else
     begin
      {Update Cursor}
      PHD44780Console(Console).CursorX:=CurrentX;
     end;     
     
    {$IF DEFINED(HD44780_DEBUG) or DEFINED(CONSOLE_DEBUG)}
    if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'HD44780:  CursorY=' + IntToStr(PHD44780Console(Console).CursorY) + ' CursorX=' + IntToStr(PHD44780Console(Console).CursorX));
    {$ENDIF}
     
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

function HD44780ConsoleDrawText(Console:PConsoleDevice;Handle:TFontHandle;const Text:String;X,Y,Forecolor,Backcolor,Len:LongWord):LongWord;
{Implementation of ConsoleDeviceDrawText API for HD44780}
{Note: Not intended to be called directly by applications, use ConsoleDeviceDrawText instead}
var
 Count:LongWord;
 CurrentX:LongWord;
 CurrentY:LongWord;
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
 
 {$IF DEFINED(HD44780_DEBUG) or DEFINED(CONSOLE_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'HD44780: Console Draw Text (X=' + IntToStr(X) + ' Y=' + IntToStr(Y) + ' Forecolor=' + IntToHex(Forecolor,8) + ' Backcolor=' + IntToHex(Backcolor,8) + ')');
 {$ENDIF}
 
 if MutexLock(Console.Lock) = ERROR_SUCCESS then 
  begin
   try
    {Check X, Y}
    if X >= Console.Width then Exit;
    if Y >= Console.Height then Exit;
    
    {Cursor Position}
    HD44780CursorPosition(PHD44780Console(Console),Y,X);
    
    {Get Position}
    CurrentX:=X;
    CurrentY:=Y;
    
    {Get Text}
    for Count:=1 to Len do
     begin
      {Update Buffer}
      PByte(PHD44780Console(Console).Buffer + (CurrentX + (CurrentY * Console.Width)))^:=Byte(Text[Count]);
      
      {Write Char}
      HD44780WriteChar(PHD44780Console(Console),Text[Count]);
    
      {Update Position}
      Inc(CurrentX);
      if CurrentX >= Console.Width then
       begin
        CurrentX:=0;
        Inc(CurrentY);
        
        if CurrentY >= Console.Height then
         begin
          CurrentY:=Console.Height - 1;
         end;
        
        {Cursor Position}
        HD44780CursorPosition(PHD44780Console(Console),CurrentY,CurrentX);
       end
      else
       begin
        {Update Cursor}
        PHD44780Console(Console).CursorX:=CurrentX;
       end;     
     end;
    
    {$IF DEFINED(HD44780_DEBUG) or DEFINED(CONSOLE_DEBUG)}
    if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'HD44780:  CursorY=' + IntToStr(PHD44780Console(Console).CursorY) + ' CursorX=' + IntToStr(PHD44780Console(Console).CursorX));
    {$ENDIF}
    
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

function HD44780ConsoleDrawBlock(Console:PConsoleDevice;X1,Y1,X2,Y2,Color:LongWord):LongWord;
{Implementation of ConsoleDeviceDrawBlock API for HD44780}
{Note: Not intended to be called directly by applications, use ConsoleDeviceDrawBlock instead}
var
 CurrentX:LongWord;
 CurrentY:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check X1, Y1, X2, Y2 (Must be a box)}
 if X1 > X2 then Exit; {Allow equal on a character console}
 if Y1 > Y2 then Exit; {Allow equal on a character console}
 
 {Check Color}
 if Color = COLOR_NONE then Exit;
 
 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IF DEFINED(HD44780_DEBUG) or DEFINED(CONSOLE_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'HD44780: Console Draw Block (X1=' + IntToStr(X1) + ' Y1=' + IntToStr(Y1) + ' X2=' + IntToStr(X2) + ' Y2=' + IntToStr(Y2) + ' Color=' + IntToHex(Color,8) + ')');
 {$ENDIF}
 
 if MutexLock(Console.Lock) = ERROR_SUCCESS then 
  begin
   try
    {Check X1, Y1, X2, Y2}
    if X1 >= Console.Width then Exit;
    if Y1 >= Console.Height then Exit;
    if X2 >= Console.Width then Exit;
    if Y2 >= Console.Height then Exit;

    {Cursor Position}
    HD44780CursorPosition(PHD44780Console(Console),Y1,X1);
    
    {Draw Block}
    CurrentY:=Y1;
    while CurrentY <= Y2 do
     begin
      CurrentX:=X1;
      while CurrentX <= X2 do
       begin
        {Update Buffer}
        PByte(PHD44780Console(Console).Buffer + (CurrentX + (CurrentY * Console.Width)))^:=32;
        
        {Write Char}
        HD44780WriteChar(PHD44780Console(Console),Chr(32));
      
        Inc(CurrentX);
        
        {Update Cursor}
        PHD44780Console(Console).CursorX:=CurrentX;
        
        if CurrentX > X2 then Break;
       end; 
       
      Inc(CurrentY);
      if CurrentY > Y2 then Break;
      
      {Cursor Position}
      HD44780CursorPosition(PHD44780Console(Console),CurrentY,X1);
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

function HD44780ConsoleDrawWindow(Console:PConsoleDevice;Handle:TWindowHandle;Flags:LongWord):LongWord;
{Implementation of ConsoleDeviceDrawWindow API for HD44780}
{Note: Not intended to be called directly by applications, use ConsoleDeviceDrawWindow instead}

{Note: Caller must hold the Window lock}
var
 CurrentX:LongWord;
 CurrentY:LongWord;
 Window:PConsoleWindow;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Console}
 if Console = nil then Exit;
 if Console.Device.Signature <> DEVICE_SIGNATURE then Exit; 

 {$IF DEFINED(HD44780_DEBUG) or DEFINED(CONSOLE_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'HD44780: Console Draw Window (Handle=' + IntToHex(Handle,8) + ')');
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
     {Clear Display}
     HD44780WriteData(PHD44780Console(Console),HD44780_CLEARDISPLAY);
    
     {Delay 3mS}
     MillisecondDelay(3);
    
     {Cursor Home}
     HD44780CursorHome(PHD44780Console(Console));
    end;  
   
   Result:=ERROR_SUCCESS;
  end
 else if Window.WindowState = WINDOW_STATE_VISIBLE then
  begin 
   {Check Flags}
   if (Flags and WINDOW_DRAW_FLAG_ALL) = WINDOW_DRAW_FLAG_ALL then
    begin
     {Draw Window}
     for CurrentY:=0 to Console.Height - 1 do
      begin
       {Cursor Position}
       HD44780CursorPosition(PHD44780Console(Console),CurrentY,0);
      
       for CurrentX:=0 to Console.Width - 1 do
        begin
         {Write Char}
         HD44780WriteChar(PHD44780Console(Console),Chr(PByte(PHD44780Console(Console).Buffer + (CurrentX + (CurrentY * Console.Width)))^));
        end;
      end;
     
     {Cursor Position}
     HD44780CursorPosition(PHD44780Console(Console),0,0);
    end; 
   
   Result:=ERROR_SUCCESS;
  end; 
end;

{==============================================================================}

function HD44780ConsolePutText(Console:PConsoleDevice;Handle:TFontHandle;const Source,Dest:TConsolePoint;Buffer:PConsoleChar;Width,Height,Skip:LongWord):LongWord;
{Implementation of ConsoleDevicePutText API for HD44780}
{Note: Not intended to be called directly by applications, use ConsoleDevicePutText instead}
var
 Line:LongWord;
 Count:LongWord;
 Offset:LongWord;
 CurrentX:LongWord;
 CurrentY:LongWord;
 TextBuffer:PConsoleChar;
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
 
 {$IF DEFINED(HD44780_DEBUG) or DEFINED(CONSOLE_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'HD44780: Console Put Text (Width=' + IntToStr(Width) + ' Height=' + IntToStr(Height) + ')');
 {$ENDIF}
 
 if MutexLock(Console.Lock) = ERROR_SUCCESS then 
  begin
   try
    {Check Source}
    if Source.X < 1 then Exit;
    if Source.Y < 1 then Exit;

    {Check Dest}
    if Dest.X >= Console.Width then Exit;
    if Dest.Y >= Console.Height then Exit;
    if (Dest.X + Width) > Console.Width then Exit;
    if (Dest.Y + Height) > Console.Height then Exit;
    
    {Get Text Buffer}
    Offset:=((Width + Skip) * (Source.Y - 1)) + (Source.X - 1);
    TextBuffer:=PConsoleChar(PtrUInt(Buffer) + (Offset  * SizeOf(TConsoleChar))); 
    
    {Get Lines}
    CurrentY:=Dest.Y;
    for Line:=1 to Height do
     begin
      {Cursor Position}
      HD44780CursorPosition(PHD44780Console(Console),CurrentY,Dest.X);
      
      {Get Text}
      CurrentX:=Dest.X;
      for Count:=1 to Width do
       begin
        {Update Buffer}
        PByte(PHD44780Console(Console).Buffer + (CurrentX + (CurrentY * Console.Width)))^:=Byte(Byte(TextBuffer.Ch));
      
        {Write Char}
        HD44780WriteChar(PHD44780Console(Console),TextBuffer.Ch);
        
        Inc(CurrentX);
        
        {Update Text Buffer}
        Inc(TextBuffer);
        
        {Update Cursor}
        PHD44780Console(Console).CursorX:=CurrentX;
       end;
         
      Inc(CurrentY);
      
      {Update Text Buffer}
      Inc(TextBuffer,Skip);
     end; 
    
    {Update Statistics}
    Inc(Console.PutCount);
    
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

function HD44780ConsoleGetPosition(Console:PConsoleDevice;Position:LongWord;var X1,Y1,X2,Y2:LongWord):LongWord;
{Implementation of ConsoleDeviceGetPosition API for HD44780}
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

 {$IF DEFINED(HD44780_DEBUG) or DEFINED(CONSOLE_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'HD44780: Console Get Position (Position=' + IntToStr(Position) + ')');
 {$ENDIF}
 
 if MutexLock(Console.Lock) = ERROR_SUCCESS then 
  begin
   try
    {Check Position}
    if Position = CONSOLE_POSITION_FULLSCREEN then
     begin
      X1:=0;
      Y1:=0;
      X2:=Console.Width - 1;
      Y2:=Console.Height - 1;
      
      Result:=ERROR_SUCCESS;
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
{HD44780 Helper Functions}
function HD44780WriteChar(Console:PHD44780Console;Ch:Char):LongWord;
{Write character data to the HD44780 device}
{Note: Caller must hold the Console lock}
var
 Value:Byte;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Console}
 if Console = nil then Exit;

 {$IF DEFINED(HD44780_DEBUG) or DEFINED(CONSOLE_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'HD44780: Write Char (Ch=' + Ch + ')');
 {$ENDIF}
 
 {Get Char}
 Value:=Ord(Ch);
 
 {Delay 1mS}
 MillisecondDelay(1);
 
 {Set RS to High (Character)}
 GPIODeviceOutputSet(Console.GPIO,Console.RS,GPIO_LEVEL_HIGH);
 
 {Write upper 4 bits}
 GPIODeviceOutputSet(Console.GPIO,Console.D4,(Value shr 4) and 1);
 GPIODeviceOutputSet(Console.GPIO,Console.D5,(Value shr 5) and 1);
 GPIODeviceOutputSet(Console.GPIO,Console.D6,(Value shr 6) and 1);
 GPIODeviceOutputSet(Console.GPIO,Console.D7,(Value shr 7) and 1);
 
 {Pulse Clock Enable}
 HD44780PulseEnable(Console);
 
 {Write lower 4 bits}
 GPIODeviceOutputSet(Console.GPIO,Console.D4,Value and 1);
 GPIODeviceOutputSet(Console.GPIO,Console.D5,(Value shr 1) and 1);
 GPIODeviceOutputSet(Console.GPIO,Console.D6,(Value shr 2) and 1);
 GPIODeviceOutputSet(Console.GPIO,Console.D7,(Value shr 3) and 1);
 
 {Pulse Clock Enable}
 HD44780PulseEnable(Console);
 
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function HD44780WriteData(Console:PHD44780Console;Value:Byte):LongWord;
{Write command data to the HD44780 device}
{Note: Caller must hold the Console lock}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Console}
 if Console = nil then Exit;

 {$IF DEFINED(HD44780_DEBUG) or DEFINED(CONSOLE_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'HD44780: Write Data (Value=' + IntToHex(Value,2) + ')');
 {$ENDIF}
 
 {Delay 1mS}
 MillisecondDelay(1);
 
 {Set RS to Low (Data)}
 GPIODeviceOutputSet(Console.GPIO,Console.RS,GPIO_LEVEL_LOW);
 
 {Write upper 4 bits}
 GPIODeviceOutputSet(Console.GPIO,Console.D4,(Value shr 4) and 1);
 GPIODeviceOutputSet(Console.GPIO,Console.D5,(Value shr 5) and 1);
 GPIODeviceOutputSet(Console.GPIO,Console.D6,(Value shr 6) and 1);
 GPIODeviceOutputSet(Console.GPIO,Console.D7,(Value shr 7) and 1);
 
 {Pulse Clock Enable}
 HD44780PulseEnable(Console);
 
 {Write lower 4 bits}
 GPIODeviceOutputSet(Console.GPIO,Console.D4,Value and 1);
 GPIODeviceOutputSet(Console.GPIO,Console.D5,(Value shr 1) and 1);
 GPIODeviceOutputSet(Console.GPIO,Console.D6,(Value shr 2) and 1);
 GPIODeviceOutputSet(Console.GPIO,Console.D7,(Value shr 3) and 1);
 
 {Pulse Clock Enable}
 HD44780PulseEnable(Console);
 
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function HD44780PulseEnable(Console:PHD44780Console):LongWord;
{Pulse the clock enable line off, on, off to send a command}
{Note: Caller must hold the Console lock}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Console}
 if Console = nil then Exit;

 {$IF DEFINED(HD44780_DEBUG) or DEFINED(CONSOLE_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'HD44780: Pulse Enable');
 {$ENDIF}
 
 {Clock Enable Off}
 GPIODeviceOutputSet(Console.GPIO,Console.EN,GPIO_LEVEL_LOW);
 
 {Delay 1uS (Enable pulse must be > 450ns)}
 MicrosecondDelay(1);
 
 {Clock Enable On}
 GPIODeviceOutputSet(Console.GPIO,Console.EN,GPIO_LEVEL_HIGH);
 
 {Delay 1uS (Enable pulse must be > 450ns)}
 MicrosecondDelay(1);
 
 {Clock Enable Off}
 GPIODeviceOutputSet(Console.GPIO,Console.EN,GPIO_LEVEL_LOW);
 
 {Delay 50uS (Commands need > 37us to settle)}
 MicrosecondDelay(1); {Note: This was set to 50 but seems to work fine at 1}
 
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function HD44780CursorHome(Console:PHD44780Console):LongWord;
{Return the cursor to the top left corner}
{Note: Caller must hold the Console lock}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Console}
 if Console = nil then Exit;

 {$IF DEFINED(HD44780_DEBUG) or DEFINED(CONSOLE_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'HD44780: Cursor Home');
 {$ENDIF}

 {Reset Cursor}
 Console.CursorX:=0;
 Console.CursorY:=0;
 
 {Cursor Home Command}
 HD44780WriteData(Console,HD44780_RETURNHOME);
 
 {Delay 3mS}
 MillisecondDelay(3);
 
 Result:=ERROR_SUCCESS;
end;
 
{==============================================================================}

function HD44780CursorBlink(Console:PHD44780Console;Blink:Boolean):LongWord;
{Set the cursor blinking to on or off}
{Note: Caller must hold the Console lock}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Console}
 if Console = nil then Exit;

 {$IF DEFINED(HD44780_DEBUG) or DEFINED(CONSOLE_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'HD44780: Cursor Blink (Blink=' + BooleanToString(Blink) + ')');
 {$ENDIF}

 if Blink and ((Console.DisplayControl and HD44780_DISPLAYCONTROL_BLINKON) = 0) then
  begin
   {Update Display Control}
   Console.DisplayControl:=Console.DisplayControl or HD44780_DISPLAYCONTROL_BLINKON;
   
   {Write Display Control}
   HD44780WriteData(Console,Console.DisplayControl);
  end
 else if not(Blink) and ((Console.DisplayControl and HD44780_DISPLAYCONTROL_BLINKON) <> 0) then
  begin
   {Update Display Control}
   Console.DisplayControl:=Console.DisplayControl and not(HD44780_DISPLAYCONTROL_BLINKON);

   {Write Display Control}
   HD44780WriteData(Console,Console.DisplayControl);
  end;
 
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function HD44780CursorVisible(Console:PHD44780Console;Visible:Boolean):LongWord;
{Set the cursor to visible or not visible}
{Note: Caller must hold the Console lock}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Console}
 if Console = nil then Exit;

 {$IF DEFINED(HD44780_DEBUG) or DEFINED(CONSOLE_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'HD44780: Cursor Visible (Visible=' + BooleanToString(Visible) + ')');
 {$ENDIF}

 if Visible and ((Console.DisplayControl and HD44780_DISPLAYCONTROL_CURSORON) = 0) then
  begin
   {Update Display Control}
   Console.DisplayControl:=Console.DisplayControl or HD44780_DISPLAYCONTROL_CURSORON;
   
   {Write Display Control}
   HD44780WriteData(Console,Console.DisplayControl);
  end
 else if not(Visible) and ((Console.DisplayControl and HD44780_DISPLAYCONTROL_CURSORON) <> 0) then
  begin
   {Update Display Control}
   Console.DisplayControl:=Console.DisplayControl and not(HD44780_DISPLAYCONTROL_CURSORON);

   {Write Display Control}
   HD44780WriteData(Console,Console.DisplayControl);
  end;
 
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function HD44780CursorPosition(Console:PHD44780Console;Row,Col:Byte):LongWord;
{Set the cursor position to Row, Col}
{Note: Caller must hold the Console lock}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Console}
 if Console = nil then Exit;

 {$IF DEFINED(HD44780_DEBUG) or DEFINED(CONSOLE_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'HD44780: Cursor Position (Row=' + IntToStr(Row) + ' Col=' + IntToStr(Col) + ')');
 {$ENDIF}

 if (Row <> Console.CursorY) or (Col <> Console.CursorX) then
  begin
   {Update Cursor}
   Console.CursorY:=Max(0,Min(Row,Console.Console.Height - 1));
   Console.CursorX:=Max(0,Min(Col,Console.Console.Width - 1));

   {$IF DEFINED(HD44780_DEBUG) or DEFINED(CONSOLE_DEBUG)}
   if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'HD44780:  CursorY=' + IntToStr(Console.CursorY) + ' CursorX=' + IntToStr(Console.CursorX));
   {$ENDIF}
   
   {Write Cursor Position}
   HD44780WriteData(Console,HD44780_SETDDRAMADDR or (Console.CursorX + HD44780_ROW_OFFSETS[Console.CursorY]));
  end;
 
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function HD44780DisplayEnabled(Console:PHD44780Console;Enabled:Boolean):LongWord;
{Set the display state to on or off}
{Note: Caller must hold the Console lock}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Console}
 if Console = nil then Exit;

 {$IF DEFINED(HD44780_DEBUG) or DEFINED(CONSOLE_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'HD44780: Display Enabled (Enabled=' + BooleanToString(Enabled) + ')');
 {$ENDIF}

 if Enabled and ((Console.DisplayControl and HD44780_DISPLAYCONTROL_DISPLAYON) = 0) then
  begin
   {Update Display Control}
   Console.DisplayControl:=Console.DisplayControl or HD44780_DISPLAYCONTROL_DISPLAYON;
   
   {Write Display Control}
   HD44780WriteData(Console,Console.DisplayControl);
  end
 else if not(Enabled) and ((Console.DisplayControl and HD44780_DISPLAYCONTROL_DISPLAYON) <> 0) then
  begin
   {Update Display Control}
   Console.DisplayControl:=Console.DisplayControl and not(HD44780_DISPLAYCONTROL_DISPLAYON);

   {Write Display Control}
   HD44780WriteData(Console,Console.DisplayControl);
  end;
  
 Result:=ERROR_SUCCESS; 
end;

{==============================================================================}

function HD44780AddCharacter(Console:PHD44780Console;Location:Byte;Character:THD44780Character):LongWord;
{Add a custom character to the display}
{Note: Caller must hold the Console lock}
var
 Count:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Console}
 if Console = nil then Exit;

 {$IF DEFINED(HD44780_DEBUG) or DEFINED(CONSOLE_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'HD44780: Add Character (Location=' + IntToStr(Location) + ')');
 {$ENDIF}

 {Set Location}
 HD44780WriteData(Console,HD44780_SETCGRAMADDR or ((Location and $07) shl 3));
 
 {Write Character}
 for Count:=0 to 7 do
  begin
   HD44780WriteChar(Console,Chr(Character[Count]));
  end;  
 
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}
{==============================================================================}

{initialization}
 {Nothing}
 
{==============================================================================}
 
{finalization}
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
