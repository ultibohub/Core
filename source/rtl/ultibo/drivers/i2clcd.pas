{
Generic I2C LCD Driver.

Copyright (C) 2021 - SoftOz Pty Ltd.

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


Generic I2C LCD
===============

 Generic 16x2 or 20x4 LCD displays with an I2C interface are available from many suppliers and
 commonly include a Hitachi HD44780 LCD and NXP PCF8574 I/O expander to convert the I2C data into
 GPIO inputs for the display.
 
 This unit combines the drivers for the HD44780 and PCF8574 to create a console device which can
 be used with the standard console and console window API.
 
 Only text mode is supported as the HD44780 does not support setting individual pixels.
 
 Some examples of generic displays that will work with the unit include the following:
 
  https://www.amazon.com/SunFounder-Serial-Module-Display-Arduino/dp/B019K5X53O
  
  https://www.auselectronicsdirect.com.au/2-x-16-lcd-display-module-with-i2c-interface-for-a

 But you can find these units from many suppliers worldwide.
 
}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit I2CLCD; 

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,I2C,GPIO,Console,PCF857X,HD44780,SysUtils;

{==============================================================================}
{Global definitions}
{$INCLUDE ..\core\GlobalDefines.inc}

{==============================================================================}
const
 {I2CLCD specific constants}
 I2CLCD_CONSOLE_DESCRIPTION = 'Generic I2C LCD';  {Description of I2CLCD device}

 I2CLCD_SIGNATURE = $00CF8574;
 
 {I2CLCD GPIO constants}
 I2CLCD_PIN_RS        = GPIO_PIN_0; {GPIO pin for the LCD RS line}
 I2CLCD_PIN_RW        = GPIO_PIN_1; {GPIO pin for the LCD RW line}
 I2CLCD_PIN_EN        = GPIO_PIN_2; {GPIO pin for the LCD EN line}
 I2CLCD_PIN_BACKLIGHT = GPIO_PIN_3; {GPIO pin for the LCD Backlight} 
 I2CLCD_PIN_D4        = GPIO_PIN_4; {GPIO pin for the LCD D4 line}
 I2CLCD_PIN_D5        = GPIO_PIN_5; {GPIO pin for the LCD D5 line}
 I2CLCD_PIN_D6        = GPIO_PIN_6; {GPIO pin for the LCD D6 line}
 I2CLCD_PIN_D7        = GPIO_PIN_7; {GPIO pin for the LCD D7 line}
 
{==============================================================================}
type
 {I2CLCD specific types}
 PI2CLCDDisplay = ^TI2CLCDDisplay;
 TI2CLCDDisplay = record
  Signature:LongWord;     {Signature for entry validation}
  Width:LongWord;         {Width in columns of this display}
  Height:LongWord;        {Height in rows of this display}
  I2C:PI2CDevice;         {I2C device for this display}
  GPIO:PGPIODevice;       {GPIO (PCF8574) device for this display}
  Console:PConsoleDevice; {Console (HD44780) device for this display}
 end;

{==============================================================================}
var
 {I2CLCD specific variables}
 I2CLCD_AUTOSTART:LongBool = True;    {If True then auto start the I2CLCD device on boot}
 
 I2CLCD_I2C_ADDRESS:Word = $27;
 I2CLCD_I2C_DEVICE:String = 'I2C0';
 
 I2CLCD_LCD_WIDTH:LongWord = 16;
 I2CLCD_LCD_HEIGHT:LongWord = 2;
 
{==============================================================================}
{Initialization Functions}
procedure I2CLCDInit;

function I2CLCDStart(const Device:String;Address:Word;Width,Height:LongWord):THandle;
function I2CLCDStop(Handle:THandle):Boolean;

{==============================================================================}
{I2CLCD Functions}
function I2CLCDBacklightOn(Handle:THandle):Boolean;
function I2CLCDBacklightOff(Handle:THandle):Boolean;

{==============================================================================}
{I2CLCD Helper Functions}
 
{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {I2CLCD specific variables}
 I2CLCDInitialized:Boolean;
 
 I2CLCDDefault:THandle = INVALID_HANDLE_VALUE;
 
{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure I2CLCDInit;
{Initialize the I2CLCD unit and parameters}

{Note: Called only during system startup}
var
 WorkInt:LongWord;
 WorkBuffer:String;
begin
 {}
 {Check Initialized}
 if I2CLCDInitialized then Exit;
 
 {Check Environment Variables}
 {I2CLCD_AUTOSTART}
 WorkInt:=StrToIntDef(SysUtils.GetEnvironmentVariable('I2CLCD_AUTOSTART'),1);
 if WorkInt = 0 then I2CLCD_AUTOSTART:=False;

 {I2CLCD_I2C_ADDRESS}
 WorkInt:=StrToIntDef(SysUtils.GetEnvironmentVariable('I2CLCD_I2C_ADDRESS'),0);
 if WorkInt > 0 then I2CLCD_I2C_ADDRESS:=WorkInt;
 
 {I2CLCD_I2C_DEVICE}
 WorkBuffer:=SysUtils.GetEnvironmentVariable('I2CLCD_I2C_DEVICE');
 if Length(WorkBuffer) <> 0 then I2CLCD_I2C_DEVICE:=WorkBuffer;
 
 {I2CLCD_LCD_WIDTH}
 WorkInt:=StrToIntDef(SysUtils.GetEnvironmentVariable('I2CLCD_LCD_WIDTH'),I2CLCD_LCD_WIDTH);
 if WorkInt <> I2CLCD_LCD_WIDTH then I2CLCD_LCD_WIDTH:=WorkInt;

 {I2CLCD_LCD_HEIGHT}
 WorkInt:=StrToIntDef(SysUtils.GetEnvironmentVariable('I2CLCD_LCD_HEIGHT'),I2CLCD_LCD_HEIGHT);
 if WorkInt <> I2CLCD_LCD_HEIGHT then I2CLCD_LCD_HEIGHT:=WorkInt;
 
 {Start I2CLCD} 
 if I2CLCD_AUTOSTART then
  begin
   I2CLCDDefault:=I2CLCDStart(I2CLCD_I2C_DEVICE,I2CLCD_I2C_ADDRESS,I2CLCD_LCD_WIDTH,I2CLCD_LCD_HEIGHT);
  end;
 
 I2CLCDInitialized:=True;
end;

{==============================================================================}

function I2CLCDStart(const Device:String;Address:Word;Width,Height:LongWord):THandle;
{Start the I2CLCD driver and register the GPIO and Console devices associated with the display}
{Device: The I2C device that the PCF8574 I/O Expander on the display is connected to}
{Address: The I2C address of the PCF8574 I/O Expander on the display}
{Width: The width in columns of the HD44780 LCD on the display}
{Height: The height in rows of the HD44780 LCD on the display}
{Return: The handle of the I2CLCD on success or INVALID_HANDLE_VALUE on failure}

{Note: This function will be called during startup if the parameter I2CLCD_AUTOSTART is True
       Can be called multiple times to support more than one LCD display}
var
 I2C:PI2CDevice;
 GPIO:PGPIODevice;
 Console:PConsoleDevice;
 I2CLCDDisplay:PI2CLCDDisplay;
begin
 {}
 Result:=INVALID_HANDLE_VALUE;
 
 {Check Device}
 if Length(Device) = 0 then Exit;
 
 {Check Address}
 if Address = I2C_ADDRESS_INVALID then Exit;
 
 {Check Width}
 if Width < 1 then Width:=16;
 
 {Check Height}
 if Height < 1 then Height:=2;
 
 {Check I2C Device}
 I2C:=PI2CDevice(DeviceFindByName(Device));
 if I2C = nil then
  begin
   I2C:=PI2CDevice(DeviceFindByDescription(Device));
   if I2C = nil then Exit;
  end;

 {Create GPIO Device}
 GPIO:=PCF8574GPIOCreate(I2C,Address);
 if GPIO = nil then Exit;
 try
  {Create Console Device}
  Console:=HD44780ConsoleCreate(GPIO,I2CLCD_CONSOLE_DESCRIPTION,Width,Height,I2CLCD_PIN_RS,I2CLCD_PIN_RW,I2CLCD_PIN_EN,I2CLCD_PIN_D4,I2CLCD_PIN_D5,I2CLCD_PIN_D6,I2CLCD_PIN_D7);
  if Console = nil then Exit;
  try
   {Create I2CLCD}
   I2CLCDDisplay:=AllocMem(SizeOf(TI2CLCDDisplay));
   if I2CLCDDisplay = nil then Exit;
   
   {Update I2CLCD}
   I2CLCDDisplay.Signature:=I2CLCD_SIGNATURE;
   I2CLCDDisplay.Width:=Width;
   I2CLCDDisplay.Height:=Height;
   I2CLCDDisplay.I2C:=I2C;
   I2CLCDDisplay.GPIO:=GPIO;
   I2CLCDDisplay.Console:=Console;
   
   {Setup Backlight}
   GPIODeviceFunctionSelect(GPIO,I2CLCD_PIN_BACKLIGHT,GPIO_FUNCTION_OUT);
   
   {Return Result}
   Result:=THandle(I2CLCDDisplay);
   
   {Check Default}
   if I2CLCDDefault = INVALID_HANDLE_VALUE then
    begin
     I2CLCDDefault:=Result;
    end;
  finally
   if Result = INVALID_HANDLE_VALUE then HD44780ConsoleDestroy(Console);
  end;  
 finally
  if Result = INVALID_HANDLE_VALUE then PCF857XGPIODestroy(GPIO);
 end;
end;

{==============================================================================}

function I2CLCDStop(Handle:THandle):Boolean;
{Stop the I2CLCD driver and deregister the GPIO and Console devices associated with the display}
{Handle: The handle of the I2CLCD or INVALID_HANDLE_VALUE for the default display}
{Return: True if completed or False on failure}
var
 I2CLCDDisplay:PI2CLCDDisplay;
begin
 {}
 Result:=False;
 
 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Handle:=I2CLCDDefault;
 if Handle = INVALID_HANDLE_VALUE then Exit;
 
 {Get Display}
 I2CLCDDisplay:=PI2CLCDDisplay(Handle);
 if I2CLCDDisplay = nil then Exit;
 if I2CLCDDisplay.Signature <> I2CLCD_SIGNATURE then Exit;
 
 {Check Console Device}
 if I2CLCDDisplay.Console <> nil then
  begin
   {Destroy Console Device}
   if HD44780ConsoleDestroy(I2CLCDDisplay.Console) = ERROR_SUCCESS then
    begin
     {Update I2CLCD}
     I2CLCDDisplay.Console:=nil;
     
     {Check GPIO Device}
     if I2CLCDDisplay.GPIO <> nil then
      begin
       {Destroy GPIO Device}
       if PCF857XGPIODestroy(I2CLCDDisplay.GPIO) = ERROR_SUCCESS then
        begin
         {Update I2CLCD}
         I2CLCDDisplay.GPIO:=nil;
         
         {Check Default}
         if I2CLCDDefault = THandle(PtrUInt(I2CLCDDisplay)) then
          begin
           I2CLCDDefault:=INVALID_HANDLE_VALUE;
          end;
         
         {Invalidate I2CLCD}
         I2CLCDDisplay.Signature:=0;
         
         {Destroy I2CLCD}
         FreeMem(I2CLCDDisplay);
         
         {Return Result}
         Result:=True;
        end; 
      end;  
    end;
  end;
end;

{==============================================================================}
{==============================================================================}
{I2CLCD Functions}
function I2CLCDBacklightOn(Handle:THandle):Boolean;
{Turn on the backlight on the I2CLCD display}
{Handle: The handle of the I2CLCD or INVALID_HANDLE_VALUE for the default display}
{Return: True if completed or False on failure}
var
 I2CLCDDisplay:PI2CLCDDisplay;
begin
 {}
 Result:=False;
 
 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Handle:=I2CLCDDefault;
 if Handle = INVALID_HANDLE_VALUE then Exit;
 
 {Get Display}
 I2CLCDDisplay:=PI2CLCDDisplay(Handle);
 if I2CLCDDisplay = nil then Exit;
 if I2CLCDDisplay.Signature <> I2CLCD_SIGNATURE then Exit;
 
 {Turn on Backlight}
 GPIODeviceOutputSet(I2CLCDDisplay.GPIO,I2CLCD_PIN_BACKLIGHT,GPIO_LEVEL_HIGH);

 Result:=True; 
end;

{==============================================================================}

function I2CLCDBacklightOff(Handle:THandle):Boolean;
{Turn off the backlight on the I2CLCD display}
{Handle: The handle of the I2CLCD or INVALID_HANDLE_VALUE for the default display}
{Return: True if completed or False on failure}
var
 I2CLCDDisplay:PI2CLCDDisplay;
begin
 {}
 Result:=False;
 
 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Handle:=I2CLCDDefault;
 if Handle = INVALID_HANDLE_VALUE then Exit;
 
 {Get Display}
 I2CLCDDisplay:=PI2CLCDDisplay(Handle);
 if I2CLCDDisplay = nil then Exit;
 if I2CLCDDisplay.Signature <> I2CLCD_SIGNATURE then Exit;
 
 {Turn off Backlight}
 GPIODeviceOutputSet(I2CLCDDisplay.GPIO,I2CLCD_PIN_BACKLIGHT,GPIO_LEVEL_LOW);

 Result:=True; 
end;

{==============================================================================}
{==============================================================================}
{I2CLCD Helper Functions}

{==============================================================================}
{==============================================================================}

initialization
 I2CLCDInit;
 
{==============================================================================}
 
{finalization}
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
