{
Adafruit 16x2 Character LCD + Keypad Driver.

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

 Adafruit RGB Positive 16x2 LCD+Keypad Kit for Raspberry Pi
  https://www.adafruit.com/products/1109

 Adafruit RGB Negative 16x2 LCD+Keypad Kit for Raspberry Pi
  https://www.adafruit.com/product/1110

 Adafruit Blue&White 16x2 LCD+Keypad Kit for Raspberry Pi
  https://www.adafruit.com/products/1115

  https://learn.adafruit.com/adafruit-16x2-character-lcd-plus-keypad-for-raspberry-pi

Adafruit 16x2 Character LCD + Keypad
====================================

 The Adafruit 16x2 LCD + Keypad is a kit that includes both a 16x2 LCD display using the Hitachi HD44780 LCD
 controller as well as a custom PCB with an MCP23017 I/O expander to connect it to a Raspberry Pi (any model)
 using I2C. It is available in Monochrome or RGB Positive and Negative versions.

 This unit ties together the various components needed to make one of these boards work with Ultibo by finding
 the correct I2C device, creating the MCP230XX GPIO device, creating the HD44780 Console device and registering
 all of it with the correct parameters for the Adafruit kit.

 The unit also includes functions to read the 5 buttons on the board and control the LCD backlight.

 You will find many LCD display boards based on the Hitachi HD44780 controller and this unit gives an example
 of how to assembler the available units to create your own driver for a different board.

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit AF16x2LCD;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,I2C,GPIO,Console,MCP230XX,HD44780,SysUtils;

{==============================================================================}
{Global definitions}
{$INCLUDE ..\core\GlobalDefines.inc}

{==============================================================================}
const
 {AF16x2LCD specific constants}
 AF16X2LCD_CONSOLE_DESCRIPTION = 'Adafruit 16x2 LCD';  {Description of AF16x2LCD device}

 AF16X2LCD_SIGNATURE = $000AF162;

 AF16X2LCD_MODEL_MONO = 0; {LCD with Monochrome backlight}
 AF16X2LCD_MODEL_RGB  = 1; {LCD with RGB backlight}

 {AF16x2LCD GPIO constants}
 AF16X2LCD_PLATE_RS      = GPIO_PIN_15; {GPIO pin for the LCD RS line}
 AF16X2LCD_PLATE_RW      = GPIO_PIN_14; {GPIO pin for the LCD RW line}
 AF16X2LCD_PLATE_EN      = GPIO_PIN_13; {GPIO pin for the LCD EN line}
 AF16X2LCD_PLATE_D4      = GPIO_PIN_12; {GPIO pin for the LCD D4 line}
 AF16X2LCD_PLATE_D5      = GPIO_PIN_11; {GPIO pin for the LCD D5 line}
 AF16X2LCD_PLATE_D6      = GPIO_PIN_10; {GPIO pin for the LCD D6 line}
 AF16X2LCD_PLATE_D7      = GPIO_PIN_9;  {GPIO pin for the LCD D7 line}
 AF16X2LCD_PLATE_RED     = GPIO_PIN_6;  {GPIO pin for the Backlight Red LED}
 AF16X2LCD_PLATE_GREEN   = GPIO_PIN_7;  {GPIO pin for the Backlight Green LED}
 AF16X2LCD_PLATE_BLUE    = GPIO_PIN_8;  {GPIO pin for the Backlight Blue LED}

 AF16X2LCD_BUTTON_SELECT = GPIO_PIN_0;  {GPIO pin for the Select button}
 AF16X2LCD_BUTTON_RIGHT  = GPIO_PIN_1;  {GPIO pin for the Right button}
 AF16X2LCD_BUTTON_DOWN   = GPIO_PIN_2;  {GPIO pin for the Down button}
 AF16X2LCD_BUTTON_UP     = GPIO_PIN_3;  {GPIO pin for the Up button}
 AF16X2LCD_BUTTON_LEFT   = GPIO_PIN_4;  {GPIO pin for the Left button}

{==============================================================================}
type
 {AF16x2LCD specific types}
 PAF16x2LCDPlate = ^TAF16x2LCDPlate;
 TAF16x2LCDPlate = record
  Signature:LongWord;     {Signature for entry validation}
  Model:LongWord;         {Plate model (eg AF16X2LCD_MODEL_MONO)}
  Invert:LongBool;        {Invert polarity of Backlight (Active low if True)}
  I2C:PI2CDevice;         {I2C device for this plate}
  GPIO:PGPIODevice;       {GPIO (MCP23017) device for this plate}
  Console:PConsoleDevice; {Console (HD44780) device for this plate}
 end;

{==============================================================================}
var
 {AF16x2LCD specific variables}
 AF16X2LCD_MODEL:LongWord = AF16X2LCD_MODEL_RGB;
 AF16X2LCD_I2C_ADDRESS:Word = $20;
 AF16X2LCD_I2C_DEVICE:String = 'I2C0';

{==============================================================================}
{Initialization Functions}
procedure AF16x2LCDInit;

function AF16x2LCDStart(Model:LongWord;Invert:Boolean;const Device:String;Address:Word):THandle;{$IFDEF API_EXPORT_AF16X2LCD} stdcall; public name 'af16x2lcd_start';{$ENDIF}
function AF16x2LCDStop(Handle:THandle):Boolean;{$IFDEF API_EXPORT_AF16X2LCD} stdcall; public name 'af16x2lcd_stop';{$ENDIF}

{==============================================================================}
{AF16x2LCD Functions}
function AF16x2LCDGetButton(Handle:THandle;Button:LongWord):LongWord;{$IFDEF API_EXPORT_AF16X2LCD} stdcall; public name 'af16x2lcd_get_button';{$ENDIF}

function AF16x2LCDBacklightOn(Handle:THandle):Boolean;{$IFDEF API_EXPORT_AF16X2LCD} stdcall; public name 'af16x2lcd_backlight_on';{$ENDIF}
function AF16x2LCDBacklightOff(Handle:THandle):Boolean;{$IFDEF API_EXPORT_AF16X2LCD} stdcall; public name 'af16x2lcd_backlight_off';{$ENDIF}
function AF16x2LCDBacklightColor(Handle:THandle;Red,Green,Blue:Byte):Boolean;{$IFDEF API_EXPORT_AF16X2LCD} stdcall; public name 'af16x2lcd_backlight_color';{$ENDIF}

{==============================================================================}
{AF16x2LCD Helper Functions}

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {AF16x2LCD specific variables}
 AF16x2LCDInitialized:Boolean;

 AF16x2LCDDefault:THandle = INVALID_HANDLE_VALUE;

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure AF16x2LCDInit;
{Initialize the AF16x2LCD unit and parameters}

{Note: Called only during system startup}
var
 WorkInt:LongWord;
 WorkBuffer:String;
begin
 {}
 {Check Initialized}
 if AF16x2LCDInitialized then Exit;

 {Check Environment Variables}
 {AF16X2LCD_AUTOSTART}
 WorkInt:=StrToIntDef(EnvironmentGet('AF16X2LCD_AUTOSTART'),1);
 if WorkInt = 0 then AF16X2LCD_AUTOSTART:=False;

 {AF16X2LCD_MODEL}
 WorkInt:=StrToIntDef(EnvironmentGet('AF16X2LCD_MODEL'),AF16X2LCD_MODEL);
 if WorkInt <> AF16X2LCD_MODEL then AF16X2LCD_MODEL:=WorkInt;

 {AF16X2LCD_I2C_ADDRESS}
 WorkInt:=StrToIntDef(EnvironmentGet('AF16X2LCD_I2C_ADDRESS'),0);
 if WorkInt > 0 then AF16X2LCD_I2C_ADDRESS:=WorkInt;

 {AF16X2LCD_I2C_DEVICE}
 WorkBuffer:=EnvironmentGet('AF16X2LCD_I2C_DEVICE');
 if Length(WorkBuffer) <> 0 then AF16X2LCD_I2C_DEVICE:=WorkBuffer;

 {Start 16x2 LCD}
 if AF16X2LCD_AUTOSTART then
  begin
   AF16x2LCDDefault:=AF16x2LCDStart(AF16X2LCD_MODEL,True,AF16X2LCD_I2C_DEVICE,AF16X2LCD_I2C_ADDRESS);
  end;

 AF16x2LCDInitialized:=True;
end;

{==============================================================================}

function AF16x2LCDStart(Model:LongWord;Invert:Boolean;const Device:String;Address:Word):THandle;{$IFDEF API_EXPORT_AF16X2LCD} stdcall;{$ENDIF}
{Start the AF16x2LCD driver and register the GPIO and Console devices associated with the display}
{Model: The Adafruit 16x2 LCD Plate model (eg AF16X2LCD_MODEL_RGB)}
{Invert: Invert the signal level for the LCD backlight (If True then GPIO_LEVEL_LOW equals On)}
{Device: The I2C device that the MCP23017 I/O Expander on the LCD Plate is connected to}
{Address: The I2C address of the MCP23017 I/O Expander on the LCD Plate}
{Return: The handle of the AF16x2LCD on success or INVALID_HANDLE_VALUE on failure}

{Note: This function will be called during startup if the parameter AF16X2LCD_AUTOSTART is True
       Can be called multiple times to support more than one 16x2 LCD display}
var
 I2C:PI2CDevice;
 GPIO:PGPIODevice;
 Console:PConsoleDevice;
 AF16x2LCDPlate:PAF16x2LCDPlate;
begin
 {}
 Result:=INVALID_HANDLE_VALUE;

 {Check Model}
 if Model > AF16X2LCD_MODEL_RGB then Exit;

 {Check Device}
 if Length(Device) = 0 then Exit;

 {Check Address}
 if Address = I2C_ADDRESS_INVALID then Exit;

 {Check I2C Device}
 I2C:=PI2CDevice(DeviceFindByName(Device));
 if I2C = nil then
  begin
   I2C:=PI2CDevice(DeviceFindByDescription(Device));
   if I2C = nil then Exit;
  end;

 {Create GPIO Device}
 GPIO:=MCP23017GPIOCreate(I2C,Address);
 if GPIO = nil then Exit;
 try
  {Create Console Device}
  Console:=HD44780ConsoleCreate(GPIO,AF16X2LCD_CONSOLE_DESCRIPTION,16,2,AF16X2LCD_PLATE_RS,AF16X2LCD_PLATE_RW,AF16X2LCD_PLATE_EN,AF16X2LCD_PLATE_D4,AF16X2LCD_PLATE_D5,AF16X2LCD_PLATE_D6,AF16X2LCD_PLATE_D7);
  if Console = nil then Exit;
  try
   {Create AF16x2LCD}
   AF16x2LCDPlate:=AllocMem(SizeOf(TAF16x2LCDPlate));
   if AF16x2LCDPlate = nil then Exit;

   {Update AF16x2LCD}
   AF16x2LCDPlate.Signature:=AF16X2LCD_SIGNATURE;
   AF16x2LCDPlate.Model:=Model;
   AF16x2LCDPlate.Invert:=Invert;
   AF16x2LCDPlate.I2C:=I2C;
   AF16x2LCDPlate.GPIO:=GPIO;
   AF16x2LCDPlate.Console:=Console;

   {Setup Buttons}
   {Select}
   GPIODeviceFunctionSelect(GPIO,AF16X2LCD_BUTTON_SELECT,GPIO_FUNCTION_IN);
   GPIODevicePullSelect(GPIO,AF16X2LCD_BUTTON_SELECT,GPIO_PULL_UP);
   {Right}
   GPIODeviceFunctionSelect(GPIO,AF16X2LCD_BUTTON_RIGHT,GPIO_FUNCTION_IN);
   GPIODevicePullSelect(GPIO,AF16X2LCD_BUTTON_RIGHT,GPIO_PULL_UP);
   {Down}
   GPIODeviceFunctionSelect(GPIO,AF16X2LCD_BUTTON_DOWN,GPIO_FUNCTION_IN);
   GPIODevicePullSelect(GPIO,AF16X2LCD_BUTTON_DOWN,GPIO_PULL_UP);
   {Up}
   GPIODeviceFunctionSelect(GPIO,AF16X2LCD_BUTTON_UP,GPIO_FUNCTION_IN);
   GPIODevicePullSelect(GPIO,AF16X2LCD_BUTTON_UP,GPIO_PULL_UP);
   {Left}
   GPIODeviceFunctionSelect(GPIO,AF16X2LCD_BUTTON_LEFT,GPIO_FUNCTION_IN);
   GPIODevicePullSelect(GPIO,AF16X2LCD_BUTTON_LEFT,GPIO_PULL_UP);

   {Setup Backlight}
   GPIODeviceFunctionSelect(GPIO,AF16X2LCD_PLATE_RED,GPIO_FUNCTION_OUT);
   GPIODeviceFunctionSelect(GPIO,AF16X2LCD_PLATE_GREEN,GPIO_FUNCTION_OUT);
   GPIODeviceFunctionSelect(GPIO,AF16X2LCD_PLATE_BLUE,GPIO_FUNCTION_OUT);

   {Return Result}
   Result:=THandle(AF16x2LCDPlate);

   {Check Default}
   if AF16x2LCDDefault = INVALID_HANDLE_VALUE then
    begin
     AF16x2LCDDefault:=Result;
    end;
  finally
   if Result = INVALID_HANDLE_VALUE then HD44780ConsoleDestroy(Console);
  end;
 finally
  if Result = INVALID_HANDLE_VALUE then MCP230XXGPIODestroy(GPIO);
 end;
end;

{==============================================================================}

function AF16x2LCDStop(Handle:THandle):Boolean;{$IFDEF API_EXPORT_AF16X2LCD} stdcall;{$ENDIF}
{Stop the AF16x2LCD driver and deregister the GPIO and Console devices associated with the display}
{Handle: The handle of the AF16x2LCD or INVALID_HANDLE_VALUE for the default display}
{Return: True if completed or False on failure}
var
 AF16x2LCDPlate:PAF16x2LCDPlate;
begin
 {}
 Result:=False;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Handle:=AF16x2LCDDefault;
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {Get Plate}
 AF16x2LCDPlate:=PAF16x2LCDPlate(Handle);
 if AF16x2LCDPlate = nil then Exit;
 if AF16x2LCDPlate.Signature <> AF16X2LCD_SIGNATURE then Exit;

 {Check Console Device}
 if AF16x2LCDPlate.Console <> nil then
  begin
   {Destroy Console Device}
   if HD44780ConsoleDestroy(AF16x2LCDPlate.Console) = ERROR_SUCCESS then
    begin
     {Update AF16x2LCD}
     AF16x2LCDPlate.Console:=nil;

     {Check GPIO Device}
     if AF16x2LCDPlate.GPIO <> nil then
      begin
       {Destroy GPIO Device}
       if MCP230XXGPIODestroy(AF16x2LCDPlate.GPIO) = ERROR_SUCCESS then
        begin
         {Update AF16x2LCD}
         AF16x2LCDPlate.GPIO:=nil;

         {Check Default}
         if AF16x2LCDDefault = THandle(AF16x2LCDPlate) then
          begin
           AF16x2LCDDefault:=INVALID_HANDLE_VALUE;
          end;

         {Invalidate AF16x2LCD}
         AF16x2LCDPlate.Signature:=0;

         {Destroy AF16x2LCD}
         FreeMem(AF16x2LCDPlate);

         {Return Result}
         Result:=True;
        end;
      end;
    end;
  end;
end;

{==============================================================================}
{==============================================================================}
{AF16x2LCD Functions}
function AF16x2LCDGetButton(Handle:THandle;Button:LongWord):LongWord;{$IFDEF API_EXPORT_AF16X2LCD} stdcall;{$ENDIF}
{Get the GPIO level of a button on the AF16x2LCD display}
{Handle: The handle of the AF16x2LCD or INVALID_HANDLE_VALUE for the default display}
{Button: The button to get the level for (eg AF16X2LCD_BUTTON_LEFT)}
{Return: The GPIO level of the button (eg GPIO_LEVEL_HIGH) or GPIO_LEVEL_UNKNOWN on failure}
var
 AF16x2LCDPlate:PAF16x2LCDPlate;
begin
 {}
 Result:=GPIO_LEVEL_UNKNOWN;

 {Check Button}
 if Button < AF16X2LCD_BUTTON_SELECT then Exit;
 if Button > AF16X2LCD_BUTTON_LEFT then Exit;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Handle:=AF16x2LCDDefault;
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {Get Plate}
 AF16x2LCDPlate:=PAF16x2LCDPlate(Handle);
 if AF16x2LCDPlate = nil then Exit;
 if AF16x2LCDPlate.Signature <> AF16X2LCD_SIGNATURE then Exit;

 {Get Button}
 Result:=GPIODeviceInputGet(AF16x2LCDPlate.GPIO,Button);
end;

{==============================================================================}

function AF16x2LCDBacklightOn(Handle:THandle):Boolean;{$IFDEF API_EXPORT_AF16X2LCD} stdcall;{$ENDIF}
{Turn on the backlight on the AF16x2LCD display}
{Handle: The handle of the AF16x2LCD or INVALID_HANDLE_VALUE for the default display}
{Return: True if completed or False on failure}
var
 Level:LongWord;
 AF16x2LCDPlate:PAF16x2LCDPlate;
begin
 {}
 Result:=False;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Handle:=AF16x2LCDDefault;
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {Get Plate}
 AF16x2LCDPlate:=PAF16x2LCDPlate(Handle);
 if AF16x2LCDPlate = nil then Exit;
 if AF16x2LCDPlate.Signature <> AF16X2LCD_SIGNATURE then Exit;

 {Get Level}
 Level:=GPIO_LEVEL_HIGH;
 if AF16x2LCDPlate.Invert then Level:=GPIO_LEVEL_LOW;

 {Turn on Backlight (White)}
 GPIODeviceOutputSet(AF16x2LCDPlate.GPIO,AF16X2LCD_PLATE_RED,Level);
 GPIODeviceOutputSet(AF16x2LCDPlate.GPIO,AF16X2LCD_PLATE_GREEN,Level);
 GPIODeviceOutputSet(AF16x2LCDPlate.GPIO,AF16X2LCD_PLATE_BLUE,Level);

 Result:=True;
end;

{==============================================================================}

function AF16x2LCDBacklightOff(Handle:THandle):Boolean;{$IFDEF API_EXPORT_AF16X2LCD} stdcall;{$ENDIF}
{Turn off the backlight on the AF16x2LCD display}
{Handle: The handle of the AF16x2LCD or INVALID_HANDLE_VALUE for the default display}
{Return: True if completed or False on failure}
var
 Level:LongWord;
 AF16x2LCDPlate:PAF16x2LCDPlate;
begin
 {}
 Result:=False;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Handle:=AF16x2LCDDefault;
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {Get Plate}
 AF16x2LCDPlate:=PAF16x2LCDPlate(Handle);
 if AF16x2LCDPlate = nil then Exit;
 if AF16x2LCDPlate.Signature <> AF16X2LCD_SIGNATURE then Exit;

 {Get Level}
 Level:=GPIO_LEVEL_LOW;
 if AF16x2LCDPlate.Invert then Level:=GPIO_LEVEL_HIGH;

 {Turn off Backlight}
 GPIODeviceOutputSet(AF16x2LCDPlate.GPIO,AF16X2LCD_PLATE_RED,Level);
 GPIODeviceOutputSet(AF16x2LCDPlate.GPIO,AF16X2LCD_PLATE_GREEN,Level);
 GPIODeviceOutputSet(AF16x2LCDPlate.GPIO,AF16X2LCD_PLATE_BLUE,Level);

 Result:=True;
end;

{==============================================================================}

function AF16x2LCDBacklightColor(Handle:THandle;Red,Green,Blue:Byte):Boolean;{$IFDEF API_EXPORT_AF16X2LCD} stdcall;{$ENDIF}
{Set the backlight color on the AF16x2LCD display}
{Handle: The handle of the AF16x2LCD or INVALID_HANDLE_VALUE for the default display}
{Red: The Red value (0 for Off / 1 for On)}
{Green: The Green value (0 for Off / 1 for On)}
{Blue: The Blue value (0 for Off / 1 for On)}
{Return: True if completed or False on failure}
var
 LevelLow:LongWord;
 LevelHigh:LongWord;
 AF16x2LCDPlate:PAF16x2LCDPlate;
begin
 {}
 Result:=False;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Handle:=AF16x2LCDDefault;
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {Get Plate}
 AF16x2LCDPlate:=PAF16x2LCDPlate(Handle);
 if AF16x2LCDPlate = nil then Exit;
 if AF16x2LCDPlate.Signature <> AF16X2LCD_SIGNATURE then Exit;

 {Get Low (Off) Level}
 LevelLow:=GPIO_LEVEL_LOW;
 if AF16x2LCDPlate.Invert then LevelLow:=GPIO_LEVEL_HIGH;

 {Get High (On) Level}
 LevelHigh:=GPIO_LEVEL_HIGH;
 if AF16x2LCDPlate.Invert then LevelHigh:=GPIO_LEVEL_LOW;

 {Turn on/off Backlight (Red)}
 if Red = 0 then
  begin
   GPIODeviceOutputSet(AF16x2LCDPlate.GPIO,AF16X2LCD_PLATE_RED,LevelLow);
  end
 else
  begin
   GPIODeviceOutputSet(AF16x2LCDPlate.GPIO,AF16X2LCD_PLATE_RED,LevelHigh);
  end;

 {Turn on/off Backlight (Green)}
 if Green = 0 then
  begin
   GPIODeviceOutputSet(AF16x2LCDPlate.GPIO,AF16X2LCD_PLATE_GREEN,LevelLow);
  end
 else
  begin
   GPIODeviceOutputSet(AF16x2LCDPlate.GPIO,AF16X2LCD_PLATE_GREEN,LevelHigh);
  end;

 {Turn on/off Backlight (Blue)}
 if Blue = 0 then
  begin
   GPIODeviceOutputSet(AF16x2LCDPlate.GPIO,AF16X2LCD_PLATE_BLUE,LevelLow);
  end
 else
  begin
   GPIODeviceOutputSet(AF16x2LCDPlate.GPIO,AF16X2LCD_PLATE_BLUE,LevelHigh);
  end;
end;

{==============================================================================}
{==============================================================================}
{AF16x2LCD Helper Functions}

{==============================================================================}
{==============================================================================}

initialization
 AF16x2LCDInit;

{==============================================================================}

{finalization}
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
