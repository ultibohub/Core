{
Adafruit 1.9" 320x170 Color IPS TFT Display LCD Driver.

Copyright (C) 2025 - EL Mahiri Nabil.

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

  Linux - \arch\arm\boot\dts\overlays\fbtft-overlay.dts

References
==========

 Adafruit 1.9" 320x170 Color IPS TFT

  Raspberry Pi A+/B+/Zero/Zero2/2B/3B/4B

   https://www.adafruit.com/product/5394

  Schematic

   https://learn.adafruit.com/assets/110257

Adafruit 1.9" 320x170 Color IPS TFT
===================================

 Adafruit 1.9" 320x170 Color IPS TFT display has 320x170 16-bit full-color pixels and is an IPS display,
 so the color looks great up to 80 degrees off-axis in any direction.

 The TFT driver (Sitronix ST7789) is very similar to the popular ST7735.

 Details:

  ST7789

   Width:   170
   Height:  320

   SPI Mode: 0
   SPI Frequency: 42000000
   SPI Chip Select: SPI_CS_0

   DC GPIO: GPIO_PIN_25 (Pull: GPIO_PULL_NONE)
   RST GPIO: GPIO_PIN_27 (Pull: GPIO_PULL_NONE)

   Backlight GPIO: GPIO_PIN_18 (GPIO / PWM)


}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit IPSTFT19;

interface

uses
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  Devices,
  GPIO,
  PWM,
  SPI,
  Framebuffer,
  ST77XX,
  SysUtils;

{==============================================================================}
{Global definitions}
{$INCLUDE ..\core\GlobalDefines.inc}

{==============================================================================}
const
 {IPSTFT19 specific constants}
 IPSTFT19_FRAMEBUFFER_DESCRIPTION = 'Adafruit 1.9" IPS TFT';  {Description of IPSTFT19 device}

 IPSTFT19_SIGNATURE = $AF000019;

 IPSTFT19_SCREEN_WIDTH  = 170;
 IPSTFT19_SCREEN_HEIGHT = 320;
 IPSTFT19_COLSTART = 35;
 IPSTFT19_DEFAULT_ROTATION = FRAMEBUFFER_ROTATION_90;

 {IPSTFT19 GPIO constants}
 IPSTFT19_LCD_DC    = GPIO_PIN_25;
 IPSTFT19_LCD_RST   = GPIO_PIN_27; //Add this one
 IPSTFT19_LCD_BL    = GPIO_PIN_18;

{==============================================================================}
type
 {IPSTFT19 specific types}
 PIPSTFT19LCD = ^TIPSTFT19LCD;
 TIPSTFT19LCD = record
  Signature:LongWord;             {Signature for entry validation}
  Rotation:LongWord;              {Framebuffer rotation (eg FRAMEBUFFER_ROTATION_180)}
  SPI:PSPIDevice;                 {SPI device for this display}
  GPIO:PGPIODevice;               {GPIO device for this display}
  Framebuffer:PFramebufferDevice; {Framebuffer (ST7789) device for this display}
 end;

{==============================================================================}
var
 {IPSTFT19 specific variables}
 IPSTFT19_SPI_DEVICE:String = 'SPI0';
 IPSTFT19_LCD_CHIPSELECT:Word = SPI_CS_0;
 IPSTFT19_BL_PWM_ENABLE:LongBool = True;

{==============================================================================}
{Initialization Functions}
procedure IPSTFT19Init;

function IPSTFT19Start(Rotation:LongWord;const Device:String;DisplaySelect:word):THandle;{$IFDEF API_EXPORT_IPSTFT19} stdcall; public name 'ipstft19_start';{$ENDIF}
function IPSTFT19Stop(Handle:THandle):Boolean;{$IFDEF API_EXPORT_IPSTFT19} stdcall; public name 'ipstft19_stop';{$ENDIF}

{==============================================================================}
{IPSTFT19 Functions}

{==============================================================================}
{IPSTFT19 Framebuffer Functions}
//To Do //Backlight PWM control (IPSTFT19_BL_PWM_ENABLE) (IPSTFT19_LCD_BL_PWM = GPIO_PIN_18)

{==============================================================================}
{IPSTFT19 Helper Functions}

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {IPSTFT19 specific variables}
 IPSTFT19Initialized:Boolean;

 IPSTFT19Default:THandle = INVALID_HANDLE_VALUE;

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure IPSTFT19Init;
{Initialize the IPSTFT19 unit and parameters}

{Note: Called only during system startup}
var
 WorkInt:LongWord;
 WorkBuffer:String;
begin
 {}
 {Check Initialized}
 if IPSTFT19Initialized then Exit;

 {Check Environment Variables}
 {IPSTFT19_AUTOSTART}
 WorkInt:=StrToIntDef(EnvironmentGet('IPSTFT19_AUTOSTART'),1);
 if WorkInt = 0 then IPSTFT19_AUTOSTART:=False;

 {IPSTFT19_SPI_DEVICE}
 WorkBuffer:=EnvironmentGet('IPSTFT19_SPI_DEVICE');
 if Length(WorkBuffer) <> 0 then IPSTFT19_SPI_DEVICE:=WorkBuffer;

 {IPSTFT19_LCD_CHIPSELECT}
 WorkInt:=StrToIntDef(EnvironmentGet('IPSTFT19_LCD_CHIPSELECT'),0);
 if WorkInt > 0 then IPSTFT19_LCD_CHIPSELECT:=WorkInt;

 {Start IPSTFT19}
 if IPSTFT19_AUTOSTART then
  begin
   IPSTFT19Default:=IPSTFT19Start(IPSTFT19_DEFAULT_ROTATION,IPSTFT19_SPI_DEVICE,IPSTFT19_LCD_CHIPSELECT);
  end;

 IPSTFT19Initialized:=True;
end;

{==============================================================================}

function IPSTFT19Start(Rotation:LongWord;const Device:String;DisplaySelect:word):THandle;{$IFDEF API_EXPORT_IPSTFT19} stdcall;{$ENDIF}
{Start the IPSTFT19 driver and Framebuffer device associated with the display}
{Rotation: The rotation of the display (eg FRAMEBUFFER_ROTATION_180)}
{Device: The SPI device that the ST7789 device is connected to}
{DisplaySelect: The SPI chip select of the ST7789 LCD controller}
{Return: The handle of the IPSTFT19 on success or INVALID_HANDLE_VALUE on failure}

{Note: This function will be called during startup if the parameter IPSTFT19_AUTOSTART is True
       Can be called multiple times to support more than one IPSTFT LCD display}
var
 SPI:PSPIDevice;
 GPIO:PGPIODevice;
 Framebuffer:PFramebufferDevice;

 BL:TGPIOInfo;
 DC:TGPIOInfo;
 RST:TGPIOInfo;

 IPSTFT19LCD:PIPSTFT19LCD;
begin
 {}
 Result:=INVALID_HANDLE_VALUE;

 {Check Rotation}
 if Rotation > FRAMEBUFFER_ROTATION_270 then Exit;

 {Check Device}
 if Length(Device) = 0 then Exit;

 {Check Display Chip Select}
 if DisplaySelect = SPI_CS_NONE then Exit;


 {Check SPI Device}
 SPI:=SPIDeviceFindByName(Device);
 if SPI = nil then
  begin
   SPI:=SPIDeviceFindByDescription(Device);
   if SPI = nil then Exit;
  end;

 {Check GPIO Device}
 GPIO:=GPIODeviceGetDefault;
 if GPIO = nil then Exit;

 {Setup Display RST (Reset)}
 RST.GPIO:=GPIO;
 RST.Pin:=IPSTFT19_LCD_RST;
 RST.Func:=GPIO_FUNCTION_OUT;
 RST.Pull:=GPIO_PULL_NONE;
 RST.Trigger:=GPIO_TRIGGER_UNKNOWN;

 {Setup Display DC (Data/Command)}
 DC.GPIO:=GPIO;
 DC.Pin:=IPSTFT19_LCD_DC;
 DC.Func:=GPIO_FUNCTION_OUT;
 DC.Pull:=GPIO_PULL_NONE;
 DC.Trigger:=GPIO_TRIGGER_UNKNOWN;

 {Setup Display BL (Backlight)}
 BL.GPIO:=GPIO;
 BL.Pin:=IPSTFT19_LCD_BL;
 BL.Func:=GPIO_FUNCTION_OUT;
 BL.Pull:=GPIO_PULL_NONE;
 BL.Trigger:=GPIO_TRIGGER_UNKNOWN;

 {Create Framebuffer Device}
 Framebuffer:=ST77XXFramebufferCreate(SPI,DisplaySelect,IPSTFT19_FRAMEBUFFER_DESCRIPTION,Rotation,IPSTFT19_SCREEN_WIDTH,IPSTFT19_SCREEN_HEIGHT,IPSTFT19_COLSTART,@RST,@DC,@BL);
 if Framebuffer = nil then Exit;
 try
  {Update Framebuffer}
  //To Do //Backlight PWM control

  {Create IPSTFT19}
  IPSTFT19LCD:=AllocMem(SizeOf(TIPSTFT19LCD));
  if IPSTFT19LCD = nil then Exit;

  {Update IPSTFT19}
  IPSTFT19LCD.Signature:=IPSTFT19_SIGNATURE;
  IPSTFT19LCD.Rotation:=Rotation;
  IPSTFT19LCD.SPI:=SPI;
  IPSTFT19LCD.GPIO:=GPIO;
  IPSTFT19LCD.Framebuffer:=Framebuffer;

  {Return Result}
  Result:=THandle(IPSTFT19LCD);

  {Check Default}
  if IPSTFT19Default = INVALID_HANDLE_VALUE then
   begin
    IPSTFT19Default:=Result;
   end;
 finally
  if Result = INVALID_HANDLE_VALUE then ST77XXFramebufferDestroy(Framebuffer);
 end;
end;

{==============================================================================}

function IPSTFT19Stop(Handle:THandle):Boolean;{$IFDEF API_EXPORT_IPSTFT19} stdcall;{$ENDIF}
{Stop the IPSTFT19 driver and Framebuffer device associated with the display}
{Handle: The handle of the IPSTFT19 or INVALID_HANDLE_VALUE for the default display}
{Return: True if completed or False on failure}
var
 IPSTFT19LCD:PIPSTFT19LCD;
begin
 {}
 Result:=False;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Handle:=IPSTFT19Default;
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {Get LCD}
 IPSTFT19LCD:=PIPSTFT19LCD(Handle);
 if IPSTFT19LCD = nil then Exit;
 if IPSTFT19LCD.Signature <> IPSTFT19_SIGNATURE then Exit;

 {Check Framebuffer Device}
 if IPSTFT19LCD.Framebuffer <> nil then
  begin
   {Destroy Framebuffer Device}
   if ST77XXFramebufferDestroy(IPSTFT19LCD.Framebuffer) = ERROR_SUCCESS then
    begin
     {Update IPSTFT19}
     IPSTFT19LCD.Framebuffer:=nil;

     {Check Default}
     if IPSTFT19Default = THandle(IPSTFT19LCD) then
      begin
       IPSTFT19Default:=INVALID_HANDLE_VALUE;
      end;

     {Invalidate IPSTFT19}
     IPSTFT19LCD.Signature:=0;

     {Destroy IPSTFT19}
     FreeMem(IPSTFT19LCD);

     {Return Result}
     Result:=True;
    end;
  end;
end;

{==============================================================================}
{==============================================================================}
{IPSTFT19 Functions}

{==============================================================================}
{==============================================================================}
{IPSTFT19 Helper Functions}

{==============================================================================}
{==============================================================================}

initialization
 IPSTFT19Init;

{==============================================================================}

{finalization}
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
