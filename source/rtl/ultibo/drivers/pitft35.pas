{
Adafruit PiTFT 3.5" LCD Driver.

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

  Linux - \arch\arm\boot\dts\overlays\pitft35-resistive-overlay.dts

References
==========

 Adafruit PiTFT 3.5" LCD

  Raspberry Pi A/B

   https://www.adafruit.com/product/2097

  Raspberry Pi A+/B+/Zero/2B/3B

   https://www.adafruit.com/products/2441

  Schematic

   https://learn.adafruit.com/assets/26348

Adafruit PiTFT 3.5" LCD
=======================

 The Adafruit PiTFT 3.5" LCD is a 480 x 320 pixel TFT with resistive touchscreen using a Himax HX8357
 driver and a STMicroelectronics STMPE610 resistive touchscreen controller.

 This unit ties together the various components needed to make one of these boards work with Ultibo by finding
 the correct SPI device, creating the STMPE610 Touch device, creating the HX8357D Framebuffer device and registering
 all of it with the correct parameters for the Adafruit board.

 Details:

  HX8357D

   Width:   320
   Height:  480

   SPI Mode: 0
   SPI Frequency: 42000000
   SPI Chip Select: SPI_CS_0

   DC GPIO: GPIO_PIN_25 (Pull: GPIO_PULL_NONE)
   RST GPIO: GPIO_PIN_UNKNOWN

   Backlight GPIO: GPIO_PIN_2  (STMPE GPIO)
                   GPIO_PIN_18 (PWM)

  STMPE

   Chip: STMPE_CHIP_610

   SPI Mode: 0
   SPI Frequency: 500000
   SPI Chip Select: SPI_CS_1

   IRQ GPIO: GPIO_PIN_24 (Trigger: GPIO_TRIGGER_FALLING)(Pull: GPIO_PULL_UP)

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit PiTFT35;

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
  Touch,
  HX8357D,
  STMPE,
  SysUtils;

{==============================================================================}
{Global definitions}
{$INCLUDE ..\core\GlobalDefines.inc}

{==============================================================================}
const
 {PiTFT35 specific constants}
 PITFT35_FRAMEBUFFER_DESCRIPTION = 'Adafruit PiTFT 3.5" LCD';  {Description of PiTFT35 device}

 PITFT35_SIGNATURE = $AF000035;

 PITFT35_SCREEN_WIDTH  = 320;
 PITFT35_SCREEN_HEIGHT = 480;

 {PiTFT35 GPIO constants}
 PITFT35_LCD_DC     = GPIO_PIN_25;
 PITFT35_TOUCH_IRQ  = GPIO_PIN_24;

 PITFT35_LCD_BL     = GPIO_PIN_2; {STMPE GPIO}
 PITFT35_LCD_BL_PWM = GPIO_PIN_18;

{==============================================================================}
type
 {PiTFT35 specific types}
 PPiTFT35LCD = ^TPiTFT35LCD;
 TPiTFT35LCD = record
  Signature:LongWord;             {Signature for entry validation}
  Rotation:LongWord;              {Framebuffer rotation (eg FRAMEBUFFER_ROTATION_180)}
  SPI:PSPIDevice;                 {SPI device for this display}
  GPIO:PGPIODevice;               {GPIO device for this display}
  Touch:PTouchDevice;             {Touch (STMPE) device for this display}
  Backlight:PGPIODevice;          {Backlight GPIO (STMPE) device for this display}
  Framebuffer:PFramebufferDevice; {Framebuffer (HX8357D) device for this display}
 end;

{==============================================================================}
var
 {PiTFT35 specific variables}
 PITFT35_SPI_DEVICE:String = 'SPI0';
 PITFT35_LCD_CHIPSELECT:Word = SPI_CS_0;
 PITFT35_TOUCH_CHIPSELECT:Word = SPI_CS_1;
 PITFT35_BL_PWM_ENABLE:LongBool = True;

{==============================================================================}
{Initialization Functions}
procedure PiTFT35Init;

function PiTFT35Start(Rotation:LongWord;const Device:String;DisplaySelect,TouchSelect:Word):THandle;{$IFDEF API_EXPORT_PITFT35} stdcall; public name 'pitft35_start';{$ENDIF}
function PiTFT35Stop(Handle:THandle):Boolean;{$IFDEF API_EXPORT_PITFT35} stdcall; public name 'pitft35_stop';{$ENDIF}

{==============================================================================}
{PiTFT35 Functions}

{==============================================================================}
{PiTFT35 Framebuffer Functions}
//To Do //Backlight PWM control (PITFT35_BL_PWM_ENABLE) (PITFT35_LCD_BL_PWM = GPIO_PIN_18)

{==============================================================================}
{PiTFT35 Helper Functions}

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {PiTFT35 specific variables}
 PiTFT35Initialized:Boolean;

 PiTFT35Default:THandle = INVALID_HANDLE_VALUE;

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure PiTFT35Init;
{Initialize the PiTFT35 unit and parameters}

{Note: Called only during system startup}
var
 WorkInt:LongWord;
 WorkBuffer:String;
begin
 {}
 {Check Initialized}
 if PiTFT35Initialized then Exit;

 {Check Environment Variables}
 {PITFT35_AUTOSTART}
 WorkInt:=StrToIntDef(EnvironmentGet('PITFT35_AUTOSTART'),1);
 if WorkInt = 0 then PITFT35_AUTOSTART:=False;

 {PITFT35_SPI_DEVICE}
 WorkBuffer:=EnvironmentGet('PITFT35_SPI_DEVICE');
 if Length(WorkBuffer) <> 0 then PITFT35_SPI_DEVICE:=WorkBuffer;

 {PITFT35_LCD_CHIPSELECT}
 WorkInt:=StrToIntDef(EnvironmentGet('PITFT35_LCD_CHIPSELECT'),0);
 if WorkInt > 0 then PITFT35_LCD_CHIPSELECT:=WorkInt;

 {PITFT35_TOUCH_CHIPSELECT}
 WorkInt:=StrToIntDef(EnvironmentGet('PITFT35_TOUCH_CHIPSELECT'),0);
 if WorkInt > 0 then PITFT35_TOUCH_CHIPSELECT:=WorkInt;

 {Start PiTFT35}
 if PITFT35_AUTOSTART then
  begin
   PiTFT35Default:=PiTFT35Start(FRAMEBUFFER_ROTATION_0,PITFT35_SPI_DEVICE,PITFT35_LCD_CHIPSELECT,PITFT35_TOUCH_CHIPSELECT);
  end;

 PiTFT35Initialized:=True;
end;

{==============================================================================}

function PiTFT35Start(Rotation:LongWord;const Device:String;DisplaySelect,TouchSelect:Word):THandle;{$IFDEF API_EXPORT_PITFT35} stdcall;{$ENDIF}
{Start the PiTFT35 driver and register the Touch, Backlight (GPIO) and Framebuffer devices associated with the display}
{Rotation: The rotation of the display (eg FRAMEBUFFER_ROTATION_180)}
{Device: The SPI device that the HX8357D and STMPE610 devices are connected to}
{DisplaySelect: The SPI chip select of the HX8357D LCD controller}
{TouchSelect: The SPI chip select of the STMPE610 touch controller}
{Return: The handle of the PiTFT35 on success or INVALID_HANDLE_VALUE on failure}

{Note: This function will be called during startup if the parameter PITFT35_AUTOSTART is True
       Can be called multiple times to support more than one PiTFT LCD display}
var
 SPI:PSPIDevice;
 GPIO:PGPIODevice;
 Touch:PTouchDevice;
 Backlight:PGPIODevice;
 Framebuffer:PFramebufferDevice;

 BL:TGPIOInfo;
 DC:TGPIOInfo;
 RST:TGPIOInfo;
 IRQ:TGPIOInfo;

 PiTFT35LCD:PPiTFT35LCD;
begin
 {}
 Result:=INVALID_HANDLE_VALUE;

 {Check Rotation}
 if Rotation > FRAMEBUFFER_ROTATION_270 then Exit;

 {Check Device}
 if Length(Device) = 0 then Exit;

 {Check Display Chip Select}
 if DisplaySelect = SPI_CS_NONE then Exit;

 {Check Touch Chip Select}
 if TouchSelect = SPI_CS_NONE then Exit;

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

 {Setup Touch IRQ (Interrupt)}
 IRQ.GPIO:=GPIO;
 IRQ.Pin:=PITFT35_TOUCH_IRQ;
 IRQ.Func:=GPIO_FUNCTION_IN;
 IRQ.Pull:=GPIO_PULL_UP;
 IRQ.Trigger:=GPIO_TRIGGER_FALLING;

 {Create Backlight (GPIO) Device}
 Backlight:=STMPE610GPIOCreate(nil,SPI,I2C_ADDRESS_INVALID,TouchSelect,nil); {IRQ not available for GPIO}
 if Backlight = nil then Exit;
 try
  {Create Touch Device}
  Touch:=STMPE610TouchCreate(nil,SPI,I2C_ADDRESS_INVALID,TouchSelect,PITFT35_SCREEN_WIDTH,PITFT35_SCREEN_HEIGHT,@IRQ);
  if Touch = nil then Exit;
  try
   {Setup Display RST (Reset)}
   RST:=GPIO_INFO_UNKNOWN;

   {Setup Display DC (Data/Command)}
   DC.GPIO:=GPIO;
   DC.Pin:=PITFT35_LCD_DC;
   DC.Func:=GPIO_FUNCTION_OUT;
   DC.Pull:=GPIO_PULL_NONE;
   DC.Trigger:=GPIO_TRIGGER_UNKNOWN;

   {Setup Display BL (Backlight)}
   BL.GPIO:=Backlight;
   BL.Pin:=PITFT35_LCD_BL;
   BL.Func:=GPIO_FUNCTION_OUT;
   BL.Pull:=GPIO_PULL_NONE;
   BL.Trigger:=GPIO_TRIGGER_UNKNOWN;

   {Create Framebuffer Device}
   Framebuffer:=HX8357DFramebufferCreate(SPI,DisplaySelect,PITFT35_FRAMEBUFFER_DESCRIPTION,Rotation,PITFT35_SCREEN_WIDTH,PITFT35_SCREEN_HEIGHT,@RST,@DC,@BL);
   if Framebuffer = nil then Exit;
   try
    {Update Framebuffer}
    //To Do //Backlight PWM control

    {Create PiTFT35}
    PiTFT35LCD:=AllocMem(SizeOf(TPiTFT35LCD));
    if PiTFT35LCD = nil then Exit;

    {Update PiTFT35}
    PiTFT35LCD.Signature:=PITFT35_SIGNATURE;
    PiTFT35LCD.Rotation:=Rotation;
    PiTFT35LCD.SPI:=SPI;
    PiTFT35LCD.GPIO:=GPIO;
    PiTFT35LCD.Touch:=Touch;
    PiTFT35LCD.Backlight:=Backlight;
    PiTFT35LCD.Framebuffer:=Framebuffer;

    {Return Result}
    Result:=THandle(PiTFT35LCD);

    {Check Default}
    if PiTFT35Default = INVALID_HANDLE_VALUE then
     begin
      PiTFT35Default:=Result;
     end;
   finally
    if Result = INVALID_HANDLE_VALUE then HX8357DFramebufferDestroy(Framebuffer);
   end;
  finally
   if Result = INVALID_HANDLE_VALUE then STMPETouchDestroy(Touch);
  end;
 finally
  if Result = INVALID_HANDLE_VALUE then STMPEGPIODestroy(Backlight);
 end;
end;

{==============================================================================}

function PiTFT35Stop(Handle:THandle):Boolean;{$IFDEF API_EXPORT_PITFT35} stdcall;{$ENDIF}
{Stop the PiTFT35 driver and deregister the Touch, Backlight (GPIO) and Framebuffer devices associated with the display}
{Handle: The handle of the PiTFT35 or INVALID_HANDLE_VALUE for the default display}
{Return: True if completed or False on failure}
var
 PiTFT35LCD:PPiTFT35LCD;
begin
 {}
 Result:=False;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Handle:=PiTFT35Default;
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {Get LCD}
 PiTFT35LCD:=PPiTFT35LCD(Handle);
 if PiTFT35LCD = nil then Exit;
 if PiTFT35LCD.Signature <> PITFT35_SIGNATURE then Exit;

 {Check Framebuffer Device}
 if PiTFT35LCD.Framebuffer <> nil then
  begin
   {Destroy Framebuffer Device}
   if HX8357DFramebufferDestroy(PiTFT35LCD.Framebuffer) = ERROR_SUCCESS then
    begin
     {Update PiTFT35}
     PiTFT35LCD.Framebuffer:=nil;

     {Check Touch Device}
     if PiTFT35LCD.Touch <> nil then
      begin
       {Destroy Touch Device}
       if STMPETouchDestroy(PiTFT35LCD.Touch) = ERROR_SUCCESS then
        begin
         {Update PiTFT35}
         PiTFT35LCD.Touch:=nil;

         {Check Backlight Device}
         if PiTFT35LCD.Backlight <> nil then
          begin
           {Destroy Backlight Device}
           if STMPEGPIODestroy(PiTFT35LCD.Backlight) = ERROR_SUCCESS then
            begin
             {Update PiTFT35}
             PiTFT35LCD.Backlight:=nil;

             {Check Default}
             if PiTFT35Default = THandle(PiTFT35LCD) then
              begin
               PiTFT35Default:=INVALID_HANDLE_VALUE;
              end;

             {Invalidate PiTFT35}
             PiTFT35LCD.Signature:=0;

             {Destroy PiTFT35}
             FreeMem(PiTFT35LCD);

             {Return Result}
             Result:=True;
            end;
          end;
        end;
      end;
    end;
  end;
end;

{==============================================================================}
{==============================================================================}
{PiTFT35 Functions}

{==============================================================================}
{==============================================================================}
{PiTFT35 Helper Functions}

{==============================================================================}
{==============================================================================}

initialization
 PiTFT35Init;

{==============================================================================}

{finalization}
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
