{
Adafruit PiTFT 2.8" LCD Driver.

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

  Linux - \arch\arm\boot\dts\overlays\pitft28-resistive-overlay.dts
  
References
==========

 Adafruit PiTFT 2.8" LCD

  Raspberry Pi A/B
  
   PiTFT - 320x240 2.8" TFT + Resistive Touchscreen (https://www.adafruit.com/product/1601)
   
  Raspberry Pi A+/B+/Zero/2B/3B/4B
  
   PiTFT Plus 320x240 2.8" TFT + Resistive Touchscreen (https://www.adafruit.com/products/2298)
 
  Schematic
 
   https://learn.adafruit.com/assets/25554
   
 Adafruit PiTFT 3.2" LCD
   
  Raspberry Pi A+/B+/Zero/2B/3B/4B
  
   PiTFT Plus 320x240 3.2" TFT + Resistive Touchscreen (https://www.adafruit.com/product/2616)

 Adafruit PiTFT 2.8" LCD (Capacitive)

  Raspberry Pi A+/B+/Zero/2B/3B/4B

   PiTFT Plus 320x240 2.8" Capacitive Touchscreen (https://www.adafruit.com/product/2423)

  Schematic

   https://learn.adafruit.com/assets/18250

Adafruit PiTFT 2.8" LCD
=======================

 The Adafruit PiTFT 2.8" and PiTFT 3.2" LCD is a 320 x 240 pixel TFT with resistive touchscreen using
 an ILITEK ILI9340 driver and a STMicroelectronics STMPE610 resistive touchscreen controller. There is
 also a version available that includes a capacitive touchscreen using the Focaltech FT6236 controller.
 
 This unit ties together the various components needed to make one of these boards work with Ultibo by
 finding the correct SPI or I2C device, creating the STMPE610 or FT6236 Touch device, creating the ILI9340
 Framebuffer device and registering all of it with the correct parameters for the Adafruit board.

 Details:
 
  ILI9340 
  
   Width:   240
   Height:  320
   
   SPI Mode: 0
   SPI Frequency: 32000000
   SPI Chip Select: SPI_CS_0
   
   DC GPIO: GPIO_PIN_25 (Pull: GPIO_PULL_NONE)
   RST GPIO: GPIO_PIN_UNKNOWN

   Backlight GPIO: GPIO_PIN_2 (STMPE GPIO)
   
  STMPE (Resistive touch)
  
   Chip: STMPE_CHIP_610
   
   SPI Mode: 0
   SPI Frequency: 500000
   SPI Chip Select: SPI_CS_1
   
   IRQ GPIO: GPIO_PIN_24 (Trigger: GPIO_TRIGGER_FALLING)(Pull: GPIO_PULL_UP)
   
  FT6236 (Capacitive touch)
  
   I2C Address: 0x38 

   IRQ GPIO: GPIO_PIN_24 (Trigger: GPIO_TRIGGER_FALLING)(Pull: GPIO_PULL_UP)
  
  Switches
  
   SW1 GPIO: GPIO_PIN_22
   
   SW2 GPIO: GPIO_PIN_27
   
   SW3 GPIO: GPIO_PIN_17
   
   SW4 GPIO: GPIO_PIN_23

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit PiTFT28; 

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,GPIO,SPI,I2C,Framebuffer,Touch,ILI9340,STMPE,FT5x06Touch,SysUtils;

{==============================================================================}
{Global definitions}
{$INCLUDE ..\core\GlobalDefines.inc}

{==============================================================================}
const
 {PiTFT28 specific constants}
 PITFT28_FRAMEBUFFER_DESCRIPTION = 'Adafruit PiTFT 2.8" LCD';  {Description of PiTFT28 device}

 PITFT28_SIGNATURE = $AF000028;
 
 PITFT28_SCREEN_WIDTH  = 240;
 PITFT28_SCREEN_HEIGHT = 320;
 
 {PiTFT28 GPIO constants}
 PITFT28_LCD_DC    = GPIO_PIN_25;
 PITFT28_TOUCH_IRQ = GPIO_PIN_24;
 
 PITFT28_LCD_BL    = GPIO_PIN_2; {STMPE GPIO}
 
{==============================================================================}
type
 {PiTFT28 specific types}
 PPiTFT28LCD = ^TPiTFT28LCD;
 TPiTFT28LCD = record
  Signature:LongWord;             {Signature for entry validation}
  Rotation:LongWord;              {Framebuffer rotation (eg FRAMEBUFFER_ROTATION_180)}
  SPI:PSPIDevice;                 {SPI device for this display}
  I2C:PI2CDevice;                 {I2C device for this display}
  GPIO:PGPIODevice;               {GPIO device for this display}
  Touch:PTouchDevice;             {Touch (STMPE or FT6236) device for this display}
  Backlight:PGPIODevice;          {Backlight GPIO (STMPE) device for this display}
  Framebuffer:PFramebufferDevice; {Framebuffer (ILI9340) device for this display}
 end;
 
{==============================================================================}
var
 {PiTFT28 specific variables}
 PITFT28_SPI_DEVICE:String = 'SPI0';
 PITFT28_LCD_CHIPSELECT:Word = SPI_CS_0;
 PITFT28_TOUCH_CHIPSELECT:Word = SPI_CS_1;
 
 PITFT28_I2C_DEVICE:String = 'I2C0';
 PITFT28_TOUCH_ADDRESS:Word = $38;
 
{==============================================================================}
{Initialization Functions}
procedure PiTFT28Init;

function PiTFT28Start(Rotation:LongWord;const Device:String;DisplaySelect,TouchSelect:Word):THandle;{$IFDEF API_EXPORT_PITFT28} stdcall; public name 'pitft28_start';{$ENDIF}
function PiTFT28ResistiveStart(Rotation:LongWord;const Device:String;DisplaySelect,TouchSelect:Word):THandle;{$IFDEF API_EXPORT_PITFT28} stdcall; public name 'pitft28_resistive_start';{$ENDIF}
function PiTFT28CapacitiveStart(Rotation:LongWord;const SPIDevice,I2CDevice:String;DisplaySelect,TouchAddress:Word):THandle;{$IFDEF API_EXPORT_PITFT28} stdcall; public name 'pitft28_capacitive_start';{$ENDIF}
function PiTFT28Stop(Handle:THandle):Boolean;{$IFDEF API_EXPORT_PITFT28} stdcall; public name 'pitft28_stop';{$ENDIF}

{==============================================================================}
{PiTFT28 Functions}

{==============================================================================}
{PiTFT28 Helper Functions}
 
{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {PiTFT28 specific variables}
 PiTFT28Initialized:Boolean;
 
 PiTFT28Default:THandle = INVALID_HANDLE_VALUE;
 
{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure PiTFT28Init;
{Initialize the PiTFT28 unit and parameters}

{Note: Called only during system startup}
var
 WorkInt:LongWord;
 WorkBool:LongBool;
 WorkBuffer:String;
begin
 {}
 {Check Initialized}
 if PiTFT28Initialized then Exit;
 
 {Check Environment Variables}
 {PITFT28_AUTOSTART}
 WorkBool:=StrToBoolDef(EnvironmentGet('PITFT28_AUTOSTART'),PITFT28_AUTOSTART);
 if WorkBool <> PITFT28_AUTOSTART then PITFT28_AUTOSTART:=WorkBool;
 
 {PITFT28_CAPACITIVE_TOUCH}
 WorkBool:=StrToBoolDef(EnvironmentGet('PITFT28_CAPACITIVE_TOUCH'),PITFT28_CAPACITIVE_TOUCH);
 if WorkBool <> PITFT28_CAPACITIVE_TOUCH then PITFT28_CAPACITIVE_TOUCH:=WorkBool;
 
 {PITFT28_SPI_DEVICE}
 WorkBuffer:=EnvironmentGet('PITFT28_SPI_DEVICE');
 if Length(WorkBuffer) <> 0 then PITFT28_SPI_DEVICE:=WorkBuffer;

 {PITFT28_I2C_DEVICE}
 WorkBuffer:=EnvironmentGet('PITFT28_I2C_DEVICE');
 if Length(WorkBuffer) <> 0 then PITFT28_I2C_DEVICE:=WorkBuffer;
 
 {PITFT28_LCD_CHIPSELECT}
 WorkInt:=StrToIntDef(EnvironmentGet('PITFT28_LCD_CHIPSELECT'),PITFT28_LCD_CHIPSELECT);
 if WorkInt <> PITFT28_LCD_CHIPSELECT then PITFT28_LCD_CHIPSELECT:=WorkInt;

 {PITFT28_TOUCH_CHIPSELECT}
 WorkInt:=StrToIntDef(EnvironmentGet('PITFT28_TOUCH_CHIPSELECT'),PITFT28_TOUCH_CHIPSELECT);
 if WorkInt <> PITFT28_TOUCH_CHIPSELECT then PITFT28_TOUCH_CHIPSELECT:=WorkInt;

 {PITFT28_TOUCH_ADDRESS}
 WorkInt:=StrToIntDef(EnvironmentGet('PITFT28_TOUCH_ADDRESS'),PITFT28_TOUCH_ADDRESS);
 if WorkInt <> PITFT28_TOUCH_ADDRESS then PITFT28_TOUCH_ADDRESS:=WorkInt;

 {Start PiTFT28} 
 if PITFT28_AUTOSTART then
  begin
   if PITFT28_CAPACITIVE_TOUCH then
    begin
     PiTFT28Default:=PiTFT28CapacitiveStart(FRAMEBUFFER_ROTATION_0,PITFT28_SPI_DEVICE,PITFT28_I2C_DEVICE,PITFT28_LCD_CHIPSELECT,PITFT28_TOUCH_ADDRESS);
    end
   else
    begin
     PiTFT28Default:=PiTFT28ResistiveStart(FRAMEBUFFER_ROTATION_0,PITFT28_SPI_DEVICE,PITFT28_LCD_CHIPSELECT,PITFT28_TOUCH_CHIPSELECT);
    end; 
  end;
 
 PiTFT28Initialized:=True;
end;

{==============================================================================}

function PiTFT28Start(Rotation:LongWord;const Device:String;DisplaySelect,TouchSelect:Word):THandle;{$IFDEF API_EXPORT_PITFT28} stdcall;{$ENDIF}
{Start the PiTFT28 driver and register the Touch, Backlight (GPIO) and Framebuffer devices associated with the display}
{Rotation: The rotation of the display (eg FRAMEBUFFER_ROTATION_180)}
{Device: The SPI device that the ILI9340 and STMPE610 devices are connected to}
{DisplaySelect: The SPI chip select of the ILI9340 LCD controller}
{TouchSelect: The SPI chip select of the STMPE610 touch controller}
{Return: The handle of the PiTFT28 on success or INVALID_HANDLE_VALUE on failure}

{Note: This function will be called during startup if the parameter PITFT28_AUTOSTART is True
       Can be called multiple times to support more than one PiTFT LCD display}

{Note: Replaced by PiTFT28ResistiveStart to accommodate capacitive display option}
begin
 {}
 Result:=PiTFT28ResistiveStart(Rotation,Device,DisplaySelect,TouchSelect);
end;

{==============================================================================}

function PiTFT28ResistiveStart(Rotation:LongWord;const Device:String;DisplaySelect,TouchSelect:Word):THandle;{$IFDEF API_EXPORT_PITFT28} stdcall;{$ENDIF}
{Start the PiTFT28 driver and register the resistive Touch, Backlight (GPIO) and Framebuffer devices associated with the display}
{Rotation: The rotation of the display (eg FRAMEBUFFER_ROTATION_180)}
{Device: The SPI device that the ILI9340 and STMPE610 devices are connected to}
{DisplaySelect: The SPI chip select of the ILI9340 LCD controller}
{TouchSelect: The SPI chip select of the STMPE610 touch controller}
{Return: The handle of the PiTFT28 on success or INVALID_HANDLE_VALUE on failure}

{Note: This function will be called during startup if the parameter PITFT28_AUTOSTART is True
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
 
 PiTFT28LCD:PPiTFT28LCD;
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
 IRQ.Pin:=PITFT28_TOUCH_IRQ;
 IRQ.Func:=GPIO_FUNCTION_IN;
 IRQ.Pull:=GPIO_PULL_UP;
 IRQ.Trigger:=GPIO_TRIGGER_FALLING;
 
 {Create Backlight (GPIO) Device}
 Backlight:=STMPE610GPIOCreate(nil,SPI,I2C_ADDRESS_INVALID,TouchSelect,nil); {IRQ not available for GPIO}
 if Backlight = nil then Exit;
 try
  {Create Touch Device}
  Touch:=STMPE610TouchCreate(nil,SPI,I2C_ADDRESS_INVALID,TouchSelect,PITFT28_SCREEN_WIDTH,PITFT28_SCREEN_HEIGHT,@IRQ);
  if Touch = nil then Exit;
  try
   {Setup Display RST (Reset)}
   RST:=GPIO_INFO_UNKNOWN;
   
   {Setup Display DC (Data/Command)}
   DC.GPIO:=GPIO;
   DC.Pin:=PITFT28_LCD_DC;
   DC.Func:=GPIO_FUNCTION_OUT;
   DC.Pull:=GPIO_PULL_NONE;
   DC.Trigger:=GPIO_TRIGGER_UNKNOWN;
   
   {Setup Display BL (Backlight)}
   BL.GPIO:=Backlight;
   BL.Pin:=PITFT28_LCD_BL;
   BL.Func:=GPIO_FUNCTION_OUT;
   BL.Pull:=GPIO_PULL_NONE;
   BL.Trigger:=GPIO_TRIGGER_UNKNOWN;
   
   {Create Framebuffer Device}
   Framebuffer:=ILI9340FramebufferCreate(SPI,DisplaySelect,PITFT28_FRAMEBUFFER_DESCRIPTION,Rotation,PITFT28_SCREEN_WIDTH,PITFT28_SCREEN_HEIGHT,@RST,@DC,@BL);
   if Framebuffer = nil then Exit;
   try
    {Create PiTFT28}
    PiTFT28LCD:=AllocMem(SizeOf(TPiTFT28LCD));
    if PiTFT28LCD = nil then Exit;
    
    {Update PiTFT28}
    PiTFT28LCD.Signature:=PITFT28_SIGNATURE;
    PiTFT28LCD.Rotation:=Rotation;
    PiTFT28LCD.SPI:=SPI;
    PiTFT28LCD.GPIO:=GPIO;
    PiTFT28LCD.Touch:=Touch;
    PiTFT28LCD.Backlight:=Backlight;
    PiTFT28LCD.Framebuffer:=Framebuffer;
    
    {Return Result}
    Result:=THandle(PiTFT28LCD);
    
    {Check Default}
    if PiTFT28Default = INVALID_HANDLE_VALUE then
     begin
      PiTFT28Default:=Result;
     end;
   finally
    if Result = INVALID_HANDLE_VALUE then ILI9340FramebufferDestroy(Framebuffer);
   end;
  finally
   if Result = INVALID_HANDLE_VALUE then STMPETouchDestroy(Touch);
  end;
 finally
  if Result = INVALID_HANDLE_VALUE then STMPEGPIODestroy(Backlight);
 end;
end;

{==============================================================================}

function PiTFT28CapacitiveStart(Rotation:LongWord;const SPIDevice,I2CDevice:String;DisplaySelect,TouchAddress:Word):THandle;{$IFDEF API_EXPORT_PITFT28} stdcall;{$ENDIF}
{Start the PiTFT28 driver and register the capacitive Touch and Framebuffer devices associated with the display}
{Rotation: The rotation of the display (eg FRAMEBUFFER_ROTATION_180)}
{SPIDevice: The SPI device that the ILI9340 device is connected to}
{I2CDevice: The I2C device that the FT6236 device is connected to}
{DisplaySelect: The SPI chip select of the ILI9340 LCD controller}
{TouchAddress: The I2C address of the FT6236 touch controller}
{Return: The handle of the PiTFT28 on success or INVALID_HANDLE_VALUE on failure}

{Note: This function will be called during startup if the parameter PITFT28_AUTOSTART is True
       Can be called multiple times to support more than one PiTFT LCD display}
var
 SPI:PSPIDevice;
 I2C:PI2CDevice;
 GPIO:PGPIODevice;
 Touch:PTouchDevice;
 Framebuffer:PFramebufferDevice;

 BL:TGPIOInfo;
 DC:TGPIOInfo;
 RST:TGPIOInfo;
 IRQ:TGPIOInfo;

 PiTFT28LCD:PPiTFT28LCD;
begin
 {}
 Result:=INVALID_HANDLE_VALUE;

 {Check Rotation}
 if Rotation > FRAMEBUFFER_ROTATION_270 then Exit;

 {Check SPI Device}
 if Length(SPIDevice) = 0 then Exit;

 {Check I2C Device}
 if Length(I2CDevice) = 0 then Exit;

 {Check Display Chip Select}
 if DisplaySelect = SPI_CS_NONE then Exit;

 {Check Touch Chip Address}
 if TouchAddress = I2C_ADDRESS_INVALID then Exit;

 {Check SPI Device}
 SPI:=SPIDeviceFindByName(SPIDevice);
 if SPI = nil then
  begin
   SPI:=SPIDeviceFindByDescription(SPIDevice);
   if SPI = nil then Exit;
  end;

 {Check I2C Device}
 I2C:=I2CDeviceFindByName(I2CDevice);
 if I2C = nil then
  begin
   I2C:=I2CDeviceFindByDescription(I2CDevice);
   if I2C = nil then Exit;
  end;

 {Check GPIO Device}
 GPIO:=GPIODeviceGetDefault;
 if GPIO = nil then Exit;

 {Setup Touch IRQ (Interrupt)}
 IRQ.GPIO:=GPIO;
 IRQ.Pin:=PITFT28_TOUCH_IRQ;
 IRQ.Func:=GPIO_FUNCTION_IN;
 IRQ.Pull:=GPIO_PULL_UP;
 IRQ.Trigger:=GPIO_TRIGGER_FALLING;

 {Setup Touch Device Parameters}
 FT5X06_MAX_TOUCH_POINTS:=2;

 {Create Touch Device}
 Touch:=FT5X06TouchCreate(I2C,TouchAddress,PITFT28_SCREEN_WIDTH,PITFT28_SCREEN_HEIGHT,@IRQ,nil);
 if Touch = nil then Exit;
 try
  {Setup Display RST (Reset)}
  RST:=GPIO_INFO_UNKNOWN;

  {Setup Display DC (Data/Command)}
  DC.GPIO:=GPIO;
  DC.Pin:=PITFT28_LCD_DC;
  DC.Func:=GPIO_FUNCTION_OUT;
  DC.Pull:=GPIO_PULL_NONE;
  DC.Trigger:=GPIO_TRIGGER_UNKNOWN;

  {Setup Display BL (Backlight)}
  BL:=GPIO_INFO_UNKNOWN;

  {Create Framebuffer Device}
  Framebuffer:=ILI9340FramebufferCreate(SPI,DisplaySelect,PITFT28_FRAMEBUFFER_DESCRIPTION,Rotation,PITFT28_SCREEN_WIDTH,PITFT28_SCREEN_HEIGHT,@RST,@DC,@BL);
  if Framebuffer = nil then Exit;
  try
   {Create PiTFT28}
   PiTFT28LCD:=AllocMem(SizeOf(TPiTFT28LCD));
   if PiTFT28LCD = nil then Exit;

   {Update PiTFT28}
   PiTFT28LCD.Signature:=PITFT28_SIGNATURE;
   PiTFT28LCD.Rotation:=Rotation;
   PiTFT28LCD.SPI:=SPI;
   PiTFT28LCD.GPIO:=GPIO;
   PiTFT28LCD.Touch:=Touch;
   PiTFT28LCD.Backlight:=nil;
   PiTFT28LCD.Framebuffer:=Framebuffer;
    
   {Return Result}
   Result:=THandle(PiTFT28LCD);

   {Check Default}
   if PiTFT28Default = INVALID_HANDLE_VALUE then
    begin
     PiTFT28Default:=Result;
    end;
  finally
   if Result = INVALID_HANDLE_VALUE then ILI9340FramebufferDestroy(Framebuffer);
  end;
 finally
  if Result = INVALID_HANDLE_VALUE then FT5X06TouchDestroy(Touch);
 end;
end;

{==============================================================================}

function PiTFT28Stop(Handle:THandle):Boolean;{$IFDEF API_EXPORT_PITFT28} stdcall;{$ENDIF}
{Stop the PiTFT28 driver and deregister the Touch, Backlight (GPIO) and Framebuffer devices associated with the display}
{Handle: The handle of the PiTFT28 or INVALID_HANDLE_VALUE for the default display}
{Return: True if completed or False on failure}
var
 PiTFT28LCD:PPiTFT28LCD;
begin
 {}
 Result:=False;
 
 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Handle:=PiTFT28Default;
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {Get LCD}
 PiTFT28LCD:=PPiTFT28LCD(Handle);
 if PiTFT28LCD = nil then Exit;
 if PiTFT28LCD.Signature <> PITFT28_SIGNATURE then Exit;
 
 {Check Framebuffer Device}
 if PiTFT28LCD.Framebuffer <> nil then
  begin
   {Destroy Framebuffer Device}
   if ILI9340FramebufferDestroy(PiTFT28LCD.Framebuffer) = ERROR_SUCCESS then
    begin
     {Update PiTFT28}
     PiTFT28LCD.Framebuffer:=nil;
 
     {Check Touch Device}
     if PiTFT28LCD.Touch <> nil then
      begin
       {Check Touch Type}
       if PiTFT28LCD.Touch.Device.DeviceType = TOUCH_TYPE_CAPACITIVE then
        begin
         {Destroy Touch Device}
         if FT5X06TouchDestroy(PiTFT28LCD.Touch) <> ERROR_SUCCESS then Exit;
        end
       else
        begin
         {Destroy Touch Device}
         if STMPETouchDestroy(PiTFT28LCD.Touch) <> ERROR_SUCCESS then Exit;
        end;

       {Update PiTFT28}
       PiTFT28LCD.Touch:=nil;

       {Check Backlight Device}
       if PiTFT28LCD.Backlight <> nil then
        begin
         {Destroy Backlight Device}
         if STMPEGPIODestroy(PiTFT28LCD.Backlight) = ERROR_SUCCESS then
          begin
           {Update PiTFT28}
           PiTFT28LCD.Backlight:=nil;

           {Check Default}
           if PiTFT28Default = THandle(PiTFT28LCD) then
            begin
             PiTFT28Default:=INVALID_HANDLE_VALUE;
            end;
          end;
        end;

       {Invalidate PiTFT28}
       PiTFT28LCD.Signature:=0;

       {Destroy PiTFT28}
       FreeMem(PiTFT28LCD);

       {Return Result}
       Result:=True;
      end;
    end;
  end;
end;
 
{==============================================================================}
{==============================================================================}
{PiTFT28 Functions}

{==============================================================================}
{==============================================================================}
{PiTFT28 Helper Functions}

{==============================================================================}
{==============================================================================}

initialization
 PiTFT28Init;
 
{==============================================================================}
 
{finalization}
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
