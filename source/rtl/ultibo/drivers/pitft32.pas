{
WaveShare SpotPear TFT 3.2" LCD Driver

Copyright (C) 2016 - Rob Judd <judd@ob-wan.com>

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

  Ultibo PiTFT28.pas - Copyright (C) 2015 - SoftOz Pty Ltd.
  
References
==========

  WaveShare SpotPear 3.2" LCD Schematic
 
  http://www.waveshare.com/w/upload/a/ad/3.2inch-RPi-LCD-B-Schematic.pdf
   

WaveShare SpotPear 3.2" LCD
===========================

 The WaveShare SpotPear 3.2" LCD is a 320 x 240 pixel TFT with resistive touchscreen using an ILITEK ILI9340
 LCD driver and an LDM XPT2046 / TI ADS7843 resistive touchscreen controller.
 
 This unit ties together the various components needed to make one of these boards work with Ultibo by
 finding the correct SPI device, creating the XPT2046 Touch device, creating the ILI9340 Framebuffer
 device and registering all of it with the correct parameters for the WaveShare board.

 Details:
 
  ILI9340 LCD Driver
  
   Width:   320
   Height:  240
   
   SPI Mode: 0
   SPI Frequency: 32000000
   SPI Chip Select: SPI_CS_0
   
   DC GPIO: GPIO_PIN_22 (Pull: GPIO_PULL_NONE)
   RST GPIO: GPIO_PIN_27

   Backlight GPIO: None (Always ON)
   
  Touch Controller
  
   Chip: LDM XPT2046
   
   SPI Mode: 0
   SPI Frequency: 500000
   SPI Chip Select: SPI_CS_1
   
   IRQ GPIO: GPIO_PIN_17 (Trigger: GPIO_TRIGGER_FALLING)(Pull: GPIO_PULL_UP)
   
  Switches
  
   SW1 GPIO: GPIO_PIN_18
   
   SW2 GPIO: GPIO_PIN_23
   
   SW3 GPIO: GPIO_PIN_24
   
}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit PiTFT32; 

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,GPIO,SPI,
Framebuffer,Touch,ILI9340,{LDMTI,}SysUtils;

{==============================================================================}
{Global definitions}
{$INCLUDE ..\core\GlobalDefines.inc}

{==============================================================================}
const
 {PiTFT32 specific constants}
 PiTFT32_FRAMEBUFFER_DESCRIPTION = 'WaveShare SpotPear 3.2" LCD';

 PiTFT32_SIGNATURE = $AF000032;
 
 PiTFT32_SCREEN_WIDTH  = 240;
 PiTFT32_SCREEN_HEIGHT = 320;
 
 {PiTFT32 GPIO constants}
 PiTFT32_LCD_DC    = GPIO_PIN_22;
 PiTFT32_LCD_RST   = GPIO_PIN_27;
 PiTFT32_TOUCH_IRQ = GPIO_PIN_17;
 
 PiTFT32_LCD_BL    = GPIO_PIN_UNKNOWN; {Backlight not selectable, always ON}
 
{==============================================================================}
type
 {PiTFT32 specific types}
 PPiTFT32LCD = ^TPiTFT32LCD;
 TPiTFT32LCD = record
  Signature:LongWord;             {Signature for entry validation}
  Rotation:LongWord;              {Framebuffer rotation (eg FRAMEBUFFER_ROTATION_180)}
  Direction:LongWord;             {Framebuffer direction (eg FRAMEBUFFER_DIRECTION_REVERSE)}
  SPI:PSPIDevice;                 {SPI device for this display}
  GPIO:PGPIODevice;               {GPIO device for this display}
  Touch:PTouchDevice;             {Touch (LDMTI) device for this display}
  Backlight:PGPIODevice;          {Backlight GPIO (LDMTI) device for this display}
  Framebuffer:PFramebufferDevice; {Framebuffer (ILI9340) device for this display}
 end;
 
{==============================================================================}
var
 {PiTFT32 specific variables}
 PiTFT32_SPI_DEVICE:String = 'SPI0';
 PiTFT32_LCD_CHIPSELECT:Word = SPI_CS_0;
 PiTFT32_TOUCH_CHIPSELECT:Word = SPI_CS_1;
 
{==============================================================================}
{Initialization Functions}
procedure PiTFT32Init;

function PiTFT32Start(Rotation,Direction:LongWord;const Device:String;DisplaySelect,TouchSelect:Word):THandle;
function PiTFT32Stop(Handle:THandle):Boolean;

{==============================================================================}
{PiTFT32 Functions}

{==============================================================================}
{PiTFT32 Helper Functions}
 
{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {PiTFT32 specific variables}
 PiTFT32Initialized:Boolean;
 
 PiTFT32Default:THandle = INVALID_HANDLE_VALUE;
 
{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure PiTFT32Init;
{Initialize the PiTFT32 unit and parameters}

{Note: Called only during system startup}
var
 WorkInt:LongWord;
 WorkBuffer:String;
begin
 {}
 {Check Initialized}
// if PiTFT32Initialized then Exit;

 {Check Environment Variables}
 {PiTFT32_AUTOSTART}
 WorkInt:=StrToIntDef(SysUtils.GetEnvironmentVariable('PiTFT32_AUTOSTART'),1);
 if WorkInt = 0 then PiTFT32_AUTOSTART:=False;
 
 {PiTFT32_SPI_DEVICE}
 WorkBuffer:=SysUtils.GetEnvironmentVariable('PiTFT32_SPI_DEVICE');
 if Length(WorkBuffer) <> 0 then PiTFT32_SPI_DEVICE:=WorkBuffer;

 {PiTFT32_LCD_CHIPSELECT}
 WorkInt:=StrToIntDef(SysUtils.GetEnvironmentVariable('PiTFT32_LCD_CHIPSELECT'),0);
 if WorkInt > 0 then PiTFT32_LCD_CHIPSELECT:=WorkInt;

 {PiTFT32_TOUCH_CHIPSELECT}
 WorkInt:=StrToIntDef(SysUtils.GetEnvironmentVariable('PiTFT32_TOUCH_CHIPSELECT'),0);
 if WorkInt > 0 then PiTFT32_TOUCH_CHIPSELECT:=WorkInt;
 
 {Start PiTFT32} 
 if PiTFT32_AUTOSTART then
  begin
   PiTFT32Default:=PiTFT32Start(FRAMEBUFFER_ROTATION_270,FRAMEBUFFER_DIRECTION_REVERSE,
     PiTFT32_SPI_DEVICE,PiTFT32_LCD_CHIPSELECT,PiTFT32_TOUCH_CHIPSELECT);
  end;
 
 PiTFT32Initialized:=True;
end;

{==============================================================================}

function PiTFT32Start(Rotation,Direction:LongWord;const Device:String;DisplaySelect,TouchSelect:Word):THandle;
{Start the PiTFT32 driver and register the Touch, Backlight (GPIO) and Framebuffer devices associated with the display}
{Rotation: The rotation of the display (eg FRAMEBUFFER_ROTATION_180)}
{Direction: The direction of the display (eg FRAMEBUFFER_DIRECTION_REVERSE)}
{Device: The SPI device that the ILI9340 and XPT2046 devices are connected to}
{DisplaySelect: The SPI chip select of the ILI9340 LCD controller}
{TouchSelect: The SPI chip select of the XPT2046 touch controller}
{Return: The handle of the PiTFT32 on success or INVALID_HANDLE_VALUE on failure}

{Note: This function will be called during startup if the parameter PiTFT32_AUTOSTART is True
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
 
 PiTFT32LCD:PPiTFT32LCD;
begin
 {}
 Result:=INVALID_HANDLE_VALUE;
 
 {Check Rotation}
 if Rotation > FRAMEBUFFER_ROTATION_270 then Exit;
 
 {Check Direction}
 if Direction > FRAMEBUFFER_DIRECTION_REVERSE then Exit;
 
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
 IRQ.Pin:=PiTFT32_TOUCH_IRQ;
 IRQ.Func:=GPIO_FUNCTION_IN;
 IRQ.Pull:=GPIO_PULL_UP;
 IRQ.Trigger:=GPIO_TRIGGER_FALLING;
 
 {Create Backlight (GPIO) Device}
 {Backlight:=LDMGPIOCreate(nil,SPI,I2C_ADDRESS_INVALID,TouchSelect,nil);} {IRQ not available for GPIO}
 {if Backlight = nil then Exit;
 try}
  {Create Touch Device} {***FIXME***}
  {Touch:=LDMTouchCreate(nil,SPI,I2C_ADDRESS_INVALID,TouchSelect,PiTFT32_SCREEN_WIDTH,PiTFT32_SCREEN_HEIGHT,@IRQ);
  if Touch = nil then Exit;
  try}
   {Setup Display RST (Reset)}
   RST.GPIO:=GPIO;   
   RST.Pin:=PiTFT32_LCD_RST;
   RST.Func:=GPIO_FUNCTION_OUT;
   RST.Pull:=GPIO_PULL_NONE;
   RST.Trigger:=GPIO_TRIGGER_UNKNOWN;
    
   {Setup Display DC (Data/Command)}
   DC.GPIO:=GPIO;
   DC.Pin:=PiTFT32_LCD_DC;
   DC.Func:=GPIO_FUNCTION_OUT;
   DC.Pull:=GPIO_PULL_NONE;
   DC.Trigger:=GPIO_TRIGGER_UNKNOWN;
   
   {Setup Display BL (Backlight)}
   BL.GPIO:=Backlight;
   BL.Pin:=PiTFT32_LCD_BL;
   BL.Func:=GPIO_FUNCTION_OUT;
   BL.Pull:=GPIO_PULL_NONE;
   BL.Trigger:=GPIO_TRIGGER_UNKNOWN;
   
   {Create Framebuffer Device}
   Framebuffer:=ILI9340FramebufferCreate(SPI,DisplaySelect,PiTFT32_FRAMEBUFFER_DESCRIPTION,Rotation,Direction,PiTFT32_SCREEN_WIDTH,PiTFT32_SCREEN_HEIGHT,@RST,@DC,@BL);
   if Framebuffer = nil then Exit;
   try
    {Create PiTFT32}
    PiTFT32LCD:=AllocMem(SizeOf(TPiTFT32LCD));
    if PiTFT32LCD = nil then Exit;
    
    {Update PiTFT32}
    PiTFT32LCD.Signature:=PiTFT32_SIGNATURE;
    PiTFT32LCD.Rotation:=Rotation;
    PiTFT32LCD.Direction:=Direction;
    PiTFT32LCD.SPI:=SPI;
    PiTFT32LCD.GPIO:=GPIO;
    PiTFT32LCD.Touch:=Touch;
    PiTFT32LCD.Backlight:=Backlight;
    PiTFT32LCD.Framebuffer:=Framebuffer;
    
    {Return Result}
    Result:=THandle(PiTFT32LCD);
    
    {Check Default}
    if PiTFT32Default = INVALID_HANDLE_VALUE then
     begin
      PiTFT32Default:=Result;
     end;
   finally
    if Result = INVALID_HANDLE_VALUE then ILI9340FramebufferDestroy(Framebuffer);
   end;
  {finally
   if Result = INVALID_HANDLE_VALUE then LDMTouchDestroy(Touch);
  end;
  finally
  if Result = INVALID_HANDLE_VALUE then LDMGPIODestroy(Backlight);
 end;}
end;

{==============================================================================}

function PiTFT32Stop(Handle:THandle):Boolean;
{Stop the PiTFT32 driver and deregister the Touch, Backlight (GPIO) and Framebuffer
 devices associated with the display}
{Handle: The handle of the PiTFT32 or INVALID_HANDLE_VALUE for the default display}
{Return: True if completed or False on failure}
var
 PiTFT32LCD:PPiTFT32LCD;
begin
 {}
 Result:=False;
 
 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Handle:=PiTFT32Default;
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {Get LCD}
 PiTFT32LCD:=PPiTFT32LCD(Handle);
 if PiTFT32LCD = nil then Exit;
 if PiTFT32LCD.Signature <> PiTFT32_SIGNATURE then Exit;
 
 {Check Framebuffer Device}
 if PiTFT32LCD.Framebuffer <> nil then
  begin
   {Destroy Framebuffer Device}
   if ILI9340FramebufferDestroy(PiTFT32LCD.Framebuffer) = ERROR_SUCCESS then
    begin
     {Update PiTFT32}
     PiTFT32LCD.Framebuffer:=nil;
 
     {Check Touch Device}
     if PiTFT32LCD.Touch <> nil then
      begin
       {Destroy Touch Device}
       {if LDMTouchDestroy(PiTFT32LCD.Touch) = ERROR_SUCCESS then
        begin}
         {Update PiTFT32}
         PiTFT32LCD.Touch:=nil;
         
         {Check Backlight Device}
         if PiTFT32LCD.Backlight <> nil then
          begin
           {Destroy Backlight Device}
           {if LDMGPIODestroy(PiTFT32LCD.Backlight) = ERROR_SUCCESS then
            begin}
             {Update PiTFT32}
             PiTFT32LCD.Backlight:=nil;
         
             {Check Default}
             if PiTFT32Default = THandle(PiTFT32LCD) then
              begin
               PiTFT32Default:=INVALID_HANDLE_VALUE;
              end;
             
             {Invalidate PiTFT32}
             PiTFT32LCD.Signature:=0;
             
             {Destroy PiTFT32}
             FreeMem(PiTFT32LCD);
             
             {Return Result}
             Result:=True;
            end; 
          end;
		  
        {end;
      end;}
    end;
  end;
end;
 
{==============================================================================}
{==============================================================================}
{PiTFT32 Functions}

{==============================================================================}
{==============================================================================}
{PiTFT32 Helper Functions}

{==============================================================================}
{==============================================================================}

initialization
 PiTFT32Init;
 
{==============================================================================}
 
{finalization}
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
