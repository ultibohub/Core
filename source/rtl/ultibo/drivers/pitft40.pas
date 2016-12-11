{
WaveShare SpotPear TFT 3.5" and 4.0" LCD Driver

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

  WaveShare SpotPear 4.0" general data
  
  http://www.waveshare.com/product/modules/oleds-lcds/raspberry-pi-lcd/4inch-rpi-lcd-a.htm
  
  
WaveShare SpotPear 3.5" and 4.0" LCD
====================================

 The WaveShare SpotPear 3.5" and 4.0" LCDs are 320 x 480 pixel TFT with resistive touchscreen using an ILITEK ILI9486 LCD driver and an LDM XPT2046 / TI ADS7843 resistive touchscreen controller.
 
 This unit ties together the various components needed to make one of these boards work with Ultibo by
 finding the correct SPI device, creating the XPT2046 Touch device, creating the ILI9486 Framebuffer
 device and registering all of it with the correct parameters for the WaveShare board.

 Details:
 
  ILI9486 LCD Driver
  
   Width:   320
   Height:  480
   
   SPI Mode: 0
   SPI Frequency: 32000000
   SPI Chip Select: SPI_CS_0
   
   DC GPIO: GPIO_PIN_24 (Pull: GPIO_PULL_NONE)
   RST GPIO: GPIO_PIN_25

   Backlight GPIO: None (Always ON)
   
  Touch Controller
  
   Chip: LDM XPT2046
   
   SPI Mode: 0
   SPI Frequency: 500000
   SPI Chip Select: SPI_CS_1
   
   IRQ GPIO: GPIO_PIN_17 (Trigger: GPIO_TRIGGER_FALLING)(Pull: GPIO_PULL_UP)
   
  Switches
  
   None
  
}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit PiTFT40; 

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,GPIO,SPI,
Framebuffer,Touch,ILI9486,{LDMTI,}SysUtils;

{==============================================================================}
{Global definitions}
{$INCLUDE ..\core\GlobalDefines.inc}

{==============================================================================}
const
 {PiTFT40 specific constants}
 PiTFT40_FRAMEBUFFER_DESCRIPTION = 'WaveShare SpotPear 4.0" LCD';

 PiTFT40_SIGNATURE = $AF000040;
 
 PiTFT40_SCREEN_WIDTH  = 480;
 PiTFT40_SCREEN_HEIGHT = 320;
 
 {PiTFT40 GPIO constants}
 PiTFT40_LCD_DC    = GPIO_PIN_24;
 PiTFT40_LCD_RST   = GPIO_PIN_25;
 PiTFT40_TOUCH_IRQ = GPIO_PIN_17;
 
 PiTFT40_LCD_BL    = GPIO_PIN_UNKNOWN; {Backlight not selectable, always ON}
 
{==============================================================================}
type
 {PiTFT40 specific types}
 PPiTFT40LCD = ^TPiTFT40LCD;
 TPiTFT40LCD = record
  Signature:LongWord;             {Signature for entry validation}
  Rotation:LongWord;              {Framebuffer rotation (eg FRAMEBUFFER_ROTATION_180)}
  Direction:LongWord;             {Framebuffer direction (eg FRAMEBUFFER_DIRECTION_REVERSE)}
  SPI:PSPIDevice;                 {SPI device for this display}
  GPIO:PGPIODevice;               {GPIO device for this display}
  Touch:PTouchDevice;             {Touch (LDMTI) device for this display}
  Backlight:PGPIODevice;          {Backlight GPIO (LDMTI) device for this display}
  Framebuffer:PFramebufferDevice; {Framebuffer (ILI9486) device for this display}
 end;
 
{==============================================================================}
var
 {PiTFT40 specific variables}
 PiTFT40_SPI_DEVICE:String = 'SPI0';
 PiTFT40_LCD_CHIPSELECT:Word = SPI_CS_0;
 PiTFT40_TOUCH_CHIPSELECT:Word = SPI_CS_1;
 
{==============================================================================}
{Initialization Functions}
procedure PiTFT40Init;

function PiTFT40Start(Rotation,Direction:LongWord;const Device:String;DisplaySelect,TouchSelect:Word):THandle;
function PiTFT40Stop(Handle:THandle):Boolean;

{==============================================================================}
{PiTFT40 Functions}

{==============================================================================}
{PiTFT40 Helper Functions}
 
{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {PiTFT40 specific variables}
 PiTFT40Initialized:Boolean;
 
 PiTFT40Default:THandle = INVALID_HANDLE_VALUE;
 
{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure PiTFT40Init;
{Initialize the PiTFT40 unit and parameters}

{Note: Called only during system startup}
var
 WorkInt:LongWord;
 WorkBuffer:String;
begin
 {}
 {Check Initialized}
 if PiTFT40Initialized then Exit;
 
 {Check Environment Variables}
 {PiTFT40_AUTOSTART}
 WorkInt:=StrToIntDef(SysUtils.GetEnvironmentVariable('PiTFT40_AUTOSTART'),1);
 if WorkInt = 0 then PiTFT40_AUTOSTART:=False;
 
 {PiTFT40_SPI_DEVICE}
 WorkBuffer:=SysUtils.GetEnvironmentVariable('PiTFT40_SPI_DEVICE');
 if Length(WorkBuffer) <> 0 then PiTFT40_SPI_DEVICE:=WorkBuffer;

 {PiTFT40_LCD_CHIPSELECT}
 WorkInt:=StrToIntDef(SysUtils.GetEnvironmentVariable('PiTFT40_LCD_CHIPSELECT'),0);
 if WorkInt > 0 then PiTFT40_LCD_CHIPSELECT:=WorkInt;

 {PiTFT40_TOUCH_CHIPSELECT}
 WorkInt:=StrToIntDef(SysUtils.GetEnvironmentVariable('PiTFT40_TOUCH_CHIPSELECT'),0);
 if WorkInt > 0 then PiTFT40_TOUCH_CHIPSELECT:=WorkInt;
 
 {Start PiTFT40} 
 if PiTFT40_AUTOSTART then
  begin
   PiTFT40Default:=PiTFT40Start(FRAMEBUFFER_ROTATION_270,FRAMEBUFFER_DIRECTION_REVERSE,
     PiTFT40_SPI_DEVICE,PiTFT40_LCD_CHIPSELECT,PiTFT40_TOUCH_CHIPSELECT);
  end;
 
 PiTFT40Initialized:=True;
end;

{==============================================================================}

function PiTFT40Start(Rotation,Direction:LongWord;const Device:String;DisplaySelect,TouchSelect:Word):THandle;
{Start the PiTFT40 driver and register the Touch, Backlight (GPIO) and Framebuffer devices associated with the display}
{Rotation: The rotation of the display (eg FRAMEBUFFER_ROTATION_180)}
{Direction: The direction of the display (eg FRAMEBUFFER_DIRECTION_REVERSE)}
{Device: The SPI device that the ILI9486 and XPT2046 devices are connected to}
{DisplaySelect: The SPI chip select of the ILI9486 LCD controller}
{TouchSelect: The SPI chip select of the XPT2046 touch controller}
{Return: The handle of the PiTFT40 on success or INVALID_HANDLE_VALUE on failure}

{Note: This function will be called during startup if the parameter PiTFT40_AUTOSTART is True
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
 
 PiTFT40LCD:PPiTFT40LCD;
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
 IRQ.Pin:=PiTFT40_TOUCH_IRQ;
 IRQ.Func:=GPIO_FUNCTION_IN;
 IRQ.Pull:=GPIO_PULL_UP;
 IRQ.Trigger:=GPIO_TRIGGER_FALLING;
 
 {Create Backlight (GPIO) Device}
 {Backlight:=LDMGPIOCreate(nil,SPI,I2C_ADDRESS_INVALID,TouchSelect,nil);} {IRQ not available for GPIO}
 {if Backlight = nil then Exit;
 try}
  {Create Touch Device} {***FIXME***}
  {Touch:=LDMTouchCreate(nil,SPI,I2C_ADDRESS_INVALID,TouchSelect,PiTFT40_SCREEN_WIDTH,PiTFT40_SCREEN_HEIGHT,@IRQ);
  if Touch = nil then Exit;
  try}
   {Setup Display RST (Reset)}
   RST.GPIO:=GPIO;   
   RST.Pin:=PiTFT40_LCD_RST;
   RST.Func:=GPIO_FUNCTION_OUT;
   RST.Pull:=GPIO_PULL_NONE;
   RST.Trigger:=GPIO_TRIGGER_UNKNOWN;
    
   {Setup Display DC (Data/Command)}
   DC.GPIO:=GPIO;
   DC.Pin:=PiTFT40_LCD_DC;
   DC.Func:=GPIO_FUNCTION_OUT;
   DC.Pull:=GPIO_PULL_NONE;
   DC.Trigger:=GPIO_TRIGGER_UNKNOWN;
   
   {Setup Display BL (Backlight)}
   BL.GPIO:=Backlight;
   BL.Pin:=PiTFT40_LCD_BL;
   BL.Func:=GPIO_FUNCTION_OUT;
   BL.Pull:=GPIO_PULL_NONE;
   BL.Trigger:=GPIO_TRIGGER_UNKNOWN;
   
   {Create Framebuffer Device}
   Framebuffer:=ILI9486FramebufferCreate(SPI,DisplaySelect,PiTFT40_FRAMEBUFFER_DESCRIPTION,Rotation,Direction,PiTFT40_SCREEN_WIDTH,PiTFT40_SCREEN_HEIGHT,@RST,@DC,@BL);
   if Framebuffer = nil then Exit;
   try
    {Create PiTFT40}
    PiTFT40LCD:=AllocMem(SizeOf(TPiTFT40LCD));
    if PiTFT40LCD = nil then Exit;
    
    {Update PiTFT40}
    PiTFT40LCD.Signature:=PiTFT40_SIGNATURE;
    PiTFT40LCD.Rotation:=Rotation;
    PiTFT40LCD.Direction:=Direction;
    PiTFT40LCD.SPI:=SPI;
    PiTFT40LCD.GPIO:=GPIO;
    PiTFT40LCD.Touch:=Touch;
    PiTFT40LCD.Backlight:=Backlight;
    PiTFT40LCD.Framebuffer:=Framebuffer;
    
    {Return Result}
    Result:=THandle(PiTFT40LCD);
    
    {Check Default}
    if PiTFT40Default = INVALID_HANDLE_VALUE then
     begin
      PiTFT40Default:=Result;
     end;
   finally
    if Result = INVALID_HANDLE_VALUE then ILI9486FramebufferDestroy(Framebuffer);
   end;
  {finally
   if Result = INVALID_HANDLE_VALUE then LDMTouchDestroy(Touch);
  end;
  finally
  if Result = INVALID_HANDLE_VALUE then LDMGPIODestroy(Backlight);
 end;}
end;

{==============================================================================}

function PiTFT40Stop(Handle:THandle):Boolean;
{Stop the PiTFT40 driver and deregister the Touch, Backlight (GPIO) and Framebuffer
 devices associated with the display}
{Handle: The handle of the PiTFT40 or INVALID_HANDLE_VALUE for the default display}
{Return: True if completed or False on failure}
var
 PiTFT40LCD:PPiTFT40LCD;
begin
 {}
 Result:=False;
 
 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Handle:=PiTFT40Default;
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {Get LCD}
 PiTFT40LCD:=PPiTFT40LCD(Handle);
 if PiTFT40LCD = nil then Exit;
 if PiTFT40LCD.Signature <> PiTFT40_SIGNATURE then Exit;
 
 {Check Framebuffer Device}
 if PiTFT40LCD.Framebuffer <> nil then
  begin
   {Destroy Framebuffer Device}
   if ILI9486FramebufferDestroy(PiTFT40LCD.Framebuffer) = ERROR_SUCCESS then
    begin
     {Update PiTFT40}
     PiTFT40LCD.Framebuffer:=nil;
 
     {Check Touch Device}
     if PiTFT40LCD.Touch <> nil then
      begin
       {Destroy Touch Device}
       {if LDMTouchDestroy(PiTFT40LCD.Touch) = ERROR_SUCCESS then
        begin}
         {Update PiTFT40}
         PiTFT40LCD.Touch:=nil;
         
         {Check Backlight Device}
         if PiTFT40LCD.Backlight <> nil then
          begin
           {Destroy Backlight Device}
           {if LDMGPIODestroy(PiTFT40LCD.Backlight) = ERROR_SUCCESS then
            begin}
             {Update PiTFT40}
             PiTFT40LCD.Backlight:=nil;
         
             {Check Default}
             if PiTFT40Default = THandle(PiTFT40LCD) then
              begin
               PiTFT40Default:=INVALID_HANDLE_VALUE;
              end;
             
             {Invalidate PiTFT40}
             PiTFT40LCD.Signature:=0;
             
             {Destroy PiTFT40}
             FreeMem(PiTFT40LCD);
             
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
{PiTFT40 Functions}

{==============================================================================}
{==============================================================================}
{PiTFT40 Helper Functions}

{==============================================================================}
{==============================================================================}

initialization
 PiTFT40Init;
 
{==============================================================================}
 
{finalization}
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
