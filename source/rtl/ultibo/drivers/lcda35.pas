{
Waveshare RPi LCD(A) 3.5" Driver.

Copyright (C) 2025 - @rcla

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

  pitft35.pas

References
==========

 Waveshare RPi LCD(A) 3.5"

  Product

   https://www.waveshare.com/product/3.5inch-RPi-LCD-A.htm

  Wiki

   https://www.waveshare.com/wiki/3.5inch_RPi_LCD_(A)


Waveshare RPi LCD(A) 3.5"
=========================

 Waveshare RPi LCD(A) 3.5" is a 480 x 320 pixel TFT with resistive touchscreen using a ILI9486
 driver and a XPT2046 resistive touchscreen controller.

 This unit ties together the various components needed to make one of these boards work with Ultibo
 by finding the correct SPI device, creating the XPT2046 Touch device (Not yet implemented), creating
 the ILI9486 Framebuffer device and registering all of it with the correct parameters for the Waveshare
 board.

 Details:

  ILI9486

   Width:   320
   Height:  480

   SPI Mode: 0
   SPI Frequency: 32000000
   SPI Chip Select: SPI_CS_0

   DC GPIO: GPIO_PIN_24 (Pull: GPIO_PULL_NONE)
   RST GPIO: GPIO_PIN_25

   Backlight GPIO: Unused (Not supported on this screen)

  Touch Controller

   Chip: XPT2046

   SPI Mode: 0
   SPI Frequency: 500000
   SPI Chip Select: SPI_CS_1

   IRQ GPIO: GPIO_PIN_17 (Trigger: GPIO_TRIGGER_FALLING)(Pull: GPIO_PULL_UP)

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

{$IFNDEF FPC_DOTTEDUNITS}
unit LCDA35;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  Core.GlobalConfig,
  Core.GlobalConst,
  Core.Platform,
  Core.Threads,
  Core.Devices,
  Core.GPIO,
  Core.SPI,
  Core.Framebuffer,
  Core.Touch,
  Drivers.ILI9486,
  System.SysUtils;
{$ELSE FPC_DOTTEDUNITS}
uses
  GlobalConfig,
  GlobalConst,
  Platform,
  Threads,
  Devices,
  GPIO,
  SPI,
  Framebuffer,
  Touch,
  ILI9486,
  SysUtils;
{$ENDIF FPC_DOTTEDUNITS}

{==============================================================================}
{Global definitions}
{$INCLUDE ..\core\GlobalDefines.inc}

{==============================================================================}
const
 {LCDA35 specific constants}
 LCDA35_FRAMEBUFFER_DESCRIPTION = 'Waveshare RPi LCD(A) 3.5"';

 LCDA35_SIGNATURE = $AF000A35;

 LCDA35_SCREEN_WIDTH  = 320;
 LCDA35_SCREEN_HEIGHT = 480;

 {LCDA35 GPIO constants}
 LCDA35_LCD_DC    = GPIO_PIN_24;
 LCDA35_LCD_RST   = GPIO_PIN_25;
 LCDA35_LCD_BL    = GPIO_PIN_UNKNOWN; {Not supported on this screen}

 LCDA35_TOUCH_IRQ = GPIO_PIN_17;

{==============================================================================}
type
 {LCDA35 specific types}
 PLCDA35LCD = ^TLCDA35LCD;
 TLCDA35LCD = record
  Signature:LongWord;             {Signature for entry validation}
  Rotation:LongWord;              {Framebuffer rotation (eg FRAMEBUFFER_ROTATION_180)}
  Direction:LongWord;             {Framebuffer direction (eg FRAMEBUFFER_DIRECTION_REVERSE)}
  SPI:PSPIDevice;                 {SPI device for this display}
  GPIO:PGPIODevice;               {GPIO device for this display}
  Touch:PTouchDevice;             {Touch device for this display}
  Backlight:PGPIODevice;          {Backlight GPIO device for this display}
  Framebuffer:PFramebufferDevice; {Framebuffer (ILI9486) device for this display}
 end;

{==============================================================================}
var
 {LCDA35 specific variables}
 LCDA35_SPI_DEVICE:String = 'SPI0';
 LCDA35_LCD_CHIPSELECT:Word = SPI_CS_0;
 LCDA35_TOUCH_CHIPSELECT:Word = SPI_CS_1;
 LCDA35_AUTOSTART:LongBool = True;

{==============================================================================}
{Initialization Functions}
procedure LCDA35Init;

function LCDA35Start(Rotation:LongWord;const Device:String;DisplaySelect,TouchSelect:Word):THandle;{$IFDEF API_EXPORT_LCDA35} stdcall; public name 'lcda35_start';{$ENDIF}
function LCDA35Stop(Handle:THandle):Boolean;{$IFDEF API_EXPORT_LCDA35} stdcall; public name 'lcda35_stop';{$ENDIF}

{==============================================================================}
{LCDA35 Functions}

{==============================================================================}
{LCDA35 Helper Functions}

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {LCDA35 specific variables}
 LCDA35Initialized:Boolean;

 LCDA35Default:THandle = INVALID_HANDLE_VALUE;

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure LCDA35Init;
{Initialize the LCDA35 unit and parameters}

{Note: Called only during system startup}
var
 WorkInt:LongWord;
 WorkBool:LongBool;
 WorkBuffer:String;
begin
 {}
 {Check Initialized}
 if LCDA35Initialized then Exit;

 {Check Environment Variables}
 {LCDA35_AUTOSTART}
 WorkBool:=StrToBoolDef(EnvironmentGet('LCDA35_AUTOSTART'),LCDA35_AUTOSTART);
 if WorkBool <> LCDA35_AUTOSTART then LCDA35_AUTOSTART:=WorkBool;

 {LCDA35_SPI_DEVICE}
 WorkBuffer:=EnvironmentGet('LCDA35_SPI_DEVICE');
 if Length(WorkBuffer) <> 0 then LCDA35_SPI_DEVICE:=WorkBuffer;

 {LCDA35_LCD_CHIPSELECT}
 WorkInt:=StrToIntDef(EnvironmentGet('LCDA35_LCD_CHIPSELECT'),LCDA35_LCD_CHIPSELECT);
 if WorkInt <> LCDA35_LCD_CHIPSELECT then LCDA35_LCD_CHIPSELECT:=WorkInt;

 {LCDA35_TOUCH_CHIPSELECT}
 WorkInt:=StrToIntDef(EnvironmentGet('LCDA35_TOUCH_CHIPSELECT'),LCDA35_TOUCH_CHIPSELECT);
 if WorkInt <> LCDA35_TOUCH_CHIPSELECT then LCDA35_TOUCH_CHIPSELECT:=WorkInt;

 {Start LCDA35}
 if LCDA35_AUTOSTART then
  begin
   LCDA35Default:=LCDA35Start(FRAMEBUFFER_ROTATION_0,LCDA35_SPI_DEVICE,LCDA35_LCD_CHIPSELECT,LCDA35_TOUCH_CHIPSELECT);
  end;

 LCDA35Initialized:=True;
end;

{==============================================================================}

function LCDA35Start(Rotation:LongWord;const Device:String;DisplaySelect,TouchSelect:Word):THandle;{$IFDEF API_EXPORT_LCDA35} stdcall;{$ENDIF}
{Start the LCDA35 driver and register the Touch and Framebuffer devices associated with the display}
{Rotation: The rotation of the display (eg FRAMEBUFFER_ROTATION_180)}
{Device: The SPI device that the ILI9486 and XPT2046 devices are connected to}
{DisplaySelect: The SPI chip select of the ILI9486 LCD controller}
{TouchSelect: The SPI chip select of the XPT2046 touch controller}
{Return: The handle of the LCDA35 on success or INVALID_HANDLE_VALUE on failure}

{Note: This function will be called during startup if the parameter LCDA35_AUTOSTART is True
       Can be called multiple times to support more than one LCD display}
var
 SPI:PSPIDevice;
 GPIO:PGPIODevice;
 Touch:PTouchDevice;
 Framebuffer:PFramebufferDevice;

 DC:TGPIOInfo;
 RST:TGPIOInfo;
 IRQ:TGPIOInfo;

 LCDA35LCD:PLCDA35LCD;
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
 IRQ.Pin:=LCDA35_TOUCH_IRQ;
 IRQ.Func:=GPIO_FUNCTION_IN;
 IRQ.Pull:=GPIO_PULL_UP;
 IRQ.Trigger:=GPIO_TRIGGER_FALLING;

 {Setup Display RST (Reset)}
 RST.GPIO:=GPIO;
 RST.Pin:=LCDA35_LCD_RST;
 RST.Func:=GPIO_FUNCTION_OUT;
 RST.Pull:=GPIO_PULL_NONE;
 RST.Trigger:=GPIO_TRIGGER_UNKNOWN;

 {Setup Display DC (Data/Command)}
 DC.GPIO:=GPIO;
 DC.Pin:=LCDA35_LCD_DC;
 DC.Func:=GPIO_FUNCTION_OUT;
 DC.Pull:=GPIO_PULL_NONE;
 DC.Trigger:=GPIO_TRIGGER_UNKNOWN;

 {Create Touch Device (Not yet implemented}
 Touch:=nil;

 {Create Framebuffer Device}
 Framebuffer:=ILI9486FramebufferCreate(SPI,DisplaySelect,LCDA35_FRAMEBUFFER_DESCRIPTION,Rotation,LCDA35_SCREEN_WIDTH,LCDA35_SCREEN_HEIGHT,@RST,@DC,nil);
 if Framebuffer = nil then Exit;
 try
   {Create LCDA35}
   LCDA35LCD:=AllocMem(SizeOf(TLCDA35LCD));
   if LCDA35LCD = nil then Exit;

   {Update LCDA35}
   LCDA35LCD.Signature:=LCDA35_SIGNATURE;
   LCDA35LCD.Rotation:=Rotation;
   LCDA35LCD.SPI:=SPI;
   LCDA35LCD.GPIO:=GPIO;
   LCDA35LCD.Touch:=Touch;
   LCDA35LCD.Backlight:=nil;
   LCDA35LCD.Framebuffer:=Framebuffer;

   {Return Result}
   Result:=THandle(LCDA35LCD);

   {Check Default}
   if LCDA35Default = INVALID_HANDLE_VALUE then
   begin
     LCDA35Default:=Result;
   end;
 finally
   if Result = INVALID_HANDLE_VALUE then ILI9486FramebufferDestroy(Framebuffer);
 end;

end;

{==============================================================================}

function LCDA35Stop(Handle:THandle):Boolean;{$IFDEF API_EXPORT_LCDA35} stdcall;{$ENDIF}
{Stop the LCDA35 driver and deregister the Touch and Framebuffer devices associated with the display}
{Handle: The handle of the LCDA35 or INVALID_HANDLE_VALUE for the default display}
{Return: True if completed or False on failure}
var
 LCDA35LCD:PLCDA35LCD;
begin
 {}
 Result:=False;

 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Handle:=LCDA35Default;
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {Get LCD}
 LCDA35LCD:=PLCDA35LCD(Handle);
 if LCDA35LCD = nil then Exit;
 if LCDA35LCD.Signature <> LCDA35_SIGNATURE then Exit;

 {Check Framebuffer Device}
 if LCDA35LCD.Framebuffer <> nil then
  begin
   {Destroy Framebuffer Device}
   if ILI9486FramebufferDestroy(LCDA35LCD.Framebuffer) = ERROR_SUCCESS then
    begin
     {Update LCDA35}
     LCDA35LCD.Framebuffer:=nil;

     {Check Touch Device}
     {if LCDA35LCD.Touch <> nil then} {Not yet implemented}
      begin
       LCDA35LCD.Touch:=nil;

       {Check Backlight Device}
       {if LCDA35LCD.Backlight <> nil then} {Not supported on this screen}
        begin
         LCDA35LCD.Backlight:=nil;

         {Check Default}
         if LCDA35Default = THandle(LCDA35LCD) then
          begin
           LCDA35Default:=INVALID_HANDLE_VALUE;
          end;

         {Invalidate LCDA35}
         LCDA35LCD.Signature:=0;

         {Destroy LCDA35}
         FreeMem(LCDA35LCD);

         {Return Result}
         Result:=True;
        end;
      end;
    end;
  end;
end;

{==============================================================================}
{==============================================================================}
{LCDA35 Functions}

{==============================================================================}
{==============================================================================}
{LCDA35 Helper Functions}

{==============================================================================}
{==============================================================================}

initialization
 LCDA35Init;

{==============================================================================}

{finalization}
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
