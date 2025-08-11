{
ILITEK ILI9486 TFT LCD Driver.

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

  Linux - \drivers\staging\fbtft\fb_ili9486.c - Copyright (C) 2013 Noralf Tronnes
  Linux - \drivers\staging\fbtft\fbtft_device.c - Copyright (C) 2013 Noralf Tronnes
  Linux - \drivers\staging\fbtft\fbtft-core.c - Copyright (C) 2013 Noralf Tronnes

References
==========

  ILI9486 - https://www.hpinfotech.ro/ILI9486.pdf

  https://github.com/notro/fbtft/wiki
  https://github.com/notro/fbtft/wiki/LCD-Modules
  https://github.com/notro/fbtft/wiki/LCD-Shields

ILITEK ILI9486
==============

 The ILITEK ILI9486 is a 320x480 Resolution RGB 262K color TFT LCD Single Chip Driver
 that supports color depths of 16 or 18bit. This driver supports the chip only in 16
 bit depth using RGB565 format.

 The chip provides an SPI interface at up to 32MHz (some manufacturers allow higher speeds)
 and supports rotations of 0, 90, 180 and 270 degrees.

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

{$IFNDEF FPC_DOTTEDUNITS}
unit ILI9486;
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
  Core.TFTFramebuffer,
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
  TFTFramebuffer,
  SysUtils;
{$ENDIF FPC_DOTTEDUNITS}

{==============================================================================}
{Global definitions}
{$INCLUDE ..\core\GlobalDefines.inc}

{==============================================================================}
const
 {ILI9486 specific constants}
 ILI9486_FRAMEBUFFER_DESCRIPTION = 'ILITEK ILI9486 TFT LCD';  {Description of ILI9486 device}

 {ILI9486 SPI constants}
 ILI9486_SPI_RATE = 32000000; {Default SPI clock rate}

 {ILI9486 Command constants}
 ILI9486_CMD_NOP      = $00; {8.2.1. NOP: No Operation}
 ILI9486_CMD_SWRESET  = $01; {8.2.2. SWRESET: Software Reset}

 ILI9486_CMD_SLPOUT   = $11; {8.2.13. SLPOUT: Sleep Out (This command turns off sleep mode)}

 ILI9486_CMD_DISPOFF  = $28; {8.2.18. DISPOFF: Display OFF (This command is used to enter into DISPLAY OFF mode. In this mode, the output from Frame Memory is disabled and blank page inserted)}
 ILI9486_CMD_DISPON   = $29; {8.2.19. DISPON: Display ON (This command is used to recover from DISPLAY OFF mode. Output from the Frame Memory is enabled)}

 ILI9486_CMD_CASET    = $2A; {8.2.20. CASET: Column Address Set (This command is used to define area of frame memory where MCU can access)}
 ILI9486_CMD_PASET    = $2B; {8.2.21. PASET: Page Address Set (This command is used to define area of frame memory where MCU can access)}
 ILI9486_CMD_RAMWR    = $2C; {8.2.22. Memory Write (This command is used to transfer data from MCU to frame memory)}

 ILI9486_CMD_MADCTL   = $36; {8.2.28. MADCTL: Memory Access Control (This command defines read/write scanning direction of frame memory)}

 ILI9486_CMD_COLMOD   = $3A; {8.2.32. COLMOD: Pixel Format Set (This command sets the pixel format for the RGB image data used by the interface)}

 ILI9486_CMD_IFMODE   = $B0; {8.2.50. IFMODE : Interface Mode Control}

 ILI9486_CMD_FRMCTR1  = $B1; {8.2.51. FRMCTR1: Frame Rate Control (In Normal Mode/Full Colors)}

 ILI9486_CMD_DISCTRL  = $B6; {8.2.56. DISCTRL: Display Function Control}

 ILI9486_CMD_PWCTRL1  = $C0; {8.2.58. PWCTRL1: Power Control 1}
 ILI9486_CMD_PWCTRL2  = $C1; {8.2.59. PWCTRL2: Power Control 2}
 ILI9486_CMD_PWCTRL3  = $C2; {8.2.60. PWCTRL3: Power Control 3 (For Normal Mode)}

 ILI9486_CMD_VMCTRL1  = $C5; {8.2.63. VMCTRL1: VCOM Control 1}

 ILI9486_CMD_PGAMCTRL = $E0; {8.2.77. PGAMCTRL: Positive Gamma Correction (Set the gray scale voltage to adjust the gamma characteristics of the TFT panel)}
 ILI9486_CMD_NGAMCTRL = $E1; {8.2.78. NGAMCTRL: Negative Gamma Correction (Set the gray scale voltage to adjust the gamma characteristics of the TFT panel)}
 ILI9486_CMD_DGAMCTRL = $E2; {8.2.79. DGAMCTRL: Digital Gamma Control 1 (Gamma Macro-adjustment registers for red gamma curve)}

 {ILI9486 Memory access control constants (See ILI9486 datasheet 8.2.28. Memory Access Control)}
 ILI9486_CMD_MADCTL_MY  = $80; {Row Address Order}
 ILI9486_CMD_MADCTL_MX  = $40; {Column Address Order}
 ILI9486_CMD_MADCTL_MV  = $20; {Row / Column Exchange}
 ILI9486_CMD_MADCTL_ML  = $10; {Vertical Refresh Order}
 ILI9486_CMD_MADCTL_RGB = $00; {Colour selector switch control(0=RGB colour filter panel, 1=BGR colour filter panel)}
 ILI9486_CMD_MADCTL_BGR = $08;
 ILI9486_CMD_MADCTL_MH  = $04; {Horizontal Refresh Order}

{==============================================================================}
type
 {ILI9486 specific types}
 PILI9486Framebuffer = ^TILI9486Framebuffer;
 TILI9486Framebuffer = record
  {TFT Properties}
  TFT:TTFTFramebuffer;
  {ILI9486 Properties}
 end;

{==============================================================================}
{var}
 {ILI9486 specific variables}

{==============================================================================}
{Initialization Functions}

{==============================================================================}
{ILI9486 Functions}
function ILI9486FramebufferCreate(SPI:PSPIDevice;ChipSelect:Word;const Name:String;Rotation,Width,Height:LongWord;RST,DC,BL:PGPIOInfo):PFramebufferDevice;{$IFDEF API_EXPORT_ILI9486} stdcall; public name 'ili9486_framebuffer_create';{$ENDIF}

function ILI9486FramebufferDestroy(Framebuffer:PFramebufferDevice):LongWord;{$IFDEF API_EXPORT_ILI9486} stdcall; public name 'ili9486_framebuffer_destroy';{$ENDIF}

{==============================================================================}
{ILI9486 Framebuffer Functions}
function ILI9486FramebufferBlank(Framebuffer:PFramebufferDevice;Blank:Boolean):LongWord;

function ILI9486FramebufferSetBacklight(Framebuffer:PFramebufferDevice;Brightness:LongWord):LongWord;

{==============================================================================}
{ILI9486 TFTFramebuffer Functions}
function ILI9486TFTFramebufferInitialize(Framebuffer:PTFTFramebuffer;Defaults:PFramebufferProperties):LongWord;
function ILI9486TFTFramebufferDeinitialize(Framebuffer:PTFTFramebuffer):LongWord;

function ILI9486TFTFramebufferGetDefaults(Framebuffer:PTFTFramebuffer;Properties,Defaults:PFramebufferProperties):LongWord;

function ILI9486TFTFramebufferSetWriteAddress(Framebuffer:PTFTFramebuffer;X1,Y1,X2,Y2:LongWord):LongWord;

function ILI9486TFTFramebufferWriteMemory(Framebuffer:PTFTFramebuffer;Address:PtrUInt;Size:LongWord):LongWord;

{==============================================================================}
{ILI9486 Helper Functions}
function ILI9486WriteCommand(Framebuffer:PTFTFramebuffer;Value:Byte):LongWord;

function ILI9486WriteData(Framebuffer:PTFTFramebuffer;Value:Byte):LongWord;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
{var}
 {ILI9486 specific variables}

{==============================================================================}
{==============================================================================}
{Initialization Functions}

{==============================================================================}
{==============================================================================}
{ILI9486 Functions}
function ILI9486FramebufferCreate(SPI:PSPIDevice;ChipSelect:Word;const Name:String;Rotation,Width,Height:LongWord;RST,DC,BL:PGPIOInfo):PFramebufferDevice;{$IFDEF API_EXPORT_ILI9486} stdcall;{$ENDIF}
{Create, register and allocate a new ILI9486 Framebuffer device which can be accessed using the framebuffer API}
{SPI: The SPI device that this ILI9486 is connected to}
{ChipSelect: The SPI chip select to use when communicating with this device}
{Name: The text description of this device which will show in the device list (Optional)}
{Rotation: The rotation value for the framebuffer device (eg FRAMEBUFFER_ROTATION_180)}
{Width: The width of the framebuffer in pixels}
{Height: The height of the framebuffer in pixels}
{RST: GPIO pin information for the Reset pin (Optional)}
{DC: GPIO pin information for the Data/Command pin}
{BL: GPIO pin information for the Backlight pin (Optional)}
{Return: Pointer to the new Framebuffer device or nil if the framebuffer device could not be created}
var
 Status:LongWord;
 SPIProperties:TSPIProperties;
 ILI9486Framebuffer:PILI9486Framebuffer;
begin
 {}
 Result:=nil;

 {$IF DEFINED(ILI9486_DEBUG) or DEFINED(FRAMEBUFFER_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'ILI9486: Framebuffer Create (Name=' + Name + ' Width=' + IntToStr(Width) + ' Height=' + IntToStr(Height) + ')');
 {$ENDIF}

 {Check Rotation}
 if Rotation > FRAMEBUFFER_ROTATION_270 then Exit;

 {Check Width and Height}
 if Width < 1 then Exit;
 if Height < 1 then Exit;

 {Check SPI}
 if SPI = nil then Exit;

 {Check Chip Select}
 if ChipSelect = SPI_CS_NONE then Exit;

 {Check RST Pin}
 if RST <> nil then
  begin
   if (RST.GPIO <> nil) and (RST.Pin <> GPIO_PIN_UNKNOWN) then
    begin
     {Setup RST Pin}
     if GPIODeviceFunctionSelect(RST.GPIO,RST.Pin,GPIO_FUNCTION_OUT) <> ERROR_SUCCESS then Exit;
     if GPIODevicePullSelect(RST.GPIO,RST.Pin,GPIO_PULL_NONE) <> ERROR_SUCCESS then Exit;
    end;
  end;

 {Check DC Pin}
 if DC = nil then Exit;
 if DC.GPIO = nil then Exit;
 if DC.Pin = GPIO_PIN_UNKNOWN then Exit;

 {Setup DC Pin}
 if GPIODeviceFunctionSelect(DC.GPIO,DC.Pin,GPIO_FUNCTION_OUT) <> ERROR_SUCCESS then Exit;
 if GPIODevicePullSelect(DC.GPIO,DC.Pin,GPIO_PULL_NONE) <> ERROR_SUCCESS then Exit;

 {Check BL Pin}
 if BL <> nil then
  begin
   if (BL.GPIO <> nil) and (BL.Pin <> GPIO_PIN_UNKNOWN) then
    begin
     {Setup BL Pin}
     if GPIODeviceFunctionSelect(BL.GPIO,BL.Pin,GPIO_FUNCTION_OUT) <> ERROR_SUCCESS then Exit;
     if GPIODevicePullSelect(BL.GPIO,BL.Pin,GPIO_PULL_NONE) <> ERROR_SUCCESS then Exit;

     {Enable Backlight}
     GPIODeviceOutputSet(BL.GPIO,BL.Pin,GPIO_LEVEL_HIGH);
    end;
  end;

 {Create Framebuffer}
 ILI9486Framebuffer:=PILI9486Framebuffer(FramebufferDeviceCreateEx(SizeOf(TILI9486Framebuffer)));
 if ILI9486Framebuffer <> nil then
  begin
   {Update Framebuffer}
   {Device}
   ILI9486Framebuffer.TFT.Framebuffer.Device.DeviceBus:=DEVICE_BUS_SPI;
   ILI9486Framebuffer.TFT.Framebuffer.Device.DeviceType:=FRAMEBUFFER_TYPE_HARDWARE;
   ILI9486Framebuffer.TFT.Framebuffer.Device.DeviceFlags:=FRAMEBUFFER_FLAG_DMA or FRAMEBUFFER_FLAG_MARK or FRAMEBUFFER_FLAG_COMMIT or FRAMEBUFFER_FLAG_BLANK or FRAMEBUFFER_FLAG_CACHED or FRAMEBUFFER_FLAG_BACKLIGHT{$IFNDEF FPC_BIG_ENDIAN}or FRAMEBUFFER_FLAG_SWAP{$ENDIF FPC_BIG_ENDIAN};
   ILI9486Framebuffer.TFT.Framebuffer.Device.DeviceData:=nil;
   if Length(Name) <> 0 then ILI9486Framebuffer.TFT.Framebuffer.Device.DeviceDescription:=Name else ILI9486Framebuffer.TFT.Framebuffer.Device.DeviceDescription:=ILI9486_FRAMEBUFFER_DESCRIPTION;
   {Framebuffer}
   ILI9486Framebuffer.TFT.Framebuffer.FramebufferState:=FRAMEBUFFER_STATE_DISABLED;
   ILI9486Framebuffer.TFT.Framebuffer.DeviceAllocate:=TFTFramebufferAllocate;
   ILI9486Framebuffer.TFT.Framebuffer.DeviceRelease:=TFTFramebufferRelease;
   ILI9486Framebuffer.TFT.Framebuffer.DeviceBlank:=ILI9486FramebufferBlank;
   ILI9486Framebuffer.TFT.Framebuffer.DeviceMark:=TFTFramebufferMark;
   ILI9486Framebuffer.TFT.Framebuffer.DeviceCommit:=TFTFramebufferCommit;
   ILI9486Framebuffer.TFT.Framebuffer.DeviceSetBacklight:=ILI9486FramebufferSetBacklight;
   {TFT}
   ILI9486Framebuffer.TFT.SPI:=SPI;
   ILI9486Framebuffer.TFT.ChipSelect:=ChipSelect;
   if RST <> nil then ILI9486Framebuffer.TFT.RST:=RST^ else ILI9486Framebuffer.TFT.RST:=GPIO_INFO_UNKNOWN;
   ILI9486Framebuffer.TFT.DC:=DC^;
   if BL <> nil then ILI9486Framebuffer.TFT.BL:=BL^ else ILI9486Framebuffer.TFT.BL:=GPIO_INFO_UNKNOWN;
   ILI9486Framebuffer.TFT.Initialize:=ILI9486TFTFramebufferInitialize;
   ILI9486Framebuffer.TFT.Deinitialize:=ILI9486TFTFramebufferDeinitialize;
   ILI9486Framebuffer.TFT.GetDefaults:=ILI9486TFTFramebufferGetDefaults;
   ILI9486Framebuffer.TFT.SetWriteAddress:=ILI9486TFTFramebufferSetWriteAddress;
   ILI9486Framebuffer.TFT.WriteMemory:=ILI9486TFTFramebufferWriteMemory;
   {Driver}
   ILI9486Framebuffer.TFT.Width:=Width;
   ILI9486Framebuffer.TFT.Height:=Height;
   ILI9486Framebuffer.TFT.Rotation:=Rotation;
   if (Rotation = FRAMEBUFFER_ROTATION_0) or (Rotation = FRAMEBUFFER_ROTATION_180) then
    begin
     ILI9486Framebuffer.TFT.Width:=Height;
     ILI9486Framebuffer.TFT.Height:=Width;
    end;
   ILI9486Framebuffer.TFT.DirtyY1:=Height - 1;
   ILI9486Framebuffer.TFT.DirtyY2:=0;
   ILI9486Framebuffer.TFT.Ready:=True;
   ILI9486Framebuffer.TFT.Lock:=INVALID_HANDLE_VALUE;
   ILI9486Framebuffer.TFT.Timer:=INVALID_HANDLE_VALUE;
   ILI9486Framebuffer.TFT.FrameRate:=TFT_FRAMEBUFFER_FRAME_RATE_DEFAULT;
   ILI9486Framebuffer.TFT.TransferSize:=LongWord(-1);
   if SPIDeviceGetProperties(SPI,@SPIProperties) = ERROR_SUCCESS then
    begin
     if SPIProperties.MaxSize <> 0 then ILI9486Framebuffer.TFT.TransferSize:=SPIProperties.MaxSize;
    end;

   {Always send in multiples of 16 bits}
   if (ILI9486Framebuffer.TFT.TransferSize and 1) <> 0 then
     ILI9486Framebuffer.TFT.TransferSize := ILI9486Framebuffer.TFT.TransferSize - 1;

   {Setup Flags}
   {Nothing}

   {Register Framebuffer}
   Status:=FramebufferDeviceRegister(@ILI9486Framebuffer.TFT.Framebuffer);
   if Status = ERROR_SUCCESS then
    begin
     {Allocate Framebuffer}
     Status:=FramebufferDeviceAllocate(@ILI9486Framebuffer.TFT.Framebuffer,nil);
     if Status = ERROR_SUCCESS then
      begin
       {Return Result}
       Result:=PFramebufferDevice(ILI9486Framebuffer);
      end
     else
      begin
       if DEVICE_LOG_ENABLED then DeviceLogError(nil,'ILI9486: Failed to allocate new framebuffer device: ' + ErrorToString(Status));

       {Deregister Framebuffer}
       FramebufferDeviceDeregister(@ILI9486Framebuffer.TFT.Framebuffer);

       {Destroy Framebuffer}
       FramebufferDeviceDestroy(@ILI9486Framebuffer.TFT.Framebuffer);
      end;
    end
   else
    begin
     if DEVICE_LOG_ENABLED then DeviceLogError(nil,'ILI9486: Failed to register new framebuffer device: ' + ErrorToString(Status));

     {Destroy Framebuffer}
     FramebufferDeviceDestroy(@ILI9486Framebuffer.TFT.Framebuffer);
    end;
  end
 else
  begin
   if DEVICE_LOG_ENABLED then DeviceLogError(nil,'ILI9486: Failed to create new framebuffer device');
  end;
end;

{==============================================================================}

function ILI9486FramebufferDestroy(Framebuffer:PFramebufferDevice):LongWord;{$IFDEF API_EXPORT_ILI9486} stdcall;{$ENDIF}
{Release, deregister and destroy an ILI9486 Framebuffer device created by this driver}
{Framebuffer: The Framebuffer device to destroy}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Framebuffer}
 if Framebuffer = nil then Exit;

 {$IF DEFINED(ILI9486_DEBUG) or DEFINED(FRAMEBUFFER_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'ILI9486: Framebuffer Destroy');
 {$ENDIF}

 {Release Framebuffer}
 Result:=FramebufferDeviceRelease(Framebuffer);
 if Result = ERROR_SUCCESS then
  begin
   {Deregister Framebuffer}
   Result:=FramebufferDeviceDeregister(Framebuffer);
   if Result = ERROR_SUCCESS then
    begin
     {Destroy Framebuffer}
     Result:=FramebufferDeviceDestroy(Framebuffer);
     if Result <> ERROR_SUCCESS then
      begin
       if DEVICE_LOG_ENABLED then DeviceLogError(nil,'ILI9486: Failed to destroy framebuffer device: ' + ErrorToString(Result));
      end;
    end
   else
    begin
     if DEVICE_LOG_ENABLED then DeviceLogError(nil,'ILI9486: Failed to deregister framebuffer device: ' + ErrorToString(Result));
    end;
  end
 else
  begin
   if DEVICE_LOG_ENABLED then DeviceLogError(nil,'ILI9486: Failed to release framebuffer device: ' + ErrorToString(Result));
  end;
end;

{==============================================================================}
{==============================================================================}
{ILI9486 Framebuffer Functions}
function ILI9486FramebufferBlank(Framebuffer:PFramebufferDevice;Blank:Boolean):LongWord;
{Implementation of FramebufferDeviceBlank API for ILI9486}
{Note: Not intended to be called directly by applications, use FramebufferDeviceBlank instead}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IF DEFINED(ILI9486_DEBUG) or DEFINED(FRAMEBUFFER_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'ILI9486: Framebuffer Blank (Blank=' + BooleanToString(Blank) + ')');
 {$ENDIF}

 {Check Blank}
 if Blank then
  begin
   {Display Off}
   Result:=ILI9486WriteCommand(PTFTFramebuffer(Framebuffer),ILI9486_CMD_DISPOFF);
  end
 else
  begin
   {Display On}
   Result:=ILI9486WriteCommand(PTFTFramebuffer(Framebuffer),ILI9486_CMD_DISPON);
  end;
end;

{==============================================================================}

function ILI9486FramebufferSetBacklight(Framebuffer:PFramebufferDevice;Brightness:LongWord):LongWord;
{Implementation of FramebufferDeviceSetBacklight API for ILI9486}
{Note: Not intended to be called directly by applications, use FramebufferDeviceSetBacklight instead}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IF DEFINED(ILI9486_DEBUG) or DEFINED(FRAMEBUFFER_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'ILI9486: Framebuffer Set Backlight (Brightness=' + IntToStr(Brightness) + ')');
 {$ENDIF}

 {Check Backlight}
 if (PTFTFramebuffer(Framebuffer).BL.GPIO <> nil) and (PTFTFramebuffer(Framebuffer).BL.Pin <> GPIO_PIN_UNKNOWN) then
  begin
   {Check Brightness}
   if Brightness = 0 then
    begin
     {Disable Backlight}
     Result:=GPIODeviceOutputSet(PTFTFramebuffer(Framebuffer).BL.GPIO,PTFTFramebuffer(Framebuffer).BL.Pin,GPIO_LEVEL_LOW);
    end
   else
    begin
     {Enable Backlight}
     Result:=GPIODeviceOutputSet(PTFTFramebuffer(Framebuffer).BL.GPIO,PTFTFramebuffer(Framebuffer).BL.Pin,GPIO_LEVEL_HIGH);
    end;
  end
 else
  begin
   Result:=ERROR_NOT_SUPPORTED;
  end;
end;

{==============================================================================}
{==============================================================================}
{ILI9486 TFTFramebuffer Functions}
function ILI9486TFTFramebufferInitialize(Framebuffer:PTFTFramebuffer;Defaults:PFramebufferProperties):LongWord;
{Implementation of TFTFramebufferInitialize API for ILI9486}
{Note: Not intended to be called directly by applications}
var
 Value:Byte;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Framebuffer}
 if Framebuffer = nil then Exit;

 {$IF DEFINED(ILI9486_DEBUG) or DEFINED(FRAMEBUFFER_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'ILI9486: Framebuffer Initialize');
 {$ENDIF}

 {Check Defaults}
 if Defaults = nil then Exit;

 {Set the SPI clock rate}
 if SPIDeviceSetClockRate(Framebuffer.SPI,Framebuffer.ChipSelect,ILI9486_SPI_RATE) <> ERROR_SUCCESS then
  begin
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;

 {Start the SPI device}
 if SPIDeviceStart(Framebuffer.SPI,SPI_MODE_4WIRE,ILI9486_SPI_RATE,SPI_CLOCK_PHASE_LOW,SPI_CLOCK_POLARITY_LOW) <> ERROR_SUCCESS then
  begin
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;

 {Check Reset}
 if (Framebuffer.RST.GPIO <> nil) and (Framebuffer.RST.Pin <> GPIO_PIN_UNKNOWN) then
  begin
   {Set High}
   GPIODeviceOutputSet(Framebuffer.RST.GPIO,Framebuffer.RST.Pin,GPIO_LEVEL_HIGH);

   {Sleep 60ms}
   Sleep(60);

   {Set Low}
   GPIODeviceOutputSet(Framebuffer.RST.GPIO,Framebuffer.RST.Pin,GPIO_LEVEL_LOW);

   {Sleep 80ms}
   Sleep(80);

   {Set High}
   GPIODeviceOutputSet(Framebuffer.RST.GPIO,Framebuffer.RST.Pin,GPIO_LEVEL_HIGH);

   {Sleep 120ms}
   Sleep(120);
  end;

 {Initialize Device}
 {Interface Mode Control}
 ILI9486WriteCommand(Framebuffer,ILI9486_CMD_IFMODE);
 ILI9486WriteData(Framebuffer,$00);

 {Sleep OUT}
 ILI9486WriteCommand(Framebuffer,ILI9486_CMD_SLPOUT);
 Sleep(120);

 {Interface Pixel Format}
 ILI9486WriteCommand(Framebuffer,ILI9486_CMD_COLMOD);
 ILI9486WriteData(Framebuffer,$55);

 {Power Control 3}
 ILI9486WriteCommand(Framebuffer,ILI9486_CMD_PWCTRL3);
 ILI9486WriteData(Framebuffer,$44);

 {VCOM Control 1}
 ILI9486WriteCommand(Framebuffer,ILI9486_CMD_VMCTRL1);
 ILI9486WriteData(Framebuffer,$00);
 ILI9486WriteData(Framebuffer,$00);
 ILI9486WriteData(Framebuffer,$00);
 ILI9486WriteData(Framebuffer,$00);

 {PGAMCTRL(Positive Gamma Control)}
 ILI9486WriteCommand(Framebuffer,ILI9486_CMD_PGAMCTRL);
 ILI9486WriteData(Framebuffer,$0F);
 ILI9486WriteData(Framebuffer,$1F);
 ILI9486WriteData(Framebuffer,$1C);
 ILI9486WriteData(Framebuffer,$0C);
 ILI9486WriteData(Framebuffer,$0F);
 ILI9486WriteData(Framebuffer,$08);
 ILI9486WriteData(Framebuffer,$48);
 ILI9486WriteData(Framebuffer,$98);
 ILI9486WriteData(Framebuffer,$37);
 ILI9486WriteData(Framebuffer,$0A);
 ILI9486WriteData(Framebuffer,$13);
 ILI9486WriteData(Framebuffer,$04);
 ILI9486WriteData(Framebuffer,$11);
 ILI9486WriteData(Framebuffer,$0D);
 ILI9486WriteData(Framebuffer,$00);

 {NGAMCTRL(Negative Gamma Control)}
 ILI9486WriteCommand(Framebuffer,ILI9486_CMD_NGAMCTRL);
 ILI9486WriteData(Framebuffer,$0F);
 ILI9486WriteData(Framebuffer,$32);
 ILI9486WriteData(Framebuffer,$2E);
 ILI9486WriteData(Framebuffer,$0B);
 ILI9486WriteData(Framebuffer,$0D);
 ILI9486WriteData(Framebuffer,$05);
 ILI9486WriteData(Framebuffer,$47);
 ILI9486WriteData(Framebuffer,$75);
 ILI9486WriteData(Framebuffer,$37);
 ILI9486WriteData(Framebuffer,$06);
 ILI9486WriteData(Framebuffer,$10);
 ILI9486WriteData(Framebuffer,$03);
 ILI9486WriteData(Framebuffer,$24);
 ILI9486WriteData(Framebuffer,$20);
 ILI9486WriteData(Framebuffer,$00);

 {Digital Gamma Control 1}
 ILI9486WriteCommand(Framebuffer,ILI9486_CMD_DGAMCTRL);
 ILI9486WriteData(Framebuffer,$0F);
 ILI9486WriteData(Framebuffer,$32);
 ILI9486WriteData(Framebuffer,$2E);
 ILI9486WriteData(Framebuffer,$0B);
 ILI9486WriteData(Framebuffer,$0D);
 ILI9486WriteData(Framebuffer,$05);
 ILI9486WriteData(Framebuffer,$47);
 ILI9486WriteData(Framebuffer,$75);
 ILI9486WriteData(Framebuffer,$37);
 ILI9486WriteData(Framebuffer,$06);
 ILI9486WriteData(Framebuffer,$10);
 ILI9486WriteData(Framebuffer,$03);
 ILI9486WriteData(Framebuffer,$24);
 ILI9486WriteData(Framebuffer,$20);
 ILI9486WriteData(Framebuffer,$00);

 {Memory Access Control}
 ILI9486WriteCommand(Framebuffer,ILI9486_CMD_MADCTL);
 Value:=0;
 {Order}
 case Defaults.Order of
  FRAMEBUFFER_ORDER_RGB: Value:=Value or ILI9486_CMD_MADCTL_BGR; {Reversed}
  FRAMEBUFFER_ORDER_BGR: Value:=Value or ILI9486_CMD_MADCTL_RGB; {Reversed}
 else
  Value:=Value or ILI9486_CMD_MADCTL_BGR;
 end;
 {Rotation}
 case Defaults.Rotation of
  FRAMEBUFFER_ROTATION_0:   Value:=Value or ILI9486_CMD_MADCTL_MY or ILI9486_CMD_MADCTL_MX or ILI9486_CMD_MADCTL_MV;
  FRAMEBUFFER_ROTATION_90:  Value:=Value or ILI9486_CMD_MADCTL_MX;
  FRAMEBUFFER_ROTATION_180: Value:=Value or ILI9486_CMD_MADCTL_MV;
  FRAMEBUFFER_ROTATION_270: Value:=Value or ILI9486_CMD_MADCTL_MY;
 else
  Value:=Value or ILI9486_CMD_MADCTL_MY or ILI9486_CMD_MADCTL_MX or ILI9486_CMD_MADCTL_MV;
 end;
 ILI9486WriteData(Framebuffer,Value);

 {Sleep OUT}
 ILI9486WriteCommand(Framebuffer,ILI9486_CMD_SLPOUT);

 {Display On}
 ILI9486WriteCommand(Framebuffer,ILI9486_CMD_DISPON);
 Sleep(150);

 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function ILI9486TFTFramebufferDeinitialize(Framebuffer:PTFTFramebuffer):LongWord;
{Implementation of TFTFramebufferDeinitialize API for ILI9486}
{Note: Not intended to be called directly by applications}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Framebuffer}
 if Framebuffer = nil then Exit;

 {$IF DEFINED(ILI9486_DEBUG) or DEFINED(FRAMEBUFFER_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'ILI9486: Framebuffer Initialize');
 {$ENDIF}

 {Display Off}
 ILI9486WriteCommand(Framebuffer,ILI9486_CMD_DISPOFF);

 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function ILI9486TFTFramebufferGetDefaults(Framebuffer:PTFTFramebuffer;Properties,Defaults:PFramebufferProperties):LongWord;
{Implementation of TFTFramebufferGetDefaults API for ILI9486}
{Note: Not intended to be called directly by applications}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Framebuffer}
 if Framebuffer = nil then Exit;

 {$IF DEFINED(ILI9486_DEBUG) or DEFINED(FRAMEBUFFER_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'ILI9486: Framebuffer Get Defaults');
 {$ENDIF}

 {Check Defaults}
 if Defaults = nil then Exit;

 {Get Defaults}
 Defaults.Depth:=FRAMEBUFFER_DEPTH_16;
 Defaults.Order:=FRAMEBUFFER_ORDER_RGB;
 Defaults.Mode:=FRAMEBUFFER_MODE_IGNORED;
 Defaults.PhysicalWidth:=Framebuffer.Width;
 Defaults.PhysicalHeight:=Framebuffer.Height;
 Defaults.VirtualWidth:=Defaults.PhysicalWidth;
 Defaults.VirtualHeight:=Defaults.PhysicalHeight;
 Defaults.OffsetX:=0;
 Defaults.OffsetY:=0;
 Defaults.OverscanTop:=0;
 Defaults.OverscanBottom:=0;
 Defaults.OverscanLeft:=0;
 Defaults.OverscanRight:=0;
 Defaults.Rotation:=Framebuffer.Rotation;

 {Check Properties}
 if Properties <> nil then
  begin
   {Adjust Depth}
   if Properties.Depth = FRAMEBUFFER_DEPTH_16 then Defaults.Depth:=Properties.Depth;
   {Adjust Order}
   if Properties.Order <= FRAMEBUFFER_ORDER_RGB then Defaults.Order:=Properties.Order;
   {Adjust Rotation}
   if Properties.Rotation <= FRAMEBUFFER_ROTATION_270 then Defaults.Rotation:=Properties.Rotation;
   {Check Rotation}
   if Properties.Rotation <> Framebuffer.Rotation then
    begin
     if (Properties.Rotation = FRAMEBUFFER_ROTATION_90) or (Properties.Rotation = FRAMEBUFFER_ROTATION_270) then
      begin
       if (Framebuffer.Rotation <> FRAMEBUFFER_ROTATION_90) and (Framebuffer.Rotation <> FRAMEBUFFER_ROTATION_270) then
        begin
         Defaults.PhysicalWidth:=Framebuffer.Height;
         Defaults.PhysicalHeight:=Framebuffer.Width;
        end;
      end
     else
      begin
       if (Framebuffer.Rotation <> FRAMEBUFFER_ROTATION_0) and (Framebuffer.Rotation <> FRAMEBUFFER_ROTATION_180) then
        begin
         Defaults.PhysicalWidth:=Framebuffer.Height;
         Defaults.PhysicalHeight:=Framebuffer.Width;
        end;
      end;

     Defaults.VirtualWidth:=Defaults.PhysicalWidth;
     Defaults.VirtualHeight:=Defaults.PhysicalHeight;
    end;
  end;

 {Get Format}
 case Defaults.Depth of
  FRAMEBUFFER_DEPTH_16:begin
    if Defaults.Order = FRAMEBUFFER_ORDER_RGB then
     begin
      Defaults.Format:=COLOR_FORMAT_RGB16
     end
    else
     begin
      Defaults.Format:=COLOR_FORMAT_BGR16
     end;
   end;
 end;

 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function ILI9486TFTFramebufferSetWriteAddress(Framebuffer:PTFTFramebuffer;X1,Y1,X2,Y2:LongWord):LongWord;
{Implementation of TFTFramebufferSetWriteAddress API for ILI9486}
{Note: Not intended to be called directly by applications}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Framebuffer}
 if Framebuffer = nil then Exit;

 {$IF DEFINED(ILI9486_DEBUG) or DEFINED(FRAMEBUFFER_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'ILI9486: Set Write Address (X1=' + IntToStr(X1) + ' Y1=' + IntToStr(Y1) + ' X2=' + IntToStr(X2) + ' Y2=' + IntToStr(Y2) + ')');
 {$ENDIF}

 {Column address set}
 ILI9486WriteCommand(Framebuffer,ILI9486_CMD_CASET);
 ILI9486WriteData(Framebuffer,X1 shr 8);
 ILI9486WriteData(Framebuffer,X1 and $FF);
 ILI9486WriteData(Framebuffer,X2 shr 8);
 ILI9486WriteData(Framebuffer,X2 and $FF);

 {Row address set}
 ILI9486WriteCommand(Framebuffer,ILI9486_CMD_PASET);
 ILI9486WriteData(Framebuffer,Y1 shr 8);
 ILI9486WriteData(Framebuffer,Y1 and $FF);
 ILI9486WriteData(Framebuffer,Y2 shr 8);
 ILI9486WriteData(Framebuffer,Y2 and $FF);

 {Memory write}
 ILI9486WriteCommand(Framebuffer,ILI9486_CMD_RAMWR);

 {Set DC High}
 GPIODeviceOutputSet(Framebuffer.DC.GPIO,Framebuffer.DC.Pin,GPIO_LEVEL_HIGH);

 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function ILI9486TFTFramebufferWriteMemory(Framebuffer:PTFTFramebuffer;Address:PtrUInt;Size:LongWord):LongWord;
{Implementation of TFTFramebufferWriteMemory API for ILI9486}
{Note: Not intended to be called directly by applications}
var
 Count:LongWord;
 Remain:LongWord;
 Offset:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Framebuffer}
 if Framebuffer = nil then Exit;

 {$IF DEFINED(ILI9486_DEBUG) or DEFINED(FRAMEBUFFER_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'ILI9486: Write Memory (Address=' + AddrToHex(Address) + ' Size=' + IntToStr(Size) + ')');
 {$ENDIF}

 {Check Size}
 if Size < 1 then Exit;

 Count:=0;
 Offset:=0;
 Remain:=Size;
 while Remain > 0 do
  begin
   if Remain > Framebuffer.TransferSize then
    begin
     {Write Memory}
     Result:=SPIDeviceWrite(Framebuffer.SPI,Framebuffer.ChipSelect,Pointer(Address + Offset),Framebuffer.TransferSize,SPI_TRANSFER_DMA,Count);
     if Result <> ERROR_SUCCESS then Exit;

     {Update Remain}
     Dec(Remain,Framebuffer.TransferSize);

     {Update Offset}
     Inc(Offset,Framebuffer.TransferSize);
    end
   else
    begin
     {Write Memory}
     Result:=SPIDeviceWrite(Framebuffer.SPI,Framebuffer.ChipSelect,Pointer(Address + Offset),Remain,SPI_TRANSFER_DMA,Count);
     if Result <> ERROR_SUCCESS then Exit;

     {Update Remain}
     Dec(Remain,Remain);

     {Update Offset}
     Inc(Offset,Remain);
    end;
  end;

 Result:=ERROR_SUCCESS;
end;

{==============================================================================}
{==============================================================================}
{ILI9486 Helper Functions}
function ILI9486WriteCommand(Framebuffer:PTFTFramebuffer;Value:Byte):LongWord;
{Write a single command value to the ILI9486}
var
 Count:LongWord;
 Data:array[0..1] of Byte;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Framebuffer}
 if Framebuffer = nil then Exit;

 Count:=0;

 {$IF DEFINED(ILI9486_DEBUG) or DEFINED(FRAMEBUFFER_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'ILI9486: Write Command (Value=' + IntToHex(Value,2) + ')');
 {$ENDIF}

 {Set DC Low}
 GPIODeviceOutputSet(Framebuffer.DC.GPIO,Framebuffer.DC.Pin,GPIO_LEVEL_LOW);

 {Always send in multiple of 16 bits}
 Data[0]:=0;
 Data[1]:=Value;

 {Write Command}
 Result:=SPIDeviceWrite(Framebuffer.SPI,Framebuffer.ChipSelect,@Data,2,SPI_TRANSFER_NONE,Count);
end;

{==============================================================================}

function ILI9486WriteData(Framebuffer:PTFTFramebuffer;Value:Byte):LongWord;
{Write a single data value to the ILI9486}
var
 Count:LongWord;
 Data:array[0..1] of Byte;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Framebuffer}
 if Framebuffer = nil then Exit;

 Count:=0;

 {$IF DEFINED(ILI9486_DEBUG) or DEFINED(FRAMEBUFFER_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'ILI9486: Write Data (Value=' + IntToHex(Value,2) + ')');
 {$ENDIF}

 {Set DC High}
 GPIODeviceOutputSet(Framebuffer.DC.GPIO,Framebuffer.DC.Pin,GPIO_LEVEL_HIGH);

 {Always send in multiple of 16 bits}
 Data[0]:=0;
 Data[1]:=Value;

 {Write Data}
 Result:=SPIDeviceWrite(Framebuffer.SPI,Framebuffer.ChipSelect,@Data,2,SPI_TRANSFER_NONE,Count);
end;

{==============================================================================}

{initialization}
 {Nothing}

{==============================================================================}

{finalization}
 {Nothing}

{==============================================================================}
{==============================================================================}

end.

