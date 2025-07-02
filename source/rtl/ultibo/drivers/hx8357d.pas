{
Himax HX8357D TFT LCD Driver.

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

  Linux - \drivers\staging\fbtft\fb_hx8357d.c - Copyright (C) 2015 Adafruit Industries
  Linux - \drivers\staging\fbtft\fbtft_device.c - Copyright (C) 2013 Noralf Tronnes
  Linux - \drivers\staging\fbtft\fbtft-core.c - Copyright (C) 2013 Noralf Tronnes
 
References
==========
 
 HX8357D - https://cdn-shop.adafruit.com/datasheets/HX8357-D_DS_April2012.pdf
  
Himax HX8357D
=============
  
 The Himax HX8357D is a 320 x 480 RGB, 16M color TFT LCD Single Chip Driver that
 supports color depths of 16, 18 or 24bit. This driver supports the chip in both
 16 and 24 bit depths using RGB888 or RGB565 formats.
 
 The chip provides an SPI interface at up to 32MHz and supports rotations of
 0, 90, 180 and 270 degrees.
 
}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit HX8357D; 

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,GPIO,SPI,Framebuffer,TFTFramebuffer,SysUtils;

{==============================================================================}
{Global definitions}
{$INCLUDE ..\core\GlobalDefines.inc}

{==============================================================================}
const
 {HX8357D specific constants}
 HX8357D_FRAMEBUFFER_DESCRIPTION = 'Himax HX8357D TFT LCD';  {Description of HX8357D device}

 {HX8357D SPI constants}
 HX8357D_SPI_RATE = 32000000; {Default SPI clock rate}
 
 {HX8357D Command constants (See HX8357-D datasheet)}
 HX8357D_CMD_NOP      = $00; {6.2.1 NOP: No Operation}
 HX8357D_CMD_SWRESET  = $01; {6.2.2 SWRESET: Software reset}
 
 HX8357D_CMD_RDDIDIF  = $04; {6.2.3 Read display identification information}
 
 HX8357D_CMD_SLPIN    = $10; {6.2.14 SLPIN: Sleep in (Enter the minimum power consumption mode) (Wait 120ms)}
 HX8357D_CMD_SLPOUT   = $11; {6.2.15 SLPOUT: Sleep out (Wait 5ms)}
 
 HX8357D_CMD_PTLON    = $12; {6.2.16 PTLON: Partial mode on (This command turns on partial mode. The partial mode window is described by the Partial Area command (30H))}
 HX8357D_CMD_NORON    = $13; {6.2.17 NORON: Normal display mode on (This command returns the display to normal mode)}
 
 HX8357D_CMD_INVOFF   = $20; {6.2.18 INVOFF: Display inversion off (This command is used to recover from display inversion mode)}
 HX8357D_CMD_INVON    = $21; {6.2.19 INVON: Display inversion on (This command is used to enter into display inversion mode)}
 
 HX8357D_CMD_ALLPOFF  = $22; {6.2.20 All pixel off (This command turns the display panel black in ‘Sleep Out’ mode)}
 HX8357D_CMD_ALLPON   = $23; {6.2.21 All pixel on (This command turns the display panel white in ‘Sleep Out‘ mode)}
 
 HX8357D_CMD_DISPOFF  = $28; {6.2.23 DISPOFF: Display off (This command is used to enter into DISPLAY OFF mode. In this mode, the output from Frame Memory is disabled and blank page inserted)}
 HX8357D_CMD_DISPON   = $29; {6.2.24 DISPON: Display on (This command is used to recover from DISPLAY OFF mode. Output from the Frame Memory is enabled)}
 
 HX8357D_CMD_CASET    = $2A; {6.2.25 CASET: Column address set (This command is used to define area of frame memory where MCU can access)}
 HX8357D_CMD_PASET    = $2B; {6.2.26 PASET: Page address set (This command is used to define area of frame memory where MCU can access)}
 HX8357D_CMD_RAMWR    = $2C; {6.2.27 RAMWR: Memory write (This command is used to transfer data from MCU to frame memory)}
 HX8357D_CMD_RAMRD    = $2E; {6.2.28 RAMRD: Memory read (This command is used to transfer data from frame memory to MCU)}
 
 HX8357D_CMD_TEON     = $35; {6.2.32 TEON: Tearing effect line on}
 
 HX8357D_CMD_MADCTL   = $36; {6.2.33 MADCTL: Memory access control}
 HX8357D_CMD_COLMOD   = $3A; {6.2.37 COLMOD: Interface pixel format}
 
 HX8357D_CMD_TESL     = $44; {6.2.40 TESL: Set tear scan line}
 
 HX8357D_CMD_SETOSC   = $B0; {6.2.73 SETOSC: set internal oscillator}
 HX8357D_CMD_SETPOWER = $B1; {6.2.74 SETPOWER: set power control}
 HX8357D_CMD_SETRGB   = $B3; {6.2.76 SETRGB: set RGB interface}
 HX8357D_CMD_SETCYC   = $B4; {6.2.77 SETCYC: set display cycle register}
 HX8357D_CMD_SETCOM   = $B6; {6.2.79 SETCOM: set VCOM voltage related register}
 
 HX8357D_CMD_SETEXTC  = $B9; {6.2.81 SETEXTC: enable extension command (Enable: FFh,83h,57h / Disable: xxh,xxh,xxh)}
 
 HX8357D_CMD_SETSTBA  = $C0; {6.2.82 SETSTBA: Set Source Option}
 
 HX8357D_CMD_SETPANEL = $CC; {6.2.87 SETPanel: set panel characteristic}
 
 HX8357D_CMD_SETGAMMA = $E0; {6.2.88 SETGamma: set gamma curve}
 
 {HX8357D Memory access control constants (See HX8357-D datasheet 6.2.33 Memory access control)}
 HX8357D_CMD_MADCTL_MY  = $80; {Page Address Order}
 HX8357D_CMD_MADCTL_MX  = $40; {Column Address Order}
 HX8357D_CMD_MADCTL_MV  = $20; {Page / Column Selection}
 HX8357D_CMD_MADCTL_ML  = $10; {Vertical Order}
 HX8357D_CMD_MADCTL_RGB = $00; {Colour selector switch control(0=RGB colour filter panel, 1=BGR colour filter panel)}
 HX8357D_CMD_MADCTL_BGR = $08; 
 HX8357D_CMD_MADCTL_SS  = $04; {Horizontal Order}
 HX8357D_CMD_MADCTL_MH  = HX8357D_CMD_MADCTL_SS;

{==============================================================================}
type
 {HX8357D specific types}
 PHX8357DFramebuffer = ^THX8357DFramebuffer;
 THX8357DFramebuffer = record
  {TFT Properties}
  TFT:TTFTFramebuffer;
  {HX8357D Properties}
 end; 
  
{==============================================================================}
{var}
 {HX8357D specific variables}
 
{==============================================================================}
{Initialization Functions}
 
{==============================================================================}
{HX8357D Functions}
function HX8357DFramebufferCreate(SPI:PSPIDevice;ChipSelect:Word;const Name:String;Rotation,Width,Height:LongWord;RST,DC,BL:PGPIOInfo):PFramebufferDevice;{$IFDEF API_EXPORT_HX8357D} stdcall; public name 'hx8357d_framebuffer_create';{$ENDIF}
  
function HX8357DFramebufferDestroy(Framebuffer:PFramebufferDevice):LongWord;{$IFDEF API_EXPORT_HX8357D} stdcall; public name 'hx8357d_framebuffer_destroy';{$ENDIF}

{==============================================================================}
{HX8357D Framebuffer Functions}
function HX8357DFramebufferBlank(Framebuffer:PFramebufferDevice;Blank:Boolean):LongWord;

function HX8357DFramebufferSetBacklight(Framebuffer:PFramebufferDevice;Brightness:LongWord):LongWord;
  
{==============================================================================}
{HX8357D TFTFramebuffer Functions}
function HX8357DTFTFramebufferInitialize(Framebuffer:PTFTFramebuffer;Defaults:PFramebufferProperties):LongWord;
function HX8357DTFTFramebufferDeinitialize(Framebuffer:PTFTFramebuffer):LongWord;

function HX8357DTFTFramebufferGetDefaults(Framebuffer:PTFTFramebuffer;Properties,Defaults:PFramebufferProperties):LongWord;

function HX8357DTFTFramebufferSetWriteAddress(Framebuffer:PTFTFramebuffer;X1,Y1,X2,Y2:LongWord):LongWord;

function HX8357DTFTFramebufferWriteMemory(Framebuffer:PTFTFramebuffer;Address:PtrUInt;Size:LongWord):LongWord;

{==============================================================================}
{HX8357D Helper Functions}
function HX8357DWriteCommand(Framebuffer:PTFTFramebuffer;Value:Byte):LongWord;
function HX8357DWriteCommandEx(Framebuffer:PTFTFramebuffer;const Values:array of Byte):LongWord;

function HX8357DWriteData(Framebuffer:PTFTFramebuffer;Value:Byte):LongWord;
function HX8357DWriteDataEx(Framebuffer:PTFTFramebuffer;const Values:array of Byte):LongWord;
 
{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
{var}
 {HX8357D specific variables}
 
{==============================================================================}
{==============================================================================}
{Initialization Functions}
 
{==============================================================================}
{==============================================================================}
{HX8357D Functions}
function HX8357DFramebufferCreate(SPI:PSPIDevice;ChipSelect:Word;const Name:String;Rotation,Width,Height:LongWord;RST,DC,BL:PGPIOInfo):PFramebufferDevice;{$IFDEF API_EXPORT_HX8357D} stdcall;{$ENDIF}
{Create, register and allocate a new HX8357D Framebuffer device which can be accessed using the framebuffer API}
{SPI: The SPI device that this HX8357D is connected to}
{ChipSelect: The SPI chip select to use when communicating with this device}
{Name: The text description of this device which will should in the device list (Optional)}
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
 HX8357DFramebuffer:PHX8357DFramebuffer;
begin
 {}
 Result:=nil;
 
 {$IF DEFINED(HX8357D_DEBUG) or DEFINED(FRAMEBUFFER_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'HX8357D: Framebuffer Create (Name=' + Name + ' Width=' + IntToStr(Width) + ' Height=' + IntToStr(Height) + ')');
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
 HX8357DFramebuffer:=PHX8357DFramebuffer(FramebufferDeviceCreateEx(SizeOf(THX8357DFramebuffer)));
 if HX8357DFramebuffer <> nil then
  begin
   {Update Framebuffer}
   {Device}
   HX8357DFramebuffer.TFT.Framebuffer.Device.DeviceBus:=DEVICE_BUS_SPI; 
   HX8357DFramebuffer.TFT.Framebuffer.Device.DeviceType:=FRAMEBUFFER_TYPE_HARDWARE;
   HX8357DFramebuffer.TFT.Framebuffer.Device.DeviceFlags:=FRAMEBUFFER_FLAG_DMA or FRAMEBUFFER_FLAG_MARK or FRAMEBUFFER_FLAG_COMMIT or FRAMEBUFFER_FLAG_BLANK or FRAMEBUFFER_FLAG_CACHED or FRAMEBUFFER_FLAG_BACKLIGHT{$IFNDEF FPC_BIG_ENDIAN}or FRAMEBUFFER_FLAG_SWAP{$ENDIF FPC_BIG_ENDIAN};
   HX8357DFramebuffer.TFT.Framebuffer.Device.DeviceData:=nil;
   if Length(Name) <> 0 then HX8357DFramebuffer.TFT.Framebuffer.Device.DeviceDescription:=Name else HX8357DFramebuffer.TFT.Framebuffer.Device.DeviceDescription:=HX8357D_FRAMEBUFFER_DESCRIPTION;
   {Framebuffer}
   HX8357DFramebuffer.TFT.Framebuffer.FramebufferState:=FRAMEBUFFER_STATE_DISABLED;
   HX8357DFramebuffer.TFT.Framebuffer.DeviceAllocate:=TFTFramebufferAllocate;
   HX8357DFramebuffer.TFT.Framebuffer.DeviceRelease:=TFTFramebufferRelease;
   HX8357DFramebuffer.TFT.Framebuffer.DeviceBlank:=HX8357DFramebufferBlank;
   HX8357DFramebuffer.TFT.Framebuffer.DeviceMark:=TFTFramebufferMark;
   HX8357DFramebuffer.TFT.Framebuffer.DeviceCommit:=TFTFramebufferCommit;
   HX8357DFramebuffer.TFT.Framebuffer.DeviceSetBacklight:=HX8357DFramebufferSetBacklight;
   {TFT}
   HX8357DFramebuffer.TFT.SPI:=SPI;
   HX8357DFramebuffer.TFT.ChipSelect:=ChipSelect;
   if RST <> nil then HX8357DFramebuffer.TFT.RST:=RST^ else HX8357DFramebuffer.TFT.RST:=GPIO_INFO_UNKNOWN;
   HX8357DFramebuffer.TFT.DC:=DC^;
   if BL <> nil then HX8357DFramebuffer.TFT.BL:=BL^ else HX8357DFramebuffer.TFT.BL:=GPIO_INFO_UNKNOWN;
   HX8357DFramebuffer.TFT.Initialize:=HX8357DTFTFramebufferInitialize;
   HX8357DFramebuffer.TFT.Deinitialize:=HX8357DTFTFramebufferDeinitialize;
   HX8357DFramebuffer.TFT.GetDefaults:=HX8357DTFTFramebufferGetDefaults;
   HX8357DFramebuffer.TFT.SetWriteAddress:=HX8357DTFTFramebufferSetWriteAddress;
   HX8357DFramebuffer.TFT.WriteMemory:=HX8357DTFTFramebufferWriteMemory;
   {Driver}
   HX8357DFramebuffer.TFT.Width:=Width;
   HX8357DFramebuffer.TFT.Height:=Height;
   HX8357DFramebuffer.TFT.Rotation:=Rotation;
   if (Rotation = FRAMEBUFFER_ROTATION_90) or (Rotation = FRAMEBUFFER_ROTATION_270) then
    begin
     HX8357DFramebuffer.TFT.Width:=Height;
     HX8357DFramebuffer.TFT.Height:=Width;
    end;
   HX8357DFramebuffer.TFT.DirtyY1:=Height - 1;
   HX8357DFramebuffer.TFT.DirtyY2:=0;
   HX8357DFramebuffer.TFT.Ready:=True;
   HX8357DFramebuffer.TFT.Lock:=INVALID_HANDLE_VALUE;
   HX8357DFramebuffer.TFT.Timer:=INVALID_HANDLE_VALUE;
   HX8357DFramebuffer.TFT.FrameRate:=TFT_FRAMEBUFFER_FRAME_RATE_DEFAULT;
   HX8357DFramebuffer.TFT.TransferSize:=LongWord(-1);
   if SPIDeviceGetProperties(SPI,@SPIProperties) = ERROR_SUCCESS then
    begin
     if SPIProperties.MaxSize <> 0 then HX8357DFramebuffer.TFT.TransferSize:=SPIProperties.MaxSize; 
    end; 
   
   {Setup Flags}
   {Nothing}
   
   {Register Framebuffer}
   Status:=FramebufferDeviceRegister(@HX8357DFramebuffer.TFT.Framebuffer);
   if Status = ERROR_SUCCESS then
    begin
     {Allocate Framebuffer}
     Status:=FramebufferDeviceAllocate(@HX8357DFramebuffer.TFT.Framebuffer,nil);
     if Status = ERROR_SUCCESS then
      begin
       {Return Result}
       Result:=PFramebufferDevice(HX8357DFramebuffer); 
      end
     else
      begin
       if DEVICE_LOG_ENABLED then DeviceLogError(nil,'HX8357D: Failed to allocate new framebuffer device: ' + ErrorToString(Status));

       {Deregister Framebuffer}
       FramebufferDeviceDeregister(@HX8357DFramebuffer.TFT.Framebuffer);

       {Destroy Framebuffer}
       FramebufferDeviceDestroy(@HX8357DFramebuffer.TFT.Framebuffer);
      end;
    end
   else
    begin     
     if DEVICE_LOG_ENABLED then DeviceLogError(nil,'HX8357D: Failed to register new framebuffer device: ' + ErrorToString(Status));

     {Destroy Framebuffer}
     FramebufferDeviceDestroy(@HX8357DFramebuffer.TFT.Framebuffer);
    end;
  end
 else
  begin
   if DEVICE_LOG_ENABLED then DeviceLogError(nil,'HX8357D: Failed to create new framebuffer device');
  end;
end;
 
{==============================================================================}
 
function HX8357DFramebufferDestroy(Framebuffer:PFramebufferDevice):LongWord;{$IFDEF API_EXPORT_HX8357D} stdcall;{$ENDIF}
{Release, deregister and destroy an HX8357D Framebuffer device created by this driver}
{Framebuffer: The Framebuffer device to destroy}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 
 {$IF DEFINED(HX8357D_DEBUG) or DEFINED(FRAMEBUFFER_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'HX8357D: Framebuffer Destroy');
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
       if DEVICE_LOG_ENABLED then DeviceLogError(nil,'HX8357D: Failed to destroy framebuffer device: ' + ErrorToString(Result));
      end;
    end
   else
    begin
     if DEVICE_LOG_ENABLED then DeviceLogError(nil,'HX8357D: Failed to deregister framebuffer device: ' + ErrorToString(Result));
    end;    
  end
 else
  begin
   if DEVICE_LOG_ENABLED then DeviceLogError(nil,'HX8357D: Failed to release framebuffer device: ' + ErrorToString(Result));
  end;  
end;
 
{==============================================================================}
{==============================================================================}
{HX8357D Framebuffer Functions}
function HX8357DFramebufferBlank(Framebuffer:PFramebufferDevice;Blank:Boolean):LongWord;
{Implementation of FramebufferDeviceBlank API for HX8357D}
{Note: Not intended to be called directly by applications, use FramebufferDeviceBlank instead}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IF DEFINED(HX8357D_DEBUG) or DEFINED(FRAMEBUFFER_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'HX8357D: Framebuffer Blank (Blank=' + BooleanToString(Blank) + ')');
 {$ENDIF}
 
 {Check Blank}
 if Blank then
  begin
   {Display Off}
   Result:=HX8357DWriteCommand(PTFTFramebuffer(Framebuffer),HX8357D_CMD_DISPOFF);
  end
 else
  begin
   {Display On}
   Result:=HX8357DWriteCommand(PTFTFramebuffer(Framebuffer),HX8357D_CMD_DISPON);
  end;  
end;

{==============================================================================}

function HX8357DFramebufferSetBacklight(Framebuffer:PFramebufferDevice;Brightness:LongWord):LongWord;
{Implementation of FramebufferDeviceSetBacklight API for HX8357D}
{Note: Not intended to be called directly by applications, use FramebufferDeviceSetBacklight instead}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IF DEFINED(HX8357D_DEBUG) or DEFINED(FRAMEBUFFER_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'HX8357D: Framebuffer Set Backlight (Brightness=' + IntToStr(Brightness) + ')');
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
{HX8357D TFTFramebuffer Functions}
function HX8357DTFTFramebufferInitialize(Framebuffer:PTFTFramebuffer;Defaults:PFramebufferProperties):LongWord;
{Implementation of TFTFramebufferInitialize API for HX8357D}
{Note: Not intended to be called directly by applications}
var
 Value:Byte;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 
 {$IF DEFINED(HX8357D_DEBUG) or DEFINED(FRAMEBUFFER_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'HX8357D: Framebuffer Initialize');
 {$ENDIF}
 
 {Check Defaults}
 if Defaults = nil then Exit;
 
 {Set the SPI clock rate}
 if SPIDeviceSetClockRate(Framebuffer.SPI,Framebuffer.ChipSelect,HX8357D_SPI_RATE) <> ERROR_SUCCESS then
  begin
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;
 
 {Start the SPI device}
 if SPIDeviceStart(Framebuffer.SPI,SPI_MODE_4WIRE,HX8357D_SPI_RATE,SPI_CLOCK_PHASE_LOW,SPI_CLOCK_POLARITY_LOW) <> ERROR_SUCCESS then
  begin
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;
  
 {Check Reset}
 if (Framebuffer.RST.GPIO <> nil) and (Framebuffer.RST.Pin <> GPIO_PIN_UNKNOWN) then
  begin
   {Set Low}
   GPIODeviceOutputSet(Framebuffer.RST.GPIO,Framebuffer.RST.Pin,GPIO_LEVEL_LOW);
   
   {Delay 20us}
   MicrosecondDelay(20);
   
   {Set High}
   GPIODeviceOutputSet(Framebuffer.RST.GPIO,Framebuffer.RST.Pin,GPIO_LEVEL_HIGH);
   
   {Sleep 120ms}
   Sleep(120);
  end;
 
 {Initialize Device}
 {Software Reset}
 HX8357DWriteCommand(Framebuffer,HX8357D_CMD_SWRESET);
 Sleep(5);
 
 {Enable Extended Command}
 HX8357DWriteCommand(Framebuffer,HX8357D_CMD_SETEXTC);
 HX8357DWriteDataEx(Framebuffer,[$FF, $83, $57]);
 {Sleep(150); Linux driver sleeps for 150ms}
 
 {Set RGB interface (Enable SDO)}
 HX8357DWriteCommand(Framebuffer,HX8357D_CMD_SETRGB);
 HX8357DWriteDataEx(Framebuffer,[$80, $00, $06, $06]);

 {Set VCOM voltage related register (-1.52V)}
 HX8357DWriteCommand(Framebuffer,HX8357D_CMD_SETCOM);
 HX8357DWriteData(Framebuffer,$25);

 {Set internal oscillator (Normal mode 70Hz, Idle mode 55Hz)}
 HX8357DWriteCommand(Framebuffer,HX8357D_CMD_SETOSC);
 HX8357DWriteData(Framebuffer,$68);

 {Set panel characteristic (BGR, Gate direction swapped)}
 HX8357DWriteCommand(Framebuffer,HX8357D_CMD_SETPANEL);
 HX8357DWriteData(Framebuffer,$05);

 {Set power control}
 HX8357DWriteCommand(Framebuffer,HX8357D_CMD_SETPOWER);
 HX8357DWriteDataEx(Framebuffer,[$00,   {Not deep standby}
                                 $15,   {BT}
                                 $1C,   {VSPR}
                                 $1C,   {VSNR}
                                 $83,   {AP}
                                 $AA]); {FS}

 {Set Source Option}
 HX8357DWriteCommand(Framebuffer,HX8357D_CMD_SETSTBA);
 HX8357DWriteDataEx(Framebuffer,[$50,   {OPON normal}
                                 $50,   {OPON idle}
                                 $01,   {STBA}
                                 $3C,   {STBA}
                                 $1E,   {STBA}
                                 $08]); {GEN}

 {Set display cycle register}
 HX8357DWriteCommand(Framebuffer,HX8357D_CMD_SETCYC);
 HX8357DWriteDataEx(Framebuffer,[$02,   {NW 0x02}
                                 $40,   {RTN}
                                 $00,   {DIV}
                                 $2A,   {DUM}
                                 $2A,   {DUM}
                                 $0D,   {GDON}
                                 $78]); {GDOFF}

 {Set gamma curve}
 HX8357DWriteCommand(Framebuffer,HX8357D_CMD_SETGAMMA);
 HX8357DWriteDataEx(Framebuffer,[$02, $0A, $11, $1D, $23, $35, $41, $4B, $4B, $42, $3A, $27, $1B, $08, $09, $03, $02,
                                 $0A, $11, $1D, $23, $35, $41, $4B, $4B, $42, $3A, $27, $1B, $08, $09, $03, $00, $01]);

 {Interface pixel format ($55 = 16 bit / $66 = 18 bit / $77 = 24 bit)}
 HX8357DWriteCommand(Framebuffer,HX8357D_CMD_COLMOD);
 if Defaults.Depth = FRAMEBUFFER_DEPTH_16 then
  begin
   {16 bit}
   Value:=$55;
  end
 else if Defaults.Depth = FRAMEBUFFER_DEPTH_24 then
  begin
   {24 bit}
   Value:=$77;
  end; 
 HX8357DWriteData(Framebuffer,Value);

 {Memory access control}
 HX8357DWriteCommand(Framebuffer,HX8357D_CMD_MADCTL);
 Value:=0;
 {RGB/BGR}
 if Defaults.Order = FRAMEBUFFER_ORDER_RGB then
  begin
   Value:=Value or HX8357D_CMD_MADCTL_RGB; 
  end
 else if Defaults.Order = FRAMEBUFFER_ORDER_BGR then
  begin
   Value:=Value or HX8357D_CMD_MADCTL_BGR;
  end;
 {Horizontal/Vertical memory direction (Rotation)}
 if Defaults.Rotation = FRAMEBUFFER_ROTATION_0 then
  begin
   Value:=Value or HX8357D_CMD_MADCTL_MX or HX8357D_CMD_MADCTL_MY;
  end
 else if Defaults.Rotation = FRAMEBUFFER_ROTATION_90 then
  begin
   Value:=Value or HX8357D_CMD_MADCTL_MV or HX8357D_CMD_MADCTL_MY;
  end
 else if Defaults.Rotation = FRAMEBUFFER_ROTATION_180 then 
  begin
   {Nothing}
  end
 else if Defaults.Rotation = FRAMEBUFFER_ROTATION_270 then
  begin
   Value:=Value or HX8357D_CMD_MADCTL_MV or HX8357D_CMD_MADCTL_MX;
  end;
 HX8357DWriteData(Framebuffer,Value); 

 {Tearing effect line on (Off)}
 HX8357DWriteCommand(Framebuffer,HX8357D_CMD_TEON);
 HX8357DWriteData(Framebuffer,$00);

 {Set tear scan line}
 HX8357DWriteCommand(Framebuffer,HX8357D_CMD_TESL);
 HX8357DWriteDataEx(Framebuffer,[$00, $02]);

 {Sleep Out}
 HX8357DWriteCommand(Framebuffer,HX8357D_CMD_SLPOUT);
 Sleep(5); {Linux driver sleeps for 150ms}

 {Display On}
 HX8357DWriteCommand(Framebuffer,HX8357D_CMD_DISPON);
 {Linux driver sleeps for 5ms}
 
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function HX8357DTFTFramebufferDeinitialize(Framebuffer:PTFTFramebuffer):LongWord;
{Implementation of TFTFramebufferDeinitialize API for HX8357D}
{Note: Not intended to be called directly by applications}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 
 {$IF DEFINED(HX8357D_DEBUG) or DEFINED(FRAMEBUFFER_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'HX8357D: Framebuffer Deinitialize');
 {$ENDIF}
 
 {Display Off}
 HX8357DWriteCommand(Framebuffer,HX8357D_CMD_DISPOFF);
 
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function HX8357DTFTFramebufferGetDefaults(Framebuffer:PTFTFramebuffer;Properties,Defaults:PFramebufferProperties):LongWord;
{Implementation of TFTFramebufferGetDefaults API for HX8357D}
{Note: Not intended to be called directly by applications}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 
 {$IF DEFINED(HX8357D_DEBUG) or DEFINED(FRAMEBUFFER_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'HX8357D: Framebuffer Get Defaults');
 {$ENDIF}
 
 {Check Defaults}
 if Defaults = nil then Exit;
 
 {Get Defaults}
 Defaults.Depth:=FRAMEBUFFER_DEPTH_24;
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
   if (Properties.Depth = FRAMEBUFFER_DEPTH_16) or (Properties.Depth = FRAMEBUFFER_DEPTH_24) then Defaults.Depth:=Properties.Depth;
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
      Defaults.Format:=COLOR_FORMAT_RGB16;
     end
    else
     begin
      Defaults.Format:=COLOR_FORMAT_BGR16;
     end;
   end;
  FRAMEBUFFER_DEPTH_24:begin
    if Defaults.Order = FRAMEBUFFER_ORDER_RGB then
     begin
      Defaults.Format:=COLOR_FORMAT_RGB24;
     end
    else
     begin
      Defaults.Format:=COLOR_FORMAT_BGR24;
     end;
   end;
 end;
 
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function HX8357DTFTFramebufferSetWriteAddress(Framebuffer:PTFTFramebuffer;X1,Y1,X2,Y2:LongWord):LongWord;
{Implementation of TFTFramebufferSetWriteAddress API for HX8357D}
{Note: Not intended to be called directly by applications}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;

 {$IF DEFINED(HX8357D_DEBUG) or DEFINED(FRAMEBUFFER_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'HX8357D: Set Write Address (X1=' + IntToStr(X1) + ' Y1=' + IntToStr(Y1) + ' X2=' + IntToStr(X2) + ' Y2=' + IntToStr(Y2) + ')');
 {$ENDIF}

 {Column address set}
 HX8357DWriteCommand(Framebuffer,HX8357D_CMD_CASET);
 HX8357DWriteDataEx(Framebuffer,[X1 shr 8,X1 and $FF,X2 shr 8,X2 and $FF]);

 {Row address set}
 HX8357DWriteCommand(Framebuffer,HX8357D_CMD_PASET);
 HX8357DWriteDataEx(Framebuffer,[Y1 shr 8,Y1 and $FF,Y2 shr 8,Y2 and $FF]);

 {Memory write}
 HX8357DWriteCommand(Framebuffer,HX8357D_CMD_RAMWR);

 {Set DC High (Ready for Write Memory)}
 GPIODeviceOutputSet(Framebuffer.DC.GPIO,Framebuffer.DC.Pin,GPIO_LEVEL_HIGH);
 
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function HX8357DTFTFramebufferWriteMemory(Framebuffer:PTFTFramebuffer;Address:PtrUInt;Size:LongWord):LongWord;
{Implementation of TFTFramebufferWriteMemory API for HX8357D}
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

 {$IF DEFINED(HX8357D_DEBUG) or DEFINED(FRAMEBUFFER_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'HX8357D: Write Memory (Address=' + AddrToHex(Address) + ' Size=' + IntToStr(Size) + ')');
 {$ENDIF}

 {Check Size}
 if Size < 1 then Exit;
 
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
{HX8357D Helper Functions}
function HX8357DWriteCommand(Framebuffer:PTFTFramebuffer;Value:Byte):LongWord;
{Write a single command value to the HX8357D}
var
 Count:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;

 {$IF DEFINED(HX8357D_DEBUG) or DEFINED(FRAMEBUFFER_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'HX8357D: Write Command (Value=' + IntToHex(Value,2) + ')');
 {$ENDIF}
 
 {Set DC Low}
 GPIODeviceOutputSet(Framebuffer.DC.GPIO,Framebuffer.DC.Pin,GPIO_LEVEL_LOW);
 
 {Write Command}
 Result:=SPIDeviceWrite(Framebuffer.SPI,Framebuffer.ChipSelect,@Value,SizeOf(Byte),SPI_TRANSFER_NONE,Count);
end;

{==============================================================================}

function HX8357DWriteCommandEx(Framebuffer:PTFTFramebuffer;const Values:array of Byte):LongWord;
{Write multiple command values to the HX8357D}
var
 Count:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;

 {$IF DEFINED(HX8357D_DEBUG) or DEFINED(FRAMEBUFFER_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'HX8357D: Write Command Ex (Values size=' + IntToStr(Length(Values)) + ')');
 {$ENDIF}
 
 {Set DC Low}
 GPIODeviceOutputSet(Framebuffer.DC.GPIO,Framebuffer.DC.Pin,GPIO_LEVEL_LOW);
 
 {Write Command}
 Result:=SPIDeviceWrite(Framebuffer.SPI,Framebuffer.ChipSelect,@Values[0],Length(Values),SPI_TRANSFER_NONE,Count);
end;

{==============================================================================}

function HX8357DWriteData(Framebuffer:PTFTFramebuffer;Value:Byte):LongWord; 
{Write a single data value to the HX8357D}
var
 Count:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;

 {$IF DEFINED(HX8357D_DEBUG) or DEFINED(FRAMEBUFFER_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'HX8357D: Write Data (Value=' + IntToHex(Value,2) + ')');
 {$ENDIF}
 
 {Set DC High}
 GPIODeviceOutputSet(Framebuffer.DC.GPIO,Framebuffer.DC.Pin,GPIO_LEVEL_HIGH);
 
 {Write Data}
 Result:=SPIDeviceWrite(Framebuffer.SPI,Framebuffer.ChipSelect,@Value,SizeOf(Byte),SPI_TRANSFER_NONE,Count);
end;

{==============================================================================}

function HX8357DWriteDataEx(Framebuffer:PTFTFramebuffer;const Values:array of Byte):LongWord;
{Write multiple data values to the HX8357D}
var
 Count:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;

 {$IF DEFINED(HX8357D_DEBUG) or DEFINED(FRAMEBUFFER_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'HX8357D: Write Data Ex (Values size=' + IntToStr(Length(Values)) + ')');
 {$ENDIF}
 
 {Set DC High}
 GPIODeviceOutputSet(Framebuffer.DC.GPIO,Framebuffer.DC.Pin,GPIO_LEVEL_HIGH);
 
 {Write Data}
 Result:=SPIDeviceWrite(Framebuffer.SPI,Framebuffer.ChipSelect,@Values[0],Length(Values),SPI_TRANSFER_NONE,Count);
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
