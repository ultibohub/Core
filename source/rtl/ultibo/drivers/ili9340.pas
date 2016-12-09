{
ILITEK ILI9340 TFT LCD Driver.

Copyright (C) 2016 - SoftOz Pty Ltd.
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

  Linux - \drivers\staging\fbtft\fb_ili9340.c - Copyright (C) 2013 Noralf Tronnes
  Linux - \drivers\staging\fbtft\fbtft_device.c - Copyright (C) 2013 Noralf Tronnes
  Linux - \drivers\staging\fbtft\fbtft-core.c - Copyright (C) 2013 Noralf Tronnes
  
References
==========
 
  ILI9340 - https://cdn-shop.adafruit.com/datasheets/ILI9340.pdf
 
  https://github.com/notro/fbtft/wiki
  https://github.com/notro/fbtft/wiki/LCD-Modules
  https://github.com/notro/fbtft/wiki/LCD-Shields
  
ILITEK ILI9340
==============
 
 The ILITEK ILI9340 is a 240x320 Resolution RGB 262K color TFT LCD Single Chip Driver
 that supports color depths of 16 or 18bit. This driver supports the chip only in 16
 bit depth using RGB565 format.
 
 The chip provides an SPI interface at up to 32MHz and supports rotations of
 0, 90, 180 and 270 degrees.
 
}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit ILI9340; 

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,GPIO,SPI,Framebuffer,TFTFramebuffer,SysUtils;

{==============================================================================}
{Global definitions}
{$INCLUDE ..\core\GlobalDefines.inc}

{==============================================================================}
const
 {ILI9340 specific constants}
 ILI9340_FRAMEBUFFER_DESCRIPTION = 'ILITEK ILI9340 TFT LCD';  {Description of ILI9340 device}
  
 {ILI9340 SPI constants}
 ILI9340_SPI_RATE = 32000000; {Default SPI clock rate}
  
 {ILI9340 Command constants}
 ILI9340_CMD_NOP      = $00; {8.2.1. NOP: No Operation}
 ILI9340_CMD_SWRESET  = $01; {8.2.2. SWRESET: Software Reset}
 
 ILI9340_CMD_SLPOUT   = $11; {8.2.12. SLPOUT: Sleep Out (This command turns off sleep mode)}
 
 ILI9340_CMD_GAMSET   = $26; {8.2.17. GAMSET: Gamma Set (This command is used to select the desired Gamma curve for the current display)}
 
 ILI9340_CMD_DISPOFF  = $28; {8.2.18. DISPOFF: Display OFF (This command is used to enter into DISPLAY OFF mode. In this mode, the output from Frame Memory is disabled and blank page inserted)}
 ILI9340_CMD_DISPON   = $29; {8.2.19. DISPON: Display ON (This command is used to recover from DISPLAY OFF mode. Output from the Frame Memory is enabled)}
 
 ILI9340_CMD_CASET    = $2A; {8.2.20. CASET: Column Address Set (This command is used to define area of frame memory where MCU can access)}
 ILI9340_CMD_PASET    = $2B; {8.2.21. PASET: Page Address Set (This command is used to define area of frame memory where MCU can access)}
 ILI9340_CMD_RAMWR    = $2C; {8.2.22. Memory Write (This command is used to transfer data from MCU to frame memory)}
 
 ILI9340_CMD_MADCTL   = $36; {8.2.29. MADCTL: Memory Access Control (This command defines read/write scanning direction of frame memory)}
 
 ILI9340_CMD_COLMOD   = $3A; {8.2.33. COLMOD: Pixel Format Set (This command sets the pixel format for the RGB image data used by the interface)}
 
 ILI9340_CMD_FRMCTR1  = $B1; {8.3.2. FRMCTR1: Frame Rate Control (In Normal Mode/Full Colors)}
 
 ILI9340_CMD_DISCTRL  = $B6; {8.3.7. DISCTRL: Display Function Control}
 
 ILI9340_CMD_PWCTRL1  = $C0; {8.3.16. PWCTRL1: Power Control 1}
 ILI9340_CMD_PWCTRL2  = $C1; {8.3.17. PWCTRL2: Power Control 2}
 
 ILI9340_CMD_VMCTRL1  = $C5; {8.3.21. VMCTRL1: VCOM Control 1}
 ILI9340_CMD_VMCTRL2  = $C7; {8.3.22. VMCTRL2: VCOM Control 2}
 
 ILI9340_CMD_PGAMCTRL = $E0; {8.3.27. PGAMCTRL: Positive Gamma Correction (Set the gray scale voltage to adjust the gamma characteristics of the TFT panel)}
 ILI9340_CMD_NGAMCTRL = $E1; {8.3.28. NGAMCTRL: Negative Gamma Correction (Set the gray scale voltage to adjust the gamma characteristics of the TFT panel)}
 
 {ILI9340 Memory access control constants (See ILI9340 datasheet 8.2.29. Memory Access Control)}
 ILI9340_CMD_MADCTL_MY  = $80; {Row Address Order}
 ILI9340_CMD_MADCTL_MX  = $40; {Column Address Order}
 ILI9340_CMD_MADCTL_MV  = $20; {Row / Column Exchange}
 ILI9340_CMD_MADCTL_ML  = $10; {Vertical Refresh Order}
 ILI9340_CMD_MADCTL_RGB = $00; {Colour selector switch control(0=RGB colour filter panel, 1=BGR colour filter panel)}
 ILI9340_CMD_MADCTL_BGR = $08; 
 ILI9340_CMD_MADCTL_MH  = $04; {Horizontal Refresh Order}
  
{==============================================================================}
type
 {ILI9340 specific types}
 PILI9340Framebuffer = ^TILI9340Framebuffer;
 TILI9340Framebuffer = record
  {TFT Properties}
  TFT:TTFTFramebuffer;
  {ILI9340 Properties}
 end; 
  
{==============================================================================}
{var}
 {ILI9340 specific variables}
 
{==============================================================================}
{Initialization Functions}
 
{==============================================================================}
{ILI9340 Functions}
function ILI9340FramebufferCreate(SPI:PSPIDevice;ChipSelect:Word;const Name:String;Rotation,Direction,Width,Height:LongWord;RST,DC,BL:PGPIOInfo):PFramebufferDevice;
  
function ILI9340FramebufferDestroy(Framebuffer:PFramebufferDevice):LongWord;

{==============================================================================}
{ILI9340 Framebuffer Functions}
function ILI9340FramebufferBlank(Framebuffer:PFramebufferDevice;Blank:Boolean):LongWord;
  
function ILI9340FramebufferSetBacklight(Framebuffer:PFramebufferDevice;Brightness:LongWord):LongWord;
  
{==============================================================================}
{ILI9340 TFTFramebuffer Functions}
function ILI9340TFTFramebufferInitialize(Framebuffer:PTFTFramebuffer;Defaults:PFramebufferProperties):LongWord;
function ILI9340TFTFramebufferDeinitialize(Framebuffer:PTFTFramebuffer):LongWord;

function ILI9340TFTFramebufferGetDefaults(Framebuffer:PTFTFramebuffer;Properties,Defaults:PFramebufferProperties):LongWord;

function ILI9340TFTFramebufferSetWriteAddress(Framebuffer:PTFTFramebuffer;X1,Y1,X2,Y2:LongWord):LongWord;

function ILI9340TFTFramebufferWriteMemory(Framebuffer:PTFTFramebuffer;Address,Size:LongWord):LongWord;

{==============================================================================}
{ILI9340 Helper Functions}
function ILI9340WriteCommand(Framebuffer:PTFTFramebuffer;Value:Byte):LongWord;
function ILI9340WriteCommandEx(Framebuffer:PTFTFramebuffer;const Values:array of Byte):LongWord;

function ILI9340WriteData(Framebuffer:PTFTFramebuffer;Value:Byte):LongWord; 
function ILI9340WriteDataEx(Framebuffer:PTFTFramebuffer;const Values:array of Byte):LongWord;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
{var}
 {ILI9340 specific variables}
 
{==============================================================================}
{==============================================================================}
{Initialization Functions}
 
{==============================================================================}
{==============================================================================}
{ILI9340 Functions}
function ILI9340FramebufferCreate(SPI:PSPIDevice;ChipSelect:Word;const Name:String;Rotation,Direction,Width,Height:LongWord;RST,DC,BL:PGPIOInfo):PFramebufferDevice;
{Create, register and allocate a new ILI9340 Framebuffer device which can be accessed using the framebuffer API}
{SPI: The SPI device that this ILI9340 is connected to}
{ChipSelect: The SPI chip select to use when communicating with this device}
{Name: The text description of this device which will should in the device list (Optional)}
{Rotation: The rotation value for the framebuffer device (eg FRAMEBUFFER_ROTATION_180)}
{Direction: The direction of the display (eg FRAMEBUFFER_DIRECTION_REVERSE)}
{Width: The width of the framebuffer in pixels}
{Height: The height of the framebuffer in pixels}
{RST: GPIO pin information for the Reset pin (Optional)}
{DC: GPIO pin information for the Data/Command pin}
{BL: GPIO pin information for the Backlight pin (Optional)}
{Return: Pointer to the new Framebuffer device or nil if the framebuffer device could not be created}
var
 Status:LongWord;
 SPIProperties:TSPIProperties;
 ILI9340Framebuffer:PILI9340Framebuffer;
begin
 {}
 Result:=nil;
 
 {$IF DEFINED(ILI9340_DEBUG) or DEFINED(FRAMEBUFFER_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'ILI9340: Framebuffer Create (Name=' + Name + ' Width=' + IntToStr(Width) + ' Height=' + IntToStr(Height) + ')');
 {$ENDIF}
 
 {Check Rotation}
 if Rotation > FRAMEBUFFER_ROTATION_270 then Exit;
 
 {Check Direction}
 if Direction > FRAMEBUFFER_DIRECTION_REVERSE then Exit;
 
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
 ILI9340Framebuffer:=PILI9340Framebuffer(FramebufferDeviceCreateEx(SizeOf(TILI9340Framebuffer)));
 if ILI9340Framebuffer <> nil then
  begin
   {Update Framebuffer}
   {Device}
   ILI9340Framebuffer.TFT.Framebuffer.Device.DeviceBus:=DEVICE_BUS_SPI; 
   ILI9340Framebuffer.TFT.Framebuffer.Device.DeviceType:=FRAMEBUFFER_TYPE_HARDWARE;
   ILI9340Framebuffer.TFT.Framebuffer.Device.DeviceFlags:=FRAMEBUFFER_FLAG_DMA or FRAMEBUFFER_FLAG_MARK or FRAMEBUFFER_FLAG_COMMIT or FRAMEBUFFER_FLAG_BLANK or FRAMEBUFFER_FLAG_CACHED or FRAMEBUFFER_FLAG_BACKLIGHT{$IFNDEF FPC_BIG_ENDIAN}or FRAMEBUFFER_FLAG_SWAP{$ENDIF FPC_BIG_ENDIAN};;
   ILI9340Framebuffer.TFT.Framebuffer.Device.DeviceData:=nil;
   if Length(Name) <> 0 then ILI9340Framebuffer.TFT.Framebuffer.Device.DeviceDescription:=Name else ILI9340Framebuffer.TFT.Framebuffer.Device.DeviceDescription:=ILI9340_FRAMEBUFFER_DESCRIPTION;
   {Framebuffer}
   ILI9340Framebuffer.TFT.Framebuffer.FramebufferState:=FRAMEBUFFER_STATE_DISABLED;
   ILI9340Framebuffer.TFT.Framebuffer.DeviceAllocate:=TFTFramebufferAllocate;
   ILI9340Framebuffer.TFT.Framebuffer.DeviceRelease:=TFTFramebufferRelease;
   ILI9340Framebuffer.TFT.Framebuffer.DeviceBlank:=ILI9340FramebufferBlank;
   ILI9340Framebuffer.TFT.Framebuffer.DeviceMark:=TFTFramebufferMark;
   ILI9340Framebuffer.TFT.Framebuffer.DeviceCommit:=TFTFramebufferCommit;
   ILI9340Framebuffer.TFT.Framebuffer.DeviceSetBacklight:=ILI9340FramebufferSetBacklight;
   ILI9340Framebuffer.TFT.Framebuffer.DeviceSetProperties:=TFTFramebufferSetProperties;
   {TFT}
   ILI9340Framebuffer.TFT.SPI:=SPI;
   ILI9340Framebuffer.TFT.ChipSelect:=ChipSelect;
   if RST <> nil then ILI9340Framebuffer.TFT.RST:=RST^ else ILI9340Framebuffer.TFT.RST:=GPIO_INFO_UNKNOWN;
   ILI9340Framebuffer.TFT.DC:=DC^;
   if BL <> nil then ILI9340Framebuffer.TFT.BL:=BL^ else ILI9340Framebuffer.TFT.BL:=GPIO_INFO_UNKNOWN;
   ILI9340Framebuffer.TFT.Initialize:=ILI9340TFTFramebufferInitialize;
   ILI9340Framebuffer.TFT.Deinitialize:=ILI9340TFTFramebufferDeinitialize;
   ILI9340Framebuffer.TFT.GetDefaults:=ILI9340TFTFramebufferGetDefaults;
   ILI9340Framebuffer.TFT.SetWriteAddress:=ILI9340TFTFramebufferSetWriteAddress;
   ILI9340Framebuffer.TFT.WriteMemory:=ILI9340TFTFramebufferWriteMemory;
   {Driver}
   ILI9340Framebuffer.TFT.Width:=Width;
   ILI9340Framebuffer.TFT.Height:=Height;
   ILI9340Framebuffer.TFT.Rotation:=Rotation;
   if (Rotation = FRAMEBUFFER_ROTATION_90) or (Rotation = FRAMEBUFFER_ROTATION_270) then
    begin
     ILI9340Framebuffer.TFT.Width:=Height;
     ILI9340Framebuffer.TFT.Height:=Width;
    end;
   ILI9340Framebuffer.TFT.Direction:=Direction;
   ILI9340Framebuffer.TFT.DirtyY1:=Height - 1;
   ILI9340Framebuffer.TFT.DirtyY2:=0;
   ILI9340Framebuffer.TFT.Ready:=True;
   ILI9340Framebuffer.TFT.Lock:=INVALID_HANDLE_VALUE;
   ILI9340Framebuffer.TFT.Timer:=INVALID_HANDLE_VALUE;
   ILI9340Framebuffer.TFT.FrameRate:=TFT_FRAMEBUFFER_FRAME_RATE_DEFAULT;
   ILI9340Framebuffer.TFT.TransferSize:=LongWord(-1);
   if SPIDeviceProperties(SPI,@SPIProperties) = ERROR_SUCCESS then
    begin
     if SPIProperties.MaxSize <> 0 then ILI9340Framebuffer.TFT.TransferSize:=SPIProperties.MaxSize; 
    end; 

   {Setup Flags}
   {Nothing}
   
   {Register Framebuffer}
   Status:=FramebufferDeviceRegister(@ILI9340Framebuffer.TFT.Framebuffer);
   if Status = ERROR_SUCCESS then
    begin
     {Allocate Framebuffer}
     Status:=FramebufferDeviceAllocate(@ILI9340Framebuffer.TFT.Framebuffer,nil);
     if Status = ERROR_SUCCESS then
      begin
       {Return Result}
       Result:=PFramebufferDevice(ILI9340Framebuffer); 
      end
     else
      begin
       if DEVICE_LOG_ENABLED then DeviceLogError(nil,'ILI9340: Failed to allocate new framebuffer device: ' + ErrorToString(Status));
      end;
    end
   else
    begin     
     if DEVICE_LOG_ENABLED then DeviceLogError(nil,'ILI9340: Failed to register new framebuffer device: ' + ErrorToString(Status));
    end;
  end
 else
  begin
   if DEVICE_LOG_ENABLED then DeviceLogError(nil,'ILI9340: Failed to create new framebuffer device');
  end;
end;

{==============================================================================}

function ILI9340FramebufferDestroy(Framebuffer:PFramebufferDevice):LongWord;
{Release, deregister and destroy an ILI9340 Framebuffer device created by this driver}
{Framebuffer: The Framebuffer device to destroy}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 
 {$IF DEFINED(ILI9340_DEBUG) or DEFINED(FRAMEBUFFER_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'ILI9340: Framebuffer Destroy');
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
       if DEVICE_LOG_ENABLED then DeviceLogError(nil,'ILI9340: Failed to destroy framebuffer device: ' + ErrorToString(Result));
      end;
    end
   else
    begin
     if DEVICE_LOG_ENABLED then DeviceLogError(nil,'ILI9340: Failed to deregister framebuffer device: ' + ErrorToString(Result));
    end;    
  end
 else
  begin
   if DEVICE_LOG_ENABLED then DeviceLogError(nil,'ILI9340: Failed to release framebuffer device: ' + ErrorToString(Result));
  end;  
end;

{==============================================================================}
{==============================================================================}
{ILI9340 Framebuffer Functions}
function ILI9340FramebufferBlank(Framebuffer:PFramebufferDevice;Blank:Boolean):LongWord;
{Implementation of FramebufferDeviceBlank API for ILI9340}
{Note: Not intended to be called directly by applications, use FramebufferDeviceBlank instead}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IF DEFINED(ILI9340_DEBUG) or DEFINED(FRAMEBUFFER_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'ILI9340: Framebuffer Blank (Blank=' + BooleanToString(Blank) + ')');
 {$ENDIF}
 
 {Check Blank}
 if Blank then
  begin
   {Display Off}
   Result:=ILI9340WriteCommand(PTFTFramebuffer(Framebuffer),ILI9340_CMD_DISPOFF);
  end
 else
  begin
   {Display On}
   Result:=ILI9340WriteCommand(PTFTFramebuffer(Framebuffer),ILI9340_CMD_DISPON);
  end;  
end;
  
{==============================================================================}
  
function ILI9340FramebufferSetBacklight(Framebuffer:PFramebufferDevice;Brightness:LongWord):LongWord;
{Implementation of FramebufferDeviceSetBacklight API for ILI9340}
{Note: Not intended to be called directly by applications, use FramebufferDeviceSetBacklight instead}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IF DEFINED(ILI9340_DEBUG) or DEFINED(FRAMEBUFFER_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'ILI9340: Framebuffer Set Backlight (Brightness=' + IntToStr(Brightness) + ')');
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
{ILI9340 TFTFramebuffer Functions}
function ILI9340TFTFramebufferInitialize(Framebuffer:PTFTFramebuffer;Defaults:PFramebufferProperties):LongWord;
{Implementation of TFTFramebufferInitialize API for ILI9340}
{Note: Not intended to be called directly by applications}
var
 Value:Byte;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 
 {$IF DEFINED(ILI9340_DEBUG) or DEFINED(FRAMEBUFFER_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'ILI9340: Framebuffer Initialize');
 {$ENDIF}
 
 {Check Defaults}
 if Defaults = nil then Exit;
 
 {Set the SPI clock rate}
 if SPIDeviceSetClockRate(Framebuffer.SPI,Framebuffer.ChipSelect,ILI9340_SPI_RATE) <> ERROR_SUCCESS then
  begin
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;
 
 {Start the SPI device}
 if SPIDeviceStart(Framebuffer.SPI,SPI_MODE_4WIRE,ILI9340_SPI_RATE,SPI_CLOCK_PHASE_LOW,SPI_CLOCK_POLARITY_LOW) <> ERROR_SUCCESS then
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
 ILI9340WriteCommand(Framebuffer,ILI9340_CMD_SWRESET);
 Sleep(5);
 
 {Undocumented}
 ILI9340WriteCommand(Framebuffer,$EF);
 ILI9340WriteDataEx(Framebuffer,[$03, $80, $02]);
 
 {Undocumented}
 ILI9340WriteCommand(Framebuffer,$CF);
 ILI9340WriteDataEx(Framebuffer,[$00, $C1, $30]);
 
 {Undocumented}
 ILI9340WriteCommand(Framebuffer,$ED);
 ILI9340WriteDataEx(Framebuffer,[$64, $03, $12, $81]);
 
 {Undocumented}
 ILI9340WriteCommand(Framebuffer,$E8);
 ILI9340WriteDataEx(Framebuffer,[$85, $00, $78]);
 
 {Undocumented}
 ILI9340WriteCommand(Framebuffer,$CB);
 ILI9340WriteDataEx(Framebuffer,[$39, $2C, $00, $34, $02]);
 
 {Undocumented}
 ILI9340WriteCommand(Framebuffer,$F7);
 ILI9340WriteData(Framebuffer,$20);
 
 {Undocumented}
 ILI9340WriteCommand(Framebuffer,$EA);
 ILI9340WriteDataEx(Framebuffer,[$00, $00]);
 
 {Power Control 1}
 ILI9340WriteCommand(Framebuffer,ILI9340_CMD_PWCTRL1);
 ILI9340WriteData(Framebuffer,$23);
 
 {Power Control 2}
 ILI9340WriteCommand(Framebuffer,ILI9340_CMD_PWCTRL2);
 ILI9340WriteData(Framebuffer,$10);
 
 {VCOM Control 1}
 ILI9340WriteCommand(Framebuffer,ILI9340_CMD_VMCTRL1);
 ILI9340WriteDataEx(Framebuffer,[$3E, $28]);
 
 {VCOM Control 2}
 ILI9340WriteCommand(Framebuffer,ILI9340_CMD_VMCTRL2);
 ILI9340WriteData(Framebuffer,$86);
 
 {COLMOD: Pixel Format Set}
 {0x55 = 16 bits/pixel / 0x66 = 18 bits/pixel}
 ILI9340WriteCommand(Framebuffer,ILI9340_CMD_COLMOD);
 ILI9340WriteData(Framebuffer,$55);
 
 {Memory access control}
 ILI9340WriteCommand(Framebuffer,ILI9340_CMD_MADCTL);
 Value:=0;
 {RGB/BGR}
 if Defaults.Order = FRAMEBUFFER_ORDER_RGB then
  begin
   Value:=Value or ILI9340_CMD_MADCTL_RGB; 
  end
 else if Defaults.Order = FRAMEBUFFER_ORDER_BGR then
  begin
   Value:=Value or ILI9340_CMD_MADCTL_BGR;
  end;
 {Horizontal/Vertical memory direction (Rotation)}
 if Defaults.Rotation = FRAMEBUFFER_ROTATION_0 then
  begin
   Value:=Value or ILI9340_CMD_MADCTL_MX or ILI9340_CMD_MADCTL_MY;
  end
 else if Defaults.Rotation = FRAMEBUFFER_ROTATION_90 then
  begin
   Value:=Value or ILI9340_CMD_MADCTL_MV or ILI9340_CMD_MADCTL_MY;
  end
 else if Defaults.Rotation = FRAMEBUFFER_ROTATION_180 then 
  begin
   {Nothing}
  end
 else if Defaults.Rotation = FRAMEBUFFER_ROTATION_270 then
  begin
   Value:=Value or ILI9340_CMD_MADCTL_MV or ILI9340_CMD_MADCTL_MX;
  end;
 
 if Defaults.Direction = FRAMEBUFFER_DIRECTION_REVERSE then
 begin
  Value:=Value xor ILI9340_CMD_MADCTL_MX;
 end;   
 
 ILI9340WriteData(Framebuffer,Value); 
 
 {Frame Rate Control (Division ratio = fosc, Frame Rate = 79Hz)}
 ILI9340WriteCommand(Framebuffer,ILI9340_CMD_FRMCTR1);
 ILI9340WriteDataEx(Framebuffer,[$00, $18]);
 
 {Display Function Control}
 ILI9340WriteCommand(Framebuffer,ILI9340_CMD_DISCTRL);
 ILI9340WriteDataEx(Framebuffer,[$08, $82, $27]);
 
 {Gamma Function Disable}
 ILI9340WriteCommand(Framebuffer,$F2);
 ILI9340WriteData(Framebuffer,$00);
 
 {Gamma curve selected}
 ILI9340WriteCommand(Framebuffer,ILI9340_CMD_GAMSET);
 ILI9340WriteData(Framebuffer,$01);
 
 {Positive Gamma Correction}
 ILI9340WriteCommand(Framebuffer,ILI9340_CMD_PGAMCTRL);
 ILI9340WriteDataEx(Framebuffer,[$0F, $31, $2B, $0C, $0E, $08, $4E, $F1, $37, $07, $10, $03, $0E, $09, $00]);
 
 {Negative Gamma Correction}
 ILI9340WriteCommand(Framebuffer,ILI9340_CMD_NGAMCTRL);
 ILI9340WriteDataEx(Framebuffer,[$00, $0E, $14, $03, $11, $07, $31, $C1, $48, $08, $0F, $0C, $31, $36, $0F]);
 
 {Sleep OUT}
 ILI9340WriteCommand(Framebuffer,ILI9340_CMD_SLPOUT);
 Sleep(5); {Linux driver sleeps for 120ms}
 
 {Display On}
 ILI9340WriteCommand(Framebuffer,ILI9340_CMD_DISPON);
 
 Result:=ERROR_SUCCESS;
end;
  
{==============================================================================}

function ILI9340TFTFramebufferDeinitialize(Framebuffer:PTFTFramebuffer):LongWord;
{Implementation of TFTFramebufferDeinitialize API for ILI9340}
{Note: Not intended to be called directly by applications}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 
 {$IF DEFINED(ILI9340_DEBUG) or DEFINED(FRAMEBUFFER_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'ILI9340: Framebuffer Initialize');
 {$ENDIF}

 {Display Off}
 ILI9340WriteCommand(Framebuffer,ILI9340_CMD_DISPOFF);
 
 Result:=ERROR_SUCCESS;
end;
  
{==============================================================================}
  
function ILI9340TFTFramebufferGetDefaults(Framebuffer:PTFTFramebuffer;Properties,Defaults:PFramebufferProperties):LongWord;
{Implementation of TFTFramebufferGetDefaults API for ILI9340}
{Note: Not intended to be called directly by applications}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 
 {$IF DEFINED(ILI9340_DEBUG) or DEFINED(FRAMEBUFFER_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'ILI9340: Framebuffer Get Defaults');
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
 Defaults.Direction:=Framebuffer.Direction;
 
 {Check Properties}
 if Properties <> nil then
  begin
   {Adjust Depth}
   if Properties.Depth = FRAMEBUFFER_DEPTH_16 then Defaults.Depth:=Properties.Depth;
   {Adjust Order}
   if Properties.Order <= FRAMEBUFFER_ORDER_RGB then Defaults.Order:=Properties.Order;
   {Adjust Rotation}
   if Properties.Rotation <= FRAMEBUFFER_ROTATION_270 then Defaults.Rotation:=Properties.Rotation;
   {Adjust Direction}
   if Properties.Direction <= FRAMEBUFFER_DIRECTION_REVERSE then Defaults.Direction:=Properties.Direction;
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

function ILI9340TFTFramebufferSetWriteAddress(Framebuffer:PTFTFramebuffer;X1,Y1,X2,Y2:LongWord):LongWord;
{Implementation of TFTFramebufferSetWriteAddress API for ILI9340}
{Note: Not intended to be called directly by applications}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;

 {$IF DEFINED(ILI9340_DEBUG) or DEFINED(FRAMEBUFFER_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'ILI9340: Set Write Address (X1=' + IntToStr(X1) + ' Y1=' + IntToStr(Y1) + ' X2=' + IntToStr(X2) + ' Y2=' + IntToStr(Y2) + ')');
 {$ENDIF}

 {Column address set}
 ILI9340WriteCommand(Framebuffer,ILI9340_CMD_CASET);
 ILI9340WriteDataEx(Framebuffer,[X1 shr 8,X1 and $FF,X2 shr 8,X2 and $FF]);

 {Row address set}
 ILI9340WriteCommand(Framebuffer,ILI9340_CMD_PASET);
 ILI9340WriteDataEx(Framebuffer,[Y1 shr 8,Y1 and $FF,Y2 shr 8,Y2 and $FF]);

 {Memory write}
 ILI9340WriteCommand(Framebuffer,ILI9340_CMD_RAMWR);

 {Set DC High}
 GPIODeviceOutputSet(Framebuffer.DC.GPIO,Framebuffer.DC.Pin,GPIO_LEVEL_HIGH);
 
 Result:=ERROR_SUCCESS;
end;
  
{==============================================================================}
  
function ILI9340TFTFramebufferWriteMemory(Framebuffer:PTFTFramebuffer;Address,Size:LongWord):LongWord;
{Implementation of TFTFramebufferWriteMemory API for ILI9340}
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

 {$IF DEFINED(ILI9340_DEBUG) or DEFINED(FRAMEBUFFER_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'ILI9340: Write Memory (Address=' + IntToHex(Address,8) + ' Size=' + IntToStr(Size) + ')');
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
{ILI9340 Helper Functions}
function ILI9340WriteCommand(Framebuffer:PTFTFramebuffer;Value:Byte):LongWord;
{Write a single command value to the ILI9340}
var
 Count:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;

 {$IF DEFINED(ILI9340_DEBUG) or DEFINED(FRAMEBUFFER_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'ILI9340: Write Command (Value=' + IntToHex(Value,2) + ')');
 {$ENDIF}
 
 {Set DC Low}
 GPIODeviceOutputSet(Framebuffer.DC.GPIO,Framebuffer.DC.Pin,GPIO_LEVEL_LOW);
 
 {Write Command}
 Result:=SPIDeviceWrite(Framebuffer.SPI,Framebuffer.ChipSelect,@Value,SizeOf(Byte),SPI_TRANSFER_NONE,Count);
end;

{==============================================================================}

function ILI9340WriteCommandEx(Framebuffer:PTFTFramebuffer;const Values:array of Byte):LongWord;
{Write multiple command values to the ILI9340}
var
 Count:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;

 {$IF DEFINED(ILI9340_DEBUG) or DEFINED(FRAMEBUFFER_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'ILI9340: Write Command Ex (Values size=' + IntToStr(Length(Values)) + ')');
 {$ENDIF}
 
 {Set DC Low}
 GPIODeviceOutputSet(Framebuffer.DC.GPIO,Framebuffer.DC.Pin,GPIO_LEVEL_LOW);
 
 {Write Command}
 Result:=SPIDeviceWrite(Framebuffer.SPI,Framebuffer.ChipSelect,@Values[0],Length(Values),SPI_TRANSFER_NONE,Count);
end;

{==============================================================================}

function ILI9340WriteData(Framebuffer:PTFTFramebuffer;Value:Byte):LongWord;
{Write a single data value to the ILI9340}
var
 Count:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;

 {$IF DEFINED(ILI9340_DEBUG) or DEFINED(FRAMEBUFFER_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'ILI9340: Write Data (Value=' + IntToHex(Value,2) + ')');
 {$ENDIF}
 
 {Set DC High}
 GPIODeviceOutputSet(Framebuffer.DC.GPIO,Framebuffer.DC.Pin,GPIO_LEVEL_HIGH);
 
 {Write Data}
 Result:=SPIDeviceWrite(Framebuffer.SPI,Framebuffer.ChipSelect,@Value,SizeOf(Byte),SPI_TRANSFER_NONE,Count);
end;

{==============================================================================}

function ILI9340WriteDataEx(Framebuffer:PTFTFramebuffer;const Values:array of Byte):LongWord;
{Write multiple data values to the ILI9340}
var
 Count:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;

 {$IF DEFINED(ILI9340_DEBUG) or DEFINED(FRAMEBUFFER_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'ILI9340: Write Data Ex (Values size=' + IntToStr(Length(Values)) + ')');
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
  