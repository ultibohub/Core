{
Sitronix ST77XX TFT LCD Driver.

Copyright (C) 2023 - Nabil EL MAHIRI.

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

  Python - https://github.com/adafruit/Adafruit-ST7735-Library/blob/master/Adafruit_ST7789.h - Copyright (C) 2013 Adafruit

References
==========

 ST77XX - https://cdn-learn.adafruit.com/assets/assets/000/082/882/original/ST7789VW_SPEC_V1.0.pdf

          https://learn.adafruit.com/adafruit-1-9-color-ips-tft-display

Sitronix ST77XX
===============

 The ST77XX is a family of single-chip controllers/drivers for color, graphic type TFT-LCD display.

 Instantiate the ST77XX driver by calling ST77XXFramebufferCreate() with the required parameters:
 
  SPI: The SPI device that this ST77XX is connected to
  ChipSelect: The SPI chip select to use when communicating with this device
  Name: The text description of this device which will show in the device list (Optional)
  Rotation: The rotation value for the framebuffer device (eg FRAMEBUFFER_ROTATION_180)
  Width: The width of the framebuffer in pixels
  Height: The height of the framebuffer in pixels
  ColStart: 
  RST: GPIO pin information for the Reset pin (Optional)
  DC: GPIO pin information for the Data/Command pin
  BL: GPIO pin information for the Backlight pin (Optional)
 
}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit ST77XX;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,GPIO,SPI,Framebuffer,TFTFramebuffer,SysUtils;

{==============================================================================}
{Global definitions}
{$INCLUDE ..\core\GlobalDefines.inc}

{==============================================================================}
const
 {ST77XX specific constants}
 ST77XX_FRAMEBUFFER_DESCRIPTION = 'ST77XX TFT LCD DRIVER';  {Description of ST77XX device}

 {ST77XX SPI constants}
 ST77XX_SPI_RATE = 32000000; {Default SPI clock rate}

 ST_CMD_DELAY = $80; // special signifier for command lists

 {ST77XX specific commands used in init}
 ST77XX_NOP      = $00; {8.2.1. NOP: No Operation}
 ST77XX_SWRESET  = $01; {8.2.2. SWRESET: Software Reset}
 ST77XX_RDDID    = $04; {new_cmd}
 ST77XX_RDDST    = $09; {new_cmd}

 ST77XX_SLPIN    = $10; {new_cmd}
 ST77XX_SLPOUT   = $11; {8.2.12. SLPOUT: Sleep Out (This command turns off sleep mode)}
 ST77XX_PTLON    = $12; {new_cmd}
 ST77XX_NORON    = $13; {new_cmd}

 ST77XX_INVOFF   = $20; {new_cmd}
 ST77XX_INVON    = $21; {new_cmd}
 ST77XX_GAMSET   = $26; {8.2.17. GAMSET: Gamma Set (This command is used to select the desired Gamma curve for the current display)}
 ST77XX_DISPOFF  = $28; {8.2.18. DISPOFF: Display OFF (This command is used to enter into DISPLAY OFF mode. In this mode, the output from Frame Memory is disabled and blank page inserted)}
 ST77XX_DISPON   = $29; {8.2.19. DISPON: Display ON (This command is used to recover from DISPLAY OFF mode. Output from the Frame Memory is enabled)}
 ST77XX_CASET    = $2A; {8.2.20. CASET: Column Address Set (This command is used to define area of frame memory where MCU can access)}
 ST77XX_RASET    = $2B; {8.2.21. PASET: Page Address Set (This command is used to define area of frame memory where MCU can access)}
 ST77XX_RAMWR    = $2C; {8.2.22. Memory Write (This command is used to transfer data from MCU to frame memory)}
 ST77XX_RGBSET   = $2D; {new_cmd} // Color setting for 4096, 64K and 262K colors
 ST77XX_RAMRD    = $2E; {new_cmd}

 ST77XX_RDDPM      = $0A; {new_cmd} // Read display power mode
 ST77XX_RDD_MADCTL = $0B; {new_cmd} // Read display MADCTL
 ST77XX_RDD_COLMOD = $0C; {new_cmd} // Read display pixel format
 ST77XX_RDDIM      = $0D; {new_cmd} // Read display image mode
 ST77XX_RDDSM      = $0E; {new_cmd} // Read display signal mode
 ST77XX_RDDSR      = $0F; {new_cmd} // Read display self-diagnostic result (ST7789V)

 ST77XX_PTLAR    = $30; {new_cmd}
 ST77XX_VSCRDEF  = $33; {new_cmd} // Vertical scrolling definition (ST7789V)
 ST77XX_TEOFF    = $34; {new_cmd} // Tearing effect line off
 ST77XX_TEON     = $35; {new_cmd} // Tearing effect line on
 ST77XX_MADCTL   = $36; {8.2.29. MADCTL: Memory Access Control (This command defines read/write scanning direction of frame memory)}
 ST77XX_IDMOFF   = $38; {new_cmd} // Idle mode off
 ST77XX_IDMON    = $39; {new_cmd} // Idle mode on
 ST77XX_RAMWRC   = $3C; {new_cmd} // Memory write continue (ST7789V)
 ST77XX_RAMRDC   = $3E; {new_cmd} // Memory read continue (ST7789V)
 ST77XX_COLMOD   = $3A; {8.2.33. COLMOD: Pixel Format Set (This command sets the pixel format for the RGB image data used by the interface)}


 {ST77XX Memory access control constants (See ST77XX datasheet 8.2.29. Memory Access Control)}
 ST77XX_MADCTL_MY  = $80; {Row Address Order}
 ST77XX_MADCTL_MX  = $40; {Column Address Order}
 ST77XX_MADCTL_MV  = $20; {Row / Column Exchange}
 ST77XX_MADCTL_ML  = $10; {Row / Column Exchange}
 ST77XX_MADCTL_MH  = $04; {Horizontal Refresh Order}
 ST77XX_MADCTL_RGB = $00; {Colour selector switch control(0=RGB colour filter panel, 1=BGR colour filter panel)}
 ST77XX_MADCTL_BGR = $08;

 ST77XX_RAMCTRL  = $B0; {new_cmd} // RAM control
 ST77XX_RGBCTRL  = $B1; {new_cmd} // RGB control
 ST77XX_PORCTRL  = $B2; {new_cmd} // Porch control
 ST77XX_FRCTR1  = $B3; {8.3.2. FRCTR1: Frame Rate Control (In Normal Mode/Full Colors)}
 ST77XX_PARCTRL  = $B5; {new_cmd} // Partial mode control
 ST77XX_GCTRL    = $B7; {new_cmd} // Gate control
 ST77XX_GTADJ    = $B8; {new_cmd} // Gate on timing adjustment
 ST77XX_DGMEN    = $BA; {new_cmd} // Digital gamma enable
 ST77XX_VCOMS    = $BB; {new_cmd} // VCOMS setting
 ST77XX_DISCTRL  = $B6; {8.3.7. DISCTRL: Display Function Control}

 ST77XX_LCMCTRL  = $C0; {new_cmd} // LCM control
 ST77XX_IDSET    = $C1; {new_cmd} // ID setting
 ST77XX_VDVVRHEN = $C2; {new_cmd} // VDV and VRH command enable
 ST77XX_VRHS     = $C3; {new_cmd} // VRH set
 ST77XX_VDVSET   = $C4; {new_cmd} // VDV setting
 ST77XX_VCMOFSET = $C5; {new_cmd} // VCOMS offset set
 ST77XX_FRCTR2   = $C6; {new_cmd} // FR Control 2
 ST77XX_CABCCTRL = $C7; {new_cmd} // CABC control
 ST77XX_REGSEL1  = $C8; {new_cmd} // Register value section 1
 ST77XX_REGSEL2  = $CA; {new_cmd} // Register value section 2
 ST77XX_PWMFRSEL = $CC; {new_cmd} // PWM frequency selection

 ST77XX_PWCTRL1  = $D0; {8.3.16. PWCTRL1: Power Control 1}
 ST77XX_VAPVANEN = $D2; {new_cmd}// Enable VAP/VAN signal output
 ST77XX_CMD2EN   = $DF; {new_cmd}// Command 2 enable

 ST77XX_VMCTRL1  = $C5; {8.3.21. VMCTRL1: VCOM Control 1}
 ST77XX_VMCTRL2  = $C7; {8.3.22. VMCTRL2: VCOM Control 2}

 ST77XX_PGAMCTRL = $E0; {new_cmd} // Positive voltage gamma control
 ST77XX_NGAMCTRL = $E1; {new_cmd} // Negative voltage gamma control
 ST77XX_DGMLUTR   = $E2; {new_cmd} // Digital gamma look-up table for red
 ST77XX_DGMLUTB   = $E3; {new_cmd} // Digital gamma look-up table for blue
 ST77XX_GATECTRL  = $E4; {new_cmd} // Gate control
 ST77XX_SPI2EN    = $E7; {new_cmd} // SPI2 enable
 ST77XX_PWCTRL2   = $E8; {new_cmd} // Power control 2
 ST77XX_EQCTRL    = $E9; {new_cmd} // Equalize time control
 ST77XX_PROMCTRL  = $EC; {new_cmd} // Program control

 ST77XX_PROMEN    = $FA; {new_cmd} // Program mode enable
 ST77XX_NVMSET    = $FC; {new_cmd} // NVM setting
 ST77XX_PROMACT   = $FE; {new_cmd} // Program action

 ST77XX_COLOR_MODE_16bit =$55;
 ST77XX_COLOR_MODE_18bit =$66;



{==============================================================================}
type
 {ST77XX specific types}
 PST77XXFramebuffer = ^TST77XXFramebuffer;
 TST77XXFramebuffer = record
  {TFT Properties}
  TFT:TTFTFramebuffer;
  {ST77XX Properties}
 end;

{==============================================================================}
{var}
{ST77XX specific variables}

{==============================================================================}
{Initialization Functions}

{==============================================================================}
{ST77XX Functions}
function ST77XXFramebufferCreate(SPI:PSPIDevice;ChipSelect:Word;const Name:String;Rotation,Width,Height,ColStart:LongWord;RST,DC,BL:PGPIOInfo):PFramebufferDevice;

function ST77XXFramebufferDestroy(Framebuffer:PFramebufferDevice):LongWord;

{==============================================================================}
{ST77XX Framebuffer Functions}
function ST77XXFramebufferBlank(Framebuffer:PFramebufferDevice;Blank:Boolean):LongWord;

function ST77XXFramebufferSetBacklight(Framebuffer:PFramebufferDevice;Brightness:LongWord):LongWord;

{==============================================================================}
{ST77XX TFTFramebuffer Functions}
function ST77XXTFTFramebufferInitialize(Framebuffer:PTFTFramebuffer;Defaults:PFramebufferProperties):LongWord;
function ST77XXTFTFramebufferDeinitialize(Framebuffer:PTFTFramebuffer):LongWord;

function ST77XXTFTFramebufferGetDefaults(Framebuffer:PTFTFramebuffer;Properties,Defaults:PFramebufferProperties):LongWord;

function ST77XXTFTFramebufferSetWriteAddress(Framebuffer:PTFTFramebuffer;X1,Y1,X2,Y2:LongWord):LongWord;

function ST77XXTFTFramebufferWriteMemory(Framebuffer:PTFTFramebuffer;Address:PtrUInt;Size:LongWord):LongWord;

{==============================================================================}
{ST77XX Helper Functions}
function ST77XXWriteCommand(Framebuffer:PTFTFramebuffer;Value:Byte):LongWord;
function ST77XXWriteCommandEx(Framebuffer:PTFTFramebuffer;const Values:array of Byte):LongWord;

function ST77XXWriteData(Framebuffer:PTFTFramebuffer;Value:Byte):LongWord;
function ST77XXWriteDataEx(Framebuffer:PTFTFramebuffer;const Values:array of Byte):LongWord;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
{var}
 {ST77XX specific variables}

{==============================================================================}
{==============================================================================}
{Initialization Functions}

{==============================================================================}
{==============================================================================}
{ST77XX Functions}
function ST77XXFramebufferCreate(SPI:PSPIDevice;ChipSelect:Word;const Name:String;Rotation,Width,Height,ColStart:LongWord;RST,DC,BL:PGPIOInfo):PFramebufferDevice;
{Create, register and allocate a new ST77XX Framebuffer device which can be accessed using the framebuffer API}
{SPI: The SPI device that this ST77XX is connected to}
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
 ST77XXFramebuffer:PST77XXFramebuffer;
begin
 {}
 Result:=nil;

 {$IF DEFINED(ST77XX_DEBUG) or DEFINED(FRAMEBUFFER_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'ST77XX: Framebuffer Create (Name=' + Name + ' Width=' + IntToStr(Width) + ' Height=' + IntToStr(Height) + ')');
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
 ST77XXFramebuffer:=PST77XXFramebuffer(FramebufferDeviceCreateEx(SizeOf(TST77XXFramebuffer)));
 if ST77XXFramebuffer <> nil then
  begin
   {Update Framebuffer}
   {Device}
   ST77XXFramebuffer.TFT.Framebuffer.Device.DeviceBus:=DEVICE_BUS_SPI;
   ST77XXFramebuffer.TFT.Framebuffer.Device.DeviceType:=FRAMEBUFFER_TYPE_HARDWARE;
   ST77XXFramebuffer.TFT.Framebuffer.Device.DeviceFlags:=FRAMEBUFFER_FLAG_DMA or FRAMEBUFFER_FLAG_MARK or FRAMEBUFFER_FLAG_COMMIT or FRAMEBUFFER_FLAG_BLANK or FRAMEBUFFER_FLAG_CACHED or FRAMEBUFFER_FLAG_BACKLIGHT{$IFNDEF FPC_BIG_ENDIAN}or FRAMEBUFFER_FLAG_SWAP{$ENDIF FPC_BIG_ENDIAN};
   ST77XXFramebuffer.TFT.Framebuffer.Device.DeviceData:=nil;
   if Length(Name) <> 0 then ST77XXFramebuffer.TFT.Framebuffer.Device.DeviceDescription:=Name else ST77XXFramebuffer.TFT.Framebuffer.Device.DeviceDescription:=ST77XX_FRAMEBUFFER_DESCRIPTION;
   {Framebuffer}
   ST77XXFramebuffer.TFT.Framebuffer.FramebufferState:=FRAMEBUFFER_STATE_DISABLED;
   ST77XXFramebuffer.TFT.Framebuffer.DeviceAllocate:=TFTFramebufferAllocate;
   ST77XXFramebuffer.TFT.Framebuffer.DeviceRelease:=TFTFramebufferRelease;
   ST77XXFramebuffer.TFT.Framebuffer.DeviceBlank:=ST77XXFramebufferBlank;
   ST77XXFramebuffer.TFT.Framebuffer.DeviceMark:=TFTFramebufferMark;
   ST77XXFramebuffer.TFT.Framebuffer.DeviceCommit:=TFTFramebufferCommit;
   ST77XXFramebuffer.TFT.Framebuffer.DeviceSetBacklight:=ST77XXFramebufferSetBacklight;
   {TFT}
   ST77XXFramebuffer.TFT.SPI:=SPI;
   ST77XXFramebuffer.TFT.ChipSelect:=ChipSelect;
   if RST <> nil then ST77XXFramebuffer.TFT.RST:=RST^ else ST77XXFramebuffer.TFT.RST:=GPIO_INFO_UNKNOWN;
   ST77XXFramebuffer.TFT.DC:=DC^;
   if BL <> nil then ST77XXFramebuffer.TFT.BL:=BL^ else ST77XXFramebuffer.TFT.BL:=GPIO_INFO_UNKNOWN;
   ST77XXFramebuffer.TFT.Initialize:=ST77XXTFTFramebufferInitialize;
   ST77XXFramebuffer.TFT.Deinitialize:=ST77XXTFTFramebufferDeinitialize;
   ST77XXFramebuffer.TFT.GetDefaults:=ST77XXTFTFramebufferGetDefaults;
   ST77XXFramebuffer.TFT.SetWriteAddress:=ST77XXTFTFramebufferSetWriteAddress;
   ST77XXFramebuffer.TFT.WriteMemory:=ST77XXTFTFramebufferWriteMemory;
   {Driver}
   ST77XXFramebuffer.TFT.Width:=Width;
   ST77XXFramebuffer.TFT.Height:=Height;
   ST77XXFramebuffer.TFT.Rotation:=Rotation;
   if (Rotation = FRAMEBUFFER_ROTATION_90) or (Rotation = FRAMEBUFFER_ROTATION_270) then
    begin
     ST77XXFramebuffer.TFT.Width:=Height;
     ST77XXFramebuffer.TFT.Height:=Width;
    end;
   ST77XXFramebuffer.TFT.DirtyY1:=Height - 1;
   ST77XXFramebuffer.TFT.DirtyY2:=0;
   ST77XXFramebuffer.TFT.Ready:=True;
   ST77XXFramebuffer.TFT.Lock:=INVALID_HANDLE_VALUE;
   ST77XXFramebuffer.TFT.Timer:=INVALID_HANDLE_VALUE;
   ST77XXFramebuffer.TFT.FrameRate:=TFT_FRAMEBUFFER_FRAME_RATE_DEFAULT;
   ST77XXFramebuffer.TFT.TransferSize:=LongWord(-1);
   if SPIDeviceGetProperties(SPI,@SPIProperties) = ERROR_SUCCESS then
    begin
     if SPIProperties.MaxSize <> 0 then ST77XXFramebuffer.TFT.TransferSize:=SPIProperties.MaxSize;
    end;

   {Setup Flags}
   {Nothing}

   {Register Framebuffer}
   Status:=FramebufferDeviceRegister(@ST77XXFramebuffer.TFT.Framebuffer);
   if Status = ERROR_SUCCESS then
    begin
     {Allocate Framebuffer}
     Status:=FramebufferDeviceAllocate(@ST77XXFramebuffer.TFT.Framebuffer,nil);
     if Status = ERROR_SUCCESS then
      begin
       {Return Result}
       Result:=PFramebufferDevice(ST77XXFramebuffer);
      end
     else
      begin
       if DEVICE_LOG_ENABLED then DeviceLogError(nil,'ST77XX: Failed to allocate new framebuffer device: ' + ErrorToString(Status));

       {Deregister Framebuffer}
       FramebufferDeviceDeregister(@ST77XXFramebuffer.TFT.Framebuffer);

       {Destroy Framebuffer}
       FramebufferDeviceDestroy(@ST77XXFramebuffer.TFT.Framebuffer);
      end;
    end
   else
    begin
     if DEVICE_LOG_ENABLED then DeviceLogError(nil,'ST77XX: Failed to register new framebuffer device: ' + ErrorToString(Status));

     {Destroy Framebuffer}
     FramebufferDeviceDestroy(@ST77XXFramebuffer.TFT.Framebuffer);
    end;
  end
 else
  begin
   if DEVICE_LOG_ENABLED then DeviceLogError(nil,'ST77XX: Failed to create new framebuffer device');
  end;
end;

{==============================================================================}

function ST77XXFramebufferDestroy(Framebuffer:PFramebufferDevice):LongWord;
{Release, deregister and destroy an ST77XX Framebuffer device created by this driver}
{Framebuffer: The Framebuffer device to destroy}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Framebuffer}
 if Framebuffer = nil then Exit;

 {$IF DEFINED(ST77XX_DEBUG) or DEFINED(FRAMEBUFFER_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'ST77XX: Framebuffer Destroy');
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
       if DEVICE_LOG_ENABLED then DeviceLogError(nil,'ST77XX: Failed to destroy framebuffer device: ' + ErrorToString(Result));
      end;
    end
   else
    begin
     if DEVICE_LOG_ENABLED then DeviceLogError(nil,'ST77XX: Failed to deregister framebuffer device: ' + ErrorToString(Result));
    end;
  end
 else
  begin
   if DEVICE_LOG_ENABLED then DeviceLogError(nil,'ST77XX: Failed to release framebuffer device: ' + ErrorToString(Result));
  end;
end;

{==============================================================================}
{==============================================================================}
{ST77XX Framebuffer Functions}
function ST77XXFramebufferBlank(Framebuffer:PFramebufferDevice;Blank:Boolean):LongWord;
{Implementation of FramebufferDeviceBlank API for ST77XX}
{Note: Not intended to be called directly by applications, use FramebufferDeviceBlank instead}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IF DEFINED(ST77XX_DEBUG) or DEFINED(FRAMEBUFFER_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'ST77XX: Framebuffer Blank (Blank=' + BooleanToString(Blank) + ')');
 {$ENDIF}

 {Check Blank}
 if Blank then
  begin
   {Display Off}
   Result:=ST77XXWriteCommand(PTFTFramebuffer(Framebuffer),ST77XX_DISPOFF);
  end
 else
  begin
   {Display On}
   Result:=ST77XXWriteCommand(PTFTFramebuffer(Framebuffer),ST77XX_DISPON);
  end;
end;

{==============================================================================}

function ST77XXFramebufferSetBacklight(Framebuffer:PFramebufferDevice;Brightness:LongWord):LongWord;
{Implementation of FramebufferDeviceSetBacklight API for ST77XX}
{Note: Not intended to be called directly by applications, use FramebufferDeviceSetBacklight instead}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IF DEFINED(ST77XX_DEBUG) or DEFINED(FRAMEBUFFER_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'ST77XX: Framebuffer Set Backlight (Brightness=' + IntToStr(Brightness) + ')');
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
{ST77XX TFTFramebuffer Functions}
function ST77XXTFTFramebufferInitialize(Framebuffer:PTFTFramebuffer;Defaults:PFramebufferProperties):LongWord;
{Implementation of TFTFramebufferInitialize API for ST77XX}
{Note: Not intended to be called directly by applications}
var
 Value:Byte;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Framebuffer}
 if Framebuffer = nil then Exit;

 {$IF DEFINED(ST77XX_DEBUG) or DEFINED(FRAMEBUFFER_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'ST77XX: Framebuffer Initialize');
 {$ENDIF}

 {Check Defaults}
 if Defaults = nil then Exit;

 {Set the SPI clock rate}
 if SPIDeviceSetClockRate(Framebuffer.SPI,Framebuffer.ChipSelect,ST77XX_SPI_RATE) <> ERROR_SUCCESS then
  begin
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;

 {Start the SPI device}
 if SPIDeviceStart(Framebuffer.SPI,SPI_MODE_4WIRE,ST77XX_SPI_RATE,SPI_CLOCK_PHASE_LOW,SPI_CLOCK_POLARITY_LOW) <> ERROR_SUCCESS then
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
 {b"\x01\x80\x96"  # _SWRESET and Delay 150ms}
 ST77XXWriteCommand(Framebuffer,ST77XX_SWRESET);
 Sleep(150);

 {0x55 = 16 bits/pixel / 0x66 = 18 bits/pixel  b"\x3A\x81\x55\x0A"  # _COLMOD and Delay 10ms}
 ST77XXWriteCommand(Framebuffer,ST77XX_COLMOD);
 ST77XXWriteData(Framebuffer,ST77XX_COLOR_MODE_16bit);
 Sleep(10);

 {Memory access control}
 ST77XXWriteCommand(Framebuffer,ST77XX_MADCTL);
 Value:=0;
 {RGB/BGR}
 if Defaults.Order = FRAMEBUFFER_ORDER_RGB then
  begin
   Value:=Value or ST77XX_MADCTL_RGB;
  end
 else if Defaults.Order = FRAMEBUFFER_ORDER_BGR then
  begin
   Value:=Value or ST77XX_MADCTL_BGR;
  end;

 {Horizontal/Vertical memory direction (Rotation)}
 if Defaults.Rotation = FRAMEBUFFER_ROTATION_0 then
  begin
   Value:=Value or ST77XX_MADCTL_MX or ST77XX_MADCTL_MY or ST77XX_MADCTL_RGB;
  end
 else if Defaults.Rotation = FRAMEBUFFER_ROTATION_90 then
  begin
   Value:=Value or ST77XX_MADCTL_MY or ST77XX_MADCTL_MV or ST77XX_MADCTL_RGB;
  end
 else if Defaults.Rotation = FRAMEBUFFER_ROTATION_180 then
  begin
   Value:=Value or ST77XX_MADCTL_RGB;
  end
 else if Defaults.Rotation = FRAMEBUFFER_ROTATION_270 then
  begin
   Value:=Value or ST77XX_MADCTL_MX or ST77XX_MADCTL_MV or ST77XX_MADCTL_RGB;
  end;
 ST77XXWriteData(Framebuffer,Value);

 ST77XXWriteCommand(Framebuffer,ST77XX_INVON);
 Sleep(10);

 ST77XXWriteCommand(Framebuffer,ST77XX_NORON);
 Sleep(10);

 {b"\x11\x80\xFF"  # _SLPOUT and Delay 500ms}
 ST77XXWriteCommand(Framebuffer,ST77XX_SLPOUT);   // Sleep out
 Sleep(10);

 ST77XXWriteCommand(Framebuffer,ST77XX_DISPON);
 Sleep(10);

 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function ST77XXTFTFramebufferDeinitialize(Framebuffer:PTFTFramebuffer):LongWord;
{Implementation of TFTFramebufferDeinitialize API for ST77XX}
{Note: Not intended to be called directly by applications}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Framebuffer}
 if Framebuffer = nil then Exit;

 {$IF DEFINED(ST77XX_DEBUG) or DEFINED(FRAMEBUFFER_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'ST77XX: Framebuffer Initialize');
 {$ENDIF}

 {Display Off}
 ST77XXWriteCommand(Framebuffer,ST77XX_DISPOFF);

 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function ST77XXTFTFramebufferGetDefaults(Framebuffer:PTFTFramebuffer;Properties,Defaults:PFramebufferProperties):LongWord;
{Implementation of TFTFramebufferGetDefaults API for ST77XX}
{Note: Not intended to be called directly by applications}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Framebuffer}
 if Framebuffer = nil then Exit;

 {$IF DEFINED(ST77XX_DEBUG) or DEFINED(FRAMEBUFFER_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'ST77XX: Framebuffer Get Defaults');
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

function ST77XXTFTFramebufferSetWriteAddress(Framebuffer:PTFTFramebuffer;X1,Y1,X2,Y2:LongWord):LongWord;
{Implementation of TFTFramebufferSetWriteAddress API for ST77XX}
{Note: Not intended to be called directly by applications}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Framebuffer}
 if Framebuffer = nil then Exit;

 {$IF DEFINED(ST77XX_DEBUG) or DEFINED(FRAMEBUFFER_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'ST77XX: Set Write Address (X1=' + IntToStr(X1) + ' Y1=' + IntToStr(Y1) + ' X2=' + IntToStr(X2) + ' Y2=' + IntToStr(Y2) + ')');
 {$ENDIF}

 {Column address set}
 ST77XXWriteCommand(Framebuffer,ST77XX_CASET);
 ST77XXWriteDataEx(Framebuffer,[X1 shr 8,X1 and $FF,X2 shr 8,X2 and $FF]);

 {Row address set}
 ST77XXWriteCommand(Framebuffer,ST77XX_RASET);
 ST77XXWriteDataEx(Framebuffer,[(Y1+35) shr 8,(Y1+35) and $FF,(Y2+35) shr 8,(Y2+35) and $FF]);

 {Memory write}
 ST77XXWriteCommand(Framebuffer,ST77XX_RAMWR);

 {Set DC High}
 GPIODeviceOutputSet(Framebuffer.DC.GPIO,Framebuffer.DC.Pin,GPIO_LEVEL_HIGH);

 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function ST77XXTFTFramebufferWriteMemory(Framebuffer:PTFTFramebuffer;Address:PtrUInt;Size:LongWord):LongWord;
{Implementation of TFTFramebufferWriteMemory API for ST77XX}
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

 {$IF DEFINED(ST77XX_DEBUG) or DEFINED(FRAMEBUFFER_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'ST77XX: Write Memory (Address=' + AddrToHex(Address) + ' Size=' + IntToStr(Size) + ')');
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
{ST77XX Helper Functions}
function ST77XXWriteCommand(Framebuffer:PTFTFramebuffer;Value:Byte):LongWord;
{Write a single command value to the ST77XX}
var
 Count:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Framebuffer}
 if Framebuffer = nil then Exit;

 {$IF DEFINED(ST77XX_DEBUG) or DEFINED(FRAMEBUFFER_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'ST77XX: Write Command (Value=' + IntToHex(Value,2) + ')');
 {$ENDIF}

 {Set DC Low}
 GPIODeviceOutputSet(Framebuffer.DC.GPIO,Framebuffer.DC.Pin,GPIO_LEVEL_LOW);

 {Write Command}
 Result:=SPIDeviceWrite(Framebuffer.SPI,Framebuffer.ChipSelect,@Value,SizeOf(Byte),SPI_TRANSFER_NONE,Count);
end;

{==============================================================================}

function ST77XXWriteCommandEx(Framebuffer:PTFTFramebuffer;const Values:array of Byte):LongWord;
{Write multiple command values to the ST77XX}
var
 Count:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Framebuffer}
 if Framebuffer = nil then Exit;

 {$IF DEFINED(ST77XX_DEBUG) or DEFINED(FRAMEBUFFER_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'ST77XX: Write Command Ex (Values size=' + IntToStr(Length(Values)) + ')');
 {$ENDIF}

 {Set DC Low}
 GPIODeviceOutputSet(Framebuffer.DC.GPIO,Framebuffer.DC.Pin,GPIO_LEVEL_LOW);

 {Write Command}
 Result:=SPIDeviceWrite(Framebuffer.SPI,Framebuffer.ChipSelect,@Values[0],Length(Values),SPI_TRANSFER_NONE,Count);
end;

{==============================================================================}

function ST77XXWriteData(Framebuffer:PTFTFramebuffer;Value:Byte):LongWord;
{Write a single data value to the ST77XX}
var
 Count:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Framebuffer}
 if Framebuffer = nil then Exit;

 {$IF DEFINED(ST77XX_DEBUG) or DEFINED(FRAMEBUFFER_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'ST77XX: Write Data (Value=' + IntToHex(Value,2) + ')');
 {$ENDIF}

 {Set DC High}
 GPIODeviceOutputSet(Framebuffer.DC.GPIO,Framebuffer.DC.Pin,GPIO_LEVEL_HIGH);

 {Write Data}
 Result:=SPIDeviceWrite(Framebuffer.SPI,Framebuffer.ChipSelect,@Value,SizeOf(Byte),SPI_TRANSFER_NONE,Count);
end;

{==============================================================================}

function ST77XXWriteDataEx(Framebuffer:PTFTFramebuffer;const Values:array of Byte):LongWord;
{Write multiple data values to the ST77XX}
var
 Count:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Framebuffer}
 if Framebuffer = nil then Exit;

 {$IF DEFINED(ST77XX_DEBUG) or DEFINED(FRAMEBUFFER_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'ST77XX: Write Data Ex (Values size=' + IntToStr(Length(Values)) + ')');
 {$ENDIF}

 {Set DC High}
 GPIODeviceOutputSet(Framebuffer.DC.GPIO,Framebuffer.DC.Pin,GPIO_LEVEL_HIGH);

 {Write Data}
 Result:=SPIDeviceWrite(Framebuffer.SPI,Framebuffer.ChipSelect,@Values[0],Length(Values),SPI_TRANSFER_NONE,Count);
end;

{==============================================================================}
{==============================================================================}


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

