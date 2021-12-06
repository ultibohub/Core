{
Pimoroni HyperPixel display driver.

Copyright (C) 2021 - SoftOz Pty Ltd.

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

  HyperPixel 4.0" Drivers - https://github.com/pimoroni/hyperpixel4

  HyperPixel 2.0" Round - https://github.com/pimoroni/hyperpixel2r

  HyperPixel 2" Round Touch Driver - https://github.com/pimoroni/hyperpixel2r-python


References
==========


HyperPixel
==========

 The HyperPixel is a TFT display produced by Pimoroni for use with the Raspberry Pi.

 It uses the DPI (Display Parallel Interface) which is directly supported by the
 VideoCore GPU and means that unlike other TFT displays using SPI or similar
 interfaces it supports hardware accelerated graphics in the same way as HDMI or
 DSI displays.

 The HyperPixel is available in several versions including a 4.0 inch rectangular,
 a 4.0 inch square and 2.1 inch round. Each version requires a slightly different set
 of instructions to initialize it and this unit provides the correct sequence for
 each of these displays.

 To use this unit simply include it in your application and then call the function
 HyperPixelInitialize() as early as possible passing the appropriate model as the only
 parameter to the function. Once initialized successfully the HyperPixel display will
 be available via the standard framebuffer functions and also via the VC4 accelerated
 graphics options such as OpenVG, OpenGLES, OpenMAX and MMAL.

 It is important to remember that no other devices should be enabled while using the
 HyperPixel display as it consumes almost all of the available GPIO pins. So it is
 not possible to use UART, I2C, SPI, PWM etc when the HyperPixel is installed.

 The touch screen controller varies between models and this unit creates and initializes
 the appropriate one for the model selected. All touch screen devices are I2C based
 however as it is not possible to utilize the hardware I2C controller (see note above)
 a software I2C host is created to provide access to the touch screen.

 The touch events generated from the device can be read from the normal touch or mouse
 API functions in much the same way as any other touch or mouse device.

 In order to enable the HyperPixel display you need to add the appropriate settings to
 your config.txt file as follows:

 For HyperPixel 4.0 Rectangular (800 x 480)
 ------------------------------------------

 enable_dpi_lcd=1
 dpi_group=2
 dpi_mode=87
 dpi_output_format=0x7f216
 dpi_timings=480 0 10 16 59 800 0 15 113 15 0 0 0 60 0 32000000 6

 For HyperPixel 4.0 Square (720 x 720)
 -------------------------------------

 enable_dpi_lcd=1
 dpi_group=2
 dpi_mode=87
 dpi_output_format=0x7f226
 dpi_timings=720 0 15 15 15 720 0 10 10 10 0 0 0 60 0 35113500 6

 For HyperPixel 4.0 Square (2021 or later) (720 x 720)
 -----------------------------------------------------

 enable_dpi_lcd=1
 dpi_group=2
 dpi_mode=87
 dpi_output_format=0x5f026
 dpi_timings=720 0 20 20 40 720 0 15 15 15 0 0 0 60 0 36720000 4

 For HyperPixel 2.1 Round (480 x 480)
 ------------------------------------

 enable_dpi_lcd=1
 dpi_group=2
 dpi_mode=87
 dpi_output_format=0x7f216
 dpi_timings=480 0 10 16 55 480 0 15 60 15 0 0 0 60 0 19200000 6


 The display can be rotated to the desired position by adding the display_lcd_rotate
 setting to the config.txt file as below, please see the official documentation at
 https://www.raspberrypi.com/documentation/computers/config_txt.html for more details.

 No Rotation
 -----------

 display_lcd_rotate=0

 Rotate 90 degrees clockwise
 ---------------------------

 display_lcd_rotate=1

 Rotate 180 degrees clockwise
 ----------------------------

 display_lcd_rotate=2

 Rotate 270 degrees clockwise
 ----------------------------

 display_lcd_rotate=3

 Horizontal flip
 ---------------

 display_lcd_rotate=0x10000

 Vertical flip
 -------------

 display_lcd_rotate=0x20000


}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit HyperPixel;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,GPIO,I2C,Touch,SysUtils; //,I2CGPIO,GoodixTouch,FT5x06Touch; //To Do

{==============================================================================}
{Global definitions}
{$INCLUDE ..\core\GlobalDefines.inc}

//To Do //How to handle displays without Touch ? Can we detect, is it a parameter to pass to Init ?

{==============================================================================}
const
 {HyperPixel specific constants}
 {HyperPixel model constants}
 HYPERPIXEL40_RECTANGLE  = 0;
 HYPERPIXEL40_SQUARE     = 1;
 HYPERPIXEL40_SQUARE2021 = 2;
 HYPERPIXEL21_ROUND      = 3;

 {HyperPixel GPIO pins (All Models)}
 HYPERPIXEL_GPIO_PIN_FUNCTION = GPIO_FUNCTION_ALT2;
 HYPERPIXEL_GPIO_PIN_PULL = GPIO_PULL_NONE;
 HYPERPIXEL_GPIO_PIN_COUNT = 22;
 HYPERPIXEL_GPIO_PINS:array[0..HYPERPIXEL_GPIO_PIN_COUNT - 1] of LongWord = (
  GPIO_PIN_0,
  GPIO_PIN_1,
  GPIO_PIN_2,
  GPIO_PIN_3,
  GPIO_PIN_4,
  GPIO_PIN_5,
  GPIO_PIN_6,
  GPIO_PIN_7,
  GPIO_PIN_8,
  GPIO_PIN_9,

  GPIO_PIN_12,
  GPIO_PIN_13,
  GPIO_PIN_14,
  GPIO_PIN_15,
  GPIO_PIN_16,
  GPIO_PIN_17,

  GPIO_PIN_20,
  GPIO_PIN_21,
  GPIO_PIN_22,
  GPIO_PIN_23,
  GPIO_PIN_24,
  GPIO_PIN_25);

 {HyperPixel SPI pins (All Models)} {Used during initialization}
 HYPERPIXEL_SPI_FUNCTION = GPIO_FUNCTION_OUT;
 HYPERPIXEL_SPI_PULL = GPIO_PULL_NONE;
 HYPERPIXEL_SPI_CLK = GPIO_PIN_27;
 HYPERPIXEL_SPI_MOSI = GPIO_PIN_26;
 HYPERPIXEL_SPI_CS = GPIO_PIN_18;
 HYPERPIXEL_SPI_DELAY = 100; {Clock pulse time in microseconds}
 HYPERPIXEL_SPI_WAIT = 120;  {Wait time in milliseconds before display on}

 HYPERPIXEL_SPI_DELAY_ALT = 10;{Clock pulse time in microseconds (Rectangle model)}
 HYPERPIXEL_SPI_WAIT_ALT = 200;  {Wait time in milliseconds before display on (Rectangle model)}

 {HyperPixel Touch I2C pins (All Models)} {Used to create a software I2C device}
 HYPERPIXEL_I2C_SDA = GPIO_PIN_10;
 HYPERPIXEL_I2C_SCL = GPIO_PIN_11;
 HYPERPIXEL_I2C_DELAY = 4;

 {HyperPixel Backlight pins (All Models)} {GPIO for backlight control}
 HYPERPIXEL_BACKLIGHT_FUNCTION = GPIO_FUNCTION_OUT;
 HYPERPIXEL_BACKLIGHT_PULL = GPIO_PULL_NONE;
 HYPERPIXEL_BACKLIGHT_PIN = GPIO_PIN_19;

 {HyperPixel Touch interrupt pins (All Models)} {GPIO for touch event registration}
 HYPERPIXEL_TOUCH_INTERRUPT_FUNCTION = GPIO_FUNCTION_IN;
 HYPERPIXEL_TOUCH_INTERRUPT_PULL = GPIO_PULL_UP;
 HYPERPIXEL_TOUCH_INTERRUPT_PIN = GPIO_PIN_27;

{==============================================================================}
type
 {HyperPixel specific types}
 {Software SPI Data}
 PHyperPixelData = ^THyperPixelData;
 THyperPixelData = record
  Device:PGPIODevice; {The GPIO device to use}
  FSEL:LongWord;      {The GPIO function select value}
  PULL:LongWord;      {The GPIO pull select value}
  CLK:LongWord;       {The CLK pin for software SPI}
  MOSI:LongWord;      {The MOSI pin for software SPI}
  CS:LongWord;        {The CS pin for software SPI}
  Wait:LongWord;      {Wait time in milliseconds before display on}
  Delay:LongWord;     {Clock delay time in microseconds}
 end;

{==============================================================================}
{var}
 {HyperPixel specific variables}

{==============================================================================}
{Initialization Functions}

{==============================================================================}
{HyperPixel Functions}
function HyperPixelInitialize(Model:LongWord):LongWord;

{==============================================================================}
{HyperPixel Helper Functions}
function HyperPixelSPISetup(Data:PHyperPixelData):LongWord;
function HyperPixelSPICleanup(Data:PHyperPixelData):LongWord;

procedure HyperPixelSPIWriteBit(Data:PHyperPixelData;Value:Byte);
procedure HyperPixelSPIWriteByte(Data:PHyperPixelData;Value:Byte);
procedure HyperPixelSPIWriteRegister(Data:PHyperPixelData;Reg,Value:Byte);

procedure HyperPixelSPISoftReset(Data:PHyperPixelData);
procedure HyperPixelSPISelectPage(Data:PHyperPixelData;Number:Byte);

procedure HyperPixelSPIWriteBits(Data:PHyperPixelData;Value,Count:Word);
procedure HyperPixelSPIWriteCommand(Data:PHyperPixelData;Command:Word);

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
{var}
 {HyperPixel specific variables}

{==============================================================================}
{==============================================================================}
{Forward Declarations}
function HyperPixelInit40Rectangle:LongWord; forward;
function HyperPixelInit40Square:LongWord; forward;
function HyperPixelInit40Square2021:LongWord; forward;
function HyperPixelInit21Round:LongWord; forward;

{==============================================================================}
{==============================================================================}
{Initialization Functions}

{==============================================================================}
{==============================================================================}
{HyperPixel Functions}
function HyperPixelInitialize(Model:LongWord):LongWord;
{Initialize an attached HyperPixel display including GPIO pin configuration}
{Model: The model of display to be initialized (eg HYPERPIXEL40_SQUARE)}
{Return: ERROR_SUCCESS on successful completion or another error code on failure}

{Note: No attempt is made to determine if the model specified matches with the
 attached display. It is the users responsibility to specify the correct model}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Model}
 if Model > HYPERPIXEL21_ROUND then Exit;

 {Perform Initialization}
 case Model of
  HYPERPIXEL40_RECTANGLE:Result:=HyperPixelInit40Rectangle;
  HYPERPIXEL40_SQUARE:Result:=HyperPixelInit40Square;
  HYPERPIXEL40_SQUARE2021:Result:=HyperPixelInit40Square2021;
  HYPERPIXEL21_ROUND:Result:=HyperPixelInit21Round;
 end;
end;

{==============================================================================}
{==============================================================================}
{HyperPixel Helper Functions}
function HyperPixelSPISetup(Data:PHyperPixelData):LongWord;
{Setup the software SPI communication used to initialize the HyperPixel display}
{Data: A pointer to a THyperPixelData structure containing the configuration}
{Return: ERROR_SUCCESS on successful completion or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Data}
 if Data = nil then Exit;

 {Setup Pin Functions}
 Result:=GPIODeviceFunctionSelect(Data.Device,Data.CLK,Data.FSEL);
 if Result <> ERROR_SUCCESS then Exit;

 Result:=GPIODeviceFunctionSelect(Data.Device,Data.MOSI,Data.FSEL);
 if Result <> ERROR_SUCCESS then Exit;

 Result:=GPIODeviceFunctionSelect(Data.Device,Data.CS,Data.FSEL);
 if Result <> ERROR_SUCCESS then Exit;

 {Setup Pin Pulls}
 Result:=GPIODevicePullSelect(Data.Device,Data.CLK,Data.PULL);
 if Result <> ERROR_SUCCESS then Exit;

 Result:=GPIODevicePullSelect(Data.Device,Data.MOSI,Data.PULL);
 if Result <> ERROR_SUCCESS then Exit;

 Result:=GPIODevicePullSelect(Data.Device,Data.CS,Data.PULL);
 if Result <> ERROR_SUCCESS then Exit;

 {Reset CS}
 GPIODeviceOutputSet(Data.Device,Data.CS,GPIO_LEVEL_HIGH);

 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function HyperPixelSPICleanup(Data:PHyperPixelData):LongWord;
{Cleanup the software SPI after HyperPixel initialization}
{Data: A pointer to the THyperPixelData structure used to configure the SPI}
{Return: ERROR_SUCCESS on successful completion or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Data}
 if Data = nil then Exit;

 {Restore the state of the Touch interrupt pin (Used for CLK)}
 Result:=GPIODeviceFunctionSelect(Data.Device,Data.CLK,HYPERPIXEL_TOUCH_INTERRUPT_FUNCTION);
 if Result <> ERROR_SUCCESS then Exit;

 Result:=GPIODevicePullSelect(Data.Device,Data.CLK,HYPERPIXEL_TOUCH_INTERRUPT_PULL);
 if Result <> ERROR_SUCCESS then Exit;

 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

procedure HyperPixelSPIWriteBit(Data:PHyperPixelData;Value:Byte);
{Write a single bit to the software SPI}
{Data: A pointer to the THyperPixelData structure used to configure the SPI}
{Value: The bit value to write (0 or 1)}

{Note: Used primarily for HyperPixel 4.0 Rectangle}
var
 Level:LongWord;
begin
 {}
 {Check Data}
 if Data = nil then Exit;

 {Get Level}
 Level:=GPIO_LEVEL_LOW;
 if Value <> 0 then Level:=GPIO_LEVEL_HIGH;

 {Set MOSI}
 GPIODeviceOutputSet(Data.Device,Data.MOSI,Level);

 {Pulse CLK}
 MicrosecondDelay(Data.Delay);
 GPIODeviceOutputSet(Data.Device,Data.CLK,GPIO_LEVEL_LOW);
 MicrosecondDelay(Data.Delay);
 GPIODeviceOutputSet(Data.Device,Data.CLK,GPIO_LEVEL_HIGH);
 MicrosecondDelay(Data.Delay);
end;

{==============================================================================}

procedure HyperPixelSPIWriteByte(Data:PHyperPixelData;Value:Byte);
{Write a byte to the software SPI}
{Data: A pointer to the THyperPixelData structure used to configure the SPI}
{Value: The byte value to write}

{Note: Used primarily for HyperPixel 4.0 Rectangle}
var
 Bit:LongWord;
 Mask:LongWord;
 Level:LongWord;
begin
 {}
 {Check Data}
 if Data = nil then Exit;

 Mask:=1 shl 7;
 for Bit:=0 to 7 do
  begin
   {Get Bit}
   Level:=GPIO_LEVEL_LOW;
   if (Value and Mask) <> 0 then Level:=GPIO_LEVEL_HIGH;

   {Set MOSI}
   GPIODeviceOutputSet(Data.Device,Data.MOSI,Level);

   {Shift Bits}
   Mask:=Mask shr 1;

   {Pulse CLK}
   MicrosecondDelay(Data.Delay);
   GPIODeviceOutputSet(Data.Device,Data.CLK,GPIO_LEVEL_LOW);
   MicrosecondDelay(Data.Delay);
   GPIODeviceOutputSet(Data.Device,Data.CLK,GPIO_LEVEL_HIGH);
   MicrosecondDelay(Data.Delay);
  end;
end;

{==============================================================================}

procedure HyperPixelSPIWriteRegister(Data:PHyperPixelData;Reg,Value:Byte);
{Perform a register select followed by a register write using the software SPI}
{Data: A pointer to the THyperPixelData structure used to configure the SPI}
{Reg: The register to be selected}
{Value: The byte value to write}

{Note: Used primarily for HyperPixel 4.0 Rectangle}
begin
 {}
 {Check Data}
 if Data = nil then Exit;

 {Set CS}
 GPIODeviceOutputSet(Data.Device,Data.CS,GPIO_LEVEL_LOW);

 {Register Select}
 HyperPixelSPIWriteBit(Data,0);
 HyperPixelSPIWriteByte(Data,Reg);

 {Register Write}
 HyperPixelSPIWriteBit(Data,1);
 HyperPixelSPIWriteByte(Data,Value);

 {Reset CS}
 GPIODeviceOutputSet(Data.Device,Data.CS,GPIO_LEVEL_HIGH);
end;

{==============================================================================}

procedure HyperPixelSPISoftReset(Data:PHyperPixelData);
{Perform a software reset of the HyperPixel display}
{Data: A pointer to the THyperPixelData structure used to configure the SPI}

{Note: Used primarily for HyperPixel 4.0 Rectangle}
begin
 {}
 {Check Data}
 if Data = nil then Exit;

 {Select Page}
 HyperPixelSPISelectPage(Data,0);

 {Soft Reset}
 HyperPixelSPIWriteRegister(Data,$01,$00);
 MillisecondDelay(Data.Wait);
end;

{==============================================================================}

procedure HyperPixelSPISelectPage(Data:PHyperPixelData;Number:Byte);
{Perform a page select operation to select a specific configuration page}
{Data: A pointer to the THyperPixelData structure used to configure the SPI}
{Number: The page number to select}

{Note: Used primarily for HyperPixel 4.0 Rectangle}
begin
 {}
 {Check Data}
 if Data = nil then Exit;

 {Set CS}
 GPIODeviceOutputSet(Data.Device,Data.CS,GPIO_LEVEL_LOW);

 {Register Select}
 HyperPixelSPIWriteBit(Data,0);
 HyperPixelSPIWriteByte(Data,$FF);

 {Register Write}
 HyperPixelSPIWriteBit(Data,1);
 HyperPixelSPIWriteByte(Data,$FF);

 {Register Write}
 HyperPixelSPIWriteBit(Data,1);
 HyperPixelSPIWriteByte(Data,$98);

 {Register Write}
 HyperPixelSPIWriteBit(Data,1);
 HyperPixelSPIWriteByte(Data,$06);

 {Register Write}
 HyperPixelSPIWriteBit(Data,1);
 HyperPixelSPIWriteByte(Data,$04);

 {Register Write}
 HyperPixelSPIWriteBit(Data,1);
 HyperPixelSPIWriteByte(Data,Number);

 {Reset CS}
 GPIODeviceOutputSet(Data.Device,Data.CS,GPIO_LEVEL_HIGH);
end;

{==============================================================================}

procedure HyperPixelSPIWriteBits(Data:PHyperPixelData;Value,Count:Word);
{Write a sequence of bits to the software SPI}
{Data: A pointer to the THyperPixelData structure used to configure the SPI}
{Value: The value containing the bits to be written}
{Count: The number of bits to write}

{Note: Used primarily for HyperPixel 2.1 Round and 4.0 Square}
var
 Bit:LongWord;
 Mask:LongWord;
 Level:LongWord;
begin
 {}
 {Check Data}
 if Data = nil then Exit;

 Mask:=1 shl (Count - 1);
 for Bit:=0 to Count - 1 do
  begin
   {Get Bit}
   Level:=GPIO_LEVEL_LOW;
   if (Value and Mask) <> 0 then Level:=GPIO_LEVEL_HIGH;

   {Set MOSI}
   GPIODeviceOutputSet(Data.Device,Data.MOSI,Level);

   {Shift Bits}
   Value:=Value shl 1;

   {Pulse CLK}
   GPIODeviceOutputSet(Data.Device,Data.CLK,GPIO_LEVEL_LOW);
   MicrosecondDelay(Data.Delay);
   GPIODeviceOutputSet(Data.Device,Data.CLK,GPIO_LEVEL_HIGH);
   MicrosecondDelay(Data.Delay);
  end;

 {Reset MOSI}
 GPIODeviceOutputSet(Data.Device,Data.MOSI,GPIO_LEVEL_LOW);
end;

{==============================================================================}

procedure HyperPixelSPIWriteCommand(Data:PHyperPixelData;Command:Word);
{Write a command to the software SPI}
{Data: A pointer to the THyperPixelData structure used to configure the SPI}
{Command: The command to be written}

{Note: Used primarily for HyperPixel 2.1 Round and 4.0 Square}
begin
 {}
 {Check Data}
 if Data = nil then Exit;

 {Set CS}
 GPIODeviceOutputSet(Data.Device,Data.CS,GPIO_LEVEL_LOW);

 {Write Bits}
 HyperPixelSPIWriteBits(Data,Command,9);

 {Reset CS}
 GPIODeviceOutputSet(Data.Device,Data.CS,GPIO_LEVEL_HIGH);
end;

{==============================================================================}
{==============================================================================}
{HyperPixel Internal Functions}
function HyperPixelInit40Rectangle:LongWord;
{Initialize an attached HyperPixel 4.0 Rectangle display}
var
 Count:LongWord;
 Data:THyperPixelData;
 I2CDevice:PI2CDevice;
 GPIODevice:PGPIODevice;
begin
 {}
 Result:=ERROR_OPERATION_FAILED;

 {Get the GPIO Device}
 GPIODevice:=GPIODeviceGetDefault;
 if GPIODevice = nil then Exit;

 {Setup GPIO Pins}
 for Count:=0 to HYPERPIXEL_GPIO_PIN_COUNT - 1 do
  begin
   {Setup Function}
   Result:=GPIODeviceFunctionSelect(GPIODevice,HYPERPIXEL_GPIO_PINS[Count],HYPERPIXEL_GPIO_PIN_FUNCTION);
   if Result <> ERROR_SUCCESS then Exit;

   {Setup Pull}
   Result:=GPIODevicePullSelect(GPIODevice,HYPERPIXEL_GPIO_PINS[Count],HYPERPIXEL_GPIO_PIN_PULL);
   if Result <> ERROR_SUCCESS then Exit;
  end;

 {Setup Software SPI}
 Data.Device:=GPIODevice;
 Data.FSEL:=HYPERPIXEL_SPI_FUNCTION;
 Data.PULL:=HYPERPIXEL_SPI_PULL;
 Data.CLK:=HYPERPIXEL_SPI_CLK;
 Data.MOSI:=HYPERPIXEL_SPI_MOSI;
 Data.CS:=HYPERPIXEL_SPI_CS;
 Data.Wait:=HYPERPIXEL_SPI_WAIT_ALT;
 Data.Delay:=HYPERPIXEL_SPI_DELAY_ALT;

 Result:=HyperPixelSPISetup(@Data);
 if Result <> ERROR_SUCCESS then Exit;

 {Send Initialization}
 {HyperPixelSPISoftReset(@Data);}
 HyperPixelSPISelectPage(@Data,1);
 {#interface mode}
 {# SEPT_SDIO = 0 (spi interface transfer through SDA pin)}
 {# SDO_STATUS = 1 (always output, but without output tri-state)}
 HyperPixelSPIWriteRegister(@Data,$08,$10);
 {# display control}
 {# VSPL = 1 (vertical sync polarity)}
 {# HSPL = 0 (horizontal sync polarity)}
 {# DPL = 0 (PCLK polarity)}
 {# EPL = 1 (data enable polarity)}
 HyperPixelSPIWriteRegister(@Data,$21,$0D);
 {# resolution control (0x02 = 480x800)}
 HyperPixelSPIWriteRegister(@Data,$30,$02);
 {# display inversion control (0x00 = column inversion)}
 HyperPixelSPIWriteRegister(@Data,$31,$00);
 {# power control}
 {# EXB1T = 0 (internal charge pump)}
 {# EXT_CPCK_SEL = 1 (pump clock control signal = output 2 x waveform)}
 {# BT = 0 (DDVDH / DDVDL voltage = VCI x 2 / VCI x -2)}
 HyperPixelSPIWriteRegister(@Data,$40,$10);
 {# power control}
 {# DDVDH_CLP = 5.6 (DDVDH clamp leve)}
 {# DDVDL_CLP = -5.6 (DDVDL clamp leve)}
 HyperPixelSPIWriteRegister(@Data,$41,$55);
 {# power control}
 {# VGH_CP = 2DDVDH - DDVDL (step up factor for VGH)}
 {# VGL_CP = DDVDL + VCL - VCIP (step up factor for VGL)}
 HyperPixelSPIWriteRegister(@Data,$42,$02);
 {# power control}
 {# VGH_CLPEN = 0 (disable VGH clamp level)}
 {# VGH_CLP = 9 (15.0 VGH clamp level - but this is disabled so not used?)}
 HyperPixelSPIWriteRegister(@Data,$43,$84);
 {# power control}
 {# VGL_CLPEN = 0 (disable VGL clamp level)}
 {# VGL_CLP = 9 (-11.0 VGL clamp level - but this is disabled so not used?)}
 HyperPixelSPIWriteRegister(@Data,$44,$84);

 {# power control}
 {# VREG1OUT voltage for positive gamma?}
 HyperPixelSPIWriteRegister(@Data,$50,$78);
 {# power control}
 {# VREG2OUT voltage for negative gamma?}
 HyperPixelSPIWriteRegister(@Data,$51,$78);

 {# power control}
 {# VCOM control}
 HyperPixelSPIWriteRegister(@Data,$52,$00);
 HyperPixelSPIWriteRegister(@Data,$53,$77);  {# doesn't match sample code}
 HyperPixelSPIWriteRegister(@Data,$57,$60);  {# in sample code, but not in our original code}
 {# source timing adjust}
 {# source SDT timing adjustment (0-63 time scales)}
 HyperPixelSPIWriteRegister(@Data,$60,$07);
 {# source timing adjust}
 {# source CR timing adjustment (0-63 time scales)}
 HyperPixelSPIWriteRegister(@Data,$61,$00);
 {# source timing adjust}
 {# source EQ timing adjustment (0-63 time scales)}
 HyperPixelSPIWriteRegister(@Data,$62,$08);

 {# source timing adjust}
 {# source PC timing adjustment (0-63 time scales)}
 HyperPixelSPIWriteRegister(@Data,$63,$00);
 {# positive gamma control}
 {# set the gray scale voltage to adjust the gamma characteristics of the panel}
 HyperPixelSPIWriteRegister(@Data,$A0,$00);
 HyperPixelSPIWriteRegister(@Data,$A1,$07);
 HyperPixelSPIWriteRegister(@Data,$A2,$0C);
 HyperPixelSPIWriteRegister(@Data,$A3,$0B);
 HyperPixelSPIWriteRegister(@Data,$A4,$03);
 HyperPixelSPIWriteRegister(@Data,$A5,$07);
 HyperPixelSPIWriteRegister(@Data,$A6,$06);
 HyperPixelSPIWriteRegister(@Data,$A7,$04);
 HyperPixelSPIWriteRegister(@Data,$A8,$08);
 HyperPixelSPIWriteRegister(@Data,$A9,$0C);
 HyperPixelSPIWriteRegister(@Data,$AA,$13);
 HyperPixelSPIWriteRegister(@Data,$AB,$06);
 HyperPixelSPIWriteRegister(@Data,$AC,$0D);
 HyperPixelSPIWriteRegister(@Data,$AD,$19);
 HyperPixelSPIWriteRegister(@Data,$AE,$10);
 HyperPixelSPIWriteRegister(@Data,$AF,$00);
 {# negative gamma control}
 {# set the gray scale voltage to adjust the gamma characteristics of the panel}
 HyperPixelSPIWriteRegister(@Data,$C0,$00);
 HyperPixelSPIWriteRegister(@Data,$C1,$07);
 HyperPixelSPIWriteRegister(@Data,$C2,$0C);
 HyperPixelSPIWriteRegister(@Data,$C3,$0B);
 HyperPixelSPIWriteRegister(@Data,$C4,$03);
 HyperPixelSPIWriteRegister(@Data,$C5,$07);
 HyperPixelSPIWriteRegister(@Data,$C6,$07);
 HyperPixelSPIWriteRegister(@Data,$C7,$04);
 HyperPixelSPIWriteRegister(@Data,$C8,$08);
 HyperPixelSPIWriteRegister(@Data,$C9,$0C);
 HyperPixelSPIWriteRegister(@Data,$CA,$13);
 HyperPixelSPIWriteRegister(@Data,$CB,$06);
 HyperPixelSPIWriteRegister(@Data,$CC,$0D);
 HyperPixelSPIWriteRegister(@Data,$CD,$18);
 HyperPixelSPIWriteRegister(@Data,$CE,$10);
 HyperPixelSPIWriteRegister(@Data,$CF,$00);
 HyperPixelSPISelectPage(@Data,6);
 {# GIP setting}
 HyperPixelSPIWriteRegister(@Data,$00,$20);
 HyperPixelSPIWriteRegister(@Data,$01,$0A);
 HyperPixelSPIWriteRegister(@Data,$02,$00);
 HyperPixelSPIWriteRegister(@Data,$03,$00);
 HyperPixelSPIWriteRegister(@Data,$04,$01);
 HyperPixelSPIWriteRegister(@Data,$05,$01);
 HyperPixelSPIWriteRegister(@Data,$06,$98);
 HyperPixelSPIWriteRegister(@Data,$07,$06);
 HyperPixelSPIWriteRegister(@Data,$08,$01);
 HyperPixelSPIWriteRegister(@Data,$09,$80);
 HyperPixelSPIWriteRegister(@Data,$0A,$00);
 HyperPixelSPIWriteRegister(@Data,$0B,$00);
 HyperPixelSPIWriteRegister(@Data,$0C,$01);
 HyperPixelSPIWriteRegister(@Data,$0D,$01);
 HyperPixelSPIWriteRegister(@Data,$0E,$00);
 HyperPixelSPIWriteRegister(@Data,$0F,$00);
 HyperPixelSPIWriteRegister(@Data,$10,$F0);
 HyperPixelSPIWriteRegister(@Data,$11,$F4);
 HyperPixelSPIWriteRegister(@Data,$12,$01);
 HyperPixelSPIWriteRegister(@Data,$13,$00);
 HyperPixelSPIWriteRegister(@Data,$14,$00);
 HyperPixelSPIWriteRegister(@Data,$15,$C0);
 HyperPixelSPIWriteRegister(@Data,$16,$08);
 HyperPixelSPIWriteRegister(@Data,$17,$00);
 HyperPixelSPIWriteRegister(@Data,$18,$00);
 HyperPixelSPIWriteRegister(@Data,$19,$00);
 HyperPixelSPIWriteRegister(@Data,$1A,$00);
 HyperPixelSPIWriteRegister(@Data,$1B,$00);
 HyperPixelSPIWriteRegister(@Data,$1C,$00);
 HyperPixelSPIWriteRegister(@Data,$1D,$00);
 HyperPixelSPIWriteRegister(@Data,$20,$01);
 HyperPixelSPIWriteRegister(@Data,$21,$23);
 HyperPixelSPIWriteRegister(@Data,$22,$45);
 HyperPixelSPIWriteRegister(@Data,$23,$67);
 HyperPixelSPIWriteRegister(@Data,$24,$01);
 HyperPixelSPIWriteRegister(@Data,$25,$23);
 HyperPixelSPIWriteRegister(@Data,$26,$45);
 HyperPixelSPIWriteRegister(@Data,$27,$67);
 HyperPixelSPIWriteRegister(@Data,$30,$11);
 HyperPixelSPIWriteRegister(@Data,$31,$11);
 HyperPixelSPIWriteRegister(@Data,$32,$00);
 HyperPixelSPIWriteRegister(@Data,$33,$EE);
 HyperPixelSPIWriteRegister(@Data,$34,$FF);
 HyperPixelSPIWriteRegister(@Data,$35,$BB);
 HyperPixelSPIWriteRegister(@Data,$36,$AA);
 HyperPixelSPIWriteRegister(@Data,$37,$DD);
 HyperPixelSPIWriteRegister(@Data,$38,$CC);
 HyperPixelSPIWriteRegister(@Data,$39,$66);
 HyperPixelSPIWriteRegister(@Data,$3A,$77);
 HyperPixelSPIWriteRegister(@Data,$3B,$22);
 HyperPixelSPIWriteRegister(@Data,$3C,$22);
 HyperPixelSPIWriteRegister(@Data,$3D,$22);
 HyperPixelSPIWriteRegister(@Data,$3E,$22);
 HyperPixelSPIWriteRegister(@Data,$3F,$22);
 HyperPixelSPIWriteRegister(@Data,$40,$22);
 HyperPixelSPIWriteRegister(@Data,$52,$10);  {# register doesn't exist on page 6?}
 HyperPixelSPIWriteRegister(@Data,$53,$10);  {# doesn't make sense, not valid according to datasheet}
 HyperPixelSPIWriteRegister(@Data,$54,$13);  {# doesn't make sense, not valid according to datasheet}
 HyperPixelSPISelectPage(@Data,7);
 {# enable VREG}
 HyperPixelSPIWriteRegister(@Data,$18,$1D);
 {# enable VGL_REG}
 HyperPixelSPIWriteRegister(@Data,$17,$22);
 HyperPixelSPIWriteRegister(@Data,$02,$77);  {# register doesn't exist on page 7?}
 HyperPixelSPIWriteRegister(@Data,$26,$B2);  {# register doesn't exist on page 7?}
 HyperPixelSPIWriteRegister(@Data,$E1,$79);  {# register doesn't exist on page 7?}
 HyperPixelSPISelectPage(@Data,0);
 {# set pixel format}
 {# DPI = 18-bits per pixel}
 HyperPixelSPIWriteRegister(@Data,$3A,$60);
 {# set tearing line effect}
 {# turns on the TE (tearing effect) output signal}
 HyperPixelSPIWriteRegister(@Data,$35,$00);

 {# turn off sleep mode}
 HyperPixelSPIWriteRegister(@Data,$11,$00);
 MillisecondDelay(Data.Wait);

 {# turn the display on}
 HyperPixelSPIWriteRegister(@Data,$29,$00);
 MillisecondDelay(Data.Wait);

 {Cleanup Software SPI}
 Result:=HyperPixelSPICleanup(@Data);
 if Result <> ERROR_SUCCESS then Exit;

 Result:=ERROR_OPERATION_FAILED;

 {Create I2CGPIO Device for Touch}
 //I2CDevice:=I2CGPIOCreate(GPIODevice,HYPERPIXEL_I2C_SCL,HYPERPIXEL_I2C_SDA,HYPERPIXEL_I2C_DELAY,0,False,False);
 //if I2CDevice = nil then Exit;

 {Create Goodix Touch Device}
 //To Do //Touch (Address 0x5d / Interrupt GPIO 27)
 //To Do //Backlight

 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function HyperPixelInit40Square:LongWord;
{Initialize an attached HyperPixel 4.0 Square display}

const
 HYPERPIXEL40_SQUARE_COMMANDS:array[0..255] of Word = (
  $00ff, $01ff, $0198, $0106, $0104, $0101, $0008, $0110,
  $0021, $0109, $0030, $0102, $0031, $0100, $0040, $0110,
  $0041, $0155, $0042, $0102, $0043, $0109, $0044, $0107,
  $0050, $0178, $0051, $0178, $0052, $0100, $0053, $016d,
  $0060, $0107, $0061, $0100, $0062, $0108, $0063, $0100,
  $00a0, $0100, $00a1, $0107, $00a2, $010c, $00a3, $010b,
  $00a4, $0103, $00a5, $0107, $00a6, $0106, $00a7, $0104,
  $00a8, $0108, $00a9, $010c, $00aa, $0113, $00ab, $0106,
  $00ac, $010d, $00ad, $0119, $00ae, $0110, $00af, $0100,
  $00c0, $0100, $00c1, $0107, $00c2, $010c, $00c3, $010b,
  $00c4, $0103, $00c5, $0107, $00c6, $0107, $00c7, $0104,
  $00c8, $0108, $00c9, $010c, $00ca, $0113, $00cb, $0106,
  $00cc, $010d, $00cd, $0118, $00ce, $0110, $00cf, $0100,
  $00ff, $01ff, $0198, $0106, $0104, $0106, $0000, $0120,
  $0001, $010a, $0002, $0100, $0003, $0100, $0004, $0101,
  $0005, $0101, $0006, $0198, $0007, $0106, $0008, $0101,
  $0009, $0180, $000a, $0100, $000b, $0100, $000c, $0101,
  $000d, $0101, $000e, $0100, $000f, $0100, $0010, $01f0,
  $0011, $01f4, $0012, $0101, $0013, $0100, $0014, $0100,
  $0015, $01c0, $0016, $0108, $0017, $0100, $0018, $0100,
  $0019, $0100, $001a, $0100, $001b, $0100, $001c, $0100,
  $001d, $0100, $0020, $0101, $0021, $0123, $0022, $0145,
  $0023, $0167, $0024, $0101, $0025, $0123, $0026, $0145,
  $0027, $0167, $0030, $0111, $0031, $0111, $0032, $0100,
  $0033, $01ee, $0034, $01ff, $0035, $01bb, $0036, $01aa,
  $0037, $01dd, $0038, $01cc, $0039, $0166, $003a, $0177,
  $003b, $0122, $003c, $0122, $003d, $0122, $003e, $0122,
  $003f, $0122, $0040, $0122, $0052, $0110, $0053, $0110,
  $00ff, $01ff, $0198, $0106, $0104, $0107, $0018, $011d,
  $0017, $0122, $0002, $0177, $0026, $01b2, $00e1, $0179,
  $00ff, $01ff, $0198, $0106, $0104, $0100, $003a, $0160,
  $0035, $0100, $0011, $0100, $FFFF, $0029, $0100, $FFFF);

var
 Command:Word;
 Count:LongWord;
 Data:THyperPixelData;
 I2CDevice:PI2CDevice;
 GPIODevice:PGPIODevice;
begin
 {}
 Result:=ERROR_OPERATION_FAILED;

 {Get the GPIO Device}
 GPIODevice:=GPIODeviceGetDefault;
 if GPIODevice = nil then Exit;

 {Setup GPIO Pins}
 for Count:=0 to HYPERPIXEL_GPIO_PIN_COUNT - 1 do
  begin
   {Setup Function}
   Result:=GPIODeviceFunctionSelect(GPIODevice,HYPERPIXEL_GPIO_PINS[Count],HYPERPIXEL_GPIO_PIN_FUNCTION);
   if Result <> ERROR_SUCCESS then Exit;

   {Setup Pull}
   Result:=GPIODevicePullSelect(GPIODevice,HYPERPIXEL_GPIO_PINS[Count],HYPERPIXEL_GPIO_PIN_PULL);
   if Result <> ERROR_SUCCESS then Exit;
  end;

 {Setup Software SPI}
 Data.Device:=GPIODevice;
 Data.FSEL:=HYPERPIXEL_SPI_FUNCTION;
 Data.PULL:=HYPERPIXEL_SPI_PULL;
 Data.CLK:=HYPERPIXEL_SPI_CLK;
 Data.MOSI:=HYPERPIXEL_SPI_MOSI;
 Data.CS:=HYPERPIXEL_SPI_CS;
 Data.Wait:=HYPERPIXEL_SPI_WAIT;
 Data.Delay:=HYPERPIXEL_SPI_DELAY;

 Result:=HyperPixelSPISetup(@Data);
 if Result <> ERROR_SUCCESS then Exit;

 {Send Initialization}
 for Count:=Low(HYPERPIXEL40_SQUARE_COMMANDS) to High(HYPERPIXEL40_SQUARE_COMMANDS) do
  begin
   Command:=HYPERPIXEL40_SQUARE_COMMANDS[Count];
   if Command = $FFFF then
    begin
     {Wait}
     MillisecondDelay(Data.Wait);
    end
   else
    begin
     HyperPixelSPIWriteCommand(@Data,Command);
    end;
  end;

 {Cleanup Software SPI}
 Result:=HyperPixelSPICleanup(@Data);
 if Result <> ERROR_SUCCESS then Exit;

 Result:=ERROR_OPERATION_FAILED;

 {Create I2CGPIO Device for Touch}
 //I2CDevice:=I2CGPIOCreate(GPIODevice,HYPERPIXEL_I2C_SCL,HYPERPIXEL_I2C_SDA,HYPERPIXEL_I2C_DELAY,0,False,False);
 //if I2CDevice = nil then Exit;

 {Create FT5x06 Touch Device}
 //To Do //Touch (Address 0x48 / Interrupt GPIO 27)
 //To Do //Backlight

 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function HyperPixelInit40Square2021:LongWord;
{Initialize an attached HyperPixel 4.0 Square (2021 or later) display}

const
 HYPERPIXEL40_SQUARE2021_COMMANDS:array[0..255] of Word = (
  $00ff, $01ff, $0198, $0106, $0104, $0101, $0008, $0110,
  $0021, $0109, $0030, $0102, $0031, $0100, $0040, $0110,
  $0041, $0155, $0042, $0102, $0043, $0109, $0044, $0107,
  $0050, $0178, $0051, $0178, $0052, $0100, $0053, $016d,
  $0060, $0107, $0061, $0100, $0062, $0108, $0063, $0100,
  $00a0, $0100, $00a1, $0107, $00a2, $010c, $00a3, $010b,
  $00a4, $0103, $00a5, $0107, $00a6, $0106, $00a7, $0104,
  $00a8, $0108, $00a9, $010c, $00aa, $0113, $00ab, $0106,
  $00ac, $010d, $00ad, $0119, $00ae, $0110, $00af, $0100,
  $00c0, $0100, $00c1, $0107, $00c2, $010c, $00c3, $010b,
  $00c4, $0103, $00c5, $0107, $00c6, $0107, $00c7, $0104,
  $00c8, $0108, $00c9, $010c, $00ca, $0113, $00cb, $0106,
  $00cc, $010d, $00cd, $0118, $00ce, $0110, $00cf, $0100,
  $00ff, $01ff, $0198, $0106, $0104, $0106, $0000, $0120,
  $0001, $010a, $0002, $0100, $0003, $0100, $0004, $0101,
  $0005, $0101, $0006, $0198, $0007, $0106, $0008, $0101,
  $0009, $0180, $000a, $0100, $000b, $0100, $000c, $0101,
  $000d, $0101, $000e, $0100, $000f, $0100, $0010, $01f0,
  $0011, $01f4, $0012, $0101, $0013, $0100, $0014, $0100,
  $0015, $01c0, $0016, $0108, $0017, $0100, $0018, $0100,
  $0019, $0100, $001a, $0100, $001b, $0100, $001c, $0100,
  $001d, $0100, $0020, $0101, $0021, $0123, $0022, $0145,
  $0023, $0167, $0024, $0101, $0025, $0123, $0026, $0145,
  $0027, $0167, $0030, $0111, $0031, $0111, $0032, $0100,
  $0033, $01ee, $0034, $01ff, $0035, $01bb, $0036, $01aa,
  $0037, $01dd, $0038, $01cc, $0039, $0166, $003a, $0177,
  $003b, $0122, $003c, $0122, $003d, $0122, $003e, $0122,
  $003f, $0122, $0040, $0122, $0052, $0110, $0053, $0110,
  $00ff, $01ff, $0198, $0106, $0104, $0107, $0018, $011d,
  $0017, $0122, $0002, $0177, $0026, $01b2, $00e1, $0179,
  $00ff, $01ff, $0198, $0106, $0104, $0100, $003a, $0160,
  $0035, $0100, $0011, $0100, $FFFF, $0029, $0100, $FFFF);

var
 Command:Word;
 Count:LongWord;
 Data:THyperPixelData;
 I2CDevice:PI2CDevice;
 GPIODevice:PGPIODevice;
begin
 {}
 Result:=ERROR_OPERATION_FAILED;

 {Get the GPIO Device}
 GPIODevice:=GPIODeviceGetDefault;
 if GPIODevice = nil then Exit;

 {Setup GPIO Pins}
 for Count:=0 to HYPERPIXEL_GPIO_PIN_COUNT - 1 do
  begin
   {Setup Function}
   Result:=GPIODeviceFunctionSelect(GPIODevice,HYPERPIXEL_GPIO_PINS[Count],HYPERPIXEL_GPIO_PIN_FUNCTION);
   if Result <> ERROR_SUCCESS then Exit;

   {Setup Pull}
   Result:=GPIODevicePullSelect(GPIODevice,HYPERPIXEL_GPIO_PINS[Count],HYPERPIXEL_GPIO_PIN_PULL);
   if Result <> ERROR_SUCCESS then Exit;
  end;

 {Setup Software SPI}
 Data.Device:=GPIODevice;
 Data.FSEL:=HYPERPIXEL_SPI_FUNCTION;
 Data.PULL:=HYPERPIXEL_SPI_PULL;
 Data.CLK:=HYPERPIXEL_SPI_CLK;
 Data.MOSI:=HYPERPIXEL_SPI_MOSI;
 Data.CS:=HYPERPIXEL_SPI_CS;
 Data.Wait:=HYPERPIXEL_SPI_WAIT;
 Data.Delay:=HYPERPIXEL_SPI_DELAY;

 Result:=HyperPixelSPISetup(@Data);
 if Result <> ERROR_SUCCESS then Exit;

 {Send Initialization}
 for Count:=Low(HYPERPIXEL40_SQUARE2021_COMMANDS) to High(HYPERPIXEL40_SQUARE2021_COMMANDS) do
  begin
   Command:=HYPERPIXEL40_SQUARE2021_COMMANDS[Count];
   if Command = $FFFF then
    begin
     {Wait}
     MillisecondDelay(Data.Wait);
    end
   else
    begin
     HyperPixelSPIWriteCommand(@Data,Command);
    end;
  end;

 {Cleanup Software SPI}
 Result:=HyperPixelSPICleanup(@Data);
 if Result <> ERROR_SUCCESS then Exit;

 Result:=ERROR_OPERATION_FAILED;

 {Create I2CGPIO Device for Touch}
 //I2CDevice:=I2CGPIOCreate(GPIODevice,HYPERPIXEL_I2C_SCL,HYPERPIXEL_I2C_SDA,HYPERPIXEL_I2C_DELAY,0,False,False);
 //if I2CDevice = nil then Exit;

 {Create FT5x06 Touch Device}
 //To Do //Touch (Address 0x48 / Interrupt GPIO 27)
 //To Do //Backlight

 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function HyperPixelInit21Round:LongWord;
{Initialize an attached HyperPixel 2.1 Round display}
var
 Count:LongWord;
 Data:THyperPixelData;
 I2CDevice:PI2CDevice;
 GPIODevice:PGPIODevice;
begin
 {}
 Result:=ERROR_OPERATION_FAILED;

 {Get the GPIO Device}
 GPIODevice:=GPIODeviceGetDefault;
 if GPIODevice = nil then Exit;

 {Setup GPIO Pins}
 for Count:=0 to HYPERPIXEL_GPIO_PIN_COUNT - 1 do
  begin
   {Setup Function}
   Result:=GPIODeviceFunctionSelect(GPIODevice,HYPERPIXEL_GPIO_PINS[Count],HYPERPIXEL_GPIO_PIN_FUNCTION);
   if Result <> ERROR_SUCCESS then Exit;

   {Setup Pull}
   Result:=GPIODevicePullSelect(GPIODevice,HYPERPIXEL_GPIO_PINS[Count],HYPERPIXEL_GPIO_PIN_PULL);
   if Result <> ERROR_SUCCESS then Exit;
  end;

 {Setup Software SPI}
 Data.Device:=GPIODevice;
 Data.FSEL:=HYPERPIXEL_SPI_FUNCTION;
 Data.PULL:=HYPERPIXEL_SPI_PULL;
 Data.CLK:=HYPERPIXEL_SPI_CLK;
 Data.MOSI:=HYPERPIXEL_SPI_MOSI;
 Data.CS:=HYPERPIXEL_SPI_CS;
 Data.Wait:=HYPERPIXEL_SPI_WAIT;
 Data.Delay:=HYPERPIXEL_SPI_DELAY;

 Result:=HyperPixelSPISetup(@Data);
 if Result <> ERROR_SUCCESS then Exit;

 HyperPixelSPIWriteCommand(@Data,$01);  {Niko added reset}

 MicrosecondDelay(240);

 HyperPixelSPIWriteCommand(@Data,$FF);
 HyperPixelSPIWriteCommand(@Data,$100 or $77);
 HyperPixelSPIWriteCommand(@Data,$100 or $01);
 HyperPixelSPIWriteCommand(@Data,$100 or $00);
 HyperPixelSPIWriteCommand(@Data,$100 or $00);
 HyperPixelSPIWriteCommand(@Data,$100 or $10);

 HyperPixelSPIWriteCommand(@Data,$C0);
 HyperPixelSPIWriteCommand(@Data,$100 or $3B); {Scan line}
 HyperPixelSPIWriteCommand(@Data,$100 or $00);

 HyperPixelSPIWriteCommand(@Data,$C1);
 HyperPixelSPIWriteCommand(@Data,$100 or $0B); {VBP}
 HyperPixelSPIWriteCommand(@Data,$100 or $02);

 HyperPixelSPIWriteCommand(@Data,$C2);
 HyperPixelSPIWriteCommand(@Data,$100 or $00);  {07 OR 00}
 HyperPixelSPIWriteCommand(@Data,$100 or $02);

 HyperPixelSPIWriteCommand(@Data,$CC);
 HyperPixelSPIWriteCommand(@Data,$100 or $10);

 {Gamma option B:}
 HyperPixelSPIWriteCommand(@Data,$B0);  {Positive Voltage Gamma Control}
 HyperPixelSPIWriteCommand(@Data,$100 or $02);
 HyperPixelSPIWriteCommand(@Data,$100 or $13);
 HyperPixelSPIWriteCommand(@Data,$100 or $1B);
 HyperPixelSPIWriteCommand(@Data,$100 or $0D);
 HyperPixelSPIWriteCommand(@Data,$100 or $10);
 HyperPixelSPIWriteCommand(@Data,$100 or $05);
 HyperPixelSPIWriteCommand(@Data,$100 or $08);
 HyperPixelSPIWriteCommand(@Data,$100 or $07);
 HyperPixelSPIWriteCommand(@Data,$100 or $07);
 HyperPixelSPIWriteCommand(@Data,$100 or $24);
 HyperPixelSPIWriteCommand(@Data,$100 or $04);
 HyperPixelSPIWriteCommand(@Data,$100 or $11);
 HyperPixelSPIWriteCommand(@Data,$100 or $0E);
 HyperPixelSPIWriteCommand(@Data,$100 or $2C);
 HyperPixelSPIWriteCommand(@Data,$100 or $33);
 HyperPixelSPIWriteCommand(@Data,$100 or $1D);

 HyperPixelSPIWriteCommand(@Data,$B1);  {Negative Voltage Gamma Control}
 HyperPixelSPIWriteCommand(@Data,$100 or $05);
 HyperPixelSPIWriteCommand(@Data,$100 or $13);
 HyperPixelSPIWriteCommand(@Data,$100 or $1B);
 HyperPixelSPIWriteCommand(@Data,$100 or $0D);
 HyperPixelSPIWriteCommand(@Data,$100 or $11);
 HyperPixelSPIWriteCommand(@Data,$100 or $05);
 HyperPixelSPIWriteCommand(@Data,$100 or $08);
 HyperPixelSPIWriteCommand(@Data,$100 or $07);
 HyperPixelSPIWriteCommand(@Data,$100 or $07);
 HyperPixelSPIWriteCommand(@Data,$100 or $24);
 HyperPixelSPIWriteCommand(@Data,$100 or $04);
 HyperPixelSPIWriteCommand(@Data,$100 or $11);
 HyperPixelSPIWriteCommand(@Data,$100 or $0E);
 HyperPixelSPIWriteCommand(@Data,$100 or $2C);
 HyperPixelSPIWriteCommand(@Data,$100 or $33);
 HyperPixelSPIWriteCommand(@Data,$100 or $1D);

 HyperPixelSPIWriteCommand(@Data,$FF);
 HyperPixelSPIWriteCommand(@Data,$100 or $77);
 HyperPixelSPIWriteCommand(@Data,$100 or $01);
 HyperPixelSPIWriteCommand(@Data,$100 or $00);
 HyperPixelSPIWriteCommand(@Data,$100 or $00);
 HyperPixelSPIWriteCommand(@Data,$100 or $11);

 HyperPixelSPIWriteCommand(@Data,$B0);  {VOP  3.5375+ *x 0.0125}
 HyperPixelSPIWriteCommand(@Data,$100 or $5D);  {6D OR 5D}

 HyperPixelSPIWriteCommand(@Data,$B1);  {VCOM amplitude setting}
 HyperPixelSPIWriteCommand(@Data,$100 or $43);  {37 OR 43}

 HyperPixelSPIWriteCommand(@Data,$B2);  {VGH Voltage setting}
 HyperPixelSPIWriteCommand(@Data,$100 or $81);  {12V}

 HyperPixelSPIWriteCommand(@Data,$B3);
 HyperPixelSPIWriteCommand(@Data,$100 or $80);

 HyperPixelSPIWriteCommand(@Data,$B5);  {VGL Voltage setting}
 HyperPixelSPIWriteCommand(@Data,$100 or $43);  {-8.3V}

 HyperPixelSPIWriteCommand(@Data,$B7);
 HyperPixelSPIWriteCommand(@Data,$100 or $85);

 HyperPixelSPIWriteCommand(@Data,$B8);
 HyperPixelSPIWriteCommand(@Data,$100 or $20);

 HyperPixelSPIWriteCommand(@Data,$C1);
 HyperPixelSPIWriteCommand(@Data,$100 or $78);

 HyperPixelSPIWriteCommand(@Data,$C2);
 HyperPixelSPIWriteCommand(@Data,$100 or $78);

 HyperPixelSPIWriteCommand(@Data,$D0);
 HyperPixelSPIWriteCommand(@Data,$100 or $88);

 HyperPixelSPIWriteCommand(@Data,$E0);
 HyperPixelSPIWriteCommand(@Data,$100 or $00);
 HyperPixelSPIWriteCommand(@Data,$100 or $00);
 HyperPixelSPIWriteCommand(@Data,$100 or $02);

 HyperPixelSPIWriteCommand(@Data,$E1);
 HyperPixelSPIWriteCommand(@Data,$100 or $03);
 HyperPixelSPIWriteCommand(@Data,$100 or $A0);
 HyperPixelSPIWriteCommand(@Data,$100 or $00);
 HyperPixelSPIWriteCommand(@Data,$100 or $00);
 HyperPixelSPIWriteCommand(@Data,$100 or $04);
 HyperPixelSPIWriteCommand(@Data,$100 or $A0);
 HyperPixelSPIWriteCommand(@Data,$100 or $00);
 HyperPixelSPIWriteCommand(@Data,$100 or $00);
 HyperPixelSPIWriteCommand(@Data,$100 or $00);
 HyperPixelSPIWriteCommand(@Data,$100 or $20);
 HyperPixelSPIWriteCommand(@Data,$100 or $20);

 HyperPixelSPIWriteCommand(@Data,$E2);
 HyperPixelSPIWriteCommand(@Data,$100 or $00);
 HyperPixelSPIWriteCommand(@Data,$100 or $00);
 HyperPixelSPIWriteCommand(@Data,$100 or $00);
 HyperPixelSPIWriteCommand(@Data,$100 or $00);
 HyperPixelSPIWriteCommand(@Data,$100 or $00);
 HyperPixelSPIWriteCommand(@Data,$100 or $00);
 HyperPixelSPIWriteCommand(@Data,$100 or $00);
 HyperPixelSPIWriteCommand(@Data,$100 or $00);
 HyperPixelSPIWriteCommand(@Data,$100 or $00);
 HyperPixelSPIWriteCommand(@Data,$100 or $00);
 HyperPixelSPIWriteCommand(@Data,$100 or $00);
 HyperPixelSPIWriteCommand(@Data,$100 or $00);
 HyperPixelSPIWriteCommand(@Data,$100 or $00);

 HyperPixelSPIWriteCommand(@Data,$E3);
 HyperPixelSPIWriteCommand(@Data,$100 or $00);
 HyperPixelSPIWriteCommand(@Data,$100 or $00);
 HyperPixelSPIWriteCommand(@Data,$100 or $11);
 HyperPixelSPIWriteCommand(@Data,$100 or $00);

 HyperPixelSPIWriteCommand(@Data,$E4);
 HyperPixelSPIWriteCommand(@Data,$100 or $22);
 HyperPixelSPIWriteCommand(@Data,$100 or $00);

 HyperPixelSPIWriteCommand(@Data,$E5);
 HyperPixelSPIWriteCommand(@Data,$100 or $05);
 HyperPixelSPIWriteCommand(@Data,$100 or $EC);
 HyperPixelSPIWriteCommand(@Data,$100 or $A0);
 HyperPixelSPIWriteCommand(@Data,$100 or $A0);
 HyperPixelSPIWriteCommand(@Data,$100 or $07);
 HyperPixelSPIWriteCommand(@Data,$100 or $EE);
 HyperPixelSPIWriteCommand(@Data,$100 or $A0);
 HyperPixelSPIWriteCommand(@Data,$100 or $A0);
 HyperPixelSPIWriteCommand(@Data,$100 or $00);
 HyperPixelSPIWriteCommand(@Data,$100 or $00);
 HyperPixelSPIWriteCommand(@Data,$100 or $00);
 HyperPixelSPIWriteCommand(@Data,$100 or $00);
 HyperPixelSPIWriteCommand(@Data,$100 or $00);
 HyperPixelSPIWriteCommand(@Data,$100 or $00);
 HyperPixelSPIWriteCommand(@Data,$100 or $00);
 HyperPixelSPIWriteCommand(@Data,$100 or $00);

 HyperPixelSPIWriteCommand(@Data,$E6);
 HyperPixelSPIWriteCommand(@Data,$100 or $00);
 HyperPixelSPIWriteCommand(@Data,$100 or $00);
 HyperPixelSPIWriteCommand(@Data,$100 or $11);
 HyperPixelSPIWriteCommand(@Data,$100 or $00);

 HyperPixelSPIWriteCommand(@Data,$E7);
 HyperPixelSPIWriteCommand(@Data,$100 or $22);
 HyperPixelSPIWriteCommand(@Data,$100 or $00);

 HyperPixelSPIWriteCommand(@Data,$E8);
 HyperPixelSPIWriteCommand(@Data,$100 or $06);
 HyperPixelSPIWriteCommand(@Data,$100 or $ED);
 HyperPixelSPIWriteCommand(@Data,$100 or $A0);
 HyperPixelSPIWriteCommand(@Data,$100 or $A0);
 HyperPixelSPIWriteCommand(@Data,$100 or $08);
 HyperPixelSPIWriteCommand(@Data,$100 or $EF);
 HyperPixelSPIWriteCommand(@Data,$100 or $A0);
 HyperPixelSPIWriteCommand(@Data,$100 or $A0);
 HyperPixelSPIWriteCommand(@Data,$100 or $00);
 HyperPixelSPIWriteCommand(@Data,$100 or $00);
 HyperPixelSPIWriteCommand(@Data,$100 or $00);
 HyperPixelSPIWriteCommand(@Data,$100 or $00);
 HyperPixelSPIWriteCommand(@Data,$100 or $00);
 HyperPixelSPIWriteCommand(@Data,$100 or $00);
 HyperPixelSPIWriteCommand(@Data,$100 or $00);
 HyperPixelSPIWriteCommand(@Data,$100 or $00);

 HyperPixelSPIWriteCommand(@Data,$EB);
 HyperPixelSPIWriteCommand(@Data,$100 or $00);
 HyperPixelSPIWriteCommand(@Data,$100 or $00);
 HyperPixelSPIWriteCommand(@Data,$100 or $40);
 HyperPixelSPIWriteCommand(@Data,$100 or $40);
 HyperPixelSPIWriteCommand(@Data,$100 or $00);
 HyperPixelSPIWriteCommand(@Data,$100 or $00);
 HyperPixelSPIWriteCommand(@Data,$100 or $00);

 HyperPixelSPIWriteCommand(@Data,$ED);
 HyperPixelSPIWriteCommand(@Data,$100 or $FF);
 HyperPixelSPIWriteCommand(@Data,$100 or $FF);
 HyperPixelSPIWriteCommand(@Data,$100 or $FF);
 HyperPixelSPIWriteCommand(@Data,$100 or $BA);
 HyperPixelSPIWriteCommand(@Data,$100 or $0A);
 HyperPixelSPIWriteCommand(@Data,$100 or $BF);
 HyperPixelSPIWriteCommand(@Data,$100 or $45);
 HyperPixelSPIWriteCommand(@Data,$100 or $FF);
 HyperPixelSPIWriteCommand(@Data,$100 or $FF);
 HyperPixelSPIWriteCommand(@Data,$100 or $54);
 HyperPixelSPIWriteCommand(@Data,$100 or $FB);
 HyperPixelSPIWriteCommand(@Data,$100 or $A0);
 HyperPixelSPIWriteCommand(@Data,$100 or $AB);
 HyperPixelSPIWriteCommand(@Data,$100 or $FF);
 HyperPixelSPIWriteCommand(@Data,$100 or $FF);
 HyperPixelSPIWriteCommand(@Data,$100 or $FF);

 HyperPixelSPIWriteCommand(@Data,$EF);
 HyperPixelSPIWriteCommand(@Data,$100 or $10);
 HyperPixelSPIWriteCommand(@Data,$100 or $0D);
 HyperPixelSPIWriteCommand(@Data,$100 or $04);
 HyperPixelSPIWriteCommand(@Data,$100 or $08); {Positive Voltage Gamma Control}
 HyperPixelSPIWriteCommand(@Data,$100 or $1F);

 HyperPixelSPIWriteCommand(@Data,$FF);
 HyperPixelSPIWriteCommand(@Data,$100 or $77);
 HyperPixelSPIWriteCommand(@Data,$100 or $01);
 HyperPixelSPIWriteCommand(@Data,$100 or $00);
 HyperPixelSPIWriteCommand(@Data,$100 or $00);
 HyperPixelSPIWriteCommand(@Data,$100 or $13);

 HyperPixelSPIWriteCommand(@Data,$EF);
 HyperPixelSPIWriteCommand(@Data,$100 or $08);

 HyperPixelSPIWriteCommand(@Data,$FF);
 HyperPixelSPIWriteCommand(@Data,$100 or $77);
 HyperPixelSPIWriteCommand(@Data,$100 or $01);
 HyperPixelSPIWriteCommand(@Data,$100 or $00);
 HyperPixelSPIWriteCommand(@Data,$100 or $00);
 HyperPixelSPIWriteCommand(@Data,$100 or $00);

 HyperPixelSPIWriteCommand(@Data,$CD);  {RGB format COLCTRL}
 HyperPixelSPIWriteCommand(@Data,$100 or $08);
 {HyperPixelSPIWriteCommand(@Data,$100 or $00);}

 HyperPixelSPIWriteCommand(@Data,$36); {MadCtl}
 HyperPixelSPIWriteCommand(@Data,$100 or $08);

 HyperPixelSPIWriteCommand(@Data,$3A);  {Colmod}
 HyperPixelSPIWriteCommand(@Data,$100 or $66);

 HyperPixelSPIWriteCommand(@Data,$11);
 MicrosecondDelay(120);

 HyperPixelSPIWriteCommand(@Data,$29);
 MicrosecondDelay(20);

 {Cleanup Software SPI}
 Result:=HyperPixelSPICleanup(@Data);
 if Result <> ERROR_SUCCESS then Exit;

 Result:=ERROR_OPERATION_FAILED;

 {Create I2CGPIO Device for Touch}
 //I2CDevice:=I2CGPIOCreate(GPIODevice,HYPERPIXEL_I2C_SCL,HYPERPIXEL_I2C_SDA,HYPERPIXEL_I2C_DELAY,0,False,False);
 //if I2CDevice = nil then Exit;

 {Create Internal Touch Device}
 //To Do //Touch (Address 0x15 / Interrupt GPIO 27)
 //To Do //Backlight

 Result:=ERROR_SUCCESS;
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
