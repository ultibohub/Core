{
Raspberry Pi Firmware GPIO Expander Driver.

Copyright (C) 2021 - SoftOz Pty Ltd.

Arch
====

 <All>

Boards
======

 Raspberry Pi 3 - Model B/B+/A+
 Raspberry Pi CM3/CM3+
 Raspberry Pi 4 - Model B
 Raspberry Pi 400
 Raspberry Pi CM4

Licence
=======

 LGPLv2.1 with static linking exception (See COPYING.modifiedLGPL.txt)

Credits
=======

 Information for this unit was obtained from:

  Linux - \drivers\gpio\gpio-bcm-exp.c - Copyright (C) 2017 Raspberry Pi Trading Ltd.
  Linux - \include\soc\bcm2835\raspberrypi-firmware.h - Copyright (C) 2015 Broadcom

References
==========

  Raspbian Linux - https://github.com/raspberrypi/linux/

Raspberry Pi GPIO Expander
==========================

 As of February 2017 the Raspberry Pi firmware contains a mailbox interface to allow
 control of the GPIO expander on the Raspberry Pi 3/4 and Compute Module 3/4.

 Unlike the earlier virtual GPIO interface this mailbox service now supports most GPIO
 functionality including getting and setting a pin value, getting and changing the pin
 function and getting or changing the pullup/down options.

 This driver implements most of the functionality available in the new mailbox service
 as a standard Ultibo GPIO device and also provides direct mailbox calls to allow access
 to any additional options not covered by the GPIO device API.

 When included in a project this driver replaces the Virtual GPIO functions provided by
 the PlatformRPi2, PlatformRPi3, PlatformRPi4 units.

 According to /arch/arm/boot/dts/bcm2710-rpi-3-b.dts the following pin assignments are known:

  GPIO_PIN_2 = Activity LED
  GPIO_PIN_4 = HDMI Detect (Input / Active Low)
  GPIO_PIN_7 = Power LED (Input / Active Low)

 For the Raspberry Pi 3B+ the assignments shown in /arch/arm/boot/dts/bcm2710-rpi-3-b.dts have
 changed as follows:

  GPIO_PIN_2 = Power LED (Active Low)
  GPIO_PIN_4 = HDMI Detect (Input / Active Low)

 For the Raspberry Pi 4B the assignments shown in /arch/arm/boot/dts/bcm2711-rpi-4-b.dts are:

  GPIO_PIN_0 = Bluetooth Power (Active High) (BT_ON)
  GPIO_PIN_1 = WiFi Power Sequencer (Active Low) (WL_ON)
  GPIO_PIN_2 = Power LED (Active Low) (PWR_LED_OFF)
  GPIO_PIN_3 = (GLOBAL_RESET) (ANT1 on CM4)
  GPIO_PIN_4 = SDIO 1.8V regulator (Active High) (VDD_SD_IO_SEL)
  GPIO_PIN_5 = CAM1 regulator (Active High) (CAM_GPIO)
  GPIO_PIN_6 = SD VCC regulator (Active High) (SD_PWR_ON)
  GPIO_PIN_7 = (SD_OC_N) (ANT2 on CM4)

 The Pi 400 and CM4 files at bcm2711-rpi-400.dts and bcm2711-rpi-cm4.dts show identical assignments
 as the Pi 4B.

 Note that this driver requires recent firmware (later than February 2017) and has been tested
 successfully with the firmware release from 8 October 2020.

 The latest version of the firmware is available from https://github.com/raspberrypi/firmware

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

{$IFNDEF FPC_DOTTEDUNITS}
unit RPiGPIOExpander;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  Core.GlobalConfig,
  Core.GlobalConst,
  Core.GlobalTypes,
  Core.Platform,
  Core.HeapManager,
  Core.Threads,
  Core.Devices,
  Core.GPIO,
  System.SysUtils;
{$ELSE FPC_DOTTEDUNITS}
uses
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  HeapManager,
  Threads,
  Devices,
  GPIO,
  SysUtils;
{$ENDIF FPC_DOTTEDUNITS}

{==============================================================================}
{Global definitions}
{$INCLUDE ..\core\GlobalDefines.inc}

{==============================================================================}
const
 {RPiGPIO Expander specific constants}
 RPIGPIOEXP_GPIO_DESCRIPTION = 'Raspberry Pi Firmware GPIO Expander';  {Description of RPiGPIO Expander device}

 RPIGPIOEXP_GPIO_MIN_PIN = GPIO_PIN_0;
 RPIGPIOEXP_GPIO_MAX_PIN = GPIO_PIN_7;

 RPIGPIOEXP_GPIO_PIN_COUNT  = 8;

 RPIGPIOEXP_GPIO_PIN_OFFSET = 128;

 RPIGPIOEXP_GPIO_MAX_LEVEL = GPIO_LEVEL_HIGH;

 RPIGPIOEXP_GPIO_MAX_PULL = GPIO_PULL_DOWN;

 RPIGPIOEXP_GPIO_MIN_FUNCTION = GPIO_FUNCTION_IN;
 RPIGPIOEXP_GPIO_MAX_FUNCTION = GPIO_FUNCTION_OUT;

{==============================================================================}
type
 {RPiGPIO Expander specific types}
 PRPiGPIOExpander = ^TRPiGPIOExpander;
 TRPiGPIOExpander = record
  {GPIO Properties}
  GPIO:TGPIODevice;
  {RPiGPIO Expander Properties}
  {Nothing}
 end;

{==============================================================================}
{var}
 {RPiGPIO Expander specific variables}

{==============================================================================}
{Initialization Functions}
procedure RPiGPIOExpanderInit;

{==============================================================================}
{RPiGPIO Expander Functions}

{==============================================================================}
{RPiGPIO Expander GPIO Functions}
function RPiGPIOExpanderStart(GPIO:PGPIODevice):LongWord;
function RPiGPIOExpanderStop(GPIO:PGPIODevice):LongWord;

function RPiGPIOExpanderInputGet(GPIO:PGPIODevice;Pin:LongWord):LongWord;

function RPiGPIOExpanderOutputSet(GPIO:PGPIODevice;Pin,Level:LongWord):LongWord;

function RPiGPIOExpanderPullGet(GPIO:PGPIODevice;Pin:LongWord):LongWord;
function RPiGPIOExpanderPullSelect(GPIO:PGPIODevice;Pin,Mode:LongWord):LongWord;

function RPiGPIOExpanderFunctionGet(GPIO:PGPIODevice;Pin:LongWord):LongWord;
function RPiGPIOExpanderFunctionSelect(GPIO:PGPIODevice;Pin,Mode:LongWord):LongWord;

{==============================================================================}
{RTL Virtual GPIO Functions}
function SysVirtualGPIOInputGet(Pin:LongWord):LongWord;
function SysVirtualGPIOOutputSet(Pin,Level:LongWord):LongWord;
function SysVirtualGPIOFunctionGet(Pin:LongWord):LongWord;
function SysVirtualGPIOFunctionSelect(Pin,Mode:LongWord):LongWord;

{==============================================================================}
{RPiGPIO Expander Helper Functions}
function RPiGPIOExpanderGetState(GPIO:LongWord;var State:LongWord):LongWord;
function RPiGPIOExpanderSetState(GPIO,State:LongWord):LongWord;

function RPiGPIOExpanderGetConfig(GPIO:LongWord;var Direction,Polarity,Terminator,PullUp:LongWord):LongWord;
function RPiGPIOExpanderSetConfig(GPIO,Direction,Polarity,Terminator,PullUp,State:LongWord):LongWord;

{==============================================================================}
{==============================================================================}

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses
  {$IFDEF CPUARMV6}
  Platforms.BCM2835
  {$ELSE}
  Platforms.BCM2837
  {$ENDIF}
  ;
{$ELSE FPC_DOTTEDUNITS}
uses
  {$IFDEF CPUARMV6}
  BCM2835
  {$ELSE}
  BCM2837
  {$ENDIF}
  ;
{$ENDIF FPC_DOTTEDUNITS}

{==============================================================================}
{==============================================================================}
const
 {RPiGPIO Expander mailbox constants}
 {$IFDEF CPUARMV6}
 RPIGPIO_MBOX_REQUEST_CODE = BCM2835_MBOX_REQUEST_CODE;

 RPIGPIO_MBOX_TAG_END = BCM2835_MBOX_TAG_END;
 RPIGPIO_MBOX_TAG_GET_GPIO_STATE = BCM2835_MBOX_TAG_GET_GPIO_STATE;
 RPIGPIO_MBOX_TAG_SET_GPIO_STATE = BCM2835_MBOX_TAG_SET_GPIO_STATE;
 RPIGPIO_MBOX_TAG_GET_GPIO_CONFIG = BCM2835_MBOX_TAG_GET_GPIO_CONFIG;
 RPIGPIO_MBOX_TAG_SET_GPIO_CONFIG = BCM2835_MBOX_TAG_SET_GPIO_CONFIG;

 RPIGPIO_MAILBOX_0 = BCM2835_MAILBOX_0;
 RPIGPIO_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC = BCM2835_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC;
 {$ELSE}
 RPIGPIO_MBOX_REQUEST_CODE = BCM2837_MBOX_REQUEST_CODE;

 RPIGPIO_MBOX_TAG_END = BCM2837_MBOX_TAG_END;
 RPIGPIO_MBOX_TAG_GET_GPIO_STATE = BCM2837_MBOX_TAG_GET_GPIO_STATE;
 RPIGPIO_MBOX_TAG_SET_GPIO_STATE = BCM2837_MBOX_TAG_SET_GPIO_STATE;
 RPIGPIO_MBOX_TAG_GET_GPIO_CONFIG = BCM2837_MBOX_TAG_GET_GPIO_CONFIG;
 RPIGPIO_MBOX_TAG_SET_GPIO_CONFIG = BCM2837_MBOX_TAG_SET_GPIO_CONFIG;

 RPIGPIO_MAILBOX_0 = BCM2837_MAILBOX_0;
 RPIGPIO_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC = BCM2837_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC;
 {$ENDIF}
{==============================================================================}
{==============================================================================}
type
 {RPiGPIO Expander mailbox types}
 {$IFDEF CPUARMV6}
 TRPiGPIOMailboxHeader = TBCM2835MailboxHeader;
 PRPiGPIOMailboxHeader = PBCM2835MailboxHeader;

 TRPiGPIOMailboxFooter = TBCM2835MailboxFooter;
 PRPiGPIOMailboxFooter = PBCM2835MailboxFooter;

 TRPiGPIOMailboxTagHeader = TBCM2835MailboxTagHeader;
 PRPiGPIOMailboxTagHeader = PBCM2835MailboxTagHeader;

 TRPiGPIOMailboxTagGetGPIOState = TBCM2835MailboxTagGetGPIOState;
 PRPiGPIOMailboxTagGetGPIOState = PBCM2835MailboxTagGetGPIOState;

 TRPiGPIOMailboxTagSetGPIOState = TBCM2835MailboxTagSetGPIOState;
 PRPiGPIOMailboxTagSetGPIOState = PBCM2835MailboxTagSetGPIOState;

 TRPiGPIOMailboxTagGetGPIOConfig = TBCM2835MailboxTagGetGPIOConfig;
 PRPiGPIOMailboxTagGetGPIOConfig = PBCM2835MailboxTagGetGPIOConfig;

 TRPiGPIOMailboxTagSetGPIOConfig = TBCM2835MailboxTagSetGPIOConfig;
 PRPiGPIOMailboxTagSetGPIOConfig = PBCM2835MailboxTagSetGPIOConfig;
 {$ELSE}
 TRPiGPIOMailboxHeader = TBCM2837MailboxHeader;
 PRPiGPIOMailboxHeader = PBCM2837MailboxHeader;

 TRPiGPIOMailboxFooter = TBCM2837MailboxFooter;
 PRPiGPIOMailboxFooter = PBCM2837MailboxFooter;

 TRPiGPIOMailboxTagHeader = TBCM2837MailboxTagHeader;
 PRPiGPIOMailboxTagHeader = PBCM2837MailboxTagHeader;

 TRPiGPIOMailboxTagGetGPIOState = TBCM2837MailboxTagGetGPIOState;
 PRPiGPIOMailboxTagGetGPIOState = PBCM2837MailboxTagGetGPIOState;

 TRPiGPIOMailboxTagSetGPIOState = TBCM2837MailboxTagSetGPIOState;
 PRPiGPIOMailboxTagSetGPIOState = PBCM2837MailboxTagSetGPIOState;

 TRPiGPIOMailboxTagGetGPIOConfig = TBCM2837MailboxTagGetGPIOConfig;
 PRPiGPIOMailboxTagGetGPIOConfig = PBCM2837MailboxTagGetGPIOConfig;

 TRPiGPIOMailboxTagSetGPIOConfig = TBCM2837MailboxTagSetGPIOConfig;
 PRPiGPIOMailboxTagSetGPIOConfig = PBCM2837MailboxTagSetGPIOConfig;
 {$ENDIF}
{==============================================================================}
{==============================================================================}
var
 {RPiGPIO Expander specific variables}
 RPiGPIOExpander:PRPiGPIOExpander;
 RPiGPIOExpanderInitialized:Boolean;

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure RPiGPIOExpanderInit;
{Initialize the RPiGPIO Expander unit and create, register and start the device}

{Note: Called only during system startup}
var
 Status:LongWord;
 BoardType:LongWord;
begin
 {}
 {Check Initialized}
 if RPiGPIOExpanderInitialized then Exit;

 {Check Board Type}
 BoardType:=BoardGetType;
 case BoardType of
  BOARD_TYPE_RPI3B,
  BOARD_TYPE_RPI3B_PLUS,
  BOARD_TYPE_RPI3A_PLUS,
  BOARD_TYPE_RPI_COMPUTE3,
  BOARD_TYPE_RPI_COMPUTE3_PLUS,
  BOARD_TYPE_RPI4B,
  BOARD_TYPE_RPI400,
  BOARD_TYPE_RPI_COMPUTE4:begin
    {Create GPIO}
    RPiGPIOExpander:=PRPiGPIOExpander(GPIODeviceCreateEx(SizeOf(TRPiGPIOExpander)));
    if RPiGPIOExpander <> nil then
     begin
      {Update GPIO}
      {Device}
      RPiGPIOExpander.GPIO.Device.DeviceBus:=DEVICE_BUS_MMIO;
      RPiGPIOExpander.GPIO.Device.DeviceType:=GPIO_TYPE_NONE;
      RPiGPIOExpander.GPIO.Device.DeviceFlags:=GPIO_FLAG_PULL_UP;
      RPiGPIOExpander.GPIO.Device.DeviceData:=nil;
      RPiGPIOExpander.GPIO.Device.DeviceDescription:=RPIGPIOEXP_GPIO_DESCRIPTION;
      {GPIO}
      RPiGPIOExpander.GPIO.GPIOState:=GPIO_STATE_DISABLED;
      RPiGPIOExpander.GPIO.DeviceStart:=RPiGPIOExpanderStart;
      RPiGPIOExpander.GPIO.DeviceStop:=RPiGPIOExpanderStop;
      RPiGPIOExpander.GPIO.DeviceInputGet:=RPiGPIOExpanderInputGet;
      RPiGPIOExpander.GPIO.DeviceOutputSet:=RPiGPIOExpanderOutputSet;
      RPiGPIOExpander.GPIO.DevicePullGet:=RPiGPIOExpanderPullGet;
      RPiGPIOExpander.GPIO.DevicePullSelect:=RPiGPIOExpanderPullSelect;
      RPiGPIOExpander.GPIO.DeviceFunctionGet:=RPiGPIOExpanderFunctionGet;
      RPiGPIOExpander.GPIO.DeviceFunctionSelect:=RPiGPIOExpanderFunctionSelect;
      {Driver}
      RPiGPIOExpander.GPIO.Address:=nil;
      RPiGPIOExpander.GPIO.Properties.Flags:=RPiGPIOExpander.GPIO.Device.DeviceFlags;
      RPiGPIOExpander.GPIO.Properties.PinMin:=RPIGPIOEXP_GPIO_MIN_PIN;
      RPiGPIOExpander.GPIO.Properties.PinMax:=RPIGPIOEXP_GPIO_MAX_PIN;
      RPiGPIOExpander.GPIO.Properties.PinCount:=RPIGPIOEXP_GPIO_PIN_COUNT;
      RPiGPIOExpander.GPIO.Properties.FunctionMin:=RPIGPIOEXP_GPIO_MIN_FUNCTION;
      RPiGPIOExpander.GPIO.Properties.FunctionMax:=RPIGPIOEXP_GPIO_MAX_FUNCTION;
      RPiGPIOExpander.GPIO.Properties.FunctionCount:=2;
      {RPiGPIO Expander}
      {Nothing}

      {Register GPIO}
      Status:=GPIODeviceRegister(@RPiGPIOExpander.GPIO);
      if Status = ERROR_SUCCESS then
       begin
        {Start GPIO}
        Status:=GPIODeviceStart(@RPiGPIOExpander.GPIO);
        if Status = ERROR_SUCCESS then
         begin
          {Register Platform Virtual GPIO Handlers}
          VirtualGPIOInputGetHandler:=SysVirtualGPIOInputGet;
          VirtualGPIOOutputSetHandler:=SysVirtualGPIOOutputSet;
          VirtualGPIOFunctionGetHandler:=SysVirtualGPIOFunctionGet;
          VirtualGPIOFunctionSelectHandler:=SysVirtualGPIOFunctionSelect;

          {Setup Virtual GPIO}
          VIRTUAL_GPIO_PIN_COUNT:=RPIGPIOEXP_GPIO_PIN_COUNT;

          case BoardType of
           BOARD_TYPE_RPI3B,
           BOARD_TYPE_RPI_COMPUTE3:begin
             {Setup Activity LED}
             ACTIVITY_LED_PIN:=VIRTUAL_GPIO_PIN_2;
             ACTIVITY_LED_FUNCTION:=VIRTUAL_GPIO_FUNCTION_OUT;
             ACTIVITY_LED_ACTIVE_LOW:=False;

             {Setup Power LED}
             POWER_LED_PIN:=VIRTUAL_GPIO_PIN_7;
             POWER_LED_FUNCTION:=VIRTUAL_GPIO_FUNCTION_OUT;
             POWER_LED_ACTIVE_LOW:=False;
            end;
           BOARD_TYPE_RPI3B_PLUS,
           BOARD_TYPE_RPI3A_PLUS,
           BOARD_TYPE_RPI_COMPUTE3_PLUS:begin
             {Activity LED is on GPIO 29}
             {Setup Power LED}
             POWER_LED_PIN:=VIRTUAL_GPIO_PIN_2;
             POWER_LED_FUNCTION:=VIRTUAL_GPIO_FUNCTION_OUT;
             POWER_LED_ACTIVE_LOW:=True;
            end;
           BOARD_TYPE_RPI4B,
           BOARD_TYPE_RPI400,
           BOARD_TYPE_RPI_COMPUTE4:begin
             {Activity LED is on GPIO 42}
             {Setup Power LED}
             POWER_LED_PIN:=VIRTUAL_GPIO_PIN_2;
             POWER_LED_FUNCTION:=VIRTUAL_GPIO_FUNCTION_OUT;
             POWER_LED_ACTIVE_LOW:=True;
            end;
          end;
         end
        else
         begin
          if GPIO_LOG_ENABLED then GPIOLogError(nil,'RPiGPIOEXP: Failed to start new GPIO device: ' + ErrorToString(Status));

          {Deregister GPIO}
          GPIODeviceDeregister(@RPiGPIOExpander.GPIO);

          {Destroy GPIO}
          GPIODeviceDestroy(@RPiGPIOExpander.GPIO);
         end;
       end
      else
       begin
        if GPIO_LOG_ENABLED then GPIOLogError(nil,'RPiGPIOEXP: Failed to register new GPIO device: ' + ErrorToString(Status));

        {Destroy GPIO}
        GPIODeviceDestroy(@RPiGPIOExpander.GPIO);
       end;
     end
    else
     begin
      if GPIO_LOG_ENABLED then GPIOLogError(nil,'RPiGPIOEXP: Failed to create new GPIO device');
     end;
   end;
 end;

 RPiGPIOExpanderInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{RPiGPIO Expander Functions}

{==============================================================================}
{==============================================================================}
{RPiGPIO Expander GPIO Functions}
function RPiGPIOExpanderStart(GPIO:PGPIODevice):LongWord;
{Implementation of GPIODeviceStart API for RPiGPIO Expander}
{Note: Not intended to be called directly by applications, use GPIODeviceStart instead}
var
 State:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check GPIO}
 if GPIO = nil then Exit;

 {$IF DEFINED(RPIGPIOEXP_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'RPiGPIOEXP: GPIO Start');
 {$ENDIF}

 {Check Supported}
 Result:=RPiGPIOExpanderGetState(GPIO_PIN_0,State);
end;

{==============================================================================}

function RPiGPIOExpanderStop(GPIO:PGPIODevice):LongWord;
{Implementation of GPIODeviceStop API for RPiGPIO Expander}
{Note: Not intended to be called directly by applications, use GPIODeviceStop instead}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check GPIO}
 if GPIO = nil then Exit;

 {$IF DEFINED(RPIGPIOEXP_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'RPiGPIOEXP: GPIO Stop');
 {$ENDIF}

 {Return Result}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function RPiGPIOExpanderInputGet(GPIO:PGPIODevice;Pin:LongWord):LongWord;
{Implementation of GPIODeviceInputGet API for RPiGPIO Expander}
{Note: Not intended to be called directly by applications, use GPIODeviceInputGet instead}
begin
 {}
 Result:=GPIO_LEVEL_UNKNOWN;

 {Check GPIO}
 if GPIO = nil then Exit;

 {$IF DEFINED(RPIGPIOEXP_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'RPiGPIOEXP: GPIO Input Get (Pin=' + GPIOPinToString(Pin) + ')');
 {$ENDIF}

 {Check Pin}
 if Pin > GPIO.Properties.PinMax then Exit;

 {Update Statistics}
 Inc(GPIO.GetCount);

 {Get State}
 RPiGPIOExpanderGetState(Pin,Result);
end;

{==============================================================================}

function RPiGPIOExpanderOutputSet(GPIO:PGPIODevice;Pin,Level:LongWord):LongWord;
{Implementation of GPIODeviceOutputSet API for RPiGPIO Expander}
{Note: Not intended to be called directly by applications, use GPIODeviceOutputSet instead}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check GPIO}
 if GPIO = nil then Exit;

 {$IF DEFINED(RPIGPIOEXP_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'RPiGPIOEXP: GPIO Output Set (Pin=' + GPIOPinToString(Pin) + ' Level=' + GPIOLevelToString(Level) + ')');
 {$ENDIF}

 {Check Pin}
 if Pin > GPIO.Properties.PinMax then Exit;

 {Check Level}
 if Level > RPIGPIOEXP_GPIO_MAX_LEVEL then Exit;

 {Update Statistics}
 Inc(GPIO.SetCount);

 {Set State}
 Result:=RPiGPIOExpanderSetState(Pin,Level);
end;

{==============================================================================}

function RPiGPIOExpanderPullGet(GPIO:PGPIODevice;Pin:LongWord):LongWord;
{Implementation of GPIODevicePullGet API for RPiGPIO Expander}
{Note: Not intended to be called directly by applications, use GPIODevicePullGet instead}
var
 Direction:LongWord;
 Polarity:LongWord;
 Terminator:LongWord;
 PullUp:LongWord;
begin
 {}
 Result:=GPIO_PULL_UNKNOWN;

 {$IF DEFINED(RPIGPIOEXP_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'RPiGPIOEXP: GPIO Pull Get (Pin=' + GPIOPinToString(Pin) + ')');
 {$ENDIF}

 {Check Pin}
 if Pin > GPIO.Properties.PinMax then Exit;

 {Get Config}
 if RPiGPIOExpanderGetConfig(Pin,Direction,Polarity,Terminator,PullUp) = ERROR_SUCCESS then
  begin
   {Check Direction}
   if Direction = GPIO_FUNCTION_IN then
    begin
     Result:=GPIO_PULL_NONE;

     {Check Terminator Enabled}
     if Terminator <> 0 then
      begin
       Result:=GPIO_PULL_DOWN;

       {Check PullUp Enabled}
       if PullUp <> 0 then
        begin
         Result:=GPIO_PULL_UP;
        end;
      end;
    end;
  end;
end;

{==============================================================================}

function RPiGPIOExpanderPullSelect(GPIO:PGPIODevice;Pin,Mode:LongWord):LongWord;
{Implementation of GPIODevicePullSelect API for RPiGPIO Expander}
{Note: Not intended to be called directly by applications, use GPIODevicePullSelect instead}
var
 Direction:LongWord;
 Polarity:LongWord;
 Terminator:LongWord;
 PullUp:LongWord;
 State:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check GPIO}
 if GPIO = nil then Exit;

 {$IF DEFINED(RPIGPIOEXP_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'RPiGPIOEXP: GPIO Pull Select (Pin=' + GPIOPinToString(Pin) + ' Mode=' + GPIOPullToString(Mode) + ')');
 {$ENDIF}

 {Check Pin}
 if Pin > GPIO.Properties.PinMax then Exit;

 {Check Mode}
 if Mode > RPIGPIOEXP_GPIO_MAX_PULL then Exit;

 {Get Config}
 Result:=RPiGPIOExpanderGetConfig(Pin,Direction,Polarity,Terminator,PullUp);
 if Result <> ERROR_SUCCESS then Exit;

 {Check Direction}
 if Direction = GPIO_FUNCTION_IN then
  begin
   {Input}
   {Direction}                                    {Retain existing}
   {Polarity}                                     {Retain existing}
   Terminator:=0;                                 {Termination Disabled}
   if Mode <> GPIO_PULL_NONE then Terminator:=1;  {Termination Enabled}
   PullUp:=0;                                     {Pull Down (N/A for Termination Disabled)}
   if Mode = GPIO_PULL_UP then PullUp:=1;         {Pull Up}
   State:=0;                                      {N/A for Input}
   Result:=RPiGPIOExpanderSetConfig(Pin,Direction,Polarity,Terminator,PullUp,State);
  end
 else
  begin
   Result:=ERROR_SUCCESS;
  end;
end;

{==============================================================================}

function RPiGPIOExpanderFunctionGet(GPIO:PGPIODevice;Pin:LongWord):LongWord;
{Implementation of GPIODeviceFunctionGet API for RPiGPIO Expander}
{Note: Not intended to be called directly by applications, use GPIODeviceFunctionGet instead}
var
 Direction:LongWord;
 Polarity:LongWord;
 Terminator:LongWord;
 PullUp:LongWord;
begin
 {}
 Result:=GPIO_FUNCTION_UNKNOWN;

 {Check GPIO}
 if GPIO = nil then Exit;

 {$IF DEFINED(RPIGPIOEXP_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'RPiGPIOEXP: GPIO Function Get (Pin=' + GPIOPinToString(Pin) + ')');
 {$ENDIF}

 {Check Pin}
 if Pin > GPIO.Properties.PinMax then Exit;

 {Get Config}
 if RPiGPIOExpanderGetConfig(Pin,Direction,Polarity,Terminator,PullUp) = ERROR_SUCCESS then
  begin
   Result:=Direction;
  end;
end;

{==============================================================================}

function RPiGPIOExpanderFunctionSelect(GPIO:PGPIODevice;Pin,Mode:LongWord):LongWord;
{Implementation of GPIODeviceFunctionSelect API for RPiGPIO Expander}
{Note: Not intended to be called directly by applications, use GPIODeviceFunctionSelect instead}
var
 Direction:LongWord;
 Polarity:LongWord;
 Terminator:LongWord;
 PullUp:LongWord;
 State:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check GPIO}
 if GPIO = nil then Exit;

 {$IF DEFINED(RPIGPIOEXP_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'RPiGPIOEXP: GPIO Function Select (Pin=' + GPIOPinToString(Pin) + ' Mode=' + GPIOFunctionToString(Mode) + ')');
 {$ENDIF}

 {Check Pin}
 if Pin > GPIO.Properties.PinMax then Exit;

 {Check Mode}
 if Mode > RPIGPIOEXP_GPIO_MAX_FUNCTION then Exit;

 {Check Mode}
 case Mode of
  GPIO_FUNCTION_IN:begin
    {Get Config}
    Result:=RPiGPIOExpanderGetConfig(Pin,Direction,Polarity,Terminator,PullUp);
    if Result <> ERROR_SUCCESS then Exit;

    {Input}
    Direction:=GPIO_FUNCTION_IN; {Input}
    {Polarity}                   {Retain existing}
    Terminator:=0;               {Termination Disabled}
    PullUp:=0;                   {N/A for Termination Disabled}
    State:=0;                    {N/A for Input}
    Result:=RPiGPIOExpanderSetConfig(Pin,Direction,Polarity,Terminator,PullUp,State);
   end;
  GPIO_FUNCTION_OUT:begin
    {Get State}
    Result:=RPiGPIOExpanderGetState(Pin,State);
    if Result <> ERROR_SUCCESS then Exit;

    {Get Config}
    Result:=RPiGPIOExpanderGetConfig(Pin,Direction,Polarity,Terminator,PullUp);
    if Result <> ERROR_SUCCESS then Exit;

    {Output}
    Direction:=GPIO_FUNCTION_OUT;
    {Polarity}                   {Retain existing}
    Terminator:=0;               {N/A for Output}
    PullUp:=0;                   {N/A for Output}
    {State}                      {Retain existing}
    Result:=RPiGPIOExpanderSetConfig(Pin,Direction,Polarity,Terminator,PullUp,State);
   end;
 end;
end;

{==============================================================================}
{==============================================================================}
{RTL Virtual GPIO Functions}
function SysVirtualGPIOInputGet(Pin:LongWord):LongWord;
{Get the current state of a virtual GPIO input pin}
{Pin: The pin to get the state for (eg VIRTUAL_GPIO_PIN_1)}
{Return: The current state (eg GPIO_LEVEL_HIGH) or GPIO_LEVEL_UNKNOWN on failure}
begin
 {}
 Result:=GPIO_LEVEL_UNKNOWN;

 if RPiGPIOExpander = nil then Exit;

 Result:=RPiGPIOExpanderInputGet(@RPiGPIOExpander.GPIO,Pin);
end;

{==============================================================================}

function SysVirtualGPIOOutputSet(Pin,Level:LongWord):LongWord;
{Set the state of a virtual GPIO output pin}
{Pin: The pin to set the state for (eg GPIO_PIN_1)}
{Level: The state to set the pin to (eg GPIO_LEVEL_HIGH)}
{Return: ERROR_SUCCESS if completed successfully or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 if RPiGPIOExpander = nil then Exit;

 Result:=RPiGPIOExpanderOutputSet(@RPiGPIOExpander.GPIO,Pin,Level);
end;

{==============================================================================}

function SysVirtualGPIOFunctionGet(Pin:LongWord):LongWord;
{Get the current function of a virtual GPIO pin}
{Pin: The pin to get the function for (eg GPIO_PIN_1)}
{Return: The current function of the pin (eg GPIO_FUNCTION_IN) or GPIO_FUNCTION_UNKNOWN on failure}
begin
 {}
 Result:=GPIO_FUNCTION_UNKNOWN;

 if RPiGPIOExpander = nil then Exit;

 Result:=RPiGPIOExpanderFunctionGet(@RPiGPIOExpander.GPIO,Pin);
end;

{==============================================================================}

function SysVirtualGPIOFunctionSelect(Pin,Mode:LongWord):LongWord;
{Change the function of a virtual GPIO pin}
{Pin: The pin to change the function for (eg GPIO_PIN_1)}
{Mode: The function to set for the pin (eg GPIO_FUNCTION_OUT)}
{Return: ERROR_SUCCESS if completed successfully or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 if RPiGPIOExpander = nil then Exit;

 Result:=RPiGPIOExpanderFunctionSelect(@RPiGPIOExpander.GPIO,Pin,Mode);
end;

{==============================================================================}
{==============================================================================}
{RPiGPIO Expander Helper Functions}
function RPiGPIOExpanderGetState(GPIO:LongWord;var State:LongWord):LongWord;
{Raspberry Pi Firmware Mailbox call for Get GPIO State}
var
 Size:LongWord;
 Response:LongWord;
 Header:PRPiGPIOMailboxHeader;
 Footer:PRPiGPIOMailboxFooter;
 Tag:PRPiGPIOMailboxTagGetGPIOState;
begin
 {}
 Result:=ERROR_OPERATION_FAILED;

 {Calculate Size}
 Size:=SizeOf(TRPiGPIOMailboxHeader) + SizeOf(TRPiGPIOMailboxTagGetGPIOState) + SizeOf(TRPiGPIOMailboxFooter);

 {Allocate Mailbox Buffer}
 {$IFDEF CPUARMV6}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 {$ELSE}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 {$ENDIF}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=RPIGPIO_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PRPiGPIOMailboxTagGetGPIOState(PtrUInt(Header) + PtrUInt(SizeOf(TRPiGPIOMailboxHeader)));
  Tag.Header.Tag:=RPIGPIO_MBOX_TAG_GET_GPIO_STATE;
  Tag.Header.Size:=SizeOf(TRPiGPIOMailboxTagGetGPIOState) - SizeOf(TRPiGPIOMailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.GPIO:=GPIO + RPIGPIOEXP_GPIO_PIN_OFFSET;

  {Setup Footer}
  Footer:=PRPiGPIOMailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TRPiGPIOMailboxTagGetGPIOState)));
  Footer.Tag:=RPIGPIO_MBOX_TAG_END;

  {Call Mailbox}
  if MailboxPropertyCall(RPIGPIO_MAILBOX_0,RPIGPIO_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
   begin
    {$IF DEFINED(RPIGPIOEXP_DEBUG) or DEFINED(GPIO_DEBUG)}
    if GPIO_LOG_ENABLED then GPIOLogDebug(nil,'RPiGPIOEXP: RPiGPIOExpanderGetState - MailboxPropertyCall Failed');
    {$ENDIF}
    Exit;
   end;

  {Get State}
  State:=Tag.Response.State;

  {Get Result}
  Result:=ERROR_SUCCESS;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPiGPIOExpanderSetState(GPIO,State:LongWord):LongWord;
{Raspberry Pi Firmware Mailbox call for Set GPIO State}
var
 Size:LongWord;
 Response:LongWord;
 Header:PRPiGPIOMailboxHeader;
 Footer:PRPiGPIOMailboxFooter;
 Tag:PRPiGPIOMailboxTagSetGPIOState;
begin
 {}
 Result:=ERROR_OPERATION_FAILED;

 {Calculate Size}
 Size:=SizeOf(TRPiGPIOMailboxHeader) + SizeOf(TRPiGPIOMailboxTagSetGPIOState) + SizeOf(TRPiGPIOMailboxFooter);

 {Allocate Mailbox Buffer}
 {$IFDEF CPUARMV6}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 {$ELSE}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 {$ENDIF}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=RPIGPIO_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PRPiGPIOMailboxTagSetGPIOState(PtrUInt(Header) + PtrUInt(SizeOf(TRPiGPIOMailboxHeader)));
  Tag.Header.Tag:=RPIGPIO_MBOX_TAG_SET_GPIO_STATE;
  Tag.Header.Size:=SizeOf(TRPiGPIOMailboxTagSetGPIOState) - SizeOf(TRPiGPIOMailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.GPIO:=GPIO + RPIGPIOEXP_GPIO_PIN_OFFSET;
  Tag.Request.State:=State;

  {Setup Footer}
  Footer:=PRPiGPIOMailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TRPiGPIOMailboxTagSetGPIOState)));
  Footer.Tag:=RPIGPIO_MBOX_TAG_END;

  {Call Mailbox}
  if MailboxPropertyCall(RPIGPIO_MAILBOX_0,RPIGPIO_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
   begin
    {$IF DEFINED(RPIGPIOEXP_DEBUG) or DEFINED(GPIO_DEBUG)}
    if GPIO_LOG_ENABLED then GPIOLogDebug(nil,'RPiGPIOEXP: RPiGPIOExpanderSetState - MailboxPropertyCall Failed');
    {$ENDIF}
    Exit;
   end;

  {Get Result}
  Result:=ERROR_SUCCESS;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPiGPIOExpanderGetConfig(GPIO:LongWord;var Direction,Polarity,Terminator,PullUp:LongWord):LongWord;
{Raspberry Pi Firmware Mailbox call for Get GPIO Config}
var
 Size:LongWord;
 Response:LongWord;
 Header:PRPiGPIOMailboxHeader;
 Footer:PRPiGPIOMailboxFooter;
 Tag:PRPiGPIOMailboxTagGetGPIOConfig;
begin
 {}
 Result:=ERROR_OPERATION_FAILED;

 {Calculate Size}
 Size:=SizeOf(TRPiGPIOMailboxHeader) + SizeOf(TRPiGPIOMailboxTagGetGPIOConfig) + SizeOf(TRPiGPIOMailboxFooter);

 {Allocate Mailbox Buffer}
 {$IFDEF CPUARMV6}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 {$ELSE}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 {$ENDIF}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=RPIGPIO_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PRPiGPIOMailboxTagGetGPIOConfig(PtrUInt(Header) + PtrUInt(SizeOf(TRPiGPIOMailboxHeader)));
  Tag.Header.Tag:=RPIGPIO_MBOX_TAG_GET_GPIO_CONFIG;
  Tag.Header.Size:=SizeOf(TRPiGPIOMailboxTagGetGPIOConfig) - SizeOf(TRPiGPIOMailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.GPIO:=GPIO + RPIGPIOEXP_GPIO_PIN_OFFSET;

  {Setup Footer}
  Footer:=PRPiGPIOMailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TRPiGPIOMailboxTagGetGPIOConfig)));
  Footer.Tag:=RPIGPIO_MBOX_TAG_END;

  {Call Mailbox}
  if MailboxPropertyCall(RPIGPIO_MAILBOX_0,RPIGPIO_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
   begin
    {$IF DEFINED(RPIGPIOEXP_DEBUG) or DEFINED(GPIO_DEBUG)}
    if GPIO_LOG_ENABLED then GPIOLogDebug(nil,'RPiGPIOEXP: RPiGPIOExpanderGetConfig - MailboxPropertyCall Failed');
    {$ENDIF}
    Exit;
   end;

  {Get Config}
  Direction:=Tag.Response.Direction;
  Polarity:=Tag.Response.Polarity;
  Terminator:=Tag.Response.Terminator;
  PullUp:=Tag.Response.PullUp;

  {Get Result}
  Result:=ERROR_SUCCESS;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}

function RPiGPIOExpanderSetConfig(GPIO,Direction,Polarity,Terminator,PullUp,State:LongWord):LongWord;
{Raspberry Pi Firmware Mailbox call for Set GPIO Config}
var
 Size:LongWord;
 Response:LongWord;
 Header:PRPiGPIOMailboxHeader;
 Footer:PRPiGPIOMailboxFooter;
 Tag:PRPiGPIOMailboxTagSetGPIOConfig;
begin
 {}
 Result:=ERROR_OPERATION_FAILED;

 {Calculate Size}
 Size:=SizeOf(TRPiGPIOMailboxHeader) + SizeOf(TRPiGPIOMailboxTagSetGPIOConfig) + SizeOf(TRPiGPIOMailboxFooter);

 {Allocate Mailbox Buffer}
 {$IFDEF CPUARMV6}
 Header:=GetSharedAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 {$ELSE}
 Header:=GetNoCacheAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 {$ENDIF}
 if Header = nil then Header:=GetAlignedMem(Size,SIZE_16); {Must be 16 byte aligned}
 if Header = nil then Exit;
 try
  {Clear Buffer}
  FillChar(Header^,Size,0);

  {Setup Header}
  Header.Size:=Size;
  Header.Code:=RPIGPIO_MBOX_REQUEST_CODE;

  {Setup Tag}
  Tag:=PRPiGPIOMailboxTagSetGPIOConfig(PtrUInt(Header) + PtrUInt(SizeOf(TRPiGPIOMailboxHeader)));
  Tag.Header.Tag:=RPIGPIO_MBOX_TAG_SET_GPIO_CONFIG;
  Tag.Header.Size:=SizeOf(TRPiGPIOMailboxTagSetGPIOConfig) - SizeOf(TRPiGPIOMailboxTagHeader);
  Tag.Header.Length:=SizeOf(Tag.Request);
  Tag.Request.GPIO:=GPIO + RPIGPIOEXP_GPIO_PIN_OFFSET;
  Tag.Request.Direction:=Direction;
  Tag.Request.Polarity:=Polarity;
  Tag.Request.Terminator:=Terminator;
  Tag.Request.PullUp:=PullUp;
  Tag.Request.State:=State;

  {Setup Footer}
  Footer:=PRPiGPIOMailboxFooter(PtrUInt(Tag) + PtrUInt(SizeOf(TRPiGPIOMailboxTagSetGPIOConfig)));
  Footer.Tag:=RPIGPIO_MBOX_TAG_END;

  {Call Mailbox}
  if MailboxPropertyCall(RPIGPIO_MAILBOX_0,RPIGPIO_MAILBOX0_CHANNEL_PROPERTYTAGS_ARMVC,Header,Response) <> ERROR_SUCCESS then
   begin
    {$IF DEFINED(RPIGPIOEXP_DEBUG) or DEFINED(GPIO_DEBUG)}
    if GPIO_LOG_ENABLED then GPIOLogDebug(nil,'RPiGPIOEXP: RPiGPIOExpanderSetConfig - MailboxPropertyCall Failed');
    {$ENDIF}
    Exit;
   end;

  {Get Result}
  Result:=ERROR_SUCCESS;
 finally
  FreeMem(Header);
 end;
end;

{==============================================================================}
{==============================================================================}

initialization
 RPiGPIOExpanderInit;

{==============================================================================}

{finalization}
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
