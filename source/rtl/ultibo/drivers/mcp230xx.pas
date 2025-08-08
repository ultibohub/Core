{
Microchip MCP230XX I/O expander Driver.

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

  Linux - \drivers\gpio\gpio-mcp23s08.c
  Adafruit - https://github.com/adafruit/Adafruit_Python_GPIO

References
==========

  MCP23008 - http://ww1.microchip.com/downloads/en/DeviceDoc/21919e.pdf
  MCP23016 - http://ww1.microchip.com/downloads/en/DeviceDoc/20090C.pdf
  MCP23017 - http://ww1.microchip.com/downloads/en/DeviceDoc/21952b.pdf

Microchip MCP230XX
==================

 The Microchip MCP23008 and MCP23017 are 8 or 16 bit I/O expanders that provide GPIO pin control
 functions over an I2C connection.

 The device can be represented in Ultibo as a standard GPIO device which is accessable via the
 GPIO unit functions. Because the MCP230XX is a chip that can be used and configured in multiple
 different scenarios this unit does not autocreate a GPIO device, instead you need to call the
 function MCP23008GPIOCreate or MCP23017GPIOCreate and pass an I2C device and address. The
 functions will create and return a GPIO device with the appropriate number of pins and other
 information for the specified chip, the returned devices will have been registered with the
 GPIO device unit and started ready for use.

 Both devices also come in an SPI interface version, these are not currently supported by this
 unit.

 Note: This unit does not currently implement the interrupt capabilities of the MCP230XX chips
 however it could be expanded to allow the interrupt pin to be connected to a GPIO pin on the
 SoC and use a trigger event from that to enable GPIOInputWait/GPIOInputEvent functions for the
 MCP230XX chips.

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit MCP230XX;

interface

uses
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  Devices,
  GPIO,
  I2C,
  SysUtils;

{==============================================================================}
{Global definitions}
{$INCLUDE ..\core\GlobalDefines.inc}

{==============================================================================}
const
 {MCP230XX specific constants}
 MCP23008_GPIO_DESCRIPTION = 'Microchip MCP23008 8-bit I/O Expander';  {Description of MCP23008 device}
 MCP23017_GPIO_DESCRIPTION = 'Microchip MCP23016 16-bit I/O Expander'; {Description of MCP23017 device}

 MCP23008_GPIO_MIN_PIN = GPIO_PIN_0;
 MCP23008_GPIO_MAX_PIN = GPIO_PIN_7;
 MCP23008_GPIO_PIN_COUNT = 8;

 MCP23017_GPIO_MIN_PIN = GPIO_PIN_0;
 MCP23017_GPIO_MAX_PIN = GPIO_PIN_15;
 MCP23017_GPIO_PIN_COUNT = 16;

 MCP230XX_GPIO_MAX_LEVEL = GPIO_LEVEL_HIGH;

 MCP230XX_GPIO_MAX_PULL = GPIO_PULL_UP;

 MCP230XX_GPIO_MIN_FUNCTION = GPIO_FUNCTION_IN;
 MCP230XX_GPIO_MAX_FUNCTION = GPIO_FUNCTION_OUT;

 {MCP230XX chip constants}
 MCP230XX_CHIP_MCP23008   = 0;
 MCP230XX_CHIP_MCP23017   = 1;

 {MCP230XX I2C constants}
 MCP230XX_I2C_RATE = 400000; {Default I2C clock rate (Device supports 100KHz, 400KHz and 1.7MHz}

 MCP23008_I2C_SIZE = 1;      {Number of bytes to read/write all pin values for any register}
 MCP23017_I2C_SIZE = 2;      {Number of bytes to read/write all pin values for any register}

 MCP230XX_I2C_MAX_SIZE = 2;  {Maximum number of bytes to read/write all pin values for any register}

 MCP23008_I2C_MAX_REG = $0A; {Maximum register address for the I2C interface}
 MCP23017_I2C_MAX_REG = $1A; {Maximum register address for the I2C interface (Only 21 (0x15) registers exists, mappings differ between modes)}

{==============================================================================}
type
 {MCP230XX specific types}
 PMCP230XXGPIO = ^TMCP230XXGPIO;
 TMCP230XXGPIO = record
  {GPIO Properties}
  GPIO:TGPIODevice;
  {MCP230XX Properties}
  I2C:PI2CDevice;                                          {The I2C device this GPIO is connected to}
  Address:Word;                                            {The I2C address of the device}
  Chip:LongWord;                                           {The chip type (eg MCP230XX_CHIP_MCP23008)}
  Size:LongWord;                                           {Size of an I2C read/write for the device}
  GPIOReg:Byte;                                            {GPIO port register for the device}
  GPPUReg:Byte;                                            {GPPU pull up resistor register for the device}
  IODIRReg:Byte;                                           {IODIR I/O direction register for the device}
  GPIOValues:array[0..MCP230XX_I2C_MAX_SIZE - 1] of Byte;  {Buffer for GPIO port values (Output only)}
  GPPUValues:array[0..MCP230XX_I2C_MAX_SIZE - 1] of Byte;  {Buffer for GPPU pull up values}
  IODIRValues:array[0..MCP230XX_I2C_MAX_SIZE - 1] of Byte; {Buffer for IODIR I/O direction values}
 end;

{==============================================================================}
{var}
 {MCP230XX specific variables}

{==============================================================================}
{Initialization Functions}

{==============================================================================}
{MCP230XX Functions}
function MCP23008GPIOCreate(I2C:PI2CDevice;Address:Word):PGPIODevice;{$IFDEF API_EXPORT_MCP230XX} stdcall; public name 'mcp23008_gpio_create';{$ENDIF}
function MCP23017GPIOCreate(I2C:PI2CDevice;Address:Word):PGPIODevice;{$IFDEF API_EXPORT_MCP230XX} stdcall; public name 'mcp23017_gpio_create';{$ENDIF}

function MCP230XXGPIODestroy(GPIO:PGPIODevice):LongWord;{$IFDEF API_EXPORT_MCP230XX} stdcall; public name 'mcp230xx_gpio_destroy';{$ENDIF}

{==============================================================================}
{MCP230XX GPIO Functions}
function MCP230XXGPIOStart(GPIO:PGPIODevice):LongWord;
function MCP230XXGPIOStop(GPIO:PGPIODevice):LongWord;

function MCP230XXGPIORead(GPIO:PGPIODevice;Reg:LongWord):LongWord;
procedure MCP230XXGPIOWrite(GPIO:PGPIODevice;Reg,Value:LongWord);

function MCP230XXGPIOInputGet(GPIO:PGPIODevice;Pin:LongWord):LongWord;

function MCP230XXGPIOOutputSet(GPIO:PGPIODevice;Pin,Level:LongWord):LongWord;

function MCP230XXGPIOPullGet(GPIO:PGPIODevice;Pin:LongWord):LongWord;
function MCP230XXGPIOPullSelect(GPIO:PGPIODevice;Pin,Mode:LongWord):LongWord;

function MCP230XXGPIOFunctionGet(GPIO:PGPIODevice;Pin:LongWord):LongWord;
function MCP230XXGPIOFunctionSelect(GPIO:PGPIODevice;Pin,Mode:LongWord):LongWord;

{==============================================================================}
{MCP230XX Helper Functions}

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
{var}
 {MCP230XX specific variables}

{==============================================================================}
{==============================================================================}
{Initialization Functions}

{==============================================================================}
{==============================================================================}
{MCP230XX Functions}
function MCP23008GPIOCreate(I2C:PI2CDevice;Address:Word):PGPIODevice;{$IFDEF API_EXPORT_MCP230XX} stdcall;{$ENDIF}
{Create, register and start a new MCP23008 GPIO device connected to the specified I2C device}
{I2C: The I2C device this MCP23008 is connected to}
{Address: The I2C address for this MCP23008}
{Return: Pointer to the new GPIO device or nil on failure}
var
 Status:LongWord;

 MCP230XXGPIO:PMCP230XXGPIO;
begin
 {}
 Result:=nil;

 {$IF DEFINED(MCP230XX_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(nil,'MCP23008: GPIO Create (Address=' + IntToHex(Address,8) + ')');
 {$ENDIF}

 {Check I2C}
 if I2C = nil then Exit;

 {Check Address}
 if Address = I2C_ADDRESS_INVALID then Exit;

 {Create GPIO}
 MCP230XXGPIO:=PMCP230XXGPIO(GPIODeviceCreateEx(SizeOf(TMCP230XXGPIO)));
 if MCP230XXGPIO <> nil then
  begin
   {Update GPIO}
   {Device}
   MCP230XXGPIO.GPIO.Device.DeviceBus:=DEVICE_BUS_I2C;
   MCP230XXGPIO.GPIO.Device.DeviceType:=GPIO_TYPE_NONE;
   MCP230XXGPIO.GPIO.Device.DeviceFlags:=GPIO_FLAG_PULL_UP;
   MCP230XXGPIO.GPIO.Device.DeviceData:=nil;
   MCP230XXGPIO.GPIO.Device.DeviceDescription:=MCP23008_GPIO_DESCRIPTION;
   {GPIO}
   MCP230XXGPIO.GPIO.GPIOState:=GPIO_STATE_DISABLED;
   MCP230XXGPIO.GPIO.DeviceStart:=MCP230XXGPIOStart;
   MCP230XXGPIO.GPIO.DeviceStop:=MCP230XXGPIOStop;
   MCP230XXGPIO.GPIO.DeviceRead:=MCP230XXGPIORead;
   MCP230XXGPIO.GPIO.DeviceWrite:=MCP230XXGPIOWrite;
   MCP230XXGPIO.GPIO.DeviceInputGet:=MCP230XXGPIOInputGet;
   MCP230XXGPIO.GPIO.DeviceOutputSet:=MCP230XXGPIOOutputSet;
   MCP230XXGPIO.GPIO.DevicePullGet:=MCP230XXGPIOPullGet;
   MCP230XXGPIO.GPIO.DevicePullSelect:=MCP230XXGPIOPullSelect;
   MCP230XXGPIO.GPIO.DeviceFunctionGet:=MCP230XXGPIOFunctionGet;
   MCP230XXGPIO.GPIO.DeviceFunctionSelect:=MCP230XXGPIOFunctionSelect;
   {Driver}
   MCP230XXGPIO.GPIO.Address:=nil;
   MCP230XXGPIO.GPIO.Properties.Flags:=MCP230XXGPIO.GPIO.Device.DeviceFlags;
   MCP230XXGPIO.GPIO.Properties.PinMin:=MCP23008_GPIO_MIN_PIN;
   MCP230XXGPIO.GPIO.Properties.PinMax:=MCP23008_GPIO_MAX_PIN;
   MCP230XXGPIO.GPIO.Properties.PinCount:=MCP23008_GPIO_PIN_COUNT;
   MCP230XXGPIO.GPIO.Properties.FunctionMin:=MCP230XX_GPIO_MIN_FUNCTION;
   MCP230XXGPIO.GPIO.Properties.FunctionMax:=MCP230XX_GPIO_MAX_FUNCTION;
   MCP230XXGPIO.GPIO.Properties.FunctionCount:=2;
   {MCP230XX}
   MCP230XXGPIO.I2C:=I2C;
   MCP230XXGPIO.Address:=Address;
   MCP230XXGPIO.Chip:=MCP230XX_CHIP_MCP23008;
   MCP230XXGPIO.Size:=MCP23008_I2C_SIZE;
   MCP230XXGPIO.GPIOReg:=$09;
   MCP230XXGPIO.GPPUReg:=$06;
   MCP230XXGPIO.IODIRReg:=$00;

   {Register GPIO}
   Status:=GPIODeviceRegister(@MCP230XXGPIO.GPIO);
   if Status = ERROR_SUCCESS then
    begin
     {Start GPIO}
     Status:=GPIODeviceStart(@MCP230XXGPIO.GPIO);
     if Status = ERROR_SUCCESS then
      begin
       {Return Result}
       Result:=PGPIODevice(MCP230XXGPIO);
      end
     else
      begin
       if GPIO_LOG_ENABLED then GPIOLogError(nil,'MCP230XX: Failed to start new GPIO device: ' + ErrorToString(Status));

       {Deregister GPIO}
       GPIODeviceDeregister(@MCP230XXGPIO.GPIO);

       {Destroy GPIO}
       GPIODeviceDestroy(@MCP230XXGPIO.GPIO);
      end;
    end
   else
    begin
     if GPIO_LOG_ENABLED then GPIOLogError(nil,'MCP230XX: Failed to register new GPIO device: ' + ErrorToString(Status));

     {Destroy GPIO}
     GPIODeviceDestroy(@MCP230XXGPIO.GPIO);
    end;
  end
 else
  begin
   if GPIO_LOG_ENABLED then GPIOLogError(nil,'MCP230XX: Failed to create new GPIO device');
  end;
end;

{==============================================================================}

function MCP23017GPIOCreate(I2C:PI2CDevice;Address:Word):PGPIODevice;{$IFDEF API_EXPORT_MCP230XX} stdcall;{$ENDIF}
{Create, register and start a new MCP23017 GPIO device connected to the specified I2C device}
{I2C: The I2C device this MCP23017 is connected to}
{Address: The I2C address for this MCP23017}
{Return: Pointer to the new GPIO device or nil on failure}
var
 Status:LongWord;

 MCP230XXGPIO:PMCP230XXGPIO;
begin
 {}
 Result:=nil;

 {$IF DEFINED(MCP230XX_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(nil,'MCP23017: GPIO Create (Address=' + IntToHex(Address,8) + ')');
 {$ENDIF}

 {Check I2C}
 if I2C = nil then Exit;

 {Check Address}
 if Address = I2C_ADDRESS_INVALID then Exit;

 {Create GPIO}
 MCP230XXGPIO:=PMCP230XXGPIO(GPIODeviceCreateEx(SizeOf(TMCP230XXGPIO)));
 if MCP230XXGPIO <> nil then
  begin
   {Update GPIO}
   {Device}
   MCP230XXGPIO.GPIO.Device.DeviceBus:=DEVICE_BUS_I2C;
   MCP230XXGPIO.GPIO.Device.DeviceType:=GPIO_TYPE_NONE;
   MCP230XXGPIO.GPIO.Device.DeviceFlags:=GPIO_FLAG_PULL_UP;
   MCP230XXGPIO.GPIO.Device.DeviceData:=nil;
   MCP230XXGPIO.GPIO.Device.DeviceDescription:=MCP23017_GPIO_DESCRIPTION;
   {GPIO}
   MCP230XXGPIO.GPIO.GPIOState:=GPIO_STATE_DISABLED;
   MCP230XXGPIO.GPIO.DeviceStart:=MCP230XXGPIOStart;
   MCP230XXGPIO.GPIO.DeviceStop:=MCP230XXGPIOStop;
   MCP230XXGPIO.GPIO.DeviceRead:=MCP230XXGPIORead;
   MCP230XXGPIO.GPIO.DeviceWrite:=MCP230XXGPIOWrite;
   MCP230XXGPIO.GPIO.DeviceInputGet:=MCP230XXGPIOInputGet;
   MCP230XXGPIO.GPIO.DeviceOutputSet:=MCP230XXGPIOOutputSet;
   MCP230XXGPIO.GPIO.DevicePullGet:=MCP230XXGPIOPullGet;
   MCP230XXGPIO.GPIO.DevicePullSelect:=MCP230XXGPIOPullSelect;
   MCP230XXGPIO.GPIO.DeviceFunctionGet:=MCP230XXGPIOFunctionGet;
   MCP230XXGPIO.GPIO.DeviceFunctionSelect:=MCP230XXGPIOFunctionSelect;
   {Driver}
   MCP230XXGPIO.GPIO.Address:=nil;
   MCP230XXGPIO.GPIO.Properties.Flags:=MCP230XXGPIO.GPIO.Device.DeviceFlags;
   MCP230XXGPIO.GPIO.Properties.PinMin:=MCP23017_GPIO_MIN_PIN;
   MCP230XXGPIO.GPIO.Properties.PinMax:=MCP23017_GPIO_MAX_PIN;
   MCP230XXGPIO.GPIO.Properties.PinCount:=MCP23017_GPIO_PIN_COUNT;
   MCP230XXGPIO.GPIO.Properties.FunctionMin:=MCP230XX_GPIO_MIN_FUNCTION;
   MCP230XXGPIO.GPIO.Properties.FunctionMax:=MCP230XX_GPIO_MAX_FUNCTION;
   MCP230XXGPIO.GPIO.Properties.FunctionCount:=2;
   {MCP230XX}
   MCP230XXGPIO.I2C:=I2C;
   MCP230XXGPIO.Address:=Address;
   MCP230XXGPIO.Chip:=MCP230XX_CHIP_MCP23017;
   MCP230XXGPIO.Size:=MCP23017_I2C_SIZE;
   MCP230XXGPIO.GPIOReg:=$12;
   MCP230XXGPIO.GPPUReg:=$0C;
   MCP230XXGPIO.IODIRReg:=$00;

   {Register GPIO}
   Status:=GPIODeviceRegister(@MCP230XXGPIO.GPIO);
   if Status = ERROR_SUCCESS then
    begin
     {Start GPIO}
     Status:=GPIODeviceStart(@MCP230XXGPIO.GPIO);
     if Status = ERROR_SUCCESS then
      begin
       {Return Result}
       Result:=PGPIODevice(MCP230XXGPIO);
      end
     else
      begin
       if GPIO_LOG_ENABLED then GPIOLogError(nil,'MCP230XX: Failed to start new GPIO device: ' + ErrorToString(Status));

       {Deregister GPIO}
       GPIODeviceDeregister(@MCP230XXGPIO.GPIO);

       {Destroy GPIO}
       GPIODeviceDestroy(@MCP230XXGPIO.GPIO);
      end;
    end
   else
    begin
     if GPIO_LOG_ENABLED then GPIOLogError(nil,'MCP230XX: Failed to register new GPIO device: ' + ErrorToString(Status));

     {Destroy GPIO}
     GPIODeviceDestroy(@MCP230XXGPIO.GPIO);
    end;
  end
 else
  begin
   if GPIO_LOG_ENABLED then GPIOLogError(nil,'MCP230XX: Failed to create new GPIO device');
  end;
end;

{==============================================================================}

function MCP230XXGPIODestroy(GPIO:PGPIODevice):LongWord;{$IFDEF API_EXPORT_MCP230XX} stdcall;{$ENDIF}
{Stop, deregister and destroy an MCP230XX GPIO device created by this driver}
{GPIO: The GPIO device to destroy}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check GPIO}
 if GPIO = nil then Exit;

 {$IF DEFINED(MCP230XX_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'MCP230XX: GPIO Destroy');
 {$ENDIF}

 {Stop GPIO}
 Result:=GPIODeviceStop(GPIO);
 if Result = ERROR_SUCCESS then
  begin
   {Deregister GPIO}
   Result:=GPIODeviceDeregister(GPIO);
   if Result = ERROR_SUCCESS then
    begin
     {Destroy GPIO}
     Result:=GPIODeviceDestroy(GPIO);
     if Result <> ERROR_SUCCESS then
      begin
       if GPIO_LOG_ENABLED then GPIOLogError(nil,'MCP230XX: Failed to destroy GPIO device: ' + ErrorToString(Result));
      end;
    end
   else
    begin
     if GPIO_LOG_ENABLED then GPIOLogError(nil,'MCP230XX: Failed to deregister GPIO device: ' + ErrorToString(Result));
    end;
  end
 else
  begin
   if GPIO_LOG_ENABLED then GPIOLogError(nil,'MCP230XX: Failed to stop GPIO device: ' + ErrorToString(Result));
  end;
end;

{==============================================================================}
{==============================================================================}
{MCP230XX GPIO Functions}
function MCP230XXGPIOStart(GPIO:PGPIODevice):LongWord;
{Implementation of GPIODeviceStart API for MCP230XX}
{Note: Not intended to be called directly by applications, use GPIODeviceStart instead}
var
 Count:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check GPIO}
 if GPIO = nil then Exit;

 {$IF DEFINED(MCP230XX_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'MCP230XX: GPIO Start');
 {$ENDIF}

 {Start I2C Device}
 if I2CDeviceStart(PMCP230XXGPIO(GPIO).I2C,MCP230XX_I2C_RATE) <> ERROR_SUCCESS then
  begin
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;

 {Read IODIR Values}
 if I2CDeviceWriteRead(PMCP230XXGPIO(GPIO).I2C,PMCP230XXGPIO(GPIO).Address,@PMCP230XXGPIO(GPIO).IODIRReg,SizeOf(Byte),@PMCP230XXGPIO(GPIO).IODIRValues,PMCP230XXGPIO(GPIO).Size,Count) <> ERROR_SUCCESS then
  begin
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;

 {$IF DEFINED(MCP230XX_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'MCP230XX:  IODIRValues[0]=' + IntToHex(PMCP230XXGPIO(GPIO).IODIRValues[0],2));
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'MCP230XX:  IODIRValues[1]=' + IntToHex(PMCP230XXGPIO(GPIO).IODIRValues[1],2));
 {$ENDIF}

 {Read GPPU Values}
 if I2CDeviceWriteRead(PMCP230XXGPIO(GPIO).I2C,PMCP230XXGPIO(GPIO).Address,@PMCP230XXGPIO(GPIO).GPPUReg,SizeOf(Byte),@PMCP230XXGPIO(GPIO).GPPUValues,PMCP230XXGPIO(GPIO).Size,Count) <> ERROR_SUCCESS then
  begin
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;

 {$IF DEFINED(MCP230XX_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'MCP230XX:  GPPUValues[0]=' + IntToHex(PMCP230XXGPIO(GPIO).GPPUValues[0],2));
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'MCP230XX:  GPPUValues[1]=' + IntToHex(PMCP230XXGPIO(GPIO).GPPUValues[1],2));
 {$ENDIF}

 {Clear GPIO Values (All pins default to Low on reset)}
 FillChar(PMCP230XXGPIO(GPIO).GPIOValues,SizeOf(PMCP230XXGPIO(GPIO).GPIOValues),0);
 if I2CDeviceWriteWrite(PMCP230XXGPIO(GPIO).I2C,PMCP230XXGPIO(GPIO).Address,@PMCP230XXGPIO(GPIO).GPIOReg,SizeOf(Byte),@PMCP230XXGPIO(GPIO).GPIOValues,PMCP230XXGPIO(GPIO).Size,Count) <> ERROR_SUCCESS then
  begin
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;

 {$IF DEFINED(MCP230XX_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'MCP230XX:  GPIOValues[0]=' + IntToHex(PMCP230XXGPIO(GPIO).GPIOValues[0],2));
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'MCP230XX:  GPIOValues[1]=' + IntToHex(PMCP230XXGPIO(GPIO).GPIOValues[1],2));
 {$ENDIF}

 {Create Pins}
 SetLength(GPIO.Pins,GPIO.Properties.PinCount);

 {Setup Pins}
 for Count:=0 to GPIO.Properties.PinCount - 1 do
  begin
   GPIO.Pins[Count].GPIO:=GPIO;
   GPIO.Pins[Count].Pin:=Count;
   GPIO.Pins[Count].Flags:=GPIO_EVENT_FLAG_NONE;
   GPIO.Pins[Count].Trigger:=GPIO_TRIGGER_NONE;
   GPIO.Pins[Count].Count:=0;
   GPIO.Pins[Count].Event:=INVALID_HANDLE_VALUE;
   GPIO.Pins[Count].Events:=nil;
  end;

 {Return Result}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function MCP230XXGPIOStop(GPIO:PGPIODevice):LongWord;
{Implementation of GPIODeviceStop API for MCP230XX}
{Note: Not intended to be called directly by applications, use GPIODeviceStop instead}
var
 Count:LongWord;
 Event:PGPIOEvent;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check GPIO}
 if GPIO = nil then Exit;

 {$IF DEFINED(MCP230XX_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'MCP230XX: GPIO Stop');
 {$ENDIF}

 {Release Pins}
 for Count:=0 to GPIO.Properties.PinCount - 1 do
  begin
   if GPIO.Pins[Count].Event <> INVALID_HANDLE_VALUE then
    begin
     EventDestroy(GPIO.Pins[Count].Event);
    end;

   if GPIO.Pins[Count].Events <> nil then
    begin
     Event:=GPIO.Pins[Count].Events;
     while Event <> nil do
      begin
       {Deregister Event}
       GPIODeviceDeregisterEvent(GPIO,@GPIO.Pins[Count],Event);

       {Destroy Event}
       GPIODeviceDestroyEvent(GPIO,Event);

       Event:=GPIO.Pins[Count].Events;
      end;
    end;
  end;

 {Destroy Pins}
 SetLength(GPIO.Pins,0);

 {Return Result}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function MCP230XXGPIORead(GPIO:PGPIODevice;Reg:LongWord):LongWord;
{Implementation of GPIODeviceRead API for MCP230XX}
{Note: Not intended to be called directly by applications, use GPIODeviceRead instead}

{Note: Will read only a single register byte}
var
 Value:Byte;
 Count:LongWord;
begin
 {}
 Result:=0;

 {Check GPIO}
 if GPIO = nil then Exit;

 {$IF DEFINED(MCP230XX_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'MCP230XX: GPIO Read (Reg=' + IntToHex(Reg,8) + ')');
 {$ENDIF}

 {Check Chip}
 case PMCP230XXGPIO(GPIO).Chip of
  MCP230XX_CHIP_MCP23008:begin
    {Check Reg}
    if Reg > MCP23008_I2C_MAX_REG then Exit;
   end;
  MCP230XX_CHIP_MCP23017:begin
    {Check Reg}
    if Reg > MCP23017_I2C_MAX_REG then Exit;
   end;
 end;

 {Read Register}
 if I2CDeviceWriteRead(PMCP230XXGPIO(GPIO).I2C,PMCP230XXGPIO(GPIO).Address,@Reg,SizeOf(Byte),@Value,SizeOf(Byte),Count) = ERROR_SUCCESS then
  begin
   Result:=Value;
  end;
end;

{==============================================================================}

procedure MCP230XXGPIOWrite(GPIO:PGPIODevice;Reg,Value:LongWord);
{Implementation of GPIODeviceWrite API for MCP230XX}
{Note: Not intended to be called directly by applications, use GPIODeviceWrite instead}

{Note: Will write only a single register byte}
var
 Count:LongWord;
begin
 {}
 {Check GPIO}
 if GPIO = nil then Exit;

 {$IF DEFINED(MCP230XX_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'MCP230XX: GPIO Write (Reg=' + IntToHex(Reg,8) + ' Value=' + IntToHex(Value,8) + ')');
 {$ENDIF}

 {Check Chip}
 case PMCP230XXGPIO(GPIO).Chip of
  MCP230XX_CHIP_MCP23008:begin
    {Check Reg}
    if Reg > MCP23008_I2C_MAX_REG then Exit;
   end;
  MCP230XX_CHIP_MCP23017:begin
    {Check Reg}
    if Reg > MCP23017_I2C_MAX_REG then Exit;
   end;
 end;

 {Write Register}
 I2CDeviceWriteWrite(PMCP230XXGPIO(GPIO).I2C,PMCP230XXGPIO(GPIO).Address,@Reg,SizeOf(Byte),@Value,SizeOf(Byte),Count);
end;

{==============================================================================}

function MCP230XXGPIOInputGet(GPIO:PGPIODevice;Pin:LongWord):LongWord;
{Implementation of GPIODeviceInputGet API for MCP230XX}
{Note: Not intended to be called directly by applications, use GPIODeviceInputGet instead}
var
 Reg:Byte;
 Shift:Byte;
 Count:LongWord;
 Values:array[0..MCP230XX_I2C_MAX_SIZE - 1] of Byte;
begin
 {}
 Result:=GPIO_LEVEL_UNKNOWN;

 {Check GPIO}
 if GPIO = nil then Exit;

 {$IF DEFINED(MCP230XX_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'MCP230XX: GPIO Input Get (Pin=' + GPIOPinToString(Pin) + ')');
 {$ENDIF}

 {Check Pin}
 if Pin > GPIO.Properties.PinMax then Exit;

 {Update Statistics}
 Inc(GPIO.GetCount);

 {Get Shift}
 Shift:=Pin mod 8;

 {Get Register}
 Reg:=Pin div 8;

 {Read GPIO Values}
 if I2CDeviceWriteRead(PMCP230XXGPIO(GPIO).I2C,PMCP230XXGPIO(GPIO).Address,@PMCP230XXGPIO(GPIO).GPIOReg,SizeOf(Byte),@Values,PMCP230XXGPIO(GPIO).Size,Count) = ERROR_SUCCESS then
  begin
   {Read Register}
   Result:=(Values[Reg] shr Shift) and 1;
  end;
end;

{==============================================================================}

function MCP230XXGPIOOutputSet(GPIO:PGPIODevice;Pin,Level:LongWord):LongWord;
{Implementation of GPIODeviceOutputSet API for MCP230XX}
{Note: Not intended to be called directly by applications, use GPIODeviceOutputSet instead}
var
 Reg:Byte;
 Shift:Byte;
 Count:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check GPIO}
 if GPIO = nil then Exit;

 {$IF DEFINED(MCP230XX_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'MCP230XX: GPIO Output Set (Pin=' + GPIOPinToString(Pin) + ' Level=' + GPIOLevelToString(Level) + ')');
 {$ENDIF}

 {Check Pin}
 if Pin > GPIO.Properties.PinMax then Exit;

 {Check Level}
 if Level > MCP230XX_GPIO_MAX_LEVEL then Exit;

 {Update Statistics}
 Inc(GPIO.SetCount);

 {Get Shift}
 Shift:=Pin mod 8;

 {Get Register}
 Reg:=Pin div 8;

 {Check Level}
 if Level = GPIO_LEVEL_HIGH then
  begin
   {Write Register}
   PMCP230XXGPIO(GPIO).GPIOValues[Reg]:=PMCP230XXGPIO(GPIO).GPIOValues[Reg] or (1 shl Shift);
  end
 else
  begin
   {Write Register}
   PMCP230XXGPIO(GPIO).GPIOValues[Reg]:=PMCP230XXGPIO(GPIO).GPIOValues[Reg] and not(1 shl Shift);
  end;

 {Write GPIO Values}
 Result:=I2CDeviceWriteWrite(PMCP230XXGPIO(GPIO).I2C,PMCP230XXGPIO(GPIO).Address,@PMCP230XXGPIO(GPIO).GPIOReg,SizeOf(Byte),@PMCP230XXGPIO(GPIO).GPIOValues,PMCP230XXGPIO(GPIO).Size,Count);
end;

{==============================================================================}

function MCP230XXGPIOPullGet(GPIO:PGPIODevice;Pin:LongWord):LongWord;
{Implementation of GPIODevicePullGet API for MCP230XX}
{Note: Not intended to be called directly by applications, use GPIODevicePullGet instead}
var
 Reg:Byte;
 Shift:Byte;
 Count:LongWord;
begin
 {}
 Result:=GPIO_PULL_UNKNOWN;

 {$IF DEFINED(MCP230XX_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'MCP230XX: GPIO Pull Get (Pin=' + GPIOPinToString(Pin) + ')');
 {$ENDIF}

 {Check Pin}
 if Pin > GPIO.Properties.PinMax then Exit;

 {Get Shift}
 Shift:=Pin mod 8;

 {Get Register}
 Reg:=Pin div 8;

 {Read Register}
 Result:=(PMCP230XXGPIO(GPIO).GPPUValues[Reg] shr Shift) and 1;
end;

{==============================================================================}

function MCP230XXGPIOPullSelect(GPIO:PGPIODevice;Pin,Mode:LongWord):LongWord;
{Implementation of GPIODevicePullSelect API for MCP230XX}
{Note: Not intended to be called directly by applications, use GPIODevicePullSelect instead}
var
 Reg:Byte;
 Shift:Byte;
 Count:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check GPIO}
 if GPIO = nil then Exit;

 {$IF DEFINED(MCP230XX_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'MCP230XX: GPIO Pull Select (Pin=' + GPIOPinToString(Pin) + ' Mode=' + GPIOPullToString(Mode) + ')');
 {$ENDIF}

 {Check Pin}
 if Pin > GPIO.Properties.PinMax then Exit;

 {Check Mode}
 if Mode > MCP230XX_GPIO_MAX_PULL then Exit;

 {Get Shift}
 Shift:=Pin mod 8;

 {Get Register}
 Reg:=Pin div 8;

 {Check Mode}
 if Mode = GPIO_PULL_UP then
  begin
   {Write Register}
   PMCP230XXGPIO(GPIO).GPPUValues[Reg]:=PMCP230XXGPIO(GPIO).GPPUValues[Reg] or (1 shl Shift);
  end
 else
  begin
   {Write Register}
   PMCP230XXGPIO(GPIO).GPPUValues[Reg]:=PMCP230XXGPIO(GPIO).GPPUValues[Reg] and not(1 shl Shift);
  end;

 {Write GPPU Values}
 Result:=I2CDeviceWriteWrite(PMCP230XXGPIO(GPIO).I2C,PMCP230XXGPIO(GPIO).Address,@PMCP230XXGPIO(GPIO).GPPUReg,SizeOf(Byte),@PMCP230XXGPIO(GPIO).GPPUValues,PMCP230XXGPIO(GPIO).Size,Count);
end;

{==============================================================================}

function MCP230XXGPIOFunctionGet(GPIO:PGPIODevice;Pin:LongWord):LongWord;
{Implementation of GPIODeviceFunctionGet API for MCP230XX}
{Note: Not intended to be called directly by applications, use GPIODeviceFunctionGet instead}
var
 Reg:Byte;
 Shift:Byte;
 Count:LongWord;
begin
 {}
 Result:=GPIO_FUNCTION_UNKNOWN;

 {Check GPIO}
 if GPIO = nil then Exit;

 {$IF DEFINED(MCP230XX_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'MCP230XX: GPIO Function Get (Pin=' + GPIOPinToString(Pin) + ')');
 {$ENDIF}

 {Check Pin}
 if Pin > GPIO.Properties.PinMax then Exit;

 {Get Shift}
 Shift:=Pin mod 8;

 {Get Register}
 Reg:=Pin div 8;

 {Read Register}
 if ((PMCP230XXGPIO(GPIO).IODIRValues[Reg] shr Shift) and 1) = 1 then
  begin
   Result:=GPIO_FUNCTION_IN;
  end
 else
  begin
   Result:=GPIO_FUNCTION_OUT;
  end;
end;

{==============================================================================}

function MCP230XXGPIOFunctionSelect(GPIO:PGPIODevice;Pin,Mode:LongWord):LongWord;
{Implementation of GPIODeviceFunctionSelect API for MCP230XX}
{Note: Not intended to be called directly by applications, use GPIODeviceFunctionSelect instead}
var
 Reg:Byte;
 Shift:Byte;
 Count:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check GPIO}
 if GPIO = nil then Exit;

 {$IF DEFINED(MCP230XX_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'MCP230XX: GPIO Function Select (Pin=' + GPIOPinToString(Pin) + ' Mode=' + GPIOFunctionToString(Mode) + ')');
 {$ENDIF}

 {Check Pin}
 if Pin > GPIO.Properties.PinMax then Exit;

 {Check Mode}
 if Mode > MCP230XX_GPIO_MAX_FUNCTION then Exit;

 {Get Shift}
 Shift:=Pin mod 8;

 {Get Register}
 Reg:=Pin div 8;

 {Check Mode}
 if Mode = GPIO_FUNCTION_IN then
  begin
   {Write Register}
   PMCP230XXGPIO(GPIO).IODIRValues[Reg]:=PMCP230XXGPIO(GPIO).IODIRValues[Reg] or (1 shl Shift);
  end
 else
  begin
   {Write Register}
   PMCP230XXGPIO(GPIO).IODIRValues[Reg]:=PMCP230XXGPIO(GPIO).IODIRValues[Reg] and not(1 shl Shift);
  end;

 {Write IODIR Values}
 Result:=I2CDeviceWriteWrite(PMCP230XXGPIO(GPIO).I2C,PMCP230XXGPIO(GPIO).Address,@PMCP230XXGPIO(GPIO).IODIRReg,SizeOf(Byte),@PMCP230XXGPIO(GPIO).IODIRValues,PMCP230XXGPIO(GPIO).Size,Count);
end;

{==============================================================================}
{==============================================================================}
{MCP230XX Helper Functions}

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

