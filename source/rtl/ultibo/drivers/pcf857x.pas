{
NXP PCF857X I/O expander Driver.

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

  Linux - \drivers\gpio\gpio-pcf857x.c

References
==========

  PCF8574 - https://www.nxp.com/docs/en/data-sheet/PCF8574_PCF8574A.pdf

NXP PCF857X
===========

 The NXP PCF8574 is an 8 bit I/O expander that provides GPIO pin control functions over an
 I2C connection.

 The device can be represented in Ultibo as a standard GPIO device which is accessible via the
 GPIO unit functions. Because the PCF857X is a chip that can be used and configured in multiple
 different scenarios this unit does not autocreate a GPIO device, instead you need to call the
 function PCF8574GPIOCreate and pass an I2C device and address. The function will create and
 return a GPIO device with the appropriate number of pins and other information for the specified
 chip, the returned devices will have been registered with the GPIO device unit and started ready
 for use.

 Note: This unit does not currently implement the interrupt capabilities of the PCF857X chips
 however it could be expanded to allow the interrupt pin to be connected to a GPIO pin on the
 SoC and use a trigger event from that to enable GPIOInputWait/GPIOInputEvent functions for the
 PCF857X chips.

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit PCF857X;

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
 {PCF857X specific constants}
 PCF8574_GPIO_DESCRIPTION = 'NXP PCF8574 8-bit I/O Expander';  {Description of PCF8574 device}

 PCF8574_GPIO_MIN_PIN = GPIO_PIN_0;
 PCF8574_GPIO_MAX_PIN = GPIO_PIN_7;
 PCF8574_GPIO_PIN_COUNT = 8;

 PCF857X_GPIO_MAX_LEVEL = GPIO_LEVEL_HIGH;

 PCF857X_GPIO_MAX_PULL = GPIO_PULL_UP;

 PCF857X_GPIO_MIN_FUNCTION = GPIO_FUNCTION_IN;
 PCF857X_GPIO_MAX_FUNCTION = GPIO_FUNCTION_OUT;

 {PCF857X chip constants}
 PCF857X_CHIP_PCF8574   = 0;

 {PCF857X I2C constants}
 PCF8574_I2C_RATE = 100000; {Default I2C clock rate (Device supports 100KHz only}

 PCF8574_I2C_SIZE = 1;      {Number of bytes to read/write all pin values}

 PCF857X_I2C_MAX_SIZE = 2;  {Maximum number of bytes to read/write all pin values}

 {Note: The PCF857X is a single register device and does not have any direction, pull or configuration registers}

{==============================================================================}
type
 {PCF857X specific types}
 PPCF857XGPIO = ^TPCF857XGPIO;
 TPCF857XGPIO = record
  {GPIO Properties}
  GPIO:TGPIODevice;
  {PCF857X Properties}
  I2C:PI2CDevice;                                          {The I2C device this GPIO is connected to}
  Address:Word;                                            {The I2C address of the device}
  Chip:LongWord;                                           {The chip type (eg PCF857X_CHIP_PCF8574)}
  Size:LongWord;                                           {Size of an I2C read/write for the device}
  Rate:LongWord;                                           {Clock rate for this device}
  GPIOValues:array[0..PCF857X_I2C_MAX_SIZE - 1] of Byte;   {Buffer for GPIO port values (Output only)}
  GPPUValues:array[0..PCF857X_I2C_MAX_SIZE - 1] of Byte;   {Buffer for GPPU pull up values}
  IODIRValues:array[0..PCF857X_I2C_MAX_SIZE - 1] of Byte;  {Buffer for IODIR I/O direction values}
 end;

{==============================================================================}
{var}
 {PCF857X specific variables}

{==============================================================================}
{Initialization Functions}

{==============================================================================}
{PCF857X Functions}
function PCF8574GPIOCreate(I2C:PI2CDevice;Address:Word):PGPIODevice;{$IFDEF API_EXPORT_PCF857X} stdcall; public name 'pcf8574gpio_create';{$ENDIF}

function PCF857XGPIODestroy(GPIO:PGPIODevice):LongWord;{$IFDEF API_EXPORT_PCF857X} stdcall; public name 'pcf857xgpio_destroy';{$ENDIF}

{==============================================================================}
{PCF857X GPIO Functions}
function PCF857XGPIOStart(GPIO:PGPIODevice):LongWord;
function PCF857XGPIOStop(GPIO:PGPIODevice):LongWord;

function PCF857XGPIORead(GPIO:PGPIODevice;Reg:LongWord):LongWord;
procedure PCF857XGPIOWrite(GPIO:PGPIODevice;Reg,Value:LongWord);

function PCF857XGPIOInputGet(GPIO:PGPIODevice;Pin:LongWord):LongWord;

function PCF857XGPIOOutputSet(GPIO:PGPIODevice;Pin,Level:LongWord):LongWord;

function PCF857XGPIOPullGet(GPIO:PGPIODevice;Pin:LongWord):LongWord;

function PCF857XGPIOFunctionGet(GPIO:PGPIODevice;Pin:LongWord):LongWord;
function PCF857XGPIOFunctionSelect(GPIO:PGPIODevice;Pin,Mode:LongWord):LongWord;

{==============================================================================}
{PCF857X Helper Functions}

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
{var}
 {PCF857X specific variables}

{==============================================================================}
{==============================================================================}
{Initialization Functions}

{==============================================================================}
{==============================================================================}
{PCF857X Functions}
function PCF8574GPIOCreate(I2C:PI2CDevice;Address:Word):PGPIODevice;{$IFDEF API_EXPORT_PCF857X} stdcall;{$ENDIF}
{Create, register and start a new PCF8574 GPIO device connected to the specified I2C device}
{I2C: The I2C device this PCF8574 is connected to}
{Address: The I2C address for this PCF8574}
{Return: Pointer to the new GPIO device or nil on failure}
var
 Status:LongWord;

 PCF857XGPIO:PPCF857XGPIO;
begin
 {}
 Result:=nil;

 {$IF DEFINED(PCF857X_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(nil,'PCF8574: GPIO Create (Address=' + IntToHex(Address,4) + ')');
 {$ENDIF}

 {Check I2C}
 if I2C = nil then Exit;

 {Check Address}
 if Address = I2C_ADDRESS_INVALID then Exit;

 {Create GPIO}
 PCF857XGPIO:=PPCF857XGPIO(GPIODeviceCreateEx(SizeOf(TPCF857XGPIO)));
 if PCF857XGPIO <> nil then
  begin
   {Update GPIO}
   {Device}
   PCF857XGPIO.GPIO.Device.DeviceBus:=DEVICE_BUS_I2C;
   PCF857XGPIO.GPIO.Device.DeviceType:=GPIO_TYPE_NONE;
   PCF857XGPIO.GPIO.Device.DeviceFlags:=GPIO_FLAG_PULL_UP;
   PCF857XGPIO.GPIO.Device.DeviceData:=nil;
   PCF857XGPIO.GPIO.Device.DeviceDescription:=PCF8574_GPIO_DESCRIPTION;
   {GPIO}
   PCF857XGPIO.GPIO.GPIOState:=GPIO_STATE_DISABLED;
   PCF857XGPIO.GPIO.DeviceStart:=PCF857XGPIOStart;
   PCF857XGPIO.GPIO.DeviceStop:=PCF857XGPIOStop;
   PCF857XGPIO.GPIO.DeviceRead:=PCF857XGPIORead;
   PCF857XGPIO.GPIO.DeviceWrite:=PCF857XGPIOWrite;
   PCF857XGPIO.GPIO.DeviceInputGet:=PCF857XGPIOInputGet;
   PCF857XGPIO.GPIO.DeviceOutputSet:=PCF857XGPIOOutputSet;
   PCF857XGPIO.GPIO.DevicePullGet:=PCF857XGPIOPullGet;
   PCF857XGPIO.GPIO.DeviceFunctionGet:=PCF857XGPIOFunctionGet;
   PCF857XGPIO.GPIO.DeviceFunctionSelect:=PCF857XGPIOFunctionSelect;
   {Driver}
   PCF857XGPIO.GPIO.Address:=nil;
   PCF857XGPIO.GPIO.Properties.Flags:=PCF857XGPIO.GPIO.Device.DeviceFlags;
   PCF857XGPIO.GPIO.Properties.PinMin:=PCF8574_GPIO_MIN_PIN;
   PCF857XGPIO.GPIO.Properties.PinMax:=PCF8574_GPIO_MAX_PIN;
   PCF857XGPIO.GPIO.Properties.PinCount:=PCF8574_GPIO_PIN_COUNT;
   PCF857XGPIO.GPIO.Properties.FunctionMin:=PCF857X_GPIO_MIN_FUNCTION;
   PCF857XGPIO.GPIO.Properties.FunctionMax:=PCF857X_GPIO_MAX_FUNCTION;
   PCF857XGPIO.GPIO.Properties.FunctionCount:=2;
   {PCF857X}
   PCF857XGPIO.I2C:=I2C;
   PCF857XGPIO.Address:=Address;
   PCF857XGPIO.Chip:=PCF857X_CHIP_PCF8574;
   PCF857XGPIO.Size:=PCF8574_I2C_SIZE;
   PCF857XGPIO.Rate:=PCF8574_I2C_RATE;

   {Register GPIO}
   Status:=GPIODeviceRegister(@PCF857XGPIO.GPIO);
   if Status = ERROR_SUCCESS then
    begin
     {Start GPIO}
     Status:=GPIODeviceStart(@PCF857XGPIO.GPIO);
     if Status = ERROR_SUCCESS then
      begin
       {Return Result}
       Result:=PGPIODevice(PCF857XGPIO);
      end
     else
      begin
       if GPIO_LOG_ENABLED then GPIOLogError(nil,'PCF857X: Failed to start new GPIO device: ' + ErrorToString(Status));

       {Deregister GPIO}
       GPIODeviceDeregister(@PCF857XGPIO.GPIO);

       {Destroy GPIO}
       GPIODeviceDestroy(@PCF857XGPIO.GPIO);
      end;
    end
   else
    begin
     if GPIO_LOG_ENABLED then GPIOLogError(nil,'PCF857X: Failed to register new GPIO device: ' + ErrorToString(Status));

     {Destroy GPIO}
     GPIODeviceDestroy(@PCF857XGPIO.GPIO);
    end;
  end
 else
  begin
   if GPIO_LOG_ENABLED then GPIOLogError(nil,'PCF857X: Failed to create new GPIO device');
  end;
end;

{==============================================================================}

function PCF857XGPIODestroy(GPIO:PGPIODevice):LongWord;{$IFDEF API_EXPORT_PCF857X} stdcall;{$ENDIF}
{Stop, deregister and destroy a PCF857X GPIO device created by this driver}
{GPIO: The GPIO device to destroy}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check GPIO}
 if GPIO = nil then Exit;

 {$IF DEFINED(PCF857X_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'PCF857X: GPIO Destroy');
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
       if GPIO_LOG_ENABLED then GPIOLogError(nil,'PCF857X: Failed to destroy GPIO device: ' + ErrorToString(Result));
      end;
    end
   else
    begin
     if GPIO_LOG_ENABLED then GPIOLogError(nil,'PCF857X: Failed to deregister GPIO device: ' + ErrorToString(Result));
    end;
  end
 else
  begin
   if GPIO_LOG_ENABLED then GPIOLogError(nil,'PCF857X: Failed to stop GPIO device: ' + ErrorToString(Result));
  end;
end;

{==============================================================================}
{==============================================================================}
{PCF857X GPIO Functions}
function PCF857XGPIOStart(GPIO:PGPIODevice):LongWord;
{Implementation of GPIODeviceStart API for PCF857X}
{Note: Not intended to be called directly by applications, use GPIODeviceStart instead}
var
 Count:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check GPIO}
 if GPIO = nil then Exit;

 {$IF DEFINED(PCF857X_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'PCF857X: GPIO Start');
 {$ENDIF}

 {Start I2C Device}
 if I2CDeviceStart(PPCF857XGPIO(GPIO).I2C,PPCF857XGPIO(GPIO).Rate) <> ERROR_SUCCESS then
  begin
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;

 {Set IODIR Values (All pins default to Input on reset)}
 PPCF857XGPIO(GPIO).IODIRValues[0]:=$FF;
 PPCF857XGPIO(GPIO).IODIRValues[1]:=$FF;

 {$IF DEFINED(PCF857X_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'PCF857X:  IODIRValues[0]=' + IntToHex(PPCF857XGPIO(GPIO).IODIRValues[0],2));
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'PCF857X:  IODIRValues[1]=' + IntToHex(PPCF857XGPIO(GPIO).IODIRValues[1],2));
 {$ENDIF}

 {Set GPPU Values (All pins default to Pull Up on reset)}
 PPCF857XGPIO(GPIO).GPPUValues[0]:=$FF;
 PPCF857XGPIO(GPIO).GPPUValues[1]:=$FF;

 {$IF DEFINED(PCF857X_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'PCF857X:  GPPUValues[0]=' + IntToHex(PPCF857XGPIO(GPIO).GPPUValues[0],2));
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'PCF857X:  GPPUValues[1]=' + IntToHex(PPCF857XGPIO(GPIO).GPPUValues[1],2));
 {$ENDIF}

 {Clear GPIO Values (All pins default to Input on reset)}
 FillChar(PPCF857XGPIO(GPIO).GPIOValues,SizeOf(PPCF857XGPIO(GPIO).GPIOValues),0);

 {$IF DEFINED(PCF857X_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'PCF857X:  GPIOValues[0]=' + IntToHex(PPCF857XGPIO(GPIO).GPIOValues[0],2));
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'PCF857X:  GPIOValues[1]=' + IntToHex(PPCF857XGPIO(GPIO).GPIOValues[1],2));
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

function PCF857XGPIOStop(GPIO:PGPIODevice):LongWord;
{Implementation of GPIODeviceStop API for PCF857X}
{Note: Not intended to be called directly by applications, use GPIODeviceStop instead}
var
 Count:LongWord;
 Event:PGPIOEvent;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check GPIO}
 if GPIO = nil then Exit;

 {$IF DEFINED(PCF857X_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'PCF857X: GPIO Stop');
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

function PCF857XGPIORead(GPIO:PGPIODevice;Reg:LongWord):LongWord;
{Implementation of GPIODeviceRead API for PCF857X}
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

 {$IF DEFINED(PCF857X_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'PCF857X: GPIO Read (Reg=' + IntToHex(Reg,8) + ')');
 {$ENDIF}

 {Check Chip}
 case PPCF857XGPIO(GPIO).Chip of
  PCF857X_CHIP_PCF8574:begin
    {Check Reg}
    if Reg > 0 then Exit;
   end;
 end;

 {Read Register}
 if I2CDeviceRead(PPCF857XGPIO(GPIO).I2C,PPCF857XGPIO(GPIO).Address,@Value,SizeOf(Byte),Count) = ERROR_SUCCESS then
  begin
   Result:=Value;
  end;
end;

{==============================================================================}

procedure PCF857XGPIOWrite(GPIO:PGPIODevice;Reg,Value:LongWord);
{Implementation of GPIODeviceWrite API for PCF857X}
{Note: Not intended to be called directly by applications, use GPIODeviceWrite instead}

{Note: Will write only a single register byte}
var
 Count:LongWord;
begin
 {}
 {Check GPIO}
 if GPIO = nil then Exit;

 {$IF DEFINED(PCF857X_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'PCF857X: GPIO Write (Reg=' + IntToHex(Reg,8) + ' Value=' + IntToHex(Value,8) + ')');
 {$ENDIF}

 {Check Chip}
 case PPCF857XGPIO(GPIO).Chip of
  PCF857X_CHIP_PCF8574:begin
    {Check Reg}
    if Reg > 0 then Exit;
   end;
 end;

 {Write Register}
 I2CDeviceWrite(PPCF857XGPIO(GPIO).I2C,PPCF857XGPIO(GPIO).Address,@Value,SizeOf(Byte),Count);
end;

{==============================================================================}

function PCF857XGPIOInputGet(GPIO:PGPIODevice;Pin:LongWord):LongWord;
{Implementation of GPIODeviceInputGet API for PCF857X}
{Note: Not intended to be called directly by applications, use GPIODeviceInputGet instead}
var
 Reg:Byte;
 Shift:Byte;
 Count:LongWord;
 Values:array[0..PCF857X_I2C_MAX_SIZE - 1] of Byte;
begin
 {}
 Result:=GPIO_LEVEL_UNKNOWN;

 {Check GPIO}
 if GPIO = nil then Exit;

 {$IF DEFINED(PCF857X_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'PCF857X: GPIO Input Get (Pin=' + GPIOPinToString(Pin) + ')');
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
 if I2CDeviceRead(PPCF857XGPIO(GPIO).I2C,PPCF857XGPIO(GPIO).Address,@Values,PPCF857XGPIO(GPIO).Size,Count) = ERROR_SUCCESS then
  begin
   {Read Register}
   Result:=(Values[Reg] shr Shift) and 1;
  end;
end;

{==============================================================================}

function PCF857XGPIOOutputSet(GPIO:PGPIODevice;Pin,Level:LongWord):LongWord;
{Implementation of GPIODeviceOutputSet API for PCF857X}
{Note: Not intended to be called directly by applications, use GPIODeviceOutputSet instead}
var
 Reg:Byte;
 Shift:Byte;
 Count:LongWord;
 Values:array[0..PCF857X_I2C_MAX_SIZE - 1] of Byte;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check GPIO}
 if GPIO = nil then Exit;

 {$IF DEFINED(PCF857X_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'PCF857X: GPIO Output Set (Pin=' + GPIOPinToString(Pin) + ' Level=' + GPIOLevelToString(Level) + ')');
 {$ENDIF}

 {Check Pin}
 if Pin > GPIO.Properties.PinMax then Exit;

 {Check Level}
 if Level > PCF857X_GPIO_MAX_LEVEL then Exit;

 {Update Statistics}
 Inc(GPIO.SetCount);

 {Get Shift}
 Shift:=Pin mod 8;

 {Get Register}
 Reg:=Pin div 8;

 {Set Output}
 PPCF857XGPIO(GPIO).IODIRValues[Reg]:=PPCF857XGPIO(GPIO).IODIRValues[Reg] and not(1 shl Shift);
 PPCF857XGPIO(GPIO).GPPUValues[Reg]:=PPCF857XGPIO(GPIO).GPPUValues[Reg] and not(1 shl Shift);

 {Check Level}
 if Level = GPIO_LEVEL_HIGH then
  begin
   {Write Register}
   PPCF857XGPIO(GPIO).GPIOValues[Reg]:=PPCF857XGPIO(GPIO).GPIOValues[Reg] or (1 shl Shift);
  end
 else
  begin
   {Write Register}
   PPCF857XGPIO(GPIO).GPIOValues[Reg]:=PPCF857XGPIO(GPIO).GPIOValues[Reg] and not(1 shl Shift);
  end;

 {Get Values}
 Values[0]:=PPCF857XGPIO(GPIO).IODIRValues[0] or PPCF857XGPIO(GPIO).GPIOValues[0];
 Values[1]:=PPCF857XGPIO(GPIO).IODIRValues[1] or PPCF857XGPIO(GPIO).GPIOValues[1];

 {Write Values}
 Result:=I2CDeviceWrite(PPCF857XGPIO(GPIO).I2C,PPCF857XGPIO(GPIO).Address,@Values,PPCF857XGPIO(GPIO).Size,Count);
end;

{==============================================================================}

function PCF857XGPIOPullGet(GPIO:PGPIODevice;Pin:LongWord):LongWord;
{Implementation of GPIODevicePullGet API for PCF857X}
{Note: Not intended to be called directly by applications, use GPIODevicePullGet instead}
var
 Reg:Byte;
 Shift:Byte;
begin
 {}
 Result:=GPIO_PULL_UNKNOWN;

 {$IF DEFINED(PCF857X_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'PCF857X: GPIO Pull Get (Pin=' + GPIOPinToString(Pin) + ')');
 {$ENDIF}

 {Check Pin}
 if Pin > GPIO.Properties.PinMax then Exit;

 {Get Shift}
 Shift:=Pin mod 8;

 {Get Register}
 Reg:=Pin div 8;

 {Read Register}
 Result:=(PPCF857XGPIO(GPIO).GPPUValues[Reg] shr Shift) and 1;
end;

{==============================================================================}

function PCF857XGPIOFunctionGet(GPIO:PGPIODevice;Pin:LongWord):LongWord;
{Implementation of GPIODeviceFunctionGet API for PCF857X}
{Note: Not intended to be called directly by applications, use GPIODeviceFunctionGet instead}
var
 Reg:Byte;
 Shift:Byte;
begin
 {}
 Result:=GPIO_FUNCTION_UNKNOWN;

 {Check GPIO}
 if GPIO = nil then Exit;

 {$IF DEFINED(PCF857X_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'PCF857X: GPIO Function Get (Pin=' + GPIOPinToString(Pin) + ')');
 {$ENDIF}

 {Check Pin}
 if Pin > GPIO.Properties.PinMax then Exit;

 {Get Shift}
 Shift:=Pin mod 8;

 {Get Register}
 Reg:=Pin div 8;

 {Read Register}
 if ((PPCF857XGPIO(GPIO).IODIRValues[Reg] shr Shift) and 1) = 1 then
  begin
   Result:=GPIO_FUNCTION_IN;
  end
 else
  begin
   Result:=GPIO_FUNCTION_OUT;
  end;
end;

{==============================================================================}

function PCF857XGPIOFunctionSelect(GPIO:PGPIODevice;Pin,Mode:LongWord):LongWord;
{Implementation of GPIODeviceFunctionSelect API for PCF857X}
{Note: Not intended to be called directly by applications, use GPIODeviceFunctionSelect instead}
var
 Reg:Byte;
 Shift:Byte;
 Count:LongWord;
 Values:array[0..PCF857X_I2C_MAX_SIZE - 1] of Byte;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check GPIO}
 if GPIO = nil then Exit;

 {$IF DEFINED(PCF857X_DEBUG) or DEFINED(GPIO_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO,'PCF857X: GPIO Function Select (Pin=' + GPIOPinToString(Pin) + ' Mode=' + GPIOFunctionToString(Mode) + ')');
 {$ENDIF}

 {Check Pin}
 if Pin > GPIO.Properties.PinMax then Exit;

 {Check Mode}
 if Mode > PCF857X_GPIO_MAX_FUNCTION then Exit;

 {Get Shift}
 Shift:=Pin mod 8;

 {Get Register}
 Reg:=Pin div 8;

 {Check Mode}
 if Mode = GPIO_FUNCTION_IN then
  begin
   {Write Register}
   PPCF857XGPIO(GPIO).IODIRValues[Reg]:=PPCF857XGPIO(GPIO).IODIRValues[Reg] or (1 shl Shift);
   PPCF857XGPIO(GPIO).GPPUValues[Reg]:=PPCF857XGPIO(GPIO).GPPUValues[Reg] or (1 shl Shift);
  end
 else
  begin
   {Write Register}
   PPCF857XGPIO(GPIO).IODIRValues[Reg]:=PPCF857XGPIO(GPIO).IODIRValues[Reg] and not(1 shl Shift);
   PPCF857XGPIO(GPIO).GPPUValues[Reg]:=PPCF857XGPIO(GPIO).GPPUValues[Reg] and not(1 shl Shift);
  end;

 {Get Values}
 Values[0]:=PPCF857XGPIO(GPIO).IODIRValues[0] or PPCF857XGPIO(GPIO).GPIOValues[0];
 Values[1]:=PPCF857XGPIO(GPIO).IODIRValues[1] or PPCF857XGPIO(GPIO).GPIOValues[1];

 {Write Values}
 Result:=I2CDeviceWrite(PPCF857XGPIO(GPIO).I2C,PPCF857XGPIO(GPIO).Address,@Values,PPCF857XGPIO(GPIO).Size,Count);
end;

{==============================================================================}
{==============================================================================}
{PCF857X Helper Functions}

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
