{
GPIO based software I2C driver.

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

  Linux - \drivers\i2c\busses\i2c-gpio.c - Copyright (C) 2007 Atmel Corporation

  Linux - \drivers\i2c\algos\i2c-algo-bit.c - Copyright (C) 1995-2000 Simon G. Vogl

References
==========

 I2C

  https://en.wikipedia.org/wiki/I%C2%B2C

 I2C Bus Specification

  https://www.i2c-bus.org/specification/

I2C GPIO
========

 This is a software based (bit bang) I2C driver which can implement an I2C interface
 using any available pair of GPIO pins without the need for a hardware controller.

 This driver supports clock stretching and 10bit addresses and can operate with
 either the default GPIO device or any other suitable GPIO device.

 Creating multiple instances on different sets of GPIO pins is fully supported, all
 instances will appear in the standard Ultibo device tables.

 Being a software based driver the clock speed is mostly meaningless, instead a
 delay parameter can be passed during device creation which determines the interval
 between high and low states and the length of start and stop conditions. The default
 delay value equates to approximately a 100KHz clock rate.

 To use this driver in place of the default hardware I2C controller on the Raspberry
 Pi (I2C0 or BSC1) which is on GPIO pins 2 and 3 you simply initialize an instance
 of the I2CGPIO device by calling the I2CGPIOCreate function like this:

 uses
  I2C,
  GPIO,
  I2CGPIO;

 var
  I2CDevice:PI2CDevice;
  GPIODevice:PGPIODevice;

 begin
  // Get the default GPIO device
  GPIODevice:=GPIODeviceGetDefault;

  // Create an instance of the I2CGPIO device
  I2CDevice:=I2CGPIOCreate(GPIODevice,GPIO_PIN_2,GPIO_PIN_3,4,0,False,False);

  // Pass the returned I2CDevice to the standard I2C API functions
  ...
 end;

 The returned I2CDevice instance can be passed to any of the standard I2C API functions
 to perform I2C reads and writes. As noted above, any available pair of GPIO pins can
 be used to create a new I2CGPIO device.

 If an instance of the I2CGPIO device is no longer required it can be released by calling
 the I2CGPIODestroy function and passing the I2CDevice instance returned from I2CGPIOCreate.

 Handling of SDA/SCL
 -------------------

 If OpenDrain is set to True in I2CGPIOCreate then the SDA/SCL pins will be set to Output
 and toggled High or Low as required, the driver assumes that the hardware provides the
 high impedance state required for the I2C protocol.

 If OpenDrain is set to False in I2CGPIOCreate then the SDA/SCL pins will be set to Input
 when the state is High and Output with a value or Low when the state is Low. This will
 provide the high impedance (floating) state required for the I2C protocol.

 If OutputOnly is set to True in I2CGPIOCreate then reading of the SCL value is not used
 and clock stretching is not supported.

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit I2CGPIO;

interface

uses
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  Devices,
  I2C,
  GPIO,
  SysUtils;

{==============================================================================}
{Global definitions}
{$INCLUDE ..\core\GlobalDefines.inc}

{==============================================================================}
const
 {I2CGPIO specific constants}
 I2CGPIO_I2C_DESCRIPTION  = 'GPIO Software I2C'; {Description of I2CGPIO I2C device}

 I2CGPIO_I2C_MAX_SIZE = $FFFF;
 I2CGPIO_I2C_MIN_CLOCK = 10000;  {Arbitrary minimum of 10KHz, actual rate is determined by Delay parameter}
 I2CGPIO_I2C_MAX_CLOCK = 100000; {Arbitrary maximum of 100KHz, actual rate is determined by Delay parameter}

 I2CGPIO_RETRY_COUNT = 3;

 I2CGPIO_DEFAULT_TIMEOUT = 100;

{==============================================================================}
type
 {I2CGPIO specific types}
 PI2CGPIODevice = ^TI2CGPIODevice;
 TI2CGPIODevice = record
  {I2C Properties}
  I2C:TI2CDevice;
  {I2CGPIO Properties}
  GPIO:PGPIODevice;             {The GPIO device this device is connected to}
  SDA:LongWord;                 {GPIO pin for the SDA line}
  SCL:LongWord;                 {GPIO pin for the SCL line}
  Delay:LongWord;               {Clock and Data delay in microseconds}
  Timeout:LongWord;             {Clock timeout in milliseconds}
  OutputOnly:LongBool;          {Clock line is output only, no test for SCL high}
  OpenDrain:LongBool;           {Clock and Data are open drain, no need to simulate by switching direction}
  {Transfer Properties}
  IgnoreNAK:LongBool;           {If True Ignore NAK responses and continue}
 end;

{==============================================================================}
{var}
 {I2CGPIO specific variables}

{==============================================================================}
{Initialization Functions}

{==============================================================================}
{I2CGPIO Functions}
function I2CGPIOCreate(GPIO:PGPIODevice;SDA,SCL,Delay,Timeout:LongWord;OutputOnly,OpenDrain:Boolean):PI2CDevice;{$IFDEF API_EXPORT_I2CGPIO} stdcall; public name 'i2cgpio_create';{$ENDIF}
function I2CGPIODestroy(I2C:PI2CDevice):LongWord;{$IFDEF API_EXPORT_I2CGPIO} stdcall; public name 'i2cgpio_destroy';{$ENDIF}

{==============================================================================}
{I2CGPIO I2C Functions}
function I2CGPIOStart(I2C:PI2CDevice;Rate:LongWord):LongWord;
function I2CGPIOStop(I2C:PI2CDevice):LongWord;

function I2CGPIORead(I2C:PI2CDevice;Address:Word;Buffer:Pointer;Size,Flags:LongWord;var Count:LongWord):LongWord;
function I2CGPIOWrite(I2C:PI2CDevice;Address:Word;Buffer:Pointer;Size,Flags:LongWord;var Count:LongWord):LongWord;
function I2CGPIOWriteRead(I2C:PI2CDevice;Address:Word;Initial:Pointer;Len:LongWord;Data:Pointer;Size,Flags:LongWord;var Count:LongWord):LongWord;
function I2CGPIOWriteWrite(I2C:PI2CDevice;Address:Word;Initial:Pointer;Len:LongWord;Data:Pointer;Size,Flags:LongWord;var Count:LongWord):LongWord;

{==============================================================================}
{I2CGPIO Helper Functions}

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
{var}
 {I2CGPIO specific variables}

{==============================================================================}
{==============================================================================}
{Forward Declarations}
function I2CGPIOGetSDAValue(I2C:PI2CGPIODevice):LongWord; forward;
function I2CGPIOGetSCLValue(I2C:PI2CGPIODevice):LongWord; forward;

procedure I2CGPIOSetSDAValue(I2C:PI2CGPIODevice;Value:LongWord); forward;
procedure I2CGPIOSetSCLValue(I2C:PI2CGPIODevice;Value:LongWord); forward;

procedure I2CGPIOSDALow(I2C:PI2CGPIODevice); forward;
procedure I2CGPIOSDAHigh(I2C:PI2CGPIODevice); forward;
procedure I2CGPIOSCLLow(I2C:PI2CGPIODevice); forward;
function I2CGPIOSCLHigh(I2C:PI2CGPIODevice):LongWord; forward;

procedure I2CGPIOI2CStart(I2C:PI2CGPIODevice); forward;
procedure I2CGPIOI2CRepstart(I2C:PI2CGPIODevice); forward;
procedure I2CGPIOI2CStop(I2C:PI2CGPIODevice); forward;

function I2CGPIOOutputByte(I2C:PI2CGPIODevice;Data:Byte):LongWord; forward;
function I2CGPIOInputByte(I2C:PI2CGPIODevice;var Data:Byte):LongWord; forward;

function I2CGPIOTryAddress(I2C:PI2CGPIODevice;Address:Byte;Retries:LongWord):LongWord; forward;
function I2CGPIOWriteAddress(I2C:PI2CGPIODevice;Address:Word;Read:Boolean):LongWord; forward;

function I2CGPIOSendBytes(I2C:PI2CGPIODevice;Data:Pointer;Size:LongWord;var Count:LongWord):LongWord; forward;
function I2CGPIOAcknowledge(I2C:PI2CGPIODevice;Acknowledge:Boolean):LongWord; forward;
function I2CGPIOReceiveBytes(I2C:PI2CGPIODevice;Data:Pointer;Size:LongWord;var Count:LongWord):LongWord; forward;

{==============================================================================}
{==============================================================================}
{Initialization Functions}

{==============================================================================}
{==============================================================================}
{I2CGPIO Functions}
function I2CGPIOCreate(GPIO:PGPIODevice;SDA,SCL,Delay,Timeout:LongWord;OutputOnly,OpenDrain:Boolean):PI2CDevice;{$IFDEF API_EXPORT_I2CGPIO} stdcall;{$ENDIF}
{Create and register a new I2CGPIO I2C device connected to the specified GPIO device}
var
 Status:LongWord;

 I2CGPIODevice:PI2CGPIODevice;
begin
 {}
 Result:=nil;

 {$IF DEFINED(I2CGPIO_DEBUG) or DEFINED(I2C_DEBUG)}
 if I2C_LOG_ENABLED then I2CLogDebug(nil,'I2CGPIO: I2C Create (SDA=' + GPIOPinToString(SDA) + ' SCL=' + GPIOPinToString(SCL) + ' Delay=' + IntToStr(Delay) + ' Timeout=' + IntToStr(Timeout) + ')');
 {$ENDIF}

 {Check GPIO}
 if GPIO = nil then Exit;

 {Create I2C}
 I2CGPIODevice:=PI2CGPIODevice(I2CDeviceCreateEx(SizeOf(TI2CGPIODevice)));
 if I2CGPIODevice <> nil then
  begin
   {Update I2C}
   {Device}
   I2CGPIODevice.I2C.Device.DeviceBus:=DEVICE_BUS_MMIO;
   I2CGPIODevice.I2C.Device.DeviceType:=I2C_TYPE_MASTER;
   I2CGPIODevice.I2C.Device.DeviceFlags:=I2CGPIODevice.I2C.Device.DeviceFlags or I2C_FLAG_10BIT; {Don't override defaults}
   I2CGPIODevice.I2C.Device.DeviceData:=nil;
   I2CGPIODevice.I2C.Device.DeviceDescription:=I2CGPIO_I2C_DESCRIPTION;
   {I2C}
   I2CGPIODevice.I2C.I2CState:=I2C_STATE_DISABLED;
   I2CGPIODevice.I2C.DeviceStart:=I2CGPIOStart;
   I2CGPIODevice.I2C.DeviceStop:=I2CGPIOStop;
   I2CGPIODevice.I2C.DeviceRead:=I2CGPIORead;
   I2CGPIODevice.I2C.DeviceWrite:=I2CGPIOWrite;
   I2CGPIODevice.I2C.DeviceWriteRead:=I2CGPIOWriteRead;
   I2CGPIODevice.I2C.DeviceWriteWrite:=I2CGPIOWriteWrite;
   {Driver}
   I2CGPIODevice.I2C.Properties.Flags:=I2CGPIODevice.I2C.Device.DeviceFlags;
   I2CGPIODevice.I2C.Properties.MaxSize:=I2CGPIO_I2C_MAX_SIZE;
   I2CGPIODevice.I2C.Properties.MinClock:=I2CGPIO_I2C_MIN_CLOCK;
   I2CGPIODevice.I2C.Properties.MaxClock:=I2CGPIO_I2C_MAX_CLOCK;
   I2CGPIODevice.I2C.Properties.ClockRate:=0;
   I2CGPIODevice.I2C.Properties.SlaveAddress:=I2C_ADDRESS_INVALID;
   {I2CGPIO}
   I2CGPIODevice.GPIO:=GPIO;
   I2CGPIODevice.SDA:=SDA;
   I2CGPIODevice.SCL:=SCL;
   I2CGPIODevice.Delay:=Delay;
   I2CGPIODevice.Timeout:=Timeout;
   I2CGPIODevice.OutputOnly:=OutputOnly;
   I2CGPIODevice.OpenDrain:=OpenDrain;

   {Update Delay}
   if Delay = 0 then
    begin
     if OutputOnly then
      begin
       I2CGPIODevice.Delay:=50; {10 kHz}
      end
     else
      begin
       I2CGPIODevice.Delay:=5; {100 kHz}
      end;
    end;

   {Update Timeout}
   if Timeout = 0 then
    begin
     I2CGPIODevice.Timeout:=I2CGPIO_DEFAULT_TIMEOUT; {100 ms}
    end;

   {Register I2C0}
   Status:=I2CDeviceRegister(@I2CGPIODevice.I2C);
   if Status = ERROR_SUCCESS then
    begin
     {Return Result}
     Result:=PI2CDevice(I2CGPIODevice);
    end
   else
    begin
     if I2C_LOG_ENABLED then I2CLogError(nil,'I2CGPIO: Failed to register new I2C device: ' + ErrorToString(Status));

     {Destroy I2C0}
     I2CDeviceDestroy(@I2CGPIODevice.I2C);
    end;
  end
 else
  begin
   if I2C_LOG_ENABLED then I2CLogError(nil,'I2CGPIO: Failed to create new I2C device');
  end;
end;

{==============================================================================}

function I2CGPIODestroy(I2C:PI2CDevice):LongWord;{$IFDEF API_EXPORT_I2CGPIO} stdcall;{$ENDIF}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check I2C}
 if I2C = nil then Exit;

 {$IF DEFINED(I2CGPIO_DEBUG) or DEFINED(I2C_DEBUG)}
 if I2C_LOG_ENABLED then I2CLogDebug(I2C,'I2CGPIO: I2C Destroy');
 {$ENDIF}

 {Stop I2C}
 Result:=I2CDeviceStop(I2C);
 if Result = ERROR_SUCCESS then
  begin
   {Deregister I2C}
   Result:=I2CDeviceDeregister(I2C);
   if Result = ERROR_SUCCESS then
    begin
     {Destroy I2C}
     Result:=I2CDeviceDestroy(I2C);
     if Result <> ERROR_SUCCESS then
      begin
       if I2C_LOG_ENABLED then I2CLogError(nil,'I2CGPIO: Failed to destroy I2C device: ' + ErrorToString(Result));
      end;
    end
   else
    begin
     if I2C_LOG_ENABLED then I2CLogError(nil,'I2CGPIO: Failed to deregister I2C device: ' + ErrorToString(Result));
    end;
  end
 else
  begin
   if I2C_LOG_ENABLED then I2CLogError(nil,'I2CGPIO: Failed to stop I2C device: ' + ErrorToString(Result));
  end;
end;

{==============================================================================}
{==============================================================================}
{I2CGPIO I2C Functions}
function I2CGPIOStart(I2C:PI2CDevice;Rate:LongWord):LongWord;
{Implementation of I2CDeviceStart API for I2CGPIO device}
{Note: Not intended to be called directly by applications, use I2CDeviceStart instead}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check I2C}
 if I2C = nil then Exit;

 {$IF DEFINED(I2CGPIO_DEBUG) or DEFINED(I2C_DEBUG)}
 if I2C_LOG_ENABLED then I2CLogDebug(I2C,'I2CGPIO: I2C Start (Rate=' + IntToStr(Rate) + ')');
 {$ENDIF}

 {Check Rate}
 if (Rate <> 0) and ((Rate < I2C.Properties.MinClock) or (Rate > I2C.Properties.MaxClock)) then Exit;

 {Get Rate}
 if Rate = 0 then Rate:=I2CGPIO_I2C_MAX_CLOCK;

 {Set SDA/SCL}
 GPIODeviceFunctionSelect(PI2CGPIODevice(I2C).GPIO,PI2CGPIODevice(I2C).SDA,GPIO_FUNCTION_IN);
 GPIODeviceFunctionSelect(PI2CGPIODevice(I2C).GPIO,PI2CGPIODevice(I2C).SCL,GPIO_FUNCTION_IN);

 {Update Properties}
 I2C.ClockRate:=Rate;
 I2C.Properties.ClockRate:=Rate;

 {Return Result}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function I2CGPIOStop(I2C:PI2CDevice):LongWord;
{Implementation of I2CDeviceStop API for I2CGPIO device}
{Note: Not intended to be called directly by applications, use I2CDeviceStop instead}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check I2C}
 if I2C = nil then Exit;

 {$IF DEFINED(I2CGPIO_DEBUG) or DEFINED(I2C_DEBUG)}
 if I2C_LOG_ENABLED then I2CLogDebug(I2C,'I2CGPIO: I2C Stop');
 {$ENDIF}

 {Reset Transfer}
 PI2CGPIODevice(I2C).IgnoreNAK:=False;

 {Return Result}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function I2CGPIORead(I2C:PI2CDevice;Address:Word;Buffer:Pointer;Size,Flags:LongWord;var Count:LongWord):LongWord;
{Implementation of I2CDeviceRead API for I2CGPIO device}
{Note: Not intended to be called directly by applications, use I2CDeviceRead instead}
var
 Status:LongWord;
 I2CDevice:PI2CGPIODevice;
begin
 {}
 {Setup Result}
 Count:=0;
 Result:=ERROR_INVALID_PARAMETER;

 {Check Buffer}
 if Buffer = nil then Exit;

 {Check I2C}
 if I2C = nil then Exit;

 {$IF DEFINED(I2CGPIO_DEBUG) or DEFINED(I2C_DEBUG)}
 if I2C_LOG_ENABLED then I2CLogDebug(I2C,'I2CGPIO: I2C Read (Address=' + IntToHex(Address,4) + ' Size=' + IntToStr(Size) + ' Flags=' + IntToHex(Flags,8) + ')');
 {$ENDIF}

 {Check Size}
 if Size > I2CGPIO_I2C_MAX_SIZE then Exit;

 {Update Statistics}
 Inc(I2C.ReadCount);

 {Read to Buffer}
 if Size > 0 then
  begin
   {Setup Address}
   if (Address <> I2C_ADDRESS_INVALID) and (Address <> I2C.SlaveAddress) then
    begin
     {Update Properties}
     I2C.SlaveAddress:=Address;
     I2C.Properties.SlaveAddress:=Address;
    end;

   {Get I2CGPIO Device}
   I2CDevice:=PI2CGPIODevice(I2C);

   {Setup Data}
   I2CDevice.IgnoreNAK:=(Flags and I2C_TRANSFER_IGNORE_NAK) <> 0;

   {Send START}
   I2CGPIOI2CStart(I2CDevice);
   try
    {Write Address}
    Status:=I2CGPIOWriteAddress(I2CDevice,I2C.SlaveAddress,True);
    if Status <> ERROR_SUCCESS then
     begin
      if I2C_LOG_ENABLED then I2CLogError(I2C,'I2CGPIO: Write address failure');

      {Update Statistics}
      Inc(I2C.ReadErrors);

      Result:=Status;
      Exit;
     end;

    {Receive Bytes}
    Status:=I2CGPIOReceiveBytes(I2CDevice,Buffer,Size,Count);
    if Status <> ERROR_SUCCESS then
     begin
      if I2C_LOG_ENABLED then I2CLogError(I2C,'I2CGPIO: Read failure or timeout');

      {Update Statistics}
      Inc(I2C.ReadErrors);

      Result:=Status;
      Exit;
     end;
   finally
    {Send STOP}
    I2CGPIOI2CStop(I2CDevice);
   end;

   {Reset Data}
   I2CDevice.IgnoreNAK:=False;
  end;

 {$IF DEFINED(I2CGPIO_DEBUG) or DEFINED(I2C_DEBUG)}
 if I2C_LOG_ENABLED then I2CLogDebug(I2C,'I2CGPIO:  Return Count=' + IntToStr(Count));
 {$ENDIF}

 {Return Result}
 if (Size = Count) then Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function I2CGPIOWrite(I2C:PI2CDevice;Address:Word;Buffer:Pointer;Size,Flags:LongWord;var Count:LongWord):LongWord;
{Implementation of I2CDeviceWrite API for I2CGPIO device}
{Note: Not intended to be called directly by applications, use I2CDeviceWrite instead}
var
 Status:LongWord;
 I2CDevice:PI2CGPIODevice;
begin
 {}
 {Setup Result}
 Count:=0;
 Result:=ERROR_INVALID_PARAMETER;

 {Check Buffer}
 if Buffer = nil then Exit;

 {Check I2C}
 if I2C = nil then Exit;

 {$IF DEFINED(I2CGPIO_DEBUG) or DEFINED(I2C_DEBUG)}
 if I2C_LOG_ENABLED then I2CLogDebug(I2C,'I2CGPIO: I2C Write (Address=' + IntToHex(Address,4) + ' Size=' + IntToStr(Size) + ' Flags=' + IntToHex(Flags,8) + ')');
 {$ENDIF}

 {Check Size}
 if Size > I2CGPIO_I2C_MAX_SIZE then Exit;

 {Update Statistics}
 Inc(I2C.WriteCount);

 {Write from Buffer}
 if Size > 0 then
  begin
   {Setup Address}
   if (Address <> I2C_ADDRESS_INVALID) and (Address <> I2C.SlaveAddress) then
    begin
     {Update Properties}
     I2C.SlaveAddress:=Address;
     I2C.Properties.SlaveAddress:=Address;
    end;

   {Get I2CGPIO Device}
   I2CDevice:=PI2CGPIODevice(I2C);

   {Setup Data}
   I2CDevice.IgnoreNAK:=(Flags and I2C_TRANSFER_IGNORE_NAK) <> 0;

   {Send START}
   I2CGPIOI2CStart(I2CDevice);
   try
    {Write Address}
    Status:=I2CGPIOWriteAddress(I2CDevice,I2C.SlaveAddress,False);
    if Status <> ERROR_SUCCESS then
     begin
      if I2C_LOG_ENABLED then I2CLogError(I2C,'I2CGPIO: Write address failure');

      {Update Statistics}
      Inc(I2C.WriteErrors);

      Result:=Status;
      Exit;
     end;

    {Send Bytes}
    Status:=I2CGPIOSendBytes(I2CDevice,Buffer,Size,Count);
    if Status <> ERROR_SUCCESS then
     begin
      if I2C_LOG_ENABLED then I2CLogError(I2C,'I2CGPIO: Write failure or timeout');

      {Update Statistics}
      Inc(I2C.WriteErrors);

      Result:=Status;
      Exit;
     end;
   finally
    {Send STOP}
    I2CGPIOI2CStop(I2CDevice);
   end;

   {Reset Data}
   I2CDevice.IgnoreNAK:=False;
  end;

 {$IF DEFINED(I2CGPIO_DEBUG) or DEFINED(I2C_DEBUG)}
 if I2C_LOG_ENABLED then I2CLogDebug(I2C,'I2CGPIO:  Return Count=' + IntToStr(Count));
 {$ENDIF}

 {Return Result}
 if (Size = Count) then Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function I2CGPIOWriteRead(I2C:PI2CDevice;Address:Word;Initial:Pointer;Len:LongWord;Data:Pointer;Size,Flags:LongWord;var Count:LongWord):LongWord;
{Implementation of I2CDeviceWriteRead API for I2CGPIO device}
{Note: Not intended to be called directly by applications, use I2CDeviceWriteRead instead}
var
 Status:LongWord;
 I2CDevice:PI2CGPIODevice;
begin
 {}
 {Setup Result}
 Count:=0;
 Result:=ERROR_INVALID_PARAMETER;

 {Check Buffers}
 if Initial = nil then Exit;
 if Data = nil then Exit;

 {Check I2C}
 if I2C = nil then Exit;

 {$IF DEFINED(I2CGPIO_DEBUG) or DEFINED(I2C_DEBUG)}
 if I2C_LOG_ENABLED then I2CLogDebug(I2C,'I2CGPIO: I2C Write Read (Address=' + IntToHex(Address,4) + ' Len=' + IntToStr(Len) + ' Size=' + IntToStr(Size) + ' Flags=' + IntToHex(Flags,8) + ')');
 {$ENDIF}

 {Check Sizes}
 if Len > I2CGPIO_I2C_MAX_SIZE then Exit;
 if Size > I2CGPIO_I2C_MAX_SIZE then Exit;

 {Update Statistics}
 Inc(I2C.WriteCount);
 Inc(I2C.ReadCount);

 {Write from Initial}
 if Len > 0 then
  begin
   {Setup Address}
   if (Address <> I2C_ADDRESS_INVALID) and (Address <> I2C.SlaveAddress) then
    begin
     {Update Properties}
     I2C.SlaveAddress:=Address;
     I2C.Properties.SlaveAddress:=Address;
    end;

   {Get I2CGPIO Device}
   I2CDevice:=PI2CGPIODevice(I2C);

   {Setup Data}
   I2CDevice.IgnoreNAK:=(Flags and I2C_TRANSFER_IGNORE_NAK) <> 0;

   {Send START}
   I2CGPIOI2CStart(I2CDevice);
   try
    {Write Address}
    Status:=I2CGPIOWriteAddress(I2CDevice,I2C.SlaveAddress,False);
    if Status <> ERROR_SUCCESS then
     begin
      if I2C_LOG_ENABLED then I2CLogError(I2C,'I2CGPIO: Write address failure');

      {Update Statistics}
      Inc(I2C.WriteErrors);

      Result:=Status;
      Exit;
     end;

    {Send Bytes}
    Status:=I2CGPIOSendBytes(I2CDevice,Initial,Len,Count);
    if Status <> ERROR_SUCCESS then
     begin
      if I2C_LOG_ENABLED then I2CLogError(I2C,'I2CGPIO: Write failure or timeout (Initial)');

      {Update Statistics}
      Inc(I2C.WriteErrors);

      Result:=Status;
      Exit;
     end;

    {Read to Data}
    if Size > 0 then
     begin
      {Send REPSTART}
      I2CGPIOI2CRepstart(I2CDevice);

      {Write Address}
      Status:=I2CGPIOWriteAddress(I2CDevice,I2C.SlaveAddress,True);
      if Status <> ERROR_SUCCESS then
       begin
        if I2C_LOG_ENABLED then I2CLogError(I2C,'I2CGPIO: Write address failure');

        {Update Statistics}
        Inc(I2C.ReadErrors);

        Result:=Status;
        Exit;
       end;

      {Receive Bytes}
      Status:=I2CGPIOReceiveBytes(I2CDevice,Data,Size,Count);
      if Status <> ERROR_SUCCESS then
       begin
        if I2C_LOG_ENABLED then I2CLogError(I2C,'I2CGPIO: Read failure or timeout (Data)');

        {Update Statistics}
        Inc(I2C.ReadErrors);

        Result:=Status;
        Exit;
       end;
     end;
   finally
    {Send STOP}
    I2CGPIOI2CStop(I2CDevice);
   end;

   {Reset Data}
   I2CDevice.IgnoreNAK:=False;
  end;

 {$IF DEFINED(I2CGPIO_DEBUG) or DEFINED(I2C_DEBUG)}
 if I2C_LOG_ENABLED then I2CLogDebug(I2C,'I2CGPIO:  Return Count=' + IntToStr(Count));
 {$ENDIF}

 {Return Result}
 if (Size = Count) then Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function I2CGPIOWriteWrite(I2C:PI2CDevice;Address:Word;Initial:Pointer;Len:LongWord;Data:Pointer;Size,Flags:LongWord;var Count:LongWord):LongWord;
{Implementation of I2CDeviceWriteWrite API for I2CGPIO device}
{Note: Not intended to be called directly by applications, use I2CDeviceWriteWrite instead}
var
 Status:LongWord;
 I2CDevice:PI2CGPIODevice;
begin
 {}
 {Setup Result}
 Count:=0;
 Result:=ERROR_INVALID_PARAMETER;

 {Check Buffers}
 if Initial = nil then Exit;
 if Data = nil then Exit;

 {Check I2C}
 if I2C = nil then Exit;

 {$IF DEFINED(I2CGPIO_DEBUG) or DEFINED(I2C_DEBUG)}
 if I2C_LOG_ENABLED then I2CLogDebug(I2C,'I2CGPIO: I2C Write Write (Address=' + IntToHex(Address,4) + ' Len=' + IntToStr(Len) + ' Size=' + IntToStr(Size) + ' Flags=' + IntToHex(Flags,8) + ')');
 {$ENDIF}

 {Check Sizes}
 if Len > I2CGPIO_I2C_MAX_SIZE then Exit;
 if Size > I2CGPIO_I2C_MAX_SIZE then Exit;

 {Update Statistics}
 Inc(I2C.WriteCount);
 Inc(I2C.WriteCount);

 {Write from Initial and Data}
 if (Len > 0) and (Size > 0) then
  begin
   {Setup Address}
   if (Address <> I2C_ADDRESS_INVALID) and (Address <> I2C.SlaveAddress) then
    begin
     {Update Properties}
     I2C.SlaveAddress:=Address;
     I2C.Properties.SlaveAddress:=Address;
    end;

   {Get I2CGPIO Device}
   I2CDevice:=PI2CGPIODevice(I2C);

   {Setup Data}
   I2CDevice.IgnoreNAK:=(Flags and I2C_TRANSFER_IGNORE_NAK) <> 0;

   {Send START}
   I2CGPIOI2CStart(I2CDevice);
   try
    {Write Address}
    Status:=I2CGPIOWriteAddress(I2CDevice,I2C.SlaveAddress,False);
    if Status <> ERROR_SUCCESS then
     begin
      if I2C_LOG_ENABLED then I2CLogError(I2C,'I2CGPIO: Write address failure');

      {Update Statistics}
      Inc(I2C.WriteErrors);

      Result:=Status;
      Exit;
     end;

    {Send Bytes}
    Status:=I2CGPIOSendBytes(I2CDevice,Initial,Len,Count);
    if Status <> ERROR_SUCCESS then
     begin
      if I2C_LOG_ENABLED then I2CLogError(I2C,'I2CGPIO: Write failure or timeout (Initial)');

      {Update Statistics}
      Inc(I2C.WriteErrors);

      Result:=Status;
      Exit;
     end;

    {Send Bytes}
    Status:=I2CGPIOSendBytes(I2CDevice,Data,Size,Count);
    if Status <> ERROR_SUCCESS then
     begin
      if I2C_LOG_ENABLED then I2CLogError(I2C,'I2CGPIO: Write failure or timeout (Data)');

      {Update Statistics}
      Inc(I2C.WriteErrors);

      Result:=Status;
      Exit;
     end;
   finally
    {Send STOP}
    I2CGPIOI2CStop(I2CDevice);
   end;

   {Reset Data}
   I2CDevice.IgnoreNAK:=False;
  end;

 {$IF DEFINED(I2CGPIO_DEBUG) or DEFINED(I2C_DEBUG)}
 if I2C_LOG_ENABLED then I2CLogDebug(I2C,'I2CGPIO:  Return Count=' + IntToStr(Count));
 {$ENDIF}

 {Return Result}
 if (Size = Count) then Result:=ERROR_SUCCESS;
end;

{==============================================================================}
{==============================================================================}
{I2CGPIO Helper Functions}

{==============================================================================}
{==============================================================================}
{I2CGPIO Internal Functions}
function I2CGPIOGetSDAValue(I2C:PI2CGPIODevice):LongWord;
{Get the I2C SDA pin value from the GPIO device}
begin
 {}
 Result:=GPIO_LEVEL_UNKNOWN;

 {Check I2C}
 if I2C = nil then Exit;

 {Get GPIO Value}
 Result:=GPIODeviceInputGet(I2C.GPIO,I2C.SDA);

 //i2c_gpio_getsda
end;

{==============================================================================}

function I2CGPIOGetSCLValue(I2C:PI2CGPIODevice):LongWord;
{Get the I2C SCL pin value from the GPIO device}
begin
 {}
 Result:=GPIO_LEVEL_UNKNOWN;

 {Check I2C}
 if I2C = nil then Exit;

 {Get GPIO Value}
 Result:=GPIODeviceInputGet(I2C.GPIO,I2C.SCL);

 //i2c_gpio_getscl
end;

{==============================================================================}

procedure I2CGPIOSetSDAValue(I2C:PI2CGPIODevice;Value:LongWord);
{Set the I2C SDA pin value on the GPIO device}
{If OpenDrain is False a High will be signalled by switching the pin to input mode}
begin
 {}
 {Check I2C}
 if I2C = nil then Exit;

 {Check Open Drain}
 if I2C.OpenDrain then
  begin
   {Set Output}
   GPIODeviceOutputSet(I2C.GPIO,I2C.SDA,Value);
  end
 else
  begin
   if Value <> GPIO_LEVEL_LOW then
    begin
     {Set to Input}
     GPIODeviceFunctionSelect(I2C.GPIO,I2C.SDA,GPIO_FUNCTION_IN);
    end
   else
    begin
     {Set to Output}
     GPIODeviceFunctionSelect(I2C.GPIO,I2C.SDA,GPIO_FUNCTION_OUT);

     {Set Output to Low}
     GPIODeviceOutputSet(I2C.GPIO,I2C.SDA,Value);
    end;
  end;

 //i2c_gpio_setsda_val
end;

{==============================================================================}

procedure I2CGPIOSetSCLValue(I2C:PI2CGPIODevice;Value:LongWord);
{Set the I2C SCL pin value on the GPIO device}
{If OpenDrain is False a High will be signalled by switching the pin to input mode}
begin
 {}
 {Check I2C}
 if I2C = nil then Exit;

 {Check Open Drain}
 if I2C.OpenDrain then
  begin
   {Set Output}
   GPIODeviceOutputSet(I2C.GPIO,I2C.SCL,Value);
  end
 else
  begin
   if Value <> GPIO_LEVEL_LOW then
    begin
     {Set to Input}
     GPIODeviceFunctionSelect(I2C.GPIO,I2C.SCL,GPIO_FUNCTION_IN);
    end
   else
    begin
     {Set to Output}
     GPIODeviceFunctionSelect(I2C.GPIO,I2C.SCL,GPIO_FUNCTION_OUT);

     {Set Output to Low}
     GPIODeviceOutputSet(I2C.GPIO,I2C.SCL,Value);
    end;
  end;

 //i2c_gpio_setscl_val
end;

{==============================================================================}

procedure I2CGPIOSDALow(I2C:PI2CGPIODevice);
{Set the I2C SDA pin Low and wait for the appropriate delay}
begin
 {}
 {Check I2C}
 if I2C = nil then Exit;

 {Set SDA Low}
 I2CGPIOSetSDAValue(I2C,GPIO_LEVEL_LOW);

 {Wait Delay}
 MicrosecondDelay((I2C.Delay + 1) div 2);

 //sdalo
end;

{==============================================================================}

procedure I2CGPIOSDAHigh(I2C:PI2CGPIODevice);
{Set the I2C SDA pin High and wait for the appropriate delay}
begin
 {}
 {Check I2C}
 if I2C = nil then Exit;

 {Set SDA High}
 I2CGPIOSetSDAValue(I2C,GPIO_LEVEL_HIGH);

 {Wait Delay}
 MicrosecondDelay((I2C.Delay + 1) div 2);

 //sdahi
end;

{==============================================================================}

procedure I2CGPIOSCLLow(I2C:PI2CGPIODevice);
{Set the I2C SCL pin Low and wait for the appropriate delay}
begin
 {}
 {Check I2C}
 if I2C = nil then Exit;

 {Set SCL Low}
 I2CGPIOSetSCLValue(I2C,GPIO_LEVEL_LOW);

 {Wait Delay}
 MicrosecondDelay(I2C.Delay div 2);

 //scllo
end;

{==============================================================================}

function I2CGPIOSCLHigh(I2C:PI2CGPIODevice):LongWord;
{Set the I2C SCL pin High, wait for the pin to go High and wait the appropriate delay}
var
 Timeout:Int64;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check I2C}
 if I2C = nil then Exit;

 {Set SCL High}
 I2CGPIOSetSCLValue(I2C,GPIO_LEVEL_HIGH);

 {Check Output Only}
 if not I2C.OutputOnly then
  begin
   Timeout:=ClockGetTotal + (I2C.Timeout * CLOCK_CYCLES_PER_MILLISECOND);

   {Wait for the clock line to go high to allow for clock stretching}
   while I2CGPIOGetSCLValue(I2C) = GPIO_LEVEL_LOW do
    begin
     if ClockGetTotal > Timeout then
      begin
       if I2CGPIOGetSCLValue(I2C) = GPIO_LEVEL_HIGH then Break;

       Result:=ERROR_TIMEOUT;
       Exit;
      end;

     {Memory Barrier}
     DataMemoryBarrier;
    end;
  end;

 {Wait Delay}
 MicrosecondDelay(I2C.Delay);

 Result:=ERROR_SUCCESS;

 //sclhi
end;

{==============================================================================}

procedure I2CGPIOI2CStart(I2C:PI2CGPIODevice);
{Send an I2C START condition}
begin
 {}
 {Check I2C}
 if I2C = nil then Exit;

 {$IF DEFINED(I2CGPIO_DEBUG) or DEFINED(I2C_DEBUG)}
 if I2C_LOG_ENABLED then I2CLogDebug(@I2C.I2C,'I2CGPIO: Sending START condition');
 {$ENDIF}

 {Starting state SCL and SDA are High}
 I2CGPIOSetSDAValue(I2C,GPIO_LEVEL_LOW);

 MicrosecondDelay(I2C.Delay);

 I2CGPIOSCLLow(I2C);

 //i2c_start
end;

{==============================================================================}

procedure I2CGPIOI2CRepstart(I2C:PI2CGPIODevice);
{Send an I2C REPSTART condition}
begin
 {}
 {Check I2C}
 if I2C = nil then Exit;

 {$IF DEFINED(I2CGPIO_DEBUG) or DEFINED(I2C_DEBUG)}
 if I2C_LOG_ENABLED then I2CLogDebug(@I2C.I2C,'I2CGPIO: Sending REPSTART condition');
 {$ENDIF}

 {Starting state SCL is Low}
 I2CGPIOSDAHigh(I2C);

 I2CGPIOSCLHigh(I2C);

 I2CGPIOSetSDAValue(I2C,GPIO_LEVEL_LOW);

 MicrosecondDelay(I2C.Delay);

 I2CGPIOSCLLow(I2C);

 //i2c_repstart
end;

{==============================================================================}

procedure I2CGPIOI2CStop(I2C:PI2CGPIODevice);
{Send an I2C STOP condition}
begin
 {}
 {Check I2C}
 if I2C = nil then Exit;

 {$IF DEFINED(I2CGPIO_DEBUG) or DEFINED(I2C_DEBUG)}
 if I2C_LOG_ENABLED then I2CLogDebug(@I2C.I2C,'I2CGPIO: Sending STOP condition');
 {$ENDIF}

 {Starting state SCL is Low}
 I2CGPIOSDALow(I2C);

 I2CGPIOSCLHigh(I2C);

 I2CGPIOSetSDAValue(I2C,GPIO_LEVEL_HIGH);

 MicrosecondDelay(I2C.Delay);

 //i2c_stop
end;

{==============================================================================}

function I2CGPIOOutputByte(I2C:PI2CGPIODevice;Data:Byte):LongWord;
{Send a byte over I2C without START or STOP condition, check ACK or NAK from slave}
var
 Count:Integer;
 Level:LongWord;
 Response:String;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check I2C}
 if I2C = nil then Exit;

 {Starting state SCL is Low}

 {Output Bits}
 for Count:=7 downto 0 do
  begin
   {Get Bit}
   Level:=(Data shr Count) and 1;

   {Set SDA}
   I2CGPIOSetSDAValue(I2C,Level);

   {Wait Delay}
   MicrosecondDelay((I2C.Delay + 1) div 2);

   {Set SCL}
   if I2CGPIOSCLHigh(I2C) = ERROR_TIMEOUT then
    begin
     {$IF DEFINED(I2CGPIO_DEBUG) or DEFINED(I2C_DEBUG)}
     if I2C_LOG_ENABLED then I2CLogDebug(@I2C.I2C,'I2CGPIO: Output byte: ' + IntToHex(Data,8) + ' timeout at bit: ' + IntToStr(Count));
     {$ENDIF}

     Result:=ERROR_TIMEOUT;
     Exit;
    end;

   {Set SCL}
   I2CGPIOSCLLow(I2C);
  end;

 {Set SDA}
 I2CGPIOSDAHigh(I2C);

 {Set SCL}
 if I2CGPIOSCLHigh(I2C) = ERROR_TIMEOUT then
  begin
   {$IF DEFINED(I2CGPIO_DEBUG) or DEFINED(I2C_DEBUG)}
   if I2C_LOG_ENABLED then I2CLogDebug(@I2C.I2C,'I2CGPIO: Output byte: ' + IntToHex(Data,8) + ' timeout at ACK');
   {$ENDIF}

   Result:=ERROR_TIMEOUT;
   Exit;
  end;

 {Read ACK (SDA should be pulled down by slave, NAK if left high)}
 Level:=I2CGPIOGetSDAValue(I2C);

 {$IF DEFINED(I2CGPIO_DEBUG) or DEFINED(I2C_DEBUG)}
 if I2C_LOG_ENABLED then
  begin
   if Level = GPIO_LEVEL_LOW then Response:='ACK' else Response:='NACK';
   I2CLogDebug(@I2C.I2C,'I2CGPIO: Output byte: ' + IntToHex(Data,8) + ' response is ' + Response);
  end;
 {$ENDIF}

 {Set SCL}
 I2CGPIOSCLLow(I2C);

 if Level = GPIO_LEVEL_LOW then Result:=ERROR_SUCCESS else Result:=ERROR_OPERATION_FAILED;

 //i2c_outb
end;

{==============================================================================}

function I2CGPIOInputByte(I2C:PI2CGPIODevice;var Data:Byte):LongWord;
{Receive a byte over I2C without START or STOP condition, ACK or NAK is sent by caller}
var
 Count:Integer;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Set Defaults}
 Data:=0;

 {Check I2C}
 if I2C = nil then Exit;

 {Starting state SCL is Low}

 {Set SDA}
 I2CGPIOSDAHigh(I2C);

 for Count:=0 to 7 do
  begin
   {Set SCL}
   if I2CGPIOSCLHigh(I2C) = ERROR_TIMEOUT then
    begin
     {$IF DEFINED(I2CGPIO_DEBUG) or DEFINED(I2C_DEBUG)}
     if I2C_LOG_ENABLED then I2CLogDebug(@I2C.I2C,'I2CGPIO: Input byte timeout at bit: ' + IntToStr(Count));
     {$ENDIF}

     Result:=ERROR_TIMEOUT;
     Exit;
    end;

   {Shift Data}
   Data:=Data * 2;

   {Read Bit}
   if I2CGPIOGetSDAValue(I2C) = GPIO_LEVEL_HIGH then Data:=Data or $01;

   {Set SCL}
   I2CGPIOSetSCLValue(I2C,GPIO_LEVEL_LOW);

   {Wait Delay}
   if Count = 7 then MicrosecondDelay(I2C.Delay div 2) else MicrosecondDelay(I2C.Delay);
  end;

 {Ending state SCL is Low}

 Result:=ERROR_SUCCESS;

 //i2c_inb
end;

{==============================================================================}

function I2CGPIOTryAddress(I2C:PI2CGPIODevice;Address:Byte;Retries:LongWord):LongWord;
{Attempt to send an address byte over I2C with multiple retries}
var
 Count:Integer;
 Status:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check I2C}
 if I2C = nil then Exit;

 for Count:=0 to Retries do
  begin
   {Send Address}
   Status:=I2CGPIOOutputByte(I2C,Address);
   if (Status = ERROR_SUCCESS) or (Count = Retries) then Break;

   {Send STOP}
   I2CGPIOI2CStop(I2C);

   {Delay}
   MicrosecondDelay(I2C.Delay);
   Sleep(0);

   {Send START}
   I2CGPIOI2CStart(I2C);
  end;

 Result:=Status;

 //try_address
end;

{==============================================================================}

function I2CGPIOWriteAddress(I2C:PI2CGPIODevice;Address:Word;Read:Boolean):LongWord;
{Write the I2C slave address to the I2C in either 7-bit or 10-bit format}
var
 Data:Byte;
 Status:LongWord;
 Retries:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {$IF DEFINED(I2CGPIO_DEBUG) or DEFINED(I2C_DEBUG)}
 if I2C_LOG_ENABLED then I2CLogDebug(@I2C.I2C,'I2CGPIO: Writing address ' + IntToHex(Address,4));
 {$ENDIF}

 {Check I2C}
 if I2C = nil then Exit;

 {Setup Retries}
 if I2C.IgnoreNAK then Retries:=0 else Retries:=I2CGPIO_RETRY_COUNT;

 if I2CIs10BitAddress(Address) then
  begin
   {Write 10bit address}
   Data:=$F0 or ((Address shr 7) and $06);

   {Try sending the first address byte}
   Status:=I2CGPIOTryAddress(I2C,Data,Retries);
   if (Status = ERROR_OPERATION_FAILED) and not I2C.IgnoreNAK then
    begin
     Result:=Status;
     Exit;
    end
   else if Status <> ERROR_SUCCESS then
    begin
     Result:=Status;
     Exit;
    end;

   {Send the remaining 8bits of the address}
   Status:=I2CGPIOOutputByte(I2C,Address and $FF);
   if (Status = ERROR_OPERATION_FAILED) and not I2C.IgnoreNAK then
    begin
     Result:=Status;
     Exit;
    end
   else if Status <> ERROR_SUCCESS then
    begin
     Result:=Status;
     Exit;
    end;

   {Check Read}
   if Read then
    begin
     {Send REPSTART}
     I2CGPIOI2CRepstart(I2C);

     {Try sending the first byte again with read selected}
     Data:=Data or $01;
     Status:=I2CGPIOTryAddress(I2C,Data,Retries);
     if (Status = ERROR_OPERATION_FAILED) and not I2C.IgnoreNAK then
      begin
       Result:=Status;
       Exit;
      end
     else if Status <> ERROR_SUCCESS then
      begin
       Result:=Status;
       Exit;
      end;
    end;
  end
 else
  begin
   {Write 7bit address}
   Data:=Address shl 1;
   if Read then Data:=Data or $01;

   {Try sending the standard 7bit address format}
   Status:=I2CGPIOTryAddress(I2C,Data,Retries);
   if (Status = ERROR_OPERATION_FAILED) and not I2C.IgnoreNAK then
    begin
     Result:=Status;
     Exit;
    end
   else if Status <> ERROR_SUCCESS then
    begin
     Result:=Status;
     Exit;
    end;
  end;

 Result:=ERROR_SUCCESS;

 //bit_doAddress
end;

{==============================================================================}

function I2CGPIOSendBytes(I2C:PI2CGPIODevice;Data:Pointer;Size:LongWord;var Count:LongWord):LongWord;
{Send multiple bytes over I2C without START or STOP condition, check ACK or NAK from slave}
var
 Buffer:PByte;
 Remain:LongWord;
 Status:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Setup Defaults}
 Count:=0;

 {$IF DEFINED(I2CGPIO_DEBUG) or DEFINED(I2C_DEBUG)}
 if I2C_LOG_ENABLED then I2CLogDebug(@I2C.I2C,'I2CGPIO: Sending ' + IntToStr(Size) + ' bytes');
 {$ENDIF}

 {Check I2C}
 if I2C = nil then Exit;

 {Check Data}
 if Data = nil then Exit;

 Buffer:=Data;
 Remain:=Size;
 while Remain > 0 do
  begin
   {Send Byte}
   Status:=I2CGPIOOutputByte(I2C,Buffer^);
   if (Status = ERROR_OPERATION_FAILED) and not I2C.IgnoreNAK then
    begin
     Result:=Status;
     Exit;
    end
   else if Status <> ERROR_SUCCESS then
    begin
     Result:=Status;
     Exit;
    end;

   {Update Count}
   Inc(Count);
   Inc(Buffer);
   Dec(Remain);
  end;

 Result:=ERROR_SUCCESS;

 //sendbytes
end;

{==============================================================================}

function I2CGPIOAcknowledge(I2C:PI2CGPIODevice;Acknowledge:Boolean):LongWord;
{Send an ACK or NAK to the slave device after reading a byte from I2C}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check I2C}
 if I2C = nil then Exit;

 {Check Acknowledge}
 if Acknowledge then I2CGPIOSetSDAValue(I2C,GPIO_LEVEL_LOW);

 {Wait Delay}
 MicrosecondDelay((I2C.Delay + 1) div 2);

 {Set SCL}
 if I2CGPIOSCLHigh(I2C) = ERROR_TIMEOUT then
  begin
   Result:=ERROR_TIMEOUT;
   Exit;
  end;

 {Set SCL}
 I2CGPIOSCLLow(I2C);

 Result:=ERROR_SUCCESS;

 //acknak
end;

{==============================================================================}

function I2CGPIOReceiveBytes(I2C:PI2CGPIODevice;Data:Pointer;Size:LongWord;var Count:LongWord):LongWord;
{Receive multiple bytes over I2C without START or STOP condition, send ACK or NAK to slave}
var
 Value:Byte;
 Buffer:PByte;
 Remain:LongWord;
 Status:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Setup Defaults}
 Count:=0;

 {$IF DEFINED(I2CGPIO_DEBUG) or DEFINED(I2C_DEBUG)}
 if I2C_LOG_ENABLED then I2CLogDebug(@I2C.I2C,'I2CGPIO: Receiving ' + IntToStr(Size) + ' bytes');
 {$ENDIF}

 {Check I2C}
 if I2C = nil then Exit;

 {Check Data}
 if Data = nil then Exit;

 Buffer:=Data;
 Remain:=Size;
 while Remain > 0 do
  begin
   {Receive Byte}
   Status:=I2CGPIOInputByte(I2C,Value);
   if Status <> ERROR_SUCCESS then
    begin
     Result:=Status;
     Exit;
    end;

   {Update Data}
   Buffer^:=Value;

   {Update Count}
   Inc(Count);
   Inc(Buffer);
   Dec(Remain);

   {Send Acknowledge (Except for last byte)}
   Status:=I2CGPIOAcknowledge(I2C,Remain <> 0);
   if Status <> ERROR_SUCCESS then
    begin
     Result:=Status;
     Exit;
    end;
  end;

 Result:=ERROR_SUCCESS;

 //readbytes
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
