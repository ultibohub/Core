{=============================================================================================}
{ ##                                                                                       ## }
{ ## ADS1X15_Unit                                                                          ## }
{ ##                                                                                       ## }
{ ## Library for ADS1115                                                                   ## }
{ ## Based on:                                                                             ## }
{ ## - Adafruit library from https://github.com/adafruit/Adafruit_CircuitPython_ADS1x15    ## }
{ ## - ads1115_pascal_library: https://github.com/ATILIUS-REGULUS/ads1115_pascal_library   ## }
{ ##                                                                                       ## }
{ ## Copyright (C) 2024   : Eng. EL Mahiri Nabil                                           ## }
{ ## Email                : nabilelmahiri@gmail.com                                        ## }
{ ##                                                                                       ## }
{ ##                                                                                       ## }
{ ##                                                                                       ## }
{=============================================================================================}


{$mode delphi}{Default to Delphi compatible syntax}
{$H+}{Default to AnsiString}
{$inline on}{Allow use of Inline procedures}

unit ADS1X15;

interface

uses GlobalConfig, GlobalConst, GlobalTypes, Platform, Threads, Devices,
  I2C, GPIO, Console, Classes, ctypes, SysUtils;

{==============================================================================}
{Global definitions}
{$INCLUDE ..\core\GlobalDefines.inc}

const

  {==============================================================================}
  {I2C general data    }
  {==============================================================================}
  {ADS1X15 specific constants}
  ADS1X15_DEVICE_DESCRIPTION = 'ADS1X15 I2C 12/16Bit 4-Ch ADC Module';
  {Description of ADS1X15 device}
  ADS1X15_SIGNATURE = $000AF163;
  ADS1X15_MODEL_0 = 0; {12 bit}
  ADS1X15_MODEL_1 = 1; {16 bit}

  ADS1015 = False;
  ADS1115 = True;
  ADS1X15_ADDRESS = $48;            // I2C adress
  ADS1X15_I2C_SPEED = 100000;
  ADS1X15_I2C_BUFFER_MAX_SIZE = 32;             // I2C maximal buffer size
  ADS1X15_DEFAULT_CHANNEL = $4000;
  ADS1X15_DEFAULT_THRESHOLD = $0000;
  ADS1115_M_BitShift = 0;
  ADS1015_M_BitShift = 4;
  {==============================================================================}
  {Pointer register }
  {==============================================================================}
  ADS1X15_REGISTER_POINTER_MASK = $03;
  ADS1X15_REGISTER_POINTER_CONVERT = $00;
  ADS1X15_REGISTER_POINTER_CONFIG = $01;
  ADS1X15_REGISTER_POINTER_LOW_THRESHOLD = $02;
  ADS1X15_REGISTER_POINTER_HIGH_THRESHOLD = $03;

  {==============================================================================}
  {Config register }
  {==============================================================================}
  ADS1X15_REGISTER_CONFIG_OS_MASK = $8000;
  ADS1X15_REGISTER_CONFIG_OS_NOEFFECT = $0000;  // When writing : No effect
  ADS1X15_REGISTER_CONFIG_OS_SINGLE = $8000;
  // When writing : Set to start a single-conversion
  ADS1X15_REGISTER_CONFIG_OS_BUSY = $0000;
  // When reading : Bit = 0 when conversion is in progress
  ADS1X15_REGISTER_CONFIG_OS_NOTBUSY = $8000;
  // When reading : Bit = 1 when device is not performing a conversion

  ADS1X15_REGISTER_CONFIG_MUX_MASK = $7000;
  ADS1X15_REGISTER_CONFIG_MUX_DIFF_0_1 = $0000;
  // Differential P = AIN0, N = AIN1 (default)
  ADS1X15_REGISTER_CONFIG_MUX_DIFF_0_3 = $1000;  // Differential P = AIN0, N = AIN3
  ADS1X15_REGISTER_CONFIG_MUX_DIFF_1_3 = $2000;  // Differential P = AIN1, N = AIN3
  ADS1X15_REGISTER_CONFIG_MUX_DIFF_2_3 = $3000;  // Differential P = AIN2, N = AIN3
  ADS1X15_REGISTER_CONFIG_MUX_SINGLE_0 = $4000;  // Single-ended AIN0
  ADS1X15_REGISTER_CONFIG_MUX_SINGLE_1 = $5000;  // Single-ended AIN1
  ADS1X15_REGISTER_CONFIG_MUX_SINGLE_2 = $6000;  // Single-ended AIN2
  ADS1X15_REGISTER_CONFIG_MUX_SINGLE_3 = $7000;  // Single-ended AIN3

  ADS1X15_REGISTER_CONFIG_PGA_MASK = $0E00;
  ADS1X15_REGISTER_CONFIG_PGA_6_144V = $0000;  // +/-6.144V range = Gain 2/3
  ADS1X15_REGISTER_CONFIG_PGA_4_096V = $0200;
  // +/-4.096V range = Gain 1 (new constructor default)
  ADS1X15_REGISTER_CONFIG_PGA_2_048V = $0400;
  // +/-2.048V range = Gain 2 (default)
  ADS1X15_REGISTER_CONFIG_PGA_1_024V = $0600;  // +/-1.024V range = Gain 4
  ADS1X15_REGISTER_CONFIG_PGA_0_512V = $0800;  // +/-0.512V range = Gain 8
  ADS1X15_REGISTER_CONFIG_PGA_0_256V = $0A00;  // +/-0.256V range = Gain 16

  ADS1X15_REGISTER_CONFIG_MODE_MASK = $0100;
  ADS1X15_REGISTER_CONFIG_MODE_CONTINUOUS = $0000;  // Continuous conversion mode
  ADS1X15_REGISTER_CONFIG_MODE_SINGLE = $0100;
  // Power-down single-shot mode (default)

  ADS1X15_DEFAULT_DATA_RATE = 4;
  ADS1X15_REGISTER_CONFIG_DR_MASK = $00E0;

  ADS1115_REGISTER_CONFIG_DR_8SPS = $0000;  // 8 samples per second
  ADS1115_REGISTER_CONFIG_DR_16SPS = $0020;  // 16 samples per second
  ADS1115_REGISTER_CONFIG_DR_32SPS = $0040;  // 32 samples per second
  ADS1115_REGISTER_CONFIG_DR_64SPS = $0060;  // 64 samples per second
  ADS1115_REGISTER_CONFIG_DR_128SPS = $0080;  // 128 samples per second (default)
  ADS1115_REGISTER_CONFIG_DR_250SPS = $00A0;  // 250 samples per second
  ADS1115_REGISTER_CONFIG_DR_475SPS = $00C0;  // 475 samples per second
  ADS1115_REGISTER_CONFIG_DR_860SPS = $00E0;  // 860 samples per second

  ADS1015_REGISTER_CONFIG_DR_128SPS = $0000;  // 128 samples per second
  ADS1015_REGISTER_CONFIG_DR_250SPS = $0020;  // 250 samples per second
  ADS1015_REGISTER_CONFIG_DR_490SPS = $0040;  // 490 samples per second
  ADS1015_REGISTER_CONFIG_DR_920SPS = $0060;  // 920 samples per second
  ADS1015_REGISTER_CONFIG_DR_1600SPS = $0080;
  // 1600 samples per second (default)
  ADS1015_REGISTER_CONFIG_DR_2400SPS = $00A0;  // 2400 samples per second
  ADS1015_REGISTER_CONFIG_DR_3300SPS = $00C0;  // 3300 samples per second

  ADS1X15_REGISTER_CONFIG_CMODE_MASK = $0010;
  ADS1X15_REGISTER_CONFIG_CMODE_TRADITIONAL = $0000;
  // Traditional comparator with hysteresis (default)
  ADS1X15_REGISTER_CONFIG_CMODE_WINDOW = $0010;  // Window comparator

  ADS1X15_REGISTER_CONFIG_CPOL_MASK = $0008;
  ADS1X15_REGISTER_CONFIG_CPOL_ACTVLOW = $0000;
  // ALERT/RDY pin is low when active (default)
  ADS1X15_REGISTER_CONFIG_CPOL_ACTVHIGH = $0008;
  // ALERT/RDY pin is high when active

  ADS1X15_REGISTER_CONFIG_CLAT_MASK = $0004;
  // Determines if ALERT/RDY pin latches once asserted
  ADS1X15_REGISTER_CONFIG_CLAT_NONLATCH = $0000;
  // Non-latching comparator (default)
  ADS1X15_REGISTER_CONFIG_CLAT_LATCH = $0004;  // Latching comparator

  ADS1X15_REGISTER_CONFIG_CQUE_MASK = $0003;
  ADS1X15_REGISTER_CONFIG_CQUE_1CONV = $0000;
  // Assert ALERT/RDY after one conversions
  ADS1X15_REGISTER_CONFIG_CQUE_2CONV = $0001;
  // Assert ALERT/RDY after two conversions
  ADS1X15_REGISTER_CONFIG_CQUE_4CONV = $0002;
  // Assert ALERT/RDY after four conversions
  ADS1X15_REGISTER_CONFIG_CQUE_NONE = $0003;
// Disable the comparator and put ALERT/RDY in high state (default)

{==============================================================================}

type

  T_I2C_Buffer_A = packed array [0 .. ADS1X15_I2C_BUFFER_MAX_SIZE + 1] of uint8_t;
  Mux_By_Channel_Array = packed array [0 .. 3] of uint16_t;
  Data_Rates_Array = packed array [0 .. 7] of uint16_t;

  {ADS1X15 specific types}
  PADS1X15 = ^TADS1X15;

  TADS1X15 = record
    Signature: longword;     {Signature for entry validation}
    Model: longword;         {ADC model (eg ADS1X15_MODEL_1)}
    I2C: PI2CDevice;         {I2C device for this ADC module}
    GPIO: PGPIODevice;       {GPIO device for this ADC module}
    M_I2C_Address: uint8_t;
    M_Gain: uint16_t;
    M_Data_Rate: uint16_t;
    M_I2C_Handle: CInt;
    M_BitShift: uint8_t;


  end;

{==============================================================================}

const
  MUX_BY_CHANNEL: Mux_By_Channel_Array = (
    ADS1X15_REGISTER_CONFIG_MUX_SINGLE_0,
    ADS1X15_REGISTER_CONFIG_MUX_SINGLE_1,
    ADS1X15_REGISTER_CONFIG_MUX_SINGLE_2,
    ADS1X15_REGISTER_CONFIG_MUX_SINGLE_3);

  ADS1115_DATA_RATES: Data_Rates_Array = (
    ADS1115_REGISTER_CONFIG_DR_8SPS,
    ADS1115_REGISTER_CONFIG_DR_16SPS,
    ADS1115_REGISTER_CONFIG_DR_32SPS,
    ADS1115_REGISTER_CONFIG_DR_64SPS,
    ADS1115_REGISTER_CONFIG_DR_128SPS,
    ADS1115_REGISTER_CONFIG_DR_250SPS,
    ADS1115_REGISTER_CONFIG_DR_475SPS,
    ADS1115_REGISTER_CONFIG_DR_860SPS
    );

  ADS1015_DATA_RATES: Data_Rates_Array = (
    ADS1015_REGISTER_CONFIG_DR_128SPS,
    ADS1015_REGISTER_CONFIG_DR_250SPS,
    ADS1015_REGISTER_CONFIG_DR_490SPS,
    ADS1015_REGISTER_CONFIG_DR_920SPS,
    ADS1015_REGISTER_CONFIG_DR_1600SPS,
    ADS1015_REGISTER_CONFIG_DR_2400SPS,
    ADS1015_REGISTER_CONFIG_DR_3300SPS,
    ADS1015_REGISTER_CONFIG_DR_3300SPS
    );

var
  {ADS1X15 specific variables}
  ADS1X15_Default: TThreadHandle = INVALID_HANDLE_VALUE;
  ADS1X15_MODEL: longword = ADS1X15_MODEL_1;
  ADS1X15_I2C_ADDRESS: word = $48;
  ADS1X15_I2C_DEVICE: string = 'I2C0';
  ADS1X15_Initialized: boolean;

procedure ADS1X15_Init;

function ADS1X15_Create(Handle: TThreadHandle; I2C_Adress: uint8_t = ADS1X15_ADDRESS;
  Gain: uint16_t = ADS1X15_REGISTER_CONFIG_PGA_4_096V; Is_ADS1015: boolean = False;
  DATA_RATE: uint8_t = ADS1X15_DEFAULT_DATA_RATE): TThreadHandle;

function ADS1X15_Start(Model: longword; const Device: string;
  Address: word): TThreadHandle;

function ADS1X15_Stop(Handle: TThreadHandle): boolean;
function ADS1X15_GPIODestroy(GPIO: PGPIODevice): longword;
function ADS1X15_I2C_Write_Register(Handle: TThreadHandle; I2C_Register: uint8_t;
  Value: uint16_t; I2C_Adress: uint8_t = ADS1X15_ADDRESS): integer;
function ADS1X15_I2C_Read_Register(Handle: TThreadHandle; I2C_Register: uint8_t;
  I2C_Adress: uint8_t = ADS1X15_ADDRESS): integer;

procedure ADS1X15_Start_ADC_Reading(Handle: TThreadHandle;
  Mux: uint16_t = ADS1X15_REGISTER_CONFIG_MUX_SINGLE_0; Continuous: boolean = False;
  RDY_Mode_Enabled: boolean = False);
procedure ADS1X15_Start_ALERT_RDY(Handle: TThreadHandle);
procedure ADS1X15_Stop_ALERT_RDY(Handle: TThreadHandle);

function ADS1X15_Read_ADC_Single_Ended(Handle: TThreadHandle;
  Channel: uint8_t = ADS1X15_DEFAULT_CHANNEL; Continuous: boolean = False;
  RDY_Mode_Enabled: boolean = False): integer;
function ADS1X15_Read_ADC_Differential_0_1_Conversion(Handle: TThreadHandle): integer;
function ADS1X15_Read_ADC_Differential_0_3_Conversion(Handle: TThreadHandle): integer;
function ADS1X15_Read_ADC_Differential_1_3_Conversion(Handle: TThreadHandle): integer;
function ADS1X15_Read_ADC_Differential_2_3_Conversion(Handle: TThreadHandle): integer;

procedure ADS1X15_Start_Comparator_Single_Ended(Handle: TThreadHandle;
  Channel: uint8_t; Threshold: uint16_t);
function ADS1X15_Get_Last_Conversion_Result(Handle: TThreadHandle): integer;
function ADS1X15_Conversion_Complete(Handle: TThreadHandle): boolean;
function ADS1X15_Compute_Volts(Handle: TThreadHandle; Counts: int16_t): double;

implementation

{==============================================================================}
{Initialization Functions}
procedure ADS1X15_Init;
{Initialize the ADS1X15 unit and create, register and start the device}
{Note: Called only during system startup}
var
  WorkInt: longword;
  WorkBuffer: string;
begin
  {Check Initialized}
  if ADS1X15_Initialized then Exit;

  {Check Environment Variables}
  {ADS1X15_AUTOSTART}
  WorkInt := StrToIntDef(SysUtils.GetEnvironmentVariable('ADS1X15_AUTOSTART'), 1);
  if WorkInt = 0 then ADS1X15_AUTOSTART := False;

  {ADS1X15_MODEL}
  WorkInt := StrToIntDef(SysUtils.GetEnvironmentVariable('ADS1X15_MODEL'),
    ADS1X15_MODEL);
  if WorkInt <> ADS1X15_MODEL then ADS1X15_MODEL := WorkInt;

  {ADS1X15_I2C_ADDRESS}
  WorkInt := StrToIntDef(SysUtils.GetEnvironmentVariable('ADS1X15_I2C_ADDRESS'), 0);
  if WorkInt > 0 then ADS1X15_I2C_ADDRESS := WorkInt;

  {ADS1X15_I2C_DEVICE}
  WorkBuffer := SysUtils.GetEnvironmentVariable('ADS1X15_I2C_DEVICE');
  if Length(WorkBuffer) <> 0 then ADS1X15_I2C_DEVICE := WorkBuffer;

   if BCM2711_REGISTER_I2C0 or BCM2710_REGISTER_I2C0 or BCM2709_REGISTER_I2C0 or BCM2708_REGISTER_I2C0 then
   begin
       ADS1X15_I2C_DEVICE := 'I2C1';
   end;

  {Start ADS1X15 ADC}
  if ADS1X15_AUTOSTART then
  begin
    ADS1X15_Default := ADS1X15_Start(ADS1X15_MODEL, ADS1X15_I2C_DEVICE,
      ADS1X15_I2C_ADDRESS);
  end;

  ADS1X15_Initialized := True;

  {$IF DEFINED(ADS1X15_DEBUG)}
  DeviceLogDebug(nil, 'ADS1X15: Initialized !');
  {$ENDIF}
end;

{==============================================================================}
function ADS1X15_Start(Model: longword; const Device: string;
  Address: word): TThreadHandle;
  {Start the ADS1X15 driver and register the GPIO and Console devices associated with the display}
  {Model: The ADS1X15 model (eg ADS1X15_MODEL_1)}
  {Invert: Invert the signal level for the LCD backlight (If True then GPIO_LEVEL_LOW equals On)}
  {Device: The I2C device that the ADS1X15 I/O Expander on the LCD Plate is connected to}
  {Address: The I2C address of the ADS1X15 I/O Expander on the LCD Plate}
  {Return: The handle of the  ADS1X15 on success or INVALID_HANDLE_VALUE on failure}

{Note: This function will be called during startup if the parameter ADS1X15_AUTOSTART is True
       Can be called multiple times to support more than one ADC Module}
var
  I2C: PI2CDevice;
  GPIO: PGPIODevice;
  MADS1X15: PADS1X15;
begin

  //Result := INVALID_HANDLE_VALUE;

  {Check Model}
  if Model > ADS1X15_MODEL_1 then Model := ADS1X15_MODEL_1;

  {Check Device}
  if Length(Device) = 0 then Exit;

  {Check Address}
  if Address = I2C_ADDRESS_INVALID then Address := ADS1X15_I2C_ADDRESS;


  {Check I2C Device}
  I2C := PI2CDevice(DeviceFindByName(Device));
  if I2C = nil then
  begin
    I2C := PI2CDevice(DeviceFindByDescription(Device));
    if I2C = nil then Exit;
  end;

  {Create GPIO Device}
  GPIO := nil;

  {Create  MADS1X15}
  MADS1X15 := AllocMem(SizeOf(TADS1X15));
  if MADS1X15 = nil then Exit;

  { Initialize data }
  MADS1X15.Signature := ADS1X15_SIGNATURE;
  MADS1X15.Model := ADS1X15_MODEL_1;
  MADS1X15.I2C := I2C;
  MADS1X15.GPIO := GPIO;
  MADS1X15.M_I2C_Address := Address;
  MADS1X15.M_Gain := ADS1X15_REGISTER_CONFIG_PGA_4_096V;
  MADS1X15.M_Data_Rate := ADS1115_DATA_RATES[ADS1X15_DEFAULT_DATA_RATE];
  MADS1X15.M_BitShift := ADS1115_M_BitShift;


  { Connect as I2C slave }
  if I2CDeviceStart(MADS1X15.I2C, ADS1X15_I2C_SPEED) <> ERROR_SUCCESS then
  begin
    {$IF DEFINED(ADS1X15_DEBUG) or DEFINED(I2C_DEBUG)}
    if I2C_LOG_ENABLED then I2CLogDebug(MADS1X15.I2C, 'ADS1X15: I2C Failed to start !');
    {$ENDIF}
    Exit;
  end;

  {Return Result}
  Result := TThreadHandle(MADS1X15);

  {Check Default}
  if ADS1X15_Default = INVALID_HANDLE_VALUE then
  begin
    ADS1X15_Default := Result;
  end;
  {$IF DEFINED(ADS1X15_DEBUG)}
  DeviceLogDebug(nil, 'ADS1X15: Started !');
  {$ENDIF}

end;

{==============================================================================}

function ADS1X15_Stop(Handle: TThreadHandle): boolean;
  {Stop the ADS1X15 driver and deregister the GPIO and Console devices associated with the module}
  {Handle: The handle of the ADS1X15 or INVALID_HANDLE_VALUE for the default module}
  {Return: True if completed or False on failure}
var
  MADS1X15: PADS1X15;
begin

  Result := False;

  {Check Handle}
  if Handle = INVALID_HANDLE_VALUE then Handle := ADS1X15_Default;
  if Handle = INVALID_HANDLE_VALUE then Exit;

  {Get Module}
  MADS1X15 := PADS1X15(Handle);
  if MADS1X15 = nil then Exit;
  if MADS1X15.Signature <> ADS1X15_SIGNATURE then Exit;

  {Check GPIO Device}
  if MADS1X15.GPIO <> nil then
  begin
    {Destroy GPIO Device}
    if ADS1X15_GPIODestroy(MADS1X15.GPIO) = ERROR_SUCCESS then
    begin
      MADS1X15.GPIO := nil;
    end;
  end;

  {Check Default}
  if ADS1X15_Default = TThreadHandle(PtrUInt(MADS1X15)) then
  begin
    ADS1X15_Default := INVALID_HANDLE_VALUE;
  end;

  {Invalidate MADS1X15}
  MADS1X15.Signature := 0;

  {Destroy MADS1X15}
  FreeMem(MADS1X15);

  {Return Result}
  Result := True;

end;


{==============================================================================}
function ADS1X15_Create(Handle: TThreadHandle; I2C_Adress: uint8_t = ADS1X15_ADDRESS;
  Gain: uint16_t = ADS1X15_REGISTER_CONFIG_PGA_4_096V; Is_ADS1015: boolean = False;
  DATA_RATE: uint8_t = ADS1X15_DEFAULT_DATA_RATE): TThreadHandle;
  { Initialization of the object                                                 }

var
  MADS1X15: PADS1X15;
  I2C: PI2CDevice;
begin

  {Create  MADS1X15}
  MADS1X15 := PADS1X15(Handle);
  if MADS1X15 = nil then
  begin
    Handle := ADS1X15_Start(ADS1X15_MODEL, ADS1X15_I2C_DEVICE, ADS1X15_I2C_ADDRESS);
    MADS1X15 := PADS1X15(Handle);
  end;

  WriteLn('ADS1X15_Create Initialize');
  { Initialize data }
  MADS1X15.M_I2C_Address := I2C_Adress;
  MADS1X15.M_Gain := Gain;

  if DATA_RATE > 7 then DATA_RATE := ADS1X15_DEFAULT_DATA_RATE; // 0-->7
  if Is_ADS1015 = ADS1015 then
  begin
    MADS1X15.M_Data_Rate := ADS1015_DATA_RATES[DATA_RATE];
    MADS1X15.M_BitShift := ADS1015_M_BitShift;
  end
  else
  begin
    MADS1X15.M_Data_Rate := ADS1115_DATA_RATES[DATA_RATE];
    MADS1X15.M_BitShift := ADS1115_M_BitShift;
  end;

  {Return Result}
  Result := TThreadHandle(MADS1X15);
  ADS1X15_Default := Result;

 {$IF DEFINED(ADS1X15_DEBUG)}
  DeviceLogDebug(nil, 'ADS1X15: Created !');
  {$ENDIF}

end;

{==============================================================================}
function ADS1X15_I2C_Write_Register(Handle: TThreadHandle; I2C_Register: uint8_t;
  Value: uint16_t; I2C_Adress: uint8_t = ADS1X15_ADDRESS): integer; inline;
  { Return last conversion result                                                }

var
  Count: longword;
  MADS1X15: PADS1X15;
  I2C_Buffer: packed array [0 .. 9] of uint8_t;
begin

  MADS1X15 := PADS1X15(Handle);
  if MADS1X15 = nil then Exit;

  I2C_Buffer[0] := Value shr 8;
  I2C_Buffer[1] := Value and $FF;

  if I2CDeviceWriteWrite(MADS1X15.I2C, I2C_Adress, @I2C_Register,
    SizeOf(byte), @I2C_Buffer, 2, Count) <> ERROR_SUCCESS then
  begin
    Result := -1;
    {$IF DEFINED(ADS1X15_DEBUG) or DEFINED(I2C_DEBUG)}
    if I2C_LOG_ENABLED then I2CLogDebug(MADS1X15.I2C,
        'ADS1X15: I2C Write Register Error');
    {$ENDIF}
  end;

end;

{==============================================================================}
function ADS1X15_I2C_Read_Register(Handle: TThreadHandle; I2C_Register: uint8_t;
  I2C_Adress: uint8_t = ADS1X15_ADDRESS): integer; inline;
  { Return last conversion result                                                           }

var
  Count: longword;
  MADS1X15: PADS1X15;
  I2C_Buffer: T_I2C_Buffer_A;

begin

  MADS1X15 := PADS1X15(Handle);
  if MADS1X15 = nil then Exit;

  I2C_Buffer[0] := I2C_Register;

  if I2CDeviceWriteRead(MADS1X15.I2C, I2C_Adress, @I2C_Register,
    SizeOf(byte), @I2C_Buffer, 2, Count) <> ERROR_SUCCESS then
  begin
    Result := -1;
    {$IF DEFINED(ADS1X15_DEBUG) or DEFINED(I2C_DEBUG)}
    if I2C_LOG_ENABLED then I2CLogDebug(MADS1X15.I2C,
        'ADS1X15: I2C Read Register Error');
    {$ENDIF}
  end
  else
  begin
    Result := (I2C_Buffer[0] shl 8) or I2C_Buffer[1];
  end;

end;


{==============================================================================}
procedure ADS1X15_Start_ALERT_RDY(Handle: TThreadHandle);
{ Start ALERT/RDY interupt signal                                              }

var
  Result: integer;

begin { PADS1X15.Start_ALERT_RDY }

  { Turn on ALERT/RDY : Set MSB of Hi_thresh to 1 }
  Result := ADS1X15_I2C_Write_Register(
    Handle, ADS1X15_REGISTER_POINTER_HIGH_THRESHOLD, $8000);
  if Result < 0 then
  begin
    {$IF DEFINED(ADS1X15_DEBUG)}
    DeviceLogDebug(nil, 'ADS1X15: Error writing to I2C (Set MSB of Hi_thresh to 1)');
    {$ENDIF}
    Exit;
  end;

  { Turn on ALERT/RDY : Set MSB of Lo_thresh to 0 }
  Result := ADS1X15_I2C_Write_Register(
    Handle, ADS1X15_REGISTER_POINTER_LOW_THRESHOLD, $0000);
  if Result < 0 then
  begin
     {$IF DEFINED(ADS1X15_DEBUG)}
    DeviceLogDebug(nil, 'ADS1X15: Error writing to I2C (Set MSB of Lo_thresh to 0)');
    {$ENDIF}
    Exit;
  end;

end; { PADS1X15.Start_ALERT_RDY }

{==============================================================================}
procedure ADS1X15_Stop_ALERT_RDY(Handle: TThreadHandle);
{ Stop ALERT/RDY interupt signal                                               }
{==============================================================================}
var
  Result: integer;
begin { PADS1X15.Stop_ALERT_RDY }

  { Turn off ALERT/RDY : Set MSB of Hi_thresh to 0 }
  Result := ADS1X15_I2C_Write_Register(
    Handle, ADS1X15_REGISTER_POINTER_HIGH_THRESHOLD, $0000);
  if Result < 0 then
  begin

     {$IF DEFINED(ADS1X15_DEBUG)}
    DeviceLogDebug(nil, 'ADS1X15: Error writing to I2C (Set MSB of Hi_thresh to 0)');
    {$ENDIF}
    Exit;
  end;

  { Turn off ALERT/RDY : Set MSB of Lo_thresh to 1 }
  Result := ADS1X15_I2C_Write_Register(
    Handle, ADS1X15_REGISTER_POINTER_LOW_THRESHOLD, $FFFF);
  if Result < 0 then
  begin
    {$IF DEFINED(ADS1X15_DEBUG)}
    DeviceLogDebug(nil, 'ADS1X15: Error writing to I2C (Set MSB of Lo_thresh to 01)');
    {$ENDIF}
    Exit;
  end;

end; { PADS1X15.Stop_ALERT_RDY }

{==============================================================================}
procedure ADS1X15_Start_Comparator_Single_Ended(Handle: TThreadHandle;
  Channel: uint8_t; Threshold: uint16_t);
{
      @brief  Sets up the comparator to operate in basic mode, causing the
            ALERT/RDY pin to assert (go from high to low) when the ADC
            value exceeds the specified threshold.

            This will also set the ADC in continuous conversion mode.

    @param channel ADC channel to use
    @param threshold comparator threshold
}

var
  MADS1X15: PADS1X15;
  Config: uint16_t;

begin { PADS1X15.Start_Comparator_Single_Ended }

  MADS1X15 := PADS1X15(Handle);
  if MADS1X15 = nil then Exit;

  // Start with default values
  Config := ADS1X15_REGISTER_CONFIG_CQUE_1CONV or
    ADS1X15_REGISTER_CONFIG_CLAT_NONLATCH or ADS1X15_REGISTER_CONFIG_CPOL_ACTVLOW or
    ADS1X15_REGISTER_CONFIG_CMODE_TRADITIONAL or
    ADS1X15_REGISTER_CONFIG_MODE_CONTINUOUS;


  // Set PGA/voltage range
  Config := Config or MADS1X15.M_Gain;

  // Set data rate
  Config := Config or MADS1X15.M_Data_Rate;


  // Set channels
  Config := Config or MUX_BY_CHANNEL[Channel];

  // Set 'start single-conversion' bit
  Config := Config or ADS1X15_REGISTER_CONFIG_OS_SINGLE;

  // Set the high threshold register
  // Shift 12-bit results left 4 bits for the ADS1015
  ADS1X15_I2C_Write_Register(Handle, ADS1X15_REGISTER_POINTER_HIGH_THRESHOLD,
    Threshold shl MADS1X15.M_BitShift);

  // Write config register to the ADC
  ADS1X15_I2C_Write_Register(Handle, ADS1X15_REGISTER_POINTER_CONFIG, Config);

end; { PADS1X15.Start_Comparator_Single_Ended }


{==============================================================================}
function ADS1X15_Get_Last_Conversion_Result(Handle: TThreadHandle): integer;
{
     @brief  In order to clear the comparator, we need to read the
            conversion results.  This function reads the last conversion
            results without changing the config value.

    @return the last ADC reading
}
var
  MADS1X15: PADS1X15;
begin { PADS1X15.Get_Last_Conversion_Result }

  MADS1X15 := PADS1X15(Handle);
  if MADS1X15 = nil then Exit;

  Result := ADS1X15_I2C_Read_Register(Handle, ADS1X15_REGISTER_POINTER_CONVERT);
  if Result < 0 then
  begin
    Exit;
  end;

  Result := Result shr MADS1X15.M_BitShift;

  if MADS1X15.M_BitShift <> 0 then
  begin
    if Result > $07FF then
    begin
      // negative number - extend the sign to 16th bit
      Result := Result or $F000;
    end;
  end;
  Result := int16_t(Result);

end; { PADS1X15.Get_Last_Conversion_Result }

{==============================================================================}
function ADS1X15_Conversion_Complete(Handle: TThreadHandle): boolean;
{
   @brief  Returns true if conversion is complete, false otherwise.

   @return True if conversion is complete, false otherwise.
}
var
  Result_Value: integer;
begin
  Result_Value := ADS1X15_I2C_Read_Register(Handle, ADS1X15_REGISTER_POINTER_CONFIG);
  if Result_Value < 0 then
  begin
    Result := False;
  end;
  Result := (Result_Value and $8000) <> 0;
end;  { PADS1X15.Conversion_Complete }

{==============================================================================}
function ADS1X15_Compute_Volts(Handle: TThreadHandle; Counts: int16_t): double;
{
   @brief  Compute volts for the given raw counts.

    @param counts the ADC reading in raw counts

    @return the ADC reading in volts
}

var
  MADS1X15: PADS1X15;
  Fs_Range: double;
begin { PADS1X15.Compute_Volts }

  MADS1X15 := PADS1X15(Handle);
  if MADS1X15 = nil then Exit;

  if MADS1X15.M_Gain = ADS1X15_REGISTER_CONFIG_PGA_6_144V then
  begin
    Fs_Range := 6.144;
  end
  else if MADS1X15.M_Gain = ADS1X15_REGISTER_CONFIG_PGA_4_096V then
  begin
    Fs_Range := 4.096;
  end
  else if MADS1X15.M_Gain = ADS1X15_REGISTER_CONFIG_PGA_2_048V then
  begin
    Fs_Range := 2.048;
  end
  else if MADS1X15.M_Gain = ADS1X15_REGISTER_CONFIG_PGA_1_024V then
  begin
    Fs_Range := 1.024;
  end
  else if MADS1X15.M_Gain = ADS1X15_REGISTER_CONFIG_PGA_0_512V then
  begin
    Fs_Range := 0.512;
  end
  else if MADS1X15.M_Gain = ADS1X15_REGISTER_CONFIG_PGA_0_256V then
  begin
    Fs_Range := 0.256;
  end;

  Result := double(Counts * (Fs_Range / (32768 shr MADS1X15.M_BitShift)));
  //(32768 shr M_BitShift)));

end; { PADS1X15.Compute_Volts }


{==============================================================================}
procedure ADS1X15_Start_ADC_Reading(Handle: TThreadHandle; Mux: uint16_t;
  Continuous: boolean = False; RDY_Mode_Enabled: boolean = False);
{
@brief  Non-blocking start conversion function

 Call getLastConversionResults() once conversionComplete() returns true.
 In continuous mode, getLastConversionResults() will always return the
 latest result.
 ALERT/RDY pin is set to RDY mode, and a 8us pulse is generated every
 time new data is ready.

 @param mux mux field value
 @param continuous continuous if set, otherwise single shot
}

var
  MADS1X15: PADS1X15;
  Config: uint16_t;

begin { PADS1X15.Start_ADC_Reading }

  MADS1X15 := PADS1X15(Handle);
  if MADS1X15 = nil then Exit;

  // Start with default values
  Config := ADS1X15_REGISTER_CONFIG_CQUE_1CONV or
    ADS1X15_REGISTER_CONFIG_CLAT_NONLATCH or ADS1X15_REGISTER_CONFIG_CPOL_ACTVLOW or
    ADS1X15_REGISTER_CONFIG_CMODE_TRADITIONAL;

  if Continuous then
  begin
    Config := Config or ADS1X15_REGISTER_CONFIG_MODE_CONTINUOUS;
  end
  else
  begin
    Config := Config or ADS1X15_REGISTER_CONFIG_MODE_SINGLE;
  end;

  // Set PGA/voltage range
  Config := Config or MADS1X15.M_Gain;

  // Set data rate
  Config := Config or MADS1X15.M_Data_Rate;


  // Set channels
  Config := Config or Mux;

  // Set 'start single-conversion' bit
  Config := Config or ADS1X15_REGISTER_CONFIG_OS_SINGLE;

  // Write config register to the ADC
  ADS1X15_I2C_Write_Register(Handle, ADS1X15_REGISTER_POINTER_CONFIG, Config);

  // Set ALERT/RDY to RDY mode.
  if RDY_Mode_Enabled then
  begin
    ADS1X15_I2C_Write_Register(Handle, ADS1X15_REGISTER_POINTER_HIGH_THRESHOLD, Config);
    ADS1X15_I2C_Write_Register(Handle, ADS1X15_REGISTER_POINTER_LOW_THRESHOLD, Config);
  end;

end; { PADS1X15.Start_ADC_Reading }


{==============================================================================}
function ADS1X15_Read_ADC_Single_Ended(Handle: TThreadHandle;
  Channel: uint8_t = ADS1X15_DEFAULT_CHANNEL; Continuous: boolean = False;
  RDY_Mode_Enabled: boolean = False): integer;

var
  MADS1X15: PADS1X15;
begin { PADS1X15.Read_ADC_Single_Ended }

  MADS1X15 := PADS1X15(Handle);
  if MADS1X15 = nil then Exit;

  if Channel > 3 then
  begin
    Exit(0);
  end;

  ADS1X15_Start_ADC_Reading(Handle, MUX_BY_CHANNEL[channel], Continuous,
    RDY_Mode_Enabled);

  if not Continuous then
  begin
    // Wait for conversion to complete
    // ADS1x1x devices settle within a single conversion cycle
    // Continuously poll conversion complete status bit
    // Wait for the conversion to complete
    while not ADS1X15_Conversion_Complete(Handle) do
    begin
    end;
  end
  else
  begin
    // Can't poll registers in CONTINUOUS mode
    // Wait expected time for two conversions to complete
    sleep(2000 div MADS1X15.M_Data_Rate);
  end;

  // Read the conversion results
  Result := ADS1X15_Get_Last_Conversion_Result(Handle);

end; { PADS1X15.Read_ADC_Single_Ended }

{==============================================================================}
function ADS1X15_Read_ADC_Differential_0_1_Conversion(Handle: TThreadHandle): integer;
{
  @brief  Reads the conversion results, measuring the voltage
          difference between the P (AIN0) and N (AIN1) input.  Generates
          a signed value since the difference can be either
          positive or negative.

  @return the ADC reading
 }


begin { PADS1X15.Start_ADC_Differential_0_1_Conversion }


  ADS1X15_Start_ADC_Reading(Handle, ADS1X15_REGISTER_CONFIG_MUX_DIFF_0_1, False);

  // Wait for the conversion to complete
  while not ADS1X15_Conversion_Complete(Handle) do
  begin
  end;

  // Read the conversion results
  Result := ADS1X15_Get_Last_Conversion_Result(Handle);

end; { PADS1X15.Start_ADC_Differential_0_1_Conversion }

{==============================================================================}
function ADS1X15_Read_ADC_Differential_0_3_Conversion(Handle: TThreadHandle): integer;
{
  @brief  Reads the conversion results, measuring the voltage
          difference between the P (AIN0) and N (AIN3) input.  Generates
          a signed value since the difference can be either
          positive or negative.
  @return the ADC reading
}


begin { PADS1X15.Read_ADC_Differential_0_3_Conversion }

  ADS1X15_Start_ADC_Reading(Handle, ADS1X15_REGISTER_CONFIG_MUX_DIFF_0_3, False);

  // Wait for the conversion to complete
  while not ADS1X15_Conversion_Complete(Handle) do
  begin
  end;

  // Read the conversion results
  Result := ADS1X15_Get_Last_Conversion_Result(Handle);
end; { PADS1X15.Read_ADC_Differential_0_3_Conversion }

{==============================================================================}
function ADS1X15_Read_ADC_Differential_1_3_Conversion(Handle: TThreadHandle): integer;
{
     @brief  Reads the conversion results, measuring the voltage
            difference between the P (AIN1) and N (AIN3) input.  Generates
            a signed value since the difference can be either
            positive or negative.
    @return the ADC reading
}


begin { PADS1X15.Read_ADC_Differential_1_3_Conversion }

  ADS1X15_Start_ADC_Reading(Handle, ADS1X15_REGISTER_CONFIG_MUX_DIFF_1_3, False);

  // Wait for the conversion to complete
  while not ADS1X15_Conversion_Complete(Handle) do
  begin
  end;

  // Read the conversion results
  Result := ADS1X15_Get_Last_Conversion_Result(Handle);
end; { PADS1X15.Read_ADC_Differential_1_3_Conversion }

{==============================================================================}
function ADS1X15_Read_ADC_Differential_2_3_Conversion(Handle: TThreadHandle): integer;
{
@brief  Reads the conversion results, measuring the voltage
        difference between the P (AIN2) and N (AIN3) input.  Generates
        a signed value since the difference can be either
        positive or negative.

@return the ADC reading
}


begin { PADS1X15.Read_ADC_Differential_2_3_Conversion }

  ADS1X15_Start_ADC_Reading(Handle, ADS1X15_REGISTER_CONFIG_MUX_DIFF_2_3, False);

  // Wait for the conversion to complete
  while not ADS1X15_Conversion_Complete(Handle) do
  begin
  end;

  // Read the conversion results
  Result := ADS1X15_Get_Last_Conversion_Result(Handle);
end; { PADS1X15.Read_ADC_Differential_1_3_Conversion }

{==============================================================================}

function ADS1X15_GPIODestroy(GPIO: PGPIODevice): longword;
  {Stop, deregister and destroy an ADS1X15_ GPIO device created by this driver}
  {GPIO: The GPIO device to destroy}
  {Return: ERROR_SUCCESS if completed or another error code on failure}
begin

  Result := ERROR_INVALID_PARAMETER;

  {Check GPIO}
  if GPIO = nil then Exit;

 {$IF DEFINED(ADS1X15_DEBUG) or DEFINED(GPIO_DEBUG)}
  if GPIO_LOG_ENABLED then GPIOLogDebug(GPIO, 'ADS1X15: GPIO Destroy');
 {$ENDIF}

  {Stop GPIO}
  Result := GPIODeviceStop(GPIO);
  if Result = ERROR_SUCCESS then
  begin
    {Deregister GPIO}
    Result := GPIODeviceDeregister(GPIO);
    if Result = ERROR_SUCCESS then
    begin
      {Destroy GPIO}
      Result := GPIODeviceDestroy(GPIO);
      if Result <> ERROR_SUCCESS then
      begin
        if GPIO_LOG_ENABLED then
          GPIOLogError(nil, 'ADS1X15: Failed to destroy GPIO device: ' +
            ErrorToString(Result));
      end;
    end
    else
    begin
      if GPIO_LOG_ENABLED then
        GPIOLogError(nil, 'ADS1X15: Failed to deregister GPIO device: ' +
          ErrorToString(Result));
    end;
  end
  else
  begin
    if GPIO_LOG_ENABLED then
      GPIOLogError(nil, 'ADS1X15: Failed to stop GPIO device: ' +
        ErrorToString(Result));
  end;
end;

{==============================================================================}

initialization
  ADS1X15_Init;

  {==============================================================================}

  {finalization}
  {Nothing}

end.