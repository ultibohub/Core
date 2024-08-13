{ ########################################################################################### }
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
{ ########################################################################################### }


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
  { ####################################################################################### }
  { ## I2C general data                                                                  ## }
  { ####################################################################################### }
  
  {ADS1X15 specific constants}
  ADS1X15_FRAMEBUFFER_DESCRIPTION = 'ADS1X15 ADC DRIVER';
  {Description of ADS1X15 device}
  ADS1X15_SIGNATURE = $000AF163;

  ADS1015 = False;
  ADS1115 = True;
  ADS1X15_ADDRESS = $48;            // I2C adress
  ADS1X15_I2C_SPEED = 100000;
  ADS1X15_I2C_BUFFER_MAX_SIZE = 32;             // I2C maximal buffer size
  ADS1X15_DEFAULT_CHANNEL = $4000;
  ADS1X15_DEFAULT_THRESHOLD = $0000;
  ADS1115_M_BitShift = 0;
  ADS1015_M_BitShift = 4;
  { ####################################################################################### }
  { ## Pointer register                                                                  ## }
  { ####################################################################################### }
  ADS1X15_REGISTER_POINTER_MASK = $03;
  ADS1X15_REGISTER_POINTER_CONVERT = $00;
  ADS1X15_REGISTER_POINTER_CONFIG = $01;
  ADS1X15_REGISTER_POINTER_LOW_THRESHOLD = $02;
  ADS1X15_REGISTER_POINTER_HIGH_THRESHOLD = $03;

  { ####################################################################################### }
  { ## Config register                                                                   ## }
  { ####################################################################################### }
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

type
  
  T_I2C_Buffer_A = packed array [0 .. ADS1X15_I2C_BUFFER_MAX_SIZE + 1] of uint8_t;
  Mux_By_Channel_Array = packed array [0 .. 3] of uint16_t;
  Data_Rates_Array = packed array [0 .. 7] of uint16_t;

  { ####################################################################################### }
  { ## PADS1X15                                                                         ## }
  { ####################################################################################### }
  PADS1X15 = class(TObject)
  protected
    M_I2C_Address: uint8_t;
    M_Gain: uint16_t;
    M_Data_Rate: uint16_t;
    M_I2C_Handle: CInt;
    M_BitShift: uint8_t;

  public
    constructor Create(I2C_Adress: uint8_t = ADS1X15_ADDRESS;
      Gain: uint16_t = ADS1X15_REGISTER_CONFIG_PGA_4_096V;
      Is_ADS1015: boolean = False; DATA_RATE: uint8_t = ADS1X15_DEFAULT_DATA_RATE);
    destructor Destroy; override;

    function I2C_Write_Register(I2C_Register: uint8_t; Value: uint16_t;
      I2C_Adress: uint8_t = ADS1X15_ADDRESS): integer;
    function I2C_Read_Register(I2C_Register: uint8_t;
      I2C_Adress: uint8_t = ADS1X15_ADDRESS): integer;

    procedure Start_ADC_Reading(Mux: uint16_t = ADS1X15_REGISTER_CONFIG_MUX_SINGLE_0;
      Continuous: boolean = False; RDY_Mode_Enabled: boolean = False);
    procedure Start_ALERT_RDY();
    procedure Stop_ALERT_RDY();

    function Read_ADC_Single_Ended(Channel: uint8_t = ADS1X15_DEFAULT_CHANNEL;
      Continuous: boolean = False; RDY_Mode_Enabled: boolean = False): integer;
    function Read_ADC_Differential_0_1_Conversion(): integer;
    function Read_ADC_Differential_0_3_Conversion(): integer;
    function Read_ADC_Differential_1_3_Conversion(): integer;
    function Read_ADC_Differential_2_3_Conversion(): integer;

    procedure Start_Comparator_Single_Ended(Channel: uint8_t; Threshold: uint16_t);
    function Get_Last_Conversion_Result(): integer;
    function Conversion_Complete(): boolean;
    function Compute_Volts(Counts: int16_t): double;
  published
  end;
{ PADS1X15 }

procedure ADS1X15_Init;

implementation

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
  //ADS1X15_ThreadI2CHandle: TThreadHandle = INVALID_HANDLE_VALUE;
  ADS1X15_I2CDevice: PI2CDevice;
  ADS1X15_Count: longword;
  ADS1X15_Debug: boolean = False;
  ADS1X15Initialized: boolean = True;

{==============================================================================}
{Initialization Functions}
procedure ADS1X15_Init;
{Initialize the ADS1X15 unit and create, register and start the device}
{Note: Called only during system startup}
begin
  {Check Initialized}
  if ADS1X15Initialized then Exit;
  ADS1X15Initialized := True;
end;
{==============================================================================}
{ --------------------------------------------------------------------------------------- }
constructor PADS1X15.Create(I2C_Adress: uint8_t = ADS1X15_ADDRESS;
  Gain: uint16_t = ADS1X15_REGISTER_CONFIG_PGA_4_096V; Is_ADS1015: boolean = False;
  DATA_RATE: uint8_t = ADS1X15_DEFAULT_DATA_RATE);
  { Initialization of the object                                                            }
  { --------------------------------------------------------------------------------------- }
begin { PADS1X15.Create }
  inherited Create();

  { Initialize data }
  M_I2C_Address := I2C_Adress;
  M_Gain := Gain;
  if DATA_RATE > 7 then DATA_RATE := ADS1X15_DEFAULT_DATA_RATE; // 0-->7

  if Is_ADS1015 = ADS1015 then
  begin

    M_Data_Rate := ADS1015_DATA_RATES[DATA_RATE];
    M_BitShift := ADS1015_M_BitShift;
  end
  else
  begin
    M_Data_Rate := ADS1115_DATA_RATES[DATA_RATE];
    M_BitShift := ADS1115_M_BitShift;
  end;

  {$IFDEF RPI}
  BCM2708I2C_COMBINED_WRITEREAD := True;
  {$ENDIF}
  {$ifdef ZERO}
  BCM2708I2C_COMBINED_WRITEREAD := True;
  {$ENDIF} 
  {$IFDEF RPI2}
  BCM2709I2C_COMBINED_WRITEREAD := True;
  {$ENDIF}
  {$IFDEF RPI3}
  BCM2710I2C_COMBINED_WRITEREAD := True;
  {$ENDIF}
  {$IFDEF ZERO2W}
  BCM2710I2C_COMBINED_WRITEREAD := True;    
  {$ENDIF}
  {$IFDEF RPI4}
  BCM2711I2C_COMBINED_WRITEREAD := True;
  {$ENDIF}

  {Check I2C Device}
  ADS1X15_I2CDevice := PI2CDevice(I2CDeviceFindByDescription(I2CGetDescription(1)));
  if ADS1X15_I2CDevice = nil then Exit;

  { Connect as I2C slave }
  if I2CDeviceStart(ADS1X15_I2CDevice, ADS1X15_I2C_SPEED) <> ERROR_SUCCESS then
  begin
    //Error Occurred
    Writeln('Can not open I2C bus, Error!!');
  end
  else
  begin
    Writeln('I2C bus is started successfully !!');
  end;
 sleep(100);

end; { PADS1X15.Create }


{ --------------------------------------------------------------------------------------- }
destructor PADS1X15.Destroy();
  { Free data                                                                               }
  { --------------------------------------------------------------------------------------- }
begin { PADS1X15.Destroy }
  I2CDeviceStop(ADS1X15_I2CDevice);
  inherited Destroy;
end; { PADS1X15.Destroy }


{ --------------------------------------------------------------------------------------- }
function PADS1X15.I2C_Write_Register(I2C_Register: uint8_t; Value: uint16_t;
  I2C_Adress: uint8_t = ADS1X15_ADDRESS): integer; inline;
  { Return last conversion result                                                           }
  { --------------------------------------------------------------------------------------- }
var
  I2C_Buffer: packed array [0 .. 9] of uint8_t;
begin { PADS1X15.I2C_Write_Register }

  I2C_Buffer[0] := Value shr 8;
  I2C_Buffer[1] := Value and $FF;

  if I2CDeviceWriteWrite(ADS1X15_I2CDevice, I2C_Adress, @I2C_Register,
    SizeOf(byte), @I2C_Buffer, 2, ADS1X15_Count) <> ERROR_SUCCESS then
  begin
    Result := -1;
    if ADS1X15_Debug then Writeln('I2C_Write_Register, error!!');
  end;

end; { PADS1X15.I2C_Write_Register }

{ --------------------------------------------------------------------------------------- }
function PADS1X15.I2C_Read_Register(I2C_Register: uint8_t;
  I2C_Adress: uint8_t = ADS1X15_ADDRESS): integer; inline;
  { Return last conversion result                                                           }
  { --------------------------------------------------------------------------------------- }
var
  I2C_Buffer: T_I2C_Buffer_A;

begin { PADS1X15.I2C_Read_Buffer }

  I2C_Buffer[0] := I2C_Register;

  if I2CDeviceWriteRead(ADS1X15_I2CDevice, I2C_Adress, @I2C_Register,
    SizeOf(byte), @I2C_Buffer, 2, ADS1X15_Count) <> ERROR_SUCCESS then
  begin 
    Result := -1;
    if ADS1X15_Debug then  Writeln('I2C_Buffer, Error!!');
  end
  else
  begin
    // if Debug then  Writeln('I2C_Read_Buffer, Success!!');
    Result := (I2C_Buffer[0] shl 8) or I2C_Buffer[1];   
  end;

end; { PADS1X15.I2C_Read_Buffer }


{ --------------------------------------------------------------------------------------- }
procedure PADS1X15.Start_ALERT_RDY();
{ Start ALERT/RDY interupt signal                                                         }
{ --------------------------------------------------------------------------------------- }
var
  Result: integer;

begin { PADS1X15.Start_ALERT_RDY }

  { Turn on ALERT/RDY : Set MSB of Hi_thresh to 1 }
  Result := I2C_Write_Register(ADS1X15_REGISTER_POINTER_HIGH_THRESHOLD, $8000);
  if Result < 0 then
  begin  
    if ADS1X15_Debug then Writeln('Error writing to I2C (Set MSB of Hi_thresh to 1)');
   Exit;
  end; 

  { Turn on ALERT/RDY : Set MSB of Lo_thresh to 0 }
  Result := I2C_Write_Register(ADS1X15_REGISTER_POINTER_LOW_THRESHOLD, $0000);
  if Result < 0 then
  begin  
    if ADS1X15_Debug then Writeln('Error writing to I2C (Set MSB of Lo_thresh to 0)');
    Exit;
  end;  

end; { PADS1X15.Start_ALERT_RDY }

{ --------------------------------------------------------------------------------------- }
procedure PADS1X15.Stop_ALERT_RDY();
{ Stop ALERT/RDY interupt signal                                                          }
{ --------------------------------------------------------------------------------------- }
var
  Result: integer;
begin { PADS1X15.Stop_ALERT_RDY }
  
  { Turn off ALERT/RDY : Set MSB of Hi_thresh to 0 }
  Result := I2C_Write_Register(ADS1X15_REGISTER_POINTER_HIGH_THRESHOLD, $0000);
  if Result < 0 then
  begin  
    if ADS1X15_Debug then Writeln('Error writing to I2C (Set MSB of Hi_thresh to 0)');
    Exit;
  end;  

  { Turn off ALERT/RDY : Set MSB of Lo_thresh to 1 }
  Result := I2C_Write_Register(ADS1X15_REGISTER_POINTER_LOW_THRESHOLD, $FFFF);
  if Result < 0 then
  begin  
    if ADS1X15_Debug then Writeln('Error writing to I2C (Set MSB of Lo_thresh to 1)');
    Exit;
  end;

end; { PADS1X15.Stop_ALERT_RDY }

{ --------------------------------------------------------------------------------------- }
procedure PADS1X15.Start_Comparator_Single_Ended(Channel: uint8_t; Threshold: uint16_t);
{
      @brief  Sets up the comparator to operate in basic mode, causing the
            ALERT/RDY pin to assert (go from high to low) when the ADC
            value exceeds the specified threshold.

            This will also set the ADC in continuous conversion mode.

    @param channel ADC channel to use
    @param threshold comparator threshold
}
{ --------------------------------------------------------------------------------------- }
var
  Config: uint16_t;

begin { PADS1X15.Start_Comparator_Single_Ended }

  // Start with default values
  Config := ADS1X15_REGISTER_CONFIG_CQUE_1CONV or
    ADS1X15_REGISTER_CONFIG_CLAT_NONLATCH or ADS1X15_REGISTER_CONFIG_CPOL_ACTVLOW or
    ADS1X15_REGISTER_CONFIG_CMODE_TRADITIONAL or
    ADS1X15_REGISTER_CONFIG_MODE_CONTINUOUS;


  // Set PGA/voltage range
  Config := Config or M_Gain;

  // Set data rate
  Config := Config or M_Data_Rate;


  // Set channels
  Config := Config or MUX_BY_CHANNEL[Channel];

  // Set 'start single-conversion' bit
  Config := Config or ADS1X15_REGISTER_CONFIG_OS_SINGLE;

  // Set the high threshold register
  // Shift 12-bit results left 4 bits for the ADS1015
  I2C_Write_Register(ADS1X15_REGISTER_POINTER_HIGH_THRESHOLD, Threshold shl M_BitShift);

  // Write config register to the ADC
  I2C_Write_Register(ADS1X15_REGISTER_POINTER_CONFIG, Config);

end; { PADS1X15.Start_Comparator_Single_Ended }


{ --------------------------------------------------------------------------------------- }
function PADS1X15.Get_Last_Conversion_Result(): integer;
{
     @brief  In order to clear the comparator, we need to read the
            conversion results.  This function reads the last conversion
            results without changing the config value.

    @return the last ADC reading
} 
begin { PADS1X15.Get_Last_Conversion_Result }

  Result := I2C_Read_Register(ADS1X15_REGISTER_POINTER_CONVERT); 
  if Result < 0 then
  begin  
    Exit;
  end;  

  Result := Result shr M_BitShift;

  if M_BitShift <> 0 then
  begin
    if ADS1X15_Debug then Writeln('BitShift <> 0' + IntToStr(Result));
    if Result > $07FF then
    begin
      // negative number - extend the sign to 16th bit
      Result := Result or $F000;
    end;
  end;
  Result := int16_t(Result);

end; { PADS1X15.Get_Last_Conversion_Result }

{ --------------------------------------------------------------------------------------- }
function PADS1X15.Conversion_Complete(): boolean;
{
   @brief  Returns true if conversion is complete, false otherwise.

   @return True if conversion is complete, false otherwise.
} 
var
Result_Value: integer;
begin
  Result_Value := I2C_Read_Register(ADS1X15_REGISTER_POINTER_CONFIG);
  if Result_Value < 0 then
  begin
    Result := False;
    Exit;
  end;
  Result := (Result_Value and $8000) <> 0;
end;  { PADS1X15.Conversion_Complete }

{ --------------------------------------------------------------------------------------- }
function PADS1X15.Compute_Volts(Counts: int16_t): double;
  {
   @brief  Compute volts for the given raw counts.

    @param counts the ADC reading in raw counts

    @return the ADC reading in volts                                                        }
  { --------------------------------------------------------------------------------------- }
var
  Fs_Range: double;
begin { PADS1X15.Compute_Volts }

  if M_Gain = ADS1X15_REGISTER_CONFIG_PGA_6_144V then
  begin
    Fs_Range := 6.144;
  end
  else if M_Gain = ADS1X15_REGISTER_CONFIG_PGA_4_096V then
  begin
    Fs_Range := 4.096;
  end
  else if M_Gain = ADS1X15_REGISTER_CONFIG_PGA_2_048V then
  begin
    Fs_Range := 2.048;
  end
  else if M_Gain = ADS1X15_REGISTER_CONFIG_PGA_1_024V then
  begin
    Fs_Range := 1.024;
  end
  else if M_Gain = ADS1X15_REGISTER_CONFIG_PGA_0_512V then
  begin
    Fs_Range := 0.512;
  end
  else if M_Gain = ADS1X15_REGISTER_CONFIG_PGA_0_256V then
  begin
    Fs_Range := 0.256;
  end;

  Result := double(Counts * (Fs_Range / (32768 shr M_BitShift)));
  //(32768 shr M_BitShift)));

end; { PADS1X15.Compute_Volts }


{ --------------------------------------------------------------------------------------- }
procedure PADS1X15.Start_ADC_Reading(Mux: uint16_t; Continuous: boolean = False; RDY_Mode_Enabled: boolean = False);
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
{ --------------------------------------------------------------------------------------- }
var
  Config: uint16_t;

begin { PADS1X15.Start_ADC_Reading }

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
  Config := Config or M_Gain;

  // Set data rate
  Config := Config or M_Data_Rate;


  // Set channels
  Config := Config or Mux;

  // Set 'start single-conversion' bit
  Config := Config or ADS1X15_REGISTER_CONFIG_OS_SINGLE;

  // Write config register to the ADC
  I2C_Write_Register(ADS1X15_REGISTER_POINTER_CONFIG, Config);

  // Set ALERT/RDY to RDY mode. 
  if RDY_Mode_Enabled then
  begin
    I2C_Write_Register(ADS1X15_REGISTER_POINTER_HIGH_THRESHOLD, Config);
    I2C_Write_Register(ADS1X15_REGISTER_POINTER_LOW_THRESHOLD, Config);
  end;

end; { PADS1X15.Start_ADC_Reading }


{ --------------------------------------------------------------------------------------- }
function PADS1X15.Read_ADC_Single_Ended(Channel: uint8_t = ADS1X15_DEFAULT_CHANNEL;
  Continuous: boolean = False; RDY_Mode_Enabled: boolean = False): integer;
  { --------------------------------------------------------------------------------------- }

begin { PADS1X15.Read_ADC_Single_Ended }

  if Channel > 3 then
  begin
    Exit(0);
  end;

  Start_ADC_Reading(MUX_BY_CHANNEL[channel], Continuous, RDY_Mode_Enabled);

  if not Continuous then
  begin
    // Wait for conversion to complete
    // ADS1x1x devices settle within a single conversion cycle
    // Continuously poll conversion complete status bit
    // Wait for the conversion to complete
    while not Conversion_Complete() do
    begin
    end;
  end
  else
  begin
    // Can't poll registers in CONTINUOUS mode
    // Wait expected time for two conversions to complete
    sleep(2 div M_Data_Rate);
  end;

  // Read the conversion results
  Result := Get_Last_Conversion_Result();

end; { PADS1X15.Read_ADC_Single_Ended }

{ --------------------------------------------------------------------------------------- }
function PADS1X15.Read_ADC_Differential_0_1_Conversion(): integer;
{
  @brief  Reads the conversion results, measuring the voltage
          difference between the P (AIN0) and N (AIN1) input.  Generates
          a signed value since the difference can be either
          positive or negative.

  @return the ADC reading
 }
  { --------------------------------------------------------------------------------------- }

begin { PADS1X15.Start_ADC_Differential_0_1_Conversion }


  Start_ADC_Reading(ADS1X15_REGISTER_CONFIG_MUX_DIFF_0_1, False);

  // Wait for the conversion to complete
  while not Conversion_Complete() do
  begin
  end;

  // Read the conversion results
  Result := Get_Last_Conversion_Result();

end; { PADS1X15.Start_ADC_Differential_0_1_Conversion }

{ --------------------------------------------------------------------------------------- }
function PADS1X15.Read_ADC_Differential_0_3_Conversion(): integer;
{
  @brief  Reads the conversion results, measuring the voltage
          difference between the P (AIN0) and N (AIN3) input.  Generates
          a signed value since the difference can be either
          positive or negative.
  @return the ADC reading
}
  { --------------------------------------------------------------------------------------- }

begin { PADS1X15.Read_ADC_Differential_0_3_Conversion }

  Start_ADC_Reading(ADS1X15_REGISTER_CONFIG_MUX_DIFF_0_3, False);

  // Wait for the conversion to complete
  while not Conversion_Complete() do
  begin
  end;

  // Read the conversion results
  Result := Get_Last_Conversion_Result();
end; { PADS1X15.Read_ADC_Differential_0_3_Conversion }

{ --------------------------------------------------------------------------------------- }
function PADS1X15.Read_ADC_Differential_1_3_Conversion(): integer;
{
     @brief  Reads the conversion results, measuring the voltage
            difference between the P (AIN1) and N (AIN3) input.  Generates
            a signed value since the difference can be either
            positive or negative.
    @return the ADC reading
}
  { --------------------------------------------------------------------------------------- }

begin { PADS1X15.Read_ADC_Differential_1_3_Conversion }

  Start_ADC_Reading(ADS1X15_REGISTER_CONFIG_MUX_DIFF_1_3, False);

  // Wait for the conversion to complete
  while not Conversion_Complete() do
  begin
  end;

  // Read the conversion results
  Result := Get_Last_Conversion_Result();
end; { PADS1X15.Read_ADC_Differential_1_3_Conversion }

{ --------------------------------------------------------------------------------------- }
function PADS1X15.Read_ADC_Differential_2_3_Conversion(): integer;
{
@brief  Reads the conversion results, measuring the voltage
        difference between the P (AIN2) and N (AIN3) input.  Generates
        a signed value since the difference can be either
        positive or negative.

@return the ADC reading
}
  { --------------------------------------------------------------------------------------- }

begin { PADS1X15.Read_ADC_Differential_2_3_Conversion }

  Start_ADC_Reading(ADS1X15_REGISTER_CONFIG_MUX_DIFF_2_3, False);

  // Wait for the conversion to complete
  while not Conversion_Complete() do
  begin
  end;

  // Read the conversion results
  Result := Get_Last_Conversion_Result();
end; { PADS1X15.Read_ADC_Differential_1_3_Conversion }

{==============================================================================}

initialization
  ADS1X15_Init;

  {==============================================================================}

  {finalization}
  {Nothing}

end.