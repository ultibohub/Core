{
Maxim DS1307 Real Time Clock Driver.

Copyright (C) 2016 - SoftOz Pty Ltd.

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

  Linux - \drivers\rtc\rtc-ds1307.c - Copyright (C) 2012 Bertrand Achard and others
  
References
==========

  http://datasheets.maximintegrated.com/en/ds/DS1307.pdf
  http://www.bristolwatch.com/pdf/ds1307.pdf
  
  http://www.dfrobot.com/wiki/index.php?title=Raspberry_Pi_RTC_Module_SKU:_DFR0386
  
Maxim DS1307
============

 The Maxim DS1307 is an I2C interface, battery backed real time clock chip with full BCD clock and 
 calendar and 56 bytes of software accessable NVRAM. The chip supports 12 or 24 hour format and is
 widely used in low cost RTC devices available from a range of suppliers.
 
 See the datasheets above for more information.
 
 This driver extends the real time clock API to include functions for directly reading and
 writing the NVRAM which could be used for storing device specific information and configuration
 or for non volatile storage of keys, passwords or tokens.
 
}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit DS1307; 

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,RTC,I2C,SysUtils;

{==============================================================================}
{Global definitions}
{$INCLUDE ..\core\GlobalDefines.inc}

{==============================================================================}
const
 {DS1307 specific constants}
 DS1307_RTC_DESCRIPTION = 'Maxim DS1307 Real Time Clock'; {Description of DS1307 device}

 DS1307_MIN_TIME = 125911584000000000; {Time starts at 01/01/2000 00:00:00 (MM/DD/YY HH:MM:SS)}
 DS1307_MAX_TIME = 157469183990000000; {Time ends at 12/31/2099 23:59:59 (MM/DD/YY HH:MM:SS)}
 
 {DS1307 chip constants}
 DS1307_CHIP_DS1307   = 0;
 DS1307_CHIP_DS1337   = 1;
 DS1307_CHIP_DS1338   = 2;
 DS1307_CHIP_DS1339   = 3;
 DS1307_CHIP_DS1340   = 4;
 DS1307_CHIP_DS1388   = 5;
 DS1307_CHIP_DS3231   = 6;
 DS1307_CHIP_M41T00   = 7;
 DS1307_CHIP_MCP794XX = 8;
 DS1307_CHIP_RX8025   = 9;
 
 {DS1307 RTC registers}
 DS1307_REG_SECONDS = $00; {00-59}
 DS1307_REG_MINUTE  = $01; {00-59}
 DS1307_REG_HOUR    = $02; {00-23 (or 1-12am,pm)}
 DS1307_REG_WEEKDAY = $03; {01-07}
 DS1307_REG_DAY     = $04; {01-31}
 DS1307_REG_MONTH   = $05; {01-12}
 DS1307_REG_YEAR    = $06; {00-99}
 
 {DS1307 Control and Sttaus registers}
 DS1307_REG_CONTROL        = $07; {Also DS1338}
 DS1337_REG_CONTROL        = $0e;
 DS1340_REG_CONTROL        = $07;
 DS1340_REG_FLAG           = $09;
 DS1337_REG_STATUS         = $0f;
 DS1339_REG_ALARM1_SECONDS = $07;
 RX8025_REG_CONTROL1       = $0e;
 RX8025_REG_CONTROL2       = $0f;
 
 {DS1307 Seconds register bits}
 DS1307_REG_SECONDS_CH    = $80; {Clock Halt bit (1 to Disable / 0 to Enable the clock)}
 DS1340_REG_SECONDS_nEOSC = $80; {}
 MCP794XX_REG_SECONDS_ST  = $80; {Clock Stop Bit (1 to Enable / 0 to Disable the clock)}
 
 {DS1307 Hour register bits}
 DS1307_REG_HOUR_12HR       = $40; {12/24 hour bit (1 = 12 hour mode / 0 = 24 hour mode)}
 DS1307_REG_HOUR_PM         = $20; {AM/PM bit (1 = PM / 0 = AM) (Only in 12 hour mode)}
 DS1340_REG_HOUR_CENTURY_EN = $80; {}
 DS1340_REG_HOUR_CENTURY    = $40; {}
 
 {MCP794XX Weekday register bits}
 MCP794XX_REG_WEEKDAY_VBATEN = $08; {Battery Backup Enabled (1 to Enable / 0 to Disable)}
 
 {DS1337 Month register bits}
 DS1337_REG_MONTH_CENTURY = $80; {}
 
 {DS1307 Control register bits}
 DS1307_REG_CONTROL_OUT  = $80; {Output Control (Output level of SQW/OUT pin when SQWE = 0)}
 DS1338_REG_CONTROL_OSF  = $20; {}
 DS1307_REG_CONTROL_SQWE = $10; {Square-Wave Enable (When set to 1, enables the oscillator output)}
 DS1307_REG_CONTROL_RS1  = $02; {Rate Select 1 (See datasheet for values)}
 DS1307_REG_CONTROL_RS0  = $01; {Rate Select 2 (See datasheet for values)}
 
 {DS1337 Control register bits}
 DS1337_REG_CONTROL_nEOSC = $80; {}
 DS1339_REG_CONTROL_BBSQI = $20; {}
 DS3231_REG_CONTROL_BBSQW = $40; {same as BBSQI}
 DS1337_REG_CONTROL_RS2   = $10; {}
 DS1337_REG_CONTROL_RS1   = $08; {}
 DS1337_REG_CONTROL_INTCN = $04; {}
 DS1337_REG_CONTROL_A2IE  = $02; {}
 DS1337_REG_CONTROL_A1IE  = $01; {}
 
 {DS1340 Control register bits}
 DS1340_REG_CONTROL_OUT           = $80; {}
 DS1340_REG_CONTROL_FT            = $40; {}
 DS1340_REG_CONTROL_CALIB_SIGN    = $20; {}
 DS1340_REG_CONTROL_M_CALIBRATION = $1f; {}

 {DS1340 Flag register bits}
 DS1340_REG_FLAG_OSF = $80; {}
 
 {DS1337 Status register bits}
 DS1337_REG_STATUS_OSF = $80; {}
 DS1337_REG_STATUS_A2I = $02; {}
 DS1337_REG_STATUS_A1I = $01; {}
 
 {RX8025 Control1 register bits}
 RX8025_REG_CONTROL1_2412 = $20; {}
 
 {RX8025 Control2 register bits}
 RX8025_REG_CONTROL2_PON  = $10; {}
 RX8025_REG_CONTROL2_VDET = $40; {}
 RX8025_REG_CONTROL2_XST  = $20; {}
 
{==============================================================================}
type
 {DS1307 specific types}
 PDS1307RTCTime = ^TDS1307RTCTime;
 TDS1307RTCTime = array[0..6] of Byte;
 
 PDS1307RTCDevice = ^TDS1307RTCDevice;
 TDS1307RTCDevice = record
  {RTC Properties}
  RTC:TRTCDevice;
  {DS1307 Properties}
  I2C:PI2CDevice;          {The I2C device this RTC is connected to}
  Address:Word;            {The I2C address of the device}
  Chip:LongWord;           {The chip type (eg DS1307_CHIP_DS1307)}
  RTCReg:Byte;             {The register offset for the RTC}
  NVRAMReg:Byte;           {The register offset for the NVRAM}
  NVRAMSize:Byte;          {The NVRAM size}
  ControlReg:Byte;         {The control register offset}
  {Statistics Properties}
  ReadCount:LongWord;      {Number of direct RTC/NVRAM/Control reads to the device}
  WriteCount:LongWord;     {Number of direct RTC/NVRAM/Control writes to the device}
 end;

{==============================================================================}
{var}
 {DS1307 specific variables}
 
{==============================================================================}
{Initialization Functions}
procedure DS1307Init;

{==============================================================================}
{DS1307 RTC Functions}
function DS1307RTCStart(RTC:PRTCDevice):LongWord;
function DS1307RTCStop(RTC:PRTCDevice):LongWord;

function DS1307RTCGetTime(RTC:PRTCDevice):Int64;
function DS1307RTCSetTime(RTC:PRTCDevice;const Time:Int64):Int64;

{==============================================================================}
{DS1307 Helper Functions}
function DS1307ReadRTC(RTC:PRTCDevice;Buffer:Pointer;Size:LongWord):LongWord;
function DS1307WriteRTC(RTC:PRTCDevice;Buffer:Pointer;Size:LongWord):LongWord;

function DS1307ReadNVRAM(RTC:PRTCDevice;Offset:Byte;Buffer:Pointer;Size:LongWord):LongWord;
function DS1307WriteNVRAM(RTC:PRTCDevice;Offset:Byte;Buffer:Pointer;Size:LongWord):LongWord;

function DS1307ReadControl(RTC:PRTCDevice;var Control:Byte):LongWord;
function DS1307WriteControl(RTC:PRTCDevice;Control:Byte):LongWord;

function DS1307DefaultRTCTime(RTC:PRTCDevice;Buffer:Pointer;Size:LongWord):Boolean;
function DS1307FileTimeToRTCTime(RTC:PRTCDevice;const FileTime:Int64;Buffer:Pointer;Size:LongWord):Boolean;
function DS1307RTCTimeToFileTime(RTC:PRTCDevice;Buffer:Pointer;Size:LongWord;var FileTime:Int64):Boolean;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {DS1307 specific variables}
 DS1307Initialized:Boolean; 
 
{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure DS1307Init;
var
 Status:LongWord;
 WorkInt:LongWord;
 WorkBuffer:String;

 DS1307RTC:PDS1307RTCDevice;
begin
 {}
 {Check Initialized}
 if DS1307Initialized then Exit;

 {Check Environment Variables}
 {DS1307_CHIP_TYPE}
 WorkInt:=StrToIntDef(SysUtils.GetEnvironmentVariable('DS1307_CHIP_TYPE'),0);
 if WorkInt > 0 then DS1307_CHIP_TYPE:=WorkInt;
 
 {DS1307_I2C_ADDRESS}
 WorkInt:=StrToIntDef(SysUtils.GetEnvironmentVariable('DS1307_I2C_ADDRESS'),0);
 if WorkInt > 0 then DS1307_I2C_ADDRESS:=WorkInt;

 {DS1307_I2C_DEVICE}
 WorkBuffer:=SysUtils.GetEnvironmentVariable('DS1307_I2C_DEVICE');
 if Length(WorkBuffer) <> 0 then DS1307_I2C_DEVICE:=WorkBuffer;
 
 {Create RTC}
 DS1307RTC:=PDS1307RTCDevice(RTCDeviceCreateEx(SizeOf(TDS1307RTCDevice)));
 if DS1307RTC <> nil then
  begin
   {Update RTC}
   {Device}
   DS1307RTC.RTC.Device.DeviceBus:=DEVICE_BUS_I2C;
   DS1307RTC.RTC.Device.DeviceType:=RTC_TYPE_NONE;
   DS1307RTC.RTC.Device.DeviceFlags:=RTC_FLAG_NONE;
   DS1307RTC.RTC.Device.DeviceData:=nil;
   DS1307RTC.RTC.Device.DeviceDescription:=DS1307_RTC_DESCRIPTION;
   {RTC}
   DS1307RTC.RTC.RTCState:=RTC_STATE_DISABLED;
   DS1307RTC.RTC.DeviceStart:=DS1307RTCStart;
   DS1307RTC.RTC.DeviceStop:=DS1307RTCStop;
   DS1307RTC.RTC.DeviceGetTime:=DS1307RTCGetTime;
   DS1307RTC.RTC.DeviceSeTTime:=DS1307RTCSetTime;
   {Driver}
   DS1307RTC.RTC.Properties.Flags:=DS1307RTC.RTC.Device.DeviceFlags;
   DS1307RTC.RTC.Properties.MinTime:=0;
   DS1307RTC.RTC.Properties.MaxTime:=0;
   DS1307RTC.RTC.Properties.AlarmCount:=0;
   {DS1307}
   DS1307RTC.Address:=DS1307_I2C_ADDRESS;
   DS1307RTC.Chip:=DS1307_CHIP_TYPE;
   
   {Register RTC}
   Status:=RTCDeviceRegister(@DS1307RTC.RTC);
   if Status = ERROR_SUCCESS then
    begin
     {Start RTC}
     Status:=RTCDeviceStart(@DS1307RTC.RTC);
     if Status <> ERROR_SUCCESS then
      begin
       if RTC_LOG_ENABLED then RTCLogError(nil,'DS1307: Failed to start new RTC device: ' + ErrorToString(Status));
      end;
    end
   else
    begin
     if RTC_LOG_ENABLED then RTCLogError(nil,'DS1307: Failed to register new RTC device: ' + ErrorToString(Status));
    end;
  end
 else 
  begin
   if RTC_LOG_ENABLED then RTCLogError(nil,'DS1307: Failed to create new RTC device');
  end; 
 
 DS1307Initialized:=True;
end;

{==============================================================================}
{==============================================================================}
{DS1307 RTC Functions}
function DS1307RTCStart(RTC:PRTCDevice):LongWord;
var
 Control:Byte;
 Data:TDS1307RTCTime;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check RTC}
 if RTC = nil then Exit;
 
 {$IF DEFINED(DS1307_DEBUG) or DEFINED(RTC_DEBUG)}
 if RTC_LOG_ENABLED then RTCLogDebug(RTC,'DS1307: RTC Start');
 {$ENDIF}
 
 {Check I2C Address}
 if PDS1307RTCDevice(RTC).Address = I2C_ADDRESS_INVALID then Exit;
 
 {Check I2C Device}
 PDS1307RTCDevice(RTC).I2C:=PI2CDevice(DeviceFindByName(DS1307_I2C_DEVICE));
 if PDS1307RTCDevice(RTC).I2C = nil then
  begin
   PDS1307RTCDevice(RTC).I2C:=PI2CDevice(DeviceFindByDescription(DS1307_I2C_DEVICE));
   if PDS1307RTCDevice(RTC).I2C = nil then Exit;
  end;
  
 {Start I2C Device}
 if I2CDeviceStart(PDS1307RTCDevice(RTC).I2C,0) <> ERROR_SUCCESS then
  begin
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;
  
 {Reset Properties}
 FillChar(RTC.Properties,SizeOf(TRTCProperties),0);
 RTC.Properties.MinTime:=DS1307_MIN_TIME;
 RTC.Properties.MaxTime:=DS1307_MAX_TIME;
 
 {Check Chip}
 case PDS1307RTCDevice(RTC).Chip of
  DS1307_CHIP_DS1307:begin
    {Update Properties}
    PDS1307RTCDevice(RTC).RTCReg:=0;
    PDS1307RTCDevice(RTC).NVRAMReg:=8;
    PDS1307RTCDevice(RTC).NVRAMSize:=56;
    PDS1307RTCDevice(RTC).ControlReg:=7;
   end;
  DS1307_CHIP_DS1337:begin
    {Update Properties}
    RTC.Properties.AlarmCount:=2;
    
    PDS1307RTCDevice(RTC).RTCReg:=0;
    PDS1307RTCDevice(RTC).ControlReg:=14;
    
    RTC.Device.DeviceFlags:=RTC_FLAG_ALARM;
    RTC.Properties.Flags:=RTC.Device.DeviceFlags;
   end;
  DS1307_CHIP_DS1338:begin
    {Update Properties}
    PDS1307RTCDevice(RTC).RTCReg:=0;
    PDS1307RTCDevice(RTC).NVRAMReg:=8;
    PDS1307RTCDevice(RTC).NVRAMSize:=56;
    PDS1307RTCDevice(RTC).ControlReg:=7;
   end;
  DS1307_CHIP_DS1339:begin
    {Update Properties}
    RTC.Properties.AlarmCount:=2;
    
    PDS1307RTCDevice(RTC).RTCReg:=0;
    PDS1307RTCDevice(RTC).ControlReg:=14;
  
    RTC.Device.DeviceFlags:=RTC_FLAG_ALARM;
    RTC.Properties.Flags:=RTC.Device.DeviceFlags;
   end;
  DS1307_CHIP_DS1340:begin
    {Update Properties}
    PDS1307RTCDevice(RTC).RTCReg:=0;
    PDS1307RTCDevice(RTC).ControlReg:=7;
   end;
  DS1307_CHIP_DS1388:begin
    {Update Properties}
    PDS1307RTCDevice(RTC).RTCReg:=1; {Register 0 is Hundredths of Seconds}
    PDS1307RTCDevice(RTC).NVRAMReg:=256;
    PDS1307RTCDevice(RTC).NVRAMSize:=512;
    PDS1307RTCDevice(RTC).ControlReg:=12;
  
    RTC.Device.DeviceFlags:=RTC_FLAG_WATCHDOG;
    RTC.Properties.Flags:=RTC.Device.DeviceFlags;
   end;
  DS1307_CHIP_DS3231:begin
    {Update Properties}
    RTC.Properties.AlarmCount:=2;
    
    PDS1307RTCDevice(RTC).RTCReg:=0;
    PDS1307RTCDevice(RTC).ControlReg:=14;
  
    RTC.Device.DeviceFlags:=RTC_FLAG_ALARM;
    RTC.Properties.Flags:=RTC.Device.DeviceFlags;
   end;
  DS1307_CHIP_M41T00:begin
    {Update Properties}
    PDS1307RTCDevice(RTC).RTCReg:=0;
    PDS1307RTCDevice(RTC).ControlReg:=7;
   end;
  DS1307_CHIP_MCP794XX:begin
    {Update Properties}
    RTC.Properties.AlarmCount:=2;
    
    PDS1307RTCDevice(RTC).RTCReg:=0;
    PDS1307RTCDevice(RTC).NVRAMReg:=32;
    PDS1307RTCDevice(RTC).NVRAMSize:=64;
    PDS1307RTCDevice(RTC).ControlReg:=7;
  
    RTC.Device.DeviceFlags:=RTC_FLAG_ALARM;
    RTC.Properties.Flags:=RTC.Device.DeviceFlags;
   end;
  DS1307_CHIP_RX8025:begin
    {Update Properties}
    //To Do //Find a datasheet
    PDS1307RTCDevice(RTC).RTCReg:=0;
    PDS1307RTCDevice(RTC).ControlReg:=7;
   end;
 end;
  
 {Check the clock is running} 
 case PDS1307RTCDevice(RTC).Chip of
  DS1307_CHIP_DS1337,DS1307_CHIP_DS1339,DS1307_CHIP_DS3231:begin
    {These chips have the Oscillator enable in the control bits}
    {Read Control}
    DS1307ReadControl(RTC,Control);
    
    {$IF DEFINED(DS1307_DEBUG) or DEFINED(RTC_DEBUG)}
    if RTC_LOG_ENABLED then RTCLogDebug(RTC,'DS1307:  Control=' + IntToHex(Control,2));
    {$ENDIF}
    
    {Check Enabled}
    if (Control and DS1337_REG_CONTROL_nEOSC) <> 0 then
     begin
      {Enable Clock}
      Control:=Control and not(DS1337_REG_CONTROL_nEOSC);
      
      {Write Control}
      DS1307WriteControl(RTC,Control);
     end;
   end;
  DS1307_CHIP_RX8025:begin 
    //To Do //Find a datasheet
   end; 
 end;
 
 {Read RTC Data}
 if DS1307ReadRTC(RTC,@Data,SizeOf(TDS1307RTCTime)) <> ERROR_SUCCESS then
  begin
   Result:=ERROR_OPERATION_FAILED;
   Exit;
  end;
 
 {Check the clock is running}
 case PDS1307RTCDevice(RTC).Chip of
  DS1307_CHIP_DS1307,DS1307_CHIP_M41T00:begin
    {Check Halted}
    if (Data[DS1307_REG_SECONDS] and DS1307_REG_SECONDS_CH) <> 0 then
     begin
      {Get Defaults}
      DS1307DefaultRTCTime(RTC,@Data,SizeOf(TDS1307RTCTime));
      
      {Write RTC Data}
      DS1307WriteRTC(RTC,@Data,SizeOf(TDS1307RTCTime));
     end;
   end;
  DS1307_CHIP_DS1338:begin
    {Check Halted}
    if (Data[DS1307_REG_SECONDS] and DS1307_REG_SECONDS_CH) <> 0 then
     begin
      {Get Defaults}
      DS1307DefaultRTCTime(RTC,@Data,SizeOf(TDS1307RTCTime));
      
      {Write RTC Data}
      DS1307WriteRTC(RTC,@Data,SizeOf(TDS1307RTCTime));
     end;
   end;
  DS1307_CHIP_DS1340:begin
    {Check Halted}
    if (Data[DS1307_REG_SECONDS] and DS1340_REG_SECONDS_nEOSC) <> 0 then
     begin
      {Get Defaults}
      DS1307DefaultRTCTime(RTC,@Data,SizeOf(TDS1307RTCTime));
      
      {Write RTC Data}
      DS1307WriteRTC(RTC,@Data,SizeOf(TDS1307RTCTime));
     end;
   end;
  DS1307_CHIP_MCP794XX:begin
    {Check Battery Backup}
    if (Data[DS1307_REG_WEEKDAY] and MCP794XX_REG_WEEKDAY_VBATEN) = 0 then
     begin
      {Get Defaults}
      DS1307DefaultRTCTime(RTC,@Data,SizeOf(TDS1307RTCTime));
      
      {Write RTC Data}
      DS1307WriteRTC(RTC,@Data,SizeOf(TDS1307RTCTime));
     end; 
    
    {Check Stopped}
    if (Data[DS1307_REG_SECONDS] and MCP794XX_REG_SECONDS_ST) = 0 then
     begin
      {Get Defaults}
      DS1307DefaultRTCTime(RTC,@Data,SizeOf(TDS1307RTCTime));
      
      {Write RTC Data}
      DS1307WriteRTC(RTC,@Data,SizeOf(TDS1307RTCTime));
     end;
   end;
 end; 
 
 {Check for 24 hour mode}
 case PDS1307RTCDevice(RTC).Chip of
  DS1307_CHIP_DS1340,DS1307_CHIP_M41T00,DS1307_CHIP_RX8025:begin
    {Nothing}
   end;
  else
   begin
    if (Data[DS1307_REG_HOUR] and DS1307_REG_HOUR_12HR) <> 0 then
     begin
      {Get Defaults}
      DS1307DefaultRTCTime(RTC,@Data,SizeOf(TDS1307RTCTime));
      
      {Write RTC Data}
      DS1307WriteRTC(RTC,@Data,SizeOf(TDS1307RTCTime));
     end;
   end;   
 end;
 
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function DS1307RTCStop(RTC:PRTCDevice):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check RTC}
 if RTC = nil then Exit;
 
 {$IF DEFINED(DS1307_DEBUG) or DEFINED(RTC_DEBUG)}
 if RTC_LOG_ENABLED then RTCLogDebug(RTC,'DS1307: RTC Stop');
 {$ENDIF}
 
 {Nothing}
 
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function DS1307RTCGetTime(RTC:PRTCDevice):Int64;
var
 FileTime:Int64;
 Data:TDS1307RTCTime;
begin
 {}
 Result:=0;
 
 {Check RTC}
 if RTC = nil then Exit;
 
 {$IF DEFINED(DS1307_DEBUG) or DEFINED(RTC_DEBUG)}
 if RTC_LOG_ENABLED then RTCLogDebug(RTC,'DS1307: RTC Get Time');
 {$ENDIF}
 
 if MutexLock(RTC.Lock) <> ERROR_SUCCESS then Exit;
 
 {Update Statistics}
 Inc(RTC.GetCount);
 
 {Read RTC Data}
 if DS1307ReadRTC(RTC,@Data,SizeOf(TDS1307RTCTime)) = ERROR_SUCCESS then
  begin
   {Convert to File Time}
   if DS1307RTCTimeToFileTime(RTC,@Data,SizeOf(TDS1307RTCTime),FileTime) then
    begin
     Result:=FileTime;
    end;
  end; 
 
 MutexUnlock(RTC.Lock);
 
 {$IF DEFINED(DS1307_DEBUG) or DEFINED(RTC_DEBUG)}
 if RTC_LOG_ENABLED then RTCLogDebug(RTC,'DS1307:  Time=' + IntToStr(Result));
 {$ENDIF}
end;

{==============================================================================}

function DS1307RTCSetTime(RTC:PRTCDevice;const Time:Int64):Int64;
var
 Data:TDS1307RTCTime;
begin
 {}
 Result:=0;

 {Check RTC}
 if RTC = nil then Exit;
 
 {$IF DEFINED(DS1307_DEBUG) or DEFINED(RTC_DEBUG)}
 if RTC_LOG_ENABLED then RTCLogDebug(RTC,'DS1307: RTC Set Time (Time=' + IntToStr(Time) + ')');
 {$ENDIF}
 
 {Check Time}
 if (Time < DS1307_MIN_TIME) or (Time > DS1307_MAX_TIME) then Exit;
 
 if MutexLock(RTC.Lock) <> ERROR_SUCCESS then Exit;
 
 {Update Statistics}
 Inc(RTC.SetCount);

 {Convert to RTC Time}
 if DS1307FileTimeToRTCTime(RTC,Time,@Data,SizeOf(TDS1307RTCTime)) then
  begin
   {Write RTC Data}
   if DS1307WriteRTC(RTC,@Data,SizeOf(TDS1307RTCTime)) = ERROR_SUCCESS then
    begin
     Result:=Time;
    end;
  end; 
 
 MutexUnlock(RTC.Lock);
end;

{==============================================================================}
{==============================================================================}
{DS1307 Helper Functions}
function DS1307ReadRTC(RTC:PRTCDevice;Buffer:Pointer;Size:LongWord):LongWord;
var
 Count:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Buffer}
 if Buffer = nil then Exit;
 
 {Check Size}
 if Size < 7 then Exit;
 
 {Check RTC}
 if RTC = nil then Exit;
 
 {$IF DEFINED(DS1307_DEBUG) or DEFINED(RTC_DEBUG)}
 if RTC_LOG_ENABLED then RTCLogDebug(RTC,'DS1307: Read RTC');
 {$ENDIF}
 
 if MutexLock(RTC.Lock) <> ERROR_SUCCESS then Exit;
 
 {Update Statistics}
 Inc(PDS1307RTCDevice(RTC).ReadCount);
 
 Count:=0;
 
 {Read RTC Registers}
 Result:=I2CDeviceWriteRead(PDS1307RTCDevice(RTC).I2C,PDS1307RTCDevice(RTC).Address,@PDS1307RTCDevice(RTC).RTCReg,SizeOf(Byte),Buffer,Size,Count);
 
 MutexUnlock(RTC.Lock);
end;

{==============================================================================}

function DS1307WriteRTC(RTC:PRTCDevice;Buffer:Pointer;Size:LongWord):LongWord;
var
 Count:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Buffer}
 if Buffer = nil then Exit;
 
 {Check Size}
 if Size < 7 then Exit;
 
 {Check RTC}
 if RTC = nil then Exit;
 
 {$IF DEFINED(DS1307_DEBUG) or DEFINED(RTC_DEBUG)}
 if RTC_LOG_ENABLED then RTCLogDebug(RTC,'DS1307: Write RTC');
 {$ENDIF}
 
 if MutexLock(RTC.Lock) <> ERROR_SUCCESS then Exit;
 
 {Update Statistics}
 Inc(PDS1307RTCDevice(RTC).WriteCount);
 
 Count:=0;
 
 {Write RTC Registers}
 Result:=I2CDeviceWriteWrite(PDS1307RTCDevice(RTC).I2C,PDS1307RTCDevice(RTC).Address,@PDS1307RTCDevice(RTC).RTCReg,SizeOf(Byte),Buffer,Size,Count);
 
 MutexUnlock(RTC.Lock);
end;

{==============================================================================}

function DS1307ReadNVRAM(RTC:PRTCDevice;Offset:Byte;Buffer:Pointer;Size:LongWord):LongWord;
var
 Reg:Byte;
 Count:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Buffer}
 if Buffer = nil then Exit;
 
 {Check RTC}
 if RTC = nil then Exit;
 
 {$IF DEFINED(DS1307_DEBUG) or DEFINED(RTC_DEBUG)}
 if RTC_LOG_ENABLED then RTCLogDebug(RTC,'DS1307: Read NVRAM');
 {$ENDIF}
 
 if MutexLock(RTC.Lock) <> ERROR_SUCCESS then Exit;
 
 {Check NVRAM}
 if PDS1307RTCDevice(RTC).NVRAMSize <> 0 then
  begin
   {Check Offset}
   if (Offset + Size) <= PDS1307RTCDevice(RTC).NVRAMSize then
    begin
     {Update Statistics}
     Inc(PDS1307RTCDevice(RTC).ReadCount);
   
     {Get Reg}
     Reg:=PDS1307RTCDevice(RTC).NVRAMReg + Offset;
     
     {Read NVRAM Registers}
     Result:=I2CDeviceWriteRead(PDS1307RTCDevice(RTC).I2C,PDS1307RTCDevice(RTC).Address,@Reg,SizeOf(Byte),Buffer,Size,Count);
    end; 
  end; 
 
 MutexUnlock(RTC.Lock);
end;

{==============================================================================}

function DS1307WriteNVRAM(RTC:PRTCDevice;Offset:Byte;Buffer:Pointer;Size:LongWord):LongWord;
var
 Reg:Byte;
 Count:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Buffer}
 if Buffer = nil then Exit;
 
 {Check RTC}
 if RTC = nil then Exit;
 
 {$IF DEFINED(DS1307_DEBUG) or DEFINED(RTC_DEBUG)}
 if RTC_LOG_ENABLED then RTCLogDebug(RTC,'DS1307: Write NVRAM');
 {$ENDIF}
 
 if MutexLock(RTC.Lock) <> ERROR_SUCCESS then Exit;
 
 {Check NVRAM}
 if PDS1307RTCDevice(RTC).NVRAMSize <> 0 then
  begin
   {Check Offset}
   if (Offset + Size) <= PDS1307RTCDevice(RTC).NVRAMSize then
    begin
     {Update Statistics}
     Inc(PDS1307RTCDevice(RTC).WriteCount);
 
     {Get Reg}
     Reg:=PDS1307RTCDevice(RTC).NVRAMReg + Offset;
     
     {Write NVRAM Registers}
     Result:=I2CDeviceWriteWrite(PDS1307RTCDevice(RTC).I2C,PDS1307RTCDevice(RTC).Address,@Reg,SizeOf(Byte),Buffer,Size,Count);
    end; 
  end;
  
 MutexUnlock(RTC.Lock);
end;
 
{==============================================================================}
 
function DS1307ReadControl(RTC:PRTCDevice;var Control:Byte):LongWord;
var
 Count:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check RTC}
 if RTC = nil then Exit;
 
 {$IF DEFINED(DS1307_DEBUG) or DEFINED(RTC_DEBUG)}
 if RTC_LOG_ENABLED then RTCLogDebug(RTC,'DS1307: Read Control');
 {$ENDIF}
 
 if MutexLock(RTC.Lock) <> ERROR_SUCCESS then Exit;
 
 {Update Statistics}
 Inc(PDS1307RTCDevice(RTC).ReadCount);
 
 Count:=0;
 
 {Read Control Register}
 Result:=I2CDeviceWriteRead(PDS1307RTCDevice(RTC).I2C,PDS1307RTCDevice(RTC).Address,@PDS1307RTCDevice(RTC).ControlReg,SizeOf(Byte),@Control,SizeOf(Byte),Count);
 
 MutexUnlock(RTC.Lock);
end;

{==============================================================================}

function DS1307WriteControl(RTC:PRTCDevice;Control:Byte):LongWord;
var
 Count:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check RTC}
 if RTC = nil then Exit;
 
 {$IF DEFINED(DS1307_DEBUG) or DEFINED(RTC_DEBUG)}
 if RTC_LOG_ENABLED then RTCLogDebug(RTC,'DS1307: Write Control');
 {$ENDIF}
 
 if MutexLock(RTC.Lock) <> ERROR_SUCCESS then Exit;
 
 {Update Statistics}
 Inc(PDS1307RTCDevice(RTC).WriteCount);
 
 Count:=0;
 
 {Write Control Register}
 Result:=I2CDeviceWriteWrite(PDS1307RTCDevice(RTC).I2C,PDS1307RTCDevice(RTC).Address,@PDS1307RTCDevice(RTC).ControlReg,SizeOf(Byte),@Control,SizeOf(Byte),Count);
 
 MutexUnlock(RTC.Lock);
end;

{==============================================================================}

function DS1307DefaultRTCTime(RTC:PRTCDevice;Buffer:Pointer;Size:LongWord):Boolean;
begin
 {}
 Result:=DS1307FileTimeToRTCTime(RTC,DS1307_MIN_TIME,Buffer,Size);
end;

{==============================================================================}

function DS1307FileTimeToRTCTime(RTC:PRTCDevice;const FileTime:Int64;Buffer:Pointer;Size:LongWord):Boolean;
var
 RTCTime:PDS1307RTCTime;
 SystemTime:TSystemTime;
begin
 {}
 Result:=False;
 
 {Check Buffer}
 if Buffer = nil then Exit;
 
 {Check Size}
 if Size < 7 then Exit;
 
 {Check RTC}
 if RTC = nil then Exit;

 {Check Time}
 if (FileTime < DS1307_MIN_TIME) or (FileTime > DS1307_MAX_TIME) then Exit;
 
 {Convert to System Time}
 if RTCFileTimeToSystemTime(FileTime,SystemTime) then
  begin
   {$IF DEFINED(DS1307_DEBUG) or DEFINED(RTC_DEBUG)}
   if RTC_LOG_ENABLED then RTCLogDebug(RTC,'DS1307: DS1307FileTimeToRTCTime');
   if RTC_LOG_ENABLED then RTCLogDebug(RTC,'DS1307:  SystemTime.Second=' + IntToStr(SystemTime.Second));
   if RTC_LOG_ENABLED then RTCLogDebug(RTC,'DS1307:  SystemTime.Minute=' + IntToStr(SystemTime.Minute));
   if RTC_LOG_ENABLED then RTCLogDebug(RTC,'DS1307:  SystemTime.Hour=' + IntToStr(SystemTime.Hour));
   if RTC_LOG_ENABLED then RTCLogDebug(RTC,'DS1307:  SystemTime.DayOfWeek=' + IntToStr(SystemTime.DayOfWeek));
   if RTC_LOG_ENABLED then RTCLogDebug(RTC,'DS1307:  SystemTime.Day=' + IntToStr(SystemTime.Day));
   if RTC_LOG_ENABLED then RTCLogDebug(RTC,'DS1307:  SystemTime.Month=' + IntToStr(SystemTime.Month));
   if RTC_LOG_ENABLED then RTCLogDebug(RTC,'DS1307:  SystemTime.Year=' + IntToStr(SystemTime.Year));
   {$ENDIF}
   
   {Convert to RTC Time}
   RTCTime:=PDS1307RTCTime(Buffer);
   RTCTime[DS1307_REG_SECONDS]:=BintoBCD(SystemTime.Second);
   RTCTime[DS1307_REG_MINUTE]:=BintoBCD(SystemTime.Minute);
   RTCTime[DS1307_REG_HOUR]:=BintoBCD(SystemTime.Hour);
   RTCTime[DS1307_REG_WEEKDAY]:=BintoBCD(SystemTime.DayOfWeek);
   RTCTime[DS1307_REG_DAY]:=BintoBCD(SystemTime.Day);
   RTCTime[DS1307_REG_MONTH]:=BintoBCD(SystemTime.Month);
   RTCTime[DS1307_REG_YEAR]:=BintoBCD(SystemTime.Year - 2000);
   
   {$IF DEFINED(DS1307_DEBUG) or DEFINED(RTC_DEBUG)}
   if RTC_LOG_ENABLED then RTCLogDebug(RTC,'DS1307: DS1307FileTimeToRTCTime');
   if RTC_LOG_ENABLED then RTCLogDebug(RTC,'DS1307:  RTCTime[DS1307_REG_SECONDS]=' + IntToHex(RTCTime[DS1307_REG_SECONDS],2));
   if RTC_LOG_ENABLED then RTCLogDebug(RTC,'DS1307:  RTCTime[DS1307_REG_MINUTE]=' + IntToHex(RTCTime[DS1307_REG_MINUTE],2));
   if RTC_LOG_ENABLED then RTCLogDebug(RTC,'DS1307:  RTCTime[DS1307_REG_HOUR]=' + IntToHex(RTCTime[DS1307_REG_HOUR],2));
   if RTC_LOG_ENABLED then RTCLogDebug(RTC,'DS1307:  RTCTime[DS1307_REG_WEEKDAY]=' + IntToHex(RTCTime[DS1307_REG_WEEKDAY],2));
   if RTC_LOG_ENABLED then RTCLogDebug(RTC,'DS1307:  RTCTime[DS1307_REG_DAY]=' + IntToHex(RTCTime[DS1307_REG_DAY],2));
   if RTC_LOG_ENABLED then RTCLogDebug(RTC,'DS1307:  RTCTime[DS1307_REG_MONTH]=' + IntToHex(RTCTime[DS1307_REG_MONTH],2));
   if RTC_LOG_ENABLED then RTCLogDebug(RTC,'DS1307:  RTCTime[DS1307_REG_YEAR]=' + IntToHex(RTCTime[DS1307_REG_YEAR],2));
   {$ENDIF}
   
   {Check Chip type}
   case PDS1307RTCDevice(RTC).Chip of
    DS1307_CHIP_DS1337,DS1307_CHIP_DS1339,DS1307_CHIP_DS3231:begin
      {Set century bit}
      RTCTime[DS1307_REG_MONTH]:=RTCTime[DS1307_REG_MONTH] or DS1337_REG_MONTH_CENTURY;
     end;
    DS1307_CHIP_DS1340 :begin
      {Set century bit}
      RTCTime[DS1307_REG_HOUR]:=RTCTime[DS1307_REG_HOUR] or DS1340_REG_HOUR_CENTURY_EN or DS1340_REG_HOUR_CENTURY;
     end;
    DS1307_CHIP_MCP794XX:begin
      {Reenable clock and battery backup}
      RTCTime[DS1307_REG_SECONDS]:=RTCTime[DS1307_REG_SECONDS] or MCP794XX_REG_SECONDS_ST;
      RTCTime[DS1307_REG_WEEKDAY]:=RTCTime[DS1307_REG_WEEKDAY] or MCP794XX_REG_WEEKDAY_VBATEN;
     end;    
   end;
   
   Result:=True;
  end; 
end;

{==============================================================================}

function DS1307RTCTimeToFileTime(RTC:PRTCDevice;Buffer:Pointer;Size:LongWord;var FileTime:Int64):Boolean;
var
 RTCTime:PDS1307RTCTime;
 SystemTime:TSystemTime;
begin
 {}
 Result:=False;
 
 {Setup FileTime}
 FileTime:=0;
 
 {Check Buffer}
 if Buffer = nil then Exit;
 
 {Check Size}
 if Size < 7 then Exit;
 
 {Check RTC}
 if RTC = nil then Exit;
 
 {Get RTC Time}
 RTCTime:=PDS1307RTCTime(Buffer);
 
 {$IF DEFINED(DS1307_DEBUG) or DEFINED(RTC_DEBUG)}
 if RTC_LOG_ENABLED then RTCLogDebug(RTC,'DS1307: DS1307RTCTimeToFileTime');
 if RTC_LOG_ENABLED then RTCLogDebug(RTC,'DS1307:  RTCTime[DS1307_REG_SECONDS]=' + IntToHex(RTCTime[DS1307_REG_SECONDS],2));
 if RTC_LOG_ENABLED then RTCLogDebug(RTC,'DS1307:  RTCTime[DS1307_REG_MINUTE]=' + IntToHex(RTCTime[DS1307_REG_MINUTE],2));
 if RTC_LOG_ENABLED then RTCLogDebug(RTC,'DS1307:  RTCTime[DS1307_REG_HOUR]=' + IntToHex(RTCTime[DS1307_REG_HOUR],2));
 if RTC_LOG_ENABLED then RTCLogDebug(RTC,'DS1307:  RTCTime[DS1307_REG_WEEKDAY]=' + IntToHex(RTCTime[DS1307_REG_WEEKDAY],2));
 if RTC_LOG_ENABLED then RTCLogDebug(RTC,'DS1307:  RTCTime[DS1307_REG_DAY]=' + IntToHex(RTCTime[DS1307_REG_DAY],2));
 if RTC_LOG_ENABLED then RTCLogDebug(RTC,'DS1307:  RTCTime[DS1307_REG_MONTH]=' + IntToHex(RTCTime[DS1307_REG_MONTH],2));
 if RTC_LOG_ENABLED then RTCLogDebug(RTC,'DS1307:  RTCTime[DS1307_REG_YEAR]=' + IntToHex(RTCTime[DS1307_REG_YEAR],2));
 {$ENDIF}
 
 {Convert to System Time}
 FillChar(SystemTime,SizeOf(TSystemTime),0);
 SystemTime.Second:=BCDtoBin(RTCTime[DS1307_REG_SECONDS] and $7F);
 SystemTime.Minute:=BCDtoBin(RTCTime[DS1307_REG_MINUTE] and $7F);
 SystemTime.Hour:=BCDtoBin(RTCTime[DS1307_REG_HOUR] and $3F);
 SystemTime.DayOfWeek:=BCDtoBin(RTCTime[DS1307_REG_WEEKDAY] and $07);
 SystemTime.Day:=BCDtoBin(RTCTime[DS1307_REG_DAY] and $3F);
 SystemTime.Month:=BCDtoBin(RTCTime[DS1307_REG_MONTH] and $1F);
 SystemTime.Year:=BCDtoBin(RTCTime[DS1307_REG_YEAR]) + 2000;
 
 {$IF DEFINED(DS1307_DEBUG) or DEFINED(RTC_DEBUG)}
 if RTC_LOG_ENABLED then RTCLogDebug(RTC,'DS1307: DS1307RTCTimeToFileTime');
 if RTC_LOG_ENABLED then RTCLogDebug(RTC,'DS1307:  SystemTime.Second=' + IntToStr(SystemTime.Second));
 if RTC_LOG_ENABLED then RTCLogDebug(RTC,'DS1307:  SystemTime.Minute=' + IntToStr(SystemTime.Minute));
 if RTC_LOG_ENABLED then RTCLogDebug(RTC,'DS1307:  SystemTime.Hour=' + IntToStr(SystemTime.Hour));
 if RTC_LOG_ENABLED then RTCLogDebug(RTC,'DS1307:  SystemTime.DayOfWeek=' + IntToStr(SystemTime.DayOfWeek));
 if RTC_LOG_ENABLED then RTCLogDebug(RTC,'DS1307:  SystemTime.Day=' + IntToStr(SystemTime.Day));
 if RTC_LOG_ENABLED then RTCLogDebug(RTC,'DS1307:  SystemTime.Month=' + IntToStr(SystemTime.Month));
 if RTC_LOG_ENABLED then RTCLogDebug(RTC,'DS1307:  SystemTime.Year=' + IntToStr(SystemTime.Year));
 {$ENDIF}
 
 {Check System Time}
 if not RTCTimeIsValid(SystemTime) then Exit;
 
 {Convert to File Time}
 Result:=RTCSystemTimeToFileTime(SystemTime,FileTime);
end;
 
{==============================================================================}
{==============================================================================}

initialization
 DS1307Init;
 
{==============================================================================}
 
finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
 