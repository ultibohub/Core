{
EDT FocalTech FT5x06 I2C Touchscreen Driver.

Copyright (C) 2022 - SoftOz Pty Ltd.

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

  Linux - \drivers\input\touchscreen\edt-ft5x06.c - Copyright (C) 2012 Simon Budig

References
==========


EDT FocalTech FT5x06 I2C Touchscreen
====================================

 The EDT FocalTech FT5x06 I2C controllers are a range of capacitive touchscreen
 controllers that include multipoint touch support and are used by a variety of
 small LCD displays.

 The exact features and capability of each device can be found by checking the
 datasheets which can be obtained online.

 This driver is compatible with the following devices:

  edt,edt-ft5206       (5 touch points)
  edt,edt-ft5306       (5 touch points)
  edt,edt-ft5406       (5 touch points)
  edt,edt-ft5506       (10 touch points)
  evervision,ev-ft5726 (10 touch points)
  focaltech,ft6236     (2 touch points)

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit FT5x06Touch;

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,Devices,GPIO,I2C,Touch,Mouse,SysUtils;

{==============================================================================}
{Global definitions}
{$INCLUDE ..\core\GlobalDefines.inc}

{==============================================================================}
const
 {FT5x06 specific constants}
 FT5X06_TOUCH_DESCRIPTION = 'EDT FocalTech FT5x06 Touch Controller';  {Description of FT5x06 Touch device}

 {FT5x06 I2C constants}
 FT5X06_I2C_RATE = 400000; {Default I2C clock rate}

 {FT5x06 register constants}
 FT5X06_WORK_REGISTER_THRESHOLD = $00;
 FT5X06_WORK_REGISTER_REPORT_RATE = $08;
 FT5X06_WORK_REGISTER_GAIN = $30;
 FT5X06_WORK_REGISTER_OFFSET = $31;
 FT5X06_WORK_REGISTER_NUM_X = $33;
 FT5X06_WORK_REGISTER_NUM_Y = $34;

 FT5X06_PMOD_REGISTER_ACTIVE = $00;
 FT5X06_PMOD_REGISTER_HIBERNATE = $03;

 FT5X06_M09_REGISTER_THRESHOLD = $80;
 FT5X06_M09_REGISTER_GAIN = $92;
 FT5X06_M09_REGISTER_OFFSET = $93;
 FT5X06_M09_REGISTER_NUM_X = $94;
 FT5X06_M09_REGISTER_NUM_Y = $95;

 FT5X06_EV_REGISTER_THRESHOLD = $40;
 FT5X06_EV_REGISTER_GAIN = $41;
 FT5X06_EV_REGISTER_OFFSET_Y = $45;
 FT5X06_EV_REGISTER_OFFSET_X = $46;

 FT5X06_NO_REGISTER = $ff;

 FT5X06_WORK_REGISTER_OPMODE = $3c;
 FT5X06_FACTORY_REGISTER_OPMODE = $01;
 FT5X06_PMOD_REGISTER_OPMODE = $a5;

 FT5X06_TOUCH_EVENT_DOWN = $00;
 FT5X06_TOUCH_EVENT_UP = $01;
 FT5X06_TOUCH_EVENT_ON = $02;
 FT5X06_TOUCH_EVENT_RESERVED = $03;

 FT5X06_EDT_NAME_LEN = 23;
 FT5X06_EDT_SWITCH_MODE_RETRIES = 10;
 FT5X06_EDT_SWITCH_MODE_DELAY = 5; {msec}
 FT5X06_EDT_RAW_DATA_RETRIES = 100;
 FT5X06_EDT_RAW_DATA_DELAY = 1000; {usec}

 FT5X06_POLL_INTERVAL_MS = 17; {17ms = 60fps}

 {FT5x06 version constants}
 FT5X06_EDT_M06    = 0;
 FT5X06_EDT_M09    = 1;
 FT5X06_EDT_M12    = 2;
 FT5X06_EV_FT      = 3;
 FT5X06_GENERIC_FT = 4;

{==============================================================================}
type
 {FT5x06 specific types}
 PFT5X06Registers = ^TFT5X06Registers;
 TFT5X06Registers = record
  Threshold:Byte;
  ReportRate:Byte;
  Gain:Byte;
  Offset:Byte;
  OffsetX:Byte;
  OffsetY:Byte;
  NumX:Byte;
  NumY:Byte;
 end;

 PFT5X06Parameters = ^TFT5X06Parameters;
 TFT5X06Parameters = record
  Threshold:Byte;
  ReportRate:Byte;
  Gain:Byte;
  Offset:Byte;
  OffsetX:Byte;
  OffsetY:Byte;
  NumX:Byte;
  NumY:Byte;
 end;

 PFT5X06Touch = ^TFT5X06Touch;
 TFT5X06Touch = record
  {Touch Properties}
  Touch:TTouchDevice;
  {I2C Properties}
  I2C:PI2CDevice;               {The I2C device this device is connected to}
  Address:Word;                 {The I2C address of the device}
  {General Properties}
  IRQ:TGPIOInfo;                {The GPIO information for the IRQ line (Optional)}
  RST:TGPIOInfo;                {The GPIO information for the Reset line (Optional)}
  Timer:TTimerHandle;           {Handle for touch polling timer}
  MaxX:Word;                    {Maximum X value from current configuration}
  MaxY:Word;                    {Maximum Y value from current configuration}
  Width:Word;                   {Screen width value supplied during create}
  Height:Word;                  {Screen height value supplied during create}
  MaxPoints:LongWord;           {Maximum touch points for this device}
  LastPoints:LongWord;          {Points reported in last input report}
  {FT5x06 Properties}
  Version:LongWord;             {Version constant for this device (eg FT5X06_EDT_M06)}
  ModelName:String;             {Model name string for this device}
  FirmwareVersion:String;       {Firmware version string for this device}
  Registers:TFT5X06Registers;   {Register addresses for this device}
  Parameters:TFT5X06Parameters; {Configuration parameters for this device}
  SwapReportXY:Boolean;         {If True swap the X and Y values in the input report}
  InvertReportX:Boolean;        {If True invert the X value in the input report}
  InvertReportY:Boolean;        {If True invert the Y value in the input report}
 end;

{==============================================================================}
var
 {FT5x06 specific variables}
 FT5X06_GAIN:Byte;
 FT5X06_OFFSET:Byte;
 FT5X06_OFFSET_X:Byte;
 FT5X06_OFFSET_Y:Byte;
 FT5X06_THRESHOLD:Byte;
 FT5X06_SWAP_REPORT_XY:Boolean;
 FT5X06_INVERT_REPORT_X:Boolean;
 FT5X06_INVERT_REPORT_Y:Boolean;
 FT5X06_MAX_TOUCH_POINTS:LongWord = 2;

{==============================================================================}
{Initialization Functions}
procedure FT5X06Init;

{==============================================================================}
{FT5x06 Functions}
function FT5X06TouchCreate(I2C:PI2CDevice;Address:Word;Width,Height:LongWord;IRQ,RST:PGPIOInfo):PTouchDevice;
function FT5X06TouchDestroy(Touch:PTouchDevice):LongWord;

{==============================================================================}
{FT5x06 Touch Functions}
function FT5X06TouchStart(Touch:PTouchDevice):LongWord;
function FT5X06TouchStop(Touch:PTouchDevice):LongWord;

function FT5X06TouchUpdate(Touch:PTouchDevice):LongWord;

procedure FT5X06TouchTimer(Touch:PFT5X06Touch);
procedure FT5X06TouchCallback(Touch:PFT5X06Touch;Pin,Trigger:LongWord);

{==============================================================================}
{FT5x06 Helper Functions}

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {FT5x06 specific variables}
 FT5X06Initialized:Boolean;

{==============================================================================}
{==============================================================================}
{Forward Declarations}
function FT5X06ReadWrite(Touch:PFT5X06Touch;WriteLen:Word;WriteData:PByte;ReadLen:Word;ReadData:PByte):LongWord; forward;

function FT5X06RegisterWrite(Touch:PFT5X06Touch;Reg,Value:Byte):LongWord; forward;
function FT5X06RegisterRead(Touch:PFT5X06Touch;Reg:Byte;var Value:Byte):LongWord; forward;

function FT5X06Identify(Touch:PFT5X06Touch):LongWord; forward;

function FT5X06GetRegisters(Touch:PFT5X06Touch):LongWord; forward;

function FT5X06SetDefaults(Touch:PFT5X06Touch):LongWord; forward;
function FT5X06GetParameters(Touch:PFT5X06Touch):LongWord; forward;

function FT5X06CheckCRC(Touch:PFT5X06Touch;Data:PByte;Size:LongWord):LongWord; forward;

function FT5X06UpdateConfig(Touch:PFT5X06Touch):LongWord; forward;

function FT5X06ProcessEvents(Touch:PFT5X06Touch;Polling:Boolean):LongWord; forward;

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure FT5X06Init;
{Initialize the FT5x06 Touch unit and parameters}

{Note: Called internally by other functions}
var
 WorkInt:LongWord;
 WorkBool:LongBool;
begin
 {}
 {Check Initialized}
 if FT5X06Initialized then Exit;

 {Check Environment Variables}
 {FT5X06_GAIN}
 WorkInt:=StrToIntDef(SysUtils.GetEnvironmentVariable('FT5X06_GAIN'),FT5X06_GAIN);
 if WorkInt <> FT5X06_GAIN then FT5X06_GAIN:=WorkInt;

 {FT5X06_OFFSET}
 WorkInt:=StrToIntDef(SysUtils.GetEnvironmentVariable('FT5X06_OFFSET'),FT5X06_OFFSET);
 if WorkInt <> FT5X06_OFFSET then FT5X06_OFFSET:=WorkInt;

 {FT5X06_OFFSET_X}
 WorkInt:=StrToIntDef(SysUtils.GetEnvironmentVariable('FT5X06_OFFSET_X'),FT5X06_OFFSET_X);
 if WorkInt <> FT5X06_OFFSET_X then FT5X06_OFFSET_X:=WorkInt;

 {FT5X06_OFFSET_Y}
 WorkInt:=StrToIntDef(SysUtils.GetEnvironmentVariable('FT5X06_OFFSET_Y'),FT5X06_OFFSET_Y);
 if WorkInt <> FT5X06_OFFSET_Y then FT5X06_OFFSET_Y:=WorkInt;

 {FT5X06_THRESHOLD}
 WorkInt:=StrToIntDef(SysUtils.GetEnvironmentVariable('FT5X06_THRESHOLD'),FT5X06_THRESHOLD);
 if WorkInt <> FT5X06_THRESHOLD then FT5X06_THRESHOLD:=WorkInt;

 {FT5X06_SWAP_REPORT_XY}
 WorkBool:=StrToBoolDef(SysUtils.GetEnvironmentVariable('FT5X06_SWAP_REPORT_XY'),FT5X06_SWAP_REPORT_XY);
 if WorkBool <> FT5X06_SWAP_REPORT_XY then FT5X06_SWAP_REPORT_XY:=WorkBool;

 {FT5X06_INVERT_REPORT_X}
 WorkBool:=StrToBoolDef(SysUtils.GetEnvironmentVariable('FT5X06_INVERT_REPORT_X'),FT5X06_INVERT_REPORT_X);
 if WorkBool <> FT5X06_INVERT_REPORT_X then FT5X06_INVERT_REPORT_X:=WorkBool;

 {FT5X06_INVERT_REPORT_Y}
 WorkBool:=StrToBoolDef(SysUtils.GetEnvironmentVariable('FT5X06_INVERT_REPORT_Y'),FT5X06_INVERT_REPORT_Y);
 if WorkBool <> FT5X06_INVERT_REPORT_Y then FT5X06_INVERT_REPORT_Y:=WorkBool;

 {FT5X06_MAX_TOUCH_POINTS}
 WorkInt:=StrToIntDef(SysUtils.GetEnvironmentVariable('FT5X06_MAX_TOUCH_POINTS'),FT5X06_MAX_TOUCH_POINTS);
 if WorkInt <> FT5X06_MAX_TOUCH_POINTS then FT5X06_MAX_TOUCH_POINTS:=WorkInt;

 FT5X06Initialized:=True;
end;

{==============================================================================}
{==============================================================================}
{FT5x06 Functions}
function FT5X06TouchCreate(I2C:PI2CDevice;Address:Word;Width,Height:LongWord;IRQ,RST:PGPIOInfo):PTouchDevice;
{Create, register and start a new FT5x06 Touch device connected to the specified I2C device}
{I2C: The I2C device this FT5x06 Touch device is connected to}
{Address: The I2C address for this FT5x06 Touch device}
{Width: The width of the screen in pixels (When set at TOUCH_ROTATION_0)}
{Height: The height of the screen in pixels (When set at TOUCH_ROTATION_0)}
{IRQ: The GPIO information for the IRQ line (Optional)}
{Return: Pointer to the new Touch device or nil on failure}
var
 Status:LongWord;

 FT5X06Touch:PFT5X06Touch;
begin
 {}
 Result:=nil;

 {Initialize}
 FT5X06Init;

 {$IF DEFINED(FT5X06_DEBUG) or DEFINED(TOUCH_DEBUG)}
 if TOUCH_LOG_ENABLED then TOUCHLogDebug(nil,'FT5X06: Touch Create (Address=' + IntToHex(Address,4) + ' Width=' + IntToStr(Width) + ' Height=' + IntToStr(Height) + ')');
 {$ENDIF}

 {Check I2C}
 if I2C = nil then Exit;

 {Check Address}
 if Address = I2C_ADDRESS_INVALID then Exit;

 {Check Width and Height}
 if Width < 1 then Exit;
 if Height < 1 then Exit;

 {Create Touch}
 FT5X06Touch:=PFT5X06Touch(TouchDeviceCreateEx(SizeOf(TFT5X06Touch)));
 if FT5X06Touch <> nil then
  begin
   {Update Touch}
   {Device}
   FT5X06Touch.Touch.Device.DeviceBus:=DEVICE_BUS_I2C;
   FT5X06Touch.Touch.Device.DeviceType:=TOUCH_TYPE_CAPACITIVE;
   FT5X06Touch.Touch.Device.DeviceFlags:=FT5X06Touch.Touch.Device.DeviceFlags or TOUCH_FLAG_MULTI_POINT;
   FT5X06Touch.Touch.Device.DeviceData:=nil;
   FT5X06Touch.Touch.Device.DeviceDescription:=FT5X06_TOUCH_DESCRIPTION;
   {Touch}
   FT5X06Touch.Touch.TouchState:=TOUCH_STATE_DISABLED;
   FT5X06Touch.Touch.DeviceStart:=FT5X06TouchStart;
   FT5X06Touch.Touch.DeviceStop:=FT5X06TouchStop;
   FT5X06Touch.Touch.DeviceUpdate:=FT5X06TouchUpdate;
   {Driver}
   FT5X06Touch.Touch.Properties.Flags:=FT5X06Touch.Touch.Device.DeviceFlags;
   FT5X06Touch.Touch.Properties.Width:=Width;
   FT5X06Touch.Touch.Properties.Height:=Height;
   FT5X06Touch.Touch.Properties.Rotation:=TOUCH_ROTATION_0;
   FT5X06Touch.Touch.Properties.MaxX:=0;
   FT5X06Touch.Touch.Properties.MaxY:=0;
   FT5X06Touch.Touch.Properties.MaxZ:=0;
   FT5X06Touch.Touch.Properties.MaxPoints:=0;
   {I2C}
   FT5X06Touch.I2C:=I2C;
   FT5X06Touch.Address:=Address;
   {General}
   if IRQ <> nil then FT5X06Touch.IRQ:=IRQ^ else FT5X06Touch.IRQ:=GPIO_INFO_UNKNOWN;
   if RST <> nil then FT5X06Touch.RST:=RST^ else FT5X06Touch.RST:=GPIO_INFO_UNKNOWN;
   FT5X06Touch.Timer:=INVALID_HANDLE_VALUE;
   FT5X06Touch.Width:=Width;
   FT5X06Touch.Height:=Height;
   FT5X06Touch.MaxPoints:=FT5X06_MAX_TOUCH_POINTS;
   {FT5X06}
   FT5X06Touch.Parameters.Gain:=FT5X06_GAIN;
   FT5X06Touch.Parameters.Offset:=FT5X06_OFFSET;
   FT5X06Touch.Parameters.OffsetX:=FT5X06_OFFSET_X;
   FT5X06Touch.Parameters.OffsetY:=FT5X06_OFFSET_Y;
   FT5X06Touch.Parameters.Threshold:=FT5X06_THRESHOLD;
   FT5X06Touch.SwapReportXY:=FT5X06_SWAP_REPORT_XY;
   FT5X06Touch.InvertReportX:=FT5X06_INVERT_REPORT_X;
   FT5X06Touch.InvertReportY:=FT5X06_INVERT_REPORT_Y;

   {Register Touch}
   Status:=TouchDeviceRegister(@FT5X06Touch.Touch);
   if Status = ERROR_SUCCESS then
    begin
     {Start Touch}
     Status:=TouchDeviceStart(@FT5X06Touch.Touch);
     if Status = ERROR_SUCCESS then
      begin
       {Return Result}
       Result:=PTouchDevice(FT5X06Touch);
      end
     else
      begin
       if TOUCH_LOG_ENABLED then TouchLogError(nil,'FT5X06: Failed to start new Touch device: ' + ErrorToString(Status));

       {Deregister Touch}
       TouchDeviceDeregister(@FT5X06Touch.Touch);

       {Destroy Touch}
       TouchDeviceDestroy(@FT5X06Touch.Touch);
      end;
    end
   else
    begin
     if TOUCH_LOG_ENABLED then TouchLogError(nil,'FT5X06: Failed to register new Touch device: ' + ErrorToString(Status));

     {Destroy Touch}
     TouchDeviceDestroy(@FT5X06Touch.Touch);
    end;
  end
 else
  begin
   if TOUCH_LOG_ENABLED then TouchLogError(nil,'FT5X06: Failed to create new Touch device');
  end;
end;

{==============================================================================}

function FT5X06TouchDestroy(Touch:PTouchDevice):LongWord;
{Stop, deregister and destroy a FT5x06 Touch device created by this driver}
{Touch: The Touch device to destroy}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Touch}
 if Touch = nil then Exit;

 {$IF DEFINED(FT5X06_DEBUG) or DEFINED(TOUCH_DEBUG)}
 if TOUCH_LOG_ENABLED then TouchLogDebug(Touch,'FT5X06: Touch Destroy');
 {$ENDIF}

 {Stop Touch}
 Result:=TouchDeviceStop(Touch);
 if Result = ERROR_SUCCESS then
  begin
   {Deregister Touch}
   Result:=TouchDeviceDeregister(Touch);
   if Result = ERROR_SUCCESS then
    begin
     {Destroy Touch}
     Result:=TouchDeviceDestroy(Touch);
     if Result <> ERROR_SUCCESS then
      begin
       if TOUCH_LOG_ENABLED then TouchLogError(nil,'FT5X06: Failed to destroy Touch device: ' + ErrorToString(Result));
      end;
    end
   else
    begin
     if TOUCH_LOG_ENABLED then TouchLogError(nil,'FT5X06: Failed to deregister Touch device: ' + ErrorToString(Result));
    end;
  end
 else
  begin
   if TOUCH_LOG_ENABLED then TouchLogError(nil,'FT5X06: Failed to stop Touch device: ' + ErrorToString(Result));
  end;
end;

{==============================================================================}
{==============================================================================}
{FT5x06 Touch Functions}
function FT5X06TouchStart(Touch:PTouchDevice):LongWord;
{Implementation of TouchDeviceStart API for FT5x06 Touch device}
{Note: Not intended to be called directly by applications, use TouchDeviceStart instead}
var
 Data:array[0..1] of Byte;
 Properties:TI2CProperties;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Touch}
 if Touch = nil then Exit;

 {$IF DEFINED(FT5X06_DEBUG) or DEFINED(TOUCH_DEBUG)}
 if TOUCH_LOG_ENABLED then TouchLogDebug(Touch,'FT5X06: Touch Start');
 {$ENDIF}

 {Check I2C Device}
 Result:=I2CDeviceGetProperties(PFT5X06Touch(Touch).I2C,@Properties);
 if Result <> ERROR_SUCCESS then Exit;

 {Start I2C Device}
 Result:=I2CDeviceStart(PFT5X06Touch(Touch).I2C,Min(FT5X06_I2C_RATE,Properties.MaxClock));
 if Result <> ERROR_SUCCESS then Exit;

 {Identify Device}
 Result:=FT5X06Identify(PFT5X06Touch(Touch));
 if Result <> ERROR_SUCCESS then Exit;

 {Dummy Read}
 {EP0700MLP1 device returns invalid data on first read}
 Data[0]:=$fc;
 Data[1]:=$00;
 FT5X06ReadWrite(PFT5X06Touch(Touch),2,@Data,2,@Data);

 {Get Registers}
 Result:=FT5X06GetRegisters(PFT5X06Touch(Touch));
 if Result <> ERROR_SUCCESS then Exit;

 {Set Defaults}
 Result:=FT5X06SetDefaults(PFT5X06Touch(Touch));
 if Result <> ERROR_SUCCESS then Exit;

 {Get Parameters}
 Result:=FT5X06GetParameters(PFT5X06Touch(Touch));
 if Result <> ERROR_SUCCESS then Exit;

 {Update Configuration}
 Result:=FT5X06UpdateConfig(PFT5X06Touch(Touch));
 if Result <> ERROR_SUCCESS then Exit;

 {Check IRQ}
 if (PFT5X06Touch(Touch).IRQ.GPIO <> nil) and (PFT5X06Touch(Touch).IRQ.Pin <> GPIO_PIN_UNKNOWN) then
  begin
   {Setup IRQ (GPIO)}
   if GPIODeviceFunctionSelect(PFT5X06Touch(Touch).IRQ.GPIO,PFT5X06Touch(Touch).IRQ.Pin,PFT5X06Touch(Touch).IRQ.Func) <> ERROR_SUCCESS then
    begin
     Result:=ERROR_OPERATION_FAILED;
     Exit;
    end;
   if GPIODevicePullSelect(PFT5X06Touch(Touch).IRQ.GPIO,PFT5X06Touch(Touch).IRQ.Pin,PFT5X06Touch(Touch).IRQ.Pull) <> ERROR_SUCCESS then
    begin
     Result:=ERROR_OPERATION_FAILED;
     Exit;
    end;

   {Create GPIO Event}
   if GPIODeviceInputEvent(PFT5X06Touch(Touch).IRQ.GPIO,PFT5X06Touch(Touch).IRQ.Pin,PFT5X06Touch(Touch).IRQ.Trigger,GPIO_EVENT_FLAG_NONE,INFINITE,TGPIOCallback(FT5X06TouchCallback),Touch) <> ERROR_SUCCESS then
    begin
     Result:=ERROR_OPERATION_FAILED;
     Exit;
    end;
  end
 else
  begin
   {Create Polling Timer}
   PFT5X06Touch(Touch).Timer:=TimerCreateEx(FT5X06_POLL_INTERVAL_MS,TIMER_STATE_ENABLED,TIMER_FLAG_WORKER,TTimerEvent(FT5X06TouchTimer),Touch);
  end;

 {Return Result}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function FT5X06TouchStop(Touch:PTouchDevice):LongWord;
{Implementation of TouchDeviceStop API for FT5x06 Touch device}
{Note: Not intended to be called directly by applications, use TouchDeviceStop instead}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Touch}
 if Touch = nil then Exit;

 {$IF DEFINED(FT5X06_DEBUG) or DEFINED(TOUCH_DEBUG)}
 if TOUCH_LOG_ENABLED then TouchLogDebug(Touch,'FT5X06: Touch Stop');
 {$ENDIF}

 {Check IRQ}
 if (PFT5X06Touch(Touch).IRQ.GPIO <> nil) and (PFT5X06Touch(Touch).IRQ.Pin <> GPIO_PIN_UNKNOWN) then
  begin
   {Cancel GPIO Event}
   GPIODeviceInputCancel(PFT5X06Touch(Touch).IRQ.GPIO,PFT5X06Touch(Touch).IRQ.Pin);
  end
 else
  begin
   {Cancel Polling Timer}
   TimerDestroy(PFT5X06Touch(Touch).Timer);
  end;

 {Return Result}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function FT5X06TouchUpdate(Touch:PTouchDevice):LongWord;
{Implementation of TouchDeviceUpdate API for FT5x06 Touch device}
{Note: Not intended to be called directly by applications, use TouchDeviceUpdate instead}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Touch}
 if Touch = nil then Exit;

 {$IF DEFINED(FT5X06_DEBUG) or DEFINED(TOUCH_DEBUG)}
 if TOUCH_LOG_ENABLED then TouchLogDebug(Touch,'FT5X06: Touch Update');
 {$ENDIF}

 {Acquire Lock}
 if MutexLock(Touch.Lock) = ERROR_SUCCESS then
  begin
   try
    {Update Configuration}
    Result:=FT5X06UpdateConfig(PFT5X06Touch(Touch));
   finally
    {Release the Lock}
    MutexUnlock(Touch.Lock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
   Exit;
  end;
end;

{==============================================================================}

procedure FT5X06TouchTimer(Touch:PFT5X06Touch);
{Touch device timer event handler for FT5x06 Touch device}
{Note: Not intended to be called directly by applications}
begin
 {}
 {Check Touch}
 if Touch = nil then Exit;

 {Acquire Lock}
 if MutexLock(Touch.Touch.Lock) = ERROR_SUCCESS then
  begin
   {$IF DEFINED(FT5X06_DEBUG) or DEFINED(TOUCH_DEBUG)}
   if TOUCH_LOG_ENABLED then TouchLogDebug(@Touch.Touch,'FT5X06: Touch Timer');
   {$ENDIF}

   {Disable Timer}
   TimerDisable(Touch.Timer);

   {Process Events}
   FT5X06ProcessEvents(Touch,True);

   {Enable Timer}
   TimerEnable(Touch.Timer);

   {Release Lock}
   MutexUnlock(Touch.Touch.Lock);
  end;
end;

{==============================================================================}

procedure FT5X06TouchCallback(Touch:PFT5X06Touch;Pin,Trigger:LongWord);
{Touch device event callback (Interrupt) handler for FT5x06 Touch device}
{Note: Not intended to be called directly by applications}
begin
 {}
 {Check Touch}
 if Touch = nil then Exit;

 {Acquire Lock}
 if MutexLock(Touch.Touch.Lock) = ERROR_SUCCESS then
  begin
   {$IF DEFINED(FT5X06_DEBUG) or DEFINED(TOUCH_DEBUG)}
   if TOUCH_LOG_ENABLED then TouchLogDebug(@Touch.Touch,'FT5X06: Touch Callback');
   {$ENDIF}

   {Process Events}
   FT5X06ProcessEvents(Touch,False);

   {Reregister Event}
   if GPIODeviceInputEvent(Touch.IRQ.GPIO,Touch.IRQ.Pin,Touch.IRQ.Trigger,GPIO_EVENT_FLAG_NONE,INFINITE,TGPIOCallback(FT5X06TouchCallback),Touch) <> ERROR_SUCCESS then
    begin
     if TOUCH_LOG_ENABLED then TouchLogError(@Touch.Touch,'FT5X06: Failed to re-register touch callback');
    end;

   {Release Lock}
   MutexUnlock(Touch.Touch.Lock);
  end;
end;

{==============================================================================}
{==============================================================================}
{FT5x06 Helper Functions}

{==============================================================================}
{==============================================================================}
{FT5x06 Internal Functions}
function FT5X06ReadWrite(Touch:PFT5X06Touch;WriteLen:Word;WriteData:PByte;ReadLen:Word;ReadData:PByte):LongWord;
var
 Count:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Touch}
 if Touch = nil then Exit;

 {$IF DEFINED(FT5X06_DEBUG) or DEFINED(TOUCH_DEBUG)}
 if TOUCH_LOG_ENABLED then TouchLogDebug(@Touch.Touch,'FT5X06: Read Write (Write Len=' + IntToStr(WriteLen) + ' Read Len=' + IntToStr(ReadLen) + ')');
 {$ENDIF}

 {Check Parameters}
 if (WriteLen = 0) and (ReadLen = 0) then Exit;
 if (WriteData = nil) and (ReadData = nil) then Exit;

 {Check Write Len}
 if (WriteLen > 0) and (ReadLen > 0) then
  begin
   {Write Read to I2C}
   Result:=I2CDeviceWriteRead(Touch.I2C,Touch.Address,WriteData,WriteLen,ReadData,ReadLen,Count);
  end
 else if WriteLen > 0 then
  begin
   {Write to I2C}
   Result:=I2CDeviceWrite(Touch.I2C,Touch.Address,WriteData,WriteLen,Count);
  end
 else if ReadLen > 0 then
  begin
   {Read to I2C}
   Result:=I2CDeviceRead(Touch.I2C,Touch.Address,ReadData,ReadLen,Count);
  end;
end;

{==============================================================================}

function FT5X06RegisterWrite(Touch:PFT5X06Touch;Reg,Value:Byte):LongWord;
var
 Data:array[0..3] of Byte;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Touch}
 if Touch = nil then Exit;

 {$IF DEFINED(FT5X06_DEBUG) or DEFINED(TOUCH_DEBUG)}
 if TOUCH_LOG_ENABLED then TouchLogDebug(@Touch.Touch,'FT5X06: Register Write (Reg=' + IntToHex(Reg,2) + ' Value=' + IntToStr(Value) + ')');
 {$ENDIF}

 {Check Version}
 case Touch.Version of
  FT5X06_EDT_M06:begin
    Data[0]:=$fc;
    Data[1]:=Reg and $3f;
    Data[2]:=Value;
    Data[3]:=Data[0] xor Data[1] xor Data[2];

    {Write Data}
    Result:=FT5X06ReadWrite(Touch,4,@Data,0,nil);
   end;
  FT5X06_EDT_M09,
  FT5X06_EDT_M12,
  FT5X06_EV_FT,
  FT5X06_GENERIC_FT:begin
    Data[0]:=Reg;
    Data[1]:=Value;

    {Write Data}
    Result:=FT5X06ReadWrite(Touch,2,@Data,0,nil);
   end;
 end;
end;

{==============================================================================}

function FT5X06RegisterRead(Touch:PFT5X06Touch;Reg:Byte;var Value:Byte):LongWord;
var
 ReadData:array[0..1] of Byte;
 WriteData:array[0..1] of Byte;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Set Defaults}
 Value:=0;

 {Check Touch}
 if Touch = nil then Exit;

 {$IF DEFINED(FT5X06_DEBUG) or DEFINED(TOUCH_DEBUG)}
 if TOUCH_LOG_ENABLED then TouchLogDebug(@Touch.Touch,'FT5X06: Register Read (Reg=' + IntToHex(Reg,2) + ')');
 {$ENDIF}

 {Check Version}
 case Touch.Version of
  FT5X06_EDT_M06:begin
    WriteData[0]:=$fc;
    WriteData[1]:=(Reg and $3f) or $40;

    {Write Read Data}
    Result:=FT5X06ReadWrite(Touch,2,@WriteData,2,@ReadData);
    if Result <> ERROR_SUCCESS then Exit;

    {Check CRC}
    if (WriteData[0] xor WriteData[1] xor ReadData[0]) <> ReadData[1] then
     begin
      if TOUCH_LOG_ENABLED then TouchLogError(@Touch.Touch,'FT5X06: CRC error on read (Expected: ' + IntToHex(WriteData[0] xor WriteData[1] xor ReadData[0],2) + ' Received: ' + IntToHex(ReadData[1],2));

      Result:=ERROR_INVALID_DATA;
      Exit;
     end;

    {Return Value}
    Value:=ReadData[0];
   end;
  FT5X06_EDT_M09,
  FT5X06_EDT_M12,
  FT5X06_EV_FT,
  FT5X06_GENERIC_FT:begin
    WriteData[0]:=Reg;

    {Write Read Data}
    Result:=FT5X06ReadWrite(Touch,1,@WriteData,1,@ReadData);
    if Result <> ERROR_SUCCESS then Exit;

    {Return Value}
    Value:=ReadData[0];
   end;
 end;
end;

{==============================================================================}

function FT5X06Identify(Touch:PFT5X06Touch):LongWord;
var
 Id:Byte;
 Reg:Byte;
 Data:array[0..FT5X06_EDT_NAME_LEN - 1] of Byte;
 ModelName:array[0..FT5X06_EDT_NAME_LEN - 1] of Char;
 FirmwareID:array[0..FT5X06_EDT_NAME_LEN - 1] of Char;
 FirmwareVersion:PChar;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Touch}
 if Touch = nil then Exit;

 {$IF DEFINED(FT5X06_DEBUG) or DEFINED(TOUCH_DEBUG)}
 if TOUCH_LOG_ENABLED then TouchLogDebug(@Touch.Touch,'FT5X06: Identify');
 {$ENDIF}

 {Read Model Name}
 FillChar(Data[0],SizeOf(Data),0);
 Reg:=$BB;
 Result:=FT5X06ReadWrite(Touch,1,@Reg,FT5X06_EDT_NAME_LEN - 1,@Data);
 if Result <> ERROR_SUCCESS then Exit;

 {Copy Model Name}
 System.Move(Data[0],ModelName[0],FT5X06_EDT_NAME_LEN);

 {Check Model Name}
 {M06 starts with a response byte, M12 gives the data directly}
 {M09 and Generic do not provide model number information}
 if StrLIComp(PChar(@ModelName[1]),'EP0',3) = 0 then
  begin
   {Version M06}
   Touch.Version:=FT5X06_EDT_M06;

   {Remove last '$' end marker}
   ModelName[FT5X06_EDT_NAME_LEN - 1]:=#0;
   if ModelName[FT5X06_EDT_NAME_LEN - 2] = '$' then ModelName[FT5X06_EDT_NAME_LEN - 2]:=#0;

   {Look for Model/Version separator}
   FirmwareVersion:=StrScan(PChar(@ModelName[1]),'*');
   if FirmwareVersion <> nil then
    begin
     FirmwareVersion^:=#0;

     Inc(FirmwareVersion);
    end;

   {Store Model Name and Firmware Version}
   Touch.ModelName:=PChar(@ModelName[1]);
   Touch.FirmwareVersion:='';
   if FirmwareVersion <> nil then Touch.FirmwareVersion:=FirmwareVersion;
  end
 else if StrLIComp(PChar(@ModelName[0]),'EP0',3) = 0 then
  begin
   {Version M12}
   Touch.Version:=FT5X06_EDT_M12;

   {Remove last '$' end marker}
   ModelName[FT5X06_EDT_NAME_LEN - 2]:=#0;
   if ModelName[FT5X06_EDT_NAME_LEN - 3] = '$' then ModelName[FT5X06_EDT_NAME_LEN - 3]:=#0;

   {Look for Model/Version separator}
   FirmwareVersion:=StrScan(PChar(@ModelName[0]),'*');
   if FirmwareVersion <> nil then
    begin
     FirmwareVersion^:=#0;

     Inc(FirmwareVersion);
    end;

   {Store Model Name and Firmware Version}
   Touch.ModelName:=PChar(@ModelName[0]);
   Touch.FirmwareVersion:='';
   if FirmwareVersion <> nil then Touch.FirmwareVersion:=FirmwareVersion;
  end
 else
  begin
   {If it is not an EDT M06/M12 touchscreen use the Firmware ID and CTPM Vendor ID}
   Touch.Version:=FT5X06_GENERIC_FT;

   {Read Firmware ID}
   FillChar(Data[0],SizeOf(Data),0);
   Reg:=$A6;
   Result:=FT5X06ReadWrite(Touch,1,@Reg,2,@Data);
   if Result <> ERROR_SUCCESS then Exit;

   {Copy Firmware ID}
   System.Move(Data[0],FirmwareID[0],FT5X06_EDT_NAME_LEN);

   {Read CTPM Vendor ID}
   FillChar(Data[0],SizeOf(Data),0);
   Reg:=$A8;
   Result:=FT5X06ReadWrite(Touch,1,@Reg,1,@Data);
   if Result <> ERROR_SUCCESS then Exit;

   {Check CTPM Vendor ID}
   Id:=Data[0];
   case Id of
    $35,      {EDT EP0350M09}
    $43,      {EDT EP0430M09}
    $50,      {EDT EP0500M09}
    $57,      {EDT EP0570M09}
    $70:begin {EDT EP0700M09}
      {Version M09}
      Touch.Version:=FT5X06_EDT_M09;

      {Get Model Name}
      ModelName:='EP0' + IntToStr(Id shr 4) + IntToStr(Id and $0f) + '0M09';
     end;
    $a1:begin {EDT EP1010ML00}
      {Version M09}
      Touch.Version:=FT5X06_EDT_M09;

      {Get Model Name}
      ModelName:='EP' + IntToStr(Id shr 4) + IntToStr(Id and $0f) + '0ML00';
     end;
    $5a:begin {Solomon Goldentek Display}
      {Version GENERIC_FT}

      {Get Model Name}
      ModelName:='GKTW50SCED1R0';
     end;
    $59:begin {Evervision Display with FT5xx6 TS}
      {Version EV_FT}
      Touch.Version:=FT5X06_EV_FT;

      {Read Firmware ID}
      FillChar(Data[0],SizeOf(Data),0);
      Reg:=$53;
      Result:=FT5X06ReadWrite(Touch,1,@Reg,1,@Data);
      if Result <> ERROR_SUCCESS then Exit;

      {Copy Firmware ID}
      System.Move(Data[0],FirmwareID[0],FT5X06_EDT_NAME_LEN);

      {Get Model Name}
      ModelName:='EVERVISION-FT5726NEi';
     end;
    else
     begin
      {Version GENERIC_FT}

      {Get Firmware ID}
      FirmwareID:='';

      {Get Model Name}
      ModelName:='Generic FT5X06 (' + IntToHex(Id,2)  + ')';
     end;
   end;

   {Store Model Name and Firmware Version}
   Touch.ModelName:=PChar(@ModelName[0]);
   Touch.FirmwareVersion:=PChar(@FirmwareID[0]);
  end;

 {$IF DEFINED(FT5X06_DEBUG) or DEFINED(TOUCH_DEBUG)}
 if TOUCH_LOG_ENABLED then TouchLogDebug(@Touch.Touch,'FT5X06:  Version: ' + IntToStr(Touch.Version) + ' Model Name: ' + Touch.ModelName + ' Firmware Version: ' + Touch.FirmwareVersion);
 {$ENDIF}

 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function FT5X06GetRegisters(Touch:PFT5X06Touch):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Touch}
 if Touch = nil then Exit;

 {$IF DEFINED(FT5X06_DEBUG) or DEFINED(TOUCH_DEBUG)}
 if TOUCH_LOG_ENABLED then TouchLogDebug(@Touch.Touch,'FT5X06: Get Registers');
 {$ENDIF}

 {Check Version}
 case Touch.Version of
  FT5X06_EDT_M06:begin
    Touch.Registers.Threshold:=FT5X06_WORK_REGISTER_THRESHOLD;
    Touch.Registers.ReportRate:=FT5X06_WORK_REGISTER_REPORT_RATE;
    Touch.Registers.Gain:=FT5X06_WORK_REGISTER_GAIN;
    Touch.Registers.Offset:=FT5X06_WORK_REGISTER_OFFSET;
    Touch.Registers.OffsetX:=FT5X06_NO_REGISTER;
    Touch.Registers.OffsetY:=FT5X06_NO_REGISTER;
    Touch.Registers.NumX:=FT5X06_WORK_REGISTER_NUM_X;
    Touch.Registers.NumY:=FT5X06_WORK_REGISTER_NUM_Y;
   end;
  FT5X06_EDT_M09,
  FT5X06_EDT_M12:begin
    Touch.Registers.Threshold:=FT5X06_M09_REGISTER_THRESHOLD;
    Touch.Registers.ReportRate:=FT5X06_NO_REGISTER;
    Touch.Registers.Gain:=FT5X06_M09_REGISTER_GAIN;
    Touch.Registers.Offset:=FT5X06_M09_REGISTER_OFFSET;
    Touch.Registers.OffsetX:=FT5X06_NO_REGISTER;
    Touch.Registers.OffsetY:=FT5X06_NO_REGISTER;
    Touch.Registers.NumX:=FT5X06_M09_REGISTER_NUM_X;
    Touch.Registers.NumY:=FT5X06_M09_REGISTER_NUM_Y;
   end;
  FT5X06_EV_FT:begin
    Touch.Registers.Threshold:=FT5X06_EV_REGISTER_THRESHOLD;
    Touch.Registers.ReportRate:=FT5X06_NO_REGISTER;
    Touch.Registers.Gain:=FT5X06_EV_REGISTER_GAIN;
    Touch.Registers.Offset:=FT5X06_NO_REGISTER;
    Touch.Registers.OffsetX:=FT5X06_EV_REGISTER_OFFSET_X;
    Touch.Registers.OffsetY:=FT5X06_EV_REGISTER_OFFSET_Y;
    Touch.Registers.NumX:=FT5X06_NO_REGISTER;
    Touch.Registers.NumY:=FT5X06_NO_REGISTER;
   end;
  FT5X06_GENERIC_FT:begin
    {These may not be accurate}
    Touch.Registers.Threshold:=FT5X06_M09_REGISTER_THRESHOLD;
    Touch.Registers.ReportRate:=FT5X06_NO_REGISTER;
    Touch.Registers.Gain:=FT5X06_M09_REGISTER_GAIN;
    Touch.Registers.Offset:=FT5X06_M09_REGISTER_OFFSET;
    Touch.Registers.OffsetX:=FT5X06_NO_REGISTER;
    Touch.Registers.OffsetY:=FT5X06_NO_REGISTER;
    Touch.Registers.NumX:=FT5X06_NO_REGISTER;
    Touch.Registers.NumY:=FT5X06_NO_REGISTER;
   end;
 end;

 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function FT5X06SetDefaults(Touch:PFT5X06Touch):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Touch}
 if Touch = nil then Exit;

 {$IF DEFINED(FT5X06_DEBUG) or DEFINED(TOUCH_DEBUG)}
 if TOUCH_LOG_ENABLED then TouchLogDebug(@Touch.Touch,'FT5X06: Set Defaults');
 {$ENDIF}

 {Check Threshold}
 if Touch.Parameters.Threshold <> 0 then
  begin
   FT5X06RegisterWrite(Touch,Touch.Registers.Threshold,Touch.Parameters.Threshold);
  end;

 {Check Gain}
 if Touch.Parameters.Gain <> 0 then
  begin
   FT5X06RegisterWrite(Touch,Touch.Registers.Gain,Touch.Parameters.Gain);
  end;

 {Check Offset}
 if Touch.Parameters.Offset <> 0 then
  begin
   if Touch.Registers.Offset <> FT5X06_NO_REGISTER then
    begin
     FT5X06RegisterWrite(Touch,Touch.Registers.Offset,Touch.Parameters.Offset);
    end;
  end;

 {Check Offset X}
 if Touch.Parameters.OffsetX <> 0 then
  begin
   if Touch.Registers.OffsetX <> FT5X06_NO_REGISTER then
    begin
     FT5X06RegisterWrite(Touch,Touch.Registers.OffsetX,Touch.Parameters.OffsetX);
    end;
  end;

 {Check Offset Y}
 if Touch.Parameters.OffsetY <> 0 then
  begin
   if Touch.Registers.OffsetY <> FT5X06_NO_REGISTER then
    begin
     FT5X06RegisterWrite(Touch,Touch.Registers.OffsetY,Touch.Parameters.OffsetY);
    end;
  end;

 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function FT5X06GetParameters(Touch:PFT5X06Touch):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Touch}
 if Touch = nil then Exit;

 {$IF DEFINED(FT5X06_DEBUG) or DEFINED(TOUCH_DEBUG)}
 if TOUCH_LOG_ENABLED then TouchLogDebug(@Touch.Touch,'FT5X06: Get Parameters');
 {$ENDIF}

 {Get Threshold}
 FT5X06RegisterRead(Touch,Touch.Registers.Threshold,Touch.Parameters.Threshold);

 {Get Gain}
 FT5X06RegisterRead(Touch,Touch.Registers.Gain,Touch.Parameters.Gain);

 {Get Offset}
 if Touch.Registers.Offset <> FT5X06_NO_REGISTER then
  begin
   FT5X06RegisterRead(Touch,Touch.Registers.Offset,Touch.Parameters.Offset);
  end;

 {Get Offset X}
 if Touch.Registers.OffsetX <> FT5X06_NO_REGISTER then
  begin
   FT5X06RegisterRead(Touch,Touch.Registers.OffsetX,Touch.Parameters.OffsetX);
  end;

 {Get Offset Y}
 if Touch.Registers.OffsetY <> FT5X06_NO_REGISTER then
  begin
   FT5X06RegisterRead(Touch,Touch.Registers.OffsetY,Touch.Parameters.OffsetY);
  end;

 {Get Report Rate}
 if Touch.Registers.ReportRate <> FT5X06_NO_REGISTER then
  begin
   FT5X06RegisterRead(Touch,Touch.Registers.ReportRate,Touch.Parameters.ReportRate);
  end;

 {Check Version}
 case Touch.Version of
  FT5X06_EDT_M06,
  FT5X06_EDT_M09,
  FT5X06_EDT_M12:begin
    {Get NumX}
    FT5X06RegisterRead(Touch,Touch.Registers.NumX,Touch.Parameters.NumX);

    {Get NumY}
    FT5X06RegisterRead(Touch,Touch.Registers.NumY,Touch.Parameters.NumY);
   end;
  else
   begin
    {Get NumX and Y}
    Touch.Parameters.NumX:=$FF;
    Touch.Parameters.NumY:=$FF;
   end;
 end;

 {$IF DEFINED(FT5X06_DEBUG) or DEFINED(TOUCH_DEBUG)}
 if TOUCH_LOG_ENABLED then
  begin
   TouchLogDebug(@Touch.Touch,'FT5X06:  Threshold: ' + IntToStr(Touch.Parameters.Threshold));
   TouchLogDebug(@Touch.Touch,'FT5X06:  Gain: ' + IntToStr(Touch.Parameters.Gain));
   TouchLogDebug(@Touch.Touch,'FT5X06:  Offset: ' + IntToStr(Touch.Parameters.Offset));
   TouchLogDebug(@Touch.Touch,'FT5X06:  OffsetX: ' + IntToStr(Touch.Parameters.OffsetX));
   TouchLogDebug(@Touch.Touch,'FT5X06:  OffsetY: ' + IntToStr(Touch.Parameters.OffsetY));
   TouchLogDebug(@Touch.Touch,'FT5X06:  ReportRate: ' + IntToStr(Touch.Parameters.ReportRate));
   TouchLogDebug(@Touch.Touch,'FT5X06:  NumX: ' + IntToStr(Touch.Parameters.NumX));
   TouchLogDebug(@Touch.Touch,'FT5X06:  NumY: ' + IntToStr(Touch.Parameters.NumY));
  end;
 {$ENDIF}

 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function FT5X06CheckCRC(Touch:PFT5X06Touch;Data:PByte;Size:LongWord):LongWord;
var
 CRC:Byte;
 Count:LongInt;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Touch}
 if Touch = nil then Exit;

 {$IF DEFINED(FT5X06_DEBUG) or DEFINED(TOUCH_DEBUG)}
 if TOUCH_LOG_ENABLED then TouchLogDebug(@Touch.Touch,'FT5X06: Check CRC');
 {$ENDIF}

 {Check Parameters}
 if Data = nil then Exit;
 if Size < 1 then Exit;

 {Calculate CRC}
 CRC:=0;
 for Count:=0 to Size - 1 do
  begin
   CRC:=CRC xor Data[Count];
  end;

 {Check CRC}
 if CRC <> Data[Size - 1] then
  begin
   if TOUCH_LOG_ENABLED then TouchLogError(@Touch.Touch,'FT5X06: CRC check error (Expected: ' + IntToHex(CRC,2) + ' Received: ' + IntToHex(Data[Size - 1],2));

   Result:=ERROR_INVALID_DATA;
   Exit;
  end;

 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function FT5X06UpdateConfig(Touch:PFT5X06Touch):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Touch}
 if Touch = nil then Exit;

 {$IF DEFINED(FT5X06_DEBUG) or DEFINED(TOUCH_DEBUG)}
 if TOUCH_LOG_ENABLED then TouchLogDebug(@Touch.Touch,'FT5X06: Update Config');
 {$ENDIF}

 {Check Version}
 case Touch.Version of
  FT5X06_EDT_M06,
  FT5X06_EDT_M09,
  FT5X06_EDT_M12:begin
    {Setup Max X and Y}
    Touch.MaxX:=Touch.Width - 1; {Touch.Parameters.NumX * 64 - 1;}
    Touch.MaxY:=Touch.Height - 1; {Touch.Parameters.NumY * 64 - 1;}
   end;
  else
   begin
    {Unknown maximum values, use screen size}
    Touch.MaxX:=Touch.Width - 1;
    Touch.MaxY:=Touch.Height - 1;
   end;
 end;

 {Check Rotation}
 case Touch.Touch.Properties.Rotation of
  TOUCH_ROTATION_0,TOUCH_ROTATION_180:begin
    {Update Width and Height}
    Touch.Touch.Properties.Width:=Touch.Width;
    Touch.Touch.Properties.Height:=Touch.Height;

    {Update Max X and Y}
    if (Touch.Touch.Device.DeviceFlags and TOUCH_FLAG_SWAP_MAX_XY) = 0 then
     begin
      Touch.Touch.Properties.MaxX:=Touch.MaxX;
      Touch.Touch.Properties.MaxY:=Touch.MaxY;
     end
    else
     begin
      Touch.Touch.Properties.MaxX:=Touch.MaxY;
      Touch.Touch.Properties.MaxY:=Touch.MaxX;
     end;
   end;
  TOUCH_ROTATION_90,TOUCH_ROTATION_270:begin
    {Update Width and Height}
    Touch.Touch.Properties.Width:=Touch.Height;
    Touch.Touch.Properties.Height:=Touch.Width;

    {Update Max X and Y}
    if (Touch.Touch.Device.DeviceFlags and TOUCH_FLAG_SWAP_MAX_XY) = 0 then
     begin
      Touch.Touch.Properties.MaxX:=Touch.MaxY;
      Touch.Touch.Properties.MaxY:=Touch.MaxX;
     end
    else
     begin
      Touch.Touch.Properties.MaxX:=Touch.MaxX;
      Touch.Touch.Properties.MaxY:=Touch.MaxY;
     end;
   end;
 end;

 {Update Max Points}
 Touch.Touch.Properties.MaxPoints:=Touch.MaxPoints;

 {$IF DEFINED(FT5X06_DEBUG) or DEFINED(TOUCH_DEBUG)}
 if TOUCH_LOG_ENABLED then TouchLogDebug(@Touch.Touch,'FT5X06:  Width: ' + IntToStr(Touch.Touch.Properties.Width) + ' Height: ' + IntToStr(Touch.Touch.Properties.Height));
 if TOUCH_LOG_ENABLED then TouchLogDebug(@Touch.Touch,'FT5X06:  Max Points: ' + IntToStr(Touch.Touch.Properties.MaxPoints) + ' Max X: ' + IntToStr(Touch.Touch.Properties.MaxX) + ' Max Y: ' + IntToStr(Touch.Touch.Properties.MaxY));
 {$ENDIF}

 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function FT5X06ProcessEvents(Touch:PFT5X06Touch;Polling:Boolean):LongWord;
var
 Reg:Byte;
 X:Word;
 Y:Word;
 Id:Byte;
 Temp:Word;
 Event:Byte;
 Count:LongInt;
 Total:LongInt;
 Status:LongWord;
 Offset:LongWord;
 CRCSize:LongWord;
 DataSize:LongWord;
 PointSize:LongWord;
 Buffer:PByte;
 TouchData:PTouchData;
 MouseData:TMouseData;
 Data:array[0..63] of Byte;
 ModifiedPoints:LongWord;
 ReleasedPoints:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Touch}
 if Touch = nil then Exit;

 {$IF DEFINED(FT5X06_DEBUG) or DEFINED(TOUCH_DEBUG)}
 if TOUCH_LOG_ENABLED then TouchLogDebug(@Touch.Touch,'FT5X06: Process Events');
 {$ENDIF}

 {Check Version}
 case Touch.Version of
  FT5X06_EDT_M06:begin
    {Get Parameters}
    Reg:=$f9;
    Offset:=5;
    PointSize:=4;
    CRCSize:=1;
   end;
  FT5X06_EDT_M09,
  FT5X06_EDT_M12,
  FT5X06_EV_FT,
  FT5X06_GENERIC_FT:begin
    {Get Parameters}
    Reg:=$00;
    Offset:=3;
    PointSize:=6;
    CRCSize:=0;
   end;
  else
   Exit;
 end;

 {Setup Defaults}
 Total:=0;
 ModifiedPoints:=0;

 {Clear Mouse Data}
 FillChar(MouseData,SizeOf(TMouseData),0);

 {Read Touch Point Data}
 FillChar(Data[0],SizeOf(Data),0);
 DataSize:=(PointSize * Touch.MaxPoints) + Offset + CRCSize;

 Status:=FT5X06ReadWrite(Touch,1,@Reg,DataSize,@Data);
 if Status <> ERROR_SUCCESS then
  begin
   if TOUCH_LOG_ENABLED then TouchLogError(@Touch.Touch,'FT5X06: Failed to read touch point data');

   Exit;
  end;

 {M09/M12 does not send header or CRC}
 if Touch.Version = FT5X06_EDT_M06 then
  begin
   {Check Header}
   if (Data[0] <> $aa) or (Data[1] <> $aa) or (Data[2] <> DataSize) then
    begin
     if TOUCH_LOG_ENABLED then TouchLogError(@Touch.Touch,'FT5X06: Invalid header received');

     Exit;
    end;
   {Check CRC}
   if FT5X06CheckCRC(Touch,@Data,DataSize) <> ERROR_SUCCESS then
    begin
     if TOUCH_LOG_ENABLED and not(Polling) then TouchLogError(@Touch.Touch,'FT5X06: CRC error');

     Exit;
    end;

   Total:=Touch.MaxPoints;
  end
 else
  begin
   {Register 2 is TD_STATUS, containing the number of touch points}
   Total:=Min(Data[2] and $0f,Touch.MaxPoints);
  end;

 {Process Touches}
 for Count:=0 to Total - 1 do
  begin
   {Get Next Point}
   Buffer:=@Data[(Count * PointSize) + Offset];

   {Get Event}
   Event:=Buffer[0] shr 6;

   {Ignore TOUCH_EVENT_RESERVED}
   if Event = FT5X06_TOUCH_EVENT_RESERVED then Continue;

   {M06 sometimes sends bogus coordinates in TOUCH_EVENT_DOWN}
   if (Touch.Version = FT5X06_EDT_M06) and (Event = FT5X06_TOUCH_EVENT_DOWN) then Continue;

   {Get X and Y}
   X:=WordBEToN(PWord(Buffer)^) and $0fff;
   Y:=WordBEToN(PWord(Buffer + 2)^) and $0fff;

   {The FT5X26 sends the Y coordinate first}
   if (Touch.Version = FT5X06_EV_FT) then
    begin
     {Swap X/Y}
     Temp:=X;
     X:=Y;
     Y:=Temp;
    end;

   {Get Id}
   Id:=(Buffer[2] shr 4) and $0f;

   {Check Swap}
   if Touch.SwapReportXY or ((Touch.Touch.Device.DeviceFlags and TOUCH_FLAG_SWAP_XY) <> 0) then
    begin
     {Swap X/Y}
     Temp:=X;
     X:=Y;
     Y:=Temp;
    end;

   {Check Invert}
   if Touch.InvertReportX or ((Touch.Touch.Device.DeviceFlags and TOUCH_FLAG_INVERT_X) <> 0) then
    begin
     {Invert X}
     X:=Touch.MaxX - X;
    end;
   if Touch.InvertReportY or ((Touch.Touch.Device.DeviceFlags and TOUCH_FLAG_INVERT_Y) <> 0) then
    begin
     {Invert Y}
     Y:=Touch.MaxY - Y;
    end;

   {$IF DEFINED(FT5X06_DEBUG) or DEFINED(TOUCH_DEBUG)}
   if TOUCH_LOG_ENABLED then TouchLogDebug(@Touch.Touch,'FT5X06:  Id=' + IntToStr(Id) + ' X=' + IntToStr(X) + ' Y=' + IntToStr(Y) + ' Event=' + IntToStr(Event));
   {$ENDIF}

   {Store Point}
   ModifiedPoints:=ModifiedPoints or (1 shl Id);

   {Check Event}
   if (Event = FT5X06_TOUCH_EVENT_DOWN) or (Event = FT5X06_TOUCH_EVENT_ON) then
    begin
     {Check Flags}
     if (Touch.Touch.Device.DeviceFlags and TOUCH_FLAG_MOUSE_DATA) = 0 then
      begin
       {For touch report all points}
       {Update Statistics}
       Inc(Touch.Touch.ReceiveCount);

       {Check Buffer}
       if (Touch.Touch.Buffer.Count < TOUCH_BUFFER_SIZE) then
        begin
         TouchData:=@Touch.Touch.Buffer.Buffer[(Touch.Touch.Buffer.Start + Touch.Touch.Buffer.Count) mod TOUCH_BUFFER_SIZE];
         if TouchData <> nil then
          begin
           {Clear Touch Data}
           FillChar(TouchData^,SizeOf(TTouchData),0);

           {Update Touch Data}
           TouchData.Info:=TOUCH_FINGER;
           TouchData.PointID:=Id + 1;

           {Check Rotation}
           case Touch.Touch.Properties.Rotation of
            TOUCH_ROTATION_0:begin
              {No Change}
              TouchData.PositionX:=X;
              TouchData.PositionY:=Y;
             end;
            TOUCH_ROTATION_90:begin
              {Swap X and Y, Invert Y}
              TouchData.PositionX:=Y;
              TouchData.PositionY:=Touch.Touch.Properties.MaxY - X;
             end;
            TOUCH_ROTATION_180:begin
              {Invert X and Y}
              TouchData.PositionX:=Touch.Touch.Properties.MaxX - X;
              TouchData.PositionY:=Touch.Touch.Properties.MaxY - Y;
             end;
            TOUCH_ROTATION_270:begin
              {Swap X and Y, Invert X}
              TouchData.PositionX:=Touch.Touch.Properties.MaxX - Y;
              TouchData.PositionY:=X;
             end;
           end;
           TouchData.PositionZ:=0;

           {Check Event}
           if Assigned(Touch.Touch.Event) then
            begin
             {Event Parameter}
             TouchData.Parameter:=Touch.Touch.Parameter;

             {Event Callback}
             Touch.Touch.Event(@Touch.Touch,TouchData);
            end
           else
            begin
             {Update Count}
             Inc(Touch.Touch.Buffer.Count);

             {Signal Data Received}
             SemaphoreSignal(Touch.Touch.Buffer.Wait);
            end;
          end;
        end
       else
        begin
         if TOUCH_LOG_ENABLED then TouchLogError(@Touch.Touch,'FT5X06: Buffer overflow, packet discarded');

         {Update Statistics}
         Inc(Touch.Touch.BufferOverruns);
        end;
      end
     else
      begin
       {For mouse report the first point}
       if Id = 0 then
        begin
         {Update Statistics}
         Inc(Touch.Touch.ReceiveCount);

         {Create Mouse Data}
         MouseData.Buttons:=MOUSE_TOUCH_BUTTON or MOUSE_ABSOLUTE_X or MOUSE_ABSOLUTE_Y; {Touch Button, Absolute X and Y}

         {Check Rotation}
         case Touch.Touch.Properties.Rotation of
          TOUCH_ROTATION_0:begin
            {No Change}
            MouseData.OffsetX:=X;
            MouseData.OffsetY:=Y;
           end;
          TOUCH_ROTATION_90:begin
            {Swap X and Y, Invert Y}
            MouseData.OffsetX:=Y;
            MouseData.OffsetY:=Touch.Touch.Properties.MaxY - X;
           end;
          TOUCH_ROTATION_180:begin
            {Invert X and Y}
            MouseData.OffsetX:=Touch.Touch.Properties.MaxX - X;
            MouseData.OffsetY:=Touch.Touch.Properties.MaxY - Y;
           end;
          TOUCH_ROTATION_270:begin
            {Swap X and Y, Invert X}
            MouseData.OffsetX:=Touch.Touch.Properties.MaxX - Y;
            MouseData.OffsetY:=X;
           end;
         end;
         MouseData.OffsetWheel:=0;

         {Maximum X, Y and Wheel}
         MouseData.MaximumX:=Touch.Touch.Properties.MaxX;
         MouseData.MaximumY:=Touch.Touch.Properties.MaxY;
         MouseData.MaximumWheel:=0;

         {Write Mouse Data}
         if MouseWrite(@MouseData,SizeOf(TMouseData),1) <> ERROR_SUCCESS then
          begin
           if TOUCH_LOG_ENABLED then TouchLogError(@Touch.Touch,'FT5X06: Failed to write mouse data, packet discarded');

           {Update Statistics}
           Inc(Touch.Touch.ReceiveErrors);
          end;
        end;
      end;
    end;
  end;

 {$IF DEFINED(FT5X06_DEBUG) or DEFINED(TOUCH_DEBUG)}
 if TOUCH_LOG_ENABLED and (ModifiedPoints > 0) then TouchLogDebug(@Touch.Touch,'FT5X06:  Modified Points=' + IntToHex(ModifiedPoints,8));
 {$ENDIF}

 {Process Releases}
 ReleasedPoints:=Touch.LastPoints and not(ModifiedPoints);
 if ReleasedPoints > 0 then
  begin
   for Count:=0 to Touch.Touch.Properties.MaxPoints - 1 do
    begin
     if (ReleasedPoints and (1 shl Count)) <> 0 then
      begin
       {$IF DEFINED(FT5X06_DEBUG) or DEFINED(TOUCH_DEBUG)}
       if TOUCH_LOG_ENABLED then TouchLogDebug(@Touch.Touch,'FT5X06:  Released Id=' + IntToStr(Count));
       {$ENDIF}

       {Check Flags}
       if (Touch.Touch.Device.DeviceFlags and TOUCH_FLAG_MOUSE_DATA) = 0 then
        begin
         {For touch report all points}
         {Check Buffer}
         if (Touch.Touch.Buffer.Count < TOUCH_BUFFER_SIZE) then
          begin
           TouchData:=@Touch.Touch.Buffer.Buffer[(Touch.Touch.Buffer.Start + Touch.Touch.Buffer.Count) mod TOUCH_BUFFER_SIZE];
           if TouchData <> nil then
            begin
             {Clear Touch Data}
             FillChar(TouchData^,SizeOf(TTouchData),0);

             {Update Touch Data}
             TouchData.Info:=0;
             TouchData.PointID:=Count + 1;
             TouchData.PositionX:=TOUCH_X_UNKNOWN;
             TouchData.PositionY:=TOUCH_Y_UNKNOWN;
             TouchData.PositionZ:=TOUCH_Z_UNKNOWN;

             {Check Event}
             if Assigned(Touch.Touch.Event) then
              begin
               {Event Parameter}
               TouchData.Parameter:=Touch.Touch.Parameter;

               {Event Callback}
               Touch.Touch.Event(@Touch.Touch,TouchData);
              end
             else
              begin
               {Update Count}
               Inc(Touch.Touch.Buffer.Count);

               {Signal Data Received}
               SemaphoreSignal(Touch.Touch.Buffer.Wait);
              end;
            end;
          end
         else
          begin
           if TOUCH_LOG_ENABLED then TouchLogError(@Touch.Touch,'FT5X06: Buffer overflow, packet discarded');

           {Update Statistics}
           Inc(Touch.Touch.BufferOverruns);
          end;
        end
       else
        begin
         {For mouse report the first point}
         if Count = 0 then
          begin
           {Create Mouse Data (Release Event)}
           MouseData.Buttons:=0; {No Buttons}
           MouseData.OffsetX:=0; {No Offset X, Y or Wheel}
           MouseData.OffsetY:=0;
           MouseData.OffsetWheel:=0;

           {Maximum X, Y and Wheel}
           MouseData.MaximumX:=Touch.Touch.Properties.MaxX;
           MouseData.MaximumY:=Touch.Touch.Properties.MaxY;
           MouseData.MaximumWheel:=0;

           {Write Mouse Data}
           if MouseWrite(@MouseData,SizeOf(TMouseData),1) <> ERROR_SUCCESS then
            begin
             if TOUCH_LOG_ENABLED then TouchLogError(@Touch.Touch,'FT5X06: Failed to write mouse data, packet discarded');

             {Update Statistics}
             Inc(Touch.Touch.ReceiveErrors);
            end;
          end;
        end;
      end;
    end;
  end;

 {Save Last Points}
 Touch.LastPoints:=ModifiedPoints;

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