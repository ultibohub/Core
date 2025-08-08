{
Goodix I2C Touchscreen Driver.

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

  Linux - \drivers\input\touchscreen\goodix.c - Copyright (c) 2014 Red Hat Inc.

References
==========


Goodix I2C Touchscreen
======================

 The GOODIX I2C controllers are a range of capacitive touchscreen controllers
 that include multipoint touch support and are used by a variety of small LCD
 displays.

 The exact features and capability of each device can be found by checking the
 datasheets which can be obtained online.

 This driver is compatible with the following devices:

  goodix,gt1151
  goodix,gt5663
  goodix,gt5688
  goodix,gt911
  goodix,gt9110
  goodix,gt912
  goodix,gt9147
  goodix,gt917s
  goodix,gt927
  goodix,gt9271
  goodix,gt928
  goodix,gt9286
  goodix,gt967

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit GoodixTouch;

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
  Touch,
  Mouse,
  Keyboard,
  SysUtils;

{==============================================================================}
{Global definitions}
{$INCLUDE ..\core\GlobalDefines.inc}

{==============================================================================}
const
 {Goodix specific constants}
 GOODIX_TOUCH_DESCRIPTION = 'Goodix Touch Controller';  {Description of Goodix Touch device}

 GOODIX_MAX_HEIGHT = 4096;
 GOODIX_MAX_WIDTH = 4096;
 GOODIX_INT_TRIGGER = 1;
 GOODIX_CONTACT_SIZE = 8;
 GOODIX_MAX_CONTACT_SIZE = 9;
 GOODIX_MAX_CONTACTS = 10;
 GOODIX_MAX_KEYS = 7;

 GOODIX_CONFIG_MIN_LENGTH = 186;
 GOODIX_CONFIG_911_LENGTH = 186;
 GOODIX_CONFIG_967_LENGTH = 228;
 GOODIX_CONFIG_GT9X_LENGTH = 240;
 GOODIX_CONFIG_MAX_LENGTH = 240;

 {Goodix I2C constants}
 GOODIX_I2C_RATE = 400000;     {Default I2C clock rate}
 GOODIX_CONFIG_TIMEOUT = 3000; {Timeout to wait for firmware ready}

 {Goodix register constants}
 GOODIX_REG_COMMAND          = $8040;
 GOODIX_CMD_SCREEN_OFF       = $05;

 GOODIX_READ_COOR_ADDR       = $814E;
 GOODIX_GT1X_REG_CONFIG_DATA = $8050;
 GOODIX_GT9X_REG_CONFIG_DATA = $8047;
 GOODIX_REG_ID               = $8140;

 GOODIX_BUFFER_STATUS_READY = (1 shl 7);
 GOODIX_HAVE_KEY            = (1 shl 4);
 GOODIX_BUFFER_STATUS_TIMEOUT = 20;

 GOODIX_ID_MAX_LEN = 4;

 GOODIX_RESOLUTION_OFFSET   = 1;
 GOODIX_MAX_CONTACTS_OFFSET = 5;
 GOODIX_TRIGGER_OFFSET      = 6;

 GOODIX_IRQ_TRIGGERS:array[0..3] of LongWord = (GPIO_TRIGGER_RISING,GPIO_TRIGGER_FALLING,GPIO_TRIGGER_LOW,GPIO_TRIGGER_HIGH);

{==============================================================================}
type
 {Goodix specific types}
 PGOODIXTouch = ^TGOODIXTouch;
 TGOODIXTouch = record
  {Touch Properties}
  Touch:TTouchDevice;
  {I2C Properties}
  I2C:PI2CDevice;              {The I2C device this device is connected to}
  Address:Word;                {The I2C address of the device}
  {General Properties}
  IRQ:TGPIOInfo;               {The GPIO information for the IRQ line (Optional)}
  RST:TGPIOInfo;               {The GPIO information for the Reset line (Optional)}
  MaxX:Word;                   {Maximum X value from current configuration}
  MaxY:Word;                   {Maximum Y value from current configuration}
  Width:Word;                  {Screen width value supplied during create}
  Height:Word;                 {Screen height value supplied during create}
  MaxPoints:LongWord;          {Maximum touch points from current configuration}
  LastKeys:LongWord;           {Keys reported in last input report}
  LastPoints:LongWord;         {Points reported in last input report}
  {Goodix Properties}
  Id:String;                   {ID String for this device}
  Version:Word;                {Version number for this device}
  ConfigAddress:Word;          {Configuration data address}
  ConfigLength:LongWord;       {Configuration data length in bytes}
  ChecksumSize:LongWord;       {Configuration checksum size}
  ContactSize:LongWord;        {Size of each contact point coordinate in touch data}
  Configuration:array[0..GOODIX_CONFIG_MAX_LENGTH - 1] of Byte;
  KeyCodes:array[0..GOODIX_MAX_KEYS - 1] of Word;
  ScanCodes:array[0..GOODIX_MAX_KEYS - 1] of Word;
  SwapConfigXY:Boolean;        {If True swap the max X and Y values in the configuration}
  SwapReportXY:Boolean;        {If True swap the X and Y values in the input report}
  InvertReportX:Boolean;       {If True invert the X value in the input report}
  InvertReportY:Boolean;       {If True invert the Y value in the input report}
  ConfigFilename:String;       {The configuration filename to load during initialization}
  ResetController:Boolean;     {If True reset the controller during initialization}
 end;

 TGOODIXPointData = array[0..(2 + GOODIX_MAX_CONTACT_SIZE * GOODIX_MAX_CONTACTS) - 1] of Byte;

{==============================================================================}
var
 {Goodix specific variables}
 GOODIX_SWAP_CONFIG_XY:Boolean;
 GOODIX_SWAP_REPORT_XY:Boolean;
 GOODIX_INVERT_REPORT_X:Boolean;
 GOODIX_INVERT_REPORT_Y:Boolean;
 GOODIX_LOAD_CONFIG_FILE:String;
 GOODIX_RESET_CONTROLLER:Boolean;

{==============================================================================}
{Initialization Functions}
procedure GOODIXInit;{$IFDEF API_EXPORT_GOODIXTOUCH} stdcall; public name 'goodix_init';{$ENDIF}

{==============================================================================}
{Goodix Functions}
function GOODIXTouchCreate(I2C:PI2CDevice;Address:Word;Width,Height:LongWord;IRQ,RST:PGPIOInfo):PTouchDevice;{$IFDEF API_EXPORT_GOODIXTOUCH} stdcall; public name 'goodix_touch_create';{$ENDIF}
function GOODIXTouchDestroy(Touch:PTouchDevice):LongWord;{$IFDEF API_EXPORT_GOODIXTOUCH} stdcall; public name 'goodix_touch_destroy';{$ENDIF}

{==============================================================================}
{Goodix Touch Functions}
function GOODIXTouchStart(Touch:PTouchDevice):LongWord;
function GOODIXTouchStop(Touch:PTouchDevice):LongWord;

function GOODIXTouchUpdate(Touch:PTouchDevice):LongWord;

procedure GOODIXTouchCallback(Touch:PGOODIXTouch;Pin,Trigger:LongWord);

{==============================================================================}
{Goodix Helper Functions}

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {Goodix specific variables}
 GOODIXInitialized:Boolean;

{==============================================================================}
{==============================================================================}
{Forward Declarations}
function GOODIXReadI2C(Touch:PGOODIXTouch;Reg:Word;Data:Pointer;Size:LongWord):LongWord; forward;
function GOODIXWriteI2C(Touch:PGOODIXTouch;Reg:Word;Data:Pointer;Size:LongWord):LongWord; forward;

function GOODIXWriteByteI2C(Touch:PGOODIXTouch;Reg:Word;Value:Byte):LongWord; forward;

function GOODIXReset(Touch:PGOODIXTouch):LongWord; forward;
function GOODIXReadVersion(Touch:PGOODIXTouch):LongWord; forward;

function GOODIXGetChipData(Touch:PGOODIXTouch):LongWord; forward;

function GOODIXReadConfig(Touch:PGOODIXTouch):LongWord; forward;
function GOODIXUpdateConfig(Touch:PGOODIXTouch):LongWord; forward;

function GOODIXConfigureDevice(Touch:PGOODIXTouch):LongWord; forward;

function GOODIXProcessEvents(Touch:PGOODIXTouch):LongWord; forward;

function GOODIXReadInputReport(Touch:PGOODIXTouch;Data:PByte;var Count:LongInt):LongWord; forward;

function GOODIXCheckConfig8(Touch:PGOODIXTouch;Data:PByte;Size:LongWord):LongWord; forward;
function GOODIXCalcChecksum8(Touch:PGOODIXTouch):LongWord; forward;

function GOODIXCheckConfig16(Touch:PGOODIXTouch;Data:PByte;Size:LongWord):LongWord; forward;
function GOODIXCalcChecksum16(Touch:PGOODIXTouch):LongWord; forward;

function GOODIXCheckConfig(Touch:PGOODIXTouch;Data:PByte;Size:LongWord):LongWord; forward;
function GOODIXSendConfig(Touch:PGOODIXTouch;Data:PByte;Size:LongWord):LongWord; forward;

function GOODIXIntDirectionOutput(Touch:PGOODIXTouch;Level:LongWord):LongWord; forward;
function GOODIXIntDirectionInput(Touch:PGOODIXTouch):LongWord; forward;

function GOODIXIntSync(Touch:PGOODIXTouch):LongWord; forward;

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure GOODIXInit;{$IFDEF API_EXPORT_GOODIXTOUCH} stdcall;{$ENDIF}
{Initialize the Goodix Touch unit and parameters}

{Note: Called internally by other functions}
var
 WorkBool:LongBool;
 WorkBuffer:String;
begin
 {}
 {Check Initialized}
 if GOODIXInitialized then Exit;

 {Check Environment Variables}
 {GOODIX_SWAP_CONFIG_XY}
 WorkBool:=StrToBoolDef(EnvironmentGet('GOODIX_SWAP_CONFIG_XY'),GOODIX_SWAP_CONFIG_XY);
 if WorkBool <> GOODIX_SWAP_CONFIG_XY then GOODIX_SWAP_CONFIG_XY:=WorkBool;

 {GOODIX_SWAP_REPORT_XY}
 WorkBool:=StrToBoolDef(EnvironmentGet('GOODIX_SWAP_REPORT_XY'),GOODIX_SWAP_REPORT_XY);
 if WorkBool <> GOODIX_SWAP_REPORT_XY then GOODIX_SWAP_REPORT_XY:=WorkBool;

 {GOODIX_INVERT_REPORT_X}
 WorkBool:=StrToBoolDef(EnvironmentGet('GOODIX_INVERT_REPORT_X'),GOODIX_INVERT_REPORT_X);
 if WorkBool <> GOODIX_INVERT_REPORT_X then GOODIX_INVERT_REPORT_X:=WorkBool;

 {GOODIX_INVERT_REPORT_Y}
 WorkBool:=StrToBoolDef(EnvironmentGet('GOODIX_INVERT_REPORT_Y'),GOODIX_INVERT_REPORT_Y);
 if WorkBool <> GOODIX_INVERT_REPORT_Y then GOODIX_INVERT_REPORT_Y:=WorkBool;

 {GOODIX_LOAD_CONFIG_FILE}
 WorkBuffer:=EnvironmentGet('GOODIX_LOAD_CONFIG_FILE');
 if Length(WorkBuffer) <> 0 then GOODIX_LOAD_CONFIG_FILE:=WorkBuffer;

 {GOODIX_RESET_CONTROLLER}
 WorkBool:=StrToBoolDef(EnvironmentGet('GOODIX_RESET_CONTROLLER'),GOODIX_RESET_CONTROLLER);
 if WorkBool <> GOODIX_RESET_CONTROLLER then GOODIX_RESET_CONTROLLER:=WorkBool;

 GOODIXInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{Goodix Functions}
function GOODIXTouchCreate(I2C:PI2CDevice;Address:Word;Width,Height:LongWord;IRQ,RST:PGPIOInfo):PTouchDevice;{$IFDEF API_EXPORT_GOODIXTOUCH} stdcall;{$ENDIF}
{Create, register and start a new Goodix Touch device connected to the specified I2C device}
{I2C: The I2C device this Goodix Touch device is connected to}
{Address: The I2C address for this Goodix Touch device}
{Width: The width of the screen in pixels (When set at TOUCH_ROTATION_0)}
{Height: The height of the screen in pixels (When set at TOUCH_ROTATION_0)}
{IRQ: The GPIO information for the IRQ line (Optional)}
{Return: Pointer to the new Touch device or nil on failure}
var
 Status:LongWord;
 GOODIXTouch:PGOODIXTouch;
begin
 {}
 Result:=nil;

 {Initialize}
 GOODIXInit;

 {$IF DEFINED(GOODIX_DEBUG) or DEFINED(TOUCH_DEBUG)}
 if GPIO_LOG_ENABLED then GPIOLogDebug(nil,'GOODIX: Touch Create (Address=' + IntToHex(Address,4) + ' Width=' + IntToStr(Width) + ' Height=' + IntToStr(Height) + ')');
 {$ENDIF}

 {Check I2C}
 if I2C = nil then Exit;

 {Check Address}
 if Address = I2C_ADDRESS_INVALID then Exit;

 {Check Width and Height}
 if (Width < 1) or (Width > GOODIX_MAX_WIDTH) then Exit;
 if (Height < 1) or (Height > GOODIX_MAX_HEIGHT) then Exit;

 {Create Touch}
 GOODIXTouch:=PGOODIXTouch(TouchDeviceCreateEx(SizeOf(TGOODIXTouch)));
 if GOODIXTouch <> nil then
  begin
   {Update Touch}
   {Device}
   GOODIXTouch.Touch.Device.DeviceBus:=DEVICE_BUS_I2C;
   GOODIXTouch.Touch.Device.DeviceType:=TOUCH_TYPE_CAPACITIVE;
   GOODIXTouch.Touch.Device.DeviceFlags:=GOODIXTouch.Touch.Device.DeviceFlags or TOUCH_FLAG_MULTI_POINT;
   GOODIXTouch.Touch.Device.DeviceData:=nil;
   GOODIXTouch.Touch.Device.DeviceDescription:=GOODIX_TOUCH_DESCRIPTION;
   {Touch}
   GOODIXTouch.Touch.TouchState:=TOUCH_STATE_DISABLED;
   GOODIXTouch.Touch.DeviceStart:=GOODIXTouchStart;
   GOODIXTouch.Touch.DeviceStop:=GOODIXTouchStop;
   GOODIXTOuch.Touch.DeviceUpdate:=GOODIXTouchUpdate;
   {Driver}
   GOODIXTouch.Touch.Properties.Flags:=GOODIXTouch.Touch.Device.DeviceFlags;
   GOODIXTouch.Touch.Properties.Width:=Width;
   GOODIXTouch.Touch.Properties.Height:=Height;
   GOODIXTouch.Touch.Properties.Rotation:=TOUCH_ROTATION_0;
   GOODIXTouch.Touch.Properties.MaxX:=0;
   GOODIXTouch.Touch.Properties.MaxY:=0;
   GOODIXTouch.Touch.Properties.MaxZ:=0;
   GOODIXTouch.Touch.Properties.MaxPoints:=0;
   {I2C}
   GOODIXTouch.I2C:=I2C;
   GOODIXTouch.Address:=Address;
   {General}
   if IRQ <> nil then GOODIXTouch.IRQ:=IRQ^ else GOODIXTouch.IRQ:=GPIO_INFO_UNKNOWN;
   if RST <> nil then GOODIXTouch.RST:=RST^ else GOODIXTouch.RST:=GPIO_INFO_UNKNOWN;
   GOODIXTouch.Width:=Width;
   GOODIXTouch.Height:=Height;
   {Goodix}
   GOODIXTouch.SwapConfigXY:=GOODIX_SWAP_CONFIG_XY;
   GOODIXTouch.SwapReportXY:=GOODIX_SWAP_REPORT_XY;
   GOODIXTouch.InvertReportX:=GOODIX_INVERT_REPORT_X;
   GOODIXTouch.InvertReportY:=GOODIX_INVERT_REPORT_Y;
   GOODIXTouch.ConfigFilename:=GOODIX_LOAD_CONFIG_FILE;
   GOODIXTouch.ResetController:=GOODIX_RESET_CONTROLLER;

   {Register Touch}
   Status:=TouchDeviceRegister(@GOODIXTouch.Touch);
   if Status = ERROR_SUCCESS then
    begin
     {Start Touch}
     Status:=TouchDeviceStart(@GOODIXTouch.Touch);
     if Status = ERROR_SUCCESS then
      begin
       {Return Result}
       Result:=PTouchDevice(GOODIXTouch);
      end
     else
      begin
       if TOUCH_LOG_ENABLED then TouchLogError(nil,'GOODIX: Failed to start new Touch device: ' + ErrorToString(Status));

       {Deregister Touch}
       TouchDeviceDeregister(@GOODIXTouch.Touch);

       {Destroy Touch}
       TouchDeviceDestroy(@GOODIXTouch.Touch);
      end;
    end
   else
    begin
     if TOUCH_LOG_ENABLED then TouchLogError(nil,'GOODIX: Failed to register new Touch device: ' + ErrorToString(Status));

     {Destroy Touch}
     TouchDeviceDestroy(@GOODIXTouch.Touch);
    end;
  end
 else
  begin
   if TOUCH_LOG_ENABLED then TouchLogError(nil,'GOODIX: Failed to create new Touch device');
  end;
end;

{==============================================================================}

function GOODIXTouchDestroy(Touch:PTouchDevice):LongWord;{$IFDEF API_EXPORT_GOODIXTOUCH} stdcall;{$ENDIF}
{Stop, deregister and destroy a Goodix Touch device created by this driver}
{Touch: The Touch device to destroy}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Touch}
 if Touch = nil then Exit;

 {$IF DEFINED(GOODIX_DEBUG) or DEFINED(TOUCH_DEBUG)}
 if TOUCH_LOG_ENABLED then TouchLogDebug(Touch,'GOODIX: Touch Destroy');
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
       if TOUCH_LOG_ENABLED then TouchLogError(nil,'GOODIX: Failed to destroy Touch device: ' + ErrorToString(Result));
      end;
    end
   else
    begin
     if TOUCH_LOG_ENABLED then TouchLogError(nil,'GOODIX: Failed to deregister Touch device: ' + ErrorToString(Result));
    end;
  end
 else
  begin
   if TOUCH_LOG_ENABLED then TouchLogError(nil,'GOODIX: Failed to stop Touch device: ' + ErrorToString(Result));
  end;
end;

{==============================================================================}
{==============================================================================}
{Goodix Touch Functions}
function GOODIXTouchStart(Touch:PTouchDevice):LongWord;
{Implementation of TouchDeviceStart API for Goodix Touch device}
{Note: Not intended to be called directly by applications, use TouchDeviceStart instead}
var
 Handle:THandle;
 Buffer:Pointer;
 Size:LongWord;
 Status:LongWord;
 Properties:TI2CProperties;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Touch}
 if Touch = nil then Exit;

 {$IF DEFINED(GOODIX_DEBUG) or DEFINED(TOUCH_DEBUG)}
 if TOUCH_LOG_ENABLED then TouchLogDebug(Touch,'GOODIX: Touch Start');
 {$ENDIF}

 {Check I2C Device}
 Result:=I2CDeviceGetProperties(PGOODIXTouch(Touch).I2C,@Properties);
 if Result <> ERROR_SUCCESS then Exit;

 {Start I2C Device}
 Result:=I2CDeviceStart(PGOODIXTouch(Touch).I2C,Min(GOODIX_I2C_RATE,Properties.MaxClock));
 if Result <> ERROR_SUCCESS then Exit;

 {Check Reset}
 if PGOODIXTouch(Touch).ResetController then
  begin
   Result:=GOODIXReset(PGOODIXTouch(Touch));
   if Result <> ERROR_SUCCESS then Exit;
  end;

 {Read Version}
 Result:=GOODIXReadVersion(PGOODIXTouch(Touch));
 if Result <> ERROR_SUCCESS then Exit;

 {Get Chip Data}
 Result:=GOODIXGetChipData(PGOODIXTouch(Touch));
 if Result <> ERROR_SUCCESS then Exit;

 {Check Load Config}
 if PGOODIXTouch(Touch).ConfigFilename <> '' then
  begin
   {Acquire Config File}
   Status:=DeviceFirmwareAcquire(DEVICE_CLASS_ANY,PGOODIXTouch(Touch).ConfigFilename,GOODIX_CONFIG_TIMEOUT,Handle,Buffer,Size);
   if Status = ERROR_SUCCESS then
    begin
     try
      {Send Configuration}
      Result:=GOODIXSendConfig(PGOODIXTouch(Touch),Buffer,Size);
      if Result <> ERROR_SUCCESS then Exit;
     finally
      {Release Config File}
      DeviceFirmwareRelease(Handle,Buffer,Size);
     end;
    end
   else
    begin
     if TOUCH_LOG_ENABLED then TouchLogError(Touch,'GOODIX: Failed to acquire touch configuration file "' + PGOODIXTouch(Touch).ConfigFilename + '": ' + ErrorToString(Status));
    end;
  end;

 {Configure Device}
 Result:=GOODIXConfigureDevice(PGOODIXTouch(Touch));
 if Result <> ERROR_SUCCESS then Exit;

 {Return Result}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function GOODIXTouchStop(Touch:PTouchDevice):LongWord;
{Implementation of TouchDeviceStop API for Goodix Touch device}
{Note: Not intended to be called directly by applications, use TouchDeviceStop instead}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Touch}
 if Touch = nil then Exit;

 {$IF DEFINED(GOODIX_DEBUG) or DEFINED(TOUCH_DEBUG)}
 if TOUCH_LOG_ENABLED then TouchLogDebug(Touch,'GOODIX: Touch Stop');
 {$ENDIF}

 {Cancel Event}
 GPIODeviceInputCancel(PGOODIXTouch(Touch).IRQ.GPIO,PGOODIXTouch(Touch).IRQ.Pin);

 {Return Result}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function GOODIXTouchUpdate(Touch:PTouchDevice):LongWord;
{Implementation of TouchDeviceUpdate API for Goodix Touch device}
{Note: Not intended to be called directly by applications, use TouchDeviceUpdate instead}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Touch}
 if Touch = nil then Exit;

 {$IF DEFINED(GOODIX_DEBUG) or DEFINED(TOUCH_DEBUG)}
 if TOUCH_LOG_ENABLED then TouchLogDebug(Touch,'GOODIX: Touch Update');
 {$ENDIF}

 {Acquire Lock}
 if MutexLock(Touch.Lock) = ERROR_SUCCESS then
  begin
   try
    {Update Configuration}
    Result:=GOODIXUpdateConfig(PGOODIXTouch(Touch));
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

procedure GOODIXTouchCallback(Touch:PGOODIXTouch;Pin,Trigger:LongWord);
{Touch device event callback (Interrupt) handler for Goodix Touch device}
{Note: Not intended to be called directly by applications}
begin
 {}
 {Check Touch}
 if Touch = nil then Exit;

 {Acquire Lock}
 if MutexLock(Touch.Touch.Lock) = ERROR_SUCCESS then
  begin
   {$IF DEFINED(GOODIX_DEBUG) or DEFINED(TOUCH_DEBUG)}
   if TOUCH_LOG_ENABLED then TouchLogDebug(@Touch.Touch,'GOODIX: Touch Callback');
   {$ENDIF}

   {Process Events}
   GOODIXProcessEvents(Touch);

   {End Command}
   if GOODIXWriteByteI2C(Touch,GOODIX_READ_COOR_ADDR,0) <> ERROR_SUCCESS then
    begin
     if TOUCH_LOG_ENABLED then TouchLogError(@Touch.Touch,'GOODIX: Failed to write end command');
    end;

   {Reregister Event}
   if GPIODeviceInputEvent(Touch.IRQ.GPIO,Touch.IRQ.Pin,Touch.IRQ.Trigger,GPIO_EVENT_FLAG_NONE,INFINITE,TGPIOCallback(GOODIXTouchCallback),Touch) <> ERROR_SUCCESS then
    begin
     if TOUCH_LOG_ENABLED then TouchLogError(@Touch.Touch,'GOODIX: Failed to re-register touch callback');
    end;

   {Release Lock}
   MutexUnlock(Touch.Touch.Lock);
  end;
end;

{==============================================================================}
{==============================================================================}
{Goodix Helper Functions}

{==============================================================================}
{==============================================================================}
{Goodix Internal Functions}
function GOODIXReadI2C(Touch:PGOODIXTouch;Reg:Word;Data:Pointer;Size:LongWord):LongWord;
var
 Value:Word;
 Count:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Touch}
 if Touch = nil then Exit;

 {$IF DEFINED(GOODIX_DEBUG) or DEFINED(TOUCH_DEBUG)}
 if TOUCH_LOG_ENABLED then TouchLogDebug(@Touch.Touch,'GOODIX: Read I2C (Reg=' + IntToHex(Reg,4) + ')');
 {$ENDIF}

 {Swap Register to BE}
 Value:=WordNtoBE(Reg);

 {Write Read to I2C}
 Result:=I2CDeviceWriteRead(Touch.I2C,Touch.Address,@Value,SizeOf(Word),Data,Size,Count);
end;

{==============================================================================}

function GOODIXWriteI2C(Touch:PGOODIXTouch;Reg:Word;Data:Pointer;Size:LongWord):LongWord;
var
 Value:Word;
 Count:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Touch}
 if Touch = nil then Exit;

 {$IF DEFINED(GOODIX_DEBUG) or DEFINED(TOUCH_DEBUG)}
 if TOUCH_LOG_ENABLED then TouchLogDebug(@Touch.Touch,'GOODIX: Write I2C (Reg=' + IntToHex(Reg,4) + ')');
 {$ENDIF}

 {Swap Register to BE}
 Value:=WordNtoBE(Reg);

 {Write Write to I2C}
 Result:=I2CDeviceWriteWrite(Touch.I2C,Touch.Address,@Value,SizeOf(Word),Data,Size,Count);
end;

{==============================================================================}

function GOODIXWriteByteI2C(Touch:PGOODIXTouch;Reg:Word;Value:Byte):LongWord;
begin
 {}
 Result:=GOODIXWriteI2C(Touch,Reg,@Value,SizeOf(Byte));
end;

{==============================================================================}

function GOODIXReset(Touch:PGOODIXTouch):LongWord;
var
 Level:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Touch}
 if Touch = nil then Exit;

 {$IF DEFINED(GOODIX_DEBUG) or DEFINED(TOUCH_DEBUG)}
 if TOUCH_LOG_ENABLED then TouchLogDebug(@Touch.Touch,'GOODIX: Reset');
 {$ENDIF}

 {Check Reset}
 if (Touch.RST.GPIO <> nil) and (Touch.RST.Pin <> GPIO_PIN_UNKNOWN) then
  begin
   {Begin select I2C slave address}
   {Set Output Low}
   Result:=GPIODeviceOutputSet(Touch.RST.GPIO,Touch.RST.Pin,GPIO_LEVEL_LOW);
   if Result <> ERROR_SUCCESS then Exit;

   {Set Direction Output}
   Result:=GPIODeviceFunctionSelect(Touch.RST.GPIO,Touch.RST.Pin,GPIO_FUNCTION_OUT);
   if Result <> ERROR_SUCCESS then Exit;

   {Wait (T2: > 10ms)}
   MillisecondDelay(20);

   {HIGH: 0x28/0x29, LOW: 0xBA/0xBB}
   Level:=GPIO_LEVEL_LOW;
   if Touch.Address = $14 then Level:=GPIO_LEVEL_HIGH;

   {IRQ to Output}
   Result:=GOODIXIntDirectionOutput(Touch,Level);
   if Result <> ERROR_SUCCESS then Exit;

   {Wait (T3: > 100us)}
   MicrosecondDelay(200);

   {Set Output High}
   Result:=GPIODeviceOutputSet(Touch.RST.GPIO,Touch.RST.Pin,GPIO_LEVEL_HIGH);
   if Result <> ERROR_SUCCESS then Exit;

   {Wait (T4: > 5ms)}
   MillisecondDelay(6);

   {End select I2C slave address}
   {Set Direction Input}
   Result:=GPIODeviceFunctionSelect(Touch.RST.GPIO,Touch.RST.Pin,GPIO_FUNCTION_IN);
   if Result <> ERROR_SUCCESS then Exit;

   {Sync Interrupt}
   Result:=GOODIXIntSync(Touch);
   if Result <> ERROR_SUCCESS then Exit;
  end;

 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function GOODIXReadVersion(Touch:PGOODIXTouch):LongWord;
var
 Buffer:array[0..5] of Byte;
 Id:array[0..GOODIX_ID_MAX_LEN] of Char;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Touch}
 if Touch = nil then Exit;

 {$IF DEFINED(GOODIX_DEBUG) or DEFINED(TOUCH_DEBUG)}
 if TOUCH_LOG_ENABLED then TouchLogDebug(@Touch.Touch,'GOODIX: Read Version');
 {$ENDIF}

 {Read Version}
 Result:=GOODIXReadI2C(Touch,GOODIX_REG_ID,@Buffer,SizeOf(Buffer));
 if Result <> ERROR_SUCCESS then Exit;

 {Copy Id}
 System.Move(Buffer[0],Id[0],GOODIX_ID_MAX_LEN);
 Touch.Id:=Id;

 {Copy Version}
 Touch.Version:=PWord(@Buffer[4])^;

 {$IF DEFINED(GOODIX_DEBUG) or DEFINED(TOUCH_DEBUG)}
 if TOUCH_LOG_ENABLED then TouchLogDebug(@Touch.Touch,'GOODIX:  Id: ' + Touch.Id + ' Version: ' + IntToHex(Touch.Version,4));
 {$ENDIF}

 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function GOODIXGetChipData(Touch:PGOODIXTouch):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Touch}
 if Touch = nil then Exit;

 {$IF DEFINED(GOODIX_DEBUG) or DEFINED(TOUCH_DEBUG)}
 if TOUCH_LOG_ENABLED then TouchLogDebug(@Touch.Touch,'GOODIX: Get Chip Data');
 {$ENDIF}

 {Check the ID}
 if (Touch.Id = '1151') or
    (Touch.Id = '5663') or
    (Touch.Id = '5688') or
    (Touch.Id = '917S') or
    (Touch.Id = '9286') then
  begin
   {GT1X}
   Touch.ConfigAddress:=GOODIX_GT1X_REG_CONFIG_DATA;
   Touch.ConfigLength:=GOODIX_CONFIG_GT9X_LENGTH;
   Touch.ChecksumSize:=16;
  end
 else if (Touch.Id = '911') or
         (Touch.Id = '9271') or
         (Touch.Id = '9110') or
         (Touch.Id = '9111') or
         (Touch.Id = '927') or
         (Touch.Id = '928') then
  begin
   {GT911}
   Touch.ConfigAddress:=GOODIX_GT9X_REG_CONFIG_DATA;
   Touch.ConfigLength:=GOODIX_CONFIG_911_LENGTH;
   Touch.ChecksumSize:=8;
  end
 else if (Touch.Id = '912') or
         (Touch.Id = '9147') or
         (Touch.Id = '967') then
  begin
   {GT967}
   Touch.ConfigAddress:=GOODIX_GT9X_REG_CONFIG_DATA;
   Touch.ConfigLength:=GOODIX_CONFIG_967_LENGTH;
   Touch.ChecksumSize:=8;
  end
 else
  begin
   {GT9X}
   Touch.ConfigAddress:=GOODIX_GT9X_REG_CONFIG_DATA;
   Touch.ConfigLength:=GOODIX_CONFIG_GT9X_LENGTH;
   Touch.ChecksumSize:=8;
  end;

 {All}
 Touch.ContactSize:=GOODIX_CONTACT_SIZE;

 {$IF DEFINED(GOODIX_DEBUG) or DEFINED(TOUCH_DEBUG)}
 if TOUCH_LOG_ENABLED then TouchLogDebug(@Touch.Touch,'GOODIX:  Address: ' + IntToHex(Touch.ConfigAddress,4) + ' Length: ' + IntToStr(Touch.ConfigLength) + ' Checksum: ' + IntToStr(Touch.ChecksumSize));
 {$ENDIF}

 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function GOODIXReadConfig(Touch:PGOODIXTouch):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Touch}
 if Touch = nil then Exit;

 {$IF DEFINED(GOODIX_DEBUG) or DEFINED(TOUCH_DEBUG)}
 if TOUCH_LOG_ENABLED then TouchLogDebug(@Touch.Touch,'GOODIX: Read Config');
 {$ENDIF}

 Result:=GOODIXReadI2C(Touch,Touch.ConfigAddress,@Touch.Configuration,Touch.ConfigLength);
 if Result <> ERROR_SUCCESS then Exit;

 {Get Trigger}
 Touch.IRQ.Trigger:=GOODIX_IRQ_TRIGGERS[Touch.Configuration[GOODIX_TRIGGER_OFFSET] and $03];

 {Get Max X and Y}
 if not Touch.SwapConfigXY then
  begin
   Touch.MaxX:=PWord(@Touch.Configuration[GOODIX_RESOLUTION_OFFSET])^;
   Touch.MaxY:=PWord(@Touch.Configuration[GOODIX_RESOLUTION_OFFSET + 2])^;
  end
 else
  begin
   Touch.MaxY:=PWord(@Touch.Configuration[GOODIX_RESOLUTION_OFFSET])^;
   Touch.MaxX:=PWord(@Touch.Configuration[GOODIX_RESOLUTION_OFFSET + 2])^;
  end;

 {Get Max Points}
 Touch.MaxPoints:=Touch.Configuration[GOODIX_MAX_CONTACTS_OFFSET] and $0F;

 {Calc Checksum}
 if Touch.ChecksumSize = 8 then
  begin
   Result:=GOODIXCalcChecksum8(Touch);
   if Result <> ERROR_SUCCESS then Exit;
  end
 else if Touch.ChecksumSize = 16 then
  begin
   Result:=GOODIXCalcChecksum16(Touch);
   if Result <> ERROR_SUCCESS then Exit;
  end;

 {Check Max X and Y}
 if (Touch.MaxX = 0) or (Touch.MaxY = 0) then
  begin
   if TOUCH_LOG_ENABLED then TouchLogError(@Touch.Touch,'GOODIX: Invalid max X or Y configuration, using defaults');

   Touch.MaxX:=GOODIX_MAX_WIDTH - 1;
   Touch.MaxY:=GOODIX_MAX_HEIGHT - 1;
  end;

 {Check Max Points}
 if Touch.MaxPoints = 0 then
  begin
   if TOUCH_LOG_ENABLED then TouchLogError(@Touch.Touch,'GOODIX: Invalid max points configuration, using defaults');

   Touch.MaxPoints:=GOODIX_MAX_CONTACTS;
  end;

 {$IF DEFINED(GOODIX_DEBUG) or DEFINED(TOUCH_DEBUG)}
 if TOUCH_LOG_ENABLED then TouchLogDebug(@Touch.Touch,'GOODIX:  Trigger: ' + GPIOTriggerToString(Touch.IRQ.Trigger) + ' Max Points: ' + IntToStr(Touch.MaxPoints) + ' Max X: ' + IntToStr(Touch.MaxX) + ' Max Y: ' + IntToStr(Touch.MaxY));
 {$ENDIF}

 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function GOODIXUpdateConfig(Touch:PGOODIXTouch):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Touch}
 if Touch = nil then Exit;

 {$IF DEFINED(GOODIX_DEBUG) or DEFINED(TOUCH_DEBUG)}
 if TOUCH_LOG_ENABLED then TouchLogDebug(@Touch.Touch,'GOODIX: Update Config');
 {$ENDIF}

 {Check Width and Height}
 if (Touch.MaxX <> (Touch.Width - 1)) or (Touch.MaxY <> (Touch.Height - 1)) then
  begin
   {Update Max X and Y}
   Touch.MaxX:=Touch.Width - 1;
   Touch.MaxY:=Touch.Height - 1;

   {Check Configuration}
   Result:=GOODIXCheckConfig(Touch,@Touch.Configuration,Touch.ConfigLength);
   if Result <> ERROR_SUCCESS then
    begin
     if TOUCH_LOG_ENABLED then TouchLogError(@Touch.Touch,'GOODIX: Check configuration failed');
     Exit;
    end;

   {Update Configuration}
   if not Touch.SwapConfigXY then
    begin
     PWord(@Touch.Configuration[GOODIX_RESOLUTION_OFFSET])^:=Touch.MaxX;
     PWord(@Touch.Configuration[GOODIX_RESOLUTION_OFFSET + 2])^:=Touch.MaxY;
    end
   else
    begin
     PWord(@Touch.Configuration[GOODIX_RESOLUTION_OFFSET])^:=Touch.MaxY;
     PWord(@Touch.Configuration[GOODIX_RESOLUTION_OFFSET + 2])^:=Touch.MaxX;
    end;

   {Update Checksum}
   if Touch.ChecksumSize = 8 then
    begin
     Result:=GOODIXCalcChecksum8(Touch);
     if Result <> ERROR_SUCCESS then Exit;
    end
   else if Touch.ChecksumSize = 16 then
    begin
     Result:=GOODIXCalcChecksum16(Touch);
     if Result <> ERROR_SUCCESS then Exit;
    end;

   {Send Configuration}
   Result:=GOODIXSendConfig(Touch,@Touch.Configuration,Touch.ConfigLength);
   if Result <> ERROR_SUCCESS then
    begin
     if TOUCH_LOG_ENABLED then TouchLogError(@Touch.Touch,'GOODIX: Send configuration failed');
     Exit;
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

 {$IF DEFINED(GOODIX_DEBUG) or DEFINED(TOUCH_DEBUG)}
 if TOUCH_LOG_ENABLED then TouchLogDebug(@Touch.Touch,'GOODIX:  Width: ' + IntToStr(Touch.Touch.Properties.Width) + ' Height: ' + IntToStr(Touch.Touch.Properties.Height));
 if TOUCH_LOG_ENABLED then TouchLogDebug(@Touch.Touch,'GOODIX:  Max Points: ' + IntToStr(Touch.Touch.Properties.MaxPoints) + ' Max X: ' + IntToStr(Touch.Touch.Properties.MaxX) + ' Max Y: ' + IntToStr(Touch.Touch.Properties.MaxY));
 {$ENDIF}

 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function GOODIXConfigureDevice(Touch:PGOODIXTouch):LongWord;
var
 Count:LongInt;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Touch}
 if Touch = nil then Exit;

 {$IF DEFINED(GOODIX_DEBUG) or DEFINED(TOUCH_DEBUG)}
 if TOUCH_LOG_ENABLED then TouchLogDebug(@Touch.Touch,'GOODIX: Configure Device');
 {$ENDIF}

 {Set Defaults}
 Touch.IRQ.Trigger:=GOODIX_IRQ_TRIGGERS[GOODIX_INT_TRIGGER];
 Touch.Touch.Properties.MaxPoints:=GOODIX_MAX_CONTACTS;

 {Capacitive Windows/Home button on some devices}
 for Count:=0 to GOODIX_MAX_KEYS - 1 do
  begin
   if Count = 0 then
    begin
     Touch.KeyCodes[Count]:=KEY_CODE_GUI;
     Touch.ScanCodes[Count]:=SCAN_CODE_LEFT_GUI; {or SCAN_CODE_RIGHT_GUI}
    end
   else
    begin
     Touch.KeyCodes[Count]:=KEY_CODE_F1 + (Count - 1);
     Touch.ScanCodes[Count]:=SCAN_CODE_F1 + (Count - 1);
    end;
  end;

 {Read Configuration}
 Result:=GOODIXReadConfig(Touch);
 if Result <> ERROR_SUCCESS then Exit;

 {Update Configuration}
 Result:=GOODIXUpdateConfig(Touch);
 if Result <> ERROR_SUCCESS then Exit;

 {Initialize IRQ}
 if (Touch.IRQ.GPIO <> nil) and (Touch.IRQ.Pin <> GPIO_PIN_UNKNOWN) then
  begin
   {Setup IRQ (GPIO)}
   if GPIODeviceFunctionSelect(Touch.IRQ.GPIO,Touch.IRQ.Pin,Touch.IRQ.Func) <> ERROR_SUCCESS then
    begin
     Result:=ERROR_OPERATION_FAILED;
     Exit;
    end;
   if GPIODevicePullSelect(Touch.IRQ.GPIO,Touch.IRQ.Pin,Touch.IRQ.Pull) <> ERROR_SUCCESS then
    begin
     Result:=ERROR_OPERATION_FAILED;
     Exit;
    end;

   {Create GPIO Event}
   if GPIODeviceInputEvent(Touch.IRQ.GPIO,Touch.IRQ.Pin,Touch.IRQ.Trigger,GPIO_EVENT_FLAG_NONE,INFINITE,TGPIOCallback(GOODIXTouchCallback),Touch) <> ERROR_SUCCESS then
    begin
     Result:=ERROR_OPERATION_FAILED;
     Exit;
    end;
  end;

 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function GOODIXProcessEvents(Touch:PGOODIXTouch):LongWord;
{Note: Caller must hold the Touch device lock}
var
 X:Word;
 Y:Word;
 W:Word;
 Id:Byte;
 Temp:Word;
 Keys:Byte;
 Count:LongInt;
 Total:LongInt;
 TouchData:PTouchData;
 MouseData:TMouseData;
 ModifiedKeys:LongWord;
 ReleasedKeys:LongWord;
 Points:TGOODIXPointData;
 ModifiedPoints:LongWord;
 ReleasedPoints:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Touch}
 if Touch = nil then Exit;

 {$IF DEFINED(GOODIX_DEBUG) or DEFINED(TOUCH_DEBUG)}
 if TOUCH_LOG_ENABLED then TouchLogDebug(@Touch.Touch,'GOODIX: Process Events');
 {$ENDIF}

 {Read Input Report}
 Result:=GOODIXReadInputReport(Touch,@Points,Total);
 if Result <> ERROR_SUCCESS then Exit;

 {$IF DEFINED(GOODIX_DEBUG) or DEFINED(TOUCH_DEBUG)}
 if TOUCH_LOG_ENABLED then TouchLogDebug(@Touch.Touch,'GOODIX:  Touch Count: ' + IntToStr(Total));
 {$ENDIF}

 {Setup Defaults}
 ModifiedKeys:=0;
 ModifiedPoints:=0;

 {Clear Mouse Data}
 FillChar(MouseData,SizeOf(TMouseData),0);

 {Process Keys}
 if (Total > 0) and ((Points[0] and GOODIX_HAVE_KEY) <> 0) then
  begin
   {Get Keys}
   Keys:=PByte(Points[1 + (Touch.ContactSize * Total)])^;

   {Check Keys}
   for Count:=0 to GOODIX_MAX_KEYS - 1 do
    begin
     if (Keys and BIT(Count)) <> 0 then
      begin
       {Store Key}
       ModifiedKeys:=ModifiedKeys or BIT(Count);

       {Check Repeat}
       if (Touch.LastKeys and BIT(Count)) = 0 then
        begin
         {Put Key}
         KeyboardPut(Touch.ScanCodes[Count],Touch.KeyCodes[Count],KEYBOARD_KEYDOWN);
        end;
      end;
    end;
  end;

 {Process Released Keys}
 ReleasedKeys:=Touch.LastKeys and not(ModifiedKeys);
 if ReleasedKeys > 0 then
  begin
   {Check Keys}
   for Count:=0 to GOODIX_MAX_KEYS - 1 do
    begin
     if (ReleasedKeys and BIT(Count)) <> 0 then
      begin
       {Put Key}
       KeyboardPut(Touch.ScanCodes[Count],Touch.KeyCodes[Count],KEYBOARD_KEYUP);
      end;
    end;
  end;

 {Save Last Keys}
 Touch.LastKeys:=ModifiedKeys;

 {Process Touches}
 for Count:=0 to Total - 1 do
  begin
   {Get Coordinates}
   if Touch.ContactSize = 9 then
    begin
     {9 byte report}
     Id:=Points[1 + (Touch.ContactSize * Count) + 0] and $0F;
     X:=PWord(@Points[1 + (Touch.ContactSize * Count) + 3])^;
     Y:=PWord(@Points[1 + (Touch.ContactSize * Count) + 5])^;
     W:=PWord(@Points[1 + (Touch.ContactSize * Count) + 7])^;
    end
   else
    begin
     {8 byte report}
     Id:=Points[1 + (Touch.ContactSize * Count) + 0] and $0F;
     X:=PWord(@Points[1 + (Touch.ContactSize * Count) + 1])^;
     Y:=PWord(@Points[1 + (Touch.ContactSize * Count) + 3])^;
     W:=PWord(@Points[1 + (Touch.ContactSize * Count) + 5])^;
    end;

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

   {$IF DEFINED(GOODIX_DEBUG) or DEFINED(TOUCH_DEBUG)}
   if TOUCH_LOG_ENABLED then TouchLogDebug(@Touch.Touch,'GOODIX:  Id=' + IntToStr(Id) + ' X=' + IntToStr(X) + ' Y=' + IntToStr(Y) + ' W=' + IntToStr(W));
   {$ENDIF}

   {Store Point}
   ModifiedPoints:=ModifiedPoints or (1 shl Id);

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
       if TOUCH_LOG_ENABLED then TouchLogError(@Touch.Touch,'GOODIX: Buffer overflow, packet discarded');

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
         if TOUCH_LOG_ENABLED then TouchLogError(@Touch.Touch,'GOODIX: Failed to write mouse data, packet discarded');

         {Update Statistics}
         Inc(Touch.Touch.ReceiveErrors);
        end;
      end;
    end;
  end;

 {$IF DEFINED(GOODIX_DEBUG) or DEFINED(TOUCH_DEBUG)}
 if TOUCH_LOG_ENABLED and (ModifiedPoints > 0) then TouchLogDebug(@Touch.Touch,'GOODIX:  Modified Points=' + IntToHex(ModifiedPoints,8));
 {$ENDIF}

 {Process Releases}
 ReleasedPoints:=Touch.LastPoints and not(ModifiedPoints);
 if ReleasedPoints > 0 then
  begin
   for Count:=0 to Touch.Touch.Properties.MaxPoints - 1 do
    begin
     if (ReleasedPoints and (1 shl Count)) <> 0 then
      begin
       {$IF DEFINED(GOODIX_DEBUG) or DEFINED(TOUCH_DEBUG)}
       if TOUCH_LOG_ENABLED then TouchLogDebug(@Touch.Touch,'GOODIX:  Released Id=' + IntToStr(Count));
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
           if TOUCH_LOG_ENABLED then TouchLogError(@Touch.Touch,'GOODIX: Buffer overflow, packet discarded');

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
             if TOUCH_LOG_ENABLED then TouchLogError(@Touch.Touch,'GOODIX: Failed to write mouse data, packet discarded');

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

function GOODIXReadInputReport(Touch:PGOODIXTouch;Data:PByte;var Count:LongInt):LongWord;
var
 Address:Word;
 Timeout:Int64;
 Size:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Set Defaults}
 Count:=0;

 {Check Touch}
 if Touch = nil then Exit;

 {$IF DEFINED(GOODIX_DEBUG) or DEFINED(TOUCH_DEBUG)}
 if TOUCH_LOG_ENABLED then TouchLogDebug(@Touch.Touch,'GOODIX: Read Input Report');
 {$ENDIF}

 Address:=GOODIX_READ_COOR_ADDR;
 Timeout:=ClockGetTotal + (GOODIX_BUFFER_STATUS_TIMEOUT * CLOCK_CYCLES_PER_MILLISECOND);

 {Read 1 byte header plus touch count * contact size plus 1 byte footer}
 Size:=1 + Touch.ContactSize + 1;

 {Wait for buffer status ready bit to be set which can take up to 10ms}
 repeat
  {Read Status Buffer}
  Result:=GOODIXReadI2C(Touch,Address,Data,Size);
  if Result <> ERROR_SUCCESS then
   begin
    if TOUCH_LOG_ENABLED then TouchLogError(@Touch.Touch,'GOODIX: Failed to read input report');

    Exit;
   end;

  {Check Buffer Status}
  if (Data[0] and GOODIX_BUFFER_STATUS_READY) <> 0 then
   begin
    {Get Touch Count}
    Count:=Data[0] and $0F;
    if Count > Touch.Touch.Properties.MaxPoints then
     begin
      if TOUCH_LOG_ENABLED then TouchLogError(@Touch.Touch,'GOODIX: Touch point count greater than maximum touch points');

      Result:=ERROR_INVALID_DATA;
      Exit;
     end;

    {Check Touch Count}
    if Count > 1 then
     begin
      {Update Location}
      Inc(Address,Size);
      Inc(Data,Size);

      {Read Touch Data}
      Result:=GOODIXReadI2C(Touch,Address,Data,Touch.ContactSize * (Count - 1));
      if Result <> ERROR_SUCCESS then Exit;
     end;

    Result:=ERROR_SUCCESS;
    Exit;
   end;

  {Delay between polls}
  MicrosecondDelay(1000);
 until ClockGetTotal > Timeout;

 Result:=ERROR_TIMEOUT;
end;

{==============================================================================}

function GOODIXCheckConfig8(Touch:PGOODIXTouch;Data:PByte;Size:LongWord):LongWord;
var
 Count:LongInt;
 Checksum:Byte;
 RawLength:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Touch}
 if Touch = nil then Exit;

 {$IF DEFINED(GOODIX_DEBUG) or DEFINED(TOUCH_DEBUG)}
 if TOUCH_LOG_ENABLED then TouchLogDebug(@Touch.Touch,'GOODIX: Check Config (8)');
 {$ENDIF}

 {Check Parameters}
 if Data = nil then Exit;
 if Size < 2 then Exit;

 {Calculate Checksum}
 Checksum:=0;
 RawLength:=Size - 2;

 for Count:=0 to RawLength - 1 do
  begin
   Checksum:=Checksum + Data[Count];
  end;

 Checksum:=(not Checksum) + 1;

 {Compare Checksum}
 if Checksum <> Data[RawLength] then
  begin
   if TOUCH_LOG_ENABLED then TouchLogError(@Touch.Touch,'GOODIX: Incorrect config checksum');

   Result:=ERROR_INVALID_DATA;
   Exit;
  end;

 {Check "config_fresh" bit}
 if Data[RawLength + 1] <> 1 then
  begin
   if TOUCH_LOG_ENABLED then TouchLogError(@Touch.Touch,'GOODIX: Config fresh bit not set');

   Result:=ERROR_INVALID_DATA;
   Exit;
  end;

 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function GOODIXCalcChecksum8(Touch:PGOODIXTouch):LongWord;
var
 Count:LongInt;
 Checksum:Byte;
 RawLength:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Touch}
 if Touch = nil then Exit;

 {$IF DEFINED(GOODIX_DEBUG) or DEFINED(TOUCH_DEBUG)}
 if TOUCH_LOG_ENABLED then TouchLogDebug(@Touch.Touch,'GOODIX: Calc Checksum (8)');
 {$ENDIF}

 {Calculate Checksum}
 Checksum:=0;
 RawLength:=Touch.ConfigLength - 2;

 for Count:=0 to RawLength - 1 do
  begin
   Checksum:=Checksum + Touch.Configuration[Count];
  end;

 Checksum:=(not Checksum) + 1;

 {Store Checksum and set "config_fresh" bit}
 Touch.Configuration[RawLength]:=Checksum;
 Touch.Configuration[RawLength + 1]:=1;

 {$IF DEFINED(GOODIX_DEBUG) or DEFINED(TOUCH_DEBUG)}
 if TOUCH_LOG_ENABLED then TouchLogDebug(@Touch.Touch,'GOODIX: Checksum: ' + IntToHex(Checksum,2));
 {$ENDIF}

 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function GOODIXCheckConfig16(Touch:PGOODIXTouch;Data:PByte;Size:LongWord):LongWord;
var
 Count:LongInt;
 Checksum:Word;
 RawLength:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Touch}
 if Touch = nil then Exit;

 {$IF DEFINED(GOODIX_DEBUG) or DEFINED(TOUCH_DEBUG)}
 if TOUCH_LOG_ENABLED then TouchLogDebug(@Touch.Touch,'GOODIX: Check Config (16)');
 {$ENDIF}

 {Check Parameters}
 if Data = nil then Exit;
 if Size < 3 then Exit;

 {Calculate Checksum}
 Checksum:=0;
 RawLength:=Size - 3;

 Count:=0;
 while Count < RawLength do
  begin
   Checksum:=Checksum + WordBEToN(PWord(@Data[Count])^);

   Inc(Count,2);
  end;

 Checksum:=(not Checksum) + 1;

 {Compare Checksum}
 if Checksum <> WordBEToN(PWord(@Data[RawLength])^) then
  begin
   if TOUCH_LOG_ENABLED then TouchLogError(@Touch.Touch,'GOODIX: Incorrect config checksum');

   Result:=ERROR_INVALID_DATA;
   Exit;
  end;

 {Check "config_fresh" bit}
 if Data[RawLength + 2] <> 1 then
  begin
   if TOUCH_LOG_ENABLED then TouchLogError(@Touch.Touch,'GOODIX: Config fresh bit not set');

   Result:=ERROR_INVALID_DATA;
   Exit;
  end;

 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function GOODIXCalcChecksum16(Touch:PGOODIXTouch):LongWord;
var
 Count:LongInt;
 Checksum:Word;
 RawLength:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Touch}
 if Touch = nil then Exit;

 {$IF DEFINED(GOODIX_DEBUG) or DEFINED(TOUCH_DEBUG)}
 if TOUCH_LOG_ENABLED then TouchLogDebug(@Touch.Touch,'GOODIX: Calc Checksum (16)');
 {$ENDIF}

 {Calculate Checksum}
 Checksum:=0;
 RawLength:=Touch.ConfigLength - 3;

 Count:=0;
 while Count < RawLength do
  begin
   Checksum:=Checksum + WordBEToN(PWord(@Touch.Configuration[Count])^);

   Inc(Count,2);
  end;

 Checksum:=(not Checksum) + 1;

 {Store Checksum and set "config_fresh" bit}
 PWord(@Touch.Configuration[RawLength])^:=WordNToBE(Checksum);
 Touch.Configuration[RawLength + 2]:=1;

 {$IF DEFINED(GOODIX_DEBUG) or DEFINED(TOUCH_DEBUG)}
 if TOUCH_LOG_ENABLED then TouchLogDebug(@Touch.Touch,'GOODIX: Checksum: ' + IntToHex(Checksum,4));
 {$ENDIF}

 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function GOODIXCheckConfig(Touch:PGOODIXTouch;Data:PByte;Size:LongWord):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Touch}
 if Touch = nil then Exit;

 {$IF DEFINED(GOODIX_DEBUG) or DEFINED(TOUCH_DEBUG)}
 if TOUCH_LOG_ENABLED then TouchLogDebug(@Touch.Touch,'GOODIX: Check Config');
 {$ENDIF}

 {Check Parameters}
 if Data = nil then Exit;
 if (Size < GOODIX_CONFIG_MIN_LENGTH) or (Size > GOODIX_CONFIG_MAX_LENGTH) then Exit;

 {Check Config}
 if Touch.ChecksumSize = 8 then
  begin
   Result:=GOODIXCheckConfig8(Touch,Data,Size);
  end
 else if Touch.ChecksumSize = 16 then
  begin
   Result:=GOODIXCheckConfig16(Touch,Data,Size);
  end;
end;

{==============================================================================}

function GOODIXSendConfig(Touch:PGOODIXTouch;Data:PByte;Size:LongWord):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Touch}
 if Touch = nil then Exit;

 {$IF DEFINED(GOODIX_DEBUG) or DEFINED(TOUCH_DEBUG)}
 if TOUCH_LOG_ENABLED then TouchLogDebug(@Touch.Touch,'GOODIX: Send Config');
 {$ENDIF}

 {Check Config}
 Result:=GOODIXCheckConfig(Touch,Data,Size);
 if Result <> ERROR_SUCCESS then Exit;

 {Write Config}
 Result:=GOODIXWriteI2C(Touch,Touch.ConfigAddress,Data,Size);
 if Result <> ERROR_SUCCESS then Exit;

 {Let the firmware reconfigure itself, so sleep for 10ms}
 MillisecondDelay(10);

 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function GOODIXIntDirectionOutput(Touch:PGOODIXTouch;Level:LongWord):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Touch}
 if Touch = nil then Exit;

 {$IF DEFINED(GOODIX_DEBUG) or DEFINED(TOUCH_DEBUG)}
 if TOUCH_LOG_ENABLED then TouchLogDebug(@Touch.Touch,'GOODIX: IRQ Direction Output');
 {$ENDIF}

 {Check IRQ}
 if (Touch.IRQ.GPIO <> nil) and (Touch.IRQ.Pin <> GPIO_PIN_UNKNOWN) then
  begin
   {Set Output Level}
   Result:=GPIODeviceOutputSet(Touch.IRQ.GPIO,Touch.IRQ.Pin,Level);
   if Result <> ERROR_SUCCESS then Exit;

   {Set Direction Output}
   Result:=GPIODeviceFunctionSelect(Touch.IRQ.GPIO,Touch.IRQ.Pin,GPIO_FUNCTION_OUT);
   if Result <> ERROR_SUCCESS then Exit;
  end;

 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function GOODIXIntDirectionInput(Touch:PGOODIXTouch):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Touch}
 if Touch = nil then Exit;

 {$IF DEFINED(GOODIX_DEBUG) or DEFINED(TOUCH_DEBUG)}
 if TOUCH_LOG_ENABLED then TouchLogDebug(@Touch.Touch,'GOODIX: IRQ Direction Input');
 {$ENDIF}

 {Check IRQ}
 if (Touch.IRQ.GPIO <> nil) and (Touch.IRQ.Pin <> GPIO_PIN_UNKNOWN) then
  begin
   {Set Direction Input}
   Result:=GPIODeviceFunctionSelect(Touch.IRQ.GPIO,Touch.IRQ.Pin,GPIO_FUNCTION_IN);
   if Result <> ERROR_SUCCESS then Exit;
  end;

 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function GOODIXIntSync(Touch:PGOODIXTouch):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Touch}
 if Touch = nil then Exit;

 {$IF DEFINED(GOODIX_DEBUG) or DEFINED(TOUCH_DEBUG)}
 if TOUCH_LOG_ENABLED then TouchLogDebug(@Touch.Touch,'GOODIX: Interrupt Sync');
 {$ENDIF}

 {IRQ to Output}
 Result:=GOODIXIntDirectionOutput(Touch,GPIO_LEVEL_LOW);
 if Result <> ERROR_SUCCESS then Exit;

 {Wait (T5: 50ms)}
 MillisecondDelay(50);

 {IRQ to Input}
 Result:=GOODIXIntDirectionInput(Touch);
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
