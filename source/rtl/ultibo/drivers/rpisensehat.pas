{
Raspberry Pi Sense HAT Driver.

Copyright (C) 2024 - SoftOz Pty Ltd.

Arch
====

 <All>

Boards
======

 Raspberry Pi - Model A/B/A+/B+/CM1
 Raspberry Pi - Model Zero/ZeroW
 Raspberry Pi 2 - Model B
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

  Linux - \drivers\mfd\rpisense-core.c - Copyright (C) 2015 Raspberry Pi
  Linux - \drivers\video\fbdev\rpisense-fb.c - Copyright (C) 2015 Raspberry Pi
  Linux - \drivers\input\joystick\rpisense-js.c - Copyright (C) 2015 Raspberry Pi
  
References
==========

  Documentation
  
   https://www.raspberrypi.org/documentation/hardware/sense-hat/README.md
   
  Schematic

   https://www.raspberrypi.org/documentation/hardware/sense-hat/images/Sense-HAT-V1_0.pdf
 
Raspberry Pi Sense Hat
======================
 
 The Sense HAT 8x8 LED matrix is presented in Ultibo as an 8x8 pixel framebuffer. This allows
 full access to the display at both the individual pixel level and also using the higher level
 console functions to display text and graphics on the matrix.
  
 The framebuffer device supports rotation around the full 360 degrees by supplying the
 required rotation value in the framebuffer properties when calling FramebufferAllocate()
 or by calling the Sense HAT specific function RPiSenseFramebufferSetRotation().
 
 As per the official Python libraries, the default rotation (FRAMEBUFFER_ROTATION_0) gives
 correct viewing when the HDMI port is facing downwards.
 
 The Sense HAT joystick appears as a keyboard device and the pressed buttons are received as
 key presses that represent the Left, Right, Up, Down and Enter keys.
  
}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

unit RPiSenseHat; 

interface

uses GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,HeapManager,Devices,GPIO,I2C,Framebuffer,Keyboard,Keymap,SysUtils;

{==============================================================================}
{Global definitions}
{$INCLUDE ..\core\GlobalDefines.inc}
  
{==============================================================================}
const
 {RPiSenseHat specific constants}
 RPISENSE_SIGNATURE = $EAEBECED; 
 
 {Framebuffer}
 RPISENSE_FRAMEBUFFER_DESCRIPTION = 'Raspberry Pi Sense HAT Framebuffer';  {Description of RPiSense framebuffer device}
 
 RPISENSE_PHYSICAL_WIDTH = 8;
 RPISENSE_PHYSICAL_HEIGHT = 8;
 
 {Joystick}
 RPISENSE_JOYSTICK_DESCRIPTION = 'Raspberry Pi Sense HAT Joystick';  {Description of RPiSense joystick device}
 
 RPISENSE_JOYSTICK_KEYMAP:array[0..4] of Word = (
  SCAN_CODE_DOWN_ARROW,
  SCAN_CODE_RIGHT_ARROW,
  SCAN_CODE_UP_ARROW,
  SCAN_CODE_ENTER,
  SCAN_CODE_LEFT_ARROW
 );
 
 {RPiSenseHat register values}
 RPISENSE_FB = $00;
 RPISENSE_WAI = $F0;
 RPISENSE_VER = $F1;
 RPISENSE_KEYS = $F2;
 RPISENSE_EE_WP = $F3;
 
 {RPiSenseHat gamma reset values}
 RPISENSE_GAMMA_VALUES_DEFAULT = 0;
 RPISENSE_GAMMA_VALUES_LOW = 1;
 RPISENSE_GAMMA_VALUES_USER = 2;
 
{==============================================================================}
type
 {RPiSenseHat specific types}
 PRPiSense = ^TRPiSense;
 TRPiSense = record
  Signature:LongWord;             {Signature for entry validation}
  I2C:PI2CDevice;                 {I2C device}
  GPIO:PGPIODevice;               {GPIO device}
  Framebuffer:PFramebufferDevice; {Framebuffer device}
  Joystick:PKeyboardDevice;       {Joystick (Keyboard) device}
 end;
 
 PRPiSenseGamma = ^TRPiSenseGamma;
 TRPiSenseGamma = array[0..31] of Byte;
 
 PRPiSenseFramebuffer = ^TRPiSenseFramebuffer;
 TRPiSenseFramebuffer = record
  {Framebuffer Properties}
  Framebuffer:TFramebufferDevice;
  {RPiSense Properties}
  I2C:PI2CDevice;                                 {The I2C device the device is connected to}
  Width:LongWord;                                 {Framebuffer Width in Pixels (Virtual)}
  Height:LongWord;                                {Framebuffer Height in Pixels (Virtual)}
  Rotation:LongWord;                              {Framebuffer Rotation (eg FRAMEBUFFER_ROTATION_180)}
  DirtyY1:LongWord;                               {First line of dirty region (or Height - 1 if none dirty)}
  DirtyY2:LongWord;                               {Last line of dirty region (or 0 if none dirty)}
  Ready:LongBool;                                 {If True timer should be enabled during Mark operation}
  Blank:LongBool;                                 {If True then display is currently blanked}
  Lock:TMutexHandle;                              {Lock for dirty region redraw}
  Timer:TTimerHandle;                             {Handle for dirty region redraw timer}
  FrameRate:LongWord;                             {Frame rate for display refresh (in Frames Per Second)}
  Gamma:TRPiSenseGamma;                           {Current gamma values}
  GammaUser:TRPiSenseGamma;                       {User gamma values}
  PixelData:array[0..63] of Word;                 {Pixel data for framebuffer update}
  BlockData:array[0..192] of Byte;                {Block data for display update}
 end; 
 
 PRPiSenseJoystick = ^TRPiSenseJoystick;
 TRPiSenseJoystick = record
  {Keyboard Properties}
  Keyboard:TKeyboardDevice;
  {RPiSense Properties}
  I2C:PI2CDevice;                                          {The I2C device the device is connected to}
  GPIO:PGPIODevice;                                        {The GPIO device the device is connected to}
  Pin:LongWord;                                            {The GPIO pin used to signal joystick events (GPIO_PIN_23)}
  Trigger:LongWord;                                        {The GPIO trigger to detect joystick events (GPIO_TRIGGER_RISING)}
  PreviousKeys:LongInt;                                    {The keys pressed on the last GPIO event callback}
  {Statistics Properties}                                                   
  CallbackCount:LongWord;                                  {Number of callback requests received by the device}
 end; 
 
{==============================================================================}
var
 {RPiSenseHat specific variables}
 RPISENSE_I2C_DEVICE:String = 'I2C0';
 RPISENSE_GPIO_DEVICE:String = 'GPIO0';
 RPISENSE_FRAMEBUFFER_ROTATION:LongWord = FRAMEBUFFER_ROTATION_0;
 RPISENSE_FRAMEBUFFER_WIDTH:LongWord = RPISENSE_PHYSICAL_WIDTH;
 RPISENSE_FRAMEBUFFER_HEIGHT:LongWord = RPISENSE_PHYSICAL_HEIGHT;
 
 RPISENSE_I2C_ADDRESS:Word = $46;
 RPISENSE_LSM9DS1_MAGN_ADDRESS:Word = $1C;
 RPISENSE_LSM9DS1_ACCEL_ADDRESS:Word = $6A;
 RPISENSE_LPS25H_PRESS_ADDRESS:Word = $5C;
 RPISENSE_HTS221_HUMID_ADDRESS:Word = $5F;
 
 {RPiSenseHat gamma values}
 RPISENSE_GAMMA_DEFAULT:TRPiSenseGamma = (
  $00, $00, $00, $00, $00, $00, $01, $01,
  $02, $02, $03, $03, $04, $05, $06, $07,
  $08, $09, $0A, $0B, $0C, $0E, $0F, $11,
  $12, $14, $15, $17, $19, $1B, $1D, $1F
 );
 
 RPISENSE_GAMMA_LOW:TRPiSenseGamma = (
  $00, $01, $01, $01, $01, $01, $01, $01,
  $01, $01, $01, $01, $01, $02, $02, $02,
  $03, $03, $03, $04, $04, $05, $05, $06,
  $06, $07, $07, $08, $08, $09, $0A, $0A
 );
 
{==============================================================================}
{Initialization Functions}
procedure RPiSenseInit;
 
function RPiSenseStart(const I2CDevice,GPIODevice:String;Rotation,Width,Height:LongWord):THandle;
function RPiSenseStop(Handle:THandle):Boolean;
 
{==============================================================================}
{RPiSenseHat Functions}
//flip_h
//flip_v
//load_image
//show_message

//get_humidity
//get_temperature
//get_pressure
//get_temperature_from_humidity
//get_temperature_from_pressure

//get_compass
//get_gyroscope
//get_accelerometer
//get_orientation_degrees
//get_orientation_radians

//https://github.com/RPi-Distro/python-sense-hat/blob/master/sense_hat/sense_hat.py
 
{==============================================================================}
{RPiSenseHat Framebuffer Functions}
function RPiSenseFramebufferCreate(I2C:PI2CDevice;const Name:String;Rotation,Width,Height:LongWord):PFramebufferDevice;
function RPiSenseFramebufferDestroy(Framebuffer:PFramebufferDevice):LongWord;

function RPiSenseFramebufferAllocate(Framebuffer:PFramebufferDevice;Properties:PFramebufferProperties):LongWord;
function RPiSenseFramebufferRelease(Framebuffer:PFramebufferDevice):LongWord;

function RPiSenseFramebufferBlank(Framebuffer:PFramebufferDevice;Blank:Boolean):LongWord;

function RPiSenseFramebufferMark(Framebuffer:PFramebufferDevice;X,Y,Width,Height,Flags:LongWord):LongWord;
function RPiSenseFramebufferCommit(Framebuffer:PFramebufferDevice;Address:PtrUInt;Size,Flags:LongWord):LongWord;

function RPiSenseFramebufferSetOffset(Framebuffer:PFramebufferDevice;X,Y:LongWord;Pan:Boolean):LongWord;

function RPiSenseFramebufferGetGamma(Framebuffer:PFramebufferDevice;var Gamma:TRPiSenseGamma):LongWord;
function RPiSenseFramebufferSetGamma(Framebuffer:PFramebufferDevice;const Gamma:TRPiSenseGamma):LongWord;
function RPiSenseFramebufferResetGamma(Framebuffer:PFramebufferDevice;Value:LongWord):LongWord;

function RPiSenseFramebufferSetRotation(Framebuffer:PFramebufferDevice;Rotation:LongWord):LongWord;

procedure RPiSenseFramebufferUpdateDisplay(Framebuffer:PRPiSenseFramebuffer);

{==============================================================================}
{RPiSenseHat Joystick Functions}
function RPiSenseJoystickCreate(I2C:PI2CDevice;GPIO:PGPIODevice;const Name:String):PKeyboardDevice;
function RPiSenseJoystickDestroy(Joystick:PKeyboardDevice):LongWord;

procedure RPiSenseJoystickCallback(Joystick:PRPiSenseJoystick;Pin,Trigger:LongWord);

{==============================================================================}
{RPiSenseHat Helper Functions}
function RPiSenseRegRead(I2C:PI2CDevice;Address:Word;Reg:Byte):LongInt;
function RPiSenseBlockWrite(I2C:PI2CDevice;Address:Word;Data:PByte;Size:Integer):LongWord;
 
{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {RPiSenseHat specific variables}
 RPiSenseInitialized:Boolean;
 
 RPiSenseDefault:THandle = INVALID_HANDLE_VALUE;

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure RPiSenseInit;
{Initialize the RPiSenseHat unit and parameters}

{Note: Called only during system startup}
var
 WorkInt:LongWord;
 WorkBuffer:String;
begin
 {}
 {Check Initialized}
 if RPiSenseInitialized then Exit;
 
 {Check Environment Variables}
 {RPISENSE_AUTOSTART}
 WorkInt:=StrToIntDef(EnvironmentGet('RPISENSE_AUTOSTART'),1);
 if WorkInt = 0 then RPISENSE_AUTOSTART:=False;
 
 {RPISENSE_I2C_DEVICE}
 WorkBuffer:=EnvironmentGet('RPISENSE_I2C_DEVICE');
 if Length(WorkBuffer) <> 0 then RPISENSE_I2C_DEVICE:=WorkBuffer;
 
 {RPISENSE_GPIO_DEVICE}
 WorkBuffer:=EnvironmentGet('RPISENSE_GPIO_DEVICE');
 if Length(WorkBuffer) <> 0 then RPISENSE_GPIO_DEVICE:=WorkBuffer;
 
 {RPISENSE_FRAMEBUFFER_ROTATION}
 WorkInt:=StrToIntDef(EnvironmentGet('RPISENSE_FRAMEBUFFER_ROTATION'),0);
 if WorkInt > FRAMEBUFFER_ROTATION_0 then RPISENSE_FRAMEBUFFER_ROTATION:=WorkInt;

 {RPISENSE_FRAMEBUFFER_WIDTH}
 WorkInt:=StrToIntDef(EnvironmentGet('RPISENSE_FRAMEBUFFER_WIDTH'),0);
 if WorkInt > 0 then RPISENSE_FRAMEBUFFER_WIDTH:=WorkInt;

 {RPISENSE_FRAMEBUFFER_HEIGHT}
 WorkInt:=StrToIntDef(EnvironmentGet('RPISENSE_FRAMEBUFFER_HEIGHT'),0);
 if WorkInt > 0 then RPISENSE_FRAMEBUFFER_HEIGHT:=WorkInt;
 
 {Start RPiSense} 
 if RPISENSE_AUTOSTART then
  begin
   RPiSenseDefault:=RPiSenseStart(RPISENSE_I2C_DEVICE,RPISENSE_GPIO_DEVICE,RPISENSE_FRAMEBUFFER_ROTATION,RPISENSE_FRAMEBUFFER_WIDTH,RPISENSE_FRAMEBUFFER_HEIGHT);
  end;
 
 RPiSenseInitialized:=True;
end;

{==============================================================================}

function RPiSenseStart(const I2CDevice,GPIODevice:String;Rotation,Width,Height:LongWord):THandle;
{Start the RPiSenseHat driver and register the Framebuffer and Joystick devices}
{I2C: The name of the I2C device that the Sense HAT is connected to}
{GPIO: The name of the GPIO device that the Sense HAT is connected to}
{Rotation: The rotation value of the framebuffer (eg FRAMEBUFFER_ROTATION_180)}
{Width: The width of the framebuffer in pixels (Virtual width only, the Physical width is fixed at 8 pixels)}
{Height: The height of the framebuffer in pixels (Virtual height only, the Physical height is fixed at 8 pixels)}
{Return: The handle of the RPiSenseHat on success or INVALID_HANDLE_VALUE on failure}

{Note: This function will be called during startup if the parameter RPISENSE_AUTOSTART is True}
var
 Value:LongInt;
 I2C:PI2CDevice;
 GPIO:PGPIODevice;
 Joystick:PKeyboardDevice;
 Framebuffer:PFramebufferDevice;
 
 RPiSense:PRPiSense;
begin
 {}
 Result:=INVALID_HANDLE_VALUE;
 
 {Check I2C Device}
 if Length(I2CDevice) = 0 then Exit;

 {Check GPIO Device}
 if Length(GPIODevice) = 0 then Exit;
 
 {Check Rotation}
 if Rotation > FRAMEBUFFER_ROTATION_270 then Exit;

 {Check Width and Height}
 if Width < RPISENSE_PHYSICAL_WIDTH then Width:=RPISENSE_PHYSICAL_WIDTH;
 if Height < RPISENSE_PHYSICAL_HEIGHT then Height:=RPISENSE_PHYSICAL_HEIGHT;
 
 {Check I2C Device}
 I2C:=I2CDeviceFindByName(I2CDevice);
 if I2C = nil then
  begin
   I2C:=I2CDeviceFindByDescription(I2CDevice);
   if I2C = nil then Exit;
  end;
 
 {Check GPIO Device}
 GPIO:=GPIODeviceFindByName(GPIODevice);
 if GPIO = nil then
  begin
   GPIO:=GPIODeviceFindByDescription(GPIODevice);
   if GPIO = nil then Exit;
  end;
 
 {Start I2C Device}
 if I2CDeviceStart(I2C,0) <> ERROR_SUCCESS then Exit;
 
 {Read RPISENSE_WAI}
 Value:=RPiSenseRegRead(I2C,RPISENSE_I2C_ADDRESS,RPISENSE_WAI);
 if Value = -1 then Exit;
 if Chr(Value) <> 's' then Exit;
 
 {Read RPISENSE_VER}
 Value:=RPiSenseRegRead(I2C,RPISENSE_I2C_ADDRESS,RPISENSE_VER);
 if Value = -1 then Exit;
 
 {$IFDEF RPISENSEHAT_DEBUG}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'RPiSenseHat: Raspberry Pi Sense HAT firmware version ' + IntToStr(Value));
 {$ENDIF}
 
 {Setup GPIO}
 if GPIODeviceFunctionSelect(GPIO,GPIO_PIN_23,GPIO_FUNCTION_IN) <> ERROR_SUCCESS then Exit;
 
 {Create Framebuffer}
 Framebuffer:=RPiSenseFramebufferCreate(I2C,RPISENSE_FRAMEBUFFER_DESCRIPTION,Rotation,Width,Height);
 if Framebuffer = nil then Exit;
 try
  {Create Joystick}
  Joystick:=RPiSenseJoystickCreate(I2C,GPIO,RPISENSE_JOYSTICK_DESCRIPTION);
  if Joystick = nil then Exit;
  try
   {Create RPiSense}
   RPiSense:=AllocMem(SizeOf(TRPiSense));
   if RPiSense = nil then Exit;
   
   {Update RPiSense}
   RPiSense.Signature:=RPISENSE_SIGNATURE;
   RPiSense.I2C:=I2C;
   RPiSense.GPIO:=GPIO;
   RPiSense.Framebuffer:=Framebuffer;
   RPiSense.Joystick:=Joystick;
   
   {Return Result}
   Result:=THandle(RPiSense);
   
   {Check Default}
   if RPiSenseDefault = INVALID_HANDLE_VALUE then
    begin
     RPiSenseDefault:=Result;
    end;
  finally
   if Result = INVALID_HANDLE_VALUE then RPiSenseJoystickDestroy(Joystick);
  end;
 finally
  if Result = INVALID_HANDLE_VALUE then RPiSenseFramebufferDestroy(Framebuffer);
 end;
end;

{==============================================================================}

function RPiSenseStop(Handle:THandle):Boolean;
{Stop the RPiSenseHat driver and deregister the Framebuffer and Joystick devices}
{Handle: The handle of the RPiSenseHat or INVALID_HANDLE_VALUE for the default}
{Return: True if completed or False on failure}
var
 RPiSense:PRPiSense;
begin
 {}
 Result:=False;
 
 {Check Handle}
 if Handle = INVALID_HANDLE_VALUE then Handle:=RPiSenseDefault;
 if Handle = INVALID_HANDLE_VALUE then Exit;

 {Get RPiSense}
 RPiSense:=PRPiSense(Handle);
 if RPiSense = nil then Exit;
 if RPiSense.Signature <> RPISENSE_SIGNATURE then Exit;
 
 {Check Joystick Device}
 if RPiSense.Joystick <> nil then
  begin
   {Destroy Joystick Device}
   if RPiSenseJoystickDestroy(RPiSense.Joystick) = ERROR_SUCCESS then
    begin
     {Update RPiSense}
     RPiSense.Joystick:=nil;
     
     {Check Framebuffer Device}
     if RPiSense.Framebuffer <> nil then
      begin
       {Destroy Framebuffer Device}
       if RPiSenseFramebufferDestroy(RPiSense.Framebuffer) = ERROR_SUCCESS then
        begin
         {Update RPiSense}
         RPiSense.Framebuffer:=nil;
     
         {Check Default}
         if RPiSenseDefault = THandle(PtrUInt(RPiSense)) then
          begin
           RPiSenseDefault:=INVALID_HANDLE_VALUE;
          end;
         
         {Invalidate RPiSense}
         RPiSense.Signature:=0;
         
         {Destroy RPiSense}
         FreeMem(RPiSense);
         
         {Return Result}
         Result:=True;
        end;
      end;  
    end;  
  end;
end;

{==============================================================================}
{==============================================================================}
{RPiSenseHat Framebuffer Functions}
function RPiSenseFramebufferCreate(I2C:PI2CDevice;const Name:String;Rotation,Width,Height:LongWord):PFramebufferDevice;
{Create, register and allocate a new RPiSenseHat Framebuffer device which can be accessed using the framebuffer API}
{I2C: The I2C device that the RPiSenseHat is connected to}
{Name: The text description of this device which will show in the device list (Optional)}
{Rotation: The rotation value of the framebuffer (eg FRAMEBUFFER_ROTATION_180)}
{Width: The width of the framebuffer in pixels (Virtual width only, the Physical width is fixed at 8 pixels)}
{Height: The height of the framebuffer in pixels (Virtual height only, the Physical height is fixed at 8 pixels)}
{Return: Pointer to the new Framebuffer device or nil if the framebuffer device could not be created}
var
 Status:LongWord;
 RPiSenseFramebuffer:PRPiSenseFramebuffer;
begin
 {}
 Result:=nil;
 
 {$IF DEFINED(RPISENSEHAT_DEBUG) or DEFINED(FRAMEBUFFER_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'RPiSenseHat: Framebuffer Create (Name=' + Name + ' Rotation=' + IntToStr(Rotation) + ' Width=' + IntToStr(Width) + ' Height=' + IntToStr(Height) + ')');
 {$ENDIF}
 
 {Check I2C}
 if I2C = nil then Exit;
 
 {Check Rotation}
 if Rotation > FRAMEBUFFER_ROTATION_270 then Exit;

 {Check Width and Height}
 if Width < RPISENSE_PHYSICAL_WIDTH then Width:=RPISENSE_PHYSICAL_WIDTH;
 if Height < RPISENSE_PHYSICAL_HEIGHT then Height:=RPISENSE_PHYSICAL_HEIGHT;
 
 {Create Framebuffer}
 RPiSenseFramebuffer:=PRPiSenseFramebuffer(FramebufferDeviceCreateEx(SizeOf(TRPiSenseFramebuffer)));
 if RPiSenseFramebuffer <> nil then
  begin
   {Update Framebuffer}
   {Device}
   RPiSenseFramebuffer.Framebuffer.Device.DeviceBus:=DEVICE_BUS_I2C; 
   RPiSenseFramebuffer.Framebuffer.Device.DeviceType:=FRAMEBUFFER_TYPE_HARDWARE;
   RPiSenseFramebuffer.Framebuffer.Device.DeviceFlags:=FRAMEBUFFER_FLAG_DMA or FRAMEBUFFER_FLAG_MARK or FRAMEBUFFER_FLAG_COMMIT or FRAMEBUFFER_FLAG_BLANK or FRAMEBUFFER_FLAG_CACHED or FRAMEBUFFER_FLAG_VIRTUAL or FRAMEBUFFER_FLAG_OFFSETX or FRAMEBUFFER_FLAG_OFFSETY; //{$IFNDEF FPC_BIG_ENDIAN}or FRAMEBUFFER_FLAG_SWAP{$ENDIF FPC_BIG_ENDIAN};
   RPiSenseFramebuffer.Framebuffer.Device.DeviceData:=nil;
   if Length(Name) <> 0 then RPiSenseFramebuffer.Framebuffer.Device.DeviceDescription:=Name else RPiSenseFramebuffer.Framebuffer.Device.DeviceDescription:=RPISENSE_FRAMEBUFFER_DESCRIPTION;
   {Framebuffer}
   RPiSenseFramebuffer.Framebuffer.FramebufferState:=FRAMEBUFFER_STATE_DISABLED;
   RPiSenseFramebuffer.Framebuffer.DeviceAllocate:=RPiSenseFramebufferAllocate;
   RPiSenseFramebuffer.Framebuffer.DeviceRelease:=RPiSenseFramebufferRelease;
   RPiSenseFramebuffer.Framebuffer.DeviceBlank:=RPiSenseFramebufferBlank;
   RPiSenseFramebuffer.Framebuffer.DeviceMark:=RPiSenseFramebufferMark;
   RPiSenseFramebuffer.Framebuffer.DeviceCommit:=RPiSenseFramebufferCommit;
   RPiSenseFramebuffer.Framebuffer.DeviceSetOffset:=RPiSenseFramebufferSetOffset;
   {RPiSenseHat}
   RPiSenseFramebuffer.I2C:=I2C;
   RPiSenseFramebuffer.Width:=Width;
   RPiSenseFramebuffer.Height:=Height;
   RPiSenseFramebuffer.Rotation:=Rotation;
   if (Rotation = FRAMEBUFFER_ROTATION_90) or (Rotation = FRAMEBUFFER_ROTATION_270) then
    begin
     RPiSenseFramebuffer.Width:=Height;
     RPiSenseFramebuffer.Height:=Width;
    end;
   RPiSenseFramebuffer.DirtyY1:=RPISENSE_PHYSICAL_HEIGHT - 1;
   RPiSenseFramebuffer.DirtyY2:=0;
   RPiSenseFramebuffer.Ready:=True;
   RPiSenseFramebuffer.Blank:=False;
   RPiSenseFramebuffer.Lock:=INVALID_HANDLE_VALUE;
   RPiSenseFramebuffer.Timer:=INVALID_HANDLE_VALUE;
   RPiSenseFramebuffer.FrameRate:=100;
   RPiSenseFramebuffer.Gamma:=RPISENSE_GAMMA_DEFAULT;
   RPiSenseFramebuffer.GammaUser:=RPISENSE_GAMMA_DEFAULT;
 
   {Setup Flags}
   {Nothing}
   
   {Register Framebuffer}
   Status:=FramebufferDeviceRegister(@RPiSenseFramebuffer.Framebuffer);
   if Status = ERROR_SUCCESS then
    begin
     {Allocate Framebuffer}
     Status:=FramebufferDeviceAllocate(@RPiSenseFramebuffer.Framebuffer,nil);
     if Status = ERROR_SUCCESS then
      begin
       {Return Result}
       Result:=PFramebufferDevice(RPiSenseFramebuffer); 
      end
     else
      begin
       if DEVICE_LOG_ENABLED then DeviceLogError(nil,'RPiSenseHat: Failed to allocate new framebuffer device: ' + ErrorToString(Status));
       
       {Deregister Framebuffer}
       FramebufferDeviceDeregister(@RPiSenseFramebuffer.Framebuffer);
       
       {Destroy Framebuffer}
       FramebufferDeviceDestroy(@RPiSenseFramebuffer.Framebuffer);
      end;
    end
   else
    begin     
     if DEVICE_LOG_ENABLED then DeviceLogError(nil,'RPiSenseHat: Failed to register new framebuffer device: ' + ErrorToString(Status));

     {Destroy Framebuffer}
     FramebufferDeviceDestroy(@RPiSenseFramebuffer.Framebuffer);
    end;
  end
 else
  begin
   if DEVICE_LOG_ENABLED then DeviceLogError(nil,'RPiSenseHat: Failed to create new framebuffer device');
  end;
end;

{==============================================================================}

function RPiSenseFramebufferDestroy(Framebuffer:PFramebufferDevice):LongWord;
{Release, deregister and destroy an RPiSenseHat Framebuffer device created by this driver}
{Framebuffer: The Framebuffer device to destroy}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 
 {$IF DEFINED(RPISENSEHAT_DEBUG) or DEFINED(FRAMEBUFFER_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'RPiSenseHat: Framebuffer Destroy');
 {$ENDIF}
 
 {Release Framebuffer}
 Result:=FramebufferDeviceRelease(Framebuffer);
 if Result = ERROR_SUCCESS then
  begin
   {Deregister Framebuffer}
   Result:=FramebufferDeviceDeregister(Framebuffer);
   if Result = ERROR_SUCCESS then
    begin
     {Destroy Framebuffer}
     Result:=FramebufferDeviceDestroy(Framebuffer);
     if Result <> ERROR_SUCCESS then
      begin
       if DEVICE_LOG_ENABLED then DeviceLogError(nil,'RPiSenseHat: Failed to destroy framebuffer device: ' + ErrorToString(Result));
      end;
    end
   else
    begin
     if DEVICE_LOG_ENABLED then DeviceLogError(nil,'RPiSenseHat: Failed to deregister framebuffer device: ' + ErrorToString(Result));
    end;    
  end
 else
  begin
   if DEVICE_LOG_ENABLED then DeviceLogError(nil,'RPiSenseHat: Failed to release framebuffer device: ' + ErrorToString(Result));
  end;  
end;

{==============================================================================}

function RPiSenseFramebufferAllocate(Framebuffer:PFramebufferDevice;Properties:PFramebufferProperties):LongWord;
{Implementation of FramebufferDeviceAllocate API for RPiSenseHat Framebuffer}
{Note: Not intended to be called directly by applications, use FramebufferDeviceAllocate instead}
var
 Bytes:LongWord;
 Buffer:Pointer;
 PhysicalWidth:LongWord;
 PhysicalHeight:LongWord;
 VirtualWidth:LongWord;
 VirtualHeight:LongWord;
 Defaults:TFramebufferProperties;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IF DEFINED(RPISENSEHAT_DEBUG) or DEFINED(FRAMEBUFFER_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'RPiSenseHat: Framebuffer Allocate');
 {$ENDIF}
 
 if MutexLock(Framebuffer.Lock) = ERROR_SUCCESS then 
  begin
   try
    {Get Defaults}
    Defaults.Depth:=16;
    Defaults.Order:=FRAMEBUFFER_ORDER_RGB;
    Defaults.Mode:=FRAMEBUFFER_MODE_IGNORED;
    Defaults.PhysicalWidth:=RPISENSE_PHYSICAL_WIDTH;
    Defaults.PhysicalHeight:=RPISENSE_PHYSICAL_HEIGHT;
    Defaults.VirtualWidth:=PRPiSenseFramebuffer(Framebuffer).Width; 
    Defaults.VirtualHeight:=PRPiSenseFramebuffer(Framebuffer).Height; 
    Defaults.OffsetX:=0;                           
    Defaults.OffsetY:=0;                            
    Defaults.OverscanTop:=0;                         
    Defaults.OverscanBottom:=0;                      
    Defaults.OverscanLeft:=0;                        
    Defaults.OverscanRight:=0;                       
    Defaults.Rotation:=PRPiSenseFramebuffer(Framebuffer).Rotation;
 
    {Check Properties}
    if Properties <> nil then
     begin
      {Adjust Physical Width} {Not supported, fixed at 8 pixels}
      PhysicalWidth:=Defaults.PhysicalWidth;
      
      {Adjust Physical Height} {Not supported, fixed at 8 pixels}
      PhysicalHeight:=Defaults.PhysicalHeight;
      
      {Adjust Virtual Width}
      VirtualWidth:=Defaults.VirtualWidth;
      if (Properties.VirtualWidth <> 0) and (Properties.VirtualWidth <> VirtualWidth) then
       begin
        {Check Virtual Width}
        if Properties.VirtualWidth < PhysicalWidth then
         begin
          Properties.VirtualWidth:=PhysicalWidth;
         end;
        
        VirtualWidth:=Properties.VirtualWidth;
        Defaults.VirtualWidth:=VirtualWidth;
       end;
 
      {Adjust Virtual Height}
      VirtualHeight:=Defaults.VirtualHeight;
      if (Properties.VirtualHeight <> 0) and (Properties.VirtualHeight <> VirtualHeight) then
       begin
        {Check Virtual Height}
        if Properties.VirtualHeight < PhysicalHeight then
         begin
          Properties.VirtualHeight:=PhysicalHeight;
         end;

        VirtualHeight:=Properties.VirtualHeight;
        Defaults.VirtualHeight:=VirtualHeight;
       end;
       
      {Adjust Offset X}
      if Properties.OffsetX <> 0 then  
       begin
        {Check Offset X}
        if Properties.OffsetX > ((VirtualWidth - PhysicalWidth) - 1) then
         begin
          Properties.OffsetX:=0;
         end;
        
        Defaults.OffsetX:=Properties.OffsetX;
       end;

      {Adjust Offset Y}
      if Properties.OffsetY <> 0 then  
       begin
        {Check Offset Y}
        if Properties.OffsetY > ((VirtualHeight - PhysicalHeight) - 1) then
         begin
          Properties.OffsetY:=0;
         end;
        
        Defaults.OffsetY:=Properties.OffsetY;
       end; 
 
      {Adjust Depth}
      {Not supported}
      
      {Adjust Order}
      {Not supported}
      
      {Adjust Rotation}
      if Properties.Rotation <= FRAMEBUFFER_ROTATION_270 then Defaults.Rotation:=Properties.Rotation;
 
      {Check Rotation}
      if Properties.Rotation <> PRPiSenseFramebuffer(Framebuffer).Rotation then
       begin
        if (Properties.Rotation = FRAMEBUFFER_ROTATION_90) or (Properties.Rotation = FRAMEBUFFER_ROTATION_270) then 
         begin
          if (PRPiSenseFramebuffer(Framebuffer).Rotation <> FRAMEBUFFER_ROTATION_90) and (PRPiSenseFramebuffer(Framebuffer).Rotation <> FRAMEBUFFER_ROTATION_270) then
           begin
            {Adjust Virtual Only}
            Defaults.VirtualWidth:=VirtualHeight; 
            Defaults.VirtualHeight:=VirtualWidth;
           end;
         end
        else
         begin
          if (PRPiSenseFramebuffer(Framebuffer).Rotation <> FRAMEBUFFER_ROTATION_0) and (PRPiSenseFramebuffer(Framebuffer).Rotation <> FRAMEBUFFER_ROTATION_180) then
           begin
            {Adjust Virtual Only}
            Defaults.VirtualWidth:=VirtualHeight; 
            Defaults.VirtualHeight:=VirtualWidth;
           end;
         end;      
       end;
     end;
 
    {Get Format}
    Defaults.Format:=COLOR_FORMAT_RGB16;    
    
    {Get Bytes}
    Bytes:=ColorFormatToBytes(Defaults.Format);
    if Bytes = 0 then Exit;
    
    {Get Size}
    Defaults.Size:=(Defaults.VirtualWidth * Defaults.VirtualHeight) * Bytes;
    
    {Get Pitch}
    Defaults.Pitch:=Defaults.VirtualWidth * Bytes;
 
    {Allocate Framebuffer}
    if ((Framebuffer.Device.DeviceFlags and FRAMEBUFFER_FLAG_DMA) <> 0) and DMAAvailable then
     begin
      {Allocate DMA Buffer}
      Buffer:=DMAAllocateBuffer(Defaults.Size);
     end
    else
     begin
      {Allocate Normal Buffer (No DMA)}
      {Use DMA Alignment and Multiplier if available}
      if (DMA_ALIGNMENT <> 0) and (DMA_MULTIPLIER <> 0) then
       begin
        Buffer:=GetAlignedMem(RoundUp(Defaults.Size,DMA_MULTIPLIER),DMA_ALIGNMENT);
       end
      else
       begin      
        Buffer:=GetMem(Defaults.Size);
       end; 
     end;
    if Buffer = nil then
     begin
      Result:=ERROR_OPERATION_FAILED;
      Exit; 
     end; 
    
    {Check Cache}
    if not(DMA_CACHE_COHERENT) then
     begin
      {Clean Cache (Dest)}
      CleanDataCacheRange(PtrUInt(Buffer),Defaults.Size);
     end;
 
    {Update Framebuffer}
    Framebuffer.Address:=PtrUInt(Buffer);
    Framebuffer.Size:=Defaults.Size;
    Framebuffer.Pitch:=Defaults.Pitch;
    Framebuffer.Depth:=Defaults.Depth;
    Framebuffer.Order:=Defaults.Order;
    Framebuffer.Mode:=Defaults.Mode;
    Framebuffer.Format:=Defaults.Format;
    Framebuffer.PhysicalWidth:=Defaults.PhysicalWidth;
    Framebuffer.PhysicalHeight:=Defaults.PhysicalHeight;
    Framebuffer.VirtualWidth:=Defaults.VirtualWidth;
    Framebuffer.VirtualHeight:=Defaults.VirtualHeight;
    Framebuffer.OffsetX:=Defaults.OffsetX;
    Framebuffer.OffsetY:=Defaults.OffsetY;
    Framebuffer.OverscanTop:=Defaults.OverscanTop;
    Framebuffer.OverscanBottom:=Defaults.OverscanBottom;
    Framebuffer.OverscanLeft:=Defaults.OverscanLeft;
    Framebuffer.OverscanRight:=Defaults.OverscanRight;
    Framebuffer.Rotation:=Defaults.Rotation;
 
    {Update Dirty Region}
    PRPiSenseFramebuffer(Framebuffer).DirtyY1:=Framebuffer.PhysicalHeight - 1;
    PRPiSenseFramebuffer(Framebuffer).DirtyY2:=0;
    PRPiSenseFramebuffer(Framebuffer).Ready:=True;
    PRPiSenseFramebuffer(Framebuffer).Lock:=MutexCreate;
    if PRPiSenseFramebuffer(Framebuffer).Lock = INVALID_HANDLE_VALUE then
     begin
      Result:=ERROR_OPERATION_FAILED;
      Exit;
     end; 
    
    {Update Blank}
    PRPiSenseFramebuffer(Framebuffer).Blank:=False;
    
    {Create Timer}
    PRPiSenseFramebuffer(Framebuffer).Timer:=TimerCreateEx(MILLISECONDS_PER_SECOND div PRPiSenseFramebuffer(Framebuffer).FrameRate,TIMER_STATE_DISABLED,TIMER_FLAG_WORKER,TTimerEvent(RpiSenseFramebufferUpdateDisplay),Framebuffer); {Scheduled as required}
    if PRPiSenseFramebuffer(Framebuffer).Timer = INVALID_HANDLE_VALUE then
     begin
      Result:=ERROR_OPERATION_FAILED;
      Exit;
     end; 
    
    {Update Statistics}
    Inc(Framebuffer.AllocateCount);
    
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    MutexUnlock(Framebuffer.Lock);
   end; 
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function RPiSenseFramebufferRelease(Framebuffer:PFramebufferDevice):LongWord;
{Implementation of FramebufferDeviceRelease API for RPiSenseHat Framebuffer}
{Note: Not intended to be called directly by applications, use FramebufferDeviceRelease instead}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IF DEFINED(RPISENSEHAT_DEBUG) or DEFINED(FRAMEBUFFER_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'RPiSenseHat: Framebuffer Release');
 {$ENDIF}
 
 if MutexLock(Framebuffer.Lock) = ERROR_SUCCESS then 
  begin
   try
    {Destroy Timer}
    Result:=TimerDestroy(PRPiSenseFramebuffer(Framebuffer).Timer);
    if Result <> ERROR_SUCCESS then Exit;
    PRPiSenseFramebuffer(Framebuffer).Timer:=INVALID_HANDLE_VALUE;
 
    {Release Framebuffer}
    if ((Framebuffer.Device.DeviceFlags and FRAMEBUFFER_FLAG_DMA) <> 0) and DMAAvailable then
     begin
      {Release DMA Buffer}
      Result:=DMAReleaseBuffer(Pointer(Framebuffer.Address));
      if Result <> ERROR_SUCCESS then Exit;
     end
    else
     begin
      {Release Normal Buffer (No DMA)}
      FreeMem(Pointer(Framebuffer.Address));
     end;
     
    {Update Framebuffer}
    Framebuffer.Address:=0;
    Framebuffer.Size:=0;
    Framebuffer.Pitch:=0;
    Framebuffer.Depth:=FRAMEBUFFER_DEPTH_32;
    Framebuffer.Order:=FRAMEBUFFER_ORDER_RGB;
    Framebuffer.Mode:=FRAMEBUFFER_MODE_ENABLED;
    Framebuffer.Format:=COLOR_FORMAT_DEFAULT;
    Framebuffer.PhysicalWidth:=0;
    Framebuffer.PhysicalHeight:=0;
    Framebuffer.VirtualWidth:=0;
    Framebuffer.VirtualHeight:=0;
    Framebuffer.OffsetX:=0;
    Framebuffer.OffsetY:=0;
    Framebuffer.OverscanTop:=0;
    Framebuffer.OverscanBottom:=0;
    Framebuffer.OverscanLeft:=0;
    Framebuffer.OverscanRight:=0;
    Framebuffer.Rotation:=FRAMEBUFFER_ROTATION_0;
    
    {Update Dirty Region}
    MutexDestroy(PRPiSenseFramebuffer(Framebuffer).Lock);
    PRPiSenseFramebuffer(Framebuffer).Lock:=INVALID_HANDLE_VALUE;
    PRPiSenseFramebuffer(Framebuffer).DirtyY1:=RPISENSE_PHYSICAL_HEIGHT - 1;
    PRPiSenseFramebuffer(Framebuffer).DirtyY2:=0;
    PRPiSenseFramebuffer(Framebuffer).Ready:=True;
    
    {Update Blank}
    PRPiSenseFramebuffer(Framebuffer).Blank:=False;
    
    {Update Statistics}
    Inc(Framebuffer.ReleaseCount);
     
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    MutexUnlock(Framebuffer.Lock);
   end; 
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function RPiSenseFramebufferBlank(Framebuffer:PFramebufferDevice;Blank:Boolean):LongWord;
{Implementation of FramebufferDevicBlank API for RPiSenseHat Framebuffer}
{Note: Not intended to be called directly by applications, use FramebufferDevicBlank instead}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 

 {$IF DEFINED(RPISENSEHAT_DEBUG) or DEFINED(FRAMEBUFFER_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'RPiSenseHat: Framebuffer Blank (Blank=' + BooleanToString(Blank) + ')');
 {$ENDIF}
 
 if MutexLock(Framebuffer.Lock) = ERROR_SUCCESS then 
  begin
   try
    {Update Blank}
    PRPiSenseFramebuffer(Framebuffer).Blank:=Blank;
    
    {Mark Update}
    Result:=FramebufferDeviceMark(Framebuffer,0,0,Framebuffer.PhysicalWidth,Framebuffer.PhysicalHeight,FRAMEBUFFER_TRANSFER_NONE);
   finally
    MutexUnlock(Framebuffer.Lock);
   end; 
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function RPiSenseFramebufferMark(Framebuffer:PFramebufferDevice;X,Y,Width,Height,Flags:LongWord):LongWord;
{Implementation of FramebufferDeviceMark API for RPiSenseHat Framebuffer}
{Note: Not intended to be called directly by applications, use FramebufferDeviceMark instead}
{Note: Marks full lines only, X and Width are ignored for RPiSenseHat Framebuffer}
var
 Enable:Boolean;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IF DEFINED(RPISENSEHAT_DEBUG) or DEFINED(FRAMEBUFFER_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'RPiSenseHat: Framebuffer Mark (X=' + IntToStr(X) + ' Y=' + IntToStr(Y) + ' Width=' + IntToStr(Width) + ' Height=' + IntToStr(Height) + ')');
 {$ENDIF}
 
 {Acquire Dirty Region Lock}
 if MutexLock(PRPiSenseFramebuffer(Framebuffer).Lock) = ERROR_SUCCESS then 
  begin
   try
    Enable:=False;
     
    {Check Y}
    if Y < PRPiSenseFramebuffer(Framebuffer).DirtyY1 then
     begin
      PRPiSenseFramebuffer(Framebuffer).DirtyY1:=Y;
      Enable:=True;
     end;
    
    {Check Height}
    if (Y + (Height - 1)) > PRPiSenseFramebuffer(Framebuffer).DirtyY2 then
     begin
      PRPiSenseFramebuffer(Framebuffer).DirtyY2:=(Y + (Height - 1));
      Enable:=True;
     end;
 
    {Check Enable and Ready}
    if Enable and (PRPiSenseFramebuffer(Framebuffer).Ready) then
     begin
      {Enable Timer}
      Result:=TimerEnable(PRPiSenseFramebuffer(Framebuffer).Timer);
      if Result = ERROR_SUCCESS then
       begin
        {Clear Ready}
        PRPiSenseFramebuffer(Framebuffer).Ready:=False;
       end; 
     end
    else
     begin
      Result:=ERROR_SUCCESS;
     end;
   finally
    {Release Dirty Region Lock}
    MutexUnlock(PRPiSenseFramebuffer(Framebuffer).Lock);
   end; 
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function RPiSenseFramebufferCommit(Framebuffer:PFramebufferDevice;Address:PtrUInt;Size,Flags:LongWord):LongWord;
{Implementation of FramebufferDeviceCommit API for RPiSenseHat Framebuffer}
{Note: Not intended to be called directly by applications, use FramebufferDeviceCommit instead}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IF DEFINED(RPISENSEHAT_DEBUG) or DEFINED(FRAMEBUFFER_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'RPiSenseHat: Framebuffer Commit (Address=' + IntToHex(Address,8) + ' Size=' + IntToStr(Size) + ')');
 {$ENDIF}
 
 {Check Flags}
 if (Flags and FRAMEBUFFER_TRANSFER_DMA) = 0 then
  begin
   {Clean Cache}
   CleanAndInvalidateDataCacheRange(Address,Size); 
  end
 else
  begin
   {Invalidate Cache}
   InvalidateDataCacheRange(Address,Size);
  end;  
 
 {Return Result}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

function RPiSenseFramebufferSetOffset(Framebuffer:PFramebufferDevice;X,Y:LongWord;Pan:Boolean):LongWord;
{Implementation of FramebufferDeviceSetOffset API for RPiSenseHat Framebuffer}
{Note: Not intended to be called directly by applications, use FramebufferDeviceSetOffset instead}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit;
 
 {$IF DEFINED(RPISENSEHAT_DEBUG) or DEFINED(FRAMEBUFFER_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'RPiSenseHat: Framebuffer Set Offset (X=' + IntToStr(X) + ' Y=' + IntToStr(Y) + ')');
 {$ENDIF}

 if MutexLock(Framebuffer.Lock) = ERROR_SUCCESS then 
  begin
   try
    {Check Offsets}
    if (X > 0) and (X > ((Framebuffer.VirtualWidth - Framebuffer.PhysicalWidth) - 1)) then Exit;
    if (Y > 0) and (Y > ((Framebuffer.VirtualHeight - Framebuffer.PhysicalHeight) - 1)) then Exit;
    
    {Update Offset}
    if not(Pan) then
     begin
      Framebuffer.OffsetX:=X;
      Framebuffer.OffsetY:=Y;
     end; 
    
    {Mark Update}
    Result:=FramebufferDeviceMark(Framebuffer,0,0,Framebuffer.PhysicalWidth,Framebuffer.PhysicalHeight,FRAMEBUFFER_TRANSFER_NONE);
   finally
    MutexUnlock(Framebuffer.Lock);
   end; 
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function RPiSenseFramebufferGetGamma(Framebuffer:PFramebufferDevice;var Gamma:TRPiSenseGamma):LongWord;
{Get the current gamma values from the RPiSenseHat framebuffer}
{Framebuffer: The Framebuffer device to get from}
{Gamma: An array of gamma values returned on completion}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IF DEFINED(RPISENSEHAT_DEBUG) or DEFINED(FRAMEBUFFER_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'RPiSenseHat: Framebuffer Get Gamma');
 {$ENDIF}
 
 if MutexLock(Framebuffer.Lock) = ERROR_SUCCESS then 
  begin
   try
    {Get Gamma}
    Gamma:=PRPiSenseFramebuffer(Framebuffer).Gamma;
    
    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    MutexUnlock(Framebuffer.Lock);
   end; 
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function RPiSenseFramebufferSetGamma(Framebuffer:PFramebufferDevice;const Gamma:TRPiSenseGamma):LongWord;
{Set the current gamma values for the RPiSenseHat framebuffer}
{Framebuffer: The Framebuffer device to set for}
{Gamma: An array of gamma values to set}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IF DEFINED(RPISENSEHAT_DEBUG) or DEFINED(FRAMEBUFFER_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'RPiSenseHat: Framebuffer Set Gamma');
 {$ENDIF}
 
 if MutexLock(Framebuffer.Lock) = ERROR_SUCCESS then 
  begin
   try
    {Set Gamma}
    PRPiSenseFramebuffer(Framebuffer).Gamma:=Gamma;
    
    {Mark Update}
    Result:=FramebufferDeviceMark(Framebuffer,0,0,Framebuffer.PhysicalWidth,Framebuffer.PhysicalHeight,FRAMEBUFFER_TRANSFER_NONE);
   finally
    MutexUnlock(Framebuffer.Lock);
   end; 
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function RPiSenseFramebufferResetGamma(Framebuffer:PFramebufferDevice;Value:LongWord):LongWord;
{Reset the current gamma values for the RPiSenseHat framebuffer to Default, Low or User}
{Framebuffer: The Framebuffer device to set for}
{Value: The gamma values to set (0 = Default / 1 = Low / 2 = User)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IF DEFINED(RPISENSEHAT_DEBUG) or DEFINED(FRAMEBUFFER_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'RPiSenseHat: Framebuffer Reset Gamma (Value=' + IntToStr(Value) + ')');
 {$ENDIF}
 
 {Check Value}
 if Value > 2 then Exit;
 
 if MutexLock(Framebuffer.Lock) = ERROR_SUCCESS then 
  begin
   try
    {Update Gamma}
    case Value of
     0:PRPiSenseFramebuffer(Framebuffer).Gamma:=RPISENSE_GAMMA_DEFAULT;
     1:PRPiSenseFramebuffer(Framebuffer).Gamma:=RPISENSE_GAMMA_LOW;
     2:PRPiSenseFramebuffer(Framebuffer).Gamma:=PRPiSenseFramebuffer(Framebuffer).GammaUser;
    end;
   
    {Mark Update}
    Result:=FramebufferDeviceMark(Framebuffer,0,0,Framebuffer.PhysicalWidth,Framebuffer.PhysicalHeight,FRAMEBUFFER_TRANSFER_NONE);
   finally
    MutexUnlock(Framebuffer.Lock);
   end; 
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function RPiSenseFramebufferSetRotation(Framebuffer:PFramebufferDevice;Rotation:LongWord):LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IF DEFINED(RPISENSEHAT_DEBUG) or DEFINED(FRAMEBUFFER_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'RPiSenseHat: Framebuffer Set Rotation (Rotation=' + IntToStr(Rotation) + ')');
 {$ENDIF}
 
 {Check Rotation}
 if Rotation > FRAMEBUFFER_ROTATION_270 then Exit;
 
 if MutexLock(Framebuffer.Lock) = ERROR_SUCCESS then 
  begin
   try
    {Update Rotation}
    Framebuffer.Rotation:=Rotation;
    PRPiSenseFramebuffer(Framebuffer).Rotation:=Rotation;
    
    {Mark Update}
    Result:=FramebufferDeviceMark(Framebuffer,0,0,Framebuffer.PhysicalWidth,Framebuffer.PhysicalHeight,FRAMEBUFFER_TRANSFER_NONE);
   finally
    MutexUnlock(Framebuffer.Lock);
   end; 
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

procedure RPiSenseFramebufferUpdateDisplay(Framebuffer:PRPiSenseFramebuffer);
{Timer function for RPiSenseHat framebuffer dirty region redraw}
{Note: Not intended to be called directly by applications}
var
 Row:PtrUInt;
 Col:PtrUInt;
 Offset:PtrUInt;
 Address:PtrUInt;
 
 Unlock:Boolean;
 DirtyY1:LongWord;
 DirtyY2:LongWord;
begin
 {}
 {Check Framebuffer}
 if Framebuffer = nil then Exit;
 if Framebuffer.Framebuffer.Device.Signature <> DEVICE_SIGNATURE then Exit; 
 
 {$IF DEFINED(RPISENSEHAT_DEBUG) or DEFINED(FRAMEBUFFER_DEBUG)}
 if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'RPiSenseHat: Framebuffer Update Display');
 {$ENDIF}
 
 {Acquire Dirty Region Lock}
 if MutexLock(Framebuffer.Lock) = ERROR_SUCCESS then 
  begin
   try
    Unlock:=True;
    
    {Get Dirty Region}
    DirtyY1:=Framebuffer.DirtyY1;
    DirtyY2:=Framebuffer.DirtyY2;
    
    {Reset Dirty Region}
    Framebuffer.DirtyY1:=Framebuffer.Framebuffer.PhysicalHeight - 1;
    Framebuffer.DirtyY2:=0;
    Framebuffer.Ready:=False;

    {Check Dirty Region}
    if DirtyY1 > DirtyY2 then
     begin
      {$IF DEFINED(RPISENSEHAT_DEBUG) or DEFINED(FRAMEBUFFER_DEBUG)}
      if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'RPiSenseHat: Error dirty region start is greater than end (DirtyY1=' + IntToStr(DirtyY1) + ' DirtyY2=' + IntToStr(DirtyY2) + ')');
      {$ENDIF}
      
      {Set Ready}
      Framebuffer.Ready:=True;
      Exit;
     end;
 
    {Check Dirty Region}
    if (DirtyY1 > (Framebuffer.Framebuffer.PhysicalHeight - 1)) or (DirtyY2 > (Framebuffer.Framebuffer.PhysicalHeight - 1)) then
     begin
      {$IF DEFINED(RPISENSEHAT_DEBUG) or DEFINED(FRAMEBUFFER_DEBUG)}
      if DEVICE_LOG_ENABLED then DeviceLogDebug(nil,'RPiSenseHat: Error dirty region start or end is greater than maximum (DirtyY1=' + IntToStr(DirtyY1) + ' DirtyY2=' + IntToStr(DirtyY2) + ')');
      {$ENDIF}
      
      {Set Ready}
      Framebuffer.Ready:=True;
      Exit;
     end;
 
    {Check Blank}
    if Framebuffer.Blank then
     begin
      {Fill Pixel Data}
      FillChar(Framebuffer.PixelData[0],SizeOf(Framebuffer.PixelData),0);
     end
    else
     begin    
      {Get Address}
      Address:=Framebuffer.Framebuffer.Address + (Framebuffer.Framebuffer.OffsetY * Framebuffer.Framebuffer.Pitch) + (Framebuffer.Framebuffer.OffsetX * (Framebuffer.Framebuffer.Depth shr 3));
      
      {Check Rotation}
      case Framebuffer.Framebuffer.Rotation of
       FRAMEBUFFER_ROTATION_0:begin
         {No Rotation}
         Offset:=0;
         
         {Copy Pixel Data}
         for Row:=0 to 7 do
          begin
           for Col:=0 to 7 do
            begin
             {Store Pixel}
             Framebuffer.PixelData[Offset]:=PWord(Address + (Col * (Framebuffer.Framebuffer.Depth shr 3)))^;
             
             {Update Offset}
             Inc(Offset);
            end;
            
           {Update Address} 
           Inc(Address,Framebuffer.Framebuffer.Pitch);
          end;
        end;
       FRAMEBUFFER_ROTATION_90:begin
         {90 degree rotation}
         {Copy Pixel Data}
         for Row:=0 to 7 do
          begin
           {Update Offset}
           Offset:=7 - Row;
           
           for Col:=0 to 7 do
            begin
             {Store Pixel}
             Framebuffer.PixelData[Offset]:=PWord(Address + (Col * (Framebuffer.Framebuffer.Depth shr 3)))^;
             
             {Update Offset}
             Inc(Offset,8);
            end;
           
           {Update Address} 
           Inc(Address,Framebuffer.Framebuffer.Pitch);
          end;
        end;
       FRAMEBUFFER_ROTATION_180:begin
         {180 degree rotation}
         Offset:=63;
         
         {Copy Pixel Data}
         for Row:=0 to 7 do
          begin
           for Col:=0 to 7 do
            begin
             {Store Pixel}
             Framebuffer.PixelData[Offset]:=PWord(Address + (Col * (Framebuffer.Framebuffer.Depth shr 3)))^;
             
             {Update Offset}
             Dec(Offset);
            end;
            
           {Update Address} 
           Inc(Address,Framebuffer.Framebuffer.Pitch);
          end;
        end;
       FRAMEBUFFER_ROTATION_270:begin
         {270 degree rotation}
         {Copy Pixel Data}
         for Row:=0 to 7 do
          begin
           {Update Offset}
           Offset:=56 + Row;
           
           for Col:=0 to 7 do
            begin
             {Store Pixel}
             Framebuffer.PixelData[Offset]:=PWord(Address + (Col * (Framebuffer.Framebuffer.Depth shr 3)))^;
             
             {Update Offset}
             Dec(Offset,8);
            end;
           
           {Update Address} 
           Inc(Address,Framebuffer.Framebuffer.Pitch);
          end;
        end;
      end;
     end;  
    
    {Release Dirty Region Lock}
    MutexUnlock(Framebuffer.Lock);
    Unlock:=False;
    
    {Create Block Data}
    for Row:=0 to 7 do
     begin
      for Col:=0 to 7 do
       begin
		Framebuffer.BlockData[(Row * 24) + Col + 1]:=Framebuffer.Gamma[(Framebuffer.PixelData[(Row * 8) + Col] shr 11) and $1F];
		Framebuffer.BlockData[(Row * 24) + (Col + 8) + 1]:=Framebuffer.Gamma[(Framebuffer.PixelData[(Row * 8) + Col] shr 6) and $1F];
		Framebuffer.BlockData[(Row * 24) + (Col + 16) + 1]:=Framebuffer.Gamma[(Framebuffer.PixelData[(Row * 8) + Col]) and $1F];
       end;
     end;
    
    {Write Block Data}
    RPiSenseBlockWrite(Framebuffer.I2C,RPISENSE_I2C_ADDRESS,@Framebuffer.BlockData,193);
    
    {Acquire Dirty Region Lock}
    if MutexLock(Framebuffer.Lock) <> ERROR_SUCCESS then Exit;
    Unlock:=True;
    
    {Check Dirty}
    if Framebuffer.DirtyY1 <= Framebuffer.DirtyY2 then
     begin
      {Enable Timer}
      TimerEnable(Framebuffer.Timer);
     end
    else
     begin
      {Set Ready}
      Framebuffer.Ready:=True;
     end;     
   finally
    {Release Dirty Region Lock}
    if Unlock then MutexUnlock(Framebuffer.Lock);
   end; 
  end;
end;
 
{==============================================================================}
{==============================================================================}
{RPiSenseHat Joystick Functions}
function RPiSenseJoystickCreate(I2C:PI2CDevice;GPIO:PGPIODevice;const Name:String):PKeyboardDevice;
{Create, register and attach a new RPiSenseHat Joystick device which can be accessed using the keyboard API}
{I2C: The I2C device that the RPiSenseHat is connected to}
{GPIO: The GPIO device that the RPiSenseHat is connected to}
{Name: The text description of this device which will show in the device list (Optional)}
{Return: Pointer to the new Joystick (Keyboard) device or nil if the device could not be created}
var
 Status:LongWord;
 RPiSenseJoystick:PRPiSenseJoystick;
begin
 {}
 Result:=nil;
 
 {$IF DEFINED(RPISENSEHAT_DEBUG) or DEFINED(KEYBOARD_DEBUG)}
 if KEYBOARD_LOG_ENABLED then KeyboardLogDebug(nil,'RPiSenseHat: Joystick Create (Name=' + Name + ')');
 {$ENDIF}
 
 {Check I2C}
 if I2C = nil then Exit;

 {Check GPIO}
 if GPIO = nil then Exit;
 
 {Create Joystick}
 RPiSenseJoystick:=PRPiSenseJoystick(KeyboardDeviceCreateEx(SizeOf(TRPiSenseJoystick)));
 if RPiSenseJoystick <> nil then
  begin
   {Update Keyboard}
   {Device}
   RPiSenseJoystick.Keyboard.Device.DeviceBus:=DEVICE_BUS_I2C; 
   RPiSenseJoystick.Keyboard.Device.DeviceType:=KEYBOARD_TYPE_NONE;
   RPiSenseJoystick.Keyboard.Device.DeviceFlags:=RPiSenseJoystick.Keyboard.Device.DeviceFlags; {Don't override defaults}
   RPiSenseJoystick.Keyboard.Device.DeviceData:=nil;
   if Length(Name) <> 0 then RPiSenseJoystick.Keyboard.Device.DeviceDescription:=Name else RPiSenseJoystick.Keyboard.Device.DeviceDescription:=RPISENSE_JOYSTICK_DESCRIPTION;
   {Keyboard}
   RPiSenseJoystick.Keyboard.KeyboardState:=KEYBOARD_STATE_ATTACHING;
   {RPiSenseHat}
   RPiSenseJoystick.I2C:=I2C;
   RPiSenseJoystick.GPIO:=GPIO;
   RPiSenseJoystick.Pin:=GPIO_PIN_23;
   RPiSenseJoystick.Trigger:=GPIO_TRIGGER_RISING;
   
   {Register Keyboard}
   Status:=KeyboardDeviceRegister(@RPiSenseJoystick.Keyboard);
   if Status = ERROR_SUCCESS then
    begin
     {Register for the GPIO Event}
     Status:=GPIODeviceInputEvent(RPiSenseJoystick.GPIO,RPiSenseJoystick.Pin,RPiSenseJoystick.Trigger,GPIO_EVENT_FLAG_REPEAT,INFINITE,TGPIOCallback(RPiSenseJoystickCallback),RPiSenseJoystick);
     if Status <> ERROR_SUCCESS then
      begin
       if KEYBOARD_LOG_ENABLED then KeyboardLogError(nil,'RPiSenseHat: Failed to register GPIO Joystick event: ' + ErrorToString(Status));
       Exit;
      end; 
     
     {Set State to Attached}
     if KeyboardDeviceSetState(@RPiSenseJoystick.Keyboard,KEYBOARD_STATE_ATTACHED) <> ERROR_SUCCESS then Exit;
     
     {Return Result}
     Result:=PKeyboardDevice(RPiSenseJoystick); 
    end
   else
    begin
     if KEYBOARD_LOG_ENABLED then KeyboardLogError(nil,'RPiSenseHat: Failed to register new Joystick device: ' + ErrorToString(Status));
     
     {Destroy Keyboard}
     KeyboardDeviceDestroy(@RPiSenseJoystick.Keyboard);
    end;
  end
 else 
  begin
   if KEYBOARD_LOG_ENABLED then KeyboardLogError(nil,'RPiSenseHat: Failed to create new Joystick device');
  end;
end;

{==============================================================================}

function RPiSenseJoystickDestroy(Joystick:PKeyboardDevice):LongWord;
{Detach, deregister and destroy a RPiSenseHat Joystick device created by this driver}
{Joystick: The Joystick (Keyboard) device to destroy}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check Joystick}
 if Joystick = nil then Exit;
 
 {$IF DEFINED(RPISENSEHAT_DEBUG) or DEFINED(KEYBOARD_DEBUG)}
 if KEYBOARD_LOG_ENABLED then KeyboardLogDebug(Joystick,'RPiSenseHat: Joystick Destroy');
 {$ENDIF}

 {Cancel the GPIO Event} 
 GPIODeviceInputCancel(PRPiSenseJoystick(Joystick).GPIO,PRPiSenseJoystick(Joystick).Pin);
 
 {Deregister Joystick}
 Result:=KeyboardDeviceDeregister(Joystick);
 if Result <> ERROR_SUCCESS then
  begin
   if KEYBOARD_LOG_ENABLED then KeyboardLogError(nil,'RPiSenseHat: Failed to deregister Joystick device: ' + ErrorToString(Result));
   Exit;
  end;

 {Destroy Joystick}
 Result:=KeyboardDeviceDestroy(Joystick);
 if Result <> ERROR_SUCCESS then
  begin
   if KEYBOARD_LOG_ENABLED then KeyboardLogError(nil,'RPiSenseHat: Failed to destroy Joystick device: ' + ErrorToString(Result));
   Exit;
  end; 
 
 {Return Result}
 Result:=ERROR_SUCCESS;
end;

{==============================================================================}

procedure RPiSenseJoystickCallback(Joystick:PRPiSenseJoystick;Pin,Trigger:LongWord);
{Callback function for the RPiSenseHat Joystick device, called on a worker thread when
 the registered GPIO event is triggered by a rising edge on the specified pin}
{Note: Not intended to be called directly by applications}
var
 Index:Byte;
 Count:Integer;
 
 Keys:LongInt;
 Changes:LongInt;
 
 Data:TKeyboardData;
 Keymap:TKeymapHandle;
begin
 {}
 {Check Joystick}
 if Joystick = nil then Exit;
 
 {$IF DEFINED(RPISENSEHAT_DEBUG) or DEFINED(KEYBOARD_DEBUG)}
 if KEYBOARD_LOG_ENABLED then KeyboardLogDebug(@Joystick.Keyboard,'RPiSenseHat: Joystick Callback (Pin=' + GPIOPinToString(Pin) + ' Trigger=' + GPIOTriggerToString(Trigger) + ')');
 {$ENDIF}
 
 {Update Statistics}
 Inc(Joystick.CallbackCount);
 
 {Read Keys}
 Keys:=RPiSenseRegRead(Joystick.I2C,RPISENSE_I2C_ADDRESS,RPISENSE_KEYS);
 if Keys = -1 then Exit;
 
 {Acquire the Lock}
 if MutexLock(Joystick.Keyboard.Lock) = ERROR_SUCCESS then
  begin
   try
    {Get Keymap}
    Keymap:=KeymapGetDefault;
    
    {Get Keymap Index}
    Index:=KEYMAP_INDEX_NORMAL;
    
    {Get Changes}
    Changes:=Keys xor Joystick.PreviousKeys;
    
    {Save Keys}
    Joystick.PreviousKeys:=Keys;
    
    {Clear Keyboard Data}
    FillChar(Data,SizeOf(TKeyboardData),0);
    
    {Check Changes}
    for Count:=0 to 4 do
     begin
      {Check Change}
      if (Changes and 1) <> 0 then
       begin
        {Get Data}
        Data.ScanCode:=RPISENSE_JOYSTICK_KEYMAP[Count];
        Data.KeyCode:=KeymapGetKeyCode(Keymap,Data.ScanCode,Index);
        Data.CharCode:=KeymapGetCharCode(Keymap,Data.KeyCode);
        Data.CharUnicode:=KeymapGetCharUnicode(Keymap,Data.KeyCode);
        
        {Check Keyup or Keydown}
        if (Keys and 1) <> 0 then
         begin
          Data.Modifiers:=KEYBOARD_KEYDOWN;
          
          {$IF DEFINED(RPISENSEHAT_DEBUG) or DEFINED(KEYBOARD_DEBUG)}
          if KEYBOARD_LOG_ENABLED then
           begin
            KeyboardLogDebug(@Joystick.Keyboard,'RPiSenseHat: Key Pressed (ScanCode=' + IntToStr(Data.ScanCode) + ' Modifiers=' + IntToHex(Data.Modifiers,8) + ' Index=' + IntToStr(Index) + ')');
            KeyboardLogDebug(@Joystick.Keyboard,'RPiSenseHat: Key Pressed (KeyCode=' + IntToHex(Data.KeyCode,4) + ' CharCode=' + IntToHex(Byte(Data.CharCode),2) + ' CharUnicode=' + IntToHex(Word(Data.CharUnicode),4) + ')');
           end; 
          {$ENDIF}
         end
        else
         begin
          Data.Modifiers:=KEYBOARD_KEYUP;
  
          {$IF DEFINED(RPISENSEHAT_DEBUG) or DEFINED(KEYBOARD_DEBUG)}
          if KEYBOARD_LOG_ENABLED then 
           begin
            KeyboardLogDebug(@Joystick.Keyboard,'RPiSenseHat: Key Released (ScanCode=' + IntToStr(Data.ScanCode) + ' Modifiers=' + IntToHex(Data.Modifiers,8) + ' Index=' + IntToStr(Index)+ ')');
            KeyboardLogDebug(@Joystick.Keyboard,'RPiSenseHat: Key Released (KeyCode=' + IntToHex(Data.KeyCode,4) + ' CharCode=' + IntToHex(Byte(Data.CharCode),2) + ' CharUnicode=' + IntToHex(Word(Data.CharUnicode),4) + ')');
           end; 
          {$ENDIF}
         end;         
       
        {Insert Data}
        KeyboardInsertData(@Joystick.Keyboard,@Data,True);
       end;
       
      {Update Keys and Changes}
      Keys:=Keys shr 1;
      Changes:=Changes shr 1;
     end;
   finally
    {Release the Lock}
    MutexUnlock(Joystick.Keyboard.Lock);
   end;
  end
 else
  begin
   if KEYBOARD_LOG_ENABLED then KeyboardLogError(@Joystick.Keyboard,'RPiSenseHat: Failed to acquire lock');
  end;
end;

{==============================================================================}
{==============================================================================}
{RPiSenseHat Helper Functions}
function RPiSenseRegRead(I2C:PI2CDevice;Address:Word;Reg:Byte):LongInt;
{Read a byte from an RPiSenseHat register}
{I2C: The I2C device the RPiSenseHat is connected to}
{Address: The I2C address of the RPiSenseHat}
{Reg: The register to read}
{Return: The register value on success or -1 on failure}
var
 Data:Byte;
 Count:LongWord;
 Status:LongWord;
begin
 {}
 Result:=-1;
 
 {Check I2C}
 if I2C = nil then Exit;
 
 {Read Register}
 Status:=I2CDeviceWriteRead(I2C,Address,@Reg,SizeOf(Byte),@Data,SizeOf(Byte),Count);
 if (Status <> ERROR_SUCCESS) or (Count <> SizeOf(Byte)) then
  begin
   if DEVICE_LOG_ENABLED then DeviceLogError(nil,'RPiSenseHat: Read from register ' + IntToHex(Reg,2) + ' failed: ' + ErrorToString(Status));
   Exit;
  end; 
 
 {Due to the BCM270x I2C clock stretching bug, some values may have MSB set. Clear it to avoid incorrect values}
 Result:=Data and $7F;
end;

{==============================================================================}

function RPiSenseBlockWrite(I2C:PI2CDevice;Address:Word;Data:PByte;Size:Integer):LongWord;
{Write a block of data to the RPiSenseHat}
{I2C: The I2C device the RPiSenseHat is connected to}
{Address: The I2C address of the RPiSenseHat}
{Data: Pointer to the data block to be written}
{Size: Size of the data block in bytes}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Count:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;
 
 {Check I2C}
 if I2C = nil then Exit;
 
 {Write Data}
 Result:=I2CDeviceWrite(I2C,Address,Data,Size,Count);
 if Result <> ERROR_SUCCESS then
  begin
   if DEVICE_LOG_ENABLED then DeviceLogError(nil,'RPiSenseHat: Block write failed: ' + ErrorToString(Result));
  end;
end;

{==============================================================================}
{==============================================================================}

initialization
 RPiSenseInit;
 
{==============================================================================}
 
{finalization}
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
