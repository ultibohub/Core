{
Ultibo Touch interface unit.

Copyright (C) 2024 - SoftOz Pty Ltd.

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


References
==========


Touch Devices
=============

}

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

{$IFNDEF FPC_DOTTEDUNITS}
unit Touch;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  Core.GlobalConfig,
  Core.GlobalConst,
  Core.GlobalTypes,
  Core.Platform,
  Core.Threads,
  Core.Devices,
  System.SysUtils;
{$ELSE FPC_DOTTEDUNITS}
uses
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  Devices,
  SysUtils;
{$ENDIF FPC_DOTTEDUNITS}

{==============================================================================}
{Global definitions}
{$INCLUDE GlobalDefines.inc}

{==============================================================================}
const
 {Touch specific constants}
 TOUCH_NAME_PREFIX = 'Touch';  {Name prefix for Touch Devices}

 {Touch Device Types}
 TOUCH_TYPE_NONE       = 0;
 TOUCH_TYPE_RESISTIVE  = 1;
 TOUCH_TYPE_CAPACITIVE = 2;

 TOUCH_TYPE_MAX        = 2;

 {Touch Type Names}
 TOUCH_TYPE_NAMES:array[TOUCH_TYPE_NONE..TOUCH_TYPE_MAX] of String = (
  'TOUCH_TYPE_NONE',
  'TOUCH_TYPE_RESISTIVE',
  'TOUCH_TYPE_CAPACITIVE');

 {Touch Device States}
 TOUCH_STATE_DISABLED = 0;
 TOUCH_STATE_ENABLED  = 1;

 TOUCH_STATE_MAX      = 1;

 {Touch State Names}
 TOUCH_STATE_NAMES:array[TOUCH_STATE_DISABLED..TOUCH_STATE_MAX] of String = (
  'TOUCH_STATE_DISABLED',
  'TOUCH_STATE_ENABLED');

 {Touch Device Flags}
 TOUCH_FLAG_NONE          = $00000000;
 TOUCH_FLAG_NON_BLOCK     = $00000001; {If set device reads are non blocking (Also supported in Flags parameter of TouchDeviceRead)}
 TOUCH_FLAG_PEEK_BUFFER   = $00000002; {Peek at the buffer to see if any data is available, don't remove it (Used only in Flags parameter of TouchDeviceRead)}
 TOUCH_FLAG_MOUSE_DATA    = $00000004; {If set the device will write a mouse data event for each touch event}
 TOUCH_FLAG_MULTI_POINT   = $00000008; {If set the device supports multi point touch}
 TOUCH_FLAG_PRESSURE      = $00000010; {If set the device supports pressure value on touch points}
 TOUCH_FLAG_SWAP_XY       = $00000020; {If set swap the X and Y coordinates}
 TOUCH_FLAG_INVERT_X      = $00000040; {If set invert the X coordinate}
 TOUCH_FLAG_INVERT_Y      = $00000080; {If set invert the Y coordinate}
 TOUCH_FLAG_SWAP_MAX_XY   = $00000100; {If set swap the maximum X and Y values}
 TOUCH_FLAG_RELEASE_TIMER = $00000200; {If set enable the touch release timer for devices that don't provide release events}

 {Flags supported by TOUCH_CONTROL_GET/SET/CLEAR_FLAG}
 TOUCH_FLAG_MASK = TOUCH_FLAG_NON_BLOCK or TOUCH_FLAG_MOUSE_DATA or TOUCH_FLAG_MULTI_POINT or TOUCH_FLAG_PRESSURE or TOUCH_FLAG_SWAP_XY or TOUCH_FLAG_INVERT_X or TOUCH_FLAG_INVERT_Y or TOUCH_FLAG_SWAP_MAX_XY or TOUCH_FLAG_RELEASE_TIMER;

 {Touch Device Control Codes}
 TOUCH_CONTROL_GET_FLAG         = 1;  {Get Flag}
 TOUCH_CONTROL_SET_FLAG         = 2;  {Set Flag}
 TOUCH_CONTROL_CLEAR_FLAG       = 3;  {Clear Flag}
 TOUCH_CONTROL_FLUSH_BUFFER     = 4;  {Flush Buffer}
 TOUCH_CONTROL_GET_WIDTH        = 5;  {Get Screen Width}
 TOUCH_CONTROL_GET_HEIGHT       = 6;  {Get Screen Height}
 TOUCH_CONTROL_GET_MAX_X        = 7;  {Get Maximum X value (Only applies to Absolute X values)}
 TOUCH_CONTROL_GET_MAX_Y        = 8;  {Get Maximum Y value (Only applies to Absolute Y values)}
 TOUCH_CONTROL_GET_MAX_Z        = 9;  {Get Maximum Z value (Only applies to Absolute Z values)}
 TOUCH_CONTROL_GET_MAX_POINTS   = 10; {Get Maximum number of Touch Points}
 TOUCH_CONTROL_GET_ROTATION     = 11; {Get Rotation value (0, 90, 180, 270)(Only where supported by the driver)}
 TOUCH_CONTROL_SET_ROTATION     = 12; {Set Rotation value (0, 90, 180, 270)(Only where supported by the driver)}
 TOUCH_CONTROL_GET_CALLBACK     = 13; {Get the registered callback function for touch events}
 TOUCH_CONTROL_SET_CALLBACK     = 14; {Set the registered callback function for touch events}

 {Touch Buffer Size}
 TOUCH_BUFFER_SIZE = 1024;

 {Touch Data Definitions (Values for TTouchData.Info)}
 TOUCH_FINGER = $00000001; {A finger is pressed at this touch point}

 {Touch Data Definitions (Values for TTouchData.PointID)}
 TOUCH_ID_UNKNOWN = Word(-1);

 {Touch Data Definitions (Values for TTouchData.PositionX)}
 TOUCH_X_UNKNOWN = -1;

 {Touch Data Definitions (Values for TTouchData.PositionY)}
 TOUCH_Y_UNKNOWN = -1;

 {Touch Data Definitions (Values for TTouchData.PositionZ)}
 TOUCH_Z_UNKNOWN = -1;

 {Touch Rotation}
 TOUCH_ROTATION_0   = FRAMEBUFFER_ROTATION_0;    {No rotation}
 TOUCH_ROTATION_90  = FRAMEBUFFER_ROTATION_90;   {90 degree rotation}
 TOUCH_ROTATION_180 = FRAMEBUFFER_ROTATION_180;  {180 degree rotation}
 TOUCH_ROTATION_270 = FRAMEBUFFER_ROTATION_270;  {270 degree rotation}

 {Touch logging}
 TOUCH_LOG_LEVEL_DEBUG     = LOG_LEVEL_DEBUG;  {Touch debugging messages}
 TOUCH_LOG_LEVEL_INFO      = LOG_LEVEL_INFO;   {Touch informational messages, such as a device being attached or detached}
 TOUCH_LOG_LEVEL_WARN      = LOG_LEVEL_WARN;   {Touch warning messages}
 TOUCH_LOG_LEVEL_ERROR     = LOG_LEVEL_ERROR;  {Touch error messages}
 TOUCH_LOG_LEVEL_NONE      = LOG_LEVEL_NONE;   {No Touch messages}

var
 TOUCH_DEFAULT_LOG_LEVEL:LongWord = TOUCH_LOG_LEVEL_DEBUG; {Minimum level for Touch messages.  Only messages with level greater than or equal to this will be printed}

var
 {Touch logging}
 TOUCH_LOG_ENABLED:Boolean;

{==============================================================================}
type
 {Touch specific types}
 {Touch Data}
 PTouchData = ^TTouchData;
 TTouchData = record
  Info:LongWord;      {Bitmap of touch info values (eg TOUCH_FINGER)}
  PointID:Word;       {The touch point ID value for this touch (First ID is 1)}
  PositionX:SmallInt; {The X position of this touch point}
  PositionY:SmallInt; {The Y position of this touch point}
  PositionZ:SmallInt; {The Z position of this touch point (If applicable)}
  TouchWidth:Word;    {The Width of this touch point (If applicable)}
  TouchHeight:Word;   {The Height of this touch point (If applicable)}
  Parameter:Pointer;  {The parameter for the event callback (If applicable)}
 end;

 {Touch Buffer}
 PTouchBuffer = ^TTouchBuffer;
 TTouchBuffer = record
  Wait:TSemaphoreHandle;     {Buffer ready semaphore}
  Start:LongWord;            {Index of first buffer ready}
  Count:LongWord;            {Number of entries ready in buffer}
  Buffer:array[0..(TOUCH_BUFFER_SIZE - 1)] of TTouchData;
 end;

 {Touch Properties}
 PTouchProperties = ^TTouchProperties;
 TTouchProperties = record
  Flags:LongWord;        {Device flags (eg TOUCH_FLAG_MULTI_POINT)}
  Width:LongWord;        {Screen Width}
  Height:LongWord;       {Screen Height}
  Rotation:LongWord;     {Screen Rotation (eg TOUCH_ROTATION_180)}
  MaxX:LongWord;         {Maximum (absolute) X position for the touch device}
  MaxY:LongWord;         {Maximum (absolute) Y position for the touch device}
  MaxZ:LongWord;         {Maximum (absolute) Z position for the touch device (If applicable)}
  MaxWidth:LongWord;     {Maximum touch width value for the touch device (If applicable)}
  MaxHeight:LongWord;    {Maximum touch height value for the touch device (If applicable)}
  MaxPoints:LongWord;    {Maximum number of touch points}
 end;

 {Touch Device}
 PTouchDevice = ^TTouchDevice;

 {Touch Event Callback}
 TTouchEvent = function(Touch:PTouchDevice;Data:PTouchData):LongWord;{$IFDEF i386} stdcall;{$ENDIF}

 {Touch Enumeration Callback}
 TTouchEnumerate = function(Touch:PTouchDevice;Data:Pointer):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 {Touch Notification Callback}
 TTouchNotification = function(Device:PDevice;Data:Pointer;Notification:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}

 {Touch Device Methods}
 TTouchDeviceStart = function(Touch:PTouchDevice):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TTouchDeviceStop = function(Touch:PTouchDevice):LongWord;{$IFDEF i386} stdcall;{$ENDIF}

 TTouchDevicePeek = function(Touch:PTouchDevice):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TTouchDeviceRead = function(Touch:PTouchDevice;Buffer:Pointer;Size,Flags:LongWord;var Count:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TTouchDeviceWrite = function(Touch:PTouchDevice;Buffer:Pointer;Size,Count:LongWord):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TTouchDeviceFlush = function(Touch:PTouchDevice):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TTouchDeviceUpdate = function(Touch:PTouchDevice):LongWord;{$IFDEF i386} stdcall;{$ENDIF}
 TTouchDeviceControl = function(Touch:PTouchDevice;Request:Integer;Argument1:PtrUInt;var Argument2:PtrUInt):LongWord;{$IFDEF i386} stdcall;{$ENDIF}

 TTouchDeviceGetProperties = function(Touch:PTouchDevice;Properties:PTouchProperties):LongWord;{$IFDEF i386} stdcall;{$ENDIF}

 TTouchDevice = record
  {Device Properties}
  Device:TDevice;                                 {The Device entry for this Touch device}
  {Touch Properties}
  TouchId:LongWord;                               {Unique Id of this Touch device in the Touch device table}
  TouchState:LongWord;                            {Touch device state (eg TOUCH_STATE_ENABLED)}
  DeviceStart:TTouchDeviceStart;                  {A Device specific DeviceStart method implementing the standard Touch device interface (Mandatory)}
  DeviceStop:TTouchDeviceStop;                    {A Device specific DeviceStop method implementing the standard Touch device interface (Mandatory)}
  DevicePeek:TTouchDevicePeek;                    {A Device specific DevicePeek method implementing a standard Touch device interface (Or nil if the default method is suitable)}
  DeviceRead:TTouchDeviceRead;                    {A Device specific DeviceRead method implementing a standard Touch device interface (Or nil if the default method is suitable)}
  DeviceWrite:TTouchDeviceWrite;                  {A Device specific DeviceWrite method implementing a standard Touch device interface (Or nil if the default method is suitable)}
  DeviceFlush:TTouchDeviceFlush;                  {A Device specific DeviceFlush method implementing a standard Touch device interface (Or nil if the default method is suitable)}
  DeviceUpdate:TTouchDeviceUpdate;                {A Device specific DeviceUpdate method implementing a standard Touch device interface (Or nil if the default method is suitable)}
  DeviceControl:TTouchDeviceControl;              {A Device specific DeviceControl method implementing a standard Touch device interface (Or nil if the default method is suitable)}
  DeviceGetProperties:TTouchDeviceGetProperties;  {A Device specific DeviceGetProperties method implementing a standard Touch device interface (Or nil if the default method is suitable)}
  {Driver Properties}
  Lock:TMutexHandle;                              {Device lock}
  Event:TTouchEvent;                              {Event callback function (If assigned)}
  Parameter:Pointer;                              {Parameter for the event callback (or nil)}
  Buffer:TTouchBuffer;                            {Touch input buffer}
  Properties:TTouchProperties;                    {Device properties}
  {Statistics Properties}
  ReceiveCount:LongWord;
  ReceiveErrors:LongWord;
  BufferOverruns:LongWord;
  {Internal Properties}
  Prev:PTouchDevice;                              {Previous entry in Touch device table}
  Next:PTouchDevice;                              {Next entry in Touch device table}
 end;

{==============================================================================}
{var}
 {Touch specific variables}

{==============================================================================}
{Initialization Functions}
procedure TouchInit;

{==============================================================================}
{Touch Functions}
function TouchDeviceStart(Touch:PTouchDevice):LongWord;
function TouchDeviceStop(Touch:PTouchDevice):LongWord;

function TouchDevicePeek(Touch:PTouchDevice):LongWord;

function TouchDeviceRead(Touch:PTouchDevice;Buffer:Pointer;Size,Flags:LongWord;var Count:LongWord):LongWord;
function TouchDeviceWrite(Touch:PTouchDevice;Buffer:Pointer;Size,Count:LongWord):LongWord;

function TouchDeviceFlush(Touch:PTouchDevice):LongWord;
function TouchDeviceUpdate(Touch:PTouchDevice):LongWord;

function TouchDeviceControl(Touch:PTouchDevice;Request:Integer;Argument1:PtrUInt;var Argument2:PtrUInt):LongWord;

function TouchDeviceProperties(Touch:PTouchDevice;Properties:PTouchProperties):LongWord; inline;
function TouchDeviceGetProperties(Touch:PTouchDevice;Properties:PTouchProperties):LongWord;

function TouchDeviceCreate:PTouchDevice;
function TouchDeviceCreateEx(Size:LongWord):PTouchDevice;
function TouchDeviceDestroy(Touch:PTouchDevice):LongWord;

function TouchDeviceRegister(Touch:PTouchDevice):LongWord;
function TouchDeviceDeregister(Touch:PTouchDevice):LongWord;

function TouchDeviceFind(TouchId:LongWord):PTouchDevice;
function TouchDeviceFindByName(const Name:String):PTouchDevice; inline;
function TouchDeviceFindByDescription(const Description:String):PTouchDevice; inline;
function TouchDeviceEnumerate(Callback:TTouchEnumerate;Data:Pointer):LongWord;

function TouchDeviceNotification(Touch:PTouchDevice;Callback:TTouchNotification;Data:Pointer;Notification,Flags:LongWord):LongWord;

{==============================================================================}
{RTL Touch Functions}

{==============================================================================}
{Touch Helper Functions}
function TouchGetCount:LongWord;
function TouchDeviceGetDefault:PTouchDevice;
function TouchDeviceSetDefault(Touch:PTouchDevice):LongWord;

function TouchDeviceCheck(Touch:PTouchDevice):PTouchDevice;

function TouchDeviceTypeToString(TouchType:LongWord):String;
function TouchDeviceStateToString(TouchState:LongWord):String;

function TouchDeviceRotationToString(Rotation:LongWord):String;

function TouchDeviceResolveRotation(ARotation:LongWord):LongWord;

function TouchDeviceSetCallback(Touch:PTouchDevice;Event:TTouchEvent;Parameter:Pointer):LongWord;

function TouchInsertData(Touch:PTouchDevice;Data:PTouchData;Signal:Boolean):LongWord;

procedure TouchLog(Level:LongWord;Touch:PTouchDevice;const AText:String);
procedure TouchLogInfo(Touch:PTouchDevice;const AText:String); inline;
procedure TouchLogWarn(Touch:PTouchDevice;const AText:String); inline;
procedure TouchLogError(Touch:PTouchDevice;const AText:String); inline;
procedure TouchLogDebug(Touch:PTouchDevice;const AText:String); inline;

{==============================================================================}
{==============================================================================}

implementation

{==============================================================================}
{==============================================================================}
var
 {Touch specific variables}
 TouchInitialized:Boolean;

 TouchDeviceTable:PTouchDevice;
 TouchDeviceTableLock:TCriticalSectionHandle = INVALID_HANDLE_VALUE;
 TouchDeviceTableCount:LongWord;

 TouchDeviceDefault:PTouchDevice;

{==============================================================================}
{==============================================================================}
{Forward Declarations}

{==============================================================================}
{==============================================================================}
{Initialization Functions}
procedure TouchInit;
{Initialize the Touch unit and Touch device table}

{Note: Called only during system startup}
begin
 {}
 {Check Initialized}
 if TouchInitialized then Exit;

 {Initialize Logging}
 TOUCH_LOG_ENABLED:=(TOUCH_DEFAULT_LOG_LEVEL <> TOUCH_LOG_LEVEL_NONE);

 {Initialize Touch Device Table}
 TouchDeviceTable:=nil;
 TouchDeviceTableLock:=CriticalSectionCreate;
 TouchDeviceTableCount:=0;
 if TouchDeviceTableLock = INVALID_HANDLE_VALUE then
  begin
   if TOUCH_LOG_ENABLED then TouchLogError(nil,'Failed to create Touch device table lock');
  end;
 TouchDeviceDefault:=nil;

 {Register Platform Touch Handlers}
 {Nothing}

 TouchInitialized:=True;
end;

{==============================================================================}
{==============================================================================}
{Touch Functions}
function TouchDeviceStart(Touch:PTouchDevice):LongWord;
{Start the specified Touch device ready for receiving events}
{Touch: The Touch device to start}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Touch}
 if Touch = nil then Exit;
 if Touch.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF TOUCH_DEBUG}
 if TOUCH_LOG_ENABLED then TouchLogDebug(Touch,'Touch Device Start');
 {$ENDIF}

 {Check Disabled}
 Result:=ERROR_SUCCESS;
 if Touch.TouchState <> TOUCH_STATE_DISABLED then Exit;

 if MutexLock(Touch.Lock) = ERROR_SUCCESS then
  begin
   try
    if Assigned(Touch.DeviceStart) then
     begin
      {Call Device Start}
      Result:=Touch.DeviceStart(Touch);
      if Result <> ERROR_SUCCESS then Exit;
     end
    else
     begin
      Result:=ERROR_INVALID_PARAMETER;
      Exit;
     end;

    {Enable Device}
    Touch.TouchState:=TOUCH_STATE_ENABLED;

    {Notify Enable}
    NotifierNotify(@Touch.Device,DEVICE_NOTIFICATION_ENABLE);

    Result:=ERROR_SUCCESS;
   finally
    MutexUnlock(Touch.Lock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function TouchDeviceStop(Touch:PTouchDevice):LongWord;
{Stop the specified Touch device and terminate receiving events}
{Touch: The Touch device to stop}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Touch}
 if Touch = nil then Exit;
 if Touch.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF TOUCH_DEBUG}
 if TOUCH_LOG_ENABLED then TouchLogDebug(Touch,'Touch Device Stop');
 {$ENDIF}

 {Check Enabled}
 Result:=ERROR_SUCCESS;
 if Touch.TouchState <> TOUCH_STATE_ENABLED then Exit;

 if MutexLock(Touch.Lock) = ERROR_SUCCESS then
  begin
   try
    if Assigned(Touch.DeviceStop) then
     begin
      {Call Device Stop}
      Result:=Touch.DeviceStop(Touch);
      if Result <> ERROR_SUCCESS then Exit;
     end
    else
     begin
      Result:=ERROR_INVALID_PARAMETER;
      Exit;
     end;

    {Disable Device}
    Touch.TouchState:=TOUCH_STATE_DISABLED;

    {Notify Disable}
    NotifierNotify(@Touch.Device,DEVICE_NOTIFICATION_DISABLE);

    Result:=ERROR_SUCCESS;
   finally
    MutexUnlock(Touch.Lock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function TouchDevicePeek(Touch:PTouchDevice):LongWord;
{Peek at the buffer of the specified touch device to see if any data packets are ready}
{Touch: The Touch device to peek at}
{Return: ERROR_SUCCESS if packets are ready, ERROR_NO_MORE_ITEMS if not or another error code on failure}
var
 Count:LongWord;
 Data:TTouchData;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Touch}
 if Touch = nil then Exit;
 if Touch.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF TOUCH_DEBUG}
 if TOUCH_LOG_ENABLED then TouchLogDebug(Touch,'Touch Device Peek');
 {$ENDIF}

 {Check Method}
 if Assigned(Touch.DevicePeek) then
  begin
   {Provided Method}
   Result:=Touch.DevicePeek(Touch);
  end
 else
  begin
   {Default Method}
   Result:=TouchDeviceRead(Touch,@Data,SizeOf(TTouchData),TOUCH_FLAG_NON_BLOCK or TOUCH_FLAG_PEEK_BUFFER,Count);
  end;
end;

{==============================================================================}

function TouchDeviceRead(Touch:PTouchDevice;Buffer:Pointer;Size,Flags:LongWord;var Count:LongWord):LongWord;
{Read touch data packets from the buffer of the specified touch device}
{Touch: The Touch device to read from}
{Buffer: Pointer to a buffer to copy the touch data packets to}
{Size: The size of the buffer in bytes (Must be at least TTouchData or greater)}
{Flags: The flags for the behaviour of the read (eg TOUCH_FLAG_NON_BLOCK)}
{Count: The number of touch data packets copied to the buffer}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Offset:PtrUInt;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Touch}
 if Touch = nil then Exit;
 if Touch.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF TOUCH_DEBUG}
 if TOUCH_LOG_ENABLED then TouchLogDebug(Touch,'Touch Device Read (Size=' + IntToStr(Size) + ')');
 {$ENDIF}

 {Check Buffer}
 if Buffer = nil then Exit;

 {Check Size}
 if Size < SizeOf(TTouchData) then Exit;

 {Check Method}
 if Assigned(Touch.DeviceRead) then
  begin
   {Provided Method}
   Result:=Touch.DeviceRead(Touch,Buffer,Size,Flags,Count);
  end
 else
  begin
   {Default Method}
   {Check Touch Enabled}
   if Touch.TouchState <> TOUCH_STATE_ENABLED then Exit;

   {$IFDEF TOUCH_DEBUG}
   if TOUCH_LOG_ENABLED then TouchLogDebug(Touch,'Attempting to read ' + IntToStr(Size) + ' bytes from touch');
   {$ENDIF}

   {Read to Buffer}
   Count:=0;
   Offset:=0;
   while Size >= SizeOf(TTouchData) do
    begin
     {Check Non Blocking}
     if (((Touch.Device.DeviceFlags and TOUCH_FLAG_NON_BLOCK) <> 0) or ((Flags and TOUCH_FLAG_NON_BLOCK) <> 0)) and (Touch.Buffer.Count = 0) then
      begin
       if Count = 0 then Result:=ERROR_NO_MORE_ITEMS;
       Break;
      end;

     {Check Peek Buffer}
     if (Flags and TOUCH_FLAG_PEEK_BUFFER) <> 0 then
      begin
       {Acquire the Lock}
       if MutexLock(Touch.Lock) = ERROR_SUCCESS then
        begin
         try
          if Touch.Buffer.Count > 0 then
           begin
            {Copy Data}
            PTouchData(PtrUInt(Buffer) + Offset)^:=Touch.Buffer.Buffer[Touch.Buffer.Start];

            {Update Count}
            Inc(Count);

            Result:=ERROR_SUCCESS;
            Break;
           end
          else
           begin
            Result:=ERROR_NO_MORE_ITEMS;
            Break;
           end;
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
      end
     else
      begin
       {Wait for Touch Data}
       if SemaphoreWait(Touch.Buffer.Wait) = ERROR_SUCCESS then
        begin
         {Acquire the Lock}
         if MutexLock(Touch.Lock) = ERROR_SUCCESS then
          begin
           try
            {Copy Data}
            PTouchData(PtrUInt(Buffer) + Offset)^:=Touch.Buffer.Buffer[Touch.Buffer.Start];

            {Update Start}
            Touch.Buffer.Start:=(Touch.Buffer.Start + 1) mod TOUCH_BUFFER_SIZE;

            {Update Count}
            Dec(Touch.Buffer.Count);

            {Update Count}
            Inc(Count);

            {Update Size and Offset}
            Dec(Size,SizeOf(TTouchData));
            Inc(Offset,SizeOf(TTouchData));
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
        end
       else
        begin
         Result:=ERROR_CAN_NOT_COMPLETE;
         Exit;
        end;
      end;

     {Return Result}
     Result:=ERROR_SUCCESS;
    end;

   {$IFDEF TOUCH_DEBUG}
   if TOUCH_LOG_ENABLED then TouchLogDebug(Touch,'Return count=' + IntToStr(Count));
   {$ENDIF}
  end;
end;

{==============================================================================}

function TouchDeviceWrite(Touch:PTouchDevice;Buffer:Pointer;Size,Count:LongWord):LongWord;
{Write touch data packets to the buffer of the specified touch device}
{Touch: The Touch device to write to}
{Buffer: Pointer to a buffer to copy the touch data packets from}
{Size: The size of the buffer in bytes (Must be at least TTouchData or greater)}
{Count: The number of touch data packets to copy from the buffer}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Offset:PtrUInt;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Touch}
 if Touch = nil then Exit;
 if Touch.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF TOUCH_DEBUG}
 if TOUCH_LOG_ENABLED then TouchLogDebug(Touch,'Touch Device Write (Size=' + IntToStr(Size) + ')');
 {$ENDIF}

 {Check Buffer}
 if Buffer = nil then Exit;

 {Check Size}
 if Size < SizeOf(TTouchData) then Exit;

 {Check Count}
 if Count < 1 then Exit;

 {Check Method}
 if Assigned(Touch.DeviceWrite) then
  begin
   {Provided Method}
   Result:=Touch.DeviceWrite(Touch,Buffer,Size,Count);
  end
 else
  begin
   {Default Method}
   {Check Touch Enabled}
   if Touch.TouchState <> TOUCH_STATE_ENABLED then Exit;

   {$IFDEF TOUCH_DEBUG}
   if TOUCH_LOG_ENABLED then TouchLogDebug(Touch,'Attempting to write ' + IntToStr(Size) + ' bytes to touch');
   {$ENDIF}

   {Write from Buffer}
   Offset:=0;
   while (Size >= SizeOf(TTouchData)) and (Count > 0) do
    begin
     {Acquire the Lock}
     if MutexLock(Touch.Lock) = ERROR_SUCCESS then
      begin
       try
        {Check Buffer}
        if (Touch.Buffer.Count < TOUCH_BUFFER_SIZE) then
         begin
          {Copy Data}
          Touch.Buffer.Buffer[(Touch.Buffer.Start + Touch.Buffer.Count) mod TOUCH_BUFFER_SIZE]:=PTouchData(PtrUInt(Buffer) + Offset)^;

          {Update Count}
          Inc(Touch.Buffer.Count);

          {Update Count}
          Dec(Count);

          {Update Size and Offset}
          Dec(Size,SizeOf(TTouchData));
          Inc(Offset,SizeOf(TTouchData));

          {Signal Data Received}
          SemaphoreSignal(Touch.Buffer.Wait);
         end
        else
         begin
          Result:=ERROR_INSUFFICIENT_BUFFER;
          Exit;
         end;
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

     {Return Result}
     Result:=ERROR_SUCCESS;
    end;
  end;
end;

{==============================================================================}

function TouchDeviceFlush(Touch:PTouchDevice):LongWord;
{Flush the contents of the buffer of the specified touch device}
{Touch: The Touch device to flush}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Touch}
 if Touch = nil then Exit;
 if Touch.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF TOUCH_DEBUG}
 if TOUCH_LOG_ENABLED then TouchLogDebug(Touch,'Touch Device Flush');
 {$ENDIF}

 {Check Method}
 if Assigned(Touch.DeviceFlush) then
  begin
   {Provided Method}
   Result:=Touch.DeviceFlush(Touch);
  end
 else
  begin
   {Default Method}
   {Check Touch Enabled}
   if Touch.TouchState <> TOUCH_STATE_ENABLED then Exit;

   {Acquire the Lock}
   if MutexLock(Touch.Lock) = ERROR_SUCCESS then
    begin
     try
      while Touch.Buffer.Count > 0 do
       begin
        {Wait for Data (Should not Block)}
        if SemaphoreWait(Touch.Buffer.Wait) = ERROR_SUCCESS then
         begin
          {Update Start}
          Touch.Buffer.Start:=(Touch.Buffer.Start + 1) mod TOUCH_BUFFER_SIZE;

          {Update Count}
          Dec(Touch.Buffer.Count);
         end
        else
         begin
          Result:=ERROR_CAN_NOT_COMPLETE;
          Exit;
         end;
       end;

      {Return Result}
      Result:=ERROR_SUCCESS;
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
end;

{==============================================================================}

function TouchDeviceUpdate(Touch:PTouchDevice):LongWord;
{Request the specified touch device to update the current configuration}
{Touch: The Touch device to update}
{Return: ERROR_SUCCESS if completed or another error code on failure}
{Note: Items updated can include rotation, maximum X and Y and flags (If supported)}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Touch}
 if Touch = nil then Exit;
 if Touch.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF TOUCH_DEBUG}
 if TOUCH_LOG_ENABLED then TouchLogDebug(Touch,'Touch Device Update');
 {$ENDIF}

 {Check Method}
 if Assigned(Touch.DeviceUpdate) then
  begin
   {Provided Method}
   Result:=Touch.DeviceUpdate(Touch);
  end
 else
  begin
   {Default Method}
   {Check Touch Enabled}
   if Touch.TouchState <> TOUCH_STATE_ENABLED then Exit;

   {Acquire the Lock}
   if MutexLock(Touch.Lock) = ERROR_SUCCESS then
    begin
     try
      {Nothing by default}

      {Return Result}
      Result:=ERROR_SUCCESS;
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
end;

{==============================================================================}

function TouchDeviceControl(Touch:PTouchDevice;Request:Integer;Argument1:PtrUInt;var Argument2:PtrUInt):LongWord;
{Perform a control request on the specified touch device}
{Touch: The Touch device to control}
{Request: The request code for the operation (eg TOUCH_CONTROL_GET_FLAG)}
{Argument1: The first argument for the operation (Dependent on request code)}
{Argument2: The second argument for the operation (Dependent on request code)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Touch}
 if Touch = nil then Exit;
 if Touch.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Method}
 if Assigned(Touch.DeviceControl) then
  begin
   {Provided Method}
   Result:=Touch.DeviceControl(Touch,Request,Argument1,Argument2);
  end
 else
  begin
   {Default Method}
   {Check Touch Enabled}
   if Touch.TouchState <> TOUCH_STATE_ENABLED then Exit;

   {Acquire the Lock}
   if MutexLock(Touch.Lock) = ERROR_SUCCESS then
    begin
     try
      case Request of
       TOUCH_CONTROL_GET_FLAG:begin
         {Get Flag}
         Argument2:=Ord(False);
         if (Touch.Device.DeviceFlags and Argument1) <> 0 then
          begin
           Argument2:=Ord(True);

           {Return Result}
           Result:=ERROR_SUCCESS;
          end;
        end;
       TOUCH_CONTROL_SET_FLAG:begin
         {Set Flag}
         if (Argument1 and not(TOUCH_FLAG_MASK)) = 0 then
          begin
           Touch.Device.DeviceFlags:=(Touch.Device.DeviceFlags or Argument1);
           Touch.Properties.Flags:=Touch.Device.DeviceFlags;

           {Request Update}
           Result:=TouchDeviceUpdate(Touch);
          end;
        end;
       TOUCH_CONTROL_CLEAR_FLAG:begin
         {Clear Flag}
         if (Argument1 and not(TOUCH_FLAG_MASK)) = 0 then
          begin
           Touch.Device.DeviceFlags:=(Touch.Device.DeviceFlags and not(Argument1));
           Touch.Properties.Flags:=Touch.Device.DeviceFlags;

           {Request Update}
           Result:=TouchDeviceUpdate(Touch);
          end;
        end;
       TOUCH_CONTROL_FLUSH_BUFFER:begin
         {Flush Buffer}
         while Touch.Buffer.Count > 0 do
          begin
           {Wait for Data (Should not Block)}
           if SemaphoreWait(Touch.Buffer.Wait) = ERROR_SUCCESS then
            begin
             {Update Start}
             Touch.Buffer.Start:=(Touch.Buffer.Start + 1) mod TOUCH_BUFFER_SIZE;

             {Update Count}
             Dec(Touch.Buffer.Count);
            end
           else
            begin
             Result:=ERROR_CAN_NOT_COMPLETE;
             Exit;
            end;
          end;

         {Return Result}
         Result:=ERROR_SUCCESS;
        end;
       TOUCH_CONTROL_GET_WIDTH:begin
         {Get Width}
         Argument2:=Touch.Properties.Width;

         {Return Result}
         Result:=ERROR_SUCCESS;
        end;
       TOUCH_CONTROL_GET_HEIGHT:begin
         {Get Height}
         Argument2:=Touch.Properties.Height;

         {Return Result}
         Result:=ERROR_SUCCESS;
        end;
       TOUCH_CONTROL_GET_MAX_X:begin
         {Get Maximum X}
         Argument2:=Touch.Properties.MaxX;

         {Return Result}
         Result:=ERROR_SUCCESS;
        end;
       TOUCH_CONTROL_GET_MAX_Y:begin
         {Get Maximum Y}
         Argument2:=Touch.Properties.MaxY;

         {Return Result}
         Result:=ERROR_SUCCESS;
        end;
       TOUCH_CONTROL_GET_MAX_Z:begin
         {Get Maximum Z}
         Argument2:=Touch.Properties.MaxZ;

         {Return Result}
         Result:=ERROR_SUCCESS;
        end;
       TOUCH_CONTROL_GET_MAX_POINTS:begin
         {Get Maximum Points}
         Argument2:=Touch.Properties.MaxPoints;

         {Return Result}
         Result:=ERROR_SUCCESS;
        end;
       TOUCH_CONTROL_GET_ROTATION:begin
         {Get Rotation}
         Argument2:=Touch.Properties.Rotation;

         {Return Result}
         Result:=ERROR_SUCCESS;
        end;
       TOUCH_CONTROL_SET_ROTATION:begin
         {Set Rotation}
         case TouchDeviceResolveRotation(Argument1) of
          TOUCH_ROTATION_0,TOUCH_ROTATION_90,TOUCH_ROTATION_180,TOUCH_ROTATION_270:begin
            Touch.Properties.Rotation:=Argument1;

            {Request Update}
            Result:=TouchDeviceUpdate(Touch);
           end;
         end;
        end;
       TOUCH_CONTROL_GET_CALLBACK:begin
         {Get Callback}
         Argument2:=PtrUInt(@Touch.Event);

         {Return Result}
         Result:=ERROR_SUCCESS;
        end;
       TOUCH_CONTROL_SET_CALLBACK:begin
         {Set Callback}
         Touch.Event:=TTouchEvent(Argument1);
         Touch.Parameter:=Pointer(Argument2);

         {Return Result}
         Result:=ERROR_SUCCESS;
        end;
      end;
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
end;

{==============================================================================}

function TouchDeviceProperties(Touch:PTouchDevice;Properties:PTouchProperties):LongWord; inline;
{Get the properties for the specified Touch device}
{Touch: The Touch device to get properties from}
{Properties: Pointer to a TTouchProperties structure to fill in}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: Replaced by TouchDeviceGetProperties for consistency}
begin
 {}
 Result:=TouchDeviceGetProperties(Touch,Properties);
end;

{==============================================================================}

function TouchDeviceGetProperties(Touch:PTouchDevice;Properties:PTouchProperties):LongWord;
{Get the properties for the specified Touch device}
{Touch: The Touch device to get properties from}
{Properties: Pointer to a TTouchProperties structure to fill in}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Properties}
 if Properties = nil then Exit;

 {Check Touch}
 if Touch = nil then Exit;
 if Touch.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {$IFDEF TOUCH_DEBUG}
 if TOUCH_LOG_ENABLED then TouchLogDebug(Touch,'Touch Device Get Properties');
 {$ENDIF}

 {Check Enabled}
 {Result:=ERROR_NOT_SUPPORTED;}
 {if Touch.TouchState <> TOUCH_STATE_ENABLED then Exit;} {Allow when disabled}

 if MutexLock(Touch.Lock) = ERROR_SUCCESS then
  begin
   if Assigned(Touch.DeviceGetProperties) then
    begin
     {Call Device Get Properites}
     Result:=Touch.DeviceGetProperties(Touch,Properties);
    end
   else
    begin
     {Get Properties}
     System.Move(Touch.Properties,Properties^,SizeOf(TTouchProperties));

     {Return Result}
     Result:=ERROR_SUCCESS;
    end;

   MutexUnlock(Touch.Lock);
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function TouchDeviceCreate:PTouchDevice;
{Create a new Touch device entry}
{Return: Pointer to new Touch device entry or nil if Touch device could not be created}
begin
 {}
 Result:=TouchDeviceCreateEx(SizeOf(TTouchDevice));
end;

{==============================================================================}

function TouchDeviceCreateEx(Size:LongWord):PTouchDevice;
{Create a new Touch device entry}
{Size: Size in bytes to allocate for new Touch device (Including the Touch device entry)}
{Return: Pointer to new Touch device entry or nil if Touch device could not be created}
begin
 {}
 Result:=nil;

 {Check Size}
 if Size < SizeOf(TTouchDevice) then Exit;

 {Create Touch}
 Result:=PTouchDevice(DeviceCreateEx(Size));
 if Result = nil then Exit;

 {Update Device}
 Result.Device.DeviceBus:=DEVICE_BUS_NONE;
 Result.Device.DeviceType:=TOUCH_TYPE_NONE;
 Result.Device.DeviceFlags:=TOUCH_FLAG_NONE;
 Result.Device.DeviceData:=nil;

 {Update Touch}
 Result.TouchId:=DEVICE_ID_ANY;
 Result.TouchState:=TOUCH_STATE_DISABLED;
 Result.DeviceStart:=nil;
 Result.DeviceStop:=nil;
 Result.DevicePeek:=nil;
 Result.DeviceRead:=nil;
 Result.DeviceWrite:=nil;
 Result.DeviceFlush:=nil;
 Result.DeviceUpdate:=nil;
 Result.DeviceControl:=nil;
 Result.DeviceGetProperties:=nil;
 Result.Lock:=INVALID_HANDLE_VALUE;
 Result.Event:=nil;
 Result.Parameter:=nil;
 Result.Buffer.Wait:=INVALID_HANDLE_VALUE;

 {Check Defaults}
 if TOUCH_MOUSE_DATA_DEFAULT then Result.Device.DeviceFlags:=Result.Device.DeviceFlags or TOUCH_FLAG_MOUSE_DATA;

 {Update Properties}
 Result.Properties.Flags:=Result.Device.DeviceFlags;
 Result.Properties.Width:=0;
 Result.Properties.Height:=0;
 Result.Properties.Rotation:=TOUCH_ROTATION_0;
 Result.Properties.MaxX:=0;
 Result.Properties.MaxY:=0;
 Result.Properties.MaxZ:=0;
 Result.Properties.MaxPoints:=0;

 {Create Lock}
 Result.Lock:=MutexCreateEx(False,MUTEX_DEFAULT_SPINCOUNT,MUTEX_FLAG_RECURSIVE);
 if Result.Lock = INVALID_HANDLE_VALUE then
  begin
   if TOUCH_LOG_ENABLED then TouchLogError(nil,'Failed to create lock for Touch device');
   TouchDeviceDestroy(Result);
   Result:=nil;
   Exit;
  end;

 {Create Buffer Semaphore}
 Result.Buffer.Wait:=SemaphoreCreate(0);
 if Result.Buffer.Wait = INVALID_HANDLE_VALUE then
  begin
   if TOUCH_LOG_ENABLED then TouchLogError(nil,'Failed to create buffer semaphore for Touch device');
   TouchDeviceDestroy(Result);
   Result:=nil;
   Exit;
  end;
end;

{==============================================================================}

function TouchDeviceDestroy(Touch:PTouchDevice):LongWord;
{Destroy an existing Touch device entry}
{Touch: The Touch device to destroy}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Touch}
 if Touch = nil then Exit;
 if Touch.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Touch}
 Result:=ERROR_IN_USE;
 if TouchDeviceCheck(Touch) = Touch then Exit;

 {Check State}
 if Touch.Device.DeviceState <> DEVICE_STATE_UNREGISTERED then Exit;

 {Destroy Buffer Semaphore}
 if Touch.Buffer.Wait <> INVALID_HANDLE_VALUE then
  begin
   SemaphoreDestroy(Touch.Buffer.Wait);
  end;

 {Destroy Lock}
 if Touch.Lock <> INVALID_HANDLE_VALUE then
  begin
   MutexDestroy(Touch.Lock);
  end;

 {Destroy Touch}
 Result:=DeviceDestroy(@Touch.Device);
end;

{==============================================================================}

function TouchDeviceRegister(Touch:PTouchDevice):LongWord;
{Register a new Touch device in the Touch device table}
{Touch: The Touch device to register}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 TouchId:LongWord;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Touch}
 if Touch = nil then Exit;
 if Touch.TouchId <> DEVICE_ID_ANY then Exit;
 if Touch.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Interfaces}
 if not(Assigned(Touch.DeviceStart)) then Exit;
 if not(Assigned(Touch.DeviceStop)) then Exit;

 {Check Touch}
 Result:=ERROR_ALREADY_EXISTS;
 if TouchDeviceCheck(Touch) = Touch then Exit;

 {Check State}
 if Touch.Device.DeviceState <> DEVICE_STATE_UNREGISTERED then Exit;

 {Insert Touch}
 if CriticalSectionLock(TouchDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Update Touch}
    TouchId:=0;
    while TouchDeviceFind(TouchId) <> nil do
     begin
      Inc(TouchId);
     end;
    Touch.TouchId:=TouchId;

    {Update Device}
    Touch.Device.DeviceName:=TOUCH_NAME_PREFIX + IntToStr(Touch.TouchId);
    Touch.Device.DeviceClass:=DEVICE_CLASS_TOUCH;

    {Register Device}
    Result:=DeviceRegister(@Touch.Device);
    if Result <> ERROR_SUCCESS then
     begin
      Touch.TouchId:=DEVICE_ID_ANY;
      Exit;
     end;

    {Link Touch}
    if TouchDeviceTable = nil then
     begin
      TouchDeviceTable:=Touch;
     end
    else
     begin
      Touch.Next:=TouchDeviceTable;
      TouchDeviceTable.Prev:=Touch;
      TouchDeviceTable:=Touch;
     end;

    {Increment Count}
    Inc(TouchDeviceTableCount);

    {Check Default}
    if TouchDeviceDefault = nil then
     begin
      TouchDeviceDefault:=Touch;
     end;

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    CriticalSectionUnlock(TouchDeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function TouchDeviceDeregister(Touch:PTouchDevice):LongWord;
{Deregister a Touch device from the Touch device table}
{Touch: The Touch device to deregister}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Prev:PTouchDevice;
 Next:PTouchDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Touch}
 if Touch = nil then Exit;
 if Touch.TouchId = DEVICE_ID_ANY then Exit;
 if Touch.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Check Touch}
 Result:=ERROR_NOT_FOUND;
 if TouchDeviceCheck(Touch) <> Touch then Exit;

 {Check State}
 if Touch.Device.DeviceState <> DEVICE_STATE_REGISTERED then Exit;

 {Remove Touch}
 if CriticalSectionLock(TouchDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Deregister Device}
    Result:=DeviceDeregister(@Touch.Device);
    if Result <> ERROR_SUCCESS then Exit;

    {Unlink Touch}
    Prev:=Touch.Prev;
    Next:=Touch.Next;
    if Prev = nil then
     begin
      TouchDeviceTable:=Next;
      if Next <> nil then
       begin
        Next.Prev:=nil;
       end;
     end
    else
     begin
      Prev.Next:=Next;
      if Next <> nil then
       begin
        Next.Prev:=Prev;
       end;
     end;

    {Decrement Count}
    Dec(TouchDeviceTableCount);

    {Check Default}
    if TouchDeviceDefault = Touch then
     begin
      TouchDeviceDefault:=TouchDeviceTable;
     end;

    {Update Touch}
    Touch.TouchId:=DEVICE_ID_ANY;

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    CriticalSectionUnlock(TouchDeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function TouchDeviceFind(TouchId:LongWord):PTouchDevice;
{Find a Touch device by ID in the Touch device table}
{TouchId: The ID number of the Touch device to find}
{Return: Pointer to Touch device entry or nil if not found}
var
 Touch:PTouchDevice;
begin
 {}
 Result:=nil;

 {Check Id}
 if TouchId = DEVICE_ID_ANY then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(TouchDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Touch}
    Touch:=TouchDeviceTable;
    while Touch <> nil do
     begin
      {Check State}
      if Touch.Device.DeviceState = DEVICE_STATE_REGISTERED then
       begin
        {Check Id}
        if Touch.TouchId = TouchId then
         begin
          Result:=Touch;
          Exit;
         end;
       end;

      {Get Next}
      Touch:=Touch.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(TouchDeviceTableLock);
   end;
  end;
end;

{==============================================================================}

function TouchDeviceFindByName(const Name:String):PTouchDevice; inline;
{Find a Touch device by name in the device table}
{Name: The name of the Touch device to find (eg Touch0)}
{Return: Pointer to Touch device entry or nil if not found}
begin
 {}
 Result:=PTouchDevice(DeviceFindByName(Name));
end;

{==============================================================================}

function TouchDeviceFindByDescription(const Description:String):PTouchDevice; inline;
{Find a Touch device by description in the device table}
{Description: The description of the Touch to find (eg USB Touchscreen)}
{Return: Pointer to Touch device entry or nil if not found}
begin
 {}
 Result:=PTouchDevice(DeviceFindByDescription(Description));
end;

{==============================================================================}

function TouchDeviceEnumerate(Callback:TTouchEnumerate;Data:Pointer):LongWord;
{Enumerate all Touch devices in the Touch device table}
{Callback: The callback function to call for each Touch device in the table}
{Data: A private data pointer to pass to callback for each Touch device in the table}
{Return: ERROR_SUCCESS if completed or another error code on failure}
var
 Touch:PTouchDevice;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Callback}
 if not Assigned(Callback) then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(TouchDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Touch}
    Touch:=TouchDeviceTable;
    while Touch <> nil do
     begin
      {Check State}
      if Touch.Device.DeviceState = DEVICE_STATE_REGISTERED then
       begin
        if Callback(Touch,Data) <> ERROR_SUCCESS then Exit;
       end;

      {Get Next}
      Touch:=Touch.Next;
     end;

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    CriticalSectionUnlock(TouchDeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function TouchDeviceNotification(Touch:PTouchDevice;Callback:TTouchNotification;Data:Pointer;Notification,Flags:LongWord):LongWord;
{Register a notification for Touch device changes}
{Touch: The Touch device to notify changes for (Optional, pass nil for all Touch devices)}
{Callback: The function to call when a notification event occurs}
{Data: A private data pointer to pass to callback when a notification event occurs}
{Notification: The events to register for notification of (eg DEVICE_NOTIFICATION_REGISTER)}
{Flags: The flags to control the notification (eg NOTIFIER_FLAG_WORKER)}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Touch}
 if Touch = nil then
  begin
   Result:=DeviceNotification(nil,DEVICE_CLASS_TOUCH,Callback,Data,Notification,Flags);
  end
 else
  begin
   {Check Touch}
   if Touch.Device.Signature <> DEVICE_SIGNATURE then Exit;

   Result:=DeviceNotification(@Touch.Device,DEVICE_CLASS_TOUCH,Callback,Data,Notification,Flags);
  end;
end;

{==============================================================================}
{==============================================================================}
{RTL Touch Functions}

{==============================================================================}
{==============================================================================}
{Touch Helper Functions}
function TouchGetCount:LongWord;
{Get the current Touch device count}
{Return: The number of Touch devices}
begin
 {}
 Result:=TouchDeviceTableCount;
end;

{==============================================================================}

function TouchDeviceGetDefault:PTouchDevice;
{Get the current default Touch device}
{Return: Pointer to default Touch device entry}
begin
 {}
 Result:=TouchDeviceDefault;
end;

{==============================================================================}

function TouchDeviceSetDefault(Touch:PTouchDevice):LongWord;
{Set the current default Touch device}
{Touch: The Touch device to set as default}
{Return: ERROR_SUCCESS if completed or another error code on failure}
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Touch}
 if Touch = nil then Exit;
 if Touch.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(TouchDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Check Touch}
    if TouchDeviceCheck(Touch) <> Touch then Exit;

    {Set Touch Default}
    TouchDeviceDefault:=Touch;

    {Return Result}
    Result:=ERROR_SUCCESS;
   finally
    {Release the Lock}
    CriticalSectionUnlock(TouchDeviceTableLock);
   end;
  end
 else
  begin
   Result:=ERROR_CAN_NOT_COMPLETE;
  end;
end;

{==============================================================================}

function TouchDeviceCheck(Touch:PTouchDevice):PTouchDevice;
{Check if the supplied Touch device is in the Touch device table}
{Touch: The Touch device to check}
{Return: Pointer to Touch device entry or nil if not found}
var
 Current:PTouchDevice;
begin
 {}
 Result:=nil;

 {Check Touch}
 if Touch = nil then Exit;
 if Touch.Device.Signature <> DEVICE_SIGNATURE then Exit;

 {Acquire the Lock}
 if CriticalSectionLock(TouchDeviceTableLock) = ERROR_SUCCESS then
  begin
   try
    {Get Touch}
    Current:=TouchDeviceTable;
    while Current <> nil do
     begin
      {Check Touch}
      if Current = Touch then
       begin
        Result:=Touch;
        Exit;
       end;

      {Get Next}
      Current:=Current.Next;
     end;
   finally
    {Release the Lock}
    CriticalSectionUnlock(TouchDeviceTableLock);
   end;
  end;
end;

{==============================================================================}

function TouchDeviceTypeToString(TouchType:LongWord):String;
{Return a string describing the Touch device type (eg TOUCH_TYPE_CAPACITIVE)}
begin
 {}
 Result:='TOUCH_TYPE_UNKNOWN';

 if TouchType <= TOUCH_TYPE_MAX then
  begin
   Result:=TOUCH_TYPE_NAMES[TouchType];
  end;
end;

{==============================================================================}

function TouchDeviceStateToString(TouchState:LongWord):String;
{Return a string describing the Touch device state (eg TOUCH_STATE_ENABLED)}
begin
 {}
 Result:='TOUCH_STATE_UNKNOWN';

 if TouchState <= TOUCH_STATE_MAX then
  begin
   Result:=TOUCH_STATE_NAMES[TouchState];
  end;
end;

{==============================================================================}

function TouchDeviceRotationToString(Rotation:LongWord):String;
{Return a string describing the supplied touch rotation value}
begin
 {}
 Result:='TOUCH_ROTATION_UNKNOWN';

 case Rotation of
  TOUCH_ROTATION_0:Result:='TOUCH_ROTATION_0';
  TOUCH_ROTATION_90:Result:='TOUCH_ROTATION_90';
  TOUCH_ROTATION_180:Result:='TOUCH_ROTATION_180';
  TOUCH_ROTATION_270:Result:='TOUCH_ROTATION_270';
 end;
end;

{==============================================================================}

function TouchDeviceResolveRotation(ARotation:LongWord):LongWord;
{Resolve a value of 0, 90, 180 or 270 to a touch rotation constant (eg TOUCH_ROTATION_180)}
{Note: Also accepts passing the touch rotation constant values directly}
begin
 {}
 case ARotation of
  90:Result:=TOUCH_ROTATION_90;
  180:Result:=TOUCH_ROTATION_180;
  270:Result:=TOUCH_ROTATION_270;
  else
   Result:=ARotation;
 end;
end;

{==============================================================================}

function TouchDeviceSetCallback(Touch:PTouchDevice;Event:TTouchEvent;Parameter:Pointer):LongWord;
{Set the event callback function for the specified touch device}
{Touch: The touch device to set the event callback for}
{Event: The event callback function to be called when touch data is received}
{Parameter: A pointer to private data to be passed to the callback with each event}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: This function also clears the TOUCH_FLAG_MOUSE_DATA flag because the event}
{      callback is not compatible with receiving touch events as mouse data}
var
 Argument2:PtrUInt;
begin
 {}
 {Set Callback}
 Result:=TouchDeviceControl(Touch,TOUCH_CONTROL_SET_CALLBACK,PtrUInt(@Event),PtrUInt(Parameter));
 if Result = ERROR_SUCCESS then
  begin
   {Clear Mouse Data Flag}
   Result:=TouchDeviceControl(Touch,TOUCH_CONTROL_CLEAR_FLAG,TOUCH_FLAG_MOUSE_DATA,Argument2);
  end;
end;

{==============================================================================}

function TouchInsertData(Touch:PTouchDevice;Data:PTouchData;Signal:Boolean):LongWord;
{Insert a TTouchData entry into the touch device buffer}
{Touch: The touch device to insert data for}
{Data: The TTouchData entry to insert}
{Signal: If True then signal that new data is available in the buffer}
{Return: ERROR_SUCCESS if completed or another error code on failure}

{Note: Caller must hold the touch device lock}
var
 Next:PTouchData;
begin
 {}
 Result:=ERROR_INVALID_PARAMETER;

 {Check Touch}
 if Touch = nil then Exit;

 {Check Data}
 if Data = nil then Exit;

 {Check Buffer}
 if (Touch.Buffer.Count < TOUCH_BUFFER_SIZE) then
  begin
   {Get Next}
   Next:=@Touch.Buffer.Buffer[(Touch.Buffer.Start + Touch.Buffer.Count) mod TOUCH_BUFFER_SIZE];
   if Next <> nil then
    begin
     {Copy Data}
     Next^:=Data^;

     {Update Count}
     Inc(Touch.Buffer.Count);

     {Signal Data Received}
     if Signal then SemaphoreSignal(Touch.Buffer.Wait);

     {Return Result}
     Result:=ERROR_SUCCESS;
    end;
  end
 else
  begin
   if TOUCH_LOG_ENABLED then TouchLogError(Touch,'Buffer overflow, touch discarded');

   {Update Statistics}
   Inc(Touch.BufferOverruns);

   Result:=ERROR_INSUFFICIENT_BUFFER;
  end;
end;

{==============================================================================}

procedure TouchLog(Level:LongWord;Touch:PTouchDevice;const AText:String);
var
 WorkBuffer:String;
begin
 {}
 {Check Level}
 if Level < TOUCH_DEFAULT_LOG_LEVEL then Exit;

 WorkBuffer:='';
 {Check Level}
 if Level = TOUCH_LOG_LEVEL_DEBUG then
  begin
   WorkBuffer:=WorkBuffer + '[DEBUG] ';
  end
 else if Level = TOUCH_LOG_LEVEL_WARN then
  begin
   WorkBuffer:=WorkBuffer + '[WARN] ';
  end
 else if Level = TOUCH_LOG_LEVEL_ERROR then
  begin
   WorkBuffer:=WorkBuffer + '[ERROR] ';
  end;

 {Add Prefix}
 WorkBuffer:=WorkBuffer + 'Touch: ';

 {Check Touch}
 if Touch <> nil then
  begin
   WorkBuffer:=WorkBuffer + TOUCH_NAME_PREFIX + IntToStr(Touch.TouchId) + ': ';
  end;

 {Output Logging}
 LoggingOutputEx(LOGGING_FACILITY_TOUCH,LogLevelToLoggingSeverity(Level),'Touch',WorkBuffer + AText);
end;

{==============================================================================}

procedure TouchLogInfo(Touch:PTouchDevice;const AText:String); inline;
begin
 {}
 TouchLog(TOUCH_LOG_LEVEL_INFO,Touch,AText);
end;

{==============================================================================}

procedure TouchLogWarn(Touch:PTouchDevice;const AText:String); inline;
begin
 {}
 TouchLog(TOUCH_LOG_LEVEL_WARN,Touch,AText);
end;

{==============================================================================}

procedure TouchLogError(Touch:PTouchDevice;const AText:String); inline;
begin
 {}
 TouchLog(TOUCH_LOG_LEVEL_ERROR,Touch,AText);
end;

{==============================================================================}

procedure TouchLogDebug(Touch:PTouchDevice;const AText:String); inline;
begin
 {}
 TouchLog(TOUCH_LOG_LEVEL_DEBUG,Touch,AText);
end;

{==============================================================================}
{==============================================================================}
{Touch Internal Functions}

{==============================================================================}
{==============================================================================}

initialization
 TouchInit;

{==============================================================================}

finalization
 {Nothing}

{==============================================================================}
{==============================================================================}

end.
